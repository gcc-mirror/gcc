// Adapted from OpenMP examples

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

int baz (double *d_bv, const double *d_av, int n);
int bar (double *d_bv, const double *d_av, int n); 

#pragma omp declare variant(bar) match(construct={dispatch}) adjust_args(need_device_ptr: f_bv, f_av)
#pragma omp declare variant(baz) match(implementation={vendor(gnu)})
int foo (double *f_bv, const double *f_av, int n);

int baz (double *bv, const double *av, int n);
int bar (double *bv, const double *av, int n); 

int foo (double *bv, const double *av, int n)
{
  for (int i = 0; i < n; i++)
    bv[i] = av[i] * i;
  return -1;
}

int baz (double *d_bv, const double *d_av, int n)
{
#pragma omp distribute parallel for
  for (int i = 0; i < n; i++)
    d_bv[i] = d_av[i] * i;
  return -3;
}

int bar (double *d_bv, const double *d_av, int n)
{
#pragma omp target is_device_ptr(d_bv, d_av)
  for (int i = 0; i < n; i++)
    d_bv[i] = d_av[i] * i;
  return -2;
}

int test (int n)
{
  const double e = 2.71828;

  double *av = (double *) malloc (n * sizeof (*av));
  double *bv = (double *) malloc (n * sizeof (*bv));
  double *d_bv = (double *) malloc (n * sizeof (*d_bv));

  for (int i = 0; i < n; i++)
    {
      av[i] = e * i;
      bv[i] = 0.0;
      d_bv[i] = 0.0;
    }

  int f, last_dev = omp_get_num_devices () - 1;
#pragma omp target data map(to: av[:n]) map(from: d_bv[:n]) device(last_dev) if (n == 1024)
  {
    #pragma omp dispatch nocontext(n > 1024) novariants(n < 1024) device(last_dev)
    f = foo (d_bv, av, n);
  }

  foo (bv, av, n);
  for (int i = 0; i < n; i++)
    {
      if (d_bv[i] != bv[i])
	{
	  fprintf (stderr, "ERROR at %d: %lf (act) != %lf (exp)\n", i, d_bv[i], bv[i]);
	  return 1;
	}
    }
    return f;
}

int
main (void)
{
  int ret = test(1023);
  if (ret != -1) return 1;
  ret = test(1024);
  if (ret != -2) return 1;
  ret = test(1025);
  if (ret != -3) return 1;
  return 0;
}
