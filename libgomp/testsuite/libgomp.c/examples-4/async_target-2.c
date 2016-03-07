/* { dg-do run } */
/* { dg-require-effective-target offload_device_nonshared_as } */

#include <omp.h>
#include <stdlib.h>

#define EPS 0.00001
#define N 10000

#pragma omp declare target
void init (float *a, float *b, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      a[i] = 0.1 * i;
      b[i] = 0.01 * i * i;
    }
}
#pragma omp end declare target

void vec_mult_ref(float *p, int n)
{
  float *v1, *v2;
  int i;

  v1 = (float *) malloc (n * sizeof (float));
  v2 = (float *) malloc (n * sizeof (float));

  init (v1, v2, n);

  for (i = 0; i < n; i++)
    p[i] = v1[i] * v2[i];

  free (v1);
  free (v2);
}

void vec_mult(float *p, int n)
{
  float *v1, *v2;
  int i;

  #pragma omp task shared(v1, v2) depend(out: v1, v2)
    #pragma omp target map(v1, v2)
      {
	if (omp_is_initial_device ())
	  abort ();

	v1 = (float *) malloc (n * sizeof (float));
	v2 = (float *) malloc (n * sizeof (float));

	init (v1, v2, n);
      }

  #pragma omp task shared(v1, v2) depend(in: v1, v2)
    #pragma omp target map(to: v1, v2) map(from: p[0:n])
      {
	if (omp_is_initial_device ())
	  abort ();

	#pragma omp parallel for
	  for (i = 0; i < n; i++)
	    p[i] = v1[i] * v2[i];

	  free (v1);
	  free (v2);
      }

  #pragma omp taskwait
}

void check (float *a, float *b, int n)
{
  int i;
  for (i = 0 ; i < n ; i++)
    {
      float err = (a[i] == 0.0) ? b[i] : (b[i] - a[i]) / a[i];
      if (((err > 0) ? err : -err) > EPS)
	abort ();
    }
}

int main ()
{
  float *p1 = (float *) malloc (N * sizeof (float));
  float *p2 = (float *) malloc (N * sizeof (float));

  vec_mult_ref (p1, N);
  vec_mult (p2, N);

  check (p1, p2, N);

  free (p1);
  free (p2);

  return 0;
}
