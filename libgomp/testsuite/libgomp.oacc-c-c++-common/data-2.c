/* Test 'acc enter/exit data' regions.  */

/* { dg-do run } */
/* { dg-xfail-run-if "TODO" { openacc_nvidia_accel_selected } } */

#include <stdlib.h>

int
main (int argc, char **argv)
{
  int N = 128; //1024 * 1024;
  float *a, *b, *c, *d, *e;
  int i;
  int nbytes;

  nbytes = N * sizeof (float);

  a = (float *) malloc (nbytes);
  b = (float *) malloc (nbytes);
  c = (float *) malloc (nbytes);
  d = (float *) malloc (nbytes);
  e = (float *) malloc (nbytes);

  for (i = 0; i < N; i++)
    {
      a[i] = 3.0;
      b[i] = 0.0;
    }

#pragma acc enter data copyin (a[0:N]) copyin (b[0:N]) copyin (N) async
#pragma acc parallel present (a[0:N], b[0:N]) async wait
#pragma acc loop
  for (i = 0; i < N; i++)
    b[i] = a[i];

#pragma acc exit data copyout (a[0:N]) copyout (b[0:N]) wait async
#pragma acc wait

  for (i = 0; i < N; i++)
    {
      if (a[i] != 3.0)
	abort ();

      if (b[i] != 3.0)
	abort ();
    }

  for (i = 0; i < N; i++)
    {
      a[i] = 3.0;
      b[i] = 0.0;
    }

#pragma acc enter data copyin (a[0:N]) async 
#pragma acc enter data copyin (b[0:N]) async wait
#pragma acc enter data copyin (N) async wait
#pragma acc parallel async wait
#pragma acc loop
  for (i = 0; i < N; i++)
    b[i] = a[i];

#pragma acc exit data copyout (a[0:N]) copyout (b[0:N]) delete (N) wait async
#pragma acc wait

  for (i = 0; i < N; i++)
    {
      if (a[i] != 3.0)
	abort ();

      if (b[i] != 3.0)
	abort ();
    }

  for (i = 0; i < N; i++)
    {
      a[i] = 2.0;
      b[i] = 0.0;
    }

#pragma acc enter data copyin (a[0:N]) copyin (b[0:N]) copyin (N) async (1)
#pragma acc parallel present (a[0:N], b[0:N])  async (1)
#pragma acc loop
  for (i = 0; i < N; i++)
    b[i] = a[i];

#pragma acc exit data copyout (a[0:N]) copyout (b[0:N]) wait (1) async (1)
#pragma acc wait (1)

  for (i = 0; i < N; i++)
    {
      if (a[i] != 2.0)
	abort ();

      if (b[i] != 2.0)
	abort ();
    }

  for (i = 0; i < N; i++)
    {
      a[i] = 3.0;
      b[i] = 0.0;
      c[i] = 0.0;
      d[i] = 0.0;
    }

#pragma acc enter data copyin (a[0:N]) copyin (b[0:N]) copyin (c[0:N]) copyin (d[0:N]) copyin (N) async (1)

#pragma acc parallel present (a[0:N], b[0:N]) async (1) wait (1)
#pragma acc loop
  for (i = 0; i < N; i++)
    b[i] = (a[i] * a[i] * a[i]) / a[i];

#pragma acc parallel present (a[0:N], c[0:N]) async (2) wait (1)
#pragma acc loop
  for (i = 0; i < N; i++)
    c[i] = (a[i] + a[i] + a[i] + a[i]) / a[i];

#pragma acc parallel present (a[0:N], d[0:N]) async (3) wait (1)
#pragma acc loop
  for (i = 0; i < N; i++)
    d[i] = ((a[i] * a[i] + a[i]) / a[i]) - a[i];

#pragma acc exit data copyout (a[0:N]) copyout (b[0:N]) copyout (c[0:N]) copyout (d[0:N]) wait (1, 2, 3) async (1)
#pragma acc wait (1)

  for (i = 0; i < N; i++)
    {
      if (a[i] != 3.0)
	abort ();

      if (b[i] != 9.0)
	abort ();

      if (c[i] != 4.0)
	abort ();

      if (d[i] != 1.0)
	abort ();
    }

  for (i = 0; i < N; i++)
    {
      a[i] = 2.0;
      b[i] = 0.0;
      c[i] = 0.0;
      d[i] = 0.0;
      e[i] = 0.0;
    }

#pragma acc enter data copyin (a[0:N]) copyin (b[0:N]) copyin (c[0:N]) copyin (d[0:N]) copyin (e[0:N]) copyin (N) async (1)

#pragma acc parallel present (a[0:N], b[0:N]) async (1) wait (1)
  for (int ii = 0; ii < N; ii++)
    b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];

#pragma acc parallel present (a[0:N], c[0:N]) async (2) wait (1)
  for (int ii = 0; ii < N; ii++)
    c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];

#pragma acc parallel present (a[0:N], d[0:N]) async (3) wait (1)
  for (int ii = 0; ii < N; ii++)
    d[ii] = ((a[ii] * a[ii] + a[ii]) / a[ii]) - a[ii];

#pragma acc parallel present (a[0:N], b[0:N], c[0:N], d[0:N], e[0:N]) \
  wait (1) async (4)
  for (int ii = 0; ii < N; ii++)
    e[ii] = a[ii] + b[ii] + c[ii] + d[ii];

#pragma acc exit data copyout (a[0:N]) copyout (b[0:N]) copyout (c[0:N]) \
  copyout (d[0:N]) copyout (e[0:N]) wait (1, 2, 3, 4) async (1)
#pragma acc wait (1)

  for (i = 0; i < N; i++)
    {
      if (a[i] != 2.0)
	abort ();

      if (b[i] != 4.0)
	abort ();

      if (c[i] != 4.0)
	abort ();

      if (d[i] != 1.0)
	abort ();

      if (e[i] != 11.0)
	abort ();
    }

  return 0;
}
