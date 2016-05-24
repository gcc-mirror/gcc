/* This test is similar to data-2.c, but it uses acc_* library functions
   to move data.  */

/* { dg-do run } */

#include <stdlib.h>
#include <assert.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  int N = 128; //1024 * 1024;
  float *a, *b, *c, *d, *e;
  void *d_a, *d_b, *d_c, *d_d;
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

  d_a = acc_copyin (a, nbytes);
  d_b = acc_copyin (b, nbytes);
  acc_copyin (&N, sizeof (int));
  
#pragma acc parallel present (a[0:N], b[0:N], N) async wait
#pragma acc loop
  for (i = 0; i < N; i++)
    b[i] = a[i];

  acc_wait_all ();

  acc_memcpy_from_device (a, d_a, nbytes);
  acc_memcpy_from_device (b, d_b, nbytes);

  for (i = 0; i < N; i++)
    {
      assert (a[i] == 3.0);
      assert (b[i] == 3.0);
    }

  for (i = 0; i < N; i++)
    {
      a[i] = 2.0;
      b[i] = 0.0;
    }

  acc_update_device (a, nbytes);
  acc_update_device (b, nbytes);
  
#pragma acc parallel present (a[0:N], b[0:N], N)  async (1)
#pragma acc loop
  for (i = 0; i < N; i++)
    b[i] = a[i];

  acc_memcpy_from_device (a, d_a, nbytes);
  acc_memcpy_from_device (b, d_b, nbytes);
  
  for (i = 0; i < N; i++)
    {
      assert (a[i] == 2.0);
      assert (b[i] == 2.0);
    }

  for (i = 0; i < N; i++)
    {
      a[i] = 3.0;
      b[i] = 0.0;
      c[i] = 0.0;
      d[i] = 0.0;
    }

  acc_update_device (a, nbytes);
  acc_update_device (b, nbytes);
  d_c = acc_copyin (c, nbytes);
  d_d = acc_copyin (d, nbytes);

#pragma acc parallel present (a[0:N], b[0:N], N) async (1)
#pragma acc loop
  for (i = 0; i < N; i++)
    b[i] = (a[i] * a[i] * a[i]) / a[i];

#pragma acc parallel present (a[0:N], c[0:N], N) async (2)
#pragma acc loop
  for (i = 0; i < N; i++)
    c[i] = (a[i] + a[i] + a[i] + a[i]) / a[i];

#pragma acc parallel present (a[0:N], d[0:N], N) async (3)
#pragma acc loop
  for (i = 0; i < N; i++)
    d[i] = ((a[i] * a[i] + a[i]) / a[i]) - a[i];

  acc_wait_all ();
  
  acc_memcpy_from_device (a, d_a, nbytes);
  acc_memcpy_from_device (b, d_b, nbytes);
  acc_memcpy_from_device (c, d_c, nbytes);
  acc_memcpy_from_device (d, d_d, nbytes);
  
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

  acc_update_device (a, nbytes);
  acc_update_device (b, nbytes);
  acc_update_device (c, nbytes);
  acc_update_device (d, nbytes);
  acc_copyin (e, nbytes);

#pragma acc parallel present (a[0:N], b[0:N], N) async (1)
  for (int ii = 0; ii < N; ii++)
    b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];

#pragma acc parallel present (a[0:N], c[0:N], N) async (2)
  for (int ii = 0; ii < N; ii++)
    c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];

#pragma acc parallel present (a[0:N], d[0:N], N) async (3)
  for (int ii = 0; ii < N; ii++)
    d[ii] = ((a[ii] * a[ii] + a[ii]) / a[ii]) - a[ii];

#pragma acc parallel present (a[0:N], b[0:N], c[0:N], d[0:N], e[0:N], N) \
  async (4)
  for (int ii = 0; ii < N; ii++)
    e[ii] = a[ii] + b[ii] + c[ii] + d[ii];

  acc_wait_all ();
  acc_copyout (a, nbytes);
  acc_copyout (b, nbytes);
  acc_copyout (c, nbytes); 
  acc_copyout (d, nbytes);
  acc_copyout (e, nbytes);
  acc_delete (&N, sizeof (int));

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
