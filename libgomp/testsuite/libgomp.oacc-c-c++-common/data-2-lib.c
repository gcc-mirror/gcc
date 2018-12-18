/* Test asynchronous, unstructed data regions, runtime library variant.  */
/* See also data-2.c.  */

#include <stdlib.h>
#undef NDEBUG
#include <assert.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  int N = 12345;
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

  acc_copyin_async (a, nbytes, acc_async_noval);
  acc_copyin_async (b, nbytes, acc_async_noval);
  acc_copyin_async (&N, sizeof (int), acc_async_noval);
  
#pragma acc parallel present (a[0:N], b[0:N], N) async
#pragma acc loop
  for (i = 0; i < N; i++)
    b[i] = a[i];

  d_a = acc_deviceptr (a);
  acc_memcpy_from_device_async (a, d_a, nbytes, acc_async_noval);
  d_b = acc_deviceptr (b);
  acc_memcpy_from_device_async (b, d_b, nbytes, acc_async_noval);

  acc_wait (acc_async_noval);

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

  acc_update_device_async (a, nbytes, 1);
  acc_update_device_async (b, nbytes, 1);
  
#pragma acc parallel present (a[0:N], b[0:N], N) async (1)
#pragma acc loop
  for (i = 0; i < N; i++)
    b[i] = a[i];

  acc_memcpy_from_device_async (a, d_a, nbytes, 1);
  acc_memcpy_from_device_async (b, d_b, nbytes, 1);

  acc_wait (1);
  /* Test unseen async-argument.  */
  acc_wait (10);

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

  acc_update_device_async (a, nbytes, 0);
  acc_update_device_async (b, nbytes, 1);
  acc_copyin_async (c, nbytes, 2);
  acc_copyin_async (d, nbytes, 3);

#pragma acc parallel present (a[0:N], b[0:N], N) wait (0) async (1)
#pragma acc loop
  for (i = 0; i < N; i++)
    b[i] = (a[i] * a[i] * a[i]) / a[i];

#pragma acc parallel present (a[0:N], c[0:N], N) wait (0) async (2)
#pragma acc loop
  for (i = 0; i < N; i++)
    c[i] = (a[i] + a[i] + a[i] + a[i]) / a[i];

#pragma acc parallel present (a[0:N], d[0:N], N) wait (0) async (3)
#pragma acc loop
  for (i = 0; i < N; i++)
    d[i] = ((a[i] * a[i] + a[i]) / a[i]) - a[i];

  acc_memcpy_from_device_async (a, d_a, nbytes, 0);
  acc_memcpy_from_device_async (b, d_b, nbytes, 1);
  d_c = acc_deviceptr (c);
  acc_memcpy_from_device_async (c, d_c, nbytes, 2);
  d_d = acc_deviceptr (d);
  acc_memcpy_from_device_async (d, d_d, nbytes, 3);
  
  acc_wait_all_async (0);
  acc_wait (0);
  
  for (i = 0; i < N; i++)
    {
      assert (a[i] == 3.0);
      assert (b[i] == 9.0);
      assert (c[i] == 4.0);
      assert (d[i] == 1.0);
    }

  for (i = 0; i < N; i++)
    {
      a[i] = 2.0;
      b[i] = 0.0;
      c[i] = 0.0;
      d[i] = 0.0;
      e[i] = 0.0;
    }

  acc_update_device_async (a, nbytes, 10);
  acc_update_device_async (b, nbytes, 11);
  acc_update_device_async (c, nbytes, 12);
  acc_update_device_async (d, nbytes, 13);
  acc_copyin_async (e, nbytes, 14);

#pragma acc parallel present (a[0:N], b[0:N], N) wait (10) async (11)
  for (int ii = 0; ii < N; ii++)
    b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];

#pragma acc parallel present (a[0:N], c[0:N], N) wait (10) async (12)
  for (int ii = 0; ii < N; ii++)
    c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];

#pragma acc parallel present (a[0:N], d[0:N], N) wait (10) async (13)
  for (int ii = 0; ii < N; ii++)
    d[ii] = ((a[ii] * a[ii] + a[ii]) / a[ii]) - a[ii];

#pragma acc parallel present (a[0:N], b[0:N], c[0:N], d[0:N], e[0:N], N)  wait (11) wait (12) wait (13) async (14)
  for (int ii = 0; ii < N; ii++)
    e[ii] = a[ii] + b[ii] + c[ii] + d[ii];

  acc_copyout_async (a, nbytes, 10);
  acc_copyout_async (b, nbytes, 11);
  acc_copyout_async (c, nbytes, 12);
  acc_copyout_async (d, nbytes, 13);
  acc_copyout_async (e, nbytes, 14);
  acc_delete_async (&N, sizeof (int), 15);
  acc_wait_all ();

  for (i = 0; i < N; i++)
    {
      assert (a[i] == 2.0);
      assert (b[i] == 4.0);
      assert (c[i] == 4.0);
      assert (d[i] == 1.0);
      assert (e[i] == 11.0);
    }

  return 0;
}
