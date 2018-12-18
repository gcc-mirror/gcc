/* Test asynchronous, unstructed data regions, directives variant.  */
/* See also data-2-lib.c.  */

#include <stdlib.h>
#undef NDEBUG
#include <assert.h>

int
main (int argc, char **argv)
{
  int N = 12345;
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

#pragma acc enter data copyin (a[0:N]) async
#pragma acc enter data copyin (b[0:N]) async
#pragma acc enter data copyin (N) async

#pragma acc parallel present (a[0:N], b[0:N], N) async
#pragma acc loop
  for (i = 0; i < N; i++)
    b[i] = a[i];

#pragma acc update self (a[0:N]) async
#pragma acc update self (b[0:N]) async

#pragma acc wait

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

#pragma acc update device (a[0:N]) async (1)
#pragma acc update device (b[0:N]) async (1)

#pragma acc parallel present (a[0:N], b[0:N], N) async (1)
#pragma acc loop
  for (i = 0; i < N; i++)
    b[i] = a[i];

#pragma acc update self (a[0:N]) async (1)
#pragma acc update self (b[0:N]) async (1)

#pragma acc wait (1)
  /* Test unseen async-argument.  */
#pragma acc wait (10)

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

#pragma acc update device (a[0:N]) async (0)
#pragma acc update device (b[0:N]) async (1)
#pragma acc enter data copyin (c[0:N]) async (2)
#pragma acc enter data copyin (d[0:N]) async (3)

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

#pragma acc update self (a[0:N]) async (0)
#pragma acc update self (b[0:N]) async (1)
#pragma acc update self (c[0:N]) async (2)
#pragma acc update self (d[0:N]) async (3)

#pragma acc wait async (0)
#pragma acc wait (0)

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

#pragma acc update device (a[0:N]) async (10)
#pragma acc update device (b[0:N]) async (11)
#pragma acc update device (c[0:N]) async (12)
#pragma acc update device (d[0:N]) async (13)
#pragma acc enter data copyin (e[0:N]) async (14)

#pragma acc parallel present (a[0:N], b[0:N], N) wait (10) async (11)
  for (int ii = 0; ii < N; ii++)
    b[ii] = (a[ii] * a[ii] * a[ii]) / a[ii];

#pragma acc parallel present (a[0:N], c[0:N], N) wait (10) async (12)
  for (int ii = 0; ii < N; ii++)
    c[ii] = (a[ii] + a[ii] + a[ii] + a[ii]) / a[ii];

#pragma acc parallel present (a[0:N], d[0:N], N) wait (10) async (13)
  for (int ii = 0; ii < N; ii++)
    d[ii] = ((a[ii] * a[ii] + a[ii]) / a[ii]) - a[ii];

#pragma acc parallel present (a[0:N], b[0:N], c[0:N], d[0:N], e[0:N], N) wait (11) wait (12) wait (13) async (14)
  for (int ii = 0; ii < N; ii++)
    e[ii] = a[ii] + b[ii] + c[ii] + d[ii];

#pragma acc exit data copyout (a[0:N]) async (10)
#pragma acc exit data copyout (b[0:N]) async (11)
#pragma acc exit data copyout (c[0:N]) async (12)
#pragma acc exit data copyout (d[0:N]) async (13)
#pragma acc exit data copyout (e[0:N]) async (14)
#pragma acc exit data delete (N) async (15)
#pragma acc wait

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
