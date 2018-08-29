/* { dg-do run } */

/* Based on asyncwait-2.f90.  */

#include <stdlib.h>

#define N 64

int *a, *b, *c;

int
main (void)
{
  a = (int *)malloc (N * sizeof (*a));
  b = (int *)malloc (N * sizeof (*b));
  c = (int *)malloc (N * sizeof (*c));

#pragma acc parallel copy (a[0:N]) async (0)
#pragma acc loop
  for (int i = 0; i < N; ++i)
    a[i] = 1;

#pragma acc parallel copy (b[0:N]) async (1)
#pragma acc loop
  for (int i = 0; i < N; ++i)
    b[i] = 1;

#pragma acc parallel copy (a[0:N], b[0:N], c[0:N]) wait (0, 1)
#pragma acc loop
  for (int i = 0; i < N; ++i)
    c[i] = a[i] + b[i];

  for (int i = 0; i < N; ++i)
    if (c[i] != 2)
      abort ();

#if 1
#pragma acc kernels copy (a[0:N]) async (0)
#pragma acc loop
  for (int i = 0; i < N; ++i)
    a[i] = 1;

#pragma acc kernels copy (b[0:N]) async (1)
#pragma acc loop
  for (int i = 0; i < N; ++i)
    b[i] = 1;

#pragma acc kernels copy (a[0:N], b[0:N], c[0:N]) wait (0, 1)
#pragma acc loop
  for (int i = 0; i < N; ++i)
    c[i] = a[i] + b[i];

  for (int i = 0; i < N; ++i)
    if (c[i] != 2)
      abort ();
#endif

  free (a);
  free (b);
  free (c);
}
