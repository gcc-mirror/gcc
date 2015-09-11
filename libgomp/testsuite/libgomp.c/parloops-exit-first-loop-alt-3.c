/* { dg-do run } */
/* { dg-additional-options "-ftree-parallelize-loops=2" } */

/* Variable bound, reduction.  */

#include <stdlib.h>

#define N 4000

unsigned int *a;

unsigned int __attribute__((noclone,noinline))
f (unsigned int n, unsigned int *__restrict__ a)
{
  int i;
  unsigned int sum = 1;

  for (i = 0; i < n; ++i)
    sum += a[i];

  return sum;
}

int
main (void)
{
  unsigned int res;
  unsigned int array[N];
  int i;

  for (i = 0; i < N; ++i)
    array[i] = i % 7;
  a = &array[0];

  res = f (N, a);
  if (res != 11995)
    abort ();

  /* Test low iteration count case.  */
  res = f (10, a);
  if (res != 25)
    abort ();

  return 0;
}
