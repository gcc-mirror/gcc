/* { dg-do run } */
/* { dg-additional-options "-ftree-parallelize-loops=2" } */

/* Constant bound, reduction.  */

#include <stdlib.h>

#define N 4000

unsigned int *a;

unsigned int
f (void)
{
  int i;
  unsigned int sum = 1;

  for (i = 0; i < N; ++i)
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
  res = f ();
  if (res != 11995)
    abort ();
  return 0;
}
