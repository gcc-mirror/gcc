/* PR tree-optimization/85063 */
/* { dg-additional-options "-ftree-switch-conversion" } */

#include <stdlib.h>

#pragma omp declare target
static int __attribute__((noinline)) foo (int n)
{
  switch (n & 3)
    {
    case 0: return 4;
    case 1: return 3;
    case 2: return 2;
    default:
      return 1;
    }
}
#pragma omp end declare target

int
main (void)
{
  int n[1];

  n[0] = 4;

#pragma omp target
  {
    n[0] = foo (n[0]);
  }

  if (n[0] != 4)
    abort ();

  return 0;
}
