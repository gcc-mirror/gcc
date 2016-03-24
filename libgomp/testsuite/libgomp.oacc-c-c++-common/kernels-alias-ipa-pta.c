/* { dg-additional-options "-fipa-pta" } */

#include <stdlib.h>

#define N 2

int
main (void)
{
  unsigned int a[N];
  unsigned int b[N];
  unsigned int c[N];

#pragma acc kernels pcopyout (a, b, c)
  {
    a[0] = 0;
    b[0] = 1;
    c[0] = a[0];
  }

  if (a[0] != 0 || b[0] != 1 || c[0] != 0)
    abort ();

  return 0;
}

