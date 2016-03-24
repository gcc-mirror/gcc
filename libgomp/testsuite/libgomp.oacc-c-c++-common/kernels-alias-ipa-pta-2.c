/* { dg-additional-options "-fipa-pta" } */

#include <stdlib.h>

#define N 2

int
main (void)
{
  unsigned int *a = (unsigned int *)malloc (N * sizeof (unsigned int));
  unsigned int *b = (unsigned int *)malloc (N * sizeof (unsigned int));
  unsigned int *c = (unsigned int *)malloc (N * sizeof (unsigned int));

#pragma acc kernels pcopyout (a[0:N], b[0:N], c[0:N])
  {
    a[0] = 0;
    b[0] = 1;
    c[0] = a[0];
  }

  if (a[0] != 0 || b[0] != 1 || c[0] != 0)
    abort ();

  free (a);
  free (b);
  free (c);
}
