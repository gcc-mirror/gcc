/* { dg-do run } */

#include <stdlib.h>

#define N 1024

unsigned int a[N];
unsigned int b[N];
unsigned int c[N];
unsigned int n = N;

int
main (void)
{
  for (unsigned int i = 0; i < n; ++i)
    {
      a[i] = i % 3;
      b[i] = i % 5;
    }

#pragma acc parallel vector_length (32) copyin (a,b) copyout (c)
  {
#pragma acc loop vector
    for (unsigned int i = 0; i < n; i++)
      c[i] = a[i] + b[i];
  }

  for (unsigned int i = 0; i < n; ++i)
    if (c[i] != (i % 3) + (i % 5))
      abort ();

  return 0;
}
