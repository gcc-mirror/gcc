/* { dg-do run } */

#include <stdlib.h>

#define n 10

static void __attribute__((noinline)) __attribute__((noclone))
foo (int beta, int *c)
{
  #pragma acc parallel copy(c[0:(n * n) - 1]) num_gangs(2)
  #pragma acc loop gang
  for (int j = 0; j < n; ++j)
    if (beta != 1)
      {
        #pragma acc loop vector
	for (int i = 0; i < n; ++i)
	  c[i + (j * n)] = 0;
      }
}

int
main (void)
{
  int c[n * n];

  c[0] = 1;
  foo (0, c);
  if (c[0] != 0)
    abort ();

  return 0;
}
