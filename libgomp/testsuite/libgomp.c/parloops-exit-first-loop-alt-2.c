/* { dg-do run } */
/* { dg-additional-options "-ftree-parallelize-loops=2" } */

/* Constant bound, vector addition.  */

#include <stdio.h>
#include <stdlib.h>

#define N 1000

unsigned int a[N];
unsigned int b[N];
unsigned int c[N];

void __attribute__((noclone,noinline))
f (void)
{
  int i;

  for (i = 0; i < N; ++i)
    c[i] = a[i] + b[i];
}

int
main (void)
{
  int i, j;

  /* Complexify loop to inhibit parloops.  */
  for (j = 0; j < 100; ++j)
    for (i = 0; i < 10; i++)
      {
	int k = i + (10 * j);
	a[k] = k;
	b[k] = (k * 3) % 7;
	c[k] = k * 2;
      }

  f ();

  for (i = 0; i < N; i++)
    {
      unsigned int actual = c[i];
      unsigned int expected = i + ((i * 3) % 7);
      if (actual != expected)
	abort ();
    }

  return 0;
}
