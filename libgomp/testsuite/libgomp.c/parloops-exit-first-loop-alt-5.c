/* { dg-do run } */
/* { dg-additional-options "-ftree-parallelize-loops=2" } */

/* Variable bound, vector addition, unsigned loop counter, unsigned bound.  */

#include <stdio.h>
#include <stdlib.h>

#define N 1000

unsigned int a[N];
unsigned int b[N];
unsigned int c[N];

void __attribute__((noclone,noinline))
f (unsigned int n, unsigned int *__restrict__ a, unsigned int *__restrict__ b,
   unsigned int *__restrict__ c)
{
  unsigned int i;

  for (i = 0; i < n; ++i)
    c[i] = a[i] + b[i];
}

static void __attribute__((noclone,noinline))
init (void)
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
}

int
main (void)
{
  int i;

  init ();

  f (N, a, b, c);

  for (i = 0; i < N; i++)
    {
      unsigned int actual = c[i];
      unsigned int expected = i + ((i * 3) % 7);
      if (actual != expected)
	abort ();
    }

  /* Test low iteration count case.  */

  init ();

  f (10, a, b, c);

  for (i = 0; i < N; i++)
    {
      unsigned int actual = c[i];
      unsigned int expected = (i < 10
			       ? i + ((i * 3) % 7)
			       : i * 2);
      if (actual != expected)
	abort ();
    }

  return 0;
}
