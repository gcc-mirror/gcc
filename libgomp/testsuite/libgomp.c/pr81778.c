/* Minimized from for-5.c.  */

#include <stdio.h>
#include <stdlib.h>

/* Size of array we want to write.  */
#define N 32

/* Size of extra space before and after.  */
#define CANARY_SIZE (N * 32)

/* Start of array we want to write.  */
#define BASE (CANARY_SIZE)

// Total size to be allocated.
#define ALLOC_SIZE (CANARY_SIZE + N + CANARY_SIZE)

#pragma omp declare target
int a[ALLOC_SIZE];
#pragma omp end declare target

int
main (void)
{
  /* Use variable step in for loop.  */
  int s = 1;

#pragma omp target update to(a)

  /* Write a[BASE] .. a[BASE + N - 1].  */
#pragma omp target simd
  for (int i = N - 1; i > -1; i -= s)
    a[BASE + i] = 1;

#pragma omp target update from(a)

  for (int i = 0; i < ALLOC_SIZE; i++)
    {
      int expected = (BASE <= i && i < BASE + N) ? 1 : 0;
      if (a[i] == expected)
	continue;

      printf ("Expected %d, got %d at base[%d]\n", expected, a[i], i - BASE);
      abort ();
    }

  return 0;
}
