/* { dg-do run } */
/* { dg-require-effective-target offload_device_nonshared_as } */

/* Test transfer of dynamically-allocated arrays to target using map
   iterators, with multiple iterators and function calls in the iterator
   expression.  */

#include <stdlib.h>

#define DIM1 16
#define DIM2 15

int mkarrays (int *x[], int *y[])
{
  int expected = 0;

  for (int i = 0; i < DIM1; i++)
    {
      x[i] = (int *) malloc (DIM2 * sizeof (int));
      y[i] = (int *) malloc (sizeof (int));
      *y[i] = rand ();
      for (int j = 0; j < DIM2; j++)
	{
	  x[i][j] = rand ();
	  expected += x[i][j] * *y[i];
	}
    }

  return expected;
}

int f (int i, int j)
{
  return i * 4 + j;
}

int main (void)
{
  int *x[DIM1], *y[DIM1];
  int sum;

  int expected = mkarrays (x, y);

  #pragma omp target enter data map(to: x, y)
  #pragma omp target map(iterator(i=0:DIM1/4, j=0:4), to: x[f(i, j)][:DIM2]) \
		     map(iterator(i=0:DIM1), to: y[i][:1]) \
		     map(from: sum)
    {
      sum = 0;
      for (int i = 0; i < DIM1; i++)
	for (int j = 0; j < DIM2; j++)
	  sum += x[i][j] * y[i][0];
    }

  return sum - expected;
}
