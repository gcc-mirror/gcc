/* { dg-do run } */
/* { dg-require-effective-target offload_device_nonshared_as } */

/* Test transfer of dynamically-allocated arrays to target using map
   iterators.  */

#include <stdlib.h>

#define DIM1 8
#define DIM2 15

int mkarray (int *x[])
{
  int expected = 0;

  for (int i = 0; i < DIM1; i++)
    {
      x[i] = (int *) malloc (DIM2 * sizeof (int));
      for (int j = 0; j < DIM2; j++)
	{
	  x[i][j] = rand ();
	  expected += x[i][j];
	}
    }

  return expected;
}

int main (void)
{
  int *x[DIM1];
  int y;

  int expected = mkarray (x);

  #pragma omp target enter data map(to: x)
  #pragma omp target map(iterator(i=0:DIM1), to: x[i][:DIM2]) \
		     map(from: y)
    {
      y = 0;
      for (int i = 0; i < DIM1; i++)
	for (int j = 0; j < DIM2; j++)
	  y += x[i][j];
    }

  return y - expected;
}
