/* { dg-do run } */
/* { dg-require-effective-target offload_device_nonshared_as } */

/* Test target enter data and target update from the target using map
   iterators.  */

#include <stdlib.h>

#define DIM1 8
#define DIM2 15

void mkarray (int *x[])
{
  for (int i = 0; i < DIM1; i++)
    {
      x[i] = (int *) malloc (DIM2 * sizeof (int));
      for (int j = 0; j < DIM2; j++)
	x[i][j] = 0;
    }
}

int main (void)
{
  int *x[DIM1];
  int sum, expected;

  mkarray (x);

  #pragma omp target enter data map(alloc: x[:DIM1])
  #pragma omp target enter data map(iterator(i=0:DIM1), to: x[i][:DIM2])
  #pragma omp target map(from: expected)
    {
      expected = 0;
      for (int i = 0; i < DIM1; i++)
	for (int j = 0; j < DIM2; j++)
	  {
	    x[i][j] = (i + 1) * (j + 2);
	    expected += x[i][j];
	  }
    }

  /* Host copy of x should remain unchanged.  */
  sum = 0;
  for (int i = 0; i < DIM1; i++)
    for (int j = 0; j < DIM2; j++)
      sum += x[i][j];
  if (sum != 0)
    return 1;

  #pragma omp target update from(iterator(i=0:DIM1): x[i][:DIM2])

  /* Host copy should now be updated.  */
  sum = 0;
  for (int i = 0; i < DIM1; i++)
    for (int j = 0; j < DIM2; j++)
      sum += x[i][j];
  return sum - expected;
}
