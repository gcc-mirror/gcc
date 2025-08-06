/* { dg-do run } */

/* Test target enter data and target update to the target using map
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
  int sum;
  int expected = mkarray (x);

  #pragma omp target enter data map(to: x[:DIM1])
  #pragma omp target enter data map(iterator(i=0:DIM1), to: x[i][:DIM2])
  #pragma omp target map(from: sum)
    {
      sum = 0;
      for (int i = 0; i < DIM1; i++)
	for (int j = 0; j < DIM2; j++)
	  sum += x[i][j];
    }

  if (sum != expected)
    return 1;

  expected = 0;
  for (int i = 0; i < DIM1; i++)
    for (int j = 0; j < DIM2; j++)
      {
	x[i][j] *= rand ();
	expected += x[i][j];
      }

  #pragma omp target update to(iterator(i=0:DIM1): x[i][:DIM2])

  #pragma omp target map(from: sum)
    {
      sum = 0;
      for (int i = 0; i < DIM1; i++)
	for (int j = 0; j < DIM2; j++)
	  sum += x[i][j];
    }

  return sum != expected;
}
