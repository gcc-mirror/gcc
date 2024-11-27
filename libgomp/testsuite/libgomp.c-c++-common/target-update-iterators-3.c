/* { dg-do run } */
/* { dg-require-effective-target offload_device_nonshared_as } */

/* Test target enter data and target update to the target using map
   iterators with a function.  */

#include <stdlib.h>

#define DIM1 8
#define DIM2 15

void mkarray (int *x[])
{
  for (int i = 0; i < DIM1; i++)
    {
      x[i] = (int *) malloc (DIM2 * sizeof (int));
      for (int j = 0; j < DIM2; j++)
	x[i][j] = rand ();
    }
}

int f (int i)
{
  return i * 2;
}

int main (void)
{
  int *x[DIM1], x_new[DIM1][DIM2];
  int sum, expected;

  mkarray (x);

  #pragma omp target enter data map(alloc: x[:DIM1])
  #pragma omp target enter data map(iterator(i=0:DIM1), to: x[i][:DIM2])

  /* Update x on host.  */
  for (int i = 0; i < DIM1; i++)
    for (int j = 0; j < DIM2; j++)
      {
	x_new[i][j] = x[i][j];
	x[i][j] = (i + 1) * (j + 2);
      }

  /* Update a subset of x on target.  */
  #pragma omp target update to(iterator(i=0:DIM1/2): x[f (i)][:DIM2])

  #pragma omp target map(from: sum)
    {
      sum = 0;
      for (int i = 0; i < DIM1; i++)
	for (int j = 0; j < DIM2; j++)
	  sum += x[i][j];
    }

  /* Calculate expected value on host.  */
  for (int i = 0; i < DIM1/2; i++)
    for (int j = 0; j < DIM2; j++)
      x_new[f (i)][j] = x[f (i)][j];

  expected = 0;
  for (int i = 0; i < DIM1; i++)
    for (int j = 0; j < DIM2; j++)
      expected += x_new[i][j];

  return sum - expected;
}
