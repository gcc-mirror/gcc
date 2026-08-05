/* { dg-do run } */
/* { dg-require-effective-target offload_device_nonshared_as } */

/* Test transfer of dynamically-allocated arrays from target using map
   iterators.  */

#include <stdlib.h>

#define DIM1 8
#define DIM2 15

void mkarray (int *x[])
{
  for (int i = 0; i < DIM1; i++)
    x[i] = (int *) malloc (DIM2 * sizeof (int));
}

int main (void)
{
  int *x[DIM1];
  int y, expected;

  mkarray (x);

  #pragma omp target enter data map(alloc: x)
  #pragma omp target map(iterator(i=0:DIM1), from: x[i][:DIM2]) \
		     map(from: expected)
    {
      expected = 0;
      for (int i = 0; i < DIM1; i++)
	for (int j = 0; j < DIM2; j++)
	  {
	    x[i][j] = (i+1) * (j+1);
	    expected += x[i][j];
	  }
    }

  y = 0;
  for (int i = 0; i < DIM1; i++)
    for (int j = 0; j < DIM2; j++)
      y += x[i][j];

  return y - expected;
}
