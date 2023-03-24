#include <string.h>
#include <stdio.h>
#include <math.h>

void
mult (float *matrix1, float *matrix2, float *result, unsigned dim0,
      unsigned dim1)
{
  memset (result, 0, sizeof (float) * dim0 * dim1);
#pragma omp target parallel for collapse(3)
#pragma omp tile sizes(8, 16, 4)
  for (unsigned i = 0; i < dim0; i++)
    for (unsigned j = 0; j < dim1; j++)
      for (unsigned k = 0; k < dim1; k++)
	result[i * dim1 + j] += matrix1[i * dim1 + k] * matrix2[k * dim0 + j];
}

int
main ()
{
  unsigned dim0 = 20;
  unsigned dim1 = 20;

  float *result = (float *)malloc (sizeof (float) * dim0 * dim1);
  float *matrix1 = (float *)malloc (sizeof (float) * dim0 * dim1);
  float *matrix2 = (float *)malloc (sizeof (float) * dim0 * dim1);

  for (unsigned i = 0; i < dim0; i++)
    for (unsigned j = 0; j < dim1; j++)
      matrix1[i * dim1 + j] = j;

  for (unsigned i = 0; i < dim1; i++)
    for (unsigned j = 0; j < dim0; j++)
      if (i == j)
	matrix2[i * dim0 + j] = 1;
      else
	matrix2[i * dim0 + j] = 0;

  mult (matrix1, matrix2, result, dim0, dim1);

  for (unsigned i = 0; i < dim0; i++)
    for (unsigned j = 0; j < dim1; j++)
      {
	if (matrix1[i * dim1 + j] != result[i * dim1 + j])
	  {
	    printf ("ERROR at %d, %d\n", i, j);
	    __builtin_abort ();
	  }
      }

  return 0;
}
