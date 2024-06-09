#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#ifndef FUN_NAME_SUFFIX
#define FUN_NAME_SUFFIX
#endif

#ifdef MULT
#undef MULT
#endif
#define MULT CAT(mult, FUN_NAME_SUFFIX)

#ifdef MAIN
#undef MAIN
#endif
#define MAIN CAT(main, FUN_NAME_SUFFIX)

void
MULT (float *matrix1, float *matrix2, float *result,
      unsigned dim0, unsigned dim1)
{
  memset (result, 0, sizeof (float) * dim0 * dim1);
  DIRECTIVE
  TRANSFORMATION1
  for (unsigned i = 0; i < dim0; i++)
    TRANSFORMATION2
    for (unsigned j = 0; j < dim1; j++)
      TRANSFORMATION3
      for (unsigned k = 0; k < dim1; k++)
	result[i * dim1 + j] += matrix1[i * dim1 + k] * matrix2[k * dim0 + j];
}

int
MAIN ()
{
  unsigned dim0 = 20;
  unsigned dim1 = 20;

  float *result = (float *) malloc (sizeof (float) * dim0 * dim1);
  float *matrix1 = (float *) malloc (sizeof (float) * dim0 * dim1);
  float *matrix2 = (float *) malloc (sizeof (float) * dim0 * dim1);

  for (unsigned i = 0; i < dim0; i++)
    for (unsigned j = 0; j < dim1; j++)
      matrix1[i * dim1 + j] = j;

  for (unsigned i = 0; i < dim0; i++)
    for (unsigned j = 0; j < dim1; j++)
      if (i == j)
	matrix2[i * dim1 + j] = 1;
      else
	matrix2[i * dim1 + j] = 0;

  MULT (matrix1, matrix2, result, dim0, dim1);

  for (unsigned i = 0; i < dim0; i++)
    for (unsigned j = 0; j < dim1; j++)
      {
	if (matrix1[i * dim1 + j] != result[i * dim1 + j])
	  {
	    print_matrix (matrix1, dim0, dim1);
	    print_matrix (matrix2, dim0, dim1);
	    print_matrix (result, dim0, dim1);
	    fprintf (stderr, "%s: ERROR at %d, %d\n", __FUNCTION__, i, j);
	    abort ();
	  }
      }

  free (matrix2);
  free (matrix1);
  free (result);

  return 0;
}

#undef DIRECTIVE
#undef TRANSFORMATION1
#undef TRANSFORMATION2
#undef TRANSFORMATION3
#undef FUN_NAME_SUFFIX
