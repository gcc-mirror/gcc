#include <stdio.h>
#include <stdlib.h>

#define CAT(x,y) XCAT(x,y)
#define XCAT(x,y) x ## y
#define DO_PRAGMA(x) XDO_PRAGMA(x)
#define XDO_PRAGMA(x) _Pragma (#x)

void
print_matrix (float *matrix, unsigned dim0, unsigned dim1)
{
  for (unsigned i = 0; i < dim0; i++)
    {
      for (unsigned j = 0; j < dim1; j++)
	fprintf (stderr, "%f ", matrix[i * dim1 + j]);
      fprintf (stderr, "\n");
    }
  fprintf (stderr, "\n");
}
