#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>

/* Allocate and create a pointer based NDIMS-dimensional array,
   each dimension DIMLEN long, with ELSIZE sized data elements.  */
void *
create_ncarray (size_t elsize, int dimlen, int ndims)
{
  size_t blk_size = 0;
  size_t n = 1;

  for (int i = 0; i < ndims - 1; i++)
    {
      n *= dimlen;
      blk_size += sizeof (void *) * n;
    }
  size_t data_rows_num = n;
  size_t data_rows_offset = blk_size;
  blk_size += elsize * n * dimlen;

  void *blk = (void *) malloc (blk_size);
  memset (blk, 0, blk_size);
  void **curr_dim = (void **) blk;
  n = 1;

  for (int d = 0; d < ndims - 1; d++)
    {
      uintptr_t next_dim = (uintptr_t) (curr_dim + n * dimlen);
      size_t next_dimlen = dimlen * (d < ndims - 2 ? sizeof (void *) : elsize);

      for (int b = 0; b < n; b++)
        for (int i = 0; i < dimlen; i++)
	  if (d < ndims - 1)
	    curr_dim[b * dimlen + i]
	      = (void*) (next_dim + b * dimlen * next_dimlen + i * next_dimlen);

      n *= dimlen;
      curr_dim = (void**) next_dim;
    }
  assert (n == data_rows_num);
  return blk;
}
