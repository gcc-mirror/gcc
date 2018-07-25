/* Implementation of the ALL intrinsic
   Copyright (C) 2002-2018 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgfortran.h"


#if defined (HAVE_GFC_LOGICAL_8)


extern void all_l8 (gfc_array_l8 * const restrict, 
	gfc_array_l1 * const restrict, const index_type * const restrict);
export_proto(all_l8);

void
all_l8 (gfc_array_l8 * const restrict retarray, 
	gfc_array_l1 * const restrict array, 
	const index_type * const restrict pdim)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type dstride[GFC_MAX_DIMENSIONS];
  const GFC_LOGICAL_1 * restrict base;
  GFC_LOGICAL_8 * restrict dest;
  index_type rank;
  index_type n;
  index_type len;
  index_type delta;
  index_type dim;
  int src_kind;
  int continue_loop;

  /* Make dim zero based to avoid confusion.  */
  dim = (*pdim) - 1;
  rank = GFC_DESCRIPTOR_RANK (array) - 1;

  src_kind = GFC_DESCRIPTOR_SIZE (array);

  len = GFC_DESCRIPTOR_EXTENT(array,dim);
  if (len < 0)
    len = 0;

  delta = GFC_DESCRIPTOR_STRIDE_BYTES(array,dim);

  for (n = 0; n < dim; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(array,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);

      if (extent[n] < 0)
	extent[n] = 0;
    }
  for (n = dim; n < rank; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(array,n + 1);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n + 1);

      if (extent[n] < 0)
	extent[n] = 0;
    }

  if (retarray->base_addr == NULL)
    {
      size_t alloc_size, str;

      for (n = 0; n < rank; n++)
        {
          if (n == 0)
            str = 1;
          else
            str = GFC_DESCRIPTOR_STRIDE(retarray,n-1) * extent[n-1];

	  GFC_DIMENSION_SET(retarray->dim[n], 0, extent[n] - 1, str);

        }

      retarray->offset = 0;
      retarray->dtype.rank = rank;

      alloc_size = GFC_DESCRIPTOR_STRIDE(retarray,rank-1) * extent[rank-1];

      if (alloc_size == 0)
	{
	  /* Make sure we have a zero-sized array.  */
	  GFC_DIMENSION_SET(retarray->dim[0], 0, -1, 1);
	  return;
	}
      else
	retarray->base_addr = xmallocarray (alloc_size, sizeof (GFC_LOGICAL_8));
    }
  else
    {
      if (rank != GFC_DESCRIPTOR_RANK (retarray))
	runtime_error ("rank of return array incorrect in"
		       " ALL intrinsic: is %ld, should be %ld",
		       (long int) GFC_DESCRIPTOR_RANK (retarray),
		       (long int) rank);

      if (unlikely (compile_options.bounds_check))
	{
	  for (n=0; n < rank; n++)
	    {
	      index_type ret_extent;

	      ret_extent = GFC_DESCRIPTOR_EXTENT(retarray,n);
	      if (extent[n] != ret_extent)
		runtime_error ("Incorrect extent in return value of"
			       " ALL intrinsic in dimension %d:"
			       " is %ld, should be %ld", (int) n + 1,
			       (long int) ret_extent, (long int) extent[n]);
	    }
	}
    }

  for (n = 0; n < rank; n++)
    {
      count[n] = 0;
      dstride[n] = GFC_DESCRIPTOR_STRIDE(retarray,n);
      if (extent[n] <= 0)
	return;
    }

  base = array->base_addr;

  if (src_kind == 1 || src_kind == 2 || src_kind == 4 || src_kind == 8
#ifdef HAVE_GFC_LOGICAL_16
      || src_kind == 16
#endif
    )
    {
      if (base)
	base = GFOR_POINTER_TO_L1 (base, src_kind);
    }
  else
    internal_error (NULL, "Funny sized logical array in ALL intrinsic");

  dest = retarray->base_addr;

  continue_loop = 1;
  while (continue_loop)
    {
      const GFC_LOGICAL_1 * restrict src;
      GFC_LOGICAL_8 result;
      src = base;
      {

  /* Return true only if all the elements are set.  */
  result = 1;
        if (len <= 0)
	  *dest = 1;
	else
	  {
	    for (n = 0; n < len; n++, src += delta)
	      {

  if (! *src)
    {
      result = 0;
      break;
    }
          }
	    *dest = result;
	  }
      }
      /* Advance to the next element.  */
      count[0]++;
      base += sstride[0];
      dest += dstride[0];
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          base -= sstride[n] * extent[n];
          dest -= dstride[n] * extent[n];
          n++;
          if (n >= rank)
            {
              /* Break out of the loop.  */
              continue_loop = 0;
              break;
            }
          else
            {
              count[n]++;
              base += sstride[n];
              dest += dstride[n];
            }
        }
    }
}

#endif
