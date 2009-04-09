/* Implementation of the ALL intrinsic
   Copyright 2002, 2007, 2009 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

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
#include <stdlib.h>
#include <assert.h>


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

  len = array->dim[dim].ubound + 1 - array->dim[dim].lbound;
  if (len < 0)
    len = 0;

  delta = array->dim[dim].stride * src_kind;

  for (n = 0; n < dim; n++)
    {
      sstride[n] = array->dim[n].stride * src_kind;
      extent[n] = array->dim[n].ubound + 1 - array->dim[n].lbound;

      if (extent[n] < 0)
	extent[n] = 0;
    }
  for (n = dim; n < rank; n++)
    {
      sstride[n] = array->dim[n + 1].stride * src_kind;
      extent[n] =
        array->dim[n + 1].ubound + 1 - array->dim[n + 1].lbound;

      if (extent[n] < 0)
	extent[n] = 0;
    }

  if (retarray->data == NULL)
    {
      size_t alloc_size;

      for (n = 0; n < rank; n++)
        {
          retarray->dim[n].lbound = 0;
          retarray->dim[n].ubound = extent[n]-1;
          if (n == 0)
            retarray->dim[n].stride = 1;
          else
            retarray->dim[n].stride = retarray->dim[n-1].stride * extent[n-1];
        }

      retarray->offset = 0;
      retarray->dtype = (array->dtype & ~GFC_DTYPE_RANK_MASK) | rank;

      alloc_size = sizeof (GFC_LOGICAL_8) * retarray->dim[rank-1].stride
    		   * extent[rank-1];

      if (alloc_size == 0)
	{
	  /* Make sure we have a zero-sized array.  */
	  retarray->dim[0].lbound = 0;
	  retarray->dim[0].ubound = -1;
	  return;
	}
      else
	retarray->data = internal_malloc_size (alloc_size);
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

	      ret_extent = retarray->dim[n].ubound + 1
		- retarray->dim[n].lbound;
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
      dstride[n] = retarray->dim[n].stride;
      if (extent[n] <= 0)
        len = 0;
    }

  base = array->data;

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

  dest = retarray->data;

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
          if (n == rank)
            {
              /* Break out of the look.  */
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
