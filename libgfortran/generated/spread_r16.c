/* Special implementation of the SPREAD intrinsic
   Copyright 2008, 2009 Free Software Foundation, Inc.
   Contributed by Thomas Koenig <tkoenig@gcc.gnu.org>, based on
   spread_generic.c written by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Ligbfortran is distributed in the hope that it will be useful,
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
#include <string.h>


#if defined (HAVE_GFC_REAL_16)

void
spread_r16 (gfc_array_r16 *ret, const gfc_array_r16 *source,
		 const index_type along, const index_type pncopies)
{
  /* r.* indicates the return array.  */
  index_type rstride[GFC_MAX_DIMENSIONS];
  index_type rstride0;
  index_type rdelta = 0;
  index_type rrank;
  index_type rs;
  GFC_REAL_16 *rptr;
  GFC_REAL_16 * restrict dest;
  /* s.* indicates the source array.  */
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type sstride0;
  index_type srank;
  const GFC_REAL_16 *sptr;

  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type n;
  index_type dim;
  index_type ncopies;

  srank = GFC_DESCRIPTOR_RANK(source);

  rrank = srank + 1;
  if (rrank > GFC_MAX_DIMENSIONS)
    runtime_error ("return rank too large in spread()");

  if (along > rrank)
      runtime_error ("dim outside of rank in spread()");

  ncopies = pncopies;

  if (ret->data == NULL)
    {
      /* The front end has signalled that we need to populate the
	 return array descriptor.  */
      ret->dtype = (source->dtype & ~GFC_DTYPE_RANK_MASK) | rrank;
      dim = 0;
      rs = 1;
      for (n = 0; n < rrank; n++)
	{
	  ret->dim[n].stride = rs;
	  ret->dim[n].lbound = 0;
	  if (n == along - 1)
	    {
	      ret->dim[n].ubound = ncopies - 1;
	      rdelta = rs;
	      rs *= ncopies;
	    }
	  else
	    {
	      count[dim] = 0;
	      extent[dim] = source->dim[dim].ubound + 1
		- source->dim[dim].lbound;
	      sstride[dim] = source->dim[dim].stride;
	      rstride[dim] = rs;

	      ret->dim[n].ubound = extent[dim]-1;
	      rs *= extent[dim];
	      dim++;
	    }
	}
      ret->offset = 0;
      if (rs > 0)
        ret->data = internal_malloc_size (rs * sizeof(GFC_REAL_16));
      else
	{
	  ret->data = internal_malloc_size (1);
	  return;
	}
    }
  else
    {
      int zero_sized;

      zero_sized = 0;

      dim = 0;
      if (GFC_DESCRIPTOR_RANK(ret) != rrank)
	runtime_error ("rank mismatch in spread()");

      if (unlikely (compile_options.bounds_check))
	{
	  for (n = 0; n < rrank; n++)
	    {
	      index_type ret_extent;

	      ret_extent = ret->dim[n].ubound + 1 - ret->dim[n].lbound;
	      if (n == along - 1)
		{
		  rdelta = ret->dim[n].stride;

		  if (ret_extent != ncopies)
		    runtime_error("Incorrect extent in return value of SPREAD"
				  " intrinsic in dimension %ld: is %ld,"
				  " should be %ld", (long int) n+1,
				  (long int) ret_extent, (long int) ncopies);
		}
	      else
		{
		  count[dim] = 0;
		  extent[dim] = source->dim[dim].ubound + 1
		    - source->dim[dim].lbound;
		  if (ret_extent != extent[dim])
		    runtime_error("Incorrect extent in return value of SPREAD"
				  " intrinsic in dimension %ld: is %ld,"
				  " should be %ld", (long int) n+1,
				  (long int) ret_extent,
				  (long int) extent[dim]);
		    
		  if (extent[dim] <= 0)
		    zero_sized = 1;
		  sstride[dim] = source->dim[dim].stride;
		  rstride[dim] = ret->dim[n].stride;
		  dim++;
		}
	    }
	}
      else
	{
	  for (n = 0; n < rrank; n++)
	    {
	      if (n == along - 1)
		{
		  rdelta = ret->dim[n].stride;
		}
	      else
		{
		  count[dim] = 0;
		  extent[dim] = source->dim[dim].ubound + 1
		    - source->dim[dim].lbound;
		  if (extent[dim] <= 0)
		    zero_sized = 1;
		  sstride[dim] = source->dim[dim].stride;
		  rstride[dim] = ret->dim[n].stride;
		  dim++;
		}
	    }
	}

      if (zero_sized)
	return;

      if (sstride[0] == 0)
	sstride[0] = 1;
    }
  sstride0 = sstride[0];
  rstride0 = rstride[0];
  rptr = ret->data;
  sptr = source->data;

  while (sptr)
    {
      /* Spread this element.  */
      dest = rptr;
      for (n = 0; n < ncopies; n++)
        {
	  *dest = *sptr;
          dest += rdelta;
        }
      /* Advance to the next element.  */
      sptr += sstride0;
      rptr += rstride0;
      count[0]++;
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          sptr -= sstride[n] * extent[n];
          rptr -= rstride[n] * extent[n];
          n++;
          if (n >= srank)
            {
              /* Break out of the loop.  */
              sptr = NULL;
              break;
            }
          else
            {
              count[n]++;
              sptr += sstride[n];
              rptr += rstride[n];
            }
        }
    }
}

/* This version of spread_internal treats the special case of a scalar
   source.  This is much simpler than the more general case above.  */

void
spread_scalar_r16 (gfc_array_r16 *ret, const GFC_REAL_16 *source,
			const index_type along, const index_type pncopies)
{
  int n;
  int ncopies = pncopies;
  GFC_REAL_16 * restrict dest;
  index_type stride;

  if (GFC_DESCRIPTOR_RANK (ret) != 1)
    runtime_error ("incorrect destination rank in spread()");

  if (along > 1)
    runtime_error ("dim outside of rank in spread()");

  if (ret->data == NULL)
    {
      ret->data = internal_malloc_size (ncopies * sizeof (GFC_REAL_16));
      ret->offset = 0;
      ret->dim[0].stride = 1;
      ret->dim[0].lbound = 0;
      ret->dim[0].ubound = ncopies - 1;
    }
  else
    {
      if (ncopies - 1 > (ret->dim[0].ubound - ret->dim[0].lbound)
			   / ret->dim[0].stride)
	runtime_error ("dim too large in spread()");
    }

  dest = ret->data;
  stride = ret->dim[0].stride;

  for (n = 0; n < ncopies; n++)
    {
      *dest = *source;
      dest += stride;
    }
}

#endif

