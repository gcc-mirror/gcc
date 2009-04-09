/* Implementation of the MINLOC intrinsic
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
#include <limits.h>


#if defined (HAVE_GFC_INTEGER_16) && defined (HAVE_GFC_INTEGER_8)


extern void minloc0_8_i16 (gfc_array_i8 * const restrict retarray, 
	gfc_array_i16 * const restrict array);
export_proto(minloc0_8_i16);

void
minloc0_8_i16 (gfc_array_i8 * const restrict retarray, 
	gfc_array_i16 * const restrict array)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type dstride;
  const GFC_INTEGER_16 *base;
  GFC_INTEGER_8 * restrict dest;
  index_type rank;
  index_type n;

  rank = GFC_DESCRIPTOR_RANK (array);
  if (rank <= 0)
    runtime_error ("Rank of array needs to be > 0");

  if (retarray->data == NULL)
    {
      retarray->dim[0].lbound = 0;
      retarray->dim[0].ubound = rank-1;
      retarray->dim[0].stride = 1;
      retarray->dtype = (retarray->dtype & ~GFC_DTYPE_RANK_MASK) | 1;
      retarray->offset = 0;
      retarray->data = internal_malloc_size (sizeof (GFC_INTEGER_8) * rank);
    }
  else
    {
      if (unlikely (compile_options.bounds_check))
	{
	  int ret_rank;
	  index_type ret_extent;

	  ret_rank = GFC_DESCRIPTOR_RANK (retarray);
	  if (ret_rank != 1)
	    runtime_error ("rank of return array in MINLOC intrinsic"
			   " should be 1, is %ld", (long int) ret_rank);

	  ret_extent = retarray->dim[0].ubound + 1 - retarray->dim[0].lbound;
	  if (ret_extent != rank)
	    runtime_error ("Incorrect extent in return value of"
			   " MINLOC intrnisic: is %ld, should be %ld",
			   (long int) ret_extent, (long int) rank);
	}
    }

  dstride = retarray->dim[0].stride;
  dest = retarray->data;
  for (n = 0; n < rank; n++)
    {
      sstride[n] = array->dim[n].stride;
      extent[n] = array->dim[n].ubound + 1 - array->dim[n].lbound;
      count[n] = 0;
      if (extent[n] <= 0)
	{
	  /* Set the return value.  */
	  for (n = 0; n < rank; n++)
	    dest[n * dstride] = 0;
	  return;
	}
    }

  base = array->data;

  /* Initialize the return value.  */
  for (n = 0; n < rank; n++)
    dest[n * dstride] = 0;
  {

  GFC_INTEGER_16 minval;

  minval = GFC_INTEGER_16_HUGE;

  while (base)
    {
      {
        /* Implementation start.  */

  if (*base < minval || !dest[0])
    {
      minval = *base;
      for (n = 0; n < rank; n++)
        dest[n * dstride] = count[n] + 1;
    }
        /* Implementation end.  */
      }
      /* Advance to the next element.  */
      count[0]++;
      base += sstride[0];
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          base -= sstride[n] * extent[n];
          n++;
          if (n == rank)
            {
              /* Break out of the loop.  */
              base = NULL;
              break;
            }
          else
            {
              count[n]++;
              base += sstride[n];
            }
        }
    }
  }
}


extern void mminloc0_8_i16 (gfc_array_i8 * const restrict, 
	gfc_array_i16 * const restrict, gfc_array_l1 * const restrict);
export_proto(mminloc0_8_i16);

void
mminloc0_8_i16 (gfc_array_i8 * const restrict retarray, 
	gfc_array_i16 * const restrict array,
	gfc_array_l1 * const restrict mask)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type mstride[GFC_MAX_DIMENSIONS];
  index_type dstride;
  GFC_INTEGER_8 *dest;
  const GFC_INTEGER_16 *base;
  GFC_LOGICAL_1 *mbase;
  int rank;
  index_type n;
  int mask_kind;

  rank = GFC_DESCRIPTOR_RANK (array);
  if (rank <= 0)
    runtime_error ("Rank of array needs to be > 0");

  if (retarray->data == NULL)
    {
      retarray->dim[0].lbound = 0;
      retarray->dim[0].ubound = rank-1;
      retarray->dim[0].stride = 1;
      retarray->dtype = (retarray->dtype & ~GFC_DTYPE_RANK_MASK) | 1;
      retarray->offset = 0;
      retarray->data = internal_malloc_size (sizeof (GFC_INTEGER_8) * rank);
    }
  else
    {
      if (unlikely (compile_options.bounds_check))
	{
	  int ret_rank, mask_rank;
	  index_type ret_extent;
	  int n;
	  index_type array_extent, mask_extent;

	  ret_rank = GFC_DESCRIPTOR_RANK (retarray);
	  if (ret_rank != 1)
	    runtime_error ("rank of return array in MINLOC intrinsic"
			   " should be 1, is %ld", (long int) ret_rank);

	  ret_extent = retarray->dim[0].ubound + 1 - retarray->dim[0].lbound;
	  if (ret_extent != rank)
	    runtime_error ("Incorrect extent in return value of"
			   " MINLOC intrnisic: is %ld, should be %ld",
			   (long int) ret_extent, (long int) rank);
	
	  mask_rank = GFC_DESCRIPTOR_RANK (mask);
	  if (rank != mask_rank)
	    runtime_error ("rank of MASK argument in MINLOC intrnisic"
	                   "should be %ld, is %ld", (long int) rank,
			   (long int) mask_rank);

	  for (n=0; n<rank; n++)
	    {
	      array_extent = array->dim[n].ubound + 1 - array->dim[n].lbound;
	      mask_extent = mask->dim[n].ubound + 1 - mask->dim[n].lbound;
	      if (array_extent != mask_extent)
		runtime_error ("Incorrect extent in MASK argument of"
			       " MINLOC intrinsic in dimension %ld:"
			       " is %ld, should be %ld", (long int) n + 1,
			       (long int) mask_extent, (long int) array_extent);
	    }
	}
    }

  mask_kind = GFC_DESCRIPTOR_SIZE (mask);

  mbase = mask->data;

  if (mask_kind == 1 || mask_kind == 2 || mask_kind == 4 || mask_kind == 8
#ifdef HAVE_GFC_LOGICAL_16
      || mask_kind == 16
#endif
      )
    mbase = GFOR_POINTER_TO_L1 (mbase, mask_kind);
  else
    runtime_error ("Funny sized logical array");

  dstride = retarray->dim[0].stride;
  dest = retarray->data;
  for (n = 0; n < rank; n++)
    {
      sstride[n] = array->dim[n].stride;
      mstride[n] = mask->dim[n].stride * mask_kind;
      extent[n] = array->dim[n].ubound + 1 - array->dim[n].lbound;
      count[n] = 0;
      if (extent[n] <= 0)
	{
	  /* Set the return value.  */
	  for (n = 0; n < rank; n++)
	    dest[n * dstride] = 0;
	  return;
	}
    }

  base = array->data;

  /* Initialize the return value.  */
  for (n = 0; n < rank; n++)
    dest[n * dstride] = 0;
  {

  GFC_INTEGER_16 minval;

  minval = GFC_INTEGER_16_HUGE;

  while (base)
    {
      {
        /* Implementation start.  */

  if (*mbase && (*base < minval || !dest[0]))
    {
      minval = *base;
      for (n = 0; n < rank; n++)
        dest[n * dstride] = count[n] + 1;
    }
        /* Implementation end.  */
      }
      /* Advance to the next element.  */
      count[0]++;
      base += sstride[0];
      mbase += mstride[0];
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          base -= sstride[n] * extent[n];
          mbase -= mstride[n] * extent[n];
          n++;
          if (n == rank)
            {
              /* Break out of the loop.  */
              base = NULL;
              break;
            }
          else
            {
              count[n]++;
              base += sstride[n];
              mbase += mstride[n];
            }
        }
    }
  }
}


extern void sminloc0_8_i16 (gfc_array_i8 * const restrict, 
	gfc_array_i16 * const restrict, GFC_LOGICAL_4 *);
export_proto(sminloc0_8_i16);

void
sminloc0_8_i16 (gfc_array_i8 * const restrict retarray, 
	gfc_array_i16 * const restrict array,
	GFC_LOGICAL_4 * mask)
{
  index_type rank;
  index_type dstride;
  index_type n;
  GFC_INTEGER_8 *dest;

  if (*mask)
    {
      minloc0_8_i16 (retarray, array);
      return;
    }

  rank = GFC_DESCRIPTOR_RANK (array);

  if (rank <= 0)
    runtime_error ("Rank of array needs to be > 0");

  if (retarray->data == NULL)
    {
      retarray->dim[0].lbound = 0;
      retarray->dim[0].ubound = rank-1;
      retarray->dim[0].stride = 1;
      retarray->dtype = (retarray->dtype & ~GFC_DTYPE_RANK_MASK) | 1;
      retarray->offset = 0;
      retarray->data = internal_malloc_size (sizeof (GFC_INTEGER_8) * rank);
    }
  else
    {
      if (unlikely (compile_options.bounds_check))
	{
	  int ret_rank;
	  index_type ret_extent;

	  ret_rank = GFC_DESCRIPTOR_RANK (retarray);
	  if (ret_rank != 1)
	    runtime_error ("rank of return array in MINLOC intrinsic"
			   " should be 1, is %ld", (long int) ret_rank);

	  ret_extent = retarray->dim[0].ubound + 1 - retarray->dim[0].lbound;
	    if (ret_extent != rank)
	      runtime_error ("dimension of return array incorrect");
	}
    }

  dstride = retarray->dim[0].stride;
  dest = retarray->data;
  for (n = 0; n<rank; n++)
    dest[n * dstride] = 0 ;
}
#endif
