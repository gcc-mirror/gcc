/* Implementation of the MINLOC intrinsic
   Copyright (C) 2002-2018 Free Software Foundation, Inc.
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
#include <assert.h>


#if defined (HAVE_GFC_REAL_8) && defined (HAVE_GFC_INTEGER_4)


extern void minloc0_4_r8 (gfc_array_i4 * const restrict retarray, 
	gfc_array_r8 * const restrict array, GFC_LOGICAL_4);
export_proto(minloc0_4_r8);

void
minloc0_4_r8 (gfc_array_i4 * const restrict retarray, 
	gfc_array_r8 * const restrict array, GFC_LOGICAL_4 back)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type dstride;
  const GFC_REAL_8 *base;
  GFC_INTEGER_4 * restrict dest;
  index_type rank;
  index_type n;

  rank = GFC_DESCRIPTOR_RANK (array);
  if (rank <= 0)
    runtime_error ("Rank of array needs to be > 0");

  if (retarray->base_addr == NULL)
    {
      GFC_DIMENSION_SET(retarray->dim[0], 0, rank-1, 1);
      retarray->dtype.rank = 1;
      retarray->offset = 0;
      retarray->base_addr = xmallocarray (rank, sizeof (GFC_INTEGER_4));
    }
  else
    {
      if (unlikely (compile_options.bounds_check))
	bounds_iforeach_return ((array_t *) retarray, (array_t *) array,
				"MINLOC");
    }

  dstride = GFC_DESCRIPTOR_STRIDE(retarray,0);
  dest = retarray->base_addr;
  for (n = 0; n < rank; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE(array,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);
      count[n] = 0;
      if (extent[n] <= 0)
	{
	  /* Set the return value.  */
	  for (n = 0; n < rank; n++)
	    dest[n * dstride] = 0;
	  return;
	}
    }

  base = array->base_addr;

  /* Initialize the return value.  */
  for (n = 0; n < rank; n++)
    dest[n * dstride] = 1;
  {

    GFC_REAL_8 minval;
#if defined(GFC_REAL_8_QUIET_NAN)
    int fast = 0;
#endif

#if defined(GFC_REAL_8_INFINITY)
    minval = GFC_REAL_8_INFINITY;
#else
    minval = GFC_REAL_8_HUGE;
#endif
  while (base)
    {
	  /* Implementation start.  */

#if defined(GFC_REAL_8_QUIET_NAN)
      if (unlikely (!fast))
	{
	  do
	    {
	      if (*base <= minval)
		{
		  fast = 1;
		  minval = *base;
		  for (n = 0; n < rank; n++)
		    dest[n * dstride] = count[n] + 1;
		  break;
		}
	      base += sstride[0];
	    }
	  while (++count[0] != extent[0]);
	  if (likely (fast))
	    continue;
	}
      else
#endif
    if (back)
      do
	{
	  if (unlikely (*base <= minval))
	    {
	      minval = *base;
	      for (n = 0; n < rank; n++)
		dest[n * dstride] = count[n] + 1;
	    }
	  base += sstride[0];
	}
      while (++count[0] != extent[0]);
    else
      do
        {
	  if (unlikely (*base < minval))
	    {
	      minval = *base;
	      for (n = 0; n < rank; n++)
	        dest[n * dstride] = count[n] + 1;
	    }
	  /* Implementation end.  */
	  /* Advance to the next element.  */
	  base += sstride[0];
	}
      while (++count[0] != extent[0]);
      n = 0;
      do
	{
	  /* When we get to the end of a dimension, reset it and increment
	     the next dimension.  */
	  count[n] = 0;
	  /* We could precalculate these products, but this is a less
	     frequently used path so probably not worth it.  */
	  base -= sstride[n] * extent[n];
	  n++;
	  if (n >= rank)
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
      while (count[n] == extent[n]);
    }
  }
}

extern void mminloc0_4_r8 (gfc_array_i4 * const restrict, 
	gfc_array_r8 * const restrict, gfc_array_l1 * const restrict,
	GFC_LOGICAL_4);
export_proto(mminloc0_4_r8);

void
mminloc0_4_r8 (gfc_array_i4 * const restrict retarray, 
	gfc_array_r8 * const restrict array,
	gfc_array_l1 * const restrict mask, GFC_LOGICAL_4 back)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type mstride[GFC_MAX_DIMENSIONS];
  index_type dstride;
  GFC_INTEGER_4 *dest;
  const GFC_REAL_8 *base;
  GFC_LOGICAL_1 *mbase;
  int rank;
  index_type n;
  int mask_kind;

  rank = GFC_DESCRIPTOR_RANK (array);
  if (rank <= 0)
    runtime_error ("Rank of array needs to be > 0");

  if (retarray->base_addr == NULL)
    {
      GFC_DIMENSION_SET(retarray->dim[0], 0, rank - 1, 1);
      retarray->dtype.rank = 1;
      retarray->offset = 0;
      retarray->base_addr = xmallocarray (rank, sizeof (GFC_INTEGER_4));
    }
  else
    {
      if (unlikely (compile_options.bounds_check))
	{

	  bounds_iforeach_return ((array_t *) retarray, (array_t *) array,
				  "MINLOC");
	  bounds_equal_extents ((array_t *) mask, (array_t *) array,
				  "MASK argument", "MINLOC");
	}
    }

  mask_kind = GFC_DESCRIPTOR_SIZE (mask);

  mbase = mask->base_addr;

  if (mask_kind == 1 || mask_kind == 2 || mask_kind == 4 || mask_kind == 8
#ifdef HAVE_GFC_LOGICAL_16
      || mask_kind == 16
#endif
      )
    mbase = GFOR_POINTER_TO_L1 (mbase, mask_kind);
  else
    runtime_error ("Funny sized logical array");

  dstride = GFC_DESCRIPTOR_STRIDE(retarray,0);
  dest = retarray->base_addr;
  for (n = 0; n < rank; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE(array,n);
      mstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(mask,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);
      count[n] = 0;
      if (extent[n] <= 0)
	{
	  /* Set the return value.  */
	  for (n = 0; n < rank; n++)
	    dest[n * dstride] = 0;
	  return;
	}
    }

  base = array->base_addr;

  /* Initialize the return value.  */
  for (n = 0; n < rank; n++)
    dest[n * dstride] = 0;
  {

  GFC_REAL_8 minval;
   int fast = 0;

#if defined(GFC_REAL_8_INFINITY)
    minval = GFC_REAL_8_INFINITY;
#else
    minval = GFC_REAL_8_HUGE;
#endif
  while (base)
    {
	  /* Implementation start.  */

      if (unlikely (!fast))
	{
	  do
	    {
	      if (*mbase)
		{
#if defined(GFC_REAL_8_QUIET_NAN)
		  if (unlikely (dest[0] == 0))
		    for (n = 0; n < rank; n++)
		      dest[n * dstride] = count[n] + 1;
		  if (*base <= minval)
#endif
		    {
		      fast = 1;
		      minval = *base;
		      for (n = 0; n < rank; n++)
			dest[n * dstride] = count[n] + 1;
		      break;
		    }
		}
	      base += sstride[0];
	      mbase += mstride[0];
	    }
	  while (++count[0] != extent[0]);
	  if (likely (fast))
	    continue;
	}
        else
        if (back)
	  do
	    {
	      if (unlikely (*mbase && (*base <= minval)))
	        {
	      	  minval = *base;
	      	  for (n = 0; n < rank; n++)
		    dest[n * dstride] = count[n] + 1;
	    	}
		base += sstride[0];
	    }
	    while (++count[0] != extent[0]);
	else
	  do
	    {
	      if (unlikely (*mbase && (*base < minval)))
		{
		  minval = *base;
		  for (n = 0; n < rank; n++)
		    dest[n * dstride] = count[n] + 1;
		}
	  /* Implementation end.  */
	  /* Advance to the next element.  */
	  base += sstride[0];
	  mbase += mstride[0];
	}
      while (++count[0] != extent[0]);
      n = 0;
      do
	{
	  /* When we get to the end of a dimension, reset it and increment
	     the next dimension.  */
	  count[n] = 0;
	  /* We could precalculate these products, but this is a less
	     frequently used path so probably not worth it.  */
	  base -= sstride[n] * extent[n];
	  mbase -= mstride[n] * extent[n];
	  n++;
	  if (n >= rank)
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
      while (count[n] == extent[n]);
    }
  }
}

extern void sminloc0_4_r8 (gfc_array_i4 * const restrict, 
	gfc_array_r8 * const restrict, GFC_LOGICAL_4 *, GFC_LOGICAL_4);
export_proto(sminloc0_4_r8);

void
sminloc0_4_r8 (gfc_array_i4 * const restrict retarray, 
	gfc_array_r8 * const restrict array,
	GFC_LOGICAL_4 * mask, GFC_LOGICAL_4 back)
{
  index_type rank;
  index_type dstride;
  index_type n;
  GFC_INTEGER_4 *dest;

  if (*mask)
    {
      minloc0_4_r8 (retarray, array, back);
      return;
    }

  rank = GFC_DESCRIPTOR_RANK (array);

  if (rank <= 0)
    runtime_error ("Rank of array needs to be > 0");

  if (retarray->base_addr == NULL)
    {
      GFC_DIMENSION_SET(retarray->dim[0], 0, rank-1, 1);
      retarray->dtype.rank = 1;
      retarray->offset = 0;
      retarray->base_addr = xmallocarray (rank, sizeof (GFC_INTEGER_4));
    }
  else if (unlikely (compile_options.bounds_check))
    {
       bounds_iforeach_return ((array_t *) retarray, (array_t *) array,
			       "MINLOC");
    }

  dstride = GFC_DESCRIPTOR_STRIDE(retarray,0);
  dest = retarray->base_addr;
  for (n = 0; n<rank; n++)
    dest[n * dstride] = 0 ;
}
#endif
