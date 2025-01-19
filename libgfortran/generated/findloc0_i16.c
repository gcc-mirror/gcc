
/* Implementation of the FINDLOC intrinsic
   Copyright (C) 2018-2025 Free Software Foundation, Inc.
   Contributed by Thomas König <tk@tkoenig.net>

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

#if defined (HAVE_GFC_INTEGER_16)
extern void findloc0_i16 (gfc_array_index_type * const restrict retarray,
       	    		gfc_array_i16 * const restrict array, GFC_INTEGER_16 value,
			 GFC_LOGICAL_4);
export_proto(findloc0_i16);

void
findloc0_i16 (gfc_array_index_type * const restrict retarray,
    	    gfc_array_i16 * const restrict array, GFC_INTEGER_16 value,
	    GFC_LOGICAL_4 back)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type dstride;
  const GFC_INTEGER_16 *base;
  index_type * restrict dest;
  index_type rank;
  index_type n;
  index_type sz;

  rank = GFC_DESCRIPTOR_RANK (array);
  if (rank <= 0)
    runtime_error ("Rank of array needs to be > 0");

  if (retarray->base_addr == NULL)
    {
      GFC_DIMENSION_SET(retarray->dim[0], 0, rank-1, 1);
      retarray->dtype.rank = 1;
      retarray->offset = 0;
      retarray->base_addr = xmallocarray (rank, sizeof (index_type));
    }
  else
    {
      if (unlikely (compile_options.bounds_check))
	bounds_iforeach_return ((array_t *) retarray, (array_t *) array,
				"FINDLOC");
    }

  dstride = GFC_DESCRIPTOR_STRIDE(retarray,0);
  dest = retarray->base_addr;

  /* Set the return value.  */
  for (n = 0; n < rank; n++)
    dest[n * dstride] = 0;

  sz = 1;
  for (n = 0; n < rank; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE(array,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);
      sz *= extent[n];
      if (extent[n] <= 0)
	return;
    }

    for (n = 0; n < rank; n++)
      count[n] = 0;

  if (back)
    {
      base = array->base_addr + (sz - 1) * 1;

      while (1)
        {
	  do
	    {
	      if (unlikely(*base == value))
	        {
		  for (n = 0; n < rank; n++)
		    dest[n * dstride] = extent[n] - count[n];

		  return;
		}
	      base -= sstride[0] * 1;
	    } while(++count[0] != extent[0]);

	  n = 0;
	  do
	    {
	      /* When we get to the end of a dimension, reset it and increment
		 the next dimension.  */
	      count[n] = 0;
	      /* We could precalculate these products, but this is a less
		 frequently used path so probably not worth it.  */
	      base += sstride[n] * extent[n] * 1;
	      n++;
	      if (n >= rank)
	        return;
	      else
		{
		  count[n]++;
		  base -= sstride[n] * 1;
		}
	    } while (count[n] == extent[n]);      
	}
    }
  else
    {
      base = array->base_addr;
      while (1)
        {
	  do
	    {
	      if (unlikely(*base == value))
	        {
		  for (n = 0; n < rank; n++)
		    dest[n * dstride] = count[n] + 1;

		  return;
		}
	      base += sstride[0] * 1;
	    } while(++count[0] != extent[0]);

	  n = 0;
	  do
	    {
	      /* When we get to the end of a dimension, reset it and increment
		 the next dimension.  */
	      count[n] = 0;
	      /* We could precalculate these products, but this is a less
		 frequently used path so probably not worth it.  */
	      base -= sstride[n] * extent[n] * 1;
	      n++;
	      if (n >= rank)
	        return;
	      else
		{
		  count[n]++;
		  base += sstride[n] * 1;
		}
	    } while (count[n] == extent[n]);
	}
    }
  return;
}

extern void mfindloc0_i16 (gfc_array_index_type * const restrict retarray,
       	    		gfc_array_i16 * const restrict array, GFC_INTEGER_16 value,
			 gfc_array_l1 *const restrict, GFC_LOGICAL_4);
export_proto(mfindloc0_i16);

void
mfindloc0_i16 (gfc_array_index_type * const restrict retarray,
    	    gfc_array_i16 * const restrict array, GFC_INTEGER_16 value,
	    gfc_array_l1 *const restrict mask, GFC_LOGICAL_4 back)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type mstride[GFC_MAX_DIMENSIONS];
  index_type dstride;
  const GFC_INTEGER_16 *base;
  index_type * restrict dest;
  GFC_LOGICAL_1 *mbase;
  index_type rank;
  index_type n;
  int mask_kind;
  index_type sz;

  rank = GFC_DESCRIPTOR_RANK (array);
  if (rank <= 0)
    runtime_error ("Rank of array needs to be > 0");

  if (retarray->base_addr == NULL)
    {
      GFC_DIMENSION_SET(retarray->dim[0], 0, rank-1, 1);
      retarray->dtype.rank = 1;
      retarray->offset = 0;
      retarray->base_addr = xmallocarray (rank, sizeof (index_type));
    }
  else
    {
      if (unlikely (compile_options.bounds_check))
	{
	  bounds_iforeach_return ((array_t *) retarray, (array_t *) array,
				  "FINDLOC");
	  bounds_equal_extents ((array_t *) mask, (array_t *) array,
				"MASK argument", "FINDLOC");
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
    internal_error (NULL, "Funny sized logical array");

  dstride = GFC_DESCRIPTOR_STRIDE(retarray,0);
  dest = retarray->base_addr;

  /* Set the return value.  */
  for (n = 0; n < rank; n++)
    dest[n * dstride] = 0;

  sz = 1;
  for (n = 0; n < rank; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE(array,n);
      mstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(mask,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);
      sz *= extent[n];
      if (extent[n] <= 0)
	return;
    }

    for (n = 0; n < rank; n++)
      count[n] = 0;

  if (back)
    {
      base = array->base_addr + (sz - 1) * 1;
      mbase = mbase + (sz - 1) * mask_kind;
      while (1)
        {
	  do
	    {
	      if (unlikely(*mbase && *base == value))
	        {
		  for (n = 0; n < rank; n++)
		    dest[n * dstride] = extent[n] - count[n];

		  return;
		}
	      base -= sstride[0] * 1;
	      mbase -= mstride[0];
	    } while(++count[0] != extent[0]);

	  n = 0;
	  do
	    {
	      /* When we get to the end of a dimension, reset it and increment
		 the next dimension.  */
	      count[n] = 0;
	      /* We could precalculate these products, but this is a less
		 frequently used path so probably not worth it.  */
	      base += sstride[n] * extent[n] * 1;
	      mbase -= mstride[n] * extent[n];
	      n++;
	      if (n >= rank)
		return;
	      else
		{
		  count[n]++;
		  base -= sstride[n] * 1;
		  mbase += mstride[n];
		}
	    } while (count[n] == extent[n]);      
	}
    }
  else
    {
      base = array->base_addr;
      while (1)
        {
	  do
	    {
	      if (unlikely(*mbase && *base == value))
	        {
		  for (n = 0; n < rank; n++)
		    dest[n * dstride] = count[n] + 1;

		  return;
		}
	      base += sstride[0] * 1;
	      mbase += mstride[0];
	    } while(++count[0] != extent[0]);

	  n = 0;
	  do
	    {
	      /* When we get to the end of a dimension, reset it and increment
		 the next dimension.  */
	      count[n] = 0;
	      /* We could precalculate these products, but this is a less
		 frequently used path so probably not worth it.  */
	      base -= sstride[n] * extent[n] * 1;
	      mbase -= mstride[n] * extent[n];
	      n++;
	      if (n >= rank)
		return;
	      else
		{
		  count[n]++;
		  base += sstride[n]* 1;
		  mbase += mstride[n];
		}
	    } while (count[n] == extent[n]);
	}
    }
  return;
}

extern void sfindloc0_i16 (gfc_array_index_type * const restrict retarray,
       	    		gfc_array_i16 * const restrict array, GFC_INTEGER_16 value,
			 GFC_LOGICAL_4 *, GFC_LOGICAL_4);
export_proto(sfindloc0_i16);

void
sfindloc0_i16 (gfc_array_index_type * const restrict retarray,
    	    gfc_array_i16 * const restrict array, GFC_INTEGER_16 value,
	    GFC_LOGICAL_4 * mask, GFC_LOGICAL_4 back)
{
  index_type rank;
  index_type dstride;
  index_type * restrict dest;
  index_type n;

  if (mask == NULL || *mask)
    {
      findloc0_i16 (retarray, array, value, back);
      return;
    }

  rank = GFC_DESCRIPTOR_RANK (array);

  if (rank <= 0)
    internal_error (NULL, "Rank of array needs to be > 0");

  if (retarray->base_addr == NULL)
    {
      GFC_DIMENSION_SET(retarray->dim[0], 0, rank-1, 1);
      retarray->dtype.rank = 1;
      retarray->offset = 0;
      retarray->base_addr = xmallocarray (rank, sizeof (index_type));
    }
  else if (unlikely (compile_options.bounds_check))
    {
       bounds_iforeach_return ((array_t *) retarray, (array_t *) array,
			       "FINDLOC");
    }

  dstride = GFC_DESCRIPTOR_STRIDE(retarray,0);
  dest = retarray->base_addr;
  for (n = 0; n<rank; n++)
    dest[n * dstride] = 0 ;
}

#endif
