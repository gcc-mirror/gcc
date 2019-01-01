/* Implementation of the FINDLOC intrinsic
   Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

#if defined (HAVE_GFC_COMPLEX_4)
extern void findloc1_c4 (gfc_array_index_type * const restrict retarray,
		         gfc_array_c4 * const restrict array, GFC_COMPLEX_4 value,
			 const index_type * restrict pdim, GFC_LOGICAL_4 back);
export_proto(findloc1_c4);

extern void
findloc1_c4 (gfc_array_index_type * const restrict retarray,
	    gfc_array_c4 * const restrict array, GFC_COMPLEX_4 value,
	    const index_type * restrict pdim, GFC_LOGICAL_4 back)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type dstride[GFC_MAX_DIMENSIONS];
  const GFC_COMPLEX_4 * restrict base;
  index_type * restrict dest;
  index_type rank;
  index_type n;
  index_type len;
  index_type delta;
  index_type dim;
  int continue_loop;

  /* Make dim zero based to avoid confusion.  */
  rank = GFC_DESCRIPTOR_RANK (array) - 1;
  dim = (*pdim) - 1;

  if (unlikely (dim < 0 || dim > rank))
    {
      runtime_error ("Dim argument incorrect in FINDLOC intrinsic: "
 		     "is %ld, should be between 1 and %ld",
		     (long int) dim + 1, (long int) rank + 1);
    }

  len = GFC_DESCRIPTOR_EXTENT(array,dim);
  if (len < 0)
    len = 0;
  delta = GFC_DESCRIPTOR_STRIDE(array,dim);

  for (n = 0; n < dim; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE(array,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);

      if (extent[n] < 0)
	extent[n] = 0;
    }
  for (n = dim; n < rank; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE(array, n + 1);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array, n + 1);

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

      retarray->base_addr = xmallocarray (alloc_size, sizeof (index_type));
      if (alloc_size == 0)
	{
	  /* Make sure we have a zero-sized array.  */
	  GFC_DIMENSION_SET(retarray->dim[0], 0, -1, 1);
	  return;
	}
    }
  else
    {
      if (rank != GFC_DESCRIPTOR_RANK (retarray))
	runtime_error ("rank of return array incorrect in"
		       " FINDLOC intrinsic: is %ld, should be %ld",
		       (long int) (GFC_DESCRIPTOR_RANK (retarray)),
		       (long int) rank);

      if (unlikely (compile_options.bounds_check))
	bounds_ifunction_return ((array_t *) retarray, extent,
				 "return value", "FINDLOC");
    }

  for (n = 0; n < rank; n++)
    {
      count[n] = 0;
      dstride[n] = GFC_DESCRIPTOR_STRIDE(retarray,n);
      if (extent[n] <= 0)
	return;
    }

  dest = retarray->base_addr;
  continue_loop = 1;

  base = array->base_addr;
  while (continue_loop)
    {
      const GFC_COMPLEX_4 * restrict src;
      index_type result;

      result = 0;
      if (back)
	{
	  src = base + (len - 1) * delta * 1;
	  for (n = len; n > 0; n--, src -= delta * 1)
	    {
	      if (*src == value)
		{
		  result = n;
		  break;
		}
	    }
	}
      else
	{
	  src = base;
	  for (n = 1; n <= len; n++, src += delta * 1)
	    {
	      if (*src == value)
		{
		  result = n;
		  break;
		}
	    }
	}
      *dest = result;

      count[0]++;
      base += sstride[0] * 1;
      dest += dstride[0];
      n = 0;
      while (count[n] == extent[n])
	{
	  count[n] = 0;
	  base -= sstride[n] * extent[n] * 1;
	  dest -= dstride[n] * extent[n];
	  n++;
	  if (n >= rank)
	    {
	      continue_loop = 0;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      base += sstride[n] * 1;
	      dest += dstride[n];
	    }
	}
    }
}
extern void mfindloc1_c4 (gfc_array_index_type * const restrict retarray,
		         gfc_array_c4 * const restrict array, GFC_COMPLEX_4 value,
			 const index_type * restrict pdim, gfc_array_l1 *const restrict mask,
			 GFC_LOGICAL_4 back);
export_proto(mfindloc1_c4);

extern void
mfindloc1_c4 (gfc_array_index_type * const restrict retarray,
	    gfc_array_c4 * const restrict array, GFC_COMPLEX_4 value,
	    const index_type * restrict pdim, gfc_array_l1 *const restrict mask,
	    GFC_LOGICAL_4 back)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type mstride[GFC_MAX_DIMENSIONS];
  index_type dstride[GFC_MAX_DIMENSIONS];
  const GFC_COMPLEX_4 * restrict base;
  const GFC_LOGICAL_1 * restrict mbase;
  index_type * restrict dest;
  index_type rank;
  index_type n;
  index_type len;
  index_type delta;
  index_type mdelta;
  index_type dim;
  int mask_kind;
  int continue_loop;

  /* Make dim zero based to avoid confusion.  */
  rank = GFC_DESCRIPTOR_RANK (array) - 1;
  dim = (*pdim) - 1;

  if (unlikely (dim < 0 || dim > rank))
    {
      runtime_error ("Dim argument incorrect in FINDLOC intrinsic: "
 		     "is %ld, should be between 1 and %ld",
		     (long int) dim + 1, (long int) rank + 1);
    }

  len = GFC_DESCRIPTOR_EXTENT(array,dim);
  if (len < 0)
    len = 0;

  delta = GFC_DESCRIPTOR_STRIDE(array,dim);
  mdelta = GFC_DESCRIPTOR_STRIDE_BYTES(mask,dim);

  mbase = mask->base_addr;

  mask_kind = GFC_DESCRIPTOR_SIZE (mask);

  if (mask_kind == 1 || mask_kind == 2 || mask_kind == 4 || mask_kind == 8
#ifdef HAVE_GFC_LOGICAL_16
      || mask_kind == 16
#endif
      )
    mbase = GFOR_POINTER_TO_L1 (mbase, mask_kind);
  else
    internal_error (NULL, "Funny sized logical array");

  for (n = 0; n < dim; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE(array,n);
      mstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(mask,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);

      if (extent[n] < 0)
	extent[n] = 0;
    }
  for (n = dim; n < rank; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE(array, n + 1);
      mstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(mask, n + 1);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array, n + 1);

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

      retarray->base_addr = xmallocarray (alloc_size, sizeof (index_type));
      if (alloc_size == 0)
	{
	  /* Make sure we have a zero-sized array.  */
	  GFC_DIMENSION_SET(retarray->dim[0], 0, -1, 1);
	  return;
	}
    }
  else
    {
      if (rank != GFC_DESCRIPTOR_RANK (retarray))
	runtime_error ("rank of return array incorrect in"
		       " FINDLOC intrinsic: is %ld, should be %ld",
		       (long int) (GFC_DESCRIPTOR_RANK (retarray)),
		       (long int) rank);

      if (unlikely (compile_options.bounds_check))
	bounds_ifunction_return ((array_t *) retarray, extent,
				 "return value", "FINDLOC");
    }

  for (n = 0; n < rank; n++)
    {
      count[n] = 0;
      dstride[n] = GFC_DESCRIPTOR_STRIDE(retarray,n);
      if (extent[n] <= 0)
	return;
    }

  dest = retarray->base_addr;
  continue_loop = 1;

  base = array->base_addr;
  while (continue_loop)
    {
      const GFC_COMPLEX_4 * restrict src;
      const GFC_LOGICAL_1 * restrict msrc;
      index_type result;

      result = 0;
      if (back)
	{
	  src = base + (len - 1) * delta * 1;
	  msrc = mbase + (len - 1) * mdelta; 
	  for (n = len; n > 0; n--, src -= delta * 1, msrc -= mdelta)
	    {
	      if (*msrc && *src == value)
		{
		  result = n;
		  break;
		}
	    }
	}
      else
	{
	  src = base;
	  msrc = mbase;
	  for (n = 1; n <= len; n++, src += delta * 1, msrc += mdelta)
	    {
	      if (*msrc && *src == value)
		{
		  result = n;
		  break;
		}
	    }
	}
      *dest = result;

      count[0]++;
      base += sstride[0] * 1;
      mbase += mstride[0];
      dest += dstride[0];
      n = 0;
      while (count[n] == extent[n])
	{
	  count[n] = 0;
	  base -= sstride[n] * extent[n] * 1;
	  mbase -= mstride[n] * extent[n];
	  dest -= dstride[n] * extent[n];
	  n++;
	  if (n >= rank)
	    {
	      continue_loop = 0;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      base += sstride[n] * 1;
	      dest += dstride[n];
	    }
	}
    }
}
extern void sfindloc1_c4 (gfc_array_index_type * const restrict retarray,
		         gfc_array_c4 * const restrict array, GFC_COMPLEX_4 value,
			 const index_type * restrict pdim, GFC_LOGICAL_4 *const restrict mask,
			 GFC_LOGICAL_4 back);
export_proto(sfindloc1_c4);

extern void
sfindloc1_c4 (gfc_array_index_type * const restrict retarray,
	    gfc_array_c4 * const restrict array, GFC_COMPLEX_4 value,
	    const index_type * restrict pdim, GFC_LOGICAL_4 *const restrict  mask,
	    GFC_LOGICAL_4 back)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type dstride[GFC_MAX_DIMENSIONS];
  index_type * restrict dest;
  index_type rank;
  index_type n;
  index_type len;
  index_type dim;
  bool continue_loop;

  if (mask == NULL || *mask)
    {
      findloc1_c4 (retarray, array, value, pdim, back);
      return;
    }
    /* Make dim zero based to avoid confusion.  */
  rank = GFC_DESCRIPTOR_RANK (array) - 1;
  dim = (*pdim) - 1;

  if (unlikely (dim < 0 || dim > rank))
    {
      runtime_error ("Dim argument incorrect in FINDLOC intrinsic: "
 		     "is %ld, should be between 1 and %ld",
		     (long int) dim + 1, (long int) rank + 1);
    }

  len = GFC_DESCRIPTOR_EXTENT(array,dim);
  if (len < 0)
    len = 0;

  for (n = 0; n < dim; n++)
    {
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);

      if (extent[n] <= 0)
	extent[n] = 0;
    }

  for (n = dim; n < rank; n++)
    {
      extent[n] =
	GFC_DESCRIPTOR_EXTENT(array,n + 1);

      if (extent[n] <= 0)
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

      retarray->base_addr = xmallocarray (alloc_size, sizeof (index_type));
      if (alloc_size == 0)
	{
	  /* Make sure we have a zero-sized array.  */
	  GFC_DIMENSION_SET(retarray->dim[0], 0, -1, 1);
	  return;
	}
    }
  else
    {
      if (rank != GFC_DESCRIPTOR_RANK (retarray))
	runtime_error ("rank of return array incorrect in"
		       " FINDLOC intrinsic: is %ld, should be %ld",
		       (long int) (GFC_DESCRIPTOR_RANK (retarray)),
		       (long int) rank);

      if (unlikely (compile_options.bounds_check))
	bounds_ifunction_return ((array_t *) retarray, extent,
				 "return value", "FINDLOC");
    }

  for (n = 0; n < rank; n++)
    {
      count[n] = 0;
      dstride[n] = GFC_DESCRIPTOR_STRIDE(retarray,n);
      if (extent[n] <= 0)
	return;
    }
  dest = retarray->base_addr;
  continue_loop = 1;

  while (continue_loop)
    {
      *dest = 0;

      count[0]++;
      dest += dstride[0];
      n = 0;
      while (count[n] == extent[n])
	{
	  count[n] = 0;
	  dest -= dstride[n] * extent[n];
	  n++;
	  if (n >= rank)
	    {
	      continue_loop = 0;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      dest += dstride[n];
	    }
	}
    }
}
#endif
