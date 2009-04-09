/* Specific implementation of the PACK intrinsic
   Copyright (C) 2002, 2004, 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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


#if defined (HAVE_GFC_REAL_4)

/* PACK is specified as follows:

   13.14.80 PACK (ARRAY, MASK, [VECTOR])

   Description: Pack an array into an array of rank one under the
   control of a mask.

   Class: Transformational function.

   Arguments:
      ARRAY   may be of any type. It shall not be scalar.
      MASK    shall be of type LOGICAL. It shall be conformable with ARRAY.
      VECTOR  (optional) shall be of the same type and type parameters
              as ARRAY. VECTOR shall have at least as many elements as
              there are true elements in MASK. If MASK is a scalar
              with the value true, VECTOR shall have at least as many
              elements as there are in ARRAY.

   Result Characteristics: The result is an array of rank one with the
   same type and type parameters as ARRAY. If VECTOR is present, the
   result size is that of VECTOR; otherwise, the result size is the
   number /t/ of true elements in MASK unless MASK is scalar with the
   value true, in which case the result size is the size of ARRAY.

   Result Value: Element /i/ of the result is the element of ARRAY
   that corresponds to the /i/th true element of MASK, taking elements
   in array element order, for /i/ = 1, 2, ..., /t/. If VECTOR is
   present and has size /n/ > /t/, element /i/ of the result has the
   value VECTOR(/i/), for /i/ = /t/ + 1, ..., /n/.

   Examples: The nonzero elements of an array M with the value
   | 0 0 0 |
   | 9 0 0 | may be "gathered" by the function PACK. The result of
   | 0 0 7 |
   PACK (M, MASK = M.NE.0) is [9,7] and the result of PACK (M, M.NE.0,
   VECTOR = (/ 2,4,6,8,10,12 /)) is [9,7,6,8,10,12].

There are two variants of the PACK intrinsic: one, where MASK is
array valued, and the other one where MASK is scalar.  */

void
pack_r4 (gfc_array_r4 *ret, const gfc_array_r4 *array,
	       const gfc_array_l1 *mask, const gfc_array_r4 *vector)
{
  /* r.* indicates the return array.  */
  index_type rstride0;
  GFC_REAL_4 * restrict rptr;
  /* s.* indicates the source array.  */
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type sstride0;
  const GFC_REAL_4 *sptr;
  /* m.* indicates the mask array.  */
  index_type mstride[GFC_MAX_DIMENSIONS];
  index_type mstride0;
  const GFC_LOGICAL_1 *mptr;

  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  int zero_sized;
  index_type n;
  index_type dim;
  index_type nelem;
  index_type total;
  int mask_kind;

  dim = GFC_DESCRIPTOR_RANK (array);

  mptr = mask->data;

  /* Use the same loop for all logical types, by using GFC_LOGICAL_1
     and using shifting to address size and endian issues.  */

  mask_kind = GFC_DESCRIPTOR_SIZE (mask);

  if (mask_kind == 1 || mask_kind == 2 || mask_kind == 4 || mask_kind == 8
#ifdef HAVE_GFC_LOGICAL_16
      || mask_kind == 16
#endif
      )
    {
      /*  Do not convert a NULL pointer as we use test for NULL below.  */
      if (mptr)
	mptr = GFOR_POINTER_TO_L1 (mptr, mask_kind);
    }
  else
    runtime_error ("Funny sized logical array");

  zero_sized = 0;
  for (n = 0; n < dim; n++)
    {
      count[n] = 0;
      extent[n] = array->dim[n].ubound + 1 - array->dim[n].lbound;
      if (extent[n] <= 0)
       zero_sized = 1;
      sstride[n] = array->dim[n].stride;
      mstride[n] = mask->dim[n].stride * mask_kind;
    }
  if (sstride[0] == 0)
    sstride[0] = 1;
  if (mstride[0] == 0)
    mstride[0] = mask_kind;

  if (zero_sized)
    sptr = NULL;
  else
    sptr = array->data;

  if (ret->data == NULL || compile_options.bounds_check)
    {
      /* Count the elements, either for allocating memory or
	 for bounds checking.  */

      if (vector != NULL)
	{
	  /* The return array will have as many
	     elements as there are in VECTOR.  */
	  total = vector->dim[0].ubound + 1 - vector->dim[0].lbound;
	  if (total < 0)
	    {
	      total = 0;
	      vector = NULL;
	    }
	}
      else
	{
	  /* We have to count the true elements in MASK.  */

	  /* TODO: We could speed up pack easily in the case of only
	     few .TRUE. entries in MASK, by keeping track of where we
	     would be in the source array during the initial traversal
	     of MASK, and caching the pointers to those elements. Then,
	     supposed the number of elements is small enough, we would
	     only have to traverse the list, and copy those elements
	     into the result array. In the case of datatypes which fit
	     in one of the integer types we could also cache the
	     value instead of a pointer to it.
	     This approach might be bad from the point of view of
	     cache behavior in the case where our cache is not big
	     enough to hold all elements that have to be copied.  */

	  const GFC_LOGICAL_1 *m = mptr;

	  total = 0;
	  if (zero_sized)
	    m = NULL;

	  while (m)
	    {
	      /* Test this element.  */
	      if (*m)
		total++;

	      /* Advance to the next element.  */
	      m += mstride[0];
	      count[0]++;
	      n = 0;
	      while (count[n] == extent[n])
		{
		  /* When we get to the end of a dimension, reset it
		     and increment the next dimension.  */
		  count[n] = 0;
		  /* We could precalculate this product, but this is a
		     less frequently used path so probably not worth
		     it.  */
		  m -= mstride[n] * extent[n];
		  n++;
		  if (n >= dim)
		    {
		      /* Break out of the loop.  */
		      m = NULL;
		      break;
		    }
		  else
		    {
		      count[n]++;
		      m += mstride[n];
		    }
		}
	    }
	}

      if (ret->data == NULL)
	{
	  /* Setup the array descriptor.  */
	  ret->dim[0].lbound = 0;
	  ret->dim[0].ubound = total - 1;
	  ret->dim[0].stride = 1;

	  ret->offset = 0;
	  if (total == 0)
	    {
	      /* In this case, nothing remains to be done.  */
	      ret->data = internal_malloc_size (1);
	      return;
	    }
	  else
	    ret->data = internal_malloc_size (sizeof (GFC_REAL_4) * total);
	}
      else 
	{
	  /* We come here because of range checking.  */
	  index_type ret_extent;

	  ret_extent = ret->dim[0].ubound + 1 - ret->dim[0].lbound;
	  if (total != ret_extent)
	    runtime_error ("Incorrect extent in return value of PACK intrinsic;"
			   " is %ld, should be %ld", (long int) total,
			   (long int) ret_extent);
	}
    }

  rstride0 = ret->dim[0].stride;
  if (rstride0 == 0)
    rstride0 = 1;
  sstride0 = sstride[0];
  mstride0 = mstride[0];
  rptr = ret->data;

  while (sptr && mptr)
    {
      /* Test this element.  */
      if (*mptr)
        {
          /* Add it.  */
	  *rptr = *sptr;
          rptr += rstride0;
        }
      /* Advance to the next element.  */
      sptr += sstride0;
      mptr += mstride0;
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
          mptr -= mstride[n] * extent[n];
          n++;
          if (n >= dim)
            {
              /* Break out of the loop.  */
              sptr = NULL;
              break;
            }
          else
            {
              count[n]++;
              sptr += sstride[n];
              mptr += mstride[n];
            }
        }
    }

  /* Add any remaining elements from VECTOR.  */
  if (vector)
    {
      n = vector->dim[0].ubound + 1 - vector->dim[0].lbound;
      nelem = ((rptr - ret->data) / rstride0);
      if (n > nelem)
        {
          sstride0 = vector->dim[0].stride;
          if (sstride0 == 0)
            sstride0 = 1;

          sptr = vector->data + sstride0 * nelem;
          n -= nelem;
          while (n--)
            {
	      *rptr = *sptr;
              rptr += rstride0;
              sptr += sstride0;
            }
        }
    }
}

#endif

