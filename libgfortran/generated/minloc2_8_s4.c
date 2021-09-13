/* Implementation of the MINLOC intrinsic
   Copyright (C) 2017-2021 Free Software Foundation, Inc.
   Contributed by Thomas Koenig

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
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#if defined (HAVE_GFC_UINTEGER_4) && defined (HAVE_GFC_INTEGER_8)

static inline int
compare_fcn (const GFC_UINTEGER_4 *a, const GFC_UINTEGER_4 *b, gfc_charlen_type n)
{
  if (sizeof (GFC_UINTEGER_4) == 1)
    return memcmp (a, b, n);
  else
    return memcmp_char4 (a, b, n);
}

extern GFC_INTEGER_8 minloc2_8_s4 (gfc_array_s4 * const restrict, GFC_LOGICAL_4 back,
       gfc_charlen_type);
export_proto(minloc2_8_s4);

GFC_INTEGER_8
minloc2_8_s4 (gfc_array_s4 * const restrict array, GFC_LOGICAL_4 back,
				gfc_charlen_type len)
{
  index_type ret;
  index_type sstride;
  index_type extent;
  const GFC_UINTEGER_4 *src;
  const GFC_UINTEGER_4 *minval;
  index_type i;

  extent = GFC_DESCRIPTOR_EXTENT(array,0);
  if (extent <= 0)
    return 0;

  sstride = GFC_DESCRIPTOR_STRIDE(array,0) * len;

  ret = 1;
  src = array->base_addr;
  minval = NULL;
  for (i=1; i<=extent; i++)
    {
      if (minval == NULL || (back ? compare_fcn (src, minval, len) <= 0 :
      	 	    	    	    compare_fcn (src, minval, len) < 0))
      {
	 ret = i;
	 minval = src;
      }
      src += sstride;
    }
  return ret;
}

extern GFC_INTEGER_8 mminloc2_8_s4 (gfc_array_s4 * const restrict,
                    gfc_array_l1 *const restrict mask, GFC_LOGICAL_4 back,
		    gfc_charlen_type);
export_proto(mminloc2_8_s4);

GFC_INTEGER_8
mminloc2_8_s4 (gfc_array_s4 * const restrict array,
				 gfc_array_l1 * const restrict mask, GFC_LOGICAL_4 back,
				 gfc_charlen_type len)
{
  index_type ret;
  index_type sstride;
  index_type extent;
  const GFC_UINTEGER_4 *src;
  const GFC_UINTEGER_4 *maxval;
  index_type i, j;
  GFC_LOGICAL_1 *mbase;
  int mask_kind;
  index_type mstride;

  extent = GFC_DESCRIPTOR_EXTENT(array,0);
  if (extent <= 0)
    return 0;

  sstride = GFC_DESCRIPTOR_STRIDE(array,0) * len;

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

  mstride = GFC_DESCRIPTOR_STRIDE_BYTES(mask,0);

  /* Search for the first occurrence of a true element in mask. */
  for (j=0; j<extent; j++)
    {
      if (*mbase)
        break;
      mbase += mstride;
    }

  if (j == extent)
    return 0;

  ret = j + 1;
  src = array->base_addr + j * sstride;
  maxval = src;

  for (i=j+1; i<=extent; i++)
    {

      if (*mbase && (back ? compare_fcn (src, maxval, len) <= 0 :
      	 	    	    compare_fcn (src, maxval, len) < 0))
      {
	 ret = i;
	 maxval = src;
      }
      src += sstride;
      mbase += mstride;
    }
  return ret;
}

extern GFC_INTEGER_8 sminloc2_8_s4 (gfc_array_s4 * const restrict,
       		    	GFC_LOGICAL_4 *mask, GFC_LOGICAL_4 back, gfc_charlen_type);
export_proto(sminloc2_8_s4);

GFC_INTEGER_8
sminloc2_8_s4 (gfc_array_s4 * const restrict array,
				 GFC_LOGICAL_4 *mask, GFC_LOGICAL_4 back, gfc_charlen_type len)
{
  if (mask)
    return minloc2_8_s4 (array, len, back);
  else
    return 0;
}

#endif
