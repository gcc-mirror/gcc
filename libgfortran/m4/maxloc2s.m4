`/* Implementation of the MAXLOC intrinsic
   Copyright (C) 2017-2025 Free Software Foundation, Inc.
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
#include <assert.h>'
include(iparm.m4)dnl

`#if defined (HAVE_'atype_name`) && defined (HAVE_'rtype_name`)

static inline int
compare_fcn (const 'atype_name` *a, const 'atype_name` *b, gfc_charlen_type n)
{
  if (sizeof ('atype_name`) == 1)
    return memcmp (a, b, n);
  else
    return memcmp_char4 (a, b, n);
}

extern 'rtype_name` 'name`'rtype_qual`_'atype_code` ('atype` * const restrict'back_arg`,
       gfc_charlen_type);
export_proto('name`'rtype_qual`_'atype_code`);

'rtype_name`
'name`'rtype_qual`_'atype_code` ('atype` * const restrict array'back_arg`, gfc_charlen_type len)
{
  index_type ret;
  index_type sstride;
  index_type extent;
  const 'atype_name` *src;
  const 'atype_name` *maxval;
  index_type i;

  extent = GFC_DESCRIPTOR_EXTENT(array,0);
  if (extent <= 0)
    return 0;

  sstride = GFC_DESCRIPTOR_STRIDE(array,0) * len;

  ret = 1;
  src = array->base_addr;
  maxval = NULL;
  for (i=1; i<=extent; i++)
    {
      if (maxval == NULL || (back ? compare_fcn (src, maxval, len) >= 0 :
      	 	    	    	    compare_fcn (src, maxval, len) > 0))
      {
	 ret = i;
	 maxval = src;
      }
      src += sstride;
    }
  return ret;
}

extern 'rtype_name` m'name`'rtype_qual`_'atype_code` ('atype` * const restrict,
       		    	gfc_array_l1 *const restrict mask'back_arg`,
			gfc_charlen_type);
export_proto(m'name`'rtype_qual`_'atype_code`);

'rtype_name`
m'name`'rtype_qual`_'atype_code` ('atype` * const restrict array,
				 gfc_array_l1 * const restrict mask'back_arg`,
				 gfc_charlen_type len)
{
  index_type ret;
  index_type sstride;
  index_type extent;
  const 'atype_name` *src;
  const 'atype_name` *maxval;
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
      if (*mbase && (back ? compare_fcn (src, maxval, len) >= 0 :
      	 	    	   compare_fcn (src, maxval, len) > 0))
      {
	 ret = i;
	 maxval = src;
      }
      src += sstride;
      mbase += mstride;
    }
  return ret;
}

extern 'rtype_name` s'name`'rtype_qual`_'atype_code` ('atype` * const restrict,
                               GFC_LOGICAL_4 *mask'back_arg`, gfc_charlen_type);
export_proto(s'name`'rtype_qual`_'atype_code`);

'rtype_name`
s'name`'rtype_qual`_'atype_code` ('atype` * const restrict array,
				 GFC_LOGICAL_4 *mask'back_arg`, gfc_charlen_type len)
{
  if (mask)
    return 'name`'rtype_qual`_'atype_code` (array, len, back);
  else
    return 0;
}

#endif'
