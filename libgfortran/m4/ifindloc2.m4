`/* Implementation of the FINDLOC intrinsic
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

#ifdef HAVE_'atype_name`'`
'header1`'`
{
  index_type i;
  index_type sstride;
  index_type extent;
  const 'atype_name`'` * restrict src;

  extent = GFC_DESCRIPTOR_EXTENT(array,0);
  if (extent <= 0)
    return 0;

  sstride = GFC_DESCRIPTOR_STRIDE(array,0) * 'base_mult`'`;
  if (back)
    {
      src = array->base_addr + (extent - 1) * sstride;
      for (i = extent; i > 0; i--)
	{
	  if ('comparison`'`)
	    return i;
	  src -= sstride;
	}
    }
  else
    {
      src = array->base_addr;
      for (i = 1; i <= extent; i++)
	{
	  if ('comparison`'`)
	    return i;
	  src += sstride;
	}
    }
  return 0;
}

'header2`'`
{
  index_type i;
  index_type sstride;
  index_type extent;
  const 'atype_name`'` * restrict src;
  const GFC_LOGICAL_1 * restrict mbase;
  int mask_kind;
  index_type mstride;

  extent = GFC_DESCRIPTOR_EXTENT(array,0);
  if (extent <= 0)
    return 0;

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

  sstride = GFC_DESCRIPTOR_STRIDE(array,0) * 'base_mult`'`;
  mstride = GFC_DESCRIPTOR_STRIDE_BYTES(mask,0);

  if (back)
    {
      src = array->base_addr + (extent - 1) * sstride;
      mbase += (extent - 1) * mstride;
      for (i = extent; i > 0; i--)
	{
	  if (*mbase && ('comparison`'`))
	    return i;
	  src -= sstride;
	  mbase -= mstride;
	}
    }
  else
    {
      src = array->base_addr;
      for (i = 1; i <= extent; i++)
	{
	  if (*mbase && ('comparison`'`))
	    return i;
	  src += sstride;
	  mbase += mstride;
	}
    }
  return 0;
}
'header3`'`
{
  if (mask == NULL || *mask)
    {
      return findloc2_'atype_code` (array, value, back, len_array, len_value);
    }
  return 0;
}
#endif'
