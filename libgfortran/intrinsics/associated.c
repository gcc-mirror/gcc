/* Implementation of the ASSOCIATED intrinsic
   Copyright (C) 2003-2019 Free Software Foundation, Inc.
   Contributed by kejia Zhao (CCRG) <kejia_zh@yahoo.com.cn>

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

extern int associated (const gfc_array_void *, const gfc_array_void *);
export_proto(associated);

int
associated (const gfc_array_void *pointer, const gfc_array_void *target)
{
  int n, rank;

  if (GFC_DESCRIPTOR_DATA (pointer) == NULL)
    return 0;
  if (GFC_DESCRIPTOR_DATA (pointer) != GFC_DESCRIPTOR_DATA (target))
    return 0;
  if (GFC_DESCRIPTOR_DTYPE (pointer).elem_len != GFC_DESCRIPTOR_DTYPE (target).elem_len)
    return 0;
  if (GFC_DESCRIPTOR_DTYPE (pointer).type != GFC_DESCRIPTOR_DTYPE (target).type)
    return 0;

  rank = GFC_DESCRIPTOR_RANK (pointer);
  for (n = 0; n < rank; n++)
    {
      long extent;
      extent = GFC_DESCRIPTOR_EXTENT(pointer,n);

      if (extent != GFC_DESCRIPTOR_EXTENT(target,n))
        return 0;
      if (GFC_DESCRIPTOR_STRIDE(pointer,n) != GFC_DESCRIPTOR_STRIDE(target,n) && extent != 1)
        return 0;
      if (extent <= 0)
	return 0;
    }

  return 1;
}
