/* Implementation of the ASSOCIATED intrinsic
   Copyright 2003 Free Software Foundation, Inc.
   Contributed by kejia Zhao (CCRG) <kejia_zh@yahoo.com.cn>

This file is part of the GNU Fortran 95 runtime library (libgfor).

Libgfor is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Ligbfor is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "libgfortran.h"

#define associated prefix(associated)


GFC_LOGICAL_4
associated (const gfc_array_void *pointer, const gfc_array_void *target)
{
  int n, rank;

  if (GFC_DESCRIPTOR_DATA (pointer) != GFC_DESCRIPTOR_DATA (target))
    return 0;
  if (GFC_DESCRIPTOR_DTYPE (pointer) != GFC_DESCRIPTOR_DTYPE (target))
    return 0;

  rank = GFC_DESCRIPTOR_RANK (pointer);
  for (n = 0; n < rank; n++)
    {
      if (pointer->dim[n].stride != target->dim[n].stride)
        return 0;
      if ((pointer->dim[n].ubound - pointer->dim[n].lbound)
          != (target->dim[n].ubound - target->dim[n].lbound))
        return 0;
    }

  return 1;
}
