/* Implementation of the ASSOCIATED intrinsic
   Copyright 2003 Free Software Foundation, Inc.
   Contributed by kejia Zhao (CCRG) <kejia_zh@yahoo.com.cn>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "libgfortran.h"

extern GFC_LOGICAL_4 associated (const gfc_array_void *,
				 const gfc_array_void *);
export_proto(associated);

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
