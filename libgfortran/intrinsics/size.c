/* Implementation of the size intrinsic.
   Copyright 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfor).

Libgfor is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Libgfor is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "libgfortran.h"

index_type size0 (const array_t * array)
{
  int n;
  index_type size;
  index_type len;

  size = 1;
  for (n = 0; n < GFC_DESCRIPTOR_RANK (array); n++)
    {
      len = array->dim[n].ubound + 1 - array->dim[n].lbound;
      if (len < 0)
        len = 0;
      size *= len;
    }
  return size;
}

#define size1 prefix(size1)
index_type size1 (const array_t * array, index_type dim)
{
  index_type size;

  dim--;

  size = array->dim[dim].ubound + 1 - array->dim[dim].lbound;
  if (size < 0)
    size = 0;
  return size;
}

