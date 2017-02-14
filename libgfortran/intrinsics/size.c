/* Implementation of the size intrinsic.
   Copyright (C) 2002-2017 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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

index_type
size0 (const array_t * array)
{
  int n;
  index_type size;
  index_type len;

  size = 1;
  for (n = 0; n < GFC_DESCRIPTOR_RANK (array); n++)
    {
      len = GFC_DESCRIPTOR_EXTENT(array,n);
      if (len < 0)
        len = 0;
      size *= len;
    }
  return size;
}
iexport(size0);

extern index_type size1 (const array_t * array, index_type dim);
export_proto(size1);

index_type
size1 (const array_t * array, index_type dim)
{
  index_type size;

  dim--;

  size = GFC_DESCRIPTOR_EXTENT(array,dim);
  if (size < 0)
    size = 0;
  return size;
}
