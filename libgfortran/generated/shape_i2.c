/* Implementation of the SHAPE intrinsic
   Copyright (C) 2002-2025 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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


#if defined (HAVE_GFC_INTEGER_2)

extern void shape_2 (gfc_array_i2 * const restrict ret, 
	const array_t * const restrict array);
export_proto(shape_2);

void
shape_2 (gfc_array_i2 * const restrict ret, 
	const array_t * const restrict array)
{
  index_type stride;
  index_type extent;

  int rank = GFC_DESCRIPTOR_RANK (array);

  if (ret->base_addr == NULL)
    {
      GFC_DIMENSION_SET(ret->dim[0], 0, rank - 1, 1);
      ret->offset = 0;
      ret->base_addr = xmallocarray (rank, sizeof (GFC_INTEGER_2));
    }

  stride = GFC_DESCRIPTOR_STRIDE(ret,0);

  if (GFC_DESCRIPTOR_EXTENT(ret,0) < 1)
    return;

  for (index_type n = 0; n < rank; n++)
    {
      extent = GFC_DESCRIPTOR_EXTENT(array,n);
      ret->base_addr[n * stride] = extent > 0 ? extent : 0 ;
    }
}

#endif
