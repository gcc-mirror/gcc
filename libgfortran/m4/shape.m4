`/* Implementation of the SHAPE intrinsic
   Copyright 2002, 2006, 2007, 2009, 2010 Free Software Foundation, Inc.
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
#include <stdlib.h>
#include <assert.h>'

include(iparm.m4)dnl

`#if defined (HAVE_'rtype_name`)

extern void shape_'rtype_kind` ('rtype` * const restrict ret, 
	const 'rtype` * const restrict array);
export_proto(shape_'rtype_kind`);

void
shape_'rtype_kind` ('rtype` * const restrict ret, 
	const 'rtype` * const restrict array)
{
  int n;
  index_type stride;
  index_type extent;
  int rank;

  rank = GFC_DESCRIPTOR_RANK (array);

  if (ret->data == NULL)
    {
      GFC_DIMENSION_SET(ret->dim[0], 0, rank - 1, 1);
      ret->offset = 0;
      ret->data = internal_malloc_size (sizeof ('rtype_name`) * rank);
    }

  stride = GFC_DESCRIPTOR_STRIDE(ret,0);

  if (GFC_DESCRIPTOR_EXTENT(ret,0) < 1)
    return;

  for (n = 0; n < rank; n++)
    {
      extent = GFC_DESCRIPTOR_EXTENT(array,n);
      ret->data[n * stride] = extent > 0 ? extent : 0 ;
    }
}

#endif'
