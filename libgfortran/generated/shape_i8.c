/* Implementation of the SHAPE intrinsic
   Copyright 2002, 2006, 2007, 2009 Free Software Foundation, Inc.
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
#include <assert.h>


#if defined (HAVE_GFC_INTEGER_8)

extern void shape_8 (gfc_array_i8 * const restrict ret, 
	const gfc_array_i8 * const restrict array);
export_proto(shape_8);

void
shape_8 (gfc_array_i8 * const restrict ret, 
	const gfc_array_i8 * const restrict array)
{
  int n;
  index_type stride;
  index_type extent;

  stride = ret->dim[0].stride;

  if (ret->dim[0].ubound < ret->dim[0].lbound)
    return;

  for (n = 0; n < GFC_DESCRIPTOR_RANK (array); n++)
    {
      extent = array->dim[n].ubound + 1 - array->dim[n].lbound;
      ret->data[n * stride] = extent > 0 ? extent : 0 ;
    }
}

#endif
