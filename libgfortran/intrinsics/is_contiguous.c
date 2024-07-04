/* Implementation of the is_contiguous intrinsic.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
   Contributed by Thomas König <tkoenig@gcc.gnu.org>

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
<https://www.gnu.org/licenses/>.  */

#include "libgfortran.h"

GFC_LOGICAL_4
is_contiguous0 (const array_t * const restrict array)
{
  index_type dim;
  index_type n;
  index_type extent, stride;

  dim = GFC_DESCRIPTOR_RANK (array);

  extent = 1;
  for (n = 0; n < dim; n++)
    {
      stride = GFC_DESCRIPTOR_STRIDE (array, n);
      if (stride != extent)
	return 0;

      extent *= GFC_DESCRIPTOR_EXTENT (array, n);
    }

  return 1;
}
iexport(is_contiguous0);
