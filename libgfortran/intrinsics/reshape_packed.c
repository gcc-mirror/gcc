/* Implementation of the RESHAPE intrinsic for packed arrays
   Copyright (C) 2002-2023 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Ligbfortran is distributed in the hope that it will be useful,
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

#include <string.h>

/* Reshape function where all arrays are packed.  Basically just memcpy.  */

void
reshape_packed (char * restrict ret, index_type rsize, const char * source,
		index_type ssize, const char * pad, index_type psize)
{
  index_type size;

  size = (rsize > ssize) ? ssize : rsize;
  memcpy (ret, source, size);
  ret += size;
  rsize -= size;
  while (rsize > 0)
    {
      size = (rsize > psize) ? psize : rsize;
      memcpy (ret, pad, size);
      ret += size;
      rsize -= size;
    }
}
