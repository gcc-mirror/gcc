/* Implementation of the RESHAPE intrinsic for packed arrays
   Copyright 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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

#include "config.h"
#include "libgfortran.h"

#include <string.h>

/* Reshape function where all arrays are packed.  Basically just memcpy.  */

void
reshape_packed (char * ret, index_type rsize, const char * source,
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
