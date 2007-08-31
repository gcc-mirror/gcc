/* Implementation of the RESHAPE intrinsic for packed arrays
   Copyright 2002, 2007 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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

Ligbfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

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
