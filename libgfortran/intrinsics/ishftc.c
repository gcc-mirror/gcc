/* Implementation of ishftc intrinsic.
   Copyright 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfor).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "libgfortran.h"

#define ishftc4 prefix(ishftc4)
GFC_INTEGER_4 ishftc4 (GFC_INTEGER_4, GFC_INTEGER_4, GFC_INTEGER_4);

#define ishftc8 prefix(ishftc8)
GFC_INTEGER_8 ishftc8 (GFC_INTEGER_8, GFC_INTEGER_8, GFC_INTEGER_8);

GFC_INTEGER_4
ishftc4 (GFC_INTEGER_4 i, GFC_INTEGER_4 shift, GFC_INTEGER_4 size)
{
  GFC_INTEGER_4 mask;
  GFC_UINTEGER_4 bits;

  if (shift < 0)
    shift = shift + size;

  if (shift == 0 || shift == size)
    return i;

  mask = (~(GFC_INTEGER_4)0) << size;
  bits = i & ~mask;
  return (i & mask) | (bits >> (size - shift)) | ((i << shift) & ~mask);
}


GFC_INTEGER_8
ishftc8 (GFC_INTEGER_8 i, GFC_INTEGER_8 shift, GFC_INTEGER_8 size)
{
  GFC_INTEGER_8 mask;
  GFC_UINTEGER_8 bits;

  if (shift < 0)
    shift = shift + size;

  if (shift == 0 || shift == size)
    return i;

  mask = (~(GFC_INTEGER_8)0) << size;
  bits = i & ~mask;
  return (i & mask) | (bits >> (size - shift)) | ((i << shift) & ~mask);
}

