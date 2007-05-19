/* Implementation of ishftc intrinsic.
   Copyright 2002, 2004 Free Software Foundation, Inc.
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

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "libgfortran.h"

extern GFC_INTEGER_4 ishftc4 (GFC_INTEGER_4, GFC_INTEGER_4, GFC_INTEGER_4);
export_proto(ishftc4);

GFC_INTEGER_4
ishftc4 (GFC_INTEGER_4 i, GFC_INTEGER_4 shift, GFC_INTEGER_4 size)
{
  GFC_UINTEGER_4 mask, bits;

  if (shift < 0)
    shift = shift + size;

  if (shift == 0 || shift == size)
    return i;

  /* In C, the result of the shift operator is undefined if the right operand
     is greater than or equal to the number of bits in the left operand. So we
     have to special case it for fortran.  */
  mask = ~((size == 32) ? (GFC_UINTEGER_4)0 : (~(GFC_UINTEGER_4)0 << size));

  bits = i & mask;
  
  return (i & ~mask) | ((bits << shift) & mask) | (bits >> (size - shift));
}

extern GFC_INTEGER_8 ishftc8 (GFC_INTEGER_8, GFC_INTEGER_4, GFC_INTEGER_4);
export_proto(ishftc8);

GFC_INTEGER_8
ishftc8 (GFC_INTEGER_8 i, GFC_INTEGER_4 shift, GFC_INTEGER_4 size)
{
  GFC_UINTEGER_8 mask, bits;

  if (shift < 0)
    shift = shift + size;

  if (shift == 0 || shift == size)
    return i;

  /* In C, the result of the shift operator is undefined if the right operand
     is greater than or equal to the number of bits in the left operand. So we
     have to special case it for fortran.  */
  mask = ~((size == 64) ? (GFC_UINTEGER_8)0 : (~(GFC_UINTEGER_8)0 << size));

  bits = i & mask;
  
  return (i & ~mask) | ((bits << shift) & mask) | (bits >> (size - shift));
}

#ifdef HAVE_GFC_INTEGER_16
extern GFC_INTEGER_16 ishftc16 (GFC_INTEGER_16, GFC_INTEGER_4, GFC_INTEGER_4);
export_proto(ishftc16);

GFC_INTEGER_16
ishftc16 (GFC_INTEGER_16 i, GFC_INTEGER_4 shift, GFC_INTEGER_4 size)
{
  GFC_UINTEGER_16 mask, bits;

  if (shift < 0)
    shift = shift + size;

  if (shift == 0 || shift == size)
    return i;

  /* In C, the result of the shift operator is undefined if the right operand
     is greater than or equal to the number of bits in the left operand. So we
     have to special case it for fortran.  */
  mask = ~((size == 128) ? (GFC_UINTEGER_16)0 : (~(GFC_UINTEGER_16)0 << size));

  bits = i & mask;
  
  return (i & ~mask) | ((bits << shift) & mask) | (bits >> (size - shift));
}
#endif
