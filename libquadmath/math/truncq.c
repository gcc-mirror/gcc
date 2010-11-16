/* Truncate argument to nearest integral value not larger than the argument.
   Copyright (C) 1997, 1999 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997 and
   		  Jakub Jelinek <jj@ultra.linux.cz>, 1999.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#include "quadmath-imp.h"


__float128
truncq (__float128 x)
{
  int32_t j0;
  uint64_t i0, i1, sx;

  GET_FLT128_WORDS64 (i0, i1, x);
  sx = i0 & 0x8000000000000000ULL;
  j0 = ((i0 >> 48) & 0x7fff) - 0x3fff;
  if (j0 < 48)
    {
      if (j0 < 0)
	/* The magnitude of the number is < 1 so the result is +-0.  */
	SET_FLT128_WORDS64 (x, sx, 0);
      else
	SET_FLT128_WORDS64 (x, i0 & ~(0x0000ffffffffffffLL >> j0), 0);
    }
  else if (j0 > 111)
    {
      if (j0 == 0x4000)
	/* x is inf or NaN.  */
	return x + x;
    }
  else
    {
      SET_FLT128_WORDS64 (x, i0, i1 & ~(0xffffffffffffffffULL >> (j0 - 48)));
    }

  return x;
}
