/* Round argument to nearest integral value according to current rounding
   direction.
   Copyright (C) 1997, 1999, 2004, 2006 Free Software Foundation, Inc.
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

static const __float128 two112[2] =
{
  5.19229685853482762853049632922009600E+33Q, /* 0x406F000000000000, 0 */
 -5.19229685853482762853049632922009600E+33Q  /* 0xC06F000000000000, 0 */
};

long int
lrintq (__float128 x)
{
  int32_t j0;
  uint64_t i0,i1;
  volatile __float128 w;
  __float128 t;
  long int result;
  int sx;

  GET_FLT128_WORDS64 (i0, i1, x);
  j0 = ((i0 >> 48) & 0x7fff) - 0x3fff;
  sx = i0 >> 63;
  i0 &= 0x0000ffffffffffffLL;
  i0 |= 0x0001000000000000LL;

  if (j0 < 48)
    {
      w = two112[sx] + x;
      t = w - two112[sx];
      GET_FLT128_WORDS64 (i0, i1, t);
      j0 = ((i0 >> 48) & 0x7fff) - 0x3fff;
      i0 &= 0x0000ffffffffffffLL;
      i0 |= 0x0001000000000000LL;

      result = (j0 < 0 ? 0 : i0 >> (48 - j0));
    }
  else if (j0 < (int32_t) (8 * sizeof (long int)) - 1)
    {
      if (j0 >= 112)
	result = ((long int) i0 << (j0 - 48)) | (i1 << (j0 - 112));
      else
	{
	  w = two112[sx] + x;
	  t = w - two112[sx];
	  GET_FLT128_WORDS64 (i0, i1, t);
	  j0 = ((i0 >> 48) & 0x7fff) - 0x3fff;
	  i0 &= 0x0000ffffffffffffLL;
	  i0 |= 0x0001000000000000LL;

	  if (j0 == 48)
	    result = (long int) i0;
	  else
	    result = ((long int) i0 << (j0 - 48)) | (i1 >> (112 - j0));
	}
    }
  else
    {
      /* The number is too large.  It is left implementation defined
	 what happens.  */
      return (long int) x;
    }

  return sx ? -result : result;
}
