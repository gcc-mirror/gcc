/* Implementation of gamma function according to ISO C.
   Copyright (C) 1997, 1999, 2002, 2004 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997 and
   		  Jakub Jelinek <jj@ultra.linux.cz, 1999.

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
tgammaq (__float128 x)
{
  /* We don't have a real gamma implementation now.  We'll use lgamma
     and the exp function.  But due to the required boundary
     conditions we must check some values separately.  */
  int64_t hx;
  uint64_t lx;
  __float128 res;
  int sign;

  GET_FLT128_WORDS64 (hx, lx, x);

  if (((hx & 0x7fffffffffffffffLL) | lx) == 0)
    /* Return value for x == 0 is Inf with divide by zero exception.  */
    return 1.0 / x;

  if (hx < 0 && (uint64_t) hx < 0xffff000000000000ULL && rintq (x) == x)
    /* Return value for integer x < 0 is NaN with invalid exception.  */
    return (x - x) / (x - x);

  if (hx == 0xffff000000000000ULL && lx == 0)
    /* x == -Inf.  According to ISO this is NaN.  */
    return x - x;

  /* XXX FIXME.  */
  res = expq (lgammaq (x));
  return signbitq (x) ? -res : res;
}
