/* Compute sine and cosine of argument.
   Copyright (C) 1997-2017 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997 and
		  Jakub Jelinek <jj@ultra.linux.cz>.

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

#include <errno.h>
#include "quadmath-imp.h"

void
sincosq (__float128 x, __float128 *sinx, __float128 *cosx)
{
  int64_t ix;

  /* High word of x. */
  GET_FLT128_MSW64 (ix, x);

  /* |x| ~< pi/4 */
  ix &= 0x7fffffffffffffffLL;
  if (ix <= 0x3ffe921fb54442d1LL)
    __quadmath_kernel_sincosq (x, 0.0Q, sinx, cosx, 0);
  else if (ix >= 0x7fff000000000000LL)
    {
      /* sin(Inf or NaN) is NaN */
      *sinx = *cosx = x - x;
      if (isinfq (x))
	errno = EDOM;
    }
  else
    {
      /* Argument reduction needed.  */
      __float128 y[2];
      int n;

      n = __quadmath_rem_pio2q (x, y);
      switch (n & 3)
	{
	case 0:
	  __quadmath_kernel_sincosq (y[0], y[1], sinx, cosx, 1);
	  break;
	case 1:
	  __quadmath_kernel_sincosq (y[0], y[1], cosx, sinx, 1);
	  *cosx = -*cosx;
	  break;
	case 2:
	  __quadmath_kernel_sincosq (y[0], y[1], sinx, cosx, 1);
	  *sinx = -*sinx;
	  *cosx = -*cosx;
	  break;
	default:
	  __quadmath_kernel_sincosq (y[0], y[1], cosx, sinx, 1);
	  *sinx = -*sinx;
	  break;
	}
    }
}
