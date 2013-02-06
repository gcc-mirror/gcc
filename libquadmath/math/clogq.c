/* Compute complex natural logarithm for complex __float128.
   Copyright (C) 1997-2012 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

#include "quadmath-imp.h"


__complex128
clogq (__complex128 x)
{
  __complex128 result;
  int rcls = fpclassifyq (__real__ x);
  int icls = fpclassifyq (__imag__ x);

  if (__builtin_expect (rcls == QUADFP_ZERO && icls == QUADFP_ZERO, 0))
    {
      /* Real and imaginary part are 0.0.  */
      __imag__ result = signbitq (__real__ x) ? M_PIq : 0.0Q;
      __imag__ result = copysignq (__imag__ result, __imag__ x);
      /* Yes, the following line raises an exception.  */
      __real__ result = -1.0Q / fabsq (__real__ x);
    }
  else if (__builtin_expect (rcls != QUADFP_NAN && icls != QUADFP_NAN, 1))
    {
      /* Neither real nor imaginary part is NaN.  */
      __float128 absx = fabsq (__real__ x), absy = fabsq (__imag__ x);
      int scale = 0;

      if (absx < absy)
	{
	  __float128 t = absx;
	  absx = absy;
	  absy = t;
	}

      if (absx > FLT128_MAX / 2.0)
	{
	  scale = -1;
	  absx = scalbnq (absx, scale);
	  absy = (absy >= FLT128_MIN * 2.0Q ? scalbnq (absy, scale) : 0.0Q);
	}
      else if (absx < FLT128_MIN && absy < FLT128_MIN)
	{
	  scale = FLT128_MANT_DIG;
	  absx = scalbnq (absx, scale);
	  absy = scalbnq (absy, scale);
	}

      if (absx == 1.0Q && scale == 0)
	{
	  __float128 absy2 = absy * absy;
	  if (absy2 <= FLT128_MIN * 2.0Q)
	    __real__ result = absy2 / 2.0Q - absy2 * absy2 / 4.0Q;
	  else
	    __real__ result = log1pq (absy2) / 2.0Q;
	}
      else if (absx > 1.0Q && absx < 2.0Q && absy < 1.0Q && scale == 0)
	{
	  __float128 d2m1 = (absx - 1.0Q) * (absx + 1.0Q);
	  if (absy >= FLT128_EPSILON)
	    d2m1 += absy * absy;
	  __real__ result = log1pq (d2m1) / 2.0Q;
	}
      else if (absx < 1.0Q
	       && absx >= 0.75Q
	       && absy < FLT128_EPSILON / 2.0Q
	       && scale == 0)
	{
	  __float128 d2m1 = (absx - 1.0Q) * (absx + 1.0Q);
	  __real__ result = log1pq (d2m1) / 2.0Q;
	}
      else if (absx < 1.0 && (absx >= 0.75Q || absy >= 0.5Q) && scale == 0)
	{
	  __float128 d2m1 = __quadmath_x2y2m1q (absx, absy);
	  __real__ result = log1pq (d2m1) / 2.0Q;
	}
      else
	{
	  __float128 d = hypotq (absx, absy);
	  __real__ result = logq (d) - scale * M_LN2q;
	}

      __imag__ result = atan2q (__imag__ x, __real__ x);
    }
  else
    {
      __imag__ result = nanq ("");
      if (rcls == QUADFP_INFINITE || icls == QUADFP_INFINITE)
	/* Real or imaginary part is infinite.  */
	__real__ result = HUGE_VALQ;
      else
	__real__ result = nanq ("");
    }

  return result;
}
