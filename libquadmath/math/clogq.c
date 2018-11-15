/* Compute complex natural logarithm.
   Copyright (C) 1997-2018 Free Software Foundation, Inc.
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

  if (__glibc_unlikely (rcls == QUADFP_ZERO && icls == QUADFP_ZERO))
    {
      /* Real and imaginary part are 0.0.  */
      __imag__ result = signbitq (__real__ x) ? (__float128) M_PIq : 0;
      __imag__ result = copysignq (__imag__ result, __imag__ x);
      /* Yes, the following line raises an exception.  */
      __real__ result = -1 / fabsq (__real__ x);
    }
  else if (__glibc_likely (rcls != QUADFP_NAN && icls != QUADFP_NAN))
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

      if (absx > FLT128_MAX / 2)
	{
	  scale = -1;
	  absx = scalbnq (absx, scale);
	  absy = (absy >= FLT128_MIN * 2 ? scalbnq (absy, scale) : 0);
	}
      else if (absx < FLT128_MIN && absy < FLT128_MIN)
	{
	  scale = FLT128_MANT_DIG;
	  absx = scalbnq (absx, scale);
	  absy = scalbnq (absy, scale);
	}

      if (absx == 1 && scale == 0)
	{
	  __real__ result = log1pq (absy * absy) / 2;
	  math_check_force_underflow_nonneg (__real__ result);
	}
      else if (absx > 1 && absx < 2 && absy < 1 && scale == 0)
	{
	  __float128 d2m1 = (absx - 1) * (absx + 1);
	  if (absy >= FLT128_EPSILON)
	    d2m1 += absy * absy;
	  __real__ result = log1pq (d2m1) / 2;
	}
      else if (absx < 1
	       && absx >= 0.5Q
	       && absy < FLT128_EPSILON / 2
	       && scale == 0)
	{
	  __float128 d2m1 = (absx - 1) * (absx + 1);
	  __real__ result = log1pq (d2m1) / 2;
	}
      else if (absx < 1
	       && absx >= 0.5Q
	       && scale == 0
	       && absx * absx + absy * absy >= 0.5Q)
	{
	  __float128 d2m1 = __quadmath_x2y2m1q (absx, absy);
	  __real__ result = log1pq (d2m1) / 2;
	}
      else
	{
	  __float128 d = hypotq (absx, absy);
	  __real__ result = logq (d) - scale * (__float128) M_LN2q;
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
