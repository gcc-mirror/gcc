/* Complex tangent function for a complex float type.
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
ctanq (__complex128 x)
{
  __complex128 res;

  if (__glibc_unlikely (!finiteq (__real__ x) || !finiteq (__imag__ x)))
    {
      if (isinfq (__imag__ x))
	{
	  if (finiteq (__real__ x) && fabsq (__real__ x) > 1)
	    {
	      __float128 sinrx, cosrx;
	      sincosq (__real__ x, &sinrx, &cosrx);
	      __real__ res = copysignq (0, sinrx * cosrx);
	    }
	  else
	    __real__ res = copysignq (0, __real__ x);
	  __imag__ res = copysignq (1, __imag__ x);
	}
      else if (__real__ x == 0)
	{
	  res = x;
	}
      else
	{
	  __real__ res = nanq ("");
	  if (__imag__ x == 0)
	    __imag__ res = __imag__ x;
	  else
	    __imag__ res = nanq ("");

	  if (isinfq (__real__ x))
	    feraiseexcept (FE_INVALID);
	}
    }
  else
    {
      __float128 sinrx, cosrx;
      __float128 den;
      const int t = (int) ((FLT128_MAX_EXP - 1) * M_LN2q / 2);

      /* tan(x+iy) = (sin(2x) + i*sinh(2y))/(cos(2x) + cosh(2y))
	 = (sin(x)*cos(x) + i*sinh(y)*cosh(y)/(cos(x)^2 + sinh(y)^2). */

      if (__glibc_likely (fabsq (__real__ x) > FLT128_MIN))
	{
	  sincosq (__real__ x, &sinrx, &cosrx);
	}
      else
	{
	  sinrx = __real__ x;
	  cosrx = 1;
	}

      if (fabsq (__imag__ x) > t)
	{
	  /* Avoid intermediate overflow when the real part of the
	     result may be subnormal.  Ignoring negligible terms, the
	     imaginary part is +/- 1, the real part is
	     sin(x)*cos(x)/sinh(y)^2 = 4*sin(x)*cos(x)/exp(2y).  */
	  __float128 exp_2t = expq (2 * t);

	  __imag__ res = copysignq (1, __imag__ x);
	  __real__ res = 4 * sinrx * cosrx;
	  __imag__ x = fabsq (__imag__ x);
	  __imag__ x -= t;
	  __real__ res /= exp_2t;
	  if (__imag__ x > t)
	    {
	      /* Underflow (original imaginary part of x has absolute
		 value > 2t).  */
	      __real__ res /= exp_2t;
	    }
	  else
	    __real__ res /= expq (2 * __imag__ x);
	}
      else
	{
	  __float128 sinhix, coshix;
	  if (fabsq (__imag__ x) > FLT128_MIN)
	    {
	      sinhix = sinhq (__imag__ x);
	      coshix = coshq (__imag__ x);
	    }
	  else
	    {
	      sinhix = __imag__ x;
	      coshix = 1;
	    }

	  if (fabsq (sinhix) > fabsq (cosrx) * FLT128_EPSILON)
	    den = cosrx * cosrx + sinhix * sinhix;
	  else
	    den = cosrx * cosrx;
	  __real__ res = sinrx * cosrx / den;
	  __imag__ res = sinhix * coshix / den;
	}
      math_check_force_underflow_complex (res);
    }

  return res;
}
