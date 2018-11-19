/* Complex hyperbolic tangent for float types.
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
ctanhq (__complex128 x)
{
  __complex128 res;

  if (__glibc_unlikely (!finiteq (__real__ x) || !finiteq (__imag__ x)))
    {
      if (isinfq (__real__ x))
	{
	  __real__ res = copysignq (1, __real__ x);
	  if (finiteq (__imag__ x) && fabsq (__imag__ x) > 1)
	    {
	      __float128 sinix, cosix;
	      sincosq (__imag__ x, &sinix, &cosix);
	      __imag__ res = copysignq (0, sinix * cosix);
	    }
	  else
	    __imag__ res = copysignq (0, __imag__ x);
	}
      else if (__imag__ x == 0)
	{
	  res = x;
	}
      else
	{
	  if (__real__ x == 0)
	    __real__ res = __real__ x;
	  else
	    __real__ res = nanq ("");
	  __imag__ res = nanq ("");

	  if (isinfq (__imag__ x))
	    feraiseexcept (FE_INVALID);
	}
    }
  else
    {
      __float128 sinix, cosix;
      __float128 den;
      const int t = (int) ((FLT128_MAX_EXP - 1) * M_LN2q / 2);

      /* tanh(x+iy) = (sinh(2x) + i*sin(2y))/(cosh(2x) + cos(2y))
	 = (sinh(x)*cosh(x) + i*sin(y)*cos(y))/(sinh(x)^2 + cos(y)^2).  */

      if (__glibc_likely (fabsq (__imag__ x) > FLT128_MIN))
	{
	  sincosq (__imag__ x, &sinix, &cosix);
	}
      else
	{
	  sinix = __imag__ x;
	  cosix = 1;
	}

      if (fabsq (__real__ x) > t)
	{
	  /* Avoid intermediate overflow when the imaginary part of
	     the result may be subnormal.  Ignoring negligible terms,
	     the real part is +/- 1, the imaginary part is
	     sin(y)*cos(y)/sinh(x)^2 = 4*sin(y)*cos(y)/exp(2x).  */
	  __float128 exp_2t = expq (2 * t);

	  __real__ res = copysignq (1, __real__ x);
	  __imag__ res = 4 * sinix * cosix;
	  __real__ x = fabsq (__real__ x);
	  __real__ x -= t;
	  __imag__ res /= exp_2t;
	  if (__real__ x > t)
	    {
	      /* Underflow (original real part of x has absolute value
		 > 2t).  */
	      __imag__ res /= exp_2t;
	    }
	  else
	    __imag__ res /= expq (2 * __real__ x);
	}
      else
	{
	  __float128 sinhrx, coshrx;
	  if (fabsq (__real__ x) > FLT128_MIN)
	    {
	      sinhrx = sinhq (__real__ x);
	      coshrx = coshq (__real__ x);
	    }
	  else
	    {
	      sinhrx = __real__ x;
	      coshrx = 1;
	    }

	  if (fabsq (sinhrx) > fabsq (cosix) * FLT128_EPSILON)
	    den = sinhrx * sinhrx + cosix * cosix;
	  else
	    den = cosix * cosix;
	  __real__ res = sinhrx * coshrx / den;
	  __imag__ res = sinix * cosix / den;
	}
      math_check_force_underflow_complex (res);
    }

  return res;
}
