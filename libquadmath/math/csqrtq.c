/* Complex square root of a float type.
   Copyright (C) 1997-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Based on an algorithm by Stephen L. Moshier <moshier@world.std.com>.
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
csqrtq (__complex128 x)
{
  __complex128 res;
  int rcls = fpclassifyq (__real__ x);
  int icls = fpclassifyq (__imag__ x);

  if (__glibc_unlikely (rcls <= QUADFP_INFINITE || icls <= QUADFP_INFINITE))
    {
      if (icls == QUADFP_INFINITE)
	{
	  __real__ res = HUGE_VALQ;
	  __imag__ res = __imag__ x;
	}
      else if (rcls == QUADFP_INFINITE)
	{
	  if (__real__ x < 0)
	    {
	      __real__ res = icls == QUADFP_NAN ? nanq ("") : 0;
	      __imag__ res = copysignq (HUGE_VALQ, __imag__ x);
	    }
	  else
	    {
	      __real__ res = __real__ x;
	      __imag__ res = (icls == QUADFP_NAN
			      ? nanq ("") : copysignq (0, __imag__ x));
	    }
	}
      else
	{
	  __real__ res = nanq ("");
	  __imag__ res = nanq ("");
	}
    }
  else
    {
      if (__glibc_unlikely (icls == QUADFP_ZERO))
	{
	  if (__real__ x < 0)
	    {
	      __real__ res = 0;
	      __imag__ res = copysignq (sqrtq (-__real__ x), __imag__ x);
	    }
	  else
	    {
	      __real__ res = fabsq (sqrtq (__real__ x));
	      __imag__ res = copysignq (0, __imag__ x);
	    }
	}
      else if (__glibc_unlikely (rcls == QUADFP_ZERO))
	{
	  __float128 r;
	  if (fabsq (__imag__ x) >= 2 * FLT128_MIN)
	    r = sqrtq (0.5Q * fabsq (__imag__ x));
	  else
	    r = 0.5Q * sqrtq (2 * fabsq (__imag__ x));

	  __real__ res = r;
	  __imag__ res = copysignq (r, __imag__ x);
	}
      else
	{
	  __float128 d, r, s;
	  int scale = 0;

	  if (fabsq (__real__ x) > FLT128_MAX / 4)
	    {
	      scale = 1;
	      __real__ x = scalbnq (__real__ x, -2 * scale);
	      __imag__ x = scalbnq (__imag__ x, -2 * scale);
	    }
	  else if (fabsq (__imag__ x) > FLT128_MAX / 4)
	    {
	      scale = 1;
	      if (fabsq (__real__ x) >= 4 * FLT128_MIN)
		__real__ x = scalbnq (__real__ x, -2 * scale);
	      else
		__real__ x = 0;
	      __imag__ x = scalbnq (__imag__ x, -2 * scale);
	    }
	  else if (fabsq (__real__ x) < 2 * FLT128_MIN
		   && fabsq (__imag__ x) < 2 * FLT128_MIN)
	    {
	      scale = -((FLT128_MANT_DIG + 1) / 2);
	      __real__ x = scalbnq (__real__ x, -2 * scale);
	      __imag__ x = scalbnq (__imag__ x, -2 * scale);
	    }

	  d = hypotq (__real__ x, __imag__ x);
	  /* Use the identity   2  Re res  Im res = Im x
	     to avoid cancellation error in  d +/- Re x.  */
	  if (__real__ x > 0)
	    {
	      r = sqrtq (0.5Q * (d + __real__ x));
	      if (scale == 1 && fabsq (__imag__ x) < 1)
		{
		  /* Avoid possible intermediate underflow.  */
		  s = __imag__ x / r;
		  r = scalbnq (r, scale);
		  scale = 0;
		}
	      else
		s = 0.5Q * (__imag__ x / r);
	    }
	  else
	    {
	      s = sqrtq (0.5Q * (d - __real__ x));
	      if (scale == 1 && fabsq (__imag__ x) < 1)
		{
		  /* Avoid possible intermediate underflow.  */
		  r = fabsq (__imag__ x / s);
		  s = scalbnq (s, scale);
		  scale = 0;
		}
	      else
		r = fabsq (0.5Q * (__imag__ x / s));
	    }

	  if (scale)
	    {
	      r = scalbnq (r, scale);
	      s = scalbnq (s, scale);
	    }

	  math_check_force_underflow (r);
	  math_check_force_underflow (s);

	  __real__ res = r;
	  __imag__ res = copysignq (s, __imag__ x);
	}
    }

  return res;
}
