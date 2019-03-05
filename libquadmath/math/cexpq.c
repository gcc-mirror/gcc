/* Return value of complex exponential function for a float type.
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
cexpq (__complex128 x)
{
  __complex128 retval;
  int rcls = fpclassifyq (__real__ x);
  int icls = fpclassifyq (__imag__ x);

  if (__glibc_likely (rcls >= QUADFP_ZERO))
    {
      /* Real part is finite.  */
      if (__glibc_likely (icls >= QUADFP_ZERO))
	{
	  /* Imaginary part is finite.  */
	  const int t = (int) ((FLT128_MAX_EXP - 1) * M_LN2q);
	  __float128 sinix, cosix;

	  if (__glibc_likely (fabsq (__imag__ x) > FLT128_MIN))
	    {
	      sincosq (__imag__ x, &sinix, &cosix);
	    }
	  else
	    {
	      sinix = __imag__ x;
	      cosix = 1;
	    }

	  if (__real__ x > t)
	    {
	      __float128 exp_t = expq (t);
	      __real__ x -= t;
	      sinix *= exp_t;
	      cosix *= exp_t;
	      if (__real__ x > t)
		{
		  __real__ x -= t;
		  sinix *= exp_t;
		  cosix *= exp_t;
		}
	    }
	  if (__real__ x > t)
	    {
	      /* Overflow (original real part of x > 3t).  */
	      __real__ retval = FLT128_MAX * cosix;
	      __imag__ retval = FLT128_MAX * sinix;
	    }
	  else
	    {
	      __float128 exp_val = expq (__real__ x);
	      __real__ retval = exp_val * cosix;
	      __imag__ retval = exp_val * sinix;
	    }
	  math_check_force_underflow_complex (retval);
	}
      else
	{
	  /* If the imaginary part is +-inf or NaN and the real part
	     is not +-inf the result is NaN + iNaN.  */
	  __real__ retval = nanq ("");
	  __imag__ retval = nanq ("");

	  feraiseexcept (FE_INVALID);
	}
    }
  else if (__glibc_likely (rcls == QUADFP_INFINITE))
    {
      /* Real part is infinite.  */
      if (__glibc_likely (icls >= QUADFP_ZERO))
	{
	  /* Imaginary part is finite.  */
	  __float128 value = signbitq (__real__ x) ? 0 : HUGE_VALQ;

	  if (icls == QUADFP_ZERO)
	    {
	      /* Imaginary part is 0.0.  */
	      __real__ retval = value;
	      __imag__ retval = __imag__ x;
	    }
	  else
	    {
	      __float128 sinix, cosix;

	      if (__glibc_likely (fabsq (__imag__ x) > FLT128_MIN))
		{
		  sincosq (__imag__ x, &sinix, &cosix);
		}
	      else
		{
		  sinix = __imag__ x;
		  cosix = 1;
		}

	      __real__ retval = copysignq (value, cosix);
	      __imag__ retval = copysignq (value, sinix);
	    }
	}
      else if (signbitq (__real__ x) == 0)
	{
	  __real__ retval = HUGE_VALQ;
	  __imag__ retval = __imag__ x - __imag__ x;
	}
      else
	{
	  __real__ retval = 0;
	  __imag__ retval = copysignq (0, __imag__ x);
	}
    }
  else
    {
      /* If the real part is NaN the result is NaN + iNaN unless the
	 imaginary part is zero.  */
      __real__ retval = nanq ("");
      if (icls == QUADFP_ZERO)
	__imag__ retval = __imag__ x;
      else
	{
	  __imag__ retval = nanq ("");

	  if (rcls != QUADFP_NAN || icls != QUADFP_NAN)
	    feraiseexcept (FE_INVALID);
	}
    }

  return retval;
}
