/* Complex cosine hyperbole function for complex __float128.
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

#ifdef HAVE_FENV_H
# include <fenv.h>
#endif


__complex128
ccoshq (__complex128 x)
{
  __complex128 retval;
  int rcls = fpclassifyq (__real__ x);
  int icls = fpclassifyq (__imag__ x);

  if (__builtin_expect (rcls >= QUADFP_ZERO, 1))
    {
      /* Real part is finite.  */
      if (__builtin_expect (icls >= QUADFP_ZERO, 1))
	{
	  /* Imaginary part is finite.  */
	  const int t = (int) ((FLT128_MAX_EXP - 1) * M_LN2q);
	  __float128 sinix, cosix;

	  if (__builtin_expect (icls != QUADFP_SUBNORMAL, 1))
	    {
	      sincosq (__imag__ x, &sinix, &cosix);
	    }
	  else
	    {
	      sinix = __imag__ x;
	      cosix = 1.0Q;
	    }

	  if (fabsq (__real__ x) > t)
	    {
	      __float128 exp_t = expq (t);
	      __float128 rx = fabsq (__real__ x);
	      if (signbitq (__real__ x))
		sinix = -sinix;
	      rx -= t;
	      sinix *= exp_t / 2.0Q;
	      cosix *= exp_t / 2.0Q;
	      if (rx > t)
		{
		  rx -= t;
		  sinix *= exp_t;
		  cosix *= exp_t;
		}
	      if (rx > t)
		{
		  /* Overflow (original real part of x > 3t).  */
		  __real__ retval = FLT128_MAX * cosix;
		  __imag__ retval = FLT128_MAX * sinix;
		}
	      else
		{
		  __float128 exp_val = expq (rx);
		  __real__ retval = exp_val * cosix;
		  __imag__ retval = exp_val * sinix;
		}
	    }
	  else
	    {
	      __real__ retval = coshq (__real__ x) * cosix;
	      __imag__ retval = sinhq (__real__ x) * sinix;
	    }
	}
      else
	{
	  __imag__ retval = __real__ x == 0.0Q ? 0.0Q : nanq ("");
	  __real__ retval = nanq ("") + nanq ("");

#ifdef HAVE_FENV_H
	  if (icls == QUADFP_INFINITE)
	    feraiseexcept (FE_INVALID);
#endif
        }
    }
  else if (rcls == QUADFP_INFINITE)
    {
      /* Real part is infinite.  */
      if (__builtin_expect (icls > QUADFP_ZERO, 1))
	{
	  /* Imaginary part is finite.  */
	  __float128 sinix, cosix;

	  if (__builtin_expect (icls != QUADFP_SUBNORMAL, 1))
	    {
	      sincosq (__imag__ x, &sinix, &cosix);
	    }
	  else
	    {
	      sinix = __imag__ x;
	      cosix = 1.0Q;
	    }

	  __real__ retval = copysignq (HUGE_VALQ, cosix);
	  __imag__ retval = (copysignq (HUGE_VALQ, sinix)
			     * copysignq (1.0Q, __real__ x));
	}
      else if (icls == QUADFP_ZERO)
	{
	  /* Imaginary part is 0.0.  */
	  __real__ retval = HUGE_VALQ;
	  __imag__ retval = __imag__ x * copysignq (1.0Q, __real__ x);
	}
      else
	{
	  /* The addition raises the invalid exception.  */
	  __real__ retval = HUGE_VALQ;
	  __imag__ retval = nanq ("") + nanq ("");

#ifdef HAVE_FENV_H
	  if (icls == QUADFP_INFINITE)
	    feraiseexcept (FE_INVALID);
#endif
	 }
    }
  else
    {
      __real__ retval = nanq ("");
      __imag__ retval = __imag__ x == 0.0 ? __imag__ x : nanq ("");
    }

  return retval;
}
