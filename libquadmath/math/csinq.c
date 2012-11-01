/* Complex sine function for complex __float128.
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
csinq (__complex128 x)
{
  __complex128 retval;
  int negate = signbitq (__real__ x);
  int rcls = fpclassifyq (__real__ x);
  int icls = fpclassifyq (__imag__ x);

  __real__ x = fabsq (__real__ x);

  if (__builtin_expect (icls >= QUADFP_ZERO, 1))
    {
      /* Imaginary part is finite.  */
      if (__builtin_expect (rcls >= QUADFP_ZERO, 1))
	{
	  /* Real part is finite.  */
	  const int t = (int) ((FLT128_MAX_EXP - 1) * M_LN2q);
	  __float128 sinix, cosix;

	  if (__builtin_expect (rcls != QUADFP_SUBNORMAL, 1))
	    {
	      sincosq (__real__ x, &sinix, &cosix);
	    }
	  else
	    {
	      sinix = __real__ x;
	      cosix = 1.0Q;
	    }

	  if (fabsq (__imag__ x) > t)
	    {
	      __float128 exp_t = expq (t);
	      __float128 ix = fabsq (__imag__ x);
	      if (signbitq (__imag__ x))
		cosix = -cosix;
	      ix -= t;
	      sinix *= exp_t / 2.0Q;
	      cosix *= exp_t / 2.0Q;
	      if (ix > t)
		{
		  ix -= t;
		  sinix *= exp_t;
		  cosix *= exp_t;
		}
	      if (ix > t)
		{
		  /* Overflow (original imaginary part of x > 3t).  */
		  __real__ retval = FLT128_MAX * sinix;
		  __imag__ retval = FLT128_MAX * cosix;
		}
	      else
		{
		  __float128 exp_val = expq (ix);
		  __real__ retval = exp_val * sinix;
		  __imag__ retval = exp_val * cosix;
		}
	    }
	  else
	    {
	      __real__ retval = coshq (__imag__ x) * sinix;
	      __imag__ retval = sinhq (__imag__ x) * cosix;
	    }

	  if (negate)
	    __real__ retval = -__real__ retval;
	}
      else
	{
	  if (icls == QUADFP_ZERO)
	    {
	      /* Imaginary part is 0.0.  */
	      __real__ retval = nanq ("");
	      __imag__ retval = __imag__ x;

#ifdef HAVE_FENV_H
	      if (rcls == QUADFP_INFINITE)
		feraiseexcept (FE_INVALID);
#endif
	    }
	  else
	    {
	      __real__ retval = nanq ("");
	      __imag__ retval = nanq ("");

#ifdef HAVE_FENV_H
	      feraiseexcept (FE_INVALID);
#endif
	    }
	}
    }
  else if (icls == QUADFP_INFINITE)
    {
      /* Imaginary part is infinite.  */
      if (rcls == QUADFP_ZERO)
	{
	  /* Real part is 0.0.  */
	  __real__ retval = copysignq (0.0Q, negate ? -1.0Q : 1.0Q);
	  __imag__ retval = __imag__ x;
	}
      else if (rcls > QUADFP_ZERO)
	{
	  /* Real part is finite.  */
	  __float128 sinix, cosix;

	  if (__builtin_expect (rcls != QUADFP_SUBNORMAL, 1))
	    {
	      sincosq (__real__ x, &sinix, &cosix);
	    }
	  else
	    {
	      sinix = __real__ x;
	      cosix = 1.0;
	    }

	  __real__ retval = copysignq (HUGE_VALQ, sinix);
	  __imag__ retval = copysignq (HUGE_VALQ, cosix);

	  if (negate)
	    __real__ retval = -__real__ retval;
	  if (signbitq (__imag__ x))
	    __imag__ retval = -__imag__ retval;
	}
      else
	{
	  /* The addition raises the invalid exception.  */
	  __real__ retval = nanq ("");
	  __imag__ retval = HUGE_VALQ;

#ifdef HAVE_FENV_H
	  if (rcls == QUADFP_INFINITE)
	    feraiseexcept (FE_INVALID);
#endif
	}
    }
  else
    {
      if (rcls == QUADFP_ZERO)
	__real__ retval = copysignq (0.0Q, negate ? -1.0Q : 1.0Q);
      else
	__real__ retval = nanq ("");
      __imag__ retval = nanq ("");
    }

  return retval;
}
