/* Complex sine hyperbole function for float.
   Copyright (C) 1997,1998 Free Software Foundation, Inc.

   This file is part of the libstdc++ version 3 distribution.

   This software is a copyrighted work licensed under the terms of the
   Cygnus libstdc++ license. Please consult the file LICENSE.STD for
   details.  */

#include <math.h>
#include "mathconf.h"


__complex__ float
csinhf (__complex__ float x)
{
  __complex__ float retval;
  int negate = signbit (__real__ x);

  __real__ x = fabsf (__real__ x);

  if (FINITEF_P (__real__ x))
    {
      /* Real part is finite.  */
      if (FINITEF_P (__imag__ x))
	{
	  /* Imaginary part is finite.  */
	  float sinh_val = sinhf (__real__ x);
	  float cosh_val = coshf (__real__ x);
	  float sinix = sin (__imag__ x);
	  float cosix = cos (__imag__ x);

	  __real__ retval = sinh_val * cosix;
	  __imag__ retval = cosh_val * sinix;

	  if (negate)
	    __real__ retval = -__real__ retval;
	}
      else
	{
	  if (__real__ x == 0.0)
	    {
	      /* Real part is 0.0.  */
	      __real__ retval = copysignf (0.0, negate ? -1.0 : 1.0);
	      __imag__ retval = NAN + NAN;
	    }
	  else
	    {
	      __real__ retval = NAN;
	      __imag__ retval = NAN;
	    }
	}
    }
  else if (INFINITEF_P (__real__ x))
    {
      /* Real part is infinite.  */
      if (__imag__ x == 0.0)
	{
	  /* Imaginary part is 0.0.  */
	  __real__ retval = negate ? -HUGE_VALF : HUGE_VALF;
	  __imag__ retval = __imag__ x;
	}
      else if (FINITEF_P (__imag__ x))
	{
	  /* Imaginary part is finite.  */
	  float sinix = sinf (__imag__ x);
	  float cosix = cosf (__imag__ x);

	  __real__ retval = copysignf (HUGE_VALF, cosix);
	  __imag__ retval = copysignf (HUGE_VALF, sinix);

	  if (negate)
	    __real__ retval = -__real__ retval;
	}
      else
	{
	  /* The addition raises the invalid exception.  */
	  __real__ retval = HUGE_VALF;
	  __imag__ retval = NAN + NAN;
	}
    }
  else
    {
      __real__ retval = NAN;
      __imag__ retval = __imag__ x == 0.0 ? __imag__ x : NAN;
    }

  return retval;
}
