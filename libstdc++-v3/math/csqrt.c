/* Complex square root of double value. */

/* Copyright (C) 1997-1999 Free Software Foundation, Inc.

   This file is part of the GNU ISO C++ Library.  This library is free
   software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this library; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
   USA.

   As a special exception, you may use this file as part of a free software
   library without restriction.  Specifically, if other files instantiate
   templates or use macros or inline functions from this file, or you compile
   this file and link it with other files to produce an executable, this
   file does not by itself cause the resulting executable to be covered by
   the GNU General Public License.  This exception does not however
   invalidate any other reasons why the executable file might be covered by
   the GNU General Public License.  */


#include <math.h>
#include "mathconf.h"


__complex__ double
csqrt (__complex__ double x)
{
  __complex__ double res;

  if (!FINITE_P (__real__ x) || !FINITE_P (__imag__ x))
    {
      if (INFINITE_P (__imag__ x))
	{
	  __real__ res = HUGE_VAL;
	  __imag__ res = __imag__ x;
	}
      else if (INFINITE_P (__real__ x))
	{
	  if (__real__ x < 0.0)
	    {
	      __real__ res = __imag__ x != __imag__ x ? NAN : 0;
	      __imag__ res = copysign (HUGE_VAL, __imag__ x);
	    }
	  else
	    {
	      __real__ res = __real__ x;
	      __imag__ res = (__imag__ x != __imag__ x
			      ? NAN : copysign (0.0, __imag__ x));
	    }
	}
      else
	{
	  __real__ res = NAN;
	  __imag__ res = NAN;
	}
    }
  else
    {
      if (__imag__ x == 0.0)
	{
	  if (__real__ x < 0.0)
	    {
	      __real__ res = 0.0;
	      __imag__ res = copysign (sqrt (-__real__ x), __imag__ x);
	    }
	  else
	    {
	      __real__ res = fabs (sqrt (__real__ x));
	      __imag__ res = copysign (0.0, __imag__ x);
	    }
	}
      else if (__real__ x == 0.0)
	{
	  double r = sqrt (0.5 * fabs (__imag__ x));

	  __real__ res = copysign (r, __imag__ x);
	  __imag__ res = r;
	}
      else
	{
	  __complex__ double q;
	  double t, r;

	  if (fabs (__imag__ x) < 2.0e-4 * fabs (__real__ x))
	    t = 0.25 * __imag__ x * (__imag__ x / __real__ x);
	  else
	    t = 0.5 * (hypot (__real__ x, __imag__ x) - __real__ x);

	  r = sqrt (t);

	  __real__ q = __imag__ x / (2.0 * r);
	  __imag__ q = r;

	  /* Heron iteration in complex arithmetic.  */
	  res = 0.5 * (q + q / x);
	}
    }

  return res;
}
