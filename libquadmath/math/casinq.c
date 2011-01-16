/* Return arc sine of complex __float128 value.
   Copyright (C) 1997 Free Software Foundation, Inc.
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
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#include "quadmath-imp.h"


__complex128
casinq (__complex128 x)
{
  __complex128 res;

  if (isnanq (__real__ x) || isnanq (__imag__ x))
    {
      if (__real__ x == 0.0)
	{
	  res = x;
	}
      else if (isinfq (__real__ x) || isinfq (__imag__ x))
	{
	  __real__ res = nanq ("");
	  __imag__ res = copysignq (HUGE_VALQ, __imag__ x);
	}
      else
	{
	  __real__ res = nanq ("");
	  __imag__ res = nanq ("");
	}
    }
  else
    {
      __complex128 y;

      __real__ y = -__imag__ x;
      __imag__ y = __real__ x;

      y = casinhq (y);

      __real__ res = __imag__ y;
      __imag__ res = -__real__ y;
    }

  return res;
}
