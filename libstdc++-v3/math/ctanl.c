/* Complex tangent function for long double. */

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


__complex__ long double
ctanl (__complex__ long double x)
{
  __complex__ long double res;

  if (!FINITEL_P (__real__ x) || !FINITEL_P (__imag__ x))
    {
      if (INFINITEL_P (__imag__ x))
	{
	  __real__ res = copysignl (0.0, __real__ x);
	  __imag__ res = copysignl (1.0, __imag__ x);
	}
      else if (__real__ x == 0.0)
	{
	  res = x;
	}
      else
	{
	  __real__ res = NAN;
	  __imag__ res = NAN;
	}
    }
  else
    {
      long double sin2rx = sinl (2.0 * __real__ x);
      long double cos2rx = cosl (2.0 * __real__ x);
      long double den;

      den = cos2rx + coshl (2.0 * __imag__ x);

      __real__ res = sin2rx / den;
      __imag__ res = sinhl (2.0 * __imag__ x) / den;
    }

  return res;
}
