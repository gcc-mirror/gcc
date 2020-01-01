/* Copyright (C) 1989-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* This is a temporary specialization of code from libgcc/libgcc2.c.  */

#include "soft-fp.h"
#include "quad-float128.h"

#define COPYSIGN(x,y) __builtin_copysignf128 (x, y)
#define INFINITY __builtin_inff128 ()
#define FABS __builtin_fabsf128
#define isnan __builtin_isnan
#define isinf __builtin_isinf
#define isfinite __builtin_isfinite

#if defined(FLOAT128_HW_INSNS) && !defined(__divkc3)
#define __divkc3 __divkc3_sw
#endif

TCtype
__divkc3 (TFtype a, TFtype b, TFtype c, TFtype d)
{
  TFtype denom, ratio, x, y;
  TCtype res;

  /* ??? We can get better behavior from logarithmic scaling instead of
     the division.  But that would mean starting to link libgcc against
     libm.  We could implement something akin to ldexp/frexp as gcc builtins
     fairly easily...  */
  if (FABS (c) < FABS (d))
    {
      ratio = c / d;
      denom = (c * ratio) + d;
      x = ((a * ratio) + b) / denom;
      y = ((b * ratio) - a) / denom;
    }
  else
    {
      ratio = d / c;
      denom = (d * ratio) + c;
      x = ((b * ratio) + a) / denom;
      y = (b - (a * ratio)) / denom;
    }

  /* Recover infinities and zeros that computed as NaN+iNaN; the only cases
     are nonzero/zero, infinite/finite, and finite/infinite.  */
  if (isnan (x) && isnan (y))
    {
      if (c == 0.0 && d == 0.0 && (!isnan (a) || !isnan (b)))
	{
	  x = COPYSIGN (INFINITY, c) * a;
	  y = COPYSIGN (INFINITY, c) * b;
	}
      else if ((isinf (a) || isinf (b)) && isfinite (c) && isfinite (d))
	{
	  a = COPYSIGN (isinf (a) ? 1 : 0, a);
	  b = COPYSIGN (isinf (b) ? 1 : 0, b);
	  x = INFINITY * (a * c + b * d);
	  y = INFINITY * (b * c - a * d);
	}
      else if ((isinf (c) || isinf (d)) && isfinite (a) && isfinite (b))
	{
	  c = COPYSIGN (isinf (c) ? 1 : 0, c);
	  d = COPYSIGN (isinf (d) ? 1 : 0, d);
	  x = 0.0 * (a * c + b * d);
	  y = 0.0 * (b * c - a * d);
	}
    }

  __real__ res = x;
  __imag__ res = y;
  return res;
}
