/* Copyright (C) 1989-2019 Free Software Foundation, Inc.

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
#define isnan __builtin_isnan
#define isinf __builtin_isinf

#if defined(FLOAT128_HW_INSNS) && !defined(__mulkc3)
#define __mulkc3 __mulkc3_sw
#endif

TCtype
__mulkc3 (TFtype a, TFtype b, TFtype c, TFtype d)
{
  TFtype ac, bd, ad, bc, x, y;
  TCtype res;

  ac = a * c;
  bd = b * d;
  ad = a * d;
  bc = b * c;

  x = ac - bd;
  y = ad + bc;

  if (isnan (x) && isnan (y))
    {
      /* Recover infinities that computed as NaN + iNaN.  */
      _Bool recalc = 0;
      if (isinf (a) || isinf (b))
	{
	  /* z is infinite.  "Box" the infinity and change NaNs in
	     the other factor to 0.  */
	  a = COPYSIGN (isinf (a) ? 1 : 0, a);
	  b = COPYSIGN (isinf (b) ? 1 : 0, b);
	  if (isnan (c)) c = COPYSIGN (0, c);
	  if (isnan (d)) d = COPYSIGN (0, d);
          recalc = 1;
	}
     if (isinf (c) || isinf (d))
	{
	  /* w is infinite.  "Box" the infinity and change NaNs in
	     the other factor to 0.  */
	  c = COPYSIGN (isinf (c) ? 1 : 0, c);
	  d = COPYSIGN (isinf (d) ? 1 : 0, d);
	  if (isnan (a)) a = COPYSIGN (0, a);
	  if (isnan (b)) b = COPYSIGN (0, b);
	  recalc = 1;
	}
     if (!recalc
	  && (isinf (ac) || isinf (bd)
	      || isinf (ad) || isinf (bc)))
	{
	  /* Recover infinities from overflow by changing NaNs to 0.  */
	  if (isnan (a)) a = COPYSIGN (0, a);
	  if (isnan (b)) b = COPYSIGN (0, b);
	  if (isnan (c)) c = COPYSIGN (0, c);
	  if (isnan (d)) d = COPYSIGN (0, d);
	  recalc = 1;
	}
      if (recalc)
	{
	  x = INFINITY * (a * c - b * d);
	  y = INFINITY * (a * d + b * c);
	}
    }

  __real__ res = x;
  __imag__ res = y;
  return res;
}

