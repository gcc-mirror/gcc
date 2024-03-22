/* Copyright (C) 1989-2024 Free Software Foundation, Inc.

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

/* Use the correct built-in function based on whether TFmode is _Float128 or
   long double.  See quad-float128.h for more details.  */
#ifndef __LONG_DOUBLE_IEEE128__
#define COPYSIGN(x,y) __builtin_copysignf128 (x, y)
#define INFINITY __builtin_inff128 ()
#define FABS __builtin_fabsf128

#else
#define COPYSIGN(x,y) __builtin_copysignl (x, y)
#define INFINITY __builtin_infl ()
#define FABS __builtin_fabsl
#endif

#define isnan __builtin_isnan
#define isinf __builtin_isinf
#define isfinite __builtin_isfinite

#if defined(FLOAT128_HW_INSNS) && !defined(__divkc3)
#define __divkc3 __divkc3_sw
#endif

#ifndef __LONG_DOUBLE_IEEE128__
#define RBIG   (__LIBGCC_KF_MAX__ / 2)
#define RMIN   (__LIBGCC_KF_MIN__)
#define RMIN2  (__LIBGCC_KF_EPSILON__)
#define RMINSCAL (1 / __LIBGCC_KF_EPSILON__)
#define RMAX2  (RBIG * RMIN2)
#else
#define RBIG   (__LIBGCC_TF_MAX__ / 2)
#define RMIN   (__LIBGCC_TF_MIN__)
#define RMIN2  (__LIBGCC_TF_EPSILON__)
#define RMINSCAL (1 / __LIBGCC_TF_EPSILON__)
#define RMAX2  (RBIG * RMIN2)
#endif

TCtype
__divkc3 (TFtype a, TFtype b, TFtype c, TFtype d)
{
  TFtype denom, ratio, x, y;
  TCtype res;

  /* long double has significant potential underflow/overflow errors that
     can be greatly reduced with a limited number of tests and adjustments.
  */

  /* Scale by max(c,d) to reduce chances of denominator overflowing.  */
  if (FABS (c) < FABS (d))
    {
      /* Prevent underflow when denominator is near max representable.  */
      if (FABS (d) >= RBIG)
	{
	  a = a / 2;
	  b = b / 2;
	  c = c / 2;
	  d = d / 2;
	}
      /* Avoid overflow/underflow issues when c and d are small.
	 Scaling up helps avoid some underflows.
	 No new overflow possible since c&d < RMIN2.  */
      if (FABS (d) < RMIN2)
	{
	  a = a * RMINSCAL;
	  b = b * RMINSCAL;
	  c = c * RMINSCAL;
	  d = d * RMINSCAL;
	}
      else
	{
	  if (((FABS (a) < RMIN) && (FABS (b) < RMAX2) && (FABS (d) < RMAX2))
	      || ((FABS (b) < RMIN) && (FABS (a) < RMAX2)
		  && (FABS (d) < RMAX2)))
	    {
	      a = a * RMINSCAL;
	      b = b * RMINSCAL;
	      c = c * RMINSCAL;
	      d = d * RMINSCAL;
	    }
	}
      ratio = c / d;
      denom = (c * ratio) + d;
      /* Choose alternate order of computation if ratio is subnormal.  */
      if (FABS (ratio) > RMIN)
	{
	  x = ((a * ratio) + b) / denom;
	  y = ((b * ratio) - a) / denom;
	}
      else
	{
	  x = ((c * (a / d)) + b) / denom;
	  y = ((c * (b / d)) - a) / denom;
	}
    }
  else
    {
      /* Prevent underflow when denominator is near max representable.  */
      if (FABS (c) >= RBIG)
	{
	  a = a / 2;
	  b = b / 2;
	  c = c / 2;
	  d = d / 2;
	}
      /* Avoid overflow/underflow issues when both c and d are small.
	 Scaling up helps avoid some underflows.
	 No new overflow possible since both c&d are less than RMIN2.  */
      if (FABS (c) < RMIN2)
	{
	  a = a * RMINSCAL;
	  b = b * RMINSCAL;
	  c = c * RMINSCAL;
	  d = d * RMINSCAL;
	}
      else
	{
	  if (((FABS (a) < RMIN) && (FABS (b) < RMAX2) && (FABS (c) < RMAX2))
	      || ((FABS (b) < RMIN) && (FABS (a) < RMAX2)
		  && (FABS (c) < RMAX2)))
	    {
	      a = a * RMINSCAL;
	      b = b * RMINSCAL;
	      c = c * RMINSCAL;
	      d = d * RMINSCAL;
	    }
	}
      ratio = d / c;
      denom = (d * ratio) + c;
      /* Choose alternate order of computation if ratio is subnormal.  */
      if (FABS (ratio) > RMIN)
	{
	  x = ((b * ratio) + a) / denom;
	  y = (b - (a * ratio)) / denom;
	}
      else
	{
	  x = (a + (d * (b / c))) / denom;
	  y = (b - (d * (a / c))) / denom;
	}
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
