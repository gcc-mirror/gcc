/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

/* Changes for 128-bit long double are
   Copyright (C) 2001 Stephen L. Moshier <moshier@na-net.ornl.gov>
   and are incorporated herein by permission of the author.  The author
   reserves the right to distribute this material elsewhere under different
   copying permissions.  These modifications are distributed here under
   the following terms:

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, see
    <http://www.gnu.org/licenses/>.  */

/* coshq(x)
 * Method :
 * mathematically coshl(x) if defined to be (exp(x)+exp(-x))/2
 *      1. Replace x by |x| (coshl(x) = coshl(-x)).
 *      2.
 *                                                      [ exp(x) - 1 ]^2
 *          0        <= x <= ln2/2  :  coshl(x) := 1 + -------------------
 *                                                         2*exp(x)
 *
 *                                                 exp(x) +  1/exp(x)
 *          ln2/2    <= x <= 22     :  coshl(x) := -------------------
 *                                                         2
 *          22       <= x <= lnovft :  coshl(x) := expq(x)/2
 *          lnovft   <= x <= ln2ovft:  coshl(x) := expq(x/2)/2 * expq(x/2)
 *          ln2ovft  <  x           :  coshl(x) := huge*huge (overflow)
 *
 * Special cases:
 *      coshl(x) is |x| if x is +INF, -INF, or NaN.
 *      only coshl(0)=1 is exact for finite x.
 */

#include "quadmath-imp.h"

static const __float128 one = 1.0, half = 0.5, huge = 1.0e4900Q,
ovf_thresh = 1.1357216553474703894801348310092223067821E4Q;

__float128
coshq (__float128 x)
{
  __float128 t, w;
  int32_t ex;
  ieee854_float128 u;

  u.value = x;
  ex = u.words32.w0 & 0x7fffffff;

  /* Absolute value of x.  */
  u.words32.w0 = ex;

  /* x is INF or NaN */
  if (ex >= 0x7fff0000)
    return x * x;

  /* |x| in [0,0.5*ln2], return 1+expm1q(|x|)^2/(2*expq(|x|)) */
  if (ex < 0x3ffd62e4) /* 0.3465728759765625 */
    {
      if (ex < 0x3fb80000) /* |x| < 2^-116 */
	return one;		/* cosh(tiny) = 1 */
      t = expm1q (u.value);
      w = one + t;

      return one + (t * t) / (w + w);
    }

  /* |x| in [0.5*ln2,40], return (exp(|x|)+1/exp(|x|)/2; */
  if (ex < 0x40044000)
    {
      t = expq (u.value);
      return half * t + half / t;
    }

  /* |x| in [22, ln(maxdouble)] return half*exp(|x|) */
  if (ex <= 0x400c62e3) /* 11356.375 */
    return half * expq (u.value);

  /* |x| in [log(maxdouble), overflowthresold] */
  if (u.value <= ovf_thresh)
    {
      w = expq (half * u.value);
      t = half * w;
      return t * w;
    }

  /* |x| > overflowthresold, cosh(x) overflow */
  return huge * huge;
}
