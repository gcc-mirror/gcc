/* sinhq.c -- __float128 version of e_sinh.c.
 * Conversion to __float128 by Ulrich Drepper,
 * Cygnus Support, drepper@cygnus.com.
 */

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

/* Changes for 128-bit __float128 are
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
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA */

/* sinhq(x)
 * Method :
 * mathematically sinh(x) if defined to be (exp(x)-exp(-x))/2
 *      1. Replace x by |x| (sinhq(-x) = -sinhq(x)).
 *      2.
 *                                                   E + E/(E+1)
 *          0        <= x <= 25     :  sinhq(x) := --------------, E=expm1q(x)
 *                                                       2
 *
 *          25       <= x <= lnovft :  sinhq(x) := expq(x)/2
 *          lnovft   <= x <= ln2ovft:  sinhq(x) := expq(x/2)/2 * expq(x/2)
 *          ln2ovft  <  x           :  sinhq(x) := x*shuge (overflow)
 *
 * Special cases:
 *      sinhq(x) is |x| if x is +INF, -INF, or NaN.
 *      only sinhq(0)=0 is exact for finite x.
 */

#include "quadmath-imp.h"

static const __float128 one = 1.0, shuge = 1.0e4931Q,
  ovf_thresh = 1.1357216553474703894801348310092223067821E4Q;

__float128
sinhq (__float128 x)
{
  __float128 t, w, h;
  uint32_t jx, ix;
  ieee854_float128 u;

  /* Words of |x|. */
  u.value = x;
  jx = u.words32.w0;
  ix = jx & 0x7fffffff;

  /* x is INF or NaN */
  if (ix >= 0x7fff0000)
    return x + x;

  h = 0.5Q;
  if (jx & 0x80000000)
    h = -h;

  /* Absolute value of x.  */
  u.words32.w0 = ix;

  /* |x| in [0,40], return sign(x)*0.5*(E+E/(E+1))) */
  if (ix <= 0x40044000)
    {
      if (ix < 0x3fc60000) /* |x| < 2^-57 */
	if (shuge + x > one)
	  return x;		/* sinh(tiny) = tiny with inexact */
      t = expm1q (u.value);
      if (ix < 0x3fff0000)
	return h * (2.0Q * t - t * t / (t + one));
      return h * (t + t / (t + one));
    }

  /* |x| in [40, log(maxdouble)] return 0.5*exp(|x|) */
  if (ix <= 0x400c62e3) /* 11356.375 */
    return h * expq (u.value);

  /* |x| in [log(maxdouble), overflowthreshold]
     Overflow threshold is log(2 * maxdouble).  */
  if (u.value <= ovf_thresh)
    {
      w = expq (0.5Q * u.value);
      t = h * w;
      return t * w;
    }

  /* |x| > overflowthreshold, sinhq(x) overflow */
  return x * shuge;
}
