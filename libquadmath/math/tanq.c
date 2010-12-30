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

/*
  Long double expansions are
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

/* __quadmath_kernel_tanq( x, y, k )
 * kernel tan function on [-pi/4, pi/4], pi/4 ~ 0.7854
 * Input x is assumed to be bounded by ~pi/4 in magnitude.
 * Input y is the tail of x.
 * Input k indicates whether tan (if k=1) or
 * -1/tan (if k= -1) is returned.
 *
 * Algorithm
 *	1. Since tan(-x) = -tan(x), we need only to consider positive x.
 *	2. if x < 2^-57, return x with inexact if x!=0.
 *	3. tan(x) is approximated by a rational form x + x^3 / 3 + x^5 R(x^2)
 *          on [0,0.67433].
 *
 *	   Note: tan(x+y) = tan(x) + tan'(x)*y
 *		          ~ tan(x) + (1+x*x)*y
 *	   Therefore, for better accuracy in computing tan(x+y), let
 *		r = x^3 * R(x^2)
 *	   then
 *		tan(x+y) = x + (x^3 / 3 + (x^2 *(r+y)+y))
 *
 *      4. For x in [0.67433,pi/4],  let y = pi/4 - x, then
 *		tan(x) = tan(pi/4-y) = (1-tan(y))/(1+tan(y))
 *		       = 1 - 2*(tan(y) - (tan(y)^2)/(1+tan(y)))
 */

#include "quadmath-imp.h"



static const __float128
  one = 1.0Q,
  pio4hi = 7.8539816339744830961566084581987569936977E-1Q,
  pio4lo = 2.1679525325309452561992610065108379921906E-35Q,

  /* tan x = x + x^3 / 3 + x^5 T(x^2)/U(x^2)
     0 <= x <= 0.6743316650390625
     Peak relative error 8.0e-36  */
 TH =  3.333333333333333333333333333333333333333E-1Q,
 T0 = -1.813014711743583437742363284336855889393E7Q,
 T1 =  1.320767960008972224312740075083259247618E6Q,
 T2 = -2.626775478255838182468651821863299023956E4Q,
 T3 =  1.764573356488504935415411383687150199315E2Q,
 T4 = -3.333267763822178690794678978979803526092E-1Q,

 U0 = -1.359761033807687578306772463253710042010E8Q,
 U1 =  6.494370630656893175666729313065113194784E7Q,
 U2 = -4.180787672237927475505536849168729386782E6Q,
 U3 =  8.031643765106170040139966622980914621521E4Q,
 U4 = -5.323131271912475695157127875560667378597E2Q;
  /* 1.000000000000000000000000000000000000000E0 */


static __float128
__quadmath_kernel_tanq (__float128 x, __float128 y, int iy)
{
  __float128 z, r, v, w, s;
  int32_t ix, sign = 1;
  ieee854_float128 u, u1;

  u.value = x;
  ix = u.words32.w0 & 0x7fffffff;
  if (ix < 0x3fc60000)		/* x < 2**-57 */
    {
      if ((int) x == 0)
	{			/* generate inexact */
	  if ((ix | u.words32.w1 | u.words32.w2 | u.words32.w3
	       | (iy + 1)) == 0)
	    return one / fabsq (x);
	  else
	    return (iy == 1) ? x : -one / x;
	}
    }
  if (ix >= 0x3ffe5942) /* |x| >= 0.6743316650390625 */
    {
      if ((u.words32.w0 & 0x80000000) != 0)
	{
	  x = -x;
	  y = -y;
	  sign = -1;
	}
      else
	sign = 1;
      z = pio4hi - x;
      w = pio4lo - y;
      x = z + w;
      y = 0.0;
    }
  z = x * x;
  r = T0 + z * (T1 + z * (T2 + z * (T3 + z * T4)));
  v = U0 + z * (U1 + z * (U2 + z * (U3 + z * (U4 + z))));
  r = r / v;

  s = z * x;
  r = y + z * (s * r + y);
  r += TH * s;
  w = x + r;
  if (ix >= 0x3ffe5942)
    {
      v = (__float128) iy;
      w = (v - 2.0Q * (x - (w * w / (w + v) - r)));
      if (sign < 0)
	w = -w;
      return w;
    }
  if (iy == 1)
    return w;
  else
    {				/* if allow error up to 2 ulp,
				   simply return -1.0/(x+r) here */
      /*  compute -1.0/(x+r) accurately */
      u1.value = w;
      u1.words32.w2 = 0;
      u1.words32.w3 = 0;
      v = r - (u1.value - x);		/* u1+v = r+x */
      z = -1.0 / w;
      u.value = z;
      u.words32.w2 = 0;
      u.words32.w3 = 0;
      s = 1.0 + u.value * u1.value;
      return u.value + z * (s + u.value * v);
    }
}







/* s_tanl.c -- long double version of s_tan.c.
 * Conversion to IEEE quad long double by Jakub Jelinek, jj@ultra.linux.cz.
 */
  
/* @(#)s_tan.c 5.1 93/09/24 */
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

/* tanl(x)
 * Return tangent function of x.
 *
 * kernel function:
 *	__kernel_tanq		... tangent function on [-pi/4,pi/4]
 *	__ieee754_rem_pio2q	... argument reduction routine
 *
 * Method.
 *      Let S,C and T denote the sin, cos and tan respectively on
 *	[-PI/4, +PI/4]. Reduce the argument x to y1+y2 = x-k*pi/2
 *	in [-pi/4 , +pi/4], and let n = k mod 4.
 *	We have
 *
 *          n        sin(x)      cos(x)        tan(x)
 *     ----------------------------------------------------------
 *	    0	       S	   C		 T
 *	    1	       C	  -S		-1/T
 *	    2	      -S	  -C		 T
 *	    3	      -C	   S		-1/T
 *     ----------------------------------------------------------
 *
 * Special cases:
 *      Let trig be any of sin, cos, or tan.
 *      trig(+-INF)  is NaN, with signals;
 *      trig(NaN)    is that NaN;
 *
 * Accuracy:
 *	TRIG(x) returns trig(x) nearly rounded
 */


__float128
tanq (__float128 x)
{
	__float128 y[2],z=0.0Q;
	int64_t n, ix;

    /* High word of x. */
	GET_FLT128_MSW64(ix,x);

    /* |x| ~< pi/4 */
	ix &= 0x7fffffffffffffffLL;
	if(ix <= 0x3ffe921fb54442d1LL) return __quadmath_kernel_tanq(x,z,1);

    /* tanl(Inf or NaN) is NaN */
	else if (ix>=0x7fff000000000000LL) {
	    if (ix == 0x7fff000000000000LL) {
		GET_FLT128_LSW64(n,x);
	    }
	    return x-x;		/* NaN */
	}

    /* argument reduction needed */
	else {
	    n = __quadmath_rem_pio2q(x,y);
					/*   1 -- n even, -1 -- n odd */
	    return __quadmath_kernel_tanq(y[0],y[1],1-((n&1)<<1));
	}
}
