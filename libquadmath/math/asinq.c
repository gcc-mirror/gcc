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
  __float128 expansions are
  Copyright (C) 2001 Stephen L. Moshier <moshier@na-net.ornl.gov>
  and are incorporated herein by permission of the author.  The author 
  reserves the right to distribute this material elsewhere under different 
  copying permissions.  These modifications are distributed here under the 
  following terms:

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

/* __ieee754_asin(x)
 * Method :
 *	Since  asin(x) = x + x^3/6 + x^5*3/40 + x^7*15/336 + ...
 *	we approximate asin(x) on [0,0.5] by
 *		asin(x) = x + x*x^2*R(x^2)
 *      Between .5 and .625 the approximation is
 *              asin(0.5625 + x) = asin(0.5625) + x rS(x) / sS(x)
 *	For x in [0.625,1]
 *		asin(x) = pi/2-2*asin(sqrt((1-x)/2))
 *	Let y = (1-x), z = y/2, s := sqrt(z), and pio2_hi+pio2_lo=pi/2;
 *	then for x>0.98
 *		asin(x) = pi/2 - 2*(s+s*z*R(z))
 *			= pio2_hi - (2*(s+s*z*R(z)) - pio2_lo)
 *	For x<=0.98, let pio4_hi = pio2_hi/2, then
 *		f = hi part of s;
 *		c = sqrt(z) - f = (z-f*f)/(s+f) 	...f+c=sqrt(z)
 *	and
 *		asin(x) = pi/2 - 2*(s+s*z*R(z))
 *			= pio4_hi+(pio4-2s)-(2s*z*R(z)-pio2_lo)
 *			= pio4_hi+(pio4-2f)-(2s*z*R(z)-(pio2_lo+2c))
 *
 * Special cases:
 *	if x is NaN, return x itself;
 *	if |x|>1, return NaN with invalid signal.
 *
 */


#include "quadmath-imp.h"

static const __float128
  one = 1.0Q,
  huge = 1.0e+4932Q,
  pio2_hi = 1.5707963267948966192313216916397514420986Q,
  pio2_lo = 4.3359050650618905123985220130216759843812E-35Q,
  pio4_hi = 7.8539816339744830961566084581987569936977E-1Q,

	/* coefficient for R(x^2) */

  /* asin(x) = x + x^3 pS(x^2) / qS(x^2)
     0 <= x <= 0.5
     peak relative error 1.9e-35  */
  pS0 = -8.358099012470680544198472400254596543711E2Q,
  pS1 =  3.674973957689619490312782828051860366493E3Q,
  pS2 = -6.730729094812979665807581609853656623219E3Q,
  pS3 =  6.643843795209060298375552684423454077633E3Q,
  pS4 = -3.817341990928606692235481812252049415993E3Q,
  pS5 =  1.284635388402653715636722822195716476156E3Q,
  pS6 = -2.410736125231549204856567737329112037867E2Q,
  pS7 =  2.219191969382402856557594215833622156220E1Q,
  pS8 = -7.249056260830627156600112195061001036533E-1Q,
  pS9 =  1.055923570937755300061509030361395604448E-3Q,

  qS0 = -5.014859407482408326519083440151745519205E3Q,
  qS1 =  2.430653047950480068881028451580393430537E4Q,
  qS2 = -4.997904737193653607449250593976069726962E4Q,
  qS3 =  5.675712336110456923807959930107347511086E4Q,
  qS4 = -3.881523118339661268482937768522572588022E4Q,
  qS5 =  1.634202194895541569749717032234510811216E4Q,
  qS6 = -4.151452662440709301601820849901296953752E3Q,
  qS7 =  5.956050864057192019085175976175695342168E2Q,
  qS8 = -4.175375777334867025769346564600396877176E1Q,
  /* 1.000000000000000000000000000000000000000E0 */

  /* asin(0.5625 + x) = asin(0.5625) + x rS(x) / sS(x)
     -0.0625 <= x <= 0.0625
     peak relative error 3.3e-35  */
  rS0 = -5.619049346208901520945464704848780243887E0Q,
  rS1 =  4.460504162777731472539175700169871920352E1Q,
  rS2 = -1.317669505315409261479577040530751477488E2Q,
  rS3 =  1.626532582423661989632442410808596009227E2Q,
  rS4 = -3.144806644195158614904369445440583873264E1Q,
  rS5 = -9.806674443470740708765165604769099559553E1Q,
  rS6 =  5.708468492052010816555762842394927806920E1Q,
  rS7 =  1.396540499232262112248553357962639431922E1Q,
  rS8 = -1.126243289311910363001762058295832610344E1Q,
  rS9 = -4.956179821329901954211277873774472383512E-1Q,
  rS10 =  3.313227657082367169241333738391762525780E-1Q,

  sS0 = -4.645814742084009935700221277307007679325E0Q,
  sS1 =  3.879074822457694323970438316317961918430E1Q,
  sS2 = -1.221986588013474694623973554726201001066E2Q,
  sS3 =  1.658821150347718105012079876756201905822E2Q,
  sS4 = -4.804379630977558197953176474426239748977E1Q,
  sS5 = -1.004296417397316948114344573811562952793E2Q,
  sS6 =  7.530281592861320234941101403870010111138E1Q,
  sS7 =  1.270735595411673647119592092304357226607E1Q,
  sS8 = -1.815144839646376500705105967064792930282E1Q,
  sS9 = -7.821597334910963922204235247786840828217E-2Q,
  /*  1.000000000000000000000000000000000000000E0 */

 asinr5625 =  5.9740641664535021430381036628424864397707E-1Q;



__float128
asinq (__float128 x)
{
  __float128 t = 0;
  __float128 w, p, q, c, r, s;
  int32_t ix, sign, flag;
  ieee854_float128 u;

  flag = 0;
  u.value = x;
  sign = u.words32.w0;
  ix = sign & 0x7fffffff;
  u.words32.w0 = ix;    /* |x| */
  if (ix >= 0x3fff0000)	/* |x|>= 1 */
    {
      if (ix == 0x3fff0000
	  && (u.words32.w1 | u.words32.w2 | u.words32.w3) == 0)
	/* asin(1)=+-pi/2 with inexact */
	return x * pio2_hi + x * pio2_lo;
      return (x - x) / (x - x);	/* asin(|x|>1) is NaN */
    }
  else if (ix < 0x3ffe0000) /* |x| < 0.5 */
    {
      if (ix < 0x3fc60000) /* |x| < 2**-57 */
	{
	  if (huge + x > one)
	    return x;		/* return x with inexact if x!=0 */
	}
      else
	{
	  t = x * x;
	  /* Mark to use pS, qS later on.  */
	  flag = 1;
	}
    }
  else if (ix < 0x3ffe4000) /* 0.625 */
    {
      t = u.value - 0.5625;
      p = ((((((((((rS10 * t
		    + rS9) * t
		   + rS8) * t
		  + rS7) * t
		 + rS6) * t
		+ rS5) * t
	       + rS4) * t
	      + rS3) * t
	     + rS2) * t
	    + rS1) * t
	   + rS0) * t;

      q = ((((((((( t
		    + sS9) * t
		  + sS8) * t
		 + sS7) * t
		+ sS6) * t
	       + sS5) * t
	      + sS4) * t
	     + sS3) * t
	    + sS2) * t
	   + sS1) * t
	+ sS0;
      t = asinr5625 + p / q;
      if ((sign & 0x80000000) == 0)
	return t;
      else
	return -t;
    }
  else
    {
      /* 1 > |x| >= 0.625 */
      w = one - u.value;
      t = w * 0.5;
    }

  p = (((((((((pS9 * t
	       + pS8) * t
	      + pS7) * t
	     + pS6) * t
	    + pS5) * t
	   + pS4) * t
	  + pS3) * t
	 + pS2) * t
	+ pS1) * t
       + pS0) * t;

  q = (((((((( t
	      + qS8) * t
	     + qS7) * t
	    + qS6) * t
	   + qS5) * t
	  + qS4) * t
	 + qS3) * t
	+ qS2) * t
       + qS1) * t
    + qS0;

  if (flag) /* 2^-57 < |x| < 0.5 */
    {
      w = p / q;
      return x + x * w;
    }

  s = sqrtq (t);
  if (ix >= 0x3ffef333) /* |x| > 0.975 */
    {
      w = p / q;
      t = pio2_hi - (2.0 * (s + s * w) - pio2_lo);
    }
  else
    {
      u.value = s;
      u.words32.w3 = 0;
      u.words32.w2 = 0;
      w = u.value;
      c = (t - w * w) / (s + w);
      r = p / q;
      p = 2.0 * s * r - (pio2_lo - 2.0 * c);
      q = pio4_hi - 2.0 * w;
      t = pio4_hi - (p - q);
    }

  if ((sign & 0x80000000) == 0)
    return t;
  else
    return -t;
}
