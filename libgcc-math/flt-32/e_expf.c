/* Single-precision floating point e^x.
   Copyright (C) 1997, 1998, 2005, 2006 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Geoffrey Keating <geoffk@ozemail.com.au>

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

/* How this works:

   The input value, x, is written as

   x = n * ln(2) + t/512 + delta[t] + x;

   where:
   - n is an integer, 127 >= n >= -150;
   - t is an integer, 177 >= t >= -177
   - delta is based on a table entry, delta[t] < 2^-28
   - x is whatever is left, |x| < 2^-10

   Then e^x is approximated as

   e^x = 2^n ( e^(t/512 + delta[t])
               + ( e^(t/512 + delta[t])
                   * ( p(x + delta[t] + n * ln(2)) - delta ) ) )

   where
   - p(x) is a polynomial approximating e(x)-1;
   - e^(t/512 + delta[t]) is obtained from a table.

   The table used is the same one as for the double precision version;
   since we have the table, we might as well use it.

   It turns out to be faster to do calculations in double precision than
   to perform an 'accurate table method' expf, because of the range reduction
   overhead (compare exp2f).
   */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <float.h>
#include <ieee754.h>
#include <math.h>
#include <inttypes.h>
#include <math_private.h>

extern const float __exp_deltatable[178];
extern const double __exp_atable[355] /* __attribute__((mode(DF))) */;

static const volatile float TWOM100 = 7.88860905e-31;
static const volatile float TWO127 = 1.7014118346e+38;

float
__ieee754_expf (float x)
{
  static const float himark = 88.72283935546875;
  static const float lomark = -103.972084045410;
  /* Check for usual case.  */
  if (isless (x, himark) && isgreater (x, lomark))
    {
      static const float THREEp42 = 13194139533312.0;
      static const float THREEp22 = 12582912.0;
      /* 1/ln(2).  */
#undef M_1_LN2
      static const float M_1_LN2 = 1.44269502163f;
      /* ln(2) */
#undef M_LN2
      static const double M_LN2 = .6931471805599452862;

      int tval;
      double x22, t, result, dx;
      float n, delta;
      union ieee754_double ex2_u;

      /* Calculate n.  */
      n = x * M_1_LN2 + THREEp22;
      n -= THREEp22;
      dx = x - n*M_LN2;

      /* Calculate t/512.  */
      t = dx + THREEp42;
      t -= THREEp42;
      dx -= t;

      /* Compute tval = t.  */
      tval = (int) (t * 512.0);

      if (t >= 0)
	delta = - __exp_deltatable[tval];
      else
	delta = __exp_deltatable[-tval];

      /* Compute ex2 = 2^n e^(t/512+delta[t]).  */
      ex2_u.d = __exp_atable[tval+177];
      ex2_u.ieee.exponent += (int) n;

      /* Approximate e^(dx+delta) - 1, using a second-degree polynomial,
	 with maximum error in [-2^-10-2^-28,2^-10+2^-28]
	 less than 5e-11.  */
      x22 = (0.5000000496709180453 * dx + 1.0000001192102037084) * dx + delta;

      /* Return result.  */
      result = x22 * ex2_u.d + ex2_u.d;
      return (float) result;
    }
  /* Exceptional cases:  */
  else if (isless (x, himark))
    {
      if (__isinff (x))
	/* e^-inf == 0, with no error.  */
	return 0;
      else
	/* Underflow */
	return TWOM100 * TWOM100;
    }
  else
    /* Return x, if x is a NaN or Inf; or overflow, otherwise.  */
    return TWO127*x;
}
