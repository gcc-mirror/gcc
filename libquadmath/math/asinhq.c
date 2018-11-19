/* s_asinhl.c -- long double version of s_asinh.c.
 * Conversion to long double by Ulrich Drepper,
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

#if defined(LIBM_SCCS) && !defined(lint)
static char rcsid[] = "$NetBSD: $";
#endif

/* asinhq(x)
 * Method :
 *      Based on
 *              asinhq(x) = signl(x) * logq [ |x| + sqrtq(x*x+1) ]
 *      we have
 *      asinhq(x) := x  if  1+x*x=1,
 *                := signl(x)*(logq(x)+ln2)) for large |x|, else
 *                := signl(x)*logq(2|x|+1/(|x|+sqrtq(x*x+1))) if|x|>2, else
 *                := signl(x)*log1pq(|x| + x^2/(1 + sqrtq(1+x^2)))
 */

#include "quadmath-imp.h"

static const __float128
  one = 1,
  ln2 = 6.931471805599453094172321214581765681e-1Q,
  huge = 1.0e+4900Q;

__float128
asinhq (__float128 x)
{
  __float128 t, w;
  int32_t ix, sign;
  ieee854_float128 u;

  u.value = x;
  sign = u.words32.w0;
  ix = sign & 0x7fffffff;
  if (ix == 0x7fff0000)
    return x + x;		/* x is inf or NaN */
  if (ix < 0x3fc70000)
    {				/* |x| < 2^ -56 */
      math_check_force_underflow (x);
      if (huge + x > one)
	return x;		/* return x inexact except 0 */
    }
  u.words32.w0 = ix;
  if (ix > 0x40350000)
    {				/* |x| > 2 ^ 54 */
      w = logq (u.value) + ln2;
    }
  else if (ix >0x40000000)
    {				/* 2^ 54 > |x| > 2.0 */
      t = u.value;
      w = logq (2.0 * t + one / (sqrtq (x * x + one) + t));
    }
  else
    {				/* 2.0 > |x| > 2 ^ -56 */
      t = x * x;
      w = log1pq (u.value + t / (one + sqrtq (one + t)));
    }
  if (sign & 0x80000000)
    return -w;
  else
    return w;
}
