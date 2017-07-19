/* frexpq.c -- __float128 version of s_frexp.c.
 * Conversion to IEEE quad long double by Jakub Jelinek, jj@ultra.linux.cz.
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

/*
 * for non-zero x
 *	x = frexpq(arg,&exp);
 * return a __float128 fp quantity x such that 0.5 <= |x| <1.0
 * and the corresponding binary exponent "exp". That is
 *	arg = x*2^exp.
 * If arg is inf, 0.0, or NaN, then frexpq(arg,&exp) returns arg
 * with *exp=0.
 */

#include "quadmath-imp.h"

static const __float128
two114 = 2.0769187434139310514121985316880384E+34Q; /* 0x4071000000000000, 0 */

__float128
frexpq (__float128 x, int *eptr)
{
  uint64_t hx, lx, ix;
  GET_FLT128_WORDS64(hx,lx,x);
  ix = 0x7fffffffffffffffULL&hx;
  *eptr = 0;
  if(ix>=0x7fff000000000000ULL||((ix|lx)==0)) return x + x;/* 0,inf,nan */
  if (ix<0x0001000000000000ULL) {		/* subnormal */
    x *= two114;
    GET_FLT128_MSW64(hx,x);
    ix = hx&0x7fffffffffffffffULL;
    *eptr = -114;
  }
  *eptr += (ix>>48)-16382;
  hx = (hx&0x8000ffffffffffffULL) | 0x3ffe000000000000ULL;
  SET_FLT128_MSW64(x,hx);
  return x;
}
