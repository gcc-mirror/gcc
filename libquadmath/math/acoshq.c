/* e_acoshl.c -- long double version of e_acosh.c.
 * Conversion to long double by Jakub Jelinek, jj@ultra.linux.cz.
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

/* acoshq(x)
 * Method :
 *	Based on
 *		acoshl(x) = logq [ x + sqrtq(x*x-1) ]
 *	we have
 *		acoshl(x) := logq(x)+ln2,	if x is large; else
 *		acoshl(x) := logq(2x-1/(sqrtq(x*x-1)+x)) if x>2; else
 *		acoshl(x) := log1pq(t+sqrtq(2.0*t+t*t)); where t=x-1.
 *
 * Special cases:
 *	acoshl(x) is NaN with signal if x<1.
 *	acoshl(NaN) is NaN without signal.
 */

#include "quadmath-imp.h"

static const __float128
one	= 1.0,
ln2	= 0.6931471805599453094172321214581766Q;

__float128
acoshq(__float128 x)
{
	__float128 t;
	uint64_t lx;
	int64_t hx;
	GET_FLT128_WORDS64(hx,lx,x);
	if(hx<0x3fff000000000000LL) {		/* x < 1 */
	    return (x-x)/(x-x);
	} else if(hx >=0x4035000000000000LL) {	/* x > 2**54 */
	    if(hx >=0x7fff000000000000LL) {	/* x is inf of NaN */
		return x+x;
	    } else
		return logq(x)+ln2;	/* acoshl(huge)=logq(2x) */
	} else if(((hx-0x3fff000000000000LL)|lx)==0) {
	    return 0;			/* acosh(1) = 0 */
	} else if (hx > 0x4000000000000000LL) {	/* 2**28 > x > 2 */
	    t=x*x;
	    return logq(2*x-one/(x+sqrtq(t-one)));
	} else {			/* 1<x<2 */
	    t = x-one;
	    return log1pq(t+sqrtq(2*t+t*t));
	}
}
