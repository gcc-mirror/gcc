/* scalbnq.c -- __float128 version of s_scalbn.c.
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
 * scalbnq (__float128 x, int n)
 * scalbnq(x,n) returns x* 2**n  computed by  exponent
 * manipulation rather than by actually performing an
 * exponentiation or a multiplication.
 */

#include "quadmath-imp.h"

static const __float128
two114 = 2.0769187434139310514121985316880384E+34Q, /* 0x4071000000000000, 0 */
twom114 = 4.8148248609680896326399448564623183E-35Q, /* 0x3F8D000000000000, 0 */
huge   = 1.0E+4900Q,
tiny   = 1.0E-4900Q;

__float128
scalbnq (__float128 x, int n)
{
	int64_t k,hx,lx;
	GET_FLT128_WORDS64(hx,lx,x);
        k = (hx>>48)&0x7fff;		/* extract exponent */
        if (k==0) {				/* 0 or subnormal x */
            if ((lx|(hx&0x7fffffffffffffffULL))==0) return x; /* +-0 */
	    x *= two114;
	    GET_FLT128_MSW64(hx,x);
	    k = ((hx>>48)&0x7fff) - 114;
	}
        if (k==0x7fff) return x+x;		/* NaN or Inf */
	if (n< -50000) return tiny*copysignq(tiny,x); /*underflow*/
        if (n> 50000 || k+n > 0x7ffe)
	  return huge*copysignq(huge,x); /* overflow  */
	/* Now k and n are bounded we know that k = k+n does not
	   overflow.  */
        k = k+n;
        if (k > 0) 				/* normal result */
	    {SET_FLT128_MSW64(x,(hx&0x8000ffffffffffffULL)|(k<<48)); return x;}
        if (k <= -114)
	  return tiny*copysignq(tiny,x); 	/*underflow*/
        k += 114;				/* subnormal result */
	SET_FLT128_MSW64(x,(hx&0x8000ffffffffffffULL)|(k<<48));
        return x*twom114;
}
