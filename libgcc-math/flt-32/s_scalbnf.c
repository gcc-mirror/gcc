/* s_scalbnf.c -- float version of s_scalbn.c.
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
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
static char rcsid[] = "$NetBSD: s_scalbnf.c,v 1.4 1995/05/10 20:48:10 jtc Exp $";
#endif

#include "math.h"
#include "math_private.h"

#ifdef __STDC__
static const float
#else
static float
#endif
two25   =  3.355443200e+07,	/* 0x4c000000 */
twom25  =  2.9802322388e-08,	/* 0x33000000 */
huge   = 1.0e+30,
tiny   = 1.0e-30;

#ifdef __STDC__
	float __scalbnf (float x, int n)
#else
	float __scalbnf (x,n)
	float x; int n;
#endif
{
	int32_t k,ix;
	GET_FLOAT_WORD(ix,x);
        k = (ix&0x7f800000)>>23;		/* extract exponent */
        if (k==0) {				/* 0 or subnormal x */
            if ((ix&0x7fffffff)==0) return x; /* +-0 */
	    x *= two25;
	    GET_FLOAT_WORD(ix,x);
	    k = ((ix&0x7f800000)>>23) - 25;
	    }
        if (k==0xff) return x+x;		/* NaN or Inf */
        k = k+n;
        if (n> 50000 || k >  0xfe)
	  return huge*__builtin_copysignf(huge,x); /* overflow  */
	if (n< -50000)
	  return tiny*__builtin_copysignf(tiny,x);	/*underflow*/
        if (k > 0) 				/* normal result */
	    {SET_FLOAT_WORD(x,(ix&0x807fffff)|(k<<23)); return x;}
        if (k <= -25)
	    return tiny*__builtin_copysignf(tiny,x);	/*underflow*/
        k += 25;				/* subnormal result */
	SET_FLOAT_WORD(x,(ix&0x807fffff)|(k<<23));
        return x*twom25;
}
weak_alias (__scalbnf, scalbnf)
