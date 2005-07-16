
/* @(#)s_scalbn.c 5.1 93/09/24 */
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
FUNCTION
<<scalbn>>, <<scalbnf>>---scale by integer
INDEX
	scalbn
INDEX
	scalbnf

ANSI_SYNOPSIS
	#include <math.h>
	double scalbn(double <[x]>, int <[y]>);
	float scalbnf(float <[x]>, int <[y]>);

TRAD_SYNOPSIS
	#include <math.h>
	double scalbn(<[x]>,<[y]>)
	double <[x]>;
	int <[y]>;
	float scalbnf(<[x]>,<[y]>)
	float <[x]>;
	int <[y]>;

DESCRIPTION
<<scalbn>> and <<scalbnf>> scale <[x]> by <[n]>, returning <[x]> times
2 to the power <[n]>.  The result is computed by manipulating the
exponent, rather than by actually performing an exponentiation or
multiplication.

RETURNS
<[x]> times 2 to the power <[n]>.

PORTABILITY
Neither <<scalbn>> nor <<scalbnf>> is required by ANSI C or by the System V
Interface Definition (Issue 2).

*/

/*
 * scalbn (double x, int n)
 * scalbn(x,n) returns x* 2**n  computed by  exponent
 * manipulation rather than by actually performing an
 * exponentiation or a multiplication.
 */

#include "fdlibm.h"

#ifndef _DOUBLE_IS_32BITS

#ifdef __STDC__
static const double
#else
static double
#endif
two54   =  1.80143985094819840000e+16, /* 0x43500000, 0x00000000 */
twom54  =  5.55111512312578270212e-17, /* 0x3C900000, 0x00000000 */
huge   = 1.0e+300,
tiny   = 1.0e-300;

#ifdef __STDC__
	double scalbn (double x, int n)
#else
	double scalbn (x,n)
	double x; int n;
#endif
{
	int32_t  k,hx,lx;
	EXTRACT_WORDS(hx,lx,x);
        k = (hx&0x7ff00000)>>20;		/* extract exponent */
        if (k==0) {				/* 0 or subnormal x */
            if ((lx|(hx&0x7fffffff))==0) return x; /* +-0 */
	    x *= two54;
	    GET_HIGH_WORD(hx,x);
	    k = ((hx&0x7ff00000)>>20) - 54;
            if (n< -50000) return tiny*x; 	/*underflow*/
	    }
        if (k==0x7ff) return x+x;		/* NaN or Inf */
        k = k+n;
        if (k >  0x7fe) return huge*copysign(huge,x); /* overflow  */
        if (k > 0) 				/* normal result */
	    {SET_HIGH_WORD(x,(hx&0x800fffff)|(k<<20)); return x;}
        if (k <= -54) {
            if (n > 50000) 	/* in case integer overflow in n+k */
		return huge*copysign(huge,x);	/*overflow*/
	    else return tiny*copysign(tiny,x); 	/*underflow*/
	}
        k += 54;				/* subnormal result */
	SET_HIGH_WORD(x,(hx&0x800fffff)|(k<<20));
        return x*twom54;
}

#endif /* _DOUBLE_IS_32BITS */
