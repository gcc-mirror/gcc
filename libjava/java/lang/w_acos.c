
/* @(#)w_acos.c 5.1 93/09/24 */
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
        <<acos>>, <<acosf>>---arc cosine

INDEX
	acos
INDEX
	acosf

ANSI_SYNOPSIS
        #include <math.h>
        double acos(double <[x]>);
        float acosf(float <[x]>);

TRAD_SYNOPSIS
        #include <math.h>
        double acos(<[x]>)
        double <[x]>;

        float acosf(<[x]>)
        float <[x]>;



DESCRIPTION

	<<acos>> computes the inverse cosine (arc cosine) of the input value.
	Arguments to <<acos>> must be in the range @minus{}1 to 1. 

	<<acosf>> is identical to <<acos>>, except that it performs
	its calculations on <<floats>>.

RETURNS
	@ifinfo
	<<acos>> and <<acosf>> return values in radians, in the range of 0 to pi.
	@end ifinfo
	@tex
	<<acos>> and <<acosf>> return values in radians, in the range of <<0>> to $\pi$.
	@end tex

	If <[x]> is not between @minus{}1 and 1, the returned value is NaN
	(not a number) the global variable <<errno>> is set to <<EDOM>>, and a
	<<DOMAIN error>> message is sent as standard error output.

	You can modify error handling for these functions using <<matherr>>.


QUICKREF ANSI SVID POSIX RENTRANT
 acos	 y,y,y,m
 acosf   n,n,n,m

MATHREF  
 acos, [-1,1], acos(arg),,,
 acos, NAN,    arg,DOMAIN,EDOM

MATHREF
 acosf, [-1,1], acosf(arg),,,
 acosf, NAN,    argf,DOMAIN,EDOM
 
*/

/*
 * wrap_acos(x)
 */

#include "fdlibm.h"
#include <errno.h>

#ifndef _DOUBLE_IS_32BITS

#ifdef __STDC__
	double acos(double x)		/* wrapper acos */
#else
	double acos(x)			/* wrapper acos */
	double x;
#endif
{
#ifdef _IEEE_LIBM
	return __ieee754_acos(x);
#else
	double z;
       	struct exception exc;
       	z = __ieee754_acos(x);
	if(_LIB_VERSION == _IEEE_ || isnan(x)) return z;
	if(fabs(x)>1.0) { 
	    /* acos(|x|>1) */
	    exc.type = DOMAIN;
	    exc.name = "acos";
	    exc.err = 0;
	    exc.arg1 = exc.arg2 = x;
	    exc.retval = 0.0;
	    if (_LIB_VERSION == _POSIX_)
	       errno = EDOM;
	    else if (!matherr(&exc)) {
	       errno = EDOM;
            }
            if (exc.err != 0)
	       errno = exc.err;
	    return exc.retval; 
	} else
	    return z;
#endif
}

#endif /* defined(_DOUBLE_IS_32BITS) */
