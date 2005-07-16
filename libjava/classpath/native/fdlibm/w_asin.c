
/* @(#)w_asin.c 5.1 93/09/24 */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 *
 */

/*
FUNCTION
        <<asin>>, <<asinf>>---arc sine

INDEX
   asin
INDEX
   asinf

ANSI_SYNOPSIS
        #include <math.h>
        double asin(double <[x]>);
        float asinf(float <[x]>);

TRAD_SYNOPSIS
        #include <math.h>
        double asin(<[x]>)
        double <[x]>;

        float asinf(<[x]>)
        float <[x]>;


DESCRIPTION

<<asin>> computes the inverse sine (arc sine) of the argument <[x]>.
Arguments to <<asin>> must be in the range @minus{}1 to 1.

<<asinf>> is identical to <<asin>>, other than taking and
returning floats.

You can modify error handling for these routines using <<matherr>>. 

RETURNS
@ifinfo
<<asin>> returns values in radians, in the range of -pi/2 to pi/2.
@end ifinfo
@tex
<<asin>> returns values in radians, in the range of $-\pi/2$ to $\pi/2$.
@end tex

If <[x]> is not in the range @minus{}1 to 1, <<asin>> and <<asinf>>
return NaN (not a number), set the global variable <<errno>> to
<<EDOM>>, and issue a <<DOMAIN error>> message.

You can change this error treatment using <<matherr>>.

QUICKREF ANSI SVID POSIX RENTRANT
 asin	 y,y,y,m
 asinf   n,n,n,m

MATHREF  
 asin,  -1<=arg<=1, asin(arg),,,
 asin,  NAN,  arg,EDOM, DOMAIN

MATHREF  
 asinf,  -1<=arg<=1, asin(arg),,,
 asinf,  NAN,  arg,EDOM, DOMAIN 


*/

/* 
 * wrapper asin(x)
 */


#include "fdlibm.h"
#include <errno.h>

#ifndef _DOUBLE_IS_32BITS

#ifdef __STDC__
	double asin(double x)		/* wrapper asin */
#else
	double asin(x)			/* wrapper asin */
	double x;
#endif
{
#ifdef _IEEE_LIBM
	return __ieee754_asin(x);
#else
	double z;
	struct exception exc;
	z = __ieee754_asin(x);
	if(_LIB_VERSION == _IEEE_ || isnan(x)) return z;
	if(fabs(x)>1.0) {
	    /* asin(|x|>1) */
	    exc.type = DOMAIN;
	    exc.name = "asin";
	    exc.err = 0;
	    exc.arg1 = exc.arg2 = x;
	    exc.retval = 0.0;
	    if(_LIB_VERSION == _POSIX_)
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
