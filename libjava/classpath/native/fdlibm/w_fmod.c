
/* @(#)w_fmod.c 5.1 93/09/24 */
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
<<fmod>>, <<fmodf>>---floating-point remainder (modulo)

INDEX
fmod
INDEX
fmodf

ANSI_SYNOPSIS
#include <math.h>
double fmod(double <[x]>, double <[y]>)
float fmodf(float <[x]>, float <[y]>)

TRAD_SYNOPSIS
#include <math.h>
double fmod(<[x]>, <[y]>)
double (<[x]>, <[y]>);

float fmodf(<[x]>, <[y]>)
float (<[x]>, <[y]>);

DESCRIPTION
The <<fmod>> and <<fmodf>> functions compute the floating-point
remainder of <[x]>/<[y]> (<[x]> modulo <[y]>).

RETURNS
The <<fmod>> function returns the value 
@ifinfo
<[x]>-<[i]>*<[y]>, 
@end ifinfo
@tex
$x-i\times y$,
@end tex
for the largest integer <[i]> such that, if <[y]> is nonzero, the
result has the same sign as <[x]> and magnitude less than the
magnitude of <[y]>. 

<<fmod(<[x]>,0)>> returns NaN, and sets <<errno>> to <<EDOM>>.

You can modify error treatment for these functions using <<matherr>>.

PORTABILITY
<<fmod>> is ANSI C. <<fmodf>> is an extension.
*/

/* 
 * wrapper fmod(x,y)
 */

#include "fdlibm.h"
#include <errno.h>

#ifndef _DOUBLE_IS_32BITS

#ifdef __STDC__
	double fmod(double x, double y)	/* wrapper fmod */
#else
	double fmod(x,y)		/* wrapper fmod */
	double x,y;
#endif
{
#ifdef _IEEE_LIBM
	return __ieee754_fmod(x,y);
#else
	double z;
	struct exception exc;
	z = __ieee754_fmod(x,y);
	if(_LIB_VERSION == _IEEE_ ||isnan(y)||isnan(x)) return z;
	if(y==0.0) {
            /* fmod(x,0) */
            exc.type = DOMAIN;
            exc.name = "fmod";
	    exc.arg1 = x;
	    exc.arg2 = y;
	    exc.err = 0;
            if (_LIB_VERSION == _SVID_)
               exc.retval = x;
	    else
	       exc.retval = 0.0/0.0;
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
