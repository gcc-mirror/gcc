
/* @(#)w_remainder.c 5.1 93/09/24 */
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
<<rint>>, <<rintf>>, <<remainder>>, <<remainderf>>---round and  remainder
INDEX
	rint
INDEX
	rintf
INDEX
	remainder
INDEX
	remainderf

ANSI_SYNOPSIS
	#include <math.h>
	double rint(double <[x]>);
	float rintf(float <[x]>);
	double remainder(double <[x]>, double <[y]>);
	float remainderf(float <[x]>, float <[y]>);

TRAD_SYNOPSIS
	#include <math.h>
	double rint(<[x]>)
	double <[x]>;
	float rintf(<[x]>)
	float <[x]>;
	double remainder(<[x]>,<[y]>)
	double <[x]>, <[y]>;
	float remainderf(<[x]>,<[y]>)
	float <[x]>, <[y]>;

DESCRIPTION
<<rint>> and <<rintf>> returns their argument rounded to the nearest
integer.  <<remainder>> and <<remainderf>> find the remainder of
<[x]>/<[y]>; this value is in the range -<[y]>/2 .. +<[y]>/2.

RETURNS
<<rint>> and <<remainder>> return the integer result as a double.

PORTABILITY
<<rint>> and <<remainder>> are System V release 4.  <<rintf>> and
<<remainderf>> are extensions.

*/

/* 
 * wrapper remainder(x,p)
 */

#include "fdlibm.h"
#include <errno.h>

#ifndef _DOUBLE_IS_32BITS

#ifdef __STDC__
	double remainder(double x, double y)	/* wrapper remainder */
#else
	double remainder(x,y)			/* wrapper remainder */
	double x,y;
#endif
{
#ifdef _IEEE_LIBM
	return __ieee754_remainder(x,y);
#else
	double z;
	struct exception exc;
	z = __ieee754_remainder(x,y);
	if(_LIB_VERSION == _IEEE_ || isnan(y)) return z;
	if(y==0.0) { 
            /* remainder(x,0) */
            exc.type = DOMAIN;
            exc.name = "remainder";
	    exc.err = 0;
	    exc.arg1 = x;
	    exc.arg2 = y;
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

















