
/* @(#)w_exp.c 5.1 93/09/24 */
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
	<<exp>>, <<expf>>---exponential
INDEX
	exp
INDEX
	expf

ANSI_SYNOPSIS
	#include <math.h>
	double exp(double <[x]>);
	float expf(float <[x]>);

TRAD_SYNOPSIS
	#include <math.h>
	double exp(<[x]>);
	double <[x]>;

	float expf(<[x]>);
	float <[x]>;

DESCRIPTION
	<<exp>> and <<expf>> calculate the exponential of <[x]>, that is, 
	@ifinfo
	e raised to the power <[x]> (where e
	@end ifinfo
	@tex
	$e^x$ (where $e$
	@end tex
	is the base of the natural system of logarithms, approximately 2.71828).

	You can use the (non-ANSI) function <<matherr>> to specify
	error handling for these functions.

RETURNS
	On success, <<exp>> and <<expf>> return the calculated value.
	If the result underflows, the returned value is <<0>>.  If the
	result overflows, the returned value is <<HUGE_VAL>>.  In
	either case, <<errno>> is set to <<ERANGE>>.

PORTABILITY
	<<exp>> is ANSI C.  <<expf>> is an extension.

*/

/* 
 * wrapper exp(x)
 */

#include "fdlibm.h"
#include <errno.h>

#ifndef _DOUBLE_IS_32BITS

#ifndef _IEEE_LIBM

#ifdef __STDC__
static const double
#else
static double
#endif
o_threshold=  7.09782712893383973096e+02,  /* 0x40862E42, 0xFEFA39EF */
u_threshold= -7.45133219101941108420e+02;  /* 0xc0874910, 0xD52D3051 */

#endif

#ifdef __STDC__
	double exp(double x)		/* wrapper exp */
#else
	double exp(x)			/* wrapper exp */
	double x;
#endif
{
#ifdef _IEEE_LIBM
	return __ieee754_exp(x);
#else
	double z;
	struct exception exc;
	z = __ieee754_exp(x);
	if(_LIB_VERSION == _IEEE_) return z;
	if(finite(x)) {
	    if(x>o_threshold) {
		/* exp(finite) overflow */
#ifndef HUGE_VAL
#define HUGE_VAL inf
	        double inf = 0.0;

	        SET_HIGH_WORD(inf,0x7ff00000);	/* set inf to infinite */
#endif
		exc.type = OVERFLOW;
		exc.name = "exp";
		exc.err = 0;
		exc.arg1 = exc.arg2 = x;
		if (_LIB_VERSION == _SVID_)
		  exc.retval = HUGE;
		else
		  exc.retval = HUGE_VAL;
		if (_LIB_VERSION == _POSIX_)
		  errno = ERANGE;
		else if (!matherr(&exc)) {
			errno = ERANGE;
		}
	        if (exc.err != 0)
	           errno = exc.err;
	        return exc.retval; 
	    } else if(x<u_threshold) {
		/* exp(finite) underflow */
		exc.type = UNDERFLOW;
		exc.name = "exp";
		exc.err = 0;
		exc.arg1 = exc.arg2 = x;
		exc.retval = 0.0;
		if (_LIB_VERSION == _POSIX_)
		  errno = ERANGE;
		else if (!matherr(&exc)) {
			errno = ERANGE;
		}
	        if (exc.err != 0)
	           errno = exc.err;
	        return exc.retval; 
	    } 
	} 
	return z;
#endif
}

#endif /* defined(_DOUBLE_IS_32BITS) */
