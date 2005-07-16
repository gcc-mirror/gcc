

/* @(#)w_pow.c 5.2 93/10/01 */
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
	<<pow>>, <<powf>>---x to the power y
INDEX
	pow
INDEX
	powf


ANSI_SYNOPSIS
	#include <math.h>
	double pow(double <[x]>, double <[y]>);
	float pow(float <[x]>, float <[y]>);

TRAD_SYNOPSIS
	#include <math.h>
	double pow(<[x]>, <[y]>);
	double <[x]>, <[y]>;

	float pow(<[x]>, <[y]>);
	float <[x]>, <[y]>;

DESCRIPTION
	<<pow>> and <<powf>> calculate <[x]> raised to the exp1.0nt <[y]>.
	@tex
	(That is, $x^y$.)
	@end tex

RETURNS
	On success, <<pow>> and <<powf>> return the value calculated.

	When the argument values would produce overflow, <<pow>>
	returns <<HUGE_VAL>> and set <<errno>> to <<ERANGE>>.  If the
	argument <[x]> passed to <<pow>> or <<powf>> is a negative
	noninteger, and <[y]> is also not an integer, then <<errno>>
	is set to <<EDOM>>.  If <[x]> and <[y]> are both 0, then
	<<pow>> and <<powf>> return <<1>>.

	You can modify error handling for these functions using <<matherr>>.

PORTABILITY
	<<pow>> is ANSI C. <<powf>> is an extension.  */

/* 
 * wrapper pow(x,y) return x**y
 */

#include "fdlibm.h"
#include <errno.h>

#ifndef _DOUBLE_IS_32BITS

#ifdef __STDC__
	double pow(double x, double y)	/* wrapper pow */
#else
	double pow(x,y)			/* wrapper pow */
	double x,y;
#endif
{
#ifdef _IEEE_LIBM
	return  __ieee754_pow(x,y);
#else
	double z;
#ifndef HUGE_VAL 
#define HUGE_VAL inf
	double inf = 0.0;

	SET_HIGH_WORD(inf,0x7ff00000);	/* set inf to infinite */
#endif
	struct exception exc;
	z=__ieee754_pow(x,y);
	if(_LIB_VERSION == _IEEE_|| isnan(y)) return z;
	if(isnan(x)) {
	    if(y==0.0) { 
		/* pow(NaN,0.0) */
		/* error only if _LIB_VERSION == _SVID_ & _XOPEN_ */
		exc.type = DOMAIN;
		exc.name = "pow";
		exc.err = 0;
		exc.arg1 = x;
		exc.arg2 = y;
		exc.retval = x;
		if (_LIB_VERSION == _IEEE_ ||
		    _LIB_VERSION == _POSIX_) exc.retval = 1.0;
		else if (!matherr(&exc)) {
			errno = EDOM;
		}
	        if (exc.err != 0)
	           errno = exc.err;
                return exc.retval; 
	    } else 
		return z;
	}
	if(x==0.0){ 
	    if(y==0.0) {
		/* pow(0.0,0.0) */
		/* error only if _LIB_VERSION == _SVID_ */
		exc.type = DOMAIN;
		exc.name = "pow";
		exc.err = 0;
		exc.arg1 = x;
		exc.arg2 = y;
		exc.retval = 0.0;
		if (_LIB_VERSION != _SVID_) exc.retval = 1.0;
		else if (!matherr(&exc)) {
			errno = EDOM;
		}
	        if (exc.err != 0)
	           errno = exc.err;
                return exc.retval; 
	    }
            if(finite(y)&&y<0.0) {
		/* 0**neg */
		exc.type = DOMAIN;
		exc.name = "pow";
		exc.err = 0;
		exc.arg1 = x;
		exc.arg2 = y;
		if (_LIB_VERSION == _SVID_) 
		  exc.retval = 0.0;
		else
		  exc.retval = -HUGE_VAL;
		if (_LIB_VERSION == _POSIX_)
		  errno = EDOM;
		else if (!matherr(&exc)) {
		  errno = EDOM;
		}
	        if (exc.err != 0)
	           errno = exc.err;
                return exc.retval; 
            } 
	    return z;
	}
	if(!finite(z)) {
	    if(finite(x)&&finite(y)) {
	        if(isnan(z)) {
		    /* neg**non-integral */
		    exc.type = DOMAIN;
		    exc.name = "pow";
		    exc.err = 0;
		    exc.arg1 = x;
		    exc.arg2 = y;
		    if (_LIB_VERSION == _SVID_) 
		        exc.retval = 0.0;
		    else 
		        exc.retval = 0.0/0.0;	/* X/Open allow NaN */
		    if (_LIB_VERSION == _POSIX_) 
		        errno = EDOM;
		    else if (!matherr(&exc)) {
		        errno = EDOM;
		    }
	            if (exc.err != 0)
	                errno = exc.err;
                    return exc.retval; 
	        } else {
		    /* pow(x,y) overflow */
		    exc.type = OVERFLOW;
		    exc.name = "pow";
		    exc.err = 0;
		    exc.arg1 = x;
		    exc.arg2 = y;
		    if (_LIB_VERSION == _SVID_) {
		       exc.retval = HUGE;
		       y *= 0.5;
		       if(x<0.0&&rint(y)!=y) exc.retval = -HUGE;
		    } else {
		       exc.retval = HUGE_VAL;
                       y *= 0.5;
		       if(x<0.0&&rint(y)!=y) exc.retval = -HUGE_VAL;
		    }
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
	} 
	if(z==0.0&&finite(x)&&finite(y)) {
	    /* pow(x,y) underflow */
	    exc.type = UNDERFLOW;
	    exc.name = "pow";
	    exc.err = 0;
	    exc.arg1 = x;
	    exc.arg2 = y;
	    exc.retval =  0.0;
	    if (_LIB_VERSION == _POSIX_)
	        errno = ERANGE;
	    else if (!matherr(&exc)) {
	     	errno = ERANGE;
	    }
	    if (exc.err != 0)
	        errno = exc.err;
            return exc.retval; 
        } 
	return z;
#endif
}

#endif /* defined(_DOUBLE_IS_32BITS) */














