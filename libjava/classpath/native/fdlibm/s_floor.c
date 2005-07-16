
/* @(#)s_floor.c 5.1 93/09/24 */
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
<<floor>>, <<floorf>>, <<ceil>>, <<ceilf>>---floor and ceiling
INDEX
	floor
INDEX
	floorf
INDEX
	ceil
INDEX
	ceilf

ANSI_SYNOPSIS
	#include <math.h>
	double floor(double <[x]>);
        float floorf(float <[x]>);
        double ceil(double <[x]>);
        float ceilf(float <[x]>);

TRAD_SYNOPSIS
	#include <math.h>
        double floor(<[x]>)
	double <[x]>;
        float floorf(<[x]>)
	float <[x]>;
        double ceil(<[x]>)
	double <[x]>;
        float ceilf(<[x]>)
	float <[x]>;

DESCRIPTION
<<floor>> and <<floorf>> find
@tex
$\lfloor x \rfloor$,
@end tex
the nearest integer less than or equal to <[x]>.
<<ceil>> and <<ceilf>> find
@tex
$\lceil x\rceil$,
@end tex
the nearest integer greater than or equal to <[x]>.

RETURNS
<<floor>> and <<ceil>> return the integer result as a double.
<<floorf>> and <<ceilf>> return the integer result as a float.

PORTABILITY
<<floor>> and <<ceil>> are ANSI.
<<floorf>> and <<ceilf>> are extensions.


*/

/*
 * floor(x)
 * Return x rounded toward -inf to integral value
 * Method:
 *	Bit twiddling.
 * Exception:
 *	Inexact flag raised if x not equal to floor(x).
 */

#include "fdlibm.h"

#ifndef _DOUBLE_IS_32BITS

#ifdef __STDC__
static const double huge = 1.0e300;
#else
static double huge = 1.0e300;
#endif

#ifdef __STDC__
	double floor(double x)
#else
	double floor(x)
	double x;
#endif
{
	int32_t i0,i1,j0;
	uint32_t i,j;
	EXTRACT_WORDS(i0,i1,x);
	j0 = ((i0>>20)&0x7ff)-0x3ff;
	if(j0<20) {
	    if(j0<0) { 	/* raise inexact if x != 0 */
		if(huge+x>0.0) {/* return 0*sign(x) if |x|<1 */
		    if(i0>=0) {i0=i1=0;}
		    else if(((i0&0x7fffffff)|i1)!=0)
			{ i0=0xbff00000;i1=0;}
		}
	    } else {
		i = (0x000fffff)>>j0;
		if(((i0&i)|i1)==0) return x; /* x is integral */
		if(huge+x>0.0) {	/* raise inexact flag */
		    if(i0<0) i0 += (0x00100000)>>j0;
		    i0 &= (~i); i1=0;
		}
	    }
	} else if (j0>51) {
	    if(j0==0x400) return x+x;	/* inf or NaN */
	    else return x;		/* x is integral */
	} else {
	    i = ((uint32_t)(0xffffffff))>>(j0-20);
	    if((i1&i)==0) return x;	/* x is integral */
	    if(huge+x>0.0) { 		/* raise inexact flag */
		if(i0<0) {
		    if(j0==20) i0+=1;
		    else {
			j = i1+(1<<(52-j0));
			if(j<(uint32_t)i1) i0 +=1 ; 	/* got a carry */
			i1=j;
		    }
		}
		i1 &= (~i);
	    }
	}
	INSERT_WORDS(x,i0,i1);
	return x;
}

#endif /* _DOUBLE_IS_32BITS */
