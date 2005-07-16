
/* @(#)s_copysign.c 5.1 93/09/24 */
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
<<copysign>>, <<copysignf>>---sign of <[y]>, magnitude of <[x]>

INDEX
	copysign
INDEX
	copysignf

ANSI_SYNOPSIS
	#include <math.h>
	double copysign (double <[x]>, double <[y]>);
	float copysignf (float <[x]>, float <[y]>);

TRAD_SYNOPSIS
	#include <math.h>
	double copysign (<[x]>, <[y]>)
	double <[x]>;
	double <[y]>;

	float copysignf (<[x]>, <[y]>)
	float <[x]>;
	float <[y]>;

DESCRIPTION
<<copysign>> constructs a number with the magnitude (absolute value)
of its first argument, <[x]>, and the sign of its second argument,
<[y]>.

<<copysignf>> does the same thing; the two functions differ only in
the type of their arguments and result.

RETURNS
<<copysign>> returns a <<double>> with the magnitude of
<[x]> and the sign of <[y]>.
<<copysignf>> returns a <<float>> with the magnitude of
<[x]> and the sign of <[y]>.

PORTABILITY
<<copysign>> is not required by either ANSI C or the System V Interface
Definition (Issue 2).

*/

/*
 * copysign(double x, double y)
 * copysign(x,y) returns a value with the magnitude of x and
 * with the sign bit of y.
 */

#include "fdlibm.h"

#ifndef _DOUBLE_IS_32BITS

#ifdef __STDC__
	double copysign(double x, double y)
#else
	double copysign(x,y)
	double x,y;
#endif
{
	uint32_t hx,hy;
	GET_HIGH_WORD(hx,x);
	GET_HIGH_WORD(hy,y);
	SET_HIGH_WORD(x,(hx&0x7fffffff)|(hy&0x80000000));
        return x;
}

#endif /* _DOUBLE_IS_32BITS */
