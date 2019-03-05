/* s_finitel.c -- long double version of s_finite.c.
 * Conversion to IEEE quad long double by Jakub Jelinek, jj@ultra.linux.cz.
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
static char rcsid[] = "$NetBSD: $";
#endif

/*
 * finiteq(x) returns 1 is x is finite, else 0;
 * no branching!
 */

#include "quadmath-imp.h"

int finiteq(__float128 x)
{
	int64_t hx;
	GET_FLT128_MSW64(hx,x);
	return (int)((uint64_t)((hx&0x7fff000000000000LL)
				-0x7fff000000000000LL)>>63);
}
