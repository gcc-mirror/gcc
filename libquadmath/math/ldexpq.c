/* @(#)s_ldexp.c 5.1 93/09/24 */
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
static char rcsid[] = "$NetBSD: s_ldexp.c,v 1.6 1995/05/10 20:47:40 jtc Exp $";
#endif

#include "quadmath-imp.h"

__float128
ldexpq (__float128 value, int exp)
{
	if(!finiteq(value)||value==0) return value + value;
	value = scalbnq(value,exp);
	if(!finiteq(value)||value==0) errno = ERANGE;
	return value;
}





/* Note, versioning issues are punted to ldbl-opt in this case.  */
