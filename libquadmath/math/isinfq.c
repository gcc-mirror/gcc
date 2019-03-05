/*
 * Written by J.T. Conklin <jtc@netbsd.org>.
 * Change for long double by Jakub Jelinek <jj@ultra.linux.cz>
 * Public domain.
 */

#if defined(LIBM_SCCS) && !defined(lint)
static char rcsid[] = "$NetBSD: $";
#endif

/*
 * isinfq(x) returns 1 if x is inf, -1 if x is -inf, else 0;
 * no branching!
 */

#include "quadmath-imp.h"

int
isinfq (__float128 x)
{
	int64_t hx,lx;
	GET_FLT128_WORDS64(hx,lx,x);
	lx |= (hx & 0x7fffffffffffffffLL) ^ 0x7fff000000000000LL;
	lx |= -lx;
	return ~(lx >> 63) & (hx >> 62);
}
