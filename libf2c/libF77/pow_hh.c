#include "f2c.h"

#ifdef KR_headers
shortint pow_hh(ap, bp) shortint *ap, *bp;
#else
shortint pow_hh(shortint *ap, shortint *bp)
#endif
{
	shortint pow, x, n;
	unsigned u;

	x = *ap;
	n = *bp;

	if (n <= 0) {
		if (n == 0 || x == 1)
			return 1;
		if (x != -1)
			return x == 0 ? 1/x : 0;
		n = -n;
		}
	u = n;
	for(pow = 1; ; )
		{
		if(u & 01)
			pow *= x;
		if(u >>= 1)
			x *= x;
		else
			break;
		}
	return(pow);
	}
