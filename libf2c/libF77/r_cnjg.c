#include "f2c.h"

#ifdef KR_headers
VOID r_cnjg(r, z) complex *r, *z;
#else
VOID r_cnjg(complex *r, complex *z)
#endif
{
	real zi = z->i;
	r->r = z->r;
	r->i = -zi;
	}
