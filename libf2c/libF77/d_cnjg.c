#include "f2c.h"

 VOID
d_cnjg(doublecomplex *r, doublecomplex *z)
{
	doublereal zi = z->i;
	r->r = z->r;
	r->i = -zi;
	}
