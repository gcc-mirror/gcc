#include "f2c.h"

#ifdef KR_headers
double sin(), cos(), sinh(), cosh();
VOID z_cos(r, z) doublecomplex *r, *z;
#else
#undef abs
#include "math.h"
void z_cos(doublecomplex *r, doublecomplex *z)
#endif
{
	double zr = z->r;
	r->r =   cos(zr) * cosh(z->i);
	r->i = - sin(zr) * sinh(z->i);
	}
