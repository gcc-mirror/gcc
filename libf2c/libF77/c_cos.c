#include "f2c.h"

#ifdef KR_headers
extern double sin(), cos(), sinh(), cosh();

VOID c_cos(r, z) complex *r, *z;
#else
#undef abs
#include "math.h"

void c_cos(complex *r, complex *z)
#endif
{
	double zr = z->r;
	r->r =   cos(zr) * cosh(z->i);
	r->i = - sin(zr) * sinh(z->i);
	}
