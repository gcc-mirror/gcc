#include "f2c.h"

#ifdef KR_headers
extern double log(), f__cabs(), atan2();
VOID c_log(r, z) complex *r, *z;
#else
#undef abs
#include "math.h"
extern double f__cabs(double, double);

void c_log(complex *r, complex *z)
#endif
{
	double zi, zr;
	r->i = atan2(zi = z->i, zr = z->r);
	r->r = log( f__cabs(zr, zi) );
	}
