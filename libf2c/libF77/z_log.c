#include "f2c.h"

#ifdef KR_headers
double log(), f__cabs(), atan2();
VOID z_log(r, z) doublecomplex *r, *z;
#else
#undef abs
#include "math.h"
extern double f__cabs(double, double);
void z_log(doublecomplex *r, doublecomplex *z)
#endif
{
	double zi = z->i;
	r->i = atan2(zi, z->r);
	r->r = log( f__cabs( z->r, zi ) );
	}
