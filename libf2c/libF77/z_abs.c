#include "f2c.h"

#ifdef KR_headers
double f__cabs();
double z_abs(z) doublecomplex *z;
#else
double f__cabs(double, double);
double z_abs(doublecomplex *z)
#endif
{
return( f__cabs( z->r, z->i ) );
}
