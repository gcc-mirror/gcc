#include "f2c.h"

#ifdef KR_headers
double cosh();
double d_cosh(x) doublereal *x;
#else
#undef abs
#include <math.h>
double d_cosh(doublereal *x)
#endif
{
return( cosh(*x) );
}
