#include "f2c.h"

#ifdef KR_headers
double sqrt();
double d_sqrt(x) doublereal *x;
#else
#undef abs
#include <math.h>
double d_sqrt(doublereal *x)
#endif
{
return( sqrt(*x) );
}
