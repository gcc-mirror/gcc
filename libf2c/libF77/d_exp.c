#include "f2c.h"

#ifdef KR_headers
double exp();
double d_exp(x) doublereal *x;
#else
#undef abs
#include <math.h>
double d_exp(doublereal *x)
#endif
{
return( exp(*x) );
}
