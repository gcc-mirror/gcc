#include "f2c.h"

#ifdef KR_headers
double floor();
double d_nint(x) doublereal *x;
#else
#undef abs
#include <math.h>
double d_nint(doublereal *x)
#endif
{
return( (*x)>=0 ?
	floor(*x + .5) : -floor(.5 - *x) );
}
