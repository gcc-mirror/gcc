#include "f2c.h"

#ifdef KR_headers
double floor();
integer i_dnnt(x) doublereal *x;
#else
#undef abs
#include <math.h>
integer i_dnnt(doublereal *x)
#endif
{
return (integer)(*x >= 0. ? floor(*x + .5) : -floor(.5 - *x));
}
