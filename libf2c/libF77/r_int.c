#include "f2c.h"

#ifdef KR_headers
double floor();
double r_int(x) real *x;
#else
#undef abs
#include <math.h>
double r_int(real *x)
#endif
{
return( (*x>0) ? floor(*x) : -floor(- *x) );
}
