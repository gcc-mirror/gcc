#include "f2c.h"

#ifdef KR_headers
double atan();
double r_atan(x) real *x;
#else
#undef abs
#include <math.h>
double r_atan(real *x)
#endif
{
return( atan(*x) );
}
