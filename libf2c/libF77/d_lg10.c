#include "f2c.h"

#define log10e 0.43429448190325182765

#ifdef KR_headers
double log();
double d_lg10(x) doublereal *x;
#else
#undef abs
#include <math.h>
double d_lg10(doublereal *x)
#endif
{
return( log10e * log(*x) );
}
