#include "f2c.h"

#ifdef KR_headers
double tanh();
double d_tanh(x) doublereal *x;
#else
#undef abs
#include <math.h>
double d_tanh(doublereal *x)
#endif
{
return( tanh(*x) );
}
