#include "f2c.h"

#ifdef KR_headers
double tanh();
double r_tanh(x) real *x;
#else
#undef abs
#include <math.h>
double r_tanh(real *x)
#endif
{
return( tanh(*x) );
}
