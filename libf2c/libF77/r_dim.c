#include "f2c.h"

#ifdef KR_headers
double r_dim(a,b) real *a, *b;
#else
double r_dim(real *a, real *b)
#endif
{
return( *a > *b ? *a - *b : 0);
}
