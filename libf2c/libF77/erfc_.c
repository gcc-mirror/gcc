#include "f2c.h"

#ifdef KR_headers
double erfc();
double G77_erfc_0 (x) real *x;
#else
extern double erfc(double);
double G77_erfc_0 (real *x)
#endif
{
return( erfc(*x) );
}
