#include "f2c.h"

#ifdef KR_headers
extern double erfc();

double G77_derfc_0 (x) doublereal *x;
#else
extern double erfc(double);

double G77_derfc_0 (doublereal *x)
#endif
{
return( erfc(*x) );
}
