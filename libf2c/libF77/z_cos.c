#include "f2c.h"

#ifdef KR_headers
double sin(), cos(), sinh(), cosh();
VOID z_cos(resx, z) doublecomplex *resx, *z;
#else
#undef abs
#include <math.h>
void z_cos(doublecomplex *resx, doublecomplex *z)
#endif
{
doublecomplex res;

res.r = cos(z->r) * cosh(z->i);
res.i = - sin(z->r) * sinh(z->i);

resx->r = res.r;
resx->i = res.i;
}
