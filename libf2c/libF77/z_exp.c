#include "f2c.h"

#ifdef KR_headers
double exp(), cos(), sin();
VOID z_exp(resx, z) doublecomplex *resx, *z;
#else
#undef abs
#include <math.h>
void z_exp(doublecomplex *resx, doublecomplex *z)
#endif
{
double expx;
doublecomplex res;

expx = exp(z->r);
res.r = expx * cos(z->i);
res.i = expx * sin(z->i);

resx->r = res.r;
resx->i = res.i;
}
