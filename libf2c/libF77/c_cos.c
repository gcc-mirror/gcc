#include "f2c.h"

#ifdef KR_headers
extern double sin(), cos(), sinh(), cosh();

VOID c_cos(resx, z) complex *resx, *z;
#else
#undef abs
#include <math.h>

void c_cos(complex *resx, complex *z)
#endif
{
complex res;

res.r = cos(z->r) * cosh(z->i);
res.i = - sin(z->r) * sinh(z->i);

resx->r = res.r;
resx->i = res.i;
}
