#include "f2c.h"

#ifdef KR_headers
double exp(), cos(), sin();
VOID z_exp(r, z) doublecomplex *r, *z;
#else
#undef abs
#include "math.h"
void z_exp(doublecomplex *r, doublecomplex *z)
#endif
{
double expx;

expx = exp(z->r);
r->r = expx * cos(z->i);
r->i = expx * sin(z->i);
}
