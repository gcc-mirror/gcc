#include "f2c.h"

#undef abs
#include <math.h>
extern double f__cabs(double,double);
void pow_zz(doublecomplex *r, doublecomplex *a, doublecomplex *b)
{
double logr, logi, x, y;

logr = log( f__cabs(a->r, a->i) );
logi = atan2(a->i, a->r);

x = exp( logr * b->r - logi * b->i );
y = logr * b->i + logi * b->r;

r->r = x * cos(y);
r->i = x * sin(y);
}
