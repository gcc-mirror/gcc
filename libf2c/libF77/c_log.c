#include "f2c.h"

#undef abs
#include "math.h"
extern double f__cabs (double, double);

void
c_log (complex * r, complex * z)
{
  double zi, zr;
  r->i = atan2 (zi = z->i, zr = z->r);
  r->r = log (f__cabs (zr, zi));
}
