#include "f2c.h"

#undef abs
#include "math.h"
void
z_exp (doublecomplex * r, doublecomplex * z)
{
  double expx, zi = z->i;

  expx = exp (z->r);
  r->r = expx * cos (zi);
  r->i = expx * sin (zi);
}
