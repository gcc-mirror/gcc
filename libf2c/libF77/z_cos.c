#include "f2c.h"

#undef abs
#include "math.h"
void
z_cos (doublecomplex * r, doublecomplex * z)
{
  double zi = z->i, zr = z->r;
  r->r = cos (zr) * cosh (zi);
  r->i = -sin (zr) * sinh (zi);
}
