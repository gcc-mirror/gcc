#include "f2c.h"

#undef abs
#include "math.h"
void
z_sin (doublecomplex * r, doublecomplex * z)
{
  double zi = z->i, zr = z->r;
  r->r = sin (zr) * cosh (zi);
  r->i = cos (zr) * sinh (zi);
}
