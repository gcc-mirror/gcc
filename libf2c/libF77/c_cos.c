#include "f2c.h"

#undef abs
#include "math.h"

void
c_cos (complex * r, complex * z)
{
  double zi = z->i, zr = z->r;
  r->r = cos (zr) * cosh (zi);
  r->i = -sin (zr) * sinh (zi);
}
