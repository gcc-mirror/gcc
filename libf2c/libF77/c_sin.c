#include "f2c.h"

#undef abs
#include "math.h"

void
c_sin (complex * r, complex * z)
{
  double zi = z->i, zr = z->r;
  r->r = sin (zr) * cosh (zi);
  r->i = cos (zr) * sinh (zi);
}
