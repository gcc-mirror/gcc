#include "f2c.h"

double
r_sign (real * a, real * b)
{
  double x;
  x = (*a >= 0 ? *a : -*a);
  return (*b >= 0 ? x : -x);
}
