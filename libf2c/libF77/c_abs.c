#include "f2c.h"

extern double f__cabs (double, double);

double
c_abs (complex * z)
{
  return (f__cabs (z->r, z->i));
}
