#include "f2c.h"

double f__cabs (double, double);
double
z_abs (doublecomplex * z)
{
  return (f__cabs (z->r, z->i));
}
