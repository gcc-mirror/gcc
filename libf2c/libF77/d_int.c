#include "f2c.h"

#undef abs
#include <math.h>
double
d_int (doublereal * x)
{
  return ((*x > 0) ? floor (*x) : -floor (-*x));
}
