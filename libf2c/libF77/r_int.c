#include "f2c.h"

#undef abs
#include <math.h>
double
r_int (real * x)
{
  return ((*x > 0) ? floor (*x) : -floor (-*x));
}
