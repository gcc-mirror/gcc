#include "f2c.h"

#undef abs
#include <math.h>
double
r_asin (real * x)
{
  return (asin (*x));
}
