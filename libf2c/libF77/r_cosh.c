#include "f2c.h"

#undef abs
#include <math.h>
double
r_cosh (real * x)
{
  return (cosh (*x));
}
