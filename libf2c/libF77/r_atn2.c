#include "f2c.h"

#undef abs
#include <math.h>
double
r_atn2 (real * x, real * y)
{
  return (atan2 (*x, *y));
}
