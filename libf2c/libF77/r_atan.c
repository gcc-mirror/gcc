#include "f2c.h"

#undef abs
#include <math.h>
double
r_atan (real * x)
{
  return (atan (*x));
}
