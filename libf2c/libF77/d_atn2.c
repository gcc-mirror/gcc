#include "f2c.h"

#undef abs
#include <math.h>
double
d_atn2 (doublereal * x, doublereal * y)
{
  return (atan2 (*x, *y));
}
