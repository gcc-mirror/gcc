#include "f2c.h"

#undef abs
#include <math.h>
double
d_sqrt (doublereal * x)
{
  return (sqrt (*x));
}
