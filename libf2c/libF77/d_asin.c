#include "f2c.h"

#undef abs
#include <math.h>
double
d_asin (doublereal * x)
{
  return (asin (*x));
}
