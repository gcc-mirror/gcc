#include "f2c.h"

#undef abs
#include <math.h>
double
d_cosh (doublereal * x)
{
  return (cosh (*x));
}
