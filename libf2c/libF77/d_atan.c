#include "f2c.h"

#undef abs
#include <math.h>
double
d_atan (doublereal * x)
{
  return (atan (*x));
}
