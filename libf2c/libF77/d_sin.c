#include "f2c.h"

#undef abs
#include <math.h>
double
d_sin (doublereal * x)
{
  return (sin (*x));
}
