#include "f2c.h"

#undef abs
#include <math.h>
double
d_tanh (doublereal * x)
{
  return (tanh (*x));
}
