#include "f2c.h"

#undef abs
#include <math.h>
double
r_tanh (real * x)
{
  return (tanh (*x));
}
