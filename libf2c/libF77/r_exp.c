#include "f2c.h"

#undef abs
#include <math.h>
double
r_exp (real * x)
{
  return (exp (*x));
}
