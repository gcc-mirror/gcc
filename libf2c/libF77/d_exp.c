#include "f2c.h"

#undef abs
#include <math.h>
double
d_exp (doublereal * x)
{
  return (exp (*x));
}
