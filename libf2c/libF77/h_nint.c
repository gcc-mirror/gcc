#include "f2c.h"

#undef abs
#include <math.h>
shortint
h_nint (real * x)
{
  return (shortint) (*x >= 0 ? floor (*x + .5) : -floor (.5 - *x));
}
