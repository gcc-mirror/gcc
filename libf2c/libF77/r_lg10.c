#include "f2c.h"

#define log10e 0.43429448190325182765

#undef abs
#include <math.h>
double
r_lg10 (real * x)
{
  return (log10e * log (*x));
}
