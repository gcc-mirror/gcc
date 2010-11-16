#include "quadmath-imp.h"


int
signbitq (const __float128 x)
{
  ieee854_float128 f;
  f.value = x;
  return f.ieee.negative;
}
