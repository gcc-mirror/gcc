#include "quadmath-imp.h"

__float128
nanq (const char *tagp __attribute__ ((unused)))
{
  // FIXME -- we should use the argument
  ieee854_float128 f;
  f.ieee.exponent = 0x7fff;
  f.ieee.mant_high = 0x1;
  return f.value;
}
