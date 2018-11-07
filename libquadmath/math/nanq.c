#include "quadmath-imp.h"

__float128
nanq (const char *tagp __attribute__ ((unused)))
{
  // FIXME -- we should use the argument
  ieee854_float128 f = { 0 };
  f.ieee_nan.exponent = 0x7fff;
  f.ieee_nan.quiet_nan = 0x1;
  return f.value;
}
