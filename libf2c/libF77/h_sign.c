#include "f2c.h"

shortint
h_sign (shortint * a, shortint * b)
{
  shortint x;
  x = (*a >= 0 ? *a : -*a);
  return (*b >= 0 ? x : -x);
}
