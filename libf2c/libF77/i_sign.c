#include "f2c.h"

integer
i_sign (integer * a, integer * b)
{
  integer x;
  x = (*a >= 0 ? *a : -*a);
  return (*b >= 0 ? x : -x);
}
