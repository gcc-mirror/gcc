#include "f2c.h"

double
r_dim (real * a, real * b)
{
  return (*a > *b ? *a - *b : 0);
}
