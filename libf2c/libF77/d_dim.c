#include "f2c.h"

double
d_dim (doublereal * a, doublereal * b)
{
  return (*a > *b ? *a - *b : 0);
}
