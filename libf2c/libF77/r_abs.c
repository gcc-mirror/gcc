#include "f2c.h"

double
r_abs (real * x)
{
  if (*x >= 0)
    return (*x);
  return (-*x);
}
