#include "f2c.h"

shortint
h_abs (shortint * x)
{
  if (*x >= 0)
    return (*x);
  return (-*x);
}
