#include "f2c.h"

shortint
h_dim (shortint * a, shortint * b)
{
  return (*a > *b ? *a - *b : 0);
}
