#include "f2c.h"

integer
i_dim (integer * a, integer * b)
{
  return (*a > *b ? *a - *b : 0);
}
