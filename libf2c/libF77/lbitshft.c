#include "f2c.h"

integer
lbit_shift (integer a, integer b)
{
  return b >= 0 ? a << b : (integer) ((uinteger) a >> -b);
}
