#include "f2c.h"

longint
qbit_shift (longint a, integer b)
{
  return b >= 0 ? a << b : (longint) ((ulongint) a >> -b);
}
