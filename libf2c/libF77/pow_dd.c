#include "f2c.h"

#undef abs
#include <math.h>
double
pow_dd (doublereal * ap, doublereal * bp)
{
  return (pow (*ap, *bp));
}
