#include "f2c.h"

extern double erfc (double);

double
G77_derfc_0 (doublereal * x)
{
  return (erfc (*x));
}
