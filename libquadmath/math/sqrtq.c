#include "quadmath-imp.h"
#include <math.h>
#include <float.h>

__float128
sqrtq (const __float128 x)
{
  __float128 y;
  int exp;

  if (isnanq (x) || (isinfq (x) && x > 0))
    return x;

  if (x == 0)
    return x;

  if (x < 0)
    {
      /* Return NaN with invalid signal.  */
      return (x - x) / (x - x);
    }

  if (x <= DBL_MAX && x >= DBL_MIN)
  {
    /* Use double result as starting point.  */
    y = sqrt ((double) x);

    /* Two Newton iterations.  */
    y -= 0.5q * (y - x / y);
    y -= 0.5q * (y - x / y);
    return y;
  }

#ifdef HAVE_SQRTL
  if (x <= LDBL_MAX && x >= LDBL_MIN)
  {
    /* Use long double result as starting point.  */
    y = sqrtl ((long double) x);

    /* One Newton iteration.  */
    y -= 0.5q * (y - x / y);
    return y;
  }
#endif

  /* If we're outside of the range of C types, we have to compute
     the initial guess the hard way.  */
  y = frexpq (x, &exp);
  if (exp % 2)
    y *= 2, exp--;

  y = sqrt (y);
  y = scalbnq (y, exp / 2);

  /* Two Newton iterations.  */
  y -= 0.5q * (y - x / y);
  y -= 0.5q * (y - x / y);
  return y;
}

