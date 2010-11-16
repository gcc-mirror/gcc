#include "quadmath-imp.h"
#include <math.h>
#include <float.h>

__float128
cbrtq (const __float128 x)
{
  __float128 y;
  int exp, i;

  if (x == 0)
    return x;

  if (isnanq (x))
    return x;

  if (x <= DBL_MAX && x >= DBL_MIN)
  {
    /* Use double result as starting point.  */
    y = cbrt ((double) x);

    /* Two Newton iterations.  */
    y -= 0.333333333333333333333333333333333333333333333333333Q
	  * (y - x / (y * y));
    y -= 0.333333333333333333333333333333333333333333333333333Q
	  * (y - x / (y * y));
    return y;
  }

#ifdef HAVE_CBRTL
  if (x <= LDBL_MAX && x >= LDBL_MIN)
  {
    /* Use long double result as starting point.  */
    y = cbrtl ((long double) x);

    /* One Newton iteration.  */
    y -= 0.333333333333333333333333333333333333333333333333333Q
	  * (y - x / (y * y));
    return y;
  }
#endif

  /* If we're outside of the range of C types, we have to compute
     the initial guess the hard way.  */
  y = frexpq (x, &exp);

  i = exp % 3;
  y = (i >= 0 ? i : -i);
  if (i == 1)
    y *= 2, exp--;
  else if (i == 2)
    y *= 4, exp -= 2;

  y = cbrt (y);
  y = scalbnq (y, exp / 3);

  /* Two Newton iterations.  */
  y -= 0.333333333333333333333333333333333333333333333333333Q
	 * (y - x / (y * y));
  y -= 0.333333333333333333333333333333333333333333333333333Q
	 * (y - x / (y * y));
  return y;
}

