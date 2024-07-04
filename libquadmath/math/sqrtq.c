#include "quadmath-imp.h"
#include <math.h>
#include <float.h>
#if __has_include("../../libgcc/soft-fp/soft-fp.h") \
    && __has_include("../../libgcc/soft-fp/quad.h") \
    && defined(FE_TONEAREST) \
    && defined(FE_UPWARD) \
    && defined(FE_DOWNWARD) \
    && defined(FE_TOWARDZERO) \
    && defined(FE_INEXACT)
#define USE_SOFT_FP 1
#include "../../libgcc/soft-fp/soft-fp.h"
#include "../../libgcc/soft-fp/quad.h"
#endif

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

#if USE_SOFT_FP
  FP_DECL_EX;
  FP_DECL_Q (X);
  FP_DECL_Q (Y);

  FP_INIT_ROUNDMODE;
  FP_UNPACK_Q (X, x);
  FP_SQRT_Q (Y, X);
  FP_PACK_Q (y, Y);
  FP_HANDLE_EXCEPTIONS;
  return y;
#else
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
  {
    long double xl = (long double) x;
    if (xl <= LDBL_MAX && xl >= LDBL_MIN)
      {
	/* Use long double result as starting point.  */
	y = (__float128) sqrtl (xl);

	/* One Newton iteration.  */
	y -= 0.5q * (y - x / y);
	return y;
      }
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
#endif
}
