/******************************************************************
 * The following routines are coded directly from the algorithms
 * and coefficients given in "Software Manual for the Elementary
 * Functions" by William J. Cody, Jr. and William Waite, Prentice
 * Hall, 1980.
 ******************************************************************/

/* Based on newlib/libm/mathfp/s_atangent.c in Newlib.  */

#include <float.h>
#include "amdgcnmach.h"

DEF_VD_MATH_FUNC (v64df, atangent, v64df x, v64df v, v64df u, int arctan2)
{
  static const double ROOT3 = 1.73205080756887729353;
  static const double a[] = { 0.0, 0.52359877559829887308, 1.57079632679489661923,
			      1.04719755119659774615 };
  static const double q[] = { 0.41066306682575781263e+2,
			      0.86157349597130242515e+2,
			      0.59578436142597344465e+2,
			      0.15024001160028576121e+2 };
  static const double p[] = { -0.13688768894191926929e+2,
			      -0.20505855195861651981e+2,
			      -0.84946240351320683534e+1,
			      -0.83758299368150059274 };
  static const float z_rooteps = 7.4505859692e-9;

  FUNCTION_INIT (v64df);

  v64df zero = VECTOR_INIT (0.0);
  v64df pi = VECTOR_INIT (__PI);
  v64df pi_over_two = VECTOR_INIT (__PI_OVER_TWO);
  v64df res;
  v64si branch = VECTOR_INIT (0);

  /* Preparation for calculating arctan2. */
  if (arctan2)
    {
      VECTOR_IF (u == 0.0, cond)
	VECTOR_IF2 (v == 0.0, cond2, cond)
	  errno = ERANGE;
	  VECTOR_RETURN (VECTOR_INIT (0.0), cond2);
	VECTOR_ELSE2 (cond2, cond)
	  VECTOR_COND_MOVE (branch, VECTOR_INIT (-1), cond2);
	  VECTOR_COND_MOVE (res, pi_over_two,  cond2);
	VECTOR_ENDIF
      VECTOR_ENDIF

      VECTOR_IF (~branch, cond)
	/* Get the exponent values of the inputs. */
	v64si expv = __builtin_gcn_frexpv_exp (v);
	v64si expu = __builtin_gcn_frexpv_exp (u);

	/* See if a divide will overflow. */
	v64si e = expv - expu;

	VECTOR_IF2 (e > DBL_MAX_EXP, cond2, cond)
	  VECTOR_COND_MOVE (branch, VECTOR_INIT (-1), cond2);
	  VECTOR_COND_MOVE (res, pi_over_two, cond2);
	VECTOR_ENDIF

	/* Also check for underflow. */
	VECTOR_IF2 (e < DBL_MIN_EXP, cond2, cond)
	  VECTOR_COND_MOVE (branch, VECTOR_INIT (-1), cond2);
	  VECTOR_COND_MOVE (res, zero, cond2);
	VECTOR_ENDIF
      VECTOR_ENDIF
    }

  VECTOR_IF (~branch, cond)
    v64df f;
    v64si N = VECTOR_INIT (0);

    if (arctan2)
      f = __builtin_gcn_fabsv (v / u);
    else
      f = __builtin_gcn_fabsv (x);

    VECTOR_IF2 (__builtin_convertvector(f > 1.0, v64si), cond2, cond)
      VECTOR_COND_MOVE (f, 1.0 / f, cond2);
      VECTOR_COND_MOVE (N, VECTOR_INIT (2), cond2);
    VECTOR_ENDIF

    VECTOR_IF2 (__builtin_convertvector(f > (2.0 - ROOT3), v64si), cond2, cond)
      double A = ROOT3 - 1.0;
      VECTOR_COND_MOVE (f, (((A * f - 0.5) - 0.5) + f) / (ROOT3 + f),
			cond2);
      N += cond2 & 1;
    VECTOR_ENDIF

    /* Check for values that are too small. */
    VECTOR_IF2 (__builtin_convertvector((-z_rooteps < f) & (f < z_rooteps), v64si), cond2, cond)
      VECTOR_COND_MOVE (res, f, cond2);

    /* Calculate the Taylor series. */
    VECTOR_ELSE2 (cond2, cond)
      v64df g = f * f;
      v64df P = (((p[3] * g + p[2]) * g + p[1]) * g + p[0]) * g;
      v64df Q = (((g + q[3]) * g + q[2]) * g + q[1]) * g + q[0];
      v64df R = P / Q;

      VECTOR_COND_MOVE (res, f + f * R, cond2);
    VECTOR_ENDIF

    VECTOR_COND_MOVE (res, -res, cond & (N > 1));

    res += VECTOR_MERGE (VECTOR_INIT (a[1]), zero, cond & (N == 1));
    res += VECTOR_MERGE (VECTOR_INIT (a[2]), zero, cond & (N == 2));
    res += VECTOR_MERGE (VECTOR_INIT (a[3]), zero, cond & (N == 3));
  VECTOR_ENDIF

  if (arctan2)
    {
      /*if (u < 0.0)*/
	VECTOR_COND_MOVE (res, pi - res, u < 0.0);
      /*if (v < 0.0)*/
	VECTOR_COND_MOVE (res, -res, v < 0.0);
    }
  /*else if (x < 0.0) */
  else
    VECTOR_COND_MOVE (res, -res, x < 0.0);

  VECTOR_RETURN (res, NO_COND);

  FUNCTION_RETURN;
}
