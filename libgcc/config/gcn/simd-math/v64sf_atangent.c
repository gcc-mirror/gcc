/******************************************************************
 * The following routines are coded directly from the algorithms
 * and coefficients given in "Software Manual for the Elementary
 * Functions" by William J. Cody, Jr. and William Waite, Prentice
 * Hall, 1980.
 ******************************************************************/

/* Based on newlib/libm/mathfp/sf_atangent.c in Newlib.  */

#include <float.h>
#include "amdgcnmach.h"

static const float ROOT3 = 1.732050807;
static const float a[] = { 0.0, 0.523598775, 1.570796326,
                        1.047197551 };
static const float q[] = { 0.1412500740e+1 };
static const float p[] = { -0.4708325141, -0.5090958253e-1 };

DEF_VS_MATH_FUNC (v64sf, atangentf, v64sf x, v64sf v, v64sf u, int arctan2)
{
  FUNCTION_INIT (v64sf);

  v64sf zero = VECTOR_INIT (0.0f);
  v64sf res;
  v64si branch = VECTOR_INIT (0);

  /* Preparation for calculating arctan2. */
  if (arctan2)
    {
      VECTOR_IF (u == 0.0f, cond)
	VECTOR_IF2 (v == 0.0f, cond2, cond)
	  errno = ERANGE;
	  VECTOR_RETURN (VECTOR_INIT (0.0f), cond2);
	VECTOR_ELSE2 (cond2, cond)
	  VECTOR_COND_MOVE (branch, VECTOR_INIT (-1), cond2);
	  VECTOR_COND_MOVE (res, VECTOR_INIT ((float) __PI_OVER_TWO),  cond2);
	VECTOR_ENDIF
      VECTOR_ENDIF

      VECTOR_IF (~branch, cond)
	/* Get the exponent values of the inputs. */
	v64si expv = __builtin_gcn_frexpvf_exp (v);
	v64si expu = __builtin_gcn_frexpvf_exp (u);

	/* See if a divide will overflow. */
	v64si e = expv - expu;

	VECTOR_IF2 (e > FLT_MAX_EXP, cond2, cond)
	  VECTOR_COND_MOVE (branch, VECTOR_INIT (-1), cond2);
	  VECTOR_COND_MOVE (res, VECTOR_INIT ((float) __PI_OVER_TWO), cond2);
	VECTOR_ENDIF

	/* Also check for underflow. */
	VECTOR_IF2 (e < FLT_MIN_EXP, cond2, cond)
	  VECTOR_COND_MOVE (branch, VECTOR_INIT (-1), cond2);
	  VECTOR_COND_MOVE (res, zero, cond2);
	VECTOR_ENDIF
      VECTOR_ENDIF
    }

  VECTOR_IF (~branch, cond)
    v64sf f;
    v64si N = VECTOR_INIT (0);

    if (arctan2)
      f = __builtin_gcn_fabsvf (v / u);
    else
      f = __builtin_gcn_fabsvf (x);

    VECTOR_IF2 (f > 1.0f, cond2, cond)
      VECTOR_COND_MOVE (f, 1.0f / f, cond2);
      VECTOR_COND_MOVE (N, VECTOR_INIT (2), cond2);
    VECTOR_ENDIF

    VECTOR_IF2 (f > (2.0f - ROOT3), cond2, cond)
      float A = ROOT3 - 1.0f;
      VECTOR_COND_MOVE (f, (((A * f - 0.5f) - 0.5f) + f) / (ROOT3 + f),
			cond2);
      N += cond2 & 1;
    VECTOR_ENDIF

    /* Check for values that are too small. */
    VECTOR_IF2 ((-z_rooteps_f < f) & (f < z_rooteps_f), cond2, cond)
      VECTOR_COND_MOVE (res, f, cond2);

    /* Calculate the Taylor series. */
    VECTOR_ELSE2 (cond2, cond)
      v64sf g = f * f;
      v64sf P = (p[1] * g + p[0]) * g;
      v64sf Q = g + q[0];
      v64sf R = P / Q;

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
	VECTOR_COND_MOVE (res, (float) __PI - res, u < 0.0f);
      /*if (v < 0.0)*/
	VECTOR_COND_MOVE (res, -res, v < 0.0f);
    }
  /*else if (x < 0.0) */
  else
    VECTOR_COND_MOVE (res, -res, x < 0.0f);

  VECTOR_RETURN (res, NO_COND);

  FUNCTION_RETURN;
}
