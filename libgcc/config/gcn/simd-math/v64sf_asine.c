/******************************************************************
 * The following routines are coded directly from the algorithms
 * and coefficients given in "Software Manual for the Elementary
 * Functions" by William J. Cody, Jr. and William Waite, Prentice
 * Hall, 1980.
 ******************************************************************/

/* Based on newlib/libm/mathfp/sf_asine.c in Newlib.  */

#include "amdgcnmach.h"

v64si v64sf_numtestf (v64sf);
v64sf v64sf_sqrtf (v64sf);

static const float p[] = { 0.933935835, -0.504400557 };
static const float q[] = { 0.560363004e+1, -0.554846723e+1 };
static const float a[] = { 0.0, 0.785398163 };
static const float b[] = { 1.570796326, 0.785398163 };

DEF_VS_MATH_FUNC (v64sf, asinef, v64sf x, int acosine)
{
  FUNCTION_INIT (v64sf);

  v64si branch = VECTOR_INIT (0);

  /* Check for special values. */
  v64si i = v64sf_numtestf (x);
  VECTOR_IF ((i == NAN) | (i == INF), cond)
    errno = EDOM;
    VECTOR_RETURN (VECTOR_MERGE (x, VECTOR_INIT (z_infinity_f.f),
                                 i == NAN),
                   cond);
  VECTOR_ENDIF

  v64sf y = __builtin_gcn_fabsvf (x);
  v64sf g, res;

  VECTOR_IF (y > 0.5f, cond)
    VECTOR_COND_MOVE (i, VECTOR_INIT (1 - acosine), cond);

    /* Check for range error. */
    VECTOR_IF2 (y > 1.0f, cond2, cond)
      errno = ERANGE;
      VECTOR_RETURN (VECTOR_INIT (z_notanum_f.f), cond2);
    VECTOR_ENDIF

    VECTOR_COND_MOVE (g, (1.0f - y) / 2.0f, cond);
    VECTOR_COND_MOVE (y, -2.0f * v64sf_sqrtf (g), cond);
    VECTOR_COND_MOVE (branch, VECTOR_INIT (-1), cond);
  VECTOR_ELSE (cond)
    VECTOR_COND_MOVE (i, VECTOR_INIT (acosine), cond);
    VECTOR_IF2 (y < z_rooteps_f, cond2, cond)
      VECTOR_COND_MOVE (res, y, cond2);
    VECTOR_ELSE2 (cond2, cond)
	    VECTOR_COND_MOVE (g, y * y, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF

  VECTOR_IF ((y >= z_rooteps_f) | branch, cond)
    {
      /* Calculate the Taylor series. */
      v64sf P = (p[1] * g + p[0]) * g;
      v64sf Q = (g + q[1]) * g + q[0];
      v64sf R = P / Q;

      VECTOR_COND_MOVE (res, y + y * R, cond);
    }
  VECTOR_ENDIF

  v64sf a_i = VECTOR_MERGE (VECTOR_INIT (a[1]), VECTOR_INIT (a[0]), i != 0);

  /* Calculate asine or acose. */
  if (acosine == 0)
    {
      VECTOR_COND_MOVE (res, (a_i + res) + a_i, NO_COND);
      VECTOR_IF (x < 0.0f, cond)
        VECTOR_COND_MOVE (res, -res, cond);
      VECTOR_ENDIF
    }
  else
    {
      v64sf b_i = VECTOR_MERGE (VECTOR_INIT(b[1]), VECTOR_INIT(b[0]), i != 0);

      VECTOR_IF (x < 0.0f, cond)
        VECTOR_COND_MOVE (res, (b_i + res) + b_i, cond);
      VECTOR_ELSE (cond)
        VECTOR_COND_MOVE (res, (a_i - res) + a_i, cond);
      VECTOR_ENDIF
    }

  VECTOR_RETURN (res, NO_COND);

  FUNCTION_RETURN;
}
