/*****************************************************************
 * The following routines are coded directly from the algorithms
 * and coefficients given in "Software Manual for the Elementary
 * Functions" by William J. Cody, Jr. and William Waite, Prentice
 * Hall, 1980.
 *****************************************************************/

/* Based on newlib/libm/mathfp/sf_sqrt.c in Newlib.  */

#include "amdgcnmach.h"

v64si v64sf_numtestf (v64sf);
v64si v64sf_isposf (v64sf);

DEF_VS_MATH_FUNC (v64sf, sqrtf, v64sf x)
{
  FUNCTION_INIT (v64sf);

  /* Check for special values. */
  v64si num_type = v64sf_numtestf (x);
  VECTOR_IF (num_type == NAN, cond)
    errno = EDOM;
    VECTOR_RETURN (x, cond);
  VECTOR_ELSEIF (num_type == INF, cond)
    VECTOR_IF2 (v64sf_isposf (x), cond2, cond)
      errno = EDOM;
      VECTOR_RETURN (VECTOR_INIT (z_notanum_f.f), cond2);
    VECTOR_ELSE2 (cond2,cond)
      errno = ERANGE;
      VECTOR_RETURN (VECTOR_INIT (z_infinity_f.f), cond);
    VECTOR_ENDIF
  VECTOR_ENDIF

  /* Initial checks are performed here. */
  VECTOR_IF (x == 0.0f, cond)
    VECTOR_RETURN (VECTOR_INIT (0.0f), cond);
  VECTOR_ENDIF
  VECTOR_IF (x < 0.0f, cond)
    errno = EDOM;
    VECTOR_RETURN (VECTOR_INIT (z_notanum_f.f), cond);
  VECTOR_ENDIF

  /* Find the exponent and mantissa for the form x = f * 2^exp. */
  v64sf f = __builtin_gcn_frexpvf_mant (x);
  v64si exp = __builtin_gcn_frexpvf_exp (x);
  v64si odd = (exp & 1) != 0;

  /* Get the initial approximation. */
  v64sf y = 0.41731f + 0.59016f * f;

  f *= 0.5f;
  /* Calculate the remaining iterations. */
  y = y * 0.5f + f / y;
  y = y * 0.5f + f / y;

  /* Calculate the final value. */
  VECTOR_COND_MOVE (y, y * (float) __SQRT_HALF, odd);
  VECTOR_COND_MOVE (exp, exp + 1, odd);
  exp >>= 1;
  y = __builtin_gcn_ldexpvf (y, exp);

  VECTOR_RETURN (y, NO_COND);

  FUNCTION_RETURN;
}

DEF_VARIANTS (sqrtf, sf, sf)
