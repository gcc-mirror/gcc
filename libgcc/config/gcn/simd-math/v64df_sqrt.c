/*****************************************************************
 * The following routines are coded directly from the algorithms
 * and coefficients given in "Software Manual for the Elementary
 * Functions" by William J. Cody, Jr. and William Waite, Prentice
 * Hall, 1980.
 *****************************************************************/

/* Based on newlib/libm/mathfp/s_sqrt.c in Newlib.  */

#include "amdgcnmach.h"

v64si v64df_numtest (v64df);
v64si v64df_ispos (v64df);

DEF_VD_MATH_FUNC (v64df, sqrt, v64df x)
{
  FUNCTION_INIT (v64df);

  /* Check for special values. */
  v64si num_type = v64df_numtest (x);
  VECTOR_IF (num_type == NAN, cond)
    errno = EDOM;
    VECTOR_RETURN (x, cond);
  VECTOR_ELSEIF (num_type == INF, cond)
    VECTOR_IF2 (v64df_ispos (x), cond2, cond)
      errno = EDOM;
      VECTOR_RETURN (VECTOR_INIT (z_notanum.d), cond2);
    VECTOR_ELSE2 (cond2,cond)
      errno = ERANGE;
      VECTOR_RETURN (VECTOR_INIT (z_infinity.d), cond);
    VECTOR_ENDIF
  VECTOR_ENDIF

  /* Initial checks are performed here. */
  VECTOR_IF (x == 0.0, cond)
    VECTOR_RETURN (VECTOR_INIT (0.0), cond);
  VECTOR_ENDIF
  VECTOR_IF (x < 0.0, cond)
    errno = EDOM;
    VECTOR_RETURN (VECTOR_INIT (z_notanum.d), cond);
  VECTOR_ENDIF

  /* Find the exponent and mantissa for the form x = f * 2^exp. */
  v64df f = __builtin_gcn_frexpv_mant (x);
  v64si exp = __builtin_gcn_frexpv_exp (x);
  v64si odd = (exp & 1) != 0;

  /* Get the initial approximation. */
  v64df y = 0.41731 + 0.59016 * f;

  f *= 0.5f;
  /* Calculate the remaining iterations. */
  y = y * 0.5f + f / y;
  y = y * 0.5f + f / y;
  y = y * 0.5f + f / y;

  /* Calculate the final value. */
  VECTOR_COND_MOVE (y, y * __SQRT_HALF, odd);
  VECTOR_COND_MOVE (exp, exp + 1, odd);
  exp >>= 1;
  y = __builtin_gcn_ldexpv (y, exp);

  VECTOR_RETURN (y, NO_COND);

  FUNCTION_RETURN;
}

DEF_VARIANTS (sqrt, df, df)
