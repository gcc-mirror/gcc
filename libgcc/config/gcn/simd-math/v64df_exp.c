/******************************************************************
 * The following routines are coded directly from the algorithms
 * and coefficients given in "Software Manual for the Elementary
 * Functions" by William J. Cody, Jr. and William Waite, Prentice
 * Hall, 1980.
 ******************************************************************/

/* Based on newlib/libm/mathfp/s_exp.c in Newlib.  */

#include "amdgcnmach.h"

v64si v64df_ispos (v64df);
v64si v64df_numtest (v64df);

static const double INV_LN2 = 1.4426950408889634074;
static const double LN2 = 0.6931471805599453094172321;
static const double p[] = { 0.25, 0.75753180159422776666e-2,
                            0.31555192765684646356e-4 };
static const double q[] = { 0.5, 0.56817302698551221787e-1,
                            0.63121894374398504557e-3,
                            0.75104028399870046114e-6 };

DEF_VD_MATH_FUNC (v64df, exp, v64df x)
{
  FUNCTION_INIT (v64df);

  v64si num_type = v64df_numtest (x);
  VECTOR_IF (num_type == NAN, cond)
    errno = EDOM;
    VECTOR_RETURN (x, cond);
  VECTOR_ELSEIF (num_type == INF, cond)
    errno = ERANGE;
    VECTOR_RETURN (VECTOR_MERGE (VECTOR_INIT (z_infinity.d),
                                 VECTOR_INIT (0.0),
                                 v64df_ispos (x)),
                   cond);
  VECTOR_ELSEIF (num_type == 0, cond)
    VECTOR_RETURN (VECTOR_INIT (1.0), cond);
  VECTOR_ENDIF

  /* Check for out of bounds. */
  VECTOR_IF ((x > BIGX) | (x < SMALLX), cond)
    errno = ERANGE;
    VECTOR_RETURN (x, cond);
  VECTOR_ENDIF

  /* Check for a value too small to calculate. */
  VECTOR_RETURN (VECTOR_INIT (1.0),
                 (-z_rooteps_f < x) & (x < z_rooteps_f));

  /* Calculate the exponent. */
  v64si Nneg = __builtin_convertvector (x * INV_LN2 - 0.5, v64si);
  v64si Npos = __builtin_convertvector (x * INV_LN2 + 0.5, v64si);
  v64si N = VECTOR_MERGE (Nneg, Npos, x < 0.0);

  /* Construct the mantissa. */
  v64df g = x - __builtin_convertvector (N, v64df) * LN2;
  v64df z = g * g;
  v64df P = g * ((p[2] * z + p[1]) * z + p[0]);
  v64df Q = ((q[3] * z + q[2]) * z + q[1]) * z + q[0];
  v64df R = 0.5 + P / (Q - P);

  /* Return the floating point value. */
  N++;
  VECTOR_RETURN (__builtin_gcn_ldexpv (R, N), NO_COND);

  FUNCTION_RETURN;
}

DEF_VARIANTS (exp, df, df)
