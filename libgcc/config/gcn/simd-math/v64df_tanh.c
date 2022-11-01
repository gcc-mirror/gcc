/*****************************************************************
 * The following routines are coded directly from the algorithms
 * and coefficients given in "Software Manual for the Elementary
 * Functions" by William J. Cody, Jr. and William Waite, Prentice
 * Hall, 1980.
 *****************************************************************/

/* Based on newlib/libm/mathfp/s_tanh.c in Newlib.  */

#include "amdgcnmach.h"

v64df v64df_exp_aux (v64df, v64di);

static const double LN3_OVER2 = 0.54930614433405484570;
static const double p[] = { -0.16134119023996228053e+4,
                            -0.99225929672236083313e+2,
                            -0.96437492777225469787 };
static const double q[] = { 0.48402357071988688686e+4,
                            0.22337720718962312926e+4,
                            0.11274474380534949335e+3 }; 

DEF_VD_MATH_FUNC (v64df, tanh, v64df x)
{
  FUNCTION_INIT (v64df);

  v64df f = __builtin_gcn_fabsv (x);
  v64df res;

  /* Check if the input is too big. */
  VECTOR_IF (f > BIGX, cond)
    VECTOR_COND_MOVE (res, VECTOR_INIT (1.0), cond);

  VECTOR_ELSEIF (f > LN3_OVER2, cond)
    VECTOR_COND_MOVE (res, 1.0 - 2.0 / (v64df_exp_aux (2 * f, __mask) + 1.0),
		      cond);

  /* Check if the input is too small. */
  VECTOR_ELSEIF (f < z_rooteps, cond)
    VECTOR_COND_MOVE (res, f, cond);

  /* Calculate the Taylor series. */
  VECTOR_ELSE (cond)
    v64df g = f * f;

    v64df P = (p[2] * g + p[1]) * g + p[0];
    v64df Q = ((g + q[2]) * g + q[1]) * g + q[0];
    v64df R = g * (P / Q);

    VECTOR_COND_MOVE (res, f + f * R, cond);
  VECTOR_ENDIF

  VECTOR_COND_MOVE (res, -res, x < 0.0);

  VECTOR_RETURN (res, NO_COND);

  FUNCTION_RETURN;
}

DEF_VARIANTS (tanh, df, df)
