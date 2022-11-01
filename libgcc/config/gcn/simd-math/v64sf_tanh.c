/*****************************************************************
 * The following routines are coded directly from the algorithms
 * and coefficients given in "Software Manual for the Elementary
 * Functions" by William J. Cody, Jr. and William Waite, Prentice
 * Hall, 1980.
 *****************************************************************/

/* Based on newlib/libm/mathfp/sf_tanh.c in Newlib.  */

#include "amdgcnmach.h"

v64sf v64sf_expf_aux (v64sf, v64si);

static const float LN3_OVER2 = 0.54930614433405484570;
static const float p[] = { -0.16134119023996228053e+4,
                           -0.99225929672236083313e+2,
                           -0.96437492777225469787 };
static const float q[] = { 0.48402357071988688686e+4,
                           0.22337720718962312926e+4,
                           0.11274474380534949335e+3 }; 

DEF_VS_MATH_FUNC (v64sf, tanhf, v64sf x)
{
  FUNCTION_INIT (v64sf);

  v64sf f = __builtin_gcn_fabsvf (x);
  v64sf res;

  /* Check if the input is too big. */
  VECTOR_IF (f > (float) BIGX, cond)
    VECTOR_COND_MOVE (res, VECTOR_INIT (1.0f), cond);

  VECTOR_ELSEIF (f > LN3_OVER2, cond)
    VECTOR_COND_MOVE (res, 1.0f - 2.0f / (v64sf_expf_aux (2.0f * f, __mask) + 1.0f),
		      cond);

  /* Check if the input is too small. */
  VECTOR_ELSEIF (f < z_rooteps_f, cond)
    VECTOR_COND_MOVE (res, f, cond);

  /* Calculate the Taylor series. */
  VECTOR_ELSE (cond)
    v64sf g = f * f;

    v64sf P = (p[2] * g + p[1]) * g + p[0];
    v64sf Q = ((g + q[2]) * g + q[1]) * g + q[0];
    v64sf R = g * (P / Q);

    VECTOR_COND_MOVE (res, f + f * R, cond);
  VECTOR_ENDIF

  VECTOR_COND_MOVE (res, -res, x < 0.0f);

  VECTOR_RETURN (res, NO_COND);

  FUNCTION_RETURN;
}

DEF_VARIANTS (tanhf, sf, sf)
