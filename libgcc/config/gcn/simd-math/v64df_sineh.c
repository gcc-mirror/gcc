/******************************************************************
 * The following routines are coded directly from the algorithms
 * and coefficients given in "Software Manual for the Elementary
 * Functions" by William J. Cody, Jr. and William Waite, Prentice
 * Hall, 1980.
 ******************************************************************/

/* Based on newlib/libm/mathfp/s_sineh.c in Newlib.  */

#include "amdgcnmach.h"

v64df v64df_exp_aux (v64df, v64di);
v64si v64df_numtest (v64df);
v64si v64df_ispos (v64df);

static const double q[] = { -0.21108770058106271242e+7,
                             0.36162723109421836460e+5,
                            -0.27773523119650701667e+3 };
static const double p[] = { -0.35181283430177117881e+6,
                            -0.11563521196851768270e+5,
                            -0.16375798202630751372e+3,
                            -0.78966127417357099479 };
static const double LNV = 0.6931610107421875000;
static const double INV_V2 = 0.24999308500451499336;
static const double V_OVER2_MINUS1 = 0.13830277879601902638e-4;

DEF_VD_MATH_FUNC (v64df, sineh, v64df x, int cosineh)
{
  const double WBAR = 18.55;
  
  FUNCTION_INIT (v64df);

  v64si sgn = VECTOR_INIT (0);
  v64di v_cosineh = VECTOR_INIT (cosineh ? -1L : 0L);

  /* Check for special values. */
  v64si num_type = v64df_numtest (x);
  VECTOR_IF (num_type == NAN, cond)
    errno = EDOM;
    VECTOR_RETURN (x, cond);
  VECTOR_ELSEIF (num_type == INF, cond)
    errno = ERANGE;
    VECTOR_RETURN (VECTOR_MERGE (VECTOR_INIT (z_infinity.d),
				 VECTOR_INIT (-z_infinity.d),
				 v64df_ispos (x)),
		   cond);
  VECTOR_ENDIF

  v64df y = __builtin_gcn_fabsv (x);

  if (!cosineh)
    VECTOR_COND_MOVE (sgn, VECTOR_INIT (-1), x < 0.0);

  v64df res;

  VECTOR_IF (((y > 1.0) & ~v_cosineh) | v_cosineh, cond)
    VECTOR_IF2 (y > BIGX, cond2, cond)
      v64df w = y - LNV;

      /* Check for w > maximum here. */
      VECTOR_IF2 (w > BIGX, cond3, cond2)
	errno = ERANGE;
	VECTOR_RETURN (x, cond3);
      VECTOR_ENDIF

      v64df z = v64df_exp_aux (w, __mask);

      VECTOR_COND_MOVE (res, z * (V_OVER2_MINUS1 + 1.0),
			cond2 & (w > WBAR));
    VECTOR_ELSE2 (cond2, cond)
      v64df z = v64df_exp_aux (y, __mask);
      if (cosineh)
	VECTOR_COND_MOVE (res, (z + 1 / z) * 0.5, cond2);
      else
	VECTOR_COND_MOVE (res, (z - 1 / z) * 0.5, cond2);
    VECTOR_ENDIF

    VECTOR_COND_MOVE (res, -res, sgn);
  VECTOR_ELSE (cond)
    /* Check for y being too small. */
    VECTOR_IF2 (y < z_rooteps, cond2, cond);
      VECTOR_COND_MOVE (res, x, cond2);
    VECTOR_ELSE2 (cond2, cond)
      /* Calculate the Taylor series. */
      v64df f = x * x;
      v64df Q = ((f + q[2]) * f + q[1]) * f + q[0];
      v64df P = ((p[3] * f + p[2]) * f + p[1]) * f + p[0];
      v64df R = f * (P / Q);

      VECTOR_COND_MOVE (res, x + x * R, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF

  VECTOR_RETURN (res, NO_COND);

  FUNCTION_RETURN;
}

