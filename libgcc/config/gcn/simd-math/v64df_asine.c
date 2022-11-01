/******************************************************************
 * The following routines are coded directly from the algorithms
 * and coefficients given in "Software Manual for the Elementary
 * Functions" by William J. Cody, Jr. and William Waite, Prentice
 * Hall, 1980.
 ******************************************************************/

/* Based on newlib/libm/mathfp/s_asine.c in Newlib.  */

#include "amdgcnmach.h"

v64si v64df_numtest (v64df);
v64df v64df_sqrt_aux (v64df, v64di);

static const double p[] = { -0.27368494524164255994e+2,
			     0.57208227877891731407e+2,
			    -0.39688862997404877339e+2,
			     0.10152522233806463645e+2,
			    -0.69674573447350646411 };
static const double q[] = { -0.16421096714498560795e+3,
			     0.41714430248260412556e+3,
			    -0.38186303361750149284e+3,
			     0.15095270841030604719e+3,
			    -0.23823859153670238830e+2 };
static const double a[] = { 0.0, 0.78539816339744830962 };
static const double b[] = { 1.57079632679489661923, 0.78539816339744830962 };

DEF_VD_MATH_FUNC (v64df, asine, v64df x, int acosine)
{
  FUNCTION_INIT (v64df);

  v64si branch = VECTOR_INIT (0);

  /* Check for special values. */
  v64si i = v64df_numtest (x);
  VECTOR_IF ((i == NAN) | (i == INF), cond)
    errno = EDOM;
    VECTOR_RETURN (VECTOR_MERGE (x, VECTOR_INIT (z_infinity.d),
                                 i == NAN),
                   cond);
  VECTOR_ENDIF

  v64df y = __builtin_gcn_fabsv (x);
  v64df g, res;

  VECTOR_IF (y > 0.5, cond)
    VECTOR_COND_MOVE (i, VECTOR_INIT (1 - acosine), cond);

    /* Check for range error. */
    VECTOR_IF2 (y > 1.0, cond2, cond)
      errno = ERANGE;
      VECTOR_RETURN (VECTOR_INIT (z_notanum.d), cond2);
    VECTOR_ENDIF

    VECTOR_COND_MOVE (g, (1.0 - y) / 2.0, cond);
    VECTOR_COND_MOVE (y, -2.0 * v64df_sqrt_aux (g, __mask), cond);
    VECTOR_COND_MOVE (branch, VECTOR_INIT (-1), cond);
  VECTOR_ELSE (cond)
    VECTOR_COND_MOVE (i, VECTOR_INIT (acosine), cond);
    VECTOR_IF2 (y < z_rooteps, cond2, cond)
      VECTOR_COND_MOVE (res, y, cond2);
    VECTOR_ELSE2 (cond2, cond)
	    VECTOR_COND_MOVE (g, y * y, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF

  VECTOR_IF ((y >= z_rooteps) | __builtin_convertvector(branch, v64di), cond)
    {
      /* Calculate the Taylor series. */
      v64df P = ((((p[4] * g + p[3]) * g + p[2]) * g + p[1]) * g + p[0]) * g;
      v64df Q = ((((g + q[4]) * g + q[3]) * g + q[2]) * g + q[1]) * g + q[0];
      v64df R = P / Q;

      VECTOR_COND_MOVE (res, y + y * R, cond);
    }
  VECTOR_ENDIF

  v64df a_i = VECTOR_MERGE (VECTOR_INIT (a[1]), VECTOR_INIT (a[0]), i != 0);

  /* Calculate asine or acose. */
  if (acosine == 0)
    {
      VECTOR_COND_MOVE (res, (a_i + res) + a_i, NO_COND);
      VECTOR_IF (x < 0.0, cond)
        VECTOR_COND_MOVE (res, -res, cond);
      VECTOR_ENDIF
    }
  else
    {
      v64df b_i = VECTOR_MERGE (VECTOR_INIT(b[1]), VECTOR_INIT(b[0]), i != 0);

      VECTOR_IF (x < 0.0, cond)
        VECTOR_COND_MOVE (res, (b_i + res) + b_i, cond);
      VECTOR_ELSE (cond)
        VECTOR_COND_MOVE (res, (a_i - res) + a_i, cond);
      VECTOR_ENDIF
    }

  VECTOR_RETURN (res, NO_COND);

  FUNCTION_RETURN;
}
