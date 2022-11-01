/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 */

/* Based on newlib/libm/mathfp/s_asinh.c in Newlib.  */

#include "amdgcnmach.h"

v64df v64df_log_aux (v64df, v64di);
v64df v64df_log1p_aux (v64df, v64di);
v64df v64df_sqrt_aux (v64df, v64di);

DEF_VD_MATH_FUNC (v64df, asinh, v64df x)
{
  static const double one =  1.00000000000000000000e+00; /* 0x3FF00000, 0x00000000 */
  static const double ln2 =  6.93147180559945286227e-01; /* 0x3FE62E42, 0xFEFA39EF */
  static const double huge = 1.00000000000000000000e+300;

  FUNCTION_INIT (v64df);

  v64df w;
  v64si hx;
  GET_HIGH_WORD (hx, x, NO_COND);
  v64si ix = hx & 0x7fffffff;

  VECTOR_IF (ix >=0x7ff00000, cond)	/* x is inf or NaN */
    VECTOR_RETURN (x + x, cond);
  VECTOR_ENDIF
  VECTOR_IF (ix < 0x3e300000, cond)	/* |x|<2**-28 */
    VECTOR_IF2 (__builtin_convertvector(huge+x > one, v64si), cond2, cond) /* return x inexact except 0 */
      VECTOR_RETURN (x, cond);
    VECTOR_ENDIF
  VECTOR_ENDIF
  VECTOR_IF (ix > 0x41b00000, cond) 	/* x > 2**28 */
    VECTOR_COND_MOVE (w, v64df_log_aux (__builtin_gcn_fabsv (x), __mask) + ln2,
		      cond);
  VECTOR_ELSEIF (ix > 0x40000000, cond)	/* 2**28 > |x| > 2.0 */
    v64df t = __builtin_gcn_fabsv (x);
    VECTOR_COND_MOVE (w, v64df_log_aux (2.0 * t	+ one / (v64df_sqrt_aux (x*x + one, __mask) + t), __mask),
		      cond);
  VECTOR_ELSE (cond)			/* 2.0 > |x| > 2**-28 */
    v64df t = x * x;
    VECTOR_COND_MOVE (w, v64df_log1p_aux (__builtin_gcn_fabsv (x) + t / (one + v64df_sqrt_aux (one + t, __mask)), __mask),
		      cond);
  VECTOR_ENDIF

  VECTOR_IF (hx > 0, cond)
    VECTOR_RETURN (w, cond);
  VECTOR_ELSE (cond)
    VECTOR_RETURN (-w, cond);
  VECTOR_ENDIF

  FUNCTION_RETURN;
}

DEF_VARIANTS (asinh, df, df)
