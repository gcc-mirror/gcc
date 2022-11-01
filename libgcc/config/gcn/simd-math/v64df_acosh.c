/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 *
 */

/* Based on newlib/libm/mathfp/e_acosh.c in Newlib.  */

#include "amdgcnmach.h"

v64df v64df_log_aux (v64df, v64di);
v64df v64df_log1p_aux (v64df, v64di);
v64df v64df_sqrt_aux (v64df, v64di);

DEF_VD_MATH_FUNC (v64df, acosh, v64df x)
{
  static const double one = 1.0;
  static const double ln2 = 6.93147180559945286227e-01;  /* 0x3FE62E42, 0xFEFA39EF */

  FUNCTION_INIT (v64df);

  v64si hx, lx;
  EXTRACT_WORDS (hx, lx, x);

  VECTOR_IF (hx < 0x3ff00000, cond)		// x < 1 */
    VECTOR_RETURN ((x-x) / (x-x), cond);
  VECTOR_ENDIF
  VECTOR_IF (hx >=0x41b00000, cond) 	// x > 2**28 */
    VECTOR_IF2 (hx >=0x7ff00000, cond2, cond)	// x is inf of NaN */
      VECTOR_RETURN (x+x, cond2);
    VECTOR_ELSE (cond2)
      /* acosh(huge)=log(2x) */
      VECTOR_RETURN (v64df_log_aux (x, __mask) + ln2, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF
  VECTOR_IF (((hx - 0x3ff00000) | lx) == 0, cond)
    /* acosh(1) = 0 */
    VECTOR_RETURN (VECTOR_INIT (0.0), cond);
  VECTOR_ENDIF
  VECTOR_IF (hx > 0x40000000, cond)	/* 2**28 > x > 2 */
  {
    v64df t = x * x;
    VECTOR_RETURN (v64df_log_aux (2.0*x - one /
				  (x + v64df_sqrt_aux (t - one, __mask)),
				  __mask),
		   cond);
  }
  VECTOR_ELSE (cond)	/* 1<x<2 */
  {
    v64df t = x - one;
    VECTOR_RETURN (v64df_log1p_aux (t + v64df_sqrt_aux(2.0*t + t*t, __mask),
				    __mask),
		   cond);
  }
  VECTOR_ENDIF

  FUNCTION_RETURN;
}

DEF_VARIANTS (acosh, df, df)

