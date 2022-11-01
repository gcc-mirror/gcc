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

/* Based on newlib/libm/mathfp/ef_acosh.c in Newlib.  */

#include "amdgcnmach.h"

v64sf v64sf_logf_aux (v64sf, v64si);
v64sf v64sf_log1pf_aux (v64sf, v64si);
v64sf v64sf_sqrtf_aux (v64sf, v64si);

DEF_VS_MATH_FUNC (v64sf, acoshf, v64sf x)
{
  static const float one = 1.0;
  static const float ln2 = 6.9314718246e-01;  /* 0x3f317218 */

  FUNCTION_INIT (v64sf);

  v64si hx;
  GET_FLOAT_WORD (hx, x, NO_COND);

  VECTOR_IF (hx < 0x3f800000, cond)		// x < 1 */
    VECTOR_RETURN ((x-x) / (x-x), cond);
  VECTOR_ENDIF
  VECTOR_IF (hx >=0x4d800000, cond) 	// x > 2**28 */
    VECTOR_IF2 (hx >=0x7f800000, cond2, cond)	// x is inf of NaN */
      VECTOR_RETURN (x+x, cond2);
    VECTOR_ELSE (cond2)
      /* acosh(huge)=log(2x) */
      VECTOR_RETURN (v64sf_logf_aux (x, __mask) + ln2, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF
  VECTOR_IF (hx == 0x3f800000, cond)
    /* acosh(1) = 0 */
    VECTOR_RETURN (VECTOR_INIT (0.0f), cond);
  VECTOR_ENDIF
  VECTOR_IF (hx > 0x40000000, cond)	/* 2**28 > x > 2 */
  {
    v64sf t = x * x;
    VECTOR_RETURN (v64sf_logf_aux (2.0f*x - 1.0f /
				   (x + v64sf_sqrtf_aux (t - 1.0f, __mask)),
				   __mask),
		   cond);
  }
  VECTOR_ELSE (cond)	/* 1<x<2 */
  {
    v64sf t = x - 1.0f;
    VECTOR_RETURN (v64sf_log1pf_aux (t + v64sf_sqrtf_aux(2.0*t + t*t, __mask),
				     __mask),
		   cond);
  }
  VECTOR_ENDIF

  FUNCTION_RETURN;
}

DEF_VARIANTS (acoshf, sf, sf)
