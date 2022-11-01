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

/* Based on newlib/libm/mathfp/e_atanh.c in Newlib.  */

#include "amdgcnmach.h"

v64df v64df_log1p_aux (v64df, v64di);

DEF_VD_MATH_FUNC (v64df, atanh, v64df x)
{
  static const double zero = 0.0;
  static const double one = 1.0, huge = 1e300;

  FUNCTION_INIT (v64df);

  v64df t;
  v64si hx, lx;
  EXTRACT_WORDS (hx, lx, x);
  v64si ix = hx & 0x7fffffff;

  VECTOR_IF ((ix | ((lx | (-lx)) >> 31)) > 0x3ff00000, cond)	// |x|>1
    VECTOR_RETURN ((x - x)/(x - x), cond);
  VECTOR_ENDIF

  VECTOR_IF (ix == 0x3ff00000, cond)
    VECTOR_RETURN (x / zero, cond);
  VECTOR_ENDIF

  VECTOR_IF ((ix < 0x3e300000) & __builtin_convertvector((huge + x) > zero, v64si), cond)	// x<2**-28
    VECTOR_RETURN (x, cond);
  VECTOR_ENDIF

  SET_HIGH_WORD (x, ix, NO_COND);

  VECTOR_IF (ix < 0x3fe00000, cond)		// x < 0.5 */
    v64df t2 = x + x;
  VECTOR_COND_MOVE (t, 0.5 * v64df_log1p_aux (t2 + t2 * x / (one - x), __mask), cond);
  VECTOR_ELSE (cond)
    VECTOR_COND_MOVE (t, 0.5 * v64df_log1p_aux ((x + x) / (one - x), __mask), cond);
  VECTOR_ENDIF

  VECTOR_IF (hx >= 0, cond)
    VECTOR_RETURN (t, cond);
  VECTOR_ELSE (cond)
    VECTOR_RETURN (-t, cond);
  VECTOR_ENDIF

  FUNCTION_RETURN;
}

DEF_VARIANTS (atanh, df, df)
