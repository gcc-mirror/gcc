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

/* Based on newlib/libm/mathfp/e_scalb.c in Newlib.  */

#include "amdgcnmach.h"

v64si v64df_isnan (v64df);
v64si v64df_finite (v64df);
v64df v64df_rint_aux (v64df, v64di);
v64df v64df_scalbn_aux (v64df, v64si, v64di);

DEF_VD_MATH_FUNC (v64df, scalb, v64df x, v64df fn)
{
  FUNCTION_INIT (v64df);

  VECTOR_IF (v64df_isnan(x) | v64df_isnan(fn), cond)
    VECTOR_RETURN (x * fn, cond);
  VECTOR_ENDIF
  VECTOR_IF (~v64df_finite (fn), cond)
    VECTOR_IF2 (fn > 0.0, cond2, cond)
      VECTOR_RETURN (x * fn, cond2);
    VECTOR_ELSE2 (cond2, cond)
      VECTOR_RETURN (x / (-fn), cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF
  VECTOR_IF (v64df_rint_aux (fn, __mask) != fn, cond)
    VECTOR_RETURN ((fn-fn)/(fn-fn), cond);
  VECTOR_ENDIF
  VECTOR_IF (fn > 65000.0, cond)
    VECTOR_RETURN (v64df_scalbn_aux (x, VECTOR_INIT (65000), __mask), cond);
  VECTOR_ENDIF
  VECTOR_IF (-fn > 65000.0, cond)
    VECTOR_RETURN (v64df_scalbn_aux (x, VECTOR_INIT (-65000), __mask), cond);
  VECTOR_ENDIF
  VECTOR_RETURN (v64df_scalbn_aux (x, __builtin_convertvector (fn, v64si), __mask),
		 NO_COND);

  FUNCTION_RETURN;
}

DEF_VARIANTS2 (scalb, df, df)
