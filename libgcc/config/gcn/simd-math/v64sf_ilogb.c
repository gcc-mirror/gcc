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

/* Based on newlib/libm/common/sf_ilogb.c in Newlib.  */

#include "amdgcnmach.h"

DEF_VS_MATH_FUNC (v64si, ilogbf, v64sf x)
{
  FUNCTION_INIT(v64si);

  v64si hx, ix;
  GET_FLOAT_WORD (hx, x, NO_COND);
  hx &= 0x7fffffff;
  VECTOR_IF (FLT_UWORD_IS_ZERO (hx), cond)
    VECTOR_RETURN (VECTOR_INIT (-__INT_MAX__), cond);  // FP_ILOGB0
  VECTOR_ENDIF
  VECTOR_IF (FLT_UWORD_IS_SUBNORMAL (hx), cond)
    ix = VECTOR_INIT (-126);
    for (v64si i = (hx << 8);
       !ALL_ZEROES_P (cond & (i > 0));
       i <<= 1)
      VECTOR_COND_MOVE (ix, ix - 1, cond & (i > 0));
    VECTOR_RETURN (ix, cond);
  VECTOR_ELSEIF (~FLT_UWORD_IS_FINITE (hx), cond)
    VECTOR_RETURN (VECTOR_INIT (__INT_MAX__), cond);
  VECTOR_ENDIF

  VECTOR_RETURN ((hx >> 23) - 127, NO_COND);

  FUNCTION_RETURN;
}

DEF_VARIANTS (ilogbf, si, sf)
