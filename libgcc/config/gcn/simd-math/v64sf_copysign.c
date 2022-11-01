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

/* Based on newlib/libm/common/sf_copysign.c in Newlib.  */

#include "amdgcnmach.h"

DEF_VS_MATH_FUNC (v64sf, copysignf, v64sf x, v64sf y)
{
  FUNCTION_INIT (v64sf);

  v64si ix, iy;
  GET_FLOAT_WORD (ix, x, NO_COND);
  GET_FLOAT_WORD (iy, y, NO_COND);
  SET_FLOAT_WORD (x, (ix & 0x7fffffff) | (iy & 0x80000000), NO_COND);
  VECTOR_RETURN (x, NO_COND);

  FUNCTION_RETURN;
}

DEF_VARIANTS2 (copysignf, sf, sf)