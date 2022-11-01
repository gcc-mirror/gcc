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

/* Based on newlib/libm/common/s_finite.c in Newlib.  */

#include "amdgcnmach.h"

DEF_VD_MATH_PRED (v64si, finite, v64df x)
{
  FUNCTION_INIT (v64si);
  v64si hx;
  GET_HIGH_WORD (hx, x, NO_COND);
  return (((hx & 0x7fffffff) - 0x7ff00000) >> 31) != 0;
}

DEF_VARIANTS (finite, si, df)
