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

/* Based on newlib/libm/common/v64sf_finite.c in Newlib.  */

#include "amdgcnmach.h"

DEF_VS_MATH_FUNC (v64si, finitef, v64sf x)
{
  v64si ix = CAST_VECTOR (v64si, x);
  ix &= 0x7fffffff;
  return FLT_UWORD_IS_FINITE(ix);
}

DEF_VARIANTS (finitef, si, sf)
