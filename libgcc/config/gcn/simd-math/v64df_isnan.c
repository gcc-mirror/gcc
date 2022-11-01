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

/* Based on newlib/libm/common/s_isnan.c in Newlib.  */

#include "amdgcnmach.h"

DEF_VD_MATH_PRED (v64si, isnan, v64df x)
{
  v64si hx, lx;
  EXTRACT_WORDS (hx, lx, x);
  hx &= 0x7fffffff;
  hx |= (lx | (-lx)) >> 31;	
  hx = 0x7ff00000 - hx;

  return (hx >> 31) != 0;
}

DEF_VARIANTS (isnan, si, df)
