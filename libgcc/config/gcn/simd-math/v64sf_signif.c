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

/* Based on newlib/libm/mathfp/sf_signif.c in Newlib.  */

#include "amdgcnmach.h"

v64sf v64sf_scalbf_aux (v64sf x, v64sf fn, v64si);
v64si v64sf_ilogbf_aux (v64sf x, v64si);

DEF_VS_MATH_FUNC (v64sf, significandf, v64sf x)
{
  return v64sf_scalbf_aux (x, -__builtin_convertvector (v64sf_ilogbf_aux (x, __mask), v64sf), __mask);
}

DEF_VARIANTS (significandf, sf, sf)
