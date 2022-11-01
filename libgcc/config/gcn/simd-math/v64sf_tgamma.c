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

/* Based on newlib/libm/math/ef_tgamma.c in Newlib. */

#include "amdgcnmach.h"

v64sf v64sf_expf_aux (v64sf x, v64si __mask);
v64sf v64sf_lgammaf_r_aux (v64sf x, v64si *signgamp, v64si __mask);

DEF_VS_MATH_FUNC (v64sf, tgammaf, v64sf x)
{
  v64si signgam_local;
  v64sf y = v64sf_expf_aux(v64sf_lgammaf_r_aux(x, &signgam_local, __mask), __mask);
  VECTOR_COND_MOVE(y, -y, signgam_local < 0);
	return y;
}

DEF_VARIANTS (tgammaf, sf, sf)
