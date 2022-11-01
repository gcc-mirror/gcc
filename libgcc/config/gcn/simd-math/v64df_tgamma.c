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

/* Based on newlib/libm/math/e_tgamma.c in Newlib. */

#include "amdgcnmach.h"

v64df v64df_exp_aux (v64df x, v64di __mask);
v64df v64df_lgamma_r_aux (v64df x, v64si *signgamp, v64di __mask);

DEF_VD_MATH_FUNC (v64df, tgamma, v64df x)
{
  v64si signgam_local;
  v64df y = v64df_exp_aux(v64df_lgamma_r_aux(x, &signgam_local, __mask), __mask);
  VECTOR_COND_MOVE(y, -y, signgam_local < 0);
	return y;
}

DEF_VARIANTS (tgamma, df, df)
