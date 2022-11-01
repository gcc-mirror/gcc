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

/* Based on newlib/libm/math/wf_lgamma.c in Newlib.  */

#include "amdgcnmach.h"

v64si v64sf_finitef_aux (v64sf x, v64si __mask);
v64sf v64sf_lgammaf_r_aux (v64sf x, v64si *signgamp, v64si __mask);

DEF_VS_MATH_FUNC (v64sf, lgammaf, v64sf x)
{
  v64sf y = v64sf_lgammaf_r_aux(x, &(_REENT_V64SI_SIGNGAM(_V64_REENT)), __mask);
  if (ALL_ZEROES_P(v64sf_finitef_aux(y, __mask)) & !ALL_ZEROES_P(v64sf_finitef_aux(x, __mask))) {
    /* lgamma(finite) overflow */
    errno = ERANGE;
  }
  return y;
}

DEF_VARIANTS (lgammaf, sf, sf)
