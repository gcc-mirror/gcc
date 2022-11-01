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

/* Based on newlib/libm/math/w_lgamma.c in Newlib.  */

#include "amdgcnmach.h"

v64si v64df_finite_aux (v64df x, v64di __mask);
v64df v64df_lgamma_r_aux (v64df x, v64si *signgamp, v64di __mask);

DEF_VD_MATH_FUNC (v64df, lgamma, v64df x)
{
  v64df y = v64df_lgamma_r_aux(x, &(_REENT_V64SI_SIGNGAM(_V64_REENT)), __mask);
  if (ALL_ZEROES_P(v64df_finite_aux(y, __mask)) & !ALL_ZEROES_P(v64df_finite_aux(x, __mask))) {
    /* lgamma(finite) overflow */
    errno = ERANGE;
  }
  return y;
}

DEF_VARIANTS (lgamma, df, df)
