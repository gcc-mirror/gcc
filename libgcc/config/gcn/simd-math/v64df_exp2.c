/* Copyright (C) 2002 by  Red Hat, Incorporated. All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software
 * is freely granted, provided that this notice is preserved.
 */

/* Based on newlib/libm/mathfp/s_exp2.c in Newlib.  */

#include "amdgcnmach.h"

v64df v64df_pow_aux (v64df, v64df, v64di);

DEF_VD_MATH_FUNC (v64df, exp2, v64df x)
{
  return v64df_pow_aux (VECTOR_INIT (2.0), x, __mask);
}

DEF_VARIANTS (exp2, df, df)
