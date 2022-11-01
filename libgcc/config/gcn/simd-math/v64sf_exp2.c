/* Copyright (C) 2002 by  Red Hat, Incorporated. All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software
 * is freely granted, provided that this notice is preserved.
 */

/* Based on newlib/libm/mathfp/sf_exp2.c in Newlib.  */

#include "amdgcnmach.h"

v64sf v64sf_powf_aux (v64sf, v64sf, v64si);

DEF_VS_MATH_FUNC (v64sf, exp2f, v64sf x)
{
  return v64sf_powf_aux (VECTOR_INIT (2.0f), x, __mask);
}

DEF_VARIANTS (exp2f, sf, sf)
