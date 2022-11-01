#include "amdgcnmach.h"

v64sf v64sf_lgammaf_aux (v64sf x, v64si __mask);

DEF_VS_MATH_FUNC (v64sf, gammaf, v64sf x)
{
  return v64sf_lgammaf_aux(x, __mask);
}

DEF_VARIANTS (gammaf, sf, sf)
