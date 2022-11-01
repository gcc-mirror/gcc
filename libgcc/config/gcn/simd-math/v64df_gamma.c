#include "amdgcnmach.h"

v64df v64df_lgamma_aux (v64df x, v64di __mask);

DEF_VD_MATH_FUNC (v64df, gamma, v64df x)
{
  return v64df_lgamma_aux(x, __mask);
}

DEF_VARIANTS (gamma, df, df)
