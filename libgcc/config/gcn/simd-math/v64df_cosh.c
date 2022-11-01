/* Based on newlib/libm/mathfp/s_cosh.c in Newlib.  */

#include "amdgcnmach.h"

v64df v64df_sineh_aux (v64df, int, v64di);

DEF_VD_MATH_FUNC (v64df, cosh, v64df x)
{
  return v64df_sineh_aux (x, 1, __mask);
}

DEF_VARIANTS (cosh, df, df)