/* Based on newlib/libm/mathfp/s_cos.c in Newlib.  */

#include "amdgcnmach.h"

v64df v64df_sine_aux (v64df, int, v64di);

DEF_VD_MATH_FUNC (v64df, cos, v64df x)
{
  return v64df_sine_aux (x, 1, __mask);
}

DEF_VARIANTS (cos, df, df)
