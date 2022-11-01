/* Based on newlib/libm/mathfp/s_sinh.c.  */

#include "amdgcnmach.h"

v64df v64df_sineh_aux (v64df, int, v64di);

DEF_VD_MATH_FUNC (v64df, sinh, v64df x)
{
  return v64df_sineh_aux (x, 0, __mask);
}

DEF_VARIANTS (sinh, df, df)
