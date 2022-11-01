/* Based on newlib/libm/mathfp/s_acos.c in Newlib.  */

#include "amdgcnmach.h"

v64df v64df_asine_aux (v64df x, int acosine, v64di);

DEF_VD_MATH_FUNC (v64df, acos, v64df x)
{
  return v64df_asine_aux(x, 1, __mask);
}

DEF_VARIANTS (acos, df, df)

