/* Based on newlib/libm/mathfp/s_asin.c in Newlib.  */

#include "amdgcnmach.h"

v64df v64df_asine_aux (v64df x, int acosine, v64di __mask);

DEF_VD_MATH_FUNC (v64df, asin, v64df x)
{
  return v64df_asine_aux (x, 0, __mask);
}

DEF_VARIANTS (asin, df, df)
