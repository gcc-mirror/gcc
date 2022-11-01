/* Based on newlib/libm/mathfp/s_atan2.c in Newlib.  */

#include "amdgcnmach.h"

v64df v64df_atangent_aux (v64df, v64df, v64df, int, v64di);

DEF_VD_MATH_FUNC (v64df, atan2, v64df v, v64df u)
{
  return (v64df_atangent_aux (VECTOR_INIT (0.0), v, u, 1, __mask));
}

DEF_VARIANTS2 (atan2, df, df)
