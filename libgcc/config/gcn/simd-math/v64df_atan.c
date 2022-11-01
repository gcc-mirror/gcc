/* Based on newlib/libm/mathfp/s_atan.c in Newlib.  */

#include "amdgcnmach.h"

v64df v64df_atangent_aux (v64df, v64df, v64df, int, v64di);

DEF_VD_MATH_FUNC (v64df, atan, v64df x)
{
  return v64df_atangent_aux (x,
			     VECTOR_INIT (0.0),
			     VECTOR_INIT (0.0), 0, __mask);
}

DEF_VARIANTS (atan, df, df)
