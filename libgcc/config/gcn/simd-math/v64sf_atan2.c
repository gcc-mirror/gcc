/* Based on newlib/libm/mathfp/sf_atan2.c in Newlib.  */

#include "amdgcnmach.h"

v64sf v64sf_atangentf_aux (v64sf, v64sf, v64sf, int, v64si);

DEF_VS_MATH_FUNC (v64sf, atan2f, v64sf v, v64sf u)
{
  return v64sf_atangentf_aux (VECTOR_INIT (0.0f), v, u, 1, __mask);
}

DEF_VARIANTS2 (atan2f, sf, sf)
