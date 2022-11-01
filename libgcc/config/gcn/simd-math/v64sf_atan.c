/* Based on newlib/libm/mathfp/sf_atan.c in Newlib.  */

#include "amdgcnmach.h"

v64sf v64sf_atangentf_aux (v64sf, v64sf, v64sf, int, v64si);

DEF_VS_MATH_FUNC (v64sf, atanf, v64sf x)
{
  return (v64sf_atangentf_aux (x, VECTOR_INIT (0.0f), VECTOR_INIT (0.0f),
                               0, __mask));
}

DEF_VARIANTS (atanf, sf, sf)
