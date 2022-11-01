/* Based on newlib/libm/mathfp/sf_sin.c in Newlib.  */

#include "amdgcnmach.h"

v64sf v64sf_sinef_aux (v64sf, int, v64si);

DEF_VS_MATH_FUNC (v64sf, sinf, v64sf x)
{
  return v64sf_sinef_aux (x, 0, __mask);
}

DEF_VARIANTS (sinf, sf, sf)
