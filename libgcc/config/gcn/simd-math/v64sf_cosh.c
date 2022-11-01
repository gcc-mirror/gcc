/* Based on newlib/libm/mathfp/sf_cosh.c in Newlib.  */

#include "amdgcnmach.h"

v64sf v64sf_sinehf_aux (v64sf, int, v64si);

DEF_VS_MATH_FUNC (v64sf, coshf, v64sf x)
{
  return v64sf_sinehf_aux (x, 1, __mask);
}

DEF_VARIANTS (coshf, sf, sf)
