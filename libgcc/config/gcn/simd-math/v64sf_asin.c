/* Based on newlib/libm/mathfp/sf_asin.c in Newlib.  */

#include "amdgcnmach.h"

v64sf v64sf_asinef_aux (v64sf, int, v64si);

DEF_VS_MATH_FUNC (v64sf, asinf, v64sf x)
{
  return v64sf_asinef_aux (x, 0, __mask);
}

DEF_VARIANTS (asinf, sf, sf)
