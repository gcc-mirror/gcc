/* Based on newlib/libm/mathfp/sf_log10.c in Newlib.  */

#include "amdgcnmach.h"

v64sf v64sf_logf_aux (v64sf, v64si);

static const float C3 = 0.4342944819;

DEF_VS_MATH_FUNC (v64sf, log10f, v64sf x)
{
  return v64sf_logf_aux (x, __mask) * C3;
}

DEF_VARIANTS (log10f, sf, sf)
