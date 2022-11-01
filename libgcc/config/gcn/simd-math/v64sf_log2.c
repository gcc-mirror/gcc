#include "amdgcnmach.h"

v64sf v64sf_logf_aux (v64sf, v64si);

static const float C3 = 1.4426950408889634073599246810019;

DEF_VS_MATH_FUNC (v64sf, log2f, v64sf x)
{
  return v64sf_logf_aux (x, __mask) * C3;
}

DEF_VARIANTS (log2f, sf, sf)
