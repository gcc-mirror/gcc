#include "amdgcnmach.h"

v64df v64df_log_aux (v64df, v64di);

static const double C3 = 1.4426950408889634073599246810019;

DEF_VD_MATH_FUNC (v64df, log2, v64df x)
{
  return v64df_log_aux (x, __mask) * C3;
}

DEF_VARIANTS (log2, df, df)
