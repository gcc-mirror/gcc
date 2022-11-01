/******************************************************************
 * The following routines are coded directly from the algorithms
 * and coefficients given in "Software Manual for the Elementary
 * Functions" by William J. Cody, Jr. and William Waite, Prentice
 * Hall, 1980.
 ******************************************************************/

/* Based on newlib/libm/mathfp/s_algorithm.c in Newlib.  */

#include "amdgcnmach.h"

v64df v64df_log_aux (v64df, v64di);

static const double C3 = 0.43429448190325182765;

DEF_VD_MATH_FUNC (v64df, log10, v64df x)
{
  return v64df_log_aux (x, __mask) * C3;
}

DEF_VARIANTS (log10, df, df)
