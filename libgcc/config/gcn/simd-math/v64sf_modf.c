/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 */

/* Based on newlib/libm/common/sf_modf.c in Newlib.  */

#include "amdgcnmach.h"

v64si v64sf_numtestf (v64sf);

DEF_VS_MATH_FUNC (v64sf, modff, v64sf x, v64sf *iptr)
{
  FUNCTION_INIT (v64sf);
  v64sf ret_i;

  v64si i0;
  GET_FLOAT_WORD (i0, x, NO_COND);
  v64si j0 = ((i0 >> 23) & 0xff) - 0x7f;        /* exponent of x */
  v64sf zero;
  SET_FLOAT_WORD (zero, i0 & 0x80000000,NO_COND);

  VECTOR_IF (j0 < 23, cond)                     /* integer part in x*/
    VECTOR_IF2 (j0 < 0, cond2, cond)            /* |x|<1 */
      VECTOR_COND_MOVE (ret_i, zero, cond2);
      VECTOR_RETURN (x, cond2);
    VECTOR_ELSE2 (cond2, cond)
      v64si i = (0x007fffff) >> j0;

      VECTOR_IF2 ((i0 & i) == 0, cond3, cond2)  /* x is integral */
        VECTOR_COND_MOVE (ret_i, x, cond3);
        VECTOR_RETURN (zero, cond3);
      VECTOR_ELSE2 (cond3, cond2)
        SET_FLOAT_WORD (ret_i, i0 & ~i, cond3);
        VECTOR_RETURN (x - ret_i, cond3);
      VECTOR_ENDIF
    VECTOR_ENDIF
  VECTOR_ELSE (cond)                            /* no fraction part */
    VECTOR_COND_MOVE (ret_i, x, cond);
    VECTOR_IF2 (v64sf_numtestf (x) == NAN, cond2, cond)
      VECTOR_COND_MOVE (ret_i, x + x, cond2);
      VECTOR_RETURN (ret_i, cond2);             /* x is NaN, return NaN */
    VECTOR_ENDIF
    VECTOR_RETURN (zero, cond);                 /* return +- 0 */
  VECTOR_ENDIF

  *iptr = ret_i;
  FUNCTION_RETURN;
}
