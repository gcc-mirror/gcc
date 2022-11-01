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

/* Based on newlib/libm/common/s_modf.c in Newlib.  */

#include "amdgcnmach.h"

v64si v64df_numtest (v64df);

DEF_VD_MATH_FUNC (v64df, modf, v64df x, v64df *iptr)
{
  FUNCTION_INIT (v64df);
  v64df ret_i;

  v64si i0, i1;
  EXTRACT_WORDS(i0, i1, x);
  v64si j0 = ((i0 >> 20) & 0x7ff) - 0x3ff;        /* exponent of x */
  v64df zero;
  v64si i;
  INSERT_WORDS (zero, i0 & 0x80000000, VECTOR_INIT (0), NO_COND);

  VECTOR_IF (j0 < 20, cond)                     /* integer part in x*/
    VECTOR_IF2 (j0 < 0, cond2, cond)            /* |x|<1 */
      VECTOR_COND_MOVE (ret_i, zero, cond2);
      VECTOR_RETURN (x, cond2);
    VECTOR_ELSE2 (cond2, cond)
      i = (0x000fffff) >> j0;

      VECTOR_IF2 (((i0 & i) | i1) == 0, cond3, cond2)  /* x is integral */
        VECTOR_COND_MOVE (ret_i, x, cond3);
        VECTOR_RETURN (zero, cond3);
      VECTOR_ELSE2 (cond3, cond2)
        INSERT_WORDS (ret_i, i0 & ~i, VECTOR_INIT (0), cond3);
        VECTOR_RETURN (x - ret_i, cond3);
      VECTOR_ENDIF
    VECTOR_ENDIF
  VECTOR_ELSEIF (j0 > 51, cond)                  /* no fraction part */
    VECTOR_COND_MOVE (ret_i, x, cond);
    VECTOR_IF2 (v64df_numtest (x) == NAN, cond2, cond)
      VECTOR_COND_MOVE (ret_i, x + x, cond2);
      VECTOR_RETURN (ret_i, cond2);             /* x is NaN, return NaN */
    VECTOR_ENDIF
    VECTOR_RETURN (zero, cond);                 /* return +- 0 */
  VECTOR_ELSE (cond)
    i = 0xffffffff >> (j0 - 20);
    VECTOR_IF2 ((i1 & i) == 0, cond2, cond)
      VECTOR_COND_MOVE (ret_i, x, cond2);
      INSERT_WORDS (x, i0 & 0x80000000, VECTOR_INIT (0), cond2);
      VECTOR_RETURN (x, cond2);
    VECTOR_ELSE2 (cond2, cond)
      INSERT_WORDS (ret_i, i0, i1 & ~i, cond2);
      VECTOR_RETURN (x - ret_i, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF

  *iptr = ret_i;
  FUNCTION_RETURN;
}
