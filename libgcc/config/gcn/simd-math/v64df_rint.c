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

/* Based on newlib/libm/common/s_rint.c in Newlib.  */

#include "amdgcnmach.h"

static const double TWO52[2] = {
  4.50359962737049600000e+15, /* 0x43300000, 0x00000000 */
 -4.50359962737049600000e+15, /* 0xC3300000, 0x00000000 */
};

DEF_VD_MATH_FUNC (v64df, rint, v64df x)
{
  FUNCTION_INIT (v64df);

  v64si i0, i1;
  EXTRACT_WORDS (i0, i1, x);
  v64si sx = (i0 >> 31) & 1;
  v64df two52 = VECTOR_MERGE (VECTOR_INIT (TWO52[1]), VECTOR_INIT (TWO52[0]), sx != 0);
  v64si j0 = ((i0 >> 20) & 0x7ff) - 0x3ff;
  v64si i;
  VECTOR_IF (j0 < 20, cond)
    VECTOR_IF2 (j0 < 0, cond2, cond)
      VECTOR_RETURN (x, cond2 & (((i0 & 0x7fffffff) | i1) == 0));
      VECTOR_COND_MOVE (i1, i1 | (i0 & 0x0fffff), cond2);
      VECTOR_COND_MOVE (i0, i0 & 0xfffe0000, cond2);
      VECTOR_COND_MOVE (i0, i0 | (((i1 | -i1) >> 12) & 0x80000), cond2);
      SET_HIGH_WORD (x, i0, cond2);
      v64df w = two52 + x;
      v64df t = w - two52;
      GET_HIGH_WORD (i0, t, cond2);
      SET_HIGH_WORD (t, (i0&0x7fffffff)|(sx<<31), cond2);
      VECTOR_RETURN (t, cond2);
    VECTOR_ELSE2 (cond2, cond)
      i = (0x000fffff) >> j0;
      VECTOR_RETURN (x, cond2 & (((i0 & i) | i1) == 0));      /* x is integral */
      i >>= 1;
      VECTOR_IF2 (((i0 & i) | i1) != 0, cond3, cond2)
        VECTOR_COND_MOVE (i1, CAST_VECTOR(v64si, VECTOR_INIT (0x80000000)), cond3 & (j0 == 19));
        VECTOR_COND_MOVE (i1, VECTOR_INIT (0), cond3 & (j0 != 19));
	VECTOR_COND_MOVE (i0, (i0 & (~i)) | ((0x40000) >> j0), cond3);
      VECTOR_ENDIF
    VECTOR_ENDIF
  VECTOR_ELSEIF (j0 > 51, cond)
    VECTOR_RETURN (x + x, cond & (j0 == 0x400));
    VECTOR_RETURN (x, cond);
  VECTOR_ELSE (cond)
    i = CAST_VECTOR (v64si, VECTOR_INIT (0xffffffff) >> (j0 - 20));
    VECTOR_RETURN (x, cond & ((i1 & i) == 0));
    i >>= 1;
    VECTOR_COND_MOVE (i1, (i1 & (~i)) | (0x40000000 >> (j0 - 20)), cond & ((i1 & i) != 0));
  VECTOR_ENDIF
  INSERT_WORDS (x, i0, i1, NO_COND);
  v64df w = two52 + x;
  VECTOR_RETURN (w - two52, NO_COND);

  FUNCTION_RETURN;
}

DEF_VARIANTS (rint, df, df)
