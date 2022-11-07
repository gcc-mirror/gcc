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

/* Based on newlib/libm/common/sf_rint.c in Newlib.  */

#include "amdgcnmach.h"

static const float TWO23[2] = {
  8.3886080000e+06, /* 0x4b000000 */
 -8.3886080000e+06, /* 0xcb000000 */
};

DEF_VS_MATH_FUNC (v64sf, rintf, v64sf x)
{
  FUNCTION_INIT (v64sf);

  v64si i0;
  GET_FLOAT_WORD (i0, x, NO_COND);
  v64si sx = (i0 >> 31) & 1;
  v64sf two23 = VECTOR_MERGE (TWO23[1] + x, TWO23[0] + x, sx != 0);
  v64si ix = (i0 & 0x7fffffff);
  v64si j0 = (ix >> 23) - 0x7f;
  VECTOR_IF (j0 < 23, cond)
    VECTOR_RETURN (x, cond & FLT_UWORD_IS_ZERO (ix));
    VECTOR_IF2 (j0 < 0, cond2, cond)
      v64si i1 = (i0 & 0x07fffff);
      VECTOR_COND_MOVE (i0, i0 & 0xfff00000, cond2);
      VECTOR_COND_MOVE (i0, i0 | (((i1 | -i1) >> 9) & 0x400000), cond2);
      SET_FLOAT_WORD (x, i0, cond2);
      v64sf w = two23 + x;
      v64sf t = w - two23;
      GET_FLOAT_WORD (i0, t, cond2);
      SET_FLOAT_WORD (t, (i0&0x7fffffff)|(sx<<31), cond2);
      VECTOR_RETURN (t, cond2);
    VECTOR_ELSE2 (cond2, cond)
      v64si i = (0x007fffff) >> j0;
      VECTOR_RETURN (x, cond2 & ((i0 & i) == 0));       /* x is integral */
      i >>= 1;
      VECTOR_COND_MOVE (i0, (i0 & (~i)) | (0x200000 >> j0),
                        cond2 & ((i0 & i) != 0));
    VECTOR_ENDIF
  VECTOR_ELSE (cond)
    VECTOR_RETURN (x + x, cond & ~FLT_UWORD_IS_FINITE (ix));    /* inf or NaN */
    VECTOR_RETURN (x, cond); /* x is integral */
  VECTOR_ENDIF

  SET_FLOAT_WORD (x, i0, NO_COND);
  v64sf w = two23 + x;
  VECTOR_RETURN (w - two23, NO_COND);

  FUNCTION_RETURN;
}

DEF_VARIANTS (rintf, sf, sf)
