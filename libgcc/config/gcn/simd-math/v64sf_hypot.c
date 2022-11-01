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

/* Based on newlib/libm/mathfp/ef_hypot.c in Newlib.  */

#include "amdgcnmach.h"

v64sf v64sf_sqrtf_aux (v64sf, v64si);

DEF_VS_MATH_FUNC (v64sf, hypotf, v64sf x, v64sf y)
{
  FUNCTION_INIT (v64sf);

  v64sf a = x;
  v64sf b = y;

  v64si ha;
  GET_FLOAT_WORD (ha, x, NO_COND);
  ha &= 0x7fffffffL;
  v64si hb;
  GET_FLOAT_WORD (hb, y, NO_COND);
  hb &= 0x7fffffffL;

  VECTOR_IF (hb > ha, cond)
    v64si j = ha;
    VECTOR_COND_MOVE (ha, hb, cond);
    VECTOR_COND_MOVE (hb, j, cond);
  VECTOR_ENDIF
  SET_FLOAT_WORD (a, ha, NO_COND);	/* a <- |a| */
  SET_FLOAT_WORD (b, hb, NO_COND);	/* b <- |b| */
  VECTOR_IF((ha - hb) > 0xf000000L, cond)	// x/y > 2**30 */
    VECTOR_RETURN (a + b, cond);
  VECTOR_ENDIF

  v64si k = VECTOR_INIT (0);

  VECTOR_IF (ha > 0x58800000L, cond)		/* a>2**50 */
    VECTOR_IF2 (ha >= 0x7f800000L, cond2, cond)	/* Inf or NaN */
      v64sf w = a + b;			// for sNaN */
      VECTOR_COND_MOVE (w, a, cond2 & (ha == 0x7f800000));
      VECTOR_COND_MOVE (w, b, cond2 & (hb == 0x7f800000));
      VECTOR_RETURN (w, cond);
    VECTOR_ENDIF
    /* scale a and b by 2**-60 */
    VECTOR_COND_MOVE (ha, ha - 0x5d800000, cond);
    VECTOR_COND_MOVE (hb, hb - 0x5d800000, cond);
    VECTOR_COND_MOVE (k, k + 60, cond);
    SET_FLOAT_WORD (a, ha, cond);
    SET_FLOAT_WORD (b, hb, cond);
  VECTOR_ENDIF
  VECTOR_IF (hb < 0x26800000, cond)		/* b < 2**-50 */
    VECTOR_IF2 (hb <= 0x007fffff, cond2, cond)	/* subnormal b or 0 */
      VECTOR_RETURN (a, cond2 & (hb == 0));
      /* t1=2^126 */
      v64sf t1;
      SET_FLOAT_WORD (t1, VECTOR_INIT (0x3f000000), cond2);
      VECTOR_COND_MOVE (b, b * t1, cond2);
      VECTOR_COND_MOVE (a, a * t1, cond2);
      VECTOR_COND_MOVE (k, k - 126, cond2);
    VECTOR_ELSE2 (cond2, cond)		/* scale a and b by 2^60 */
      VECTOR_COND_MOVE (ha, ha + 0x5d800000, cond2);	/* a *= 2^60 */
      VECTOR_COND_MOVE (hb, hb + 0x5d800000, cond2);	/* b *= 2^60 */
      VECTOR_COND_MOVE (k, k - 60, cond2);
      SET_FLOAT_WORD (a, ha, cond2);
      SET_FLOAT_WORD (b, hb, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF
  /* medium size a and b */
  v64sf w = a - b;
  VECTOR_IF (w > b, cond)
    v64sf t1;
    SET_FLOAT_WORD (t1, ha & 0xfffff000, cond);
    v64sf t2 = a - t1;
    VECTOR_COND_MOVE (w, v64sf_sqrtf_aux (t1*t1 - (b*(-b) - t2 * (a + t1)), __mask), cond);
  VECTOR_ELSE (cond)
    VECTOR_COND_MOVE (a, a+a, cond);
    v64sf y1;
    SET_FLOAT_WORD (y1, hb & 0xfffff000, cond);
    v64sf y2 = b - y1;
    v64sf t1;
    SET_FLOAT_WORD (t1, ha + 0x00800000, cond);
    v64sf t2 = a - t1;
    VECTOR_COND_MOVE (w, v64sf_sqrtf_aux (t1*y1 - (w*(-w) - (t1*y2 + t2*b)), __mask), cond);
  VECTOR_ENDIF
  VECTOR_IF (k != 0, cond)
    v64sf t1;
    SET_FLOAT_WORD (t1, 0x3f800000 + (k << 23), cond);
    VECTOR_RETURN (t1 * w, cond);
  VECTOR_ELSE (cond)
    VECTOR_RETURN (w, cond);
  VECTOR_ENDIF

  FUNCTION_RETURN;
}

DEF_VARIANTS2 (hypotf, sf, sf)
