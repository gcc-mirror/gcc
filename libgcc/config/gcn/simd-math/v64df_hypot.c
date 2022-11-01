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

/* Based on newlib/libm/mathfp/e_hypot.c in Newlib.  */

#include "amdgcnmach.h"

v64df v64df_sqrt_aux (v64df, v64di);

DEF_VD_MATH_FUNC (v64df, hypot, v64df x, v64df y)
{
  FUNCTION_INIT (v64df);

  v64df a = x;
  v64df b = y;

  v64si ha;
  GET_HIGH_WORD (ha, x, NO_COND);
  ha &= 0x7fffffffL;
  v64si hb;
  GET_HIGH_WORD (hb, y, NO_COND);
  hb &= 0x7fffffffL;

  VECTOR_IF (hb > ha, cond)
    VECTOR_COND_MOVE (a, y, cond);
    VECTOR_COND_MOVE (b, x, cond);
    v64si j = ha;
    VECTOR_COND_MOVE (ha, hb, cond);
    VECTOR_COND_MOVE (hb, j, cond);
  VECTOR_ENDIF
  SET_HIGH_WORD (a, ha, NO_COND);	/* a <- |a| */
  SET_HIGH_WORD (b, hb, NO_COND);	/* b <- |b| */
  VECTOR_IF((ha - hb) > 0x3c00000L, cond)	// x/y > 2**60 */
    VECTOR_RETURN (a + b, cond);
  VECTOR_ENDIF

  v64si k = VECTOR_INIT (0);

  VECTOR_IF (ha > 0x5f300000L, cond)		/* a>2**500 */
    VECTOR_IF2 (ha >= 0x7ff00000L, cond2, cond)	/* Inf or NaN */
      v64df w = a + b;			// for sNaN */
      v64si low;
      GET_LOW_WORD (low, a, cond2);
      VECTOR_COND_MOVE (w, a, cond2 & (((ha & 0xfffff) | low) == 0));
      GET_LOW_WORD (low, b, cond2);
      VECTOR_COND_MOVE (w, b, cond2 & (((hb & 0xfffff) | low) == 0));
      VECTOR_RETURN (w, cond);
    VECTOR_ENDIF
    /* scale a and b by 2**-600 */
    VECTOR_COND_MOVE (ha, ha - 0x25800000, cond);
    VECTOR_COND_MOVE (hb, hb - 0x25800000, cond);
    VECTOR_COND_MOVE (k, k + 600, cond);
    SET_HIGH_WORD (a, ha, cond);
    SET_HIGH_WORD (b, hb, cond);
  VECTOR_ENDIF
  VECTOR_IF (hb < 0x20b00000, cond)		/* b < 2**-500 */
    VECTOR_IF2 (hb <= 0x000fffff, cond2, cond)	/* subnormal b or 0 */
      v64si low;
      GET_LOW_WORD (low, b, cond);
      VECTOR_RETURN (a, cond2 & ((hb | low) == 0));
      /* t1=2^1022 */
      v64df t1 = VECTOR_INIT (0.0);
      SET_HIGH_WORD (t1, VECTOR_INIT (0x7fd00000), cond2);
      VECTOR_COND_MOVE (b, b * t1, cond2);
      VECTOR_COND_MOVE (a, a * t1, cond2);
      VECTOR_COND_MOVE (k, k - 1022, cond2);
    VECTOR_ELSE2 (cond2, cond)		/* scale a and b by 2^600 */
      VECTOR_COND_MOVE (ha, ha + 0x25800000, cond2);	/* a *= 2^600 */
      VECTOR_COND_MOVE (hb, hb + 0x25800000, cond2);	/* b *= 2^600 */
      VECTOR_COND_MOVE (k, k - 600, cond2);
      SET_HIGH_WORD (a, ha, cond2);
      SET_HIGH_WORD (b, hb, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF
  /* medium size a and b */
  v64df w = a - b;
  VECTOR_IF (w > b, cond)
    v64df t1 = VECTOR_INIT (0.0);
    SET_HIGH_WORD (t1, ha, cond);
    v64df t2 = a - t1;
    VECTOR_COND_MOVE (w, v64df_sqrt_aux (t1*t1 - (b*(-b) - t2 * (a + t1)), __mask), cond);
  VECTOR_ELSE (cond)
    VECTOR_COND_MOVE (a, a+a, cond);
    v64df y1 = VECTOR_INIT (0.0);
    SET_HIGH_WORD (y1, hb, cond);
    v64df y2 = b - y1;
    v64df t1;
    SET_HIGH_WORD (t1, ha + 0x00100000, cond);
    v64df t2 = a - t1;
    VECTOR_COND_MOVE (w, v64df_sqrt_aux (t1*y1 - (w*(-w) - (t1*y2 + t2*b)), __mask), cond);
  VECTOR_ENDIF
  VECTOR_IF (k != 0, cond)
    v64si high;
    v64df t1 = VECTOR_INIT (1.0);
    GET_HIGH_WORD (high, t1, cond);
    SET_HIGH_WORD (t1, high + (k << 20), cond);
    VECTOR_RETURN (t1 * w, cond);
  VECTOR_ELSE (cond)
    VECTOR_RETURN (w, cond);
  VECTOR_ENDIF

  FUNCTION_RETURN;
}

DEF_VARIANTS2 (hypot, df, df)
