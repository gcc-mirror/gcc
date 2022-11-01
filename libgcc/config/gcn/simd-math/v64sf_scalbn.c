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

/* Based on newlib/libm/common/sf_scalbn.c in Newlib.  */

#include "amdgcnmach.h"
#include <limits.h>
#include <float.h>

#if INT_MAX > 50000
#define OVERFLOW_INT 50000
#else
#define OVERFLOW_INT 30000
#endif

static const float
two25   =  3.355443200e+07,	/* 0x4c000000 */
twom25  =  2.9802322388e-08,	/* 0x33000000 */
huge   = 1.0e+30,
tiny   = 1.0e-30;

v64sf v64sf_copysignf_aux (v64sf, v64sf, v64si);

DEF_VS_MATH_FUNC (v64sf, scalbnf, v64sf x, v64si n)
{
  FUNCTION_INIT (v64sf);

  const v64sf huge_v = VECTOR_INIT ((float) huge);
  const v64sf tiny_v = VECTOR_INIT ((float) tiny);

  v64si ix;
  GET_FLOAT_WORD (ix, x, NO_COND);
  v64si hx = ix & 0x7fffffff;
  v64si k = hx >> 23;		/* extract exponent */
  VECTOR_IF (FLT_UWORD_IS_ZERO(hx), cond)
    VECTOR_RETURN (x, cond);
  VECTOR_ENDIF
  VECTOR_IF (~FLT_UWORD_IS_FINITE(hx), cond)	/* NaN or Inf */
    VECTOR_RETURN (x + x, cond);
  VECTOR_ENDIF
  VECTOR_IF (FLT_UWORD_IS_SUBNORMAL (hx), cond);
    VECTOR_COND_MOVE (x, x * two25, cond);
    GET_FLOAT_WORD (ix, x, cond);
    VECTOR_COND_MOVE (k, ((ix & 0x7f800000) >> 23) - 25, cond);
    VECTOR_IF2 (n < -50000, cond2, cond)	/*underflow*/
      VECTOR_RETURN (tiny * x, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF
  VECTOR_IF (n > OVERFLOW_INT, cond) 	// in case integer overflow in n+k
    VECTOR_RETURN (huge_v * v64sf_copysignf_aux (huge_v, x, __mask), cond);
  VECTOR_ENDIF
  k = k + n;
  VECTOR_IF (k > FLT_LARGEST_EXP, cond) 	/* overflow */
    VECTOR_RETURN (huge_v * v64sf_copysignf_aux (huge_v, x, __mask), cond);
  VECTOR_ENDIF
  VECTOR_IF (k > 0, cond)			/* normal result */
    SET_FLOAT_WORD (x, (ix & 0x807fffff) | (k << 23), cond);
    VECTOR_RETURN (x, cond);
  VECTOR_ENDIF
  VECTOR_IF (k < FLT_SMALLEST_EXP, cond)	/*underflow*/
    VECTOR_RETURN (tiny_v * v64sf_copysignf_aux (tiny_v, x, __mask),
		   k < FLT_SMALLEST_EXP);
  VECTOR_ENDIF

  k += 25;				/* subnormal result */
  SET_FLOAT_WORD (x, (ix & 0x807fffff) | (k << 23), NO_COND);
  VECTOR_RETURN (x * twom25, NO_COND);

  FUNCTION_RETURN;
}
