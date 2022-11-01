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

/* Based on newlib/libm/common/s_scalbn.c in Newlib.  */

#include "amdgcnmach.h"
#include <limits.h>
#include <float.h>

static const double
two54   =  1.80143985094819840000e+16, /* 0x43500000, 0x00000000 */
twom54  =  5.55111512312578270212e-17, /* 0x3C900000, 0x00000000 */
huge   = 1.0e+300,
tiny   = 1.0e-300;

v64df v64df_copysign_aux (v64df, v64df, v64di);

DEF_VD_MATH_FUNC (v64df, scalbn, v64df x, v64si n)
{
  FUNCTION_INIT (v64df);

  const v64df huge_v = VECTOR_INIT ((double) huge);
  const v64df tiny_v = VECTOR_INIT ((double) tiny);

  v64si hx, lx;
  EXTRACT_WORDS (hx, lx, x);
  v64si k =(hx&0x7ff00000)>>20;                 /* extract exponent */
  VECTOR_IF (k == 0, cond)                      /* 0 or subnormal x */
    VECTOR_RETURN (x, cond & ((lx|(hx&0x7fffffff))==0)); /* +- 0 */
    VECTOR_COND_MOVE (x, x * two54, cond);
    GET_HIGH_WORD (hx, x, cond);
    VECTOR_COND_MOVE (k, ((hx&0x7ff00000)>>20) - 54, cond);
    VECTOR_RETURN (tiny*x, cond & (n < -50000));  /*underflow*/
  VECTOR_ENDIF
  VECTOR_RETURN (x+x, k == 0x7ff);             /* NaN or Inf */
  /* in case integer overflow in n+k */
  VECTOR_RETURN (huge_v * v64df_copysign_aux (huge_v, x, __mask), n > 50000);
  k = k + n;
  VECTOR_RETURN (huge_v * v64df_copysign_aux (huge_v, x, __mask), k > 0x7fe);
  VECTOR_IF (k > 0, cond)			/* normal result */
    SET_HIGH_WORD (x, (hx&0x800fffff)|(k<<20), cond);
    VECTOR_RETURN (x, cond);
  VECTOR_ENDIF
  VECTOR_RETURN (tiny_v * v64df_copysign_aux (tiny_v, x, __mask), k <= -54);   /*underflow*/
  k += 54;				/* subnormal result */
  SET_HIGH_WORD (x, (hx&0x800fffff)|(k<<20), NO_COND);
  VECTOR_RETURN (x * twom54, NO_COND);

  FUNCTION_RETURN;
}
