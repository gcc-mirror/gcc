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

/* Based on newlib/libm/mathfp/ef_remainder.c in Newlib.  */

#include "amdgcnmach.h"

v64sf v64sf_fmodf_aux (v64sf, v64sf, v64si);

DEF_VS_MATH_FUNC (v64sf, remainderf, v64sf x, v64sf p)
{
  FUNCTION_INIT (v64sf);

  v64si hx;
  GET_FLOAT_WORD (hx, x, NO_COND);
  v64si hp;
  GET_FLOAT_WORD (hp, p, NO_COND);
  v64si sx = hx & 0x80000000;
  hp &= 0x7fffffff;
  hx &= 0x7fffffff;

  /* purge off exception values */
  /*if(hp==0)	 	// p = 0 */
  /*if((hx>=0x7f800000)||	// x not finite
    ((hp>0x7f800000)))		// p is NaN */
    VECTOR_RETURN ((x*p) / (x*p),
		   (hp == 0) | (hx >= 0x7f800000) | (hp > 0x7f800000));

  /* if (hp<=0x7effffff) 	// now x < 2p */
    VECTOR_COND_MOVE (x, v64sf_fmodf_aux (x, p+p, __mask), hp <= 0x7effffff);

  /*if ((hx-hp)==0) */
    VECTOR_RETURN (0.0f * x, (hx-hp) == 0);

  x = __builtin_gcn_fabsvf (x);
  p = __builtin_gcn_fabsvf (p);

  VECTOR_IF (hp < 0x01000000, cond)
    VECTOR_IF2 (x + x > p, cond2, cond)
      VECTOR_COND_MOVE (x, x - p, cond2);
      VECTOR_COND_MOVE (x, x - p, cond2 & (x + x >= p));
    VECTOR_ENDIF
  VECTOR_ELSE (cond)
    v64sf p_half = 0.5f * p;
    VECTOR_IF2 (x > p_half, cond2, cond)
      VECTOR_COND_MOVE (x, x - p, cond2);
      VECTOR_COND_MOVE (x, x - p, cond2 & (x >= p_half));
    VECTOR_ENDIF
  VECTOR_ENDIF

  GET_FLOAT_WORD (hx, x, NO_COND);
  SET_FLOAT_WORD (x, hx ^ sx, NO_COND);

  VECTOR_RETURN (x, NO_COND);

  FUNCTION_RETURN;
}

DEF_VARIANTS2 (remainderf, sf, sf)
