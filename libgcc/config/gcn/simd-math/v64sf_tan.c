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

/* Based on newlib/libm/math/kf_tan.c in Newlib.  */

#include "amdgcnmach.h"

static const float 
one   =  1.0000000000e+00, /* 0x3f800000 */
pio4  =  7.8539812565e-01, /* 0x3f490fda */
pio4lo=  3.7748947079e-08, /* 0x33222168 */
T[] =  {
  3.3333334327e-01, /* 0x3eaaaaab */
  1.3333334029e-01, /* 0x3e088889 */
  5.3968254477e-02, /* 0x3d5d0dd1 */
  2.1869488060e-02, /* 0x3cb327a4 */
  8.8632395491e-03, /* 0x3c11371f */
  3.5920790397e-03, /* 0x3b6b6916 */
  1.4562094584e-03, /* 0x3abede48 */
  5.8804126456e-04, /* 0x3a1a26c8 */
  2.4646313977e-04, /* 0x398137b9 */
  7.8179444245e-05, /* 0x38a3f445 */
  7.1407252108e-05, /* 0x3895c07a */
 -1.8558637748e-05, /* 0xb79bae5f */
  2.5907305826e-05, /* 0x37d95384 */
};

static v64sf
v64sf_kernel_tanf (v64sf x, v64sf y, v64si iy, v64si __mask)
{
  FUNCTION_INIT (v64sf);

  v64si hx;
  GET_FLOAT_WORD(hx, x, NO_COND);
  v64si ix = hx & 0x7fffffff;	/* high word of |x| */

  VECTOR_IF(ix<0x31800000, cond)			/* x < 2**-28 */
    VECTOR_IF2(__builtin_convertvector (x, v64si)==0, cond2, cond)			/* generate inexact */
      VECTOR_RETURN (1.0f / __builtin_gcn_fabsvf (x), (ix|(iy+1))==0);
      VECTOR_RETURN (x, cond2 & (iy == 1));
      VECTOR_RETURN (-1.0f / x, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF
  VECTOR_IF(ix>=0x3f2ca140, cond)			/* |x|>=0.6744 */
    VECTOR_COND_MOVE (x, -x, cond & (hx < 0));
    VECTOR_COND_MOVE (y, -y, cond & (hx < 0));
    v64sf z = pio4-x;
    v64sf w = pio4lo-y;
    VECTOR_COND_MOVE (x, z+w, cond);
    VECTOR_COND_MOVE (y, VECTOR_INIT (0.0f), cond);
  VECTOR_ENDIF
  v64sf z	= x*x;
  v64sf w = z*z;
  /* Break x^5*(T[1]+x^2*T[2]+...) into
    *	  x^5(T[1]+x^4*T[3]+...+x^20*T[11]) +
    *	  x^5(x^2*(T[2]+x^4*T[4]+...+x^22*[T12]))
    */
  v64sf r = T[1]+w*(T[3]+w*(T[5]+w*(T[7]+w*(T[9]+w*T[11]))));
  v64sf v = z*(T[2]+w*(T[4]+w*(T[6]+w*(T[8]+w*(T[10]+w*T[12])))));
  v64sf s = z*x;
  r = y + z*(s*(r+v)+y);
  r += T[0]*s;
  w = x+r;
  VECTOR_IF(ix>=0x3f2ca140, cond)
    v = __builtin_convertvector (iy, v64sf);
    VECTOR_RETURN (__builtin_convertvector (1-((hx>>30)&2), v64sf)
                   * (v-2.0f*(x-(w*w/(w+v)-r))), cond);
  VECTOR_ENDIF
  VECTOR_RETURN (w, iy == 1);
  /* if allow error up to 2 ulp, 
     simply return -1.0/(x+r) here */
  /*  compute -1.0/(x+r) accurately */
  z = w;
  v64si i;
  GET_FLOAT_WORD(i,z, NO_COND);
  SET_FLOAT_WORD(z,i&0xfffff000, NO_COND);
  v  = r - (z - x); 	/* z+v = r+x */
  v64sf a, t;
  t = a  = -1.0f/w;	/* a = -1.0/w */
  GET_FLOAT_WORD(i,t, NO_COND);
  SET_FLOAT_WORD(t,i&0xfffff000, NO_COND);
  s  = 1.0f+t*z;
  VECTOR_RETURN (t+a*(s+t*v), NO_COND);

  FUNCTION_RETURN;
}

static v64si
v64sf_rem_pio2f (v64sf x, v64sf *y)
{
  /* Work in double-precision for better accuracy.  */
  v64df dx = __builtin_convertvector (x, v64df);
  v64df r = dx * __INV_PI_OVER_TWO_2_24;
  v64si n = (__builtin_convertvector (r, v64si) + 0x800000) >> 24;
  dx = dx - __builtin_convertvector (n, v64df) * __PI_OVER_TWO;
  
  y[0] = __builtin_convertvector (dx, v64sf);
  y[1] = __builtin_convertvector (dx, v64sf) - y[0];
  return n;
}

DEF_VS_MATH_FUNC (v64sf, tanf, v64sf x)
{
  FUNCTION_INIT (v64sf);

  v64si ix;
  GET_FLOAT_WORD (ix, x, NO_COND);

  /* |x| ~< pi/4 */
  ix &= 0x7fffffff;
  VECTOR_RETURN (v64sf_kernel_tanf (x, VECTOR_INIT (0.0f), VECTOR_INIT (1), __mask),
                 ix <= 0x3f490fda);

  /* tan(Inf or NaN) is NaN */
  VECTOR_RETURN (x-x, ~FLT_UWORD_IS_FINITE(ix));  /* NaN */

  /* argument reduction needed */
  v64sf y[2];
  v64si n = v64sf_rem_pio2f (x,y);
  VECTOR_RETURN (v64sf_kernel_tanf (y[0], y[1], 1-((n&1)<<1), __mask),  //  1 -- n even
                 NO_COND);                                              // -1 -- n odd

  FUNCTION_RETURN;
}

DEF_VARIANTS (tanf, sf, sf)
