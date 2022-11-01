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

static const double 
pio4  =  7.85398163397448278999e-01, /* 0x3FE921FB, 0x54442D18 */
pio4lo=  3.06161699786838301793e-17, /* 0x3C81A626, 0x33145C07 */
T[] =  {
  3.33333333333334091986e-01, /* 0x3FD55555, 0x55555563 */
  1.33333333333201242699e-01, /* 0x3FC11111, 0x1110FE7A */
  5.39682539762260521377e-02, /* 0x3FABA1BA, 0x1BB341FE */
  2.18694882948595424599e-02, /* 0x3F9664F4, 0x8406D637 */
  8.86323982359930005737e-03, /* 0x3F8226E3, 0xE96E8493 */
  3.59207910759131235356e-03, /* 0x3F6D6D22, 0xC9560328 */
  1.45620945432529025516e-03, /* 0x3F57DBC8, 0xFEE08315 */
  5.88041240820264096874e-04, /* 0x3F4344D8, 0xF2F26501 */
  2.46463134818469906812e-04, /* 0x3F3026F7, 0x1A8D1068 */
  7.81794442939557092300e-05, /* 0x3F147E88, 0xA03792A6 */
  7.14072491382608190305e-05, /* 0x3F12B80F, 0x32F0A7E9 */
 -1.85586374855275456654e-05, /* 0xBEF375CB, 0xDB605373 */
  2.59073051863633712884e-05, /* 0x3EFB2A70, 0x74BF7AD4 */
};

static v64df
v64df_kernel_tan (v64df x, v64df y, v64si iy, v64di __mask)
{
  FUNCTION_INIT (v64df);

  v64si hx;
  GET_HIGH_WORD(hx, x, NO_COND);
  v64si ix = hx & 0x7fffffff;	/* high word of |x| */

  VECTOR_IF (ix < 0x3e300000, cond) /* x < 2**-28 */
    VECTOR_IF2(__builtin_convertvector (x, v64si)==0, cond2, cond)			/* generate inexact */
      v64si low;
      GET_LOW_WORD (low, x, cond2);
      VECTOR_RETURN (1.0 / __builtin_gcn_fabsv (x), ((ix|low)|(iy+1))==0);
      VECTOR_RETURN (x, cond2 & (iy == 1));
      v64df z, w;
      z = w = x + y;
      SET_LOW_WORD (z, VECTOR_INIT (0.0), cond2);
      v64df v = y - (z - x);
      v64df t, a;
      t = a = -1.0 / w;
      SET_LOW_WORD(t, VECTOR_INIT (0.0), cond2);
      v64df s = 1.0 + t * z;
      VECTOR_RETURN ( t + a * (s + t * v), cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF
  VECTOR_IF(ix>=0x3FE59428, cond)			/* |x|>=0.6744 */
    VECTOR_COND_MOVE (x, -x, cond & (hx < 0));
    VECTOR_COND_MOVE (y, -y, cond & (hx < 0));
    v64df z = pio4-x;
    v64df w = pio4lo-y;
    VECTOR_COND_MOVE (x, z+w, cond);
    VECTOR_COND_MOVE (y, VECTOR_INIT (0.0), cond);
  VECTOR_ENDIF
  v64df z = x*x;
  v64df w = z*z;
  /* Break x^5*(T[1]+x^2*T[2]+...) into
    *	  x^5(T[1]+x^4*T[3]+...+x^20*T[11]) +
    *	  x^5(x^2*(T[2]+x^4*T[4]+...+x^22*[T12]))
    */
  v64df r = T[1]+w*(T[3]+w*(T[5]+w*(T[7]+w*(T[9]+w*T[11]))));
  v64df v = z*(T[2]+w*(T[4]+w*(T[6]+w*(T[8]+w*(T[10]+w*T[12])))));
  v64df s = z*x;
  r = y + z*(s*(r+v)+y);
  r += T[0]*s;
  w = x+r;
  VECTOR_IF(ix>=0x3FE59428, cond)
    v = __builtin_convertvector (iy, v64df);
    VECTOR_RETURN (__builtin_convertvector (1-((hx>>30)&2), v64df)
                   * (v-2.0*(x-(w*w/(w+v)-r))), cond);
  VECTOR_ENDIF
  VECTOR_RETURN (w, iy == 1);
  /* if allow error up to 2 ulp, 
     simply return -1.0/(x+r) here */
  /*  compute -1.0/(x+r) accurately */
  z = w;
  SET_LOW_WORD (z, VECTOR_INIT (0), NO_COND);
  v = r - (z - x); 	/* z+v = r+x */
  v64df a, t;
  t = a = -1.0/w;	/* a = -1.0/w */
  SET_LOW_WORD(t, VECTOR_INIT (0), NO_COND);
  s = 1.0+t*z;
  VECTOR_RETURN (t+a*(s+t*v), NO_COND);

  FUNCTION_RETURN;
}

static v64si
v64df_rem_pio2 (v64df x, v64df *y)
{
  v64df r = x * __INV_PI_OVER_TWO_2_24;
  v64si n = (__builtin_convertvector (r, v64si) + 0x800000) >> 24;
  x = x - __builtin_convertvector (n, v64df) * __PI_OVER_TWO;
  
  y[0] = x;
  y[1] = x - y[0];
  return n;
}

DEF_VD_MATH_FUNC (v64df, tan, v64df x)
{
  FUNCTION_INIT (v64df);

  v64si ix;
  GET_HIGH_WORD (ix, x, NO_COND);

  /* |x| ~< pi/4 */
  ix &= 0x7fffffff;
  VECTOR_RETURN (v64df_kernel_tan (x, VECTOR_INIT (0.0), VECTOR_INIT (1), __mask),
                 ix <= 0x3fe921fb);

  /* tan(Inf or NaN) is NaN */
  VECTOR_RETURN (x-x, ix >= 0x7ff00000);  /* NaN */

  /* argument reduction needed */
  v64df y[2];
  v64si n = v64df_rem_pio2 (x,y);
  VECTOR_RETURN (v64df_kernel_tan (y[0], y[1], 1-((n&1)<<1), __mask),  //  1 -- n even
                 NO_COND);                                             // -1 -- n odd

  FUNCTION_RETURN;
}

DEF_VARIANTS (tan, df, df)
