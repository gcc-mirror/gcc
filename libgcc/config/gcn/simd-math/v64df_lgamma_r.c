/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 *
 */

/* Based on newlib/libm/math/er_lgamma.c in Newlib.  */

#include "amdgcnmach.h"

static const double two52=  4.50359962737049600000e+15, /* 0x43300000, 0x00000000 */
half=  5.00000000000000000000e-01, /* 0x3FE00000, 0x00000000 */
one =  1.00000000000000000000e+00, /* 0x3FF00000, 0x00000000 */
pi  =  3.14159265358979311600e+00, /* 0x400921FB, 0x54442D18 */
a0  =  7.72156649015328655494e-02, /* 0x3FB3C467, 0xE37DB0C8 */
a1  =  3.22467033424113591611e-01, /* 0x3FD4A34C, 0xC4A60FAD */
a2  =  6.73523010531292681824e-02, /* 0x3FB13E00, 0x1A5562A7 */
a3  =  2.05808084325167332806e-02, /* 0x3F951322, 0xAC92547B */
a4  =  7.38555086081402883957e-03, /* 0x3F7E404F, 0xB68FEFE8 */
a5  =  2.89051383673415629091e-03, /* 0x3F67ADD8, 0xCCB7926B */
a6  =  1.19270763183362067845e-03, /* 0x3F538A94, 0x116F3F5D */
a7  =  5.10069792153511336608e-04, /* 0x3F40B6C6, 0x89B99C00 */
a8  =  2.20862790713908385557e-04, /* 0x3F2CF2EC, 0xED10E54D */
a9  =  1.08011567247583939954e-04, /* 0x3F1C5088, 0x987DFB07 */
a10 =  2.52144565451257326939e-05, /* 0x3EFA7074, 0x428CFA52 */
a11 =  4.48640949618915160150e-05, /* 0x3F07858E, 0x90A45837 */
tc  =  1.46163214496836224576e+00, /* 0x3FF762D8, 0x6356BE3F */
tf  = -1.21486290535849611461e-01, /* 0xBFBF19B9, 0xBCC38A42 */
/* tt = -(tail of tf) */
tt  = -3.63867699703950536541e-18, /* 0xBC50C7CA, 0xA48A971F */
t0  =  4.83836122723810047042e-01, /* 0x3FDEF72B, 0xC8EE38A2 */
t1  = -1.47587722994593911752e-01, /* 0xBFC2E427, 0x8DC6C509 */
t2  =  6.46249402391333854778e-02, /* 0x3FB08B42, 0x94D5419B */
t3  = -3.27885410759859649565e-02, /* 0xBFA0C9A8, 0xDF35B713 */
t4  =  1.79706750811820387126e-02, /* 0x3F9266E7, 0x970AF9EC */
t5  = -1.03142241298341437450e-02, /* 0xBF851F9F, 0xBA91EC6A */
t6  =  6.10053870246291332635e-03, /* 0x3F78FCE0, 0xE370E344 */
t7  = -3.68452016781138256760e-03, /* 0xBF6E2EFF, 0xB3E914D7 */
t8  =  2.25964780900612472250e-03, /* 0x3F6282D3, 0x2E15C915 */
t9  = -1.40346469989232843813e-03, /* 0xBF56FE8E, 0xBF2D1AF1 */
t10 =  8.81081882437654011382e-04, /* 0x3F4CDF0C, 0xEF61A8E9 */
t11 = -5.38595305356740546715e-04, /* 0xBF41A610, 0x9C73E0EC */
t12 =  3.15632070903625950361e-04, /* 0x3F34AF6D, 0x6C0EBBF7 */
t13 = -3.12754168375120860518e-04, /* 0xBF347F24, 0xECC38C38 */
t14 =  3.35529192635519073543e-04, /* 0x3F35FD3E, 0xE8C2D3F4 */
u0  = -7.72156649015328655494e-02, /* 0xBFB3C467, 0xE37DB0C8 */
u1  =  6.32827064025093366517e-01, /* 0x3FE4401E, 0x8B005DFF */
u2  =  1.45492250137234768737e+00, /* 0x3FF7475C, 0xD119BD6F */
u3  =  9.77717527963372745603e-01, /* 0x3FEF4976, 0x44EA8450 */
u4  =  2.28963728064692451092e-01, /* 0x3FCD4EAE, 0xF6010924 */
u5  =  1.33810918536787660377e-02, /* 0x3F8B678B, 0xBF2BAB09 */
v1  =  2.45597793713041134822e+00, /* 0x4003A5D7, 0xC2BD619C */
v2  =  2.12848976379893395361e+00, /* 0x40010725, 0xA42B18F5 */
v3  =  7.69285150456672783825e-01, /* 0x3FE89DFB, 0xE45050AF */
v4  =  1.04222645593369134254e-01, /* 0x3FBAAE55, 0xD6537C88 */
v5  =  3.21709242282423911810e-03, /* 0x3F6A5ABB, 0x57D0CF61 */
s0  = -7.72156649015328655494e-02, /* 0xBFB3C467, 0xE37DB0C8 */
s1  =  2.14982415960608852501e-01, /* 0x3FCB848B, 0x36E20878 */
s2  =  3.25778796408930981787e-01, /* 0x3FD4D98F, 0x4F139F59 */
s3  =  1.46350472652464452805e-01, /* 0x3FC2BB9C, 0xBEE5F2F7 */
s4  =  2.66422703033638609560e-02, /* 0x3F9B481C, 0x7E939961 */
s5  =  1.84028451407337715652e-03, /* 0x3F5E26B6, 0x7368F239 */
s6  =  3.19475326584100867617e-05, /* 0x3F00BFEC, 0xDD17E945 */
r1  =  1.39200533467621045958e+00, /* 0x3FF645A7, 0x62C4AB74 */
r2  =  7.21935547567138069525e-01, /* 0x3FE71A18, 0x93D3DCDC */
r3  =  1.71933865632803078993e-01, /* 0x3FC601ED, 0xCCFBDF27 */
r4  =  1.86459191715652901344e-02, /* 0x3F9317EA, 0x742ED475 */
r5  =  7.77942496381893596434e-04, /* 0x3F497DDA, 0xCA41A95B */
r6  =  7.32668430744625636189e-06, /* 0x3EDEBAF7, 0xA5B38140 */
w0  =  4.18938533204672725052e-01, /* 0x3FDACFE3, 0x90C97D69 */
w1  =  8.33333333333329678849e-02, /* 0x3FB55555, 0x5555553B */
w2  = -2.77777777728775536470e-03, /* 0xBF66C16C, 0x16B02E5C */
w3  =  7.93650558643019558500e-04, /* 0x3F4A019F, 0x98CF38B6 */
w4  = -5.95187557450339963135e-04, /* 0xBF4380CB, 0x8C0FE741 */
w5  =  8.36339918996282139126e-04, /* 0x3F4B67BA, 0x4CDAD5D1 */
w6  = -1.63092934096575273989e-03; /* 0xBF5AB89D, 0x0B9E43E4 */

static const double zero=  0.00000000000000000000e+00;

v64df v64df_cos_aux (v64df x, v64di __mask);
v64df v64df_log_aux (v64df x, v64di __mask);
v64df v64df_sin_aux (v64df x, v64di __mask);

static v64df
v64df_sin_pi (v64df x)
{
  // Explicitly create mask for internal function.
  v64di __mask = VECTOR_INIT (-1L);
  FUNCTION_INIT (v64df);

  v64df y, z;
  v64si n, ix;

  GET_HIGH_WORD (ix, x, NO_COND);
  ix &= 0x7fffffff;

  VECTOR_IF (ix < 0x3fd00000, cond)
    VECTOR_RETURN (v64df_sin_aux (pi * x, __mask), cond);
  VECTOR_ENDIF
  y = -x; /* x is assume negative */

  /*
   * argument reduction, make sure inexact flag not raised if input
   * is an integer
   */
  z = __builtin_gcn_floorv (y);
  VECTOR_IF (z != y, cond)
    /* inexact anyway */
    VECTOR_COND_MOVE(y, y * 0.5, cond);
    VECTOR_COND_MOVE(y, 2.0 * (y - __builtin_gcn_floorv (y)), cond); /* y = |x| mod 2.0 */
    VECTOR_COND_MOVE(n, __builtin_convertvector(y * 4.0, v64si), cond);
  VECTOR_ELSE (cond)
    VECTOR_IF2 (__builtin_convertvector(ix >= 0x43400000, v64di), cond2, cond)
      VECTOR_COND_MOVE(y, VECTOR_INIT(zero), cond2);
      VECTOR_COND_MOVE(n, VECTOR_INIT(0), cond2); /* y must be even */
    VECTOR_ELSE2 (cond2, cond)
      VECTOR_COND_MOVE(z, y + two52 /* exact */, cond2 & __builtin_convertvector(ix < 0x43300000, v64di));
      GET_LOW_WORD (n, z, cond2);
      VECTOR_COND_MOVE(n, n & 1, cond2);
      VECTOR_COND_MOVE(y, __builtin_convertvector(n, v64df), cond2);
      VECTOR_COND_MOVE(n, n << 2, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF
    
  VECTOR_IF (n == 0, cond)
    VECTOR_COND_MOVE(y, v64df_sin_aux (pi * y, __mask), cond);
  VECTOR_ELSEIF (n == 1 | n == 2, cond)
    VECTOR_COND_MOVE(y, v64df_cos_aux (pi * (0.5 - y), __mask), cond);
  VECTOR_ELSEIF (n == 3 | n == 4, cond)
    VECTOR_COND_MOVE(y, v64df_sin_aux (pi * (VECTOR_INIT(one) - y), __mask), cond);
  VECTOR_ELSEIF (n == 5 | n == 6, cond)
    VECTOR_COND_MOVE(y, -v64df_cos_aux (pi * (y - 1.5), __mask), cond);
  VECTOR_ELSE (cond)
    VECTOR_COND_MOVE(y, v64df_sin_aux (pi * (y - 2.0), __mask), cond);
  VECTOR_ENDIF

  VECTOR_RETURN(-y, NO_COND);
  FUNCTION_RETURN;
}

DEF_VD_MATH_FUNC (v64df, lgamma_r, v64df x, v64si *signgamp)
{
  FUNCTION_INIT (v64df);

  v64df t,y,z,nadj = VECTOR_INIT(0.0),p,p1,p2,p3,q,r,w;
  v64si i,hx,lx,ix;

  EXTRACT_WORDS(hx,lx,x);

  /* purge off +-inf, NaN, +-0, and negative arguments */
  *signgamp = VECTOR_INIT(1);
  ix = hx&0x7fffffff;
  VECTOR_IF(ix>=0x7ff00000, cond)
    VECTOR_RETURN (x*x, cond);
  VECTOR_ENDIF
  VECTOR_IF((ix|lx)==0, cond)
    VECTOR_COND_MOVE(*signgamp, VECTOR_INIT(-1), cond & (hx<0));
    VECTOR_RETURN(one/(x-x), cond);
  VECTOR_ENDIF
  VECTOR_IF (ix < 0x3b900000, cond) /* |x|<2**-70, return -log(|x|) */
    VECTOR_IF2(hx<0, cond2, cond)
      VECTOR_COND_MOVE(*signgamp, VECTOR_INIT(-1), cond);
      VECTOR_RETURN (-v64df_log_aux(-x, __mask), cond2);
    VECTOR_ELSE2(cond2, cond)
      VECTOR_RETURN (-v64df_log_aux(x, __mask), cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF
  VECTOR_IF (hx < 0, cond)
    VECTOR_IF2(ix>=0x43300000, cond2, cond) /* |x|>=2**52, must be -integer */
      VECTOR_RETURN(one/(x-x), cond2); /* -integer */
    VECTOR_ENDIF
    VECTOR_COND_MOVE (t, v64df_sin_pi (x), cond);
    VECTOR_IF2(__builtin_convertvector(t==zero, v64si), cond2, cond)
      VECTOR_RETURN(one/(x-x), cond2); /* -integer */
    VECTOR_ENDIF
    VECTOR_COND_MOVE(nadj, v64df_log_aux(VECTOR_INIT(pi)/__builtin_gcn_fabsv(t*x), __mask), cond);
    VECTOR_COND_MOVE(*signgamp, VECTOR_INIT(-1), cond & __builtin_convertvector(t < zero, v64si));
    VECTOR_COND_MOVE(x, -x, cond);
  VECTOR_ENDIF
        
  /* purge off 1 and 2 */
  VECTOR_IF((((ix-0x3ff00000)|lx)==0)|(((ix-0x40000000)|lx)==0), cond)
    VECTOR_COND_MOVE(r, VECTOR_INIT(0.0), cond);
    /* for x < 2.0 */
  VECTOR_ELSEIF(ix<0x40000000, cond)
    VECTOR_IF2(ix<=0x3feccccc, cond2, cond)
      /* lgamma(x) = lgamma(x+1)-log(x) */
      r = -v64df_log_aux(x, __mask);
      VECTOR_IF2(ix>=0x3FE76944, cond3, cond2)
        VECTOR_COND_MOVE(y, one-x, cond3);
        VECTOR_COND_MOVE(i, VECTOR_INIT(0), cond3);
      VECTOR_ELSEIF2(ix>=0x3FCDA661, cond3, cond2)
        VECTOR_COND_MOVE(y, x-(tc-one), cond3);
        VECTOR_COND_MOVE(i, VECTOR_INIT(1), cond3);
      VECTOR_ELSE2(cond3, cond2)
        VECTOR_COND_MOVE(y, x, cond3);
        VECTOR_COND_MOVE(i, VECTOR_INIT(2), cond3);
      VECTOR_ENDIF
    VECTOR_ELSE2(cond2, cond)
      VECTOR_COND_MOVE(r, VECTOR_INIT(zero), cond2);
      VECTOR_IF2(ix>=0x3FFBB4C3, cond3, cond2) /* [1.7316,2] */
        VECTOR_COND_MOVE(y, VECTOR_INIT(2.0)-x, cond3);
        VECTOR_COND_MOVE(i, VECTOR_INIT(0), cond3);
      VECTOR_ELSEIF2(ix>=0x3FF3B4C4, cond3, cond2) /* [1.23,1.73] */
        VECTOR_COND_MOVE(y, x-tc, cond3);
        VECTOR_COND_MOVE(i, VECTOR_INIT(1), cond3);
      VECTOR_ELSE2(cond3, cond2)
        VECTOR_COND_MOVE(y, x-one, cond3);
        VECTOR_COND_MOVE(i, VECTOR_INIT(2), cond3);
      VECTOR_ENDIF
    VECTOR_ENDIF

    VECTOR_IF2(i==0, cond2, cond)
      VECTOR_COND_MOVE(z, y*y, cond2);
      VECTOR_COND_MOVE(p1, a0+z*(a2+z*(a4+z*(a6+z*(a8+z*a10)))), cond2);
      VECTOR_COND_MOVE(p2, z*(a1+z*(a3+z*(a5+z*(a7+z*(a9+z*a11))))), cond2);
      VECTOR_COND_MOVE(p, y*p1+p2, cond2);
      VECTOR_COND_MOVE(r, r + (p-0.5*y), cond2);
    VECTOR_ELSEIF2(i==1, cond2, cond)
      VECTOR_COND_MOVE(z, y*y, cond2);
      VECTOR_COND_MOVE(w, z*y, cond2);
      VECTOR_COND_MOVE(p1, t0+w*(t3+w*(t6+w*(t9 +w*t12))), cond2); /* parallel comp */
      VECTOR_COND_MOVE(p2, t1+w*(t4+w*(t7+w*(t10+w*t13))), cond2);
      VECTOR_COND_MOVE(p3, t2+w*(t5+w*(t8+w*(t11+w*t14))), cond2);
      VECTOR_COND_MOVE(p, z*p1-(tt-w*(p2+y*p3)), cond2);
      VECTOR_COND_MOVE(r, r + (tf + p), cond2);
    VECTOR_ELSEIF2(i==2, cond2, cond)
      VECTOR_COND_MOVE(p1, y*(u0+y*(u1+y*(u2+y*(u3+y*(u4+y*u5))))), cond2);
      VECTOR_COND_MOVE(p2, one+y*(v1+y*(v2+y*(v3+y*(v4+y*v5)))), cond2);
      VECTOR_COND_MOVE(r, r + (-0.5*y + p1/p2), cond2);
    VECTOR_ENDIF
  VECTOR_ELSEIF(ix<0x40200000, cond)
  /* x < 8.0 */
    VECTOR_COND_MOVE(i, __builtin_convertvector(x, v64si), cond);
    VECTOR_COND_MOVE(t, VECTOR_INIT(zero), cond);
    VECTOR_COND_MOVE(y, x-__builtin_convertvector(i, v64df), cond);
    VECTOR_COND_MOVE(p, y*(s0+y*(s1+y*(s2+y*(s3+y*(s4+y*(s5+y*s6)))))), cond);
    VECTOR_COND_MOVE(q, one+y*(r1+y*(r2+y*(r3+y*(r4+y*(r5+y*r6))))), cond);
    VECTOR_COND_MOVE(r, half*y+p/q, cond);
    VECTOR_COND_MOVE(z, VECTOR_INIT(one), cond); /* lgamma(1+s) = log(s) + lgamma(s) */
    VECTOR_IF2(i==7, cond2, cond)
      VECTOR_COND_MOVE(z, z * (y+6.0), cond2);
    VECTOR_ENDIF
    VECTOR_IF2(i==7 | i==6, cond2, cond)
      VECTOR_COND_MOVE(z, z * (y+5.0), cond2);
    VECTOR_ENDIF
    VECTOR_IF2(i<=7 & i>=5, cond2, cond)
      VECTOR_COND_MOVE(z, z * (y+4.0), cond2);
    VECTOR_ENDIF
    VECTOR_IF2(i<=7 & i>=4, cond2, cond)
      VECTOR_COND_MOVE(z, z * (y+3.0), cond2);
    VECTOR_ENDIF
    VECTOR_IF2(i<=7 & i>=3, cond2, cond)
      VECTOR_COND_MOVE(z, z * (y+2.0), cond2);
      VECTOR_COND_MOVE(r, r + v64df_log_aux(z, __mask), cond2);
    VECTOR_ENDIF
    /* 8.0 <= x < 2**58 */
  VECTOR_ELSEIF(ix < 0x43900000, cond)
    VECTOR_COND_MOVE(t, v64df_log_aux(x, __mask), cond);
    VECTOR_COND_MOVE(z, one/x, cond);
    VECTOR_COND_MOVE(y, z*z, cond);
    VECTOR_COND_MOVE(w, w0+z*(w1+y*(w2+y*(w3+y*(w4+y*(w5+y*w6))))), cond);
    VECTOR_COND_MOVE(r, (x-half)*(t-one)+w, cond);
  VECTOR_ELSE(cond)
    /* 2**58 <= x <= inf */
    VECTOR_COND_MOVE(r, x*(v64df_log_aux(x, __mask)-one), cond);
  VECTOR_ENDIF
  VECTOR_IF(hx<0, cond)
    VECTOR_COND_MOVE(r, nadj - r, cond);
  VECTOR_ENDIF

  VECTOR_RETURN(r, NO_COND);
  FUNCTION_RETURN;
}
