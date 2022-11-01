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

/* Based on newlib/libm/math/e_pow.c in Newlib.  */

#include "amdgcnmach.h"

static const double
bp[] = {1.0, 1.5,},
dp_h[] = { 0.0, 5.84962487220764160156e-01,}, /* 0x3FE2B803, 0x40000000 */
dp_l[] = { 0.0, 1.35003920212974897128e-08,}, /* 0x3E4CFDEB, 0x43CFD006 */
zero    =  0.0,
one	=  1.0,
two	=  2.0,
two53	=  9007199254740992.0,	/* 0x43400000, 0x00000000 */
	/* poly coefs for (3/2)*(log(x)-2s-2/3*s**3 */
L1  =  5.99999999999994648725e-01, /* 0x3FE33333, 0x33333303 */
L2  =  4.28571428578550184252e-01, /* 0x3FDB6DB6, 0xDB6FABFF */
L3  =  3.33333329818377432918e-01, /* 0x3FD55555, 0x518F264D */
L4  =  2.72728123808534006489e-01, /* 0x3FD17460, 0xA91D4101 */
L5  =  2.30660745775561754067e-01, /* 0x3FCD864A, 0x93C9DB65 */
L6  =  2.06975017800338417784e-01, /* 0x3FCA7E28, 0x4A454EEF */
P1   =  1.66666666666666019037e-01, /* 0x3FC55555, 0x5555553E */
P2   = -2.77777777770155933842e-03, /* 0xBF66C16C, 0x16BEBD93 */
P3   =  6.61375632143793436117e-05, /* 0x3F11566A, 0xAF25DE2C */
P4   = -1.65339022054652515390e-06, /* 0xBEBBBD41, 0xC5D26BF1 */
P5   =  4.13813679705723846039e-08, /* 0x3E663769, 0x72BEA4D0 */
lg2  =  6.93147180559945286227e-01, /* 0x3FE62E42, 0xFEFA39EF */
lg2_h  =  6.93147182464599609375e-01, /* 0x3FE62E43, 0x00000000 */
lg2_l  = -1.90465429995776804525e-09, /* 0xBE205C61, 0x0CA86C39 */
ovt =  8.0085662595372944372e-0017, /* -(1024-log2(ovfl+.5ulp)) */
cp    =  9.61796693925975554329e-01, /* 0x3FEEC709, 0xDC3A03FD =2/(3ln2) */
cp_h  =  9.61796700954437255859e-01, /* 0x3FEEC709, 0xE0000000 =(float)cp */
cp_l  = -7.02846165095275826516e-09, /* 0xBE3E2FE0, 0x145B01F5 =tail of cp_h*/
ivln2    =  1.44269504088896338700e+00, /* 0x3FF71547, 0x652B82FE =1/ln2 */
ivln2_h  =  1.44269502162933349609e+00, /* 0x3FF71547, 0x60000000 =24b 1/ln2*/
ivln2_l  =  1.92596299112661746887e-08; /* 0x3E54AE0B, 0xF85DDF44 =1/ln2 tail*/

v64df v64df_sqrt_aux (v64df, v64di);
v64df v64df_scalbn_aux (v64df, v64si, v64di);

static v64df v64df_math_oflow (v64di sign)
{
  errno = ERANGE;
  return VECTOR_MERGE (VECTOR_INIT (-0x1p769),
                       VECTOR_INIT (0x1p769), sign) * 0x1p769;
}

static v64df v64df_math_uflow (v64di sign)
{
  errno = ERANGE;
  return VECTOR_MERGE (VECTOR_INIT (-0x1p-767),
                       VECTOR_INIT (0x1p-767), sign) * 0x1p-767;
}

static v64si v64df_issignaling_inline (v64df x)
{
  v64si __mask = VECTOR_INIT (-1);
  v64si ix;
  GET_HIGH_WORD (ix, x, NO_COND);
  /* Use IEEE-754 2008 encoding - i.e. exponent bits all 1, MSB of
     significand is 0 for signalling NaN.  */
  return ((ix & 0x7ff00000) == 0x7ff00000) & ((ix & 0x00080000) == 0);
}

DEF_VD_MATH_FUNC (v64df, pow, v64df x, v64df y)
{
  FUNCTION_INIT (v64df);

  v64si hx, hy, lx, ly;
  EXTRACT_WORDS(hx,lx,x);
  EXTRACT_WORDS(hy,ly,y);
  v64si ix = hx&0x7fffffff;
  v64si iy = hy&0x7fffffff;

  /* y==zero: x**0 = 1 unless x is snan */
  VECTOR_IF ((iy|ly)==0, cond)
    VECTOR_RETURN (x + y, cond & v64df_issignaling_inline(x));
    VECTOR_RETURN (VECTOR_INIT (1.0), cond);
  VECTOR_ENDIF

  /* x|y==NaN return NaN unless x==1 then return 1 */
  VECTOR_IF ((ix > 0x7ff00000) | ((ix==0x7ff00000)&(lx!=0))
             | (iy > 0x7ff00000) | ((iy==0x7ff00000)&(ly!=0)), cond)
    VECTOR_RETURN (VECTOR_INIT (1.0), cond & ((hx-0x3ff00000)|lx)==0
                                      & ~v64df_issignaling_inline(y));
    VECTOR_RETURN (x + y, cond);
  VECTOR_ENDIF

  /* determine if y is an odd int when x < 0
    * yisint = 0	... y is not an integer
    * yisint = 1	... y is an odd int
    * yisint = 2	... y is an even int
    */
  v64si yisint = VECTOR_INIT (0);

  VECTOR_IF (hx < 0, cond)
    VECTOR_IF2(iy>=0x43400000, cond2, cond)
      VECTOR_COND_MOVE (yisint, VECTOR_INIT (2), cond2); /* even integer y */
    VECTOR_ELSEIF2 (iy>=0x3ff00000, cond2, cond)
      v64si k = (iy>>20)-0x3ff;	   /* exponent */
      VECTOR_IF2 (k>20, cond3, cond2)
        v64si j = ly>>(52-k);
        VECTOR_COND_MOVE (yisint, 2-(j&1), cond3 & (j<<(52-k))==ly);
      VECTOR_ELSEIF2 (ly==0, cond3, cond2)
        v64si j = iy>>(20-k);
        VECTOR_COND_MOVE (yisint, 2-(j&1), cond3 & (j<<(20-k))==iy);
      VECTOR_ENDIF
    VECTOR_ENDIF
  VECTOR_ENDIF

  /* special value of y */
	VECTOR_IF (ly==0, cond)
    VECTOR_IF2 (iy==0x7ff00000, cond2, cond)	/* y is +-inf */
      VECTOR_IF2 (((ix-0x3ff00000)|lx)==0, cond3, cond2)
        VECTOR_RETURN (VECTOR_INIT (1.0), cond3);		/* +-1**+-inf = 1 */
      VECTOR_ELSEIF2 (ix >= 0x3ff00000, cond3, cond2) /* (|x|>1)**+-inf = inf,0 */
        VECTOR_RETURN (y, cond3 & hy>=0);
        VECTOR_RETURN (VECTOR_INIT (0.0), cond3);
      VECTOR_ELSE2 (cond3, cond2) /* (|x|<1)**-,+inf = inf,0 */
        VECTOR_RETURN (-y, cond3 & hy<0);
        VECTOR_RETURN (VECTOR_INIT (0.0), cond3);
      VECTOR_ENDIF
    VECTOR_ENDIF
    VECTOR_IF2 (iy==0x3ff00000, cond2, cond)  /* y is  +-1 */
      VECTOR_RETURN (VECTOR_INIT (1.0) / x, cond2 & hy<0);
		  VECTOR_RETURN (x, cond2);
    VECTOR_ENDIF
    VECTOR_RETURN (x*x, cond & hy==0x40000000); /* y is  2 */
    /* y is  0.5 */
    /* x >= +0 */
    VECTOR_RETURN (v64df_sqrt_aux (x, __mask), cond & (hy==0x3fe00000) & (hx>=0));
  VECTOR_ENDIF

  v64df ax = __builtin_gcn_fabsv(x);
    /* special value of x */
  VECTOR_IF (lx==0, cond)
    VECTOR_IF2 ((ix==0x7ff00000)|(ix==0)|(ix==0x3ff00000), cond2, cond)
      v64df z = ax;			/*x is +-0,+-inf,+-1*/
      VECTOR_COND_MOVE (z, VECTOR_INIT (1.0) / z, cond2 & (hy<0));  /* z = (1/|x|) */
      VECTOR_IF2 (hx<0, cond3, cond2)
        VECTOR_IF2 (((ix-0x3ff00000)|yisint)==0, cond4, cond3)
          VECTOR_COND_MOVE (z, (z-z)/(z-z), cond4); /* (-1)**non-int is NaN */
        VECTOR_ELSEIF2 (yisint==1, cond4, cond3) 
          VECTOR_COND_MOVE (z, -z, cond4); /* (x<0)**odd = -(|x|**odd) */
        VECTOR_ENDIF
      VECTOR_ENDIF
      VECTOR_RETURN (z, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF

  /* (x<0)**(non-int) is NaN */
  VECTOR_RETURN ((x-x)/(x-x), ((((hx >> 31) & 1) - 1)|yisint)==0);

  v64df t1, t2;

    /* |y| is huge */
  VECTOR_IF(iy>0x41e00000, cond) /* if |y| > 2**31 */
    VECTOR_IF2 (iy>0x43f00000, cond2, cond)	/* if |y| > 2**64, must o/uflow */
      VECTOR_IF2 (ix<=0x3fefffff, cond3, cond2)
        VECTOR_RETURN (v64df_math_oflow (VECTOR_INIT (0L)), cond3 & (hy<0));
        VECTOR_RETURN (v64df_math_uflow (VECTOR_INIT (0L)), cond3);
      VECTOR_ENDIF
      VECTOR_IF2 (ix>=0x3ff00000, cond3, cond2)
        VECTOR_RETURN (v64df_math_oflow (VECTOR_INIT (0L)), cond3 & (hy>0));
        VECTOR_RETURN (v64df_math_uflow (VECTOR_INIT (0L)), cond3);
      VECTOR_ENDIF
    VECTOR_ENDIF
    /* over/underflow if x is not close to one */
    VECTOR_IF2 (ix<0x3fefffff, cond2, cond)
      VECTOR_RETURN (v64df_math_oflow (VECTOR_INIT (0L)), cond2 & (hy<0));
      VECTOR_RETURN (v64df_math_uflow (VECTOR_INIT (0L)), cond2);
    VECTOR_ENDIF
    VECTOR_IF2 (ix>0x3ff00000, cond2, cond)
      VECTOR_RETURN (v64df_math_oflow (VECTOR_INIT (0L)), cond2 & (hy>0));
      VECTOR_RETURN (v64df_math_uflow (VECTOR_INIT (0L)), cond2);
    VECTOR_ENDIF
    /* now |1-x| is tiny <= 2**-20, suffice to compute
       log(x) by x-x^2/2+x^3/3-x^4/4 */
    v64df t = ax-1;		/* t has 20 trailing zeros */
    v64df w = (t*t)*(0.5-t*(0.3333333333333333333333-t*0.25));
    v64df u = ivln2_h*t;	/* ivln2_h has 21 sig. bits */
    v64df v = t*ivln2_l-w*ivln2;
    VECTOR_COND_MOVE (t1, u+v, cond);
    SET_LOW_WORD (t1, VECTOR_INIT (0), cond);
    VECTOR_COND_MOVE (t2, v-(t1-u), cond);
  VECTOR_ELSE (cond)
    v64si n = VECTOR_INIT (0);
    /* take care subnormal number */
    VECTOR_IF2 (ix<0x00100000, cond2, cond)
      VECTOR_COND_MOVE (ax, ax * two53, cond2);
      VECTOR_COND_MOVE (n, n - 53, cond2);
      GET_HIGH_WORD (ix, ax, cond2);
    VECTOR_ENDIF
    n  += ((ix)>>20)-0x3ff;
    v64si j  = ix&0x000fffff;
    /* determine interval */
    ix = j|0x3ff00000;		/* normalize ix */
    v64si k;
    VECTOR_IF2 (j<=0x3988E, cond2, cond)
      VECTOR_COND_MOVE (k, VECTOR_INIT (0), cond2); /* |x|<sqrt(3/2) */
    VECTOR_ELSEIF2 (j<0xBB67A, cond2, cond)
      VECTOR_COND_MOVE (k, VECTOR_INIT (1), cond2); /* |x|<sqrt(3)   */
    VECTOR_ELSE2 (cond2, cond)
      VECTOR_COND_MOVE (k, VECTOR_INIT (0), cond2);
      VECTOR_COND_MOVE (n, n + 1, cond2);
      VECTOR_COND_MOVE (ix, ix - 0x00100000, cond2);
    VECTOR_ENDIF
    SET_HIGH_WORD (ax, ix, cond);

    /* compute s = s_h+s_l = (x-1)/(x+1) or (x-1.5)/(x+1.5) */
    v64df bp_k = VECTOR_MERGE (VECTOR_INIT (bp[1]), VECTOR_INIT (bp[0]), k == 1);
    v64df u = ax-bp_k;		/* bp[0]=1.0, bp[1]=1.5 */
    v64df v = 1.0/(ax+bp_k);
    v64df s = u*v;
    v64df s_h = s;
    SET_LOW_WORD (s_h, VECTOR_INIT (0), cond);
    /* t_h=ax+bp[k] High */
    v64df t_h = VECTOR_INIT (0.0);
    SET_HIGH_WORD (t_h,((ix>>1)|0x20000000)+0x00080000+(k<<18), cond);
    v64df t_l = ax - (t_h-bp_k);
    v64df s_l = v*((u-s_h*t_h)-s_h*t_l);
    /* compute log(ax) */
    v64df s2 = s*s;
    v64df r = s2*s2*(L1+s2*(L2+s2*(L3+s2*(L4+s2*(L5+s2*L6)))));
    r += s_l*(s_h+s);
    s2  = s_h*s_h;
    t_h = 3.0+s2+r;
    SET_LOW_WORD (t_h, VECTOR_INIT (0), cond);
    t_l = r-((t_h-3.0)-s2);
    /* u+v = s*(1+...) */
    u = s_h*t_h;
    v = s_l*t_h+t_l*s;
    /* 2/(3log2)*(s+...) */
    v64df p_h = u+v;
    SET_LOW_WORD (p_h, VECTOR_INIT (0), cond);
    v64df p_l = v-(p_h-u);
    v64df z_h = cp_h*p_h;		/* cp_h+cp_l = 2/(3*log2) */
    v64df dp_l_k = VECTOR_MERGE (VECTOR_INIT (dp_l[1]), VECTOR_INIT (dp_l[0]), k == 1);
    v64df z_l = cp_l*p_h+p_l*cp+dp_l_k;
    /* log2(ax) = (s+..)*2/(3*log2) = n + dp_h + z_h + z_l */
    v64df t = __builtin_convertvector (n, v64df);
    v64df dp_h_k = VECTOR_MERGE (VECTOR_INIT (dp_h[1]), VECTOR_INIT (dp_h[0]), k == 1);
    VECTOR_COND_MOVE (t1, ((z_h+z_l)+dp_h_k)+t, cond);
    SET_LOW_WORD (t1, VECTOR_INIT (0), cond);
    VECTOR_COND_MOVE (t2, z_l-(((t1-t)-dp_h_k)-z_h), cond);
  VECTOR_ENDIF

  v64df s = VECTOR_INIT (1.0); /* s (sign of result -ve**odd) = -1 else = 1 */
  VECTOR_COND_MOVE (s, VECTOR_INIT (-1.0),  /* (-ve)**(odd int) */
                    ((hx>>31) != 0)&(yisint == 1));

  /* split up y into y1+y2 and compute (y1+y2)*(t1+t2) */
  v64df y1 = y;
  SET_LOW_WORD (y1, VECTOR_INIT (0), NO_COND);
  v64df p_l = (y-y1)*t1+y*t2;
  v64df p_h = y1*t1;
  v64df z = p_l+p_h;
  v64si i, j;
  EXTRACT_WORDS(j, i, z);
  VECTOR_IF (j>=0x40900000, cond) /* z >= 1024 */
    /* if z > 1024 */
    v64di cond_di = __builtin_convertvector (cond, v64di);
    VECTOR_RETURN (v64df_math_oflow(s<0), cond & (((j-0x40900000)|i)!=0)); /* overflow */
    VECTOR_RETURN (v64df_math_oflow(s<0), cond_di & (p_l+ovt>z-p_h));	/* overflow */
  VECTOR_ELSEIF ((j&0x7fffffff)>=0x4090cc00, cond)  /* z <= -1075 */
    /* z < -1075 */
    v64di cond_di = __builtin_convertvector (cond, v64di);
    VECTOR_RETURN (v64df_math_uflow(s<0), cond & (((j-0xc090cc00)|i)!=0));  /* underflow */
    VECTOR_RETURN (v64df_math_uflow(s<0), cond_di & (p_l<=z-p_h)); /* underflow */
  VECTOR_ENDIF

  /*
   * compute 2**(p_h+p_l)
   */
  i = j&0x7fffffff;
  v64si k = (i>>20)-0x3ff;
  v64si n = VECTOR_INIT (0);
  VECTOR_IF (i>0x3fe00000, cond)  /* if |z| > 0.5, set n = [z+0.5] */
    VECTOR_COND_MOVE (n, j+(0x00100000>>(k+1)), cond);
    k = ((n&0x7fffffff)>>20)-0x3ff;	/* new k for n */
    v64df t = VECTOR_INIT (0.0);
    SET_HIGH_WORD(t, n&~(0x000fffff>>k), cond);
    VECTOR_COND_MOVE (n, ((n&0x000fffff)|0x00100000)>>(20-k), cond);
    VECTOR_COND_MOVE (n, -n, cond & (j<0));
    VECTOR_COND_MOVE (p_h, p_h - t, cond);
  VECTOR_ENDIF
  v64df t = p_l+p_h;
  SET_LOW_WORD(t, VECTOR_INIT (0), NO_COND);
  v64df u = t*lg2_h;
  v64df v = (p_l-(t-p_h))*lg2+t*lg2_l;
  z = u+v;
  v64df w = v-(z-u);
  t  = z*z;
  t1  = z - t*(P1+t*(P2+t*(P3+t*(P4+t*P5))));
  v64df r  = (z*t1)/(t1-two)-(w+z*w);
  z  = VECTOR_INIT (1.0)-(r-z);
  GET_HIGH_WORD(j,z, NO_COND);
  j += (n<<20);
  VECTOR_IF ((j>>20)<=0, cond)
    VECTOR_COND_MOVE (z, v64df_scalbn_aux (z, n, __mask), cond); /* subnormal output */
	VECTOR_ELSE (cond)
    SET_HIGH_WORD(z,j, cond);
  VECTOR_ENDIF
  VECTOR_RETURN (s*z, NO_COND);

  FUNCTION_RETURN;
}

DEF_VARIANTS2 (pow, df, df)
