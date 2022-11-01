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

/* Based on newlib/libm/math/ef_pow.c in Newlib.  */

#include "amdgcnmach.h"

static const float
bp[] = {1.0, 1.5,},
dp_h[] = { 0.0, 5.84960938e-01,}, /* 0x3f15c000 */
dp_l[] = { 0.0, 1.56322085e-06,}, /* 0x35d1cfdc */
zero    =  0.0,
one	=  1.0,
two	=  2.0,
two24	=  16777216.0,	/* 0x4b800000 */
	/* poly coefs for (3/2)*(log(x)-2s-2/3*s**3 */
L1  =  6.0000002384e-01, /* 0x3f19999a */
L2  =  4.2857143283e-01, /* 0x3edb6db7 */
L3  =  3.3333334327e-01, /* 0x3eaaaaab */
L4  =  2.7272811532e-01, /* 0x3e8ba305 */
L5  =  2.3066075146e-01, /* 0x3e6c3255 */
L6  =  2.0697501302e-01, /* 0x3e53f142 */
P1   =  1.6666667163e-01, /* 0x3e2aaaab */
P2   = -2.7777778450e-03, /* 0xbb360b61 */
P3   =  6.6137559770e-05, /* 0x388ab355 */
P4   = -1.6533901999e-06, /* 0xb5ddea0e */
P5   =  4.1381369442e-08, /* 0x3331bb4c */
lg2  =  6.9314718246e-01, /* 0x3f317218 */
lg2_h  =  6.93145752e-01, /* 0x3f317200 */
lg2_l  =  1.42860654e-06, /* 0x35bfbe8c */
ovt =  4.2995665694e-08, /* -(128-log2(ovfl+.5ulp)) */
cp    =  9.6179670095e-01, /* 0x3f76384f =2/(3ln2) */
cp_h  =  9.6179199219e-01, /* 0x3f763800 =head of cp */
cp_l  =  4.7017383622e-06, /* 0x369dc3a0 =tail of cp_h */
ivln2    =  1.4426950216e+00, /* 0x3fb8aa3b =1/ln2 */
ivln2_h  =  1.4426879883e+00, /* 0x3fb8aa00 =16b 1/ln2*/
ivln2_l  =  7.0526075433e-06; /* 0x36eca570 =1/ln2 tail*/

v64sf v64sf_sqrtf_aux (v64sf, v64si);
v64sf v64sf_scalbnf_aux (v64sf, v64si, v64si);

DEF_VS_MATH_FUNC (v64sf, powf, v64sf x, v64sf y)
{
  FUNCTION_INIT (v64sf);

  v64si hx, hy;
  GET_FLOAT_WORD (hx,x, NO_COND);
  GET_FLOAT_WORD (hy,y, NO_COND);
  v64si ix = hx&0x7fffffff;
  v64si iy = hy&0x7fffffff;

  /* y==zero: x**0 = 1 */
  VECTOR_IF (FLT_UWORD_IS_ZERO(iy), cond)
    VECTOR_RETURN (x + y, cond & v64sf_issignalingf_inline(x));
    VECTOR_RETURN (VECTOR_INIT (1.0f), cond);
  VECTOR_ENDIF

  /* x|y==NaN return NaN unless x==1 then return 1 */
  VECTOR_IF (FLT_UWORD_IS_NAN(ix) | FLT_UWORD_IS_NAN(iy), cond)
    VECTOR_IF2 (hx==0x3f800000 & ~v64sf_issignalingf_inline(y), cond2, cond)
      VECTOR_RETURN (VECTOR_INIT (1.0f), cond2);
    VECTOR_ELSE2 (cond2, cond)
      VECTOR_RETURN (x + y, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF

  /* determine if y is an odd int when x < 0
    * yisint = 0	... y is not an integer
    * yisint = 1	... y is an odd int
    * yisint = 2	... y is an even int
    */
  v64si yisint = VECTOR_INIT (0);

  VECTOR_IF (hx < 0, cond)
    VECTOR_IF2 (iy >= 0x4b800000, cond2, cond)
      VECTOR_COND_MOVE (yisint, VECTOR_INIT (2), cond2); /* even integer y */
    VECTOR_ELSEIF2 (iy >= 0x3f800000, cond2, cond)
      v64si k = (iy>>23)-0x7f;	   /* exponent */
      v64si j = iy>>(23-k);
      VECTOR_COND_MOVE (yisint, 2-(j&1), cond2 & (j<<(23-k))==iy);
    VECTOR_ENDIF
  VECTOR_ENDIF

    /* special value of y */
  VECTOR_IF (FLT_UWORD_IS_INFINITE(iy), cond)     /* y is +-inf */
    VECTOR_IF2 (ix==0x3f800000, cond2, cond)
      VECTOR_RETURN (VECTOR_INIT (1.0f), cond2);  /* +-1**+-inf = 1 */
    VECTOR_ELSEIF2 (ix > 0x3f800000, cond2, cond) /* (|x|>1)**+-inf = inf,0 */
      VECTOR_RETURN (y, cond2 & (hy >= 0));
      VECTOR_RETURN (VECTOR_INIT (0.0f), cond2);
    VECTOR_ELSE2 (cond2, cond) /* (|x|<1)**-,+inf = inf,0 */
      VECTOR_RETURN (-y, cond2 & (hy<0));
      VECTOR_RETURN (VECTOR_INIT (0.0f), cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF
  VECTOR_IF (iy==0x3f800000, cond)  /* y is  +-1 */
    VECTOR_RETURN (VECTOR_INIT (1.0f) / x, cond & (hy<0));
    VECTOR_RETURN (x, cond);
  VECTOR_ENDIF

  VECTOR_RETURN (x*x, hy==0x40000000);  /* y is  2 */
  /* y is  0.5 */
  /* x >= +0 */
  VECTOR_RETURN (v64sf_sqrtf_aux (x, __mask), (hy==0x3f000000) & (hx >= 0));

  v64sf ax = __builtin_gcn_fabsvf(x);
    /* special value of x */
  VECTOR_IF (FLT_UWORD_IS_INFINITE(ix)|FLT_UWORD_IS_ZERO(ix)|ix==0x3f800000, cond)
    v64sf z = ax;			/*x is +-0,+-inf,+-1*/
    VECTOR_COND_MOVE (z, VECTOR_INIT (1.0f) / z, cond & (hy < 0)); /* z = (1/|x|) */
    VECTOR_IF2 (hx<0, cond2, cond)
      VECTOR_IF2 (((ix-0x3f800000)|yisint)==0, cond3, cond2)
        /* (-1)**non-int is NaN */
        VECTOR_COND_MOVE (z, (z-z)/(z-z), cond3);
      VECTOR_ELSEIF2 (yisint==1, cond3, cond2)
        /* (x<0)**odd = -(|x|**odd) */
        VECTOR_COND_MOVE (z, -z, cond3);
      VECTOR_ENDIF
    VECTOR_ENDIF
    VECTOR_RETURN (z, cond);
  VECTOR_ENDIF
    
  /* (x<0)**(non-int) is NaN */
  VECTOR_RETURN ((x-x)/(x-x), ((((hx >> 31) & 1) - 1)|yisint)==0);

  v64sf t1, t2;

  /* |y| is huge */
  VECTOR_IF (iy>0x4d000000, cond) /* if |y| > 2**27 */
    /* over/underflow if x is not close to one */
    VECTOR_IF2(ix<0x3f7ffff4, cond2, cond)
      VECTOR_RETURN (v64sf_math_oflowf(VECTOR_INIT (0)), cond2 & (hy < 0));
      VECTOR_RETURN (v64sf_math_uflowf(VECTOR_INIT (0)), cond2);
    VECTOR_ENDIF
    VECTOR_IF2(ix>0x3f800007, cond2, cond)
      VECTOR_RETURN (v64sf_math_oflowf(VECTOR_INIT (0)), cond2 & (hy > 0));
      VECTOR_RETURN (v64sf_math_uflowf(VECTOR_INIT (0)), cond2);
    VECTOR_ENDIF
    /* now |1-x| is tiny <= 2**-20, suffice to compute 
       log(x) by x-x^2/2+x^3/3-x^4/4 */
    v64sf t = ax-1;		/* t has 20 trailing zeros */
    v64sf w = (t*t)*(0.5f-t*(0.333333333333f-t*0.25f));
    v64sf u = ivln2_h*t;	/* ivln2_h has 16 sig. bits */
    v64sf v = t*ivln2_l-w*ivln2;
    VECTOR_COND_MOVE (t1, u+v, cond);
    v64si is;
    GET_FLOAT_WORD(is,t1, cond);
    SET_FLOAT_WORD(t1,is&0xfffff000, cond);
    VECTOR_COND_MOVE (t2, v-(t1-u), cond);
    VECTOR_RETURN (VECTOR_INIT (0.123456f), cond);
  VECTOR_ELSE (cond)
    v64si n = VECTOR_INIT (0);
    /* take care subnormal number */
    VECTOR_IF2 (FLT_UWORD_IS_SUBNORMAL(ix), cond2, cond)
      VECTOR_COND_MOVE (ax, ax * two24, cond);
      VECTOR_COND_MOVE (n, n - 24, cond2);
      GET_FLOAT_WORD(ix,ax, cond2);
    VECTOR_ENDIF
    n  += (ix>>23)-0x7f;
    v64si j  = ix&0x007fffff;
    /* determine interval */
    v64si ix = j|0x3f800000;		/* normalize ix */
    v64si k;
    VECTOR_IF2 (j<=0x1cc471, cond2, cond)
      VECTOR_COND_MOVE (k, VECTOR_INIT (0), cond2); /* |x|<sqrt(3/2) */
    VECTOR_ELSEIF2 (j<0x5db3d7, cond2, cond)
      VECTOR_COND_MOVE (k, VECTOR_INIT (1), cond2); /* |x|<sqrt(3)   */
    VECTOR_ELSE2 (cond2, cond)
      VECTOR_COND_MOVE (k, VECTOR_INIT (0), cond2);
      VECTOR_COND_MOVE (n, n + 1, cond2);
      VECTOR_COND_MOVE (ix, ix - 0x00800000, cond2);
    VECTOR_ENDIF
    SET_FLOAT_WORD(ax,ix, cond);

    /* compute s = s_h+s_l = (x-1)/(x+1) or (x-1.5)/(x+1.5) */
    /* bp[0]=1.0, bp[1]=1.5 */
    v64sf bp_k = VECTOR_MERGE (VECTOR_INIT (bp[1]), VECTOR_INIT (bp[0]), k == 1);
    v64sf u = ax-bp_k;
    v64sf v = 1.0f/(ax+bp_k);
    v64sf s = u*v;
    v64sf s_h = s;
    v64si is;
    GET_FLOAT_WORD(is,s_h, cond);
    SET_FLOAT_WORD(s_h,is&0xfffff000, cond);
    /* t_h=ax+bp[k] High */
    v64sf t_h;
    SET_FLOAT_WORD(t_h,((ix>>1)|0x20000000)+0x0040000+(k<<21), cond);
    v64sf t_l = ax - (t_h-bp_k);
    v64sf s_l = v*((u-s_h*t_h)-s_h*t_l);
    /* compute log(ax) */
    v64sf s2 = s*s;
    v64sf r = s2*s2*(L1+s2*(L2+s2*(L3+s2*(L4+s2*(L5+s2*L6)))));
    r += s_l*(s_h+s);
    s2  = s_h*s_h;
    t_h = __builtin_convertvector(3.0f+s2+r, v64sf);
    GET_FLOAT_WORD(is,t_h, cond);
    SET_FLOAT_WORD(t_h,is&0xfffff000, cond);
    t_l = r-((t_h-3.0f)-s2);
    /* u+v = s*(1+...) */
    u = s_h*t_h;
    v = s_l*t_h+t_l*s;
    /* 2/(3log2)*(s+...) */
    v64sf p_h = u+v;
    GET_FLOAT_WORD(is,p_h, cond);
    SET_FLOAT_WORD(p_h,is&0xfffff000, cond);
    v64sf p_l = v-(p_h-u);
    v64sf z_h = cp_h*p_h;		/* cp_h+cp_l = 2/(3*log2) */
    v64sf dp_l_k = VECTOR_MERGE (VECTOR_INIT (dp_l[1]), VECTOR_INIT (dp_l[0]), k == 1);
    v64sf z_l = cp_l*p_h+p_l*cp+dp_l_k;
    /* log2(ax) = (s+..)*2/(3*log2) = n + dp_h + z_h + z_l */
    v64sf t = __builtin_convertvector (n, v64sf);
    v64sf dp_h_k = VECTOR_MERGE (VECTOR_INIT (dp_h[1]), VECTOR_INIT (dp_h[0]), k == 1);
    VECTOR_COND_MOVE (t1, (((z_h+z_l)+dp_h_k)+t), cond);
    GET_FLOAT_WORD(is,t1, cond);
    SET_FLOAT_WORD(t1,is&0xfffff000, cond);
    VECTOR_COND_MOVE (t2, z_l-(((t1-t)-dp_h_k)-z_h), cond);
  VECTOR_ENDIF

  v64sf s = VECTOR_INIT (1.0f); /* s (sign of result -ve**odd) = -1 else = 1 */
  VECTOR_COND_MOVE (s, VECTOR_INIT (-1.0f), /* (-ve)**(odd int) */
                    ((hx>>31) != 0)&(yisint == 1));

  /* split up y into y1+y2 and compute (y1+y2)*(t1+t2) */
  v64si is;
  GET_FLOAT_WORD(is,y, NO_COND);
  v64sf y1;
  SET_FLOAT_WORD(y1,is&0xfffff000, NO_COND);
  v64sf p_l = (y-y1)*t1+y*t2;
  v64sf p_h = y1*t1;
  v64sf z = p_l+p_h;
  v64si j;
  GET_FLOAT_WORD(j,z, NO_COND);
  v64si i = j&0x7fffffff;
  VECTOR_IF (j>0, cond)
    VECTOR_RETURN (v64sf_math_oflowf(s<0), cond & i>FLT_UWORD_EXP_MAX); /* overflow */
    VECTOR_RETURN (v64sf_math_oflowf(s<0), cond & (i==FLT_UWORD_EXP_MAX)
                                           & (p_l+ovt>z-p_h));  /* overflow */
  VECTOR_ELSE (cond)
    VECTOR_RETURN (v64sf_math_uflowf(s<0), cond & (i>FLT_UWORD_EXP_MIN)); /* underflow */
    VECTOR_RETURN (v64sf_math_uflowf(s<0), cond & (i==FLT_UWORD_EXP_MIN)
                                           & (p_l<=z-p_h));   /* underflow */
  VECTOR_ENDIF
  /*
    * compute 2**(p_h+p_l)
    */
  v64si k = (i>>23)-0x7f;
  v64si n = VECTOR_INIT (0);
  VECTOR_IF (i>0x3f000000, cond)		/* if |z| > 0.5, set n = [z+0.5] */
    VECTOR_COND_MOVE (n, j+(0x00800000>>(k+1)), cond);
    k = ((n&0x7fffffff)>>23)-0x7f;	/* new k for n */
    v64sf t;
    SET_FLOAT_WORD(t,n&~(0x007fffff>>k), cond);
    VECTOR_COND_MOVE (n, ((n&0x007fffff)|0x00800000)>>(23-k), cond);
    VECTOR_COND_MOVE (n, -n, cond & (j<0));
    VECTOR_COND_MOVE (p_h, p_h - t, cond);
  VECTOR_ENDIF
  v64sf t = p_l+p_h;
  GET_FLOAT_WORD(is,t, NO_COND);
  SET_FLOAT_WORD(t,is&0xfffff000, NO_COND);
  v64sf u = t*lg2_h;
  v64sf v = (p_l-(t-p_h))*lg2+t*lg2_l;
  z = u+v;
  v64sf w = v-(z-u);
  t  = z*z;
  t1  = z - t*(P1+t*(P2+t*(P3+t*(P4+t*P5))));
  v64sf r  = (z*t1)/(t1-2.0f)-(w+z*w);
  z  = VECTOR_INIT (1.0f)-(r-z);
  GET_FLOAT_WORD(j,z, NO_COND);
  j += (n<<23);
  VECTOR_IF ((j>>23)<=0, cond)
    VECTOR_COND_MOVE (z, v64sf_scalbnf_aux(z, n, __mask), cond);	/* subnormal output */
  VECTOR_ELSE (cond)
    SET_FLOAT_WORD(z, j, cond);
  VECTOR_ENDIF
  VECTOR_RETURN (s*z, NO_COND);

  FUNCTION_RETURN;
}

DEF_VARIANTS2 (powf, sf, sf)
