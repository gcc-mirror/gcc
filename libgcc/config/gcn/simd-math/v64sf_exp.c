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

/* Based on newlib/libm/math/ef_exp.c in Newlib.  */

#include "amdgcnmach.h"

static const float
huge	= 1.0e+30,
twom100 = 7.8886090522e-31,      /* 2**-100=0x0d800000 */
ln2HI[2]   ={ 6.9313812256e-01,		/* 0x3f317180 */
	     -6.9313812256e-01,},	/* 0xbf317180 */
ln2LO[2]   ={ 9.0580006145e-06,  	/* 0x3717f7d1 */
	     -9.0580006145e-06,},	/* 0xb717f7d1 */
invln2 =  1.4426950216e+00, 		/* 0x3fb8aa3b */
P1   =  1.6666667163e-01, /* 0x3e2aaaab */
P2   = -2.7777778450e-03, /* 0xbb360b61 */
P3   =  6.6137559770e-05, /* 0x388ab355 */
P4   = -1.6533901999e-06, /* 0xb5ddea0e */
P5   =  4.1381369442e-08; /* 0x3331bb4c */

DEF_VS_MATH_FUNC (v64sf, expf, v64sf x)
{
  FUNCTION_INIT (v64sf);

  v64si k = VECTOR_INIT (0);
  v64si sx;
  GET_FLOAT_WORD(sx, x, NO_COND);
  v64si xsb = (sx>>31)&1;		/* sign bit of x */
  v64si hx = sx & 0x7fffffff;		/* high word of |x| */

  /* filter out non-finite argument */
  VECTOR_RETURN (x+x, FLT_UWORD_IS_NAN(hx));     /* NaN */
  VECTOR_RETURN (x, FLT_UWORD_IS_INFINITE(hx) & (xsb == 0));
  VECTOR_RETURN (VECTOR_INIT (0.0f), FLT_UWORD_IS_INFINITE (hx)); /* exp(+-inf)={inf,0} */
  VECTOR_RETURN (v64sf_math_oflowf (VECTOR_INIT (0)), sx > FLT_UWORD_LOG_MAX);  /* overflow */
  VECTOR_RETURN (v64sf_math_uflowf (VECTOR_INIT (0)), (sx < 0) & (hx > FLT_UWORD_LOG_MIN));   /* underflow */
	
  /* argument reduction */
  v64sf hi, lo;
  VECTOR_IF (hx > 0x3eb17218, cond)		/* if  |x| > 0.5 ln2 */ 
    VECTOR_IF2 (hx < 0x3F851592, cond2, cond)	/* and |x| < 1.5 ln2 */
      VECTOR_COND_MOVE (hi, x-ln2HI[0], cond2 & (xsb == 0));
      VECTOR_COND_MOVE (hi, x-ln2HI[1], cond2 & (xsb == 1));
      VECTOR_COND_MOVE (lo, VECTOR_INIT (ln2LO[0]), cond2 & (xsb == 0));
      VECTOR_COND_MOVE (lo, VECTOR_INIT (ln2LO[1]), cond2 & (xsb == 1));
      VECTOR_COND_MOVE (k, 1-xsb-xsb, cond2);
    VECTOR_ELSE2 (cond2, cond)
      VECTOR_COND_MOVE (k, __builtin_convertvector (invln2*x + 0.5f, v64si), cond2 & (xsb == 0));
      VECTOR_COND_MOVE (k, __builtin_convertvector (invln2*x - 0.5f, v64si), cond2 & (xsb == 1));
      v64sf t = __builtin_convertvector (k, v64sf);
      VECTOR_COND_MOVE (hi, x - t*ln2HI[0], cond2);	/* t*ln2HI is exact here */
      VECTOR_COND_MOVE (lo, t*ln2LO[0], cond2);
    VECTOR_ENDIF
    VECTOR_COND_MOVE (x, hi - lo, cond);
  VECTOR_ELSEIF (hx < 0x34000000, cond) /* when |x|<2**-23 */
    VECTOR_RETURN (1.0f+x, cond & (huge+x > 1.0f));  /* trigger inexact */
  VECTOR_ENDIF

    /* x is now in primary range */
  v64sf t = x*x;
  v64sf c = x - t*(P1+t*(P2+t*(P3+t*(P4+t*P5))));
  VECTOR_RETURN (1.0f - ((x*c)/(c-2.0f)-x), k==0);
  v64sf y = 1.0f - ((lo-(x*c)/(2.0f-c))-hi);
  VECTOR_IF (k >= -125, cond)
    v64si hy;
    GET_FLOAT_WORD(hy, y, cond);
    SET_FLOAT_WORD(y,hy+(k<<23), cond);	/* add k to y's exponent */
    VECTOR_RETURN (y, cond);
  VECTOR_ELSE (cond)
    v64si hy;
    GET_FLOAT_WORD(hy, y, cond);
    SET_FLOAT_WORD(y, hy+((k+100)<<23), cond);	/* add k to y's exponent */
    VECTOR_RETURN (y*twom100, cond);
  VECTOR_ENDIF

  FUNCTION_RETURN;
}

DEF_VARIANTS (expf, sf, sf)
