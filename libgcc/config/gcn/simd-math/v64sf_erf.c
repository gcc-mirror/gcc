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

/* Based on newlib/libm/mathfp/sf_erf.c in Newlib.  */

#include "amdgcnmach.h"

v64sf v64sf_expf_aux (v64sf, v64si);

static const float
tiny	    = 1e-30,
half=  5.0000000000e-01, /* 0x3F000000 */
one =  1.0000000000e+00, /* 0x3F800000 */
two =  2.0000000000e+00, /* 0x40000000 */
	/* c = (subfloat)0.84506291151 */
erx =  8.4506291151e-01, /* 0x3f58560b */
/*
 * Coefficients for approximation to  erf on [0,0.84375]
 */
efx =  1.2837916613e-01, /* 0x3e0375d4 */
efx8=  1.0270333290e+00, /* 0x3f8375d4 */
pp0  =  1.2837916613e-01, /* 0x3e0375d4 */
pp1  = -3.2504209876e-01, /* 0xbea66beb */
pp2  = -2.8481749818e-02, /* 0xbce9528f */
pp3  = -5.7702702470e-03, /* 0xbbbd1489 */
pp4  = -2.3763017452e-05, /* 0xb7c756b1 */
qq1  =  3.9791721106e-01, /* 0x3ecbbbce */
qq2  =  6.5022252500e-02, /* 0x3d852a63 */
qq3  =  5.0813062117e-03, /* 0x3ba68116 */
qq4  =  1.3249473704e-04, /* 0x390aee49 */
qq5  = -3.9602282413e-06, /* 0xb684e21a */
/*
 * Coefficients for approximation to  erf  in [0.84375,1.25]
 */
pa0  = -2.3621185683e-03, /* 0xbb1acdc6 */
pa1  =  4.1485610604e-01, /* 0x3ed46805 */
pa2  = -3.7220788002e-01, /* 0xbebe9208 */
pa3  =  3.1834661961e-01, /* 0x3ea2fe54 */
pa4  = -1.1089469492e-01, /* 0xbde31cc2 */
pa5  =  3.5478305072e-02, /* 0x3d1151b3 */
pa6  = -2.1663755178e-03, /* 0xbb0df9c0 */
qa1  =  1.0642088205e-01, /* 0x3dd9f331 */
qa2  =  5.4039794207e-01, /* 0x3f0a5785 */
qa3  =  7.1828655899e-02, /* 0x3d931ae7 */
qa4  =  1.2617121637e-01, /* 0x3e013307 */
qa5  =  1.3637083583e-02, /* 0x3c5f6e13 */
qa6  =  1.1984500103e-02, /* 0x3c445aa3 */
/*
 * Coefficients for approximation to  erfc in [1.25,1/0.35]
 */
ra0  = -9.8649440333e-03, /* 0xbc21a093 */
ra1  = -6.9385856390e-01, /* 0xbf31a0b7 */
ra2  = -1.0558626175e+01, /* 0xc128f022 */
ra3  = -6.2375331879e+01, /* 0xc2798057 */
ra4  = -1.6239666748e+02, /* 0xc322658c */
ra5  = -1.8460508728e+02, /* 0xc3389ae7 */
ra6  = -8.1287437439e+01, /* 0xc2a2932b */
ra7  = -9.8143291473e+00, /* 0xc11d077e */
sa1  =  1.9651271820e+01, /* 0x419d35ce */
sa2  =  1.3765776062e+02, /* 0x4309a863 */
sa3  =  4.3456588745e+02, /* 0x43d9486f */
sa4  =  6.4538726807e+02, /* 0x442158c9 */
sa5  =  4.2900814819e+02, /* 0x43d6810b */
sa6  =  1.0863500214e+02, /* 0x42d9451f */
sa7  =  6.5702495575e+00, /* 0x40d23f7c */
sa8  = -6.0424413532e-02, /* 0xbd777f97 */
/*
 * Coefficients for approximation to  erfc in [1/.35,28]
 */
rb0  = -9.8649431020e-03, /* 0xbc21a092 */
rb1  = -7.9928326607e-01, /* 0xbf4c9dd4 */
rb2  = -1.7757955551e+01, /* 0xc18e104b */
rb3  = -1.6063638306e+02, /* 0xc320a2ea */
rb4  = -6.3756646729e+02, /* 0xc41f6441 */
rb5  = -1.0250950928e+03, /* 0xc480230b */
rb6  = -4.8351919556e+02, /* 0xc3f1c275 */
sb1  =  3.0338060379e+01, /* 0x41f2b459 */
sb2  =  3.2579251099e+02, /* 0x43a2e571 */
sb3  =  1.5367296143e+03, /* 0x44c01759 */
sb4  =  3.1998581543e+03, /* 0x4547fdbb */
sb5  =  2.5530502930e+03, /* 0x451f90ce */
sb6  =  4.7452853394e+02, /* 0x43ed43a7 */
sb7  = -2.2440952301e+01; /* 0xc1b38712 */

DEF_VS_MATH_FUNC (v64sf, erff, v64sf x)
{
  FUNCTION_INIT (v64sf);

  v64si hx;
  GET_FLOAT_WORD (hx, x, NO_COND);
  v64si ix = hx & 0x7fffffff;

  VECTOR_IF (ix >= 0x7f800000, cond)		/* erf(nan)=nan */
    v64si i = (hx >> 31) << 1;
    /* erf(+-inf)=+-1 */
    VECTOR_RETURN (__builtin_convertvector (1 - i, v64sf) + 1.0f / x, cond);
  VECTOR_ENDIF

  VECTOR_IF (ix < 0x3f580000, cond)			/* |x|<0.84375 */
    VECTOR_IF2 (ix < 0x31800000, cond2, cond)		/* |x|<2**-28 */
      VECTOR_IF2 (ix < 0x04000000, cond3, cond2)	/* avoid underflow */
	VECTOR_RETURN (0.125f*(8.0f*x + efx8*x), cond3);
      VECTOR_ENDIF
      VECTOR_RETURN (x + efx*x, cond2);
    VECTOR_ENDIF

    v64sf z = x*x;
    v64sf r = pp0+z*(pp1+z*(pp2+z*(pp3+z*pp4)));
    v64sf s = one+z*(qq1+z*(qq2+z*(qq3+z*(qq4+z*qq5))));
    v64sf y = r/s;

    VECTOR_RETURN (x + x*y, cond);
  VECTOR_ENDIF

  VECTOR_IF (ix < 0x3fa00000, cond)	/* 0.84375 <= |x| < 1.25 */
    v64sf s = __builtin_gcn_fabsvf (x) - 1.0f;
    v64sf P = pa0+s*(pa1+s*(pa2+s*(pa3+s*(pa4+s*(pa5+s*pa6)))));
    v64sf Q = one+s*(qa1+s*(qa2+s*(qa3+s*(qa4+s*(qa5+s*qa6)))));
    VECTOR_IF2 (hx >= 0, cond2, cond)
      VECTOR_RETURN (erx + P/Q, cond2);
    VECTOR_ELSE2 (cond2, cond)
      VECTOR_RETURN (-erx - P/Q, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF

  VECTOR_IF (ix >= 0x40c00000, cond)	/* inf>|x|>=6 */
    VECTOR_IF2 (hx >= 0, cond2, cond)
      VECTOR_RETURN (VECTOR_INIT (1.0f - tiny), cond2);
    VECTOR_ELSE2 (cond2, cond)
      VECTOR_RETURN (VECTOR_INIT (tiny - 1.0f), cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF

  x = __builtin_gcn_fabsvf(x);
  v64sf s = 1.0f / (x*x);
  v64sf R, S;
  VECTOR_IF (ix < 0x4036DB6E, cond)	/* |x| < 1/0.35 */
    VECTOR_COND_MOVE (R, ra0+s*(ra1+s*(ra2+s*(ra3+s*(ra4+s*(
			 ra5+s*(ra6+s*ra7)))))), cond);
    VECTOR_COND_MOVE (S, one+s*(sa1+s*(sa2+s*(sa3+s*(sa4+s*(
			 sa5+s*(sa6+s*(sa7+s*sa8))))))), cond);
  VECTOR_ELSE (cond)			/* |x| >= 1/0.35 */
    VECTOR_COND_MOVE (R, rb0+s*(rb1+s*(rb2+s*(rb3+s*(rb4+s*(
			 rb5+s*rb6))))), cond);
    VECTOR_COND_MOVE (S, one+s*(sb1+s*(sb2+s*(sb3+s*(sb4+s*(
			 sb5+s*(sb6+s*sb7)))))), cond);
  VECTOR_ENDIF

  GET_FLOAT_WORD (ix, x, NO_COND);
  v64sf z;
  SET_FLOAT_WORD (z, ix & 0xfffff000, NO_COND);
  v64sf r = v64sf_expf_aux (-z*z - 0.5625f, __mask)
            * v64sf_expf_aux ((z-x)*(z+x) + R/S, __mask);
  VECTOR_RETURN (one - r/x, hx >= 0);
  VECTOR_RETURN (r/x - one, hx < 0);

  FUNCTION_RETURN;
}

DEF_VARIANTS (erff, sf, sf)
