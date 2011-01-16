/* e_atan2l.c -- long double version of e_atan2.c.
 * Conversion to long double by Jakub Jelinek, jj@ultra.linux.cz.
 */

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

/* atan2q(y,x)
 * Method :
 *	1. Reduce y to positive by atan2q(y,x)=-atan2q(-y,x).
 *	2. Reduce x to positive by (if x and y are unexceptional):
 *		ARG (x+iy) = arctan(y/x)   	   ... if x > 0,
 *		ARG (x+iy) = pi - arctan[y/(-x)]   ... if x < 0,
 *
 * Special cases:
 *
 *	ATAN2((anything), NaN ) is NaN;
 *	ATAN2(NAN , (anything) ) is NaN;
 *	ATAN2(+-0, +(anything but NaN)) is +-0  ;
 *	ATAN2(+-0, -(anything but NaN)) is +-pi ;
 *	ATAN2(+-(anything but 0 and NaN), 0) is +-pi/2;
 *	ATAN2(+-(anything but INF and NaN), +INF) is +-0 ;
 *	ATAN2(+-(anything but INF and NaN), -INF) is +-pi;
 *	ATAN2(+-INF,+INF ) is +-pi/4 ;
 *	ATAN2(+-INF,-INF ) is +-3pi/4;
 *	ATAN2(+-INF, (anything but,0,NaN, and INF)) is +-pi/2;
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following
 * constants. The decimal values may be used, provided that the
 * compiler will convert from decimal to binary accurately enough
 * to produce the hexadecimal values shown.
 */

#include "quadmath-imp.h"

static const __float128
tiny  = 1.0e-4900Q,
zero  = 0.0,
pi_o_4  = 7.85398163397448309615660845819875699e-01Q, /* 3ffe921fb54442d18469898cc51701b8 */
pi_o_2  = 1.57079632679489661923132169163975140e+00Q, /* 3fff921fb54442d18469898cc51701b8 */
pi      = 3.14159265358979323846264338327950280e+00Q, /* 4000921fb54442d18469898cc51701b8 */
pi_lo   = 8.67181013012378102479704402604335225e-35Q; /* 3f8dcd129024e088a67cc74020bbea64 */

__float128
atan2q (__float128 y, __float128 x)
{
	__float128 z;
	int64_t k,m,hx,hy,ix,iy;
	uint64_t lx,ly;

	GET_FLT128_WORDS64(hx,lx,x);
	ix = hx&0x7fffffffffffffffLL;
	GET_FLT128_WORDS64(hy,ly,y);
	iy = hy&0x7fffffffffffffffLL;
	if(((ix|((lx|-lx)>>63))>0x7fff000000000000LL)||
	   ((iy|((ly|-ly)>>63))>0x7fff000000000000LL))	/* x or y is NaN */
	   return x+y;
	if(((hx-0x3fff000000000000LL)|lx)==0) return atanq(y);   /* x=1.0Q */
	m = ((hy>>63)&1)|((hx>>62)&2);	/* 2*sign(x)+sign(y) */

    /* when y = 0 */
	if((iy|ly)==0) {
	    switch(m) {
		case 0:
		case 1: return y; 	/* atan(+-0,+anything)=+-0 */
		case 2: return  pi+tiny;/* atan(+0,-anything) = pi */
		case 3: return -pi-tiny;/* atan(-0,-anything) =-pi */
	    }
	}
    /* when x = 0 */
	if((ix|lx)==0) return (hy<0)?  -pi_o_2-tiny: pi_o_2+tiny;

    /* when x is INF */
	if(ix==0x7fff000000000000LL) {
	    if(iy==0x7fff000000000000LL) {
		switch(m) {
		    case 0: return  pi_o_4+tiny;/* atan(+INF,+INF) */
		    case 1: return -pi_o_4-tiny;/* atan(-INF,+INF) */
		    case 2: return  3.0Q*pi_o_4+tiny;/*atan(+INF,-INF)*/
		    case 3: return -3.0Q*pi_o_4-tiny;/*atan(-INF,-INF)*/
		}
	    } else {
		switch(m) {
		    case 0: return  zero  ;	/* atan(+...,+INF) */
		    case 1: return -zero  ;	/* atan(-...,+INF) */
		    case 2: return  pi+tiny  ;	/* atan(+...,-INF) */
		    case 3: return -pi-tiny  ;	/* atan(-...,-INF) */
		}
	    }
	}
    /* when y is INF */
	if(iy==0x7fff000000000000LL) return (hy<0)? -pi_o_2-tiny: pi_o_2+tiny;

    /* compute y/x */
	k = (iy-ix)>>48;
	if(k > 120) z=pi_o_2+0.5Q*pi_lo; 	/* |y/x| >  2**120 */
	else if(hx<0&&k<-120) z=0.0Q; 		/* |y|/x < -2**120 */
	else z=atanq(fabsq(y/x));		/* safe to do y/x */
	switch (m) {
	    case 0: return       z  ;	/* atan(+,+) */
	    case 1: {
	    	      uint64_t zh;
		      GET_FLT128_MSW64(zh,z);
		      SET_FLT128_MSW64(z,zh ^ 0x8000000000000000ULL);
		    }
		    return       z  ;	/* atan(-,+) */
	    case 2: return  pi-(z-pi_lo);/* atan(+,-) */
	    default: /* case 3 */
	    	    return  (z-pi_lo)-pi;/* atan(-,-) */
	}
}
