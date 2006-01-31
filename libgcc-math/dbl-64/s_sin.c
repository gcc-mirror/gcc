/*
 * IBM Accurate Mathematical Library
 * written by International Business Machines Corp.
 * Copyright (C) 2001 Free Software Foundation
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU  Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
/****************************************************************************/
/*                                                                          */
/* MODULE_NAME:usncs.c                                                      */
/*                                                                          */
/* FUNCTIONS: usin                                                          */
/*            ucos                                                          */
/*            slow                                                          */
/*            slow1                                                         */
/*            slow2                                                         */
/*            sloww                                                         */
/*            sloww1                                                        */
/*            sloww2                                                        */
/*            bsloww                                                        */
/*            bsloww1                                                       */
/*            bsloww2                                                       */
/*            cslow2                                                        */
/*            csloww                                                        */
/*            csloww1                                                       */
/*            csloww2                                                       */
/* FILES NEEDED: dla.h endian.h mpa.h mydefs.h  usncs.h                     */
/*               branred.c sincos32.c dosincos.c mpa.c                      */
/*               sincos.tbl                                                 */
/*                                                                          */
/* An ultimate sin and  routine. Given an IEEE double machine number x       */
/* it computes the correctly rounded (to nearest) value of sin(x) or cos(x) */
/* Assumption: Machine arithmetic operations are performed in               */
/* round to nearest mode of IEEE 754 standard.                              */
/*                                                                          */
/****************************************************************************/


#include "endian.h"
#include "mydefs.h"
#include "usncs.h"
#include "MathLib.h"
#include "sincos.tbl"
#include "math_private.h"

static const double
          sn3 = -1.66666666666664880952546298448555E-01,
          sn5 =  8.33333214285722277379541354343671E-03,
          cs2 =  4.99999999999999999999950396842453E-01,
          cs4 = -4.16666666666664434524222570944589E-02,
          cs6 =  1.38888874007937613028114285595617E-03;

void __dubsin(double x, double dx, double w[]);
void __docos(double x, double dx, double w[]);
double __mpsin(double x, double dx);
double __mpcos(double x, double dx);
double __mpsin1(double x);
double __mpcos1(double x);
static double slow(double x);
static double slow1(double x);
static double slow2(double x);
static double sloww(double x, double dx, double orig);
static double sloww1(double x, double dx, double orig);
static double sloww2(double x, double dx, double orig, int n);
static double bsloww(double x, double dx, double orig, int n);
static double bsloww1(double x, double dx, double orig, int n);
static double bsloww2(double x, double dx, double orig, int n);
int __branred(double x, double *a, double *aa);
static double cslow2(double x);
static double csloww(double x, double dx, double orig);
static double csloww1(double x, double dx, double orig);
static double csloww2(double x, double dx, double orig, int n);
/*******************************************************************/
/* An ultimate sin routine. Given an IEEE double machine number x   */
/* it computes the correctly rounded (to nearest) value of sin(x)  */
/*******************************************************************/
double __sin(double x){
	double xx,res,t,cor,y,s,c,sn,ssn,cs,ccs,xn,a,da,db,eps,xn1,xn2;
#if 0
	double w[2];
#endif
	mynumber u,v;
	int4 k,m,n;
#if 0
	int4 nn;
#endif

	u.x = x;
	m = u.i[HIGH_HALF];
	k = 0x7fffffff&m;              /* no sign           */
	if (k < 0x3e500000)            /* if x->0 =>sin(x)=x */
	 return x;
 /*---------------------------- 2^-26 < |x|< 0.25 ----------------------*/
	else  if (k < 0x3fd00000){
	  xx = x*x;
	  /*Taylor series */
	  t = ((((s5.x*xx + s4.x)*xx + s3.x)*xx + s2.x)*xx + s1.x)*(xx*x);
	  res = x+t;
	  cor = (x-res)+t;
	  return (res == res + 1.07*cor)? res : slow(x);
	}    /*  else  if (k < 0x3fd00000)    */
/*---------------------------- 0.25<|x|< 0.855469---------------------- */
	else if (k < 0x3feb6000)  {
	  u.x=(m>0)?big.x+x:big.x-x;
	  y=(m>0)?x-(u.x-big.x):x+(u.x-big.x);
	  xx=y*y;
	  s = y + y*xx*(sn3 +xx*sn5);
	  c = xx*(cs2 +xx*(cs4 + xx*cs6));
	  k=u.i[LOW_HALF]<<2;
	  sn=(m>0)?sincos.x[k]:-sincos.x[k];
	  ssn=(m>0)?sincos.x[k+1]:-sincos.x[k+1];
	  cs=sincos.x[k+2];
	  ccs=sincos.x[k+3];
	  cor=(ssn+s*ccs-sn*c)+cs*s;
	  res=sn+cor;
	  cor=(sn-res)+cor;
	  return (res==res+1.025*cor)? res : slow1(x);
	}    /*   else  if (k < 0x3feb6000)    */

/*----------------------- 0.855469  <|x|<2.426265  ----------------------*/
	else if (k <  0x400368fd ) {

	  y = (m>0)? hp0.x-x:hp0.x+x;
	  if (y>=0) {
	    u.x = big.x+y;
	    y = (y-(u.x-big.x))+hp1.x;
	  }
	  else {
	    u.x = big.x-y;
	    y = (-hp1.x) - (y+(u.x-big.x));
	  }
	  xx=y*y;
	  s = y + y*xx*(sn3 +xx*sn5);
	  c = xx*(cs2 +xx*(cs4 + xx*cs6));
	  k=u.i[LOW_HALF]<<2;
	  sn=sincos.x[k];
	  ssn=sincos.x[k+1];
	  cs=sincos.x[k+2];
	  ccs=sincos.x[k+3];
	  cor=(ccs-s*ssn-cs*c)-sn*s;
	  res=cs+cor;
	  cor=(cs-res)+cor;
	  return (res==res+1.020*cor)? ((m>0)?res:-res) : slow2(x);
	} /*   else  if (k < 0x400368fd)    */

/*-------------------------- 2.426265<|x|< 105414350 ----------------------*/
	else if (k < 0x419921FB ) {
	  t = (x*hpinv.x + toint.x);
	  xn = t - toint.x;
	  v.x = t;
	  y = (x - xn*mp1.x) - xn*mp2.x;
	  n =v.i[LOW_HALF]&3;
	  da = xn*mp3.x;
	  a=y-da;
	  da = (y-a)-da;
	  eps = ABS(x)*1.2e-30;

	  switch (n) { /* quarter of unit circle */
	  case 0:
	  case 2:
	    xx = a*a;
	    if (n) {a=-a;da=-da;}
	    if (xx < 0.01588) {
                      /*Taylor series */
	      t = (((((s5.x*xx + s4.x)*xx + s3.x)*xx + s2.x)*xx + s1.x)*a - 0.5*da)*xx+da;
	      res = a+t;
	      cor = (a-res)+t;
	      cor = (cor>0)? 1.02*cor+eps : 1.02*cor -eps;
	      return (res == res + cor)? res : sloww(a,da,x);
	    }
	    else  {
	      if (a>0)
		{m=1;t=a;db=da;}
	      else
		{m=0;t=-a;db=-da;}
	      u.x=big.x+t;
	      y=t-(u.x-big.x);
	      xx=y*y;
	      s = y + (db+y*xx*(sn3 +xx*sn5));
	      c = y*db+xx*(cs2 +xx*(cs4 + xx*cs6));
	      k=u.i[LOW_HALF]<<2;
	      sn=sincos.x[k];
	      ssn=sincos.x[k+1];
	      cs=sincos.x[k+2];
	      ccs=sincos.x[k+3];
	      cor=(ssn+s*ccs-sn*c)+cs*s;
	      res=sn+cor;
	      cor=(sn-res)+cor;
	      cor = (cor>0)? 1.035*cor+eps : 1.035*cor-eps;
	      return (res==res+cor)? ((m)?res:-res) : sloww1(a,da,x);
	    }
	    break;

	  case 1:
	  case 3:
	    if (a<0)
	      {a=-a;da=-da;}
	    u.x=big.x+a;
	    y=a-(u.x-big.x)+da;
	    xx=y*y;
	    k=u.i[LOW_HALF]<<2;
	    sn=sincos.x[k];
	    ssn=sincos.x[k+1];
	    cs=sincos.x[k+2];
	    ccs=sincos.x[k+3];
	    s = y + y*xx*(sn3 +xx*sn5);
	    c = xx*(cs2 +xx*(cs4 + xx*cs6));
	    cor=(ccs-s*ssn-cs*c)-sn*s;
	    res=cs+cor;
	    cor=(cs-res)+cor;
	    cor = (cor>0)? 1.025*cor+eps : 1.025*cor-eps;
	    return (res==res+cor)? ((n&2)?-res:res) : sloww2(a,da,x,n);

	    break;

	  }

	}    /*   else  if (k <  0x419921FB )    */

/*---------------------105414350 <|x|< 281474976710656 --------------------*/
	else if (k < 0x42F00000 ) {
	  t = (x*hpinv.x + toint.x);
	  xn = t - toint.x;
	  v.x = t;
	  xn1 = (xn+8.0e22)-8.0e22;
	  xn2 = xn - xn1;
	  y = ((((x - xn1*mp1.x) - xn1*mp2.x)-xn2*mp1.x)-xn2*mp2.x);
	  n =v.i[LOW_HALF]&3;
	  da = xn1*pp3.x;
	  t=y-da;
	  da = (y-t)-da;
	  da = (da - xn2*pp3.x) -xn*pp4.x;
	  a = t+da;
	  da = (t-a)+da;
	  eps = 1.0e-24;

	  switch (n) {
	  case 0:
	  case 2:
	    xx = a*a;
	    if (n) {a=-a;da=-da;}
	    if (xx < 0.01588) {
              /* Taylor series */
	      t = (((((s5.x*xx + s4.x)*xx + s3.x)*xx + s2.x)*xx + s1.x)*a - 0.5*da)*xx+da;
	      res = a+t;
	      cor = (a-res)+t;
	      cor = (cor>0)? 1.02*cor+eps : 1.02*cor -eps;
	      return (res == res + cor)? res : bsloww(a,da,x,n);
	    }
	    else  {
	      if (a>0) {m=1;t=a;db=da;}
	      else {m=0;t=-a;db=-da;}
	      u.x=big.x+t;
	      y=t-(u.x-big.x);
	      xx=y*y;
	      s = y + (db+y*xx*(sn3 +xx*sn5));
	      c = y*db+xx*(cs2 +xx*(cs4 + xx*cs6));
	      k=u.i[LOW_HALF]<<2;
	      sn=sincos.x[k];
	      ssn=sincos.x[k+1];
	      cs=sincos.x[k+2];
	      ccs=sincos.x[k+3];
	      cor=(ssn+s*ccs-sn*c)+cs*s;
	      res=sn+cor;
	      cor=(sn-res)+cor;
	      cor = (cor>0)? 1.035*cor+eps : 1.035*cor-eps;
	      return (res==res+cor)? ((m)?res:-res) : bsloww1(a,da,x,n);
		   }
	    break;

	  case 1:
	  case 3:
	    if (a<0)
	      {a=-a;da=-da;}
	    u.x=big.x+a;
	    y=a-(u.x-big.x)+da;
	    xx=y*y;
	    k=u.i[LOW_HALF]<<2;
	    sn=sincos.x[k];
	    ssn=sincos.x[k+1];
	    cs=sincos.x[k+2];
	    ccs=sincos.x[k+3];
	    s = y + y*xx*(sn3 +xx*sn5);
	    c = xx*(cs2 +xx*(cs4 + xx*cs6));
	    cor=(ccs-s*ssn-cs*c)-sn*s;
	    res=cs+cor;
	    cor=(cs-res)+cor;
	    cor = (cor>0)? 1.025*cor+eps : 1.025*cor-eps;
	    return (res==res+cor)? ((n&2)?-res:res) : bsloww2(a,da,x,n);

	    break;

	  }

	}    /*   else  if (k <  0x42F00000 )   */

/* -----------------281474976710656 <|x| <2^1024----------------------------*/
	else if (k < 0x7ff00000) {

	  n = __branred(x,&a,&da);
	  switch (n) {
	  case 0:
	    if (a*a < 0.01588) return bsloww(a,da,x,n);
	    else return bsloww1(a,da,x,n);
	    break;
	  case 2:
	    if (a*a < 0.01588) return bsloww(-a,-da,x,n);
	    else return bsloww1(-a,-da,x,n);
	    break;

	  case 1:
	  case 3:
	    return  bsloww2(a,da,x,n);
	    break;
	  }

	}    /*   else  if (k <  0x7ff00000 )    */

/*--------------------- |x| > 2^1024 ----------------------------------*/
	else return x / x;
	return 0;         /* unreachable */
}


/*******************************************************************/
/* An ultimate cos routine. Given an IEEE double machine number x   */
/* it computes the correctly rounded (to nearest) value of cos(x)  */
/*******************************************************************/

double __cos(double x)
{
  double y,xx,res,t,cor,s,c,sn,ssn,cs,ccs,xn,a,da,db,eps,xn1,xn2;
  mynumber u,v;
  int4 k,m,n;

  u.x = x;
  m = u.i[HIGH_HALF];
  k = 0x7fffffff&m;

  if (k < 0x3e400000 ) return 1.0; /* |x|<2^-27 => cos(x)=1 */

  else if (k < 0x3feb6000 ) {/* 2^-27 < |x| < 0.855469 */
    y=ABS(x);
    u.x = big.x+y;
    y = y-(u.x-big.x);
    xx=y*y;
    s = y + y*xx*(sn3 +xx*sn5);
    c = xx*(cs2 +xx*(cs4 + xx*cs6));
    k=u.i[LOW_HALF]<<2;
    sn=sincos.x[k];
    ssn=sincos.x[k+1];
    cs=sincos.x[k+2];
    ccs=sincos.x[k+3];
    cor=(ccs-s*ssn-cs*c)-sn*s;
    res=cs+cor;
    cor=(cs-res)+cor;
    return (res==res+1.020*cor)? res : cslow2(x);

}    /*   else  if (k < 0x3feb6000)    */

  else if (k <  0x400368fd ) {/* 0.855469  <|x|<2.426265  */;
    y=hp0.x-ABS(x);
    a=y+hp1.x;
    da=(y-a)+hp1.x;
    xx=a*a;
    if (xx < 0.01588) {
      t = (((((s5.x*xx + s4.x)*xx + s3.x)*xx + s2.x)*xx + s1.x)*a - 0.5*da)*xx+da;
      res = a+t;
      cor = (a-res)+t;
      cor = (cor>0)? 1.02*cor+1.0e-31 : 1.02*cor -1.0e-31;
      return (res == res + cor)? res : csloww(a,da,x);
    }
    else  {
      if (a>0) {m=1;t=a;db=da;}
      else {m=0;t=-a;db=-da;}
      u.x=big.x+t;
      y=t-(u.x-big.x);
      xx=y*y;
      s = y + (db+y*xx*(sn3 +xx*sn5));
      c = y*db+xx*(cs2 +xx*(cs4 + xx*cs6));
      k=u.i[LOW_HALF]<<2;
      sn=sincos.x[k];
      ssn=sincos.x[k+1];
      cs=sincos.x[k+2];
      ccs=sincos.x[k+3];
      cor=(ssn+s*ccs-sn*c)+cs*s;
      res=sn+cor;
      cor=(sn-res)+cor;
      cor = (cor>0)? 1.035*cor+1.0e-31 : 1.035*cor-1.0e-31;
      return (res==res+cor)? ((m)?res:-res) : csloww1(a,da,x);
}

}    /*   else  if (k < 0x400368fd)    */


  else if (k < 0x419921FB ) {/* 2.426265<|x|< 105414350 */
    t = (x*hpinv.x + toint.x);
    xn = t - toint.x;
    v.x = t;
    y = (x - xn*mp1.x) - xn*mp2.x;
    n =v.i[LOW_HALF]&3;
    da = xn*mp3.x;
    a=y-da;
    da = (y-a)-da;
    eps = ABS(x)*1.2e-30;

    switch (n) {
    case 1:
    case 3:
      xx = a*a;
      if (n == 1) {a=-a;da=-da;}
      if (xx < 0.01588) {
	t = (((((s5.x*xx + s4.x)*xx + s3.x)*xx + s2.x)*xx + s1.x)*a - 0.5*da)*xx+da;
	res = a+t;
	cor = (a-res)+t;
	cor = (cor>0)? 1.02*cor+eps : 1.02*cor -eps;
	return (res == res + cor)? res : csloww(a,da,x);
      }
      else  {
	if (a>0) {m=1;t=a;db=da;}
	else {m=0;t=-a;db=-da;}
	u.x=big.x+t;
	y=t-(u.x-big.x);
	xx=y*y;
	s = y + (db+y*xx*(sn3 +xx*sn5));
	c = y*db+xx*(cs2 +xx*(cs4 + xx*cs6));
	k=u.i[LOW_HALF]<<2;
	sn=sincos.x[k];
	ssn=sincos.x[k+1];
	cs=sincos.x[k+2];
	ccs=sincos.x[k+3];
	cor=(ssn+s*ccs-sn*c)+cs*s;
	res=sn+cor;
	cor=(sn-res)+cor;
	cor = (cor>0)? 1.035*cor+eps : 1.035*cor-eps;
	return (res==res+cor)? ((m)?res:-res) : csloww1(a,da,x);
      }
      break;

  case 0:
    case 2:
      if (a<0) {a=-a;da=-da;}
      u.x=big.x+a;
      y=a-(u.x-big.x)+da;
      xx=y*y;
      k=u.i[LOW_HALF]<<2;
      sn=sincos.x[k];
      ssn=sincos.x[k+1];
      cs=sincos.x[k+2];
      ccs=sincos.x[k+3];
      s = y + y*xx*(sn3 +xx*sn5);
      c = xx*(cs2 +xx*(cs4 + xx*cs6));
      cor=(ccs-s*ssn-cs*c)-sn*s;
      res=cs+cor;
      cor=(cs-res)+cor;
      cor = (cor>0)? 1.025*cor+eps : 1.025*cor-eps;
      return (res==res+cor)? ((n)?-res:res) : csloww2(a,da,x,n);

           break;

    }

  }    /*   else  if (k <  0x419921FB )    */


  else if (k < 0x42F00000 ) {
    t = (x*hpinv.x + toint.x);
    xn = t - toint.x;
    v.x = t;
    xn1 = (xn+8.0e22)-8.0e22;
    xn2 = xn - xn1;
    y = ((((x - xn1*mp1.x) - xn1*mp2.x)-xn2*mp1.x)-xn2*mp2.x);
    n =v.i[LOW_HALF]&3;
    da = xn1*pp3.x;
    t=y-da;
    da = (y-t)-da;
    da = (da - xn2*pp3.x) -xn*pp4.x;
    a = t+da;
    da = (t-a)+da;
    eps = 1.0e-24;

    switch (n) {
    case 1:
    case 3:
      xx = a*a;
      if (n==1) {a=-a;da=-da;}
      if (xx < 0.01588) {
	t = (((((s5.x*xx + s4.x)*xx + s3.x)*xx + s2.x)*xx + s1.x)*a - 0.5*da)*xx+da;
	res = a+t;
	cor = (a-res)+t;
	cor = (cor>0)? 1.02*cor+eps : 1.02*cor -eps;
	return (res == res + cor)? res : bsloww(a,da,x,n);
      }
      else  {
	if (a>0) {m=1;t=a;db=da;}
	else {m=0;t=-a;db=-da;}
	u.x=big.x+t;
	y=t-(u.x-big.x);
	xx=y*y;
	s = y + (db+y*xx*(sn3 +xx*sn5));
	c = y*db+xx*(cs2 +xx*(cs4 + xx*cs6));
	k=u.i[LOW_HALF]<<2;
	sn=sincos.x[k];
	ssn=sincos.x[k+1];
	cs=sincos.x[k+2];
	ccs=sincos.x[k+3];
	cor=(ssn+s*ccs-sn*c)+cs*s;
	res=sn+cor;
	cor=(sn-res)+cor;
	cor = (cor>0)? 1.035*cor+eps : 1.035*cor-eps;
	return (res==res+cor)? ((m)?res:-res) : bsloww1(a,da,x,n);
      }
      break;

    case 0:
    case 2:
      if (a<0) {a=-a;da=-da;}
      u.x=big.x+a;
      y=a-(u.x-big.x)+da;
      xx=y*y;
      k=u.i[LOW_HALF]<<2;
      sn=sincos.x[k];
      ssn=sincos.x[k+1];
      cs=sincos.x[k+2];
      ccs=sincos.x[k+3];
      s = y + y*xx*(sn3 +xx*sn5);
      c = xx*(cs2 +xx*(cs4 + xx*cs6));
      cor=(ccs-s*ssn-cs*c)-sn*s;
      res=cs+cor;
      cor=(cs-res)+cor;
      cor = (cor>0)? 1.025*cor+eps : 1.025*cor-eps;
      return (res==res+cor)? ((n)?-res:res) : bsloww2(a,da,x,n);
      break;

    }

  }    /*   else  if (k <  0x42F00000 )    */

  else if (k < 0x7ff00000) {/* 281474976710656 <|x| <2^1024 */

    n = __branred(x,&a,&da);
    switch (n) {
    case 1:
      if (a*a < 0.01588) return bsloww(-a,-da,x,n);
      else return bsloww1(-a,-da,x,n);
      break;
		case 3:
		  if (a*a < 0.01588) return bsloww(a,da,x,n);
		  else return bsloww1(a,da,x,n);
		  break;

    case 0:
    case 2:
      return  bsloww2(a,da,x,n);
      break;
    }

  }    /*   else  if (k <  0x7ff00000 )    */




  else return x / x; /* |x| > 2^1024 */
  return 0;

}

/************************************************************************/
/*  Routine compute sin(x) for  2^-26 < |x|< 0.25 by  Taylor with more   */
/* precision  and if still doesn't accurate enough by mpsin   or dubsin */
/************************************************************************/

static double slow(double x) {
static const double th2_36 = 206158430208.0;   /*    1.5*2**37   */
 double y,x1,x2,xx,r,t,res,cor,w[2];
 x1=(x+th2_36)-th2_36;
 y = aa.x*x1*x1*x1;
 r=x+y;
 x2=x-x1;
 xx=x*x;
 t = (((((s5.x*xx + s4.x)*xx + s3.x)*xx + s2.x)*xx + bb.x)*xx + 3.0*aa.x*x1*x2)*x +aa.x*x2*x2*x2;
 t=((x-r)+y)+t;
 res=r+t;
 cor = (r-res)+t;
 if (res == res + 1.0007*cor) return res;
 else {
   __dubsin(ABS(x),0,w);
   if (w[0] == w[0]+1.000000001*w[1]) return (x>0)?w[0]:-w[0];
   else return (x>0)?__mpsin(x,0):-__mpsin(-x,0);
 }
}
/*******************************************************************************/
/* Routine compute sin(x) for   0.25<|x|< 0.855469 by  sincos.tbl   and Taylor */
/* and if result still doesn't accurate enough by mpsin   or dubsin            */
/*******************************************************************************/

static double slow1(double x) {
  mynumber u;
  double sn,ssn,cs,ccs,s,c,w[2],y,y1,y2,c1,c2,xx,cor,res;
  static const double t22 = 6291456.0;
  int4 k;
  y=ABS(x);
  u.x=big.x+y;
  y=y-(u.x-big.x);
  xx=y*y;
  s = y*xx*(sn3 +xx*sn5);
  c = xx*(cs2 +xx*(cs4 + xx*cs6));
  k=u.i[LOW_HALF]<<2;
  sn=sincos.x[k];          /* Data          */
  ssn=sincos.x[k+1];       /*  from         */
  cs=sincos.x[k+2];        /*   tables      */
  ccs=sincos.x[k+3];       /*    sincos.tbl */
  y1 = (y+t22)-t22;
  y2 = y - y1;
  c1 = (cs+t22)-t22;
  c2=(cs-c1)+ccs;
  cor=(ssn+s*ccs+cs*s+c2*y+c1*y2)-sn*c;
  y=sn+c1*y1;
  cor = cor+((sn-y)+c1*y1);
  res=y+cor;
  cor=(y-res)+cor;
  if (res == res+1.0005*cor) return (x>0)?res:-res;
  else {
    __dubsin(ABS(x),0,w);
    if (w[0] == w[0]+1.000000005*w[1]) return (x>0)?w[0]:-w[0];
    else return (x>0)?__mpsin(x,0):-__mpsin(-x,0);
  }
}
/**************************************************************************/
/*  Routine compute sin(x) for   0.855469  <|x|<2.426265  by  sincos.tbl  */
/* and if result still doesn't accurate enough by mpsin   or dubsin       */
/**************************************************************************/
static double slow2(double x) {
  mynumber u;
  double sn,ssn,cs,ccs,s,c,w[2],y,y1,y2,e1,e2,xx,cor,res,del;
  static const double t22 = 6291456.0;
  int4 k;
  y=ABS(x);
  y = hp0.x-y;
  if (y>=0) {
    u.x = big.x+y;
    y = y-(u.x-big.x);
    del = hp1.x;
  }
  else {
    u.x = big.x-y;
    y = -(y+(u.x-big.x));
    del = -hp1.x;
  }
  xx=y*y;
  s = y*xx*(sn3 +xx*sn5);
  c = y*del+xx*(cs2 +xx*(cs4 + xx*cs6));
  k=u.i[LOW_HALF]<<2;
  sn=sincos.x[k];
  ssn=sincos.x[k+1];
  cs=sincos.x[k+2];
  ccs=sincos.x[k+3];
  y1 = (y+t22)-t22;
  y2 = (y - y1)+del;
  e1 = (sn+t22)-t22;
  e2=(sn-e1)+ssn;
  cor=(ccs-cs*c-e1*y2-e2*y)-sn*s;
  y=cs-e1*y1;
  cor = cor+((cs-y)-e1*y1);
  res=y+cor;
  cor=(y-res)+cor;
  if (res == res+1.0005*cor) return (x>0)?res:-res;
  else {
    y=ABS(x)-hp0.x;
    y1=y-hp1.x;
    y2=(y-y1)-hp1.x;
    __docos(y1,y2,w);
    if (w[0] == w[0]+1.000000005*w[1]) return (x>0)?w[0]:-w[0];
    else return (x>0)?__mpsin(x,0):-__mpsin(-x,0);
  }
}
/***************************************************************************/
/*  Routine compute sin(x+dx) (Double-Length number) where x is small enough*/
/* to use Taylor series around zero and   (x+dx)                            */
/* in first or third quarter of unit circle.Routine receive also            */
/* (right argument) the  original   value of x for computing error of      */
/* result.And if result not accurate enough routine calls mpsin1 or dubsin */
/***************************************************************************/

static double sloww(double x,double dx, double orig) {
  static const double th2_36 = 206158430208.0;   /*    1.5*2**37   */
  double y,x1,x2,xx,r,t,res,cor,w[2],a,da,xn;
  union {int4 i[2]; double x;} v;
  int4 n;
  x1=(x+th2_36)-th2_36;
  y = aa.x*x1*x1*x1;
  r=x+y;
  x2=(x-x1)+dx;
  xx=x*x;
  t = (((((s5.x*xx + s4.x)*xx + s3.x)*xx + s2.x)*xx + bb.x)*xx + 3.0*aa.x*x1*x2)*x +aa.x*x2*x2*x2+dx;
  t=((x-r)+y)+t;
  res=r+t;
  cor = (r-res)+t;
  cor = (cor>0)? 1.0005*cor+ABS(orig)*3.1e-30 : 1.0005*cor-ABS(orig)*3.1e-30;
  if (res == res + cor) return res;
  else {
    (x>0)? __dubsin(x,dx,w) : __dubsin(-x,-dx,w);
    cor = (w[1]>0)? 1.000000001*w[1] + ABS(orig)*1.1e-30 : 1.000000001*w[1] - ABS(orig)*1.1e-30;
    if (w[0] == w[0]+cor) return (x>0)?w[0]:-w[0];
    else {
      t = (orig*hpinv.x + toint.x);
      xn = t - toint.x;
      v.x = t;
      y = (orig - xn*mp1.x) - xn*mp2.x;
      n =v.i[LOW_HALF]&3;
      da = xn*pp3.x;
      t=y-da;
      da = (y-t)-da;
      y = xn*pp4.x;
      a = t - y;
      da = ((t-a)-y)+da;
      if (n&2) {a=-a; da=-da;}
      (a>0)? __dubsin(a,da,w) : __dubsin(-a,-da,w);
      cor = (w[1]>0)? 1.000000001*w[1] + ABS(orig)*1.1e-40 : 1.000000001*w[1] - ABS(orig)*1.1e-40;
      if (w[0] == w[0]+cor) return (a>0)?w[0]:-w[0];
      else return __mpsin1(orig);
    }
  }
}
/***************************************************************************/
/*  Routine compute sin(x+dx)   (Double-Length number) where x in first or  */
/*  third quarter of unit circle.Routine receive also (right argument) the  */
/*  original   value of x for computing error of result.And if result not  */
/* accurate enough routine calls  mpsin1   or dubsin                       */
/***************************************************************************/

static double sloww1(double x, double dx, double orig) {
  mynumber u;
  double sn,ssn,cs,ccs,s,c,w[2],y,y1,y2,c1,c2,xx,cor,res;
  static const double t22 = 6291456.0;
  int4 k;
  y=ABS(x);
  u.x=big.x+y;
  y=y-(u.x-big.x);
  dx=(x>0)?dx:-dx;
  xx=y*y;
  s = y*xx*(sn3 +xx*sn5);
  c = xx*(cs2 +xx*(cs4 + xx*cs6));
  k=u.i[LOW_HALF]<<2;
  sn=sincos.x[k];
  ssn=sincos.x[k+1];
  cs=sincos.x[k+2];
  ccs=sincos.x[k+3];
  y1 = (y+t22)-t22;
  y2 = (y - y1)+dx;
  c1 = (cs+t22)-t22;
  c2=(cs-c1)+ccs;
  cor=(ssn+s*ccs+cs*s+c2*y+c1*y2-sn*y*dx)-sn*c;
  y=sn+c1*y1;
  cor = cor+((sn-y)+c1*y1);
  res=y+cor;
  cor=(y-res)+cor;
  cor = (cor>0)? 1.0005*cor+3.1e-30*ABS(orig) : 1.0005*cor-3.1e-30*ABS(orig);
  if (res == res + cor) return (x>0)?res:-res;
  else {
    __dubsin(ABS(x),dx,w);
    cor = (w[1]>0)? 1.000000005*w[1]+1.1e-30*ABS(orig) : 1.000000005*w[1]-1.1e-30*ABS(orig);
    if (w[0] == w[0]+cor) return (x>0)?w[0]:-w[0];
  else  return __mpsin1(orig);
  }
}
/***************************************************************************/
/*  Routine compute sin(x+dx)   (Double-Length number) where x in second or */
/*  fourth quarter of unit circle.Routine receive also  the  original value */
/* and quarter(n= 1or 3)of x for computing error of result.And if result not*/
/* accurate enough routine calls  mpsin1   or dubsin                       */
/***************************************************************************/

static double sloww2(double x, double dx, double orig, int n) {
  mynumber u;
  double sn,ssn,cs,ccs,s,c,w[2],y,y1,y2,e1,e2,xx,cor,res;
  static const double t22 = 6291456.0;
  int4 k;
  y=ABS(x);
  u.x=big.x+y;
  y=y-(u.x-big.x);
  dx=(x>0)?dx:-dx;
  xx=y*y;
  s = y*xx*(sn3 +xx*sn5);
  c = y*dx+xx*(cs2 +xx*(cs4 + xx*cs6));
  k=u.i[LOW_HALF]<<2;
  sn=sincos.x[k];
  ssn=sincos.x[k+1];
  cs=sincos.x[k+2];
  ccs=sincos.x[k+3];

  y1 = (y+t22)-t22;
  y2 = (y - y1)+dx;
  e1 = (sn+t22)-t22;
  e2=(sn-e1)+ssn;
  cor=(ccs-cs*c-e1*y2-e2*y)-sn*s;
  y=cs-e1*y1;
  cor = cor+((cs-y)-e1*y1);
  res=y+cor;
  cor=(y-res)+cor;
  cor = (cor>0)? 1.0005*cor+3.1e-30*ABS(orig) : 1.0005*cor-3.1e-30*ABS(orig);
  if (res == res + cor) return (n&2)?-res:res;
  else {
   __docos(ABS(x),dx,w);
   cor = (w[1]>0)? 1.000000005*w[1]+1.1e-30*ABS(orig) : 1.000000005*w[1]-1.1e-30*ABS(orig);
   if (w[0] == w[0]+cor) return (n&2)?-w[0]:w[0];
   else  return __mpsin1(orig);
  }
}
/***************************************************************************/
/*  Routine compute sin(x+dx) or cos(x+dx) (Double-Length number) where x   */
/* is small enough to use Taylor series around zero and   (x+dx)            */
/* in first or third quarter of unit circle.Routine receive also            */
/* (right argument) the  original   value of x for computing error of      */
/* result.And if result not accurate enough routine calls other routines    */
/***************************************************************************/

static double bsloww(double x,double dx, double orig,int n) {
  static const double th2_36 = 206158430208.0;   /*    1.5*2**37   */
  double y,x1,x2,xx,r,t,res,cor,w[2];
#if 0
  double a,da,xn;
  union {int4 i[2]; double x;} v;
#endif
  x1=(x+th2_36)-th2_36;
  y = aa.x*x1*x1*x1;
  r=x+y;
  x2=(x-x1)+dx;
  xx=x*x;
  t = (((((s5.x*xx + s4.x)*xx + s3.x)*xx + s2.x)*xx + bb.x)*xx + 3.0*aa.x*x1*x2)*x +aa.x*x2*x2*x2+dx;
  t=((x-r)+y)+t;
  res=r+t;
  cor = (r-res)+t;
  cor = (cor>0)? 1.0005*cor+1.1e-24 : 1.0005*cor-1.1e-24;
  if (res == res + cor) return res;
  else {
    (x>0)? __dubsin(x,dx,w) : __dubsin(-x,-dx,w);
    cor = (w[1]>0)? 1.000000001*w[1] + 1.1e-24 : 1.000000001*w[1] - 1.1e-24;
    if (w[0] == w[0]+cor) return (x>0)?w[0]:-w[0];
    else return (n&1)?__mpcos1(orig):__mpsin1(orig);
  }
}

/***************************************************************************/
/*  Routine compute sin(x+dx)  or cos(x+dx) (Double-Length number) where x  */
/* in first or third quarter of unit circle.Routine receive also            */
/* (right argument) the original  value of x for computing error of result.*/
/* And if result not  accurate enough routine calls  other routines         */
/***************************************************************************/

static double bsloww1(double x, double dx, double orig,int n) {
mynumber u;
 double sn,ssn,cs,ccs,s,c,w[2],y,y1,y2,c1,c2,xx,cor,res;
 static const double t22 = 6291456.0;
 int4 k;
 y=ABS(x);
 u.x=big.x+y;
 y=y-(u.x-big.x);
 dx=(x>0)?dx:-dx;
 xx=y*y;
 s = y*xx*(sn3 +xx*sn5);
 c = xx*(cs2 +xx*(cs4 + xx*cs6));
 k=u.i[LOW_HALF]<<2;
 sn=sincos.x[k];
 ssn=sincos.x[k+1];
 cs=sincos.x[k+2];
 ccs=sincos.x[k+3];
 y1 = (y+t22)-t22;
 y2 = (y - y1)+dx;
 c1 = (cs+t22)-t22;
 c2=(cs-c1)+ccs;
 cor=(ssn+s*ccs+cs*s+c2*y+c1*y2-sn*y*dx)-sn*c;
 y=sn+c1*y1;
 cor = cor+((sn-y)+c1*y1);
 res=y+cor;
 cor=(y-res)+cor;
 cor = (cor>0)? 1.0005*cor+1.1e-24 : 1.0005*cor-1.1e-24;
 if (res == res + cor) return (x>0)?res:-res;
 else {
   __dubsin(ABS(x),dx,w);
   cor = (w[1]>0)? 1.000000005*w[1]+1.1e-24: 1.000000005*w[1]-1.1e-24;
   if (w[0] == w[0]+cor) return (x>0)?w[0]:-w[0];
   else  return (n&1)?__mpcos1(orig):__mpsin1(orig);
 }
}

/***************************************************************************/
/*  Routine compute sin(x+dx)  or cos(x+dx) (Double-Length number) where x  */
/* in second or fourth quarter of unit circle.Routine receive also  the     */
/* original value and quarter(n= 1or 3)of x for computing error of result.  */
/* And if result not accurate enough routine calls  other routines          */
/***************************************************************************/

static double bsloww2(double x, double dx, double orig, int n) {
mynumber u;
 double sn,ssn,cs,ccs,s,c,w[2],y,y1,y2,e1,e2,xx,cor,res;
 static const double t22 = 6291456.0;
 int4 k;
 y=ABS(x);
 u.x=big.x+y;
 y=y-(u.x-big.x);
 dx=(x>0)?dx:-dx;
 xx=y*y;
 s = y*xx*(sn3 +xx*sn5);
 c = y*dx+xx*(cs2 +xx*(cs4 + xx*cs6));
 k=u.i[LOW_HALF]<<2;
 sn=sincos.x[k];
 ssn=sincos.x[k+1];
 cs=sincos.x[k+2];
 ccs=sincos.x[k+3];

 y1 = (y+t22)-t22;
 y2 = (y - y1)+dx;
 e1 = (sn+t22)-t22;
 e2=(sn-e1)+ssn;
 cor=(ccs-cs*c-e1*y2-e2*y)-sn*s;
 y=cs-e1*y1;
 cor = cor+((cs-y)-e1*y1);
 res=y+cor;
 cor=(y-res)+cor;
 cor = (cor>0)? 1.0005*cor+1.1e-24 : 1.0005*cor-1.1e-24;
 if (res == res + cor) return (n&2)?-res:res;
 else {
   __docos(ABS(x),dx,w);
   cor = (w[1]>0)? 1.000000005*w[1]+1.1e-24 : 1.000000005*w[1]-1.1e-24;
   if (w[0] == w[0]+cor) return (n&2)?-w[0]:w[0];
   else  return (n&1)?__mpsin1(orig):__mpcos1(orig);
 }
}

/************************************************************************/
/*  Routine compute cos(x) for  2^-27 < |x|< 0.25 by  Taylor with more   */
/* precision  and if still doesn't accurate enough by mpcos   or docos  */
/************************************************************************/

static double cslow2(double x) {
  mynumber u;
  double sn,ssn,cs,ccs,s,c,w[2],y,y1,y2,e1,e2,xx,cor,res;
  static const double t22 = 6291456.0;
  int4 k;
  y=ABS(x);
  u.x = big.x+y;
  y = y-(u.x-big.x);
  xx=y*y;
  s = y*xx*(sn3 +xx*sn5);
  c = xx*(cs2 +xx*(cs4 + xx*cs6));
  k=u.i[LOW_HALF]<<2;
  sn=sincos.x[k];
  ssn=sincos.x[k+1];
  cs=sincos.x[k+2];
  ccs=sincos.x[k+3];
  y1 = (y+t22)-t22;
  y2 = y - y1;
  e1 = (sn+t22)-t22;
  e2=(sn-e1)+ssn;
  cor=(ccs-cs*c-e1*y2-e2*y)-sn*s;
  y=cs-e1*y1;
  cor = cor+((cs-y)-e1*y1);
  res=y+cor;
  cor=(y-res)+cor;
  if (res == res+1.0005*cor)
    return res;
  else {
    y=ABS(x);
    __docos(y,0,w);
    if (w[0] == w[0]+1.000000005*w[1]) return w[0];
    else return __mpcos(x,0);
  }
}

/***************************************************************************/
/*  Routine compute cos(x+dx) (Double-Length number) where x is small enough*/
/* to use Taylor series around zero and   (x+dx) .Routine receive also      */
/* (right argument) the  original   value of x for computing error of      */
/* result.And if result not accurate enough routine calls other routines    */
/***************************************************************************/


static double csloww(double x,double dx, double orig) {
  static const double th2_36 = 206158430208.0;   /*    1.5*2**37   */
  double y,x1,x2,xx,r,t,res,cor,w[2],a,da,xn;
  union {int4 i[2]; double x;} v;
  int4 n;
  x1=(x+th2_36)-th2_36;
  y = aa.x*x1*x1*x1;
  r=x+y;
  x2=(x-x1)+dx;
  xx=x*x;
    /* Taylor series */
  t = (((((s5.x*xx + s4.x)*xx + s3.x)*xx + s2.x)*xx + bb.x)*xx + 3.0*aa.x*x1*x2)*x +aa.x*x2*x2*x2+dx;
  t=((x-r)+y)+t;
  res=r+t;
  cor = (r-res)+t;
  cor = (cor>0)? 1.0005*cor+ABS(orig)*3.1e-30 : 1.0005*cor-ABS(orig)*3.1e-30;
  if (res == res + cor) return res;
  else {
    (x>0)? __dubsin(x,dx,w) : __dubsin(-x,-dx,w);
    cor = (w[1]>0)? 1.000000001*w[1] + ABS(orig)*1.1e-30 : 1.000000001*w[1] - ABS(orig)*1.1e-30;
    if (w[0] == w[0]+cor) return (x>0)?w[0]:-w[0];
    else {
      t = (orig*hpinv.x + toint.x);
      xn = t - toint.x;
      v.x = t;
      y = (orig - xn*mp1.x) - xn*mp2.x;
      n =v.i[LOW_HALF]&3;
      da = xn*pp3.x;
      t=y-da;
      da = (y-t)-da;
      y = xn*pp4.x;
      a = t - y;
      da = ((t-a)-y)+da;
      if (n==1) {a=-a; da=-da;}
      (a>0)? __dubsin(a,da,w) : __dubsin(-a,-da,w);
      cor = (w[1]>0)? 1.000000001*w[1] + ABS(orig)*1.1e-40 : 1.000000001*w[1] - ABS(orig)*1.1e-40;
      if (w[0] == w[0]+cor) return (a>0)?w[0]:-w[0];
      else return __mpcos1(orig);
    }
  }
}

/***************************************************************************/
/*  Routine compute sin(x+dx)   (Double-Length number) where x in first or  */
/*  third quarter of unit circle.Routine receive also (right argument) the  */
/*  original   value of x for computing error of result.And if result not  */
/* accurate enough routine calls  other routines                            */
/***************************************************************************/

static double csloww1(double x, double dx, double orig) {
  mynumber u;
  double sn,ssn,cs,ccs,s,c,w[2],y,y1,y2,c1,c2,xx,cor,res;
  static const double t22 = 6291456.0;
  int4 k;
  y=ABS(x);
  u.x=big.x+y;
  y=y-(u.x-big.x);
  dx=(x>0)?dx:-dx;
  xx=y*y;
  s = y*xx*(sn3 +xx*sn5);
  c = xx*(cs2 +xx*(cs4 + xx*cs6));
  k=u.i[LOW_HALF]<<2;
  sn=sincos.x[k];
  ssn=sincos.x[k+1];
  cs=sincos.x[k+2];
  ccs=sincos.x[k+3];
  y1 = (y+t22)-t22;
  y2 = (y - y1)+dx;
  c1 = (cs+t22)-t22;
  c2=(cs-c1)+ccs;
  cor=(ssn+s*ccs+cs*s+c2*y+c1*y2-sn*y*dx)-sn*c;
  y=sn+c1*y1;
  cor = cor+((sn-y)+c1*y1);
  res=y+cor;
  cor=(y-res)+cor;
  cor = (cor>0)? 1.0005*cor+3.1e-30*ABS(orig) : 1.0005*cor-3.1e-30*ABS(orig);
  if (res == res + cor) return (x>0)?res:-res;
  else {
    __dubsin(ABS(x),dx,w);
    cor = (w[1]>0)? 1.000000005*w[1]+1.1e-30*ABS(orig) : 1.000000005*w[1]-1.1e-30*ABS(orig);
    if (w[0] == w[0]+cor) return (x>0)?w[0]:-w[0];
    else  return __mpcos1(orig);
  }
}


/***************************************************************************/
/*  Routine compute sin(x+dx)   (Double-Length number) where x in second or */
/*  fourth quarter of unit circle.Routine receive also  the  original value */
/* and quarter(n= 1or 3)of x for computing error of result.And if result not*/
/* accurate enough routine calls  other routines                            */
/***************************************************************************/

static double csloww2(double x, double dx, double orig, int n) {
  mynumber u;
  double sn,ssn,cs,ccs,s,c,w[2],y,y1,y2,e1,e2,xx,cor,res;
  static const double t22 = 6291456.0;
  int4 k;
  y=ABS(x);
  u.x=big.x+y;
  y=y-(u.x-big.x);
  dx=(x>0)?dx:-dx;
  xx=y*y;
  s = y*xx*(sn3 +xx*sn5);
  c = y*dx+xx*(cs2 +xx*(cs4 + xx*cs6));
  k=u.i[LOW_HALF]<<2;
  sn=sincos.x[k];
  ssn=sincos.x[k+1];
  cs=sincos.x[k+2];
  ccs=sincos.x[k+3];

  y1 = (y+t22)-t22;
  y2 = (y - y1)+dx;
  e1 = (sn+t22)-t22;
  e2=(sn-e1)+ssn;
  cor=(ccs-cs*c-e1*y2-e2*y)-sn*s;
  y=cs-e1*y1;
  cor = cor+((cs-y)-e1*y1);
  res=y+cor;
  cor=(y-res)+cor;
  cor = (cor>0)? 1.0005*cor+3.1e-30*ABS(orig) : 1.0005*cor-3.1e-30*ABS(orig);
  if (res == res + cor) return (n)?-res:res;
  else {
    __docos(ABS(x),dx,w);
    cor = (w[1]>0)? 1.000000005*w[1]+1.1e-30*ABS(orig) : 1.000000005*w[1]-1.1e-30*ABS(orig);
    if (w[0] == w[0]+cor) return (n)?-w[0]:w[0];
    else  return __mpcos1(orig);
  }
}

weak_alias (__cos, cos)
weak_alias (__sin, sin)

#ifdef NO_LONG_DOUBLE
strong_alias (__sin, __sinl)
weak_alias (__sin, sinl)
strong_alias (__cos, __cosl)
weak_alias (__cos, cosl)
#endif
