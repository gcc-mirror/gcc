/*
 * IBM Accurate Mathematical Library
 * written by International Business Machines Corp.
 * Copyright (C) 2001, 2002, 2004 Free Software Foundation
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
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
/***************************************************************************/
/*  MODULE_NAME: upow.c                                                    */
/*                                                                         */
/*  FUNCTIONS: upow                                                        */
/*             power1                                                      */
/*             my_log2                                                        */
/*             log1                                                        */
/*             checkint                                                    */
/* FILES NEEDED: dla.h endian.h mpa.h mydefs.h                             */
/*               halfulp.c mpexp.c mplog.c slowexp.c slowpow.c mpa.c       */
/*                          uexp.c  upow.c			           */
/*               root.tbl uexp.tbl upow.tbl                                */
/* An ultimate power routine. Given two IEEE double machine numbers y,x    */
/* it computes the correctly rounded (to nearest) value of x^y.            */
/* Assumption: Machine arithmetic operations are performed in              */
/* round to nearest mode of IEEE 754 standard.                             */
/*                                                                         */
/***************************************************************************/
#include "endian.h"
#include "upow.h"
#include "dla.h"
#include "mydefs.h"
#include "MathLib.h"
#include "upow.tbl"
#include "math_private.h"


double __exp1(double x, double xx, double error);
static double log1(double x, double *delta, double *error);
static double my_log2(double x, double *delta, double *error);
double __slowpow(double x, double y,double z);
static double power1(double x, double y);
static int checkint(double x);

/***************************************************************************/
/* An ultimate power routine. Given two IEEE double machine numbers y,x    */
/* it computes the correctly rounded (to nearest) value of X^y.            */
/***************************************************************************/
double __ieee754_pow(double x, double y) {
  double z,a,aa,error, t,a1,a2,y1,y2;
#if 0
  double gor=1.0;
#endif
  mynumber u,v;
  int k;
  int4 qx,qy;
  v.x=y;
  u.x=x;
  if (v.i[LOW_HALF] == 0) { /* of y */
    qx = u.i[HIGH_HALF]&0x7fffffff;
    /* Checking  if x is not too small to compute */
    if (((qx==0x7ff00000)&&(u.i[LOW_HALF]!=0))||(qx>0x7ff00000)) return NaNQ.x;
    if (y == 1.0) return x;
    if (y == 2.0) return x*x;
    if (y == -1.0) return 1.0/x;
    if (y == 0) return 1.0;
  }
  /* else */
  if(((u.i[HIGH_HALF]>0 && u.i[HIGH_HALF]<0x7ff00000)||        /* x>0 and not x->0 */
       (u.i[HIGH_HALF]==0 && u.i[LOW_HALF]!=0))  &&
                                      /*   2^-1023< x<= 2^-1023 * 0x1.0000ffffffff */
      (v.i[HIGH_HALF]&0x7fffffff) < 0x4ff00000) {              /* if y<-1 or y>1   */
    z = log1(x,&aa,&error);                                 /* x^y  =e^(y log (X)) */
    t = y*134217729.0;
    y1 = t - (t-y);
    y2 = y - y1;
    t = z*134217729.0;
    a1 = t - (t-z);
    a2 = (z - a1)+aa;
    a = y1*a1;
    aa = y2*a1 + y*a2;
    a1 = a+aa;
    a2 = (a-a1)+aa;
    error = error*ABS(y);
    t = __exp1(a1,a2,1.9e16*error);     /* return -10 or 0 if wasn't computed exactly */
    return (t>0)?t:power1(x,y);
  }

  if (x == 0) {
    if (((v.i[HIGH_HALF] & 0x7fffffff) == 0x7ff00000 && v.i[LOW_HALF] != 0)
	|| (v.i[HIGH_HALF] & 0x7fffffff) > 0x7ff00000)
      return y;
    if (ABS(y) > 1.0e20) return (y>0)?0:INF.x;
    k = checkint(y);
    if (k == -1)
      return y < 0 ? 1.0/x : x;
    else
      return y < 0 ? 1.0/ABS(x) : 0.0;                               /* return 0 */
  }
  /* if x<0 */
  if (u.i[HIGH_HALF] < 0) {
    k = checkint(y);
    if (k==0) {
      if ((v.i[HIGH_HALF] & 0x7fffffff) == 0x7ff00000 && v.i[LOW_HALF] == 0) {
	if (x == -1.0) return 1.0;
	else if (x > -1.0) return v.i[HIGH_HALF] < 0 ? INF.x : 0.0;
	else return v.i[HIGH_HALF] < 0 ? 0.0 : INF.x;
      }
      else if (u.i[HIGH_HALF] == 0xfff00000 && u.i[LOW_HALF] == 0)
	return y < 0 ? 0.0 : INF.x;
      return NaNQ.x;                              /* y not integer and x<0 */
    }
    else if (u.i[HIGH_HALF] == 0xfff00000 && u.i[LOW_HALF] == 0)
      {
	if (k < 0)
	  return y < 0 ? nZERO.x : nINF.x;
	else
	  return y < 0 ? 0.0 : INF.x;
      }
    return (k==1)?__ieee754_pow(-x,y):-__ieee754_pow(-x,y); /* if y even or odd */
  }
  /* x>0 */
  qx = u.i[HIGH_HALF]&0x7fffffff;  /*   no sign   */
  qy = v.i[HIGH_HALF]&0x7fffffff;  /*   no sign   */

  if (qx > 0x7ff00000 || (qx == 0x7ff00000 && u.i[LOW_HALF] != 0)) return NaNQ.x;
                                                                 /*  if 0<x<2^-0x7fe */
  if (qy > 0x7ff00000 || (qy == 0x7ff00000 && v.i[LOW_HALF] != 0))
    return x == 1.0 ? 1.0 : NaNQ.x;
                                                                 /*  if y<2^-0x7fe   */

  if (qx == 0x7ff00000)                              /* x= 2^-0x3ff */
    {if (y == 0) return NaNQ.x;
    return (y>0)?x:0; }

  if (qy > 0x45f00000 && qy < 0x7ff00000) {
    if (x == 1.0) return 1.0;
    if (y>0) return (x>1.0)?INF.x:0;
    if (y<0) return (x<1.0)?INF.x:0;
  }

  if (x == 1.0) return 1.0;
  if (y>0) return (x>1.0)?INF.x:0;
  if (y<0) return (x<1.0)?INF.x:0;
  return 0;     /* unreachable, to make the compiler happy */
}

/**************************************************************************/
/* Computing x^y using more accurate but more slow log routine            */
/**************************************************************************/
static double power1(double x, double y) {
  double z,a,aa,error, t,a1,a2,y1,y2;
  z = my_log2(x,&aa,&error);
  t = y*134217729.0;
  y1 = t - (t-y);
  y2 = y - y1;
  t = z*134217729.0;
  a1 = t - (t-z);
  a2 = z - a1;
  a = y*z;
  aa = ((y1*a1-a)+y1*a2+y2*a1)+y2*a2+aa*y;
  a1 = a+aa;
  a2 = (a-a1)+aa;
  error = error*ABS(y);
  t = __exp1(a1,a2,1.9e16*error);
  return (t >= 0)?t:__slowpow(x,y,z);
}

/****************************************************************************/
/* Computing log(x) (x is left argument). The result is the returned double */
/* + the parameter delta.                                                   */
/* The result is bounded by error (rightmost argument)                      */
/****************************************************************************/
static double log1(double x, double *delta, double *error) {
  int i,j,m;
#if 0
  int n;
#endif
  double uu,vv,eps,nx,e,e1,e2,t,t1,t2,res,add=0;
#if 0
  double cor;
#endif
  mynumber u,v;
#ifdef BIG_ENDI
  mynumber
/**/ two52          = {{0x43300000, 0x00000000}}; /* 2**52         */
#else
#ifdef LITTLE_ENDI
  mynumber
/**/ two52          = {{0x00000000, 0x43300000}}; /* 2**52         */
#endif
#endif

  u.x = x;
  m = u.i[HIGH_HALF];
  *error = 0;
  *delta = 0;
  if (m < 0x00100000)             /*  1<x<2^-1007 */
    { x = x*t52.x; add = -52.0; u.x = x; m = u.i[HIGH_HALF];}

  if ((m&0x000fffff) < 0x0006a09e)
    {u.i[HIGH_HALF] = (m&0x000fffff)|0x3ff00000; two52.i[LOW_HALF]=(m>>20); }
  else
    {u.i[HIGH_HALF] = (m&0x000fffff)|0x3fe00000; two52.i[LOW_HALF]=(m>>20)+1; }

  v.x = u.x + bigu.x;
  uu = v.x - bigu.x;
  i = (v.i[LOW_HALF]&0x000003ff)<<2;
  if (two52.i[LOW_HALF] == 1023)         /* nx = 0              */
  {
      if (i > 1192 && i < 1208)          /* |x-1| < 1.5*2**-10  */
      {
	  t = x - 1.0;
	  t1 = (t+5.0e6)-5.0e6;
	  t2 = t-t1;
	  e1 = t - 0.5*t1*t1;
	  e2 = t*t*t*(r3+t*(r4+t*(r5+t*(r6+t*(r7+t*r8)))))-0.5*t2*(t+t1);
	  res = e1+e2;
	  *error = 1.0e-21*ABS(t);
	  *delta = (e1-res)+e2;
	  return res;
      }                  /* |x-1| < 1.5*2**-10  */
      else
      {
	  v.x = u.x*(ui.x[i]+ui.x[i+1])+bigv.x;
	  vv = v.x-bigv.x;
	  j = v.i[LOW_HALF]&0x0007ffff;
	  j = j+j+j;
	  eps = u.x - uu*vv;
	  e1 = eps*ui.x[i];
	  e2 = eps*(ui.x[i+1]+vj.x[j]*(ui.x[i]+ui.x[i+1]));
	  e = e1+e2;
	  e2 =  ((e1-e)+e2);
	  t=ui.x[i+2]+vj.x[j+1];
	  t1 = t+e;
	  t2 = (((t-t1)+e)+(ui.x[i+3]+vj.x[j+2]))+e2+e*e*(p2+e*(p3+e*p4));
	  res=t1+t2;
	  *error = 1.0e-24;
	  *delta = (t1-res)+t2;
	  return res;
      }
  }   /* nx = 0 */
  else                            /* nx != 0   */
  {
      eps = u.x - uu;
      nx = (two52.x - two52e.x)+add;
      e1 = eps*ui.x[i];
      e2 = eps*ui.x[i+1];
      e=e1+e2;
      e2 = (e1-e)+e2;
      t=nx*ln2a.x+ui.x[i+2];
      t1=t+e;
      t2=(((t-t1)+e)+nx*ln2b.x+ui.x[i+3]+e2)+e*e*(q2+e*(q3+e*(q4+e*(q5+e*q6))));
      res = t1+t2;
      *error = 1.0e-21;
      *delta = (t1-res)+t2;
      return res;
  }                                /* nx != 0   */
}

/****************************************************************************/
/* More slow but more accurate routine of log                               */
/* Computing log(x)(x is left argument).The result is return double + delta.*/
/* The result is bounded by error (right argument)                           */
/****************************************************************************/
static double my_log2(double x, double *delta, double *error) {
  int i,j,m;
#if 0
  int n;
#endif
  double uu,vv,eps,nx,e,e1,e2,t,t1,t2,res,add=0;
#if 0
  double cor;
#endif
  double ou1,ou2,lu1,lu2,ov,lv1,lv2,a,a1,a2;
  double y,yy,z,zz,j1,j2,j3,j4,j5,j6,j7,j8;
  mynumber u,v;
#ifdef BIG_ENDI
  mynumber
/**/ two52          = {{0x43300000, 0x00000000}}; /* 2**52         */
#else
#ifdef LITTLE_ENDI
  mynumber
/**/ two52          = {{0x00000000, 0x43300000}}; /* 2**52         */
#endif
#endif

  u.x = x;
  m = u.i[HIGH_HALF];
  *error = 0;
  *delta = 0;
  add=0;
  if (m<0x00100000) {  /* x < 2^-1022 */
    x = x*t52.x;  add = -52.0; u.x = x; m = u.i[HIGH_HALF]; }

  if ((m&0x000fffff) < 0x0006a09e)
    {u.i[HIGH_HALF] = (m&0x000fffff)|0x3ff00000; two52.i[LOW_HALF]=(m>>20); }
  else
    {u.i[HIGH_HALF] = (m&0x000fffff)|0x3fe00000; two52.i[LOW_HALF]=(m>>20)+1; }

  v.x = u.x + bigu.x;
  uu = v.x - bigu.x;
  i = (v.i[LOW_HALF]&0x000003ff)<<2;
  /*------------------------------------- |x-1| < 2**-11-------------------------------  */
  if ((two52.i[LOW_HALF] == 1023)  && (i == 1200))
  {
      t = x - 1.0;
      EMULV(t,s3,y,yy,j1,j2,j3,j4,j5);
      ADD2(-0.5,0,y,yy,z,zz,j1,j2);
      MUL2(t,0,z,zz,y,yy,j1,j2,j3,j4,j5,j6,j7,j8);
      MUL2(t,0,y,yy,z,zz,j1,j2,j3,j4,j5,j6,j7,j8);

      e1 = t+z;
      e2 = (((t-e1)+z)+zz)+t*t*t*(ss3+t*(s4+t*(s5+t*(s6+t*(s7+t*s8)))));
      res = e1+e2;
      *error = 1.0e-25*ABS(t);
      *delta = (e1-res)+e2;
      return res;
  }
  /*----------------------------- |x-1| > 2**-11  --------------------------  */
  else
  {          /*Computing log(x) according to log table                        */
      nx = (two52.x - two52e.x)+add;
      ou1 = ui.x[i];
      ou2 = ui.x[i+1];
      lu1 = ui.x[i+2];
      lu2 = ui.x[i+3];
      v.x = u.x*(ou1+ou2)+bigv.x;
      vv = v.x-bigv.x;
      j = v.i[LOW_HALF]&0x0007ffff;
      j = j+j+j;
      eps = u.x - uu*vv;
      ov  = vj.x[j];
      lv1 = vj.x[j+1];
      lv2 = vj.x[j+2];
      a = (ou1+ou2)*(1.0+ov);
      a1 = (a+1.0e10)-1.0e10;
      a2 = a*(1.0-a1*uu*vv);
      e1 = eps*a1;
      e2 = eps*a2;
      e = e1+e2;
      e2 = (e1-e)+e2;
      t=nx*ln2a.x+lu1+lv1;
      t1 = t+e;
      t2 = (((t-t1)+e)+(lu2+lv2+nx*ln2b.x+e2))+e*e*(p2+e*(p3+e*p4));
      res=t1+t2;
      *error = 1.0e-27;
      *delta = (t1-res)+t2;
      return res;
  }
}

/**********************************************************************/
/* Routine receives a double x and checks if it is an integer. If not */
/* it returns 0, else it returns 1 if even or -1 if odd.              */
/**********************************************************************/
static int checkint(double x) {
  union {int4 i[2]; double x;} u;
  int k,m,n;
#if 0
  int l;
#endif
  u.x = x;
  m = u.i[HIGH_HALF]&0x7fffffff;    /* no sign */
  if (m >= 0x7ff00000) return 0;    /*  x is +/-inf or NaN  */
  if (m >= 0x43400000) return 1;    /*  |x| >= 2**53   */
  if (m < 0x40000000) return 0;     /* |x| < 2,  can not be 0 or 1  */
  n = u.i[LOW_HALF];
  k = (m>>20)-1023;                 /*  1 <= k <= 52   */
  if (k == 52) return (n&1)? -1:1;  /* odd or even*/
  if (k>20) {
    if (n<<(k-20)) return 0;        /* if not integer */
    return (n<<(k-21))?-1:1;
  }
  if (n) return 0;                  /*if  not integer*/
  if (k == 20) return (m&1)? -1:1;
  if (m<<(k+12)) return 0;
  return (m<<(k+11))?-1:1;
}
