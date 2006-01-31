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
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
/************************************************************************/
/*  MODULE_NAME: atnat.c                                                */
/*                                                                      */
/*  FUNCTIONS:  uatan                                                   */
/*              atanMp                                                  */
/*              signArctan                                              */
/*                                                                      */
/*                                                                      */
/*  FILES NEEDED: dla.h endian.h mpa.h mydefs.h atnat.h                 */
/*                mpatan.c mpatan2.c mpsqrt.c                           */
/*                uatan.tbl                                             */
/*                                                                      */
/* An ultimate atan() routine. Given an IEEE double machine number x    */
/* it computes the correctly rounded (to nearest) value of atan(x).     */
/*                                                                      */
/* Assumption: Machine arithmetic operations are performed in           */
/* round to nearest mode of IEEE 754 standard.                          */
/*                                                                      */
/************************************************************************/

#include "dla.h"
#include "mpa.h"
#include "MathLib.h"
#include "uatan.tbl"
#include "atnat.h"
#include "math.h"

void __mpatan(mp_no *,mp_no *,int);          /* see definition in mpatan.c */
static double atanMp(double,const int[]);
double __signArctan(double,double);
/* An ultimate atan() routine. Given an IEEE double machine number x,    */
/* routine computes the correctly rounded (to nearest) value of atan(x). */
double atan(double x) {


  double cor,s1,ss1,s2,ss2,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,u,u2,u3,
         v,vv,w,ww,y,yy,z,zz;
#if 0
  double y1,y2;
#endif
  int i,ux,dx;
#if 0
  int p;
#endif
  static const int pr[M]={6,8,10,32};
  number num;
#if 0
  mp_no mpt1,mpx,mpy,mpy1,mpy2,mperr;
#endif

  num.d = x;  ux = num.i[HIGH_HALF];  dx = num.i[LOW_HALF];

  /* x=NaN */
  if (((ux&0x7ff00000)==0x7ff00000) && (((ux&0x000fffff)|dx)!=0x00000000))
    return x+x;

  /* Regular values of x, including denormals +-0 and +-INF */
  u = (x<ZERO) ? -x : x;
  if (u<C) {
    if (u<B) {
      if (u<A) {                                           /* u < A */
         return x; }
      else {                                               /* A <= u < B */
        v=x*x;  yy=x*v*(d3.d+v*(d5.d+v*(d7.d+v*(d9.d+v*(d11.d+v*d13.d)))));
        if ((y=x+(yy-U1*x)) == x+(yy+U1*x))  return y;

        EMULV(x,x,v,vv,t1,t2,t3,t4,t5)                       /* v+vv=x^2 */
        s1=v*(f11.d+v*(f13.d+v*(f15.d+v*(f17.d+v*f19.d))));
        ADD2(f9.d,ff9.d,s1,ZERO,s2,ss2,t1,t2)
        MUL2(v,vv,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
        ADD2(f7.d,ff7.d,s1,ss1,s2,ss2,t1,t2)
        MUL2(v,vv,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
        ADD2(f5.d,ff5.d,s1,ss1,s2,ss2,t1,t2)
        MUL2(v,vv,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
        ADD2(f3.d,ff3.d,s1,ss1,s2,ss2,t1,t2)
        MUL2(v,vv,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
        MUL2(x,ZERO,s1,ss1,s2,ss2,t1,t2,t3,t4,t5,t6,t7,t8)
        ADD2(x,ZERO,s2,ss2,s1,ss1,t1,t2)
        if ((y=s1+(ss1-U5*s1)) == s1+(ss1+U5*s1))  return y;

        return atanMp(x,pr);
      } }
    else {  /* B <= u < C */
      i=(TWO52+TWO8*u)-TWO52;  i-=16;
      z=u-cij[i][0].d;
      yy=z*(cij[i][2].d+z*(cij[i][3].d+z*(cij[i][4].d+
                        z*(cij[i][5].d+z* cij[i][6].d))));
      t1=cij[i][1].d;
      if (i<112) {
        if (i<48)  u2=U21;    /* u < 1/4        */
        else       u2=U22; }  /* 1/4 <= u < 1/2 */
      else {
        if (i<176) u2=U23;    /* 1/2 <= u < 3/4 */
        else       u2=U24; }  /* 3/4 <= u <= 1  */
      if ((y=t1+(yy-u2*t1)) == t1+(yy+u2*t1))  return __signArctan(x,y);

      z=u-hij[i][0].d;
      s1=z*(hij[i][11].d+z*(hij[i][12].d+z*(hij[i][13].d+
         z*(hij[i][14].d+z* hij[i][15].d))));
      ADD2(hij[i][9].d,hij[i][10].d,s1,ZERO,s2,ss2,t1,t2)
      MUL2(z,ZERO,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
      ADD2(hij[i][7].d,hij[i][8].d,s1,ss1,s2,ss2,t1,t2)
      MUL2(z,ZERO,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
      ADD2(hij[i][5].d,hij[i][6].d,s1,ss1,s2,ss2,t1,t2)
      MUL2(z,ZERO,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
      ADD2(hij[i][3].d,hij[i][4].d,s1,ss1,s2,ss2,t1,t2)
      MUL2(z,ZERO,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
      ADD2(hij[i][1].d,hij[i][2].d,s1,ss1,s2,ss2,t1,t2)
      if ((y=s2+(ss2-U6*s2)) == s2+(ss2+U6*s2))  return __signArctan(x,y);

      return atanMp(x,pr);
    }
  }
  else {
    if (u<D) { /* C <= u < D */
      w=ONE/u;
      EMULV(w,u,t1,t2,t3,t4,t5,t6,t7)
      ww=w*((ONE-t1)-t2);
      i=(TWO52+TWO8*w)-TWO52;  i-=16;
      z=(w-cij[i][0].d)+ww;
      yy=HPI1-z*(cij[i][2].d+z*(cij[i][3].d+z*(cij[i][4].d+
                             z*(cij[i][5].d+z* cij[i][6].d))));
      t1=HPI-cij[i][1].d;
      if (i<112)  u3=U31;  /* w <  1/2 */
      else        u3=U32;  /* w >= 1/2 */
      if ((y=t1+(yy-u3)) == t1+(yy+u3))  return __signArctan(x,y);

      DIV2(ONE,ZERO,u,ZERO,w,ww,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
      t1=w-hij[i][0].d;
      EADD(t1,ww,z,zz)
      s1=z*(hij[i][11].d+z*(hij[i][12].d+z*(hij[i][13].d+
         z*(hij[i][14].d+z* hij[i][15].d))));
      ADD2(hij[i][9].d,hij[i][10].d,s1,ZERO,s2,ss2,t1,t2)
      MUL2(z,zz,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
      ADD2(hij[i][7].d,hij[i][8].d,s1,ss1,s2,ss2,t1,t2)
      MUL2(z,zz,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
      ADD2(hij[i][5].d,hij[i][6].d,s1,ss1,s2,ss2,t1,t2)
      MUL2(z,zz,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
      ADD2(hij[i][3].d,hij[i][4].d,s1,ss1,s2,ss2,t1,t2)
      MUL2(z,zz,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
      ADD2(hij[i][1].d,hij[i][2].d,s1,ss1,s2,ss2,t1,t2)
      SUB2(HPI,HPI1,s2,ss2,s1,ss1,t1,t2)
      if ((y=s1+(ss1-U7)) == s1+(ss1+U7))  return __signArctan(x,y);

    return atanMp(x,pr);
    }
    else {
      if (u<E) { /* D <= u < E */
        w=ONE/u;   v=w*w;
        EMULV(w,u,t1,t2,t3,t4,t5,t6,t7)
        yy=w*v*(d3.d+v*(d5.d+v*(d7.d+v*(d9.d+v*(d11.d+v*d13.d)))));
        ww=w*((ONE-t1)-t2);
        ESUB(HPI,w,t3,cor)
        yy=((HPI1+cor)-ww)-yy;
        if ((y=t3+(yy-U4)) == t3+(yy+U4))  return __signArctan(x,y);

        DIV2(ONE,ZERO,u,ZERO,w,ww,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
        MUL2(w,ww,w,ww,v,vv,t1,t2,t3,t4,t5,t6,t7,t8)
        s1=v*(f11.d+v*(f13.d+v*(f15.d+v*(f17.d+v*f19.d))));
        ADD2(f9.d,ff9.d,s1,ZERO,s2,ss2,t1,t2)
        MUL2(v,vv,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
        ADD2(f7.d,ff7.d,s1,ss1,s2,ss2,t1,t2)
        MUL2(v,vv,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
        ADD2(f5.d,ff5.d,s1,ss1,s2,ss2,t1,t2)
        MUL2(v,vv,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
        ADD2(f3.d,ff3.d,s1,ss1,s2,ss2,t1,t2)
        MUL2(v,vv,s2,ss2,s1,ss1,t1,t2,t3,t4,t5,t6,t7,t8)
        MUL2(w,ww,s1,ss1,s2,ss2,t1,t2,t3,t4,t5,t6,t7,t8)
        ADD2(w,ww,s2,ss2,s1,ss1,t1,t2)
        SUB2(HPI,HPI1,s1,ss1,s2,ss2,t1,t2)
        if ((y=s2+(ss2-U8)) == s2+(ss2+U8))  return __signArctan(x,y);

      return atanMp(x,pr);
      }
      else {
        /* u >= E */
        if (x>0) return  HPI;
        else     return MHPI; }
    }
  }

}


  /* Fix the sign of y and return */
double  __signArctan(double x,double y){

    if (x<ZERO) return -y;
    else        return  y;
}

 /* Final stages. Compute atan(x) by multiple precision arithmetic */
static double atanMp(double x,const int pr[]){
  mp_no mpx,mpy,mpy2,mperr,mpt1,mpy1;
  double y1,y2;
  int i,p;

for (i=0; i<M; i++) {
    p = pr[i];
    __dbl_mp(x,&mpx,p);          __mpatan(&mpx,&mpy,p);
    __dbl_mp(u9[i].d,&mpt1,p);   __mul(&mpy,&mpt1,&mperr,p);
    __add(&mpy,&mperr,&mpy1,p);  __sub(&mpy,&mperr,&mpy2,p);
    __mp_dbl(&mpy1,&y1,p);       __mp_dbl(&mpy2,&y2,p);
    if (y1==y2)   return y1;
  }
  return y1; /*if unpossible to do exact computing */
}

#ifdef NO_LONG_DOUBLE
weak_alias (atan, atanl)
#endif
