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
/*********************************************************************/
/*                                                                   */
/*      MODULE_NAME:ulog.c                                           */
/*                                                                   */
/*      FUNCTION:ulog                                                */
/*                                                                   */
/*      FILES NEEDED: dla.h endian.h mpa.h mydefs.h ulog.h           */
/*                    mpexp.c mplog.c mpa.c                          */
/*                    ulog.tbl                                       */
/*                                                                   */
/* An ultimate log routine. Given an IEEE double machine number x    */
/* it computes the correctly rounded (to nearest) value of log(x).   */
/* Assumption: Machine arithmetic operations are performed in        */
/* round to nearest mode of IEEE 754 standard.                       */
/*                                                                   */
/*********************************************************************/


#include "endian.h"
#include "dla.h"
#include "mpa.h"
#include "MathLib.h"
#include "math_private.h"

void __mplog(mp_no *, mp_no *, int);

/*********************************************************************/
/* An ultimate log routine. Given an IEEE double machine number x     */
/* it computes the correctly rounded (to nearest) value of log(x).   */
/*********************************************************************/
double __ieee754_log(double x) {
#define M 4
  static const int pr[M]={8,10,18,32};
  int i,j,n,ux,dx,p;
#if 0
  int k;
#endif
  double dbl_n,u,p0,q,r0,w,nln2a,luai,lubi,lvaj,lvbj,
         sij,ssij,ttij,A,B,B0,y,y1,y2,polI,polII,sa,sb,
         t1,t2,t3,t4,t5,t6,t7,t8,t,ra,rb,ww,
         a0,aa0,s1,s2,ss2,s3,ss3,a1,aa1,a,aa,b,bb,c;
  number num;
  mp_no mpx,mpy,mpy1,mpy2,mperr;

#include "ulog.tbl"
#include "ulog.h"

  /* Treating special values of x ( x<=0, x=INF, x=NaN etc.). */

  num.d = x;  ux = num.i[HIGH_HALF];  dx = num.i[LOW_HALF];
  n=0;
  if (ux < 0x00100000) {
    if (((ux & 0x7fffffff) | dx) == 0)  return MHALF/ZERO; /* return -INF */
    if (ux < 0) return (x-x)/ZERO;                         /* return NaN  */
    n -= 54;    x *= two54.d;                              /* scale x     */
    num.d = x;
  }
  if (ux >= 0x7ff00000) return x+x;                        /* INF or NaN  */

  /* Regular values of x */

  w = x-ONE;
  if (ABS(w) > U03) { goto case_03; }


  /*--- Stage I, the case abs(x-1) < 0.03 */

  t8 = MHALF*w;
  EMULV(t8,w,a,aa,t1,t2,t3,t4,t5)
  EADD(w,a,b,bb)

  /* Evaluate polynomial II */
  polII = (b0.d+w*(b1.d+w*(b2.d+w*(b3.d+w*(b4.d+
          w*(b5.d+w*(b6.d+w*(b7.d+w*b8.d))))))))*w*w*w;
  c = (aa+bb)+polII;

  /* End stage I, case abs(x-1) < 0.03 */
  if ((y=b+(c+b*E2)) == b+(c-b*E2))  return y;

  /*--- Stage II, the case abs(x-1) < 0.03 */

  a = d11.d+w*(d12.d+w*(d13.d+w*(d14.d+w*(d15.d+w*(d16.d+
            w*(d17.d+w*(d18.d+w*(d19.d+w*d20.d))))))));
  EMULV(w,a,s2,ss2,t1,t2,t3,t4,t5)
  ADD2(d10.d,dd10.d,s2,ss2,s3,ss3,t1,t2)
  MUL2(w,ZERO,s3,ss3,s2,ss2,t1,t2,t3,t4,t5,t6,t7,t8)
  ADD2(d9.d,dd9.d,s2,ss2,s3,ss3,t1,t2)
  MUL2(w,ZERO,s3,ss3,s2,ss2,t1,t2,t3,t4,t5,t6,t7,t8)
  ADD2(d8.d,dd8.d,s2,ss2,s3,ss3,t1,t2)
  MUL2(w,ZERO,s3,ss3,s2,ss2,t1,t2,t3,t4,t5,t6,t7,t8)
  ADD2(d7.d,dd7.d,s2,ss2,s3,ss3,t1,t2)
  MUL2(w,ZERO,s3,ss3,s2,ss2,t1,t2,t3,t4,t5,t6,t7,t8)
  ADD2(d6.d,dd6.d,s2,ss2,s3,ss3,t1,t2)
  MUL2(w,ZERO,s3,ss3,s2,ss2,t1,t2,t3,t4,t5,t6,t7,t8)
  ADD2(d5.d,dd5.d,s2,ss2,s3,ss3,t1,t2)
  MUL2(w,ZERO,s3,ss3,s2,ss2,t1,t2,t3,t4,t5,t6,t7,t8)
  ADD2(d4.d,dd4.d,s2,ss2,s3,ss3,t1,t2)
  MUL2(w,ZERO,s3,ss3,s2,ss2,t1,t2,t3,t4,t5,t6,t7,t8)
  ADD2(d3.d,dd3.d,s2,ss2,s3,ss3,t1,t2)
  MUL2(w,ZERO,s3,ss3,s2,ss2,t1,t2,t3,t4,t5,t6,t7,t8)
  ADD2(d2.d,dd2.d,s2,ss2,s3,ss3,t1,t2)
  MUL2(w,ZERO,s3,ss3,s2,ss2,t1,t2,t3,t4,t5,t6,t7,t8)
  MUL2(w,ZERO,s2,ss2,s3,ss3,t1,t2,t3,t4,t5,t6,t7,t8)
  ADD2(w,ZERO,    s3,ss3, b, bb,t1,t2)

  /* End stage II, case abs(x-1) < 0.03 */
  if ((y=b+(bb+b*E4)) == b+(bb-b*E4))  return y;
  goto stage_n;

  /*--- Stage I, the case abs(x-1) > 0.03 */
  case_03:

  /* Find n,u such that x = u*2**n,   1/sqrt(2) < u < sqrt(2)  */
  n += (num.i[HIGH_HALF] >> 20) - 1023;
  num.i[HIGH_HALF] = (num.i[HIGH_HALF] & 0x000fffff) | 0x3ff00000;
  if (num.d > SQRT_2) { num.d *= HALF;  n++; }
  u = num.d;  dbl_n = (double) n;

  /* Find i such that ui=1+(i-75)/2**8 is closest to u (i= 0,1,2,...,181) */
  num.d += h1.d;
  i = (num.i[HIGH_HALF] & 0x000fffff) >> 12;

  /* Find j such that vj=1+(j-180)/2**16 is closest to v=u/ui (j= 0,...,361) */
  num.d = u*Iu[i].d + h2.d;
  j = (num.i[HIGH_HALF] & 0x000fffff) >> 4;

  /* Compute w=(u-ui*vj)/(ui*vj) */
  p0=(ONE+(i-75)*DEL_U)*(ONE+(j-180)*DEL_V);
  q=u-p0;   r0=Iu[i].d*Iv[j].d;   w=q*r0;

  /* Evaluate polynomial I */
  polI = w+(a2.d+a3.d*w)*w*w;

  /* Add up everything */
  nln2a = dbl_n*LN2A;
  luai  = Lu[i][0].d;   lubi  = Lu[i][1].d;
  lvaj  = Lv[j][0].d;   lvbj  = Lv[j][1].d;
  EADD(luai,lvaj,sij,ssij)
  EADD(nln2a,sij,A  ,ttij)
  B0 = (((lubi+lvbj)+ssij)+ttij)+dbl_n*LN2B;
  B  = polI+B0;

  /* End stage I, case abs(x-1) >= 0.03 */
  if ((y=A+(B+E1)) == A+(B-E1))  return y;


  /*--- Stage II, the case abs(x-1) > 0.03 */

  /* Improve the accuracy of r0 */
  EMULV(p0,r0,sa,sb,t1,t2,t3,t4,t5)
  t=r0*((ONE-sa)-sb);
  EADD(r0,t,ra,rb)

  /* Compute w */
  MUL2(q,ZERO,ra,rb,w,ww,t1,t2,t3,t4,t5,t6,t7,t8)

  EADD(A,B0,a0,aa0)

  /* Evaluate polynomial III */
  s1 = (c3.d+(c4.d+c5.d*w)*w)*w;
  EADD(c2.d,s1,s2,ss2)
  MUL2(s2,ss2,w,ww,s3,ss3,t1,t2,t3,t4,t5,t6,t7,t8)
  MUL2(s3,ss3,w,ww,s2,ss2,t1,t2,t3,t4,t5,t6,t7,t8)
  ADD2(s2,ss2,w,ww,s3,ss3,t1,t2)
  ADD2(s3,ss3,a0,aa0,a1,aa1,t1,t2)

  /* End stage II, case abs(x-1) >= 0.03 */
  if ((y=a1+(aa1+E3)) == a1+(aa1-E3)) return y;


  /* Final stages. Use multi-precision arithmetic. */
  stage_n:

  for (i=0; i<M; i++) {
    p = pr[i];
    __dbl_mp(x,&mpx,p);  __dbl_mp(y,&mpy,p);
    __mplog(&mpx,&mpy,p);
    __dbl_mp(e[i].d,&mperr,p);
    __add(&mpy,&mperr,&mpy1,p);  __sub(&mpy,&mperr,&mpy2,p);
    __mp_dbl(&mpy1,&y1,p);       __mp_dbl(&mpy2,&y2,p);
    if (y1==y2)   return y1;
  }
  return y1;
}
