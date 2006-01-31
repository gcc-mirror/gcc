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
/********************************************************************/
/*                                                                  */
/* MODULE_NAME: dosincos.c                                          */
/*                                                                  */
/*                                                                  */
/* FUNCTIONS:   dubsin                                              */
/*              dubcos                                              */
/*              docos                                               */
/* FILES NEEDED: endian.h mydefs.h dla.h dosincos.h                 */
/*               sincos.tbl                                         */
/*                                                                  */
/* Routines compute sin() and cos() as Double-Length numbers         */
/********************************************************************/



#include "endian.h"
#include "mydefs.h"
#include "sincos.tbl"
#include "dla.h"
#include "dosincos.h"
#include "math_private.h"

/***********************************************************************/
/* Routine receive Double-Length number (x+dx) and computing sin(x+dx) */
/* as Double-Length number and store it at array v .It computes it by  */
/* arithmetic action on Double-Length numbers                          */
/*(x+dx) between 0 and PI/4                                            */
/***********************************************************************/

void __dubsin(double x, double dx, double v[]) {
  double r,s,p,hx,tx,hy,ty,q,c,cc,d,dd,d2,dd2,e,ee,
    sn,ssn,cs,ccs,ds,dss,dc,dcc;
#if 0
  double xx,y,yy,z,zz;
#endif
  mynumber u;
  int4 k;

  u.x=x+big.x;
  k = u.i[LOW_HALF]<<2;
  x=x-(u.x-big.x);
  d=x+dx;
  dd=(x-d)+dx;
         /* sin(x+dx)=sin(Xi+t)=sin(Xi)*cos(t) + cos(Xi)sin(t) where t ->0 */
  MUL2(d,dd,d,dd,d2,dd2,p,hx,tx,hy,ty,q,c,cc);
  sn=sincos.x[k];     /*                                  */
  ssn=sincos.x[k+1];  /*      sin(Xi) and cos(Xi)         */
  cs=sincos.x[k+2];   /*                                  */
  ccs=sincos.x[k+3];  /*                                  */
  MUL2(d2,dd2,s7.x,ss7.x,ds,dss,p,hx,tx,hy,ty,q,c,cc);  /* Taylor    */
  ADD2(ds,dss,s5.x,ss5.x,ds,dss,r,s);
  MUL2(d2,dd2,ds,dss,ds,dss,p,hx,tx,hy,ty,q,c,cc);      /* series    */
  ADD2(ds,dss,s3.x,ss3.x,ds,dss,r,s);
  MUL2(d2,dd2,ds,dss,ds,dss,p,hx,tx,hy,ty,q,c,cc);      /* for sin   */
  MUL2(d,dd,ds,dss,ds,dss,p,hx,tx,hy,ty,q,c,cc);
  ADD2(ds,dss,d,dd,ds,dss,r,s);                         /* ds=sin(t) */

  MUL2(d2,dd2,c8.x,cc8.x,dc,dcc,p,hx,tx,hy,ty,q,c,cc); ;/* Taylor    */
  ADD2(dc,dcc,c6.x,cc6.x,dc,dcc,r,s);
  MUL2(d2,dd2,dc,dcc,dc,dcc,p,hx,tx,hy,ty,q,c,cc);      /* series    */
  ADD2(dc,dcc,c4.x,cc4.x,dc,dcc,r,s);
  MUL2(d2,dd2,dc,dcc,dc,dcc,p,hx,tx,hy,ty,q,c,cc);      /* for cos   */
  ADD2(dc,dcc,c2.x,cc2.x,dc,dcc,r,s);
  MUL2(d2,dd2,dc,dcc,dc,dcc,p,hx,tx,hy,ty,q,c,cc);      /* dc=cos(t) */

  MUL2(cs,ccs,ds,dss,e,ee,p,hx,tx,hy,ty,q,c,cc);
  MUL2(dc,dcc,sn,ssn,dc,dcc,p,hx,tx,hy,ty,q,c,cc);
  SUB2(e,ee,dc,dcc,e,ee,r,s);
  ADD2(e,ee,sn,ssn,e,ee,r,s);                    /* e+ee=sin(x+dx) */

  v[0]=e;
  v[1]=ee;
}
/**********************************************************************/
/* Routine receive Double-Length number (x+dx) and computes cos(x+dx) */
/* as Double-Length number and store it in array v .It computes it by */
/* arithmetic action on Double-Length numbers                         */
/*(x+dx) between 0 and PI/4                                           */
/**********************************************************************/

void __dubcos(double x, double dx, double v[]) {
  double r,s,p,hx,tx,hy,ty,q,c,cc,d,dd,d2,dd2,e,ee,
    sn,ssn,cs,ccs,ds,dss,dc,dcc;
#if 0
  double xx,y,yy,z,zz;
#endif
  mynumber u;
  int4 k;
  u.x=x+big.x;
  k = u.i[LOW_HALF]<<2;
  x=x-(u.x-big.x);
  d=x+dx;
  dd=(x-d)+dx;  /* cos(x+dx)=cos(Xi+t)=cos(Xi)cos(t) - sin(Xi)sin(t) */
  MUL2(d,dd,d,dd,d2,dd2,p,hx,tx,hy,ty,q,c,cc);
  sn=sincos.x[k];     /*                                  */
  ssn=sincos.x[k+1];  /*      sin(Xi) and cos(Xi)         */
  cs=sincos.x[k+2];   /*                                  */
  ccs=sincos.x[k+3];  /*                                  */
  MUL2(d2,dd2,s7.x,ss7.x,ds,dss,p,hx,tx,hy,ty,q,c,cc);
  ADD2(ds,dss,s5.x,ss5.x,ds,dss,r,s);
  MUL2(d2,dd2,ds,dss,ds,dss,p,hx,tx,hy,ty,q,c,cc);
  ADD2(ds,dss,s3.x,ss3.x,ds,dss,r,s);
  MUL2(d2,dd2,ds,dss,ds,dss,p,hx,tx,hy,ty,q,c,cc);
  MUL2(d,dd,ds,dss,ds,dss,p,hx,tx,hy,ty,q,c,cc);
  ADD2(ds,dss,d,dd,ds,dss,r,s);

  MUL2(d2,dd2,c8.x,cc8.x,dc,dcc,p,hx,tx,hy,ty,q,c,cc);
  ADD2(dc,dcc,c6.x,cc6.x,dc,dcc,r,s);
  MUL2(d2,dd2,dc,dcc,dc,dcc,p,hx,tx,hy,ty,q,c,cc);
  ADD2(dc,dcc,c4.x,cc4.x,dc,dcc,r,s);
  MUL2(d2,dd2,dc,dcc,dc,dcc,p,hx,tx,hy,ty,q,c,cc);
  ADD2(dc,dcc,c2.x,cc2.x,dc,dcc,r,s);
  MUL2(d2,dd2,dc,dcc,dc,dcc,p,hx,tx,hy,ty,q,c,cc);

  MUL2(cs,ccs,ds,dss,e,ee,p,hx,tx,hy,ty,q,c,cc);
  MUL2(dc,dcc,sn,ssn,dc,dcc,p,hx,tx,hy,ty,q,c,cc);

  MUL2(d2,dd2,s7.x,ss7.x,ds,dss,p,hx,tx,hy,ty,q,c,cc);
  ADD2(ds,dss,s5.x,ss5.x,ds,dss,r,s);
  MUL2(d2,dd2,ds,dss,ds,dss,p,hx,tx,hy,ty,q,c,cc);
  ADD2(ds,dss,s3.x,ss3.x,ds,dss,r,s);
  MUL2(d2,dd2,ds,dss,ds,dss,p,hx,tx,hy,ty,q,c,cc);
  MUL2(d,dd,ds,dss,ds,dss,p,hx,tx,hy,ty,q,c,cc);
  ADD2(ds,dss,d,dd,ds,dss,r,s);
  MUL2(d2,dd2,c8.x,cc8.x,dc,dcc,p,hx,tx,hy,ty,q,c,cc);
  ADD2(dc,dcc,c6.x,cc6.x,dc,dcc,r,s);
  MUL2(d2,dd2,dc,dcc,dc,dcc,p,hx,tx,hy,ty,q,c,cc);
  ADD2(dc,dcc,c4.x,cc4.x,dc,dcc,r,s);
  MUL2(d2,dd2,dc,dcc,dc,dcc,p,hx,tx,hy,ty,q,c,cc);
  ADD2(dc,dcc,c2.x,cc2.x,dc,dcc,r,s);
  MUL2(d2,dd2,dc,dcc,dc,dcc,p,hx,tx,hy,ty,q,c,cc);
  MUL2(sn,ssn,ds,dss,e,ee,p,hx,tx,hy,ty,q,c,cc);
  MUL2(dc,dcc,cs,ccs,dc,dcc,p,hx,tx,hy,ty,q,c,cc);
  ADD2(e,ee,dc,dcc,e,ee,r,s);
  SUB2(cs,ccs,e,ee,e,ee,r,s);

  v[0]=e;
  v[1]=ee;
}
/**********************************************************************/
/* Routine receive Double-Length number (x+dx) and computes cos(x+dx) */
/* as Double-Length number and store it in array v                    */
/**********************************************************************/
void __docos(double x, double dx, double v[]) {
  double y,yy,p,w[2];
  if (x>0) {y=x; yy=dx;}
     else {y=-x; yy=-dx;}
  if (y<0.5*hp0.x)                                 /*  y< PI/4    */
           {__dubcos(y,yy,w); v[0]=w[0]; v[1]=w[1];}
     else if (y<1.5*hp0.x) {                       /* y< 3/4 * PI */
       p=hp0.x-y;  /* p = PI/2 - y */
       yy=hp1.x-yy;
       y=p+yy;
       yy=(p-y)+yy;
       if (y>0) {__dubsin(y,yy,w); v[0]=w[0]; v[1]=w[1];}
                                       /* cos(x) = sin ( 90 -  x ) */
         else {__dubsin(-y,-yy,w); v[0]=-w[0]; v[1]=-w[1];
	 }
     }
  else { /* y>= 3/4 * PI */
    p=2.0*hp0.x-y;    /* p = PI- y */
    yy=2.0*hp1.x-yy;
    y=p+yy;
    yy=(p-y)+yy;
    __dubcos(y,yy,w);
    v[0]=-w[0];
    v[1]=-w[1];
  }
}
