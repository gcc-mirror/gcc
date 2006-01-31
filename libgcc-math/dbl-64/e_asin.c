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
/******************************************************************/
/*     MODULE_NAME:uasncs.c                                       */
/*                                                                */
/*     FUNCTIONS: uasin                                           */
/*                uacos                                           */
/* FILES NEEDED: dla.h endian.h mpa.h mydefs.h  usncs.h           */
/*               doasin.c sincos32.c dosincos.c mpa.c             */
/*               sincos.tbl  asincos.tbl  powtwo.tbl root.tbl     */
/*                                                                */
/* Ultimate asin/acos routines. Given an IEEE double machine      */
/* number x, compute the correctly rounded value of               */
/* arcsin(x)or arccos(x)  according to the function called.       */
/* Assumption: Machine arithmetic operations are performed in     */
/* round to nearest mode of IEEE 754 standard.                    */
/*                                                                */
/******************************************************************/
#include "endian.h"
#include "mydefs.h"
#include "asincos.tbl"
#include "root.tbl"
#include "powtwo.tbl"
#include "MathLib.h"
#include "uasncs.h"
#include "math_private.h"

void __doasin(double x, double dx, double w[]);
void __dubsin(double x, double dx, double v[]);
void __dubcos(double x, double dx, double v[]);
void __docos(double x, double dx, double v[]);
double __sin32(double x, double res, double res1);
double __cos32(double x, double res, double res1);

/***************************************************************************/
/* An ultimate asin routine. Given an IEEE double machine number x         */
/* it computes the correctly rounded (to nearest) value of arcsin(x)       */
/***************************************************************************/
double __ieee754_asin(double x){
  double x1,x2,xx,s1,s2,res1,p,t,res,r,cor,cc,y,c,z,w[2];
  mynumber u,v;
  int4 k,m,n;
#if 0
  int4 nn;
#endif

  u.x = x;
  m = u.i[HIGH_HALF];
  k = 0x7fffffff&m;              /* no sign */

  if (k < 0x3e500000) return x;  /* for x->0 => sin(x)=x */
  /*----------------------2^-26 <= |x| < 2^ -3    -----------------*/
  else
  if (k < 0x3fc00000) {
    x2 = x*x;
    t = (((((f6*x2 + f5)*x2 + f4)*x2 + f3)*x2 + f2)*x2 + f1)*(x2*x);
    res = x+t;         /*  res=arcsin(x) according to Taylor series  */
    cor = (x-res)+t;
    if (res == res+1.025*cor) return res;
    else {
      x1 = x+big;
      xx = x*x;
      x1 -= big;
      x2 = x - x1;
      p = x1*x1*x1;
      s1 = a1.x*p;
      s2 = ((((((c7*xx + c6)*xx + c5)*xx + c4)*xx + c3)*xx + c2)*xx*xx*x +
	     ((a1.x+a2.x)*x2*x2+ 0.5*x1*x)*x2) + a2.x*p;
      res1 = x+s1;
      s2 = ((x-res1)+s1)+s2;
      res = res1+s2;
      cor = (res1-res)+s2;
      if (res == res+1.00014*cor) return res;
      else {
	__doasin(x,0,w);
	if (w[0]==(w[0]+1.00000001*w[1])) return w[0];
	else {
	  y=ABS(x);
	  res=ABS(w[0]);
	  res1=ABS(w[0]+1.1*w[1]);
	  return (m>0)?__sin32(y,res,res1):-__sin32(y,res,res1);
	}
      }
    }
  }
  /*---------------------0.125 <= |x| < 0.5 -----------------------------*/
  else if (k < 0x3fe00000) {
    if (k<0x3fd00000) n = 11*((k&0x000fffff)>>15);
    else n = 11*((k&0x000fffff)>>14)+352;
    if (m>0) xx = x - asncs.x[n];
    else xx = -x - asncs.x[n];
    t = asncs.x[n+1]*xx;
    p=xx*xx*(asncs.x[n+2]+xx*(asncs.x[n+3]+xx*(asncs.x[n+4]+xx*(asncs.x[n+5]
     +xx*asncs.x[n+6]))))+asncs.x[n+7];
    t+=p;
    res =asncs.x[n+8] +t;
    cor = (asncs.x[n+8]-res)+t;
    if (res == res+1.05*cor) return (m>0)?res:-res;
    else {
      r=asncs.x[n+8]+xx*asncs.x[n+9];
      t=((asncs.x[n+8]-r)+xx*asncs.x[n+9])+(p+xx*asncs.x[n+10]);
      res = r+t;
      cor = (r-res)+t;
      if (res == res+1.0005*cor) return (m>0)?res:-res;
      else {
	res1=res+1.1*cor;
	z=0.5*(res1-res);
	__dubsin(res,z,w);
	z=(w[0]-ABS(x))+w[1];
	if (z>1.0e-27) return (m>0)?min(res,res1):-min(res,res1);
	else if (z<-1.0e-27) return (m>0)?max(res,res1):-max(res,res1);
	else {
	  y=ABS(x);
	  return (m>0)?__sin32(y,res,res1):-__sin32(y,res,res1);
	}
      }
    }
  }    /*   else  if (k < 0x3fe00000)    */
  /*-------------------- 0.5 <= |x| < 0.75 -----------------------------*/
  else
  if (k < 0x3fe80000) {
    n = 1056+((k&0x000fe000)>>11)*3;
    if (m>0) xx = x - asncs.x[n];
    else xx = -x - asncs.x[n];
    t = asncs.x[n+1]*xx;
    p=xx*xx*(asncs.x[n+2]+xx*(asncs.x[n+3]+xx*(asncs.x[n+4]+xx*(asncs.x[n+5]
	   +xx*(asncs.x[n+6]+xx*asncs.x[n+7])))))+asncs.x[n+8];
    t+=p;
    res =asncs.x[n+9] +t;
    cor = (asncs.x[n+9]-res)+t;
    if (res == res+1.01*cor) return (m>0)?res:-res;
    else {
      r=asncs.x[n+9]+xx*asncs.x[n+10];
      t=((asncs.x[n+9]-r)+xx*asncs.x[n+10])+(p+xx*asncs.x[n+11]);
      res = r+t;
      cor = (r-res)+t;
      if (res == res+1.0005*cor) return (m>0)?res:-res;
      else {
	res1=res+1.1*cor;
	z=0.5*(res1-res);
	__dubsin(res,z,w);
	z=(w[0]-ABS(x))+w[1];
	if (z>1.0e-27) return (m>0)?min(res,res1):-min(res,res1);
	else if (z<-1.0e-27) return (m>0)?max(res,res1):-max(res,res1);
	else {
	  y=ABS(x);
	  return (m>0)?__sin32(y,res,res1):-__sin32(y,res,res1);
	}
      }
    }
  }    /*   else  if (k < 0x3fe80000)    */
  /*--------------------- 0.75 <= |x|< 0.921875 ----------------------*/
  else
  if (k < 0x3fed8000) {
    n = 992+((k&0x000fe000)>>13)*13;
    if (m>0) xx = x - asncs.x[n];
    else xx = -x - asncs.x[n];
    t = asncs.x[n+1]*xx;
    p=xx*xx*(asncs.x[n+2]+xx*(asncs.x[n+3]+xx*(asncs.x[n+4]+xx*(asncs.x[n+5]
     +xx*(asncs.x[n+6]+xx*(asncs.x[n+7]+xx*asncs.x[n+8]))))))+asncs.x[n+9];
    t+=p;
    res =asncs.x[n+10] +t;
    cor = (asncs.x[n+10]-res)+t;
    if (res == res+1.01*cor) return (m>0)?res:-res;
    else {
      r=asncs.x[n+10]+xx*asncs.x[n+11];
      t=((asncs.x[n+10]-r)+xx*asncs.x[n+11])+(p+xx*asncs.x[n+12]);
      res = r+t;
      cor = (r-res)+t;
      if (res == res+1.0008*cor) return (m>0)?res:-res;
      else {
	res1=res+1.1*cor;
	z=0.5*(res1-res);
	y=hp0.x-res;
	z=((hp0.x-y)-res)+(hp1.x-z);
	__dubcos(y,z,w);
	z=(w[0]-ABS(x))+w[1];
	if (z>1.0e-27) return (m>0)?min(res,res1):-min(res,res1);
	else if (z<-1.0e-27) return (m>0)?max(res,res1):-max(res,res1);
	else {
	  y=ABS(x);
	  return (m>0)?__sin32(y,res,res1):-__sin32(y,res,res1);
	}
      }
    }
  }    /*   else  if (k < 0x3fed8000)    */
  /*-------------------0.921875 <= |x| < 0.953125 ------------------------*/
  else
  if (k < 0x3fee8000) {
    n = 884+((k&0x000fe000)>>13)*14;
    if (m>0) xx = x - asncs.x[n];
    else xx = -x - asncs.x[n];
    t = asncs.x[n+1]*xx;
    p=xx*xx*(asncs.x[n+2]+xx*(asncs.x[n+3]+xx*(asncs.x[n+4]+
                      xx*(asncs.x[n+5]+xx*(asncs.x[n+6]
		      +xx*(asncs.x[n+7]+xx*(asncs.x[n+8]+
                      xx*asncs.x[n+9])))))))+asncs.x[n+10];
    t+=p;
    res =asncs.x[n+11] +t;
    cor = (asncs.x[n+11]-res)+t;
    if (res == res+1.01*cor) return (m>0)?res:-res;
    else {
      r=asncs.x[n+11]+xx*asncs.x[n+12];
      t=((asncs.x[n+11]-r)+xx*asncs.x[n+12])+(p+xx*asncs.x[n+13]);
      res = r+t;
      cor = (r-res)+t;
      if (res == res+1.0007*cor) return (m>0)?res:-res;
      else {
	res1=res+1.1*cor;
	z=0.5*(res1-res);
	y=(hp0.x-res)-z;
	z=y+hp1.x;
	y=(y-z)+hp1.x;
	__dubcos(z,y,w);
	z=(w[0]-ABS(x))+w[1];
	if (z>1.0e-27) return (m>0)?min(res,res1):-min(res,res1);
	else if (z<-1.0e-27) return (m>0)?max(res,res1):-max(res,res1);
	else {
	  y=ABS(x);
	  return (m>0)?__sin32(y,res,res1):-__sin32(y,res,res1);
	}
      }
    }
  }    /*   else  if (k < 0x3fee8000)    */

  /*--------------------0.953125 <= |x| < 0.96875 ------------------------*/
  else
  if (k < 0x3fef0000) {
    n = 768+((k&0x000fe000)>>13)*15;
    if (m>0) xx = x - asncs.x[n];
    else xx = -x - asncs.x[n];
    t = asncs.x[n+1]*xx;
    p=xx*xx*(asncs.x[n+2]+xx*(asncs.x[n+3]+xx*(asncs.x[n+4]+
                         xx*(asncs.x[n+5]+xx*(asncs.x[n+6]
			 +xx*(asncs.x[n+7]+xx*(asncs.x[n+8]+
                    xx*(asncs.x[n+9]+xx*asncs.x[n+10]))))))))+asncs.x[n+11];
    t+=p;
    res =asncs.x[n+12] +t;
    cor = (asncs.x[n+12]-res)+t;
    if (res == res+1.01*cor) return (m>0)?res:-res;
    else {
      r=asncs.x[n+12]+xx*asncs.x[n+13];
      t=((asncs.x[n+12]-r)+xx*asncs.x[n+13])+(p+xx*asncs.x[n+14]);
      res = r+t;
      cor = (r-res)+t;
      if (res == res+1.0007*cor) return (m>0)?res:-res;
      else {
	res1=res+1.1*cor;
	z=0.5*(res1-res);
	y=(hp0.x-res)-z;
	z=y+hp1.x;
	y=(y-z)+hp1.x;
	__dubcos(z,y,w);
	z=(w[0]-ABS(x))+w[1];
	if (z>1.0e-27) return (m>0)?min(res,res1):-min(res,res1);
	else if (z<-1.0e-27) return (m>0)?max(res,res1):-max(res,res1);
	else {
	  y=ABS(x);
	  return (m>0)?__sin32(y,res,res1):-__sin32(y,res,res1);
	}
      }
    }
  }    /*   else  if (k < 0x3fef0000)    */
  /*--------------------0.96875 <= |x| < 1 --------------------------------*/
  else
  if (k<0x3ff00000)  {
    z = 0.5*((m>0)?(1.0-x):(1.0+x));
    v.x=z;
    k=v.i[HIGH_HALF];
    t=inroot[(k&0x001fffff)>>14]*powtwo[511-(k>>21)];
    r=1.0-t*t*z;
    t = t*(rt0+r*(rt1+r*(rt2+r*rt3)));
    c=t*z;
    t=c*(1.5-0.5*t*c);
    y=(c+t24)-t24;
    cc = (z-y*y)/(t+y);
    p=(((((f6*z+f5)*z+f4)*z+f3)*z+f2)*z+f1)*z;
    cor = (hp1.x - 2.0*cc)-2.0*(y+cc)*p;
    res1 = hp0.x - 2.0*y;
    res =res1 + cor;
    if (res == res+1.003*((res1-res)+cor)) return (m>0)?res:-res;
    else {
      c=y+cc;
      cc=(y-c)+cc;
      __doasin(c,cc,w);
      res1=hp0.x-2.0*w[0];
      cor=((hp0.x-res1)-2.0*w[0])+(hp1.x-2.0*w[1]);
      res = res1+cor;
      cor = (res1-res)+cor;
      if (res==(res+1.0000001*cor)) return (m>0)?res:-res;
      else {
	y=ABS(x);
	res1=res+1.1*cor;
	return (m>0)?__sin32(y,res,res1):-__sin32(y,res,res1);
      }
    }
  }    /*   else  if (k < 0x3ff00000)    */
  /*---------------------------- |x|>=1 -------------------------------*/
  else if (k==0x3ff00000 && u.i[LOW_HALF]==0) return (m>0)?hp0.x:-hp0.x;
  else
  if (k>0x7ff00000 || (k == 0x7ff00000 && u.i[LOW_HALF] != 0)) return x;
  else {
    u.i[HIGH_HALF]=0x7ff00000;
    v.i[HIGH_HALF]=0x7ff00000;
    u.i[LOW_HALF]=0;
    v.i[LOW_HALF]=0;
    return u.x/v.x;  /* NaN */
 }
}

/*******************************************************************/
/*                                                                 */
/*         End of arcsine,  below is arccosine                     */
/*                                                                 */
/*******************************************************************/

double __ieee754_acos(double x)
{
  double x1,x2,xx,s1,s2,res1,p,t,res,r,cor,cc,y,c,z,w[2],eps;
#if 0
  double fc;
#endif
  mynumber u,v;
  int4 k,m,n;
#if 0
  int4 nn;
#endif
  u.x = x;
  m = u.i[HIGH_HALF];
  k = 0x7fffffff&m;
  /*-------------------  |x|<2.77556*10^-17 ----------------------*/
  if (k < 0x3c880000) return hp0.x;

  /*-----------------  2.77556*10^-17 <= |x| < 2^-3 --------------*/
  else
  if (k < 0x3fc00000) {
    x2 = x*x;
    t = (((((f6*x2 + f5)*x2 + f4)*x2 + f3)*x2 + f2)*x2 + f1)*(x2*x);
    r=hp0.x-x;
    cor=(((hp0.x-r)-x)+hp1.x)-t;
    res = r+cor;
    cor = (r-res)+cor;
    if (res == res+1.004*cor) return res;
    else {
      x1 = x+big;
      xx = x*x;
      x1 -= big;
      x2 = x - x1;
      p = x1*x1*x1;
      s1 = a1.x*p;
      s2 = ((((((c7*xx + c6)*xx + c5)*xx + c4)*xx + c3)*xx + c2)*xx*xx*x +
	    ((a1.x+a2.x)*x2*x2+ 0.5*x1*x)*x2) + a2.x*p;
      res1 = x+s1;
      s2 = ((x-res1)+s1)+s2;
      r=hp0.x-res1;
      cor=(((hp0.x-r)-res1)+hp1.x)-s2;
      res = r+cor;
      cor = (r-res)+cor;
      if (res == res+1.00004*cor) return res;
      else {
	__doasin(x,0,w);
	r=hp0.x-w[0];
	cor=((hp0.x-r)-w[0])+(hp1.x-w[1]);
	res=r+cor;
	cor=(r-res)+cor;
	if (res ==(res +1.00000001*cor)) return res;
	else {
	  res1=res+1.1*cor;
	  return __cos32(x,res,res1);
	}
      }
    }
  }    /*   else  if (k < 0x3fc00000)    */
  /*----------------------  0.125 <= |x| < 0.5 --------------------*/
  else
  if (k < 0x3fe00000) {
    if (k<0x3fd00000) n = 11*((k&0x000fffff)>>15);
    else n = 11*((k&0x000fffff)>>14)+352;
    if (m>0) xx = x - asncs.x[n];
    else xx = -x - asncs.x[n];
    t = asncs.x[n+1]*xx;
    p=xx*xx*(asncs.x[n+2]+xx*(asncs.x[n+3]+xx*(asncs.x[n+4]+
                   xx*(asncs.x[n+5]+xx*asncs.x[n+6]))))+asncs.x[n+7];
    t+=p;
    y = (m>0)?(hp0.x-asncs.x[n+8]):(hp0.x+asncs.x[n+8]);
    t = (m>0)?(hp1.x-t):(hp1.x+t);
    res = y+t;
    if (res == res+1.02*((y-res)+t)) return res;
    else {
      r=asncs.x[n+8]+xx*asncs.x[n+9];
      t=((asncs.x[n+8]-r)+xx*asncs.x[n+9])+(p+xx*asncs.x[n+10]);
      if (m>0)
	{p = hp0.x-r; t = (((hp0.x-p)-r)-t)+hp1.x; }
      else
	{p = hp0.x+r; t = ((hp0.x-p)+r)+(hp1.x+t); }
      res = p+t;
      cor = (p-res)+t;
      if (res == (res+1.0002*cor)) return res;
      else {
	res1=res+1.1*cor;
	z=0.5*(res1-res);
	__docos(res,z,w);
	z=(w[0]-x)+w[1];
	if (z>1.0e-27) return max(res,res1);
	else if (z<-1.0e-27) return min(res,res1);
	else return __cos32(x,res,res1);
      }
    }
  }    /*   else  if (k < 0x3fe00000)    */

  /*--------------------------- 0.5 <= |x| < 0.75 ---------------------*/
  else
  if (k < 0x3fe80000) {
    n = 1056+((k&0x000fe000)>>11)*3;
    if (m>0) {xx = x - asncs.x[n]; eps=1.04; }
    else {xx = -x - asncs.x[n]; eps=1.02; }
    t = asncs.x[n+1]*xx;
    p=xx*xx*(asncs.x[n+2]+xx*(asncs.x[n+3]+xx*(asncs.x[n+4]+
                   xx*(asncs.x[n+5]+xx*(asncs.x[n+6]+
                   xx*asncs.x[n+7])))))+asncs.x[n+8];
    t+=p;
   y = (m>0)?(hp0.x-asncs.x[n+9]):(hp0.x+asncs.x[n+9]);
   t = (m>0)?(hp1.x-t):(hp1.x+t);
   res = y+t;
   if (res == res+eps*((y-res)+t)) return res;
   else {
     r=asncs.x[n+9]+xx*asncs.x[n+10];
     t=((asncs.x[n+9]-r)+xx*asncs.x[n+10])+(p+xx*asncs.x[n+11]);
     if (m>0) {p = hp0.x-r; t = (((hp0.x-p)-r)-t)+hp1.x; eps=1.0004; }
     else   {p = hp0.x+r; t = ((hp0.x-p)+r)+(hp1.x+t); eps=1.0002; }
     res = p+t;
     cor = (p-res)+t;
     if (res == (res+eps*cor)) return res;
     else {
       res1=res+1.1*cor;
       z=0.5*(res1-res);
       __docos(res,z,w);
       z=(w[0]-x)+w[1];
       if (z>1.0e-27) return max(res,res1);
       else if (z<-1.0e-27) return min(res,res1);
       else return __cos32(x,res,res1);
     }
   }
  }    /*   else  if (k < 0x3fe80000)    */

/*------------------------- 0.75 <= |x| < 0.921875 -------------*/
  else
  if (k < 0x3fed8000) {
    n = 992+((k&0x000fe000)>>13)*13;
    if (m>0) {xx = x - asncs.x[n]; eps = 1.04; }
    else {xx = -x - asncs.x[n]; eps = 1.01; }
    t = asncs.x[n+1]*xx;
    p=xx*xx*(asncs.x[n+2]+xx*(asncs.x[n+3]+xx*(asncs.x[n+4]+
                      xx*(asncs.x[n+5]+xx*(asncs.x[n+6]+xx*(asncs.x[n+7]+
                      xx*asncs.x[n+8]))))))+asncs.x[n+9];
    t+=p;
    y = (m>0)?(hp0.x-asncs.x[n+10]):(hp0.x+asncs.x[n+10]);
    t = (m>0)?(hp1.x-t):(hp1.x+t);
    res = y+t;
    if (res == res+eps*((y-res)+t)) return res;
    else {
      r=asncs.x[n+10]+xx*asncs.x[n+11];
      t=((asncs.x[n+10]-r)+xx*asncs.x[n+11])+(p+xx*asncs.x[n+12]);
      if (m>0) {p = hp0.x-r; t = (((hp0.x-p)-r)-t)+hp1.x; eps=1.0032; }
      else   {p = hp0.x+r; t = ((hp0.x-p)+r)+(hp1.x+t); eps=1.0008; }
      res = p+t;
      cor = (p-res)+t;
      if (res == (res+eps*cor)) return res;
      else {
	res1=res+1.1*cor;
	z=0.5*(res1-res);
	__docos(res,z,w);
	z=(w[0]-x)+w[1];
	if (z>1.0e-27) return max(res,res1);
	else if (z<-1.0e-27) return min(res,res1);
	else return __cos32(x,res,res1);
      }
    }
  }    /*   else  if (k < 0x3fed8000)    */

/*-------------------0.921875 <= |x| < 0.953125 ------------------*/
  else
  if (k < 0x3fee8000) {
    n = 884+((k&0x000fe000)>>13)*14;
    if (m>0) {xx = x - asncs.x[n]; eps=1.04; }
    else {xx = -x - asncs.x[n]; eps =1.005; }
    t = asncs.x[n+1]*xx;
    p=xx*xx*(asncs.x[n+2]+xx*(asncs.x[n+3]+xx*(asncs.x[n+4]+
                   xx*(asncs.x[n+5]+xx*(asncs.x[n+6]
		   +xx*(asncs.x[n+7]+xx*(asncs.x[n+8]+
                   xx*asncs.x[n+9])))))))+asncs.x[n+10];
    t+=p;
    y = (m>0)?(hp0.x-asncs.x[n+11]):(hp0.x+asncs.x[n+11]);
    t = (m>0)?(hp1.x-t):(hp1.x+t);
    res = y+t;
    if (res == res+eps*((y-res)+t)) return res;
    else {
      r=asncs.x[n+11]+xx*asncs.x[n+12];
      t=((asncs.x[n+11]-r)+xx*asncs.x[n+12])+(p+xx*asncs.x[n+13]);
      if (m>0) {p = hp0.x-r; t = (((hp0.x-p)-r)-t)+hp1.x; eps=1.0030; }
      else   {p = hp0.x+r; t = ((hp0.x-p)+r)+(hp1.x+t); eps=1.0005; }
      res = p+t;
      cor = (p-res)+t;
      if (res == (res+eps*cor)) return res;
      else {
	res1=res+1.1*cor;
	z=0.5*(res1-res);
	__docos(res,z,w);
	z=(w[0]-x)+w[1];
	if (z>1.0e-27) return max(res,res1);
	else if (z<-1.0e-27) return min(res,res1);
	else return __cos32(x,res,res1);
      }
    }
  }    /*   else  if (k < 0x3fee8000)    */

  /*--------------------0.953125 <= |x| < 0.96875 ----------------*/
  else
  if (k < 0x3fef0000) {
    n = 768+((k&0x000fe000)>>13)*15;
    if (m>0) {xx = x - asncs.x[n]; eps=1.04; }
    else {xx = -x - asncs.x[n]; eps=1.005;}
    t = asncs.x[n+1]*xx;
    p=xx*xx*(asncs.x[n+2]+xx*(asncs.x[n+3]+xx*(asncs.x[n+4]+
            xx*(asncs.x[n+5]+xx*(asncs.x[n+6]
	    +xx*(asncs.x[n+7]+xx*(asncs.x[n+8]+xx*(asncs.x[n+9]+
            xx*asncs.x[n+10]))))))))+asncs.x[n+11];
    t+=p;
    y = (m>0)?(hp0.x-asncs.x[n+12]):(hp0.x+asncs.x[n+12]);
   t = (m>0)?(hp1.x-t):(hp1.x+t);
   res = y+t;
   if (res == res+eps*((y-res)+t)) return res;
   else {
     r=asncs.x[n+12]+xx*asncs.x[n+13];
     t=((asncs.x[n+12]-r)+xx*asncs.x[n+13])+(p+xx*asncs.x[n+14]);
     if (m>0) {p = hp0.x-r; t = (((hp0.x-p)-r)-t)+hp1.x; eps=1.0030; }
     else   {p = hp0.x+r; t = ((hp0.x-p)+r)+(hp1.x+t); eps=1.0005; }
     res = p+t;
     cor = (p-res)+t;
     if (res == (res+eps*cor)) return res;
     else {
       res1=res+1.1*cor;
       z=0.5*(res1-res);
       __docos(res,z,w);
       z=(w[0]-x)+w[1];
       if (z>1.0e-27) return max(res,res1);
       else if (z<-1.0e-27) return min(res,res1);
       else return __cos32(x,res,res1);
     }
   }
  }    /*   else  if (k < 0x3fef0000)    */
  /*-----------------0.96875 <= |x| < 1 ---------------------------*/

  else
  if (k<0x3ff00000)  {
    z = 0.5*((m>0)?(1.0-x):(1.0+x));
    v.x=z;
    k=v.i[HIGH_HALF];
    t=inroot[(k&0x001fffff)>>14]*powtwo[511-(k>>21)];
    r=1.0-t*t*z;
    t = t*(rt0+r*(rt1+r*(rt2+r*rt3)));
    c=t*z;
    t=c*(1.5-0.5*t*c);
    y = (t27*c+c)-t27*c;
    cc = (z-y*y)/(t+y);
    p=(((((f6*z+f5)*z+f4)*z+f3)*z+f2)*z+f1)*z;
    if (m<0) {
      cor = (hp1.x - cc)-(y+cc)*p;
      res1 = hp0.x - y;
      res =res1 + cor;
      if (res == res+1.002*((res1-res)+cor)) return (res+res);
      else {
	c=y+cc;
	cc=(y-c)+cc;
	__doasin(c,cc,w);
	res1=hp0.x-w[0];
	cor=((hp0.x-res1)-w[0])+(hp1.x-w[1]);
	res = res1+cor;
	cor = (res1-res)+cor;
	if (res==(res+1.000001*cor)) return (res+res);
	else {
	  res=res+res;
	  res1=res+1.2*cor;
	  return __cos32(x,res,res1);
	}
      }
    }
    else {
      cor = cc+p*(y+cc);
      res = y + cor;
      if (res == res+1.03*((y-res)+cor)) return (res+res);
      else {
	c=y+cc;
	cc=(y-c)+cc;
	__doasin(c,cc,w);
	res = w[0];
	cor=w[1];
	if (res==(res+1.000001*cor)) return (res+res);
	else {
	  res=res+res;
	  res1=res+1.2*cor;
	  return __cos32(x,res,res1);
	}
      }
    }
  }    /*   else  if (k < 0x3ff00000)    */

  /*---------------------------- |x|>=1 -----------------------*/
  else
  if (k==0x3ff00000 && u.i[LOW_HALF]==0) return (m>0)?0:2.0*hp0.x;
  else
  if (k>0x7ff00000 || (k == 0x7ff00000 && u.i[LOW_HALF] != 0)) return x;
  else {
    u.i[HIGH_HALF]=0x7ff00000;
    v.i[HIGH_HALF]=0x7ff00000;
    u.i[LOW_HALF]=0;
    v.i[LOW_HALF]=0;
    return u.x/v.x;
  }
}
