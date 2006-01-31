
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
/*  MODULE_NAME: mpa.c                                                  */
/*                                                                      */
/*  FUNCTIONS:                                                          */
/*               mcr                                                    */
/*               acr                                                    */
/*               cr                                                     */
/*               cpy                                                    */
/*               cpymn                                                  */
/*               norm                                                   */
/*               denorm                                                 */
/*               mp_dbl                                                 */
/*               dbl_mp                                                 */
/*               add_magnitudes                                         */
/*               sub_magnitudes                                         */
/*               add                                                    */
/*               sub                                                    */
/*               mul                                                    */
/*               inv                                                    */
/*               dvd                                                    */
/*                                                                      */
/* Arithmetic functions for multiple precision numbers.                 */
/* Relative errors are bounded                                          */
/************************************************************************/


#include "endian.h"
#include "mpa.h"
#include "mpa2.h"
#include <sys/param.h>	/* For MIN() */
/* mcr() compares the sizes of the mantissas of two multiple precision  */
/* numbers. Mantissas are compared regardless of the signs of the       */
/* numbers, even if x->d[0] or y->d[0] are zero. Exponents are also     */
/* disregarded.                                                         */
static int mcr(const mp_no *x, const mp_no *y, int p) {
  int i;
  for (i=1; i<=p; i++) {
    if      (X[i] == Y[i])  continue;
    else if (X[i] >  Y[i])  return  1;
    else                    return -1; }
  return 0;
}



/* acr() compares the absolute values of two multiple precision numbers */
int __acr(const mp_no *x, const mp_no *y, int p) {
  int i;

  if      (X[0] == ZERO) {
    if    (Y[0] == ZERO) i= 0;
    else                 i=-1;
  }
  else if (Y[0] == ZERO) i= 1;
  else {
    if      (EX >  EY)   i= 1;
    else if (EX <  EY)   i=-1;
    else                 i= mcr(x,y,p);
  }

  return i;
}


/* cr90 compares the values of two multiple precision numbers           */
int  __cr(const mp_no *x, const mp_no *y, int p) {
  int i;

  if      (X[0] > Y[0])  i= 1;
  else if (X[0] < Y[0])  i=-1;
  else if (X[0] < ZERO ) i= __acr(y,x,p);
  else                   i= __acr(x,y,p);

  return i;
}


/* Copy a multiple precision number. Set *y=*x. x=y is permissible.      */
void __cpy(const mp_no *x, mp_no *y, int p) {
  int i;

  EY = EX;
  for (i=0; i <= p; i++)    Y[i] = X[i];

  return;
}


/* Copy a multiple precision number x of precision m into a */
/* multiple precision number y of precision n. In case n>m, */
/* the digits of y beyond the m'th are set to zero. In case */
/* n<m, the digits of x beyond the n'th are ignored.        */
/* x=y is permissible.                                      */

void __cpymn(const mp_no *x, int m, mp_no *y, int n) {

  int i,k;

  EY = EX;     k=MIN(m,n);
  for (i=0; i <= k; i++)    Y[i] = X[i];
  for (   ; i <= n; i++)    Y[i] = ZERO;

  return;
}

/* Convert a multiple precision number *x into a double precision */
/* number *y, normalized case  (|x| >= 2**(-1022))) */
static void norm(const mp_no *x, double *y, int p)
{
  #define R  radixi.d
  int i;
#if 0
  int k;
#endif
  double a,c,u,v,z[5];
  if (p<5) {
    if      (p==1) c = X[1];
    else if (p==2) c = X[1] + R* X[2];
    else if (p==3) c = X[1] + R*(X[2]  +   R* X[3]);
    else if (p==4) c =(X[1] + R* X[2]) + R*R*(X[3] + R*X[4]);
  }
  else {
    for (a=ONE, z[1]=X[1]; z[1] < TWO23; )
        {a *= TWO;   z[1] *= TWO; }

    for (i=2; i<5; i++) {
      z[i] = X[i]*a;
      u = (z[i] + CUTTER)-CUTTER;
      if  (u > z[i])  u -= RADIX;
      z[i] -= u;
      z[i-1] += u*RADIXI;
    }

    u = (z[3] + TWO71) - TWO71;
    if (u > z[3])   u -= TWO19;
    v = z[3]-u;

    if (v == TWO18) {
      if (z[4] == ZERO) {
        for (i=5; i <= p; i++) {
          if (X[i] == ZERO)   continue;
          else                {z[3] += ONE;   break; }
        }
      }
      else              z[3] += ONE;
    }

    c = (z[1] + R *(z[2] + R * z[3]))/a;
  }

  c *= X[0];

  for (i=1; i<EX; i++)   c *= RADIX;
  for (i=1; i>EX; i--)   c *= RADIXI;

  *y = c;
  return;
#undef R
}

/* Convert a multiple precision number *x into a double precision */
/* number *y, denormalized case  (|x| < 2**(-1022))) */
static void denorm(const mp_no *x, double *y, int p)
{
  int i,k;
  double c,u,z[5];
#if 0
  double a,v;
#endif

#define R  radixi.d
  if (EX<-44 || (EX==-44 && X[1]<TWO5))
     { *y=ZERO; return; }

  if      (p==1) {
    if      (EX==-42) {z[1]=X[1]+TWO10;  z[2]=ZERO;  z[3]=ZERO;  k=3;}
    else if (EX==-43) {z[1]=     TWO10;  z[2]=X[1];  z[3]=ZERO;  k=2;}
    else              {z[1]=     TWO10;  z[2]=ZERO;  z[3]=X[1];  k=1;}
  }
  else if (p==2) {
    if      (EX==-42) {z[1]=X[1]+TWO10;  z[2]=X[2];  z[3]=ZERO;  k=3;}
    else if (EX==-43) {z[1]=     TWO10;  z[2]=X[1];  z[3]=X[2];  k=2;}
    else              {z[1]=     TWO10;  z[2]=ZERO;  z[3]=X[1];  k=1;}
  }
  else {
    if      (EX==-42) {z[1]=X[1]+TWO10;  z[2]=X[2];  k=3;}
    else if (EX==-43) {z[1]=     TWO10;  z[2]=X[1];  k=2;}
    else              {z[1]=     TWO10;  z[2]=ZERO;  k=1;}
    z[3] = X[k];
  }

  u = (z[3] + TWO57) - TWO57;
  if  (u > z[3])   u -= TWO5;

  if (u==z[3]) {
    for (i=k+1; i <= p; i++) {
      if (X[i] == ZERO)   continue;
      else {z[3] += ONE;   break; }
    }
  }

  c = X[0]*((z[1] + R*(z[2] + R*z[3])) - TWO10);

  *y = c*TWOM1032;
  return;

#undef R
}

/* Convert a multiple precision number *x into a double precision number *y. */
/* The result is correctly rounded to the nearest/even. *x is left unchanged */

void __mp_dbl(const mp_no *x, double *y, int p) {
#if 0
  int i,k;
  double a,c,u,v,z[5];
#endif

  if (X[0] == ZERO)  {*y = ZERO;  return; }

  if      (EX> -42)                 norm(x,y,p);
  else if (EX==-42 && X[1]>=TWO10)  norm(x,y,p);
  else                              denorm(x,y,p);
}


/* dbl_mp() converts a double precision number x into a multiple precision  */
/* number *y. If the precision p is too small the result is truncated. x is */
/* left unchanged.                                                          */

void __dbl_mp(double x, mp_no *y, int p) {

  int i,n;
  double u;

  /* Sign */
  if      (x == ZERO)  {Y[0] = ZERO;  return; }
  else if (x >  ZERO)   Y[0] = ONE;
  else                 {Y[0] = MONE;  x=-x;   }

  /* Exponent */
  for (EY=ONE; x >= RADIX; EY += ONE)   x *= RADIXI;
  for (      ; x <  ONE;   EY -= ONE)   x *= RADIX;

  /* Digits */
  n=MIN(p,4);
  for (i=1; i<=n; i++) {
    u = (x + TWO52) - TWO52;
    if (u>x)   u -= ONE;
    Y[i] = u;     x -= u;    x *= RADIX; }
  for (   ; i<=p; i++)     Y[i] = ZERO;
  return;
}


/*  add_magnitudes() adds the magnitudes of *x & *y assuming that           */
/*  abs(*x) >= abs(*y) > 0.                                                 */
/* The sign of the sum *z is undefined. x&y may overlap but not x&z or y&z. */
/* No guard digit is used. The result equals the exact sum, truncated.      */
/* *x & *y are left unchanged.                                              */

static void add_magnitudes(const mp_no *x, const mp_no *y, mp_no *z, int p) {

  int i,j,k;

  EZ = EX;

  i=p;    j=p+ EY - EX;    k=p+1;

  if (j<1)
     {__cpy(x,z,p);  return; }
  else   Z[k] = ZERO;

  for (; j>0; i--,j--) {
    Z[k] += X[i] + Y[j];
    if (Z[k] >= RADIX) {
      Z[k]  -= RADIX;
      Z[--k] = ONE; }
    else
      Z[--k] = ZERO;
  }

  for (; i>0; i--) {
    Z[k] += X[i];
    if (Z[k] >= RADIX) {
      Z[k]  -= RADIX;
      Z[--k] = ONE; }
    else
      Z[--k] = ZERO;
  }

  if (Z[1] == ZERO) {
    for (i=1; i<=p; i++)    Z[i] = Z[i+1]; }
  else   EZ += ONE;
}


/*  sub_magnitudes() subtracts the magnitudes of *x & *y assuming that      */
/*  abs(*x) > abs(*y) > 0.                                                  */
/* The sign of the difference *z is undefined. x&y may overlap but not x&z  */
/* or y&z. One guard digit is used. The error is less than one ulp.         */
/* *x & *y are left unchanged.                                              */

static void sub_magnitudes(const mp_no *x, const mp_no *y, mp_no *z, int p) {

  int i,j,k;

  EZ = EX;

  if (EX == EY) {
    i=j=k=p;
    Z[k] = Z[k+1] = ZERO; }
  else {
    j= EX - EY;
    if (j > p)  {__cpy(x,z,p);  return; }
    else {
      i=p;   j=p+1-j;   k=p;
      if (Y[j] > ZERO) {
        Z[k+1] = RADIX - Y[j--];
        Z[k]   = MONE; }
      else {
        Z[k+1] = ZERO;
        Z[k]   = ZERO;   j--;}
    }
  }

  for (; j>0; i--,j--) {
    Z[k] += (X[i] - Y[j]);
    if (Z[k] < ZERO) {
      Z[k]  += RADIX;
      Z[--k] = MONE; }
    else
      Z[--k] = ZERO;
  }

  for (; i>0; i--) {
    Z[k] += X[i];
    if (Z[k] < ZERO) {
      Z[k]  += RADIX;
      Z[--k] = MONE; }
    else
      Z[--k] = ZERO;
  }

  for (i=1; Z[i] == ZERO; i++) ;
  EZ = EZ - i + 1;
  for (k=1; i <= p+1; )
    Z[k++] = Z[i++];
  for (; k <= p; )
    Z[k++] = ZERO;

  return;
}


/* Add two multiple precision numbers. Set *z = *x + *y. x&y may overlap  */
/* but not x&z or y&z. One guard digit is used. The error is less than    */
/* one ulp. *x & *y are left unchanged.                                   */

void __add(const mp_no *x, const mp_no *y, mp_no *z, int p) {

  int n;

  if      (X[0] == ZERO)     {__cpy(y,z,p);  return; }
  else if (Y[0] == ZERO)     {__cpy(x,z,p);  return; }

  if (X[0] == Y[0])   {
    if (__acr(x,y,p) > 0)      {add_magnitudes(x,y,z,p);  Z[0] = X[0]; }
    else                     {add_magnitudes(y,x,z,p);  Z[0] = Y[0]; }
  }
  else                       {
    if ((n=__acr(x,y,p)) == 1) {sub_magnitudes(x,y,z,p);  Z[0] = X[0]; }
    else if (n == -1)        {sub_magnitudes(y,x,z,p);  Z[0] = Y[0]; }
    else                      Z[0] = ZERO;
  }
  return;
}


/* Subtract two multiple precision numbers. *z is set to *x - *y. x&y may */
/* overlap but not x&z or y&z. One guard digit is used. The error is      */
/* less than one ulp. *x & *y are left unchanged.                         */

void __sub(const mp_no *x, const mp_no *y, mp_no *z, int p) {

  int n;

  if      (X[0] == ZERO)     {__cpy(y,z,p);  Z[0] = -Z[0];  return; }
  else if (Y[0] == ZERO)     {__cpy(x,z,p);                 return; }

  if (X[0] != Y[0])    {
    if (__acr(x,y,p) > 0)      {add_magnitudes(x,y,z,p);  Z[0] =  X[0]; }
    else                     {add_magnitudes(y,x,z,p);  Z[0] = -Y[0]; }
  }
  else                       {
    if ((n=__acr(x,y,p)) == 1) {sub_magnitudes(x,y,z,p);  Z[0] =  X[0]; }
    else if (n == -1)        {sub_magnitudes(y,x,z,p);  Z[0] = -Y[0]; }
    else                      Z[0] = ZERO;
  }
  return;
}


/* Multiply two multiple precision numbers. *z is set to *x * *y. x&y      */
/* may overlap but not x&z or y&z. In case p=1,2,3 the exact result is     */
/* truncated to p digits. In case p>3 the error is bounded by 1.001 ulp.   */
/* *x & *y are left unchanged.                                             */

void __mul(const mp_no *x, const mp_no *y, mp_no *z, int p) {

  int i, i1, i2, j, k, k2;
  double u;

                      /* Is z=0? */
  if (X[0]*Y[0]==ZERO)
     { Z[0]=ZERO;  return; }

                       /* Multiply, add and carry */
  k2 = (p<3) ? p+p : p+3;
  Z[k2]=ZERO;
  for (k=k2; k>1; ) {
    if (k > p)  {i1=k-p; i2=p+1; }
    else        {i1=1;   i2=k;   }
    for (i=i1,j=i2-1; i<i2; i++,j--)  Z[k] += X[i]*Y[j];

    u = (Z[k] + CUTTER)-CUTTER;
    if  (u > Z[k])  u -= RADIX;
    Z[k]  -= u;
    Z[--k] = u*RADIXI;
  }

                 /* Is there a carry beyond the most significant digit? */
  if (Z[1] == ZERO) {
    for (i=1; i<=p; i++)  Z[i]=Z[i+1];
    EZ = EX + EY - 1; }
  else
    EZ = EX + EY;

  Z[0] = X[0] * Y[0];
  return;
}


/* Invert a multiple precision number. Set *y = 1 / *x.                     */
/* Relative error bound = 1.001*r**(1-p) for p=2, 1.063*r**(1-p) for p=3,   */
/* 2.001*r**(1-p) for p>3.                                                  */
/* *x=0 is not permissible. *x is left unchanged.                           */

void __inv(const mp_no *x, mp_no *y, int p) {
  int i;
#if 0
  int l;
#endif
  double t;
  mp_no z,w;
  static const int np1[] = {0,0,0,0,1,2,2,2,2,3,3,3,3,3,3,3,3,3,
                            4,4,4,4,4,4,4,4,4,4,4,4,4,4,4};
  const mp_no mptwo = {1,{1.0,2.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                         0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                         0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                         0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0}};

  __cpy(x,&z,p);  z.e=0;  __mp_dbl(&z,&t,p);
  t=ONE/t;   __dbl_mp(t,y,p);    EY -= EX;

  for (i=0; i<np1[p]; i++) {
    __cpy(y,&w,p);
    __mul(x,&w,y,p);
    __sub(&mptwo,y,&z,p);
    __mul(&w,&z,y,p);
  }
  return;
}


/* Divide one multiple precision number by another.Set *z = *x / *y. *x & *y */
/* are left unchanged. x&y may overlap but not x&z or y&z.                   */
/* Relative error bound = 2.001*r**(1-p) for p=2, 2.063*r**(1-p) for p=3     */
/* and 3.001*r**(1-p) for p>3. *y=0 is not permissible.                      */

void __dvd(const mp_no *x, const mp_no *y, mp_no *z, int p) {

  mp_no w;

  if (X[0] == ZERO)    Z[0] = ZERO;
  else                {__inv(y,&w,p);   __mul(x,&w,z,p);}
  return;
}
