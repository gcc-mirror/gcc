
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
/****************************************************************************/
/*  MODULE_NAME:mpsqrt.c                                                    */
/*                                                                          */
/*  FUNCTION:mpsqrt                                                         */
/*           fastiroot                                                      */
/*                                                                          */
/* FILES NEEDED:endian.h mpa.h mpsqrt.h                                     */
/*              mpa.c                                                       */
/* Multi-Precision square root function subroutine for precision p >= 4.    */
/* The relative error is bounded by 3.501*r**(1-p), where r=2**24.          */
/*                                                                          */
/****************************************************************************/
#include "endian.h"
#include "mpa.h"

/****************************************************************************/
/* Multi-Precision square root function subroutine for precision p >= 4.    */
/* The relative error is bounded by 3.501*r**(1-p), where r=2**24.          */
/* Routine receives two pointers to  Multi Precision numbers:               */
/* x (left argument) and y (next argument). Routine also receives precision */
/* p as integer. Routine computes sqrt(*x) and stores result in *y          */
/****************************************************************************/

double fastiroot(double);

void __mpsqrt(mp_no *x, mp_no *y, int p) {
#include "mpsqrt.h"

  int i,m,ex,ey;
  double dx,dy;
  mp_no
    mphalf   = {0,{0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                   0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                   0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0}},
    mp3halfs = {0,{0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                   0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                   0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0}};
  mp_no mpxn,mpz,mpu,mpt1,mpt2;

  /* Prepare multi-precision 1/2 and 3/2 */
  mphalf.e  =0;  mphalf.d[0]  =ONE;  mphalf.d[1]  =HALFRAD;
  mp3halfs.e=1;  mp3halfs.d[0]=ONE;  mp3halfs.d[1]=ONE;  mp3halfs.d[2]=HALFRAD;

  ex=EX;      ey=EX/2;     __cpy(x,&mpxn,p);    mpxn.e -= (ey+ey);
  __mp_dbl(&mpxn,&dx,p);   dy=fastiroot(dx);    __dbl_mp(dy,&mpu,p);
  __mul(&mpxn,&mphalf,&mpz,p);

  m=mp[p];
  for (i=0; i<m; i++) {
    __mul(&mpu,&mpu,&mpt1,p);
    __mul(&mpt1,&mpz,&mpt2,p);
    __sub(&mp3halfs,&mpt2,&mpt1,p);
    __mul(&mpu,&mpt1,&mpt2,p);
    __cpy(&mpt2,&mpu,p);
  }
  __mul(&mpxn,&mpu,y,p);  EY += ey;

  return;
}

/***********************************************************/
/* Compute a double precision approximation for 1/sqrt(x)  */
/* with the relative error bounded by 2**-51.              */
/***********************************************************/
double fastiroot(double x) {
  union {int i[2]; double d;} p,q;
  double y,z, t;
  int n;
  static const double c0 = 0.99674, c1 = -0.53380, c2 = 0.45472, c3 = -0.21553;

  p.d = x;
  p.i[HIGH_HALF] = (p.i[HIGH_HALF] & 0x3FFFFFFF ) | 0x3FE00000 ;
  q.d = x;
  y = p.d;
  z = y -1.0;
  n = (q.i[HIGH_HALF] - p.i[HIGH_HALF])>>1;
  z = ((c3*z + c2)*z + c1)*z + c0;            /* 2**-7         */
  z = z*(1.5 - 0.5*y*z*z);                    /* 2**-14        */
  p.d = z*(1.5 - 0.5*y*z*z);                  /* 2**-28        */
  p.i[HIGH_HALF] -= n;
  t = x*p.d;
  return p.d*(1.5 - 0.5*p.d*t);
}
