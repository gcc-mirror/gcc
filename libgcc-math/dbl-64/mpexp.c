
/*
 * IBM Accurate Mathematical Library
 * written by International Business Machines Corp.
 * Copyright (C) 2001 Free Software Foundation
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU  Lesser General Public License as published by
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
/*************************************************************************/
/*   MODULE_NAME:mpexp.c                                                 */
/*                                                                       */
/*   FUNCTIONS: mpexp                                                    */
/*                                                                       */
/*   FILES NEEDED: mpa.h endian.h mpexp.h                                */
/*                 mpa.c                                                 */
/*                                                                       */
/* Multi-Precision exponential function subroutine                       */
/*   (  for p >= 4, 2**(-55) <= abs(x) <= 1024     ).                    */
/*************************************************************************/

#include "endian.h"
#include "mpa.h"
#include "mpa2.h"
#include "mpexp.h"

/* Multi-Precision exponential function subroutine (for p >= 4,          */
/* 2**(-55) <= abs(x) <= 1024).                                          */
void __mpexp(mp_no *x, mp_no *y, int p) {

  int i,j,k,m,m1,m2,n;
  double a,b;
  static const int np[33] = {0,0,0,0,3,3,4,4,5,4,4,5,5,5,6,6,6,6,6,6,
                             6,6,6,6,7,7,7,7,8,8,8,8,8};
  static const int m1p[33]= {0,0,0,0,17,23,23,28,27,38,42,39,43,47,43,47,50,54,
                               57,60,64,67,71,74,68,71,74,77,70,73,76,78,81};
  static const int m1np[7][18] = {
                 { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                 { 0, 0, 0, 0,36,48,60,72, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                 { 0, 0, 0, 0,24,32,40,48,56,64,72, 0, 0, 0, 0, 0, 0, 0},
                 { 0, 0, 0, 0,17,23,29,35,41,47,53,59,65, 0, 0, 0, 0, 0},
                 { 0, 0, 0, 0, 0, 0,23,28,33,38,42,47,52,57,62,66, 0, 0},
                 { 0, 0, 0, 0, 0, 0, 0, 0,27, 0, 0,39,43,47,51,55,59,63},
                 { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,43,47,50,54}};
  mp_no mpone = {0,{0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                    0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                    0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0}};
  mp_no mpk   = {0,{0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                    0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                    0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0}};
  mp_no mps,mpak,mpt1,mpt2;

  /* Choose m,n and compute a=2**(-m) */
  n = np[p];    m1 = m1p[p];    a = twomm1[p].d;
  for (i=0; i<EX; i++)  a *= RADIXI;
  for (   ; i>EX; i--)  a *= RADIX;
  b = X[1]*RADIXI;   m2 = 24*EX;
  for (; b<HALF; m2--)  { a *= TWO;   b *= TWO; }
  if (b == HALF) {
    for (i=2; i<=p; i++) { if (X[i]!=ZERO)  break; }
    if (i==p+1)  { m2--;  a *= TWO; }
  }
  if ((m=m1+m2) <= 0) {
    m=0;  a=ONE;
    for (i=n-1; i>0; i--,n--) { if (m1np[i][p]+m2>0)  break; }
  }

  /* Compute s=x*2**(-m). Put result in mps */
  __dbl_mp(a,&mpt1,p);
  __mul(x,&mpt1,&mps,p);

  /* Evaluate the polynomial. Put result in mpt2 */
  mpone.e=1;   mpone.d[0]=ONE;   mpone.d[1]=ONE;
  mpk.e = 1;   mpk.d[0] = ONE;   mpk.d[1]=nn[n].d;
  __dvd(&mps,&mpk,&mpt1,p);
  __add(&mpone,&mpt1,&mpak,p);
  for (k=n-1; k>1; k--) {
    __mul(&mps,&mpak,&mpt1,p);
    mpk.d[1]=nn[k].d;
    __dvd(&mpt1,&mpk,&mpt2,p);
    __add(&mpone,&mpt2,&mpak,p);
  }
  __mul(&mps,&mpak,&mpt1,p);
  __add(&mpone,&mpt1,&mpt2,p);

  /* Raise polynomial value to the power of 2**m. Put result in y */
  for (k=0,j=0; k<m; ) {
    __mul(&mpt2,&mpt2,&mpt1,p);  k++;
    if (k==m)  { j=1;  break; }
    __mul(&mpt1,&mpt1,&mpt2,p);  k++;
  }
  if (j)  __cpy(&mpt1,y,p);
  else    __cpy(&mpt2,y,p);
  return;
}
