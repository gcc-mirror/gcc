
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
/******************************************************************/
/*                                                                */
/* MODULE_NAME:mpatan.c                                           */
/*                                                                */
/* FUNCTIONS:mpatan                                               */
/*                                                                */
/* FILES NEEDED: mpa.h endian.h mpatan.h                          */
/*               mpa.c                                            */
/*                                                                */
/* Multi-Precision Atan function subroutine, for precision p >= 4.*/
/* The relative error of the result is bounded by 34.32*r**(1-p), */
/* where r=2**24.                                                 */
/******************************************************************/

#include "endian.h"
#include "mpa.h"
void __mpsqrt(mp_no *, mp_no *, int);

void __mpatan(mp_no *x, mp_no *y, int p) {
#include "mpatan.h"

  int i,m,n;
  double dx;
  mp_no
    mpone    = {0,{0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
		0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
		0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0}},
    mptwo    = {0,{0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
		0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
		0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0}},
    mptwoim1 = {0,{0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
		0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
		0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0}};

  mp_no mps,mpsm,mpt,mpt1,mpt2,mpt3;

                      /* Choose m and initiate mpone, mptwo & mptwoim1 */
    if      (EX>0) m=7;
    else if (EX<0) m=0;
    else {
      __mp_dbl(x,&dx,p);  dx=ABS(dx);
      for (m=6; m>0; m--)
        {if (dx>xm[m].d) break;}
    }
    mpone.e    = mptwo.e    = mptwoim1.e = 1;
    mpone.d[0] = mpone.d[1] = mptwo.d[0] = mptwoim1.d[0] = ONE;
    mptwo.d[1] = TWO;

                                 /* Reduce x m times */
    __mul(x,x,&mpsm,p);
    if (m==0) __cpy(x,&mps,p);
    else {
      for (i=0; i<m; i++) {
	__add(&mpone,&mpsm,&mpt1,p);
	__mpsqrt(&mpt1,&mpt2,p);
	__add(&mpt2,&mpt2,&mpt1,p);
	__add(&mptwo,&mpsm,&mpt2,p);
	__add(&mpt1,&mpt2,&mpt3,p);
	__dvd(&mpsm,&mpt3,&mpt1,p);
	__cpy(&mpt1,&mpsm,p);
      }
      __mpsqrt(&mpsm,&mps,p);    mps.d[0] = X[0];
    }

                    /* Evaluate a truncated power series for Atan(s) */
    n=np[p];    mptwoim1.d[1] = twonm1[p].d;
    __dvd(&mpsm,&mptwoim1,&mpt,p);
    for (i=n-1; i>1; i--) {
      mptwoim1.d[1] -= TWO;
      __dvd(&mpsm,&mptwoim1,&mpt1,p);
      __mul(&mpsm,&mpt,&mpt2,p);
      __sub(&mpt1,&mpt2,&mpt,p);
    }
    __mul(&mps,&mpt,&mpt1,p);
    __sub(&mps,&mpt1,&mpt,p);

                          /* Compute Atan(x) */
    mptwoim1.d[1] = twom[m].d;
    __mul(&mptwoim1,&mpt,y,p);

  return;
}
