
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
/*  MODULE_NAME: mpatan2.c                                        */
/*                                                                */
/*  FUNCTIONS:mpatan2                                             */
/*                                                                */
/*  FILES NEEDED: mpa.h                                           */
/*                mpa.c mpatan.c mpsqrt.c                         */
/*                                                                */
/* Multi-Precision Atan2(y,x) function subroutine,                */
/* for precision p >= 4.                                          */
/* y=0 is not permitted if x<=0. No error messages are given.     */
/* The relative error of the result is bounded by 44.84*r**(1-p)  */
/* if x <= 0,  y != 0 and by 37.33*r**(1-p) if x>0. here r=2**24. */
/*                                                                */
/******************************************************************/



#include "mpa.h"

void __mpsqrt(mp_no *, mp_no *, int);
void __mpatan(mp_no *, mp_no *, int);

/* Multi-Precision Atan2(y,x) function subroutine, for p >= 4.    */
/* y=0 is not permitted if x<=0. No error messages are given.     */
void __mpatan2(mp_no *y, mp_no *x, mp_no *z, int p) {

  static const double Zero = 0.0, One = 1.0;

  mp_no mpone = {0,{0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                    0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                    0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0}};
  mp_no mpt1,mpt2,mpt3;


  if (X[0] <= Zero) {
    mpone.e = 1;                 mpone.d[0] = mpone.d[1] = One;
    __dvd(x,y,&mpt1,p);          __mul(&mpt1,&mpt1,&mpt2,p);
    if (mpt1.d[0] != Zero)       mpt1.d[0] = One;
    __add(&mpt2,&mpone,&mpt3,p); __mpsqrt(&mpt3,&mpt2,p);
    __add(&mpt1,&mpt2,&mpt3,p);  mpt3.d[0]=Y[0];
    __mpatan(&mpt3,&mpt1,p);     __add(&mpt1,&mpt1,z,p);
  }
  else
  { __dvd(y,x,&mpt1,p);
    __mpatan(&mpt1,z,p);
  }

  return;
}
