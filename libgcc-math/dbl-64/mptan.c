
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
/**********************************************************************/
/* MODULE_NAME:mptan.c                                                */
/*                                                                    */
/* FUNCTION: mptan                                                    */
/*                                                                    */
/* FILES NEEDED: endian.h  mpa.h                                      */
/*               mpa.c  sincos32.c branred.c                          */
/*                                                                    */
/* Multi-Precision tan() function subroutine, for p=32.  It is based  */
/* on the routines mpranred() and c32(). mpranred() performs range    */
/* reduction of a double number x into a multiple precision number    */
/* y, such that y=x-n*pi/2, abs(y)<pi/4, n=0,+-1,+-2,....  c32()      */
/* computes both sin(y), cos(y).  tan(x) is either sin(y)/cos(y)      */
/* or -cos(y)/sin(y). The precision of the result is of about 559     */
/* significant bits.                                                  */
/*                                                                    */
/**********************************************************************/
#include "endian.h"
#include "mpa.h"

int __mpranred(double, mp_no *, int);
void __c32(mp_no *, mp_no *, mp_no *, int);

void __mptan(double x, mp_no *mpy, int p) {

  static const double Mone = -1.0;

  int n;
  mp_no mpw, mpc, mps;

  n = __mpranred(x, &mpw, p) & 0x00000001; /* negative or positive result */
  __c32(&mpw, &mpc, &mps, p);              /* computing sin(x) and cos(x) */
  if (n)                     /* second or fourth quarter of unit circle */
  { __dvd(&mpc,&mps,mpy,p);
    mpy->d[0] *= Mone;
  }                          /* tan is negative in this area */
  else  __dvd(&mps,&mpc,mpy,p);

  return;
}
