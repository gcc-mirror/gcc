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
/*************************************************************************/
/* MODULE_NAME:slowpow.c                                                 */
/*                                                                       */
/* FUNCTION:slowpow                                                      */
/*                                                                       */
/*FILES NEEDED:mpa.h                                                     */
/*             mpa.c mpexp.c mplog.c halfulp.c                           */
/*                                                                       */
/* Given two IEEE double machine numbers y,x , routine  computes the     */
/* correctly  rounded (to nearest) value of x^y. Result calculated  by   */
/* multiplication (in halfulp.c) or if result isn't accurate enough      */
/* then routine converts x and y into multi-precision doubles     and    */
/* calls to mpexp routine                                                */
/*************************************************************************/

#include "mpa.h"
#include "math_private.h"

void __mpexp(mp_no *x, mp_no *y, int p);
void __mplog(mp_no *x, mp_no *y, int p);
double ulog(double);
double __halfulp(double x,double y);

double __slowpow(double x, double y, double z) {
  double res,res1;
  mp_no mpx, mpy, mpz,mpw,mpp,mpr,mpr1;
  static const mp_no eps = {-3,{1.0,4.0}};
  int p;

  res = __halfulp(x,y);        /* halfulp() returns -10 or x^y             */
  if (res >= 0) return res;  /* if result was really computed by halfulp */
                  /*  else, if result was not really computed by halfulp */
  p = 10;         /*  p=precision   */
  __dbl_mp(x,&mpx,p);
  __dbl_mp(y,&mpy,p);
  __dbl_mp(z,&mpz,p);
  __mplog(&mpx, &mpz, p);     /* log(x) = z   */
  __mul(&mpy,&mpz,&mpw,p);    /*  y * z =w    */
  __mpexp(&mpw, &mpp, p);     /*  e^w =pp     */
  __add(&mpp,&eps,&mpr,p);    /*  pp+eps =r   */
  __mp_dbl(&mpr, &res, p);
  __sub(&mpp,&eps,&mpr1,p);   /*  pp -eps =r1 */
  __mp_dbl(&mpr1, &res1, p);  /*  converting into double precision */
  if (res == res1) return res;

  p = 32;     /* if we get here result wasn't calculated exactly, continue */
  __dbl_mp(x,&mpx,p);                          /* for more exact calculation */
  __dbl_mp(y,&mpy,p);
  __dbl_mp(z,&mpz,p);
  __mplog(&mpx, &mpz, p);   /* log(c)=z  */
  __mul(&mpy,&mpz,&mpw,p);  /* y*z =w    */
  __mpexp(&mpw, &mpp, p);   /* e^w=pp    */
  __mp_dbl(&mpp, &res, p);  /* converting into double precision */
  return res;
}
