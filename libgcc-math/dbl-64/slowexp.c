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
/**************************************************************************/
/*  MODULE_NAME:slowexp.c                                                 */
/*                                                                        */
/*  FUNCTION:slowexp                                                      */
/*                                                                        */
/*  FILES NEEDED:mpa.h                                                    */
/*               mpa.c mpexp.c                                            */
/*                                                                        */
/*Converting from double precision to Multi-precision and calculating     */
/* e^x                                                                    */
/**************************************************************************/
#include "mpa.h"
#include "math_private.h"

void __mpexp(mp_no *x, mp_no *y, int p);

/*Converting from double precision to Multi-precision and calculating  e^x */
double __slowexp(double x) {
  double w,z,res,eps=3.0e-26;
#if 0
  double y;
#endif
  int p;
#if 0
  int orig,i;
#endif
  mp_no mpx, mpy, mpz,mpw,mpeps,mpcor;

  p=6;
  __dbl_mp(x,&mpx,p); /* Convert a double precision number  x               */
                    /* into a multiple precision number mpx with prec. p. */
  __mpexp(&mpx, &mpy, p); /* Multi-Precision exponential function */
  __dbl_mp(eps,&mpeps,p);
  __mul(&mpeps,&mpy,&mpcor,p);
  __add(&mpy,&mpcor,&mpw,p);
  __sub(&mpy,&mpcor,&mpz,p);
  __mp_dbl(&mpw, &w, p);
  __mp_dbl(&mpz, &z, p);
  if (w == z) return w;
  else  {                   /* if calculating is not exactly   */
    p = 32;
    __dbl_mp(x,&mpx,p);
    __mpexp(&mpx, &mpy, p);
    __mp_dbl(&mpy, &res, p);
    return res;
  }
}
