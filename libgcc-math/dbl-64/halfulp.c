/*
 * IBM Accurate Mathematical Library
 * written by International Business Machines Corp.
 * Copyright (C) 2001, 2005 Free Software Foundation
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
/*                                                                      */
/* MODULE_NAME:halfulp.c                                                */
/*                                                                      */
/*  FUNCTIONS:halfulp                                                   */
/*  FILES NEEDED: mydefs.h dla.h endian.h                               */
/*                uroot.c                                               */
/*                                                                      */
/*Routine halfulp(double x, double y) computes x^y where result does    */
/*not need rounding. If the result is closer to 0 than can be           */
/*represented it returns 0.                                             */
/*     In the following cases the function does not compute anything    */
/*and returns a negative number:                                        */
/*1. if the result needs rounding,                                      */
/*2. if y is outside the interval [0,  2^20-1],                         */
/*3. if x can be represented by  x=2**n for some integer n.             */
/************************************************************************/

#include "endian.h"
#include "mydefs.h"
#include "dla.h"
#include "math_private.h"

double __ieee754_sqrt(double x);

static const int4 tab54[32] = {
   262143, 11585, 1782, 511, 210, 107, 63, 42,
       30,    22,   17,  14,  12,  10,  9,  7,
        7,     6,    5,   5,   5,   4,  4,  4,
        3,     3,    3,   3,   3,   3,  3,  3 };


double __halfulp(double x, double y)
{
  mynumber v;
  double z,u,uu,j1,j2,j3,j4,j5;
  int4 k,l,m,n;
  if (y <= 0) {               /*if power is negative or zero */
    v.x = y;
    if (v.i[LOW_HALF] != 0) return -10.0;
    v.x = x;
    if (v.i[LOW_HALF] != 0) return -10.0;
    if ((v.i[HIGH_HALF]&0x000fffff) != 0) return -10;   /* if x =2 ^ n */
    k = ((v.i[HIGH_HALF]&0x7fffffff)>>20)-1023;         /* find this n */
    z = (double) k;
    return (z*y == -1075.0)?0: -10.0;
  }
                              /* if y > 0  */
  v.x = y;
    if (v.i[LOW_HALF] != 0) return -10.0;

  v.x=x;
                              /*  case where x = 2**n for some integer n */
  if (((v.i[HIGH_HALF]&0x000fffff)|v.i[LOW_HALF]) == 0) {
    k=(v.i[HIGH_HALF]>>20)-1023;
    return (((double) k)*y == -1075.0)?0:-10.0;
  }

  v.x = y;
  k = v.i[HIGH_HALF];
  m = k<<12;
  l = 0;
  while (m)
    {m = m<<1; l++; }
  n = (k&0x000fffff)|0x00100000;
  n = n>>(20-l);                       /*   n is the odd integer of y    */
  k = ((k>>20) -1023)-l;               /*   y = n*2**k                   */
  if (k>5) return -10.0;
  if (k>0) for (;k>0;k--) n *= 2;
  if (n > 34) return -10.0;
  k = -k;
  if (k>5) return -10.0;

                            /*   now treat x        */
  while (k>0) {
    z = __ieee754_sqrt(x);
    EMULV(z,z,u,uu,j1,j2,j3,j4,j5);
    if (((u-x)+uu) != 0) break;
    x = z;
    k--;
 }
  if (k) return -10.0;

  /* it is impossible that n == 2,  so the mantissa of x must be short  */

  v.x = x;
  if (v.i[LOW_HALF]) return -10.0;
  k = v.i[HIGH_HALF];
  m = k<<12;
  l = 0;
  while (m) {m = m<<1; l++; }
  m = (k&0x000fffff)|0x00100000;
  m = m>>(20-l);                       /*   m is the odd integer of x    */

            /*   now check whether the length of m**n is at most 54 bits */

  if  (m > tab54[n-3]) return -10.0;

             /* yes, it is - now compute x**n by simple multiplications  */

  u = x;
  for (k=1;k<n;k++) u = u*x;
  return u;
}
