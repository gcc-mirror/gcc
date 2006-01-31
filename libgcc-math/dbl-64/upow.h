/*
 * IBM Accurate Mathematical Library
 * Written by International Business Machines Corp.
 * Copyright (C) 2001, 2002 Free Software Foundation, Inc.
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
/*                                                                */
/* MODULE_NAME:upow.h                                             */
/*                                                                */
/* common data and variables prototype and definition             */
/******************************************************************/

#ifndef UPOW_H
#define UPOW_H

#include "mydefs.h"

#ifdef BIG_ENDI
  const static mynumber
/**/ nZERO	    = {{0x80000000, 0}},	  /* -0.0          */
/**/ INF            = {{0x7ff00000, 0x00000000}}, /* INF           */
/**/ nINF           = {{0xfff00000, 0x00000000}}, /* -INF          */
/**/ NaNQ           = {{0x7ff80000, 0x00000000}}, /* NaNQ          */
/**/ sqrt_2         = {{0x3ff6a09e, 0x667f3bcc}}, /* sqrt(2)       */
/**/ ln2a           = {{0x3fe62e42, 0xfefa3800}}, /* ln(2) 43 bits */
/**/ ln2b           = {{0x3d2ef357, 0x93c76730}}, /* ln(2)-ln2a    */
/**/ bigu           = {{0x4297ffff, 0xfffffd2c}}, /* 1.5*2**42 -724*2**-10  */
/**/ bigv           = {{0x4207ffff, 0xfff8016a}}, /* 1.5*2**33-1+362*2**-19  */
/**/ t52            = {{0x43300000, 0x00000000}}, /* 2**52         */
/**/ two52e         = {{0x43300000, 0x000003ff}}; /* 2**52'        */

#else
#ifdef LITTLE_ENDI
  const static mynumber
/**/ nZERO	    = {{0, 0x80000000}},	  /* -0.0          */
/**/ INF            = {{0x00000000, 0x7ff00000}}, /* INF           */
/**/ nINF           = {{0x00000000, 0xfff00000}}, /* -INF           */
/**/ NaNQ           = {{0x00000000, 0x7ff80000}}, /* NaNQ          */
/**/ sqrt_2         = {{0x667f3bcc, 0x3ff6a09e}}, /* sqrt(2)       */
/**/ ln2a           = {{0xfefa3800, 0x3fe62e42}}, /* ln(2) 43 bits */
/**/ ln2b           = {{0x93c76730, 0x3d2ef357}}, /* ln(2)-ln2a    */
/**/ bigu           = {{0xfffffd2c, 0x4297ffff}}, /* 1.5*2**42 -724*2**-10  */
/**/ bigv           = {{0xfff8016a, 0x4207ffff}}, /* 1.5*2**33-1+362*2**-19  */
/**/ t52            = {{0x00000000, 0x43300000}}, /* 2**52         */
/**/ two52e         = {{0x000003ff, 0x43300000}}; /* 2**52'        */

#endif
#endif

const static double p2=-0.5, p3 =  3.3333333333333333333e-1, p4 = -0.25,
  q2 = -0.5, q3 = 3.3333333333331404e-01, q4 =  -2.4999999999996436e-01,
  q5 =  2.0000010500004459e-01, q6 =  -1.6666678916688004e-01,
  r3 =  3.33333333333333333372884096563030E-01,
  r4 = -2.50000000000000000213574153875908E-01,
  r5 =  1.99999999999683593814072199830603E-01,
  r6 = -1.66666666666065494878165510225378E-01,
  r7 =  1.42857517857114380606360005067609E-01,
  r8 = -1.25000449999974370683775964001702E-01,
  s3 =  0.333251953125000000e0,
 ss3 =  8.138020833333333333e-05,
  s4 = -2.500000000000000000e-01,
  s5 =  1.999999999999960937e-01,
  s6 = -1.666666666666592447e-01,
  s7 =  1.428571845238194705e-01,
  s8 = -1.250000500000149097e-01;
#endif
