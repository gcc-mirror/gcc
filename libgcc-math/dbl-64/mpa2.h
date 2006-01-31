
/*
 * IBM Accurate Mathematical Library
 * Written by International Business Machines Corp.
 * Copyright (C) 2001 Free Software Foundation, Inc.
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
/*                                                                        */
/* MODULE_NAME:mpa2.h                                                     */
/*                                                                        */
/*                                                                        */
/*   variables prototype and definition   according to type of processor  */
/*   types definition                                                     */
/**************************************************************************/

#ifndef MPA2_H
#define MPA2_H


#ifdef BIG_ENDI
static const number
/**/ radix          = {{0x41700000, 0x00000000} }, /* 2**24  */
/**/ radixi         = {{0x3e700000, 0x00000000} }, /* 2**-24 */
/**/ cutter         = {{0x44b00000, 0x00000000} }, /* 2**76  */
/**/ zero           = {{0x00000000, 0x00000000} }, /*  0     */
/**/ one            = {{0x3ff00000, 0x00000000} }, /*  1     */
/**/ mone           = {{0xbff00000, 0x00000000} }, /* -1     */
/**/ two            = {{0x40000000, 0x00000000} }, /*  2     */
/**/ half           = {{0x3fe00000, 0x00000000} }, /* 1/2    */
/**/ two5           = {{0x40400000, 0x00000000} }, /* 2**5   */
/**/ two10          = {{0x40900000, 0x00000000} }, /* 2**10  */
/**/ two18          = {{0x41100000, 0x00000000} }, /* 2**18  */
/**/ two19          = {{0x41200000, 0x00000000} }, /* 2**19  */
/**/ two23          = {{0x41600000, 0x00000000} }, /* 2**23  */
/**/ two52          = {{0x43300000, 0x00000000} }, /* 2**52  */
/**/ two57          = {{0x43800000, 0x00000000} }, /* 2**57  */
/**/ two71          = {{0x44600000, 0x00000000} }, /* 2**71  */
/**/ twom1032       = {{0x00000400, 0x00000000} }; /* 2**-1032 */

#else
#ifdef LITTLE_ENDI
static const number
/**/ radix          = {{0x00000000, 0x41700000} }, /* 2**24  */
/**/ radixi         = {{0x00000000, 0x3e700000} }, /* 2**-24 */
/**/ cutter         = {{0x00000000, 0x44b00000} }, /* 2**76  */
/**/ zero           = {{0x00000000, 0x00000000} }, /*  0     */
/**/ one            = {{0x00000000, 0x3ff00000} }, /*  1     */
/**/ mone           = {{0x00000000, 0xbff00000} }, /* -1     */
/**/ two            = {{0x00000000, 0x40000000} }, /*  2     */
/**/ half           = {{0x00000000, 0x3fe00000} }, /* 1/2    */
/**/ two5           = {{0x00000000, 0x40400000} }, /* 2**5   */
/**/ two10          = {{0x00000000, 0x40900000} }, /* 2**10  */
/**/ two18          = {{0x00000000, 0x41100000} }, /* 2**18  */
/**/ two19          = {{0x00000000, 0x41200000} }, /* 2**19  */
/**/ two23          = {{0x00000000, 0x41600000} }, /* 2**23  */
/**/ two52          = {{0x00000000, 0x43300000} }, /* 2**52  */
/**/ two57          = {{0x00000000, 0x43800000} }, /* 2**57  */
/**/ two71          = {{0x00000000, 0x44600000} }, /* 2**71  */
/**/ twom1032       = {{0x00000000, 0x00000400} }; /* 2**-1032 */

#endif
#endif

#define  RADIX     radix.d
#define  RADIXI    radixi.d
#define  CUTTER    cutter.d
#define  ZERO      zero.d
#define  ONE       one.d
#define  MONE      mone.d
#define  TWO       two.d
#define  HALF      half.d
#define  TWO5      two5.d
#define  TWO10     two10.d
#define  TWO18     two18.d
#define  TWO19     two19.d
#define  TWO23     two23.d
#define  TWO52     two52.d
#define  TWO57     two57.d
#define  TWO71     two71.d
#define  TWOM1032  twom1032.d


#endif
