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

/******************************************************************/
/*                                                                */
/* MODULE_NAME:uexp.h                                             */
/*                                                                */
/* common data and variables prototype and definition             */
/******************************************************************/

#ifndef UEXP_H
#define UEXP_H

#include "mydefs.h"

const static double one = 1.0, zero = 0.0, hhuge = 1.0e300, tiny = 1.0e-300,
err_0 = 1.000014, err_1 = 0.000016;
const static int4 bigint = 0x40862002,
             badint = 0x40876000,smallint = 0x3C8fffff;
const static int4 hugeint = 0x7FFFFFFF, infint = 0x7ff00000;

#ifdef BIG_ENDI
const static mynumber  inf  = {{0x7FF00000, 0}}; /* inf   */
const static mynumber t256  = {{0x4ff00000, 0}}; /* 2^256 */

const static mynumber ln_two1  = {{0x3FE62E42, 0xFEFA3800}};/*0.69314718055989033 */
const static mynumber ln_two2  = {{0x3D2EF357, 0x93C76730}};/*5.4979230187083712e-14*/
const static mynumber log2e    = {{0x3FF71547, 0x652B82FE}};/* 1.4426950408889634 */

const static mynumber p2       = {{0x3FE00000, 0x000004DC}};/* 0.50000000000013811 */
const static mynumber p3       = {{0x3FC55555, 0x55555A0F}};/* 0.16666666666670024 */

const static mynumber three33  = {{0x42180000, 0}};         /* 25769803776 */
const static mynumber three51  = {{0x43380000, 0}};         /*  6755399441055744 */

#else
#ifdef LITTLE_ENDI
 const static mynumber  inf  = {{0, 0x7FF00000}}; /* inf   */
 const static mynumber t256  = {{0, 0x4ff00000}}; /* 2^256 */

 const static mynumber ln_two1 = {{0xFEFA3800, 0x3FE62E42}};/*0.69314718055989033 */
 const static mynumber ln_two2 = {{0x93C76730, 0x3D2EF357}};/*5.4979230187083712e-14*/
 const static mynumber log2e   = {{0x652B82FE, 0x3FF71547}};/* 1.4426950408889634 */

 const static mynumber p2      = {{0x000004DC, 0x3FE00000}};/* 0.50000000000013811 */
 const static mynumber p3      = {{0x55555A0F, 0x3FC55555}};/* 0.16666666666670024 */

 const static mynumber three33 = {{0, 0x42180000}};   /*  25769803776      */
 const static mynumber three51 = {{0, 0x43380000}};   /*  6755399441055744 */

#endif
#endif
#endif
