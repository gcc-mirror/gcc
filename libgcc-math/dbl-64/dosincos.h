
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

/************************************************************************/
/*  MODULE_NAME: dosincos.h                                                */
/*                                                                      */
/*                                                                      */
/* 	common data and variables definition for BIG or LITTLE ENDIAN   */
/************************************************************************/



#ifndef DOSINCOS_H
#define DOSINCOS_H


#ifdef BIG_ENDI
static const mynumber
/**/             s3 = {{0xBFC55555, 0x55555555}},/* -0.16666666666666666    */
/**/            ss3 = {{0xBC6553AA, 0xE77EE482}},/* -9.2490366677784492e-18 */
/**/             s5 = {{0x3F811111, 0x11110F15}},/*  0.008333333333332452   */
/**/            ss5 = {{0xBC21AC06, 0xDA488820}},/* -4.7899996586987931e-19 */
/**/             s7 = {{0xBF2A019F, 0x5816C78D}},/* -0.00019841261022928957 */
/**/            ss7 = {{0x3BCDCEC9, 0x6A18BF2A}},/*  1.2624077757871259e-20 */
/**/             c2 = {{0x3FE00000, 0x00000000}},/*  0.5                    */
/**/            cc2 = {{0xBA282FD8, 0x00000000}},/* -1.5264073330037701e-28 */
/**/             c4 = {{0xBFA55555, 0x55555555}},/* -0.041666666666666664   */
/**/            cc4 = {{0xBC4554BC, 0x2FFF257E}},/* -2.312711276085743e-18  */
/**/             c6 = {{0x3F56C16C, 0x16C16A96}},/*  0.0013888888888888055  */
/**/            cc6 = {{0xBBD2E846, 0xE6346F14}},/* -1.6015133010194884e-20 */
/**/             c8 = {{0xBEFA019F, 0x821D5987}},/* -2.480157866754367e-05  */
/**/            cc8 = {{0x3B7AB71E, 0x72FFE5CC}},/*  3.5357416224857556e-22 */

/**/            big = {{0x42c80000, 0x00000000}}, /* 52776558133248         */

/**/            hp0 = {{0x3FF921FB, 0x54442D18}}, /* PI / 2                 */
/**/            hp1 = {{0x3C91A626, 0x33145C07}}; /* 6.123233995736766e-17  */
#else
#ifdef LITTLE_ENDI
static const mynumber
/**/             s3 = {{0x55555555, 0xBFC55555}},/* -0.16666666666666666    */
/**/            ss3 = {{0xE77EE482, 0xBC6553AA}},/* -9.2490366677784492e-18 */
/**/             s5 = {{0x11110F15, 0x3F811111}},/*  0.008333333333332452   */
/**/            ss5 = {{0xDA488820, 0xBC21AC06}},/* -4.7899996586987931e-19 */
/**/             s7 = {{0x5816C78D, 0xBF2A019F}},/* -0.00019841261022928957 */
/**/            ss7 = {{0x6A18BF2A, 0x3BCDCEC9}},/*  1.2624077757871259e-20 */
/**/             c2 = {{0x00000000, 0x3FE00000}},/*  0.5                    */
/**/            cc2 = {{0x00000000, 0xBA282FD8}},/* -1.5264073330037701e-28 */
/**/             c4 = {{0x55555555, 0xBFA55555}},/* -0.041666666666666664   */
/**/            cc4 = {{0x2FFF257E, 0xBC4554BC}},/* -2.312711276085743e-18  */
/**/             c6 = {{0x16C16A96, 0x3F56C16C}},/*  0.0013888888888888055  */
/**/            cc6 = {{0xE6346F14, 0xBBD2E846}},/* -1.6015133010194884e-20 */
/**/             c8 = {{0x821D5987, 0xBEFA019F}},/* -2.480157866754367e-05  */
/**/            cc8 = {{0x72FFE5CC, 0x3B7AB71E}},/*  3.5357416224857556e-22 */

/**/            big = {{0x00000000, 0x42c80000}}, /* 52776558133248         */

/**/            hp0 = {{0x54442D18, 0x3FF921FB}}, /* PI / 2                 */
/**/            hp1 = {{0x33145C07, 0x3C91A626}}; /* 6.123233995736766e-17  */
#endif
#endif

#endif
