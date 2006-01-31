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
/*  MODULE_NAME: dosincos.h                                             */
/*                                                                      */
/*                                                                      */
/* 	common data and variables definition for BIG or LITTLE ENDIAN   */
/************************************************************************/

#ifndef USNCS_H
#define USNCS_H

#ifdef BIG_ENDI
static const mynumber

/**/          NAN = {{0x7ff80000, 0x00000000 }}, /*  NaN                     */
/**/           s1 = {{0xBFC55555, 0x55555555 }}, /* -0.16666666666666666     */
/**/           s2 = {{0x3F811111, 0x11110ECE }}, /*  0.0083333333333323288   */
/**/           s3 = {{0xBF2A01A0, 0x19DB08B8 }}, /* -0.00019841269834414642  */
/**/           s4 = {{0x3EC71DE2, 0x7B9A7ED9 }}, /*  2.755729806860771e-06   */
/**/           s5 = {{0xBE5ADDFF, 0xC2FCDF59 }}, /* -2.5022014848318398e-08  */
/**/           aa = {{0xBFC55580, 0x00000000 }}, /* -0.1666717529296875      */
/**/           bb = {{0x3ED55555, 0x55556E24 }}, /*  5.0862630208387126e-06  */
/**/          big = {{0x42c80000, 0x00000000 }}, /*  52776558133248          */
/**/          hp0 = {{0x3FF921FB, 0x54442D18 }}, /*  1.5707963267948966      */
/**/          hp1 = {{0x3C91A626, 0x33145C07 }}, /*  6.123233995736766e-17   */
/**/          mp1 = {{0x3FF921FB, 0x58000000 }}, /*  1.5707963407039642      */
/**/          mp2 = {{0xBE4DDE97, 0x3C000000 }}, /* -1.3909067564377153e-08  */
/**/          mp3 = {{0xBC8CB3B3, 0x99D747F2 }}, /* -4.9789962505147994e-17  */
/**/          pp3 = {{0xBC8CB3B3, 0x98000000 }}, /* -4.9789962314799099e-17  */
/**/          pp4 = {{0xbacd747f, 0x23e32ed7 }}, /* -1.9034889620193266e-25  */
/**/        hpinv = {{0x3FE45F30, 0x6DC9C883 }}, /*  0.63661977236758138     */
/**/        toint = {{0x43380000, 0x00000000 }}; /*  6755399441055744        */

#else
#ifdef LITTLE_ENDI
static const mynumber

/**/          NAN = {{0x00000000, 0x7ff80000 }},/*  NaN                     */
/**/           s1 = {{0x55555555, 0xBFC55555 }},/* -0.16666666666666666     */
/**/           s2 = {{0x11110ECE, 0x3F811111 }},/*  0.0083333333333323288   */
/**/           s3 = {{0x19DB08B8, 0xBF2A01A0 }},/* -0.00019841269834414642  */
/**/           s4 = {{0x7B9A7ED9, 0x3EC71DE2 }},/*  2.755729806860771e-06   */
/**/           s5 = {{0xC2FCDF59, 0xBE5ADDFF }},/* -2.5022014848318398e-08  */
/**/           aa = {{0x00000000, 0xBFC55580 }},/* -0.1666717529296875      */
/**/           bb = {{0x55556E24, 0x3ED55555 }},/*  5.0862630208387126e-06  */
/**/          big = {{0x00000000, 0x42c80000 }},/*  52776558133248          */
/**/          hp0 = {{0x54442D18, 0x3FF921FB }},/*  1.5707963267948966      */
/**/          hp1 = {{0x33145C07, 0x3C91A626 }},/*  6.123233995736766e-17   */
/**/          mp1 = {{0x58000000, 0x3FF921FB }},/*  1.5707963407039642      */
/**/          mp2 = {{0x3C000000, 0xBE4DDE97 }},/* -1.3909067564377153e-08  */
/**/          mp3 = {{0x99D747F2, 0xBC8CB3B3 }},/* -4.9789962505147994e-17  */
/**/          pp3 = {{0x98000000, 0xBC8CB3B3 }},/* -4.9789962314799099e-17  */
/**/          pp4 = {{0x23e32ed7, 0xbacd747f }},/* -1.9034889620193266e-25  */
/**/        hpinv = {{0x6DC9C883, 0x3FE45F30 }},/*  0.63661977236758138     */
/**/        toint = {{0x00000000, 0x43380000 }};/*  6755399441055744        */


#endif
#endif

#endif
