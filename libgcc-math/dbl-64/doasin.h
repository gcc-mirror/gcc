
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
/*  MODULE_NAME: doasin.h                                                */
/*                                                                      */
/*                                                                      */
/* 	common data and variables definition for BIG or LITTLE ENDIAN   */
/************************************************************************/



#ifndef DOASIN_H
#define DOASIN_H

#ifdef BIG_ENDI

 static const  mynumber
/**/             c1 = {{0x3FC55555, 0x55555555}}, /*  0.16666666666666666    */
/**/            cc1 = {{0x3C655555, 0x55775389}}, /*  9.2518585419753846e-18 */
/**/             c2 = {{0x3FB33333, 0x33333333}}, /*  0.074999999999999997   */
/**/            cc2 = {{0x3C499993, 0x63F1A115}}, /*  2.7755472886508899e-18 */
/**/             c3 = {{0x3FA6DB6D, 0xB6DB6DB7}}, /*  0.044642857142857144   */
/**/            cc3 = {{0xBC320FC0, 0x3D5CF0C5}}, /* -9.7911734574147224e-19 */
/**/             c4 = {{0x3F9F1C71, 0xC71C71C5}}, /*  0.030381944444444437   */
/**/            cc4 = {{0xBC02B240, 0xFF23ED1E}}; /* -1.2669108566898312e-19 */

#else
#ifdef LITTLE_ENDI

 static const  mynumber
/**/             c1 = {{0x55555555, 0x3FC55555}}, /*  0.16666666666666666    */
/**/            cc1 = {{0x55775389, 0x3C655555}}, /*  9.2518585419753846e-18 */
/**/             c2 = {{0x33333333, 0x3FB33333}}, /*  0.074999999999999997   */
/**/            cc2 = {{0x63F1A115, 0x3C499993}}, /*  2.7755472886508899e-18 */
/**/             c3 = {{0xB6DB6DB7, 0x3FA6DB6D}}, /*  0.044642857142857144   */
/**/            cc3 = {{0x3D5CF0C5, 0xBC320FC0}}, /* -9.7911734574147224e-19 */
/**/             c4 = {{0xC71C71C5, 0x3F9F1C71}}, /*  0.030381944444444437   */
/**/            cc4 = {{0xFF23ED1E, 0xBC02B240}}; /* -1.2669108566898312e-19 */


#endif
#endif


#endif
