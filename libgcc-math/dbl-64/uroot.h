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
/* MODULE_NAME:uroot.h                                            */
/*                                                                */
/* common data and variables prototype and definition             */
/******************************************************************/

#ifndef UROOT_H
#define UROOT_H

#ifdef BIG_ENDI
 static const  mynumber
/**/           t512 = {{0x5ff00000, 0x00000000 }},  /* 2^512  */
/**/          tm256 = {{0x2ff00000, 0x00000000 }};  /* 2^-256 */

#else
#ifdef LITTLE_ENDI
 static const  mynumber
/**/           t512 = {{0x00000000, 0x5ff00000 }}, /* 2^512  */
/**/          tm256 = {{0x00000000, 0x2ff00000 }}; /* 2^-256 */
#endif
#endif

#endif
