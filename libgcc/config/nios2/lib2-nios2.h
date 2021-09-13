/* Integer arithmetic support for Altera Nios II.
   
   Copyright (C) 2012-2021 Free Software Foundation, Inc.
   Contributed by Altera and Mentor Graphics, Inc.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.
   
   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
   
   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.
   
   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef LIB2_NIOS2_H
#define LIB2_NIOS2_H

/* Types.  */

typedef char QItype __attribute__ ((mode (QI)));
typedef unsigned char UQItype __attribute__ ((mode (QI)));
typedef short HItype __attribute__ ((mode (HI)));
typedef unsigned short UHItype __attribute__ ((mode (HI)));
typedef int SItype __attribute__ ((mode (SI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef int word_type __attribute__ ((mode (__word__)));

/* Exported functions.  */
extern SItype __divsi3 (SItype, SItype);
extern SItype __modsi3 (SItype, SItype);
extern SItype __udivsi3 (SItype, SItype);
extern SItype __umodsi3 (SItype, SItype);
extern HItype __divhi3 (HItype, HItype);
extern HItype __modhi3 (HItype, HItype);
extern UHItype __udivhi3 (UHItype, UHItype);
extern UHItype __umodhi3 (UHItype, UHItype);
extern SItype __mulsi3 (SItype, SItype);

#endif /* LIB2_NIOS2_H */
