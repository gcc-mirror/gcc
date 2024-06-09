/* Integer arithmetic support for Lattice Mico32.
   Contributed by Jon Beniston <jon@beniston.com> 
   
   Copyright (C) 2009-2024 Free Software Foundation, Inc.

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
   <http://www.gnu.org/licenses/>. */

#ifndef LIBGCC_LM32_H
#define LIBGCC_LM32_H 

/* Types.  */

typedef unsigned char UQItype __attribute__ ((mode (QI)));
typedef long SItype __attribute__ ((mode (SI)));
typedef unsigned long USItype __attribute__ ((mode (SI)));

/* Prototypes.  */

USItype __mulsi3 (USItype a, USItype b);
USItype __udivmodsi4 (USItype num, USItype den, int modwanted);
SItype __divsi3 (SItype a, SItype b);
SItype __modsi3 (SItype a, SItype b);
USItype __udivsi3 (USItype a, USItype b);
USItype __umodsi3 (USItype a, USItype b);

#endif /* LIBGCC_LM32_H */
