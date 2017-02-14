/* libgcc routines for MSP430
   Copyright (C) 2005-2017 Free Software Foundation, Inc.
   Contributed by Red Hat.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

typedef unsigned int  uint32_type   __attribute__ ((mode (SI)));
typedef unsigned int  uint16_type   __attribute__ ((mode (HI)));
typedef unsigned int  uint08_type   __attribute__ ((mode (QI)));

#define C3B(a,b,c) a##b##c
#define C3(a,b,c) C3B(a,b,c)

#if defined MUL_NONE

/* The software multiply library needs __mspabi_mpyll.  */

#undef UINT_TYPE
#undef BITS_MINUS_1
#undef NAME_MODE

#define UINT_TYPE	uint32_type
#define BITS_MINUS_1	31
#define NAME_MODE	si

#include "msp430-mul.h"

#elif defined MUL_16

signed long long
__mspabi_mpysll (signed long a, signed long b)
{
  return (signed long long) a * (signed long long) b;
}

unsigned long long
__mspabi_mpyull (unsigned long a, unsigned long b)
{
  return (unsigned long long) a * (unsigned long long) b;
}

#else

#undef UINT_TYPE
#undef BITS_MINUS_1
#undef NAME_MODE

#define UINT_TYPE	uint08_type
#define BITS_MINUS_1	7
#define NAME_MODE	qi

#include "msp430-mul.h"

#endif /* MUL_NONE */
