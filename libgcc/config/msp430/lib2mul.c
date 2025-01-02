/* libgcc routines for MSP430
   Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

#if defined (MUL_NONE) || defined (MUL_16)
/* __muldi3 must be excluded from libgcc.a to prevent multiple-definition
   errors for the hwmult configurations that have their own definition.
   However, for MUL_NONE and MUL_16, the software version is still required, so
   the necessary preprocessed output from libgcc2.c to compile that
   software version of __muldi3 is below.  */
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));
typedef int SItype __attribute__ ((mode (SI)));
struct DWstruct {SItype low, high;};

typedef union
{
  struct DWstruct s;
  DItype ll;
} DWunion;

DItype __muldi3 (DItype u, DItype v);

DItype
__muldi3 (DItype u, DItype v)
{
  const DWunion uu = {.ll = u};
  const DWunion vv = {.ll = v};
  /* The next block of code is expanded from the following line:
     DWunion w = {.ll = __umulsidi3 (uu.s.low, vv.s.low)};  */
  DWunion w;
  USItype __x0, __x1, __x2, __x3;
  USItype __ul, __vl, __uh, __vh;
  __ul = ((USItype) (uu.s.low) & (((USItype) 1 << ((4 * 8) / 2)) - 1));
  __uh = ((USItype) (uu.s.low) >> ((4 * 8) / 2));
  __vl = ((USItype) (vv.s.low) & (((USItype) 1 << ((4 * 8) / 2)) - 1));
  __vh = ((USItype) (vv.s.low) >> ((4 * 8) / 2));
  __x0 = (USItype) __ul * __vl;
  __x1 = (USItype) __ul * __vh;
  __x2 = (USItype) __uh * __vl;
  __x3 = (USItype) __uh * __vh;
  __x1 += ((USItype) (__x0) >> ((4 * 8) / 2));
  __x1 += __x2;
  if (__x1 < __x2)
    __x3 += ((USItype) 1 << ((4 * 8) / 2));
  (w.s.high) = __x3 + ((USItype) (__x1) >> ((4 * 8) / 2));
  (w.s.low) = ((USItype) (__x1) & (((USItype) 1 << ((4 * 8) / 2)) - 1))
    * ((USItype) 1 << ((4 * 8) / 2))
    + ((USItype) (__x0) & (((USItype) 1 << ((4 * 8) / 2)) - 1));

  w.s.high += ((USItype) uu.s.low * (USItype) vv.s.high
	       + (USItype) uu.s.high * (USItype) vv.s.low);
  return w.ll;
}
#endif

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

/* The 16-bit multiply library needs a software version of SI->DI widening
   multiplication.  */

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
