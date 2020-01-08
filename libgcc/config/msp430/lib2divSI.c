/* SI mode divide routines for libgcc for MSP430
   Copyright (C) 2012-2020 Free Software Foundation, Inc.
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

typedef          int  sint32_type   __attribute__ ((mode (SI)));
typedef unsigned int  uint32_type   __attribute__ ((mode (SI)));
typedef          int  sint16_type   __attribute__ ((mode (HI)));
typedef unsigned int  uint16_type   __attribute__ ((mode (HI)));
typedef          int  sint08_type   __attribute__ ((mode (QI)));
typedef unsigned int  uint08_type   __attribute__ ((mode (QI)));
typedef int           word_type     __attribute__ ((mode (__word__)));

#define C3B(a,b,c) a##b##c
#define C3(a,b,c) C3B(a,b,c)

#define UINT_TYPE	uint32_type
#define SINT_TYPE	sint32_type
#define BITS_MINUS_1	31
#define NAME_MODE	si

#include "msp430-divmod.h"

/* ---------------------------------------------------------------------*/

/* There is a typo in the MSP430 ABI document.  It calls the unsigned
   long integer division function __mspabi_divlu when it should be
   __mspabi_divul.  Likewise the unsigned long long integer division
   function is called __mspabi_divllu when it should be __mspabi_divull.

   Earlier versions of this toolchain used generate the ABI compliant
   names, so in order to support object files built with those tools
   we provide stub functions that call the correct routines.  */

asm (".global __mspabi_divlu\n\
         .set __mspabi_divlu, __mspabi_divul");

/* We cannot use the same trick for __mspabi_divllu as that is defined
   in a different file.  Instead we create a stub here.  The cost of
   executing the branch instruction will be trivial compared to the
   cost of executing a long long division.  */

#ifdef __MSP430X_LARGE__
asm (".global __mspabi_divllu\n\
      __mspabi_divllu:\n\
           BRA #__mspabi_divull");
#else
asm (".global __mspabi_divllu\n\
      __mspabi_divllu:\n\
           BR #__mspabi_divull");
#endif
