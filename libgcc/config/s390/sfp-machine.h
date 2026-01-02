/* Copyright (C) 2001-2026 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#if ! __s390x__
# error "soft-fp implemented for s390x only"
#endif

#define _FP_W_TYPE_SIZE         64
#define _FP_W_TYPE              unsigned long long
#define _FP_WS_TYPE             signed long long
#define _FP_I_TYPE              long long

typedef int TItype __attribute__ ((mode (TI)));
typedef unsigned int UTItype __attribute__ ((mode (TI)));
#define TI_BITS (__CHAR_BIT__ * (int) sizeof (TItype))

#define _FP_NANFRAC_H           ((_FP_QNANBIT_H << 1) - 1)
#define _FP_NANFRAC_S           ((_FP_QNANBIT_S << 1) - 1)
#define _FP_NANFRAC_D		((_FP_QNANBIT_D << 1) - 1)
#define _FP_NANFRAC_Q		((_FP_QNANBIT_Q << 1) - 1), -1

#define _FP_NANSIGN_H 		0
#define _FP_NANSIGN_S		0
#define _FP_NANSIGN_D		0
#define _FP_NANSIGN_Q		0

#define _FP_KEEPNANFRACP	1
#define _FP_QNANNEGATEDP	0

#define FP_EX_INVALID		0x01
#define FP_EX_DIVZERO		0x02
#define FP_EX_OVERFLOW		0x04
#define FP_EX_UNDERFLOW		0x08
#define FP_EX_INEXACT		0x10
#define FP_EX_ALL \
	(FP_EX_INVALID | FP_EX_DIVZERO | FP_EX_OVERFLOW \
	 | FP_EX_UNDERFLOW | FP_EX_INEXACT)

void __sfp_handle_exceptions (int);

#define FP_HANDLE_EXCEPTIONS			\
  do {						\
    if (__builtin_expect (_fex, 0))		\
      __sfp_handle_exceptions (_fex);		\
  } while (0)

#define _FP_TININESS_AFTER_ROUNDING 0

#define FP_RND_NEAREST		0x0
#define FP_RND_ZERO		0x1
#define FP_RND_PINF		0x2
#define FP_RND_MINF		0x3
#define FP_RND_MASK		0x3

#define _FP_DECL_EX \
  unsigned int _fpcr __attribute__ ((unused)) = FP_RND_NEAREST

#define FP_INIT_ROUNDMODE			\
  do {						\
    __asm__ __volatile__ ("stfpc %0"		\
			  : "=m" (_fpcr));	\
  } while (0)

#define FP_ROUNDMODE		(_fpcr & FP_RND_MASK)

#define __LITTLE_ENDIAN		1234
#define __BIG_ENDIAN		4321

#define __BYTE_ORDER __BIG_ENDIAN
