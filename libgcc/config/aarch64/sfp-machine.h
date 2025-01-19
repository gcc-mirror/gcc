/* Machine description for AArch64 architecture.
   Copyright (C) 2009-2025 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#define _FP_W_TYPE_SIZE		64
#define _FP_W_TYPE		unsigned long long
#define _FP_WS_TYPE		signed long long
#define _FP_I_TYPE		long long

typedef int TItype __attribute__ ((mode (TI)));
typedef unsigned int UTItype __attribute__ ((mode (TI)));
#define TI_BITS (__CHAR_BIT__ * (int)sizeof(TItype))

/* The type of the result of a floating point comparison.  This must
   match __libgcc_cmp_return__ in GCC for the target.  */
typedef int __gcc_CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));
#define CMPtype __gcc_CMPtype

#define _FP_MUL_MEAT_Q(R,X,Y)				\
  _FP_MUL_MEAT_2_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_Q(R,X,Y)	_FP_DIV_MEAT_2_udiv(Q,R,X,Y)

#define _FP_NANFRAC_H		((_FP_QNANBIT_H << 1) - 1)
#define _FP_NANFRAC_B		((_FP_QNANBIT_B << 1) - 1)
#define _FP_NANFRAC_S		((_FP_QNANBIT_S << 1) - 1)
#define _FP_NANFRAC_D		((_FP_QNANBIT_D << 1) - 1)
#define _FP_NANFRAC_Q		((_FP_QNANBIT_Q << 1) - 1), -1
#define _FP_NANSIGN_H		0
#define _FP_NANSIGN_B		0
#define _FP_NANSIGN_S		0
#define _FP_NANSIGN_D		0
#define _FP_NANSIGN_Q		0

#define _FP_KEEPNANFRACP 1
#define _FP_QNANNEGATEDP 0

/* This appears to be in line with the VFP conventions in the v7-a
   ARM-ARM. Need to check with the v8 version.  */
#define _FP_CHOOSENAN(fs, wc, R, X, Y, OP)			\
  do {								\
    if ((_FP_FRAC_HIGH_RAW_##fs(X) & _FP_QNANBIT_##fs)		\
	&& !(_FP_FRAC_HIGH_RAW_##fs(Y) & _FP_QNANBIT_##fs))	\
      {								\
	R##_s = Y##_s;						\
	_FP_FRAC_COPY_##wc(R,Y);				\
      }								\
    else							\
      {								\
	R##_s = X##_s;						\
	_FP_FRAC_COPY_##wc(R,X);				\
      }								\
    R##_c = FP_CLS_NAN;						\
  } while (0)

#define FP_EX_INVALID	0x01
#define FP_EX_DIVZERO	0x02
#define FP_EX_OVERFLOW	0x04
#define FP_EX_UNDERFLOW	0x08
#define FP_EX_INEXACT	0x10
#define FP_EX_SHIFT 8
#define FP_EX_ALL \
	(FP_EX_INVALID | FP_EX_DIVZERO | FP_EX_OVERFLOW | FP_EX_UNDERFLOW \
	 | FP_EX_INEXACT)

#define _FP_TININESS_AFTER_ROUNDING 0

void __sfp_handle_exceptions (int);

#define FP_HANDLE_EXCEPTIONS			\
  do {						\
    if (__builtin_expect (_fex, 0))		\
      __sfp_handle_exceptions (_fex);		\
  } while (0)

#define FP_TRAPPING_EXCEPTIONS ((_fpcr >> FP_EX_SHIFT) & FP_EX_ALL)

#define FP_RND_NEAREST		0x000000
#define FP_RND_PINF		0x400000
#define FP_RND_MINF		0x800000
#define FP_RND_ZERO		0xc00000
#define FP_RND_MASK		0xc00000

#define _FP_DECL_EX \
  unsigned long int _fpcr __attribute__ ((unused)) = FP_RND_NEAREST

#define FP_INIT_ROUNDMODE			\
  do {						\
    __asm__ __volatile__ ("mrs	%0, fpcr"	\
			  : "=r" (_fpcr));	\
  } while (0)

#define FP_ROUNDMODE (_fpcr & FP_RND_MASK)

#define	__LITTLE_ENDIAN	1234
#define	__BIG_ENDIAN	4321

#if defined __AARCH64EB__
# define __BYTE_ORDER __BIG_ENDIAN
#else
# define __BYTE_ORDER __LITTLE_ENDIAN
#endif


/* Define ALIASNAME as a strong alias for NAME.  */
# define strong_alias(name, aliasname) _strong_alias(name, aliasname)
# define _strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));
