/* Software floating-point machine description for SuperH.

Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

#define _FP_W_TYPE_SIZE	32
#define _FP_W_TYPE	unsigned long
#define _FP_WS_TYPE	signed long
#define _FP_I_TYPE	long

#define _FP_MUL_MEAT_S(R,X,Y) \
  _FP_MUL_MEAT_1_wide(_FP_WFRACBITS_S,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_D(R,X,Y) \
  _FP_MUL_MEAT_2_wide(_FP_WFRACBITS_D,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_Q(R,X,Y) \
  _FP_MUL_MEAT_4_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_S(R,X,Y)	_FP_DIV_MEAT_1_udiv_norm(S,R,X,Y)
#define _FP_DIV_MEAT_D(R,X,Y)	_FP_DIV_MEAT_2_udiv(D,R,X,Y)
#define _FP_DIV_MEAT_Q(R,X,Y)	_FP_DIV_MEAT_4_udiv(Q,R,X,Y)

#define _FP_NANFRAC_B	(_FP_QNANBIT_B - 1)
#define _FP_NANFRAC_H	(_FP_QNANBIT_H - 1)
#define _FP_NANFRAC_S	(_FP_QNANBIT_S - 1)
#define _FP_NANFRAC_D	(_FP_QNANBIT_D - 1), -1
#define _FP_NANFRAC_Q	(_FP_QNANBIT_Q - 1), -1, -1, -1

/* The type of the result of a floating point comparison.  This must
   match __libgcc_cmp_return__ in GCC for the target.  */
typedef int __gcc_CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));
#define CMPtype __gcc_CMPtype

#define _FP_NANSIGN_B	0
#define _FP_NANSIGN_H	0
#define _FP_NANSIGN_S	0
#define _FP_NANSIGN_D	0
#define _FP_NANSIGN_Q	0

#define _FP_KEEPNANFRACP 1
#define _FP_QNANNEGATEDP 1

/* X is chosen unless one of the NaNs is sNaN.  */
# define _FP_CHOOSENAN(fs, wc, R, X, Y, OP)			\
  do {								\
    if ((_FP_FRAC_HIGH_RAW_##fs(X) |				\
	 _FP_FRAC_HIGH_RAW_##fs(Y)) & _FP_QNANBIT_##fs)		\
      {								\
	R##_s = _FP_NANSIGN_##fs;				\
	_FP_FRAC_SET_##wc(R,_FP_NANFRAC_##fs);			\
      }								\
    else							\
      {								\
	R##_s = X##_s;						\
	_FP_FRAC_COPY_##wc(R,X);				\
      }								\
    R##_c = FP_CLS_NAN;						\
  } while (0)

#ifdef __SH_FPU_ANY__
#define _FPU_GETCW(fpscr) fpscr = __builtin_sh_get_fpscr ()
#define _FPU_SETCW(fpscr) __builtin_sh_set_fpscr (fpscr)
#define FP_EX_ENABLE_SHIFT 5
#define FP_EX_CAUSE_SHIFT	10

#define	FP_EX_INVALID	0x0040
#define	FP_EX_DIVZERO	0x0020
#if defined (__SH2E__)
#define	FP_EX_ALL	(FP_EX_DIVZERO | FP_EX_INVALID)
#else
#define	FP_EX_OVERFLOW	0x0010
#define	FP_EX_UNDERFLOW	0x0008
#define	FP_EX_INEXACT	0x0004
#define	FP_EX_ALL	(FP_EX_DIVZERO | FP_EX_INEXACT | \
    FP_EX_INVALID | FP_EX_OVERFLOW | FP_EX_UNDERFLOW)
#endif
#define _FP_DECL_EX \
  unsigned int _fcsr __attribute__ ((unused)) = FP_RND_NEAREST
/* Rounding modes.  */
#define	FP_RND_NEAREST  0x0
#define	FP_RND_ZERO     0x1
/* Placeholder, hardware does not have PINF/MINF modes.  */
#define FP_RND_PINF     0x2
#define FP_RND_MINF     0x3
#define FP_RND_MASK     3

#define FP_INIT_ROUNDMODE _FPU_GETCW (_fcsr)
#define FP_ROUNDMODE (_fcsr & FP_RND_MASK)
#define FP_TRAPPING_EXCEPTIONS ((_fcsr >> FP_EX_ENABLE_SHIFT) & FP_EX_ALL)
#define FP_HANDLE_EXCEPTIONS				\
  do {							\
    _fcsr &= ~(FP_EX_ALL << FP_EX_CAUSE_SHIFT);		\
    _fcsr |= _fex | (_fex << FP_EX_CAUSE_SHIFT);	\
    _FPU_SETCW (_fcsr);			\
  } while (0)
#else
#define FP_EX_INVALID (1 << 4)
#define FP_EX_DIVZERO (1 << 3)
#if !defined (__SH2E__)
#define FP_EX_OVERFLOW (1 << 2)
#define FP_EX_UNDERFLOW (1 << 1)
#define FP_EX_INEXACT (1 << 0)
#endif
#endif

#define _FP_TININESS_AFTER_ROUNDING 1

#define __LITTLE_ENDIAN 1234
#define __BIG_ENDIAN    4321

#if defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
#define __BYTE_ORDER __BIG_ENDIAN
#else
#define __BYTE_ORDER __LITTLE_ENDIAN
#endif

/* Define ALIASNAME as a strong alias for NAME.  */
# define strong_alias(name, aliasname) _strong_alias(name, aliasname)
# define _strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));
