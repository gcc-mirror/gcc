/* softfp machine description for PA-RISC.
   Copyright (C) 2009-2025 Free Software Foundation, Inc.

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

#ifdef __LP64__
#define _FP_W_TYPE_SIZE		64
#define _FP_W_TYPE		unsigned long
#define _FP_WS_TYPE		signed long
#define _FP_I_TYPE		long

typedef int TItype __attribute__ ((mode (TI)));
typedef unsigned int UTItype __attribute__ ((mode (TI)));
#define TI_BITS (__CHAR_BIT__ * (int) sizeof (TItype))

#define _FP_MUL_MEAT_S(R,X,Y)				\
  _FP_MUL_MEAT_1_wide(_FP_WFRACBITS_S,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_D(R,X,Y)				\
  _FP_MUL_MEAT_1_wide(_FP_WFRACBITS_D,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_Q(R,X,Y)				\
  _FP_MUL_MEAT_2_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_S(R,X,Y)	_FP_DIV_MEAT_1_udiv_norm(S,R,X,Y)
#define _FP_DIV_MEAT_D(R,X,Y)	_FP_DIV_MEAT_1_udiv_norm(D,R,X,Y)
#define _FP_DIV_MEAT_Q(R,X,Y)	_FP_DIV_MEAT_2_udiv(Q,R,X,Y)

#define _FP_NANFRAC_S		(_FP_QNANBIT_S - 1)
#define _FP_NANFRAC_D		(_FP_QNANBIT_D - 1)
#define _FP_NANFRAC_Q		(_FP_QNANBIT_Q - 1), -1
#else
#define _FP_W_TYPE_SIZE		32
#define _FP_W_TYPE		unsigned int
#define _FP_WS_TYPE		signed int
#define _FP_I_TYPE		int

#define _FP_MUL_MEAT_S(R,X,Y)				\
  _FP_MUL_MEAT_1_wide(_FP_WFRACBITS_S,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_D(R,X,Y)				\
  _FP_MUL_MEAT_2_wide(_FP_WFRACBITS_D,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_Q(R,X,Y)				\
  _FP_MUL_MEAT_4_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_S(R,X,Y)	_FP_DIV_MEAT_1_udiv_norm(S,R,X,Y)
#define _FP_DIV_MEAT_D(R,X,Y)	_FP_DIV_MEAT_2_udiv(D,R,X,Y)
#define _FP_DIV_MEAT_Q(R,X,Y)	_FP_DIV_MEAT_4_udiv(Q,R,X,Y)

#define _FP_NANFRAC_S		(_FP_QNANBIT_S - 1)
#define _FP_NANFRAC_D		(_FP_QNANBIT_D - 1), -1
#define _FP_NANFRAC_Q		(_FP_QNANBIT_Q - 1), -1, -1, -1
#endif

/* The type of the result of a floating point comparison.  This must
   match __libgcc_cmp_return__ in GCC for the target.  */
typedef int __gcc_CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));
#define CMPtype __gcc_CMPtype

#define _FP_NANSIGN_S		0
#define _FP_NANSIGN_D		0
#define _FP_NANSIGN_Q		0

#define _FP_KEEPNANFRACP	1
#define _FP_QNANNEGATEDP	1

/* Comment from glibc: */
/* From my experiments it seems X is chosen unless one of the
   NaNs is sNaN,  in which case the result is NANSIGN/NANFRAC.  */
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

#define FP_RND_NEAREST		(0 << 9)
#define FP_RND_ZERO		(1 << 9)
#define FP_RND_PINF		(2 << 9)
#define FP_RND_MINF		(3 << 9)
#define FP_RND_MASK		(3 << 9)

#define FP_EX_INEXACT		0x01
#define FP_EX_UNDERFLOW		0x02
#define FP_EX_OVERFLOW		0x04
#define FP_EX_DIVZERO		0x08
#define FP_EX_INVALID		0x10
#define FP_EX_ALL		0x1F
#define FP_EX_SHIFT		27

#define _FP_TININESS_AFTER_ROUNDING 1

#define _FP_DECL_EX \
  unsigned int _fcsr __attribute__ ((unused)) = FP_RND_NEAREST

/* Get floating-point status.  */
#define _FPU_GETCW(cw)						\
({								\
  union { unsigned long long __fpreg;				\
	  unsigned int __halfreg[2]; } __fullfp;		\
  /* Get the current status word. */				\
  __asm__ ("fstd %%fr0,0(%1)\n\t"				\
	   "fldd 0(%1),%%fr0\n\t"				\
	    : "=m" (__fullfp.__fpreg)				\
	    : "r" (&__fullfp.__fpreg)				\
	    : "%r0");						\
  cw = __fullfp.__halfreg[0];					\
})

#define FP_INIT_ROUNDMODE _FPU_GETCW(_fcsr)
#define FP_ROUNDMODE (_fcsr & FP_RND_MASK)
#define FP_TRAPPING_EXCEPTIONS (_fcsr & FP_EX_ALL)

void __sfp_handle_exceptions (int);

#define FP_HANDLE_EXCEPTIONS				\
  do {							\
    if (__builtin_expect (_fex, 0))			\
      __sfp_handle_exceptions (_fex);			\
  } while (0)

#define	__LITTLE_ENDIAN	1234
#define	__BIG_ENDIAN	4321

#define __BYTE_ORDER __BIG_ENDIAN

/* Define ALIASNAME as a strong alias for NAME.  */
# define strong_alias(name, aliasname) _strong_alias(name, aliasname)
# define _strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));
