/* Software floating-point machine description for SuperH.

Copyright (C) 2024 Free Software Foundation, Inc.

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

#define _FP_NANFRAC_B	_FP_QNANBIT_B
#define _FP_NANFRAC_H	_FP_QNANBIT_H
#define _FP_NANFRAC_S	_FP_QNANBIT_S
#define _FP_NANFRAC_D	_FP_QNANBIT_D, 0
#define _FP_NANFRAC_Q	_FP_QNANBIT_Q, 0, 0, 0

/* The type of the result of a floating point comparison.  This must
   match __libgcc_cmp_return__ in GCC for the target.  */
typedef int __gcc_CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));
#define CMPtype __gcc_CMPtype

#define _FP_NANSIGN_B	0
#define _FP_NANSIGN_H	0
#define _FP_NANSIGN_S	0
#define _FP_NANSIGN_D	0
#define _FP_NANSIGN_Q	0

#define _FP_KEEPNANFRACP 0
#define _FP_QNANNEGATEDP 0

#define _FP_CHOOSENAN(fs, wc, R, X, Y, OP)  \
  do {                      \
    R##_s = _FP_NANSIGN_##fs;           \
    _FP_FRAC_SET_##wc(R,_FP_NANFRAC_##fs);  \
    R##_c = FP_CLS_NAN;             \
  } while (0)

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
