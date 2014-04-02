/* Soft-FP definitions for TI C6X.
   Copyright (C) 2010-2014 Free Software Foundation, Inc.

   This files is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   In addition to the permissions in the GNU Lesser General Public
   License, the Free Software Foundation gives you unlimited
   permission to link the compiled version of this file into
   combinations with other programs, and to distribute those
   combinations without any restriction coming from the use of this
   file.  (The Lesser General Public License restrictions do apply in
   other respects; for example, they cover modification of the file,
   and distribution when not linked into a combine executable.)

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with GCC; see the file COPYING.LIB.  If not see
   <http://www.gnu.org/licenses/>.  */

#define _FP_W_TYPE_SIZE		32
#define _FP_W_TYPE		unsigned long
#define _FP_WS_TYPE		signed long
#define _FP_I_TYPE		long

#define _FP_MUL_MEAT_S(R,X,Y)				\
  _FP_MUL_MEAT_1_wide(_FP_WFRACBITS_S,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_D(R,X,Y)				\
  _FP_MUL_MEAT_2_wide(_FP_WFRACBITS_D,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_Q(R,X,Y)				\
  _FP_MUL_MEAT_4_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_S(R,X,Y)	_FP_DIV_MEAT_1_loop(S,R,X,Y)
#define _FP_DIV_MEAT_D(R,X,Y)	_FP_DIV_MEAT_2_udiv(D,R,X,Y)
#define _FP_DIV_MEAT_Q(R,X,Y)	_FP_DIV_MEAT_4_udiv(Q,R,X,Y)

#define _FP_NANFRAC_H		((_FP_QNANBIT_H << 1) - 1)
#define _FP_NANFRAC_S		((_FP_QNANBIT_S << 1) - 1)
#define _FP_NANFRAC_D		((_FP_QNANBIT_D << 1) - 1), -1
#define _FP_NANFRAC_Q		((_FP_QNANBIT_Q << 1) - 1), -1, -1, -1
#define _FP_NANSIGN_H		0
#define _FP_NANSIGN_S		0
#define _FP_NANSIGN_D		0
#define _FP_NANSIGN_Q		0

#define _FP_KEEPNANFRACP 1
#define _FP_QNANNEGATEDP 0

/* Someone please check this.  */
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

#define	__LITTLE_ENDIAN	1234
#define	__BIG_ENDIAN	4321

#if defined _BIG_ENDIAN
# define __BYTE_ORDER __BIG_ENDIAN
#else
# define __BYTE_ORDER __LITTLE_ENDIAN
#endif

/* Not checked.  */
#define _FP_TININESS_AFTER_ROUNDING 0


/* Define ALIASNAME as a strong alias for NAME.  */
# define strong_alias(name, aliasname) _strong_alias(name, aliasname)
# define _strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));

/* Rename helper functions to the names specified in the C6000 ELF ABI.  */
#define __fixdfsi     __c6xabi_fixdi
#define __fixsfsi     __c6xabi_fixfi
#define __floatsidf   __c6xabi_fltid
#define __floatunsidf __c6xabi_fltud
#define __floatsisf   __c6xabi_fltif
#define __floatunsisf __c6xabi_fltuf
#define __truncdfsf2  __c6xabi_cvtdf
#define __extendsfdf2 __c6xabi_cvtfd
#define __adddf3      __c6xabi_addd
#define __subdf3      __c6xabi_subd
#define __muldf3      __c6xabi_mpyd
#define __divdf3      __c6xabi_divd
#define __negdf2      __c6xabi_negd
#define __absdf2      __c6xabi_absd
#define __addsf3      __c6xabi_addf
#define __subsf3      __c6xabi_subf
#define __mulsf3      __c6xabi_mpyf
#define __divsf3      __c6xabi_divf
#define __negsf2      __c6xabi_negf
#define __abssf2      __c6xabi_absf
#define __lesf2       __c6xabi_cmpf
#define __ledf2       __c6xabi_cmpd
#define __ltsf2       __gnu_ltsf2
#define __ltdf2       __gnu_ltdf2
#define __gesf2       __gnu_gesf2
#define __gedf2       __gnu_gedf2
#define __gtsf2       __gnu_gtsf2
#define __gtdf2       __gnu_gtdf2
#define __eqsf2       __gnu_eqsf2
#define __eqdf2       __gnu_eqdf2
#define __nesf2       __c6xabi_neqf
#define __nedf2       __c6xabi_neqd
#define __unordsf2    __c6xabi_unordf
#define __unorddf2    __c6xabi_unordd
