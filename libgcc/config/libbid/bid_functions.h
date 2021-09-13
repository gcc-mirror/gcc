/* Copyright (C) 2007-2021 Free Software Foundation, Inc.

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

#ifndef _BID_FUNCTIONS_H
#define _BID_FUNCTIONS_H

#ifdef IN_LIBGCC2
// When we are built as the part of the gcc runtime library, libgcc,
// we will use gcc types defined in bid_gcc_intrinsics.h.
#include "bid_gcc_intrinsics.h"

#define ALIGN(n) __attribute__ ((aligned(n)))
#else
typedef char SINT8;
typedef unsigned char UINT8;
typedef unsigned UINT32;
typedef signed SINT32;

#ifdef __GNUC__
#define __int64 long long
#endif

#if __GNUC__ || defined LINUX || defined SUNOS
typedef unsigned long long UINT64;
typedef signed long long SINT64;
#else
typedef unsigned __int64 UINT64;
typedef signed __int64 SINT64;
#endif

#if defined _MSC_VER
#if defined _M_IX86 && !defined __INTEL_COMPILER	// Win IA-32, MS compiler
#define ALIGN(n)
#else
#define ALIGN(n) __declspec(align(n))
#endif
#else
#define ALIGN(n) __attribute__ ((aligned(n)))
#endif

// bid_gcc_intrinsics.h will also define this.
typedef
ALIGN (16)
     struct {
       UINT64 w[2];
     } UINT128;
#endif


#if !defined _MSC_VER || defined __INTEL_COMPILER
#define __ENABLE_BINARY80__  1
#endif

#ifndef HPUX_OS
#define BINARY80 long double
#define BINARY128 UINT128
#define SQRT80 sqrtl
#else
#define BINARY80 __float80
#define BINARY128 __float128
#define SQRT80 sqrtw
#endif

     typedef ALIGN (8)
     struct {
       UINT64 w[3];
     } UINT192;
     typedef ALIGN (16)
     struct {
       UINT64 w[4];
     } UINT256;
     typedef unsigned int FPSC;	// floating-point status and control

// TYPE parameters
#define BID128_MAXDIGITS	34
#define BID64_MAXDIGITS		16
#define BID32_MAXDIGITS		7

// rounding modes
#define ROUNDING_TO_NEAREST	0x00000
#define ROUNDING_DOWN		0x00001
#define ROUNDING_UP		0x00002
#define ROUNDING_TO_ZERO	0x00003
#define ROUNDING_TIES_AWAY      0x00004

#define RMODE_MASK (ROUNDING_TO_NEAREST | ROUNDING_DOWN | ROUNDING_UP | ROUNDING_TO_ZERO | ROUNDING_TIES_AWAY)

// status
#define FLAG_MASK	        0x0000003f
#define BID_IEEE_FLAGS		0x0000003d
#define EXACT_STATUS            0x00000000
#define INEXACT_EXCEPTION	0x00000020
#define UNDERFLOW_EXCEPTION	0x00000010
#define OVERFLOW_EXCEPTION	0x00000008
#define ZERO_DIVIDE_EXCEPTION	0x00000004
#define DENORMAL_EXCEPTION	0x00000002
#define INVALID_EXCEPTION	0x00000001

#define MODE_MASK               0x00001f80
#define INEXACT_MODE            0x00001000
#define UNDERFLOW_MODE          0x00000800
#define OVERFLOW_MODE           0x00000400
#define ZERO_DIVIDE_MODE        0x00000200
#define DENORMAL_MODE           0x00000100
#define INVALID_MODE            0x00000080

#if defined LINUX || defined __GLIBC__ || defined SUNOS
#define LX16  "%016llx"
#define LX    "%llx"
#define LD4   "%4llu"
#define LD16  "%016lld"
#define LD    "%lld"
#define LUD   "%llu"
#define LUD16 "%016llu"
#define X8    "%08x"
#define X4    "%04x"

#define FMT_LLX16  "%016llx"
#define FMT_LLX    "%llx"
#define FMT_LLU4   "%4llu"
#define FMT_LLD16  "%016lld"
#define FMT_LLD    "%lld"
#define FMT_LLU    "%llu"
#define FMT_LLU16  "%016llu"
#define FMT_X8     "%08x"
#define FMT_X4     "%04x"
#else
#define LX16  "%016I64x"
#define LX    "%I64x"
#define LD16  "%016I64d"
#define LD4   "%4I64u"
#define LD    "%I64d"
#define LUD    "%I64u"
#define LUD16 "%016I64u"
#define X8    "%08x"
#define X4    "%04x"

#define FMT_LLX16 "%016I64x"
#define FMT_LLX   "%I64x"
#define FMT_LLD16 "%016I64d"
#define FMT_LLU4  "%4I64u"
#define FMT_LLD   "%I64d"
#define FMT_LLU   "%I64u"
#define FMT_LLU16 "%016I64u"
#define FMT_X8    "%08x"
#define FMT_X4    "%04x"
#endif

#define decNumberIsSNaN(dn)      (((dn)->bits&(DECSNAN))!=0)
     int __signbitf (float);
     int __signbit (double);

#define __IMFC99MACRO_( __x__, __func__ ) \
        (( sizeof( __x__ ) > sizeof( float )) \
         ? __func__( (double)(__x__) ) \
                : __func__##f( (float)(__x__) ))

#define signbit( __x__ )    __IMFC99MACRO_( __x__, __signbit )

#if !defined(__INTEL_COMPILER)

#define __fence

#define isinf( __x__ )      __IMFC99MACRO_( __x__, __isinf )
#define isnan( __x__ )      __IMFC99MACRO_( __x__, __isnan )

     int __isnanf (float);
     int __isnan (double);

     int __isinff (float);
     int __isinf (double);

#endif

/* rounding modes */
// typedef unsigned int _IDEC_round;
     extern _IDEC_round _IDEC_gblround;	// initialized to ROUNDING_TO_NEAREST

/* exception flags */
// typedef unsigned int _IDEC_flags;  // could be a struct with diagnostic info
     extern _IDEC_flags _IDEC_gblflags;	// initialized to EXACT_STATUS

/* exception masks */
     typedef unsigned int _IDEC_exceptionmasks;
     extern _IDEC_exceptionmasks _IDEC_gblexceptionmasks;	// initialized to MODE_MASK

#if DECIMAL_ALTERNATE_EXCEPTION_HANDLING

/* exception information */

     typedef struct {
       unsigned int inexact_result:1;
       unsigned int underflow:1;
       unsigned int overflow:1;
       unsigned int zero_divide:1;
       unsigned int invalid_operation:1;
     } fpieee_exception_flags_t;

     typedef enum {
       _fp_round_nearest,
       _fp_round_minus_infinity,
       _fp_round_plus_infinity,
       _fp_round_chopped,
       _fp_round_away
     } fpieee_rounding_mode_t;

     typedef enum {
       _fp_precision24,
       _fp_precision63,
       _fp_precision64,
       _fp_precision7,
       _fp_precision16,
       _fp_precision34
     } _fpieee_precision_t;

     typedef enum {
       _fp_code_unspecified,
       _fp_code_add,
       _fp_code_subtract,
       _fp_code_multiply,
       _fp_code_divide,
       _fp_code_square_root,
       _fp_code_compare,
       _fp_code_convert,
       _fp_code_convert_to_integer_neareven,
       _fp_code_convert_to_integer_down,
       _fp_code_convert_to_integer_up,
       _fp_code_convert_to_integer_truncate,
       _fp_code_convert_to_integer_nearaway,
       _fp_code_fma,
       _fp_code_fmin,
       _fp_code_fmax,
       _fp_code_famin,
       _fp_code_famax,
       _fp_code_round_to_integral,
       _fp_code_minnum,
       _fp_code_maxnum,
       _fp_code_minnummag,
       _fp_code_maxnummag,
       _fp_code_quantize,
       _fp_code_logb,
       _fp_code_scaleb,
       _fp_code_remainder,
       _fp_code_nextup,
       _fp_code_nextdown,
       _fp_code_nextafter,
     } fp_operation_code_t;

     typedef enum {
       _fp_compare_equal,
       _fp_compare_greater,
       _fp_compare_less,
       _fp_compare_unordered
     } fpieee_compare_result_t;

     typedef enum {
       _fp_format_fp32,
       _fp_format_fp64,
       _fp_format_fp80,
       _fp_format_fp128,
       _fp_format_dec_fp32,
       _fp_format_dec_fp64,
       _fp_format_dec_fp128,
       _fp_format_i8,		/* 8-bit integer */
       _fp_format_i16,		/* 16-bit integer */
       _fp_format_i32,		/* 32-bit integer */
       _fp_format_i64,		/* 64-bit integer */
       _fp_format_u8,		/* 8-bit unsigned integer */
       _fp_format_u16,		/* 16-bit unsigned integer */
       _fp_format_u32,		/* 32-bit unsigned integer */
       _fp_format_u64,		/* 64-bit unsigned integer */
       _fp_format_compare,	/* compare value format */
       _fp_format_decimal_char,	/* decimal character */
       _fp_format_string	/* string */
     } fpieee_format_t;

     typedef struct {
       unsigned short W[5];
     } _float80_t;

     typedef struct {
       unsigned int W[4];
     } _float128_t;

     typedef struct {
       union {
	 float fp32_value;
	 double fp64_value;
	 _float80_t fp80_value;
	 _float128_t fp128_value;
	 UINT32 decfp32_value;
	 UINT64 decfp64_value;
	 UINT128 decfp128_value;
	 char i8_value;
	 short i16_value;
	 int i32_value;
	 SINT64 i64_value;
	 unsigned char u8_value;
	 unsigned short u16_value;
	 unsigned int u32_value;
	 unsigned long u64_value;
	 fpieee_compare_result_t compare_value;
	 unsigned char s[256];
       } value;
       unsigned int operand_valid:1;
       fpieee_format_t format:5;
     } fpieee_value_t;

     typedef struct {
       unsigned int rounding_mode:3;
       unsigned int precision:3;
       unsigned int operation:26;
       fpieee_exception_flags_t cause;
       fpieee_exception_flags_t enable;
       fpieee_exception_flags_t status;
       fpieee_value_t operand1;
       fpieee_value_t operand2;
       fpieee_value_t operand3;
       fpieee_value_t result;
     } _IDEC_excepthandling;
     extern _IDEC_excepthandling _IDEC_glbexcepthandling;

#endif

#if DECIMAL_CALL_BY_REFERENCE

     extern void bid_to_dpd32 (UINT32 * pres, UINT32 * px);
     extern void bid_to_dpd64 (UINT64 * pres, UINT64 * px);
     extern void bid_to_dpd128 (UINT128 * pres, UINT128 * px);
     extern void dpd_to_bid32 (UINT32 * pres, UINT32 * px);
     extern void dpd_to_bid64 (UINT64 * pres, UINT64 * px);
     extern void dpd_to_bid128 (UINT128 * pres, UINT128 * px);

     extern void bid128dd_add (UINT128 * pres, UINT64 * px,
			       UINT64 * py
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128dq_add (UINT128 * pres, UINT64 * px,
			       UINT128 * py
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128qd_add (UINT128 * pres, UINT128 * px,
			       UINT64 * py
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_add (UINT128 * pres, UINT128 * px,
			     UINT128 *
			     py _RND_MODE_PARAM _EXC_FLAGS_PARAM
			     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128dd_sub (UINT128 * pres, UINT64 * px,
			       UINT64 * py
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128dq_sub (UINT128 * pres, UINT64 * px,
			       UINT128 * py
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128qd_sub (UINT128 * pres, UINT128 * px,
			       UINT64 * py
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_sub (UINT128 * pres, UINT128 * px,
			     UINT128 *
			     py _RND_MODE_PARAM _EXC_FLAGS_PARAM
			     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128dd_mul (UINT128 * pres, UINT64 * px,
			       UINT64 * py
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128dq_mul (UINT128 * pres, UINT64 * px,
			       UINT128 * py
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128qd_mul (UINT128 * pres, UINT128 * px,
			       UINT64 * py
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_mul (UINT128 * pres, UINT128 * px,
			     UINT128 * py
			     _RND_MODE_PARAM _EXC_FLAGS_PARAM
			     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_div (UINT128 * pres, UINT128 * px,
			     UINT128 *
			     py _RND_MODE_PARAM _EXC_FLAGS_PARAM
			     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128dd_div (UINT128 * pres, UINT64 * px,
			       UINT64 * py
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128dq_div (UINT128 * pres, UINT64 * px,
			       UINT128 * py
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128qd_div (UINT128 * pres, UINT128 * px,
			       UINT64 * py
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_fma (UINT128 * pres, UINT128 * px,
			     UINT128 * py, UINT128 * pz
			     _RND_MODE_PARAM _EXC_FLAGS_PARAM
			     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128ddd_fma (UINT128 * pres, UINT64 * px,
				UINT64 * py, UINT64 * pz
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128ddq_fma (UINT128 * pres, UINT64 * px,
				UINT64 * py, UINT128 * pz
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128dqd_fma (UINT128 * pres, UINT64 * px,
				UINT128 * py, UINT64 * pz
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128dqq_fma (UINT128 * pres, UINT64 * px,
				UINT128 * py, UINT128 * pz
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128qdd_fma (UINT128 * pres, UINT128 * px,
				UINT64 * py, UINT64 * pz
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128qdq_fma (UINT128 * pres, UINT128 * px,
				UINT64 * py, UINT128 * pz
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128qqd_fma (UINT128 * pres, UINT128 * px,
				UINT128 * py, UINT64 * pz
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     // Note: bid128qqq_fma is represented by bid128_fma
     // Note: bid64ddd_fma is represented by bid64_fma
     extern void bid64ddq_fma (UINT64 * pres, UINT64 * px,
			       UINT64 * py, UINT128 * pz
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64dqd_fma (UINT64 * pres, UINT64 * px,
			       UINT128 * py, UINT64 * pz
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64dqq_fma (UINT64 * pres, UINT64 * px,
			       UINT128 * py, UINT128 * pz
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64qdd_fma (UINT64 * pres, UINT128 * px,
			       UINT64 * py, UINT64 * pz
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64qdq_fma (UINT64 * pres, UINT128 * px,
			       UINT64 * py, UINT128 * pz
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64qqd_fma (UINT64 * pres, UINT128 * px,
			       UINT128 * py, UINT64 * pz
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64qqq_fma (UINT64 * pres, UINT128 * px,
			       UINT128 * py, UINT128 * pz
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid128_sqrt (UINT128 * pres,
			      UINT128 *
			      px _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128d_sqrt (UINT128 * pres, UINT64 * px
			       _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid64_add (UINT64 * pres, UINT64 * px,
			    UINT64 *
			    py _RND_MODE_PARAM _EXC_FLAGS_PARAM
			    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64dq_add (UINT64 * pres, UINT64 * px,
			      UINT128 * py
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64qd_add (UINT64 * pres, UINT128 * px,
			      UINT64 * py
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64qq_add (UINT64 * pres, UINT128 * px,
			      UINT128 * py
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_sub (UINT64 * pres, UINT64 * px,
			    UINT64 *
			    py _RND_MODE_PARAM _EXC_FLAGS_PARAM
			    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64dq_sub (UINT64 * pres, UINT64 * px,
			      UINT128 * py
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64qd_sub (UINT64 * pres, UINT128 * px,
			      UINT64 * py
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64qq_sub (UINT64 * pres, UINT128 * px,
			      UINT128 * py
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_mul (UINT64 * pres, UINT64 * px,
			    UINT64 * py
			    _RND_MODE_PARAM _EXC_FLAGS_PARAM
			    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64dq_mul (UINT64 * pres, UINT64 * px,
			      UINT128 * py
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64qd_mul (UINT64 * pres, UINT128 * px,
			      UINT64 * py
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64qq_mul (UINT64 * pres, UINT128 * px,
			      UINT128 * py
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_div (UINT64 * pres, UINT64 * px,
			    UINT64 *
			    py _RND_MODE_PARAM _EXC_FLAGS_PARAM
			    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64dq_div (UINT64 * pres, UINT64 * px,
			      UINT128 * py
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64qd_div (UINT64 * pres, UINT128 * px,
			      UINT64 * py
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64qq_div (UINT64 * pres, UINT128 * px,
			      UINT128 * py
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_fma (UINT64 * pres, UINT64 * px,
			    UINT64 * py,
			    UINT64 *
			    pz _RND_MODE_PARAM _EXC_FLAGS_PARAM
			    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_sqrt (UINT64 * pres,
			     UINT64 *
			     px _RND_MODE_PARAM _EXC_FLAGS_PARAM
			     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64q_sqrt (UINT64 * pres, UINT128 * px
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid128_to_int8_rnint (char *pres,
				       UINT128 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_to_int8_xrnint (char *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_int8_rninta (char *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_int8_xrninta (char *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_int8_int (char *pres,
				     UINT128 *
				     px _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_to_int8_xint (char *pres,
				      UINT128 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_to_int8_floor (char *pres,
				       UINT128 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_to_int8_xfloor (char *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_int8_ceil (char *pres,
				      UINT128 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_to_int8_xceil (char *pres,
				       UINT128 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_to_int16_rnint (short *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_int16_xrnint (short *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_int16_rninta (short *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_int16_xrninta (short *pres,
					  UINT128 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid128_to_int16_int (short *pres,
				      UINT128 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_to_int16_xint (short *pres,
				       UINT128 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_to_int16_floor (short *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_int16_xfloor (short *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_int16_ceil (short *pres,
				       UINT128 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_to_int16_xceil (short *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_uint8_rnint (unsigned char *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_uint8_xrnint (unsigned char *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_uint8_rninta (unsigned char *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_uint8_xrninta (unsigned char *pres,
					  UINT128 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid128_to_uint8_int (unsigned char *pres,
				      UINT128 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_to_uint8_xint (unsigned char *pres,
				       UINT128 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_to_uint8_floor (unsigned char *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_uint8_xfloor (unsigned char *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_uint8_ceil (unsigned char *pres,
				       UINT128 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_to_uint8_xceil (unsigned char *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_uint16_rnint (unsigned short *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_uint16_xrnint (unsigned short *pres,
					  UINT128 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid128_to_uint16_rninta (unsigned short *pres,
					  UINT128 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid128_to_uint16_xrninta (unsigned short *pres,
					   UINT128 *
					   px _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern void bid128_to_uint16_int (unsigned short *pres,
				       UINT128 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_to_uint16_xint (unsigned short *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_uint16_floor (unsigned short *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_uint16_xfloor (unsigned short *pres,
					  UINT128 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid128_to_uint16_ceil (unsigned short *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_uint16_xceil (unsigned short *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_int32_rnint (int *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_int32_xrnint (int *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_int32_rninta (int *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_int32_xrninta (int *pres,
					  UINT128 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid128_to_int32_int (int *pres,
				      UINT128 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_to_int32_xint (int *pres,
				       UINT128 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_to_int32_floor (int *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_int32_xfloor (int *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_int32_ceil (int *pres,
				       UINT128 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_to_int32_xceil (int *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_uint32_rnint (unsigned int *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_uint32_xrnint (unsigned int *pres,
					  UINT128 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid128_to_uint32_rninta (unsigned int *pres,
					  UINT128 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid128_to_uint32_xrninta (unsigned int *pres,
					   UINT128 *
					   px _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern void bid128_to_uint32_int (unsigned int *pres,
				       UINT128 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_to_uint32_xint (unsigned int *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_uint32_floor (unsigned int *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_uint32_xfloor (unsigned int *pres,
					  UINT128 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid128_to_uint32_ceil (unsigned int *pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_uint32_xceil (unsigned int *pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_int64_rnint (SINT64 * pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_int64_xrnint (SINT64 * pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_int64_rninta (SINT64 * pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_int64_xrninta (SINT64 * pres,
					  UINT128 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid128_to_int64_int (SINT64 * pres,
				      UINT128 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_to_int64_xint (SINT64 * pres,
				       UINT128 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_to_int64_floor (SINT64 * pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_int64_xfloor (SINT64 * pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_int64_ceil (SINT64 * pres,
				       UINT128 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_to_int64_xceil (SINT64 * pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_uint64_rnint (UINT64 * pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_uint64_xrnint (UINT64 * pres,
					  UINT128 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid128_to_uint64_rninta (UINT64 * pres,
					  UINT128 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid128_to_uint64_xrninta (UINT64 * pres,
					   UINT128 *
					   px _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern void bid128_to_uint64_int (UINT64 * pres,
				       UINT128 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_to_uint64_xint (UINT64 * pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_uint64_floor (UINT64 * pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_to_uint64_xfloor (UINT64 * pres,
					  UINT128 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid128_to_uint64_ceil (UINT64 * pres,
					UINT128 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_to_uint64_xceil (UINT64 * pres,
					 UINT128 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_to_int32_rnint (int *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_int32_xrnint (int *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_int32_rninta (int *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_int32_xrninta (int *pres,
					 UINT64 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_to_int32_int (int *pres,
				     UINT64 *
				     px _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_int32_xint (int *pres,
				      UINT64 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_int32_floor (int *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_int32_xfloor (int *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_int32_ceil (int *pres,
				      UINT64 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_int32_xceil (int *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_int8_rnint (char *pres,
				      UINT64 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_int8_xrnint (char *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_int8_rninta (char *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_int8_xrninta (char *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_int8_int (char *pres,
				    UINT64 *
				    px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				    _EXC_INFO_PARAM);
     extern void bid64_to_int8_xint (char *pres,
				     UINT64 *
				     px _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_int8_floor (char *pres,
				      UINT64 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_int8_xfloor (char *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_int8_ceil (char *pres,
				     UINT64 *
				     px _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_int8_xceil (char *pres,
				      UINT64 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_int16_rnint (short *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_int16_xrnint (short *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_int16_rninta (short *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_int16_xrninta (short *pres,
					 UINT64 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_to_int16_int (short *pres,
				     UINT64 *
				     px _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_int16_xint (short *pres,
				      UINT64 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_int16_floor (short *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_int16_xfloor (short *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_int16_ceil (short *pres,
				      UINT64 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_int16_xceil (short *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_uint8_rnint (unsigned char *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_uint8_xrnint (unsigned char *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_uint8_rninta (unsigned char *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_uint8_xrninta (unsigned char *pres,
					 UINT64 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_to_uint8_int (unsigned char *pres,
				     UINT64 *
				     px _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_uint8_xint (unsigned char *pres,
				      UINT64 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_uint8_floor (unsigned char *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_uint8_xfloor (unsigned char *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_uint8_ceil (unsigned char *pres,
				      UINT64 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_uint8_xceil (unsigned char *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_uint16_rnint (unsigned short *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_uint16_xrnint (unsigned short *pres,
					 UINT64 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_to_uint16_rninta (unsigned short *pres,
					 UINT64 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_to_uint16_xrninta (unsigned short *pres,
					  UINT64 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid64_to_uint16_int (unsigned short *pres,
				      UINT64 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_uint16_xint (unsigned short *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_uint16_floor (unsigned short *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_uint16_xfloor (unsigned short *pres,
					 UINT64 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_to_uint16_ceil (unsigned short *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_uint16_xceil (unsigned short *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_uint32_rnint (unsigned int *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_uint32_xrnint (unsigned int *pres,
					 UINT64 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_to_uint32_rninta (unsigned int *pres,
					 UINT64 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_to_uint32_xrninta (unsigned int *pres,
					  UINT64 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid64_to_uint32_int (unsigned int *pres,
				      UINT64 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_uint32_xint (unsigned int *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_uint32_floor (unsigned int *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_uint32_xfloor (unsigned int *pres,
					 UINT64 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_to_uint32_ceil (unsigned int *pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_uint32_xceil (unsigned int *pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_int64_rnint (SINT64 * pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_int64_xrnint (SINT64 * pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_int64_rninta (SINT64 * pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_int64_xrninta (SINT64 * pres,
					 UINT64 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_to_int64_int (SINT64 * pres,
				     UINT64 *
				     px _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_int64_xint (SINT64 * pres,
				      UINT64 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_int64_floor (SINT64 * pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_int64_xfloor (SINT64 * pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_int64_ceil (SINT64 * pres,
				      UINT64 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_int64_xceil (SINT64 * pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_uint64_rnint (UINT64 * pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_uint64_xrnint (UINT64 * pres,
					 UINT64 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_to_uint64_rninta (UINT64 * pres,
					 UINT64 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_to_uint64_xrninta (UINT64 * pres,
					  UINT64 *
					  px _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid64_to_uint64_int (UINT64 * pres,
				      UINT64 *
				      px _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_uint64_xint (UINT64 * pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_uint64_floor (UINT64 * pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_to_uint64_xfloor (UINT64 * pres,
					 UINT64 *
					 px _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_to_uint64_ceil (UINT64 * pres,
				       UINT64 *
				       px _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_to_uint64_xceil (UINT64 * pres,
					UINT64 *
					px _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);

     extern void bid64_quiet_equal (int *pres, UINT64 * px, UINT64 * py
				    _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				    _EXC_INFO_PARAM);
     extern void bid64_quiet_greater (int *pres, UINT64 * px,
				      UINT64 *
				      py _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_quiet_greater_equal (int *pres, UINT64 * px,
					    UINT64 *
					    py _EXC_FLAGS_PARAM
					    _EXC_MASKS_PARAM
					    _EXC_INFO_PARAM);
     extern void bid64_quiet_greater_unordered (int *pres, UINT64 * px,
						UINT64 *
						py _EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern void bid64_quiet_less (int *pres, UINT64 * px,
				   UINT64 *
				   py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				   _EXC_INFO_PARAM);
     extern void bid64_quiet_less_equal (int *pres, UINT64 * px,
					 UINT64 *
					 py _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid64_quiet_less_unordered (int *pres, UINT64 * px,
					     UINT64 *
					     py _EXC_FLAGS_PARAM
					     _EXC_MASKS_PARAM
					     _EXC_INFO_PARAM);
     extern void bid64_quiet_not_equal (int *pres, UINT64 * px,
					UINT64 *
					py _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_quiet_not_greater (int *pres, UINT64 * px,
					  UINT64 *
					  py _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid64_quiet_not_less (int *pres, UINT64 * px,
				       UINT64 *
				       py _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_quiet_ordered (int *pres, UINT64 * px,
				      UINT64 *
				      py _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_quiet_unordered (int *pres, UINT64 * px,
					UINT64 *
					py _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid64_signaling_greater (int *pres, UINT64 * px,
					  UINT64 *
					  py _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid64_signaling_greater_equal (int *pres, UINT64 * px,
						UINT64 *
						py _EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern void bid64_signaling_greater_unordered (int *pres,
						    UINT64 * px,
						    UINT64 *
						    py _EXC_FLAGS_PARAM
						    _EXC_MASKS_PARAM
						    _EXC_INFO_PARAM);
     extern void bid64_signaling_less (int *pres, UINT64 * px,
				       UINT64 *
				       py _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid64_signaling_less_equal (int *pres, UINT64 * px,
					     UINT64 *
					     py _EXC_FLAGS_PARAM
					     _EXC_MASKS_PARAM
					     _EXC_INFO_PARAM);
     extern void bid64_signaling_less_unordered (int *pres, UINT64 * px,
						 UINT64 *
						 py _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern void bid64_signaling_not_greater (int *pres, UINT64 * px,
					      UINT64 *
					      py _EXC_FLAGS_PARAM
					      _EXC_MASKS_PARAM
					      _EXC_INFO_PARAM);
     extern void bid64_signaling_not_less (int *pres, UINT64 * px,
					   UINT64 *
					   py _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);

     extern void bid128_quiet_equal (int *pres, UINT128 * px,
				     UINT128 *
				     py _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_quiet_greater (int *pres, UINT128 * px,
				       UINT128 *
				       py _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_quiet_greater_equal (int *pres, UINT128 * px,
					     UINT128 *
					     py _EXC_FLAGS_PARAM
					     _EXC_MASKS_PARAM
					     _EXC_INFO_PARAM);
     extern void bid128_quiet_greater_unordered (int *pres,
						 UINT128 * px,
						 UINT128 *
						 py _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern void bid128_quiet_less (int *pres, UINT128 * px,
				    UINT128 *
				    py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				    _EXC_INFO_PARAM);
     extern void bid128_quiet_less_equal (int *pres, UINT128 * px,
					  UINT128 *
					  py _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern void bid128_quiet_less_unordered (int *pres, UINT128 * px,
					      UINT128 *
					      py _EXC_FLAGS_PARAM
					      _EXC_MASKS_PARAM
					      _EXC_INFO_PARAM);
     extern void bid128_quiet_not_equal (int *pres, UINT128 * px,
					 UINT128 *
					 py _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_quiet_not_greater (int *pres, UINT128 * px,
					   UINT128 *
					   py _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern void bid128_quiet_not_less (int *pres, UINT128 * px,
					UINT128 *
					py _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_quiet_ordered (int *pres, UINT128 * px,
				       UINT128 *
				       py _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_quiet_unordered (int *pres, UINT128 * px,
					 UINT128 *
					 py _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern void bid128_signaling_greater (int *pres, UINT128 * px,
					   UINT128 *
					   py _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern void bid128_signaling_greater_equal (int *pres,
						 UINT128 * px,
						 UINT128 *
						 py _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern void bid128_signaling_greater_unordered (int *pres,
						     UINT128 * px,
						     UINT128 *
						     py _EXC_FLAGS_PARAM
						     _EXC_MASKS_PARAM
						     _EXC_INFO_PARAM);
     extern void bid128_signaling_less (int *pres, UINT128 * px,
					UINT128 *
					py _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern void bid128_signaling_less_equal (int *pres, UINT128 * px,
					      UINT128 *
					      py _EXC_FLAGS_PARAM
					      _EXC_MASKS_PARAM
					      _EXC_INFO_PARAM);
     extern void bid128_signaling_less_unordered (int *pres,
						  UINT128 * px,
						  UINT128 *
						  py _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern void bid128_signaling_not_greater (int *pres, UINT128 * px,
					       UINT128 *
					       py _EXC_FLAGS_PARAM
					       _EXC_MASKS_PARAM
					       _EXC_INFO_PARAM);
     extern void bid128_signaling_not_less (int *pres, UINT128 * px,
					    UINT128 *
					    py _EXC_FLAGS_PARAM
					    _EXC_MASKS_PARAM
					    _EXC_INFO_PARAM);

     extern void bid64_round_integral_exact (UINT64 * pres, UINT64 * px
					     _RND_MODE_PARAM
					     _EXC_FLAGS_PARAM
					     _EXC_MASKS_PARAM
					     _EXC_INFO_PARAM);
     extern void bid64_round_integral_nearest_even (UINT64 * pres,
						    UINT64 *
						    px _EXC_FLAGS_PARAM
						    _EXC_MASKS_PARAM
						    _EXC_INFO_PARAM);
     extern void bid64_round_integral_negative (UINT64 * pres,
						UINT64 *
						px _EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern void bid64_round_integral_positive (UINT64 * pres,
						UINT64 *
						px _EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern void bid64_round_integral_zero (UINT64 * pres,
					    UINT64 *
					    px _EXC_FLAGS_PARAM
					    _EXC_MASKS_PARAM
					    _EXC_INFO_PARAM);
     extern void bid64_round_integral_nearest_away (UINT64 * pres,
						    UINT64 *
						    px _EXC_FLAGS_PARAM
						    _EXC_MASKS_PARAM
						    _EXC_INFO_PARAM);

     extern void bid128_round_integral_exact (UINT128 * pres,
					      UINT128 *
					      px _RND_MODE_PARAM
					      _EXC_FLAGS_PARAM
					      _EXC_MASKS_PARAM
					      _EXC_INFO_PARAM);
     extern void bid128_round_integral_nearest_even (UINT128 * pres,
						     UINT128 *
						     px _EXC_FLAGS_PARAM
						     _EXC_MASKS_PARAM
						     _EXC_INFO_PARAM);
     extern void bid128_round_integral_negative (UINT128 * pres,
						 UINT128 *
						 px _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern void bid128_round_integral_positive (UINT128 * pres,
						 UINT128 *
						 px _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern void bid128_round_integral_zero (UINT128 * pres,
					     UINT128 *
					     px _EXC_FLAGS_PARAM
					     _EXC_MASKS_PARAM
					     _EXC_INFO_PARAM);
     extern void bid128_round_integral_nearest_away (UINT128 * pres,
						     UINT128 *
						     px _EXC_FLAGS_PARAM
						     _EXC_MASKS_PARAM
						     _EXC_INFO_PARAM);

     extern void bid64_nextup (UINT64 * pres, UINT64 * px
			       _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			       _EXC_INFO_PARAM);
     extern void bid64_nextdown (UINT64 * pres,
				 UINT64 *
				 px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				 _EXC_INFO_PARAM);
     extern void bid64_nextafter (UINT64 * pres, UINT64 * px,
				  UINT64 *
				  py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				  _EXC_INFO_PARAM);

     extern void bid128_nextup (UINT128 * pres, UINT128 * px
				_EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				_EXC_INFO_PARAM);
     extern void bid128_nextdown (UINT128 * pres,
				  UINT128 *
				  px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				  _EXC_INFO_PARAM);
     extern void bid128_nextafter (UINT128 * pres, UINT128 * px,
				   UINT128 *
				   py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				   _EXC_INFO_PARAM);

     extern void bid64_minnum (UINT64 * pres, UINT64 * px, UINT64 * py
			       _EXC_FLAGS_PARAM);
     extern void bid64_minnum_mag (UINT64 * pres, UINT64 * px,
				   UINT64 * py _EXC_FLAGS_PARAM);
     extern void bid64_maxnum (UINT64 * pres, UINT64 * px, UINT64 * py
			       _EXC_FLAGS_PARAM);
     extern void bid64_maxnum_mag (UINT64 * pres, UINT64 * px,
				   UINT64 * py _EXC_FLAGS_PARAM);

     extern void bid128_minnum (UINT128 * pres, UINT128 * px,
				UINT128 * py _EXC_FLAGS_PARAM);
     extern void bid128_minnum_mag (UINT128 * pres, UINT128 * px,
				    UINT128 * py _EXC_FLAGS_PARAM);
     extern void bid128_maxnum (UINT128 * pres, UINT128 * px,
				UINT128 * py _EXC_FLAGS_PARAM);
     extern void bid128_maxnum_mag (UINT128 * pres, UINT128 * px,
				    UINT128 * py _EXC_FLAGS_PARAM);

     extern void bid64_from_int32 (UINT64 * pres, int *px
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_from_uint32 (UINT64 * pres, unsigned int *px
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_from_int64 (UINT64 * pres, SINT64 * px
				   _RND_MODE_PARAM _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_from_uint64 (UINT64 * pres,
				    UINT64 *
				    px _RND_MODE_PARAM _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_from_int32 (UINT128 * pres,
				    int *px _EXC_MASKS_PARAM
				    _EXC_INFO_PARAM);
     extern void bid128_from_uint32 (UINT128 * pres,
				     unsigned int *px _EXC_MASKS_PARAM
				     _EXC_INFO_PARAM);
     extern void bid128_from_int64 (UINT128 * pres,
				    SINT64 *
				    px _EXC_MASKS_PARAM
				    _EXC_INFO_PARAM);
     extern void bid128_from_uint64 (UINT128 * pres,
				     UINT64 *
				     px _EXC_MASKS_PARAM
				     _EXC_INFO_PARAM);

     extern void bid64_isSigned (int *pres, UINT64 * px
				 _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_isNormal (int *pres, UINT64 * px
				 _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_isSubnormal (int *pres, UINT64 * px
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_isFinite (int *pres, UINT64 * px
				 _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_isZero (int *pres, UINT64 * px
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_isInf (int *pres, UINT64 * px
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_isSignaling (int *pres, UINT64 * px
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_isCanonical (int *pres, UINT64 * px
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_isNaN (int *pres, UINT64 * px
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_copy (UINT64 * pres, UINT64 * px
			     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_negate (UINT64 * pres, UINT64 * px
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_abs (UINT64 * pres, UINT64 * px
			    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_copySign (UINT64 * pres, UINT64 * px, UINT64 * py
				 _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_class (int *pres, UINT64 * px
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_sameQuantum (int *pres, UINT64 * px, UINT64 * py
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_totalOrder (int *pres, UINT64 * px, UINT64 * py
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_totalOrderMag (int *pres, UINT64 * px,
				      UINT64 *
				      py _EXC_MASKS_PARAM
				      _EXC_INFO_PARAM);
     extern void bid64_radix (int *pres,
			      UINT64 *
			      px _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid128_isSigned (int *pres, UINT128 * px
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_isNormal (int *pres, UINT128 * px
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_isSubnormal (int *pres, UINT128 * px
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_isFinite (int *pres, UINT128 * px
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_isZero (int *pres, UINT128 * px
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_isInf (int *pres, UINT128 * px
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_isSignaling (int *pres, UINT128 * px
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_isCanonical (int *pres, UINT128 * px
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_isNaN (int *pres, UINT128 * px
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_copy (UINT128 * pres, UINT128 * px
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_negate (UINT128 * pres, UINT128 * px
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_abs (UINT128 * pres, UINT128 * px
			     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_copySign (UINT128 * pres, UINT128 * px,
				  UINT128 *
				  py _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_class (int *pres,
			       UINT128 *
			       px _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_sameQuantum (int *pres, UINT128 * px,
				     UINT128 *
				     py _EXC_MASKS_PARAM
				     _EXC_INFO_PARAM);
     extern void bid128_totalOrder (int *pres, UINT128 * px,
				    UINT128 *
				    py _EXC_MASKS_PARAM
				    _EXC_INFO_PARAM);
     extern void bid128_totalOrderMag (int *pres, UINT128 * px,
				       UINT128 *
				       py _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern void bid128_radix (int *pres,
			       UINT128 *
			       px _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid64_rem (UINT64 * pres, UINT64 * px, UINT64 * py
			    _EXC_FLAGS_PARAM
			    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_logb (int * pres, UINT64 * px
			     _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			     _EXC_INFO_PARAM);
     extern void bid64_scalb (UINT64 * pres, UINT64 * px,
			      int *pn _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid128_rem (UINT128 * pres, UINT128 * px, UINT128 * py
			     _EXC_FLAGS_PARAM
			     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_logb (int * pres, UINT128 * px
			      _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_scalb (UINT128 * pres, UINT128 * px,
			       int *pn _RND_MODE_PARAM _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid32_to_bid64 (UINT64 * pres,
				 UINT32 *
				 px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				 _EXC_INFO_PARAM);
     extern void bid32_to_bid128 (UINT128 * pres,
				  UINT32 *
				  px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				  _EXC_INFO_PARAM);
     extern void bid64_to_bid128 (UINT128 * pres,
				  UINT64 *
				  px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				  _EXC_INFO_PARAM);
     extern void bid64_to_bid32 (UINT32 * pres,
				 UINT64 *
				 px _RND_MODE_PARAM _EXC_FLAGS_PARAM
				 _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_to_bid32 (UINT32 * pres,
				  UINT128 *
				  px _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_to_bid64 (UINT64 * pres,
				  UINT128 *
				  px _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid64_from_string (UINT64 * pres, char *ps
				    _RND_MODE_PARAM _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid64_to_string (char *ps, UINT64 * px
				  _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_from_string (UINT128 * pres, char *ps
				     _RND_MODE_PARAM _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_to_string (char *str, UINT128 * px
				   _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid64_quantize (UINT64 * pres, UINT64 * px,
				 UINT64 *
				 py _RND_MODE_PARAM _EXC_FLAGS_PARAM
				 _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid128_quantize (UINT128 * pres, UINT128 * px,
				  UINT128 *
				  py _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid128_to_binary32 (float *pres, UINT128 * px
				     _RND_MODE_PARAM _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid128_to_binary64 (double *pres, UINT128 * px
				     _RND_MODE_PARAM _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid128_to_binary80 (BINARY80 * pres, UINT128 * px
				     _RND_MODE_PARAM _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid128_to_binary128 (BINARY128 * pres, UINT128 * px
				      _RND_MODE_PARAM _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void binary128_to_bid32 (UINT32 * pres, BINARY128 * px
				     _RND_MODE_PARAM _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void binary128_to_bid64 (UINT64 * pres, BINARY128 * px
				     _RND_MODE_PARAM _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void binary128_to_bid128 (UINT128 * pres, BINARY128 * px
				      _RND_MODE_PARAM _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid64_to_binary32 (float *pres, UINT64 * px
				    _RND_MODE_PARAM _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid64_to_binary64 (double *pres, UINT64 * px
				    _RND_MODE_PARAM _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid64_to_binary80 (BINARY80 * pres, UINT64 * px
				    _RND_MODE_PARAM _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid64_to_binary128 (BINARY128 * pres, UINT64 * px
				     _RND_MODE_PARAM _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void binary64_to_bid32 (UINT32 * pres, double *px
				    _RND_MODE_PARAM _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void binary64_to_bid64 (UINT64 * pres, double *px
				    _RND_MODE_PARAM _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void binary64_to_bid128 (UINT128 * pres, double *px
				     _RND_MODE_PARAM _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid32_to_binary32 (float *pres, UINT32 * px
				    _RND_MODE_PARAM _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid32_to_binary64 (double *pres, UINT32 * px
				    _RND_MODE_PARAM _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid32_to_binary80 (BINARY80 * pres, UINT32 * px
				    _RND_MODE_PARAM _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid32_to_binary128 (BINARY128 * pres, UINT32 * px
				     _RND_MODE_PARAM _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void binary32_to_bid32 (UINT32 * pres, float *px
				    _RND_MODE_PARAM _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void binary32_to_bid64 (UINT64 * pres, float *px
				    _RND_MODE_PARAM _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void binary32_to_bid128 (UINT128 * pres, float *px
				     _RND_MODE_PARAM _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void binary80_to_bid32 (UINT32 * pres, BINARY80 * px
				    _RND_MODE_PARAM _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void binary80_to_bid64 (UINT64 * pres, BINARY80 * px
				    _RND_MODE_PARAM _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void binary80_to_bid128 (UINT128 * pres, BINARY80 * px
				     _RND_MODE_PARAM _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void is754 (int *retval);

     extern void is754R (int *retval);

     extern void signalException (_IDEC_flags *
				  pflagsmask _EXC_FLAGS_PARAM);

     extern void lowerFlags (_IDEC_flags * pflagsmask _EXC_FLAGS_PARAM);

     extern void testFlags (_IDEC_flags * praised,
			    _IDEC_flags * pflagsmask _EXC_FLAGS_PARAM);

     extern void testSavedFlags (_IDEC_flags * praised,
				 _IDEC_flags * psavedflags,
				 _IDEC_flags * pflagsmask);

     extern void restoreFlags (_IDEC_flags * pflagsvalues,
			       _IDEC_flags *
			       pflagsmask _EXC_FLAGS_PARAM);

     extern void saveFlags (_IDEC_flags * pflagsvalues,
			    _IDEC_flags * pflagsmask _EXC_FLAGS_PARAM);

     void getDecimalRoundingDirection (_IDEC_round *
				       rounding_mode _RND_MODE_PARAM);

     void setDecimalRoundingDirection (_IDEC_round *
				       rounding_mode _RND_MODE_PARAM);

#else

     extern UINT32 bid_to_dpd32 (UINT32 px);
     extern UINT64 bid_to_dpd64 (UINT64 px);
     extern UINT128 bid_to_dpd128 (UINT128 px);
     extern UINT32 dpd_to_bid32 (UINT32 px);
     extern UINT64 dpd_to_bid64 (UINT64 px);
     extern UINT128 dpd_to_bid128 (UINT128 px);

     extern UINT128 bid128dd_add (UINT64 x, UINT64 y
				  _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128dq_add (UINT64 x, UINT128 y
				  _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128qd_add (UINT128 x, UINT64 y
				  _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128_add (UINT128 x, UINT128 y
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128dd_sub (UINT64 x, UINT64 y
				  _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128dq_sub (UINT64 x, UINT128 y
				  _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128qd_sub (UINT128 x, UINT64 y
				  _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128_sub (UINT128 x,
				UINT128 y _RND_MODE_PARAM
				_EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				_EXC_INFO_PARAM);
     extern UINT128 bid128dd_mul (UINT64 x, UINT64 y
				  _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128dq_mul (UINT64 x, UINT128 y
				  _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128qd_mul (UINT128 x, UINT64 y
				  _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128_mul (UINT128 x, UINT128 y
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128_div (UINT128 x,
				UINT128 y _RND_MODE_PARAM
				_EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				_EXC_INFO_PARAM);
     extern UINT128 bid128dd_div (UINT64 x, UINT64 y
				  _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128dq_div (UINT64 x, UINT128 y
				  _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128qd_div (UINT128 x, UINT64 y
				  _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128_fma (UINT128 x, UINT128 y, UINT128 z
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128ddd_fma (UINT64 x, UINT64 y, UINT64 z
				   _RND_MODE_PARAM _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128ddq_fma (UINT64 x, UINT64 y, UINT128 z
				   _RND_MODE_PARAM _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128dqd_fma (UINT64 x, UINT128 y, UINT64 z
				   _RND_MODE_PARAM _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128dqq_fma (UINT64 x, UINT128 y,
				   UINT128 z
				   _RND_MODE_PARAM _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128qdd_fma (UINT128 x, UINT64 y, UINT64 z
				   _RND_MODE_PARAM _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128qdq_fma (UINT128 x, UINT64 y,
				   UINT128 z
				   _RND_MODE_PARAM _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128qqd_fma (UINT128 x, UINT128 y,
				   UINT64 z
				   _RND_MODE_PARAM _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     // Note: bid128qqq_fma is represented by bid128_fma
     // Note: bid64ddd_fma is represented by bid64_fma
     extern UINT64 bid64ddq_fma (UINT64 x, UINT64 y,
				 UINT128 z
				 _RND_MODE_PARAM _EXC_FLAGS_PARAM
				 _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64dqd_fma (UINT64 x, UINT128 y,
				 UINT64 z
				 _RND_MODE_PARAM _EXC_FLAGS_PARAM
				 _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64dqq_fma (UINT64 x, UINT128 y,
				 UINT128 z
				 _RND_MODE_PARAM _EXC_FLAGS_PARAM
				 _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64qdd_fma (UINT128 x, UINT64 y,
				 UINT64 z
				 _RND_MODE_PARAM _EXC_FLAGS_PARAM
				 _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64qdq_fma (UINT128 x, UINT64 y,
				 UINT128 z
				 _RND_MODE_PARAM _EXC_FLAGS_PARAM
				 _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64qqd_fma (UINT128 x, UINT128 y,
				 UINT64 z
				 _RND_MODE_PARAM _EXC_FLAGS_PARAM
				 _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64qqq_fma (UINT128 x, UINT128 y,
				 UINT128 z
				 _RND_MODE_PARAM _EXC_FLAGS_PARAM
				 _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern UINT128 bid128_sqrt (UINT128 x _RND_MODE_PARAM
				 _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				 _EXC_INFO_PARAM);
     extern UINT128 bid128d_sqrt (UINT64 x
				  _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern UINT64 bid64_add (UINT64 x, UINT64 y
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64dq_add (UINT64 x, UINT128 y
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64qd_add (UINT128 x, UINT64 y
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64qq_add (UINT128 x, UINT128 y
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64_sub (UINT64 x,
			      UINT64 y _RND_MODE_PARAM
			      _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			      _EXC_INFO_PARAM);
     extern UINT64 bid64dq_sub (UINT64 x, UINT128 y
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64qd_sub (UINT128 x, UINT64 y
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64qq_sub (UINT128 x, UINT128 y
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64_mul (UINT64 x, UINT64 y
			      _RND_MODE_PARAM _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64dq_mul (UINT64 x, UINT128 y
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64qd_mul (UINT128 x, UINT64 y
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64qq_mul (UINT128 x, UINT128 y
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64_div (UINT64 x,
			      UINT64 y _RND_MODE_PARAM
			      _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			      _EXC_INFO_PARAM);
     extern UINT64 bid64dq_div (UINT64 x, UINT128 y
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64qd_div (UINT128 x, UINT64 y
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64qq_div (UINT128 x, UINT128 y
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64_fma (UINT64 x, UINT64 y,
			      UINT64 z _RND_MODE_PARAM
			      _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			      _EXC_INFO_PARAM);
     extern UINT64 bid64_sqrt (UINT64 x _RND_MODE_PARAM
			       _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			       _EXC_INFO_PARAM);
     extern UINT64 bid64q_sqrt (UINT128 x
				_RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern char bid128_to_int8_rnint (UINT128 x
				       _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern char bid128_to_int8_xrnint (UINT128 x
					_EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern char bid128_to_int8_rninta (UINT128 x
					_EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern char bid128_to_int8_xrninta (UINT128 x
					 _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern char bid128_to_int8_int (UINT128 x _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern char bid128_to_int8_xint (UINT128 x _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern char bid128_to_int8_floor (UINT128 x
				       _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern char bid128_to_int8_xfloor (UINT128 x
					_EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern char bid128_to_int8_ceil (UINT128 x _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern char bid128_to_int8_xceil (UINT128 x
				       _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern short bid128_to_int16_rnint (UINT128 x
					 _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern short bid128_to_int16_xrnint (UINT128 x
					  _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern short bid128_to_int16_rninta (UINT128 x
					  _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern short bid128_to_int16_xrninta (UINT128 x
					   _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern short bid128_to_int16_int (UINT128 x _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern short bid128_to_int16_xint (UINT128 x _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern short bid128_to_int16_floor (UINT128 x
					 _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern short bid128_to_int16_xfloor (UINT128 x
					  _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern short bid128_to_int16_ceil (UINT128 x _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern short bid128_to_int16_xceil (UINT128 x
					 _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern unsigned char bid128_to_uint8_rnint (UINT128 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern unsigned char bid128_to_uint8_xrnint (UINT128 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern unsigned char bid128_to_uint8_rninta (UINT128 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern unsigned char bid128_to_uint8_xrninta (UINT128 x
						   _EXC_FLAGS_PARAM
						   _EXC_MASKS_PARAM
						   _EXC_INFO_PARAM);
     extern unsigned char bid128_to_uint8_int (UINT128 x
					       _EXC_FLAGS_PARAM
					       _EXC_MASKS_PARAM
					       _EXC_INFO_PARAM);
     extern unsigned char bid128_to_uint8_xint (UINT128 x
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern unsigned char bid128_to_uint8_floor (UINT128 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern unsigned char bid128_to_uint8_xfloor (UINT128 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern unsigned char bid128_to_uint8_ceil (UINT128 x
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern unsigned char bid128_to_uint8_xceil (UINT128 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern unsigned short bid128_to_uint16_rnint (UINT128 x
						   _EXC_FLAGS_PARAM
						   _EXC_MASKS_PARAM
						   _EXC_INFO_PARAM);
     extern unsigned short bid128_to_uint16_xrnint (UINT128 x
						    _EXC_FLAGS_PARAM
						    _EXC_MASKS_PARAM
						    _EXC_INFO_PARAM);
     extern unsigned short bid128_to_uint16_rninta (UINT128 x
						    _EXC_FLAGS_PARAM
						    _EXC_MASKS_PARAM
						    _EXC_INFO_PARAM);
     extern unsigned short bid128_to_uint16_xrninta (UINT128 x
						     _EXC_FLAGS_PARAM
						     _EXC_MASKS_PARAM
						     _EXC_INFO_PARAM);
     extern unsigned short bid128_to_uint16_int (UINT128 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern unsigned short bid128_to_uint16_xint (UINT128 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern unsigned short bid128_to_uint16_floor (UINT128 x
						   _EXC_FLAGS_PARAM
						   _EXC_MASKS_PARAM
						   _EXC_INFO_PARAM);
     extern unsigned short bid128_to_uint16_xfloor (UINT128 x
						    _EXC_FLAGS_PARAM
						    _EXC_MASKS_PARAM
						    _EXC_INFO_PARAM);
     extern unsigned short bid128_to_uint16_ceil (UINT128 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern unsigned short bid128_to_uint16_xceil (UINT128 x
						   _EXC_FLAGS_PARAM
						   _EXC_MASKS_PARAM
						   _EXC_INFO_PARAM);
     extern int bid128_to_int32_rnint (UINT128 x _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern int bid128_to_int32_xrnint (UINT128 x _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern int bid128_to_int32_rninta (UINT128 x _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern int bid128_to_int32_xrninta (UINT128 x _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern int bid128_to_int32_int (UINT128 x _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid128_to_int32_xint (UINT128 x _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid128_to_int32_floor (UINT128 x _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern int bid128_to_int32_xfloor (UINT128 x _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern int bid128_to_int32_ceil (UINT128 x _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid128_to_int32_xceil (UINT128 x _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern unsigned int bid128_to_uint32_rnint (UINT128 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern unsigned int bid128_to_uint32_xrnint (UINT128 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern unsigned int bid128_to_uint32_rninta (UINT128 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern unsigned int bid128_to_uint32_xrninta (UINT128 x
						   _EXC_FLAGS_PARAM
						   _EXC_MASKS_PARAM
						   _EXC_INFO_PARAM);
     extern unsigned int bid128_to_uint32_int (UINT128 x
					       _EXC_FLAGS_PARAM
					       _EXC_MASKS_PARAM
					       _EXC_INFO_PARAM);
     extern unsigned int bid128_to_uint32_xint (UINT128 x
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern unsigned int bid128_to_uint32_floor (UINT128 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern unsigned int bid128_to_uint32_xfloor (UINT128 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern unsigned int bid128_to_uint32_ceil (UINT128 x
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern unsigned int bid128_to_uint32_xceil (UINT128 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern SINT64 bid128_to_int64_rnint (UINT128 x _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern SINT64 bid128_to_int64_xrnint (UINT128 x _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern SINT64 bid128_to_int64_rninta (UINT128 x _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern SINT64 bid128_to_int64_xrninta (UINT128 x _EXC_FLAGS_PARAM
					    _EXC_MASKS_PARAM
					    _EXC_INFO_PARAM);
     extern SINT64 bid128_to_int64_int (UINT128 x _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern SINT64 bid128_to_int64_xint (UINT128 x _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern SINT64 bid128_to_int64_floor (UINT128 x _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern SINT64 bid128_to_int64_xfloor (UINT128 x _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern SINT64 bid128_to_int64_ceil (UINT128 x _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern SINT64 bid128_to_int64_xceil (UINT128 x _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern UINT64 bid128_to_uint64_rnint (UINT128 x _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern UINT64 bid128_to_uint64_xrnint (UINT128 x _EXC_FLAGS_PARAM
					    _EXC_MASKS_PARAM
					    _EXC_INFO_PARAM);
     extern UINT64 bid128_to_uint64_rninta (UINT128 x _EXC_FLAGS_PARAM
					    _EXC_MASKS_PARAM
					    _EXC_INFO_PARAM);
     extern UINT64 bid128_to_uint64_xrninta (UINT128 x _EXC_FLAGS_PARAM
					     _EXC_MASKS_PARAM
					     _EXC_INFO_PARAM);
     extern UINT64 bid128_to_uint64_int (UINT128 x _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern UINT64 bid128_to_uint64_xint (UINT128 x _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern UINT64 bid128_to_uint64_floor (UINT128 x _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern UINT64 bid128_to_uint64_xfloor (UINT128 x _EXC_FLAGS_PARAM
					    _EXC_MASKS_PARAM
					    _EXC_INFO_PARAM);
     extern UINT64 bid128_to_uint64_ceil (UINT128 x _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern UINT64 bid128_to_uint64_xceil (UINT128 x _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern int bid64_to_int32_rnint (UINT64 x _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_to_int32_xrnint (UINT64 x _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern int bid64_to_int32_rninta (UINT64 x _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern int bid64_to_int32_xrninta (UINT64 x _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern int bid64_to_int32_int (UINT64 x _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_to_int32_xint (UINT64 x _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_to_int32_floor (UINT64 x _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_to_int32_xfloor (UINT64 x _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern int bid64_to_int32_ceil (UINT64 x _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_to_int32_xceil (UINT64 x _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern char bid64_to_int8_rnint (UINT64 x _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern char bid64_to_int8_xrnint (UINT64 x _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern char bid64_to_int8_rninta (UINT64 x _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern char bid64_to_int8_xrninta (UINT64 x _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern char bid64_to_int8_int (UINT64 x _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern char bid64_to_int8_xint (UINT64 x _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern char bid64_to_int8_floor (UINT64 x _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern char bid64_to_int8_xfloor (UINT64 x _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern char bid64_to_int8_ceil (UINT64 x _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern char bid64_to_int8_xceil (UINT64 x _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern short bid64_to_int16_rnint (UINT64 x _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern short bid64_to_int16_xrnint (UINT64 x _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern short bid64_to_int16_rninta (UINT64 x _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern short bid64_to_int16_xrninta (UINT64 x _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern short bid64_to_int16_int (UINT64 x _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern short bid64_to_int16_xint (UINT64 x _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern short bid64_to_int16_floor (UINT64 x _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern short bid64_to_int16_xfloor (UINT64 x _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern short bid64_to_int16_ceil (UINT64 x _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern short bid64_to_int16_xceil (UINT64 x _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern unsigned char bid64_to_uint8_rnint (UINT64 x
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern unsigned char bid64_to_uint8_xrnint (UINT64 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern unsigned char bid64_to_uint8_rninta (UINT64 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern unsigned char bid64_to_uint8_xrninta (UINT64 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern unsigned char bid64_to_uint8_int (UINT64 x _EXC_FLAGS_PARAM
					      _EXC_MASKS_PARAM
					      _EXC_INFO_PARAM);
     extern unsigned char bid64_to_uint8_xint (UINT64 x _EXC_FLAGS_PARAM
					       _EXC_MASKS_PARAM
					       _EXC_INFO_PARAM);
     extern unsigned char bid64_to_uint8_floor (UINT64 x
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern unsigned char bid64_to_uint8_xfloor (UINT64 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern unsigned char bid64_to_uint8_ceil (UINT64 x _EXC_FLAGS_PARAM
					       _EXC_MASKS_PARAM
					       _EXC_INFO_PARAM);
     extern unsigned char bid64_to_uint8_xceil (UINT64 x
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern unsigned short bid64_to_uint16_rnint (UINT64 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern unsigned short bid64_to_uint16_xrnint (UINT64 x
						   _EXC_FLAGS_PARAM
						   _EXC_MASKS_PARAM
						   _EXC_INFO_PARAM);
     extern unsigned short bid64_to_uint16_rninta (UINT64 x
						   _EXC_FLAGS_PARAM
						   _EXC_MASKS_PARAM
						   _EXC_INFO_PARAM);
     extern unsigned short bid64_to_uint16_xrninta (UINT64 x
						    _EXC_FLAGS_PARAM
						    _EXC_MASKS_PARAM
						    _EXC_INFO_PARAM);
     extern unsigned short bid64_to_uint16_int (UINT64 x
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern unsigned short bid64_to_uint16_xint (UINT64 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern unsigned short bid64_to_uint16_floor (UINT64 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern unsigned short bid64_to_uint16_xfloor (UINT64 x
						   _EXC_FLAGS_PARAM
						   _EXC_MASKS_PARAM
						   _EXC_INFO_PARAM);
     extern unsigned short bid64_to_uint16_ceil (UINT64 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern unsigned short bid64_to_uint16_xceil (UINT64 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern unsigned int bid64_to_uint32_rnint (UINT64 x
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern unsigned int bid64_to_uint32_xrnint (UINT64 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern unsigned int bid64_to_uint32_rninta (UINT64 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern unsigned int bid64_to_uint32_xrninta (UINT64 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern unsigned int bid64_to_uint32_int (UINT64 x _EXC_FLAGS_PARAM
					      _EXC_MASKS_PARAM
					      _EXC_INFO_PARAM);
     extern unsigned int bid64_to_uint32_xint (UINT64 x _EXC_FLAGS_PARAM
					       _EXC_MASKS_PARAM
					       _EXC_INFO_PARAM);
     extern unsigned int bid64_to_uint32_floor (UINT64 x
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern unsigned int bid64_to_uint32_xfloor (UINT64 x
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern unsigned int bid64_to_uint32_ceil (UINT64 x _EXC_FLAGS_PARAM
					       _EXC_MASKS_PARAM
					       _EXC_INFO_PARAM);
     extern unsigned int bid64_to_uint32_xceil (UINT64 x
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern SINT64 bid64_to_int64_rnint (UINT64 x _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern SINT64 bid64_to_int64_xrnint (UINT64 x _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern SINT64 bid64_to_int64_rninta (UINT64 x _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern SINT64 bid64_to_int64_xrninta (UINT64 x _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern SINT64 bid64_to_int64_int (UINT64 x _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern SINT64 bid64_to_int64_xint (UINT64 x _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern SINT64 bid64_to_int64_floor (UINT64 x _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern SINT64 bid64_to_int64_xfloor (UINT64 x _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern SINT64 bid64_to_int64_ceil (UINT64 x _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern SINT64 bid64_to_int64_xceil (UINT64 x _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern UINT64 bid64_to_uint64_rnint (UINT64 x _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern UINT64 bid64_to_uint64_xrnint (UINT64 x _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern UINT64 bid64_to_uint64_rninta (UINT64 x _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern UINT64 bid64_to_uint64_xrninta (UINT64 x _EXC_FLAGS_PARAM
					    _EXC_MASKS_PARAM
					    _EXC_INFO_PARAM);
     extern UINT64 bid64_to_uint64_int (UINT64 x _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern UINT64 bid64_to_uint64_xint (UINT64 x _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern UINT64 bid64_to_uint64_floor (UINT64 x _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern UINT64 bid64_to_uint64_xfloor (UINT64 x _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern UINT64 bid64_to_uint64_ceil (UINT64 x _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern UINT64 bid64_to_uint64_xceil (UINT64 x _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);

     extern int bid64_quiet_equal (UINT64 x, UINT64 y
				   _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				   _EXC_INFO_PARAM);
     extern int bid64_quiet_greater (UINT64 x,
				     UINT64 y _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_quiet_greater_equal (UINT64 x,
					   UINT64 y _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);
     extern int bid64_quiet_greater_unordered (UINT64 x,
					       UINT64 y _EXC_FLAGS_PARAM
					       _EXC_MASKS_PARAM
					       _EXC_INFO_PARAM);
     extern int bid64_quiet_less (UINT64 x,
				  UINT64 y _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_quiet_less_equal (UINT64 x,
					UINT64 y _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern int bid64_quiet_less_unordered (UINT64 x,
					    UINT64 y _EXC_FLAGS_PARAM
					    _EXC_MASKS_PARAM
					    _EXC_INFO_PARAM);
     extern int bid64_quiet_not_equal (UINT64 x,
				       UINT64 y _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern int bid64_quiet_not_greater (UINT64 x,
					 UINT64 y _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern int bid64_quiet_not_less (UINT64 x,
				      UINT64 y _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_quiet_ordered (UINT64 x,
				     UINT64 y _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_quiet_unordered (UINT64 x,
				       UINT64 y _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern int bid64_signaling_greater (UINT64 x,
					 UINT64 y _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern int bid64_signaling_greater_equal (UINT64 x,
					       UINT64 y _EXC_FLAGS_PARAM
					       _EXC_MASKS_PARAM
					       _EXC_INFO_PARAM);
     extern int bid64_signaling_greater_unordered (UINT64 x,
						   UINT64 y
						   _EXC_FLAGS_PARAM
						   _EXC_MASKS_PARAM
						   _EXC_INFO_PARAM);
     extern int bid64_signaling_less (UINT64 x,
				      UINT64 y _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_signaling_less_equal (UINT64 x,
					    UINT64 y _EXC_FLAGS_PARAM
					    _EXC_MASKS_PARAM
					    _EXC_INFO_PARAM);
     extern int bid64_signaling_less_unordered (UINT64 x,
						UINT64 y
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern int bid64_signaling_not_greater (UINT64 x,
					     UINT64 y _EXC_FLAGS_PARAM
					     _EXC_MASKS_PARAM
					     _EXC_INFO_PARAM);
     extern int bid64_signaling_not_less (UINT64 x,
					  UINT64 y _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);

     extern int bid128_quiet_equal (UINT128 x, UINT128 y
				    _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				    _EXC_INFO_PARAM);
     extern int bid128_quiet_greater (UINT128 x,
				      UINT128 y _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid128_quiet_greater_equal (UINT128 x,
					    UINT128 y _EXC_FLAGS_PARAM
					    _EXC_MASKS_PARAM
					    _EXC_INFO_PARAM);
     extern int bid128_quiet_greater_unordered (UINT128 x,
						UINT128 y
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern int bid128_quiet_less (UINT128 x,
				   UINT128 y _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid128_quiet_less_equal (UINT128 x,
					 UINT128 y _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);
     extern int bid128_quiet_less_unordered (UINT128 x,
					     UINT128 y _EXC_FLAGS_PARAM
					     _EXC_MASKS_PARAM
					     _EXC_INFO_PARAM);
     extern int bid128_quiet_not_equal (UINT128 x,
					UINT128 y _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern int bid128_quiet_not_greater (UINT128 x,
					  UINT128 y _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern int bid128_quiet_not_less (UINT128 x,
				       UINT128 y _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern int bid128_quiet_ordered (UINT128 x,
				      UINT128 y _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid128_quiet_unordered (UINT128 x,
					UINT128 y _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern int bid128_signaling_greater (UINT128 x,
					  UINT128 y _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);
     extern int bid128_signaling_greater_equal (UINT128 x,
						UINT128 y
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern int bid128_signaling_greater_unordered (UINT128 x,
						    UINT128 y
						    _EXC_FLAGS_PARAM
						    _EXC_MASKS_PARAM
						    _EXC_INFO_PARAM);
     extern int bid128_signaling_less (UINT128 x,
				       UINT128 y _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern int bid128_signaling_less_equal (UINT128 x,
					     UINT128 y _EXC_FLAGS_PARAM
					     _EXC_MASKS_PARAM
					     _EXC_INFO_PARAM);
     extern int bid128_signaling_less_unordered (UINT128 x,
						 UINT128 y
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern int bid128_signaling_not_greater (UINT128 x,
					      UINT128 y _EXC_FLAGS_PARAM
					      _EXC_MASKS_PARAM
					      _EXC_INFO_PARAM);
     extern int bid128_signaling_not_less (UINT128 x,
					   UINT128 y _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);

     extern UINT64 bid64_round_integral_exact (UINT64 x
					       _RND_MODE_PARAM
					       _EXC_FLAGS_PARAM
					       _EXC_MASKS_PARAM
					       _EXC_INFO_PARAM);
     extern UINT64 bid64_round_integral_nearest_even (UINT64 x
						      _EXC_FLAGS_PARAM
						      _EXC_MASKS_PARAM
						      _EXC_INFO_PARAM);
     extern UINT64 bid64_round_integral_negative (UINT64 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern UINT64 bid64_round_integral_positive (UINT64 x
						  _EXC_FLAGS_PARAM
						  _EXC_MASKS_PARAM
						  _EXC_INFO_PARAM);
     extern UINT64 bid64_round_integral_zero (UINT64 x _EXC_FLAGS_PARAM
					      _EXC_MASKS_PARAM
					      _EXC_INFO_PARAM);
     extern UINT64 bid64_round_integral_nearest_away (UINT64 x
						      _EXC_FLAGS_PARAM
						      _EXC_MASKS_PARAM
						      _EXC_INFO_PARAM);

     extern UINT128 bid128_round_integral_exact (UINT128 x
						 _RND_MODE_PARAM
						 _EXC_FLAGS_PARAM
						 _EXC_MASKS_PARAM
						 _EXC_INFO_PARAM);
     extern UINT128 bid128_round_integral_nearest_even (UINT128 x
							_EXC_FLAGS_PARAM
							_EXC_MASKS_PARAM
							_EXC_INFO_PARAM);
     extern UINT128 bid128_round_integral_negative (UINT128 x
						    _EXC_FLAGS_PARAM
						    _EXC_MASKS_PARAM
						    _EXC_INFO_PARAM);
     extern UINT128 bid128_round_integral_positive (UINT128 x
						    _EXC_FLAGS_PARAM
						    _EXC_MASKS_PARAM
						    _EXC_INFO_PARAM);
     extern UINT128 bid128_round_integral_zero (UINT128 x
						_EXC_FLAGS_PARAM
						_EXC_MASKS_PARAM
						_EXC_INFO_PARAM);
     extern UINT128 bid128_round_integral_nearest_away (UINT128 x
							_EXC_FLAGS_PARAM
							_EXC_MASKS_PARAM
							_EXC_INFO_PARAM);

     extern UINT64 bid64_nextup (UINT64 x
				 _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				 _EXC_INFO_PARAM);
     extern UINT64 bid64_nextdown (UINT64 x _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64_nextafter (UINT64 x,
				    UINT64 y _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern UINT128 bid128_nextup (UINT128 x
				   _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				   _EXC_INFO_PARAM);
     extern UINT128 bid128_nextdown (UINT128 x _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128_nextafter (UINT128 x,
				      UINT128 y _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern UINT64 bid64_minnum (UINT64 x, UINT64 y _EXC_FLAGS_PARAM);
     extern UINT64 bid64_minnum_mag (UINT64 x,
				     UINT64 y _EXC_FLAGS_PARAM);
     extern UINT64 bid64_maxnum (UINT64 x, UINT64 y _EXC_FLAGS_PARAM);
     extern UINT64 bid64_maxnum_mag (UINT64 x,
				     UINT64 y _EXC_FLAGS_PARAM);

     extern UINT128 bid128_minnum (UINT128 x,
				   UINT128 y _EXC_FLAGS_PARAM);
     extern UINT128 bid128_minnum_mag (UINT128 x,
				       UINT128 y _EXC_FLAGS_PARAM);
     extern UINT128 bid128_maxnum (UINT128 x,
				   UINT128 y _EXC_FLAGS_PARAM);
     extern UINT128 bid128_maxnum_mag (UINT128 x,
				       UINT128 y _EXC_FLAGS_PARAM);

     extern UINT64 bid64_from_int32 (int x _EXC_MASKS_PARAM
				     _EXC_INFO_PARAM);
     extern UINT64 bid64_from_uint32 (unsigned int x _EXC_MASKS_PARAM
				      _EXC_INFO_PARAM);
     extern UINT64 bid64_from_int64 (SINT64 x _RND_MODE_PARAM
				     _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				     _EXC_INFO_PARAM);
     extern UINT64 bid64_from_uint64 (UINT64 _RND_MODE_PARAM
				      _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				      _EXC_INFO_PARAM);
     extern UINT128 bid128_from_int32 (int x _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern UINT128 bid128_from_uint32 (unsigned int x _EXC_MASKS_PARAM
					_EXC_INFO_PARAM);
     extern UINT128 bid128_from_int64 (SINT64 x _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);
     extern UINT128 bid128_from_uint64 (UINT64 x _EXC_MASKS_PARAM
					_EXC_INFO_PARAM);

     extern int bid64_isSigned (UINT64 x _EXC_MASKS_PARAM
				_EXC_INFO_PARAM);
     extern int bid64_isNormal (UINT64 x _EXC_MASKS_PARAM
				_EXC_INFO_PARAM);
     extern int bid64_isSubnormal (UINT64 x _EXC_MASKS_PARAM
				   _EXC_INFO_PARAM);
     extern int bid64_isFinite (UINT64 x _EXC_MASKS_PARAM
				_EXC_INFO_PARAM);
     extern int bid64_isZero (UINT64 x _EXC_MASKS_PARAM
			      _EXC_INFO_PARAM);
     extern int bid64_isInf (UINT64 x _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_isSignaling (UINT64 x _EXC_MASKS_PARAM
				   _EXC_INFO_PARAM);
     extern int bid64_isCanonical (UINT64 x _EXC_MASKS_PARAM
				   _EXC_INFO_PARAM);
     extern int bid64_isNaN (UINT64 x _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64_copy (UINT64 x _EXC_MASKS_PARAM
			       _EXC_INFO_PARAM);
     extern UINT64 bid64_negate (UINT64 x _EXC_MASKS_PARAM
				 _EXC_INFO_PARAM);
     extern UINT64 bid64_abs (UINT64 x _EXC_MASKS_PARAM
			      _EXC_INFO_PARAM);
     extern UINT64 bid64_copySign (UINT64 x,
				   UINT64 y _EXC_MASKS_PARAM
				   _EXC_INFO_PARAM);
     extern int bid64_class (UINT64 x _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_sameQuantum (UINT64 x, UINT64 y
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_totalOrder (UINT64 x, UINT64 y
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_totalOrderMag (UINT64 x, UINT64 y
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_radix (UINT64 x _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern int bid128_isSigned (UINT128 x _EXC_MASKS_PARAM
				 _EXC_INFO_PARAM);
     extern int bid128_isNormal (UINT128 x _EXC_MASKS_PARAM
				 _EXC_INFO_PARAM);
     extern int bid128_isSubnormal (UINT128 x _EXC_MASKS_PARAM
				    _EXC_INFO_PARAM);
     extern int bid128_isFinite (UINT128 x _EXC_MASKS_PARAM
				 _EXC_INFO_PARAM);
     extern int bid128_isZero (UINT128 x _EXC_MASKS_PARAM
			       _EXC_INFO_PARAM);
     extern int bid128_isInf (UINT128 x _EXC_MASKS_PARAM
			      _EXC_INFO_PARAM);
     extern int bid128_isSignaling (UINT128 x _EXC_MASKS_PARAM
				    _EXC_INFO_PARAM);
     extern int bid128_isCanonical (UINT128 x _EXC_MASKS_PARAM
				    _EXC_INFO_PARAM);
     extern int bid128_isNaN (UINT128 x _EXC_MASKS_PARAM
			      _EXC_INFO_PARAM);
     extern UINT128 bid128_copy (UINT128 x _EXC_MASKS_PARAM
				 _EXC_INFO_PARAM);
     extern UINT128 bid128_negate (UINT128 x _EXC_MASKS_PARAM
				   _EXC_INFO_PARAM);
     extern UINT128 bid128_abs (UINT128 x _EXC_MASKS_PARAM
				_EXC_INFO_PARAM);
     extern UINT128 bid128_copySign (UINT128 x,
				     UINT128 y _EXC_MASKS_PARAM
				     _EXC_INFO_PARAM);
     extern int bid128_class (UINT128 x _EXC_MASKS_PARAM
			      _EXC_INFO_PARAM);
     extern int bid128_sameQuantum (UINT128 x,
				    UINT128 y _EXC_MASKS_PARAM
				    _EXC_INFO_PARAM);
     extern int bid128_totalOrder (UINT128 x,
				   UINT128 y _EXC_MASKS_PARAM
				   _EXC_INFO_PARAM);
     extern int bid128_totalOrderMag (UINT128 x,
				      UINT128 y _EXC_MASKS_PARAM
				      _EXC_INFO_PARAM);
     extern int bid128_radix (UINT128 x _EXC_MASKS_PARAM
			      _EXC_INFO_PARAM);

     extern UINT64 bid64_rem (UINT64 x, UINT64 y
			      _EXC_FLAGS_PARAM
			      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid64_logb (UINT64 x _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64_scalb (UINT64 x,
				int n _RND_MODE_PARAM _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern UINT128 bid128_rem (UINT128 x, UINT128 y
				_EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern int bid128_logb (UINT128 x
				 _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				 _EXC_INFO_PARAM);
     extern UINT128 bid128_scalb (UINT128 x,
				  int n _RND_MODE_PARAM _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern UINT64 bid32_to_bid64 (UINT32 x _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid32_to_bid128 (UINT32 x _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid64_to_bid128 (UINT64 x _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT32 bid64_to_bid32 (UINT64 x
				   _RND_MODE_PARAM _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT32 bid128_to_bid32 (UINT128 x _RND_MODE_PARAM
				    _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid128_to_bid64 (UINT128 x _RND_MODE_PARAM
				    _EXC_FLAGS_PARAM
				    _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern void bid64_to_string (char *ps, UINT64 x
				  _EXC_FLAGS_PARAM
				  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT64 bid64_from_string (char *ps
				      _RND_MODE_PARAM _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern void bid128_to_string (char *str, UINT128 x
				   _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);
     extern UINT128 bid128_from_string (char *ps
					_RND_MODE_PARAM _EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);

     extern UINT64 bid64_quantize (UINT64 x, UINT64 y
				   _RND_MODE_PARAM _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern UINT128 bid128_quantize (UINT128 x, UINT128 y
				     _RND_MODE_PARAM
				     _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);


     extern UINT32 binary128_to_bid32 (BINARY128 x
				       _RND_MODE_PARAM
				       _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);

     extern UINT64 binary128_to_bid64 (BINARY128 x
				       _RND_MODE_PARAM
				       _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);

     extern UINT128 binary128_to_bid128 (BINARY128 x
					 _RND_MODE_PARAM
					 _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);

     extern UINT32 binary64_to_bid32 (double x
				      _RND_MODE_PARAM
				      _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern UINT64 binary64_to_bid64 (double x
				      _RND_MODE_PARAM
				      _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern UINT128 binary64_to_bid128 (double x
					_RND_MODE_PARAM
					_EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);

     extern UINT32 binary80_to_bid32 (BINARY80 x
				      _RND_MODE_PARAM
				      _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern UINT64 binary80_to_bid64 (BINARY80 x
				      _RND_MODE_PARAM
				      _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern UINT128 binary80_to_bid128 (BINARY80 x
					_RND_MODE_PARAM
					_EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);

     extern UINT32 binary32_to_bid32 (float x
				      _RND_MODE_PARAM
				      _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern UINT64 binary32_to_bid64 (float x
				      _RND_MODE_PARAM
				      _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern UINT128 binary32_to_bid128 (float x
					_RND_MODE_PARAM
					_EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);

     extern float bid128_to_binary32 (UINT128 x
				      _RND_MODE_PARAM _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern double bid128_to_binary64 (UINT128 x
				       _RND_MODE_PARAM _EXC_FLAGS_PARAM
				       _EXC_MASKS_PARAM
				       _EXC_INFO_PARAM);

     extern BINARY80 bid128_to_binary80 (UINT128 x
					 _RND_MODE_PARAM
					 _EXC_FLAGS_PARAM
					 _EXC_MASKS_PARAM
					 _EXC_INFO_PARAM);

     extern BINARY128 bid128_to_binary128 (UINT128 x
					   _RND_MODE_PARAM
					   _EXC_FLAGS_PARAM
					   _EXC_MASKS_PARAM
					   _EXC_INFO_PARAM);

     extern float bid64_to_binary32 (UINT64 x
				     _RND_MODE_PARAM _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern double bid64_to_binary64 (UINT64 x
				      _RND_MODE_PARAM _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern BINARY80 bid64_to_binary80 (UINT64 x
					_RND_MODE_PARAM
					_EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);

     extern BINARY128 bid64_to_binary128 (UINT64 x
					  _RND_MODE_PARAM
					  _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);

     extern float bid32_to_binary32 (UINT32 x
				     _RND_MODE_PARAM _EXC_FLAGS_PARAM
				     _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern double bid32_to_binary64 (UINT32 x
				      _RND_MODE_PARAM _EXC_FLAGS_PARAM
				      _EXC_MASKS_PARAM _EXC_INFO_PARAM);

     extern BINARY80 bid32_to_binary80 (UINT32 x
					_RND_MODE_PARAM
					_EXC_FLAGS_PARAM
					_EXC_MASKS_PARAM
					_EXC_INFO_PARAM);

     extern BINARY128 bid32_to_binary128 (UINT32 x
					  _RND_MODE_PARAM
					  _EXC_FLAGS_PARAM
					  _EXC_MASKS_PARAM
					  _EXC_INFO_PARAM);

     extern int is754 (void);

     extern int is754R (void);

     extern void signalException (_IDEC_flags flagsmask
				  _EXC_FLAGS_PARAM);

     extern void lowerFlags (_IDEC_flags flagsmask _EXC_FLAGS_PARAM);

     extern _IDEC_flags testFlags (_IDEC_flags flagsmask
				   _EXC_FLAGS_PARAM);

     extern _IDEC_flags testSavedFlags (_IDEC_flags savedflags,
					_IDEC_flags flagsmask);

     extern void restoreFlags (_IDEC_flags flagsvalues,
			       _IDEC_flags flagsmask _EXC_FLAGS_PARAM);

     extern _IDEC_flags saveFlags (_IDEC_flags flagsmask
				   _EXC_FLAGS_PARAM);

#if !DECIMAL_GLOBAL_ROUNDING
     _IDEC_round getDecimalRoundingDirection (_IDEC_round rnd_mode);
#else
     _IDEC_round getDecimalRoundingDirection (void);
#endif

#if !DECIMAL_GLOBAL_ROUNDING
     _IDEC_round setDecimalRoundingDirection (_IDEC_round
					      rounding_mode
					      _RND_MODE_PARAM);
#else
     void setDecimalRoundingDirection (_IDEC_round rounding_mode);
#endif

#endif

// Internal Functions

     extern void
       round64_2_18 (int q,
		     int x,
		     UINT64 C,
		     UINT64 * ptr_Cstar,
		     int *delta_exp,
		     int *ptr_is_midpoint_lt_even,
		     int *ptr_is_midpoint_gt_even,
		     int *ptr_is_inexact_lt_midpoint,
		     int *ptr_is_inexact_gt_midpoint);

     extern void
       round128_19_38 (int q,
		       int x,
		       UINT128 C,
		       UINT128 * ptr_Cstar,
		       int *delta_exp,
		       int *ptr_is_midpoint_lt_even,
		       int *ptr_is_midpoint_gt_even,
		       int *ptr_is_inexact_lt_midpoint,
		       int *ptr_is_inexact_gt_midpoint);

     extern void
       round192_39_57 (int q,
		       int x,
		       UINT192 C,
		       UINT192 * ptr_Cstar,
		       int *delta_exp,
		       int *ptr_is_midpoint_lt_even,
		       int *ptr_is_midpoint_gt_even,
		       int *ptr_is_inexact_lt_midpoint,
		       int *ptr_is_inexact_gt_midpoint);

     extern void
       round256_58_76 (int q,
		       int x,
		       UINT256 C,
		       UINT256 * ptr_Cstar,
		       int *delta_exp,
		       int *ptr_is_midpoint_lt_even,
		       int *ptr_is_midpoint_gt_even,
		       int *ptr_is_inexact_lt_midpoint,
		       int *ptr_is_inexact_gt_midpoint);

#endif

// Prototypes for Internal Functions

     extern UINT32 bid_to_bid32 (UINT32);
     extern UINT64 bid_to_bid64 (UINT64);
     extern UINT128 bid_to_bid128 (UINT128);
     extern UINT32 bid32_canonize (UINT32);
     extern UINT64 bid64_canonize (UINT64);
     extern UINT128 bid128_canonize (UINT128);
