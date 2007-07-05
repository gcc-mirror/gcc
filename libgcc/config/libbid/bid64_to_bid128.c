/* Copyright (C) 2007  Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#define BID_128RES
#include "bid_internal.h"

/*
 * Takes a BID64 as input and converts it to a BID128 and returns it.
 */
TYPE0_FUNCTION_ARGTYPE1_NORND(UINT128, __bid64_to_bid128, UINT64, x)

  UINT128 new_coeff, res;
  UINT64 sign_x;
  int exponent_x = 0;
  UINT64 coefficient_x;

  if (!unpack_BID64 (&sign_x, &exponent_x, &coefficient_x, x)) {
    if (((x) << 1) >= 0xf000000000000000ull) {
      res.w[0] = 0;
      res.w[1] = (x) & 0xfe03ffffffffffffull;
      BID_RETURN (res);
    }
  }

  new_coeff.w[0] = coefficient_x;
  new_coeff.w[1] = 0;
  get_BID128_very_fast (&res, sign_x,
			exponent_x + DECIMAL_EXPONENT_BIAS_128 -
			DECIMAL_EXPONENT_BIAS, new_coeff);
  BID_RETURN (res);
}	// convert_bid64_to_bid128



/*
 * Takes a BID128 as input and converts it to a BID64 and returns it.
 */
#if DECIMAL_CALL_BY_REFERENCE

void
__bid128_to_bid64 (UINT64 * pres,
		 UINT128 *
		 px _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		 _EXC_INFO_PARAM) {
  UINT128 x = *px;
#else

UINT64
__bid128_to_bid64 (UINT128 x _RND_MODE_PARAM _EXC_FLAGS_PARAM
		 _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT128 CX, T128, TP128, Qh, Ql, Qh1, Stemp, Tmp, Tmp1;
  UINT64 sign_x, carry, cy, res;
  SINT64 D;
  int_float f64, fx;
  int exponent_x = 0, extra_digits, amount, bin_expon_cx;
  unsigned rmode, status, uf_check = 0;

#if DECIMAL_CALL_BY_REFERENCE
#if !DECIMAL_GLOBAL_ROUNDING
  _IDEC_round rnd_mode = *prnd_mode;
#endif
#endif

  BID_SWAP128(x);
  // unpack arguments, check for NaN or Infinity or 0
  if (!unpack_BID128 (&sign_x, &exponent_x, &CX, &x)) {
    if ((x.w[1] << 1) >= 0xf000000000000000ull) {
	  cy = ((x.w[1] & 0x00003fffffffffffull)<<4) | (x.w[0] >> 60);
      res = (x.w[1] & 0xfe00000000000000ull) | cy;
      BID_RETURN_VAL (res);
    }
    exponent_x =
      exponent_x - DECIMAL_EXPONENT_BIAS_128 + DECIMAL_EXPONENT_BIAS;
    if (exponent_x < 0) {
      res = sign_x;
      BID_RETURN_VAL (res);
    }
    if (exponent_x > DECIMAL_MAX_EXPON_64)
      exponent_x = DECIMAL_MAX_EXPON_64;
    res = sign_x | (((UINT64) exponent_x) << 53);
    BID_RETURN_VAL (res);
  }

  if (CX.w[1] || (CX.w[0] >= 10000000000000000ull)) {
    // find number of digits in coefficient
    // 2^64
    f64.i = 0x5f800000;
    // fx ~ CX
    fx.d = (float) CX.w[1] * f64.d + (float) CX.w[0];
    bin_expon_cx = ((fx.i >> 23) & 0xff) - 0x7f;
    extra_digits = __bid_estimate_decimal_digits[bin_expon_cx] - 16;
    // scale = 38-__bid_estimate_decimal_digits[bin_expon_cx];
    D = CX.w[1] - __bid_power10_index_binexp_128[bin_expon_cx].w[1];
    if (D > 0
	|| (!D
	    && CX.w[0] >= __bid_power10_index_binexp_128[bin_expon_cx].w[0]))
      extra_digits++;

    exponent_x += extra_digits;

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
    rmode = rnd_mode;
    if (sign_x && (unsigned) (rmode - 1) < 2)
      rmode = 3 - rmode;
#else
    rmode = 0;
#endif
#else
    rmode = 0;
#endif
    if (exponent_x < DECIMAL_EXPONENT_BIAS_128 - DECIMAL_EXPONENT_BIAS) {
      uf_check = 1;
      if (extra_digits == 1
	  && (exponent_x - DECIMAL_EXPONENT_BIAS_128 +
	      DECIMAL_EXPONENT_BIAS + 16 >= 0)) {
	extra_digits =
	  1 + DECIMAL_EXPONENT_BIAS_128 - DECIMAL_EXPONENT_BIAS -
	  exponent_x;
	exponent_x = DECIMAL_EXPONENT_BIAS_128 - DECIMAL_EXPONENT_BIAS;
	uf_check = 2;
      } else
	rmode = ROUNDING_TO_ZERO;
    }

    T128 = __bid_round_const_table_128[rmode][extra_digits];
    __add_carry_out (CX.w[0], carry, T128.w[0], CX.w[0]);
    CX.w[1] = CX.w[1] + T128.w[1] + carry;

    TP128 = __bid_reciprocals10_128[extra_digits];
    __mul_128x128_full (Qh, Ql, CX, TP128);
    amount = __bid_recip_scale[extra_digits];

    if (amount >= 64) {
      CX.w[0] = Qh.w[1] >> (amount - 64);
      CX.w[1] = 0;
    } else {
      __shr_128 (CX, Qh, amount);
    }

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
    if (!(rmode))
#endif
      if (CX.w[0] & 1) {
	// check whether fractional part of initial_P/10^ed1 is exactly .5

	// get remainder
	__shl_128_long (Qh1, Qh, (128 - amount));

	if (!Qh1.w[1] && !Qh1.w[0]
	    && (Ql.w[1] < __bid_reciprocals10_128[extra_digits].w[1]
		|| (Ql.w[1] == __bid_reciprocals10_128[extra_digits].w[1]
		    && Ql.w[0] < __bid_reciprocals10_128[extra_digits].w[0]))) {
	  CX.w[0]--;
	}
      }
#endif

    {
      status = INEXACT_EXCEPTION;
      // get remainder
      __shl_128_long (Qh1, Qh, (128 - amount));

      switch (rmode) {
      case ROUNDING_TO_NEAREST:
      case ROUNDING_TIES_AWAY:
	// test whether fractional part is 0
	if (Qh1.w[1] == 0x8000000000000000ull && (!Qh1.w[0])
	    && (Ql.w[1] < __bid_reciprocals10_128[extra_digits].w[1]
		|| (Ql.w[1] == __bid_reciprocals10_128[extra_digits].w[1]
		    && Ql.w[0] < __bid_reciprocals10_128[extra_digits].w[0])))
	  status = EXACT_STATUS;
	break;
      case ROUNDING_DOWN:
      case ROUNDING_TO_ZERO:
	if ((!Qh1.w[1]) && (!Qh1.w[0])
	    && (Ql.w[1] < __bid_reciprocals10_128[extra_digits].w[1]
		|| (Ql.w[1] == __bid_reciprocals10_128[extra_digits].w[1]
		    && Ql.w[0] < __bid_reciprocals10_128[extra_digits].w[0])))
	  status = EXACT_STATUS;
	break;
      default:
	// round up
	__add_carry_out (Stemp.w[0], cy, Ql.w[0],
			 __bid_reciprocals10_128[extra_digits].w[0]);
	__add_carry_in_out (Stemp.w[1], carry, Ql.w[1],
			    __bid_reciprocals10_128[extra_digits].w[1], cy);
	__shr_128_long (Qh, Qh1, (128 - amount));
	Tmp.w[0] = 1;
	Tmp.w[1] = 0;
	__shl_128_long (Tmp1, Tmp, amount);
	Qh.w[0] += carry;
	if (Qh.w[0] < carry)
	  Qh.w[1]++;
	if (__unsigned_compare_ge_128 (Qh, Tmp1))
	  status = EXACT_STATUS;
      }

      if (status != EXACT_STATUS) {
#ifdef SET_STATUS_FLAGS
	__set_status_flags (pfpsf, status);
#endif
	if (uf_check) {
	  if (uf_check == 1)	// result has not already been computed
	  {
	    res =
	      get_BID64_UF (sign_x,
			    exponent_x - DECIMAL_EXPONENT_BIAS_128 +
			    DECIMAL_EXPONENT_BIAS, CX.w[0], 1, rnd_mode,
			    pfpsf);
	    BID_RETURN_VAL (res);
	  } else {
#ifdef SET_STATUS_FLAGS
	    __set_status_flags (pfpsf, UNDERFLOW_EXCEPTION);
#endif
	  }
	}
      }
    }

  }

  res =
    get_BID64 (sign_x,
	       exponent_x - DECIMAL_EXPONENT_BIAS_128 +
	       DECIMAL_EXPONENT_BIAS, CX.w[0], rnd_mode, pfpsf);
  BID_RETURN_VAL (res);

}
