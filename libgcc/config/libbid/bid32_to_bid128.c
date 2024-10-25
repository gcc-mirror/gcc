/* Copyright (C) 2007-2024 Free Software Foundation, Inc.

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

#define BID_128RES
#include "bid_internal.h"

/*
 * Takes a BID32 as input and converts it to a BID128 and returns it.
 */
TYPE0_FUNCTION_ARGTYPE1_NORND (UINT128, bid32_to_bid128, UINT32, x)

     UINT128 new_coeff, res;
     UINT32 sign_x;
     int exponent_x;
     UINT32 coefficient_x;

if (!unpack_BID32 (&sign_x, &exponent_x, &coefficient_x, x)) {
if (((x) & 0x78000000) == 0x78000000) {
#ifdef SET_STATUS_FLAGS
  if (((x) & 0x7e000000) == 0x7e000000)	// sNaN
    __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
  res.w[0] = (coefficient_x & 0x000fffff);
  __mul_64x128_low (res, res.w[0], power10_table_128[27]);
  res.w[1] |=
    ((((UINT64) coefficient_x) << 32) & 0xfc00000000000000ull);

  BID_RETURN (res);
}
}

new_coeff.w[0] = coefficient_x;
new_coeff.w[1] = 0;
get_BID128_very_fast (&res, ((UINT64) sign_x) << 32,
		      exponent_x + DECIMAL_EXPONENT_BIAS_128 -
		      DECIMAL_EXPONENT_BIAS_32, new_coeff);
BID_RETURN (res);
}	// convert_bid32_to_bid128


/*
 * Takes a BID128 as input and converts it to a BID32 and returns it.
 */
#if DECIMAL_CALL_BY_REFERENCE

void
bid128_to_bid32 (UINT32 * pres,
		 UINT128 *
		 px _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		 _EXC_INFO_PARAM) {
  UINT128 x = *px;
#else

UINT32
bid128_to_bid32 (UINT128 x _RND_MODE_PARAM _EXC_FLAGS_PARAM
		 _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT128 CX, T128, TP128, Qh, Ql, Qh1, Stemp, Tmp, Tmp1, CX1;
  UINT64 sign_x, carry, cy;
  SINT64 D;
  UINT32 res;
  int_float f64, fx;
  int exponent_x, extra_digits, amount, bin_expon_cx, uf_check = 0;
  unsigned rmode, status;

#if DECIMAL_CALL_BY_REFERENCE
#if !DECIMAL_GLOBAL_ROUNDING
  _IDEC_round rnd_mode = *prnd_mode;
#endif
#endif

  BID_SWAP128 (x);
  // unpack arguments, check for NaN or Infinity or 0
  if (!unpack_BID128_value (&sign_x, &exponent_x, &CX, x)) {
    if (((x.w[1]) & 0x7800000000000000ull) == 0x7800000000000000ull) {
      Tmp.w[1] = (CX.w[1] & 0x00003fffffffffffull);
      Tmp.w[0] = CX.w[0];
      TP128 = reciprocals10_128[27];
      __mul_128x128_full (Qh, Ql, Tmp, TP128);
      amount = recip_scale[27] - 64;
      res = ((CX.w[1] >> 32) & 0xfc000000) | (Qh.w[1] >> amount);
#ifdef SET_STATUS_FLAGS
      if ((x.w[1] & SNAN_MASK64) == SNAN_MASK64)	// sNaN
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      BID_RETURN_VAL (res);
    }
    // x is 0
    exponent_x =
      exponent_x - DECIMAL_EXPONENT_BIAS_128 + DECIMAL_EXPONENT_BIAS_32;
    if (exponent_x < 0)
      exponent_x = 0;
    if (exponent_x > DECIMAL_MAX_EXPON_32)
      exponent_x = DECIMAL_MAX_EXPON_32;
    res = (sign_x >> 32) | (exponent_x << 23);
    BID_RETURN_VAL (res);

  }

  if (CX.w[1] || (CX.w[0] >= 10000000)) {
    // find number of digits in coefficient
    // 2^64
    f64.i = 0x5f800000;
    // fx ~ CX
    fx.d = (float) CX.w[1] * f64.d + (float) CX.w[0];
    bin_expon_cx = ((fx.i >> 23) & 0xff) - 0x7f;
    extra_digits = estimate_decimal_digits[bin_expon_cx] - 7;
    // scale = 38-estimate_decimal_digits[bin_expon_cx];
    D = CX.w[1] - power10_index_binexp_128[bin_expon_cx].w[1];
    if (D > 0
	|| (!D
	    && CX.w[0] >= power10_index_binexp_128[bin_expon_cx].w[0]))
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
    if (exponent_x <
	DECIMAL_EXPONENT_BIAS_128 - DECIMAL_EXPONENT_BIAS_32) {
      uf_check = 1;
      if (-extra_digits + exponent_x - DECIMAL_EXPONENT_BIAS_128 +
	  DECIMAL_EXPONENT_BIAS_32 + 35 >= 0) {
	if (exponent_x ==
	    DECIMAL_EXPONENT_BIAS_128 - DECIMAL_EXPONENT_BIAS_32 - 1) {
	  T128 = round_const_table_128[rmode][extra_digits];
	  __add_carry_out (CX1.w[0], carry, T128.w[0], CX.w[0]);
	  CX1.w[1] = CX.w[1] + T128.w[1] + carry;
	}
	extra_digits =
	  extra_digits + DECIMAL_EXPONENT_BIAS_128 -
	  DECIMAL_EXPONENT_BIAS_32 - exponent_x;
	exponent_x =
	  DECIMAL_EXPONENT_BIAS_128 - DECIMAL_EXPONENT_BIAS_32;
      } else
	rmode = ROUNDING_TO_ZERO;
    }

    T128 = round_const_table_128[rmode][extra_digits];
    __add_carry_out (CX.w[0], carry, T128.w[0], CX.w[0]);
    CX.w[1] = CX.w[1] + T128.w[1] + carry;

    TP128 = reciprocals10_128[extra_digits];
    __mul_128x128_full (Qh, Ql, CX, TP128);
    amount = recip_scale[extra_digits];

    if (amount >= 64) {
      CX.w[0] = Qh.w[1] >> (amount - 64);
      CX.w[1] = 0;
    } else {
      __shr_128 (CX, Qh, amount);
    }

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
    if (!(rnd_mode))
#endif
      if (CX.w[0] & 1) {
	// check whether fractional part of initial_P/10^ed1 is exactly .5

	// get remainder
	__shl_128_long (Qh1, Qh, (128 - amount));

	if (!Qh1.w[1] && !Qh1.w[0]
	    && (Ql.w[1] < reciprocals10_128[extra_digits].w[1]
		|| (Ql.w[1] == reciprocals10_128[extra_digits].w[1]
		    && Ql.w[0] < reciprocals10_128[extra_digits].w[0]))) {
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
	    && (Ql.w[1] < reciprocals10_128[extra_digits].w[1]
		|| (Ql.w[1] == reciprocals10_128[extra_digits].w[1]
		    && Ql.w[0] < reciprocals10_128[extra_digits].w[0])))
	  status = EXACT_STATUS;
	break;
      case ROUNDING_DOWN:
      case ROUNDING_TO_ZERO:
	if ((!Qh1.w[1]) && (!Qh1.w[0])
	    && (Ql.w[1] < reciprocals10_128[extra_digits].w[1]
		|| (Ql.w[1] == reciprocals10_128[extra_digits].w[1]
		    && Ql.w[0] < reciprocals10_128[extra_digits].w[0])))
	  status = EXACT_STATUS;
	break;
      default:
	// round up
	__add_carry_out (Stemp.w[0], cy, Ql.w[0],
			 reciprocals10_128[extra_digits].w[0]);
	__add_carry_in_out (Stemp.w[1], carry, Ql.w[1],
			    reciprocals10_128[extra_digits].w[1], cy);
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
	if (uf_check) {
	  status |= UNDERFLOW_EXCEPTION;
	}
#ifdef SET_STATUS_FLAGS
	__set_status_flags (pfpsf, status);
#endif
      }
    }

  }

  res =
    get_BID32 ((UINT32) (sign_x >> 32),
	       exponent_x - DECIMAL_EXPONENT_BIAS_128 +
	       DECIMAL_EXPONENT_BIAS_32, CX.w[0], rnd_mode, pfpsf);
  BID_RETURN_VAL (res);

}
