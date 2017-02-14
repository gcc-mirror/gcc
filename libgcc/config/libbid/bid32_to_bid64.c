/* Copyright (C) 2007-2017 Free Software Foundation, Inc.

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

#include "bid_internal.h"

/*
 * Takes a BID32 as input and converts it to a BID64 and returns it.
 */
TYPE0_FUNCTION_ARGTYPE1_NORND (UINT64, bid32_to_bid64, UINT32, x)

     UINT64 res;
     UINT32 sign_x;
     int exponent_x;
     UINT32 coefficient_x;

if (!unpack_BID32 (&sign_x, &exponent_x, &coefficient_x, x)) {
    // Inf, NaN, 0
if (((x) & 0x78000000) == 0x78000000) {
  if (((x) & 0x7e000000) == 0x7e000000) {	// sNaN
#ifdef SET_STATUS_FLAGS
    __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
  }
  res = (coefficient_x & 0x000fffff);
  res *= 1000000000;
  res |= ((((UINT64) coefficient_x) << 32) & 0xfc00000000000000ull);

  BID_RETURN (res);
}
}

res =
very_fast_get_BID64_small_mantissa (((UINT64) sign_x) << 32,
				    exponent_x +
				    DECIMAL_EXPONENT_BIAS -
				    DECIMAL_EXPONENT_BIAS_32,
				    (UINT64) coefficient_x);
BID_RETURN (res);
}	// convert_bid32_to_bid64


/*
 * Takes a BID64 as input and converts it to a BID32 and returns it.
 */
#if DECIMAL_CALL_BY_REFERENCE

void
bid64_to_bid32 (UINT32 * pres,
		UINT64 *
		px _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		_EXC_INFO_PARAM) {
  UINT64 x;
#else

UINT32
bid64_to_bid32 (UINT64 x _RND_MODE_PARAM _EXC_FLAGS_PARAM
		_EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT128 Q;
  UINT64 sign_x, coefficient_x, remainder_h, carry, Stemp;
  UINT32 res;
  int_float tempx;
  int exponent_x, bin_expon_cx, extra_digits, rmode = 0, amount;
  unsigned status = 0;

#if DECIMAL_CALL_BY_REFERENCE
#if !DECIMAL_GLOBAL_ROUNDING
  _IDEC_round rnd_mode = *prnd_mode;
#endif
  x = *px;
#endif

  // unpack arguments, check for NaN or Infinity, 0
  if (!unpack_BID64 (&sign_x, &exponent_x, &coefficient_x, x)) {
    if (((x) & 0x7800000000000000ull) == 0x7800000000000000ull) {
      res = (coefficient_x & 0x0003ffffffffffffull);
      res /= 1000000000ull;
      res |= ((coefficient_x >> 32) & 0xfc000000);
#ifdef SET_STATUS_FLAGS
      if ((x & SNAN_MASK64) == SNAN_MASK64)	// sNaN
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      BID_RETURN (res);
    }
    exponent_x =
      exponent_x - DECIMAL_EXPONENT_BIAS + DECIMAL_EXPONENT_BIAS_32;
    if (exponent_x < 0)
      exponent_x = 0;
    if (exponent_x > DECIMAL_MAX_EXPON_32)
      exponent_x = DECIMAL_MAX_EXPON_32;
    res = (sign_x >> 32) | (exponent_x << 23);
    BID_RETURN (res);
  }

  exponent_x =
    exponent_x - DECIMAL_EXPONENT_BIAS + DECIMAL_EXPONENT_BIAS_32;

  // check number of digits
  if (coefficient_x >= 10000000) {
    tempx.d = (float) coefficient_x;
    bin_expon_cx = ((tempx.i >> 23) & 0xff) - 0x7f;
    extra_digits = estimate_decimal_digits[bin_expon_cx] - 7;
    // add test for range
    if (coefficient_x >= power10_index_binexp[bin_expon_cx])
      extra_digits++;

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

    exponent_x += extra_digits;
    if ((exponent_x < 0) && (exponent_x + MAX_FORMAT_DIGITS_32 >= 0)) {
      status = UNDERFLOW_EXCEPTION;
      if (exponent_x == -1)
	if (coefficient_x + round_const_table[rmode][extra_digits] >=
	    power10_table_128[extra_digits + 7].w[0])
	  status = 0;
      extra_digits -= exponent_x;
      exponent_x = 0;
    }
    coefficient_x += round_const_table[rmode][extra_digits];
    __mul_64x64_to_128 (Q, coefficient_x,
			reciprocals10_64[extra_digits]);

    // now get P/10^extra_digits: shift Q_high right by M[extra_digits]-128
    amount = short_recip_scale[extra_digits];

    coefficient_x = Q.w[1] >> amount;

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
    if (rmode == 0)	//ROUNDING_TO_NEAREST
#endif
      if (coefficient_x & 1) {
	// check whether fractional part of initial_P/10^extra_digits 
	// is exactly .5

	// get remainder
	remainder_h = Q.w[1] << (64 - amount);

	if (!remainder_h && (Q.w[0] < reciprocals10_64[extra_digits]))
	  coefficient_x--;
      }
#endif

#ifdef SET_STATUS_FLAGS

    {
      status |= INEXACT_EXCEPTION;
      // get remainder
      remainder_h = Q.w[1] << (64 - amount);

      switch (rmode) {
      case ROUNDING_TO_NEAREST:
      case ROUNDING_TIES_AWAY:
	// test whether fractional part is 0
	if (remainder_h == 0x8000000000000000ull
	    && (Q.w[0] < reciprocals10_64[extra_digits]))
	  status = EXACT_STATUS;
	break;
      case ROUNDING_DOWN:
      case ROUNDING_TO_ZERO:
	if (!remainder_h && (Q.w[0] < reciprocals10_64[extra_digits]))
	  status = EXACT_STATUS;
	break;
      default:
	// round up
	__add_carry_out (Stemp, carry, Q.w[0],
			 reciprocals10_64[extra_digits]);
	if ((remainder_h >> (64 - amount)) + carry >=
	    (((UINT64) 1) << amount))
	  status = EXACT_STATUS;
      }

      if (status != EXACT_STATUS)
	__set_status_flags (pfpsf, status);
    }

#endif

  }

  res =
    get_BID32 ((UINT32) (sign_x >> 32),
	       exponent_x, coefficient_x, rnd_mode, pfpsf);
  BID_RETURN (res);

}
