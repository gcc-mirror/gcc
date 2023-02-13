/* Copyright (C) 2007-2023 Free Software Foundation, Inc.

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

/*****************************************************************************
 *    BID64 multiply
 *****************************************************************************
 *
 *  Algorithm description:
 *
 *  if(number_digits(coefficient_x)+number_digits(coefficient_y) guaranteed
 *       below 16)
 *      return get_BID64(sign_x^sign_y, exponent_x + exponent_y - dec_bias,
 *                     coefficient_x*coefficient_y)
 *  else
 *      get long product: coefficient_x*coefficient_y
 *      determine number of digits to round off (extra_digits)
 *      rounding is performed as a 128x128-bit multiplication by 
 *         2^M[extra_digits]/10^extra_digits, followed by a shift
 *         M[extra_digits] is sufficiently large for required accuracy 
 *
 ****************************************************************************/

#include "bid_internal.h"

#if DECIMAL_CALL_BY_REFERENCE

void
bid64_mul (UINT64 * pres, UINT64 * px,
	   UINT64 *
	   py _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	   _EXC_INFO_PARAM) {
  UINT64 x, y;
#else

UINT64
bid64_mul (UINT64 x,
	   UINT64 y _RND_MODE_PARAM _EXC_FLAGS_PARAM
	   _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT128 P, PU, C128, Q_high, Q_low, Stemp;
  UINT64 sign_x, sign_y, coefficient_x, coefficient_y;
  UINT64 C64, remainder_h, carry, CY, res;
  UINT64 valid_x, valid_y;
  int_double tempx, tempy;
  int extra_digits, exponent_x, exponent_y, bin_expon_cx, bin_expon_cy,
    bin_expon_product;
  int rmode, digits_p, bp, amount, amount2, final_exponent, round_up;
  unsigned status, uf_status;

#if DECIMAL_CALL_BY_REFERENCE
#if !DECIMAL_GLOBAL_ROUNDING
  _IDEC_round rnd_mode = *prnd_mode;
#endif
  x = *px;
  y = *py;
#endif

  valid_x = unpack_BID64 (&sign_x, &exponent_x, &coefficient_x, x);
  valid_y = unpack_BID64 (&sign_y, &exponent_y, &coefficient_y, y);

  // unpack arguments, check for NaN or Infinity
  if (!valid_x) {

#ifdef SET_STATUS_FLAGS
    if ((y & SNAN_MASK64) == SNAN_MASK64)	// y is sNaN
      __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
    // x is Inf. or NaN

    // test if x is NaN
    if ((x & NAN_MASK64) == NAN_MASK64) {
#ifdef SET_STATUS_FLAGS
      if ((x & SNAN_MASK64) == SNAN_MASK64)	// sNaN
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      BID_RETURN (coefficient_x & QUIET_MASK64);
    }
    // x is Infinity?
    if ((x & INFINITY_MASK64) == INFINITY_MASK64) {
      // check if y is 0
      if (((y & INFINITY_MASK64) != INFINITY_MASK64)
	  && !coefficient_y) {
#ifdef SET_STATUS_FLAGS
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
	// y==0 , return NaN
	BID_RETURN (NAN_MASK64);
      }
      // check if y is NaN
      if ((y & NAN_MASK64) == NAN_MASK64)
	// y==NaN , return NaN
	BID_RETURN (coefficient_y & QUIET_MASK64);
      // otherwise return +/-Inf
      BID_RETURN (((x ^ y) & 0x8000000000000000ull) | INFINITY_MASK64);
    }
    // x is 0
    if (((y & INFINITY_MASK64) != INFINITY_MASK64)) {
      if ((y & SPECIAL_ENCODING_MASK64) == SPECIAL_ENCODING_MASK64)
	exponent_y = ((UINT32) (y >> 51)) & 0x3ff;
      else
	exponent_y = ((UINT32) (y >> 53)) & 0x3ff;
      sign_y = y & 0x8000000000000000ull;

      exponent_x += exponent_y - DECIMAL_EXPONENT_BIAS;
      if (exponent_x > DECIMAL_MAX_EXPON_64)
	exponent_x = DECIMAL_MAX_EXPON_64;
      else if (exponent_x < 0)
	exponent_x = 0;
      BID_RETURN ((sign_x ^ sign_y) | (((UINT64) exponent_x) << 53));
    }
  }
  if (!valid_y) {
    // y is Inf. or NaN

    // test if y is NaN
    if ((y & NAN_MASK64) == NAN_MASK64) {
#ifdef SET_STATUS_FLAGS
      if ((y & SNAN_MASK64) == SNAN_MASK64)	// sNaN
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      BID_RETURN (coefficient_y & QUIET_MASK64);
    }
    // y is Infinity?
    if ((y & INFINITY_MASK64) == INFINITY_MASK64) {
      // check if x is 0
      if (!coefficient_x) {
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
	// x==0, return NaN
	BID_RETURN (NAN_MASK64);
      }
      // otherwise return +/-Inf
      BID_RETURN (((x ^ y) & 0x8000000000000000ull) | INFINITY_MASK64);
    }
    // y is 0
    exponent_x += exponent_y - DECIMAL_EXPONENT_BIAS;
    if (exponent_x > DECIMAL_MAX_EXPON_64)
      exponent_x = DECIMAL_MAX_EXPON_64;
    else if (exponent_x < 0)
      exponent_x = 0;
    BID_RETURN ((sign_x ^ sign_y) | (((UINT64) exponent_x) << 53));
  }
  //--- get number of bits in the coefficients of x and y ---
  // version 2 (original)
  tempx.d = (double) coefficient_x;
  bin_expon_cx = ((tempx.i & MASK_BINARY_EXPONENT) >> 52);
  tempy.d = (double) coefficient_y;
  bin_expon_cy = ((tempy.i & MASK_BINARY_EXPONENT) >> 52);

  // magnitude estimate for coefficient_x*coefficient_y is 
  //        2^(unbiased_bin_expon_cx + unbiased_bin_expon_cx)
  bin_expon_product = bin_expon_cx + bin_expon_cy;

  // check if coefficient_x*coefficient_y<2^(10*k+3)
  // equivalent to unbiased_bin_expon_cx + unbiased_bin_expon_cx < 10*k+1
  if (bin_expon_product < UPPER_EXPON_LIMIT + 2 * BINARY_EXPONENT_BIAS) {
    //  easy multiply
    C64 = coefficient_x * coefficient_y;

    res =
      get_BID64_small_mantissa (sign_x ^ sign_y,
				exponent_x + exponent_y -
				DECIMAL_EXPONENT_BIAS, C64, rnd_mode,
				pfpsf);
    BID_RETURN (res);
  } else {
    uf_status = 0;
    // get 128-bit product: coefficient_x*coefficient_y
    __mul_64x64_to_128 (P, coefficient_x, coefficient_y);

    // tighten binary range of P:  leading bit is 2^bp
    // unbiased_bin_expon_product <= bp <= unbiased_bin_expon_product+1
    bin_expon_product -= 2 * BINARY_EXPONENT_BIAS;

    __tight_bin_range_128 (bp, P, bin_expon_product);

    // get number of decimal digits in the product
    digits_p = estimate_decimal_digits[bp];
    if (!(__unsigned_compare_gt_128 (power10_table_128[digits_p], P)))
      digits_p++;	// if power10_table_128[digits_p] <= P

    // determine number of decimal digits to be rounded out
    extra_digits = digits_p - MAX_FORMAT_DIGITS;
    final_exponent =
      exponent_x + exponent_y + extra_digits - DECIMAL_EXPONENT_BIAS;

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
    rmode = rnd_mode;
    if (sign_x ^ sign_y && (unsigned) (rmode - 1) < 2)
      rmode = 3 - rmode;
#else
    rmode = 0;
#endif
#else
    rmode = 0;
#endif

    round_up = 0;
    if (((unsigned) final_exponent) >= 3 * 256) {
      if (final_exponent < 0) {
	// underflow
	if (final_exponent + 16 < 0) {
	  res = sign_x ^ sign_y;
	  __set_status_flags (pfpsf,
			      UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION);
	  if (rmode == ROUNDING_UP)
	    res |= 1;
	  BID_RETURN (res);
	}

	uf_status = UNDERFLOW_EXCEPTION;
	if (final_exponent == -1) {
	  __add_128_64 (PU, P, round_const_table[rmode][extra_digits]);
	  if (__unsigned_compare_ge_128
	      (PU, power10_table_128[extra_digits + 16]))
	    uf_status = 0;
	}
	extra_digits -= final_exponent;
	final_exponent = 0;

	if (extra_digits > 17) {
	  __mul_128x128_full (Q_high, Q_low, P, reciprocals10_128[16]);

	  amount = recip_scale[16];
	  __shr_128 (P, Q_high, amount);

	  // get sticky bits
	  amount2 = 64 - amount;
	  remainder_h = 0;
	  remainder_h--;
	  remainder_h >>= amount2;
	  remainder_h = remainder_h & Q_high.w[0];

	  extra_digits -= 16;
	  if (remainder_h || (Q_low.w[1] > reciprocals10_128[16].w[1]
			      || (Q_low.w[1] ==
				  reciprocals10_128[16].w[1]
				  && Q_low.w[0] >=
				  reciprocals10_128[16].w[0]))) {
	    round_up = 1;
	    __set_status_flags (pfpsf,
				UNDERFLOW_EXCEPTION |
				INEXACT_EXCEPTION);
	    P.w[0] = (P.w[0] << 3) + (P.w[0] << 1);
	    P.w[0] |= 1;
	    extra_digits++;
	  }
	}
      } else {
	res =
	  fast_get_BID64_check_OF (sign_x ^ sign_y, final_exponent,
				   1000000000000000ull, rnd_mode,
				   pfpsf);
	BID_RETURN (res);
      }
    }


    if (extra_digits > 0) {
      // will divide by 10^(digits_p - 16)

      // add a constant to P, depending on rounding mode
      // 0.5*10^(digits_p - 16) for round-to-nearest
      __add_128_64 (P, P, round_const_table[rmode][extra_digits]);

      // get P*(2^M[extra_digits])/10^extra_digits
      __mul_128x128_full (Q_high, Q_low, P,
			  reciprocals10_128[extra_digits]);

      // now get P/10^extra_digits: shift Q_high right by M[extra_digits]-128
      amount = recip_scale[extra_digits];
      __shr_128 (C128, Q_high, amount);

      C64 = __low_64 (C128);

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
      if (rmode == 0)	//ROUNDING_TO_NEAREST
#endif
	if ((C64 & 1) && !round_up) {
	  // check whether fractional part of initial_P/10^extra_digits 
	  // is exactly .5
	  // this is the same as fractional part of 
	  // (initial_P + 0.5*10^extra_digits)/10^extra_digits is exactly zero

	  // get remainder
	  remainder_h = Q_high.w[0] << (64 - amount);

	  // test whether fractional part is 0
	  if (!remainder_h
	      && (Q_low.w[1] < reciprocals10_128[extra_digits].w[1]
		  || (Q_low.w[1] == reciprocals10_128[extra_digits].w[1]
		      && Q_low.w[0] <
		      reciprocals10_128[extra_digits].w[0]))) {
	    C64--;
	  }
	}
#endif

#ifdef SET_STATUS_FLAGS
      status = INEXACT_EXCEPTION | uf_status;

      // get remainder
      remainder_h = Q_high.w[0] << (64 - amount);

      switch (rmode) {
      case ROUNDING_TO_NEAREST:
      case ROUNDING_TIES_AWAY:
	// test whether fractional part is 0
	if (remainder_h == 0x8000000000000000ull
	    && (Q_low.w[1] < reciprocals10_128[extra_digits].w[1]
		|| (Q_low.w[1] == reciprocals10_128[extra_digits].w[1]
		    && Q_low.w[0] <
		    reciprocals10_128[extra_digits].w[0])))
	  status = EXACT_STATUS;
	break;
      case ROUNDING_DOWN:
      case ROUNDING_TO_ZERO:
	if (!remainder_h
	    && (Q_low.w[1] < reciprocals10_128[extra_digits].w[1]
		|| (Q_low.w[1] == reciprocals10_128[extra_digits].w[1]
		    && Q_low.w[0] <
		    reciprocals10_128[extra_digits].w[0])))
	  status = EXACT_STATUS;
	break;
      default:
	// round up
	__add_carry_out (Stemp.w[0], CY, Q_low.w[0],
			 reciprocals10_128[extra_digits].w[0]);
	__add_carry_in_out (Stemp.w[1], carry, Q_low.w[1],
			    reciprocals10_128[extra_digits].w[1], CY);
	if ((remainder_h >> (64 - amount)) + carry >=
	    (((UINT64) 1) << amount))
	  status = EXACT_STATUS;
      }

      __set_status_flags (pfpsf, status);
#endif

      // convert to BID and return
      res =
	fast_get_BID64_check_OF (sign_x ^ sign_y, final_exponent, C64,
				 rmode, pfpsf);
      BID_RETURN (res);
    }
    // go to convert_format and exit
    C64 = __low_64 (P);
    res =
      get_BID64 (sign_x ^ sign_y,
		 exponent_x + exponent_y - DECIMAL_EXPONENT_BIAS, C64,
		 rmode, pfpsf);
    BID_RETURN (res);
  }
}
