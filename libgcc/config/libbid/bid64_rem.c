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

/*****************************************************************************
 *    BID64 remainder
 *****************************************************************************
 *
 *  Algorithm description:
 *
 *  if(exponent_x < exponent_y)
 *    scale coefficient_y so exponents are aligned
 *    perform coefficient divide (64-bit integer divide), unless
 *            coefficient_y is longer than 64 bits (clearly larger 
 *                                               than coefficient_x) 
 *  else  // exponent_x > exponent_y
 *     use a loop to scale coefficient_x to 18_digits, divide by 
 *         coefficient_y (64-bit integer divide), calculate remainder
 *         as new_coefficient_x and repeat until final remainder is obtained 
 *         (when new_exponent_x < exponent_y)
 *
 ****************************************************************************/

#include "bid_internal.h"

#define MAX_FORMAT_DIGITS     16
#define DECIMAL_EXPONENT_BIAS 398
#define MASK_BINARY_EXPONENT  0x7ff0000000000000ull
#define BINARY_EXPONENT_BIAS  0x3ff
#define UPPER_EXPON_LIMIT     51

#if DECIMAL_CALL_BY_REFERENCE

void
bid64_rem (UINT64 * pres, UINT64 * px,
	   UINT64 *
	   py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  UINT64 x, y;
#else

UINT64
bid64_rem (UINT64 x,
	   UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT128 CY;
  UINT64 sign_x, sign_y, coefficient_x, coefficient_y, res;
  UINT64 Q, R, R2, T, valid_y, valid_x;
  int_float tempx;
  int exponent_x, exponent_y, bin_expon, e_scale;
  int digits_x, diff_expon;

#if DECIMAL_CALL_BY_REFERENCE
  x = *px;
  y = *py;
#endif

  valid_y = unpack_BID64 (&sign_y, &exponent_y, &coefficient_y, y);
  valid_x = unpack_BID64 (&sign_x, &exponent_x, &coefficient_x, x);

  // unpack arguments, check for NaN or Infinity
  if (!valid_x) {
    // x is Inf. or NaN or 0
#ifdef SET_STATUS_FLAGS
    if ((y & SNAN_MASK64) == SNAN_MASK64)	// y is sNaN
      __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif

    // test if x is NaN
    if ((x & 0x7c00000000000000ull) == 0x7c00000000000000ull) {
#ifdef SET_STATUS_FLAGS
      if (((x & SNAN_MASK64) == SNAN_MASK64))
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      res = coefficient_x & QUIET_MASK64;;
      BID_RETURN (res);
    }
    // x is Infinity?
    if ((x & 0x7800000000000000ull) == 0x7800000000000000ull) {
      if (((y & NAN_MASK64) != NAN_MASK64)) {
#ifdef SET_STATUS_FLAGS
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
	// return NaN
	res = 0x7c00000000000000ull;
	BID_RETURN (res);
      }
    }
    // x is 0
    // return x if y != 0
    if (((y & 0x7800000000000000ull) < 0x7800000000000000ull) &&
	coefficient_y) {
      if ((y & 0x6000000000000000ull) == 0x6000000000000000ull)
	exponent_y = (y >> 51) & 0x3ff;
      else
	exponent_y = (y >> 53) & 0x3ff;

      if (exponent_y < exponent_x)
	exponent_x = exponent_y;

      x = exponent_x;
      x <<= 53;

      res = x | sign_x;
      BID_RETURN (res);
    }

  }
  if (!valid_y) {
    // y is Inf. or NaN

    // test if y is NaN
    if ((y & 0x7c00000000000000ull) == 0x7c00000000000000ull) {
#ifdef SET_STATUS_FLAGS
      if (((y & SNAN_MASK64) == SNAN_MASK64))
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      res = coefficient_y & QUIET_MASK64;;
      BID_RETURN (res);
    }
    // y is Infinity?
    if ((y & 0x7800000000000000ull) == 0x7800000000000000ull) {
      res = very_fast_get_BID64 (sign_x, exponent_x, coefficient_x);
      BID_RETURN (res);
    }
    // y is 0, return NaN
    {
#ifdef SET_STATUS_FLAGS
      __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      res = 0x7c00000000000000ull;
      BID_RETURN (res);
    }
  }


  diff_expon = exponent_x - exponent_y;
  if (diff_expon <= 0) {
    diff_expon = -diff_expon;

    if (diff_expon > 16) {
      // |x|<|y| in this case
      res = x;
      BID_RETURN (res);
    }
    // set exponent of y to exponent_x, scale coefficient_y
    T = power10_table_128[diff_expon].w[0];
    __mul_64x64_to_128 (CY, coefficient_y, T);

    if (CY.w[1] || CY.w[0] > (coefficient_x << 1)) {
      res = x;
      BID_RETURN (res);
    }

    Q = coefficient_x / CY.w[0];
    R = coefficient_x - Q * CY.w[0];

    R2 = R + R;
    if (R2 > CY.w[0] || (R2 == CY.w[0] && (Q & 1))) {
      R = CY.w[0] - R;
      sign_x ^= 0x8000000000000000ull;
    }

    res = very_fast_get_BID64 (sign_x, exponent_x, R);
    BID_RETURN (res);
  }


  while (diff_expon > 0) {
    // get number of digits in coeff_x
    tempx.d = (float) coefficient_x;
    bin_expon = ((tempx.i >> 23) & 0xff) - 0x7f;
    digits_x = estimate_decimal_digits[bin_expon];
    // will not use this test, dividend will have 18 or 19 digits
    //if(coefficient_x >= power10_table_128[digits_x].w[0])
    //      digits_x++;

    e_scale = 18 - digits_x;
    if (diff_expon >= e_scale) {
      diff_expon -= e_scale;
    } else {
      e_scale = diff_expon;
      diff_expon = 0;
    }

    // scale dividend to 18 or 19 digits
    coefficient_x *= power10_table_128[e_scale].w[0];

    // quotient
    Q = coefficient_x / coefficient_y;
    // remainder
    coefficient_x -= Q * coefficient_y;

    // check for remainder == 0
    if (!coefficient_x) {
      res = very_fast_get_BID64_small_mantissa (sign_x, exponent_y, 0);
      BID_RETURN (res);
    }
  }

  R2 = coefficient_x + coefficient_x;
  if (R2 > coefficient_y || (R2 == coefficient_y && (Q & 1))) {
    coefficient_x = coefficient_y - coefficient_x;
    sign_x ^= 0x8000000000000000ull;
  }

  res = very_fast_get_BID64 (sign_x, exponent_y, coefficient_x);
  BID_RETURN (res);

}
