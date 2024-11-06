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
#include "bid_div_macros.h"


BID128_FUNCTION_ARG2_NORND_CUSTOMRESTYPE (UINT128, bid128_rem, x, y)

     UINT256 P256;
     UINT128 CX, CY, CX2, CQ, CR, T, CXS, P128, res;
     UINT64 sign_x, sign_y, valid_y;
     SINT64 D;
     int_float f64, fx;
     int exponent_x, exponent_y, diff_expon, bin_expon_cx, scale,
       scale0;

  // unpack arguments, check for NaN or Infinity

valid_y = unpack_BID128_value (&sign_y, &exponent_y, &CY, y);

if (!unpack_BID128_value (&sign_x, &exponent_x, &CX, x)) {
#ifdef SET_STATUS_FLAGS
if ((y.w[1] & SNAN_MASK64) == SNAN_MASK64)	// y is sNaN
  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
    // test if x is NaN
if ((x.w[1] & 0x7c00000000000000ull) == 0x7c00000000000000ull) {
#ifdef SET_STATUS_FLAGS
  if ((x.w[1] & SNAN_MASK64) == SNAN_MASK64)	// y is sNaN
    __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
  res.w[1] = CX.w[1] & QUIET_MASK64;
  res.w[0] = CX.w[0];
  BID_RETURN (res);
}
    // x is Infinity?
if ((x.w[1] & 0x7800000000000000ull) == 0x7800000000000000ull) {
  // check if y is Inf.
  if (((y.w[1] & 0x7c00000000000000ull) != 0x7c00000000000000ull))
    // return NaN
  {
#ifdef SET_STATUS_FLAGS
    // set status flags
    __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
    res.w[1] = 0x7c00000000000000ull;
    res.w[0] = 0;
    BID_RETURN (res);
  }

}
    // x is 0
if ((!CY.w[1]) && (!CY.w[0])) {
#ifdef SET_STATUS_FLAGS
  // set status flags
  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
  // x=y=0, return NaN
  res.w[1] = 0x7c00000000000000ull;
  res.w[0] = 0;
  BID_RETURN (res);
}
if (valid_y || ((y.w[1] & NAN_MASK64) == INFINITY_MASK64)) {
  // return 0
  if ((exponent_x > exponent_y)
      && ((y.w[1] & NAN_MASK64) != INFINITY_MASK64))
    exponent_x = exponent_y;

  res.w[1] = sign_x | (((UINT64) exponent_x) << 49);
  res.w[0] = 0;
  BID_RETURN (res);
}
}
if (!valid_y) {
  // y is Inf. or NaN

  // test if y is NaN
  if ((y.w[1] & 0x7c00000000000000ull) == 0x7c00000000000000ull) {
#ifdef SET_STATUS_FLAGS
    if ((y.w[1] & SNAN_MASK64) == SNAN_MASK64)	// y is sNaN
      __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
    res.w[1] = CY.w[1] & QUIET_MASK64;
    res.w[0] = CY.w[0];
    BID_RETURN (res);
  }
  // y is Infinity?
  if ((y.w[1] & 0x7800000000000000ull) == 0x7800000000000000ull) {
    // return x
    res.w[1] = x.w[1];
    res.w[0] = x.w[0];
    BID_RETURN (res);
  }
  // y is 0
#ifdef SET_STATUS_FLAGS
  // set status flags
  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
  res.w[1] = 0x7c00000000000000ull;
  res.w[0] = 0;
  BID_RETURN (res);
}

diff_expon = exponent_x - exponent_y;

if (diff_expon <= 0) {
  diff_expon = -diff_expon;

  if (diff_expon > 34) {
    // |x|<|y| in this case
    res = x;
    BID_RETURN (res);
  }
  // set exponent of y to exponent_x, scale coefficient_y
  T = power10_table_128[diff_expon];
  __mul_128x128_to_256 (P256, CY, T);

  if (P256.w[2] || P256.w[3]) {
    // |x|<|y| in this case
    res = x;
    BID_RETURN (res);
  }

  CX2.w[1] = (CX.w[1] << 1) | (CX.w[0] >> 63);
  CX2.w[0] = CX.w[0] << 1;
  if (__unsigned_compare_ge_128 (P256, CX2)) {
    // |x|<|y| in this case
    res = x;
    BID_RETURN (res);
  }

  P128.w[0] = P256.w[0];
  P128.w[1] = P256.w[1];
  __div_128_by_128 (&CQ, &CR, CX, P128);

  CX2.w[1] = (CR.w[1] << 1) | (CR.w[0] >> 63);
  CX2.w[0] = CR.w[0] << 1;
  if ((__unsigned_compare_gt_128 (CX2, P256))
      || (CX2.w[1] == P256.w[1] && CX2.w[0] == P256.w[0]
	  && (CQ.w[0] & 1))) {
    __sub_128_128 (CR, P256, CR);
    sign_x ^= 0x8000000000000000ull;
  }

  get_BID128_very_fast (&res, sign_x, exponent_x, CR);
  BID_RETURN (res);
}
  // 2^64
f64.i = 0x5f800000;

scale0 = 38;
if (!CY.w[1])
  scale0 = 34;

while (diff_expon > 0) {
  // get number of digits in CX and scale=38-digits
  // fx ~ CX
  fx.d = (float) CX.w[1] * f64.d + (float) CX.w[0];
  bin_expon_cx = ((fx.i >> 23) & 0xff) - 0x7f;
  scale = scale0 - estimate_decimal_digits[bin_expon_cx];
  // scale = 38-estimate_decimal_digits[bin_expon_cx];
  D = CX.w[1] - power10_index_binexp_128[bin_expon_cx].w[1];
  if (D > 0
      || (!D && CX.w[0] >= power10_index_binexp_128[bin_expon_cx].w[0]))
    scale--;

  if (diff_expon >= scale)
    diff_expon -= scale;
  else {
    scale = diff_expon;
    diff_expon = 0;
  }

  T = power10_table_128[scale];
  __mul_128x128_low (CXS, CX, T);

  __div_128_by_128 (&CQ, &CX, CXS, CY);

  // check for remainder == 0
  if (!CX.w[1] && !CX.w[0]) {
    get_BID128_very_fast (&res, sign_x, exponent_y, CX);
    BID_RETURN (res);
  }
}

CX2.w[1] = (CX.w[1] << 1) | (CX.w[0] >> 63);
CX2.w[0] = CX.w[0] << 1;
if ((__unsigned_compare_gt_128 (CX2, CY))
    || (CX2.w[1] == CY.w[1] && CX2.w[0] == CY.w[0] && (CQ.w[0] & 1))) {
  __sub_128_128 (CX, CY, CX);
  sign_x ^= 0x8000000000000000ull;
}

get_BID128_very_fast (&res, sign_x, exponent_y, CX);
BID_RETURN (res);
}
