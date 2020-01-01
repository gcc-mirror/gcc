/* Copyright (C) 2007-2020 Free Software Foundation, Inc.

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

BID128_FUNCTION_ARG2 (bid128_quantize, x, y)

     UINT256 CT;
     UINT128 CX, CY, T, CX2, CR, Stemp, res, REM_H, C2N;
     UINT64 sign_x, sign_y, remainder_h, carry, CY64, valid_x;
     int_float tempx;
     int exponent_x, exponent_y, digits_x, extra_digits, amount;
     int expon_diff, total_digits, bin_expon_cx, rmode, status;

valid_x = unpack_BID128_value (&sign_x, &exponent_x, &CX, x);

  // unpack arguments, check for NaN or Infinity
if (!unpack_BID128_value (&sign_y, &exponent_y, &CY, y)) {
    // y is Inf. or NaN
#ifdef SET_STATUS_FLAGS
if ((x.w[1] & SNAN_MASK64) == SNAN_MASK64)	// y is sNaN
  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif

    // test if y is NaN
if ((y.w[1] & 0x7c00000000000000ull) == 0x7c00000000000000ull) {
#ifdef SET_STATUS_FLAGS
  if ((y.w[1] & 0x7e00000000000000ull) == 0x7e00000000000000ull) {
    // set status flags
    __set_status_flags (pfpsf, INVALID_EXCEPTION);
  }
#endif
  if ((x.w[1] & 0x7c00000000000000ull) != 0x7c00000000000000ull) {
    res.w[1] = CY.w[1] & QUIET_MASK64;
    res.w[0] = CY.w[0];
  } else {
    res.w[1] = CX.w[1] & QUIET_MASK64;
    res.w[0] = CX.w[0];
  }
  BID_RETURN (res);
}
    // y is Infinity?
if ((y.w[1] & 0x7800000000000000ull) == 0x7800000000000000ull) {
  // check if x is not Inf.
  if (((x.w[1] & 0x7c00000000000000ull) < 0x7800000000000000ull)) {
    // return NaN 
#ifdef SET_STATUS_FLAGS
    // set status flags
    __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
    res.w[1] = 0x7c00000000000000ull;
    res.w[0] = 0;
    BID_RETURN (res);
  } else
    if (((x.w[1] & 0x7c00000000000000ull) <= 0x7800000000000000ull)) {
    res.w[1] = CX.w[1] & QUIET_MASK64;
    res.w[0] = CX.w[0];
    BID_RETURN (res);
  }
}

}

if (!valid_x) {
  // test if x is NaN or Inf
  if ((x.w[1] & 0x7c00000000000000ull) == 0x7800000000000000ull) {
#ifdef SET_STATUS_FLAGS
    // set status flags
    __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
    res.w[1] = 0x7c00000000000000ull;
    res.w[0] = 0;
    BID_RETURN (res);
  } else if ((x.w[1] & 0x7c00000000000000ull) == 0x7c00000000000000ull) {
    if ((x.w[1] & 0x7e00000000000000ull) == 0x7e00000000000000ull) {
#ifdef SET_STATUS_FLAGS
      // set status flags
      __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
    }
    res.w[1] = CX.w[1] & QUIET_MASK64;
    res.w[0] = CX.w[0];
    BID_RETURN (res);
  }
  if (!CX.w[1] && !CX.w[0]) {
    get_BID128_very_fast (&res, sign_x, exponent_y, CX);
    BID_RETURN (res);
  }
}
  // get number of decimal digits in coefficient_x
if (CX.w[1]) {
  tempx.d = (float) CX.w[1];
  bin_expon_cx = ((tempx.i >> 23) & 0xff) - 0x7f + 64;
} else {
  tempx.d = (float) CX.w[0];
  bin_expon_cx = ((tempx.i >> 23) & 0xff) - 0x7f;
}

digits_x = estimate_decimal_digits[bin_expon_cx];
if (CX.w[1] > power10_table_128[digits_x].w[1]
    || (CX.w[1] == power10_table_128[digits_x].w[1]
	&& CX.w[0] >= power10_table_128[digits_x].w[0]))
  digits_x++;

expon_diff = exponent_x - exponent_y;
total_digits = digits_x + expon_diff;

if ((UINT32) total_digits <= 34) {
  if (expon_diff >= 0) {
    T = power10_table_128[expon_diff];
    __mul_128x128_low (CX2, T, CX);
    get_BID128_very_fast (&res, sign_x, exponent_y, CX2);
    BID_RETURN (res);
  }
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
  // must round off -expon_diff digits
  extra_digits = -expon_diff;
  __add_128_128 (CX, CX, round_const_table_128[rmode][extra_digits]);

  // get P*(2^M[extra_digits])/10^extra_digits
  __mul_128x128_to_256 (CT, CX, reciprocals10_128[extra_digits]);

  // now get P/10^extra_digits: shift C64 right by M[extra_digits]-128
  amount = recip_scale[extra_digits];
  CX2.w[0] = CT.w[2];
  CX2.w[1] = CT.w[3];
  if (amount >= 64) {
    CR.w[1] = 0;
    CR.w[0] = CX2.w[1] >> (amount - 64);
  } else {
    __shr_128 (CR, CX2, amount);
  }

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
  if (rnd_mode == 0)
#endif
    if (CR.w[0] & 1) {
      // check whether fractional part of initial_P/10^extra_digits is 
      // exactly .5 this is the same as fractional part of 
      // (initial_P + 0.5*10^extra_digits)/10^extra_digits is exactly zero

      // get remainder
      if (amount >= 64) {
	remainder_h = CX2.w[0] | (CX2.w[1] << (128 - amount));
      } else
	remainder_h = CX2.w[0] << (64 - amount);

      // test whether fractional part is 0
      if (!remainder_h
	  && (CT.w[1] < reciprocals10_128[extra_digits].w[1]
	      || (CT.w[1] == reciprocals10_128[extra_digits].w[1]
		  && CT.w[0] < reciprocals10_128[extra_digits].w[0]))) {
	CR.w[0]--;
      }
    }
#endif

#ifdef SET_STATUS_FLAGS
  status = INEXACT_EXCEPTION;

  // get remainder
  if (amount >= 64) {
    REM_H.w[1] = (CX2.w[1] << (128 - amount));
    REM_H.w[0] = CX2.w[0];
  } else {
    REM_H.w[1] = CX2.w[0] << (64 - amount);
    REM_H.w[0] = 0;
  }

  switch (rmode) {
  case ROUNDING_TO_NEAREST:
  case ROUNDING_TIES_AWAY:
    // test whether fractional part is 0
    if (REM_H.w[1] == 0x8000000000000000ull && !REM_H.w[0]
	&& (CT.w[1] < reciprocals10_128[extra_digits].w[1]
	    || (CT.w[1] == reciprocals10_128[extra_digits].w[1]
		&& CT.w[0] < reciprocals10_128[extra_digits].w[0])))
      status = EXACT_STATUS;
    break;
  case ROUNDING_DOWN:
  case ROUNDING_TO_ZERO:
    if (!(REM_H.w[1] | REM_H.w[0])
	&& (CT.w[1] < reciprocals10_128[extra_digits].w[1]
	    || (CT.w[1] == reciprocals10_128[extra_digits].w[1]
		&& CT.w[0] < reciprocals10_128[extra_digits].w[0])))
      status = EXACT_STATUS;
    break;
  default:
    // round up
    __add_carry_out (Stemp.w[0], CY64, CT.w[0],
		     reciprocals10_128[extra_digits].w[0]);
    __add_carry_in_out (Stemp.w[1], carry, CT.w[1],
			reciprocals10_128[extra_digits].w[1], CY64);
    if (amount < 64) {
      C2N.w[1] = 0;
      C2N.w[0] = ((UINT64) 1) << amount;
      REM_H.w[0] = REM_H.w[1] >> (64 - amount);
      REM_H.w[1] = 0;
    } else {
      C2N.w[1] = ((UINT64) 1) << (amount - 64);
      C2N.w[0] = 0;
      REM_H.w[1] >>= (128 - amount);
    }
    REM_H.w[0] += carry;
    if (REM_H.w[0] < carry)
      REM_H.w[1]++;
    if (__unsigned_compare_ge_128 (REM_H, C2N))
      status = EXACT_STATUS;
  }

  __set_status_flags (pfpsf, status);

#endif
  get_BID128_very_fast (&res, sign_x, exponent_y, CR);
  BID_RETURN (res);
}
if (total_digits < 0) {
  CR.w[1] = CR.w[0] = 0;
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
  rmode = rnd_mode;
  if (sign_x && (unsigned) (rmode - 1) < 2)
    rmode = 3 - rmode;
  if (rmode == ROUNDING_UP)
    CR.w[0] = 1;
#endif
#endif
#ifdef SET_STATUS_FLAGS
  __set_status_flags (pfpsf, INEXACT_EXCEPTION);
#endif
  get_BID128_very_fast (&res, sign_x, exponent_y, CR);
  BID_RETURN (res);
}
  // else  more than 34 digits in coefficient
#ifdef SET_STATUS_FLAGS
__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
res.w[1] = 0x7c00000000000000ull;
res.w[0] = 0;
BID_RETURN (res);

}
