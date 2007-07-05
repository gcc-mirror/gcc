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
 *    BID64 square root
 *****************************************************************************
 *
 *  Algorithm description:
 *
 *  if(exponent_x is odd)
 *     scale coefficient_x by 10, adjust exponent
 *  - get lower estimate for number of digits in coefficient_x
 *  - scale coefficient x to between 31 and 33 decimal digits
 *  - in parallel, check for exact case and return if true
 *  - get high part of result coefficient using double precision sqrt
 *  - compute remainder and refine coefficient in one iteration (which 
 *                                 modifies it by at most 1)
 *  - result exponent is easy to compute from the adjusted arg. exponent 
 *
 ****************************************************************************/

#include "bid_internal.h"

extern double sqrt (double);

#if DECIMAL_CALL_BY_REFERENCE

void
__bid64_sqrt (UINT64 * pres,
	    UINT64 *
	    px _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	    _EXC_INFO_PARAM) {
  UINT64 x;
#else

UINT64
__bid64_sqrt (UINT64 x _RND_MODE_PARAM _EXC_FLAGS_PARAM
	    _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT128 CA, CT;
  UINT64 sign_x, coefficient_x;
  UINT64 Q, Q2, A10, C4, R, R2, QE, res;
  SINT64 D;
  int_double t_scale;
  int_float tempx;
  double da, dq, da_h, da_l, dqe;
  int exponent_x, exponent_q, bin_expon_cx;
  int digits_x;
  int scale;

#if DECIMAL_CALL_BY_REFERENCE
#if !DECIMAL_GLOBAL_ROUNDING
  _IDEC_round rnd_mode = *prnd_mode;
#endif
  x = *px;
#endif

  // unpack arguments, check for NaN or Infinity
  if (!unpack_BID64 (&sign_x, &exponent_x, &coefficient_x, x)) {
    // x is Inf. or NaN or 0

    if ((x & INFINITY_MASK64) == INFINITY_MASK64) {
      res = x;
      if (x == 0xf800000000000000ull)	// -Infinity
      {
	res = NAN_MASK64;
#ifdef SET_STATUS_FLAGS
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      }
#ifdef SET_STATUS_FLAGS
      if ((x & SNAN_MASK64) == SNAN_MASK64)	// sNaN
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      BID_RETURN (res & QUIET_MASK64);
    }
    // x is 0
    exponent_x = (exponent_x + DECIMAL_EXPONENT_BIAS) >> 1;
    res = sign_x | (((UINT64) exponent_x) << 53);
    BID_RETURN (res);
  }
  // x<0?
  if (sign_x && coefficient_x) {
    res = NAN_MASK64;
#ifdef SET_STATUS_FLAGS
    __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
    BID_RETURN (res);
  }
  //--- get number of bits in the coefficient of x ---
  tempx.d = (float) coefficient_x;
  bin_expon_cx = ((tempx.i >> 23) & 0xff) - 0x7f;
  digits_x = __bid_estimate_decimal_digits[bin_expon_cx];
  // add test for range
  if (coefficient_x >= __bid_power10_index_binexp[bin_expon_cx])
    digits_x++;

  A10 = coefficient_x;
  if (exponent_x & 1) {
    A10 = (A10 << 2) + A10;
    A10 += A10;
  }

  dqe = sqrt ((double) A10);
  QE = (UINT32) dqe;
  if (QE * QE == A10) {
    res =
      very_fast_get_BID64 (0, (exponent_x + DECIMAL_EXPONENT_BIAS) >> 1,
			   QE);
    BID_RETURN (res);
  }
  // if exponent is odd, scale coefficient by 10
  scale = 31 - digits_x;
  exponent_q = exponent_x - scale;
  scale += (exponent_q & 1);	// exp. bias is even

  CT = __bid_power10_table_128[scale];
  __mul_64x128_short (CA, coefficient_x, CT);

  // 2^64
  t_scale.i = 0x43f0000000000000ull;
  // convert CA to DP
  da_h = CA.w[1];
  da_l = CA.w[0];
  da = da_h * t_scale.d + da_l;

  dq = sqrt (da);

  Q = (UINT64) dq;

  // get sign(sqrt(CA)-Q)
  R = CA.w[0] - Q * Q;
  R = ((SINT64) R) >> 63;
  D = R + R + 1;

  exponent_q = (exponent_q + DECIMAL_EXPONENT_BIAS) >> 1;

#ifdef SET_STATUS_FLAGS
  __set_status_flags (pfpsf, INEXACT_EXCEPTION);
#endif

#ifndef IEEE_ROUND_NEAREST
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
  if (!((rnd_mode) & 3)) {
#endif
#endif

    // midpoint to check
    Q2 = Q + Q + D;
    C4 = CA.w[0] << 2;

    // get sign(-sqrt(CA)+Midpoint)
    R2 = Q2 * Q2 - C4;
    R2 = ((SINT64) R2) >> 63;

    // adjust Q if R!=R2
    Q += (D & (R ^ R2));
#ifndef IEEE_ROUND_NEAREST
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
  } else {
    C4 = CA.w[0];
    Q += D;
    if ((SINT64) (Q * Q - C4) > 0)
      Q--;
    if (rnd_mode == ROUNDING_UP)
      Q++;
  }
#endif
#endif

  res = fast_get_BID64 (0, exponent_q, Q);
  BID_RETURN (res);
}
