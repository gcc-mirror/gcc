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

#include "bid_internal.h"

#if DECIMAL_CALL_BY_REFERENCE
void
__bid128_mul (UINT128 * pres, UINT128 * px,
            UINT128 *
            py _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
            _EXC_INFO_PARAM) {
  UINT128 x = *px, y = *py;

#if !DECIMAL_GLOBAL_ROUNDING
  unsigned int rnd_mode = *prnd_mode;

#endif
#else
UINT128
__bid128_mul (UINT128 x,
            UINT128 y _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
            _EXC_INFO_PARAM) {

#endif
  UINT128 res;
  UINT64 x_sign, y_sign, sign;
  UINT64 x_exp, y_exp;

  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
  // Note: C2.w[1], C2.w[0] represent y_signif_hi, y_signif_lo (all are UINT64)
  UINT64 tmp64, tmp64A;
  BID_UI64DOUBLE tmp1, tmp2;
  int x_nr_bits, y_nr_bits;
  int q1, q2, ind, shift;
  UINT128 C1, C2;
  UINT128 Cstar;  // C* represents up to 34 decimal digits ~ 113 bits
  UINT384 fstar;
  int q;
  UINT128 P128, R128; // for underflow path
  UINT192 P192, R192; // for underflow path
  UINT256 C, P256, R256;
  UINT384 P384;
  UINT512 P512;
  int incr_exp = 0; // for underflow path
  int incr_exp1 = 0; // for underflow path
  int tmp_fpa = 0;  // if possible underflow and q>=34, use to undo the rounding
  UINT64 C1_hi, C2_hi;
  UINT64 C1_lo, C2_lo;
  int is_inexact = 0;
  int is_midpoint_lt_even = 0, is_midpoint_gt_even = 0;
  int is_inexact_lt_midpoint = 0, is_inexact_gt_midpoint = 0;
  int is_midpoint_lt_even1 = 0, is_midpoint_gt_even1 = 0;
  int is_inexact_lt_midpoint1 = 0, is_inexact_gt_midpoint1 = 0;
  int is_overflow = 0;
  int no_underflow = 0;

  // unpack the arguments
  // unpack x
  x_sign = x.w[1] & MASK_SIGN; // 0 for positive, MASK_SIGN for negative
  x_exp = x.w[1] & MASK_EXP; // biased and shifted left 49 bit positions
  C1_hi = x.w[1] & MASK_COEFF;
  C1_lo = x.w[0];

  // unpack y
  y_sign = y.w[1] & MASK_SIGN; // 0 for positive, MASK_SIGN for negative
  y_exp = y.w[1] & MASK_EXP; // biased and shifted left 49 bit positions
  C2_hi = y.w[1] & MASK_COEFF;
  C2_lo = y.w[0];
  sign = x_sign ^ y_sign;

  // check for NaN or Infinity
  if (((x.w[1] & MASK_SPECIAL) == MASK_SPECIAL)
      || ((y.w[1] & MASK_SPECIAL) == MASK_SPECIAL)) {

    // x is special or y is special
    if ((x.w[1] & MASK_NAN) == MASK_NAN) { // x is NAN
      if ((x.w[1] & MASK_SNAN) == MASK_SNAN) { // x is SNAN
        // set invalid flag
        *pfpsf |= INVALID_EXCEPTION;

        // return quiet (x)
        res.w[1] = x.w[1] & 0xfdffffffffffffffull;
        res.w[0] = x.w[0];
      } else { // x is QNaN
        if ((y.w[1] & MASK_SNAN) == MASK_SNAN) { // y is SNAN
          // set invalid flag
          *pfpsf |= INVALID_EXCEPTION;
        }
        // return x
        res.w[1] = x.w[1];
        res.w[0] = x.w[0];
      }
      BID_RETURN (res);
    } else if ((y.w[1] & MASK_NAN) == MASK_NAN) { // y is NAN
      if ((y.w[1] & MASK_SNAN) == MASK_SNAN) { // y is SNAN
        // set invalid flag
        *pfpsf |= INVALID_EXCEPTION;

        // return quiet (y)
        res.w[1] = y.w[1] & 0xfdffffffffffffffull;
        res.w[0] = y.w[0];
      } else { // y is QNaN
        // return y
        res.w[1] = y.w[1];
        res.w[0] = y.w[0];
      }
      BID_RETURN (res);
    } else { // neither x nor y is NaN; at least one is infinity
      if ((x.w[1] & MASK_ANY_INF) == MASK_INF) { // x is infinity
        if (((y.w[1] & MASK_ANY_INF) == MASK_INF) || (C2_hi != 0x0ull)
            || (C2_lo != 0x0ull)) {

          // y is infinity OR y is finite 
          // if same sign, return +inf otherwise return -inf
          if (!sign) {
            res.w[1] = 0x7800000000000000ull; // +inf
            res.w[0] = 0x0000000000000000ull;
          } else { // x and y are infinities of opposite signs
            res.w[1] = 0xf800000000000000ull; // -inf
            res.w[0] = 0x0000000000000000ull;
          }
        } else { // if y is 0
          // set invalid flag
          *pfpsf |= INVALID_EXCEPTION;

          // return QNaN Indefinite
          res.w[1] = 0x7c00000000000000ull;
          res.w[0] = 0x0000000000000000ull;
        }
      } else { // x is not NaN or infinity, so y must be infinity
        if ((C1_hi != 0x0ull) || (C1_lo != 0x0ull)) {

          // x is finite
          // if same sign, return +inf otherwise return -inf
          if (!sign) {
            res.w[1] = 0x7800000000000000ull; // +inf
            res.w[0] = 0x0000000000000000ull;
          } else { // y and x are of opposite signs
            res.w[1] = 0xf800000000000000ull; // -inf
            res.w[0] = 0x0000000000000000ull;
          }
        } else { // if x is 0
          // set invalid flag
          *pfpsf |= INVALID_EXCEPTION;

          // return QNaN Indefinite
          res.w[1] = 0x7c00000000000000ull;
          res.w[0] = 0x0000000000000000ull;
        }
      }
      BID_RETURN (res);
    }
  }
  // test for non-canonical values:
  // - values whose encoding begins with x00, x01, or x10 and whose
  //   coefficient is larger than 10^34 -1, or
  // - values whose encoding begins with x1100, x1101, x1110 (if NaNs
  //   and infinitis were eliminated already this test is reduced to
  //   checking for x10x)

  // test for non-canonical values of the argument x
  if ((((C1_hi > 0x0001ed09bead87c0ull) || 
      ((C1_hi == 0x0001ed09bead87c0ull) && (C1_lo > 0x378d8e63ffffffffull))) && 
      ((x.w[1] & 0x6000000000000000ull) != 0x6000000000000000ull)) || 
      ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull)) {
    // check for the case where the exponent is shifted right by 2 bits!
    if ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull) {
      x_exp = (x.w[1] << 2) & MASK_EXP; // same position as for G[0]G[1] != 11
    }
    x.w[1] = x.w[1] & 0x8000000000000000ull; // preserve the sign bit
    x.w[0] = 0;
    C1_hi = 0;
    C1_lo = 0;
  }
  // test for non-canonical values of the argument y
  if ((((C2_hi > 0x0001ed09bead87c0ull)
       || ((C2_hi == 0x0001ed09bead87c0ull)
           && (C2_lo > 0x378d8e63ffffffffull)))
      && ((y.w[1] & 0x6000000000000000ull) != 0x6000000000000000ull))
      || ((y.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull)) {

    // check for the case where the exponent is shifted right by 2 bits!
    if ((y.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull) {
      y_exp = (y.w[1] << 2) & MASK_EXP; // same position as for G[0]G[1] != 11
    }
    y.w[1] = y.w[1] & 0x8000000000000000ull; // preserve the sign bit
    y.w[0] = 0;
    C2_hi = 0;
    C2_lo = 0;
  }
  if (((C1_hi == 0x0ull) && (C1_lo == 0x0ull)) || ((C2_hi == 0x0ull)
      && (C2_lo == 0x0ull))) {

    // x is 0 and y is not special OR y is 0 and x is not special
    // if same sign, return +0 otherwise return -0
    ind = (x_exp >> 49) + (y_exp >> 49) - 6176;
    if (ind < 0)
      ind = 0;
    if (ind > 0x2fff)
      ind = 0x2fff; // 6111 + 6176
    if ((x.w[1] & MASK_SIGN) == (y.w[1] & MASK_SIGN)) {
      res.w[1] = 0x0000000000000000ull | ((UINT64) ind << 49); // +0.0
      res.w[0] = 0x0000000000000000ull;
    } else { // opposite signs
      res.w[1] = 0x8000000000000000ull | ((UINT64) ind << 49); // -0.0
      res.w[0] = 0x0000000000000000ull;
    }
    BID_RETURN (res);
  } else { // x and y are not special and are not zero
    // unpack x
    C1.w[1] = C1_hi;
    C1.w[0] = C1_lo;

    // q1 = nr. of decimal digits in x
    // determine first the nr. of bits in x
    if (C1.w[1] == 0) {
      if (C1.w[0] >= 0x0020000000000000ull) { // x >= 2^53
        // split the 64-bit value in two 32-bit halves to avoid rounding errors
        if (C1.w[0] >= 0x0000000100000000ull) { // x >= 2^32
          tmp1.d = (double) (C1.w[0] >> 32); // exact conversion
          x_nr_bits =
            33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
        } else { // x < 2^32
          tmp1.d = (double) (C1.w[0]); // exact conversion
          x_nr_bits =
            1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      }} else { // if x < 2^53
        tmp1.d = (double) C1.w[0]; // exact conversion
        x_nr_bits =
          1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }} else { // C1.w[1] != 0 => nr. bits = 64 + nr_bits (C1.w[1])
      tmp1.d = (double) C1.w[1]; // exact conversion
      x_nr_bits =
        65 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    } q1 = __bid_nr_digits[x_nr_bits - 1].digits;
    if (q1 == 0) {
      q1 = __bid_nr_digits[x_nr_bits - 1].digits1;
      if (C1.w[1] > __bid_nr_digits[x_nr_bits - 1].threshold_hi
          || (C1.w[1] == __bid_nr_digits[x_nr_bits - 1].threshold_hi
          && C1.w[0] >= __bid_nr_digits[x_nr_bits - 1].threshold_lo))
        q1++;
    }
    C2.w[1] = C2_hi;
    C2.w[0] = C2_lo;
    if (C2.w[1] == 0) {
      if (C2.w[0] >= 0x0020000000000000ull) { // y >= 2^53
        // split the 64-bit value in two 32-bit halves to avoid rounding errors
        if (C2.w[0] >= 0x0000000100000000ull) { // y >= 2^32
          tmp2.d = (double) (C2.w[0] >> 32); // exact conversion
          y_nr_bits =
            32 + ((((unsigned int) (tmp2.ui64 >> 52)) & 0x7ff) - 0x3ff);
        } else { // y < 2^32
          tmp2.d = (double) C2.w[0]; // exact conversion
          y_nr_bits =
            ((((unsigned int) (tmp2.ui64 >> 52)) & 0x7ff) - 0x3ff);
      }} else { // if y < 2^53
        tmp2.d = (double) C2.w[0]; // exact conversion
        y_nr_bits =
          ((((unsigned int) (tmp2.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }} else { // C2.w[1] != 0 => nr. bits = 64 + nr_bits (C2.w[1])
      tmp2.d = (double) C2.w[1]; // exact conversion
      y_nr_bits =
        64 + ((((unsigned int) (tmp2.ui64 >> 52)) & 0x7ff) - 0x3ff);
    } q2 = __bid_nr_digits[y_nr_bits].digits;
    if (q2 == 0) {
      q2 = __bid_nr_digits[y_nr_bits].digits1;
      if (C2.w[1] > __bid_nr_digits[y_nr_bits].threshold_hi
          || (C2.w[1] == __bid_nr_digits[y_nr_bits].threshold_hi
          && C2.w[0] >= __bid_nr_digits[y_nr_bits].threshold_lo))
        q2++;
    }
    // the exact product has either q1 + q2 - 1 or q1 + q2 decimal digits
    // where 2 <= q1 + q2 <= 68
    // calculate C' = C1 * C2 and determine q
    C.w[3] = C.w[2] = C.w[1] = C.w[0] = 0;
    if (q1 + q2 <= 19) { // if 2 <= q1 + q2 <= 19, C' = C1 * C2 fits in 64 bits
      C.w[0] = C1.w[0] * C2.w[0];

      // if C' < 10^(q1+q2-1) then q = q1 + q2 - 1 else q = q1 + q2
      if (C.w[0] < __bid_ten2k64[q1 + q2 - 1])
        q = q1 + q2 - 1; // q in [1, 18]
      else
        q = q1 + q2; // q in [2, 19]
      // length of C1 * C2 rounded up to a multiple of 64 bits is len = 64;
    } else if (q1 + q2 == 20) { // C' = C1 * C2 fits in 64 or 128 bits
      // q1 <= 19 and q2 <= 19 so both C1 and C2 fit in 64 bits
      __mul_64x64_to_128MACH (C, C1.w[0], C2.w[0]);

      // if C' < 10^(q1+q2-1) = 10^19 then q = q1+q2-1 = 19 else q = q1+q2 = 20
      if (C.w[1] == 0 && C.w[0] < __bid_ten2k64[19]) { // 19 = q1+q2-1
        // length of C1 * C2 rounded up to a multiple of 64 bits is len = 64;
        q = 19; // 19 = q1 + q2 - 1
      } else {

        // if (C.w[1] == 0)
        //   length of C1 * C2 rounded up to a multiple of 64 bits is len = 64;
        // else
        //   length of C1 * C2 rounded up to a multiple of 64 bits is len = 128;
        q = 20; // 20 = q1 + q2
      }
    } else if (q1 + q2 <= 38) { // 21 <= q1 + q2 <= 38
      // C' = C1 * C2 fits in 64 or 128 bits
      // (64 bits possibly, but only when q1 + q2 = 21 and C' has 20 digits)
      // at least one of C1, C2 has at most 19 decimal digits & fits in 64 bits
      if (q1 <= 19) {
        __mul_128x64_to_128 (C, C1.w[0], C2);
      } else { // q2 <= 19
        __mul_128x64_to_128 (C, C2.w[0], C1);
      }

      // if C' < 10^(q1+q2-1) then q = q1 + q2 - 1 else q = q1 + q2
      if (C.w[1] < __bid_ten2k128[q1 + q2 - 21].w[1]
          || (C.w[1] == __bid_ten2k128[q1 + q2 - 21].w[1]
          && C.w[0] < __bid_ten2k128[q1 + q2 - 21].w[0])) {

        // if (C.w[1] == 0) // q = 20, necessarily
        //   length of C1 * C2 rounded up to a multiple of 64 bits is len = 64;
        // else
        //   length of C1 * C2 rounded up to a multiple of 64 bits is len = 128;
        q = q1 + q2 - 1; // q in [20, 37]
      } else {

        // length of C1 * C2 rounded up to a multiple of 64 bits is len = 128;
        q = q1 + q2; // q in [21, 38]
      }
    } else if (q1 + q2 == 39) { // C' = C1 * C2 fits in 128 or 192 bits
      // both C1 and C2 fit in 128 bits (actually in 113 bits)
      // may replace this by 128x128_to192
      __mul_128x128_to_256 (C, C1, C2); // C.w[3] is 0
      // if C' < 10^(q1+q2-1) = 10^38 then q = q1+q2-1 = 38 else q = q1+q2 = 39
      if (C.w[2] == 0 && (C.w[1] < __bid_ten2k128[18].w[1] || 
          (C.w[1] == __bid_ten2k128[18].w[1] && C.w[0] < __bid_ten2k128[18].w[0]))) { 
          // 18 = 38 - 20 = q1+q2-1 - 20
        // length of C1 * C2 rounded up to a multiple of 64 bits is len = 128;
        q = 38; // 38 = q1 + q2 - 1
      } else {

        // if (C.w[2] == 0)
        // length of C1 * C2 rounded up to a multiple of 64 bits is len = 128;
        // else
        //   length of C1 * C2 rounded up to a multiple of 64 bits is len = 192;
        q = 39; // 39 = q1 + q2
      }
    } else if (q1 + q2 <= 57) { // 40 <= q1 + q2 <= 57
      // C' = C1 * C2 fits in 128 or 192 bits
      // (128 bits possibly, but only when q1 + q2 = 40 and C' has 39 digits)
      // both C1 and C2 fit in 128 bits (actually in 113 bits); at most one
      // may fit in 64 bits
      if (C1.w[1] == 0) { // C1 fits in 64 bits
        // __mul_64x128_full (REShi64, RESlo128, A64, B128)
        __mul_64x128_full (C.w[2], C, C1.w[0], C2);
      } else if (C2.w[1] == 0) { // C2 fits in 64 bits
        // __mul_64x128_full (REShi64, RESlo128, A64, B128)
        __mul_64x128_full (C.w[2], C, C2.w[0], C1);
      } else { // both C1 and C2 require 128 bits
        // may use __mul_128x128_to_192 (C.w[2], C.w[0], C2.w[0], C1);
        __mul_128x128_to_256 (C, C1, C2); // C.w[3] = 0
      }

      // if C' < 10^(q1+q2-1) then q = q1 + q2 - 1 else q = q1 + q2
      if (C.w[2] < __bid_ten2k256[q1 + q2 - 40].w[2]
          || (C.w[2] == __bid_ten2k256[q1 + q2 - 40].w[2]
          && (C.w[1] < __bid_ten2k256[q1 + q2 - 40].w[1]
              || (C.w[1] == __bid_ten2k256[q1 + q2 - 40].w[1]
              && C.w[0] < __bid_ten2k256[q1 + q2 - 40].w[0])))) {

        // if (C.w[2] == 0) // q = 39, necessarily
        //   length of C1 * C2 rounded up to a multiple of 64 bits is len = 128;
        // else
        //   length of C1 * C2 rounded up to a multiple of 64 bits is len = 192;
        q = q1 + q2 - 1; // q in [39, 56]
      } else {

        // length of C1 * C2 rounded up to a multiple of 64 bits is len = 192;
        q = q1 + q2; // q in [40, 57]
      }
    } else if (q1 + q2 == 58) { // C' = C1 * C2 fits in 192 or 256 bits
      // both C1 and C2 fit in 128 bits (actually in 113 bits); at most one
      // may fit in 64 bits
      if (C1.w[1] == 0) { // C1 * C2 will fit in 192 bits
        __mul_64x128_full (C.w[2], C, C1.w[0], C2); // may use 64x128_to_192
      } else if (C2.w[1] == 0) { // C1 * C2 will fit in 192 bits
        __mul_64x128_full (C.w[2], C, C2.w[0], C1); // may use 64x128_to_192
      } else { // C1 * C2 will fit in 192 bits or in 256 bits
        __mul_128x128_to_256 (C, C1, C2);
      }

      // if C' < 10^(q1+q2-1) = 10^57 then q = q1+q2-1 = 57 else q = q1+q2 = 58
      if (C.w[3] == 0 && (C.w[2] < __bid_ten2k256[18].w[2] || 
          (C.w[2] == __bid_ten2k256[18].w[2] && (C.w[1] < __bid_ten2k256[18].w[1] || 
          (C.w[1] == __bid_ten2k256[18].w[1] && C.w[0] < __bid_ten2k256[18].w[0]))))) {
          // 18 = 57 - 39 = q1+q2-1 - 39
        // length of C1 * C2 rounded up to a multiple of 64 bits is len = 192;
        q = 57; // 57 = q1 + q2 - 1
      } else {

        // if (C.w[3] == 0)
        //   length of C1 * C2 rounded up to a multiple of 64 bits is len = 192;
        // else
        //   length of C1 * C2 rounded up to a multiple of 64 bits is len = 256;
        q = 58; // 58 = q1 + q2
      }
    } else { // if 59 <= q1 + q2 <= 68
      // C' = C1 * C2 fits in 192 or 256 bits
      // (192 bits possibly, but only when q1 + q2 = 59 and C' has 58 digits)
      // both C1 and C2 fit in 128 bits (actually in 113 bits); none fits in
      // 64 bits
      // may use __mul_128x128_to_192 (C.w[2], C.w[0], C2.w[0], C1);
      __mul_128x128_to_256 (C, C1, C2); // C.w[3] = 0
      // if C' < 10^(q1+q2-1) then q = q1 + q2 - 1 else q = q1 + q2
      if (C.w[3] < __bid_ten2k256[q1 + q2 - 40].w[3]
          || (C.w[3] == __bid_ten2k256[q1 + q2 - 40].w[3]
          && (C.w[2] < __bid_ten2k256[q1 + q2 - 40].w[2]
              || (C.w[2] == __bid_ten2k256[q1 + q2 - 40].w[2]
              && (C.w[1] < __bid_ten2k256[q1 + q2 - 40].w[1]
                  || (C.w[1] == __bid_ten2k256[q1 + q2 - 40].w[1]
                  && C.w[0] < __bid_ten2k256[q1 + q2 - 40].w[0])))))) {

        // if (C.w[3] == 0) // q = 58, necessarily
        //   length of C1 * C2 rounded up to a multiple of 64 bits is len = 192;
        // else
        //   length of C1 * C2 rounded up to a multiple of 64 bits is len = 256;
        q = q1 + q2 - 1; // q in [58, 67]
      } else {

        // length of C1 * C2 rounded up to a multiple of 64 bits is len = 256;
        q = q1 + q2; // q in [59, 68]
      }
    }
    if (((UINT64) q << 49) + x_exp + y_exp <
        ((UINT64) P34 << 49) + EXP_MIN + BIN_EXP_BIAS) {

      // possible underflow
      // q + ex + ey < P34 + EMIN <=> q - P34 < EMIN - ex - ey <=> q - P34 < ind
      goto _underflow_path;
    }
    if (q <= 34) { // 2 <= q <= 34 the result is exact, and fits in 113 bits
      tmp64 = x_exp + y_exp;
      if (tmp64 > EXP_MAX + BIN_EXP_BIAS) { // possible overflow
        ind = (tmp64 - EXP_MAX - BIN_EXP_BIAS) >> 49;
        if (ind > 34 - q) { // overflow in all rounding modes
          // |res| >= 10^p * 10^emax = 10^(p-1) * 10^(emax+1)
          // assemble the result
          if (rnd_mode == ROUNDING_TO_NEAREST
              || rnd_mode == ROUNDING_TIES_AWAY) {
            res.w[1] = sign | 0x7800000000000000ull;
            res.w[0] = 0x0ull;
          } else if (rnd_mode == ROUNDING_DOWN) {
            if (sign) { // res = -inf
              res.w[1] = 0xf800000000000000ull;
              res.w[0] = 0x0ull;
            } else { // res = +MAXFP
              res.w[1] = 0x5fffed09bead87c0ull;
              res.w[0] = 0x378d8e63ffffffffull;
            }
          } else if (rnd_mode == ROUNDING_UP) {
            if (sign) { // res = -MAXFP
              res.w[1] = 0xdfffed09bead87c0ull;
              res.w[0] = 0x378d8e63ffffffffull;
            } else { // res = +inf
              res.w[1] = 0x7800000000000000ull;
              res.w[0] = 0x0ull;
            }
          } else { // if (rnd_mode == ROUNDING_TO_ZERO)
            // |res| = (10^34 - 1) * 10^6111 = +MAXFP
            res.w[1] = sign | 0x5fffed09bead87c0ull;
            res.w[0] = 0x378d8e63ffffffffull;
          }

          // set the inexact flag
          *pfpsf |= INEXACT_EXCEPTION;

          // set the overflow flag
          *pfpsf |= OVERFLOW_EXCEPTION;

          // is_overflow = 1;
          BID_RETURN (res);
        } else { // tmp64 > EXP_MAX + BIN_EXP_BIAS but 
          // ind = ((tmp64-EXP_MAX-BIN_EXP_BIAS)>>49) <= 34 - q
          // the exponent will be the maximum exponent
          // multiply C by 10^ind; the result fits in 34 digits
          if (ind <= 19) { // multiply by __bid_ten2k64[ind]
            if (q <= 19) { // 64x64 -> 128
              __mul_64x64_to_128MACH (C, C.w[0], __bid_ten2k64[ind]);
            } else { // 128 x 64 -> 128
              // may optimize to multiply 64 x 128 -> 128
              __mul_64x128_full (tmp64, C, __bid_ten2k64[ind], C);
            }
          } else { // if 20 <= ind <= 32 multiply by __bid_ten2k128[ind - 20]
            // it must be that C.w[1] = 0, as C < 10^14
            // may optimize to multiply 64 x 128 -> 128
            __mul_64x128_full (tmp64, C, C.w[0], __bid_ten2k128[ind - 20]);
          }
          res.w[0] = C.w[0];
          res.w[1] = C.w[1];
          res.w[1] |= EXP_MAX; // EXP MAX
        }
      } else {
        res.w[0] = C.w[0];
        res.w[1] = C.w[1];
        res.w[1] |= (tmp64 - BIN_EXP_BIAS);
      }
      res.w[1] |= sign;
    } else if (q <= 38) { // 35 <= q <= 38; exact coefficient fits in 128 bits
      // C = C + 1/2 * 10^x where the result C fits in 127 bits
      ind = q - 35;
      tmp64 = C.w[0];
      C.w[0] = C.w[0] + __bid_midpoint64[ind];
      if (C.w[0] < tmp64)
        C.w[1]++;

      // x = q - p = q - 34, 1 <= x <= 4
      // kx = 10^(-x) = __bid_ten2mk128M[ind]
      // C* = (C + 1/2 * 10^x) * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 128 bits
      __mul_128x128_to_256 (P256, C, __bid_ten2mk128M[ind]);
      Cstar.w[1] = P256.w[3];
      Cstar.w[0] = P256.w[2];
      fstar.w[2] = Cstar.w[0] & __bid_maskhigh128M[ind]; // fstar.w[3|4|5]=0
      fstar.w[1] = P256.w[1];
      fstar.w[0] = P256.w[0];

      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // __bid_shiftright128M[] and __bid_maskhigh128M[]
      // the top Ex bits of 10^(-x) are T* = __bid_ten2mk128truncM[ind], e.g.
      // if x=1, T*=__bid_ten2mk128truncM[0]=0xcccccccccccccccccccccccccccccccc
      // if (0 < f* < 10^(-x)) then the result is a midpoint
      //   if floor(C*) is even then C* = floor(C*) - logical right
      //       shift; C* has p decimal digits, correct by Prop. 1)
      //   else if floor(C*) is odd C* = floor(C*)-1 (logical right
      //       shift; C* has p decimal digits, correct by Pr. 1)
      // else
      //   C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-128 = __bid_shiftright128M[ind]
      shift = __bid_shiftright128M[ind]; // 3 <= shift <= 13
      Cstar.w[0] = (Cstar.w[0] >> shift) | (Cstar.w[1] << (64 - shift));
      Cstar.w[1] = (Cstar.w[1] >> shift);

      // determine inexactness of the rounding of C*
      // if (0 < f* - 1/2 < 10^(-x)) then
      //   the result is exact
      // else // if (f* - 1/2 > T*) then
      //   the result is inexact
      if (fstar.w[2] > __bid_one_half128M[ind]
          || (fstar.w[2] == __bid_one_half128M[ind]
          && (fstar.w[1] || fstar.w[0]))) {

        // f* > 1/2 and the result may be exact
        // Calculate f* - 1/2
        tmp64 = fstar.w[2] - __bid_one_half128M[ind];
        if (tmp64 || fstar.w[1] > __bid_ten2mk128truncM[ind].w[1] || 
            (fstar.w[1] == __bid_ten2mk128truncM[ind].w[1] && 
            fstar.w[0] > __bid_ten2mk128truncM[ind].w[0])) { // f* - 1/2 > 10^(-x)
          // set the inexact flag
          *pfpsf |= INEXACT_EXCEPTION;
          is_inexact_lt_midpoint = 1;
        }        // else the result is exact
      } else { // the result is inexact; f2* <= 1/2
        // set the inexact flag
        *pfpsf |= INEXACT_EXCEPTION;
        tmp_fpa = 1;
        is_inexact_gt_midpoint = 1;
      }

      // check for midpoints (could do this before determining inexactness)
      if ((fstar.w[2] == 0) && (fstar.w[1] || fstar.w[0])
          && (fstar.w[1] < __bid_ten2mk128truncM[ind].w[1]
              || (fstar.w[1] == __bid_ten2mk128truncM[ind].w[1]
              && fstar.w[0] <= __bid_ten2mk128truncM[ind].w[0]))) {

        // the result is a midpoint
        if (Cstar.w[0] & 0x01) { // Cstar.w[0] is odd; MP in [EVEN, ODD]
          // if floor(C*) is odd C = floor(C*) - 1; the result may be 0
          Cstar.w[0]--; // Cstar.w[0] is now even
          if (tmp_fpa == 1)
            tmp_fpa = 0;
          is_midpoint_gt_even = 1;
          is_inexact_lt_midpoint = 0;
          is_inexact_gt_midpoint = 0;
        } else { // else MP in [ODD, EVEN]
          is_midpoint_lt_even = 1;
          is_inexact_lt_midpoint = 0;
          is_inexact_gt_midpoint = 0;
        }
      }
      // check for rounding overflow
      if (Cstar.w[1] == 0x0001ed09bead87c0ull && 
          Cstar.w[0] == 0x378d8e6400000000ull) { // if  Cstar = 10^34
        tmp64 = x_exp + y_exp + ((UINT64) (ind + 2) << 49);
        Cstar.w[1] = 0x0000314dc6448d93ull; // Cstar = 10^33
        Cstar.w[0] = 0x38c15b0a00000000ull;

        // if rounding overflow made the exponent equal to emin, set underflow
        if (tmp64 == EXP_MIN + BIN_EXP_BIAS)
          *pfpsf |= UNDERFLOW_EXCEPTION;
      } else { // 10^33 <= Cstar <= 10^34 - 1
        tmp64 = x_exp + y_exp + ((UINT64) (ind + 1) << 49); // ind+1 = q-34
      }
      if (tmp64 >= EXP_MAX + BIN_EXP_BIAS) { // possibble overflow
        // exp >= emax for the result rounded to nearest even
        if (rnd_mode == ROUNDING_TO_NEAREST
            || rnd_mode == ROUNDING_TIES_AWAY) {
          if (tmp64 > EXP_MAX + BIN_EXP_BIAS) {

            // |res| >= 10^(p-1) * 10^(emax+1) <=> exp >= emax+1
            res.w[1] = sign | 0x7800000000000000ull; // +/-inf
            res.w[0] = 0x0ull;
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else { // not overflow
            res.w[0] = Cstar.w[0];
            res.w[1] = Cstar.w[1];
            res.w[1] |= (tmp64 - BIN_EXP_BIAS);
          }
        } else if (rnd_mode == ROUNDING_DOWN) {
          if (!sign && (tmp64 > EXP_MAX + BIN_EXP_BIAS) && 
              !(tmp64 == EXP_MAX + BIN_EXP_BIAS + EXP_P1 && 
              Cstar.w[1] == 0x0000314dc6448d93ull && 
              Cstar.w[0] == 0x38c15b0a00000000ull && // 10^33 * 10^(emax+1)
              (is_midpoint_lt_even || is_inexact_gt_midpoint))) {

            // res = +MAXFP
            res.w[1] = 0x5fffed09bead87c0ull;
            res.w[0] = 0x378d8e63ffffffffull; // (10^34-1) * 10^emax
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else if (sign && ((tmp64 > EXP_MAX + BIN_EXP_BIAS) || 
              ((tmp64 == EXP_MAX + BIN_EXP_BIAS) && 
              Cstar.w[1] == 0x0001ed09bead87c0ull && 
              Cstar.w[0] == 0x378d8e63ffffffffull && // (10^34-1) * 10^emax
              is_inexact_lt_midpoint))) {
            res.w[1] = 0xf800000000000000ull; // -inf
            res.w[0] = 0x0ull;
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else { // not overflow
            res.w[0] = Cstar.w[0];
            res.w[1] = Cstar.w[1];
            res.w[1] |= (tmp64 - BIN_EXP_BIAS);
          }
        } else if (rnd_mode == ROUNDING_UP) {
          if (sign && (tmp64 > EXP_MAX + BIN_EXP_BIAS) && 
              !(tmp64 == EXP_MAX + BIN_EXP_BIAS + EXP_P1 && 
              Cstar.w[1] == 0x0000314dc6448d93ull && 
              Cstar.w[0] == 0x38c15b0a00000000ull && // 10^33 * 10^(emax+1)
              (is_midpoint_lt_even || is_inexact_gt_midpoint))) {
            // res = -MAXFP
            res.w[1] = 0xdfffed09bead87c0ull;
            res.w[0] = 0x378d8e63ffffffffull; // -(10^34-1) * 10^emax
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else if (!sign && ((tmp64 > EXP_MAX + BIN_EXP_BIAS) || 
              ((tmp64 == EXP_MAX + BIN_EXP_BIAS) && 
              Cstar.w[1] == 0x0001ed09bead87c0ull && 
              Cstar.w[0] == 0x378d8e63ffffffffull && // (10^34-1) * 10^emax
              is_inexact_lt_midpoint))) {
            res.w[1] = 0x7800000000000000ull; // inf
            res.w[0] = 0x0ull;
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else { // not overflow
            res.w[0] = Cstar.w[0];
            res.w[1] = Cstar.w[1];
            res.w[1] |= (tmp64 - BIN_EXP_BIAS);
          }
        } else { // if (rnd_mode == ROUNDING_TO_ZERO)
          if (!sign && (tmp64 > EXP_MAX + BIN_EXP_BIAS) && 
              !(tmp64 == EXP_MAX + BIN_EXP_BIAS + EXP_P1 && 
              Cstar.w[1] == 0x0000314dc6448d93ull && 
              Cstar.w[0] == 0x38c15b0a00000000ull && // 10^33 * 10^(emax+1)
              (is_midpoint_lt_even || is_inexact_gt_midpoint))) {
            // res = +MAXFP
            res.w[1] = 0x5fffed09bead87c0ull;
            res.w[0] = 0x378d8e63ffffffffull; // (10^34-1) * 10^emax
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else if (sign && (tmp64 > EXP_MAX + BIN_EXP_BIAS) && 
              !(tmp64 == EXP_MAX + BIN_EXP_BIAS + EXP_P1 && 
              Cstar.w[1] == 0x0000314dc6448d93ull && 
              Cstar.w[0] == 0x38c15b0a00000000ull && // 10^33 * 10^(emax+1)
              (is_midpoint_lt_even || is_inexact_gt_midpoint))) {
            // res = -MAXFP
            res.w[1] = 0xdfffed09bead87c0ull;
            res.w[0] = 0x378d8e63ffffffffull; // -(10^34-1) * 10^emax
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else { // not overflow
            res.w[0] = Cstar.w[0];
            res.w[1] = Cstar.w[1];
            res.w[1] |= (tmp64 - BIN_EXP_BIAS);
          }
        }
        if (is_overflow) { // return for overflow
          // set the inexact flag
          *pfpsf |= INEXACT_EXCEPTION;

          // set the overflow flag
          *pfpsf |= OVERFLOW_EXCEPTION;

          // is_overflow = 1;
          BID_RETURN (res);
        }
      } else {
        res.w[0] = Cstar.w[0];
        res.w[1] = Cstar.w[1];
        res.w[1] |= (tmp64 - BIN_EXP_BIAS);
      }
      res.w[1] |= sign;
    } else if (q <= 57) { // 39 <= q <= 57; exact coefficient takes 128-192 bits
      // C = C + 1/2 * 10^x where the result C fits in 190 bits
      // (10^57 - 1 + 1/2 * 10^23 can be represented with 190 bits)
      // x = q - p = q - 34, 5 <= x <= 23
      // kx = 10^(-x) = __bid_ten2mk192M[ind]
      // C* = (C + 1/2 * 10^x) * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 192 bits
      ind = q - 39; // 0 <= ind <= 18
      tmp64 = C.w[0];
      tmp64A = C.w[1];

      // Note:
      // if 5 <= x <= 19 <=> 0 <= ind <= 14 then
      //   f* has 256 bits
      // else // if 20 <= x <= 23 <=> 15 <= ind <= 18 then
      //   f* has 320 bits
      if (ind <= 14) { // x - 1 = q - 35 = ind + 4 <= 18 
        // add one 64-bit word
        C.w[0] = C.w[0] + __bid_midpoint64[ind + 4];
        if (C.w[0] < tmp64)
          C.w[1]++;
        if (C.w[1] < tmp64A)
          C.w[2]++;
        __mul_192x192_to_384 (P384, C, __bid_ten2mk192M[ind])
          // calculate C* and f*; C* is actually floor(C*) in this case
          // C* and f* need shifting and masking, as shown by 
          // __bid_shiftright192M[] and __bid_maskhigh192M[]
          // C* has 128 bits; P384.w[5], P384.w[4], P384.w[3] need to be
          // shifted right by Ex-192 = __bid_shiftright192M[ind]
          shift = __bid_shiftright192M[ind]; // 16 <= shift <= 63
        Cstar.w[0] = (P384.w[3] >> shift) | (P384.w[4] << (64 - shift));
        Cstar.w[1] = (P384.w[4] >> shift) | (P384.w[5] << (64 - shift));

        // f* has 256 bits
        fstar.w[3] = P384.w[3] & __bid_maskhigh192M[ind];
        fstar.w[2] = P384.w[2];
        fstar.w[1] = P384.w[1];
        fstar.w[0] = P384.w[0];

        // the top Ex bits of 10^(-x) are T* = __bid_ten2mk192truncM[ind], e.g. 
        // if x=5, T* = __bid_ten2mk192truncM[0] =
        //   0xa7c5ac471b4784230fcf80dc33721d53cddd6e04c0592103
        // if (0 < f* < 10^(-x)) then the result is a midpoint
        //   if floor(C*) is even then C* = floor(C*) - logical right
        //       shift; C* has p decimal digits, correct by Prop. 1)
        //   else if floor(C*) is odd C* = floor(C*)-1 (logical right
        //       shift; C* has p decimal digits, correct by Pr. 1)
        // else
        //   C* = floor(C*) (logical right shift; C has p decimal digits,
        //       correct by Property 1)
        // n = C* * 10^(e+x)

        // determine inexactness of the rounding of C*
        // if (0 < f* - 1/2 < T* ~= 10^(-x)) then
        //   the result is exact
        // else // if (f* - 1/2 >= T*) then
        //   the result is inexact
        if (fstar.w[3] > __bid_one_half192M[ind]
            || (fstar.w[3] == __bid_one_half192M[ind]
            && (fstar.w[2] || fstar.w[1] || fstar.w[0]))) {

          // f* > 1/2 and the result may be exact
          // Calculate f* - 1/2
          tmp64 = fstar.w[3] - __bid_one_half192M[ind];
          if (tmp64 || fstar.w[2] > __bid_ten2mk192truncM[ind].w[2] || 
              (fstar.w[2] == __bid_ten2mk192truncM[ind].w[2] && 
              fstar.w[1] > __bid_ten2mk192truncM[ind].w[1]) || 
              (fstar.w[2] == __bid_ten2mk192truncM[ind].w[2] && 
              fstar.w[1] == __bid_ten2mk192truncM[ind].w[1] && 
              fstar.w[0] > __bid_ten2mk192truncM[ind].w[0])) { // f* - 1/2 > 10^(-x)
            // set the inexact flag
            *pfpsf |= INEXACT_EXCEPTION;
            is_inexact_lt_midpoint = 1;
          }        // else the result is exact
        } else { // the result is inexact; f2* <= 1/2
          // set the inexact flag
          *pfpsf |= INEXACT_EXCEPTION;
          tmp_fpa = 1;
          is_inexact_gt_midpoint = 1;
        }

        // check for midpoints (could do this before determining inexactness)
        if ((fstar.w[3] == 0)
            && (fstar.w[2] || fstar.w[1] || fstar.w[0])
            && (fstar.w[2] < __bid_ten2mk192truncM[ind].w[2]
                || (fstar.w[2] == __bid_ten2mk192truncM[ind].w[2]
                && fstar.w[1] < __bid_ten2mk192truncM[ind].w[1])
                || (fstar.w[2] == __bid_ten2mk192truncM[ind].w[2]
                && fstar.w[1] == __bid_ten2mk192truncM[ind].w[1]
                && fstar.w[0] <= __bid_ten2mk192truncM[ind].w[0]))) {

          // the result is a midpoint
          if (Cstar.w[0] & 0x01) { // Cstar.w[0] is odd; MP in [EVEN, ODD]
            // if floor(C*) is odd C = floor(C*) - 1; the result may be 0
            Cstar.w[0]--; // Cstar.w[0] is now even
            if (tmp_fpa == 1)
              tmp_fpa = 0;
            is_midpoint_gt_even = 1;
            is_inexact_lt_midpoint = 0;
            is_inexact_gt_midpoint = 0;
          } else { // else MP in [ODD, EVEN]
            is_midpoint_lt_even = 1;
            is_inexact_lt_midpoint = 0;
            is_inexact_gt_midpoint = 0;
          }
        }
      } else { // if ind >= 15 <=> x - 1 = q - 35 = ind + 4 >= 19
        // add two 64-bit words
        C.w[0] = C.w[0] + __bid_midpoint128[ind - 15].w[0];
        C.w[1] = C.w[1] + __bid_midpoint128[ind - 15].w[1];
        if (C.w[0] < tmp64)
          C.w[1]++;
        if (C.w[1] < tmp64A)
          C.w[2]++;
        __mul_192x192_to_384 (P384, C, __bid_ten2mk192M[ind])
          // calculate C* and f*; C* is actually floor(C*) in this case
          // C* and f* need shifting and masking, as shown by
          // __bid_shiftright192M[] and __bid_maskhigh192M[]
          // C* has 128 bits; P384.w[5], P384.w[4], need to be
          // shifted right by Ex-256 = __bid_shiftright192M[ind]
          shift = __bid_shiftright192M[ind]; // 2 <= shift <= 12
        Cstar.w[0] = (P384.w[4] >> shift) | (P384.w[5] << (64 - shift));
        Cstar.w[1] = (P384.w[5] >> shift);

        // f* has 320 bits
        fstar.w[4] = P384.w[4] & __bid_maskhigh192M[ind];
        fstar.w[3] = P384.w[3];
        fstar.w[2] = P384.w[2];
        fstar.w[1] = P384.w[1];
        fstar.w[0] = P384.w[0];

        // the top Ex bits of 10^(-x) are T* = __bid_ten2mk192truncM[ind], e.g. 
        // if x=23, T* = __bid_ten2mk192truncM[18] =
        //   0xc16d9a0095928a2775b7053c0f1782938d6f439b43088650
        // if (0 < f* < 10^(-x)) then the result is a midpoint
        //   if floor(C*) is even then C* = floor(C*) - logical right
        //       shift; C* has p decimal digits, correct by Prop. 1)
        //   else if floor(C*) is odd C* = floor(C*)-1 (logical right
        //       shift; C* has p decimal digits, correct by Pr. 1)
        // else
        //   C* = floor(C*) (logical right shift; C has p decimal digits,
        //       correct by Property 1)
        // n = C* * 10^(e+x)

        // determine inexactness of the rounding of C*
        // if (0 < f* - 1/2 < T* ~= 10^(-x)) then
        //   the result is exact
        // else // if (f* - 1/2 >= T*) then
        //   the result is inexact
        if (fstar.w[4] > __bid_one_half192M[ind]
            || (fstar.w[4] == __bid_one_half192M[ind]
            && (fstar.w[3] || fstar.w[2] || fstar.w[1] || fstar.w[0]))) {

          // f* > 1/2 and the result may be exact
          // Calculate f* - 1/2
          tmp64 = fstar.w[4] - __bid_one_half192M[ind];
          if (tmp64 || fstar.w[3] || fstar.w[2] > __bid_ten2mk192truncM[ind].w[2] || 
              (fstar.w[2] == __bid_ten2mk192truncM[ind].w[2] && 
              fstar.w[1] > __bid_ten2mk192truncM[ind].w[1]) || 
              (fstar.w[2] == __bid_ten2mk192truncM[ind].w[2] && 
              fstar.w[1] == __bid_ten2mk192truncM[ind].w[1] && 
              fstar.w[0] > __bid_ten2mk192truncM[ind].w[0])) { // f* - 1/2 > 10^(-x)
            // set the inexact flag
            *pfpsf |= INEXACT_EXCEPTION;
            is_inexact_lt_midpoint = 1;
          } // else the result is exact
        } else { // the result is inexact; f2* <= 1/2
          // set the inexact flag
          *pfpsf |= INEXACT_EXCEPTION;
          tmp_fpa = 1;
          is_inexact_gt_midpoint = 1;
        }

        // check for midpoints (could do this before determining inexactness)
        if ((fstar.w[4] == 0) && (fstar.w[3] == 0)
            && (fstar.w[2] || fstar.w[1] || fstar.w[0])
            && (fstar.w[2] < __bid_ten2mk192truncM[ind].w[2]
                || (fstar.w[2] == __bid_ten2mk192truncM[ind].w[2]
                && fstar.w[1] < __bid_ten2mk192truncM[ind].w[1])
                || (fstar.w[2] == __bid_ten2mk192truncM[ind].w[2]
                && fstar.w[1] == __bid_ten2mk192truncM[ind].w[1]
                && fstar.w[0] <= __bid_ten2mk192truncM[ind].w[0]))) {

          // the result is a midpoint
          if (Cstar.w[0] & 0x01) { // Cstar.w[0] is odd; MP in [EVEN, ODD]
            // if floor(C*) is odd C = floor(C*) - 1; the result may be 0
            Cstar.w[0]--; // Cstar.w[0] is now even
            if (tmp_fpa == 1)
              tmp_fpa = 0;
            is_midpoint_gt_even = 1;
            is_inexact_lt_midpoint = 0;
            is_inexact_gt_midpoint = 0;
          } else { // else MP in [ODD, EVEN]
            is_midpoint_lt_even = 1;
            is_inexact_lt_midpoint = 0;
            is_inexact_gt_midpoint = 0;
          }
        }
      }

      // check for rounding overflow
      if (Cstar.w[1] == 0x0001ed09bead87c0ull && 
          Cstar.w[0] == 0x378d8e6400000000ull) { // if  Cstar = 10^34
        tmp64 = x_exp + y_exp + ((UINT64) (ind + 6) << 49);
        Cstar.w[1] = 0x0000314dc6448d93ull; // Cstar = 10^33
        Cstar.w[0] = 0x38c15b0a00000000ull;
      } else { // 10^33 <= Cstar <= 10^34 - 1
        tmp64 = x_exp + y_exp + ((UINT64) (ind + 5) << 49); // ind+5 = q-34
      }
      if (tmp64 >= EXP_MAX + BIN_EXP_BIAS) { // possibble overflow
        // exp >= emax for the result rounded to nearest even
        if (rnd_mode == ROUNDING_TO_NEAREST
            || rnd_mode == ROUNDING_TIES_AWAY) {
          if (tmp64 > EXP_MAX + BIN_EXP_BIAS) {

            // |res| >= 10^(p-1) * 10^(emax+1) <=> exp >= emax+1
            res.w[1] = sign | 0x7800000000000000ull; // +/-inf
            res.w[0] = 0x0ull;
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else { // not overflow
            res.w[0] = Cstar.w[0];
            res.w[1] = Cstar.w[1];
            res.w[1] |= (tmp64 - BIN_EXP_BIAS);
          }
        } else if (rnd_mode == ROUNDING_DOWN) {
          if (!sign && (tmp64 > EXP_MAX + BIN_EXP_BIAS) && 
              !(tmp64 == EXP_MAX + BIN_EXP_BIAS + EXP_P1 && 
              Cstar.w[1] == 0x0000314dc6448d93ull && 
              Cstar.w[0] == 0x38c15b0a00000000ull && // 10^33 * 10^(emax+1)
              (is_midpoint_lt_even || is_inexact_gt_midpoint))) {
            // res = +MAXFP
            res.w[1] = 0x5fffed09bead87c0ull;
            res.w[0] = 0x378d8e63ffffffffull; // (10^34-1) * 10^emax
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else if (sign && ((tmp64 > EXP_MAX + BIN_EXP_BIAS) || 
              ((tmp64 == EXP_MAX + BIN_EXP_BIAS) && 
              Cstar.w[1] == 0x0001ed09bead87c0ull && 
              Cstar.w[0] == 0x378d8e63ffffffffull && // (10^34-1) * 10^emax
              is_inexact_lt_midpoint))) {
            res.w[1] = 0xf800000000000000ull; // -inf
            res.w[0] = 0x0ull;
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else { // not overflow
            res.w[0] = Cstar.w[0];
            res.w[1] = Cstar.w[1];
            res.w[1] |= (tmp64 - BIN_EXP_BIAS);
          }
        } else if (rnd_mode == ROUNDING_UP) {
          if (sign && (tmp64 > EXP_MAX + BIN_EXP_BIAS) && 
              !(tmp64 == EXP_MAX + BIN_EXP_BIAS + EXP_P1 && 
              Cstar.w[1] == 0x0000314dc6448d93ull && 
              Cstar.w[0] == 0x38c15b0a00000000ull && // 10^33 * 10^(emax+1)
              (is_midpoint_lt_even || is_inexact_gt_midpoint))) {
            // res = -MAXFP
            res.w[1] = 0xdfffed09bead87c0ull;
            res.w[0] = 0x378d8e63ffffffffull; // -(10^34-1) * 10^emax
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else if (!sign && ((tmp64 > EXP_MAX + BIN_EXP_BIAS) || 
              ((tmp64 == EXP_MAX + BIN_EXP_BIAS) && 
              Cstar.w[1] == 0x0001ed09bead87c0ull && 
              Cstar.w[0] == 0x378d8e63ffffffffull && // (10^34-1) * 10^emax
              is_inexact_lt_midpoint))) {
            res.w[1] = 0x7800000000000000ull; // inf
            res.w[0] = 0x0ull;
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else { // not overflow
            res.w[0] = Cstar.w[0];
            res.w[1] = Cstar.w[1];
            res.w[1] |= (tmp64 - BIN_EXP_BIAS);
          }
        } else { // if (rnd_mode == ROUNDING_TO_ZERO)
          if (!sign && (tmp64 > EXP_MAX + BIN_EXP_BIAS) && 
              !(tmp64 == EXP_MAX + BIN_EXP_BIAS + EXP_P1 && 
              Cstar.w[1] == 0x0000314dc6448d93ull && 
              Cstar.w[0] == 0x38c15b0a00000000ull && // 10^33 * 10^(emax+1)
              (is_midpoint_lt_even || is_inexact_gt_midpoint))) {
            // res = +MAXFP
            res.w[1] = 0x5fffed09bead87c0ull;
            res.w[0] = 0x378d8e63ffffffffull; // (10^34-1) * 10^emax
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else if (sign && (tmp64 > EXP_MAX + BIN_EXP_BIAS) && 
              !(tmp64 == EXP_MAX + BIN_EXP_BIAS + EXP_P1 && 
              Cstar.w[1] == 0x0000314dc6448d93ull && 
              Cstar.w[0] == 0x38c15b0a00000000ull && // 10^33 * 10^(emax+1)
              (is_midpoint_lt_even || is_inexact_gt_midpoint))) {
            // res = -MAXFP
            res.w[1] = 0xdfffed09bead87c0ull;
            res.w[0] = 0x378d8e63ffffffffull; // -(10^34-1) * 10^emax
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else { // not overflow
            res.w[0] = Cstar.w[0];
            res.w[1] = Cstar.w[1];
            res.w[1] |= (tmp64 - BIN_EXP_BIAS);
          }
        }
        if (is_overflow) { // return for overflow
          // set the inexact flag
          *pfpsf |= INEXACT_EXCEPTION;

          // set the overflow flag
          *pfpsf |= OVERFLOW_EXCEPTION;

          // is_overflow = 1;
        BID_RETURN (res)}
      } else {
        res.w[0] = Cstar.w[0];
        res.w[1] = Cstar.w[1];
        res.w[1] |= (tmp64 - BIN_EXP_BIAS);
      }
      res.w[1] |= sign;
    } else { // if (58 <= q <= 68) exact coefficient takes 192-226 bits
      // C = C + 1/2 * 10^x where the result C fits in 226 bits
      // (10^68 - 1 + 1/2 * 10^34 can be represented with 226 bits)
      // x = q - p = q - 34, 24 <= x <= 34
      // kx = 10^(-x) = __bid_ten2mk256M[ind]
      // C* = (C + 1/2 * 10^x) * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 256 bits
      ind = q - 58; // 0 <= ind <= 10
      tmp64 = C.w[0];
      tmp64A = C.w[1];

      // Note:
      // f* has 384 bits (more than 320 bits)
      // x - 1 = q - 35 = ind + 23
      // add two 64-bit words; e.g. for ind=0 <=> q=58, add 1/2*10^24
      C.w[0] = C.w[0] + __bid_midpoint128[ind + 4].w[0];
      C.w[1] = C.w[1] + __bid_midpoint128[ind + 4].w[1];
      if (C.w[0] < tmp64)
        C.w[1]++;
      if (C.w[1] < tmp64A)
        C.w[2]++;
      if (C.w[2] == 0)
        C.w[3]++;
      __mul_256x256_to_512 (P512, C, __bid_ten2mk256M[ind])
        // calculate C* and f*; C* is actually floor(C*) in this case
        // C* and f* need shifting and masking, as shown by 
        // __bid_shiftright256M[] and __bid_maskhigh256M[]
        // C* has 128 bits; P512.w[7], P512.w[6], P512.w[5] need to be
        // shifted right by Ex-320 = __bid_shiftright256M[ind]
        shift = __bid_shiftright256M[ind]; // 15 <= shift <= 48
      if (shift == 32) {
        Cstar.w[0] =
          ((P512.w[5] >> 31) >> 1) | ((P512.w[6] << 31) << 1);
        Cstar.w[1] =
          ((P512.w[6] >> 31) >> 1) | ((P512.w[7] << 31) << 1);
      } else {
        Cstar.w[0] = (P512.w[5] >> shift) | (P512.w[6] << (64 - shift));
        Cstar.w[1] = (P512.w[6] >> shift) | (P512.w[7] << (64 - shift));
      }
      // f* has 384 bits
      fstar.w[5] = P512.w[5] & __bid_maskhigh256M[ind];
      fstar.w[4] = P512.w[4];
      fstar.w[3] = P512.w[3];
      fstar.w[2] = P512.w[2];
      fstar.w[1] = P512.w[1];
      fstar.w[0] = P512.w[0];

      // the top Ex bits of 10^(-x) are T* = __bid_ten2mk256truncM[ind], e.g. 
      // if x=24, T* = __bid_ten2mk256truncM[0] =
      //   0x9abe14cd44753b52c4926a9672793542d78c3615cf3a050cf23472530ce6e3ec =~
      //   10^(-24) * 2^335
      // if (0 < f* < 10^(-x)) then the result is a midpoint
      //   if floor(C*) is even then C* = floor(C*) - logical right
      //       shift; C* has p decimal digits, correct by Prop. 1)
      //   else if floor(C*) is odd C* = floor(C*)-1 (logical right
      //       shift; C* has p decimal digits, correct by Pr. 1)
      // else
      //   C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // n = C* * 10^(e+x)

      // determine inexactness of the rounding of C*
      // if (0 < f* - 1/2 < T* ~= 10^(-x)) then
      //   the result is exact
      // else // if (f* - 1/2 >= T*) then
      //   the result is inexact
      if (fstar.w[5] > __bid_one_half256M[ind]
          || (fstar.w[5] == __bid_one_half256M[ind]
          && (fstar.w[4] || fstar.w[3] || fstar.w[2] || fstar.w[1]
              || fstar.w[0]))) {

        // f* > 1/2 and the result may be exact
        // Calculate f* - 1/2
        tmp64 = fstar.w[5] - __bid_one_half256M[ind]; // tmp64 >= 0
        if (tmp64 || fstar.w[4] || fstar.w[3] > __bid_ten2mk256truncM[ind].w[3] || 
            (fstar.w[3] == __bid_ten2mk256truncM[ind].w[3] && 
            fstar.w[2] > __bid_ten2mk256truncM[ind].w[2]) || 
            (fstar.w[3] == __bid_ten2mk256truncM[ind].w[3] && 
            fstar.w[2] == __bid_ten2mk256truncM[ind].w[2] && 
            fstar.w[1] > __bid_ten2mk256truncM[ind].w[1]) || 
            (fstar.w[3] == __bid_ten2mk256truncM[ind].w[3] && 
            fstar.w[2] == __bid_ten2mk256truncM[ind].w[2] && 
            fstar.w[1] == __bid_ten2mk256truncM[ind].w[1] && 
            fstar.w[0] > __bid_ten2mk256truncM[ind].w[0])) { // f* - 1/2 > 10^(-x)
          // set the inexact flag
          *pfpsf |= INEXACT_EXCEPTION;
          is_inexact_lt_midpoint = 1;
        } // else the result is exact
      } else { // the result is inexact; f2* <= 1/2
        // set the inexact flag
        *pfpsf |= INEXACT_EXCEPTION;
        tmp_fpa = 1;
        is_inexact_gt_midpoint = 1;
      }

      // check for midpoints (could do this before determining inexactness)
      if ((fstar.w[5] == 0) && (fstar.w[4] == 0)
          && (fstar.w[3] || fstar.w[2] || fstar.w[1] || fstar.w[0])
          && (fstar.w[3] < __bid_ten2mk256truncM[ind].w[3]
              || (fstar.w[3] == __bid_ten2mk256truncM[ind].w[3]
              && fstar.w[2] < __bid_ten2mk256truncM[ind].w[2])
              || (fstar.w[3] == __bid_ten2mk256truncM[ind].w[3]
              && fstar.w[2] == __bid_ten2mk256truncM[ind].w[2]
              && fstar.w[1] < __bid_ten2mk256truncM[ind].w[1])
              || (fstar.w[3] == __bid_ten2mk256truncM[ind].w[3]
              && fstar.w[2] == __bid_ten2mk256truncM[ind].w[2]
              && fstar.w[1] == __bid_ten2mk256truncM[ind].w[1]
              && fstar.w[0] <= __bid_ten2mk256truncM[ind].w[1]))) {

        // the result is a midpoint
        if (Cstar.w[0] & 0x01) { // Cstar.w[0] is odd; MP in [EVEN, ODD]
          // if floor(C*) is odd C = floor(C*) - 1; the result may be 0
          Cstar.w[0]--; // Cstar.w[0] is now even
          if (tmp_fpa == 1)
            tmp_fpa = 0;
          is_midpoint_gt_even = 1;
          is_inexact_lt_midpoint = 0;
          is_inexact_gt_midpoint = 0;
        } else { // else MP in [ODD, EVEN]
          is_midpoint_lt_even = 1;
          is_inexact_lt_midpoint = 0;
          is_inexact_gt_midpoint = 0;
        }
      }
      // check for rounding overflow
      if (Cstar.w[1] == 0x0001ed09bead87c0ull && 
          Cstar.w[0] == 0x378d8e6400000000ull) { // if  Cstar = 10^34
        tmp64 = x_exp + y_exp + ((UINT64) (ind + 25) << 49);
        Cstar.w[1] = 0x0000314dc6448d93ull; // Cstar = 10^33
        Cstar.w[0] = 0x38c15b0a00000000ull;
      } else { // 10^33 <= Cstar <= 10^34 - 1
        tmp64 = x_exp + y_exp + ((UINT64) (ind + 24) << 49); // ind+24 = q-34
      }
      if (tmp64 >= EXP_MAX + BIN_EXP_BIAS) { // possibble overflow
        // exp >= emax for the result rounded to nearest even
        if (rnd_mode == ROUNDING_TO_NEAREST
            || rnd_mode == ROUNDING_TIES_AWAY) {
          if (tmp64 > EXP_MAX + BIN_EXP_BIAS) {

            // |res| >= 10^(p-1) * 10^(emax+1) <=> exp >= emax+1
            res.w[1] = sign | 0x7800000000000000ull; // +/-inf
            res.w[0] = 0x0ull;
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else { // not overflow
            res.w[0] = Cstar.w[0];
            res.w[1] = Cstar.w[1];
            res.w[1] |= (tmp64 - BIN_EXP_BIAS);
          }
        } else if (rnd_mode == ROUNDING_DOWN) {
          if (!sign && (tmp64 > EXP_MAX + BIN_EXP_BIAS) && 
              !(tmp64 == EXP_MAX + BIN_EXP_BIAS + EXP_P1 && 
              Cstar.w[1] == 0x0000314dc6448d93ull && 
              Cstar.w[0] == 0x38c15b0a00000000ull && // 10^33 * 10^(emax+1)
              (is_midpoint_lt_even || is_inexact_gt_midpoint))) {
            // res = +MAXFP
            res.w[1] = 0x5fffed09bead87c0ull;
            res.w[0] = 0x378d8e63ffffffffull; // (10^34-1) * 10^emax
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else if (sign && ((tmp64 > EXP_MAX + BIN_EXP_BIAS) || 
              ((tmp64 == EXP_MAX + BIN_EXP_BIAS) && 
              Cstar.w[1] == 0x0001ed09bead87c0ull && 
              Cstar.w[0] == 0x378d8e63ffffffffull && // (10^34-1) * 10^emax
              is_inexact_lt_midpoint))) {
            res.w[1] = 0xf800000000000000ull; // -inf
            res.w[0] = 0x0ull;
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else { // not overflow
            res.w[0] = Cstar.w[0];
            res.w[1] = Cstar.w[1];
            res.w[1] |= (tmp64 - BIN_EXP_BIAS);
          }
        } else if (rnd_mode == ROUNDING_UP) {
          if (sign && (tmp64 > EXP_MAX + BIN_EXP_BIAS) && 
              !(tmp64 == EXP_MAX + BIN_EXP_BIAS + EXP_P1 && 
              Cstar.w[1] == 0x0000314dc6448d93ull && 
              Cstar.w[0] == 0x38c15b0a00000000ull && // 10^33 * 10^(emax+1)
              (is_midpoint_lt_even || is_inexact_gt_midpoint))) {
            // res = -MAXFP
            res.w[1] = 0xdfffed09bead87c0ull;
            res.w[0] = 0x378d8e63ffffffffull; // -(10^34-1) * 10^emax
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else if (!sign && ((tmp64 > EXP_MAX + BIN_EXP_BIAS) || 
              ((tmp64 == EXP_MAX + BIN_EXP_BIAS) && 
              Cstar.w[1] == 0x0001ed09bead87c0ull && 
              Cstar.w[0] == 0x378d8e63ffffffffull && // (10^34-1) * 10^emax
              is_inexact_lt_midpoint))) {
            res.w[1] = 0x7800000000000000ull; // inf
            res.w[0] = 0x0ull;
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else { // not overflow
            res.w[0] = Cstar.w[0];
            res.w[1] = Cstar.w[1];
            res.w[1] |= (tmp64 - BIN_EXP_BIAS);
          }
        } else { // if (rnd_mode == ROUNDING_TO_ZERO)
          if (!sign && (tmp64 > EXP_MAX + BIN_EXP_BIAS) && 
              !(tmp64 == EXP_MAX + BIN_EXP_BIAS + EXP_P1 && 
              Cstar.w[1] == 0x0000314dc6448d93ull && 
              Cstar.w[0] == 0x38c15b0a00000000ull && // 10^33 * 10^(emax+1)
              (is_midpoint_lt_even || is_inexact_gt_midpoint))) {
            // res = +MAXFP
            res.w[1] = 0x5fffed09bead87c0ull;
            res.w[0] = 0x378d8e63ffffffffull; // (10^34-1) * 10^emax
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else if (sign && (tmp64 > EXP_MAX + BIN_EXP_BIAS) && 
              !(tmp64 == EXP_MAX + BIN_EXP_BIAS + EXP_P1 && 
              Cstar.w[1] == 0x0000314dc6448d93ull && 
              Cstar.w[0] == 0x38c15b0a00000000ull && // 10^33 * 10^(emax+1)
              (is_midpoint_lt_even || is_inexact_gt_midpoint))) {
            // res = -MAXFP
            res.w[1] = 0xdfffed09bead87c0ull;
            res.w[0] = 0x378d8e63ffffffffull; // -(10^34-1) * 10^emax
            *pfpsf |= INEXACT_EXCEPTION; // set the inexact flag
            *pfpsf |= OVERFLOW_EXCEPTION; // set the overflow flag
            is_overflow = 1;
          } else { // not overflow
            res.w[0] = Cstar.w[0];
            res.w[1] = Cstar.w[1];
            res.w[1] |= (tmp64 - BIN_EXP_BIAS);
          }
        }
        if (is_overflow) { // return for overflow
          // set the inexact flag
          *pfpsf |= INEXACT_EXCEPTION;

          // set the overflow flag
          *pfpsf |= OVERFLOW_EXCEPTION;

          // is_overflow = 1;
          BID_RETURN (res);
        }
      } else {
        res.w[0] = Cstar.w[0];
        res.w[1] = Cstar.w[1];
        res.w[1] |= (tmp64 - BIN_EXP_BIAS);
      }
      res.w[1] |= sign;
    }

    // general correction from RN to RA, RM, RP, RZ
    if (rnd_mode != ROUNDING_TO_NEAREST && !is_overflow) { // overflow is solved
      x_exp = res.w[1] & MASK_EXP; // biased and shifted left 49 bit positions
      C1_hi = res.w[1] & MASK_COEFF;
      C1_lo = res.w[0];
      if ((!sign && ((rnd_mode == ROUNDING_UP && is_inexact_lt_midpoint) || 
          ((rnd_mode == ROUNDING_TIES_AWAY || rnd_mode == ROUNDING_UP) && 
          is_midpoint_gt_even))) || 
          (sign && ((rnd_mode == ROUNDING_DOWN && is_inexact_lt_midpoint) || 
          ((rnd_mode == ROUNDING_TIES_AWAY || rnd_mode == ROUNDING_DOWN) && 
          is_midpoint_gt_even)))) {

        // C1 = C1 + 1
        C1_lo = C1_lo + 1;
        if (C1_lo == 0) { // rounding overflow in the low 64 bits
          C1_hi = C1_hi + 1;
          if (C1_hi == 0x0001ed09bead87c0ull
              && C1_lo == 0x378d8e6400000000ull) {

            // C1 = 10^34 => rounding overflow
            C1_hi = 0x0000314dc6448d93ull;
            C1_lo = 0x38c15b0a00000000ull; // 10^33
            x_exp = x_exp + EXP_P1;
          }
        }
      } else if ((is_midpoint_lt_even || is_inexact_gt_midpoint)
          && ((sign && (rnd_mode == ROUNDING_UP || 
          rnd_mode == ROUNDING_TO_ZERO)) || 
          (!sign && (rnd_mode == ROUNDING_DOWN || 
          rnd_mode == ROUNDING_TO_ZERO)))) {

        // C1 = C1 - 1
        C1_lo = C1_lo - 1;
        if (C1_lo == 0xffffffffffffffffull)
          C1_hi--;

        // check if we crossed into the lower decade
        if (C1_hi == 0x0000314dc6448d93ull && C1_lo == 0x38c15b09ffffffffull) {
          // 10^33 - 1
          C1_hi = 0x0001ed09bead87c0ull; // 10^34 - 1
          C1_lo = 0x378d8e63ffffffffull;
          x_exp = x_exp - EXP_P1; // no underflow (TO CHECK)
        }
      } else {
        ; // exact, the result is already correct
      }

      // assemble the result
      res.w[1] = x_exp | C1_hi;
      res.w[0] = C1_lo;
    }
    res.w[1] |= sign;
    BID_RETURN (res);
  }
_underflow_path:
  // got here because q - P34 < ind where ind = EMIN - ex - ey
  // q is the number of digits in C; ind is the [positive] exponent of the
  // negative power of 10 that must multiply C in order to make the result's
  // exponent equal to e_min - P34 + 1 = -6176
  ind =
    (int) (((SINT64) EXP_MIN + (SINT64) BIN_EXP_BIAS - (SINT64) x_exp -
            (SINT64) y_exp) >> 49);

  // q - P34 < ind => -P34 + 1 < ind => -P34 + 2 <= ind
  // ind = EMIN - ex - ey < -6176 + 6176 + 6176 = 6176
  if (q < ind) { // q - ind < 0; result rounds to 0 when rounding to nearest
    // set the inexact and underflow flags
    *pfpsf |= (INEXACT_EXCEPTION | UNDERFLOW_EXCEPTION);
    res.w[1] = EXP_MIN; // EXP_MIN = 0x0
    res.w[0] = 0x0;
    if (rnd_mode != ROUNDING_TO_NEAREST) {
      if ((rnd_mode == ROUNDING_DOWN && sign) || 
          (rnd_mode == ROUNDING_UP && !sign))
        res.w[0] = 0x0000000000000001ull;
    }
  } else if (q == ind) { // q - ind = 0; result rounds to 0 or +/-1*10^EMIN
    // set the inexact and underflow flags
    *pfpsf |= (INEXACT_EXCEPTION | UNDERFLOW_EXCEPTION);

    // if C <= 5*10^(q-1) then C = 0 else C = 1
    if (q <= 19) {
      if (C.w[0] == __bid_midpoint64[q - 1]) { // C = 0.5 * 10^emin
        if (rnd_mode == ROUNDING_TO_NEAREST || (rnd_mode == ROUNDING_DOWN
            && !sign) || (rnd_mode == ROUNDING_UP && sign)
            || rnd_mode == ROUNDING_TO_ZERO) {
          res.w[1] = EXP_MIN;
          res.w[0] = 0;
        } else {
          res.w[1] = EXP_MIN;
          res.w[0] = 1;
        }
      } else if (C.w[0] < __bid_midpoint64[q - 1]) { // C < 0.5 * 10^emin
        if (rnd_mode == ROUNDING_TO_NEAREST
            || rnd_mode == ROUNDING_TIES_AWAY
            || (rnd_mode == ROUNDING_DOWN && !sign)
            || (rnd_mode == ROUNDING_UP && sign)
            || rnd_mode == ROUNDING_TO_ZERO) {
          res.w[1] = EXP_MIN;
          res.w[0] = 0;
        } else {
          res.w[1] = EXP_MIN;
          res.w[0] = 1;
        }
      } else { // C > 0.5 * 10^emin
        if (rnd_mode == ROUNDING_TO_NEAREST
            || rnd_mode == ROUNDING_TIES_AWAY
            || (rnd_mode == ROUNDING_DOWN && sign)
            || (rnd_mode == ROUNDING_UP && !sign)) {
          res.w[1] = EXP_MIN;
          res.w[0] = 1;
        } else {
          res.w[1] = EXP_MIN;
          res.w[0] = 0;
        }
      }
    } else if (q <= 38) { // 20 <= q <= 38
      // if q <= P34 = 34 the exact result rounded to P34 digits with unbounded 
      // exponent will have an exponent smaller than e_min; otherwise if
      // 35 <= q <= 38, it depends
      if (C.w[1] == __bid_midpoint128[q - 20].w[1] && 
          C.w[0] == __bid_midpoint128[q - 20].w[0]) { // C = 0.5 * 10^emin
        if (rnd_mode == ROUNDING_TO_NEAREST || (rnd_mode == ROUNDING_DOWN
            && !sign) || (rnd_mode == ROUNDING_UP && sign)
            || rnd_mode == ROUNDING_TO_ZERO) {
          res.w[1] = EXP_MIN;
          res.w[0] = 0;
        } else {
          res.w[1] = EXP_MIN;
          res.w[0] = 1;
        }
      } else if (C.w[1] < __bid_midpoint128[q - 20].w[1] || 
          (C.w[1] == __bid_midpoint128[q - 20].w[1] && 
          C.w[0] < __bid_midpoint128[q - 20].w[0])) { // C < 0.5 * 10^emin 
        if (rnd_mode == ROUNDING_TO_NEAREST
            || rnd_mode == ROUNDING_TIES_AWAY
            || (rnd_mode == ROUNDING_DOWN && !sign)
            || (rnd_mode == ROUNDING_UP && sign)
            || rnd_mode == ROUNDING_TO_ZERO) {
          res.w[1] = EXP_MIN;
          res.w[0] = 0;
        } else {
          res.w[1] = EXP_MIN;
          res.w[0] = 1;
        }
      } else { // C > 0.5 * 10^emin
        if (rnd_mode == ROUNDING_TO_NEAREST
            || rnd_mode == ROUNDING_TIES_AWAY
            || (rnd_mode == ROUNDING_DOWN && sign)
            || (rnd_mode == ROUNDING_UP && !sign)) {
          res.w[1] = EXP_MIN;
          res.w[0] = 1;
        } else {
          res.w[1] = EXP_MIN;
          res.w[0] = 0;
        }
      }
    } else if (q <= 58) { // 39 <= q <= 58
      // Note: for q = 58 C may take 256 bits, so need to test C.w[3]
      if (C.w[3] == 0x0 && C.w[2] == __bid_midpoint192[q - 39].w[2] && 
          C.w[1] == __bid_midpoint192[q - 39].w[1] && 
          C.w[0] == __bid_midpoint192[q - 39].w[0]) { // C = 0.5 * 10^emin
        if (rnd_mode == ROUNDING_TO_NEAREST || (rnd_mode == ROUNDING_DOWN
            && !sign) || (rnd_mode == ROUNDING_UP && sign)
            || rnd_mode == ROUNDING_TO_ZERO) {
          res.w[1] = EXP_MIN;
          res.w[0] = 0;
        } else {
          res.w[1] = EXP_MIN;
          res.w[0] = 1;
        }
      } else if ((C.w[3] == 0x0 && C.w[2] < __bid_midpoint192[q - 39].w[2]) || 
          (C.w[3] == 0x0 && C.w[2] == __bid_midpoint192[q - 39].w[2] && 
          C.w[1] < __bid_midpoint192[q - 39].w[1]) || (C.w[3] == 0x0 && 
          C.w[2] == __bid_midpoint192[q - 39].w[2] && 
          C.w[1] == __bid_midpoint192[q - 39].w[1] && 
          C.w[0] < __bid_midpoint192[q - 39].w[0])) { // C < 0.5 * 10^emin
        if (rnd_mode == ROUNDING_TO_NEAREST
            || rnd_mode == ROUNDING_TIES_AWAY
            || (rnd_mode == ROUNDING_DOWN && !sign)
            || (rnd_mode == ROUNDING_UP && sign)
            || rnd_mode == ROUNDING_TO_ZERO) {
          res.w[1] = EXP_MIN;
          res.w[0] = 0;
        } else {
          res.w[1] = EXP_MIN;
          res.w[0] = 1;
        }
      } else { // C > 0.5 * 10^emin
        if (rnd_mode == ROUNDING_TO_NEAREST
            || rnd_mode == ROUNDING_TIES_AWAY
            || (rnd_mode == ROUNDING_DOWN && sign)
            || (rnd_mode == ROUNDING_UP && !sign)) {
          res.w[1] = EXP_MIN;
          res.w[0] = 1;
        } else {
          res.w[1] = EXP_MIN;
          res.w[0] = 0;
        }
      }
    } else { // if (q <= 68), i.e. 59 <= q <= 68
      if (C.w[3] == __bid_midpoint256[q - 59].w[3] && 
          C.w[2] == __bid_midpoint256[q - 59].w[2] && 
          C.w[1] == __bid_midpoint256[q - 59].w[1] && 
          C.w[0] == __bid_midpoint256[q - 59].w[0]) { // C = 0.5 * 10^emin
        if (rnd_mode == ROUNDING_TO_NEAREST || (rnd_mode == ROUNDING_DOWN
            && !sign) || (rnd_mode == ROUNDING_UP && sign)
            || rnd_mode == ROUNDING_TO_ZERO) {
          res.w[1] = EXP_MIN;
          res.w[0] = 0;
        } else {
          res.w[1] = EXP_MIN;
          res.w[0] = 1;
        }
      } else if (C.w[3] < __bid_midpoint256[q - 59].w[3] || 
          (C.w[3] == __bid_midpoint256[q - 59].w[3] && 
          C.w[2] < __bid_midpoint256[q - 59].w[2]) || 
          (C.w[3] == __bid_midpoint256[q - 59].w[3] && 
          C.w[2] == __bid_midpoint256[q - 59].w[2] && 
          C.w[1] < __bid_midpoint256[q - 59].w[1]) || 
          (C.w[3] == __bid_midpoint256[q - 59].w[3] && 
          C.w[2] == __bid_midpoint256[q - 59].w[2] && 
          C.w[1] == __bid_midpoint256[q - 59].w[1] && 
          C.w[0] < __bid_midpoint256[q - 59].w[0])) { // C < 0.5 * 10^emin
        if (rnd_mode == ROUNDING_TO_NEAREST
            || rnd_mode == ROUNDING_TIES_AWAY
            || (rnd_mode == ROUNDING_DOWN && !sign)
            || (rnd_mode == ROUNDING_UP && sign)
            || rnd_mode == ROUNDING_TO_ZERO) {
          res.w[1] = EXP_MIN;
          res.w[0] = 0;
        } else {
          res.w[1] = EXP_MIN;
          res.w[0] = 1;
        }
      } else { // C > 0.5 * 10^emin
        if (rnd_mode == ROUNDING_TO_NEAREST
            || rnd_mode == ROUNDING_TIES_AWAY
            || (rnd_mode == ROUNDING_DOWN && sign)
            || (rnd_mode == ROUNDING_UP && !sign)) {
          res.w[1] = EXP_MIN;
          res.w[0] = 1;
        } else {
          res.w[1] = EXP_MIN;
          res.w[0] = 0;
        }
      }
    }
  } else { // if 0 < q - ind < P34 <=> 1 <= q - ind <= P34 - 1 = 33
    // In general -P34 + 2 <= ind <= 6176 => -P34 + 2 <= ind < q =>
    // -P34 + 2 <= ind <= q - 1
    if (rnd_mode != ROUNDING_TO_NEAREST) {
      is_inexact_lt_midpoint = 0;
      is_inexact_gt_midpoint = 0;
      is_midpoint_lt_even = 0;
      is_midpoint_gt_even = 0;
    }
    if (ind <= 0) { // 0 <= -ind
      // the result is exact
      res.w[1] = (x_exp + y_exp - BIN_EXP_BIAS) | C.w[1];
      res.w[0] = C.w[0];

      // because the result is exact the U and I status flags are not set
    } else {

      // if ind > 0 <=> 1 <= ind <= q - 1; must remove ind digits
      // from C, which may have up to 68 digits; note that q >= ind + 1 >= 2
      // Note: there is no underflow in some cases when the coefficient of
      // the result is 10^33 or 10^33 - 1
      if (q <= 18) { // 2 <= q <= 18
        __bid_round64_2_18 (q, ind, C.w[0], &res.w[0], &incr_exp,
                      &is_midpoint_lt_even, &is_midpoint_gt_even,
                      &is_inexact_lt_midpoint, &is_inexact_gt_midpoint);
        if (incr_exp) {

          // multiply by 10; this cannot be 10^33
          __mul_64x64_to_128MACH (res, res.w[0], __bid_ten2k64[1]);
          res.w[1] |= (UINT64) EXP_MIN;
        } else { // underflow
          res.w[1] = (UINT64) EXP_MIN;
        }
        if (is_midpoint_lt_even || is_midpoint_gt_even
            || is_inexact_lt_midpoint || is_inexact_gt_midpoint) {

          // set the inexact and underflow flags
          *pfpsf |= (INEXACT_EXCEPTION | UNDERFLOW_EXCEPTION);
        }
      } else if (q <= 38) { // 19 <= q <= 38
        P128.w[1] = C.w[1];
        P128.w[0] = C.w[0];
        __bid_round128_19_38 (q, ind, P128, &res, &incr_exp,
                        &is_midpoint_lt_even, &is_midpoint_gt_even,
                        &is_inexact_lt_midpoint,
                        &is_inexact_gt_midpoint);
        if (incr_exp) {

          // multiply by 10 and check is this is 10^33, because in that case
          // it is possible that this is not underflow
          if (q - ind <= 19) {
            __mul_64x64_to_128MACH (res, res.w[0], __bid_ten2k64[1]);
          } else { // if 20 <= q - ind
            __mul_128x64_to_128 (res, __bid_ten2k64[1], res);
          }
          if ((q - ind + 1) == P34) { // the result is 10^(P34-1)
            // if the result rounded directly to P34 digits is the same, then
            // there is no underflow
            __bid_round128_19_38 (q, ind - 1, P128, &R128, &incr_exp1,
                            &is_midpoint_lt_even1,
                            &is_midpoint_gt_even1,
                            &is_inexact_lt_midpoint1,
                            &is_inexact_gt_midpoint1);
            if (res.w[1] == R128.w[1] && res.w[0] == R128.w[0]) {
              no_underflow = 1;
            }
          }
          // res.w[1] |= (UINT64)EXP_MIN; // redundant
        } else { // underflow
          // res.w[1] = (UINT64)EXP_MIN | res.w[1]; // redundant
        }
        if (is_midpoint_lt_even || is_midpoint_gt_even
            || is_inexact_lt_midpoint || is_inexact_gt_midpoint) {

          // set the inexact and underflow flags
          *pfpsf |= INEXACT_EXCEPTION;
          is_inexact = 1;
          if (!no_underflow)
            *pfpsf |= UNDERFLOW_EXCEPTION;
        }
      } else if (q <= 57) { // 39 <= q <= 57
        P192.w[2] = C.w[2];
        P192.w[1] = C.w[1];
        P192.w[0] = C.w[0];
        __bid_round192_39_57 (q, ind, P192, &R192, &incr_exp,
                        &is_midpoint_lt_even, &is_midpoint_gt_even,
                        &is_inexact_lt_midpoint,
                        &is_inexact_gt_midpoint);
        if (incr_exp) {

          // multiply by 10 and check is this is 10^33, because in that case
          // it is possible that this is not underflow
          res.w[1] = R192.w[1]; // res has q - ind digits
          res.w[0] = R192.w[0];
          if (q - ind <= 19) {
            __mul_64x64_to_128MACH (res, res.w[0], __bid_ten2k64[1]);
          } else { // if 20 <= q - ind
            __mul_128x64_to_128 (res, __bid_ten2k64[1], res);
          }
          if ((q - ind + 1) == P34) { // the result is 10^(P34-1) 
            // if the result rounded directly to P34 digits is the same, then
            // there is no underflow
            __bid_round192_39_57 (q, ind - 1, P192, &R192, &incr_exp1,
                            &is_midpoint_lt_even1,
                            &is_midpoint_gt_even1,
                            &is_inexact_lt_midpoint1,
                            &is_inexact_gt_midpoint1);
            if (res.w[1] == R192.w[1] && res.w[0] == R192.w[0]) {
              no_underflow = 1;
            }
          }
          // res.w[1] |= (UINT64)EXP_MIN; // redundant
        } else { // underflow
          res.w[1] = (UINT64) EXP_MIN | R192.w[1];
          res.w[0] = R192.w[0];
        }
        if (is_midpoint_lt_even || is_midpoint_gt_even
            || is_inexact_lt_midpoint || is_inexact_gt_midpoint) {

          // set the inexact and underflow flags
          *pfpsf |= INEXACT_EXCEPTION;
          is_inexact = 1;
          if (!no_underflow)
            *pfpsf |= UNDERFLOW_EXCEPTION;
        }
      } else if (q <= 76) { // 58 <= q <= 76 (actually 58 <= q <= 68)
        P256.w[3] = C.w[3];
        P256.w[2] = C.w[2];
        P256.w[1] = C.w[1];
        P256.w[0] = C.w[0];
        __bid_round256_58_76 (q, ind, P256, &R256, &incr_exp,
                        &is_midpoint_lt_even, &is_midpoint_gt_even,
                        &is_inexact_lt_midpoint,
                        &is_inexact_gt_midpoint);
        if (incr_exp) {

          // multiply by 10 and check is this is 10^33, because in that case
          // it is possible that this is not underflow
          res.w[1] = R256.w[1]; // res has q - ind digits
          res.w[0] = R256.w[0];
          if (q - ind <= 19) {
            __mul_64x64_to_128MACH (res, res.w[0], __bid_ten2k64[1]);
          } else { // if 20 <= q - ind
            __mul_128x64_to_128 (res, __bid_ten2k64[1], res);
          }
          if ((q - ind + 1) == P34) { // the result is 10^(P34-1) 
            // if the result rounded directly to P34 digits is the same, then
            // there is no underflow
            __bid_round256_58_76 (q, ind - 1, P256, &R256, &incr_exp1,
                            &is_midpoint_lt_even1,
                            &is_midpoint_gt_even1,
                            &is_inexact_lt_midpoint1,
                            &is_inexact_gt_midpoint1);
            if (res.w[1] == R256.w[1] && res.w[0] == R256.w[0]) {
              no_underflow = 1;
            }
          }
          // res.w[1] |= (UINT64)EXP_MIN; // redundant
        } else { // underflow
          res.w[1] = (UINT64) EXP_MIN | R256.w[1];
          res.w[0] = R256.w[0];
        }
        if (is_midpoint_lt_even || is_midpoint_gt_even
            || is_inexact_lt_midpoint || is_inexact_gt_midpoint) {

          // set the inexact and underflow flags
          *pfpsf |= INEXACT_EXCEPTION;
          is_inexact = 1;
          if (!no_underflow)
            *pfpsf |= UNDERFLOW_EXCEPTION;
        }
      }
    }

    // general correction from RN to RA, RM, RP, RZ
    if (rnd_mode != ROUNDING_TO_NEAREST) {
      x_exp = res.w[1] & MASK_EXP; // biased and shifted left 49 bit positions
      // this must be e_min
      C1_hi = res.w[1] & MASK_COEFF;
      C1_lo = res.w[0];
      if ((!sign && ((rnd_mode == ROUNDING_UP && is_inexact_lt_midpoint) || 
          ((rnd_mode == ROUNDING_TIES_AWAY || rnd_mode == ROUNDING_UP) && 
          is_midpoint_gt_even))) || 
          (sign && ((rnd_mode == ROUNDING_DOWN && is_inexact_lt_midpoint) || 
          ((rnd_mode == ROUNDING_TIES_AWAY || rnd_mode == ROUNDING_DOWN) && 
          is_midpoint_gt_even)))) {

        // C1 = C1 + 1
        C1_lo = C1_lo + 1;
        if (C1_lo == 0) { // rounding overflow in the low 64 bits
          C1_hi = C1_hi + 1;
          if (C1_hi == 0x0001ed09bead87c0ull
              && C1_lo == 0x378d8e6400000000ull) {

            // C1 = 10^34 => rounding overflow (not possible) TO CHECK
            C1_hi = 0x0000314dc6448d93ull;
            C1_lo = 0x38c15b0a00000000ull; // 10^33
            x_exp = x_exp + EXP_P1; // this must be e_min
          }
        }
      } else if ((is_midpoint_lt_even || is_inexact_gt_midpoint) && 
          ((sign && 
          (rnd_mode == ROUNDING_UP || rnd_mode == ROUNDING_TO_ZERO)) || 
          (!sign && 
          (rnd_mode == ROUNDING_DOWN || rnd_mode == ROUNDING_TO_ZERO)))) {

        // C1 = C1 - 1 (the exponent is emin already)
        C1_lo = C1_lo - 1;
        if (C1_lo == 0xffffffffffffffffull)
          C1_hi--;

        // cannot cross into the lower decade anymore, but the result can be 0
      } else {
        ; // exact, the result is already correct
      }

      // no overflow is possible
      // assemble the result
      res.w[1] = x_exp | C1_hi;
      res.w[0] = C1_lo;

      // Now fix the case where the general rounding routine returned a non-tiny
      // result, but after the correction for rounding modes other than to
      // nearest, the result is less in magnitude than 100...0[34] * 10^(-6176)
      // (this is due to the fact that the general rounding routine works only
      // with rounding to nearest)
      if (is_inexact && (x_exp == EXP_MIN)
          && (C1_hi < 0x0000314dc6448d93ull
              || (C1_hi == 0x0000314dc6448d93ull
              && C1_lo < 0x38c15b0a00000000ull))) {
        *pfpsf |= UNDERFLOW_EXCEPTION;
      }
    }
  }
  res.w[1] |= sign;
  BID_RETURN (res);
}
