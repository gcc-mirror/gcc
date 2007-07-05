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

/*****************************************************************************
 *  BID128 nextup
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
__bid128_nextup (UINT128 * pres,
               UINT128 *
               px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  UINT128 x = *px;
#else
UINT128
__bid128_nextup (UINT128 x _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
               _EXC_INFO_PARAM) {
#endif

  UINT128 res;
  UINT64 x_sign;
  UINT64 x_exp;
  int exp;
  BID_UI64DOUBLE tmp1;
  int x_nr_bits;
  int q1, ind;
  UINT128 C1;   // C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (UINT64)

  BID_SWAP128(x);
  // unpack the argument
  x_sign = x.w[1] & MASK_SIGN; // 0 for positive, MASK_SIGN for negative
  x_exp = x.w[1] & MASK_EXP; // biased and shifted left 49 bit positions
  C1.w[1] = x.w[1] & MASK_COEFF;
  C1.w[0] = x.w[0];

  // check for NaN or Infinity
  if ((x.w[1] & MASK_SPECIAL) == MASK_SPECIAL) {
    // x is special
    if ((x.w[1] & MASK_NAN) == MASK_NAN) { // x is NAN
      if ((x.w[1] & MASK_SNAN) == MASK_SNAN) { // x is SNAN
        // set invalid flag
        *pfpsf |= INVALID_EXCEPTION;
        // return quiet (x)
        res.w[1] = x.w[1] & 0xfdffffffffffffffull;
        res.w[0] = x.w[0];
      } else { // x is QNaN
        // return x
        res.w[1] = x.w[1];
        res.w[0] = x.w[0];
      }
    } else { // x is not NaN, so it must be infinity
      if (!x_sign) { // x is +inf
        res.w[1] = 0x7800000000000000ull; // +inf
        res.w[0] = 0x0000000000000000ull;
      } else { // x is -inf
        res.w[1] = 0xdfffed09bead87c0ull; // -MAXFP = -999...99 * 10^emax
        res.w[0] = 0x378d8e63ffffffffull;
      }
    }
    BID_RETURN (res);
  }
  // test for non-canonical values of the argument x
  // - values whose encoding begins with x00, x01, or x10 and whose
  //   coefficient is larger than 10^34 -1, or
  // - values whose encoding begins with x1100, x1101, x1110 (if NaNs
  //   and infinitis were eliminated already this test is reduced to
  //   checking for x10x)
  if ((((C1.w[1] > 0x0001ed09bead87c0ull) || 
      ((C1.w[1] == 0x0001ed09bead87c0ull) && (C1.w[0] > 0x378d8e63ffffffffull)))
      && ((x.w[1] & 0x6000000000000000ull) != 0x6000000000000000ull)) || 
      ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull)) {
    x.w[1] = 0;
    x.w[0] = 0;
    C1.w[1] = 0;
    C1.w[0] = 0;
  }

  if ((C1.w[1] == 0x0ull) && (C1.w[0] == 0x0ull)) {
    // x is +/-0
    res.w[1] = 0x0000000000000000ull; // +1 * 10^emin
    res.w[0] = 0x0000000000000001ull;
  } else { // x is not special and is not zero
    if (x.w[1] == 0x5fffed09bead87c0ull
        && x.w[0] == 0x378d8e63ffffffffull) {
      // x = +MAXFP = 999...99 * 10^emax
      res.w[1] = 0x7800000000000000ull; // +inf
      res.w[0] = 0x0000000000000000ull;
    } else if (x.w[1] == 0x8000000000000000ull
               && x.w[0] == 0x0000000000000001ull) {
      // x = -MINFP = 1...99 * 10^emin
      res.w[1] = 0x8000000000000000ull; // -0
      res.w[0] = 0x0000000000000000ull;
    } else { // -MAXFP <= x <= -MINFP - 1 ulp OR MINFP <= x <= MAXFP - 1 ulp
      // can add/subtract 1 ulp to the significand

      // Note: we could check here if x >= 10^34 to speed up the case q1 = 34
      // q1 = nr. of decimal digits in x
      // determine first the nr. of bits in x
      if (C1.w[1] == 0) {
        if (C1.w[0] >= 0x0020000000000000ull) { // x >= 2^53
          // split the 64-bit value in two 32-bit halves to avoid rnd errors
          if (C1.w[0] >= 0x0000000100000000ull) { // x >= 2^32
            tmp1.d = (double) (C1.w[0] >> 32); // exact conversion
            x_nr_bits =
              33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) -
                    0x3ff);
          } else { // x < 2^32
            tmp1.d = (double) (C1.w[0]); // exact conversion
            x_nr_bits =
              1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
          }
        } else { // if x < 2^53
          tmp1.d = (double) C1.w[0]; // exact conversion
          x_nr_bits =
            1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
        }
      } else { // C1.w[1] != 0 => nr. bits = 64 + nr_bits (C1.w[1])
        tmp1.d = (double) C1.w[1]; // exact conversion
        x_nr_bits =
          65 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      }
      q1 = __bid_nr_digits[x_nr_bits - 1].digits;
      if (q1 == 0) {
        q1 = __bid_nr_digits[x_nr_bits - 1].digits1;
        if (C1.w[1] > __bid_nr_digits[x_nr_bits - 1].threshold_hi
            || (C1.w[1] == __bid_nr_digits[x_nr_bits - 1].threshold_hi
            && C1.w[0] >= __bid_nr_digits[x_nr_bits - 1].threshold_lo))
          q1++;
      }
      // if q1 < P34 then pad the significand with zeros
      if (q1 < P34) {
        exp = (x_exp >> 49) - 6176;
        if (exp + 6176 > P34 - q1) {
          ind = P34 - q1; // 1 <= ind <= P34 - 1
          // pad with P34 - q1 zeros, until exponent = emin
          // C1 = C1 * 10^ind
          if (q1 <= 19) { // 64-bit C1
            if (ind <= 19) { // 64-bit 10^ind and 64-bit C1
              __mul_64x64_to_128MACH (C1, C1.w[0], __bid_ten2k64[ind]);
            } else { // 128-bit 10^ind and 64-bit C1
              __mul_128x64_to_128 (C1, C1.w[0], __bid_ten2k128[ind - 20]);
            }
          } else { // C1 is (most likely) 128-bit
            if (ind <= 14) { // 64-bit 10^ind and 128-bit C1 (most likely)
              __mul_128x64_to_128 (C1, __bid_ten2k64[ind], C1);
            } else if (ind <= 19) { // 64-bit 10^ind and 64-bit C1 (q1 <= 19)
              __mul_64x64_to_128MACH (C1, C1.w[0], __bid_ten2k64[ind]);
            } else { // 128-bit 10^ind and 64-bit C1 (C1 must be 64-bit)
              __mul_128x64_to_128 (C1, C1.w[0], __bid_ten2k128[ind - 20]);
            }
          }
          x_exp = x_exp - ((UINT64) ind << 49);
        } else { // pad with zeros until the exponent reaches emin
          ind = exp + 6176;
          // C1 = C1 * 10^ind
          if (ind <= 19) { // 1 <= P34 - q1 <= 19 <=> 15 <= q1 <= 33
            if (q1 <= 19) { // 64-bit C1, 64-bit 10^ind 
              __mul_64x64_to_128MACH (C1, C1.w[0], __bid_ten2k64[ind]);
            } else { // 20 <= q1 <= 33 => 128-bit C1, 64-bit 10^ind
              __mul_128x64_to_128 (C1, __bid_ten2k64[ind], C1);
            }
          } else { // if 20 <= P34 - q1 <= 33 <=> 1 <= q1 <= 14 =>
            // 64-bit C1, 128-bit 10^ind
            __mul_128x64_to_128 (C1, C1.w[0], __bid_ten2k128[ind - 20]);
          }
          x_exp = EXP_MIN;
        }
      }
      if (!x_sign) { // x > 0
        // add 1 ulp (add 1 to the significand)
        C1.w[0]++;
        if (C1.w[0] == 0)
          C1.w[1]++;
        if (C1.w[1] == 0x0001ed09bead87c0ull && 
            C1.w[0] == 0x378d8e6400000000ull) { // if  C1 = 10^34
          C1.w[1] = 0x0000314dc6448d93ull; // C1 = 10^33
          C1.w[0] = 0x38c15b0a00000000ull;
          x_exp = x_exp + EXP_P1;
        }
      } else { // x < 0
        // subtract 1 ulp (subtract 1 from the significand)
        C1.w[0]--;
        if (C1.w[0] == 0xffffffffffffffffull)
          C1.w[1]--;
        if (x_exp != 0 && C1.w[1] == 0x0000314dc6448d93ull && 
            C1.w[0] == 0x38c15b09ffffffffull) { // if  C1 = 10^33 - 1
          C1.w[1] = 0x0001ed09bead87c0ull; // C1 = 10^34 - 1
          C1.w[0] = 0x378d8e63ffffffffull;
          x_exp = x_exp - EXP_P1;
        }
      }
      // assemble the result
      res.w[1] = x_sign | x_exp | C1.w[1];
      res.w[0] = C1.w[0];
    }        // end -MAXFP <= x <= -MINFP - 1 ulp OR MINFP <= x <= MAXFP - 1 ulp
  }        // end x is not special and is not zero
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID128 nextdown
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
__bid128_nextdown (UINT128 * pres,
                 UINT128 *
                 px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  UINT128 x = *px;
#else
UINT128
__bid128_nextdown (UINT128 x _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
                 _EXC_INFO_PARAM) {
#endif

  UINT128 res;
  UINT64 x_sign;
  UINT64 x_exp;
  int exp;
  BID_UI64DOUBLE tmp1;
  int x_nr_bits;
  int q1, ind;
  UINT128 C1;   // C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (UINT64)

  BID_SWAP128(x);
  // unpack the argument
  x_sign = x.w[1] & MASK_SIGN; // 0 for positive, MASK_SIGN for negative
  x_exp = x.w[1] & MASK_EXP; // biased and shifted left 49 bit positions
  C1.w[1] = x.w[1] & MASK_COEFF;
  C1.w[0] = x.w[0];

  // check for NaN or Infinity
  if ((x.w[1] & MASK_SPECIAL) == MASK_SPECIAL) {
    // x is special
    if ((x.w[1] & MASK_NAN) == MASK_NAN) { // x is NAN
      if ((x.w[1] & MASK_SNAN) == MASK_SNAN) { // x is SNAN
        // set invalid flag
        *pfpsf |= INVALID_EXCEPTION;
        // return quiet (x)
        res.w[1] = x.w[1] & 0xfdffffffffffffffull;
        res.w[0] = x.w[0];
      } else { // x is QNaN
        // return x
        res.w[1] = x.w[1];
        res.w[0] = x.w[0];
      }
    } else { // x is not NaN, so it must be infinity
      if (!x_sign) { // x is +inf
        res.w[1] = 0x5fffed09bead87c0ull; // +MAXFP = +999...99 * 10^emax
        res.w[0] = 0x378d8e63ffffffffull;
      } else { // x is -inf
        res.w[1] = 0xf800000000000000ull; // -inf
        res.w[0] = 0x0000000000000000ull;
      }
    }
    BID_RETURN (res);
  }
  // test for non-canonical values of the argument x
  // - values whose encoding begins with x00, x01, or x10 and whose
  //   coefficient is larger than 10^34 -1, or
  // - values whose encoding begins with x1100, x1101, x1110 (if NaNs
  //   and infinitis were eliminated already this test is reduced to
  //   checking for x10x)
  if ((((C1.w[1] > 0x0001ed09bead87c0ull) || ((C1.w[1] == 0x0001ed09bead87c0ull)
      && (C1.w[0] > 0x378d8e63ffffffffull))) && 
      ((x.w[1] & 0x6000000000000000ull) != 0x6000000000000000ull)) || 
      ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull)) {
    x.w[1] = 0;
    x.w[0] = 0;
    C1.w[1] = 0;
    C1.w[0] = 0;
  }

  if ((C1.w[1] == 0x0ull) && (C1.w[0] == 0x0ull)) {
    // x is +/-0
    res.w[1] = 0x8000000000000000ull; // -1 * 10^emin
    res.w[0] = 0x0000000000000001ull;
  } else { // x is not special and is not zero
    if (x.w[1] == 0xdfffed09bead87c0ull
        && x.w[0] == 0x378d8e63ffffffffull) {
      // x = -MAXFP = -999...99 * 10^emax
      res.w[1] = 0xf800000000000000ull; // -inf
      res.w[0] = 0x0000000000000000ull;
    } else if (x.w[1] == 0x0ull && x.w[0] == 0x0000000000000001ull) { // +MINFP
      res.w[1] = 0x0000000000000000ull; // +0
      res.w[0] = 0x0000000000000000ull;
    } else { // -MAXFP <= x <= -MINFP - 1 ulp OR MINFP <= x <= MAXFP - 1 ulp
      // can add/subtract 1 ulp to the significand

      // Note: we could check here if x >= 10^34 to speed up the case q1 = 34
      // q1 = nr. of decimal digits in x
      // determine first the nr. of bits in x
      if (C1.w[1] == 0) {
        if (C1.w[0] >= 0x0020000000000000ull) { // x >= 2^53
          // split the 64-bit value in two 32-bit halves to avoid rnd errors
          if (C1.w[0] >= 0x0000000100000000ull) { // x >= 2^32
            tmp1.d = (double) (C1.w[0] >> 32); // exact conversion
            x_nr_bits =
              33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) -
                    0x3ff);
          } else { // x < 2^32
            tmp1.d = (double) (C1.w[0]); // exact conversion
            x_nr_bits =
              1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
          }
        } else { // if x < 2^53
          tmp1.d = (double) C1.w[0]; // exact conversion
          x_nr_bits =
            1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
        }
      } else { // C1.w[1] != 0 => nr. bits = 64 + nr_bits (C1.w[1])
        tmp1.d = (double) C1.w[1]; // exact conversion
        x_nr_bits =
          65 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      }
      q1 = __bid_nr_digits[x_nr_bits - 1].digits;
      if (q1 == 0) {
        q1 = __bid_nr_digits[x_nr_bits - 1].digits1;
        if (C1.w[1] > __bid_nr_digits[x_nr_bits - 1].threshold_hi
            || (C1.w[1] == __bid_nr_digits[x_nr_bits - 1].threshold_hi
            && C1.w[0] >= __bid_nr_digits[x_nr_bits - 1].threshold_lo))
          q1++;
      }
      // if q1 < P then pad the significand with zeros
      if (q1 < P34) {
        exp = (x_exp >> 49) - 6176;
        if (exp + 6176 > P34 - q1) {
          ind = P34 - q1; // 1 <= ind <= P34 - 1
          // pad with P34 - q1 zeros, until exponent = emin
          // C1 = C1 * 10^ind
          if (q1 <= 19) { // 64-bit C1
            if (ind <= 19) { // 64-bit 10^ind and 64-bit C1
              __mul_64x64_to_128MACH (C1, C1.w[0], __bid_ten2k64[ind]);
            } else { // 128-bit 10^ind and 64-bit C1
              __mul_128x64_to_128 (C1, C1.w[0], __bid_ten2k128[ind - 20]);
            }
          } else { // C1 is (most likely) 128-bit
            if (ind <= 14) { // 64-bit 10^ind and 128-bit C1 (most likely)
              __mul_128x64_to_128 (C1, __bid_ten2k64[ind], C1);
            } else if (ind <= 19) { // 64-bit 10^ind and 64-bit C1 (q1 <= 19)
              __mul_64x64_to_128MACH (C1, C1.w[0], __bid_ten2k64[ind]);
            } else { // 128-bit 10^ind and 64-bit C1 (C1 must be 64-bit)
              __mul_128x64_to_128 (C1, C1.w[0], __bid_ten2k128[ind - 20]);
            }
          }
          x_exp = x_exp - ((UINT64) ind << 49);
        } else { // pad with zeros until the exponent reaches emin
          ind = exp + 6176;
          // C1 = C1 * 10^ind
          if (ind <= 19) { // 1 <= P34 - q1 <= 19 <=> 15 <= q1 <= 33
            if (q1 <= 19) { // 64-bit C1, 64-bit 10^ind 
              __mul_64x64_to_128MACH (C1, C1.w[0], __bid_ten2k64[ind]);
            } else { // 20 <= q1 <= 33 => 128-bit C1, 64-bit 10^ind
              __mul_128x64_to_128 (C1, __bid_ten2k64[ind], C1);
            }
          } else { // if 20 <= P34 - q1 <= 33 <=> 1 <= q1 <= 14 =>
            // 64-bit C1, 128-bit 10^ind
            __mul_128x64_to_128 (C1, C1.w[0], __bid_ten2k128[ind - 20]);
          }
          x_exp = EXP_MIN;
        }
      }
      if (x_sign) { // x < 0
        // add 1 ulp (add 1 to the significand)
        C1.w[0]++;
        if (C1.w[0] == 0)
          C1.w[1]++;
        if (C1.w[1] == 0x0001ed09bead87c0ull && 
            C1.w[0] == 0x378d8e6400000000ull) { // if  C1 = 10^34
          C1.w[1] = 0x0000314dc6448d93ull; // C1 = 10^33
          C1.w[0] = 0x38c15b0a00000000ull;
          x_exp = x_exp + EXP_P1;
        }
      } else { // x > 0
        // subtract 1 ulp (subtract 1 from the significand)
        C1.w[0]--;
        if (C1.w[0] == 0xffffffffffffffffull)
          C1.w[1]--;
        if (x_exp != 0 && C1.w[1] == 0x0000314dc6448d93ull && 
            C1.w[0] == 0x38c15b09ffffffffull) { // if  C1 = 10^33 - 1
          C1.w[1] = 0x0001ed09bead87c0ull; // C1 = 10^34 - 1
          C1.w[0] = 0x378d8e63ffffffffull;
          x_exp = x_exp - EXP_P1;
        }
      }
      // assemble the result
      res.w[1] = x_sign | x_exp | C1.w[1];
      res.w[0] = C1.w[0];
    } // end -MAXFP <= x <= -MINFP - 1 ulp OR MINFP <= x <= MAXFP - 1 ulp
  } // end x is not special and is not zero
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID128 nextafter
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
__bid128_nextafter (UINT128 * pres, UINT128 * px,
                  UINT128 *
                  py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM _EXC_INFO_PARAM) 
{
  UINT128 x = *px;
  UINT128 y = *py;
  UINT128 xnswp = *px;
  UINT128 ynswp = *py;
#else
UINT128
__bid128_nextafter (UINT128 x,
                  UINT128 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
                  _EXC_INFO_PARAM) {
  UINT128 xnswp = x;
  UINT128 ynswp = y;
#endif

  UINT128 res;
  UINT128 tmp1, tmp2;
  FPSC tmp_fpsf = 0; // dummy fpsf for calls to comparison functions
  int res1, res2;

  BID_SWAP128(x);
  BID_SWAP128(y);
  // check for NaNs
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
    } else {
      ; // let infinities fall through
    }
  }
  // neither x nor y is NaN
  tmp_fpsf = *pfpsf; // save fpsf
#if DECIMAL_CALL_BY_REFERENCE
  __bid128_quiet_equal (&res1, &xnswp,
                      &ynswp _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  __bid128_quiet_greater (&res2, &xnswp,
                        &ynswp _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
#else
  res1 =
    __bid128_quiet_equal (xnswp,
                        ynswp _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  res2 =
    __bid128_quiet_greater (xnswp,
                          ynswp _EXC_FLAGS_ARG _EXC_MASKS_ARG
                          _EXC_INFO_ARG);
#endif
  *pfpsf = tmp_fpsf; // restore fpsf
  if (res1) { // x = y
    // return x with the sign of y
    res.w[1] = 
        (x.w[1] & 0x7fffffffffffffffull) | (y.w[1] & 0x8000000000000000ull);
    res.w[0] = x.w[0];
  } else if (res2) { // x > y
#if DECIMAL_CALL_BY_REFERENCE
    __bid128_nextdown (&res,
                     &xnswp _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
#else
    res =
      __bid128_nextdown (xnswp _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
#endif
    BID_SWAP128(res);
  } else { // x < y
#if DECIMAL_CALL_BY_REFERENCE
    __bid128_nextup (&res,
                   &xnswp _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
#else
    res = __bid128_nextup (xnswp _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
#endif
    BID_SWAP128(res);
  }
  // if the operand x is finite but the result is infinite, signal 
  // overflow and inexact
  if (((x.w[1] & MASK_SPECIAL) != MASK_SPECIAL)
      && ((res.w[1] & MASK_SPECIAL) == MASK_SPECIAL)) {
    // set the inexact flag
    *pfpsf |= INEXACT_EXCEPTION;
    // set the overflow flag
    *pfpsf |= OVERFLOW_EXCEPTION;
  }
  // if the result is in (-10^emin, 10^emin), and is different from the
  // operand x, signal underflow and inexact
  tmp1.w[HIGH_128W] = 0x0000314dc6448d93ull; 
  tmp1.w[LOW_128W] = 0x38c15b0a00000000ull; // +100...0[34] * 10^emin
  tmp2.w[HIGH_128W] = res.w[1] & 0x7fffffffffffffffull;
  tmp2.w[LOW_128W] = res.w[0];
  tmp_fpsf = *pfpsf; // save fpsf
#if DECIMAL_CALL_BY_REFERENCE
  __bid128_quiet_greater (&res1, &tmp1,
                        &tmp2 _EXC_FLAGS_ARG _EXC_MASKS_ARG
                        _EXC_INFO_ARG);
  __bid128_quiet_not_equal (&res2, &xnswp,
                          &res _EXC_FLAGS_ARG _EXC_MASKS_ARG
                          _EXC_INFO_ARG);
#else
  res1 =
    __bid128_quiet_greater (tmp1,
                          tmp2 _EXC_FLAGS_ARG _EXC_MASKS_ARG
                          _EXC_INFO_ARG);
  res2 =
    __bid128_quiet_not_equal (xnswp,
                            res _EXC_FLAGS_ARG _EXC_MASKS_ARG
                            _EXC_INFO_ARG);
#endif
  *pfpsf = tmp_fpsf; // restore fpsf
  if (res1 && res2) {
    // set the inexact flag 
    *pfpsf |= INEXACT_EXCEPTION;
    // set the underflow flag 
    *pfpsf |= UNDERFLOW_EXCEPTION;
  }
  BID_RETURN (res);
}
