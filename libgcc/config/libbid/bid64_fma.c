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
 *    BID64 fma
 *****************************************************************************
 *
 *  Algorithm description:
 *
 *  if multiplication is guranteed exact (short coefficients)
 *     call the unpacked arg. equivalent of __bid64_add(x*y, z)
 *  else 
 *     get full coefficient_x*coefficient_y product
 *     call subroutine to perform addition of 64-bit argument 
 *                                         to 128-bit product
 *
 ****************************************************************************/

#include "inline_bid_add.h"

//////////////////////////////////////////////////////////////////////////
//
//  If coefficient_z is less than 16 digits long, normalize to 16 digits
//
/////////////////////////////////////////////////////////////////////////

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
  #ifndef IEEE_ROUND_NEAREST
static UINT64
__bid_normalize (UINT64 sign_z, int exponent_z,
               UINT64 coefficient_z, UINT64 round_dir, int round_flag,
               int rounding_mode, unsigned *fpsc) {
  #else
static UINT64
__bid_normalize (UINT64 z, UINT64 sign_z, int exponent_z,
               UINT64 coefficient_z, UINT64 round_dir, int round_flag,
               int rounding_mode, unsigned *fpsc) {
  #endif
#else
static UINT64
__bid_normalize (UINT64 z, UINT64 sign_z, int exponent_z,
               UINT64 coefficient_z, UINT64 round_dir, int round_flag,
               int rounding_mode, unsigned *fpsc) {
#endif
  SINT64 D;
  int_double tempx;
  int digits_z, bin_expon, scale, rmode;

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
  rmode = rounding_mode;
  if (sign_z && (unsigned) (rmode - 1) < 2)
    rmode = 3 - rmode;
#else
  if (coefficient_z >= __bid_power10_table_128[15].w[0])
    return z;
#endif
#endif
#ifdef IEEE_ROUND_NEAREST_TIES_AWAY
  if (coefficient_z >= __bid_power10_table_128[15].w[0])
    return z;
#endif

  //--- get number of bits in the coefficients of x and y ---
  tempx.d = (double) coefficient_z;
  bin_expon = ((tempx.i & MASK_BINARY_EXPONENT) >> 52) - 0x3ff;
  // get number of decimal digits in the coeff_x
  digits_z = __bid_estimate_decimal_digits[bin_expon];
  if (coefficient_z >= __bid_power10_table_128[digits_z].w[0])
    digits_z++;

  scale = 16 - digits_z;
  exponent_z -= scale;
  if (exponent_z < 0) {
    scale += exponent_z;
    exponent_z = 0;
  }
  coefficient_z *= __bid_power10_table_128[scale].w[0];

#ifdef SET_STATUS_FLAGS
  if (round_flag) {
    __set_status_flags (fpsc, INEXACT_EXCEPTION);
    if (coefficient_z < 1000000000000000ull)
      __set_status_flags (fpsc, UNDERFLOW_EXCEPTION);
    else if ((coefficient_z == 1000000000000000ull) && !exponent_z
	     && ((SINT64) (round_dir ^ sign_z) < 0) && round_flag
	     && (rmode == ROUNDING_DOWN || rmode == ROUNDING_TO_ZERO))
      __set_status_flags (fpsc, UNDERFLOW_EXCEPTION);
  }
#endif

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
  if (round_flag && (rmode & 3)) {
    D = round_dir ^ sign_z;

    if (rmode == ROUNDING_UP) {
      if (D >= 0)
	coefficient_z++;
    } else {
      if (D < 0)
	coefficient_z--;
      if (coefficient_z < 1000000000000000ull && exponent_z) {
	coefficient_z = 9999999999999999ull;
	exponent_z--;
      }
    }
  }
#endif
#endif

  return get_BID64 (sign_z, exponent_z, coefficient_z, rounding_mode,
		    fpsc);
}


#if DECIMAL_CALL_BY_REFERENCE
extern void __bid64_mul (UINT64 * pres, UINT64 * px,
		       UINT64 *
		       py _RND_MODE_PARAM _EXC_FLAGS_PARAM
		       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
#else

extern UINT64 __bid64_mul (UINT64 x,
			     UINT64 y _RND_MODE_PARAM
			     _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			     _EXC_INFO_PARAM);
#endif

#if DECIMAL_CALL_BY_REFERENCE

void
__bid64_fma (UINT64 * pres, UINT64 * px, UINT64 * py,
	   UINT64 *
	   pz _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	   _EXC_INFO_PARAM) {
  UINT64 x, y, z;
#else

UINT64
__bid64_fma (UINT64 x, UINT64 y,
	   UINT64 z _RND_MODE_PARAM _EXC_FLAGS_PARAM
	   _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT128 P, PU, CT, CZ;
  UINT64 sign_x, sign_y, coefficient_x, coefficient_y, sign_z,
    coefficient_z;
  UINT64 C64, remainder_y, res;
  UINT64 CYh, CY0L, T;
  int_double tempx, tempy;
  int extra_digits, exponent_x = 0, exponent_y = 0, bin_expon_cx, bin_expon_cy,
    bin_expon_product, rmode;
  int digits_p, bp, final_exponent, exponent_z, digits_z, ez, ey,
    scale_z, uf_status;

#if DECIMAL_CALL_BY_REFERENCE
#if !DECIMAL_GLOBAL_ROUNDING
  _IDEC_round rnd_mode = *prnd_mode;
#endif
  x = *px;
  y = *py;
  z = *pz;
#endif

  // unpack arguments, check for NaN or Infinity
  if (!unpack_BID64 (&sign_x, &exponent_x, &coefficient_x, x)) {
    // x is Inf. or NaN

    // test if x is NaN
    if ((x & 0x7c00000000000000ull) == 0x7c00000000000000ull) {
#ifdef SET_STATUS_FLAGS
      if (((x & 0x7e00000000000000ull) == 0x7e00000000000000ull) ||	// sNaN
	  ((y & 0x7e00000000000000ull) == 0x7e00000000000000ull) ||	// sNaN
	  ((z & 0x7e00000000000000ull) == 0x7e00000000000000ull))	// sNaN
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      BID_RETURN (x & QUIET_MASK64);
    }
    // x is Infinity?
    if ((x & 0x7800000000000000ull) == 0x7800000000000000ull) {
      // check if y is 0
      if (((y & 0x6000000000000000ull) != 0x6000000000000000ull)
	  && !(y << (64 - 53))) {
	// y==0, return NaN
#ifdef SET_STATUS_FLAGS
	if ((z & 0x7e00000000000000ull) != 0x7c00000000000000ull)
	  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
	BID_RETURN (0x7c00000000000000ull);
      }
      // test if z is Inf of oposite sign
      if (((z & 0x7c00000000000000ull) == 0x7800000000000000ull)
	  && (((x ^ y) ^ z) & 0x8000000000000000ull)) {
	// return NaN 
#ifdef SET_STATUS_FLAGS
	if (((y & 0x7e00000000000000ull) != 0x7c00000000000000ull) ||	// qNaN
	    ((z & 0x7e00000000000000ull) == 0x7e00000000000000ull))	// sNaN
	  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
	BID_RETURN (0x7c00000000000000ull);
      }
      if ((y & 0x7c00000000000000ull) == 0x7c00000000000000ull) {
#ifdef SET_STATUS_FLAGS
	if (((y & 0x7e00000000000000ull) == 0x7e00000000000000ull) ||	// sNaN
	    ((z & 0x7e00000000000000ull) == 0x7e00000000000000ull))	// sNaN
	  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
	BID_RETURN (y & QUIET_MASK64);
      }
      if ((z & 0x7c00000000000000ull) == 0x7c00000000000000ull) {
#ifdef SET_STATUS_FLAGS
	if (((z & 0x7e00000000000000ull) == 0x7e00000000000000ull))	// sNaN
	  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
	BID_RETURN (z & QUIET_MASK64);
      }
      // otherwise return +/-Inf
      BID_RETURN (((x ^ y) & 0x8000000000000000ull) |
		  0x7800000000000000ull);
    }
    // x is 0
    if (((y & 0x7800000000000000ull) != 0x7800000000000000ull)
	&& ((z & 0x7800000000000000ull) != 0x7800000000000000ull)) {
      if ((z & 0x6000000000000000ull) == 0x6000000000000000ull) {
	exponent_z = ((UINT32) (z >> 51)) & 0x3ff;
	coefficient_z =
	  (z & 0x0007ffffffffffffull) | 0x0020000000000000ull;
      } else {
	exponent_z = ((UINT32) (z >> 53)) & 0x3ff;
	coefficient_z = z & 0x001fffffffffffffull;
      }

      if (coefficient_z) {
	if ((y & 0x6000000000000000ull) == 0x6000000000000000ull)
	  exponent_y =
	    exponent_x - DECIMAL_EXPONENT_BIAS +
	    (((UINT32) (y >> 51)) & 0x3ff);
	else
	  exponent_y =
	    exponent_x - DECIMAL_EXPONENT_BIAS +
	    (((UINT32) (y >> 53)) & 0x3ff);

	sign_z = z & 0x8000000000000000ull;

	if (exponent_y >= exponent_z)
	  BID_RETURN (z);
	res =
	  add_zero64 (exponent_y, sign_z, exponent_z, coefficient_z,
		      &rnd_mode, pfpsf);
	BID_RETURN (res);
      }
    }
  }
  if (!unpack_BID64 (&sign_y, &exponent_y, &coefficient_y, y)) {
    // y is Inf. or NaN

    // test if y is NaN
    if ((y & 0x7c00000000000000ull) == 0x7c00000000000000ull) {
#ifdef SET_STATUS_FLAGS
      if (((y & 0x7e00000000000000ull) == 0x7e00000000000000ull) ||	// sNaN
	  ((z & 0x7e00000000000000ull) == 0x7e00000000000000ull))	// sNaN
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      BID_RETURN (y & QUIET_MASK64);
    }
    // y is Infinity?
    if ((y & 0x7800000000000000ull) == 0x7800000000000000ull) {
      // check if x is 0
      if (((x & 0x6000000000000000ull) != 0x6000000000000000ull)
	  && !(x << (64 - 53))) {
	// y==0, return NaN
#ifdef SET_STATUS_FLAGS
	if ((z & 0x7e00000000000000ull) != 0x7c00000000000000ull)
	  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
	BID_RETURN (0x7c00000000000000ull);
      }
      // test if z is Inf of oposite sign
      if (((z & 0x7c00000000000000ull) == 0x7800000000000000ull)
	  && (((x ^ y) ^ z) & 0x8000000000000000ull)) {
#ifdef SET_STATUS_FLAGS
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
	// return NaN
	BID_RETURN (0x7c00000000000000ull);
      }
      if ((z & 0x7c00000000000000ull) == 0x7c00000000000000ull) {
#ifdef SET_STATUS_FLAGS
	if (((z & 0x7e00000000000000ull) == 0x7e00000000000000ull))	// sNaN
	  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
	BID_RETURN (z & QUIET_MASK64);
      }
      // otherwise return +/-Inf
      BID_RETURN (((x ^ y) & 0x8000000000000000ull) |
		  0x7800000000000000ull);
    }
    // y is 0 
    if (((z & 0x7800000000000000ull) != 0x7800000000000000ull)) {
      if ((z & 0x6000000000000000ull) == 0x6000000000000000ull) {
	exponent_z = ((UINT32) (z >> 51)) & 0x3ff;
	coefficient_z =
	  (z & 0x0007ffffffffffffull) | 0x0020000000000000ull;
      } else {
	exponent_z = ((UINT32) (z >> 53)) & 0x3ff;
	coefficient_z = z & 0x001fffffffffffffull;
      }

      if (coefficient_z) {
	exponent_y += exponent_x - DECIMAL_EXPONENT_BIAS;

	sign_z = z & 0x8000000000000000ull;

	if (exponent_y >= exponent_z)
	  BID_RETURN (z);
	res =
	  add_zero64 (exponent_y, sign_z, exponent_z, coefficient_z,
		      &rnd_mode, pfpsf);
	BID_RETURN (res);
      }
    }
  }

  if (!unpack_BID64 (&sign_z, &exponent_z, &coefficient_z, z)) {
    // y is Inf. or NaN or 0

    // test if y is NaN/Inf
    if ((z & 0x7800000000000000ull) == 0x7800000000000000ull) {
#ifdef SET_STATUS_FLAGS
      if ((z & 0x7e00000000000000ull) == 0x7e00000000000000ull)	// sNaN
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      BID_RETURN (z & QUIET_MASK64);
    }
    // z is 0, return x*y
    if ((!coefficient_x) || (!coefficient_y)) {
      //0+/-0
      exponent_x += exponent_y - DECIMAL_EXPONENT_BIAS;
      if (exponent_x > DECIMAL_MAX_EXPON_64)
	exponent_x = DECIMAL_MAX_EXPON_64;
      else if (exponent_x < 0)
	exponent_x = 0;
      if (exponent_x <= exponent_z)
	res = ((UINT64) exponent_x) << 53;
      else
	res = ((z << 1) >> 1);
      if ((sign_x ^ sign_y) == sign_z)
	res |= sign_z;
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
      else if (rnd_mode == ROUNDING_DOWN)
	res |= 0x8000000000000000ull;
#endif
#endif
      BID_RETURN (res);
    }
  }


  /* get binary coefficients of x and y */

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
    final_exponent = exponent_x + exponent_y - DECIMAL_EXPONENT_BIAS;
    if ((final_exponent > 0) || (!coefficient_z)) {
      res =
	get_add64 (sign_z, exponent_z, coefficient_z, sign_x ^ sign_y,
		   final_exponent, C64, rnd_mode, pfpsf);
      BID_RETURN (res);
    } else {
      P.w[0] = C64;
      P.w[1] = 0;
      extra_digits = 0;
    }
  } else {
    if (!coefficient_z) {
#if DECIMAL_CALL_BY_REFERENCE
      __bid64_mul (&res, px,
		 py _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
		 _EXC_INFO_ARG);
#else
      res =
	__bid64_mul (x,
		   y _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
		   _EXC_INFO_ARG);
#endif
      BID_RETURN (res);
    }
    // get 128-bit product: coefficient_x*coefficient_y
    __mul_64x64_to_128 (P, coefficient_x, coefficient_y);

    // tighten binary range of P:  leading bit is 2^bp
    // unbiased_bin_expon_product <= bp <= unbiased_bin_expon_product+1
    bin_expon_product -= 2 * BINARY_EXPONENT_BIAS;
    __tight_bin_range_128 (bp, P, bin_expon_product);

    // get number of decimal digits in the product
    digits_p = __bid_estimate_decimal_digits[bp];
    if (!(__unsigned_compare_gt_128 (__bid_power10_table_128[digits_p], P)))
      digits_p++;	// if __bid_power10_table_128[digits_p] <= P

    // determine number of decimal digits to be rounded out
    extra_digits = digits_p - MAX_FORMAT_DIGITS;
    final_exponent =
      exponent_x + exponent_y + extra_digits - DECIMAL_EXPONENT_BIAS;
  }

  if (((unsigned) final_exponent) >= 3 * 256) {
    if (final_exponent < 0) {
      //--- get number of bits in the coefficients of z  ---
      tempx.d = (double) coefficient_z;
      bin_expon_cx = ((tempx.i & MASK_BINARY_EXPONENT) >> 52) - 0x3ff;
      // get number of decimal digits in the coeff_x
      digits_z = __bid_estimate_decimal_digits[bin_expon_cx];
      if (coefficient_z >= __bid_power10_table_128[digits_z].w[0])
	digits_z++;
      // underflow
      if ((final_exponent + 16 < 0)
	  || (exponent_z + digits_z > 33 + final_exponent)) {
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
  #ifndef IEEE_ROUND_NEAREST
        res = __bid_normalize (sign_z, exponent_z, coefficient_z,
                         sign_x ^ sign_y, 1, rnd_mode, pfpsf);
  #else
        res = __bid_normalize (z, sign_z, exponent_z, coefficient_z,
                         sign_x ^ sign_y, 1, rnd_mode, pfpsf);
  #endif
#else
        res = __bid_normalize (z, sign_z, exponent_z, coefficient_z,
                         sign_x ^ sign_y, 1, rnd_mode, pfpsf);
#endif
	BID_RETURN (res);
      }

      ez = exponent_z + digits_z - 16;
      if (ez < 0)
	ez = 0;
      scale_z = exponent_z - ez;
      coefficient_z *= __bid_power10_table_128[scale_z].w[0];
      ey = final_exponent - extra_digits;
      extra_digits = ez - ey;
      if (extra_digits > 33) {
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
  #ifndef IEEE_ROUND_NEAREST
	res = __bid_normalize (sign_z, exponent_z, coefficient_z,
                         sign_x ^ sign_y, 1, rnd_mode, pfpsf);
  #else
        res = __bid_normalize (z, sign_z, exponent_z, coefficient_z,
                         sign_x ^ sign_y, 1, rnd_mode, pfpsf);

  #endif
#else
	res = __bid_normalize (z, sign_z, exponent_z, coefficient_z,
			 sign_x ^ sign_y, 1, rnd_mode, pfpsf);
#endif
	BID_RETURN (res);
      }
      //else  // extra_digits<=32

      if (extra_digits > 17) {
	CYh = __truncate (P, 16);
	// get remainder
	T = __bid_power10_table_128[16].w[0];
	__mul_64x64_to_64 (CY0L, CYh, T);
	remainder_y = P.w[0] - CY0L;

	extra_digits -= 16;
	P.w[0] = CYh;
	P.w[1] = 0;
      } else
	remainder_y = 0;

      // align coeff_x, CYh
      __mul_64x64_to_128 (CZ, coefficient_z,
			  __bid_power10_table_128[extra_digits].w[0]);

      if (sign_z == (sign_y ^ sign_x)) {
	__add_128_128 (CT, CZ, P);
	if (__unsigned_compare_ge_128
	    (CT, __bid_power10_table_128[16 + extra_digits])) {
	  extra_digits++;
	  ez++;
	}
      } else {
	if (remainder_y && (__unsigned_compare_ge_128 (CZ, P))) {
	  P.w[0]++;
	  if (!P.w[0])
	    P.w[1]++;
	}
	__sub_128_128 (CT, CZ, P);
	if (((SINT64) CT.w[1]) < 0) {
	  sign_z = sign_y ^ sign_x;
	  CT.w[0] = 0 - CT.w[0];
	  CT.w[1] = 0 - CT.w[1];
	  if (CT.w[0])
	    CT.w[1]--;
	}
	if (ez
	    &&
	    (__unsigned_compare_gt_128
	     (__bid_power10_table_128[15 + extra_digits], CT))) {
	  extra_digits--;
	  ez--;
	}
      }

#ifdef SET_STATUS_FLAGS
      uf_status = 0;
      if ((!ez)
	  &&
	  __unsigned_compare_gt_128 (__bid_power10_table_128
				     [extra_digits + 15], CT)) {
	rmode = rnd_mode;
	if (sign_z && (unsigned) (rmode - 1) < 2)
	  rmode = 3 - rmode;
	//__add_128_64(PU, CT, __bid_round_const_table[rmode][extra_digits]);
	PU = __bid_power10_table_128[extra_digits + 15];
	PU.w[0]--;
	if (__unsigned_compare_gt_128 (PU, CT)
	    || (rmode == ROUNDING_DOWN)
	    || (rmode == ROUNDING_TO_ZERO))
	  uf_status = UNDERFLOW_EXCEPTION;
	else if (extra_digits < 2) {
	  if ((rmode == ROUNDING_UP)) {
	    if (!extra_digits)
	      uf_status = UNDERFLOW_EXCEPTION;
	    else {
	      if (remainder_y && (sign_z != (sign_y ^ sign_x)))
		remainder_y = __bid_power10_table_128[16].w[0] - remainder_y;

	      if (__bid_power10_table_128[15].w[0] > remainder_y)
		uf_status = UNDERFLOW_EXCEPTION;
	    }
	  } else	// RN or RN_away
	  {
	    if (remainder_y && (sign_z != (sign_y ^ sign_x)))
	      remainder_y = __bid_power10_table_128[16].w[0] - remainder_y;

	    if (!extra_digits) {
	      remainder_y += __bid_round_const_table[rmode][15];
	      if (remainder_y < __bid_power10_table_128[16].w[0])
		uf_status = UNDERFLOW_EXCEPTION;
	    } else {
	      if (remainder_y < __bid_round_const_table[rmode][16])
		uf_status = UNDERFLOW_EXCEPTION;
	    }
	  }
	  //__set_status_flags (pfpsf, uf_status);
	}
      }
#endif
      res =
	__bid_full_round64_remainder (sign_z, ez - extra_digits, CT,
				      extra_digits, remainder_y,
				      rnd_mode, pfpsf, uf_status);
      BID_RETURN (res);

    } else {
      if ((sign_z == (sign_x ^ sign_y))
	  || (final_exponent > 3 * 256 + 15)) {
	res =
	  fast_get_BID64_check_OF (sign_x ^ sign_y, final_exponent,
				   1000000000000000ull, rnd_mode,
				   pfpsf);
	BID_RETURN (res);
      }
    }
  }


  if (extra_digits > 0) {
    res =
      get_add128 (sign_z, exponent_z, coefficient_z, sign_x ^ sign_y,
		  final_exponent, P, extra_digits, rnd_mode, pfpsf);
    BID_RETURN (res);
  }
  // go to convert_format and exit
  else {
    C64 = __low_64 (P);

    res =
      get_add64 (sign_z, exponent_z, coefficient_z, sign_x ^ sign_y,
		 exponent_x + exponent_y - DECIMAL_EXPONENT_BIAS, C64,
		 rnd_mode, pfpsf);
    BID_RETURN (res);
  }
}
