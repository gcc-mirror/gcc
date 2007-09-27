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
 *     call the unpacked arg. equivalent of bid64_add(x*y, z)
 *  else 
 *     get full coefficient_x*coefficient_y product
 *     call subroutine to perform addition of 64-bit argument 
 *                                         to 128-bit product
 *
 ****************************************************************************/

#include "bid_inline_add.h"

#if DECIMAL_CALL_BY_REFERENCE
extern void bid64_mul (UINT64 * pres, UINT64 * px,
		       UINT64 *
		       py _RND_MODE_PARAM _EXC_FLAGS_PARAM
		       _EXC_MASKS_PARAM _EXC_INFO_PARAM);
#else

extern UINT64 bid64_mul (UINT64 x,
			 UINT64 y _RND_MODE_PARAM
			 _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			 _EXC_INFO_PARAM);
#endif

#if DECIMAL_CALL_BY_REFERENCE

void
bid64_fma (UINT64 * pres, UINT64 * px, UINT64 * py,
	   UINT64 *
	   pz _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	   _EXC_INFO_PARAM) {
  UINT64 x, y, z;
#else

UINT64
bid64_fma (UINT64 x, UINT64 y,
	   UINT64 z _RND_MODE_PARAM _EXC_FLAGS_PARAM
	   _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT128 P, PU, CT, CZ;
  UINT64 sign_x, sign_y, coefficient_x, coefficient_y, sign_z,
    coefficient_z;
  UINT64 C64, remainder_y, res;
  UINT64 CYh, CY0L, T, valid_x, valid_y, valid_z;
  int_double tempx, tempy;
  int extra_digits, exponent_x, exponent_y, bin_expon_cx, bin_expon_cy,
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

  valid_x = unpack_BID64 (&sign_x, &exponent_x, &coefficient_x, x);
  valid_y = unpack_BID64 (&sign_y, &exponent_y, &coefficient_y, y);
  valid_z = unpack_BID64 (&sign_z, &exponent_z, &coefficient_z, z);

  // unpack arguments, check for NaN, Infinity, or 0
  if (!valid_x || !valid_y || !valid_z) {

    if ((y & MASK_NAN) == MASK_NAN) {	// y is NAN
      // if x = {0, f, inf, NaN}, y = NaN, z = {0, f, inf, NaN} then res = Q (y)
      // check first for non-canonical NaN payload
      y = y & 0xfe03ffffffffffffull;	// clear G6-G12
      if ((y & 0x0003ffffffffffffull) > 999999999999999ull) {
	y = y & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
      }
      if ((y & MASK_SNAN) == MASK_SNAN) {	// y is SNAN
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return quiet (y)
	res = y & 0xfdffffffffffffffull;
      } else {	// y is QNaN
	// return y
	res = y;
	// if z = SNaN or x = SNaN signal invalid exception
	if ((z & MASK_SNAN) == MASK_SNAN
	    || (x & MASK_SNAN) == MASK_SNAN) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	}
      }
      BID_RETURN (res)
    } else if ((z & MASK_NAN) == MASK_NAN) {	// z is NAN
      // if x = {0, f, inf, NaN}, y = {0, f, inf}, z = NaN then res = Q (z)
      // check first for non-canonical NaN payload
      z = z & 0xfe03ffffffffffffull;	// clear G6-G12
      if ((z & 0x0003ffffffffffffull) > 999999999999999ull) {
	z = z & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
      }
      if ((z & MASK_SNAN) == MASK_SNAN) {	// z is SNAN
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return quiet (z)
	res = z & 0xfdffffffffffffffull;
      } else {	// z is QNaN
	// return z
	res = z;
	// if x = SNaN signal invalid exception
	if ((x & MASK_SNAN) == MASK_SNAN) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	}
      }
      BID_RETURN (res)
    } else if ((x & MASK_NAN) == MASK_NAN) {	// x is NAN
      // if x = NaN, y = {0, f, inf}, z = {0, f, inf} then res = Q (x)
      // check first for non-canonical NaN payload
      x = x & 0xfe03ffffffffffffull;	// clear G6-G12
      if ((x & 0x0003ffffffffffffull) > 999999999999999ull) {
	x = x & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
      }
      if ((x & MASK_SNAN) == MASK_SNAN) {	// x is SNAN
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return quiet (x)
	res = x & 0xfdffffffffffffffull;
      } else {	// x is QNaN
	// return x
	res = x;	// clear out G[6]-G[16]
      }
      BID_RETURN (res)
    }

    if (!valid_x) {
      // x is Inf. or 0

      // x is Infinity?
      if ((x & 0x7800000000000000ull) == 0x7800000000000000ull) {
	// check if y is 0
	if (!coefficient_y) {
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
	  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
	  BID_RETURN (0x7c00000000000000ull);
	}
	// otherwise return +/-Inf
	BID_RETURN (((x ^ y) & 0x8000000000000000ull) |
		    0x7800000000000000ull);
      }
      // x is 0
      if (((y & 0x7800000000000000ull) != 0x7800000000000000ull)
	  && ((z & 0x7800000000000000ull) != 0x7800000000000000ull)) {

	if (coefficient_z) {
	  exponent_y = exponent_x - DECIMAL_EXPONENT_BIAS + exponent_y;

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
    if (!valid_y) {
      // y is Inf. or 0

      // y is Infinity?
      if ((y & 0x7800000000000000ull) == 0x7800000000000000ull) {
	// check if x is 0
	if (!coefficient_x) {
	  // y==0, return NaN
#ifdef SET_STATUS_FLAGS
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
	// otherwise return +/-Inf
	BID_RETURN (((x ^ y) & 0x8000000000000000ull) |
		    0x7800000000000000ull);
      }
      // y is 0 
      if (((z & 0x7800000000000000ull) != 0x7800000000000000ull)) {

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

    if (!valid_z) {
      // y is Inf. or 0

      // test if y is NaN/Inf
      if ((z & 0x7800000000000000ull) == 0x7800000000000000ull) {
	BID_RETURN (coefficient_z & QUIET_MASK64);
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
	  res = ((UINT64) exponent_z) << 53;
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
	get_add64 (sign_x ^ sign_y,
		   final_exponent, C64, sign_z, exponent_z, coefficient_z, rnd_mode, pfpsf);
      BID_RETURN (res);
    } else {
      P.w[0] = C64;
      P.w[1] = 0;
      extra_digits = 0;
    }
  } else {
    if (!coefficient_z) {
#if DECIMAL_CALL_BY_REFERENCE
      bid64_mul (&res, px,
		 py _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
		 _EXC_INFO_ARG);
#else
      res =
	bid64_mul (x,
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
    digits_p = estimate_decimal_digits[bp];
    if (!(__unsigned_compare_gt_128 (power10_table_128[digits_p], P)))
      digits_p++;	// if power10_table_128[digits_p] <= P

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
      digits_z = estimate_decimal_digits[bin_expon_cx];
      if (coefficient_z >= power10_table_128[digits_z].w[0])
	digits_z++;
      // underflow
      if ((final_exponent + 16 < 0)
	  || (exponent_z + digits_z > 33 + final_exponent)) {
	res =
	  BID_normalize (sign_z, exponent_z, coefficient_z,
			 sign_x ^ sign_y, 1, rnd_mode, pfpsf);
	BID_RETURN (res);
      }

      ez = exponent_z + digits_z - 16;
      if (ez < 0)
	ez = 0;
      scale_z = exponent_z - ez;
      coefficient_z *= power10_table_128[scale_z].w[0];
      ey = final_exponent - extra_digits;
      extra_digits = ez - ey;
      if (extra_digits > 33) {
	res =
	  BID_normalize (sign_z, exponent_z, coefficient_z,
			 sign_x ^ sign_y, 1, rnd_mode, pfpsf);
	BID_RETURN (res);
      }
      //else  // extra_digits<=32

      if (extra_digits > 17) {
	CYh = __truncate (P, 16);
	// get remainder
	T = power10_table_128[16].w[0];
	__mul_64x64_to_64 (CY0L, CYh, T);
	remainder_y = P.w[0] - CY0L;

	extra_digits -= 16;
	P.w[0] = CYh;
	P.w[1] = 0;
      } else
	remainder_y = 0;

      // align coeff_x, CYh
      __mul_64x64_to_128 (CZ, coefficient_z,
			  power10_table_128[extra_digits].w[0]);

      if (sign_z == (sign_y ^ sign_x)) {
	__add_128_128 (CT, CZ, P);
	if (__unsigned_compare_ge_128
	    (CT, power10_table_128[16 + extra_digits])) {
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
	} else if(!(CT.w[1]|CT.w[0]))
		sign_z = (rnd_mode!=ROUNDING_DOWN)? 0: 0x8000000000000000ull;
	if (ez
	    &&
	    (__unsigned_compare_gt_128
	     (power10_table_128[15 + extra_digits], CT))) {
	  extra_digits--;
	  ez--;
	}
      }

#ifdef SET_STATUS_FLAGS
      uf_status = 0;
      if ((!ez)
	  &&
	  __unsigned_compare_gt_128 (power10_table_128
				     [extra_digits + 15], CT)) {
	rmode = rnd_mode;
	if (sign_z && (unsigned) (rmode - 1) < 2)
	  rmode = 3 - rmode;
	//__add_128_64(PU, CT, round_const_table[rmode][extra_digits]);
	PU = power10_table_128[extra_digits + 15];
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
		remainder_y = power10_table_128[16].w[0] - remainder_y;

	      if (power10_table_128[15].w[0] > remainder_y)
		uf_status = UNDERFLOW_EXCEPTION;
	    }
	  } else	// RN or RN_away
	  {
	    if (remainder_y && (sign_z != (sign_y ^ sign_x)))
	      remainder_y = power10_table_128[16].w[0] - remainder_y;

	    if (!extra_digits) {
	      remainder_y += round_const_table[rmode][15];
	      if (remainder_y < power10_table_128[16].w[0])
		uf_status = UNDERFLOW_EXCEPTION;
	    } else {
	      if (remainder_y < round_const_table[rmode][16])
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
      get_add64 (sign_x ^ sign_y,
		 exponent_x + exponent_y - DECIMAL_EXPONENT_BIAS, C64, 
		 sign_z, exponent_z, coefficient_z, 
		 rnd_mode, pfpsf);
    BID_RETURN (res);
  }
}
