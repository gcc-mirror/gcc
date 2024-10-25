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

/*****************************************************************************
 *    BID64 add
 *****************************************************************************
 *
 *  Algorithm description:
 *
 *   if(exponent_a < exponent_b)
 *       switch a, b
 *   diff_expon = exponent_a - exponent_b
 *   if(diff_expon > 16)
 *      return normalize(a)
 *   if(coefficient_a*10^diff_expon guaranteed below 2^62)
 *       S = sign_a*coefficient_a*10^diff_expon + sign_b*coefficient_b
 *       if(|S|<10^16)
 *           return get_BID64(sign(S),exponent_b,|S|)
 *       else
 *          determine number of extra digits in S (1, 2, or 3)
 *            return rounded result
 *   else // large exponent difference
 *       if(number_digits(coefficient_a*10^diff_expon) +/- 10^16)
 *          guaranteed the same as
 *          number_digits(coefficient_a*10^diff_expon) )
 *           S = normalize(coefficient_a + (sign_a^sign_b)*10^(16-diff_expon))
 *           corr = 10^16 + (sign_a^sign_b)*coefficient_b
 *           corr*10^exponent_b is rounded so it aligns with S*10^exponent_S
 *           return get_BID64(sign_a,exponent(S),S+rounded(corr))
 *       else
 *         add sign_a*coefficient_a*10^diff_expon, sign_b*coefficient_b
 *             in 128-bit integer arithmetic, then round to 16 decimal digits
 *
 *
 ****************************************************************************/

#include "bid_internal.h"

#if DECIMAL_CALL_BY_REFERENCE
void bid64_add (UINT64 * pres, UINT64 * px,
		UINT64 *
		py _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		_EXC_INFO_PARAM);
#else
UINT64 bid64_add (UINT64 x,
		  UINT64 y _RND_MODE_PARAM _EXC_FLAGS_PARAM
		  _EXC_MASKS_PARAM _EXC_INFO_PARAM);
#endif

#if DECIMAL_CALL_BY_REFERENCE

void
bid64_sub (UINT64 * pres, UINT64 * px,
	   UINT64 *
	   py _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	   _EXC_INFO_PARAM) {
  UINT64 y = *py;
#if !DECIMAL_GLOBAL_ROUNDING
  _IDEC_round rnd_mode = *prnd_mode;
#endif
  // check if y is not NaN
  if (((y & NAN_MASK64) != NAN_MASK64))
    y ^= 0x8000000000000000ull;
  bid64_add (pres, px,
	     &y _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
	     _EXC_INFO_ARG);
}
#else

UINT64
bid64_sub (UINT64 x,
	   UINT64 y _RND_MODE_PARAM _EXC_FLAGS_PARAM
	   _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  // check if y is not NaN
  if (((y & NAN_MASK64) != NAN_MASK64))
    y ^= 0x8000000000000000ull;

  return bid64_add (x,
		    y _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
		    _EXC_INFO_ARG);
}
#endif



#if DECIMAL_CALL_BY_REFERENCE

void
bid64_add (UINT64 * pres, UINT64 * px,
	   UINT64 *
	   py _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	   _EXC_INFO_PARAM) {
  UINT64 x, y;
#else

UINT64
bid64_add (UINT64 x,
	   UINT64 y _RND_MODE_PARAM _EXC_FLAGS_PARAM
	   _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif

  UINT128 CA, CT, CT_new;
  UINT64 sign_x, sign_y, coefficient_x, coefficient_y, C64_new;
  UINT64 valid_x, valid_y;
  UINT64 res;
  UINT64 sign_a, sign_b, coefficient_a, coefficient_b, sign_s, sign_ab,
    rem_a;
  UINT64 saved_ca, saved_cb, C0_64, C64, remainder_h, T1, carry, tmp;
  int_double tempx;
  int exponent_x, exponent_y, exponent_a, exponent_b, diff_dec_expon;
  int bin_expon_ca, extra_digits, amount, scale_k, scale_ca;
  unsigned rmode, status;

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
    // x is Inf. or NaN

    // test if x is NaN
    if ((x & NAN_MASK64) == NAN_MASK64) {
#ifdef SET_STATUS_FLAGS
      if (((x & SNAN_MASK64) == SNAN_MASK64)	// sNaN
	  || ((y & SNAN_MASK64) == SNAN_MASK64))
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      res = coefficient_x & QUIET_MASK64;
      BID_RETURN (res);
    }
    // x is Infinity?
    if ((x & INFINITY_MASK64) == INFINITY_MASK64) {
      // check if y is Inf
      if (((y & NAN_MASK64) == INFINITY_MASK64)) {
	if (sign_x == (y & 0x8000000000000000ull)) {
	  res = coefficient_x;
	  BID_RETURN (res);
	}
	// return NaN
	{
#ifdef SET_STATUS_FLAGS
	  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
	  res = NAN_MASK64;
	  BID_RETURN (res);
	}
      }
      // check if y is NaN
      if (((y & NAN_MASK64) == NAN_MASK64)) {
	res = coefficient_y & QUIET_MASK64;
#ifdef SET_STATUS_FLAGS
	if (((y & SNAN_MASK64) == SNAN_MASK64))
	  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
	BID_RETURN (res);
      }
      // otherwise return +/-Inf
      {
	res = coefficient_x;
	BID_RETURN (res);
      }
    }
    // x is 0
    {
      if (((y & INFINITY_MASK64) != INFINITY_MASK64) && coefficient_y) {
	if (exponent_y <= exponent_x) {
	  res = y;
	  BID_RETURN (res);
	}
      }
    }

  }
  if (!valid_y) {
    // y is Inf. or NaN?
    if (((y & INFINITY_MASK64) == INFINITY_MASK64)) {
#ifdef SET_STATUS_FLAGS
      if ((y & SNAN_MASK64) == SNAN_MASK64)	// sNaN
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      res = coefficient_y & QUIET_MASK64;
      BID_RETURN (res);
    }
    // y is 0
    if (!coefficient_x) {	// x==0
      if (exponent_x <= exponent_y)
	res = ((UINT64) exponent_x) << 53;
      else
	res = ((UINT64) exponent_y) << 53;
      if (sign_x == sign_y)
	res |= sign_x;
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
      if (rnd_mode == ROUNDING_DOWN && sign_x != sign_y)
	res |= 0x8000000000000000ull;
#endif
#endif
      BID_RETURN (res);
    } else if (exponent_y >= exponent_x) {
      res = x;
      BID_RETURN (res);
    }
  }
  // sort arguments by exponent
  if (exponent_x < exponent_y) {
    sign_a = sign_y;
    exponent_a = exponent_y;
    coefficient_a = coefficient_y;
    sign_b = sign_x;
    exponent_b = exponent_x;
    coefficient_b = coefficient_x;
  } else {
    sign_a = sign_x;
    exponent_a = exponent_x;
    coefficient_a = coefficient_x;
    sign_b = sign_y;
    exponent_b = exponent_y;
    coefficient_b = coefficient_y;
  }

  // exponent difference
  diff_dec_expon = exponent_a - exponent_b;

  /* get binary coefficients of x and y */

  //--- get number of bits in the coefficients of x and y ---

  // version 2 (original)
  tempx.d = (double) coefficient_a;
  bin_expon_ca = ((tempx.i & MASK_BINARY_EXPONENT) >> 52) - 0x3ff;

  if (diff_dec_expon > MAX_FORMAT_DIGITS) {
    // normalize a to a 16-digit coefficient

    scale_ca = estimate_decimal_digits[bin_expon_ca];
    if (coefficient_a >= power10_table_128[scale_ca].w[0])
      scale_ca++;

    scale_k = 16 - scale_ca;

    coefficient_a *= power10_table_128[scale_k].w[0];

    diff_dec_expon -= scale_k;
    exponent_a -= scale_k;

    /* get binary coefficients of x and y */

    //--- get number of bits in the coefficients of x and y ---
    tempx.d = (double) coefficient_a;
    bin_expon_ca = ((tempx.i & MASK_BINARY_EXPONENT) >> 52) - 0x3ff;

    if (diff_dec_expon > MAX_FORMAT_DIGITS) {
#ifdef SET_STATUS_FLAGS
      if (coefficient_b) {
	__set_status_flags (pfpsf, INEXACT_EXCEPTION);
      }
#endif

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
      if (((rnd_mode) & 3) && coefficient_b)	// not ROUNDING_TO_NEAREST
      {
	switch (rnd_mode) {
	case ROUNDING_DOWN:
	  if (sign_b) {
	    coefficient_a -= ((((SINT64) sign_a) >> 63) | 1);
	    if (coefficient_a < 1000000000000000ull) {
	      exponent_a--;
	      coefficient_a = 9999999999999999ull;
	    } else if (coefficient_a >= 10000000000000000ull) {
	      exponent_a++;
	      coefficient_a = 1000000000000000ull;
	    }
	  }
	  break;
	case ROUNDING_UP:
	  if (!sign_b) {
	    coefficient_a += ((((SINT64) sign_a) >> 63) | 1);
	    if (coefficient_a < 1000000000000000ull) {
	      exponent_a--;
	      coefficient_a = 9999999999999999ull;
	    } else if (coefficient_a >= 10000000000000000ull) {
	      exponent_a++;
	      coefficient_a = 1000000000000000ull;
	    }
	  }
	  break;
	default:	// RZ
	  if (sign_a != sign_b) {
	    coefficient_a--;
	    if (coefficient_a < 1000000000000000ull) {
	      exponent_a--;
	      coefficient_a = 9999999999999999ull;
	    }
	  }
	  break;
	}
      } else
#endif
#endif
	// check special case here
	if ((coefficient_a == 1000000000000000ull)
	    && (diff_dec_expon == MAX_FORMAT_DIGITS + 1)
	    && (sign_a ^ sign_b)
	    && (coefficient_b > 5000000000000000ull)) {
	coefficient_a = 9999999999999999ull;
	exponent_a--;
      }

      res =
	fast_get_BID64_check_OF (sign_a, exponent_a, coefficient_a,
				 rnd_mode, pfpsf);
      BID_RETURN (res);
    }
  }
  // test whether coefficient_a*10^(exponent_a-exponent_b)  may exceed 2^62
  if (bin_expon_ca + estimate_bin_expon[diff_dec_expon] < 60) {
    // coefficient_a*10^(exponent_a-exponent_b)<2^63

    // multiply by 10^(exponent_a-exponent_b)
    coefficient_a *= power10_table_128[diff_dec_expon].w[0];

    // sign mask
    sign_b = ((SINT64) sign_b) >> 63;
    // apply sign to coeff. of b
    coefficient_b = (coefficient_b + sign_b) ^ sign_b;

    // apply sign to coefficient a
    sign_a = ((SINT64) sign_a) >> 63;
    coefficient_a = (coefficient_a + sign_a) ^ sign_a;

    coefficient_a += coefficient_b;
    // get sign
    sign_s = ((SINT64) coefficient_a) >> 63;
    coefficient_a = (coefficient_a + sign_s) ^ sign_s;
    sign_s &= 0x8000000000000000ull;

    // coefficient_a < 10^16 ?
    if (coefficient_a < power10_table_128[MAX_FORMAT_DIGITS].w[0]) {
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
      if (rnd_mode == ROUNDING_DOWN && (!coefficient_a)
	  && sign_a != sign_b)
	sign_s = 0x8000000000000000ull;
#endif
#endif
      res = very_fast_get_BID64 (sign_s, exponent_b, coefficient_a);
      BID_RETURN (res);
    }
    // otherwise rounding is necessary

    // already know coefficient_a<10^19
    // coefficient_a < 10^17 ?
    if (coefficient_a < power10_table_128[17].w[0])
      extra_digits = 1;
    else if (coefficient_a < power10_table_128[18].w[0])
      extra_digits = 2;
    else
      extra_digits = 3;

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
    rmode = rnd_mode;
    if (sign_s && (unsigned) (rmode - 1) < 2)
      rmode = 3 - rmode;
#else
    rmode = 0;
#endif
#else
    rmode = 0;
#endif
    coefficient_a += round_const_table[rmode][extra_digits];

    // get P*(2^M[extra_digits])/10^extra_digits
    __mul_64x64_to_128 (CT, coefficient_a,
			reciprocals10_64[extra_digits]);

    // now get P/10^extra_digits: shift C64 right by M[extra_digits]-128
    amount = short_recip_scale[extra_digits];
    C64 = CT.w[1] >> amount;

  } else {
    // coefficient_a*10^(exponent_a-exponent_b) is large
    sign_s = sign_a;

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
    rmode = rnd_mode;
    if (sign_s && (unsigned) (rmode - 1) < 2)
      rmode = 3 - rmode;
#else
    rmode = 0;
#endif
#else
    rmode = 0;
#endif

    // check whether we can take faster path
    scale_ca = estimate_decimal_digits[bin_expon_ca];

    sign_ab = sign_a ^ sign_b;
    sign_ab = ((SINT64) sign_ab) >> 63;

    // T1 = 10^(16-diff_dec_expon)
    T1 = power10_table_128[16 - diff_dec_expon].w[0];

    // get number of digits in coefficient_a
    if (coefficient_a >= power10_table_128[scale_ca].w[0]) {
      scale_ca++;
    }

    scale_k = 16 - scale_ca;

    // addition
    saved_ca = coefficient_a - T1;
    coefficient_a =
      (SINT64) saved_ca *(SINT64) power10_table_128[scale_k].w[0];
    extra_digits = diff_dec_expon - scale_k;

    // apply sign
    saved_cb = (coefficient_b + sign_ab) ^ sign_ab;
    // add 10^16 and rounding constant
    coefficient_b =
      saved_cb + 10000000000000000ull +
      round_const_table[rmode][extra_digits];

    // get P*(2^M[extra_digits])/10^extra_digits
    __mul_64x64_to_128 (CT, coefficient_b,
			reciprocals10_64[extra_digits]);

    // now get P/10^extra_digits: shift C64 right by M[extra_digits]-128
    amount = short_recip_scale[extra_digits];
    C0_64 = CT.w[1] >> amount;

    // result coefficient
    C64 = C0_64 + coefficient_a;
    // filter out difficult (corner) cases
    // this test ensures the number of digits in coefficient_a does not change
    // after adding (the appropriately scaled and rounded) coefficient_b
    if ((UINT64) (C64 - 1000000000000000ull - 1) >
	9000000000000000ull - 2) {
      if (C64 >= 10000000000000000ull) {
	// result has more than 16 digits
	if (!scale_k) {
	  // must divide coeff_a by 10
	  saved_ca = saved_ca + T1;
	  __mul_64x64_to_128 (CA, saved_ca, 0x3333333333333334ull);
	  //reciprocals10_64[1]);
	  coefficient_a = CA.w[1] >> 1;
	  rem_a =
	    saved_ca - (coefficient_a << 3) - (coefficient_a << 1);
	  coefficient_a = coefficient_a - T1;

	  saved_cb += rem_a * power10_table_128[diff_dec_expon].w[0];
	} else
	  coefficient_a =
	    (SINT64) (saved_ca - T1 -
		      (T1 << 3)) * (SINT64) power10_table_128[scale_k -
							      1].w[0];

	extra_digits++;
	coefficient_b =
	  saved_cb + 100000000000000000ull +
	  round_const_table[rmode][extra_digits];

	// get P*(2^M[extra_digits])/10^extra_digits
	__mul_64x64_to_128 (CT, coefficient_b,
			    reciprocals10_64[extra_digits]);

	// now get P/10^extra_digits: shift C64 right by M[extra_digits]-128
	amount = short_recip_scale[extra_digits];
	C0_64 = CT.w[1] >> amount;

	// result coefficient
	C64 = C0_64 + coefficient_a;
      } else if (C64 <= 1000000000000000ull) {
	// less than 16 digits in result
	coefficient_a =
	  (SINT64) saved_ca *(SINT64) power10_table_128[scale_k +
							1].w[0];
	//extra_digits --;
	exponent_b--;
	coefficient_b =
	  (saved_cb << 3) + (saved_cb << 1) + 100000000000000000ull +
	  round_const_table[rmode][extra_digits];

	// get P*(2^M[extra_digits])/10^extra_digits
	__mul_64x64_to_128 (CT_new, coefficient_b,
			    reciprocals10_64[extra_digits]);

	// now get P/10^extra_digits: shift C64 right by M[extra_digits]-128
	amount = short_recip_scale[extra_digits];
	C0_64 = CT_new.w[1] >> amount;

	// result coefficient
	C64_new = C0_64 + coefficient_a;
	if (C64_new < 10000000000000000ull) {
	  C64 = C64_new;
#ifdef SET_STATUS_FLAGS
	  CT = CT_new;
#endif
	} else
	  exponent_b++;
      }

    }

  }

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
  if (rmode == 0)	//ROUNDING_TO_NEAREST
#endif
    if (C64 & 1) {
      // check whether fractional part of initial_P/10^extra_digits is
      // exactly .5
      // this is the same as fractional part of
      //      (initial_P + 0.5*10^extra_digits)/10^extra_digits is exactly zero

      // get remainder
      remainder_h = CT.w[1] << (64 - amount);

      // test whether fractional part is 0
      if (!remainder_h && (CT.w[0] < reciprocals10_64[extra_digits])) {
	C64--;
      }
    }
#endif

#ifdef SET_STATUS_FLAGS
  status = INEXACT_EXCEPTION;

  // get remainder
  remainder_h = CT.w[1] << (64 - amount);

  switch (rmode) {
  case ROUNDING_TO_NEAREST:
  case ROUNDING_TIES_AWAY:
    // test whether fractional part is 0
    if ((remainder_h == 0x8000000000000000ull)
	&& (CT.w[0] < reciprocals10_64[extra_digits]))
      status = EXACT_STATUS;
    break;
  case ROUNDING_DOWN:
  case ROUNDING_TO_ZERO:
    if (!remainder_h && (CT.w[0] < reciprocals10_64[extra_digits]))
      status = EXACT_STATUS;
    //if(!C64 && rmode==ROUNDING_DOWN) sign_s=sign_y;
    break;
  default:
    // round up
    __add_carry_out (tmp, carry, CT.w[0],
		     reciprocals10_64[extra_digits]);
    if ((remainder_h >> (64 - amount)) + carry >=
	(((UINT64) 1) << amount))
      status = EXACT_STATUS;
    break;
  }
  __set_status_flags (pfpsf, status);

#endif

  res =
    fast_get_BID64_check_OF (sign_s, exponent_b + extra_digits, C64,
			     rnd_mode, pfpsf);
  BID_RETURN (res);
}
