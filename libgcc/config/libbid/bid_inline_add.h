/* Copyright (C) 2007-2013 Free Software Foundation, Inc.

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
 *
 *    Helper add functions (for fma)
 *
 *    __BID_INLINE__ UINT64 get_add64(
 *        UINT64 sign_x, int exponent_x, UINT64 coefficient_x, 
 *        UINT64 sign_y, int exponent_y, UINT64 coefficient_y, 
 *  					 int rounding_mode)
 *
 *   __BID_INLINE__ UINT64 get_add128(
 *                       UINT64 sign_x, int exponent_x, UINT64 coefficient_x, 
 *                       UINT64 sign_y, int final_exponent_y, UINT128 CY, 
 *                       int extra_digits, int rounding_mode)
 *
 *****************************************************************************
 *
 *  Algorithm description:
 *
 *  get_add64:  same as BID64 add, but arguments are unpacked and there 
 *                                 are no special case checks
 *
 *  get_add128: add 64-bit coefficient to 128-bit product (which contains 
 *                                        16+extra_digits decimal digits), 
 *                         return BID64 result
 *              - the exponents are compared and the two coefficients are 
 *                properly aligned for addition/subtraction
 *              - multiple paths are needed
 *              - final result exponent is calculated and the lower term is
 *                      rounded first if necessary, to avoid manipulating 
 *                      coefficients longer than 128 bits 
 *
 ****************************************************************************/

#ifndef _INLINE_BID_ADD_H_
#define _INLINE_BID_ADD_H_

#include "bid_internal.h"

#define MAX_FORMAT_DIGITS     16
#define DECIMAL_EXPONENT_BIAS 398
#define MASK_BINARY_EXPONENT  0x7ff0000000000000ull
#define BINARY_EXPONENT_BIAS  0x3ff
#define UPPER_EXPON_LIMIT     51

///////////////////////////////////////////////////////////////////////
//
// get_add64() is essentially the same as bid_add(), except that 
//             the arguments are unpacked
//
//////////////////////////////////////////////////////////////////////
__BID_INLINE__ UINT64
get_add64 (UINT64 sign_x, int exponent_x, UINT64 coefficient_x,
	   UINT64 sign_y, int exponent_y, UINT64 coefficient_y,
	   int rounding_mode, unsigned *fpsc) {
  UINT128 CA, CT, CT_new;
  UINT64 sign_a, sign_b, coefficient_a, coefficient_b, sign_s, sign_ab,
    rem_a;
  UINT64 saved_ca, saved_cb, C0_64, C64, remainder_h, T1, carry, tmp,
    C64_new;
  int_double tempx;
  int exponent_a, exponent_b, diff_dec_expon;
  int bin_expon_ca, extra_digits, amount, scale_k, scale_ca;
  unsigned rmode, status;

  // sort arguments by exponent
  if (exponent_x <= exponent_y) {
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

  tempx.d = (double) coefficient_a;
  bin_expon_ca = ((tempx.i & MASK_BINARY_EXPONENT) >> 52) - 0x3ff;

  if (!coefficient_a) {
    return get_BID64 (sign_b, exponent_b, coefficient_b, rounding_mode,
		      fpsc);
  }
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
	__set_status_flags (fpsc, INEXACT_EXCEPTION);
      }
#endif

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
      if (((rounding_mode) & 3) && coefficient_b)	// not ROUNDING_TO_NEAREST
      {
	switch (rounding_mode) {
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

      return get_BID64 (sign_a, exponent_a, coefficient_a,
			rounding_mode, fpsc);
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
      if (rounding_mode == ROUNDING_DOWN && (!coefficient_a)
	  && sign_a != sign_b)
	sign_s = 0x8000000000000000ull;
#endif
#endif
      return get_BID64 (sign_s, exponent_b, coefficient_a,
			rounding_mode, fpsc);
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
    rmode = rounding_mode;
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
    rmode = rounding_mode;
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
    //P_ca = power10_table_128[scale_ca].w[0];
    //P_ca_m1 = power10_table_128[scale_ca-1].w[0];
    if (coefficient_a >= power10_table_128[scale_ca].w[0]) {
      scale_ca++;
      //P_ca_m1 = P_ca;
      //P_ca = power10_table_128[scale_ca].w[0];
    }

    scale_k = 16 - scale_ca;

    // apply sign
    //Ts = (T1 + sign_ab) ^ sign_ab;

    // test range of ca
    //X = coefficient_a + Ts - P_ca_m1;

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
    // the following test is equivalent to 
    // ( (initial_coefficient_a + Ts) < P_ca && 
    //     (initial_coefficient_a + Ts) > P_ca_m1 ), 
    // which ensures the number of digits in coefficient_a does not change 
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

	  saved_cb +=
	    /*90000000000000000 */ +rem_a *
	    power10_table_128[diff_dec_expon].w[0];
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
      // check whether fractional part of initial_P/10^extra_digits 
      // is exactly .5
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
  __set_status_flags (fpsc, status);

#endif

  return get_BID64 (sign_s, exponent_b + extra_digits, C64,
		    rounding_mode, fpsc);
}


///////////////////////////////////////////////////////////////////
// round 128-bit coefficient and return result in BID64 format
// do not worry about midpoint cases
//////////////////////////////////////////////////////////////////
static UINT64
__bid_simple_round64_sticky (UINT64 sign, int exponent, UINT128 P,
			     int extra_digits, int rounding_mode,
			     unsigned *fpsc) {
  UINT128 Q_high, Q_low, C128;
  UINT64 C64;
  int amount, rmode;

  rmode = rounding_mode;
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
  if (sign && (unsigned) (rmode - 1) < 2)
    rmode = 3 - rmode;
#endif
#endif
  __add_128_64 (P, P, round_const_table[rmode][extra_digits]);

  // get P*(2^M[extra_digits])/10^extra_digits
  __mul_128x128_full (Q_high, Q_low, P,
		      reciprocals10_128[extra_digits]);

  // now get P/10^extra_digits: shift Q_high right by M[extra_digits]-128
  amount = recip_scale[extra_digits];
  __shr_128 (C128, Q_high, amount);

  C64 = __low_64 (C128);

#ifdef SET_STATUS_FLAGS

  __set_status_flags (fpsc, INEXACT_EXCEPTION);

#endif

  return get_BID64 (sign, exponent, C64, rounding_mode, fpsc);
}

///////////////////////////////////////////////////////////////////
// round 128-bit coefficient and return result in BID64 format
///////////////////////////////////////////////////////////////////
static UINT64
__bid_full_round64 (UINT64 sign, int exponent, UINT128 P,
		    int extra_digits, int rounding_mode,
		    unsigned *fpsc) {
  UINT128 Q_high, Q_low, C128, Stemp, PU;
  UINT64 remainder_h, C64, carry, CY;
  int amount, amount2, rmode, status = 0;

  if (exponent < 0) {
    if (exponent >= -16 && (extra_digits + exponent < 0)) {
      extra_digits = -exponent;
#ifdef SET_STATUS_FLAGS
      if (extra_digits > 0) {
	rmode = rounding_mode;
	if (sign && (unsigned) (rmode - 1) < 2)
	  rmode = 3 - rmode;
	__add_128_128 (PU, P,
		       round_const_table_128[rmode][extra_digits]);
	if (__unsigned_compare_gt_128
	    (power10_table_128[extra_digits + 15], PU))
	  status = UNDERFLOW_EXCEPTION;
      }
#endif
    }
  }

  if (extra_digits > 0) {
    exponent += extra_digits;
    rmode = rounding_mode;
    if (sign && (unsigned) (rmode - 1) < 2)
      rmode = 3 - rmode;
    __add_128_128 (P, P, round_const_table_128[rmode][extra_digits]);

    // get P*(2^M[extra_digits])/10^extra_digits
    __mul_128x128_full (Q_high, Q_low, P,
			reciprocals10_128[extra_digits]);

    // now get P/10^extra_digits: shift Q_high right by M[extra_digits]-128
    amount = recip_scale[extra_digits];
    __shr_128_long (C128, Q_high, amount);

    C64 = __low_64 (C128);

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
    if (rmode == 0)	//ROUNDING_TO_NEAREST
#endif
      if (C64 & 1) {
	// check whether fractional part of initial_P/10^extra_digits 
	// is exactly .5

	// get remainder
	amount2 = 64 - amount;
	remainder_h = 0;
	remainder_h--;
	remainder_h >>= amount2;
	remainder_h = remainder_h & Q_high.w[0];

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
    status |= INEXACT_EXCEPTION;

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

    __set_status_flags (fpsc, status);

#endif
  } else {
    C64 = P.w[0];
    if (!C64) {
      sign = 0;
      if (rounding_mode == ROUNDING_DOWN)
	sign = 0x8000000000000000ull;
    }
  }
  return get_BID64 (sign, exponent, C64, rounding_mode, fpsc);
}

/////////////////////////////////////////////////////////////////////////////////
// round 192-bit coefficient (P, remainder_P) and return result in BID64 format
// the lowest 64 bits (remainder_P) are used for midpoint checking only
////////////////////////////////////////////////////////////////////////////////
static UINT64
__bid_full_round64_remainder (UINT64 sign, int exponent, UINT128 P,
			      int extra_digits, UINT64 remainder_P,
			      int rounding_mode, unsigned *fpsc,
			      unsigned uf_status) {
  UINT128 Q_high, Q_low, C128, Stemp;
  UINT64 remainder_h, C64, carry, CY;
  int amount, amount2, rmode, status = uf_status;

  rmode = rounding_mode;
  if (sign && (unsigned) (rmode - 1) < 2)
    rmode = 3 - rmode;
  if (rmode == ROUNDING_UP && remainder_P) {
    P.w[0]++;
    if (!P.w[0])
      P.w[1]++;
  }

  if (extra_digits) {
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
      if (!remainder_P && (C64 & 1)) {
	// check whether fractional part of initial_P/10^extra_digits 
	// is exactly .5

	// get remainder
	amount2 = 64 - amount;
	remainder_h = 0;
	remainder_h--;
	remainder_h >>= amount2;
	remainder_h = remainder_h & Q_high.w[0];

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
    status |= INEXACT_EXCEPTION;

    if (!remainder_P) {
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
    }
    __set_status_flags (fpsc, status);

#endif
  } else {
    C64 = P.w[0];
#ifdef SET_STATUS_FLAGS
    if (remainder_P) {
      __set_status_flags (fpsc, uf_status | INEXACT_EXCEPTION);
    }
#endif
  }

  return get_BID64 (sign, exponent + extra_digits, C64, rounding_mode,
		    fpsc);
}


///////////////////////////////////////////////////////////////////
// get P/10^extra_digits
// result fits in 64 bits
///////////////////////////////////////////////////////////////////
__BID_INLINE__ UINT64
__truncate (UINT128 P, int extra_digits)
// extra_digits <= 16
{
  UINT128 Q_high, Q_low, C128;
  UINT64 C64;
  int amount;

  // get P*(2^M[extra_digits])/10^extra_digits
  __mul_128x128_full (Q_high, Q_low, P,
		      reciprocals10_128[extra_digits]);

  // now get P/10^extra_digits: shift Q_high right by M[extra_digits]-128
  amount = recip_scale[extra_digits];
  __shr_128 (C128, Q_high, amount);

  C64 = __low_64 (C128);

  return C64;
}


///////////////////////////////////////////////////////////////////
// return number of decimal digits in 128-bit value X
///////////////////////////////////////////////////////////////////
__BID_INLINE__ int
__get_dec_digits64 (UINT128 X) {
  int_double tempx;
  int digits_x, bin_expon_cx;

  if (!X.w[1]) {
    //--- get number of bits in the coefficients of x and y ---
    tempx.d = (double) X.w[0];
    bin_expon_cx = ((tempx.i & MASK_BINARY_EXPONENT) >> 52) - 0x3ff;
    // get number of decimal digits in the coeff_x
    digits_x = estimate_decimal_digits[bin_expon_cx];
    if (X.w[0] >= power10_table_128[digits_x].w[0])
      digits_x++;
    return digits_x;
  }
  tempx.d = (double) X.w[1];
  bin_expon_cx = ((tempx.i & MASK_BINARY_EXPONENT) >> 52) - 0x3ff;
  // get number of decimal digits in the coeff_x
  digits_x = estimate_decimal_digits[bin_expon_cx + 64];
  if (__unsigned_compare_ge_128 (X, power10_table_128[digits_x]))
    digits_x++;

  return digits_x;
}


////////////////////////////////////////////////////////////////////////////////
//
// add 64-bit coefficient to 128-bit coefficient, return result in BID64 format
//
////////////////////////////////////////////////////////////////////////////////
__BID_INLINE__ UINT64
get_add128 (UINT64 sign_x, int exponent_x, UINT64 coefficient_x,
	    UINT64 sign_y, int final_exponent_y, UINT128 CY,
	    int extra_digits, int rounding_mode, unsigned *fpsc) {
  UINT128 CY_L, CX, FS, F, CT, ST, T2;
  UINT64 CYh, CY0L, T, S, coefficient_y, remainder_y;
  SINT64 D = 0;
  int_double tempx;
  int diff_dec_expon, extra_digits2, exponent_y, status;
  int extra_dx, diff_dec2, bin_expon_cx, digits_x, rmode;

  // CY has more than 16 decimal digits

  exponent_y = final_exponent_y - extra_digits;

#ifdef IEEE_ROUND_NEAREST_TIES_AWAY
  rounding_mode = 0;
#endif
#ifdef IEEE_ROUND_NEAREST
  rounding_mode = 0;
#endif

  if (exponent_x > exponent_y) {
    // normalize x
    //--- get number of bits in the coefficients of x and y ---
    tempx.d = (double) coefficient_x;
    bin_expon_cx = ((tempx.i & MASK_BINARY_EXPONENT) >> 52) - 0x3ff;
    // get number of decimal digits in the coeff_x
    digits_x = estimate_decimal_digits[bin_expon_cx];
    if (coefficient_x >= power10_table_128[digits_x].w[0])
      digits_x++;

    extra_dx = 16 - digits_x;
    coefficient_x *= power10_table_128[extra_dx].w[0];
    if ((sign_x ^ sign_y) && (coefficient_x == 1000000000000000ull)) {
      extra_dx++;
      coefficient_x = 10000000000000000ull;
    }
    exponent_x -= extra_dx;

    if (exponent_x > exponent_y) {

      // exponent_x > exponent_y
      diff_dec_expon = exponent_x - exponent_y;

      if (exponent_x <= final_exponent_y + 1) {
	__mul_64x64_to_128 (CX, coefficient_x,
			    power10_table_128[diff_dec_expon].w[0]);

	if (sign_x == sign_y) {
	  __add_128_128 (CT, CY, CX);
	  if ((exponent_x >
	       final_exponent_y) /*&& (final_exponent_y>0) */ )
	    extra_digits++;
	  if (__unsigned_compare_ge_128
	      (CT, power10_table_128[16 + extra_digits]))
	    extra_digits++;
	} else {
	  __sub_128_128 (CT, CY, CX);
	  if (((SINT64) CT.w[1]) < 0) {
	    CT.w[0] = 0 - CT.w[0];
	    CT.w[1] = 0 - CT.w[1];
	    if (CT.w[0])
	      CT.w[1]--;
	    sign_y = sign_x;
	  } else if (!(CT.w[1] | CT.w[0])) {
	    sign_y =
	      (rounding_mode !=
	       ROUNDING_DOWN) ? 0 : 0x8000000000000000ull;
	  }
	  if ((exponent_x + 1 >=
	       final_exponent_y) /*&& (final_exponent_y>=0) */ ) {
	    extra_digits = __get_dec_digits64 (CT) - 16;
	    if (extra_digits <= 0) {
	      if (!CT.w[0] && rounding_mode == ROUNDING_DOWN)
		sign_y = 0x8000000000000000ull;
	      return get_BID64 (sign_y, exponent_y, CT.w[0],
				rounding_mode, fpsc);
	    }
	  } else
	    if (__unsigned_compare_gt_128
		(power10_table_128[15 + extra_digits], CT))
	    extra_digits--;
	}

	return __bid_full_round64 (sign_y, exponent_y, CT, extra_digits,
				   rounding_mode, fpsc);
      }
      // diff_dec2+extra_digits is the number of digits to eliminate from 
      //                           argument CY
      diff_dec2 = exponent_x - final_exponent_y;

      if (diff_dec2 >= 17) {
#ifndef IEEE_ROUND_NEAREST
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
	if ((rounding_mode) & 3) {
	  switch (rounding_mode) {
	  case ROUNDING_UP:
	    if (!sign_y) {
	      D = ((SINT64) (sign_x ^ sign_y)) >> 63;
	      D = D + D + 1;
	      coefficient_x += D;
	    }
	    break;
	  case ROUNDING_DOWN:
	    if (sign_y) {
	      D = ((SINT64) (sign_x ^ sign_y)) >> 63;
	      D = D + D + 1;
	      coefficient_x += D;
	    }
	    break;
	  case ROUNDING_TO_ZERO:
	    if (sign_y != sign_x) {
	      D = 0 - 1;
	      coefficient_x += D;
	    }
	    break;
	  }
	  if (coefficient_x < 1000000000000000ull) {
	    coefficient_x -= D;
	    coefficient_x =
	      D + (coefficient_x << 1) + (coefficient_x << 3);
	    exponent_x--;
	  }
	}
#endif
#endif
#ifdef SET_STATUS_FLAGS
	if (CY.w[1] | CY.w[0])
	  __set_status_flags (fpsc, INEXACT_EXCEPTION);
#endif
	return get_BID64 (sign_x, exponent_x, coefficient_x,
			  rounding_mode, fpsc);
      }
      // here exponent_x <= 16+final_exponent_y

      // truncate CY to 16 dec. digits
      CYh = __truncate (CY, extra_digits);

      // get remainder
      T = power10_table_128[extra_digits].w[0];
      __mul_64x64_to_64 (CY0L, CYh, T);

      remainder_y = CY.w[0] - CY0L;

      // align coeff_x, CYh
      __mul_64x64_to_128 (CX, coefficient_x,
			  power10_table_128[diff_dec2].w[0]);

      if (sign_x == sign_y) {
	__add_128_64 (CT, CX, CYh);
	if (__unsigned_compare_ge_128
	    (CT, power10_table_128[16 + diff_dec2]))
	  diff_dec2++;
      } else {
	if (remainder_y)
	  CYh++;
	__sub_128_64 (CT, CX, CYh);
	if (__unsigned_compare_gt_128
	    (power10_table_128[15 + diff_dec2], CT))
	  diff_dec2--;
      }

      return __bid_full_round64_remainder (sign_x, final_exponent_y, CT,
					   diff_dec2, remainder_y,
					   rounding_mode, fpsc, 0);
    }
  }
  // Here (exponent_x <= exponent_y)
  {
    diff_dec_expon = exponent_y - exponent_x;

    if (diff_dec_expon > MAX_FORMAT_DIGITS) {
      rmode = rounding_mode;

      if ((sign_x ^ sign_y)) {
	if (!CY.w[0])
	  CY.w[1]--;
	CY.w[0]--;
	if (__unsigned_compare_gt_128
	    (power10_table_128[15 + extra_digits], CY)) {
	  if (rmode & 3) {
	    extra_digits--;
	    final_exponent_y--;
	  } else {
	    CY.w[0] = 1000000000000000ull;
	    CY.w[1] = 0;
	    extra_digits = 0;
	  }
	}
      }
      __scale128_10 (CY, CY);
      extra_digits++;
      CY.w[0] |= 1;

      return __bid_simple_round64_sticky (sign_y, final_exponent_y, CY,
					  extra_digits, rmode, fpsc);
    }
    // apply sign to coeff_x
    sign_x ^= sign_y;
    sign_x = ((SINT64) sign_x) >> 63;
    CX.w[0] = (coefficient_x + sign_x) ^ sign_x;
    CX.w[1] = sign_x;

    // check whether CY (rounded to 16 digits) and CX have 
    //                     any digits in the same position
    diff_dec2 = final_exponent_y - exponent_x;

    if (diff_dec2 <= 17) {
      // align CY to 10^ex
      S = power10_table_128[diff_dec_expon].w[0];
      __mul_64x128_short (CY_L, S, CY);

      __add_128_128 (ST, CY_L, CX);
      extra_digits2 = __get_dec_digits64 (ST) - 16;
      return __bid_full_round64 (sign_y, exponent_x, ST, extra_digits2,
				 rounding_mode, fpsc);
    }
    // truncate CY to 16 dec. digits
    CYh = __truncate (CY, extra_digits);

    // get remainder
    T = power10_table_128[extra_digits].w[0];
    __mul_64x64_to_64 (CY0L, CYh, T);

    coefficient_y = CY.w[0] - CY0L;
    // add rounding constant
    rmode = rounding_mode;
    if (sign_y && (unsigned) (rmode - 1) < 2)
      rmode = 3 - rmode;
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
    if (!(rmode & 3))	//ROUNDING_TO_NEAREST
#endif
#endif
    {
      coefficient_y += round_const_table[rmode][extra_digits];
    }
    // align coefficient_y,  coefficient_x
    S = power10_table_128[diff_dec_expon].w[0];
    __mul_64x64_to_128 (F, coefficient_y, S);

    // fraction
    __add_128_128 (FS, F, CX);

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
    if (rmode == 0)	//ROUNDING_TO_NEAREST
#endif
    {
      // rounding code, here RN_EVEN
      // 10^(extra_digits+diff_dec_expon)
      T2 = power10_table_128[diff_dec_expon + extra_digits];
      if (__unsigned_compare_gt_128 (FS, T2)
	  || ((CYh & 1) && __test_equal_128 (FS, T2))) {
	CYh++;
	__sub_128_128 (FS, FS, T2);
      }
    }
#endif
#ifndef IEEE_ROUND_NEAREST
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
    if (rmode == 4)	//ROUNDING_TO_NEAREST
#endif
    {
      // rounding code, here RN_AWAY
      // 10^(extra_digits+diff_dec_expon)
      T2 = power10_table_128[diff_dec_expon + extra_digits];
      if (__unsigned_compare_ge_128 (FS, T2)) {
	CYh++;
	__sub_128_128 (FS, FS, T2);
      }
    }
#endif
#ifndef IEEE_ROUND_NEAREST
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
    switch (rmode) {
    case ROUNDING_DOWN:
    case ROUNDING_TO_ZERO:
      if ((SINT64) FS.w[1] < 0) {
	CYh--;
	if (CYh < 1000000000000000ull) {
	  CYh = 9999999999999999ull;
	  final_exponent_y--;
	}
      } else {
	T2 = power10_table_128[diff_dec_expon + extra_digits];
	if (__unsigned_compare_ge_128 (FS, T2)) {
	  CYh++;
	  __sub_128_128 (FS, FS, T2);
	}
      }
      break;
    case ROUNDING_UP:
      if ((SINT64) FS.w[1] < 0)
	break;
      T2 = power10_table_128[diff_dec_expon + extra_digits];
      if (__unsigned_compare_gt_128 (FS, T2)) {
	CYh += 2;
	__sub_128_128 (FS, FS, T2);
      } else if ((FS.w[1] == T2.w[1]) && (FS.w[0] == T2.w[0])) {
	CYh++;
	FS.w[1] = FS.w[0] = 0;
      } else if (FS.w[1] | FS.w[0])
	CYh++;
      break;
    }
#endif
#endif

#ifdef SET_STATUS_FLAGS
    status = INEXACT_EXCEPTION;
#ifndef IEEE_ROUND_NEAREST
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
    if (!(rmode & 3))
#endif
#endif
    {
      // RN modes
      if ((FS.w[1] ==
	   round_const_table_128[0][diff_dec_expon + extra_digits].w[1])
	  && (FS.w[0] ==
	      round_const_table_128[0][diff_dec_expon +
				       extra_digits].w[0]))
	status = EXACT_STATUS;
    }
#ifndef IEEE_ROUND_NEAREST
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
    else if (!FS.w[1] && !FS.w[0])
      status = EXACT_STATUS;
#endif
#endif

    __set_status_flags (fpsc, status);
#endif

    return get_BID64 (sign_y, final_exponent_y, CYh, rounding_mode,
		      fpsc);
  }

}

//////////////////////////////////////////////////////////////////////////
//
//  If coefficient_z is less than 16 digits long, normalize to 16 digits
//
/////////////////////////////////////////////////////////////////////////
static UINT64
BID_normalize (UINT64 sign_z, int exponent_z,
	       UINT64 coefficient_z, UINT64 round_dir, int round_flag,
	       int rounding_mode, unsigned *fpsc) {
  SINT64 D;
  int_double tempx;
  int digits_z, bin_expon, scale, rmode;

#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
#ifndef IEEE_ROUND_NEAREST
  rmode = rounding_mode;
  if (sign_z && (unsigned) (rmode - 1) < 2)
    rmode = 3 - rmode;
#else
  if (coefficient_z >= power10_table_128[15].w[0])
    return z;
#endif
#endif

  //--- get number of bits in the coefficients of x and y ---
  tempx.d = (double) coefficient_z;
  bin_expon = ((tempx.i & MASK_BINARY_EXPONENT) >> 52) - 0x3ff;
  // get number of decimal digits in the coeff_x
  digits_z = estimate_decimal_digits[bin_expon];
  if (coefficient_z >= power10_table_128[digits_z].w[0])
    digits_z++;

  scale = 16 - digits_z;
  exponent_z -= scale;
  if (exponent_z < 0) {
    scale += exponent_z;
    exponent_z = 0;
  }
  coefficient_z *= power10_table_128[scale].w[0];

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


//////////////////////////////////////////////////////////////////////////
//
//    0*10^ey + cz*10^ez,   ey<ez  
//
//////////////////////////////////////////////////////////////////////////

__BID_INLINE__ UINT64
add_zero64 (int exponent_y, UINT64 sign_z, int exponent_z,
	    UINT64 coefficient_z, unsigned *prounding_mode,
	    unsigned *fpsc) {
  int_double tempx;
  int bin_expon, scale_k, scale_cz;
  int diff_expon;

  diff_expon = exponent_z - exponent_y;

  tempx.d = (double) coefficient_z;
  bin_expon = ((tempx.i & MASK_BINARY_EXPONENT) >> 52) - 0x3ff;
  scale_cz = estimate_decimal_digits[bin_expon];
  if (coefficient_z >= power10_table_128[scale_cz].w[0])
    scale_cz++;

  scale_k = 16 - scale_cz;
  if (diff_expon < scale_k)
    scale_k = diff_expon;
  coefficient_z *= power10_table_128[scale_k].w[0];

  return get_BID64 (sign_z, exponent_z - scale_k, coefficient_z,
		    *prounding_mode, fpsc);
}
#endif
