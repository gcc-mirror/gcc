/* Copyright (C) 2007-2017 Free Software Foundation, Inc.

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

#include "bid_internal.h"

#if DECIMAL_CALL_BY_REFERENCE
void
bid64dq_mul (UINT64 * pres, UINT64 * px, UINT128 * py
	     _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	     _EXC_INFO_PARAM) {
  UINT64 x = *px;
#if !DECIMAL_GLOBAL_ROUNDING
  unsigned int rnd_mode = *prnd_mode;
#endif
#else
UINT64
bid64dq_mul (UINT64 x, UINT128 y
	     _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	     _EXC_INFO_PARAM) {
#endif
  UINT64 res = 0xbaddbaddbaddbaddull;
  UINT128 x1;

#if DECIMAL_CALL_BY_REFERENCE
  bid64_to_bid128 (&x1, &x _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  bid64qq_mul (&res, &x1, py
	       _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
	       _EXC_INFO_ARG);
#else
  x1 = bid64_to_bid128 (x _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  res = bid64qq_mul (x1, y
		     _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
		     _EXC_INFO_ARG);
#endif
  BID_RETURN (res);
}


#if DECIMAL_CALL_BY_REFERENCE
void
bid64qd_mul (UINT64 * pres, UINT128 * px, UINT64 * py
	     _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	     _EXC_INFO_PARAM) {
  UINT64 y = *py;
#if !DECIMAL_GLOBAL_ROUNDING
  unsigned int rnd_mode = *prnd_mode;
#endif
#else
UINT64
bid64qd_mul (UINT128 x, UINT64 y
	     _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	     _EXC_INFO_PARAM) {
#endif
  UINT64 res = 0xbaddbaddbaddbaddull;
  UINT128 y1;

#if DECIMAL_CALL_BY_REFERENCE
  bid64_to_bid128 (&y1, &y _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  bid64qq_mul (&res, px, &y1
	       _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
	       _EXC_INFO_ARG);
#else
  y1 = bid64_to_bid128 (y _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  res = bid64qq_mul (x, y1
		     _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
		     _EXC_INFO_ARG);
#endif
  BID_RETURN (res);
}


#if DECIMAL_CALL_BY_REFERENCE
void
bid64qq_mul (UINT64 * pres, UINT128 * px, UINT128 * py
	     _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	     _EXC_INFO_PARAM) {
  UINT128 x = *px, y = *py;
#if !DECIMAL_GLOBAL_ROUNDING
  unsigned int rnd_mode = *prnd_mode;
#endif
#else
UINT64
bid64qq_mul (UINT128 x, UINT128 y
	     _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	     _EXC_INFO_PARAM) {
#endif

  UINT128 z = { {0x0000000000000000ull, 0x5ffe000000000000ull}
  };
  UINT64 res = 0xbaddbaddbaddbaddull;
  UINT64 x_sign, y_sign, p_sign;
  UINT64 x_exp, y_exp, p_exp;
  int true_p_exp;
  UINT128 C1, C2;

  BID_SWAP128 (z);
  // skip cases where at least one operand is NaN or infinity
  if (!(((x.w[HIGH_128W] & MASK_NAN) == MASK_NAN) ||
	((y.w[HIGH_128W] & MASK_NAN) == MASK_NAN) ||
	((x.w[HIGH_128W] & MASK_ANY_INF) == MASK_INF) ||
	((y.w[HIGH_128W] & MASK_ANY_INF) == MASK_INF))) {
    // x, y are 0 or f but not inf or NaN => unpack the arguments and check
    // for non-canonical values

    x_sign = x.w[HIGH_128W] & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
    C1.w[1] = x.w[HIGH_128W] & MASK_COEFF;
    C1.w[0] = x.w[LOW_128W];
    // check for non-canonical values - treated as zero
    if ((x.w[HIGH_128W] & 0x6000000000000000ull) ==
	0x6000000000000000ull) {
      // G0_G1=11 => non-canonical
      x_exp = (x.w[HIGH_128W] << 2) & MASK_EXP;	// biased and shifted left 49 bits
      C1.w[1] = 0;	// significand high
      C1.w[0] = 0;	// significand low
    } else {	// G0_G1 != 11
      x_exp = x.w[HIGH_128W] & MASK_EXP;	// biased and shifted left 49 bits
      if (C1.w[1] > 0x0001ed09bead87c0ull ||
	  (C1.w[1] == 0x0001ed09bead87c0ull &&
	   C1.w[0] > 0x378d8e63ffffffffull)) {
	// x is non-canonical if coefficient is larger than 10^34 -1
	C1.w[1] = 0;
	C1.w[0] = 0;
      } else {	// canonical          
	;
      }
    }
    y_sign = y.w[HIGH_128W] & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
    C2.w[1] = y.w[HIGH_128W] & MASK_COEFF;
    C2.w[0] = y.w[LOW_128W];
    // check for non-canonical values - treated as zero
    if ((y.w[HIGH_128W] & 0x6000000000000000ull) ==
	0x6000000000000000ull) {
      // G0_G1=11 => non-canonical
      y_exp = (y.w[HIGH_128W] << 2) & MASK_EXP;	// biased and shifted left 49 bits
      C2.w[1] = 0;	// significand high
      C2.w[0] = 0;	// significand low 
    } else {	// G0_G1 != 11
      y_exp = y.w[HIGH_128W] & MASK_EXP;	// biased and shifted left 49 bits
      if (C2.w[1] > 0x0001ed09bead87c0ull ||
	  (C2.w[1] == 0x0001ed09bead87c0ull &&
	   C2.w[0] > 0x378d8e63ffffffffull)) {
	// y is non-canonical if coefficient is larger than 10^34 -1
	C2.w[1] = 0;
	C2.w[0] = 0;
      } else {	// canonical
	;
      }
    }
    p_sign = x_sign ^ y_sign;	// sign of the product

    true_p_exp = (x_exp >> 49) - 6176 + (y_exp >> 49) - 6176;
    // true_p_exp, p_exp are used only for 0 * 0, 0 * f, or f * 0 
    if (true_p_exp < -398)
      p_exp = 0;	// cannot be less than EXP_MIN
    else if (true_p_exp > 369)
      p_exp = (UINT64) (369 + 398) << 53;	// cannot be more than EXP_MAX
    else
      p_exp = (UINT64) (true_p_exp + 398) << 53;

    if ((C1.w[1] == 0x0 && C1.w[0] == 0x0) ||
	(C2.w[1] == 0x0 && C2.w[0] == 0x0)) {
      // x = 0 or y = 0
      // the result is 0
      res = p_sign | p_exp;	// preferred exponent in [EXP_MIN, EXP_MAX]
      BID_RETURN (res)
    }	// else continue
  }
  // swap x and y - ensure that a NaN in x has 'higher precedence' than one in y
#if DECIMAL_CALL_BY_REFERENCE
  bid64qqq_fma (&res, &y, &x, &z
		_RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
		_EXC_INFO_ARG);
#else
  res = bid64qqq_fma (y, x, z
		      _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
		      _EXC_INFO_ARG);
#endif
  BID_RETURN (res);
}


#if DECIMAL_CALL_BY_REFERENCE
void
bid128dd_mul (UINT128 * pres, UINT64 * px, UINT64 * py
	      _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	      _EXC_INFO_PARAM) {
  UINT64 x = *px, y = *py;
#if !DECIMAL_GLOBAL_ROUNDING
  unsigned int rnd_mode = *prnd_mode;
#endif
#else
UINT128
bid128dd_mul (UINT64 x, UINT64 y
	      _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	      _EXC_INFO_PARAM) {
#endif
  UINT128 res = { {0xbaddbaddbaddbaddull, 0xbaddbaddbaddbaddull}
  };
  UINT128 x1, y1;

#if DECIMAL_CALL_BY_REFERENCE
  bid64_to_bid128 (&x1, &x _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  bid64_to_bid128 (&y1, &y _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  bid128_mul (&res, &x1, &y1
	      _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
	      _EXC_INFO_ARG);
#else
  x1 = bid64_to_bid128 (x _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  y1 = bid64_to_bid128 (y _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  res = bid128_mul (x1, y1
		    _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
		    _EXC_INFO_ARG);
#endif
  BID_RETURN (res);
}


#if DECIMAL_CALL_BY_REFERENCE
void
bid128dq_mul (UINT128 * pres, UINT64 * px, UINT128 * py
	      _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	      _EXC_INFO_PARAM) {
  UINT64 x = *px;
#if !DECIMAL_GLOBAL_ROUNDING
  unsigned int rnd_mode = *prnd_mode;
#endif
#else
UINT128
bid128dq_mul (UINT64 x, UINT128 y
	      _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	      _EXC_INFO_PARAM) {
#endif
  UINT128 res = { {0xbaddbaddbaddbaddull, 0xbaddbaddbaddbaddull}
  };
  UINT128 x1;

#if DECIMAL_CALL_BY_REFERENCE
  bid64_to_bid128 (&x1, &x _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  bid128_mul (&res, &x1, py
	      _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
	      _EXC_INFO_ARG);
#else
  x1 = bid64_to_bid128 (x _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  res = bid128_mul (x1, y
		    _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
		    _EXC_INFO_ARG);
#endif
  BID_RETURN (res);
}


#if DECIMAL_CALL_BY_REFERENCE
void
bid128qd_mul (UINT128 * pres, UINT128 * px, UINT64 * py
	      _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	      _EXC_INFO_PARAM) {
  UINT64 y = *py;
#if !DECIMAL_GLOBAL_ROUNDING
  unsigned int rnd_mode = *prnd_mode;
#endif
#else
UINT128
bid128qd_mul (UINT128 x, UINT64 y
	      _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	      _EXC_INFO_PARAM) {
#endif
  UINT128 res = { {0xbaddbaddbaddbaddull, 0xbaddbaddbaddbaddull}
  };
  UINT128 y1;

#if DECIMAL_CALL_BY_REFERENCE
  bid64_to_bid128 (&y1, &y _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  bid128_mul (&res, px, &y1
	      _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
	      _EXC_INFO_ARG);
#else
  y1 = bid64_to_bid128 (y _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  res = bid128_mul (x, y1
		    _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
		    _EXC_INFO_ARG);
#endif
  BID_RETURN (res);
}


// bid128_mul stands for bid128qq_mul
#if DECIMAL_CALL_BY_REFERENCE
void
bid128_mul (UINT128 * pres, UINT128 * px,
	    UINT128 *
	    py _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	    _EXC_INFO_PARAM) {
  UINT128 x = *px, y = *py;

#if !DECIMAL_GLOBAL_ROUNDING
  unsigned int rnd_mode = *prnd_mode;

#endif
#else
UINT128
bid128_mul (UINT128 x,
	    UINT128 y _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	    _EXC_INFO_PARAM) {

#endif
  UINT128 z = { {0x0000000000000000ull, 0x5ffe000000000000ull}
  };
  UINT128 res = { {0xbaddbaddbaddbaddull, 0xbaddbaddbaddbaddull}
  };
  UINT64 x_sign, y_sign, p_sign;
  UINT64 x_exp, y_exp, p_exp;
  int true_p_exp;
  UINT128 C1, C2;

  BID_SWAP128 (x);
  BID_SWAP128 (y);
  // skip cases where at least one operand is NaN or infinity
  if (!(((x.w[1] & MASK_NAN) == MASK_NAN) ||
	((y.w[1] & MASK_NAN) == MASK_NAN) ||
	((x.w[1] & MASK_ANY_INF) == MASK_INF) ||
	((y.w[1] & MASK_ANY_INF) == MASK_INF))) {
    // x, y are 0 or f but not inf or NaN => unpack the arguments and check
    // for non-canonical values

    x_sign = x.w[1] & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
    C1.w[1] = x.w[1] & MASK_COEFF;
    C1.w[0] = x.w[0];
    // check for non-canonical values - treated as zero
    if ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull) {
      // G0_G1=11 => non-canonical
      x_exp = (x.w[1] << 2) & MASK_EXP;	// biased and shifted left 49 bits
      C1.w[1] = 0;	// significand high
      C1.w[0] = 0;	// significand low
    } else {	// G0_G1 != 11
      x_exp = x.w[1] & MASK_EXP;	// biased and shifted left 49 bits
      if (C1.w[1] > 0x0001ed09bead87c0ull ||
	  (C1.w[1] == 0x0001ed09bead87c0ull &&
	   C1.w[0] > 0x378d8e63ffffffffull)) {
	// x is non-canonical if coefficient is larger than 10^34 -1
	C1.w[1] = 0;
	C1.w[0] = 0;
      } else {	// canonical          
	;
      }
    }
    y_sign = y.w[1] & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
    C2.w[1] = y.w[1] & MASK_COEFF;
    C2.w[0] = y.w[0];
    // check for non-canonical values - treated as zero
    if ((y.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull) {
      // G0_G1=11 => non-canonical
      y_exp = (y.w[1] << 2) & MASK_EXP;	// biased and shifted left 49 bits
      C2.w[1] = 0;	// significand high
      C2.w[0] = 0;	// significand low 
    } else {	// G0_G1 != 11
      y_exp = y.w[1] & MASK_EXP;	// biased and shifted left 49 bits
      if (C2.w[1] > 0x0001ed09bead87c0ull ||
	  (C2.w[1] == 0x0001ed09bead87c0ull &&
	   C2.w[0] > 0x378d8e63ffffffffull)) {
	// y is non-canonical if coefficient is larger than 10^34 -1
	C2.w[1] = 0;
	C2.w[0] = 0;
      } else {	// canonical
	;
      }
    }
    p_sign = x_sign ^ y_sign;	// sign of the product

    true_p_exp = (x_exp >> 49) - 6176 + (y_exp >> 49) - 6176;
    // true_p_exp, p_exp are used only for 0 * 0, 0 * f, or f * 0 
    if (true_p_exp < -6176)
      p_exp = 0;	// cannot be less than EXP_MIN
    else if (true_p_exp > 6111)
      p_exp = (UINT64) (6111 + 6176) << 49;	// cannot be more than EXP_MAX
    else
      p_exp = (UINT64) (true_p_exp + 6176) << 49;

    if ((C1.w[1] == 0x0 && C1.w[0] == 0x0) ||
	(C2.w[1] == 0x0 && C2.w[0] == 0x0)) {
      // x = 0 or y = 0
      // the result is 0
      res.w[1] = p_sign | p_exp;	// preferred exponent in [EXP_MIN, EXP_MAX]
      res.w[0] = 0x0;
      BID_SWAP128 (res);
      BID_RETURN (res)
    }	// else continue
  }

  BID_SWAP128 (x);
  BID_SWAP128 (y);
  BID_SWAP128 (z);
  // swap x and y - ensure that a NaN in x has 'higher precedence' than one in y
#if DECIMAL_CALL_BY_REFERENCE
  bid128_fma (&res, &y, &x, &z
	      _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
	      _EXC_INFO_ARG);
#else
  res = bid128_fma (y, x, z
		    _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG
		    _EXC_INFO_ARG);
#endif
  BID_RETURN (res);
}
