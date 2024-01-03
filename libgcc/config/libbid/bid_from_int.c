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

#include "bid_internal.h"

/*****************************************************************************
 *  BID64_round_integral_exact
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_from_int32 (UINT64 * pres,
		  int *px _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  int x = *px;
#else
UINT64
bid64_from_int32 (int x _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT64 res;

  // if integer is negative, put the absolute value
  // in the lowest 32bits of the result
  if ((x & SIGNMASK32) == SIGNMASK32) {
    // negative int32
    x = ~x + 1;	// 2's complement of x
    res = (unsigned int) x | 0xb1c0000000000000ull;
    // (exp << 53)) = biased exp. is 0
  } else {	// positive int32
    res = x | 0x31c0000000000000ull;	// (exp << 53)) = biased exp. is 0
  }
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_from_uint32 (UINT64 * pres, unsigned int *px
		   _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  unsigned int x = *px;
#else
UINT64
bid64_from_uint32 (unsigned int x _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT64 res;

  res = x | 0x31c0000000000000ull;	// (exp << 53)) = biased exp. is 0
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_from_int64 (UINT64 * pres, SINT64 * px
		  _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		  _EXC_INFO_PARAM) {
  SINT64 x = *px;
#if !DECIMAL_GLOBAL_ROUNDING
  unsigned int rnd_mode = *prnd_mode;
#endif
#else
UINT64
bid64_from_int64 (SINT64 x
		  _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		  _EXC_INFO_PARAM) {
#endif

  UINT64 res;
  UINT64 x_sign, C;
  unsigned int q, ind;
  int incr_exp = 0;
  int is_midpoint_lt_even = 0, is_midpoint_gt_even = 0;
  int is_inexact_lt_midpoint = 0, is_inexact_gt_midpoint = 0;

  x_sign = x & 0x8000000000000000ull;
  // if the integer is negative, use the absolute value
  if (x_sign)
    C = ~((UINT64) x) + 1;
  else
    C = x;
  if (C <= BID64_SIG_MAX) {	// |C| <= 10^16-1 and the result is exact
    if (C < 0x0020000000000000ull) {	// C < 2^53
      res = x_sign | 0x31c0000000000000ull | C;
    } else {	// C >= 2^53
      res =
	x_sign | 0x6c70000000000000ull | (C & 0x0007ffffffffffffull);
    }
  } else {	// |C| >= 10^16 and the result may be inexact 
    // the smallest |C| is 10^16 which has 17 decimal digits
    // the largest |C| is 0x8000000000000000 = 9223372036854775808 w/ 19 digits
    if (C < 0x16345785d8a0000ull) {	// x < 10^17 
      q = 17;
      ind = 1;	// number of digits to remove for q = 17
    } else if (C < 0xde0b6b3a7640000ull) {	// C < 10^18
      q = 18;
      ind = 2;	// number of digits to remove for q = 18 
    } else {	// C < 10^19
      q = 19;
      ind = 3;	// number of digits to remove for q = 19
    }
    // overflow and underflow are not possible
    // Note: performace can be improved by inlining this call
    round64_2_18 (	// will work for 19 digits too if C fits in 64 bits
		   q, ind, C, &res, &incr_exp,
		   &is_midpoint_lt_even, &is_midpoint_gt_even,
		   &is_inexact_lt_midpoint, &is_inexact_gt_midpoint);
    if (incr_exp)
      ind++;
    // set the inexact flag
    if (is_inexact_lt_midpoint || is_inexact_gt_midpoint ||
	is_midpoint_lt_even || is_midpoint_gt_even)
      *pfpsf |= INEXACT_EXCEPTION;
    // general correction from RN to RA, RM, RP, RZ; result uses ind for exp
    if (rnd_mode != ROUNDING_TO_NEAREST) {
      if ((!x_sign
	   && ((rnd_mode == ROUNDING_UP && is_inexact_lt_midpoint)
	       ||
	       ((rnd_mode == ROUNDING_TIES_AWAY
		 || rnd_mode == ROUNDING_UP) && is_midpoint_gt_even)))
	  || (x_sign
	      && ((rnd_mode == ROUNDING_DOWN && is_inexact_lt_midpoint)
		  ||
		  ((rnd_mode == ROUNDING_TIES_AWAY
		    || rnd_mode == ROUNDING_DOWN)
		   && is_midpoint_gt_even)))) {
	res = res + 1;
	if (res == 0x002386f26fc10000ull) {	// res = 10^16 => rounding overflow
	  res = 0x00038d7ea4c68000ull;	// 10^15
	  ind = ind + 1;
	}
      } else if ((is_midpoint_lt_even || is_inexact_gt_midpoint) &&
		 ((x_sign && (rnd_mode == ROUNDING_UP ||
			      rnd_mode == ROUNDING_TO_ZERO)) ||
		  (!x_sign && (rnd_mode == ROUNDING_DOWN ||
			       rnd_mode == ROUNDING_TO_ZERO)))) {
	res = res - 1;
	// check if we crossed into the lower decade
	if (res == 0x00038d7ea4c67fffull) {	// 10^15 - 1
	  res = 0x002386f26fc0ffffull;	// 10^16 - 1
	  ind = ind - 1;
	}
      } else {
	;	// exact, the result is already correct
      }
    }
    if (res < 0x0020000000000000ull) {	// res < 2^53
      res = x_sign | (((UINT64) ind + 398) << 53) | res;
    } else {	// res >= 2^53 
      res =
	x_sign | 0x6000000000000000ull | (((UINT64) ind + 398) << 51) |
	(res & 0x0007ffffffffffffull);
    }
  }
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_from_uint64 (UINT64 * pres, UINT64 * px
		   _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		   _EXC_INFO_PARAM) {
  UINT64 x = *px;
#if !DECIMAL_GLOBAL_ROUNDING
  unsigned int rnd_mode = *prnd_mode;
#endif
#else
UINT64
bid64_from_uint64 (UINT64 x
		   _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		   _EXC_INFO_PARAM) {
#endif

  UINT64 res;
  UINT128 x128, res128;
  unsigned int q, ind;
  int incr_exp = 0;
  int is_midpoint_lt_even = 0, is_midpoint_gt_even = 0;
  int is_inexact_lt_midpoint = 0, is_inexact_gt_midpoint = 0;

  if (x <= BID64_SIG_MAX) {	// x <= 10^16-1 and the result is exact
    if (x < 0x0020000000000000ull) {	// x < 2^53
      res = 0x31c0000000000000ull | x;
    } else {	// x >= 2^53
      res = 0x6c70000000000000ull | (x & 0x0007ffffffffffffull);
    }
  } else {	// x >= 10^16 and the result may be inexact 
    // the smallest x is 10^16 which has 17 decimal digits
    // the largest x is 0xffffffffffffffff = 18446744073709551615 w/ 20 digits
    if (x < 0x16345785d8a0000ull) {	// x < 10^17 
      q = 17;
      ind = 1;	// number of digits to remove for q = 17
    } else if (x < 0xde0b6b3a7640000ull) {	// x < 10^18
      q = 18;
      ind = 2;	// number of digits to remove for q = 18 
    } else if (x < 0x8ac7230489e80000ull) {	// x < 10^19
      q = 19;
      ind = 3;	// number of digits to remove for q = 19
    } else {	// x < 10^20
      q = 20;
      ind = 4;	// number of digits to remove for q = 20
    }
    // overflow and underflow are not possible
    // Note: performace can be improved by inlining this call
    if (q <= 19) {
      round64_2_18 (	// will work for 20 digits too if x fits in 64 bits
		     q, ind, x, &res, &incr_exp,
		     &is_midpoint_lt_even, &is_midpoint_gt_even,
		     &is_inexact_lt_midpoint, &is_inexact_gt_midpoint);
    } else {	// q = 20
      x128.w[1] = 0x0;
      x128.w[0] = x;
      round128_19_38 (q, ind, x128, &res128, &incr_exp,
		      &is_midpoint_lt_even, &is_midpoint_gt_even,
		      &is_inexact_lt_midpoint, &is_inexact_gt_midpoint);
      res = res128.w[0];	// res.w[1] is 0
    }
    if (incr_exp)
      ind++;
    // set the inexact flag
    if (is_inexact_lt_midpoint || is_inexact_gt_midpoint ||
	is_midpoint_lt_even || is_midpoint_gt_even)
      *pfpsf |= INEXACT_EXCEPTION;
    // general correction from RN to RA, RM, RP, RZ; result uses ind for exp
    if (rnd_mode != ROUNDING_TO_NEAREST) {
      if ((rnd_mode == ROUNDING_UP && is_inexact_lt_midpoint) ||
	  ((rnd_mode == ROUNDING_TIES_AWAY || rnd_mode == ROUNDING_UP)
	   && is_midpoint_gt_even)) {
	res = res + 1;
	if (res == 0x002386f26fc10000ull) {	// res = 10^16 => rounding overflow
	  res = 0x00038d7ea4c68000ull;	// 10^15
	  ind = ind + 1;
	}
      } else if ((is_midpoint_lt_even || is_inexact_gt_midpoint) &&
		 (rnd_mode == ROUNDING_DOWN ||
		  rnd_mode == ROUNDING_TO_ZERO)) {
	res = res - 1;
	// check if we crossed into the lower decade
	if (res == 0x00038d7ea4c67fffull) {	// 10^15 - 1
	  res = 0x002386f26fc0ffffull;	// 10^16 - 1
	  ind = ind - 1;
	}
      } else {
	;	// exact, the result is already correct
      }
    }
    if (res < 0x0020000000000000ull) {	// res < 2^53
      res = (((UINT64) ind + 398) << 53) | res;
    } else {	// res >= 2^53 
      res = 0x6000000000000000ull | (((UINT64) ind + 398) << 51) |
	(res & 0x0007ffffffffffffull);
    }
  }
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid128_from_int32 (UINT128 * pres,
		   int *px _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  int x = *px;
#else
UINT128
bid128_from_int32 (int x _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT128 res;

  // if integer is negative, use the absolute value
  if ((x & SIGNMASK32) == SIGNMASK32) {
    res.w[HIGH_128W] = 0xb040000000000000ull;
    res.w[LOW_128W] = ~((unsigned int) x) + 1;	// 2's complement of x
  } else {
    res.w[HIGH_128W] = 0x3040000000000000ull;
    res.w[LOW_128W] = (unsigned int) x;
  }
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid128_from_uint32 (UINT128 * pres, unsigned int *px
		    _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  unsigned int x = *px;
#else
UINT128
bid128_from_uint32 (unsigned int x _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT128 res;

  res.w[HIGH_128W] = 0x3040000000000000ull;
  res.w[LOW_128W] = x;
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid128_from_int64 (UINT128 * pres, SINT64 * px
		   _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  SINT64 x = *px;
#else
UINT128
bid128_from_int64 (SINT64 x _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif

  UINT128 res;

  // if integer is negative, use the absolute value
  if ((x & SIGNMASK64) == SIGNMASK64) {
    res.w[HIGH_128W] = 0xb040000000000000ull;
    res.w[LOW_128W] = ~x + 1;	// 2's complement of x
  } else {
    res.w[HIGH_128W] = 0x3040000000000000ull;
    res.w[LOW_128W] = x;
  }
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid128_from_uint64 (UINT128 * pres, UINT64 * px
		    _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT128
bid128_from_uint64 (UINT64 x _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif

  UINT128 res;

  res.w[HIGH_128W] = 0x3040000000000000ull;
  res.w[LOW_128W] = x;
  BID_RETURN (res);
}
