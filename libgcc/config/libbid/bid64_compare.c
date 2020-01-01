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

#include "bid_internal.h"

static const UINT64 mult_factor[16] = {
  1ull, 10ull, 100ull, 1000ull,
  10000ull, 100000ull, 1000000ull, 10000000ull,
  100000000ull, 1000000000ull, 10000000000ull, 100000000000ull,
  1000000000000ull, 10000000000000ull,
  100000000000000ull, 1000000000000000ull
};

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_quiet_equal (int *pres, UINT64 * px,
		   UINT64 *
		   py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		   _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_quiet_equal (UINT64 x,
		   UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		   _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y, exp_t;
  UINT64 sig_x, sig_y, sig_t;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y, lcv;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered, 
  // rather than equal : return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    if ((x & MASK_SNAN) == MASK_SNAN || (y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if sNaN
    }
    res = 0;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equivalent.
  if (x == y) {
    res = 1;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if (((x & MASK_INF) == MASK_INF) && ((y & MASK_INF) == MASK_INF)) {
    res = (((x ^ y) & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // ONE INFINITY (CASE3')
  if (((x & MASK_INF) == MASK_INF) || ((y & MASK_INF) == MASK_INF)) {
    res = 0;
    BID_RETURN (res);
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }
  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //    therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  if (x_is_zero && y_is_zero) {
    res = 1;
    BID_RETURN (res);
  } else if ((x_is_zero && !y_is_zero) || (!x_is_zero && y_is_zero)) {
    res = 0;
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ => not equal : return 0
  if ((x ^ y) & MASK_SIGN) {
    res = 0;
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  if (exp_x > exp_y) {	// to simplify the loop below,
    SWAP (exp_x, exp_y, exp_t);	// put the larger exp in y,
    SWAP (sig_x, sig_y, sig_t);	// and the smaller exp in x
  }
  if (exp_y - exp_x > 15) {
    res = 0;	// difference cannot be greater than 10^15
    BID_RETURN (res);
  }
  for (lcv = 0; lcv < (exp_y - exp_x); lcv++) {
    // recalculate y's significand upwards
    sig_y = sig_y * 10;
    if (sig_y > 9999999999999999ull) {
      res = 0;
      BID_RETURN (res);
    }
  }
  res = (sig_y == sig_x);
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_quiet_greater (int *pres, UINT64 * px,
		     UINT64 *
		     py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		     _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_quiet_greater (UINT64 x,
		     UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		     _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered, rather than equal : 
  // return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    if ((x & MASK_SNAN) == MASK_SNAN || (y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if sNaN
    }
    res = 0;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal (not Greater).
  if (x == y) {
    res = 0;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x is neg infinity, there is no way it is greater than y, return 0
    if (((x & MASK_SIGN) == MASK_SIGN)) {
      res = 0;
      BID_RETURN (res);
    } else {
      // x is pos infinity, it is greater, unless y is positive 
      // infinity => return y!=pos_infinity
      res = (((y & MASK_INF) != MASK_INF)
	     || ((y & MASK_SIGN) == MASK_SIGN));
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so if y is positive infinity, then x is less, return 0
    //                 if y is negative infinity, then x is greater, return 1
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }
  // ZERO (CASE4)
  // some properties:
  //(+ZERO==-ZERO) => therefore ignore the sign, and neither number is greater
  //(ZERO x 10^A == ZERO x 10^B) for any valid A, B => therefore ignore the 
  // exponent field
  // (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  // if both numbers are zero, neither is greater => return NOTGREATERTHAN
  if (x_is_zero && y_is_zero) {
    res = 0;
    BID_RETURN (res);
  } else if (x_is_zero) {
    // is x is zero, it is greater if Y is negative
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  } else if (y_is_zero) {
    // is y is zero, X is greater if it is positive
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is greater if y is negative
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller, 
  // it is clear what needs to be done
  if (sig_x > sig_y && exp_x > exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x < exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {	// difference cannot be greater than 10^15
    if (x & MASK_SIGN)	// if both are negative
      res = 0;
    else	// if both are positive
      res = 1;
    BID_RETURN (res);
  }
  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    if (x & MASK_SIGN)	// if both are negative
      res = 1;
    else	// if both are positive
      res = 0;
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,
    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);
    // if postitive, return whichever significand is larger (converse if neg.)
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 0;
      BID_RETURN (res);
    }
    res = (((sig_n_prime.w[1] > 0)
	    || sig_n_prime.w[0] > sig_y) ^ ((x & MASK_SIGN) ==
					    MASK_SIGN));
    BID_RETURN (res);
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);
  // if postitive, return whichever significand is larger 
  //     (converse if negative)
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 0;
    BID_RETURN (res);
  }
  res = (((sig_n_prime.w[1] == 0)
	  && (sig_x > sig_n_prime.w[0])) ^ ((x & MASK_SIGN) ==
					    MASK_SIGN));
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_quiet_greater_equal (int *pres, UINT64 * px,
			   UINT64 *
			   py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			   _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_quiet_greater_equal (UINT64 x,
			   UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			   _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered : return 1
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    if ((x & MASK_SNAN) == MASK_SNAN || (y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if sNaN
    }
    res = 0;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal.
  if (x == y) {
    res = 1;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x==neg_inf, { res = (y == neg_inf)?1:0; BID_RETURN (res) }
    if ((x & MASK_SIGN) == MASK_SIGN) {
      // x is -inf, so it is less than y unless y is -inf
      res = (((y & MASK_INF) == MASK_INF)
	     && (y & MASK_SIGN) == MASK_SIGN);
      BID_RETURN (res);
    } else {	// x is pos_inf, no way for it to be less than y
      res = 1;
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so:
    //    if y is +inf, x<y
    //    if y is -inf, x>y
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }
  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither number is greater
  // (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //   therefore ignore the exponent field
  //  (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  if (x_is_zero && y_is_zero) {
    // if both numbers are zero, they are equal
    res = 1;
    BID_RETURN (res);
  } else if (x_is_zero) {
    // if x is zero, it is lessthan if Y is positive
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  } else if (y_is_zero) {
    // if y is zero, X is less if it is negative
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is less than if y is positive
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    // difference cannot be greater than 10^15
    BID_RETURN (res);
  }
  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,
    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);
    // return 1 if values are equal
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 1;
      BID_RETURN (res);
    }
    // if postitive, return whichever significand abs is smaller 
    // (converse if negative)
    res = (((sig_n_prime.w[1] == 0)
	    && sig_n_prime.w[0] < sig_y) ^ ((x & MASK_SIGN) !=
					    MASK_SIGN));
    BID_RETURN (res);
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);
  // return 0 if values are equal
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 1;
    BID_RETURN (res);
  }
  // if positive, return whichever significand abs is smaller 
  // (converse if negative)
  res = (((sig_n_prime.w[1] > 0)
	  || (sig_x < sig_n_prime.w[0])) ^ ((x & MASK_SIGN) !=
					    MASK_SIGN));
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_quiet_greater_unordered (int *pres, UINT64 * px,
			       UINT64 *
			       py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			       _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_quiet_greater_unordered (UINT64 x,
			       UINT64 y _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered, rather than equal : 
  // return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    if ((x & MASK_SNAN) == MASK_SNAN || (y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if sNaN
    }
    res = 1;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal (not Greater).
  if (x == y) {
    res = 0;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x is neg infinity, there is no way it is greater than y, return 0
    if (((x & MASK_SIGN) == MASK_SIGN)) {
      res = 0;
      BID_RETURN (res);
    } else {
      // x is pos infinity, it is greater, unless y is positive infinity => 
      // return y!=pos_infinity
      res = (((y & MASK_INF) != MASK_INF)
	     || ((y & MASK_SIGN) == MASK_SIGN));
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so if y is positive infinity, then x is less, return 0
    //                 if y is negative infinity, then x is greater, return 1
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }
  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither number is greater
  // (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  // therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  // if both numbers are zero, neither is greater => return NOTGREATERTHAN
  if (x_is_zero && y_is_zero) {
    res = 0;
    BID_RETURN (res);
  } else if (x_is_zero) {
    // is x is zero, it is greater if Y is negative
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  } else if (y_is_zero) {
    // is y is zero, X is greater if it is positive
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is greater if y is negative
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    // difference cannot be greater than 10^15
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,
    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);
    // if postitive, return whichever significand is larger 
    // (converse if negative)
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 0;
      BID_RETURN (res);
    }
    res = (((sig_n_prime.w[1] > 0)
	    || sig_n_prime.w[0] > sig_y) ^ ((x & MASK_SIGN) ==
					    MASK_SIGN));
    BID_RETURN (res);
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);
  // if postitive, return whichever significand is larger (converse if negative)
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 0;
    BID_RETURN (res);
  }
  res = (((sig_n_prime.w[1] == 0)
	  && (sig_x > sig_n_prime.w[0])) ^ ((x & MASK_SIGN) ==
					    MASK_SIGN));
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_quiet_less (int *pres, UINT64 * px,
		  UINT64 *
		  py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM _EXC_INFO_PARAM) 
{
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_quiet_less (UINT64 x,
		  UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		  _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered : return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    if ((x & MASK_SNAN) == MASK_SNAN || (y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if sNaN
    }
    res = 0;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal.
  if (x == y) {
    res = 0;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x==neg_inf, { res = (y == neg_inf)?0:1; BID_RETURN (res) }
    if ((x & MASK_SIGN) == MASK_SIGN) {
      // x is -inf, so it is less than y unless y is -inf
      res = (((y & MASK_INF) != MASK_INF)
	     || (y & MASK_SIGN) != MASK_SIGN);
      BID_RETURN (res);
    } else {
      // x is pos_inf, no way for it to be less than y
      res = 0;
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so:
    //    if y is +inf, x<y
    //    if y is -inf, x>y
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }
  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither number is greater
  // (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //  therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  if (x_is_zero && y_is_zero) {
    // if both numbers are zero, they are equal
    res = 0;
    BID_RETURN (res);
  } else if (x_is_zero) {
    // if x is zero, it is lessthan if Y is positive
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  } else if (y_is_zero) {
    // if y is zero, X is less if it is negative
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is less than if y is positive
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller, 
  // it is clear what needs to be done
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    // difference cannot be greater than 10^15
    BID_RETURN (res);
  }
  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,
    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);
    // return 0 if values are equal
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 0;
      BID_RETURN (res);
    }
    // if postitive, return whichever significand abs is smaller 
    // (converse if negative)
    res = (((sig_n_prime.w[1] == 0)
	    && sig_n_prime.w[0] < sig_y) ^ ((x & MASK_SIGN) ==
					    MASK_SIGN));
    BID_RETURN (res);
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);
  // return 0 if values are equal
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 0;
    BID_RETURN (res);
  }
  // if positive, return whichever significand abs is smaller 
  // (converse if negative)
  res = (((sig_n_prime.w[1] > 0)
	  || (sig_x < sig_n_prime.w[0])) ^ ((x & MASK_SIGN) ==
					    MASK_SIGN));
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_quiet_less_equal (int *pres, UINT64 * px,
			UINT64 *
			py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			_EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_quiet_less_equal (UINT64 x,
			UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			_EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered, rather than equal : 
  //     return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    if ((x & MASK_SNAN) == MASK_SNAN || (y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if sNaN
    }
    res = 0;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal (LESSEQUAL).
  if (x == y) {
    res = 1;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    if (((x & MASK_SIGN) == MASK_SIGN)) {
      // if x is neg infinity, it must be lessthan or equal to y return 1
      res = 1;
      BID_RETURN (res);
    } else {
      // x is pos infinity, it is greater, unless y is positive infinity => 
      // return y==pos_infinity
      res = !(((y & MASK_INF) != MASK_INF)
	      || ((y & MASK_SIGN) == MASK_SIGN));
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so if y is positive infinity, then x is less, return 1
    //                 if y is negative infinity, then x is greater, return 0
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }
  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither number is greater
  // (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //     therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  if (x_is_zero && y_is_zero) {
    // if both numbers are zero, they are equal -> return 1
    res = 1;
    BID_RETURN (res);
  } else if (x_is_zero) {
    // if x is zero, it is lessthan if Y is positive
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  } else if (y_is_zero) {
    // if y is zero, X is less if it is negative
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is less than if y is positive
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    // difference cannot be greater than 10^15
    BID_RETURN (res);
  }
  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,
    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);
    // return 1 if values are equal
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 1;
      BID_RETURN (res);
    }
    // if postitive, return whichever significand abs is smaller 
    //     (converse if negative)
    res = (((sig_n_prime.w[1] == 0)
	    && sig_n_prime.w[0] < sig_y) ^ ((x & MASK_SIGN) ==
					    MASK_SIGN));
    BID_RETURN (res);
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);
  // return 1 if values are equal
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 1;
    BID_RETURN (res);
  }
  // if positive, return whichever significand abs is smaller 
  //     (converse if negative)
  res = (((sig_n_prime.w[1] > 0)
	  || (sig_x < sig_n_prime.w[0])) ^ ((x & MASK_SIGN) ==
					    MASK_SIGN));
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_quiet_less_unordered (int *pres, UINT64 * px,
			    UINT64 *
			    py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			    _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_quiet_less_unordered (UINT64 x,
			    UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			    _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered : return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    if ((x & MASK_SNAN) == MASK_SNAN || (y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if sNaN
    }
    res = 1;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal.
  if (x == y) {
    res = 0;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x==neg_inf, { res = (y == neg_inf)?0:1; BID_RETURN (res) }
    if ((x & MASK_SIGN) == MASK_SIGN) {
      // x is -inf, so it is less than y unless y is -inf
      res = (((y & MASK_INF) != MASK_INF)
	     || (y & MASK_SIGN) != MASK_SIGN);
      BID_RETURN (res);
    } else {
      // x is pos_inf, no way for it to be less than y
      res = 0;
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so:
    //    if y is +inf, x<y
    //    if y is -inf, x>y
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }
  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither number is greater
  // (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //     therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  if (x_is_zero && y_is_zero) {
    // if both numbers are zero, they are equal
    res = 0;
    BID_RETURN (res);
  } else if (x_is_zero) {
    // if x is zero, it is lessthan if Y is positive
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  } else if (y_is_zero) {
    // if y is zero, X is less if it is negative
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is less than if y is positive
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    // difference cannot be greater than 10^15
    BID_RETURN (res);
  }
  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,
    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);
    // return 0 if values are equal
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 0;
      BID_RETURN (res);
    }
    // if postitive, return whichever significand abs is smaller 
    //     (converse if negative)
    res = (((sig_n_prime.w[1] == 0)
	    && sig_n_prime.w[0] < sig_y) ^ ((x & MASK_SIGN) ==
					    MASK_SIGN));
    BID_RETURN (res);
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);
  // return 0 if values are equal
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 0;
    BID_RETURN (res);
  }
  // if positive, return whichever significand abs is smaller 
  //     (converse if negative)
  res = (((sig_n_prime.w[1] > 0)
	  || (sig_x < sig_n_prime.w[0])) ^ ((x & MASK_SIGN) ==
					    MASK_SIGN));
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_quiet_not_equal (int *pres, UINT64 * px,
		       UINT64 *
		       py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		       _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_quiet_not_equal (UINT64 x,
		       UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		       _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y, exp_t;
  UINT64 sig_x, sig_y, sig_t;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y, lcv;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered, 
  // rather than equal : return 1
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    if ((x & MASK_SNAN) == MASK_SNAN || (y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if sNaN
    }
    res = 1;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equivalent.
  if (x == y) {
    res = 0;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if (((x & MASK_INF) == MASK_INF) && ((y & MASK_INF) == MASK_INF)) {
    res = (((x ^ y) & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // ONE INFINITY (CASE3')
  if (((x & MASK_INF) == MASK_INF) || ((y & MASK_INF) == MASK_INF)) {
    res = 1;
    BID_RETURN (res);
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }

  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //        therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }

  if (x_is_zero && y_is_zero) {
    res = 0;
    BID_RETURN (res);
  } else if ((x_is_zero && !y_is_zero) || (!x_is_zero && y_is_zero)) {
    res = 1;
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ => not equal : return 1
  if ((x ^ y) & MASK_SIGN) {
    res = 1;
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  if (exp_x > exp_y) {	// to simplify the loop below,
    SWAP (exp_x, exp_y, exp_t);	// put the larger exp in y,
    SWAP (sig_x, sig_y, sig_t);	// and the smaller exp in x
  }

  if (exp_y - exp_x > 15) {
    res = 1;
    BID_RETURN (res);
  }
  // difference cannot be greater than 10^16

  for (lcv = 0; lcv < (exp_y - exp_x); lcv++) {

    // recalculate y's significand upwards
    sig_y = sig_y * 10;
    if (sig_y > 9999999999999999ull) {
      res = 1;
      BID_RETURN (res);
    }
  }

  {
    res = sig_y != sig_x;
    BID_RETURN (res);
  }

}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_quiet_not_greater (int *pres, UINT64 * px,
			 UINT64 *
			 py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			 _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_quiet_not_greater (UINT64 x,
			 UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			 _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered, 
  //   rather than equal : return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    if ((x & MASK_SNAN) == MASK_SNAN || (y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if sNaN
    }
    res = 1;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal (LESSEQUAL).
  if (x == y) {
    res = 1;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x is neg infinity, it must be lessthan or equal to y return 1
    if (((x & MASK_SIGN) == MASK_SIGN)) {
      res = 1;
      BID_RETURN (res);
    }
    // x is pos infinity, it is greater, unless y is positive 
    // infinity => return y==pos_infinity
    else {
      res = !(((y & MASK_INF) != MASK_INF)
	      || ((y & MASK_SIGN) == MASK_SIGN));
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so if y is positive infinity, then x is less, return 1
    //                 if y is negative infinity, then x is greater, return 0
    {
      res = ((y & MASK_SIGN) != MASK_SIGN);
      BID_RETURN (res);
    }
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }

  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither 
  //         number is greater
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //         therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  // if both numbers are zero, they are equal -> return 1
  if (x_is_zero && y_is_zero) {
    res = 1;
    BID_RETURN (res);
  }
  // if x is zero, it is lessthan if Y is positive
  else if (x_is_zero) {
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if y is zero, X is less if it is negative
  else if (y_is_zero) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is less than if y is positive
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // difference cannot be greater than 10^15

  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,

    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);

    // return 1 if values are equal
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 1;
      BID_RETURN (res);
    }
    // if postitive, return whichever significand abs is smaller 
    //     (converse if negative)
    {
      res = (((sig_n_prime.w[1] == 0)
	      && sig_n_prime.w[0] < sig_y) ^ ((x & MASK_SIGN) ==
					      MASK_SIGN));
      BID_RETURN (res);
    }
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);

  // return 1 if values are equal
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 1;
    BID_RETURN (res);
  }
  // if positive, return whichever significand abs is smaller 
  //     (converse if negative)
  {
    res = (((sig_n_prime.w[1] > 0)
	    || (sig_x < sig_n_prime.w[0])) ^ ((x & MASK_SIGN) ==
					      MASK_SIGN));
    BID_RETURN (res);
  }
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_quiet_not_less (int *pres, UINT64 * px,
		      UINT64 *
		      py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		      _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_quiet_not_less (UINT64 x,
		      UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		      _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered : return 1
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    if ((x & MASK_SNAN) == MASK_SNAN || (y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if sNaN
    }
    res = 1;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal.
  if (x == y) {
    res = 1;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x==neg_inf, { res = (y == neg_inf)?1:0; BID_RETURN (res) }
    if ((x & MASK_SIGN) == MASK_SIGN)
      // x is -inf, so it is less than y unless y is -inf
    {
      res = (((y & MASK_INF) == MASK_INF)
	     && (y & MASK_SIGN) == MASK_SIGN);
      BID_RETURN (res);
    } else
      // x is pos_inf, no way for it to be less than y
    {
      res = 1;
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so:
    //    if y is +inf, x<y
    //    if y is -inf, x>y
    {
      res = ((y & MASK_SIGN) == MASK_SIGN);
      BID_RETURN (res);
    }
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }

  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither 
  //        number is greater
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //        therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  // if both numbers are zero, they are equal
  if (x_is_zero && y_is_zero) {
    res = 1;
    BID_RETURN (res);
  }
  // if x is zero, it is lessthan if Y is positive
  else if (x_is_zero) {
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if y is zero, X is less if it is negative
  else if (y_is_zero) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is less than if y is positive
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // difference cannot be greater than 10^15

  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,

    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);

    // return 0 if values are equal
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 1;
      BID_RETURN (res);
    }
    // if postitive, return whichever significand abs is smaller 
    //     (converse if negative)
    {
      res = (((sig_n_prime.w[1] == 0)
	      && sig_n_prime.w[0] < sig_y) ^ ((x & MASK_SIGN) !=
					      MASK_SIGN));
      BID_RETURN (res);
    }
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);

  // return 0 if values are equal
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 1;
    BID_RETURN (res);
  }
  // if positive, return whichever significand abs is smaller 
  //     (converse if negative)
  {
    res = (((sig_n_prime.w[1] > 0)
	    || (sig_x < sig_n_prime.w[0])) ^ ((x & MASK_SIGN) !=
					      MASK_SIGN));
    BID_RETURN (res);
  }
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_quiet_ordered (int *pres, UINT64 * px,
		     UINT64 *
		     py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		     _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_quiet_ordered (UINT64 x,
		     UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		     _EXC_INFO_PARAM) {
#endif
  int res;

  // NaN (CASE1)
  // if either number is NAN, the comparison is ordered, rather than equal : return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    if ((x & MASK_SNAN) == MASK_SNAN || (y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if sNaN
    }
    res = 0;
    BID_RETURN (res);
  } else {
    res = 1;
    BID_RETURN (res);
  }
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_quiet_unordered (int *pres, UINT64 * px,
		       UINT64 *
		       py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		       _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_quiet_unordered (UINT64 x,
		       UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		       _EXC_INFO_PARAM) {
#endif
  int res;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered, 
  //     rather than equal : return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    if ((x & MASK_SNAN) == MASK_SNAN || (y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if sNaN
    }
    res = 1;
    BID_RETURN (res);
  } else {
    res = 0;
    BID_RETURN (res);
  }
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_signaling_greater (int *pres, UINT64 * px,
			 UINT64 *
			 py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			 _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_signaling_greater (UINT64 x,
			 UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			 _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered, 
  //     rather than equal : return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    *pfpsf |= INVALID_EXCEPTION;	// set invalid exception if NaN
    res = 0;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal (not Greater).
  if (x == y) {
    res = 0;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x is neg infinity, there is no way it is greater than y, return 0
    if (((x & MASK_SIGN) == MASK_SIGN)) {
      res = 0;
      BID_RETURN (res);
    }
    // x is pos infinity, it is greater, 
    // unless y is positive infinity => return y!=pos_infinity
    else {
      res = (((y & MASK_INF) != MASK_INF)
	     || ((y & MASK_SIGN) == MASK_SIGN));
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so if y is positive infinity, then x is less, return 0
    //                 if y is negative infinity, then x is greater, return 1
    {
      res = ((y & MASK_SIGN) == MASK_SIGN);
      BID_RETURN (res);
    }
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }

  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither number is greater
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //      therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  // if both numbers are zero, neither is greater => return NOTGREATERTHAN
  if (x_is_zero && y_is_zero) {
    res = 0;
    BID_RETURN (res);
  }
  // is x is zero, it is greater if Y is negative
  else if (x_is_zero) {
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // is y is zero, X is greater if it is positive
  else if (y_is_zero) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is greater if y is negative
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)

  // if both components are either bigger or smaller
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // difference cannot be greater than 10^15

  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,

    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);


    // if postitive, return whichever significand is larger 
    //     (converse if negative)
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 0;
      BID_RETURN (res);
    }

    {
      res = (((sig_n_prime.w[1] > 0)
	      || sig_n_prime.w[0] > sig_y) ^ ((x & MASK_SIGN) ==
					      MASK_SIGN));
      BID_RETURN (res);
    }
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);

  // if postitive, return whichever significand is larger 
  //     (converse if negative)
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 0;
    BID_RETURN (res);
  }
  {
    res = (((sig_n_prime.w[1] == 0)
	    && (sig_x > sig_n_prime.w[0])) ^ ((x & MASK_SIGN) ==
					      MASK_SIGN));
    BID_RETURN (res);
  }
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_signaling_greater_equal (int *pres, UINT64 * px,
			       UINT64 *
			       py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			       _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_signaling_greater_equal (UINT64 x,
			       UINT64 y _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered : return 1
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    *pfpsf |= INVALID_EXCEPTION;	// set invalid exception if NaN
    res = 0;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal.
  if (x == y) {
    res = 1;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x==neg_inf, { res = (y == neg_inf)?1:0; BID_RETURN (res) }
    if ((x & MASK_SIGN) == MASK_SIGN)
      // x is -inf, so it is less than y unless y is -inf
    {
      res = (((y & MASK_INF) == MASK_INF)
	     && (y & MASK_SIGN) == MASK_SIGN);
      BID_RETURN (res);
    } else
      // x is pos_inf, no way for it to be less than y
    {
      res = 1;
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so:
    //    if y is +inf, x<y
    //    if y is -inf, x>y
    {
      res = ((y & MASK_SIGN) == MASK_SIGN);
      BID_RETURN (res);
    }
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }

  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither number is greater
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //      therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  // if both numbers are zero, they are equal
  if (x_is_zero && y_is_zero) {
    res = 1;
    BID_RETURN (res);
  }
  // if x is zero, it is lessthan if Y is positive
  else if (x_is_zero) {
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if y is zero, X is less if it is negative
  else if (y_is_zero) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is less than if y is positive
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // difference cannot be greater than 10^15

  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,

    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);

    // return 1 if values are equal
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 1;
      BID_RETURN (res);
    }
    // if postitive, return whichever significand abs is smaller 
    //     (converse if negative)
    {
      res = (((sig_n_prime.w[1] == 0)
	      && sig_n_prime.w[0] < sig_y) ^ ((x & MASK_SIGN) !=
					      MASK_SIGN));
      BID_RETURN (res);
    }
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);

  // return 0 if values are equal
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 1;
    BID_RETURN (res);
  }
  // if positive, return whichever significand abs is smaller 
  //     (converse if negative)
  {
    res = (((sig_n_prime.w[1] > 0)
	    || (sig_x < sig_n_prime.w[0])) ^ ((x & MASK_SIGN) !=
					      MASK_SIGN));
    BID_RETURN (res);
  }
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_signaling_greater_unordered (int *pres, UINT64 * px,
				   UINT64 *
				   py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				   _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_signaling_greater_unordered (UINT64 x,
				   UINT64 y _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered, 
  // rather than equal : return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    *pfpsf |= INVALID_EXCEPTION;	// set invalid exception if NaN
    res = 1;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal (not Greater).
  if (x == y) {
    res = 0;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x is neg infinity, there is no way it is greater than y, return 0
    if (((x & MASK_SIGN) == MASK_SIGN)) {
      res = 0;
      BID_RETURN (res);
    }
    // x is pos infinity, it is greater, 
    // unless y is positive infinity => return y!=pos_infinity
    else {
      res = (((y & MASK_INF) != MASK_INF)
	     || ((y & MASK_SIGN) == MASK_SIGN));
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so if y is positive infinity, then x is less, return 0
    //                 if y is negative infinity, then x is greater, return 1
    {
      res = ((y & MASK_SIGN) == MASK_SIGN);
      BID_RETURN (res);
    }
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }

  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither number is greater
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //      therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  // if both numbers are zero, neither is greater => return NOTGREATERTHAN
  if (x_is_zero && y_is_zero) {
    res = 0;
    BID_RETURN (res);
  }
  // is x is zero, it is greater if Y is negative
  else if (x_is_zero) {
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // is y is zero, X is greater if it is positive
  else if (y_is_zero) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is greater if y is negative
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)

  // if both components are either bigger or smaller
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // difference cannot be greater than 10^15

  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,

    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);

    // if postitive, return whichever significand is larger 
    //     (converse if negative)
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 0;
      BID_RETURN (res);
    }

    {
      res = (((sig_n_prime.w[1] > 0)
	      || sig_n_prime.w[0] > sig_y) ^ ((x & MASK_SIGN) ==
					      MASK_SIGN));
      BID_RETURN (res);
    }
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);

  // if postitive, return whichever significand is larger 
  //     (converse if negative)
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 0;
    BID_RETURN (res);
  }
  {
    res = (((sig_n_prime.w[1] == 0)
	    && (sig_x > sig_n_prime.w[0])) ^ ((x & MASK_SIGN) ==
					      MASK_SIGN));
    BID_RETURN (res);
  }
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_signaling_less (int *pres, UINT64 * px,
		      UINT64 *
		      py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		      _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_signaling_less (UINT64 x,
		      UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		      _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered : return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    *pfpsf |= INVALID_EXCEPTION;	// set invalid exception if NaN
    res = 0;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal.
  if (x == y) {
    res = 0;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x==neg_inf, { res = (y == neg_inf)?0:1; BID_RETURN (res) }
    if ((x & MASK_SIGN) == MASK_SIGN)
      // x is -inf, so it is less than y unless y is -inf
    {
      res = (((y & MASK_INF) != MASK_INF)
	     || (y & MASK_SIGN) != MASK_SIGN);
      BID_RETURN (res);
    } else
      // x is pos_inf, no way for it to be less than y
    {
      res = 0;
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so:
    //    if y is +inf, x<y
    //    if y is -inf, x>y
    {
      res = ((y & MASK_SIGN) != MASK_SIGN);
      BID_RETURN (res);
    }
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }

  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither number is greater
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //      therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  // if both numbers are zero, they are equal
  if (x_is_zero && y_is_zero) {
    res = 0;
    BID_RETURN (res);
  }
  // if x is zero, it is lessthan if Y is positive
  else if (x_is_zero) {
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if y is zero, X is less if it is negative
  else if (y_is_zero) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is less than if y is positive
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // difference cannot be greater than 10^15

  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,

    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);

    // return 0 if values are equal
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 0;
      BID_RETURN (res);
    }
    // if postitive, return whichever significand abs is smaller 
    //     (converse if negative)
    {
      res = (((sig_n_prime.w[1] == 0)
	      && sig_n_prime.w[0] < sig_y) ^ ((x & MASK_SIGN) ==
					      MASK_SIGN));
      BID_RETURN (res);
    }
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);

  // return 0 if values are equal
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 0;
    BID_RETURN (res);
  }
  // if positive, return whichever significand abs is smaller 
  //     (converse if negative)
  {
    res = (((sig_n_prime.w[1] > 0)
	    || (sig_x < sig_n_prime.w[0])) ^ ((x & MASK_SIGN) ==
					      MASK_SIGN));
    BID_RETURN (res);
  }
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_signaling_less_equal (int *pres, UINT64 * px,
			    UINT64 *
			    py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			    _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_signaling_less_equal (UINT64 x,
			    UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			    _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered, 
  // rather than equal : return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    *pfpsf |= INVALID_EXCEPTION;	// set invalid exception if NaN
    res = 0;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal (LESSEQUAL).
  if (x == y) {
    res = 1;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x is neg infinity, it must be lessthan or equal to y return 1
    if (((x & MASK_SIGN) == MASK_SIGN)) {
      res = 1;
      BID_RETURN (res);
    }
    // x is pos infinity, it is greater, 
    // unless y is positive infinity => return y==pos_infinity
    else {
      res = !(((y & MASK_INF) != MASK_INF)
	      || ((y & MASK_SIGN) == MASK_SIGN));
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so if y is positive infinity, then x is less, return 1
    //                 if y is negative infinity, then x is greater, return 0
    {
      res = ((y & MASK_SIGN) != MASK_SIGN);
      BID_RETURN (res);
    }
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }

  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither number is greater
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //      therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  // if both numbers are zero, they are equal -> return 1
  if (x_is_zero && y_is_zero) {
    res = 1;
    BID_RETURN (res);
  }
  // if x is zero, it is lessthan if Y is positive
  else if (x_is_zero) {
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if y is zero, X is less if it is negative
  else if (y_is_zero) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is less than if y is positive
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // difference cannot be greater than 10^15

  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,

    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);

    // return 1 if values are equal
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 1;
      BID_RETURN (res);
    }
    // if postitive, return whichever significand abs is smaller 
    //     (converse if negative)
    {
      res = (((sig_n_prime.w[1] == 0)
	      && sig_n_prime.w[0] < sig_y) ^ ((x & MASK_SIGN) ==
					      MASK_SIGN));
      BID_RETURN (res);
    }
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);

  // return 1 if values are equal
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 1;
    BID_RETURN (res);
  }
  // if positive, return whichever significand abs is smaller 
  //     (converse if negative)
  {
    res = (((sig_n_prime.w[1] > 0)
	    || (sig_x < sig_n_prime.w[0])) ^ ((x & MASK_SIGN) ==
					      MASK_SIGN));
    BID_RETURN (res);
  }
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_signaling_less_unordered (int *pres, UINT64 * px,
				UINT64 *
				py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				_EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_signaling_less_unordered (UINT64 x,
				UINT64 y _EXC_FLAGS_PARAM
				_EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered : return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    *pfpsf |= INVALID_EXCEPTION;	// set invalid exception if NaN
    res = 1;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal.
  if (x == y) {
    res = 0;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x==neg_inf, { res = (y == neg_inf)?0:1; BID_RETURN (res) }
    if ((x & MASK_SIGN) == MASK_SIGN)
      // x is -inf, so it is less than y unless y is -inf
    {
      res = (((y & MASK_INF) != MASK_INF)
	     || (y & MASK_SIGN) != MASK_SIGN);
      BID_RETURN (res);
    } else
      // x is pos_inf, no way for it to be less than y
    {
      res = 0;
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so:
    //    if y is +inf, x<y
    //    if y is -inf, x>y
    {
      res = ((y & MASK_SIGN) != MASK_SIGN);
      BID_RETURN (res);
    }
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }

  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither number is greater
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //      therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  // if both numbers are zero, they are equal
  if (x_is_zero && y_is_zero) {
    res = 0;
    BID_RETURN (res);
  }
  // if x is zero, it is lessthan if Y is positive
  else if (x_is_zero) {
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if y is zero, X is less if it is negative
  else if (y_is_zero) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is less than if y is positive
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // difference cannot be greater than 10^15

  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,

    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);

    // return 0 if values are equal
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 0;
      BID_RETURN (res);
    }
    // if postitive, return whichever significand abs is smaller 
    //     (converse if negative)
    {
      res = (((sig_n_prime.w[1] == 0)
	      && sig_n_prime.w[0] < sig_y) ^ ((x & MASK_SIGN) ==
					      MASK_SIGN));
      BID_RETURN (res);
    }
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);

  // return 0 if values are equal
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 0;
    BID_RETURN (res);
  }
  // if positive, return whichever significand abs is smaller 
  //     (converse if negative)
  {
    res = (((sig_n_prime.w[1] > 0)
	    || (sig_x < sig_n_prime.w[0])) ^ ((x & MASK_SIGN) ==
					      MASK_SIGN));
    BID_RETURN (res);
  }
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_signaling_not_greater (int *pres, UINT64 * px,
			     UINT64 *
			     py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			     _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_signaling_not_greater (UINT64 x,
			     UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			     _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered, 
  // rather than equal : return 0
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    *pfpsf |= INVALID_EXCEPTION;	// set invalid exception if NaN
    res = 1;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal (LESSEQUAL).
  if (x == y) {
    res = 1;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x is neg infinity, it must be lessthan or equal to y return 1
    if (((x & MASK_SIGN) == MASK_SIGN)) {
      res = 1;
      BID_RETURN (res);
    }
    // x is pos infinity, it is greater, 
    // unless y is positive infinity => return y==pos_infinity
    else {
      res = !(((y & MASK_INF) != MASK_INF)
	      || ((y & MASK_SIGN) == MASK_SIGN));
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so if y is positive infinity, then x is less, return 1
    //                 if y is negative infinity, then x is greater, return 0
    {
      res = ((y & MASK_SIGN) != MASK_SIGN);
      BID_RETURN (res);
    }
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }

  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither number is greater
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //      therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  // if both numbers are zero, they are equal -> return 1
  if (x_is_zero && y_is_zero) {
    res = 1;
    BID_RETURN (res);
  }
  // if x is zero, it is lessthan if Y is positive
  else if (x_is_zero) {
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if y is zero, X is less if it is negative
  else if (y_is_zero) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is less than if y is positive
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // difference cannot be greater than 10^15

  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,

    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);

    // return 1 if values are equal
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 1;
      BID_RETURN (res);
    }
    // if postitive, return whichever significand abs is smaller 
    //     (converse if negative)
    {
      res = (((sig_n_prime.w[1] == 0)
	      && sig_n_prime.w[0] < sig_y) ^ ((x & MASK_SIGN) ==
					      MASK_SIGN));
      BID_RETURN (res);
    }
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);

  // return 1 if values are equal
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 1;
    BID_RETURN (res);
  }
  // if positive, return whichever significand abs is smaller 
  //     (converse if negative)
  {
    res = (((sig_n_prime.w[1] > 0)
	    || (sig_x < sig_n_prime.w[0])) ^ ((x & MASK_SIGN) ==
					      MASK_SIGN));
    BID_RETURN (res);
  }
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_signaling_not_less (int *pres, UINT64 * px,
			  UINT64 *
			  py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			  _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
int
bid64_signaling_not_less (UINT64 x,
			  UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			  _EXC_INFO_PARAM) {
#endif
  int res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0, non_canon_x, non_canon_y;

  // NaN (CASE1)
  // if either number is NAN, the comparison is unordered : return 1
  if (((x & MASK_NAN) == MASK_NAN) || ((y & MASK_NAN) == MASK_NAN)) {
    *pfpsf |= INVALID_EXCEPTION;	// set invalid exception if NaN
    res = 1;
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal.
  if (x == y) {
    res = 1;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x==neg_inf, { res = (y == neg_inf)?1:0; BID_RETURN (res) }
    if ((x & MASK_SIGN) == MASK_SIGN)
      // x is -inf, so it is less than y unless y is -inf
    {
      res = (((y & MASK_INF) == MASK_INF)
	     && (y & MASK_SIGN) == MASK_SIGN);
      BID_RETURN (res);
    } else
      // x is pos_inf, no way for it to be less than y
    {
      res = 1;
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so:
    //    if y is +inf, x<y
    //    if y is -inf, x>y
    {
      res = ((y & MASK_SIGN) == MASK_SIGN);
      BID_RETURN (res);
    }
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_x > 9999999999999999ull) {
      non_canon_x = 1;
    } else {
      non_canon_x = 0;
    }
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
    non_canon_x = 0;
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (sig_y > 9999999999999999ull) {
      non_canon_y = 1;
    } else {
      non_canon_y = 0;
    }
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
    non_canon_y = 0;
  }

  // ZERO (CASE4)
  // some properties:
  // (+ZERO==-ZERO) => therefore ignore the sign, and neither number is greater
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //      therefore ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (non_canon_x || sig_x == 0) {
    x_is_zero = 1;
  }
  if (non_canon_y || sig_y == 0) {
    y_is_zero = 1;
  }
  // if both numbers are zero, they are equal
  if (x_is_zero && y_is_zero) {
    res = 1;
    BID_RETURN (res);
  }
  // if x is zero, it is lessthan if Y is positive
  else if (x_is_zero) {
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if y is zero, X is less if it is negative
  else if (y_is_zero) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is less than if y is positive
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN);
    BID_RETURN (res);
  }
  // difference cannot be greater than 10^15

  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN);
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,

    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);

    // return 0 if values are equal
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      res = 1;
      BID_RETURN (res);
    }
    // if postitive, return whichever significand abs is smaller 
    //     (converse if negative)
    {
      res = (((sig_n_prime.w[1] == 0)
	      && sig_n_prime.w[0] < sig_y) ^ ((x & MASK_SIGN) !=
					      MASK_SIGN));
      BID_RETURN (res);
    }
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);

  // return 0 if values are equal
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = 1;
    BID_RETURN (res);
  }
  // if positive, return whichever significand abs is smaller 
  //     (converse if negative)
  {
    res = (((sig_n_prime.w[1] > 0)
	    || (sig_x < sig_n_prime.w[0])) ^ ((x & MASK_SIGN) !=
					      MASK_SIGN));
    BID_RETURN (res);
  }
}
