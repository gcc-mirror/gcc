/* Copyright (C) 2007-2023 Free Software Foundation, Inc.

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
 *  BID64 minimum function - returns greater of two numbers
 *****************************************************************************/

static const UINT64 mult_factor[16] = {
  1ull, 10ull, 100ull, 1000ull,
  10000ull, 100000ull, 1000000ull, 10000000ull,
  100000000ull, 1000000000ull, 10000000000ull, 100000000000ull,
  1000000000000ull, 10000000000000ull,
  100000000000000ull, 1000000000000000ull
};

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_minnum (UINT64 * pres, UINT64 * px, UINT64 * py _EXC_FLAGS_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
UINT64
bid64_minnum (UINT64 x, UINT64 y _EXC_FLAGS_PARAM) {
#endif

  UINT64 res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0;

  // check for non-canonical x
  if ((x & MASK_NAN) == MASK_NAN) {	// x is NaN
    x = x & 0xfe03ffffffffffffull;	// clear G6-G12
    if ((x & 0x0003ffffffffffffull) > 999999999999999ull) {
      x = x & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
    }
  } else if ((x & MASK_INF) == MASK_INF) {	// check for Infinity
    x = x & (MASK_SIGN | MASK_INF);
  } else {	// x is not special
    // check for non-canonical values - treated as zero
    if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
      // if the steering bits are 11, then the exponent is G[0:w+1]
      if (((x & MASK_BINARY_SIG2) | MASK_BINARY_OR2) >
	  9999999999999999ull) {
	// non-canonical
	x = (x & MASK_SIGN) | ((x & MASK_BINARY_EXPONENT2) << 2);
      }	// else canonical
    }	// else canonical 
  }

  // check for non-canonical y
  if ((y & MASK_NAN) == MASK_NAN) {	// y is NaN
    y = y & 0xfe03ffffffffffffull;	// clear G6-G12
    if ((y & 0x0003ffffffffffffull) > 999999999999999ull) {
      y = y & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
    }
  } else if ((y & MASK_INF) == MASK_INF) {	// check for Infinity
    y = y & (MASK_SIGN | MASK_INF);
  } else {	// y is not special
    // check for non-canonical values - treated as zero
    if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
      // if the steering bits are 11, then the exponent is G[0:w+1]
      if (((y & MASK_BINARY_SIG2) | MASK_BINARY_OR2) >
	  9999999999999999ull) {
	// non-canonical
	y = (y & MASK_SIGN) | ((y & MASK_BINARY_EXPONENT2) << 2);
      }	// else canonical
    }	// else canonical
  }

  // NaN (CASE1)
  if ((x & MASK_NAN) == MASK_NAN) {	// x is NAN
    if ((x & MASK_SNAN) == MASK_SNAN) {	// x is SNaN
      // if x is SNAN, then return quiet (x)
      *pfpsf |= INVALID_EXCEPTION;	// set exception if SNaN
      x = x & 0xfdffffffffffffffull;	// quietize x
      res = x;
    } else {	// x is QNaN
      if ((y & MASK_NAN) == MASK_NAN) {	// y is NAN
	if ((y & MASK_SNAN) == MASK_SNAN) {	// y is SNAN
	  *pfpsf |= INVALID_EXCEPTION;	// set invalid flag
	}
	res = x;
      } else {
	res = y;
      }
    }
    BID_RETURN (res);
  } else if ((y & MASK_NAN) == MASK_NAN) {	// y is NaN, but x is not
    if ((y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if SNaN
      y = y & 0xfdffffffffffffffull;	// quietize y
      res = y;
    } else {
      // will return x (which is not NaN)
      res = x;
    }
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal, return either number
  if (x == y) {
    res = x;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x is neg infinity, there is no way it is greater than y, return x
    if (((x & MASK_SIGN) == MASK_SIGN)) {
      res = x;
      BID_RETURN (res);
    }
    // x is pos infinity, return y
    else {
      res = y;
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so if y is positive infinity, then x is less, return y
    //                 if y is negative infinity, then x is greater, return x
    res = ((y & MASK_SIGN) == MASK_SIGN) ? y : x;
    BID_RETURN (res);
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
  }

  // ZERO (CASE4)
  // some properties:
  //    (+ZERO == -ZERO) => therefore 
  //        ignore the sign, and neither number is greater
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //        ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (sig_x == 0) {
    x_is_zero = 1;
  }
  if (sig_y == 0) {
    y_is_zero = 1;
  }

  if (x_is_zero && y_is_zero) {
    // if both numbers are zero, neither is greater => return either
    res = y;
    BID_RETURN (res);
  } else if (x_is_zero) {
    // is x is zero, it is greater if Y is negative
    res = ((y & MASK_SIGN) == MASK_SIGN) ? y : x;
    BID_RETURN (res);
  } else if (y_is_zero) {
    // is y is zero, X is greater if it is positive
    res = ((x & MASK_SIGN) != MASK_SIGN) ? y : x;;
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is greater if y is negative
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) == MASK_SIGN) ? y : x;
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)

  // if both components are either bigger or smaller, 
  // it is clear what needs to be done
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN) ? y : x;
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN) ? y : x;
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN) ? y : x;	// difference cannot be >10^15
    BID_RETURN (res);
  }
  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN) ? y : x;
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
      res = y;
      BID_RETURN (res);
    }

    res = (((sig_n_prime.w[1] > 0)
	    || sig_n_prime.w[0] > sig_y) ^ ((x & MASK_SIGN) ==
					    MASK_SIGN)) ? y : x;
    BID_RETURN (res);
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);

  // if postitive, return whichever significand is larger (converse if negative)
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = y;
    BID_RETURN (res);
  }
  res = (((sig_n_prime.w[1] == 0)
	  && (sig_x > sig_n_prime.w[0])) ^ ((x & MASK_SIGN) ==
					    MASK_SIGN)) ? y : x;
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64 minimum magnitude function - returns greater of two numbers
 *****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_minnum_mag (UINT64 * pres, UINT64 * px,
		  UINT64 * py _EXC_FLAGS_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
UINT64
bid64_minnum_mag (UINT64 x, UINT64 y _EXC_FLAGS_PARAM) {
#endif

  UINT64 res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;

  // check for non-canonical x
  if ((x & MASK_NAN) == MASK_NAN) {	// x is NaN
    x = x & 0xfe03ffffffffffffull;	// clear G6-G12
    if ((x & 0x0003ffffffffffffull) > 999999999999999ull) {
      x = x & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
    }
  } else if ((x & MASK_INF) == MASK_INF) {	// check for Infinity
    x = x & (MASK_SIGN | MASK_INF);
  } else {	// x is not special
    // check for non-canonical values - treated as zero
    if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
      // if the steering bits are 11, then the exponent is G[0:w+1]
      if (((x & MASK_BINARY_SIG2) | MASK_BINARY_OR2) >
	  9999999999999999ull) {
	// non-canonical
	x = (x & MASK_SIGN) | ((x & MASK_BINARY_EXPONENT2) << 2);
      }	// else canonical
    }	// else canonical 
  }

  // check for non-canonical y
  if ((y & MASK_NAN) == MASK_NAN) {	// y is NaN
    y = y & 0xfe03ffffffffffffull;	// clear G6-G12
    if ((y & 0x0003ffffffffffffull) > 999999999999999ull) {
      y = y & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
    }
  } else if ((y & MASK_INF) == MASK_INF) {	// check for Infinity
    y = y & (MASK_SIGN | MASK_INF);
  } else {	// y is not special
    // check for non-canonical values - treated as zero
    if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
      // if the steering bits are 11, then the exponent is G[0:w+1]
      if (((y & MASK_BINARY_SIG2) | MASK_BINARY_OR2) >
	  9999999999999999ull) {
	// non-canonical
	y = (y & MASK_SIGN) | ((y & MASK_BINARY_EXPONENT2) << 2);
      }	// else canonical
    }	// else canonical
  }

  // NaN (CASE1)
  if ((x & MASK_NAN) == MASK_NAN) {	// x is NAN
    if ((x & MASK_SNAN) == MASK_SNAN) {	// x is SNaN
      // if x is SNAN, then return quiet (x)
      *pfpsf |= INVALID_EXCEPTION;	// set exception if SNaN
      x = x & 0xfdffffffffffffffull;	// quietize x
      res = x;
    } else {	// x is QNaN
      if ((y & MASK_NAN) == MASK_NAN) {	// y is NAN
	if ((y & MASK_SNAN) == MASK_SNAN) {	// y is SNAN
	  *pfpsf |= INVALID_EXCEPTION;	// set invalid flag
	}
	res = x;
      } else {
	res = y;
      }
    }
    BID_RETURN (res);
  } else if ((y & MASK_NAN) == MASK_NAN) {	// y is NaN, but x is not
    if ((y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if SNaN
      y = y & 0xfdffffffffffffffull;	// quietize y
      res = y;
    } else {
      // will return x (which is not NaN)
      res = x;
    }
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal, return either number
  if (x == y) {
    res = x;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // x is infinity, its magnitude is greater than or equal to y
    // return x only if y is infinity and x is negative
    res = ((x & MASK_SIGN) == MASK_SIGN
	   && (y & MASK_INF) == MASK_INF) ? x : y;
    BID_RETURN (res);
  } else if ((y & MASK_INF) == MASK_INF) {
    // y is infinity, then it must be greater in magnitude, return x
    res = x;
    BID_RETURN (res);
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
  }

  // ZERO (CASE4)
  // some properties:
  //    (+ZERO == -ZERO) => therefore 
  //        ignore the sign, and neither number is greater
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //        ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (sig_x == 0) {
    res = x;	// x_is_zero, its magnitude must be smaller than y
    BID_RETURN (res);
  }
  if (sig_y == 0) {
    res = y;	// y_is_zero, its magnitude must be smaller than x
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller, 
  // it is clear what needs to be done
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = y;
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = x;
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = y;	// difference cannot be greater than 10^15
    BID_RETURN (res);
  }
  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = x;
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,
    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);
    // now, sig_n_prime has: sig_x * 10^(exp_x-exp_y), this is 
    // the compensated signif.
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      // two numbers are equal, return minNum(x,y)
      res = ((y & MASK_SIGN) == MASK_SIGN) ? y : x;
      BID_RETURN (res);
    }
    // now, if compensated_x (sig_n_prime) is greater than y, return y,  
    // otherwise return x
    res = ((sig_n_prime.w[1] != 0) || sig_n_prime.w[0] > sig_y) ? y : x;
    BID_RETURN (res);
  }
  // exp_y must be greater than exp_x, thus adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);

  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = ((y & MASK_SIGN) == MASK_SIGN) ? y : x;
    // two numbers are equal, return either
    BID_RETURN (res);
  }

  res = ((sig_n_prime.w[1] == 0) && (sig_x > sig_n_prime.w[0])) ? y : x;
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64 maximum function - returns greater of two numbers
 *****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_maxnum (UINT64 * pres, UINT64 * px, UINT64 * py _EXC_FLAGS_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
UINT64
bid64_maxnum (UINT64 x, UINT64 y _EXC_FLAGS_PARAM) {
#endif

  UINT64 res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;
  char x_is_zero = 0, y_is_zero = 0;

  // check for non-canonical x
  if ((x & MASK_NAN) == MASK_NAN) {	// x is NaN
    x = x & 0xfe03ffffffffffffull;	// clear G6-G12
    if ((x & 0x0003ffffffffffffull) > 999999999999999ull) {
      x = x & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
    }
  } else if ((x & MASK_INF) == MASK_INF) {	// check for Infinity
    x = x & (MASK_SIGN | MASK_INF);
  } else {	// x is not special
    // check for non-canonical values - treated as zero
    if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
      // if the steering bits are 11, then the exponent is G[0:w+1]
      if (((x & MASK_BINARY_SIG2) | MASK_BINARY_OR2) >
	  9999999999999999ull) {
	// non-canonical
	x = (x & MASK_SIGN) | ((x & MASK_BINARY_EXPONENT2) << 2);
      }	// else canonical
    }	// else canonical 
  }

  // check for non-canonical y
  if ((y & MASK_NAN) == MASK_NAN) {	// y is NaN
    y = y & 0xfe03ffffffffffffull;	// clear G6-G12
    if ((y & 0x0003ffffffffffffull) > 999999999999999ull) {
      y = y & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
    }
  } else if ((y & MASK_INF) == MASK_INF) {	// check for Infinity
    y = y & (MASK_SIGN | MASK_INF);
  } else {	// y is not special
    // check for non-canonical values - treated as zero
    if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
      // if the steering bits are 11, then the exponent is G[0:w+1]
      if (((y & MASK_BINARY_SIG2) | MASK_BINARY_OR2) >
	  9999999999999999ull) {
	// non-canonical
	y = (y & MASK_SIGN) | ((y & MASK_BINARY_EXPONENT2) << 2);
      }	// else canonical
    }	// else canonical
  }

  // NaN (CASE1)
  if ((x & MASK_NAN) == MASK_NAN) {	// x is NAN
    if ((x & MASK_SNAN) == MASK_SNAN) {	// x is SNaN
      // if x is SNAN, then return quiet (x)
      *pfpsf |= INVALID_EXCEPTION;	// set exception if SNaN
      x = x & 0xfdffffffffffffffull;	// quietize x
      res = x;
    } else {	// x is QNaN
      if ((y & MASK_NAN) == MASK_NAN) {	// y is NAN
	if ((y & MASK_SNAN) == MASK_SNAN) {	// y is SNAN
	  *pfpsf |= INVALID_EXCEPTION;	// set invalid flag
	}
	res = x;
      } else {
	res = y;
      }
    }
    BID_RETURN (res);
  } else if ((y & MASK_NAN) == MASK_NAN) {	// y is NaN, but x is not
    if ((y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if SNaN
      y = y & 0xfdffffffffffffffull;	// quietize y
      res = y;
    } else {
      // will return x (which is not NaN)
      res = x;
    }
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal (not Greater).
  if (x == y) {
    res = x;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // if x is neg infinity, there is no way it is greater than y, return y
    // x is pos infinity, it is greater, unless y is positive infinity => 
    // return y!=pos_infinity
    if (((x & MASK_SIGN) == MASK_SIGN)) {
      res = y;
      BID_RETURN (res);
    } else {
      res = (((y & MASK_INF) != MASK_INF)
	     || ((y & MASK_SIGN) == MASK_SIGN)) ? x : y;
      BID_RETURN (res);
    }
  } else if ((y & MASK_INF) == MASK_INF) {
    // x is finite, so if y is positive infinity, then x is less, return y
    //                 if y is negative infinity, then x is greater, return x
    res = ((y & MASK_SIGN) == MASK_SIGN) ? x : y;
    BID_RETURN (res);
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
  }

  // ZERO (CASE4)
  // some properties:
  //    (+ZERO == -ZERO) => therefore 
  //        ignore the sign, and neither number is greater
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //        ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (sig_x == 0) {
    x_is_zero = 1;
  }
  if (sig_y == 0) {
    y_is_zero = 1;
  }

  if (x_is_zero && y_is_zero) {
    // if both numbers are zero, neither is greater => return NOTGREATERTHAN
    res = y;
    BID_RETURN (res);
  } else if (x_is_zero) {
    // is x is zero, it is greater if Y is negative
    res = ((y & MASK_SIGN) == MASK_SIGN) ? x : y;
    BID_RETURN (res);
  } else if (y_is_zero) {
    // is y is zero, X is greater if it is positive
    res = ((x & MASK_SIGN) != MASK_SIGN) ? x : y;;
    BID_RETURN (res);
  }
  // OPPOSITE SIGN (CASE5)
  // now, if the sign bits differ, x is greater if y is negative
  if (((x ^ y) & MASK_SIGN) == MASK_SIGN) {
    res = ((y & MASK_SIGN) == MASK_SIGN) ? x : y;
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)

  // if both components are either bigger or smaller, 
  //     it is clear what needs to be done
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = ((x & MASK_SIGN) != MASK_SIGN) ? x : y;
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = ((x & MASK_SIGN) == MASK_SIGN) ? x : y;
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = ((x & MASK_SIGN) != MASK_SIGN) ? x : y;
    // difference cannot be > 10^15
    BID_RETURN (res);
  }
  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = ((x & MASK_SIGN) == MASK_SIGN) ? x : y;
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
      res = y;
      BID_RETURN (res);
    }
    res = (((sig_n_prime.w[1] > 0)
	    || sig_n_prime.w[0] > sig_y) ^ ((x & MASK_SIGN) ==
					    MASK_SIGN)) ? x : y;
    BID_RETURN (res);
  }
  // adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);

  // if postitive, return whichever significand is larger (converse if negative)
  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = y;
    BID_RETURN (res);
  }
  res = (((sig_n_prime.w[1] == 0)
	  && (sig_x > sig_n_prime.w[0])) ^ ((x & MASK_SIGN) ==
					    MASK_SIGN)) ? x : y;
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64 maximum magnitude function - returns greater of two numbers
 *****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_maxnum_mag (UINT64 * pres, UINT64 * px,
		  UINT64 * py _EXC_FLAGS_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
UINT64
bid64_maxnum_mag (UINT64 x, UINT64 y _EXC_FLAGS_PARAM) {
#endif

  UINT64 res;
  int exp_x, exp_y;
  UINT64 sig_x, sig_y;
  UINT128 sig_n_prime;

  // check for non-canonical x
  if ((x & MASK_NAN) == MASK_NAN) {	// x is NaN
    x = x & 0xfe03ffffffffffffull;	// clear G6-G12
    if ((x & 0x0003ffffffffffffull) > 999999999999999ull) {
      x = x & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
    }
  } else if ((x & MASK_INF) == MASK_INF) {	// check for Infinity
    x = x & (MASK_SIGN | MASK_INF);
  } else {	// x is not special
    // check for non-canonical values - treated as zero
    if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
      // if the steering bits are 11, then the exponent is G[0:w+1]
      if (((x & MASK_BINARY_SIG2) | MASK_BINARY_OR2) >
	  9999999999999999ull) {
	// non-canonical
	x = (x & MASK_SIGN) | ((x & MASK_BINARY_EXPONENT2) << 2);
      }	// else canonical
    }	// else canonical 
  }

  // check for non-canonical y
  if ((y & MASK_NAN) == MASK_NAN) {	// y is NaN
    y = y & 0xfe03ffffffffffffull;	// clear G6-G12
    if ((y & 0x0003ffffffffffffull) > 999999999999999ull) {
      y = y & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
    }
  } else if ((y & MASK_INF) == MASK_INF) {	// check for Infinity
    y = y & (MASK_SIGN | MASK_INF);
  } else {	// y is not special
    // check for non-canonical values - treated as zero
    if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
      // if the steering bits are 11, then the exponent is G[0:w+1]
      if (((y & MASK_BINARY_SIG2) | MASK_BINARY_OR2) >
	  9999999999999999ull) {
	// non-canonical
	y = (y & MASK_SIGN) | ((y & MASK_BINARY_EXPONENT2) << 2);
      }	// else canonical
    }	// else canonical
  }

  // NaN (CASE1)
  if ((x & MASK_NAN) == MASK_NAN) {	// x is NAN
    if ((x & MASK_SNAN) == MASK_SNAN) {	// x is SNaN
      // if x is SNAN, then return quiet (x)
      *pfpsf |= INVALID_EXCEPTION;	// set exception if SNaN
      x = x & 0xfdffffffffffffffull;	// quietize x
      res = x;
    } else {	// x is QNaN
      if ((y & MASK_NAN) == MASK_NAN) {	// y is NAN
	if ((y & MASK_SNAN) == MASK_SNAN) {	// y is SNAN
	  *pfpsf |= INVALID_EXCEPTION;	// set invalid flag
	}
	res = x;
      } else {
	res = y;
      }
    }
    BID_RETURN (res);
  } else if ((y & MASK_NAN) == MASK_NAN) {	// y is NaN, but x is not
    if ((y & MASK_SNAN) == MASK_SNAN) {
      *pfpsf |= INVALID_EXCEPTION;	// set exception if SNaN
      y = y & 0xfdffffffffffffffull;	// quietize y
      res = y;
    } else {
      // will return x (which is not NaN)
      res = x;
    }
    BID_RETURN (res);
  }
  // SIMPLE (CASE2)
  // if all the bits are the same, these numbers are equal, return either number
  if (x == y) {
    res = x;
    BID_RETURN (res);
  }
  // INFINITY (CASE3)
  if ((x & MASK_INF) == MASK_INF) {
    // x is infinity, its magnitude is greater than or equal to y
    // return y as long as x isn't negative infinity
    res = ((x & MASK_SIGN) == MASK_SIGN
	   && (y & MASK_INF) == MASK_INF) ? y : x;
    BID_RETURN (res);
  } else if ((y & MASK_INF) == MASK_INF) {
    // y is infinity, then it must be greater in magnitude
    res = y;
    BID_RETURN (res);
  }
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_x = (x & MASK_BINARY_EXPONENT2) >> 51;
    sig_x = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
  } else {
    exp_x = (x & MASK_BINARY_EXPONENT1) >> 53;
    sig_x = (x & MASK_BINARY_SIG1);
  }

  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((y & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    exp_y = (y & MASK_BINARY_EXPONENT2) >> 51;
    sig_y = (y & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
  } else {
    exp_y = (y & MASK_BINARY_EXPONENT1) >> 53;
    sig_y = (y & MASK_BINARY_SIG1);
  }

  // ZERO (CASE4)
  // some properties:
  //    (+ZERO == -ZERO) => therefore 
  //        ignore the sign, and neither number is greater
  //    (ZERO x 10^A == ZERO x 10^B) for any valid A, B => 
  //        ignore the exponent field
  //    (Any non-canonical # is considered 0)
  if (sig_x == 0) {
    res = y;	// x_is_zero, its magnitude must be smaller than y
    BID_RETURN (res);
  }
  if (sig_y == 0) {
    res = x;	// y_is_zero, its magnitude must be smaller than x
    BID_RETURN (res);
  }
  // REDUNDANT REPRESENTATIONS (CASE6)
  // if both components are either bigger or smaller, 
  // it is clear what needs to be done
  if (sig_x > sig_y && exp_x >= exp_y) {
    res = x;
    BID_RETURN (res);
  }
  if (sig_x < sig_y && exp_x <= exp_y) {
    res = y;
    BID_RETURN (res);
  }
  // if exp_x is 15 greater than exp_y, no need for compensation
  if (exp_x - exp_y > 15) {
    res = x;	// difference cannot be greater than 10^15
    BID_RETURN (res);
  }
  // if exp_x is 15 less than exp_y, no need for compensation
  if (exp_y - exp_x > 15) {
    res = y;
    BID_RETURN (res);
  }
  // if |exp_x - exp_y| < 15, it comes down to the compensated significand
  if (exp_x > exp_y) {	// to simplify the loop below,
    // otherwise adjust the x significand upwards
    __mul_64x64_to_128MACH (sig_n_prime, sig_x,
			    mult_factor[exp_x - exp_y]);
    // now, sig_n_prime has: sig_x * 10^(exp_x-exp_y), 
    // this is the compensated signif.
    if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_y)) {
      // two numbers are equal, return maxNum(x,y)
      res = ((y & MASK_SIGN) == MASK_SIGN) ? x : y;
      BID_RETURN (res);
    }
    // now, if compensated_x (sig_n_prime) is greater than y return y,  
    // otherwise return x
    res = ((sig_n_prime.w[1] != 0) || sig_n_prime.w[0] > sig_y) ? x : y;
    BID_RETURN (res);
  }
  // exp_y must be greater than exp_x, thus adjust the y significand upwards
  __mul_64x64_to_128MACH (sig_n_prime, sig_y,
			  mult_factor[exp_y - exp_x]);

  if (sig_n_prime.w[1] == 0 && (sig_n_prime.w[0] == sig_x)) {
    res = ((y & MASK_SIGN) == MASK_SIGN) ? x : y;
    // two numbers are equal, return either
    BID_RETURN (res);
  }

  res = ((sig_n_prime.w[1] == 0) && (sig_x > sig_n_prime.w[0])) ? x : y;
  BID_RETURN (res);
}
