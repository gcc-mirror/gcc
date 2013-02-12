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

#include "bid_internal.h"

/*****************************************************************************
 *  BID64_round_integral_exact
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_round_integral_exact (UINT64 * pres,
			    UINT64 *
			    px _RND_MODE_PARAM _EXC_FLAGS_PARAM
			    _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  UINT64 x = *px;
#if !DECIMAL_GLOBAL_ROUNDING
  unsigned int rnd_mode = *prnd_mode;
#endif
#else
UINT64
bid64_round_integral_exact (UINT64 x _RND_MODE_PARAM _EXC_FLAGS_PARAM
			    _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif

  UINT64 res = 0xbaddbaddbaddbaddull;
  UINT64 x_sign;
  int exp;			// unbiased exponent
  // Note: C1 represents the significand (UINT64)
  BID_UI64DOUBLE tmp1;
  int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  // UINT64 res is C* at first - represents up to 16 decimal digits <= 54 bits
  UINT128 fstar = { {0x0ull, 0x0ull} };
  UINT128 P128;

  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative

  // check for NaNs and infinities
  if ((x & MASK_NAN) == MASK_NAN) {	// check for NaN
    if ((x & 0x0003ffffffffffffull) > 999999999999999ull)
      x = x & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
    else
      x = x & 0xfe03ffffffffffffull;	// clear G6-G12
    if ((x & MASK_SNAN) == MASK_SNAN) {	// SNaN
      // set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return quiet (SNaN)
      res = x & 0xfdffffffffffffffull;
    } else {	// QNaN
      res = x;
    }
    BID_RETURN (res);
  } else if ((x & MASK_INF) == MASK_INF) {	// check for Infinity
    res = x_sign | 0x7800000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    // if the steering bits are 11 (condition will be 0), then 
    // the exponent is G[0:w+1]
    exp = ((x & MASK_BINARY_EXPONENT2) >> 51) - 398;
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      C1 = 0;
    }
  } else {	// if ((x & MASK_STEERING_BITS) != MASK_STEERING_BITS)
    exp = ((x & MASK_BINARY_EXPONENT1) >> 53) - 398;
    C1 = (x & MASK_BINARY_SIG1);
  }

  // if x is 0 or non-canonical return 0 preserving the sign bit and 
  // the preferred exponent of MAX(Q(x), 0)
  if (C1 == 0) {
    if (exp < 0)
      exp = 0;
    res = x_sign | (((UINT64) exp + 398) << 53);
    BID_RETURN (res);
  }
  // x is a finite non-zero number (not 0, non-canonical, or special)

  switch (rnd_mode) {
  case ROUNDING_TO_NEAREST:
  case ROUNDING_TIES_AWAY:
    // return 0 if (exp <= -(p+1))
    if (exp <= -17) {
      res = x_sign | 0x31c0000000000000ull;
      *pfpsf |= INEXACT_EXCEPTION;
      BID_RETURN (res);
    }
    break;
  case ROUNDING_DOWN:
    // return 0 if (exp <= -p)
    if (exp <= -16) {
      if (x_sign) {
	res = 0xb1c0000000000001ull;
      } else {
	res = 0x31c0000000000000ull;
      }
      *pfpsf |= INEXACT_EXCEPTION;
      BID_RETURN (res);
    }
    break;
  case ROUNDING_UP:
    // return 0 if (exp <= -p)
    if (exp <= -16) {
      if (x_sign) {
	res = 0xb1c0000000000000ull;
      } else {
	res = 0x31c0000000000001ull;
      }
      *pfpsf |= INEXACT_EXCEPTION;
      BID_RETURN (res);
    }
    break;
  case ROUNDING_TO_ZERO:
    // return 0 if (exp <= -p) 
    if (exp <= -16) {
      res = x_sign | 0x31c0000000000000ull;
      *pfpsf |= INEXACT_EXCEPTION;
      BID_RETURN (res);
    }
    break;
  }	// end switch ()

  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    q = 16;
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    q = nr_digits[x_nr_bits - 1].digits;
    if (q == 0) {
      q = nr_digits[x_nr_bits - 1].digits1;
      if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
	q++;
    }
  }

  if (exp >= 0) {	// -exp <= 0
    // the argument is an integer already
    res = x;
    BID_RETURN (res);
  }

  switch (rnd_mode) {
  case ROUNDING_TO_NEAREST:
    if ((q + exp) >= 0) {	// exp < 0 and 1 <= -exp <= q
      // need to shift right -exp digits from the coefficient; exp will be 0
      ind = -exp;	// 1 <= ind <= 16; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1 
      // C1 = C1 + 1/2 * 10^x where the result C1 fits in 64 bits
      // FOR ROUND_TO_NEAREST, WE ADD 1/2 ULP(y) then truncate
      C1 = C1 + midpoint64[ind - 1];
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 16
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = (C1 + 1/2 * 10^x) * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 64 bits
      __mul_64x64_to_128 (P128, C1, ten2mk64[ind - 1]);

      // if (0 < f* < 10^(-x)) then the result is a midpoint
      //   if floor(C*) is even then C* = floor(C*) - logical right
      //       shift; C* has p decimal digits, correct by Prop. 1)
      //   else if floor(C*) is odd C* = floor(C*)-1 (logical right
      //       shift; C* has p decimal digits, correct by Pr. 1)
      // else  
      //   C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // n = C* * 10^(e+x)  

      if (ind - 1 <= 2) {	// 0 <= ind - 1 <= 2 => shift = 0
	res = P128.w[1];
	fstar.w[1] = 0;
	fstar.w[0] = P128.w[0];
      } else if (ind - 1 <= 21) {	// 3 <= ind - 1 <= 21 => 3 <= shift <= 63
	shift = shiftright128[ind - 1];	// 3 <= shift <= 63
	res = (P128.w[1] >> shift);
	fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
	fstar.w[0] = P128.w[0];
      }
      // if (0 < f* < 10^(-x)) then the result is a midpoint
      // since round_to_even, subtract 1 if current result is odd
      if ((res & 0x0000000000000001ull) && (fstar.w[1] == 0)
	  && (fstar.w[0] < ten2mk64[ind - 1])) {
	res--;
      }
      // determine inexactness of the rounding of C*
      // if (0 < f* - 1/2 < 10^(-x)) then
      //   the result is exact
      // else // if (f* - 1/2 > T*) then
      //   the result is inexact
      if (ind - 1 <= 2) {
	if (fstar.w[0] > 0x8000000000000000ull) {
	  // f* > 1/2 and the result may be exact
	  // fstar.w[0] - 0x8000000000000000ull is f* - 1/2
	  if ((fstar.w[0] - 0x8000000000000000ull) > ten2mk64[ind - 1]) {
	    // set the inexact flag
	    *pfpsf |= INEXACT_EXCEPTION;
	  }	// else the result is exact
	} else {	// the result is inexact; f2* <= 1/2
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}
      } else {	// if 3 <= ind - 1 <= 21
	if (fstar.w[1] > onehalf128[ind - 1] ||
	    (fstar.w[1] == onehalf128[ind - 1] && fstar.w[0])) {
	  // f2* > 1/2 and the result may be exact
	  // Calculate f2* - 1/2
	  if (fstar.w[1] > onehalf128[ind - 1]
	      || fstar.w[0] > ten2mk64[ind - 1]) {
	    // set the inexact flag
	    *pfpsf |= INEXACT_EXCEPTION;
	  }	// else the result is exact
	} else {	// the result is inexact; f2* <= 1/2
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}
      }
      // set exponent to zero as it was negative before.
      res = x_sign | 0x31c0000000000000ull | res;
      BID_RETURN (res);
    } else {	// if exp < 0 and q + exp < 0
      // the result is +0 or -0
      res = x_sign | 0x31c0000000000000ull;
      *pfpsf |= INEXACT_EXCEPTION;
      BID_RETURN (res);
    }
    break;
  case ROUNDING_TIES_AWAY:
    if ((q + exp) >= 0) {	// exp < 0 and 1 <= -exp <= q
      // need to shift right -exp digits from the coefficient; exp will be 0
      ind = -exp;	// 1 <= ind <= 16; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1 
      // C1 = C1 + 1/2 * 10^x where the result C1 fits in 64 bits
      // FOR ROUND_TO_NEAREST, WE ADD 1/2 ULP(y) then truncate
      C1 = C1 + midpoint64[ind - 1];
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 16
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = (C1 + 1/2 * 10^x) * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 64 bits
      __mul_64x64_to_128 (P128, C1, ten2mk64[ind - 1]);

      // if (0 < f* < 10^(-x)) then the result is a midpoint
      //   C* = floor(C*) - logical right shift; C* has p decimal digits, 
      //       correct by Prop. 1)
      // else
      //   C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // n = C* * 10^(e+x)

      if (ind - 1 <= 2) {	// 0 <= ind - 1 <= 2 => shift = 0
	res = P128.w[1];
	fstar.w[1] = 0;
	fstar.w[0] = P128.w[0];
      } else if (ind - 1 <= 21) {	// 3 <= ind - 1 <= 21 => 3 <= shift <= 63
	shift = shiftright128[ind - 1];	// 3 <= shift <= 63
	res = (P128.w[1] >> shift);
	fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
	fstar.w[0] = P128.w[0];
      }
      // midpoints are already rounded correctly
      // determine inexactness of the rounding of C*
      // if (0 < f* - 1/2 < 10^(-x)) then
      //   the result is exact
      // else // if (f* - 1/2 > T*) then
      //   the result is inexact
      if (ind - 1 <= 2) {
	if (fstar.w[0] > 0x8000000000000000ull) {
	  // f* > 1/2 and the result may be exact 
	  // fstar.w[0] - 0x8000000000000000ull is f* - 1/2
	  if ((fstar.w[0] - 0x8000000000000000ull) > ten2mk64[ind - 1]) {
	    // set the inexact flag
	    *pfpsf |= INEXACT_EXCEPTION;
	  }	// else the result is exact
	} else {	// the result is inexact; f2* <= 1/2
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}
      } else {	// if 3 <= ind - 1 <= 21
	if (fstar.w[1] > onehalf128[ind - 1] ||
	    (fstar.w[1] == onehalf128[ind - 1] && fstar.w[0])) {
	  // f2* > 1/2 and the result may be exact
	  // Calculate f2* - 1/2
	  if (fstar.w[1] > onehalf128[ind - 1]
	      || fstar.w[0] > ten2mk64[ind - 1]) {
	    // set the inexact flag
	    *pfpsf |= INEXACT_EXCEPTION;
	  }	// else the result is exact
	} else {	// the result is inexact; f2* <= 1/2
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}
      }
      // set exponent to zero as it was negative before.
      res = x_sign | 0x31c0000000000000ull | res;
      BID_RETURN (res);
    } else {	// if exp < 0 and q + exp < 0
      // the result is +0 or -0
      res = x_sign | 0x31c0000000000000ull;
      *pfpsf |= INEXACT_EXCEPTION;
      BID_RETURN (res);
    }
    break;
  case ROUNDING_DOWN:
    if ((q + exp) > 0) {	// exp < 0 and 1 <= -exp < q
      // need to shift right -exp digits from the coefficient; exp will be 0
      ind = -exp;	// 1 <= ind <= 16; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1 
      // C1 fits in 64 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 16
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 64 bits
      __mul_64x64_to_128 (P128, C1, ten2mk64[ind - 1]);

      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // if (0 < f* < 10^(-x)) then the result is exact
      // n = C* * 10^(e+x)  

      if (ind - 1 <= 2) {	// 0 <= ind - 1 <= 2 => shift = 0
	res = P128.w[1];
	fstar.w[1] = 0;
	fstar.w[0] = P128.w[0];
      } else if (ind - 1 <= 21) {	// 3 <= ind - 1 <= 21 => 3 <= shift <= 63
	shift = shiftright128[ind - 1];	// 3 <= shift <= 63
	res = (P128.w[1] >> shift);
	fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
	fstar.w[0] = P128.w[0];
      }
      // if (f* > 10^(-x)) then the result is inexact
      if ((fstar.w[1] != 0) || (fstar.w[0] >= ten2mk64[ind - 1])) {
	if (x_sign) {
	  // if negative and not exact, increment magnitude
	  res++;
	}
	*pfpsf |= INEXACT_EXCEPTION;
      }
      // set exponent to zero as it was negative before.
      res = x_sign | 0x31c0000000000000ull | res;
      BID_RETURN (res);
    } else {	// if exp < 0 and q + exp <= 0
      // the result is +0 or -1
      if (x_sign) {
	res = 0xb1c0000000000001ull;
      } else {
	res = 0x31c0000000000000ull;
      }
      *pfpsf |= INEXACT_EXCEPTION;
      BID_RETURN (res);
    }
    break;
  case ROUNDING_UP:
    if ((q + exp) > 0) {	// exp < 0 and 1 <= -exp < q
      // need to shift right -exp digits from the coefficient; exp will be 0
      ind = -exp;	// 1 <= ind <= 16; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1 
      // C1 fits in 64 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 16
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 64 bits
      __mul_64x64_to_128 (P128, C1, ten2mk64[ind - 1]);

      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // if (0 < f* < 10^(-x)) then the result is exact
      // n = C* * 10^(e+x)  

      if (ind - 1 <= 2) {	// 0 <= ind - 1 <= 2 => shift = 0
	res = P128.w[1];
	fstar.w[1] = 0;
	fstar.w[0] = P128.w[0];
      } else if (ind - 1 <= 21) {	// 3 <= ind - 1 <= 21 => 3 <= shift <= 63
	shift = shiftright128[ind - 1];	// 3 <= shift <= 63
	res = (P128.w[1] >> shift);
	fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
	fstar.w[0] = P128.w[0];
      }
      // if (f* > 10^(-x)) then the result is inexact
      if ((fstar.w[1] != 0) || (fstar.w[0] >= ten2mk64[ind - 1])) {
	if (!x_sign) {
	  // if positive and not exact, increment magnitude
	  res++;
	}
	*pfpsf |= INEXACT_EXCEPTION;
      }
      // set exponent to zero as it was negative before.
      res = x_sign | 0x31c0000000000000ull | res;
      BID_RETURN (res);
    } else {	// if exp < 0 and q + exp <= 0
      // the result is -0 or +1
      if (x_sign) {
	res = 0xb1c0000000000000ull;
      } else {
	res = 0x31c0000000000001ull;
      }
      *pfpsf |= INEXACT_EXCEPTION;
      BID_RETURN (res);
    }
    break;
  case ROUNDING_TO_ZERO:
    if ((q + exp) >= 0) {	// exp < 0 and 1 <= -exp <= q
      // need to shift right -exp digits from the coefficient; exp will be 0
      ind = -exp;	// 1 <= ind <= 16; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1 
      // C1 fits in 127 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 16
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 64 bits
      __mul_64x64_to_128 (P128, C1, ten2mk64[ind - 1]);

      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // if (0 < f* < 10^(-x)) then the result is exact
      // n = C* * 10^(e+x)  

      if (ind - 1 <= 2) {	// 0 <= ind - 1 <= 2 => shift = 0
	res = P128.w[1];
	fstar.w[1] = 0;
	fstar.w[0] = P128.w[0];
      } else if (ind - 1 <= 21) {	// 3 <= ind - 1 <= 21 => 3 <= shift <= 63
	shift = shiftright128[ind - 1];	// 3 <= shift <= 63
	res = (P128.w[1] >> shift);
	fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
	fstar.w[0] = P128.w[0];
      }
      // if (f* > 10^(-x)) then the result is inexact
      if ((fstar.w[1] != 0) || (fstar.w[0] >= ten2mk64[ind - 1])) {
	*pfpsf |= INEXACT_EXCEPTION;
      }
      // set exponent to zero as it was negative before.
      res = x_sign | 0x31c0000000000000ull | res;
      BID_RETURN (res);
    } else {	// if exp < 0 and q + exp < 0
      // the result is +0 or -0
      res = x_sign | 0x31c0000000000000ull;
      *pfpsf |= INEXACT_EXCEPTION;
      BID_RETURN (res);
    }
    break;
  }	// end switch ()
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64_round_integral_nearest_even
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_round_integral_nearest_even (UINT64 * pres,
				   UINT64 *
				   px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				   _EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_round_integral_nearest_even (UINT64 x _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif

  UINT64 res = 0xbaddbaddbaddbaddull;
  UINT64 x_sign;
  int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
  BID_UI64DOUBLE tmp1;
  int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  UINT128 fstar;
  UINT128 P128;

  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative

  // check for NaNs and infinities
  if ((x & MASK_NAN) == MASK_NAN) {	// check for NaN
    if ((x & 0x0003ffffffffffffull) > 999999999999999ull)
      x = x & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
    else
      x = x & 0xfe03ffffffffffffull;	// clear G6-G12 
    if ((x & MASK_SNAN) == MASK_SNAN) {	// SNaN 
      // set invalid flag 
      *pfpsf |= INVALID_EXCEPTION;
      // return quiet (SNaN) 
      res = x & 0xfdffffffffffffffull;
    } else {	// QNaN
      res = x;
    }
    BID_RETURN (res);
  } else if ((x & MASK_INF) == MASK_INF) {	// check for Infinity
    res = x_sign | 0x7800000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    // if the steering bits are 11 (condition will be 0), then
    // the exponent is G[0:w+1]
    exp = ((x & MASK_BINARY_EXPONENT2) >> 51) - 398;
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      C1 = 0;
    }
  } else {	// if ((x & MASK_STEERING_BITS) != MASK_STEERING_BITS)
    exp = ((x & MASK_BINARY_EXPONENT1) >> 53) - 398;
    C1 = (x & MASK_BINARY_SIG1);
  }

  // if x is 0 or non-canonical
  if (C1 == 0) {
    if (exp < 0)
      exp = 0;
    res = x_sign | (((UINT64) exp + 398) << 53);
    BID_RETURN (res);
  }
  // x is a finite non-zero number (not 0, non-canonical, or special)

  // return 0 if (exp <= -(p+1))
  if (exp <= -17) {
    res = x_sign | 0x31c0000000000000ull;
    BID_RETURN (res);
  }
  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    q = 16;
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    q = nr_digits[x_nr_bits - 1].digits;
    if (q == 0) {
      q = nr_digits[x_nr_bits - 1].digits1;
      if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
	q++;
    }
  }

  if (exp >= 0) {	// -exp <= 0
    // the argument is an integer already
    res = x;
    BID_RETURN (res);
  } else if ((q + exp) >= 0) {	// exp < 0 and 1 <= -exp <= q
    // need to shift right -exp digits from the coefficient; the exp will be 0
    ind = -exp;	// 1 <= ind <= 16; ind is a synonym for 'x'
    // chop off ind digits from the lower part of C1 
    // C1 = C1 + 1/2 * 10^x where the result C1 fits in 64 bits
    // FOR ROUND_TO_NEAREST, WE ADD 1/2 ULP(y) then truncate
    C1 = C1 + midpoint64[ind - 1];
    // calculate C* and f*
    // C* is actually floor(C*) in this case
    // C* and f* need shifting and masking, as shown by
    // shiftright128[] and maskhigh128[]
    // 1 <= x <= 16
    // kx = 10^(-x) = ten2mk64[ind - 1]
    // C* = (C1 + 1/2 * 10^x) * 10^(-x)
    // the approximation of 10^(-x) was rounded up to 64 bits
    __mul_64x64_to_128 (P128, C1, ten2mk64[ind - 1]);

    // if (0 < f* < 10^(-x)) then the result is a midpoint
    //   if floor(C*) is even then C* = floor(C*) - logical right
    //       shift; C* has p decimal digits, correct by Prop. 1)
    //   else if floor(C*) is odd C* = floor(C*)-1 (logical right
    //       shift; C* has p decimal digits, correct by Pr. 1)
    // else  
    //   C* = floor(C*) (logical right shift; C has p decimal digits,
    //       correct by Property 1)
    // n = C* * 10^(e+x)  

    if (ind - 1 <= 2) {	// 0 <= ind - 1 <= 2 => shift = 0
      res = P128.w[1];
      fstar.w[1] = 0;
      fstar.w[0] = P128.w[0];
    } else if (ind - 1 <= 21) {	// 3 <= ind - 1 <= 21 => 3 <= shift <= 63
      shift = shiftright128[ind - 1];	// 3 <= shift <= 63
      res = (P128.w[1] >> shift);
      fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
      fstar.w[0] = P128.w[0];
    }
    // if (0 < f* < 10^(-x)) then the result is a midpoint
    // since round_to_even, subtract 1 if current result is odd
    if ((res & 0x0000000000000001ull) && (fstar.w[1] == 0)
	&& (fstar.w[0] < ten2mk64[ind - 1])) {
      res--;
    }
    // set exponent to zero as it was negative before.
    res = x_sign | 0x31c0000000000000ull | res;
    BID_RETURN (res);
  } else {	// if exp < 0 and q + exp < 0
    // the result is +0 or -0
    res = x_sign | 0x31c0000000000000ull;
    BID_RETURN (res);
  }
}

/*****************************************************************************
 *  BID64_round_integral_negative
 *****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_round_integral_negative (UINT64 * pres,
			       UINT64 *
			       px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			       _EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_round_integral_negative (UINT64 x _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif

  UINT64 res = 0xbaddbaddbaddbaddull;
  UINT64 x_sign;
  int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
  BID_UI64DOUBLE tmp1;
  int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  // UINT64 res is C* at first - represents up to 34 decimal digits ~ 113 bits
  UINT128 fstar;
  UINT128 P128;

  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative

  // check for NaNs and infinities
  if ((x & MASK_NAN) == MASK_NAN) {	// check for NaN
    if ((x & 0x0003ffffffffffffull) > 999999999999999ull)
      x = x & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
    else
      x = x & 0xfe03ffffffffffffull;	// clear G6-G12 
    if ((x & MASK_SNAN) == MASK_SNAN) {	// SNaN 
      // set invalid flag 
      *pfpsf |= INVALID_EXCEPTION;
      // return quiet (SNaN) 
      res = x & 0xfdffffffffffffffull;
    } else {	// QNaN
      res = x;
    }
    BID_RETURN (res);
  } else if ((x & MASK_INF) == MASK_INF) {	// check for Infinity
    res = x_sign | 0x7800000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    // if the steering bits are 11 (condition will be 0), then
    // the exponent is G[0:w+1]
    exp = ((x & MASK_BINARY_EXPONENT2) >> 51) - 398;
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      C1 = 0;
    }
  } else {	// if ((x & MASK_STEERING_BITS) != MASK_STEERING_BITS)
    exp = ((x & MASK_BINARY_EXPONENT1) >> 53) - 398;
    C1 = (x & MASK_BINARY_SIG1);
  }

  // if x is 0 or non-canonical
  if (C1 == 0) {
    if (exp < 0)
      exp = 0;
    res = x_sign | (((UINT64) exp + 398) << 53);
    BID_RETURN (res);
  }
  // x is a finite non-zero number (not 0, non-canonical, or special)

  // return 0 if (exp <= -p)
  if (exp <= -16) {
    if (x_sign) {
      res = 0xb1c0000000000001ull;
    } else {
      res = 0x31c0000000000000ull;
    }
    BID_RETURN (res);
  }
  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    q = 16;
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    q = nr_digits[x_nr_bits - 1].digits;
    if (q == 0) {
      q = nr_digits[x_nr_bits - 1].digits1;
      if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
	q++;
    }
  }

  if (exp >= 0) {	// -exp <= 0
    // the argument is an integer already
    res = x;
    BID_RETURN (res);
  } else if ((q + exp) > 0) {	// exp < 0 and 1 <= -exp < q
    // need to shift right -exp digits from the coefficient; the exp will be 0
    ind = -exp;	// 1 <= ind <= 16; ind is a synonym for 'x'
    // chop off ind digits from the lower part of C1 
    // C1 fits in 64 bits
    // calculate C* and f*
    // C* is actually floor(C*) in this case
    // C* and f* need shifting and masking, as shown by
    // shiftright128[] and maskhigh128[]
    // 1 <= x <= 16
    // kx = 10^(-x) = ten2mk64[ind - 1]
    // C* = C1 * 10^(-x)
    // the approximation of 10^(-x) was rounded up to 64 bits
    __mul_64x64_to_128 (P128, C1, ten2mk64[ind - 1]);

    // C* = floor(C*) (logical right shift; C has p decimal digits,
    //       correct by Property 1)
    // if (0 < f* < 10^(-x)) then the result is exact
    // n = C* * 10^(e+x)  

    if (ind - 1 <= 2) {	// 0 <= ind - 1 <= 2 => shift = 0
      res = P128.w[1];
      fstar.w[1] = 0;
      fstar.w[0] = P128.w[0];
    } else if (ind - 1 <= 21) {	// 3 <= ind - 1 <= 21 => 3 <= shift <= 63
      shift = shiftright128[ind - 1];	// 3 <= shift <= 63
      res = (P128.w[1] >> shift);
      fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
      fstar.w[0] = P128.w[0];
    }
    // if (f* > 10^(-x)) then the result is inexact
    if (x_sign
	&& ((fstar.w[1] != 0) || (fstar.w[0] >= ten2mk64[ind - 1]))) {
      // if negative and not exact, increment magnitude
      res++;
    }
    // set exponent to zero as it was negative before.
    res = x_sign | 0x31c0000000000000ull | res;
    BID_RETURN (res);
  } else {	// if exp < 0 and q + exp <= 0
    // the result is +0 or -1
    if (x_sign) {
      res = 0xb1c0000000000001ull;
    } else {
      res = 0x31c0000000000000ull;
    }
    BID_RETURN (res);
  }
}

/*****************************************************************************
 *  BID64_round_integral_positive
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_round_integral_positive (UINT64 * pres,
			       UINT64 *
			       px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			       _EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_round_integral_positive (UINT64 x _EXC_FLAGS_PARAM
			       _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif

  UINT64 res = 0xbaddbaddbaddbaddull;
  UINT64 x_sign;
  int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
  BID_UI64DOUBLE tmp1;
  int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  // UINT64 res is C* at first - represents up to 34 decimal digits ~ 113 bits
  UINT128 fstar;
  UINT128 P128;

  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative

  // check for NaNs and infinities
  if ((x & MASK_NAN) == MASK_NAN) {	// check for NaN
    if ((x & 0x0003ffffffffffffull) > 999999999999999ull)
      x = x & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
    else
      x = x & 0xfe03ffffffffffffull;	// clear G6-G12 
    if ((x & MASK_SNAN) == MASK_SNAN) {	// SNaN 
      // set invalid flag 
      *pfpsf |= INVALID_EXCEPTION;
      // return quiet (SNaN) 
      res = x & 0xfdffffffffffffffull;
    } else {	// QNaN
      res = x;
    }
    BID_RETURN (res);
  } else if ((x & MASK_INF) == MASK_INF) {	// check for Infinity
    res = x_sign | 0x7800000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    // if the steering bits are 11 (condition will be 0), then
    // the exponent is G[0:w+1]
    exp = ((x & MASK_BINARY_EXPONENT2) >> 51) - 398;
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      C1 = 0;
    }
  } else {	// if ((x & MASK_STEERING_BITS) != MASK_STEERING_BITS)
    exp = ((x & MASK_BINARY_EXPONENT1) >> 53) - 398;
    C1 = (x & MASK_BINARY_SIG1);
  }

  // if x is 0 or non-canonical
  if (C1 == 0) {
    if (exp < 0)
      exp = 0;
    res = x_sign | (((UINT64) exp + 398) << 53);
    BID_RETURN (res);
  }
  // x is a finite non-zero number (not 0, non-canonical, or special)

  // return 0 if (exp <= -p)
  if (exp <= -16) {
    if (x_sign) {
      res = 0xb1c0000000000000ull;
    } else {
      res = 0x31c0000000000001ull;
    }
    BID_RETURN (res);
  }
  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    q = 16;
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    q = nr_digits[x_nr_bits - 1].digits;
    if (q == 0) {
      q = nr_digits[x_nr_bits - 1].digits1;
      if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
	q++;
    }
  }

  if (exp >= 0) {	// -exp <= 0
    // the argument is an integer already
    res = x;
    BID_RETURN (res);
  } else if ((q + exp) > 0) {	// exp < 0 and 1 <= -exp < q
    // need to shift right -exp digits from the coefficient; the exp will be 0
    ind = -exp;	// 1 <= ind <= 16; ind is a synonym for 'x'
    // chop off ind digits from the lower part of C1 
    // C1 fits in 64 bits
    // calculate C* and f*
    // C* is actually floor(C*) in this case
    // C* and f* need shifting and masking, as shown by
    // shiftright128[] and maskhigh128[]
    // 1 <= x <= 16
    // kx = 10^(-x) = ten2mk64[ind - 1]
    // C* = C1 * 10^(-x)
    // the approximation of 10^(-x) was rounded up to 64 bits
    __mul_64x64_to_128 (P128, C1, ten2mk64[ind - 1]);

    // C* = floor(C*) (logical right shift; C has p decimal digits,
    //       correct by Property 1)
    // if (0 < f* < 10^(-x)) then the result is exact
    // n = C* * 10^(e+x)  

    if (ind - 1 <= 2) {	// 0 <= ind - 1 <= 2 => shift = 0
      res = P128.w[1];
      fstar.w[1] = 0;
      fstar.w[0] = P128.w[0];
    } else if (ind - 1 <= 21) {	// 3 <= ind - 1 <= 21 => 3 <= shift <= 63
      shift = shiftright128[ind - 1];	// 3 <= shift <= 63
      res = (P128.w[1] >> shift);
      fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
      fstar.w[0] = P128.w[0];
    }
    // if (f* > 10^(-x)) then the result is inexact
    if (!x_sign
	&& ((fstar.w[1] != 0) || (fstar.w[0] >= ten2mk64[ind - 1]))) {
      // if positive and not exact, increment magnitude
      res++;
    }
    // set exponent to zero as it was negative before.
    res = x_sign | 0x31c0000000000000ull | res;
    BID_RETURN (res);
  } else {	// if exp < 0 and q + exp <= 0
    // the result is -0 or +1
    if (x_sign) {
      res = 0xb1c0000000000000ull;
    } else {
      res = 0x31c0000000000001ull;
    }
    BID_RETURN (res);
  }
}

/*****************************************************************************
 *  BID64_round_integral_zero
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_round_integral_zero (UINT64 * pres,
			   UINT64 *
			   px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			   _EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_round_integral_zero (UINT64 x _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			   _EXC_INFO_PARAM) {
#endif

  UINT64 res = 0xbaddbaddbaddbaddull;
  UINT64 x_sign;
  int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
  BID_UI64DOUBLE tmp1;
  int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  // UINT64 res is C* at first - represents up to 34 decimal digits ~ 113 bits
  UINT128 P128;

  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative

  // check for NaNs and infinities
  if ((x & MASK_NAN) == MASK_NAN) {	// check for NaN
    if ((x & 0x0003ffffffffffffull) > 999999999999999ull)
      x = x & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
    else
      x = x & 0xfe03ffffffffffffull;	// clear G6-G12 
    if ((x & MASK_SNAN) == MASK_SNAN) {	// SNaN 
      // set invalid flag 
      *pfpsf |= INVALID_EXCEPTION;
      // return quiet (SNaN) 
      res = x & 0xfdffffffffffffffull;
    } else {	// QNaN
      res = x;
    }
    BID_RETURN (res);
  } else if ((x & MASK_INF) == MASK_INF) {	// check for Infinity
    res = x_sign | 0x7800000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    // if the steering bits are 11 (condition will be 0), then
    // the exponent is G[0:w+1]
    exp = ((x & MASK_BINARY_EXPONENT2) >> 51) - 398;
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      C1 = 0;
    }
  } else {	// if ((x & MASK_STEERING_BITS) != MASK_STEERING_BITS)
    exp = ((x & MASK_BINARY_EXPONENT1) >> 53) - 398;
    C1 = (x & MASK_BINARY_SIG1);
  }

  // if x is 0 or non-canonical
  if (C1 == 0) {
    if (exp < 0)
      exp = 0;
    res = x_sign | (((UINT64) exp + 398) << 53);
    BID_RETURN (res);
  }
  // x is a finite non-zero number (not 0, non-canonical, or special)

  // return 0 if (exp <= -p)
  if (exp <= -16) {
    res = x_sign | 0x31c0000000000000ull;
    BID_RETURN (res);
  }
  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    q = 16;
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    q = nr_digits[x_nr_bits - 1].digits;
    if (q == 0) {
      q = nr_digits[x_nr_bits - 1].digits1;
      if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
	q++;
    }
  }

  if (exp >= 0) {	// -exp <= 0
    // the argument is an integer already
    res = x;
    BID_RETURN (res);
  } else if ((q + exp) >= 0) {	// exp < 0 and 1 <= -exp <= q
    // need to shift right -exp digits from the coefficient; the exp will be 0
    ind = -exp;	// 1 <= ind <= 16; ind is a synonym for 'x'
    // chop off ind digits from the lower part of C1 
    // C1 fits in 127 bits
    // calculate C* and f*
    // C* is actually floor(C*) in this case
    // C* and f* need shifting and masking, as shown by
    // shiftright128[] and maskhigh128[]
    // 1 <= x <= 16
    // kx = 10^(-x) = ten2mk64[ind - 1]
    // C* = C1 * 10^(-x)
    // the approximation of 10^(-x) was rounded up to 64 bits
    __mul_64x64_to_128 (P128, C1, ten2mk64[ind - 1]);

    // C* = floor(C*) (logical right shift; C has p decimal digits,
    //       correct by Property 1)
    // if (0 < f* < 10^(-x)) then the result is exact
    // n = C* * 10^(e+x)  

    if (ind - 1 <= 2) {	// 0 <= ind - 1 <= 2 => shift = 0
      res = P128.w[1];
      // redundant fstar.w[1] = 0;
      // redundant fstar.w[0] = P128.w[0];
    } else if (ind - 1 <= 21) {	// 3 <= ind - 1 <= 21 => 3 <= shift <= 63
      shift = shiftright128[ind - 1];	// 3 <= shift <= 63
      res = (P128.w[1] >> shift);
      // redundant fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
      // redundant fstar.w[0] = P128.w[0];
    }
    // if (f* > 10^(-x)) then the result is inexact
    // if ((fstar.w[1] != 0) || (fstar.w[0] >= ten2mk64[ind-1])){
    //   // redundant
    // }
    // set exponent to zero as it was negative before.
    res = x_sign | 0x31c0000000000000ull | res;
    BID_RETURN (res);
  } else {	// if exp < 0 and q + exp < 0
    // the result is +0 or -0
    res = x_sign | 0x31c0000000000000ull;
    BID_RETURN (res);
  }
}

/*****************************************************************************
 *  BID64_round_integral_nearest_away
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_round_integral_nearest_away (UINT64 * pres,
				   UINT64 *
				   px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
				   _EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_round_integral_nearest_away (UINT64 x _EXC_FLAGS_PARAM
				   _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif

  UINT64 res = 0xbaddbaddbaddbaddull;
  UINT64 x_sign;
  int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
  BID_UI64DOUBLE tmp1;
  int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  UINT128 P128;

  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative

  // check for NaNs and infinities
  if ((x & MASK_NAN) == MASK_NAN) {	// check for NaN
    if ((x & 0x0003ffffffffffffull) > 999999999999999ull)
      x = x & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
    else
      x = x & 0xfe03ffffffffffffull;	// clear G6-G12 
    if ((x & MASK_SNAN) == MASK_SNAN) {	// SNaN 
      // set invalid flag 
      *pfpsf |= INVALID_EXCEPTION;
      // return quiet (SNaN) 
      res = x & 0xfdffffffffffffffull;
    } else {	// QNaN
      res = x;
    }
    BID_RETURN (res);
  } else if ((x & MASK_INF) == MASK_INF) {	// check for Infinity
    res = x_sign | 0x7800000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    // if the steering bits are 11 (condition will be 0), then
    // the exponent is G[0:w+1]
    exp = ((x & MASK_BINARY_EXPONENT2) >> 51) - 398;
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      C1 = 0;
    }
  } else {	// if ((x & MASK_STEERING_BITS) != MASK_STEERING_BITS)
    exp = ((x & MASK_BINARY_EXPONENT1) >> 53) - 398;
    C1 = (x & MASK_BINARY_SIG1);
  }

  // if x is 0 or non-canonical
  if (C1 == 0) {
    if (exp < 0)
      exp = 0;
    res = x_sign | (((UINT64) exp + 398) << 53);
    BID_RETURN (res);
  }
  // x is a finite non-zero number (not 0, non-canonical, or special)

  // return 0 if (exp <= -(p+1))
  if (exp <= -17) {
    res = x_sign | 0x31c0000000000000ull;
    BID_RETURN (res);
  }
  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    q = 16;
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    q = nr_digits[x_nr_bits - 1].digits;
    if (q == 0) {
      q = nr_digits[x_nr_bits - 1].digits1;
      if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
	q++;
    }
  }

  if (exp >= 0) {	// -exp <= 0
    // the argument is an integer already
    res = x;
    BID_RETURN (res);
  } else if ((q + exp) >= 0) {	// exp < 0 and 1 <= -exp <= q
    // need to shift right -exp digits from the coefficient; the exp will be 0
    ind = -exp;	// 1 <= ind <= 16; ind is a synonym for 'x'
    // chop off ind digits from the lower part of C1 
    // C1 = C1 + 1/2 * 10^x where the result C1 fits in 64 bits
    // FOR ROUND_TO_NEAREST, WE ADD 1/2 ULP(y) then truncate
    C1 = C1 + midpoint64[ind - 1];
    // calculate C* and f*
    // C* is actually floor(C*) in this case
    // C* and f* need shifting and masking, as shown by
    // shiftright128[] and maskhigh128[]
    // 1 <= x <= 16
    // kx = 10^(-x) = ten2mk64[ind - 1]
    // C* = (C1 + 1/2 * 10^x) * 10^(-x)
    // the approximation of 10^(-x) was rounded up to 64 bits
    __mul_64x64_to_128 (P128, C1, ten2mk64[ind - 1]);

    // if (0 < f* < 10^(-x)) then the result is a midpoint
    //   C* = floor(C*) - logical right shift; C* has p decimal digits, 
    //       correct by Prop. 1)
    // else
    //   C* = floor(C*) (logical right shift; C has p decimal digits,
    //       correct by Property 1)
    // n = C* * 10^(e+x)

    if (ind - 1 <= 2) {	// 0 <= ind - 1 <= 2 => shift = 0
      res = P128.w[1];
    } else if (ind - 1 <= 21) {	// 3 <= ind - 1 <= 21 => 3 <= shift <= 63
      shift = shiftright128[ind - 1];	// 3 <= shift <= 63
      res = (P128.w[1] >> shift);
    }
    // midpoints are already rounded correctly
    // set exponent to zero as it was negative before.
    res = x_sign | 0x31c0000000000000ull | res;
    BID_RETURN (res);
  } else {	// if exp < 0 and q + exp < 0
    // the result is +0 or -0
    res = x_sign | 0x31c0000000000000ull;
    BID_RETURN (res);
  }
}
