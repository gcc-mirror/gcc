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

#include "bid_internal.h"

/*****************************************************************************
 *  BID64_to_uint64_rnint
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_to_uint64_rnint (UINT64 * pres, UINT64 * px
		       _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		       _EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_to_uint64_rnint (UINT64 x
		       _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		       _EXC_INFO_PARAM) {
#endif
  UINT64 res;
  UINT64 x_sign;
  UINT64 x_exp;
  int exp;			// unbiased exponent
  // Note: C1 represents x_significand (UINT64)
  BID_UI64DOUBLE tmp1;
  unsigned int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  UINT128 C;
  UINT64 Cstar;			// C* represents up to 16 decimal digits ~ 54 bits
  UINT128 fstar;
  UINT128 P128;

  // check for NaN or Infinity
  if ((x & MASK_NAN) == MASK_NAN || (x & MASK_INF) == MASK_INF) {
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    x_exp = (x & MASK_BINARY_EXPONENT2) >> 51;	// biased
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      x_exp = 0;
      C1 = 0;
    }
  } else {
    x_exp = (x & MASK_BINARY_EXPONENT1) >> 53;	// biased
    C1 = x & MASK_BINARY_SIG1;
  }

  // check for zeros (possibly from non-canonical values)
  if (C1 == 0x0ull) {
    // x is 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  }
  // x is not special and is not zero

  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    // split the 64-bit value in two 32-bit halves to avoid rounding errors
    if (C1 >= 0x0000000100000000ull) {	// x >= 2^32
      tmp1.d = (double) (C1 >> 32);	// exact conversion
      x_nr_bits =
	33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    } else {	// x < 2^32
      tmp1.d = (double) C1;	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
      q++;
  }
  exp = x_exp - 398;	// unbiased exponent

  if ((q + exp) > 20) {	// x >= 10^20 ~= 2^66.45... (cannot fit in 64 bits)
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  } else if ((q + exp) == 20) {	// x = c(0)c(1)...c(19).c(20)...c(q-1)
    // in this case 2^63.11... ~= 10^19 <= x < 10^20 ~= 2^66.43...
    // so x rounded to an integer may or may not fit in an unsigned 64-bit int
    // the cases that do not fit are identified here; the ones that fit
    // fall through and will be handled with other cases further,
    // under '1 <= q + exp <= 20'
    if (x_sign) {	// if n < 0 and q + exp = 20 then x is much less than -1/2
      // => set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    } else {	// if n > 0 and q + exp = 20
      // if n >= 2^64 - 1/2 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*(2^65-1)
      // <=> C * 10^(21-q) >= 0x9fffffffffffffffb, 1<=q<=16
      if (q == 1) {
	// C * 10^20 >= 0x9fffffffffffffffb
	__mul_128x64_to_128 (C, C1, ten2k128[0]);	// 10^20 * C
	if (C.w[1] > 0x09 ||
	    (C.w[1] == 0x09 && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if (2 <= q <= 16) => 5 <= 21 - q <= 19
	// Note: C * 10^(21-q) has 20 or 21 digits; 0x9fffffffffffffffb 
	// has 21 digits
	__mul_64x64_to_128MACH (C, C1, ten2k64[21 - q]);
	if (C.w[1] > 0x09 ||
	    (C.w[1] == 0x09 && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      }
    }
  }
  // n is not too large to be converted to int64 if -1/2 <= n < 2^64 - 1/2
  // Note: some of the cases tested for above fall through to this point
  if ((q + exp) < 0) {	// n = +/-0.0...c(0)c(1)...c(q-1)
    // return 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  } else if ((q + exp) == 0) {	// n = +/-0.c(0)c(1)...c(q-1)
    // if 0.c(0)c(1)...c(q-1) <= 0.5 <=> c(0)c(1)...c(q-1) <= 5 * 10^(q-1)
    //   res = 0
    // else if x > 0
    //   res = +1
    // else // if x < 0
    //   invalid exc
    ind = q - 1;	// 0 <= ind <= 15
    if (C1 <= midpoint64[ind]) {
      res = 0x0000000000000000ull;	// return 0
    } else if (!x_sign) {	// n > 0
      res = 0x0000000000000001ull;	// return +1
    } else {	// if n < 0
      res = 0x8000000000000000ull;
      *pfpsf |= INVALID_EXCEPTION;
      BID_RETURN (res);
    }
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 16, -15 <= exp <= 19)
    // x <= -1 or 1 <= x < 2^64-1/2 so if positive x can be rounded
    // to nearest to a 64-bit unsigned signed integer
    if (x_sign) {	// x <= -1
      // set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    }
    // 1 <= x < 2^64-1/2 so x can be rounded
    // to nearest to a 64-bit unsigned integer
    if (exp < 0) {	// 2 <= q <= 16, -15 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 15; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 = C1 + 1/2 * 10^ind where the result C1 fits in 64 bits
      C1 = C1 + midpoint64[ind - 1];
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 15 
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = (C1 + 1/2 * 10^x) * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 54 bits
      __mul_64x64_to_128MACH (P128, C1, ten2mk64[ind - 1]);
      Cstar = P128.w[1];
      fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
      fstar.w[0] = P128.w[0];
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind].w[0], e.g.
      // if x=1, T*=ten2mk128trunc[0].w[0]=0x1999999999999999
      // if (0 < f* < 10^(-x)) then the result is a midpoint
      //   if floor(C*) is even then C* = floor(C*) - logical right
      //       shift; C* has p decimal digits, correct by Prop. 1)
      //   else if floor(C*) is odd C* = floor(C*)-1 (logical right
      //       shift; C* has p decimal digits, correct by Pr. 1)
      // else
      //   C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-64 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 39
      Cstar = Cstar >> shift;

      // if the result was a midpoint it was rounded away from zero, so
      // it will need a correction
      // check for midpoints
      if ((fstar.w[1] == 0) && fstar.w[0] &&
	  (fstar.w[0] <= ten2mk128trunc[ind - 1].w[1])) {
	// ten2mk128trunc[ind -1].w[1] is identical to 
	// ten2mk128[ind -1].w[1]
	// the result is a midpoint; round to nearest
	if (Cstar & 0x01) {	// Cstar is odd; MP in [EVEN, ODD]
	  // if floor(C*) is odd C = floor(C*) - 1; the result >= 1
	  Cstar--;	// Cstar is now even
	}	// else MP in [ODD, EVEN]
      }
      res = Cstar;	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 10
      // res = +C (exact)
      res = C1;	// the result is positive
    } else {	// if (exp > 0) => 1 <= exp <= 9, 1 <= q < 9, 2 <= q + exp <= 10
      // res = +C * 10^exp (exact)
      res = C1 * ten2k64[exp];	// the result is positive
    }
  }
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64_to_uint64_xrnint
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_to_uint64_xrnint (UINT64 * pres, UINT64 * px
			_EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			_EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_to_uint64_xrnint (UINT64 x
			_EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			_EXC_INFO_PARAM) {
#endif
  UINT64 res;
  UINT64 x_sign;
  UINT64 x_exp;
  int exp;			// unbiased exponent
  // Note: C1 represents x_significand (UINT64)
  UINT64 tmp64;
  BID_UI64DOUBLE tmp1;
  unsigned int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  UINT128 C;
  UINT64 Cstar;			// C* represents up to 16 decimal digits ~ 54 bits
  UINT128 fstar;
  UINT128 P128;

  // check for NaN or Infinity
  if ((x & MASK_NAN) == MASK_NAN || (x & MASK_INF) == MASK_INF) {
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    x_exp = (x & MASK_BINARY_EXPONENT2) >> 51;	// biased
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      x_exp = 0;
      C1 = 0;
    }
  } else {
    x_exp = (x & MASK_BINARY_EXPONENT1) >> 53;	// biased
    C1 = x & MASK_BINARY_SIG1;
  }

  // check for zeros (possibly from non-canonical values)
  if (C1 == 0x0ull) {
    // x is 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  }
  // x is not special and is not zero

  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    // split the 64-bit value in two 32-bit halves to avoid rounding errors
    if (C1 >= 0x0000000100000000ull) {	// x >= 2^32
      tmp1.d = (double) (C1 >> 32);	// exact conversion
      x_nr_bits =
	33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    } else {	// x < 2^32
      tmp1.d = (double) C1;	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
      q++;
  }
  exp = x_exp - 398;	// unbiased exponent

  if ((q + exp) > 20) {	// x >= 10^20 ~= 2^66.45... (cannot fit in 64 bits)
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  } else if ((q + exp) == 20) {	// x = c(0)c(1)...c(19).c(20)...c(q-1)
    // in this case 2^63.11... ~= 10^19 <= x < 10^20 ~= 2^66.43...
    // so x rounded to an integer may or may not fit in an unsigned 64-bit int
    // the cases that do not fit are identified here; the ones that fit
    // fall through and will be handled with other cases further,
    // under '1 <= q + exp <= 20'
    if (x_sign) {	// if n < 0 and q + exp = 20 then x is much less than -1/2
      // => set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    } else {	// if n > 0 and q + exp = 20
      // if n >= 2^64 - 1/2 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*(2^65-1)
      // <=> C * 10^(21-q) >= 0x9fffffffffffffffb, 1<=q<=16
      if (q == 1) {
	// C * 10^20 >= 0x9fffffffffffffffb
	__mul_128x64_to_128 (C, C1, ten2k128[0]);	// 10^20 * C
	if (C.w[1] > 0x09 ||
	    (C.w[1] == 0x09 && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if (2 <= q <= 16) => 5 <= 21 - q <= 19
	// Note: C * 10^(21-q) has 20 or 21 digits; 0x9fffffffffffffffb 
	// has 21 digits
	__mul_64x64_to_128MACH (C, C1, ten2k64[21 - q]);
	if (C.w[1] > 0x09 ||
	    (C.w[1] == 0x09 && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      }
    }
  }
  // n is not too large to be converted to int64 if -1/2 <= n < 2^64 - 1/2
  // Note: some of the cases tested for above fall through to this point
  if ((q + exp) < 0) {	// n = +/-0.0...c(0)c(1)...c(q-1)
    // set inexact flag
    *pfpsf |= INEXACT_EXCEPTION;
    // return 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  } else if ((q + exp) == 0) {	// n = +/-0.c(0)c(1)...c(q-1)
    // if 0.c(0)c(1)...c(q-1) <= 0.5 <=> c(0)c(1)...c(q-1) <= 5 * 10^(q-1)
    //   res = 0
    // else if x > 0
    //   res = +1
    // else // if x < 0
    //   invalid exc
    ind = q - 1;	// 0 <= ind <= 15
    if (C1 <= midpoint64[ind]) {
      res = 0x0000000000000000ull;	// return 0
    } else if (!x_sign) {	// n > 0
      res = 0x0000000000000001ull;	// return +1
    } else {	// if n < 0
      res = 0x8000000000000000ull;
      *pfpsf |= INVALID_EXCEPTION;
      BID_RETURN (res);
    }
    // set inexact flag
    *pfpsf |= INEXACT_EXCEPTION;
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 16, -15 <= exp <= 19)
    // x <= -1 or 1 <= x < 2^64-1/2 so if positive x can be rounded
    // to nearest to a 64-bit unsigned signed integer
    if (x_sign) {	// x <= -1
      // set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    }
    // 1 <= x < 2^64-1/2 so x can be rounded
    // to nearest to a 64-bit unsigned integer
    if (exp < 0) {	// 2 <= q <= 16, -15 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 15; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 = C1 + 1/2 * 10^ind where the result C1 fits in 64 bits
      C1 = C1 + midpoint64[ind - 1];
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 15 
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = (C1 + 1/2 * 10^x) * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 54 bits
      __mul_64x64_to_128MACH (P128, C1, ten2mk64[ind - 1]);
      Cstar = P128.w[1];
      fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
      fstar.w[0] = P128.w[0];
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind].w[0], e.g.
      // if x=1, T*=ten2mk128trunc[0].w[0]=0x1999999999999999
      // if (0 < f* < 10^(-x)) then the result is a midpoint
      //   if floor(C*) is even then C* = floor(C*) - logical right
      //       shift; C* has p decimal digits, correct by Prop. 1)
      //   else if floor(C*) is odd C* = floor(C*)-1 (logical right
      //       shift; C* has p decimal digits, correct by Pr. 1)
      // else
      //   C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-64 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 39
      Cstar = Cstar >> shift;
      // determine inexactness of the rounding of C*
      // if (0 < f* - 1/2 < 10^(-x)) then
      //   the result is exact
      // else // if (f* - 1/2 > T*) then
      //   the result is inexact
      if (ind - 1 <= 2) {	// fstar.w[1] is 0
	if (fstar.w[0] > 0x8000000000000000ull) {
	  // f* > 1/2 and the result may be exact
	  tmp64 = fstar.w[0] - 0x8000000000000000ull;	// f* - 1/2
	  if ((tmp64 > ten2mk128trunc[ind - 1].w[1])) {
	    // ten2mk128trunc[ind -1].w[1] is identical to
	    // ten2mk128[ind -1].w[1]
	    // set the inexact flag
	    *pfpsf |= INEXACT_EXCEPTION;
	  }	// else the result is exact
	} else {	// the result is inexact; f2* <= 1/2
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}
      } else {	// if 3 <= ind - 1 <= 14
	if (fstar.w[1] > onehalf128[ind - 1] ||
	    (fstar.w[1] == onehalf128[ind - 1] && fstar.w[0])) {
	  // f2* > 1/2 and the result may be exact
	  // Calculate f2* - 1/2
	  tmp64 = fstar.w[1] - onehalf128[ind - 1];
	  if (tmp64 || fstar.w[0] > ten2mk128trunc[ind - 1].w[1]) {
	    // ten2mk128trunc[ind -1].w[1] is identical to
	    // ten2mk128[ind -1].w[1]
	    // set the inexact flag
	    *pfpsf |= INEXACT_EXCEPTION;
	  }	// else the result is exact
	} else {	// the result is inexact; f2* <= 1/2
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}
      }

      // if the result was a midpoint it was rounded away from zero, so
      // it will need a correction
      // check for midpoints
      if ((fstar.w[1] == 0) && fstar.w[0] &&
	  (fstar.w[0] <= ten2mk128trunc[ind - 1].w[1])) {
	// ten2mk128trunc[ind -1].w[1] is identical to 
	// ten2mk128[ind -1].w[1]
	// the result is a midpoint; round to nearest
	if (Cstar & 0x01) {	// Cstar is odd; MP in [EVEN, ODD]
	  // if floor(C*) is odd C = floor(C*) - 1; the result >= 1
	  Cstar--;	// Cstar is now even
	}	// else MP in [ODD, EVEN]
      }
      res = Cstar;	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 10
      // res = +C (exact)
      res = C1;	// the result is positive
    } else {	// if (exp > 0) => 1 <= exp <= 9, 1 <= q < 9, 2 <= q + exp <= 10
      // res = +C * 10^exp (exact)
      res = C1 * ten2k64[exp];	// the result is positive
    }
  }
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64_to_uint64_floor
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_to_uint64_floor (UINT64 * pres, UINT64 * px
		       _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		       _EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_to_uint64_floor (UINT64 x
		       _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		       _EXC_INFO_PARAM) {
#endif
  UINT64 res;
  UINT64 x_sign;
  UINT64 x_exp;
  int exp;			// unbiased exponent
  // Note: C1 represents x_significand (UINT64)
  BID_UI64DOUBLE tmp1;
  unsigned int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  UINT128 C;
  UINT64 Cstar;			// C* represents up to 16 decimal digits ~ 54 bits
  UINT128 P128;

  // check for NaN or Infinity
  if ((x & MASK_NAN) == MASK_NAN || (x & MASK_INF) == MASK_INF) {
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    x_exp = (x & MASK_BINARY_EXPONENT2) >> 51;	// biased
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      x_exp = 0;
      C1 = 0;
    }
  } else {
    x_exp = (x & MASK_BINARY_EXPONENT1) >> 53;	// biased
    C1 = x & MASK_BINARY_SIG1;
  }

  // check for zeros (possibly from non-canonical values)
  if (C1 == 0x0ull) {
    // x is 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  }
  // x is not special and is not zero

  if (x_sign) {	// if n < 0 the conversion is invalid
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  }
  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    // split the 64-bit value in two 32-bit halves to avoid rounding errors
    if (C1 >= 0x0000000100000000ull) {	// x >= 2^32
      tmp1.d = (double) (C1 >> 32);	// exact conversion
      x_nr_bits =
	33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    } else {	// x < 2^32
      tmp1.d = (double) C1;	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
      q++;
  }
  exp = x_exp - 398;	// unbiased exponent

  if ((q + exp) > 20) {	// x >= 10^20 ~= 2^66.45... (cannot fit in 64 bits)
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  } else if ((q + exp) == 20) {	// x = c(0)c(1)...c(19).c(20)...c(q-1)
    // in this case 2^63.11... ~= 10^19 <= x < 10^20 ~= 2^66.43...
    // so x rounded to an integer may or may not fit in an unsigned 64-bit int
    // the cases that do not fit are identified here; the ones that fit
    // fall through and will be handled with other cases further,
    // under '1 <= q + exp <= 20'
    // n > 0 and q + exp = 20
    // if n >= 2^64 then n is too large
    // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64
    // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64
    // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*(2^65)
    // <=> C * 10^(21-q) >= 0xa0000000000000000, 1<=q<=16
    if (q == 1) {
      // C * 10^20 >= 0xa0000000000000000
      __mul_128x64_to_128 (C, C1, ten2k128[0]);	// 10^20 * C
      if (C.w[1] >= 0x0a) {
	// actually C.w[1] == 0x0a && C.w[0] >= 0x0000000000000000ull) {
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
      // else cases that can be rounded to a 64-bit int fall through
      // to '1 <= q + exp <= 20'
    } else {	// if (2 <= q <= 16) => 5 <= 21 - q <= 19
      // Note: C * 10^(21-q) has 20 or 21 digits; 0xa0000000000000000 
      // has 21 digits
      __mul_64x64_to_128MACH (C, C1, ten2k64[21 - q]);
      if (C.w[1] >= 0x0a) {
	// actually C.w[1] == 0x0a && C.w[0] >= 0x0000000000000000ull) {
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
      // else cases that can be rounded to a 64-bit int fall through
      // to '1 <= q + exp <= 20'
    }
  }
  // n is not too large to be converted to int64 if -1 < n < 2^64
  // Note: some of the cases tested for above fall through to this point
  if ((q + exp) <= 0) {	// n = +0.[0...0]c(0)c(1)...c(q-1)
    // return 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 16, -15 <= exp <= 19)
    // 1 <= x < 2^64 so x can be rounded
    // to nearest to a 64-bit unsigned signed integer
    if (exp < 0) {	// 2 <= q <= 16, -15 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 15; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 fits in 64 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 15 
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 54 bits
      __mul_64x64_to_128MACH (P128, C1, ten2mk64[ind - 1]);
      Cstar = P128.w[1];
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind].w[0], e.g.
      // if x=1, T*=ten2mk128trunc[0].w[0]=0x1999999999999999
      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //     correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-64 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 39
      Cstar = Cstar >> shift;
      res = Cstar;	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 10
      // res = +C (exact)
      res = C1;	// the result is positive
    } else {	// if (exp > 0) => 1 <= exp <= 9, 1 <= q < 9, 2 <= q + exp <= 10
      // res = +C * 10^exp (exact)
      res = C1 * ten2k64[exp];	// the result is positive
    }
  }
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64_to_uint64_xfloor
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_to_uint64_xfloor (UINT64 * pres, UINT64 * px
			_EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			_EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_to_uint64_xfloor (UINT64 x
			_EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			_EXC_INFO_PARAM) {
#endif
  UINT64 res;
  UINT64 x_sign;
  UINT64 x_exp;
  int exp;			// unbiased exponent
  // Note: C1 represents x_significand (UINT64)
  BID_UI64DOUBLE tmp1;
  unsigned int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  UINT128 C;
  UINT64 Cstar;			// C* represents up to 16 decimal digits ~ 54 bits
  UINT128 fstar;
  UINT128 P128;

  // check for NaN or Infinity
  if ((x & MASK_NAN) == MASK_NAN || (x & MASK_INF) == MASK_INF) {
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    x_exp = (x & MASK_BINARY_EXPONENT2) >> 51;	// biased
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      x_exp = 0;
      C1 = 0;
    }
  } else {
    x_exp = (x & MASK_BINARY_EXPONENT1) >> 53;	// biased
    C1 = x & MASK_BINARY_SIG1;
  }

  // check for zeros (possibly from non-canonical values)
  if (C1 == 0x0ull) {
    // x is 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  }
  // x is not special and is not zero

  if (x_sign) {	// if n < 0 the conversion is invalid
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  }
  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    // split the 64-bit value in two 32-bit halves to avoid rounding errors
    if (C1 >= 0x0000000100000000ull) {	// x >= 2^32
      tmp1.d = (double) (C1 >> 32);	// exact conversion
      x_nr_bits =
	33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    } else {	// x < 2^32
      tmp1.d = (double) C1;	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
      q++;
  }
  exp = x_exp - 398;	// unbiased exponent

  if ((q + exp) > 20) {	// x >= 10^20 ~= 2^66.45... (cannot fit in 64 bits)
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  } else if ((q + exp) == 20) {	// x = c(0)c(1)...c(19).c(20)...c(q-1)
    // in this case 2^63.11... ~= 10^19 <= x < 10^20 ~= 2^66.43...
    // so x rounded to an integer may or may not fit in an unsigned 64-bit int
    // the cases that do not fit are identified here; the ones that fit
    // fall through and will be handled with other cases further,
    // under '1 <= q + exp <= 20'
    // n > 0 and q + exp = 20
    // if n >= 2^64 then n is too large
    // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64
    // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64
    // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*(2^65)
    // <=> C * 10^(21-q) >= 0xa0000000000000000, 1<=q<=16
    if (q == 1) {
      // C * 10^20 >= 0xa0000000000000000
      __mul_128x64_to_128 (C, C1, ten2k128[0]);	// 10^20 * C
      if (C.w[1] >= 0x0a) {
	// actually C.w[1] == 0x0a && C.w[0] >= 0x0000000000000000ull) {
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
      // else cases that can be rounded to a 64-bit int fall through
      // to '1 <= q + exp <= 20'
    } else {	// if (2 <= q <= 16) => 5 <= 21 - q <= 19
      // Note: C * 10^(21-q) has 20 or 21 digits; 0xa0000000000000000 
      // has 21 digits
      __mul_64x64_to_128MACH (C, C1, ten2k64[21 - q]);
      if (C.w[1] >= 0x0a) {
	// actually C.w[1] == 0x0a && C.w[0] >= 0x0000000000000000ull) {
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
      // else cases that can be rounded to a 64-bit int fall through
      // to '1 <= q + exp <= 20'
    }
  }
  // n is not too large to be converted to int64 if -1 < n < 2^64
  // Note: some of the cases tested for above fall through to this point
  if ((q + exp) <= 0) {	// n = +0.[0...0]c(0)c(1)...c(q-1)
    // set inexact flag
    *pfpsf |= INEXACT_EXCEPTION;
    // return 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 16, -15 <= exp <= 19)
    // 1 <= x < 2^64 so x can be rounded
    // to nearest to a 64-bit unsigned signed integer
    if (exp < 0) {	// 2 <= q <= 16, -15 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 15; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 fits in 64 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 15 
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 54 bits
      __mul_64x64_to_128MACH (P128, C1, ten2mk64[ind - 1]);
      Cstar = P128.w[1];
      fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
      fstar.w[0] = P128.w[0];
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind].w[0], e.g.
      // if x=1, T*=ten2mk128trunc[0].w[0]=0x1999999999999999
      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //     correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-64 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 39
      Cstar = Cstar >> shift;
      // determine inexactness of the rounding of C*
      // if (0 < f* < 10^(-x)) then
      //   the result is exact
      // else // if (f* > T*) then
      //   the result is inexact
      if (ind - 1 <= 2) {	// fstar.w[1] is 0
	if (fstar.w[0] > ten2mk128trunc[ind - 1].w[1]) {
	  // ten2mk128trunc[ind -1].w[1] is identical to
	  // ten2mk128[ind -1].w[1]
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      } else {	// if 3 <= ind - 1 <= 14
	if (fstar.w[1] || fstar.w[0] > ten2mk128trunc[ind - 1].w[1]) {
	  // ten2mk128trunc[ind -1].w[1] is identical to
	  // ten2mk128[ind -1].w[1]
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      }

      res = Cstar;	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 10
      // res = +C (exact)
      res = C1;	// the result is positive
    } else {	// if (exp > 0) => 1 <= exp <= 9, 1 <= q < 9, 2 <= q + exp <= 10
      // res = +C * 10^exp (exact)
      res = C1 * ten2k64[exp];	// the result is positive
    }
  }
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64_to_uint64_ceil
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_to_uint64_ceil (UINT64 * pres, UINT64 * px
		      _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		      _EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_to_uint64_ceil (UINT64 x
		      _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		      _EXC_INFO_PARAM) {
#endif
  UINT64 res;
  UINT64 x_sign;
  UINT64 x_exp;
  int exp;			// unbiased exponent
  // Note: C1 represents x_significand (UINT64)
  BID_UI64DOUBLE tmp1;
  unsigned int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  UINT128 C;
  UINT64 Cstar;			// C* represents up to 16 decimal digits ~ 54 bits
  UINT128 fstar;
  UINT128 P128;

  // check for NaN or Infinity
  if ((x & MASK_NAN) == MASK_NAN || (x & MASK_INF) == MASK_INF) {
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    x_exp = (x & MASK_BINARY_EXPONENT2) >> 51;	// biased
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      x_exp = 0;
      C1 = 0;
    }
  } else {
    x_exp = (x & MASK_BINARY_EXPONENT1) >> 53;	// biased
    C1 = x & MASK_BINARY_SIG1;
  }

  // check for zeros (possibly from non-canonical values)
  if (C1 == 0x0ull) {
    // x is 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  }
  // x is not special and is not zero

  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    // split the 64-bit value in two 32-bit halves to avoid rounding errors
    if (C1 >= 0x0000000100000000ull) {	// x >= 2^32
      tmp1.d = (double) (C1 >> 32);	// exact conversion
      x_nr_bits =
	33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    } else {	// x < 2^32
      tmp1.d = (double) C1;	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
      q++;
  }
  exp = x_exp - 398;	// unbiased exponent

  if ((q + exp) > 20) {	// x >= 10^20 ~= 2^66.45... (cannot fit in 64 bits)
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  } else if ((q + exp) == 20) {	// x = c(0)c(1)...c(19).c(20)...c(q-1)
    // in this case 2^63.11... ~= 10^19 <= x < 10^20 ~= 2^66.43...
    // so x rounded to an integer may or may not fit in an unsigned 64-bit int
    // the cases that do not fit are identified here; the ones that fit
    // fall through and will be handled with other cases further,
    // under '1 <= q + exp <= 20'
    if (x_sign) {	// if n < 0 and q + exp = 20 then x is much less than -1
      // => set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    } else {	// if n > 0 and q + exp = 20
      // if n > 2^64 - 1 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) > 2^64 - 1
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 > 2^64 - 1
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*(2^65 - 2)
      // <=> C * 10^(21-q) > 0x9fffffffffffffff6, 1<=q<=16
      if (q == 1) {
	// C * 10^20 > 0x9fffffffffffffff6
	__mul_128x64_to_128 (C, C1, ten2k128[0]);	// 10^20 * C
	if (C.w[1] > 0x09 ||
	    (C.w[1] == 0x09 && C.w[0] > 0xfffffffffffffff6ull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if (2 <= q <= 16) => 5 <= 21 - q <= 19
	// Note: C * 10^(21-q) has 20 or 21 digits; 0x9fffffffffffffff6
	// has 21 digits
	__mul_64x64_to_128MACH (C, C1, ten2k64[21 - q]);
	if (C.w[1] > 0x09 ||
	    (C.w[1] == 0x09 && C.w[0] > 0xfffffffffffffff6ull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      }
    }
  }
  // n is not too large to be converted to int64 if -1 < n < 2^64
  // Note: some of the cases tested for above fall through to this point
  if ((q + exp) <= 0) {	// n = +/-0.[0...0]c(0)c(1)...c(q-1)
    // return 0 or 1
    if (x_sign)
      res = 0x0000000000000000ull;
    else
      res = 0x0000000000000001ull;
    BID_RETURN (res);
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 16, -15 <= exp <= 19)
    // x <= -1 or 1 <= x <= 2^64 - 1 so if positive x can be rounded
    // to nearest to a 64-bit unsigned signed integer
    if (x_sign) {	// x <= -1
      // set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    }
    // 1 <= x <= 2^64 - 1 so x can be rounded
    // to nearest to a 64-bit unsigned integer
    if (exp < 0) {	// 2 <= q <= 16, -15 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 15; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 fits in 64 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 15 
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 54 bits
      __mul_64x64_to_128MACH (P128, C1, ten2mk64[ind - 1]);
      Cstar = P128.w[1];
      fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
      fstar.w[0] = P128.w[0];
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind].w[0], e.g.
      // if x=1, T*=ten2mk128trunc[0].w[0]=0x1999999999999999
      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //     correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-64 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 39
      Cstar = Cstar >> shift;
      // determine inexactness of the rounding of C*
      // if (0 < f* < 10^(-x)) then
      //   the result is exact
      // else // if (f* > T*) then
      //   the result is inexact
      if (ind - 1 <= 2) {	// fstar.w[1] is 0
	if (fstar.w[0] > ten2mk128trunc[ind - 1].w[1]) {
	  // ten2mk128trunc[ind -1].w[1] is identical to
	  // ten2mk128[ind -1].w[1]
	  Cstar++;
	}	// else the result is exact
      } else {	// if 3 <= ind - 1 <= 14
	if (fstar.w[1] || fstar.w[0] > ten2mk128trunc[ind - 1].w[1]) {
	  // ten2mk128trunc[ind -1].w[1] is identical to
	  // ten2mk128[ind -1].w[1]
	  Cstar++;
	}	// else the result is exact
      }

      res = Cstar;	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 10
      // res = +C (exact)
      res = C1;	// the result is positive
    } else {	// if (exp > 0) => 1 <= exp <= 9, 1 <= q < 9, 2 <= q + exp <= 10
      // res = +C * 10^exp (exact)
      res = C1 * ten2k64[exp];	// the result is positive
    }
  }
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64_to_uint64_xceil
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_to_uint64_xceil (UINT64 * pres, UINT64 * px
		       _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		       _EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_to_uint64_xceil (UINT64 x
		       _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		       _EXC_INFO_PARAM) {
#endif
  UINT64 res;
  UINT64 x_sign;
  UINT64 x_exp;
  int exp;			// unbiased exponent
  // Note: C1 represents x_significand (UINT64)
  BID_UI64DOUBLE tmp1;
  unsigned int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  UINT128 C;
  UINT64 Cstar;			// C* represents up to 16 decimal digits ~ 54 bits
  UINT128 fstar;
  UINT128 P128;

  // check for NaN or Infinity
  if ((x & MASK_NAN) == MASK_NAN || (x & MASK_INF) == MASK_INF) {
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    x_exp = (x & MASK_BINARY_EXPONENT2) >> 51;	// biased
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      x_exp = 0;
      C1 = 0;
    }
  } else {
    x_exp = (x & MASK_BINARY_EXPONENT1) >> 53;	// biased
    C1 = x & MASK_BINARY_SIG1;
  }

  // check for zeros (possibly from non-canonical values)
  if (C1 == 0x0ull) {
    // x is 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  }
  // x is not special and is not zero

  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    // split the 64-bit value in two 32-bit halves to avoid rounding errors
    if (C1 >= 0x0000000100000000ull) {	// x >= 2^32
      tmp1.d = (double) (C1 >> 32);	// exact conversion
      x_nr_bits =
	33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    } else {	// x < 2^32
      tmp1.d = (double) C1;	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
      q++;
  }
  exp = x_exp - 398;	// unbiased exponent

  if ((q + exp) > 20) {	// x >= 10^20 ~= 2^66.45... (cannot fit in 64 bits)
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  } else if ((q + exp) == 20) {	// x = c(0)c(1)...c(19).c(20)...c(q-1)
    // in this case 2^63.11... ~= 10^19 <= x < 10^20 ~= 2^66.43...
    // so x rounded to an integer may or may not fit in an unsigned 64-bit int
    // the cases that do not fit are identified here; the ones that fit
    // fall through and will be handled with other cases further,
    // under '1 <= q + exp <= 20'
    if (x_sign) {	// if n < 0 and q + exp = 20 then x is much less than -1
      // => set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    } else {	// if n > 0 and q + exp = 20
      // if n > 2^64 - 1 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) > 2^64 - 1
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 > 2^64 - 1
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*(2^65 - 2)
      // <=> C * 10^(21-q) > 0x9fffffffffffffff6, 1<=q<=16
      if (q == 1) {
	// C * 10^20 > 0x9fffffffffffffff6
	__mul_128x64_to_128 (C, C1, ten2k128[0]);	// 10^20 * C
	if (C.w[1] > 0x09 ||
	    (C.w[1] == 0x09 && C.w[0] > 0xfffffffffffffff6ull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if (2 <= q <= 16) => 5 <= 21 - q <= 19
	// Note: C * 10^(21-q) has 20 or 21 digits; 0x9fffffffffffffff6
	// has 21 digits
	__mul_64x64_to_128MACH (C, C1, ten2k64[21 - q]);
	if (C.w[1] > 0x09 ||
	    (C.w[1] == 0x09 && C.w[0] > 0xfffffffffffffff6ull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      }
    }
  }
  // n is not too large to be converted to int64 if -1 < n < 2^64
  // Note: some of the cases tested for above fall through to this point
  if ((q + exp) <= 0) {	// n = +/-0.[0...0]c(0)c(1)...c(q-1)
    // set inexact flag
    *pfpsf |= INEXACT_EXCEPTION;
    // return 0 or 1
    if (x_sign)
      res = 0x0000000000000000ull;
    else
      res = 0x0000000000000001ull;
    BID_RETURN (res);
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 16, -15 <= exp <= 19)
    // x <= -1 or 1 <= x <= 2^64 - 1 so if positive x can be rounded
    // to nearest to a 64-bit unsigned signed integer
    if (x_sign) {	// x <= -1
      // set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    }
    // 1 <= x <= 2^64 - 1 so x can be rounded
    // to nearest to a 64-bit unsigned integer
    if (exp < 0) {	// 2 <= q <= 16, -15 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 15; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 fits in 64 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 15 
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 54 bits
      __mul_64x64_to_128MACH (P128, C1, ten2mk64[ind - 1]);
      Cstar = P128.w[1];
      fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
      fstar.w[0] = P128.w[0];
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind].w[0], e.g.
      // if x=1, T*=ten2mk128trunc[0].w[0]=0x1999999999999999
      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //     correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-64 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 39
      Cstar = Cstar >> shift;
      // determine inexactness of the rounding of C*
      // if (0 < f* < 10^(-x)) then
      //   the result is exact
      // else // if (f* > T*) then
      //   the result is inexact
      if (ind - 1 <= 2) {	// fstar.w[1] is 0
	if (fstar.w[0] > ten2mk128trunc[ind - 1].w[1]) {
	  // ten2mk128trunc[ind -1].w[1] is identical to
	  // ten2mk128[ind -1].w[1]
	  Cstar++;
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      } else {	// if 3 <= ind - 1 <= 14
	if (fstar.w[1] || fstar.w[0] > ten2mk128trunc[ind - 1].w[1]) {
	  // ten2mk128trunc[ind -1].w[1] is identical to
	  // ten2mk128[ind -1].w[1]
	  Cstar++;
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      }

      res = Cstar;	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 10
      // res = +C (exact)
      res = C1;	// the result is positive
    } else {	// if (exp > 0) => 1 <= exp <= 9, 1 <= q < 9, 2 <= q + exp <= 10
      // res = +C * 10^exp (exact)
      res = C1 * ten2k64[exp];	// the result is positive
    }
  }
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64_to_uint64_int
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_to_uint64_int (UINT64 * pres, UINT64 * px
		     _EXC_FLAGS_PARAM _EXC_MASKS_PARAM _EXC_INFO_PARAM) 
{
  UINT64 x = *px;
#else
UINT64
bid64_to_uint64_int (UINT64 x
		     _EXC_FLAGS_PARAM _EXC_MASKS_PARAM _EXC_INFO_PARAM) 
{
#endif
  UINT64 res;
  UINT64 x_sign;
  UINT64 x_exp;
  int exp;			// unbiased exponent
  // Note: C1 represents x_significand (UINT64)
  BID_UI64DOUBLE tmp1;
  unsigned int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  UINT128 C;
  UINT64 Cstar;			// C* represents up to 16 decimal digits ~ 54 bits
  UINT128 P128;

  // check for NaN or Infinity
  if ((x & MASK_NAN) == MASK_NAN || (x & MASK_INF) == MASK_INF) {
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    x_exp = (x & MASK_BINARY_EXPONENT2) >> 51;	// biased
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      x_exp = 0;
      C1 = 0;
    }
  } else {
    x_exp = (x & MASK_BINARY_EXPONENT1) >> 53;	// biased
    C1 = x & MASK_BINARY_SIG1;
  }

  // check for zeros (possibly from non-canonical values)
  if (C1 == 0x0ull) {
    // x is 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  }
  // x is not special and is not zero

  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    // split the 64-bit value in two 32-bit halves to avoid rounding errors
    if (C1 >= 0x0000000100000000ull) {	// x >= 2^32
      tmp1.d = (double) (C1 >> 32);	// exact conversion
      x_nr_bits =
	33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    } else {	// x < 2^32
      tmp1.d = (double) C1;	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
      q++;
  }
  exp = x_exp - 398;	// unbiased exponent

  if ((q + exp) > 20) {	// x >= 10^20 ~= 2^66.45... (cannot fit in 64 bits)
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  } else if ((q + exp) == 20) {	// x = c(0)c(1)...c(19).c(20)...c(q-1)
    // in this case 2^63.11... ~= 10^19 <= x < 10^20 ~= 2^66.43...
    // so x rounded to an integer may or may not fit in an unsigned 64-bit int
    // the cases that do not fit are identified here; the ones that fit
    // fall through and will be handled with other cases further,
    // under '1 <= q + exp <= 20'
    if (x_sign) {	// if n < 0 and q + exp = 20 then x is much less than -1
      // => set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    } else {	// if n > 0 and q + exp = 20
      // if n >= 2^64 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*(2^65)
      // <=> C * 10^(21-q) >= 0xa0000000000000000, 1<=q<=16
      if (q == 1) {
	// C * 10^20 >= 0xa0000000000000000
	__mul_128x64_to_128 (C, C1, ten2k128[0]);	// 10^20 * C
	if (C.w[1] >= 0x0a) {
	  // actually C.w[1] == 0x0a && C.w[0] >= 0x0000000000000000ull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if (2 <= q <= 16) => 5 <= 21 - q <= 19
	// Note: C * 10^(21-q) has 20 or 21 digits; 0xa0000000000000000 
	// has 21 digits
	__mul_64x64_to_128MACH (C, C1, ten2k64[21 - q]);
	if (C.w[1] >= 0x0a) {
	  // actually C.w[1] == 0x0a && C.w[0] >= 0x0000000000000000ull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      }
    }
  }
  // n is not too large to be converted to int64 if -1 < n < 2^64
  // Note: some of the cases tested for above fall through to this point
  if ((q + exp) <= 0) {	// n = +/-0.[0...0]c(0)c(1)...c(q-1)
    // return 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 16, -15 <= exp <= 19)
    // x <= -1 or 1 <= x < 2^64 so if positive x can be rounded
    // to nearest to a 64-bit unsigned signed integer
    if (x_sign) {	// x <= -1
      // set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    }
    // 1 <= x < 2^64 so x can be rounded
    // to nearest to a 64-bit unsigned integer
    if (exp < 0) {	// 2 <= q <= 16, -15 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 15; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 fits in 64 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 15 
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 54 bits
      __mul_64x64_to_128MACH (P128, C1, ten2mk64[ind - 1]);
      Cstar = P128.w[1];
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind].w[0], e.g.
      // if x=1, T*=ten2mk128trunc[0].w[0]=0x1999999999999999
      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //     correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-64 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 39
      Cstar = Cstar >> shift;
      res = Cstar;	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 10
      // res = +C (exact)
      res = C1;	// the result is positive
    } else {	// if (exp > 0) => 1 <= exp <= 9, 1 <= q < 9, 2 <= q + exp <= 10
      // res = +C * 10^exp (exact)
      res = C1 * ten2k64[exp];	// the result is positive
    }
  }
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64_to_uint64_xint
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_to_uint64_xint (UINT64 * pres, UINT64 * px
		      _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		      _EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_to_uint64_xint (UINT64 x
		      _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		      _EXC_INFO_PARAM) {
#endif
  UINT64 res;
  UINT64 x_sign;
  UINT64 x_exp;
  int exp;			// unbiased exponent
  // Note: C1 represents x_significand (UINT64)
  BID_UI64DOUBLE tmp1;
  unsigned int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  UINT128 C;
  UINT64 Cstar;			// C* represents up to 16 decimal digits ~ 54 bits
  UINT128 fstar;
  UINT128 P128;

  // check for NaN or Infinity
  if ((x & MASK_NAN) == MASK_NAN || (x & MASK_INF) == MASK_INF) {
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    x_exp = (x & MASK_BINARY_EXPONENT2) >> 51;	// biased
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      x_exp = 0;
      C1 = 0;
    }
  } else {
    x_exp = (x & MASK_BINARY_EXPONENT1) >> 53;	// biased
    C1 = x & MASK_BINARY_SIG1;
  }

  // check for zeros (possibly from non-canonical values)
  if (C1 == 0x0ull) {
    // x is 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  }
  // x is not special and is not zero

  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    // split the 64-bit value in two 32-bit halves to avoid rounding errors
    if (C1 >= 0x0000000100000000ull) {	// x >= 2^32
      tmp1.d = (double) (C1 >> 32);	// exact conversion
      x_nr_bits =
	33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    } else {	// x < 2^32
      tmp1.d = (double) C1;	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
      q++;
  }
  exp = x_exp - 398;	// unbiased exponent

  if ((q + exp) > 20) {	// x >= 10^20 ~= 2^66.45... (cannot fit in 64 bits)
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  } else if ((q + exp) == 20) {	// x = c(0)c(1)...c(19).c(20)...c(q-1)
    // in this case 2^63.11... ~= 10^19 <= x < 10^20 ~= 2^66.43...
    // so x rounded to an integer may or may not fit in an unsigned 64-bit int
    // the cases that do not fit are identified here; the ones that fit
    // fall through and will be handled with other cases further,
    // under '1 <= q + exp <= 20'
    if (x_sign) {	// if n < 0 and q + exp = 20 then x is much less than -1
      // => set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    } else {	// if n > 0 and q + exp = 20
      // if n >= 2^64 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*(2^65)
      // <=> C * 10^(21-q) >= 0xa0000000000000000, 1<=q<=16
      if (q == 1) {
	// C * 10^20 >= 0xa0000000000000000
	__mul_128x64_to_128 (C, C1, ten2k128[0]);	// 10^20 * C
	if (C.w[1] >= 0x0a) {
	  // actually C.w[1] == 0x0a && C.w[0] >= 0x0000000000000000ull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if (2 <= q <= 16) => 5 <= 21 - q <= 19
	// Note: C * 10^(21-q) has 20 or 21 digits; 0xa0000000000000000 
	// has 21 digits
	__mul_64x64_to_128MACH (C, C1, ten2k64[21 - q]);
	if (C.w[1] >= 0x0a) {
	  // actually C.w[1] == 0x0a && C.w[0] >= 0x0000000000000000ull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      }
    }
  }
  // n is not too large to be converted to int64 if -1 < n < 2^64
  // Note: some of the cases tested for above fall through to this point
  if ((q + exp) <= 0) {	// n = +/-0.[0...0]c(0)c(1)...c(q-1)
    // set inexact flag
    *pfpsf |= INEXACT_EXCEPTION;
    // return 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 16, -15 <= exp <= 19)
    // x <= -1 or 1 <= x < 2^64 so if positive x can be rounded
    // to nearest to a 64-bit unsigned signed integer
    if (x_sign) {	// x <= -1
      // set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    }
    // 1 <= x < 2^64 so x can be rounded
    // to nearest to a 64-bit unsigned integer
    if (exp < 0) {	// 2 <= q <= 16, -15 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 15; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 fits in 64 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 15 
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 54 bits
      __mul_64x64_to_128MACH (P128, C1, ten2mk64[ind - 1]);
      Cstar = P128.w[1];
      fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
      fstar.w[0] = P128.w[0];
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind].w[0], e.g.
      // if x=1, T*=ten2mk128trunc[0].w[0]=0x1999999999999999
      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //     correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-64 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 39
      Cstar = Cstar >> shift;
      // determine inexactness of the rounding of C*
      // if (0 < f* < 10^(-x)) then
      //   the result is exact
      // else // if (f* > T*) then
      //   the result is inexact
      if (ind - 1 <= 2) {	// fstar.w[1] is 0
	if (fstar.w[0] > ten2mk128trunc[ind - 1].w[1]) {
	  // ten2mk128trunc[ind -1].w[1] is identical to
	  // ten2mk128[ind -1].w[1]
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      } else {	// if 3 <= ind - 1 <= 14
	if (fstar.w[1] || fstar.w[0] > ten2mk128trunc[ind - 1].w[1]) {
	  // ten2mk128trunc[ind -1].w[1] is identical to
	  // ten2mk128[ind -1].w[1]
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      }

      res = Cstar;	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 10
      // res = +C (exact)
      res = C1;	// the result is positive
    } else {	// if (exp > 0) => 1 <= exp <= 9, 1 <= q < 9, 2 <= q + exp <= 10
      // res = +C * 10^exp (exact)
      res = C1 * ten2k64[exp];	// the result is positive
    }
  }
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64_to_uint64_rninta
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_to_uint64_rninta (UINT64 * pres, UINT64 * px
			_EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			_EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_to_uint64_rninta (UINT64 x
			_EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			_EXC_INFO_PARAM) {
#endif
  UINT64 res;
  UINT64 x_sign;
  UINT64 x_exp;
  int exp;			// unbiased exponent
  // Note: C1 represents x_significand (UINT64)
  BID_UI64DOUBLE tmp1;
  unsigned int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  UINT128 C;
  UINT64 Cstar;			// C* represents up to 16 decimal digits ~ 54 bits
  UINT128 P128;

  // check for NaN or Infinity
  if ((x & MASK_NAN) == MASK_NAN || (x & MASK_INF) == MASK_INF) {
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    x_exp = (x & MASK_BINARY_EXPONENT2) >> 51;	// biased
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      x_exp = 0;
      C1 = 0;
    }
  } else {
    x_exp = (x & MASK_BINARY_EXPONENT1) >> 53;	// biased
    C1 = x & MASK_BINARY_SIG1;
  }

  // check for zeros (possibly from non-canonical values)
  if (C1 == 0x0ull) {
    // x is 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  }
  // x is not special and is not zero

  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    // split the 64-bit value in two 32-bit halves to avoid rounding errors
    if (C1 >= 0x0000000100000000ull) {	// x >= 2^32
      tmp1.d = (double) (C1 >> 32);	// exact conversion
      x_nr_bits =
	33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    } else {	// x < 2^32
      tmp1.d = (double) C1;	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
      q++;
  }
  exp = x_exp - 398;	// unbiased exponent

  if ((q + exp) > 20) {	// x >= 10^20 ~= 2^66.45... (cannot fit in 64 bits)
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  } else if ((q + exp) == 20) {	// x = c(0)c(1)...c(19).c(20)...c(q-1)
    // in this case 2^63.11... ~= 10^19 <= x < 10^20 ~= 2^66.43...
    // so x rounded to an integer may or may not fit in an unsigned 64-bit int
    // the cases that do not fit are identified here; the ones that fit
    // fall through and will be handled with other cases further,
    // under '1 <= q + exp <= 20'
    if (x_sign) {	// if n < 0 and q + exp = 20 then x is much less than -1/2
      // => set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    } else {	// if n > 0 and q + exp = 20
      // if n >= 2^64 - 1/2 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*(2^65-1)
      // <=> C * 10^(21-q) >= 0x9fffffffffffffffb, 1<=q<=16
      if (q == 1) {
	// C * 10^20 >= 0x9fffffffffffffffb
	__mul_128x64_to_128 (C, C1, ten2k128[0]);	// 10^20 * C
	if (C.w[1] > 0x09 ||
	    (C.w[1] == 0x09 && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if (2 <= q <= 16) => 5 <= 21 - q <= 19
	// Note: C * 10^(21-q) has 20 or 21 digits; 0x9fffffffffffffffb 
	// has 21 digits
	__mul_64x64_to_128MACH (C, C1, ten2k64[21 - q]);
	if (C.w[1] > 0x09 ||
	    (C.w[1] == 0x09 && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      }
    }
  }
  // n is not too large to be converted to int64 if -1/2 <= n < 2^64 - 1/2
  // Note: some of the cases tested for above fall through to this point
  if ((q + exp) < 0) {	// n = +/-0.0...c(0)c(1)...c(q-1)
    // return 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  } else if ((q + exp) == 0) {	// n = +/-0.c(0)c(1)...c(q-1)
    // if 0.c(0)c(1)...c(q-1) < 0.5 <=> c(0)c(1)...c(q-1) < 5 * 10^(q-1)
    //   res = 0
    // else if x > 0
    //   res = +1
    // else // if x < 0
    //   invalid exc
    ind = q - 1;	// 0 <= ind <= 15
    if (C1 < midpoint64[ind]) {
      res = 0x0000000000000000ull;	// return 0
    } else if (!x_sign) {	// n > 0
      res = 0x0000000000000001ull;	// return +1
    } else {	// if n < 0
      res = 0x8000000000000000ull;
      *pfpsf |= INVALID_EXCEPTION;
      BID_RETURN (res);
    }
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 16, -15 <= exp <= 19)
    // x <= -1 or 1 <= x < 2^64-1/2 so if positive x can be rounded
    // to nearest to a 64-bit unsigned signed integer
    if (x_sign) {	// x <= -1
      // set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    }
    // 1 <= x < 2^64-1/2 so x can be rounded
    // to nearest to a 64-bit unsigned integer
    if (exp < 0) {	// 2 <= q <= 16, -15 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 15; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 = C1 + 1/2 * 10^ind where the result C1 fits in 64 bits
      C1 = C1 + midpoint64[ind - 1];
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 15 
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = (C1 + 1/2 * 10^x) * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 54 bits
      __mul_64x64_to_128MACH (P128, C1, ten2mk64[ind - 1]);
      Cstar = P128.w[1];
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind].w[0], e.g.
      // if x=1, T*=ten2mk128trunc[0].w[0]=0x1999999999999999
      // if (0 < f* < 10^(-x)) then the result is a midpoint
      //   if floor(C*) is even then C* = floor(C*) - logical right
      //       shift; C* has p decimal digits, correct by Prop. 1)
      //   else if floor(C*) is odd C* = floor(C*)-1 (logical right
      //       shift; C* has p decimal digits, correct by Pr. 1)
      // else
      //   C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-64 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 39
      Cstar = Cstar >> shift;

      // if the result was a midpoint it was rounded away from zero
      res = Cstar;	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 10
      // res = +C (exact)
      res = C1;	// the result is positive
    } else {	// if (exp > 0) => 1 <= exp <= 9, 1 <= q < 9, 2 <= q + exp <= 10
      // res = +C * 10^exp (exact)
      res = C1 * ten2k64[exp];	// the result is positive
    }
  }
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64_to_uint64_xrninta
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_to_uint64_xrninta (UINT64 * pres, UINT64 * px
			 _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			 _EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_to_uint64_xrninta (UINT64 x
			 _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
			 _EXC_INFO_PARAM) {
#endif
  UINT64 res;
  UINT64 x_sign;
  UINT64 x_exp;
  int exp;			// unbiased exponent
  // Note: C1 represents x_significand (UINT64)
  UINT64 tmp64;
  BID_UI64DOUBLE tmp1;
  unsigned int x_nr_bits;
  int q, ind, shift;
  UINT64 C1;
  UINT128 C;
  UINT64 Cstar;			// C* represents up to 16 decimal digits ~ 54 bits
  UINT128 fstar;
  UINT128 P128;

  // check for NaN or Infinity
  if ((x & MASK_NAN) == MASK_NAN || (x & MASK_INF) == MASK_INF) {
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  }
  // unpack x
  x_sign = x & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
  // if steering bits are 11 (condition will be 0), then exponent is G[0:w+1] =>
  if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
    x_exp = (x & MASK_BINARY_EXPONENT2) >> 51;	// biased
    C1 = (x & MASK_BINARY_SIG2) | MASK_BINARY_OR2;
    if (C1 > 9999999999999999ull) {	// non-canonical
      x_exp = 0;
      C1 = 0;
    }
  } else {
    x_exp = (x & MASK_BINARY_EXPONENT1) >> 53;	// biased
    C1 = x & MASK_BINARY_SIG1;
  }

  // check for zeros (possibly from non-canonical values)
  if (C1 == 0x0ull) {
    // x is 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  }
  // x is not special and is not zero

  // q = nr. of decimal digits in x (1 <= q <= 54)
  //  determine first the nr. of bits in x
  if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
    // split the 64-bit value in two 32-bit halves to avoid rounding errors
    if (C1 >= 0x0000000100000000ull) {	// x >= 2^32
      tmp1.d = (double) (C1 >> 32);	// exact conversion
      x_nr_bits =
	33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    } else {	// x < 2^32
      tmp1.d = (double) C1;	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// if x < 2^53
    tmp1.d = (double) C1;	// exact conversion
    x_nr_bits =
      1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
      q++;
  }
  exp = x_exp - 398;	// unbiased exponent

  if ((q + exp) > 20) {	// x >= 10^20 ~= 2^66.45... (cannot fit in 64 bits)
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  } else if ((q + exp) == 20) {	// x = c(0)c(1)...c(19).c(20)...c(q-1)
    // in this case 2^63.11... ~= 10^19 <= x < 10^20 ~= 2^66.43...
    // so x rounded to an integer may or may not fit in an unsigned 64-bit int
    // the cases that do not fit are identified here; the ones that fit
    // fall through and will be handled with other cases further,
    // under '1 <= q + exp <= 20'
    if (x_sign) {	// if n < 0 and q + exp = 20 then x is much less than -1/2
      // => set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    } else {	// if n > 0 and q + exp = 20
      // if n >= 2^64 - 1/2 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*(2^65-1)
      // <=> C * 10^(21-q) >= 0x9fffffffffffffffb, 1<=q<=16
      if (q == 1) {
	// C * 10^20 >= 0x9fffffffffffffffb
	__mul_128x64_to_128 (C, C1, ten2k128[0]);	// 10^20 * C
	if (C.w[1] > 0x09 ||
	    (C.w[1] == 0x09 && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if (2 <= q <= 16) => 5 <= 21 - q <= 19
	// Note: C * 10^(21-q) has 20 or 21 digits; 0x9fffffffffffffffb 
	// has 21 digits
	__mul_64x64_to_128MACH (C, C1, ten2k64[21 - q]);
	if (C.w[1] > 0x09 ||
	    (C.w[1] == 0x09 && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      }
    }
  }
  // n is not too large to be converted to int64 if -1/2 <= n < 2^64 - 1/2
  // Note: some of the cases tested for above fall through to this point
  if ((q + exp) < 0) {	// n = +/-0.0...c(0)c(1)...c(q-1)
    // set inexact flag
    *pfpsf |= INEXACT_EXCEPTION;
    // return 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  } else if ((q + exp) == 0) {	// n = +/-0.c(0)c(1)...c(q-1)
    // if 0.c(0)c(1)...c(q-1) < 0.5 <=> c(0)c(1)...c(q-1) < 5 * 10^(q-1)
    //   res = 0
    // else if x > 0
    //   res = +1
    // else // if x < 0
    //   invalid exc
    ind = q - 1;	// 0 <= ind <= 15
    if (C1 < midpoint64[ind]) {
      res = 0x0000000000000000ull;	// return 0
    } else if (!x_sign) {	// n > 0
      res = 0x0000000000000001ull;	// return +1
    } else {	// if n < 0
      res = 0x8000000000000000ull;
      *pfpsf |= INVALID_EXCEPTION;
      BID_RETURN (res);
    }
    // set inexact flag
    *pfpsf |= INEXACT_EXCEPTION;
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 16, -15 <= exp <= 19)
    // x <= -1 or 1 <= x < 2^64-1/2 so if positive x can be rounded
    // to nearest to a 64-bit unsigned signed integer
    if (x_sign) {	// x <= -1
      // set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    }
    // 1 <= x < 2^64-1/2 so x can be rounded
    // to nearest to a 64-bit unsigned integer
    if (exp < 0) {	// 2 <= q <= 16, -15 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 15; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 = C1 + 1/2 * 10^ind where the result C1 fits in 64 bits
      C1 = C1 + midpoint64[ind - 1];
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 15 
      // kx = 10^(-x) = ten2mk64[ind - 1]
      // C* = (C1 + 1/2 * 10^x) * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 54 bits
      __mul_64x64_to_128MACH (P128, C1, ten2mk64[ind - 1]);
      Cstar = P128.w[1];
      fstar.w[1] = P128.w[1] & maskhigh128[ind - 1];
      fstar.w[0] = P128.w[0];
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind].w[0], e.g.
      // if x=1, T*=ten2mk128trunc[0].w[0]=0x1999999999999999
      // if (0 < f* < 10^(-x)) then the result is a midpoint
      //   if floor(C*) is even then C* = floor(C*) - logical right
      //       shift; C* has p decimal digits, correct by Prop. 1)
      //   else if floor(C*) is odd C* = floor(C*)-1 (logical right
      //       shift; C* has p decimal digits, correct by Pr. 1)
      // else
      //   C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-64 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 39
      Cstar = Cstar >> shift;
      // determine inexactness of the rounding of C*
      // if (0 < f* - 1/2 < 10^(-x)) then
      //   the result is exact
      // else // if (f* - 1/2 > T*) then
      //   the result is inexact
      if (ind - 1 <= 2) {	// fstar.w[1] is 0
	if (fstar.w[0] > 0x8000000000000000ull) {
	  // f* > 1/2 and the result may be exact
	  tmp64 = fstar.w[0] - 0x8000000000000000ull;	// f* - 1/2
	  if ((tmp64 > ten2mk128trunc[ind - 1].w[1])) {
	    // ten2mk128trunc[ind -1].w[1] is identical to
	    // ten2mk128[ind -1].w[1]
	    // set the inexact flag
	    *pfpsf |= INEXACT_EXCEPTION;
	  }	// else the result is exact
	} else {	// the result is inexact; f2* <= 1/2
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}
      } else {	// if 3 <= ind - 1 <= 14
	if (fstar.w[1] > onehalf128[ind - 1] ||
	    (fstar.w[1] == onehalf128[ind - 1] && fstar.w[0])) {
	  // f2* > 1/2 and the result may be exact
	  // Calculate f2* - 1/2
	  tmp64 = fstar.w[1] - onehalf128[ind - 1];
	  if (tmp64 || fstar.w[0] > ten2mk128trunc[ind - 1].w[1]) {
	    // ten2mk128trunc[ind -1].w[1] is identical to
	    // ten2mk128[ind -1].w[1]
	    // set the inexact flag
	    *pfpsf |= INEXACT_EXCEPTION;
	  }	// else the result is exact
	} else {	// the result is inexact; f2* <= 1/2
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}
      }

      // if the result was a midpoint it was rounded away from zero
      res = Cstar;	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 10
      // res = +C (exact)
      res = C1;	// the result is positive
    } else {	// if (exp > 0) => 1 <= exp <= 9, 1 <= q < 9, 2 <= q + exp <= 10
      // res = +C * 10^exp (exact)
      res = C1 * ten2k64[exp];	// the result is positive
    }
  }
  BID_RETURN (res);
}
