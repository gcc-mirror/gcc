/* Copyright (C) 2007-2019 Free Software Foundation, Inc.

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
 *  BID128_to_uint64_rnint
 ****************************************************************************/

BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE (UINT64,
					  bid128_to_uint64_rnint, x)

     UINT64 res;
     UINT64 x_sign;
     UINT64 x_exp;
     int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
     UINT64 tmp64;
     BID_UI64DOUBLE tmp1;
     unsigned int x_nr_bits;
     int q, ind, shift;
     UINT128 C1, C;
     UINT128 Cstar;		// C* represents up to 34 decimal digits ~ 113 bits
     UINT256 fstar;
     UINT256 P256;

  // unpack x
x_sign = x.w[1] & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
x_exp = x.w[1] & MASK_EXP;	// biased and shifted left 49 bit positions
C1.w[1] = x.w[1] & MASK_COEFF;
C1.w[0] = x.w[0];

  // check for NaN or Infinity
if ((x.w[1] & MASK_SPECIAL) == MASK_SPECIAL) {
    // x is special
if ((x.w[1] & MASK_NAN) == MASK_NAN) {	// x is NAN
  if ((x.w[1] & MASK_SNAN) == MASK_SNAN) {	// x is SNAN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is QNaN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
} else {	// x is not a NaN, so it must be infinity
  if (!x_sign) {	// x is +inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is -inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
}
}
  // check for non-canonical values (after the check for special values)
if ((C1.w[1] > 0x0001ed09bead87c0ull)
    || (C1.w[1] == 0x0001ed09bead87c0ull
	&& (C1.w[0] > 0x378d8e63ffffffffull))
    || ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull)) {
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else if ((C1.w[1] == 0x0ull) && (C1.w[0] == 0x0ull)) {
  // x is 0
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else {	// x is not special and is not zero

  // q = nr. of decimal digits in x
  //  determine first the nr. of bits in x
  if (C1.w[1] == 0) {
    if (C1.w[0] >= 0x0020000000000000ull) {	// x >= 2^53
      // split the 64-bit value in two 32-bit halves to avoid rounding errors
      if (C1.w[0] >= 0x0000000100000000ull) {	// x >= 2^32
	tmp1.d = (double) (C1.w[0] >> 32);	// exact conversion
	x_nr_bits =
	  33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      } else {	// x < 2^32
	tmp1.d = (double) (C1.w[0]);	// exact conversion
	x_nr_bits =
	  1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      }
    } else {	// if x < 2^53
      tmp1.d = (double) C1.w[0];	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// C1.w[1] != 0 => nr. bits = 64 + nr_bits (C1.w[1])
    tmp1.d = (double) C1.w[1];	// exact conversion
    x_nr_bits =
      65 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1.w[1] > nr_digits[x_nr_bits - 1].threshold_hi
	|| (C1.w[1] == nr_digits[x_nr_bits - 1].threshold_hi
	    && C1.w[0] >= nr_digits[x_nr_bits - 1].threshold_lo))
      q++;
  }
  exp = (x_exp >> 49) - 6176;

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
    if (x_sign) {	// if n < 0 and q + exp = 20
      // if n < -1/2 then n cannot be converted to uint64 with RN
      // too large if c(0)c(1)...c(19).c(20)...c(q-1) > 1/2
      // <=> 0.c(0)c(1)...c(q-1) * 10^21 > 0x05, 1<=q<=34
      // <=> C * 10^(21-q) > 0x05, 1<=q<=34
      if (q == 21) {
	// C > 5 
	if (C1.w[1] != 0 || C1.w[0] > 0x05ull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to 64-bit unsigned int fall through
	// to '1 <= q + exp <= 20'
      } else {
	// if 1 <= q <= 20
	//   C * 10^(21-q) > 5 is true because C >= 1 and 10^(21-q) >= 10
	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	//   C > 5 * 10^(q-21) is true because C > 2^64 and 5*10^(q-21) < 2^64
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
    } else {	// if n > 0 and q + exp = 20
      // if n >= 2^64 - 1/2 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*(2^65-1)
      // <=> C * 10^(21-q) >= 0x9fffffffffffffffb, 1<=q<=34
      if (q == 1) {
	// C * 10^20 >= 0x9fffffffffffffffb
	__mul_128x64_to_128 (C, C1.w[0], ten2k128[0]);	// 10^20 * C
	if (C.w[1] > 0x09 || (C.w[1] == 0x09
			      && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q <= 19) {
	// C * 10^(21-q) >= 0x9fffffffffffffffb
	__mul_64x64_to_128MACH (C, C1.w[0], ten2k64[21 - q]);
	if (C.w[1] > 0x09 || (C.w[1] == 0x09
			      && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q == 20) {
	// C * 10 >= 0x9fffffffffffffffb <=> C * 2 > 1ffffffffffffffff
	C.w[0] = C1.w[0] + C1.w[0];
	C.w[1] = C1.w[1] + C1.w[1];
	if (C.w[0] < C1.w[0])
	  C.w[1]++;
	if (C.w[1] > 0x01 || (C.w[1] == 0x01
			      && C.w[0] >= 0xffffffffffffffffull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q == 21) {
	// C >= 0x9fffffffffffffffb
	if (C1.w[1] > 0x09 || (C1.w[1] == 0x09
			       && C1.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	// C  >= 10^(q-21) * 0x9fffffffffffffffb max 44 bits x 68 bits
	C.w[1] = 0x09;
	C.w[0] = 0xfffffffffffffffbull;
	__mul_128x64_to_128 (C, ten2k64[q - 21], C);
	if (C1.w[1] > C.w[1]
	    || (C1.w[1] == C.w[1] && C1.w[0] >= C.w[0])) {
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
    ind = q - 1;
    if (ind <= 18) {	// 0 <= ind <= 18
      if ((C1.w[1] == 0) && (C1.w[0] <= midpoint64[ind])) {
	res = 0x0000000000000000ull;	// return 0
      } else if (!x_sign) {	// n > 0
	res = 0x00000001;	// return +1
      } else {
	res = 0x8000000000000000ull;
	*pfpsf |= INVALID_EXCEPTION;
      }
    } else {	// 19 <= ind <= 33
      if ((C1.w[1] < midpoint128[ind - 19].w[1])
	  || ((C1.w[1] == midpoint128[ind - 19].w[1])
	      && (C1.w[0] <= midpoint128[ind - 19].w[0]))) {
	res = 0x0000000000000000ull;	// return 0
      } else if (!x_sign) {	// n > 0
	res = 0x00000001;	// return +1
      } else {
	res = 0x8000000000000000ull;
	*pfpsf |= INVALID_EXCEPTION;
      }
    }
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 34, -33 <= exp <= 19)
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
    if (exp < 0) {	// 2 <= q <= 34, -33 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 33; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 = C1 + 1/2 * 10^ind where the result C1 fits in 127 bits
      tmp64 = C1.w[0];
      if (ind <= 19) {
	C1.w[0] = C1.w[0] + midpoint64[ind - 1];
      } else {
	C1.w[0] = C1.w[0] + midpoint128[ind - 20].w[0];
	C1.w[1] = C1.w[1] + midpoint128[ind - 20].w[1];
      }
      if (C1.w[0] < tmp64)
	C1.w[1]++;
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 33
      // kx = 10^(-x) = ten2mk128[ind - 1]
      // C* = (C1 + 1/2 * 10^x) * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 118 bits
      __mul_128x128_to_256 (P256, C1, ten2mk128[ind - 1]);
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[1] = P256.w[3];
	Cstar.w[0] = P256.w[2];
	fstar.w[3] = 0;
	fstar.w[2] = P256.w[2] & maskhigh128[ind - 1];
	fstar.w[1] = P256.w[1];
	fstar.w[0] = P256.w[0];
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[1] = 0;
	Cstar.w[0] = P256.w[3];
	fstar.w[3] = P256.w[3] & maskhigh128[ind - 1];
	fstar.w[2] = P256.w[2];
	fstar.w[1] = P256.w[1];
	fstar.w[0] = P256.w[0];
      }
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind], e.g.
      // if x=1, T*=ten2mk128trunc[0]=0x19999999999999999999999999999999
      // if (0 < f* < 10^(-x)) then the result is a midpoint
      //   if floor(C*) is even then C* = floor(C*) - logical right
      //       shift; C* has p decimal digits, correct by Prop. 1)
      //   else if floor(C*) is odd C* = floor(C*)-1 (logical right
      //       shift; C* has p decimal digits, correct by Pr. 1)
      // else
      //   C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-128 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 102
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[0] =
	  (Cstar.w[0] >> shift) | (Cstar.w[1] << (64 - shift));
	// redundant, it will be 0! Cstar.w[1] = (Cstar.w[1] >> shift);
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[0] = (Cstar.w[0] >> (shift - 64));	// 2 <= shift - 64 <= 38
      }
      // if the result was a midpoint it was rounded away from zero, so
      // it will need a correction
      // check for midpoints
      if ((fstar.w[3] == 0) && (fstar.w[2] == 0)
	  && (fstar.w[1] || fstar.w[0])
	  && (fstar.w[1] < ten2mk128trunc[ind - 1].w[1]
	      || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		  && fstar.w[0] <= ten2mk128trunc[ind - 1].w[0]))) {
	// the result is a midpoint; round to nearest
	if (Cstar.w[0] & 0x01) {	// Cstar.w[0] is odd; MP in [EVEN, ODD]
	  // if floor(C*) is odd C = floor(C*) - 1; the result >= 1
	  Cstar.w[0]--;	// Cstar.w[0] is now even
	}	// else MP in [ODD, EVEN]
      }
      res = Cstar.w[0];	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 20, but x < 2^64 - 1/2 so in this case C1.w[1] has to be 0
      // res = C (exact)
      res = C1.w[0];
    } else {
      // if (exp > 0) => 1 <= exp <= 19, 1 <= q < 19, 2 <= q + exp <= 20
      // res = C * 10^exp (exact) - must fit in 64 bits
      res = C1.w[0] * ten2k64[exp];
    }
  }
}

BID_RETURN (res);
}

/*****************************************************************************
 *  BID128_to_uint64_xrnint
 ****************************************************************************/

BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE (UINT64,
					  bid128_to_uint64_xrnint, x)

     UINT64 res;
     UINT64 x_sign;
     UINT64 x_exp;
     int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
     UINT64 tmp64, tmp64A;
     BID_UI64DOUBLE tmp1;
     unsigned int x_nr_bits;
     int q, ind, shift;
     UINT128 C1, C;
     UINT128 Cstar;		// C* represents up to 34 decimal digits ~ 113 bits
     UINT256 fstar;
     UINT256 P256;

  // unpack x
x_sign = x.w[1] & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
x_exp = x.w[1] & MASK_EXP;	// biased and shifted left 49 bit positions
C1.w[1] = x.w[1] & MASK_COEFF;
C1.w[0] = x.w[0];

  // check for NaN or Infinity
if ((x.w[1] & MASK_SPECIAL) == MASK_SPECIAL) {
    // x is special
if ((x.w[1] & MASK_NAN) == MASK_NAN) {	// x is NAN
  if ((x.w[1] & MASK_SNAN) == MASK_SNAN) {	// x is SNAN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is QNaN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
} else {	// x is not a NaN, so it must be infinity
  if (!x_sign) {	// x is +inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is -inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
}
}
  // check for non-canonical values (after the check for special values)
if ((C1.w[1] > 0x0001ed09bead87c0ull)
    || (C1.w[1] == 0x0001ed09bead87c0ull
	&& (C1.w[0] > 0x378d8e63ffffffffull))
    || ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull)) {
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else if ((C1.w[1] == 0x0ull) && (C1.w[0] == 0x0ull)) {
  // x is 0
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else {	// x is not special and is not zero

  // q = nr. of decimal digits in x
  //  determine first the nr. of bits in x
  if (C1.w[1] == 0) {
    if (C1.w[0] >= 0x0020000000000000ull) {	// x >= 2^53
      // split the 64-bit value in two 32-bit halves to avoid rounding errors
      if (C1.w[0] >= 0x0000000100000000ull) {	// x >= 2^32
	tmp1.d = (double) (C1.w[0] >> 32);	// exact conversion
	x_nr_bits =
	  33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      } else {	// x < 2^32
	tmp1.d = (double) (C1.w[0]);	// exact conversion
	x_nr_bits =
	  1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      }
    } else {	// if x < 2^53
      tmp1.d = (double) C1.w[0];	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// C1.w[1] != 0 => nr. bits = 64 + nr_bits (C1.w[1])
    tmp1.d = (double) C1.w[1];	// exact conversion
    x_nr_bits =
      65 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1.w[1] > nr_digits[x_nr_bits - 1].threshold_hi
	|| (C1.w[1] == nr_digits[x_nr_bits - 1].threshold_hi
	    && C1.w[0] >= nr_digits[x_nr_bits - 1].threshold_lo))
      q++;
  }
  exp = (x_exp >> 49) - 6176;

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
    if (x_sign) {	// if n < 0 and q + exp = 20
      // if n < -1/2 then n cannot be converted to uint64 with RN
      // too large if c(0)c(1)...c(19).c(20)...c(q-1) > 1/2
      // <=> 0.c(0)c(1)...c(q-1) * 10^21 > 0x05, 1<=q<=34
      // <=> C * 10^(21-q) > 0x05, 1<=q<=34
      if (q == 21) {
	// C > 5 
	if (C1.w[1] != 0 || C1.w[0] > 0x05ull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to 64-bit unsigned int fall through
	// to '1 <= q + exp <= 20'
      } else {
	// if 1 <= q <= 20
	//   C * 10^(21-q) > 5 is true because C >= 1 and 10^(21-q) >= 10
	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	//   C > 5 * 10^(q-21) is true because C > 2^64 and 5*10^(q-21) < 2^64
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
    } else {	// if n > 0 and q + exp = 20
      // if n >= 2^64 - 1/2 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*(2^65-1)
      // <=> C * 10^(21-q) >= 0x9fffffffffffffffb, 1<=q<=34
      if (q == 1) {
	// C * 10^20 >= 0x9fffffffffffffffb
	__mul_128x64_to_128 (C, C1.w[0], ten2k128[0]);	// 10^20 * C
	if (C.w[1] > 0x09 || (C.w[1] == 0x09
			      && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q <= 19) {
	// C * 10^(21-q) >= 0x9fffffffffffffffb
	__mul_64x64_to_128MACH (C, C1.w[0], ten2k64[21 - q]);
	if (C.w[1] > 0x09 || (C.w[1] == 0x09
			      && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q == 20) {
	// C * 10 >= 0x9fffffffffffffffb <=> C * 2 > 1ffffffffffffffff
	C.w[0] = C1.w[0] + C1.w[0];
	C.w[1] = C1.w[1] + C1.w[1];
	if (C.w[0] < C1.w[0])
	  C.w[1]++;
	if (C.w[1] > 0x01 || (C.w[1] == 0x01
			      && C.w[0] >= 0xffffffffffffffffull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q == 21) {
	// C >= 0x9fffffffffffffffb
	if (C1.w[1] > 0x09 || (C1.w[1] == 0x09
			       && C1.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	// C  >= 10^(q-21) * 0x9fffffffffffffffb max 44 bits x 68 bits
	C.w[1] = 0x09;
	C.w[0] = 0xfffffffffffffffbull;
	__mul_128x64_to_128 (C, ten2k64[q - 21], C);
	if (C1.w[1] > C.w[1]
	    || (C1.w[1] == C.w[1] && C1.w[0] >= C.w[0])) {
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
    ind = q - 1;
    if (ind <= 18) {	// 0 <= ind <= 18
      if ((C1.w[1] == 0) && (C1.w[0] <= midpoint64[ind])) {
	res = 0x0000000000000000ull;	// return 0
      } else if (!x_sign) {	// n > 0
	res = 0x00000001;	// return +1
      } else {
	res = 0x8000000000000000ull;
	*pfpsf |= INVALID_EXCEPTION;
	BID_RETURN (res);
      }
    } else {	// 19 <= ind <= 33
      if ((C1.w[1] < midpoint128[ind - 19].w[1])
	  || ((C1.w[1] == midpoint128[ind - 19].w[1])
	      && (C1.w[0] <= midpoint128[ind - 19].w[0]))) {
	res = 0x0000000000000000ull;	// return 0
      } else if (!x_sign) {	// n > 0
	res = 0x00000001;	// return +1
      } else {
	res = 0x8000000000000000ull;
	*pfpsf |= INVALID_EXCEPTION;
	BID_RETURN (res);
      }
    }
    // set inexact flag
    *pfpsf |= INEXACT_EXCEPTION;
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 34, -33 <= exp <= 19)
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
    if (exp < 0) {	// 2 <= q <= 34, -33 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 33; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 = C1 + 1/2 * 10^ind where the result C1 fits in 127 bits
      tmp64 = C1.w[0];
      if (ind <= 19) {
	C1.w[0] = C1.w[0] + midpoint64[ind - 1];
      } else {
	C1.w[0] = C1.w[0] + midpoint128[ind - 20].w[0];
	C1.w[1] = C1.w[1] + midpoint128[ind - 20].w[1];
      }
      if (C1.w[0] < tmp64)
	C1.w[1]++;
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 33
      // kx = 10^(-x) = ten2mk128[ind - 1]
      // C* = (C1 + 1/2 * 10^x) * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 118 bits
      __mul_128x128_to_256 (P256, C1, ten2mk128[ind - 1]);
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[1] = P256.w[3];
	Cstar.w[0] = P256.w[2];
	fstar.w[3] = 0;
	fstar.w[2] = P256.w[2] & maskhigh128[ind - 1];
	fstar.w[1] = P256.w[1];
	fstar.w[0] = P256.w[0];
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[1] = 0;
	Cstar.w[0] = P256.w[3];
	fstar.w[3] = P256.w[3] & maskhigh128[ind - 1];
	fstar.w[2] = P256.w[2];
	fstar.w[1] = P256.w[1];
	fstar.w[0] = P256.w[0];
      }
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind], e.g.
      // if x=1, T*=ten2mk128trunc[0]=0x19999999999999999999999999999999
      // if (0 < f* < 10^(-x)) then the result is a midpoint
      //   if floor(C*) is even then C* = floor(C*) - logical right
      //       shift; C* has p decimal digits, correct by Prop. 1)
      //   else if floor(C*) is odd C* = floor(C*)-1 (logical right
      //       shift; C* has p decimal digits, correct by Pr. 1)
      // else
      //   C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-128 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 102
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[0] =
	  (Cstar.w[0] >> shift) | (Cstar.w[1] << (64 - shift));
	// redundant, it will be 0! Cstar.w[1] = (Cstar.w[1] >> shift);
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[0] = (Cstar.w[0] >> (shift - 64));	// 2 <= shift - 64 <= 38
      }
      // determine inexactness of the rounding of C*
      // if (0 < f* - 1/2 < 10^(-x)) then
      //   the result is exact
      // else // if (f* - 1/2 > T*) then
      //   the result is inexact
      if (ind - 1 <= 2) {
	if (fstar.w[1] > 0x8000000000000000ull ||
	    (fstar.w[1] == 0x8000000000000000ull
	     && fstar.w[0] > 0x0ull)) {
	  // f* > 1/2 and the result may be exact
	  tmp64 = fstar.w[1] - 0x8000000000000000ull;	// f* - 1/2
	  if (tmp64 > ten2mk128trunc[ind - 1].w[1]
	      || (tmp64 == ten2mk128trunc[ind - 1].w[1]
		  && fstar.w[0] >= ten2mk128trunc[ind - 1].w[0])) {
	    // set the inexact flag
	    *pfpsf |= INEXACT_EXCEPTION;
	  }	// else the result is exact
	} else {	// the result is inexact; f2* <= 1/2
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}
      } else if (ind - 1 <= 21) {	// if 3 <= ind <= 21
	if (fstar.w[3] > 0x0 ||
	    (fstar.w[3] == 0x0 && fstar.w[2] > onehalf128[ind - 1]) ||
	    (fstar.w[3] == 0x0 && fstar.w[2] == onehalf128[ind - 1] &&
	     (fstar.w[1] || fstar.w[0]))) {
	  // f2* > 1/2 and the result may be exact
	  // Calculate f2* - 1/2
	  tmp64 = fstar.w[2] - onehalf128[ind - 1];
	  tmp64A = fstar.w[3];
	  if (tmp64 > fstar.w[2])
	    tmp64A--;
	  if (tmp64A || tmp64
	      || fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	      || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		  && fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	    // set the inexact flag
	    *pfpsf |= INEXACT_EXCEPTION;
	  }	// else the result is exact
	} else {	// the result is inexact; f2* <= 1/2
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}
      } else {	// if 22 <= ind <= 33
	if (fstar.w[3] > onehalf128[ind - 1] ||
	    (fstar.w[3] == onehalf128[ind - 1] &&
	     (fstar.w[2] || fstar.w[1] || fstar.w[0]))) {
	  // f2* > 1/2 and the result may be exact
	  // Calculate f2* - 1/2
	  tmp64 = fstar.w[3] - onehalf128[ind - 1];
	  if (tmp64 || fstar.w[2]
	      || fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	      || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		  && fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
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
      if ((fstar.w[3] == 0) && (fstar.w[2] == 0)
	  && (fstar.w[1] || fstar.w[0])
	  && (fstar.w[1] < ten2mk128trunc[ind - 1].w[1]
	      || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		  && fstar.w[0] <= ten2mk128trunc[ind - 1].w[0]))) {
	// the result is a midpoint; round to nearest
	if (Cstar.w[0] & 0x01) {	// Cstar.w[0] is odd; MP in [EVEN, ODD]
	  // if floor(C*) is odd C = floor(C*) - 1; the result >= 1
	  Cstar.w[0]--;	// Cstar.w[0] is now even
	}	// else MP in [ODD, EVEN]
      }
      res = Cstar.w[0];	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 20, but x < 2^64 - 1/2 so in this case C1.w[1] has to be 0
      // res = C (exact)
      res = C1.w[0];
    } else {
      // if (exp > 0) => 1 <= exp <= 19, 1 <= q < 19, 2 <= q + exp <= 20
      // res = C * 10^exp (exact) - must fit in 64 bits
      res = C1.w[0] * ten2k64[exp];
    }
  }
}

BID_RETURN (res);
}

/*****************************************************************************
 *  BID128_to_uint64_floor
 ****************************************************************************/

BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE (UINT64,
					  bid128_to_uint64_floor, x)

     UINT64 res;
     UINT64 x_sign;
     UINT64 x_exp;
     int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
     BID_UI64DOUBLE tmp1;
     unsigned int x_nr_bits;
     int q, ind, shift;
     UINT128 C1, C;
     UINT128 Cstar;		// C* represents up to 34 decimal digits ~ 113 bits
     UINT256 P256;

  // unpack x
x_sign = x.w[1] & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
x_exp = x.w[1] & MASK_EXP;	// biased and shifted left 49 bit positions
C1.w[1] = x.w[1] & MASK_COEFF;
C1.w[0] = x.w[0];

  // check for NaN or Infinity
if ((x.w[1] & MASK_SPECIAL) == MASK_SPECIAL) {
    // x is special
if ((x.w[1] & MASK_NAN) == MASK_NAN) {	// x is NAN
  if ((x.w[1] & MASK_SNAN) == MASK_SNAN) {	// x is SNAN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is QNaN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
} else {	// x is not a NaN, so it must be infinity
  if (!x_sign) {	// x is +inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is -inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
}
}
  // check for non-canonical values (after the check for special values)
if ((C1.w[1] > 0x0001ed09bead87c0ull)
    || (C1.w[1] == 0x0001ed09bead87c0ull
	&& (C1.w[0] > 0x378d8e63ffffffffull))
    || ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull)) {
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else if ((C1.w[1] == 0x0ull) && (C1.w[0] == 0x0ull)) {
  // x is 0
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else {	// x is not special and is not zero

  // if n < 0 then n cannot be converted to uint64 with RM
  if (x_sign) {	// if n < 0 and q + exp = 20
    // too large if c(0)c(1)...c(19).c(20)...c(q-1) > 0
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  }
  // q = nr. of decimal digits in x
  //  determine first the nr. of bits in x
  if (C1.w[1] == 0) {
    if (C1.w[0] >= 0x0020000000000000ull) {	// x >= 2^53
      // split the 64-bit value in two 32-bit halves to avoid rounding errors
      if (C1.w[0] >= 0x0000000100000000ull) {	// x >= 2^32
	tmp1.d = (double) (C1.w[0] >> 32);	// exact conversion
	x_nr_bits =
	  33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      } else {	// x < 2^32
	tmp1.d = (double) (C1.w[0]);	// exact conversion
	x_nr_bits =
	  1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      }
    } else {	// if x < 2^53
      tmp1.d = (double) C1.w[0];	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// C1.w[1] != 0 => nr. bits = 64 + nr_bits (C1.w[1])
    tmp1.d = (double) C1.w[1];	// exact conversion
    x_nr_bits =
      65 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1.w[1] > nr_digits[x_nr_bits - 1].threshold_hi
	|| (C1.w[1] == nr_digits[x_nr_bits - 1].threshold_hi
	    && C1.w[0] >= nr_digits[x_nr_bits - 1].threshold_lo))
      q++;
  }
  exp = (x_exp >> 49) - 6176;
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
    // if n > 0 and q + exp = 20
    // if n >= 2^64 then n is too large
    // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64
    // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64
    // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*2^65
    // <=> C * 10^(21-q) >= 0xa0000000000000000, 1<=q<=34
    if (q == 1) {
      // C * 10^20 >= 0xa0000000000000000
      __mul_128x64_to_128 (C, C1.w[0], ten2k128[0]);	// 10^20 * C
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
    } else if (q <= 19) {
      // C * 10^(21-q) >= 0xa0000000000000000
      __mul_64x64_to_128MACH (C, C1.w[0], ten2k64[21 - q]);
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
    } else if (q == 20) {
      // C >= 0x10000000000000000
      if (C1.w[1] >= 0x01) {
	// actually C1.w[1] == 0x01 && C1.w[0] >= 0x0000000000000000ull) {
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
      // else cases that can be rounded to a 64-bit int fall through
      // to '1 <= q + exp <= 20'
    } else if (q == 21) {
      // C >= 0xa0000000000000000
      if (C1.w[1] >= 0x0a) {
	// actually C1.w[1] == 0x0a && C1.w[0] >= 0x0000000000000000ull) {
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
      // else cases that can be rounded to a 64-bit int fall through
      // to '1 <= q + exp <= 20'
    } else {	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
      // C  >= 10^(q-21) * 0xa0000000000000000 max 44 bits x 68 bits
      C.w[1] = 0x0a;
      C.w[0] = 0x0000000000000000ull;
      __mul_128x64_to_128 (C, ten2k64[q - 21], C);
      if (C1.w[1] > C.w[1] || (C1.w[1] == C.w[1] && C1.w[0] >= C.w[0])) {
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
  // n is not too large to be converted to int64 if 0 <= n < 2^64
  // Note: some of the cases tested for above fall through to this point
  if ((q + exp) <= 0) {	// n = +0.[0...0]c(0)c(1)...c(q-1)
    // return 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 34, -33 <= exp <= 19)
    // 1 <= x < 2^64 so x can be rounded
    // down to a 64-bit unsigned signed integer
    if (exp < 0) {	// 2 <= q <= 34, -33 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 33; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 fits in 127 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 33
      // kx = 10^(-x) = ten2mk128[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 118 bits
      __mul_128x128_to_256 (P256, C1, ten2mk128[ind - 1]);
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[1] = P256.w[3];
	Cstar.w[0] = P256.w[2];
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[1] = 0;
	Cstar.w[0] = P256.w[3];
      }
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind], e.g.
      // if x=1, T*=ten2mk128trunc[0]=0x19999999999999999999999999999999
      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //     correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-128 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 102
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[0] =
	  (Cstar.w[0] >> shift) | (Cstar.w[1] << (64 - shift));
	// redundant, it will be 0! Cstar.w[1] = (Cstar.w[1] >> shift);
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[0] = (Cstar.w[0] >> (shift - 64));	// 2 <= shift - 64 <= 38
      }
      res = Cstar.w[0];	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 20, but x < 2^64 - 1/2 so in this case C1.w[1] has to be 0
      // res = C (exact)
      res = C1.w[0];
    } else {
      // if (exp > 0) => 1 <= exp <= 19, 1 <= q < 19, 2 <= q + exp <= 20
      // res = C * 10^exp (exact) - must fit in 64 bits
      res = C1.w[0] * ten2k64[exp];
    }
  }
}

BID_RETURN (res);
}

/*****************************************************************************
 *  BID128_to_uint64_xfloor
 ****************************************************************************/

BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE (UINT64,
					  bid128_to_uint64_xfloor, x)

     UINT64 res;
     UINT64 x_sign;
     UINT64 x_exp;
     int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
     BID_UI64DOUBLE tmp1;
     unsigned int x_nr_bits;
     int q, ind, shift;
     UINT128 C1, C;
     UINT128 Cstar;		// C* represents up to 34 decimal digits ~ 113 bits
     UINT256 fstar;
     UINT256 P256;

  // unpack x
x_sign = x.w[1] & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
x_exp = x.w[1] & MASK_EXP;	// biased and shifted left 49 bit positions
C1.w[1] = x.w[1] & MASK_COEFF;
C1.w[0] = x.w[0];

  // check for NaN or Infinity
if ((x.w[1] & MASK_SPECIAL) == MASK_SPECIAL) {
    // x is special
if ((x.w[1] & MASK_NAN) == MASK_NAN) {	// x is NAN
  if ((x.w[1] & MASK_SNAN) == MASK_SNAN) {	// x is SNAN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is QNaN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
} else {	// x is not a NaN, so it must be infinity
  if (!x_sign) {	// x is +inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is -inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
}
}
  // check for non-canonical values (after the check for special values)
if ((C1.w[1] > 0x0001ed09bead87c0ull)
    || (C1.w[1] == 0x0001ed09bead87c0ull
	&& (C1.w[0] > 0x378d8e63ffffffffull))
    || ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull)) {
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else if ((C1.w[1] == 0x0ull) && (C1.w[0] == 0x0ull)) {
  // x is 0
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else {	// x is not special and is not zero

  // if n < 0 then n cannot be converted to uint64 with RM
  if (x_sign) {	// if n < 0 and q + exp = 20
    // too large if c(0)c(1)...c(19).c(20)...c(q-1) > 0
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
    BID_RETURN (res);
  }
  // q = nr. of decimal digits in x
  //  determine first the nr. of bits in x
  if (C1.w[1] == 0) {
    if (C1.w[0] >= 0x0020000000000000ull) {	// x >= 2^53
      // split the 64-bit value in two 32-bit halves to avoid rounding errors
      if (C1.w[0] >= 0x0000000100000000ull) {	// x >= 2^32
	tmp1.d = (double) (C1.w[0] >> 32);	// exact conversion
	x_nr_bits =
	  33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      } else {	// x < 2^32
	tmp1.d = (double) (C1.w[0]);	// exact conversion
	x_nr_bits =
	  1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      }
    } else {	// if x < 2^53
      tmp1.d = (double) C1.w[0];	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// C1.w[1] != 0 => nr. bits = 64 + nr_bits (C1.w[1])
    tmp1.d = (double) C1.w[1];	// exact conversion
    x_nr_bits =
      65 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1.w[1] > nr_digits[x_nr_bits - 1].threshold_hi
	|| (C1.w[1] == nr_digits[x_nr_bits - 1].threshold_hi
	    && C1.w[0] >= nr_digits[x_nr_bits - 1].threshold_lo))
      q++;
  }
  exp = (x_exp >> 49) - 6176;
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
    // if n > 0 and q + exp = 20
    // if n >= 2^64 then n is too large
    // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64
    // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64
    // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*2^65
    // <=> C * 10^(21-q) >= 0xa0000000000000000, 1<=q<=34
    if (q == 1) {
      // C * 10^20 >= 0xa0000000000000000
      __mul_128x64_to_128 (C, C1.w[0], ten2k128[0]);	// 10^20 * C
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
    } else if (q <= 19) {
      // C * 10^(21-q) >= 0xa0000000000000000
      __mul_64x64_to_128MACH (C, C1.w[0], ten2k64[21 - q]);
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
    } else if (q == 20) {
      // C >= 0x10000000000000000
      if (C1.w[1] >= 0x01) {
	// actually C1.w[1] == 0x01 && C1.w[0] >= 0x0000000000000000ull) {
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
      // else cases that can be rounded to a 64-bit int fall through
      // to '1 <= q + exp <= 20'
    } else if (q == 21) {
      // C >= 0xa0000000000000000
      if (C1.w[1] >= 0x0a) {
	// actually C1.w[1] == 0x0a && C1.w[0] >= 0x0000000000000000ull) {
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
      // else cases that can be rounded to a 64-bit int fall through
      // to '1 <= q + exp <= 20'
    } else {	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
      // C  >= 10^(q-21) * 0xa0000000000000000 max 44 bits x 68 bits
      C.w[1] = 0x0a;
      C.w[0] = 0x0000000000000000ull;
      __mul_128x64_to_128 (C, ten2k64[q - 21], C);
      if (C1.w[1] > C.w[1] || (C1.w[1] == C.w[1] && C1.w[0] >= C.w[0])) {
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
  // n is not too large to be converted to int64 if 0 <= n < 2^64
  // Note: some of the cases tested for above fall through to this point
  if ((q + exp) <= 0) {	// n = +0.[0...0]c(0)c(1)...c(q-1)
    // set inexact flag
    *pfpsf |= INEXACT_EXCEPTION;
    // return 0
    res = 0x0000000000000000ull;
    BID_RETURN (res);
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 34, -33 <= exp <= 19)
    // 1 <= x < 2^64 so x can be rounded
    // down to a 64-bit unsigned signed integer
    if (exp < 0) {	// 2 <= q <= 34, -33 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 33; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 fits in 127 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 33
      // kx = 10^(-x) = ten2mk128[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 118 bits
      __mul_128x128_to_256 (P256, C1, ten2mk128[ind - 1]);
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[1] = P256.w[3];
	Cstar.w[0] = P256.w[2];
	fstar.w[3] = 0;
	fstar.w[2] = P256.w[2] & maskhigh128[ind - 1];
	fstar.w[1] = P256.w[1];
	fstar.w[0] = P256.w[0];
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[1] = 0;
	Cstar.w[0] = P256.w[3];
	fstar.w[3] = P256.w[3] & maskhigh128[ind - 1];
	fstar.w[2] = P256.w[2];
	fstar.w[1] = P256.w[1];
	fstar.w[0] = P256.w[0];
      }
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind], e.g.
      // if x=1, T*=ten2mk128trunc[0]=0x19999999999999999999999999999999
      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //     correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-128 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 102
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[0] =
	  (Cstar.w[0] >> shift) | (Cstar.w[1] << (64 - shift));
	// redundant, it will be 0! Cstar.w[1] = (Cstar.w[1] >> shift);
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[0] = (Cstar.w[0] >> (shift - 64));	// 2 <= shift - 64 <= 38
      }
      // determine inexactness of the rounding of C*
      // if (0 < f* < 10^(-x)) then
      //   the result is exact
      // else // if (f* > T*) then
      //   the result is inexact
      if (ind - 1 <= 2) {
	if (fstar.w[1] > ten2mk128trunc[ind - 1].w[1] ||
	    (fstar.w[1] == ten2mk128trunc[ind - 1].w[1] &&
	     fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      } else if (ind - 1 <= 21) {	// if 3 <= ind <= 21
	if (fstar.w[2] || fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	    || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		&& fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      } else {	// if 22 <= ind <= 33
	if (fstar.w[3] || fstar.w[2]
	    || fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	    || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		&& fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      }

      res = Cstar.w[0];	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 20, but x < 2^64 - 1/2 so in this case C1.w[1] has to be 0
      // res = C (exact)
      res = C1.w[0];
    } else {
      // if (exp > 0) => 1 <= exp <= 19, 1 <= q < 19, 2 <= q + exp <= 20
      // res = C * 10^exp (exact) - must fit in 64 bits
      res = C1.w[0] * ten2k64[exp];
    }
  }
}

BID_RETURN (res);
}

/*****************************************************************************
 *  BID128_to_uint64_ceil
 ****************************************************************************/

BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE (UINT64, bid128_to_uint64_ceil,
					  x)

     UINT64 res;
     UINT64 x_sign;
     UINT64 x_exp;
     int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
     BID_UI64DOUBLE tmp1;
     unsigned int x_nr_bits;
     int q, ind, shift;
     UINT128 C1, C;
     UINT128 Cstar;		// C* represents up to 34 decimal digits ~ 113 bits
     UINT256 fstar;
     UINT256 P256;

  // unpack x
x_sign = x.w[1] & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
x_exp = x.w[1] & MASK_EXP;	// biased and shifted left 49 bit positions
C1.w[1] = x.w[1] & MASK_COEFF;
C1.w[0] = x.w[0];

  // check for NaN or Infinity
if ((x.w[1] & MASK_SPECIAL) == MASK_SPECIAL) {
    // x is special
if ((x.w[1] & MASK_NAN) == MASK_NAN) {	// x is NAN
  if ((x.w[1] & MASK_SNAN) == MASK_SNAN) {	// x is SNAN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is QNaN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
} else {	// x is not a NaN, so it must be infinity
  if (!x_sign) {	// x is +inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is -inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
}
}
  // check for non-canonical values (after the check for special values)
if ((C1.w[1] > 0x0001ed09bead87c0ull)
    || (C1.w[1] == 0x0001ed09bead87c0ull
	&& (C1.w[0] > 0x378d8e63ffffffffull))
    || ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull)) {
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else if ((C1.w[1] == 0x0ull) && (C1.w[0] == 0x0ull)) {
  // x is 0
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else {	// x is not special and is not zero

  // q = nr. of decimal digits in x
  //  determine first the nr. of bits in x
  if (C1.w[1] == 0) {
    if (C1.w[0] >= 0x0020000000000000ull) {	// x >= 2^53
      // split the 64-bit value in two 32-bit halves to avoid rounding errors
      if (C1.w[0] >= 0x0000000100000000ull) {	// x >= 2^32
	tmp1.d = (double) (C1.w[0] >> 32);	// exact conversion
	x_nr_bits =
	  33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      } else {	// x < 2^32
	tmp1.d = (double) (C1.w[0]);	// exact conversion
	x_nr_bits =
	  1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      }
    } else {	// if x < 2^53
      tmp1.d = (double) C1.w[0];	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// C1.w[1] != 0 => nr. bits = 64 + nr_bits (C1.w[1])
    tmp1.d = (double) C1.w[1];	// exact conversion
    x_nr_bits =
      65 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1.w[1] > nr_digits[x_nr_bits - 1].threshold_hi
	|| (C1.w[1] == nr_digits[x_nr_bits - 1].threshold_hi
	    && C1.w[0] >= nr_digits[x_nr_bits - 1].threshold_lo))
      q++;
  }
  exp = (x_exp >> 49) - 6176;
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
    if (x_sign) {	// if n < 0 and q + exp = 20
      // if n <= -1 then n cannot be converted to uint64 with RZ
      // too large if c(0)c(1)...c(19).c(20)...c(q-1) >= 1
      // <=> 0.c(0)c(1)...c(q-1) * 10^21 >= 0x0a, 1<=q<=34
      // <=> C * 10^(21-q) >= 0x0a, 1<=q<=34
      if (q == 21) {
	// C >= a 
	if (C1.w[1] != 0 || C1.w[0] >= 0x0aull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to 64-bit unsigned int fall through
	// to '1 <= q + exp <= 20'
      } else {
	// if 1 <= q <= 20
	//   C * 10^(21-q) >= a is true because C >= 1 and 10^(21-q) >= 10
	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	//  C >= a * 10^(q-21) is true because C > 2^64 and a*10^(q-21) < 2^64
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
    } else {	// if n > 0 and q + exp = 20
      // if n > 2^64 - 1 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) > 2^64 - 1
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 > 2^64 - 1
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 > 10 * (2^64 - 1) 
      // <=> C * 10^(21-q) > 0x9fffffffffffffff6, 1<=q<=34
      if (q == 1) {
	// C * 10^20 > 0x9fffffffffffffff6
	__mul_128x64_to_128 (C, C1.w[0], ten2k128[0]);	// 10^20 * C
	if (C.w[1] > 0x09 || (C.w[1] == 0x09
			      && C.w[0] > 0xfffffffffffffff6ull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q <= 19) {
	// C * 10^(21-q) > 0x9fffffffffffffff6
	__mul_64x64_to_128MACH (C, C1.w[0], ten2k64[21 - q]);
	if (C.w[1] > 0x09 || (C.w[1] == 0x09
			      && C.w[0] > 0xfffffffffffffff6ull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q == 20) {
	// C > 0xffffffffffffffff
	if (C1.w[1]) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q == 21) {
	// C > 0x9fffffffffffffff6
	if (C1.w[1] > 0x09 || (C1.w[1] == 0x09
			       && C1.w[0] > 0xfffffffffffffff6ull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	// C  > 10^(q-21) * 0x9fffffffffffffff6 max 44 bits x 68 bits
	C.w[1] = 0x09;
	C.w[0] = 0xfffffffffffffff6ull;
	__mul_128x64_to_128 (C, ten2k64[q - 21], C);
	if (C1.w[1] > C.w[1] || (C1.w[1] == C.w[1] && C1.w[0] > C.w[0])) {
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
  // n is not too large to be converted to int64 if -1 < n <= 2^64 - 1
  // Note: some of the cases tested for above fall through to this point
  if ((q + exp) <= 0) {	// n = +/-0.[0...0]c(0)c(1)...c(q-1)
    // return 0 or 1
    if (x_sign)
      res = 0x0000000000000000ull;
    else
      res = 0x0000000000000001ull;
    BID_RETURN (res);
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 34, -33 <= exp <= 19)
    // x <= -1 or 1 <= x < 2^64 so if positive x can be rounded
    // to zero to a 64-bit unsigned signed integer
    if (x_sign) {	// x <= -1
      // set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    }
    // 1 <= x <= 2^64 - 1 so x can be rounded
    // to zero to a 64-bit unsigned integer
    if (exp < 0) {	// 2 <= q <= 34, -33 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 33; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 fits in 127 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 33
      // kx = 10^(-x) = ten2mk128[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 118 bits
      __mul_128x128_to_256 (P256, C1, ten2mk128[ind - 1]);
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[1] = P256.w[3];
	Cstar.w[0] = P256.w[2];
	fstar.w[3] = 0;
	fstar.w[2] = P256.w[2] & maskhigh128[ind - 1];
	fstar.w[1] = P256.w[1];
	fstar.w[0] = P256.w[0];
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[1] = 0;
	Cstar.w[0] = P256.w[3];
	fstar.w[3] = P256.w[3] & maskhigh128[ind - 1];
	fstar.w[2] = P256.w[2];
	fstar.w[1] = P256.w[1];
	fstar.w[0] = P256.w[0];
      }
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind], e.g.
      // if x=1, T*=ten2mk128trunc[0]=0x19999999999999999999999999999999
      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //     correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-128 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 102
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[0] =
	  (Cstar.w[0] >> shift) | (Cstar.w[1] << (64 - shift));
	// redundant, it will be 0! Cstar.w[1] = (Cstar.w[1] >> shift);
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[0] = (Cstar.w[0] >> (shift - 64));	// 2 <= shift - 64 <= 38
      }
      // if the result is positive and inexact, need to add 1 to it

      // determine inexactness of the rounding of C*
      // if (0 < f* < 10^(-x)) then
      //   the result is exact
      // else // if (f* > T*) then
      //   the result is inexact
      if (ind - 1 <= 2) {
	if (fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	    || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		&& fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	  if (!x_sign) {	// positive and inexact
	    Cstar.w[0]++;
	    if (Cstar.w[0] == 0x0)
	      Cstar.w[1]++;
	  }
	}	// else the result is exact
      } else if (ind - 1 <= 21) {	// if 3 <= ind <= 21
	if (fstar.w[2] || fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	    || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		&& fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	  if (!x_sign) {	// positive and inexact
	    Cstar.w[0]++;
	    if (Cstar.w[0] == 0x0)
	      Cstar.w[1]++;
	  }
	}	// else the result is exact
      } else {	// if 22 <= ind <= 33
	if (fstar.w[3] || fstar.w[2]
	    || fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	    || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		&& fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	  if (!x_sign) {	// positive and inexact
	    Cstar.w[0]++;
	    if (Cstar.w[0] == 0x0)
	      Cstar.w[1]++;
	  }
	}	// else the result is exact
      }

      res = Cstar.w[0];	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 20, but x < 2^64 - 1/2 so in this case C1.w[1] has to be 0
      // res = C (exact)
      res = C1.w[0];
    } else {
      // if (exp > 0) => 1 <= exp <= 19, 1 <= q < 19, 2 <= q + exp <= 20
      // res = C * 10^exp (exact) - must fit in 64 bits
      res = C1.w[0] * ten2k64[exp];
    }
  }
}

BID_RETURN (res);
}

/*****************************************************************************
 *  BID128_to_uint64_xceil
 ****************************************************************************/

BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE (UINT64,
					  bid128_to_uint64_xceil, x)

     UINT64 res;
     UINT64 x_sign;
     UINT64 x_exp;
     int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
     BID_UI64DOUBLE tmp1;
     unsigned int x_nr_bits;
     int q, ind, shift;
     UINT128 C1, C;
     UINT128 Cstar;		// C* represents up to 34 decimal digits ~ 113 bits
     UINT256 fstar;
     UINT256 P256;

  // unpack x
x_sign = x.w[1] & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
x_exp = x.w[1] & MASK_EXP;	// biased and shifted left 49 bit positions
C1.w[1] = x.w[1] & MASK_COEFF;
C1.w[0] = x.w[0];

  // check for NaN or Infinity
if ((x.w[1] & MASK_SPECIAL) == MASK_SPECIAL) {
    // x is special
if ((x.w[1] & MASK_NAN) == MASK_NAN) {	// x is NAN
  if ((x.w[1] & MASK_SNAN) == MASK_SNAN) {	// x is SNAN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is QNaN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
} else {	// x is not a NaN, so it must be infinity
  if (!x_sign) {	// x is +inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is -inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
}
}
  // check for non-canonical values (after the check for special values)
if ((C1.w[1] > 0x0001ed09bead87c0ull)
    || (C1.w[1] == 0x0001ed09bead87c0ull
	&& (C1.w[0] > 0x378d8e63ffffffffull))
    || ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull)) {
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else if ((C1.w[1] == 0x0ull) && (C1.w[0] == 0x0ull)) {
  // x is 0
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else {	// x is not special and is not zero

  // q = nr. of decimal digits in x
  //  determine first the nr. of bits in x
  if (C1.w[1] == 0) {
    if (C1.w[0] >= 0x0020000000000000ull) {	// x >= 2^53
      // split the 64-bit value in two 32-bit halves to avoid rounding errors
      if (C1.w[0] >= 0x0000000100000000ull) {	// x >= 2^32
	tmp1.d = (double) (C1.w[0] >> 32);	// exact conversion
	x_nr_bits =
	  33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      } else {	// x < 2^32
	tmp1.d = (double) (C1.w[0]);	// exact conversion
	x_nr_bits =
	  1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      }
    } else {	// if x < 2^53
      tmp1.d = (double) C1.w[0];	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// C1.w[1] != 0 => nr. bits = 64 + nr_bits (C1.w[1])
    tmp1.d = (double) C1.w[1];	// exact conversion
    x_nr_bits =
      65 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1.w[1] > nr_digits[x_nr_bits - 1].threshold_hi
	|| (C1.w[1] == nr_digits[x_nr_bits - 1].threshold_hi
	    && C1.w[0] >= nr_digits[x_nr_bits - 1].threshold_lo))
      q++;
  }
  exp = (x_exp >> 49) - 6176;
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
    if (x_sign) {	// if n < 0 and q + exp = 20
      // if n <= -1 then n cannot be converted to uint64 with RZ
      // too large if c(0)c(1)...c(19).c(20)...c(q-1) >= 1
      // <=> 0.c(0)c(1)...c(q-1) * 10^21 >= 0x0a, 1<=q<=34
      // <=> C * 10^(21-q) >= 0x0a, 1<=q<=34
      if (q == 21) {
	// C >= a 
	if (C1.w[1] != 0 || C1.w[0] >= 0x0aull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to 64-bit unsigned int fall through
	// to '1 <= q + exp <= 20'
      } else {
	// if 1 <= q <= 20
	//   C * 10^(21-q) >= a is true because C >= 1 and 10^(21-q) >= 10
	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	//  C >= a * 10^(q-21) is true because C > 2^64 and a*10^(q-21) < 2^64
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
    } else {	// if n > 0 and q + exp = 20
      // if n > 2^64 - 1 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) > 2^64 - 1
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 > 2^64 - 1
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 > 10 * (2^64 - 1) 
      // <=> C * 10^(21-q) > 0x9fffffffffffffff6, 1<=q<=34
      if (q == 1) {
	// C * 10^20 > 0x9fffffffffffffff6
	__mul_128x64_to_128 (C, C1.w[0], ten2k128[0]);	// 10^20 * C
	if (C.w[1] > 0x09 || (C.w[1] == 0x09
			      && C.w[0] > 0xfffffffffffffff6ull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q <= 19) {
	// C * 10^(21-q) > 0x9fffffffffffffff6
	__mul_64x64_to_128MACH (C, C1.w[0], ten2k64[21 - q]);
	if (C.w[1] > 0x09 || (C.w[1] == 0x09
			      && C.w[0] > 0xfffffffffffffff6ull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q == 20) {
	// C > 0xffffffffffffffff
	if (C1.w[1]) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q == 21) {
	// C > 0x9fffffffffffffff6
	if (C1.w[1] > 0x09 || (C1.w[1] == 0x09
			       && C1.w[0] > 0xfffffffffffffff6ull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	// C  > 10^(q-21) * 0x9fffffffffffffff6 max 44 bits x 68 bits
	C.w[1] = 0x09;
	C.w[0] = 0xfffffffffffffff6ull;
	__mul_128x64_to_128 (C, ten2k64[q - 21], C);
	if (C1.w[1] > C.w[1] || (C1.w[1] == C.w[1] && C1.w[0] > C.w[0])) {
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
  // n is not too large to be converted to int64 if -1 < n <= 2^64 - 1
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
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 34, -33 <= exp <= 19)
    // x <= -1 or 1 <= x < 2^64 so if positive x can be rounded
    // to zero to a 64-bit unsigned signed integer
    if (x_sign) {	// x <= -1
      // set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    }
    // 1 <= x <= 2^64 - 1 so x can be rounded
    // to zero to a 64-bit unsigned integer
    if (exp < 0) {	// 2 <= q <= 34, -33 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 33; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 fits in 127 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 33
      // kx = 10^(-x) = ten2mk128[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 118 bits
      __mul_128x128_to_256 (P256, C1, ten2mk128[ind - 1]);
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[1] = P256.w[3];
	Cstar.w[0] = P256.w[2];
	fstar.w[3] = 0;
	fstar.w[2] = P256.w[2] & maskhigh128[ind - 1];
	fstar.w[1] = P256.w[1];
	fstar.w[0] = P256.w[0];
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[1] = 0;
	Cstar.w[0] = P256.w[3];
	fstar.w[3] = P256.w[3] & maskhigh128[ind - 1];
	fstar.w[2] = P256.w[2];
	fstar.w[1] = P256.w[1];
	fstar.w[0] = P256.w[0];
      }
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind], e.g.
      // if x=1, T*=ten2mk128trunc[0]=0x19999999999999999999999999999999
      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //     correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-128 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 102
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[0] =
	  (Cstar.w[0] >> shift) | (Cstar.w[1] << (64 - shift));
	// redundant, it will be 0! Cstar.w[1] = (Cstar.w[1] >> shift);
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[0] = (Cstar.w[0] >> (shift - 64));	// 2 <= shift - 64 <= 38
      }
      // if the result is positive and inexact, need to add 1 to it

      // determine inexactness of the rounding of C*
      // if (0 < f* < 10^(-x)) then
      //   the result is exact
      // else // if (f* > T*) then
      //   the result is inexact
      if (ind - 1 <= 2) {
	if (fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	    || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		&& fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	  if (!x_sign) {	// positive and inexact
	    Cstar.w[0]++;
	    if (Cstar.w[0] == 0x0)
	      Cstar.w[1]++;
	  }
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      } else if (ind - 1 <= 21) {	// if 3 <= ind <= 21
	if (fstar.w[2] || fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	    || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		&& fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	  if (!x_sign) {	// positive and inexact
	    Cstar.w[0]++;
	    if (Cstar.w[0] == 0x0)
	      Cstar.w[1]++;
	  }
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      } else {	// if 22 <= ind <= 33
	if (fstar.w[3] || fstar.w[2]
	    || fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	    || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		&& fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	  if (!x_sign) {	// positive and inexact
	    Cstar.w[0]++;
	    if (Cstar.w[0] == 0x0)
	      Cstar.w[1]++;
	  }
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      }

      res = Cstar.w[0];	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 20, but x < 2^64 - 1/2 so in this case C1.w[1] has to be 0
      // res = C (exact)
      res = C1.w[0];
    } else {
      // if (exp > 0) => 1 <= exp <= 19, 1 <= q < 19, 2 <= q + exp <= 20
      // res = C * 10^exp (exact) - must fit in 64 bits
      res = C1.w[0] * ten2k64[exp];
    }
  }
}

BID_RETURN (res);
}

/*****************************************************************************
 *  BID128_to_uint64_int
 ****************************************************************************/

BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE (UINT64, bid128_to_uint64_int,
					  x)

     UINT64 res;
     UINT64 x_sign;
     UINT64 x_exp;
     int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
     BID_UI64DOUBLE tmp1;
     unsigned int x_nr_bits;
     int q, ind, shift;
     UINT128 C1, C;
     UINT128 Cstar;		// C* represents up to 34 decimal digits ~ 113 bits
     UINT256 P256;

  // unpack x
x_sign = x.w[1] & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
x_exp = x.w[1] & MASK_EXP;	// biased and shifted left 49 bit positions
C1.w[1] = x.w[1] & MASK_COEFF;
C1.w[0] = x.w[0];

  // check for NaN or Infinity
if ((x.w[1] & MASK_SPECIAL) == MASK_SPECIAL) {
    // x is special
if ((x.w[1] & MASK_NAN) == MASK_NAN) {	// x is NAN
  if ((x.w[1] & MASK_SNAN) == MASK_SNAN) {	// x is SNAN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is QNaN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
} else {	// x is not a NaN, so it must be infinity
  if (!x_sign) {	// x is +inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is -inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
}
}
  // check for non-canonical values (after the check for special values)
if ((C1.w[1] > 0x0001ed09bead87c0ull)
    || (C1.w[1] == 0x0001ed09bead87c0ull
	&& (C1.w[0] > 0x378d8e63ffffffffull))
    || ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull)) {
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else if ((C1.w[1] == 0x0ull) && (C1.w[0] == 0x0ull)) {
  // x is 0
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else {	// x is not special and is not zero

  // q = nr. of decimal digits in x
  //  determine first the nr. of bits in x
  if (C1.w[1] == 0) {
    if (C1.w[0] >= 0x0020000000000000ull) {	// x >= 2^53
      // split the 64-bit value in two 32-bit halves to avoid rounding errors
      if (C1.w[0] >= 0x0000000100000000ull) {	// x >= 2^32
	tmp1.d = (double) (C1.w[0] >> 32);	// exact conversion
	x_nr_bits =
	  33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      } else {	// x < 2^32
	tmp1.d = (double) (C1.w[0]);	// exact conversion
	x_nr_bits =
	  1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      }
    } else {	// if x < 2^53
      tmp1.d = (double) C1.w[0];	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// C1.w[1] != 0 => nr. bits = 64 + nr_bits (C1.w[1])
    tmp1.d = (double) C1.w[1];	// exact conversion
    x_nr_bits =
      65 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1.w[1] > nr_digits[x_nr_bits - 1].threshold_hi
	|| (C1.w[1] == nr_digits[x_nr_bits - 1].threshold_hi
	    && C1.w[0] >= nr_digits[x_nr_bits - 1].threshold_lo))
      q++;
  }
  exp = (x_exp >> 49) - 6176;

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
    if (x_sign) {	// if n < 0 and q + exp = 20
      // if n <= -1 then n cannot be converted to uint64 with RZ
      // too large if c(0)c(1)...c(19).c(20)...c(q-1) >= 1
      // <=> 0.c(0)c(1)...c(q-1) * 10^21 >= 0x0a, 1<=q<=34
      // <=> C * 10^(21-q) >= 0x0a, 1<=q<=34
      if (q == 21) {
	// C >= a 
	if (C1.w[1] != 0 || C1.w[0] >= 0x0aull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to 64-bit unsigned int fall through
	// to '1 <= q + exp <= 20'
      } else {
	// if 1 <= q <= 20
	//   C * 10^(21-q) >= a is true because C >= 1 and 10^(21-q) >= 10
	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	//  C >= a * 10^(q-21) is true because C > 2^64 and a*10^(q-21) < 2^64
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
    } else {	// if n > 0 and q + exp = 20
      // if n >= 2^64 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*2^65
      // <=> C * 10^(21-q) >= 0xa0000000000000000, 1<=q<=34
      if (q == 1) {
	// C * 10^20 >= 0xa0000000000000000
	__mul_128x64_to_128 (C, C1.w[0], ten2k128[0]);	// 10^20 * C
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
      } else if (q <= 19) {
	// C * 10^(21-q) >= 0xa0000000000000000
	__mul_64x64_to_128MACH (C, C1.w[0], ten2k64[21 - q]);
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
      } else if (q == 20) {
	// C >= 0x10000000000000000
	if (C1.w[1] >= 0x01) {
	  // actually C1.w[1] == 0x01 && C1.w[0] >= 0x0000000000000000ull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q == 21) {
	// C >= 0xa0000000000000000
	if (C1.w[1] >= 0x0a) {
	  // actually C1.w[1] == 0x0a && C1.w[0] >= 0x0000000000000000ull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	// C  >= 10^(q-21) * 0xa0000000000000000 max 44 bits x 68 bits
	C.w[1] = 0x0a;
	C.w[0] = 0x0000000000000000ull;
	__mul_128x64_to_128 (C, ten2k64[q - 21], C);
	if (C1.w[1] > C.w[1]
	    || (C1.w[1] == C.w[1] && C1.w[0] >= C.w[0])) {
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
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 34, -33 <= exp <= 19)
    // x <= -1 or 1 <= x < 2^64 so if positive x can be rounded
    // to zero to a 64-bit unsigned signed integer
    if (x_sign) {	// x <= -1
      // set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    }
    // 1 <= x < 2^64 so x can be rounded
    // to zero to a 64-bit unsigned integer
    if (exp < 0) {	// 2 <= q <= 34, -33 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 33; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 fits in 127 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 33
      // kx = 10^(-x) = ten2mk128[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 118 bits
      __mul_128x128_to_256 (P256, C1, ten2mk128[ind - 1]);
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[1] = P256.w[3];
	Cstar.w[0] = P256.w[2];
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[1] = 0;
	Cstar.w[0] = P256.w[3];
      }
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind], e.g.
      // if x=1, T*=ten2mk128trunc[0]=0x19999999999999999999999999999999
      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //     correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-128 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 102
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[0] =
	  (Cstar.w[0] >> shift) | (Cstar.w[1] << (64 - shift));
	// redundant, it will be 0! Cstar.w[1] = (Cstar.w[1] >> shift);
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[0] = (Cstar.w[0] >> (shift - 64));	// 2 <= shift - 64 <= 38
      }
      res = Cstar.w[0];	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 20, but x < 2^64 - 1/2 so in this case C1.w[1] has to be 0
      // res = C (exact)
      res = C1.w[0];
    } else {
      // if (exp > 0) => 1 <= exp <= 19, 1 <= q < 19, 2 <= q + exp <= 20
      // res = C * 10^exp (exact) - must fit in 64 bits
      res = C1.w[0] * ten2k64[exp];
    }
  }
}

BID_RETURN (res);
}

/*****************************************************************************
 *  BID128_to_uint64_xint
 ****************************************************************************/

BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE (UINT64, bid128_to_uint64_xint,
					  x)

     UINT64 res;
     UINT64 x_sign;
     UINT64 x_exp;
     int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
     BID_UI64DOUBLE tmp1;
     unsigned int x_nr_bits;
     int q, ind, shift;
     UINT128 C1, C;
     UINT128 Cstar;		// C* represents up to 34 decimal digits ~ 113 bits
     UINT256 fstar;
     UINT256 P256;

  // unpack x
x_sign = x.w[1] & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
x_exp = x.w[1] & MASK_EXP;	// biased and shifted left 49 bit positions
C1.w[1] = x.w[1] & MASK_COEFF;
C1.w[0] = x.w[0];

  // check for NaN or Infinity
if ((x.w[1] & MASK_SPECIAL) == MASK_SPECIAL) {
    // x is special
if ((x.w[1] & MASK_NAN) == MASK_NAN) {	// x is NAN
  if ((x.w[1] & MASK_SNAN) == MASK_SNAN) {	// x is SNAN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is QNaN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
} else {	// x is not a NaN, so it must be infinity
  if (!x_sign) {	// x is +inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is -inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
}
}
  // check for non-canonical values (after the check for special values)
if ((C1.w[1] > 0x0001ed09bead87c0ull)
    || (C1.w[1] == 0x0001ed09bead87c0ull
	&& (C1.w[0] > 0x378d8e63ffffffffull))
    || ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull)) {
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else if ((C1.w[1] == 0x0ull) && (C1.w[0] == 0x0ull)) {
  // x is 0
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else {	// x is not special and is not zero

  // q = nr. of decimal digits in x
  //  determine first the nr. of bits in x
  if (C1.w[1] == 0) {
    if (C1.w[0] >= 0x0020000000000000ull) {	// x >= 2^53
      // split the 64-bit value in two 32-bit halves to avoid rounding errors
      if (C1.w[0] >= 0x0000000100000000ull) {	// x >= 2^32
	tmp1.d = (double) (C1.w[0] >> 32);	// exact conversion
	x_nr_bits =
	  33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      } else {	// x < 2^32
	tmp1.d = (double) (C1.w[0]);	// exact conversion
	x_nr_bits =
	  1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      }
    } else {	// if x < 2^53
      tmp1.d = (double) C1.w[0];	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// C1.w[1] != 0 => nr. bits = 64 + nr_bits (C1.w[1])
    tmp1.d = (double) C1.w[1];	// exact conversion
    x_nr_bits =
      65 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1.w[1] > nr_digits[x_nr_bits - 1].threshold_hi
	|| (C1.w[1] == nr_digits[x_nr_bits - 1].threshold_hi
	    && C1.w[0] >= nr_digits[x_nr_bits - 1].threshold_lo))
      q++;
  }
  exp = (x_exp >> 49) - 6176;
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
    if (x_sign) {	// if n < 0 and q + exp = 20
      // if n <= -1 then n cannot be converted to uint64 with RZ
      // too large if c(0)c(1)...c(19).c(20)...c(q-1) >= 1
      // <=> 0.c(0)c(1)...c(q-1) * 10^21 >= 0x0a, 1<=q<=34
      // <=> C * 10^(21-q) >= 0x0a, 1<=q<=34
      if (q == 21) {
	// C >= a 
	if (C1.w[1] != 0 || C1.w[0] >= 0x0aull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to 64-bit unsigned int fall through
	// to '1 <= q + exp <= 20'
      } else {
	// if 1 <= q <= 20
	//   C * 10^(21-q) >= a is true because C >= 1 and 10^(21-q) >= 10
	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	//  C >= a * 10^(q-21) is true because C > 2^64 and a*10^(q-21) < 2^64
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
    } else {	// if n > 0 and q + exp = 20
      // if n >= 2^64 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*2^65
      // <=> C * 10^(21-q) >= 0xa0000000000000000, 1<=q<=34
      if (q == 1) {
	// C * 10^20 >= 0xa0000000000000000
	__mul_128x64_to_128 (C, C1.w[0], ten2k128[0]);	// 10^20 * C
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
      } else if (q <= 19) {
	// C * 10^(21-q) >= 0xa0000000000000000
	__mul_64x64_to_128MACH (C, C1.w[0], ten2k64[21 - q]);
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
      } else if (q == 20) {
	// C >= 0x10000000000000000
	if (C1.w[1] >= 0x01) {
	  // actually C1.w[1] == 0x01 && C1.w[0] >= 0x0000000000000000ull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q == 21) {
	// C >= 0xa0000000000000000
	if (C1.w[1] >= 0x0a) {
	  // actually C1.w[1] == 0x0a && C1.w[0] >= 0x0000000000000000ull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	// C  >= 10^(q-21) * 0xa0000000000000000 max 44 bits x 68 bits
	C.w[1] = 0x0a;
	C.w[0] = 0x0000000000000000ull;
	__mul_128x64_to_128 (C, ten2k64[q - 21], C);
	if (C1.w[1] > C.w[1]
	    || (C1.w[1] == C.w[1] && C1.w[0] >= C.w[0])) {
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
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 34, -33 <= exp <= 19)
    // x <= -1 or 1 <= x < 2^64 so if positive x can be rounded
    // to zero to a 64-bit unsigned signed integer
    if (x_sign) {	// x <= -1
      // set invalid flag
      *pfpsf |= INVALID_EXCEPTION;
      // return Integer Indefinite
      res = 0x8000000000000000ull;
      BID_RETURN (res);
    }
    // 1 <= x < 2^64 so x can be rounded
    // to zero to a 64-bit unsigned integer
    if (exp < 0) {	// 2 <= q <= 34, -33 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 33; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 fits in 127 bits
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 33
      // kx = 10^(-x) = ten2mk128[ind - 1]
      // C* = C1 * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 118 bits
      __mul_128x128_to_256 (P256, C1, ten2mk128[ind - 1]);
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[1] = P256.w[3];
	Cstar.w[0] = P256.w[2];
	fstar.w[3] = 0;
	fstar.w[2] = P256.w[2] & maskhigh128[ind - 1];
	fstar.w[1] = P256.w[1];
	fstar.w[0] = P256.w[0];
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[1] = 0;
	Cstar.w[0] = P256.w[3];
	fstar.w[3] = P256.w[3] & maskhigh128[ind - 1];
	fstar.w[2] = P256.w[2];
	fstar.w[1] = P256.w[1];
	fstar.w[0] = P256.w[0];
      }
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind], e.g.
      // if x=1, T*=ten2mk128trunc[0]=0x19999999999999999999999999999999
      // C* = floor(C*) (logical right shift; C has p decimal digits,
      //     correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-128 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 102
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[0] =
	  (Cstar.w[0] >> shift) | (Cstar.w[1] << (64 - shift));
	// redundant, it will be 0! Cstar.w[1] = (Cstar.w[1] >> shift);
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[0] = (Cstar.w[0] >> (shift - 64));	// 2 <= shift - 64 <= 38
      }
      // determine inexactness of the rounding of C*
      // if (0 < f* < 10^(-x)) then
      //   the result is exact
      // else // if (f* > T*) then
      //   the result is inexact
      if (ind - 1 <= 2) {
	if (fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	    || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		&& fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      } else if (ind - 1 <= 21) {	// if 3 <= ind <= 21
	if (fstar.w[2] || fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	    || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		&& fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      } else {	// if 22 <= ind <= 33
	if (fstar.w[3] || fstar.w[2]
	    || fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	    || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		&& fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}	// else the result is exact
      }

      res = Cstar.w[0];	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 20, but x < 2^64 - 1/2 so in this case C1.w[1] has to be 0
      // res = C (exact)
      res = C1.w[0];
    } else {
      // if (exp > 0) => 1 <= exp <= 19, 1 <= q < 19, 2 <= q + exp <= 20
      // res = C * 10^exp (exact) - must fit in 64 bits
      res = C1.w[0] * ten2k64[exp];
    }
  }
}

BID_RETURN (res);
}

/*****************************************************************************
 *  BID128_to_uint64_rninta
 ****************************************************************************/

BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE (UINT64,
					  bid128_to_uint64_rninta, x)

     UINT64 res;
     UINT64 x_sign;
     UINT64 x_exp;
     int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
     UINT64 tmp64;
     BID_UI64DOUBLE tmp1;
     unsigned int x_nr_bits;
     int q, ind, shift;
     UINT128 C1, C;
     UINT128 Cstar;		// C* represents up to 34 decimal digits ~ 113 bits
     UINT256 P256;

  // unpack x
x_sign = x.w[1] & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
x_exp = x.w[1] & MASK_EXP;	// biased and shifted left 49 bit positions
C1.w[1] = x.w[1] & MASK_COEFF;
C1.w[0] = x.w[0];

  // check for NaN or Infinity
if ((x.w[1] & MASK_SPECIAL) == MASK_SPECIAL) {
    // x is special
if ((x.w[1] & MASK_NAN) == MASK_NAN) {	// x is NAN
  if ((x.w[1] & MASK_SNAN) == MASK_SNAN) {	// x is SNAN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is QNaN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
} else {	// x is not a NaN, so it must be infinity
  if (!x_sign) {	// x is +inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is -inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
}
}
  // check for non-canonical values (after the check for special values)
if ((C1.w[1] > 0x0001ed09bead87c0ull)
    || (C1.w[1] == 0x0001ed09bead87c0ull
	&& (C1.w[0] > 0x378d8e63ffffffffull))
    || ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull)) {
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else if ((C1.w[1] == 0x0ull) && (C1.w[0] == 0x0ull)) {
  // x is 0
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else {	// x is not special and is not zero

  // q = nr. of decimal digits in x
  //  determine first the nr. of bits in x
  if (C1.w[1] == 0) {
    if (C1.w[0] >= 0x0020000000000000ull) {	// x >= 2^53
      // split the 64-bit value in two 32-bit halves to avoid rounding errors
      if (C1.w[0] >= 0x0000000100000000ull) {	// x >= 2^32
	tmp1.d = (double) (C1.w[0] >> 32);	// exact conversion
	x_nr_bits =
	  33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      } else {	// x < 2^32
	tmp1.d = (double) (C1.w[0]);	// exact conversion
	x_nr_bits =
	  1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      }
    } else {	// if x < 2^53
      tmp1.d = (double) C1.w[0];	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// C1.w[1] != 0 => nr. bits = 64 + nr_bits (C1.w[1])
    tmp1.d = (double) C1.w[1];	// exact conversion
    x_nr_bits =
      65 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1.w[1] > nr_digits[x_nr_bits - 1].threshold_hi
	|| (C1.w[1] == nr_digits[x_nr_bits - 1].threshold_hi
	    && C1.w[0] >= nr_digits[x_nr_bits - 1].threshold_lo))
      q++;
  }
  exp = (x_exp >> 49) - 6176;
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
    if (x_sign) {	// if n < 0 and q + exp = 20
      // if n <= -1/2 then n cannot be converted to uint64 with RN
      // too large if c(0)c(1)...c(19).c(20)...c(q-1) >= 1/2
      // <=> 0.c(0)c(1)...c(q-1) * 10^21 >= 0x05, 1<=q<=34
      // <=> C * 10^(21-q) >= 0x05, 1<=q<=34
      if (q == 21) {
	// C >= 5 
	if (C1.w[1] != 0 || C1.w[0] >= 0x05ull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to 64-bit unsigned int fall through
	// to '1 <= q + exp <= 20'
      } else {
	// if 1 <= q <= 20
	//   C * 10^(21-q) >= 5 is true because C >= 1 and 10^(21-q) >= 10
	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	//  C >= 5 * 10^(q-21) is true because C > 2^64 and 5*10^(q-21) < 2^64
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
    } else {	// if n > 0 and q + exp = 20
      // if n >= 2^64 - 1/2 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*(2^65-1)
      // <=> C * 10^(21-q) >= 0x9fffffffffffffffb, 1<=q<=34
      if (q == 1) {
	// C * 10^20 >= 0x9fffffffffffffffb
	__mul_128x64_to_128 (C, C1.w[0], ten2k128[0]);	// 10^20 * C
	if (C.w[1] > 0x09 || (C.w[1] == 0x09
			      && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q <= 19) {
	// C * 10^(21-q) >= 0x9fffffffffffffffb
	__mul_64x64_to_128MACH (C, C1.w[0], ten2k64[21 - q]);
	if (C.w[1] > 0x09 || (C.w[1] == 0x09
			      && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q == 20) {
	// C * 10 >= 0x9fffffffffffffffb <=> C * 2 > 1ffffffffffffffff
	C.w[0] = C1.w[0] + C1.w[0];
	C.w[1] = C1.w[1] + C1.w[1];
	if (C.w[0] < C1.w[0])
	  C.w[1]++;
	if (C.w[1] > 0x01 || (C.w[1] == 0x01
			      && C.w[0] >= 0xffffffffffffffffull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q == 21) {
	// C >= 0x9fffffffffffffffb
	if (C1.w[1] > 0x09 || (C1.w[1] == 0x09
			       && C1.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	// C  >= 10^(q-21) * 0x9fffffffffffffffb max 44 bits x 68 bits
	C.w[1] = 0x09;
	C.w[0] = 0xfffffffffffffffbull;
	__mul_128x64_to_128 (C, ten2k64[q - 21], C);
	if (C1.w[1] > C.w[1]
	    || (C1.w[1] == C.w[1] && C1.w[0] >= C.w[0])) {
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
  // n is not too large to be converted to int64 if -1/2 < n < 2^64 - 1/2
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
    ind = q - 1;
    if (ind <= 18) {	// 0 <= ind <= 18
      if ((C1.w[1] == 0) && (C1.w[0] < midpoint64[ind])) {
	res = 0x0000000000000000ull;	// return 0
      } else if (!x_sign) {	// n > 0
	res = 0x00000001;	// return +1
      } else {
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
    } else {	// 19 <= ind <= 33
      if ((C1.w[1] < midpoint128[ind - 19].w[1])
	  || ((C1.w[1] == midpoint128[ind - 19].w[1])
	      && (C1.w[0] < midpoint128[ind - 19].w[0]))) {
	res = 0x0000000000000000ull;	// return 0
      } else if (!x_sign) {	// n > 0
	res = 0x00000001;	// return +1
      } else {
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
    }
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 34, -33 <= exp <= 19)
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
    if (exp < 0) {	// 2 <= q <= 34, -33 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 33; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 = C1 + 1/2 * 10^ind where the result C1 fits in 127 bits
      tmp64 = C1.w[0];
      if (ind <= 19) {
	C1.w[0] = C1.w[0] + midpoint64[ind - 1];
      } else {
	C1.w[0] = C1.w[0] + midpoint128[ind - 20].w[0];
	C1.w[1] = C1.w[1] + midpoint128[ind - 20].w[1];
      }
      if (C1.w[0] < tmp64)
	C1.w[1]++;
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 33
      // kx = 10^(-x) = ten2mk128[ind - 1]
      // C* = (C1 + 1/2 * 10^x) * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 118 bits
      __mul_128x128_to_256 (P256, C1, ten2mk128[ind - 1]);
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[1] = P256.w[3];
	Cstar.w[0] = P256.w[2];
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[1] = 0;
	Cstar.w[0] = P256.w[3];
      }
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind], e.g.
      // if x=1, T*=ten2mk128trunc[0]=0x19999999999999999999999999999999
      // if (0 < f* < 10^(-x)) then the result is a midpoint
      //   if floor(C*) is even then C* = floor(C*) - logical right
      //       shift; C* has p decimal digits, correct by Prop. 1)
      //   else if floor(C*) is odd C* = floor(C*)-1 (logical right
      //       shift; C* has p decimal digits, correct by Pr. 1)
      // else
      //   C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-128 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 102
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[0] =
	  (Cstar.w[0] >> shift) | (Cstar.w[1] << (64 - shift));
	// redundant, it will be 0! Cstar.w[1] = (Cstar.w[1] >> shift);
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[0] = (Cstar.w[0] >> (shift - 64));	// 2 <= shift - 64 <= 38
      }

      // if the result was a midpoint it was rounded away from zero
      res = Cstar.w[0];	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 20, but x < 2^64 - 1/2 so in this case C1.w[1] has to be 0
      // res = C (exact)
      res = C1.w[0];
    } else {
      // if (exp > 0) => 1 <= exp <= 19, 1 <= q < 19, 2 <= q + exp <= 20
      // res = C * 10^exp (exact) - must fit in 64 bits
      res = C1.w[0] * ten2k64[exp];
    }
  }
}

BID_RETURN (res);
}

/*****************************************************************************
 *  BID128_to_uint64_xrninta
 ****************************************************************************/

BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE (UINT64,
					  bid128_to_uint64_xrninta, x)

     UINT64 res;
     UINT64 x_sign;
     UINT64 x_exp;
     int exp;			// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
     UINT64 tmp64, tmp64A;
     BID_UI64DOUBLE tmp1;
     unsigned int x_nr_bits;
     int q, ind, shift;
     UINT128 C1, C;
     UINT128 Cstar;		// C* represents up to 34 decimal digits ~ 113 bits
     UINT256 fstar;
     UINT256 P256;

  // unpack x
x_sign = x.w[1] & MASK_SIGN;	// 0 for positive, MASK_SIGN for negative
x_exp = x.w[1] & MASK_EXP;	// biased and shifted left 49 bit positions
C1.w[1] = x.w[1] & MASK_COEFF;
C1.w[0] = x.w[0];

  // check for NaN or Infinity
if ((x.w[1] & MASK_SPECIAL) == MASK_SPECIAL) {
    // x is special
if ((x.w[1] & MASK_NAN) == MASK_NAN) {	// x is NAN
  if ((x.w[1] & MASK_SNAN) == MASK_SNAN) {	// x is SNAN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is QNaN
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
} else {	// x is not a NaN, so it must be infinity
  if (!x_sign) {	// x is +inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  } else {	// x is -inf
    // set invalid flag
    *pfpsf |= INVALID_EXCEPTION;
    // return Integer Indefinite
    res = 0x8000000000000000ull;
  }
  BID_RETURN (res);
}
}
  // check for non-canonical values (after the check for special values)
if ((C1.w[1] > 0x0001ed09bead87c0ull)
    || (C1.w[1] == 0x0001ed09bead87c0ull
	&& (C1.w[0] > 0x378d8e63ffffffffull))
    || ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull)) {
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else if ((C1.w[1] == 0x0ull) && (C1.w[0] == 0x0ull)) {
  // x is 0
  res = 0x0000000000000000ull;
  BID_RETURN (res);
} else {	// x is not special and is not zero

  // q = nr. of decimal digits in x
  //  determine first the nr. of bits in x
  if (C1.w[1] == 0) {
    if (C1.w[0] >= 0x0020000000000000ull) {	// x >= 2^53
      // split the 64-bit value in two 32-bit halves to avoid rounding errors
      if (C1.w[0] >= 0x0000000100000000ull) {	// x >= 2^32
	tmp1.d = (double) (C1.w[0] >> 32);	// exact conversion
	x_nr_bits =
	  33 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      } else {	// x < 2^32
	tmp1.d = (double) (C1.w[0]);	// exact conversion
	x_nr_bits =
	  1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
      }
    } else {	// if x < 2^53
      tmp1.d = (double) C1.w[0];	// exact conversion
      x_nr_bits =
	1 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
    }
  } else {	// C1.w[1] != 0 => nr. bits = 64 + nr_bits (C1.w[1])
    tmp1.d = (double) C1.w[1];	// exact conversion
    x_nr_bits =
      65 + ((((unsigned int) (tmp1.ui64 >> 52)) & 0x7ff) - 0x3ff);
  }
  q = nr_digits[x_nr_bits - 1].digits;
  if (q == 0) {
    q = nr_digits[x_nr_bits - 1].digits1;
    if (C1.w[1] > nr_digits[x_nr_bits - 1].threshold_hi
	|| (C1.w[1] == nr_digits[x_nr_bits - 1].threshold_hi
	    && C1.w[0] >= nr_digits[x_nr_bits - 1].threshold_lo))
      q++;
  }
  exp = (x_exp >> 49) - 6176;

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
    if (x_sign) {	// if n < 0 and q + exp = 20
      // if n <= -1/2 then n cannot be converted to uint64 with RN
      // too large if c(0)c(1)...c(19).c(20)...c(q-1) >= 1/2
      // <=> 0.c(0)c(1)...c(q-1) * 10^21 >= 0x05, 1<=q<=34
      // <=> C * 10^(21-q) >= 0x05, 1<=q<=34
      if (q == 21) {
	// C >= 5 
	if (C1.w[1] != 0 || C1.w[0] >= 0x05ull) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to 64-bit unsigned int fall through
	// to '1 <= q + exp <= 20'
      } else {
	// if 1 <= q <= 20
	//   C * 10^(21-q) >= 5 is true because C >= 1 and 10^(21-q) >= 10
	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	//  C >= 5 * 10^(q-21) is true because C > 2^64 and 5*10^(q-21) < 2^64
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
    } else {	// if n > 0 and q + exp = 20
      // if n >= 2^64 - 1/2 then n is too large
      // <=> c(0)c(1)...c(19).c(20)...c(q-1) >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^20 >= 2^64-1/2
      // <=> 0.c(0)c(1)...c(19)c(20)...c(q-1) * 10^21 >= 5*(2^65-1)
      // <=> C * 10^(21-q) >= 0x9fffffffffffffffb, 1<=q<=34
      if (q == 1) {
	// C * 10^20 >= 0x9fffffffffffffffb
	__mul_128x64_to_128 (C, C1.w[0], ten2k128[0]);	// 10^20 * C
	if (C.w[1] > 0x09 || (C.w[1] == 0x09
			      && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q <= 19) {
	// C * 10^(21-q) >= 0x9fffffffffffffffb
	__mul_64x64_to_128MACH (C, C1.w[0], ten2k64[21 - q]);
	if (C.w[1] > 0x09 || (C.w[1] == 0x09
			      && C.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q == 20) {
	// C * 10 >= 0x9fffffffffffffffb <=> C * 2 > 1ffffffffffffffff
	C.w[0] = C1.w[0] + C1.w[0];
	C.w[1] = C1.w[1] + C1.w[1];
	if (C.w[0] < C1.w[0])
	  C.w[1]++;
	if (C.w[1] > 0x01 || (C.w[1] == 0x01
			      && C.w[0] >= 0xffffffffffffffffull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else if (q == 21) {
	// C >= 0x9fffffffffffffffb
	if (C1.w[1] > 0x09 || (C1.w[1] == 0x09
			       && C1.w[0] >= 0xfffffffffffffffbull)) {
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	  // return Integer Indefinite
	  res = 0x8000000000000000ull;
	  BID_RETURN (res);
	}
	// else cases that can be rounded to a 64-bit int fall through
	// to '1 <= q + exp <= 20'
      } else {	// if 22 <= q <= 34 => 1 <= q - 21 <= 13
	// C  >= 10^(q-21) * 0x9fffffffffffffffb max 44 bits x 68 bits
	C.w[1] = 0x09;
	C.w[0] = 0xfffffffffffffffbull;
	__mul_128x64_to_128 (C, ten2k64[q - 21], C);
	if (C1.w[1] > C.w[1]
	    || (C1.w[1] == C.w[1] && C1.w[0] >= C.w[0])) {
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
  // n is not too large to be converted to int64 if -1/2 < n < 2^64 - 1/2
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
    ind = q - 1;
    if (ind <= 18) {	// 0 <= ind <= 18
      if ((C1.w[1] == 0) && (C1.w[0] < midpoint64[ind])) {
	res = 0x0000000000000000ull;	// return 0
      } else if (!x_sign) {	// n > 0
	res = 0x00000001;	// return +1
      } else {
	res = 0x8000000000000000ull;
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
    } else {	// 19 <= ind <= 33
      if ((C1.w[1] < midpoint128[ind - 19].w[1])
	  || ((C1.w[1] == midpoint128[ind - 19].w[1])
	      && (C1.w[0] < midpoint128[ind - 19].w[0]))) {
	res = 0x0000000000000000ull;	// return 0
      } else if (!x_sign) {	// n > 0
	res = 0x00000001;	// return +1
      } else {
	res = 0x8000000000000000ull;
	*pfpsf |= INVALID_EXCEPTION;
	// return Integer Indefinite
	res = 0x8000000000000000ull;
	BID_RETURN (res);
      }
    }
    // set inexact flag
    *pfpsf |= INEXACT_EXCEPTION;
  } else {	// if (1 <= q + exp <= 20, 1 <= q <= 34, -33 <= exp <= 19)
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
    if (exp < 0) {	// 2 <= q <= 34, -33 <= exp <= -1, 1 <= q + exp <= 20
      ind = -exp;	// 1 <= ind <= 33; ind is a synonym for 'x'
      // chop off ind digits from the lower part of C1
      // C1 = C1 + 1/2 * 10^ind where the result C1 fits in 127 bits
      tmp64 = C1.w[0];
      if (ind <= 19) {
	C1.w[0] = C1.w[0] + midpoint64[ind - 1];
      } else {
	C1.w[0] = C1.w[0] + midpoint128[ind - 20].w[0];
	C1.w[1] = C1.w[1] + midpoint128[ind - 20].w[1];
      }
      if (C1.w[0] < tmp64)
	C1.w[1]++;
      // calculate C* and f*
      // C* is actually floor(C*) in this case
      // C* and f* need shifting and masking, as shown by
      // shiftright128[] and maskhigh128[]
      // 1 <= x <= 33
      // kx = 10^(-x) = ten2mk128[ind - 1]
      // C* = (C1 + 1/2 * 10^x) * 10^(-x)
      // the approximation of 10^(-x) was rounded up to 118 bits
      __mul_128x128_to_256 (P256, C1, ten2mk128[ind - 1]);
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[1] = P256.w[3];
	Cstar.w[0] = P256.w[2];
	fstar.w[3] = 0;
	fstar.w[2] = P256.w[2] & maskhigh128[ind - 1];
	fstar.w[1] = P256.w[1];
	fstar.w[0] = P256.w[0];
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[1] = 0;
	Cstar.w[0] = P256.w[3];
	fstar.w[3] = P256.w[3] & maskhigh128[ind - 1];
	fstar.w[2] = P256.w[2];
	fstar.w[1] = P256.w[1];
	fstar.w[0] = P256.w[0];
      }
      // the top Ex bits of 10^(-x) are T* = ten2mk128trunc[ind], e.g.
      // if x=1, T*=ten2mk128trunc[0]=0x19999999999999999999999999999999
      // if (0 < f* < 10^(-x)) then the result is a midpoint
      //   if floor(C*) is even then C* = floor(C*) - logical right
      //       shift; C* has p decimal digits, correct by Prop. 1)
      //   else if floor(C*) is odd C* = floor(C*)-1 (logical right
      //       shift; C* has p decimal digits, correct by Pr. 1)
      // else
      //   C* = floor(C*) (logical right shift; C has p decimal digits,
      //       correct by Property 1)
      // n = C* * 10^(e+x)

      // shift right C* by Ex-128 = shiftright128[ind]
      shift = shiftright128[ind - 1];	// 0 <= shift <= 102
      if (ind - 1 <= 21) {	// 0 <= ind - 1 <= 21
	Cstar.w[0] =
	  (Cstar.w[0] >> shift) | (Cstar.w[1] << (64 - shift));
	// redundant, it will be 0! Cstar.w[1] = (Cstar.w[1] >> shift);
      } else {	// 22 <= ind - 1 <= 33
	Cstar.w[0] = (Cstar.w[0] >> (shift - 64));	// 2 <= shift - 64 <= 38
      }
      // determine inexactness of the rounding of C*
      // if (0 < f* - 1/2 < 10^(-x)) then
      //   the result is exact
      // else // if (f* - 1/2 > T*) then
      //   the result is inexact
      if (ind - 1 <= 2) {
	if (fstar.w[1] > 0x8000000000000000ull ||
	    (fstar.w[1] == 0x8000000000000000ull
	     && fstar.w[0] > 0x0ull)) {
	  // f* > 1/2 and the result may be exact
	  tmp64 = fstar.w[1] - 0x8000000000000000ull;	// f* - 1/2
	  if (tmp64 > ten2mk128trunc[ind - 1].w[1]
	      || (tmp64 == ten2mk128trunc[ind - 1].w[1]
		  && fstar.w[0] >= ten2mk128trunc[ind - 1].w[0])) {
	    // set the inexact flag
	    *pfpsf |= INEXACT_EXCEPTION;
	  }	// else the result is exact
	} else {	// the result is inexact; f2* <= 1/2
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}
      } else if (ind - 1 <= 21) {	// if 3 <= ind <= 21
	if (fstar.w[3] > 0x0 ||
	    (fstar.w[3] == 0x0 && fstar.w[2] > onehalf128[ind - 1]) ||
	    (fstar.w[3] == 0x0 && fstar.w[2] == onehalf128[ind - 1] &&
	     (fstar.w[1] || fstar.w[0]))) {
	  // f2* > 1/2 and the result may be exact
	  // Calculate f2* - 1/2
	  tmp64 = fstar.w[2] - onehalf128[ind - 1];
	  tmp64A = fstar.w[3];
	  if (tmp64 > fstar.w[2])
	    tmp64A--;
	  if (tmp64A || tmp64
	      || fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	      || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		  && fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	    // set the inexact flag
	    *pfpsf |= INEXACT_EXCEPTION;
	  }	// else the result is exact
	} else {	// the result is inexact; f2* <= 1/2
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}
      } else {	// if 22 <= ind <= 33
	if (fstar.w[3] > onehalf128[ind - 1] ||
	    (fstar.w[3] == onehalf128[ind - 1] &&
	     (fstar.w[2] || fstar.w[1] || fstar.w[0]))) {
	  // f2* > 1/2 and the result may be exact
	  // Calculate f2* - 1/2
	  tmp64 = fstar.w[3] - onehalf128[ind - 1];
	  if (tmp64 || fstar.w[2]
	      || fstar.w[1] > ten2mk128trunc[ind - 1].w[1]
	      || (fstar.w[1] == ten2mk128trunc[ind - 1].w[1]
		  && fstar.w[0] > ten2mk128trunc[ind - 1].w[0])) {
	    // set the inexact flag
	    *pfpsf |= INEXACT_EXCEPTION;
	  }	// else the result is exact
	} else {	// the result is inexact; f2* <= 1/2
	  // set the inexact flag
	  *pfpsf |= INEXACT_EXCEPTION;
	}
      }

      // if the result was a midpoint it was rounded away from zero
      res = Cstar.w[0];	// the result is positive
    } else if (exp == 0) {
      // 1 <= q <= 20, but x < 2^64 - 1/2 so in this case C1.w[1] has to be 0
      // res = C (exact)
      res = C1.w[0];
    } else {
      // if (exp > 0) => 1 <= exp <= 19, 1 <= q < 19, 2 <= q + exp <= 20
      // res = C * 10^exp (exact) - must fit in 64 bits
      res = C1.w[0] * ten2k64[exp];
    }
  }
}

BID_RETURN (res);
}
