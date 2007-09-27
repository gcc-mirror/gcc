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
 *  BID64 nextup
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_nextup (UINT64 * pres,
	      UINT64 *
	      px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_nextup (UINT64 x _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	      _EXC_INFO_PARAM) {
#endif

  UINT64 res;
  UINT64 x_sign;
  UINT64 x_exp;
  BID_UI64DOUBLE tmp1;
  int x_nr_bits;
  int q1, ind;
  UINT64 C1;			// C1 represents x_signif (UINT64)

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
    if (!(x & 0x8000000000000000ull)) {	// x is +inf
      res = 0x7800000000000000ull;
    } else {	// x is -inf
      res = 0xf7fb86f26fc0ffffull;	// -MAXFP = -999...99 * 10^emax
    }
    BID_RETURN (res);
  }
  // unpack the argument
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
    res = 0x0000000000000001ull;	// MINFP = 1 * 10^emin
  } else {	// x is not special and is not zero
    if (x == 0x77fb86f26fc0ffffull) {
      // x = +MAXFP = 999...99 * 10^emax
      res = 0x7800000000000000ull;	// +inf
    } else if (x == 0x8000000000000001ull) {
      // x = -MINFP = 1...99 * 10^emin
      res = 0x8000000000000000ull;	// -0
    } else {	// -MAXFP <= x <= -MINFP - 1 ulp OR MINFP <= x <= MAXFP - 1 ulp
      // can add/subtract 1 ulp to the significand

      // Note: we could check here if x >= 10^16 to speed up the case q1 =16 
      // q1 = nr. of decimal digits in x (1 <= q1 <= 54)
      //  determine first the nr. of bits in x
      if (C1 >= MASK_BINARY_OR2) {	// x >= 2^53
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
      q1 = nr_digits[x_nr_bits - 1].digits;
      if (q1 == 0) {
	q1 = nr_digits[x_nr_bits - 1].digits1;
	if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
	  q1++;
      }
      // if q1 < P16 then pad the significand with zeros
      if (q1 < P16) {
	if (x_exp > (UINT64) (P16 - q1)) {
	  ind = P16 - q1;	// 1 <= ind <= P16 - 1
	  // pad with P16 - q1 zeros, until exponent = emin
	  // C1 = C1 * 10^ind
	  C1 = C1 * ten2k64[ind];
	  x_exp = x_exp - ind;
	} else {	// pad with zeros until the exponent reaches emin
	  ind = x_exp;
	  C1 = C1 * ten2k64[ind];
	  x_exp = EXP_MIN;
	}
      }
      if (!x_sign) {	// x > 0
	// add 1 ulp (add 1 to the significand)
	C1++;
	if (C1 == 0x002386f26fc10000ull) {	// if  C1 = 10^16
	  C1 = 0x00038d7ea4c68000ull;	// C1 = 10^15
	  x_exp++;
	}
	// Ok, because MAXFP = 999...99 * 10^emax was caught already
      } else {	// x < 0
	// subtract 1 ulp (subtract 1 from the significand)
	C1--;
	if (C1 == 0x00038d7ea4c67fffull && x_exp != 0) {	// if  C1 = 10^15 - 1
	  C1 = 0x002386f26fc0ffffull;	// C1 = 10^16 - 1
	  x_exp--;
	}
      }
      // assemble the result
      // if significand has 54 bits
      if (C1 & MASK_BINARY_OR2) {
	res =
	  x_sign | (x_exp << 51) | MASK_STEERING_BITS | (C1 &
							 MASK_BINARY_SIG2);
      } else {	// significand fits in 53 bits
	res = x_sign | (x_exp << 53) | C1;
      }
    }	// end -MAXFP <= x <= -MINFP - 1 ulp OR MINFP <= x <= MAXFP - 1 ulp
  }	// end x is not special and is not zero
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64 nextdown
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_nextdown (UINT64 * pres,
		UINT64 *
		px _EXC_FLAGS_PARAM _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  UINT64 x = *px;
#else
UINT64
bid64_nextdown (UINT64 x _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		_EXC_INFO_PARAM) {
#endif

  UINT64 res;
  UINT64 x_sign;
  UINT64 x_exp;
  BID_UI64DOUBLE tmp1;
  int x_nr_bits;
  int q1, ind;
  UINT64 C1;			// C1 represents x_signif (UINT64)

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
    if (x & 0x8000000000000000ull) {	// x is -inf
      res = 0xf800000000000000ull;
    } else {	// x is +inf
      res = 0x77fb86f26fc0ffffull;	// +MAXFP = +999...99 * 10^emax
    }
    BID_RETURN (res);
  }
  // unpack the argument
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
    res = 0x8000000000000001ull;	// -MINFP = -1 * 10^emin
  } else {	// x is not special and is not zero
    if (x == 0xf7fb86f26fc0ffffull) {
      // x = -MAXFP = -999...99 * 10^emax
      res = 0xf800000000000000ull;	// -inf
    } else if (x == 0x0000000000000001ull) {
      // x = +MINFP = 1...99 * 10^emin
      res = 0x0000000000000000ull;	// -0
    } else {	// -MAXFP + 1ulp <= x <= -MINFP OR MINFP + 1 ulp <= x <= MAXFP
      // can add/subtract 1 ulp to the significand

      // Note: we could check here if x >= 10^16 to speed up the case q1 =16 
      // q1 = nr. of decimal digits in x (1 <= q1 <= 16)
      //  determine first the nr. of bits in x
      if (C1 >= 0x0020000000000000ull) {	// x >= 2^53
	// split the 64-bit value in two 32-bit halves to avoid 
	// rounding errors
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
      q1 = nr_digits[x_nr_bits - 1].digits;
      if (q1 == 0) {
	q1 = nr_digits[x_nr_bits - 1].digits1;
	if (C1 >= nr_digits[x_nr_bits - 1].threshold_lo)
	  q1++;
      }
      // if q1 < P16 then pad the significand with zeros
      if (q1 < P16) {
	if (x_exp > (UINT64) (P16 - q1)) {
	  ind = P16 - q1;	// 1 <= ind <= P16 - 1
	  // pad with P16 - q1 zeros, until exponent = emin
	  // C1 = C1 * 10^ind
	  C1 = C1 * ten2k64[ind];
	  x_exp = x_exp - ind;
	} else {	// pad with zeros until the exponent reaches emin
	  ind = x_exp;
	  C1 = C1 * ten2k64[ind];
	  x_exp = EXP_MIN;
	}
      }
      if (x_sign) {	// x < 0
	// add 1 ulp (add 1 to the significand)
	C1++;
	if (C1 == 0x002386f26fc10000ull) {	// if  C1 = 10^16
	  C1 = 0x00038d7ea4c68000ull;	// C1 = 10^15
	  x_exp++;
	  // Ok, because -MAXFP = -999...99 * 10^emax was caught already
	}
      } else {	// x > 0
	// subtract 1 ulp (subtract 1 from the significand)
	C1--;
	if (C1 == 0x00038d7ea4c67fffull && x_exp != 0) {	// if  C1 = 10^15 - 1
	  C1 = 0x002386f26fc0ffffull;	// C1 = 10^16 - 1
	  x_exp--;
	}
      }
      // assemble the result
      // if significand has 54 bits
      if (C1 & MASK_BINARY_OR2) {
	res =
	  x_sign | (x_exp << 51) | MASK_STEERING_BITS | (C1 &
							 MASK_BINARY_SIG2);
      } else {	// significand fits in 53 bits
	res = x_sign | (x_exp << 53) | C1;
      }
    }	// end -MAXFP <= x <= -MINFP - 1 ulp OR MINFP <= x <= MAXFP - 1 ulp
  }	// end x is not special and is not zero
  BID_RETURN (res);
}

/*****************************************************************************
 *  BID64 nextafter
 ****************************************************************************/

#if DECIMAL_CALL_BY_REFERENCE
void
bid64_nextafter (UINT64 * pres, UINT64 * px,
		 UINT64 *
		 py _EXC_FLAGS_PARAM _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  UINT64 x = *px;
  UINT64 y = *py;
#else
UINT64
bid64_nextafter (UINT64 x,
		 UINT64 y _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		 _EXC_INFO_PARAM) {
#endif

  UINT64 res;
  UINT64 tmp1, tmp2;
  FPSC tmp_fpsf = 0;		// dummy fpsf for calls to comparison functions
  int res1, res2;

  // check for NaNs or infinities
  if (((x & MASK_SPECIAL) == MASK_SPECIAL) ||
      ((y & MASK_SPECIAL) == MASK_SPECIAL)) {
    // x is NaN or infinity or y is NaN or infinity

    if ((x & MASK_NAN) == MASK_NAN) {	// x is NAN
      if ((x & 0x0003ffffffffffffull) > 999999999999999ull)
	x = x & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
      else
	x = x & 0xfe03ffffffffffffull;	// clear G6-G12
      if ((x & MASK_SNAN) == MASK_SNAN) {	// x is SNAN
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return quiet (x)
	res = x & 0xfdffffffffffffffull;
      } else {	// x is QNaN
	if ((y & MASK_SNAN) == MASK_SNAN) {	// y is SNAN
	  // set invalid flag
	  *pfpsf |= INVALID_EXCEPTION;
	}
	// return x
	res = x;
      }
      BID_RETURN (res);
    } else if ((y & MASK_NAN) == MASK_NAN) {	// y is NAN
      if ((y & 0x0003ffffffffffffull) > 999999999999999ull)
	y = y & 0xfe00000000000000ull;	// clear G6-G12 and the payload bits
      else
	y = y & 0xfe03ffffffffffffull;	// clear G6-G12
      if ((y & MASK_SNAN) == MASK_SNAN) {	// y is SNAN
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	// return quiet (y)
	res = y & 0xfdffffffffffffffull;
      } else {	// y is QNaN
	// return y
	res = y;
      }
      BID_RETURN (res);
    } else {	// at least one is infinity
      if ((x & MASK_ANY_INF) == MASK_INF) {	// x = inf
	x = x & (MASK_SIGN | MASK_INF);
      }
      if ((y & MASK_ANY_INF) == MASK_INF) {	// y = inf
	y = y & (MASK_SIGN | MASK_INF);
      }
    }
  }
  // neither x nor y is NaN

  // if not infinity, check for non-canonical values x (treated as zero)
  if ((x & MASK_ANY_INF) != MASK_INF) {	// x != inf
    // unpack x
    if ((x & MASK_STEERING_BITS) == MASK_STEERING_BITS) {
      // if the steering bits are 11 (condition will be 0), then
      // the exponent is G[0:w+1]
      if (((x & MASK_BINARY_SIG2) | MASK_BINARY_OR2) >
	  9999999999999999ull) {
	// non-canonical
	x = (x & MASK_SIGN) | ((x & MASK_BINARY_EXPONENT2) << 2);
      }
    } else {	// if ((x & MASK_STEERING_BITS) != MASK_STEERING_BITS) x is unch.
      ;	// canonical
    }
  }
  // no need to check for non-canonical y

  // neither x nor y is NaN
  tmp_fpsf = *pfpsf;	// save fpsf
#if DECIMAL_CALL_BY_REFERENCE
  bid64_quiet_equal (&res1, px,
		     py _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  bid64_quiet_greater (&res2, px,
		       py _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
#else
  res1 =
    bid64_quiet_equal (x,
		       y _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
  res2 =
    bid64_quiet_greater (x,
			 y _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
#endif
  *pfpsf = tmp_fpsf;	// restore fpsf
  if (res1) {	// x = y
    // return x with the sign of y
    res = (y & 0x8000000000000000ull) | (x & 0x7fffffffffffffffull);
  } else if (res2) {	// x > y
#if DECIMAL_CALL_BY_REFERENCE
    bid64_nextdown (&res,
		    px _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
#else
    res =
      bid64_nextdown (x _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
#endif
  } else {	// x < y
#if DECIMAL_CALL_BY_REFERENCE
    bid64_nextup (&res, px _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
#else
    res = bid64_nextup (x _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG);
#endif
  }
  // if the operand x is finite but the result is infinite, signal
  // overflow and inexact
  if (((x & MASK_INF) != MASK_INF) && ((res & MASK_INF) == MASK_INF)) {
    // set the inexact flag
    *pfpsf |= INEXACT_EXCEPTION;
    // set the overflow flag
    *pfpsf |= OVERFLOW_EXCEPTION;
  }
  // if the result is in (-10^emin, 10^emin), and is different from the
  // operand x, signal underflow and inexact 
  tmp1 = 0x00038d7ea4c68000ull;	// +100...0[16] * 10^emin
  tmp2 = res & 0x7fffffffffffffffull;
  tmp_fpsf = *pfpsf;	// save fpsf
#if DECIMAL_CALL_BY_REFERENCE
  bid64_quiet_greater (&res1, &tmp1,
		       &tmp2 _EXC_FLAGS_ARG _EXC_MASKS_ARG
		       _EXC_INFO_ARG);
  bid64_quiet_not_equal (&res2, &x,
			 &res _EXC_FLAGS_ARG _EXC_MASKS_ARG
			 _EXC_INFO_ARG);
#else
  res1 =
    bid64_quiet_greater (tmp1,
			 tmp2 _EXC_FLAGS_ARG _EXC_MASKS_ARG
			 _EXC_INFO_ARG);
  res2 =
    bid64_quiet_not_equal (x,
			   res _EXC_FLAGS_ARG _EXC_MASKS_ARG
			   _EXC_INFO_ARG);
#endif
  *pfpsf = tmp_fpsf;	// restore fpsf
  if (res1 && res2) {
    // if (bid64_quiet_greater (tmp1, tmp2, &tmp_fpsf) &&
    // bid64_quiet_not_equal (x, res, &tmp_fpsf)) {
    // set the inexact flag
    *pfpsf |= INEXACT_EXCEPTION;
    // set the underflow flag
    *pfpsf |= UNDERFLOW_EXCEPTION;
  }
  BID_RETURN (res);
}
