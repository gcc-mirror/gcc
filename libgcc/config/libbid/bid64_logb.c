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

#define MAX_FORMAT_DIGITS     16
#define DECIMAL_EXPONENT_BIAS 398

#if DECIMAL_CALL_BY_REFERENCE

void
__bid64_logb (UINT64 * pres,
	    UINT64 *
	    px _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	    _EXC_INFO_PARAM) {
  UINT64 x;
#else

UINT64
__bid64_logb (UINT64 x _RND_MODE_PARAM _EXC_FLAGS_PARAM
	    _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT64 sign_x, coefficient_x, res;
  UINT64 logb_coeff, logb_sign;
  SINT32 sign_e;
  int exponent_x;

#if DECIMAL_CALL_BY_REFERENCE
#if !DECIMAL_GLOBAL_ROUNDING
  _IDEC_round rnd_mode = *prnd_mode;
#endif
  x = *px;
#endif

  // unpack arguments, check for NaN or Infinity
  if (!unpack_BID64 (&sign_x, &exponent_x, &coefficient_x, x)) {
    // x is Inf. or NaN

    // test if x is NaN
    if ((x & 0x7c00000000000000ull) == 0x7c00000000000000ull) {
      res = x;
      BID_RETURN (res);
    }
    // x is Infinity?
    if ((x & 0x7800000000000000ull) == 0x7800000000000000ull) {
      res = 0x7800000000000000ull;
      BID_RETURN (res);
    }
    // x is 0
    {
#ifdef SET_STATUS_FLAGS
      __set_status_flags (pfpsf, ZERO_DIVIDE_EXCEPTION);
#endif
      res = 0xf800000000000000ull;
      BID_RETURN (res);
    }
  }

  exponent_x =
    exponent_x - DECIMAL_EXPONENT_BIAS + MAX_FORMAT_DIGITS - 1;

  // extract sign and absolute value from exponent_x
  sign_e = ((SINT32) exponent_x) >> 31;
  exponent_x = (exponent_x + sign_e) ^ sign_e;

  logb_coeff = exponent_x;
  logb_sign = sign_e & 1;
  logb_sign <<= 63;

  res =
    very_fast_get_BID64_small_mantissa (logb_sign,
					DECIMAL_EXPONENT_BIAS,
					logb_coeff);
  BID_RETURN (res);
}
