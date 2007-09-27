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
bid64_logb (int * pres, UINT64 * px
	    _EXC_FLAGS_PARAM _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
  UINT64 x;
#else

int
bid64_logb (UINT64 x _EXC_FLAGS_PARAM _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT64 sign_x, coefficient_x;
  int_double dx;
  int exponent_x, bin_expon_cx, digits;

#if DECIMAL_CALL_BY_REFERENCE
  x = *px;
#endif
  // unpack arguments, check for NaN or Infinity
  if (!unpack_BID64 (&sign_x, &exponent_x, &coefficient_x, x)) {
    // x is Inf. or NaN
#ifdef SET_STATUS_FLAGS
      __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      BID_RETURN (0x80000000);
  }
  // find number of digits in coefficient
  if (coefficient_x >= 1000000000000000ull) {
    digits = 16;
  } else {
    dx.d = (double)coefficient_x;   // exact conversion;
    bin_expon_cx = (int)(dx.i >> 52) - 1023;
    digits = estimate_decimal_digits[bin_expon_cx];
    if (coefficient_x >= power10_table_128[digits].w[0])
      digits++;
  }
  exponent_x = exponent_x - DECIMAL_EXPONENT_BIAS + digits - 1;

  BID_RETURN (exponent_x);
}
