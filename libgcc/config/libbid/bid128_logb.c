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

#define BID_128RES
#include "bid_internal.h"

BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE(int, bid128_logb, x)

  UINT128 CX;
  UINT64 sign_x;
  SINT64 D;
  int_float f64, fx;
  int exponent_x, bin_expon_cx, digits;

  if (!unpack_BID128_value (&sign_x, &exponent_x, &CX, x)) {
#ifdef SET_STATUS_FLAGS
      __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
    BID_RETURN_VAL (0x80000000);
  }
  // find number of digits in coefficient
  // 2^64
  f64.i = 0x5f800000;
  // fx ~ CX
  fx.d = (float) CX.w[1] * f64.d + (float) CX.w[0];
  bin_expon_cx = ((fx.i >> 23) & 0xff) - 0x7f;
  digits = estimate_decimal_digits[bin_expon_cx];
  // scale = 38-estimate_decimal_digits[bin_expon_cx];
  D = CX.w[1] - power10_index_binexp_128[bin_expon_cx].w[1];
  if (D > 0 || (!D && CX.w[0] >= power10_index_binexp_128[bin_expon_cx].w[0])) {
    digits++;
  }

  exponent_x = exponent_x - DECIMAL_EXPONENT_BIAS_128 - 1 + digits;

  BID_RETURN_VAL (exponent_x);

}
