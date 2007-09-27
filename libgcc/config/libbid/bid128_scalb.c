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

#define DECIMAL_EXPONENT_BIAS_128 6176
#define MAX_DECIMAL_EXPONENT_128  12287



BID128_FUNCTION_ARG128_ARGTYPE2 (bid128_scalb, x, int, n)

     UINT128 CX, CX2, CX8, res;
     SINT64 exp64;
     UINT64 sign_x;
     int exponent_x, rmode;

  // unpack arguments, check for NaN or Infinity
if (!unpack_BID128_value (&sign_x, &exponent_x, &CX, x)) {
    // x is Inf. or NaN or 0
#ifdef SET_STATUS_FLAGS
if ((x.w[1] & SNAN_MASK64) == SNAN_MASK64)	// y is sNaN
  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
res.w[1] = CX.w[1] & QUIET_MASK64;
res.w[0] = CX.w[0];
if (!CX.w[1]) {
       exp64 = (SINT64) exponent_x + (SINT64) n;
	   if(exp64<0) exp64=0;
	   if(exp64>MAX_DECIMAL_EXPONENT_128) exp64=MAX_DECIMAL_EXPONENT_128;
       exponent_x = exp64;
  get_BID128_very_fast (&res, sign_x, exponent_x, CX);
}
BID_RETURN (res);
}

exp64 = (SINT64) exponent_x + (SINT64) n;
exponent_x = exp64;

if ((UINT32) exponent_x <= MAX_DECIMAL_EXPONENT_128) {
  get_BID128_very_fast (&res, sign_x, exponent_x, CX);
  BID_RETURN (res);
}
  // check for overflow
if (exp64 > MAX_DECIMAL_EXPONENT_128) {
  if (CX.w[1] < 0x314dc6448d93ull) {
    // try to normalize coefficient
    do {
      CX8.w[1] = (CX.w[1] << 3) | (CX.w[0] >> 61);
      CX8.w[0] = CX.w[0] << 3;
      CX2.w[1] = (CX.w[1] << 1) | (CX.w[0] >> 63);
      CX2.w[0] = CX.w[0] << 1;
      __add_128_128 (CX, CX2, CX8);

      exponent_x--;
      exp64--;
    }
    while (CX.w[1] < 0x314dc6448d93ull
	   && exp64 > MAX_DECIMAL_EXPONENT_128);

  }
  if (exp64 <= MAX_DECIMAL_EXPONENT_128) {
    get_BID128_very_fast (&res, sign_x, exponent_x, CX);
    BID_RETURN (res);
  } else
    exponent_x = 0x7fffffff;	// overflow
}
  // exponent < 0
  // the BID pack routine will round the coefficient
rmode = rnd_mode;
get_BID128 (&res, sign_x, exponent_x, CX, (unsigned int *) &rmode,
	    pfpsf);
BID_RETURN (res);

}
