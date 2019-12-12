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

#define  DECNUMDIGITS 34	// work with up to 34 digits

#include "bid_internal.h"
#include "bid_b2d.h"

UINT32
bid_to_bid32 (UINT32 ba) {
  UINT32 res;
  UINT32 sign, comb, exp;
  UINT32 trailing;
  UINT32 bcoeff;

  sign = (ba & 0x80000000ul);
  comb = (ba & 0x7ff00000ul) >> 20;
  trailing = (ba & 0x000ffffful);

  if ((comb & 0x780) == 0x780) {
    ba &= 0xfff0000ul;
    return ba;
  } else {
    if ((comb & 0x600) == 0x600) {	// G0..G1 = 11 -> exp is G2..G11
      exp = (comb >> 1) & 0xff;
      bcoeff = ((8 + (comb & 1)) << 20) | trailing;
    } else {
      exp = (comb >> 3) & 0xff;
      bcoeff = ((comb & 7) << 20) | trailing;
    }
    if (bcoeff >= 10000000)
      bcoeff = 0;
    res = very_fast_get_BID32 (sign, exp, bcoeff);
  }
  return res;
}

UINT64
bid_to_bid64 (UINT64 ba) {
  UINT64 res;
  UINT64 sign, comb, exp;
  UINT64 trailing;
  UINT64 bcoeff;

  sign = (ba & 0x8000000000000000ull);
  comb = (ba & 0x7ffc000000000000ull) >> 50;
  trailing = (ba & 0x0003ffffffffffffull);

  if ((comb & 0x1e00) == 0x1e00) {
    ba &= 0xfff000000000000ULL;
    return ba;
  } else {
    if ((comb & 0x1800) == 0x1800) {	// G0..G1 = 11 -> exp is G2..G11
      exp = (comb >> 1) & 0x3ff;
      bcoeff = ((8 + (comb & 1)) << 50) | trailing;
    } else {
      exp = (comb >> 3) & 0x3ff;
      bcoeff = ((comb & 7) << 50) | trailing;
    }
    if (bcoeff >= 10000000000000000ull)
      bcoeff = 0ull;
    res = very_fast_get_BID64 (sign, exp, bcoeff);
  }
  return res;
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid_to_dpd32 (UINT32 * pres, UINT32 * pba) {
  UINT32 ba = *pba;
#else
UINT32
bid_to_dpd32 (UINT32 ba) {
#endif
  UINT32 res;

  UINT32 sign, comb, exp, trailing;
  UINT32 b0, b1, b2;
  UINT32 bcoeff, dcoeff;
  UINT32 nanb = 0;

  sign = (ba & 0x80000000);
  comb = (ba & 0x7ff00000) >> 20;
  trailing = (ba & 0xfffff);

  // Detect infinity, and return canonical infinity
  if ((comb & 0x7c0) == 0x780) {
    res = sign | 0x78000000;
    BID_RETURN (res);
    // Detect NaN, and canonicalize trailing
  } else if ((comb & 0x7c0) == 0x7c0) {
    if (trailing > 999999)
      trailing = 0;
    nanb = ba & 0xfe000000;
    exp = 0;
    bcoeff = trailing;
  } else {	// Normal number
    if ((comb & 0x600) == 0x600) {	// G0..G1 = 11 -> exp is G2..G11
      exp = (comb >> 1) & 0xff;
      bcoeff = ((8 + (comb & 1)) << 20) | trailing;
    } else {
      exp = (comb >> 3) & 0xff;
      bcoeff = ((comb & 7) << 20) | trailing;
    }
    // Zero the coefficient if non-canonical (>= 10^7)
    if (bcoeff >= 10000000)
      bcoeff = 0;
  }

  b0 = bcoeff / 1000000;
  b1 = (bcoeff / 1000) % 1000;
  b2 = bcoeff % 1000;
  dcoeff = (b2d[b1] << 10) | b2d[b2];

  if (b0 >= 8)	// is b0 8 or 9?
    res =
      sign |
      ((0x600 | ((exp >> 6) << 7) | ((b0 & 1) << 6) | (exp & 0x3f)) <<
       20) | dcoeff;
  else	// else b0 is 0..7
    res =
      sign | ((((exp >> 6) << 9) | (b0 << 6) | (exp & 0x3f)) << 20) |
      dcoeff;

  res |= nanb;

  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid_to_dpd64 (UINT64 * pres, UINT64 * pba) {
  UINT64 ba = *pba;
#else
UINT64
bid_to_dpd64 (UINT64 ba) {
#endif
  UINT64 res;

  UINT64 sign, comb, exp;
  UINT64 trailing;
  UINT32 b0, b1, b2, b3, b4, b5;
  UINT64 bcoeff;
  UINT64 dcoeff;
  UINT32 yhi, ylo;
  UINT64 nanb = 0;

//printf("arg bid "FMT_LLX16" \n", ba);
  sign = (ba & 0x8000000000000000ull);
  comb = (ba & 0x7ffc000000000000ull) >> 50;
  trailing = (ba & 0x0003ffffffffffffull);

  // Detect infinity, and return canonical infinity
  if ((comb & 0x1f00) == 0x1e00) {
    res = sign | 0x7800000000000000ull;
    BID_RETURN (res);
    // Detect NaN, and canonicalize trailing
  } else if ((comb & 0x1e00) == 0x1e00) {
    if (trailing > 999999999999999ull)
      trailing = 0;
    nanb = ba & 0xfe00000000000000ull;
    exp = 0;
    bcoeff = trailing;
  } else {	// Normal number
    if ((comb & 0x1800) == 0x1800) {	// G0..G1 = 11 -> exp is G2..G11
      exp = (comb >> 1) & 0x3ff;
      bcoeff = ((8 + (comb & 1)) << 50) | trailing;
    } else {
      exp = (comb >> 3) & 0x3ff;
      bcoeff = ((comb & 7) << 50) | trailing;
    }

    // Zero the coefficient if it is non-canonical (>= 10^16)
    if (bcoeff >= 10000000000000000ull)
      bcoeff = 0;
  }

// Floor(2^61 / 10^9)
#define D61 (2305843009ull)

// Multipy the binary coefficient by ceil(2^64 / 1000), and take the upper
// 64-bits in order to compute a division by 1000.

#if 1
  yhi =
    ((UINT64) D61 *
     (UINT64) (UINT32) (bcoeff >> (UINT64) 27)) >> (UINT64) 34;
  ylo = bcoeff - 1000000000ull * yhi;
  if (ylo >= 1000000000) {
    ylo = ylo - 1000000000;
    yhi = yhi + 1;
  }
#else
  yhi = bcoeff / 1000000000ull;
  ylo = bcoeff % 1000000000ull;
#endif

  // yhi = ABBBCCC ylo = DDDEEEFFF
  b5 = ylo % 1000;	// b5 = FFF
  b3 = ylo / 1000000;	// b3 = DDD
  b4 = (ylo / 1000) - (1000 * b3);	// b4 = EEE
  b2 = yhi % 1000;	// b2 = CCC
  b0 = yhi / 1000000;	// b0 = A
  b1 = (yhi / 1000) - (1000 * b0);	// b1 = BBB

  dcoeff = b2d[b5] | b2d2[b4] | b2d3[b3] | b2d4[b2] | b2d5[b1];

  if (b0 >= 8)	// is b0 8 or 9?
    res =
      sign |
      ((0x1800 | ((exp >> 8) << 9) | ((b0 & 1) << 8) | (exp & 0xff)) <<
       50) | dcoeff;
  else	// else b0 is 0..7
    res =
      sign | ((((exp >> 8) << 11) | (b0 << 8) | (exp & 0xff)) << 50) |
      dcoeff;

  res |= nanb;

  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
dpd_to_bid32 (UINT32 * pres, UINT32 * pda) {
  UINT32 da = *pda;
#else
UINT32
dpd_to_bid32 (UINT32 da) {
#endif
  UINT32 in = *(UINT32 *) & da;
  UINT32 res;

  UINT32 sign, comb, exp;
  UINT32 trailing;
  UINT32 d0 = 0, d1, d2;
  UINT64 bcoeff;
  UINT32 nanb = 0;

  sign = (in & 0x80000000);
  comb = (in & 0x7ff00000) >> 20;
  trailing = (in & 0x000fffff);

  if ((comb & 0x7e0) == 0x780) {	// G0..G4 = 1111 -> Inf
    res = in & 0xf8000000;
    BID_RETURN (res);
  } else if ((comb & 0x7c0) == 0x7c0) {	// G0..G5 = 11111 -> NaN
    nanb = in & 0xfe000000;
    exp = 0;
  } else {	// Normal number
    if ((comb & 0x600) == 0x600) {	// G0..G1 = 11 -> d0 = 8 + G4
      d0 = ((comb >> 6) & 1) | 8;
      exp = ((comb & 0x180) >> 1) | (comb & 0x3f);
    } else {
      d0 = (comb >> 6) & 0x7;
      exp = ((comb & 0x600) >> 3) | (comb & 0x3f);
    }
  }
  d1 = d2b2[(trailing >> 10) & 0x3ff];
  d2 = d2b[(trailing) & 0x3ff];

  bcoeff = d2 + d1 + (1000000 * d0);
  if (bcoeff < 0x800000) {
    res = (exp << 23) | bcoeff | sign;
  } else {
    res = (exp << 21) | sign | 0x60000000 | (bcoeff & 0x1fffff);
  }

  res |= nanb;

  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
dpd_to_bid64 (UINT64 * pres, UINT64 * pda) {
  UINT64 da = *pda;
#else
UINT64
dpd_to_bid64 (UINT64 da) {
#endif
  UINT64 in = *(UINT64 *) & da;
  UINT64 res;

  UINT64 sign, comb, exp;
  UINT64 trailing;
  // UINT64 d0, d1, d2, d3, d4, d5;

  UINT64 d1, d2;
  UINT32 d0, d3, d4, d5;
  UINT64 bcoeff;
  UINT64 nanb = 0;

//printf("arg dpd "FMT_LLX16" \n", in);
  sign = (in & 0x8000000000000000ull);
  comb = (in & 0x7ffc000000000000ull) >> 50;
  trailing = (in & 0x0003ffffffffffffull);

  if ((comb & 0x1f00) == 0x1e00) {	// G0..G4 = 1111 -> Inf
    res = in & 0xf800000000000000ull;
    BID_RETURN (res);
  } else if ((comb & 0x1f00) == 0x1f00) {	// G0..G5 = 11111 -> NaN
    nanb = in & 0xfe00000000000000ull;
    exp = 0;
    d0 = 0;
  } else {	// Normal number
    if ((comb & 0x1800) == 0x1800) {	// G0..G1 = 11 -> d0 = 8 + G4
      d0 = ((comb >> 8) & 1) | 8;
      // d0 = (comb & 0x0100 ? 9 : 8);
      exp = (comb & 0x600) >> 1;
      // exp = (comb & 0x0400 ? 1 : 0) * 0x200 + (comb & 0x0200 ? 1 : 0) * 0x100; // exp leading bits are G2..G3
    } else {
      d0 = (comb >> 8) & 0x7;
      exp = (comb & 0x1800) >> 3;
      // exp = (comb & 0x1000 ? 1 : 0) * 0x200 + (comb & 0x0800 ? 1 : 0) * 0x100; // exp loading bits are G0..G1
    }
  }
  d1 = d2b5[(trailing >> 40) & 0x3ff];
  d2 = d2b4[(trailing >> 30) & 0x3ff];
  d3 = d2b3[(trailing >> 20) & 0x3ff];
  d4 = d2b2[(trailing >> 10) & 0x3ff];
  d5 = d2b[(trailing) & 0x3ff];

  bcoeff = (d5 + d4 + d3) + d2 + d1 + (1000000000000000ull * d0);
  exp += (comb & 0xff);
  res = very_fast_get_BID64 (sign, exp, bcoeff);

  res |= nanb;

  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
bid_to_dpd128 (UINT128 * pres, UINT128 * pba) {
  UINT128 ba = *pba;
#else
UINT128
bid_to_dpd128 (UINT128 ba) {
#endif
  UINT128 res;

  UINT128 sign;
  UINT32 comb, exp;
  UINT128 trailing;
  UINT128 d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11;
  UINT128 bcoeff;
  UINT128 dcoeff;
  UINT64 nanb = 0;

  sign.w[1] = (ba.w[HIGH_128W] & 0x8000000000000000ull);
  sign.w[0] = 0;
  comb = (ba.w[HIGH_128W] & 0x7fffc00000000000ull) >> 46;
  trailing.w[1] = (ba.w[HIGH_128W] & 0x00003fffffffffffull);
  trailing.w[0] = ba.w[LOW_128W];
  exp = 0;

  if ((comb & 0x1f000) == 0x1e000) {	// G0..G4 = 1111 -> Inf
    res.w[HIGH_128W] = ba.w[HIGH_128W] & 0xf800000000000000ull;
    res.w[LOW_128W] = 0;
    BID_RETURN (res);
    // Detect NaN, and canonicalize trailing
  } else if ((comb & 0x1f000) == 0x1f000) {
    if ((trailing.w[1] > 0x0000314dc6448d93ULL) ||	// significand is non-canonical
	((trailing.w[1] == 0x0000314dc6448d93ULL)
	 && (trailing.w[0] >= 0x38c15b0a00000000ULL))
	// significand is non-canonical
      ) {
      trailing.w[1] = trailing.w[0] = 0ull;
    }
    bcoeff.w[1] = trailing.w[1];
    bcoeff.w[0] = trailing.w[0];
    nanb = ba.w[HIGH_128W] & 0xfe00000000000000ull;
    exp = 0;
  } else {	// Normal number
    if ((comb & 0x18000) == 0x18000) {	// G0..G1 = 11 -> exp is G2..G11
      exp = (comb >> 1) & 0x3fff;
      bcoeff.w[1] =
	((UINT64) (8 + (comb & 1)) << (UINT64) 46) | trailing.w[1];
      bcoeff.w[0] = trailing.w[0];
    } else {
      exp = (comb >> 3) & 0x3fff;
      bcoeff.w[1] =
	((UINT64) (comb & 7) << (UINT64) 46) | trailing.w[1];
      bcoeff.w[0] = trailing.w[0];
    }
    // Zero the coefficient if non-canonical (>= 10^34)
    if (bcoeff.w[1] > 0x1ed09bead87c0ull ||
	(bcoeff.w[1] == 0x1ed09bead87c0ull
	 && bcoeff.w[0] >= 0x378D8E6400000000ull)) {
      bcoeff.w[0] = bcoeff.w[1] = 0;
    }
  }
  // Constant 2^128 / 1000 + 1
  {
    UINT128 t;
    UINT64 t2;
    UINT128 d1000;
    UINT128 b11, b10, b9, b8, b7, b6, b5, b4, b3, b2, b1;
    d1000.w[1] = 0x4189374BC6A7EFull;
    d1000.w[0] = 0x9DB22D0E56041894ull;
    __mul_128x128_high (b11, bcoeff, d1000);
    __mul_128x128_high (b10, b11, d1000);
    __mul_128x128_high (b9, b10, d1000);
    __mul_128x128_high (b8, b9, d1000);
    __mul_128x128_high (b7, b8, d1000);
    __mul_128x128_high (b6, b7, d1000);
    __mul_128x128_high (b5, b6, d1000);
    __mul_128x128_high (b4, b5, d1000);
    __mul_128x128_high (b3, b4, d1000);
    __mul_128x128_high (b2, b3, d1000);
    __mul_128x128_high (b1, b2, d1000);


    __mul_64x128_full (t2, t, 1000ull, b11);
    __sub_128_128 (d11, bcoeff, t);
    __mul_64x128_full (t2, t, 1000ull, b10);
    __sub_128_128 (d10, b11, t);
    __mul_64x128_full (t2, t, 1000ull, b9);
    __sub_128_128 (d9, b10, t);
    __mul_64x128_full (t2, t, 1000ull, b8);
    __sub_128_128 (d8, b9, t);
    __mul_64x128_full (t2, t, 1000ull, b7);
    __sub_128_128 (d7, b8, t);
    __mul_64x128_full (t2, t, 1000ull, b6);
    __sub_128_128 (d6, b7, t);
    __mul_64x128_full (t2, t, 1000ull, b5);
    __sub_128_128 (d5, b6, t);
    __mul_64x128_full (t2, t, 1000ull, b4);
    __sub_128_128 (d4, b5, t);
    __mul_64x128_full (t2, t, 1000ull, b3);
    __sub_128_128 (d3, b4, t);
    __mul_64x128_full (t2, t, 1000ull, b2);
    __sub_128_128 (d2, b3, t);
    __mul_64x128_full (t2, t, 1000ull, b1);
    __sub_128_128 (d1, b2, t);
    d0 = b1;

  }

  dcoeff.w[0] = b2d[d11.w[0]] | (b2d[d10.w[0]] << 10) |
    (b2d[d9.w[0]] << 20) | (b2d[d8.w[0]] << 30) | (b2d[d7.w[0]] << 40) |
    (b2d[d6.w[0]] << 50) | (b2d[d5.w[0]] << 60);
  dcoeff.w[1] =
    (b2d[d5.w[0]] >> 4) | (b2d[d4.w[0]] << 6) | (b2d[d3.w[0]] << 16) |
    (b2d[d2.w[0]] << 26) | (b2d[d1.w[0]] << 36);

  res.w[0] = dcoeff.w[0];
  if (d0.w[0] >= 8) {
    res.w[1] =
      sign.
      w[1] |
      ((0x18000 | ((exp >> 12) << 13) | ((d0.w[0] & 1) << 12) |
	(exp & 0xfff)) << 46) | dcoeff.w[1];
  } else {
    res.w[1] =
      sign.
      w[1] | ((((exp >> 12) << 15) | (d0.w[0] << 12) | (exp & 0xfff))
	      << 46) | dcoeff.w[1];
  }

  res.w[1] |= nanb;

  BID_SWAP128 (res);
  BID_RETURN (res);
}

#if DECIMAL_CALL_BY_REFERENCE
void
dpd_to_bid128 (UINT128 * pres, UINT128 * pda) {
  UINT128 da = *pda;
#else
UINT128
dpd_to_bid128 (UINT128 da) {
#endif
  UINT128 in = *(UINT128 *) & da;
  UINT128 res;

  UINT128 sign;
  UINT64 exp, comb;
  UINT128 trailing;
  UINT64 d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11;
  UINT128 bcoeff;
  UINT64 tl, th;
  UINT64 nanb = 0;

  sign.w[1] = (in.w[HIGH_128W] & 0x8000000000000000ull);
  sign.w[0] = 0;
  comb = (in.w[HIGH_128W] & 0x7fffc00000000000ull) >> 46;
  trailing.w[1] = (in.w[HIGH_128W] & 0x00003fffffffffffull);
  trailing.w[0] = in.w[LOW_128W];
  exp = 0;

  if ((comb & 0x1f000) == 0x1e000) {	// G0..G4 = 1111 -> Inf
    res.w[HIGH_128W] = in.w[HIGH_128W] & 0xf800000000000000ull;
    res.w[LOW_128W] = 0ull;
    BID_RETURN (res);
  } else if ((comb & 0x1f000) == 0x1f000) {	// G0..G4 = 11111 -> NaN
    nanb = in.w[HIGH_128W] & 0xfe00000000000000ull;
    exp = 0;
    d0 = 0;
  } else {	// Normal number
    if ((comb & 0x18000) == 0x18000) {	// G0..G1 = 11 -> d0 = 8 + G4
      d0 = 8 + (comb & 0x01000 ? 1 : 0);
      exp =
	(comb & 0x04000 ? 1 : 0) * 0x2000 +
	(comb & 0x02000 ? 1 : 0) * 0x1000;
      // exp leading bits are G2..G3
    } else {
      d0 =
	4 * (comb & 0x04000 ? 1 : 0) + 2 * (comb & 0x2000 ? 1 : 0) +
	(comb & 0x1000 ? 1 : 0);
      exp =
	(comb & 0x10000 ? 1 : 0) * 0x2000 +
	(comb & 0x08000 ? 1 : 0) * 0x1000;
      // exp loading bits are G0..G1
    }
  }

  d11 = d2b[(trailing.w[0]) & 0x3ff];
  d10 = d2b[(trailing.w[0] >> 10) & 0x3ff];
  d9 = d2b[(trailing.w[0] >> 20) & 0x3ff];
  d8 = d2b[(trailing.w[0] >> 30) & 0x3ff];
  d7 = d2b[(trailing.w[0] >> 40) & 0x3ff];
  d6 = d2b[(trailing.w[0] >> 50) & 0x3ff];
  d5 = d2b[(trailing.w[0] >> 60) | ((trailing.w[1] & 0x3f) << 4)];
  d4 = d2b[(trailing.w[1] >> 6) & 0x3ff];
  d3 = d2b[(trailing.w[1] >> 16) & 0x3ff];
  d2 = d2b[(trailing.w[1] >> 26) & 0x3ff];
  d1 = d2b[(trailing.w[1] >> 36) & 0x3ff];

  tl =
    d11 + (d10 * 1000ull) + (d9 * 1000000ull) + (d8 * 1000000000ull) +
    (d7 * 1000000000000ull) + (d6 * 1000000000000000ull);
  th =
    d5 + (d4 * 1000ull) + (d3 * 1000000ull) + (d2 * 1000000000ull) +
    (d1 * 1000000000000ull) + (d0 * 1000000000000000ull);
  __mul_64x64_to_128 (bcoeff, th, 1000000000000000000ull);
  __add_128_64 (bcoeff, bcoeff, tl);

  if (!nanb)
    exp += (comb & 0xfff);

  res.w[0] = bcoeff.w[0];
  res.w[1] = (exp << 49) | sign.w[1] | bcoeff.w[1];

  res.w[1] |= nanb;

  BID_SWAP128 (res);
  BID_RETURN (res);
}

UINT128
bid_to_bid128 (UINT128 bq) {
  UINT128 res;
  UINT64 sign, comb, exp;
  UINT64 trailing;
  UINT64 bcoeff;

  UINT128 rq;
  UINT128 qcoeff;
  UINT64 ba, bb;

  ba = *((UINT64 *) & bq + HIGH_128W);
  bb = *((UINT64 *) & bq + LOW_128W);

  sign = (ba & 0x8000000000000000ull);
  comb = (ba & 0x7fffc00000000000ull) >> 46;
  trailing = (ba & 0x00003fffffffffffull);

  if ((comb & 0x18000) == 0x18000) {	// G0..G1 = 11 -> exp is G2..G11
    exp = (comb >> 1) & 0x3fff;
    bcoeff = ((8 + (comb & 1)) << 46) | trailing;
  } else {
    exp = (comb >> 3) & 0x3fff;
    bcoeff = ((comb & 7) << 46) | trailing;
  }

  if ((comb & 0x1f000) == 0x1f000) {	//NaN
    ba &= 0xfe003fffffffffffULL;	// make exponent 0
    bcoeff &= 0x00003fffffffffffull;	// NaN payloat is only T.  
    if ((bcoeff > 0x0000314dc6448d93ULL) ||	// significand is non-canonical
	((bcoeff == 0x0000314dc6448d93ULL)
	 && (bb >= 0x38c15b0a00000000ULL))
	// significand is non-canonical
      ) {
      bcoeff = 0ull;
      ba &= ~0x00003fffffffffffull;
      bb = 0ull;
    }
    *((UINT64 *) & rq + HIGH_128W) = ba;
    *((UINT64 *) & rq + LOW_128W) = bb;
    return rq;
  } else if ((comb & 0x1e000) == 0x1e000) {	//Inf
    ba &= 0xf800000000000000ULL;	// make exponent and significand 0
    bb = 0;
    *((UINT64 *) & rq + HIGH_128W) = ba;
    *((UINT64 *) & rq + LOW_128W) = bb;
    return rq;
  }

  if ((bcoeff > 0x0001ed09bead87c0ull)
      || ((bcoeff == 0x0001ed09bead87c0ull)
	  && (bb > 0x378d8e63ffffffffull))) {
    // significand is non-canonical
    bcoeff = 0ull;
    bb = 0ull;
  }

  *((UINT64 *) & qcoeff + 1) = bcoeff;
  *((UINT64 *) & qcoeff + 0) = bb;

  get_BID128_fast (&res, sign, exp, qcoeff);

  BID_SWAP128 (res);
  return res;
}

UINT32
bid32_canonize (UINT32 ba) {
  FPSC bidrnd;
  unsigned int rnd = 0;

  UINT32 res;
  UINT32 sign, comb, exp;
  UINT32 trailing;
  UINT32 bcoeff;

  sign = (ba & 0x80000000);
  comb = (ba & 0x7ff00000) >> 20;
  trailing = (ba & 0x000fffff);

  if ((comb & 0x600) == 0x600) {	// G0..G1 = 11 -> exp is G2..G11
    exp = (comb >> 1) & 0xff;
    bcoeff = ((8 + (comb & 1)) << 20) | trailing;
  } else {
    exp = (comb >> 3) & 0xff;
    bcoeff = ((comb & 7) << 20) | trailing;
  }

  if ((comb & 0x7c0) == 0x7c0) {	//NaN
    ba &= 0xfe0fffff;	// make exponent 0
    bcoeff &= 0x000fffff;	// NaN payloat is only T.     
    if (bcoeff >= 1000000)
      ba &= 0xfff00000;	//treat non-canonical significand
    return ba;
  } else if ((comb & 0x780) == 0x780) {	//Inf
    ba &= 0xf8000000;	// make exponent and significand 0
    return ba;
  }

  if (bcoeff >= 10000000)
    bcoeff = 0;
  rnd = bidrnd = ROUNDING_TO_NEAREST;
  res = get_BID32 (sign, exp, bcoeff, rnd, &bidrnd);
  return res;
}

UINT64
bid64_canonize (UINT64 ba) {
  UINT64 res;
  UINT64 sign, comb, exp;
  UINT64 trailing;
  UINT64 bcoeff;

  sign = (ba & 0x8000000000000000ull);
  comb = (ba & 0x7ffc000000000000ull) >> 50;
  trailing = (ba & 0x0003ffffffffffffull);


  if ((comb & 0x1800) == 0x1800) {	// G0..G1 = 11 -> exp is G2..G11
    exp = (comb >> 1) & 0x3ff;
    bcoeff = ((8 + (comb & 1)) << 50) | trailing;
  } else {
    exp = (comb >> 3) & 0x3ff;
    bcoeff = ((comb & 7) << 50) | trailing;
  }

  if ((comb & 0x1f00) == 0x1f00) {	//NaN
    ba &= 0xfe03ffffffffffffULL;	// make exponent 0
    bcoeff &= 0x0003ffffffffffffull;	// NaN payloat is only T.  
    if (bcoeff >= 1000000000000000ull)
      ba &= 0xfe00000000000000ull;	// treat non canonical significand and zero G6-G12
    return ba;
  } else if ((comb & 0x1e00) == 0x1e00) {	//Inf
    ba &= 0xf800000000000000ULL;	// make exponent and significand 0
    return ba;
  }

  if (bcoeff >= 10000000000000000ull) {
    bcoeff = 0ull;
  }
  res = very_fast_get_BID64 (sign, exp, bcoeff);
  return res;
}

UINT128
bid128_canonize (UINT128 bq) {
  UINT128 res;
  UINT64 sign, comb, exp;
  UINT64 trailing;
  UINT64 bcoeff;

  UINT128 rq;
  UINT128 qcoeff;
  UINT64 ba, bb;

  ba = *((UINT64 *) & bq + HIGH_128W);
  bb = *((UINT64 *) & bq + LOW_128W);

  sign = (ba & 0x8000000000000000ull);
  comb = (ba & 0x7fffc00000000000ull) >> 46;
  trailing = (ba & 0x00003fffffffffffull);

  if ((comb & 0x18000) == 0x18000) {	// G0..G1 = 11 -> exp is G2..G11
    exp = (comb >> 1) & 0x3fff;
    bcoeff = ((8 + (comb & 1)) << 46) | trailing;
  } else {
    exp = (comb >> 3) & 0x3fff;
    bcoeff = ((comb & 7) << 46) | trailing;
  }

  if ((comb & 0x1f000) == 0x1f000) {	//NaN
    ba &= 0xfe003fffffffffffULL;	// make exponent 0
    bcoeff &= 0x00003fffffffffffull;	// NaN payload is only T.  

    if ((bcoeff > 0x0000314dc6448d93ULL) ||	// significand is non-canonical
	((bcoeff == 0x0000314dc6448d93ULL)
	 && (bb >= 0x38c15b0a00000000ULL))
	// significand is non-canonical
      ) {
      bcoeff = 0ull;
      ba &= ~0x00003fffffffffffull;
      bb = 0ull;
    }
    *((UINT64 *) & rq + HIGH_128W) = ba;
    *((UINT64 *) & rq + LOW_128W) = bb;
    return rq;
  } else if ((comb & 0x1e000) == 0x1e000) {	//Inf
    ba &= 0xf800000000000000ULL;	// make exponent and significand 0
    bb = 0;
    *((UINT64 *) & rq + HIGH_128W) = ba;
    *((UINT64 *) & rq + LOW_128W) = bb;
    return rq;
  }

  if ((bcoeff > 0x0001ed09bead87c0ull) ||	// significand is non-canonical
      ((bcoeff == 0x0001ed09bead87c0ull)
       && (bb > 0x378d8e63ffffffffull))
      // significand is non-canonical
    ) {
    bcoeff = 0ull;
    bb = 0ull;
  }

  *((UINT64 *) & qcoeff + 1) = bcoeff;
  *((UINT64 *) & qcoeff + 0) = bb;

  get_BID128_fast (&res, sign, exp, qcoeff);
  BID_SWAP128 (res);
  return res;
}
