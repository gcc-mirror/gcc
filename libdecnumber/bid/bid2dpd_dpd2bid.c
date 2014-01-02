/* Copyright (C) 2007-2014 Free Software Foundation, Inc.

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

#undef IN_LIBGCC2
#include "bid-dpd.h"

/* get full 64x64bit product */
#define __mul_64x64_to_128(P, CX, CY)             \
{                                                 \
  UINT64 CXH, CXL, CYH,CYL,PL,PH,PM,PM2;  \
  CXH = (CX) >> 32;                               \
  CXL = (UINT32)(CX);                             \
  CYH = (CY) >> 32;                               \
  CYL = (UINT32)(CY);                             \
                                                  \
  PM = CXH*CYL;                                   \
  PH = CXH*CYH;                                   \
  PL = CXL*CYL;                                   \
  PM2 = CXL*CYH;                                  \
  PH += (PM>>32);                                 \
  PM = (UINT64)((UINT32)PM)+PM2+(PL>>32);         \
                                                  \
  (P).w[1] = PH + (PM>>32);                       \
  (P).w[0] = (PM<<32)+(UINT32)PL;                 \
}

/* add 64-bit value to 128-bit */
#define __add_128_64(R128, A128, B64)             \
{                                                 \
  UINT64 R64H;                                    \
  R64H = (A128).w[1];                             \
  (R128).w[0] = (B64) + (A128).w[0];              \
  if((R128).w[0] < (B64)) R64H ++;                \
  (R128).w[1] = R64H;                             \
}

/* add 128-bit value to 128-bit (assume no carry-out) */
#define __add_128_128(R128, A128, B128)           \
{                                                 \
  UINT128 Q128;                                   \
  Q128.w[1] = (A128).w[1]+(B128).w[1];            \
  Q128.w[0] = (B128).w[0] + (A128).w[0];          \
  if(Q128.w[0] < (B128).w[0]) Q128.w[1] ++;       \
  (R128).w[1] = Q128.w[1];                        \
  (R128).w[0] = Q128.w[0];                        \
}

#define __mul_128x128_high(Q, A, B)               \
{                                                 \
  UINT128 ALBL, ALBH, AHBL, AHBH, QM, QM2;        \
                                                  \
  __mul_64x64_to_128(ALBH, (A).w[0], (B).w[1]);   \
  __mul_64x64_to_128(AHBL, (B).w[0], (A).w[1]);   \
  __mul_64x64_to_128(ALBL, (A).w[0], (B).w[0]);   \
  __mul_64x64_to_128(AHBH, (A).w[1],(B).w[1]);    \
                                                  \
  __add_128_128(QM, ALBH, AHBL);                  \
  __add_128_64(QM2, QM, ALBL.w[1]);               \
  __add_128_64((Q), AHBH, QM2.w[1]);              \
}

#include "bid2dpd_dpd2bid.h"

static const unsigned int dm103[] =
  { 0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000 };

void _bid_to_dpd32 (_Decimal32 *, _Decimal32 *);

void
_bid_to_dpd32 (_Decimal32 *pres, _Decimal32 *px) {
  unsigned int sign, coefficient_x, exp, dcoeff;
  unsigned int b2, b1, b0, b01, res;
  _Decimal32 x = *px;

  sign = (x & 0x80000000);
  if ((x & 0x60000000ul) == 0x60000000ul) {
    /* special encodings */
    if ((x & 0x78000000ul) == 0x78000000ul) {
      *pres = x; /* NaN or Infinity */
      return;
    }
    /* coefficient */
    coefficient_x = (x & 0x001ffffful) | 0x00800000ul;
    if (coefficient_x >= 10000000) coefficient_x = 0;
    /* get exponent */
    exp = (x >> 21) & 0xff;
  } else {
    exp = (x >> 23) & 0xff;
    coefficient_x = (x & 0x007ffffful);
  }
  b01 = coefficient_x / 1000;
  b2 = coefficient_x - 1000 * b01;
  b0 = b01 / 1000;
  b1 = b01 - 1000 * b0;
  dcoeff = b2d[b2] | b2d2[b1];
  if (b0 >= 8) { /* is b0 8 or 9? */
    res = sign | ((0x600 | ((exp >> 6) << 7) | 
        ((b0 & 1) << 6) | (exp & 0x3f)) << 20) | dcoeff;
  } else { /* else b0 is 0..7 */
    res = sign | ((((exp >> 6) << 9) | (b0 << 6) | 
        (exp & 0x3f)) << 20) | dcoeff;
  }
  *pres = res;
}

void _dpd_to_bid32 (_Decimal32 *, _Decimal32 *);

void
_dpd_to_bid32 (_Decimal32 *pres, _Decimal32 *px) {
  unsigned int r;
  unsigned int sign, exp, bcoeff;
  UINT64 trailing;
  unsigned int d0, d1, d2;
  _Decimal32 x = *px;

  sign = (x & 0x80000000);
  trailing = (x & 0x000fffff);
  if ((x & 0x78000000) == 0x78000000) {
    *pres = x;
    return;
  } else { /* normal number */
    if ((x & 0x60000000) == 0x60000000) { /* G0..G1 = 11 -> d0 = 8 + G4 */
      d0 = d2b3[((x >> 26) & 1) | 8]; /* d0 = (comb & 0x0100 ? 9 : 8); */
      exp = (x >> 27) & 3; /* exp leading bits are G2..G3 */
    } else {
      d0 = d2b3[(x >> 26) & 0x7];
      exp = (x >> 29) & 3; /* exp loading bits are G0..G1 */
    }
    d1 = d2b2[(trailing >> 10) & 0x3ff];
    d2 = d2b[(trailing) & 0x3ff];
    bcoeff = d2 + d1 + d0;
    exp = (exp << 6) + ((x >> 20) & 0x3f);
    if (bcoeff < (1 << 23)) {
      r = exp;
      r <<= 23;
      r |= (bcoeff | sign);
    } else {
      r = exp;
      r <<= 21;
      r |= (sign | 0x60000000ul);
      /* add coeff, without leading bits */
      r |= (((unsigned int) bcoeff) & 0x1fffff);
    }
  }
  *pres = r;
}

void _bid_to_dpd64 (_Decimal64 *, _Decimal64 *);

void
_bid_to_dpd64 (_Decimal64 *pres, _Decimal64 *px) {
  UINT64 res;
  UINT64 sign, comb, exp, B34, B01;
  UINT64 d103, D61;
  UINT64 b0, b2, b3, b5;
  unsigned int b1, b4;
  UINT64 bcoeff;
  UINT64 dcoeff;
  unsigned int yhi, ylo;
  _Decimal64 x = *px;

  sign = (x & 0x8000000000000000ull);
  comb = (x & 0x7ffc000000000000ull) >> 51;
  if ((comb & 0xf00) == 0xf00) {
    *pres = x;
    return;
  } else { /* Normal number */
    if ((comb & 0xc00) == 0xc00) { /* G0..G1 = 11 -> exp is G2..G11 */
      exp = (comb) & 0x3ff;
      bcoeff = (x & 0x0007ffffffffffffull) | 0x0020000000000000ull;
    } else {
      exp = (comb >> 2) & 0x3ff;
      bcoeff = (x & 0x001fffffffffffffull);
    }
    D61 = 2305843009ull; /* Floor(2^61 / 10^9) */
    /* Multiply the binary coefficient by ceil(2^64 / 1000), and take the upper
       64-bits in order to compute a division by 1000. */
    yhi = (D61 * (UINT64)(bcoeff >> (UINT64)27)) >> (UINT64)34;
    ylo = bcoeff - 1000000000ull * yhi;
    if (ylo >= 1000000000) {
      ylo = ylo - 1000000000;
      yhi = yhi + 1;
    }
    d103 = 0x4189374c;
    B34 = ((UINT64) ylo * d103) >> (32 + 8);
    B01 = ((UINT64) yhi * d103) >> (32 + 8);
    b5 = ylo - B34 * 1000;
    b2 = yhi - B01 * 1000;
    b3 = ((UINT64) B34 * d103) >> (32 + 8);
    b0 = ((UINT64) B01 * d103) >> (32 + 8);
    b4 = (unsigned int) B34 - (unsigned int) b3 *1000;
    b1 = (unsigned int) B01 - (unsigned int) dm103[b0];
    dcoeff = b2d[b5] | b2d2[b4] | b2d3[b3] | b2d4[b2] | b2d5[b1];
    if (b0 >= 8) /* is b0 8 or 9? */
      res = sign | ((0x1800 | ((exp >> 8) << 9) | ((b0 & 1) << 8) | 
          (exp & 0xff)) << 50) | dcoeff;
    else /* else b0 is 0..7 */
      res = sign | ((((exp >> 8) << 11) | (b0 << 8) | 
          (exp & 0xff)) << 50) | dcoeff;
  }
  *pres = res;
}

void _dpd_to_bid64 (_Decimal64 *, _Decimal64 *);

void
_dpd_to_bid64 (_Decimal64 *pres, _Decimal64 *px) {
  UINT64 res;
  UINT64 sign, comb, exp;
  UINT64 trailing;
  UINT64 d0, d1, d2;
  unsigned int d3, d4, d5;
  UINT64 bcoeff, mask;
  _Decimal64 x = *px;

  sign = (x & 0x8000000000000000ull);
  comb = (x & 0x7ffc000000000000ull) >> 50;
  trailing = (x & 0x0003ffffffffffffull);
  if ((comb & 0x1e00) == 0x1e00) {
    if ((comb & 0x1f00) == 0x1f00) { /* G0..G4 = 11111 -> NaN */
      if (comb & 0x0100) { /* G5 = 1 -> sNaN */
        *pres = x;
      } else { /* G5 = 0 -> qNaN */
        *pres = x;
      }
    } else { /*if ((comb & 0x1e00) == 0x1e00); G0..G4 = 11110 -> INF */
      *pres = x;
    }
    return;
  } else { /* normal number */
    if ((comb & 0x1800) == 0x1800) { /* G0..G1 = 11 -> d0 = 8 + G4 */
      d0 = d2b6[((comb >> 8) & 1) | 8]; /* d0 = (comb & 0x0100 ? 9 : 8); */
      exp = (comb & 0x600) >> 1; /* exp = (comb & 0x0400 ? 1 : 0) * 0x200 + 
          (comb & 0x0200 ? 1 : 0) * 0x100; exp leading bits are G2..G3 */
    } else {
      d0 = d2b6[(comb >> 8) & 0x7];
      exp = (comb & 0x1800) >> 3; /* exp = (comb & 0x1000 ? 1 : 0) * 0x200 + 
          (comb & 0x0800 ? 1 : 0) * 0x100; exp loading bits are G0..G1 */
    }
    d1 = d2b5[(trailing >> 40) & 0x3ff];
    d2 = d2b4[(trailing >> 30) & 0x3ff];
    d3 = d2b3[(trailing >> 20) & 0x3ff];
    d4 = d2b2[(trailing >> 10) & 0x3ff];
    d5 = d2b[(trailing) & 0x3ff];
    bcoeff = (d5 + d4 + d3) + d2 + d1 + d0;
    exp += (comb & 0xff);
    mask = 1;
    mask <<= 53;
    if (bcoeff < mask) { /* check whether coefficient fits in 10*5+3 bits */
      res = exp;
      res <<= 53;
      res |= (bcoeff | sign);
      *pres = res;
      return;
    }
    /* special format */
    res = (exp << 51) | (sign | 0x6000000000000000ull);
    /* add coeff, without leading bits */
    mask = (mask >> 2) - 1;
    bcoeff &= mask;
    res |= bcoeff;
  }
  *pres = res;
}

void _bid_to_dpd128 (_Decimal128 *, _Decimal128 *);

void
_bid_to_dpd128 (_Decimal128 *pres, _Decimal128 *px) {
  UINT128 res;
  UINT128 sign;
  unsigned int comb;
  UINT128 bcoeff;
  UINT128 dcoeff;
  UINT128 BH, d1018, BT2, BT1;
  UINT64 exp, BL, d109;
  UINT64 d106, d103;
  UINT64 k1, k2, k4, k5, k7, k8, k10, k11;
  unsigned int BHH32, BLL32, BHL32, BLH32, k0, k3, k6, k9, amount;
  _Decimal128 x = *px;

  sign.w[1] = (x.w[1] & 0x8000000000000000ull);
  sign.w[0] = 0;
  comb = (x.w[1] /*& 0x7fffc00000000000ull */ ) >> 46;
  exp = 0;
  if ((comb & 0x1e000) == 0x1e000) {
    if ((comb & 0x1f000) == 0x1f000) { /* G0..G4 = 11111 -> NaN */
      if (comb & 0x01000) { /* G5 = 1 -> sNaN */
        res = x;
      } else { /* G5 = 0 -> qNaN */
        res = x;
      }
    } else { /* G0..G4 = 11110 -> INF */
      res = x;
    }
  } else { /* normal number */
    exp = ((x.w[1] & 0x7fff000000000000ull) >> 49) & 0x3fff;
    bcoeff.w[1] = (x.w[1] & 0x0001ffffffffffffull);
    bcoeff.w[0] = x.w[0];
    d1018 = reciprocals10_128[18];
    __mul_128x128_high (BH, bcoeff, d1018);
    amount = recip_scale[18];
    BH.w[0] = (BH.w[0] >> amount) | (BH.w[1] << (64 - amount));
    BL = bcoeff.w[0] - BH.w[0] * 1000000000000000000ull;
    d109 = reciprocals10_64[9];
    __mul_64x64_to_128 (BT1, BH.w[0], d109);
    BHH32 = (unsigned int) (BT1.w[1] >> short_recip_scale[9]);
    BHL32 = (unsigned int) BH.w[0] - BHH32 * 1000000000;
    __mul_64x64_to_128 (BT2, BL, d109);
    BLH32 = (unsigned int) (BT2.w[1] >> short_recip_scale[9]);
    BLL32 = (unsigned int) BL - BLH32 * 1000000000;
    d106 = 0x431BDE83;
    d103 = 0x4189374c;
    k0 = ((UINT64) BHH32 * d106) >> (32 + 18);
    BHH32 -= (unsigned int) k0 *1000000;
    k1 = ((UINT64) BHH32 * d103) >> (32 + 8);
    k2 = BHH32 - (unsigned int) k1 *1000;
    k3 = ((UINT64) BHL32 * d106) >> (32 + 18);
    BHL32 -= (unsigned int) k3 *1000000;
    k4 = ((UINT64) BHL32 * d103) >> (32 + 8);
    k5 = BHL32 - (unsigned int) k4 *1000;
    k6 = ((UINT64) BLH32 * d106) >> (32 + 18);
    BLH32 -= (unsigned int) k6 *1000000;
    k7 = ((UINT64) BLH32 * d103) >> (32 + 8);
    k8 = BLH32 - (unsigned int) k7 *1000;
    k9 = ((UINT64) BLL32 * d106) >> (32 + 18);
    BLL32 -= (unsigned int) k9 *1000000;
    k10 = ((UINT64) BLL32 * d103) >> (32 + 8);
    k11 = BLL32 - (unsigned int) k10 *1000;
    dcoeff.w[1] = (b2d[k5] >> 4) | (b2d[k4] << 6) | (b2d[k3] << 16) | 
        (b2d[k2] << 26) | (b2d[k1] << 36);
    dcoeff.w[0] = b2d[k11] | (b2d[k10] << 10) | (b2d[k9] << 20) | 
        (b2d[k8] << 30) | (b2d[k7] << 40) | (b2d[k6] << 50) | (b2d[k5] << 60);
    res.w[0] = dcoeff.w[0];
    if (k0 >= 8) {
      res.w[1] = sign.w[1] | ((0x18000 | ((exp >> 12) << 13) | 
          ((k0 & 1) << 12) | (exp & 0xfff)) << 46) | dcoeff.w[1];
    } else {
      res.w[1] = sign.w[1] | ((((exp >> 12) << 15) | (k0 << 12) | 
          (exp & 0xfff)) << 46) | dcoeff.w[1];
    }
  }
  *pres = res;
}

void _dpd_to_bid128 (_Decimal128 *, _Decimal128 *);

void
_dpd_to_bid128 (_Decimal128 *pres, _Decimal128 *px) {
  UINT128 res;
  UINT128 sign;
  UINT64 exp, comb;
  UINT128 trailing;
  UINT64 d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11;
  UINT128 bcoeff;
  UINT64 tl, th;
  _Decimal128 x = *px;

  sign.w[1] = (x.w[1] & 0x8000000000000000ull);
  sign.w[0] = 0;
  comb = (x.w[1] & 0x7fffc00000000000ull) >> 46;
  trailing.w[1] = x.w[1];
  trailing.w[0] = x.w[0];
  if ((comb & 0x1e000) == 0x1e000) {
    if ((comb & 0x1f000) == 0x1f000) { /* G0..G4 = 11111 -> NaN */
      if (comb & 0x01000) { /* G5 = 1 -> sNaN */
        *pres = x;
      } else { /* G5 = 0 -> qNaN */
        *pres = x;
      }
    } else { /* G0..G4 = 11110 -> INF */
      *pres = x;
    }
    return;
  } else { /* Normal number */
    if ((comb & 0x18000) == 0x18000) { /* G0..G1 = 11 -> d0 = 8 + G4 */
      d0 = d2b6[8 + ((comb & 0x01000) >> 12)];
      exp = (comb & 0x06000) >> 1;  /* exp leading bits are G2..G3 */
    } else {
      d0 = d2b6[((comb & 0x07000) >> 12)];
      exp = (comb & 0x18000) >> 3;  /* exp loading bits are G0..G1 */
    }
    d11 = d2b[(trailing.w[0]) & 0x3ff];
    d10 = d2b2[(trailing.w[0] >> 10) & 0x3ff];
    d9 = d2b3[(trailing.w[0] >> 20) & 0x3ff];
    d8 = d2b4[(trailing.w[0] >> 30) & 0x3ff];
    d7 = d2b5[(trailing.w[0] >> 40) & 0x3ff];
    d6 = d2b6[(trailing.w[0] >> 50) & 0x3ff];
    d5 = d2b[(trailing.w[0] >> 60) | ((trailing.w[1] & 0x3f) << 4)];
    d4 = d2b2[(trailing.w[1] >> 6) & 0x3ff];
    d3 = d2b3[(trailing.w[1] >> 16) & 0x3ff];
    d2 = d2b4[(trailing.w[1] >> 26) & 0x3ff];
    d1 = d2b5[(trailing.w[1] >> 36) & 0x3ff];
    tl = d11 + d10 + d9 + d8 + d7 + d6;
    th = d5 + d4 + d3 + d2 + d1 + d0;
    __mul_64x64_to_128 (bcoeff, th, 1000000000000000000ull);
    __add_128_64 (bcoeff, bcoeff, tl);
    exp += (comb & 0xfff);
    res.w[0] = bcoeff.w[0];
    res.w[1] = (exp << 49) | sign.w[1] | bcoeff.w[1];
  }
  *pres = res;
}
