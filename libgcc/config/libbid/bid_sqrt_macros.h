/* Copyright (C) 2007-2025 Free Software Foundation, Inc.

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

#ifndef _SQRT_MACROS_H_
#define _SQRT_MACROS_H_

#define FENCE __fence

#if DOUBLE_EXTENDED_ON

extern BINARY80 SQRT80 (BINARY80);


__BID_INLINE__ UINT64
short_sqrt128 (UINT128 A10) {
  BINARY80 lx, ly, l64;
  int_float f64;

  // 2^64
  f64.i = 0x5f800000;
  l64 = (BINARY80) f64.d;
  lx = (BINARY80) A10.w[1] * l64 + (BINARY80) A10.w[0];
  ly = SQRT80 (lx);
  return (UINT64) ly;
}


__BID_INLINE__ void
long_sqrt128 (UINT128 * pCS, UINT256 C256) {
  UINT256 C4;
  UINT128 CS;
  UINT64 X;
  SINT64 SE;
  BINARY80 l64, lm64, l128, lxL, lx, ly, lS, lSH, lSL, lE, l3, l2,
    l1, l0, lp, lCl;
  int_float fx, f64, fm64;
  int *ple = (int *) &lx;

  // 2^64
  f64.i = 0x5f800000;
  l64 = (BINARY80) f64.d;

  l128 = l64 * l64;
  lx = l3 = (BINARY80) C256.w[3] * l64 * l128;
  l2 = (BINARY80) C256.w[2] * l128;
  lx = FENCE (lx + l2);
  l1 = (BINARY80) C256.w[1] * l64;
  lx = FENCE (lx + l1);
  l0 = (BINARY80) C256.w[0];
  lx = FENCE (lx + l0);
  // sqrt(C256)
  lS = SQRT80 (lx);

  // get coefficient
  // 2^(-64)
  fm64.i = 0x1f800000;
  lm64 = (BINARY80) fm64.d;
  CS.w[1] = (UINT64) (lS * lm64);
  CS.w[0] = (UINT64) (lS - (BINARY80) CS.w[1] * l64);

  ///////////////////////////////////////
  //  CAUTION!
  //  little endian code only
  //  add solution for big endian
  //////////////////////////////////////
  lSH = lS;
  *((UINT64 *) & lSH) &= 0xffffffff00000000ull;

  // correction for C256 rounding
  lCl = FENCE (l3 - lx);
  lCl = FENCE (lCl + l2);
  lCl = FENCE (lCl + l1);
  lCl = FENCE (lCl + l0);

  lSL = lS - lSH;

  //////////////////////////////////////////
  //   Watch for compiler re-ordering
  //
  /////////////////////////////////////////
  // C256-S^2
  lxL = FENCE (lx - lSH * lSH);
  lp = lSH * lSL;
  lp += lp;
  lxL = FENCE (lxL - lp);
  lSL *= lSL;
  lxL = FENCE (lxL - lSL);
  lCl += lxL;

  // correction term
  lE = lCl / (lS + lS);

  // get low part of coefficient
  X = CS.w[0];
  if (lCl >= 0) {
    SE = (SINT64) (lE);
    CS.w[0] += SE;
    if (CS.w[0] < X)
      CS.w[1]++;
  } else {
    SE = (SINT64) (-lE);
    CS.w[0] -= SE;
    if (CS.w[0] > X)
      CS.w[1]--;
  }

  pCS->w[0] = CS.w[0];
  pCS->w[1] = CS.w[1];
}

#else

extern double sqrt (double);

__BID_INLINE__ UINT64
short_sqrt128 (UINT128 A10) {
  UINT256 ARS, ARS0, AE0, AE, S;

  UINT64 MY, ES, CY;
  double lx, l64;
  int_double f64, ly;
  int ey, k;

  // 2^64
  f64.i = 0x43f0000000000000ull;
  l64 = f64.d;
  lx = (double) A10.w[1] * l64 + (double) A10.w[0];
  ly.d = 1.0 / sqrt (lx);

  MY = (ly.i & 0x000fffffffffffffull) | 0x0010000000000000ull;
  ey = 0x3ff - (ly.i >> 52);

  // A10*RS^2
  __mul_64x128_to_192 (ARS0, MY, A10);
  __mul_64x192_to_256 (ARS, MY, ARS0);

  // shr by 2*ey+40, to get a 64-bit value
  k = (ey << 1) + 104 - 64;
  if (k >= 128) {
    if (k > 128)
      ES = (ARS.w[2] >> (k - 128)) | (ARS.w[3] << (192 - k));
    else
      ES = ARS.w[2];
  } else {
    if (k >= 64) {
      ARS.w[0] = ARS.w[1];
      ARS.w[1] = ARS.w[2];
      k -= 64;
    }
    if (k) {
      __shr_128 (ARS, ARS, k);
    }
    ES = ARS.w[0];
  }

  ES = ((SINT64) ES) >> 1;

  if (((SINT64) ES) < 0) {
    ES = -ES;

    // A*RS*eps (scaled by 2^64)
    __mul_64x192_to_256 (AE0, ES, ARS0);

    AE.w[0] = AE0.w[1];
    AE.w[1] = AE0.w[2];
    AE.w[2] = AE0.w[3];

    __add_carry_out (S.w[0], CY, ARS0.w[0], AE.w[0]);
    __add_carry_in_out (S.w[1], CY, ARS0.w[1], AE.w[1], CY);
    S.w[2] = ARS0.w[2] + AE.w[2] + CY;
  } else {
    // A*RS*eps (scaled by 2^64)
    __mul_64x192_to_256 (AE0, ES, ARS0);

    AE.w[0] = AE0.w[1];
    AE.w[1] = AE0.w[2];
    AE.w[2] = AE0.w[3];

    __sub_borrow_out (S.w[0], CY, ARS0.w[0], AE.w[0]);
    __sub_borrow_in_out (S.w[1], CY, ARS0.w[1], AE.w[1], CY);
    S.w[2] = ARS0.w[2] - AE.w[2] - CY;
  }

  k = ey + 51;

  if (k >= 64) {
    if (k >= 128) {
      S.w[0] = S.w[2];
      S.w[1] = 0;
      k -= 128;
    } else {
      S.w[0] = S.w[1];
      S.w[1] = S.w[2];
    }
    k -= 64;
  }
  if (k) {
    __shr_128 (S, S, k);
  }


  return (UINT64) ((S.w[0] + 1) >> 1);

}



__BID_INLINE__ void
long_sqrt128 (UINT128 * pCS, UINT256 C256) {
  UINT512 ARS0, ARS;
  UINT256 ARS00, AE, AE2, S;
  UINT128 ES, ES2, ARS1;
  UINT64 ES32, CY, MY;
  double l64, l128, lx, l2, l1, l0;
  int_double f64, ly;
  int ey, k, k2;

  // 2^64
  f64.i = 0x43f0000000000000ull;
  l64 = f64.d;

  l128 = l64 * l64;
  lx = (double) C256.w[3] * l64 * l128;
  l2 = (double) C256.w[2] * l128;
  lx = FENCE (lx + l2);
  l1 = (double) C256.w[1] * l64;
  lx = FENCE (lx + l1);
  l0 = (double) C256.w[0];
  lx = FENCE (lx + l0);
  // sqrt(C256)
  ly.d = 1.0 / sqrt (lx);

  MY = (ly.i & 0x000fffffffffffffull) | 0x0010000000000000ull;
  ey = 0x3ff - (ly.i >> 52);

  // A10*RS^2, scaled by 2^(2*ey+104)
  __mul_64x256_to_320 (ARS0, MY, C256);
  __mul_64x320_to_384 (ARS, MY, ARS0);

  // shr by k=(2*ey+104)-128
  // expect k is in the range (192, 256) if result in [10^33, 10^34)
  // apply an additional signed shift by 1 at the same time (to get eps=eps0/2)
  k = (ey << 1) + 104 - 128 - 192;
  k2 = 64 - k;
  ES.w[0] = (ARS.w[3] >> (k + 1)) | (ARS.w[4] << (k2 - 1));
  ES.w[1] = (ARS.w[4] >> k) | (ARS.w[5] << k2);
  ES.w[1] = ((SINT64) ES.w[1]) >> 1;

  // A*RS >> 192 (for error term computation)
  ARS1.w[0] = ARS0.w[3];
  ARS1.w[1] = ARS0.w[4];

  // A*RS>>64
  ARS00.w[0] = ARS0.w[1];
  ARS00.w[1] = ARS0.w[2];
  ARS00.w[2] = ARS0.w[3];
  ARS00.w[3] = ARS0.w[4];

  if (((SINT64) ES.w[1]) < 0) {
    ES.w[0] = -ES.w[0];
    ES.w[1] = -ES.w[1];
    if (ES.w[0])
      ES.w[1]--;

    // A*RS*eps
    __mul_128x128_to_256 (AE, ES, ARS1);

    __add_carry_out (S.w[0], CY, ARS00.w[0], AE.w[0]);
    __add_carry_in_out (S.w[1], CY, ARS00.w[1], AE.w[1], CY);
    __add_carry_in_out (S.w[2], CY, ARS00.w[2], AE.w[2], CY);
    S.w[3] = ARS00.w[3] + AE.w[3] + CY;
  } else {
    // A*RS*eps
    __mul_128x128_to_256 (AE, ES, ARS1);

    __sub_borrow_out (S.w[0], CY, ARS00.w[0], AE.w[0]);
    __sub_borrow_in_out (S.w[1], CY, ARS00.w[1], AE.w[1], CY);
    __sub_borrow_in_out (S.w[2], CY, ARS00.w[2], AE.w[2], CY);
    S.w[3] = ARS00.w[3] - AE.w[3] - CY;
  }

  // 3/2*eps^2, scaled by 2^128
  ES32 = ES.w[1] + (ES.w[1] >> 1);
  __mul_64x64_to_128 (ES2, ES32, ES.w[1]);
  // A*RS*3/2*eps^2
  __mul_128x128_to_256 (AE2, ES2, ARS1);

  // result, scaled by 2^(ey+52-64)
  __add_carry_out (S.w[0], CY, S.w[0], AE2.w[0]);
  __add_carry_in_out (S.w[1], CY, S.w[1], AE2.w[1], CY);
  __add_carry_in_out (S.w[2], CY, S.w[2], AE2.w[2], CY);
  S.w[3] = S.w[3] + AE2.w[3] + CY;

  // k in (0, 64)
  k = ey + 51 - 128;
  k2 = 64 - k;
  S.w[0] = (S.w[1] >> k) | (S.w[2] << k2);
  S.w[1] = (S.w[2] >> k) | (S.w[3] << k2);

  // round to nearest
  S.w[0]++;
  if (!S.w[0])
    S.w[1]++;

  pCS->w[0] = (S.w[1] << 63) | (S.w[0] >> 1);
  pCS->w[1] = S.w[1] >> 1;

}

#endif
#endif
