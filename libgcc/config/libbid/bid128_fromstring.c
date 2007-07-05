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

#define MAX_FORMAT_DIGITS_128   34
#define MAX_STRING_DIGITS_128   100
#define MAX_SEARCH              MAX_STRING_DIGITS_128-MAX_FORMAT_DIGITS_128-1


#if DECIMAL_CALL_BY_REFERENCE

void
__bid128_from_string (UINT128 * pres,
                    char *ps _RND_MODE_PARAM _EXC_FLAGS_PARAM
                    _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#else

UINT128
__bid128_from_string (char *ps _RND_MODE_PARAM _EXC_FLAGS_PARAM
                    _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT128 CX, res;
  UINT64 sign_x, coeff_high, coeff_low, coeff2, coeff_l2, carry,
    scale_high, right_radix_leading_zeros;
  int ndigits_before, ndigits_after, ndigits_total, dec_expon, sgn_exp,
    i, d2, rdx_pt_enc;
  char c, buffer[MAX_STRING_DIGITS_128];

#if DECIMAL_CALL_BY_REFERENCE
#if !DECIMAL_GLOBAL_ROUNDING
  _IDEC_round rnd_mode = *prnd_mode;
#endif
#endif

  right_radix_leading_zeros = rdx_pt_enc = 0;

  // if null string, return NaN
  if (!ps) {
    res.w[1] = 0x7c00000000000000ull;
    res.w[0] = 0;
    BID_RETURN (res);
  }
  // eliminate leading white space
  while ((*ps == ' ') || (*ps == '\t'))
    ps++;

  // c gets first character
  c = *ps;


  // if c is null or not equal to a (radix point, negative sign, 
  // positive sign, or number) it might be SNaN, sNaN, Infinity
  if (!c
      || (c != '.' && c != '-' && c != '+'
          && ((unsigned) (c - '0') > 9))) {
    res.w[0] = 0;
    // Infinity?
    if ((tolower_macro (ps[0]) == 'i' && tolower_macro (ps[1]) == 'n'
         && tolower_macro (ps[2]) == 'f')
        && (!ps[3]
            || (tolower_macro (ps[3]) == 'i'
                && tolower_macro (ps[4]) == 'n'
                && tolower_macro (ps[5]) == 'i'
                && tolower_macro (ps[6]) == 't'
                && tolower_macro (ps[7]) == 'y' && !ps[8])
        )) {
      res.w[1] = 0x7800000000000000ull;
      BID_RETURN (res);
    }
    // return sNaN
    if (tolower_macro (ps[0]) == 's' && tolower_macro (ps[1]) == 'n' && 
        tolower_macro (ps[2]) == 'a' && tolower_macro (ps[3]) == 'n') {        
        // case insensitive check for snan
      res.w[1] = 0x7e00000000000000ull;
      BID_RETURN (res);
    } else {
      // return qNaN
      res.w[1] = 0x7c00000000000000ull;
      BID_RETURN (res);
    }
  }
  // if +Inf, -Inf, +Infinity, or -Infinity (case insensitive check for inf)   
  if ((tolower_macro (ps[1]) == 'i' && tolower_macro (ps[2]) == 'n' && 
      tolower_macro (ps[3]) == 'f') && (!ps[4] || 
      (tolower_macro (ps[4]) == 'i' && tolower_macro (ps[5]) == 'n' && 
      tolower_macro (ps[6]) == 'i' && tolower_macro (ps[7]) == 't' && 
      tolower_macro (ps[8]) == 'y' && !ps[9]))) { // ci check for infinity
    res.w[0] = 0;

    if (c == '+')
      res.w[1] = 0x7800000000000000ull;
    else if (c == '-')
      res.w[1] = 0xf800000000000000ull;
    else
      res.w[1] = 0x7c00000000000000ull;

    BID_RETURN (res);
  }
  // if +sNaN, +SNaN, -sNaN, or -SNaN
  if (tolower_macro (ps[1]) == 's' && tolower_macro (ps[2]) == 'n'
      && tolower_macro (ps[3]) == 'a' && tolower_macro (ps[4]) == 'n') {
    res.w[0] = 0;
    if (c == '-')
      res.w[1] = 0xfe00000000000000ull;
    else
      res.w[1] = 0x7e00000000000000ull;
    BID_RETURN (res);
  }
  // set up sign_x to be OR'ed with the upper word later
  if (c == '-')
    sign_x = 0x8000000000000000ull;
  else
    sign_x = 0;

  // go to next character if leading sign
  if (c == '-' || c == '+')
    ps++;

  c = *ps;

  // if c isn't a decimal point or a decimal digit, return NaN
  if (c != '.' && ((unsigned) (c - '0') > 9)) {
    res.w[1] = 0x7c00000000000000ull | sign_x;
    res.w[0] = 0;
    BID_RETURN (res);
  }
  // detect zero (and eliminate/ignore leading zeros)
  if (*(ps) == '0') {

    // if all numbers are zeros (with possibly 1 radix point, the number is zero
    // should catch cases such as: 000.0
    while (*ps == '0') {

      ps++;

      // for numbers such as 0.0000000000000000000000000000000000001001, 
      // we want to count the leading zeros
      if (rdx_pt_enc) {
        right_radix_leading_zeros++;
      }
      // if this character is a radix point, make sure we haven't already 
      // encountered one
      if (*(ps) == '.') {
        if (rdx_pt_enc == 0) {
          rdx_pt_enc = 1;
          // if this is the first radix point, and the next character is NULL, 
          // we have a zero
          if (!*(ps + 1)) {
            res.w[1] =
              (0x3040000000000000ull -
               (right_radix_leading_zeros << 49)) | sign_x;
            res.w[0] = 0;
            BID_RETURN (res);
          }
          ps = ps + 1;
        } else {
          // if 2 radix points, return NaN
          res.w[1] = 0x7c00000000000000ull | sign_x;
          res.w[0] = 0;
          BID_RETURN (res);
        }
      } else if (!*(ps)) {
        //res.w[1] = 0x3040000000000000ull | sign_x;
        res.w[1] =
          (0x3040000000000000ull -
           (right_radix_leading_zeros << 49)) | sign_x;
        res.w[0] = 0;
        BID_RETURN (res);
      }
    }
  }

  c = *ps;

  // initialize local variables
  ndigits_before = ndigits_after = ndigits_total = 0;
  sgn_exp = 0;
  // pstart_coefficient = ps;

  if (!rdx_pt_enc) {
    // investigate string (before radix point)
    while ((unsigned) (c - '0') <= 9
           && ndigits_before < MAX_STRING_DIGITS_128) {
      buffer[ndigits_before] = c;
      ps++;
      c = *ps;
      ndigits_before++;
    }

    ndigits_total = ndigits_before;
    if (c == '.') {
      ps++;
      if ((c = *ps)) {

        // investigate string (after radix point)
        while ((unsigned) (c - '0') <= 9
               && ndigits_total < MAX_STRING_DIGITS_128) {
          buffer[ndigits_total] = c;
          ps++;
          c = *ps;
          ndigits_total++;
        }
        ndigits_after = ndigits_total - ndigits_before;
      }
    }
  } else {
    // we encountered a radix point while detecting zeros
    //if (c = *ps){

    c = *ps;
    ndigits_total = 0;
    // investigate string (after radix point)
    while ((unsigned) (c - '0') <= 9
           && ndigits_total < MAX_STRING_DIGITS_128) {
      buffer[ndigits_total] = c;
      ps++;
      c = *ps;
      ndigits_total++;
    }
    ndigits_after = ndigits_total - ndigits_before;
  }

  // get exponent
  dec_expon = 0;
  if (ndigits_total < MAX_STRING_DIGITS_128) {
    if (c) {
      if (c != 'e' && c != 'E') {
        // return NaN
        res.w[1] = 0x7c00000000000000ull;
        res.w[0] = 0;
        BID_RETURN (res);
      }
      ps++;
      c = *ps;

      if (((unsigned) (c - '0') > 9)
          && ((c != '+' && c != '-') || (unsigned) (ps[1] - '0') > 9)) {
        // return NaN
        res.w[1] = 0x7c00000000000000ull;
        res.w[0] = 0;
        BID_RETURN (res);
      }

      if (c == '-') {
        sgn_exp = -1;
        ps++;
        c = *ps;
      } else if (c == '+') {
        ps++;
        c = *ps;
      }

      dec_expon = c - '0';
      i = 1;
      ps++;
      c = *ps - '0';
      while (((unsigned) c) <= 9 && i < 7) {
        d2 = dec_expon + dec_expon;
        dec_expon = (d2 << 2) + d2 + c;
        ps++;
        c = *ps - '0';
        i++;
      }
    }

    dec_expon = (dec_expon + sgn_exp) ^ sgn_exp;
  }


  if (ndigits_total <= MAX_FORMAT_DIGITS_128) {
    dec_expon +=
      DECIMAL_EXPONENT_BIAS_128 - ndigits_after -
      right_radix_leading_zeros;
    if (dec_expon < 0) {
      res.w[1] = 0 | sign_x;
      res.w[0] = 0;
    }
    if (ndigits_total == 0) {
      CX.w[0] = 0;
      CX.w[1] = 0;
    } else if (ndigits_total <= 19) {
      coeff_high = buffer[0] - '0';
      for (i = 1; i < ndigits_total; i++) {
        coeff2 = coeff_high + coeff_high;
        coeff_high = (coeff2 << 2) + coeff2 + buffer[i] - '0';
      }
      CX.w[0] = coeff_high;
      CX.w[1] = 0;
    } else {
      coeff_high = buffer[0] - '0';
      for (i = 1; i < ndigits_total - 17; i++) {
        coeff2 = coeff_high + coeff_high;
        coeff_high = (coeff2 << 2) + coeff2 + buffer[i] - '0';
      }
      coeff_low = buffer[i] - '0';
      i++;
      for (; i < ndigits_total; i++) {
        coeff_l2 = coeff_low + coeff_low;
        coeff_low = (coeff_l2 << 2) + coeff_l2 + buffer[i] - '0';
      }
      // now form the coefficient as coeff_high*10^19+coeff_low+carry
      scale_high = 100000000000000000ull;
      __mul_64x64_to_128_fast (CX, coeff_high, scale_high);

      CX.w[0] += coeff_low;
      if (CX.w[0] < coeff_low)
        CX.w[1]++;
    }
    get_BID128_string (&res, sign_x, dec_expon, CX);
    BID_RETURN (res);
  } else {
    // simply round using the digits that were read

    dec_expon +=
      ndigits_before + DECIMAL_EXPONENT_BIAS_128 -
      MAX_FORMAT_DIGITS_128 - right_radix_leading_zeros;

    if (dec_expon < 0) {
      res.w[1] = 0 | sign_x;
      res.w[0] = 0;
    }

    coeff_high = buffer[0] - '0';
    for (i = 1; i < MAX_FORMAT_DIGITS_128 - 17; i++) {
      coeff2 = coeff_high + coeff_high;
      coeff_high = (coeff2 << 2) + coeff2 + buffer[i] - '0';
    }
    coeff_low = buffer[i] - '0';
    i++;
    for (; i < MAX_FORMAT_DIGITS_128; i++) {
      coeff_l2 = coeff_low + coeff_low;
      coeff_low = (coeff_l2 << 2) + coeff_l2 + buffer[i] - '0';
    }
    carry = ((unsigned) ('4' - buffer[i])) >> 31;
    if ((buffer[i] == '5' && !(coeff_low & 1)) || dec_expon < 0) {
      if (dec_expon >= 0) {
        carry = 0;
        i++;
      }
      for (; i < ndigits_total; i++) {
        if (buffer[i] > '0') {
          carry = 1;
          break;
        }
      }
    }
    // now form the coefficient as coeff_high*10^17+coeff_low+carry
    scale_high = 100000000000000000ull;
    if (dec_expon < 0) {
      if (dec_expon > -MAX_FORMAT_DIGITS_128) {
        scale_high = 1000000000000000000ull;
        coeff_low = (coeff_low << 3) + (coeff_low << 1);
        dec_expon--;
      }
      if (dec_expon == -MAX_FORMAT_DIGITS_128
          && coeff_high > 50000000000000000ull)
        carry = 0;
    }

    __mul_64x64_to_128_fast (CX, coeff_high, scale_high);

    coeff_low += carry;
    CX.w[0] += coeff_low;
    if (CX.w[0] < coeff_low)
      CX.w[1]++;

    get_BID128_string (&res, sign_x, dec_expon, CX);
    BID_RETURN (res);
  }
}
