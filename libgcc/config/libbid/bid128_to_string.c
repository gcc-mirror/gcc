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

/*****************************************************************************
 *    BID128_to_string
 ****************************************************************************/

#include <stdio.h>
#include "bid_internal.h"
#include "bid128_2_str.h"
#include "bid128_2_str_macros.h"

extern int __bid128_coeff_2_string (UINT64 X_hi, UINT64 X_lo,
				  char *char_ptr);

#if DECIMAL_CALL_BY_REFERENCE

void
__bid128_to_string (char *str,
		  UINT128 *
		  px _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
		  _EXC_INFO_PARAM) {
  UINT128 x;
#else

void
__bid128_to_string (char *str,
		  UINT128 x _RND_MODE_PARAM _EXC_FLAGS_PARAM
		  _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT64 x_sign;
  UINT64 x_exp;
  int exp; 	// unbiased exponent
  // Note: C1.w[1], C1.w[0] represent x_signif_hi, x_signif_lo (all are UINT64)
  int ind;
  UINT128 C1;
  unsigned int k = 0; // pointer in the string
  unsigned int d0, d123;
  UINT64 HI_18Dig, LO_18Dig, Tmp;
  UINT32 MiDi[12], *ptr;
  char *c_ptr_start, *c_ptr;
  int midi_ind, k_lcv, len;

#if DECIMAL_CALL_BY_REFERENCE
#if !DECIMAL_GLOBAL_ROUNDING
  _IDEC_round rnd_mode = *prnd_mode;
#endif
  x = *px;
#endif

  // check for NaN or Infinity
  if ((x.w[1] & MASK_SPECIAL) == MASK_SPECIAL) {
    // x is special
    if ((x.w[1] & MASK_NAN) == MASK_NAN) { // x is NAN
      if ((x.w[1] & MASK_SNAN) == MASK_SNAN) { // x is SNAN
	// set invalid flag
	*pfpsf |= INVALID_EXCEPTION;
	str[0] = 'S';
	str[1] = 'N';
	str[2] = 'a';
	str[3] = 'N';
	str[4] = '\0';
      } else { // x is QNaN
	str[0] = 'Q';
	str[1] = 'N';
	str[2] = 'a';
	str[3] = 'N';
	str[4] = '\0';
      }
    } else { // x is not a NaN, so it must be infinity
      if ((x.w[1] & MASK_SIGN) == 0x0ull) { // x is +inf
	str[0] = '+';
	str[1] = 'I';
	str[2] = 'n';
	str[3] = 'f';
	str[4] = '\0';
      } else { // x is -inf 
	str[0] = '-';
	str[1] = 'I';
	str[2] = 'n';
	str[3] = 'f';
	str[4] = '\0';
      }
    }
    return;
  } else if (((x.w[1] & MASK_COEFF) == 0x0ull) && (x.w[0] == 0x0ull)) {
    // x is 0
    len = 0;

    //determine if +/-
    if (x.w[1] & MASK_SIGN)
      str[len++] = '-';
    else
      str[len++] = '+';
    str[len++] = '0';
    str[len++] = 'E';

    // extract the exponent and print
    exp = (int) (((x.w[1] & MASK_EXP) >> 49) - 6176);
    if (exp >= 0) {
      str[len++] = '+';
      len += sprintf (str + len, "%u", exp);// should not use sprintf (should 
      // use sophisticated algorithm, since we know range of exp is limited)
      str[len++] = '\0';
    } else {
      len += sprintf (str + len, "%d", exp);// should not use sprintf (should 
      // use sophisticated algorithm, since we know range of exp is limited)
      str[len++] = '\0';
    }
    return;
  } else { // x is not special and is not zero
    // unpack x
    x_sign = x.w[1] & MASK_SIGN;// 0 for positive, MASK_SIGN for negative
    x_exp = x.w[1] & MASK_EXP;// biased and shifted left 49 bit positions
    C1.w[1] = x.w[1] & MASK_COEFF;
    C1.w[0] = x.w[0];
    exp = (x_exp >> 49) - 6176;

    // determine sign's representation as a char
    if (x_sign)
      str[k++] = '-';// negative number
    else
      str[k++] = '+';// positive number

    // determine coefficient's representation as a decimal string

    // if zero or non-canonical, set coefficient to '0'
    if ((C1.w[1] > 0x0001ed09bead87c0ull) || 
        (C1.w[1] == 0x0001ed09bead87c0ull && 
        (C1.w[0] > 0x378d8e63ffffffffull)) || 
        ((x.w[1] & 0x6000000000000000ull) == 0x6000000000000000ull) || 
        ((C1.w[1] == 0) && (C1.w[0] == 0))) {
      str[k++] = '0';
    } else {
      /* ****************************************************
         This takes a bid coefficient in C1.w[1],C1.w[0] 
         and put the converted character sequence at location 
         starting at &(str[k]). The function returns the number
         of MiDi returned. Note that the character sequence 
         does not have leading zeros EXCEPT when the input is of
         zero value. It will then output 1 character '0'
         The algorithm essentailly tries first to get a sequence of
         Millenial Digits "MiDi" and then uses table lookup to get the
         character strings of these MiDis.
         **************************************************** */
      /* Algorithm first decompose possibly 34 digits in hi and lo
         18 digits. (The high can have at most 16 digits). It then
         uses macro that handle 18 digit portions.
         The first step is to get hi and lo such that
         2^(64) C1.w[1] + C1.w[0] = hi * 10^18  + lo,   0 <= lo < 10^18.
         We use a table lookup method to obtain the hi and lo 18 digits.
         [C1.w[1],C1.w[0]] = c_8 2^(107) + c_7 2^(101) + ... + c_0 2^(59) + d
         where 0 <= d < 2^59 and each c_j has 6 bits. Because d fits in
         18 digits,  we set hi = 0, and lo = d to begin with.
         We then retrieve from a table, for j = 0, 1, ..., 8
         that gives us A and B where c_j 2^(59+6j) = A * 10^18 + B.
         hi += A ; lo += B; After each accumulation into lo, we normalize 
         immediately. So at the end, we have the decomposition as we need. */

      Tmp = C1.w[0] >> 59;
      LO_18Dig = (C1.w[0] << 5) >> 5;
      Tmp += (C1.w[1] << 5);
      HI_18Dig = 0;
      k_lcv = 0;
      // Tmp = {C1.w[1]{49:0}, C1.w[0]{63:59}}
      // Lo_18Dig = {C1.w[0]{58:0}}

      while (Tmp) {
	midi_ind = (int) (Tmp & 0x000000000000003FLL);
	midi_ind <<= 1;
	Tmp >>= 6;
	HI_18Dig += __bid_mod10_18_tbl[k_lcv][midi_ind++];
	LO_18Dig += __bid_mod10_18_tbl[k_lcv++][midi_ind];
	__L0_Normalize_10to18 (HI_18Dig, LO_18Dig);
      }
      ptr = MiDi;
      if (HI_18Dig == 0LL) {
	__L1_Split_MiDi_6_Lead (LO_18Dig, ptr);
      } else {
	__L1_Split_MiDi_6_Lead (HI_18Dig, ptr);
	__L1_Split_MiDi_6 (LO_18Dig, ptr);
      }
      len = ptr - MiDi;
      c_ptr_start = &(str[k]);
      c_ptr = c_ptr_start;

      /* now convert the MiDi into character strings */
      __L0_MiDi2Str_Lead (MiDi[0], c_ptr);
      for (k_lcv = 1; k_lcv < len; k_lcv++) {
	__L0_MiDi2Str (MiDi[k_lcv], c_ptr);
      }
      k = k + (c_ptr - c_ptr_start);
    }

    // print E and sign of exponent
    str[k++] = 'E';
    if (exp < 0) {
      exp = -exp;
      str[k++] = '-';
    } else {
      str[k++] = '+';
    }

    // determine exponent's representation as a decimal string
    // d0 = exp / 1000;
    // Use Property 1
    d0 = (exp * 0x418a) >> 24;// 0x418a * 2^-24 = (10^(-3))RP,15
    d123 = exp - 1000 * d0;

    if (d0) { // 1000 <= exp <= 6144 => 4 digits to return
      str[k++] = d0 + 0x30;// ASCII for decimal digit d0
      ind = 3 * d123;
      str[k++] = __bid_char_table3[ind];
      str[k++] = __bid_char_table3[ind + 1];
      str[k++] = __bid_char_table3[ind + 2];
    } else { // 0 <= exp <= 999 => d0 = 0
      if (d123 < 10) { // 0 <= exp <= 9 => 1 digit to return
	str[k++] = d123 + 0x30;// ASCII
      } else if (d123 < 100) { // 10 <= exp <= 99 => 2 digits to return
	ind = 2 * (d123 - 10);
	str[k++] = __bid_char_table2[ind];
	str[k++] = __bid_char_table2[ind + 1];
      } else { // 100 <= exp <= 999 => 3 digits to return
	ind = 3 * d123;
	str[k++] = __bid_char_table3[ind];
	str[k++] = __bid_char_table3[ind + 1];
	str[k++] = __bid_char_table3[ind + 2];
      }
    }
    str[k] = '\0';

  }
  return;

}
