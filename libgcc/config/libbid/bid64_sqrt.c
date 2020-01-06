/* Copyright (C) 2007-2020 Free Software Foundation, Inc.

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

/*****************************************************************************
 *    BID64 square root
 *****************************************************************************
 *
 *  Algorithm description:
 *
 *  if(exponent_x is odd)
 *     scale coefficient_x by 10, adjust exponent
 *  - get lower estimate for number of digits in coefficient_x
 *  - scale coefficient x to between 31 and 33 decimal digits
 *  - in parallel, check for exact case and return if true
 *  - get high part of result coefficient using double precision sqrt
 *  - compute remainder and refine coefficient in one iteration (which 
 *                                 modifies it by at most 1)
 *  - result exponent is easy to compute from the adjusted arg. exponent 
 *
 ****************************************************************************/

#include "bid_internal.h"
#include "bid_sqrt_macros.h"
#ifdef UNCHANGED_BINARY_STATUS_FLAGS
#include <fenv.h>

#define FE_ALL_FLAGS FE_INVALID|FE_DIVBYZERO|FE_OVERFLOW|FE_UNDERFLOW|FE_INEXACT
#endif

extern double sqrt (double);

#if DECIMAL_CALL_BY_REFERENCE

void
bid64_sqrt (UINT64 * pres,
	    UINT64 *
	    px _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM
	    _EXC_INFO_PARAM) {
  UINT64 x;
#else

UINT64
bid64_sqrt (UINT64 x _RND_MODE_PARAM _EXC_FLAGS_PARAM
	    _EXC_MASKS_PARAM _EXC_INFO_PARAM) {
#endif
  UINT128 CA, CT;
  UINT64 sign_x, coefficient_x;
  UINT64 Q, Q2, A10, C4, R, R2, QE, res;
  SINT64 D;
  int_double t_scale;
  int_float tempx;
  double da, dq, da_h, da_l, dqe;
  int exponent_x, exponent_q, bin_expon_cx;
  int digits_x;
  int scale;
#ifdef UNCHANGED_BINARY_STATUS_FLAGS
  fexcept_t binaryflags = 0;
#endif

#if DECIMAL_CALL_BY_REFERENCE
#if !DECIMAL_GLOBAL_ROUNDING
  _IDEC_round rnd_mode = *prnd_mode;
#endif
  x = *px;
#endif

  // unpack arguments, check for NaN or Infinity
  if (!unpack_BID64 (&sign_x, &exponent_x, &coefficient_x, x)) {
    // x is Inf. or NaN or 0
    if ((x & INFINITY_MASK64) == INFINITY_MASK64) {
      res = coefficient_x;
      if ((coefficient_x & SSNAN_MASK64) == SINFINITY_MASK64)	// -Infinity
      {
	res = NAN_MASK64;
#ifdef SET_STATUS_FLAGS
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      }
#ifdef SET_STATUS_FLAGS
      if ((x & SNAN_MASK64) == SNAN_MASK64)	// sNaN
	__set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
      BID_RETURN (res & QUIET_MASK64);
    }
    // x is 0
    exponent_x = (exponent_x + DECIMAL_EXPONENT_BIAS) >> 1;
    res = sign_x | (((UINT64) exponent_x) << 53);
    BID_RETURN (res);
  }
  // x<0?
  if (sign_x && coefficient_x) {
    res = NAN_MASK64;
#ifdef SET_STATUS_FLAGS
    __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
    BID_RETURN (res);
  }
#ifdef UNCHANGED_BINARY_STATUS_FLAGS
  (void) fegetexceptflag (&binaryflags, FE_ALL_FLAGS);
#endif
  //--- get number of bits in the coefficient of x ---
  tempx.d = (float) coefficient_x;
  bin_expon_cx = ((tempx.i >> 23) & 0xff) - 0x7f;
  digits_x = estimate_decimal_digits[bin_expon_cx];
  // add test for range
  if (coefficient_x >= power10_index_binexp[bin_expon_cx])
    digits_x++;

  A10 = coefficient_x;
  if (exponent_x & 1) {
    A10 = (A10 << 2) + A10;
    A10 += A10;
  }

  dqe = sqrt ((double) A10);
  QE = (UINT32) dqe;
  if (QE * QE == A10) {
    res =
      very_fast_get_BID64 (0, (exponent_x + DECIMAL_EXPONENT_BIAS) >> 1,
			   QE);
#ifdef UNCHANGED_BINARY_STATUS_FLAGS
    (void) fesetexceptflag (&binaryflags, FE_ALL_FLAGS);
#endif
    BID_RETURN (res);
  }
  // if exponent is odd, scale coefficient by 10
  scale = 31 - digits_x;
  exponent_q = exponent_x - scale;
  scale += (exponent_q & 1);	// exp. bias is even

  CT = power10_table_128[scale];
  __mul_64x128_short (CA, coefficient_x, CT);

  // 2^64
  t_scale.i = 0x43f0000000000000ull;
  // convert CA to DP
  da_h = CA.w[1];
  da_l = CA.w[0];
  da = da_h * t_scale.d + da_l;

  dq = sqrt (da);

  Q = (UINT64) dq;

  // get sign(sqrt(CA)-Q)
  R = CA.w[0] - Q * Q;
  R = ((SINT64) R) >> 63;
  D = R + R + 1;

  exponent_q = (exponent_q + DECIMAL_EXPONENT_BIAS) >> 1;

#ifdef SET_STATUS_FLAGS
  __set_status_flags (pfpsf, INEXACT_EXCEPTION);
#endif

#ifndef IEEE_ROUND_NEAREST
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
  if (!((rnd_mode) & 3)) {
#endif
#endif

    // midpoint to check
    Q2 = Q + Q + D;
    C4 = CA.w[0] << 2;

    // get sign(-sqrt(CA)+Midpoint)
    R2 = Q2 * Q2 - C4;
    R2 = ((SINT64) R2) >> 63;

    // adjust Q if R!=R2
    Q += (D & (R ^ R2));
#ifndef IEEE_ROUND_NEAREST
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
  } else {
    C4 = CA.w[0];
    Q += D;
    if ((SINT64) (Q * Q - C4) > 0)
      Q--;
    if (rnd_mode == ROUNDING_UP)
      Q++;
  }
#endif
#endif

  res = fast_get_BID64 (0, exponent_q, Q);
#ifdef UNCHANGED_BINARY_STATUS_FLAGS
  (void) fesetexceptflag (&binaryflags, FE_ALL_FLAGS);
#endif
  BID_RETURN (res);
}


TYPE0_FUNCTION_ARG1 (UINT64, bid64q_sqrt, x)

     UINT256 M256, C4, C8;
     UINT128 CX, CX2, A10, S2, T128, CS, CSM, CS2, C256, CS1,
       mul_factor2_long = { {0x0ull, 0x0ull} }, QH, Tmp, TP128, Qh, Ql;
UINT64 sign_x, Carry, B10, res, mul_factor, mul_factor2 = 0x0ull, CS0;
SINT64 D;
int_float fx, f64;
int exponent_x, bin_expon_cx, done = 0;
int digits, scale, exponent_q = 0, exact = 1, amount, extra_digits;
#ifdef UNCHANGED_BINARY_STATUS_FLAGS
fexcept_t binaryflags = 0;
#endif

	// unpack arguments, check for NaN or Infinity
if (!unpack_BID128_value (&sign_x, &exponent_x, &CX, x)) {
  res = CX.w[1];
  // NaN ?
  if ((x.w[1] & 0x7c00000000000000ull) == 0x7c00000000000000ull) {
#ifdef SET_STATUS_FLAGS
    if ((x.w[1] & 0x7e00000000000000ull) == 0x7e00000000000000ull)	// sNaN
      __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
    Tmp.w[1] = (CX.w[1] & 0x00003fffffffffffull);
    Tmp.w[0] = CX.w[0];
    TP128 = reciprocals10_128[18];
    __mul_128x128_full (Qh, Ql, Tmp, TP128);
    amount = recip_scale[18];
    __shr_128 (Tmp, Qh, amount);
    res = (CX.w[1] & 0xfc00000000000000ull) | Tmp.w[0];
    BID_RETURN (res);
  }
  // x is Infinity?
  if ((x.w[1] & 0x7800000000000000ull) == 0x7800000000000000ull) {
    if (sign_x) {
      // -Inf, return NaN
      res = 0x7c00000000000000ull;
#ifdef SET_STATUS_FLAGS
      __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
    }
    BID_RETURN (res);
  }
  // x is 0 otherwise

  exponent_x =
    ((exponent_x - DECIMAL_EXPONENT_BIAS_128) >> 1) +
    DECIMAL_EXPONENT_BIAS;
  if (exponent_x < 0)
    exponent_x = 0;
  if (exponent_x > DECIMAL_MAX_EXPON_64)
    exponent_x = DECIMAL_MAX_EXPON_64;
  //res= sign_x | (((UINT64)exponent_x)<<53);
  res = get_BID64 (sign_x, exponent_x, 0, rnd_mode, pfpsf);
  BID_RETURN (res);
}
if (sign_x) {
  res = 0x7c00000000000000ull;
#ifdef SET_STATUS_FLAGS
  __set_status_flags (pfpsf, INVALID_EXCEPTION);
#endif
  BID_RETURN (res);
}
#ifdef UNCHANGED_BINARY_STATUS_FLAGS
(void) fegetexceptflag (&binaryflags, FE_ALL_FLAGS);
#endif

	   // 2^64
f64.i = 0x5f800000;

	   // fx ~ CX
fx.d = (float) CX.w[1] * f64.d + (float) CX.w[0];
bin_expon_cx = ((fx.i >> 23) & 0xff) - 0x7f;
digits = estimate_decimal_digits[bin_expon_cx];

A10 = CX;
if (exponent_x & 1) {
  A10.w[1] = (CX.w[1] << 3) | (CX.w[0] >> 61);
  A10.w[0] = CX.w[0] << 3;
  CX2.w[1] = (CX.w[1] << 1) | (CX.w[0] >> 63);
  CX2.w[0] = CX.w[0] << 1;
  __add_128_128 (A10, A10, CX2);
}

C256.w[1] = A10.w[1];
C256.w[0] = A10.w[0];
CS.w[0] = short_sqrt128 (A10);
CS.w[1] = 0;
mul_factor = 0;
	   // check for exact result  
if (CS.w[0] < 10000000000000000ull) {
  if (CS.w[0] * CS.w[0] == A10.w[0]) {
    __sqr64_fast (S2, CS.w[0]);
    if (S2.w[1] == A10.w[1])	// && S2.w[0]==A10.w[0])
    {
      res =
	get_BID64 (0,
		   ((exponent_x - DECIMAL_EXPONENT_BIAS_128) >> 1) +
		   DECIMAL_EXPONENT_BIAS, CS.w[0], rnd_mode, pfpsf);
#ifdef UNCHANGED_BINARY_STATUS_FLAGS
      (void) fesetexceptflag (&binaryflags, FE_ALL_FLAGS);
#endif
      BID_RETURN (res);
    }
  }
  if (CS.w[0] >= 1000000000000000ull) {
    done = 1;
    exponent_q = exponent_x;
    C256.w[1] = A10.w[1];
    C256.w[0] = A10.w[0];
  }
#ifdef SET_STATUS_FLAGS
  __set_status_flags (pfpsf, INEXACT_EXCEPTION);
#endif
  exact = 0;
} else {
  B10 = 0x3333333333333334ull;
  __mul_64x64_to_128_full (CS2, CS.w[0], B10);
  CS0 = CS2.w[1] >> 1;
  if (CS.w[0] != ((CS0 << 3) + (CS0 << 1))) {
#ifdef SET_STATUS_FLAGS
    __set_status_flags (pfpsf, INEXACT_EXCEPTION);
#endif
    exact = 0;
  }
  done = 1;
  CS.w[0] = CS0;
  exponent_q = exponent_x + 2;
  mul_factor = 10;
  mul_factor2 = 100;
  if (CS.w[0] >= 10000000000000000ull) {
    __mul_64x64_to_128_full (CS2, CS.w[0], B10);
    CS0 = CS2.w[1] >> 1;
    if (CS.w[0] != ((CS0 << 3) + (CS0 << 1))) {
#ifdef SET_STATUS_FLAGS
      __set_status_flags (pfpsf, INEXACT_EXCEPTION);
#endif
      exact = 0;
    }
    exponent_q += 2;
    CS.w[0] = CS0;
    mul_factor = 100;
    mul_factor2 = 10000;
  }
  if (exact) {
    CS0 = CS.w[0] * mul_factor;
    __sqr64_fast (CS1, CS0)
      if ((CS1.w[0] != A10.w[0]) || (CS1.w[1] != A10.w[1])) {
#ifdef SET_STATUS_FLAGS
      __set_status_flags (pfpsf, INEXACT_EXCEPTION);
#endif
      exact = 0;
    }
  }
}

if (!done) {
  // get number of digits in CX
  D = CX.w[1] - power10_index_binexp_128[bin_expon_cx].w[1];
  if (D > 0
      || (!D && CX.w[0] >= power10_index_binexp_128[bin_expon_cx].w[0]))
    digits++;

  // if exponent is odd, scale coefficient by 10
  scale = 31 - digits;
  exponent_q = exponent_x - scale;
  scale += (exponent_q & 1);	// exp. bias is even

  T128 = power10_table_128[scale];
  __mul_128x128_low (C256, CX, T128);


  CS.w[0] = short_sqrt128 (C256);
}
   //printf("CS=%016I64x\n",CS.w[0]);

exponent_q =
  ((exponent_q - DECIMAL_EXPONENT_BIAS_128) >> 1) +
  DECIMAL_EXPONENT_BIAS;
if ((exponent_q < 0) && (exponent_q + MAX_FORMAT_DIGITS >= 0)) {
  extra_digits = -exponent_q;
  exponent_q = 0;

  // get coeff*(2^M[extra_digits])/10^extra_digits
  __mul_64x64_to_128 (QH, CS.w[0], reciprocals10_64[extra_digits]);

  // now get P/10^extra_digits: shift Q_high right by M[extra_digits]-128
  amount = short_recip_scale[extra_digits];

  CS0 = QH.w[1] >> amount;

#ifdef SET_STATUS_FLAGS
  if (exact) {
    if (CS.w[0] != CS0 * power10_table_128[extra_digits].w[0])
      exact = 0;
  }
  if (!exact)
    __set_status_flags (pfpsf, UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION);
#endif

  CS.w[0] = CS0;
  if (!mul_factor)
    mul_factor = 1;
  mul_factor *= power10_table_128[extra_digits].w[0];
  __mul_64x64_to_128 (mul_factor2_long, mul_factor, mul_factor);
  if (mul_factor2_long.w[1])
    mul_factor2 = 0;
  else
    mul_factor2 = mul_factor2_long.w[1];
}
	   // 4*C256
C4.w[1] = (C256.w[1] << 2) | (C256.w[0] >> 62);
C4.w[0] = C256.w[0] << 2;

#ifndef IEEE_ROUND_NEAREST
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
if (!((rnd_mode) & 3)) {
#endif
#endif
  // compare to midpoints
  CSM.w[0] = (CS.w[0] + CS.w[0]) | 1;
  //printf("C256=%016I64x %016I64x, CSM=%016I64x %016I64x %016I64x\n",C4.w[1],C4.w[0],CSM.w[1],CSM.w[0], CS.w[0]);
  if (mul_factor)
    CSM.w[0] *= mul_factor;
  // CSM^2
  __mul_64x64_to_128 (M256, CSM.w[0], CSM.w[0]);
  //__mul_128x128_to_256(M256, CSM, CSM);

  if (C4.w[1] > M256.w[1] ||
      (C4.w[1] == M256.w[1] && C4.w[0] > M256.w[0])) {
    // round up
    CS.w[0]++;
  } else {
    C8.w[0] = CS.w[0] << 3;
    C8.w[1] = 0;
    if (mul_factor) {
      if (mul_factor2) {
	__mul_64x64_to_128 (C8, C8.w[0], mul_factor2);
      } else {
	__mul_64x128_low (C8, C8.w[0], mul_factor2_long);
      }
    }
    // M256 - 8*CSM
    __sub_borrow_out (M256.w[0], Carry, M256.w[0], C8.w[0]);
    M256.w[1] = M256.w[1] - C8.w[1] - Carry;

    // if CSM' > C256, round up
    if (M256.w[1] > C4.w[1] ||
	(M256.w[1] == C4.w[1] && M256.w[0] > C4.w[0])) {
      // round down
      if (CS.w[0])
	CS.w[0]--;
    }
  }
#ifndef IEEE_ROUND_NEAREST
#ifndef IEEE_ROUND_NEAREST_TIES_AWAY
} else {
  CS.w[0]++;
  CSM.w[0] = CS.w[0];
  C8.w[0] = CSM.w[0] << 1;
  if (mul_factor)
    CSM.w[0] *= mul_factor;
  __mul_64x64_to_128 (M256, CSM.w[0], CSM.w[0]);
  C8.w[1] = 0;
  if (mul_factor) {
    if (mul_factor2) {
      __mul_64x64_to_128 (C8, C8.w[0], mul_factor2);
    } else {
      __mul_64x128_low (C8, C8.w[0], mul_factor2_long);
    }
  }
  //printf("C256=%016I64x %016I64x, CSM=%016I64x %016I64x %016I64x\n",C256.w[1],C256.w[0],M256.w[1],M256.w[0], CS.w[0]);

  if (M256.w[1] > C256.w[1] ||
      (M256.w[1] == C256.w[1] && M256.w[0] > C256.w[0])) {
    __sub_borrow_out (M256.w[0], Carry, M256.w[0], C8.w[0]);
    M256.w[1] = M256.w[1] - Carry - C8.w[1];
    M256.w[0]++;
    if (!M256.w[0]) {
      M256.w[1]++;

    }

    if ((M256.w[1] > C256.w[1] ||
	 (M256.w[1] == C256.w[1] && M256.w[0] > C256.w[0]))
	&& (CS.w[0] > 1)) {

      CS.w[0]--;

      if (CS.w[0] > 1) {
	__sub_borrow_out (M256.w[0], Carry, M256.w[0], C8.w[0]);
	M256.w[1] = M256.w[1] - Carry - C8.w[1];
	M256.w[0]++;
	if (!M256.w[0]) {
	  M256.w[1]++;
	}

	if (M256.w[1] > C256.w[1] ||
	    (M256.w[1] == C256.w[1] && M256.w[0] > C256.w[0]))
	  CS.w[0]--;
      }
    }
  }

  else {
				/*__add_carry_out(M256.w[0], Carry, M256.w[0], C8.w[0]);
				M256.w[1] = M256.w[1] + Carry + C8.w[1];
				M256.w[0]++;
				if(!M256.w[0]) 
				{
					M256.w[1]++;
				}
				CS.w[0]++;
			if(M256.w[1]<C256.w[1] ||
				(M256.w[1]==C256.w[1] && M256.w[0]<=C256.w[0]))
			{
				CS.w[0]++;
			}*/
    CS.w[0]++;
  }
  //printf("C256=%016I64x %016I64x, CSM=%016I64x %016I64x %016I64x %d\n",C4.w[1],C4.w[0],M256.w[1],M256.w[0], CS.w[0], exact);
  // RU?
  if (((rnd_mode) != ROUNDING_UP) || exact) {
    if (CS.w[0])
      CS.w[0]--;
  }

}
#endif
#endif
 //printf("C256=%016I64x %016I64x, CSM=%016I64x %016I64x %016I64x %d\n",C4.w[1],C4.w[0],M256.w[1],M256.w[0], CS.w[0], exact);

res = get_BID64 (0, exponent_q, CS.w[0], rnd_mode, pfpsf);
#ifdef UNCHANGED_BINARY_STATUS_FLAGS
(void) fesetexceptflag (&binaryflags, FE_ALL_FLAGS);
#endif
BID_RETURN (res);


}
