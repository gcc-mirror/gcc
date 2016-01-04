/* Copyright (C) 2009-2016 Free Software Foundation, Inc.
 
   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.
 
   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.
 
   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include <spu_intrinsics.h>

vector double __divv2df3 (vector double a_in, vector double b_in);

/* __divv2df3 divides the vector dividend a by the vector divisor b and 
   returns the resulting vector quotient.  Maximum error about 0.5 ulp 
   over entire double range including denorms, compared to true result
   in round-to-nearest rounding mode.  Handles Inf or NaN operands and 
   results correctly.  */

vector double
__divv2df3 (vector double a_in, vector double b_in)
{
  /* Variables */
  vec_int4    exp, exp_bias;
  vec_uint4   no_underflow, overflow;
  vec_float4  mant_bf, inv_bf;
  vec_ullong2 exp_a, exp_b;
  vec_ullong2 a_nan, a_zero, a_inf, a_denorm, a_denorm0;
  vec_ullong2 b_nan, b_zero, b_inf, b_denorm, b_denorm0;
  vec_ullong2 nan;
  vec_uint4   a_exp, b_exp;
  vec_ullong2 a_mant_0, b_mant_0;
  vec_ullong2 a_exp_1s, b_exp_1s;
  vec_ullong2 sign_exp_mask;

  vec_double2 a, b;
  vec_double2 mant_a, mant_b, inv_b, q0, q1, q2, mult;

  /* Constants */
  vec_uint4   exp_mask_u32 = spu_splats((unsigned int)0x7FF00000);
  vec_uchar16 splat_hi = (vec_uchar16){0,1,2,3, 0,1,2,3,  8, 9,10,11, 8,9,10,11};
  vec_uchar16 swap_32 = (vec_uchar16){4,5,6,7, 0,1,2,3, 12,13,14,15, 8,9,10,11};
  vec_ullong2 exp_mask = spu_splats(0x7FF0000000000000ULL);
  vec_ullong2 sign_mask = spu_splats(0x8000000000000000ULL);
  vec_float4  onef = spu_splats(1.0f);
  vec_double2 one = spu_splats(1.0);
  vec_double2 exp_53 = (vec_double2)spu_splats(0x0350000000000000ULL);

  sign_exp_mask = spu_or(sign_mask, exp_mask);

  /* Extract the floating point components from each of the operands including
   * exponent and mantissa.
   */
  a_exp = (vec_uint4)spu_and((vec_uint4)a_in, exp_mask_u32);
  a_exp = spu_shuffle(a_exp, a_exp, splat_hi);
  b_exp = (vec_uint4)spu_and((vec_uint4)b_in, exp_mask_u32);
  b_exp = spu_shuffle(b_exp, b_exp, splat_hi);

  a_mant_0 = (vec_ullong2)spu_cmpeq((vec_uint4)spu_andc((vec_ullong2)a_in, sign_exp_mask), 0);
  a_mant_0 = spu_and(a_mant_0, spu_shuffle(a_mant_0, a_mant_0, swap_32));

  b_mant_0 = (vec_ullong2)spu_cmpeq((vec_uint4)spu_andc((vec_ullong2)b_in, sign_exp_mask), 0);
  b_mant_0 = spu_and(b_mant_0, spu_shuffle(b_mant_0, b_mant_0, swap_32));

  a_exp_1s = (vec_ullong2)spu_cmpeq(a_exp, exp_mask_u32);
  b_exp_1s = (vec_ullong2)spu_cmpeq(b_exp, exp_mask_u32);

  /* Identify all possible special values that must be accommodated including:
   * +-denorm, +-0, +-infinity, and NaNs.
   */
  a_denorm0= (vec_ullong2)spu_cmpeq(a_exp, 0);
  a_nan    = spu_andc(a_exp_1s, a_mant_0);
  a_zero   = spu_and (a_denorm0, a_mant_0);
  a_inf    = spu_and (a_exp_1s, a_mant_0);
  a_denorm = spu_andc(a_denorm0, a_zero);

  b_denorm0= (vec_ullong2)spu_cmpeq(b_exp, 0);
  b_nan    = spu_andc(b_exp_1s, b_mant_0);
  b_zero   = spu_and (b_denorm0, b_mant_0);
  b_inf    = spu_and (b_exp_1s, b_mant_0);
  b_denorm = spu_andc(b_denorm0, b_zero);

  /* Scale denorm inputs to into normalized numbers by conditionally scaling the 
   * input parameters.
   */
  a = spu_sub(spu_or(a_in, exp_53), spu_sel(exp_53, a_in, sign_mask));
  a = spu_sel(a_in, a, a_denorm);

  b = spu_sub(spu_or(b_in, exp_53), spu_sel(exp_53, b_in, sign_mask));
  b = spu_sel(b_in, b, b_denorm);

  /* Extract the divisor and dividend exponent and force parameters into the signed 
   * range [1.0,2.0) or [-1.0,2.0).
   */
  exp_a = spu_and((vec_ullong2)a, exp_mask);
  exp_b = spu_and((vec_ullong2)b, exp_mask);

  mant_a = spu_sel(a, one, (vec_ullong2)exp_mask);
  mant_b = spu_sel(b, one, (vec_ullong2)exp_mask);
  
  /* Approximate the single reciprocal of b by using
   * the single precision reciprocal estimate followed by one 
   * single precision iteration of Newton-Raphson.
   */
  mant_bf = spu_roundtf(mant_b);
  inv_bf = spu_re(mant_bf);
  inv_bf = spu_madd(spu_nmsub(mant_bf, inv_bf, onef), inv_bf, inv_bf);

  /* Perform 2 more Newton-Raphson iterations in double precision. The
   * result (q1) is in the range (0.5, 2.0).
   */
  inv_b = spu_extend(inv_bf);
  inv_b = spu_madd(spu_nmsub(mant_b, inv_b, one), inv_b, inv_b);
  q0 = spu_mul(mant_a, inv_b);
  q1 = spu_madd(spu_nmsub(mant_b, q0, mant_a), inv_b, q0);

  /* Determine the exponent correction factor that must be applied 
   * to q1 by taking into account the exponent of the normalized inputs
   * and the scale factors that were applied to normalize them.
   */
  exp = spu_rlmaska(spu_sub((vec_int4)exp_a, (vec_int4)exp_b), -20);
  exp = spu_add(exp, (vec_int4)spu_add(spu_and((vec_int4)a_denorm, -0x34), spu_and((vec_int4)b_denorm, 0x34)));
  
  /* Bias the quotient exponent depending on the sign of the exponent correction
   * factor so that a single multiplier will ensure the entire double precision
   * domain (including denorms) can be achieved.
   *
   *    exp 	       bias q1     adjust exp
   *   =====	       ========    ==========
   *   positive         2^+65         -65
   *   negative         2^-64         +64
   */
  exp_bias = spu_xor(spu_rlmaska(exp, -31), 64);
  exp = spu_sub(exp, exp_bias);

  q1 = spu_sel(q1, (vec_double2)spu_add((vec_int4)q1, spu_sl(exp_bias, 20)), exp_mask);

  /* Compute a multiplier (mult) to applied to the quotient (q1) to produce the 
   * expected result. On overflow, clamp the multiplier to the maximum non-infinite
   * number in case the rounding mode is not round-to-nearest.
   */
  exp = spu_add(exp, 0x3FF);
  no_underflow = spu_cmpgt(exp, 0);
  overflow = spu_cmpgt(exp, 0x7FE);
  exp = spu_and(spu_sl(exp, 20), (vec_int4)no_underflow);
  exp = spu_and(exp, (vec_int4)exp_mask);

  mult = spu_sel((vec_double2)exp, (vec_double2)(spu_add((vec_uint4)exp_mask, -1)), (vec_ullong2)overflow);

  /* Handle special value conditions. These include:
   *
   * 1) IF either operand is a NaN OR both operands are 0 or INFINITY THEN a NaN 
   *    results.
   * 2) ELSE IF the dividend is an INFINITY OR the divisor is 0 THEN a INFINITY results.
   * 3) ELSE IF the dividend is 0 OR the divisor is INFINITY THEN a 0 results.
   */
  mult = spu_andc(mult, (vec_double2)spu_or(a_zero, b_inf));
  mult = spu_sel(mult, (vec_double2)exp_mask, spu_or(a_inf, b_zero));

  nan = spu_or(a_nan, b_nan);
  nan = spu_or(nan, spu_and(a_zero, b_zero));
  nan = spu_or(nan, spu_and(a_inf, b_inf));

  mult = spu_or(mult, (vec_double2)nan);

  /* Scale the final quotient */

  q2 = spu_mul(q1, mult);

  return (q2);
}


/* We use the same function for vector and scalar division.  Provide the
   scalar entry point as an alias.  */
double __divdf3 (double a, double b)
  __attribute__ ((__alias__ ("__divv2df3")));

/* Some toolchain builds used the __fast_divdf3 name for this helper function.
   Provide this as another alternate entry point for compatibility.  */
double __fast_divdf3 (double a, double b)
  __attribute__ ((__alias__ ("__divv2df3")));

