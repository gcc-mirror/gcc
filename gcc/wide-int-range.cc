/* Support routines for range operations on wide ints.
   Copyright (C) 2018 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "fold-const.h"
#include "wide-int-range.h"

/* Wrapper around wide_int_binop that adjusts for overflow.

   Return true if we can compute the result; i.e. if the operation
   doesn't overflow or if the overflow is undefined.  In the latter
   case (if the operation overflows and overflow is undefined), then
   adjust the result to be -INF or +INF depending on CODE, VAL1 and
   VAL2.  Return the value in *RES.

   Return false for division by zero, for which the result is
   indeterminate.  */

static bool
wide_int_binop_overflow (wide_int &res,
			 enum tree_code code,
			 const wide_int &w0, const wide_int &w1,
			 signop sign, bool overflow_undefined)
{
  wi::overflow_type overflow;
  if (!wide_int_binop (res, code, w0, w1, sign, &overflow))
    return false;

  /* If the operation overflowed return -INF or +INF depending on the
     operation and the combination of signs of the operands.  */
  if (overflow && overflow_undefined)
    {
      switch (code)
	{
	case MULT_EXPR:
	  /* For multiplication, the sign of the overflow is given
	     by the comparison of the signs of the operands.  */
	  if (sign == UNSIGNED || w0.sign_mask () == w1.sign_mask ())
	    res = wi::max_value (w0.get_precision (), sign);
	  else
	    res = wi::min_value (w0.get_precision (), sign);
	  return true;

	case TRUNC_DIV_EXPR:
	case FLOOR_DIV_EXPR:
	case CEIL_DIV_EXPR:
	case EXACT_DIV_EXPR:
	case ROUND_DIV_EXPR:
	  /* For division, the only case is -INF / -1 = +INF.  */
	  res = wi::max_value (w0.get_precision (), sign);
	  return true;

	default:
	  gcc_unreachable ();
	}
    }
  return !overflow;
}

/* For range [LB, UB] compute two wide_int bit masks.

   In the MAY_BE_NONZERO bit mask, if some bit is unset, it means that
   for all numbers in the range the bit is 0, otherwise it might be 0
   or 1.

   In the MUST_BE_NONZERO bit mask, if some bit is set, it means that
   for all numbers in the range the bit is 1, otherwise it might be 0
   or 1.  */

void
wide_int_range_set_zero_nonzero_bits (signop sign,
				      const wide_int &lb, const wide_int &ub,
				      wide_int &may_be_nonzero,
				      wide_int &must_be_nonzero)
{
  may_be_nonzero = wi::minus_one (lb.get_precision ());
  must_be_nonzero = wi::zero (lb.get_precision ());

  if (wi::eq_p (lb, ub))
    {
      may_be_nonzero = lb;
      must_be_nonzero = may_be_nonzero;
    }
  else if (wi::ge_p (lb, 0, sign) || wi::lt_p (ub, 0, sign))
    {
      wide_int xor_mask = lb ^ ub;
      may_be_nonzero = lb | ub;
      must_be_nonzero = lb & ub;
      if (xor_mask != 0)
	{
	  wide_int mask = wi::mask (wi::floor_log2 (xor_mask), false,
				    may_be_nonzero.get_precision ());
	  may_be_nonzero = may_be_nonzero | mask;
	  must_be_nonzero = wi::bit_and_not (must_be_nonzero, mask);
	}
    }
}

/* Order 2 sets of wide int ranges (w0/w1, w2/w3) and set MIN/MAX
   accordingly.  */

static void
wide_int_range_order_set (wide_int &min, wide_int &max,
			  wide_int &w0, wide_int &w1,
			  wide_int &w2, wide_int &w3,
			  signop sign)
{
  /* Order pairs w0,w1 and w2,w3.  */
  if (wi::gt_p (w0, w1, sign))
    std::swap (w0, w1);
  if (wi::gt_p (w2, w3, sign))
    std::swap (w2, w3);

  /* Choose min and max from the ordered pairs.  */
  min = wi::min (w0, w2, sign);
  max = wi::max (w1, w3, sign);
}

/* Calculate the cross product of two sets of ranges (VR0 and VR1) and
   store the result in [RES_LB, RES_UB].

   CODE is the operation to perform with sign SIGN.

   OVERFLOW_UNDEFINED is set if overflow is undefined for the operation type.

   Return TRUE if we were able to calculate the cross product.  */

bool
wide_int_range_cross_product (wide_int &res_lb, wide_int &res_ub,
			      enum tree_code code, signop sign,
			      const wide_int &vr0_lb, const wide_int &vr0_ub,
			      const wide_int &vr1_lb, const wide_int &vr1_ub,
			      bool overflow_undefined)
{
  wide_int cp1, cp2, cp3, cp4;

  /* Compute the 4 cross operations, bailing if we get an overflow we
     can't handle.  */

  if (!wide_int_binop_overflow (cp1, code, vr0_lb, vr1_lb, sign,
				overflow_undefined))
    return false;

  if (wi::eq_p (vr0_lb, vr0_ub))
    cp3 = cp1;
  else if (!wide_int_binop_overflow (cp3, code, vr0_ub, vr1_lb, sign,
				     overflow_undefined))
    return false;

  if (wi::eq_p (vr1_lb, vr1_ub))
    cp2 = cp1;
  else if (!wide_int_binop_overflow (cp2, code, vr0_lb, vr1_ub, sign,
				     overflow_undefined))
    return false;

  if (wi::eq_p (vr0_lb, vr0_ub))
    cp4 = cp2;
  else if (!wide_int_binop_overflow (cp4, code, vr0_ub, vr1_ub, sign,
				     overflow_undefined))
    return false;

  wide_int_range_order_set (res_lb, res_ub, cp1, cp2, cp3, cp4, sign);
  return true;
}

/* Multiply two ranges when TYPE_OVERFLOW_WRAPS:

     [RES_LB, RES_UB] = [MIN0, MAX0] * [MIN1, MAX1]

   This is basically fancy code so we don't drop to varying with an
   unsigned [-3,-1]*[-3,-1].

   Return TRUE if we were able to perform the operation.  */

bool
wide_int_range_mult_wrapping (wide_int &res_lb,
			      wide_int &res_ub,
			      signop sign,
			      unsigned prec,
			      const wide_int &min0_,
			      const wide_int &max0_,
			      const wide_int &min1_,
			      const wide_int &max1_)
{
  /* This test requires 2*prec bits if both operands are signed and
     2*prec + 2 bits if either is not.  Therefore, extend the values
     using the sign of the result to PREC2.  From here on out,
     everthing is just signed math no matter what the input types
     were.  */
  widest2_int min0 = widest2_int::from (min0_, sign);
  widest2_int max0 = widest2_int::from (max0_, sign);
  widest2_int min1 = widest2_int::from (min1_, sign);
  widest2_int max1 = widest2_int::from (max1_, sign);
  widest2_int sizem1 = wi::mask <widest2_int> (prec, false);
  widest2_int size = sizem1 + 1;

  /* Canonicalize the intervals.  */
  if (sign == UNSIGNED)
    {
      if (wi::ltu_p (size, min0 + max0))
	{
	  min0 -= size;
	  max0 -= size;
	}

      if (wi::ltu_p (size, min1 + max1))
	{
	  min1 -= size;
	  max1 -= size;
	}
    }

  widest2_int prod0 = min0 * min1;
  widest2_int prod1 = min0 * max1;
  widest2_int prod2 = max0 * min1;
  widest2_int prod3 = max0 * max1;

  /* Sort the 4 products so that min is in prod0 and max is in
     prod3.  */
  /* min0min1 > max0max1 */
  if (prod0 > prod3)
    std::swap (prod0, prod3);

  /* min0max1 > max0min1 */
  if (prod1 > prod2)
    std::swap (prod1, prod2);

  if (prod0 > prod1)
    std::swap (prod0, prod1);

  if (prod2 > prod3)
    std::swap (prod2, prod3);

  /* diff = max - min.  */
  prod2 = prod3 - prod0;
  if (wi::geu_p (prod2, sizem1))
    /* The range covers all values.  */
    return false;

  res_lb = wide_int::from (prod0, prec, sign);
  res_ub = wide_int::from (prod3, prec, sign);
  return true;
}

/* Perform multiplicative operation CODE on two ranges:

     [RES_LB, RES_UB] = [VR0_LB, VR0_UB] .CODE. [VR1_LB, VR1_LB]

   Return TRUE if we were able to perform the operation.

   NOTE: If code is MULT_EXPR and TYPE_OVERFLOW_WRAPS, the resulting
   range must be canonicalized by the caller because its components
   may be swapped.  */

bool
wide_int_range_multiplicative_op (wide_int &res_lb, wide_int &res_ub,
				  enum tree_code code,
				  signop sign,
				  unsigned prec,
				  const wide_int &vr0_lb,
				  const wide_int &vr0_ub,
				  const wide_int &vr1_lb,
				  const wide_int &vr1_ub,
				  bool overflow_undefined,
				  bool overflow_wraps)
{
  /* Multiplications, divisions and shifts are a bit tricky to handle,
     depending on the mix of signs we have in the two ranges, we
     need to operate on different values to get the minimum and
     maximum values for the new range.  One approach is to figure
     out all the variations of range combinations and do the
     operations.

     However, this involves several calls to compare_values and it
     is pretty convoluted.  It's simpler to do the 4 operations
     (MIN0 OP MIN1, MIN0 OP MAX1, MAX0 OP MIN1 and MAX0 OP MAX0 OP
     MAX1) and then figure the smallest and largest values to form
     the new range.  */
  if (code == MULT_EXPR && overflow_wraps)
    return wide_int_range_mult_wrapping (res_lb, res_ub,
					 sign, prec,
					 vr0_lb, vr0_ub, vr1_lb, vr1_ub);
  return wide_int_range_cross_product (res_lb, res_ub,
				       code, sign,
				       vr0_lb, vr0_ub, vr1_lb, vr1_ub,
				       overflow_undefined);
}

/* Perform a left shift operation on two ranges:

     [RES_LB, RES_UB] = [VR0_LB, VR0_UB] << [VR1_LB, VR1_LB]

   Return TRUE if we were able to perform the operation.

   NOTE: The resulting range must be canonicalized by the caller
   because its contents components may be swapped.  */

bool
wide_int_range_lshift (wide_int &res_lb, wide_int &res_ub,
		       signop sign, unsigned prec,
		       const wide_int &vr0_lb, const wide_int &vr0_ub,
		       const wide_int &vr1_lb, const wide_int &vr1_ub,
		       bool overflow_undefined, bool overflow_wraps)
{
  /* Transform left shifts by constants into multiplies.  */
  if (wi::eq_p (vr1_lb, vr1_ub))
    {
      unsigned shift = vr1_ub.to_uhwi ();
      wide_int tmp = wi::set_bit_in_zero (shift, prec);
      return wide_int_range_multiplicative_op (res_lb, res_ub,
					       MULT_EXPR, sign, prec,
					       vr0_lb, vr0_ub, tmp, tmp,
					       overflow_undefined,
					       /*overflow_wraps=*/true);
    }

  int overflow_pos = prec;
  if (sign == SIGNED)
    overflow_pos -= 1;
  int bound_shift = overflow_pos - vr1_ub.to_shwi ();
  /* If bound_shift == HOST_BITS_PER_WIDE_INT, the llshift can
     overflow.  However, for that to happen, vr1.max needs to be
     zero, which means vr1 is a singleton range of zero, which
     means it should be handled by the previous LSHIFT_EXPR
     if-clause.  */
  wide_int bound = wi::set_bit_in_zero (bound_shift, prec);
  wide_int complement = ~(bound - 1);
  wide_int low_bound, high_bound;
  bool in_bounds = false;
  if (sign == UNSIGNED)
    {
      low_bound = bound;
      high_bound = complement;
      if (wi::ltu_p (vr0_ub, low_bound))
	{
	  /* [5, 6] << [1, 2] == [10, 24].  */
	  /* We're shifting out only zeroes, the value increases
	     monotonically.  */
	  in_bounds = true;
	}
      else if (wi::ltu_p (high_bound, vr0_lb))
	{
	  /* [0xffffff00, 0xffffffff] << [1, 2]
	     == [0xfffffc00, 0xfffffffe].  */
	  /* We're shifting out only ones, the value decreases
	     monotonically.  */
	  in_bounds = true;
	}
    }
  else
    {
      /* [-1, 1] << [1, 2] == [-4, 4].  */
      low_bound = complement;
      high_bound = bound;
      if (wi::lts_p (vr0_ub, high_bound)
	  && wi::lts_p (low_bound, vr0_lb))
	{
	  /* For non-negative numbers, we're shifting out only
	     zeroes, the value increases monotonically.
	     For negative numbers, we're shifting out only ones, the
	     value decreases monotomically.  */
	  in_bounds = true;
	}
    }
  if (in_bounds)
    return wide_int_range_multiplicative_op (res_lb, res_ub,
					     LSHIFT_EXPR, sign, prec,
					     vr0_lb, vr0_ub,
					     vr1_lb, vr1_ub,
					     overflow_undefined,
					     overflow_wraps);
  return false;
}

/* Return TRUE if a bit operation on two ranges can be easily
   optimized in terms of a mask.

   Basically, for BIT_AND_EXPR or BIT_IOR_EXPR see if we can optimize:

	[LB, UB] op Z
   into:
	[LB op Z, UB op Z]

   It is up to the caller to perform the actual folding above.  */

static bool
wide_int_range_can_optimize_bit_op (tree_code code,
				    const wide_int &lb, const wide_int &ub,
				    const wide_int &mask)

{
  if (code != BIT_AND_EXPR && code != BIT_IOR_EXPR)
    return false;
  /* If Z is a constant which (for op | its bitwise not) has n
     consecutive least significant bits cleared followed by m 1
     consecutive bits set immediately above it and either
     m + n == precision, or (x >> (m + n)) == (y >> (m + n)).

     The least significant n bits of all the values in the range are
     cleared or set, the m bits above it are preserved and any bits
     above these are required to be the same for all values in the
     range.  */

  wide_int w = mask;
  int m = 0, n = 0;
  if (code == BIT_IOR_EXPR)
    w = ~w;
  if (wi::eq_p (w, 0))
    n = w.get_precision ();
  else
    {
      n = wi::ctz (w);
      w = ~(w | wi::mask (n, false, w.get_precision ()));
      if (wi::eq_p (w, 0))
	m = w.get_precision () - n;
      else
	m = wi::ctz (w) - n;
    }
  wide_int new_mask = wi::mask (m + n, true, w.get_precision ());
  if ((new_mask & lb) == (new_mask & ub))
    return true;

  return false;
}

/* Helper function for wide_int_range_optimize_bit_op.

   Calculates bounds and mask for a pair of ranges.  The mask is the
   singleton range among the ranges, if any.  The bounds are the
   bounds for the remaining range.  */

bool
wide_int_range_get_mask_and_bounds (wide_int &mask,
				    wide_int &lower_bound,
				    wide_int &upper_bound,
				    const wide_int &vr0_min,
				    const wide_int &vr0_max,
				    const wide_int &vr1_min,
				    const wide_int &vr1_max)
{
  if (wi::eq_p (vr1_min, vr1_max))
    {
      mask = vr1_min;
      lower_bound = vr0_min;
      upper_bound = vr0_max;
      return true;
    }
  else if (wi::eq_p (vr0_min, vr0_max))
    {
      mask = vr0_min;
      lower_bound = vr1_min;
      upper_bound = vr1_max;
      return true;
    }
  return false;
}

/* Optimize a bit operation (BIT_AND_EXPR or BIT_IOR_EXPR) if
   possible.  If so, return TRUE and store the result in
   [RES_LB, RES_UB].  */

bool
wide_int_range_optimize_bit_op (wide_int &res_lb, wide_int &res_ub,
				enum tree_code code,
				signop sign,
				const wide_int &vr0_min,
				const wide_int &vr0_max,
				const wide_int &vr1_min,
				const wide_int &vr1_max)
{
  gcc_assert (code == BIT_AND_EXPR || code == BIT_IOR_EXPR);

  wide_int lower_bound, upper_bound, mask;
  if (!wide_int_range_get_mask_and_bounds (mask, lower_bound, upper_bound,
					   vr0_min, vr0_max, vr1_min, vr1_max))
    return false;
  if (wide_int_range_can_optimize_bit_op (code,
					  lower_bound, upper_bound, mask))
    {
      wi::overflow_type ovf;
      wide_int_binop (res_lb, code, lower_bound, mask, sign, &ovf);
      wide_int_binop (res_ub, code, upper_bound, mask, sign, &ovf);
      return true;
    }
  return false;
}

/* Calculate the XOR of two ranges and store the result in [WMIN,WMAX].
   The two input ranges are described by their MUST_BE_NONZERO and
   MAY_BE_NONZERO bit masks.

   Return TRUE if we were able to successfully calculate the new range.  */

bool
wide_int_range_bit_xor (wide_int &wmin, wide_int &wmax,
			signop sign,
			unsigned prec,
			const wide_int &must_be_nonzero0,
			const wide_int &may_be_nonzero0,
			const wide_int &must_be_nonzero1,
			const wide_int &may_be_nonzero1)
{
  wide_int result_zero_bits = ((must_be_nonzero0 & must_be_nonzero1)
			       | ~(may_be_nonzero0 | may_be_nonzero1));
  wide_int result_one_bits
    = (wi::bit_and_not (must_be_nonzero0, may_be_nonzero1)
       | wi::bit_and_not (must_be_nonzero1, may_be_nonzero0));
  wmax = ~result_zero_bits;
  wmin = result_one_bits;
  /* If the range has all positive or all negative values, the result
     is better than VARYING.  */
  if (wi::lt_p (wmin, 0, sign) || wi::ge_p (wmax, 0, sign))
    return true;
  wmin = wi::min_value (prec, sign);
  wmax = wi::max_value (prec, sign);
  return false;
}

/* Calculate the IOR of two ranges and store the result in [WMIN,WMAX].
   Return TRUE if we were able to successfully calculate the new range.  */

bool
wide_int_range_bit_ior (wide_int &wmin, wide_int &wmax,
			signop sign,
			const wide_int &vr0_min,
			const wide_int &vr0_max,
			const wide_int &vr1_min,
			const wide_int &vr1_max,
			const wide_int &must_be_nonzero0,
			const wide_int &may_be_nonzero0,
			const wide_int &must_be_nonzero1,
			const wide_int &may_be_nonzero1)
{
  if (wide_int_range_optimize_bit_op (wmin, wmax, BIT_IOR_EXPR, sign,
				      vr0_min, vr0_max,
				      vr1_min, vr1_max))
    return true;
  wmin = must_be_nonzero0 | must_be_nonzero1;
  wmax = may_be_nonzero0 | may_be_nonzero1;
  /* If the input ranges contain only positive values we can
     truncate the minimum of the result range to the maximum
     of the input range minima.  */
  if (wi::ge_p (vr0_min, 0, sign)
      && wi::ge_p (vr1_min, 0, sign))
    {
      wmin = wi::max (wmin, vr0_min, sign);
      wmin = wi::max (wmin, vr1_min, sign);
    }
  /* If either input range contains only negative values
     we can truncate the minimum of the result range to the
     respective minimum range.  */
  if (wi::lt_p (vr0_max, 0, sign))
    wmin = wi::max (wmin, vr0_min, sign);
  if (wi::lt_p (vr1_max, 0, sign))
    wmin = wi::max (wmin, vr1_min, sign);
  /* If the limits got swapped around, indicate error so we can adjust
     the range to VARYING.  */
  if (wi::gt_p (wmin, wmax,sign))
    return false;
  return true;
}

/* Calculate the bitwise AND of two ranges and store the result in [WMIN,WMAX].
   Return TRUE if we were able to successfully calculate the new range.  */

bool
wide_int_range_bit_and (wide_int &wmin, wide_int &wmax,
			signop sign,
			unsigned prec,
			const wide_int &vr0_min,
			const wide_int &vr0_max,
			const wide_int &vr1_min,
			const wide_int &vr1_max,
			const wide_int &must_be_nonzero0,
			const wide_int &may_be_nonzero0,
			const wide_int &must_be_nonzero1,
			const wide_int &may_be_nonzero1)
{
  if (wide_int_range_optimize_bit_op (wmin, wmax, BIT_AND_EXPR, sign,
				      vr0_min, vr0_max,
				      vr1_min, vr1_max))
    return true;
  wmin = must_be_nonzero0 & must_be_nonzero1;
  wmax = may_be_nonzero0 & may_be_nonzero1;
  /* If both input ranges contain only negative values we can
     truncate the result range maximum to the minimum of the
     input range maxima.  */
  if (wi::lt_p (vr0_max, 0, sign) && wi::lt_p (vr1_max, 0, sign))
    {
      wmax = wi::min (wmax, vr0_max, sign);
      wmax = wi::min (wmax, vr1_max, sign);
    }
  /* If either input range contains only non-negative values
     we can truncate the result range maximum to the respective
     maximum of the input range.  */
  if (wi::ge_p (vr0_min, 0, sign))
    wmax = wi::min (wmax, vr0_max, sign);
  if (wi::ge_p (vr1_min, 0, sign))
    wmax = wi::min (wmax, vr1_max, sign);
  /* PR68217: In case of signed & sign-bit-CST should
     result in [-INF, 0] instead of [-INF, INF].  */
  if (wi::gt_p (wmin, wmax, sign))
    {
      wide_int sign_bit = wi::set_bit_in_zero (prec - 1, prec);
      if (sign == SIGNED
	  && ((wi::eq_p (vr0_min, vr0_max)
	       && !wi::cmps (vr0_min, sign_bit))
	      || (wi::eq_p (vr1_min, vr1_max)
		  && !wi::cmps (vr1_min, sign_bit))))
	{
	  wmin = wi::min_value (prec, sign);
	  wmax = wi::zero (prec);
	}
    }
  /* If the limits got swapped around, indicate error so we can adjust
     the range to VARYING.  */
  if (wi::gt_p (wmin, wmax,sign))
    return false;
  return true;
}

/* Calculate TRUNC_MOD_EXPR on two ranges and store the result in
   [WMIN,WMAX].  */

void
wide_int_range_trunc_mod (wide_int &wmin, wide_int &wmax,
			  signop sign,
			  unsigned prec,
			  const wide_int &vr0_min,
			  const wide_int &vr0_max,
			  const wide_int &vr1_min,
			  const wide_int &vr1_max)
{
  wide_int tmp;

  /* ABS (A % B) < ABS (B) and either
     0 <= A % B <= A or A <= A % B <= 0.  */
  wmax = vr1_max - 1;
  if (sign == SIGNED)
    {
      tmp = -1 - vr1_min;
      wmax = wi::smax (wmax, tmp);
    }

  if (sign == UNSIGNED)
    wmin = wi::zero (prec);
  else
    {
      wmin = -wmax;
      tmp = vr0_min;
      if (wi::gts_p (tmp, 0))
	tmp = wi::zero (prec);
      wmin = wi::smax (wmin, tmp);
    }
  tmp = vr0_max;
  if (sign == SIGNED && wi::neg_p (tmp))
    tmp = wi::zero (prec);
  wmax = wi::min (wmax, tmp, sign);
}

/* Calculate ABS_EXPR on a range and store the result in [MIN, MAX].  */

bool
wide_int_range_abs (wide_int &min, wide_int &max,
		    signop sign, unsigned prec,
		    const wide_int &vr0_min, const wide_int &vr0_max,
		    bool overflow_undefined)
{
  /* Pass through VR0 the easy cases.  */
  if (sign == UNSIGNED || wi::ge_p (vr0_min, 0, sign))
    {
      min = vr0_min;
      max = vr0_max;
      return true;
    }

  /* -TYPE_MIN_VALUE = TYPE_MIN_VALUE with flag_wrapv so we can't get a
     useful range.  */
  wide_int min_value = wi::min_value (prec, sign);
  wide_int max_value = wi::max_value (prec, sign);
  if (!overflow_undefined && wi::eq_p (vr0_min, min_value))
    return false;

  /* ABS_EXPR may flip the range around, if the original range
     included negative values.  */
  if (wi::eq_p (vr0_min, min_value))
    min = max_value;
  else
    min = wi::abs (vr0_min);
  if (wi::eq_p (vr0_max, min_value))
    max = max_value;
  else
    max = wi::abs (vr0_max);

  /* If the range contains zero then we know that the minimum value in the
     range will be zero.  */
  if (wi::le_p (vr0_min, 0, sign) && wi::ge_p (vr0_max, 0, sign))
    {
      if (wi::gt_p (min, max, sign))
	max = min;
      min = wi::zero (prec);
    }
  else
    {
      /* If the range was reversed, swap MIN and MAX.  */
      if (wi::gt_p (min, max, sign))
	std::swap (min, max);
    }

  /* If the new range has its limits swapped around (MIN > MAX), then
     the operation caused one of them to wrap around.  The only thing
     we know is that the result is positive.  */
  if (wi::gt_p (min, max, sign))
    {
      min = wi::zero (prec);
      max = max_value;
    }
  return true;
}

/* Convert range in [VR0_MIN, VR0_MAX] with INNER_SIGN and INNER_PREC,
   to a range in [MIN, MAX] with OUTER_SIGN and OUTER_PREC.

   Return TRUE if we were able to successfully calculate the new range.

   Caller is responsible for canonicalizing the resulting range.  */

bool
wide_int_range_convert (wide_int &min, wide_int &max,
			signop inner_sign,
			unsigned inner_prec,
			signop outer_sign,
			unsigned outer_prec,
			const wide_int &vr0_min,
			const wide_int &vr0_max)
{
  /* If the conversion is not truncating we can convert the min and
     max values and canonicalize the resulting range.  Otherwise we
     can do the conversion if the size of the range is less than what
     the precision of the target type can represent.  */
  if (outer_prec >= inner_prec
      || wi::rshift (wi::sub (vr0_max, vr0_min),
		     wi::uhwi (outer_prec, inner_prec),
		     inner_sign) == 0)
    {
      min = wide_int::from (vr0_min, outer_prec, inner_sign);
      max = wide_int::from (vr0_max, outer_prec, inner_sign);
      return (!wi::eq_p (min, wi::min_value (outer_prec, outer_sign))
	      || !wi::eq_p (max, wi::max_value (outer_prec, outer_sign)));
    }
  return false;
}

/* Calculate a division operation on two ranges and store the result in
   [WMIN, WMAX] U [EXTRA_MIN, EXTRA_MAX].

   If EXTRA_RANGE_P is set upon return, EXTRA_MIN/EXTRA_MAX hold
   meaningful information, otherwise they should be ignored.

   Return TRUE if we were able to successfully calculate the new range.  */

bool
wide_int_range_div (wide_int &wmin, wide_int &wmax,
		    tree_code code, signop sign, unsigned prec,
		    const wide_int &dividend_min, const wide_int &dividend_max,
		    const wide_int &divisor_min, const wide_int &divisor_max,
		    bool overflow_undefined,
		    bool overflow_wraps,
		    bool &extra_range_p,
		    wide_int &extra_min, wide_int &extra_max)
{
  extra_range_p = false;

  /* If we know we won't divide by zero, just do the division.  */
  if (!wide_int_range_includes_zero_p (divisor_min, divisor_max, sign))
    return wide_int_range_multiplicative_op (wmin, wmax, code, sign, prec,
					     dividend_min, dividend_max,
					     divisor_min, divisor_max,
					     overflow_undefined,
					     overflow_wraps);

  /* If flag_non_call_exceptions, we must not eliminate a division
     by zero.  */
  if (cfun->can_throw_non_call_exceptions)
    return false;

  /* If we're definitely dividing by zero, there's nothing to do.  */
  if (wide_int_range_zero_p (divisor_min, divisor_max, prec))
    return false;

  /* Perform the division in 2 parts, [LB, -1] and [1, UB],
     which will skip any division by zero.

     First divide by the negative numbers, if any.  */
  if (wi::neg_p (divisor_min, sign))
    {
      if (!wide_int_range_multiplicative_op (wmin, wmax,
					     code, sign, prec,
					     dividend_min, dividend_max,
					     divisor_min, wi::minus_one (prec),
					     overflow_undefined,
					     overflow_wraps))
	return false;
      extra_range_p = true;
    }
  /* Then divide by the non-zero positive numbers, if any.  */
  if (wi::gt_p (divisor_max, wi::zero (prec), sign))
    {
      if (!wide_int_range_multiplicative_op (extra_range_p ? extra_min : wmin,
					     extra_range_p ? extra_max : wmax,
					     code, sign, prec,
					     dividend_min, dividend_max,
					     wi::one (prec), divisor_max,
					     overflow_undefined,
					     overflow_wraps))
	return false;
    }
  else
    extra_range_p = false;
  return true;
}
