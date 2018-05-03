/* wide-int routines for trees and ranges.
   Copyright (C) 2018 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

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
#include "backend.h"
#include "insn-codes.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "wide-int.h"
#include "range.h"
#include "range-op.h"
#include "tree-vrp.h"
#include "fold-const.h"



/* Perform a binary tree operation on wide_ints  */

bool
wide_int_binop (enum tree_code code, wide_int& res, const wide_int& arg1,
		const wide_int& arg2, signop sign, bool& overflow)
{
  wide_int tmp;
  overflow = false;
  switch (code)
    {
    case BIT_IOR_EXPR:
      res = wi::bit_or (arg1, arg2);
      break;

    case BIT_XOR_EXPR:
      res = wi::bit_xor (arg1, arg2);
      break;

    case BIT_AND_EXPR:
      res = wi::bit_and (arg1, arg2);
      break;

    case RSHIFT_EXPR:
    case LSHIFT_EXPR:
      if (wi::neg_p (arg2))
	{
	  tmp = -arg2;
	  if (code == RSHIFT_EXPR)
	    code = LSHIFT_EXPR;
	  else
	    code = RSHIFT_EXPR;
	}
      else
        tmp = arg2;

      if (code == RSHIFT_EXPR)
	/* It's unclear from the C standard whether shifts can overflow.
	   The following code ignores overflow; perhaps a C standard
	   interpretation ruling is needed.  */
	res = wi::rshift (arg1, tmp, sign);
      else
	res = wi::lshift (arg1, tmp);
      break;

    case RROTATE_EXPR:
    case LROTATE_EXPR:
      if (wi::neg_p (arg2))
	{
	  tmp = -arg2;
	  if (code == RROTATE_EXPR)
	    code = LROTATE_EXPR;
	  else
	    code = RROTATE_EXPR;
	}
      else
        tmp = arg2;

      if (code == RROTATE_EXPR)
	res = wi::rrotate (arg1, tmp);
      else
	res = wi::lrotate (arg1, tmp);
      break;

    case PLUS_EXPR:
      res = wi::add (arg1, arg2, sign, &overflow);
      break;

    case MINUS_EXPR:
      res = wi::sub (arg1, arg2, sign, &overflow);
      break;

    case MULT_EXPR:
      res = wi::mul (arg1, arg2, sign, &overflow);
      break;

    case MULT_HIGHPART_EXPR:
      res = wi::mul_high (arg1, arg2, sign);
      break;

    case TRUNC_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::div_trunc (arg1, arg2, sign, &overflow);
      break;

    case FLOOR_DIV_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::div_floor (arg1, arg2, sign, &overflow);
      break;

    case CEIL_DIV_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::div_ceil (arg1, arg2, sign, &overflow);
      break;

    case ROUND_DIV_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::div_round (arg1, arg2, sign, &overflow);
      break;

    case TRUNC_MOD_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::mod_trunc (arg1, arg2, sign, &overflow);
      break;

    case FLOOR_MOD_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::mod_floor (arg1, arg2, sign, &overflow);
      break;

    case CEIL_MOD_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::mod_ceil (arg1, arg2, sign, &overflow);
      break;

    case ROUND_MOD_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::mod_round (arg1, arg2, sign, &overflow);
      break;

    case MIN_EXPR:
      res = wi::min (arg1, arg2, sign);
      break;

    case MAX_EXPR:
      res = wi::max (arg1, arg2, sign);
      break;

    default:
      return false;
    }

  return true;
}

bool
range_binop (enum tree_code code, wide_int& res, const wide_int& arg1,
	     const wide_int& arg2, signop sign, bool& overflow,
	     bool ov_undefined)
{
  if (!wide_int_binop (code, res, arg1, arg2, sign, overflow))
    return false;
  // for ranges, overflows can be converted to either max or min sometimes.
  if (overflow && ov_undefined)
    {
      switch (code)
        {
	  /* For multiplication, the sign of the overflow is given
	     by the comparison of the signs of the operands.  */
	  case MULT_EXPR:
	    if (sign == UNSIGNED || arg1.sign_mask () == arg2.sign_mask ())
	      res = wi::max_value (arg1.get_precision (), sign);
	    else
	      res = wi::min_value (arg1.get_precision (), sign);
	    break;

	  /* For division, the only case is -INF / -1 = +INF.  */
	  case TRUNC_DIV_EXPR:
          case FLOOR_DIV_EXPR:
          case CEIL_DIV_EXPR:
          case EXACT_DIV_EXPR:
          case ROUND_DIV_EXPR:
	    res = wi::max_value (arg1.get_precision (), sign);
	    break;

	  /* For addition, the operands must be of the same sign
             to yield an overflow.
	     For subtraction, operands must be of
             different signs to yield an overflow.  Its sign is
             therefore that of the first operand or the opposite of
             that of the second operand.  A first operand of 0 counts
             as positive here, for the corner case 0 - (-INF), which
             overflows, but must yield +INF.  */
	  case PLUS_EXPR:
	  case MINUS_EXPR:
	    if (sign == UNSIGNED || arg1.sign_mask () == 0)
	      res = wi::max_value (arg1.get_precision (), sign);
	    else
	      res = wi::min_value (arg1.get_precision (), sign);
	    break;
	      
	  default:
	    return false;
	}
      /* reset overflow if we set a max/min.  */
      overflow = false;
    }
  return true;
}
void
choose_min_max (signop s, wide_int& min, wide_int& max, wide_int& w0,
		wide_int& w1, wide_int& w2, wide_int& w3)

{
  // Order pairs w0,w1  and w2,w3.
  if (wi::gt_p (w0, w1, s))
    std::swap (w0, w1);
  if (wi::gt_p (w2, w3, s))
    std::swap (w2, w3);

  // Then choose min and max from the ordered pairs.
  min = wi::min (w0, w2, s);
  max = wi::max (w1, w3, s);
}

bool
do_cross_product (enum tree_code code, signop s, wide_int& lb, wide_int& ub,
		  const wide_int& lh_lb, const wide_int& lh_ub,
		  const wide_int& rh_lb, const wide_int& rh_ub) 
{
  bool ov;
  wide_int cp1, cp2, cp3, cp4;

  // Compute the 4 cross operations, bailing if an overflow occurs.
  
  if (!wide_int_binop (code, cp1, lh_lb, rh_lb, s, ov) || ov)
    return false;

  if (wi::eq_p (lh_lb, lh_ub))
    cp3 = cp1;
  else
    if (!wide_int_binop (code, cp3, lh_ub, rh_lb, s, ov) || ov)
      return false;

  if (wi::eq_p (rh_lb, rh_ub))
    cp2 = cp1;
  else
    if (!wide_int_binop (code, cp2, lh_lb, rh_ub, s, ov) || ov)
      return false;

  if (wi::eq_p (lh_lb, lh_ub))
    cp4 = cp2;
  else
    if (!wide_int_binop (code, cp4, lh_ub, rh_ub, s, ov) || ov)
      return false;

  // Order properly and add to the range.
  choose_min_max (s, lb, ub, cp1, cp2, cp3, cp4);
  return true;
}
