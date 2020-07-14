/* Support routines for value ranges.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.

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
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "tree-pretty-print.h"
#include "fold-const.h"

value_range::value_range (tree min, tree max, value_range_kind kind)
{
  set (min, max, kind);
}

value_range::value_range (tree type)
{
  set_varying (type);
}

value_range::value_range (tree type,
			  const wide_int &wmin, const wide_int &wmax,
			  enum value_range_kind kind)
{
  tree min = wide_int_to_tree (type, wmin);
  tree max = wide_int_to_tree (type, wmax);
  gcc_checking_assert (kind == VR_RANGE || kind == VR_ANTI_RANGE);
  set (min, max, kind);
}

void
value_range::set_undefined ()
{
  m_kind = VR_UNDEFINED;
  m_min = m_max = NULL;
}

void
value_range::set_varying (tree type)
{
  m_kind = VR_VARYING;
  if (supports_type_p (type))
    {
      m_min = vrp_val_min (type);
      m_max = vrp_val_max (type);
    }
  else
    /* We can't do anything range-wise with these types.  */
    m_min = m_max = error_mark_node;
}

/* Set value range to the canonical form of {VRTYPE, MIN, MAX, EQUIV}.
   This means adjusting VRTYPE, MIN and MAX representing the case of a
   wrapping range with MAX < MIN covering [MIN, type_max] U [type_min, MAX]
   as anti-rage ~[MAX+1, MIN-1].  Likewise for wrapping anti-ranges.
   In corner cases where MAX+1 or MIN-1 wraps this will fall back
   to varying.
   This routine exists to ease canonicalization in the case where we
   extract ranges from var + CST op limit.  */

void
value_range::set (tree min, tree max, value_range_kind kind)
{
  /* Use the canonical setters for VR_UNDEFINED and VR_VARYING.  */
  if (kind == VR_UNDEFINED)
    {
      set_undefined ();
      return;
    }

  if (kind == VR_RANGE)
    {
      /* Convert POLY_INT_CST bounds into worst-case INTEGER_CST bounds.  */
      if (POLY_INT_CST_P (min))
	{
	  tree type_min = vrp_val_min (TREE_TYPE (min));
	  widest_int lb
	    = constant_lower_bound_with_limit (wi::to_poly_widest (min),
					       wi::to_widest (type_min));
	  min = wide_int_to_tree (TREE_TYPE (min), lb);
	}
      if (POLY_INT_CST_P (max))
	{
	  tree type_max = vrp_val_max (TREE_TYPE (max));
	  widest_int ub
	    = constant_upper_bound_with_limit (wi::to_poly_widest (max),
					       wi::to_widest (type_max));
	  max = wide_int_to_tree (TREE_TYPE (max), ub);
	}
    }
  else if (kind != VR_VARYING)
    {
      if (POLY_INT_CST_P (min) || POLY_INT_CST_P (max))
	kind = VR_VARYING;
    }

  if (kind == VR_VARYING)
    {
      gcc_assert (TREE_TYPE (min) == TREE_TYPE (max));
      tree typ = TREE_TYPE (min);
      if (supports_type_p (typ))
	{
	  gcc_assert (vrp_val_min (typ));
	  gcc_assert (vrp_val_max (typ));
	}
      set_varying (typ);
      return;
    }

  /* Nothing to canonicalize for symbolic ranges.  */
  if (TREE_CODE (min) != INTEGER_CST
      || TREE_CODE (max) != INTEGER_CST)
    {
      m_kind = kind;
      m_min = min;
      m_max = max;
      return;
    }

  /* Wrong order for min and max, to swap them and the VR type we need
     to adjust them.  */
  if (tree_int_cst_lt (max, min))
    {
      tree one, tmp;

      /* For one bit precision if max < min, then the swapped
	 range covers all values, so for VR_RANGE it is varying and
	 for VR_ANTI_RANGE empty range, so drop to varying as well.  */
      if (TYPE_PRECISION (TREE_TYPE (min)) == 1)
	{
	  set_varying (TREE_TYPE (min));
	  return;
	}

      one = build_int_cst (TREE_TYPE (min), 1);
      tmp = int_const_binop (PLUS_EXPR, max, one);
      max = int_const_binop (MINUS_EXPR, min, one);
      min = tmp;

      /* There's one corner case, if we had [C+1, C] before we now have
	 that again.  But this represents an empty value range, so drop
	 to varying in this case.  */
      if (tree_int_cst_lt (max, min))
	{
	  set_varying (TREE_TYPE (min));
	  return;
	}

      kind = kind == VR_RANGE ? VR_ANTI_RANGE : VR_RANGE;
    }

  tree type = TREE_TYPE (min);

  /* Anti-ranges that can be represented as ranges should be so.  */
  if (kind == VR_ANTI_RANGE)
    {
      /* For -fstrict-enums we may receive out-of-range ranges so consider
         values < -INF and values > INF as -INF/INF as well.  */
      bool is_min = vrp_val_is_min (min);
      bool is_max = vrp_val_is_max (max);

      if (is_min && is_max)
	{
	  /* We cannot deal with empty ranges, drop to varying.
	     ???  This could be VR_UNDEFINED instead.  */
	  set_varying (type);
	  return;
	}
      else if (TYPE_PRECISION (TREE_TYPE (min)) == 1
	       && (is_min || is_max))
	{
	  /* Non-empty boolean ranges can always be represented
	     as a singleton range.  */
	  if (is_min)
	    min = max = vrp_val_max (TREE_TYPE (min));
	  else
	    min = max = vrp_val_min (TREE_TYPE (min));
	  kind = VR_RANGE;
	}
      else if (is_min)
        {
	  tree one = build_int_cst (TREE_TYPE (max), 1);
	  min = int_const_binop (PLUS_EXPR, max, one);
	  max = vrp_val_max (TREE_TYPE (max));
	  kind = VR_RANGE;
        }
      else if (is_max)
        {
	  tree one = build_int_cst (TREE_TYPE (min), 1);
	  max = int_const_binop (MINUS_EXPR, min, one);
	  min = vrp_val_min (TREE_TYPE (min));
	  kind = VR_RANGE;
        }
    }

  /* Normalize [MIN, MAX] into VARYING and ~[MIN, MAX] into UNDEFINED.

     Avoid using TYPE_{MIN,MAX}_VALUE because -fstrict-enums can
     restrict those to a subset of what actually fits in the type.
     Instead use the extremes of the type precision which will allow
     compare_range_with_value() to check if a value is inside a range,
     whereas if we used TYPE_*_VAL, said function would just punt
     upon seeing a VARYING.  */
  unsigned prec = TYPE_PRECISION (type);
  signop sign = TYPE_SIGN (type);
  if (wi::eq_p (wi::to_wide (min), wi::min_value (prec, sign))
      && wi::eq_p (wi::to_wide (max), wi::max_value (prec, sign)))
    {
      if (kind == VR_RANGE)
	set_varying (type);
      else if (kind == VR_ANTI_RANGE)
	set_undefined ();
      else
	gcc_unreachable ();
      return;
    }

  /* Do not drop [-INF(OVF), +INF(OVF)] to varying.  (OVF) has to be sticky
     to make sure VRP iteration terminates, otherwise we can get into
     oscillations.  */

  m_kind = kind;
  m_min = min;
  m_max = max;
  if (flag_checking)
    check ();
}

void
value_range::set (tree val)
{
  gcc_assert (TREE_CODE (val) == SSA_NAME || is_gimple_min_invariant (val));
  if (TREE_OVERFLOW_P (val))
    val = drop_tree_overflow (val);
  set (val, val);
}

/* Set value range VR to a nonzero range of type TYPE.  */

void
value_range::set_nonzero (tree type)
{
  tree zero = build_int_cst (type, 0);
  set (zero, zero, VR_ANTI_RANGE);
}

/* Set value range VR to a ZERO range of type TYPE.  */

void
value_range::set_zero (tree type)
{
  set (build_int_cst (type, 0));
}

/* Check the validity of the range.  */

void
value_range::check ()
{
  switch (m_kind)
    {
    case VR_RANGE:
    case VR_ANTI_RANGE:
      {
	gcc_assert (m_min && m_max);
	gcc_assert (!TREE_OVERFLOW_P (m_min) && !TREE_OVERFLOW_P (m_max));

	/* Creating ~[-MIN, +MAX] is stupid because that would be
	   the empty set.  */
	if (INTEGRAL_TYPE_P (TREE_TYPE (m_min)) && m_kind == VR_ANTI_RANGE)
	  gcc_assert (!vrp_val_is_min (m_min) || !vrp_val_is_max (m_max));

	int cmp = compare_values (m_min, m_max);
	gcc_assert (cmp == 0 || cmp == -1 || cmp == -2);
	break;
      }
    case VR_UNDEFINED:
      gcc_assert (!min () && !max ());
      break;
    case VR_VARYING:
      gcc_assert (m_min && m_max);
      break;
    default:
      gcc_unreachable ();
    }
}

/* Return the number of sub-ranges in a range.  */

unsigned
value_range::num_pairs () const
{
  if (undefined_p ())
    return 0;
  if (varying_p ())
    return 1;
  if (symbolic_p ())
    {
      value_range numeric_range (*this);
      numeric_range.normalize_symbolics ();
      return numeric_range.num_pairs ();
    }
  if (m_kind == VR_ANTI_RANGE)
    {
      // ~[MIN, X] has one sub-range of [X+1, MAX], and
      // ~[X, MAX] has one sub-range of [MIN, X-1].
      if (vrp_val_is_min (m_min) || vrp_val_is_max (m_max))
	return 1;
      return 2;
    }
  return 1;
}

/* Return the lower bound for a sub-range.  PAIR is the sub-range in
   question.  */

wide_int
value_range::lower_bound (unsigned pair) const
{
  if (symbolic_p ())
    {
      value_range numeric_range (*this);
      numeric_range.normalize_symbolics ();
      return numeric_range.lower_bound (pair);
    }

  gcc_checking_assert (!undefined_p ());
  gcc_checking_assert (pair + 1 <= num_pairs ());
  tree t = NULL;
  if (m_kind == VR_ANTI_RANGE)
    {
      tree typ = type ();
      if (pair == 1 || vrp_val_is_min (m_min))
	t = wide_int_to_tree (typ, wi::to_wide (m_max) + 1);
      else
	t = vrp_val_min (typ);
    }
  else
    t = m_min;
  return wi::to_wide (t);
}

/* Return the upper bound for a sub-range.  PAIR is the sub-range in
   question.  */

wide_int
value_range::upper_bound (unsigned pair) const
{
  if (symbolic_p ())
    {
      value_range numeric_range (*this);
      numeric_range.normalize_symbolics ();
      return numeric_range.upper_bound (pair);
    }

  gcc_checking_assert (!undefined_p ());
  gcc_checking_assert (pair + 1 <= num_pairs ());
  tree t = NULL;
  if (m_kind == VR_ANTI_RANGE)
    {
      tree typ = type ();
      if (pair == 1 || vrp_val_is_min (m_min))
	t = vrp_val_max (typ);
      else
	t = wide_int_to_tree (typ, wi::to_wide (m_min) - 1);
    }
  else
    t = m_max;
  return wi::to_wide (t);
}

/* Return the highest bound in a range.  */

wide_int
value_range::upper_bound () const
{
  unsigned pairs = num_pairs ();
  gcc_checking_assert (pairs > 0);
  return upper_bound (pairs - 1);
}

bool
value_range::equal_p (const value_range &other) const
{
  /* Ignore types for undefined.  All undefines are equal.  */
  if (undefined_p ())
    return m_kind == other.m_kind;

  return (m_kind == other.m_kind
	  && vrp_operand_equal_p (m_min, other.m_min)
	  && vrp_operand_equal_p (m_max, other.m_max));
}

bool
value_range::operator== (const value_range &r) const
{
  return equal_p (r);
}

/* If range is a singleton, place it in RESULT and return TRUE.
   Note: A singleton can be any gimple invariant, not just constants.
   So, [&x, &x] counts as a singleton.  */
/* Return TRUE if this is a symbolic range.  */

bool
value_range::symbolic_p () const
{
  return (!varying_p ()
	  && !undefined_p ()
	  && (!is_gimple_min_invariant (m_min)
	      || !is_gimple_min_invariant (m_max)));
}

/* NOTE: This is not the inverse of symbolic_p because the range
   could also be varying or undefined.  Ideally they should be inverse
   of each other, with varying only applying to symbolics.  Varying of
   constants would be represented as [-MIN, +MAX].  */

bool
value_range::constant_p () const
{
  return (!varying_p ()
	  && !undefined_p ()
	  && TREE_CODE (m_min) == INTEGER_CST
	  && TREE_CODE (m_max) == INTEGER_CST);
}

bool
value_range::singleton_p (tree *result) const
{
  if (m_kind == VR_ANTI_RANGE)
    {
      if (nonzero_p ())
	{
	  if (TYPE_PRECISION (type ()) == 1)
	    {
	      if (result)
		*result = m_max;
	      return true;
	    }
	  return false;
	}
      if (num_pairs () == 1)
	{
	  value_range vr0, vr1;
	  ranges_from_anti_range (this, &vr0, &vr1);
	  return vr0.singleton_p (result);
	}
    }
  if (m_kind == VR_RANGE
      && vrp_operand_equal_p (min (), max ())
      && is_gimple_min_invariant (min ()))
    {
      if (result)
        *result = min ();
      return true;
    }
  return false;
}

/* Return 1 if VAL is inside value range.
          0 if VAL is not inside value range.
	 -2 if we cannot tell either way.

   Benchmark compile/20001226-1.c compilation time after changing this
   function.  */

int
value_range::value_inside_range (tree val) const
{
  int cmp1, cmp2;

  if (varying_p ())
    return 1;

  if (undefined_p ())
    return 0;

  cmp1 = operand_less_p (val, m_min);
  if (cmp1 == -2)
    return -2;
  if (cmp1 == 1)
    return m_kind != VR_RANGE;

  cmp2 = operand_less_p (m_max, val);
  if (cmp2 == -2)
    return -2;

  if (m_kind == VR_RANGE)
    return !cmp2;
  else
    return !!cmp2;
}

/* Return TRUE if it is possible that range contains VAL.  */

bool
value_range::may_contain_p (tree val) const
{
  return value_inside_range (val) != 0;
}

/* Return TRUE if range contains INTEGER_CST.  */

bool
value_range::contains_p (tree cst) const
{
  gcc_checking_assert (TREE_CODE (cst) == INTEGER_CST);
  if (symbolic_p ())
    {
      value_range numeric_range (*this);
      numeric_range.normalize_symbolics ();
      return numeric_range.contains_p (cst);
    }
  return value_inside_range (cst) == 1;
}

/* Normalize addresses into constants.  */

void
value_range::normalize_addresses ()
{
  if (undefined_p ())
    return;

  if (!POINTER_TYPE_P (type ()) || range_has_numeric_bounds_p (this))
    return;

  if (!range_includes_zero_p (this))
    {
      gcc_checking_assert (TREE_CODE (m_min) == ADDR_EXPR
			   || TREE_CODE (m_max) == ADDR_EXPR);
      set_nonzero (type ());
      return;
    }
  set_varying (type ());
}

/* Normalize symbolics and addresses into constants.  */

void
value_range::normalize_symbolics ()
{
  if (varying_p () || undefined_p ())
    return;

  tree ttype = type ();
  bool min_symbolic = !is_gimple_min_invariant (min ());
  bool max_symbolic = !is_gimple_min_invariant (max ());
  if (!min_symbolic && !max_symbolic)
    {
      normalize_addresses ();
      return;
    }

  // [SYM, SYM] -> VARYING
  if (min_symbolic && max_symbolic)
    {
      set_varying (ttype);
      return;
    }
  if (kind () == VR_RANGE)
    {
      // [SYM, NUM] -> [-MIN, NUM]
      if (min_symbolic)
	{
	  set (vrp_val_min (ttype), max ());
	  return;
	}
      // [NUM, SYM] -> [NUM, +MAX]
      set (min (), vrp_val_max (ttype));
      return;
    }
  gcc_checking_assert (kind () == VR_ANTI_RANGE);
  // ~[SYM, NUM] -> [NUM + 1, +MAX]
  if (min_symbolic)
    {
      if (!vrp_val_is_max (max ()))
	{
	  tree n = wide_int_to_tree (ttype, wi::to_wide (max ()) + 1);
	  set (n, vrp_val_max (ttype));
	  return;
	}
      set_varying (ttype);
      return;
    }
  // ~[NUM, SYM] -> [-MIN, NUM - 1]
  if (!vrp_val_is_min (min ()))
    {
      tree n = wide_int_to_tree (ttype, wi::to_wide (min ()) - 1);
      set (vrp_val_min (ttype), n);
      return;
    }
  set_varying (ttype);
}

/* Intersect the two value-ranges { *VR0TYPE, *VR0MIN, *VR0MAX } and
   { VR1TYPE, VR0MIN, VR0MAX } and store the result
   in { *VR0TYPE, *VR0MIN, *VR0MAX }.  This may not be the smallest
   possible such range.  The resulting range is not canonicalized.  */

static void
intersect_ranges (enum value_range_kind *vr0type,
		  tree *vr0min, tree *vr0max,
		  enum value_range_kind vr1type,
		  tree vr1min, tree vr1max)
{
  bool mineq = vrp_operand_equal_p (*vr0min, vr1min);
  bool maxeq = vrp_operand_equal_p (*vr0max, vr1max);

  /* [] is vr0, () is vr1 in the following classification comments.  */
  if (mineq && maxeq)
    {
      /* [(  )] */
      if (*vr0type == vr1type)
	/* Nothing to do for equal ranges.  */
	;
      else if ((*vr0type == VR_RANGE
		&& vr1type == VR_ANTI_RANGE)
	       || (*vr0type == VR_ANTI_RANGE
		   && vr1type == VR_RANGE))
	{
	  /* For anti-range with range intersection the result is empty.  */
	  *vr0type = VR_UNDEFINED;
	  *vr0min = NULL_TREE;
	  *vr0max = NULL_TREE;
	}
      else
	gcc_unreachable ();
    }
  else if (operand_less_p (*vr0max, vr1min) == 1
	   || operand_less_p (vr1max, *vr0min) == 1)
    {
      /* [ ] ( ) or ( ) [ ]
	 If the ranges have an empty intersection, the result of the
	 intersect operation is the range for intersecting an
	 anti-range with a range or empty when intersecting two ranges.  */
      if (*vr0type == VR_RANGE
	  && vr1type == VR_ANTI_RANGE)
	;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  *vr0type = vr1type;
	  *vr0min = vr1min;
	  *vr0max = vr1max;
	}
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_RANGE)
	{
	  *vr0type = VR_UNDEFINED;
	  *vr0min = NULL_TREE;
	  *vr0max = NULL_TREE;
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  /* If the anti-ranges are adjacent to each other merge them.  */
	  if (TREE_CODE (*vr0max) == INTEGER_CST
	      && TREE_CODE (vr1min) == INTEGER_CST
	      && operand_less_p (*vr0max, vr1min) == 1
	      && integer_onep (int_const_binop (MINUS_EXPR,
						vr1min, *vr0max)))
	    *vr0max = vr1max;
	  else if (TREE_CODE (vr1max) == INTEGER_CST
		   && TREE_CODE (*vr0min) == INTEGER_CST
		   && operand_less_p (vr1max, *vr0min) == 1
		   && integer_onep (int_const_binop (MINUS_EXPR,
						     *vr0min, vr1max)))
	    *vr0min = vr1min;
	  /* Else arbitrarily take VR0.  */
	}
    }
  else if ((maxeq || operand_less_p (vr1max, *vr0max) == 1)
	   && (mineq || operand_less_p (*vr0min, vr1min) == 1))
    {
      /* [ (  ) ] or [(  ) ] or [ (  )] */
      if (*vr0type == VR_RANGE
	  && vr1type == VR_RANGE)
	{
	  /* If both are ranges the result is the inner one.  */
	  *vr0type = vr1type;
	  *vr0min = vr1min;
	  *vr0max = vr1max;
	}
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  /* Choose the right gap if the left one is empty.  */
	  if (mineq)
	    {
	      if (TREE_CODE (vr1max) != INTEGER_CST)
		*vr0min = vr1max;
	      else if (TYPE_PRECISION (TREE_TYPE (vr1max)) == 1
		       && !TYPE_UNSIGNED (TREE_TYPE (vr1max)))
		*vr0min
		  = int_const_binop (MINUS_EXPR, vr1max,
				     build_int_cst (TREE_TYPE (vr1max), -1));
	      else
		*vr0min
		  = int_const_binop (PLUS_EXPR, vr1max,
				     build_int_cst (TREE_TYPE (vr1max), 1));
	    }
	  /* Choose the left gap if the right one is empty.  */
	  else if (maxeq)
	    {
	      if (TREE_CODE (vr1min) != INTEGER_CST)
		*vr0max = vr1min;
	      else if (TYPE_PRECISION (TREE_TYPE (vr1min)) == 1
		       && !TYPE_UNSIGNED (TREE_TYPE (vr1min)))
		*vr0max
		  = int_const_binop (PLUS_EXPR, vr1min,
				     build_int_cst (TREE_TYPE (vr1min), -1));
	      else
		*vr0max
		  = int_const_binop (MINUS_EXPR, vr1min,
				     build_int_cst (TREE_TYPE (vr1min), 1));
	    }
	  /* Choose the anti-range if the range is effectively varying.  */
	  else if (vrp_val_is_min (*vr0min)
		   && vrp_val_is_max (*vr0max))
	    {
	      *vr0type = vr1type;
	      *vr0min = vr1min;
	      *vr0max = vr1max;
	    }
	  /* Else choose the range.  */
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_ANTI_RANGE)
	/* If both are anti-ranges the result is the outer one.  */
	;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  /* The intersection is empty.  */
	  *vr0type = VR_UNDEFINED;
	  *vr0min = NULL_TREE;
	  *vr0max = NULL_TREE;
	}
      else
	gcc_unreachable ();
    }
  else if ((maxeq || operand_less_p (*vr0max, vr1max) == 1)
	   && (mineq || operand_less_p (vr1min, *vr0min) == 1))
    {
      /* ( [  ] ) or ([  ] ) or ( [  ]) */
      if (*vr0type == VR_RANGE
	  && vr1type == VR_RANGE)
	/* Choose the inner range.  */
	;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  /* Choose the right gap if the left is empty.  */
	  if (mineq)
	    {
	      *vr0type = VR_RANGE;
	      if (TREE_CODE (*vr0max) != INTEGER_CST)
		*vr0min = *vr0max;
	      else if (TYPE_PRECISION (TREE_TYPE (*vr0max)) == 1
		       && !TYPE_UNSIGNED (TREE_TYPE (*vr0max)))
		*vr0min
		  = int_const_binop (MINUS_EXPR, *vr0max,
				     build_int_cst (TREE_TYPE (*vr0max), -1));
	      else
		*vr0min
		  = int_const_binop (PLUS_EXPR, *vr0max,
				     build_int_cst (TREE_TYPE (*vr0max), 1));
	      *vr0max = vr1max;
	    }
	  /* Choose the left gap if the right is empty.  */
	  else if (maxeq)
	    {
	      *vr0type = VR_RANGE;
	      if (TREE_CODE (*vr0min) != INTEGER_CST)
		*vr0max = *vr0min;
	      else if (TYPE_PRECISION (TREE_TYPE (*vr0min)) == 1
		       && !TYPE_UNSIGNED (TREE_TYPE (*vr0min)))
		*vr0max
		  = int_const_binop (PLUS_EXPR, *vr0min,
				     build_int_cst (TREE_TYPE (*vr0min), -1));
	      else
		*vr0max
		  = int_const_binop (MINUS_EXPR, *vr0min,
				     build_int_cst (TREE_TYPE (*vr0min), 1));
	      *vr0min = vr1min;
	    }
	  /* Choose the anti-range if the range is effectively varying.  */
	  else if (vrp_val_is_min (vr1min)
		   && vrp_val_is_max (vr1max))
	    ;
	  /* Choose the anti-range if it is ~[0,0], that range is special
	     enough to special case when vr1's range is relatively wide.
	     At least for types bigger than int - this covers pointers
	     and arguments to functions like ctz.  */
	  else if (*vr0min == *vr0max
		   && integer_zerop (*vr0min)
		   && ((TYPE_PRECISION (TREE_TYPE (*vr0min))
			>= TYPE_PRECISION (integer_type_node))
		       || POINTER_TYPE_P (TREE_TYPE (*vr0min)))
		   && TREE_CODE (vr1max) == INTEGER_CST
		   && TREE_CODE (vr1min) == INTEGER_CST
		   && (wi::clz (wi::to_wide (vr1max) - wi::to_wide (vr1min))
		       < TYPE_PRECISION (TREE_TYPE (*vr0min)) / 2))
	    ;
	  /* Else choose the range.  */
	  else
	    {
	      *vr0type = vr1type;
	      *vr0min = vr1min;
	      *vr0max = vr1max;
	    }
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  /* If both are anti-ranges the result is the outer one.  */
	  *vr0type = vr1type;
	  *vr0min = vr1min;
	  *vr0max = vr1max;
	}
      else if (vr1type == VR_ANTI_RANGE
	       && *vr0type == VR_RANGE)
	{
	  /* The intersection is empty.  */
	  *vr0type = VR_UNDEFINED;
	  *vr0min = NULL_TREE;
	  *vr0max = NULL_TREE;
	}
      else
	gcc_unreachable ();
    }
  else if ((operand_less_p (vr1min, *vr0max) == 1
	    || operand_equal_p (vr1min, *vr0max, 0))
	   && operand_less_p (*vr0min, vr1min) == 1)
    {
      /* [  (  ]  ) or [  ](  ) */
      if (*vr0type == VR_ANTI_RANGE
	  && vr1type == VR_ANTI_RANGE)
	*vr0max = vr1max;
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_RANGE)
	*vr0min = vr1min;
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  if (TREE_CODE (vr1min) == INTEGER_CST)
	    *vr0max = int_const_binop (MINUS_EXPR, vr1min,
				       build_int_cst (TREE_TYPE (vr1min), 1));
	  else
	    *vr0max = vr1min;
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  *vr0type = VR_RANGE;
	  if (TREE_CODE (*vr0max) == INTEGER_CST)
	    *vr0min = int_const_binop (PLUS_EXPR, *vr0max,
				       build_int_cst (TREE_TYPE (*vr0max), 1));
	  else
	    *vr0min = *vr0max;
	  *vr0max = vr1max;
	}
      else
	gcc_unreachable ();
    }
  else if ((operand_less_p (*vr0min, vr1max) == 1
	    || operand_equal_p (*vr0min, vr1max, 0))
	   && operand_less_p (vr1min, *vr0min) == 1)
    {
      /* (  [  )  ] or (  )[  ] */
      if (*vr0type == VR_ANTI_RANGE
	  && vr1type == VR_ANTI_RANGE)
	*vr0min = vr1min;
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_RANGE)
	*vr0max = vr1max;
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  if (TREE_CODE (vr1max) == INTEGER_CST)
	    *vr0min = int_const_binop (PLUS_EXPR, vr1max,
				       build_int_cst (TREE_TYPE (vr1max), 1));
	  else
	    *vr0min = vr1max;
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  *vr0type = VR_RANGE;
	  if (TREE_CODE (*vr0min) == INTEGER_CST)
	    *vr0max = int_const_binop (MINUS_EXPR, *vr0min,
				       build_int_cst (TREE_TYPE (*vr0min), 1));
	  else
	    *vr0max = *vr0min;
	  *vr0min = vr1min;
	}
      else
	gcc_unreachable ();
    }

  /* If we know the intersection is empty, there's no need to
     conservatively add anything else to the set.  */
  if (*vr0type == VR_UNDEFINED)
    return;

  /* As a fallback simply use { *VRTYPE, *VR0MIN, *VR0MAX } as
     result for the intersection.  That's always a conservative
     correct estimate unless VR1 is a constant singleton range
     in which case we choose that.  */
  if (vr1type == VR_RANGE
      && is_gimple_min_invariant (vr1min)
      && vrp_operand_equal_p (vr1min, vr1max))
    {
      *vr0type = vr1type;
      *vr0min = vr1min;
      *vr0max = vr1max;
    }
}

/* Helper for the intersection operation for value ranges.  Given two
   value ranges VR0 and VR1, return the intersection of the two
   ranges.  This may not be the smallest possible such range.  */

value_range
value_range::intersect_helper (const value_range *vr0, const value_range *vr1)
{
  /* If either range is VR_VARYING the other one wins.  */
  if (vr1->varying_p ())
    return *vr0;
  if (vr0->varying_p ())
    return *vr1;

  /* When either range is VR_UNDEFINED the resulting range is
     VR_UNDEFINED, too.  */
  if (vr0->undefined_p ())
    return *vr0;
  if (vr1->undefined_p ())
    return *vr1;

  value_range_kind vr0kind = vr0->kind ();
  tree vr0min = vr0->min ();
  tree vr0max = vr0->max ();
  intersect_ranges (&vr0kind, &vr0min, &vr0max,
		    vr1->kind (), vr1->min (), vr1->max ());
  /* Make sure to canonicalize the result though as the inversion of a
     VR_RANGE can still be a VR_RANGE.  Work on a temporary so we can
     fall back to vr0 when this turns things to varying.  */
  value_range tem;
  if (vr0kind == VR_UNDEFINED)
    tem.set_undefined ();
  else if (vr0kind == VR_VARYING)
    tem.set_varying (vr0->type ());
  else
    tem.set (vr0min, vr0max, vr0kind);
  /* If that failed, use the saved original VR0.  */
  if (tem.varying_p ())
    return *vr0;

  return tem;
}

/* Union the two value-ranges { *VR0TYPE, *VR0MIN, *VR0MAX } and
   { VR1TYPE, VR0MIN, VR0MAX } and store the result
   in { *VR0TYPE, *VR0MIN, *VR0MAX }.  This may not be the smallest
   possible such range.  The resulting range is not canonicalized.  */

static void
union_ranges (enum value_range_kind *vr0type,
	      tree *vr0min, tree *vr0max,
	      enum value_range_kind vr1type,
	      tree vr1min, tree vr1max)
{
  int cmpmin = compare_values (*vr0min, vr1min);
  int cmpmax = compare_values (*vr0max, vr1max);
  bool mineq = cmpmin == 0;
  bool maxeq = cmpmax == 0;

  /* [] is vr0, () is vr1 in the following classification comments.  */
  if (mineq && maxeq)
    {
      /* [(  )] */
      if (*vr0type == vr1type)
	/* Nothing to do for equal ranges.  */
	;
      else if ((*vr0type == VR_RANGE
		&& vr1type == VR_ANTI_RANGE)
	       || (*vr0type == VR_ANTI_RANGE
		   && vr1type == VR_RANGE))
	{
	  /* For anti-range with range union the result is varying.  */
	  goto give_up;
	}
      else
	gcc_unreachable ();
    }
  else if (operand_less_p (*vr0max, vr1min) == 1
	   || operand_less_p (vr1max, *vr0min) == 1)
    {
      /* [ ] ( ) or ( ) [ ]
	 If the ranges have an empty intersection, result of the union
	 operation is the anti-range or if both are anti-ranges
	 it covers all.  */
      if (*vr0type == VR_ANTI_RANGE
	  && vr1type == VR_ANTI_RANGE)
	goto give_up;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	;
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  *vr0type = vr1type;
	  *vr0min = vr1min;
	  *vr0max = vr1max;
	}
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_RANGE)
	{
	  /* The result is the convex hull of both ranges.  */
	  if (operand_less_p (*vr0max, vr1min) == 1)
	    {
	      /* If the result can be an anti-range, create one.  */
	      if (TREE_CODE (*vr0max) == INTEGER_CST
		  && TREE_CODE (vr1min) == INTEGER_CST
		  && vrp_val_is_min (*vr0min)
		  && vrp_val_is_max (vr1max))
		{
		  tree min = int_const_binop (PLUS_EXPR,
					      *vr0max,
					      build_int_cst (TREE_TYPE (*vr0max), 1));
		  tree max = int_const_binop (MINUS_EXPR,
					      vr1min,
					      build_int_cst (TREE_TYPE (vr1min), 1));
		  if (!operand_less_p (max, min))
		    {
		      *vr0type = VR_ANTI_RANGE;
		      *vr0min = min;
		      *vr0max = max;
		    }
		  else
		    *vr0max = vr1max;
		}
	      else
		*vr0max = vr1max;
	    }
	  else
	    {
	      /* If the result can be an anti-range, create one.  */
	      if (TREE_CODE (vr1max) == INTEGER_CST
		  && TREE_CODE (*vr0min) == INTEGER_CST
		  && vrp_val_is_min (vr1min)
		  && vrp_val_is_max (*vr0max))
		{
		  tree min = int_const_binop (PLUS_EXPR,
					      vr1max,
					      build_int_cst (TREE_TYPE (vr1max), 1));
		  tree max = int_const_binop (MINUS_EXPR,
					      *vr0min,
					      build_int_cst (TREE_TYPE (*vr0min), 1));
		  if (!operand_less_p (max, min))
		    {
		      *vr0type = VR_ANTI_RANGE;
		      *vr0min = min;
		      *vr0max = max;
		    }
		  else
		    *vr0min = vr1min;
		}
	      else
		*vr0min = vr1min;
	    }
	}
      else
	gcc_unreachable ();
    }
  else if ((maxeq || cmpmax == 1)
	   && (mineq || cmpmin == -1))
    {
      /* [ (  ) ] or [(  ) ] or [ (  )] */
      if (*vr0type == VR_RANGE
	  && vr1type == VR_RANGE)
	;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  *vr0type = vr1type;
	  *vr0min = vr1min;
	  *vr0max = vr1max;
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  /* Arbitrarily choose the right or left gap.  */
	  if (!mineq && TREE_CODE (vr1min) == INTEGER_CST)
	    *vr0max = int_const_binop (MINUS_EXPR, vr1min,
				       build_int_cst (TREE_TYPE (vr1min), 1));
	  else if (!maxeq && TREE_CODE (vr1max) == INTEGER_CST)
	    *vr0min = int_const_binop (PLUS_EXPR, vr1max,
				       build_int_cst (TREE_TYPE (vr1max), 1));
	  else
	    goto give_up;
	}
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	/* The result covers everything.  */
	goto give_up;
      else
	gcc_unreachable ();
    }
  else if ((maxeq || cmpmax == -1)
	   && (mineq || cmpmin == 1))
    {
      /* ( [  ] ) or ([  ] ) or ( [  ]) */
      if (*vr0type == VR_RANGE
	  && vr1type == VR_RANGE)
	{
	  *vr0type = vr1type;
	  *vr0min = vr1min;
	  *vr0max = vr1max;
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_ANTI_RANGE)
	;
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  *vr0type = VR_ANTI_RANGE;
	  if (!mineq && TREE_CODE (*vr0min) == INTEGER_CST)
	    {
	      *vr0max = int_const_binop (MINUS_EXPR, *vr0min,
					 build_int_cst (TREE_TYPE (*vr0min), 1));
	      *vr0min = vr1min;
	    }
	  else if (!maxeq && TREE_CODE (*vr0max) == INTEGER_CST)
	    {
	      *vr0min = int_const_binop (PLUS_EXPR, *vr0max,
					 build_int_cst (TREE_TYPE (*vr0max), 1));
	      *vr0max = vr1max;
	    }
	  else
	    goto give_up;
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	/* The result covers everything.  */
	goto give_up;
      else
	gcc_unreachable ();
    }
  else if (cmpmin == -1
	   && cmpmax == -1
	   && (operand_less_p (vr1min, *vr0max) == 1
	       || operand_equal_p (vr1min, *vr0max, 0)))
    {
      /* [  (  ]  ) or [   ](   ) */
      if (*vr0type == VR_RANGE
	  && vr1type == VR_RANGE)
	*vr0max = vr1max;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_ANTI_RANGE)
	*vr0min = vr1min;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  if (TREE_CODE (vr1min) == INTEGER_CST)
	    *vr0max = int_const_binop (MINUS_EXPR, vr1min,
				       build_int_cst (TREE_TYPE (vr1min), 1));
	  else
	    goto give_up;
	}
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  if (TREE_CODE (*vr0max) == INTEGER_CST)
	    {
	      *vr0type = vr1type;
	      *vr0min = int_const_binop (PLUS_EXPR, *vr0max,
					 build_int_cst (TREE_TYPE (*vr0max), 1));
	      *vr0max = vr1max;
	    }
	  else
	    goto give_up;
	}
      else
	gcc_unreachable ();
    }
  else if (cmpmin == 1
	   && cmpmax == 1
	   && (operand_less_p (*vr0min, vr1max) == 1
	       || operand_equal_p (*vr0min, vr1max, 0)))
    {
      /* (  [  )  ] or (   )[   ] */
      if (*vr0type == VR_RANGE
	  && vr1type == VR_RANGE)
	*vr0min = vr1min;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_ANTI_RANGE)
	*vr0max = vr1max;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  if (TREE_CODE (vr1max) == INTEGER_CST)
	    *vr0min = int_const_binop (PLUS_EXPR, vr1max,
				       build_int_cst (TREE_TYPE (vr1max), 1));
	  else
	    goto give_up;
	}
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  if (TREE_CODE (*vr0min) == INTEGER_CST)
	    {
	      *vr0type = vr1type;
	      *vr0max = int_const_binop (MINUS_EXPR, *vr0min,
					 build_int_cst (TREE_TYPE (*vr0min), 1));
	      *vr0min = vr1min;
	    }
	  else
	    goto give_up;
	}
      else
	gcc_unreachable ();
    }
  else
    goto give_up;

  return;

give_up:
  *vr0type = VR_VARYING;
  *vr0min = NULL_TREE;
  *vr0max = NULL_TREE;
}

/* Helper for meet operation for value ranges.  Given two value ranges VR0 and
   VR1, return a range that contains both VR0 and VR1.  This may not be the
   smallest possible such range.  */

value_range
value_range::union_helper (const value_range *vr0, const value_range *vr1)
{
  /* VR0 has the resulting range if VR1 is undefined or VR0 is varying.  */
  if (vr1->undefined_p ()
      || vr0->varying_p ())
    return *vr0;

  /* VR1 has the resulting range if VR0 is undefined or VR1 is varying.  */
  if (vr0->undefined_p ()
      || vr1->varying_p ())
    return *vr1;

  value_range_kind vr0kind = vr0->kind ();
  tree vr0min = vr0->min ();
  tree vr0max = vr0->max ();
  union_ranges (&vr0kind, &vr0min, &vr0max,
		vr1->kind (), vr1->min (), vr1->max ());

  /* Work on a temporary so we can still use vr0 when union returns varying.  */
  value_range tem;
  if (vr0kind == VR_UNDEFINED)
    tem.set_undefined ();
  else if (vr0kind == VR_VARYING)
    tem.set_varying (vr0->type ());
  else
    tem.set (vr0min, vr0max, vr0kind);

  /* Failed to find an efficient meet.  Before giving up and setting
     the result to VARYING, see if we can at least derive a useful
     anti-range.  */
  if (tem.varying_p ()
      && range_includes_zero_p (vr0) == 0
      && range_includes_zero_p (vr1) == 0)
    {
      tem.set_nonzero (vr0->type ());
      return tem;
    }

  return tem;
}

/* Meet operation for value ranges.  Given two value ranges VR0 and
   VR1, store in VR0 a range that contains both VR0 and VR1.  This
   may not be the smallest possible such range.  */

void
value_range::union_ (const value_range *other)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Meeting\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\nand\n  ");
      dump_value_range (dump_file, other);
      fprintf (dump_file, "\n");
    }

  *this = union_helper (this, other);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "to\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\n");
    }
}

/* Range union, but for references.  */

void
value_range::union_ (const value_range &r)
{
  /* Disable details for now, because it makes the ranger dump
     unnecessarily verbose.  */
  bool details = dump_flags & TDF_DETAILS;
  if (details)
    dump_flags &= ~TDF_DETAILS;
  union_ (&r);
  if (details)
    dump_flags |= TDF_DETAILS;
}

void
value_range::intersect (const value_range *other)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Intersecting\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\nand\n  ");
      dump_value_range (dump_file, other);
      fprintf (dump_file, "\n");
    }

  *this = intersect_helper (this, other);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "to\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\n");
    }
}

/* Range intersect, but for references.  */

void
value_range::intersect (const value_range &r)
{
  /* Disable details for now, because it makes the ranger dump
     unnecessarily verbose.  */
  bool details = dump_flags & TDF_DETAILS;
  if (details)
    dump_flags &= ~TDF_DETAILS;
  intersect (&r);
  if (details)
    dump_flags |= TDF_DETAILS;
}

/* Return the inverse of a range.  */

void
value_range::invert ()
{
  /* We can't just invert VR_RANGE and VR_ANTI_RANGE because we may
     create non-canonical ranges.  Use the constructors instead.  */
  if (m_kind == VR_RANGE)
    *this = value_range (m_min, m_max, VR_ANTI_RANGE);
  else if (m_kind == VR_ANTI_RANGE)
    *this = value_range (m_min, m_max);
  else
    gcc_unreachable ();
}

void
value_range::dump (FILE *file) const
{
  if (undefined_p ())
    fprintf (file, "UNDEFINED");
  else if (m_kind == VR_RANGE || m_kind == VR_ANTI_RANGE)
    {
      tree ttype = type ();

      print_generic_expr (file, ttype);
      fprintf (file, " ");

      fprintf (file, "%s[", (m_kind == VR_ANTI_RANGE) ? "~" : "");

      if (INTEGRAL_TYPE_P (ttype)
	  && !TYPE_UNSIGNED (ttype)
	  && vrp_val_is_min (min ())
	  && TYPE_PRECISION (ttype) != 1)
	fprintf (file, "-INF");
      else
	print_generic_expr (file, min ());

      fprintf (file, ", ");

      if (supports_type_p (ttype)
	  && vrp_val_is_max (max ())
	  && TYPE_PRECISION (ttype) != 1)
	fprintf (file, "+INF");
      else
	print_generic_expr (file, max ());

      fprintf (file, "]");
    }
  else if (varying_p ())
    {
      print_generic_expr (file, type ());
      fprintf (file, " VARYING");
    }
  else
    gcc_unreachable ();
}

void
value_range::dump () const
{
  dump (stderr);
}

void
dump_value_range (FILE *file, const value_range *vr)
{
  if (!vr)
    fprintf (file, "[]");
  else
    vr->dump (file);
}

DEBUG_FUNCTION void
debug (const value_range *vr)
{
  dump_value_range (stderr, vr);
}

DEBUG_FUNCTION void
debug (const value_range &vr)
{
  dump_value_range (stderr, &vr);
}

/* Create two value-ranges in *VR0 and *VR1 from the anti-range *AR
   so that *VR0 U *VR1 == *AR.  Returns true if that is possible,
   false otherwise.  If *AR can be represented with a single range
   *VR1 will be VR_UNDEFINED.  */

bool
ranges_from_anti_range (const value_range *ar,
			value_range *vr0, value_range *vr1)
{
  tree type = ar->type ();

  vr0->set_undefined ();
  vr1->set_undefined ();

  /* As a future improvement, we could handle ~[0, A] as: [-INF, -1] U
     [A+1, +INF].  Not sure if this helps in practice, though.  */

  if (ar->kind () != VR_ANTI_RANGE
      || TREE_CODE (ar->min ()) != INTEGER_CST
      || TREE_CODE (ar->max ()) != INTEGER_CST
      || !vrp_val_min (type)
      || !vrp_val_max (type))
    return false;

  if (tree_int_cst_lt (vrp_val_min (type), ar->min ()))
    vr0->set (vrp_val_min (type),
	      wide_int_to_tree (type, wi::to_wide (ar->min ()) - 1));
  if (tree_int_cst_lt (ar->max (), vrp_val_max (type)))
    vr1->set (wide_int_to_tree (type, wi::to_wide (ar->max ()) + 1),
	      vrp_val_max (type));
  if (vr0->undefined_p ())
    {
      *vr0 = *vr1;
      vr1->set_undefined ();
    }

  return !vr0->undefined_p ();
}

bool
range_has_numeric_bounds_p (const value_range *vr)
{
  return (vr->min ()
	  && TREE_CODE (vr->min ()) == INTEGER_CST
	  && TREE_CODE (vr->max ()) == INTEGER_CST);
}

/* Return the maximum value for TYPE.  */

tree
vrp_val_max (const_tree type)
{
  if (INTEGRAL_TYPE_P (type))
    return TYPE_MAX_VALUE (type);
  if (POINTER_TYPE_P (type))
    {
      wide_int max = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));
      return wide_int_to_tree (const_cast<tree> (type), max);
    }
  return NULL_TREE;
}

/* Return the minimum value for TYPE.  */

tree
vrp_val_min (const_tree type)
{
  if (INTEGRAL_TYPE_P (type))
    return TYPE_MIN_VALUE (type);
  if (POINTER_TYPE_P (type))
    return build_zero_cst (const_cast<tree> (type));
  return NULL_TREE;
}

/* Return whether VAL is equal to the maximum value of its type.
   We can't do a simple equality comparison with TYPE_MAX_VALUE because
   C typedefs and Ada subtypes can produce types whose TYPE_MAX_VALUE
   is not == to the integer constant with the same value in the type.  */

bool
vrp_val_is_max (const_tree val)
{
  tree type_max = vrp_val_max (TREE_TYPE (val));
  return (val == type_max
	  || (type_max != NULL_TREE
	      && operand_equal_p (val, type_max, 0)));
}

/* Return whether VAL is equal to the minimum value of its type.  */

bool
vrp_val_is_min (const_tree val)
{
  tree type_min = vrp_val_min (TREE_TYPE (val));
  return (val == type_min
	  || (type_min != NULL_TREE
	      && operand_equal_p (val, type_min, 0)));
}

/* Return true, if VAL1 and VAL2 are equal values for VRP purposes.  */

bool
vrp_operand_equal_p (const_tree val1, const_tree val2)
{
  if (val1 == val2)
    return true;
  if (!val1 || !val2 || !operand_equal_p (val1, val2, 0))
    return false;
  return true;
}
