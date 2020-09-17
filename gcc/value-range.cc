/* Support routines for value ranges.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
   Major hacks by Aldy Hernandez <aldyh@redhat.com> and
   Andrew MacLeod <amacleod@redhat.com>.

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

// Here we copy between any two irange's.  The ranges can be legacy or
// multi-ranges, and copying between any combination works correctly.

irange &
irange::operator= (const irange &src)
{
  if (legacy_mode_p () != src.legacy_mode_p ())
    {
      copy_legacy_range (src);
      return *this;
    }
  if (legacy_mode_p ())
    {
      gcc_checking_assert (src.legacy_mode_p ());
      m_num_ranges = src.m_num_ranges;
      m_base[0] = src.m_base[0];
      m_base[1] = src.m_base[1];
      m_kind = src.m_kind;
      return *this;
    }

  unsigned x;
  unsigned lim = src.m_num_ranges;
  if (lim > m_max_ranges)
    lim = m_max_ranges;

  for (x = 0; x < lim * 2; ++x)
    m_base[x] = src.m_base[x];

  // If the range didn't fit, the last range should cover the rest.
  if (lim != src.m_num_ranges)
    m_base[x - 1] = src.m_base[src.m_num_ranges * 2 - 1];

  m_num_ranges = lim;
  return *this;
}

// Return TRUE if range is a multi-range that can be represented as a
// VR_ANTI_RANGE.

bool
irange::maybe_anti_range () const
{
  tree ttype = type ();
  unsigned int precision = TYPE_PRECISION (ttype);
  signop sign = TYPE_SIGN (ttype);
  return (num_pairs () > 1
	  && precision > 1
	  && lower_bound () == wi::min_value (precision, sign)
	  && upper_bound () == wi::max_value (precision, sign));
}

// Copy between a legacy and a multi-range, or vice-versa.

void
irange::copy_legacy_range (const irange &src)
{
  gcc_checking_assert (src.legacy_mode_p () != legacy_mode_p ());
  if (src.undefined_p ())
    set_undefined ();
  else if (src.varying_p ())
    set_varying (src.type ());
  else if (src.kind () == VR_ANTI_RANGE)
    {
      if (src.legacy_mode_p () && !range_has_numeric_bounds_p (&src))
	set_varying (src.type ());
      else
	set (src.min (), src.max (), VR_ANTI_RANGE);
    }
  else if (legacy_mode_p () && src.maybe_anti_range ())
    {
      int_range<3> tmp (src);
      tmp.invert ();
      set (tmp.min (), wide_int_to_tree (src.type (), tmp.upper_bound (0)),
	   VR_ANTI_RANGE);
    }
  else
    {
      // If copying legacy to int_range, normalize any symbolics.
      if (src.legacy_mode_p () && !range_has_numeric_bounds_p (&src))
	{
	  value_range cst (src);
	  cst.normalize_symbolics ();
	  set (cst.min (), cst.max ());
	  return;
	}
      set (src.min (), src.max ());
    }
}

// Swap min/max if they are out of order.  Return TRUE if further
// processing of the range is necessary, FALSE otherwise.

bool
irange::swap_out_of_order_endpoints (tree &min, tree &max,
					  value_range_kind &kind)
{
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
	  return false;
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
	  return false;
	}
      kind = kind == VR_RANGE ? VR_ANTI_RANGE : VR_RANGE;
    }
  return true;
}

void
irange::irange_set (tree min, tree max)
{
  gcc_checking_assert (!POLY_INT_CST_P (min));
  gcc_checking_assert (!POLY_INT_CST_P (max));

  m_base[0] = min;
  m_base[1] = max;
  m_num_ranges = 1;
  if (flag_checking)
    verify_range ();
}

void
irange::irange_set_anti_range (tree min, tree max)
{
  gcc_checking_assert (!POLY_INT_CST_P (min));
  gcc_checking_assert (!POLY_INT_CST_P (max));

  // set an anti-range
  tree type = TREE_TYPE (min);
  signop sign = TYPE_SIGN (type);
  int_range<2> type_range (type);
  // Calculate INVERSE([I,J]) as [-MIN, I-1][J+1, +MAX].
  m_num_ranges = 0;
  wi::overflow_type ovf;

  wide_int w_min = wi::to_wide (min);
  if (wi::ne_p (w_min, type_range.lower_bound ()))
    {
      wide_int lim1 = wi::sub (w_min, 1, sign, &ovf);
      gcc_checking_assert (ovf != wi::OVF_OVERFLOW);
      m_base[0] = type_range.tree_lower_bound (0);
      m_base[1] = wide_int_to_tree (type, lim1);
      m_num_ranges = 1;
    }
  wide_int w_max = wi::to_wide (max);
  if (wi::ne_p (w_max, type_range.upper_bound ()))
    {
      wide_int lim2 = wi::add (w_max, 1, sign, &ovf);
      gcc_checking_assert (ovf != wi::OVF_OVERFLOW);
      m_base[m_num_ranges * 2] = wide_int_to_tree (type, lim2);
      m_base[m_num_ranges * 2 + 1] = type_range.tree_upper_bound (0);
      ++m_num_ranges;
    }
  if (flag_checking)
    verify_range ();
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
irange::set (tree min, tree max, value_range_kind kind)
{
  if (!legacy_mode_p ())
    {
      if (kind == VR_RANGE)
	irange_set (min, max);
      else
	{
	  gcc_checking_assert (kind == VR_ANTI_RANGE);
	  irange_set_anti_range (min, max);
	}
      return;
    }
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
      set_varying (TREE_TYPE (min));
      return;
    }

  tree type = TREE_TYPE (min);
  // Nothing to canonicalize for symbolic ranges.
  if (TREE_CODE (min) != INTEGER_CST
      || TREE_CODE (max) != INTEGER_CST)
    {
      m_kind = kind;
      m_base[0] = min;
      m_base[1] = max;
      m_num_ranges = 1;
      return;
    }
  if (!swap_out_of_order_endpoints (min, max, kind))
    goto cleanup_set;

  // Anti-ranges that can be represented as ranges should be so.
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
  else if (!swap_out_of_order_endpoints (min, max, kind))
    goto cleanup_set;

  /* Do not drop [-INF(OVF), +INF(OVF)] to varying.  (OVF) has to be sticky
     to make sure VRP iteration terminates, otherwise we can get into
     oscillations.  */
  if (!normalize_min_max (type, min, max, kind))
    {
      m_kind = kind;
      m_base[0] = min;
      m_base[1] = max;
      m_num_ranges = 1;
      if (flag_checking)
	verify_range ();
    }

 cleanup_set:
  // Avoid using TYPE_{MIN,MAX}_VALUE because -fstrict-enums can
  // restrict those to a subset of what actually fits in the type.
  // Instead use the extremes of the type precision
  unsigned prec = TYPE_PRECISION (type);
  signop sign = TYPE_SIGN (type);
  if (wi::eq_p (wi::to_wide (min), wi::min_value (prec, sign))
      && wi::eq_p (wi::to_wide (max), wi::max_value (prec, sign)))
    m_kind = VR_VARYING;
  else if (undefined_p ())
    m_kind = VR_UNDEFINED;
  if (flag_checking)
    verify_range ();
}

/* Check the validity of the range.  */

void
irange::verify_range ()
{
  if (!legacy_mode_p ())
    {
      gcc_checking_assert (m_kind == VR_RANGE);
      for (unsigned i = 0; i < m_num_ranges; ++i)
	{
	  tree lb = tree_lower_bound (i);
	  tree ub = tree_upper_bound (i);
	  int c = compare_values (lb, ub);
	  gcc_assert (c == 0 || c == -1);
	}
      return;
    }

  switch (m_kind)
    {
    case VR_UNDEFINED:
      gcc_assert (m_num_ranges == 0);
      break;

    case VR_VARYING:
      gcc_assert (m_num_ranges == 1);
      break;

    case VR_ANTI_RANGE:
    case VR_RANGE:
      {
	gcc_assert (m_num_ranges == 1);
	int cmp = compare_values (tree_lower_bound (0), tree_upper_bound (0));
	gcc_assert (cmp == 0 || cmp == -1 || cmp == -2);
	return;
      }

    default:
      gcc_unreachable ();
    }
}

unsigned
irange::legacy_num_pairs () const
{
  gcc_checking_assert (legacy_mode_p ());

  if (undefined_p ())
    return 0;
  if (varying_p ())
    return 1;
  // Inlined symbolic_p for performance:
  if (!is_gimple_min_invariant (min ()) || !is_gimple_min_invariant (max ()))
    {
      value_range numeric_range (*this);
      numeric_range.normalize_symbolics ();
      return numeric_range.num_pairs ();
    }
  if (m_kind == VR_ANTI_RANGE)
    {
      // ~[MIN, X] has one sub-range of [X+1, MAX], and
      // ~[X, MAX] has one sub-range of [MIN, X-1].
      if (vrp_val_is_min (min ()) || vrp_val_is_max (max ()))
	return 1;
      return 2;
    }
  gcc_checking_assert (m_num_ranges == 1);
  return 1;
}

// Return the lower bound for a sub-range.  PAIR is the sub-range in
// question.

wide_int
irange::legacy_lower_bound (unsigned pair) const
{
  gcc_checking_assert (legacy_mode_p ());
  if (symbolic_p ())
    {
      value_range numeric_range (*this);
      numeric_range.normalize_symbolics ();
      return numeric_range.legacy_lower_bound (pair);
    }
  gcc_checking_assert (!undefined_p ());
  gcc_checking_assert (pair + 1 <= num_pairs ());
  if (m_kind == VR_ANTI_RANGE)
    {
      tree typ = type (), t;
      if (pair == 1 || vrp_val_is_min (min ()))
	t = wide_int_to_tree (typ, wi::to_wide (max ()) + 1);
      else
	t = vrp_val_min (typ);
      return wi::to_wide (t);
    }
 return wi::to_wide (tree_lower_bound (pair));
}

// Return the upper bound for a sub-range.  PAIR is the sub-range in
// question.

wide_int
irange::legacy_upper_bound (unsigned pair) const
{
  gcc_checking_assert (legacy_mode_p ());
  if (symbolic_p ())
    {
      value_range numeric_range (*this);
      numeric_range.normalize_symbolics ();
      return numeric_range.legacy_upper_bound (pair);
    }
  gcc_checking_assert (!undefined_p ());
  gcc_checking_assert (pair + 1 <= num_pairs ());
  if (m_kind == VR_ANTI_RANGE)
    {
      tree typ = type (), t;
      if (pair == 1 || vrp_val_is_min (min ()))
	t = vrp_val_max (typ);
      else
	t = wide_int_to_tree (typ, wi::to_wide (min ()) - 1);
      return wi::to_wide (t);
    }
  return wi::to_wide (tree_upper_bound (pair));
}

bool
irange::legacy_equal_p (const irange &other) const
{
  gcc_checking_assert (legacy_mode_p () && other.legacy_mode_p ());

  if (m_kind != other.m_kind)
   return false;
  if (m_kind == VR_UNDEFINED || m_kind == VR_VARYING)
    return true;
  return (vrp_operand_equal_p (tree_lower_bound (0),
			       other.tree_lower_bound (0))
	  && vrp_operand_equal_p (tree_upper_bound (0),
				  other.tree_upper_bound (0)));
}

bool
irange::equal_p (const irange &other) const
{
  if (legacy_mode_p ())
    {
      if (other.legacy_mode_p ())
	return legacy_equal_p (other);
      value_range tmp (other);
      return legacy_equal_p (tmp);
    }
  if (other.legacy_mode_p ())
    {
      value_range tmp2 (*this);
      return tmp2.legacy_equal_p (other);
    }

  if (m_num_ranges != other.m_num_ranges)
    return false;

  for (unsigned i = 0; i < m_num_ranges; ++i)
    {
      tree lb = tree_lower_bound (i);
      tree ub = tree_upper_bound (i);
      tree lb_other = other.tree_lower_bound (i);
      tree ub_other = other.tree_upper_bound (i);
      if (!operand_equal_p (lb, lb_other, 0)
	  || !operand_equal_p (ub, ub_other, 0))
	return false;
    }
  return true;
}

/* Return TRUE if this is a symbolic range.  */

bool
irange::symbolic_p () const
{
  return (!varying_p ()
	  && !undefined_p ()
	  && (!is_gimple_min_invariant (min ())
	      || !is_gimple_min_invariant (max ())));
}

/* NOTE: This is not the inverse of symbolic_p because the range
   could also be varying or undefined.  Ideally they should be inverse
   of each other, with varying only applying to symbolics.  Varying of
   constants would be represented as [-MIN, +MAX].  */

bool
irange::constant_p () const
{
  return (!varying_p ()
	  && !undefined_p ()
	  && TREE_CODE (min ()) == INTEGER_CST
	  && TREE_CODE (max ()) == INTEGER_CST);
}

/* If range is a singleton, place it in RESULT and return TRUE.
   Note: A singleton can be any gimple invariant, not just constants.
   So, [&x, &x] counts as a singleton.  */

bool
irange::singleton_p (tree *result) const
{
  if (!legacy_mode_p ())
    {
      if (num_pairs () == 1 && (wi::to_wide (tree_lower_bound ())
				== wi::to_wide (tree_upper_bound ())))
	{
	  if (result)
	    *result = tree_lower_bound ();
	  return true;
	}
      return false;
    }
  if (m_kind == VR_ANTI_RANGE)
    {
      if (nonzero_p ())
	{
	  if (TYPE_PRECISION (type ()) == 1)
	    {
	      if (result)
		*result = max ();
	      return true;
	    }
	  return false;
	}
      if (num_pairs () == 1)
	{
	  value_range vr0, vr1;
	  ranges_from_anti_range ((const value_range *) this, &vr0, &vr1);
	  return vr0.singleton_p (result);
	}
    }
  // Catches non-numeric extremes as well.
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
irange::value_inside_range (tree val) const
{
  if (varying_p ())
    return 1;

  if (undefined_p ())
    return 0;

  if (!legacy_mode_p () && TREE_CODE (val) == INTEGER_CST)
    return contains_p (val);

  int cmp1 = operand_less_p (val, min ());
  if (cmp1 == -2)
    return -2;
  if (cmp1 == 1)
    return m_kind != VR_RANGE;

  int cmp2 = operand_less_p (max (), val);
  if (cmp2 == -2)
    return -2;

  if (m_kind == VR_RANGE)
    return !cmp2;
  else
    return !!cmp2;
}

/* Return TRUE if it is possible that range contains VAL.  */

bool
irange::may_contain_p (tree val) const
{
  return value_inside_range (val) != 0;
}

/* Return TRUE if range contains INTEGER_CST.  */
/* Return 1 if VAL is inside value range.
	  0 if VAL is not inside value range.

   Benchmark compile/20001226-1.c compilation time after changing this
   function.  */


bool
irange::contains_p (tree cst) const
{
  if (undefined_p ())
    return false;

  if (legacy_mode_p ())
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

  gcc_checking_assert (TREE_CODE (cst) == INTEGER_CST);
  signop sign = TYPE_SIGN (TREE_TYPE (cst));
  wide_int v = wi::to_wide (cst);
  for (unsigned r = 0; r < m_num_ranges; ++r)
    {
      if (wi::lt_p (v, lower_bound (r), sign))
	return false;
      if (wi::le_p (v, upper_bound (r), sign))
	return true;
    }

  return false;
}


/* Normalize addresses into constants.  */

void
irange::normalize_addresses ()
{
  if (undefined_p ())
    return;

  if (!POINTER_TYPE_P (type ()) || range_has_numeric_bounds_p (this))
    return;

  if (!range_includes_zero_p (this))
    {
      gcc_checking_assert (TREE_CODE (min ()) == ADDR_EXPR
			   || TREE_CODE (max ()) == ADDR_EXPR);
      set_nonzero (type ());
      return;
    }
  set_varying (type ());
}

/* Normalize symbolics and addresses into constants.  */

void
irange::normalize_symbolics ()
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
   ranges VR0 and VR1, set VR0 to the intersection of both ranges.
   This may not be the smallest possible such range.  */

void
irange::legacy_intersect (irange *vr0, const irange *vr1)
{
  /* If either range is VR_VARYING the other one wins.  */
  if (vr1->varying_p ())
    return;
  if (vr0->varying_p ())
    {
      /* Avoid the full copy if we already know both sides are simple
	 and can be trivially copied.  */
      if (vr1->legacy_mode_p ())
	{
	  vr0->set (vr1->min (), vr1->max (), vr1->kind ());
	  return;
	}
      *vr0 = *vr1;
      return;
    }

  /* When either range is VR_UNDEFINED the resulting range is
     VR_UNDEFINED, too.  */
  if (vr0->undefined_p ())
    return;
  if (vr1->undefined_p ())
    {
      vr0->set_undefined ();
      return;
    }

  value_range_kind vr0kind = vr0->kind ();
  tree vr0min = vr0->min ();
  tree vr0max = vr0->max ();
  /* Handle multi-ranges that can be represented as anti-ranges.  */
  if (!vr1->legacy_mode_p () && vr1->maybe_anti_range ())
    {
      int_range<3> tmp (*vr1);
      tmp.invert ();
      intersect_ranges (&vr0kind, &vr0min, &vr0max,
			VR_ANTI_RANGE, tmp.min (), tmp.max ());
    }
  else
    intersect_ranges (&vr0kind, &vr0min, &vr0max,
		      vr1->kind (), vr1->min (), vr1->max ());

  /* Make sure to canonicalize the result though as the inversion of a
     VR_RANGE can still be a VR_RANGE.  */
  if (vr0kind == VR_UNDEFINED)
    vr0->set_undefined ();
  else if (vr0kind == VR_VARYING)
    {
      /* If we failed, use the original VR0.  */
      return;
    }
  else
    vr0->set (vr0min, vr0max, vr0kind);
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

/* Helper for meet operation for value ranges.  Given two ranges VR0
   and VR1, set VR0 to the union of both ranges.  This may not be the
   smallest possible such range.  */

void
irange::legacy_union (irange *vr0, const irange *vr1)
{
  /* VR0 has the resulting range if VR1 is undefined or VR0 is varying.  */
  if (vr1->undefined_p ()
      || vr0->varying_p ())
    return;

  /* VR1 has the resulting range if VR0 is undefined or VR1 is varying.  */
  if (vr0->undefined_p ())
    {
      /* Avoid the full copy if we already know both sides are simple
	 and can be trivially copied.  */
      if (vr1->legacy_mode_p ())
	{
	  vr0->set (vr1->min (), vr1->max (), vr1->kind ());
	  return;
	}
      *vr0 = *vr1;
      return;
    }
  if (vr1->varying_p ())
    {
      vr0->set_varying (vr1->type ());
      return;
    }

  value_range_kind vr0kind = vr0->kind ();
  tree vr0min = vr0->min ();
  tree vr0max = vr0->max ();
  /* Handle multi-ranges that can be represented as anti-ranges.  */
  if (!vr1->legacy_mode_p () && vr1->maybe_anti_range ())
    {
      int_range<3> tmp (*vr1);
      tmp.invert ();
      union_ranges (&vr0kind, &vr0min, &vr0max,
		    VR_ANTI_RANGE, tmp.min (), tmp.max ());
    }
  else
    union_ranges (&vr0kind, &vr0min, &vr0max,
		  vr1->kind (), vr1->min (), vr1->max ());

  if (vr0kind == VR_UNDEFINED)
    vr0->set_undefined ();
  else if (vr0kind == VR_VARYING)
    {
      /* Failed to find an efficient meet.  Before giving up and
	 setting the result to VARYING, see if we can at least derive
	 a non-zero range.  */
      if (range_includes_zero_p (vr0) == 0
	  && range_includes_zero_p (vr1) == 0)
	vr0->set_nonzero (vr0->type ());
      else
	vr0->set_varying (vr0->type ());
    }
  else
    vr0->set (vr0min, vr0max, vr0kind);
}

/* Meet operation for value ranges.  Given two value ranges VR0 and
   VR1, store in VR0 a range that contains both VR0 and VR1.  This
   may not be the smallest possible such range.  */

void
irange::union_ (const irange *other)
{
  if (legacy_mode_p ())
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Meeting\n  ");
	  dump_value_range (dump_file, this);
	  fprintf (dump_file, "\nand\n  ");
	  dump_value_range (dump_file, other);
	  fprintf (dump_file, "\n");
	}

      legacy_union (this, other);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "to\n  ");
	  dump_value_range (dump_file, this);
	  fprintf (dump_file, "\n");
	}
      return;
    }

  if (other->legacy_mode_p ())
    {
      int_range<2> wider;
      wider = *other;
      irange_union (wider);
    }
  else
    irange_union (*other);
}

void
irange::intersect (const irange *other)
{
  if (legacy_mode_p ())
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Intersecting\n  ");
	  dump_value_range (dump_file, this);
	  fprintf (dump_file, "\nand\n  ");
	  dump_value_range (dump_file, other);
	  fprintf (dump_file, "\n");
	}

      legacy_intersect (this, other);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "to\n  ");
	  dump_value_range (dump_file, this);
	  fprintf (dump_file, "\n");
	}
      return;
    }

  if (other->legacy_mode_p ())
    {
      int_range<2> wider;
      wider = *other;
      irange_intersect (wider);
    }
  else
    irange_intersect (*other);
}

// union_ for multi-ranges.

void
irange::irange_union (const irange &r)
{
  gcc_checking_assert (!legacy_mode_p () && !r.legacy_mode_p ());

  if (r.undefined_p () || varying_p ())
    return;

  if (undefined_p () || r.varying_p ())
    {
      operator= (r);
      return;
    }

  // Do not worry about merging and such by reserving twice as many
  // pairs as needed, and then simply sort the 2 ranges into this
  // intermediate form.
  //
  // The intermediate result will have the property that the beginning
  // of each range is <= the beginning of the next range.  There may
  // be overlapping ranges at this point.  I.e. this would be valid
  // [-20, 10], [-10, 0], [0, 20], [40, 90] as it satisfies this
  // contraint : -20 < -10 < 0 < 40.  When the range is rebuilt into r,
  // the merge is performed.
  //
  // [Xi,Yi]..[Xn,Yn]  U  [Xj,Yj]..[Xm,Ym]   -->  [Xk,Yk]..[Xp,Yp]
  tree ttype = r.type ();
  signop sign = TYPE_SIGN (ttype);

  auto_vec<tree, 20> res;
  wide_int u1 ;
  wi::overflow_type ovf;
  unsigned i = 0, j = 0, k = 0;

  while (i < m_num_ranges * 2 && j < r.m_num_ranges * 2)
    {
      // lower of Xi and Xj is the lowest point.
      if (wi::le_p (wi::to_wide (m_base[i]), wi::to_wide (r.m_base[j]), sign))
	{
	  res.safe_push (m_base[i]);
	  res.safe_push (m_base[i + 1]);
	  k += 2;
	  i += 2;
	}
      else
	{
	  res.safe_push (r.m_base[j]);
	  res.safe_push (r.m_base[j + 1]);
	  k += 2;
	  j += 2;
	}
    }
  for ( ; i < m_num_ranges * 2; i += 2)
    {
      res.safe_push (m_base[i]);
      res.safe_push (m_base[i + 1]);
      k += 2;
    }
  for ( ; j < r.m_num_ranges * 2; j += 2)
    {
      res.safe_push (r.m_base[j]);
      res.safe_push (r.m_base[j + 1]);
      k += 2;
    }

  // Now normalize the vector removing any overlaps.
  i = 2;
  int prec = TYPE_PRECISION (ttype);
  wide_int max_val = wi::max_value (prec, sign);
  for (j = 2; j < k ; j += 2)
    {
      wide_int val_im1 = wi::to_wide (res[i - 1]);
      if (val_im1 == max_val)
	break;
      u1 = wi::add (val_im1, 1, sign, &ovf);

      // Overflow indicates we are at MAX already.
      // A wide int bug requires the previous max_val check
      // trigger: gcc.c-torture/compile/pr80443.c  with -O3
      if (ovf == wi::OVF_OVERFLOW)
	break;

      wide_int val_j = wi::to_wide (res[j]);
      wide_int val_jp1 = wi::to_wide (res[j+1]);
      // Current upper+1 is >= lower bound next pair, then we merge ranges.
      if (wi::ge_p (u1, val_j, sign))
	{
	  // New upper bounds is greater of current or the next one.
	  if (wi::gt_p (val_jp1, val_im1, sign))
	    res [i - 1] = res[j + 1];
	}
      else
	{
	  // This is a new distinct range, but no point in copying it
	  // if it is already in the right place.
	  if (i != j)
	    {
	      res[i++] = res[j];
	      res[i++] = res[j + 1];
	    }
	  else
	    i += 2;
	}
    }

  // At this point, the vector should have i ranges, none overlapping.
  // Now it simply needs to be copied, and if there are too many
  // ranges, merge some.  We wont do any analysis as to what the
  // "best" merges are, simply combine the final ranges into one.
  if (i > m_max_ranges * 2)
    {
      res[m_max_ranges * 2 - 1] = res[i - 1];
      i = m_max_ranges * 2;
    }

  for (j = 0; j < i ; j++)
    m_base[j] = res [j];
  m_num_ranges = i / 2;

  if (flag_checking)
    verify_range ();
}

// intersect for multi-ranges.

void
irange::irange_intersect (const irange &r)
{
  gcc_checking_assert (!legacy_mode_p () && !r.legacy_mode_p ());

  if (undefined_p () || r.varying_p ())
    return;
  if (r.undefined_p ())
    {
      set_undefined ();
      return;
    }
  if (varying_p ())
    {
      operator= (r);
      return;
    }

  signop sign = TYPE_SIGN (TREE_TYPE(m_base[0]));
  unsigned bld_pair = 0;
  unsigned bld_lim = m_max_ranges;
  int_range_max r2 (*this);
  unsigned r2_lim = r2.num_pairs ();
  unsigned i2 = 0;
  for (unsigned i = 0; i < r.num_pairs (); )
    {
      // If r1's upper is < r2's lower, we can skip r1's pair.
      tree ru = r.m_base[i * 2 + 1];
      tree r2l = r2.m_base[i2 * 2];
      if (wi::lt_p (wi::to_wide (ru), wi::to_wide (r2l), sign))
	{
	  i++;
	  continue;
	}
      // Likewise, skip r2's pair if its excluded.
      tree r2u = r2.m_base[i2 * 2 + 1];
      tree rl = r.m_base[i * 2];
      if (wi::lt_p (wi::to_wide (r2u), wi::to_wide (rl), sign))
	{
	  i2++;
	  if (i2 < r2_lim)
	    continue;
	  // No more r2, break.
	  break;
	}

      // Must be some overlap.  Find the highest of the lower bounds,
      // and set it, unless the build limits lower bounds is already
      // set.
      if (bld_pair < bld_lim)
	{
	  if (wi::ge_p (wi::to_wide (rl), wi::to_wide (r2l), sign))
	    m_base[bld_pair * 2] = rl;
	  else
	    m_base[bld_pair * 2] = r2l;
	}
      else
	// Decrease and set a new upper.
	bld_pair--;

      // ...and choose the lower of the upper bounds.
      if (wi::le_p (wi::to_wide (ru), wi::to_wide (r2u), sign))
	{
	  m_base[bld_pair * 2 + 1] = ru;
	  bld_pair++;
	  // Move past the r1 pair and keep trying.
	  i++;
	  continue;
	}
      else
	{
	  m_base[bld_pair * 2 + 1] = r2u;
	  bld_pair++;
	  i2++;
	  if (i2 < r2_lim)
	    continue;
	  // No more r2, break.
	  break;
	}
      // r2 has the higher lower bound.
    }

  // At the exit of this loop, it is one of 2 things:
  // ran out of r1, or r2, but either means we are done.
  m_num_ranges = bld_pair;
  if (flag_checking)
    verify_range ();
}

static wide_int inline
subtract_one (const wide_int &x, tree type, wi::overflow_type &overflow)
{
  // A signed 1-bit bit-field, has a range of [-1,0] so subtracting +1
  // overflows, since +1 is unrepresentable.  This is why we have an
  // addition of -1 here.
  if (TYPE_SIGN (type) == SIGNED)
    return wi::add (x, -1 , SIGNED, &overflow);
  else
    return wi::sub (x, 1, UNSIGNED, &overflow);
}

/* Return the inverse of a range.  */

void
irange::invert ()
{
  if (legacy_mode_p ())
    {
      // We can't just invert VR_RANGE and VR_ANTI_RANGE because we may
      // create non-canonical ranges.  Use the constructors instead.
      if (m_kind == VR_RANGE)
	*this = value_range (min (), max (), VR_ANTI_RANGE);
      else if (m_kind == VR_ANTI_RANGE)
	*this = value_range (min (), max ());
      else
	gcc_unreachable ();
      return;
    }

  gcc_assert (!undefined_p () && !varying_p ());

  // We always need one more set of bounds to represent an inverse, so
  // if we're at the limit, we can't properly represent things.
  //
  // For instance, to represent the inverse of a 2 sub-range set
  // [5, 10][20, 30], we would need a 3 sub-range set
  // [-MIN, 4][11, 19][31, MAX].
  //
  // In this case, return the most conservative thing.
  //
  // However, if any of the extremes of the range are -MIN/+MAX, we
  // know we will not need an extra bound.  For example:
  //
  // 	INVERT([-MIN,20][30,40]) => [21,29][41,+MAX]
  // 	INVERT([-MIN,20][30,MAX]) => [21,29]
  tree ttype = type ();
  unsigned prec = TYPE_PRECISION (ttype);
  signop sign = TYPE_SIGN (ttype);
  wide_int type_min = wi::min_value (prec, sign);
  wide_int type_max = wi::max_value (prec, sign);
  if (m_num_ranges == m_max_ranges
      && lower_bound () != type_min
      && upper_bound () != type_max)
    {
      m_base[1] = wide_int_to_tree (ttype, type_max);
      m_num_ranges = 1;
      return;
    }
  // The algorithm is as follows.  To calculate INVERT ([a,b][c,d]), we
  // generate [-MIN, a-1][b+1, c-1][d+1, MAX].
  //
  // If there is an over/underflow in the calculation for any
  // sub-range, we eliminate that subrange.  This allows us to easily
  // calculate INVERT([-MIN, 5]) with: [-MIN, -MIN-1][6, MAX].  And since
  // we eliminate the underflow, only [6, MAX] remains.
  unsigned i = 0;
  wi::overflow_type ovf;
  // Construct leftmost range.
  int_range_max orig_range (*this);
  unsigned nitems = 0;
  wide_int tmp;
  // If this is going to underflow on the MINUS 1, don't even bother
  // checking.  This also handles subtracting one from an unsigned 0,
  // which doesn't set the underflow bit.
  if (type_min != orig_range.lower_bound ())
    {
      m_base[nitems++] = wide_int_to_tree (ttype, type_min);
      tmp = subtract_one (orig_range.lower_bound (), ttype, ovf);
      m_base[nitems++] = wide_int_to_tree (ttype, tmp);
      if (ovf)
	nitems = 0;
    }
  i++;
  // Construct middle ranges if applicable.
  if (orig_range.num_pairs () > 1)
    {
      unsigned j = i;
      for (; j < (orig_range.num_pairs () * 2) - 1; j += 2)
	{
	  // The middle ranges cannot have MAX/MIN, so there's no need
	  // to check for unsigned overflow on the +1 and -1 here.
	  tmp = wi::add (wi::to_wide (orig_range.m_base[j]), 1, sign, &ovf);
	  m_base[nitems++] = wide_int_to_tree (ttype, tmp);
	  tmp = subtract_one (wi::to_wide (orig_range.m_base[j + 1]),
			      ttype, ovf);
	  m_base[nitems++] = wide_int_to_tree (ttype, tmp);
	  if (ovf)
	    nitems -= 2;
	}
      i = j;
    }
  // Construct rightmost range.
  //
  // However, if this will overflow on the PLUS 1, don't even bother.
  // This also handles adding one to an unsigned MAX, which doesn't
  // set the overflow bit.
  if (type_max != wi::to_wide (orig_range.m_base[i]))
    {
      tmp = wi::add (wi::to_wide (orig_range.m_base[i]), 1, sign, &ovf);
      m_base[nitems++] = wide_int_to_tree (ttype, tmp);
      m_base[nitems++] = wide_int_to_tree (ttype, type_max);
      if (ovf)
	nitems -= 2;
    }
  m_num_ranges = nitems / 2;

  if (flag_checking)
    verify_range ();
}

static void
dump_bound_with_infinite_markers (FILE *file, tree bound)
{
  tree type = TREE_TYPE (bound);
  if (INTEGRAL_TYPE_P (type)
      && !TYPE_UNSIGNED (type)
      && vrp_val_is_min (bound)
      && TYPE_PRECISION (type) != 1)
    fprintf (file, "-INF");
  else if (vrp_val_is_max (bound)
	   && TYPE_PRECISION (type) != 1)
    fprintf (file, "+INF");
  else
    print_generic_expr (file, bound);
}

void
irange::dump (FILE *file) const
{
  if (undefined_p ())
    {
      fprintf (file, "UNDEFINED");
      return;
    }
  print_generic_expr (file, type ());
  fprintf (file, " ");
  if (varying_p ())
    {
      fprintf (file, "VARYING");
      return;
    }
 if (legacy_mode_p ())
    {
      fprintf (file, "%s[", (m_kind == VR_ANTI_RANGE) ? "~" : "");
      dump_bound_with_infinite_markers (file, min ());
      fprintf (file, ", ");
      dump_bound_with_infinite_markers (file, max ());
      fprintf (file, "]");
      return;
    }
  for (unsigned i = 0; i < m_num_ranges; ++i)
    {
      tree lb = m_base[i * 2];
      tree ub = m_base[i * 2 + 1];
      fprintf (file, "[");
      dump_bound_with_infinite_markers (file, lb);
      fprintf (file, ", ");
      dump_bound_with_infinite_markers (file, ub);
      fprintf (file, "]");
    }
}

void
dump_value_range (FILE *file, const irange *vr)
{
  vr->dump (file);
}

DEBUG_FUNCTION void
debug (const irange *vr)
{
  dump_value_range (stderr, vr);
  fprintf (stderr, "\n");
}

DEBUG_FUNCTION void
debug (const irange &vr)
{
  debug (&vr);
}

DEBUG_FUNCTION void
debug (const value_range *vr)
{
  dump_value_range (stderr, vr);
  fprintf (stderr, "\n");
}

DEBUG_FUNCTION void
debug (const value_range &vr)
{
  dump_value_range (stderr, &vr);
  fprintf (stderr, "\n");
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
range_has_numeric_bounds_p (const irange *vr)
{
  return (!vr->undefined_p ()
	  && TREE_CODE (vr->min ()) == INTEGER_CST
	  && TREE_CODE (vr->max ()) == INTEGER_CST);
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

#define DEFINE_INT_RANGE_GC_STUBS(N)		\
  void						\
  gt_pch_nx (int_range<N> *&x)			\
  {						\
    for (unsigned i = 0; i < N; ++i)		\
      {						\
	gt_pch_nx (x->m_ranges[i * 2]);		\
	gt_pch_nx (x->m_ranges[i * 2 + 1]);	\
      }		  		       		\
  }						\
						\
  void						\
  gt_ggc_mx (int_range<N> *&x)			\
  {	    	       				\
    for (unsigned i = 0; i < N; ++i)		\
      {						\
	  gt_ggc_mx (x->m_ranges[i * 2]);	\
	  gt_ggc_mx (x->m_ranges[i * 2 + 1]);	\
      }						\
  }

#define DEFINE_INT_RANGE_INSTANCE(N)					\
  template int_range<N>::int_range(tree, tree, value_range_kind);	\
  template int_range<N>::int_range(tree_node *,				\
				   const wide_int &,			\
				   const wide_int &,			\
				   value_range_kind);			\
  template int_range<N>::int_range(tree);				\
  template int_range<N>::int_range(const irange &);		\
  template int_range<N>::int_range(const int_range &);			\
  template int_range<N>& int_range<N>::operator= (const int_range &);

DEFINE_INT_RANGE_INSTANCE(1)
DEFINE_INT_RANGE_INSTANCE(2)
DEFINE_INT_RANGE_INSTANCE(3)
DEFINE_INT_RANGE_INSTANCE(255)
DEFINE_INT_RANGE_GC_STUBS(1)
