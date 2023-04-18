/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2005-2023 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>.

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
#include "basic-block.h"
#include "bitmap.h"
#include "sbitmap.h"
#include "options.h"
#include "dominance.h"
#include "function.h"
#include "cfg.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "cfganal.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-into-ssa.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-propagate.h"
#include "domwalk.h"
#include "vr-values.h"
#include "gimple-array-bounds.h"
#include "gimple-range.h"
#include "gimple-range-path.h"
#include "value-pointer-equiv.h"
#include "gimple-fold.h"
#include "tree-dfa.h"
#include "tree-ssa-dce.h"

// This class is utilized by VRP and ranger to remove __builtin_unreachable
// calls, and reflect any resulting global ranges.
//
// maybe_register_block () is called on basic blocks, and if that block
// matches the pattern of one branch being a builtin_unreachable, register
// the resulting executable edge in a list.
//
// After all blocks have been processed, remove_and_update_globals() will
// - check all exports from registered blocks
// - ensure the cache entry of each export is set with the appropriate range
// - rewrite the conditions to take the executable edge
// - perform DCE on any feeding instructions to those rewritten conditions
//
// Then each of the immediate use chain of each export is walked, and a new
// global range created by unioning the ranges at all remaining use locations.

class remove_unreachable {
public:
  remove_unreachable (gimple_ranger &r) : m_ranger (r) { m_list.create (30); }
  ~remove_unreachable () { m_list.release (); }
  void maybe_register_block (basic_block bb);
  bool remove_and_update_globals (bool final_p);
  vec<std::pair<int, int> > m_list;
  gimple_ranger &m_ranger;
};

// Check if block BB has a __builtin_unreachable () call on one arm, and
// register the executable edge if so.

void
remove_unreachable::maybe_register_block (basic_block bb)
{
  gimple *s = gimple_outgoing_range_stmt_p (bb);
  if (!s || gimple_code (s) != GIMPLE_COND)
    return;

  edge e0 = EDGE_SUCC (bb, 0);
  basic_block bb0 = e0->dest;
  bool un0 = EDGE_COUNT (bb0->succs) == 0
	     && gimple_seq_unreachable_p (bb_seq (bb0));
  edge e1 = EDGE_SUCC (bb, 1);
  basic_block bb1 = e1->dest;
  bool un1 = EDGE_COUNT (bb1->succs) == 0
	     && gimple_seq_unreachable_p (bb_seq (bb1));

  // If the 2 blocks are not different, ignore.
  if (un0 == un1)
    return;

  if (un0)
    m_list.safe_push (std::make_pair (e1->src->index, e1->dest->index));
  else
    m_list.safe_push (std::make_pair (e0->src->index, e0->dest->index));
}

// Process the edges in the list, change the conditions and removing any
// dead code feeding those conditions.  Calculate the range of any
// names that may have been exported from those blocks, and determine if
// there is any updates to their global ranges..
// FINAL_P indicates all builtin_unreachable calls should be removed.
// Return true if any builtin_unreachables/globals eliminated/updated.

bool
remove_unreachable::remove_and_update_globals (bool final_p)
{
  if (m_list.length () == 0)
    return false;

  // Ensure the cache in SCEV has been cleared before processing
  // globals to be removed.
  scev_reset ();

  bool change = false;
  tree name;
  unsigned i;
  bitmap_iterator bi;
  auto_bitmap all_exports;
  for (i = 0; i < m_list.length (); i++)
    {
      auto eb = m_list[i];
      basic_block src = BASIC_BLOCK_FOR_FN (cfun, eb.first);
      basic_block dest = BASIC_BLOCK_FOR_FN (cfun, eb.second);
      if (!src || !dest)
	continue;
      edge e = find_edge (src, dest);
      gimple *s = gimple_outgoing_range_stmt_p (e->src);
      gcc_checking_assert (gimple_code (s) == GIMPLE_COND);
      bool lhs_p = TREE_CODE (gimple_cond_lhs (s)) == SSA_NAME;
      bool rhs_p = TREE_CODE (gimple_cond_rhs (s)) == SSA_NAME;
      // Do not remove __builtin_unreachable if it confers a relation, or
      // that relation will be lost in subsequent passes.  Unless its the
      // final pass.
      if (!final_p && lhs_p && rhs_p)
	continue;
      // If this is already a constant condition, don't look either
      if (!lhs_p && !rhs_p)
	continue;

      bool dominate_exit_p = true;
      FOR_EACH_GORI_EXPORT_NAME (m_ranger.gori (), e->src, name)
	{
	  // Ensure the cache is set for NAME in the succ block.
	  Value_Range r(TREE_TYPE (name));
	  Value_Range ex(TREE_TYPE (name));
	  m_ranger.range_on_entry (r, e->dest, name);
	  m_ranger.range_on_entry (ex, EXIT_BLOCK_PTR_FOR_FN (cfun), name);
	  // If the range produced by this __builtin_unreachacble expression
	  // is not fully reflected in the range at exit, then it does not
	  // dominate the exit of the function.
	  if (ex.intersect (r))
	    dominate_exit_p = false;
	}

      // If the exit is dominated, add to the export list.  Otherwise if this
      // isn't the final VRP pass, leave the call in the IL.
      if (dominate_exit_p)
	bitmap_ior_into (all_exports, m_ranger.gori ().exports (e->src));
      else if (!final_p)
	continue;

      change = true;
      // Rewrite the condition.
      if (e->flags & EDGE_TRUE_VALUE)
	gimple_cond_make_true (as_a<gcond *> (s));
      else
	gimple_cond_make_false (as_a<gcond *> (s));
      update_stmt (s);
    }

  if (bitmap_empty_p (all_exports))
    return false;
  // Invoke DCE on all exported names to eliminate dead feeding defs.
  auto_bitmap dce;
  bitmap_copy (dce, all_exports);
  // Don't attempt to DCE parameters.
  EXECUTE_IF_SET_IN_BITMAP (all_exports, 0, i, bi)
    if (!ssa_name (i) || SSA_NAME_IS_DEFAULT_DEF (ssa_name (i)))
      bitmap_clear_bit (dce, i);
  simple_dce_from_worklist (dce);

  // Loop over all uses of each name and find maximal range. This is the
  // new global range.
  use_operand_p use_p;
  imm_use_iterator iter;
  EXECUTE_IF_SET_IN_BITMAP (all_exports, 0, i, bi)
    {
      name = ssa_name (i);
      if (!name || SSA_NAME_IN_FREE_LIST (name))
	continue;
      Value_Range r (TREE_TYPE (name));
      Value_Range exp_range (TREE_TYPE (name));
      r.set_undefined ();
      FOR_EACH_IMM_USE_FAST (use_p, iter, name)
	{
	  gimple *use_stmt = USE_STMT (use_p);
	  if (is_gimple_debug (use_stmt))
	    continue;
	  if (!m_ranger.range_of_expr (exp_range, name, use_stmt))
	    exp_range.set_varying (TREE_TYPE (name));
	  r.union_ (exp_range);
	  if (r.varying_p ())
	    break;
	}
      // Include the on-exit range to ensure non-dominated unreachables
      // don't incorrectly impact the global range.
      m_ranger.range_on_entry (exp_range, EXIT_BLOCK_PTR_FOR_FN (cfun), name);
      r.union_ (exp_range);
      if (r.varying_p () || r.undefined_p ())
	continue;
      if (!set_range_info (name, r))
	continue;
      change = true;
      if (dump_file)
	{
	  fprintf (dump_file, "Global Exported (via unreachable): ");
	  print_generic_expr (dump_file, name, TDF_SLIM);
	  fprintf (dump_file, " = ");
	  gimple_range_global (r, name);
	  r.dump (dump_file);
	  fputc ('\n', dump_file);
	}
    }
  return change;
}

/* VR_TYPE describes a range with minimum value *MIN and maximum
   value *MAX.  Restrict the range to the set of values that have
   no bits set outside NONZERO_BITS.  Update *MIN and *MAX and
   return the new range type.

   SGN gives the sign of the values described by the range.  */

enum value_range_kind
intersect_range_with_nonzero_bits (enum value_range_kind vr_type,
				   wide_int *min, wide_int *max,
				   const wide_int &nonzero_bits,
				   signop sgn)
{
  if (vr_type == VR_ANTI_RANGE)
    {
      /* The VR_ANTI_RANGE is equivalent to the union of the ranges
	 A: [-INF, *MIN) and B: (*MAX, +INF].  First use NONZERO_BITS
	 to create an inclusive upper bound for A and an inclusive lower
	 bound for B.  */
      wide_int a_max = wi::round_down_for_mask (*min - 1, nonzero_bits);
      wide_int b_min = wi::round_up_for_mask (*max + 1, nonzero_bits);

      /* If the calculation of A_MAX wrapped, A is effectively empty
	 and A_MAX is the highest value that satisfies NONZERO_BITS.
	 Likewise if the calculation of B_MIN wrapped, B is effectively
	 empty and B_MIN is the lowest value that satisfies NONZERO_BITS.  */
      bool a_empty = wi::ge_p (a_max, *min, sgn);
      bool b_empty = wi::le_p (b_min, *max, sgn);

      /* If both A and B are empty, there are no valid values.  */
      if (a_empty && b_empty)
	return VR_UNDEFINED;

      /* If exactly one of A or B is empty, return a VR_RANGE for the
	 other one.  */
      if (a_empty || b_empty)
	{
	  *min = b_min;
	  *max = a_max;
	  gcc_checking_assert (wi::le_p (*min, *max, sgn));
	  return VR_RANGE;
	}

      /* Update the VR_ANTI_RANGE bounds.  */
      *min = a_max + 1;
      *max = b_min - 1;
      gcc_checking_assert (wi::le_p (*min, *max, sgn));

      /* Now check whether the excluded range includes any values that
	 satisfy NONZERO_BITS.  If not, switch to a full VR_RANGE.  */
      if (wi::round_up_for_mask (*min, nonzero_bits) == b_min)
	{
	  unsigned int precision = min->get_precision ();
	  *min = wi::min_value (precision, sgn);
	  *max = wi::max_value (precision, sgn);
	  vr_type = VR_RANGE;
	}
    }
  if (vr_type == VR_RANGE || vr_type == VR_VARYING)
    {
      *max = wi::round_down_for_mask (*max, nonzero_bits);

      /* Check that the range contains at least one valid value.  */
      if (wi::gt_p (*min, *max, sgn))
	return VR_UNDEFINED;

      *min = wi::round_up_for_mask (*min, nonzero_bits);
      gcc_checking_assert (wi::le_p (*min, *max, sgn));
    }
  return vr_type;
}

/* Return true if max and min of VR are INTEGER_CST.  It's not necessary
   a singleton.  */

bool
range_int_cst_p (const value_range *vr)
{
  return (vr->kind () == VR_RANGE && range_has_numeric_bounds_p (vr));
}

/* Return the single symbol (an SSA_NAME) contained in T if any, or NULL_TREE
   otherwise.  We only handle additive operations and set NEG to true if the
   symbol is negated and INV to the invariant part, if any.  */

static tree
get_single_symbol (tree t, bool *neg, tree *inv)
{
  bool neg_;
  tree inv_;

  *inv = NULL_TREE;
  *neg = false;

  if (TREE_CODE (t) == PLUS_EXPR
      || TREE_CODE (t) == POINTER_PLUS_EXPR
      || TREE_CODE (t) == MINUS_EXPR)
    {
      if (is_gimple_min_invariant (TREE_OPERAND (t, 0)))
	{
	  neg_ = (TREE_CODE (t) == MINUS_EXPR);
	  inv_ = TREE_OPERAND (t, 0);
	  t = TREE_OPERAND (t, 1);
	}
      else if (is_gimple_min_invariant (TREE_OPERAND (t, 1)))
	{
	  neg_ = false;
	  inv_ = TREE_OPERAND (t, 1);
	  t = TREE_OPERAND (t, 0);
	}
      else
        return NULL_TREE;
    }
  else
    {
      neg_ = false;
      inv_ = NULL_TREE;
    }

  if (TREE_CODE (t) == NEGATE_EXPR)
    {
      t = TREE_OPERAND (t, 0);
      neg_ = !neg_;
    }

  if (TREE_CODE (t) != SSA_NAME)
    return NULL_TREE;

  if (inv_ && TREE_OVERFLOW_P (inv_))
    inv_ = drop_tree_overflow (inv_);

  *neg = neg_;
  *inv = inv_;
  return t;
}

/* Return
   1 if VAL < VAL2
   0 if !(VAL < VAL2)
   -2 if those are incomparable.  */
int
operand_less_p (tree val, tree val2)
{
  /* LT is folded faster than GE and others.  Inline the common case.  */
  if (TREE_CODE (val) == INTEGER_CST && TREE_CODE (val2) == INTEGER_CST)
    return tree_int_cst_lt (val, val2);
  else if (TREE_CODE (val) == SSA_NAME && TREE_CODE (val2) == SSA_NAME)
    return val == val2 ? 0 : -2;
  else
    {
      int cmp = compare_values (val, val2);
      if (cmp == -1)
	return 1;
      else if (cmp == 0 || cmp == 1)
	return 0;
      else
	return -2;
    }
}

/* Compare two values VAL1 and VAL2.  Return

   	-2 if VAL1 and VAL2 cannot be compared at compile-time,
   	-1 if VAL1 < VAL2,
   	 0 if VAL1 == VAL2,
	+1 if VAL1 > VAL2, and
	+2 if VAL1 != VAL2

   This is similar to tree_int_cst_compare but supports pointer values
   and values that cannot be compared at compile time.

   If STRICT_OVERFLOW_P is not NULL, then set *STRICT_OVERFLOW_P to
   true if the return value is only valid if we assume that signed
   overflow is undefined.  */

int
compare_values_warnv (tree val1, tree val2, bool *strict_overflow_p)
{
  if (val1 == val2)
    return 0;

  /* Below we rely on the fact that VAL1 and VAL2 are both pointers or
     both integers.  */
  gcc_assert (POINTER_TYPE_P (TREE_TYPE (val1))
	      == POINTER_TYPE_P (TREE_TYPE (val2)));

  /* Convert the two values into the same type.  This is needed because
     sizetype causes sign extension even for unsigned types.  */
  if (!useless_type_conversion_p (TREE_TYPE (val1), TREE_TYPE (val2)))
    val2 = fold_convert (TREE_TYPE (val1), val2);

  const bool overflow_undefined
    = INTEGRAL_TYPE_P (TREE_TYPE (val1))
      && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (val1));
  tree inv1, inv2;
  bool neg1, neg2;
  tree sym1 = get_single_symbol (val1, &neg1, &inv1);
  tree sym2 = get_single_symbol (val2, &neg2, &inv2);

  /* If VAL1 and VAL2 are of the form '[-]NAME [+ CST]', return -1 or +1
     accordingly.  If VAL1 and VAL2 don't use the same name, return -2.  */
  if (sym1 && sym2)
    {
      /* Both values must use the same name with the same sign.  */
      if (sym1 != sym2 || neg1 != neg2)
	return -2;

      /* [-]NAME + CST == [-]NAME + CST.  */
      if (inv1 == inv2)
	return 0;

      /* If overflow is defined we cannot simplify more.  */
      if (!overflow_undefined)
	return -2;

      if (strict_overflow_p != NULL
	  /* Symbolic range building sets the no-warning bit to declare
	     that overflow doesn't happen.  */
	  && (!inv1 || !warning_suppressed_p (val1, OPT_Woverflow))
	  && (!inv2 || !warning_suppressed_p (val2, OPT_Woverflow)))
	*strict_overflow_p = true;

      if (!inv1)
	inv1 = build_int_cst (TREE_TYPE (val1), 0);
      if (!inv2)
	inv2 = build_int_cst (TREE_TYPE (val2), 0);

      return wi::cmp (wi::to_wide (inv1), wi::to_wide (inv2),
		      TYPE_SIGN (TREE_TYPE (val1)));
    }

  const bool cst1 = is_gimple_min_invariant (val1);
  const bool cst2 = is_gimple_min_invariant (val2);

  /* If one is of the form '[-]NAME + CST' and the other is constant, then
     it might be possible to say something depending on the constants.  */
  if ((sym1 && inv1 && cst2) || (sym2 && inv2 && cst1))
    {
      if (!overflow_undefined)
	return -2;

      if (strict_overflow_p != NULL
	  /* Symbolic range building sets the no-warning bit to declare
	     that overflow doesn't happen.  */
	  && (!sym1 || !warning_suppressed_p (val1, OPT_Woverflow))
	  && (!sym2 || !warning_suppressed_p (val2, OPT_Woverflow)))
	*strict_overflow_p = true;

      const signop sgn = TYPE_SIGN (TREE_TYPE (val1));
      tree cst = cst1 ? val1 : val2;
      tree inv = cst1 ? inv2 : inv1;

      /* Compute the difference between the constants.  If it overflows or
	 underflows, this means that we can trivially compare the NAME with
	 it and, consequently, the two values with each other.  */
      wide_int diff = wi::to_wide (cst) - wi::to_wide (inv);
      if (wi::cmp (0, wi::to_wide (inv), sgn)
	  != wi::cmp (diff, wi::to_wide (cst), sgn))
	{
	  const int res = wi::cmp (wi::to_wide (cst), wi::to_wide (inv), sgn);
	  return cst1 ? res : -res;
	}

      return -2;
    }

  /* We cannot say anything more for non-constants.  */
  if (!cst1 || !cst2)
    return -2;

  if (!POINTER_TYPE_P (TREE_TYPE (val1)))
    {
      /* We cannot compare overflowed values.  */
      if (TREE_OVERFLOW (val1) || TREE_OVERFLOW (val2))
	return -2;

      if (TREE_CODE (val1) == INTEGER_CST
	  && TREE_CODE (val2) == INTEGER_CST)
	return tree_int_cst_compare (val1, val2);

      if (poly_int_tree_p (val1) && poly_int_tree_p (val2))
	{
	  if (known_eq (wi::to_poly_widest (val1),
			wi::to_poly_widest (val2)))
	    return 0;
	  if (known_lt (wi::to_poly_widest (val1),
			wi::to_poly_widest (val2)))
	    return -1;
	  if (known_gt (wi::to_poly_widest (val1),
			wi::to_poly_widest (val2)))
	    return 1;
	}

      return -2;
    }
  else
    {
      if (TREE_CODE (val1) == INTEGER_CST && TREE_CODE (val2) == INTEGER_CST)
	{
	  /* We cannot compare overflowed values.  */
	  if (TREE_OVERFLOW (val1) || TREE_OVERFLOW (val2))
	    return -2;

	  return tree_int_cst_compare (val1, val2);
	}

      /* First see if VAL1 and VAL2 are not the same.  */
      if (operand_equal_p (val1, val2, 0))
	return 0;

      fold_defer_overflow_warnings ();

      /* If VAL1 is a lower address than VAL2, return -1.  */
      tree t = fold_binary_to_constant (LT_EXPR, boolean_type_node, val1, val2);
      if (t && integer_onep (t))
	{
	  fold_undefer_and_ignore_overflow_warnings ();
	  return -1;
	}

      /* If VAL1 is a higher address than VAL2, return +1.  */
      t = fold_binary_to_constant (LT_EXPR, boolean_type_node, val2, val1);
      if (t && integer_onep (t))
	{
	  fold_undefer_and_ignore_overflow_warnings ();
	  return 1;
	}

      /* If VAL1 is different than VAL2, return +2.  */
      t = fold_binary_to_constant (NE_EXPR, boolean_type_node, val1, val2);
      fold_undefer_and_ignore_overflow_warnings ();
      if (t && integer_onep (t))
	return 2;

      return -2;
    }
}

/* Compare values like compare_values_warnv.  */

int
compare_values (tree val1, tree val2)
{
  bool sop;
  return compare_values_warnv (val1, val2, &sop);
}

/* If the types passed are supported, return TRUE, otherwise set VR to
   VARYING and return FALSE.  */

static bool
supported_types_p (value_range *vr,
		   tree type0,
		   tree = NULL)
{
  if (!value_range::supports_p (type0))
    {
      vr->set_varying (type0);
      return false;
    }
  return true;
}

/* If any of the ranges passed are defined, return TRUE, otherwise set
   VR to UNDEFINED and return FALSE.  */

static bool
defined_ranges_p (value_range *vr,
		  const value_range *vr0, const value_range *vr1 = NULL)
{
  if (vr0->undefined_p () && (!vr1 || vr1->undefined_p ()))
    {
      vr->set_undefined ();
      return false;
    }
  return true;
}

/* Perform a binary operation on a pair of ranges.  */

void
range_fold_binary_expr (value_range *vr,
			enum tree_code code,
			tree expr_type,
			const value_range *vr0_,
			const value_range *vr1_)
{
  if (!supported_types_p (vr, expr_type)
      || !defined_ranges_p (vr, vr0_, vr1_))
    return;
  range_op_handler op (code, expr_type);
  if (!op)
    {
      vr->set_varying (expr_type);
      return;
    }

  value_range vr0 (*vr0_);
  value_range vr1 (*vr1_);
  if (vr0.undefined_p ())
    vr0.set_varying (expr_type);
  if (vr1.undefined_p ())
    vr1.set_varying (expr_type);
  vr0.normalize_addresses ();
  vr1.normalize_addresses ();
  if (!op.fold_range (*vr, expr_type, vr0, vr1))
    vr->set_varying (expr_type);
}

/* Perform a unary operation on a range.  */

void
range_fold_unary_expr (value_range *vr,
		       enum tree_code code, tree expr_type,
		       const value_range *vr0,
		       tree vr0_type)
{
  if (!supported_types_p (vr, expr_type, vr0_type)
      || !defined_ranges_p (vr, vr0))
    return;
  range_op_handler op (code, expr_type);
  if (!op)
    {
      vr->set_varying (expr_type);
      return;
    }

  value_range vr0_cst (*vr0);
  vr0_cst.normalize_addresses ();
  if (!op.fold_range (*vr, expr_type, vr0_cst, value_range (expr_type)))
    vr->set_varying (expr_type);
}

/* Helper for overflow_comparison_p

   OP0 CODE OP1 is a comparison.  Examine the comparison and potentially
   OP1's defining statement to see if it ultimately has the form
   OP0 CODE (OP0 PLUS INTEGER_CST)

   If so, return TRUE indicating this is an overflow test and store into
   *NEW_CST an updated constant that can be used in a narrowed range test.

   REVERSED indicates if the comparison was originally:

   OP1 CODE' OP0.

   This affects how we build the updated constant.  */

static bool
overflow_comparison_p_1 (enum tree_code code, tree op0, tree op1,
			 bool reversed, tree *new_cst)
{
  /* See if this is a relational operation between two SSA_NAMES with
     unsigned, overflow wrapping values.  If so, check it more deeply.  */
  if ((code == LT_EXPR || code == LE_EXPR
       || code == GE_EXPR || code == GT_EXPR)
      && TREE_CODE (op0) == SSA_NAME
      && TREE_CODE (op1) == SSA_NAME
      && INTEGRAL_TYPE_P (TREE_TYPE (op0))
      && TYPE_UNSIGNED (TREE_TYPE (op0))
      && TYPE_OVERFLOW_WRAPS (TREE_TYPE (op0)))
    {
      gimple *op1_def = SSA_NAME_DEF_STMT (op1);

      /* Now look at the defining statement of OP1 to see if it adds
	 or subtracts a nonzero constant from another operand.  */
      if (op1_def
	  && is_gimple_assign (op1_def)
	  && gimple_assign_rhs_code (op1_def) == PLUS_EXPR
	  && TREE_CODE (gimple_assign_rhs2 (op1_def)) == INTEGER_CST
	  && !integer_zerop (gimple_assign_rhs2 (op1_def)))
	{
	  tree target = gimple_assign_rhs1 (op1_def);

	  /* If we did not find our target SSA_NAME, then this is not
	     an overflow test.  */
	  if (op0 != target)
	    return false;

	  tree type = TREE_TYPE (op0);
	  wide_int max = wi::max_value (TYPE_PRECISION (type), UNSIGNED);
	  tree inc = gimple_assign_rhs2 (op1_def);
	  if (reversed)
	    *new_cst = wide_int_to_tree (type, max + wi::to_wide (inc));
	  else
	    *new_cst = wide_int_to_tree (type, max - wi::to_wide (inc));
	  return true;
	}
    }
  return false;
}

/* OP0 CODE OP1 is a comparison.  Examine the comparison and potentially
   OP1's defining statement to see if it ultimately has the form
   OP0 CODE (OP0 PLUS INTEGER_CST)

   If so, return TRUE indicating this is an overflow test and store into
   *NEW_CST an updated constant that can be used in a narrowed range test.

   These statements are left as-is in the IL to facilitate discovery of
   {ADD,SUB}_OVERFLOW sequences later in the optimizer pipeline.  But
   the alternate range representation is often useful within VRP.  */

bool
overflow_comparison_p (tree_code code, tree name, tree val, tree *new_cst)
{
  if (overflow_comparison_p_1 (code, name, val, false, new_cst))
    return true;
  return overflow_comparison_p_1 (swap_tree_comparison (code), val, name,
				  true, new_cst);
}

/* Handle
   _4 = x_3 & 31;
   if (_4 != 0)
     goto <bb 6>;
   else
     goto <bb 7>;
   <bb 6>:
   __builtin_unreachable ();
   <bb 7>:

   If x_3 has no other immediate uses (checked by caller), var is the
   x_3 var, we can clear low 5 bits from the non-zero bitmask.  */

void
maybe_set_nonzero_bits (edge e, tree var)
{
  basic_block cond_bb = e->src;
  gimple *stmt = last_stmt (cond_bb);
  tree cst;

  if (stmt == NULL
      || gimple_code (stmt) != GIMPLE_COND
      || gimple_cond_code (stmt) != ((e->flags & EDGE_TRUE_VALUE)
				     ? EQ_EXPR : NE_EXPR)
      || TREE_CODE (gimple_cond_lhs (stmt)) != SSA_NAME
      || !integer_zerop (gimple_cond_rhs (stmt)))
    return;

  stmt = SSA_NAME_DEF_STMT (gimple_cond_lhs (stmt));
  if (!is_gimple_assign (stmt)
      || gimple_assign_rhs_code (stmt) != BIT_AND_EXPR
      || TREE_CODE (gimple_assign_rhs2 (stmt)) != INTEGER_CST)
    return;
  if (gimple_assign_rhs1 (stmt) != var)
    {
      gimple *stmt2;

      if (TREE_CODE (gimple_assign_rhs1 (stmt)) != SSA_NAME)
	return;
      stmt2 = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));
      if (!gimple_assign_cast_p (stmt2)
	  || gimple_assign_rhs1 (stmt2) != var
	  || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt2))
	  || (TYPE_PRECISION (TREE_TYPE (gimple_assign_rhs1 (stmt)))
			      != TYPE_PRECISION (TREE_TYPE (var))))
	return;
    }
  cst = gimple_assign_rhs2 (stmt);
  if (POINTER_TYPE_P (TREE_TYPE (var)))
    {
      struct ptr_info_def *pi = SSA_NAME_PTR_INFO (var);
      if (pi && pi->misalign)
	return;
      wide_int w = wi::bit_not (wi::to_wide (cst));
      unsigned int bits = wi::ctz (w);
      if (bits == 0 || bits >= HOST_BITS_PER_INT)
	return;
      unsigned int align = 1U << bits;
      if (pi == NULL || pi->align < align)
	set_ptr_info_alignment (get_ptr_info (var), align, 0);
    }
  else
    set_nonzero_bits (var, wi::bit_and_not (get_nonzero_bits (var),
					    wi::to_wide (cst)));
}

/* Searches the case label vector VEC for the index *IDX of the CASE_LABEL
   that includes the value VAL.  The search is restricted to the range
   [START_IDX, n - 1] where n is the size of VEC.

   If there is a CASE_LABEL for VAL, its index is placed in IDX and true is
   returned.

   If there is no CASE_LABEL for VAL and there is one that is larger than VAL,
   it is placed in IDX and false is returned.

   If VAL is larger than any CASE_LABEL, n is placed on IDX and false is
   returned. */

bool
find_case_label_index (gswitch *stmt, size_t start_idx, tree val, size_t *idx)
{
  size_t n = gimple_switch_num_labels (stmt);
  size_t low, high;

  /* Find case label for minimum of the value range or the next one.
     At each iteration we are searching in [low, high - 1]. */

  for (low = start_idx, high = n; high != low; )
    {
      tree t;
      int cmp;
      /* Note that i != high, so we never ask for n. */
      size_t i = (high + low) / 2;
      t = gimple_switch_label (stmt, i);

      /* Cache the result of comparing CASE_LOW and val.  */
      cmp = tree_int_cst_compare (CASE_LOW (t), val);

      if (cmp == 0)
	{
	  /* Ranges cannot be empty. */
	  *idx = i;
	  return true;
	}
      else if (cmp > 0)
	high = i;
      else
	{
	  low = i + 1;
	  if (CASE_HIGH (t) != NULL
	      && tree_int_cst_compare (CASE_HIGH (t), val) >= 0)
	    {
	      *idx = i;
	      return true;
	    }
	}
    }

  *idx = high;
  return false;
}

/* Searches the case label vector VEC for the range of CASE_LABELs that is used
   for values between MIN and MAX. The first index is placed in MIN_IDX. The
   last index is placed in MAX_IDX. If the range of CASE_LABELs is empty
   then MAX_IDX < MIN_IDX.
   Returns true if the default label is not needed. */

bool
find_case_label_range (gswitch *stmt, tree min, tree max, size_t *min_idx,
		       size_t *max_idx)
{
  size_t i, j;
  bool min_take_default = !find_case_label_index (stmt, 1, min, &i);
  bool max_take_default = !find_case_label_index (stmt, i, max, &j);

  if (i == j
      && min_take_default
      && max_take_default)
    {
      /* Only the default case label reached.
         Return an empty range. */
      *min_idx = 1;
      *max_idx = 0;
      return false;
    }
  else
    {
      bool take_default = min_take_default || max_take_default;
      tree low, high;
      size_t k;

      if (max_take_default)
	j--;

      /* If the case label range is continuous, we do not need
	 the default case label.  Verify that.  */
      high = CASE_LOW (gimple_switch_label (stmt, i));
      if (CASE_HIGH (gimple_switch_label (stmt, i)))
	high = CASE_HIGH (gimple_switch_label (stmt, i));
      for (k = i + 1; k <= j; ++k)
	{
	  low = CASE_LOW (gimple_switch_label (stmt, k));
	  if (!integer_onep (int_const_binop (MINUS_EXPR, low, high)))
	    {
	      take_default = true;
	      break;
	    }
	  high = low;
	  if (CASE_HIGH (gimple_switch_label (stmt, k)))
	    high = CASE_HIGH (gimple_switch_label (stmt, k));
	}

      *min_idx = i;
      *max_idx = j;
      return !take_default;
    }
}

/* Given a SWITCH_STMT, return the case label that encompasses the
   known possible values for the switch operand.  RANGE_OF_OP is a
   range for the known values of the switch operand.  */

tree
find_case_label_range (gswitch *switch_stmt, const irange *range_of_op)
{
  if (range_of_op->undefined_p ()
      || range_of_op->varying_p ())
    return NULL_TREE;

  size_t i, j;
  tree op = gimple_switch_index (switch_stmt);
  tree type = TREE_TYPE (op);
  tree tmin = wide_int_to_tree (type, range_of_op->lower_bound ());
  tree tmax = wide_int_to_tree (type, range_of_op->upper_bound ());
  find_case_label_range (switch_stmt, tmin, tmax, &i, &j);
  if (i == j)
    {
      /* Look for exactly one label that encompasses the range of
	 the operand.  */
      tree label = gimple_switch_label (switch_stmt, i);
      tree case_high
	= CASE_HIGH (label) ? CASE_HIGH (label) : CASE_LOW (label);
      int_range_max label_range (CASE_LOW (label), case_high);
      if (!types_compatible_p (label_range.type (), range_of_op->type ()))
	range_cast (label_range, range_of_op->type ());
      label_range.intersect (*range_of_op);
      if (label_range == *range_of_op)
	return label;
    }
  else if (i > j)
    {
      /* If there are no labels at all, take the default.  */
      return gimple_switch_label (switch_stmt, 0);
    }
  else
    {
      /* Otherwise, there are various labels that can encompass
	 the range of operand.  In which case, see if the range of
	 the operand is entirely *outside* the bounds of all the
	 (non-default) case labels.  If so, take the default.  */
      unsigned n = gimple_switch_num_labels (switch_stmt);
      tree min_label = gimple_switch_label (switch_stmt, 1);
      tree max_label = gimple_switch_label (switch_stmt, n - 1);
      tree case_high = CASE_HIGH (max_label);
      if (!case_high)
	case_high = CASE_LOW (max_label);
      int_range_max label_range (CASE_LOW (min_label), case_high);
      if (!types_compatible_p (label_range.type (), range_of_op->type ()))
	range_cast (label_range, range_of_op->type ());
      label_range.intersect (*range_of_op);
      if (label_range.undefined_p ())
	return gimple_switch_label (switch_stmt, 0);
    }
  return NULL_TREE;
}

struct case_info
{
  tree expr;
  basic_block bb;
};

// This is a ranger based folder which continues to use the dominator
// walk to access the substitute and fold machinery.  Ranges are calculated
// on demand.

class rvrp_folder : public substitute_and_fold_engine
{
public:

  rvrp_folder (gimple_ranger *r) : substitute_and_fold_engine (),
				   m_unreachable (*r),
				   m_simplifier (r, r->non_executable_edge_flag)
  {
    m_ranger = r;
    m_pta = new pointer_equiv_analyzer (m_ranger);
    m_last_bb_stmt = NULL;
  }

  ~rvrp_folder ()
  {
    delete m_pta;
  }

  tree value_of_expr (tree name, gimple *s = NULL) override
  {
    // Shortcircuit subst_and_fold callbacks for abnormal ssa_names.
    if (TREE_CODE (name) == SSA_NAME && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name))
      return NULL;
    tree ret = m_ranger->value_of_expr (name, s);
    if (!ret && supported_pointer_equiv_p (name))
      ret = m_pta->get_equiv (name);
    return ret;
  }

  tree value_on_edge (edge e, tree name) override
  {
    // Shortcircuit subst_and_fold callbacks for abnormal ssa_names.
    if (TREE_CODE (name) == SSA_NAME && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name))
      return NULL;
    tree ret = m_ranger->value_on_edge (e, name);
    if (!ret && supported_pointer_equiv_p (name))
      ret = m_pta->get_equiv (name);
    return ret;
  }

  tree value_of_stmt (gimple *s, tree name = NULL) override
  {
    // Shortcircuit subst_and_fold callbacks for abnormal ssa_names.
    if (TREE_CODE (name) == SSA_NAME && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name))
      return NULL;
    return m_ranger->value_of_stmt (s, name);
  }

  void pre_fold_bb (basic_block bb) override
  {
    m_pta->enter (bb);
    for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	 gsi_next (&gsi))
      m_ranger->register_inferred_ranges (gsi.phi ());
    m_last_bb_stmt = last_stmt (bb);
  }

  void post_fold_bb (basic_block bb) override
  {
    m_pta->leave (bb);
    if (cfun->after_inlining)
      m_unreachable.maybe_register_block (bb);
  }

  void pre_fold_stmt (gimple *stmt) override
  {
    m_pta->visit_stmt (stmt);
    // If this is the last stmt and there are inferred ranges, reparse the
    // block for transitive inferred ranges that occur earlier in the block.
    if (stmt == m_last_bb_stmt)
      m_ranger->register_transitive_inferred_ranges (gimple_bb (stmt));
  }

  bool fold_stmt (gimple_stmt_iterator *gsi) override
  {
    bool ret = m_simplifier.simplify (gsi);
    if (!ret)
      ret = m_ranger->fold_stmt (gsi, follow_single_use_edges);
    m_ranger->register_inferred_ranges (gsi_stmt (*gsi));
    return ret;
  }

  remove_unreachable m_unreachable;
private:
  DISABLE_COPY_AND_ASSIGN (rvrp_folder);
  gimple_ranger *m_ranger;
  simplify_using_ranges m_simplifier;
  pointer_equiv_analyzer *m_pta;
  gimple *m_last_bb_stmt;
};

/* Main entry point for a VRP pass using just ranger. This can be called
  from anywhere to perform a VRP pass, including from EVRP.  */

unsigned int
execute_ranger_vrp (struct function *fun, bool warn_array_bounds_p,
		    bool final_p)
{
  loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
  scev_initialize ();
  calculate_dominance_info (CDI_DOMINATORS);

  set_all_edges_as_executable (fun);
  gimple_ranger *ranger = enable_ranger (fun, false);
  rvrp_folder folder (ranger);
  folder.substitute_and_fold ();
  // Remove tagged builtin-unreachable and maybe update globals.
  folder.m_unreachable.remove_and_update_globals (final_p);
  if (dump_file && (dump_flags & TDF_DETAILS))
    ranger->dump (dump_file);

  if ((warn_array_bounds || warn_strict_flex_arrays) && warn_array_bounds_p)
    {
      // Set all edges as executable, except those ranger says aren't.
      int non_exec_flag = ranger->non_executable_edge_flag;
      basic_block bb;
      FOR_ALL_BB_FN (bb, fun)
	{
	  edge_iterator ei;
	  edge e;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (e->flags & non_exec_flag)
	      e->flags &= ~EDGE_EXECUTABLE;
	    else
	      e->flags |= EDGE_EXECUTABLE;
	}
      scev_reset ();
      array_bounds_checker array_checker (fun, ranger);
      array_checker.check ();
    }

  disable_ranger (fun);
  scev_finalize ();
  loop_optimizer_finalize ();
  return 0;
}

namespace {

const pass_data pass_data_vrp =
{
  GIMPLE_PASS, /* type */
  "vrp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_VRP, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg | TODO_update_ssa ), /* todo_flags_finish */
};

const pass_data pass_data_early_vrp =
{
  GIMPLE_PASS, /* type */
  "evrp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_EARLY_VRP, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg | TODO_update_ssa | TODO_verify_all ),
};

static int vrp_pass_num = 0;
class pass_vrp : public gimple_opt_pass
{
public:
  pass_vrp (gcc::context *ctxt, const pass_data &data_)
    : gimple_opt_pass (data_, ctxt), data (data_), warn_array_bounds_p (false),
      my_pass (vrp_pass_num++)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override { return new pass_vrp (m_ctxt, data); }
  void set_pass_param (unsigned int n, bool param) final override
    {
      gcc_assert (n == 0);
      warn_array_bounds_p = param;
    }
  bool gate (function *) final override { return flag_tree_vrp != 0; }
  unsigned int execute (function *fun) final override
    {
      // Early VRP pass.
      if (my_pass == 0)
	return execute_ranger_vrp (fun, /*warn_array_bounds_p=*/false, false);

      return execute_ranger_vrp (fun, warn_array_bounds_p, my_pass == 2);
    }

 private:
  const pass_data &data;
  bool warn_array_bounds_p;
  int my_pass;
}; // class pass_vrp

const pass_data pass_data_assumptions =
{
  GIMPLE_PASS, /* type */
  "assumptions", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_ASSUMPTIONS, /* tv_id */
  PROP_ssa, /* properties_required */
  PROP_assumptions_done, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_end */
};

class pass_assumptions : public gimple_opt_pass
{
public:
  pass_assumptions (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_assumptions, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *fun) final override { return fun->assume_function; }
  unsigned int execute (function *) final override
    {
      assume_query query;
      if (dump_file)
	fprintf (dump_file, "Assumptions :\n--------------\n");

      for (tree arg = DECL_ARGUMENTS (cfun->decl); arg; arg = DECL_CHAIN (arg))
	{
	  tree name = ssa_default_def (cfun, arg);
	  if (!name || !gimple_range_ssa_p (name))
	    continue;
	  tree type = TREE_TYPE (name);
	  if (!Value_Range::supports_type_p (type))
	    continue;
	  Value_Range assume_range (type);
	  if (query.assume_range_p (assume_range, name))
	    {
	      // Set the global range of NAME to anything calculated.
	      set_range_info (name, assume_range);
	      if (dump_file)
		{
		  print_generic_expr (dump_file, name, TDF_SLIM);
		  fprintf (dump_file, " -> ");
		  assume_range.dump (dump_file);
		  fputc ('\n', dump_file);
		}
	    }
	}
      if (dump_file)
	{
	  fputc ('\n', dump_file);
	  gimple_dump_cfg (dump_file, dump_flags & ~TDF_DETAILS);
	  if (dump_flags & TDF_DETAILS)
	    query.dump (dump_file);
	}
      return TODO_discard_function;
    }

}; // class pass_assumptions

} // anon namespace

gimple_opt_pass *
make_pass_vrp (gcc::context *ctxt)
{
  return new pass_vrp (ctxt, pass_data_vrp);
}

gimple_opt_pass *
make_pass_early_vrp (gcc::context *ctxt)
{
  return new pass_vrp (ctxt, pass_data_early_vrp);
}

gimple_opt_pass *
make_pass_assumptions (gcc::context *ctx)
{
  return new pass_assumptions (ctx);
}
