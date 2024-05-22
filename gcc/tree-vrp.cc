/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2005-2024 Free Software Foundation, Inc.
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
#include "alloc-pool.h"
#include "cgraph.h"
#include "symbol-summary.h"
#include "ipa-utils.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "attribs.h"

// This class is utilized by VRP and ranger to remove __builtin_unreachable
// calls, and reflect any resulting global ranges.
//
// maybe_register() is called on condition statements , and if that
// matches the pattern of one branch being a builtin_unreachable, either check
// for early removal or register the resulting executable edge in a list.
//
// During early/non-final processing, we check to see if ALL exports from the
// block can be safely updated with a new global value.  If they can, then
// we rewrite the condition and update those values immediately.  Otherwise
// the unreachable condition is left in the IL until the final pass.
//
// During final processing, after all blocks have been registered,
// remove_and_update_globals() will
// - check all exports from registered blocks
// - ensure the cache entry of each export is set with the appropriate range
// - rewrite the conditions to take the executable edge
// - perform DCE on any feeding instructions to those rewritten conditions
//
// Then each of the immediate use chain of each export is walked, and a new
// global range created by unioning the ranges at all remaining use locations.

class remove_unreachable {
public:
  remove_unreachable (range_query &r, bool all) : m_ranger (r), final_p (all)
    { m_list.create (30); }
  ~remove_unreachable () { m_list.release (); }
  void handle_early (gimple *s, edge e);
  void maybe_register (gimple *s);
  bool remove ();
  bool remove_and_update_globals ();
  vec<std::pair<int, int> > m_list;
  range_query &m_ranger;
  bool final_p;
};

// Check if block BB has a __builtin_unreachable () call on one arm, and
// register the executable edge if so.

void
remove_unreachable::maybe_register (gimple *s)
{
  gcc_checking_assert  (gimple_code (s) == GIMPLE_COND);
  basic_block bb = gimple_bb (s);

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

  // Constant expressions are ignored.
  if (TREE_CODE (gimple_cond_lhs (s)) != SSA_NAME
      && TREE_CODE (gimple_cond_rhs (s)) != SSA_NAME)
    return;

  edge e = un0 ? e1 : e0;

  if (!final_p)
    handle_early (s, e);
  else
    m_list.safe_push (std::make_pair (e->src->index, e->dest->index));
}

// Return true if all uses of NAME are dominated by block BB.  1 use
// is allowed in block BB, This is one we hope to remove.
// ie
//  _2 = _1 & 7;
//  if (_2 != 0)
//    goto <bb 3>; [0.00%]
//  Any additional use of _1 or _2 in this block invalidates early replacement.

static bool
fully_replaceable (tree name, basic_block bb)
{
  use_operand_p use_p;
  imm_use_iterator iter;
  bool saw_in_bb = false;

  // If a name loads from memory, we may lose information used in
  // commoning opportunities later.  See tree-ssa/ssa-pre-34.c.
  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
  if (gimple_vuse (def_stmt))
    return false;

  FOR_EACH_IMM_USE_FAST (use_p, iter, name)
    {
      gimple *use_stmt = USE_STMT (use_p);
      // Ignore debug stmts and the branch.
      if (is_gimple_debug (use_stmt))
	continue;
      basic_block use_bb = gimple_bb (use_stmt);
      // Only one use in the block allowed to avoid complicated cases.
      if (use_bb == bb)
	{
	  if (saw_in_bb)
	    return false;
	  else
	    saw_in_bb = true;
	}
      else if (!dominated_by_p (CDI_DOMINATORS, use_bb, bb))
	return false;
    }
  return true;
}

// This routine is called to check builtin_unreachable calls during any
// time before final removal.  The only way we can be sure it does not
// provide any additional information is to expect that we can update the
// global values of all exports from a block.   This means the branch
// to the unreachable call must dominate all uses of those ssa-names, with
// the exception that there can be a single use in the block containing
// the branch. IF the name used in the branch is defined in the block, it may
// contain the name of something else that will be an export.  And likewise
// that may also use another name that is an export etc.
//
// As long as there is only a single use, we can be sure that there are no other
// side effects (like being passed to a call, or stored to a global, etc.
// This means we will miss cases where there are 2 or more uses that have
// no interveneing statements that may had side effects, but it catches most
// of the caes we care about, and prevents expensive in depth analysis.
//
// Ranger will still reflect the proper ranges at other places in these missed
// cases, we simply will not remove/set globals early.

void
remove_unreachable::handle_early (gimple *s, edge e)
{
  // If there is no gori_ssa, there is no early processsing.
  if (!m_ranger.gori_ssa ())
    return ;
  bool lhs_p = TREE_CODE (gimple_cond_lhs (s)) == SSA_NAME;
  bool rhs_p = TREE_CODE (gimple_cond_rhs (s)) == SSA_NAME;
  // Do not remove __builtin_unreachable if it confers a relation, or
  // that relation may be lost in subsequent passes.
  if (lhs_p && rhs_p)
    return;
  // Do not remove addresses early. ie if (x == &y)
  if (lhs_p && TREE_CODE (gimple_cond_rhs (s)) == ADDR_EXPR)
    return;

  gcc_checking_assert (gimple_outgoing_range_stmt_p (e->src) == s);
  gcc_checking_assert (!final_p);

  // Check if every export use is dominated by this branch.
  tree name;
  FOR_EACH_GORI_EXPORT_NAME (m_ranger.gori_ssa (), e->src, name)
    {
      if (!fully_replaceable (name, e->src))
	return;
    }

  // Set the global value for each.
  FOR_EACH_GORI_EXPORT_NAME (m_ranger.gori_ssa (), e->src, name)
    {
      Value_Range r (TREE_TYPE (name));
      m_ranger.range_on_entry (r, e->dest, name);
      // Nothing at this late stage we can do if the write fails.
      if (!set_range_info (name, r))
	continue;
      if (dump_file)
	{
	  fprintf (dump_file, "Global Exported (via early unreachable): ");
	  print_generic_expr (dump_file, name, TDF_SLIM);
	  fprintf (dump_file, " = ");
	  gimple_range_global (r, name);
	  r.dump (dump_file);
	  fputc ('\n', dump_file);
	}
    }

  tree ssa = lhs_p ? gimple_cond_lhs (s) : gimple_cond_rhs (s);

  // Rewrite the condition.
  if (e->flags & EDGE_TRUE_VALUE)
    gimple_cond_make_true (as_a<gcond *> (s));
  else
    gimple_cond_make_false (as_a<gcond *> (s));
  update_stmt (s);

  // If the name on S is defined in this block, see if there is DCE work to do.
  if (gimple_bb (SSA_NAME_DEF_STMT (ssa)) == e->src)
    {
      auto_bitmap dce;
      bitmap_set_bit (dce, SSA_NAME_VERSION (ssa));
      simple_dce_from_worklist (dce);
    }
}

// Process the edges in the list, change the conditions and removing any
// dead code feeding those conditions.   This removes the unreachables, but
// makes no attempt to set globals values.

bool
remove_unreachable::remove ()
{
  if (!final_p || m_list.length () == 0)
    return false;

  bool change = false;
  unsigned i;
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

      change = true;
      // Rewrite the condition.
      if (e->flags & EDGE_TRUE_VALUE)
	gimple_cond_make_true (as_a<gcond *> (s));
      else
	gimple_cond_make_false (as_a<gcond *> (s));
      update_stmt (s);
    }

  return change;
}


// Process the edges in the list, change the conditions and removing any
// dead code feeding those conditions.  Calculate the range of any
// names that may have been exported from those blocks, and determine if
// there is any updates to their global ranges..
// Return true if any builtin_unreachables/globals eliminated/updated.

bool
remove_unreachable::remove_and_update_globals ()
{
  if (m_list.length () == 0)
    return false;

  // If there is no import/export info, just remove unreachables if necessary.
  if (!m_ranger.gori_ssa ())
    return remove ();

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

      bool dominate_exit_p = true;
      FOR_EACH_GORI_EXPORT_NAME (m_ranger.gori_ssa (), e->src, name)
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
	bitmap_ior_into (all_exports,
			 m_ranger.gori_ssa ()->exports (e->src));
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
      wide_int wlow = wi::to_wide (CASE_LOW (label));
      wide_int whigh = wi::to_wide (case_high);
      int_range_max label_range (TREE_TYPE (case_high), wlow, whigh);
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
      int_range_max label_range (TREE_TYPE (CASE_LOW (min_label)),
				 wi::to_wide (CASE_LOW (min_label)),
				 wi::to_wide (case_high));
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

  rvrp_folder (gimple_ranger *r, bool all)
    : substitute_and_fold_engine (),
      m_unreachable (*r, all),
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
    m_last_bb_stmt = last_nondebug_stmt (bb);
  }

  void post_fold_bb (basic_block bb) override
  {
    m_pta->leave (bb);
  }

  void pre_fold_stmt (gimple *stmt) override
  {
    m_pta->visit_stmt (stmt);
    // If this is the last stmt and there are inferred ranges, reparse the
    // block for transitive inferred ranges that occur earlier in the block.
    if (stmt == m_last_bb_stmt)
      {
	m_ranger->register_transitive_inferred_ranges (gimple_bb (stmt));
	// Also check for builtin_unreachable calls.
	if (cfun->after_inlining && gimple_code (stmt) == GIMPLE_COND)
	  m_unreachable.maybe_register (stmt);
      }
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
  rvrp_folder folder (ranger, final_p);
  phi_analysis_initialize (ranger->const_query ());
  folder.substitute_and_fold ();
  // Remove tagged builtin-unreachable and maybe update globals.
  folder.m_unreachable.remove_and_update_globals ();
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


  if (Value_Range::supports_type_p (TREE_TYPE
				     (TREE_TYPE (current_function_decl)))
      && flag_ipa_vrp
      && !lookup_attribute ("noipa", DECL_ATTRIBUTES (current_function_decl)))
    {
      edge e;
      edge_iterator ei;
      bool found = false;
      Value_Range return_range (TREE_TYPE (TREE_TYPE (current_function_decl)));
      FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
	if (greturn *ret = dyn_cast <greturn *> (*gsi_last_bb (e->src)))
	  {
	    tree retval = gimple_return_retval (ret);
	    if (!retval)
	      {
		return_range.set_varying (TREE_TYPE (TREE_TYPE (current_function_decl)));
		found = true;
		continue;
	      }
	    Value_Range r (TREE_TYPE (retval));
	    if (ranger->range_of_expr (r, retval, ret)
		&& !r.undefined_p ()
		&& !r.varying_p ())
	      {
		if (!found)
		  return_range = r;
		else
		  return_range.union_ (r);
	      }
	    else
	      return_range.set_varying (TREE_TYPE (retval));
	    found = true;
	  }
      if (found && !return_range.varying_p ())
	{
	  ipa_record_return_value_range (return_range);
	  if (POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl)))
	      && return_range.nonzero_p ()
	      && cgraph_node::get (current_function_decl)
			->add_detected_attribute ("returns_nonnull"))
	    warn_function_returns_nonnull (current_function_decl);
	}
    }

  phi_analysis_finalize ();
  disable_ranger (fun);
  scev_finalize ();
  loop_optimizer_finalize ();
  return 0;
}

// Implement a Fast VRP folder.  Not quite as effective but faster.

class fvrp_folder : public substitute_and_fold_engine
{
public:
  fvrp_folder (dom_ranger *dr) : substitute_and_fold_engine (),
				 m_simplifier (dr)
  { m_dom_ranger = dr; }

  ~fvrp_folder () { }

  tree value_of_expr (tree name, gimple *s = NULL) override
  {
    // Shortcircuit subst_and_fold callbacks for abnormal ssa_names.
    if (TREE_CODE (name) == SSA_NAME && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name))
      return NULL;
    return m_dom_ranger->value_of_expr (name, s);
  }

  tree value_on_edge (edge e, tree name) override
  {
    // Shortcircuit subst_and_fold callbacks for abnormal ssa_names.
    if (TREE_CODE (name) == SSA_NAME && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name))
      return NULL;
    return m_dom_ranger->value_on_edge (e, name);
  }

  tree value_of_stmt (gimple *s, tree name = NULL) override
  {
    // Shortcircuit subst_and_fold callbacks for abnormal ssa_names.
    if (TREE_CODE (name) == SSA_NAME && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name))
      return NULL;
    return m_dom_ranger->value_of_stmt (s, name);
  }

  void pre_fold_bb (basic_block bb) override
  {
    m_dom_ranger->pre_bb (bb);
    // Now process the PHIs in advance.
    gphi_iterator psi = gsi_start_phis (bb);
    for ( ; !gsi_end_p (psi); gsi_next (&psi))
      {
	tree name = gimple_range_ssa_p (PHI_RESULT (psi.phi ()));
	if (name)
	  {
	    Value_Range vr(TREE_TYPE (name));
	    m_dom_ranger->range_of_stmt (vr, psi.phi (), name);
	  }
      }
  }

  void post_fold_bb (basic_block bb) override
  {
    m_dom_ranger->post_bb (bb);
  }

  void pre_fold_stmt (gimple *s) override
  {
    // Ensure range_of_stmt has been called.
    tree type = gimple_range_type (s);
    if (type)
      {
	Value_Range vr(type);
	m_dom_ranger->range_of_stmt (vr, s);
      }
  }

  bool fold_stmt (gimple_stmt_iterator *gsi) override
  {
    bool ret = m_simplifier.simplify (gsi);
    if (!ret)
      ret = ::fold_stmt (gsi, follow_single_use_edges);
    return ret;
  }

private:
  DISABLE_COPY_AND_ASSIGN (fvrp_folder);
  simplify_using_ranges m_simplifier;
  dom_ranger *m_dom_ranger;
};


// Main entry point for a FAST VRP pass using a dom ranger.

unsigned int
execute_fast_vrp (struct function *fun)
{
  calculate_dominance_info (CDI_DOMINATORS);
  dom_ranger dr;
  fvrp_folder folder (&dr);

  gcc_checking_assert (!fun->x_range_query);
  fun->x_range_query = &dr;

  folder.substitute_and_fold ();

  fun->x_range_query = NULL;
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

const pass_data pass_data_fast_vrp =
{
  GIMPLE_PASS, /* type */
  "fvrp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_FAST_VRP, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg | TODO_update_ssa | TODO_verify_all ),
};


class pass_vrp : public gimple_opt_pass
{
public:
  pass_vrp (gcc::context *ctxt, const pass_data &data_, bool warn_p)
    : gimple_opt_pass (data_, ctxt), data (data_),
      warn_array_bounds_p (warn_p), final_p (false)
    { }

  /* opt_pass methods: */
  opt_pass * clone () final override
    { return new pass_vrp (m_ctxt, data, false); }
  void set_pass_param (unsigned int n, bool param) final override
    {
      gcc_assert (n == 0);
      final_p = param;
    }
  bool gate (function *) final override { return flag_tree_vrp != 0; }
  unsigned int execute (function *fun) final override
    {
      // Check for fast vrp.
      if (&data == &pass_data_fast_vrp)
	return execute_fast_vrp (fun);

      return execute_ranger_vrp (fun, warn_array_bounds_p, final_p);
    }

 private:
  const pass_data &data;
  bool warn_array_bounds_p;
  bool final_p;
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
  return new pass_vrp (ctxt, pass_data_vrp, true);
}

gimple_opt_pass *
make_pass_early_vrp (gcc::context *ctxt)
{
  return new pass_vrp (ctxt, pass_data_early_vrp, false);
}

gimple_opt_pass *
make_pass_fast_vrp (gcc::context *ctxt)
{
  return new pass_vrp (ctxt, pass_data_fast_vrp, false);
}

gimple_opt_pass *
make_pass_assumptions (gcc::context *ctx)
{
  return new pass_assumptions (ctx);
}
