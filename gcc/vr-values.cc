/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2005-2024 Free Software Foundation, Inc.

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
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "fold-const.h"
#include "calls.h"
#include "cfganal.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "intl.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-propagate.h"
#include "tree-chrec.h"
#include "omp-general.h"
#include "case-cfn-macros.h"
#include "alloc-pool.h"
#include "attribs.h"
#include "range.h"
#include "vr-values.h"
#include "cfghooks.h"
#include "range-op.h"
#include "gimple-range.h"

/* Return true if op is in a boolean [0, 1] value-range.  */

bool
simplify_using_ranges::op_with_boolean_value_range_p (tree op, gimple *s)
{
  if (TYPE_PRECISION (TREE_TYPE (op)) == 1)
    return true;

  if (integer_zerop (op)
      || integer_onep (op))
    return true;

  if (TREE_CODE (op) != SSA_NAME)
    return false;

  /* ?? Errr, this should probably check for [0,0] and [1,1] as well
     as [0,1].  */
  int_range_max vr;
  return (query->range_of_expr (vr, op, s)
	  && vr == range_true_and_false (TREE_TYPE (op)));
}

/* Helper function for simplify_internal_call_using_ranges and
   extract_range_basic.  Return true if OP0 SUBCODE OP1 for
   SUBCODE {PLUS,MINUS,MULT}_EXPR is known to never overflow or
   always overflow.  Set *OVF to true if it is known to always
   overflow.  */

static bool
check_for_binary_op_overflow (range_query *query,
			      enum tree_code subcode, tree type,
			      tree op0, tree op1, bool *ovf, gimple *s = NULL)
{
  int_range_max vr0, vr1;
  if (!query->range_of_expr (vr0, op0, s) || vr0.undefined_p ())
    vr0.set_varying (TREE_TYPE (op0));
  if (!query->range_of_expr (vr1, op1, s) || vr1.undefined_p ())
    vr1.set_varying (TREE_TYPE (op1));

  tree vr0min = wide_int_to_tree (TREE_TYPE (op0), vr0.lower_bound ());
  tree vr0max = wide_int_to_tree (TREE_TYPE (op0), vr0.upper_bound ());
  tree vr1min = wide_int_to_tree (TREE_TYPE (op1), vr1.lower_bound ());
  tree vr1max = wide_int_to_tree (TREE_TYPE (op1), vr1.upper_bound ());

  *ovf = arith_overflowed_p (subcode, type, vr0min,
			     subcode == MINUS_EXPR ? vr1max : vr1min);
  if (arith_overflowed_p (subcode, type, vr0max,
			  subcode == MINUS_EXPR ? vr1min : vr1max) != *ovf)
    return false;
  if (subcode == MULT_EXPR)
    {
      if (arith_overflowed_p (subcode, type, vr0min, vr1max) != *ovf
	  || arith_overflowed_p (subcode, type, vr0max, vr1min) != *ovf)
	return false;
    }
  if (*ovf)
    {
      /* So far we found that there is an overflow on the boundaries.
	 That doesn't prove that there is an overflow even for all values
	 in between the boundaries.  For that compute widest2_int range
	 of the result and see if it doesn't overlap the range of
	 type.  */
      widest2_int wmin, wmax;
      widest2_int w[4];
      int i;
      signop sign0 = TYPE_SIGN (TREE_TYPE (op0));
      signop sign1 = TYPE_SIGN (TREE_TYPE (op1));
      w[0] = widest2_int::from (vr0.lower_bound (), sign0);
      w[1] = widest2_int::from (vr0.upper_bound (), sign0);
      w[2] = widest2_int::from (vr1.lower_bound (), sign1);
      w[3] = widest2_int::from (vr1.upper_bound (), sign1);
      for (i = 0; i < 4; i++)
	{
	  widest2_int wt;
	  switch (subcode)
	    {
	    case PLUS_EXPR:
	      wt = wi::add (w[i & 1], w[2 + (i & 2) / 2]);
	      break;
	    case MINUS_EXPR:
	      wt = wi::sub (w[i & 1], w[2 + (i & 2) / 2]);
	      break;
	    case MULT_EXPR:
	      wt = wi::mul (w[i & 1], w[2 + (i & 2) / 2]);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  if (i == 0)
	    {
	      wmin = wt;
	      wmax = wt;
	    }
	  else
	    {
	      wmin = wi::smin (wmin, wt);
	      wmax = wi::smax (wmax, wt);
	    }
	}
      /* The result of op0 CODE op1 is known to be in range
	 [wmin, wmax].  */
      widest2_int wtmin
	= widest2_int::from (irange_val_min (type), TYPE_SIGN (type));
      widest2_int wtmax
	= widest2_int::from (irange_val_max (type), TYPE_SIGN (type));
      /* If all values in [wmin, wmax] are smaller than
	 [wtmin, wtmax] or all are larger than [wtmin, wtmax],
	 the arithmetic operation will always overflow.  */
      if (wmax < wtmin || wmin > wtmax)
	return true;
      return false;
    }
  return true;
}

/* Set INIT, STEP, and DIRECTION to the corresponding values of NAME
   within LOOP, and return TRUE.  Otherwise return FALSE, and set R to
   the conservative range of NAME within the loop.  */

static bool
get_scev_info (vrange &r, tree name, gimple *stmt, class loop *l,
	       tree &init, tree &step, enum ev_direction &dir)
{
  tree ev = analyze_scalar_evolution (l, name);
  tree chrec = instantiate_parameters (l, ev);
  tree type = TREE_TYPE (name);
  if (TREE_CODE (chrec) != POLYNOMIAL_CHREC)
    {
      r.set_varying (type);
      return false;
    }
  if (is_gimple_min_invariant (chrec))
    {
      if (is_gimple_constant (chrec))
	r.set (chrec, chrec);
      else
	r.set_varying (type);
      return false;
    }

  init = initial_condition_in_loop_num (chrec, l->num);
  step = evolution_part_in_loop_num (chrec, l->num);
  if (!init || !step)
    {
      r.set_varying (type);
      return false;
    }
  dir = scev_direction (chrec);
  if (dir == EV_DIR_UNKNOWN
      || scev_probably_wraps_p (NULL, init, step, stmt,
				get_chrec_loop (chrec), true))
    {
      r.set_varying (type);
      return false;
    }
  return true;
}

/* Return TRUE if STEP * NIT may overflow when calculated in TYPE.  */

static bool
induction_variable_may_overflow_p (tree type,
				   const wide_int &step, const widest_int &nit)
{
  wi::overflow_type ovf;
  signop sign = TYPE_SIGN (type);
  widest_int max_step = wi::mul (widest_int::from (step, sign),
				 nit, sign, &ovf);

  if (ovf || !wi::fits_to_tree_p (max_step, type))
    return true;

  /* For a signed type we have to check whether the result has the
     expected signedness which is that of the step as number of
     iterations is unsigned.  */
  return (sign == SIGNED
	  && wi::gts_p (max_step, 0) != wi::gts_p (step, 0));
}

/* Set R to the range from BEGIN to END, assuming the direction of the
   loop is DIR.  */

static void
range_from_loop_direction (irange &r, tree type,
			   const irange &begin, const irange &end,
			   ev_direction dir)
{
  signop sign = TYPE_SIGN (type);

  if (begin.undefined_p () || end.undefined_p ())
    r.set_varying (type);
  else if (dir == EV_DIR_GROWS)
    {
      if (wi::gt_p (begin.lower_bound (), end.upper_bound (), sign))
	r.set_varying (type);
      else
	r = int_range<1> (type, begin.lower_bound (), end.upper_bound ());
    }
  else
    {
      if (wi::gt_p (end.lower_bound (), begin.upper_bound (), sign))
	r.set_varying (type);
      else
	r = int_range<1> (type, end.lower_bound (), begin.upper_bound ());
    }
}

/* Set V to the range of NAME in STMT within LOOP.  Return TRUE if a
   range was found.  */

bool
range_of_var_in_loop (vrange &v, tree name, class loop *l, gimple *stmt,
		      range_query *query)
{
  tree init, step;
  enum ev_direction dir;
  if (!get_scev_info (v, name, stmt, l, init, step, dir))
    return true;

  // Calculate ranges for the values from SCEV.
  irange &r = as_a <irange> (v);
  tree type = TREE_TYPE (init);
  int_range<2> rinit (type), rstep (type), max_init (type);
  if (!query->range_of_expr (rinit, init, stmt)
      || !query->range_of_expr (rstep, step, stmt))
    return false;

  // Calculate the final range of NAME if possible.
  if (rinit.singleton_p () && rstep.singleton_p ())
    {
      widest_int nit;
      if (!max_loop_iterations (l, &nit))
	return false;

      if (!induction_variable_may_overflow_p (type, rstep.lower_bound (), nit))
	{
	  // Calculate the max bounds for init (init + niter * step).
	  wide_int w = wide_int::from (nit, TYPE_PRECISION (type), TYPE_SIGN (type));
	  int_range<1> niter (type, w, w);
	  int_range_max max_step;
	  range_op_handler mult_handler (MULT_EXPR);
	  range_op_handler plus_handler (PLUS_EXPR);
	  if (!mult_handler.fold_range (max_step, type, niter, rstep)
	      || !plus_handler.fold_range (max_init, type, rinit, max_step))
	    return false;
	}
    }
  range_from_loop_direction (r, type, rinit, max_init, dir);
  return true;
}

/* Helper function for vrp_evaluate_conditional_warnv & other
   optimizers.  */

tree
simplify_using_ranges::fold_cond_with_ops (enum tree_code code,
					   tree op0, tree op1, gimple *s)
{
  value_range r0 (TREE_TYPE (op0));
  value_range r1 (TREE_TYPE (op1));
  if (!query->range_of_expr (r0, op0, s)
      || !query->range_of_expr (r1, op1, s))
    return NULL_TREE;

  int_range<1> res;
  range_op_handler handler (code);
  if (handler && handler.fold_range (res, boolean_type_node, r0, r1))
    {
      if (res == range_true ())
	return boolean_true_node;
      if (res == range_false ())
	return boolean_false_node;
    }
  return NULL;
}

/* Helper function for legacy_fold_cond.  */

tree
simplify_using_ranges::legacy_fold_cond_overflow (gimple *stmt)
{
  tree ret;
  tree_code code = gimple_cond_code (stmt);
  tree op0 = gimple_cond_lhs (stmt);
  tree op1 = gimple_cond_rhs (stmt);

  /* We only deal with integral and pointer types.  */
  if (!INTEGRAL_TYPE_P (TREE_TYPE (op0))
      && !POINTER_TYPE_P (TREE_TYPE (op0)))
    return NULL_TREE;

  /* If OP0 CODE OP1 is an overflow comparison, if it can be expressed
     as a simple equality test, then prefer that over its current form
     for evaluation.

     An overflow test which collapses to an equality test can always be
     expressed as a comparison of one argument against zero.  Overflow
     occurs when the chosen argument is zero and does not occur if the
     chosen argument is not zero.  */
  tree x;
  if (overflow_comparison_p (code, op0, op1, &x))
    {
      wide_int max = wi::max_value (TYPE_PRECISION (TREE_TYPE (op0)), UNSIGNED);
      /* B = A - 1; if (A < B) -> B = A - 1; if (A == 0)
         B = A - 1; if (A > B) -> B = A - 1; if (A != 0)
         B = A + 1; if (B < A) -> B = A + 1; if (B == 0)
         B = A + 1; if (B > A) -> B = A + 1; if (B != 0) */
      if (integer_zerop (x))
	{
	  op1 = x;
	  code = (code == LT_EXPR || code == LE_EXPR) ? EQ_EXPR : NE_EXPR;
	}
      /* B = A + 1; if (A > B) -> B = A + 1; if (B == 0)
         B = A + 1; if (A < B) -> B = A + 1; if (B != 0)
         B = A - 1; if (B > A) -> B = A - 1; if (A == 0)
         B = A - 1; if (B < A) -> B = A - 1; if (A != 0) */
      else if (wi::to_wide (x) == max - 1)
	{
	  op0 = op1;
	  op1 = wide_int_to_tree (TREE_TYPE (op0), 0);
	  code = (code == GT_EXPR || code == GE_EXPR) ? EQ_EXPR : NE_EXPR;
	}
      else
	{
	  int_range_max vro, vri;
	  tree type = TREE_TYPE (op0);
	  if (code == GT_EXPR || code == GE_EXPR)
	    {
	      vro.set (type,
		       wi::to_wide (TYPE_MIN_VALUE (type)),
		       wi::to_wide (x), VR_ANTI_RANGE);
	      vri.set (type,
		       wi::to_wide (TYPE_MIN_VALUE (type)),
		       wi::to_wide (x));
	    }
	  else if (code == LT_EXPR || code == LE_EXPR)
	    {
	      vro.set (type,
		       wi::to_wide (TYPE_MIN_VALUE (type)),
		       wi::to_wide (x));
	      vri.set (type,
		       wi::to_wide (TYPE_MIN_VALUE (type)),
		       wi::to_wide (x),
		       VR_ANTI_RANGE);
	    }
	  else
	    gcc_unreachable ();
	  int_range_max vr0;
	  if (!query->range_of_expr (vr0, op0, stmt))
	    vr0.set_varying (TREE_TYPE (op0));
	  /* If vro, the range for OP0 to pass the overflow test, has
	     no intersection with *vr0, OP0's known range, then the
	     overflow test can't pass, so return the node for false.
	     If it is the inverted range, vri, that has no
	     intersection, then the overflow test must pass, so return
	     the node for true.  In other cases, we could proceed with
	     a simplified condition comparing OP0 and X, with LE_EXPR
	     for previously LE_ or LT_EXPR and GT_EXPR otherwise, but
	     the comments next to the enclosing if suggest it's not
	     generally profitable to do so.  */
	  vro.intersect (vr0);
	  if (vro.undefined_p ())
	    return boolean_false_node;
	  vri.intersect (vr0);
	  if (vri.undefined_p ())
	    return boolean_true_node;
	}
    }

  if ((ret = fold_cond_with_ops (code, op0, op1, stmt)))
    return ret;
  return NULL_TREE;
}

/* Visit conditional statement STMT.  If we can determine which edge
   will be taken out of STMT's basic block, record it in
   *TAKEN_EDGE_P.  Otherwise, set *TAKEN_EDGE_P to NULL.  */

void
simplify_using_ranges::legacy_fold_cond (gcond *stmt, edge *taken_edge_p)
{
  tree val;

  *taken_edge_p = NULL;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      tree use;
      ssa_op_iter i;

      fprintf (dump_file, "\nVisiting conditional with predicate: ");
      print_gimple_stmt (dump_file, stmt, 0);
      fprintf (dump_file, "\nWith known ranges\n");

      FOR_EACH_SSA_TREE_OPERAND (use, stmt, i, SSA_OP_USE)
	{
	  fprintf (dump_file, "\t");
	  print_generic_expr (dump_file, use);
	  fprintf (dump_file, ": ");
	  value_range r (TREE_TYPE (use));
	  query->range_of_expr (r, use, stmt);
	  r.dump (dump_file);
	}

      fprintf (dump_file, "\n");
    }

  val = legacy_fold_cond_overflow (stmt);
  if (val)
    *taken_edge_p = find_taken_edge (gimple_bb (stmt), val);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nPredicate evaluates to: ");
      if (val == NULL_TREE)
	fprintf (dump_file, "DON'T KNOW\n");
      else
	print_generic_stmt (dump_file, val);
    }
}

/* Searches the case label vector VEC for the ranges of CASE_LABELs that are
   used in range VR.  The indices are placed in MIN_IDX1, MAX_IDX, MIN_IDX2 and
   MAX_IDX2.  If the ranges of CASE_LABELs are empty then MAX_IDX1 < MIN_IDX1.
   Returns true if the default label is not needed.  */

static bool
find_case_label_ranges (gswitch *stmt, const irange *vr,
			size_t *min_idx1, size_t *max_idx1,
			size_t *min_idx2, size_t *max_idx2)
{
  size_t i, j, k, l;
  unsigned int n = gimple_switch_num_labels (stmt);
  bool take_default;
  tree case_low, case_high;
  tree min, max;
  value_range_kind kind = get_legacy_range (*vr, min, max);

  gcc_checking_assert (!vr->varying_p () && !vr->undefined_p ());

  take_default = !find_case_label_range (stmt, min, max, &i, &j);

  /* Set second range to empty.  */
  *min_idx2 = 1;
  *max_idx2 = 0;

  if (kind == VR_RANGE)
    {
      *min_idx1 = i;
      *max_idx1 = j;
      return !take_default;
    }

  /* Set first range to all case labels.  */
  *min_idx1 = 1;
  *max_idx1 = n - 1;

  if (i > j)
    return false;

  /* Make sure all the values of case labels [i , j] are contained in
     range [MIN, MAX].  */
  case_low = CASE_LOW (gimple_switch_label (stmt, i));
  case_high = CASE_HIGH (gimple_switch_label (stmt, j));
  if (tree_int_cst_compare (case_low, min) < 0)
    i += 1;
  if (case_high != NULL_TREE
      && tree_int_cst_compare (max, case_high) < 0)
    j -= 1;

  if (i > j)
    return false;

  /* If the range spans case labels [i, j], the corresponding anti-range spans
     the labels [1, i - 1] and [j + 1, n -  1].  */
  k = j + 1;
  l = n - 1;
  if (k > l)
    {
      k = 1;
      l = 0;
    }

  j = i - 1;
  i = 1;
  if (i > j)
    {
      i = k;
      j = l;
      k = 1;
      l = 0;
    }

  *min_idx1 = i;
  *max_idx1 = j;
  *min_idx2 = k;
  *max_idx2 = l;
  return false;
}

/* Simplify boolean operations if the source is known
   to be already a boolean.  */
bool
simplify_using_ranges::simplify_truth_ops_using_ranges
					(gimple_stmt_iterator *gsi,
					 gimple *stmt)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
  tree lhs, op0, op1;
  bool need_conversion;

  /* We handle only !=/== case here.  */
  gcc_assert (rhs_code == EQ_EXPR || rhs_code == NE_EXPR);

  op0 = gimple_assign_rhs1 (stmt);
  if (!op_with_boolean_value_range_p (op0, stmt))
    return false;

  op1 = gimple_assign_rhs2 (stmt);
  if (!op_with_boolean_value_range_p (op1, stmt))
    return false;

  /* Reduce number of cases to handle to NE_EXPR.  As there is no
     BIT_XNOR_EXPR we cannot replace A == B with a single statement.  */
  if (rhs_code == EQ_EXPR)
    {
      if (TREE_CODE (op1) == INTEGER_CST)
	op1 = int_const_binop (BIT_XOR_EXPR, op1,
			       build_int_cst (TREE_TYPE (op1), 1));
      else
	return false;
    }

  lhs = gimple_assign_lhs (stmt);
  need_conversion
    = !useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (op0));

  /* Make sure to not sign-extend a 1-bit 1 when converting the result.  */
  if (need_conversion
      && !TYPE_UNSIGNED (TREE_TYPE (op0))
      && TYPE_PRECISION (TREE_TYPE (op0)) == 1
      && TYPE_PRECISION (TREE_TYPE (lhs)) > 1)
    return false;

  /* For A != 0 we can substitute A itself.  */
  if (integer_zerop (op1))
    gimple_assign_set_rhs_with_ops (gsi,
				    need_conversion
				    ? NOP_EXPR : TREE_CODE (op0), op0);
  /* For A != B we substitute A ^ B.  Either with conversion.  */
  else if (need_conversion)
    {
      tree tem = make_ssa_name (TREE_TYPE (op0));
      gassign *newop
	= gimple_build_assign (tem, BIT_XOR_EXPR, op0, op1);
      gsi_insert_before (gsi, newop, GSI_SAME_STMT);
      if (INTEGRAL_TYPE_P (TREE_TYPE (tem))
	  && TYPE_PRECISION (TREE_TYPE (tem)) > 1)
	{
	  int_range<1> vr (TREE_TYPE (tem),
			   wi::zero (TYPE_PRECISION (TREE_TYPE (tem))),
			   wi::one (TYPE_PRECISION (TREE_TYPE (tem))));
	  set_range_info (tem, vr);
	}
      gimple_assign_set_rhs_with_ops (gsi, NOP_EXPR, tem);
    }
  /* Or without.  */
  else
    gimple_assign_set_rhs_with_ops (gsi, BIT_XOR_EXPR, op0, op1);
  update_stmt (gsi_stmt (*gsi));
  fold_stmt (gsi, follow_single_use_edges);

  return true;
}

/* Simplify a division or modulo operator to a right shift or bitwise and
   if the first operand is unsigned or is greater than zero and the second
   operand is an exact power of two.  For TRUNC_MOD_EXPR op0 % op1 with
   constant op1 (op1min = op1) or with op1 in [op1min, op1max] range,
   optimize it into just op0 if op0's range is known to be a subset of
   [-op1min + 1, op1min - 1] for signed and [0, op1min - 1] for unsigned
   modulo.  */

bool
simplify_using_ranges::simplify_div_or_mod_using_ranges
					(gimple_stmt_iterator *gsi,
					 gimple *stmt)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
  tree val = NULL;
  tree op0 = gimple_assign_rhs1 (stmt);
  tree op1 = gimple_assign_rhs2 (stmt);
  tree op0min = NULL_TREE, op0max = NULL_TREE;
  tree op1min = op1;
  int_range_max vr;

  if (TREE_CODE (op0) == INTEGER_CST)
    {
      op0min = op0;
      op0max = op0;
    }
  else
    {
      if (!query->range_of_expr (vr, op0, stmt))
	vr.set_varying (TREE_TYPE (op0));
      if (!vr.varying_p () && !vr.undefined_p ())
	{
	  tree type = vr.type ();
	  op0min = wide_int_to_tree (type, vr.lower_bound ());
	  op0max = wide_int_to_tree (type, vr.upper_bound ());
	}
    }

  if (rhs_code == TRUNC_MOD_EXPR
      && TREE_CODE (op1) == SSA_NAME)
    {
      int_range_max vr1;
      if (!query->range_of_expr (vr1, op1, stmt))
	vr1.set_varying (TREE_TYPE (op1));
      if (!vr1.varying_p () && !vr1.undefined_p ())
	op1min = wide_int_to_tree (vr1.type (), vr1.lower_bound ());
    }
  if (rhs_code == TRUNC_MOD_EXPR
      && TREE_CODE (op1min) == INTEGER_CST
      && tree_int_cst_sgn (op1min) == 1
      && op0max
      && tree_int_cst_lt (op0max, op1min))
    {
      if (TYPE_UNSIGNED (TREE_TYPE (op0))
	  || tree_int_cst_sgn (op0min) >= 0
	  || tree_int_cst_lt (fold_unary (NEGATE_EXPR, TREE_TYPE (op1min), op1min),
			      op0min))
	{
	  /* If op0 already has the range op0 % op1 has,
	     then TRUNC_MOD_EXPR won't change anything.  */
	  gimple_assign_set_rhs_from_tree (gsi, op0);
	  return true;
	}
    }

  if (TREE_CODE (op0) != SSA_NAME)
    return false;

  if (!integer_pow2p (op1))
    {
      /* X % -Y can be only optimized into X % Y either if
	 X is not INT_MIN, or Y is not -1.  Fold it now, as after
	 remove_range_assertions the range info might be not available
	 anymore.  */
      if (rhs_code == TRUNC_MOD_EXPR
	  && fold_stmt (gsi, follow_single_use_edges))
	return true;
      return false;
    }

  if (TYPE_UNSIGNED (TREE_TYPE (op0)))
    val = integer_one_node;
  else
    {
      tree zero = build_zero_cst (TREE_TYPE (op0));
      val = fold_cond_with_ops (GE_EXPR, op0, zero, stmt);
    }

  if (val && integer_onep (val))
    {
      tree t;

      if (rhs_code == TRUNC_DIV_EXPR)
	{
	  t = build_int_cst (integer_type_node, tree_log2 (op1));
	  gimple_assign_set_rhs_code (stmt, RSHIFT_EXPR);
	  gimple_assign_set_rhs1 (stmt, op0);
	  gimple_assign_set_rhs2 (stmt, t);
	}
      else
	{
	  t = build_int_cst (TREE_TYPE (op1), 1);
	  t = int_const_binop (MINUS_EXPR, op1, t);
	  t = fold_convert (TREE_TYPE (op0), t);

	  gimple_assign_set_rhs_code (stmt, BIT_AND_EXPR);
	  gimple_assign_set_rhs1 (stmt, op0);
	  gimple_assign_set_rhs2 (stmt, t);
	}

      update_stmt (stmt);
      fold_stmt (gsi, follow_single_use_edges);
      return true;
    }

  return false;
}

/* Simplify a min or max if the ranges of the two operands are
   disjoint.   Return true if we do simplify.  */

bool
simplify_using_ranges::simplify_min_or_max_using_ranges
				(gimple_stmt_iterator *gsi,
				 gimple *stmt)
{
  tree op0 = gimple_assign_rhs1 (stmt);
  tree op1 = gimple_assign_rhs2 (stmt);
  tree val;

  val = fold_cond_with_ops (LE_EXPR, op0, op1, stmt);
  if (!val)
    val = fold_cond_with_ops (LT_EXPR, op0, op1, stmt);

  if (val)
    {
      /* VAL == TRUE -> OP0 < or <= op1
	 VAL == FALSE -> OP0 > or >= op1.  */
      tree res = ((gimple_assign_rhs_code (stmt) == MAX_EXPR)
		  == integer_zerop (val)) ? op0 : op1;
      gimple_assign_set_rhs_from_tree (gsi, res);
      return true;
    }

  return false;
}

/* If the operand to an ABS_EXPR is >= 0, then eliminate the
   ABS_EXPR.  If the operand is <= 0, then simplify the
   ABS_EXPR into a NEGATE_EXPR.  */

bool
simplify_using_ranges::simplify_abs_using_ranges (gimple_stmt_iterator *gsi,
						  gimple *stmt)
{
  tree op = gimple_assign_rhs1 (stmt);
  tree zero = build_zero_cst (TREE_TYPE (op));
  tree val = fold_cond_with_ops (LE_EXPR, op, zero, stmt);

  if (!val)
    {
      /* The range is neither <= 0 nor > 0.  Now see if it is
	 either < 0 or >= 0.  */
      val = fold_cond_with_ops (LT_EXPR, op, zero, stmt);
    }
  if (val)
    {
      gimple_assign_set_rhs1 (stmt, op);
      if (integer_zerop (val))
	gimple_assign_set_rhs_code (stmt, SSA_NAME);
      else
	gimple_assign_set_rhs_code (stmt, NEGATE_EXPR);
      update_stmt (stmt);
      fold_stmt (gsi, follow_single_use_edges);
      return true;
    }
  return false;
}

/* irange wrapper for wi_set_zero_nonzero_bits.

   Return TRUE if VR was a constant range and we were able to compute
   the bit masks.  */

static bool
vr_set_zero_nonzero_bits (const tree expr_type,
			  const irange *vr,
			  wide_int *may_be_nonzero,
			  wide_int *must_be_nonzero)
{
  if (vr->varying_p () || vr->undefined_p ())
    {
      *may_be_nonzero = wi::minus_one (TYPE_PRECISION (expr_type));
      *must_be_nonzero = wi::zero (TYPE_PRECISION (expr_type));
      return false;
    }
  wi_set_zero_nonzero_bits (expr_type, vr->lower_bound (), vr->upper_bound (),
			    *may_be_nonzero, *must_be_nonzero);
  return true;
}

/* Optimize away redundant BIT_AND_EXPR and BIT_IOR_EXPR.
   If all the bits that are being cleared by & are already
   known to be zero from VR, or all the bits that are being
   set by | are already known to be one from VR, the bit
   operation is redundant.  */

bool
simplify_using_ranges::simplify_bit_ops_using_ranges
				(gimple_stmt_iterator *gsi,
				 gimple *stmt)
{
  tree op0 = gimple_assign_rhs1 (stmt);
  tree op1 = gimple_assign_rhs2 (stmt);
  tree op = NULL_TREE;
  int_range_max vr0, vr1;
  wide_int may_be_nonzero0, may_be_nonzero1;
  wide_int must_be_nonzero0, must_be_nonzero1;
  wide_int mask;

  if (!query->range_of_expr (vr0, op0, stmt)
      || vr0.undefined_p ())
    return false;
  if (!query->range_of_expr (vr1, op1, stmt)
      || vr1.undefined_p ())
    return false;

  if (!vr_set_zero_nonzero_bits (TREE_TYPE (op0), &vr0, &may_be_nonzero0,
				 &must_be_nonzero0))
    return false;
  if (!vr_set_zero_nonzero_bits (TREE_TYPE (op1), &vr1, &may_be_nonzero1,
				 &must_be_nonzero1))
    return false;

  switch (gimple_assign_rhs_code (stmt))
    {
    case BIT_AND_EXPR:
      mask = wi::bit_and_not (may_be_nonzero0, must_be_nonzero1);
      if (mask == 0)
	{
	  op = op0;
	  break;
	}
      mask = wi::bit_and_not (may_be_nonzero1, must_be_nonzero0);
      if (mask == 0)
	{
	  op = op1;
	  break;
	}
      break;
    case BIT_IOR_EXPR:
      mask = wi::bit_and_not (may_be_nonzero0, must_be_nonzero1);
      if (mask == 0)
	{
	  op = op1;
	  break;
	}
      mask = wi::bit_and_not (may_be_nonzero1, must_be_nonzero0);
      if (mask == 0)
	{
	  op = op0;
	  break;
	}
      break;
    default:
      gcc_unreachable ();
    }

  if (op == NULL_TREE)
    return false;

  gimple_assign_set_rhs_with_ops (gsi, TREE_CODE (op), op);
  update_stmt (gsi_stmt (*gsi));
  return true;
}

/* We are comparing trees OP0 and OP1 using COND_CODE.  OP0 has
   a known value range VR.

   If there is one and only one value which will satisfy the
   conditional, then return that value.  Else return NULL.

   If signed overflow must be undefined for the value to satisfy
   the conditional, then set *STRICT_OVERFLOW_P to true.  */

static tree
test_for_singularity (enum tree_code cond_code, tree op0,
		      tree op1, const irange *vr)
{
  tree min = NULL;
  tree max = NULL;

  /* Extract minimum/maximum values which satisfy the conditional as it was
     written.  */
  if (cond_code == LE_EXPR || cond_code == LT_EXPR)
    {
      min = TYPE_MIN_VALUE (TREE_TYPE (op0));

      max = op1;
      if (cond_code == LT_EXPR)
	{
	  tree one = build_int_cst (TREE_TYPE (op0), 1);
	  max = fold_build2 (MINUS_EXPR, TREE_TYPE (op0), max, one);
	  /* Signal to compare_values_warnv this expr doesn't overflow.  */
	  if (EXPR_P (max))
	    suppress_warning (max, OPT_Woverflow);
	}
    }
  else if (cond_code == GE_EXPR || cond_code == GT_EXPR)
    {
      max = TYPE_MAX_VALUE (TREE_TYPE (op0));

      min = op1;
      if (cond_code == GT_EXPR)
	{
	  tree one = build_int_cst (TREE_TYPE (op0), 1);
	  min = fold_build2 (PLUS_EXPR, TREE_TYPE (op0), min, one);
	  /* Signal to compare_values_warnv this expr doesn't overflow.  */
	  if (EXPR_P (min))
	    suppress_warning (min, OPT_Woverflow);
	}
    }

  /* Now refine the minimum and maximum values using any
     value range information we have for op0.  */
  if (min && max)
    {
      tree type = TREE_TYPE (op0);
      tree tmin = wide_int_to_tree (type, vr->lower_bound ());
      tree tmax = wide_int_to_tree (type, vr->upper_bound ());
      if (compare_values (tmin, min) == 1)
	min = tmin;
      if (compare_values (tmax, max) == -1)
	max = tmax;

      /* If the new min/max values have converged to a single value,
	 then there is only one value which can satisfy the condition,
	 return that value.  */
      if (operand_equal_p (min, max, 0) && is_gimple_min_invariant (min))
	return min;
    }
  return NULL;
}

/* Return whether the value range *VR fits in an integer type specified
   by PRECISION and UNSIGNED_P.  */

bool
range_fits_type_p (const irange *vr,
		   unsigned dest_precision, signop dest_sgn)
{
  tree src_type;
  unsigned src_precision;
  widest_int tem;
  signop src_sgn;

  /* We can only handle integral and pointer types.  */
  src_type = vr->type ();
  if (!INTEGRAL_TYPE_P (src_type)
      && !POINTER_TYPE_P (src_type))
    return false;

  /* An extension is fine unless VR is SIGNED and dest_sgn is UNSIGNED,
     and so is an identity transform.  */
  src_precision = TYPE_PRECISION (vr->type ());
  src_sgn = TYPE_SIGN (src_type);
  if ((src_precision < dest_precision
       && !(dest_sgn == UNSIGNED && src_sgn == SIGNED))
      || (src_precision == dest_precision && src_sgn == dest_sgn))
    return true;

  /* Now we can only handle ranges with constant bounds.  */
  if (vr->undefined_p () || vr->varying_p ())
    return false;

  wide_int vrmin = vr->lower_bound ();
  wide_int vrmax = vr->upper_bound ();

  /* For sign changes, the MSB of the wide_int has to be clear.
     An unsigned value with its MSB set cannot be represented by
     a signed wide_int, while a negative value cannot be represented
     by an unsigned wide_int.  */
  if (src_sgn != dest_sgn
      && (wi::lts_p (vrmin, 0) || wi::lts_p (vrmax, 0)))
    return false;

  /* Then we can perform the conversion on both ends and compare
     the result for equality.  */
  signop sign = TYPE_SIGN (vr->type ());
  tem = wi::ext (widest_int::from (vrmin, sign), dest_precision, dest_sgn);
  if (tem != widest_int::from (vrmin, sign))
    return false;
  tem = wi::ext (widest_int::from (vrmax, sign), dest_precision, dest_sgn);
  if (tem != widest_int::from (vrmax, sign))
    return false;

  return true;
}

// Clear edge E of EDGE_EXECUTABLE (it is unexecutable). If it wasn't
// previously clear, propagate to successor blocks if appropriate.

void
simplify_using_ranges::set_and_propagate_unexecutable (edge e)
{
  // If not_executable is already set, we're done.
  // This works in the absence of a flag as well.
  if ((e->flags & m_not_executable_flag) == m_not_executable_flag)
    return;

  e->flags |= m_not_executable_flag;
  m_flag_set_edges.safe_push (e);

  // Check if the destination block needs to propagate the property.
  basic_block bb = e->dest;

  // If any incoming edge is executable, we are done.
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->preds)
    if ((e->flags & m_not_executable_flag) == 0)
      return;

  // This block is also unexecutable, propagate to all exit edges as well.
  FOR_EACH_EDGE (e, ei, bb->succs)
    set_and_propagate_unexecutable (e);
}

/* If COND can be folded entirely as TRUE or FALSE, rewrite the
   conditional as such, and return TRUE.  */

bool
simplify_using_ranges::fold_cond (gcond *cond)
{
  int_range_max r;
  if (query->range_of_stmt (r, cond) && r.singleton_p ())
    {
      // COND has already been folded if arguments are constant.
      if (TREE_CODE (gimple_cond_lhs (cond)) != SSA_NAME
	  && TREE_CODE (gimple_cond_rhs (cond)) != SSA_NAME)
	return false;
      if (dump_file)
	{
	  fprintf (dump_file, "Folding predicate ");
	  print_gimple_expr (dump_file, cond, 0);
	  fprintf (dump_file, " to ");
	}
      edge e0 = EDGE_SUCC (gimple_bb (cond), 0);
      edge e1 = EDGE_SUCC (gimple_bb (cond), 1);
      if (r.zero_p ())
	{
	  if (dump_file)
	    fprintf (dump_file, "0\n");
	  gimple_cond_make_false (cond);
	  if (e0->flags & EDGE_TRUE_VALUE)
	    set_and_propagate_unexecutable (e0);
	  else
	    set_and_propagate_unexecutable (e1);
	}
      else
	{
	  if (dump_file)
	    fprintf (dump_file, "1\n");
	  gimple_cond_make_true (cond);
	  if (e0->flags & EDGE_FALSE_VALUE)
	    set_and_propagate_unexecutable (e0);
	  else
	    set_and_propagate_unexecutable (e1);
	}
      update_stmt (cond);
      return true;
    }

  // FIXME: Audit the code below and make sure it never finds anything.
  edge taken_edge;
  legacy_fold_cond (cond, &taken_edge);

  if (taken_edge)
    {
      if (taken_edge->flags & EDGE_TRUE_VALUE)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "\nVRP Predicate evaluates to: 1\n");
	  gimple_cond_make_true (cond);
	}
      else if (taken_edge->flags & EDGE_FALSE_VALUE)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "\nVRP Predicate evaluates to: 0\n");
	  gimple_cond_make_false (cond);
	}
      else
       gcc_unreachable ();
      update_stmt (cond);
      return true;
    }
  return false;
}

/* Simplify a conditional using a relational operator to an equality
   test if the range information indicates only one value can satisfy
   the original conditional.  */

bool
simplify_using_ranges::simplify_cond_using_ranges_1 (gcond *stmt)
{
  tree op0 = gimple_cond_lhs (stmt);
  tree op1 = gimple_cond_rhs (stmt);
  enum tree_code cond_code = gimple_cond_code (stmt);

  if (fold_cond (stmt))
    return true;

  if (simplify_compare_using_ranges_1 (cond_code, op0, op1, stmt))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "Simplified relational ");
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, " into ");
	}

      gimple_cond_set_code (stmt, cond_code);
      gimple_cond_set_lhs (stmt, op0);
      gimple_cond_set_rhs (stmt, op1);

      update_stmt (stmt);

       if (dump_file)
	{
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "\n");
	}
      return true;
    }
  return false;
}

/* Like simplify_cond_using_ranges_1 but for assignments rather
   than GIMPLE_COND. */

bool
simplify_using_ranges::simplify_compare_assign_using_ranges_1
					(gimple_stmt_iterator *gsi,
					 gimple *stmt)
{
  enum tree_code code = gimple_assign_rhs_code (stmt);
  tree op0 = gimple_assign_rhs1 (stmt);
  tree op1 = gimple_assign_rhs2 (stmt);
  gcc_assert (TREE_CODE_CLASS (code) == tcc_comparison);
  bool happened = false;

  if (simplify_compare_using_ranges_1 (code, op0, op1, stmt))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "Simplified relational ");
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, " into ");
	}

      gimple_assign_set_rhs_code (stmt, code);
      gimple_assign_set_rhs1 (stmt, op0);
      gimple_assign_set_rhs2 (stmt, op1);

      update_stmt (stmt);

       if (dump_file)
	{
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "\n");
	}
      happened = true;
    }

  /* Transform EQ_EXPR, NE_EXPR into BIT_XOR_EXPR or identity
     if the RHS is zero or one, and the LHS are known to be boolean
     values.  */
  if ((code == EQ_EXPR || code == NE_EXPR)
      && INTEGRAL_TYPE_P (TREE_TYPE (op0))
      && simplify_truth_ops_using_ranges (gsi, stmt))
    happened = true;

  return happened;
}

/* Try to simplify OP0 COND_CODE OP1 using a relational operator to an
   equality test if the range information indicates only one value can
   satisfy the original conditional.   */

bool
simplify_using_ranges::simplify_compare_using_ranges_1 (tree_code &cond_code, tree &op0, tree &op1, gimple *stmt)
{
  bool happened = false;
  if (cond_code != NE_EXPR
      && cond_code != EQ_EXPR
      && TREE_CODE (op0) == SSA_NAME
      && INTEGRAL_TYPE_P (TREE_TYPE (op0))
      && is_gimple_min_invariant (op1))
    {
      int_range_max vr;

      if (!query->range_of_expr (vr, op0, stmt))
	vr.set_undefined ();

      /* If we have range information for OP0, then we might be
	 able to simplify this conditional. */
      if (!vr.undefined_p () && !vr.varying_p ())
	{
	  tree new_tree = test_for_singularity (cond_code, op0, op1, &vr);
	  if (new_tree)
	    {
	      cond_code = EQ_EXPR;
	      op1 = new_tree;
	      happened = true;
	    }

	  /* Try again after inverting the condition.  We only deal
	     with integral types here, so no need to worry about
	     issues with inverting FP comparisons.  */
	  new_tree = test_for_singularity
		       (invert_tree_comparison (cond_code, false),
			op0, op1, &vr);
	  if (new_tree)
	    {
	      cond_code = NE_EXPR;
	      op1 = new_tree;
	      happened = true;
	    }
	}
    }
  // Try to simplify casted conditions.
  if (simplify_casted_compare (cond_code, op0, op1))
    happened = true;
  return happened;
}

/* Simplify OP0 code OP1 when OP1 is a constant and OP0 was a SSA_NAME
   defined by a type conversion. Replacing OP0 with RHS of the type conversion.
   Doing so makes the conversion dead which helps subsequent passes.  */

bool
simplify_using_ranges::simplify_casted_compare (tree_code &, tree &op0, tree &op1)
{

  /* If we have a comparison of an SSA_NAME (OP0) against a constant,
     see if OP0 was set by a type conversion where the source of
     the conversion is another SSA_NAME with a range that fits
     into the range of OP0's type.

     If so, the conversion is redundant as the earlier SSA_NAME can be
     used for the comparison directly if we just massage the constant in the
     comparison.  */
  if (TREE_CODE (op0) == SSA_NAME
      && TREE_CODE (op1) == INTEGER_CST)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (op0);
      tree innerop;

      if (!is_gimple_assign (def_stmt))
	return false;

      switch (gimple_assign_rhs_code (def_stmt))
	{
	CASE_CONVERT:
	  innerop = gimple_assign_rhs1 (def_stmt);
	  break;
	case VIEW_CONVERT_EXPR:
	  innerop = TREE_OPERAND (gimple_assign_rhs1 (def_stmt), 0);
	  if (!INTEGRAL_TYPE_P (TREE_TYPE (innerop)))
	    return false;
	  break;
	default:
	  return false;
	}

      if (TREE_CODE (innerop) == SSA_NAME
	  && !POINTER_TYPE_P (TREE_TYPE (innerop))
	  && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (innerop)
	  && desired_pro_or_demotion_p (TREE_TYPE (innerop), TREE_TYPE (op0)))
	{
	  int_range_max vr;

	  if (query->range_of_expr (vr, innerop)
	      && !vr.varying_p ()
	      && !vr.undefined_p ()
	      && range_fits_type_p (&vr,
				    TYPE_PRECISION (TREE_TYPE (op0)),
				    TYPE_SIGN (TREE_TYPE (op0)))
	      && int_fits_type_p (op1, TREE_TYPE (innerop)))
	    {
	      tree newconst = fold_convert (TREE_TYPE (innerop), op1);
	      op0 = innerop;
	      op1 = newconst;
	      return true;
	    }
	}
    }
  return false;
}

/* Simplify a switch statement using the value range of the switch
   argument.  */

bool
simplify_using_ranges::simplify_switch_using_ranges (gswitch *stmt)
{
  tree op = gimple_switch_index (stmt);
  int_range_max vr;
  bool take_default;
  edge e;
  edge_iterator ei;
  size_t i = 0, j = 0, n, n2;
  tree vec2;
  switch_update su;
  size_t k = 1, l = 0;

  if (TREE_CODE (op) == SSA_NAME)
    {
      if (!query->range_of_expr (vr, op, stmt)
	  || vr.varying_p () || vr.undefined_p ())
	return false;

      /* Find case label for min/max of the value range.  */
      take_default = !find_case_label_ranges (stmt, &vr, &i, &j, &k, &l);
    }
  else if (TREE_CODE (op) == INTEGER_CST)
    {
      take_default = !find_case_label_index (stmt, 1, op, &i);
      if (take_default)
	{
	  i = 1;
	  j = 0;
	}
      else
	{
	  j = i;
	}
    }
  else
    return false;

  n = gimple_switch_num_labels (stmt);

  /* We can truncate the case label ranges that partially overlap with OP's
     value range.  */
  size_t min_idx = 1, max_idx = 0;
  tree min, max;
  value_range_kind kind = get_legacy_range (vr, min, max);
  if (!vr.undefined_p ())
    find_case_label_range (stmt, min, max, &min_idx, &max_idx);
  if (min_idx <= max_idx)
    {
      tree min_label = gimple_switch_label (stmt, min_idx);
      tree max_label = gimple_switch_label (stmt, max_idx);

      /* Avoid changing the type of the case labels when truncating.  */
      tree case_label_type = TREE_TYPE (CASE_LOW (min_label));
      tree vr_min = fold_convert (case_label_type, min);
      tree vr_max = fold_convert (case_label_type, max);

      if (kind == VR_RANGE)
	{
	  /* If OP's value range is [2,8] and the low label range is
	     0 ... 3, truncate the label's range to 2 .. 3.  */
	  if (tree_int_cst_compare (CASE_LOW (min_label), vr_min) < 0
	      && CASE_HIGH (min_label) != NULL_TREE
	      && tree_int_cst_compare (CASE_HIGH (min_label), vr_min) >= 0)
	    CASE_LOW (min_label) = vr_min;

	  /* If OP's value range is [2,8] and the high label range is
	     7 ... 10, truncate the label's range to 7 .. 8.  */
	  if (tree_int_cst_compare (CASE_LOW (max_label), vr_max) <= 0
	      && CASE_HIGH (max_label) != NULL_TREE
	      && tree_int_cst_compare (CASE_HIGH (max_label), vr_max) > 0)
	    CASE_HIGH (max_label) = vr_max;
	}
      else if (kind == VR_ANTI_RANGE)
	{
	  tree one_cst = build_one_cst (case_label_type);

	  if (min_label == max_label)
	    {
	      /* If OP's value range is ~[7,8] and the label's range is
		 7 ... 10, truncate the label's range to 9 ... 10.  */
	      if (tree_int_cst_compare (CASE_LOW (min_label), vr_min) == 0
		  && CASE_HIGH (min_label) != NULL_TREE
		  && tree_int_cst_compare (CASE_HIGH (min_label), vr_max) > 0)
		CASE_LOW (min_label)
		  = int_const_binop (PLUS_EXPR, vr_max, one_cst);

	      /* If OP's value range is ~[7,8] and the label's range is
		 5 ... 8, truncate the label's range to 5 ... 6.  */
	      if (tree_int_cst_compare (CASE_LOW (min_label), vr_min) < 0
		  && CASE_HIGH (min_label) != NULL_TREE
		  && tree_int_cst_compare (CASE_HIGH (min_label), vr_max) == 0)
		CASE_HIGH (min_label)
		  = int_const_binop (MINUS_EXPR, vr_min, one_cst);
	    }
	  else
	    {
	      /* If OP's value range is ~[2,8] and the low label range is
		 0 ... 3, truncate the label's range to 0 ... 1.  */
	      if (tree_int_cst_compare (CASE_LOW (min_label), vr_min) < 0
		  && CASE_HIGH (min_label) != NULL_TREE
		  && tree_int_cst_compare (CASE_HIGH (min_label), vr_min) >= 0)
		CASE_HIGH (min_label)
		  = int_const_binop (MINUS_EXPR, vr_min, one_cst);

	      /* If OP's value range is ~[2,8] and the high label range is
		 7 ... 10, truncate the label's range to 9 ... 10.  */
	      if (tree_int_cst_compare (CASE_LOW (max_label), vr_max) <= 0
		  && CASE_HIGH (max_label) != NULL_TREE
		  && tree_int_cst_compare (CASE_HIGH (max_label), vr_max) > 0)
		CASE_LOW (max_label)
		  = int_const_binop (PLUS_EXPR, vr_max, one_cst);
	    }
	}

      /* Canonicalize singleton case ranges.  */
      if (tree_int_cst_equal (CASE_LOW (min_label), CASE_HIGH (min_label)))
	CASE_HIGH (min_label) = NULL_TREE;
      if (tree_int_cst_equal (CASE_LOW (max_label), CASE_HIGH (max_label)))
	CASE_HIGH (max_label) = NULL_TREE;
    }

  /* We can also eliminate case labels that lie completely outside OP's value
     range.  */

  /* Bail out if this is just all edges taken.  */
  if (i == 1
      && j == n - 1
      && take_default)
    return false;

  /* Build a new vector of taken case labels.  */
  vec2 = make_tree_vec (j - i + 1 + l - k + 1 + (int)take_default);
  n2 = 0;

  /* Add the default edge, if necessary.  */
  if (take_default)
    TREE_VEC_ELT (vec2, n2++) = gimple_switch_default_label (stmt);

  for (; i <= j; ++i, ++n2)
    TREE_VEC_ELT (vec2, n2) = gimple_switch_label (stmt, i);

  for (; k <= l; ++k, ++n2)
    TREE_VEC_ELT (vec2, n2) = gimple_switch_label (stmt, k);

  /* Mark needed edges.  */
  for (i = 0; i < n2; ++i)
    {
      e = find_edge (gimple_bb (stmt),
		     label_to_block (cfun,
				     CASE_LABEL (TREE_VEC_ELT (vec2, i))));
      e->aux = (void *)-1;
    }

  /* Queue not needed edges for later removal.  */
  FOR_EACH_EDGE (e, ei, gimple_bb (stmt)->succs)
    {
      if (e->aux == (void *)-1)
	{
	  e->aux = NULL;
	  continue;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "removing unreachable case label\n");
	}
      to_remove_edges.safe_push (e);
      set_and_propagate_unexecutable (e);
      e->flags &= ~EDGE_EXECUTABLE;
      e->flags |= EDGE_IGNORE;
    }

  /* And queue an update for the stmt.  */
  su.stmt = stmt;
  su.vec = vec2;
  to_update_switch_stmts.safe_push (su);
  return true;
}

void
simplify_using_ranges::cleanup_edges_and_switches (void)
{
  int i;
  edge e;
  switch_update *su;

  /* Clear any edges marked as not executable.  */
  if (m_not_executable_flag)
    {
      FOR_EACH_VEC_ELT (m_flag_set_edges, i, e)
	e->flags &= ~m_not_executable_flag;
    }
  /* Remove dead edges from SWITCH_EXPR optimization.  This leaves the
     CFG in a broken state and requires a cfg_cleanup run.  */
  FOR_EACH_VEC_ELT (to_remove_edges, i, e)
    remove_edge (e);

  /* Update SWITCH_EXPR case label vector.  */
  FOR_EACH_VEC_ELT (to_update_switch_stmts, i, su)
    {
      size_t j;
      size_t n = TREE_VEC_LENGTH (su->vec);
      tree label;
      gimple_switch_set_num_labels (su->stmt, n);
      for (j = 0; j < n; j++)
	gimple_switch_set_label (su->stmt, j, TREE_VEC_ELT (su->vec, j));
      /* As we may have replaced the default label with a regular one
	 make sure to make it a real default label again.  This ensures
	 optimal expansion.  */
      label = gimple_switch_label (su->stmt, 0);
      CASE_LOW (label) = NULL_TREE;
      CASE_HIGH (label) = NULL_TREE;
    }

  if (!to_remove_edges.is_empty ())
    {
      free_dominance_info (CDI_DOMINATORS);
      loops_state_set (LOOPS_NEED_FIXUP);
    }

  to_remove_edges.release ();
  to_update_switch_stmts.release ();
}

/* Simplify an integral conversion from an SSA name in STMT.  */

static bool
simplify_conversion_using_ranges (gimple_stmt_iterator *gsi, gimple *stmt)
{
  tree innerop, middleop, finaltype;
  gimple *def_stmt;
  signop inner_sgn, middle_sgn, final_sgn;
  unsigned inner_prec, middle_prec, final_prec;
  widest_int innermin, innermed, innermax, middlemin, middlemed, middlemax;

  finaltype = TREE_TYPE (gimple_assign_lhs (stmt));
  if (!INTEGRAL_TYPE_P (finaltype))
    return false;
  middleop = gimple_assign_rhs1 (stmt);
  def_stmt = SSA_NAME_DEF_STMT (middleop);
  if (!is_gimple_assign (def_stmt)
      || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def_stmt)))
    return false;
  innerop = gimple_assign_rhs1 (def_stmt);
  if (TREE_CODE (innerop) != SSA_NAME
      || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (innerop))
    return false;

  /* Get the value-range of the inner operand.  Use global ranges in
     case innerop was created during substitute-and-fold.  */
  wide_int imin, imax;
  int_range_max vr;
  if (!INTEGRAL_TYPE_P (TREE_TYPE (innerop)))
    return false;
  get_range_query (cfun)->range_of_expr (vr, innerop, stmt);
  if (vr.undefined_p () || vr.varying_p ())
    return false;
  innermin = widest_int::from (vr.lower_bound (), TYPE_SIGN (TREE_TYPE (innerop)));
  innermax = widest_int::from (vr.upper_bound (), TYPE_SIGN (TREE_TYPE (innerop)));

  /* Simulate the conversion chain to check if the result is equal if
     the middle conversion is removed.  */
  inner_prec = TYPE_PRECISION (TREE_TYPE (innerop));
  middle_prec = TYPE_PRECISION (TREE_TYPE (middleop));
  final_prec = TYPE_PRECISION (finaltype);

  /* If the first conversion is not injective, the second must not
     be widening.  */
  if (wi::gtu_p (innermax - innermin,
		 wi::mask <widest_int> (middle_prec, false))
      && middle_prec < final_prec)
    return false;
  /* We also want a medium value so that we can track the effect that
     narrowing conversions with sign change have.  */
  inner_sgn = TYPE_SIGN (TREE_TYPE (innerop));
  if (inner_sgn == UNSIGNED)
    innermed = wi::shifted_mask <widest_int> (1, inner_prec - 1, false);
  else
    innermed = 0;
  if (wi::cmp (innermin, innermed, inner_sgn) >= 0
      || wi::cmp (innermed, innermax, inner_sgn) >= 0)
    innermed = innermin;

  middle_sgn = TYPE_SIGN (TREE_TYPE (middleop));
  middlemin = wi::ext (innermin, middle_prec, middle_sgn);
  middlemed = wi::ext (innermed, middle_prec, middle_sgn);
  middlemax = wi::ext (innermax, middle_prec, middle_sgn);

  /* Require that the final conversion applied to both the original
     and the intermediate range produces the same result.  */
  final_sgn = TYPE_SIGN (finaltype);
  if (wi::ext (middlemin, final_prec, final_sgn)
	 != wi::ext (innermin, final_prec, final_sgn)
      || wi::ext (middlemed, final_prec, final_sgn)
	 != wi::ext (innermed, final_prec, final_sgn)
      || wi::ext (middlemax, final_prec, final_sgn)
	 != wi::ext (innermax, final_prec, final_sgn))
    return false;

  gimple_assign_set_rhs1 (stmt, innerop);
  fold_stmt (gsi, follow_single_use_edges);
  return true;
}

/* Simplify a conversion from integral SSA name to float in STMT.  */

bool
simplify_using_ranges::simplify_float_conversion_using_ranges
					(gimple_stmt_iterator *gsi,
					 gimple *stmt)
{
  tree rhs1 = gimple_assign_rhs1 (stmt);
  int_range_max vr;
  scalar_float_mode fltmode
    = SCALAR_FLOAT_TYPE_MODE (TREE_TYPE (gimple_assign_lhs (stmt)));
  scalar_int_mode mode;
  tree tem;
  gassign *conv;

  /* We can only handle constant ranges.  */
  if (!query->range_of_expr (vr, rhs1, stmt)
      || vr.varying_p ()
      || vr.undefined_p ())
    return false;

  /* The code below doesn't work for large/huge _BitInt, nor is really
     needed for those, bitint lowering does use ranges already.  */
  if (TREE_CODE (TREE_TYPE (rhs1)) == BITINT_TYPE
      && TYPE_MODE (TREE_TYPE (rhs1)) == BLKmode)
    return false;
  /* First check if we can use a signed type in place of an unsigned.  */
  scalar_int_mode rhs_mode = SCALAR_INT_TYPE_MODE (TREE_TYPE (rhs1));
  if (TYPE_UNSIGNED (TREE_TYPE (rhs1))
      && can_float_p (fltmode, rhs_mode, 0) != CODE_FOR_nothing
      && range_fits_type_p (&vr, TYPE_PRECISION (TREE_TYPE (rhs1)), SIGNED))
    mode = rhs_mode;
  /* If we can do the conversion in the current input mode do nothing.  */
  else if (can_float_p (fltmode, rhs_mode,
			TYPE_UNSIGNED (TREE_TYPE (rhs1))) != CODE_FOR_nothing)
    return false;
  /* Otherwise search for a mode we can use, starting from the narrowest
     integer mode available.  */
  else
    {
      mode = NARROWEST_INT_MODE;
      for (;;)
	{
	  /* If we cannot do a signed conversion to float from mode
	     or if the value-range does not fit in the signed type
	     try with a wider mode.  */
	  if (can_float_p (fltmode, mode, 0) != CODE_FOR_nothing
	      && range_fits_type_p (&vr, GET_MODE_PRECISION (mode), SIGNED))
	    break;

	  /* But do not widen the input.  Instead leave that to the
	     optabs expansion code.  */
	  if (!GET_MODE_WIDER_MODE (mode).exists (&mode)
	      || GET_MODE_PRECISION (mode) > TYPE_PRECISION (TREE_TYPE (rhs1)))
	    return false;
	}
    }

  /* It works, insert a truncation or sign-change before the
     float conversion.  */
  tem = make_ssa_name (build_nonstandard_integer_type
			  (GET_MODE_PRECISION (mode), 0));
  conv = gimple_build_assign (tem, NOP_EXPR, rhs1);
  gsi_insert_before (gsi, conv, GSI_SAME_STMT);
  gimple_assign_set_rhs1 (stmt, tem);
  fold_stmt (gsi, follow_single_use_edges);

  return true;
}

/* Simplify an internal fn call using ranges if possible.  */

bool
simplify_using_ranges::simplify_internal_call_using_ranges
					(gimple_stmt_iterator *gsi,
					 gimple *stmt)
{
  enum tree_code subcode;
  bool is_ubsan = false;
  bool ovf = false;
  switch (gimple_call_internal_fn (stmt))
    {
    case IFN_UBSAN_CHECK_ADD:
      subcode = PLUS_EXPR;
      is_ubsan = true;
      break;
    case IFN_UBSAN_CHECK_SUB:
      subcode = MINUS_EXPR;
      is_ubsan = true;
      break;
    case IFN_UBSAN_CHECK_MUL:
      subcode = MULT_EXPR;
      is_ubsan = true;
      break;
    case IFN_ADD_OVERFLOW:
      subcode = PLUS_EXPR;
      break;
    case IFN_SUB_OVERFLOW:
      subcode = MINUS_EXPR;
      break;
    case IFN_MUL_OVERFLOW:
      subcode = MULT_EXPR;
      break;
    default:
      return false;
    }

  tree op0 = gimple_call_arg (stmt, 0);
  tree op1 = gimple_call_arg (stmt, 1);
  tree type;
  if (is_ubsan)
    {
      type = TREE_TYPE (op0);
      if (VECTOR_TYPE_P (type))
	return false;
    }
  else if (gimple_call_lhs (stmt) == NULL_TREE)
    return false;
  else
    type = TREE_TYPE (TREE_TYPE (gimple_call_lhs (stmt)));
  if (!check_for_binary_op_overflow (query, subcode, type, op0, op1, &ovf, stmt)
      || (is_ubsan && ovf))
    return false;

  gimple *g;
  location_t loc = gimple_location (stmt);
  if (is_ubsan)
    g = gimple_build_assign (gimple_call_lhs (stmt), subcode, op0, op1);
  else
    {
      tree utype = type;
      if (ovf
	  || !useless_type_conversion_p (type, TREE_TYPE (op0))
	  || !useless_type_conversion_p (type, TREE_TYPE (op1)))
	utype = unsigned_type_for (type);
      if (TREE_CODE (op0) == INTEGER_CST)
	op0 = fold_convert (utype, op0);
      else if (!useless_type_conversion_p (utype, TREE_TYPE (op0)))
	{
	  g = gimple_build_assign (make_ssa_name (utype), NOP_EXPR, op0);
	  gimple_set_location (g, loc);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	  op0 = gimple_assign_lhs (g);
	}
      if (TREE_CODE (op1) == INTEGER_CST)
	op1 = fold_convert (utype, op1);
      else if (!useless_type_conversion_p (utype, TREE_TYPE (op1)))
	{
	  g = gimple_build_assign (make_ssa_name (utype), NOP_EXPR, op1);
	  gimple_set_location (g, loc);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	  op1 = gimple_assign_lhs (g);
	}
      g = gimple_build_assign (make_ssa_name (utype), subcode, op0, op1);
      gimple_set_location (g, loc);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
      if (utype != type)
	{
	  g = gimple_build_assign (make_ssa_name (type), NOP_EXPR,
				   gimple_assign_lhs (g));
	  gimple_set_location (g, loc);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	}
      g = gimple_build_assign (gimple_call_lhs (stmt), COMPLEX_EXPR,
			       gimple_assign_lhs (g),
			       build_int_cst (type, ovf));
    }
  gimple_set_location (g, loc);
  gsi_replace (gsi, g, false);
  return true;
}

/* Return true if VAR is a two-valued variable.  Set a and b with the
   two-values when it is true.  Return false otherwise.  */

bool
simplify_using_ranges::two_valued_val_range_p (tree var, tree *a, tree *b,
					       gimple *s)
{
  int_range_max vr;
  if (!query->range_of_expr (vr, var, s))
    return false;
  if (vr.varying_p () || vr.undefined_p ())
    return false;

  if ((vr.num_pairs () == 1 && vr.upper_bound () - vr.lower_bound () == 1)
      || (vr.num_pairs () == 2
	  && vr.lower_bound (0) == vr.upper_bound (0)
	  && vr.lower_bound (1) == vr.upper_bound (1)))
    {
      *a = wide_int_to_tree (TREE_TYPE (var), vr.lower_bound ());
      *b = wide_int_to_tree (TREE_TYPE (var), vr.upper_bound ());
      return true;
    }
  return false;
}

simplify_using_ranges::simplify_using_ranges (range_query *query,
					      int not_executable_flag)
  : query (query)
{
  to_remove_edges = vNULL;
  to_update_switch_stmts = vNULL;
  m_not_executable_flag = not_executable_flag;
  m_flag_set_edges = vNULL;
}

simplify_using_ranges::~simplify_using_ranges ()
{
  cleanup_edges_and_switches ();
  m_flag_set_edges.release ();
}

/* Simplify STMT using ranges if possible.  */

bool
simplify_using_ranges::simplify (gimple_stmt_iterator *gsi)
{
  gcc_checking_assert (query);

  gimple *stmt = gsi_stmt (*gsi);
  if (is_gimple_assign (stmt))
    {
      enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
      tree rhs1 = gimple_assign_rhs1 (stmt);
      tree rhs2 = gimple_assign_rhs2 (stmt);
      tree lhs = gimple_assign_lhs (stmt);
      tree val1 = NULL_TREE, val2 = NULL_TREE;
      use_operand_p use_p;
      gimple *use_stmt;

      /* Convert:
	 LHS = CST BINOP VAR
	 Where VAR is two-valued and LHS is used in GIMPLE_COND only
	 To:
	 LHS = VAR == VAL1 ? (CST BINOP VAL1) : (CST BINOP VAL2)

	 Also handles:
	 LHS = VAR BINOP CST
	 Where VAR is two-valued and LHS is used in GIMPLE_COND only
	 To:
	 LHS = VAR == VAL1 ? (VAL1 BINOP CST) : (VAL2 BINOP CST) */

      if (TREE_CODE_CLASS (rhs_code) == tcc_binary
	  && INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
	  && ((TREE_CODE (rhs1) == INTEGER_CST
	       && TREE_CODE (rhs2) == SSA_NAME)
	      || (TREE_CODE (rhs2) == INTEGER_CST
		  && TREE_CODE (rhs1) == SSA_NAME))
	  && single_imm_use (lhs, &use_p, &use_stmt)
	  && gimple_code (use_stmt) == GIMPLE_COND)

	{
	  tree new_rhs1 = NULL_TREE;
	  tree new_rhs2 = NULL_TREE;
	  tree cmp_var = NULL_TREE;

	  if (TREE_CODE (rhs2) == SSA_NAME
	      && two_valued_val_range_p (rhs2, &val1, &val2, stmt))
	    {
	      /* Optimize RHS1 OP [VAL1, VAL2].  */
	      new_rhs1 = int_const_binop (rhs_code, rhs1, val1);
	      new_rhs2 = int_const_binop (rhs_code, rhs1, val2);
	      cmp_var = rhs2;
	    }
	  else if (TREE_CODE (rhs1) == SSA_NAME
		   && two_valued_val_range_p (rhs1, &val1, &val2, stmt))
	    {
	      /* Optimize [VAL1, VAL2] OP RHS2.  */
	      new_rhs1 = int_const_binop (rhs_code, val1, rhs2);
	      new_rhs2 = int_const_binop (rhs_code, val2, rhs2);
	      cmp_var = rhs1;
	    }

	  /* If we could not find two-vals or the optimzation is invalid as
	     in divide by zero, new_rhs1 / new_rhs will be NULL_TREE.  */
	  if (new_rhs1 && new_rhs2)
	    {
	      tree cond = gimple_build (gsi, true, GSI_SAME_STMT,
					UNKNOWN_LOCATION,
					EQ_EXPR, boolean_type_node,
					cmp_var, val1);
	      gimple_assign_set_rhs_with_ops (gsi,
					      COND_EXPR, cond,
					      new_rhs1,
					      new_rhs2);
	      update_stmt (gsi_stmt (*gsi));
	      fold_stmt (gsi, follow_single_use_edges);
	      return true;
	    }
	}

      if (TREE_CODE_CLASS (rhs_code) == tcc_comparison)
	return simplify_compare_assign_using_ranges_1 (gsi, stmt);

      switch (rhs_code)
	{

      /* Transform TRUNC_DIV_EXPR and TRUNC_MOD_EXPR into RSHIFT_EXPR
	 and BIT_AND_EXPR respectively if the first operand is greater
	 than zero and the second operand is an exact power of two.
	 Also optimize TRUNC_MOD_EXPR away if the second operand is
	 constant and the first operand already has the right value
	 range.  */
	case TRUNC_DIV_EXPR:
	case TRUNC_MOD_EXPR:
	  if ((TREE_CODE (rhs1) == SSA_NAME
	       || TREE_CODE (rhs1) == INTEGER_CST)
	      && INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
	    return simplify_div_or_mod_using_ranges (gsi, stmt);
	  break;

      /* Transform ABS (X) into X or -X as appropriate.  */
	case ABS_EXPR:
	  if (TREE_CODE (rhs1) == SSA_NAME
	      && INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
	    return simplify_abs_using_ranges (gsi, stmt);
	  break;

	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	  /* Optimize away BIT_AND_EXPR and BIT_IOR_EXPR
	     if all the bits being cleared are already cleared or
	     all the bits being set are already set.  */
	  if (INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
	    return simplify_bit_ops_using_ranges (gsi, stmt);
	  break;

	CASE_CONVERT:
	  if (TREE_CODE (rhs1) == SSA_NAME
	      && INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
	    return simplify_conversion_using_ranges (gsi, stmt);
	  break;

	case FLOAT_EXPR:
	  if (TREE_CODE (rhs1) == SSA_NAME
	      && INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
	    return simplify_float_conversion_using_ranges (gsi, stmt);
	  break;

	case MIN_EXPR:
	case MAX_EXPR:
	  if (INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
	    return simplify_min_or_max_using_ranges (gsi, stmt);
	  break;

	case RSHIFT_EXPR:
	  {
	    tree op0 = gimple_assign_rhs1 (stmt);
	    tree type = TREE_TYPE (op0);
	    int_range_max range;
	    if (TYPE_SIGN (type) == SIGNED
		&& query->range_of_expr (range, op0, stmt))
	      {
		unsigned prec = TYPE_PRECISION (TREE_TYPE (op0));
		int_range<2> nzm1 (type, wi::minus_one (prec), wi::zero (prec),
				   VR_ANTI_RANGE);
		range.intersect (nzm1);
		// If there are no ranges other than [-1, 0] remove the shift.
		if (range.undefined_p ())
		  {
		    gimple_assign_set_rhs_from_tree (gsi, op0);
		    return true;
		  }
		return false;
	      }
	    break;
	  }
	default:
	  break;
	}
    }
  else if (gimple_code (stmt) == GIMPLE_COND)
    return simplify_cond_using_ranges_1 (as_a <gcond *> (stmt));
  else if (gimple_code (stmt) == GIMPLE_SWITCH)
    return simplify_switch_using_ranges (as_a <gswitch *> (stmt));
  else if (is_gimple_call (stmt)
	   && gimple_call_internal_p (stmt))
    return simplify_internal_call_using_ranges (gsi, stmt);

  return false;
}
