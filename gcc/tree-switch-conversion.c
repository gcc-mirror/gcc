/* Lower GIMPLE_SWITCH expressions to something more efficient than
   a jump table.
   Copyright (C) 2006-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* This file handles the lowering of GIMPLE_SWITCH to an indexed
   load, or a series of bit-test-and-branch expressions.  */

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
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "params.h"
#include "fold-const.h"
#include "varasm.h"
#include "stor-layout.h"
#include "cfganal.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "cfgloop.h"
#include "alloc-pool.h"
#include "target.h"
#include "tree-into-ssa.h"

/* ??? For lang_hooks.types.type_for_mode, but is there a word_mode
   type in the GIMPLE type system that is language-independent?  */
#include "langhooks.h"


/* Maximum number of case bit tests.
   FIXME: This should be derived from PARAM_CASE_VALUES_THRESHOLD and
	  targetm.case_values_threshold(), or be its own param.  */
#define MAX_CASE_BIT_TESTS  3

/* Split the basic block at the statement pointed to by GSIP, and insert
   a branch to the target basic block of E_TRUE conditional on tree
   expression COND.

   It is assumed that there is already an edge from the to-be-split
   basic block to E_TRUE->dest block.  This edge is removed, and the
   profile information on the edge is re-used for the new conditional
   jump.
   
   The CFG is updated.  The dominator tree will not be valid after
   this transformation, but the immediate dominators are updated if
   UPDATE_DOMINATORS is true.
   
   Returns the newly created basic block.  */

static basic_block
hoist_edge_and_branch_if_true (gimple_stmt_iterator *gsip,
			       tree cond, edge e_true,
			       bool update_dominators)
{
  tree tmp;
  gcond *cond_stmt;
  edge e_false;
  basic_block new_bb, split_bb = gsi_bb (*gsip);
  bool dominated_e_true = false;

  gcc_assert (e_true->src == split_bb);

  if (update_dominators
      && get_immediate_dominator (CDI_DOMINATORS, e_true->dest) == split_bb)
    dominated_e_true = true;

  tmp = force_gimple_operand_gsi (gsip, cond, /*simple=*/true, NULL,
				  /*before=*/true, GSI_SAME_STMT);
  cond_stmt = gimple_build_cond_from_tree (tmp, NULL_TREE, NULL_TREE);
  gsi_insert_before (gsip, cond_stmt, GSI_SAME_STMT);

  e_false = split_block (split_bb, cond_stmt);
  new_bb = e_false->dest;
  redirect_edge_pred (e_true, split_bb);

  e_true->flags &= ~EDGE_FALLTHRU;
  e_true->flags |= EDGE_TRUE_VALUE;

  e_false->flags &= ~EDGE_FALLTHRU;
  e_false->flags |= EDGE_FALSE_VALUE;
  e_false->probability = e_true->probability.invert ();
  new_bb->count = e_false->count ();

  if (update_dominators)
    {
      if (dominated_e_true)
	set_immediate_dominator (CDI_DOMINATORS, e_true->dest, split_bb);
      set_immediate_dominator (CDI_DOMINATORS, e_false->dest, split_bb);
    }

  return new_bb;
}


/* Return true if a switch should be expanded as a bit test.
   RANGE is the difference between highest and lowest case.
   UNIQ is number of unique case node targets, not counting the default case.
   COUNT is the number of comparisons needed, not counting the default case.  */

static bool
expand_switch_using_bit_tests_p (tree range,
				 unsigned int uniq,
				 unsigned int count, bool speed_p)
{
  return (((uniq == 1 && count >= 3)
	   || (uniq == 2 && count >= 5)
	   || (uniq == 3 && count >= 6))
	  && lshift_cheap_p (speed_p)
	  && compare_tree_int (range, GET_MODE_BITSIZE (word_mode)) < 0
	  && compare_tree_int (range, 0) > 0);
}

/* Implement switch statements with bit tests

A GIMPLE switch statement can be expanded to a short sequence of bit-wise
comparisons.  "switch(x)" is converted into "if ((1 << (x-MINVAL)) & CST)"
where CST and MINVAL are integer constants.  This is better than a series
of compare-and-banch insns in some cases,  e.g. we can implement:

	if ((x==4) || (x==6) || (x==9) || (x==11))

as a single bit test:

	if ((1<<x) & ((1<<4)|(1<<6)|(1<<9)|(1<<11)))

This transformation is only applied if the number of case targets is small,
if CST constains at least 3 bits, and "1 << x" is cheap.  The bit tests are
performed in "word_mode".

The following example shows the code the transformation generates:

	int bar(int x)
	{
		switch (x)
		{
		case '0':  case '1':  case '2':  case '3':  case '4':
		case '5':  case '6':  case '7':  case '8':  case '9':
		case 'A':  case 'B':  case 'C':  case 'D':  case 'E':
		case 'F':
			return 1;
		}
		return 0;
	}

==>

	bar (int x)
	{
		tmp1 = x - 48;
		if (tmp1 > (70 - 48)) goto L2;
		tmp2 = 1 << tmp1;
		tmp3 = 0b11111100000001111111111;
		if ((tmp2 & tmp3) != 0) goto L1 ; else goto L2;
	L1:
		return 1;
	L2:
		return 0;
	}

TODO: There are still some improvements to this transformation that could
be implemented:

* A narrower mode than word_mode could be used if that is cheaper, e.g.
  for x86_64 where a narrower-mode shift may result in smaller code.

* The compounded constant could be shifted rather than the one.  The
  test would be either on the sign bit or on the least significant bit,
  depending on the direction of the shift.  On some machines, the test
  for the branch would be free if the bit to test is already set by the
  shift operation.

This transformation was contributed by Roger Sayle, see this e-mail:
   http://gcc.gnu.org/ml/gcc-patches/2003-01/msg01950.html
*/

/* A case_bit_test represents a set of case nodes that may be
   selected from using a bit-wise comparison.  HI and LO hold
   the integer to be tested against, TARGET_EDGE contains the
   edge to the basic block to jump to upon success and BITS
   counts the number of case nodes handled by this test,
   typically the number of bits set in HI:LO.  The LABEL field
   is used to quickly identify all cases in this set without
   looking at label_to_block for every case label.  */

struct case_bit_test
{
  wide_int mask;
  edge target_edge;
  tree label;
  int bits;
};

/* Comparison function for qsort to order bit tests by decreasing
   probability of execution.  Our best guess comes from a measured
   profile.  If the profile counts are equal, break even on the
   number of case nodes, i.e. the node with the most cases gets
   tested first.

   TODO: Actually this currently runs before a profile is available.
   Therefore the case-as-bit-tests transformation should be done
   later in the pass pipeline, or something along the lines of
   "Efficient and effective branch reordering using profile data"
   (Yang et. al., 2002) should be implemented (although, how good
   is a paper is called "Efficient and effective ..." when the
   latter is implied by the former, but oh well...).  */

static int
case_bit_test_cmp (const void *p1, const void *p2)
{
  const struct case_bit_test *const d1 = (const struct case_bit_test *) p1;
  const struct case_bit_test *const d2 = (const struct case_bit_test *) p2;

  if (d2->target_edge->count () < d1->target_edge->count ())
    return -1;
  if (d2->target_edge->count () > d1->target_edge->count ())
    return 1;
  if (d2->bits != d1->bits)
    return d2->bits - d1->bits;

  /* Stabilize the sort.  */
  return LABEL_DECL_UID (d2->label) - LABEL_DECL_UID (d1->label);
}

/*  Expand a switch statement by a short sequence of bit-wise
    comparisons.  "switch(x)" is effectively converted into
    "if ((1 << (x-MINVAL)) & CST)" where CST and MINVAL are
    integer constants.

    INDEX_EXPR is the value being switched on.

    MINVAL is the lowest case value of in the case nodes,
    and RANGE is highest value minus MINVAL.  MINVAL and RANGE
    are not guaranteed to be of the same type as INDEX_EXPR
    (the gimplifier doesn't change the type of case label values,
    and MINVAL and RANGE are derived from those values).
    MAXVAL is MINVAL + RANGE.

    There *MUST* be MAX_CASE_BIT_TESTS or less unique case
    node targets.  */

static void
emit_case_bit_tests (gswitch *swtch, tree index_expr,
		     tree minval, tree range, tree maxval)
{
  struct case_bit_test test[MAX_CASE_BIT_TESTS] = { {} };
  unsigned int i, j, k;
  unsigned int count;

  basic_block switch_bb = gimple_bb (swtch);
  basic_block default_bb, new_default_bb, new_bb;
  edge default_edge;
  bool update_dom = dom_info_available_p (CDI_DOMINATORS);

  vec<basic_block> bbs_to_fix_dom = vNULL;

  tree index_type = TREE_TYPE (index_expr);
  tree unsigned_index_type = unsigned_type_for (index_type);
  unsigned int branch_num = gimple_switch_num_labels (swtch);

  gimple_stmt_iterator gsi;
  gassign *shift_stmt;

  tree idx, tmp, csui;
  tree word_type_node = lang_hooks.types.type_for_mode (word_mode, 1);
  tree word_mode_zero = fold_convert (word_type_node, integer_zero_node);
  tree word_mode_one = fold_convert (word_type_node, integer_one_node);
  int prec = TYPE_PRECISION (word_type_node);
  wide_int wone = wi::one (prec);

  /* Get the edge for the default case.  */
  tmp = gimple_switch_default_label (swtch);
  default_bb = label_to_block (CASE_LABEL (tmp));
  default_edge = find_edge (switch_bb, default_bb);

  /* Go through all case labels, and collect the case labels, profile
     counts, and other information we need to build the branch tests.  */
  count = 0;
  for (i = 1; i < branch_num; i++)
    {
      unsigned int lo, hi;
      tree cs = gimple_switch_label (swtch, i);
      tree label = CASE_LABEL (cs);
      edge e = find_edge (switch_bb, label_to_block (label));
      for (k = 0; k < count; k++)
	if (e == test[k].target_edge)
	  break;

      if (k == count)
	{
	  gcc_checking_assert (count < MAX_CASE_BIT_TESTS);
	  test[k].mask = wi::zero (prec);
	  test[k].target_edge = e;
	  test[k].label = label;
	  test[k].bits = 1;
	  count++;
	}
      else
        test[k].bits++;

      lo = tree_to_uhwi (int_const_binop (MINUS_EXPR,
					  CASE_LOW (cs), minval));
      if (CASE_HIGH (cs) == NULL_TREE)
	hi = lo;
      else
	hi = tree_to_uhwi (int_const_binop (MINUS_EXPR,
					    CASE_HIGH (cs), minval));

      for (j = lo; j <= hi; j++)
	test[k].mask |= wi::lshift (wone, j);
    }

  qsort (test, count, sizeof (*test), case_bit_test_cmp);

  /* If all values are in the 0 .. BITS_PER_WORD-1 range, we can get rid of
     the minval subtractions, but it might make the mask constants more
     expensive.  So, compare the costs.  */
  if (compare_tree_int (minval, 0) > 0
      && compare_tree_int (maxval, GET_MODE_BITSIZE (word_mode)) < 0)
    {
      int cost_diff;
      HOST_WIDE_INT m = tree_to_uhwi (minval);
      rtx reg = gen_raw_REG (word_mode, 10000);
      bool speed_p = optimize_bb_for_speed_p (gimple_bb (swtch));
      cost_diff = set_rtx_cost (gen_rtx_PLUS (word_mode, reg,
					      GEN_INT (-m)), speed_p);
      for (i = 0; i < count; i++)
	{
	  rtx r = immed_wide_int_const (test[i].mask, word_mode);
	  cost_diff += set_src_cost (gen_rtx_AND (word_mode, reg, r),
				     word_mode, speed_p);
	  r = immed_wide_int_const (wi::lshift (test[i].mask, m), word_mode);
	  cost_diff -= set_src_cost (gen_rtx_AND (word_mode, reg, r),
				     word_mode, speed_p);
	}
      if (cost_diff > 0)
	{
	  for (i = 0; i < count; i++)
	    test[i].mask = wi::lshift (test[i].mask, m);
	  minval = build_zero_cst (TREE_TYPE (minval));
	  range = maxval;
	}
    }

  /* We generate two jumps to the default case label.
     Split the default edge, so that we don't have to do any PHI node
     updating.  */
  new_default_bb = split_edge (default_edge);

  if (update_dom)
    {
      bbs_to_fix_dom.create (10);
      bbs_to_fix_dom.quick_push (switch_bb);
      bbs_to_fix_dom.quick_push (default_bb);
      bbs_to_fix_dom.quick_push (new_default_bb);
    }

  /* Now build the test-and-branch code.  */

  gsi = gsi_last_bb (switch_bb);

  /* idx = (unsigned)x - minval.  */
  idx = fold_convert (unsigned_index_type, index_expr);
  idx = fold_build2 (MINUS_EXPR, unsigned_index_type, idx,
		     fold_convert (unsigned_index_type, minval));
  idx = force_gimple_operand_gsi (&gsi, idx,
				  /*simple=*/true, NULL_TREE,
				  /*before=*/true, GSI_SAME_STMT);

  /* if (idx > range) goto default */
  range = force_gimple_operand_gsi (&gsi,
				    fold_convert (unsigned_index_type, range),
				    /*simple=*/true, NULL_TREE,
				    /*before=*/true, GSI_SAME_STMT);
  tmp = fold_build2 (GT_EXPR, boolean_type_node, idx, range);
  new_bb = hoist_edge_and_branch_if_true (&gsi, tmp, default_edge, update_dom);
  if (update_dom)
    bbs_to_fix_dom.quick_push (new_bb);
  gcc_assert (gimple_bb (swtch) == new_bb);
  gsi = gsi_last_bb (new_bb);

  /* Any blocks dominated by the GIMPLE_SWITCH, but that are not successors
     of NEW_BB, are still immediately dominated by SWITCH_BB.  Make it so.  */
  if (update_dom)
    {
      vec<basic_block> dom_bbs;
      basic_block dom_son;

      dom_bbs = get_dominated_by (CDI_DOMINATORS, new_bb);
      FOR_EACH_VEC_ELT (dom_bbs, i, dom_son)
	{
	  edge e = find_edge (new_bb, dom_son);
	  if (e && single_pred_p (e->dest))
	    continue;
	  set_immediate_dominator (CDI_DOMINATORS, dom_son, switch_bb);
	  bbs_to_fix_dom.safe_push (dom_son);
	}
      dom_bbs.release ();
    }

  /* csui = (1 << (word_mode) idx) */
  csui = make_ssa_name (word_type_node);
  tmp = fold_build2 (LSHIFT_EXPR, word_type_node, word_mode_one,
		     fold_convert (word_type_node, idx));
  tmp = force_gimple_operand_gsi (&gsi, tmp,
				  /*simple=*/false, NULL_TREE,
				  /*before=*/true, GSI_SAME_STMT);
  shift_stmt = gimple_build_assign (csui, tmp);
  gsi_insert_before (&gsi, shift_stmt, GSI_SAME_STMT);
  update_stmt (shift_stmt);

  /* for each unique set of cases:
        if (const & csui) goto target  */
  for (k = 0; k < count; k++)
    {
      tmp = wide_int_to_tree (word_type_node, test[k].mask);
      tmp = fold_build2 (BIT_AND_EXPR, word_type_node, csui, tmp);
      tmp = force_gimple_operand_gsi (&gsi, tmp,
				      /*simple=*/true, NULL_TREE,
				      /*before=*/true, GSI_SAME_STMT);
      tmp = fold_build2 (NE_EXPR, boolean_type_node, tmp, word_mode_zero);
      new_bb = hoist_edge_and_branch_if_true (&gsi, tmp, test[k].target_edge,
					      update_dom);
      if (update_dom)
	bbs_to_fix_dom.safe_push (new_bb);
      gcc_assert (gimple_bb (swtch) == new_bb);
      gsi = gsi_last_bb (new_bb);
    }

  /* We should have removed all edges now.  */
  gcc_assert (EDGE_COUNT (gsi_bb (gsi)->succs) == 0);

  /* If nothing matched, go to the default label.  */
  make_edge (gsi_bb (gsi), new_default_bb, EDGE_FALLTHRU);

  /* The GIMPLE_SWITCH is now redundant.  */
  gsi_remove (&gsi, true);

  if (update_dom)
    {
      /* Fix up the dominator tree.  */
      iterate_fix_dominators (CDI_DOMINATORS, bbs_to_fix_dom, true);
      bbs_to_fix_dom.release ();
    }
}

/*
     Switch initialization conversion

The following pass changes simple initializations of scalars in a switch
statement into initializations from a static array.  Obviously, the values
must be constant and known at compile time and a default branch must be
provided.  For example, the following code:

        int a,b;

        switch (argc)
	{
         case 1:
         case 2:
                a_1 = 8;
                b_1 = 6;
                break;
         case 3:
                a_2 = 9;
                b_2 = 5;
                break;
         case 12:
                a_3 = 10;
                b_3 = 4;
                break;
         default:
                a_4 = 16;
                b_4 = 1;
		break;
        }
	a_5 = PHI <a_1, a_2, a_3, a_4>
	b_5 = PHI <b_1, b_2, b_3, b_4>


is changed into:

        static const int = CSWTCH01[] = {6, 6, 5, 1, 1, 1, 1, 1, 1, 1, 1, 4};
        static const int = CSWTCH02[] = {8, 8, 9, 16, 16, 16, 16, 16, 16, 16,
                                 16, 16, 10};

        if (((unsigned) argc) - 1 < 11)
          {
	    a_6 = CSWTCH02[argc - 1];
            b_6 = CSWTCH01[argc - 1];
	  }
	else
	  {
	    a_7 = 16;
	    b_7 = 1;
          }
	a_5 = PHI <a_6, a_7>
	b_b = PHI <b_6, b_7>

There are further constraints.  Specifically, the range of values across all
case labels must not be bigger than SWITCH_CONVERSION_BRANCH_RATIO (default
eight) times the number of the actual switch branches.

This transformation was contributed by Martin Jambor, see this e-mail:
   http://gcc.gnu.org/ml/gcc-patches/2008-07/msg00011.html  */

/* The main structure of the pass.  */
struct switch_conv_info
{
  /* The expression used to decide the switch branch.  */
  tree index_expr;

  /* The following integer constants store the minimum and maximum value
     covered by the case labels.  */
  tree range_min;
  tree range_max;

  /* The difference between the above two numbers.  Stored here because it
     is used in all the conversion heuristics, as well as for some of the
     transformation, and it is expensive to re-compute it all the time.  */
  tree range_size;

  /* Basic block that contains the actual GIMPLE_SWITCH.  */
  basic_block switch_bb;

  /* Basic block that is the target of the default case.  */
  basic_block default_bb;

  /* The single successor block of all branches out of the GIMPLE_SWITCH,
     if such a block exists.  Otherwise NULL.  */
  basic_block final_bb;

  /* The probability of the default edge in the replaced switch.  */
  profile_probability default_prob;

  /* The count of the default edge in the replaced switch.  */
  profile_count default_count;

  /* Combined count of all other (non-default) edges in the replaced switch.  */
  profile_count other_count;

  /* Number of phi nodes in the final bb (that we'll be replacing).  */
  int phi_count;

  /* Array of default values, in the same order as phi nodes.  */
  tree *default_values;

  /* Constructors of new static arrays.  */
  vec<constructor_elt, va_gc> **constructors;

  /* Array of ssa names that are initialized with a value from a new static
     array.  */
  tree *target_inbound_names;

  /* Array of ssa names that are initialized with the default value if the
     switch expression is out of range.  */
  tree *target_outbound_names;

  /* VOP SSA_NAME.  */
  tree target_vop;

  /* The first load statement that loads a temporary from a new static array.
   */
  gimple *arr_ref_first;

  /* The last load statement that loads a temporary from a new static array.  */
  gimple *arr_ref_last;

  /* String reason why the case wasn't a good candidate that is written to the
     dump file, if there is one.  */
  const char *reason;

  /* True if default case is not used for any value between range_min and
     range_max inclusive.  */
  bool contiguous_range;

  /* True if default case does not have the required shape for other case
     labels.  */
  bool default_case_nonstandard;

  /* Parameters for expand_switch_using_bit_tests.  Should be computed
     the same way as in expand_case.  */
  unsigned int uniq;
  unsigned int count;
};

/* Collect information about GIMPLE_SWITCH statement SWTCH into INFO.  */

static void
collect_switch_conv_info (gswitch *swtch, struct switch_conv_info *info)
{
  unsigned int branch_num = gimple_switch_num_labels (swtch);
  tree min_case, max_case;
  unsigned int count, i;
  edge e, e_default, e_first;
  edge_iterator ei;
  basic_block first;

  memset (info, 0, sizeof (*info));

  /* The gimplifier has already sorted the cases by CASE_LOW and ensured there
     is a default label which is the first in the vector.
     Collect the bits we can deduce from the CFG.  */
  info->index_expr = gimple_switch_index (swtch);
  info->switch_bb = gimple_bb (swtch);
  info->default_bb
    = label_to_block (CASE_LABEL (gimple_switch_default_label (swtch)));
  e_default = find_edge (info->switch_bb, info->default_bb);
  info->default_prob = e_default->probability;
  info->default_count = e_default->count ();
  FOR_EACH_EDGE (e, ei, info->switch_bb->succs)
    if (e != e_default)
      info->other_count += e->count ();

  /* Get upper and lower bounds of case values, and the covered range.  */
  min_case = gimple_switch_label (swtch, 1);
  max_case = gimple_switch_label (swtch, branch_num - 1);

  info->range_min = CASE_LOW (min_case);
  if (CASE_HIGH (max_case) != NULL_TREE)
    info->range_max = CASE_HIGH (max_case);
  else
    info->range_max = CASE_LOW (max_case);

  info->contiguous_range = true;
  tree last = CASE_HIGH (min_case) ? CASE_HIGH (min_case) : info->range_min;
  for (i = 2; i < branch_num; i++)
    {
      tree elt = gimple_switch_label (swtch, i);
      if (wi::to_wide (last) + 1 != wi::to_wide (CASE_LOW (elt)))
	{
	  info->contiguous_range = false;
	  break;
	}
      last = CASE_HIGH (elt) ? CASE_HIGH (elt) : CASE_LOW (elt);
    }

  if (info->contiguous_range)
    {
      first = label_to_block (CASE_LABEL (gimple_switch_label (swtch, 1)));
      e_first = find_edge (info->switch_bb, first);
    }
  else
    {
      first = info->default_bb;
      e_first = e_default;
    }

  /* See if there is one common successor block for all branch
     targets.  If it exists, record it in FINAL_BB.
     Start with the destination of the first non-default case
     if the range is contiguous and default case otherwise as
     guess or its destination in case it is a forwarder block.  */
  if (! single_pred_p (e_first->dest))
    info->final_bb = e_first->dest;
  else if (single_succ_p (e_first->dest)
	   && ! single_pred_p (single_succ (e_first->dest)))
    info->final_bb = single_succ (e_first->dest);
  /* Require that all switch destinations are either that common
     FINAL_BB or a forwarder to it, except for the default
     case if contiguous range.  */
  if (info->final_bb)
    FOR_EACH_EDGE (e, ei, info->switch_bb->succs)
      {
	if (e->dest == info->final_bb)
	  continue;

	if (single_pred_p (e->dest)
	    && single_succ_p (e->dest)
	    && single_succ (e->dest) == info->final_bb)
	  continue;

	if (e == e_default && info->contiguous_range)
	  {
	    info->default_case_nonstandard = true;
	    continue;
	  }

	info->final_bb = NULL;
	break;
      }

  info->range_size
    = int_const_binop (MINUS_EXPR, info->range_max, info->range_min);

  /* Get a count of the number of case labels.  Single-valued case labels
     simply count as one, but a case range counts double, since it may
     require two compares if it gets lowered as a branching tree.  */
  count = 0;
  for (i = 1; i < branch_num; i++)
    {
      tree elt = gimple_switch_label (swtch, i);
      count++;
      if (CASE_HIGH (elt)
	  && ! tree_int_cst_equal (CASE_LOW (elt), CASE_HIGH (elt)))
	count++;
    }
  info->count = count;
 
  /* Get the number of unique non-default targets out of the GIMPLE_SWITCH
     block.  Assume a CFG cleanup would have already removed degenerate
     switch statements, this allows us to just use EDGE_COUNT.  */
  info->uniq = EDGE_COUNT (gimple_bb (swtch)->succs) - 1;
}

/* Checks whether the range given by individual case statements of the SWTCH
   switch statement isn't too big and whether the number of branches actually
   satisfies the size of the new array.  */

static bool
check_range (struct switch_conv_info *info)
{
  gcc_assert (info->range_size);
  if (!tree_fits_uhwi_p (info->range_size))
    {
      info->reason = "index range way too large or otherwise unusable";
      return false;
    }

  if (tree_to_uhwi (info->range_size)
      > ((unsigned) info->count * SWITCH_CONVERSION_BRANCH_RATIO))
    {
      info->reason = "the maximum range-branch ratio exceeded";
      return false;
    }

  return true;
}

/* Checks whether all but the FINAL_BB basic blocks are empty.  */

static bool
check_all_empty_except_final (struct switch_conv_info *info)
{
  edge e, e_default = find_edge (info->switch_bb, info->default_bb);
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, info->switch_bb->succs)
    {
      if (e->dest == info->final_bb)
	continue;

      if (!empty_block_p (e->dest))
	{
	  if (info->contiguous_range && e == e_default)
	    {
	      info->default_case_nonstandard = true;
	      continue;
	    }

	  info->reason = "bad case - a non-final BB not empty";
	  return false;
	}
    }

  return true;
}

/* This function checks whether all required values in phi nodes in final_bb
   are constants.  Required values are those that correspond to a basic block
   which is a part of the examined switch statement.  It returns true if the
   phi nodes are OK, otherwise false.  */

static bool
check_final_bb (gswitch *swtch, struct switch_conv_info *info)
{
  gphi_iterator gsi;

  info->phi_count = 0;
  for (gsi = gsi_start_phis (info->final_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      unsigned int i;

      if (virtual_operand_p (gimple_phi_result (phi)))
	continue;

      info->phi_count++;

      for (i = 0; i < gimple_phi_num_args (phi); i++)
	{
	  basic_block bb = gimple_phi_arg_edge (phi, i)->src;

	  if (bb == info->switch_bb
	      || (single_pred_p (bb)
		  && single_pred (bb) == info->switch_bb
		  && (!info->default_case_nonstandard
		      || empty_block_p (bb))))
	    {
	      tree reloc, val;
	      const char *reason = NULL;

	      val = gimple_phi_arg_def (phi, i);
	      if (!is_gimple_ip_invariant (val))
		reason = "non-invariant value from a case";
	      else
		{
		  reloc = initializer_constant_valid_p (val, TREE_TYPE (val));
		  if ((flag_pic && reloc != null_pointer_node)
		      || (!flag_pic && reloc == NULL_TREE))
		    {
		      if (reloc)
			reason
			  = "value from a case would need runtime relocations";
		      else
			reason
			  = "value from a case is not a valid initializer";
		    }
		}
	      if (reason)
		{
		  /* For contiguous range, we can allow non-constant
		     or one that needs relocation, as long as it is
		     only reachable from the default case.  */
		  if (bb == info->switch_bb)
		    bb = info->final_bb;
		  if (!info->contiguous_range || bb != info->default_bb)
		    {
		      info->reason = reason;
		      return false;
		    }

		  unsigned int branch_num = gimple_switch_num_labels (swtch);
		  for (unsigned int i = 1; i < branch_num; i++)
		    {
		      tree lab = CASE_LABEL (gimple_switch_label (swtch, i));
		      if (label_to_block (lab) == bb)
			{
			  info->reason = reason;
			  return false;
			}
		    }
		  info->default_case_nonstandard = true;
		}
	    }
	}
    }

  return true;
}

/* The following function allocates default_values, target_{in,out}_names and
   constructors arrays.  The last one is also populated with pointers to
   vectors that will become constructors of new arrays.  */

static void
create_temp_arrays (struct switch_conv_info *info)
{
  int i;

  info->default_values = XCNEWVEC (tree, info->phi_count * 3);
  /* ??? Macros do not support multi argument templates in their
     argument list.  We create a typedef to work around that problem.  */
  typedef vec<constructor_elt, va_gc> *vec_constructor_elt_gc;
  info->constructors = XCNEWVEC (vec_constructor_elt_gc, info->phi_count);
  info->target_inbound_names = info->default_values + info->phi_count;
  info->target_outbound_names = info->target_inbound_names + info->phi_count;
  for (i = 0; i < info->phi_count; i++)
    vec_alloc (info->constructors[i], tree_to_uhwi (info->range_size) + 1);
}

/* Free the arrays created by create_temp_arrays().  The vectors that are
   created by that function are not freed here, however, because they have
   already become constructors and must be preserved.  */

static void
free_temp_arrays (struct switch_conv_info *info)
{
  XDELETEVEC (info->constructors);
  XDELETEVEC (info->default_values);
}

/* Populate the array of default values in the order of phi nodes.
   DEFAULT_CASE is the CASE_LABEL_EXPR for the default switch branch
   if the range is non-contiguous or the default case has standard
   structure, otherwise it is the first non-default case instead.  */

static void
gather_default_values (tree default_case, struct switch_conv_info *info)
{
  gphi_iterator gsi;
  basic_block bb = label_to_block (CASE_LABEL (default_case));
  edge e;
  int i = 0;

  gcc_assert (CASE_LOW (default_case) == NULL_TREE
	      || info->default_case_nonstandard);

  if (bb == info->final_bb)
    e = find_edge (info->switch_bb, bb);
  else
    e = single_succ_edge (bb);

  for (gsi = gsi_start_phis (info->final_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      if (virtual_operand_p (gimple_phi_result (phi)))
	continue;
      tree val = PHI_ARG_DEF_FROM_EDGE (phi, e);
      gcc_assert (val);
      info->default_values[i++] = val;
    }
}

/* The following function populates the vectors in the constructors array with
   future contents of the static arrays.  The vectors are populated in the
   order of phi nodes.  SWTCH is the switch statement being converted.  */

static void
build_constructors (gswitch *swtch, struct switch_conv_info *info)
{
  unsigned i, branch_num = gimple_switch_num_labels (swtch);
  tree pos = info->range_min;
  tree pos_one = build_int_cst (TREE_TYPE (pos), 1);

  for (i = 1; i < branch_num; i++)
    {
      tree cs = gimple_switch_label (swtch, i);
      basic_block bb = label_to_block (CASE_LABEL (cs));
      edge e;
      tree high;
      gphi_iterator gsi;
      int j;

      if (bb == info->final_bb)
	e = find_edge (info->switch_bb, bb);
      else
	e = single_succ_edge (bb);
      gcc_assert (e);

      while (tree_int_cst_lt (pos, CASE_LOW (cs)))
	{
	  int k;
	  gcc_assert (!info->contiguous_range);
	  for (k = 0; k < info->phi_count; k++)
	    {
	      constructor_elt elt;

	      elt.index = int_const_binop (MINUS_EXPR, pos, info->range_min);
	      elt.value
		= unshare_expr_without_location (info->default_values[k]);
	      info->constructors[k]->quick_push (elt);
	    }

	  pos = int_const_binop (PLUS_EXPR, pos, pos_one);
	}
      gcc_assert (tree_int_cst_equal (pos, CASE_LOW (cs)));

      j = 0;
      if (CASE_HIGH (cs))
	high = CASE_HIGH (cs);
      else
	high = CASE_LOW (cs);
      for (gsi = gsi_start_phis (info->final_bb);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  if (virtual_operand_p (gimple_phi_result (phi)))
	    continue;
	  tree val = PHI_ARG_DEF_FROM_EDGE (phi, e);
	  tree low = CASE_LOW (cs);
	  pos = CASE_LOW (cs);

	  do
	    {
	      constructor_elt elt;

	      elt.index = int_const_binop (MINUS_EXPR, pos, info->range_min);
	      elt.value = unshare_expr_without_location (val);
	      info->constructors[j]->quick_push (elt);

	      pos = int_const_binop (PLUS_EXPR, pos, pos_one);
	    } while (!tree_int_cst_lt (high, pos)
		     && tree_int_cst_lt (low, pos));
	  j++;
	}
    }
}

/* If all values in the constructor vector are the same, return the value.
   Otherwise return NULL_TREE.  Not supposed to be called for empty
   vectors.  */

static tree
constructor_contains_same_values_p (vec<constructor_elt, va_gc> *vec)
{
  unsigned int i;
  tree prev = NULL_TREE;
  constructor_elt *elt;

  FOR_EACH_VEC_SAFE_ELT (vec, i, elt)
    {
      if (!prev)
	prev = elt->value;
      else if (!operand_equal_p (elt->value, prev, OEP_ONLY_CONST))
	return NULL_TREE;
    }
  return prev;
}

/* Return type which should be used for array elements, either TYPE's
   main variant or, for integral types, some smaller integral type
   that can still hold all the constants.  */

static tree
array_value_type (gswitch *swtch, tree type, int num,
		  struct switch_conv_info *info)
{
  unsigned int i, len = vec_safe_length (info->constructors[num]);
  constructor_elt *elt;
  int sign = 0;
  tree smaller_type;

  /* Types with alignments greater than their size can reach here, e.g. out of
     SRA.  We couldn't use these as an array component type so get back to the
     main variant first, which, for our purposes, is fine for other types as
     well.  */

  type = TYPE_MAIN_VARIANT (type);

  if (!INTEGRAL_TYPE_P (type))
    return type;

  scalar_int_mode type_mode = SCALAR_INT_TYPE_MODE (type);
  scalar_int_mode mode = get_narrowest_mode (type_mode);
  if (GET_MODE_SIZE (type_mode) <= GET_MODE_SIZE (mode))
    return type;

  if (len < (optimize_bb_for_size_p (gimple_bb (swtch)) ? 2 : 32))
    return type;

  FOR_EACH_VEC_SAFE_ELT (info->constructors[num], i, elt)
    {
      wide_int cst;

      if (TREE_CODE (elt->value) != INTEGER_CST)
	return type;

      cst = wi::to_wide (elt->value);
      while (1)
	{
	  unsigned int prec = GET_MODE_BITSIZE (mode);
	  if (prec > HOST_BITS_PER_WIDE_INT)
	    return type;

	  if (sign >= 0 && cst == wi::zext (cst, prec))
	    {
	      if (sign == 0 && cst == wi::sext (cst, prec))
		break;
	      sign = 1;
	      break;
	    }
	  if (sign <= 0 && cst == wi::sext (cst, prec))
	    {
	      sign = -1;
	      break;
	    }

	  if (sign == 1)
	    sign = 0;

	  if (!GET_MODE_WIDER_MODE (mode).exists (&mode)
	      || GET_MODE_SIZE (mode) >= GET_MODE_SIZE (type_mode))
	    return type;
	}
    }

  if (sign == 0)
    sign = TYPE_UNSIGNED (type) ? 1 : -1;
  smaller_type = lang_hooks.types.type_for_mode (mode, sign >= 0);
  if (GET_MODE_SIZE (type_mode)
      <= GET_MODE_SIZE (SCALAR_INT_TYPE_MODE (smaller_type)))
    return type;

  return smaller_type;
}

/* Create an appropriate array type and declaration and assemble a static array
   variable.  Also create a load statement that initializes the variable in
   question with a value from the static array.  SWTCH is the switch statement
   being converted, NUM is the index to arrays of constructors, default values
   and target SSA names for this particular array.  ARR_INDEX_TYPE is the type
   of the index of the new array, PHI is the phi node of the final BB that
   corresponds to the value that will be loaded from the created array.  TIDX
   is an ssa name of a temporary variable holding the index for loads from the
   new array.  */

static void
build_one_array (gswitch *swtch, int num, tree arr_index_type,
		 gphi *phi, tree tidx, struct switch_conv_info *info)
{
  tree name, cst;
  gimple *load;
  gimple_stmt_iterator gsi = gsi_for_stmt (swtch);
  location_t loc = gimple_location (swtch);

  gcc_assert (info->default_values[num]);

  name = copy_ssa_name (PHI_RESULT (phi));
  info->target_inbound_names[num] = name;

  cst = constructor_contains_same_values_p (info->constructors[num]);
  if (cst)
    load = gimple_build_assign (name, cst);
  else
    {
      tree array_type, ctor, decl, value_type, fetch, default_type;

      default_type = TREE_TYPE (info->default_values[num]);
      value_type = array_value_type (swtch, default_type, num, info);
      array_type = build_array_type (value_type, arr_index_type);
      if (default_type != value_type)
	{
	  unsigned int i;
	  constructor_elt *elt;

	  FOR_EACH_VEC_SAFE_ELT (info->constructors[num], i, elt)
	    elt->value = fold_convert (value_type, elt->value);
	}
      ctor = build_constructor (array_type, info->constructors[num]);
      TREE_CONSTANT (ctor) = true;
      TREE_STATIC (ctor) = true;

      decl = build_decl (loc, VAR_DECL, NULL_TREE, array_type);
      TREE_STATIC (decl) = 1;
      DECL_INITIAL (decl) = ctor;

      DECL_NAME (decl) = create_tmp_var_name ("CSWTCH");
      DECL_ARTIFICIAL (decl) = 1;
      DECL_IGNORED_P (decl) = 1;
      TREE_CONSTANT (decl) = 1;
      TREE_READONLY (decl) = 1;
      DECL_IGNORED_P (decl) = 1;
      varpool_node::finalize_decl (decl);

      fetch = build4 (ARRAY_REF, value_type, decl, tidx, NULL_TREE,
		      NULL_TREE);
      if (default_type != value_type)
	{
	  fetch = fold_convert (default_type, fetch);
	  fetch = force_gimple_operand_gsi (&gsi, fetch, true, NULL_TREE,
					    true, GSI_SAME_STMT);
	}
      load = gimple_build_assign (name, fetch);
    }

  gsi_insert_before (&gsi, load, GSI_SAME_STMT);
  update_stmt (load);
  info->arr_ref_last = load;
}

/* Builds and initializes static arrays initialized with values gathered from
   the SWTCH switch statement.  Also creates statements that load values from
   them.  */

static void
build_arrays (gswitch *swtch, struct switch_conv_info *info)
{
  tree arr_index_type;
  tree tidx, sub, utype;
  gimple *stmt;
  gimple_stmt_iterator gsi;
  gphi_iterator gpi;
  int i;
  location_t loc = gimple_location (swtch);

  gsi = gsi_for_stmt (swtch);

  /* Make sure we do not generate arithmetics in a subrange.  */
  utype = TREE_TYPE (info->index_expr);
  if (TREE_TYPE (utype))
    utype = lang_hooks.types.type_for_mode (TYPE_MODE (TREE_TYPE (utype)), 1);
  else
    utype = lang_hooks.types.type_for_mode (TYPE_MODE (utype), 1);

  arr_index_type = build_index_type (info->range_size);
  tidx = make_ssa_name (utype);
  sub = fold_build2_loc (loc, MINUS_EXPR, utype,
			 fold_convert_loc (loc, utype, info->index_expr),
			 fold_convert_loc (loc, utype, info->range_min));
  sub = force_gimple_operand_gsi (&gsi, sub,
				  false, NULL, true, GSI_SAME_STMT);
  stmt = gimple_build_assign (tidx, sub);

  gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
  update_stmt (stmt);
  info->arr_ref_first = stmt;

  for (gpi = gsi_start_phis (info->final_bb), i = 0;
       !gsi_end_p (gpi); gsi_next (&gpi))
    {
      gphi *phi = gpi.phi ();
      if (!virtual_operand_p (gimple_phi_result (phi)))
	build_one_array (swtch, i++, arr_index_type, phi, tidx, info);
      else
	{
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, info->switch_bb->succs)
	    {
	      if (e->dest == info->final_bb)
		break;
	      if (!info->default_case_nonstandard
		  || e->dest != info->default_bb)
		{
		  e = single_succ_edge (e->dest);
		  break;
		}
	    }
	  gcc_assert (e && e->dest == info->final_bb);
	  info->target_vop = PHI_ARG_DEF_FROM_EDGE (phi, e);
	}
    }
}

/* Generates and appropriately inserts loads of default values at the position
   given by BSI.  Returns the last inserted statement.  */

static gassign *
gen_def_assigns (gimple_stmt_iterator *gsi, struct switch_conv_info *info)
{
  int i;
  gassign *assign = NULL;

  for (i = 0; i < info->phi_count; i++)
    {
      tree name = copy_ssa_name (info->target_inbound_names[i]);
      info->target_outbound_names[i] = name;
      assign = gimple_build_assign (name, info->default_values[i]);
      gsi_insert_before (gsi, assign, GSI_SAME_STMT);
      update_stmt (assign);
    }
  return assign;
}

/* Deletes the unused bbs and edges that now contain the switch statement and
   its empty branch bbs.  BBD is the now dead BB containing the original switch
   statement, FINAL is the last BB of the converted switch statement (in terms
   of succession).  */

static void
prune_bbs (basic_block bbd, basic_block final, basic_block default_bb)
{
  edge_iterator ei;
  edge e;

  for (ei = ei_start (bbd->succs); (e = ei_safe_edge (ei)); )
    {
      basic_block bb;
      bb = e->dest;
      remove_edge (e);
      if (bb != final && bb != default_bb)
	delete_basic_block (bb);
    }
  delete_basic_block (bbd);
}

/* Add values to phi nodes in final_bb for the two new edges.  E1F is the edge
   from the basic block loading values from an array and E2F from the basic
   block loading default values.  BBF is the last switch basic block (see the
   bbf description in the comment below).  */

static void
fix_phi_nodes (edge e1f, edge e2f, basic_block bbf,
	       struct switch_conv_info *info)
{
  gphi_iterator gsi;
  int i;

  for (gsi = gsi_start_phis (bbf), i = 0;
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree inbound, outbound;
      if (virtual_operand_p (gimple_phi_result (phi)))
	inbound = outbound = info->target_vop;
      else
	{
	  inbound = info->target_inbound_names[i];
	  outbound = info->target_outbound_names[i++];
	}
      add_phi_arg (phi, inbound, e1f, UNKNOWN_LOCATION);
      if (!info->default_case_nonstandard)
	add_phi_arg (phi, outbound, e2f, UNKNOWN_LOCATION);
    }
}

/* Creates a check whether the switch expression value actually falls into the
   range given by all the cases.  If it does not, the temporaries are loaded
   with default values instead.  SWTCH is the switch statement being converted.

   bb0 is the bb with the switch statement, however, we'll end it with a
       condition instead.

   bb1 is the bb to be used when the range check went ok.  It is derived from
       the switch BB

   bb2 is the bb taken when the expression evaluated outside of the range
       covered by the created arrays.  It is populated by loads of default
       values.

   bbF is a fall through for both bb1 and bb2 and contains exactly what
       originally followed the switch statement.

   bbD contains the switch statement (in the end).  It is unreachable but we
       still need to strip off its edges.
*/

static void
gen_inbound_check (gswitch *swtch, struct switch_conv_info *info)
{
  tree label_decl1 = create_artificial_label (UNKNOWN_LOCATION);
  tree label_decl2 = create_artificial_label (UNKNOWN_LOCATION);
  tree label_decl3 = create_artificial_label (UNKNOWN_LOCATION);
  glabel *label1, *label2, *label3;
  tree utype, tidx;
  tree bound;

  gcond *cond_stmt;

  gassign *last_assign = NULL;
  gimple_stmt_iterator gsi;
  basic_block bb0, bb1, bb2, bbf, bbd;
  edge e01 = NULL, e02, e21, e1d, e1f, e2f;
  location_t loc = gimple_location (swtch);

  gcc_assert (info->default_values);

  bb0 = gimple_bb (swtch);

  tidx = gimple_assign_lhs (info->arr_ref_first);
  utype = TREE_TYPE (tidx);

  /* (end of) block 0 */
  gsi = gsi_for_stmt (info->arr_ref_first);
  gsi_next (&gsi);

  bound = fold_convert_loc (loc, utype, info->range_size);
  cond_stmt = gimple_build_cond (LE_EXPR, tidx, bound, NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, cond_stmt, GSI_SAME_STMT);
  update_stmt (cond_stmt);

  /* block 2 */
  if (!info->default_case_nonstandard)
    {
      label2 = gimple_build_label (label_decl2);
      gsi_insert_before (&gsi, label2, GSI_SAME_STMT);
      last_assign = gen_def_assigns (&gsi, info);
    }

  /* block 1 */
  label1 = gimple_build_label (label_decl1);
  gsi_insert_before (&gsi, label1, GSI_SAME_STMT);

  /* block F */
  gsi = gsi_start_bb (info->final_bb);
  label3 = gimple_build_label (label_decl3);
  gsi_insert_before (&gsi, label3, GSI_SAME_STMT);

  /* cfg fix */
  e02 = split_block (bb0, cond_stmt);
  bb2 = e02->dest;

  if (info->default_case_nonstandard)
    {
      bb1 = bb2;
      bb2 = info->default_bb;
      e01 = e02;
      e01->flags = EDGE_TRUE_VALUE;
      e02 = make_edge (bb0, bb2, EDGE_FALSE_VALUE);
      edge e_default = find_edge (bb1, bb2);
      for (gphi_iterator gsi = gsi_start_phis (bb2);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  tree arg = PHI_ARG_DEF_FROM_EDGE (phi, e_default);
	  add_phi_arg (phi, arg, e02,
		       gimple_phi_arg_location_from_edge (phi, e_default));
	}
      /* Partially fix the dominator tree, if it is available.  */
      if (dom_info_available_p (CDI_DOMINATORS))
	redirect_immediate_dominators (CDI_DOMINATORS, bb1, bb0);
    }
  else
    {
      e21 = split_block (bb2, last_assign);
      bb1 = e21->dest;
      remove_edge (e21);
    }

  e1d = split_block (bb1, info->arr_ref_last);
  bbd = e1d->dest;
  remove_edge (e1d);

  /* flags and profiles of the edge for in-range values */
  if (!info->default_case_nonstandard)
    e01 = make_edge (bb0, bb1, EDGE_TRUE_VALUE);
  e01->probability = info->default_prob.invert ();

  /* flags and profiles of the edge taking care of out-of-range values */
  e02->flags &= ~EDGE_FALLTHRU;
  e02->flags |= EDGE_FALSE_VALUE;
  e02->probability = info->default_prob;

  bbf = info->final_bb;

  e1f = make_edge (bb1, bbf, EDGE_FALLTHRU);
  e1f->probability = profile_probability::always ();

  if (info->default_case_nonstandard)
    e2f = NULL;
  else
    {
      e2f = make_edge (bb2, bbf, EDGE_FALLTHRU);
      e2f->probability = profile_probability::always ();
    }

  /* frequencies of the new BBs */
  bb1->frequency = EDGE_FREQUENCY (e01);
  bb2->frequency = EDGE_FREQUENCY (e02);
  if (!info->default_case_nonstandard)
    bbf->frequency = EDGE_FREQUENCY (e1f) + EDGE_FREQUENCY (e2f);

  /* Tidy blocks that have become unreachable.  */
  prune_bbs (bbd, info->final_bb,
	     info->default_case_nonstandard ? info->default_bb : NULL);

  /* Fixup the PHI nodes in bbF.  */
  fix_phi_nodes (e1f, e2f, bbf, info);

  /* Fix the dominator tree, if it is available.  */
  if (dom_info_available_p (CDI_DOMINATORS))
    {
      vec<basic_block> bbs_to_fix_dom;

      set_immediate_dominator (CDI_DOMINATORS, bb1, bb0);
      if (!info->default_case_nonstandard)
	set_immediate_dominator (CDI_DOMINATORS, bb2, bb0);
      if (! get_immediate_dominator (CDI_DOMINATORS, bbf))
	/* If bbD was the immediate dominator ...  */
	set_immediate_dominator (CDI_DOMINATORS, bbf, bb0);

      bbs_to_fix_dom.create (3 + (bb2 != bbf));
      bbs_to_fix_dom.quick_push (bb0);
      bbs_to_fix_dom.quick_push (bb1);
      if (bb2 != bbf)
	bbs_to_fix_dom.quick_push (bb2);
      bbs_to_fix_dom.quick_push (bbf);

      iterate_fix_dominators (CDI_DOMINATORS, bbs_to_fix_dom, true);
      bbs_to_fix_dom.release ();
    }
}

/* The following function is invoked on every switch statement (the current one
   is given in SWTCH) and runs the individual phases of switch conversion on it
   one after another until one fails or the conversion is completed.
   Returns NULL on success, or a pointer to a string with the reason why the
   conversion failed.  */

static const char *
process_switch (gswitch *swtch)
{
  struct switch_conv_info info;

  /* Group case labels so that we get the right results from the heuristics
     that decide on the code generation approach for this switch.  */
  group_case_labels_stmt (swtch);

  /* If this switch is now a degenerate case with only a default label,
     there is nothing left for us to do.   */
  if (gimple_switch_num_labels (swtch) < 2)
    return "switch is a degenerate case";

  collect_switch_conv_info (swtch, &info);

  /* No error markers should reach here (they should be filtered out
     during gimplification).  */
  gcc_checking_assert (TREE_TYPE (info.index_expr) != error_mark_node);

  /* A switch on a constant should have been optimized in tree-cfg-cleanup.  */
  gcc_checking_assert (! TREE_CONSTANT (info.index_expr));

  if (info.uniq <= MAX_CASE_BIT_TESTS)
    {
      if (expand_switch_using_bit_tests_p (info.range_size,
					   info.uniq, info.count,
					   optimize_bb_for_speed_p
					     (gimple_bb (swtch))))
	{
	  if (dump_file)
	    fputs ("  expanding as bit test is preferable\n", dump_file);
	  emit_case_bit_tests (swtch, info.index_expr, info.range_min,
			       info.range_size, info.range_max);
	  loops_state_set (LOOPS_NEED_FIXUP);
	  return NULL;
	}

      if (info.uniq <= 2)
	/* This will be expanded as a decision tree in stmt.c:expand_case.  */
	return "  expanding as jumps is preferable";
    }

  /* If there is no common successor, we cannot do the transformation.  */
  if (! info.final_bb)
    return "no common successor to all case label target blocks found";

  /* Check the case label values are within reasonable range:  */
  if (!check_range (&info))
    {
      gcc_assert (info.reason);
      return info.reason;
    }

  /* For all the cases, see whether they are empty, the assignments they
     represent constant and so on...  */
  if (! check_all_empty_except_final (&info))
    {
      gcc_assert (info.reason);
      return info.reason;
    }
  if (!check_final_bb (swtch, &info))
    {
      gcc_assert (info.reason);
      return info.reason;
    }

  /* At this point all checks have passed and we can proceed with the
     transformation.  */

  create_temp_arrays (&info);
  gather_default_values (info.default_case_nonstandard
			 ? gimple_switch_label (swtch, 1)
			 : gimple_switch_default_label (swtch), &info);
  build_constructors (swtch, &info);

  build_arrays (swtch, &info); /* Build the static arrays and assignments.   */
  gen_inbound_check (swtch, &info);	/* Build the bounds check.  */

  /* Cleanup:  */
  free_temp_arrays (&info);
  return NULL;
}

/* The main function of the pass scans statements for switches and invokes
   process_switch on them.  */

namespace {

const pass_data pass_data_convert_switch =
{
  GIMPLE_PASS, /* type */
  "switchconv", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_SWITCH_CONVERSION, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_convert_switch : public gimple_opt_pass
{
public:
  pass_convert_switch (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_convert_switch, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_tree_switch_conversion != 0; }
  virtual unsigned int execute (function *);

}; // class pass_convert_switch

unsigned int
pass_convert_switch::execute (function *fun)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, fun)
  {
    const char *failure_reason;
    gimple *stmt = last_stmt (bb);
    if (stmt && gimple_code (stmt) == GIMPLE_SWITCH)
      {
	if (dump_file)
	  {
	    expanded_location loc = expand_location (gimple_location (stmt));

	    fprintf (dump_file, "beginning to process the following "
		     "SWITCH statement (%s:%d) : ------- \n",
		     loc.file, loc.line);
	    print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	    putc ('\n', dump_file);
	  }

	failure_reason = process_switch (as_a <gswitch *> (stmt));
	if (! failure_reason)
	  {
	    if (dump_file)
	      {
		fputs ("Switch converted\n", dump_file);
		fputs ("--------------------------------\n", dump_file);
	      }

	    /* Make no effort to update the post-dominator tree.  It is actually not
	       that hard for the transformations we have performed, but it is not
	       supported by iterate_fix_dominators.  */
	    free_dominance_info (CDI_POST_DOMINATORS);
	  }
	else
	  {
	    if (dump_file)
	      {
		fputs ("Bailing out - ", dump_file);
		fputs (failure_reason, dump_file);
		fputs ("\n--------------------------------\n", dump_file);
	      }
	  }
      }
  }

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_convert_switch (gcc::context *ctxt)
{
  return new pass_convert_switch (ctxt);
}

struct case_node
{
  case_node		*left;	/* Left son in binary tree.  */
  case_node		*right;	/* Right son in binary tree;
				   also node chain.  */
  case_node		*parent; /* Parent of node in binary tree.  */
  tree			low;	/* Lowest index value for this label.  */
  tree			high;	/* Highest index value for this label.  */
  basic_block		case_bb; /* Label to jump to when node matches.  */
  tree			case_label; /* Label to jump to when node matches.  */
  profile_probability   prob; /* Probability of taking this case.  */
  profile_probability   subtree_prob;  /* Probability of reaching subtree
					  rooted at this node.  */
};

typedef case_node *case_node_ptr;

static basic_block emit_case_nodes (basic_block, tree, case_node_ptr,
				    basic_block, tree, profile_probability,
				    tree, hash_map<tree, tree> *);
static bool node_has_low_bound (case_node_ptr, tree);
static bool node_has_high_bound (case_node_ptr, tree);
static bool node_is_bounded (case_node_ptr, tree);

/* Return the smallest number of different values for which it is best to use a
   jump-table instead of a tree of conditional branches.  */

static unsigned int
case_values_threshold (void)
{
  unsigned int threshold = PARAM_VALUE (PARAM_CASE_VALUES_THRESHOLD);

  if (threshold == 0)
    threshold = targetm.case_values_threshold ();

  return threshold;
}

/* Reset the aux field of all outgoing edges of basic block BB.  */

static inline void
reset_out_edges_aux (basic_block bb)
{
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->succs)
    e->aux = (void *) 0;
}

/* Compute the number of case labels that correspond to each outgoing edge of
   STMT.  Record this information in the aux field of the edge.  */

static inline void
compute_cases_per_edge (gswitch *stmt)
{
  basic_block bb = gimple_bb (stmt);
  reset_out_edges_aux (bb);
  int ncases = gimple_switch_num_labels (stmt);
  for (int i = ncases - 1; i >= 1; --i)
    {
      tree elt = gimple_switch_label (stmt, i);
      tree lab = CASE_LABEL (elt);
      basic_block case_bb = label_to_block_fn (cfun, lab);
      edge case_edge = find_edge (bb, case_bb);
      case_edge->aux = (void *) ((intptr_t) (case_edge->aux) + 1);
    }
}

/* Do the insertion of a case label into case_list.  The labels are
   fed to us in descending order from the sorted vector of case labels used
   in the tree part of the middle end.  So the list we construct is
   sorted in ascending order.

   LABEL is the case label to be inserted.  LOW and HIGH are the bounds
   against which the index is compared to jump to LABEL and PROB is the
   estimated probability LABEL is reached from the switch statement.  */

static case_node *
add_case_node (case_node *head, tree low, tree high, basic_block case_bb,
	       tree case_label, profile_probability prob,
	       object_allocator<case_node> &case_node_pool)
{
  case_node *r;

  gcc_checking_assert (low);
  gcc_checking_assert (high && (TREE_TYPE (low) == TREE_TYPE (high)));

  /* Add this label to the chain.  */
  r = case_node_pool.allocate ();
  r->low = low;
  r->high = high;
  r->case_bb = case_bb;
  r->case_label = case_label;
  r->parent = r->left = NULL;
  r->prob = prob;
  r->subtree_prob = prob;
  r->right = head;
  return r;
}

/* Dump ROOT, a list or tree of case nodes, to file.  */

static void
dump_case_nodes (FILE *f, case_node *root, int indent_step, int indent_level)
{
  if (root == 0)
    return;
  indent_level++;

  dump_case_nodes (f, root->left, indent_step, indent_level);

  fputs (";; ", f);
  fprintf (f, "%*s", indent_step * indent_level, "");
  print_dec (wi::to_wide (root->low), f, TYPE_SIGN (TREE_TYPE (root->low)));
  if (!tree_int_cst_equal (root->low, root->high))
    {
      fprintf (f, " ... ");
      print_dec (wi::to_wide (root->high), f,
		 TYPE_SIGN (TREE_TYPE (root->high)));
    }
  fputs ("\n", f);

  dump_case_nodes (f, root->right, indent_step, indent_level);
}

/* Take an ordered list of case nodes
   and transform them into a near optimal binary tree,
   on the assumption that any target code selection value is as
   likely as any other.

   The transformation is performed by splitting the ordered
   list into two equal sections plus a pivot.  The parts are
   then attached to the pivot as left and right branches.  Each
   branch is then transformed recursively.  */

static void
balance_case_nodes (case_node_ptr *head, case_node_ptr parent)
{
  case_node_ptr np;

  np = *head;
  if (np)
    {
      int i = 0;
      int ranges = 0;
      case_node_ptr *npp;
      case_node_ptr left;

      /* Count the number of entries on branch.  Also count the ranges.  */

      while (np)
	{
	  if (!tree_int_cst_equal (np->low, np->high))
	    ranges++;

	  i++;
	  np = np->right;
	}

      if (i > 2)
	{
	  /* Split this list if it is long enough for that to help.  */
	  npp = head;
	  left = *npp;

	  /* If there are just three nodes, split at the middle one.  */
	  if (i == 3)
	    npp = &(*npp)->right;
	  else
	    {
	      /* Find the place in the list that bisects the list's total cost,
		 where ranges count as 2.
		 Here I gets half the total cost.  */
	      i = (i + ranges + 1) / 2;
	      while (1)
		{
		  /* Skip nodes while their cost does not reach that amount.  */
		  if (!tree_int_cst_equal ((*npp)->low, (*npp)->high))
		    i--;
		  i--;
		  if (i <= 0)
		    break;
		  npp = &(*npp)->right;
		}
	    }
	  *head = np = *npp;
	  *npp = 0;
	  np->parent = parent;
	  np->left = left;

	  /* Optimize each of the two split parts.  */
	  balance_case_nodes (&np->left, np);
	  balance_case_nodes (&np->right, np);
	  np->subtree_prob = np->prob;
	  np->subtree_prob += np->left->subtree_prob;
	  np->subtree_prob += np->right->subtree_prob;
	}
      else
	{
	  /* Else leave this branch as one level,
	     but fill in `parent' fields.  */
	  np = *head;
	  np->parent = parent;
	  np->subtree_prob = np->prob;
	  for (; np->right; np = np->right)
	    {
	      np->right->parent = np;
	      (*head)->subtree_prob += np->right->subtree_prob;
	    }
	}
    }
}

/* Return true if a switch should be expanded as a decision tree.
   RANGE is the difference between highest and lowest case.
   UNIQ is number of unique case node targets, not counting the default case.
   COUNT is the number of comparisons needed, not counting the default case.  */

static bool
expand_switch_as_decision_tree_p (tree range,
				  unsigned int uniq ATTRIBUTE_UNUSED,
				  unsigned int count)
{
  int max_ratio;

  /* If neither casesi or tablejump is available, or flag_jump_tables
     over-ruled us, we really have no choice.  */
  if (!targetm.have_casesi () && !targetm.have_tablejump ())
    return true;
  if (!flag_jump_tables)
    return true;
#ifndef ASM_OUTPUT_ADDR_DIFF_ELT
  if (flag_pic)
    return true;
#endif

  /* If the switch is relatively small such that the cost of one
     indirect jump on the target are higher than the cost of a
     decision tree, go with the decision tree.

     If range of values is much bigger than number of values,
     or if it is too large to represent in a HOST_WIDE_INT,
     make a sequence of conditional branches instead of a dispatch.

     The definition of "much bigger" depends on whether we are
     optimizing for size or for speed.  If the former, the maximum
     ratio range/count = 3, because this was found to be the optimal
     ratio for size on i686-pc-linux-gnu, see PR11823.  The ratio
     10 is much older, and was probably selected after an extensive
     benchmarking investigation on numerous platforms.  Or maybe it
     just made sense to someone at some point in the history of GCC,
     who knows...  */
  max_ratio = optimize_insn_for_size_p () ? 3 : 10;
  if (count < case_values_threshold () || !tree_fits_uhwi_p (range)
      || compare_tree_int (range, max_ratio * count) > 0)
    return true;

  return false;
}

static void
fix_phi_operands_for_edge (edge e, hash_map<tree, tree> *phi_mapping)
{
  basic_block bb = e->dest;
  gphi_iterator gsi;
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();

      tree *definition = phi_mapping->get (gimple_phi_result (phi));
      if (definition)
	add_phi_arg (phi, *definition, e, UNKNOWN_LOCATION);
    }
}


/* Add an unconditional jump to CASE_BB that happens in basic block BB.  */

static void
emit_jump (basic_block bb, basic_block case_bb,
	   hash_map<tree, tree> *phi_mapping)
{
  edge e = single_succ_edge (bb);
  redirect_edge_succ (e, case_bb);
  fix_phi_operands_for_edge (e, phi_mapping);
}

/* Generate a decision tree, switching on INDEX_EXPR and jumping to
   one of the labels in CASE_LIST or to the DEFAULT_LABEL.
   DEFAULT_PROB is the estimated probability that it jumps to
   DEFAULT_LABEL.

   We generate a binary decision tree to select the appropriate target
   code.  */

static void
emit_case_decision_tree (gswitch *s, tree index_expr, tree index_type,
			 case_node_ptr case_list, basic_block default_bb,
			 tree default_label, profile_probability default_prob,
			 hash_map<tree, tree> *phi_mapping)
{
  balance_case_nodes (&case_list, NULL);

  if (dump_file)
    dump_function_to_file (current_function_decl, dump_file, dump_flags);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      int indent_step = ceil_log2 (TYPE_PRECISION (index_type)) + 2;
      fprintf (dump_file, ";; Expanding GIMPLE switch as decision tree:\n");
      dump_case_nodes (dump_file, case_list, indent_step, 0);
    }

  basic_block bb = gimple_bb (s);
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  edge e;
  if (gsi_end_p (gsi))
    e = split_block_after_labels (bb);
  else
    {
      gsi_prev (&gsi);
      e = split_block (bb, gsi_stmt (gsi));
    }
  bb = split_edge (e);

  bb = emit_case_nodes (bb, index_expr, case_list, default_bb, default_label,
			default_prob, index_type, phi_mapping);

  if (bb)
    emit_jump (bb, default_bb, phi_mapping);

  /* Remove all edges and do just an edge that will reach default_bb.  */
  gsi = gsi_last_bb (gimple_bb (s));
  gsi_remove (&gsi, true);
}

static void
record_phi_operand_mapping (const vec<basic_block> bbs, basic_block switch_bb,
			    hash_map <tree, tree> *map)
{
  /* Record all PHI nodes that have to be fixed after conversion.  */
  for (unsigned i = 0; i < bbs.length (); i++)
    {
      basic_block bb = bbs[i];

      gphi_iterator gsi;
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();

	  for (unsigned i = 0; i < gimple_phi_num_args (phi); i++)
	    {
	      basic_block phi_src_bb = gimple_phi_arg_edge (phi, i)->src;
	      if (phi_src_bb == switch_bb)
		{
		  tree def = gimple_phi_arg_def (phi, i);
		  tree result = gimple_phi_result (phi);
		  map->put (result, def);
		  break;
		}
	    }
	}
    }
}

/* Attempt to expand gimple switch STMT to a decision tree.  */

static bool
try_switch_expansion (gswitch *stmt)
{
  tree minval = NULL_TREE, maxval = NULL_TREE, range = NULL_TREE;
  basic_block default_bb;
  unsigned int count, uniq;
  int i;
  int ncases = gimple_switch_num_labels (stmt);
  tree index_expr = gimple_switch_index (stmt);
  tree index_type = TREE_TYPE (index_expr);
  tree elt;
  basic_block bb = gimple_bb (stmt);

  hash_map<tree, tree> phi_mapping;
  auto_vec<basic_block> case_bbs;

  /* A list of case labels; it is first built as a list and it may then
     be rearranged into a nearly balanced binary tree.  */
  case_node *case_list = 0;

  /* A pool for case nodes.  */
  object_allocator<case_node> case_node_pool ("struct case_node pool");

  /* cleanup_tree_cfg removes all SWITCH_EXPR with their index
     expressions being INTEGER_CST.  */
  gcc_assert (TREE_CODE (index_expr) != INTEGER_CST);

  if (ncases == 1)
    return false;

  /* Find the default case target label.  */
  tree default_label = CASE_LABEL (gimple_switch_default_label (stmt));
  default_bb = label_to_block_fn (cfun, default_label);
  edge default_edge = find_edge (bb, default_bb);
  profile_probability default_prob = default_edge->probability;
  case_bbs.safe_push (default_bb);

  /* Get upper and lower bounds of case values.  */
  elt = gimple_switch_label (stmt, 1);
  minval = fold_convert (index_type, CASE_LOW (elt));
  elt = gimple_switch_label (stmt, ncases - 1);
  if (CASE_HIGH (elt))
    maxval = fold_convert (index_type, CASE_HIGH (elt));
  else
    maxval = fold_convert (index_type, CASE_LOW (elt));

  /* Compute span of values.  */
  range = fold_build2 (MINUS_EXPR, index_type, maxval, minval);

  /* Listify the labels queue and gather some numbers to decide
     how to expand this switch.  */
  uniq = 0;
  count = 0;
  hash_set<tree> seen_labels;
  compute_cases_per_edge (stmt);

  for (i = ncases - 1; i >= 1; --i)
    {
      elt = gimple_switch_label (stmt, i);
      tree low = CASE_LOW (elt);
      gcc_assert (low);
      tree high = CASE_HIGH (elt);
      gcc_assert (!high || tree_int_cst_lt (low, high));
      tree lab = CASE_LABEL (elt);

      /* Count the elements.
	 A range counts double, since it requires two compares.  */
      count++;
      if (high)
	count++;

      /* If we have not seen this label yet, then increase the
	 number of unique case node targets seen.  */
      if (!seen_labels.add (lab))
	uniq++;

      /* The bounds on the case range, LOW and HIGH, have to be converted
	 to case's index type TYPE.  Note that the original type of the
	 case index in the source code is usually "lost" during
	 gimplification due to type promotion, but the case labels retain the
	 original type.  Make sure to drop overflow flags.  */
      low = fold_convert (index_type, low);
      if (TREE_OVERFLOW (low))
	low = wide_int_to_tree (index_type, wi::to_wide (low));

      /* The canonical from of a case label in GIMPLE is that a simple case
	 has an empty CASE_HIGH.  For the casesi and tablejump expanders,
	 the back ends want simple cases to have high == low.  */
      if (!high)
	high = low;
      high = fold_convert (index_type, high);
      if (TREE_OVERFLOW (high))
	high = wide_int_to_tree (index_type, wi::to_wide (high));

      basic_block case_bb = label_to_block_fn (cfun, lab);
      edge case_edge = find_edge (bb, case_bb);
      case_list = add_case_node (
	case_list, low, high, case_bb, lab,
	case_edge->probability.apply_scale (1, (intptr_t) (case_edge->aux)),
	case_node_pool);

      case_bbs.safe_push (case_bb);
    }
  reset_out_edges_aux (bb);
  record_phi_operand_mapping (case_bbs, bb, &phi_mapping);

  /* cleanup_tree_cfg removes all SWITCH_EXPR with a single
     destination, such as one with a default case only.
     It also removes cases that are out of range for the switch
     type, so we should never get a zero here.  */
  gcc_assert (count > 0);

  /* Decide how to expand this switch.
     The two options at this point are a dispatch table (casesi or
     tablejump) or a decision tree.  */

  if (expand_switch_as_decision_tree_p (range, uniq, count))
    {
      emit_case_decision_tree (stmt, index_expr, index_type, case_list,
			       default_bb, default_label, default_prob,
			       &phi_mapping);
      return true;
    }

  return false;
}

/* The main function of the pass scans statements for switches and invokes
   process_switch on them.  */

namespace {

const pass_data pass_data_lower_switch =
{
  GIMPLE_PASS, /* type */
  "switchlower", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_SWITCH_LOWERING, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa | TODO_cleanup_cfg, /* todo_flags_finish */
};

class pass_lower_switch : public gimple_opt_pass
{
public:
  pass_lower_switch (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_lower_switch, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return true; }
  virtual unsigned int execute (function *);

}; // class pass_lower_switch

unsigned int
pass_lower_switch::execute (function *fun)
{
  basic_block bb;
  bool expanded = false;

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple *stmt = last_stmt (bb);
      if (stmt && gimple_code (stmt) == GIMPLE_SWITCH)
	{
	  if (dump_file)
	    {
	      expanded_location loc = expand_location (gimple_location (stmt));

	      fprintf (dump_file, "beginning to process the following "
				  "SWITCH statement (%s:%d) : ------- \n",
		       loc.file, loc.line);
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	      putc ('\n', dump_file);
	    }

	  expanded |= try_switch_expansion (as_a<gswitch *> (stmt));
	}
    }

  if (expanded)
    {
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
      mark_virtual_operands_for_renaming (cfun);
    }

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_lower_switch (gcc::context *ctxt)
{
  return new pass_lower_switch (ctxt);
}

/* Generate code to jump to LABEL if OP0 and OP1 are equal in mode MODE.
   PROB is the probability of jumping to LABEL.  */
static basic_block
do_jump_if_equal (basic_block bb, tree op0, tree op1, basic_block label_bb,
		  profile_probability prob, hash_map<tree, tree> *phi_mapping)
{
  gcond *cond = gimple_build_cond (EQ_EXPR, op0, op1, NULL_TREE, NULL_TREE);
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gsi_insert_before (&gsi, cond, GSI_SAME_STMT);

  gcc_assert (single_succ_p (bb));

  /* Make a new basic block where false branch will take place.  */
  edge false_edge = split_block (bb, cond);
  false_edge->flags = EDGE_FALSE_VALUE;
  false_edge->probability = prob.invert ();

  edge true_edge = make_edge (bb, label_bb, EDGE_TRUE_VALUE);
  fix_phi_operands_for_edge (true_edge, phi_mapping);
  true_edge->probability = prob;

  return false_edge->dest;
}

/* Generate code to compare X with Y so that the condition codes are
   set and to jump to LABEL if the condition is true.  If X is a
   constant and Y is not a constant, then the comparison is swapped to
   ensure that the comparison RTL has the canonical form.

   UNSIGNEDP nonzero says that X and Y are unsigned; this matters if they
   need to be widened.  UNSIGNEDP is also used to select the proper
   branch condition code.

   If X and Y have mode BLKmode, then SIZE specifies the size of both X and Y.

   MODE is the mode of the inputs (in case they are const_int).

   COMPARISON is the rtl operator to compare with (EQ, NE, GT, etc.).
   It will be potentially converted into an unsigned variant based on
   UNSIGNEDP to select a proper jump instruction.

   PROB is the probability of jumping to LABEL.  */

static basic_block
emit_cmp_and_jump_insns (basic_block bb, tree op0, tree op1,
			 tree_code comparison, basic_block label_bb,
			 profile_probability prob,
			 hash_map<tree, tree> *phi_mapping)
{
  gcond *cond = gimple_build_cond (comparison, op0, op1, NULL_TREE, NULL_TREE);
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gsi_insert_after (&gsi, cond, GSI_NEW_STMT);

  gcc_assert (single_succ_p (bb));

  /* Make a new basic block where false branch will take place.  */
  edge false_edge = split_block (bb, cond);
  false_edge->flags = EDGE_FALSE_VALUE;
  false_edge->probability = prob.invert ();

  edge true_edge = make_edge (bb, label_bb, EDGE_TRUE_VALUE);
  fix_phi_operands_for_edge (true_edge, phi_mapping);
  true_edge->probability = prob;

  return false_edge->dest;
}

/* Computes the conditional probability of jumping to a target if the branch
   instruction is executed.
   TARGET_PROB is the estimated probability of jumping to a target relative
   to some basic block BB.
   BASE_PROB is the probability of reaching the branch instruction relative
   to the same basic block BB.  */

static inline profile_probability
conditional_probability (profile_probability target_prob,
			 profile_probability base_prob)
{
  return target_prob / base_prob;
}

/* Emit step-by-step code to select a case for the value of INDEX.
   The thus generated decision tree follows the form of the
   case-node binary tree NODE, whose nodes represent test conditions.
   INDEX_TYPE is the type of the index of the switch.

   Care is taken to prune redundant tests from the decision tree
   by detecting any boundary conditions already checked by
   emitted rtx.  (See node_has_high_bound, node_has_low_bound
   and node_is_bounded, above.)

   Where the test conditions can be shown to be redundant we emit
   an unconditional jump to the target code.  As a further
   optimization, the subordinates of a tree node are examined to
   check for bounded nodes.  In this case conditional and/or
   unconditional jumps as a result of the boundary check for the
   current node are arranged to target the subordinates associated
   code for out of bound conditions on the current node.

   We can assume that when control reaches the code generated here,
   the index value has already been compared with the parents
   of this node, and determined to be on the same side of each parent
   as this node is.  Thus, if this node tests for the value 51,
   and a parent tested for 52, we don't need to consider
   the possibility of a value greater than 51.  If another parent
   tests for the value 50, then this node need not test anything.  */

static basic_block
emit_case_nodes (basic_block bb, tree index, case_node_ptr node,
		 basic_block default_bb, tree default_label,
		 profile_probability default_prob, tree index_type,
		 hash_map<tree, tree> *phi_mapping)
{
  /* If INDEX has an unsigned type, we must make unsigned branches.  */
  profile_probability probability;
  profile_probability prob = node->prob, subtree_prob = node->subtree_prob;

  /* See if our parents have already tested everything for us.
     If they have, emit an unconditional jump for this node.  */
  if (node_is_bounded (node, index_type))
    {
      emit_jump (bb, node->case_bb, phi_mapping);
      return NULL;
    }

  else if (tree_int_cst_equal (node->low, node->high))
    {
      probability = conditional_probability (prob, subtree_prob + default_prob);
      /* Node is single valued.  First see if the index expression matches
	 this node and then check our children, if any.  */
      bb = do_jump_if_equal (bb, index, node->low, node->case_bb, probability,
			     phi_mapping);
      /* Since this case is taken at this point, reduce its weight from
	 subtree_weight.  */
      subtree_prob -= prob;
      if (node->right != 0 && node->left != 0)
	{
	  /* This node has children on both sides.
	     Dispatch to one side or the other
	     by comparing the index value with this node's value.
	     If one subtree is bounded, check that one first,
	     so we can avoid real branches in the tree.  */

	  if (node_is_bounded (node->right, index_type))
	    {
	      probability
		= conditional_probability (node->right->prob,
					   subtree_prob + default_prob);
	      bb = emit_cmp_and_jump_insns (bb, index, node->high, GT_EXPR,
					    node->right->case_bb, probability,
					    phi_mapping);
	      bb = emit_case_nodes (bb, index, node->left, default_bb,
				    default_label, default_prob, index_type,
				    phi_mapping);
	    }

	  else if (node_is_bounded (node->left, index_type))
	    {
	      probability
		= conditional_probability (node->left->prob,
					   subtree_prob + default_prob);
	      bb = emit_cmp_and_jump_insns (bb, index, node->high, LT_EXPR,
					    node->left->case_bb, probability,
					    phi_mapping);
	      bb = emit_case_nodes (bb, index, node->right, default_bb,
				    default_label, default_prob, index_type,
				    phi_mapping);
	    }

	  /* If both children are single-valued cases with no
	     children, finish up all the work.  This way, we can save
	     one ordered comparison.  */
	  else if (tree_int_cst_equal (node->right->low, node->right->high)
		   && node->right->left == 0 && node->right->right == 0
		   && tree_int_cst_equal (node->left->low, node->left->high)
		   && node->left->left == 0 && node->left->right == 0)
	    {
	      /* Neither node is bounded.  First distinguish the two sides;
		 then emit the code for one side at a time.  */

	      /* See if the value matches what the right hand side
		 wants.  */
	      probability
		= conditional_probability (node->right->prob,
					   subtree_prob + default_prob);
	      bb = do_jump_if_equal (bb, index, node->right->low,
				     node->right->case_bb, probability,
				     phi_mapping);

	      /* See if the value matches what the left hand side
		 wants.  */
	      probability
		= conditional_probability (node->left->prob,
					   subtree_prob + default_prob);
	      bb = do_jump_if_equal (bb, index, node->left->low,
				     node->left->case_bb, probability,
				     phi_mapping);
	    }

	  else
	    {
	      /* Neither node is bounded.  First distinguish the two sides;
		 then emit the code for one side at a time.  */

	      basic_block test_bb = split_edge (single_succ_edge (bb));
	      redirect_edge_succ (single_pred_edge (test_bb),
				  single_succ_edge (bb)->dest);

	      /* The default label could be reached either through the right
		 subtree or the left subtree.  Divide the probability
		 equally.  */
	      probability
		= conditional_probability (node->right->subtree_prob
					     + default_prob.apply_scale (1, 2),
					   subtree_prob + default_prob);
	      /* See if the value is on the right.  */
	      bb = emit_cmp_and_jump_insns (bb, index, node->high, GT_EXPR,
					    test_bb, probability, phi_mapping);
	      default_prob = default_prob.apply_scale (1, 2);

	      /* Value must be on the left.
		 Handle the left-hand subtree.  */
	      bb = emit_case_nodes (bb, index, node->left, default_bb,
				    default_label, default_prob, index_type,
				    phi_mapping);
	      /* If left-hand subtree does nothing,
		 go to default.  */

	      if (bb && default_bb)
		emit_jump (bb, default_bb, phi_mapping);

	      /* Code branches here for the right-hand subtree.  */
	      bb = emit_case_nodes (test_bb, index, node->right, default_bb,
				    default_label, default_prob, index_type,
				    phi_mapping);
	    }
	}
      else if (node->right != 0 && node->left == 0)
	{
	  /* Here we have a right child but no left so we issue a conditional
	     branch to default and process the right child.

	     Omit the conditional branch to default if the right child
	     does not have any children and is single valued; it would
	     cost too much space to save so little time.  */

	  if (node->right->right || node->right->left
	      || !tree_int_cst_equal (node->right->low, node->right->high))
	    {
	      if (!node_has_low_bound (node, index_type))
		{
		  probability
		    = conditional_probability (default_prob.apply_scale (1, 2),
					       subtree_prob + default_prob);
		  bb = emit_cmp_and_jump_insns (bb, index, node->high, LT_EXPR,
						default_bb, probability,
						phi_mapping);
		  default_prob = default_prob.apply_scale (1, 2);
		}

	      bb = emit_case_nodes (bb, index, node->right, default_bb,
				    default_label, default_prob, index_type,
				    phi_mapping);
	    }
	  else
	    {
	      probability
		= conditional_probability (node->right->subtree_prob,
					   subtree_prob + default_prob);
	      /* We cannot process node->right normally
		 since we haven't ruled out the numbers less than
		 this node's value.  So handle node->right explicitly.  */
	      bb = do_jump_if_equal (bb, index, node->right->low,
				     node->right->case_bb, probability,
				     phi_mapping);
	    }
	}

      else if (node->right == 0 && node->left != 0)
	{
	  /* Just one subtree, on the left.  */
	  if (node->left->left || node->left->right
	      || !tree_int_cst_equal (node->left->low, node->left->high))
	    {
	      if (!node_has_high_bound (node, index_type))
		{
		  probability
		    = conditional_probability (default_prob.apply_scale (1, 2),
					       subtree_prob + default_prob);
		  bb = emit_cmp_and_jump_insns (bb, index, node->high, GT_EXPR,
						default_bb, probability,
						phi_mapping);
		  default_prob = default_prob.apply_scale (1, 2);
		}

	      bb = emit_case_nodes (bb, index, node->left, default_bb,
				    default_label, default_prob, index_type,
				    phi_mapping);
	    }
	  else
	    {
	      probability
		= conditional_probability (node->left->subtree_prob,
					   subtree_prob + default_prob);
	      /* We cannot process node->left normally
		 since we haven't ruled out the numbers less than
		 this node's value.  So handle node->left explicitly.  */
	      do_jump_if_equal (bb, index, node->left->low, node->left->case_bb,
				probability, phi_mapping);
	    }
	}
    }
  else
    {
      /* Node is a range.  These cases are very similar to those for a single
	 value, except that we do not start by testing whether this node
	 is the one to branch to.  */

      if (node->right != 0 && node->left != 0)
	{
	  /* Node has subtrees on both sides.
	     If the right-hand subtree is bounded,
	     test for it first, since we can go straight there.
	     Otherwise, we need to make a branch in the control structure,
	     then handle the two subtrees.  */
	  basic_block test_bb = NULL;

	  if (node_is_bounded (node->right, index_type))
	    {
	      /* Right hand node is fully bounded so we can eliminate any
		 testing and branch directly to the target code.  */
	      probability
		= conditional_probability (node->right->subtree_prob,
					   subtree_prob + default_prob);
	      bb = emit_cmp_and_jump_insns (bb, index, node->high, GT_EXPR,
					    node->right->case_bb, probability,
					    phi_mapping);
	    }
	  else
	    {
	      /* Right hand node requires testing.
		 Branch to a label where we will handle it later.  */

	      test_bb = split_edge (single_succ_edge (bb));
	      redirect_edge_succ (single_pred_edge (test_bb),
				  single_succ_edge (bb)->dest);

	      probability
		= conditional_probability (node->right->subtree_prob
					     + default_prob.apply_scale (1, 2),
					   subtree_prob + default_prob);
	      bb = emit_cmp_and_jump_insns (bb, index, node->high, GT_EXPR,
					    test_bb, probability, phi_mapping);
	      default_prob = default_prob.apply_scale (1, 2);
	    }

	  /* Value belongs to this node or to the left-hand subtree.  */

	  probability
	    = conditional_probability (prob, subtree_prob + default_prob);
	  bb = emit_cmp_and_jump_insns (bb, index, node->low, GE_EXPR,
					node->case_bb, probability,
					phi_mapping);

	  /* Handle the left-hand subtree.  */
	  bb = emit_case_nodes (bb, index, node->left, default_bb,
				default_label, default_prob, index_type,
				phi_mapping);

	  /* If right node had to be handled later, do that now.  */
	  if (test_bb)
	    {
	      /* If the left-hand subtree fell through,
		 don't let it fall into the right-hand subtree.  */
	      if (bb && default_bb)
		emit_jump (bb, default_bb, phi_mapping);

	      bb = emit_case_nodes (test_bb, index, node->right, default_bb,
				    default_label, default_prob, index_type,
				    phi_mapping);
	    }
	}

      else if (node->right != 0 && node->left == 0)
	{
	  /* Deal with values to the left of this node,
	     if they are possible.  */
	  if (!node_has_low_bound (node, index_type))
	    {
	      probability
		= conditional_probability (default_prob.apply_scale (1, 2),
					   subtree_prob + default_prob);
	      bb = emit_cmp_and_jump_insns (bb, index, node->low, LT_EXPR,
					    default_bb, probability,
					    phi_mapping);
	      default_prob = default_prob.apply_scale (1, 2);
	    }

	  /* Value belongs to this node or to the right-hand subtree.  */

	  probability
	    = conditional_probability (prob, subtree_prob + default_prob);
	  bb = emit_cmp_and_jump_insns (bb, index, node->high, LE_EXPR,
					node->case_bb, probability,
					phi_mapping);

	  bb = emit_case_nodes (bb, index, node->right, default_bb,
				default_label, default_prob, index_type,
				phi_mapping);
	}

      else if (node->right == 0 && node->left != 0)
	{
	  /* Deal with values to the right of this node,
	     if they are possible.  */
	  if (!node_has_high_bound (node, index_type))
	    {
	      probability
		= conditional_probability (default_prob.apply_scale (1, 2),
					   subtree_prob + default_prob);
	      bb = emit_cmp_and_jump_insns (bb, index, node->high, GT_EXPR,
					    default_bb, probability,
					    phi_mapping);
	      default_prob = default_prob.apply_scale (1, 2);
	    }

	  /* Value belongs to this node or to the left-hand subtree.  */

	  probability
	    = conditional_probability (prob, subtree_prob + default_prob);
	  bb = emit_cmp_and_jump_insns (bb, index, node->low, GE_EXPR,
					node->case_bb, probability,
					phi_mapping);

	  bb = emit_case_nodes (bb, index, node->left, default_bb,
				default_label, default_prob, index_type,
				phi_mapping);
	}

      else
	{
	  /* Node has no children so we check low and high bounds to remove
	     redundant tests.  Only one of the bounds can exist,
	     since otherwise this node is bounded--a case tested already.  */
	  bool high_bound = node_has_high_bound (node, index_type);
	  bool low_bound = node_has_low_bound (node, index_type);

	  if (!high_bound && low_bound)
	    {
	      probability
		= conditional_probability (default_prob,
					   subtree_prob + default_prob);
	      bb = emit_cmp_and_jump_insns (bb, index, node->high, GT_EXPR,
					    default_bb, probability,
					    phi_mapping);
	    }

	  else if (!low_bound && high_bound)
	    {
	      probability
		= conditional_probability (default_prob,
					   subtree_prob + default_prob);
	      bb = emit_cmp_and_jump_insns (bb, index, node->low, LT_EXPR,
					    default_bb, probability,
					    phi_mapping);
	    }
	  else if (!low_bound && !high_bound)
	    {
	      tree lhs, rhs;
	      generate_range_test (bb, index, node->low, node->high,
				   &lhs, &rhs);
	      probability
		= conditional_probability (default_prob,
					   subtree_prob + default_prob);
	      bb = emit_cmp_and_jump_insns (bb, lhs, rhs, GT_EXPR,
					    default_bb, probability,
					    phi_mapping);
	    }

	  emit_jump (bb, node->case_bb, phi_mapping);
	  return NULL;
	}
    }

  return bb;
}

/* Search the parent sections of the case node tree
   to see if a test for the lower bound of NODE would be redundant.
   INDEX_TYPE is the type of the index expression.

   The instructions to generate the case decision tree are
   output in the same order as nodes are processed so it is
   known that if a parent node checks the range of the current
   node minus one that the current node is bounded at its lower
   span.  Thus the test would be redundant.  */

static bool
node_has_low_bound (case_node_ptr node, tree index_type)
{
  tree low_minus_one;
  case_node_ptr pnode;

  /* If the lower bound of this node is the lowest value in the index type,
     we need not test it.  */

  if (tree_int_cst_equal (node->low, TYPE_MIN_VALUE (index_type)))
    return true;

  /* If this node has a left branch, the value at the left must be less
     than that at this node, so it cannot be bounded at the bottom and
     we need not bother testing any further.  */

  if (node->left)
    return false;

  low_minus_one = fold_build2 (MINUS_EXPR, TREE_TYPE (node->low), node->low,
			       build_int_cst (TREE_TYPE (node->low), 1));

  /* If the subtraction above overflowed, we can't verify anything.
     Otherwise, look for a parent that tests our value - 1.  */

  if (!tree_int_cst_lt (low_minus_one, node->low))
    return false;

  for (pnode = node->parent; pnode; pnode = pnode->parent)
    if (tree_int_cst_equal (low_minus_one, pnode->high))
      return true;

  return false;
}

/* Search the parent sections of the case node tree
   to see if a test for the upper bound of NODE would be redundant.
   INDEX_TYPE is the type of the index expression.

   The instructions to generate the case decision tree are
   output in the same order as nodes are processed so it is
   known that if a parent node checks the range of the current
   node plus one that the current node is bounded at its upper
   span.  Thus the test would be redundant.  */

static bool
node_has_high_bound (case_node_ptr node, tree index_type)
{
  tree high_plus_one;
  case_node_ptr pnode;

  /* If there is no upper bound, obviously no test is needed.  */

  if (TYPE_MAX_VALUE (index_type) == NULL)
    return true;

  /* If the upper bound of this node is the highest value in the type
     of the index expression, we need not test against it.  */

  if (tree_int_cst_equal (node->high, TYPE_MAX_VALUE (index_type)))
    return true;

  /* If this node has a right branch, the value at the right must be greater
     than that at this node, so it cannot be bounded at the top and
     we need not bother testing any further.  */

  if (node->right)
    return false;

  high_plus_one = fold_build2 (PLUS_EXPR, TREE_TYPE (node->high), node->high,
			       build_int_cst (TREE_TYPE (node->high), 1));

  /* If the addition above overflowed, we can't verify anything.
     Otherwise, look for a parent that tests our value + 1.  */

  if (!tree_int_cst_lt (node->high, high_plus_one))
    return false;

  for (pnode = node->parent; pnode; pnode = pnode->parent)
    if (tree_int_cst_equal (high_plus_one, pnode->low))
      return true;

  return false;
}

/* Search the parent sections of the
   case node tree to see if both tests for the upper and lower
   bounds of NODE would be redundant.  */

static bool
node_is_bounded (case_node_ptr node, tree index_type)
{
  return (node_has_low_bound (node, index_type)
	  && node_has_high_bound (node, index_type));
}
