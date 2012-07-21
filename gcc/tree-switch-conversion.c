/* Lower GIMPLE_SWITCH expressions to something more efficient than
   a jump table.
   Copyright (C) 2006, 2008, 2009, 2010, 2011, 2012
   Free Software Foundation, Inc.

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
#include "tm.h"
#include "line-map.h"
#include "params.h"
#include "flags.h"
#include "tree.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "tree-flow-inline.h"
#include "tree-ssa-operands.h"
#include "tree-pass.h"
#include "gimple-pretty-print.h"

/* ??? For lang_hooks.types.type_for_mode, but is there a word_mode
   type in the GIMPLE type system that is language-independent?  */
#include "langhooks.h"

/* Need to include expr.h and optabs.h for lshift_cheap_p.  */
#include "expr.h"
#include "optabs.h"

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
  gimple cond_stmt;
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
  e_false->probability = REG_BR_PROB_BASE - e_true->probability;
  e_false->count = split_bb->count - e_true->count;
  new_bb->count = e_false->count;

  if (update_dominators)
    {
      if (dominated_e_true)
	set_immediate_dominator (CDI_DOMINATORS, e_true->dest, split_bb);
      set_immediate_dominator (CDI_DOMINATORS, e_false->dest, split_bb);
    }

  return new_bb;
}


/* Determine whether "1 << x" is relatively cheap in word_mode.  */
/* FIXME: This is the function that we need rtl.h and optabs.h for.
   This function (and similar RTL-related cost code in e.g. IVOPTS) should
   be moved to some kind of interface file for GIMPLE/RTL interactions.  */
static bool
lshift_cheap_p (void)
{
  /* FIXME: This should be made target dependent via this "this_target"
     mechanism, similar to e.g. can_copy_init_p in gcse.c.  */
  static bool init[2] = {false, false};
  static bool cheap[2] = {true, true};
  bool speed_p;

  /* If the targer has no lshift in word_mode, the operation will most
     probably not be cheap.  ??? Does GCC even work for such targets?  */
  if (optab_handler (ashl_optab, word_mode) == CODE_FOR_nothing)
    return false;

  speed_p = optimize_insn_for_speed_p ();

  if (!init[speed_p])
    {
      rtx reg = gen_raw_REG (word_mode, 10000);
      int cost = set_src_cost (gen_rtx_ASHIFT (word_mode, const1_rtx, reg),
			       speed_p);
      cheap[speed_p] = cost < COSTS_N_INSNS (MAX_CASE_BIT_TESTS);
      init[speed_p] = true;
    }

  return cheap[speed_p];
}

/* Return true if a switch should be expanded as a bit test.
   RANGE is the difference between highest and lowest case.
   UNIQ is number of unique case node targets, not counting the default case.
   COUNT is the number of comparisons needed, not counting the default case.  */

static bool
expand_switch_using_bit_tests_p (tree range,
				 unsigned int uniq,
				 unsigned int count)
{
  return (((uniq == 1 && count >= 3)
	   || (uniq == 2 && count >= 5)
	   || (uniq == 3 && count >= 6))
	  && lshift_cheap_p ()
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
  HOST_WIDE_INT hi;
  HOST_WIDE_INT lo;
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

  if (d2->target_edge->count != d1->target_edge->count)
    return d2->target_edge->count - d1->target_edge->count;
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

    There *MUST* be MAX_CASE_BIT_TESTS or less unique case
    node targets.  */

static void
emit_case_bit_tests (gimple swtch, tree index_expr,
		     tree minval, tree range)
{
  struct case_bit_test test[MAX_CASE_BIT_TESTS];
  unsigned int i, j, k;
  unsigned int count;

  basic_block switch_bb = gimple_bb (swtch);
  basic_block default_bb, new_default_bb, new_bb;
  edge default_edge;
  bool update_dom = dom_info_available_p (CDI_DOMINATORS);

  VEC (basic_block, heap) *bbs_to_fix_dom = NULL;

  tree index_type = TREE_TYPE (index_expr);
  tree unsigned_index_type = unsigned_type_for (index_type);
  unsigned int branch_num = gimple_switch_num_labels (swtch);

  gimple_stmt_iterator gsi;
  gimple shift_stmt;

  tree idx, tmp, csui;
  tree word_type_node = lang_hooks.types.type_for_mode (word_mode, 1);
  tree word_mode_zero = fold_convert (word_type_node, integer_zero_node);
  tree word_mode_one = fold_convert (word_type_node, integer_one_node);

  memset (&test, 0, sizeof (test));

  /* Get the edge for the default case.  */
  tmp = gimple_switch_label (swtch, 0);
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
	  test[k].hi = 0;
	  test[k].lo = 0;
	  test[k].target_edge = e;
	  test[k].label = label;
	  test[k].bits = 1;
	  count++;
	}
      else
        test[k].bits++;

      lo = tree_low_cst (int_const_binop (MINUS_EXPR,
					  CASE_LOW (cs), minval),
			 1);
      if (CASE_HIGH (cs) == NULL_TREE)
	hi = lo;
      else
	hi = tree_low_cst (int_const_binop (MINUS_EXPR, 
					    CASE_HIGH (cs), minval),
			   1);

      for (j = lo; j <= hi; j++)
        if (j >= HOST_BITS_PER_WIDE_INT)
	  test[k].hi |= (HOST_WIDE_INT) 1 << (j - HOST_BITS_PER_INT);
	else
	  test[k].lo |= (HOST_WIDE_INT) 1 << j;
    }

  qsort (test, count, sizeof(*test), case_bit_test_cmp);

  /* We generate two jumps to the default case label.
     Split the default edge, so that we don't have to do any PHI node
     updating.  */
  new_default_bb = split_edge (default_edge);

  if (update_dom)
    {
      bbs_to_fix_dom = VEC_alloc (basic_block, heap, 10);
      VEC_quick_push (basic_block, bbs_to_fix_dom, switch_bb);
      VEC_quick_push (basic_block, bbs_to_fix_dom, default_bb);
      VEC_quick_push (basic_block, bbs_to_fix_dom, new_default_bb);
    }

  /* Now build the test-and-branch code.  */

  gsi = gsi_last_bb (switch_bb);

  /* idx = (unsigned) (x - minval) */
  idx = fold_build2 (MINUS_EXPR, index_type, index_expr,
		     fold_convert (index_type, minval));
  idx = fold_convert (unsigned_index_type, idx);
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
    VEC_quick_push (basic_block, bbs_to_fix_dom, new_bb);
  gcc_assert (gimple_bb (swtch) == new_bb);
  gsi = gsi_last_bb (new_bb);

  /* Any blocks dominated by the GIMPLE_SWITCH, but that are not successors
     of NEW_BB, are still immediately dominated by SWITCH_BB.  Make it so.  */
  if (update_dom)
    {
      VEC (basic_block, heap) *dom_bbs;
      basic_block dom_son;

      dom_bbs = get_dominated_by (CDI_DOMINATORS, new_bb);
      FOR_EACH_VEC_ELT (basic_block, dom_bbs, i, dom_son)
	{
	  edge e = find_edge (new_bb, dom_son);
	  if (e && single_pred_p (e->dest))
	    continue;
	  set_immediate_dominator (CDI_DOMINATORS, dom_son, switch_bb);
	  VEC_safe_push (basic_block, heap, bbs_to_fix_dom, dom_son);
	}
      VEC_free (basic_block, heap, dom_bbs);
    }

  /* csui = (1 << (word_mode) idx) */
  tmp = create_tmp_var (word_type_node, "csui");
  add_referenced_var (tmp);
  csui = make_ssa_name (tmp, NULL);
  tmp = fold_build2 (LSHIFT_EXPR, word_type_node, word_mode_one,
		     fold_convert (word_type_node, idx));
  tmp = force_gimple_operand_gsi (&gsi, tmp,
				  /*simple=*/false, NULL_TREE,
				  /*before=*/true, GSI_SAME_STMT);
  shift_stmt = gimple_build_assign (csui, tmp);
  SSA_NAME_DEF_STMT (csui) = shift_stmt;
  gsi_insert_before (&gsi, shift_stmt, GSI_SAME_STMT);
  update_stmt (shift_stmt);

  /* for each unique set of cases:
        if (const & csui) goto target  */
  for (k = 0; k < count; k++)
    {
      tmp = build_int_cst_wide (word_type_node, test[k].lo, test[k].hi);
      tmp = fold_build2 (BIT_AND_EXPR, word_type_node, csui, tmp);
      tmp = force_gimple_operand_gsi (&gsi, tmp,
				      /*simple=*/true, NULL_TREE,
				      /*before=*/true, GSI_SAME_STMT);
      tmp = fold_build2 (NE_EXPR, boolean_type_node, tmp, word_mode_zero);
      new_bb = hoist_edge_and_branch_if_true (&gsi, tmp, test[k].target_edge,
					      update_dom);
      if (update_dom)
	VEC_safe_push (basic_block, heap, bbs_to_fix_dom, new_bb);
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
      VEC_free (basic_block, heap, bbs_to_fix_dom);
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
  int default_prob;

  /* The count of the default edge in the replaced switch.  */
  gcov_type default_count;

  /* Combined count of all other (non-default) edges in the replaced switch.  */
  gcov_type other_count;

  /* Number of phi nodes in the final bb (that we'll be replacing).  */
  int phi_count;

  /* Array of default values, in the same order as phi nodes.  */
  tree *default_values;

  /* Constructors of new static arrays.  */
  VEC (constructor_elt, gc) **constructors;

  /* Array of ssa names that are initialized with a value from a new static
     array.  */
  tree *target_inbound_names;

  /* Array of ssa names that are initialized with the default value if the
     switch expression is out of range.  */
  tree *target_outbound_names;

  /* The first load statement that loads a temporary from a new static array.
   */
  gimple arr_ref_first;

  /* The last load statement that loads a temporary from a new static array.  */
  gimple arr_ref_last;

  /* String reason why the case wasn't a good candidate that is written to the
     dump file, if there is one.  */
  const char *reason;

  /* Parameters for expand_switch_using_bit_tests.  Should be computed
     the same way as in expand_case.  */
  unsigned int uniq;
  unsigned int count;
};

/* Collect information about GIMPLE_SWITCH statement SWTCH into INFO.  */

static void
collect_switch_conv_info (gimple swtch, struct switch_conv_info *info)
{
  unsigned int branch_num = gimple_switch_num_labels (swtch);
  tree min_case, max_case;
  unsigned int count, i;
  edge e, e_default;
  edge_iterator ei;

  memset (info, 0, sizeof (*info));

  /* The gimplifier has already sorted the cases by CASE_LOW and ensured there
     is a default label which is the first in the vector.  */
  gcc_assert (CASE_LOW (gimple_switch_label (swtch, 0)) == NULL_TREE);

  /* Collect the bits we can deduce from the CFG.  */
  info->index_expr = gimple_switch_index (swtch);
  info->switch_bb = gimple_bb (swtch);
  info->default_bb =
    label_to_block (CASE_LABEL (gimple_switch_label (swtch, 0)));
  e_default = find_edge (info->switch_bb, info->default_bb);
  info->default_prob = e_default->probability;
  info->default_count = e_default->count;
  FOR_EACH_EDGE (e, ei, info->switch_bb->succs)
    if (e != e_default)
      info->other_count += e->count;

  /* See if there is one common successor block for all branch
     targets.  If it exists, record it in FINAL_BB.  */
  FOR_EACH_EDGE (e, ei, info->switch_bb->succs)
    {
      if (! single_pred_p (e->dest))
	{
	  info->final_bb = e->dest;
	  break;
	}
    }
  if (info->final_bb)
    FOR_EACH_EDGE (e, ei, info->switch_bb->succs)
      {
	if (e->dest == info->final_bb)
	  continue;

	if (single_pred_p (e->dest)
	    && single_succ_p (e->dest)
	    && single_succ (e->dest) == info->final_bb)
	  continue;

	info->final_bb = NULL;
	break;
      }

  /* Get upper and lower bounds of case values, and the covered range.  */
  min_case = gimple_switch_label (swtch, 1);
  max_case = gimple_switch_label (swtch, branch_num - 1);

  info->range_min = CASE_LOW (min_case);
  if (CASE_HIGH (max_case) != NULL_TREE)
    info->range_max = CASE_HIGH (max_case);
  else
    info->range_max = CASE_LOW (max_case);

  info->range_size =
    int_const_binop (MINUS_EXPR, info->range_max, info->range_min);

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
  if (!host_integerp (info->range_size, 1))
    {
      info->reason = "index range way too large or otherwise unusable";
      return false;
    }

  if ((unsigned HOST_WIDE_INT) tree_low_cst (info->range_size, 1)
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
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, info->switch_bb->succs)
    {
      if (e->dest == info->final_bb)
	continue;

      if (!empty_block_p (e->dest))
	{
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
check_final_bb (struct switch_conv_info *info)
{
  gimple_stmt_iterator gsi;

  info->phi_count = 0;
  for (gsi = gsi_start_phis (info->final_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      unsigned int i;

      info->phi_count++;

      for (i = 0; i < gimple_phi_num_args (phi); i++)
	{
	  basic_block bb = gimple_phi_arg_edge (phi, i)->src;

	  if (bb == info->switch_bb
	      || (single_pred_p (bb) && single_pred (bb) == info->switch_bb))
	    {
	      tree reloc, val;

	      val = gimple_phi_arg_def (phi, i);
	      if (!is_gimple_ip_invariant (val))
		{
		  info->reason = "non-invariant value from a case";
		  return false; /* Non-invariant argument.  */
		}
	      reloc = initializer_constant_valid_p (val, TREE_TYPE (val));
	      if ((flag_pic && reloc != null_pointer_node)
		  || (!flag_pic && reloc == NULL_TREE))
		{
		  if (reloc)
		    info->reason
		      = "value from a case would need runtime relocations";
		  else
		    info->reason
		      = "value from a case is not a valid initializer";
		  return false;
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
  info->constructors = XCNEWVEC (VEC (constructor_elt, gc) *, info->phi_count);
  info->target_inbound_names = info->default_values + info->phi_count;
  info->target_outbound_names = info->target_inbound_names + info->phi_count;
  for (i = 0; i < info->phi_count; i++)
    info->constructors[i]
      = VEC_alloc (constructor_elt, gc, tree_low_cst (info->range_size, 1) + 1);
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
   DEFAULT_CASE is the CASE_LABEL_EXPR for the default switch branch.  */

static void
gather_default_values (tree default_case, struct switch_conv_info *info)
{
  gimple_stmt_iterator gsi;
  basic_block bb = label_to_block (CASE_LABEL (default_case));
  edge e;
  int i = 0;

  gcc_assert (CASE_LOW (default_case) == NULL_TREE);

  if (bb == info->final_bb)
    e = find_edge (info->switch_bb, bb);
  else
    e = single_succ_edge (bb);

  for (gsi = gsi_start_phis (info->final_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      tree val = PHI_ARG_DEF_FROM_EDGE (phi, e);
      gcc_assert (val);
      info->default_values[i++] = val;
    }
}

/* The following function populates the vectors in the constructors array with
   future contents of the static arrays.  The vectors are populated in the
   order of phi nodes.  SWTCH is the switch statement being converted.  */

static void
build_constructors (gimple swtch, struct switch_conv_info *info)
{
  unsigned i, branch_num = gimple_switch_num_labels (swtch);
  tree pos = info->range_min;

  for (i = 1; i < branch_num; i++)
    {
      tree cs = gimple_switch_label (swtch, i);
      basic_block bb = label_to_block (CASE_LABEL (cs));
      edge e;
      tree high;
      gimple_stmt_iterator gsi;
      int j;

      if (bb == info->final_bb)
	e = find_edge (info->switch_bb, bb);
      else
	e = single_succ_edge (bb);
      gcc_assert (e);

      while (tree_int_cst_lt (pos, CASE_LOW (cs)))
	{
	  int k;
	  for (k = 0; k < info->phi_count; k++)
	    {
	      constructor_elt *elt;

	      elt = VEC_quick_push (constructor_elt,
				    info->constructors[k], NULL);
	      elt->index = int_const_binop (MINUS_EXPR, pos,
					    info->range_min);
	      elt->value = info->default_values[k];
	    }

	  pos = int_const_binop (PLUS_EXPR, pos, integer_one_node);
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
	  gimple phi = gsi_stmt (gsi);
	  tree val = PHI_ARG_DEF_FROM_EDGE (phi, e);
	  tree low = CASE_LOW (cs);
	  pos = CASE_LOW (cs);

	  do
	    {
	      constructor_elt *elt;

	      elt = VEC_quick_push (constructor_elt,
				    info->constructors[j], NULL);
	      elt->index = int_const_binop (MINUS_EXPR, pos, info->range_min);
	      elt->value = val;

	      pos = int_const_binop (PLUS_EXPR, pos, integer_one_node);
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
constructor_contains_same_values_p (VEC (constructor_elt, gc) *vec)
{
  unsigned int i;
  tree prev = NULL_TREE;
  constructor_elt *elt;

  FOR_EACH_VEC_ELT (constructor_elt, vec, i, elt)
    {
      if (!prev)
	prev = elt->value;
      else if (!operand_equal_p (elt->value, prev, OEP_ONLY_CONST))
	return NULL_TREE;
    }
  return prev;
}

/* Return type which should be used for array elements, either TYPE,
   or for integral type some smaller integral type that can still hold
   all the constants.  */

static tree
array_value_type (gimple swtch, tree type, int num,
		  struct switch_conv_info *info)
{
  unsigned int i, len = VEC_length (constructor_elt, info->constructors[num]);
  constructor_elt *elt;
  enum machine_mode mode;
  int sign = 0;
  tree smaller_type;

  if (!INTEGRAL_TYPE_P (type))
    return type;

  mode = GET_CLASS_NARROWEST_MODE (GET_MODE_CLASS (TYPE_MODE (type)));
  if (GET_MODE_SIZE (TYPE_MODE (type)) <= GET_MODE_SIZE (mode))
    return type;

  if (len < (optimize_bb_for_size_p (gimple_bb (swtch)) ? 2 : 32))
    return type;

  FOR_EACH_VEC_ELT (constructor_elt, info->constructors[num], i, elt)
    {
      double_int cst;

      if (TREE_CODE (elt->value) != INTEGER_CST)
	return type;

      cst = TREE_INT_CST (elt->value);
      while (1)
	{
	  unsigned int prec = GET_MODE_BITSIZE (mode);
	  if (prec > HOST_BITS_PER_WIDE_INT)
	    return type;

	  if (sign >= 0
	      && double_int_equal_p (cst, double_int_zext (cst, prec)))
	    {
	      if (sign == 0
		  && double_int_equal_p (cst, double_int_sext (cst, prec)))
		break;
	      sign = 1;
	      break;
	    }
	  if (sign <= 0
	      && double_int_equal_p (cst, double_int_sext (cst, prec)))
	    {
	      sign = -1;
	      break;
	    }

	  if (sign == 1)
	    sign = 0;

	  mode = GET_MODE_WIDER_MODE (mode);
	  if (mode == VOIDmode
	      || GET_MODE_SIZE (mode) >= GET_MODE_SIZE (TYPE_MODE (type)))
	    return type;
	}
    }

  if (sign == 0)
    sign = TYPE_UNSIGNED (type) ? 1 : -1;
  smaller_type = lang_hooks.types.type_for_mode (mode, sign >= 0);
  if (GET_MODE_SIZE (TYPE_MODE (type))
      <= GET_MODE_SIZE (TYPE_MODE (smaller_type)))
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
build_one_array (gimple swtch, int num, tree arr_index_type, gimple phi,
		 tree tidx, struct switch_conv_info *info)
{
  tree name, cst;
  gimple load;
  gimple_stmt_iterator gsi = gsi_for_stmt (swtch);
  location_t loc = gimple_location (swtch);

  gcc_assert (info->default_values[num]);

  name = make_ssa_name (SSA_NAME_VAR (PHI_RESULT (phi)), NULL);
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

	  FOR_EACH_VEC_ELT (constructor_elt, info->constructors[num], i, elt)
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
      TREE_CONSTANT (decl) = 1;
      TREE_READONLY (decl) = 1;
      varpool_finalize_decl (decl);

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

  SSA_NAME_DEF_STMT (name) = load;
  gsi_insert_before (&gsi, load, GSI_SAME_STMT);
  update_stmt (load);
  info->arr_ref_last = load;
}

/* Builds and initializes static arrays initialized with values gathered from
   the SWTCH switch statement.  Also creates statements that load values from
   them.  */

static void
build_arrays (gimple swtch, struct switch_conv_info *info)
{
  tree arr_index_type;
  tree tidx, sub, tmp, utype;
  gimple stmt;
  gimple_stmt_iterator gsi;
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
  tmp = create_tmp_var (utype, "csui");
  add_referenced_var (tmp);
  tidx = make_ssa_name (tmp, NULL);
  sub = fold_build2_loc (loc, MINUS_EXPR, utype,
			 fold_convert_loc (loc, utype, info->index_expr),
			 fold_convert_loc (loc, utype, info->range_min));
  sub = force_gimple_operand_gsi (&gsi, sub,
				  false, NULL, true, GSI_SAME_STMT);
  stmt = gimple_build_assign (tidx, sub);
  SSA_NAME_DEF_STMT (tidx) = stmt;

  gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
  update_stmt (stmt);
  info->arr_ref_first = stmt;

  for (gsi = gsi_start_phis (info->final_bb), i = 0;
       !gsi_end_p (gsi); gsi_next (&gsi), i++)
    build_one_array (swtch, i, arr_index_type, gsi_stmt (gsi), tidx, info);
}

/* Generates and appropriately inserts loads of default values at the position
   given by BSI.  Returns the last inserted statement.  */

static gimple
gen_def_assigns (gimple_stmt_iterator *gsi, struct switch_conv_info *info)
{
  int i;
  gimple assign = NULL;

  for (i = 0; i < info->phi_count; i++)
    {
      tree name
	= make_ssa_name (SSA_NAME_VAR (info->target_inbound_names[i]), NULL);

      info->target_outbound_names[i] = name;
      assign = gimple_build_assign (name, info->default_values[i]);
      SSA_NAME_DEF_STMT (name) = assign;
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
prune_bbs (basic_block bbd, basic_block final)
{
  edge_iterator ei;
  edge e;

  for (ei = ei_start (bbd->succs); (e = ei_safe_edge (ei)); )
    {
      basic_block bb;
      bb = e->dest;
      remove_edge (e);
      if (bb != final)
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
  gimple_stmt_iterator gsi;
  int i;

  for (gsi = gsi_start_phis (bbf), i = 0;
       !gsi_end_p (gsi); gsi_next (&gsi), i++)
    {
      gimple phi = gsi_stmt (gsi);
      add_phi_arg (phi, info->target_inbound_names[i], e1f, UNKNOWN_LOCATION);
      add_phi_arg (phi, info->target_outbound_names[i], e2f, UNKNOWN_LOCATION);
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
gen_inbound_check (gimple swtch, struct switch_conv_info *info)
{
  tree label_decl1 = create_artificial_label (UNKNOWN_LOCATION);
  tree label_decl2 = create_artificial_label (UNKNOWN_LOCATION);
  tree label_decl3 = create_artificial_label (UNKNOWN_LOCATION);
  gimple label1, label2, label3;
  tree utype, tidx;
  tree bound;

  gimple cond_stmt;

  gimple last_assign;
  gimple_stmt_iterator gsi;
  basic_block bb0, bb1, bb2, bbf, bbd;
  edge e01, e02, e21, e1d, e1f, e2f;
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
  label2 = gimple_build_label (label_decl2);
  gsi_insert_before (&gsi, label2, GSI_SAME_STMT);
  last_assign = gen_def_assigns (&gsi, info);

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

  e21 = split_block (bb2, last_assign);
  bb1 = e21->dest;
  remove_edge (e21);

  e1d = split_block (bb1, info->arr_ref_last);
  bbd = e1d->dest;
  remove_edge (e1d);

  /* flags and profiles of the edge for in-range values */
  e01 = make_edge (bb0, bb1, EDGE_TRUE_VALUE);
  e01->probability = REG_BR_PROB_BASE - info->default_prob;
  e01->count = info->other_count;

  /* flags and profiles of the edge taking care of out-of-range values */
  e02->flags &= ~EDGE_FALLTHRU;
  e02->flags |= EDGE_FALSE_VALUE;
  e02->probability = info->default_prob;
  e02->count = info->default_count;

  bbf = info->final_bb;

  e1f = make_edge (bb1, bbf, EDGE_FALLTHRU);
  e1f->probability = REG_BR_PROB_BASE;
  e1f->count = info->other_count;

  e2f = make_edge (bb2, bbf, EDGE_FALLTHRU);
  e2f->probability = REG_BR_PROB_BASE;
  e2f->count = info->default_count;

  /* frequencies of the new BBs */
  bb1->frequency = EDGE_FREQUENCY (e01);
  bb2->frequency = EDGE_FREQUENCY (e02);
  bbf->frequency = EDGE_FREQUENCY (e1f) + EDGE_FREQUENCY (e2f);

  /* Tidy blocks that have become unreachable.  */
  prune_bbs (bbd, info->final_bb);

  /* Fixup the PHI nodes in bbF.  */
  fix_phi_nodes (e1f, e2f, bbf, info);

  /* Fix the dominator tree, if it is available.  */
  if (dom_info_available_p (CDI_DOMINATORS))
    {
      VEC (basic_block, heap) *bbs_to_fix_dom;

      set_immediate_dominator (CDI_DOMINATORS, bb1, bb0);
      set_immediate_dominator (CDI_DOMINATORS, bb2, bb0);
      if (! get_immediate_dominator (CDI_DOMINATORS, bbf))
	/* If bbD was the immediate dominator ...  */
	set_immediate_dominator (CDI_DOMINATORS, bbf, bb0);

      bbs_to_fix_dom = VEC_alloc (basic_block, heap, 4);
      VEC_quick_push (basic_block, bbs_to_fix_dom, bb0);
      VEC_quick_push (basic_block, bbs_to_fix_dom, bb1);
      VEC_quick_push (basic_block, bbs_to_fix_dom, bb2);
      VEC_quick_push (basic_block, bbs_to_fix_dom, bbf);

      iterate_fix_dominators (CDI_DOMINATORS, bbs_to_fix_dom, true);
      VEC_free (basic_block, heap, bbs_to_fix_dom);
    }
}

/* The following function is invoked on every switch statement (the current one
   is given in SWTCH) and runs the individual phases of switch conversion on it
   one after another until one fails or the conversion is completed.
   Returns NULL on success, or a pointer to a string with the reason why the
   conversion failed.  */

static const char *
process_switch (gimple swtch)
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
					   info.uniq, info.count))
	{
	  if (dump_file)
	    fputs ("  expanding as bit test is preferable\n", dump_file);
	  emit_case_bit_tests (swtch, info.index_expr,
			       info.range_min, info.range_size);
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
  if (!check_final_bb (&info))
    {
      gcc_assert (info.reason);
      return info.reason;
    }

  /* At this point all checks have passed and we can proceed with the
     transformation.  */

  create_temp_arrays (&info);
  gather_default_values (gimple_switch_label (swtch, 0), &info);
  build_constructors (swtch, &info);

  build_arrays (swtch, &info); /* Build the static arrays and assignments.   */
  gen_inbound_check (swtch, &info);	/* Build the bounds check.  */

  /* Cleanup:  */
  free_temp_arrays (&info);
  return NULL;
}

/* The main function of the pass scans statements for switches and invokes
   process_switch on them.  */

static unsigned int
do_switchconv (void)
{
  basic_block bb;

  FOR_EACH_BB (bb)
  {
    const char *failure_reason;
    gimple stmt = last_stmt (bb);
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

	failure_reason = process_switch (stmt);
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

/* The pass gate. */

static bool
switchconv_gate (void)
{
  return flag_tree_switch_conversion != 0;
}

struct gimple_opt_pass pass_convert_switch =
{
 {
  GIMPLE_PASS,
  "switchconv",				/* name */
  switchconv_gate,			/* gate */
  do_switchconv,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_SWITCH_CONVERSION,		/* tv_id */
  PROP_cfg | PROP_ssa,	                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_update_ssa 
  | TODO_ggc_collect | TODO_verify_ssa
  | TODO_verify_stmts
  | TODO_verify_flow			/* todo_flags_finish */
 }
};
