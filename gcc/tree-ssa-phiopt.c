/* Optimization of PHI nodes by converting them into straightline code.
   Copyright (C) 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

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
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "rtl.h"
#include "flags.h"
#include "tm_p.h"
#include "basic-block.h"
#include "timevar.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-dump.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "domwalk.h"

static unsigned int tree_ssa_phiopt_worker (bool);
static bool conditional_replacement (basic_block, basic_block,
				     edge, edge, tree, tree, tree);
static bool value_replacement (basic_block, basic_block,
			       edge, edge, tree, tree, tree);
static bool minmax_replacement (basic_block, basic_block,
				edge, edge, tree, tree, tree);
static bool abs_replacement (basic_block, basic_block,
			     edge, edge, tree, tree, tree);
static bool cond_store_replacement (basic_block, basic_block, edge, edge,
				    struct pointer_set_t *);
static struct pointer_set_t * get_non_trapping (void);
static void replace_phi_edge_with_variable (basic_block, edge, tree, tree);

/* This pass tries to replaces an if-then-else block with an
   assignment.  We have four kinds of transformations.  Some of these
   transformations are also performed by the ifcvt RTL optimizer.

   Conditional Replacement
   -----------------------

   This transformation, implemented in conditional_replacement,
   replaces

     bb0:
      if (cond) goto bb2; else goto bb1;
     bb1:
     bb2:
      x = PHI <0 (bb1), 1 (bb0), ...>;

   with

     bb0:
      x' = cond;
      goto bb2;
     bb2:
      x = PHI <x' (bb0), ...>;

   We remove bb1 as it becomes unreachable.  This occurs often due to
   gimplification of conditionals.

   Value Replacement
   -----------------

   This transformation, implemented in value_replacement, replaces

     bb0:
       if (a != b) goto bb2; else goto bb1;
     bb1:
     bb2:
       x = PHI <a (bb1), b (bb0), ...>;

   with

     bb0:
     bb2:
       x = PHI <b (bb0), ...>;

   This opportunity can sometimes occur as a result of other
   optimizations.

   ABS Replacement
   ---------------

   This transformation, implemented in abs_replacement, replaces

     bb0:
       if (a >= 0) goto bb2; else goto bb1;
     bb1:
       x = -a;
     bb2:
       x = PHI <x (bb1), a (bb0), ...>;

   with

     bb0:
       x' = ABS_EXPR< a >;
     bb2:
       x = PHI <x' (bb0), ...>;

   MIN/MAX Replacement
   -------------------

   This transformation, minmax_replacement replaces

     bb0:
       if (a <= b) goto bb2; else goto bb1;
     bb1:
     bb2:
       x = PHI <b (bb1), a (bb0), ...>;

   with

     bb0:
       x' = MIN_EXPR (a, b)
     bb2:
       x = PHI <x' (bb0), ...>;

   A similar transformation is done for MAX_EXPR.  */

static unsigned int
tree_ssa_phiopt (void)
{
  return tree_ssa_phiopt_worker (false);
}

/* This pass tries to transform conditional stores into unconditional
   ones, enabling further simplifications with the simpler then and else
   blocks.  In particular it replaces this:

     bb0:
       if (cond) goto bb2; else goto bb1;
     bb1:
       *p = RHS
     bb2:

   with

     bb0:
       if (cond) goto bb1; else goto bb2;
     bb1:
       condtmp' = *p;
     bb2:
       condtmp = PHI <RHS, condtmp'>
       *p = condtmp

   This transformation can only be done under several constraints,
   documented below.  */

static unsigned int
tree_ssa_cs_elim (void)
{
  return tree_ssa_phiopt_worker (true);
}

/* For conditional store replacement we need a temporary to
   put the old contents of the memory in.  */
static tree condstoretemp;

/* The core routine of conditional store replacement and normal
   phi optimizations.  Both share much of the infrastructure in how
   to match applicable basic block patterns.  DO_STORE_ELIM is true
   when we want to do conditional store replacement, false otherwise.  */
static unsigned int
tree_ssa_phiopt_worker (bool do_store_elim)
{
  basic_block bb;
  basic_block *bb_order;
  unsigned n, i;
  bool cfgchanged = false;
  struct pointer_set_t *nontrap = 0;

  if (do_store_elim)
    {
      condstoretemp = NULL_TREE;
      /* Calculate the set of non-trapping memory accesses.  */
      nontrap = get_non_trapping ();
    }

  /* Search every basic block for COND_EXPR we may be able to optimize.

     We walk the blocks in order that guarantees that a block with
     a single predecessor is processed before the predecessor.
     This ensures that we collapse inner ifs before visiting the
     outer ones, and also that we do not try to visit a removed
     block.  */
  bb_order = blocks_in_phiopt_order ();
  n = n_basic_blocks - NUM_FIXED_BLOCKS;

  for (i = 0; i < n; i++) 
    {
      tree cond_expr;
      tree phi;
      basic_block bb1, bb2;
      edge e1, e2;
      tree arg0, arg1;

      bb = bb_order[i];

      cond_expr = last_stmt (bb);
      /* Check to see if the last statement is a COND_EXPR.  */
      if (!cond_expr
          || TREE_CODE (cond_expr) != COND_EXPR)
        continue;

      e1 = EDGE_SUCC (bb, 0);
      bb1 = e1->dest;
      e2 = EDGE_SUCC (bb, 1);
      bb2 = e2->dest;

      /* We cannot do the optimization on abnormal edges.  */
      if ((e1->flags & EDGE_ABNORMAL) != 0
          || (e2->flags & EDGE_ABNORMAL) != 0)
       continue;

      /* If either bb1's succ or bb2 or bb2's succ is non NULL.  */
      if (EDGE_COUNT (bb1->succs) == 0
          || bb2 == NULL
	  || EDGE_COUNT (bb2->succs) == 0)
        continue;

      /* Find the bb which is the fall through to the other.  */
      if (EDGE_SUCC (bb1, 0)->dest == bb2)
        ;
      else if (EDGE_SUCC (bb2, 0)->dest == bb1)
        {
	  basic_block bb_tmp = bb1;
	  edge e_tmp = e1;
	  bb1 = bb2;
	  bb2 = bb_tmp;
	  e1 = e2;
	  e2 = e_tmp;
	}
      else
        continue;

      e1 = EDGE_SUCC (bb1, 0);

      /* Make sure that bb1 is just a fall through.  */
      if (!single_succ_p (bb1)
	  || (e1->flags & EDGE_FALLTHRU) == 0)
        continue;

      /* Also make sure that bb1 only have one predecessor and that it
	 is bb.  */
      if (!single_pred_p (bb1)
          || single_pred (bb1) != bb)
	continue;

      if (do_store_elim)
	{
	  /* bb1 is the middle block, bb2 the join block, bb the split block,
	     e1 the fallthrough edge from bb1 to bb2.  We can't do the
	     optimization if the join block has more than two predecessors.  */
	  if (EDGE_COUNT (bb2->preds) > 2)
	    continue;
	  if (cond_store_replacement (bb1, bb2, e1, e2, nontrap))
	    cfgchanged = true;
	}
      else
	{
	  phi = phi_nodes (bb2);

	  /* Check to make sure that there is only one PHI node.
	     TODO: we could do it with more than one iff the other PHI nodes
	     have the same elements for these two edges.  */
	  if (!phi || PHI_CHAIN (phi) != NULL)
	    continue;

	  arg0 = PHI_ARG_DEF_TREE (phi, e1->dest_idx);
	  arg1 = PHI_ARG_DEF_TREE (phi, e2->dest_idx);

	  /* Something is wrong if we cannot find the arguments in the PHI
	     node.  */
	  gcc_assert (arg0 != NULL && arg1 != NULL);

	  /* Do the replacement of conditional if it can be done.  */
	  if (conditional_replacement (bb, bb1, e1, e2, phi, arg0, arg1))
	    cfgchanged = true;
	  else if (value_replacement (bb, bb1, e1, e2, phi, arg0, arg1))
	    cfgchanged = true;
	  else if (abs_replacement (bb, bb1, e1, e2, phi, arg0, arg1))
	    cfgchanged = true;
	  else if (minmax_replacement (bb, bb1, e1, e2, phi, arg0, arg1))
	    cfgchanged = true;
	}
    }

  free (bb_order);
  
  if (do_store_elim)
    pointer_set_destroy (nontrap);
  /* If the CFG has changed, we should cleanup the CFG.  */
  if (cfgchanged && do_store_elim)
    {
      /* In cond-store replacement we have added some loads on edges
         and new VOPS (as we moved the store, and created a load).  */
      bsi_commit_edge_inserts ();
      return TODO_cleanup_cfg | TODO_update_ssa_only_virtuals;
    }
  else if (cfgchanged)
    return TODO_cleanup_cfg;
  return 0;
}

/* Returns the list of basic blocks in the function in an order that guarantees
   that if a block X has just a single predecessor Y, then Y is after X in the
   ordering.  */

basic_block *
blocks_in_phiopt_order (void)
{
  basic_block x, y;
  basic_block *order = XNEWVEC (basic_block, n_basic_blocks);
  unsigned n = n_basic_blocks - NUM_FIXED_BLOCKS; 
  unsigned np, i;
  sbitmap visited = sbitmap_alloc (last_basic_block); 

#define MARK_VISITED(BB) (SET_BIT (visited, (BB)->index)) 
#define VISITED_P(BB) (TEST_BIT (visited, (BB)->index)) 

  sbitmap_zero (visited);

  MARK_VISITED (ENTRY_BLOCK_PTR);
  FOR_EACH_BB (x)
    {
      if (VISITED_P (x))
	continue;

      /* Walk the predecessors of x as long as they have precisely one
	 predecessor and add them to the list, so that they get stored
	 after x.  */
      for (y = x, np = 1;
	   single_pred_p (y) && !VISITED_P (single_pred (y));
	   y = single_pred (y))
	np++;
      for (y = x, i = n - np;
	   single_pred_p (y) && !VISITED_P (single_pred (y));
	   y = single_pred (y), i++)
	{
	  order[i] = y;
	  MARK_VISITED (y);
	}
      order[i] = y;
      MARK_VISITED (y);

      gcc_assert (i == n - 1);
      n -= np;
    }

  sbitmap_free (visited);
  gcc_assert (n == 0);
  return order;

#undef MARK_VISITED
#undef VISITED_P
}


/* Return TRUE if block BB has no executable statements, otherwise return
   FALSE.  */

bool
empty_block_p (basic_block bb)
{
  block_stmt_iterator bsi;

  /* BB must have no executable statements.  */
  bsi = bsi_start (bb);
  while (!bsi_end_p (bsi)
	  && (TREE_CODE (bsi_stmt (bsi)) == LABEL_EXPR
	      || IS_EMPTY_STMT (bsi_stmt (bsi))))
    bsi_next (&bsi);

  if (!bsi_end_p (bsi))
    return false;

  return true;
}

/* Replace PHI node element whose edge is E in block BB with variable NEW.
   Remove the edge from COND_BLOCK which does not lead to BB (COND_BLOCK
   is known to have two edges, one of which must reach BB).  */

static void
replace_phi_edge_with_variable (basic_block cond_block,
				edge e, tree phi, tree new_tree)
{
  basic_block bb = bb_for_stmt (phi);
  basic_block block_to_remove;
  block_stmt_iterator bsi;

  /* Change the PHI argument to new.  */
  SET_USE (PHI_ARG_DEF_PTR (phi, e->dest_idx), new_tree);

  /* Remove the empty basic block.  */
  if (EDGE_SUCC (cond_block, 0)->dest == bb)
    {
      EDGE_SUCC (cond_block, 0)->flags |= EDGE_FALLTHRU;
      EDGE_SUCC (cond_block, 0)->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);
      EDGE_SUCC (cond_block, 0)->probability = REG_BR_PROB_BASE;
      EDGE_SUCC (cond_block, 0)->count += EDGE_SUCC (cond_block, 1)->count;

      block_to_remove = EDGE_SUCC (cond_block, 1)->dest;
    }
  else
    {
      EDGE_SUCC (cond_block, 1)->flags |= EDGE_FALLTHRU;
      EDGE_SUCC (cond_block, 1)->flags
	&= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);
      EDGE_SUCC (cond_block, 1)->probability = REG_BR_PROB_BASE;
      EDGE_SUCC (cond_block, 1)->count += EDGE_SUCC (cond_block, 0)->count;

      block_to_remove = EDGE_SUCC (cond_block, 0)->dest;
    }
  delete_basic_block (block_to_remove);

  /* Eliminate the COND_EXPR at the end of COND_BLOCK.  */
  bsi = bsi_last (cond_block);
  bsi_remove (&bsi, true);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	      "COND_EXPR in block %d and PHI in block %d converted to straightline code.\n",
	      cond_block->index,
	      bb->index);
}

/*  The function conditional_replacement does the main work of doing the
    conditional replacement.  Return true if the replacement is done.
    Otherwise return false.
    BB is the basic block where the replacement is going to be done on.  ARG0
    is argument 0 from PHI.  Likewise for ARG1.  */

static bool
conditional_replacement (basic_block cond_bb, basic_block middle_bb,
			 edge e0, edge e1, tree phi,
			 tree arg0, tree arg1)
{
  tree result;
  tree old_result = NULL;
  tree new_stmt, cond;
  block_stmt_iterator bsi;
  edge true_edge, false_edge;
  tree new_var = NULL;
  tree new_var1;

  /* FIXME: Gimplification of complex type is too hard for now.  */
  if (TREE_CODE (TREE_TYPE (arg0)) == COMPLEX_TYPE
      || TREE_CODE (TREE_TYPE (arg1)) == COMPLEX_TYPE)
    return false;

  /* The PHI arguments have the constants 0 and 1, then convert
     it to the conditional.  */
  if ((integer_zerop (arg0) && integer_onep (arg1))
      || (integer_zerop (arg1) && integer_onep (arg0)))
    ;
  else
    return false;

  if (!empty_block_p (middle_bb))
    return false;

  /* If the condition is not a naked SSA_NAME and its type does not
     match the type of the result, then we have to create a new
     variable to optimize this case as it would likely create
     non-gimple code when the condition was converted to the
     result's type.  */
  cond = COND_EXPR_COND (last_stmt (cond_bb));
  result = PHI_RESULT (phi);
  if (TREE_CODE (cond) != SSA_NAME
      && !useless_type_conversion_p (TREE_TYPE (result), TREE_TYPE (cond)))
    {
      tree tmp;

      if (!COMPARISON_CLASS_P (cond))
	return false;

      tmp = create_tmp_var (TREE_TYPE (cond), NULL);
      add_referenced_var (tmp);
      new_var = make_ssa_name (tmp, NULL);
      old_result = cond;
      cond = new_var;
    }

  /* If the condition was a naked SSA_NAME and the type is not the
     same as the type of the result, then convert the type of the
     condition.  */
  if (!useless_type_conversion_p (TREE_TYPE (result), TREE_TYPE (cond)))
    cond = fold_convert (TREE_TYPE (result), cond);

  /* We need to know which is the true edge and which is the false
     edge so that we know when to invert the condition below.  */
  extract_true_false_edges_from_block (cond_bb, &true_edge, &false_edge);

  /* Insert our new statement at the end of conditional block before the
     COND_EXPR.  */
  bsi = bsi_last (cond_bb);
  bsi_insert_before (&bsi, build_empty_stmt (), BSI_NEW_STMT);

  if (old_result)
    {
      tree new1;

      new1 = build2 (TREE_CODE (old_result), TREE_TYPE (old_result),
		     TREE_OPERAND (old_result, 0),
		     TREE_OPERAND (old_result, 1));

      new1 = build_gimple_modify_stmt (new_var, new1);
      SSA_NAME_DEF_STMT (new_var) = new1;

      bsi_insert_after (&bsi, new1, BSI_NEW_STMT);
    }

  new_var1 = duplicate_ssa_name (PHI_RESULT (phi), NULL);


  /* At this point we know we have a COND_EXPR with two successors.
     One successor is BB, the other successor is an empty block which
     falls through into BB.

     There is a single PHI node at the join point (BB) and its arguments
     are constants (0, 1).

     So, given the condition COND, and the two PHI arguments, we can
     rewrite this PHI into non-branching code:

       dest = (COND) or dest = COND'

     We use the condition as-is if the argument associated with the
     true edge has the value one or the argument associated with the
     false edge as the value zero.  Note that those conditions are not
     the same since only one of the outgoing edges from the COND_EXPR
     will directly reach BB and thus be associated with an argument.  */
  if ((e0 == true_edge && integer_onep (arg0))
      || (e0 == false_edge && integer_zerop (arg0))
      || (e1 == true_edge && integer_onep (arg1))
      || (e1 == false_edge && integer_zerop (arg1)))
    {
      new_stmt = build_gimple_modify_stmt (new_var1, cond);
    }
  else
    {
      tree cond1 = invert_truthvalue (cond);

      cond = cond1;

      /* If what we get back is a conditional expression, there is no
	  way that it can be gimple.  */
      if (TREE_CODE (cond) == COND_EXPR)
	{
	  release_ssa_name (new_var1);
	  return false;
	}

      /* If COND is not something we can expect to be reducible to a GIMPLE
	 condition, return early.  */
      if (is_gimple_cast (cond))
	cond1 = TREE_OPERAND (cond, 0);
      if (TREE_CODE (cond1) == TRUTH_NOT_EXPR
	  && !is_gimple_val (TREE_OPERAND (cond1, 0)))
	{
	  release_ssa_name (new_var1);
	  return false;
	}

      /* If what we get back is not gimple try to create it as gimple by
	 using a temporary variable.  */
      if (is_gimple_cast (cond)
	  && !is_gimple_val (TREE_OPERAND (cond, 0)))
	{
	  tree op0, tmp, cond_tmp;

	  /* Only "real" casts are OK here, not everything that is
	     acceptable to is_gimple_cast.  Make sure we don't do
	     anything stupid here.  */
	  gcc_assert (TREE_CODE (cond) == NOP_EXPR
		      || TREE_CODE (cond) == CONVERT_EXPR);

	  op0 = TREE_OPERAND (cond, 0);
	  tmp = create_tmp_var (TREE_TYPE (op0), NULL);
	  add_referenced_var (tmp);
	  cond_tmp = make_ssa_name (tmp, NULL);
	  new_stmt = build_gimple_modify_stmt (cond_tmp, op0);
	  SSA_NAME_DEF_STMT (cond_tmp) = new_stmt;

	  bsi_insert_after (&bsi, new_stmt, BSI_NEW_STMT);
	  cond = fold_convert (TREE_TYPE (result), cond_tmp);
	}

      new_stmt = build_gimple_modify_stmt (new_var1, cond);
    }

  bsi_insert_after (&bsi, new_stmt, BSI_NEW_STMT);

  SSA_NAME_DEF_STMT (new_var1) = new_stmt;

  replace_phi_edge_with_variable (cond_bb, e1, phi, new_var1);

  /* Note that we optimized this PHI.  */
  return true;
}

/*  The function value_replacement does the main work of doing the value
    replacement.  Return true if the replacement is done.  Otherwise return
    false.
    BB is the basic block where the replacement is going to be done on.  ARG0
    is argument 0 from the PHI.  Likewise for ARG1.  */

static bool
value_replacement (basic_block cond_bb, basic_block middle_bb,
		   edge e0, edge e1, tree phi,
		   tree arg0, tree arg1)
{
  tree cond;
  edge true_edge, false_edge;

  /* If the type says honor signed zeros we cannot do this
     optimization.  */
  if (HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (arg1))))
    return false;

  if (!empty_block_p (middle_bb))
    return false;

  cond = COND_EXPR_COND (last_stmt (cond_bb));

  /* This transformation is only valid for equality comparisons.  */
  if (TREE_CODE (cond) != NE_EXPR && TREE_CODE (cond) != EQ_EXPR)
    return false;

  /* We need to know which is the true edge and which is the false
      edge so that we know if have abs or negative abs.  */
  extract_true_false_edges_from_block (cond_bb, &true_edge, &false_edge);

  /* At this point we know we have a COND_EXPR with two successors.
     One successor is BB, the other successor is an empty block which
     falls through into BB.

     The condition for the COND_EXPR is known to be NE_EXPR or EQ_EXPR.

     There is a single PHI node at the join point (BB) with two arguments.

     We now need to verify that the two arguments in the PHI node match
     the two arguments to the equality comparison.  */

  if ((operand_equal_for_phi_arg_p (arg0, TREE_OPERAND (cond, 0))
       && operand_equal_for_phi_arg_p (arg1, TREE_OPERAND (cond, 1)))
      || (operand_equal_for_phi_arg_p (arg1, TREE_OPERAND (cond, 0))
	  && operand_equal_for_phi_arg_p (arg0, TREE_OPERAND (cond, 1))))
    {
      edge e;
      tree arg;

      /* For NE_EXPR, we want to build an assignment result = arg where
	 arg is the PHI argument associated with the true edge.  For
	 EQ_EXPR we want the PHI argument associated with the false edge.  */
      e = (TREE_CODE (cond) == NE_EXPR ? true_edge : false_edge);

      /* Unfortunately, E may not reach BB (it may instead have gone to
	 OTHER_BLOCK).  If that is the case, then we want the single outgoing
	 edge from OTHER_BLOCK which reaches BB and represents the desired
	 path from COND_BLOCK.  */
      if (e->dest == middle_bb)
	e = single_succ_edge (e->dest);

      /* Now we know the incoming edge to BB that has the argument for the
	 RHS of our new assignment statement.  */
      if (e0 == e)
	arg = arg0;
      else
	arg = arg1;

      replace_phi_edge_with_variable (cond_bb, e1, phi, arg);

      /* Note that we optimized this PHI.  */
      return true;
    }
  return false;
}

/*  The function minmax_replacement does the main work of doing the minmax
    replacement.  Return true if the replacement is done.  Otherwise return
    false.
    BB is the basic block where the replacement is going to be done on.  ARG0
    is argument 0 from the PHI.  Likewise for ARG1.  */

static bool
minmax_replacement (basic_block cond_bb, basic_block middle_bb,
		    edge e0, edge e1, tree phi,
		    tree arg0, tree arg1)
{
  tree result, type;
  tree cond, new_stmt;
  edge true_edge, false_edge;
  enum tree_code cmp, minmax, ass_code;
  tree smaller, larger, arg_true, arg_false;
  block_stmt_iterator bsi, bsi_from;

  type = TREE_TYPE (PHI_RESULT (phi));

  /* The optimization may be unsafe due to NaNs.  */
  if (HONOR_NANS (TYPE_MODE (type)))
    return false;

  cond = COND_EXPR_COND (last_stmt (cond_bb));
  cmp = TREE_CODE (cond);
  result = PHI_RESULT (phi);

  /* This transformation is only valid for order comparisons.  Record which
     operand is smaller/larger if the result of the comparison is true.  */
  if (cmp == LT_EXPR || cmp == LE_EXPR)
    {
      smaller = TREE_OPERAND (cond, 0);
      larger = TREE_OPERAND (cond, 1);
    }
  else if (cmp == GT_EXPR || cmp == GE_EXPR)
    {
      smaller = TREE_OPERAND (cond, 1);
      larger = TREE_OPERAND (cond, 0);
    }
  else
    return false;

  /* We need to know which is the true edge and which is the false
      edge so that we know if have abs or negative abs.  */
  extract_true_false_edges_from_block (cond_bb, &true_edge, &false_edge);

  /* Forward the edges over the middle basic block.  */
  if (true_edge->dest == middle_bb)
    true_edge = EDGE_SUCC (true_edge->dest, 0);
  if (false_edge->dest == middle_bb)
    false_edge = EDGE_SUCC (false_edge->dest, 0);

  if (true_edge == e0)
    {
      gcc_assert (false_edge == e1);
      arg_true = arg0;
      arg_false = arg1;
    }
  else
    {
      gcc_assert (false_edge == e0);
      gcc_assert (true_edge == e1);
      arg_true = arg1;
      arg_false = arg0;
    }

  if (empty_block_p (middle_bb))
    {
      if (operand_equal_for_phi_arg_p (arg_true, smaller)
	  && operand_equal_for_phi_arg_p (arg_false, larger))
	{
	  /* Case
	 
	     if (smaller < larger)
	     rslt = smaller;
	     else
	     rslt = larger;  */
	  minmax = MIN_EXPR;
	}
      else if (operand_equal_for_phi_arg_p (arg_false, smaller)
	       && operand_equal_for_phi_arg_p (arg_true, larger))
	minmax = MAX_EXPR;
      else
	return false;
    }
  else
    {
      /* Recognize the following case, assuming d <= u:

	 if (a <= u)
	   b = MAX (a, d);
	 x = PHI <b, u>

	 This is equivalent to

	 b = MAX (a, d);
	 x = MIN (b, u);  */

      tree assign = last_and_only_stmt (middle_bb);
      tree lhs, rhs, op0, op1, bound;

      if (!assign
	  || TREE_CODE (assign) != GIMPLE_MODIFY_STMT)
	return false;

      lhs = GIMPLE_STMT_OPERAND (assign, 0);
      rhs = GIMPLE_STMT_OPERAND (assign, 1);
      ass_code = TREE_CODE (rhs);
      if (ass_code != MAX_EXPR && ass_code != MIN_EXPR)
	return false;
      op0 = TREE_OPERAND (rhs, 0);
      op1 = TREE_OPERAND (rhs, 1);

      if (true_edge->src == middle_bb)
	{
	  /* We got here if the condition is true, i.e., SMALLER < LARGER.  */
	  if (!operand_equal_for_phi_arg_p (lhs, arg_true))
	    return false;

	  if (operand_equal_for_phi_arg_p (arg_false, larger))
	    {
	      /* Case

		 if (smaller < larger)
		   {
		     r' = MAX_EXPR (smaller, bound)
		   }
		 r = PHI <r', larger>  --> to be turned to MIN_EXPR.  */
	      if (ass_code != MAX_EXPR)
		return false;

	      minmax = MIN_EXPR;
	      if (operand_equal_for_phi_arg_p (op0, smaller))
		bound = op1;
	      else if (operand_equal_for_phi_arg_p (op1, smaller))
		bound = op0;
	      else
		return false;

	      /* We need BOUND <= LARGER.  */
	      if (!integer_nonzerop (fold_build2 (LE_EXPR, boolean_type_node,
						  bound, larger)))
		return false;
	    }
	  else if (operand_equal_for_phi_arg_p (arg_false, smaller))
	    {
	      /* Case

		 if (smaller < larger)
		   {
		     r' = MIN_EXPR (larger, bound)
		   }
		 r = PHI <r', smaller>  --> to be turned to MAX_EXPR.  */
	      if (ass_code != MIN_EXPR)
		return false;

	      minmax = MAX_EXPR;
	      if (operand_equal_for_phi_arg_p (op0, larger))
		bound = op1;
	      else if (operand_equal_for_phi_arg_p (op1, larger))
		bound = op0;
	      else
		return false;

	      /* We need BOUND >= SMALLER.  */
	      if (!integer_nonzerop (fold_build2 (GE_EXPR, boolean_type_node,
						  bound, smaller)))
		return false;
	    }
	  else
	    return false;
	}
      else
	{
	  /* We got here if the condition is false, i.e., SMALLER > LARGER.  */
	  if (!operand_equal_for_phi_arg_p (lhs, arg_false))
	    return false;

	  if (operand_equal_for_phi_arg_p (arg_true, larger))
	    {
	      /* Case

		 if (smaller > larger)
		   {
		     r' = MIN_EXPR (smaller, bound)
		   }
		 r = PHI <r', larger>  --> to be turned to MAX_EXPR.  */
	      if (ass_code != MIN_EXPR)
		return false;

	      minmax = MAX_EXPR;
	      if (operand_equal_for_phi_arg_p (op0, smaller))
		bound = op1;
	      else if (operand_equal_for_phi_arg_p (op1, smaller))
		bound = op0;
	      else
		return false;

	      /* We need BOUND >= LARGER.  */
	      if (!integer_nonzerop (fold_build2 (GE_EXPR, boolean_type_node,
						  bound, larger)))
		return false;
	    }
	  else if (operand_equal_for_phi_arg_p (arg_true, smaller))
	    {
	      /* Case

		 if (smaller > larger)
		   {
		     r' = MAX_EXPR (larger, bound)
		   }
		 r = PHI <r', smaller>  --> to be turned to MIN_EXPR.  */
	      if (ass_code != MAX_EXPR)
		return false;

	      minmax = MIN_EXPR;
	      if (operand_equal_for_phi_arg_p (op0, larger))
		bound = op1;
	      else if (operand_equal_for_phi_arg_p (op1, larger))
		bound = op0;
	      else
		return false;

	      /* We need BOUND <= SMALLER.  */
	      if (!integer_nonzerop (fold_build2 (LE_EXPR, boolean_type_node,
						  bound, smaller)))
		return false;
	    }
	  else
	    return false;
	}

      /* Move the statement from the middle block.  */
      bsi = bsi_last (cond_bb);
      bsi_from = bsi_last (middle_bb);
      bsi_move_before (&bsi_from, &bsi);
    }

  /* Emit the statement to compute min/max.  */
  result = duplicate_ssa_name (PHI_RESULT (phi), NULL);
  new_stmt = build_gimple_modify_stmt (result, build2 (minmax, type, arg0, arg1));
  SSA_NAME_DEF_STMT (result) = new_stmt;
  bsi = bsi_last (cond_bb);
  bsi_insert_before (&bsi, new_stmt, BSI_NEW_STMT);

  replace_phi_edge_with_variable (cond_bb, e1, phi, result);
  return true;
}

/*  The function absolute_replacement does the main work of doing the absolute
    replacement.  Return true if the replacement is done.  Otherwise return
    false.
    bb is the basic block where the replacement is going to be done on.  arg0
    is argument 0 from the phi.  Likewise for arg1.  */

static bool
abs_replacement (basic_block cond_bb, basic_block middle_bb,
		 edge e0 ATTRIBUTE_UNUSED, edge e1,
		 tree phi, tree arg0, tree arg1)
{
  tree result;
  tree new_stmt, cond;
  block_stmt_iterator bsi;
  edge true_edge, false_edge;
  tree assign;
  edge e;
  tree rhs, lhs;
  bool negate;
  enum tree_code cond_code;

  /* If the type says honor signed zeros we cannot do this
     optimization.  */
  if (HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (arg1))))
    return false;

  /* OTHER_BLOCK must have only one executable statement which must have the
     form arg0 = -arg1 or arg1 = -arg0.  */

  assign = last_and_only_stmt (middle_bb);
  /* If we did not find the proper negation assignment, then we can not
     optimize.  */
  if (assign == NULL)
    return false;
      
  /* If we got here, then we have found the only executable statement
     in OTHER_BLOCK.  If it is anything other than arg = -arg1 or
     arg1 = -arg0, then we can not optimize.  */
  if (TREE_CODE (assign) != GIMPLE_MODIFY_STMT)
    return false;

  lhs = GIMPLE_STMT_OPERAND (assign, 0);
  rhs = GIMPLE_STMT_OPERAND (assign, 1);

  if (TREE_CODE (rhs) != NEGATE_EXPR)
    return false;

  rhs = TREE_OPERAND (rhs, 0);
              
  /* The assignment has to be arg0 = -arg1 or arg1 = -arg0.  */
  if (!(lhs == arg0 && rhs == arg1)
      && !(lhs == arg1 && rhs == arg0))
    return false;

  cond = COND_EXPR_COND (last_stmt (cond_bb));
  result = PHI_RESULT (phi);

  /* Only relationals comparing arg[01] against zero are interesting.  */
  cond_code = TREE_CODE (cond);
  if (cond_code != GT_EXPR && cond_code != GE_EXPR
      && cond_code != LT_EXPR && cond_code != LE_EXPR)
    return false;

  /* Make sure the conditional is arg[01] OP y.  */
  if (TREE_OPERAND (cond, 0) != rhs)
    return false;

  if (FLOAT_TYPE_P (TREE_TYPE (TREE_OPERAND (cond, 1)))
	       ? real_zerop (TREE_OPERAND (cond, 1))
	       : integer_zerop (TREE_OPERAND (cond, 1)))
    ;
  else
    return false;

  /* We need to know which is the true edge and which is the false
     edge so that we know if have abs or negative abs.  */
  extract_true_false_edges_from_block (cond_bb, &true_edge, &false_edge);

  /* For GT_EXPR/GE_EXPR, if the true edge goes to OTHER_BLOCK, then we
     will need to negate the result.  Similarly for LT_EXPR/LE_EXPR if
     the false edge goes to OTHER_BLOCK.  */
  if (cond_code == GT_EXPR || cond_code == GE_EXPR)
    e = true_edge;
  else
    e = false_edge;

  if (e->dest == middle_bb)
    negate = true;
  else
    negate = false;

  result = duplicate_ssa_name (result, NULL);

  if (negate)
    {
      tree tmp = create_tmp_var (TREE_TYPE (result), NULL);
      add_referenced_var (tmp);
      lhs = make_ssa_name (tmp, NULL);
    }
  else
    lhs = result;

  /* Build the modify expression with abs expression.  */
  new_stmt = build_gimple_modify_stmt (lhs,
				       build1 (ABS_EXPR, TREE_TYPE (lhs), rhs));
  SSA_NAME_DEF_STMT (lhs) = new_stmt;

  bsi = bsi_last (cond_bb);
  bsi_insert_before (&bsi, new_stmt, BSI_NEW_STMT);

  if (negate)
    {
      /* Get the right BSI.  We want to insert after the recently
	 added ABS_EXPR statement (which we know is the first statement
	 in the block.  */
      new_stmt = build_gimple_modify_stmt (result,
				           build1 (NEGATE_EXPR, TREE_TYPE (lhs),
					           lhs));
      SSA_NAME_DEF_STMT (result) = new_stmt;

      bsi_insert_after (&bsi, new_stmt, BSI_NEW_STMT);
    }

  replace_phi_edge_with_variable (cond_bb, e1, phi, result);

  /* Note that we optimized this PHI.  */
  return true;
}

/* Auxiliary functions to determine the set of memory accesses which
   can't trap because they are preceded by accesses to the same memory
   portion.  We do that for INDIRECT_REFs, so we only need to track
   the SSA_NAME of the pointer indirectly referenced.  The algorithm
   simply is a walk over all instructions in dominator order.  When
   we see an INDIRECT_REF we determine if we've already seen a same
   ref anywhere up to the root of the dominator tree.  If we do the
   current access can't trap.  If we don't see any dominating access
   the current access might trap, but might also make later accesses
   non-trapping, so we remember it.  We need to be careful with loads
   or stores, for instance a load might not trap, while a store would,
   so if we see a dominating read access this doesn't mean that a later
   write access would not trap.  Hence we also need to differentiate the
   type of access(es) seen.

   ??? We currently are very conservative and assume that a load might
   trap even if a store doesn't (write-only memory).  This probably is
   overly conservative.  */

/* A hash-table of SSA_NAMEs, and in which basic block an INDIRECT_REF
   through it was seen, which would constitute a no-trap region for
   same accesses.  */
struct name_to_bb
{
  tree ssa_name;
  basic_block bb;
  unsigned store : 1;
};

/* The hash table for remembering what we've seen.  */
static htab_t seen_ssa_names;

/* The set of INDIRECT_REFs which can't trap.  */
static struct pointer_set_t *nontrap_set;

/* The hash function, based on the pointer to the pointer SSA_NAME.  */
static hashval_t
name_to_bb_hash (const void *p)
{
  tree n = ((struct name_to_bb *)p)->ssa_name;
  return htab_hash_pointer (n) ^ ((struct name_to_bb *)p)->store;
}

/* The equality function of *P1 and *P2.  SSA_NAMEs are shared, so
   it's enough to simply compare them for equality.  */
static int
name_to_bb_eq (const void *p1, const void *p2)
{
  const struct name_to_bb *n1 = (const struct name_to_bb *)p1;
  const struct name_to_bb *n2 = (const struct name_to_bb *)p2;

  return n1->ssa_name == n2->ssa_name && n1->store == n2->store;
}

/* We see a the expression EXP in basic block BB.  If it's an interesting
   expression (an INDIRECT_REF through an SSA_NAME) possibly insert the
   expression into the set NONTRAP or the hash table of seen expressions.
   STORE is true if this expression is on the LHS, otherwise it's on
   the RHS.  */
static void
add_or_mark_expr (basic_block bb, tree exp,
		  struct pointer_set_t *nontrap, bool store)
{
  if (INDIRECT_REF_P (exp)
      && TREE_CODE (TREE_OPERAND (exp, 0)) == SSA_NAME)
    {
      tree name = TREE_OPERAND (exp, 0);
      struct name_to_bb map;
      void **slot;
      struct name_to_bb *n2bb;
      basic_block found_bb = 0;

      /* Try to find the last seen INDIRECT_REF through the same
         SSA_NAME, which can trap.  */
      map.ssa_name = name;
      map.bb = 0;
      map.store = store;
      slot = htab_find_slot (seen_ssa_names, &map, INSERT);
      n2bb = (struct name_to_bb *) *slot;
      if (n2bb)
        found_bb = n2bb->bb;

      /* If we've found a trapping INDIRECT_REF, _and_ it dominates EXP
         (it's in a basic block on the path from us to the dominator root)
	 then we can't trap.  */
      if (found_bb && found_bb->aux == (void *)1)
	{
	  pointer_set_insert (nontrap, exp);
	}
      else
        {
	  /* EXP might trap, so insert it into the hash table.  */
	  if (n2bb)
	    {
	      n2bb->bb = bb;
	    }
	  else
	    {
	      n2bb = XNEW (struct name_to_bb);
	      n2bb->ssa_name = name;
	      n2bb->bb = bb;
	      n2bb->store = store;
	      *slot = n2bb;
	    }
	}
    }
}

/* Called by walk_dominator_tree, when entering the block BB.  */
static void
nt_init_block (struct dom_walk_data *data ATTRIBUTE_UNUSED, basic_block bb)
{
  block_stmt_iterator bsi;
  /* Mark this BB as being on the path to dominator root.  */
  bb->aux = (void*)1;

  /* And walk the statements in order.  */
  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      tree stmt = bsi_stmt (bsi);

      if (TREE_CODE (stmt) == GIMPLE_MODIFY_STMT)
	{
	  tree lhs = GIMPLE_STMT_OPERAND (stmt, 0);
	  tree rhs = GIMPLE_STMT_OPERAND (stmt, 1);
	  add_or_mark_expr (bb, rhs, nontrap_set, false);
	  add_or_mark_expr (bb, lhs, nontrap_set, true);
	}
    }
}

/* Called by walk_dominator_tree, when basic block BB is exited.  */
static void
nt_fini_block (struct dom_walk_data *data ATTRIBUTE_UNUSED, basic_block bb)
{
  /* This BB isn't on the path to dominator root anymore.  */
  bb->aux = NULL;
}

/* This is the entry point of gathering non trapping memory accesses.
   It will do a dominator walk over the whole function, and it will
   make use of the bb->aux pointers.  It returns a set of trees
   (the INDIRECT_REFs itself) which can't trap.  */
static struct pointer_set_t *
get_non_trapping (void)
{
  struct pointer_set_t *nontrap;
  struct dom_walk_data walk_data;

  nontrap = pointer_set_create ();
  seen_ssa_names = htab_create (128, name_to_bb_hash, name_to_bb_eq,
				free);
  /* We're going to do a dominator walk, so ensure that we have
     dominance information.  */
  calculate_dominance_info (CDI_DOMINATORS);

  /* Setup callbacks for the generic dominator tree walker.  */
  nontrap_set = nontrap;
  walk_data.walk_stmts_backward = false;
  walk_data.dom_direction = CDI_DOMINATORS;
  walk_data.initialize_block_local_data = NULL;
  walk_data.before_dom_children_before_stmts = nt_init_block;
  walk_data.before_dom_children_walk_stmts = NULL;
  walk_data.before_dom_children_after_stmts = NULL;
  walk_data.after_dom_children_before_stmts = NULL;
  walk_data.after_dom_children_walk_stmts = NULL;
  walk_data.after_dom_children_after_stmts = nt_fini_block;
  walk_data.global_data = NULL;
  walk_data.block_local_data_size = 0;
  walk_data.interesting_blocks = NULL;

  init_walk_dominator_tree (&walk_data);
  walk_dominator_tree (&walk_data, ENTRY_BLOCK_PTR);
  fini_walk_dominator_tree (&walk_data);
  htab_delete (seen_ssa_names);

  return nontrap;
}

/* Do the main work of conditional store replacement.  We already know
   that the recognized pattern looks like so:

   split:
     if (cond) goto MIDDLE_BB; else goto JOIN_BB (edge E1)
   MIDDLE_BB:
     something
     fallthrough (edge E0)
   JOIN_BB:
     some more

   We check that MIDDLE_BB contains only one store, that that store
   doesn't trap (not via NOTRAP, but via checking if an access to the same
   memory location dominates us) and that the store has a "simple" RHS.  */

static bool
cond_store_replacement (basic_block middle_bb, basic_block join_bb,
			edge e0, edge e1, struct pointer_set_t *nontrap)
{
  tree assign = last_and_only_stmt (middle_bb);
  tree lhs, rhs, newexpr, name;
  tree newphi;
  block_stmt_iterator bsi;

  /* Check if middle_bb contains of only one store.  */
  if (!assign
      || TREE_CODE (assign) != GIMPLE_MODIFY_STMT)
    return false;

  lhs = GIMPLE_STMT_OPERAND (assign, 0);
  if (!INDIRECT_REF_P (lhs))
    return false;
  rhs = GIMPLE_STMT_OPERAND (assign, 1);
  if (TREE_CODE (rhs) != SSA_NAME && !is_gimple_min_invariant (rhs))
    return false;
  /* Prove that we can move the store down.  We could also check
     TREE_THIS_NOTRAP here, but in that case we also could move stores,
     whose value is not available readily, which we want to avoid.  */
  if (!pointer_set_contains (nontrap, lhs))
    return false;

  /* Now we've checked the constraints, so do the transformation:
     1) Remove the single store.  */
  mark_symbols_for_renaming (assign);
  bsi = bsi_for_stmt (assign);
  bsi_remove (&bsi, true);

  /* 2) Create a temporary where we can store the old content
        of the memory touched by the store, if we need to.  */
  if (!condstoretemp || TREE_TYPE (lhs) != TREE_TYPE (condstoretemp))
    {
      condstoretemp = create_tmp_var (TREE_TYPE (lhs), "cstore");
      get_var_ann (condstoretemp);
      if (TREE_CODE (TREE_TYPE (lhs)) == COMPLEX_TYPE
          || TREE_CODE (TREE_TYPE (lhs)) == VECTOR_TYPE)
	DECL_GIMPLE_REG_P (condstoretemp) = 1;
    }
  add_referenced_var (condstoretemp);

  /* 3) Insert a load from the memory of the store to the temporary
        on the edge which did not contain the store.  */
  lhs = unshare_expr (lhs);
  newexpr = build_gimple_modify_stmt (condstoretemp, lhs);
  name = make_ssa_name (condstoretemp, newexpr);
  GIMPLE_STMT_OPERAND (newexpr, 0) = name;
  mark_symbols_for_renaming (newexpr);
  bsi_insert_on_edge (e1, newexpr);

  /* 4) Create a PHI node at the join block, with one argument
        holding the old RHS, and the other holding the temporary
        where we stored the old memory contents.  */
  newphi = create_phi_node (condstoretemp, join_bb);
  add_phi_arg (newphi, rhs, e0);
  add_phi_arg (newphi, name, e1);

  lhs = unshare_expr (lhs);
  newexpr = build_gimple_modify_stmt (lhs, PHI_RESULT (newphi));
  mark_symbols_for_renaming (newexpr);

  /* 5) Insert that PHI node.  */
  bsi = bsi_start (join_bb);
  while (!bsi_end_p (bsi) && TREE_CODE (bsi_stmt (bsi)) == LABEL_EXPR)
    bsi_next (&bsi);
  if (bsi_end_p (bsi))
    {
      bsi = bsi_last (join_bb);
      bsi_insert_after (&bsi, newexpr, BSI_NEW_STMT);
    }
  else
    bsi_insert_before (&bsi, newexpr, BSI_NEW_STMT);

  return true;
}

/* Always do these optimizations if we have SSA
   trees to work on.  */
static bool
gate_phiopt (void)
{
  return 1;
}

struct tree_opt_pass pass_phiopt =
{
  "phiopt",				/* name */
  gate_phiopt,				/* gate */
  tree_ssa_phiopt,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_PHIOPT,			/* tv_id */
  PROP_cfg | PROP_ssa | PROP_alias,	/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func
    | TODO_ggc_collect
    | TODO_verify_ssa
    | TODO_verify_flow
    | TODO_verify_stmts,		/* todo_flags_finish */
  0					/* letter */
};

static bool
gate_cselim (void)
{
  return flag_tree_cselim;
}

struct tree_opt_pass pass_cselim =
{
  "cselim",				/* name */
  gate_cselim,				/* gate */
  tree_ssa_cs_elim,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_PHIOPT,			/* tv_id */
  PROP_cfg | PROP_ssa | PROP_alias,	/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func
    | TODO_ggc_collect
    | TODO_verify_ssa
    | TODO_verify_flow
    | TODO_verify_stmts,		/* todo_flags_finish */
  0					/* letter */
};
