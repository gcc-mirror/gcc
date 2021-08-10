/* Optimization of PHI nodes by converting them into straightline code.
   Copyright (C) 2004-2021 Free Software Foundation, Inc.

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
#include "backend.h"
#include "insn-codes.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "tree-ssa.h"
#include "optabs-tree.h"
#include "insn-config.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "cfganal.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "domwalk.h"
#include "cfgloop.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-inline.h"
#include "case-cfn-macros.h"
#include "tree-eh.h"
#include "gimple-fold.h"
#include "internal-fn.h"
#include "gimple-range.h"
#include "gimple-match.h"
#include "dbgcnt.h"

static unsigned int tree_ssa_phiopt_worker (bool, bool, bool);
static bool two_value_replacement (basic_block, basic_block, edge, gphi *,
				   tree, tree);
static bool match_simplify_replacement (basic_block, basic_block,
					edge, edge, gphi *, tree, tree, bool);
static gphi *factor_out_conditional_conversion (edge, edge, gphi *, tree, tree,
						gimple *);
static int value_replacement (basic_block, basic_block,
			      edge, edge, gphi *, tree, tree);
static bool minmax_replacement (basic_block, basic_block,
				edge, edge, gphi *, tree, tree);
static bool spaceship_replacement (basic_block, basic_block,
				   edge, edge, gphi *, tree, tree);
static bool cond_removal_in_builtin_zero_pattern (basic_block, basic_block,
						  edge, edge, gphi *,
						  tree, tree);
static bool cond_store_replacement (basic_block, basic_block, edge, edge,
				    hash_set<tree> *);
static bool cond_if_else_store_replacement (basic_block, basic_block, basic_block);
static hash_set<tree> * get_non_trapping ();
static void replace_phi_edge_with_variable (basic_block, edge, gphi *, tree);
static void hoist_adjacent_loads (basic_block, basic_block,
				  basic_block, basic_block);
static bool gate_hoist_loads (void);

/* This pass tries to transform conditional stores into unconditional
   ones, enabling further simplifications with the simpler then and else
   blocks.  In particular it replaces this:

     bb0:
       if (cond) goto bb2; else goto bb1;
     bb1:
       *p = RHS;
     bb2:

   with

     bb0:
       if (cond) goto bb1; else goto bb2;
     bb1:
       condtmp' = *p;
     bb2:
       condtmp = PHI <RHS, condtmp'>
       *p = condtmp;

   This transformation can only be done under several constraints,
   documented below.  It also replaces:

     bb0:
       if (cond) goto bb2; else goto bb1;
     bb1:
       *p = RHS1;
       goto bb3;
     bb2:
       *p = RHS2;
     bb3:

   with

     bb0:
       if (cond) goto bb3; else goto bb1;
     bb1:
     bb3:
       condtmp = PHI <RHS1, RHS2>
       *p = condtmp;  */

static unsigned int
tree_ssa_cs_elim (void)
{
  unsigned todo;
  /* ???  We are not interested in loop related info, but the following
     will create it, ICEing as we didn't init loops with pre-headers.
     An interfacing issue of find_data_references_in_bb.  */
  loop_optimizer_init (LOOPS_NORMAL);
  scev_initialize ();
  todo = tree_ssa_phiopt_worker (true, false, false);
  scev_finalize ();
  loop_optimizer_finalize ();
  return todo;
}

/* Return the singleton PHI in the SEQ of PHIs for edges E0 and E1. */

static gphi *
single_non_singleton_phi_for_edges (gimple_seq seq, edge e0, edge e1)
{
  gimple_stmt_iterator i;
  gphi *phi = NULL;
  if (gimple_seq_singleton_p (seq))
    return as_a <gphi *> (gsi_stmt (gsi_start (seq)));
  for (i = gsi_start (seq); !gsi_end_p (i); gsi_next (&i))
    {
      gphi *p = as_a <gphi *> (gsi_stmt (i));
      /* If the PHI arguments are equal then we can skip this PHI. */
      if (operand_equal_for_phi_arg_p (gimple_phi_arg_def (p, e0->dest_idx),
				       gimple_phi_arg_def (p, e1->dest_idx)))
	continue;

      /* If we already have a PHI that has the two edge arguments are
	 different, then return it is not a singleton for these PHIs. */
      if (phi)
	return NULL;

      phi = p;
    }
  return phi;
}

/* The core routine of conditional store replacement and normal
   phi optimizations.  Both share much of the infrastructure in how
   to match applicable basic block patterns.  DO_STORE_ELIM is true
   when we want to do conditional store replacement, false otherwise.
   DO_HOIST_LOADS is true when we want to hoist adjacent loads out
   of diamond control flow patterns, false otherwise.  */
static unsigned int
tree_ssa_phiopt_worker (bool do_store_elim, bool do_hoist_loads, bool early_p)
{
  basic_block bb;
  basic_block *bb_order;
  unsigned n, i;
  bool cfgchanged = false;
  hash_set<tree> *nontrap = 0;

  calculate_dominance_info (CDI_DOMINATORS);

  if (do_store_elim)
    /* Calculate the set of non-trapping memory accesses.  */
    nontrap = get_non_trapping ();

  /* Search every basic block for COND_EXPR we may be able to optimize.

     We walk the blocks in order that guarantees that a block with
     a single predecessor is processed before the predecessor.
     This ensures that we collapse inner ifs before visiting the
     outer ones, and also that we do not try to visit a removed
     block.  */
  bb_order = single_pred_before_succ_order ();
  n = n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS;

  for (i = 0; i < n; i++)
    {
      gimple *cond_stmt;
      gphi *phi;
      basic_block bb1, bb2;
      edge e1, e2;
      tree arg0, arg1;

      bb = bb_order[i];

      cond_stmt = last_stmt (bb);
      /* Check to see if the last statement is a GIMPLE_COND.  */
      if (!cond_stmt
          || gimple_code (cond_stmt) != GIMPLE_COND)
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
	  std::swap (bb1, bb2);
	  std::swap (e1, e2);
	}
      else if (do_store_elim
	       && EDGE_SUCC (bb1, 0)->dest == EDGE_SUCC (bb2, 0)->dest)
	{
	  basic_block bb3 = EDGE_SUCC (bb1, 0)->dest;

	  if (!single_succ_p (bb1)
	      || (EDGE_SUCC (bb1, 0)->flags & EDGE_FALLTHRU) == 0
	      || !single_succ_p (bb2)
	      || (EDGE_SUCC (bb2, 0)->flags & EDGE_FALLTHRU) == 0
	      || EDGE_COUNT (bb3->preds) != 2)
	    continue;
	  if (cond_if_else_store_replacement (bb1, bb2, bb3))
	    cfgchanged = true;
	  continue;
	}
      else if (do_hoist_loads
	       && EDGE_SUCC (bb1, 0)->dest == EDGE_SUCC (bb2, 0)->dest)
	{
	  basic_block bb3 = EDGE_SUCC (bb1, 0)->dest;

	  if (!FLOAT_TYPE_P (TREE_TYPE (gimple_cond_lhs (cond_stmt)))
	      && single_succ_p (bb1)
	      && single_succ_p (bb2)
	      && single_pred_p (bb1)
	      && single_pred_p (bb2)
	      && EDGE_COUNT (bb->succs) == 2
	      && EDGE_COUNT (bb3->preds) == 2
	      /* If one edge or the other is dominant, a conditional move
		 is likely to perform worse than the well-predicted branch.  */
	      && !predictable_edge_p (EDGE_SUCC (bb, 0))
	      && !predictable_edge_p (EDGE_SUCC (bb, 1)))
	    hoist_adjacent_loads (bb, bb1, bb2, bb3);
	  continue;
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
	  gimple_seq phis = phi_nodes (bb2);
	  gimple_stmt_iterator gsi;
	  bool candorest = true;

	  /* Value replacement can work with more than one PHI
	     so try that first. */
	  if (!early_p)
	    for (gsi = gsi_start (phis); !gsi_end_p (gsi); gsi_next (&gsi))
	      {
		phi = as_a <gphi *> (gsi_stmt (gsi));
		arg0 = gimple_phi_arg_def (phi, e1->dest_idx);
		arg1 = gimple_phi_arg_def (phi, e2->dest_idx);
		if (value_replacement (bb, bb1, e1, e2, phi, arg0, arg1) == 2)
		  {
		    candorest = false;
		    cfgchanged = true;
		    break;
		  }
	      }

	  if (!candorest)
	    continue;

	  phi = single_non_singleton_phi_for_edges (phis, e1, e2);
	  if (!phi)
	    continue;

	  arg0 = gimple_phi_arg_def (phi, e1->dest_idx);
	  arg1 = gimple_phi_arg_def (phi, e2->dest_idx);

	  /* Something is wrong if we cannot find the arguments in the PHI
	     node.  */
	  gcc_assert (arg0 != NULL_TREE && arg1 != NULL_TREE);

	  gphi *newphi = factor_out_conditional_conversion (e1, e2, phi,
							    arg0, arg1,
							    cond_stmt);
	  if (newphi != NULL)
	    {
	      phi = newphi;
	      /* factor_out_conditional_conversion may create a new PHI in
		 BB2 and eliminate an existing PHI in BB2.  Recompute values
		 that may be affected by that change.  */
	      arg0 = gimple_phi_arg_def (phi, e1->dest_idx);
	      arg1 = gimple_phi_arg_def (phi, e2->dest_idx);
	      gcc_assert (arg0 != NULL_TREE && arg1 != NULL_TREE);
	    }

	  /* Do the replacement of conditional if it can be done.  */
	  if (!early_p && two_value_replacement (bb, bb1, e2, phi, arg0, arg1))
	    cfgchanged = true;
	  else if (match_simplify_replacement (bb, bb1, e1, e2, phi,
					       arg0, arg1,
					       early_p))
	    cfgchanged = true;
	  else if (!early_p
		   && cond_removal_in_builtin_zero_pattern (bb, bb1, e1, e2,
							    phi, arg0, arg1))
	    cfgchanged = true;
	  else if (minmax_replacement (bb, bb1, e1, e2, phi, arg0, arg1))
	    cfgchanged = true;
	  else if (spaceship_replacement (bb, bb1, e1, e2, phi, arg0, arg1))
	    cfgchanged = true;
	}
    }

  free (bb_order);

  if (do_store_elim)
    delete nontrap;
  /* If the CFG has changed, we should cleanup the CFG.  */
  if (cfgchanged && do_store_elim)
    {
      /* In cond-store replacement we have added some loads on edges
         and new VOPS (as we moved the store, and created a load).  */
      gsi_commit_edge_inserts ();
      return TODO_cleanup_cfg | TODO_update_ssa_only_virtuals;
    }
  else if (cfgchanged)
    return TODO_cleanup_cfg;
  return 0;
}

/* Replace PHI node element whose edge is E in block BB with variable NEW.
   Remove the edge from COND_BLOCK which does not lead to BB (COND_BLOCK
   is known to have two edges, one of which must reach BB).  */

static void
replace_phi_edge_with_variable (basic_block cond_block,
				edge e, gphi *phi, tree new_tree)
{
  basic_block bb = gimple_bb (phi);
  basic_block block_to_remove;
  gimple_stmt_iterator gsi;
  tree phi_result = PHI_RESULT (phi);

  /* Duplicate range info if they are the only things setting the target PHI.
     This is needed as later on, the new_tree will be replacing
     The assignement of the PHI.
     For an example:
     bb1:
     _4 = min<a_1, 255>
     goto bb2

     # RANGE [-INF, 255]
     a_3 = PHI<_4(1)>
     bb3:

     use(a_3)
     And _4 gets propagated into the use of a_3 and losing the range info.
     This can't be done for more than 2 incoming edges as the propagation
     won't happen.
     The new_tree needs to be defined in the same basic block as the conditional.  */
  if (TREE_CODE (new_tree) == SSA_NAME
      && EDGE_COUNT (gimple_bb (phi)->preds) == 2
      && INTEGRAL_TYPE_P (TREE_TYPE (phi_result))
      && !SSA_NAME_RANGE_INFO (new_tree)
      && SSA_NAME_RANGE_INFO (phi_result)
      && gimple_bb (SSA_NAME_DEF_STMT (new_tree)) == cond_block
      && dbg_cnt (phiopt_edge_range))
    duplicate_ssa_name_range_info (new_tree,
				   SSA_NAME_RANGE_TYPE (phi_result),
				   SSA_NAME_RANGE_INFO (phi_result));

  /* Change the PHI argument to new.  */
  SET_USE (PHI_ARG_DEF_PTR (phi, e->dest_idx), new_tree);

  /* Remove the empty basic block.  */
  if (EDGE_SUCC (cond_block, 0)->dest == bb)
    {
      EDGE_SUCC (cond_block, 0)->flags |= EDGE_FALLTHRU;
      EDGE_SUCC (cond_block, 0)->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);
      EDGE_SUCC (cond_block, 0)->probability = profile_probability::always ();

      block_to_remove = EDGE_SUCC (cond_block, 1)->dest;
    }
  else
    {
      EDGE_SUCC (cond_block, 1)->flags |= EDGE_FALLTHRU;
      EDGE_SUCC (cond_block, 1)->flags
	&= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);
      EDGE_SUCC (cond_block, 1)->probability = profile_probability::always ();

      block_to_remove = EDGE_SUCC (cond_block, 0)->dest;
    }
  delete_basic_block (block_to_remove);

  /* Eliminate the COND_EXPR at the end of COND_BLOCK.  */
  gsi = gsi_last_bb (cond_block);
  gsi_remove (&gsi, true);

  statistics_counter_event (cfun, "Replace PHI with variable", 1);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	      "COND_EXPR in block %d and PHI in block %d converted to straightline code.\n",
	      cond_block->index,
	      bb->index);
}

/* PR66726: Factor conversion out of COND_EXPR.  If the arguments of the PHI
   stmt are CONVERT_STMT, factor out the conversion and perform the conversion
   to the result of PHI stmt.  COND_STMT is the controlling predicate.
   Return the newly-created PHI, if any.  */

static gphi *
factor_out_conditional_conversion (edge e0, edge e1, gphi *phi,
				   tree arg0, tree arg1, gimple *cond_stmt)
{
  gimple *arg0_def_stmt = NULL, *arg1_def_stmt = NULL, *new_stmt;
  tree new_arg0 = NULL_TREE, new_arg1 = NULL_TREE;
  tree temp, result;
  gphi *newphi;
  gimple_stmt_iterator gsi, gsi_for_def;
  location_t locus = gimple_location (phi);
  enum tree_code convert_code;

  /* Handle only PHI statements with two arguments.  TODO: If all
     other arguments to PHI are INTEGER_CST or if their defining
     statement have the same unary operation, we can handle more
     than two arguments too.  */
  if (gimple_phi_num_args (phi) != 2)
    return NULL;

  /* First canonicalize to simplify tests.  */
  if (TREE_CODE (arg0) != SSA_NAME)
    {
      std::swap (arg0, arg1);
      std::swap (e0, e1);
    }

  if (TREE_CODE (arg0) != SSA_NAME
      || (TREE_CODE (arg1) != SSA_NAME
	  && TREE_CODE (arg1) != INTEGER_CST))
    return NULL;

  /* Check if arg0 is an SSA_NAME and the stmt which defines arg0 is
     a conversion.  */
  arg0_def_stmt = SSA_NAME_DEF_STMT (arg0);
  if (!gimple_assign_cast_p (arg0_def_stmt))
    return NULL;

  /* Use the RHS as new_arg0.  */
  convert_code = gimple_assign_rhs_code (arg0_def_stmt);
  new_arg0 = gimple_assign_rhs1 (arg0_def_stmt);
  if (convert_code == VIEW_CONVERT_EXPR)
    {
      new_arg0 = TREE_OPERAND (new_arg0, 0);
      if (!is_gimple_reg_type (TREE_TYPE (new_arg0)))
	return NULL;
    }
  if (TREE_CODE (new_arg0) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (new_arg0))
    return NULL;

  if (TREE_CODE (arg1) == SSA_NAME)
    {
      /* Check if arg1 is an SSA_NAME and the stmt which defines arg1
	 is a conversion.  */
      arg1_def_stmt = SSA_NAME_DEF_STMT (arg1);
      if (!is_gimple_assign (arg1_def_stmt)
	  || gimple_assign_rhs_code (arg1_def_stmt) != convert_code)
	return NULL;

      /* Either arg1_def_stmt or arg0_def_stmt should be conditional.  */
      if (dominated_by_p (CDI_DOMINATORS, gimple_bb (phi), gimple_bb (arg0_def_stmt))
	  && dominated_by_p (CDI_DOMINATORS,
			     gimple_bb (phi), gimple_bb (arg1_def_stmt)))
	return NULL;

      /* Use the RHS as new_arg1.  */
      new_arg1 = gimple_assign_rhs1 (arg1_def_stmt);
      if (convert_code == VIEW_CONVERT_EXPR)
	new_arg1 = TREE_OPERAND (new_arg1, 0);
      if (TREE_CODE (new_arg1) == SSA_NAME
	  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (new_arg1))
	return NULL;
    }
  else
    {
      /* arg0_def_stmt should be conditional.  */
      if (dominated_by_p (CDI_DOMINATORS, gimple_bb (phi), gimple_bb (arg0_def_stmt)))
	return NULL;
      /* If arg1 is an INTEGER_CST, fold it to new type.  */
      if (INTEGRAL_TYPE_P (TREE_TYPE (new_arg0))
	  && int_fits_type_p (arg1, TREE_TYPE (new_arg0)))
	{
	  if (gimple_assign_cast_p (arg0_def_stmt))
	    {
	      /* For the INTEGER_CST case, we are just moving the
		 conversion from one place to another, which can often
		 hurt as the conversion moves further away from the
		 statement that computes the value.  So, perform this
		 only if new_arg0 is an operand of COND_STMT, or
		 if arg0_def_stmt is the only non-debug stmt in
		 its basic block, because then it is possible this
		 could enable further optimizations (minmax replacement
		 etc.).  See PR71016.  */
	      if (new_arg0 != gimple_cond_lhs (cond_stmt)
		  && new_arg0 != gimple_cond_rhs (cond_stmt)
		  && gimple_bb (arg0_def_stmt) == e0->src)
		{
		  gsi = gsi_for_stmt (arg0_def_stmt);
		  gsi_prev_nondebug (&gsi);
		  if (!gsi_end_p (gsi))
		    {
		      if (gassign *assign
			    = dyn_cast <gassign *> (gsi_stmt (gsi)))
			{
			  tree lhs = gimple_assign_lhs (assign);
			  enum tree_code ass_code
			    = gimple_assign_rhs_code (assign);
			  if (ass_code != MAX_EXPR && ass_code != MIN_EXPR)
			    return NULL;
			  if (lhs != gimple_assign_rhs1 (arg0_def_stmt))
			    return NULL;
			  gsi_prev_nondebug (&gsi);
			  if (!gsi_end_p (gsi))
			    return NULL;
			}
		      else
			return NULL;
		    }
		  gsi = gsi_for_stmt (arg0_def_stmt);
		  gsi_next_nondebug (&gsi);
		  if (!gsi_end_p (gsi))
		    return NULL;
		}
	      new_arg1 = fold_convert (TREE_TYPE (new_arg0), arg1);
	    }
	  else
	    return NULL;
	}
      else
	return NULL;
    }

  /*  If arg0/arg1 have > 1 use, then this transformation actually increases
      the number of expressions evaluated at runtime.  */
  if (!has_single_use (arg0)
      || (arg1_def_stmt && !has_single_use (arg1)))
    return NULL;

  /* If types of new_arg0 and new_arg1 are different bailout.  */
  if (!types_compatible_p (TREE_TYPE (new_arg0), TREE_TYPE (new_arg1)))
    return NULL;

  /* Create a new PHI stmt.  */
  result = PHI_RESULT (phi);
  temp = make_ssa_name (TREE_TYPE (new_arg0), NULL);
  newphi = create_phi_node (temp, gimple_bb (phi));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "PHI ");
      print_generic_expr (dump_file, gimple_phi_result (phi));
      fprintf (dump_file,
	       " changed to factor conversion out from COND_EXPR.\n");
      fprintf (dump_file, "New stmt with CAST that defines ");
      print_generic_expr (dump_file, result);
      fprintf (dump_file, ".\n");
    }

  /* Remove the old cast(s) that has single use.  */
  gsi_for_def = gsi_for_stmt (arg0_def_stmt);
  gsi_remove (&gsi_for_def, true);
  release_defs (arg0_def_stmt);

  if (arg1_def_stmt)
    {
      gsi_for_def = gsi_for_stmt (arg1_def_stmt);
      gsi_remove (&gsi_for_def, true);
      release_defs (arg1_def_stmt);
    }

  add_phi_arg (newphi, new_arg0, e0, locus);
  add_phi_arg (newphi, new_arg1, e1, locus);

  /* Create the conversion stmt and insert it.  */
  if (convert_code == VIEW_CONVERT_EXPR)
    {
      temp = fold_build1 (VIEW_CONVERT_EXPR, TREE_TYPE (result), temp);
      new_stmt = gimple_build_assign (result, temp);
    }
  else
    new_stmt = gimple_build_assign (result, convert_code, temp);
  gsi = gsi_after_labels (gimple_bb (phi));
  gsi_insert_before (&gsi, new_stmt, GSI_SAME_STMT);

  /* Remove the original PHI stmt.  */
  gsi = gsi_for_stmt (phi);
  gsi_remove (&gsi, true);

  statistics_counter_event (cfun, "factored out cast", 1);

  return newphi;
}

/* Optimize
   # x_5 in range [cst1, cst2] where cst2 = cst1 + 1
   if (x_5 op cstN) # where op is == or != and N is 1 or 2
     goto bb3;
   else
     goto bb4;
   bb3:
   bb4:
   # r_6 = PHI<cst3(2), cst4(3)> # where cst3 == cst4 + 1 or cst4 == cst3 + 1

   to r_6 = x_5 + (min (cst3, cst4) - cst1) or
   r_6 = (min (cst3, cst4) + cst1) - x_5 depending on op, N and which
   of cst3 and cst4 is smaller.  */

static bool
two_value_replacement (basic_block cond_bb, basic_block middle_bb,
		       edge e1, gphi *phi, tree arg0, tree arg1)
{
  /* Only look for adjacent integer constants.  */
  if (!INTEGRAL_TYPE_P (TREE_TYPE (arg0))
      || !INTEGRAL_TYPE_P (TREE_TYPE (arg1))
      || TREE_CODE (arg0) != INTEGER_CST
      || TREE_CODE (arg1) != INTEGER_CST
      || (tree_int_cst_lt (arg0, arg1)
	  ? wi::to_widest (arg0) + 1 != wi::to_widest (arg1)
	  : wi::to_widest (arg1) + 1 != wi::to_widest (arg0)))
    return false;

  if (!empty_block_p (middle_bb))
    return false;

  gimple *stmt = last_stmt (cond_bb);
  tree lhs = gimple_cond_lhs (stmt);
  tree rhs = gimple_cond_rhs (stmt);

  if (TREE_CODE (lhs) != SSA_NAME
      || !INTEGRAL_TYPE_P (TREE_TYPE (lhs))
      || TREE_CODE (rhs) != INTEGER_CST)
    return false;

  switch (gimple_cond_code (stmt))
    {
    case EQ_EXPR:
    case NE_EXPR:
      break;
    default:
      return false;
    }

  /* Defer boolean x ? 0 : {1,-1} or x ? {1,-1} : 0 to
     match_simplify_replacement.  */
  if (TREE_CODE (TREE_TYPE (lhs)) == BOOLEAN_TYPE
      && (integer_zerop (arg0)
	  || integer_zerop (arg1)
	  || TREE_CODE (TREE_TYPE (arg0)) == BOOLEAN_TYPE
	  || (TYPE_PRECISION (TREE_TYPE (arg0))
	      <= TYPE_PRECISION (TREE_TYPE (lhs)))))
    return false;

  wide_int min, max;
  value_range r;
  get_range_query (cfun)->range_of_expr (r, lhs);

  if (r.kind () == VR_RANGE)
    {
      min = r.lower_bound ();
      max = r.upper_bound ();
    }
  else
    {
      int prec = TYPE_PRECISION (TREE_TYPE (lhs));
      signop sgn = TYPE_SIGN (TREE_TYPE (lhs));
      min = wi::min_value (prec, sgn);
      max = wi::max_value (prec, sgn);
    }
  if (min + 1 != max
      || (wi::to_wide (rhs) != min
	  && wi::to_wide (rhs) != max))
    return false;

  /* We need to know which is the true edge and which is the false
     edge so that we know when to invert the condition below.  */
  edge true_edge, false_edge;
  extract_true_false_edges_from_block (cond_bb, &true_edge, &false_edge);
  if ((gimple_cond_code (stmt) == EQ_EXPR)
      ^ (wi::to_wide (rhs) == max)
      ^ (e1 == false_edge))
    std::swap (arg0, arg1);

  tree type;
  if (TYPE_PRECISION (TREE_TYPE (lhs)) == TYPE_PRECISION (TREE_TYPE (arg0)))
    {
      /* Avoid performing the arithmetics in bool type which has different
	 semantics, otherwise prefer unsigned types from the two with
	 the same precision.  */
      if (TREE_CODE (TREE_TYPE (arg0)) == BOOLEAN_TYPE
	  || !TYPE_UNSIGNED (TREE_TYPE (arg0)))
	type = TREE_TYPE (lhs);
      else
	type = TREE_TYPE (arg0);
    }
  else if (TYPE_PRECISION (TREE_TYPE (lhs)) > TYPE_PRECISION (TREE_TYPE (arg0)))
    type = TREE_TYPE (lhs);
  else
    type = TREE_TYPE (arg0);

  min = wide_int::from (min, TYPE_PRECISION (type),
			TYPE_SIGN (TREE_TYPE (lhs)));
  wide_int a = wide_int::from (wi::to_wide (arg0), TYPE_PRECISION (type),
			       TYPE_SIGN (TREE_TYPE (arg0)));
  enum tree_code code;
  wi::overflow_type ovf;
  if (tree_int_cst_lt (arg0, arg1))
    {
      code = PLUS_EXPR;
      a -= min;
      if (!TYPE_UNSIGNED (type))
	{
	  /* lhs is known to be in range [min, min+1] and we want to add a
	     to it.  Check if that operation can overflow for those 2 values
	     and if yes, force unsigned type.  */
	  wi::add (min + (wi::neg_p (a) ? 0 : 1), a, SIGNED, &ovf);
	  if (ovf)
	    type = unsigned_type_for (type);
	}
    }
  else
    {
      code = MINUS_EXPR;
      a += min;
      if (!TYPE_UNSIGNED (type))
	{
	  /* lhs is known to be in range [min, min+1] and we want to subtract
	     it from a.  Check if that operation can overflow for those 2
	     values and if yes, force unsigned type.  */
	  wi::sub (a, min + (wi::neg_p (min) ? 0 : 1), SIGNED, &ovf);
	  if (ovf)
	    type = unsigned_type_for (type);
	}
    }

  tree arg = wide_int_to_tree (type, a);
  gimple_seq stmts = NULL;
  lhs = gimple_convert (&stmts, type, lhs);
  tree new_rhs;
  if (code == PLUS_EXPR)
    new_rhs = gimple_build (&stmts, PLUS_EXPR, type, lhs, arg);
  else
    new_rhs = gimple_build (&stmts, MINUS_EXPR, type, arg, lhs);
  new_rhs = gimple_convert (&stmts, TREE_TYPE (arg0), new_rhs);
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);

  replace_phi_edge_with_variable (cond_bb, e1, phi, new_rhs);

  /* Note that we optimized this PHI.  */
  return true;
}

/* Return TRUE if SEQ/OP pair should be allowed during early phiopt.
   Currently this is to allow MIN/MAX and ABS/NEGATE and constants.  */
static bool
phiopt_early_allow (gimple_seq &seq, gimple_match_op &op)
{
  /* Don't allow functions. */
  if (!op.code.is_tree_code ())
    return false;
  tree_code code = (tree_code)op.code;

  /* For non-empty sequence, only allow one statement.  */
  if (!gimple_seq_empty_p (seq))
    {
      /* Check to make sure op was already a SSA_NAME.  */
      if (code != SSA_NAME)
	return false;
      if (!gimple_seq_singleton_p (seq))
	return false;
      gimple *stmt = gimple_seq_first_stmt (seq);
      /* Only allow assignments.  */
      if (!is_gimple_assign (stmt))
	return false;
      if (gimple_assign_lhs (stmt) != op.ops[0])
	return false;
      code = gimple_assign_rhs_code (stmt);
    }

  switch (code)
    {
      case MIN_EXPR:
      case MAX_EXPR:
      case ABS_EXPR:
      case ABSU_EXPR:
      case NEGATE_EXPR:
      case SSA_NAME:
	return true;
      case INTEGER_CST:
      case REAL_CST:
      case VECTOR_CST:
      case FIXED_CST:
	return true;
      default:
	return false;
    }
}

/* gimple_simplify_phiopt is like gimple_simplify but designed for PHIOPT.
   Return NULL if nothing can be simplified or the resulting simplified value
   with parts pushed if EARLY_P was true. Also rejects non allowed tree code
   if EARLY_P is set.
   Takes the comparison from COMP_STMT and two args, ARG0 and ARG1 and tries
   to simplify CMP ? ARG0 : ARG1.
   Also try to simplify (!CMP) ? ARG1 : ARG0 if the non-inverse failed.  */
static tree
gimple_simplify_phiopt (bool early_p, tree type, gimple *comp_stmt,
			tree arg0, tree arg1,
			gimple_seq *seq)
{
  tree result;
  gimple_seq seq1 = NULL;
  enum tree_code comp_code = gimple_cond_code (comp_stmt);
  location_t loc = gimple_location (comp_stmt);
  tree cmp0 = gimple_cond_lhs (comp_stmt);
  tree cmp1 = gimple_cond_rhs (comp_stmt);
  /* To handle special cases like floating point comparison, it is easier and
     less error-prone to build a tree and gimplify it on the fly though it is
     less efficient.
     Don't use fold_build2 here as that might create (bool)a instead of just
     "a != 0".  */
  tree cond = build2_loc (loc, comp_code, boolean_type_node,
			  cmp0, cmp1);
  gimple_match_op op (gimple_match_cond::UNCOND,
		      COND_EXPR, type, cond, arg0, arg1);

  if (op.resimplify (&seq1, follow_all_ssa_edges))
    {
      /* Early we want only to allow some generated tree codes. */
      if (!early_p
	  || phiopt_early_allow (seq1, op))
	{
	  result = maybe_push_res_to_seq (&op, &seq1);
	  if (result)
	    {
	      gimple_seq_add_seq_without_update (seq, seq1);
	      return result;
	    }
	}
    }
  gimple_seq_discard (seq1);
  seq1 = NULL;

  /* Try the inverted comparison, that is !COMP ? ARG1 : ARG0. */
  comp_code = invert_tree_comparison (comp_code, HONOR_NANS (cmp0));

  if (comp_code == ERROR_MARK)
    return NULL;

  cond = build2_loc (loc,
		     comp_code, boolean_type_node,
		     cmp0, cmp1);
  gimple_match_op op1 (gimple_match_cond::UNCOND,
		       COND_EXPR, type, cond, arg1, arg0);

  if (op1.resimplify (&seq1, follow_all_ssa_edges))
    {
      /* Early we want only to allow some generated tree codes. */
      if (!early_p
	  || phiopt_early_allow (seq1, op1))
	{
	  result = maybe_push_res_to_seq (&op1, &seq1);
	  if (result)
	    {
	      gimple_seq_add_seq_without_update (seq, seq1);
	      return result;
	    }
	}
    }
  gimple_seq_discard (seq1);

  return NULL;
}

/*  The function match_simplify_replacement does the main work of doing the
    replacement using match and simplify.  Return true if the replacement is done.
    Otherwise return false.
    BB is the basic block where the replacement is going to be done on.  ARG0
    is argument 0 from PHI.  Likewise for ARG1.  */

static bool
match_simplify_replacement (basic_block cond_bb, basic_block middle_bb,
			    edge e0, edge e1, gphi *phi,
			    tree arg0, tree arg1, bool early_p)
{
  gimple *stmt;
  gimple_stmt_iterator gsi;
  edge true_edge, false_edge;
  gimple_seq seq = NULL;
  tree result;
  gimple *stmt_to_move = NULL;

  /* Special case A ? B : B as this will always simplify to B. */
  if (operand_equal_for_phi_arg_p (arg0, arg1))
    return false;

  /* If the basic block only has a cheap preparation statement,
     allow it and move it once the transformation is done. */
  if (!empty_block_p (middle_bb))
    {
      stmt_to_move = last_and_only_stmt (middle_bb);
      if (!stmt_to_move)
	return false;

      if (gimple_vuse (stmt_to_move))
	return false;

      if (gimple_could_trap_p (stmt_to_move)
	  || gimple_has_side_effects (stmt_to_move))
	return false;

      if (gimple_uses_undefined_value_p (stmt_to_move))
	return false;

      /* Allow assignments and not no calls.
	 As const calls don't match any of the above, yet they could
	 still have some side-effects - they could contain
	 gimple_could_trap_p statements, like floating point
	 exceptions or integer division by zero.  See PR70586.
	 FIXME: perhaps gimple_has_side_effects or gimple_could_trap_p
	 should handle this.  */
      if (!is_gimple_assign (stmt_to_move))
	return false;

      tree lhs = gimple_assign_lhs (stmt_to_move);
      gimple *use_stmt;
      use_operand_p use_p;

      /* Allow only a statement which feeds into the phi.  */
      if (!lhs || TREE_CODE (lhs) != SSA_NAME
	  || !single_imm_use (lhs, &use_p, &use_stmt)
	  || use_stmt != phi)
	return false;
    }

    /* At this point we know we have a GIMPLE_COND with two successors.
     One successor is BB, the other successor is an empty block which
     falls through into BB.

     There is a single PHI node at the join point (BB).

     So, given the condition COND, and the two PHI arguments, match and simplify
     can happen on (COND) ? arg0 : arg1. */

  stmt = last_stmt (cond_bb);

  /* We need to know which is the true edge and which is the false
     edge so that we know when to invert the condition below.  */
  extract_true_false_edges_from_block (cond_bb, &true_edge, &false_edge);
  if (e1 == true_edge || e0 == false_edge)
    std::swap (arg0, arg1);

  tree type = TREE_TYPE (gimple_phi_result (phi));
  result = gimple_simplify_phiopt (early_p, type, stmt,
				   arg0, arg1,
				   &seq);
  if (!result)
    return false;

  gsi = gsi_last_bb (cond_bb);
  /* Insert the sequence generated from gimple_simplify_phiopt.  */
  if (seq)
    gsi_insert_seq_before (&gsi, seq, GSI_CONTINUE_LINKING);

  /* If there was a statement to move and the result of the statement
     is going to be used, move it to right before the original
     conditional.  */
  if (stmt_to_move
      && (gimple_assign_lhs (stmt_to_move) == result
	  || !has_single_use (gimple_assign_lhs (stmt_to_move))))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "statement un-sinked:\n");
	  print_gimple_stmt (dump_file, stmt_to_move, 0,
			   TDF_VOPS|TDF_MEMSYMS);
	}
      gimple_stmt_iterator gsi1 = gsi_for_stmt (stmt_to_move);
      gsi_move_before (&gsi1, &gsi);
      reset_flow_sensitive_info (gimple_assign_lhs (stmt_to_move));
    }

  replace_phi_edge_with_variable (cond_bb, e1, phi, result);

  /* Add Statistic here even though replace_phi_edge_with_variable already
     does it as we want to be able to count when match-simplify happens vs
     the others.  */
  statistics_counter_event (cfun, "match-simplify PHI replacement", 1);

  /* Note that we optimized this PHI.  */
  return true;
}

/* Update *ARG which is defined in STMT so that it contains the
   computed value if that seems profitable.  Return true if the
   statement is made dead by that rewriting.  */

static bool
jump_function_from_stmt (tree *arg, gimple *stmt)
{
  enum tree_code code = gimple_assign_rhs_code (stmt);
  if (code == ADDR_EXPR)
    {
      /* For arg = &p->i transform it to p, if possible.  */
      tree rhs1 = gimple_assign_rhs1 (stmt);
      poly_int64 offset;
      tree tem = get_addr_base_and_unit_offset (TREE_OPERAND (rhs1, 0),
						&offset);
      if (tem
	  && TREE_CODE (tem) == MEM_REF
	  && known_eq (mem_ref_offset (tem) + offset, 0))
	{
	  *arg = TREE_OPERAND (tem, 0);
	  return true;
	}
    }
  /* TODO: Much like IPA-CP jump-functions we want to handle constant
     additions symbolically here, and we'd need to update the comparison
     code that compares the arg + cst tuples in our caller.  For now the
     code above exactly handles the VEC_BASE pattern from vec.h.  */
  return false;
}

/* RHS is a source argument in a BIT_AND_EXPR which feeds a conditional
   of the form SSA_NAME NE 0.

   If RHS is fed by a simple EQ_EXPR comparison of two values, see if
   the two input values of the EQ_EXPR match arg0 and arg1.

   If so update *code and return TRUE.  Otherwise return FALSE.  */

static bool
rhs_is_fed_for_value_replacement (const_tree arg0, const_tree arg1,
				  enum tree_code *code, const_tree rhs)
{
  /* Obviously if RHS is not an SSA_NAME, we can't look at the defining
     statement.  */
  if (TREE_CODE (rhs) == SSA_NAME)
    {
      gimple *def1 = SSA_NAME_DEF_STMT (rhs);

      /* Verify the defining statement has an EQ_EXPR on the RHS.  */
      if (is_gimple_assign (def1) && gimple_assign_rhs_code (def1) == EQ_EXPR)
	{
	  /* Finally verify the source operands of the EQ_EXPR are equal
	     to arg0 and arg1.  */
	  tree op0 = gimple_assign_rhs1 (def1);
	  tree op1 = gimple_assign_rhs2 (def1);
	  if ((operand_equal_for_phi_arg_p (arg0, op0)
	       && operand_equal_for_phi_arg_p (arg1, op1))
	      || (operand_equal_for_phi_arg_p (arg0, op1)
               && operand_equal_for_phi_arg_p (arg1, op0)))
	    {
	      /* We will perform the optimization.  */
	      *code = gimple_assign_rhs_code (def1);
	      return true;
	    }
	}
    }
  return false;
}

/* Return TRUE if arg0/arg1 are equal to the rhs/lhs or lhs/rhs of COND. 

   Also return TRUE if arg0/arg1 are equal to the source arguments of a
   an EQ comparison feeding a BIT_AND_EXPR which feeds COND. 

   Return FALSE otherwise.  */

static bool
operand_equal_for_value_replacement (const_tree arg0, const_tree arg1,
				     enum tree_code *code, gimple *cond)
{
  gimple *def;
  tree lhs = gimple_cond_lhs (cond);
  tree rhs = gimple_cond_rhs (cond);

  if ((operand_equal_for_phi_arg_p (arg0, lhs)
       && operand_equal_for_phi_arg_p (arg1, rhs))
      || (operand_equal_for_phi_arg_p (arg1, lhs)
	  && operand_equal_for_phi_arg_p (arg0, rhs)))
    return true;

  /* Now handle more complex case where we have an EQ comparison
     which feeds a BIT_AND_EXPR which feeds COND.

     First verify that COND is of the form SSA_NAME NE 0.  */
  if (*code != NE_EXPR || !integer_zerop (rhs)
      || TREE_CODE (lhs) != SSA_NAME)
    return false;

  /* Now ensure that SSA_NAME is set by a BIT_AND_EXPR.  */
  def = SSA_NAME_DEF_STMT (lhs);
  if (!is_gimple_assign (def) || gimple_assign_rhs_code (def) != BIT_AND_EXPR)
    return false;

  /* Now verify arg0/arg1 correspond to the source arguments of an 
     EQ comparison feeding the BIT_AND_EXPR.  */
     
  tree tmp = gimple_assign_rhs1 (def);
  if (rhs_is_fed_for_value_replacement (arg0, arg1, code, tmp))
    return true;

  tmp = gimple_assign_rhs2 (def);
  if (rhs_is_fed_for_value_replacement (arg0, arg1, code, tmp))
    return true;

  return false;
}

/* Returns true if ARG is a neutral element for operation CODE
   on the RIGHT side.  */

static bool
neutral_element_p (tree_code code, tree arg, bool right)
{
  switch (code)
    {
    case PLUS_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      return integer_zerop (arg);

    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case MINUS_EXPR:
    case POINTER_PLUS_EXPR:
      return right && integer_zerop (arg);

    case MULT_EXPR:
      return integer_onep (arg);

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      return right && integer_onep (arg);

    case BIT_AND_EXPR:
      return integer_all_onesp (arg);

    default:
      return false;
    }
}

/* Returns true if ARG is an absorbing element for operation CODE.  */

static bool
absorbing_element_p (tree_code code, tree arg, bool right, tree rval)
{
  switch (code)
    {
    case BIT_IOR_EXPR:
      return integer_all_onesp (arg);

    case MULT_EXPR:
    case BIT_AND_EXPR:
      return integer_zerop (arg);

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      return !right && integer_zerop (arg);

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
      return (!right
	      && integer_zerop (arg)
	      && tree_single_nonzero_warnv_p (rval, NULL));

    default:
      return false;
    }
}

/*  The function value_replacement does the main work of doing the value
    replacement.  Return non-zero if the replacement is done.  Otherwise return
    0.  If we remove the middle basic block, return 2.
    BB is the basic block where the replacement is going to be done on.  ARG0
    is argument 0 from the PHI.  Likewise for ARG1.  */

static int
value_replacement (basic_block cond_bb, basic_block middle_bb,
		   edge e0, edge e1, gphi *phi, tree arg0, tree arg1)
{
  gimple_stmt_iterator gsi;
  gimple *cond;
  edge true_edge, false_edge;
  enum tree_code code;
  bool empty_or_with_defined_p = true;

  /* If the type says honor signed zeros we cannot do this
     optimization.  */
  if (HONOR_SIGNED_ZEROS (arg1))
    return 0;

  /* If there is a statement in MIDDLE_BB that defines one of the PHI
     arguments, then adjust arg0 or arg1.  */
  gsi = gsi_start_nondebug_after_labels_bb (middle_bb);
  while (!gsi_end_p (gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      tree lhs;
      gsi_next_nondebug (&gsi);
      if (!is_gimple_assign (stmt))
	{
	  if (gimple_code (stmt) != GIMPLE_PREDICT
	      && gimple_code (stmt) != GIMPLE_NOP)
	    empty_or_with_defined_p = false;
	  continue;
	}
      /* Now try to adjust arg0 or arg1 according to the computation
	 in the statement.  */
      lhs = gimple_assign_lhs (stmt);
      if (!(lhs == arg0
	     && jump_function_from_stmt (&arg0, stmt))
	    || (lhs == arg1
		&& jump_function_from_stmt (&arg1, stmt)))
	empty_or_with_defined_p = false;
    }

  cond = last_stmt (cond_bb);
  code = gimple_cond_code (cond);

  /* This transformation is only valid for equality comparisons.  */
  if (code != NE_EXPR && code != EQ_EXPR)
    return 0;

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

  if (operand_equal_for_value_replacement (arg0, arg1, &code, cond))
    {
      edge e;
      tree arg;

      /* For NE_EXPR, we want to build an assignment result = arg where
	 arg is the PHI argument associated with the true edge.  For
	 EQ_EXPR we want the PHI argument associated with the false edge.  */
      e = (code == NE_EXPR ? true_edge : false_edge);

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

      /* If the middle basic block was empty or is defining the
	 PHI arguments and this is a single phi where the args are different
	 for the edges e0 and e1 then we can remove the middle basic block. */
      if (empty_or_with_defined_p
	  && single_non_singleton_phi_for_edges (phi_nodes (gimple_bb (phi)),
						 e0, e1) == phi)
	{
          replace_phi_edge_with_variable (cond_bb, e1, phi, arg);
	  /* Note that we optimized this PHI.  */
	  return 2;
	}
      else
	{
	  statistics_counter_event (cfun, "Replace PHI with variable/value_replacement", 1);

	  /* Replace the PHI arguments with arg. */
	  SET_PHI_ARG_DEF (phi, e0->dest_idx, arg);
	  SET_PHI_ARG_DEF (phi, e1->dest_idx, arg);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "PHI ");
	      print_generic_expr (dump_file, gimple_phi_result (phi));
	      fprintf (dump_file, " reduced for COND_EXPR in block %d to ",
		       cond_bb->index);
	      print_generic_expr (dump_file, arg);
	      fprintf (dump_file, ".\n");
            }
          return 1;
	}

    }

  /* Now optimize (x != 0) ? x + y : y to just x + y.  */
  gsi = gsi_last_nondebug_bb (middle_bb);
  if (gsi_end_p (gsi))
    return 0;

  gimple *assign = gsi_stmt (gsi);
  if (!is_gimple_assign (assign)
      || gimple_assign_rhs_class (assign) != GIMPLE_BINARY_RHS
      || (!INTEGRAL_TYPE_P (TREE_TYPE (arg0))
	  && !POINTER_TYPE_P (TREE_TYPE (arg0))))
    return 0;

  /* Punt if there are (degenerate) PHIs in middle_bb, there should not be.  */
  if (!gimple_seq_empty_p (phi_nodes (middle_bb)))
    return 0;

  /* Allow up to 2 cheap preparation statements that prepare argument
     for assign, e.g.:
      if (y_4 != 0)
	goto <bb 3>;
      else
	goto <bb 4>;
     <bb 3>:
      _1 = (int) y_4;
      iftmp.0_6 = x_5(D) r<< _1;
     <bb 4>:
      # iftmp.0_2 = PHI <iftmp.0_6(3), x_5(D)(2)>
     or:
      if (y_3(D) == 0)
	goto <bb 4>;
      else
	goto <bb 3>;
     <bb 3>:
      y_4 = y_3(D) & 31;
      _1 = (int) y_4;
      _6 = x_5(D) r<< _1;
     <bb 4>:
      # _2 = PHI <x_5(D)(2), _6(3)>  */
  gimple *prep_stmt[2] = { NULL, NULL };
  int prep_cnt;
  for (prep_cnt = 0; ; prep_cnt++)
    {
      gsi_prev_nondebug (&gsi);
      if (gsi_end_p (gsi))
	break;

      gimple *g = gsi_stmt (gsi);
      if (gimple_code (g) == GIMPLE_LABEL)
	break;

      if (prep_cnt == 2 || !is_gimple_assign (g))
	return 0;

      tree lhs = gimple_assign_lhs (g);
      tree rhs1 = gimple_assign_rhs1 (g);
      use_operand_p use_p;
      gimple *use_stmt;
      if (TREE_CODE (lhs) != SSA_NAME
	  || TREE_CODE (rhs1) != SSA_NAME
	  || !INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	  || !INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
	  || !single_imm_use (lhs, &use_p, &use_stmt)
	  || use_stmt != (prep_cnt ? prep_stmt[prep_cnt - 1] : assign))
	return 0;
      switch (gimple_assign_rhs_code (g))
	{
	CASE_CONVERT:
	  break;
	case PLUS_EXPR:
	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	case BIT_XOR_EXPR:
	  if (TREE_CODE (gimple_assign_rhs2 (g)) != INTEGER_CST)
	    return 0;
	  break;
	default:
	  return 0;
	}
      prep_stmt[prep_cnt] = g;
    }

  /* Only transform if it removes the condition.  */
  if (!single_non_singleton_phi_for_edges (phi_nodes (gimple_bb (phi)), e0, e1))
    return 0;

  /* Size-wise, this is always profitable.  */
  if (optimize_bb_for_speed_p (cond_bb)
      /* The special case is useless if it has a low probability.  */
      && profile_status_for_fn (cfun) != PROFILE_ABSENT
      && EDGE_PRED (middle_bb, 0)->probability < profile_probability::even ()
      /* If assign is cheap, there is no point avoiding it.  */
      && estimate_num_insns_seq (bb_seq (middle_bb), &eni_time_weights)
	 >= 3 * estimate_num_insns (cond, &eni_time_weights))
    return 0;

  tree lhs = gimple_assign_lhs (assign);
  tree rhs1 = gimple_assign_rhs1 (assign);
  tree rhs2 = gimple_assign_rhs2 (assign);
  enum tree_code code_def = gimple_assign_rhs_code (assign);
  tree cond_lhs = gimple_cond_lhs (cond);
  tree cond_rhs = gimple_cond_rhs (cond);

  /* Propagate the cond_rhs constant through preparation stmts,
     make sure UB isn't invoked while doing that.  */
  for (int i = prep_cnt - 1; i >= 0; --i)
    {
      gimple *g = prep_stmt[i];
      tree grhs1 = gimple_assign_rhs1 (g);
      if (!operand_equal_for_phi_arg_p (cond_lhs, grhs1))
	return 0;
      cond_lhs = gimple_assign_lhs (g);
      cond_rhs = fold_convert (TREE_TYPE (grhs1), cond_rhs);
      if (TREE_CODE (cond_rhs) != INTEGER_CST
	  || TREE_OVERFLOW (cond_rhs))
	return 0;
      if (gimple_assign_rhs_class (g) == GIMPLE_BINARY_RHS)
	{
	  cond_rhs = int_const_binop (gimple_assign_rhs_code (g), cond_rhs,
				      gimple_assign_rhs2 (g));
	  if (TREE_OVERFLOW (cond_rhs))
	    return 0;
	}
      cond_rhs = fold_convert (TREE_TYPE (cond_lhs), cond_rhs);
      if (TREE_CODE (cond_rhs) != INTEGER_CST
	  || TREE_OVERFLOW (cond_rhs))
	return 0;
    }

  if (((code == NE_EXPR && e1 == false_edge)
	|| (code == EQ_EXPR && e1 == true_edge))
      && arg0 == lhs
      && ((arg1 == rhs1
	   && operand_equal_for_phi_arg_p (rhs2, cond_lhs)
	   && neutral_element_p (code_def, cond_rhs, true))
	  || (arg1 == rhs2
	      && operand_equal_for_phi_arg_p (rhs1, cond_lhs)
	      && neutral_element_p (code_def, cond_rhs, false))
	  || (operand_equal_for_phi_arg_p (arg1, cond_rhs)
	      && ((operand_equal_for_phi_arg_p (rhs2, cond_lhs)
		   && absorbing_element_p (code_def, cond_rhs, true, rhs2))
		  || (operand_equal_for_phi_arg_p (rhs1, cond_lhs)
		      && absorbing_element_p (code_def,
					      cond_rhs, false, rhs2))))))
    {
      gsi = gsi_for_stmt (cond);
      /* Moving ASSIGN might change VR of lhs, e.g. when moving u_6
	 def-stmt in:
	   if (n_5 != 0)
	     goto <bb 3>;
	   else
	     goto <bb 4>;

	   <bb 3>:
	   # RANGE [0, 4294967294]
	   u_6 = n_5 + 4294967295;

	   <bb 4>:
	   # u_3 = PHI <u_6(3), 4294967295(2)>  */
      reset_flow_sensitive_info (lhs);
      gimple_stmt_iterator gsi_from;
      for (int i = prep_cnt - 1; i >= 0; --i)
	{
	  tree plhs = gimple_assign_lhs (prep_stmt[i]);
	  reset_flow_sensitive_info (plhs);
	  gsi_from = gsi_for_stmt (prep_stmt[i]);
	  gsi_move_before (&gsi_from, &gsi);
	}
      gsi_from = gsi_for_stmt (assign);
      gsi_move_before (&gsi_from, &gsi);
      replace_phi_edge_with_variable (cond_bb, e1, phi, lhs);
      return 2;
    }

  return 0;
}

/*  The function minmax_replacement does the main work of doing the minmax
    replacement.  Return true if the replacement is done.  Otherwise return
    false.
    BB is the basic block where the replacement is going to be done on.  ARG0
    is argument 0 from the PHI.  Likewise for ARG1.  */

static bool
minmax_replacement (basic_block cond_bb, basic_block middle_bb,
		    edge e0, edge e1, gphi *phi, tree arg0, tree arg1)
{
  tree result;
  edge true_edge, false_edge;
  enum tree_code minmax, ass_code;
  tree smaller, larger, arg_true, arg_false;
  gimple_stmt_iterator gsi, gsi_from;

  tree type = TREE_TYPE (PHI_RESULT (phi));

  /* The optimization may be unsafe due to NaNs.  */
  if (HONOR_NANS (type) || HONOR_SIGNED_ZEROS (type))
    return false;

  gcond *cond = as_a <gcond *> (last_stmt (cond_bb));
  enum tree_code cmp = gimple_cond_code (cond);
  tree rhs = gimple_cond_rhs (cond);

  /* Turn EQ/NE of extreme values to order comparisons.  */
  if ((cmp == NE_EXPR || cmp == EQ_EXPR)
      && TREE_CODE (rhs) == INTEGER_CST
      && INTEGRAL_TYPE_P (TREE_TYPE (rhs)))
    {
      if (wi::eq_p (wi::to_wide (rhs), wi::min_value (TREE_TYPE (rhs))))
	{
	  cmp = (cmp == EQ_EXPR) ? LT_EXPR : GE_EXPR;
	  rhs = wide_int_to_tree (TREE_TYPE (rhs),
				  wi::min_value (TREE_TYPE (rhs)) + 1);
	}
      else if (wi::eq_p (wi::to_wide (rhs), wi::max_value (TREE_TYPE (rhs))))
	{
	  cmp = (cmp == EQ_EXPR) ? GT_EXPR : LE_EXPR;
	  rhs = wide_int_to_tree (TREE_TYPE (rhs),
				  wi::max_value (TREE_TYPE (rhs)) - 1);
	}
    }

  /* This transformation is only valid for order comparisons.  Record which
     operand is smaller/larger if the result of the comparison is true.  */
  tree alt_smaller = NULL_TREE;
  tree alt_larger = NULL_TREE;
  if (cmp == LT_EXPR || cmp == LE_EXPR)
    {
      smaller = gimple_cond_lhs (cond);
      larger = rhs;
      /* If we have smaller < CST it is equivalent to smaller <= CST-1.
	 Likewise smaller <= CST is equivalent to smaller < CST+1.  */
      if (TREE_CODE (larger) == INTEGER_CST
	  && INTEGRAL_TYPE_P (TREE_TYPE (larger)))
	{
	  if (cmp == LT_EXPR)
	    {
	      wi::overflow_type overflow;
	      wide_int alt = wi::sub (wi::to_wide (larger), 1,
				      TYPE_SIGN (TREE_TYPE (larger)),
				      &overflow);
	      if (! overflow)
		alt_larger = wide_int_to_tree (TREE_TYPE (larger), alt);
	    }
	  else
	    {
	      wi::overflow_type overflow;
	      wide_int alt = wi::add (wi::to_wide (larger), 1,
				      TYPE_SIGN (TREE_TYPE (larger)),
				      &overflow);
	      if (! overflow)
		alt_larger = wide_int_to_tree (TREE_TYPE (larger), alt);
	    }
	}
    }
  else if (cmp == GT_EXPR || cmp == GE_EXPR)
    {
      smaller = rhs;
      larger = gimple_cond_lhs (cond);
      /* If we have larger > CST it is equivalent to larger >= CST+1.
	 Likewise larger >= CST is equivalent to larger > CST-1.  */
      if (TREE_CODE (smaller) == INTEGER_CST
	  && INTEGRAL_TYPE_P (TREE_TYPE (smaller)))
	{
	  wi::overflow_type overflow;
	  if (cmp == GT_EXPR)
	    {
	      wide_int alt = wi::add (wi::to_wide (smaller), 1,
				      TYPE_SIGN (TREE_TYPE (smaller)),
				      &overflow);
	      if (! overflow)
		alt_smaller = wide_int_to_tree (TREE_TYPE (smaller), alt);
	    }
	  else
	    {
	      wide_int alt = wi::sub (wi::to_wide (smaller), 1,
				      TYPE_SIGN (TREE_TYPE (smaller)),
				      &overflow);
	      if (! overflow)
		alt_smaller = wide_int_to_tree (TREE_TYPE (smaller), alt);
	    }
	}
    }
  else
    return false;

  /* Handle the special case of (signed_type)x < 0 being equivalent
     to x > MAX_VAL(signed_type) and (signed_type)x >= 0 equivalent
     to x <= MAX_VAL(signed_type).  */
  if ((cmp == GE_EXPR || cmp == LT_EXPR)
      && INTEGRAL_TYPE_P (type)
      && TYPE_UNSIGNED (type)
      && integer_zerop (rhs))
    {
      tree op = gimple_cond_lhs (cond);
      if (TREE_CODE (op) == SSA_NAME
	  && INTEGRAL_TYPE_P (TREE_TYPE (op))
	  && !TYPE_UNSIGNED (TREE_TYPE (op)))
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (op);
	  if (gimple_assign_cast_p (def_stmt))
	    {
	      tree op1 = gimple_assign_rhs1 (def_stmt);
	      if (INTEGRAL_TYPE_P (TREE_TYPE (op1))
		  && TYPE_UNSIGNED (TREE_TYPE (op1))
		  && (TYPE_PRECISION (TREE_TYPE (op))
		      == TYPE_PRECISION (TREE_TYPE (op1)))
		  && useless_type_conversion_p (type, TREE_TYPE (op1)))
		{
		  wide_int w1 = wi::max_value (TREE_TYPE (op));
		  wide_int w2 = wi::add (w1, 1);
		  if (cmp == LT_EXPR)
		    {
		      larger = op1;
		      smaller = wide_int_to_tree (TREE_TYPE (op1), w1);
		      alt_smaller = wide_int_to_tree (TREE_TYPE (op1), w2);
		      alt_larger = NULL_TREE;
		    }
		  else
		    {
		      smaller = op1;
		      larger = wide_int_to_tree (TREE_TYPE (op1), w1);
		      alt_larger = wide_int_to_tree (TREE_TYPE (op1), w2);
		      alt_smaller = NULL_TREE;
		    }
		}
	    }
	}
    }

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
      if ((operand_equal_for_phi_arg_p (arg_true, smaller)
	   || (alt_smaller
	       && operand_equal_for_phi_arg_p (arg_true, alt_smaller)))
	  && (operand_equal_for_phi_arg_p (arg_false, larger)
	      || (alt_larger
		  && operand_equal_for_phi_arg_p (arg_true, alt_larger))))
	{
	  /* Case

	     if (smaller < larger)
	     rslt = smaller;
	     else
	     rslt = larger;  */
	  minmax = MIN_EXPR;
	}
      else if ((operand_equal_for_phi_arg_p (arg_false, smaller)
		|| (alt_smaller
		    && operand_equal_for_phi_arg_p (arg_false, alt_smaller)))
	       && (operand_equal_for_phi_arg_p (arg_true, larger)
		   || (alt_larger
		       && operand_equal_for_phi_arg_p (arg_true, alt_larger))))
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

      gimple *assign = last_and_only_stmt (middle_bb);
      tree lhs, op0, op1, bound;

      if (!assign
	  || gimple_code (assign) != GIMPLE_ASSIGN)
	return false;

      lhs = gimple_assign_lhs (assign);
      ass_code = gimple_assign_rhs_code (assign);
      if (ass_code != MAX_EXPR && ass_code != MIN_EXPR)
	return false;
      op0 = gimple_assign_rhs1 (assign);
      op1 = gimple_assign_rhs2 (assign);

      if (true_edge->src == middle_bb)
	{
	  /* We got here if the condition is true, i.e., SMALLER < LARGER.  */
	  if (!operand_equal_for_phi_arg_p (lhs, arg_true))
	    return false;

	  if (operand_equal_for_phi_arg_p (arg_false, larger)
	      || (alt_larger
		  && operand_equal_for_phi_arg_p (arg_false, alt_larger)))
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
	      if (operand_equal_for_phi_arg_p (op0, smaller)
		  || (alt_smaller
		      && operand_equal_for_phi_arg_p (op0, alt_smaller)))
		bound = op1;
	      else if (operand_equal_for_phi_arg_p (op1, smaller)
		       || (alt_smaller
			   && operand_equal_for_phi_arg_p (op1, alt_smaller)))
		bound = op0;
	      else
		return false;

	      /* We need BOUND <= LARGER.  */
	      if (!integer_nonzerop (fold_build2 (LE_EXPR, boolean_type_node,
						  bound, larger)))
		return false;
	    }
	  else if (operand_equal_for_phi_arg_p (arg_false, smaller)
		   || (alt_smaller
		       && operand_equal_for_phi_arg_p (arg_false, alt_smaller)))
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
	      if (operand_equal_for_phi_arg_p (op0, larger)
		  || (alt_larger
		      && operand_equal_for_phi_arg_p (op0, alt_larger)))
		bound = op1;
	      else if (operand_equal_for_phi_arg_p (op1, larger)
		       || (alt_larger
			   && operand_equal_for_phi_arg_p (op1, alt_larger)))
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

	  if (operand_equal_for_phi_arg_p (arg_true, larger)
	      || (alt_larger
		  && operand_equal_for_phi_arg_p (arg_true, alt_larger)))
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
	      if (operand_equal_for_phi_arg_p (op0, smaller)
		  || (alt_smaller
		      && operand_equal_for_phi_arg_p (op0, alt_smaller)))
		bound = op1;
	      else if (operand_equal_for_phi_arg_p (op1, smaller)
		       || (alt_smaller
			   && operand_equal_for_phi_arg_p (op1, alt_smaller)))
		bound = op0;
	      else
		return false;

	      /* We need BOUND >= LARGER.  */
	      if (!integer_nonzerop (fold_build2 (GE_EXPR, boolean_type_node,
						  bound, larger)))
		return false;
	    }
	  else if (operand_equal_for_phi_arg_p (arg_true, smaller)
		   || (alt_smaller
		       && operand_equal_for_phi_arg_p (arg_true, alt_smaller)))
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
      gsi = gsi_last_bb (cond_bb);
      gsi_from = gsi_last_nondebug_bb (middle_bb);
      reset_flow_sensitive_info (SINGLE_SSA_TREE_OPERAND (gsi_stmt (gsi_from),
							  SSA_OP_DEF));
      gsi_move_before (&gsi_from, &gsi);
    }

  /* Emit the statement to compute min/max.  */
  gimple_seq stmts = NULL;
  tree phi_result = PHI_RESULT (phi);
  result = gimple_build (&stmts, minmax, TREE_TYPE (phi_result), arg0, arg1);

  gsi = gsi_last_bb (cond_bb);
  gsi_insert_seq_before (&gsi, stmts, GSI_NEW_STMT);

  replace_phi_edge_with_variable (cond_bb, e1, phi, result);

  return true;
}

/* Return true if the only executable statement in BB is a GIMPLE_COND.  */

static bool
cond_only_block_p (basic_block bb)
{
  /* BB must have no executable statements.  */
  gimple_stmt_iterator gsi = gsi_after_labels (bb);
  if (phi_nodes (bb))
    return false;
  while (!gsi_end_p (gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (is_gimple_debug (stmt))
	;
      else if (gimple_code (stmt) == GIMPLE_NOP
	       || gimple_code (stmt) == GIMPLE_PREDICT
	       || gimple_code (stmt) == GIMPLE_COND)
	;
      else
	return false;
      gsi_next (&gsi);
    }
  return true;
}

/* Attempt to optimize (x <=> y) cmp 0 and similar comparisons.
   For strong ordering <=> try to match something like:
    <bb 2> :  // cond3_bb (== cond2_bb)
    if (x_4(D) != y_5(D))
      goto <bb 3>; [INV]
    else
      goto <bb 6>; [INV]

    <bb 3> :  // cond_bb
    if (x_4(D) < y_5(D))
      goto <bb 6>; [INV]
    else
      goto <bb 4>; [INV]

    <bb 4> :  // middle_bb

    <bb 6> :  // phi_bb
    # iftmp.0_2 = PHI <1(4), 0(2), -1(3)>
    _1 = iftmp.0_2 == 0;

   and for partial ordering <=> something like:

    <bb 2> :  // cond3_bb
    if (a_3(D) == b_5(D))
      goto <bb 6>; [50.00%]
    else
      goto <bb 3>; [50.00%]

    <bb 3> [local count: 536870913]:  // cond2_bb
    if (a_3(D) < b_5(D))
      goto <bb 6>; [50.00%]
    else
      goto <bb 4>; [50.00%]

    <bb 4> [local count: 268435456]:  // cond_bb
    if (a_3(D) > b_5(D))
      goto <bb 6>; [50.00%]
    else
      goto <bb 5>; [50.00%]

    <bb 5> [local count: 134217728]:  // middle_bb

    <bb 6> [local count: 1073741824]:  // phi_bb
    # SR.27_4 = PHI <0(2), -1(3), 1(4), 2(5)>
    _2 = SR.27_4 > 0;  */

static bool
spaceship_replacement (basic_block cond_bb, basic_block middle_bb,
		       edge e0, edge e1, gphi *phi,
		       tree arg0, tree arg1)
{
  tree phires = PHI_RESULT (phi);
  if (!INTEGRAL_TYPE_P (TREE_TYPE (phires))
      || TYPE_UNSIGNED (TREE_TYPE (phires))
      || !tree_fits_shwi_p (arg0)
      || !tree_fits_shwi_p (arg1)
      || !IN_RANGE (tree_to_shwi (arg0), -1, 2)
      || !IN_RANGE (tree_to_shwi (arg1), -1, 2))
    return false;

  basic_block phi_bb = gimple_bb (phi);
  gcc_assert (phi_bb == e0->dest && phi_bb == e1->dest);
  if (!IN_RANGE (EDGE_COUNT (phi_bb->preds), 3, 4))
    return false;

  use_operand_p use_p;
  gimple *use_stmt;
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (phires))
    return false;
  if (!single_imm_use (phires, &use_p, &use_stmt))
    return false;
  enum tree_code cmp;
  tree lhs, rhs;
  gimple *orig_use_stmt = use_stmt;
  tree orig_use_lhs = NULL_TREE;
  int prec = TYPE_PRECISION (TREE_TYPE (phires));
  if (is_gimple_assign (use_stmt)
      && gimple_assign_rhs_code (use_stmt) == BIT_AND_EXPR
      && TREE_CODE (gimple_assign_rhs2 (use_stmt)) == INTEGER_CST
      && (wi::to_wide (gimple_assign_rhs2 (use_stmt))
	  == wi::shifted_mask (1, prec - 1, false, prec)))
    {
      /* For partial_ordering result operator>= with unspec as second
	 argument is (res & 1) == res, folded by match.pd into
	 (res & ~1) == 0.  */
      orig_use_lhs = gimple_assign_lhs (use_stmt);
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (orig_use_lhs))
	return false;
      if (EDGE_COUNT (phi_bb->preds) != 4)
	return false;
      if (!single_imm_use (orig_use_lhs, &use_p, &use_stmt))
	return false;
    }
  if (gimple_code (use_stmt) == GIMPLE_COND)
    {
      cmp = gimple_cond_code (use_stmt);
      lhs = gimple_cond_lhs (use_stmt);
      rhs = gimple_cond_rhs (use_stmt);
    }
  else if (is_gimple_assign (use_stmt))
    {
      if (gimple_assign_rhs_class (use_stmt) == GIMPLE_BINARY_RHS)
	{
	  cmp = gimple_assign_rhs_code (use_stmt);
	  lhs = gimple_assign_rhs1 (use_stmt);
	  rhs = gimple_assign_rhs2 (use_stmt);
	}
      else if (gimple_assign_rhs_code (use_stmt) == COND_EXPR)
	{
	  tree cond = gimple_assign_rhs1 (use_stmt);
	  if (!COMPARISON_CLASS_P (cond))
	    return false;
	  cmp = TREE_CODE (cond);
	  lhs = TREE_OPERAND (cond, 0);
	  rhs = TREE_OPERAND (cond, 1);
	}
      else
	return false;
    }
  else
    return false;
  switch (cmp)
    {
    case EQ_EXPR:
    case NE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
      break;
    default:
      return false;
    }
  if (lhs != (orig_use_lhs ? orig_use_lhs : phires)
      || !tree_fits_shwi_p (rhs)
      || !IN_RANGE (tree_to_shwi (rhs), -1, 1))
    return false;
  if (orig_use_lhs)
    {
      if ((cmp != EQ_EXPR && cmp != NE_EXPR) || !integer_zerop (rhs))
	return false;
      /* As for -ffast-math we assume the 2 return to be
	 impossible, canonicalize (res & ~1) == 0 into
	 res >= 0 and (res & ~1) != 0 as res < 0.  */
      cmp = cmp == EQ_EXPR ? GE_EXPR : LT_EXPR;
    }

  if (!empty_block_p (middle_bb))
    return false;

  gcond *cond1 = as_a <gcond *> (last_stmt (cond_bb));
  enum tree_code cmp1 = gimple_cond_code (cond1);
  switch (cmp1)
    {
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
      break;
    default:
      return false;
    }
  tree lhs1 = gimple_cond_lhs (cond1);
  tree rhs1 = gimple_cond_rhs (cond1);
  /* The optimization may be unsafe due to NaNs.  */
  if (HONOR_NANS (TREE_TYPE (lhs1)))
    return false;
  if (TREE_CODE (lhs1) == SSA_NAME && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs1))
    return false;
  if (TREE_CODE (rhs1) == SSA_NAME && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs1))
    return false;

  if (!single_pred_p (cond_bb) || !cond_only_block_p (cond_bb))
    return false;

  basic_block cond2_bb = single_pred (cond_bb);
  if (EDGE_COUNT (cond2_bb->succs) != 2)
    return false;
  edge cond2_phi_edge;
  if (EDGE_SUCC (cond2_bb, 0)->dest == cond_bb)
    {
      if (EDGE_SUCC (cond2_bb, 1)->dest != phi_bb)
	return false;
      cond2_phi_edge = EDGE_SUCC (cond2_bb, 1);
    }
  else if (EDGE_SUCC (cond2_bb, 0)->dest != phi_bb)
    return false;
  else
    cond2_phi_edge = EDGE_SUCC (cond2_bb, 0);
  tree arg2 = gimple_phi_arg_def (phi, cond2_phi_edge->dest_idx);
  if (!tree_fits_shwi_p (arg2))
    return false;
  gimple *cond2 = last_stmt (cond2_bb);
  if (cond2 == NULL || gimple_code (cond2) != GIMPLE_COND)
    return false;
  enum tree_code cmp2 = gimple_cond_code (cond2);
  tree lhs2 = gimple_cond_lhs (cond2);
  tree rhs2 = gimple_cond_rhs (cond2);
  if (lhs2 == lhs1)
    {
      if (!operand_equal_p (rhs2, rhs1, 0))
	{
	  if ((cmp2 == EQ_EXPR || cmp2 == NE_EXPR)
	      && TREE_CODE (rhs1) == INTEGER_CST
	      && TREE_CODE (rhs2) == INTEGER_CST)
	    {
	      /* For integers, we can have cond2 x == 5
		 and cond1 x < 5, x <= 4, x <= 5, x < 6,
		 x > 5, x >= 6, x >= 5 or x > 4.  */
	      if (tree_int_cst_lt (rhs1, rhs2))
		{
		  if (wi::ne_p (wi::to_wide (rhs1) + 1, wi::to_wide (rhs2)))
		    return false;
		  if (cmp1 == LE_EXPR)
		    cmp1 = LT_EXPR;
		  else if (cmp1 == GT_EXPR)
		    cmp1 = GE_EXPR;
		  else
		    return false;
		}
	      else
		{
		  gcc_checking_assert (tree_int_cst_lt (rhs2, rhs1));
		  if (wi::ne_p (wi::to_wide (rhs2) + 1, wi::to_wide (rhs1)))
		    return false;
		  if (cmp1 == LT_EXPR)
		    cmp1 = LE_EXPR;
		  else if (cmp1 == GE_EXPR)
		    cmp1 = GT_EXPR;
		  else
		    return false;
		}
	      rhs1 = rhs2;
	    }
	  else
	    return false;
	}
    }
  else if (lhs2 == rhs1)
    {
      if (rhs2 != lhs1)
	return false;
    }
  else
    return false;

  tree arg3 = arg2;
  basic_block cond3_bb = cond2_bb;
  edge cond3_phi_edge = cond2_phi_edge;
  gimple *cond3 = cond2;
  enum tree_code cmp3 = cmp2;
  tree lhs3 = lhs2;
  tree rhs3 = rhs2;
  if (EDGE_COUNT (phi_bb->preds) == 4)
    {
      if (absu_hwi (tree_to_shwi (arg2)) != 1)
	return false;
      if (e1->flags & EDGE_TRUE_VALUE)
	{
	  if (tree_to_shwi (arg0) != 2
	      || absu_hwi (tree_to_shwi (arg1)) != 1
	      || wi::to_widest (arg1) == wi::to_widest (arg2))
	    return false;
	}
      else if (tree_to_shwi (arg1) != 2
	       || absu_hwi (tree_to_shwi (arg0)) != 1
	       || wi::to_widest (arg0) == wi::to_widest (arg1))
	return false;
      switch (cmp2)
	{
	case LT_EXPR:
	case LE_EXPR:
	case GT_EXPR:
	case GE_EXPR:
	  break;
	default:
	  return false;
	}
      /* if (x < y) goto phi_bb; else fallthru;
	 if (x > y) goto phi_bb; else fallthru;
	 bbx:;
	 phi_bb:;
	 is ok, but if x and y are swapped in one of the comparisons,
	 or the comparisons are the same and operands not swapped,
	 or the true and false edges are swapped, it is not.  */
      if ((lhs2 == lhs1)
	  ^ (((cond2_phi_edge->flags
	       & ((cmp2 == LT_EXPR || cmp2 == LE_EXPR)
		  ? EDGE_TRUE_VALUE : EDGE_FALSE_VALUE)) != 0)
	     != ((e1->flags
		  & ((cmp1 == LT_EXPR || cmp1 == LE_EXPR)
		     ? EDGE_TRUE_VALUE : EDGE_FALSE_VALUE)) != 0)))
	return false;
      if (!single_pred_p (cond2_bb) || !cond_only_block_p (cond2_bb))
	return false;
      cond3_bb = single_pred (cond2_bb);
      if (EDGE_COUNT (cond2_bb->succs) != 2)
	return false;
      if (EDGE_SUCC (cond3_bb, 0)->dest == cond2_bb)
	{
	  if (EDGE_SUCC (cond3_bb, 1)->dest != phi_bb)
	    return false;
	  cond3_phi_edge = EDGE_SUCC (cond3_bb, 1);
	}
      else if (EDGE_SUCC (cond3_bb, 0)->dest != phi_bb)
	return false;
      else
	cond3_phi_edge = EDGE_SUCC (cond3_bb, 0);
      arg3 = gimple_phi_arg_def (phi, cond3_phi_edge->dest_idx);
      cond3 = last_stmt (cond3_bb);
      if (cond3 == NULL || gimple_code (cond3) != GIMPLE_COND)
	return false;
      cmp3 = gimple_cond_code (cond3);
      lhs3 = gimple_cond_lhs (cond3);
      rhs3 = gimple_cond_rhs (cond3);
      if (lhs3 == lhs1)
	{
	  if (!operand_equal_p (rhs3, rhs1, 0))
	    return false;
	}
      else if (lhs3 == rhs1)
	{
	  if (rhs3 != lhs1)
	    return false;
	}
      else
	return false;
    }
  else if (absu_hwi (tree_to_shwi (arg0)) != 1
	   || absu_hwi (tree_to_shwi (arg1)) != 1
	   || wi::to_widest (arg0) == wi::to_widest (arg1))
    return false;

  if (!integer_zerop (arg3) || (cmp3 != EQ_EXPR && cmp3 != NE_EXPR))
    return false;
  if ((cond3_phi_edge->flags & (cmp3 == EQ_EXPR
				? EDGE_TRUE_VALUE : EDGE_FALSE_VALUE)) == 0)
    return false;

  /* lhs1 one_cmp rhs1 results in phires of 1.  */
  enum tree_code one_cmp;
  if ((cmp1 == LT_EXPR || cmp1 == LE_EXPR)
      ^ (!integer_onep ((e1->flags & EDGE_TRUE_VALUE) ? arg1 : arg0)))
    one_cmp = LT_EXPR;
  else
    one_cmp = GT_EXPR;

  enum tree_code res_cmp;
  switch (cmp)
    {
    case EQ_EXPR:
      if (integer_zerop (rhs))
	res_cmp = EQ_EXPR;
      else if (integer_minus_onep (rhs))
	res_cmp = one_cmp == LT_EXPR ? GT_EXPR : LT_EXPR;
      else if (integer_onep (rhs))
	res_cmp = one_cmp;
      else
	return false;
      break;
    case NE_EXPR:
      if (integer_zerop (rhs))
	res_cmp = NE_EXPR;
      else if (integer_minus_onep (rhs))
	res_cmp = one_cmp == LT_EXPR ? LE_EXPR : GE_EXPR;
      else if (integer_onep (rhs))
	res_cmp = one_cmp == LT_EXPR ? GE_EXPR : LE_EXPR;
      else
	return false;
      break;
    case LT_EXPR:
      if (integer_onep (rhs))
	res_cmp = one_cmp == LT_EXPR ? GE_EXPR : LE_EXPR;
      else if (integer_zerop (rhs))
	res_cmp = one_cmp == LT_EXPR ? GT_EXPR : LT_EXPR;
      else
	return false;
      break;
    case LE_EXPR:
      if (integer_zerop (rhs))
	res_cmp = one_cmp == LT_EXPR ? GE_EXPR : LE_EXPR;
      else if (integer_minus_onep (rhs))
	res_cmp = one_cmp == LT_EXPR ? GT_EXPR : LT_EXPR;
      else
	return false;
      break;
    case GT_EXPR:
      if (integer_minus_onep (rhs))
	res_cmp = one_cmp == LT_EXPR ? LE_EXPR : GE_EXPR;
      else if (integer_zerop (rhs))
	res_cmp = one_cmp;
      else
	return false;
      break;
    case GE_EXPR:
      if (integer_zerop (rhs))
	res_cmp = one_cmp == LT_EXPR ? LE_EXPR : GE_EXPR;
      else if (integer_onep (rhs))
	res_cmp = one_cmp;
      else
	return false;
      break;
    default:
      gcc_unreachable ();
    }

  if (gimple_code (use_stmt) == GIMPLE_COND)
    {
      gcond *use_cond = as_a <gcond *> (use_stmt);
      gimple_cond_set_code (use_cond, res_cmp);
      gimple_cond_set_lhs (use_cond, lhs1);
      gimple_cond_set_rhs (use_cond, rhs1);
    }
  else if (gimple_assign_rhs_class (use_stmt) == GIMPLE_BINARY_RHS)
    {
      gimple_assign_set_rhs_code (use_stmt, res_cmp);
      gimple_assign_set_rhs1 (use_stmt, lhs1);
      gimple_assign_set_rhs2 (use_stmt, rhs1);
    }
  else
    {
      tree cond = build2 (res_cmp, TREE_TYPE (gimple_assign_rhs1 (use_stmt)),
			  lhs1, rhs1);
      gimple_assign_set_rhs1 (use_stmt, cond);
    }
  update_stmt (use_stmt);

  if (MAY_HAVE_DEBUG_BIND_STMTS)
    {
      use_operand_p use_p;
      imm_use_iterator iter;
      bool has_debug_uses = false;
      FOR_EACH_IMM_USE_FAST (use_p, iter, phires)
	{
	  gimple *use_stmt = USE_STMT (use_p);
	  if (orig_use_lhs && use_stmt == orig_use_stmt)
	    continue;
	  gcc_assert (is_gimple_debug (use_stmt));
	  has_debug_uses = true;
	  break;
	}
      if (orig_use_lhs)
	{
	  if (!has_debug_uses)
	    FOR_EACH_IMM_USE_FAST (use_p, iter, orig_use_lhs)
	      {
		gimple *use_stmt = USE_STMT (use_p);
		gcc_assert (is_gimple_debug (use_stmt));
		has_debug_uses = true;
	      }
	  gimple_stmt_iterator gsi = gsi_for_stmt (orig_use_stmt);
	  tree zero = build_zero_cst (TREE_TYPE (orig_use_lhs));
	  gimple_assign_set_rhs_with_ops (&gsi, INTEGER_CST, zero);
	  update_stmt (orig_use_stmt);
	}

      if (has_debug_uses)
	{
	  /* If there are debug uses, emit something like:
	     # DEBUG D#1 => i_2(D) > j_3(D) ? 1 : -1
	     # DEBUG D#2 => i_2(D) == j_3(D) ? 0 : D#1
	     where > stands for the comparison that yielded 1
	     and replace debug uses of phi result with that D#2.
	     Ignore the value of 2, because if NaNs aren't expected,
	     all floating point numbers should be comparable.  */
	  gimple_stmt_iterator gsi = gsi_after_labels (gimple_bb (phi));
	  tree type = TREE_TYPE (phires);
	  tree temp1 = make_node (DEBUG_EXPR_DECL);
	  DECL_ARTIFICIAL (temp1) = 1;
	  TREE_TYPE (temp1) = type;
	  SET_DECL_MODE (temp1, TYPE_MODE (type));
	  tree t = build2 (one_cmp, boolean_type_node, lhs1, rhs2);
	  t = build3 (COND_EXPR, type, t, build_one_cst (type),
		      build_int_cst (type, -1));
	  gimple *g = gimple_build_debug_bind (temp1, t, phi);
	  gsi_insert_before (&gsi, g, GSI_SAME_STMT);
	  tree temp2 = make_node (DEBUG_EXPR_DECL);
	  DECL_ARTIFICIAL (temp2) = 1;
	  TREE_TYPE (temp2) = type;
	  SET_DECL_MODE (temp2, TYPE_MODE (type));
	  t = build2 (EQ_EXPR, boolean_type_node, lhs1, rhs2);
	  t = build3 (COND_EXPR, type, t, build_zero_cst (type), temp1);
	  g = gimple_build_debug_bind (temp2, t, phi);
	  gsi_insert_before (&gsi, g, GSI_SAME_STMT);
	  replace_uses_by (phires, temp2);
	  if (orig_use_lhs)
	    replace_uses_by (orig_use_lhs, temp2);
	}
    }

  if (orig_use_lhs)
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (orig_use_stmt);
      gsi_remove (&gsi, true);
    }

  gimple_stmt_iterator psi = gsi_for_stmt (phi);
  remove_phi_node (&psi, true);
  statistics_counter_event (cfun, "spaceship replacement", 1);

  return true;
}

/* Optimize x ? __builtin_fun (x) : C, where C is __builtin_fun (0).
   Convert

   <bb 2>
   if (b_4(D) != 0)
   goto <bb 3>
   else
   goto <bb 4>

   <bb 3>
   _2 = (unsigned long) b_4(D);
   _9 = __builtin_popcountl (_2);
   OR
   _9 = __builtin_popcountl (b_4(D));

   <bb 4>
   c_12 = PHI <0(2), _9(3)>

   Into
   <bb 2>
   _2 = (unsigned long) b_4(D);
   _9 = __builtin_popcountl (_2);
   OR
   _9 = __builtin_popcountl (b_4(D));

   <bb 4>
   c_12 = PHI <_9(2)>

   Similarly for __builtin_clz or __builtin_ctz if
   C?Z_DEFINED_VALUE_AT_ZERO is 2, optab is present and
   instead of 0 above it uses the value from that macro.  */

static bool
cond_removal_in_builtin_zero_pattern (basic_block cond_bb,
				      basic_block middle_bb,
				      edge e1, edge e2, gphi *phi,
				      tree arg0, tree arg1)
{
  gimple *cond;
  gimple_stmt_iterator gsi, gsi_from;
  gimple *call;
  gimple *cast = NULL;
  tree lhs, arg;

  /* Check that
   _2 = (unsigned long) b_4(D);
   _9 = __builtin_popcountl (_2);
   OR
   _9 = __builtin_popcountl (b_4(D));
   are the only stmts in the middle_bb.  */

  gsi = gsi_start_nondebug_after_labels_bb (middle_bb);
  if (gsi_end_p (gsi))
    return false;
  cast = gsi_stmt (gsi);
  gsi_next_nondebug (&gsi);
  if (!gsi_end_p (gsi))
    {
      call = gsi_stmt (gsi);
      gsi_next_nondebug (&gsi);
      if (!gsi_end_p (gsi))
	return false;
    }
  else
    {
      call = cast;
      cast = NULL;
    }

  /* Check that we have a popcount/clz/ctz builtin.  */
  if (!is_gimple_call (call) || gimple_call_num_args (call) != 1)
    return false;

  arg = gimple_call_arg (call, 0);
  lhs = gimple_get_lhs (call);

  if (lhs == NULL_TREE)
    return false;

  combined_fn cfn = gimple_call_combined_fn (call);
  internal_fn ifn = IFN_LAST;
  int val = 0;
  switch (cfn)
    {
    case CFN_BUILT_IN_BSWAP16:
    case CFN_BUILT_IN_BSWAP32:
    case CFN_BUILT_IN_BSWAP64:
    case CFN_BUILT_IN_BSWAP128:
    CASE_CFN_FFS:
    CASE_CFN_PARITY:
    CASE_CFN_POPCOUNT:
      break;
    CASE_CFN_CLZ:
      if (INTEGRAL_TYPE_P (TREE_TYPE (arg)))
	{
	  tree type = TREE_TYPE (arg);
	  if (direct_internal_fn_supported_p (IFN_CLZ, type, OPTIMIZE_FOR_BOTH)
	      && CLZ_DEFINED_VALUE_AT_ZERO (SCALAR_INT_TYPE_MODE (type),
					    val) == 2)
	    {
	      ifn = IFN_CLZ;
	      break;
	    }
	}
      return false;
    CASE_CFN_CTZ:
      if (INTEGRAL_TYPE_P (TREE_TYPE (arg)))
	{
	  tree type = TREE_TYPE (arg);
	  if (direct_internal_fn_supported_p (IFN_CTZ, type, OPTIMIZE_FOR_BOTH)
	      && CTZ_DEFINED_VALUE_AT_ZERO (SCALAR_INT_TYPE_MODE (type),
					    val) == 2)
	    {
	      ifn = IFN_CTZ;
	      break;
	    }
	}
      return false;
    case CFN_BUILT_IN_CLRSB:
      val = TYPE_PRECISION (integer_type_node) - 1;
      break;
    case CFN_BUILT_IN_CLRSBL:
      val = TYPE_PRECISION (long_integer_type_node) - 1;
      break;
    case CFN_BUILT_IN_CLRSBLL:
      val = TYPE_PRECISION (long_long_integer_type_node) - 1;
      break;
    default:
      return false;
    }

  if (cast)
    {
      /* We have a cast stmt feeding popcount/clz/ctz builtin.  */
      /* Check that we have a cast prior to that.  */
      if (gimple_code (cast) != GIMPLE_ASSIGN
	  || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (cast)))
	return false;
      /* Result of the cast stmt is the argument to the builtin.  */
      if (arg != gimple_assign_lhs (cast))
	return false;
      arg = gimple_assign_rhs1 (cast);
    }

  cond = last_stmt (cond_bb);

  /* Cond_bb has a check for b_4 [!=|==] 0 before calling the popcount/clz/ctz
     builtin.  */
  if (gimple_code (cond) != GIMPLE_COND
      || (gimple_cond_code (cond) != NE_EXPR
	  && gimple_cond_code (cond) != EQ_EXPR)
      || !integer_zerop (gimple_cond_rhs (cond))
      || arg != gimple_cond_lhs (cond))
    return false;

  /* Canonicalize.  */
  if ((e2->flags & EDGE_TRUE_VALUE
       && gimple_cond_code (cond) == NE_EXPR)
      || (e1->flags & EDGE_TRUE_VALUE
	  && gimple_cond_code (cond) == EQ_EXPR))
    {
      std::swap (arg0, arg1);
      std::swap (e1, e2);
    }

  /* Check PHI arguments.  */
  if (lhs != arg0
      || TREE_CODE (arg1) != INTEGER_CST
      || wi::to_wide (arg1) != val)
    return false;

  /* And insert the popcount/clz/ctz builtin and cast stmt before the
     cond_bb.  */
  gsi = gsi_last_bb (cond_bb);
  if (cast)
    {
      gsi_from = gsi_for_stmt (cast);
      gsi_move_before (&gsi_from, &gsi);
      reset_flow_sensitive_info (gimple_get_lhs (cast));
    }
  gsi_from = gsi_for_stmt (call);
  if (ifn == IFN_LAST || gimple_call_internal_p (call))
    gsi_move_before (&gsi_from, &gsi);
  else
    {
      /* For __builtin_c[lt]z* force .C[LT]Z ifn, because only
	 the latter is well defined at zero.  */
      call = gimple_build_call_internal (ifn, 1, gimple_call_arg (call, 0));
      gimple_call_set_lhs (call, lhs);
      gsi_insert_before (&gsi, call, GSI_SAME_STMT);
      gsi_remove (&gsi_from, true);
    }
  reset_flow_sensitive_info (lhs);

  /* Now update the PHI and remove unneeded bbs.  */
  replace_phi_edge_with_variable (cond_bb, e2, phi, lhs);
  return true;
}

/* Auxiliary functions to determine the set of memory accesses which
   can't trap because they are preceded by accesses to the same memory
   portion.  We do that for MEM_REFs, so we only need to track
   the SSA_NAME of the pointer indirectly referenced.  The algorithm
   simply is a walk over all instructions in dominator order.  When
   we see an MEM_REF we determine if we've already seen a same
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
   overly conservative.

   We currently support a special case that for !TREE_ADDRESSABLE automatic
   variables, it could ignore whether something is a load or store because the
   local stack should be always writable.  */

/* A hash-table of references (MEM_REF/ARRAY_REF/COMPONENT_REF), and in which
   basic block an *_REF through it was seen, which would constitute a
   no-trap region for same accesses.

   Size is needed to support 2 MEM_REFs of different types, like
   MEM<double>(s_1) and MEM<long>(s_1), which would compare equal with
   OEP_ADDRESS_OF.  */
struct ref_to_bb
{
  tree exp;
  HOST_WIDE_INT size;
  unsigned int phase;
  basic_block bb;
};

/* Hashtable helpers.  */

struct refs_hasher : free_ptr_hash<ref_to_bb>
{
  static inline hashval_t hash (const ref_to_bb *);
  static inline bool equal (const ref_to_bb *, const ref_to_bb *);
};

/* Used for quick clearing of the hash-table when we see calls.
   Hash entries with phase < nt_call_phase are invalid.  */
static unsigned int nt_call_phase;

/* The hash function.  */

inline hashval_t
refs_hasher::hash (const ref_to_bb *n)
{
  inchash::hash hstate;
  inchash::add_expr (n->exp, hstate, OEP_ADDRESS_OF);
  hstate.add_hwi (n->size);
  return hstate.end ();
}

/* The equality function of *P1 and *P2.  */

inline bool
refs_hasher::equal (const ref_to_bb *n1, const ref_to_bb *n2)
{
  return operand_equal_p (n1->exp, n2->exp, OEP_ADDRESS_OF)
	 && n1->size == n2->size;
}

class nontrapping_dom_walker : public dom_walker
{
public:
  nontrapping_dom_walker (cdi_direction direction, hash_set<tree> *ps)
    : dom_walker (direction), m_nontrapping (ps), m_seen_refs (128)
  {}

  virtual edge before_dom_children (basic_block);
  virtual void after_dom_children (basic_block);

private:

  /* We see the expression EXP in basic block BB.  If it's an interesting
     expression (an MEM_REF through an SSA_NAME) possibly insert the
     expression into the set NONTRAP or the hash table of seen expressions.
     STORE is true if this expression is on the LHS, otherwise it's on
     the RHS.  */
  void add_or_mark_expr (basic_block, tree, bool);

  hash_set<tree> *m_nontrapping;

  /* The hash table for remembering what we've seen.  */
  hash_table<refs_hasher> m_seen_refs;
};

/* Called by walk_dominator_tree, when entering the block BB.  */
edge
nontrapping_dom_walker::before_dom_children (basic_block bb)
{
  edge e;
  edge_iterator ei;
  gimple_stmt_iterator gsi;

  /* If we haven't seen all our predecessors, clear the hash-table.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    if ((((size_t)e->src->aux) & 2) == 0)
      {
	nt_call_phase++;
	break;
      }

  /* Mark this BB as being on the path to dominator root and as visited.  */
  bb->aux = (void*)(1 | 2);

  /* And walk the statements in order.  */
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      if ((gimple_code (stmt) == GIMPLE_ASM && gimple_vdef (stmt))
	  || (is_gimple_call (stmt)
	      && (!nonfreeing_call_p (stmt) || !nonbarrier_call_p (stmt))))
	nt_call_phase++;
      else if (gimple_assign_single_p (stmt) && !gimple_has_volatile_ops (stmt))
	{
	  add_or_mark_expr (bb, gimple_assign_lhs (stmt), true);
	  add_or_mark_expr (bb, gimple_assign_rhs1 (stmt), false);
	}
    }
  return NULL;
}

/* Called by walk_dominator_tree, when basic block BB is exited.  */
void
nontrapping_dom_walker::after_dom_children (basic_block bb)
{
  /* This BB isn't on the path to dominator root anymore.  */
  bb->aux = (void*)2;
}

/* We see the expression EXP in basic block BB.  If it's an interesting
   expression of:
     1) MEM_REF
     2) ARRAY_REF
     3) COMPONENT_REF
   possibly insert the expression into the set NONTRAP or the hash table
   of seen expressions.  STORE is true if this expression is on the LHS,
   otherwise it's on the RHS.  */
void
nontrapping_dom_walker::add_or_mark_expr (basic_block bb, tree exp, bool store)
{
  HOST_WIDE_INT size;

  if ((TREE_CODE (exp) == MEM_REF || TREE_CODE (exp) == ARRAY_REF
       || TREE_CODE (exp) == COMPONENT_REF)
      && (size = int_size_in_bytes (TREE_TYPE (exp))) > 0)
    {
      struct ref_to_bb map;
      ref_to_bb **slot;
      struct ref_to_bb *r2bb;
      basic_block found_bb = 0;

      if (!store)
	{
	  tree base = get_base_address (exp);
	  /* Only record a LOAD of a local variable without address-taken, as
	     the local stack is always writable.  This allows cselim on a STORE
	     with a dominating LOAD.  */
	  if (!auto_var_p (base) || TREE_ADDRESSABLE (base))
	    return;
	}

      /* Try to find the last seen *_REF, which can trap.  */
      map.exp = exp;
      map.size = size;
      slot = m_seen_refs.find_slot (&map, INSERT);
      r2bb = *slot;
      if (r2bb && r2bb->phase >= nt_call_phase)
	found_bb = r2bb->bb;

      /* If we've found a trapping *_REF, _and_ it dominates EXP
	 (it's in a basic block on the path from us to the dominator root)
	 then we can't trap.  */
      if (found_bb && (((size_t)found_bb->aux) & 1) == 1)
	{
	  m_nontrapping->add (exp);
	}
      else
	{
	  /* EXP might trap, so insert it into the hash table.  */
	  if (r2bb)
	    {
	      r2bb->phase = nt_call_phase;
	      r2bb->bb = bb;
	    }
	  else
	    {
	      r2bb = XNEW (struct ref_to_bb);
	      r2bb->phase = nt_call_phase;
	      r2bb->bb = bb;
	      r2bb->exp = exp;
	      r2bb->size = size;
	      *slot = r2bb;
	    }
	}
    }
}

/* This is the entry point of gathering non trapping memory accesses.
   It will do a dominator walk over the whole function, and it will
   make use of the bb->aux pointers.  It returns a set of trees
   (the MEM_REFs itself) which can't trap.  */
static hash_set<tree> *
get_non_trapping (void)
{
  nt_call_phase = 0;
  hash_set<tree> *nontrap = new hash_set<tree>;

  nontrapping_dom_walker (CDI_DOMINATORS, nontrap)
    .walk (cfun->cfg->x_entry_block_ptr);

  clear_aux_for_blocks ();
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
   memory location dominates us, or the store is to a local addressable
   object) and that the store has a "simple" RHS.  */

static bool
cond_store_replacement (basic_block middle_bb, basic_block join_bb,
			edge e0, edge e1, hash_set<tree> *nontrap)
{
  gimple *assign = last_and_only_stmt (middle_bb);
  tree lhs, rhs, name, name2;
  gphi *newphi;
  gassign *new_stmt;
  gimple_stmt_iterator gsi;
  location_t locus;

  /* Check if middle_bb contains of only one store.  */
  if (!assign
      || !gimple_assign_single_p (assign)
      || gimple_has_volatile_ops (assign))
    return false;

  /* And no PHI nodes so all uses in the single stmt are also
     available where we insert to.  */
  if (!gimple_seq_empty_p (phi_nodes (middle_bb)))
    return false;

  locus = gimple_location (assign);
  lhs = gimple_assign_lhs (assign);
  rhs = gimple_assign_rhs1 (assign);
  if ((!REFERENCE_CLASS_P (lhs)
       && !DECL_P (lhs))
      || !is_gimple_reg_type (TREE_TYPE (lhs)))
    return false;

  /* Prove that we can move the store down.  We could also check
     TREE_THIS_NOTRAP here, but in that case we also could move stores,
     whose value is not available readily, which we want to avoid.  */
  if (!nontrap->contains (lhs))
    {
      /* If LHS is an access to a local variable without address-taken
	 (or when we allow data races) and known not to trap, we could
	 always safely move down the store.  */
      tree base = get_base_address (lhs);
      if (!auto_var_p (base)
	  || (TREE_ADDRESSABLE (base) && !flag_store_data_races)
	  || tree_could_trap_p (lhs))
	return false;
    }

  /* Now we've checked the constraints, so do the transformation:
     1) Remove the single store.  */
  gsi = gsi_for_stmt (assign);
  unlink_stmt_vdef (assign);
  gsi_remove (&gsi, true);
  release_defs (assign);

  /* Make both store and load use alias-set zero as we have to
     deal with the case of the store being a conditional change
     of the dynamic type.  */
  lhs = unshare_expr (lhs);
  tree *basep = &lhs;
  while (handled_component_p (*basep))
    basep = &TREE_OPERAND (*basep, 0);
  if (TREE_CODE (*basep) == MEM_REF
      || TREE_CODE (*basep) == TARGET_MEM_REF)
    TREE_OPERAND (*basep, 1)
      = fold_convert (ptr_type_node, TREE_OPERAND (*basep, 1));
  else
    *basep = build2 (MEM_REF, TREE_TYPE (*basep),
		     build_fold_addr_expr (*basep),
		     build_zero_cst (ptr_type_node));

  /* 2) Insert a load from the memory of the store to the temporary
        on the edge which did not contain the store.  */
  name = make_temp_ssa_name (TREE_TYPE (lhs), NULL, "cstore");
  new_stmt = gimple_build_assign (name, lhs);
  gimple_set_location (new_stmt, locus);
  lhs = unshare_expr (lhs);
  {
    /* Set the no-warning bit on the rhs of the load to avoid uninit
       warnings.  */
    tree rhs1 = gimple_assign_rhs1 (new_stmt);
    suppress_warning (rhs1, OPT_Wuninitialized);
  }
  gsi_insert_on_edge (e1, new_stmt);

  /* 3) Create a PHI node at the join block, with one argument
        holding the old RHS, and the other holding the temporary
        where we stored the old memory contents.  */
  name2 = make_temp_ssa_name (TREE_TYPE (lhs), NULL, "cstore");
  newphi = create_phi_node (name2, join_bb);
  add_phi_arg (newphi, rhs, e0, locus);
  add_phi_arg (newphi, name, e1, locus);

  new_stmt = gimple_build_assign (lhs, PHI_RESULT (newphi));

  /* 4) Insert that PHI node.  */
  gsi = gsi_after_labels (join_bb);
  if (gsi_end_p (gsi))
    {
      gsi = gsi_last_bb (join_bb);
      gsi_insert_after (&gsi, new_stmt, GSI_NEW_STMT);
    }
  else
    gsi_insert_before (&gsi, new_stmt, GSI_NEW_STMT);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nConditional store replacement happened!");
      fprintf (dump_file, "\nReplaced the store with a load.");
      fprintf (dump_file, "\nInserted a new PHI statement in joint block:\n");
      print_gimple_stmt (dump_file, new_stmt, 0, TDF_VOPS|TDF_MEMSYMS);
    }
  statistics_counter_event (cfun, "conditional store replacement", 1);

  return true;
}

/* Do the main work of conditional store replacement.  */

static bool
cond_if_else_store_replacement_1 (basic_block then_bb, basic_block else_bb,
				  basic_block join_bb, gimple *then_assign,
				  gimple *else_assign)
{
  tree lhs_base, lhs, then_rhs, else_rhs, name;
  location_t then_locus, else_locus;
  gimple_stmt_iterator gsi;
  gphi *newphi;
  gassign *new_stmt;

  if (then_assign == NULL
      || !gimple_assign_single_p (then_assign)
      || gimple_clobber_p (then_assign)
      || gimple_has_volatile_ops (then_assign)
      || else_assign == NULL
      || !gimple_assign_single_p (else_assign)
      || gimple_clobber_p (else_assign)
      || gimple_has_volatile_ops (else_assign))
    return false;

  lhs = gimple_assign_lhs (then_assign);
  if (!is_gimple_reg_type (TREE_TYPE (lhs))
      || !operand_equal_p (lhs, gimple_assign_lhs (else_assign), 0))
    return false;

  lhs_base = get_base_address (lhs);
  if (lhs_base == NULL_TREE
      || (!DECL_P (lhs_base) && TREE_CODE (lhs_base) != MEM_REF))
    return false;

  then_rhs = gimple_assign_rhs1 (then_assign);
  else_rhs = gimple_assign_rhs1 (else_assign);
  then_locus = gimple_location (then_assign);
  else_locus = gimple_location (else_assign);

  /* Now we've checked the constraints, so do the transformation:
     1) Remove the stores.  */
  gsi = gsi_for_stmt (then_assign);
  unlink_stmt_vdef (then_assign);
  gsi_remove (&gsi, true);
  release_defs (then_assign);

  gsi = gsi_for_stmt (else_assign);
  unlink_stmt_vdef (else_assign);
  gsi_remove (&gsi, true);
  release_defs (else_assign);

  /* 2) Create a PHI node at the join block, with one argument
	holding the old RHS, and the other holding the temporary
	where we stored the old memory contents.  */
  name = make_temp_ssa_name (TREE_TYPE (lhs), NULL, "cstore");
  newphi = create_phi_node (name, join_bb);
  add_phi_arg (newphi, then_rhs, EDGE_SUCC (then_bb, 0), then_locus);
  add_phi_arg (newphi, else_rhs, EDGE_SUCC (else_bb, 0), else_locus);

  new_stmt = gimple_build_assign (lhs, PHI_RESULT (newphi));

  /* 3) Insert that PHI node.  */
  gsi = gsi_after_labels (join_bb);
  if (gsi_end_p (gsi))
    {
      gsi = gsi_last_bb (join_bb);
      gsi_insert_after (&gsi, new_stmt, GSI_NEW_STMT);
    }
  else
    gsi_insert_before (&gsi, new_stmt, GSI_NEW_STMT);

  statistics_counter_event (cfun, "if-then-else store replacement", 1);

  return true;
}

/* Return the single store in BB with VDEF or NULL if there are
   other stores in the BB or loads following the store.  */

static gimple *
single_trailing_store_in_bb (basic_block bb, tree vdef)
{
  if (SSA_NAME_IS_DEFAULT_DEF (vdef))
    return NULL;
  gimple *store = SSA_NAME_DEF_STMT (vdef);
  if (gimple_bb (store) != bb
      || gimple_code (store) == GIMPLE_PHI)
    return NULL;

  /* Verify there is no other store in this BB.  */
  if (!SSA_NAME_IS_DEFAULT_DEF (gimple_vuse (store))
      && gimple_bb (SSA_NAME_DEF_STMT (gimple_vuse (store))) == bb
      && gimple_code (SSA_NAME_DEF_STMT (gimple_vuse (store))) != GIMPLE_PHI)
    return NULL;

  /* Verify there is no load or store after the store.  */
  use_operand_p use_p;
  imm_use_iterator imm_iter;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, gimple_vdef (store))
    if (USE_STMT (use_p) != store
	&& gimple_bb (USE_STMT (use_p)) == bb)
      return NULL;

  return store;
}

/* Conditional store replacement.  We already know
   that the recognized pattern looks like so:

   split:
     if (cond) goto THEN_BB; else goto ELSE_BB (edge E1)
   THEN_BB:
     ...
     X = Y;
     ...
     goto JOIN_BB;
   ELSE_BB:
     ...
     X = Z;
     ...
     fallthrough (edge E0)
   JOIN_BB:
     some more

   We check that it is safe to sink the store to JOIN_BB by verifying that
   there are no read-after-write or write-after-write dependencies in
   THEN_BB and ELSE_BB.  */

static bool
cond_if_else_store_replacement (basic_block then_bb, basic_block else_bb,
                                basic_block join_bb)
{
  vec<data_reference_p> then_datarefs, else_datarefs;
  vec<ddr_p> then_ddrs, else_ddrs;
  gimple *then_store, *else_store;
  bool found, ok = false, res;
  struct data_dependence_relation *ddr;
  data_reference_p then_dr, else_dr;
  int i, j;
  tree then_lhs, else_lhs;
  basic_block blocks[3];

  /* Handle the case with single store in THEN_BB and ELSE_BB.  That is
     cheap enough to always handle as it allows us to elide dependence
     checking.  */
  gphi *vphi = NULL;
  for (gphi_iterator si = gsi_start_phis (join_bb); !gsi_end_p (si);
       gsi_next (&si))
    if (virtual_operand_p (gimple_phi_result (si.phi ())))
      {
	vphi = si.phi ();
	break;
      }
  if (!vphi)
    return false;
  tree then_vdef = PHI_ARG_DEF_FROM_EDGE (vphi, single_succ_edge (then_bb));
  tree else_vdef = PHI_ARG_DEF_FROM_EDGE (vphi, single_succ_edge (else_bb));
  gimple *then_assign = single_trailing_store_in_bb (then_bb, then_vdef);
  if (then_assign)
    {
      gimple *else_assign = single_trailing_store_in_bb (else_bb, else_vdef);
      if (else_assign)
	return cond_if_else_store_replacement_1 (then_bb, else_bb, join_bb,
						 then_assign, else_assign);
    }

  /* If either vectorization or if-conversion is disabled then do
     not sink any stores.  */
  if (param_max_stores_to_sink == 0
      || (!flag_tree_loop_vectorize && !flag_tree_slp_vectorize)
      || !flag_tree_loop_if_convert)
    return false;

  /* Find data references.  */
  then_datarefs.create (1);
  else_datarefs.create (1);
  if ((find_data_references_in_bb (NULL, then_bb, &then_datarefs)
        == chrec_dont_know)
      || !then_datarefs.length ()
      || (find_data_references_in_bb (NULL, else_bb, &else_datarefs)
	  == chrec_dont_know)
      || !else_datarefs.length ())
    {
      free_data_refs (then_datarefs);
      free_data_refs (else_datarefs);
      return false;
    }

  /* Find pairs of stores with equal LHS.  */
  auto_vec<gimple *, 1> then_stores, else_stores;
  FOR_EACH_VEC_ELT (then_datarefs, i, then_dr)
    {
      if (DR_IS_READ (then_dr))
        continue;

      then_store = DR_STMT (then_dr);
      then_lhs = gimple_get_lhs (then_store);
      if (then_lhs == NULL_TREE)
	continue;
      found = false;

      FOR_EACH_VEC_ELT (else_datarefs, j, else_dr)
        {
          if (DR_IS_READ (else_dr))
            continue;

          else_store = DR_STMT (else_dr);
          else_lhs = gimple_get_lhs (else_store);
	  if (else_lhs == NULL_TREE)
	    continue;

          if (operand_equal_p (then_lhs, else_lhs, 0))
            {
              found = true;
              break;
            }
        }

      if (!found)
        continue;

      then_stores.safe_push (then_store);
      else_stores.safe_push (else_store);
    }

  /* No pairs of stores found.  */
  if (!then_stores.length ()
      || then_stores.length () > (unsigned) param_max_stores_to_sink)
    {
      free_data_refs (then_datarefs);
      free_data_refs (else_datarefs);
      return false;
    }

  /* Compute and check data dependencies in both basic blocks.  */
  then_ddrs.create (1);
  else_ddrs.create (1);
  if (!compute_all_dependences (then_datarefs, &then_ddrs,
				vNULL, false)
      || !compute_all_dependences (else_datarefs, &else_ddrs,
				   vNULL, false))
    {
      free_dependence_relations (then_ddrs);
      free_dependence_relations (else_ddrs);
      free_data_refs (then_datarefs);
      free_data_refs (else_datarefs);
      return false;
    }
  blocks[0] = then_bb;
  blocks[1] = else_bb;
  blocks[2] = join_bb;
  renumber_gimple_stmt_uids_in_blocks (blocks, 3);

  /* Check that there are no read-after-write or write-after-write dependencies
     in THEN_BB.  */
  FOR_EACH_VEC_ELT (then_ddrs, i, ddr)
    {
      struct data_reference *dra = DDR_A (ddr);
      struct data_reference *drb = DDR_B (ddr);

      if (DDR_ARE_DEPENDENT (ddr) != chrec_known
          && ((DR_IS_READ (dra) && DR_IS_WRITE (drb)
               && gimple_uid (DR_STMT (dra)) > gimple_uid (DR_STMT (drb)))
              || (DR_IS_READ (drb) && DR_IS_WRITE (dra)
                  && gimple_uid (DR_STMT (drb)) > gimple_uid (DR_STMT (dra)))
              || (DR_IS_WRITE (dra) && DR_IS_WRITE (drb))))
        {
          free_dependence_relations (then_ddrs);
          free_dependence_relations (else_ddrs);
	  free_data_refs (then_datarefs);
	  free_data_refs (else_datarefs);
          return false;
        }
    }

  /* Check that there are no read-after-write or write-after-write dependencies
     in ELSE_BB.  */
  FOR_EACH_VEC_ELT (else_ddrs, i, ddr)
    {
      struct data_reference *dra = DDR_A (ddr);
      struct data_reference *drb = DDR_B (ddr);

      if (DDR_ARE_DEPENDENT (ddr) != chrec_known
          && ((DR_IS_READ (dra) && DR_IS_WRITE (drb)
               && gimple_uid (DR_STMT (dra)) > gimple_uid (DR_STMT (drb)))
              || (DR_IS_READ (drb) && DR_IS_WRITE (dra)
                  && gimple_uid (DR_STMT (drb)) > gimple_uid (DR_STMT (dra)))
              || (DR_IS_WRITE (dra) && DR_IS_WRITE (drb))))
        {
          free_dependence_relations (then_ddrs);
          free_dependence_relations (else_ddrs);
	  free_data_refs (then_datarefs);
	  free_data_refs (else_datarefs);
          return false;
        }
    }

  /* Sink stores with same LHS.  */
  FOR_EACH_VEC_ELT (then_stores, i, then_store)
    {
      else_store = else_stores[i];
      res = cond_if_else_store_replacement_1 (then_bb, else_bb, join_bb,
                                              then_store, else_store);
      ok = ok || res;
    }

  free_dependence_relations (then_ddrs);
  free_dependence_relations (else_ddrs);
  free_data_refs (then_datarefs);
  free_data_refs (else_datarefs);

  return ok;
}

/* Return TRUE if STMT has a VUSE whose corresponding VDEF is in BB.  */

static bool
local_mem_dependence (gimple *stmt, basic_block bb)
{
  tree vuse = gimple_vuse (stmt);
  gimple *def;

  if (!vuse)
    return false;

  def = SSA_NAME_DEF_STMT (vuse);
  return (def && gimple_bb (def) == bb);
}

/* Given a "diamond" control-flow pattern where BB0 tests a condition,
   BB1 and BB2 are "then" and "else" blocks dependent on this test,
   and BB3 rejoins control flow following BB1 and BB2, look for
   opportunities to hoist loads as follows.  If BB3 contains a PHI of
   two loads, one each occurring in BB1 and BB2, and the loads are
   provably of adjacent fields in the same structure, then move both
   loads into BB0.  Of course this can only be done if there are no
   dependencies preventing such motion.

   One of the hoisted loads will always be speculative, so the
   transformation is currently conservative:

    - The fields must be strictly adjacent.
    - The two fields must occupy a single memory block that is
      guaranteed to not cross a page boundary.

    The last is difficult to prove, as such memory blocks should be
    aligned on the minimum of the stack alignment boundary and the
    alignment guaranteed by heap allocation interfaces.  Thus we rely
    on a parameter for the alignment value.

    Provided a good value is used for the last case, the first
    restriction could possibly be relaxed.  */

static void
hoist_adjacent_loads (basic_block bb0, basic_block bb1,
		      basic_block bb2, basic_block bb3)
{
  int param_align = param_l1_cache_line_size;
  unsigned param_align_bits = (unsigned) (param_align * BITS_PER_UNIT);
  gphi_iterator gsi;

  /* Walk the phis in bb3 looking for an opportunity.  We are looking
     for phis of two SSA names, one each of which is defined in bb1 and
     bb2.  */
  for (gsi = gsi_start_phis (bb3); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi_stmt = gsi.phi ();
      gimple *def1, *def2;
      tree arg1, arg2, ref1, ref2, field1, field2;
      tree tree_offset1, tree_offset2, tree_size2, next;
      int offset1, offset2, size2;
      unsigned align1;
      gimple_stmt_iterator gsi2;
      basic_block bb_for_def1, bb_for_def2;

      if (gimple_phi_num_args (phi_stmt) != 2
	  || virtual_operand_p (gimple_phi_result (phi_stmt)))
	continue;

      arg1 = gimple_phi_arg_def (phi_stmt, 0);
      arg2 = gimple_phi_arg_def (phi_stmt, 1);

      if (TREE_CODE (arg1) != SSA_NAME
	  || TREE_CODE (arg2) != SSA_NAME
	  || SSA_NAME_IS_DEFAULT_DEF (arg1)
	  || SSA_NAME_IS_DEFAULT_DEF (arg2))
	continue;

      def1 = SSA_NAME_DEF_STMT (arg1);
      def2 = SSA_NAME_DEF_STMT (arg2);

      if ((gimple_bb (def1) != bb1 || gimple_bb (def2) != bb2)
	  && (gimple_bb (def2) != bb1 || gimple_bb (def1) != bb2))
	continue;

      /* Check the mode of the arguments to be sure a conditional move
	 can be generated for it.  */
      if (optab_handler (movcc_optab, TYPE_MODE (TREE_TYPE (arg1)))
	  == CODE_FOR_nothing)
	continue;

      /* Both statements must be assignments whose RHS is a COMPONENT_REF.  */
      if (!gimple_assign_single_p (def1)
	  || !gimple_assign_single_p (def2)
	  || gimple_has_volatile_ops (def1)
	  || gimple_has_volatile_ops (def2))
	continue;

      ref1 = gimple_assign_rhs1 (def1);
      ref2 = gimple_assign_rhs1 (def2);

      if (TREE_CODE (ref1) != COMPONENT_REF
	  || TREE_CODE (ref2) != COMPONENT_REF)
	continue;

      /* The zeroth operand of the two component references must be
	 identical.  It is not sufficient to compare get_base_address of
	 the two references, because this could allow for different
	 elements of the same array in the two trees.  It is not safe to
	 assume that the existence of one array element implies the
	 existence of a different one.  */
      if (!operand_equal_p (TREE_OPERAND (ref1, 0), TREE_OPERAND (ref2, 0), 0))
	continue;

      field1 = TREE_OPERAND (ref1, 1);
      field2 = TREE_OPERAND (ref2, 1);

      /* Check for field adjacency, and ensure field1 comes first.  */
      for (next = DECL_CHAIN (field1);
	   next && TREE_CODE (next) != FIELD_DECL;
	   next = DECL_CHAIN (next))
	;

      if (next != field2)
	{
	  for (next = DECL_CHAIN (field2);
	       next && TREE_CODE (next) != FIELD_DECL;
	       next = DECL_CHAIN (next))
	    ;

	  if (next != field1)
	    continue;

	  std::swap (field1, field2);
	  std::swap (def1, def2);
	}

      bb_for_def1 = gimple_bb (def1);
      bb_for_def2 = gimple_bb (def2);

      /* Check for proper alignment of the first field.  */
      tree_offset1 = bit_position (field1);
      tree_offset2 = bit_position (field2);
      tree_size2 = DECL_SIZE (field2);

      if (!tree_fits_uhwi_p (tree_offset1)
	  || !tree_fits_uhwi_p (tree_offset2)
	  || !tree_fits_uhwi_p (tree_size2))
	continue;

      offset1 = tree_to_uhwi (tree_offset1);
      offset2 = tree_to_uhwi (tree_offset2);
      size2 = tree_to_uhwi (tree_size2);
      align1 = DECL_ALIGN (field1) % param_align_bits;

      if (offset1 % BITS_PER_UNIT != 0)
	continue;

      /* For profitability, the two field references should fit within
	 a single cache line.  */
      if (align1 + offset2 - offset1 + size2 > param_align_bits)
	continue;

      /* The two expressions cannot be dependent upon vdefs defined
	 in bb1/bb2.  */
      if (local_mem_dependence (def1, bb_for_def1)
	  || local_mem_dependence (def2, bb_for_def2))
	continue;

      /* The conditions are satisfied; hoist the loads from bb1 and bb2 into
	 bb0.  We hoist the first one first so that a cache miss is handled
         efficiently regardless of hardware cache-fill policy.  */
      gsi2 = gsi_for_stmt (def1);
      gsi_move_to_bb_end (&gsi2, bb0);
      gsi2 = gsi_for_stmt (def2);
      gsi_move_to_bb_end (&gsi2, bb0);
      statistics_counter_event (cfun, "hoisted loads", 1);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file,
		   "\nHoisting adjacent loads from %d and %d into %d: \n",
		   bb_for_def1->index, bb_for_def2->index, bb0->index);
	  print_gimple_stmt (dump_file, def1, 0, TDF_VOPS|TDF_MEMSYMS);
	  print_gimple_stmt (dump_file, def2, 0, TDF_VOPS|TDF_MEMSYMS);
	}
    }
}

/* Determine whether we should attempt to hoist adjacent loads out of
   diamond patterns in pass_phiopt.  Always hoist loads if
   -fhoist-adjacent-loads is specified and the target machine has
   both a conditional move instruction and a defined cache line size.  */

static bool
gate_hoist_loads (void)
{
  return (flag_hoist_adjacent_loads == 1
	  && param_l1_cache_line_size
	  && HAVE_conditional_move);
}

/* This pass tries to replaces an if-then-else block with an
   assignment.  We have four kinds of transformations.  Some of these
   transformations are also performed by the ifcvt RTL optimizer.

   Conditional Replacement
   -----------------------

   This transformation, implemented in match_simplify_replacement,
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


   Another case caught by value replacement looks like this:

     bb0:
       t1 = a == CONST;
       t2 = b > c;
       t3 = t1 & t2;
       if (t3 != 0) goto bb1; else goto bb2;
     bb1:
     bb2:
       x = PHI (CONST, a)

   Gets replaced with:
     bb0:
     bb2:
       t1 = a == CONST;
       t2 = b > c;
       t3 = t1 & t2;
       x = a;

   ABS Replacement
   ---------------

   This transformation, implemented in match_simplify_replacement, replaces

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

   A similar transformation is done for MAX_EXPR.


   This pass also performs a fifth transformation of a slightly different
   flavor.

   Factor conversion in COND_EXPR
   ------------------------------

   This transformation factors the conversion out of COND_EXPR with
   factor_out_conditional_conversion.

   For example:
   if (a <= CST) goto <bb 3>; else goto <bb 4>;
   <bb 3>:
   tmp = (int) a;
   <bb 4>:
   tmp = PHI <tmp, CST>

   Into:
   if (a <= CST) goto <bb 3>; else goto <bb 4>;
   <bb 3>:
   <bb 4>:
   a = PHI <a, CST>
   tmp = (int) a;

   Adjacent Load Hoisting
   ----------------------

   This transformation replaces

     bb0:
       if (...) goto bb2; else goto bb1;
     bb1:
       x1 = (<expr>).field1;
       goto bb3;
     bb2:
       x2 = (<expr>).field2;
     bb3:
       # x = PHI <x1, x2>;

   with

     bb0:
       x1 = (<expr>).field1;
       x2 = (<expr>).field2;
       if (...) goto bb2; else goto bb1;
     bb1:
       goto bb3;
     bb2:
     bb3:
       # x = PHI <x1, x2>;

   The purpose of this transformation is to enable generation of conditional
   move instructions such as Intel CMOVE or PowerPC ISEL.  Because one of
   the loads is speculative, the transformation is restricted to very
   specific cases to avoid introducing a page fault.  We are looking for
   the common idiom:

     if (...)
       x = y->left;
     else
       x = y->right;

   where left and right are typically adjacent pointers in a tree structure.  */

namespace {

const pass_data pass_data_phiopt =
{
  GIMPLE_PASS, /* type */
  "phiopt", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_PHIOPT, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_phiopt : public gimple_opt_pass
{
public:
  pass_phiopt (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_phiopt, ctxt), early_p (false)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_phiopt (m_ctxt); }
  void set_pass_param (unsigned n, bool param)
    {
      gcc_assert (n == 0);
      early_p = param;
    }
  virtual bool gate (function *) { return flag_ssa_phiopt; }
  virtual unsigned int execute (function *)
    {
      return tree_ssa_phiopt_worker (false,
				     !early_p ? gate_hoist_loads () : false,
				     early_p);
    }

private:
  bool early_p;
}; // class pass_phiopt

} // anon namespace

gimple_opt_pass *
make_pass_phiopt (gcc::context *ctxt)
{
  return new pass_phiopt (ctxt);
}

namespace {

const pass_data pass_data_cselim =
{
  GIMPLE_PASS, /* type */
  "cselim", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_PHIOPT, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_cselim : public gimple_opt_pass
{
public:
  pass_cselim (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_cselim, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_tree_cselim; }
  virtual unsigned int execute (function *) { return tree_ssa_cs_elim (); }

}; // class pass_cselim

} // anon namespace

gimple_opt_pass *
make_pass_cselim (gcc::context *ctxt)
{
  return new pass_cselim (ctxt);
}
