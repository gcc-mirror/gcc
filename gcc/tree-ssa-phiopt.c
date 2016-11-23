/* Optimization of PHI nodes by converting them into straightline code.
   Copyright (C) 2004-2016 Free Software Foundation, Inc.

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
#include "params.h"

static unsigned int tree_ssa_phiopt_worker (bool, bool);
static bool conditional_replacement (basic_block, basic_block,
				     edge, edge, gphi *, tree, tree);
static gphi *factor_out_conditional_conversion (edge, edge, gphi *, tree, tree);
static int value_replacement (basic_block, basic_block,
			      edge, edge, gimple *, tree, tree);
static bool minmax_replacement (basic_block, basic_block,
				edge, edge, gimple *, tree, tree);
static bool abs_replacement (basic_block, basic_block,
			     edge, edge, gimple *, tree, tree);
static bool cond_store_replacement (basic_block, basic_block, edge, edge,
				    hash_set<tree> *);
static bool cond_if_else_store_replacement (basic_block, basic_block, basic_block);
static hash_set<tree> * get_non_trapping ();
static void replace_phi_edge_with_variable (basic_block, edge, gimple *, tree);
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
  todo = tree_ssa_phiopt_worker (true, false);
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
tree_ssa_phiopt_worker (bool do_store_elim, bool do_hoist_loads)
{
  basic_block bb;
  basic_block *bb_order;
  unsigned n, i;
  bool cfgchanged = false;
  hash_set<tree> *nontrap = 0;

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
							    arg0, arg1);
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
	  if (conditional_replacement (bb, bb1, e1, e2, phi, arg0, arg1))
	    cfgchanged = true;
	  else if (abs_replacement (bb, bb1, e1, e2, phi, arg0, arg1))
	    cfgchanged = true;
	  else if (minmax_replacement (bb, bb1, e1, e2, phi, arg0, arg1))
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
				edge e, gimple *phi, tree new_tree)
{
  basic_block bb = gimple_bb (phi);
  basic_block block_to_remove;
  gimple_stmt_iterator gsi;

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
  gsi = gsi_last_bb (cond_block);
  gsi_remove (&gsi, true);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	      "COND_EXPR in block %d and PHI in block %d converted to straightline code.\n",
	      cond_block->index,
	      bb->index);
}

/* PR66726: Factor conversion out of COND_EXPR.  If the arguments of the PHI
   stmt are CONVERT_STMT, factor out the conversion and perform the conversion
   to the result of PHI stmt.  Return the newly-created PHI, if any.  */

static gphi *
factor_out_conditional_conversion (edge e0, edge e1, gphi *phi,
				   tree arg0, tree arg1)
{
  gimple *arg0_def_stmt = NULL, *arg1_def_stmt = NULL, *new_stmt;
  tree new_arg0 = NULL_TREE, new_arg1 = NULL_TREE;
  tree temp, result;
  gphi *newphi;
  gimple_stmt_iterator gsi, gsi_for_def;
  source_location locus = gimple_location (phi);
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

  if (TREE_CODE (arg1) == SSA_NAME)
    {
      /* Check if arg1 is an SSA_NAME and the stmt which defines arg1
	 is a conversion.  */
      arg1_def_stmt = SSA_NAME_DEF_STMT (arg1);
      if (!is_gimple_assign (arg1_def_stmt)
	  || gimple_assign_rhs_code (arg1_def_stmt) != convert_code)
	return NULL;

      /* Use the RHS as new_arg1.  */
      new_arg1 = gimple_assign_rhs1 (arg1_def_stmt);
      if (convert_code == VIEW_CONVERT_EXPR)
	new_arg1 = TREE_OPERAND (new_arg1, 0);
    }
  else
    {
      /* If arg1 is an INTEGER_CST, fold it to new type.  */
      if (INTEGRAL_TYPE_P (TREE_TYPE (new_arg0))
	  && int_fits_type_p (arg1, TREE_TYPE (new_arg0)))
	{
	  if (gimple_assign_cast_p (arg0_def_stmt))
	    new_arg1 = fold_convert (TREE_TYPE (new_arg0), arg1);
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
      print_generic_expr (dump_file, gimple_phi_result (phi), 0);
      fprintf (dump_file,
	       " changed to factor conversion out from COND_EXPR.\n");
      fprintf (dump_file, "New stmt with CAST that defines ");
      print_generic_expr (dump_file, result, 0);
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
    temp = fold_build1 (VIEW_CONVERT_EXPR, TREE_TYPE (result), temp);
  new_stmt = gimple_build_assign (result,  convert_code, temp);
  gsi = gsi_after_labels (gimple_bb (phi));
  gsi_insert_before (&gsi, new_stmt, GSI_SAME_STMT);

  /* Remove the original PHI stmt.  */
  gsi = gsi_for_stmt (phi);
  gsi_remove (&gsi, true);
  return newphi;
}

/*  The function conditional_replacement does the main work of doing the
    conditional replacement.  Return true if the replacement is done.
    Otherwise return false.
    BB is the basic block where the replacement is going to be done on.  ARG0
    is argument 0 from PHI.  Likewise for ARG1.  */

static bool
conditional_replacement (basic_block cond_bb, basic_block middle_bb,
			 edge e0, edge e1, gphi *phi,
			 tree arg0, tree arg1)
{
  tree result;
  gimple *stmt;
  gassign *new_stmt;
  tree cond;
  gimple_stmt_iterator gsi;
  edge true_edge, false_edge;
  tree new_var, new_var2;
  bool neg;

  /* FIXME: Gimplification of complex type is too hard for now.  */
  /* We aren't prepared to handle vectors either (and it is a question
     if it would be worthwhile anyway).  */
  if (!(INTEGRAL_TYPE_P (TREE_TYPE (arg0))
	|| POINTER_TYPE_P (TREE_TYPE (arg0)))
      || !(INTEGRAL_TYPE_P (TREE_TYPE (arg1))
	   || POINTER_TYPE_P (TREE_TYPE (arg1))))
    return false;

  /* The PHI arguments have the constants 0 and 1, or 0 and -1, then
     convert it to the conditional.  */
  if ((integer_zerop (arg0) && integer_onep (arg1))
      || (integer_zerop (arg1) && integer_onep (arg0)))
    neg = false;
  else if ((integer_zerop (arg0) && integer_all_onesp (arg1))
	   || (integer_zerop (arg1) && integer_all_onesp (arg0)))
    neg = true;
  else
    return false;

  if (!empty_block_p (middle_bb))
    return false;

  /* At this point we know we have a GIMPLE_COND with two successors.
     One successor is BB, the other successor is an empty block which
     falls through into BB.

     There is a single PHI node at the join point (BB) and its arguments
     are constants (0, 1) or (0, -1).

     So, given the condition COND, and the two PHI arguments, we can
     rewrite this PHI into non-branching code:

       dest = (COND) or dest = COND'

     We use the condition as-is if the argument associated with the
     true edge has the value one or the argument associated with the
     false edge as the value zero.  Note that those conditions are not
     the same since only one of the outgoing edges from the GIMPLE_COND
     will directly reach BB and thus be associated with an argument.  */

  stmt = last_stmt (cond_bb);
  result = PHI_RESULT (phi);

  /* To handle special cases like floating point comparison, it is easier and
     less error-prone to build a tree and gimplify it on the fly though it is
     less efficient.  */
  cond = fold_build2_loc (gimple_location (stmt),
			  gimple_cond_code (stmt), boolean_type_node,
			  gimple_cond_lhs (stmt), gimple_cond_rhs (stmt));

  /* We need to know which is the true edge and which is the false
     edge so that we know when to invert the condition below.  */
  extract_true_false_edges_from_block (cond_bb, &true_edge, &false_edge);
  if ((e0 == true_edge && integer_zerop (arg0))
      || (e0 == false_edge && !integer_zerop (arg0))
      || (e1 == true_edge && integer_zerop (arg1))
      || (e1 == false_edge && !integer_zerop (arg1)))
    cond = fold_build1_loc (gimple_location (stmt),
                            TRUTH_NOT_EXPR, TREE_TYPE (cond), cond);

  if (neg)
    {
      cond = fold_convert_loc (gimple_location (stmt),
                               TREE_TYPE (result), cond);
      cond = fold_build1_loc (gimple_location (stmt),
                              NEGATE_EXPR, TREE_TYPE (cond), cond);
    }

  /* Insert our new statements at the end of conditional block before the
     COND_STMT.  */
  gsi = gsi_for_stmt (stmt);
  new_var = force_gimple_operand_gsi (&gsi, cond, true, NULL, true,
				      GSI_SAME_STMT);

  if (!useless_type_conversion_p (TREE_TYPE (result), TREE_TYPE (new_var)))
    {
      source_location locus_0, locus_1;

      new_var2 = make_ssa_name (TREE_TYPE (result));
      new_stmt = gimple_build_assign (new_var2, CONVERT_EXPR, new_var);
      gsi_insert_before (&gsi, new_stmt, GSI_SAME_STMT);
      new_var = new_var2;

      /* Set the locus to the first argument, unless is doesn't have one.  */
      locus_0 = gimple_phi_arg_location (phi, 0);
      locus_1 = gimple_phi_arg_location (phi, 1);
      if (locus_0 == UNKNOWN_LOCATION)
        locus_0 = locus_1;
      gimple_set_location (new_stmt, locus_0);
    }

  replace_phi_edge_with_variable (cond_bb, e1, phi, new_var);
  reset_flow_sensitive_info_in_bb (cond_bb);

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
      HOST_WIDE_INT offset;
      tree tem = get_addr_base_and_unit_offset (TREE_OPERAND (rhs1, 0),
						&offset);
      if (tem
	  && TREE_CODE (tem) == MEM_REF
	  && (mem_ref_offset (tem) + offset) == 0)
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
absorbing_element_p (tree_code code, tree arg)
{
  switch (code)
    {
    case BIT_IOR_EXPR:
      return integer_all_onesp (arg);

    case MULT_EXPR:
    case BIT_AND_EXPR:
      return integer_zerop (arg);

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
		   edge e0, edge e1, gimple *phi,
		   tree arg0, tree arg1)
{
  gimple_stmt_iterator gsi;
  gimple *cond;
  edge true_edge, false_edge;
  enum tree_code code;
  bool emtpy_or_with_defined_p = true;

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
	  emtpy_or_with_defined_p = false;
	  continue;
	}
      /* Now try to adjust arg0 or arg1 according to the computation
	 in the statement.  */
      lhs = gimple_assign_lhs (stmt);
      if (!(lhs == arg0
	     && jump_function_from_stmt (&arg0, stmt))
	    || (lhs == arg1
		&& jump_function_from_stmt (&arg1, stmt)))
	emtpy_or_with_defined_p = false;
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
      if (emtpy_or_with_defined_p
	  && single_non_singleton_phi_for_edges (phi_nodes (gimple_bb (phi)),
						 e0, e1) == phi)
	{
          replace_phi_edge_with_variable (cond_bb, e1, phi, arg);
	  /* Note that we optimized this PHI.  */
	  return 2;
	}
      else
	{
	  /* Replace the PHI arguments with arg. */
	  SET_PHI_ARG_DEF (phi, e0->dest_idx, arg);
	  SET_PHI_ARG_DEF (phi, e1->dest_idx, arg);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "PHI ");
	      print_generic_expr (dump_file, gimple_phi_result (phi), 0);
	      fprintf (dump_file, " reduced for COND_EXPR in block %d to ",
		       cond_bb->index);
	      print_generic_expr (dump_file, arg, 0);
	      fprintf (dump_file, ".\n");
            }
          return 1;
	}

    }

  /* Now optimize (x != 0) ? x + y : y to just y.
     The following condition is too restrictive, there can easily be another
     stmt in middle_bb, for instance a CONVERT_EXPR for the second argument.  */
  gimple *assign = last_and_only_stmt (middle_bb);
  if (!assign || gimple_code (assign) != GIMPLE_ASSIGN
      || gimple_assign_rhs_class (assign) != GIMPLE_BINARY_RHS
      || (!INTEGRAL_TYPE_P (TREE_TYPE (arg0))
	  && !POINTER_TYPE_P (TREE_TYPE (arg0))))
    return 0;

  /* Punt if there are (degenerate) PHIs in middle_bb, there should not be.  */
  if (!gimple_seq_empty_p (phi_nodes (middle_bb)))
    return 0;

  /* Only transform if it removes the condition.  */
  if (!single_non_singleton_phi_for_edges (phi_nodes (gimple_bb (phi)), e0, e1))
    return 0;

  /* Size-wise, this is always profitable.  */
  if (optimize_bb_for_speed_p (cond_bb)
      /* The special case is useless if it has a low probability.  */
      && profile_status_for_fn (cfun) != PROFILE_ABSENT
      && EDGE_PRED (middle_bb, 0)->probability < PROB_EVEN
      /* If assign is cheap, there is no point avoiding it.  */
      && estimate_num_insns (assign, &eni_time_weights)
	 >= 3 * estimate_num_insns (cond, &eni_time_weights))
    return 0;

  tree lhs = gimple_assign_lhs (assign);
  tree rhs1 = gimple_assign_rhs1 (assign);
  tree rhs2 = gimple_assign_rhs2 (assign);
  enum tree_code code_def = gimple_assign_rhs_code (assign);
  tree cond_lhs = gimple_cond_lhs (cond);
  tree cond_rhs = gimple_cond_rhs (cond);

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
	      && (operand_equal_for_phi_arg_p (rhs2, cond_lhs)
		  || operand_equal_for_phi_arg_p (rhs1, cond_lhs))
	      && absorbing_element_p (code_def, cond_rhs))))
    {
      gsi = gsi_for_stmt (cond);
      if (INTEGRAL_TYPE_P (TREE_TYPE (lhs)))
	{
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
	  SSA_NAME_RANGE_INFO (lhs) = NULL;
	  /* If available, we can use VR of phi result at least.  */
	  tree phires = gimple_phi_result (phi);
	  struct range_info_def *phires_range_info
	    = SSA_NAME_RANGE_INFO (phires);
	  if (phires_range_info)
	    duplicate_ssa_name_range_info (lhs, SSA_NAME_RANGE_TYPE (phires),
					   phires_range_info);
	}
      gimple_stmt_iterator gsi_from = gsi_for_stmt (assign);
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
		    edge e0, edge e1, gimple *phi,
		    tree arg0, tree arg1)
{
  tree result, type;
  gcond *cond;
  gassign *new_stmt;
  edge true_edge, false_edge;
  enum tree_code cmp, minmax, ass_code;
  tree smaller, alt_smaller, larger, alt_larger, arg_true, arg_false;
  gimple_stmt_iterator gsi, gsi_from;

  type = TREE_TYPE (PHI_RESULT (phi));

  /* The optimization may be unsafe due to NaNs.  */
  if (HONOR_NANS (type))
    return false;

  cond = as_a <gcond *> (last_stmt (cond_bb));
  cmp = gimple_cond_code (cond);

  /* This transformation is only valid for order comparisons.  Record which
     operand is smaller/larger if the result of the comparison is true.  */
  alt_smaller = NULL_TREE;
  alt_larger = NULL_TREE;
  if (cmp == LT_EXPR || cmp == LE_EXPR)
    {
      smaller = gimple_cond_lhs (cond);
      larger = gimple_cond_rhs (cond);
      /* If we have smaller < CST it is equivalent to smaller <= CST-1.
	 Likewise smaller <= CST is equivalent to smaller < CST+1.  */
      if (TREE_CODE (larger) == INTEGER_CST)
	{
	  if (cmp == LT_EXPR)
	    {
	      bool overflow;
	      wide_int alt = wi::sub (larger, 1, TYPE_SIGN (TREE_TYPE (larger)),
				      &overflow);
	      if (! overflow)
		alt_larger = wide_int_to_tree (TREE_TYPE (larger), alt);
	    }
	  else
	    {
	      bool overflow;
	      wide_int alt = wi::add (larger, 1, TYPE_SIGN (TREE_TYPE (larger)),
				      &overflow);
	      if (! overflow)
		alt_larger = wide_int_to_tree (TREE_TYPE (larger), alt);
	    }
	}
    }
  else if (cmp == GT_EXPR || cmp == GE_EXPR)
    {
      smaller = gimple_cond_rhs (cond);
      larger = gimple_cond_lhs (cond);
      /* If we have larger > CST it is equivalent to larger >= CST+1.
	 Likewise larger >= CST is equivalent to larger > CST-1.  */
      if (TREE_CODE (smaller) == INTEGER_CST)
	{
	  if (cmp == GT_EXPR)
	    {
	      bool overflow;
	      wide_int alt = wi::add (smaller, 1, TYPE_SIGN (TREE_TYPE (smaller)),
				      &overflow);
	      if (! overflow)
		alt_smaller = wide_int_to_tree (TREE_TYPE (smaller), alt);
	    }
	  else
	    {
	      bool overflow;
	      wide_int alt = wi::sub (smaller, 1, TYPE_SIGN (TREE_TYPE (smaller)),
				      &overflow);
	      if (! overflow)
		alt_smaller = wide_int_to_tree (TREE_TYPE (smaller), alt);
	    }
	}
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
      gsi_move_before (&gsi_from, &gsi);
    }

  /* Create an SSA var to hold the min/max result.  If we're the only
     things setting the target PHI, then we  can clone the PHI
     variable.  Otherwise we must create a new one.  */
  result = PHI_RESULT (phi);
  if (EDGE_COUNT (gimple_bb (phi)->preds) == 2)
    result = duplicate_ssa_name (result, NULL);
  else
    result = make_ssa_name (TREE_TYPE (result));

  /* Emit the statement to compute min/max.  */
  new_stmt = gimple_build_assign (result, minmax, arg0, arg1);
  gsi = gsi_last_bb (cond_bb);
  gsi_insert_before (&gsi, new_stmt, GSI_NEW_STMT);

  replace_phi_edge_with_variable (cond_bb, e1, phi, result);
  reset_flow_sensitive_info_in_bb (cond_bb);

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
		 gimple *phi, tree arg0, tree arg1)
{
  tree result;
  gassign *new_stmt;
  gimple *cond;
  gimple_stmt_iterator gsi;
  edge true_edge, false_edge;
  gimple *assign;
  edge e;
  tree rhs, lhs;
  bool negate;
  enum tree_code cond_code;

  /* If the type says honor signed zeros we cannot do this
     optimization.  */
  if (HONOR_SIGNED_ZEROS (arg1))
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
  if (gimple_code (assign) != GIMPLE_ASSIGN)
    return false;

  lhs = gimple_assign_lhs (assign);

  if (gimple_assign_rhs_code (assign) != NEGATE_EXPR)
    return false;

  rhs = gimple_assign_rhs1 (assign);

  /* The assignment has to be arg0 = -arg1 or arg1 = -arg0.  */
  if (!(lhs == arg0 && rhs == arg1)
      && !(lhs == arg1 && rhs == arg0))
    return false;

  cond = last_stmt (cond_bb);
  result = PHI_RESULT (phi);

  /* Only relationals comparing arg[01] against zero are interesting.  */
  cond_code = gimple_cond_code (cond);
  if (cond_code != GT_EXPR && cond_code != GE_EXPR
      && cond_code != LT_EXPR && cond_code != LE_EXPR)
    return false;

  /* Make sure the conditional is arg[01] OP y.  */
  if (gimple_cond_lhs (cond) != rhs)
    return false;

  if (FLOAT_TYPE_P (TREE_TYPE (gimple_cond_rhs (cond)))
	       ? real_zerop (gimple_cond_rhs (cond))
	       : integer_zerop (gimple_cond_rhs (cond)))
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

  /* If the code negates only iff positive then make sure to not
     introduce undefined behavior when negating or computing the absolute.
     ???  We could use range info if present to check for arg1 == INT_MIN.  */
  if (negate
      && (ANY_INTEGRAL_TYPE_P (TREE_TYPE (arg1))
	  && ! TYPE_OVERFLOW_WRAPS (TREE_TYPE (arg1))))
    return false;

  result = duplicate_ssa_name (result, NULL);

  if (negate)
    lhs = make_ssa_name (TREE_TYPE (result));
  else
    lhs = result;

  /* Build the modify expression with abs expression.  */
  new_stmt = gimple_build_assign (lhs, ABS_EXPR, rhs);

  gsi = gsi_last_bb (cond_bb);
  gsi_insert_before (&gsi, new_stmt, GSI_NEW_STMT);

  if (negate)
    {
      /* Get the right GSI.  We want to insert after the recently
	 added ABS_EXPR statement (which we know is the first statement
	 in the block.  */
      new_stmt = gimple_build_assign (result, NEGATE_EXPR, lhs);

      gsi_insert_after (&gsi, new_stmt, GSI_NEW_STMT);
    }

  replace_phi_edge_with_variable (cond_bb, e1, phi, result);
  reset_flow_sensitive_info_in_bb (cond_bb);

  /* Note that we optimized this PHI.  */
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
   overly conservative.  */

/* A hash-table of SSA_NAMEs, and in which basic block an MEM_REF
   through it was seen, which would constitute a no-trap region for
   same accesses.  */
struct name_to_bb
{
  unsigned int ssa_name_ver;
  unsigned int phase;
  bool store;
  HOST_WIDE_INT offset, size;
  basic_block bb;
};

/* Hashtable helpers.  */

struct ssa_names_hasher : free_ptr_hash <name_to_bb>
{
  static inline hashval_t hash (const name_to_bb *);
  static inline bool equal (const name_to_bb *, const name_to_bb *);
};

/* Used for quick clearing of the hash-table when we see calls.
   Hash entries with phase < nt_call_phase are invalid.  */
static unsigned int nt_call_phase;

/* The hash function.  */

inline hashval_t
ssa_names_hasher::hash (const name_to_bb *n)
{
  return n->ssa_name_ver ^ (((hashval_t) n->store) << 31)
         ^ (n->offset << 6) ^ (n->size << 3);
}

/* The equality function of *P1 and *P2.  */

inline bool
ssa_names_hasher::equal (const name_to_bb *n1, const name_to_bb *n2)
{
  return n1->ssa_name_ver == n2->ssa_name_ver
         && n1->store == n2->store
         && n1->offset == n2->offset
         && n1->size == n2->size;
}

class nontrapping_dom_walker : public dom_walker
{
public:
  nontrapping_dom_walker (cdi_direction direction, hash_set<tree> *ps)
    : dom_walker (direction), m_nontrapping (ps), m_seen_ssa_names (128) {}

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
  hash_table<ssa_names_hasher> m_seen_ssa_names;
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
   expression (an MEM_REF through an SSA_NAME) possibly insert the
   expression into the set NONTRAP or the hash table of seen expressions.
   STORE is true if this expression is on the LHS, otherwise it's on
   the RHS.  */
void
nontrapping_dom_walker::add_or_mark_expr (basic_block bb, tree exp, bool store)
{
  HOST_WIDE_INT size;

  if (TREE_CODE (exp) == MEM_REF
      && TREE_CODE (TREE_OPERAND (exp, 0)) == SSA_NAME
      && tree_fits_shwi_p (TREE_OPERAND (exp, 1))
      && (size = int_size_in_bytes (TREE_TYPE (exp))) > 0)
    {
      tree name = TREE_OPERAND (exp, 0);
      struct name_to_bb map;
      name_to_bb **slot;
      struct name_to_bb *n2bb;
      basic_block found_bb = 0;

      /* Try to find the last seen MEM_REF through the same
         SSA_NAME, which can trap.  */
      map.ssa_name_ver = SSA_NAME_VERSION (name);
      map.phase = 0;
      map.bb = 0;
      map.store = store;
      map.offset = tree_to_shwi (TREE_OPERAND (exp, 1));
      map.size = size;

      slot = m_seen_ssa_names.find_slot (&map, INSERT);
      n2bb = *slot;
      if (n2bb && n2bb->phase >= nt_call_phase)
        found_bb = n2bb->bb;

      /* If we've found a trapping MEM_REF, _and_ it dominates EXP
         (it's in a basic block on the path from us to the dominator root)
	 then we can't trap.  */
      if (found_bb && (((size_t)found_bb->aux) & 1) == 1)
	{
	  m_nontrapping->add (exp);
	}
      else
        {
	  /* EXP might trap, so insert it into the hash table.  */
	  if (n2bb)
	    {
	      n2bb->phase = nt_call_phase;
	      n2bb->bb = bb;
	    }
	  else
	    {
	      n2bb = XNEW (struct name_to_bb);
	      n2bb->ssa_name_ver = SSA_NAME_VERSION (name);
	      n2bb->phase = nt_call_phase;
	      n2bb->bb = bb;
	      n2bb->store = store;
	      n2bb->offset = map.offset;
	      n2bb->size = size;
	      *slot = n2bb;
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
  /* We're going to do a dominator walk, so ensure that we have
     dominance information.  */
  calculate_dominance_info (CDI_DOMINATORS);

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
   memory location dominates us) and that the store has a "simple" RHS.  */

static bool
cond_store_replacement (basic_block middle_bb, basic_block join_bb,
			edge e0, edge e1, hash_set<tree> *nontrap)
{
  gimple *assign = last_and_only_stmt (middle_bb);
  tree lhs, rhs, name, name2;
  gphi *newphi;
  gassign *new_stmt;
  gimple_stmt_iterator gsi;
  source_location locus;

  /* Check if middle_bb contains of only one store.  */
  if (!assign
      || !gimple_assign_single_p (assign)
      || gimple_has_volatile_ops (assign))
    return false;

  locus = gimple_location (assign);
  lhs = gimple_assign_lhs (assign);
  rhs = gimple_assign_rhs1 (assign);
  if (TREE_CODE (lhs) != MEM_REF
      || TREE_CODE (TREE_OPERAND (lhs, 0)) != SSA_NAME
      || !is_gimple_reg_type (TREE_TYPE (lhs)))
    return false;

  /* Prove that we can move the store down.  We could also check
     TREE_THIS_NOTRAP here, but in that case we also could move stores,
     whose value is not available readily, which we want to avoid.  */
  if (!nontrap->contains (lhs))
    return false;

  /* Now we've checked the constraints, so do the transformation:
     1) Remove the single store.  */
  gsi = gsi_for_stmt (assign);
  unlink_stmt_vdef (assign);
  gsi_remove (&gsi, true);
  release_defs (assign);

  /* 2) Insert a load from the memory of the store to the temporary
        on the edge which did not contain the store.  */
  lhs = unshare_expr (lhs);
  name = make_temp_ssa_name (TREE_TYPE (lhs), NULL, "cstore");
  new_stmt = gimple_build_assign (name, lhs);
  gimple_set_location (new_stmt, locus);
  gsi_insert_on_edge (e1, new_stmt);

  /* 3) Create a PHI node at the join block, with one argument
        holding the old RHS, and the other holding the temporary
        where we stored the old memory contents.  */
  name2 = make_temp_ssa_name (TREE_TYPE (lhs), NULL, "cstore");
  newphi = create_phi_node (name2, join_bb);
  add_phi_arg (newphi, rhs, e0, locus);
  add_phi_arg (newphi, name, e1, locus);

  lhs = unshare_expr (lhs);
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

  return true;
}

/* Do the main work of conditional store replacement.  */

static bool
cond_if_else_store_replacement_1 (basic_block then_bb, basic_block else_bb,
				  basic_block join_bb, gimple *then_assign,
				  gimple *else_assign)
{
  tree lhs_base, lhs, then_rhs, else_rhs, name;
  source_location then_locus, else_locus;
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

  return true;
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
  gimple *then_assign = last_and_only_stmt (then_bb);
  gimple *else_assign = last_and_only_stmt (else_bb);
  vec<data_reference_p> then_datarefs, else_datarefs;
  vec<ddr_p> then_ddrs, else_ddrs;
  gimple *then_store, *else_store;
  bool found, ok = false, res;
  struct data_dependence_relation *ddr;
  data_reference_p then_dr, else_dr;
  int i, j;
  tree then_lhs, else_lhs;
  basic_block blocks[3];

  if (MAX_STORES_TO_SINK == 0)
    return false;

  /* Handle the case with single statement in THEN_BB and ELSE_BB.  */
  if (then_assign && else_assign)
    return cond_if_else_store_replacement_1 (then_bb, else_bb, join_bb,
                                             then_assign, else_assign);

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
      || then_stores.length () > (unsigned) MAX_STORES_TO_SINK)
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
  int param_align = PARAM_VALUE (PARAM_L1_CACHE_LINE_SIZE);
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
	  && PARAM_VALUE (PARAM_L1_CACHE_LINE_SIZE)
	  && HAVE_conditional_move);
}

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
    : gimple_opt_pass (pass_data_phiopt, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_phiopt (m_ctxt); }
  virtual bool gate (function *) { return flag_ssa_phiopt; }
  virtual unsigned int execute (function *)
    {
      return tree_ssa_phiopt_worker (false, gate_hoist_loads ());
    }

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
