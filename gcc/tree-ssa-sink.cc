/* Code sinking for trees
   Copyright (C) 2001-2023 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dan@dberlin.org>

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
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "cfganal.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "cfgloop.h"
#include "tree-eh.h"

/* TODO:
   1. Sinking store only using scalar promotion (IE without moving the RHS):

   *q = p;
   p = p + 1;
   if (something)
     *q = <not p>;
   else
     y = *q;


   should become
   sinktemp = p;
   p = p + 1;
   if (something)
     *q = <not p>;
   else
   {
     *q = sinktemp;
     y = *q
   }
   Store copy propagation will take care of the store elimination above.


   2. Sinking using Partial Dead Code Elimination.  */


static struct
{
  /* The number of statements sunk down the flowgraph by code sinking.  */
  int sunk;

  /* The number of stores commoned and sunk down by store commoning.  */
  int commoned;
} sink_stats;


/* Given a PHI, and one of its arguments (DEF), find the edge for
   that argument and return it.  If the argument occurs twice in the PHI node,
   we return NULL.  */

static basic_block
find_bb_for_arg (gphi *phi, tree def)
{
  size_t i;
  bool foundone = false;
  basic_block result = NULL;
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    if (PHI_ARG_DEF (phi, i) == def)
      {
	if (foundone)
	  return NULL;
	foundone = true;
	result = gimple_phi_arg_edge (phi, i)->src;
      }
  return result;
}

/* When the first immediate use is in a statement, then return true if all
   immediate uses in IMM are in the same statement.
   We could also do the case where  the first immediate use is in a phi node,
   and all the other uses are in phis in the same basic block, but this
   requires some expensive checking later (you have to make sure no def/vdef
   in the statement occurs for multiple edges in the various phi nodes it's
   used in, so that you only have one place you can sink it to.  */

static bool
all_immediate_uses_same_place (def_operand_p def_p)
{
  tree var = DEF_FROM_PTR (def_p);
  imm_use_iterator imm_iter;
  use_operand_p use_p;

  gimple *firstuse = NULL;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, var)
    {
      if (is_gimple_debug (USE_STMT (use_p)))
	continue;
      if (firstuse == NULL)
	firstuse = USE_STMT (use_p);
      else
	if (firstuse != USE_STMT (use_p))
	  return false;
    }

  return true;
}

/* Find the nearest common dominator of all of the immediate uses in IMM.  */

static basic_block
nearest_common_dominator_of_uses (def_operand_p def_p, bool *debug_stmts)
{
  tree var = DEF_FROM_PTR (def_p);
  auto_bitmap blocks;
  basic_block commondom;
  unsigned int j;
  bitmap_iterator bi;
  imm_use_iterator imm_iter;
  use_operand_p use_p;

  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, var)
    {
      gimple *usestmt = USE_STMT (use_p);
      basic_block useblock;

      if (gphi *phi = dyn_cast <gphi *> (usestmt))
	{
	  int idx = PHI_ARG_INDEX_FROM_USE (use_p);

	  useblock = gimple_phi_arg_edge (phi, idx)->src;
	}
      else if (is_gimple_debug (usestmt))
	{
	  *debug_stmts = true;
	  continue;
	}
      else
	{
	  useblock = gimple_bb (usestmt);
	}

      /* Short circuit. Nothing dominates the entry block.  */
      if (useblock == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	return NULL;

      bitmap_set_bit (blocks, useblock->index);
    }
  commondom = BASIC_BLOCK_FOR_FN (cfun, bitmap_first_set_bit (blocks));
  EXECUTE_IF_SET_IN_BITMAP (blocks, 0, j, bi)
    commondom = nearest_common_dominator (CDI_DOMINATORS, commondom,
					  BASIC_BLOCK_FOR_FN (cfun, j));
  return commondom;
}

/* Given EARLY_BB and LATE_BB, two blocks in a path through the dominator
   tree, return the best basic block between them (inclusive) to place
   statements.

   We want the most control dependent block in the shallowest loop nest.

   If the resulting block is in a shallower loop nest, then use it.  Else
   only use the resulting block if it has significantly lower execution
   frequency than EARLY_BB to avoid gratuitous statement movement.  We
   consider statements with VOPS more desirable to move.

   This pass would obviously benefit from PDO as it utilizes block
   frequencies.  It would also benefit from recomputing frequencies
   if profile data is not available since frequencies often get out
   of sync with reality.  */

static basic_block
select_best_block (basic_block early_bb,
		   basic_block late_bb,
		   gimple *stmt)
{
  basic_block best_bb = late_bb;
  basic_block temp_bb = late_bb;
  int threshold;

  while (temp_bb != early_bb)
    {
      /* If we've moved into a lower loop nest, then that becomes
	 our best block.  */
      if (bb_loop_depth (temp_bb) < bb_loop_depth (best_bb))
	best_bb = temp_bb;

      /* Walk up the dominator tree, hopefully we'll find a shallower
 	 loop nest.  */
      temp_bb = get_immediate_dominator (CDI_DOMINATORS, temp_bb);
    }

  /* Placing a statement before a setjmp-like function would be invalid
     (it cannot be reevaluated when execution follows an abnormal edge).
     If we selected a block with abnormal predecessors, just punt.  */
  if (bb_has_abnormal_pred (best_bb))
    return early_bb;

  /* If we found a shallower loop nest, then we always consider that
     a win.  This will always give us the most control dependent block
     within that loop nest.  */
  if (bb_loop_depth (best_bb) < bb_loop_depth (early_bb))
    return best_bb;

  /* Get the sinking threshold.  If the statement to be moved has memory
     operands, then increase the threshold by 7% as those are even more
     profitable to avoid, clamping at 100%.  */
  threshold = param_sink_frequency_threshold;
  if (gimple_vuse (stmt) || gimple_vdef (stmt))
    {
      threshold += 7;
      if (threshold > 100)
	threshold = 100;
    }

  /* If BEST_BB is at the same nesting level, then require it to have
     significantly lower execution frequency to avoid gratuitous movement.  */
  if (bb_loop_depth (best_bb) == bb_loop_depth (early_bb)
      /* If result of comparsion is unknown, prefer EARLY_BB.
	 Thus use !(...>=..) rather than (...<...)  */
      && !(best_bb->count * 100 >= early_bb->count * threshold))
    return best_bb;

  /* No better block found, so return EARLY_BB, which happens to be the
     statement's original block.  */
  return early_bb;
}

/* Given a statement (STMT) and the basic block it is currently in (FROMBB),
   determine the location to sink the statement to, if any.
   Returns true if there is such location; in that case, TOGSI points to the
   statement before that STMT should be moved.  */

static bool
statement_sink_location (gimple *stmt, basic_block frombb,
			 gimple_stmt_iterator *togsi, bool *zero_uses_p)
{
  gimple *use;
  use_operand_p one_use = NULL_USE_OPERAND_P;
  basic_block sinkbb;
  use_operand_p use_p;
  def_operand_p def_p;
  ssa_op_iter iter;
  imm_use_iterator imm_iter;

  *zero_uses_p = false;

  /* We only can sink assignments and const/pure calls that are guaranteed
     to return exactly once.  */
  int cf;
  if (!is_gimple_assign (stmt)
      && (!is_gimple_call (stmt)
	  || !((cf = gimple_call_flags (stmt)) & (ECF_CONST|ECF_PURE))
	  || (cf & (ECF_LOOPING_CONST_OR_PURE|ECF_RETURNS_TWICE))))
    return false;

  /* We only can sink stmts with a single definition.  */
  def_p = single_ssa_def_operand (stmt, SSA_OP_ALL_DEFS);
  if (def_p == NULL_DEF_OPERAND_P)
    return false;

  /* There are a few classes of things we can't or don't move, some because we
     don't have code to handle it, some because it's not profitable and some
     because it's not legal.

     We can't sink things that may be global stores, at least not without
     calculating a lot more information, because we may cause it to no longer
     be seen by an external routine that needs it depending on where it gets
     moved to.

     We can't sink statements that end basic blocks without splitting the
     incoming edge for the sink location to place it there.

     We can't sink statements that have volatile operands.

     We don't want to sink dead code, so anything with 0 immediate uses is not
     sunk.

     Don't sink BLKmode assignments if current function has any local explicit
     register variables, as BLKmode assignments may involve memcpy or memset
     calls or, on some targets, inline expansion thereof that sometimes need
     to use specific hard registers.

  */
  if (stmt_ends_bb_p (stmt)
      || gimple_has_side_effects (stmt)
      || (cfun->has_local_explicit_reg_vars
	  && TYPE_MODE (TREE_TYPE (gimple_get_lhs (stmt))) == BLKmode))
    return false;

  /* Return if there are no immediate uses of this stmt.  */
  if (has_zero_uses (DEF_FROM_PTR (def_p)))
    {
      *zero_uses_p = true;
      return false;
    }

  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (DEF_FROM_PTR (def_p)))
    return false;

  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
    {
      tree use = USE_FROM_PTR (use_p);
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (use))
	return false;
    }

  use = NULL;

  /* If stmt is a store the one and only use needs to be the VOP
     merging PHI node.  */
  if (virtual_operand_p (DEF_FROM_PTR (def_p)))
    {
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, DEF_FROM_PTR (def_p))
	{
	  gimple *use_stmt = USE_STMT (use_p);

	  /* A killing definition is not a use.  */
	  if ((gimple_has_lhs (use_stmt)
	       && operand_equal_p (gimple_get_lhs (stmt),
				   gimple_get_lhs (use_stmt), 0))
	      || stmt_kills_ref_p (use_stmt, gimple_get_lhs (stmt)))
	    {
	      /* If use_stmt is or might be a nop assignment then USE_STMT
	         acts as a use as well as definition.  */
	      if (stmt != use_stmt
		  && ref_maybe_used_by_stmt_p (use_stmt,
					       gimple_get_lhs (stmt)))
		return false;
	      continue;
	    }

	  if (gimple_code (use_stmt) != GIMPLE_PHI)
	    return false;

	  if (use
	      && use != use_stmt)
	    return false;

	  use = use_stmt;
	}
      if (!use)
	return false;
    }
  /* If all the immediate uses are not in the same place, find the nearest
     common dominator of all the immediate uses.  For PHI nodes, we have to
     find the nearest common dominator of all of the predecessor blocks, since
     that is where insertion would have to take place.  */
  else if (gimple_vuse (stmt)
	   || !all_immediate_uses_same_place (def_p))
    {
      bool debug_stmts = false;
      basic_block commondom = nearest_common_dominator_of_uses (def_p,
								&debug_stmts);

      if (commondom == frombb)
	return false;

      /* If this is a load then do not sink past any stores.
	 Look for virtual definitions in the path from frombb to the sink
	 location computed from the real uses and if found, adjust
	 that it a common dominator.  */
      if (gimple_vuse (stmt))
	{
	  /* Do not sink loads from hard registers.  */
	  if (gimple_assign_single_p (stmt)
	      && TREE_CODE (gimple_assign_rhs1 (stmt)) == VAR_DECL
	      && DECL_HARD_REGISTER (gimple_assign_rhs1 (stmt)))
	    return false;

	  imm_use_iterator imm_iter;
	  use_operand_p use_p;
	  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, gimple_vuse (stmt))
	    {
	      gimple *use_stmt = USE_STMT (use_p);
	      basic_block bb = gimple_bb (use_stmt);
	      /* For PHI nodes the block we know sth about is the incoming block
		 with the use.  */
	      if (gimple_code (use_stmt) == GIMPLE_PHI)
		{
		  /* If the PHI defines the virtual operand, ignore it.  */
		  if (gimple_phi_result (use_stmt) == gimple_vuse (stmt))
		    continue;
		  /* In case the PHI node post-dominates the current insert
		     location we can disregard it.  But make sure it is not
		     dominating it as well as can happen in a CFG cycle.  */
		  if (commondom != bb
		      && !dominated_by_p (CDI_DOMINATORS, commondom, bb)
		      && dominated_by_p (CDI_POST_DOMINATORS, commondom, bb)
		      /* If the blocks are possibly within the same irreducible
			 cycle the above check breaks down.  */
		      && !((bb->flags & commondom->flags & BB_IRREDUCIBLE_LOOP)
			   && bb->loop_father == commondom->loop_father)
		      && !((commondom->flags & BB_IRREDUCIBLE_LOOP)
			   && flow_loop_nested_p (commondom->loop_father,
						  bb->loop_father))
		      && !((bb->flags & BB_IRREDUCIBLE_LOOP)
			   && flow_loop_nested_p (bb->loop_father,
						  commondom->loop_father)))
		    continue;
		  bb = EDGE_PRED (bb, PHI_ARG_INDEX_FROM_USE (use_p))->src;
		}
	      else if (!gimple_vdef (use_stmt))
		continue;
	      /* If the use is not dominated by the path entry it is not on
		 the path.  */
	      if (!dominated_by_p (CDI_DOMINATORS, bb, frombb))
		continue;
	      /* There is no easy way to disregard defs not on the path from
		 frombb to commondom so just consider them all.  */
	      commondom = nearest_common_dominator (CDI_DOMINATORS,
						    bb, commondom);
	      if (commondom == frombb)
		return false;
	    }
	}

      /* Our common dominator has to be dominated by frombb in order to be a
	 trivially safe place to put this statement, since it has multiple
	 uses.  */
      if (!dominated_by_p (CDI_DOMINATORS, commondom, frombb))
	return false;

      commondom = select_best_block (frombb, commondom, stmt);

      if (commondom == frombb)
	return false;	

      *togsi = gsi_after_labels (commondom);

      return true;
    }
  else
    {
      FOR_EACH_IMM_USE_FAST (one_use, imm_iter, DEF_FROM_PTR (def_p))
	{
	  if (is_gimple_debug (USE_STMT (one_use)))
	    continue;
	  break;
	}
      use = USE_STMT (one_use);

      if (gimple_code (use) != GIMPLE_PHI)
	{
	  sinkbb = select_best_block (frombb, gimple_bb (use), stmt);

	  if (sinkbb == frombb)
	    return false;

	  if (sinkbb == gimple_bb (use))
	    *togsi = gsi_for_stmt (use);
	  else
	    *togsi = gsi_after_labels (sinkbb);

	  return true;
	}
    }

  sinkbb = find_bb_for_arg (as_a <gphi *> (use), DEF_FROM_PTR (def_p));

  /* This can happen if there are multiple uses in a PHI.  */
  if (!sinkbb)
    return false;
  
  sinkbb = select_best_block (frombb, sinkbb, stmt);
  if (!sinkbb || sinkbb == frombb)
    return false;

  /* If the latch block is empty, don't make it non-empty by sinking
     something into it.  */
  if (sinkbb == frombb->loop_father->latch
      && empty_block_p (sinkbb))
    return false;

  *togsi = gsi_after_labels (sinkbb);

  return true;
}

/* Very simplistic code to sink common stores from the predecessor through
   our virtual PHI.  We do this before sinking stmts from BB as it might
   expose sinking opportunities of the merged stores.
   Once we have partial dead code elimination through sth like SSU-PRE this
   should be moved there.  */

static unsigned
sink_common_stores_to_bb (basic_block bb)
{
  unsigned todo = 0;
  gphi *phi;

  if (EDGE_COUNT (bb->preds) > 1
      && (phi = get_virtual_phi (bb)))
    {
      /* Repeat until no more common stores are found.  */
      while (1)
	{
	  gimple *first_store = NULL;
	  auto_vec <tree, 5> vdefs;
	  gimple_stmt_iterator gsi;

	  /* Search for common stores defined by all virtual PHI args.
	     ???  Common stores not present in all predecessors could
	     be handled by inserting a forwarder to sink to.  Generally
	     this involves deciding which stores to do this for if
	     multiple common stores are present for different sets of
	     predecessors.  See PR11832 for an interesting case.  */
	  for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
	    {
	      tree arg = gimple_phi_arg_def (phi, i);
	      gimple *def = SSA_NAME_DEF_STMT (arg);
	      if (! is_gimple_assign (def)
		  || stmt_can_throw_internal (cfun, def)
		  || (gimple_phi_arg_edge (phi, i)->flags & EDGE_ABNORMAL))
		{
		  /* ???  We could handle some cascading with the def being
		     another PHI.  We'd have to insert multiple PHIs for
		     the rhs then though (if they are not all equal).  */
		  first_store = NULL;
		  break;
		}
	      /* ???  Do not try to do anything fancy with aliasing, thus
		 do not sink across non-aliased loads (or even stores,
		 so different store order will make the sinking fail).  */
	      bool all_uses_on_phi = true;
	      imm_use_iterator iter;
	      use_operand_p use_p;
	      FOR_EACH_IMM_USE_FAST (use_p, iter, arg)
		if (USE_STMT (use_p) != phi)
		  {
		    all_uses_on_phi = false;
		    break;
		  }
	      if (! all_uses_on_phi)
		{
		  first_store = NULL;
		  break;
		}
	      /* Check all stores are to the same LHS.  */
	      if (! first_store)
		first_store = def;
	      /* ??? We could handle differing SSA uses in the LHS by inserting
		 PHIs for them.  */
	      else if (! operand_equal_p (gimple_assign_lhs (first_store),
					  gimple_assign_lhs (def), 0)
		       || (gimple_clobber_p (first_store)
			   != gimple_clobber_p (def)))
		{
		  first_store = NULL;
		  break;
		}
	      vdefs.safe_push (arg);
	    }
	  if (! first_store)
	    break;

	  /* Check if we need a PHI node to merge the stored values.  */
	  bool allsame = true;
	  if (!gimple_clobber_p (first_store))
	    for (unsigned i = 1; i < vdefs.length (); ++i)
	      {
		gimple *def = SSA_NAME_DEF_STMT (vdefs[i]);
		if (! operand_equal_p (gimple_assign_rhs1 (first_store),
				       gimple_assign_rhs1 (def), 0))
		  {
		    allsame = false;
		    break;
		  }
	      }

	  /* We cannot handle aggregate values if we need to merge them.  */
	  tree type = TREE_TYPE (gimple_assign_lhs (first_store));
	  if (! allsame
	      && ! is_gimple_reg_type (type))
	    break;

	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS,
			       first_store,
			       "sinking common stores %sto ",
			       allsame ? "with same value " : "");
	      dump_generic_expr (MSG_OPTIMIZED_LOCATIONS, TDF_SLIM,
				 gimple_assign_lhs (first_store));
	      dump_printf (MSG_OPTIMIZED_LOCATIONS, "\n");
	    }

	  /* Insert a PHI to merge differing stored values if necessary.
	     Note that in general inserting PHIs isn't a very good idea as
	     it makes the job of coalescing and register allocation harder.
	     Even common SSA uses on the rhs/lhs might extend their lifetime
	     across multiple edges by this code motion which makes
	     register allocation harder.  */
	  tree from;
	  if (! allsame)
	    {
	      from = make_ssa_name (type);
	      gphi *newphi = create_phi_node (from, bb);
	      for (unsigned i = 0; i < vdefs.length (); ++i)
		{
		  gimple *def = SSA_NAME_DEF_STMT (vdefs[i]);
		  add_phi_arg (newphi, gimple_assign_rhs1 (def),
			       EDGE_PRED (bb, i), UNKNOWN_LOCATION);
		}
	    }
	  else
	    from = gimple_assign_rhs1 (first_store);

	  /* Remove all stores.  */
	  for (unsigned i = 0; i < vdefs.length (); ++i)
	    TREE_VISITED (vdefs[i]) = 1;
	  for (unsigned i = 0; i < vdefs.length (); ++i)
	    /* If we have more than one use of a VDEF on the PHI make sure
	       we remove the defining stmt only once.  */
	    if (TREE_VISITED (vdefs[i]))
	      {
		TREE_VISITED (vdefs[i]) = 0;
		gimple *def = SSA_NAME_DEF_STMT (vdefs[i]);
		gsi = gsi_for_stmt (def);
		unlink_stmt_vdef (def);
		gsi_remove (&gsi, true);
		release_defs (def);
	      }

	  /* Insert the first store at the beginning of the merge BB.  */
	  gimple_set_vdef (first_store, gimple_phi_result (phi));
	  SSA_NAME_DEF_STMT (gimple_vdef (first_store)) = first_store;
	  gimple_phi_set_result (phi, make_ssa_name (gimple_vop (cfun)));
	  gimple_set_vuse (first_store, gimple_phi_result (phi));
	  gimple_assign_set_rhs1 (first_store, from);
	  /* ???  Should we reset first_stores location?  */
	  gsi = gsi_after_labels (bb);
	  gsi_insert_before (&gsi, first_store, GSI_SAME_STMT);
	  sink_stats.commoned++;

	  todo |= TODO_cleanup_cfg;
	}

      /* We could now have empty predecessors that we could remove,
	 forming a proper CFG for further sinking.  Note that even
	 CFG cleanup doesn't do this fully at the moment and it
	 doesn't preserve post-dominators in the process either.
	 The mergephi pass might do it though.  gcc.dg/tree-ssa/ssa-sink-13.c
	 shows this nicely if you disable tail merging or (same effect)
	 make the stored values unequal.  */
    }

  return todo;
}

/* Perform code sinking on BB */

static unsigned
sink_code_in_bb (basic_block bb)
{
  basic_block son;
  gimple_stmt_iterator gsi;
  edge_iterator ei;
  edge e;
  bool last = true;
  unsigned todo = 0;

  /* Sink common stores from the predecessor through our virtual PHI.  */
  todo |= sink_common_stores_to_bb (bb);

  /* If this block doesn't dominate anything, there can't be any place to sink
     the statements to.  */
  if (first_dom_son (CDI_DOMINATORS, bb) == NULL)
    goto earlyout;

  /* We can't move things across abnormal edges, so don't try.  */
  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->flags & EDGE_ABNORMAL)
      goto earlyout;

  for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi);)
    {
      gimple *stmt = gsi_stmt (gsi);
      gimple_stmt_iterator togsi;
      bool zero_uses_p;

      if (!statement_sink_location (stmt, bb, &togsi, &zero_uses_p))
	{
	  gimple_stmt_iterator saved = gsi;
	  if (!gsi_end_p (gsi))
	    gsi_prev (&gsi);
	  /* If we face a dead stmt remove it as it possibly blocks
	     sinking of uses.  */
	  if (zero_uses_p
	      && !gimple_vdef (stmt)
	      && (cfun->can_delete_dead_exceptions
		  || !stmt_could_throw_p (cfun, stmt)))
	    {
	      gsi_remove (&saved, true);
	      release_defs (stmt);
	    }
	  else
	    last = false;
	  continue;
	}
      if (dump_file)
	{
	  fprintf (dump_file, "Sinking ");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS);
	  fprintf (dump_file, " from bb %d to bb %d\n",
		   bb->index, (gsi_bb (togsi))->index);
	}

      /* Update virtual operands of statements in the path we
         do not sink to.  */
      if (gimple_vdef (stmt))
	{
	  imm_use_iterator iter;
	  use_operand_p use_p;
	  gimple *vuse_stmt;

	  FOR_EACH_IMM_USE_STMT (vuse_stmt, iter, gimple_vdef (stmt))
	    if (gimple_code (vuse_stmt) != GIMPLE_PHI)
	      FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
		SET_USE (use_p, gimple_vuse (stmt));
	}

      /* If this is the end of the basic block, we need to insert at the end
         of the basic block.  */
      if (gsi_end_p (togsi))
	gsi_move_to_bb_end (&gsi, gsi_bb (togsi));
      else
	gsi_move_before (&gsi, &togsi);

      sink_stats.sunk++;

      /* If we've just removed the last statement of the BB, the
	 gsi_end_p() test below would fail, but gsi_prev() would have
	 succeeded, and we want it to succeed.  So we keep track of
	 whether we're at the last statement and pick up the new last
	 statement.  */
      if (last)
	{
	  gsi = gsi_last_bb (bb);
	  continue;
	}

      last = false;
      if (!gsi_end_p (gsi))
	gsi_prev (&gsi);

    }
 earlyout:
  for (son = first_dom_son (CDI_POST_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_POST_DOMINATORS, son))
    {
      todo |= sink_code_in_bb (son);
    }

  return todo;
}

/* Perform code sinking.
   This moves code down the flowgraph when we know it would be
   profitable to do so, or it wouldn't increase the number of
   executions of the statement.

   IE given

   a_1 = b + c;
   if (<something>)
   {
   }
   else
   {
     foo (&b, &c);
     a_5 = b + c;
   }
   a_6 = PHI (a_5, a_1);
   USE a_6.

   we'll transform this into:

   if (<something>)
   {
      a_1 = b + c;
   }
   else
   {
      foo (&b, &c);
      a_5 = b + c;
   }
   a_6 = PHI (a_5, a_1);
   USE a_6.

   Note that this reduces the number of computations of a = b + c to 1
   when we take the else edge, instead of 2.
*/
namespace {

const pass_data pass_data_sink_code =
{
  GIMPLE_PASS, /* type */
  "sink", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_SINK, /* tv_id */
  /* PROP_no_crit_edges is ensured by running split_edges_for_insertion in
     pass_data_sink_code::execute ().  */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_sink_code : public gimple_opt_pass
{
public:
  pass_sink_code (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_sink_code, ctxt), unsplit_edges (false)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return flag_tree_sink != 0; }
  unsigned int execute (function *) final override;
  opt_pass *clone (void) final override { return new pass_sink_code (m_ctxt); }
  void set_pass_param (unsigned n, bool param) final override
    {
      gcc_assert (n == 0);
      unsplit_edges = param;
    }

private:
  bool unsplit_edges;
}; // class pass_sink_code

unsigned int
pass_sink_code::execute (function *fun)
{
  loop_optimizer_init (LOOPS_NORMAL);
  split_edges_for_insertion ();
  /* Arrange for the critical edge splitting to be undone if requested.  */
  unsigned todo = unsplit_edges ? TODO_cleanup_cfg : 0;
  connect_infinite_loops_to_exit ();
  memset (&sink_stats, 0, sizeof (sink_stats));
  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);
  todo |= sink_code_in_bb (EXIT_BLOCK_PTR_FOR_FN (fun));
  statistics_counter_event (fun, "Sunk statements", sink_stats.sunk);
  statistics_counter_event (fun, "Commoned stores", sink_stats.commoned);
  free_dominance_info (CDI_POST_DOMINATORS);
  remove_fake_exit_edges ();
  loop_optimizer_finalize ();

  return todo;
}

} // anon namespace

gimple_opt_pass *
make_pass_sink_code (gcc::context *ctxt)
{
  return new pass_sink_code (ctxt);
}
