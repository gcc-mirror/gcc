/* Vectorizer Specific Loop Manipulations
   Copyright (C) 2003-2017 Free Software Foundation, Inc.
   Contributed by Dorit Naishlos <dorit@il.ibm.com>
   and Ira Rosen <irar@il.ibm.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

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
#include "fold-const.h"
#include "cfganal.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-into-ssa.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-vectorizer.h"
#include "tree-ssa-loop-ivopts.h"

/*************************************************************************
  Simple Loop Peeling Utilities

  Utilities to support loop peeling for vectorization purposes.
 *************************************************************************/


/* Renames the use *OP_P.  */

static void
rename_use_op (use_operand_p op_p)
{
  tree new_name;

  if (TREE_CODE (USE_FROM_PTR (op_p)) != SSA_NAME)
    return;

  new_name = get_current_def (USE_FROM_PTR (op_p));

  /* Something defined outside of the loop.  */
  if (!new_name)
    return;

  /* An ordinary ssa name defined in the loop.  */

  SET_USE (op_p, new_name);
}


/* Renames the variables in basic block BB.  Allow renaming  of PHI arguments
   on edges incoming from outer-block header if RENAME_FROM_OUTER_LOOP is
   true.  */

static void
rename_variables_in_bb (basic_block bb, bool rename_from_outer_loop)
{
  gimple *stmt;
  use_operand_p use_p;
  ssa_op_iter iter;
  edge e;
  edge_iterator ei;
  struct loop *loop = bb->loop_father;
  struct loop *outer_loop = NULL;

  if (rename_from_outer_loop)
    {
      gcc_assert (loop);
      outer_loop = loop_outer (loop);
    }

  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      stmt = gsi_stmt (gsi);
      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
	rename_use_op (use_p);
    }

  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      if (!flow_bb_inside_loop_p (loop, e->src))
	{
	  if (!rename_from_outer_loop)
	    continue;
	  if (e->src != outer_loop->header)
	    {
	      if (outer_loop->inner->next)
		{
		  /* If outer_loop has 2 inner loops, allow there to
		     be an extra basic block which decides which of the
		     two loops to use using LOOP_VECTORIZED.  */
		  if (!single_pred_p (e->src)
		      || single_pred (e->src) != outer_loop->header)
		    continue;
		}
	      else
		continue;
	    }
	}
      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
        rename_use_op (PHI_ARG_DEF_PTR_FROM_EDGE (gsi.phi (), e));
    }
}


struct adjust_info
{
  tree from, to;
  basic_block bb;
};

/* A stack of values to be adjusted in debug stmts.  We have to
   process them LIFO, so that the closest substitution applies.  If we
   processed them FIFO, without the stack, we might substitute uses
   with a PHI DEF that would soon become non-dominant, and when we got
   to the suitable one, it wouldn't have anything to substitute any
   more.  */
static vec<adjust_info, va_heap> adjust_vec;

/* Adjust any debug stmts that referenced AI->from values to use the
   loop-closed AI->to, if the references are dominated by AI->bb and
   not by the definition of AI->from.  */

static void
adjust_debug_stmts_now (adjust_info *ai)
{
  basic_block bbphi = ai->bb;
  tree orig_def = ai->from;
  tree new_def = ai->to;
  imm_use_iterator imm_iter;
  gimple *stmt;
  basic_block bbdef = gimple_bb (SSA_NAME_DEF_STMT (orig_def));

  gcc_assert (dom_info_available_p (CDI_DOMINATORS));

  /* Adjust any debug stmts that held onto non-loop-closed
     references.  */
  FOR_EACH_IMM_USE_STMT (stmt, imm_iter, orig_def)
    {
      use_operand_p use_p;
      basic_block bbuse;

      if (!is_gimple_debug (stmt))
	continue;

      gcc_assert (gimple_debug_bind_p (stmt));

      bbuse = gimple_bb (stmt);

      if ((bbuse == bbphi
	   || dominated_by_p (CDI_DOMINATORS, bbuse, bbphi))
	  && !(bbuse == bbdef
	       || dominated_by_p (CDI_DOMINATORS, bbuse, bbdef)))
	{
	  if (new_def)
	    FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
	      SET_USE (use_p, new_def);
	  else
	    {
	      gimple_debug_bind_reset_value (stmt);
	      update_stmt (stmt);
	    }
	}
    }
}

/* Adjust debug stmts as scheduled before.  */

static void
adjust_vec_debug_stmts (void)
{
  if (!MAY_HAVE_DEBUG_STMTS)
    return;

  gcc_assert (adjust_vec.exists ());

  while (!adjust_vec.is_empty ())
    {
      adjust_debug_stmts_now (&adjust_vec.last ());
      adjust_vec.pop ();
    }
}

/* Adjust any debug stmts that referenced FROM values to use the
   loop-closed TO, if the references are dominated by BB and not by
   the definition of FROM.  If adjust_vec is non-NULL, adjustments
   will be postponed until adjust_vec_debug_stmts is called.  */

static void
adjust_debug_stmts (tree from, tree to, basic_block bb)
{
  adjust_info ai;

  if (MAY_HAVE_DEBUG_STMTS
      && TREE_CODE (from) == SSA_NAME
      && ! SSA_NAME_IS_DEFAULT_DEF (from)
      && ! virtual_operand_p (from))
    {
      ai.from = from;
      ai.to = to;
      ai.bb = bb;

      if (adjust_vec.exists ())
	adjust_vec.safe_push (ai);
      else
	adjust_debug_stmts_now (&ai);
    }
}

/* Change E's phi arg in UPDATE_PHI to NEW_DEF, and record information
   to adjust any debug stmts that referenced the old phi arg,
   presumably non-loop-closed references left over from other
   transformations.  */

static void
adjust_phi_and_debug_stmts (gimple *update_phi, edge e, tree new_def)
{
  tree orig_def = PHI_ARG_DEF_FROM_EDGE (update_phi, e);

  SET_PHI_ARG_DEF (update_phi, e->dest_idx, new_def);

  if (MAY_HAVE_DEBUG_STMTS)
    adjust_debug_stmts (orig_def, PHI_RESULT (update_phi),
			gimple_bb (update_phi));
}

/* Make the LOOP iterate NITERS times. This is done by adding a new IV
   that starts at zero, increases by one and its limit is NITERS.

   Assumption: the exit-condition of LOOP is the last stmt in the loop.  */

void
slpeel_make_loop_iterate_ntimes (struct loop *loop, tree niters)
{
  tree indx_before_incr, indx_after_incr;
  gcond *cond_stmt;
  gcond *orig_cond;
  edge exit_edge = single_exit (loop);
  gimple_stmt_iterator loop_cond_gsi;
  gimple_stmt_iterator incr_gsi;
  bool insert_after;
  tree init = build_int_cst (TREE_TYPE (niters), 0);
  tree step = build_int_cst (TREE_TYPE (niters), 1);
  source_location loop_loc;
  enum tree_code code;

  orig_cond = get_loop_exit_condition (loop);
  gcc_assert (orig_cond);
  loop_cond_gsi = gsi_for_stmt (orig_cond);

  standard_iv_increment_position (loop, &incr_gsi, &insert_after);
  create_iv (init, step, NULL_TREE, loop,
             &incr_gsi, insert_after, &indx_before_incr, &indx_after_incr);

  indx_after_incr = force_gimple_operand_gsi (&loop_cond_gsi, indx_after_incr,
					      true, NULL_TREE, true,
					      GSI_SAME_STMT);
  niters = force_gimple_operand_gsi (&loop_cond_gsi, niters, true, NULL_TREE,
				     true, GSI_SAME_STMT);

  code = (exit_edge->flags & EDGE_TRUE_VALUE) ? GE_EXPR : LT_EXPR;
  cond_stmt = gimple_build_cond (code, indx_after_incr, niters, NULL_TREE,
				 NULL_TREE);

  gsi_insert_before (&loop_cond_gsi, cond_stmt, GSI_SAME_STMT);

  /* Remove old loop exit test:  */
  gsi_remove (&loop_cond_gsi, true);
  free_stmt_vec_info (orig_cond);

  loop_loc = find_loop_location (loop);
  if (dump_enabled_p ())
    {
      if (LOCATION_LOCUS (loop_loc) != UNKNOWN_LOCATION)
	dump_printf (MSG_NOTE, "\nloop at %s:%d: ", LOCATION_FILE (loop_loc),
		     LOCATION_LINE (loop_loc));
      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, cond_stmt, 0);
    }

  /* Record the number of latch iterations.  */
  loop->nb_iterations = fold_build2 (MINUS_EXPR, TREE_TYPE (niters), niters,
				     build_int_cst (TREE_TYPE (niters), 1));
}

/* Helper routine of slpeel_tree_duplicate_loop_to_edge_cfg.
   For all PHI arguments in FROM->dest and TO->dest from those
   edges ensure that TO->dest PHI arguments have current_def
   to that in from.  */

static void
slpeel_duplicate_current_defs_from_edges (edge from, edge to)
{
  gimple_stmt_iterator gsi_from, gsi_to;

  for (gsi_from = gsi_start_phis (from->dest),
       gsi_to = gsi_start_phis (to->dest);
       !gsi_end_p (gsi_from) && !gsi_end_p (gsi_to);)
    {
      gimple *from_phi = gsi_stmt (gsi_from);
      gimple *to_phi = gsi_stmt (gsi_to);
      tree from_arg = PHI_ARG_DEF_FROM_EDGE (from_phi, from);
      tree to_arg = PHI_ARG_DEF_FROM_EDGE (to_phi, to);
      if (virtual_operand_p (from_arg))
	{
	  gsi_next (&gsi_from);
	  continue;
	}
      if (virtual_operand_p (to_arg))
	{
	  gsi_next (&gsi_to);
	  continue;
	}
      if (TREE_CODE (from_arg) != SSA_NAME)
	gcc_assert (operand_equal_p (from_arg, to_arg, 0));
      else
	{
	  if (get_current_def (to_arg) == NULL_TREE)
	    set_current_def (to_arg, get_current_def (from_arg));
	}
      gsi_next (&gsi_from);
      gsi_next (&gsi_to);
    }

  gphi *from_phi = get_virtual_phi (from->dest);
  gphi *to_phi = get_virtual_phi (to->dest);
  if (from_phi)
    set_current_def (PHI_ARG_DEF_FROM_EDGE (to_phi, to),
		     get_current_def (PHI_ARG_DEF_FROM_EDGE (from_phi, from)));
}


/* Given LOOP this function generates a new copy of it and puts it
   on E which is either the entry or exit of LOOP.  If SCALAR_LOOP is
   non-NULL, assume LOOP and SCALAR_LOOP are equivalent and copy the
   basic blocks from SCALAR_LOOP instead of LOOP, but to either the
   entry or exit of LOOP.  */

struct loop *
slpeel_tree_duplicate_loop_to_edge_cfg (struct loop *loop,
					struct loop *scalar_loop, edge e)
{
  struct loop *new_loop;
  basic_block *new_bbs, *bbs, *pbbs;
  bool at_exit;
  bool was_imm_dom;
  basic_block exit_dest;
  edge exit, new_exit;
  bool duplicate_outer_loop = false;

  exit = single_exit (loop);
  at_exit = (e == exit);
  if (!at_exit && e != loop_preheader_edge (loop))
    return NULL;

  if (scalar_loop == NULL)
    scalar_loop = loop;

  bbs = XNEWVEC (basic_block, scalar_loop->num_nodes + 1);
  pbbs = bbs + 1;
  get_loop_body_with_size (scalar_loop, pbbs, scalar_loop->num_nodes);
  /* Allow duplication of outer loops.  */
  if (scalar_loop->inner)
    duplicate_outer_loop = true;
  /* Check whether duplication is possible.  */
  if (!can_copy_bbs_p (pbbs, scalar_loop->num_nodes))
    {
      free (bbs);
      return NULL;
    }

  /* Generate new loop structure.  */
  new_loop = duplicate_loop (scalar_loop, loop_outer (scalar_loop));
  duplicate_subloops (scalar_loop, new_loop);

  exit_dest = exit->dest;
  was_imm_dom = (get_immediate_dominator (CDI_DOMINATORS,
					  exit_dest) == loop->header ?
		 true : false);

  /* Also copy the pre-header, this avoids jumping through hoops to
     duplicate the loop entry PHI arguments.  Create an empty
     pre-header unconditionally for this.  */
  basic_block preheader = split_edge (loop_preheader_edge (scalar_loop));
  edge entry_e = single_pred_edge (preheader);
  bbs[0] = preheader;
  new_bbs = XNEWVEC (basic_block, scalar_loop->num_nodes + 1);

  exit = single_exit (scalar_loop);
  copy_bbs (bbs, scalar_loop->num_nodes + 1, new_bbs,
	    &exit, 1, &new_exit, NULL,
	    at_exit ? loop->latch : e->src, true);
  exit = single_exit (loop);
  basic_block new_preheader = new_bbs[0];

  add_phi_args_after_copy (new_bbs, scalar_loop->num_nodes + 1, NULL);

  if (scalar_loop != loop)
    {
      /* If we copied from SCALAR_LOOP rather than LOOP, SSA_NAMEs from
	 SCALAR_LOOP will have current_def set to SSA_NAMEs in the new_loop,
	 but LOOP will not.  slpeel_update_phi_nodes_for_guard{1,2} expects
	 the LOOP SSA_NAMEs (on the exit edge and edge from latch to
	 header) to have current_def set, so copy them over.  */
      slpeel_duplicate_current_defs_from_edges (single_exit (scalar_loop),
						exit);
      slpeel_duplicate_current_defs_from_edges (EDGE_SUCC (scalar_loop->latch,
							   0),
						EDGE_SUCC (loop->latch, 0));
    }

  if (at_exit) /* Add the loop copy at exit.  */
    {
      if (scalar_loop != loop)
	{
	  gphi_iterator gsi;
	  new_exit = redirect_edge_and_branch (new_exit, exit_dest);

	  for (gsi = gsi_start_phis (exit_dest); !gsi_end_p (gsi);
	       gsi_next (&gsi))
	    {
	      gphi *phi = gsi.phi ();
	      tree orig_arg = PHI_ARG_DEF_FROM_EDGE (phi, e);
	      location_t orig_locus
		= gimple_phi_arg_location_from_edge (phi, e);

	      add_phi_arg (phi, orig_arg, new_exit, orig_locus);
	    }
	}
      redirect_edge_and_branch_force (e, new_preheader);
      flush_pending_stmts (e);
      set_immediate_dominator (CDI_DOMINATORS, new_preheader, e->src);
      if (was_imm_dom || duplicate_outer_loop)
	set_immediate_dominator (CDI_DOMINATORS, exit_dest, new_exit->src);

      /* And remove the non-necessary forwarder again.  Keep the other
         one so we have a proper pre-header for the loop at the exit edge.  */
      redirect_edge_pred (single_succ_edge (preheader),
			  single_pred (preheader));
      delete_basic_block (preheader);
      set_immediate_dominator (CDI_DOMINATORS, scalar_loop->header,
			       loop_preheader_edge (scalar_loop)->src);
    }
  else /* Add the copy at entry.  */
    {
      if (scalar_loop != loop)
	{
	  /* Remove the non-necessary forwarder of scalar_loop again.  */
	  redirect_edge_pred (single_succ_edge (preheader),
			      single_pred (preheader));
	  delete_basic_block (preheader);
	  set_immediate_dominator (CDI_DOMINATORS, scalar_loop->header,
				   loop_preheader_edge (scalar_loop)->src);
	  preheader = split_edge (loop_preheader_edge (loop));
	  entry_e = single_pred_edge (preheader);
	}

      redirect_edge_and_branch_force (entry_e, new_preheader);
      flush_pending_stmts (entry_e);
      set_immediate_dominator (CDI_DOMINATORS, new_preheader, entry_e->src);

      redirect_edge_and_branch_force (new_exit, preheader);
      flush_pending_stmts (new_exit);
      set_immediate_dominator (CDI_DOMINATORS, preheader, new_exit->src);

      /* And remove the non-necessary forwarder again.  Keep the other
         one so we have a proper pre-header for the loop at the exit edge.  */
      redirect_edge_pred (single_succ_edge (new_preheader),
			  single_pred (new_preheader));
      delete_basic_block (new_preheader);
      set_immediate_dominator (CDI_DOMINATORS, new_loop->header,
			       loop_preheader_edge (new_loop)->src);
    }

  for (unsigned i = 0; i < scalar_loop->num_nodes + 1; i++)
    rename_variables_in_bb (new_bbs[i], duplicate_outer_loop);

  if (scalar_loop != loop)
    {
      /* Update new_loop->header PHIs, so that on the preheader
	 edge they are the ones from loop rather than scalar_loop.  */
      gphi_iterator gsi_orig, gsi_new;
      edge orig_e = loop_preheader_edge (loop);
      edge new_e = loop_preheader_edge (new_loop);

      for (gsi_orig = gsi_start_phis (loop->header),
	   gsi_new = gsi_start_phis (new_loop->header);
	   !gsi_end_p (gsi_orig) && !gsi_end_p (gsi_new);
	   gsi_next (&gsi_orig), gsi_next (&gsi_new))
	{
	  gphi *orig_phi = gsi_orig.phi ();
	  gphi *new_phi = gsi_new.phi ();
	  tree orig_arg = PHI_ARG_DEF_FROM_EDGE (orig_phi, orig_e);
	  location_t orig_locus
	    = gimple_phi_arg_location_from_edge (orig_phi, orig_e);

	  add_phi_arg (new_phi, orig_arg, new_e, orig_locus);
	}
    }

  free (new_bbs);
  free (bbs);

  checking_verify_dominators (CDI_DOMINATORS);

  return new_loop;
}


/* Given the condition expression COND, put it as the last statement of
   GUARD_BB; set both edges' probability; set dominator of GUARD_TO to
   DOM_BB; return the skip edge.  GUARD_TO is the target basic block to
   skip the loop.  PROBABILITY is the skip edge's probability.  Mark the
   new edge as irreducible if IRREDUCIBLE_P is true.  */

static edge
slpeel_add_loop_guard (basic_block guard_bb, tree cond,
		       basic_block guard_to, basic_block dom_bb,
		       profile_probability probability, bool irreducible_p)
{
  gimple_stmt_iterator gsi;
  edge new_e, enter_e;
  gcond *cond_stmt;
  gimple_seq gimplify_stmt_list = NULL;

  enter_e = EDGE_SUCC (guard_bb, 0);
  enter_e->flags &= ~EDGE_FALLTHRU;
  enter_e->flags |= EDGE_FALSE_VALUE;
  gsi = gsi_last_bb (guard_bb);

  cond = force_gimple_operand_1 (cond, &gimplify_stmt_list, is_gimple_condexpr,
				 NULL_TREE);
  if (gimplify_stmt_list)
    gsi_insert_seq_after (&gsi, gimplify_stmt_list, GSI_NEW_STMT);

  cond_stmt = gimple_build_cond_from_tree (cond, NULL_TREE, NULL_TREE);
  gsi = gsi_last_bb (guard_bb);
  gsi_insert_after (&gsi, cond_stmt, GSI_NEW_STMT);

  /* Add new edge to connect guard block to the merge/loop-exit block.  */
  new_e = make_edge (guard_bb, guard_to, EDGE_TRUE_VALUE);

  new_e->count = guard_bb->count;
  new_e->probability = probability;
  new_e->count = enter_e->count.apply_probability (probability);
  if (irreducible_p)
    new_e->flags |= EDGE_IRREDUCIBLE_LOOP;

  enter_e->count -= new_e->count;
  enter_e->probability = probability.invert ();
  set_immediate_dominator (CDI_DOMINATORS, guard_to, dom_bb);

  /* Split enter_e to preserve LOOPS_HAVE_PREHEADERS.  */
  if (enter_e->dest->loop_father->header == enter_e->dest)
    split_edge (enter_e);

  return new_e;
}


/* This function verifies that the following restrictions apply to LOOP:
   (1) it consists of exactly 2 basic blocks - header, and an empty latch
       for innermost loop and 5 basic blocks for outer-loop.
   (2) it is single entry, single exit
   (3) its exit condition is the last stmt in the header
   (4) E is the entry/exit edge of LOOP.
 */

bool
slpeel_can_duplicate_loop_p (const struct loop *loop, const_edge e)
{
  edge exit_e = single_exit (loop);
  edge entry_e = loop_preheader_edge (loop);
  gcond *orig_cond = get_loop_exit_condition (loop);
  gimple_stmt_iterator loop_exit_gsi = gsi_last_bb (exit_e->src);
  unsigned int num_bb = loop->inner? 5 : 2;

  /* All loops have an outer scope; the only case loop->outer is NULL is for
     the function itself.  */
  if (!loop_outer (loop)
      || loop->num_nodes != num_bb
      || !empty_block_p (loop->latch)
      || !single_exit (loop)
      /* Verify that new loop exit condition can be trivially modified.  */
      || (!orig_cond || orig_cond != gsi_stmt (loop_exit_gsi))
      || (e != exit_e && e != entry_e))
    return false;

  return true;
}

/* If the loop has a virtual PHI, but exit bb doesn't, create a virtual PHI
   in the exit bb and rename all the uses after the loop.  This simplifies
   the *guard[12] routines, which assume loop closed SSA form for all PHIs
   (but normally loop closed SSA form doesn't require virtual PHIs to be
   in the same form).  Doing this early simplifies the checking what
   uses should be renamed.  */

static void
create_lcssa_for_virtual_phi (struct loop *loop)
{
  gphi_iterator gsi;
  edge exit_e = single_exit (loop);

  for (gsi = gsi_start_phis (loop->header); !gsi_end_p (gsi); gsi_next (&gsi))
    if (virtual_operand_p (gimple_phi_result (gsi_stmt (gsi))))
      {
	gphi *phi = gsi.phi ();
	for (gsi = gsi_start_phis (exit_e->dest);
	     !gsi_end_p (gsi); gsi_next (&gsi))
	  if (virtual_operand_p (gimple_phi_result (gsi_stmt (gsi))))
	    break;
	if (gsi_end_p (gsi))
	  {
	    tree new_vop = copy_ssa_name (PHI_RESULT (phi));
	    gphi *new_phi = create_phi_node (new_vop, exit_e->dest);
	    tree vop = PHI_ARG_DEF_FROM_EDGE (phi, EDGE_SUCC (loop->latch, 0));
	    imm_use_iterator imm_iter;
	    gimple *stmt;
	    use_operand_p use_p;

	    add_phi_arg (new_phi, vop, exit_e, UNKNOWN_LOCATION);
	    gimple_phi_set_result (new_phi, new_vop);
	    FOR_EACH_IMM_USE_STMT (stmt, imm_iter, vop)
	      if (stmt != new_phi
		  && !flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
		FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
		  SET_USE (use_p, new_vop);
	  }
	break;
      }

}

/* Function vect_get_loop_location.

   Extract the location of the loop in the source code.
   If the loop is not well formed for vectorization, an estimated
   location is calculated.
   Return the loop location if succeed and NULL if not.  */

source_location
find_loop_location (struct loop *loop)
{
  gimple *stmt = NULL;
  basic_block bb;
  gimple_stmt_iterator si;

  if (!loop)
    return UNKNOWN_LOCATION;

  stmt = get_loop_exit_condition (loop);

  if (stmt
      && LOCATION_LOCUS (gimple_location (stmt)) > BUILTINS_LOCATION)
    return gimple_location (stmt);

  /* If we got here the loop is probably not "well formed",
     try to estimate the loop location */

  if (!loop->header)
    return UNKNOWN_LOCATION;

  bb = loop->header;

  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      stmt = gsi_stmt (si);
      if (LOCATION_LOCUS (gimple_location (stmt)) > BUILTINS_LOCATION)
        return gimple_location (stmt);
    }

  return UNKNOWN_LOCATION;
}

/* Return true if PHI defines an IV of the loop to be vectorized.  */

static bool
iv_phi_p (gphi *phi)
{
  if (virtual_operand_p (PHI_RESULT (phi)))
    return false;

  stmt_vec_info stmt_info = vinfo_for_stmt (phi);
  gcc_assert (stmt_info != NULL);
  if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def
      || STMT_VINFO_DEF_TYPE (stmt_info) == vect_double_reduction_def)
    return false;

  return true;
}

/* Function vect_can_advance_ivs_p

   In case the number of iterations that LOOP iterates is unknown at compile
   time, an epilog loop will be generated, and the loop induction variables
   (IVs) will be "advanced" to the value they are supposed to take just before
   the epilog loop.  Here we check that the access function of the loop IVs
   and the expression that represents the loop bound are simple enough.
   These restrictions will be relaxed in the future.  */

bool
vect_can_advance_ivs_p (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block bb = loop->header;
  gphi_iterator gsi;

  /* Analyze phi functions of the loop header.  */

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "vect_can_advance_ivs_p:\n");
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      tree evolution_part;

      gphi *phi = gsi.phi ();
      if (dump_enabled_p ())
	{
          dump_printf_loc (MSG_NOTE, vect_location, "Analyze phi: ");
          dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi, 0);
	}

      /* Skip virtual phi's. The data dependences that are associated with
	 virtual defs/uses (i.e., memory accesses) are analyzed elsewhere.

	 Skip reduction phis.  */
      if (!iv_phi_p (phi))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "reduc or virtual phi. skip.\n");
	  continue;
	}

      /* Analyze the evolution function.  */

      evolution_part
	= STMT_VINFO_LOOP_PHI_EVOLUTION_PART (vinfo_for_stmt (phi));
      if (evolution_part == NULL_TREE)
        {
	  if (dump_enabled_p ())
	    dump_printf (MSG_MISSED_OPTIMIZATION,
			 "No access function or evolution.\n");
	  return false;
        }

      /* FORNOW: We do not transform initial conditions of IVs
	 which evolution functions are not invariants in the loop.  */

      if (!expr_invariant_in_loop_p (loop, evolution_part))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "evolution not invariant in loop.\n");
	  return false;
	}

      /* FORNOW: We do not transform initial conditions of IVs
	 which evolution functions are a polynomial of degree >= 2.  */

      if (tree_is_chrec (evolution_part))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "evolution is chrec.\n");
	  return false;
	}
    }

  return true;
}


/*   Function vect_update_ivs_after_vectorizer.

     "Advance" the induction variables of LOOP to the value they should take
     after the execution of LOOP.  This is currently necessary because the
     vectorizer does not handle induction variables that are used after the
     loop.  Such a situation occurs when the last iterations of LOOP are
     peeled, because:
     1. We introduced new uses after LOOP for IVs that were not originally used
        after LOOP: the IVs of LOOP are now used by an epilog loop.
     2. LOOP is going to be vectorized; this means that it will iterate N/VF
        times, whereas the loop IVs should be bumped N times.

     Input:
     - LOOP - a loop that is going to be vectorized. The last few iterations
              of LOOP were peeled.
     - NITERS - the number of iterations that LOOP executes (before it is
                vectorized). i.e, the number of times the ivs should be bumped.
     - UPDATE_E - a successor edge of LOOP->exit that is on the (only) path
                  coming out from LOOP on which there are uses of the LOOP ivs
		  (this is the path from LOOP->exit to epilog_loop->preheader).

                  The new definitions of the ivs are placed in LOOP->exit.
                  The phi args associated with the edge UPDATE_E in the bb
                  UPDATE_E->dest are updated accordingly.

     Assumption 1: Like the rest of the vectorizer, this function assumes
     a single loop exit that has a single predecessor.

     Assumption 2: The phi nodes in the LOOP header and in update_bb are
     organized in the same order.

     Assumption 3: The access function of the ivs is simple enough (see
     vect_can_advance_ivs_p).  This assumption will be relaxed in the future.

     Assumption 4: Exactly one of the successors of LOOP exit-bb is on a path
     coming out of LOOP on which the ivs of LOOP are used (this is the path
     that leads to the epilog loop; other paths skip the epilog loop).  This
     path starts with the edge UPDATE_E, and its destination (denoted update_bb)
     needs to have its phis updated.
 */

static void
vect_update_ivs_after_vectorizer (loop_vec_info loop_vinfo,
				  tree niters, edge update_e)
{
  gphi_iterator gsi, gsi1;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block update_bb = update_e->dest;
  basic_block exit_bb = single_exit (loop)->dest;

  /* Make sure there exists a single-predecessor exit bb:  */
  gcc_assert (single_pred_p (exit_bb));
  gcc_assert (single_succ_edge (exit_bb) == update_e);

  for (gsi = gsi_start_phis (loop->header), gsi1 = gsi_start_phis (update_bb);
       !gsi_end_p (gsi) && !gsi_end_p (gsi1);
       gsi_next (&gsi), gsi_next (&gsi1))
    {
      tree init_expr;
      tree step_expr, off;
      tree type;
      tree var, ni, ni_name;
      gimple_stmt_iterator last_gsi;

      gphi *phi = gsi.phi ();
      gphi *phi1 = gsi1.phi ();
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "vect_update_ivs_after_vectorizer: phi: ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi, 0);
	}

      /* Skip reduction and virtual phis.  */
      if (!iv_phi_p (phi))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "reduc or virtual phi. skip.\n");
	  continue;
	}

      type = TREE_TYPE (gimple_phi_result (phi));
      step_expr = STMT_VINFO_LOOP_PHI_EVOLUTION_PART (vinfo_for_stmt (phi));
      step_expr = unshare_expr (step_expr);

      /* FORNOW: We do not support IVs whose evolution function is a polynomial
         of degree >= 2 or exponential.  */
      gcc_assert (!tree_is_chrec (step_expr));

      init_expr = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));

      off = fold_build2 (MULT_EXPR, TREE_TYPE (step_expr),
			 fold_convert (TREE_TYPE (step_expr), niters),
			 step_expr);
      if (POINTER_TYPE_P (type))
	ni = fold_build_pointer_plus (init_expr, off);
      else
	ni = fold_build2 (PLUS_EXPR, type,
			  init_expr, fold_convert (type, off));

      var = create_tmp_var (type, "tmp");

      last_gsi = gsi_last_bb (exit_bb);
      gimple_seq new_stmts = NULL;
      ni_name = force_gimple_operand (ni, &new_stmts, false, var);
      /* Exit_bb shouldn't be empty.  */
      if (!gsi_end_p (last_gsi))
	gsi_insert_seq_after (&last_gsi, new_stmts, GSI_SAME_STMT);
      else
	gsi_insert_seq_before (&last_gsi, new_stmts, GSI_SAME_STMT);

      /* Fix phi expressions in the successor bb.  */
      adjust_phi_and_debug_stmts (phi1, update_e, ni_name);
    }
}

/* Function vect_gen_prolog_loop_niters

   Generate the number of iterations which should be peeled as prolog for the
   loop represented by LOOP_VINFO.  It is calculated as the misalignment of
   DR - the data reference recorded in LOOP_VINFO_UNALIGNED_DR (LOOP_VINFO).
   As a result, after the execution of this loop, the data reference DR will
   refer to an aligned location.  The following computation is generated:

   If the misalignment of DR is known at compile time:
     addr_mis = int mis = DR_MISALIGNMENT (dr);
   Else, compute address misalignment in bytes:
     addr_mis = addr & (vectype_align - 1)

   prolog_niters = ((VF - addr_mis/elem_size)&(VF-1))/step

   (elem_size = element type size; an element is the scalar element whose type
   is the inner type of the vectype)

   The computations will be emitted at the end of BB.  We also compute and
   store upper bound (included) of the result in BOUND.

   When the step of the data-ref in the loop is not 1 (as in interleaved data
   and SLP), the number of iterations of the prolog must be divided by the step
   (which is equal to the size of interleaved group).

   The above formulas assume that VF == number of elements in the vector. This
   may not hold when there are multiple-types in the loop.
   In this case, for some data-references in the loop the VF does not represent
   the number of elements that fit in the vector.  Therefore, instead of VF we
   use TYPE_VECTOR_SUBPARTS.  */

static tree
vect_gen_prolog_loop_niters (loop_vec_info loop_vinfo,
			     basic_block bb, int *bound)
{
  struct data_reference *dr = LOOP_VINFO_UNALIGNED_DR (loop_vinfo);
  tree var;
  tree niters_type = TREE_TYPE (LOOP_VINFO_NITERS (loop_vinfo));
  gimple_seq stmts = NULL, new_stmts = NULL;
  tree iters, iters_name;
  gimple *dr_stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (dr_stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  int vectype_align = TYPE_ALIGN (vectype) / BITS_PER_UNIT;
  int nelements = TYPE_VECTOR_SUBPARTS (vectype);

  if (LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo) > 0)
    {
      int npeel = LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo);

      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "known peeling = %d.\n", npeel);

      iters = build_int_cst (niters_type, npeel);
      *bound = LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo);
    }
  else
    {
      bool negative = tree_int_cst_compare (DR_STEP (dr), size_zero_node) < 0;
      tree offset = negative
	  ? size_int (-TYPE_VECTOR_SUBPARTS (vectype) + 1) : size_zero_node;
      tree start_addr = vect_create_addr_base_for_vector_ref (dr_stmt,
							      &stmts, offset);
      tree type = unsigned_type_for (TREE_TYPE (start_addr));
      tree vectype_align_minus_1 = build_int_cst (type, vectype_align - 1);
      HOST_WIDE_INT elem_size =
                int_cst_value (TYPE_SIZE_UNIT (TREE_TYPE (vectype)));
      tree elem_size_log = build_int_cst (type, exact_log2 (elem_size));
      tree nelements_minus_1 = build_int_cst (type, nelements - 1);
      tree nelements_tree = build_int_cst (type, nelements);
      tree byte_misalign;
      tree elem_misalign;

      /* Create:  byte_misalign = addr & (vectype_align - 1)  */
      byte_misalign =
	fold_build2 (BIT_AND_EXPR, type, fold_convert (type, start_addr),
		     vectype_align_minus_1);

      /* Create:  elem_misalign = byte_misalign / element_size  */
      elem_misalign =
	fold_build2 (RSHIFT_EXPR, type, byte_misalign, elem_size_log);

      /* Create:  (niters_type) (nelements - elem_misalign)&(nelements - 1)  */
      if (negative)
	iters = fold_build2 (MINUS_EXPR, type, elem_misalign, nelements_tree);
      else
	iters = fold_build2 (MINUS_EXPR, type, nelements_tree, elem_misalign);
      iters = fold_build2 (BIT_AND_EXPR, type, iters, nelements_minus_1);
      iters = fold_convert (niters_type, iters);
      *bound = nelements - 1;
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
                       "niters for prolog loop: ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, iters);
      dump_printf (MSG_NOTE, "\n");
    }

  var = create_tmp_var (niters_type, "prolog_loop_niters");
  iters_name = force_gimple_operand (iters, &new_stmts, false, var);

  if (new_stmts)
    gimple_seq_add_seq (&stmts, new_stmts);
  if (stmts)
    {
      gcc_assert (single_succ_p (bb));
      gimple_stmt_iterator gsi = gsi_last_bb (bb);
      if (gsi_end_p (gsi))
	gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);
      else
	gsi_insert_seq_after (&gsi, stmts, GSI_SAME_STMT);
    }
  return iters_name;
}


/* Function vect_update_init_of_dr

   NITERS iterations were peeled from LOOP.  DR represents a data reference
   in LOOP.  This function updates the information recorded in DR to
   account for the fact that the first NITERS iterations had already been
   executed.  Specifically, it updates the OFFSET field of DR.  */

static void
vect_update_init_of_dr (struct data_reference *dr, tree niters)
{
  tree offset = DR_OFFSET (dr);

  niters = fold_build2 (MULT_EXPR, sizetype,
			fold_convert (sizetype, niters),
			fold_convert (sizetype, DR_STEP (dr)));
  offset = fold_build2 (PLUS_EXPR, sizetype,
			fold_convert (sizetype, offset), niters);
  DR_OFFSET (dr) = offset;
}


/* Function vect_update_inits_of_drs

   NITERS iterations were peeled from the loop represented by LOOP_VINFO.
   This function updates the information recorded for the data references in
   the loop to account for the fact that the first NITERS iterations had
   already been executed.  Specifically, it updates the initial_condition of
   the access_function of all the data_references in the loop.  */

static void
vect_update_inits_of_drs (loop_vec_info loop_vinfo, tree niters)
{
  unsigned int i;
  vec<data_reference_p> datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  struct data_reference *dr;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "=== vect_update_inits_of_dr ===\n");

  /* Adjust niters to sizetype and insert stmts on loop preheader edge.  */
  if (!types_compatible_p (sizetype, TREE_TYPE (niters)))
    {
      gimple_seq seq;
      edge pe = loop_preheader_edge (LOOP_VINFO_LOOP (loop_vinfo));
      tree var = create_tmp_var (sizetype, "prolog_loop_adjusted_niters");

      niters = fold_convert (sizetype, niters);
      niters = force_gimple_operand (niters, &seq, false, var);
      if (seq)
	{
	  basic_block new_bb = gsi_insert_seq_on_edge_immediate (pe, seq);
	  gcc_assert (!new_bb);
	}
    }

  FOR_EACH_VEC_ELT (datarefs, i, dr)
    vect_update_init_of_dr (dr, niters);
}


/* This function builds ni_name = number of iterations.  Statements
   are emitted on the loop preheader edge.  If NEW_VAR_P is not NULL, set
   it to TRUE if new ssa_var is generated.  */

tree
vect_build_loop_niters (loop_vec_info loop_vinfo, bool *new_var_p)
{
  tree ni = unshare_expr (LOOP_VINFO_NITERS (loop_vinfo));
  if (TREE_CODE (ni) == INTEGER_CST)
    return ni;
  else
    {
      tree ni_name, var;
      gimple_seq stmts = NULL;
      edge pe = loop_preheader_edge (LOOP_VINFO_LOOP (loop_vinfo));

      var = create_tmp_var (TREE_TYPE (ni), "niters");
      ni_name = force_gimple_operand (ni, &stmts, false, var);
      if (stmts)
	{
	  gsi_insert_seq_on_edge_immediate (pe, stmts);
	  if (new_var_p != NULL)
	    *new_var_p = true;
	}

      return ni_name;
    }
}

/* Calculate the number of iterations above which vectorized loop will be
   preferred than scalar loop.  NITERS_PROLOG is the number of iterations
   of prolog loop.  If it's integer const, the integer number is also passed
   in INT_NITERS_PROLOG.  BOUND_PROLOG is the upper bound (included) of
   number of iterations of prolog loop.  VFM1 is vector factor minus one.
   If CHECK_PROFITABILITY is true, TH is the threshold below which scalar
   (rather than vectorized) loop will be executed.  This function stores
   upper bound (included) of the result in BOUND_SCALAR.  */

static tree
vect_gen_scalar_loop_niters (tree niters_prolog, int int_niters_prolog,
			     int bound_prolog, int vfm1, int th,
			     int *bound_scalar, bool check_profitability)
{
  tree type = TREE_TYPE (niters_prolog);
  tree niters = fold_build2 (PLUS_EXPR, type, niters_prolog,
			     build_int_cst (type, vfm1));

  *bound_scalar = vfm1 + bound_prolog;
  if (check_profitability)
    {
      /* TH indicates the minimum niters of vectorized loop, while we
	 compute the maximum niters of scalar loop.  */
      th--;
      /* Peeling for constant times.  */
      if (int_niters_prolog >= 0)
	{
	  *bound_scalar = (int_niters_prolog + vfm1 < th
			    ? th
			    : vfm1 + int_niters_prolog);
	  return build_int_cst (type, *bound_scalar);
	}
      /* Peeling for unknown times.  Note BOUND_PROLOG is the upper
	 bound (inlcuded) of niters of prolog loop.  */
      if (th >=  vfm1 + bound_prolog)
	{
	  *bound_scalar = th;
	  return build_int_cst (type, th);
	}
      /* Need to do runtime comparison, but BOUND_SCALAR remains the same.  */
      else if (th > vfm1)
	return fold_build2 (MAX_EXPR, type, build_int_cst (type, th), niters);
    }
  return niters;
}

/* This function generates the following statements:

   niters = number of iterations loop executes (after peeling)
   niters_vector = niters / vf

   and places them on the loop preheader edge.  NITERS_NO_OVERFLOW is
   true if NITERS doesn't overflow.  */

void
vect_gen_vector_loop_niters (loop_vec_info loop_vinfo, tree niters,
			     tree *niters_vector_ptr, bool niters_no_overflow)
{
  tree ni_minus_gap, var;
  tree niters_vector, type = TREE_TYPE (niters);
  int vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  edge pe = loop_preheader_edge (LOOP_VINFO_LOOP (loop_vinfo));
  tree log_vf = build_int_cst (type, exact_log2 (vf));

  /* If epilogue loop is required because of data accesses with gaps, we
     subtract one iteration from the total number of iterations here for
     correct calculation of RATIO.  */
  if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo))
    {
      ni_minus_gap = fold_build2 (MINUS_EXPR, type, niters,
				  build_one_cst (type));
      if (!is_gimple_val (ni_minus_gap))
	{
	  var = create_tmp_var (type, "ni_gap");
	  gimple *stmts = NULL;
	  ni_minus_gap = force_gimple_operand (ni_minus_gap, &stmts,
					       true, var);
	  gsi_insert_seq_on_edge_immediate (pe, stmts);
	}
    }
  else
    ni_minus_gap = niters;

  /* Create: niters >> log2(vf) */
  /* If it's known that niters == number of latch executions + 1 doesn't
     overflow, we can generate niters >> log2(vf); otherwise we generate
     (niters - vf) >> log2(vf) + 1 by using the fact that we know ratio
     will be at least one.  */
  if (niters_no_overflow)
    niters_vector = fold_build2 (RSHIFT_EXPR, type, ni_minus_gap, log_vf);
  else
    niters_vector
      = fold_build2 (PLUS_EXPR, type,
		     fold_build2 (RSHIFT_EXPR, type,
				  fold_build2 (MINUS_EXPR, type, ni_minus_gap,
					       build_int_cst (type, vf)),
				  log_vf),
		     build_int_cst (type, 1));

  if (!is_gimple_val (niters_vector))
    {
      var = create_tmp_var (type, "bnd");
      gimple_seq stmts = NULL;
      niters_vector = force_gimple_operand (niters_vector, &stmts, true, var);
      gsi_insert_seq_on_edge_immediate (pe, stmts);
      /* Peeling algorithm guarantees that vector loop bound is at least ONE,
	 we set range information to make niters analyzer's life easier.  */
      if (stmts != NULL)
	set_range_info (niters_vector, VR_RANGE, build_int_cst (type, 1),
			fold_build2 (RSHIFT_EXPR, type,
				     TYPE_MAX_VALUE (type), log_vf));
    }
  *niters_vector_ptr = niters_vector;

  return;
}

/* Given NITERS_VECTOR which is the number of iterations for vectorized
   loop specified by LOOP_VINFO after vectorization, compute the number
   of iterations before vectorization (niters_vector * vf) and store it
   to NITERS_VECTOR_MULT_VF_PTR.  */

static void
vect_gen_vector_loop_niters_mult_vf (loop_vec_info loop_vinfo,
				     tree niters_vector,
				     tree *niters_vector_mult_vf_ptr)
{
  int vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree type = TREE_TYPE (niters_vector);
  tree log_vf = build_int_cst (type, exact_log2 (vf));
  basic_block exit_bb = single_exit (loop)->dest;

  gcc_assert (niters_vector_mult_vf_ptr != NULL);
  tree niters_vector_mult_vf = fold_build2 (LSHIFT_EXPR, type,
					    niters_vector, log_vf);
  if (!is_gimple_val (niters_vector_mult_vf))
    {
      tree var = create_tmp_var (type, "niters_vector_mult_vf");
      gimple_seq stmts = NULL;
      niters_vector_mult_vf = force_gimple_operand (niters_vector_mult_vf,
						    &stmts, true, var);
      gimple_stmt_iterator gsi = gsi_start_bb (exit_bb);
      gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);
    }
  *niters_vector_mult_vf_ptr = niters_vector_mult_vf;
}

/* Function slpeel_tree_duplicate_loop_to_edge_cfg duplciates FIRST/SECOND
   from SECOND/FIRST and puts it at the original loop's preheader/exit
   edge, the two loops are arranged as below:

       preheader_a:
     first_loop:
       header_a:
	 i_1 = PHI<i_0, i_2>;
	 ...
	 i_2 = i_1 + 1;
	 if (cond_a)
	   goto latch_a;
	 else
	   goto between_bb;
       latch_a:
	 goto header_a;

       between_bb:
	 ;; i_x = PHI<i_2>;   ;; LCSSA phi node to be created for FIRST,

     second_loop:
       header_b:
	 i_3 = PHI<i_0, i_4>; ;; Use of i_0 to be replaced with i_x,
				 or with i_2 if no LCSSA phi is created
				 under condition of CREATE_LCSSA_FOR_IV_PHIS.
	 ...
	 i_4 = i_3 + 1;
	 if (cond_b)
	   goto latch_b;
	 else
	   goto exit_bb;
       latch_b:
	 goto header_b;

       exit_bb:

   This function creates loop closed SSA for the first loop; update the
   second loop's PHI nodes by replacing argument on incoming edge with the
   result of newly created lcssa PHI nodes.  IF CREATE_LCSSA_FOR_IV_PHIS
   is false, Loop closed ssa phis will only be created for non-iv phis for
   the first loop.

   This function assumes exit bb of the first loop is preheader bb of the
   second loop, i.e, between_bb in the example code.  With PHIs updated,
   the second loop will execute rest iterations of the first.  */

static void
slpeel_update_phi_nodes_for_loops (loop_vec_info loop_vinfo,
				   struct loop *first, struct loop *second,
				   bool create_lcssa_for_iv_phis)
{
  gphi_iterator gsi_update, gsi_orig;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);

  edge first_latch_e = EDGE_SUCC (first->latch, 0);
  edge second_preheader_e = loop_preheader_edge (second);
  basic_block between_bb = single_exit (first)->dest;

  gcc_assert (between_bb == second_preheader_e->src);
  gcc_assert (single_pred_p (between_bb) && single_succ_p (between_bb));
  /* Either the first loop or the second is the loop to be vectorized.  */
  gcc_assert (loop == first || loop == second);

  for (gsi_orig = gsi_start_phis (first->header),
       gsi_update = gsi_start_phis (second->header);
       !gsi_end_p (gsi_orig) && !gsi_end_p (gsi_update);
       gsi_next (&gsi_orig), gsi_next (&gsi_update))
    {
      gphi *orig_phi = gsi_orig.phi ();
      gphi *update_phi = gsi_update.phi ();

      tree arg = PHI_ARG_DEF_FROM_EDGE (orig_phi, first_latch_e);
      /* Generate lcssa PHI node for the first loop.  */
      gphi *vect_phi = (loop == first) ? orig_phi : update_phi;
      if (create_lcssa_for_iv_phis || !iv_phi_p (vect_phi))
	{
	  tree new_res = copy_ssa_name (PHI_RESULT (orig_phi));
	  gphi *lcssa_phi = create_phi_node (new_res, between_bb);
	  add_phi_arg (lcssa_phi, arg, single_exit (first), UNKNOWN_LOCATION);
	  arg = new_res;
	}

      /* Update PHI node in the second loop by replacing arg on the loop's
	 incoming edge.  */
      adjust_phi_and_debug_stmts (update_phi, second_preheader_e, arg);
    }
}

/* Function slpeel_add_loop_guard adds guard skipping from the beginning
   of SKIP_LOOP to the beginning of UPDATE_LOOP.  GUARD_EDGE and MERGE_EDGE
   are two pred edges of the merge point before UPDATE_LOOP.  The two loops
   appear like below:

       guard_bb:
	 if (cond)
	   goto merge_bb;
	 else
	   goto skip_loop;

     skip_loop:
       header_a:
	 i_1 = PHI<i_0, i_2>;
	 ...
	 i_2 = i_1 + 1;
	 if (cond_a)
	   goto latch_a;
	 else
	   goto exit_a;
       latch_a:
	 goto header_a;

       exit_a:
	 i_5 = PHI<i_2>;

       merge_bb:
	 ;; PHI (i_x = PHI<i_0, i_5>) to be created at merge point.

     update_loop:
       header_b:
	 i_3 = PHI<i_5, i_4>;  ;; Use of i_5 to be replaced with i_x.
	 ...
	 i_4 = i_3 + 1;
	 if (cond_b)
	   goto latch_b;
	 else
	   goto exit_bb;
       latch_b:
	 goto header_b;

       exit_bb:

   This function creates PHI nodes at merge_bb and replaces the use of i_5
   in the update_loop's PHI node with the result of new PHI result.  */

static void
slpeel_update_phi_nodes_for_guard1 (struct loop *skip_loop,
				    struct loop *update_loop,
				    edge guard_edge, edge merge_edge)
{
  source_location merge_loc, guard_loc;
  edge orig_e = loop_preheader_edge (skip_loop);
  edge update_e = loop_preheader_edge (update_loop);
  gphi_iterator gsi_orig, gsi_update;

  for ((gsi_orig = gsi_start_phis (skip_loop->header),
	gsi_update = gsi_start_phis (update_loop->header));
       !gsi_end_p (gsi_orig) && !gsi_end_p (gsi_update);
       gsi_next (&gsi_orig), gsi_next (&gsi_update))
    {
      gphi *orig_phi = gsi_orig.phi ();
      gphi *update_phi = gsi_update.phi ();

      /* Generate new phi node at merge bb of the guard.  */
      tree new_res = copy_ssa_name (PHI_RESULT (orig_phi));
      gphi *new_phi = create_phi_node (new_res, guard_edge->dest);

      /* Merge bb has two incoming edges: GUARD_EDGE and MERGE_EDGE.  Set the
	 args in NEW_PHI for these edges.  */
      tree merge_arg = PHI_ARG_DEF_FROM_EDGE (update_phi, update_e);
      tree guard_arg = PHI_ARG_DEF_FROM_EDGE (orig_phi, orig_e);
      merge_loc = gimple_phi_arg_location_from_edge (update_phi, update_e);
      guard_loc = gimple_phi_arg_location_from_edge (orig_phi, orig_e);
      add_phi_arg (new_phi, merge_arg, merge_edge, merge_loc);
      add_phi_arg (new_phi, guard_arg, guard_edge, guard_loc);

      /* Update phi in UPDATE_PHI.  */
      adjust_phi_and_debug_stmts (update_phi, update_e, new_res);
    }
}

/* LCSSA_PHI is a lcssa phi of EPILOG loop which is copied from LOOP,
   this function searches for the corresponding lcssa phi node in exit
   bb of LOOP.  If it is found, return the phi result; otherwise return
   NULL.  */

static tree
find_guard_arg (struct loop *loop, struct loop *epilog ATTRIBUTE_UNUSED,
		gphi *lcssa_phi)
{
  gphi_iterator gsi;
  edge e = single_exit (loop);

  gcc_assert (single_pred_p (e->dest));
  for (gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      if (operand_equal_p (PHI_ARG_DEF (phi, 0),
			   PHI_ARG_DEF (lcssa_phi, 0), 0))
	return PHI_RESULT (phi);
    }
  return NULL_TREE;
}

/* LOOP and EPILOG are two consecutive loops in CFG and EPILOG is copied
   from LOOP.  Function slpeel_add_loop_guard adds guard skipping from a
   point between the two loops to the end of EPILOG.  Edges GUARD_EDGE
   and MERGE_EDGE are the two pred edges of merge_bb at the end of EPILOG.
   The CFG looks like:

     loop:
       header_a:
	 i_1 = PHI<i_0, i_2>;
	 ...
	 i_2 = i_1 + 1;
	 if (cond_a)
	   goto latch_a;
	 else
	   goto exit_a;
       latch_a:
	 goto header_a;

       exit_a:

       guard_bb:
	 if (cond)
	   goto merge_bb;
	 else
	   goto epilog_loop;

       ;; fall_through_bb

     epilog_loop:
       header_b:
	 i_3 = PHI<i_2, i_4>;
	 ...
	 i_4 = i_3 + 1;
	 if (cond_b)
	   goto latch_b;
	 else
	   goto merge_bb;
       latch_b:
	 goto header_b;

       merge_bb:
	 ; PHI node (i_y = PHI<i_2, i_4>) to be created at merge point.

       exit_bb:
	 i_x = PHI<i_4>;  ;Use of i_4 to be replaced with i_y in merge_bb.

   For each name used out side EPILOG (i.e - for each name that has a lcssa
   phi in exit_bb) we create a new PHI in merge_bb.  The new PHI has two
   args corresponding to GUARD_EDGE and MERGE_EDGE.  Arg for MERGE_EDGE is
   the arg of the original PHI in exit_bb, arg for GUARD_EDGE is defined
   by LOOP and is found in the exit bb of LOOP.  Arg of the original PHI
   in exit_bb will also be updated.  */

static void
slpeel_update_phi_nodes_for_guard2 (struct loop *loop, struct loop *epilog,
				    edge guard_edge, edge merge_edge)
{
  gphi_iterator gsi;
  basic_block merge_bb = guard_edge->dest;

  gcc_assert (single_succ_p (merge_bb));
  edge e = single_succ_edge (merge_bb);
  basic_block exit_bb = e->dest;
  gcc_assert (single_pred_p (exit_bb));
  gcc_assert (single_pred (exit_bb) == single_exit (epilog)->dest);

  for (gsi = gsi_start_phis (exit_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *update_phi = gsi.phi ();
      tree old_arg = PHI_ARG_DEF (update_phi, 0);
      /* This loop-closed-phi actually doesn't represent a use out of the
	 loop - the phi arg is a constant.  */
      if (TREE_CODE (old_arg) != SSA_NAME)
	continue;

      tree merge_arg = get_current_def (old_arg);
      if (!merge_arg)
	merge_arg = old_arg;

      tree guard_arg = find_guard_arg (loop, epilog, update_phi);
      /* If the var is live after loop but not a reduction, we simply
	 use the old arg.  */
      if (!guard_arg)
	guard_arg = old_arg;

      /* Create new phi node in MERGE_BB:  */
      tree new_res = copy_ssa_name (PHI_RESULT (update_phi));
      gphi *merge_phi = create_phi_node (new_res, merge_bb);

      /* MERGE_BB has two incoming edges: GUARD_EDGE and MERGE_EDGE, Set
	 the two PHI args in merge_phi for these edges.  */
      add_phi_arg (merge_phi, merge_arg, merge_edge, UNKNOWN_LOCATION);
      add_phi_arg (merge_phi, guard_arg, guard_edge, UNKNOWN_LOCATION);

      /* Update the original phi in exit_bb.  */
      adjust_phi_and_debug_stmts (update_phi, e, new_res);
    }
}

/* EPILOG loop is duplicated from the original loop for vectorizing,
   the arg of its loop closed ssa PHI needs to be updated.  */

static void
slpeel_update_phi_nodes_for_lcssa (struct loop *epilog)
{
  gphi_iterator gsi;
  basic_block exit_bb = single_exit (epilog)->dest;

  gcc_assert (single_pred_p (exit_bb));
  edge e = EDGE_PRED (exit_bb, 0);
  for (gsi = gsi_start_phis (exit_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    rename_use_op (PHI_ARG_DEF_PTR_FROM_EDGE (gsi.phi (), e));
}

/* Function vect_do_peeling.

   Input:
   - LOOP_VINFO: Represent a loop to be vectorized, which looks like:

       preheader:
     LOOP:
       header_bb:
	 loop_body
	 if (exit_loop_cond) goto exit_bb
	 else                goto header_bb
       exit_bb:

   - NITERS: The number of iterations of the loop.
   - NITERSM1: The number of iterations of the loop's latch.
   - NITERS_NO_OVERFLOW: No overflow in computing NITERS.
   - TH, CHECK_PROFITABILITY: Threshold of niters to vectorize loop if
			      CHECK_PROFITABILITY is true.
   Output:
   - NITERS_VECTOR: The number of iterations of loop after vectorization.

   This function peels prolog and epilog from the loop, adds guards skipping
   PROLOG and EPILOG for various conditions.  As a result, the changed CFG
   would look like:

       guard_bb_1:
	 if (prefer_scalar_loop) goto merge_bb_1
	 else                    goto guard_bb_2

       guard_bb_2:
         if (skip_prolog) goto merge_bb_2
         else             goto prolog_preheader

       prolog_preheader:
     PROLOG:
       prolog_header_bb:
	 prolog_body
	 if (exit_prolog_cond) goto prolog_exit_bb
	 else                  goto prolog_header_bb
       prolog_exit_bb:

       merge_bb_2:

       vector_preheader:
     VECTOR LOOP:
       vector_header_bb:
	 vector_body
	 if (exit_vector_cond) goto vector_exit_bb
	 else                  goto vector_header_bb
       vector_exit_bb:

       guard_bb_3:
	 if (skip_epilog) goto merge_bb_3
	 else             goto epilog_preheader

       merge_bb_1:

       epilog_preheader:
     EPILOG:
       epilog_header_bb:
	 epilog_body
	 if (exit_epilog_cond) goto merge_bb_3
	 else                  goto epilog_header_bb

       merge_bb_3:

   Note this function peels prolog and epilog only if it's necessary,
   as well as guards.
   Returns created epilogue or NULL.

   TODO: Guard for prefer_scalar_loop should be emitted along with
   versioning conditions if loop versioning is needed.  */


struct loop *
vect_do_peeling (loop_vec_info loop_vinfo, tree niters, tree nitersm1,
		 tree *niters_vector, int th, bool check_profitability,
		 bool niters_no_overflow)
{
  edge e, guard_e;
  tree type = TREE_TYPE (niters), guard_cond;
  basic_block guard_bb, guard_to;
  profile_probability prob_prolog, prob_vector, prob_epilog;
  int bound_prolog = 0, bound_scalar = 0, bound = 0;
  int vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  int prolog_peeling = LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo);
  bool epilog_peeling = (LOOP_VINFO_PEELING_FOR_NITER (loop_vinfo)
			 || LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo));

  if (!prolog_peeling && !epilog_peeling)
    return NULL;

  prob_vector = profile_probability::guessed_always ().apply_scale (9, 10);
  if ((vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo)) == 2)
    vf = 3;
  prob_prolog = prob_epilog = profile_probability::guessed_always ()
			.apply_scale (vf - 1, vf);
  vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);

  struct loop *prolog, *epilog = NULL, *loop = LOOP_VINFO_LOOP (loop_vinfo);
  struct loop *first_loop = loop;
  bool irred_flag = loop_preheader_edge (loop)->flags & EDGE_IRREDUCIBLE_LOOP;
  create_lcssa_for_virtual_phi (loop);
  update_ssa (TODO_update_ssa_only_virtuals);

  if (MAY_HAVE_DEBUG_STMTS)
    {
      gcc_assert (!adjust_vec.exists ());
      adjust_vec.create (32);
    }
  initialize_original_copy_tables ();

  /* Prolog loop may be skipped.  */
  bool skip_prolog = (prolog_peeling != 0);
  /* Skip to epilog if scalar loop may be preferred.  It's only needed
     when we peel for epilog loop and when it hasn't been checked with
     loop versioning.  */
  bool skip_vector = (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
		      && !LOOP_REQUIRES_VERSIONING (loop_vinfo));
  /* Epilog loop must be executed if the number of iterations for epilog
     loop is known at compile time, otherwise we need to add a check at
     the end of vector loop and skip to the end of epilog loop.  */
  bool skip_epilog = (prolog_peeling < 0
		      || !LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo));
  /* PEELING_FOR_GAPS is special because epilog loop must be executed.  */
  if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo))
    skip_epilog = false;

  /* Record the anchor bb at which guard should be placed if scalar loop
     may be preferred.  */
  basic_block anchor = loop_preheader_edge (loop)->src;
  if (skip_vector)
    {
      split_edge (loop_preheader_edge (loop));

      /* Due to the order in which we peel prolog and epilog, we first
	 propagate probability to the whole loop.  The purpose is to
	 avoid adjusting probabilities of both prolog and vector loops
	 separately.  Note in this case, the probability of epilog loop
	 needs to be scaled back later.  */
      basic_block bb_before_loop = loop_preheader_edge (loop)->src;
      if (prob_vector.initialized_p ())
      scale_bbs_frequencies (&bb_before_loop, 1, prob_vector);
      scale_loop_profile (loop, prob_vector, bound);
    }

  tree niters_prolog = build_int_cst (type, 0);
  source_location loop_loc = find_loop_location (loop);
  struct loop *scalar_loop = LOOP_VINFO_SCALAR_LOOP (loop_vinfo);
  if (prolog_peeling)
    {
      e = loop_preheader_edge (loop);
      if (!slpeel_can_duplicate_loop_p (loop, e))
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, loop_loc,
			   "loop can't be duplicated to preheader edge.\n");
	  gcc_unreachable ();
	}
      /* Peel prolog and put it on preheader edge of loop.  */
      prolog = slpeel_tree_duplicate_loop_to_edge_cfg (loop, scalar_loop, e);
      if (!prolog)
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, loop_loc,
			   "slpeel_tree_duplicate_loop_to_edge_cfg failed.\n");
	  gcc_unreachable ();
	}
      slpeel_update_phi_nodes_for_loops (loop_vinfo, prolog, loop, true);
      first_loop = prolog;
      reset_original_copy_tables ();

      /* Generate and update the number of iterations for prolog loop.  */
      niters_prolog = vect_gen_prolog_loop_niters (loop_vinfo, anchor,
						   &bound_prolog);
      slpeel_make_loop_iterate_ntimes (prolog, niters_prolog);

      /* Skip the prolog loop.  */
      if (skip_prolog)
	{
	  guard_cond = fold_build2 (EQ_EXPR, boolean_type_node,
				    niters_prolog, build_int_cst (type, 0));
	  guard_bb = loop_preheader_edge (prolog)->src;
	  basic_block bb_after_prolog = loop_preheader_edge (loop)->src;
	  guard_to = split_edge (loop_preheader_edge (loop));
	  guard_e = slpeel_add_loop_guard (guard_bb, guard_cond,
					   guard_to, guard_bb,
					   prob_prolog.invert (),
					   irred_flag);
	  e = EDGE_PRED (guard_to, 0);
	  e = (e != guard_e ? e : EDGE_PRED (guard_to, 1));
	  slpeel_update_phi_nodes_for_guard1 (prolog, loop, guard_e, e);

	  scale_bbs_frequencies (&bb_after_prolog, 1, prob_prolog);
	  scale_loop_profile (prolog, prob_prolog, bound_prolog);
	}
      /* Update init address of DRs.  */
      vect_update_inits_of_drs (loop_vinfo, niters_prolog);
      /* Update niters for vector loop.  */
      LOOP_VINFO_NITERS (loop_vinfo)
	= fold_build2 (MINUS_EXPR, type, niters, niters_prolog);
      LOOP_VINFO_NITERSM1 (loop_vinfo)
	= fold_build2 (MINUS_EXPR, type,
		       LOOP_VINFO_NITERSM1 (loop_vinfo), niters_prolog);
      bool new_var_p = false;
      niters = vect_build_loop_niters (loop_vinfo, &new_var_p);
      /* It's guaranteed that vector loop bound before vectorization is at
	 least VF, so set range information for newly generated var.  */
      if (new_var_p)
	set_range_info (niters, VR_RANGE,
			build_int_cst (type, vf), TYPE_MAX_VALUE (type));

      /* Prolog iterates at most bound_prolog times, latch iterates at
	 most bound_prolog - 1 times.  */
      record_niter_bound (prolog, bound_prolog - 1, false, true);
      delete_update_ssa ();
      adjust_vec_debug_stmts ();
      scev_reset ();
    }

  if (epilog_peeling)
    {
      e = single_exit (loop);
      if (!slpeel_can_duplicate_loop_p (loop, e))
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, loop_loc,
			   "loop can't be duplicated to exit edge.\n");
	  gcc_unreachable ();
	}
      /* Peel epilog and put it on exit edge of loop.  */
      epilog = slpeel_tree_duplicate_loop_to_edge_cfg (loop, scalar_loop, e);
      if (!epilog)
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, loop_loc,
			   "slpeel_tree_duplicate_loop_to_edge_cfg failed.\n");
	  gcc_unreachable ();
	}
      slpeel_update_phi_nodes_for_loops (loop_vinfo, loop, epilog, false);

      /* Scalar version loop may be preferred.  In this case, add guard
	 and skip to epilog.  Note this only happens when the number of
	 iterations of loop is unknown at compile time, otherwise this
	 won't be vectorized.  */
      if (skip_vector)
	{
	  /* Additional epilogue iteration is peeled if gap exists.  */
	  bool peel_for_gaps = LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo);
	  tree t = vect_gen_scalar_loop_niters (niters_prolog, prolog_peeling,
						bound_prolog,
						peel_for_gaps ? vf : vf - 1,
						th, &bound_scalar,
						check_profitability);
	  /* Build guard against NITERSM1 since NITERS may overflow.  */
	  guard_cond = fold_build2 (LT_EXPR, boolean_type_node, nitersm1, t);
	  guard_bb = anchor;
	  guard_to = split_edge (loop_preheader_edge (epilog));
	  guard_e = slpeel_add_loop_guard (guard_bb, guard_cond,
					   guard_to, guard_bb,
					   prob_vector.invert (),
					   irred_flag);
	  e = EDGE_PRED (guard_to, 0);
	  e = (e != guard_e ? e : EDGE_PRED (guard_to, 1));
	  slpeel_update_phi_nodes_for_guard1 (first_loop, epilog, guard_e, e);

	  /* Simply propagate profile info from guard_bb to guard_to which is
	     a merge point of control flow.  */
	  guard_to->frequency = guard_bb->frequency;
	  guard_to->count = guard_bb->count;
	  single_succ_edge (guard_to)->count = guard_to->count;
	  /* Scale probability of epilog loop back.
	     FIXME: We should avoid scaling down and back up.  Profile may
	     get lost if we scale down to 0.  */
	  int scale_up = REG_BR_PROB_BASE * REG_BR_PROB_BASE
			 / prob_vector.to_reg_br_prob_base ();
	  basic_block *bbs = get_loop_body (epilog);
	  scale_bbs_frequencies_int (bbs, epilog->num_nodes, scale_up,
				     REG_BR_PROB_BASE);
	  free (bbs);
	}

      basic_block bb_before_epilog = loop_preheader_edge (epilog)->src;
      tree niters_vector_mult_vf;
      /* If loop is peeled for non-zero constant times, now niters refers to
	 orig_niters - prolog_peeling, it won't overflow even the orig_niters
	 overflows.  */
      niters_no_overflow |= (prolog_peeling > 0);
      vect_gen_vector_loop_niters (loop_vinfo, niters,
				   niters_vector, niters_no_overflow);
      vect_gen_vector_loop_niters_mult_vf (loop_vinfo, *niters_vector,
					   &niters_vector_mult_vf);
      /* Update IVs of original loop as if they were advanced by
	 niters_vector_mult_vf steps.  */
      gcc_checking_assert (vect_can_advance_ivs_p (loop_vinfo));
      edge update_e = skip_vector ? e : loop_preheader_edge (epilog);
      vect_update_ivs_after_vectorizer (loop_vinfo, niters_vector_mult_vf,
					update_e);

      if (skip_epilog)
	{
	  guard_cond = fold_build2 (EQ_EXPR, boolean_type_node,
				    niters, niters_vector_mult_vf);
	  guard_bb = single_exit (loop)->dest;
	  guard_to = split_edge (single_exit (epilog));
	  guard_e = slpeel_add_loop_guard (guard_bb, guard_cond, guard_to,
					   skip_vector ? anchor : guard_bb,
					   prob_epilog.invert (),
					   irred_flag);
	  slpeel_update_phi_nodes_for_guard2 (loop, epilog, guard_e,
					      single_exit (epilog));
	  /* Only need to handle basic block before epilog loop if it's not
	     the guard_bb, which is the case when skip_vector is true.  */
	  if (guard_bb != bb_before_epilog)
	    {
	      prob_epilog = prob_vector * prob_epilog + prob_vector.invert ();

	      scale_bbs_frequencies (&bb_before_epilog, 1, prob_epilog);
	    }
	  scale_loop_profile (epilog, prob_epilog, bound);
	}
      else
	slpeel_update_phi_nodes_for_lcssa (epilog);

      bound = LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo) ? vf - 1 : vf - 2;
      /* We share epilog loop with scalar version loop.  */
      bound = MAX (bound, bound_scalar - 1);
      record_niter_bound (epilog, bound, false, true);

      delete_update_ssa ();
      adjust_vec_debug_stmts ();
      scev_reset ();
    }
  adjust_vec.release ();
  free_original_copy_tables ();

  return epilog;
}

/* Function vect_create_cond_for_niters_checks.

   Create a conditional expression that represents the run-time checks for
   loop's niter.  The loop is guaranteed to to terminate if the run-time
   checks hold.

   Input:
   COND_EXPR  - input conditional expression.  New conditions will be chained
		with logical AND operation.  If it is NULL, then the function
		is used to return the number of alias checks.
   LOOP_VINFO - field LOOP_VINFO_MAY_ALIAS_STMTS contains the list of ddrs
		to be checked.

   Output:
   COND_EXPR - conditional expression.

   The returned COND_EXPR is the conditional expression to be used in the
   if statement that controls which version of the loop gets executed at
   runtime.  */

static void
vect_create_cond_for_niters_checks (loop_vec_info loop_vinfo, tree *cond_expr)
{
  tree part_cond_expr = LOOP_VINFO_NITERS_ASSUMPTIONS (loop_vinfo);

  if (*cond_expr)
    *cond_expr = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
			      *cond_expr, part_cond_expr);
  else
    *cond_expr = part_cond_expr;
}

/* Set *COND_EXPR to a tree that is true when both the original *COND_EXPR
   and PART_COND_EXPR are true.  Treat a null *COND_EXPR as "true".  */

static void
chain_cond_expr (tree *cond_expr, tree part_cond_expr)
{
  if (*cond_expr)
    *cond_expr = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
			      *cond_expr, part_cond_expr);
  else
    *cond_expr = part_cond_expr;
}

/* Function vect_create_cond_for_align_checks.

   Create a conditional expression that represents the alignment checks for
   all of data references (array element references) whose alignment must be
   checked at runtime.

   Input:
   COND_EXPR  - input conditional expression.  New conditions will be chained
                with logical AND operation.
   LOOP_VINFO - two fields of the loop information are used.
                LOOP_VINFO_PTR_MASK is the mask used to check the alignment.
                LOOP_VINFO_MAY_MISALIGN_STMTS contains the refs to be checked.

   Output:
   COND_EXPR_STMT_LIST - statements needed to construct the conditional
                         expression.
   The returned value is the conditional expression to be used in the if
   statement that controls which version of the loop gets executed at runtime.

   The algorithm makes two assumptions:
     1) The number of bytes "n" in a vector is a power of 2.
     2) An address "a" is aligned if a%n is zero and that this
        test can be done as a&(n-1) == 0.  For example, for 16
        byte vectors the test is a&0xf == 0.  */

static void
vect_create_cond_for_align_checks (loop_vec_info loop_vinfo,
                                   tree *cond_expr,
				   gimple_seq *cond_expr_stmt_list)
{
  vec<gimple *> may_misalign_stmts
    = LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo);
  gimple *ref_stmt;
  int mask = LOOP_VINFO_PTR_MASK (loop_vinfo);
  tree mask_cst;
  unsigned int i;
  tree int_ptrsize_type;
  char tmp_name[20];
  tree or_tmp_name = NULL_TREE;
  tree and_tmp_name;
  gimple *and_stmt;
  tree ptrsize_zero;
  tree part_cond_expr;

  /* Check that mask is one less than a power of 2, i.e., mask is
     all zeros followed by all ones.  */
  gcc_assert ((mask != 0) && ((mask & (mask+1)) == 0));

  int_ptrsize_type = signed_type_for (ptr_type_node);

  /* Create expression (mask & (dr_1 || ... || dr_n)) where dr_i is the address
     of the first vector of the i'th data reference. */

  FOR_EACH_VEC_ELT (may_misalign_stmts, i, ref_stmt)
    {
      gimple_seq new_stmt_list = NULL;
      tree addr_base;
      tree addr_tmp_name;
      tree new_or_tmp_name;
      gimple *addr_stmt, *or_stmt;
      stmt_vec_info stmt_vinfo = vinfo_for_stmt (ref_stmt);
      tree vectype = STMT_VINFO_VECTYPE (stmt_vinfo);
      bool negative = tree_int_cst_compare
	(DR_STEP (STMT_VINFO_DATA_REF (stmt_vinfo)), size_zero_node) < 0;
      tree offset = negative
	? size_int (-TYPE_VECTOR_SUBPARTS (vectype) + 1) : size_zero_node;

      /* create: addr_tmp = (int)(address_of_first_vector) */
      addr_base =
	vect_create_addr_base_for_vector_ref (ref_stmt, &new_stmt_list,
					      offset);
      if (new_stmt_list != NULL)
	gimple_seq_add_seq (cond_expr_stmt_list, new_stmt_list);

      sprintf (tmp_name, "addr2int%d", i);
      addr_tmp_name = make_temp_ssa_name (int_ptrsize_type, NULL, tmp_name);
      addr_stmt = gimple_build_assign (addr_tmp_name, NOP_EXPR, addr_base);
      gimple_seq_add_stmt (cond_expr_stmt_list, addr_stmt);

      /* The addresses are OR together.  */

      if (or_tmp_name != NULL_TREE)
        {
          /* create: or_tmp = or_tmp | addr_tmp */
          sprintf (tmp_name, "orptrs%d", i);
	  new_or_tmp_name = make_temp_ssa_name (int_ptrsize_type, NULL, tmp_name);
	  or_stmt = gimple_build_assign (new_or_tmp_name, BIT_IOR_EXPR,
					 or_tmp_name, addr_tmp_name);
	  gimple_seq_add_stmt (cond_expr_stmt_list, or_stmt);
          or_tmp_name = new_or_tmp_name;
        }
      else
        or_tmp_name = addr_tmp_name;

    } /* end for i */

  mask_cst = build_int_cst (int_ptrsize_type, mask);

  /* create: and_tmp = or_tmp & mask  */
  and_tmp_name = make_temp_ssa_name (int_ptrsize_type, NULL, "andmask");

  and_stmt = gimple_build_assign (and_tmp_name, BIT_AND_EXPR,
				  or_tmp_name, mask_cst);
  gimple_seq_add_stmt (cond_expr_stmt_list, and_stmt);

  /* Make and_tmp the left operand of the conditional test against zero.
     if and_tmp has a nonzero bit then some address is unaligned.  */
  ptrsize_zero = build_int_cst (int_ptrsize_type, 0);
  part_cond_expr = fold_build2 (EQ_EXPR, boolean_type_node,
				and_tmp_name, ptrsize_zero);
  chain_cond_expr (cond_expr, part_cond_expr);
}

/* If LOOP_VINFO_CHECK_UNEQUAL_ADDRS contains <A1, B1>, ..., <An, Bn>,
   create a tree representation of: (&A1 != &B1) && ... && (&An != &Bn).
   Set *COND_EXPR to a tree that is true when both the original *COND_EXPR
   and this new condition are true.  Treat a null *COND_EXPR as "true".  */

static void
vect_create_cond_for_unequal_addrs (loop_vec_info loop_vinfo, tree *cond_expr)
{
  vec<vec_object_pair> pairs = LOOP_VINFO_CHECK_UNEQUAL_ADDRS (loop_vinfo);
  unsigned int i;
  vec_object_pair *pair;
  FOR_EACH_VEC_ELT (pairs, i, pair)
    {
      tree addr1 = build_fold_addr_expr (pair->first);
      tree addr2 = build_fold_addr_expr (pair->second);
      tree part_cond_expr = fold_build2 (NE_EXPR, boolean_type_node,
					 addr1, addr2);
      chain_cond_expr (cond_expr, part_cond_expr);
    }
}

/* Function vect_create_cond_for_alias_checks.

   Create a conditional expression that represents the run-time checks for
   overlapping of address ranges represented by a list of data references
   relations passed as input.

   Input:
   COND_EXPR  - input conditional expression.  New conditions will be chained
                with logical AND operation.  If it is NULL, then the function
                is used to return the number of alias checks.
   LOOP_VINFO - field LOOP_VINFO_MAY_ALIAS_STMTS contains the list of ddrs
	        to be checked.

   Output:
   COND_EXPR - conditional expression.

   The returned COND_EXPR is the conditional expression to be used in the if
   statement that controls which version of the loop gets executed at runtime.
*/

void
vect_create_cond_for_alias_checks (loop_vec_info loop_vinfo, tree * cond_expr)
{
  vec<dr_with_seg_len_pair_t> comp_alias_ddrs =
    LOOP_VINFO_COMP_ALIAS_DDRS (loop_vinfo);

  if (comp_alias_ddrs.is_empty ())
    return;

  create_runtime_alias_checks (LOOP_VINFO_LOOP (loop_vinfo),
			       &comp_alias_ddrs, cond_expr);
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "created %u versioning for alias checks.\n",
		     comp_alias_ddrs.length ());
}


/* Function vect_loop_versioning.

   If the loop has data references that may or may not be aligned or/and
   has data reference relations whose independence was not proven then
   two versions of the loop need to be generated, one which is vectorized
   and one which isn't.  A test is then generated to control which of the
   loops is executed.  The test checks for the alignment of all of the
   data references that may or may not be aligned.  An additional
   sequence of runtime tests is generated for each pairs of DDRs whose
   independence was not proven.  The vectorized version of loop is
   executed only if both alias and alignment tests are passed.

   The test generated to check which version of loop is executed
   is modified to also check for profitability as indicated by the
   cost model threshold TH.

   The versioning precondition(s) are placed in *COND_EXPR and
   *COND_EXPR_STMT_LIST.  */

void
vect_loop_versioning (loop_vec_info loop_vinfo,
		      unsigned int th, bool check_profitability)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo), *nloop;
  struct loop *scalar_loop = LOOP_VINFO_SCALAR_LOOP (loop_vinfo);
  basic_block condition_bb;
  gphi_iterator gsi;
  gimple_stmt_iterator cond_exp_gsi;
  basic_block merge_bb;
  basic_block new_exit_bb;
  edge new_exit_e, e;
  gphi *orig_phi, *new_phi;
  tree cond_expr = NULL_TREE;
  gimple_seq cond_expr_stmt_list = NULL;
  tree arg;
  profile_probability prob = profile_probability::likely ();
  gimple_seq gimplify_stmt_list = NULL;
  tree scalar_loop_iters = LOOP_VINFO_NITERSM1 (loop_vinfo);
  bool version_align = LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo);
  bool version_alias = LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo);
  bool version_niter = LOOP_REQUIRES_VERSIONING_FOR_NITERS (loop_vinfo);

  if (check_profitability)
    cond_expr = fold_build2 (GE_EXPR, boolean_type_node, scalar_loop_iters,
			     build_int_cst (TREE_TYPE (scalar_loop_iters),
					    th - 1));

  if (version_niter)
    vect_create_cond_for_niters_checks (loop_vinfo, &cond_expr);

  if (cond_expr)
    cond_expr = force_gimple_operand_1 (cond_expr, &cond_expr_stmt_list,
					is_gimple_condexpr, NULL_TREE);

  if (version_align)
    vect_create_cond_for_align_checks (loop_vinfo, &cond_expr,
				       &cond_expr_stmt_list);

  if (version_alias)
    {
      vect_create_cond_for_unequal_addrs (loop_vinfo, &cond_expr);
      vect_create_cond_for_alias_checks (loop_vinfo, &cond_expr);
    }

  cond_expr = force_gimple_operand_1 (cond_expr, &gimplify_stmt_list,
				      is_gimple_condexpr, NULL_TREE);
  gimple_seq_add_seq (&cond_expr_stmt_list, gimplify_stmt_list);

  initialize_original_copy_tables ();
  if (scalar_loop)
    {
      edge scalar_e;
      basic_block preheader, scalar_preheader;

      /* We don't want to scale SCALAR_LOOP's frequencies, we need to
	 scale LOOP's frequencies instead.  */
      nloop = loop_version (scalar_loop, cond_expr, &condition_bb,
			    prob, prob.invert (), prob, prob.invert (), true);
      scale_loop_frequencies (loop, prob);
      /* CONDITION_BB was created above SCALAR_LOOP's preheader,
	 while we need to move it above LOOP's preheader.  */
      e = loop_preheader_edge (loop);
      scalar_e = loop_preheader_edge (scalar_loop);
      gcc_assert (empty_block_p (e->src)
		  && single_pred_p (e->src));
      gcc_assert (empty_block_p (scalar_e->src)
		  && single_pred_p (scalar_e->src));
      gcc_assert (single_pred_p (condition_bb));
      preheader = e->src;
      scalar_preheader = scalar_e->src;
      scalar_e = find_edge (condition_bb, scalar_preheader);
      e = single_pred_edge (preheader);
      redirect_edge_and_branch_force (single_pred_edge (condition_bb),
				      scalar_preheader);
      redirect_edge_and_branch_force (scalar_e, preheader);
      redirect_edge_and_branch_force (e, condition_bb);
      set_immediate_dominator (CDI_DOMINATORS, condition_bb,
			       single_pred (condition_bb));
      set_immediate_dominator (CDI_DOMINATORS, scalar_preheader,
			       single_pred (scalar_preheader));
      set_immediate_dominator (CDI_DOMINATORS, preheader,
			       condition_bb);
    }
  else
    nloop = loop_version (loop, cond_expr, &condition_bb,
			  prob, prob.invert (), prob, prob.invert (), true);

  if (version_niter)
    {
      /* The versioned loop could be infinite, we need to clear existing
	 niter information which is copied from the original loop.  */
      gcc_assert (loop_constraint_set_p (loop, LOOP_C_FINITE));
      vect_free_loop_info_assumptions (nloop);
      /* And set constraint LOOP_C_INFINITE for niter analyzer.  */
      loop_constraint_set (loop, LOOP_C_INFINITE);
    }

  if (LOCATION_LOCUS (vect_location) != UNKNOWN_LOCATION
      && dump_enabled_p ())
    {
      if (version_alias)
        dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
                         "loop versioned for vectorization because of "
			 "possible aliasing\n");
      if (version_align)
        dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
                         "loop versioned for vectorization to enhance "
			 "alignment\n");

    }
  free_original_copy_tables ();

  /* Loop versioning violates an assumption we try to maintain during
     vectorization - that the loop exit block has a single predecessor.
     After versioning, the exit block of both loop versions is the same
     basic block (i.e. it has two predecessors). Just in order to simplify
     following transformations in the vectorizer, we fix this situation
     here by adding a new (empty) block on the exit-edge of the loop,
     with the proper loop-exit phis to maintain loop-closed-form.
     If loop versioning wasn't done from loop, but scalar_loop instead,
     merge_bb will have already just a single successor.  */

  merge_bb = single_exit (loop)->dest;
  if (scalar_loop == NULL || EDGE_COUNT (merge_bb->preds) >= 2)
    {
      gcc_assert (EDGE_COUNT (merge_bb->preds) >= 2);
      new_exit_bb = split_edge (single_exit (loop));
      new_exit_e = single_exit (loop);
      e = EDGE_SUCC (new_exit_bb, 0);

      for (gsi = gsi_start_phis (merge_bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  tree new_res;
	  orig_phi = gsi.phi ();
	  new_res = copy_ssa_name (PHI_RESULT (orig_phi));
	  new_phi = create_phi_node (new_res, new_exit_bb);
	  arg = PHI_ARG_DEF_FROM_EDGE (orig_phi, e);
	  add_phi_arg (new_phi, arg, new_exit_e,
		       gimple_phi_arg_location_from_edge (orig_phi, e));
	  adjust_phi_and_debug_stmts (orig_phi, e, PHI_RESULT (new_phi));
	}
    }

  /* End loop-exit-fixes after versioning.  */

  if (cond_expr_stmt_list)
    {
      cond_exp_gsi = gsi_last_bb (condition_bb);
      gsi_insert_seq_before (&cond_exp_gsi, cond_expr_stmt_list,
			     GSI_SAME_STMT);
    }
  update_ssa (TODO_update_ssa);
}
