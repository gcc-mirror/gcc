/* Vectorizer Specific Loop Manipulations
   Copyright (C) 2003-2016 Free Software Foundation, Inc.
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


/* Renames the variables in basic block BB.  Allow renaming  of PHI argumnets
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
      if (!flow_bb_inside_loop_p (loop, e->src)
	  && (!rename_from_outer_loop || e->src != outer_loop->header))
	continue;
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

  adjust_vec.release ();
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


/* Update PHI nodes for a guard of the LOOP.

   Input:
   - LOOP, GUARD_EDGE: LOOP is a loop for which we added guard code that
        controls whether LOOP is to be executed.  GUARD_EDGE is the edge that
        originates from the guard-bb, skips LOOP and reaches the (unique) exit
        bb of LOOP.  This loop-exit-bb is an empty bb with one successor.
        We denote this bb NEW_MERGE_BB because before the guard code was added
        it had a single predecessor (the LOOP header), and now it became a merge
        point of two paths - the path that ends with the LOOP exit-edge, and
        the path that ends with GUARD_EDGE.
   - NEW_EXIT_BB: New basic block that is added by this function between LOOP
        and NEW_MERGE_BB. It is used to place loop-closed-ssa-form exit-phis.

   ===> The CFG before the guard-code was added:
        LOOP_header_bb:
          loop_body
          if (exit_loop) goto update_bb
          else           goto LOOP_header_bb
        update_bb:

   ==> The CFG after the guard-code was added:
        guard_bb:
          if (LOOP_guard_condition) goto new_merge_bb
          else                      goto LOOP_header_bb
        LOOP_header_bb:
          loop_body
          if (exit_loop_condition) goto new_merge_bb
          else                     goto LOOP_header_bb
        new_merge_bb:
          goto update_bb
        update_bb:

   ==> The CFG after this function:
        guard_bb:
          if (LOOP_guard_condition) goto new_merge_bb
          else                      goto LOOP_header_bb
        LOOP_header_bb:
          loop_body
          if (exit_loop_condition) goto new_exit_bb
          else                     goto LOOP_header_bb
        new_exit_bb:
        new_merge_bb:
          goto update_bb
        update_bb:

   This function:
   1. creates and updates the relevant phi nodes to account for the new
      incoming edge (GUARD_EDGE) into NEW_MERGE_BB. This involves:
      1.1. Create phi nodes at NEW_MERGE_BB.
      1.2. Update the phi nodes at the successor of NEW_MERGE_BB (denoted
           UPDATE_BB).  UPDATE_BB was the exit-bb of LOOP before NEW_MERGE_BB
   2. preserves loop-closed-ssa-form by creating the required phi nodes
      at the exit of LOOP (i.e, in NEW_EXIT_BB).

   There are two flavors to this function:

   slpeel_update_phi_nodes_for_guard1:
     Here the guard controls whether we enter or skip LOOP, where LOOP is a
     prolog_loop (loop1 below), and the new phis created in NEW_MERGE_BB are
     for variables that have phis in the loop header.

   slpeel_update_phi_nodes_for_guard2:
     Here the guard controls whether we enter or skip LOOP, where LOOP is an
     epilog_loop (loop2 below), and the new phis created in NEW_MERGE_BB are
     for variables that have phis in the loop exit.

   I.E., the overall structure is:

        loop1_preheader_bb:
                guard1 (goto loop1/merge1_bb)
        loop1
        loop1_exit_bb:
                guard2 (goto merge1_bb/merge2_bb)
        merge1_bb
        loop2
        loop2_exit_bb
        merge2_bb
        next_bb

   slpeel_update_phi_nodes_for_guard1 takes care of creating phis in
   loop1_exit_bb and merge1_bb. These are entry phis (phis for the vars
   that have phis in loop1->header).

   slpeel_update_phi_nodes_for_guard2 takes care of creating phis in
   loop2_exit_bb and merge2_bb. These are exit phis (phis for the vars
   that have phis in next_bb). It also adds some of these phis to
   loop1_exit_bb.

   slpeel_update_phi_nodes_for_guard1 is always called before
   slpeel_update_phi_nodes_for_guard2. They are both needed in order
   to create correct data-flow and loop-closed-ssa-form.

   Generally slpeel_update_phi_nodes_for_guard1 creates phis for variables
   that change between iterations of a loop (and therefore have a phi-node
   at the loop entry), whereas slpeel_update_phi_nodes_for_guard2 creates
   phis for variables that are used out of the loop (and therefore have
   loop-closed exit phis). Some variables may be both updated between
   iterations and used after the loop. This is why in loop1_exit_bb we
   may need both entry_phis (created by slpeel_update_phi_nodes_for_guard1)
   and exit phis (created by slpeel_update_phi_nodes_for_guard2).

   - IS_NEW_LOOP: if IS_NEW_LOOP is true, then LOOP is a newly created copy of
     an original loop. i.e., we have:

           orig_loop
           guard_bb (goto LOOP/new_merge)
           new_loop <-- LOOP
           new_exit
           new_merge
           next_bb

     If IS_NEW_LOOP is false, then LOOP is an original loop, in which case we
     have:

           new_loop
           guard_bb (goto LOOP/new_merge)
           orig_loop <-- LOOP
           new_exit
           new_merge
           next_bb

     The SSA names defined in the original loop have a current
     reaching definition that records the corresponding new ssa-name
     used in the new duplicated loop copy.
  */

/* Function slpeel_update_phi_nodes_for_guard1

   Input:
   - GUARD_EDGE, LOOP, IS_NEW_LOOP, NEW_EXIT_BB - as explained above.
   - DEFS - a bitmap of ssa names to mark new names for which we recorded
            information.

   In the context of the overall structure, we have:

        loop1_preheader_bb:
                guard1 (goto loop1/merge1_bb)
LOOP->  loop1
        loop1_exit_bb:
                guard2 (goto merge1_bb/merge2_bb)
        merge1_bb
        loop2
        loop2_exit_bb
        merge2_bb
        next_bb

   For each name updated between loop iterations (i.e - for each name that has
   an entry (loop-header) phi in LOOP) we create a new phi in:
   1. merge1_bb (to account for the edge from guard1)
   2. loop1_exit_bb (an exit-phi to keep LOOP in loop-closed form)
*/

static void
slpeel_update_phi_nodes_for_guard1 (edge guard_edge, struct loop *loop,
                                    bool is_new_loop, basic_block *new_exit_bb)
{
  gphi *orig_phi, *new_phi;
  gphi *update_phi, *update_phi2;
  tree guard_arg, loop_arg;
  basic_block new_merge_bb = guard_edge->dest;
  edge e = EDGE_SUCC (new_merge_bb, 0);
  basic_block update_bb = e->dest;
  basic_block orig_bb = loop->header;
  edge new_exit_e;
  tree current_new_name;
  gphi_iterator gsi_orig, gsi_update;

  /* Create new bb between loop and new_merge_bb.  */
  *new_exit_bb = split_edge (single_exit (loop));

  new_exit_e = EDGE_SUCC (*new_exit_bb, 0);

  for (gsi_orig = gsi_start_phis (orig_bb),
       gsi_update = gsi_start_phis (update_bb);
       !gsi_end_p (gsi_orig) && !gsi_end_p (gsi_update);
       gsi_next (&gsi_orig), gsi_next (&gsi_update))
    {
      source_location loop_locus, guard_locus;
      tree new_res;
      orig_phi = gsi_orig.phi ();
      update_phi = gsi_update.phi ();

      /** 1. Handle new-merge-point phis  **/

      /* 1.1. Generate new phi node in NEW_MERGE_BB:  */
      new_res = copy_ssa_name (PHI_RESULT (orig_phi));
      new_phi = create_phi_node (new_res, new_merge_bb);

      /* 1.2. NEW_MERGE_BB has two incoming edges: GUARD_EDGE and the exit-edge
            of LOOP. Set the two phi args in NEW_PHI for these edges:  */
      loop_arg = PHI_ARG_DEF_FROM_EDGE (orig_phi, EDGE_SUCC (loop->latch, 0));
      loop_locus = gimple_phi_arg_location_from_edge (orig_phi,
						      EDGE_SUCC (loop->latch,
								 0));
      guard_arg = PHI_ARG_DEF_FROM_EDGE (orig_phi, loop_preheader_edge (loop));
      guard_locus
	= gimple_phi_arg_location_from_edge (orig_phi,
					     loop_preheader_edge (loop));

      add_phi_arg (new_phi, loop_arg, new_exit_e, loop_locus);
      add_phi_arg (new_phi, guard_arg, guard_edge, guard_locus);

      /* 1.3. Update phi in successor block.  */
      gcc_assert (PHI_ARG_DEF_FROM_EDGE (update_phi, e) == loop_arg
                  || PHI_ARG_DEF_FROM_EDGE (update_phi, e) == guard_arg);
      adjust_phi_and_debug_stmts (update_phi, e, PHI_RESULT (new_phi));
      update_phi2 = new_phi;


      /** 2. Handle loop-closed-ssa-form phis  **/

      if (virtual_operand_p (PHI_RESULT (orig_phi)))
	continue;

      /* 2.1. Generate new phi node in NEW_EXIT_BB:  */
      new_res = copy_ssa_name (PHI_RESULT (orig_phi));
      new_phi = create_phi_node (new_res, *new_exit_bb);

      /* 2.2. NEW_EXIT_BB has one incoming edge: the exit-edge of the loop.  */
      add_phi_arg (new_phi, loop_arg, single_exit (loop), loop_locus);

      /* 2.3. Update phi in successor of NEW_EXIT_BB:  */
      gcc_assert (PHI_ARG_DEF_FROM_EDGE (update_phi2, new_exit_e) == loop_arg);
      adjust_phi_and_debug_stmts (update_phi2, new_exit_e,
				  PHI_RESULT (new_phi));

      /* 2.4. Record the newly created name with set_current_def.
         We want to find a name such that
                name = get_current_def (orig_loop_name)
         and to set its current definition as follows:
                set_current_def (name, new_phi_name)

         If LOOP is a new loop then loop_arg is already the name we're
         looking for. If LOOP is the original loop, then loop_arg is
         the orig_loop_name and the relevant name is recorded in its
         current reaching definition.  */
      if (is_new_loop)
        current_new_name = loop_arg;
      else
        {
          current_new_name = get_current_def (loop_arg);
	  /* current_def is not available only if the variable does not
	     change inside the loop, in which case we also don't care
	     about recording a current_def for it because we won't be
	     trying to create loop-exit-phis for it.  */
	  if (!current_new_name)
	    continue;
        }
      tree new_name = get_current_def (current_new_name);
      /* Because of peeled_chrec optimization it is possible that we have
	 set this earlier.  Verify the PHI has the same value.  */
      if (new_name)
	{
	  gimple *phi = SSA_NAME_DEF_STMT (new_name);
	  gcc_assert (gimple_code (phi) == GIMPLE_PHI
		      && gimple_bb (phi) == *new_exit_bb
		      && (PHI_ARG_DEF_FROM_EDGE (phi, single_exit (loop))
			  == loop_arg));
	  continue;
	}

      set_current_def (current_new_name, PHI_RESULT (new_phi));
    }
}


/* Function slpeel_update_phi_nodes_for_guard2

   Input:
   - GUARD_EDGE, LOOP, IS_NEW_LOOP, NEW_EXIT_BB - as explained above.

   In the context of the overall structure, we have:

        loop1_preheader_bb:
                guard1 (goto loop1/merge1_bb)
        loop1
        loop1_exit_bb:
                guard2 (goto merge1_bb/merge2_bb)
        merge1_bb
LOOP->  loop2
        loop2_exit_bb
        merge2_bb
        next_bb

   For each name used out side the loop (i.e - for each name that has an exit
   phi in next_bb) we create a new phi in:
   1. merge2_bb (to account for the edge from guard_bb)
   2. loop2_exit_bb (an exit-phi to keep LOOP in loop-closed form)
   3. guard2 bb (an exit phi to keep the preceding loop in loop-closed form),
      if needed (if it wasn't handled by slpeel_update_phis_nodes_for_phi1).
*/

static void
slpeel_update_phi_nodes_for_guard2 (edge guard_edge, struct loop *loop,
                                    bool is_new_loop, basic_block *new_exit_bb)
{
  gphi *orig_phi, *new_phi;
  gphi *update_phi, *update_phi2;
  tree guard_arg, loop_arg;
  basic_block new_merge_bb = guard_edge->dest;
  edge e = EDGE_SUCC (new_merge_bb, 0);
  basic_block update_bb = e->dest;
  edge new_exit_e;
  tree orig_def, orig_def_new_name;
  tree new_name, new_name2;
  tree arg;
  gphi_iterator gsi;

  /* Create new bb between loop and new_merge_bb.  */
  *new_exit_bb = split_edge (single_exit (loop));

  new_exit_e = EDGE_SUCC (*new_exit_bb, 0);

  for (gsi = gsi_start_phis (update_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      tree new_res;
      update_phi = gsi.phi ();
      orig_phi = update_phi;
      orig_def = PHI_ARG_DEF_FROM_EDGE (orig_phi, e);
      /* This loop-closed-phi actually doesn't represent a use
         out of the loop - the phi arg is a constant.  */
      if (TREE_CODE (orig_def) != SSA_NAME)
        continue;
      orig_def_new_name = get_current_def (orig_def);
      arg = NULL_TREE;

      /** 1. Handle new-merge-point phis  **/

      /* 1.1. Generate new phi node in NEW_MERGE_BB:  */
      new_res = copy_ssa_name (PHI_RESULT (orig_phi));
      new_phi = create_phi_node (new_res, new_merge_bb);

      /* 1.2. NEW_MERGE_BB has two incoming edges: GUARD_EDGE and the exit-edge
            of LOOP. Set the two PHI args in NEW_PHI for these edges:  */
      new_name = orig_def;
      new_name2 = NULL_TREE;
      if (orig_def_new_name)
        {
          new_name = orig_def_new_name;
	  /* Some variables have both loop-entry-phis and loop-exit-phis.
	     Such variables were given yet newer names by phis placed in
	     guard_bb by slpeel_update_phi_nodes_for_guard1. I.e:
	     new_name2 = get_current_def (get_current_def (orig_name)).  */
          new_name2 = get_current_def (new_name);
        }

      if (is_new_loop)
        {
          guard_arg = orig_def;
          loop_arg = new_name;
        }
      else
        {
          guard_arg = new_name;
          loop_arg = orig_def;
        }
      if (new_name2)
        guard_arg = new_name2;

      add_phi_arg (new_phi, loop_arg, new_exit_e, UNKNOWN_LOCATION);
      add_phi_arg (new_phi, guard_arg, guard_edge, UNKNOWN_LOCATION);

      /* 1.3. Update phi in successor block.  */
      gcc_assert (PHI_ARG_DEF_FROM_EDGE (update_phi, e) == orig_def);
      adjust_phi_and_debug_stmts (update_phi, e, PHI_RESULT (new_phi));
      update_phi2 = new_phi;


      /** 2. Handle loop-closed-ssa-form phis  **/

      /* 2.1. Generate new phi node in NEW_EXIT_BB:  */
      new_res = copy_ssa_name (PHI_RESULT (orig_phi));
      new_phi = create_phi_node (new_res, *new_exit_bb);

      /* 2.2. NEW_EXIT_BB has one incoming edge: the exit-edge of the loop.  */
      add_phi_arg (new_phi, loop_arg, single_exit (loop), UNKNOWN_LOCATION);

      /* 2.3. Update phi in successor of NEW_EXIT_BB:  */
      gcc_assert (PHI_ARG_DEF_FROM_EDGE (update_phi2, new_exit_e) == loop_arg);
      adjust_phi_and_debug_stmts (update_phi2, new_exit_e,
				  PHI_RESULT (new_phi));


      /** 3. Handle loop-closed-ssa-form phis for first loop  **/

      /* 3.1. Find the relevant names that need an exit-phi in
	 GUARD_BB, i.e. names for which
	 slpeel_update_phi_nodes_for_guard1 had not already created a
	 phi node. This is the case for names that are used outside
	 the loop (and therefore need an exit phi) but are not updated
	 across loop iterations (and therefore don't have a
	 loop-header-phi).

	 slpeel_update_phi_nodes_for_guard1 is responsible for
	 creating loop-exit phis in GUARD_BB for names that have a
	 loop-header-phi.  When such a phi is created we also record
	 the new name in its current definition.  If this new name
	 exists, then guard_arg was set to this new name (see 1.2
	 above).  Therefore, if guard_arg is not this new name, this
	 is an indication that an exit-phi in GUARD_BB was not yet
	 created, so we take care of it here.  */
      if (guard_arg == new_name2)
	continue;
      arg = guard_arg;

      /* 3.2. Generate new phi node in GUARD_BB:  */
      new_res = copy_ssa_name (PHI_RESULT (orig_phi));
      new_phi = create_phi_node (new_res, guard_edge->src);

      /* 3.3. GUARD_BB has one incoming edge:  */
      gcc_assert (EDGE_COUNT (guard_edge->src->preds) == 1);
      add_phi_arg (new_phi, arg, EDGE_PRED (guard_edge->src, 0),
		   UNKNOWN_LOCATION);

      /* 3.4. Update phi in successor of GUARD_BB:  */
      gcc_assert (PHI_ARG_DEF_FROM_EDGE (update_phi2, guard_edge)
                                                                == guard_arg);
      adjust_phi_and_debug_stmts (update_phi2, guard_edge,
				  PHI_RESULT (new_phi));
    }
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
  loop->nb_iterations = niters;
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
      if (TREE_CODE (from_arg) != SSA_NAME)
	{	
	  gsi_next (&gsi_from);
	  continue;
	}
      tree to_arg = PHI_ARG_DEF_FROM_EDGE (to_phi, to);
      if (TREE_CODE (to_arg) != SSA_NAME)
	{	
	  gsi_next (&gsi_to);
	  continue;
	}
      if (get_current_def (to_arg) == NULL_TREE)
	set_current_def (to_arg, get_current_def (from_arg));
      gsi_next (&gsi_from);
      gsi_next (&gsi_to);
    }
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
  basic_block *new_bbs, *bbs;
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
  get_loop_body_with_size (scalar_loop, bbs, scalar_loop->num_nodes);
  /* Allow duplication of outer loops.  */
  if (scalar_loop->inner)
    duplicate_outer_loop = true;
  /* Check whether duplication is possible.  */
  if (!can_copy_bbs_p (bbs, scalar_loop->num_nodes))
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
  bbs[scalar_loop->num_nodes] = preheader;
  new_bbs = XNEWVEC (basic_block, scalar_loop->num_nodes + 1);

  exit = single_exit (scalar_loop);
  copy_bbs (bbs, scalar_loop->num_nodes + 1, new_bbs,
	    &exit, 1, &new_exit, NULL,
	    e->src, true);
  exit = single_exit (loop);
  basic_block new_preheader = new_bbs[scalar_loop->num_nodes];

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


/* Given the condition statement COND, put it as the last statement
   of GUARD_BB; EXIT_BB is the basic block to skip the loop;
   Assumes that this is the single exit of the guarded loop.
   Returns the skip edge, inserts new stmts on the COND_EXPR_STMT_LIST.  */

static edge
slpeel_add_loop_guard (basic_block guard_bb, tree cond,
		       gimple_seq cond_expr_stmt_list,
		       basic_block exit_bb, basic_block dom_bb,
		       int probability)
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
    gimple_seq_add_seq (&cond_expr_stmt_list, gimplify_stmt_list);
  cond_stmt = gimple_build_cond_from_tree (cond, NULL_TREE, NULL_TREE);
  if (cond_expr_stmt_list)
    gsi_insert_seq_after (&gsi, cond_expr_stmt_list, GSI_NEW_STMT);

  gsi = gsi_last_bb (guard_bb);
  gsi_insert_after (&gsi, cond_stmt, GSI_NEW_STMT);

  /* Add new edge to connect guard block to the merge/loop-exit block.  */
  new_e = make_edge (guard_bb, exit_bb, EDGE_TRUE_VALUE);

  new_e->count = guard_bb->count;
  new_e->probability = probability;
  new_e->count = apply_probability (enter_e->count, probability);
  enter_e->count -= new_e->count;
  enter_e->probability = inverse_probability (probability);
  set_immediate_dominator (CDI_DOMINATORS, exit_bb, dom_bb);
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

static void
slpeel_checking_verify_cfg_after_peeling (struct loop *first_loop,
					  struct loop *second_loop)
{
  if (!flag_checking)
    return;

  basic_block loop1_exit_bb = single_exit (first_loop)->dest;
  basic_block loop2_entry_bb = loop_preheader_edge (second_loop)->src;
  basic_block loop1_entry_bb = loop_preheader_edge (first_loop)->src;

  /* A guard that controls whether the second_loop is to be executed or skipped
     is placed in first_loop->exit.  first_loop->exit therefore has two
     successors - one is the preheader of second_loop, and the other is a bb
     after second_loop.
   */
  gcc_assert (EDGE_COUNT (loop1_exit_bb->succs) == 2);

  /* 1. Verify that one of the successors of first_loop->exit is the preheader
        of second_loop.  */

  /* The preheader of new_loop is expected to have two predecessors:
     first_loop->exit and the block that precedes first_loop.  */

  gcc_assert (EDGE_COUNT (loop2_entry_bb->preds) == 2
              && ((EDGE_PRED (loop2_entry_bb, 0)->src == loop1_exit_bb
                   && EDGE_PRED (loop2_entry_bb, 1)->src == loop1_entry_bb)
               || (EDGE_PRED (loop2_entry_bb, 1)->src ==  loop1_exit_bb
                   && EDGE_PRED (loop2_entry_bb, 0)->src == loop1_entry_bb)));

  /* Verify that the other successor of first_loop->exit is after the
     second_loop.  */
  /* TODO */
}

/* If the run time cost model check determines that vectorization is
   not profitable and hence scalar loop should be generated then set
   FIRST_NITERS to prologue peeled iterations. This will allow all the
   iterations to be executed in the prologue peeled scalar loop.  */

static void
set_prologue_iterations (basic_block bb_before_first_loop,
			 tree *first_niters,
			 struct loop *loop,
			 unsigned int th,
			 int probability)
{
  edge e;
  basic_block cond_bb, then_bb;
  tree var, prologue_after_cost_adjust_name;
  gimple_stmt_iterator gsi;
  gphi *newphi;
  edge e_true, e_false, e_fallthru;
  gcond *cond_stmt;
  gimple_seq stmts = NULL;
  tree cost_pre_condition = NULL_TREE;
  tree scalar_loop_iters =
    unshare_expr (LOOP_VINFO_NITERS_UNCHANGED (loop_vec_info_for_loop (loop)));

  e = single_pred_edge (bb_before_first_loop);
  cond_bb = split_edge (e);

  e = single_pred_edge (bb_before_first_loop);
  then_bb = split_edge (e);
  set_immediate_dominator (CDI_DOMINATORS, then_bb, cond_bb);

  e_false = make_single_succ_edge (cond_bb, bb_before_first_loop,
				   EDGE_FALSE_VALUE);
  set_immediate_dominator (CDI_DOMINATORS, bb_before_first_loop, cond_bb);

  e_true = EDGE_PRED (then_bb, 0);
  e_true->flags &= ~EDGE_FALLTHRU;
  e_true->flags |= EDGE_TRUE_VALUE;

  e_true->probability = probability;
  e_false->probability = inverse_probability (probability);
  e_true->count = apply_probability (cond_bb->count, probability);
  e_false->count = cond_bb->count - e_true->count;
  then_bb->frequency = EDGE_FREQUENCY (e_true);
  then_bb->count = e_true->count;

  e_fallthru = EDGE_SUCC (then_bb, 0);
  e_fallthru->count = then_bb->count;

  gsi = gsi_last_bb (cond_bb);
  cost_pre_condition =
    fold_build2 (LE_EXPR, boolean_type_node, scalar_loop_iters,
	         build_int_cst (TREE_TYPE (scalar_loop_iters), th));
  cost_pre_condition =
    force_gimple_operand_gsi_1 (&gsi, cost_pre_condition, is_gimple_condexpr,
				NULL_TREE, false, GSI_CONTINUE_LINKING);
  cond_stmt = gimple_build_cond_from_tree (cost_pre_condition,
					   NULL_TREE, NULL_TREE);
  gsi_insert_after (&gsi, cond_stmt, GSI_NEW_STMT);

  var = create_tmp_var (TREE_TYPE (scalar_loop_iters),
			"prologue_after_cost_adjust");
  prologue_after_cost_adjust_name =
    force_gimple_operand (scalar_loop_iters, &stmts, false, var);

  gsi = gsi_last_bb (then_bb);
  if (stmts)
    gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);

  newphi = create_phi_node (var, bb_before_first_loop);
  add_phi_arg (newphi, prologue_after_cost_adjust_name, e_fallthru,
	       UNKNOWN_LOCATION);
  add_phi_arg (newphi, *first_niters, e_false, UNKNOWN_LOCATION);

  *first_niters = PHI_RESULT (newphi);
}

/* Function slpeel_tree_peel_loop_to_edge.

   Peel the first (last) iterations of LOOP into a new prolog (epilog) loop
   that is placed on the entry (exit) edge E of LOOP. After this transformation
   we have two loops one after the other - first-loop iterates FIRST_NITERS
   times, and second-loop iterates the remainder NITERS - FIRST_NITERS times.
   If the cost model indicates that it is profitable to emit a scalar
   loop instead of the vector one, then the prolog (epilog) loop will iterate
   for the entire unchanged scalar iterations of the loop.

   Input:
   - LOOP: the loop to be peeled.
   - SCALAR_LOOP: if non-NULL, the alternate loop from which basic blocks
	should be copied.
   - E: the exit or entry edge of LOOP.
        If it is the entry edge, we peel the first iterations of LOOP. In this
        case first-loop is LOOP, and second-loop is the newly created loop.
        If it is the exit edge, we peel the last iterations of LOOP. In this
        case, first-loop is the newly created loop, and second-loop is LOOP.
   - NITERS: the number of iterations that LOOP iterates.
   - FIRST_NITERS: the number of iterations that the first-loop should iterate.
   - UPDATE_FIRST_LOOP_COUNT:  specified whether this function is responsible
        for updating the loop bound of the first-loop to FIRST_NITERS.  If it
        is false, the caller of this function may want to take care of this
        (this can be useful if we don't want new stmts added to first-loop).
   - TH: cost model profitability threshold of iterations for vectorization.
   - CHECK_PROFITABILITY: specify whether cost model check has not occurred
                          during versioning and hence needs to occur during
			  prologue generation or whether cost model check
			  has not occurred during prologue generation and hence
			  needs to occur during epilogue generation.
   - BOUND1 is the upper bound on number of iterations of the first loop (if known)
   - BOUND2 is the upper bound on number of iterations of the second loop (if known)


   Output:
   The function returns a pointer to the new loop-copy, or NULL if it failed
   to perform the transformation.

   The function generates two if-then-else guards: one before the first loop,
   and the other before the second loop:
   The first guard is:
     if (FIRST_NITERS == 0) then skip the first loop,
     and go directly to the second loop.
   The second guard is:
     if (FIRST_NITERS == NITERS) then skip the second loop.

   If the optional COND_EXPR and COND_EXPR_STMT_LIST arguments are given
   then the generated condition is combined with COND_EXPR and the
   statements in COND_EXPR_STMT_LIST are emitted together with it.

   FORNOW only simple loops are supported (see slpeel_can_duplicate_loop_p).
   FORNOW the resulting code will not be in loop-closed-ssa form.
*/

static struct loop *
slpeel_tree_peel_loop_to_edge (struct loop *loop, struct loop *scalar_loop,
			       edge e, tree *first_niters,
			       tree niters, bool update_first_loop_count,
			       unsigned int th, bool check_profitability,
			       tree cond_expr, gimple_seq cond_expr_stmt_list,
			       int bound1, int bound2)
{
  struct loop *new_loop = NULL, *first_loop, *second_loop;
  edge skip_e;
  tree pre_condition = NULL_TREE;
  basic_block bb_before_second_loop, bb_after_second_loop;
  basic_block bb_before_first_loop;
  basic_block bb_between_loops;
  basic_block new_exit_bb;
  gphi_iterator gsi;
  edge exit_e = single_exit (loop);
  source_location loop_loc;
  /* There are many aspects to how likely the first loop is going to be executed.
     Without histogram we can't really do good job.  Simply set it to
     2/3, so the first loop is not reordered to the end of function and
     the hot path through stays short.  */
  int first_guard_probability = 2 * REG_BR_PROB_BASE / 3;
  int second_guard_probability = 2 * REG_BR_PROB_BASE / 3;
  int probability_of_second_loop;

  if (!slpeel_can_duplicate_loop_p (loop, e))
    return NULL;

  /* We might have a queued need to update virtual SSA form.  As we
     delete the update SSA machinery below after doing a regular
     incremental SSA update during loop copying make sure we don't
     lose that fact.
     ???  Needing to update virtual SSA form by renaming is unfortunate
     but not all of the vectorizer code inserting new loads / stores
     properly assigns virtual operands to those statements.  */
  update_ssa (TODO_update_ssa_only_virtuals);
 
  /* If the loop has a virtual PHI, but exit bb doesn't, create a virtual PHI
     in the exit bb and rename all the uses after the loop.  This simplifies
     the *guard[12] routines, which assume loop closed SSA form for all PHIs
     (but normally loop closed SSA form doesn't require virtual PHIs to be
     in the same form).  Doing this early simplifies the checking what
     uses should be renamed.  */
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

  /* 1. Generate a copy of LOOP and put it on E (E is the entry/exit of LOOP).
        Resulting CFG would be:

        first_loop:
        do {
        } while ...

        second_loop:
        do {
        } while ...

        orig_exit_bb:
   */

  if (!(new_loop = slpeel_tree_duplicate_loop_to_edge_cfg (loop, scalar_loop,
							   e)))
    {
      loop_loc = find_loop_location (loop);
      dump_printf_loc (MSG_MISSED_OPTIMIZATION, loop_loc,
                       "tree_duplicate_loop_to_edge_cfg failed.\n");
      return NULL;
    }

  if (MAY_HAVE_DEBUG_STMTS)
    {
      gcc_assert (!adjust_vec.exists ());
      adjust_vec.create (32);
    }

  if (e == exit_e)
    {
      /* NEW_LOOP was placed after LOOP.  */
      first_loop = loop;
      second_loop = new_loop;
    }
  else
    {
      /* NEW_LOOP was placed before LOOP.  */
      first_loop = new_loop;
      second_loop = loop;
    }

  /* 2.  Add the guard code in one of the following ways:

     2.a Add the guard that controls whether the first loop is executed.
         This occurs when this function is invoked for prologue or epilogue
	 generation and when the cost model check can be done at compile time.

         Resulting CFG would be:

         bb_before_first_loop:
         if (FIRST_NITERS == 0) GOTO bb_before_second_loop
                                GOTO first-loop

         first_loop:
         do {
         } while ...

         bb_before_second_loop:

         second_loop:
         do {
         } while ...

         orig_exit_bb:

     2.b Add the cost model check that allows the prologue
         to iterate for the entire unchanged scalar
         iterations of the loop in the event that the cost
         model indicates that the scalar loop is more
         profitable than the vector one. This occurs when
	 this function is invoked for prologue generation
	 and the cost model check needs to be done at run
	 time.

         Resulting CFG after prologue peeling would be:

         if (scalar_loop_iterations <= th)
           FIRST_NITERS = scalar_loop_iterations

         bb_before_first_loop:
         if (FIRST_NITERS == 0) GOTO bb_before_second_loop
                                GOTO first-loop

         first_loop:
         do {
         } while ...

         bb_before_second_loop:

         second_loop:
         do {
         } while ...

         orig_exit_bb:

     2.c Add the cost model check that allows the epilogue
         to iterate for the entire unchanged scalar
         iterations of the loop in the event that the cost
         model indicates that the scalar loop is more
         profitable than the vector one. This occurs when
	 this function is invoked for epilogue generation
	 and the cost model check needs to be done at run
	 time.  This check is combined with any pre-existing
	 check in COND_EXPR to avoid versioning.

         Resulting CFG after prologue peeling would be:

         bb_before_first_loop:
         if ((scalar_loop_iterations <= th)
             ||
             FIRST_NITERS == 0) GOTO bb_before_second_loop
                                GOTO first-loop

         first_loop:
         do {
         } while ...

         bb_before_second_loop:

         second_loop:
         do {
         } while ...

         orig_exit_bb:
  */

  bb_before_first_loop = split_edge (loop_preheader_edge (first_loop));
  /* Loop copying insterted a forwarder block for us here.  */
  bb_before_second_loop = single_exit (first_loop)->dest;

  probability_of_second_loop = (inverse_probability (first_guard_probability)
			        + combine_probabilities (second_guard_probability,
                                                         first_guard_probability));
  /* Theoretically preheader edge of first loop and exit edge should have
     same frequencies.  Loop exit probablities are however easy to get wrong.
     It is safer to copy value from original loop entry.  */
  bb_before_second_loop->frequency
     = combine_probabilities (bb_before_first_loop->frequency,
                              probability_of_second_loop);
  bb_before_second_loop->count
     = apply_probability (bb_before_first_loop->count,
			  probability_of_second_loop);
  single_succ_edge (bb_before_second_loop)->count
     = bb_before_second_loop->count;

  /* Epilogue peeling.  */
  if (!update_first_loop_count)
    {
      loop_vec_info loop_vinfo = loop_vec_info_for_loop (loop);
      tree scalar_loop_iters = LOOP_VINFO_NITERSM1 (loop_vinfo);
      unsigned limit = LOOP_VINFO_VECT_FACTOR (loop_vinfo) - 1;
      if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo))
	limit = limit + 1;
      if (check_profitability
	  && th > limit)
	limit = th;
      pre_condition =
	fold_build2 (LT_EXPR, boolean_type_node, scalar_loop_iters,
		     build_int_cst (TREE_TYPE (scalar_loop_iters), limit));
      if (cond_expr)
	{
	  pre_condition =
	    fold_build2 (TRUTH_OR_EXPR, boolean_type_node,
			 pre_condition,
			 fold_build1 (TRUTH_NOT_EXPR, boolean_type_node,
				      cond_expr));
	}
    }

  /* Prologue peeling.  */
  else
    {
      if (check_profitability)
	set_prologue_iterations (bb_before_first_loop, first_niters,
				 loop, th, first_guard_probability);

      pre_condition =
	fold_build2 (LE_EXPR, boolean_type_node, *first_niters,
		     build_int_cst (TREE_TYPE (*first_niters), 0));
    }

  skip_e = slpeel_add_loop_guard (bb_before_first_loop, pre_condition,
				  cond_expr_stmt_list,
                                  bb_before_second_loop, bb_before_first_loop,
				  inverse_probability (first_guard_probability));
  scale_loop_profile (first_loop, first_guard_probability,
		      check_profitability && (int)th > bound1 ? th : bound1);
  slpeel_update_phi_nodes_for_guard1 (skip_e, first_loop,
				      first_loop == new_loop,
				      &new_exit_bb);


  /* 3. Add the guard that controls whether the second loop is executed.
        Resulting CFG would be:

        bb_before_first_loop:
        if (FIRST_NITERS == 0) GOTO bb_before_second_loop (skip first loop)
                               GOTO first-loop

        first_loop:
        do {
        } while ...

        bb_between_loops:
        if (FIRST_NITERS == NITERS) GOTO bb_after_second_loop (skip second loop)
                                    GOTO bb_before_second_loop

        bb_before_second_loop:

        second_loop:
        do {
        } while ...

        bb_after_second_loop:

        orig_exit_bb:
   */

  bb_between_loops = new_exit_bb;
  bb_after_second_loop = split_edge (single_exit (second_loop));

  pre_condition =
	fold_build2 (EQ_EXPR, boolean_type_node, *first_niters, niters);
  skip_e = slpeel_add_loop_guard (bb_between_loops, pre_condition, NULL,
                                  bb_after_second_loop, bb_before_first_loop,
				  inverse_probability (second_guard_probability));
  scale_loop_profile (second_loop, probability_of_second_loop, bound2);
  slpeel_update_phi_nodes_for_guard2 (skip_e, second_loop,
                                     second_loop == new_loop, &new_exit_bb);

  /* 4. Make first-loop iterate FIRST_NITERS times, if requested.
   */
  if (update_first_loop_count)
    slpeel_make_loop_iterate_ntimes (first_loop, *first_niters);

  delete_update_ssa ();

  adjust_vec_debug_stmts ();

  return new_loop;
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
  gimple *phi;
  gphi_iterator gsi;

  /* Analyze phi functions of the loop header.  */

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "vect_can_advance_ivs_p:\n");
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      tree evolution_part;

      phi = gsi.phi ();
      if (dump_enabled_p ())
	{
          dump_printf_loc (MSG_NOTE, vect_location, "Analyze phi: ");
          dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi, 0);
	}

      /* Skip virtual phi's. The data dependences that are associated with
         virtual defs/uses (i.e., memory accesses) are analyzed elsewhere.  */

      if (virtual_operand_p (PHI_RESULT (phi)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "virtual phi. skip.\n");
	  continue;
	}

      /* Skip reduction phis.  */

      if (STMT_VINFO_DEF_TYPE (vinfo_for_stmt (phi)) == vect_reduction_def)
        {
          if (dump_enabled_p ())
            dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "reduc phi. skip.\n");
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
vect_update_ivs_after_vectorizer (loop_vec_info loop_vinfo, tree niters,
				  edge update_e)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block exit_bb = single_exit (loop)->dest;
  gphi *phi, *phi1;
  gphi_iterator gsi, gsi1;
  basic_block update_bb = update_e->dest;

  gcc_checking_assert (vect_can_advance_ivs_p (loop_vinfo));

  /* Make sure there exists a single-predecessor exit bb:  */
  gcc_assert (single_pred_p (exit_bb));

  for (gsi = gsi_start_phis (loop->header), gsi1 = gsi_start_phis (update_bb);
       !gsi_end_p (gsi) && !gsi_end_p (gsi1);
       gsi_next (&gsi), gsi_next (&gsi1))
    {
      tree init_expr;
      tree step_expr, off;
      tree type;
      tree var, ni, ni_name;
      gimple_stmt_iterator last_gsi;
      stmt_vec_info stmt_info;

      phi = gsi.phi ();
      phi1 = gsi1.phi ();
      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_NOTE, vect_location,
                           "vect_update_ivs_after_vectorizer: phi: ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi, 0);
        }

      /* Skip virtual phi's.  */
      if (virtual_operand_p (PHI_RESULT (phi)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "virtual phi. skip.\n");
	  continue;
	}

      /* Skip reduction phis.  */
      stmt_info = vinfo_for_stmt (phi);
      if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def
	  || STMT_VINFO_DEF_TYPE (stmt_info) == vect_double_reduction_def)
        {
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "reduc phi. skip.\n");
          continue;
        }

      type = TREE_TYPE (gimple_phi_result (phi));
      step_expr = STMT_VINFO_LOOP_PHI_EVOLUTION_PART (stmt_info);
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
      ni_name = force_gimple_operand_gsi (&last_gsi, ni, false, var,
					  true, GSI_SAME_STMT);

      /* Fix phi expressions in the successor bb.  */
      adjust_phi_and_debug_stmts (phi1, update_e, ni_name);
    }
}

/* Function vect_do_peeling_for_loop_bound

   Peel the last iterations of the loop represented by LOOP_VINFO.
   The peeled iterations form a new epilog loop.  Given that the loop now
   iterates NITERS times, the new epilog loop iterates
   NITERS % VECTORIZATION_FACTOR times.

   If CHECK_PROFITABILITY is 1 then profitability check is generated
   using TH as a cost model profitability threshold of iterations for
   vectorization.

   The original loop will later be made to iterate
   NITERS / VECTORIZATION_FACTOR times (this value is placed into RATIO).

   COND_EXPR and COND_EXPR_STMT_LIST are combined with a new generated
   test.  */

void
vect_do_peeling_for_loop_bound (loop_vec_info loop_vinfo,
				tree ni_name, tree ratio_mult_vf_name,
				unsigned int th, bool check_profitability)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  struct loop *scalar_loop = LOOP_VINFO_SCALAR_LOOP (loop_vinfo);
  struct loop *new_loop;
  edge update_e;
  basic_block preheader;
  int loop_num;
  int max_iter;
  tree cond_expr = NULL_TREE;
  gimple_seq cond_expr_stmt_list = NULL;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "=== vect_do_peeling_for_loop_bound ===\n");

  initialize_original_copy_tables ();

  loop_num  = loop->num;

  new_loop
    = slpeel_tree_peel_loop_to_edge (loop, scalar_loop, single_exit (loop),
				     &ratio_mult_vf_name, ni_name, false,
				     th, check_profitability,
				     cond_expr, cond_expr_stmt_list,
				     0, LOOP_VINFO_VECT_FACTOR (loop_vinfo));
  gcc_assert (new_loop);
  gcc_assert (loop_num == loop->num);
  slpeel_checking_verify_cfg_after_peeling (loop, new_loop);

  /* A guard that controls whether the new_loop is to be executed or skipped
     is placed in LOOP->exit.  LOOP->exit therefore has two successors - one
     is the preheader of NEW_LOOP, where the IVs from LOOP are used.  The other
     is a bb after NEW_LOOP, where these IVs are not used.  Find the edge that
     is on the path where the LOOP IVs are used and need to be updated.  */

  preheader = loop_preheader_edge (new_loop)->src;
  if (EDGE_PRED (preheader, 0)->src == single_exit (loop)->dest)
    update_e = EDGE_PRED (preheader, 0);
  else
    update_e = EDGE_PRED (preheader, 1);

  /* Update IVs of original loop as if they were advanced
     by ratio_mult_vf_name steps.  */
  vect_update_ivs_after_vectorizer (loop_vinfo, ratio_mult_vf_name, update_e);

  /* For vectorization factor N, we need to copy last N-1 values in epilogue
     and this means N-2 loopback edge executions.

     PEELING_FOR_GAPS works by subtracting last iteration and thus the epilogue
     will execute at least LOOP_VINFO_VECT_FACTOR times.  */
  max_iter = (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo)
	      ? LOOP_VINFO_VECT_FACTOR (loop_vinfo) * 2
	      : LOOP_VINFO_VECT_FACTOR (loop_vinfo)) - 2;
  if (check_profitability)
    max_iter = MAX (max_iter, (int) th - 1);
  record_niter_bound (new_loop, max_iter, false, true);
  dump_printf (MSG_NOTE,
               "Setting upper bound of nb iterations for epilogue "
               "loop to %d\n", max_iter);

  /* After peeling we have to reset scalar evolution analyzer.  */
  scev_reset ();

  free_original_copy_tables ();
}


/* Function vect_gen_niters_for_prolog_loop

   Set the number of iterations for the loop represented by LOOP_VINFO
   to the minimum between LOOP_NITERS (the original iteration count of the loop)
   and the misalignment of DR - the data reference recorded in
   LOOP_VINFO_UNALIGNED_DR (LOOP_VINFO).  As a result, after the execution of
   this loop, the data reference DR will refer to an aligned location.

   The following computation is generated:

   If the misalignment of DR is known at compile time:
     addr_mis = int mis = DR_MISALIGNMENT (dr);
   Else, compute address misalignment in bytes:
     addr_mis = addr & (vectype_align - 1)

   prolog_niters = min (LOOP_NITERS, ((VF - addr_mis/elem_size)&(VF-1))/step)

   (elem_size = element type size; an element is the scalar element whose type
   is the inner type of the vectype)

   When the step of the data-ref in the loop is not 1 (as in interleaved data
   and SLP), the number of iterations of the prolog must be divided by the step
   (which is equal to the size of interleaved group).

   The above formulas assume that VF == number of elements in the vector. This
   may not hold when there are multiple-types in the loop.
   In this case, for some data-references in the loop the VF does not represent
   the number of elements that fit in the vector.  Therefore, instead of VF we
   use TYPE_VECTOR_SUBPARTS.  */

static tree
vect_gen_niters_for_prolog_loop (loop_vec_info loop_vinfo, tree loop_niters, int *bound)
{
  struct data_reference *dr = LOOP_VINFO_UNALIGNED_DR (loop_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree var;
  gimple_seq stmts;
  tree iters, iters_name;
  edge pe;
  basic_block new_bb;
  gimple *dr_stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (dr_stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  int vectype_align = TYPE_ALIGN (vectype) / BITS_PER_UNIT;
  tree niters_type = TREE_TYPE (loop_niters);
  int nelements = TYPE_VECTOR_SUBPARTS (vectype);

  pe = loop_preheader_edge (loop);

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
      gimple_seq new_stmts = NULL;
      bool negative = tree_int_cst_compare (DR_STEP (dr), size_zero_node) < 0;
      tree offset = negative
	  ? size_int (-TYPE_VECTOR_SUBPARTS (vectype) + 1) : size_zero_node;
      tree start_addr = vect_create_addr_base_for_vector_ref (dr_stmt,
						&new_stmts, offset, loop);
      tree type = unsigned_type_for (TREE_TYPE (start_addr));
      tree vectype_align_minus_1 = build_int_cst (type, vectype_align - 1);
      HOST_WIDE_INT elem_size =
                int_cst_value (TYPE_SIZE_UNIT (TREE_TYPE (vectype)));
      tree elem_size_log = build_int_cst (type, exact_log2 (elem_size));
      tree nelements_minus_1 = build_int_cst (type, nelements - 1);
      tree nelements_tree = build_int_cst (type, nelements);
      tree byte_misalign;
      tree elem_misalign;

      new_bb = gsi_insert_seq_on_edge_immediate (pe, new_stmts);
      gcc_assert (!new_bb);

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
      *bound = nelements;
    }

  /* Create:  prolog_loop_niters = min (iters, loop_niters) */
  /* If the loop bound is known at compile time we already verified that it is
     greater than vf; since the misalignment ('iters') is at most vf, there's
     no need to generate the MIN_EXPR in this case.  */
  if (TREE_CODE (loop_niters) != INTEGER_CST)
    iters = fold_build2 (MIN_EXPR, niters_type, iters, loop_niters);

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
                       "niters for prolog loop: ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, iters);
      dump_printf (MSG_NOTE, "\n");
    }

  var = create_tmp_var (niters_type, "prolog_loop_niters");
  stmts = NULL;
  iters_name = force_gimple_operand (iters, &stmts, false, var);

  /* Insert stmt on loop preheader edge.  */
  if (stmts)
    {
      basic_block new_bb = gsi_insert_seq_on_edge_immediate (pe, stmts);
      gcc_assert (!new_bb);
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

  FOR_EACH_VEC_ELT (datarefs, i, dr)
    vect_update_init_of_dr (dr, niters);
}


/* Function vect_do_peeling_for_alignment

   Peel the first 'niters' iterations of the loop represented by LOOP_VINFO.
   'niters' is set to the misalignment of one of the data references in the
   loop, thereby forcing it to refer to an aligned location at the beginning
   of the execution of this loop.  The data reference for which we are
   peeling is recorded in LOOP_VINFO_UNALIGNED_DR.

   If CHECK_PROFITABILITY is 1 then profitability check is generated
   using TH as a cost model profitability threshold of iterations for
   vectorization.  */

void
vect_do_peeling_for_alignment (loop_vec_info loop_vinfo, tree ni_name,
			       unsigned int th, bool check_profitability)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  struct loop *scalar_loop = LOOP_VINFO_SCALAR_LOOP (loop_vinfo);
  tree niters_of_prolog_loop;
  tree wide_prolog_niters;
  struct loop *new_loop;
  int max_iter;
  int bound = 0;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
                     "loop peeled for vectorization to enhance"
                     " alignment\n");

  initialize_original_copy_tables ();

  gimple_seq stmts = NULL;
  gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);
  niters_of_prolog_loop = vect_gen_niters_for_prolog_loop (loop_vinfo,
							   ni_name,
							   &bound);

  /* Peel the prolog loop and iterate it niters_of_prolog_loop.  */
  new_loop =
    slpeel_tree_peel_loop_to_edge (loop, scalar_loop,
				   loop_preheader_edge (loop),
				   &niters_of_prolog_loop, ni_name, true,
				   th, check_profitability, NULL_TREE, NULL,
				   bound, 0);

  gcc_assert (new_loop);
  slpeel_checking_verify_cfg_after_peeling (new_loop, loop);
  /* For vectorization factor N, we need to copy at most N-1 values 
     for alignment and this means N-2 loopback edge executions.  */
  max_iter = LOOP_VINFO_VECT_FACTOR (loop_vinfo) - 2;
  if (check_profitability)
    max_iter = MAX (max_iter, (int) th - 1);
  record_niter_bound (new_loop, max_iter, false, true);
  dump_printf (MSG_NOTE,
               "Setting upper bound of nb iterations for prologue "
               "loop to %d\n", max_iter);

  /* Update number of times loop executes.  */
  LOOP_VINFO_NITERS (loop_vinfo) = fold_build2 (MINUS_EXPR,
		TREE_TYPE (ni_name), ni_name, niters_of_prolog_loop);
  LOOP_VINFO_NITERSM1 (loop_vinfo) = fold_build2 (MINUS_EXPR,
		TREE_TYPE (ni_name),
		LOOP_VINFO_NITERSM1 (loop_vinfo), niters_of_prolog_loop);

  if (types_compatible_p (sizetype, TREE_TYPE (niters_of_prolog_loop)))
    wide_prolog_niters = niters_of_prolog_loop;
  else
    {
      gimple_seq seq = NULL;
      edge pe = loop_preheader_edge (loop);
      tree wide_iters = fold_convert (sizetype, niters_of_prolog_loop);
      tree var = create_tmp_var (sizetype, "prolog_loop_adjusted_niters");
      wide_prolog_niters = force_gimple_operand (wide_iters, &seq, false,
                                                 var);
      if (seq)
	{
	  /* Insert stmt on loop preheader edge.  */
          basic_block new_bb = gsi_insert_seq_on_edge_immediate (pe, seq);
          gcc_assert (!new_bb);
        }
    }

  /* Update the init conditions of the access functions of all data refs.  */
  vect_update_inits_of_drs (loop_vinfo, wide_prolog_niters);

  /* After peeling we have to reset scalar evolution analyzer.  */
  scev_reset ();

  free_original_copy_tables ();
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
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
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
					      offset, loop);
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
  if (*cond_expr)
    *cond_expr = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
			      *cond_expr, part_cond_expr);
  else
    *cond_expr = part_cond_expr;
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
  tree part_cond_expr;

  /* Create expression
     ((store_ptr_0 + store_segment_length_0) <= load_ptr_0)
     || (load_ptr_0 + load_segment_length_0) <= store_ptr_0))
     &&
     ...
     &&
     ((store_ptr_n + store_segment_length_n) <= load_ptr_n)
     || (load_ptr_n + load_segment_length_n) <= store_ptr_n))  */

  if (comp_alias_ddrs.is_empty ())
    return;

  for (size_t i = 0, s = comp_alias_ddrs.length (); i < s; ++i)
    {
      const dr_with_seg_len& dr_a = comp_alias_ddrs[i].first;
      const dr_with_seg_len& dr_b = comp_alias_ddrs[i].second;
      tree segment_length_a = dr_a.seg_len;
      tree segment_length_b = dr_b.seg_len;
      tree addr_base_a = DR_BASE_ADDRESS (dr_a.dr);
      tree addr_base_b = DR_BASE_ADDRESS (dr_b.dr);
      tree offset_a = DR_OFFSET (dr_a.dr), offset_b = DR_OFFSET (dr_b.dr);

      offset_a = fold_build2 (PLUS_EXPR, TREE_TYPE (offset_a),
			      offset_a, DR_INIT (dr_a.dr));
      offset_b = fold_build2 (PLUS_EXPR, TREE_TYPE (offset_b),
			      offset_b, DR_INIT (dr_b.dr));
      addr_base_a = fold_build_pointer_plus (addr_base_a, offset_a);
      addr_base_b = fold_build_pointer_plus (addr_base_b, offset_b);

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "create runtime check for data references ");
	  dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dr_a.dr));
	  dump_printf (MSG_NOTE, " and ");
	  dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dr_b.dr));
	  dump_printf (MSG_NOTE, "\n");
	}

      tree seg_a_min = addr_base_a;
      tree seg_a_max = fold_build_pointer_plus (addr_base_a, segment_length_a);
      /* For negative step, we need to adjust address range by TYPE_SIZE_UNIT
	 bytes, e.g., int a[3] -> a[1] range is [a+4, a+16) instead of
	 [a, a+12) */
      if (tree_int_cst_compare (DR_STEP (dr_a.dr), size_zero_node) < 0)
	{
	  tree unit_size = TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dr_a.dr)));
	  seg_a_min = fold_build_pointer_plus (seg_a_max, unit_size);
	  seg_a_max = fold_build_pointer_plus (addr_base_a, unit_size);
	}

      tree seg_b_min = addr_base_b;
      tree seg_b_max = fold_build_pointer_plus (addr_base_b, segment_length_b);
      if (tree_int_cst_compare (DR_STEP (dr_b.dr), size_zero_node) < 0)
	{
	  tree unit_size = TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dr_b.dr)));
	  seg_b_min = fold_build_pointer_plus (seg_b_max, unit_size);
	  seg_b_max = fold_build_pointer_plus (addr_base_b, unit_size);
	}

      part_cond_expr =
      	fold_build2 (TRUTH_OR_EXPR, boolean_type_node,
	  fold_build2 (LE_EXPR, boolean_type_node, seg_a_max, seg_b_min),
	  fold_build2 (LE_EXPR, boolean_type_node, seg_b_max, seg_a_min));

      if (*cond_expr)
	*cond_expr = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
				  *cond_expr, part_cond_expr);
      else
	*cond_expr = part_cond_expr;
    }

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
  unsigned prob = 4 * REG_BR_PROB_BASE / 5;
  gimple_seq gimplify_stmt_list = NULL;
  tree scalar_loop_iters = LOOP_VINFO_NITERS (loop_vinfo);
  bool version_align = LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo);
  bool version_alias = LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo);
  bool version_niter = LOOP_REQUIRES_VERSIONING_FOR_NITERS (loop_vinfo);

  if (check_profitability)
    cond_expr = fold_build2 (GT_EXPR, boolean_type_node, scalar_loop_iters,
			     build_int_cst (TREE_TYPE (scalar_loop_iters),
						       th));

  if (version_niter)
    vect_create_cond_for_niters_checks (loop_vinfo, &cond_expr);

  if (cond_expr)
    cond_expr = force_gimple_operand_1 (cond_expr, &cond_expr_stmt_list,
					is_gimple_condexpr, NULL_TREE);

  if (version_align)
    vect_create_cond_for_align_checks (loop_vinfo, &cond_expr,
				       &cond_expr_stmt_list);

  if (version_alias)
    vect_create_cond_for_alias_checks (loop_vinfo, &cond_expr);

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
      nloop = loop_version (scalar_loop, cond_expr, &condition_bb, prob,
			    REG_BR_PROB_BASE, REG_BR_PROB_BASE - prob, true);
      scale_loop_frequencies (loop, prob, REG_BR_PROB_BASE);
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
			  prob, prob, REG_BR_PROB_BASE - prob, true);

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
