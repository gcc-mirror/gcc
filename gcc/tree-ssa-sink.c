/* Code sinking for trees
   Copyright (C) 2001-2014 Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "stor-layout.h"
#include "basic-block.h"
#include "gimple-pretty-print.h"
#include "tree-inline.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "hashtab.h"
#include "tree-iterator.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "flags.h"
#include "cfgloop.h"
#include "params.h"

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

} sink_stats;


/* Given a PHI, and one of its arguments (DEF), find the edge for
   that argument and return it.  If the argument occurs twice in the PHI node,
   we return NULL.  */

static basic_block
find_bb_for_arg (gimple phi, tree def)
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
all_immediate_uses_same_place (gimple stmt)
{
  gimple firstuse = NULL;
  ssa_op_iter op_iter;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  tree var;

  FOR_EACH_SSA_TREE_OPERAND (var, stmt, op_iter, SSA_OP_ALL_DEFS)
    {
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
    }

  return true;
}

/* Find the nearest common dominator of all of the immediate uses in IMM.  */

static basic_block
nearest_common_dominator_of_uses (gimple stmt, bool *debug_stmts)
{
  bitmap blocks = BITMAP_ALLOC (NULL);
  basic_block commondom;
  unsigned int j;
  bitmap_iterator bi;
  ssa_op_iter op_iter;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  tree var;

  bitmap_clear (blocks);
  FOR_EACH_SSA_TREE_OPERAND (var, stmt, op_iter, SSA_OP_ALL_DEFS)
    {
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, var)
        {
	  gimple usestmt = USE_STMT (use_p);
	  basic_block useblock;

	  if (gimple_code (usestmt) == GIMPLE_PHI)
	    {
	      int idx = PHI_ARG_INDEX_FROM_USE (use_p);

	      useblock = gimple_phi_arg_edge (usestmt, idx)->src;
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
	    {
	      BITMAP_FREE (blocks);
	      return NULL;
	    }
	  bitmap_set_bit (blocks, useblock->index);
	}
    }
  commondom = BASIC_BLOCK_FOR_FN (cfun, bitmap_first_set_bit (blocks));
  EXECUTE_IF_SET_IN_BITMAP (blocks, 0, j, bi)
    commondom = nearest_common_dominator (CDI_DOMINATORS, commondom,
					  BASIC_BLOCK_FOR_FN (cfun, j));
  BITMAP_FREE (blocks);
  return commondom;
}

/* Given EARLY_BB and LATE_BB, two blocks in a path through the dominator
   tree, return the best basic block between them (inclusive) to place
   statements.

   We want the most control dependent block in the shallowest loop nest.

   If the resulting block is in a shallower loop nest, then use it.  Else
   only use the resulting block if it has significantly lower execution
   frequency than EARLY_BB to avoid gratutious statement movement.  We
   consider statements with VOPS more desirable to move.

   This pass would obviously benefit from PDO as it utilizes block
   frequencies.  It would also benefit from recomputing frequencies
   if profile data is not available since frequencies often get out
   of sync with reality.  */

static basic_block
select_best_block (basic_block early_bb,
		   basic_block late_bb,
		   gimple stmt)
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

  /* If we found a shallower loop nest, then we always consider that
     a win.  This will always give us the most control dependent block
     within that loop nest.  */
  if (bb_loop_depth (best_bb) < bb_loop_depth (early_bb))
    return best_bb;

  /* Get the sinking threshold.  If the statement to be moved has memory
     operands, then increase the threshold by 7% as those are even more
     profitable to avoid, clamping at 100%.  */
  threshold = PARAM_VALUE (PARAM_SINK_FREQUENCY_THRESHOLD);
  if (gimple_vuse (stmt) || gimple_vdef (stmt))
    {
      threshold += 7;
      if (threshold > 100)
	threshold = 100;
    }

  /* If BEST_BB is at the same nesting level, then require it to have
     significantly lower execution frequency to avoid gratutious movement.  */
  if (bb_loop_depth (best_bb) == bb_loop_depth (early_bb)
      && best_bb->frequency < (early_bb->frequency * threshold / 100.0))
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
statement_sink_location (gimple stmt, basic_block frombb,
			 gimple_stmt_iterator *togsi)
{
  gimple use;
  use_operand_p one_use = NULL_USE_OPERAND_P;
  basic_block sinkbb;
  use_operand_p use_p;
  def_operand_p def_p;
  ssa_op_iter iter;
  imm_use_iterator imm_iter;

  /* We only can sink assignments.  */
  if (!is_gimple_assign (stmt))
    return false;

  /* We only can sink stmts with a single definition.  */
  def_p = single_ssa_def_operand (stmt, SSA_OP_ALL_DEFS);
  if (def_p == NULL_DEF_OPERAND_P)
    return false;

  /* Return if there are no immediate uses of this stmt.  */
  if (has_zero_uses (DEF_FROM_PTR (def_p)))
    return false;

  /* There are a few classes of things we can't or don't move, some because we
     don't have code to handle it, some because it's not profitable and some
     because it's not legal.

     We can't sink things that may be global stores, at least not without
     calculating a lot more information, because we may cause it to no longer
     be seen by an external routine that needs it depending on where it gets
     moved to.

     We don't want to sink loads from memory.

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
      || gimple_has_volatile_ops (stmt)
      || (gimple_vuse (stmt) && !gimple_vdef (stmt))
      || (cfun->has_local_explicit_reg_vars
	  && TYPE_MODE (TREE_TYPE (gimple_assign_lhs (stmt))) == BLKmode))
    return false;

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
  if (gimple_vdef (stmt))
    {
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, DEF_FROM_PTR (def_p))
	{
	  gimple use_stmt = USE_STMT (use_p);

	  /* A killing definition is not a use.  */
	  if ((gimple_has_lhs (use_stmt)
	       && operand_equal_p (gimple_assign_lhs (stmt),
				   gimple_get_lhs (use_stmt), 0))
	      || stmt_kills_ref_p (use_stmt, gimple_assign_lhs (stmt)))
	    {
	      /* If use_stmt is or might be a nop assignment then USE_STMT
	         acts as a use as well as definition.  */
	      if (stmt != use_stmt
		  && ref_maybe_used_by_stmt_p (use_stmt,
					       gimple_assign_lhs (stmt)))
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
  else if (!all_immediate_uses_same_place (stmt))
    {
      bool debug_stmts = false;
      basic_block commondom = nearest_common_dominator_of_uses (stmt,
								&debug_stmts);

      if (commondom == frombb)
	return false;

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
	  sinkbb = gimple_bb (use);
	  sinkbb = select_best_block (frombb, gimple_bb (use), stmt);

	  if (sinkbb == frombb)
	    return false;

	  *togsi = gsi_for_stmt (use);

	  return true;
	}
    }

  sinkbb = find_bb_for_arg (use, DEF_FROM_PTR (def_p));

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

/* Perform code sinking on BB */

static void
sink_code_in_bb (basic_block bb)
{
  basic_block son;
  gimple_stmt_iterator gsi;
  edge_iterator ei;
  edge e;
  bool last = true;

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
      gimple stmt = gsi_stmt (gsi);
      gimple_stmt_iterator togsi;

      if (!statement_sink_location (stmt, bb, &togsi))
	{
	  if (!gsi_end_p (gsi))
	    gsi_prev (&gsi);
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
	  gimple vuse_stmt;

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
      sink_code_in_bb (son);
    }
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
static void
execute_sink_code (void)
{
  loop_optimizer_init (LOOPS_NORMAL);
  split_critical_edges ();
  connect_infinite_loops_to_exit ();
  memset (&sink_stats, 0, sizeof (sink_stats));
  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);
  sink_code_in_bb (EXIT_BLOCK_PTR_FOR_FN (cfun));
  statistics_counter_event (cfun, "Sunk statements", sink_stats.sunk);
  free_dominance_info (CDI_POST_DOMINATORS);
  remove_fake_exit_edges ();
  loop_optimizer_finalize ();
}

/* Gate and execute functions for PRE.  */

static unsigned int
do_sink (void)
{
  execute_sink_code ();
  return 0;
}

static bool
gate_sink (void)
{
  return flag_tree_sink != 0;
}

namespace {

const pass_data pass_data_sink_code =
{
  GIMPLE_PASS, /* type */
  "sink", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_SINK, /* tv_id */
  /* PROP_no_crit_edges is ensured by running split_critical_edges in
     execute_sink_code.  */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_update_ssa | TODO_verify_ssa
    | TODO_verify_flow ), /* todo_flags_finish */
};

class pass_sink_code : public gimple_opt_pass
{
public:
  pass_sink_code (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_sink_code, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_sink (); }
  unsigned int execute () { return do_sink (); }

}; // class pass_sink_code

} // anon namespace

gimple_opt_pass *
make_pass_sink_code (gcc::context *ctxt)
{
  return new pass_sink_code (ctxt);
}
