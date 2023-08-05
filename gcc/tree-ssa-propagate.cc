/* Generic SSA value propagation engine.
   Copyright (C) 2004-2023 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

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
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "dumpfile.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "tree-ssa-propagate.h"
#include "domwalk.h"
#include "cfgloop.h"
#include "tree-cfgcleanup.h"
#include "cfganal.h"
#include "tree-ssa-dce.h"

/* This file implements a generic value propagation engine based on
   the same propagation used by the SSA-CCP algorithm [1].

   Propagation is performed by simulating the execution of every
   statement that produces the value being propagated.  Simulation
   proceeds as follows:

   1- Initially, all edges of the CFG are marked not executable and
      the CFG worklist is seeded with all the statements in the entry
      basic block (block 0).

   2- Every statement S is simulated with a call to the call-back
      function SSA_PROP_VISIT_STMT.  This evaluation may produce 3
      results:

      	SSA_PROP_NOT_INTERESTING: Statement S produces nothing of
	    interest and does not affect any of the work lists.
	    The statement may be simulated again if any of its input
	    operands change in future iterations of the simulator.

	SSA_PROP_VARYING: The value produced by S cannot be determined
	    at compile time.  Further simulation of S is not required.
	    If S is a conditional jump, all the outgoing edges for the
	    block are considered executable and added to the work
	    list.

	SSA_PROP_INTERESTING: S produces a value that can be computed
	    at compile time.  Its result can be propagated into the
	    statements that feed from S.  Furthermore, if S is a
	    conditional jump, only the edge known to be taken is added
	    to the work list.  Edges that are known not to execute are
	    never simulated.

   3- PHI nodes are simulated with a call to SSA_PROP_VISIT_PHI.  The
      return value from SSA_PROP_VISIT_PHI has the same semantics as
      described in #2.

   4- Three work lists are kept.  Statements are only added to these
      lists if they produce one of SSA_PROP_INTERESTING or
      SSA_PROP_VARYING.

   	CFG_BLOCKS contains the list of blocks to be simulated.
	    Blocks are added to this list if their incoming edges are
	    found executable.

	SSA_EDGE_WORKLIST contains the list of statements that we 
	    need to revisit.

   5- Simulation terminates when all three work lists are drained.

   Before calling ssa_propagate, it is important to clear
   prop_simulate_again_p for all the statements in the program that
   should be simulated.  This initialization allows an implementation
   to specify which statements should never be simulated.

   It is also important to compute def-use information before calling
   ssa_propagate.

   References:

     [1] Constant propagation with conditional branches,
         Wegman and Zadeck, ACM TOPLAS 13(2):181-210.

     [2] Building an Optimizing Compiler,
	 Robert Morgan, Butterworth-Heinemann, 1998, Section 8.9.

     [3] Advanced Compiler Design and Implementation,
	 Steven Muchnick, Morgan Kaufmann, 1997, Section 12.6  */

/* Worklists of control flow edge destinations.  This contains
   the CFG order number of the blocks so we can iterate in CFG
   order by visiting in bit-order.  We use two worklists to
   first make forward progress before iterating.  */
static bitmap cfg_blocks;
static int *bb_to_cfg_order;
static int *cfg_order_to_bb;

/* Worklists of SSA edges which will need reexamination as their
   definition has changed.  SSA edges are def-use edges in the SSA
   web.  For each D-U edge, we store the target statement or PHI node
   UID in a bitmap.  UIDs order stmts in execution order.  We use
   two worklists to first make forward progress before iterating.  */
static bitmap ssa_edge_worklist;
static vec<gimple *> uid_to_stmt;

/* Current RPO index in the iteration.  */
static int curr_order;


/* We have just defined a new value for VAR.  If IS_VARYING is true,
   add all immediate uses of VAR to VARYING_SSA_EDGES, otherwise add
   them to INTERESTING_SSA_EDGES.  */

static void
add_ssa_edge (tree var)
{
  imm_use_iterator iter;
  use_operand_p use_p;

  FOR_EACH_IMM_USE_FAST (use_p, iter, var)
    {
      gimple *use_stmt = USE_STMT (use_p);
      if (!prop_simulate_again_p (use_stmt))
	continue;

      /* If we did not yet simulate the block wait for this to happen
         and do not add the stmt to the SSA edge worklist.  */
      basic_block use_bb = gimple_bb (use_stmt);
      if (! (use_bb->flags & BB_VISITED))
	continue;

      /* If this is a use on a not yet executable edge do not bother to
	 queue it.  */
      if (gimple_code (use_stmt) == GIMPLE_PHI
	  && !(EDGE_PRED (use_bb, PHI_ARG_INDEX_FROM_USE (use_p))->flags
	       & EDGE_EXECUTABLE))
	continue;

      if (bitmap_set_bit (ssa_edge_worklist, gimple_uid (use_stmt)))
	{
	  uid_to_stmt[gimple_uid (use_stmt)] = use_stmt;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "ssa_edge_worklist: adding SSA use in ");
	      print_gimple_stmt (dump_file, use_stmt, 0, TDF_SLIM);
	    }
	}
    }
}


/* Add edge E to the control flow worklist.  */

static void
add_control_edge (edge e)
{
  basic_block bb = e->dest;
  if (bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return;

  /* If the edge had already been executed, skip it.  */
  if (e->flags & EDGE_EXECUTABLE)
    return;

  e->flags |= EDGE_EXECUTABLE;

  int bb_order = bb_to_cfg_order[bb->index];
  bitmap_set_bit (cfg_blocks, bb_order);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Adding destination of edge (%d -> %d) to worklist\n",
	e->src->index, e->dest->index);
}


/* Simulate the execution of STMT and update the work lists accordingly.  */

void
ssa_propagation_engine::simulate_stmt (gimple *stmt)
{
  enum ssa_prop_result val = SSA_PROP_NOT_INTERESTING;
  edge taken_edge = NULL;
  tree output_name = NULL_TREE;

  /* Pull the stmt off the SSA edge worklist.  */
  bitmap_clear_bit (ssa_edge_worklist, gimple_uid (stmt));

  /* Don't bother visiting statements that are already
     considered varying by the propagator.  */
  if (!prop_simulate_again_p (stmt))
    return;

  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      val = visit_phi (as_a <gphi *> (stmt));
      output_name = gimple_phi_result (stmt);
    }
  else
    val = visit_stmt (stmt, &taken_edge, &output_name);

  if (val == SSA_PROP_VARYING)
    {
      prop_set_simulate_again (stmt, false);

      /* If the statement produced a new varying value, add the SSA
	 edges coming out of OUTPUT_NAME.  */
      if (output_name)
	add_ssa_edge (output_name);

      /* If STMT transfers control out of its basic block, add
	 all outgoing edges to the work list.  */
      if (stmt_ends_bb_p (stmt))
	{
	  edge e;
	  edge_iterator ei;
	  basic_block bb = gimple_bb (stmt);
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    add_control_edge (e);
	}
      return;
    }
  else if (val == SSA_PROP_INTERESTING)
    {
      /* If the statement produced new value, add the SSA edges coming
	 out of OUTPUT_NAME.  */
      if (output_name)
	add_ssa_edge (output_name);

      /* If we know which edge is going to be taken out of this block,
	 add it to the CFG work list.  */
      if (taken_edge)
	add_control_edge (taken_edge);
    }

  /* If there are no SSA uses on the stmt whose defs are simulated
     again then this stmt will be never visited again.  */
  bool has_simulate_again_uses = false;
  use_operand_p use_p;
  ssa_op_iter iter;
  if (gimple_code  (stmt) == GIMPLE_PHI)
    {
      edge_iterator ei;
      edge e;
      tree arg;
      FOR_EACH_EDGE (e, ei, gimple_bb (stmt)->preds)
	if (!(e->flags & EDGE_EXECUTABLE)
	    || ((arg = PHI_ARG_DEF_FROM_EDGE (stmt, e))
		&& TREE_CODE (arg) == SSA_NAME
		&& !SSA_NAME_IS_DEFAULT_DEF (arg)
		&& prop_simulate_again_p (SSA_NAME_DEF_STMT (arg))))
	  {
	    has_simulate_again_uses = true;
	    break;
	  }
    }
  else
    FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
      {
	gimple *def_stmt = SSA_NAME_DEF_STMT (USE_FROM_PTR (use_p));
	if (!gimple_nop_p (def_stmt)
	    && prop_simulate_again_p (def_stmt))
	  {
	    has_simulate_again_uses = true;
	    break;
	  }
      }
  if (!has_simulate_again_uses)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "marking stmt to be not simulated again\n");
      prop_set_simulate_again (stmt, false);
    }
}


/* Simulate the execution of BLOCK.  Evaluate the statement associated
   with each variable reference inside the block.  */

void
ssa_propagation_engine::simulate_block (basic_block block)
{
  gimple_stmt_iterator gsi;

  /* There is nothing to do for the exit block.  */
  if (block == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nSimulating block %d\n", block->index);

  /* Always simulate PHI nodes, even if we have simulated this block
     before.  */
  for (gsi = gsi_start_phis (block); !gsi_end_p (gsi); gsi_next (&gsi))
    simulate_stmt (gsi_stmt (gsi));

  /* If this is the first time we've simulated this block, then we
     must simulate each of its statements.  */
  if (! (block->flags & BB_VISITED))
    {
      gimple_stmt_iterator j;
      unsigned int normal_edge_count;
      edge e, normal_edge;
      edge_iterator ei;

      for (j = gsi_start_bb (block); !gsi_end_p (j); gsi_next (&j))
	simulate_stmt (gsi_stmt (j));

      /* Note that we have simulated this block.  */
      block->flags |= BB_VISITED;

      /* We cannot predict when abnormal and EH edges will be executed, so
	 once a block is considered executable, we consider any
	 outgoing abnormal edges as executable.

	 TODO: This is not exactly true.  Simplifying statement might
	 prove it non-throwing and also computed goto can be handled
	 when destination is known.

	 At the same time, if this block has only one successor that is
	 reached by non-abnormal edges, then add that successor to the
	 worklist.  */
      normal_edge_count = 0;
      normal_edge = NULL;
      FOR_EACH_EDGE (e, ei, block->succs)
	{
	  if (e->flags & (EDGE_ABNORMAL | EDGE_EH))
	    add_control_edge (e);
	  else
	    {
	      normal_edge_count++;
	      normal_edge = e;
	    }
	}

      if (normal_edge_count == 1)
	add_control_edge (normal_edge);
    }
}


/* Initialize local data structures and work lists.  */

static void
ssa_prop_init (void)
{
  edge e;
  edge_iterator ei;
  basic_block bb;

  /* Worklists of SSA edges.  */
  ssa_edge_worklist = BITMAP_ALLOC (NULL);
  bitmap_tree_view (ssa_edge_worklist);

  /* Worklist of basic-blocks.  */
  bb_to_cfg_order = XNEWVEC (int, last_basic_block_for_fn (cfun) + 1);
  cfg_order_to_bb = XNEWVEC (int, n_basic_blocks_for_fn (cfun));
  int n = pre_and_rev_post_order_compute_fn (cfun, NULL,
					     cfg_order_to_bb, false);
  for (int i = 0; i < n; ++i)
    bb_to_cfg_order[cfg_order_to_bb[i]] = i;
  cfg_blocks = BITMAP_ALLOC (NULL);

  /* Initially assume that every edge in the CFG is not executable.
     (including the edges coming out of the entry block).  Mark blocks
     as not visited, blocks not yet visited will have all their statements
     simulated once an incoming edge gets executable.  */
  set_gimple_stmt_max_uid (cfun, 0);
  for (int i = 0; i < n; ++i)
    {
      gimple_stmt_iterator si;
      bb = BASIC_BLOCK_FOR_FN (cfun, cfg_order_to_bb[i]);

      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	}

      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	}

      bb->flags &= ~BB_VISITED;
      FOR_EACH_EDGE (e, ei, bb->succs)
	e->flags &= ~EDGE_EXECUTABLE;
    }
  uid_to_stmt.safe_grow (gimple_stmt_max_uid (cfun), true);
}


/* Free allocated storage.  */

static void
ssa_prop_fini (void)
{
  BITMAP_FREE (cfg_blocks);
  free (bb_to_cfg_order);
  free (cfg_order_to_bb);
  BITMAP_FREE (ssa_edge_worklist);
  uid_to_stmt.release ();
}


/* Entry point to the propagation engine.

   The VISIT_STMT virtual function is called for every statement
   visited and the VISIT_PHI virtual function is called for every PHI
   node visited.  */

void
ssa_propagation_engine::ssa_propagate (void)
{
  ssa_prop_init ();

  curr_order = 0;

  /* Iterate until the worklists are empty.  We iterate both blocks
     and stmts in RPO order, prioritizing backedge processing.
     Seed the algorithm by adding the successors of the entry block to the
     edge worklist.  */
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs)
    {
      e->flags &= ~EDGE_EXECUTABLE;
      add_control_edge (e);
    }
  while (1)
    {
      int next_block_order = (bitmap_empty_p (cfg_blocks)
			      ? -1 : bitmap_first_set_bit (cfg_blocks));
      int next_stmt_uid = (bitmap_empty_p (ssa_edge_worklist)
			   ? -1 : bitmap_first_set_bit (ssa_edge_worklist));
      if (next_block_order == -1 && next_stmt_uid == -1)
	break;

      int next_stmt_bb_order = -1;
      gimple *next_stmt = NULL;
      if (next_stmt_uid != -1)
	{
	  next_stmt = uid_to_stmt[next_stmt_uid];
	  next_stmt_bb_order = bb_to_cfg_order[gimple_bb (next_stmt)->index];
	}

      /* Pull the next block to simulate off the worklist if it comes first.  */
      if (next_block_order != -1
	  && (next_stmt_bb_order == -1
	      || next_block_order <= next_stmt_bb_order))
	{
	  curr_order = next_block_order;
	  bitmap_clear_bit (cfg_blocks, next_block_order);
	  basic_block bb
	    = BASIC_BLOCK_FOR_FN (cfun, cfg_order_to_bb [next_block_order]);
	  simulate_block (bb);
	}
      /* Else simulate from the SSA edge worklist.  */
      else
	{
	  curr_order = next_stmt_bb_order;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "\nSimulating statement: ");
	      print_gimple_stmt (dump_file, next_stmt, 0, dump_flags);
	    }
	  simulate_stmt (next_stmt);
	}
    }

  ssa_prop_fini ();
}

/* Return true if STMT is of the form 'mem_ref = RHS', where 'mem_ref'
   is a non-volatile pointer dereference, a structure reference or a
   reference to a single _DECL.  Ignore volatile memory references
   because they are not interesting for the optimizers.  */

bool
stmt_makes_single_store (gimple *stmt)
{
  tree lhs;

  if (gimple_code (stmt) != GIMPLE_ASSIGN
      && gimple_code (stmt) != GIMPLE_CALL)
    return false;

  if (!gimple_vdef (stmt))
    return false;

  lhs = gimple_get_lhs (stmt);

  /* A call statement may have a null LHS.  */
  if (!lhs)
    return false;

  return (!TREE_THIS_VOLATILE (lhs)
          && (DECL_P (lhs)
	      || REFERENCE_CLASS_P (lhs)));
}


/* Propagation statistics.  */
struct prop_stats_d
{
  long num_const_prop;
  long num_copy_prop;
  long num_stmts_folded;
};

static struct prop_stats_d prop_stats;

// range_query default methods to drive from a value_of_expr() ranther than
// range_of_expr.

tree
substitute_and_fold_engine::value_on_edge (edge, tree expr)
{
  return value_of_expr (expr);
}

tree
substitute_and_fold_engine::value_of_stmt (gimple *stmt, tree name)
{
  if (!name)
    name = gimple_get_lhs (stmt);

  gcc_checking_assert (!name || name == gimple_get_lhs (stmt));

  if (name)
    return value_of_expr (name);
  return NULL_TREE;
}

bool
substitute_and_fold_engine::range_of_expr (vrange &, tree, gimple *)
{
  return false;
}

/* Replace USE references in statement STMT with the values stored in
   PROP_VALUE. Return true if at least one reference was replaced.  */

bool
substitute_and_fold_engine::replace_uses_in (gimple *stmt)
{
  bool replaced = false;
  use_operand_p use;
  ssa_op_iter iter;

  FOR_EACH_SSA_USE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      tree tuse = USE_FROM_PTR (use);
      tree val = value_of_expr (tuse, stmt);

      if (val == tuse || val == NULL_TREE)
	continue;

      if (gimple_code (stmt) == GIMPLE_ASM
	  && !may_propagate_copy_into_asm (tuse))
	continue;

      if (!may_propagate_copy (tuse, val))
	continue;

      if (TREE_CODE (val) != SSA_NAME)
	prop_stats.num_const_prop++;
      else
	prop_stats.num_copy_prop++;

      propagate_value (use, val);

      replaced = true;
    }

  return replaced;
}


/* Replace propagated values into all the arguments for PHI using the
   values from PROP_VALUE.  */

bool
substitute_and_fold_engine::replace_phi_args_in (gphi *phi)
{
  size_t i;
  bool replaced = false;

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg_def (phi, i);

      if (TREE_CODE (arg) == SSA_NAME)
	{
	  edge e = gimple_phi_arg_edge (phi, i);
	  tree val = value_on_edge (e, arg);

	  if (val && val != arg && may_propagate_copy (arg, val))
	    {
	      if (TREE_CODE (val) != SSA_NAME)
		prop_stats.num_const_prop++;
	      else
		prop_stats.num_copy_prop++;

	      propagate_value (PHI_ARG_DEF_PTR (phi, i), val);
	      replaced = true;

	      /* If we propagated a copy and this argument flows
		 through an abnormal edge, update the replacement
		 accordingly.  */
	      if (TREE_CODE (val) == SSA_NAME
		  && e->flags & EDGE_ABNORMAL
		  && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (val))
		{
		  /* This can only occur for virtual operands, since
		     for the real ones SSA_NAME_OCCURS_IN_ABNORMAL_PHI (val))
		     would prevent replacement.  */
		  gcc_checking_assert (virtual_operand_p (val));
		  SSA_NAME_OCCURS_IN_ABNORMAL_PHI (val) = 1;
		}
	    }
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (!replaced)
	fprintf (dump_file, "No folding possible\n");
      else
	{
	  fprintf (dump_file, "Folded into: ");
	  print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}
    }

  return replaced;
}


class substitute_and_fold_dom_walker : public dom_walker
{
public:
    substitute_and_fold_dom_walker (cdi_direction direction,
				    class substitute_and_fold_engine *engine)
	: dom_walker (direction),
          something_changed (false),
	  substitute_and_fold_engine (engine)
    {
      dceworklist = BITMAP_ALLOC (NULL);
      stmts_to_fixup.create (0);
      need_eh_cleanup = BITMAP_ALLOC (NULL);
      need_ab_cleanup = BITMAP_ALLOC (NULL);
    }
    ~substitute_and_fold_dom_walker ()
    {
      BITMAP_FREE (dceworklist);
      stmts_to_fixup.release ();
      BITMAP_FREE (need_eh_cleanup);
      BITMAP_FREE (need_ab_cleanup);
    }

    edge before_dom_children (basic_block) final override;
    void after_dom_children (basic_block bb) final override
    {
      substitute_and_fold_engine->post_fold_bb (bb);
    }

    bool something_changed;
    bitmap dceworklist;
    vec<gimple *> stmts_to_fixup;
    bitmap need_eh_cleanup;
    bitmap need_ab_cleanup;

    class substitute_and_fold_engine *substitute_and_fold_engine;

private:
    void foreach_new_stmt_in_bb (gimple_stmt_iterator old_gsi,
				 gimple_stmt_iterator new_gsi);
};

/* Call post_new_stmt for each new statement that has been added
   to the current BB.  OLD_GSI is the statement iterator before the BB
   changes ocurred.  NEW_GSI is the iterator which may contain new
   statements.  */

void
substitute_and_fold_dom_walker::foreach_new_stmt_in_bb
				(gimple_stmt_iterator old_gsi,
				 gimple_stmt_iterator new_gsi)
{
  basic_block bb = gsi_bb (new_gsi);
  if (gsi_end_p (old_gsi))
    old_gsi = gsi_start_bb (bb);
  else
    gsi_next (&old_gsi);
  while (gsi_stmt (old_gsi) != gsi_stmt (new_gsi))
    {
      gimple *stmt = gsi_stmt (old_gsi);
      substitute_and_fold_engine->post_new_stmt (stmt);
      gsi_next (&old_gsi);
    }
}

bool
substitute_and_fold_engine::propagate_into_phi_args (basic_block bb)
{
  edge e;
  edge_iterator ei;
  bool propagated = false;

  /* Visit BB successor PHI nodes and replace PHI args.  */
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      for (gphi_iterator gpi = gsi_start_phis (e->dest);
	   !gsi_end_p (gpi); gsi_next (&gpi))
	{
	  gphi *phi = gpi.phi ();
	  use_operand_p use_p = PHI_ARG_DEF_PTR_FROM_EDGE (phi, e);
	  tree arg = USE_FROM_PTR (use_p);
	  if (TREE_CODE (arg) != SSA_NAME
	      || virtual_operand_p (arg))
	    continue;
	  tree val = value_on_edge (e, arg);
	  if (val
	      && is_gimple_min_invariant (val)
	      && may_propagate_copy (arg, val))
	    {
	      propagate_value (use_p, val);
	      propagated = true;
	    }
	}
    }
  return propagated;
}

edge
substitute_and_fold_dom_walker::before_dom_children (basic_block bb)
{
  substitute_and_fold_engine->pre_fold_bb (bb);

  /* Propagate known values into PHI nodes.  */
  for (gphi_iterator i = gsi_start_phis (bb);
       !gsi_end_p (i);
       gsi_next (&i))
    {
      gphi *phi = i.phi ();
      tree res = gimple_phi_result (phi);
      if (virtual_operand_p (res))
	continue;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Folding PHI node: ");
	  print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
	}
      if (res && TREE_CODE (res) == SSA_NAME)
	{
	  tree sprime = substitute_and_fold_engine->value_of_expr (res, phi);
	  if (sprime
	      && sprime != res
	      && may_propagate_copy (res, sprime))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Queued PHI for removal.  Folds to: ");
		  print_generic_expr (dump_file, sprime);
		  fprintf (dump_file, "\n");
		}
	      bitmap_set_bit (dceworklist, SSA_NAME_VERSION (res));
	      continue;
	    }
	}
      something_changed |= substitute_and_fold_engine->replace_phi_args_in (phi);
    }

  /* Propagate known values into stmts.  In some case it exposes
     more trivially deletable stmts to walk backward.  */
  for (gimple_stmt_iterator i = gsi_start_bb (bb);
       !gsi_end_p (i);
       gsi_next (&i))
    {
      bool did_replace;
      gimple *stmt = gsi_stmt (i);

      substitute_and_fold_engine->pre_fold_stmt (stmt);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Folding statement: ");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}

      /* No point propagating into a stmt we have a value for we
         can propagate into all uses.  Mark it for removal instead.  */
      tree lhs = gimple_get_lhs (stmt);
      if (lhs && TREE_CODE (lhs) == SSA_NAME)
	{
	  tree sprime = substitute_and_fold_engine->value_of_stmt (stmt, lhs);
	  if (sprime
	      && sprime != lhs
	      && may_propagate_copy (lhs, sprime)
	      && !stmt_could_throw_p (cfun, stmt)
	      && !gimple_has_side_effects (stmt))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Queued stmt for removal.  Folds to: ");
		  print_generic_expr (dump_file, sprime);
		  fprintf (dump_file, "\n");
		}
	      bitmap_set_bit (dceworklist, SSA_NAME_VERSION (lhs));
	      continue;
	    }
	}

      /* Replace the statement with its folded version and mark it
	 folded.  */
      did_replace = false;
      gimple *old_stmt = stmt;
      bool was_noreturn = false;
      bool can_make_abnormal_goto = false;
      if (is_gimple_call (stmt))
	{
	  was_noreturn = gimple_call_noreturn_p (stmt);
	  can_make_abnormal_goto = stmt_can_make_abnormal_goto (stmt);
	}

      /* Replace real uses in the statement.  */
      did_replace |= substitute_and_fold_engine->replace_uses_in (stmt);

      gimple_stmt_iterator prev_gsi = i;
      gsi_prev (&prev_gsi);

      /* If we made a replacement, fold the statement.  */
      if (did_replace)
	{
	  fold_stmt (&i, follow_single_use_edges);
	  stmt = gsi_stmt (i);
	  gimple_set_modified (stmt, true);
	}
      /* Also fold if we want to fold all statements.  */
      else if (substitute_and_fold_engine->fold_all_stmts
	  && fold_stmt (&i, follow_single_use_edges))
	{
	  did_replace = true;
	  stmt = gsi_stmt (i);
	  gimple_set_modified (stmt, true);
	}

      /* Some statements may be simplified using propagator
	 specific information.  Do this before propagating
	 into the stmt to not disturb pass specific information.  */
      update_stmt_if_modified (stmt);
      if (substitute_and_fold_engine->fold_stmt (&i))
	{
	  did_replace = true;
	  prop_stats.num_stmts_folded++;
	  stmt = gsi_stmt (i);
	  gimple_set_modified (stmt, true);
	}

      /* If this is a control statement the propagator left edges
         unexecuted on force the condition in a way consistent with
	 that.  See PR66945 for cases where the propagator can end
	 up with a different idea of a taken edge than folding
	 (once undefined behavior is involved).  */
      if (gimple_code (stmt) == GIMPLE_COND)
	{
	  if ((EDGE_SUCC (bb, 0)->flags & EDGE_EXECUTABLE)
	      ^ (EDGE_SUCC (bb, 1)->flags & EDGE_EXECUTABLE))
	    {
	      if (((EDGE_SUCC (bb, 0)->flags & EDGE_TRUE_VALUE) != 0)
		  == ((EDGE_SUCC (bb, 0)->flags & EDGE_EXECUTABLE) != 0))
		gimple_cond_make_true (as_a <gcond *> (stmt));
	      else
		gimple_cond_make_false (as_a <gcond *> (stmt));
	      gimple_set_modified (stmt, true);
	      did_replace = true;
	    }
	}

      /* Now cleanup.  */
      if (did_replace)
	{
	  foreach_new_stmt_in_bb (prev_gsi, i);

	  /* If we cleaned up EH information from the statement,
	     remove EH edges.  */
	  if (maybe_clean_or_replace_eh_stmt (old_stmt, stmt))
	    bitmap_set_bit (need_eh_cleanup, bb->index);

	  /* If we turned a call with possible abnormal control transfer
	     into one that doesn't, remove abnormal edges.  */
	  if (can_make_abnormal_goto
	      && !stmt_can_make_abnormal_goto (stmt))
	    bitmap_set_bit (need_ab_cleanup, bb->index);

	  /* If we turned a not noreturn call into a noreturn one
	     schedule it for fixup.  */
	  if (!was_noreturn
	      && is_gimple_call (stmt)
	      && gimple_call_noreturn_p (stmt))
	    stmts_to_fixup.safe_push (stmt);

	  if (gimple_assign_single_p (stmt))
	    {
	      tree rhs = gimple_assign_rhs1 (stmt);

	      if (TREE_CODE (rhs) == ADDR_EXPR)
		recompute_tree_invariant_for_addr_expr (rhs);
	    }

	  /* Determine what needs to be done to update the SSA form.  */
	  update_stmt_if_modified (stmt);
	  if (!is_gimple_debug (stmt))
	    something_changed = true;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (did_replace)
	    {
	      fprintf (dump_file, "Folded into: ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	      fprintf (dump_file, "\n");
	    }
	  else
	    fprintf (dump_file, "Not folded\n");
	}
    }

  something_changed |= substitute_and_fold_engine->propagate_into_phi_args (bb);

  return NULL;
}



/* Perform final substitution and folding of propagated values.
   Process the whole function if BLOCK is null, otherwise only
   process the blocks that BLOCK dominates.  In the latter case,
   it is the caller's responsibility to ensure that dominator
   information is available and up-to-date.

   PROP_VALUE[I] contains the single value that should be substituted
   at every use of SSA name N_I.  If PROP_VALUE is NULL, no values are
   substituted.

   If FOLD_FN is non-NULL the function will be invoked on all statements
   before propagating values for pass specific simplification.

   DO_DCE is true if trivially dead stmts can be removed.

   If DO_DCE is true, the statements within a BB are walked from
   last to first element.  Otherwise we scan from first to last element.

   Return TRUE when something changed.  */

bool
substitute_and_fold_engine::substitute_and_fold (basic_block block)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nSubstituting values and folding statements\n\n");

  memset (&prop_stats, 0, sizeof (prop_stats));

  /* Don't call calculate_dominance_info when iterating over a subgraph.
     Callers that are using the interface this way are likely to want to
     iterate over several disjoint subgraphs, and it would be expensive
     in enable-checking builds to revalidate the whole dominance tree
     each time.  */
  if (block)
    gcc_assert (dom_info_state (CDI_DOMINATORS));
  else
    calculate_dominance_info (CDI_DOMINATORS);
  substitute_and_fold_dom_walker walker (CDI_DOMINATORS, this);
  walker.walk (block ? block : ENTRY_BLOCK_PTR_FOR_FN (cfun));

  simple_dce_from_worklist (walker.dceworklist, walker.need_eh_cleanup);
  if (!bitmap_empty_p (walker.need_eh_cleanup))
    gimple_purge_all_dead_eh_edges (walker.need_eh_cleanup);
  if (!bitmap_empty_p (walker.need_ab_cleanup))
    gimple_purge_all_dead_abnormal_call_edges (walker.need_ab_cleanup);

  /* Fixup stmts that became noreturn calls.  This may require splitting
     blocks and thus isn't possible during the dominator walk.  Do this
     in reverse order so we don't inadvertedly remove a stmt we want to
     fixup by visiting a dominating now noreturn call first.  */
  while (!walker.stmts_to_fixup.is_empty ())
    {
      gimple *stmt = walker.stmts_to_fixup.pop ();
      if (dump_file && dump_flags & TDF_DETAILS)
	{
	  fprintf (dump_file, "Fixing up noreturn call ");
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "\n");
	}
      fixup_noreturn_call (stmt);
    }

  statistics_counter_event (cfun, "Constants propagated",
			    prop_stats.num_const_prop);
  statistics_counter_event (cfun, "Copies propagated",
			    prop_stats.num_copy_prop);
  statistics_counter_event (cfun, "Statements folded",
			    prop_stats.num_stmts_folded);

  return walker.something_changed;
}


/* Return true if we may propagate ORIG into DEST, false otherwise.
   If DEST_NOT_PHI_ARG_P is true then assume the propagation does
   not happen into a PHI argument which relaxes some constraints.  */

bool
may_propagate_copy (tree dest, tree orig, bool dest_not_phi_arg_p)
{
  tree type_d = TREE_TYPE (dest);
  tree type_o = TREE_TYPE (orig);

  /* If ORIG is a default definition which flows in from an abnormal edge
     then the copy can be propagated.  It is important that we do so to avoid
     uninitialized copies.  */
  if (TREE_CODE (orig) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (orig)
      && SSA_NAME_IS_DEFAULT_DEF (orig)
      && (SSA_NAME_VAR (orig) == NULL_TREE
	  || VAR_P (SSA_NAME_VAR (orig))))
    ;
  /* Otherwise if ORIG just flows in from an abnormal edge then the copy cannot
     be propagated.  */
  else if (TREE_CODE (orig) == SSA_NAME
	   && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (orig))
    return false;
  /* Similarly if DEST flows in from an abnormal edge then the copy cannot be
     propagated.  If we know we do not propagate into a PHI argument this
     does not apply.  */
  else if (!dest_not_phi_arg_p
	   && TREE_CODE (dest) == SSA_NAME
	   && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (dest))
    return false;

  /* Do not copy between types for which we *do* need a conversion.  */
  if (!useless_type_conversion_p (type_d, type_o))
    return false;

  /* Generally propagating virtual operands is not ok as that may
     create overlapping life-ranges.  */
  if (TREE_CODE (dest) == SSA_NAME && virtual_operand_p (dest))
    return false;

  /* Anything else is OK.  */
  return true;
}

/* Like may_propagate_copy, but use as the destination expression
   the principal expression (typically, the RHS) contained in
   statement DEST.  This is more efficient when working with the
   gimple tuples representation.  */

bool
may_propagate_copy_into_stmt (gimple *dest, tree orig)
{
  tree type_d;
  tree type_o;

  /* If the statement is a switch or a single-rhs assignment,
     then the expression to be replaced by the propagation may
     be an SSA_NAME.  Fortunately, there is an explicit tree
     for the expression, so we delegate to may_propagate_copy.  */

  if (gimple_assign_single_p (dest))
    return may_propagate_copy (gimple_assign_rhs1 (dest), orig, true);
  else if (gswitch *dest_swtch = dyn_cast <gswitch *> (dest))
    return may_propagate_copy (gimple_switch_index (dest_swtch), orig, true);

  /* In other cases, the expression is not materialized, so there
     is no destination to pass to may_propagate_copy.  On the other
     hand, the expression cannot be an SSA_NAME, so the analysis
     is much simpler.  */

  if (TREE_CODE (orig) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (orig))
    return false;

  if (is_gimple_assign (dest))
    type_d = TREE_TYPE (gimple_assign_lhs (dest));
  else if (gimple_code (dest) == GIMPLE_COND)
    type_d = boolean_type_node;
  else if (is_gimple_call (dest)
           && gimple_call_lhs (dest) != NULL_TREE)
    type_d = TREE_TYPE (gimple_call_lhs (dest));
  else
    gcc_unreachable ();

  type_o = TREE_TYPE (orig);

  if (!useless_type_conversion_p (type_d, type_o))
    return false;

  return true;
}

/* Similarly, but we know that we're propagating into an ASM_EXPR.  */

bool
may_propagate_copy_into_asm (tree dest ATTRIBUTE_UNUSED)
{
  return true;
}


/* Replace *OP_P with value VAL (assumed to be a constant or another SSA_NAME).

   Use this version when not const/copy propagating values.  For example,
   PRE uses this version when building expressions as they would appear
   in specific blocks taking into account actions of PHI nodes.

   The statement in which an expression has been replaced should be
   folded using fold_stmt_inplace.  */

void
replace_exp (use_operand_p op_p, tree val)
{
  if (TREE_CODE (val) == SSA_NAME || CONSTANT_CLASS_P (val))
    SET_USE (op_p, val);
  else
    SET_USE (op_p, unshare_expr (val));
}


/* Propagate the value VAL (assumed to be a constant or another SSA_NAME)
   into the operand pointed to by OP_P.

   Use this version for const/copy propagation as it will perform additional
   checks to ensure validity of the const/copy propagation.  */

void
propagate_value (use_operand_p op_p, tree val)
{
  if (flag_checking)
    gcc_assert (may_propagate_copy (USE_FROM_PTR (op_p), val,
				    !is_a <gphi *> (USE_STMT (op_p))));
  replace_exp (op_p, val);
}


/* Propagate the value VAL (assumed to be a constant or another SSA_NAME)
   into the tree pointed to by OP_P.

   Use this version for const/copy propagation when SSA operands are not
   available.  It will perform the additional checks to ensure validity of
   the const/copy propagation, but will not update any operand information.
   Be sure to mark the stmt as modified.  */

void
propagate_tree_value (tree *op_p, tree val)
{
  if (TREE_CODE (val) == SSA_NAME)
    *op_p = val;
  else
    *op_p = unshare_expr (val);
}


/* Like propagate_tree_value, but use as the operand to replace
   the principal expression (typically, the RHS) contained in the
   statement referenced by iterator GSI.  Note that it is not
   always possible to update the statement in-place, so a new
   statement may be created to replace the original.  */

void
propagate_tree_value_into_stmt (gimple_stmt_iterator *gsi, tree val)
{
  gimple *stmt = gsi_stmt (*gsi);

  if (is_gimple_assign (stmt))
    {
      tree expr = NULL_TREE;
      if (gimple_assign_single_p (stmt))
        expr = gimple_assign_rhs1 (stmt);
      propagate_tree_value (&expr, val);
      gimple_assign_set_rhs_from_tree (gsi, expr);
    }
  else if (gcond *cond_stmt = dyn_cast <gcond *> (stmt))
    {
      tree lhs = NULL_TREE;
      tree rhs = build_zero_cst (TREE_TYPE (val));
      propagate_tree_value (&lhs, val);
      gimple_cond_set_code (cond_stmt, NE_EXPR);
      gimple_cond_set_lhs (cond_stmt, lhs);
      gimple_cond_set_rhs (cond_stmt, rhs);
    }
  else if (is_gimple_call (stmt)
           && gimple_call_lhs (stmt) != NULL_TREE)
    {
      tree expr = NULL_TREE;
      propagate_tree_value (&expr, val);
      replace_call_with_value (gsi, expr);
    }
  else if (gswitch *swtch_stmt = dyn_cast <gswitch *> (stmt))
    propagate_tree_value (gimple_switch_index_ptr (swtch_stmt), val);
  else
    gcc_unreachable ();
}

/* Check exits of each loop in FUN, walk over loop closed PHIs in
   each exit basic block and propagate degenerate PHIs.  */

unsigned
clean_up_loop_closed_phi (function *fun)
{
  gphi *phi;
  tree rhs;
  tree lhs;
  gphi_iterator gsi;

  /* Avoid possibly quadratic work when scanning for loop exits across
   all loops of a nest.  */
  if (!loops_state_satisfies_p (LOOPS_HAVE_RECORDED_EXITS))
    return 0;

  /* replace_uses_by might purge dead EH edges and we want it to also
     remove dominated blocks.  */
  calculate_dominance_info  (CDI_DOMINATORS);

  /* Walk over loop in function.  */
  for (auto loop : loops_list (fun, 0))
    {
      /* Check each exit edege of loop.  */
      auto_vec<edge> exits = get_loop_exit_edges (loop);
      for (edge e : exits)
	if (single_pred_p (e->dest))
	  /* Walk over loop-closed PHIs.  */
	  for (gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi);)
	    {
	      phi = gsi.phi ();
	      rhs = gimple_phi_arg_def (phi, 0);
	      lhs = gimple_phi_result (phi);

	      if (virtual_operand_p (rhs))
		{
		  imm_use_iterator iter;
		  use_operand_p use_p;
		  gimple *stmt;

		  FOR_EACH_IMM_USE_STMT (stmt, iter, lhs)
		    FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
		      SET_USE (use_p, rhs);

		  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
		    SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs) = 1;
		  remove_phi_node (&gsi, true);
		}
	      else if (may_propagate_copy (lhs, rhs))
		{
		  /* Dump details.  */
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "  Replacing '");
		      print_generic_expr (dump_file, lhs, dump_flags);
		      fprintf (dump_file, "' with '");
		      print_generic_expr (dump_file, rhs, dump_flags);
		      fprintf (dump_file, "'\n");
		    }

		  replace_uses_by (lhs, rhs);
		  remove_phi_node (&gsi, true);
		}
	      else
		gsi_next (&gsi);
	    }
    }

  return 0;
}
