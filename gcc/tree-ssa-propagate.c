/* Generic SSA value propagation engine.
   Copyright (C) 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "rtl.h"
#include "tm_p.h"
#include "ggc.h"
#include "basic-block.h"
#include "output.h"
#include "errors.h"
#include "expr.h"
#include "function.h"
#include "diagnostic.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "langhooks.h"
#include "varray.h"
#include "vec.h"

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

	VARYING_SSA_EDGES contains the list of statements that feed
	    from statements that produce an SSA_PROP_VARYING result.
	    These are simulated first to speed up processing.

	INTERESTING_SSA_EDGES contains the list of statements that
	    feed from statements that produce an SSA_PROP_INTERESTING
	    result.

   5- Simulation terminates when all three work lists are drained.

   Before calling ssa_propagate, it is important to clear
   DONT_SIMULATE_AGAIN for all the statements in the program that
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

/* Function pointers used to parameterize the propagation engine.  */
static ssa_prop_visit_stmt_fn ssa_prop_visit_stmt;
static ssa_prop_visit_phi_fn ssa_prop_visit_phi;

/* Use the TREE_DEPRECATED bitflag to mark statements that have been
   added to one of the SSA edges worklists.  This flag is used to
   avoid visiting statements unnecessarily when draining an SSA edge
   worklist.  If while simulating a basic block, we find a statement with
   STMT_IN_SSA_EDGE_WORKLIST set, we clear it to prevent SSA edge
   processing from visiting it again.  */
#define STMT_IN_SSA_EDGE_WORKLIST(T)	TREE_DEPRECATED (T)

/* A bitmap to keep track of executable blocks in the CFG.  */
static sbitmap executable_blocks;

/* Array of control flow edges on the worklist.  */
static GTY(()) varray_type cfg_blocks = NULL;

static unsigned int cfg_blocks_num = 0;
static int cfg_blocks_tail;
static int cfg_blocks_head;

static sbitmap bb_in_list;

/* Worklist of SSA edges which will need reexamination as their
   definition has changed.  SSA edges are def-use edges in the SSA
   web.  For each D-U edge, we store the target statement or PHI node
   U.  */
static GTY(()) VEC(tree) *interesting_ssa_edges;

/* Identical to INTERESTING_SSA_EDGES.  For performance reasons, the
   list of SSA edges is split into two.  One contains all SSA edges
   who need to be reexamined because their lattice value changed to
   varying (this worklist), and the other contains all other SSA edges
   to be reexamined (INTERESTING_SSA_EDGES).

   Since most values in the program are VARYING, the ideal situation
   is to move them to that lattice value as quickly as possible.
   Thus, it doesn't make sense to process any other type of lattice
   value until all VARYING values are propagated fully, which is one
   thing using the VARYING worklist achieves.  In addition, if we
   don't use a separate worklist for VARYING edges, we end up with
   situations where lattice values move from
   UNDEFINED->INTERESTING->VARYING instead of UNDEFINED->VARYING.  */
static GTY(()) VEC(tree) *varying_ssa_edges;


/* Return true if the block worklist empty.  */

static inline bool
cfg_blocks_empty_p (void)
{
  return (cfg_blocks_num == 0);
}


/* Add a basic block to the worklist.  The block must not be already
   in the worklist, and it must not be the ENTRY or EXIT block.  */

static void 
cfg_blocks_add (basic_block bb)
{
  gcc_assert (bb != ENTRY_BLOCK_PTR && bb != EXIT_BLOCK_PTR);
  gcc_assert (!TEST_BIT (bb_in_list, bb->index));

  if (cfg_blocks_empty_p ())
    {
      cfg_blocks_tail = cfg_blocks_head = 0;
      cfg_blocks_num = 1;
    }
  else
    {
      cfg_blocks_num++;
      if (cfg_blocks_num > VARRAY_SIZE (cfg_blocks))
	{
	  /* We have to grow the array now.  Adjust to queue to occupy the
	     full space of the original array.  */
	  cfg_blocks_tail = VARRAY_SIZE (cfg_blocks);
	  cfg_blocks_head = 0;
	  VARRAY_GROW (cfg_blocks, 2 * VARRAY_SIZE (cfg_blocks));
	}
      else
	cfg_blocks_tail = (cfg_blocks_tail + 1) % VARRAY_SIZE (cfg_blocks);
    }

  VARRAY_BB (cfg_blocks, cfg_blocks_tail) = bb;
  SET_BIT (bb_in_list, bb->index);
}


/* Remove a block from the worklist.  */

static basic_block
cfg_blocks_get (void)
{
  basic_block bb;

  bb = VARRAY_BB (cfg_blocks, cfg_blocks_head);

  gcc_assert (!cfg_blocks_empty_p ());
  gcc_assert (bb);

  cfg_blocks_head = (cfg_blocks_head + 1) % VARRAY_SIZE (cfg_blocks);
  --cfg_blocks_num;
  RESET_BIT (bb_in_list, bb->index);

  return bb;
}


/* We have just defined a new value for VAR.  If IS_VARYING is true,
   add all immediate uses of VAR to VARYING_SSA_EDGES, otherwise add
   them to INTERESTING_SSA_EDGES.  */

static void
add_ssa_edge (tree var, bool is_varying)
{
  tree stmt = SSA_NAME_DEF_STMT (var);
  dataflow_t df = get_immediate_uses (stmt);
  int num_uses = num_immediate_uses (df);
  int i;

  for (i = 0; i < num_uses; i++)
    {
      tree use_stmt = immediate_use (df, i);

      if (!DONT_SIMULATE_AGAIN (use_stmt)
	  && !STMT_IN_SSA_EDGE_WORKLIST (use_stmt))
	{
	  STMT_IN_SSA_EDGE_WORKLIST (use_stmt) = 1;
	  if (is_varying)
	    VEC_safe_push (tree, varying_ssa_edges, use_stmt);
	  else
	    VEC_safe_push (tree, interesting_ssa_edges, use_stmt);
	}
    }
}


/* Add edge E to the control flow worklist.  */

static void
add_control_edge (edge e)
{
  basic_block bb = e->dest;
  if (bb == EXIT_BLOCK_PTR)
    return;

  /* If the edge had already been executed, skip it.  */
  if (e->flags & EDGE_EXECUTABLE)
    return;

  e->flags |= EDGE_EXECUTABLE;

  /* If the block is already in the list, we're done.  */
  if (TEST_BIT (bb_in_list, bb->index))
    return;

  cfg_blocks_add (bb);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Adding Destination of edge (%d -> %d) to worklist\n\n",
	e->src->index, e->dest->index);
}


/* Simulate the execution of STMT and update the work lists accordingly.  */

static void
simulate_stmt (tree stmt)
{
  enum ssa_prop_result val = SSA_PROP_NOT_INTERESTING;
  edge taken_edge = NULL;
  tree output_name = NULL_TREE;

  /* Don't bother visiting statements that are already
     considered varying by the propagator.  */
  if (DONT_SIMULATE_AGAIN (stmt))
    return;

  if (TREE_CODE (stmt) == PHI_NODE)
    {
      val = ssa_prop_visit_phi (stmt);
      output_name = PHI_RESULT (stmt);
    }
  else
    val = ssa_prop_visit_stmt (stmt, &taken_edge, &output_name);

  if (val == SSA_PROP_VARYING)
    {
      DONT_SIMULATE_AGAIN (stmt) = 1;

      /* If the statement produced a new varying value, add the SSA
	 edges coming out of OUTPUT_NAME.  */
      if (output_name)
	add_ssa_edge (output_name, true);

      /* If STMT transfers control out of its basic block, add
	 all outgoing edges to the work list.  */
      if (stmt_ends_bb_p (stmt))
	{
	  edge e;
	  edge_iterator ei;
	  basic_block bb = bb_for_stmt (stmt);
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    add_control_edge (e);
	}
    }
  else if (val == SSA_PROP_INTERESTING)
    {
      /* If the statement produced new value, add the SSA edges coming
	 out of OUTPUT_NAME.  */
      if (output_name)
	add_ssa_edge (output_name, false);

      /* If we know which edge is going to be taken out of this block,
	 add it to the CFG work list.  */
      if (taken_edge)
	add_control_edge (taken_edge);
    }
}

/* Process an SSA edge worklist.  WORKLIST is the SSA edge worklist to
   drain.  This pops statements off the given WORKLIST and processes
   them until there are no more statements on WORKLIST.
   We take a pointer to WORKLIST because it may be reallocated when an
   SSA edge is added to it in simulate_stmt.  */

static void
process_ssa_edge_worklist (VEC(tree) **worklist)
{
  /* Drain the entire worklist.  */
  while (VEC_length (tree, *worklist) > 0)
    {
      basic_block bb;

      /* Pull the statement to simulate off the worklist.  */
      tree stmt = VEC_pop (tree, *worklist);

      /* If this statement was already visited by simulate_block, then
	 we don't need to visit it again here.  */
      if (!STMT_IN_SSA_EDGE_WORKLIST (stmt))
	continue;

      /* STMT is no longer in a worklist.  */
      STMT_IN_SSA_EDGE_WORKLIST (stmt) = 0;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nSimulating statement (from ssa_edges): ");
	  print_generic_stmt (dump_file, stmt, dump_flags);
	}

      bb = bb_for_stmt (stmt);

      /* PHI nodes are always visited, regardless of whether or not
	 the destination block is executable.  Otherwise, visit the
	 statement only if its block is marked executable.  */
      if (TREE_CODE (stmt) == PHI_NODE
	  || TEST_BIT (executable_blocks, bb->index))
	simulate_stmt (stmt);
    }
}


/* Simulate the execution of BLOCK.  Evaluate the statement associated
   with each variable reference inside the block.  */

static void
simulate_block (basic_block block)
{
  tree phi;

  /* There is nothing to do for the exit block.  */
  if (block == EXIT_BLOCK_PTR)
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nSimulating block %d\n", block->index);

  /* Always simulate PHI nodes, even if we have simulated this block
     before.  */
  for (phi = phi_nodes (block); phi; phi = PHI_CHAIN (phi))
    simulate_stmt (phi);

  /* If this is the first time we've simulated this block, then we
     must simulate each of its statements.  */
  if (!TEST_BIT (executable_blocks, block->index))
    {
      block_stmt_iterator j;
      unsigned int normal_edge_count;
      edge e, normal_edge;
      edge_iterator ei;

      /* Note that we have simulated this block.  */
      SET_BIT (executable_blocks, block->index);

      for (j = bsi_start (block); !bsi_end_p (j); bsi_next (&j))
	{
	  tree stmt = bsi_stmt (j);

	  /* If this statement is already in the worklist then
	     "cancel" it.  The reevaluation implied by the worklist
	     entry will produce the same value we generate here and
	     thus reevaluating it again from the worklist is
	     pointless.  */
	  if (STMT_IN_SSA_EDGE_WORKLIST (stmt))
	    STMT_IN_SSA_EDGE_WORKLIST (stmt) = 0;

	  simulate_stmt (stmt);
	}

      /* We can not predict when abnormal edges will be executed, so
	 once a block is considered executable, we consider any
	 outgoing abnormal edges as executable.

	 At the same time, if this block has only one successor that is
	 reached by non-abnormal edges, then add that successor to the
	 worklist.  */
      normal_edge_count = 0;
      normal_edge = NULL;
      FOR_EACH_EDGE (e, ei, block->succs)
	{
	  if (e->flags & EDGE_ABNORMAL)
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
  interesting_ssa_edges = VEC_alloc (tree, 20);
  varying_ssa_edges = VEC_alloc (tree, 20);

  executable_blocks = sbitmap_alloc (last_basic_block);
  sbitmap_zero (executable_blocks);

  bb_in_list = sbitmap_alloc (last_basic_block);
  sbitmap_zero (bb_in_list);

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_immediate_uses (dump_file);

  VARRAY_BB_INIT (cfg_blocks, 20, "cfg_blocks");

  /* Initially assume that every edge in the CFG is not executable
     (including the edges coming out of ENTRY_BLOCK_PTR).  */
  FOR_ALL_BB (bb)
    {
      block_stmt_iterator si;

      for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
	STMT_IN_SSA_EDGE_WORKLIST (bsi_stmt (si)) = 0;

      FOR_EACH_EDGE (e, ei, bb->succs)
	e->flags &= ~EDGE_EXECUTABLE;
    }

  /* Seed the algorithm by adding the successors of the entry block to the
     edge worklist.  */
  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR->succs)
    add_control_edge (e);
}


/* Free allocated storage.  */

static void
ssa_prop_fini (void)
{
  VEC_free (tree, interesting_ssa_edges);
  VEC_free (tree, varying_ssa_edges);
  cfg_blocks = NULL;
  sbitmap_free (bb_in_list);
  sbitmap_free (executable_blocks);
  free_df ();
}


/* Get the main expression from statement STMT.  */

tree
get_rhs (tree stmt)
{
  enum tree_code code = TREE_CODE (stmt);

  switch (code)
    {
    case RETURN_EXPR:
      stmt = TREE_OPERAND (stmt, 0);
      if (!stmt || TREE_CODE (stmt) != MODIFY_EXPR)
	return stmt;
      /* FALLTHRU */

    case MODIFY_EXPR:
      stmt = TREE_OPERAND (stmt, 1);
      if (TREE_CODE (stmt) == WITH_SIZE_EXPR)
	return TREE_OPERAND (stmt, 0);
      else
	return stmt;

    case COND_EXPR:
      return COND_EXPR_COND (stmt);
    case SWITCH_EXPR:
      return SWITCH_COND (stmt);
    case GOTO_EXPR:
      return GOTO_DESTINATION (stmt);
    case LABEL_EXPR:
      return LABEL_EXPR_LABEL (stmt);

    default:
      return stmt;
    }
}


/* Set the main expression of *STMT_P to EXPR.  If EXPR is not a valid
   GIMPLE expression no changes are done and the function returns
   false.  */

bool
set_rhs (tree *stmt_p, tree expr)
{
  tree stmt = *stmt_p, op;
  enum tree_code code = TREE_CODE (expr);
  stmt_ann_t ann;
  tree var;
  ssa_op_iter iter;

  /* Verify the constant folded result is valid gimple.  */
  if (TREE_CODE_CLASS (code) == tcc_binary)
    {
      if (!is_gimple_val (TREE_OPERAND (expr, 0))
	  || !is_gimple_val (TREE_OPERAND (expr, 1)))
	return false;
    }
  else if (TREE_CODE_CLASS (code) == tcc_unary)
    {
      if (!is_gimple_val (TREE_OPERAND (expr, 0)))
	return false;
    }
  else if (code == COMPOUND_EXPR)
    return false;

  switch (TREE_CODE (stmt))
    {
    case RETURN_EXPR:
      op = TREE_OPERAND (stmt, 0);
      if (TREE_CODE (op) != MODIFY_EXPR)
	{
	  TREE_OPERAND (stmt, 0) = expr;
	  break;
	}
      stmt = op;
      /* FALLTHRU */

    case MODIFY_EXPR:
      op = TREE_OPERAND (stmt, 1);
      if (TREE_CODE (op) == WITH_SIZE_EXPR)
	stmt = op;
      TREE_OPERAND (stmt, 1) = expr;
      break;

    case COND_EXPR:
      COND_EXPR_COND (stmt) = expr;
      break;
    case SWITCH_EXPR:
      SWITCH_COND (stmt) = expr;
      break;
    case GOTO_EXPR:
      GOTO_DESTINATION (stmt) = expr;
      break;
    case LABEL_EXPR:
      LABEL_EXPR_LABEL (stmt) = expr;
      break;

    default:
      /* Replace the whole statement with EXPR.  If EXPR has no side
	 effects, then replace *STMT_P with an empty statement.  */
      ann = stmt_ann (stmt);
      *stmt_p = TREE_SIDE_EFFECTS (expr) ? expr : build_empty_stmt ();
      (*stmt_p)->common.ann = (tree_ann_t) ann;

      if (TREE_SIDE_EFFECTS (expr))
	{
	  /* Fix all the SSA_NAMEs created by *STMT_P to point to its new
	     replacement.  */
	  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_ALL_DEFS)
	    {
	      if (TREE_CODE (var) == SSA_NAME)
		SSA_NAME_DEF_STMT (var) = *stmt_p;
	    }
	}
      break;
    }

  return true;
}


/* Entry point to the propagation engine.

   VISIT_STMT is called for every statement visited.
   VISIT_PHI is called for every PHI node visited.  */

void
ssa_propagate (ssa_prop_visit_stmt_fn visit_stmt,
	       ssa_prop_visit_phi_fn visit_phi)
{
  ssa_prop_visit_stmt = visit_stmt;
  ssa_prop_visit_phi = visit_phi;

  ssa_prop_init ();

  /* Iterate until the worklists are empty.  */
  while (!cfg_blocks_empty_p () 
	 || VEC_length (tree, interesting_ssa_edges) > 0
	 || VEC_length (tree, varying_ssa_edges) > 0)
    {
      if (!cfg_blocks_empty_p ())
	{
	  /* Pull the next block to simulate off the worklist.  */
	  basic_block dest_block = cfg_blocks_get ();
	  simulate_block (dest_block);
	}

      /* In order to move things to varying as quickly as
	 possible,process the VARYING_SSA_EDGES worklist first.  */
      process_ssa_edge_worklist (&varying_ssa_edges);

      /* Now process the INTERESTING_SSA_EDGES worklist.  */
      process_ssa_edge_worklist (&interesting_ssa_edges);
    }

  ssa_prop_fini ();
}

#include "gt-tree-ssa-propagate.h"
