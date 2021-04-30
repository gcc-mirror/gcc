/* SSA Jump Threading
   Copyright (C) 2005-2021 Free Software Foundation, Inc.

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
#include "predict.h"
#include "tree.h"
#include "gimple.h"
#include "fold-const.h"
#include "cfgloop.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-threadupdate.h"
#include "tree-ssa-loop.h"
#include "cfganal.h"
#include "tree-pass.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "tree-inline.h"
#include "tree-vectorizer.h"

// Path registry for the backwards threader.  After all paths have been
// registered with register_path(), thread_through_all_blocks() is called
// to modify the CFG.

class back_threader_registry
{
public:
  back_threader_registry (int max_allowable_paths);
  ~back_threader_registry ();
  bool register_path (const vec<basic_block> &, edge taken);
  bool thread_through_all_blocks ();

private:
  vec<vec<basic_block>> m_all_paths;
  jump_thread_path_registry m_lowlevel_registry;
  const int m_max_allowable_paths;
  int m_threaded_paths;
};

// Class to abstract the profitability code for the backwards threader.

class back_threader_profitability
{
public:
  back_threader_profitability (bool speed_p)
    : m_speed_p (speed_p)
  { }
  bool profitable_path_p (const vec<basic_block> &, tree name, edge taken,
			  bool *irreducible_loop = NULL);

private:
  const bool m_speed_p;
};

class thread_jumps
{
public:
  thread_jumps (bool speed_p = true)
    : m_profit (speed_p), m_registry (param_max_fsm_thread_paths)
  { }
  void find_jump_threads_backwards (basic_block bb);
  bool thread_through_all_blocks ();

private:
  void maybe_register_path (const vec<basic_block> &m_path,
			    tree name,
			    edge taken_edge);
  edge find_taken_edge (const vec<basic_block> &path, tree arg);
  void handle_assignment (gimple *stmt, basic_block def_bb);
  void handle_phi (gphi *phi, basic_block def_bb);
  void fsm_find_control_statement_thread_paths (tree name);
  bool check_subpath_and_update_thread_path (basic_block last_bb,
					     basic_block new_bb,
					     int *next_path_length);

  /* Hash to keep track of seen bbs.  */
  hash_set<basic_block> m_visited_bbs;
  /* Current path we're analyzing.  */
  auto_vec<basic_block> m_path;
  /* Tracks if we have recursed through a loop PHI node.  */
  bool m_seen_loop_phi;

  tree m_name;
  back_threader_profitability m_profit;
  back_threader_registry m_registry;
};

// Perform the actual jump threading for the all queued paths.

bool
thread_jumps::thread_through_all_blocks ()
{
  return m_registry.thread_through_all_blocks ();
}

/* Simple helper to get the last statement from BB, which is assumed
   to be a control statement.   Return NULL if the last statement is
   not a control statement.  */

static gimple *
get_gimple_control_stmt (basic_block bb)
{
  gimple_stmt_iterator gsi = gsi_last_nondebug_bb (bb);

  if (gsi_end_p (gsi))
    return NULL;

  gimple *stmt = gsi_stmt (gsi);
  enum gimple_code code = gimple_code (stmt);
  if (code == GIMPLE_COND || code == GIMPLE_SWITCH || code == GIMPLE_GOTO)
    return stmt;
  return NULL;
}

/* Return true if the CFG contains at least one path from START_BB to
   END_BB.  When a path is found, record in PATH the blocks from
   END_BB to START_BB.  LOCAL_VISITED_BBS is used to make sure we
   don't fall into an infinite loop.  Bound the recursion to basic
   blocks belonging to LOOP.  */

static bool
fsm_find_thread_path (basic_block start_bb, basic_block end_bb,
		      vec<basic_block> &path,
		      hash_set<basic_block> &local_visited_bbs,
		      loop_p loop)
{
  if (loop != start_bb->loop_father)
    return false;

  if (start_bb == end_bb)
    {
      path.safe_push (start_bb);
      return true;
    }

  if (!local_visited_bbs.add (start_bb))
    {
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, start_bb->succs)
	if (fsm_find_thread_path (e->dest, end_bb, path, local_visited_bbs,
				  loop))
	  {
	    path.safe_push (start_bb);
	    return true;
	  }
    }

  return false;
}

back_threader_registry::back_threader_registry (int max_allowable_paths)
  : m_max_allowable_paths (max_allowable_paths)
{
  m_all_paths.create (5);
  m_threaded_paths = 0;
}

back_threader_registry::~back_threader_registry ()
{
  m_all_paths.release ();
}

bool
back_threader_registry::thread_through_all_blocks ()
{
  return m_lowlevel_registry.thread_through_all_blocks (true);
}

/* Examine jump threading path PATH and return TRUE if it is profitable to
   thread it, otherwise return FALSE.

   NAME is the SSA_NAME of the variable we found to have a constant
   value on PATH.  If unknown, SSA_NAME is NULL.

   If the taken edge out of the path is known ahead of time it is passed in
   TAKEN_EDGE, otherwise it is NULL.

   CREATES_IRREDUCIBLE_LOOP, if non-null is set to TRUE if threading this path
   would create an irreducible loop.  */

bool
back_threader_profitability::profitable_path_p (const vec<basic_block> &m_path,
						tree name,
						edge taken_edge,
						bool *creates_irreducible_loop)
{
  gcc_checking_assert (!m_path.is_empty ());

  /* We can an empty path here (excluding the DEF block) when the
     statement that makes a conditional generate a compile-time
     constant result is in the same block as the conditional.

     That's not really a jump threading opportunity, but instead is
     simple cprop & simplification.  We could handle it here if we
     wanted by wiring up all the incoming edges.  If we run this
     early in IPA, that might be worth doing.   For now we just
     reject that case.  */
  if (m_path.length () <= 1)
      return false;

  if (m_path.length () > (unsigned) param_max_fsm_thread_length)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  FAIL: FSM jump-thread path not considered: "
		 "the number of basic blocks on the path "
		 "exceeds PARAM_MAX_FSM_THREAD_LENGTH.\n");
      return false;
    }

  int n_insns = 0;
  gimple_stmt_iterator gsi;
  loop_p loop = m_path[0]->loop_father;
  bool path_crosses_loops = false;
  bool threaded_through_latch = false;
  bool multiway_branch_in_path = false;
  bool threaded_multiway_branch = false;
  bool contains_hot_bb = false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Checking profitability of path (backwards): ");

  /* Count the number of instructions on the path: as these instructions
     will have to be duplicated, we will not record the path if there
     are too many instructions on the path.  Also check that all the
     blocks in the path belong to a single loop.  */
  for (unsigned j = 0; j < m_path.length (); j++)
    {
      basic_block bb = m_path[j];

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " bb:%i", bb->index);
      /* Remember, blocks in the path are stored in opposite order
	 in the PATH array.  The last entry in the array represents
	 the block with an outgoing edge that we will redirect to the
	 jump threading path.  Thus we don't care about that block's
	 loop father, nor how many statements are in that block because
	 it will not be copied or whether or not it ends in a multiway
	 branch.  */
      if (j < m_path.length () - 1)
	{
	  int orig_n_insns = n_insns;
	  if (bb->loop_father != loop)
	    {
	      path_crosses_loops = true;
	      break;
	    }

	  /* PHIs in the path will create degenerate PHIS in the
	     copied path which will then get propagated away, so
	     looking at just the duplicate path the PHIs would
	     seem unimportant.

	     But those PHIs, because they're assignments to objects
	     typically with lives that exist outside the thread path,
	     will tend to generate PHIs (or at least new PHI arguments)
	     at points where we leave the thread path and rejoin
	     the original blocks.  So we do want to account for them.

	     We ignore virtual PHIs.  We also ignore cases where BB
	     has a single incoming edge.  That's the most common
	     degenerate PHI we'll see here.  Finally we ignore PHIs
	     that are associated with the value we're tracking as
	     that object likely dies.  */
	  if (EDGE_COUNT (bb->succs) > 1 && EDGE_COUNT (bb->preds) > 1)
	    {
	      for (gphi_iterator gsip = gsi_start_phis (bb);
		   !gsi_end_p (gsip);
		   gsi_next (&gsip))
		{
		  gphi *phi = gsip.phi ();
		  tree dst = gimple_phi_result (phi);

		  /* Note that if both NAME and DST are anonymous
		     SSA_NAMEs, then we do not have enough information
		     to consider them associated.  */
		  if (dst != name
		      && name
		      && TREE_CODE (name) == SSA_NAME
		      && (SSA_NAME_VAR (dst) != SSA_NAME_VAR (name)
			  || !SSA_NAME_VAR (dst))
		      && !virtual_operand_p (dst))
		    ++n_insns;
		}
	    }

	  if (!contains_hot_bb && m_speed_p)
	    contains_hot_bb |= optimize_bb_for_speed_p (bb);
	  for (gsi = gsi_after_labels (bb);
	       !gsi_end_p (gsi);
	       gsi_next_nondebug (&gsi))
	    {
	      /* Do not allow OpenACC loop markers and __builtin_constant_p on
		 threading paths.  The latter is disallowed, because an
		 expression might be constant on two threading paths, and
		 become non-constant (i.e.: phi) when they merge.  */
	      gimple *stmt = gsi_stmt (gsi);
	      if (gimple_call_internal_p (stmt, IFN_UNIQUE)
		  || gimple_call_builtin_p (stmt, BUILT_IN_CONSTANT_P))
		return false;
	      /* Do not count empty statements and labels.  */
	      if (gimple_code (stmt) != GIMPLE_NOP
		  && !(gimple_code (stmt) == GIMPLE_ASSIGN
		       && gimple_assign_rhs_code (stmt) == ASSERT_EXPR)
		  && !is_gimple_debug (stmt))
		n_insns += estimate_num_insns (stmt, &eni_size_weights);
	    }
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " (%i insns)", n_insns-orig_n_insns);

	  /* We do not look at the block with the threaded branch
	     in this loop.  So if any block with a last statement that
	     is a GIMPLE_SWITCH or GIMPLE_GOTO is seen, then we have a
	     multiway branch on our path.

	     The block in PATH[0] is special, it's the block were we're
	     going to be able to eliminate its branch.  */
	  gimple *last = last_stmt (bb);
	  if (last && (gimple_code (last) == GIMPLE_SWITCH
		       || gimple_code (last) == GIMPLE_GOTO))
	    {
	      if (j == 0)
		threaded_multiway_branch = true;
	      else
		multiway_branch_in_path = true;
	    }
	}

      /* Note if we thread through the latch, we will want to include
	 the last entry in the array when determining if we thread
	 through the loop latch.  */
      if (loop->latch == bb)
	threaded_through_latch = true;
    }

  gimple *stmt = get_gimple_control_stmt (m_path[0]);
  gcc_assert (stmt);

  /* We are going to remove the control statement at the end of the
     last block in the threading path.  So don't count it against our
     statement count.  */

  int stmt_insns = estimate_num_insns (stmt, &eni_size_weights);
  n_insns-= stmt_insns;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n  Control statement insns: %i\n"
	     "  Overall: %i insns\n",
	     stmt_insns, n_insns);

  if (creates_irreducible_loop)
    {
      /* If this path threaded through the loop latch back into the
	 same loop and the destination does not dominate the loop
	 latch, then this thread would create an irreducible loop.  */
      *creates_irreducible_loop = false;
      if (taken_edge
	  && threaded_through_latch
	  && loop == taken_edge->dest->loop_father
	  && (determine_bb_domination_status (loop, taken_edge->dest)
	      == DOMST_NONDOMINATING))
	*creates_irreducible_loop = true;
    }

  if (path_crosses_loops)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  FAIL: FSM jump-thread path not considered: "
		 "the path crosses loops.\n");
      return false;
    }

  /* Threading is profitable if the path duplicated is hot but also
     in a case we separate cold path from hot path and permit optimization
     of the hot path later.  Be on the agressive side here. In some testcases,
     as in PR 78407 this leads to noticeable improvements.  */
  if (m_speed_p
      && ((taken_edge && optimize_edge_for_speed_p (taken_edge))
	  || contains_hot_bb))
    {
      if (n_insns >= param_max_fsm_thread_path_insns)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  FAIL: FSM jump-thread path not considered: "
		     "the number of instructions on the path "
		     "exceeds PARAM_MAX_FSM_THREAD_PATH_INSNS.\n");
	  return false;
	}
    }
  else if (!m_speed_p && n_insns > 1)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  FAIL: FSM jump-thread path not considered: "
		 "duplication of %i insns is needed and optimizing for size.\n",
		 n_insns);
      return false;
    }

  /* We avoid creating irreducible inner loops unless we thread through
     a multiway branch, in which case we have deemed it worth losing
     other loop optimizations later.

     We also consider it worth creating an irreducible inner loop if
     the number of copied statement is low relative to the length of
     the path -- in that case there's little the traditional loop
     optimizer would have done anyway, so an irreducible loop is not
     so bad.  */
  if (!threaded_multiway_branch
      && creates_irreducible_loop
      && *creates_irreducible_loop
      && (n_insns * (unsigned) param_fsm_scale_path_stmts
	  > (m_path.length () *
	     (unsigned) param_fsm_scale_path_blocks)))

    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  FAIL: FSM would create irreducible loop without threading "
		 "multiway branch.\n");
      return false;
    }

  /* If this path does not thread through the loop latch, then we are
     using the FSM threader to find old style jump threads.  This
     is good, except the FSM threader does not re-use an existing
     threading path to reduce code duplication.

     So for that case, drastically reduce the number of statements
     we are allowed to copy.  */
  if (!(threaded_through_latch && threaded_multiway_branch)
      && (n_insns * param_fsm_scale_path_stmts
	  >= param_max_jump_thread_duplication_stmts))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  FAIL: FSM did not thread around loop and would copy too "
		 "many statements.\n");
      return false;
    }

  /* When there is a multi-way branch on the path, then threading can
     explode the CFG due to duplicating the edges for that multi-way
     branch.  So like above, only allow a multi-way branch on the path
     if we actually thread a multi-way branch.  */
  if (!threaded_multiway_branch && multiway_branch_in_path)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  FAIL: FSM Thread through multiway branch without threading "
		 "a multiway branch.\n");
      return false;
    }
  return true;
}

/* Return the taken edge out of a path, assuming that the underlying assignment
   or PHI SSA resolves to ARG.  */

edge
thread_jumps::find_taken_edge (const vec<basic_block> &path, tree arg)
{
  if (TREE_CODE_CLASS (TREE_CODE (arg)) != tcc_constant)
    return NULL;

  gcc_checking_assert (!path.is_empty ());
  gimple *stmt = get_gimple_control_stmt (m_path[0]);

  /* We have found a constant value for ARG.  For GIMPLE_SWITCH
     and GIMPLE_GOTO, we use it as-is.  However, for a GIMPLE_COND
     we need to substitute, fold and simplify so we can determine
     the edge taken out of the last block.  */
  if (gimple_code (stmt) == GIMPLE_COND)
    {
      enum tree_code cond_code = gimple_cond_code (stmt);

      /* We know the underyling format of the condition.  */
      arg = fold_binary (cond_code, boolean_type_node,
			 arg, gimple_cond_rhs (stmt));
    }

  /* If this path threaded through the loop latch back into the
     same loop and the destination does not dominate the loop
     latch, then this thread would create an irreducible loop.

     We have to know the outgoing edge to figure this out.  */
  return ::find_taken_edge (m_path[0], arg);
}

/* The current path PATH is a vector of blocks forming a jump threading
   path in reverse order.  TAKEN_EDGE is the edge taken from path[0].

   Convert the current path into the form used by register_jump_thread and
   register it.

   Return TRUE if successful or FALSE otherwise.  */

bool
back_threader_registry::register_path (const vec<basic_block> &m_path,
				       edge taken_edge)
{
  if (m_threaded_paths > m_max_allowable_paths)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  FAIL: FSM jump-thread path not considered: "
		 "the number of previously recorded FSM paths to "
		 "thread exceeds PARAM_MAX_FSM_THREAD_PATHS.\n");
      return false;
    }

  vec<jump_thread_edge *> *jump_thread_path
    = m_lowlevel_registry.allocate_thread_path ();

  /* Record the edges between the blocks in PATH.  */
  for (unsigned int j = 0; j + 1 < m_path.length (); j++)
    {
      basic_block bb1 = m_path[m_path.length () - j - 1];
      basic_block bb2 = m_path[m_path.length () - j - 2];

      edge e = find_edge (bb1, bb2);
      gcc_assert (e);
      jump_thread_edge *x
	= m_lowlevel_registry.allocate_thread_edge (e, EDGE_FSM_THREAD);
      jump_thread_path->safe_push (x);
    }

  /* Add the edge taken when the control variable has value ARG.  */
  jump_thread_edge *x
    = m_lowlevel_registry.allocate_thread_edge (taken_edge,
						EDGE_NO_COPY_SRC_BLOCK);
  jump_thread_path->safe_push (x);

  m_lowlevel_registry.register_jump_thread (jump_thread_path);
  ++m_threaded_paths;
  return true;
}

/* While following a chain of SSA_NAME definitions, we jumped from a
   definition in LAST_BB to a definition in NEW_BB (walking
   backwards).

   Verify there is a single path between the blocks and none of the
   blocks in the path is already in VISITED_BBS.  If so, then update
   VISISTED_BBS, add the new blocks to PATH and return TRUE.
   Otherwise return FALSE.

   Store the length of the subpath in NEXT_PATH_LENGTH.  */

bool
thread_jumps::check_subpath_and_update_thread_path (basic_block last_bb,
						    basic_block new_bb,
						    int *next_path_length)
{
  edge e;
  int e_count = 0;
  edge_iterator ei;
  auto_vec<basic_block> next_path;

  FOR_EACH_EDGE (e, ei, last_bb->preds)
    {
      hash_set<basic_block> local_visited_bbs;

      if (fsm_find_thread_path (new_bb, e->src, next_path,
				local_visited_bbs, e->src->loop_father))
	++e_count;

      /* If there is more than one path, stop.  */
      if (e_count > 1)
	return false;
    }

  /* Stop if we have not found a path: this could occur when the recursion
     is stopped by one of the bounds.  */
  if (e_count == 0)
    return false;

  /* Make sure we haven't already visited any of the nodes in
     NEXT_PATH.  Don't add them here to avoid pollution.  */
  for (unsigned int i = 0; i + 1 < next_path.length (); i++)
    {
      if (m_visited_bbs.contains (next_path[i]))
	return false;
    }

  /* Now add the nodes to VISISTED_BBS.  */
  for (unsigned int i = 0; i + 1 < next_path.length (); i++)
    m_visited_bbs.add (next_path[i]);

  /* Append all the nodes from NEXT_PATH to PATH.  */
  m_path.safe_splice (next_path);
  *next_path_length = next_path.length ();

  return true;
}

/* If this is a profitable jump thread path, register it.

   NAME is an SSA NAME with a possible constant value of ARG on PATH.

   DEF_BB is the basic block that ultimately defines the constant.  */

void
thread_jumps::maybe_register_path (const vec<basic_block> &m_path,
				   tree name,
				   edge taken_edge)
{
  bool irreducible = false;
  bool profitable = m_profit.profitable_path_p (m_path, name, taken_edge,
						&irreducible);
  if (profitable)
    {
      if (!m_registry.register_path (m_path, taken_edge))
	return;

      if (irreducible)
	vect_free_loop_info_assumptions (m_path[0]->loop_father);
    }
}

/* Given PHI which defines NAME in block DEF_BB, recurse through the
   PHI's arguments searching for paths where NAME will ultimately have
   a constant value.

   PATH contains the series of blocks to traverse that will result in
   NAME having a constant value.  */

void
thread_jumps::handle_phi (gphi *phi, basic_block def_bb)
{
  /* Iterate over the arguments of PHI.  */
  for (unsigned int i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg_def (phi, i);
      basic_block bbi = gimple_phi_arg_edge (phi, i)->src;

      /* Skip edges pointing outside the current loop.  */
      if (!arg || def_bb->loop_father != bbi->loop_father)
	continue;

      if (TREE_CODE (arg) == SSA_NAME)
	{
	  m_path.safe_push (bbi);
	  /* Recursively follow SSA_NAMEs looking for a constant
	     definition.  */
	  fsm_find_control_statement_thread_paths (arg);

	  m_path.pop ();
	  continue;
	}

      m_path.safe_push (bbi);
      edge taken_edge = find_taken_edge (m_path, arg);
      if (taken_edge)
	maybe_register_path (m_path, m_name, taken_edge);
      m_path.pop ();
    }
}

/* Return TRUE if STMT is a gimple assignment we want to either directly
   handle or recurse through.  Return FALSE otherwise.

   Note that adding more cases here requires adding cases to handle_assignment
   below.  */

static bool
handle_assignment_p (gimple *stmt)
{
  if (is_gimple_assign (stmt))
    {
      enum tree_code def_code = gimple_assign_rhs_code (stmt);

      /* If the RHS is an SSA_NAME, then we will recurse through it.
	 Go ahead and filter out cases where the SSA_NAME is a default
	 definition.  There's little to be gained by trying to handle that.  */
      if (def_code == SSA_NAME
	  && !SSA_NAME_IS_DEFAULT_DEF (gimple_assign_rhs1 (stmt)))
	return true;

      /* If the RHS is a constant, then it's a terminal that we'll want
	 to handle as well.  */
      if (TREE_CODE_CLASS (def_code) == tcc_constant)
	return true;
    }

  /* Anything not explicitly allowed is not handled.  */
  return false;
}

/* Given STMT which defines NAME in block DEF_BB, recurse through the
   PHI's arguments searching for paths where NAME will ultimately have
   a constant value.

   PATH contains the series of blocks to traverse that will result in
   NAME having a constant value.  */

void
thread_jumps::handle_assignment (gimple *stmt, basic_block def_bb)
{
  tree arg = gimple_assign_rhs1 (stmt);

  if (TREE_CODE (arg) == SSA_NAME)
    fsm_find_control_statement_thread_paths (arg);
  else
    {
      if (CHECKING_P)
	{
	  gcc_assert (!m_path.is_empty ());
	  basic_block top = m_path[m_path.length () - 1];
	  gcc_assert (top == def_bb);
	}
      edge taken_edge = find_taken_edge (m_path, arg);
      if (taken_edge)
	maybe_register_path (m_path, m_name, taken_edge);
    }
}

/* We trace the value of the SSA_NAME NAME back through any phi nodes
   looking for places where it gets a constant value and save the
   path.  */

void
thread_jumps::fsm_find_control_statement_thread_paths (tree name)
{
  /* If NAME appears in an abnormal PHI, then don't try to trace its
     value back through PHI nodes.  */
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name))
    return;

  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
  basic_block def_bb = gimple_bb (def_stmt);

  if (def_bb == NULL)
    return;

  /* We allow the SSA chain to contains PHIs and simple copies and constant
     initializations.  */
  if (gimple_code (def_stmt) != GIMPLE_PHI
      && gimple_code (def_stmt) != GIMPLE_ASSIGN)
    return;

  if (gimple_code (def_stmt) == GIMPLE_PHI
      && (gimple_phi_num_args (def_stmt)
	  >= (unsigned) param_fsm_maximum_phi_arguments))
    return;

  if (is_gimple_assign (def_stmt)
      && ! handle_assignment_p (def_stmt))
    return;

  /* Avoid infinite recursion.  */
  if (m_visited_bbs.add (def_bb))
    return;

  int next_path_length = 0;
  basic_block last_bb_in_path = m_path.last ();

  if (loop_containing_stmt (def_stmt)->header == gimple_bb (def_stmt))
    {
      /* Do not walk through more than one loop PHI node.  */
      if (m_seen_loop_phi)
	return;
      m_seen_loop_phi = true;
    }

  /* Following the chain of SSA_NAME definitions, we jumped from a definition in
     LAST_BB_IN_PATH to a definition in DEF_BB.  When these basic blocks are
     different, append to PATH the blocks from LAST_BB_IN_PATH to DEF_BB.  */
  if (def_bb != last_bb_in_path)
    {
      /* When DEF_BB == LAST_BB_IN_PATH, then the first block in the path
	 will already be in VISITED_BBS.  When they are not equal, then we
	 must ensure that first block is accounted for to ensure we do not
	 create bogus jump threading paths.  */
      m_visited_bbs.add (m_path[0]);
      if (!check_subpath_and_update_thread_path (last_bb_in_path, def_bb,
						 &next_path_length))
	return;
    }

  gcc_assert (m_path.last () == def_bb);

  if (gimple_code (def_stmt) == GIMPLE_PHI)
    handle_phi (as_a <gphi *> (def_stmt), def_bb);
  else if (gimple_code (def_stmt) == GIMPLE_ASSIGN)
    handle_assignment (def_stmt, def_bb);

  /* Remove all the nodes that we added from NEXT_PATH.  */
  if (next_path_length)
    m_path.truncate (m_path.length () - next_path_length);
}

/* Search backwards from BB looking for paths where NAME (an SSA_NAME)
   is a constant.  Record such paths for jump threading.

   It is assumed that BB ends with a control statement and that by
   finding a path where NAME is a constant, we can thread the path.
   SPEED_P indicates that we could increase code size to improve the
   code path.  */

void
thread_jumps::find_jump_threads_backwards (basic_block bb)
{
  gimple *stmt = get_gimple_control_stmt (bb);
  if (!stmt)
    return;

  enum gimple_code code = gimple_code (stmt);
  tree name = NULL;
  if (code == GIMPLE_SWITCH)
    name = gimple_switch_index (as_a <gswitch *> (stmt));
  else if (code == GIMPLE_GOTO)
    name = gimple_goto_dest (stmt);
  else if (code == GIMPLE_COND)
    {
      if (TREE_CODE (gimple_cond_lhs (stmt)) == SSA_NAME
	  && TREE_CODE_CLASS (TREE_CODE (gimple_cond_rhs (stmt))) == tcc_constant
	  && (INTEGRAL_TYPE_P (TREE_TYPE (gimple_cond_lhs (stmt)))
	      || POINTER_TYPE_P (TREE_TYPE (gimple_cond_lhs (stmt)))))
	name = gimple_cond_lhs (stmt);
    }

  if (!name || TREE_CODE (name) != SSA_NAME)
    return;

  /* Initialize pass local data that's different for each BB.  */
  m_path.truncate (0);
  m_path.safe_push (bb);
  m_visited_bbs.empty ();
  m_seen_loop_phi = false;
  m_name = name;

  fsm_find_control_statement_thread_paths (name);
}

namespace {

const pass_data pass_data_thread_jumps =
{
  GIMPLE_PASS,
  "thread",
  OPTGROUP_NONE,
  TV_TREE_SSA_THREAD_JUMPS,
  ( PROP_cfg | PROP_ssa ),
  0,
  0,
  0,
  TODO_update_ssa,
};

class pass_thread_jumps : public gimple_opt_pass
{
public:
  pass_thread_jumps (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_thread_jumps, ctxt)
  {}

  opt_pass * clone (void) { return new pass_thread_jumps (m_ctxt); }
  virtual bool gate (function *);
  virtual unsigned int execute (function *);
};

bool
pass_thread_jumps::gate (function *fun ATTRIBUTE_UNUSED)
{
  return flag_expensive_optimizations;
}


unsigned int
pass_thread_jumps::execute (function *fun)
{
  loop_optimizer_init (LOOPS_HAVE_PREHEADERS | LOOPS_HAVE_SIMPLE_LATCHES);

  /* Try to thread each block with more than one successor.  */
  thread_jumps threader;
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      if (EDGE_COUNT (bb->succs) > 1)
	threader.find_jump_threads_backwards (bb);
    }
  bool changed = threader.thread_through_all_blocks ();

  loop_optimizer_finalize ();
  return changed ? TODO_cleanup_cfg : 0;
}

}

gimple_opt_pass *
make_pass_thread_jumps (gcc::context *ctxt)
{
  return new pass_thread_jumps (ctxt);
}

namespace {

const pass_data pass_data_early_thread_jumps =
{
  GIMPLE_PASS,
  "ethread",
  OPTGROUP_NONE,
  TV_TREE_SSA_THREAD_JUMPS,
  ( PROP_cfg | PROP_ssa ),
  0,
  0,
  0,
  ( TODO_cleanup_cfg | TODO_update_ssa ),
};

class pass_early_thread_jumps : public gimple_opt_pass
{
public:
  pass_early_thread_jumps (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_early_thread_jumps, ctxt)
  {}

  opt_pass * clone (void) { return new pass_early_thread_jumps (m_ctxt); }
  virtual bool gate (function *);
  virtual unsigned int execute (function *);
};

bool
pass_early_thread_jumps::gate (function *fun ATTRIBUTE_UNUSED)
{
  return true;
}


unsigned int
pass_early_thread_jumps::execute (function *fun)
{
  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);

  /* Try to thread each block with more than one successor.  */
  thread_jumps threader (/*speed_p=*/false);
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      if (EDGE_COUNT (bb->succs) > 1)
	threader.find_jump_threads_backwards (bb);
    }
  threader.thread_through_all_blocks ();

  loop_optimizer_finalize ();
  return 0;
}

}

gimple_opt_pass *
make_pass_early_thread_jumps (gcc::context *ctxt)
{
  return new pass_early_thread_jumps (ctxt);
}
