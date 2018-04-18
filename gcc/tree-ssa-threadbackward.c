/* SSA Jump Threading
   Copyright (C) 2005-2018 Free Software Foundation, Inc.

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
#include "params.h"
#include "tree-ssa-loop.h"
#include "cfganal.h"
#include "tree-pass.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "tree-inline.h"
#include "tree-vectorizer.h"
#include "stringpool.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "ssa-range.h"
#include "domwalk.h"
#include "graph.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"

DEBUG_FUNCTION
void graphme()
{
  system("rm -f /tmp/base.dot");
  print_graph_cfg("/tmp/base", cfun);
  system("~/bin/dotview");
}

/* Class to generate all paths from an SSA name to a use of NAME.
   Note: we discard any paths greater than PARAM_MAX_FSM_THREAD_LENGTH.

   Use it like this:

	bb_paths p;
	p.calculate (x_99, some_bb);
	for (unsigned i = 0; i < p.length (); i++)
	  {
	    vec<basic_block> path = p[i];
	    do_funky_things (path);
	  }
 */

class bb_paths
{
  /* All paths from DEF_BB to USE_BB.  */
  vec< vec<basic_block> > all_paths;
  /* The SSA name we are interested in.  */
  tree name;
  /* The BB defining NAME.  */
  basic_block def_bb;
  /* The BB using NAME.  */
  basic_block use_bb;
  /* One ranger for everything so ranges get cached.  */
  path_ranger ranger;

  void calculate_1 (vec<basic_block> &path, basic_block bb,
		    hash_set<basic_block> &visited);
  void prune_duplicate_paths (void);
  void prune_irrelevant_range_blocks (vec <basic_block> &path);
  void dump_one_path (FILE *out, const vec<basic_block> &path);

 public:
  bb_paths ();
  ~bb_paths ();
  tree get_name (void) { return name; }
  void calculate (tree ssa, basic_block use);
  /* Calculate the range for PATH and store it in R.
     Return TRUE if R is not the entire range for type.  */
  bool range_of_path (irange &r, vec<basic_block> &path,
		      edge start_edge = NULL)
  {
    return ranger.path_range (r, name, path, path_ranger::REVERSE, start_edge);
  }
  /* Attempt to fold STMT given VAR and its known range VAR_RANGE.
     Store the resulting range in R and return TRUE if R is non
     empty.  */
  bool range_of_folded_stmt (irange &r, gimple *stmt, tree var,
			     const irange var_range)
  {
    return ranger.range_of_def (r, stmt, var, var_range);
  }
  /* Return the ultimate SSA name for which NAME depends on.  */
  tree terminal_name (void)
  {
    return ranger.single_import (name);
  }
  const vec<basic_block> &operator[] (unsigned i) const { return all_paths[i]; }
  vec<basic_block> &operator[] (unsigned i) { return all_paths[i]; }
  unsigned length () const { return all_paths.length (); }
  void dump ();
};

bb_paths::bb_paths ()
{
  all_paths = vNULL;
}

bb_paths::~bb_paths ()
{
  for (unsigned i = 0; i < all_paths.length (); ++i)
    all_paths[i].release ();
  all_paths.release ();
}

/* Generate all paths from the definition of SSA to its USE.
   Accumulate the paths in ALL_PATHS.  */

void
bb_paths::calculate (tree ssa, basic_block use)
{
  all_paths = vNULL;
  name = ssa;
  use_bb = use;
  if (SSA_NAME_IS_DEFAULT_DEF (name))
    def_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  else
    def_bb = gimple_bb (SSA_NAME_DEF_STMT (name));

  /* A path where the use and the definition are in the same block, is
     a path of one block, and can be handled specially.  */
  if (use_bb == def_bb)
    {
      vec<basic_block> v;
      v.create (1);
      v.quick_push (use_bb);
      all_paths.safe_push (v);
      return;
    }

  hash_set<basic_block> visited;
  auto_vec<basic_block> path;
  calculate_1 (path, use_bb, visited);
  prune_duplicate_paths ();
}

/* Helper function for calculate().  Calculate all paths from DEF_BB
   to BB and store them into ALL_PATHS.

   PATH is the current path being accumulated.  VISITED is a hash of
   visited blocks.  */

void
bb_paths::calculate_1 (vec<basic_block> &path, basic_block bb,
		       hash_set<basic_block> &visited)
{
  if (!def_bb)
    return;
  /* Discard loops.  */
  if (visited.add (bb))
    return;
  if ((int)path.length () + 1 > PARAM_VALUE (PARAM_MAX_FSM_THREAD_LENGTH))
    return;

  /* As an optimization, we disregard paths that cross loops.  Since
     profitable_jump_thread_path() will ignore them, we can avoid
     putting them in the queue altogether.  */
  if (!path.is_empty () && path[0]->loop_father != bb->loop_father)
    return;

  path.safe_push (bb);

  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      /* If we reached the defining block, we're at the top, and have a
	 complete path.  */
      if (e->src == def_bb)
	{
	  /* If we've already seen DEF_BB, we have a complete loop
	     back to DEF_BB.  We discard loops, so...  */
	  if (visited.contains (def_bb))
	    return;

	  /* As mentioned in profitable_jump_thread_path(), the last
	     entry in a path (DEF_BB in our case) represents the block
	     with an outgoing edge that will redirect to the jump
	     threading path.  Thus, we don't care if DEF_BB lives in a
	     loop different than the rest of the path we are
	     accumulating.  This is why we don't perform the
	     loop_father optimization at the beginning of this
	     function.  */

	  /* Push the DEF_BB for completeness sake.  */
	  path.safe_push (def_bb);
	  vec<basic_block> t = path.copy ();
	  all_paths.safe_push (t);
	  path.pop ();
	}
      else
	calculate_1 (path, e->src, visited);
    }
  path.pop ();
  visited.remove (bb);
}

/* If we have a path that generates range information for SSA, any
   blocks until the first block that contains range information is
   irrelevant for the range.  Prune such blocks.  */

void
bb_paths::prune_irrelevant_range_blocks (vec <basic_block> &path)
{
  /* If the path is just one block, we have a path where the
     definition and the use are in the same BB.  In this case, there
     is nothing to do.  */
  if (path.length () == 1)
    return;

  for (int i = path.length () - 1; i > 0; --i)
    {
      irange r;
      edge e;

      gcc_assert (e = find_edge (path[i], path[i - 1]));
      /* ?? When the path ranger can understand copies, we should look
	 into using it instead of gori.  For instance, we may not have
	 a range here, but the path ranger may realize that earlier
	 than E we have a range because of a copy??.  */
      if (ranger.range_on_edge (r, name, e))
	{
	  /* Remove anything that came before here.  */
	  path.truncate (i + 1);
	  return;
	}
    }
  path.truncate (0);
}

/* Clean-up paths by removing any irrelevant blocks in the paths
   themselves and then removing any duplicate paths.  */

void
bb_paths::prune_duplicate_paths (void)
{
  if (all_paths.is_empty ())
    return;

  vec<basic_block> prev_path = vNULL;
  vec< vec<basic_block> > new_all_paths;
  /* If we prune any duplicates, new_all_paths will have some extra
     memory allocated.  We dont' care, as this won't live long.  */
  new_all_paths.create (all_paths.length ());

  /* Get rid of useless blocks per path, and then accumulate all
     non-duplicate paths into new_all_paths.  Removing duplicates is
     easy because all_paths is already sorted.  */
  for (unsigned i = 0; i < all_paths.length (); ++i)
    {
      vec<basic_block> path = all_paths[i];

      prune_irrelevant_range_blocks (path);
      if (path.is_empty ())
	{
	  path.release ();
	  continue;
	}

      /* Is this a duplicate of the last known path?  */
      bool duplicate = false;
      if (prev_path.length () == path.length ())
	{
	  duplicate = true;
	  for (unsigned i = 0; i < path.length (); ++i)
	    if (path[i] != prev_path[i])
	      {
		duplicate = false;
		break;
	      }
	}
      if (duplicate)
	path.release ();
      else
	{
	  prev_path = path;
	  new_all_paths.safe_push (path);
	}
    }
  all_paths.release ();
  all_paths = new_all_paths;
}

/* Helper function for bb_paths::dump to dump one PATH to OUT.  */

void
bb_paths::dump_one_path (FILE *out, const vec<basic_block> &path)
{
  fprintf (out, "\tpath: ");
  if (path.is_empty ())
    {
      fprintf (out, "<empty>\n");
      return;
    }
  for (int i = path.length () - 1; i > 0; --i)
    {
      fprintf (out, "bb%d", path[i]->index);
      edge e = find_edge (path[i], path[i - 1]);
      gcc_assert (e);
      fprintf (out, " => ");
    }
  fprintf (out, "bb%d\n", path[0]->index);
}

/* Dump all available paths.  */

void
bb_paths::dump ()
{
  if (all_paths.is_empty ())
    return;

  fprintf (stderr, "range path to BB%d for SSA = ", use_bb->index);
  print_generic_stmt (stderr, name, 0);

  for (unsigned i = 0; i < length (); ++i)
    {
      irange r;
      vec<basic_block> path = all_paths[i];

      dump_one_path (stderr, path);
      if (range_of_path (r, path))
	{
	  fprintf (stderr, "\t  ");
	  r.dump ();
	}
      else
	fprintf (stderr, "\n");
    }
  fprintf (stderr, "-----------------------------\n");
}

class thread_jumps
{
 public:
  void find_jump_threads_backwards (basic_block bb, bool speed_p);
 private:
  edge profitable_jump_thread_path (basic_block bbi, tree name,
				    const irange &range_for_name,
				    bool *creates_irreducible_loop);
  void convert_and_register_current_path (edge taken_edge);
  void register_jump_thread_path_if_profitable (tree name, const irange &irange,
						basic_block def_bb);
  void handle_assignment (gimple *stmt, tree name, basic_block def_bb);
  void handle_phi (gphi *phi, tree name, basic_block def_bb);
  void fsm_find_control_statement_thread_paths (tree name);
  bool check_subpath_and_update_thread_path (basic_block last_bb,
					     basic_block new_bb,
					     int *next_path_length);
  void find_range_based_jump_threads (tree name, basic_block bb);
  bool maybe_register_range_based_phis (void);
  void register_current_range_based_path (irange &r);
  bool resolve_control_statement (gimple *stmt, tree name,
				  const irange &range_for_name, tree &result);

  /* Maximum number of BBs we are allowed to thread.  */
  int m_max_threaded_paths;
  /* Hash to keep track of seen bbs.  */
  hash_set<basic_block> m_visited_bbs;
  /* Current path we're analyzing.  */
  auto_vec<basic_block> m_path;
  /* Tracks if we have recursed through a loop PHI node.  */
  bool m_seen_loop_phi;
  /* Indicate that we could increase code size to improve the
     code path.  */
  bool m_speed_p;
  /* All paths from a definition of an SSA name to a use of it.  */
  bb_paths m_all_paths_to_ssa;
};

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

/* Try to evaluate the control statement and see if we can fold to a
   constant.  If we do, set RESULT to it and return TRUE.
   Otherwise, return FALSE.

   For GIMPLE_COND we evaluate the control statement and set the
   substituted true/false value in RESULT.

   For GIMPLE_SWITCH we set RESULT to NULL if evaluating the switch
   will yield the default case.  */
bool
thread_jumps::resolve_control_statement (gimple *stmt, tree name,
					 const irange &range_for_name,
					 tree &result)
{
  wide_int singleton;
  switch (gimple_code (stmt))
    {
    case GIMPLE_COND:
      {
	/* It looks like NAME is not necessarily the same SSA name as
	   the LHS conditional here.  Look at the LHS to make sure
	   the ranger gets the right range.  */
	tree var = gimple_cond_lhs (stmt);
	irange r;
	if (!m_all_paths_to_ssa.range_of_folded_stmt (r, stmt, var,
						      range_for_name)
	    || !r.singleton_p (singleton))
	  return false;
	result = wide_int_to_tree (TREE_TYPE (name), singleton);
	return true;
      }
    case GIMPLE_SWITCH:
      {
	/* Handle the simple case fast.  */
	if (range_for_name.singleton_p (singleton))
	  {
	    result = wide_int_to_tree (TREE_TYPE (name), singleton);
	    return true;
	  }
	gswitch *gs = as_a <gswitch *> (stmt);
	for (unsigned i = 1; i < gimple_switch_num_labels (gs); ++i)
	  {
	    tree label = gimple_switch_label (gs, i);
	    tree case_low = CASE_LOW (label);
	    tree case_high = CASE_HIGH (label);
	    if (!case_high)
	      case_high = case_low;
	    irange label_range (TREE_TYPE (name), case_low, case_high);
	    /* If NAME can fall into one of the switch cases, we can't
	       be sure where the switch will land.  */
	    if (!irange_intersect (range_for_name, label_range).empty_p ())
	      return false;
	    /* If we have an exact match, for example, a case of 3..10
	       with a known range of [3,10], then we know we will
	       always select this case.  Set RESULT to any number
	       within the range so find_taken_edge() can find the
	       right case.

	       ?? Is this even worth the effort?  */
	    if (range_for_name == label_range)
	      {
		wide_int any_number_in_range = range_for_name.lower_bound ();
		result = wide_int_to_tree (TREE_TYPE (name),
					   any_number_in_range);
		return true;
	      }
	  }
	/* If we couldn't find anything, the only alternative is that
	   we will always select the default case.  Returning NULL
	   indicates that the switch will yield the default case.  */
        result = NULL;
	return true;
      }
    case GIMPLE_GOTO:
      if (!range_for_name.singleton_p (singleton))
	return false;
      result = wide_int_to_tree (TREE_TYPE (name), singleton);
      return true;
    default:
      gcc_unreachable ();
      return false;
    }
}

/* Examine jump threading path PATH to which we want to add BBI.

   If the resulting path is profitable to thread, then return the
   final taken edge from the path, NULL otherwise.

   NAME is the SSA_NAME of the variable we found to have a constant
   value on PATH.  RANGE_FOR_NAME is the range of NAME on that path.

   BBI will be appended to PATH when we have a profitable jump
   threading path.  Callers are responsible for removing BBI from PATH
   in that case.  */

edge
thread_jumps::profitable_jump_thread_path (basic_block bbi, tree name,
					   const irange &range_for_name,
					   bool *creates_irreducible_loop)
{
  /* Note BBI is not in the path yet, hence the +1 in the test below
     to make sure BBI is accounted for in the path length test.  */

  /* We can get a length of 0 here when the statement that
     makes a conditional generate a compile-time constant
     result is in the same block as the conditional.

     That's not really a jump threading opportunity, but instead is
     simple cprop & simplification.  We could handle it here if we
     wanted by wiring up all the incoming edges.  If we run this
     early in IPA, that might be worth doing.   For now we just
     reject that case.  */
  if (m_path.is_empty ())
      return NULL;

  if (m_path.length () + 1
      > (unsigned) PARAM_VALUE (PARAM_MAX_FSM_THREAD_LENGTH))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "FSM jump-thread path not considered: "
		 "the number of basic blocks on the path "
		 "exceeds PARAM_MAX_FSM_THREAD_LENGTH.\n");
      return NULL;
    }

  if (m_max_threaded_paths <= 0)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "FSM jump-thread path not considered: "
		 "the number of previously recorded FSM paths to "
		 "thread exceeds PARAM_MAX_FSM_THREAD_PATHS.\n");
      return NULL;
    }

  if (range_for_name.empty_p ())
    return NULL;

  gimple *stmt = get_gimple_control_stmt (m_path[0]);
  tree arg;
  edge taken_edge = NULL;
  if (!resolve_control_statement (stmt, name, range_for_name, arg))
    return NULL;
  /* Special case in resolve_control_statement.  NULL means take the
     default case.  */
  if (gimple_code (stmt) == GIMPLE_SWITCH && arg == NULL)
    {
      tree taken_case = gimple_switch_default_label (as_a <gswitch *> (stmt));
      basic_block dest_bb = label_to_block (CASE_LABEL (taken_case));
      taken_edge = find_edge (m_path[0], dest_bb);
    }

  /* Add BBI to the path.
     From this point onward, if we decide we the path is not profitable
     to thread, we must remove BBI from the path.  */
  m_path.safe_push (bbi);

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
	      gimple *stmt = gsi_stmt (gsi);
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

  /* We are going to remove the control statement at the end of the
     last block in the threading path.  So don't count it against our
     statement count.  */

  int stmt_insns = estimate_num_insns (stmt, &eni_size_weights);
  n_insns-= stmt_insns;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n  Control statement insns: %i\n"
	     "  Overall: %i insns\n",
	     stmt_insns, n_insns);

  /* If this path threaded through the loop latch back into the
     same loop and the destination does not dominate the loop
     latch, then this thread would create an irreducible loop.

     We have to know the outgoing edge to figure this out.  */
  if (!taken_edge)
    taken_edge = find_taken_edge (m_path[0], arg);

  /* There are cases where we may not be able to extract the
     taken edge.  For example, a computed goto to an absolute
     address.  Handle those cases gracefully.  */
  if (taken_edge == NULL)
    {
      m_path.pop ();
      return NULL;
    }

  *creates_irreducible_loop = false;
  if (threaded_through_latch
      && loop == taken_edge->dest->loop_father
      && (determine_bb_domination_status (loop, taken_edge->dest)
	  == DOMST_NONDOMINATING))
    *creates_irreducible_loop = true;

  if (path_crosses_loops)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "FSM jump-thread path not considered: "
		 "the path crosses loops.\n");
      m_path.pop ();
      return NULL;
    }

  /* Threading is profitable if the path duplicated is hot but also
     in a case we separate cold path from hot path and permit optimization
     of the hot path later.  Be on the agressive side here. In some testcases,
     as in PR 78407 this leads to noticeable improvements.  */
  if (m_speed_p && (optimize_edge_for_speed_p (taken_edge) || contains_hot_bb))
    {
      if (n_insns >= PARAM_VALUE (PARAM_MAX_FSM_THREAD_PATH_INSNS))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "FSM jump-thread path not considered: "
		     "the number of instructions on the path "
		     "exceeds PARAM_MAX_FSM_THREAD_PATH_INSNS.\n");
	  m_path.pop ();
	  return NULL;
	}
    }
  else if (n_insns > 1)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "FSM jump-thread path not considered: "
		 "duplication of %i insns is needed and optimizing for size.\n",
		 n_insns);
      m_path.pop ();
      return NULL;
    }

  /* We avoid creating irreducible inner loops unless we thread through
     a multiway branch, in which case we have deemed it worth losing
     other loop optimizations later.

     We also consider it worth creating an irreducible inner loop if
     the number of copied statement is low relative to the length of
     the path -- in that case there's little the traditional loop
     optimizer would have done anyway, so an irreducible loop is not
     so bad.  */
  if (!threaded_multiway_branch && *creates_irreducible_loop
      && (n_insns * (unsigned) PARAM_VALUE (PARAM_FSM_SCALE_PATH_STMTS)
	  > (m_path.length () *
	     (unsigned) PARAM_VALUE (PARAM_FSM_SCALE_PATH_BLOCKS))))

    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "FSM would create irreducible loop without threading "
		 "multiway branch.\n");
      m_path.pop ();
      return NULL;
    }


  /* If this path does not thread through the loop latch, then we are
     using the FSM threader to find old style jump threads.  This
     is good, except the FSM threader does not re-use an existing
     threading path to reduce code duplication.

     So for that case, drastically reduce the number of statements
     we are allowed to copy.  */
  if (!(threaded_through_latch && threaded_multiway_branch)
      && (n_insns * PARAM_VALUE (PARAM_FSM_SCALE_PATH_STMTS)
	  >= PARAM_VALUE (PARAM_MAX_JUMP_THREAD_DUPLICATION_STMTS)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "FSM did not thread around loop and would copy too "
		 "many statements (%d) [max=%d].\n",
		 n_insns * PARAM_VALUE (PARAM_FSM_SCALE_PATH_STMTS),
		 PARAM_VALUE (PARAM_MAX_JUMP_THREAD_DUPLICATION_STMTS));
      m_path.pop ();
      return NULL;
    }

  /* When there is a multi-way branch on the path, then threading can
     explode the CFG due to duplicating the edges for that multi-way
     branch.  So like above, only allow a multi-way branch on the path
     if we actually thread a multi-way branch.  */
  if (!threaded_multiway_branch && multiway_branch_in_path)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "FSM Thread through multiway branch without threading "
		 "a multiway branch.\n");
      m_path.pop ();
      return NULL;
    }
  return taken_edge;
}

/* The current path PATH is a vector of blocks forming a jump threading
   path in reverse order.  TAKEN_EDGE is the edge taken from path[0].

   Convert the current path into the form used by register_jump_thread and
   register it.   */

void
thread_jumps::convert_and_register_current_path (edge taken_edge)
{
  vec<jump_thread_edge *> *jump_thread_path = new vec<jump_thread_edge *> ();

  /* Record the edges between the blocks in PATH.  */
  for (unsigned int j = 0; j + 1 < m_path.length (); j++)
    {
      basic_block bb1 = m_path[m_path.length () - j - 1];
      basic_block bb2 = m_path[m_path.length () - j - 2];

      edge e = find_edge (bb1, bb2);
      gcc_assert (e);
      jump_thread_edge *x = new jump_thread_edge (e, EDGE_FSM_THREAD);
      jump_thread_path->safe_push (x);
    }

  /* Add the edge taken when the control variable has value ARG.  */
  jump_thread_edge *x
    = new jump_thread_edge (taken_edge, EDGE_NO_COPY_SRC_BLOCK);
  jump_thread_path->safe_push (x);

  register_jump_thread (jump_thread_path);
  --m_max_threaded_paths;
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
thread_jumps::register_jump_thread_path_if_profitable (tree name,
						       const irange &range,
						       basic_block def_bb)
{
  if (range.empty_p ())
    return;

  bool irreducible = false;
  edge taken_edge = profitable_jump_thread_path (def_bb, name, range,
						 &irreducible);
  if (taken_edge)
    {
      convert_and_register_current_path (taken_edge);
      m_path.pop ();

      if (irreducible)
	vect_free_loop_info_assumptions (m_path[0]->loop_father);
    }
}

/* Given PHI which defines NAME in block DEF_BB, recurse through the
   PHI's arguments searching for paths where NAME will ultimately have
   a constant value.

   M_PATH contains the series of blocks to traverse that will result in
   NAME having a constant value.  */

void
thread_jumps::handle_phi (gphi *phi, tree name, basic_block def_bb)
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

      if (TREE_CODE_CLASS (TREE_CODE (arg)) == tcc_constant)
	{
	  irange range (TREE_TYPE (name), arg, arg);
	  register_jump_thread_path_if_profitable (name, range, bbi);
	}
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
thread_jumps::handle_assignment (gimple *stmt, tree name, basic_block def_bb)
{
  tree arg = gimple_assign_rhs1 (stmt);

  if (TREE_CODE (arg) == SSA_NAME)
    fsm_find_control_statement_thread_paths (arg);

  else
    {
      /* register_jump_thread_path_if_profitable will push the current
	 block onto the path.  But the path will always have the current
	 block at this point.  So we can just pop it.  */
      m_path.pop ();

      if (TREE_CODE_CLASS (TREE_CODE (arg)) == tcc_constant)
	{
	  irange range (TREE_TYPE (name), arg, arg);
	  register_jump_thread_path_if_profitable (name, range, def_bb);
	}

      /* And put the current block back onto the path so that the
	 state of the stack is unchanged when we leave.  */
      m_path.safe_push (def_bb);
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
	  >= (unsigned) PARAM_VALUE (PARAM_FSM_MAXIMUM_PHI_ARGUMENTS)))
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
    handle_phi (as_a <gphi *> (def_stmt), name, def_bb);
  else if (gimple_code (def_stmt) == GIMPLE_ASSIGN)
    handle_assignment (def_stmt, name, def_bb);

  /* Remove all the nodes that we added from NEXT_PATH.  */
  if (next_path_length)
    m_path.truncate (m_path.length () - next_path_length);
}

/* Register a range based thread in M_PATH.

   R is the known range for said path.  */

void
thread_jumps::register_current_range_based_path (irange &r)
{
  tree name = m_all_paths_to_ssa.get_name ();
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file,
	       "Considering range-based path for jump threading: "
	       "SSA = ");
      print_generic_stmt (dump_file, name, 0);
      fprintf (dump_file, "\tRange is: ");
      r.dump (dump_file);
    }
  /* register_jump_thread_path_if_profitable will push
     the current block onto the path.  But the path
     will always have the current block at this point.
     So we can just pop it.  */
  basic_block def_bb = m_path.pop ();
  register_jump_thread_path_if_profitable (m_all_paths_to_ssa.get_name (),
					   r, def_bb);
}

/* If M_PATH is not threadable, but taking into account the PHI that
   feeds into it makes it threadable, thread it and return TRUE.

   This is a special case we get for cheap without looking back deep
   through PHIs.  For example:

	<bb 5>
	# v1_595 = PHI <var(3), 0(4)>
	_14 = v1_595 * 3600;
	_15 = (unsigned int) _14;
	_16 = _15 / 60;
	...
	<bb 99>
	if (_16 > 123)

   Even though paths through bb5->bb99 are not threadable just taking
   _16 into account, if we take into account the path coming in
   through bb4, we can thread.  This special cases PHIs that feed into
   and are defined in the same BB as the range variable.  */

bool
thread_jumps::maybe_register_range_based_phis ()
{
  tree terminal = m_all_paths_to_ssa.terminal_name ();
  if (!terminal)
    return false;

  /* See if NAME depends on a PHI that is defined in the same BB as
     NAME.  */
  gimple *terminal_def = SSA_NAME_DEF_STMT (terminal);
  basic_block terminal_bb = gimple_bb (terminal_def);
  basic_block def_bb = m_path[m_path.length () - 1];
  if (gimple_code (terminal_def) != GIMPLE_PHI
      || def_bb != terminal_bb)
    return false;

  /* If this PHI has any constant arguments, thread this range along
     with the PHI edge that has the constant.  */
  bool ret = false;
  gphi *phi = as_a <gphi *> (terminal_def);
  for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
    {
      tree arg = gimple_phi_arg_def (phi, i);
      edge e = gimple_phi_arg_edge (phi, i);
      irange r;
      /* If we're threading across loop boundaries, children will die.  */
      if (e->src->loop_father != e->dest->loop_father)
	continue;
      if (TREE_CODE_CLASS (TREE_CODE (arg)) == tcc_constant
	  && m_all_paths_to_ssa.range_of_path (r, m_path, e))
	{
	  m_path.safe_push (e->src);
	  register_current_range_based_path (r);
	  return true;
	}
    }
  return ret;
}

/* Record jump threads based on range information.

   It is assumed that BB ends with a control statement and that by
   finding a path where NAME is a constant, we can thread the path.  */

void
thread_jumps::find_range_based_jump_threads (tree name, basic_block bb)
{
  if (!INTEGRAL_TYPE_P (TREE_TYPE (name)))
    return;

  m_all_paths_to_ssa.calculate (name, bb);

  for (unsigned i = 0; i < m_all_paths_to_ssa.length (); ++i)
    {
      vec<basic_block> p = m_all_paths_to_ssa[i];
      m_path.create (p.length ());
      for (unsigned j = 0; j < p.length (); j++)
	m_path.safe_push (p[j]);

      /* Prefer paths containing a PHI with constant arguments, as
	 these paths, starting off with a constant, have a higher
	 chance of degrading into a singleton range.  */
      if (maybe_register_range_based_phis ())
	continue;
      irange r;
      if (m_all_paths_to_ssa.range_of_path (r, m_path))
	register_current_range_based_path (r);
    }
}

/* Search backwards from BB looking for paths where NAME (an SSA_NAME)
   is a constant.  Record such paths for jump threading.

   It is assumed that BB ends with a control statement and that by
   finding a path where NAME is a constant, we can thread the path.
   SPEED_P indicates that we could increase code size to improve the
   code path.  */

void
thread_jumps::find_jump_threads_backwards (basic_block bb, bool speed_p)
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
  m_speed_p = speed_p;
  m_max_threaded_paths = PARAM_VALUE (PARAM_MAX_FSM_THREAD_PATHS);

  fsm_find_control_statement_thread_paths (name);

  /* If we didn't thread any paths, try threading by making use of
     available range information.  */
  if (m_max_threaded_paths == PARAM_VALUE (PARAM_MAX_FSM_THREAD_PATHS))
    find_range_based_jump_threads (name, bb);
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
	threader.find_jump_threads_backwards (bb, true);
    }
  bool changed = thread_through_all_blocks (true);

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
  thread_jumps threader;
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      if (EDGE_COUNT (bb->succs) > 1)
	threader.find_jump_threads_backwards (bb, false);
    }
  thread_through_all_blocks (true);

  loop_optimizer_finalize ();
  return 0;
}

}

gimple_opt_pass *
make_pass_early_thread_jumps (gcc::context *ctxt)
{
  return new pass_early_thread_jumps (ctxt);
}
