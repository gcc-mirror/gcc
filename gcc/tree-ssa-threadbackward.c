/* SSA Jump Threading
   Copyright (C) 2005-2022 Free Software Foundation, Inc.

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
#include "value-range.h"
#include "gimple-range.h"
#include "tree-ssa-threadedge.h"
#include "gimple-range-path.h"
#include "ssa.h"
#include "tree-cfgcleanup.h"
#include "tree-pretty-print.h"
#include "cfghooks.h"
#include "dbgcnt.h"

// Path registry for the backwards threader.  After all paths have been
// registered with register_path(), thread_through_all_blocks() is called
// to modify the CFG.

class back_threader_registry : public back_jt_path_registry
{
public:
  bool register_path (const vec<basic_block> &, edge taken);
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

// Back threader flags.
#define BT_NONE 0
// Generate fast code at the expense of code size.
#define BT_SPEED 1
// Resolve unknown SSAs on entry to a threading path.  If set, use the
// ranger.  If not, assume all ranges on entry to a path are VARYING.
#define BT_RESOLVE 2

class back_threader
{
public:
  back_threader (function *fun, unsigned flags, bool first);
  ~back_threader ();
  unsigned thread_blocks ();
private:
  void maybe_thread_block (basic_block bb);
  void find_paths (basic_block bb, tree name);
  bool debug_counter ();
  edge maybe_register_path ();
  void maybe_register_path_dump (edge taken_edge);
  void find_paths_to_names (basic_block bb, bitmap imports);
  void resolve_phi (gphi *phi, bitmap imports);
  edge find_taken_edge (const vec<basic_block> &path);
  edge find_taken_edge_cond (const vec<basic_block> &path, gcond *);
  edge find_taken_edge_switch (const vec<basic_block> &path, gswitch *);
  virtual void debug ();
  virtual void dump (FILE *out);

  back_threader_registry m_registry;
  back_threader_profitability m_profit;
  path_range_query *m_solver;

  // Current path being analyzed.
  auto_vec<basic_block> m_path;
  // Hash to mark visited BBs while analyzing a path.
  hash_set<basic_block> m_visited_bbs;
  // The set of SSA names, any of which could potentially change the
  // value of the final conditional in a path.
  auto_bitmap m_imports;
  // The last statement in the path.
  gimple *m_last_stmt;
  // This is a bit of a wart.  It's used to pass the LHS SSA name to
  // the profitability engine.
  tree m_name;
  // Marker to differentiate unreachable edges.
  static const edge UNREACHABLE_EDGE;
  // Set to TRUE if unknown SSA names along a path should be resolved
  // with the ranger.  Otherwise, unknown SSA names are assumed to be
  // VARYING.  Setting to true is more precise but slower.
  function *m_fun;
  unsigned m_flags;
  // Set to TRUE for the first of each thread[12] pass or the first of
  // each threadfull[12] pass.  This is used to differentiate between
  // the different threading passes so we can set up debug counters.
  bool m_first;
};

// Used to differentiate unreachable edges, so we may stop the search
// in a the given direction.
const edge back_threader::UNREACHABLE_EDGE = (edge) -1;

back_threader::back_threader (function *fun, unsigned flags, bool first)
  : m_profit (flags & BT_SPEED),
    m_first (first)
{
  if (flags & BT_SPEED)
    loop_optimizer_init (LOOPS_HAVE_PREHEADERS | LOOPS_HAVE_SIMPLE_LATCHES);
  else
    loop_optimizer_init (AVOID_CFG_MODIFICATIONS);

  m_fun = fun;
  m_flags = flags;
  m_solver = new path_range_query (flags & BT_RESOLVE);
  m_last_stmt = NULL;
}

back_threader::~back_threader ()
{
  delete m_solver;

  loop_optimizer_finalize ();
}

// A wrapper for the various debug counters for the threading passes.
// Returns TRUE if it's OK to register the current threading
// candidate.

bool
back_threader::debug_counter ()
{
  // The ethread pass is mostly harmless ;-).
  if ((m_flags & BT_SPEED) == 0)
    return true;

  if (m_flags & BT_RESOLVE)
    {
      if (m_first && !dbg_cnt (back_threadfull1))
	return false;

      if (!m_first && !dbg_cnt (back_threadfull2))
	return false;
    }
  else
    {
      if (m_first && !dbg_cnt (back_thread1))
	return false;

      if (!m_first && !dbg_cnt (back_thread2))
	return false;
    }
  return true;
}

static void
dump_path (FILE *dump_file, const vec<basic_block> &path)
{
  for (unsigned i = path.length (); i > 0; --i)
    {
      basic_block bb = path[i - 1];
      fprintf (dump_file, "%d", bb->index);
      if (i > 1)
	fprintf (dump_file, "->");
    }
}

// Dump details of an attempt to register a path.

void
back_threader::maybe_register_path_dump (edge taken)
{
  if (m_path.is_empty ())
    return;

  fprintf (dump_file, "path: ");
  dump_path (dump_file, m_path);
  fprintf (dump_file, "->");

  if (taken == UNREACHABLE_EDGE)
    fprintf (dump_file, "xx REJECTED (unreachable)\n");
  else if (taken)
    fprintf (dump_file, "%d SUCCESS\n", taken->dest->index);
  else
    fprintf (dump_file, "xx REJECTED\n");
}

// If an outgoing edge can be determined out of the current path,
// register it for jump threading and return the taken edge.
//
// Return NULL if it is unprofitable to thread this path, or the
// outgoing edge is unknown.  Return UNREACHABLE_EDGE if the path is
// unreachable.

edge
back_threader::maybe_register_path ()
{
  edge taken_edge = find_taken_edge (m_path);

  if (taken_edge && taken_edge != UNREACHABLE_EDGE)
    {
      if (m_visited_bbs.contains (taken_edge->dest))
	{
	  // Avoid circular paths by indicating there is nothing to
	  // see in this direction.
	  taken_edge = UNREACHABLE_EDGE;
	}
      else
	{
	  bool irreducible = false;
	  if (m_profit.profitable_path_p (m_path, m_name, taken_edge,
					  &irreducible)
	      && debug_counter ())
	    {
	      m_registry.register_path (m_path, taken_edge);

	      if (irreducible)
		vect_free_loop_info_assumptions (m_path[0]->loop_father);
	    }
	  else
	    taken_edge = NULL;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    maybe_register_path_dump (taken_edge);

  return taken_edge;
}

// Return the known taken edge out of a path.  If the path can be
// determined to be unreachable, return UNREACHABLE_EDGE.  If no
// outgoing edge can be calculated, return NULL.

edge
back_threader::find_taken_edge (const vec<basic_block> &path)
{
  gcc_checking_assert (path.length () > 1);
  switch (gimple_code (m_last_stmt))
    {
    case GIMPLE_COND:
      return find_taken_edge_cond (path, as_a<gcond *> (m_last_stmt));

    case GIMPLE_SWITCH:
      return find_taken_edge_switch (path, as_a<gswitch *> (m_last_stmt));

    default:
      return NULL;
    }
}

// Same as find_taken_edge, but for paths ending in a switch.

edge
back_threader::find_taken_edge_switch (const vec<basic_block> &path,
				       gswitch *sw)
{
  tree name = gimple_switch_index (sw);
  int_range_max r;

  m_solver->compute_ranges (path, m_imports);
  m_solver->range_of_expr (r, name, sw);

  if (r.undefined_p ())
    return UNREACHABLE_EDGE;

  if (r.varying_p ())
    return NULL;

  tree label = find_case_label_range (sw, &r);
  if (!label)
    return NULL;

  return find_edge (gimple_bb (sw), label_to_block (cfun, CASE_LABEL (label)));
}

// Same as find_taken_edge, but for paths ending in a GIMPLE_COND.

edge
back_threader::find_taken_edge_cond (const vec<basic_block> &path,
				     gcond *cond)
{
  int_range_max r;

  m_solver->compute_ranges (path, m_imports);
  m_solver->range_of_stmt (r, cond);

  if (m_solver->unreachable_path_p ())
    return UNREACHABLE_EDGE;

  int_range<2> true_range (boolean_true_node, boolean_true_node);
  int_range<2> false_range (boolean_false_node, boolean_false_node);

  if (r == true_range || r == false_range)
    {
      edge e_true, e_false;
      basic_block bb = gimple_bb (cond);
      extract_true_false_edges_from_block (bb, &e_true, &e_false);
      return r == true_range ? e_true : e_false;
    }
  return NULL;
}

// Populate a vector of trees from a bitmap.

static inline void
populate_worklist (vec<tree> &worklist, bitmap bits)
{
  bitmap_iterator bi;
  unsigned i;

  EXECUTE_IF_SET_IN_BITMAP (bits, 0, i, bi)
    {
      tree name = ssa_name (i);
      worklist.quick_push (name);
    }
}

// Find jump threading paths that go through a PHI.

void
back_threader::resolve_phi (gphi *phi, bitmap interesting)
{
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_phi_result (phi)))
    return;

  for (size_t i = 0; i < gimple_phi_num_args (phi); ++i)
    {
      edge e = gimple_phi_arg_edge (phi, i);

      // This is like path_crosses_loops in profitable_path_p but more
      // restrictive to avoid peeling off loop iterations (see
      // tree-ssa/pr14341.c for an example).
      bool profitable_p = m_path[0]->loop_father == e->src->loop_father;
      if (!profitable_p)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file,
		       "  FAIL: path through PHI in bb%d (incoming bb:%d) crosses loop\n",
		       e->dest->index, e->src->index);
	      fprintf (dump_file, "path: %d->", e->src->index);
	      dump_path (dump_file, m_path);
	      fprintf (dump_file, "->xx REJECTED\n");
	    }
	  continue;
	}

      tree arg = gimple_phi_arg_def (phi, i);
      unsigned v = 0;

      if (TREE_CODE (arg) == SSA_NAME
	  && !bitmap_bit_p (interesting, SSA_NAME_VERSION (arg)))
	{
	  // Record that ARG is interesting when searching down this path.
	  v = SSA_NAME_VERSION (arg);
	  gcc_checking_assert (v != 0);
	  bitmap_set_bit (interesting, v);
	  bitmap_set_bit (m_imports, v);
	}

      find_paths_to_names (e->src, interesting);

      if (v)
	bitmap_clear_bit (interesting, v);
    }
}

// Find jump threading paths to any of the SSA names in the
// INTERESTING bitmap, and register any such paths.
//
// BB is the current path being processed.

void
back_threader::find_paths_to_names (basic_block bb, bitmap interesting)
{
  if (m_visited_bbs.add (bb))
    return;

  m_path.safe_push (bb);

  // Try to resolve the path without looking back.
  if (m_path.length () > 1
      && (!m_profit.profitable_path_p (m_path, m_name, NULL)
	  || maybe_register_path ()))
    {
      m_path.pop ();
      m_visited_bbs.remove (bb);
      return;
    }

  auto_bitmap processed;
  bool done = false;
  // We use a worklist instead of iterating through the bitmap,
  // because we may add new items in-flight.
  auto_vec<tree> worklist (bitmap_count_bits (interesting));
  populate_worklist (worklist, interesting);
  while (!worklist.is_empty ())
    {
      tree name = worklist.pop ();
      unsigned i = SSA_NAME_VERSION (name);
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);
      basic_block def_bb = gimple_bb (def_stmt);

      // Process any PHIs defined in this block.
      if (def_bb == bb
	  && bitmap_set_bit (processed, i)
	  && gimple_code (def_stmt) == GIMPLE_PHI)
	{
	  resolve_phi (as_a<gphi *> (def_stmt), interesting);
	  done = true;
	  break;
	}
    }
  // If there are interesting names not yet processed, keep looking.
  if (!done)
    {
      bitmap_and_compl_into (interesting, processed);
      if (!bitmap_empty_p (interesting))
	{
	  edge_iterator iter;
	  edge e;
	  FOR_EACH_EDGE (e, iter, bb->preds)
	    if ((e->flags & EDGE_ABNORMAL) == 0)
	      find_paths_to_names (e->src, interesting);
	}
    }

  // Reset things to their original state.
  bitmap_ior_into (interesting, processed);
  m_path.pop ();
  m_visited_bbs.remove (bb);
}

// Search backwards from BB looking for paths where the final
// conditional out of BB can be determined.  NAME is the LHS of the
// final conditional.  Register such paths for jump threading.

void
back_threader::find_paths (basic_block bb, tree name)
{
  gimple *stmt = last_stmt (bb);
  if (!stmt
      || (gimple_code (stmt) != GIMPLE_COND
	  && gimple_code (stmt) != GIMPLE_SWITCH))
    return;

  if (EDGE_COUNT (bb->succs) > 1
      || single_succ_to_potentially_threadable_block (bb))
    {
      m_last_stmt = stmt;
      m_visited_bbs.empty ();
      m_path.truncate (0);
      m_name = name;
      m_solver->compute_imports (m_imports, bb);

      auto_bitmap interesting;
      bitmap_copy (interesting, m_imports);
      find_paths_to_names (bb, interesting);
    }
}

// Simple helper to get the last statement from BB, which is assumed
// to be a control statement.  Return NULL if the last statement is
// not a control statement.

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

// Search backwards from BB looking for paths where the final
// conditional maybe threaded to a successor block.  Record such paths
// for jump threading.

void
back_threader::maybe_thread_block (basic_block bb)
{
  gimple *stmt = get_gimple_control_stmt (bb);
  if (!stmt)
    return;

  enum gimple_code code = gimple_code (stmt);
  tree name;
  if (code == GIMPLE_SWITCH)
    name = gimple_switch_index (as_a <gswitch *> (stmt));
  else if (code == GIMPLE_COND)
    name = gimple_cond_lhs (stmt);
  else if (code == GIMPLE_GOTO)
    name = gimple_goto_dest (stmt);
  else
    name = NULL;

  find_paths (bb, name);
}

DEBUG_FUNCTION void
debug (const vec <basic_block> &path)
{
  dump_path (stderr, path);
  fputc ('\n', stderr);
}

void
back_threader::dump (FILE *out)
{
  m_solver->dump (out);
  fprintf (out, "\nCandidates for pre-computation:\n");
  fprintf (out, "===================================\n");

  bitmap_iterator bi;
  unsigned i;

  EXECUTE_IF_SET_IN_BITMAP (m_imports, 0, i, bi)
    {
      tree name = ssa_name (i);
      print_generic_expr (out, name, TDF_NONE);
      fprintf (out, "\n");
    }
}

void
back_threader::debug ()
{
  dump (stderr);
}

/* Examine jump threading path PATH and return TRUE if it is profitable to
   thread it, otherwise return FALSE.

   NAME is the SSA_NAME of the variable we found to have a constant
   value on PATH.  If unknown, SSA_NAME is NULL.

   If the taken edge out of the path is known ahead of time it is passed in
   TAKEN_EDGE, otherwise it is NULL.

   CREATES_IRREDUCIBLE_LOOP, if non-null is set to TRUE if threading this path
   would create an irreducible loop.

   ?? It seems we should be able to loosen some of the restrictions in
   this function after loop optimizations have run.  */

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
	fprintf (dump_file, "  FAIL: Jump-thread path not considered: "
		 "the number of basic blocks on the path "
		 "exceeds PARAM_MAX_FSM_THREAD_LENGTH.\n");
      return false;
    }

  int n_insns = 0;
  gimple_stmt_iterator gsi;
  loop_p loop = m_path[0]->loop_father;
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
      /* Remember, blocks in the path are stored in opposite order in
	 the PATH array.  The last entry in the array represents the
	 block with an outgoing edge that we will redirect to the jump
	 threading path.  Thus we don't care how many statements are
	 in that block because it will not be copied or whether or not
	 it ends in a multiway branch.  */
      if (j < m_path.length () - 1)
	{
	  int orig_n_insns = n_insns;
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
	{
	  threaded_through_latch = true;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " (latch)");
	}
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
	    fprintf (dump_file, "  FAIL: Jump-thread path not considered: "
		     "the number of instructions on the path "
		     "exceeds PARAM_MAX_FSM_THREAD_PATH_INSNS.\n");
	  return false;
	}
    }
  else if (!m_speed_p && n_insns > 1)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  FAIL: Jump-thread path not considered: "
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
		 "  FAIL: Would create irreducible loop without threading "
		 "multiway branch.\n");
      return false;
    }

  /* The generic copier used by the backthreader does not re-use an
     existing threading path to reduce code duplication.  So for that
     case, drastically reduce the number of statements we are allowed
     to copy.  */
  if (!(threaded_through_latch && threaded_multiway_branch)
      && (n_insns * param_fsm_scale_path_stmts
	  >= param_max_jump_thread_duplication_stmts))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  FAIL: Did not thread around loop and would copy too "
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
		 "  FAIL: Thread through multiway branch without threading "
		 "a multiway branch.\n");
      return false;
    }

  /* Threading through an empty latch would cause code to be added to
     the latch.  This could alter the loop form sufficiently to cause
     loop optimizations to fail.  Disable these threads until after
     loop optimizations have run.  */
  if ((threaded_through_latch
       || (taken_edge && taken_edge->dest == loop->latch))
      && !(cfun->curr_properties & PROP_loop_opts_done)
      && empty_block_p (loop->latch))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  FAIL: Thread through latch before loop opts would create non-empty latch\n");
      return false;

    }
  return true;
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
  vec<jump_thread_edge *> *jump_thread_path = allocate_thread_path ();

  // The generic copier ignores the edge type.  We can build the
  // thread edges with any type.
  for (unsigned int j = 0; j + 1 < m_path.length (); j++)
    {
      basic_block bb1 = m_path[m_path.length () - j - 1];
      basic_block bb2 = m_path[m_path.length () - j - 2];

      edge e = find_edge (bb1, bb2);
      gcc_assert (e);
      push_edge (jump_thread_path, e, EDGE_COPY_SRC_BLOCK);
    }

  push_edge (jump_thread_path, taken_edge, EDGE_NO_COPY_SRC_BLOCK);
  register_jump_thread (jump_thread_path);
  return true;
}

// Thread all suitable paths in the current function.
//
// Return TODO_flags.

unsigned int
back_threader::thread_blocks ()
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, m_fun)
    if (EDGE_COUNT (bb->succs) > 1)
      maybe_thread_block (bb);

  bool changed = m_registry.thread_through_all_blocks (true);

  if (m_flags & BT_SPEED)
    return changed ? TODO_cleanup_cfg : 0;

  return false;
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

const pass_data pass_data_thread_jumps_full =
{
  GIMPLE_PASS,
  "threadfull",
  OPTGROUP_NONE,
  TV_TREE_SSA_THREAD_JUMPS,
  ( PROP_cfg | PROP_ssa ),
  0,
  0,
  0,
  TODO_update_ssa,
};

// Early jump threading pass optimizing for size.
class pass_early_thread_jumps : public gimple_opt_pass
{
public:
  pass_early_thread_jumps (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_early_thread_jumps, ctxt)
  {}

  opt_pass * clone () override
  {
    return new pass_early_thread_jumps (m_ctxt);
  }
  void set_pass_param (unsigned int, bool param) override
  {
    m_first = param;
  }
  bool gate (function *) override
  {
    return flag_thread_jumps;
  }
  unsigned int execute (function *fun) override
  {
    back_threader threader (fun, BT_NONE, m_first);
    return threader.thread_blocks ();
  }
private:
  bool m_first;
};

// Jump threading pass without resolving of unknown SSAs.
class pass_thread_jumps : public gimple_opt_pass
{
public:
  pass_thread_jumps (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_thread_jumps, ctxt)
  {}
  opt_pass * clone (void) override
  {
    return new pass_thread_jumps (m_ctxt);
  }
  void set_pass_param (unsigned int, bool param) override
  {
    m_first = param;
  }
  bool gate (function *) override
  {
    return flag_thread_jumps && flag_expensive_optimizations;
  }
  unsigned int execute (function *fun) override
  {
    back_threader threader (fun, BT_SPEED, m_first);
    return threader.thread_blocks ();
  }
private:
  bool m_first;
};

// Jump threading pass that fully resolves unknown SSAs.
class pass_thread_jumps_full : public gimple_opt_pass
{
public:
  pass_thread_jumps_full (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_thread_jumps_full, ctxt)
  {}
  opt_pass * clone (void) override
  {
    return new pass_thread_jumps_full (m_ctxt);
  }
  void set_pass_param (unsigned int, bool param) override
  {
    m_first = param;
  }
  bool gate (function *) override
  {
    return flag_thread_jumps && flag_expensive_optimizations;
  }
  unsigned int execute (function *fun) override
  {
    back_threader threader (fun, BT_SPEED | BT_RESOLVE, m_first);
    return threader.thread_blocks ();
  }
private:
  bool m_first;
};

} // namespace {

gimple_opt_pass *
make_pass_thread_jumps (gcc::context *ctxt)
{
  return new pass_thread_jumps (ctxt);
}

gimple_opt_pass *
make_pass_thread_jumps_full (gcc::context *ctxt)
{
  return new pass_thread_jumps_full (ctxt);
}

gimple_opt_pass *
make_pass_early_thread_jumps (gcc::context *ctxt)
{
  return new pass_early_thread_jumps (ctxt);
}
