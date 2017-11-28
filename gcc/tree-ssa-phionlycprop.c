/* Const/Copy propagation originating from degenerate PHIs
   Copyright (C) 2001-2017 Free Software Foundation, Inc.

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
#include "cfghooks.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "fold-const.h"
#include "cfgloop.h"
#include "gimple-pretty-print.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"


/* PHI-ONLY copy and constant propagation.  This pass is meant to clean
   up degenerate PHIs created by or exposed by jump threading.  */

/* Given a statement STMT, which is either a PHI node or an assignment,
   remove it from the IL.  */

static void
remove_stmt_or_phi (gimple *stmt)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);

  if (gimple_code (stmt) == GIMPLE_PHI)
    remove_phi_node (&gsi, true);
  else
    {
      gsi_remove (&gsi, true);
      release_defs (stmt);
    }
}

/* Given a statement STMT, which is either a PHI node or an assignment,
   return the "rhs" of the node, in the case of a non-degenerate
   phi, NULL is returned.  */

static tree
get_rhs_or_phi_arg (gimple *stmt)
{
  if (gimple_code (stmt) == GIMPLE_PHI)
    return degenerate_phi_result (as_a <gphi *> (stmt));
  else if (gimple_assign_single_p (stmt))
    return gimple_assign_rhs1 (stmt);
  else
    gcc_unreachable ();
}


/* Given a statement STMT, which is either a PHI node or an assignment,
   return the "lhs" of the node.  */

static tree
get_lhs_or_phi_result (gimple *stmt)
{
  if (gimple_code (stmt) == GIMPLE_PHI)
    return gimple_phi_result (stmt);
  else if (is_gimple_assign (stmt))
    return gimple_assign_lhs (stmt);
  else
    gcc_unreachable ();
}

/* Propagate RHS into all uses of LHS (when possible).

   RHS and LHS are derived from STMT, which is passed in solely so
   that we can remove it if propagation is successful.

   When propagating into a PHI node or into a statement which turns
   into a trivial copy or constant initialization, set the
   appropriate bit in INTERESTING_NAMEs so that we will visit those
   nodes as well in an effort to pick up secondary optimization
   opportunities. 

   NEED_EH_CLEANUP tracks blocks that need their EH information
   cleaned up after changing EH information on a statement.  */

static bool
propagate_rhs_into_lhs (gimple *stmt, tree lhs, tree rhs,
			bitmap interesting_names, bitmap need_eh_cleanup)
{
  bool cfg_altered = false;

  /* First verify that propagation is valid.  */
  if (may_propagate_copy (lhs, rhs))
    {
      use_operand_p use_p;
      imm_use_iterator iter;
      gimple *use_stmt;
      bool all = true;

      /* Dump details.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Replacing '");
	  print_generic_expr (dump_file, lhs, dump_flags);
	  fprintf (dump_file, "' with %s '",
	           (TREE_CODE (rhs) != SSA_NAME ? "constant" : "variable"));
		   print_generic_expr (dump_file, rhs, dump_flags);
	  fprintf (dump_file, "'\n");
	}

      /* Walk over every use of LHS and try to replace the use with RHS.
	 At this point the only reason why such a propagation would not
	 be successful would be if the use occurs in an ASM_EXPR.  */
      FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
	{
	  /* Leave debug stmts alone.  If we succeed in propagating
	     all non-debug uses, we'll drop the DEF, and propagation
	     into debug stmts will occur then.  */
	  if (gimple_debug_bind_p (use_stmt))
	    continue;

	  /* It's not always safe to propagate into an ASM_EXPR.  */
	  if (gimple_code (use_stmt) == GIMPLE_ASM
              && ! may_propagate_copy_into_asm (lhs))
	    {
	      all = false;
	      continue;
	    }

	  /* It's not ok to propagate into the definition stmt of RHS.
		<bb 9>:
		  # prephitmp.12_36 = PHI <g_67.1_6(9)>
		  g_67.1_6 = prephitmp.12_36;
		  goto <bb 9>;
	     While this is strictly all dead code we do not want to
	     deal with this here.  */
	  if (TREE_CODE (rhs) == SSA_NAME
	      && SSA_NAME_DEF_STMT (rhs) == use_stmt)
	    {
	      all = false;
	      continue;
	    }

	  /* Dump details.  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "    Original statement:");
	      print_gimple_stmt (dump_file, use_stmt, 0, dump_flags);
	    }

	  /* Propagate the RHS into this use of the LHS.  */
	  FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	    propagate_value (use_p, rhs);

	  /* Special cases to avoid useless calls into the folding
	     routines, operand scanning, etc.

	     Propagation into a PHI may cause the PHI to become
	     a degenerate, so mark the PHI as interesting.  No other
	     actions are necessary.  */
	  if (gimple_code (use_stmt) == GIMPLE_PHI)
	    {
	      tree result;

	      /* Dump details.  */
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "    Updated statement:");
		  print_gimple_stmt (dump_file, use_stmt, 0, dump_flags);
		}

	      result = get_lhs_or_phi_result (use_stmt);
	      bitmap_set_bit (interesting_names, SSA_NAME_VERSION (result));
	      continue;
	    }

	  /* From this point onward we are propagating into a
	     real statement.  Folding may (or may not) be possible,
	     we may expose new operands, expose dead EH edges,
	     etc.  */
          /* NOTE tuples. In the tuples world, fold_stmt_inplace
             cannot fold a call that simplifies to a constant,
             because the GIMPLE_CALL must be replaced by a
             GIMPLE_ASSIGN, and there is no way to effect such a
             transformation in-place.  We might want to consider
             using the more general fold_stmt here.  */
	    {
	      gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
	      fold_stmt_inplace (&gsi);
	    }

	  /* Sometimes propagation can expose new operands to the
	     renamer.  */
	  update_stmt (use_stmt);

	  /* Dump details.  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "    Updated statement:");
	      print_gimple_stmt (dump_file, use_stmt, 0, dump_flags);
	    }

	  /* If we replaced a variable index with a constant, then
	     we would need to update the invariant flag for ADDR_EXPRs.  */
          if (gimple_assign_single_p (use_stmt)
              && TREE_CODE (gimple_assign_rhs1 (use_stmt)) == ADDR_EXPR)
	    recompute_tree_invariant_for_addr_expr
                (gimple_assign_rhs1 (use_stmt));

	  /* If we cleaned up EH information from the statement,
	     mark its containing block as needing EH cleanups.  */
	  if (maybe_clean_or_replace_eh_stmt (use_stmt, use_stmt))
	    {
	      bitmap_set_bit (need_eh_cleanup, gimple_bb (use_stmt)->index);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  Flagged to clear EH edges.\n");
	    }

	  /* Propagation may expose new trivial copy/constant propagation
	     opportunities.  */
          if (gimple_assign_single_p (use_stmt)
              && TREE_CODE (gimple_assign_lhs (use_stmt)) == SSA_NAME
              && (TREE_CODE (gimple_assign_rhs1 (use_stmt)) == SSA_NAME
                  || is_gimple_min_invariant (gimple_assign_rhs1 (use_stmt))))
            {
	      tree result = get_lhs_or_phi_result (use_stmt);
	      bitmap_set_bit (interesting_names, SSA_NAME_VERSION (result));
	    }

	  /* Propagation into these nodes may make certain edges in
	     the CFG unexecutable.  We want to identify them as PHI nodes
	     at the destination of those unexecutable edges may become
	     degenerates.  */
	  else if (gimple_code (use_stmt) == GIMPLE_COND
		   || gimple_code (use_stmt) == GIMPLE_SWITCH
		   || gimple_code (use_stmt) == GIMPLE_GOTO)
            {
	      tree val;

	      if (gimple_code (use_stmt) == GIMPLE_COND)
                val = fold_binary_loc (gimple_location (use_stmt),
				   gimple_cond_code (use_stmt),
                                   boolean_type_node,
                                   gimple_cond_lhs (use_stmt),
                                   gimple_cond_rhs (use_stmt));
              else if (gimple_code (use_stmt) == GIMPLE_SWITCH)
		val = gimple_switch_index (as_a <gswitch *> (use_stmt));
	      else
		val = gimple_goto_dest  (use_stmt);

	      if (val && is_gimple_min_invariant (val))
		{
		  basic_block bb = gimple_bb (use_stmt);
		  edge te = find_taken_edge (bb, val);
		  if (!te)
		    continue;

		  edge_iterator ei;
		  edge e;
		  gimple_stmt_iterator gsi;
		  gphi_iterator psi;

		  /* Remove all outgoing edges except TE.  */
		  for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei));)
		    {
		      if (e != te)
			{
			  /* Mark all the PHI nodes at the destination of
			     the unexecutable edge as interesting.  */
                          for (psi = gsi_start_phis (e->dest);
                               !gsi_end_p (psi);
                               gsi_next (&psi))
                            {
                              gphi *phi = psi.phi ();

			      tree result = gimple_phi_result (phi);
			      int version = SSA_NAME_VERSION (result);

			      bitmap_set_bit (interesting_names, version);
			    }

			  te->probability += e->probability;

			  remove_edge (e);
			  cfg_altered = true;
			}
		      else
			ei_next (&ei);
		    }

		  gsi = gsi_last_bb (gimple_bb (use_stmt));
		  gsi_remove (&gsi, true);

		  /* And fixup the flags on the single remaining edge.  */
		  te->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);
		  te->flags &= ~EDGE_ABNORMAL;
		  te->flags |= EDGE_FALLTHRU;
	        }
	    }
	}

      /* Ensure there is nothing else to do. */
      gcc_assert (!all || has_zero_uses (lhs));

      /* If we were able to propagate away all uses of LHS, then
	 we can remove STMT.  */
      if (all)
	remove_stmt_or_phi (stmt);
    }
  return cfg_altered;
}

/* STMT is either a PHI node (potentially a degenerate PHI node) or
   a statement that is a trivial copy or constant initialization.

   Attempt to eliminate STMT by propagating its RHS into all uses of
   its LHS.  This may in turn set new bits in INTERESTING_NAMES
   for nodes we want to revisit later.

   All exit paths should clear INTERESTING_NAMES for the result
   of STMT.

   NEED_EH_CLEANUP tracks blocks that need their EH information
   cleaned up after changing EH information on a statement.  It is
   not set or queried here, but passed along to children.  */

static bool
eliminate_const_or_copy (gimple *stmt, bitmap interesting_names,
			 bitmap need_eh_cleanup)
{
  tree lhs = get_lhs_or_phi_result (stmt);
  tree rhs;
  int version = SSA_NAME_VERSION (lhs);
  bool cfg_altered = false;

  /* If the LHS of this statement or PHI has no uses, then we can
     just eliminate it.  This can occur if, for example, the PHI
     was created by block duplication due to threading and its only
     use was in the conditional at the end of the block which was
     deleted.  */
  if (has_zero_uses (lhs))
    {
      bitmap_clear_bit (interesting_names, version);
      remove_stmt_or_phi (stmt);
      return cfg_altered;
    }

  /* Get the RHS of the assignment or PHI node if the PHI is a
     degenerate.  */
  rhs = get_rhs_or_phi_arg (stmt);
  if (!rhs)
    {
      bitmap_clear_bit (interesting_names, version);
      return cfg_altered;
    }

  if (!virtual_operand_p (lhs))
    cfg_altered = propagate_rhs_into_lhs (stmt, lhs, rhs,
					  interesting_names, need_eh_cleanup);
  else
    {
      gimple *use_stmt;
      imm_use_iterator iter;
      use_operand_p use_p;
      /* For virtual operands we have to propagate into all uses as
         otherwise we will create overlapping life-ranges.  */
      FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
	FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	  SET_USE (use_p, rhs);
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs) = 1;
      remove_stmt_or_phi (stmt);
    }

  /* Note that STMT may well have been deleted by now, so do
     not access it, instead use the saved version # to clear
     T's entry in the worklist.  */
  bitmap_clear_bit (interesting_names, version);
  return cfg_altered;
}

/* The first phase in degenerate PHI elimination.

   Eliminate the degenerate PHIs in BB, then recurse on the
   dominator children of BB. 

   INTERESTING_NAMES tracks SSA_NAMEs that we may want to revisit
   in the future.  It is not set or queried here, but passed along
   to children. 

   NEED_EH_CLEANUP tracks blocks that need their EH information
   cleaned up after changing EH information on a statement.  It is
   not set or queried here, but passed along to children.  */

static bool
eliminate_degenerate_phis_1 (basic_block bb, bitmap interesting_names,
			     bitmap need_eh_cleanup)
{
  gphi_iterator gsi;
  basic_block son;
  bool cfg_altered = false;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi);)
    {
      gphi *phi = gsi.phi ();
      /* We might end up removing PHI so advance the iterator now.  */
      gsi_next (&gsi);
      cfg_altered |= eliminate_const_or_copy (phi, interesting_names,
					      need_eh_cleanup);
    }

  /* Recurse into the dominator children of BB.  */
  for (son = first_dom_son (CDI_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    cfg_altered |= eliminate_degenerate_phis_1 (son, interesting_names,
						need_eh_cleanup);

  return cfg_altered;
}


/* A very simple pass to eliminate degenerate PHI nodes from the
   IL.  This is meant to be fast enough to be able to be run several
   times in the optimization pipeline.

   Certain optimizations, particularly those which duplicate blocks
   or remove edges from the CFG can create or expose PHIs which are
   trivial copies or constant initializations.

   While we could pick up these optimizations in DOM or with the
   combination of copy-prop and CCP, those solutions are far too
   heavy-weight for our needs.

   This implementation has two phases so that we can efficiently
   eliminate the first order degenerate PHIs and second order
   degenerate PHIs.

   The first phase performs a dominator walk to identify and eliminate
   the vast majority of the degenerate PHIs.  When a degenerate PHI
   is identified and eliminated any affected statements or PHIs
   are put on a worklist.

   The second phase eliminates degenerate PHIs and trivial copies
   or constant initializations using the worklist.  This is how we
   pick up the secondary optimization opportunities with minimal
   cost.  */

namespace {

const pass_data pass_data_phi_only_cprop =
{
  GIMPLE_PASS, /* type */
  "phicprop", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_PHI_CPROP, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg | TODO_update_ssa ), /* todo_flags_finish */
};

class pass_phi_only_cprop : public gimple_opt_pass
{
public:
  pass_phi_only_cprop (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_phi_only_cprop, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_phi_only_cprop (m_ctxt); }
  virtual bool gate (function *) { return flag_tree_dom != 0; }
  virtual unsigned int execute (function *);

}; // class pass_phi_only_cprop

unsigned int
pass_phi_only_cprop::execute (function *fun)
{
  bool cfg_altered = false;

  /* Bitmap of blocks which need EH information updated.  We can not
     update it on-the-fly as doing so invalidates the dominator tree.  */
  auto_bitmap need_eh_cleanup;

  /* INTERESTING_NAMES is effectively our worklist, indexed by
     SSA_NAME_VERSION.

     A set bit indicates that the statement or PHI node which
     defines the SSA_NAME should be (re)examined to determine if
     it has become a degenerate PHI or trivial const/copy propagation
     opportunity.

     Experiments have show we generally get better compilation
     time behavior with bitmaps rather than sbitmaps.  */
  auto_bitmap interesting_names;
  auto_bitmap interesting_names1;

  calculate_dominance_info (CDI_DOMINATORS);
  cfg_altered = false;

  /* First phase.  Eliminate degenerate PHIs via a dominator
     walk of the CFG.

     Experiments have indicated that we generally get better
     compile-time behavior by visiting blocks in the first
     phase in dominator order.  Presumably this is because walking
     in dominator order leaves fewer PHIs for later examination
     by the worklist phase.  */
  cfg_altered = eliminate_degenerate_phis_1 (ENTRY_BLOCK_PTR_FOR_FN (fun),
					     interesting_names,
					     need_eh_cleanup);

  /* Second phase.  Eliminate second order degenerate PHIs as well
     as trivial copies or constant initializations identified by
     the first phase or this phase.  Basically we keep iterating
     until our set of INTERESTING_NAMEs is empty.   */
  while (!bitmap_empty_p (interesting_names))
    {
      unsigned int i;
      bitmap_iterator bi;

      /* EXECUTE_IF_SET_IN_BITMAP does not like its bitmap
	 changed during the loop.  Copy it to another bitmap and
	 use that.  */
      bitmap_copy (interesting_names1, interesting_names);

      EXECUTE_IF_SET_IN_BITMAP (interesting_names1, 0, i, bi)
	{
	  tree name = ssa_name (i);

	  /* Ignore SSA_NAMEs that have been released because
	     their defining statement was deleted (unreachable).  */
	  if (name)
	    cfg_altered
	      |= eliminate_const_or_copy (SSA_NAME_DEF_STMT (ssa_name (i)),
					  interesting_names, need_eh_cleanup);
	}
    }

  if (cfg_altered)
    {
      free_dominance_info (CDI_DOMINATORS);
      /* If we changed the CFG schedule loops for fixup by cfgcleanup.  */
      loops_state_set (LOOPS_NEED_FIXUP);
    }

  /* Propagation of const and copies may make some EH edges dead.  Purge
     such edges from the CFG as needed.  */
  if (!bitmap_empty_p (need_eh_cleanup))
    gimple_purge_all_dead_eh_edges (need_eh_cleanup);

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_phi_only_cprop (gcc::context *ctxt)
{
  return new pass_phi_only_cprop (ctxt);
}
