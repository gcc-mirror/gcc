/* Routines for discovering and unpropagating edge equivalences.
   Copyright (C) 2005-2019 Free Software Foundation, Inc.

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
#include "tree-pass.h"
#include "ssa.h"
#include "fold-const.h"
#include "cfganal.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "domwalk.h"
#include "tree-hash-traits.h"
#include "tree-ssa-live.h"
#include "tree-ssa-coalesce.h"

/* The basic structure describing an equivalency created by traversing
   an edge.  Traversing the edge effectively means that we can assume
   that we've seen an assignment LHS = RHS.  */
struct edge_equivalency
{
  tree rhs;
  tree lhs;
};

/* This routine finds and records edge equivalences for every edge
   in the CFG.

   When complete, each edge that creates an equivalency will have an
   EDGE_EQUIVALENCY structure hanging off the edge's AUX field.
   The caller is responsible for freeing the AUX fields.  */

static void
associate_equivalences_with_edges (void)
{
  basic_block bb;

  /* Walk over each block.  If the block ends with a control statement,
     then it might create a useful equivalence.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi = gsi_last_bb (bb);
      gimple *stmt;

      /* If the block does not end with a COND_EXPR or SWITCH_EXPR
	 then there is nothing to do.  */
      if (gsi_end_p (gsi))
	continue;

      stmt = gsi_stmt (gsi);

      if (!stmt)
	continue;

      /* A COND_EXPR may create an equivalency in a variety of different
	 ways.  */
      if (gimple_code (stmt) == GIMPLE_COND)
	{
	  edge true_edge;
	  edge false_edge;
	  struct edge_equivalency *equivalency;
	  enum tree_code code = gimple_cond_code (stmt);

	  extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

	  /* Equality tests may create one or two equivalences.  */
	  if (code == EQ_EXPR || code == NE_EXPR)
	    {
	      tree op0 = gimple_cond_lhs (stmt);
	      tree op1 = gimple_cond_rhs (stmt);

	      /* Special case comparing booleans against a constant as we
		 know the value of OP0 on both arms of the branch.  i.e., we
		 can record an equivalence for OP0 rather than COND.  */
	      if (TREE_CODE (op0) == SSA_NAME
		  && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op0)
		  && ssa_name_has_boolean_range (op0)
		  && is_gimple_min_invariant (op1)
		  && (integer_zerop (op1) || integer_onep (op1)))
		{
		  tree true_val = constant_boolean_node (true, TREE_TYPE (op0));
		  tree false_val = constant_boolean_node (false,
							  TREE_TYPE (op0));
		  if (code == EQ_EXPR)
		    {
		      equivalency = XNEW (struct edge_equivalency);
		      equivalency->lhs = op0;
		      equivalency->rhs = (integer_zerop (op1)
					  ? false_val
					  : true_val);
		      true_edge->aux = equivalency;

		      equivalency = XNEW (struct edge_equivalency);
		      equivalency->lhs = op0;
		      equivalency->rhs = (integer_zerop (op1)
					  ? true_val
					  : false_val);
		      false_edge->aux = equivalency;
		    }
		  else
		    {
		      equivalency = XNEW (struct edge_equivalency);
		      equivalency->lhs = op0;
		      equivalency->rhs = (integer_zerop (op1)
					  ? true_val
					  : false_val);
		      true_edge->aux = equivalency;

		      equivalency = XNEW (struct edge_equivalency);
		      equivalency->lhs = op0;
		      equivalency->rhs = (integer_zerop (op1)
					  ? false_val
					  : true_val);
		      false_edge->aux = equivalency;
		    }
		}

	      else if (TREE_CODE (op0) == SSA_NAME
		       && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op0)
		       && (is_gimple_min_invariant (op1)
			   || (TREE_CODE (op1) == SSA_NAME
			       && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op1))))
		{
		  /* For IEEE, -0.0 == 0.0, so we don't necessarily know
		     the sign of a variable compared against zero.  If
		     we're honoring signed zeros, then we cannot record
		     this value unless we know that the value is nonzero.  */
		  if (HONOR_SIGNED_ZEROS (op0)
		      && (TREE_CODE (op1) != REAL_CST
			  || real_equal (&dconst0, &TREE_REAL_CST (op1))))
		    continue;

		  equivalency = XNEW (struct edge_equivalency);
		  equivalency->lhs = op0;
		  equivalency->rhs = op1;
		  if (code == EQ_EXPR)
		    true_edge->aux = equivalency;
		  else
		    false_edge->aux = equivalency;

		}
	    }

	  /* ??? TRUTH_NOT_EXPR can create an equivalence too.  */
	}

      /* For a SWITCH_EXPR, a case label which represents a single
	 value and which is the only case label which reaches the
	 target block creates an equivalence.  */
      else if (gimple_code (stmt) == GIMPLE_SWITCH)
	{
	  gswitch *switch_stmt = as_a <gswitch *> (stmt);
	  tree cond = gimple_switch_index (switch_stmt);

	  if (TREE_CODE (cond) == SSA_NAME
	      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (cond))
	    {
	      int i, n_labels = gimple_switch_num_labels (switch_stmt);
	      tree *info = XCNEWVEC (tree, last_basic_block_for_fn (cfun));

	      /* Walk over the case label vector.  Record blocks
		 which are reached by a single case label which represents
		 a single value.  */
	      for (i = 0; i < n_labels; i++)
		{
		  tree label = gimple_switch_label (switch_stmt, i);
		  basic_block bb = label_to_block (cfun, CASE_LABEL (label));

		  if (CASE_HIGH (label)
		      || !CASE_LOW (label)
		      || info[bb->index])
		    info[bb->index] = error_mark_node;
		  else
		    info[bb->index] = label;
		}

	      /* Now walk over the blocks to determine which ones were
		 marked as being reached by a useful case label.  */
	      for (i = 0; i < n_basic_blocks_for_fn (cfun); i++)
		{
		  tree node = info[i];

		  if (node != NULL
		      && node != error_mark_node)
		    {
		      tree x = fold_convert (TREE_TYPE (cond), CASE_LOW (node));
		      struct edge_equivalency *equivalency;

		      /* Record an equivalency on the edge from BB to basic
			 block I.  */
		      equivalency = XNEW (struct edge_equivalency);
		      equivalency->rhs = x;
		      equivalency->lhs = cond;
		      find_edge (bb, BASIC_BLOCK_FOR_FN (cfun, i))->aux =
			equivalency;
		    }
		}
	      free (info);
	    }
	}

    }
}


/* Translating out of SSA sometimes requires inserting copies and
   constant initializations on edges to eliminate PHI nodes.

   In some cases those copies and constant initializations are
   redundant because the target already has the value on the
   RHS of the assignment.

   We previously tried to catch these cases after translating
   out of SSA form.  However, that code often missed cases.  Worse
   yet, the cases it missed were also often missed by the RTL
   optimizers.  Thus the resulting code had redundant instructions.

   This pass attempts to detect these situations before translating
   out of SSA form.

   The key concept that this pass is built upon is that these
   redundant copies and constant initializations often occur
   due to constant/copy propagating equivalences resulting from
   COND_EXPRs and SWITCH_EXPRs.

   We want to do those propagations as they can sometimes allow
   the SSA optimizers to do a better job.  However, in the cases
   where such propagations do not result in further optimization,
   we would like to "undo" the propagation to avoid the redundant
   copies and constant initializations.

   This pass works by first associating equivalences with edges in
   the CFG.  For example, the edge leading from a SWITCH_EXPR to
   its associated CASE_LABEL will have an equivalency between
   SWITCH_COND and the value in the case label.

   Once we have found the edge equivalences, we proceed to walk
   the CFG in dominator order.  As we traverse edges we record
   equivalences associated with those edges we traverse.

   When we encounter a PHI node, we walk its arguments to see if we
   have an equivalence for the PHI argument.  If so, then we replace
   the argument.

   Equivalences are looked up based on their value (think of it as
   the RHS of an assignment).   A value may be an SSA_NAME or an
   invariant.  We may have several SSA_NAMEs with the same value,
   so with each value we have a list of SSA_NAMEs that have the
   same value.  */

typedef hash_map<tree_operand_hash, auto_vec<tree> > val_ssa_equiv_t;

/* Global hash table implementing a mapping from invariant values
   to a list of SSA_NAMEs which have the same value.  We might be
   able to reuse tree-vn for this code.  */
val_ssa_equiv_t *val_ssa_equiv;

static void uncprop_into_successor_phis (basic_block);

/* Remove the most recently recorded equivalency for VALUE.  */

static void
remove_equivalence (tree value)
{
    val_ssa_equiv->get (value)->pop ();
}

/* Record EQUIVALENCE = VALUE into our hash table.  */

static void
record_equiv (tree value, tree equivalence)
{
  val_ssa_equiv->get_or_insert (value).safe_push (equivalence);
}

class uncprop_dom_walker : public dom_walker
{
public:
  uncprop_dom_walker (cdi_direction direction) : dom_walker (direction) {}

  virtual edge before_dom_children (basic_block);
  virtual void after_dom_children (basic_block);

private:

  /* As we enter each block we record the value for any edge equivalency
     leading to this block.  If no such edge equivalency exists, then we
     record NULL.  These equivalences are live until we leave the dominator
     subtree rooted at the block where we record the equivalency.  */
  auto_vec<tree, 2> m_equiv_stack;
};

/* We have finished processing the dominator children of BB, perform
   any finalization actions in preparation for leaving this node in
   the dominator tree.  */

void
uncprop_dom_walker::after_dom_children (basic_block bb ATTRIBUTE_UNUSED)
{
  /* Pop the topmost value off the equiv stack.  */
  tree value = m_equiv_stack.pop ();

  /* If that value was non-null, then pop the topmost equivalency off
     its equivalency stack.  */
  if (value != NULL)
    remove_equivalence (value);
}

/* Unpropagate values from PHI nodes in successor blocks of BB.  */

static void
uncprop_into_successor_phis (basic_block bb)
{
  edge e;
  edge_iterator ei;

  /* For each successor edge, first temporarily record any equivalence
     on that edge.  Then unpropagate values in any PHI nodes at the
     destination of the edge.  Then remove the temporary equivalence.  */
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      gimple_seq phis = phi_nodes (e->dest);
      gimple_stmt_iterator gsi;

      /* If there are no PHI nodes in this destination, then there is
	 no sense in recording any equivalences.  */
      if (gimple_seq_empty_p (phis))
	continue;

      /* Record any equivalency associated with E.  */
      if (e->aux)
	{
	  struct edge_equivalency *equiv = (struct edge_equivalency *) e->aux;
	  record_equiv (equiv->rhs, equiv->lhs);
	}

      /* Walk over the PHI nodes, unpropagating values.  */
      for (gsi = gsi_start (phis) ; !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *phi = gsi_stmt (gsi);
	  tree arg = PHI_ARG_DEF (phi, e->dest_idx);
	  tree res = PHI_RESULT (phi);

	  /* If the argument is not an invariant and can be potentially
	     coalesced with the result, then there's no point in
	     un-propagating the argument.  */
	  if (!is_gimple_min_invariant (arg)
	      && gimple_can_coalesce_p (arg, res))
	    continue;

	  /* Lookup this argument's value in the hash table.  */
	  vec<tree> *equivalences = val_ssa_equiv->get (arg);
	  if (equivalences)
	    {
	      /* Walk every equivalence with the same value.  If we find
		 one that can potentially coalesce with the PHI rsult,
		 then replace the value in the argument with its equivalent
		 SSA_NAME.  Use the most recent equivalence as hopefully
		 that results in shortest lifetimes.  */
	      for (int j = equivalences->length () - 1; j >= 0; j--)
		{
		  tree equiv = (*equivalences)[j];

		  if (gimple_can_coalesce_p (equiv, res))
		    {
		      SET_PHI_ARG_DEF (phi, e->dest_idx, equiv);
		      break;
		    }
		}
	    }
	}

      /* If we had an equivalence associated with this edge, remove it.  */
      if (e->aux)
	{
	  struct edge_equivalency *equiv = (struct edge_equivalency *) e->aux;
	  remove_equivalence (equiv->rhs);
	}
    }
}

edge
uncprop_dom_walker::before_dom_children (basic_block bb)
{
  basic_block parent;
  bool recorded = false;

  /* If this block is dominated by a single incoming edge and that edge
     has an equivalency, then record the equivalency and push the
     VALUE onto EQUIV_STACK.  Else push a NULL entry on EQUIV_STACK.  */
  parent = get_immediate_dominator (CDI_DOMINATORS, bb);
  if (parent)
    {
      edge e = single_pred_edge_ignoring_loop_edges (bb, false);

      if (e && e->src == parent && e->aux)
	{
	  struct edge_equivalency *equiv = (struct edge_equivalency *) e->aux;

	  record_equiv (equiv->rhs, equiv->lhs);
	  m_equiv_stack.safe_push (equiv->rhs);
	  recorded = true;
	}
    }

  if (!recorded)
    m_equiv_stack.safe_push (NULL_TREE);

  uncprop_into_successor_phis (bb);
  return NULL;
}

namespace {

const pass_data pass_data_uncprop =
{
  GIMPLE_PASS, /* type */
  "uncprop", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_SSA_UNCPROP, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_uncprop : public gimple_opt_pass
{
public:
  pass_uncprop (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_uncprop, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_uncprop (m_ctxt); }
  virtual bool gate (function *) { return flag_tree_dom != 0; }
  virtual unsigned int execute (function *);

}; // class pass_uncprop

unsigned int
pass_uncprop::execute (function *fun)
{
  basic_block bb;

  associate_equivalences_with_edges ();

  /* Create our global data structures.  */
  val_ssa_equiv = new val_ssa_equiv_t (1024);

  /* We're going to do a dominator walk, so ensure that we have
     dominance information.  */
  calculate_dominance_info (CDI_DOMINATORS);

  /* Recursively walk the dominator tree undoing unprofitable
     constant/copy propagations.  */
  uncprop_dom_walker (CDI_DOMINATORS).walk (fun->cfg->x_entry_block_ptr);

  /* we just need to empty elements out of the hash table, and cleanup the
    AUX field on the edges.  */
  delete val_ssa_equiv;
  val_ssa_equiv = NULL;
  FOR_EACH_BB_FN (bb, fun)
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (e->aux)
	    {
	      free (e->aux);
	      e->aux = NULL;
	    }
	}
    }
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_uncprop (gcc::context *ctxt)
{
  return new pass_uncprop (ctxt);
}
