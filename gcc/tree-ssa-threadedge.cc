/* SSA Jump Threading
   Copyright (C) 2005-2023 Free Software Foundation, Inc.
   Contributed by Jeff Law  <law@redhat.com>

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
#include "predict.h"
#include "ssa.h"
#include "fold-const.h"
#include "cfgloop.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-threadupdate.h"
#include "tree-ssa-scopedtables.h"
#include "tree-ssa-threadedge.h"
#include "gimple-fold.h"
#include "cfganal.h"
#include "alloc-pool.h"
#include "vr-values.h"
#include "gimple-range.h"
#include "gimple-range-path.h"

/* To avoid code explosion due to jump threading, we limit the
   number of statements we are going to copy.  This variable
   holds the number of statements currently seen that we'll have
   to copy as part of the jump threading process.  */
static int stmt_count;

/* Array to record value-handles per SSA_NAME.  */
vec<tree> ssa_name_values;

/* Set the value for the SSA name NAME to VALUE.  */

void
set_ssa_name_value (tree name, tree value)
{
  if (SSA_NAME_VERSION (name) >= ssa_name_values.length ())
    ssa_name_values.safe_grow_cleared (SSA_NAME_VERSION (name) + 1, true);
  if (value && TREE_OVERFLOW_P (value))
    value = drop_tree_overflow (value);
  ssa_name_values[SSA_NAME_VERSION (name)] = value;
}

jump_threader::jump_threader (jt_simplifier *simplifier, jt_state *state)
{
  /* Initialize the per SSA_NAME value-handles array.  */
  gcc_assert (!ssa_name_values.exists ());
  ssa_name_values.create (num_ssa_names);

  dummy_cond = gimple_build_cond (NE_EXPR, integer_zero_node,
				  integer_zero_node, NULL, NULL);

  m_registry = new fwd_jt_path_registry ();
  m_simplifier = simplifier;
  m_state = state;
}

jump_threader::~jump_threader (void)
{
  ssa_name_values.release ();
  ggc_free (dummy_cond);
  delete m_registry;
}

void
jump_threader::remove_jump_threads_including (edge_def *e)
{
  m_registry->remove_jump_threads_including (e);
}

bool
jump_threader::thread_through_all_blocks (bool may_peel_loop_headers)
{
  return m_registry->thread_through_all_blocks (may_peel_loop_headers);
}

static inline bool
has_phis_p (basic_block bb)
{
  return !gsi_end_p (gsi_start_phis (bb));
}

/* Return TRUE for a block with PHIs but no statements.  */

static bool
empty_block_with_phis_p (basic_block bb)
{
  return gsi_end_p (gsi_start_nondebug_bb (bb)) && has_phis_p (bb);
}

/* Return TRUE if we may be able to thread an incoming edge into
   BB to an outgoing edge from BB.  Return FALSE otherwise.  */

static bool
potentially_threadable_block (basic_block bb)
{
  gimple_stmt_iterator gsi;

  /* Special case.  We can get blocks that are forwarders, but are
     not optimized away because they forward from outside a loop
     to the loop header.   We want to thread through them as we can
     sometimes thread to the loop exit, which is obviously profitable.
     The interesting case here is when the block has PHIs.  */
  if (empty_block_with_phis_p (bb))
    return true;

  /* If BB has a single successor or a single predecessor, then
     there is no threading opportunity.  */
  if (single_succ_p (bb) || single_pred_p (bb))
    return false;

  /* If BB does not end with a conditional, switch or computed goto,
     then there is no threading opportunity.  */
  gsi = gsi_last_bb (bb);
  if (gsi_end_p (gsi)
      || ! gsi_stmt (gsi)
      || (gimple_code (gsi_stmt (gsi)) != GIMPLE_COND
	  && gimple_code (gsi_stmt (gsi)) != GIMPLE_GOTO
	  && gimple_code (gsi_stmt (gsi)) != GIMPLE_SWITCH))
    return false;

  return true;
}

/* Record temporary equivalences created by PHIs at the target of the
   edge E.

   If a PHI which prevents threading is encountered, then return FALSE
   indicating we should not thread this edge, else return TRUE.  */

bool
jump_threader::record_temporary_equivalences_from_phis (edge e)
{
  gphi_iterator gsi;

  /* Each PHI creates a temporary equivalence, record them.
     These are context sensitive equivalences and will be removed
     later.  */
  for (gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree src = PHI_ARG_DEF_FROM_EDGE (phi, e);
      tree dst = gimple_phi_result (phi);

      /* If the desired argument is not the same as this PHI's result
	 and it is set by a PHI in E->dest, then we cannot thread
	 through E->dest.  */
      if (src != dst
	  && TREE_CODE (src) == SSA_NAME
	  && gimple_code (SSA_NAME_DEF_STMT (src)) == GIMPLE_PHI
	  && gimple_bb (SSA_NAME_DEF_STMT (src)) == e->dest)
	return false;

      /* We consider any non-virtual PHI as a statement since it
	 count result in a constant assignment or copy operation.  */
      if (!virtual_operand_p (dst))
	stmt_count++;

      m_state->register_equiv (dst, src, /*update_range=*/true);
    }
  return true;
}

/* Valueize hook for gimple_fold_stmt_to_constant_1.  */

static tree
threadedge_valueize (tree t)
{
  if (TREE_CODE (t) == SSA_NAME)
    {
      tree tem = SSA_NAME_VALUE (t);
      if (tem)
	return tem;
    }
  return t;
}

/* Try to simplify each statement in E->dest, ultimately leading to
   a simplification of the COND_EXPR at the end of E->dest.

   Record unwind information for temporary equivalences onto STACK.

   Uses M_SIMPLIFIER to further simplify statements using pass specific
   information.

   We might consider marking just those statements which ultimately
   feed the COND_EXPR.  It's not clear if the overhead of bookkeeping
   would be recovered by trying to simplify fewer statements.

   If we are able to simplify a statement into the form
   SSA_NAME = (SSA_NAME | gimple invariant), then we can record
   a context sensitive equivalence which may help us simplify
   later statements in E->dest.  */

gimple *
jump_threader::record_temporary_equivalences_from_stmts_at_dest (edge e)
{
  gimple *stmt = NULL;
  gimple_stmt_iterator gsi;
  int max_stmt_count;

  max_stmt_count = param_max_jump_thread_duplication_stmts;

  /* Walk through each statement in the block recording equivalences
     we discover.  Note any equivalences we discover are context
     sensitive (ie, are dependent on traversing E) and must be unwound
     when we're finished processing E.  */
  for (gsi = gsi_start_bb (e->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt = gsi_stmt (gsi);

      /* Ignore empty statements and labels.  */
      if (gimple_code (stmt) == GIMPLE_NOP
	  || gimple_code (stmt) == GIMPLE_LABEL
	  || is_gimple_debug (stmt))
	continue;

      /* If the statement has volatile operands, then we assume we
	 cannot thread through this block.  This is overly
	 conservative in some ways.  */
      if (gimple_code (stmt) == GIMPLE_ASM
	  && gimple_asm_volatile_p (as_a <gasm *> (stmt)))
	return NULL;

      /* If the statement is a unique builtin, we cannot thread
	 through here.  */
      if (gimple_code (stmt) == GIMPLE_CALL
	  && gimple_call_internal_p (stmt)
	  && gimple_call_internal_unique_p (stmt))
	return NULL;

      /* We cannot thread through __builtin_constant_p, because an
	 expression that is constant on two threading paths may become
	 non-constant (i.e.: phi) when they merge.  */
      if (gimple_call_builtin_p (stmt, BUILT_IN_CONSTANT_P))
	return NULL;

      /* If duplicating this block is going to cause too much code
	 expansion, then do not thread through this block.  */
      stmt_count++;
      if (stmt_count > max_stmt_count)
	{
	  /* If any of the stmts in the PATH's dests are going to be
	     killed due to threading, grow the max count
	     accordingly.  */
	  if (max_stmt_count
	      == param_max_jump_thread_duplication_stmts)
	    {
	      max_stmt_count += estimate_threading_killed_stmts (e->dest);
	      if (dump_file)
		fprintf (dump_file, "threading bb %i up to %i stmts\n",
			 e->dest->index, max_stmt_count);
	    }
	  /* If we're still past the limit, we're done.  */
	  if (stmt_count > max_stmt_count)
	    return NULL;
	}

      m_state->record_ranges_from_stmt (stmt, true);

      /* If this is not a statement that sets an SSA_NAME to a new
	 value, then do not try to simplify this statement as it will
	 not simplify in any way that is helpful for jump threading.  */
      if ((gimple_code (stmt) != GIMPLE_ASSIGN
           || TREE_CODE (gimple_assign_lhs (stmt)) != SSA_NAME)
          && (gimple_code (stmt) != GIMPLE_CALL
              || gimple_call_lhs (stmt) == NULL_TREE
              || TREE_CODE (gimple_call_lhs (stmt)) != SSA_NAME))
	continue;

      /* The result of __builtin_object_size depends on all the arguments
	 of a phi node. Temporarily using only one edge produces invalid
	 results. For example

	 if (x < 6)
	   goto l;
	 else
	   goto l;

	 l:
	 r = PHI <&w[2].a[1](2), &a.a[6](3)>
	 __builtin_object_size (r, 0)

	 The result of __builtin_object_size is defined to be the maximum of
	 remaining bytes. If we use only one edge on the phi, the result will
	 change to be the remaining bytes for the corresponding phi argument.

	 Similarly for __builtin_constant_p:

	 r = PHI <1(2), 2(3)>
	 __builtin_constant_p (r)

	 Both PHI arguments are constant, but x ? 1 : 2 is still not
	 constant.  */

      if (is_gimple_call (stmt))
	{
	  tree fndecl = gimple_call_fndecl (stmt);
	  if (fndecl
	      && fndecl_built_in_p (fndecl, BUILT_IN_NORMAL)
	      && (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_OBJECT_SIZE
		  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CONSTANT_P))
	    continue;
	}

      m_state->register_equivs_stmt (stmt, e->src, m_simplifier);
    }
  return stmt;
}

/* Simplify the control statement at the end of the block E->dest.

   Use SIMPLIFY (a pointer to a callback function) to further simplify
   a condition using pass specific information.

   Return the simplified condition or NULL if simplification could
   not be performed.  When simplifying a GIMPLE_SWITCH, we may return
   the CASE_LABEL_EXPR that will be taken.  */

tree
jump_threader::simplify_control_stmt_condition (edge e, gimple *stmt)
{
  tree cond, cached_lhs;
  enum gimple_code code = gimple_code (stmt);

  /* For comparisons, we have to update both operands, then try
     to simplify the comparison.  */
  if (code == GIMPLE_COND)
    {
      tree op0, op1;
      enum tree_code cond_code;

      op0 = gimple_cond_lhs (stmt);
      op1 = gimple_cond_rhs (stmt);
      cond_code = gimple_cond_code (stmt);

      /* Get the current value of both operands.  */
      if (TREE_CODE (op0) == SSA_NAME)
	{
	  for (int i = 0; i < 2; i++)
	    {
	      if (TREE_CODE (op0) == SSA_NAME
		  && SSA_NAME_VALUE (op0))
		op0 = SSA_NAME_VALUE (op0);
	      else
		break;
	    }
	}

      if (TREE_CODE (op1) == SSA_NAME)
	{
	  for (int i = 0; i < 2; i++)
	    {
	      if (TREE_CODE (op1) == SSA_NAME
		  && SSA_NAME_VALUE (op1))
		op1 = SSA_NAME_VALUE (op1);
	      else
		break;
	    }
	}

      const unsigned recursion_limit = 4;

      cached_lhs
	= simplify_control_stmt_condition_1 (e, stmt, op0, cond_code, op1,
					     recursion_limit);

      /* If we were testing an integer/pointer against a constant,
	 then we can trace the value of the SSA_NAME.  If a value is
	 found, then the condition will collapse to a constant.

	 Return the SSA_NAME we want to trace back rather than the full
	 expression and give the threader a chance to find its value.  */
      if (cached_lhs == NULL)
	{
	  /* Recover the original operands.  They may have been simplified
	     using context sensitive equivalences.  Those context sensitive
	     equivalences may not be valid on paths.  */
	  tree op0 = gimple_cond_lhs (stmt);
	  tree op1 = gimple_cond_rhs (stmt);

	  if ((INTEGRAL_TYPE_P (TREE_TYPE (op0))
	       || POINTER_TYPE_P (TREE_TYPE (op0)))
	      && TREE_CODE (op0) == SSA_NAME
	      && TREE_CODE (op1) == INTEGER_CST)
	    return op0;
	}

      return cached_lhs;
    }

  if (code == GIMPLE_SWITCH)
    cond = gimple_switch_index (as_a <gswitch *> (stmt));
  else if (code == GIMPLE_GOTO)
    cond = gimple_goto_dest (stmt);
  else
    gcc_unreachable ();

  /* We can have conditionals which just test the state of a variable
     rather than use a relational operator.  These are simpler to handle.  */
  if (TREE_CODE (cond) == SSA_NAME)
    {
      tree original_lhs = cond;
      cached_lhs = cond;

      /* Get the variable's current value from the equivalence chains.

	 It is possible to get loops in the SSA_NAME_VALUE chains
	 (consider threading the backedge of a loop where we have
	 a loop invariant SSA_NAME used in the condition).  */
      if (cached_lhs)
	{
	  for (int i = 0; i < 2; i++)
	    {
	      if (TREE_CODE (cached_lhs) == SSA_NAME
		  && SSA_NAME_VALUE (cached_lhs))
		cached_lhs = SSA_NAME_VALUE (cached_lhs);
	      else
		break;
	    }
	}

      /* If we haven't simplified to an invariant yet, then use the
	 pass specific callback to try and simplify it further.  */
      if (cached_lhs && ! is_gimple_min_invariant (cached_lhs))
	{
	  if (code == GIMPLE_SWITCH)
	    {
	      /* Replace the index operand of the GIMPLE_SWITCH with any LHS
		 we found before handing off to VRP.  If simplification is
	         possible, the simplified value will be a CASE_LABEL_EXPR of
		 the label that is proven to be taken.  */
	      gswitch *dummy_switch = as_a<gswitch *> (gimple_copy (stmt));
	      gimple_switch_set_index (dummy_switch, cached_lhs);
	      cached_lhs = m_simplifier->simplify (dummy_switch, stmt, e->src,
						   m_state);
	      ggc_free (dummy_switch);
	    }
	  else
	    cached_lhs = m_simplifier->simplify (stmt, stmt, e->src, m_state);
	}

      /* We couldn't find an invariant.  But, callers of this
	 function may be able to do something useful with the
	 unmodified destination.  */
      if (!cached_lhs)
	cached_lhs = original_lhs;
    }
  else
    cached_lhs = NULL;

  return cached_lhs;
}

/* Recursive helper for simplify_control_stmt_condition.  */

tree
jump_threader::simplify_control_stmt_condition_1
					(edge e,
					 gimple *stmt,
					 tree op0,
					 enum tree_code cond_code,
					 tree op1,
					 unsigned limit)
{
  if (limit == 0)
    return NULL_TREE;

  /* We may need to canonicalize the comparison.  For
     example, op0 might be a constant while op1 is an
     SSA_NAME.  Failure to canonicalize will cause us to
     miss threading opportunities.  */
  if (tree_swap_operands_p (op0, op1))
    {
      cond_code = swap_tree_comparison (cond_code);
      std::swap (op0, op1);
    }

  /* If the condition has the form (A & B) CMP 0 or (A | B) CMP 0 then
     recurse into the LHS to see if there is a simplification that
     makes this condition always true or always false along the edge
     E.  */
  if ((cond_code == EQ_EXPR || cond_code == NE_EXPR)
      && TREE_CODE (op0) == SSA_NAME
      && integer_zerop (op1))
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (op0);
      if (gimple_code (def_stmt) != GIMPLE_ASSIGN)
        ;
      else if (gimple_assign_rhs_code (def_stmt) == BIT_AND_EXPR
	       || gimple_assign_rhs_code (def_stmt) == BIT_IOR_EXPR)
	{
	  enum tree_code rhs_code = gimple_assign_rhs_code (def_stmt);
	  const tree rhs1 = gimple_assign_rhs1 (def_stmt);
	  const tree rhs2 = gimple_assign_rhs2 (def_stmt);

	  /* Is A != 0 ?  */
	  const tree res1
	    = simplify_control_stmt_condition_1 (e, def_stmt,
						 rhs1, NE_EXPR, op1,
						 limit - 1);
	  if (res1 == NULL_TREE)
	    ;
	  else if (rhs_code == BIT_AND_EXPR && integer_zerop (res1))
	    {
	      /* If A == 0 then (A & B) != 0 is always false.  */
	      if (cond_code == NE_EXPR)
	        return boolean_false_node;
	      /* If A == 0 then (A & B) == 0 is always true.  */
	      if (cond_code == EQ_EXPR)
		return boolean_true_node;
	    }
	  else if (rhs_code == BIT_IOR_EXPR && integer_nonzerop (res1))
	    {
	      /* If A != 0 then (A | B) != 0 is always true.  */
	      if (cond_code == NE_EXPR)
		return boolean_true_node;
	      /* If A != 0 then (A | B) == 0 is always false.  */
	      if (cond_code == EQ_EXPR)
		return boolean_false_node;
	    }

	  /* Is B != 0 ?  */
	  const tree res2
	    = simplify_control_stmt_condition_1 (e, def_stmt,
						 rhs2, NE_EXPR, op1,
						 limit - 1);
	  if (res2 == NULL_TREE)
	    ;
	  else if (rhs_code == BIT_AND_EXPR && integer_zerop (res2))
	    {
	      /* If B == 0 then (A & B) != 0 is always false.  */
	      if (cond_code == NE_EXPR)
	        return boolean_false_node;
	      /* If B == 0 then (A & B) == 0 is always true.  */
	      if (cond_code == EQ_EXPR)
		return boolean_true_node;
	    }
	  else if (rhs_code == BIT_IOR_EXPR && integer_nonzerop (res2))
	    {
	      /* If B != 0 then (A | B) != 0 is always true.  */
	      if (cond_code == NE_EXPR)
		return boolean_true_node;
	      /* If B != 0 then (A | B) == 0 is always false.  */
	      if (cond_code == EQ_EXPR)
		return boolean_false_node;
	    }

	  if (res1 != NULL_TREE && res2 != NULL_TREE)
	    {
	      if (rhs_code == BIT_AND_EXPR
		  && TYPE_PRECISION (TREE_TYPE (op0)) == 1
		  && integer_nonzerop (res1)
		  && integer_nonzerop (res2))
		{
		  /* If A != 0 and B != 0 then (bool)(A & B) != 0 is true.  */
		  if (cond_code == NE_EXPR)
		    return boolean_true_node;
		  /* If A != 0 and B != 0 then (bool)(A & B) == 0 is false.  */
		  if (cond_code == EQ_EXPR)
		    return boolean_false_node;
		}

	      if (rhs_code == BIT_IOR_EXPR
		  && integer_zerop (res1)
		  && integer_zerop (res2))
		{
		  /* If A == 0 and B == 0 then (A | B) != 0 is false.  */
		  if (cond_code == NE_EXPR)
		    return boolean_false_node;
		  /* If A == 0 and B == 0 then (A | B) == 0 is true.  */
		  if (cond_code == EQ_EXPR)
		    return boolean_true_node;
		}
	    }
	}
      /* Handle (A CMP B) CMP 0.  */
      else if (TREE_CODE_CLASS (gimple_assign_rhs_code (def_stmt))
	       == tcc_comparison)
	{
	  tree rhs1 = gimple_assign_rhs1 (def_stmt);
	  tree rhs2 = gimple_assign_rhs2 (def_stmt);

	  tree_code new_cond = gimple_assign_rhs_code (def_stmt);
	  if (cond_code == EQ_EXPR)
	    new_cond = invert_tree_comparison (new_cond, false);

	  tree res
	    = simplify_control_stmt_condition_1 (e, def_stmt,
						 rhs1, new_cond, rhs2,
						 limit - 1);
	  if (res != NULL_TREE && is_gimple_min_invariant (res))
	    return res;
	}
    }

  gimple_cond_set_code (dummy_cond, cond_code);
  gimple_cond_set_lhs (dummy_cond, op0);
  gimple_cond_set_rhs (dummy_cond, op1);

  /* We absolutely do not care about any type conversions
     we only care about a zero/nonzero value.  */
  fold_defer_overflow_warnings ();

  tree res = fold_binary (cond_code, boolean_type_node, op0, op1);
  if (res)
    while (CONVERT_EXPR_P (res))
      res = TREE_OPERAND (res, 0);

  fold_undefer_overflow_warnings ((res && is_gimple_min_invariant (res)),
				  stmt, WARN_STRICT_OVERFLOW_CONDITIONAL);

  /* If we have not simplified the condition down to an invariant,
     then use the pass specific callback to simplify the condition.  */
  if (!res
      || !is_gimple_min_invariant (res))
    res = m_simplifier->simplify (dummy_cond, stmt, e->src, m_state);

  return res;
}

/* Copy debug stmts from DEST's chain of single predecessors up to
   SRC, so that we don't lose the bindings as PHI nodes are introduced
   when DEST gains new predecessors.  */
void
propagate_threaded_block_debug_into (basic_block dest, basic_block src)
{
  if (!MAY_HAVE_DEBUG_BIND_STMTS)
    return;

  if (!single_pred_p (dest))
    return;

  gcc_checking_assert (dest != src);

  gimple_stmt_iterator gsi = gsi_after_labels (dest);
  int i = 0;
  const int alloc_count = 16; // ?? Should this be a PARAM?

  /* Estimate the number of debug vars overridden in the beginning of
     DEST, to tell how many we're going to need to begin with.  */
  for (gimple_stmt_iterator si = gsi;
       i * 4 <= alloc_count * 3 && !gsi_end_p (si); gsi_next (&si))
    {
      gimple *stmt = gsi_stmt (si);
      if (!is_gimple_debug (stmt))
	break;
      if (gimple_debug_nonbind_marker_p (stmt))
	continue;
      i++;
    }

  auto_vec<tree, alloc_count> fewvars;
  hash_set<tree> *vars = NULL;

  /* If we're already starting with 3/4 of alloc_count, go for a
     hash_set, otherwise start with an unordered stack-allocated
     VEC.  */
  if (i * 4 > alloc_count * 3)
    vars = new hash_set<tree>;

  /* Now go through the initial debug stmts in DEST again, this time
     actually inserting in VARS or FEWVARS.  Don't bother checking for
     duplicates in FEWVARS.  */
  for (gimple_stmt_iterator si = gsi; !gsi_end_p (si); gsi_next (&si))
    {
      gimple *stmt = gsi_stmt (si);
      if (!is_gimple_debug (stmt))
	break;

      tree var;

      if (gimple_debug_bind_p (stmt))
	var = gimple_debug_bind_get_var (stmt);
      else if (gimple_debug_source_bind_p (stmt))
	var = gimple_debug_source_bind_get_var (stmt);
      else if (gimple_debug_nonbind_marker_p (stmt))
	continue;
      else
	gcc_unreachable ();

      if (vars)
	vars->add (var);
      else
	fewvars.quick_push (var);
    }

  basic_block bb = dest;

  do
    {
      bb = single_pred (bb);
      for (gimple_stmt_iterator si = gsi_last_bb (bb);
	   !gsi_end_p (si); gsi_prev (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  if (!is_gimple_debug (stmt))
	    continue;

	  tree var;

	  if (gimple_debug_bind_p (stmt))
	    var = gimple_debug_bind_get_var (stmt);
	  else if (gimple_debug_source_bind_p (stmt))
	    var = gimple_debug_source_bind_get_var (stmt);
	  else if (gimple_debug_nonbind_marker_p (stmt))
	    continue;
	  else
	    gcc_unreachable ();

	  /* Discard debug bind overlaps.  Unlike stmts from src,
	     copied into a new block that will precede BB, debug bind
	     stmts in bypassed BBs may actually be discarded if
	     they're overwritten by subsequent debug bind stmts.  We
	     want to copy binds for all modified variables, so that we
	     retain a bind to the shared def if there is one, or to a
	     newly introduced PHI node if there is one.  Our bind will
	     end up reset if the value is dead, but that implies the
	     variable couldn't have survived, so it's fine.  We are
	     not actually running the code that performed the binds at
	     this point, we're just adding binds so that they survive
	     the new confluence, so markers should not be copied.  */
	  if (vars && vars->add (var))
	    continue;
	  else if (!vars)
	    {
	      int i = fewvars.length ();
	      while (i--)
		if (fewvars[i] == var)
		  break;
	      if (i >= 0)
		continue;
	      else if (fewvars.length () < (unsigned) alloc_count)
		fewvars.quick_push (var);
	      else
		{
		  vars = new hash_set<tree>;
		  for (i = 0; i < alloc_count; i++)
		    vars->add (fewvars[i]);
		  fewvars.release ();
		  vars->add (var);
		}
	    }

	  stmt = gimple_copy (stmt);
	  /* ??? Should we drop the location of the copy to denote
	     they're artificial bindings?  */
	  gsi_insert_before (&gsi, stmt, GSI_NEW_STMT);
	}
    }
  while (bb != src && single_pred_p (bb));

  if (vars)
    delete vars;
  else if (fewvars.exists ())
    fewvars.release ();
}

/* See if TAKEN_EDGE->dest is a threadable block with no side effecs (ie, it
   need not be duplicated as part of the CFG/SSA updating process).

   If it is threadable, add it to PATH and VISITED and recurse, ultimately
   returning TRUE from the toplevel call.   Otherwise do nothing and
   return false.  */

bool
jump_threader::thread_around_empty_blocks (vec<jump_thread_edge *> *path,
					   edge taken_edge,
					   bitmap visited)
{
  basic_block bb = taken_edge->dest;
  gimple_stmt_iterator gsi;
  gimple *stmt;
  tree cond;

  /* The key property of these blocks is that they need not be duplicated
     when threading.  Thus they cannot have visible side effects such
     as PHI nodes.  */
  if (has_phis_p (bb))
    return false;

  /* Skip over DEBUG statements at the start of the block.  */
  gsi = gsi_start_nondebug_bb (bb);

  /* If the block has no statements, but does have a single successor, then
     it's just a forwarding block and we can thread through it trivially.

     However, note that just threading through empty blocks with single
     successors is not inherently profitable.  For the jump thread to
     be profitable, we must avoid a runtime conditional.

     By taking the return value from the recursive call, we get the
     desired effect of returning TRUE when we found a profitable jump
     threading opportunity and FALSE otherwise.

     This is particularly important when this routine is called after
     processing a joiner block.  Returning TRUE too aggressively in
     that case results in pointless duplication of the joiner block.  */
  if (gsi_end_p (gsi))
    {
      if (single_succ_p (bb))
	{
	  taken_edge = single_succ_edge (bb);

	  if ((taken_edge->flags & EDGE_DFS_BACK) != 0)
	    return false;

	  if (!bitmap_bit_p (visited, taken_edge->dest->index))
	    {
	      m_registry->push_edge (path, taken_edge, EDGE_NO_COPY_SRC_BLOCK);
	      m_state->append_path (taken_edge->dest);
	      bitmap_set_bit (visited, taken_edge->dest->index);
	      return thread_around_empty_blocks (path, taken_edge, visited);
	    }
	}

      /* We have a block with no statements, but multiple successors?  */
      return false;
    }

  /* The only real statements this block can have are a control
     flow altering statement.  Anything else stops the thread.  */
  stmt = gsi_stmt (gsi);
  if (gimple_code (stmt) != GIMPLE_COND
      && gimple_code (stmt) != GIMPLE_GOTO
      && gimple_code (stmt) != GIMPLE_SWITCH)
    return false;

  /* Extract and simplify the condition.  */
  cond = simplify_control_stmt_condition (taken_edge, stmt);

  /* If the condition can be statically computed and we have not already
     visited the destination edge, then add the taken edge to our thread
     path.  */
  if (cond != NULL_TREE
      && (is_gimple_min_invariant (cond)
	  || TREE_CODE (cond) == CASE_LABEL_EXPR))
    {
      if (TREE_CODE (cond) == CASE_LABEL_EXPR)
	taken_edge = find_edge (bb, label_to_block (cfun, CASE_LABEL (cond)));
      else
	taken_edge = find_taken_edge (bb, cond);

      if (!taken_edge
	  || (taken_edge->flags & EDGE_DFS_BACK) != 0)
	return false;

      if (bitmap_bit_p (visited, taken_edge->dest->index))
	return false;
      bitmap_set_bit (visited, taken_edge->dest->index);

      m_registry->push_edge (path, taken_edge, EDGE_NO_COPY_SRC_BLOCK);
      m_state->append_path (taken_edge->dest);

      thread_around_empty_blocks (path, taken_edge, visited);
      return true;
    }

  return false;
}

/* We are exiting E->src, see if E->dest ends with a conditional
   jump which has a known value when reached via E.

   E->dest can have arbitrary side effects which, if threading is
   successful, will be maintained.

   Special care is necessary if E is a back edge in the CFG as we
   may have already recorded equivalences for E->dest into our
   various tables, including the result of the conditional at
   the end of E->dest.  Threading opportunities are severely
   limited in that case to avoid short-circuiting the loop
   incorrectly.

   Positive return value is success.  Zero return value is failure, but
   the block can still be duplicated as a joiner in a jump thread path,
   negative indicates the block should not be duplicated and thus is not
   suitable for a joiner in a jump threading path.  */

int
jump_threader::thread_through_normal_block (vec<jump_thread_edge *> *path,
					    edge e, bitmap visited)
{
  m_state->register_equivs_edge (e);

  /* PHIs create temporary equivalences.
     Note that if we found a PHI that made the block non-threadable, then
     we need to bubble that up to our caller in the same manner we do
     when we prematurely stop processing statements below.  */
  if (!record_temporary_equivalences_from_phis (e))
    return -1;

  /* Now walk each statement recording any context sensitive
     temporary equivalences we can detect.  */
  gimple *stmt = record_temporary_equivalences_from_stmts_at_dest (e);

  /* There's two reasons STMT might be null, and distinguishing
     between them is important.

     First the block may not have had any statements.  For example, it
     might have some PHIs and unconditionally transfer control elsewhere.
     Such blocks are suitable for jump threading, particularly as a
     joiner block.

     The second reason would be if we did not process all the statements
     in the block (because there were too many to make duplicating the
     block profitable.   If we did not look at all the statements, then
     we may not have invalidated everything needing invalidation.  Thus
     we must signal to our caller that this block is not suitable for
     use as a joiner in a threading path.  */
  if (!stmt)
    {
      /* First case.  The statement simply doesn't have any instructions, but
	 does have PHIs.  */
      if (empty_block_with_phis_p (e->dest))
	return 0;

      /* Second case.  */
      return -1;
    }

  /* If we stopped at a COND_EXPR or SWITCH_EXPR, see if we know which arm
     will be taken.  */
  if (gimple_code (stmt) == GIMPLE_COND
      || gimple_code (stmt) == GIMPLE_GOTO
      || gimple_code (stmt) == GIMPLE_SWITCH)
    {
      tree cond;

      /* Extract and simplify the condition.  */
      cond = simplify_control_stmt_condition (e, stmt);

      if (!cond)
	return 0;

      if (is_gimple_min_invariant (cond)
	  || TREE_CODE (cond) == CASE_LABEL_EXPR)
	{
	  edge taken_edge;
	  if (TREE_CODE (cond) == CASE_LABEL_EXPR)
	    taken_edge = find_edge (e->dest,
				    label_to_block (cfun, CASE_LABEL (cond)));
	  else
	    taken_edge = find_taken_edge (e->dest, cond);

	  basic_block dest = (taken_edge ? taken_edge->dest : NULL);

	  /* DEST could be NULL for a computed jump to an absolute
	     address.  */
	  if (dest == NULL
	      || dest == e->dest
	      || (taken_edge->flags & EDGE_DFS_BACK) != 0
	      || bitmap_bit_p (visited, dest->index))
	    return 0;

	  /* Only push the EDGE_START_JUMP_THREAD marker if this is
	     first edge on the path.  */
	  if (path->length () == 0)
	    m_registry->push_edge (path, e, EDGE_START_JUMP_THREAD);

	  m_registry->push_edge (path, taken_edge, EDGE_COPY_SRC_BLOCK);
	  m_state->append_path (taken_edge->dest);

	  /* See if we can thread through DEST as well, this helps capture
	     secondary effects of threading without having to re-run DOM or
	     VRP.

	     We don't want to thread back to a block we have already
 	     visited.  This may be overly conservative.  */
	  bitmap_set_bit (visited, dest->index);
	  bitmap_set_bit (visited, e->dest->index);
	  thread_around_empty_blocks (path, taken_edge, visited);
	  return 1;
	}
    }
  return 0;
}

/* There are basic blocks look like:
   <P0>
   p0 = a CMP b ; or p0 = (INT) (a CMP b)
   goto <X>;

   <P1>
   p1 = c CMP d
   goto <X>;

   <X>
   # phi = PHI <p0 (P0), p1 (P1)>
   if (phi != 0) goto <Y>; else goto <Z>;

   Then, edge (P0,X) or (P1,X) could be marked as EDGE_START_JUMP_THREAD
   And edge (X,Y), (X,Z) is EDGE_COPY_SRC_JOINER_BLOCK

   Return true if E is (P0,X) or (P1,X)  */

bool
edge_forwards_cmp_to_conditional_jump_through_empty_bb_p (edge e)
{
  /* See if there is only one stmt which is gcond.  */
  gcond *gs;
  if (!(gs = safe_dyn_cast<gcond *> (last_and_only_stmt (e->dest))))
    return false;

  /* See if gcond's cond is "(phi !=/== 0/1)" in the basic block.  */
  tree cond = gimple_cond_lhs (gs);
  enum tree_code code = gimple_cond_code (gs);
  tree rhs = gimple_cond_rhs (gs);
  if (TREE_CODE (cond) != SSA_NAME
      || (code != NE_EXPR && code != EQ_EXPR)
      || (!integer_onep (rhs) && !integer_zerop (rhs)))
    return false;
  gphi *phi = dyn_cast <gphi *> (SSA_NAME_DEF_STMT (cond));
  if (phi == NULL || gimple_bb (phi) != e->dest)
    return false;

  /* Check if phi's incoming value is CMP.  */
  gassign *def;
  tree value = PHI_ARG_DEF_FROM_EDGE (phi, e);
  if (TREE_CODE (value) != SSA_NAME
      || !has_single_use (value)
      || !(def = dyn_cast <gassign *> (SSA_NAME_DEF_STMT (value))))
    return false;

  /* Or if it is (INT) (a CMP b).  */
  if (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def)))
    {
      value = gimple_assign_rhs1 (def);
      if (TREE_CODE (value) != SSA_NAME
	  || !has_single_use (value)
	  || !(def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (value))))
	return false;
    }

  if (TREE_CODE_CLASS (gimple_assign_rhs_code (def)) != tcc_comparison)
    return false;

  return true;
}

/* We are exiting E->src, see if E->dest ends with a conditional jump
   which has a known value when reached via E.  If so, thread the
   edge.  */

void
jump_threader::thread_across_edge (edge e)
{
  auto_bitmap visited;

  m_state->push (e);

  stmt_count = 0;

  vec<jump_thread_edge *> *path = m_registry->allocate_thread_path ();
  bitmap_set_bit (visited, e->src->index);
  bitmap_set_bit (visited, e->dest->index);

  int threaded = 0;
  if ((e->flags & EDGE_DFS_BACK) == 0)
    threaded = thread_through_normal_block (path, e, visited);

  if (threaded > 0)
    {
      propagate_threaded_block_debug_into (path->last ()->e->dest,
					   e->dest);
      m_registry->register_jump_thread (path);
      m_state->pop ();
      return;
    }

  gcc_checking_assert (path->length () == 0);
  path->release ();

  if (threaded < 0)
    {
      /* The target block was deemed too big to duplicate.  Just quit
	 now rather than trying to use the block as a joiner in a jump
	 threading path.

	 This prevents unnecessary code growth, but more importantly if we
	 do not look at all the statements in the block, then we may have
	 missed some invalidations if we had traversed a backedge!  */
      m_state->pop ();
      return;
    }

 /* We were unable to determine what out edge from E->dest is taken.  However,
    we might still be able to thread through successors of E->dest.  This
    often occurs when E->dest is a joiner block which then fans back out
    based on redundant tests.

    If so, we'll copy E->dest and redirect the appropriate predecessor to
    the copy.  Within the copy of E->dest, we'll thread one or more edges
    to points deeper in the CFG.

    This is a stopgap until we have a more structured approach to path
    isolation.  */
  {
    edge taken_edge;
    edge_iterator ei;
    bool found;

    /* If E->dest has abnormal outgoing edges, then there's no guarantee
       we can safely redirect any of the edges.  Just punt those cases.  */
    FOR_EACH_EDGE (taken_edge, ei, e->dest->succs)
      if (taken_edge->flags & EDGE_COMPLEX)
	{
	  m_state->pop ();
	  return;
	}

    /* Look at each successor of E->dest to see if we can thread through it.  */
    FOR_EACH_EDGE (taken_edge, ei, e->dest->succs)
      {
	if ((e->flags & EDGE_DFS_BACK) != 0
	    || (taken_edge->flags & EDGE_DFS_BACK) != 0)
	  continue;

	m_state->push (taken_edge);

	/* Avoid threading to any block we have already visited.  */
	bitmap_clear (visited);
	bitmap_set_bit (visited, e->src->index);
	bitmap_set_bit (visited, e->dest->index);
	bitmap_set_bit (visited, taken_edge->dest->index);

	vec<jump_thread_edge *> *path = m_registry->allocate_thread_path ();
	m_registry->push_edge (path, e, EDGE_START_JUMP_THREAD);
	m_registry->push_edge (path, taken_edge, EDGE_COPY_SRC_JOINER_BLOCK);

	found = thread_around_empty_blocks (path, taken_edge, visited);

	if (!found)
	  found = thread_through_normal_block (path,
					       path->last ()->e, visited) > 0;

	/* If we were able to thread through a successor of E->dest, then
	   record the jump threading opportunity.  */
	if (found
	    || edge_forwards_cmp_to_conditional_jump_through_empty_bb_p (e))
	  {
	    if (taken_edge->dest != path->last ()->e->dest)
	      propagate_threaded_block_debug_into (path->last ()->e->dest,
						   taken_edge->dest);
	    m_registry->register_jump_thread (path);
	  }
	else
	  path->release ();

	m_state->pop ();
      }
  }

  m_state->pop ();
}

/* Return TRUE if BB has a single successor to a block with multiple
   incoming and outgoing edges.  */

bool
single_succ_to_potentially_threadable_block (basic_block bb)
{
  int flags = (EDGE_IGNORE | EDGE_COMPLEX | EDGE_ABNORMAL);
  return (single_succ_p (bb)
	  && (single_succ_edge (bb)->flags & flags) == 0
	  && potentially_threadable_block (single_succ (bb)));
}

/* Examine the outgoing edges from BB and conditionally
   try to thread them.  */

void
jump_threader::thread_outgoing_edges (basic_block bb)
{
  int flags = (EDGE_IGNORE | EDGE_COMPLEX | EDGE_ABNORMAL);
  gimple *last;

  if (!flag_thread_jumps)
    return;

  /* If we have an outgoing edge to a block with multiple incoming and
     outgoing edges, then we may be able to thread the edge, i.e., we
     may be able to statically determine which of the outgoing edges
     will be traversed when the incoming edge from BB is traversed.  */
  if (single_succ_to_potentially_threadable_block (bb))
    thread_across_edge (single_succ_edge (bb));
  else if ((last = last_stmt (bb))
	   && gimple_code (last) == GIMPLE_COND
	   && EDGE_COUNT (bb->succs) == 2
	   && (EDGE_SUCC (bb, 0)->flags & flags) == 0
	   && (EDGE_SUCC (bb, 1)->flags & flags) == 0)
    {
      edge true_edge, false_edge;

      extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

      /* Only try to thread the edge if it reaches a target block with
	 more than one predecessor and more than one successor.  */
      if (potentially_threadable_block (true_edge->dest))
	thread_across_edge (true_edge);

      /* Similarly for the ELSE arm.  */
      if (potentially_threadable_block (false_edge->dest))
	thread_across_edge (false_edge);
    }
}

// Marker to keep track of the start of the current path.
const basic_block jt_state::BB_MARKER = (basic_block) -1;

// Record that E is being crossed.

void
jt_state::push (edge e)
{
  m_blocks.safe_push (BB_MARKER);
  if (m_blocks.length () == 1)
    m_blocks.safe_push (e->src);
  m_blocks.safe_push (e->dest);
}

// Pop to the last pushed state.

void
jt_state::pop ()
{
  if (!m_blocks.is_empty ())
    {
      while (m_blocks.last () != BB_MARKER)
	m_blocks.pop ();
      // Pop marker.
      m_blocks.pop ();
    }
}

// Add BB to the list of blocks seen.

void
jt_state::append_path (basic_block bb)
{
  gcc_checking_assert (!m_blocks.is_empty ());
  m_blocks.safe_push (bb);
}

void
jt_state::dump (FILE *out)
{
  if (!m_blocks.is_empty ())
    {
      auto_vec<basic_block> path;
      get_path (path);
      dump_ranger (out, path);
    }
}

void
jt_state::debug ()
{
  push_dump_file save (stderr, TDF_DETAILS);
  dump (stderr);
}

// Convert the current path in jt_state into a path suitable for the
// path solver.  Return the resulting path in PATH.

void
jt_state::get_path (vec<basic_block> &path)
{
  path.truncate (0);

  for (int i = (int) m_blocks.length () - 1; i >= 0; --i)
    {
      basic_block bb = m_blocks[i];

      if (bb != BB_MARKER)
	path.safe_push (bb);
    }
}

// Record an equivalence from DST to SRC.  If UPDATE_RANGE is TRUE,
// update the value range associated with DST.

void
jt_state::register_equiv (tree dest ATTRIBUTE_UNUSED,
			  tree src ATTRIBUTE_UNUSED,
			  bool update_range ATTRIBUTE_UNUSED)
{
}

// Record any ranges calculated in STMT.  If TEMPORARY is TRUE, then
// this is a temporary equivalence and should be recorded into the
// unwind table, instead of the global table.

void
jt_state::record_ranges_from_stmt (gimple *,
				   bool temporary ATTRIBUTE_UNUSED)
{
}

// Record any equivalences created by traversing E.

void
jt_state::register_equivs_edge (edge)
{
}

void
jt_state::register_equivs_stmt (gimple *stmt, basic_block bb,
				jt_simplifier *simplifier)
{
  /* At this point we have a statement which assigns an RHS to an
     SSA_VAR on the LHS.  We want to try and simplify this statement
     to expose more context sensitive equivalences which in turn may
     allow us to simplify the condition at the end of the loop.

     Handle simple copy operations.  */
  tree cached_lhs = NULL;
  if (gimple_assign_single_p (stmt)
      && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
    cached_lhs = gimple_assign_rhs1 (stmt);
  else
    {
      /* A statement that is not a trivial copy.
	 Try to fold the new expression.  Inserting the
	 expression into the hash table is unlikely to help.  */
      /* ???  The DOM callback below can be changed to setting
	 the mprts_hook around the call to thread_across_edge,
	 avoiding the use substitution.  */
      cached_lhs = gimple_fold_stmt_to_constant_1 (stmt,
						   threadedge_valueize);
      if (NUM_SSA_OPERANDS (stmt, SSA_OP_ALL_USES) != 0
	  && (!cached_lhs
	      || (TREE_CODE (cached_lhs) != SSA_NAME
		  && !is_gimple_min_invariant (cached_lhs))))
	{
	  /* We're going to temporarily copy propagate the operands
	     and see if that allows us to simplify this statement.  */
	  tree *copy;
	  ssa_op_iter iter;
	  use_operand_p use_p;
	  unsigned int num, i = 0;

	  num = NUM_SSA_OPERANDS (stmt, SSA_OP_ALL_USES);
	  copy = XALLOCAVEC (tree, num);

	  /* Make a copy of the uses & vuses into USES_COPY, then cprop into
	     the operands.  */
	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
	    {
	      tree tmp = NULL;
	      tree use = USE_FROM_PTR (use_p);

	      copy[i++] = use;
	      if (TREE_CODE (use) == SSA_NAME)
		tmp = SSA_NAME_VALUE (use);
	      if (tmp)
		SET_USE (use_p, tmp);
	    }

	  /* Do not pass state to avoid calling the ranger with the
	     temporarily altered IL.  */
	  cached_lhs = simplifier->simplify (stmt, stmt, bb, /*state=*/NULL);

	  /* Restore the statement's original uses/defs.  */
	  i = 0;
	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
	    SET_USE (use_p, copy[i++]);
	}
    }

  /* Record the context sensitive equivalence if we were able
     to simplify this statement.  */
  if (cached_lhs
      && (TREE_CODE (cached_lhs) == SSA_NAME
	  || is_gimple_min_invariant (cached_lhs)))
    register_equiv (gimple_get_lhs (stmt), cached_lhs,
		    /*update_range=*/false);
}

// Hybrid threader implementation.

hybrid_jt_simplifier::hybrid_jt_simplifier (gimple_ranger *r,
					    path_range_query *q)
{
  m_ranger = r;
  m_query = q;
}

tree
hybrid_jt_simplifier::simplify (gimple *stmt, gimple *, basic_block,
				jt_state *state)
{
  auto_bitmap dependencies;
  auto_vec<basic_block> path;

  state->get_path (path);
  compute_exit_dependencies (dependencies, path, stmt);
  m_query->reset_path (path, dependencies);

  if (gimple_code (stmt) == GIMPLE_COND
      || gimple_code (stmt) == GIMPLE_ASSIGN)
    {
      Value_Range r (gimple_range_type (stmt));
      tree ret;
      if (m_query->range_of_stmt (r, stmt) && r.singleton_p (&ret))
	return ret;
    }
  else if (gimple_code (stmt) == GIMPLE_SWITCH)
    {
      int_range_max r;
      gswitch *switch_stmt = dyn_cast <gswitch *> (stmt);
      tree index = gimple_switch_index (switch_stmt);
      if (m_query->range_of_expr (r, index, stmt))
	return find_case_label_range (switch_stmt, &r);
    }
  return NULL;
}

// Calculate the set of exit dependencies for a path and statement to
// be simplified.  This is different than the
// compute_exit_dependencies in the path solver because the forward
// threader asks questions about statements not necessarily in the
// path.  Using the default compute_exit_dependencies in the path
// solver gets noticeably less threads.

void
hybrid_jt_simplifier::compute_exit_dependencies (bitmap dependencies,
						 const vec<basic_block> &path,
						 gimple *stmt)
{
  gori_compute &gori = m_ranger->gori ();

  // Start with the imports to the final conditional.
  bitmap_copy (dependencies, gori.imports (path[0]));

  // Add any other interesting operands we may have missed.
  if (gimple_bb (stmt) != path[0])
    {
      for (unsigned i = 0; i < gimple_num_ops (stmt); ++i)
	{
	  tree op = gimple_op (stmt, i);
	  if (op
	      && TREE_CODE (op) == SSA_NAME
	      && Value_Range::supports_type_p (TREE_TYPE (op)))
	    bitmap_set_bit (dependencies, SSA_NAME_VERSION (op));
	}
    }
}
