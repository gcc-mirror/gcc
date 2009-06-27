/* SSA Jump Threading
   Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "rtl.h"
#include "tm_p.h"
#include "ggc.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "output.h"
#include "expr.h"
#include "function.h"
#include "diagnostic.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-flow.h"
#include "real.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "langhooks.h"
#include "params.h"

/* To avoid code explosion due to jump threading, we limit the
   number of statements we are going to copy.  This variable
   holds the number of statements currently seen that we'll have
   to copy as part of the jump threading process.  */
static int stmt_count;

/* Array to record value-handles per SSA_NAME.  */
VEC(tree,heap) *ssa_name_values;

/* Set the value for the SSA name NAME to VALUE.  */

void
set_ssa_name_value (tree name, tree value)
{
  if (SSA_NAME_VERSION (name) >= VEC_length (tree, ssa_name_values))
    VEC_safe_grow_cleared (tree, heap, ssa_name_values,
			   SSA_NAME_VERSION (name) + 1);
  VEC_replace (tree, ssa_name_values, SSA_NAME_VERSION (name), value);
}

/* Initialize the per SSA_NAME value-handles array.  Returns it.  */
void
threadedge_initialize_values (void)
{
  gcc_assert (ssa_name_values == NULL);
  ssa_name_values = VEC_alloc(tree, heap, num_ssa_names);
}

/* Free the per SSA_NAME value-handle array.  */
void
threadedge_finalize_values (void)
{
  VEC_free(tree, heap, ssa_name_values);
}

/* Return TRUE if we may be able to thread an incoming edge into
   BB to an outgoing edge from BB.  Return FALSE otherwise.  */

bool
potentially_threadable_block (basic_block bb)
{
  gimple_stmt_iterator gsi;

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

/* Return the LHS of any ASSERT_EXPR where OP appears as the first
   argument to the ASSERT_EXPR and in which the ASSERT_EXPR dominates
   BB.  If no such ASSERT_EXPR is found, return OP.  */

static tree
lhs_of_dominating_assert (tree op, basic_block bb, gimple stmt)
{
  imm_use_iterator imm_iter;
  gimple use_stmt;
  use_operand_p use_p;

  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, op)
    {
      use_stmt = USE_STMT (use_p);
      if (use_stmt != stmt
          && gimple_assign_single_p (use_stmt)
          && TREE_CODE (gimple_assign_rhs1 (use_stmt)) == ASSERT_EXPR
          && TREE_OPERAND (gimple_assign_rhs1 (use_stmt), 0) == op
	  && dominated_by_p (CDI_DOMINATORS, bb, gimple_bb (use_stmt)))
	{
	  return gimple_assign_lhs (use_stmt);
	}
    }
  return op;
}

/* We record temporary equivalences created by PHI nodes or
   statements within the target block.  Doing so allows us to
   identify more jump threading opportunities, even in blocks
   with side effects.

   We keep track of those temporary equivalences in a stack
   structure so that we can unwind them when we're done processing
   a particular edge.  This routine handles unwinding the data
   structures.  */

static void
remove_temporary_equivalences (VEC(tree, heap) **stack)
{
  while (VEC_length (tree, *stack) > 0)
    {
      tree prev_value, dest;

      dest = VEC_pop (tree, *stack);

      /* A NULL value indicates we should stop unwinding, otherwise
	 pop off the next entry as they're recorded in pairs.  */
      if (dest == NULL)
	break;

      prev_value = VEC_pop (tree, *stack);
      set_ssa_name_value (dest, prev_value);
    }
}

/* Record a temporary equivalence, saving enough information so that
   we can restore the state of recorded equivalences when we're
   done processing the current edge.  */

static void
record_temporary_equivalence (tree x, tree y, VEC(tree, heap) **stack)
{
  tree prev_x = SSA_NAME_VALUE (x);

  if (TREE_CODE (y) == SSA_NAME)
    {
      tree tmp = SSA_NAME_VALUE (y);
      y = tmp ? tmp : y;
    }

  set_ssa_name_value (x, y);
  VEC_reserve (tree, heap, *stack, 2);
  VEC_quick_push (tree, *stack, prev_x);
  VEC_quick_push (tree, *stack, x);
}

/* Record temporary equivalences created by PHIs at the target of the
   edge E.  Record unwind information for the equivalences onto STACK. 

   If a PHI which prevents threading is encountered, then return FALSE
   indicating we should not thread this edge, else return TRUE.  */

static bool
record_temporary_equivalences_from_phis (edge e, VEC(tree, heap) **stack)
{
  gimple_stmt_iterator gsi;

  /* Each PHI creates a temporary equivalence, record them.
     These are context sensitive equivalences and will be removed
     later.  */
  for (gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      tree src = PHI_ARG_DEF_FROM_EDGE (phi, e);
      tree dst = gimple_phi_result (phi);

      /* If the desired argument is not the same as this PHI's result 
	 and it is set by a PHI in E->dest, then we can not thread
	 through E->dest.  */
      if (src != dst
	  && TREE_CODE (src) == SSA_NAME
	  && gimple_code (SSA_NAME_DEF_STMT (src)) == GIMPLE_PHI
	  && gimple_bb (SSA_NAME_DEF_STMT (src)) == e->dest)
	return false;

      /* We consider any non-virtual PHI as a statement since it
	 count result in a constant assignment or copy operation.  */
      if (is_gimple_reg (dst))
	stmt_count++;

      record_temporary_equivalence (dst, src, stack);
    }
  return true;
}

/* Fold the RHS of an assignment statement and return it as a tree.
   May return NULL_TREE if no simplification is possible.  */

static tree
fold_assignment_stmt (gimple stmt)
{
  enum tree_code subcode = gimple_assign_rhs_code (stmt);

  switch (get_gimple_rhs_class (subcode))
    {
    case GIMPLE_SINGLE_RHS:
      {
        tree rhs = gimple_assign_rhs1 (stmt);

        if (TREE_CODE (rhs) == COND_EXPR)
          {
            /* Sadly, we have to handle conditional assignments specially
               here, because fold expects all the operands of an expression
               to be folded before the expression itself is folded, but we
               can't just substitute the folded condition here.  */
            tree cond = fold (COND_EXPR_COND (rhs));
            if (cond == boolean_true_node)
              rhs = COND_EXPR_THEN (rhs);
            else if (cond == boolean_false_node)
              rhs = COND_EXPR_ELSE (rhs);
          }

        return fold (rhs);
      }
      break;
    case GIMPLE_UNARY_RHS:
      {
        tree lhs = gimple_assign_lhs (stmt);
        tree op0 = gimple_assign_rhs1 (stmt);
        return fold_unary (subcode, TREE_TYPE (lhs), op0);
      }
      break;
    case GIMPLE_BINARY_RHS:
      {
        tree lhs = gimple_assign_lhs (stmt);
        tree op0 = gimple_assign_rhs1 (stmt);
        tree op1 = gimple_assign_rhs2 (stmt);
        return fold_binary (subcode, TREE_TYPE (lhs), op0, op1);
      }
      break;
    default:
      gcc_unreachable ();
    }
}

/* Try to simplify each statement in E->dest, ultimately leading to
   a simplification of the COND_EXPR at the end of E->dest.

   Record unwind information for temporary equivalences onto STACK.

   Use SIMPLIFY (a pointer to a callback function) to further simplify
   statements using pass specific information. 

   We might consider marking just those statements which ultimately
   feed the COND_EXPR.  It's not clear if the overhead of bookkeeping
   would be recovered by trying to simplify fewer statements.

   If we are able to simplify a statement into the form
   SSA_NAME = (SSA_NAME | gimple invariant), then we can record
   a context sensitive equivalence which may help us simplify
   later statements in E->dest.  */

static gimple
record_temporary_equivalences_from_stmts_at_dest (edge e,
						  VEC(tree, heap) **stack,
						  tree (*simplify) (gimple,
								    gimple))
{
  gimple stmt = NULL;
  gimple_stmt_iterator gsi;
  int max_stmt_count;

  max_stmt_count = PARAM_VALUE (PARAM_MAX_JUMP_THREAD_DUPLICATION_STMTS);

  /* Walk through each statement in the block recording equivalences
     we discover.  Note any equivalences we discover are context
     sensitive (ie, are dependent on traversing E) and must be unwound
     when we're finished processing E.  */
  for (gsi = gsi_start_bb (e->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      tree cached_lhs = NULL;

      stmt = gsi_stmt (gsi);

      /* Ignore empty statements and labels.  */
      if (gimple_code (stmt) == GIMPLE_NOP || gimple_code (stmt) == GIMPLE_LABEL)
	continue;

      /* If the statement has volatile operands, then we assume we
	 can not thread through this block.  This is overly
	 conservative in some ways.  */
      if (gimple_code (stmt) == GIMPLE_ASM && gimple_asm_volatile_p (stmt))
	return NULL;

      /* If duplicating this block is going to cause too much code
	 expansion, then do not thread through this block.  */
      stmt_count++;
      if (stmt_count > max_stmt_count)
	return NULL;

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
	      && (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_OBJECT_SIZE
		  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CONSTANT_P))
	    continue;
	}

      /* At this point we have a statement which assigns an RHS to an
	 SSA_VAR on the LHS.  We want to try and simplify this statement
	 to expose more context sensitive equivalences which in turn may
	 allow us to simplify the condition at the end of the loop. 

	 Handle simple copy operations as well as implied copies from
	 ASSERT_EXPRs.  */
      if (gimple_assign_single_p (stmt)
          && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
	cached_lhs = gimple_assign_rhs1 (stmt);
      else if (gimple_assign_single_p (stmt)
               && TREE_CODE (gimple_assign_rhs1 (stmt)) == ASSERT_EXPR)
	cached_lhs = TREE_OPERAND (gimple_assign_rhs1 (stmt), 0);
      else
	{
	  /* A statement that is not a trivial copy or ASSERT_EXPR.
	     We're going to temporarily copy propagate the operands
	     and see if that allows us to simplify this statement.  */
	  tree *copy;
	  ssa_op_iter iter;
	  use_operand_p use_p;
	  unsigned int num, i = 0;

	  num = NUM_SSA_OPERANDS (stmt, (SSA_OP_USE | SSA_OP_VUSE));
	  copy = XCNEWVEC (tree, num);

	  /* Make a copy of the uses & vuses into USES_COPY, then cprop into
	     the operands.  */
	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE | SSA_OP_VUSE)
	    {
	      tree tmp = NULL;
	      tree use = USE_FROM_PTR (use_p);

	      copy[i++] = use;
	      if (TREE_CODE (use) == SSA_NAME)
		tmp = SSA_NAME_VALUE (use);
	      if (tmp)
		SET_USE (use_p, tmp);
	    }

	  /* Try to fold/lookup the new expression.  Inserting the
	     expression into the hash table is unlikely to help.  */
          if (is_gimple_call (stmt))
            cached_lhs = fold_call_stmt (stmt, false);
	  else
            cached_lhs = fold_assignment_stmt (stmt);

          if (!cached_lhs
              || (TREE_CODE (cached_lhs) != SSA_NAME
                  && !is_gimple_min_invariant (cached_lhs)))
            cached_lhs = (*simplify) (stmt, stmt);
          
	  /* Restore the statement's original uses/defs.  */
	  i = 0;
	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE | SSA_OP_VUSE)
	    SET_USE (use_p, copy[i++]);

	  free (copy);
	}

      /* Record the context sensitive equivalence if we were able
	 to simplify this statement.  */
      if (cached_lhs
	  && (TREE_CODE (cached_lhs) == SSA_NAME
	      || is_gimple_min_invariant (cached_lhs)))
	record_temporary_equivalence (gimple_get_lhs (stmt), cached_lhs, stack);
    }
  return stmt;
}

/* Simplify the control statement at the end of the block E->dest.

   To avoid allocating memory unnecessarily, a scratch GIMPLE_COND
   is available to use/clobber in DUMMY_COND.

   Use SIMPLIFY (a pointer to a callback function) to further simplify
   a condition using pass specific information.

   Return the simplified condition or NULL if simplification could
   not be performed.  */

static tree
simplify_control_stmt_condition (edge e,
				 gimple stmt,
				 gimple dummy_cond,
				 tree (*simplify) (gimple, gimple),
				 bool handle_dominating_asserts)
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
          tree tmp = SSA_NAME_VALUE (op0);
	  if (tmp)
	    op0 = tmp;
	}

      if (TREE_CODE (op1) == SSA_NAME)
	{
	  tree tmp = SSA_NAME_VALUE (op1);
	  if (tmp)
	    op1 = tmp;
	}

      if (handle_dominating_asserts)
	{
	  /* Now see if the operand was consumed by an ASSERT_EXPR
	     which dominates E->src.  If so, we want to replace the
	     operand with the LHS of the ASSERT_EXPR.  */
	  if (TREE_CODE (op0) == SSA_NAME)
	    op0 = lhs_of_dominating_assert (op0, e->src, stmt);

	  if (TREE_CODE (op1) == SSA_NAME)
	    op1 = lhs_of_dominating_assert (op1, e->src, stmt);
	}

      /* We may need to canonicalize the comparison.  For
	 example, op0 might be a constant while op1 is an
	 SSA_NAME.  Failure to canonicalize will cause us to
	 miss threading opportunities.  */
      if (tree_swap_operands_p (op0, op1, false))
	{
	  tree tmp;
	  cond_code = swap_tree_comparison (cond_code);
	  tmp = op0;
	  op0 = op1;
	  op1 = tmp;
	}

      /* Stuff the operator and operands into our dummy conditional
	 expression.  */
      gimple_cond_set_code (dummy_cond, cond_code);
      gimple_cond_set_lhs (dummy_cond, op0);
      gimple_cond_set_rhs (dummy_cond, op1);

      /* We absolutely do not care about any type conversions
         we only care about a zero/nonzero value.  */
      fold_defer_overflow_warnings ();

      cached_lhs = fold_binary (cond_code, boolean_type_node, op0, op1);
      if (cached_lhs)
	while (CONVERT_EXPR_P (cached_lhs))
          cached_lhs = TREE_OPERAND (cached_lhs, 0);

      fold_undefer_overflow_warnings ((cached_lhs
                                       && is_gimple_min_invariant (cached_lhs)),
				      stmt, WARN_STRICT_OVERFLOW_CONDITIONAL);

      /* If we have not simplified the condition down to an invariant,
	 then use the pass specific callback to simplify the condition.  */
      if (!cached_lhs
          || !is_gimple_min_invariant (cached_lhs))
        cached_lhs = (*simplify) (dummy_cond, stmt);

      return cached_lhs;
    }

  if (code == GIMPLE_SWITCH)
    cond = gimple_switch_index (stmt);
  else if (code == GIMPLE_GOTO)
    cond = gimple_goto_dest (stmt);
  else
    gcc_unreachable ();

  /* We can have conditionals which just test the state of a variable
     rather than use a relational operator.  These are simpler to handle.  */
  if (TREE_CODE (cond) == SSA_NAME)
    {
      cached_lhs = cond;

      /* Get the variable's current value from the equivalence chains.

	 It is possible to get loops in the SSA_NAME_VALUE chains
	 (consider threading the backedge of a loop where we have
	 a loop invariant SSA_NAME used in the condition.  */
      if (cached_lhs
	  && TREE_CODE (cached_lhs) == SSA_NAME
	  && SSA_NAME_VALUE (cached_lhs))
	cached_lhs = SSA_NAME_VALUE (cached_lhs);

      /* If we're dominated by a suitable ASSERT_EXPR, then
	 update CACHED_LHS appropriately.  */
      if (handle_dominating_asserts && TREE_CODE (cached_lhs) == SSA_NAME)
	cached_lhs = lhs_of_dominating_assert (cached_lhs, e->src, stmt);

      /* If we haven't simplified to an invariant yet, then use the
	 pass specific callback to try and simplify it further.  */
      if (cached_lhs && ! is_gimple_min_invariant (cached_lhs))
        cached_lhs = (*simplify) (stmt, stmt);
    }
  else
    cached_lhs = NULL;

  return cached_lhs;
}

/* We are exiting E->src, see if E->dest ends with a conditional
   jump which has a known value when reached via E. 

   Special care is necessary if E is a back edge in the CFG as we
   may have already recorded equivalences for E->dest into our
   various tables, including the result of the conditional at
   the end of E->dest.  Threading opportunities are severely
   limited in that case to avoid short-circuiting the loop
   incorrectly.

   Note it is quite common for the first block inside a loop to
   end with a conditional which is either always true or always
   false when reached via the loop backedge.  Thus we do not want
   to blindly disable threading across a loop backedge.
 
   DUMMY_COND is a shared cond_expr used by condition simplification as scratch,
   to avoid allocating memory.
 
   HANDLE_DOMINATING_ASSERTS is true if we should try to replace operands of
   the simplified condition with left-hand sides of ASSERT_EXPRs they are
   used in.
 
   STACK is used to undo temporary equivalences created during the walk of
   E->dest.

   SIMPLIFY is a pass-specific function used to simplify statements.  */

void
thread_across_edge (gimple dummy_cond,
		    edge e,
		    bool handle_dominating_asserts,
		    VEC(tree, heap) **stack,
		    tree (*simplify) (gimple, gimple))
{
  gimple stmt;

  /* If E is a backedge, then we want to verify that the COND_EXPR,
     SWITCH_EXPR or GOTO_EXPR at the end of e->dest is not affected
     by any statements in e->dest.  If it is affected, then it is not
     safe to thread this edge.  */
  if (e->flags & EDGE_DFS_BACK)
    {
      ssa_op_iter iter;
      use_operand_p use_p;
      gimple last = gsi_stmt (gsi_last_bb (e->dest));

      FOR_EACH_SSA_USE_OPERAND (use_p, last, iter, SSA_OP_USE | SSA_OP_VUSE)
	{
	  tree use = USE_FROM_PTR (use_p);

          if (TREE_CODE (use) == SSA_NAME
	      && gimple_code (SSA_NAME_DEF_STMT (use)) != GIMPLE_PHI
	      && gimple_bb (SSA_NAME_DEF_STMT (use)) == e->dest)
	    goto fail;
	}
    }
     
  stmt_count = 0;

  /* PHIs create temporary equivalences.  */
  if (!record_temporary_equivalences_from_phis (e, stack))
    goto fail;

  /* Now walk each statement recording any context sensitive
     temporary equivalences we can detect.  */
  stmt = record_temporary_equivalences_from_stmts_at_dest (e, stack, simplify);
  if (!stmt)
    goto fail;

  /* If we stopped at a COND_EXPR or SWITCH_EXPR, see if we know which arm
     will be taken.  */
  if (gimple_code (stmt) == GIMPLE_COND
      || gimple_code (stmt) == GIMPLE_GOTO
      || gimple_code (stmt) == GIMPLE_SWITCH)
    {
      tree cond;

      /* Extract and simplify the condition.  */
      cond = simplify_control_stmt_condition (e, stmt, dummy_cond, simplify, handle_dominating_asserts);

      if (cond && is_gimple_min_invariant (cond))
	{
	  edge taken_edge = find_taken_edge (e->dest, cond);
	  basic_block dest = (taken_edge ? taken_edge->dest : NULL);

	  if (dest == e->dest)
	    goto fail;

	  remove_temporary_equivalences (stack);
	  register_jump_thread (e, taken_edge);
	}
    }

 fail:
  remove_temporary_equivalences (stack);
}
