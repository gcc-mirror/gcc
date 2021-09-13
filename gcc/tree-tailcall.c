/* Tail call optimization on trees.
   Copyright (C) 2003-2021 Free Software Foundation, Inc.

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
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "except.h"
#include "tree-eh.h"
#include "dbgcnt.h"
#include "cfgloop.h"
#include "common/common-target.h"
#include "ipa-utils.h"
#include "tree-ssa-live.h"

/* The file implements the tail recursion elimination.  It is also used to
   analyze the tail calls in general, passing the results to the rtl level
   where they are used for sibcall optimization.

   In addition to the standard tail recursion elimination, we handle the most
   trivial cases of making the call tail recursive by creating accumulators.
   For example the following function

   int sum (int n)
   {
     if (n > 0)
       return n + sum (n - 1);
     else
       return 0;
   }

   is transformed into

   int sum (int n)
   {
     int acc = 0;

     while (n > 0)
       acc += n--;

     return acc;
   }

   To do this, we maintain two accumulators (a_acc and m_acc) that indicate
   when we reach the return x statement, we should return a_acc + x * m_acc
   instead.  They are initially initialized to 0 and 1, respectively,
   so the semantics of the function is obviously preserved.  If we are
   guaranteed that the value of the accumulator never change, we
   omit the accumulator.

   There are three cases how the function may exit.  The first one is
   handled in adjust_return_value, the other two in adjust_accumulator_values
   (the second case is actually a special case of the third one and we
   present it separately just for clarity):

   1) Just return x, where x is not in any of the remaining special shapes.
      We rewrite this to a gimple equivalent of return m_acc * x + a_acc.

   2) return f (...), where f is the current function, is rewritten in a
      classical tail-recursion elimination way, into assignment of arguments
      and jump to the start of the function.  Values of the accumulators
      are unchanged.

   3) return a + m * f(...), where a and m do not depend on call to f.
      To preserve the semantics described before we want this to be rewritten
      in such a way that we finally return

      a_acc + (a + m * f(...)) * m_acc = (a_acc + a * m_acc) + (m * m_acc) * f(...).

      I.e. we increase a_acc by a * m_acc, multiply m_acc by m and
      eliminate the tail call to f.  Special cases when the value is just
      added or just multiplied are obtained by setting a = 0 or m = 1.

   TODO -- it is possible to do similar tricks for other operations.  */

/* A structure that describes the tailcall.  */

struct tailcall
{
  /* The iterator pointing to the call statement.  */
  gimple_stmt_iterator call_gsi;

  /* True if it is a call to the current function.  */
  bool tail_recursion;

  /* The return value of the caller is mult * f + add, where f is the return
     value of the call.  */
  tree mult, add;

  /* Next tailcall in the chain.  */
  struct tailcall *next;
};

/* The variables holding the value of multiplicative and additive
   accumulator.  */
static tree m_acc, a_acc;

/* Bitmap with a bit for each function parameter which is set to true if we
   have to copy the parameter for conversion of tail-recursive calls.  */

static bitmap tailr_arg_needs_copy;

/* Returns false when the function is not suitable for tail call optimization
   from some reason (e.g. if it takes variable number of arguments).  */

static bool
suitable_for_tail_opt_p (void)
{
  if (cfun->stdarg)
    return false;

  return true;
}

/* Returns false when the function is not suitable for tail call optimization
   for some reason (e.g. if it takes variable number of arguments).
   This test must pass in addition to suitable_for_tail_opt_p in order to make
   tail call discovery happen.  */

static bool
suitable_for_tail_call_opt_p (void)
{
  tree param;

  /* alloca (until we have stack slot life analysis) inhibits
     sibling call optimizations, but not tail recursion.  */
  if (cfun->calls_alloca)
    return false;

  /* If we are using sjlj exceptions, we may need to add a call to
     _Unwind_SjLj_Unregister at exit of the function.  Which means
     that we cannot do any sibcall transformations.  */
  if (targetm_common.except_unwind_info (&global_options) == UI_SJLJ
      && current_function_has_exception_handlers ())
    return false;

  /* Any function that calls setjmp might have longjmp called from
     any called function.  ??? We really should represent this
     properly in the CFG so that this needn't be special cased.  */
  if (cfun->calls_setjmp)
    return false;

  /* Various targets don't handle tail calls correctly in functions
     that call __builtin_eh_return.  */
  if (cfun->calls_eh_return)
    return false;

  /* ??? It is OK if the argument of a function is taken in some cases,
     but not in all cases.  See PR15387 and PR19616.  Revisit for 4.1.  */
  for (param = DECL_ARGUMENTS (current_function_decl);
       param;
       param = DECL_CHAIN (param))
    if (TREE_ADDRESSABLE (param))
      return false;

  return true;
}

/* Checks whether the expression EXPR in stmt AT is independent of the
   statement pointed to by GSI (in a sense that we already know EXPR's value
   at GSI).  We use the fact that we are only called from the chain of
   basic blocks that have only single successor.  Returns the expression
   containing the value of EXPR at GSI.  */

static tree
independent_of_stmt_p (tree expr, gimple *at, gimple_stmt_iterator gsi,
		       bitmap to_move)
{
  basic_block bb, call_bb, at_bb;
  edge e;
  edge_iterator ei;

  if (is_gimple_min_invariant (expr))
    return expr;

  if (TREE_CODE (expr) != SSA_NAME)
    return NULL_TREE;

  if (bitmap_bit_p (to_move, SSA_NAME_VERSION (expr)))
    return expr;

  /* Mark the blocks in the chain leading to the end.  */
  at_bb = gimple_bb (at);
  call_bb = gimple_bb (gsi_stmt (gsi));
  for (bb = call_bb; bb != at_bb; bb = single_succ (bb))
    bb->aux = &bb->aux;
  bb->aux = &bb->aux;

  while (1)
    {
      at = SSA_NAME_DEF_STMT (expr);
      bb = gimple_bb (at);

      /* The default definition or defined before the chain.  */
      if (!bb || !bb->aux)
	break;

      if (bb == call_bb)
	{
	  for (; !gsi_end_p (gsi); gsi_next (&gsi))
	    if (gsi_stmt (gsi) == at)
	      break;

	  if (!gsi_end_p (gsi))
	    expr = NULL_TREE;
	  break;
	}

      if (gimple_code (at) != GIMPLE_PHI)
	{
	  expr = NULL_TREE;
	  break;
	}

      FOR_EACH_EDGE (e, ei, bb->preds)
	if (e->src->aux)
	  break;
      gcc_assert (e);

      expr = PHI_ARG_DEF_FROM_EDGE (at, e);
      if (TREE_CODE (expr) != SSA_NAME)
	{
	  /* The value is a constant.  */
	  break;
	}
    }

  /* Unmark the blocks.  */
  for (bb = call_bb; bb != at_bb; bb = single_succ (bb))
    bb->aux = NULL;
  bb->aux = NULL;

  return expr;
}

enum par { FAIL, OK, TRY_MOVE };

/* Simulates the effect of an assignment STMT on the return value of the tail
   recursive CALL passed in ASS_VAR.  M and A are the multiplicative and the
   additive factor for the real return value.  */

static par
process_assignment (gassign *stmt,
		    gimple_stmt_iterator call, tree *m,
		    tree *a, tree *ass_var, bitmap to_move)
{
  tree op0, op1 = NULL_TREE, non_ass_var = NULL_TREE;
  tree dest = gimple_assign_lhs (stmt);
  enum tree_code code = gimple_assign_rhs_code (stmt);
  enum gimple_rhs_class rhs_class = get_gimple_rhs_class (code);
  tree src_var = gimple_assign_rhs1 (stmt);

  /* See if this is a simple copy operation of an SSA name to the function
     result.  In that case we may have a simple tail call.  Ignore type
     conversions that can never produce extra code between the function
     call and the function return.  */
  if ((rhs_class == GIMPLE_SINGLE_RHS || gimple_assign_cast_p (stmt))
      && src_var == *ass_var)
    {
      /* Reject a tailcall if the type conversion might need
	 additional code.  */
      if (gimple_assign_cast_p (stmt))
	{
	  if (TYPE_MODE (TREE_TYPE (dest)) != TYPE_MODE (TREE_TYPE (src_var)))
	    return FAIL;

	  /* Even if the type modes are the same, if the precision of the
	     type is smaller than mode's precision,
	     reduce_to_bit_field_precision would generate additional code.  */
	  if (INTEGRAL_TYPE_P (TREE_TYPE (dest))
	      && !type_has_mode_precision_p (TREE_TYPE (dest)))
	    return FAIL;
	}

      *ass_var = dest;
      return OK;
    }

  switch (rhs_class)
    {
    case GIMPLE_BINARY_RHS:
      op1 = gimple_assign_rhs2 (stmt);

      /* Fall through.  */

    case GIMPLE_UNARY_RHS:
      op0 = gimple_assign_rhs1 (stmt);
      break;

    default:
      return FAIL;
    }

  /* Accumulator optimizations will reverse the order of operations.
     We can only do that for floating-point types if we're assuming
     that addition and multiplication are associative.  */
  if (!flag_associative_math)
    if (FLOAT_TYPE_P (TREE_TYPE (DECL_RESULT (current_function_decl))))
      return FAIL;

  if (rhs_class == GIMPLE_UNARY_RHS
      && op0 == *ass_var)
    ;
  else if (op0 == *ass_var
	   && (non_ass_var = independent_of_stmt_p (op1, stmt, call,
						    to_move)))
    ;
  else if (*ass_var
	   && op1 == *ass_var
	   && (non_ass_var = independent_of_stmt_p (op0, stmt, call,
						    to_move)))
    ;
  else
    return TRY_MOVE;

  switch (code)
    {
    case PLUS_EXPR:
      *a = non_ass_var;
      *ass_var = dest;
      return OK;

    case POINTER_PLUS_EXPR:
      if (op0 != *ass_var)
	return FAIL;
      *a = non_ass_var;
      *ass_var = dest;
      return OK;

    case MULT_EXPR:
      *m = non_ass_var;
      *ass_var = dest;
      return OK;

    case NEGATE_EXPR:
      *m = build_minus_one_cst (TREE_TYPE (op0));
      *ass_var = dest;
      return OK;

    case MINUS_EXPR:
      if (*ass_var == op0)
        *a = fold_build1 (NEGATE_EXPR, TREE_TYPE (non_ass_var), non_ass_var);
      else
        {
	  *m = build_minus_one_cst (TREE_TYPE (non_ass_var));
          *a = fold_build1 (NEGATE_EXPR, TREE_TYPE (non_ass_var), non_ass_var);
        }

      *ass_var = dest;
      return OK;

    default:
      return FAIL;
    }
}

/* Propagate VAR through phis on edge E.  */

static tree
propagate_through_phis (tree var, edge e)
{
  basic_block dest = e->dest;
  gphi_iterator gsi;

  for (gsi = gsi_start_phis (dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      if (PHI_ARG_DEF_FROM_EDGE (phi, e) == var)
        return PHI_RESULT (phi);
    }
  return var;
}

/* Argument for compute_live_vars/live_vars_at_stmt and what compute_live_vars
   returns.  Computed lazily, but just once for the function.  */
static live_vars_map *live_vars;
static vec<bitmap_head> live_vars_vec;

/* Finds tailcalls falling into basic block BB. The list of found tailcalls is
   added to the start of RET.  */

static void
find_tail_calls (basic_block bb, struct tailcall **ret)
{
  tree ass_var = NULL_TREE, ret_var, func, param;
  gimple *stmt;
  gcall *call = NULL;
  gimple_stmt_iterator gsi, agsi;
  bool tail_recursion;
  struct tailcall *nw;
  edge e;
  tree m, a;
  basic_block abb;
  size_t idx;
  tree var;

  if (!single_succ_p (bb))
    return;

  for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
    {
      stmt = gsi_stmt (gsi);

      /* Ignore labels, returns, nops, clobbers and debug stmts.  */
      if (gimple_code (stmt) == GIMPLE_LABEL
	  || gimple_code (stmt) == GIMPLE_RETURN
	  || gimple_code (stmt) == GIMPLE_NOP
	  || gimple_code (stmt) == GIMPLE_PREDICT
	  || gimple_clobber_p (stmt)
	  || is_gimple_debug (stmt))
	continue;

      /* Check for a call.  */
      if (is_gimple_call (stmt))
	{
	  call = as_a <gcall *> (stmt);
	  ass_var = gimple_call_lhs (call);
	  break;
	}

      /* Allow simple copies between local variables, even if they're
	 aggregates.  */
      if (is_gimple_assign (stmt)
	  && auto_var_in_fn_p (gimple_assign_lhs (stmt), cfun->decl)
	  && auto_var_in_fn_p (gimple_assign_rhs1 (stmt), cfun->decl))
	continue;

      /* If the statement references memory or volatile operands, fail.  */
      if (gimple_references_memory_p (stmt)
	  || gimple_has_volatile_ops (stmt))
	return;
    }

  if (gsi_end_p (gsi))
    {
      edge_iterator ei;
      /* Recurse to the predecessors.  */
      FOR_EACH_EDGE (e, ei, bb->preds)
	find_tail_calls (e->src, ret);

      return;
    }

  /* If the LHS of our call is not just a simple register or local
     variable, we can't transform this into a tail or sibling call.
     This situation happens, in (e.g.) "*p = foo()" where foo returns a
     struct.  In this case we won't have a temporary here, but we need
     to carry out the side effect anyway, so tailcall is impossible.

     ??? In some situations (when the struct is returned in memory via
     invisible argument) we could deal with this, e.g. by passing 'p'
     itself as that argument to foo, but it's too early to do this here,
     and expand_call() will not handle it anyway.  If it ever can, then
     we need to revisit this here, to allow that situation.  */
  if (ass_var
      && !is_gimple_reg (ass_var)
      && !auto_var_in_fn_p (ass_var, cfun->decl))
    return;

  /* If the call might throw an exception that wouldn't propagate out of
     cfun, we can't transform to a tail or sibling call (82081).  */
  if (stmt_could_throw_p (cfun, stmt)
      && !stmt_can_throw_external (cfun, stmt))
    return;

  /* If the function returns a value, then at present, the tail call
     must return the same type of value.  There is conceptually a copy
     between the object returned by the tail call candidate and the
     object returned by CFUN itself.

     This means that if we have:

	 lhs = f (&<retval>);    // f reads from <retval>
				 // (lhs is usually also <retval>)

     there is a copy between the temporary object returned by f and lhs,
     meaning that any use of <retval> in f occurs before the assignment
     to lhs begins.  Thus the <retval> that is live on entry to the call
     to f is really an independent local variable V that happens to be
     stored in the RESULT_DECL rather than a local VAR_DECL.

     Turning this into a tail call would remove the copy and make the
     lifetimes of the return value and V overlap.  The same applies to
     tail recursion, since if f can read from <retval>, we have to assume
     that CFUN might already have written to <retval> before the call.

     The problem doesn't apply when <retval> is passed by value, but that
     isn't a case we handle anyway.  */
  tree result_decl = DECL_RESULT (cfun->decl);
  if (result_decl
      && may_be_aliased (result_decl)
      && ref_maybe_used_by_stmt_p (call, result_decl, false))
    return;

  /* We found the call, check whether it is suitable.  */
  tail_recursion = false;
  func = gimple_call_fndecl (call);
  if (func
      && !fndecl_built_in_p (func)
      && recursive_call_p (current_function_decl, func))
    {
      tree arg;

      for (param = DECL_ARGUMENTS (current_function_decl), idx = 0;
	   param && idx < gimple_call_num_args (call);
	   param = DECL_CHAIN (param), idx ++)
	{
	  arg = gimple_call_arg (call, idx);
	  if (param != arg)
	    {
	      /* Make sure there are no problems with copying.  The parameter
	         have a copyable type and the two arguments must have reasonably
	         equivalent types.  The latter requirement could be relaxed if
	         we emitted a suitable type conversion statement.  */
	      if (!is_gimple_reg_type (TREE_TYPE (param))
		  || !useless_type_conversion_p (TREE_TYPE (param),
					         TREE_TYPE (arg)))
		break;

	      /* The parameter should be a real operand, so that phi node
		 created for it at the start of the function has the meaning
		 of copying the value.  This test implies is_gimple_reg_type
		 from the previous condition, however this one could be
		 relaxed by being more careful with copying the new value
		 of the parameter (emitting appropriate GIMPLE_ASSIGN and
		 updating the virtual operands).  */
	      if (!is_gimple_reg (param))
		break;
	    }
	}
      if (idx == gimple_call_num_args (call) && !param)
	tail_recursion = true;
    }

  /* Compute live vars if not computed yet.  */
  if (live_vars == NULL)
    {
      unsigned int cnt = 0;
      FOR_EACH_LOCAL_DECL (cfun, idx, var)
	if (VAR_P (var)
	    && auto_var_in_fn_p (var, cfun->decl)
	    && may_be_aliased (var))
	  {
	    if (live_vars == NULL)
	      live_vars = new live_vars_map;
	    live_vars->put (DECL_UID (var), cnt++);
	  }
      if (live_vars)
	live_vars_vec = compute_live_vars (cfun, live_vars);
    }

  /* Determine a bitmap of variables which are still in scope after the
     call.  */
  bitmap local_live_vars = NULL;
  if (live_vars)
    local_live_vars = live_vars_at_stmt (live_vars_vec, live_vars, call);

  /* Make sure the tail invocation of this function does not indirectly
     refer to local variables.  (Passing variables directly by value
     is OK.)  */
  FOR_EACH_LOCAL_DECL (cfun, idx, var)
    {
      if (TREE_CODE (var) != PARM_DECL
	  && auto_var_in_fn_p (var, cfun->decl)
	  && may_be_aliased (var)
	  && (ref_maybe_used_by_stmt_p (call, var, false)
	      || call_may_clobber_ref_p (call, var, false)))
	{
	  if (!VAR_P (var))
	    {
	      if (local_live_vars)
		BITMAP_FREE (local_live_vars);
	      return;
	    }
	  else
	    {
	      unsigned int *v = live_vars->get (DECL_UID (var));
	      if (bitmap_bit_p (local_live_vars, *v))
		{
		  BITMAP_FREE (local_live_vars);
		  return;
		}
	    }
	}
    }

  if (local_live_vars)
    BITMAP_FREE (local_live_vars);

  /* Now check the statements after the call.  None of them has virtual
     operands, so they may only depend on the call through its return
     value.  The return value should also be dependent on each of them,
     since we are running after dce.  */
  m = NULL_TREE;
  a = NULL_TREE;
  auto_bitmap to_move_defs;
  auto_vec<gimple *> to_move_stmts;

  abb = bb;
  agsi = gsi;
  while (1)
    {
      tree tmp_a = NULL_TREE;
      tree tmp_m = NULL_TREE;
      gsi_next (&agsi);

      while (gsi_end_p (agsi))
	{
	  ass_var = propagate_through_phis (ass_var, single_succ_edge (abb));
	  abb = single_succ (abb);
	  agsi = gsi_start_bb (abb);
	}

      stmt = gsi_stmt (agsi);
      if (gimple_code (stmt) == GIMPLE_RETURN)
	break;

      if (gimple_code (stmt) == GIMPLE_LABEL
	  || gimple_code (stmt) == GIMPLE_NOP
	  || gimple_code (stmt) == GIMPLE_PREDICT
	  || gimple_clobber_p (stmt)
	  || is_gimple_debug (stmt))
	continue;

      if (gimple_code (stmt) != GIMPLE_ASSIGN)
	return;

      /* This is a gimple assign. */
      par ret = process_assignment (as_a <gassign *> (stmt), gsi,
				    &tmp_m, &tmp_a, &ass_var, to_move_defs);
      if (ret == FAIL)
	return;
      else if (ret == TRY_MOVE)
	{
	  if (! tail_recursion)
	    return;
	  /* Do not deal with checking dominance, the real fix is to
	     do path isolation for the transform phase anyway, removing
	     the need to compute the accumulators with new stmts.  */
	  if (abb != bb)
	    return;
	  for (unsigned opno = 1; opno < gimple_num_ops (stmt); ++opno)
	    {
	      tree op = gimple_op (stmt, opno);
	      if (independent_of_stmt_p (op, stmt, gsi, to_move_defs) != op)
		return;
	    }
	  bitmap_set_bit (to_move_defs,
			  SSA_NAME_VERSION (gimple_assign_lhs (stmt)));
	  to_move_stmts.safe_push (stmt);
	  continue;
	}

      if (tmp_a)
	{
	  tree type = TREE_TYPE (tmp_a);
	  if (a)
	    a = fold_build2 (PLUS_EXPR, type, fold_convert (type, a), tmp_a);
	  else
	    a = tmp_a;
	}
      if (tmp_m)
	{
	  tree type = TREE_TYPE (tmp_m);
	  if (m)
	    m = fold_build2 (MULT_EXPR, type, fold_convert (type, m), tmp_m);
	  else
	    m = tmp_m;

	  if (a)
	    a = fold_build2 (MULT_EXPR, type, fold_convert (type, a), tmp_m);
	}
    }

  /* See if this is a tail call we can handle.  */
  ret_var = gimple_return_retval (as_a <greturn *> (stmt));

  /* We may proceed if there either is no return value, or the return value
     is identical to the call's return or if the return decl is an empty type
     variable and the call's return was not assigned. */
  if (ret_var
      && (ret_var != ass_var
	  && !(is_empty_type (TREE_TYPE (ret_var)) && !ass_var)))
    return;

  /* If this is not a tail recursive call, we cannot handle addends or
     multiplicands.  */
  if (!tail_recursion && (m || a))
    return;

  /* For pointers only allow additions.  */
  if (m && POINTER_TYPE_P (TREE_TYPE (DECL_RESULT (current_function_decl))))
    return;

  /* Move queued defs.  */
  if (tail_recursion)
    {
      unsigned i;
      FOR_EACH_VEC_ELT (to_move_stmts, i, stmt)
	{
	  gimple_stmt_iterator mgsi = gsi_for_stmt (stmt);
	  gsi_move_before (&mgsi, &gsi);
	}
      if (!tailr_arg_needs_copy)
	tailr_arg_needs_copy = BITMAP_ALLOC (NULL);
      for (param = DECL_ARGUMENTS (current_function_decl), idx = 0;
	   param;
	   param = DECL_CHAIN (param), idx++)
	{
	  tree ddef, arg = gimple_call_arg (call, idx);
	  if (is_gimple_reg (param)
	      && (ddef = ssa_default_def (cfun, param))
	      && (arg != ddef))
	    bitmap_set_bit (tailr_arg_needs_copy, idx);
	}
    }

  nw = XNEW (struct tailcall);

  nw->call_gsi = gsi;

  nw->tail_recursion = tail_recursion;

  nw->mult = m;
  nw->add = a;

  nw->next = *ret;
  *ret = nw;
}

/* Helper to insert PHI_ARGH to the phi of VAR in the destination of edge E.  */

static void
add_successor_phi_arg (edge e, tree var, tree phi_arg)
{
  gphi_iterator gsi;

  for (gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    if (PHI_RESULT (gsi.phi ()) == var)
      break;

  gcc_assert (!gsi_end_p (gsi));
  add_phi_arg (gsi.phi (), phi_arg, e, UNKNOWN_LOCATION);
}

/* Creates a GIMPLE statement which computes the operation specified by
   CODE, ACC and OP1 to a new variable with name LABEL and inserts the
   statement in the position specified by GSI.  Returns the
   tree node of the statement's result.  */

static tree
adjust_return_value_with_ops (enum tree_code code, const char *label,
			      tree acc, tree op1, gimple_stmt_iterator gsi)
{

  tree ret_type = TREE_TYPE (DECL_RESULT (current_function_decl));
  tree result = make_temp_ssa_name (ret_type, NULL, label);
  gassign *stmt;

  if (POINTER_TYPE_P (ret_type))
    {
      gcc_assert (code == PLUS_EXPR && TREE_TYPE (acc) == sizetype);
      code = POINTER_PLUS_EXPR;
    }
  if (types_compatible_p (TREE_TYPE (acc), TREE_TYPE (op1))
      && code != POINTER_PLUS_EXPR)
    stmt = gimple_build_assign (result, code, acc, op1);
  else
    {
      tree tem;
      if (code == POINTER_PLUS_EXPR)
	tem = fold_build2 (code, TREE_TYPE (op1), op1, acc);
      else
	tem = fold_build2 (code, TREE_TYPE (op1),
			   fold_convert (TREE_TYPE (op1), acc), op1);
      tree rhs = fold_convert (ret_type, tem);
      rhs = force_gimple_operand_gsi (&gsi, rhs,
				      false, NULL, true, GSI_SAME_STMT);
      stmt = gimple_build_assign (result, rhs);
    }

  gsi_insert_before (&gsi, stmt, GSI_NEW_STMT);
  return result;
}

/* Creates a new GIMPLE statement that adjusts the value of accumulator ACC by
   the computation specified by CODE and OP1 and insert the statement
   at the position specified by GSI as a new statement.  Returns new SSA name
   of updated accumulator.  */

static tree
update_accumulator_with_ops (enum tree_code code, tree acc, tree op1,
			     gimple_stmt_iterator gsi)
{
  gassign *stmt;
  tree var = copy_ssa_name (acc);
  if (types_compatible_p (TREE_TYPE (acc), TREE_TYPE (op1)))
    stmt = gimple_build_assign (var, code, acc, op1);
  else
    {
      tree rhs = fold_convert (TREE_TYPE (acc),
			       fold_build2 (code,
					    TREE_TYPE (op1),
					    fold_convert (TREE_TYPE (op1), acc),
					    op1));
      rhs = force_gimple_operand_gsi (&gsi, rhs,
				      false, NULL, false, GSI_CONTINUE_LINKING);
      stmt = gimple_build_assign (var, rhs);
    }
  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
  return var;
}

/* Adjust the accumulator values according to A and M after GSI, and update
   the phi nodes on edge BACK.  */

static void
adjust_accumulator_values (gimple_stmt_iterator gsi, tree m, tree a, edge back)
{
  tree var, a_acc_arg, m_acc_arg;

  if (m)
    m = force_gimple_operand_gsi (&gsi, m, true, NULL, true, GSI_SAME_STMT);
  if (a)
    a = force_gimple_operand_gsi (&gsi, a, true, NULL, true, GSI_SAME_STMT);

  a_acc_arg = a_acc;
  m_acc_arg = m_acc;
  if (a)
    {
      if (m_acc)
	{
	  if (integer_onep (a))
	    var = m_acc;
	  else
	    var = adjust_return_value_with_ops (MULT_EXPR, "acc_tmp", m_acc,
						a, gsi);
	}
      else
	var = a;

      a_acc_arg = update_accumulator_with_ops (PLUS_EXPR, a_acc, var, gsi);
    }

  if (m)
    m_acc_arg = update_accumulator_with_ops (MULT_EXPR, m_acc, m, gsi);

  if (a_acc)
    add_successor_phi_arg (back, a_acc, a_acc_arg);

  if (m_acc)
    add_successor_phi_arg (back, m_acc, m_acc_arg);
}

/* Adjust value of the return at the end of BB according to M and A
   accumulators.  */

static void
adjust_return_value (basic_block bb, tree m, tree a)
{
  tree retval;
  greturn *ret_stmt = as_a <greturn *> (gimple_seq_last_stmt (bb_seq (bb)));
  gimple_stmt_iterator gsi = gsi_last_bb (bb);

  gcc_assert (gimple_code (ret_stmt) == GIMPLE_RETURN);

  retval = gimple_return_retval (ret_stmt);
  if (!retval || retval == error_mark_node)
    return;

  if (m)
    retval = adjust_return_value_with_ops (MULT_EXPR, "mul_tmp", m_acc, retval,
					   gsi);
  if (a)
    retval = adjust_return_value_with_ops (PLUS_EXPR, "acc_tmp", a_acc, retval,
					   gsi);
  gimple_return_set_retval (ret_stmt, retval);
  update_stmt (ret_stmt);
}

/* Subtract COUNT and FREQUENCY from the basic block and it's
   outgoing edge.  */
static void
decrease_profile (basic_block bb, profile_count count)
{
  bb->count = bb->count - count;
  if (!single_succ_p (bb))
    {
      gcc_assert (!EDGE_COUNT (bb->succs));
      return;
    }
}

/* Eliminates tail call described by T.  TMP_VARS is a list of
   temporary variables used to copy the function arguments.
   Allocates *NEW_LOOP if not already done and initializes it.  */

static void
eliminate_tail_call (struct tailcall *t, class loop *&new_loop)
{
  tree param, rslt;
  gimple *stmt, *call;
  tree arg;
  size_t idx;
  basic_block bb, first;
  edge e;
  gphi *phi;
  gphi_iterator gpi;
  gimple_stmt_iterator gsi;
  gimple *orig_stmt;

  stmt = orig_stmt = gsi_stmt (t->call_gsi);
  bb = gsi_bb (t->call_gsi);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Eliminated tail recursion in bb %d : ",
	       bb->index);
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  gcc_assert (is_gimple_call (stmt));

  first = single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun));

  /* Remove the code after call_gsi that will become unreachable.  The
     possibly unreachable code in other blocks is removed later in
     cfg cleanup.  */
  gsi = t->call_gsi;
  gimple_stmt_iterator gsi2 = gsi_last_bb (gimple_bb (gsi_stmt (gsi)));
  while (gsi_stmt (gsi2) != gsi_stmt (gsi))
    {
      gimple *t = gsi_stmt (gsi2);
      /* Do not remove the return statement, so that redirect_edge_and_branch
	 sees how the block ends.  */
      if (gimple_code (t) != GIMPLE_RETURN)
	{
	  gimple_stmt_iterator gsi3 = gsi2;
	  gsi_prev (&gsi2);
	  gsi_remove (&gsi3, true);
	  release_defs (t);
	}
      else
	gsi_prev (&gsi2);
    }

  /* Number of executions of function has reduced by the tailcall.  */
  e = single_succ_edge (gsi_bb (t->call_gsi));

  profile_count count = e->count ();

  /* When profile is inconsistent and the recursion edge is more frequent
     than number of executions of functions, scale it down, so we do not end
     up with 0 executions of entry block.  */
  if (count >= ENTRY_BLOCK_PTR_FOR_FN (cfun)->count)
    count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count.apply_scale (7, 8);
  decrease_profile (EXIT_BLOCK_PTR_FOR_FN (cfun), count);
  decrease_profile (ENTRY_BLOCK_PTR_FOR_FN (cfun), count);
  if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
    decrease_profile (e->dest, count);

  /* Replace the call by a jump to the start of function.  */
  e = redirect_edge_and_branch (single_succ_edge (gsi_bb (t->call_gsi)),
				first);
  gcc_assert (e);
  PENDING_STMT (e) = NULL;

  /* Add the new loop.  */
  if (!new_loop)
    {
      new_loop = alloc_loop ();
      new_loop->header = first;
      new_loop->finite_p = true;
    }
  else
    gcc_assert (new_loop->header == first);

  /* Add phi node entries for arguments.  The ordering of the phi nodes should
     be the same as the ordering of the arguments.  */
  for (param = DECL_ARGUMENTS (current_function_decl),
	 idx = 0, gpi = gsi_start_phis (first);
       param;
       param = DECL_CHAIN (param), idx++)
    {
      if (!bitmap_bit_p (tailr_arg_needs_copy, idx))
	continue;

      arg = gimple_call_arg (stmt, idx);
      phi = gpi.phi ();
      gcc_assert (param == SSA_NAME_VAR (PHI_RESULT (phi)));

      add_phi_arg (phi, arg, e, gimple_location (stmt));
      gsi_next (&gpi);
    }

  /* Update the values of accumulators.  */
  adjust_accumulator_values (t->call_gsi, t->mult, t->add, e);

  call = gsi_stmt (t->call_gsi);
  rslt = gimple_call_lhs (call);
  if (rslt != NULL_TREE && TREE_CODE (rslt) == SSA_NAME)
    {
      /* Result of the call will no longer be defined.  So adjust the
	 SSA_NAME_DEF_STMT accordingly.  */
      SSA_NAME_DEF_STMT (rslt) = gimple_build_nop ();
    }

  gsi_remove (&t->call_gsi, true);
  release_defs (call);
}

/* Optimizes the tailcall described by T.  If OPT_TAILCALLS is true, also
   mark the tailcalls for the sibcall optimization.  */

static bool
optimize_tail_call (struct tailcall *t, bool opt_tailcalls,
		    class loop *&new_loop)
{
  if (t->tail_recursion)
    {
      eliminate_tail_call (t, new_loop);
      return true;
    }

  if (opt_tailcalls)
    {
      gcall *stmt = as_a <gcall *> (gsi_stmt (t->call_gsi));

      gimple_call_set_tail (stmt, true);
      cfun->tail_call_marked = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
        {
	  fprintf (dump_file, "Found tail call ");
	  print_gimple_stmt (dump_file, stmt, 0, dump_flags);
	  fprintf (dump_file, " in bb %i\n", (gsi_bb (t->call_gsi))->index);
	}
    }

  return false;
}

/* Creates a tail-call accumulator of the same type as the return type of the
   current function.  LABEL is the name used to creating the temporary
   variable for the accumulator.  The accumulator will be inserted in the
   phis of a basic block BB with single predecessor with an initial value
   INIT converted to the current function return type.  */

static tree
create_tailcall_accumulator (const char *label, basic_block bb, tree init)
{
  tree ret_type = TREE_TYPE (DECL_RESULT (current_function_decl));
  if (POINTER_TYPE_P (ret_type))
    ret_type = sizetype;

  tree tmp = make_temp_ssa_name (ret_type, NULL, label);
  gphi *phi;

  phi = create_phi_node (tmp, bb);
  add_phi_arg (phi, init, single_pred_edge (bb),
	       UNKNOWN_LOCATION);
  return PHI_RESULT (phi);
}

/* Optimizes tail calls in the function, turning the tail recursion
   into iteration.  */

static unsigned int
tree_optimize_tail_calls_1 (bool opt_tailcalls)
{
  edge e;
  bool phis_constructed = false;
  struct tailcall *tailcalls = NULL, *act, *next;
  bool changed = false;
  basic_block first = single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  tree param;
  gimple *stmt;
  edge_iterator ei;

  if (!suitable_for_tail_opt_p ())
    return 0;
  if (opt_tailcalls)
    opt_tailcalls = suitable_for_tail_call_opt_p ();

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    {
      /* Only traverse the normal exits, i.e. those that end with return
	 statement.  */
      stmt = last_stmt (e->src);

      if (stmt
	  && gimple_code (stmt) == GIMPLE_RETURN)
	find_tail_calls (e->src, &tailcalls);
    }

  if (live_vars)
    {
      destroy_live_vars (live_vars_vec);
      delete live_vars;
      live_vars = NULL;
    }

  /* Construct the phi nodes and accumulators if necessary.  */
  a_acc = m_acc = NULL_TREE;
  for (act = tailcalls; act; act = act->next)
    {
      if (!act->tail_recursion)
	continue;

      if (!phis_constructed)
	{
	  /* Ensure that there is only one predecessor of the block
	     or if there are existing degenerate PHI nodes.  */
	  if (!single_pred_p (first)
	      || !gimple_seq_empty_p (phi_nodes (first)))
	    first =
	      split_edge (single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun)));

	  /* Copy the args if needed.  */
	  unsigned idx;
	  for (param = DECL_ARGUMENTS (current_function_decl), idx = 0;
	       param;
	       param = DECL_CHAIN (param), idx++)
	    if (bitmap_bit_p (tailr_arg_needs_copy, idx))
	      {
		tree name = ssa_default_def (cfun, param);
		tree new_name = make_ssa_name (param, SSA_NAME_DEF_STMT (name));
		gphi *phi;

		set_ssa_default_def (cfun, param, new_name);
		phi = create_phi_node (name, first);
		add_phi_arg (phi, new_name, single_pred_edge (first),
			     EXPR_LOCATION (param));
	      }
	  phis_constructed = true;
	}
      tree ret_type = TREE_TYPE (DECL_RESULT (current_function_decl));
      if (POINTER_TYPE_P (ret_type))
	ret_type = sizetype;

      if (act->add && !a_acc)
	a_acc = create_tailcall_accumulator ("add_acc", first,
					     build_zero_cst (ret_type));

      if (act->mult && !m_acc)
	m_acc = create_tailcall_accumulator ("mult_acc", first,
					     build_one_cst (ret_type));
    }

  if (a_acc || m_acc)
    {
      /* When the tail call elimination using accumulators is performed,
	 statements adding the accumulated value are inserted at all exits.
	 This turns all other tail calls to non-tail ones.  */
      opt_tailcalls = false;
    }

  class loop *new_loop = NULL;
  for (; tailcalls; tailcalls = next)
    {
      next = tailcalls->next;
      changed |= optimize_tail_call (tailcalls, opt_tailcalls, new_loop);
      free (tailcalls);
    }
  if (new_loop)
    add_loop (new_loop, loops_for_fn (cfun)->tree_root);

  if (a_acc || m_acc)
    {
      /* Modify the remaining return statements.  */
      FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
	{
	  stmt = last_stmt (e->src);

	  if (stmt
	      && gimple_code (stmt) == GIMPLE_RETURN)
	    adjust_return_value (e->src, m_acc, a_acc);
	}
    }

  if (changed)
    free_dominance_info (CDI_DOMINATORS);

  /* Add phi nodes for the virtual operands defined in the function to the
     header of the loop created by tail recursion elimination.  Do so
     by triggering the SSA renamer.  */
  if (phis_constructed)
    mark_virtual_operands_for_renaming (cfun);

  if (tailr_arg_needs_copy)
    BITMAP_FREE (tailr_arg_needs_copy);

  if (changed)
    return TODO_cleanup_cfg | TODO_update_ssa_only_virtuals;
  return 0;
}

static bool
gate_tail_calls (void)
{
  return flag_optimize_sibling_calls != 0 && dbg_cnt (tail_call);
}

static unsigned int
execute_tail_calls (void)
{
  return tree_optimize_tail_calls_1 (true);
}

namespace {

const pass_data pass_data_tail_recursion =
{
  GIMPLE_PASS, /* type */
  "tailr", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_tail_recursion : public gimple_opt_pass
{
public:
  pass_tail_recursion (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tail_recursion, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_tail_recursion (m_ctxt); }
  virtual bool gate (function *) { return gate_tail_calls (); }
  virtual unsigned int execute (function *)
    {
      return tree_optimize_tail_calls_1 (false);
    }

}; // class pass_tail_recursion

} // anon namespace

gimple_opt_pass *
make_pass_tail_recursion (gcc::context *ctxt)
{
  return new pass_tail_recursion (ctxt);
}

namespace {

const pass_data pass_data_tail_calls =
{
  GIMPLE_PASS, /* type */
  "tailc", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_tail_calls : public gimple_opt_pass
{
public:
  pass_tail_calls (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tail_calls, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return gate_tail_calls (); }
  virtual unsigned int execute (function *) { return execute_tail_calls (); }

}; // class pass_tail_calls

} // anon namespace

gimple_opt_pass *
make_pass_tail_calls (gcc::context *ctxt)
{
  return new pass_tail_calls (ctxt);
}
