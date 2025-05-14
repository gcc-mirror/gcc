/* Tail call optimization on trees.
   Copyright (C) 2003-2025 Free Software Foundation, Inc.

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
#include "intl.h"
#include "common/common-target.h"
#include "ipa-utils.h"
#include "tree-ssa-live.h"
#include "diagnostic-core.h"
#include "gimple-range.h"
#include "alloc-pool.h"
#include "sreal.h"
#include "symbol-summary.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "attribs.h"
#include "asan.h"

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

  /* True if there is __tsan_func_exit call after the call.  */
  bool has_tsan_func_exit;

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

static void maybe_error_musttail (gcall *call, const char *err, bool);

/* Returns false when the function is not suitable for tail call optimization
   from some reason (e.g. if it takes variable number of arguments). CALL
   is call to report for.  */

static bool
suitable_for_tail_opt_p (gcall *call, bool diag_musttail)
{
  if (cfun->stdarg)
    {
      maybe_error_musttail (call, _("caller uses stdargs"), diag_musttail);
      return false;
    }

  return true;
}

/* Returns false when the function is not suitable for tail call optimization
   for some reason (e.g. if it takes variable number of arguments).
   This test must pass in addition to suitable_for_tail_opt_p in order to make
   tail call discovery happen. CALL is call to report error for.  */

static bool
suitable_for_tail_call_opt_p (gcall *call, bool diag_musttail)
{
  /* alloca (until we have stack slot life analysis) inhibits
     sibling call optimizations, but not tail recursion.  */
  if (cfun->calls_alloca)
    {
      maybe_error_musttail (call, _("caller uses alloca"), diag_musttail);
      return false;
    }

  /* If we are using sjlj exceptions, we may need to add a call to
     _Unwind_SjLj_Unregister at exit of the function.  Which means
     that we cannot do any sibcall transformations.  */
  if (targetm_common.except_unwind_info (&global_options) == UI_SJLJ
      && current_function_has_exception_handlers ())
    {
      maybe_error_musttail (call, _("caller uses sjlj exceptions"),
			    diag_musttail);
      return false;
    }

  /* Any function that calls setjmp might have longjmp called from
     any called function.  ??? We really should represent this
     properly in the CFG so that this needn't be special cased.  */
  if (cfun->calls_setjmp)
    {
      maybe_error_musttail (call, _("caller uses setjmp"), diag_musttail);
      return false;
    }

  /* Various targets don't handle tail calls correctly in functions
     that call __builtin_eh_return.  */
  if (cfun->calls_eh_return)
    {
      maybe_error_musttail (call, _("caller uses __builtin_eh_return"),
			    diag_musttail);
      return false;
    }

  if (diag_musttail
      && gimple_call_must_tail_p (call)
      && warn_musttail_local_addr)
    for (unsigned int i = 0; i < gimple_call_num_args (call); i++)
      {
	tree arg = gimple_call_arg (call, i);
	if (!POINTER_TYPE_P (TREE_TYPE (arg)))
	  continue;
	if (TREE_CODE (arg) == ADDR_EXPR)
	  {
	    arg = get_base_address (TREE_OPERAND (arg, 0));
	    if (auto_var_in_fn_p (arg, current_function_decl))
	      {
		if (TREE_CODE (arg) == LABEL_DECL)
		  warning_at (gimple_location (call), OPT_Wmusttail_local_addr,
			      "address of label passed to %<musttail%> "
			      "call argument");
		else if (TREE_CODE (arg) == PARM_DECL)
		  warning_at (gimple_location (call), OPT_Wmusttail_local_addr,
			      "address of parameter %qD passed to "
			      "%<musttail%> call argument", arg);
		else if (!DECL_ARTIFICIAL (arg) && DECL_NAME (arg))
		  warning_at (gimple_location (call), OPT_Wmusttail_local_addr,
			      "address of automatic variable %qD passed to "
			      "%<musttail%> call argument", arg);
		else
		  warning_at (gimple_location (call), OPT_Wmusttail_local_addr,
			      "address of local variable passed to "
			      "%<musttail%> call argument");
		suppress_warning (call, OPT_Wmaybe_musttail_local_addr);
	      }
	  }
      }

  return true;
}

/* Return single successor edge ignoring EDGE_EH edges.  */

static edge
single_non_eh_succ_edge (basic_block bb)
{
   edge e, ret = NULL;
   edge_iterator ei;
   FOR_EACH_EDGE (e, ei, bb->succs)
    if ((e->flags & EDGE_EH) == 0)
      {
	gcc_assert (ret == NULL);
	ret = e;
      }
  gcc_assert (ret);
  return ret;
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
  for (bb = call_bb; bb != at_bb; bb = single_non_eh_succ_edge (bb)->dest)
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
  for (bb = call_bb; bb != at_bb; bb = single_non_eh_succ_edge (bb)->dest)
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

  /* We at least cannot build -1 for all fixed point types.  */
  if (FIXED_POINT_TYPE_P (TREE_TYPE (DECL_RESULT (current_function_decl))))
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

/* Report an error for failing to tail convert must call CALL
   with error message ERR. Also clear the flag to prevent further
   errors.  */

static void
maybe_error_musttail (gcall *call, const char *err, bool diag_musttail)
{
  if (gimple_call_must_tail_p (call) && diag_musttail)
    {
      error_at (gimple_location (call), "cannot tail-call: %s", err);
      /* Avoid another error. ??? If there are multiple reasons why tail
	 calls fail it might be useful to report them all to avoid
	 whack-a-mole for the user. But currently there is too much
	 redundancy in the reporting, so keep it simple.  */
      gimple_call_set_must_tail (call, false); /* Avoid another error.  */
      gimple_call_set_tail (call, false);
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Cannot tail-call: %s: ", err);
      print_gimple_stmt (dump_file, call, 0, TDF_SLIM);
    }
}

/* Return true if there is no real work performed in the exception
   path starting at BB and it will in the end result in external exception.
   Search at most CNT basic blocks (so that we don't need to do trivial
   loop discovery).  */
static bool
empty_eh_cleanup (basic_block bb, int *eh_has_tsan_func_exit, int cnt)
{
  if (EDGE_COUNT (bb->succs) > 1)
    return false;

  for (gimple_stmt_iterator gsi = gsi_after_labels (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *g = gsi_stmt (gsi);
      if (is_gimple_debug (g) || gimple_clobber_p (g))
	continue;
      if (eh_has_tsan_func_exit
	  && !*eh_has_tsan_func_exit
	  && sanitize_flags_p (SANITIZE_THREAD)
	  && gimple_call_builtin_p (g, BUILT_IN_TSAN_FUNC_EXIT))
	{
	  *eh_has_tsan_func_exit = 1;
	  continue;
	}
      if (is_gimple_resx (g) && stmt_can_throw_external (cfun, g))
	return true;
      return false;
    }
  if (!single_succ_p (bb))
    return false;
  if (cnt == 1)
    return false;
  return empty_eh_cleanup (single_succ (bb), eh_has_tsan_func_exit, cnt - 1);
}

/* Argument for compute_live_vars/live_vars_at_stmt and what compute_live_vars
   returns.  Computed lazily, but just once for the function.  */
static live_vars_map *live_vars;
static vec<bitmap_head> live_vars_vec;

/* Finds tailcalls falling into basic block BB.  The list of found tailcalls is
   added to the start of RET.  When ONLY_MUSTTAIL is set only handle musttail.
   Update OPT_TAILCALLS as output parameter.  If DIAG_MUSTTAIL, diagnose
   failures for musttail calls.  RETRY_TSAN_FUNC_EXIT is initially 0 and
   in that case the last call is attempted to be tail called, including
   __tsan_func_exit with -fsanitize=thread.  It is set to -1 if we
   detect __tsan_func_exit call and in that case tree_optimize_tail_calls_1
   will retry with it set to 1 (regardless of whether turning the
   __tsan_func_exit was successfully detected as tail call or not) and that
   will allow turning musttail calls before that call into tail calls as well
   by adding __tsan_func_exit call before the call.  */

static void
find_tail_calls (basic_block bb, struct tailcall **ret, bool only_musttail,
		 bool &opt_tailcalls, bool diag_musttail,
		 int &retry_tsan_func_exit)
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
  bool only_tailr = false;
  bool has_tsan_func_exit = false;
  int eh_has_tsan_func_exit = -1;

  if (!single_succ_p (bb)
      && (EDGE_COUNT (bb->succs) || !cfun->has_musttail || !diag_musttail))
    {
      /* If there is an abnormal edge assume it's the only extra one.
	 Tolerate that case so that we can give better error messages
	 for musttail later.  */
      if (!has_abnormal_or_eh_outgoing_edge_p (bb))
	{
	  if (dump_file)
	    fprintf (dump_file, "Basic block %d has extra exit edges\n",
		     bb->index);
	  return;
	}
      if (!cfun->has_musttail)
	return;
    }

  bool bad_stmt = false;
  gimple *last_stmt = nullptr;
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

      if (cfun->has_musttail
	  && sanitize_flags_p (SANITIZE_THREAD)
	  && gimple_call_builtin_p (stmt, BUILT_IN_TSAN_FUNC_EXIT)
	  && diag_musttail)
	{
	  if (retry_tsan_func_exit == 0)
	    retry_tsan_func_exit = -1;
	  else if (retry_tsan_func_exit == 1)
	    continue;
	}

      if (!last_stmt)
	last_stmt = stmt;
      /* Check for a call.  */
      if (is_gimple_call (stmt))
	{
	  call = as_a <gcall *> (stmt);
	  /* Handle only musttail calls when not optimizing.  */
	  if (only_musttail && !gimple_call_must_tail_p (call))
	    return;
	  if (bad_stmt)
	    {
	      maybe_error_musttail (call, _("memory reference or volatile "
					    "after call"), diag_musttail);
	      return;
	    }
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
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "Cannot handle ");
	      print_gimple_stmt (dump_file, stmt, 0);
	    }
	  bad_stmt = true;
	  if (!cfun->has_musttail)
	    break;
	}
    }

  if (bad_stmt)
    return;

  if (gsi_end_p (gsi))
    {
      edge_iterator ei;
      /* Recurse to the predecessors.  */
      FOR_EACH_EDGE (e, ei, bb->preds)
	find_tail_calls (e->src, ret, only_musttail, opt_tailcalls,
			 diag_musttail, retry_tsan_func_exit);

      return;
    }

  if (!suitable_for_tail_opt_p (call, diag_musttail))
    return;

  if (!suitable_for_tail_call_opt_p (call, diag_musttail))
    opt_tailcalls = false;

  /* ??? It is OK if the argument of a function is taken in some cases,
     but not in all cases.  See PR15387 and PR19616.  Revisit for 4.1.  */
  if (!diag_musttail || !gimple_call_must_tail_p (call))
    for (param = DECL_ARGUMENTS (current_function_decl);
	 param; param = DECL_CHAIN (param))
      if (TREE_ADDRESSABLE (param))
	{
	  maybe_error_musttail (call, _("address of caller arguments taken"),
				diag_musttail);
	  /* If current function has musttail calls, we can't disable tail
	     calls altogether for the whole caller, because those might be
	     actually fine.  So just punt if this exact call is not
	     a tail recursion.  */
	  if (cfun->has_musttail)
	    only_tailr = true;
	  else
	    opt_tailcalls = false;
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
    {
      maybe_error_musttail (call, _("return value in memory"), diag_musttail);
      return;
    }

  if (cfun->calls_setjmp)
    {
      maybe_error_musttail (call, _("caller uses setjmp"), diag_musttail);
      return;
    }

  /* If the call might throw an exception that wouldn't propagate out of
     cfun, we can't transform to a tail or sibling call (82081).  */
  if ((stmt_could_throw_p (cfun, stmt)
       && !stmt_can_throw_external (cfun, stmt)) || EDGE_COUNT (bb->succs) > 1)
  {
    if (stmt != last_stmt)
      {
	maybe_error_musttail (call, _("code between call and return"),
			      diag_musttail);
	return;
      }

    edge e;
    edge_iterator ei;
    FOR_EACH_EDGE (e, ei, bb->succs)
      if (e->flags & EDGE_EH)
	break;

    if (!e)
      {
	maybe_error_musttail (call, _("call may throw exception that does not "
				      "propagate"), diag_musttail);
	return;
      }

    if (diag_musttail && gimple_call_must_tail_p (call))
      eh_has_tsan_func_exit = 0;
    if (!gimple_call_must_tail_p (call)
	|| !empty_eh_cleanup (e->dest,
			      eh_has_tsan_func_exit
			      ? NULL : &eh_has_tsan_func_exit, 20)
	|| EDGE_COUNT (bb->succs) > 2)
      {
	maybe_error_musttail (call, _("call may throw exception caught "
				      "locally or perform cleanups"),
			      diag_musttail);
	return;
      }
  }

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
    {
      maybe_error_musttail (call, _("return value used after call"),
			    diag_musttail);
      return;
    }

  /* We found the call, check whether it is suitable.  */
  tail_recursion = false;
  func = gimple_call_fndecl (call);
  if (func
      && !fndecl_built_in_p (func)
      && recursive_call_p (current_function_decl, func)
      && !only_musttail)
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
	      if (TREE_ADDRESSABLE (TREE_TYPE (param))
		  || !useless_type_conversion_p (TREE_TYPE (param),
					         TREE_TYPE (arg)))
		break;

	      if (is_gimple_reg_type (TREE_TYPE (param))
		  ? !is_gimple_reg (param)
		  : (!is_gimple_variable (param)
		     || TREE_THIS_VOLATILE (param)
		     || may_be_aliased (param)))
		break;
	    }
	}
      if (idx == gimple_call_num_args (call) && !param)
	tail_recursion = true;
    }

  if (only_tailr && !tail_recursion)
    return;

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
	      if (diag_musttail && gimple_call_must_tail_p (call))
		{
		  auto opt = OPT_Wmaybe_musttail_local_addr;
		  if (!warning_suppressed_p (call,
					     opt))
		    {
		      warning_at (gimple_location (call), opt,
				  "address of local variable can escape to "
				  "%<musttail%> call");
		      suppress_warning (call, opt);
		    }
		  continue;
		}
	      if (local_live_vars)
		BITMAP_FREE (local_live_vars);
	      maybe_error_musttail (call, _("call invocation refers to "
					    "locals"), diag_musttail);
	      return;
	    }
	  else
	    {
	      unsigned int *v = live_vars->get (DECL_UID (var));
	      if (bitmap_bit_p (local_live_vars, *v))
		{
		  if (diag_musttail && gimple_call_must_tail_p (call))
		    {
		      auto opt = OPT_Wmaybe_musttail_local_addr;
		      if (!warning_suppressed_p (call, opt))
			{
			  if (!DECL_ARTIFICIAL (var) && DECL_NAME (var))
			    warning_at (gimple_location (call), opt,
					"address of automatic variable %qD "
					"can escape to %<musttail%> call",
					var);
			  else
			    warning_at (gimple_location (call), opt,
					"address of local variable can escape "
					"to %<musttail%> call");
			  suppress_warning (call, opt);
			}
		      continue;
		    }
		  BITMAP_FREE (local_live_vars);
		  maybe_error_musttail (call, _("call invocation refers to "
						"locals"), diag_musttail);
		  return;
		}
	    }
	}
    }
  if (diag_musttail
      && gimple_call_must_tail_p (call)
      && !warning_suppressed_p (call, OPT_Wmaybe_musttail_local_addr))
    for (tree param = DECL_ARGUMENTS (current_function_decl);
	 param; param = DECL_CHAIN (param))
      if (may_be_aliased (param)
	  && (ref_maybe_used_by_stmt_p (call, param, false)
	      || call_may_clobber_ref_p (call, param, false)))
	{
	  auto opt = OPT_Wmaybe_musttail_local_addr;
	  warning_at (gimple_location (call), opt,
		      "address of parameter %qD can escape to "
		      "%<musttail%> call", param);
	  suppress_warning (call, opt);
	  break;
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
  bool is_noreturn = gimple_call_noreturn_p (call);
  auto_vec<edge> edges;

  abb = bb;
  agsi = gsi;
  while (!is_noreturn)
    {
      tree tmp_a = NULL_TREE;
      tree tmp_m = NULL_TREE;
      gsi_next (&agsi);

      while (gsi_end_p (agsi))
	{
	  edge e = single_non_eh_succ_edge (abb);
	  ass_var = propagate_through_phis (ass_var, e);
	  if (!ass_var)
	    edges.safe_push (e);
	  abb = e->dest;
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

      if (cfun->has_musttail
	  && sanitize_flags_p (SANITIZE_THREAD)
	  && retry_tsan_func_exit == 1
	  && gimple_call_builtin_p (stmt, BUILT_IN_TSAN_FUNC_EXIT)
	  && !has_tsan_func_exit
	  && gimple_call_must_tail_p (call))
	{
	  has_tsan_func_exit = true;
	  continue;
	}

      if (gimple_code (stmt) != GIMPLE_ASSIGN)
	{
	  maybe_error_musttail (call, _("unhandled code after call"),
				diag_musttail);
	  return;
	}

      /* This is a gimple assign. */
      par ret = process_assignment (as_a <gassign *> (stmt), gsi,
				    &tmp_m, &tmp_a, &ass_var, to_move_defs);
      if (ret == FAIL || (ret == TRY_MOVE && !tail_recursion))
	{
	  maybe_error_musttail (call, _("return value changed after call"),
				diag_musttail);
	  return;
	}
      else if (ret == TRY_MOVE)
	{
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
  if (is_noreturn)
    {
      if (gimple_call_internal_p (call))
	{
	  maybe_error_musttail (call, _("internal call"), diag_musttail);
	  return;
	}
      tree rettype = TREE_TYPE (TREE_TYPE (current_function_decl));
      tree calltype = TREE_TYPE (gimple_call_fntype (call));
      if (!VOID_TYPE_P (rettype)
	  && !useless_type_conversion_p (rettype, calltype))
	{
	  maybe_error_musttail (call, _("call and return value are different"),
				diag_musttail);
	  return;
	}
      ret_var = NULL_TREE;
    }
  else
    ret_var = gimple_return_retval (as_a <greturn *> (stmt));

  /* We may proceed if there either is no return value, or the return value
     is identical to the call's return or if the return decl is an empty type
     variable and the call's return was not assigned. */
  if (ret_var
      && (ret_var != ass_var
	  && !(is_empty_type (TREE_TYPE (ret_var)) && !ass_var)))
    {
      bool ok = false;
      value_range val;
      if (ass_var == NULL_TREE && !tail_recursion)
	{
	  tree other_value = NULL_TREE;
	  /* If we have a function call that we know the return value is the same
	     as the argument, try the argument too. */
	  int flags = gimple_call_return_flags (call);
	  if ((flags & ERF_RETURNS_ARG) != 0
	      && (flags & ERF_RETURN_ARG_MASK) < gimple_call_num_args (call))
	    {
	      tree arg = gimple_call_arg (call, flags & ERF_RETURN_ARG_MASK);
	      if (useless_type_conversion_p (TREE_TYPE (ret_var), TREE_TYPE (arg) ))
		other_value = arg;
	    }
	  /* If IPA-VRP proves called function always returns a singleton range,
	     the return value is replaced by the only value in that range.
	     For tail call purposes, pretend such replacement didn't happen.  */
	  else if (tree type = gimple_range_type (call))
	    if (tree callee = gimple_call_fndecl (call))
	      {
		tree valr;
		if ((INTEGRAL_TYPE_P (type)
		     || SCALAR_FLOAT_TYPE_P (type)
		     || POINTER_TYPE_P (type))
		    && useless_type_conversion_p (TREE_TYPE (TREE_TYPE (callee)),
					      type)
		    && useless_type_conversion_p (TREE_TYPE (ret_var), type)
		    && ipa_return_value_range (val, callee)
		    && val.singleton_p (&valr))
		  other_value = valr;
	      }

	  if (other_value)
	    {
	      tree rv = ret_var;
	      unsigned int i = edges.length ();
	      /* If ret_var is equal to other_value, we can tail optimize.  */
	      if (operand_equal_p (ret_var, other_value, 0))
		ok = true;
	      else
		/* Otherwise, if ret_var is a PHI result, try to find out
		   if other_value isn't propagated through PHIs on the path from
		   call's bb to SSA_NAME_DEF_STMT (ret_var)'s bb.  */
		while (TREE_CODE (rv) == SSA_NAME
		      && gimple_code (SSA_NAME_DEF_STMT (rv)) == GIMPLE_PHI)
		  {
		    tree nrv = NULL_TREE;
		    gimple *g = SSA_NAME_DEF_STMT (rv);
		    for (; i; --i)
		      {
			if (edges[i - 1]->dest == gimple_bb (g))
			  {
			    nrv = gimple_phi_arg_def_from_edge (g,
								edges[i - 1]);
			    --i;
			    break;
			  }
		      }
		    if (nrv == NULL_TREE)
		      break;
		    if (operand_equal_p (nrv, other_value, 0))
		      {
			ok = true;
			break;
		      }
		      rv = nrv;
		  }
	  }
	}
      if (!ok)
	{
	  maybe_error_musttail (call, _("call and return value are different"),
				diag_musttail);
	  return;
	}
    }

  /* If this is not a tail recursive call, we cannot handle addends or
     multiplicands.  */
  if (!tail_recursion && (m || a))
    {
      maybe_error_musttail (call, _("operations after non tail recursive "
				    "call"), diag_musttail);
      return;
    }

  /* For pointers only allow additions.  */
  if (m && POINTER_TYPE_P (TREE_TYPE (DECL_RESULT (current_function_decl))))
    {
      maybe_error_musttail (call, _("tail recursion with pointers can only "
				    "use additions"), diag_musttail);
      return;
    }

  if (eh_has_tsan_func_exit != -1
      && eh_has_tsan_func_exit != has_tsan_func_exit)
    {
      if (eh_has_tsan_func_exit)
	maybe_error_musttail (call, _("call may throw exception caught "
				      "locally or perform cleanups"),
			      diag_musttail);
      else
	maybe_error_musttail (call, _("exception cleanups omit "
				      "__tsan_func_exit call"), diag_musttail);
      return;
    }

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
	  if (!is_gimple_reg (param)
	      || ((ddef = ssa_default_def (cfun, param))
		  && arg != ddef))
	    bitmap_set_bit (tailr_arg_needs_copy, idx);
	}
    }

  nw = XNEW (struct tailcall);

  nw->call_gsi = gsi;

  nw->tail_recursion = tail_recursion;
  nw->has_tsan_func_exit = has_tsan_func_exit;

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

  if (gimple_call_noreturn_p (as_a <gcall *> (stmt)))
    {
      e = make_edge (gsi_bb (t->call_gsi), first, EDGE_FALLTHRU);
      e->probability = profile_probability::always ();
    }
  else
    {
      /* Number of executions of function has reduced by the tailcall.  */
      e = single_non_eh_succ_edge (gsi_bb (t->call_gsi));

      profile_count count = e->count ();

      /* When profile is inconsistent and the recursion edge is more frequent
	 than number of executions of functions, scale it down, so we do not
	 end up with 0 executions of entry block.  */
      if (count >= ENTRY_BLOCK_PTR_FOR_FN (cfun)->count)
	count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count.apply_scale (7, 8);
      decrease_profile (EXIT_BLOCK_PTR_FOR_FN (cfun), count);
      decrease_profile (ENTRY_BLOCK_PTR_FOR_FN (cfun), count);
      if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
	decrease_profile (e->dest, count);

      /* Replace the call by a jump to the start of function.  */
      e = redirect_edge_and_branch (e, first);
    }
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
  auto_vec<tree> copies;
  for (param = DECL_ARGUMENTS (current_function_decl),
	 idx = 0, gpi = gsi_start_phis (first);
       param;
       param = DECL_CHAIN (param), idx++)
    {
      if (!bitmap_bit_p (tailr_arg_needs_copy, idx))
	continue;

      if (!is_gimple_reg_type (TREE_TYPE (param)))
	{
	  if (param == gimple_call_arg (stmt, idx))
	    continue;
	  /* First check if param isn't used by any of the following
	     call arguments.  If it is, we need to copy first to
	     a temporary and only after doing all the assignments copy it
	     to param.  */
	  size_t idx2 = idx + 1;
	  tree param2 = DECL_CHAIN (param);
	  for (; param2; param2 = DECL_CHAIN (param2), idx2++)
	    if (!is_gimple_reg_type (TREE_TYPE (param)))
	      {
		tree base = get_base_address (gimple_call_arg (stmt, idx2));
		if (base == param)
		  break;
	      }
	  tree tmp = param;
	  if (param2)
	    {
	      tmp = create_tmp_var (TREE_TYPE (param));
	      copies.safe_push (param);
	      copies.safe_push (tmp);
	    }
	  gimple *g = gimple_build_assign (tmp, gimple_call_arg (stmt, idx));
	  gsi_insert_before (&t->call_gsi, g, GSI_SAME_STMT);
	  continue;
	}

      arg = gimple_call_arg (stmt, idx);
      phi = gpi.phi ();
      gcc_assert (param == SSA_NAME_VAR (PHI_RESULT (phi)));

      add_phi_arg (phi, arg, e, gimple_location (stmt));
      gsi_next (&gpi);
    }
  for (unsigned i = 0; i < copies.length (); i += 2)
    {
      gimple *g = gimple_build_assign (copies[i], copies[i + 1]);
      gsi_insert_before (&t->call_gsi, g, GSI_SAME_STMT);
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
  if (t->has_tsan_func_exit && (t->tail_recursion || opt_tailcalls))
    {
      tree builtin_decl = builtin_decl_implicit (BUILT_IN_TSAN_FUNC_EXIT);
      gimple *g = gimple_build_call (builtin_decl, 0);
      gimple_set_location (g, cfun->function_end_locus);
      gsi_insert_before (&t->call_gsi, g, GSI_SAME_STMT);
    }

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
      return t->has_tsan_func_exit;
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
   into iteration.  When ONLY_MUSTTAIL is true only optimize musttail
   marked calls.  When DIAG_MUSTTAIL, diagnose if musttail calls can't
   be tail call optimized.  */

static unsigned int
tree_optimize_tail_calls_1 (bool opt_tailcalls, bool only_musttail,
			    bool diag_musttail)
{
  edge e;
  bool phis_constructed = false;
  struct tailcall *tailcalls = NULL, *act, *next;
  bool changed = false;
  basic_block first = single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  tree param;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    {
      /* Only traverse the normal exits, i.e. those that end with return
	 statement.  */
      if (safe_is_a <greturn *> (*gsi_last_bb (e->src)))
	{
	  int retry_tsan_func_exit = 0;
	  find_tail_calls (e->src, &tailcalls, only_musttail, opt_tailcalls,
			   diag_musttail, retry_tsan_func_exit);
	  if (retry_tsan_func_exit == -1)
	    {
	      retry_tsan_func_exit = 1;
	      find_tail_calls (e->src, &tailcalls, only_musttail,
			       opt_tailcalls, diag_musttail,
			       retry_tsan_func_exit);
	    }
	}
    }
  if (cfun->has_musttail && diag_musttail)
    {
      basic_block bb;
      int retry_tsan_func_exit = 0;
      FOR_EACH_BB_FN (bb, cfun)
	if (EDGE_COUNT (bb->succs) == 0
	    || (single_succ_p (bb)
		&& (single_succ_edge (bb)->flags & EDGE_EH)))
	  if (gimple *c = last_nondebug_stmt (bb))
	    if (is_gimple_call (c)
		&& gimple_call_must_tail_p (as_a <gcall *> (c))
		&& gimple_call_noreturn_p (as_a <gcall *> (c)))
	      find_tail_calls (bb, &tailcalls, only_musttail, opt_tailcalls,
			       diag_musttail, retry_tsan_func_exit);
    }

  if (live_vars)
    {
      destroy_live_vars (live_vars_vec);
      delete live_vars;
      live_vars = NULL;
    }

  if (cfun->has_musttail)
    {
      /* We can't mix non-recursive must tail calls with tail recursive
	 calls which require accumulators, because in that case we have to
	 emit code in between the musttail calls and return, which prevent
	 calling them as tail calls.  So, in that case give up on the
	 tail recursion.  */
      for (act = tailcalls; act; act = act->next)
	if (!act->tail_recursion)
	  {
	    gcall *call = as_a <gcall *> (gsi_stmt (act->call_gsi));
	    if (gimple_call_must_tail_p (call))
	      break;
	  }
      if (act)
	for (struct tailcall **p = &tailcalls; *p; )
	  {
	    if ((*p)->tail_recursion && ((*p)->add || (*p)->mult))
	      {
		struct tailcall *a = *p;
		*p = (*p)->next;
		gcall *call = as_a <gcall *> (gsi_stmt (a->call_gsi));
		maybe_error_musttail (call, _("tail recursion with "
					      "accumulation mixed with "
					      "musttail non-recursive call"),
				      diag_musttail);
		free (a);
	      }
	    else
	      p = &(*p)->next;
	  }
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
	    first
	      = split_edge (single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun)));

	  /* Copy the args if needed.  */
	  unsigned idx;
	  for (param = DECL_ARGUMENTS (current_function_decl), idx = 0;
	       param;
	       param = DECL_CHAIN (param), idx++)
	    if (bitmap_bit_p (tailr_arg_needs_copy, idx))
	      {
		if (!is_gimple_reg_type (TREE_TYPE (param)))
		  continue;
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
	  if (safe_is_a <greturn *> (*gsi_last_bb (e->src)))
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

  if (diag_musttail)
    cfun->has_musttail = false;

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
  return tree_optimize_tail_calls_1 (true, false, true);
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
  opt_pass * clone () final override
  {
    return new pass_tail_recursion (m_ctxt);
  }
  bool gate (function *) final override { return gate_tail_calls (); }
  unsigned int execute (function *) final override
    {
      return tree_optimize_tail_calls_1 (false, false, false);
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
  bool gate (function *) final override { return gate_tail_calls (); }
  unsigned int execute (function *) final override
  {
    return execute_tail_calls ();
  }

}; // class pass_tail_calls

} // anon namespace

gimple_opt_pass *
make_pass_tail_calls (gcc::context *ctxt)
{
  return new pass_tail_calls (ctxt);
}

namespace {

const pass_data pass_data_musttail =
{
  GIMPLE_PASS, /* type */
  "musttail", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_musttail : public gimple_opt_pass
{
public:
  pass_musttail (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_musttail, ctxt)
  {}

  /* opt_pass methods: */
  /* This pass is only used when the other tail call pass
     doesn't run to make [[musttail]] still work.  But only
     run it when there is actually a musttail in the function.  */
  bool gate (function *f) final override
  {
    return f->has_musttail;
  }
  unsigned int execute (function *) final override
  {
    return tree_optimize_tail_calls_1 (true, true, true);
  }

}; // class pass_musttail

} // anon namespace

gimple_opt_pass *
make_pass_musttail (gcc::context *ctxt)
{
  return new pass_musttail (ctxt);
}
