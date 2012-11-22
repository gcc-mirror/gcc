/* GCC instrumentation plugin for ThreadSanitizer.
   Copyright (C) 2011, 2012 Free Software Foundation, Inc.
   Contributed by Dmitry Vyukov <dvyukov@google.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "intl.h"
#include "tm.h"
#include "basic-block.h"
#include "gimple.h"
#include "function.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-iterator.h"
#include "langhooks.h"
#include "output.h"
#include "options.h"
#include "target.h"
#include "cgraph.h"
#include "diagnostic.h"

/* Number of instrumented memory accesses in the current function.  */

/* Builds the following decl
   void __tsan_read/writeX (void *addr);  */

static tree
get_memory_access_decl (bool is_write, unsigned size)
{
  enum built_in_function fcode;

  if (size <= 1)
    fcode = is_write ? BUILT_IN_TSAN_WRITE_1
		     : BUILT_IN_TSAN_READ_1;
  else if (size <= 3)
    fcode = is_write ? BUILT_IN_TSAN_WRITE_2
		     : BUILT_IN_TSAN_READ_2;
  else if (size <= 7)
    fcode = is_write ? BUILT_IN_TSAN_WRITE_4
		     : BUILT_IN_TSAN_READ_4;
  else if (size <= 15)
    fcode = is_write ? BUILT_IN_TSAN_WRITE_8
		     : BUILT_IN_TSAN_READ_8;
  else
    fcode = is_write ? BUILT_IN_TSAN_WRITE_16
		     : BUILT_IN_TSAN_READ_16;

  return builtin_decl_implicit (fcode);
}

/* Check as to whether EXPR refers to a store to vptr.  */

static tree
is_vptr_store (gimple stmt, tree expr, bool is_write)
{
  if (is_write == true
      && gimple_assign_single_p (stmt)
      && TREE_CODE (expr) == COMPONENT_REF)
    {
      tree field = TREE_OPERAND (expr, 1);
      if (TREE_CODE (field) == FIELD_DECL
	  && DECL_VIRTUAL_P (field))
	return gimple_assign_rhs1 (stmt);
    }
  return NULL;
}

/* Checks as to whether EXPR refers to constant var/field/param.
   Don't bother to instrument them.  */

static bool
is_load_of_const_p (tree expr, bool is_write)
{
  if (is_write)
    return false;
  if (TREE_CODE (expr) == COMPONENT_REF)
    expr = TREE_OPERAND (expr, 1);
  if (TREE_CODE (expr) == VAR_DECL
      || TREE_CODE (expr) == PARM_DECL
      || TREE_CODE (expr) == FIELD_DECL)
    {
      if (TREE_READONLY (expr))
	return true;
    }
  return false;
}

/* Instruments EXPR if needed. If any instrumentation is inserted,
 * return true. */

static bool
instrument_expr (gimple_stmt_iterator gsi, tree expr, bool is_write)
{
  enum tree_code tcode;
  tree base, rhs, expr_type, expr_ptr, builtin_decl;
  basic_block bb;
  HOST_WIDE_INT size;
  gimple stmt, g;
  location_t loc;

  base = get_base_address (expr);
  if (base == NULL_TREE
      || TREE_CODE (base) == SSA_NAME
      || TREE_CODE (base) == STRING_CST)
    return false;

  tcode = TREE_CODE (expr);

  /* Below are things we do not instrument
     (no possibility of races or not implemented yet).  */
  if (/* Compiler-emitted artificial variables.  */
      (DECL_P (expr) && DECL_ARTIFICIAL (expr))
      /* The var does not live in memory -> no possibility of races.  */
      || (tcode == VAR_DECL
	  && !TREE_ADDRESSABLE (expr)
	  && TREE_STATIC (expr) == 0)
      /* Not implemented.  */
      || TREE_CODE (TREE_TYPE (expr)) == RECORD_TYPE
      /* Not implemented.  */
      || tcode == CONSTRUCTOR
      /* Not implemented.  */
      || tcode == PARM_DECL
      /* Load of a const variable/parameter/field.  */
      || is_load_of_const_p (expr, is_write))
    return false;

  size = int_size_in_bytes (TREE_TYPE (expr));
  if (size == -1)
    return false;

  /* For now just avoid instrumenting bit field acceses.
     TODO: handle bit-fields as if touching the whole field.  */
  HOST_WIDE_INT bitsize, bitpos;
  tree offset;
  enum machine_mode mode;
  int volatilep = 0, unsignedp = 0;
  get_inner_reference (expr, &bitsize, &bitpos, &offset,
		       &mode, &unsignedp, &volatilep, false);
  if (bitpos % (size * BITS_PER_UNIT)
      || bitsize != size * BITS_PER_UNIT)
    return false;

  /* TODO: handle other case: ARRAY_RANGE_REF. */
  if (tcode != ARRAY_REF
      && tcode != VAR_DECL
      && tcode != COMPONENT_REF
      && tcode != INDIRECT_REF
      && tcode != MEM_REF)
    return false;

  stmt = gsi_stmt (gsi);
  loc = gimple_location (stmt);
  rhs = is_vptr_store (stmt, expr, is_write);
  gcc_checking_assert (rhs != NULL || is_gimple_addressable (expr));
  expr_ptr = build_fold_addr_expr (unshare_expr (expr));
  if (rhs == NULL)
    {
      expr_type = TREE_TYPE (expr);
      while (TREE_CODE (expr_type) == ARRAY_TYPE)
	expr_type = TREE_TYPE (expr_type);
      size = int_size_in_bytes (expr_type);
      g = gimple_build_call (get_memory_access_decl (is_write, size),
			     1, expr_ptr);
    }
  else
    {
      builtin_decl = builtin_decl_implicit (BUILT_IN_TSAN_VPTR_UPDATE);
      g = gimple_build_call (builtin_decl, 1, expr_ptr);
    }
  gimple_set_location (g, loc);
  /* Instrumentation for assignment of a function result
     must be inserted after the call.  Instrumentation for
     reads of function arguments must be inserted before the call.
     That's because the call can contain synchronization.  */
  if (is_gimple_call (stmt) && is_write)
    {
      /* If the call can throw, it must be the last stmt in
	 a basic block, so the instrumented stmts need to be
	 inserted in successor bbs. */
      if (is_ctrl_altering_stmt (stmt))
	{
	  edge e;

	  bb = gsi_bb (gsi);
	  e = find_fallthru_edge (bb->succs);
	  if (e)
	    gsi_insert_seq_on_edge_immediate (e, g);
	}
      else
	gsi_insert_after (&gsi, g, GSI_NEW_STMT);
    }
  else
    gsi_insert_before (&gsi, g, GSI_SAME_STMT);

  return true;
}

/* Instruments the gimple pointed to by GSI. Return
 * true if func entry/exit should be instrumented. */

static bool
instrument_gimple (gimple_stmt_iterator gsi)
{
  gimple stmt;
  tree rhs, lhs;
  bool instrumented = false;

  stmt = gsi_stmt (gsi);
  if (is_gimple_call (stmt)
      && (gimple_call_fndecl (stmt)
	  != builtin_decl_implicit (BUILT_IN_TSAN_INIT)))
    return true;
  else if (is_gimple_assign (stmt))
    {
      if (gimple_store_p (stmt))
	{
	  lhs = gimple_assign_lhs (stmt);
	  instrumented = instrument_expr (gsi, lhs, true);
	}
      if (gimple_assign_load_p (stmt))
	{
	  rhs = gimple_assign_rhs1 (stmt);
	  instrumented = instrument_expr (gsi, rhs, false);
	}
    }
  return instrumented;
}

/* Instruments all interesting memory accesses in the current function.
 * Return true if func entry/exit should be instrumented. */

static bool
instrument_memory_accesses (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  bool fentry_exit_instrument = false;

  FOR_EACH_BB (bb)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      fentry_exit_instrument |= instrument_gimple (gsi);
  return fentry_exit_instrument;
}

/* Instruments function entry.  */

static void
instrument_func_entry (void)
{
  basic_block succ_bb;
  gimple_stmt_iterator gsi;
  tree ret_addr, builtin_decl;
  gimple g;

  succ_bb = single_succ (ENTRY_BLOCK_PTR);
  gsi = gsi_after_labels (succ_bb);

  builtin_decl = builtin_decl_implicit (BUILT_IN_RETURN_ADDRESS);
  g = gimple_build_call (builtin_decl, 1, integer_zero_node);
  ret_addr = make_ssa_name (ptr_type_node, NULL);
  gimple_call_set_lhs (g, ret_addr);
  gimple_set_location (g, cfun->function_start_locus);
  gsi_insert_before (&gsi, g, GSI_SAME_STMT);

  builtin_decl =  builtin_decl_implicit (BUILT_IN_TSAN_FUNC_ENTRY);
  g = gimple_build_call (builtin_decl, 1, ret_addr);
  gimple_set_location (g, cfun->function_start_locus);
  gsi_insert_before (&gsi, g, GSI_SAME_STMT);
}

/* Instruments function exits.  */

static void
instrument_func_exit (void)
{
  location_t loc;
  basic_block exit_bb;
  gimple_stmt_iterator gsi;
  gimple stmt, g;
  tree builtin_decl;
  edge e;
  edge_iterator ei;

  /* Find all function exits.  */
  exit_bb = EXIT_BLOCK_PTR;
  FOR_EACH_EDGE (e, ei, exit_bb->preds)
    {
      gsi = gsi_last_bb (e->src);
      stmt = gsi_stmt (gsi);
      gcc_assert (gimple_code (stmt) == GIMPLE_RETURN);
      loc = gimple_location (stmt);
      builtin_decl = builtin_decl_implicit (BUILT_IN_TSAN_FUNC_EXIT);
      g = gimple_build_call (builtin_decl, 0);
      gimple_set_location (g, loc);
      gsi_insert_before (&gsi, g, GSI_SAME_STMT);
    }
}

/* ThreadSanitizer instrumentation pass.  */

static unsigned
tsan_pass (void)
{
  if (instrument_memory_accesses ())
    {
      instrument_func_entry ();
      instrument_func_exit ();
    }
  return 0;
}

/* The pass's gate.  */

static bool
tsan_gate (void)
{
  return flag_tsan != 0
	 && builtin_decl_implicit_p (BUILT_IN_TSAN_INIT);
}

/* Inserts __tsan_init () into the list of CTORs.  */

void
tsan_finish_file (void)
{
  tree ctor_statements;
  tree init_decl;

  ctor_statements = NULL_TREE;
  init_decl = builtin_decl_implicit (BUILT_IN_TSAN_INIT);
  append_to_statement_list (build_call_expr (init_decl, 0),
			    &ctor_statements);
  cgraph_build_static_cdtor ('I', ctor_statements,
			     MAX_RESERVED_INIT_PRIORITY - 1);
}

/* The pass descriptor.  */

struct gimple_opt_pass pass_tsan =
{
 {
  GIMPLE_PASS,
  "tsan",				/* name  */
  OPTGROUP_NONE,			/* optinfo_flags */
  tsan_gate,				/* gate  */
  tsan_pass,				/* execute  */
  NULL,					/* sub  */
  NULL,					/* next  */
  0,					/* static_pass_number  */
  TV_NONE,				/* tv_id  */
  PROP_ssa | PROP_cfg,			/* properties_required  */
  0,					/* properties_provided  */
  0,					/* properties_destroyed  */
  0,					/* todo_flags_start  */
  TODO_verify_all | TODO_update_ssa
  | TODO_update_address_taken		/* todo_flags_finish  */
 }
};

static bool
tsan_gate_O0 (void)
{
  return flag_tsan != 0 && !optimize
	 && builtin_decl_implicit_p (BUILT_IN_TSAN_INIT);
}

struct gimple_opt_pass pass_tsan_O0 =
{
 {
  GIMPLE_PASS,
  "tsan0",				/* name  */
  OPTGROUP_NONE,			/* optinfo_flags */
  tsan_gate_O0,				/* gate  */
  tsan_pass,				/* execute  */
  NULL,					/* sub  */
  NULL,					/* next  */
  0,					/* static_pass_number  */
  TV_NONE,				/* tv_id  */
  PROP_ssa | PROP_cfg,			/* properties_required  */
  0,					/* properties_provided  */
  0,					/* properties_destroyed  */
  0,					/* todo_flags_start  */
  TODO_verify_all | TODO_update_ssa
  | TODO_update_address_taken		/* todo_flags_finish  */
 }
};
