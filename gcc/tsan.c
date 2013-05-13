/* GCC instrumentation plugin for ThreadSanitizer.
   Copyright (C) 2011-2013 Free Software Foundation, Inc.
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
#include "tree-ssa-propagate.h"
#include "tsan.h"
#include "asan.h"

/* Number of instrumented memory accesses in the current function.  */

/* Builds the following decl
   void __tsan_read/writeX (void *addr);  */

static tree
get_memory_access_decl (bool is_write, unsigned size)
{
  enum built_in_function fcode;

  if (size <= 1)
    fcode = is_write ? BUILT_IN_TSAN_WRITE1
		     : BUILT_IN_TSAN_READ1;
  else if (size <= 3)
    fcode = is_write ? BUILT_IN_TSAN_WRITE2
		     : BUILT_IN_TSAN_READ2;
  else if (size <= 7)
    fcode = is_write ? BUILT_IN_TSAN_WRITE4
		     : BUILT_IN_TSAN_READ4;
  else if (size <= 15)
    fcode = is_write ? BUILT_IN_TSAN_WRITE8
		     : BUILT_IN_TSAN_READ8;
  else
    fcode = is_write ? BUILT_IN_TSAN_WRITE16
		     : BUILT_IN_TSAN_READ16;

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

/* Instruments EXPR if needed. If any instrumentation is inserted,
   return true.  */

static bool
instrument_expr (gimple_stmt_iterator gsi, tree expr, bool is_write)
{
  tree base, rhs, expr_ptr, builtin_decl;
  basic_block bb;
  HOST_WIDE_INT size;
  gimple stmt, g;
  gimple_seq seq;
  location_t loc;

  size = int_size_in_bytes (TREE_TYPE (expr));
  if (size == -1)
    return false;

  /* For now just avoid instrumenting bit field acceses.
     TODO: handle bit-fields as if touching the whole field.  */
  HOST_WIDE_INT bitsize, bitpos;
  tree offset;
  enum machine_mode mode;
  int volatilep = 0, unsignedp = 0;
  base = get_inner_reference (expr, &bitsize, &bitpos, &offset,
			      &mode, &unsignedp, &volatilep, false);

  /* No need to instrument accesses to decls that don't escape,
     they can't escape to other threads then.  */
  if (DECL_P (base))
    {
      struct pt_solution pt;
      memset (&pt, 0, sizeof (pt));
      pt.escaped = 1;
      pt.ipa_escaped = flag_ipa_pta != 0;
      pt.nonlocal = 1;
      if (!pt_solution_includes (&pt, base))
	return false;
      if (!is_global_var (base) && !may_be_aliased (base))
	return false;
    }

  if (TREE_READONLY (base)
      || (TREE_CODE (base) == VAR_DECL
	  && DECL_HARD_REGISTER (base)))
    return false;

  if (size == 0
      || bitpos % (size * BITS_PER_UNIT)
      || bitsize != size * BITS_PER_UNIT)
    return false;

  stmt = gsi_stmt (gsi);
  loc = gimple_location (stmt);
  rhs = is_vptr_store (stmt, expr, is_write);
  gcc_checking_assert (rhs != NULL || is_gimple_addressable (expr));
  expr_ptr = build_fold_addr_expr (unshare_expr (expr));
  seq = NULL;
  if (!is_gimple_val (expr_ptr))
    {
      g = gimple_build_assign (make_ssa_name (TREE_TYPE (expr_ptr), NULL),
			       expr_ptr);
      expr_ptr = gimple_assign_lhs (g);
      gimple_set_location (g, loc);
      gimple_seq_add_stmt_without_update (&seq, g);
    }
  if (rhs == NULL)
    g = gimple_build_call (get_memory_access_decl (is_write, size),
			   1, expr_ptr);
  else
    {
      builtin_decl = builtin_decl_implicit (BUILT_IN_TSAN_VPTR_UPDATE);
      g = gimple_build_call (builtin_decl, 1, expr_ptr);
    }
  gimple_set_location (g, loc);
  gimple_seq_add_stmt_without_update (&seq, g);
  /* Instrumentation for assignment of a function result
     must be inserted after the call.  Instrumentation for
     reads of function arguments must be inserted before the call.
     That's because the call can contain synchronization.  */
  if (is_gimple_call (stmt) && is_write)
    {
      /* If the call can throw, it must be the last stmt in
	 a basic block, so the instrumented stmts need to be
	 inserted in successor bbs.  */
      if (is_ctrl_altering_stmt (stmt))
	{
	  edge e;

	  bb = gsi_bb (gsi);
	  e = find_fallthru_edge (bb->succs);
	  if (e)
	    gsi_insert_seq_on_edge_immediate (e, seq);
	}
      else
	gsi_insert_seq_after (&gsi, seq, GSI_NEW_STMT);
    }
  else
    gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);

  return true;
}

/* Actions for sync/atomic builtin transformations.  */
enum tsan_atomic_action
{
  check_last, add_seq_cst, add_acquire, weak_cas, strong_cas,
  bool_cas, val_cas, lock_release, fetch_op, fetch_op_seq_cst
};

/* Table how to map sync/atomic builtins to their corresponding
   tsan equivalents.  */
static struct tsan_map_atomic
{
  enum built_in_function fcode, tsan_fcode;
  enum tsan_atomic_action action;
  enum tree_code code;
} tsan_atomic_table[] =
{
#define TRANSFORM(fcode, tsan_fcode, action, code) \
  { BUILT_IN_##fcode, BUILT_IN_##tsan_fcode, action, code }
#define CHECK_LAST(fcode, tsan_fcode) \
  TRANSFORM (fcode, tsan_fcode, check_last, ERROR_MARK)
#define ADD_SEQ_CST(fcode, tsan_fcode) \
  TRANSFORM (fcode, tsan_fcode, add_seq_cst, ERROR_MARK)
#define ADD_ACQUIRE(fcode, tsan_fcode) \
  TRANSFORM (fcode, tsan_fcode, add_acquire, ERROR_MARK)
#define WEAK_CAS(fcode, tsan_fcode) \
  TRANSFORM (fcode, tsan_fcode, weak_cas, ERROR_MARK)
#define STRONG_CAS(fcode, tsan_fcode) \
  TRANSFORM (fcode, tsan_fcode, strong_cas, ERROR_MARK)
#define BOOL_CAS(fcode, tsan_fcode) \
  TRANSFORM (fcode, tsan_fcode, bool_cas, ERROR_MARK)
#define VAL_CAS(fcode, tsan_fcode) \
  TRANSFORM (fcode, tsan_fcode, val_cas, ERROR_MARK)
#define LOCK_RELEASE(fcode, tsan_fcode) \
  TRANSFORM (fcode, tsan_fcode, lock_release, ERROR_MARK)
#define FETCH_OP(fcode, tsan_fcode, code) \
  TRANSFORM (fcode, tsan_fcode, fetch_op, code)
#define FETCH_OPS(fcode, tsan_fcode, code) \
  TRANSFORM (fcode, tsan_fcode, fetch_op_seq_cst, code)

  CHECK_LAST (ATOMIC_LOAD_1, TSAN_ATOMIC8_LOAD),
  CHECK_LAST (ATOMIC_LOAD_2, TSAN_ATOMIC16_LOAD),
  CHECK_LAST (ATOMIC_LOAD_4, TSAN_ATOMIC32_LOAD),
  CHECK_LAST (ATOMIC_LOAD_8, TSAN_ATOMIC64_LOAD),
  CHECK_LAST (ATOMIC_LOAD_16, TSAN_ATOMIC128_LOAD),
  CHECK_LAST (ATOMIC_STORE_1, TSAN_ATOMIC8_STORE),
  CHECK_LAST (ATOMIC_STORE_2, TSAN_ATOMIC16_STORE),
  CHECK_LAST (ATOMIC_STORE_4, TSAN_ATOMIC32_STORE),
  CHECK_LAST (ATOMIC_STORE_8, TSAN_ATOMIC64_STORE),
  CHECK_LAST (ATOMIC_STORE_16, TSAN_ATOMIC128_STORE),
  CHECK_LAST (ATOMIC_EXCHANGE_1, TSAN_ATOMIC8_EXCHANGE),
  CHECK_LAST (ATOMIC_EXCHANGE_2, TSAN_ATOMIC16_EXCHANGE),
  CHECK_LAST (ATOMIC_EXCHANGE_4, TSAN_ATOMIC32_EXCHANGE),
  CHECK_LAST (ATOMIC_EXCHANGE_8, TSAN_ATOMIC64_EXCHANGE),
  CHECK_LAST (ATOMIC_EXCHANGE_16, TSAN_ATOMIC128_EXCHANGE),
  CHECK_LAST (ATOMIC_FETCH_ADD_1, TSAN_ATOMIC8_FETCH_ADD),
  CHECK_LAST (ATOMIC_FETCH_ADD_2, TSAN_ATOMIC16_FETCH_ADD),
  CHECK_LAST (ATOMIC_FETCH_ADD_4, TSAN_ATOMIC32_FETCH_ADD),
  CHECK_LAST (ATOMIC_FETCH_ADD_8, TSAN_ATOMIC64_FETCH_ADD),
  CHECK_LAST (ATOMIC_FETCH_ADD_16, TSAN_ATOMIC128_FETCH_ADD),
  CHECK_LAST (ATOMIC_FETCH_SUB_1, TSAN_ATOMIC8_FETCH_SUB),
  CHECK_LAST (ATOMIC_FETCH_SUB_2, TSAN_ATOMIC16_FETCH_SUB),
  CHECK_LAST (ATOMIC_FETCH_SUB_4, TSAN_ATOMIC32_FETCH_SUB),
  CHECK_LAST (ATOMIC_FETCH_SUB_8, TSAN_ATOMIC64_FETCH_SUB),
  CHECK_LAST (ATOMIC_FETCH_SUB_16, TSAN_ATOMIC128_FETCH_SUB),
  CHECK_LAST (ATOMIC_FETCH_AND_1, TSAN_ATOMIC8_FETCH_AND),
  CHECK_LAST (ATOMIC_FETCH_AND_2, TSAN_ATOMIC16_FETCH_AND),
  CHECK_LAST (ATOMIC_FETCH_AND_4, TSAN_ATOMIC32_FETCH_AND),
  CHECK_LAST (ATOMIC_FETCH_AND_8, TSAN_ATOMIC64_FETCH_AND),
  CHECK_LAST (ATOMIC_FETCH_AND_16, TSAN_ATOMIC128_FETCH_AND),
  CHECK_LAST (ATOMIC_FETCH_OR_1, TSAN_ATOMIC8_FETCH_OR),
  CHECK_LAST (ATOMIC_FETCH_OR_2, TSAN_ATOMIC16_FETCH_OR),
  CHECK_LAST (ATOMIC_FETCH_OR_4, TSAN_ATOMIC32_FETCH_OR),
  CHECK_LAST (ATOMIC_FETCH_OR_8, TSAN_ATOMIC64_FETCH_OR),
  CHECK_LAST (ATOMIC_FETCH_OR_16, TSAN_ATOMIC128_FETCH_OR),
  CHECK_LAST (ATOMIC_FETCH_XOR_1, TSAN_ATOMIC8_FETCH_XOR),
  CHECK_LAST (ATOMIC_FETCH_XOR_2, TSAN_ATOMIC16_FETCH_XOR),
  CHECK_LAST (ATOMIC_FETCH_XOR_4, TSAN_ATOMIC32_FETCH_XOR),
  CHECK_LAST (ATOMIC_FETCH_XOR_8, TSAN_ATOMIC64_FETCH_XOR),
  CHECK_LAST (ATOMIC_FETCH_XOR_16, TSAN_ATOMIC128_FETCH_XOR),
  CHECK_LAST (ATOMIC_FETCH_NAND_1, TSAN_ATOMIC8_FETCH_NAND),
  CHECK_LAST (ATOMIC_FETCH_NAND_2, TSAN_ATOMIC16_FETCH_NAND),
  CHECK_LAST (ATOMIC_FETCH_NAND_4, TSAN_ATOMIC32_FETCH_NAND),
  CHECK_LAST (ATOMIC_FETCH_NAND_8, TSAN_ATOMIC64_FETCH_NAND),
  CHECK_LAST (ATOMIC_FETCH_NAND_16, TSAN_ATOMIC128_FETCH_NAND),

  CHECK_LAST (ATOMIC_THREAD_FENCE, TSAN_ATOMIC_THREAD_FENCE),
  CHECK_LAST (ATOMIC_SIGNAL_FENCE, TSAN_ATOMIC_SIGNAL_FENCE),

  FETCH_OP (ATOMIC_ADD_FETCH_1, TSAN_ATOMIC8_FETCH_ADD, PLUS_EXPR),
  FETCH_OP (ATOMIC_ADD_FETCH_2, TSAN_ATOMIC16_FETCH_ADD, PLUS_EXPR),
  FETCH_OP (ATOMIC_ADD_FETCH_4, TSAN_ATOMIC32_FETCH_ADD, PLUS_EXPR),
  FETCH_OP (ATOMIC_ADD_FETCH_8, TSAN_ATOMIC64_FETCH_ADD, PLUS_EXPR),
  FETCH_OP (ATOMIC_ADD_FETCH_16, TSAN_ATOMIC128_FETCH_ADD, PLUS_EXPR),
  FETCH_OP (ATOMIC_SUB_FETCH_1, TSAN_ATOMIC8_FETCH_SUB, MINUS_EXPR),
  FETCH_OP (ATOMIC_SUB_FETCH_2, TSAN_ATOMIC16_FETCH_SUB, MINUS_EXPR),
  FETCH_OP (ATOMIC_SUB_FETCH_4, TSAN_ATOMIC32_FETCH_SUB, MINUS_EXPR),
  FETCH_OP (ATOMIC_SUB_FETCH_8, TSAN_ATOMIC64_FETCH_SUB, MINUS_EXPR),
  FETCH_OP (ATOMIC_SUB_FETCH_16, TSAN_ATOMIC128_FETCH_SUB, MINUS_EXPR),
  FETCH_OP (ATOMIC_AND_FETCH_1, TSAN_ATOMIC8_FETCH_AND, BIT_AND_EXPR),
  FETCH_OP (ATOMIC_AND_FETCH_2, TSAN_ATOMIC16_FETCH_AND, BIT_AND_EXPR),
  FETCH_OP (ATOMIC_AND_FETCH_4, TSAN_ATOMIC32_FETCH_AND, BIT_AND_EXPR),
  FETCH_OP (ATOMIC_AND_FETCH_8, TSAN_ATOMIC64_FETCH_AND, BIT_AND_EXPR),
  FETCH_OP (ATOMIC_AND_FETCH_16, TSAN_ATOMIC128_FETCH_AND, BIT_AND_EXPR),
  FETCH_OP (ATOMIC_OR_FETCH_1, TSAN_ATOMIC8_FETCH_OR, BIT_IOR_EXPR),
  FETCH_OP (ATOMIC_OR_FETCH_2, TSAN_ATOMIC16_FETCH_OR, BIT_IOR_EXPR),
  FETCH_OP (ATOMIC_OR_FETCH_4, TSAN_ATOMIC32_FETCH_OR, BIT_IOR_EXPR),
  FETCH_OP (ATOMIC_OR_FETCH_8, TSAN_ATOMIC64_FETCH_OR, BIT_IOR_EXPR),
  FETCH_OP (ATOMIC_OR_FETCH_16, TSAN_ATOMIC128_FETCH_OR, BIT_IOR_EXPR),
  FETCH_OP (ATOMIC_XOR_FETCH_1, TSAN_ATOMIC8_FETCH_XOR, BIT_XOR_EXPR),
  FETCH_OP (ATOMIC_XOR_FETCH_2, TSAN_ATOMIC16_FETCH_XOR, BIT_XOR_EXPR),
  FETCH_OP (ATOMIC_XOR_FETCH_4, TSAN_ATOMIC32_FETCH_XOR, BIT_XOR_EXPR),
  FETCH_OP (ATOMIC_XOR_FETCH_8, TSAN_ATOMIC64_FETCH_XOR, BIT_XOR_EXPR),
  FETCH_OP (ATOMIC_XOR_FETCH_16, TSAN_ATOMIC128_FETCH_XOR, BIT_XOR_EXPR),
  FETCH_OP (ATOMIC_NAND_FETCH_1, TSAN_ATOMIC8_FETCH_NAND, BIT_NOT_EXPR),
  FETCH_OP (ATOMIC_NAND_FETCH_2, TSAN_ATOMIC16_FETCH_NAND, BIT_NOT_EXPR),
  FETCH_OP (ATOMIC_NAND_FETCH_4, TSAN_ATOMIC32_FETCH_NAND, BIT_NOT_EXPR),
  FETCH_OP (ATOMIC_NAND_FETCH_8, TSAN_ATOMIC64_FETCH_NAND, BIT_NOT_EXPR),
  FETCH_OP (ATOMIC_NAND_FETCH_16, TSAN_ATOMIC128_FETCH_NAND, BIT_NOT_EXPR),

  ADD_ACQUIRE (SYNC_LOCK_TEST_AND_SET_1, TSAN_ATOMIC8_EXCHANGE),
  ADD_ACQUIRE (SYNC_LOCK_TEST_AND_SET_2, TSAN_ATOMIC16_EXCHANGE),
  ADD_ACQUIRE (SYNC_LOCK_TEST_AND_SET_4, TSAN_ATOMIC32_EXCHANGE),
  ADD_ACQUIRE (SYNC_LOCK_TEST_AND_SET_8, TSAN_ATOMIC64_EXCHANGE),
  ADD_ACQUIRE (SYNC_LOCK_TEST_AND_SET_16, TSAN_ATOMIC128_EXCHANGE),

  ADD_SEQ_CST (SYNC_FETCH_AND_ADD_1, TSAN_ATOMIC8_FETCH_ADD),
  ADD_SEQ_CST (SYNC_FETCH_AND_ADD_2, TSAN_ATOMIC16_FETCH_ADD),
  ADD_SEQ_CST (SYNC_FETCH_AND_ADD_4, TSAN_ATOMIC32_FETCH_ADD),
  ADD_SEQ_CST (SYNC_FETCH_AND_ADD_8, TSAN_ATOMIC64_FETCH_ADD),
  ADD_SEQ_CST (SYNC_FETCH_AND_ADD_16, TSAN_ATOMIC128_FETCH_ADD),
  ADD_SEQ_CST (SYNC_FETCH_AND_SUB_1, TSAN_ATOMIC8_FETCH_SUB),
  ADD_SEQ_CST (SYNC_FETCH_AND_SUB_2, TSAN_ATOMIC16_FETCH_SUB),
  ADD_SEQ_CST (SYNC_FETCH_AND_SUB_4, TSAN_ATOMIC32_FETCH_SUB),
  ADD_SEQ_CST (SYNC_FETCH_AND_SUB_8, TSAN_ATOMIC64_FETCH_SUB),
  ADD_SEQ_CST (SYNC_FETCH_AND_SUB_16, TSAN_ATOMIC128_FETCH_SUB),
  ADD_SEQ_CST (SYNC_FETCH_AND_AND_1, TSAN_ATOMIC8_FETCH_AND),
  ADD_SEQ_CST (SYNC_FETCH_AND_AND_2, TSAN_ATOMIC16_FETCH_AND),
  ADD_SEQ_CST (SYNC_FETCH_AND_AND_4, TSAN_ATOMIC32_FETCH_AND),
  ADD_SEQ_CST (SYNC_FETCH_AND_AND_8, TSAN_ATOMIC64_FETCH_AND),
  ADD_SEQ_CST (SYNC_FETCH_AND_AND_16, TSAN_ATOMIC128_FETCH_AND),
  ADD_SEQ_CST (SYNC_FETCH_AND_OR_1, TSAN_ATOMIC8_FETCH_OR),
  ADD_SEQ_CST (SYNC_FETCH_AND_OR_2, TSAN_ATOMIC16_FETCH_OR),
  ADD_SEQ_CST (SYNC_FETCH_AND_OR_4, TSAN_ATOMIC32_FETCH_OR),
  ADD_SEQ_CST (SYNC_FETCH_AND_OR_8, TSAN_ATOMIC64_FETCH_OR),
  ADD_SEQ_CST (SYNC_FETCH_AND_OR_16, TSAN_ATOMIC128_FETCH_OR),
  ADD_SEQ_CST (SYNC_FETCH_AND_XOR_1, TSAN_ATOMIC8_FETCH_XOR),
  ADD_SEQ_CST (SYNC_FETCH_AND_XOR_2, TSAN_ATOMIC16_FETCH_XOR),
  ADD_SEQ_CST (SYNC_FETCH_AND_XOR_4, TSAN_ATOMIC32_FETCH_XOR),
  ADD_SEQ_CST (SYNC_FETCH_AND_XOR_8, TSAN_ATOMIC64_FETCH_XOR),
  ADD_SEQ_CST (SYNC_FETCH_AND_XOR_16, TSAN_ATOMIC128_FETCH_XOR),
  ADD_SEQ_CST (SYNC_FETCH_AND_NAND_1, TSAN_ATOMIC8_FETCH_NAND),
  ADD_SEQ_CST (SYNC_FETCH_AND_NAND_2, TSAN_ATOMIC16_FETCH_NAND),
  ADD_SEQ_CST (SYNC_FETCH_AND_NAND_4, TSAN_ATOMIC32_FETCH_NAND),
  ADD_SEQ_CST (SYNC_FETCH_AND_NAND_8, TSAN_ATOMIC64_FETCH_NAND),
  ADD_SEQ_CST (SYNC_FETCH_AND_NAND_16, TSAN_ATOMIC128_FETCH_NAND),

  ADD_SEQ_CST (SYNC_SYNCHRONIZE, TSAN_ATOMIC_THREAD_FENCE),

  FETCH_OPS (SYNC_ADD_AND_FETCH_1, TSAN_ATOMIC8_FETCH_ADD, PLUS_EXPR),
  FETCH_OPS (SYNC_ADD_AND_FETCH_2, TSAN_ATOMIC16_FETCH_ADD, PLUS_EXPR),
  FETCH_OPS (SYNC_ADD_AND_FETCH_4, TSAN_ATOMIC32_FETCH_ADD, PLUS_EXPR),
  FETCH_OPS (SYNC_ADD_AND_FETCH_8, TSAN_ATOMIC64_FETCH_ADD, PLUS_EXPR),
  FETCH_OPS (SYNC_ADD_AND_FETCH_16, TSAN_ATOMIC128_FETCH_ADD, PLUS_EXPR),
  FETCH_OPS (SYNC_SUB_AND_FETCH_1, TSAN_ATOMIC8_FETCH_SUB, MINUS_EXPR),
  FETCH_OPS (SYNC_SUB_AND_FETCH_2, TSAN_ATOMIC16_FETCH_SUB, MINUS_EXPR),
  FETCH_OPS (SYNC_SUB_AND_FETCH_4, TSAN_ATOMIC32_FETCH_SUB, MINUS_EXPR),
  FETCH_OPS (SYNC_SUB_AND_FETCH_8, TSAN_ATOMIC64_FETCH_SUB, MINUS_EXPR),
  FETCH_OPS (SYNC_SUB_AND_FETCH_16, TSAN_ATOMIC128_FETCH_SUB, MINUS_EXPR),
  FETCH_OPS (SYNC_AND_AND_FETCH_1, TSAN_ATOMIC8_FETCH_AND, BIT_AND_EXPR),
  FETCH_OPS (SYNC_AND_AND_FETCH_2, TSAN_ATOMIC16_FETCH_AND, BIT_AND_EXPR),
  FETCH_OPS (SYNC_AND_AND_FETCH_4, TSAN_ATOMIC32_FETCH_AND, BIT_AND_EXPR),
  FETCH_OPS (SYNC_AND_AND_FETCH_8, TSAN_ATOMIC64_FETCH_AND, BIT_AND_EXPR),
  FETCH_OPS (SYNC_AND_AND_FETCH_16, TSAN_ATOMIC128_FETCH_AND, BIT_AND_EXPR),
  FETCH_OPS (SYNC_OR_AND_FETCH_1, TSAN_ATOMIC8_FETCH_OR, BIT_IOR_EXPR),
  FETCH_OPS (SYNC_OR_AND_FETCH_2, TSAN_ATOMIC16_FETCH_OR, BIT_IOR_EXPR),
  FETCH_OPS (SYNC_OR_AND_FETCH_4, TSAN_ATOMIC32_FETCH_OR, BIT_IOR_EXPR),
  FETCH_OPS (SYNC_OR_AND_FETCH_8, TSAN_ATOMIC64_FETCH_OR, BIT_IOR_EXPR),
  FETCH_OPS (SYNC_OR_AND_FETCH_16, TSAN_ATOMIC128_FETCH_OR, BIT_IOR_EXPR),
  FETCH_OPS (SYNC_XOR_AND_FETCH_1, TSAN_ATOMIC8_FETCH_XOR, BIT_XOR_EXPR),
  FETCH_OPS (SYNC_XOR_AND_FETCH_2, TSAN_ATOMIC16_FETCH_XOR, BIT_XOR_EXPR),
  FETCH_OPS (SYNC_XOR_AND_FETCH_4, TSAN_ATOMIC32_FETCH_XOR, BIT_XOR_EXPR),
  FETCH_OPS (SYNC_XOR_AND_FETCH_8, TSAN_ATOMIC64_FETCH_XOR, BIT_XOR_EXPR),
  FETCH_OPS (SYNC_XOR_AND_FETCH_16, TSAN_ATOMIC128_FETCH_XOR, BIT_XOR_EXPR),
  FETCH_OPS (SYNC_NAND_AND_FETCH_1, TSAN_ATOMIC8_FETCH_NAND, BIT_NOT_EXPR),
  FETCH_OPS (SYNC_NAND_AND_FETCH_2, TSAN_ATOMIC16_FETCH_NAND, BIT_NOT_EXPR),
  FETCH_OPS (SYNC_NAND_AND_FETCH_4, TSAN_ATOMIC32_FETCH_NAND, BIT_NOT_EXPR),
  FETCH_OPS (SYNC_NAND_AND_FETCH_8, TSAN_ATOMIC64_FETCH_NAND, BIT_NOT_EXPR),
  FETCH_OPS (SYNC_NAND_AND_FETCH_16, TSAN_ATOMIC128_FETCH_NAND, BIT_NOT_EXPR),

  WEAK_CAS (ATOMIC_COMPARE_EXCHANGE_1, TSAN_ATOMIC8_COMPARE_EXCHANGE_WEAK),
  WEAK_CAS (ATOMIC_COMPARE_EXCHANGE_2, TSAN_ATOMIC16_COMPARE_EXCHANGE_WEAK),
  WEAK_CAS (ATOMIC_COMPARE_EXCHANGE_4, TSAN_ATOMIC32_COMPARE_EXCHANGE_WEAK),
  WEAK_CAS (ATOMIC_COMPARE_EXCHANGE_8, TSAN_ATOMIC64_COMPARE_EXCHANGE_WEAK),
  WEAK_CAS (ATOMIC_COMPARE_EXCHANGE_16, TSAN_ATOMIC128_COMPARE_EXCHANGE_WEAK),

  STRONG_CAS (ATOMIC_COMPARE_EXCHANGE_1, TSAN_ATOMIC8_COMPARE_EXCHANGE_STRONG),
  STRONG_CAS (ATOMIC_COMPARE_EXCHANGE_2,
	      TSAN_ATOMIC16_COMPARE_EXCHANGE_STRONG),
  STRONG_CAS (ATOMIC_COMPARE_EXCHANGE_4,
	      TSAN_ATOMIC32_COMPARE_EXCHANGE_STRONG),
  STRONG_CAS (ATOMIC_COMPARE_EXCHANGE_8,
	      TSAN_ATOMIC64_COMPARE_EXCHANGE_STRONG),
  STRONG_CAS (ATOMIC_COMPARE_EXCHANGE_16,
	      TSAN_ATOMIC128_COMPARE_EXCHANGE_STRONG),

  BOOL_CAS (SYNC_BOOL_COMPARE_AND_SWAP_1,
	    TSAN_ATOMIC8_COMPARE_EXCHANGE_STRONG),
  BOOL_CAS (SYNC_BOOL_COMPARE_AND_SWAP_2,
	    TSAN_ATOMIC16_COMPARE_EXCHANGE_STRONG),
  BOOL_CAS (SYNC_BOOL_COMPARE_AND_SWAP_4,
	    TSAN_ATOMIC32_COMPARE_EXCHANGE_STRONG),
  BOOL_CAS (SYNC_BOOL_COMPARE_AND_SWAP_8,
	    TSAN_ATOMIC64_COMPARE_EXCHANGE_STRONG),
  BOOL_CAS (SYNC_BOOL_COMPARE_AND_SWAP_16,
	    TSAN_ATOMIC128_COMPARE_EXCHANGE_STRONG),

  VAL_CAS (SYNC_VAL_COMPARE_AND_SWAP_1, TSAN_ATOMIC8_COMPARE_EXCHANGE_STRONG),
  VAL_CAS (SYNC_VAL_COMPARE_AND_SWAP_2, TSAN_ATOMIC16_COMPARE_EXCHANGE_STRONG),
  VAL_CAS (SYNC_VAL_COMPARE_AND_SWAP_4, TSAN_ATOMIC32_COMPARE_EXCHANGE_STRONG),
  VAL_CAS (SYNC_VAL_COMPARE_AND_SWAP_8, TSAN_ATOMIC64_COMPARE_EXCHANGE_STRONG),
  VAL_CAS (SYNC_VAL_COMPARE_AND_SWAP_16,
	   TSAN_ATOMIC128_COMPARE_EXCHANGE_STRONG),

  LOCK_RELEASE (SYNC_LOCK_RELEASE_1, TSAN_ATOMIC8_STORE),
  LOCK_RELEASE (SYNC_LOCK_RELEASE_2, TSAN_ATOMIC16_STORE),
  LOCK_RELEASE (SYNC_LOCK_RELEASE_4, TSAN_ATOMIC32_STORE),
  LOCK_RELEASE (SYNC_LOCK_RELEASE_8, TSAN_ATOMIC64_STORE),
  LOCK_RELEASE (SYNC_LOCK_RELEASE_16, TSAN_ATOMIC128_STORE)
};

/* Instrument an atomic builtin.  */

static void
instrument_builtin_call (gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi), g;
  tree callee = gimple_call_fndecl (stmt), last_arg, args[6], t, lhs;
  enum built_in_function fcode = DECL_FUNCTION_CODE (callee);
  unsigned int i, num = gimple_call_num_args (stmt), j;
  for (j = 0; j < 6 && j < num; j++)
    args[j] = gimple_call_arg (stmt, j);
  for (i = 0; i < ARRAY_SIZE (tsan_atomic_table); i++)
    if (fcode != tsan_atomic_table[i].fcode)
      continue;
    else
      {
	tree decl = builtin_decl_implicit (tsan_atomic_table[i].tsan_fcode);
	if (decl == NULL_TREE)
	  return;
	switch (tsan_atomic_table[i].action)
	  {
	  case check_last:
	  case fetch_op:
	    last_arg = gimple_call_arg (stmt, num - 1);
	    if (!host_integerp (last_arg, 1)
		|| (unsigned HOST_WIDE_INT) tree_low_cst (last_arg, 1)
		   > MEMMODEL_SEQ_CST)
	      return;
	    gimple_call_set_fndecl (stmt, decl);
	    update_stmt (stmt);
	    if (tsan_atomic_table[i].action == fetch_op)
	      {
		args[1] = gimple_call_arg (stmt, 1);
		goto adjust_result;
	      }
	    return;
	  case add_seq_cst:
	  case add_acquire:
	  case fetch_op_seq_cst:
	    gcc_assert (num <= 2);
	    for (j = 0; j < num; j++)
	      args[j] = gimple_call_arg (stmt, j);
	    for (; j < 2; j++)
	      args[j] = NULL_TREE;
	    args[num] = build_int_cst (NULL_TREE,
				       tsan_atomic_table[i].action
				       != add_acquire
				       ? MEMMODEL_SEQ_CST
				       : MEMMODEL_ACQUIRE);
	    update_gimple_call (gsi, decl, num + 1, args[0], args[1], args[2]);
	    stmt = gsi_stmt (*gsi);
	    if (tsan_atomic_table[i].action == fetch_op_seq_cst)
	      {
	      adjust_result:
		lhs = gimple_call_lhs (stmt);
		if (lhs == NULL_TREE)
		  return;
		if (!useless_type_conversion_p (TREE_TYPE (lhs),
						TREE_TYPE (args[1])))
		  {
		    tree var = make_ssa_name (TREE_TYPE (lhs), NULL);
		    g = gimple_build_assign_with_ops (NOP_EXPR, var,
						      args[1], NULL_TREE);
		    gsi_insert_after (gsi, g, GSI_NEW_STMT);
		    args[1] = var;
		  }
		gimple_call_set_lhs (stmt,
				     make_ssa_name (TREE_TYPE (lhs), NULL));
		/* BIT_NOT_EXPR stands for NAND.  */
		if (tsan_atomic_table[i].code == BIT_NOT_EXPR)
		  {
		    tree var = make_ssa_name (TREE_TYPE (lhs), NULL);
		    g = gimple_build_assign_with_ops (BIT_AND_EXPR, var,
						      gimple_call_lhs (stmt),
						      args[1]);
		    gsi_insert_after (gsi, g, GSI_NEW_STMT);
		    g = gimple_build_assign_with_ops (BIT_NOT_EXPR, lhs, var,
						      NULL_TREE);
		  }
		else
		  g = gimple_build_assign_with_ops (tsan_atomic_table[i].code,
						    lhs,
						    gimple_call_lhs (stmt),
						    args[1]);
		update_stmt (stmt);
		gsi_insert_after (gsi, g, GSI_NEW_STMT);
	      }
	    return;
	  case weak_cas:
	    if (!integer_nonzerop (gimple_call_arg (stmt, 3)))
	      continue;
	    /* FALLTHRU */
	  case strong_cas:
	    gcc_assert (num == 6);
	    for (j = 0; j < 6; j++)
	      args[j] = gimple_call_arg (stmt, j);
	    if (!host_integerp (args[4], 1)
		|| (unsigned HOST_WIDE_INT) tree_low_cst (args[4], 1)
		   > MEMMODEL_SEQ_CST)
	      return;
	    if (!host_integerp (args[5], 1)
		|| (unsigned HOST_WIDE_INT) tree_low_cst (args[5], 1)
		   > MEMMODEL_SEQ_CST)
	      return;
	    update_gimple_call (gsi, decl, 5, args[0], args[1], args[2],
				args[4], args[5]);
	    return;
	  case bool_cas:
	  case val_cas:
	    gcc_assert (num == 3);
	    for (j = 0; j < 3; j++)
	      args[j] = gimple_call_arg (stmt, j);
	    t = TYPE_ARG_TYPES (TREE_TYPE (decl));
	    t = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (t)));
	    t = create_tmp_var (t, NULL);
	    mark_addressable (t);
	    if (!useless_type_conversion_p (TREE_TYPE (t),
					    TREE_TYPE (args[1])))
	      {
		g = gimple_build_assign_with_ops (NOP_EXPR,
						  make_ssa_name (TREE_TYPE (t),
								 NULL),
						  args[1], NULL_TREE);
		gsi_insert_before (gsi, g, GSI_SAME_STMT);
		args[1] = gimple_assign_lhs (g);
	      }
	    g = gimple_build_assign (t, args[1]);
	    gsi_insert_before (gsi, g, GSI_SAME_STMT);
	    lhs = gimple_call_lhs (stmt);
	    update_gimple_call (gsi, decl, 5, args[0],
				build_fold_addr_expr (t), args[2],
				build_int_cst (NULL_TREE,
					       MEMMODEL_SEQ_CST),
				build_int_cst (NULL_TREE,
					       MEMMODEL_SEQ_CST));
	    if (tsan_atomic_table[i].action == val_cas && lhs)
	      {
		tree cond;
		stmt = gsi_stmt (*gsi);
		g = gimple_build_assign (make_ssa_name (TREE_TYPE (t), NULL),
					 t);
		gsi_insert_after (gsi, g, GSI_NEW_STMT);
		t = make_ssa_name (TREE_TYPE (TREE_TYPE (decl)), stmt);
		cond = build2 (NE_EXPR, boolean_type_node, t,
			       build_int_cst (TREE_TYPE (t), 0));
		g = gimple_build_assign_with_ops (COND_EXPR, lhs, cond,
						  args[1],
						  gimple_assign_lhs (g));
		gimple_call_set_lhs (stmt, t);
		update_stmt (stmt);
		gsi_insert_after (gsi, g, GSI_NEW_STMT);
	      }
	    return;
	  case lock_release:
	    gcc_assert (num == 1);
	    t = TYPE_ARG_TYPES (TREE_TYPE (decl));
	    t = TREE_VALUE (TREE_CHAIN (t));
	    update_gimple_call (gsi, decl, 3, gimple_call_arg (stmt, 0),
				build_int_cst (t, 0),
				build_int_cst (NULL_TREE,
					       MEMMODEL_RELEASE));
	    return;
	  default:
	    continue;
	  }
      }
}

/* Instruments the gimple pointed to by GSI. Return
   true if func entry/exit should be instrumented.  */

static bool
instrument_gimple (gimple_stmt_iterator *gsi)
{
  gimple stmt;
  tree rhs, lhs;
  bool instrumented = false;

  stmt = gsi_stmt (*gsi);
  if (is_gimple_call (stmt)
      && (gimple_call_fndecl (stmt)
	  != builtin_decl_implicit (BUILT_IN_TSAN_INIT)))
    {
      if (is_gimple_builtin_call (stmt))
	instrument_builtin_call (gsi);
      return true;
    }
  else if (is_gimple_assign (stmt)
	   && !gimple_clobber_p (stmt))
    {
      if (gimple_store_p (stmt))
	{
	  lhs = gimple_assign_lhs (stmt);
	  instrumented = instrument_expr (*gsi, lhs, true);
	}
      if (gimple_assign_load_p (stmt))
	{
	  rhs = gimple_assign_rhs1 (stmt);
	  instrumented = instrument_expr (*gsi, rhs, false);
	}
    }
  return instrumented;
}

/* Instruments all interesting memory accesses in the current function.
   Return true if func entry/exit should be instrumented.  */

static bool
instrument_memory_accesses (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  bool fentry_exit_instrument = false;

  FOR_EACH_BB (bb)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      fentry_exit_instrument |= instrument_gimple (&gsi);
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
      gcc_assert (gimple_code (stmt) == GIMPLE_RETURN
		  || gimple_call_builtin_p (stmt, BUILT_IN_RETURN));
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
  initialize_sanitizer_builtins ();
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
  return flag_tsan != 0;
}

/* Inserts __tsan_init () into the list of CTORs.  */

void
tsan_finish_file (void)
{
  tree ctor_statements = NULL_TREE;

  initialize_sanitizer_builtins ();
  tree init_decl = builtin_decl_implicit (BUILT_IN_TSAN_INIT);
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
  TODO_verify_all | TODO_update_ssa	/* todo_flags_finish  */
 }
};

static bool
tsan_gate_O0 (void)
{
  return flag_tsan != 0 && !optimize;
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
  TODO_verify_all | TODO_update_ssa	/* todo_flags_finish  */
 }
};
