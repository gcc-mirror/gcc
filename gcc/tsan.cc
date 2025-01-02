/* GCC instrumentation plugin for ThreadSanitizer.
   Copyright (C) 2011-2025 Free Software Foundation, Inc.
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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "fold-const.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-iterator.h"
#include "gimple-fold.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-eh.h"
#include "tsan.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "builtins.h"
#include "target.h"
#include "diagnostic-core.h"

/* Number of instrumented memory accesses in the current function.  */

/* Builds the following decl
   void __tsan_read/writeX (void *addr);  */

static tree
get_memory_access_decl (bool is_write, unsigned size, bool volatilep)
{
  enum built_in_function fcode;
  int pos;

  if (size <= 1)
    pos = 0;
  else if (size <= 3)
    pos = 1;
  else if (size <= 7)
    pos = 2;
  else if (size <= 15)
    pos = 3;
  else
    pos = 4;

  if (param_tsan_distinguish_volatile && volatilep)
    fcode = is_write ? BUILT_IN_TSAN_VOLATILE_WRITE1
		     : BUILT_IN_TSAN_VOLATILE_READ1;
  else
    fcode = is_write ? BUILT_IN_TSAN_WRITE1
		     : BUILT_IN_TSAN_READ1;
  fcode = (built_in_function)(fcode + pos);

  return builtin_decl_implicit (fcode);
}

/* Check as to whether EXPR refers to a store to vptr.  */

static tree
is_vptr_store (gimple *stmt, tree expr, bool is_write)
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
  gimple *stmt, *g;
  gimple_seq seq;
  location_t loc;
  unsigned int align;

  size = int_size_in_bytes (TREE_TYPE (expr));
  if (size <= 0)
    return false;

  poly_int64 unused_bitsize, unused_bitpos;
  tree offset;
  machine_mode mode;
  int unsignedp, reversep, volatilep = 0;
  base = get_inner_reference (expr, &unused_bitsize, &unused_bitpos, &offset,
			      &mode, &unsignedp, &reversep, &volatilep);

  /* No need to instrument accesses to decls that don't escape,
     they can't escape to other threads then.  */
  if (DECL_P (base) && !is_global_var (base))
    {
      struct pt_solution pt;
      memset (&pt, 0, sizeof (pt));
      pt.escaped = 1;
      pt.ipa_escaped = flag_ipa_pta != 0;
      if (!pt_solution_includes (&pt, base))
	return false;
      if (!may_be_aliased (base))
	return false;
    }

  if (TREE_READONLY (base) || (VAR_P (base) && DECL_HARD_REGISTER (base)))
    return false;

  if (!ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (TREE_TYPE (base))))
    return false;

  stmt = gsi_stmt (gsi);
  loc = gimple_location (stmt);
  rhs = is_vptr_store (stmt, expr, is_write);

  if ((TREE_CODE (expr) == COMPONENT_REF
       && DECL_BIT_FIELD_TYPE (TREE_OPERAND (expr, 1)))
      || TREE_CODE (expr) == BIT_FIELD_REF)
    {
      HOST_WIDE_INT bitpos, bitsize;
      base = TREE_OPERAND (expr, 0);
      if (TREE_CODE (expr) == COMPONENT_REF)
	{
	  expr = TREE_OPERAND (expr, 1);
	  if (is_write && DECL_BIT_FIELD_REPRESENTATIVE (expr))
	    expr = DECL_BIT_FIELD_REPRESENTATIVE (expr);
	  if (!tree_fits_uhwi_p (DECL_FIELD_OFFSET (expr))
	      || !tree_fits_uhwi_p (DECL_FIELD_BIT_OFFSET (expr))
	      || !tree_fits_uhwi_p (DECL_SIZE (expr)))
	    return false;
	  bitpos = tree_to_uhwi (DECL_FIELD_OFFSET (expr)) * BITS_PER_UNIT
		   + tree_to_uhwi (DECL_FIELD_BIT_OFFSET (expr));
	  bitsize = tree_to_uhwi (DECL_SIZE (expr));
	}
      else
	{
	  if (!tree_fits_uhwi_p (TREE_OPERAND (expr, 2))
	      || !tree_fits_uhwi_p (TREE_OPERAND (expr, 1)))
	    return false;
	  bitpos = tree_to_uhwi (TREE_OPERAND (expr, 2));
	  bitsize = tree_to_uhwi (TREE_OPERAND (expr, 1));
	}
      if (bitpos < 0 || bitsize <= 0)
	return false;
      size = (bitpos % BITS_PER_UNIT + bitsize + BITS_PER_UNIT - 1)
	     / BITS_PER_UNIT;
      if (may_be_nonaddressable_p (base))
	return false;
      align = get_object_alignment (base);
      if (align < BITS_PER_UNIT)
	return false;
      bitpos = bitpos & ~(BITS_PER_UNIT - 1);
      if ((align - 1) & bitpos)
	{
	  align = (align - 1) & bitpos;
	  align = least_bit_hwi (align);
	}
      expr = build_fold_addr_expr (unshare_expr (base));
      expr = build2 (MEM_REF, char_type_node, expr,
		     build_int_cst (TREE_TYPE (expr), bitpos / BITS_PER_UNIT));
      expr_ptr = build_fold_addr_expr (expr);
    }
  else
    {
      if (may_be_nonaddressable_p (expr))
	return false;
      align = get_object_alignment (expr);
      if (align < BITS_PER_UNIT)
	return false;
      expr_ptr = build_fold_addr_expr (unshare_expr (expr));
    }
  expr_ptr = force_gimple_operand (expr_ptr, &seq, true, NULL_TREE);
  if ((size & (size - 1)) != 0 || size > 16
      || align < MIN (size, 8) * BITS_PER_UNIT)
    {
      builtin_decl = builtin_decl_implicit (is_write
					    ? BUILT_IN_TSAN_WRITE_RANGE
					    : BUILT_IN_TSAN_READ_RANGE);
      g = gimple_build_call (builtin_decl, 2, expr_ptr, size_int (size));
    }
  else if (rhs == NULL)
    g = gimple_build_call (get_memory_access_decl (is_write, size,
						   TREE_THIS_VOLATILE (expr)),
			   1, expr_ptr);
  else
    {
      builtin_decl = builtin_decl_implicit (BUILT_IN_TSAN_VPTR_UPDATE);
      g = gimple_build_call (builtin_decl, 2, expr_ptr, unshare_expr (rhs));
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
  bool_cas, val_cas, lock_release, fetch_op, fetch_op_seq_cst,
  bool_clear, bool_test_and_set
};

/* Table how to map sync/atomic builtins to their corresponding
   tsan equivalents.  */
static const struct tsan_map_atomic
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
#define BOOL_CLEAR(fcode, tsan_fcode) \
  TRANSFORM (fcode, tsan_fcode, bool_clear, ERROR_MARK)
#define BOOL_TEST_AND_SET(fcode, tsan_fcode) \
  TRANSFORM (fcode, tsan_fcode, bool_test_and_set, ERROR_MARK)

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
  LOCK_RELEASE (SYNC_LOCK_RELEASE_16, TSAN_ATOMIC128_STORE),

  BOOL_CLEAR (ATOMIC_CLEAR, TSAN_ATOMIC8_STORE),

  BOOL_TEST_AND_SET (ATOMIC_TEST_AND_SET, TSAN_ATOMIC8_EXCHANGE)
};

/* Instrument an atomic builtin.  */

static void
instrument_builtin_call (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi), *g;
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
	if (fcode == BUILT_IN_ATOMIC_THREAD_FENCE)
	  warning_at (gimple_location (stmt), OPT_Wtsan,
		      "%qs is not supported with %qs", "atomic_thread_fence",
		      "-fsanitize=thread");

	tree decl = builtin_decl_implicit (tsan_atomic_table[i].tsan_fcode);
	if (decl == NULL_TREE)
	  return;
	switch (tsan_atomic_table[i].action)
	  {
	  case check_last:
	  case fetch_op:
	    last_arg = gimple_call_arg (stmt, num - 1);
	    if (tree_fits_uhwi_p (last_arg)
		&& memmodel_base (tree_to_uhwi (last_arg)) >= MEMMODEL_LAST)
	      return;
	    gimple_call_set_fndecl (stmt, decl);
	    update_stmt (stmt);
	    maybe_clean_eh_stmt (stmt);
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
	    maybe_clean_or_replace_eh_stmt (stmt, gsi_stmt (*gsi));
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
		    tree var = make_ssa_name (TREE_TYPE (lhs));
		    g = gimple_build_assign (var, NOP_EXPR, args[1]);
		    gsi_insert_after (gsi, g, GSI_NEW_STMT);
		    args[1] = var;
		  }
		gimple_call_set_lhs (stmt, make_ssa_name (TREE_TYPE (lhs)));
		/* BIT_NOT_EXPR stands for NAND.  */
		if (tsan_atomic_table[i].code == BIT_NOT_EXPR)
		  {
		    tree var = make_ssa_name (TREE_TYPE (lhs));
		    g = gimple_build_assign (var, BIT_AND_EXPR,
					     gimple_call_lhs (stmt), args[1]);
		    gsi_insert_after (gsi, g, GSI_NEW_STMT);
		    g = gimple_build_assign (lhs, BIT_NOT_EXPR, var);
		  }
		else
		  g = gimple_build_assign (lhs, tsan_atomic_table[i].code,
					   gimple_call_lhs (stmt), args[1]);
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
	    if (tree_fits_uhwi_p (args[4])
		&& memmodel_base (tree_to_uhwi (args[4])) >= MEMMODEL_LAST)
	      return;
	    if (tree_fits_uhwi_p (args[5])
		&& memmodel_base (tree_to_uhwi (args[5])) >= MEMMODEL_LAST)
	      return;
	    update_gimple_call (gsi, decl, 5, args[0], args[1], args[2],
				args[4], args[5]);
	    maybe_clean_or_replace_eh_stmt (stmt, gsi_stmt (*gsi));
	    return;
	  case bool_cas:
	  case val_cas:
	    gcc_assert (num == 3);
	    for (j = 0; j < 3; j++)
	      args[j] = gimple_call_arg (stmt, j);
	    t = TYPE_ARG_TYPES (TREE_TYPE (decl));
	    t = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (t)));
	    t = create_tmp_var (t);
	    mark_addressable (t);
	    if (!useless_type_conversion_p (TREE_TYPE (t),
					    TREE_TYPE (args[1])))
	      {
		g = gimple_build_assign (make_ssa_name (TREE_TYPE (t)),
					 NOP_EXPR, args[1]);
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
	    maybe_clean_or_replace_eh_stmt (stmt, gsi_stmt (*gsi));
	    if (tsan_atomic_table[i].action == val_cas && lhs)
	      {
		stmt = gsi_stmt (*gsi);
		tree t2 = make_ssa_name (TREE_TYPE (t));
		g = gimple_build_assign (t2, t);
		gsi_insert_after (gsi, g, GSI_NEW_STMT);
		t = make_ssa_name (TREE_TYPE (TREE_TYPE (decl)), stmt);
		tree cond = make_ssa_name (boolean_type_node);
		g = gimple_build_assign (cond, NE_EXPR,
					 t, build_zero_cst (TREE_TYPE (t)));
		gsi_insert_after (gsi, g, GSI_NEW_STMT);
		g = gimple_build_assign (lhs, COND_EXPR, cond, args[1], t2);
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
	    maybe_clean_or_replace_eh_stmt (stmt, gsi_stmt (*gsi));
	    return;
	  case bool_clear:
	  case bool_test_and_set:
	    if (BOOL_TYPE_SIZE != 8)
	      {
		decl = NULL_TREE;
		for (j = 1; j < 5; j++)
		  if (BOOL_TYPE_SIZE == (8 << j))
		    {
		      enum built_in_function tsan_fcode
			= (enum built_in_function)
			  (tsan_atomic_table[i].tsan_fcode + j);
		      decl = builtin_decl_implicit (tsan_fcode);
		      break;
		    }
		if (decl == NULL_TREE)
		  return;
	      }
	    last_arg = gimple_call_arg (stmt, num - 1);
	    if (tree_fits_uhwi_p (last_arg)
		&& memmodel_base (tree_to_uhwi (last_arg)) >= MEMMODEL_LAST)
	      return;
	    t = TYPE_ARG_TYPES (TREE_TYPE (decl));
	    t = TREE_VALUE (TREE_CHAIN (t));
	    if (tsan_atomic_table[i].action == bool_clear)
	      {
		update_gimple_call (gsi, decl, 3, gimple_call_arg (stmt, 0),
				    build_int_cst (t, 0), last_arg);
		maybe_clean_or_replace_eh_stmt (stmt, gsi_stmt (*gsi));
		return;
	      }
	    t = build_int_cst (t, targetm.atomic_test_and_set_trueval);
	    update_gimple_call (gsi, decl, 3, gimple_call_arg (stmt, 0),
				t, last_arg);
	    maybe_clean_or_replace_eh_stmt (stmt, gsi_stmt (*gsi));
	    stmt = gsi_stmt (*gsi);
	    lhs = gimple_call_lhs (stmt);
	    if (lhs == NULL_TREE)
	      return;
	    if (targetm.atomic_test_and_set_trueval != 1
		|| !useless_type_conversion_p (TREE_TYPE (lhs),
					       TREE_TYPE (t)))
	      {
		tree new_lhs = make_ssa_name (TREE_TYPE (t));
		gimple_call_set_lhs (stmt, new_lhs);
		if (targetm.atomic_test_and_set_trueval != 1)
		  g = gimple_build_assign (lhs, NE_EXPR, new_lhs,
					   build_int_cst (TREE_TYPE (t), 0));
		else
		  g = gimple_build_assign (lhs, NOP_EXPR, new_lhs);
		gsi_insert_after (gsi, g, GSI_NEW_STMT);
		update_stmt (stmt);
	      }
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
  gimple *stmt;
  tree rhs, lhs;
  bool instrumented = false;

  stmt = gsi_stmt (*gsi);
  if (is_gimple_call (stmt)
      && (gimple_call_fndecl (stmt)
	  != builtin_decl_implicit (BUILT_IN_TSAN_INIT)))
    {
      /* All functions with function call will have exit instrumented,
	 therefore no function calls other than __tsan_func_exit
	 shall appear in the functions.  */
      gimple_call_set_tail (as_a <gcall *> (stmt), false);
      if (gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
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

/* Replace TSAN_FUNC_EXIT internal call with function exit tsan builtin.  */

static void
replace_func_exit (gimple *stmt)
{
  tree builtin_decl = builtin_decl_implicit (BUILT_IN_TSAN_FUNC_EXIT);
  gimple *g = gimple_build_call (builtin_decl, 0);
  gimple_set_location (g, cfun->function_end_locus);
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  gsi_replace (&gsi, g, true);
}

/* Instrument function exit.  Used when TSAN_FUNC_EXIT does not exist.  */

static void
instrument_func_exit (void)
{
  location_t loc;
  basic_block exit_bb;
  gimple_stmt_iterator gsi;
  gimple *stmt, *g;
  tree builtin_decl;
  edge e;
  edge_iterator ei;

  /* Find all function exits.  */
  exit_bb = EXIT_BLOCK_PTR_FOR_FN (cfun);
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

/* Instruments all interesting memory accesses in the current function.
   Return true if func entry/exit should be instrumented.  */

static bool
instrument_memory_accesses (bool *cfg_changed)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  bool fentry_exit_instrument = false;
  bool func_exit_seen = false;
  auto_vec<gimple *> tsan_func_exits;

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (gimple_call_internal_p (stmt, IFN_TSAN_FUNC_EXIT))
	    {
	      if (fentry_exit_instrument)
		replace_func_exit (stmt);
	      else
		tsan_func_exits.safe_push (stmt);
	      func_exit_seen = true;
	    }
	  else
	    fentry_exit_instrument
	      |= (instrument_gimple (&gsi)
		  && param_tsan_instrument_func_entry_exit);
	}
      if (gimple_purge_dead_eh_edges (bb))
	*cfg_changed = true;
    }
  unsigned int i;
  gimple *stmt;
  FOR_EACH_VEC_ELT (tsan_func_exits, i, stmt)
    if (fentry_exit_instrument)
      replace_func_exit (stmt);
    else
      {
	gsi = gsi_for_stmt (stmt);
	gsi_remove (&gsi, true);
      }
  if (fentry_exit_instrument && !func_exit_seen)
    instrument_func_exit ();
  return fentry_exit_instrument;
}

/* Instruments function entry.  */

static void
instrument_func_entry (void)
{
  tree ret_addr, builtin_decl;
  gimple *g;
  gimple_seq seq = NULL;

  builtin_decl = builtin_decl_implicit (BUILT_IN_RETURN_ADDRESS);
  g = gimple_build_call (builtin_decl, 1, integer_zero_node);
  ret_addr = make_ssa_name (ptr_type_node);
  gimple_call_set_lhs (g, ret_addr);
  gimple_set_location (g, cfun->function_start_locus);
  gimple_seq_add_stmt_without_update (&seq, g);

  builtin_decl = builtin_decl_implicit (BUILT_IN_TSAN_FUNC_ENTRY);
  g = gimple_build_call (builtin_decl, 1, ret_addr);
  gimple_set_location (g, cfun->function_start_locus);
  gimple_seq_add_stmt_without_update (&seq, g);

  edge e = single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  gsi_insert_seq_on_edge_immediate (e, seq);
}

/* ThreadSanitizer instrumentation pass.  */

static unsigned
tsan_pass (void)
{
  initialize_sanitizer_builtins ();
  bool cfg_changed = false;
  if (instrument_memory_accesses (&cfg_changed))
    instrument_func_entry ();
  return cfg_changed ? TODO_cleanup_cfg : 0;
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

namespace {

const pass_data pass_data_tsan =
{
  GIMPLE_PASS, /* type */
  "tsan", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_ssa | PROP_cfg ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_tsan : public gimple_opt_pass
{
public:
  pass_tsan (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tsan, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override { return new pass_tsan (m_ctxt); }
  bool gate (function *) final override
{
  return sanitize_flags_p (SANITIZE_THREAD);
}

  unsigned int execute (function *) final override { return tsan_pass (); }

}; // class pass_tsan

} // anon namespace

gimple_opt_pass *
make_pass_tsan (gcc::context *ctxt)
{
  return new pass_tsan (ctxt);
}

namespace {

const pass_data pass_data_tsan_O0 =
{
  GIMPLE_PASS, /* type */
  "tsan0", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_ssa | PROP_cfg ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_tsan_O0 : public gimple_opt_pass
{
public:
  pass_tsan_O0 (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tsan_O0, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      return (sanitize_flags_p (SANITIZE_THREAD) && !optimize);
    }

  unsigned int execute (function *) final override { return tsan_pass (); }

}; // class pass_tsan_O0

} // anon namespace

gimple_opt_pass *
make_pass_tsan_O0 (gcc::context *ctxt)
{
  return new pass_tsan_O0 (ctxt);
}
