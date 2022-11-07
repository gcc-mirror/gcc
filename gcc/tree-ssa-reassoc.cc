/* Reassociation for trees.
   Copyright (C) 2005-2022 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dan@dberlin.org>

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
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "memmodel.h"
#include "tm_p.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "cfganal.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-ssa-loop.h"
#include "flags.h"
#include "tree-ssa.h"
#include "langhooks.h"
#include "cfgloop.h"
#include "builtins.h"
#include "gimplify.h"
#include "case-cfn-macros.h"
#include "tree-ssa-reassoc.h"
#include "tree-ssa-math-opts.h"
#include "gimple-range.h"

/*  This is a simple global reassociation pass.  It is, in part, based
    on the LLVM pass of the same name (They do some things more/less
    than we do, in different orders, etc).

    It consists of five steps:

    1. Breaking up subtract operations into addition + negate, where
    it would promote the reassociation of adds.

    2. Left linearization of the expression trees, so that (A+B)+(C+D)
    becomes (((A+B)+C)+D), which is easier for us to rewrite later.
    During linearization, we place the operands of the binary
    expressions into a vector of operand_entry_*

    3. Optimization of the operand lists, eliminating things like a +
    -a, a & a, etc.

    3a. Combine repeated factors with the same occurrence counts
    into a __builtin_powi call that will later be optimized into
    an optimal number of multiplies.

    4. Rewrite the expression trees we linearized and optimized so
    they are in proper rank order.

    5. Repropagate negates, as nothing else will clean it up ATM.

    A bit of theory on #4, since nobody seems to write anything down
    about why it makes sense to do it the way they do it:

    We could do this much nicer theoretically, but don't (for reasons
    explained after how to do it theoretically nice :P).

    In order to promote the most redundancy elimination, you want
    binary expressions whose operands are the same rank (or
    preferably, the same value) exposed to the redundancy eliminator,
    for possible elimination.

    So the way to do this if we really cared, is to build the new op
    tree from the leaves to the roots, merging as you go, and putting the
    new op on the end of the worklist, until you are left with one
    thing on the worklist.

    IE if you have to rewrite the following set of operands (listed with
    rank in parentheses), with opcode PLUS_EXPR:

    a (1),  b (1),  c (1),  d (2), e (2)


    We start with our merge worklist empty, and the ops list with all of
    those on it.

    You want to first merge all leaves of the same rank, as much as
    possible.

    So first build a binary op of

    mergetmp = a + b, and put "mergetmp" on the merge worklist.

    Because there is no three operand form of PLUS_EXPR, c is not going to
    be exposed to redundancy elimination as a rank 1 operand.

    So you might as well throw it on the merge worklist (you could also
    consider it to now be a rank two operand, and merge it with d and e,
    but in this case, you then have evicted e from a binary op. So at
    least in this situation, you can't win.)

    Then build a binary op of d + e
    mergetmp2 = d + e

    and put mergetmp2 on the merge worklist.

    so merge worklist = {mergetmp, c, mergetmp2}

    Continue building binary ops of these operations until you have only
    one operation left on the worklist.

    So we have

    build binary op
    mergetmp3 = mergetmp + c

    worklist = {mergetmp2, mergetmp3}

    mergetmp4 = mergetmp2 + mergetmp3

    worklist = {mergetmp4}

    because we have one operation left, we can now just set the original
    statement equal to the result of that operation.

    This will at least expose a + b  and d + e to redundancy elimination
    as binary operations.

    For extra points, you can reuse the old statements to build the
    mergetmps, since you shouldn't run out.

    So why don't we do this?

    Because it's expensive, and rarely will help.  Most trees we are
    reassociating have 3 or less ops.  If they have 2 ops, they already
    will be written into a nice single binary op.  If you have 3 ops, a
    single simple check suffices to tell you whether the first two are of the
    same rank.  If so, you know to order it

    mergetmp = op1 + op2
    newstmt = mergetmp + op3

    instead of
    mergetmp = op2 + op3
    newstmt = mergetmp + op1

    If all three are of the same rank, you can't expose them all in a
    single binary operator anyway, so the above is *still* the best you
    can do.

    Thus, this is what we do.  When we have three ops left, we check to see
    what order to put them in, and call it a day.  As a nod to vector sum
    reduction, we check if any of the ops are really a phi node that is a
    destructive update for the associating op, and keep the destructive
    update together for vector sum reduction recognition.  */

/* Enable insertion of __builtin_powi calls during execute_reassoc.  See
   point 3a in the pass header comment.  */
static bool reassoc_insert_powi_p;

/* Enable biasing ranks of loop accumulators.  We don't want this before
   vectorization, since it interferes with reduction chains.  */
static bool reassoc_bias_loop_carried_phi_ranks_p;

/* Statistics */
static struct
{
  int linearized;
  int constants_eliminated;
  int ops_eliminated;
  int rewritten;
  int pows_encountered;
  int pows_created;
} reassociate_stats;


static object_allocator<operand_entry> operand_entry_pool
  ("operand entry pool");

/* This is used to assign a unique ID to each struct operand_entry
   so that qsort results are identical on different hosts.  */
static unsigned int next_operand_entry_id;

/* Starting rank number for a given basic block, so that we can rank
   operations using unmovable instructions in that BB based on the bb
   depth.  */
static int64_t *bb_rank;

/* Operand->rank hashtable.  */
static hash_map<tree, int64_t> *operand_rank;

/* SSA_NAMEs that are forms of loop accumulators and whose ranks need to be
   biased.  */
static auto_bitmap biased_names;

/* Vector of SSA_NAMEs on which after reassociate_bb is done with
   all basic blocks the CFG should be adjusted - basic blocks
   split right after that SSA_NAME's definition statement and before
   the only use, which must be a bit ior.  */
static vec<tree> reassoc_branch_fixups;

/* Forward decls.  */
static int64_t get_rank (tree);
static bool reassoc_stmt_dominates_stmt_p (gimple *, gimple *);

/* Wrapper around gsi_remove, which adjusts gimple_uid of debug stmts
   possibly added by gsi_remove.  */

static bool
reassoc_remove_stmt (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);

  if (!MAY_HAVE_DEBUG_BIND_STMTS || gimple_code (stmt) == GIMPLE_PHI)
    return gsi_remove (gsi, true);

  gimple_stmt_iterator prev = *gsi;
  gsi_prev (&prev);
  unsigned uid = gimple_uid (stmt);
  basic_block bb = gimple_bb (stmt);
  bool ret = gsi_remove (gsi, true);
  if (!gsi_end_p (prev))
    gsi_next (&prev);
  else
    prev = gsi_start_bb (bb);
  gimple *end_stmt = gsi_stmt (*gsi);
  while ((stmt = gsi_stmt (prev)) != end_stmt)
    {
      gcc_assert (stmt && is_gimple_debug (stmt) && gimple_uid (stmt) == 0);
      gimple_set_uid (stmt, uid);
      gsi_next (&prev);
    }
  return ret;
}

/* Bias amount for loop-carried phis.  We want this to be larger than
   the depth of any reassociation tree we can see, but not larger than
   the rank difference between two blocks.  */
#define PHI_LOOP_BIAS (1 << 15)

/* Return TRUE iff PHI_LOOP_BIAS should be propagated from one of the STMT's
   operands to the STMT's left-hand side.  The goal is to preserve bias in code
   like this:

     x_1 = phi(x_0, x_2)
     a = x_1 | 1
     b = a ^ 2
     .MEM = b
     c = b + d
     x_2 = c + e

   That is, we need to preserve bias along single-use chains originating from
   loop-carried phis.  Only GIMPLE_ASSIGNs to SSA_NAMEs are considered to be
   uses, because only they participate in rank propagation.  */
static bool
propagate_bias_p (gimple *stmt)
{
  use_operand_p use;
  imm_use_iterator use_iter;
  gimple *single_use_stmt = NULL;

  if (TREE_CODE_CLASS (gimple_assign_rhs_code (stmt)) == tcc_reference)
    return false;

  FOR_EACH_IMM_USE_FAST (use, use_iter, gimple_assign_lhs (stmt))
    {
      gimple *current_use_stmt = USE_STMT (use);

      if (is_gimple_assign (current_use_stmt)
	  && TREE_CODE (gimple_assign_lhs (current_use_stmt)) == SSA_NAME)
	{
	  if (single_use_stmt != NULL && single_use_stmt != current_use_stmt)
	    return false;
	  single_use_stmt = current_use_stmt;
	}
    }

  if (single_use_stmt == NULL)
    return false;

  if (gimple_bb (stmt)->loop_father
      != gimple_bb (single_use_stmt)->loop_father)
    return false;

  return true;
}

/* Rank assigned to a phi statement.  If STMT is a loop-carried phi of
   an innermost loop, and the phi has only a single use which is inside
   the loop, then the rank is the block rank of the loop latch plus an
   extra bias for the loop-carried dependence.  This causes expressions
   calculated into an accumulator variable to be independent for each
   iteration of the loop.  If STMT is some other phi, the rank is the
   block rank of its containing block.  */
static int64_t
phi_rank (gimple *stmt)
{
  basic_block bb = gimple_bb (stmt);
  class loop *father = bb->loop_father;
  tree res;
  unsigned i;
  use_operand_p use;
  gimple *use_stmt;

  if (!reassoc_bias_loop_carried_phi_ranks_p)
    return bb_rank[bb->index];

  /* We only care about real loops (those with a latch).  */
  if (!father->latch)
    return bb_rank[bb->index];

  /* Interesting phis must be in headers of innermost loops.  */
  if (bb != father->header
      || father->inner)
    return bb_rank[bb->index];

  /* Ignore virtual SSA_NAMEs.  */
  res = gimple_phi_result (stmt);
  if (virtual_operand_p (res))
    return bb_rank[bb->index];

  /* The phi definition must have a single use, and that use must be
     within the loop.  Otherwise this isn't an accumulator pattern.  */
  if (!single_imm_use (res, &use, &use_stmt)
      || gimple_bb (use_stmt)->loop_father != father)
    return bb_rank[bb->index];

  /* Look for phi arguments from within the loop.  If found, bias this phi.  */
  for (i = 0; i < gimple_phi_num_args (stmt); i++)
    {
      tree arg = gimple_phi_arg_def (stmt, i);
      if (TREE_CODE (arg) == SSA_NAME
	  && !SSA_NAME_IS_DEFAULT_DEF (arg))
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (arg);
	  if (gimple_bb (def_stmt)->loop_father == father)
	    return bb_rank[father->latch->index] + PHI_LOOP_BIAS;
	}
    }

  /* Must be an uninteresting phi.  */
  return bb_rank[bb->index];
}

/* Return the maximum of RANK and the rank that should be propagated
   from expression OP.  For most operands, this is just the rank of OP.
   For loop-carried phis, the value is zero to avoid undoing the bias
   in favor of the phi.  */
static int64_t
propagate_rank (int64_t rank, tree op, bool *maybe_biased_p)
{
  int64_t op_rank;

  op_rank = get_rank (op);

  /* Check whether op is biased after the get_rank () call, since it might have
     updated biased_names.  */
  if (TREE_CODE (op) == SSA_NAME
      && bitmap_bit_p (biased_names, SSA_NAME_VERSION (op)))
    {
      if (maybe_biased_p == NULL)
	return rank;
      *maybe_biased_p = true;
    }

  return MAX (rank, op_rank);
}

/* Look up the operand rank structure for expression E.  */

static inline int64_t
find_operand_rank (tree e)
{
  int64_t *slot = operand_rank->get (e);
  return slot ? *slot : -1;
}

/* Insert {E,RANK} into the operand rank hashtable.  */

static inline void
insert_operand_rank (tree e, int64_t rank)
{
  gcc_assert (rank > 0);
  gcc_assert (!operand_rank->put (e, rank));
}

/* Given an expression E, return the rank of the expression.  */

static int64_t
get_rank (tree e)
{
  /* SSA_NAME's have the rank of the expression they are the result
     of.
     For globals and uninitialized values, the rank is 0.
     For function arguments, use the pre-setup rank.
     For PHI nodes, stores, asm statements, etc, we use the rank of
     the BB.
     For simple operations, the rank is the maximum rank of any of
     its operands, or the bb_rank, whichever is less.
     I make no claims that this is optimal, however, it gives good
     results.  */

  /* We make an exception to the normal ranking system to break
     dependences of accumulator variables in loops.  Suppose we
     have a simple one-block loop containing:

       x_1 = phi(x_0, x_2)
       b = a + x_1
       c = b + d
       x_2 = c + e

     As shown, each iteration of the calculation into x is fully
     dependent upon the iteration before it.  We would prefer to
     see this in the form:

       x_1 = phi(x_0, x_2)
       b = a + d
       c = b + e
       x_2 = c + x_1

     If the loop is unrolled, the calculations of b and c from
     different iterations can be interleaved.

     To obtain this result during reassociation, we bias the rank
     of the phi definition x_1 upward, when it is recognized as an
     accumulator pattern.  The artificial rank causes it to be 
     added last, providing the desired independence.  */

  if (TREE_CODE (e) == SSA_NAME)
    {
      ssa_op_iter iter;
      gimple *stmt;
      int64_t rank;
      tree op;

      /* If we already have a rank for this expression, use that.  */
      rank = find_operand_rank (e);
      if (rank != -1)
	return rank;

      stmt = SSA_NAME_DEF_STMT (e);
      if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  rank = phi_rank (stmt);
	  if (rank != bb_rank[gimple_bb (stmt)->index])
	    bitmap_set_bit (biased_names, SSA_NAME_VERSION (e));
	}

      else if (!is_gimple_assign (stmt))
	rank = bb_rank[gimple_bb (stmt)->index];

      else
	{
	  bool biased_p = false;
	  bool *maybe_biased_p = propagate_bias_p (stmt) ? &biased_p : NULL;

	  /* Otherwise, find the maximum rank for the operands.  As an
	     exception, remove the bias from loop-carried phis when propagating
	     the rank so that dependent operations are not also biased.  */
	  /* Simply walk over all SSA uses - this takes advatage of the
	     fact that non-SSA operands are is_gimple_min_invariant and
	     thus have rank 0.  */
	  rank = 0;
	  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE)
	    rank = propagate_rank (rank, op, maybe_biased_p);

	  rank += 1;
	  if (biased_p)
	    bitmap_set_bit (biased_names, SSA_NAME_VERSION (e));
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Rank for ");
	  print_generic_expr (dump_file, e);
	  fprintf (dump_file, " is %" PRId64 "\n", rank);
	}

      /* Note the rank in the hashtable so we don't recompute it.  */
      insert_operand_rank (e, rank);
      return rank;
    }

  /* Constants, globals, etc., are rank 0 */
  return 0;
}


/* We want integer ones to end up last no matter what, since they are
   the ones we can do the most with.  */
#define INTEGER_CONST_TYPE 1 << 4
#define FLOAT_ONE_CONST_TYPE 1 << 3
#define FLOAT_CONST_TYPE 1 << 2
#define OTHER_CONST_TYPE 1 << 1

/* Classify an invariant tree into integer, float, or other, so that
   we can sort them to be near other constants of the same type.  */
static inline int
constant_type (tree t)
{
  if (INTEGRAL_TYPE_P (TREE_TYPE (t)))
    return INTEGER_CONST_TYPE;
  else if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (t)))
    {
      /* Sort -1.0 and 1.0 constants last, while in some cases
	 const_binop can't optimize some inexact operations, multiplication
	 by -1.0 or 1.0 can be always merged with others.  */
      if (real_onep (t) || real_minus_onep (t))
	return FLOAT_ONE_CONST_TYPE;
      return FLOAT_CONST_TYPE;
    }
  else
    return OTHER_CONST_TYPE;
}

/* qsort comparison function to sort operand entries PA and PB by rank
   so that the sorted array is ordered by rank in decreasing order.  */
static int
sort_by_operand_rank (const void *pa, const void *pb)
{
  const operand_entry *oea = *(const operand_entry *const *)pa;
  const operand_entry *oeb = *(const operand_entry *const *)pb;

  if (oeb->rank != oea->rank)
    return oeb->rank > oea->rank ? 1 : -1;

  /* It's nicer for optimize_expression if constants that are likely
     to fold when added/multiplied/whatever are put next to each
     other.  Since all constants have rank 0, order them by type.  */
  if (oea->rank == 0)
    {
      if (constant_type (oeb->op) != constant_type (oea->op))
	return constant_type (oea->op) - constant_type (oeb->op);
      else
	/* To make sorting result stable, we use unique IDs to determine
	   order.  */
	return oeb->id > oea->id ? 1 : -1;
    }

  if (TREE_CODE (oea->op) != SSA_NAME)
    {
      if (TREE_CODE (oeb->op) != SSA_NAME)
	return oeb->id > oea->id ? 1 : -1;
      else
	return 1;
    }
  else if (TREE_CODE (oeb->op) != SSA_NAME)
    return -1;

  /* Lastly, make sure the versions that are the same go next to each
     other.  */
  if (SSA_NAME_VERSION (oeb->op) != SSA_NAME_VERSION (oea->op))
    {
      /* As SSA_NAME_VERSION is assigned pretty randomly, because we reuse
	 versions of removed SSA_NAMEs, so if possible, prefer to sort
	 based on basic block and gimple_uid of the SSA_NAME_DEF_STMT.
	 See PR60418.  */
      gimple *stmta = SSA_NAME_DEF_STMT (oea->op);
      gimple *stmtb = SSA_NAME_DEF_STMT (oeb->op);
      basic_block bba = gimple_bb (stmta);
      basic_block bbb = gimple_bb (stmtb);
      if (bbb != bba)
	{
	  /* One of the SSA_NAMEs can be defined in oeN->stmt_to_insert
	     but the other might not.  */
	  if (!bba)
	    return 1;
	  if (!bbb)
	    return -1;
	  /* If neither is, compare bb_rank.  */
	  if (bb_rank[bbb->index] != bb_rank[bba->index])
	    return (bb_rank[bbb->index] >> 16) - (bb_rank[bba->index] >> 16);
	}

      bool da = reassoc_stmt_dominates_stmt_p (stmta, stmtb);
      bool db = reassoc_stmt_dominates_stmt_p (stmtb, stmta);
      if (da != db)
	return da ? 1 : -1;

      return SSA_NAME_VERSION (oeb->op) > SSA_NAME_VERSION (oea->op) ? 1 : -1;
    }

  return oeb->id > oea->id ? 1 : -1;
}

/* Add an operand entry to *OPS for the tree operand OP.  */

static void
add_to_ops_vec (vec<operand_entry *> *ops, tree op, gimple *stmt_to_insert = NULL)
{
  operand_entry *oe = operand_entry_pool.allocate ();

  oe->op = op;
  oe->rank = get_rank (op);
  oe->id = next_operand_entry_id++;
  oe->count = 1;
  oe->stmt_to_insert = stmt_to_insert;
  ops->safe_push (oe);
}

/* Add an operand entry to *OPS for the tree operand OP with repeat
   count REPEAT.  */

static void
add_repeat_to_ops_vec (vec<operand_entry *> *ops, tree op,
		       HOST_WIDE_INT repeat)
{
  operand_entry *oe = operand_entry_pool.allocate ();

  oe->op = op;
  oe->rank = get_rank (op);
  oe->id = next_operand_entry_id++;
  oe->count = repeat;
  oe->stmt_to_insert = NULL;
  ops->safe_push (oe);

  reassociate_stats.pows_encountered++;
}

/* Returns true if we can associate the SSA def OP.  */

static bool
can_reassociate_op_p (tree op)
{
  if (TREE_CODE (op) == SSA_NAME && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op))
    return false;
  /* Make sure asm goto outputs do not participate in reassociation since
     we have no way to find an insertion place after asm goto.  */
  if (TREE_CODE (op) == SSA_NAME
      && gimple_code (SSA_NAME_DEF_STMT (op)) == GIMPLE_ASM
      && gimple_asm_nlabels (as_a <gasm *> (SSA_NAME_DEF_STMT (op))) != 0)
    return false;
  return true;
}

/* Returns true if we can reassociate operations of TYPE.
   That is for integral or non-saturating fixed-point types, and for
   floating point type when associative-math is enabled.  */

static bool
can_reassociate_type_p (tree type)
{
  if ((ANY_INTEGRAL_TYPE_P (type) && TYPE_OVERFLOW_WRAPS (type))
      || NON_SAT_FIXED_POINT_TYPE_P (type)
      || (flag_associative_math && FLOAT_TYPE_P (type)))
    return true;
  return false;
}

/* Return true if STMT is reassociable operation containing a binary
   operation with tree code CODE, and is inside LOOP.  */

static bool
is_reassociable_op (gimple *stmt, enum tree_code code, class loop *loop)
{
  basic_block bb = gimple_bb (stmt);

  if (gimple_bb (stmt) == NULL)
    return false;

  if (!flow_bb_inside_loop_p (loop, bb))
    return false;

  if (is_gimple_assign (stmt)
      && gimple_assign_rhs_code (stmt) == code
      && has_single_use (gimple_assign_lhs (stmt)))
    {
      tree rhs1 = gimple_assign_rhs1 (stmt);
      tree rhs2 = gimple_assign_rhs2 (stmt);
      if (!can_reassociate_op_p (rhs1)
	  || (rhs2 && !can_reassociate_op_p (rhs2)))
	return false;
      return true;
    }

  return false;
}


/* Return true if STMT is a nop-conversion.  */

static bool
gimple_nop_conversion_p (gimple *stmt)
{
  if (gassign *ass = dyn_cast <gassign *> (stmt))
    {
      if (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (ass))
	  && tree_nop_conversion_p (TREE_TYPE (gimple_assign_lhs (ass)),
				    TREE_TYPE (gimple_assign_rhs1 (ass))))
	return true;
    }
  return false;
}

/* Given NAME, if NAME is defined by a unary operation OPCODE, return the
   operand of the negate operation.  Otherwise, return NULL.  */

static tree
get_unary_op (tree name, enum tree_code opcode)
{
  gimple *stmt = SSA_NAME_DEF_STMT (name);

  /* Look through nop conversions (sign changes).  */
  if (gimple_nop_conversion_p (stmt)
      && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
    stmt = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));

  if (!is_gimple_assign (stmt))
    return NULL_TREE;

  if (gimple_assign_rhs_code (stmt) == opcode)
    return gimple_assign_rhs1 (stmt);
  return NULL_TREE;
}

/* Return true if OP1 and OP2 have the same value if casted to either type.  */

static bool
ops_equal_values_p (tree op1, tree op2)
{
  if (op1 == op2)
    return true;

  tree orig_op1 = op1;
  if (TREE_CODE (op1) == SSA_NAME)
    {
      gimple *stmt = SSA_NAME_DEF_STMT (op1);
      if (gimple_nop_conversion_p (stmt))
	{
	  op1 = gimple_assign_rhs1 (stmt);
	  if (op1 == op2)
	    return true;
	}
    }

  if (TREE_CODE (op2) == SSA_NAME)
    {
      gimple *stmt = SSA_NAME_DEF_STMT (op2);
      if (gimple_nop_conversion_p (stmt))
	{
	  op2 = gimple_assign_rhs1 (stmt);
	  if (op1 == op2
	      || orig_op1 == op2)
	    return true;
	}
    }

  return false;
}


/* If CURR and LAST are a pair of ops that OPCODE allows us to
   eliminate through equivalences, do so, remove them from OPS, and
   return true.  Otherwise, return false.  */

static bool
eliminate_duplicate_pair (enum tree_code opcode,
			  vec<operand_entry *> *ops,
			  bool *all_done,
			  unsigned int i,
			  operand_entry *curr,
			  operand_entry *last)
{

  /* If we have two of the same op, and the opcode is & |, min, or max,
     we can eliminate one of them.
     If we have two of the same op, and the opcode is ^, we can
     eliminate both of them.  */

  if (last && last->op == curr->op)
    {
      switch (opcode)
	{
	case MAX_EXPR:
	case MIN_EXPR:
	case BIT_IOR_EXPR:
	case BIT_AND_EXPR:
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Equivalence: ");
	      print_generic_expr (dump_file, curr->op);
	      fprintf (dump_file, " [&|minmax] ");
	      print_generic_expr (dump_file, last->op);
	      fprintf (dump_file, " -> ");
	      print_generic_stmt (dump_file, last->op);
	    }

	  ops->ordered_remove (i);
	  reassociate_stats.ops_eliminated ++;

	  return true;

	case BIT_XOR_EXPR:
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Equivalence: ");
	      print_generic_expr (dump_file, curr->op);
	      fprintf (dump_file, " ^ ");
	      print_generic_expr (dump_file, last->op);
	      fprintf (dump_file, " -> nothing\n");
	    }

	  reassociate_stats.ops_eliminated += 2;

	  if (ops->length () == 2)
	    {
	      ops->truncate (0);
	      add_to_ops_vec (ops, build_zero_cst (TREE_TYPE (last->op)));
	      *all_done = true;
	    }
	  else
	    {
	      ops->ordered_remove (i-1);
	      ops->ordered_remove (i-1);
	    }

	  return true;

	default:
	  break;
	}
    }
  return false;
}

static vec<tree> plus_negates;

/* If OPCODE is PLUS_EXPR, CURR->OP is a negate expression or a bitwise not
   expression, look in OPS for a corresponding positive operation to cancel
   it out.  If we find one, remove the other from OPS, replace
   OPS[CURRINDEX] with 0 or -1, respectively, and return true.  Otherwise,
   return false. */

static bool
eliminate_plus_minus_pair (enum tree_code opcode,
			   vec<operand_entry *> *ops,
			   unsigned int currindex,
			   operand_entry *curr)
{
  tree negateop;
  tree notop;
  unsigned int i;
  operand_entry *oe;

  if (opcode != PLUS_EXPR || TREE_CODE (curr->op) != SSA_NAME)
    return false;

  negateop = get_unary_op (curr->op, NEGATE_EXPR);
  notop = get_unary_op (curr->op, BIT_NOT_EXPR);
  if (negateop == NULL_TREE && notop == NULL_TREE)
    return false;

  /* Any non-negated version will have a rank that is one less than
     the current rank.  So once we hit those ranks, if we don't find
     one, we can stop.  */

  for (i = currindex + 1;
       ops->iterate (i, &oe)
       && oe->rank >= curr->rank - 1 ;
       i++)
    {
      if (negateop
	  && ops_equal_values_p (oe->op, negateop))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Equivalence: ");
	      print_generic_expr (dump_file, negateop);
	      fprintf (dump_file, " + -");
	      print_generic_expr (dump_file, oe->op);
	      fprintf (dump_file, " -> 0\n");
	    }

	  ops->ordered_remove (i);
	  add_to_ops_vec (ops, build_zero_cst (TREE_TYPE (oe->op)));
	  ops->ordered_remove (currindex);
	  reassociate_stats.ops_eliminated ++;

	  return true;
	}
      else if (notop
	       && ops_equal_values_p (oe->op, notop))
	{
	  tree op_type = TREE_TYPE (oe->op);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Equivalence: ");
	      print_generic_expr (dump_file, notop);
	      fprintf (dump_file, " + ~");
	      print_generic_expr (dump_file, oe->op);
	      fprintf (dump_file, " -> -1\n");
	    }

	  ops->ordered_remove (i);
	  add_to_ops_vec (ops, build_all_ones_cst (op_type));
	  ops->ordered_remove (currindex);
	  reassociate_stats.ops_eliminated ++;

	  return true;
	}
    }

  /* If CURR->OP is a negate expr without nop conversion in a plus expr: 
     save it for later inspection in repropagate_negates().  */
  if (negateop != NULL_TREE
      && gimple_assign_rhs_code (SSA_NAME_DEF_STMT (curr->op)) == NEGATE_EXPR)
    plus_negates.safe_push (curr->op);

  return false;
}

/* If OPCODE is BIT_IOR_EXPR, BIT_AND_EXPR, and, CURR->OP is really a
   bitwise not expression, look in OPS for a corresponding operand to
   cancel it out.  If we find one, remove the other from OPS, replace
   OPS[CURRINDEX] with 0, and return true.  Otherwise, return
   false. */

static bool
eliminate_not_pairs (enum tree_code opcode,
		     vec<operand_entry *> *ops,
		     unsigned int currindex,
		     operand_entry *curr)
{
  tree notop;
  unsigned int i;
  operand_entry *oe;

  if ((opcode != BIT_IOR_EXPR && opcode != BIT_AND_EXPR)
      || TREE_CODE (curr->op) != SSA_NAME)
    return false;

  notop = get_unary_op (curr->op, BIT_NOT_EXPR);
  if (notop == NULL_TREE)
    return false;

  /* Any non-not version will have a rank that is one less than
     the current rank.  So once we hit those ranks, if we don't find
     one, we can stop.  */

  for (i = currindex + 1;
       ops->iterate (i, &oe)
       && oe->rank >= curr->rank - 1;
       i++)
    {
      if (oe->op == notop)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Equivalence: ");
	      print_generic_expr (dump_file, notop);
	      if (opcode == BIT_AND_EXPR)
		fprintf (dump_file, " & ~");
	      else if (opcode == BIT_IOR_EXPR)
		fprintf (dump_file, " | ~");
	      print_generic_expr (dump_file, oe->op);
	      if (opcode == BIT_AND_EXPR)
		fprintf (dump_file, " -> 0\n");
	      else if (opcode == BIT_IOR_EXPR)
		fprintf (dump_file, " -> -1\n");
	    }

	  if (opcode == BIT_AND_EXPR)
	    oe->op = build_zero_cst (TREE_TYPE (oe->op));
	  else if (opcode == BIT_IOR_EXPR)
	    oe->op = build_all_ones_cst (TREE_TYPE (oe->op));

	  reassociate_stats.ops_eliminated += ops->length () - 1;
	  ops->truncate (0);
	  ops->quick_push (oe);
	  return true;
	}
    }

  return false;
}

/* Use constant value that may be present in OPS to try to eliminate
   operands.  Note that this function is only really used when we've
   eliminated ops for other reasons, or merged constants.  Across
   single statements, fold already does all of this, plus more.  There
   is little point in duplicating logic, so I've only included the
   identities that I could ever construct testcases to trigger.  */

static void
eliminate_using_constants (enum tree_code opcode,
			   vec<operand_entry *> *ops)
{
  operand_entry *oelast = ops->last ();
  tree type = TREE_TYPE (oelast->op);

  if (oelast->rank == 0
      && (ANY_INTEGRAL_TYPE_P (type) || FLOAT_TYPE_P (type)))
    {
      switch (opcode)
	{
	case BIT_AND_EXPR:
	  if (integer_zerop (oelast->op))
	    {
	      if (ops->length () != 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Found & 0, removing all other ops\n");

		  reassociate_stats.ops_eliminated += ops->length () - 1;

		  ops->truncate (0);
		  ops->quick_push (oelast);
		  return;
		}
	    }
	  else if (integer_all_onesp (oelast->op))
	    {
	      if (ops->length () != 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Found & -1, removing\n");
		  ops->pop ();
		  reassociate_stats.ops_eliminated++;
		}
	    }
	  break;
	case BIT_IOR_EXPR:
	  if (integer_all_onesp (oelast->op))
	    {
	      if (ops->length () != 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Found | -1, removing all other ops\n");

		  reassociate_stats.ops_eliminated += ops->length () - 1;

		  ops->truncate (0);
		  ops->quick_push (oelast);
		  return;
		}
	    }
	  else if (integer_zerop (oelast->op))
	    {
	      if (ops->length () != 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Found | 0, removing\n");
		  ops->pop ();
		  reassociate_stats.ops_eliminated++;
		}
	    }
	  break;
	case MULT_EXPR:
	  if (integer_zerop (oelast->op)
	      || (FLOAT_TYPE_P (type)
		  && !HONOR_NANS (type)
		  && !HONOR_SIGNED_ZEROS (type)
		  && real_zerop (oelast->op)))
	    {
	      if (ops->length () != 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Found * 0, removing all other ops\n");

		  reassociate_stats.ops_eliminated += ops->length () - 1;
		  ops->truncate (0);
		  ops->quick_push (oelast);
		  return;
		}
	    }
	  else if (integer_onep (oelast->op)
		   || (FLOAT_TYPE_P (type)
		       && !HONOR_SNANS (type)
		       && real_onep (oelast->op)))
	    {
	      if (ops->length () != 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Found * 1, removing\n");
		  ops->pop ();
		  reassociate_stats.ops_eliminated++;
		  return;
		}
	    }
	  break;
	case BIT_XOR_EXPR:
	case PLUS_EXPR:
	case MINUS_EXPR:
	  if (integer_zerop (oelast->op)
	      || (FLOAT_TYPE_P (type)
		  && (opcode == PLUS_EXPR || opcode == MINUS_EXPR)
		  && fold_real_zero_addition_p (type, 0, oelast->op,
						opcode == MINUS_EXPR)))
	    {
	      if (ops->length () != 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Found [|^+] 0, removing\n");
		  ops->pop ();
		  reassociate_stats.ops_eliminated++;
		  return;
		}
	    }
	  break;
	default:
	  break;
	}
    }
}


static void linearize_expr_tree (vec<operand_entry *> *, gimple *,
				 bool, bool);

/* Structure for tracking and counting operands.  */
struct oecount {
  unsigned int cnt;
  unsigned int id;
  enum tree_code oecode;
  tree op;
};


/* The heap for the oecount hashtable and the sorted list of operands.  */
static vec<oecount> cvec;


/* Oecount hashtable helpers.  */

struct oecount_hasher : int_hash <int, 0, 1>
{
  static inline hashval_t hash (int);
  static inline bool equal (int, int);
};

/* Hash function for oecount.  */

inline hashval_t
oecount_hasher::hash (int p)
{
  const oecount *c = &cvec[p - 42];
  return htab_hash_pointer (c->op) ^ (hashval_t)c->oecode;
}

/* Comparison function for oecount.  */

inline bool
oecount_hasher::equal (int p1, int p2)
{
  const oecount *c1 = &cvec[p1 - 42];
  const oecount *c2 = &cvec[p2 - 42];
  return c1->oecode == c2->oecode && c1->op == c2->op;
}

/* Comparison function for qsort sorting oecount elements by count.  */

static int
oecount_cmp (const void *p1, const void *p2)
{
  const oecount *c1 = (const oecount *)p1;
  const oecount *c2 = (const oecount *)p2;
  if (c1->cnt != c2->cnt)
    return c1->cnt > c2->cnt ? 1 : -1;
  else
    /* If counts are identical, use unique IDs to stabilize qsort.  */
    return c1->id > c2->id ? 1 : -1;
}

/* Return TRUE iff STMT represents a builtin call that raises OP
   to some exponent.  */

static bool
stmt_is_power_of_op (gimple *stmt, tree op)
{
  if (!is_gimple_call (stmt))
    return false;

  switch (gimple_call_combined_fn (stmt))
    {
    CASE_CFN_POW:
    CASE_CFN_POWI:
      return (operand_equal_p (gimple_call_arg (stmt, 0), op, 0));
      
    default:
      return false;
    }
}

/* Given STMT which is a __builtin_pow* call, decrement its exponent
   in place and return the result.  Assumes that stmt_is_power_of_op
   was previously called for STMT and returned TRUE.  */

static HOST_WIDE_INT
decrement_power (gimple *stmt)
{
  REAL_VALUE_TYPE c, cint;
  HOST_WIDE_INT power;
  tree arg1;

  switch (gimple_call_combined_fn (stmt))
    {
    CASE_CFN_POW:
      arg1 = gimple_call_arg (stmt, 1);
      c = TREE_REAL_CST (arg1);
      power = real_to_integer (&c) - 1;
      real_from_integer (&cint, VOIDmode, power, SIGNED);
      gimple_call_set_arg (stmt, 1, build_real (TREE_TYPE (arg1), cint));
      return power;

    CASE_CFN_POWI:
      arg1 = gimple_call_arg (stmt, 1);
      power = TREE_INT_CST_LOW (arg1) - 1;
      gimple_call_set_arg (stmt, 1, build_int_cst (TREE_TYPE (arg1), power));
      return power;

    default:
      gcc_unreachable ();
    }
}

/* Replace SSA defined by STMT and replace all its uses with new
   SSA.  Also return the new SSA.  */

static tree
make_new_ssa_for_def (gimple *stmt, enum tree_code opcode, tree op)
{
  gimple *use_stmt;
  use_operand_p use;
  imm_use_iterator iter;
  tree new_lhs, new_debug_lhs = NULL_TREE;
  tree lhs = gimple_get_lhs (stmt);

  new_lhs = make_ssa_name (TREE_TYPE (lhs));
  gimple_set_lhs (stmt, new_lhs);

  /* Also need to update GIMPLE_DEBUGs.  */
  FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
    {
      tree repl = new_lhs;
      if (is_gimple_debug (use_stmt))
	{
	  if (new_debug_lhs == NULL_TREE)
	    {
	      new_debug_lhs = build_debug_expr_decl (TREE_TYPE (lhs));
	      gdebug *def_temp
		= gimple_build_debug_bind (new_debug_lhs,
					   build2 (opcode, TREE_TYPE (lhs),
						   new_lhs, op),
					   stmt);
	      gimple_set_uid (def_temp, gimple_uid (stmt));
	      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	      gsi_insert_after (&gsi, def_temp, GSI_SAME_STMT);
	    }
	  repl = new_debug_lhs;
	}
      FOR_EACH_IMM_USE_ON_STMT (use, iter)
	SET_USE (use, repl);
      update_stmt (use_stmt);
    }
  return new_lhs;
}

/* Replace all SSAs defined in STMTS_TO_FIX and replace its
   uses with new SSAs.  Also do this for the stmt that defines DEF
   if *DEF is not OP.  */

static void
make_new_ssa_for_all_defs (tree *def, enum tree_code opcode, tree op,
			   vec<gimple *> &stmts_to_fix)
{
  unsigned i;
  gimple *stmt;

  if (*def != op
      && TREE_CODE (*def) == SSA_NAME
      && (stmt = SSA_NAME_DEF_STMT (*def))
      && gimple_code (stmt) != GIMPLE_NOP)
    *def = make_new_ssa_for_def (stmt, opcode, op);

  FOR_EACH_VEC_ELT (stmts_to_fix, i, stmt)
    make_new_ssa_for_def (stmt, opcode, op);
}

/* Find the single immediate use of STMT's LHS, and replace it
   with OP.  Remove STMT.  If STMT's LHS is the same as *DEF,
   replace *DEF with OP as well.  */

static void
propagate_op_to_single_use (tree op, gimple *stmt, tree *def)
{
  tree lhs;
  gimple *use_stmt;
  use_operand_p use;
  gimple_stmt_iterator gsi;

  if (is_gimple_call (stmt))
    lhs = gimple_call_lhs (stmt);
  else
    lhs = gimple_assign_lhs (stmt);

  gcc_assert (has_single_use (lhs));
  single_imm_use (lhs, &use, &use_stmt);
  if (lhs == *def)
    *def = op;
  SET_USE (use, op);
  if (TREE_CODE (op) != SSA_NAME)
    update_stmt (use_stmt);
  gsi = gsi_for_stmt (stmt);
  unlink_stmt_vdef (stmt);
  reassoc_remove_stmt (&gsi);
  release_defs (stmt);
}

/* Walks the linear chain with result *DEF searching for an operation
   with operand OP and code OPCODE removing that from the chain.  *DEF
   is updated if there is only one operand but no operation left.  */

static void
zero_one_operation (tree *def, enum tree_code opcode, tree op)
{
  tree orig_def = *def;
  gimple *stmt = SSA_NAME_DEF_STMT (*def);
  /* PR72835 - Record the stmt chain that has to be updated such that
     we dont use the same LHS when the values computed are different.  */
  auto_vec<gimple *, 64> stmts_to_fix;

  do
    {
      tree name;

      if (opcode == MULT_EXPR)
	{
	  if (stmt_is_power_of_op (stmt, op))
	    {
	      if (decrement_power (stmt) == 1)
		{
		  if (stmts_to_fix.length () > 0)
		    stmts_to_fix.pop ();
		  propagate_op_to_single_use (op, stmt, def);
		}
	      break;
	    }
	  else if (gimple_assign_rhs_code (stmt) == NEGATE_EXPR)
	    {
	      if (gimple_assign_rhs1 (stmt) == op)
		{
		  tree cst = build_minus_one_cst (TREE_TYPE (op));
		  if (stmts_to_fix.length () > 0)
		    stmts_to_fix.pop ();
		  propagate_op_to_single_use (cst, stmt, def);
		  break;
		}
	      else if (integer_minus_onep (op)
		       || real_minus_onep (op))
		{
		  gimple_assign_set_rhs_code
		    (stmt, TREE_CODE (gimple_assign_rhs1 (stmt)));
		  break;
		}
	    }
	}

      name = gimple_assign_rhs1 (stmt);

      /* If this is the operation we look for and one of the operands
         is ours simply propagate the other operand into the stmts
	 single use.  */
      if (gimple_assign_rhs_code (stmt) == opcode
	  && (name == op
	      || gimple_assign_rhs2 (stmt) == op))
	{
	  if (name == op)
	    name = gimple_assign_rhs2 (stmt);
	  if (stmts_to_fix.length () > 0)
	    stmts_to_fix.pop ();
	  propagate_op_to_single_use (name, stmt, def);
	  break;
	}

      /* We might have a multiply of two __builtin_pow* calls, and
	 the operand might be hiding in the rightmost one.  Likewise
	 this can happen for a negate.  */
      if (opcode == MULT_EXPR
	  && gimple_assign_rhs_code (stmt) == opcode
	  && TREE_CODE (gimple_assign_rhs2 (stmt)) == SSA_NAME
	  && has_single_use (gimple_assign_rhs2 (stmt)))
	{
	  gimple *stmt2 = SSA_NAME_DEF_STMT (gimple_assign_rhs2 (stmt));
	  if (stmt_is_power_of_op (stmt2, op))
	    {
	      if (decrement_power (stmt2) == 1)
		propagate_op_to_single_use (op, stmt2, def);
	      else
		stmts_to_fix.safe_push (stmt2);
	      break;
	    }
	  else if (is_gimple_assign (stmt2)
		   && gimple_assign_rhs_code (stmt2) == NEGATE_EXPR)
	    {
	      if (gimple_assign_rhs1 (stmt2) == op)
		{
		  tree cst = build_minus_one_cst (TREE_TYPE (op));
		  propagate_op_to_single_use (cst, stmt2, def);
		  break;
		}
	      else if (integer_minus_onep (op)
		       || real_minus_onep (op))
		{
		  stmts_to_fix.safe_push (stmt2);
		  gimple_assign_set_rhs_code
		    (stmt2, TREE_CODE (gimple_assign_rhs1 (stmt2)));
		  break;
		}
	    }
	}

      /* Continue walking the chain.  */
      gcc_assert (name != op
		  && TREE_CODE (name) == SSA_NAME);
      stmt = SSA_NAME_DEF_STMT (name);
      stmts_to_fix.safe_push (stmt);
    }
  while (1);

  if (stmts_to_fix.length () > 0 || *def == orig_def)
    make_new_ssa_for_all_defs (def, opcode, op, stmts_to_fix);
}

/* Returns true if statement S1 dominates statement S2.  Like
   stmt_dominates_stmt_p, but uses stmt UIDs to optimize.  */

static bool
reassoc_stmt_dominates_stmt_p (gimple *s1, gimple *s2)
{
  basic_block bb1 = gimple_bb (s1), bb2 = gimple_bb (s2);

  /* If bb1 is NULL, it should be a GIMPLE_NOP def stmt of an (D)
     SSA_NAME.  Assume it lives at the beginning of function and
     thus dominates everything.  */
  if (!bb1 || s1 == s2)
    return true;

  /* If bb2 is NULL, it doesn't dominate any stmt with a bb.  */
  if (!bb2)
    return false;

  if (bb1 == bb2)
    {
      /* PHIs in the same basic block are assumed to be
	 executed all in parallel, if only one stmt is a PHI,
	 it dominates the other stmt in the same basic block.  */
      if (gimple_code (s1) == GIMPLE_PHI)
	return true;

      if (gimple_code (s2) == GIMPLE_PHI)
	return false;

      gcc_assert (gimple_uid (s1) && gimple_uid (s2));

      if (gimple_uid (s1) < gimple_uid (s2))
	return true;

      if (gimple_uid (s1) > gimple_uid (s2))
	return false;

      gimple_stmt_iterator gsi = gsi_for_stmt (s1);
      unsigned int uid = gimple_uid (s1);
      for (gsi_next (&gsi); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *s = gsi_stmt (gsi);
	  if (gimple_uid (s) != uid)
	    break;
	  if (s == s2)
	    return true;
	}

      return false;
    }

  return dominated_by_p (CDI_DOMINATORS, bb2, bb1);
}

/* Insert STMT after INSERT_POINT.  */

static void
insert_stmt_after (gimple *stmt, gimple *insert_point)
{
  gimple_stmt_iterator gsi;
  basic_block bb;

  if (gimple_code (insert_point) == GIMPLE_PHI)
    bb = gimple_bb (insert_point);
  else if (!stmt_ends_bb_p (insert_point))
    {
      gsi = gsi_for_stmt (insert_point);
      gimple_set_uid (stmt, gimple_uid (insert_point));
      gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
      return;
    }
  else if (gimple_code (insert_point) == GIMPLE_ASM
	   && gimple_asm_nlabels (as_a <gasm *> (insert_point)) != 0)
    /* We have no idea where to insert - it depends on where the
       uses will be placed.  */
    gcc_unreachable ();
  else
    /* We assume INSERT_POINT is a SSA_NAME_DEF_STMT of some SSA_NAME,
       thus if it must end a basic block, it should be a call that can
       throw, or some assignment that can throw.  If it throws, the LHS
       of it will not be initialized though, so only valid places using
       the SSA_NAME should be dominated by the fallthru edge.  */
    bb = find_fallthru_edge (gimple_bb (insert_point)->succs)->dest;
  gsi = gsi_after_labels (bb);
  if (gsi_end_p (gsi))
    {
      gimple_stmt_iterator gsi2 = gsi_last_bb (bb);
      gimple_set_uid (stmt,
		      gsi_end_p (gsi2) ? 1 : gimple_uid (gsi_stmt (gsi2)));
    }
  else
    gimple_set_uid (stmt, gimple_uid (gsi_stmt (gsi)));
  gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
}

/* Builds one statement performing OP1 OPCODE OP2 using TMPVAR for
   the result.  Places the statement after the definition of either
   OP1 or OP2.  Returns the new statement.  */

static gimple *
build_and_add_sum (tree type, tree op1, tree op2, enum tree_code opcode)
{
  gimple *op1def = NULL, *op2def = NULL;
  gimple_stmt_iterator gsi;
  tree op;
  gassign *sum;

  /* Create the addition statement.  */
  op = make_ssa_name (type);
  sum = gimple_build_assign (op, opcode, op1, op2);

  /* Find an insertion place and insert.  */
  if (TREE_CODE (op1) == SSA_NAME)
    op1def = SSA_NAME_DEF_STMT (op1);
  if (TREE_CODE (op2) == SSA_NAME)
    op2def = SSA_NAME_DEF_STMT (op2);
  if ((!op1def || gimple_nop_p (op1def))
      && (!op2def || gimple_nop_p (op2def)))
    {
      gsi = gsi_after_labels (single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun)));
      if (gsi_end_p (gsi))
	{
	  gimple_stmt_iterator gsi2
	    = gsi_last_bb (single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun)));
	  gimple_set_uid (sum,
			  gsi_end_p (gsi2) ? 1 : gimple_uid (gsi_stmt (gsi2)));
	}
      else
	gimple_set_uid (sum, gimple_uid (gsi_stmt (gsi)));
      gsi_insert_before (&gsi, sum, GSI_NEW_STMT);
    }
  else
    {
      gimple *insert_point;
      if ((!op1def || gimple_nop_p (op1def))
	   || (op2def && !gimple_nop_p (op2def)
	       && reassoc_stmt_dominates_stmt_p (op1def, op2def)))
	insert_point = op2def;
      else
	insert_point = op1def;
      insert_stmt_after (sum, insert_point);
    }
  update_stmt (sum);

  return sum;
}

/* Perform un-distribution of divisions and multiplications.
   A * X + B * X is transformed into (A + B) * X and A / X + B / X
   to (A + B) / X for real X.

   The algorithm is organized as follows.

    - First we walk the addition chain *OPS looking for summands that
      are defined by a multiplication or a real division.  This results
      in the candidates bitmap with relevant indices into *OPS.

    - Second we build the chains of multiplications or divisions for
      these candidates, counting the number of occurrences of (operand, code)
      pairs in all of the candidates chains.

    - Third we sort the (operand, code) pairs by number of occurrence and
      process them starting with the pair with the most uses.

      * For each such pair we walk the candidates again to build a
        second candidate bitmap noting all multiplication/division chains
	that have at least one occurrence of (operand, code).

      * We build an alternate addition chain only covering these
        candidates with one (operand, code) operation removed from their
	multiplication/division chain.

      * The first candidate gets replaced by the alternate addition chain
        multiplied/divided by the operand.

      * All candidate chains get disabled for further processing and
        processing of (operand, code) pairs continues.

  The alternate addition chains built are re-processed by the main
  reassociation algorithm which allows optimizing a * x * y + b * y * x
  to (a + b ) * x * y in one invocation of the reassociation pass.  */

static bool
undistribute_ops_list (enum tree_code opcode,
		       vec<operand_entry *> *ops, class loop *loop)
{
  unsigned int length = ops->length ();
  operand_entry *oe1;
  unsigned i, j;
  unsigned nr_candidates, nr_candidates2;
  sbitmap_iterator sbi0;
  vec<operand_entry *> *subops;
  bool changed = false;
  unsigned int next_oecount_id = 0;

  if (length <= 1
      || opcode != PLUS_EXPR)
    return false;

  /* Build a list of candidates to process.  */
  auto_sbitmap candidates (length);
  bitmap_clear (candidates);
  nr_candidates = 0;
  FOR_EACH_VEC_ELT (*ops, i, oe1)
    {
      enum tree_code dcode;
      gimple *oe1def;

      if (TREE_CODE (oe1->op) != SSA_NAME)
	continue;
      oe1def = SSA_NAME_DEF_STMT (oe1->op);
      if (!is_gimple_assign (oe1def))
	continue;
      dcode = gimple_assign_rhs_code (oe1def);
      if ((dcode != MULT_EXPR
	   && dcode != RDIV_EXPR)
	  || !is_reassociable_op (oe1def, dcode, loop))
	continue;

      bitmap_set_bit (candidates, i);
      nr_candidates++;
    }

  if (nr_candidates < 2)
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "searching for un-distribute opportunities ");
      print_generic_expr (dump_file,
	(*ops)[bitmap_first_set_bit (candidates)]->op, TDF_NONE);
      fprintf (dump_file, " %d\n", nr_candidates);
    }

  /* Build linearized sub-operand lists and the counting table.  */
  cvec.create (0);

  hash_table<oecount_hasher> ctable (15);

  /* ??? Macro arguments cannot have multi-argument template types in
     them.  This typedef is needed to workaround that limitation.  */
  typedef vec<operand_entry *> vec_operand_entry_t_heap;
  subops = XCNEWVEC (vec_operand_entry_t_heap, ops->length ());
  EXECUTE_IF_SET_IN_BITMAP (candidates, 0, i, sbi0)
    {
      gimple *oedef;
      enum tree_code oecode;
      unsigned j;

      oedef = SSA_NAME_DEF_STMT ((*ops)[i]->op);
      oecode = gimple_assign_rhs_code (oedef);
      linearize_expr_tree (&subops[i], oedef,
			   associative_tree_code (oecode), false);

      FOR_EACH_VEC_ELT (subops[i], j, oe1)
	{
	  oecount c;
	  int *slot;
	  int idx;
	  c.oecode = oecode;
	  c.cnt = 1;
	  c.id = next_oecount_id++;
	  c.op = oe1->op;
	  cvec.safe_push (c);
	  idx = cvec.length () + 41;
	  slot = ctable.find_slot (idx, INSERT);
	  if (!*slot)
	    {
	      *slot = idx;
	    }
	  else
	    {
	      cvec.pop ();
	      cvec[*slot - 42].cnt++;
	    }
	}
    }

  /* Sort the counting table.  */
  cvec.qsort (oecount_cmp);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      oecount *c;
      fprintf (dump_file, "Candidates:\n");
      FOR_EACH_VEC_ELT (cvec, j, c)
	{
	  fprintf (dump_file, "  %u %s: ", c->cnt,
		   c->oecode == MULT_EXPR
		   ? "*" : c->oecode == RDIV_EXPR ? "/" : "?");
	  print_generic_expr (dump_file, c->op);
	  fprintf (dump_file, "\n");
	}
    }

  /* Process the (operand, code) pairs in order of most occurrence.  */
  auto_sbitmap candidates2 (length);
  while (!cvec.is_empty ())
    {
      oecount *c = &cvec.last ();
      if (c->cnt < 2)
	break;

      /* Now collect the operands in the outer chain that contain
         the common operand in their inner chain.  */
      bitmap_clear (candidates2);
      nr_candidates2 = 0;
      EXECUTE_IF_SET_IN_BITMAP (candidates, 0, i, sbi0)
	{
	  gimple *oedef;
	  enum tree_code oecode;
	  unsigned j;
	  tree op = (*ops)[i]->op;

	  /* If we undistributed in this chain already this may be
	     a constant.  */
	  if (TREE_CODE (op) != SSA_NAME)
	    continue;

	  oedef = SSA_NAME_DEF_STMT (op);
	  oecode = gimple_assign_rhs_code (oedef);
	  if (oecode != c->oecode)
	    continue;

	  FOR_EACH_VEC_ELT (subops[i], j, oe1)
	    {
	      if (oe1->op == c->op)
		{
		  bitmap_set_bit (candidates2, i);
		  ++nr_candidates2;
		  break;
		}
	    }
	}

      if (nr_candidates2 >= 2)
	{
	  operand_entry *oe1, *oe2;
	  gimple *prod;
	  int first = bitmap_first_set_bit (candidates2);

	  /* Build the new addition chain.  */
	  oe1 = (*ops)[first];
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Building (");
	      print_generic_expr (dump_file, oe1->op);
	    }
	  zero_one_operation (&oe1->op, c->oecode, c->op);
	  EXECUTE_IF_SET_IN_BITMAP (candidates2, first+1, i, sbi0)
	    {
	      gimple *sum;
	      oe2 = (*ops)[i];
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, " + ");
		  print_generic_expr (dump_file, oe2->op);
		}
	      zero_one_operation (&oe2->op, c->oecode, c->op);
	      sum = build_and_add_sum (TREE_TYPE (oe1->op),
				       oe1->op, oe2->op, opcode);
	      oe2->op = build_zero_cst (TREE_TYPE (oe2->op));
	      oe2->rank = 0;
	      oe1->op = gimple_get_lhs (sum);
	    }

	  /* Apply the multiplication/division.  */
	  prod = build_and_add_sum (TREE_TYPE (oe1->op),
				    oe1->op, c->op, c->oecode);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, ") %s ", c->oecode == MULT_EXPR ? "*" : "/");
	      print_generic_expr (dump_file, c->op);
	      fprintf (dump_file, "\n");
	    }

	  /* Record it in the addition chain and disable further
	     undistribution with this op.  */
	  oe1->op = gimple_assign_lhs (prod);
	  oe1->rank = get_rank (oe1->op);
	  subops[first].release ();

	  changed = true;
	}

      cvec.pop ();
    }

  for (i = 0; i < ops->length (); ++i)
    subops[i].release ();
  free (subops);
  cvec.release ();

  return changed;
}

/* Pair to hold the information of one specific VECTOR_TYPE SSA_NAME:
   first: element index for each relevant BIT_FIELD_REF.
   second: the index of vec ops* for each relevant BIT_FIELD_REF.  */
typedef std::pair<unsigned, unsigned> v_info_elem;
struct v_info {
  tree vec_type;
  auto_vec<v_info_elem, 32> vec;
};
typedef v_info *v_info_ptr;

/* Comparison function for qsort on VECTOR SSA_NAME trees by machine mode.  */
static int
sort_by_mach_mode (const void *p_i, const void *p_j)
{
  const tree tr1 = *((const tree *) p_i);
  const tree tr2 = *((const tree *) p_j);
  unsigned int mode1 = TYPE_MODE (TREE_TYPE (tr1));
  unsigned int mode2 = TYPE_MODE (TREE_TYPE (tr2));
  if (mode1 > mode2)
    return 1;
  else if (mode1 < mode2)
    return -1;
  if (SSA_NAME_VERSION (tr1) < SSA_NAME_VERSION (tr2))
    return -1;
  else if (SSA_NAME_VERSION (tr1) > SSA_NAME_VERSION (tr2))
    return 1;
  return 0;
}

/* Cleanup hash map for VECTOR information.  */
static void
cleanup_vinfo_map (hash_map<tree, v_info_ptr> &info_map)
{
  for (hash_map<tree, v_info_ptr>::iterator it = info_map.begin ();
       it != info_map.end (); ++it)
    {
      v_info_ptr info = (*it).second;
      delete info;
      (*it).second = NULL;
    }
}

/* Perform un-distribution of BIT_FIELD_REF on VECTOR_TYPE.
     V1[0] + V1[1] + ... + V1[k] + V2[0] + V2[1] + ... + V2[k] + ... Vn[k]
   is transformed to
     Vs = (V1 + V2 + ... + Vn)
     Vs[0] + Vs[1] + ... + Vs[k]

   The basic steps are listed below:

    1) Check the addition chain *OPS by looking those summands coming from
       VECTOR bit_field_ref on VECTOR type.  Put the information into
       v_info_map for each satisfied summand, using VECTOR SSA_NAME as key.

    2) For each key (VECTOR SSA_NAME), validate all its BIT_FIELD_REFs are
       continuous, they can cover the whole VECTOR perfectly without any holes.
       Obtain one VECTOR list which contain candidates to be transformed.

    3) Sort the VECTOR list by machine mode of VECTOR type, for each group of
       candidates with same mode, build the addition statements for them and
       generate BIT_FIELD_REFs accordingly.

   TODO:
       The current implementation requires the whole VECTORs should be fully
       covered, but it can be extended to support partial, checking adjacent
       but not fill the whole, it may need some cost model to define the
       boundary to do or not.
*/
static bool
undistribute_bitref_for_vector (enum tree_code opcode,
				vec<operand_entry *> *ops, struct loop *loop)
{
  if (ops->length () <= 1)
    return false;

  if (opcode != PLUS_EXPR
      && opcode != MULT_EXPR
      && opcode != BIT_XOR_EXPR
      && opcode != BIT_IOR_EXPR
      && opcode != BIT_AND_EXPR)
    return false;

  hash_map<tree, v_info_ptr> v_info_map;
  operand_entry *oe1;
  unsigned i;

  /* Find those summands from VECTOR BIT_FIELD_REF in addition chain, put the
     information into map.  */
  FOR_EACH_VEC_ELT (*ops, i, oe1)
    {
      enum tree_code dcode;
      gimple *oe1def;

      if (TREE_CODE (oe1->op) != SSA_NAME)
	continue;
      oe1def = SSA_NAME_DEF_STMT (oe1->op);
      if (!is_gimple_assign (oe1def))
	continue;
      dcode = gimple_assign_rhs_code (oe1def);
      if (dcode != BIT_FIELD_REF || !is_reassociable_op (oe1def, dcode, loop))
	continue;

      tree rhs = gimple_assign_rhs1 (oe1def);
      tree vec = TREE_OPERAND (rhs, 0);
      tree vec_type = TREE_TYPE (vec);

      if (TREE_CODE (vec) != SSA_NAME || !VECTOR_TYPE_P (vec_type))
	continue;

      /* Ignore it if target machine can't support this VECTOR type.  */
      if (!VECTOR_MODE_P (TYPE_MODE (vec_type)))
	continue;

      /* Check const vector type, constrain BIT_FIELD_REF offset and size.  */
      if (!TYPE_VECTOR_SUBPARTS (vec_type).is_constant ())
	continue;

      if (VECTOR_TYPE_P (TREE_TYPE (rhs))
	  || !is_a <scalar_mode> (TYPE_MODE (TREE_TYPE (rhs))))
	continue;

      /* The type of BIT_FIELD_REF might not be equal to the element type of
	 the vector.  We want to use a vector type with element type the
	 same as the BIT_FIELD_REF and size the same as TREE_TYPE (vec).  */
      if (!useless_type_conversion_p (TREE_TYPE (rhs), TREE_TYPE (vec_type)))
	{
	  machine_mode simd_mode;
	  unsigned HOST_WIDE_INT size, nunits;
	  unsigned HOST_WIDE_INT elem_size
	    = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (rhs)));
	  if (!GET_MODE_BITSIZE (TYPE_MODE (vec_type)).is_constant (&size))
	    continue;
	  if (size <= elem_size || (size % elem_size) != 0)
	    continue;
	  nunits = size / elem_size;
	  if (!mode_for_vector (SCALAR_TYPE_MODE (TREE_TYPE (rhs)),
				nunits).exists (&simd_mode))
	    continue;
	  vec_type = build_vector_type_for_mode (TREE_TYPE (rhs), simd_mode);

	  /* Ignore it if target machine can't support this VECTOR type.  */
	  if (!VECTOR_MODE_P (TYPE_MODE (vec_type)))
	    continue;

	  /* Check const vector type, constrain BIT_FIELD_REF offset and
	     size.  */
	  if (!TYPE_VECTOR_SUBPARTS (vec_type).is_constant ())
	    continue;

	  if (maybe_ne (GET_MODE_SIZE (TYPE_MODE (vec_type)),
			GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (vec)))))
	    continue;
	}

      tree elem_type = TREE_TYPE (vec_type);
      unsigned HOST_WIDE_INT elem_size = tree_to_uhwi (TYPE_SIZE (elem_type));
      if (maybe_ne (bit_field_size (rhs), elem_size))
	continue;

      unsigned idx;
      if (!constant_multiple_p (bit_field_offset (rhs), elem_size, &idx))
	continue;

      /* Ignore it if target machine can't support this type of VECTOR
         operation.  */
      optab op_tab = optab_for_tree_code (opcode, vec_type, optab_vector);
      if (optab_handler (op_tab, TYPE_MODE (vec_type)) == CODE_FOR_nothing)
	continue;

      bool existed;
      v_info_ptr &info = v_info_map.get_or_insert (vec, &existed);
      if (!existed)
	{
	  info = new v_info;
	  info->vec_type = vec_type;
	}
      else if (!types_compatible_p (vec_type, info->vec_type))
	continue;
      info->vec.safe_push (std::make_pair (idx, i));
    }

  /* At least two VECTOR to combine.  */
  if (v_info_map.elements () <= 1)
    {
      cleanup_vinfo_map (v_info_map);
      return false;
    }

  /* Verify all VECTOR candidates by checking two conditions:
       1) sorted offsets are adjacent, no holes.
       2) can fill the whole VECTOR perfectly.
     And add the valid candidates to a vector for further handling.  */
  auto_vec<tree> valid_vecs (v_info_map.elements ());
  for (hash_map<tree, v_info_ptr>::iterator it = v_info_map.begin ();
       it != v_info_map.end (); ++it)
    {
      tree cand_vec = (*it).first;
      v_info_ptr cand_info = (*it).second;
      unsigned int num_elems
	= TYPE_VECTOR_SUBPARTS (cand_info->vec_type).to_constant ();
      if (cand_info->vec.length () != num_elems)
	continue;
      sbitmap holes = sbitmap_alloc (num_elems);
      bitmap_ones (holes);
      bool valid = true;
      v_info_elem *curr;
      FOR_EACH_VEC_ELT (cand_info->vec, i, curr)
	{
	  if (!bitmap_bit_p (holes, curr->first))
	    {
	      valid = false;
	      break;
	    }
	  else
	    bitmap_clear_bit (holes, curr->first);
	}
      if (valid && bitmap_empty_p (holes))
	valid_vecs.quick_push (cand_vec);
      sbitmap_free (holes);
    }

  /* At least two VECTOR to combine.  */
  if (valid_vecs.length () <= 1)
    {
      cleanup_vinfo_map (v_info_map);
      return false;
    }

  valid_vecs.qsort (sort_by_mach_mode);
  /* Go through all candidates by machine mode order, query the mode_to_total
     to get the total number for each mode and skip the single one.  */
  for (unsigned i = 0; i < valid_vecs.length () - 1; ++i)
    {
      tree tvec = valid_vecs[i];
      enum machine_mode mode = TYPE_MODE (TREE_TYPE (tvec));

      /* Skip modes with only a single candidate.  */
      if (TYPE_MODE (TREE_TYPE (valid_vecs[i + 1])) != mode)
	continue;

      unsigned int idx, j;
      gimple *sum = NULL;
      tree sum_vec = tvec;
      v_info_ptr info_ptr = *(v_info_map.get (tvec));
      v_info_elem *elem;
      tree vec_type = info_ptr->vec_type;

      /* Build the sum for all candidates with same mode.  */
      do
	{
	  sum = build_and_add_sum (vec_type, sum_vec,
				   valid_vecs[i + 1], opcode);
	  if (!useless_type_conversion_p (vec_type,
					  TREE_TYPE (valid_vecs[i + 1])))
	    {
	      /* Update the operands only after build_and_add_sum,
		 so that we don't have to repeat the placement algorithm
		 of build_and_add_sum.  */
	      gimple_stmt_iterator gsi = gsi_for_stmt (sum);
	      tree vce = build1 (VIEW_CONVERT_EXPR, vec_type,
				 valid_vecs[i + 1]);
	      tree lhs = make_ssa_name (vec_type);
	      gimple *g = gimple_build_assign (lhs, VIEW_CONVERT_EXPR, vce);
	      gimple_set_uid (g, gimple_uid (sum));
	      gsi_insert_before (&gsi, g, GSI_NEW_STMT);
	      gimple_assign_set_rhs2 (sum, lhs);
	      if (sum_vec == tvec)
		{
		  vce = build1 (VIEW_CONVERT_EXPR, vec_type, sum_vec);
		  lhs = make_ssa_name (vec_type);
		  g = gimple_build_assign (lhs, VIEW_CONVERT_EXPR, vce);
		  gimple_set_uid (g, gimple_uid (sum));
		  gsi_insert_before (&gsi, g, GSI_NEW_STMT);
		  gimple_assign_set_rhs1 (sum, lhs);
		}
	      update_stmt (sum);
	    }
	  sum_vec = gimple_get_lhs (sum);
	  info_ptr = *(v_info_map.get (valid_vecs[i + 1]));
	  gcc_assert (types_compatible_p (vec_type, info_ptr->vec_type));
	  /* Update those related ops of current candidate VECTOR.  */
	  FOR_EACH_VEC_ELT (info_ptr->vec, j, elem)
	    {
	      idx = elem->second;
	      gimple *def = SSA_NAME_DEF_STMT ((*ops)[idx]->op);
	      /* Set this then op definition will get DCEd later.  */
	      gimple_set_visited (def, true);
	      if (opcode == PLUS_EXPR
		  || opcode == BIT_XOR_EXPR
		  || opcode == BIT_IOR_EXPR)
		(*ops)[idx]->op = build_zero_cst (TREE_TYPE ((*ops)[idx]->op));
	      else if (opcode == MULT_EXPR)
		(*ops)[idx]->op = build_one_cst (TREE_TYPE ((*ops)[idx]->op));
	      else
		{
		  gcc_assert (opcode == BIT_AND_EXPR);
		  (*ops)[idx]->op
		    = build_all_ones_cst (TREE_TYPE ((*ops)[idx]->op));
		}
	      (*ops)[idx]->rank = 0;
	    }
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Generating addition -> ");
	      print_gimple_stmt (dump_file, sum, 0);
	    }
	  i++;
	}
      while ((i < valid_vecs.length () - 1)
	     && TYPE_MODE (TREE_TYPE (valid_vecs[i + 1])) == mode);

      /* Referring to first valid VECTOR with this mode, generate the
         BIT_FIELD_REF statements accordingly.  */
      info_ptr = *(v_info_map.get (tvec));
      gcc_assert (sum);
      tree elem_type = TREE_TYPE (vec_type);
      FOR_EACH_VEC_ELT (info_ptr->vec, j, elem)
	{
	  idx = elem->second;
	  tree dst = make_ssa_name (elem_type);
	  tree pos = bitsize_int (elem->first
				  * tree_to_uhwi (TYPE_SIZE (elem_type)));
	  tree bfr = build3 (BIT_FIELD_REF, elem_type, sum_vec,
			     TYPE_SIZE (elem_type), pos);
	  gimple *gs = gimple_build_assign (dst, BIT_FIELD_REF, bfr);
	  insert_stmt_after (gs, sum);
	  gimple *def = SSA_NAME_DEF_STMT ((*ops)[idx]->op);
	  /* Set this then op definition will get DCEd later.  */
	  gimple_set_visited (def, true);
	  (*ops)[idx]->op = gimple_assign_lhs (gs);
	  (*ops)[idx]->rank = get_rank ((*ops)[idx]->op);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Generating bit_field_ref -> ");
	      print_gimple_stmt (dump_file, gs, 0);
	    }
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "undistributiong bit_field_ref for vector done.\n");

  cleanup_vinfo_map (v_info_map);

  return true;
}

/* If OPCODE is BIT_IOR_EXPR or BIT_AND_EXPR and CURR is a comparison
   expression, examine the other OPS to see if any of them are comparisons
   of the same values, which we may be able to combine or eliminate.
   For example, we can rewrite (a < b) | (a == b) as (a <= b).  */

static bool
eliminate_redundant_comparison (enum tree_code opcode,
				vec<operand_entry *> *ops,
				unsigned int currindex,
				operand_entry *curr)
{
  tree op1, op2;
  enum tree_code lcode, rcode;
  gimple *def1, *def2;
  int i;
  operand_entry *oe;

  if (opcode != BIT_IOR_EXPR && opcode != BIT_AND_EXPR)
    return false;

  /* Check that CURR is a comparison.  */
  if (TREE_CODE (curr->op) != SSA_NAME)
    return false;
  def1 = SSA_NAME_DEF_STMT (curr->op);
  if (!is_gimple_assign (def1))
    return false;
  lcode = gimple_assign_rhs_code (def1);
  if (TREE_CODE_CLASS (lcode) != tcc_comparison)
    return false;
  op1 = gimple_assign_rhs1 (def1);
  op2 = gimple_assign_rhs2 (def1);

  /* Now look for a similar comparison in the remaining OPS.  */
  for (i = currindex + 1; ops->iterate (i, &oe); i++)
    {
      tree t;

      if (TREE_CODE (oe->op) != SSA_NAME)
	continue;
      def2 = SSA_NAME_DEF_STMT (oe->op);
      if (!is_gimple_assign (def2))
	continue;
      rcode = gimple_assign_rhs_code (def2);
      if (TREE_CODE_CLASS (rcode) != tcc_comparison)
	continue;

      /* If we got here, we have a match.  See if we can combine the
	 two comparisons.  */
      tree type = TREE_TYPE (gimple_assign_lhs (def1));
      if (opcode == BIT_IOR_EXPR)
	t = maybe_fold_or_comparisons (type,
				       lcode, op1, op2,
				       rcode, gimple_assign_rhs1 (def2),
				       gimple_assign_rhs2 (def2));
      else
	t = maybe_fold_and_comparisons (type,
					lcode, op1, op2,
					rcode, gimple_assign_rhs1 (def2),
					gimple_assign_rhs2 (def2));
      if (!t)
	continue;

      /* maybe_fold_and_comparisons and maybe_fold_or_comparisons
	 always give us a boolean_type_node value back.  If the original
	 BIT_AND_EXPR or BIT_IOR_EXPR was of a wider integer type,
	 we need to convert.  */
      if (!useless_type_conversion_p (TREE_TYPE (curr->op), TREE_TYPE (t)))
	{
	  if (!fold_convertible_p (TREE_TYPE (curr->op), t))
	    continue;
	  t = fold_convert (TREE_TYPE (curr->op), t);
	}

      if (TREE_CODE (t) != INTEGER_CST
	  && !operand_equal_p (t, curr->op, 0))
	{
	  enum tree_code subcode;
	  tree newop1, newop2;
	  if (!COMPARISON_CLASS_P (t))
	    continue;
	  extract_ops_from_tree (t, &subcode, &newop1, &newop2);
	  STRIP_USELESS_TYPE_CONVERSION (newop1);
	  STRIP_USELESS_TYPE_CONVERSION (newop2);
	  if (!is_gimple_val (newop1) || !is_gimple_val (newop2))
	    continue;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Equivalence: ");
	  print_generic_expr (dump_file, curr->op);
	  fprintf (dump_file, " %s ", op_symbol_code (opcode));
	  print_generic_expr (dump_file, oe->op);
	  fprintf (dump_file, " -> ");
	  print_generic_expr (dump_file, t);
	  fprintf (dump_file, "\n");
	}

      /* Now we can delete oe, as it has been subsumed by the new combined
         expression t.  */
      ops->ordered_remove (i);
      reassociate_stats.ops_eliminated ++;

      /* If t is the same as curr->op, we're done.  Otherwise we must
	 replace curr->op with t.  Special case is if we got a constant
	 back, in which case we add it to the end instead of in place of
	 the current entry.  */
      if (TREE_CODE (t) == INTEGER_CST)
	{
	  ops->ordered_remove (currindex);
	  add_to_ops_vec (ops, t);
	}
      else if (!operand_equal_p (t, curr->op, 0))
	{
	  gimple *sum;
	  enum tree_code subcode;
	  tree newop1;
	  tree newop2;
	  gcc_assert (COMPARISON_CLASS_P (t));
	  extract_ops_from_tree (t, &subcode, &newop1, &newop2);
	  STRIP_USELESS_TYPE_CONVERSION (newop1);
	  STRIP_USELESS_TYPE_CONVERSION (newop2);
	  gcc_checking_assert (is_gimple_val (newop1)
			       && is_gimple_val (newop2));
	  sum = build_and_add_sum (TREE_TYPE (t), newop1, newop2, subcode);
	  curr->op = gimple_get_lhs (sum);
	}
      return true;
    }

  return false;
}


/* Transform repeated addition of same values into multiply with
   constant.  */
static bool
transform_add_to_multiply (vec<operand_entry *> *ops)
{
  operand_entry *oe;
  tree op = NULL_TREE;
  int j;
  int i, start = -1, end = 0, count = 0;
  auto_vec<std::pair <int, int> > indxs;
  bool changed = false;

  if (!INTEGRAL_TYPE_P (TREE_TYPE ((*ops)[0]->op))
      && (!SCALAR_FLOAT_TYPE_P (TREE_TYPE ((*ops)[0]->op))
	  || !flag_unsafe_math_optimizations))
    return false;

  /* Look for repeated operands.  */
  FOR_EACH_VEC_ELT (*ops, i, oe)
    {
      if (start == -1)
	{
	  count = 1;
	  op = oe->op;
	  start = i;
	}
      else if (operand_equal_p (oe->op, op, 0))
	{
	  count++;
	  end = i;
	}
      else
	{
	  if (count > 1)
	    indxs.safe_push (std::make_pair (start, end));
	  count = 1;
	  op = oe->op;
	  start = i;
	}
    }

  if (count > 1)
    indxs.safe_push (std::make_pair (start, end));

  for (j = indxs.length () - 1; j >= 0; --j)
    {
      /* Convert repeated operand addition to multiplication.  */
      start = indxs[j].first;
      end = indxs[j].second;
      op = (*ops)[start]->op;
      count = end - start + 1;
      for (i = end; i >= start; --i)
	ops->unordered_remove (i);
      tree tmp = make_ssa_name (TREE_TYPE (op));
      tree cst = build_int_cst (integer_type_node, count);
      gassign *mul_stmt
	= gimple_build_assign (tmp, MULT_EXPR,
			       op, fold_convert (TREE_TYPE (op), cst));
      gimple_set_visited (mul_stmt, true);
      add_to_ops_vec (ops, tmp, mul_stmt);
      changed = true;
    }

  return changed;
}


/* Perform various identities and other optimizations on the list of
   operand entries, stored in OPS.  The tree code for the binary
   operation between all the operands is OPCODE.  */

static void
optimize_ops_list (enum tree_code opcode,
		   vec<operand_entry *> *ops)
{
  unsigned int length = ops->length ();
  unsigned int i;
  operand_entry *oe;
  operand_entry *oelast = NULL;
  bool iterate = false;

  if (length == 1)
    return;

  oelast = ops->last ();

  /* If the last two are constants, pop the constants off, merge them
     and try the next two.  */
  if (oelast->rank == 0 && is_gimple_min_invariant (oelast->op))
    {
      operand_entry *oelm1 = (*ops)[length - 2];

      if (oelm1->rank == 0
	  && is_gimple_min_invariant (oelm1->op)
	  && useless_type_conversion_p (TREE_TYPE (oelm1->op),
				       TREE_TYPE (oelast->op)))
	{
	  tree folded = fold_binary (opcode, TREE_TYPE (oelm1->op),
				     oelm1->op, oelast->op);

	  if (folded && is_gimple_min_invariant (folded))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Merging constants\n");

	      ops->pop ();
	      ops->pop ();

	      add_to_ops_vec (ops, folded);
	      reassociate_stats.constants_eliminated++;

	      optimize_ops_list (opcode, ops);
	      return;
	    }
	}
    }

  eliminate_using_constants (opcode, ops);
  oelast = NULL;

  for (i = 0; ops->iterate (i, &oe);)
    {
      bool done = false;

      if (eliminate_not_pairs (opcode, ops, i, oe))
	return;
      if (eliminate_duplicate_pair (opcode, ops, &done, i, oe, oelast)
	  || (!done && eliminate_plus_minus_pair (opcode, ops, i, oe))
	  || (!done && eliminate_redundant_comparison (opcode, ops, i, oe)))
	{
	  if (done)
	    return;
	  iterate = true;
	  oelast = NULL;
	  continue;
	}
      oelast = oe;
      i++;
    }

  if (iterate)
    optimize_ops_list (opcode, ops);
}

/* The following functions are subroutines to optimize_range_tests and allow
   it to try to change a logical combination of comparisons into a range
   test.

   For example, both
	X == 2 || X == 5 || X == 3 || X == 4
   and
	X >= 2 && X <= 5
   are converted to
	(unsigned) (X - 2) <= 3

   For more information see comments above fold_test_range in fold-const.cc,
   this implementation is for GIMPLE.  */



/* Dump the range entry R to FILE, skipping its expression if SKIP_EXP.  */

void
dump_range_entry (FILE *file, struct range_entry *r, bool skip_exp)
{
  if (!skip_exp)
    print_generic_expr (file, r->exp);
  fprintf (file, " %c[", r->in_p ? '+' : '-');
  print_generic_expr (file, r->low);
  fputs (", ", file);
  print_generic_expr (file, r->high);
  fputc (']', file);
}

/* Dump the range entry R to STDERR.  */

DEBUG_FUNCTION void
debug_range_entry (struct range_entry *r)
{
  dump_range_entry (stderr, r, false);
  fputc ('\n', stderr);
}

/* This is similar to make_range in fold-const.cc, but on top of
   GIMPLE instead of trees.  If EXP is non-NULL, it should be
   an SSA_NAME and STMT argument is ignored, otherwise STMT
   argument should be a GIMPLE_COND.  */

void
init_range_entry (struct range_entry *r, tree exp, gimple *stmt)
{
  int in_p;
  tree low, high;
  bool is_bool, strict_overflow_p;

  r->exp = NULL_TREE;
  r->in_p = false;
  r->strict_overflow_p = false;
  r->low = NULL_TREE;
  r->high = NULL_TREE;
  if (exp != NULL_TREE
      && (TREE_CODE (exp) != SSA_NAME || !INTEGRAL_TYPE_P (TREE_TYPE (exp))))
    return;

  /* Start with simply saying "EXP != 0" and then look at the code of EXP
     and see if we can refine the range.  Some of the cases below may not
     happen, but it doesn't seem worth worrying about this.  We "continue"
     the outer loop when we've changed something; otherwise we "break"
     the switch, which will "break" the while.  */
  low = exp ? build_int_cst (TREE_TYPE (exp), 0) : boolean_false_node;
  high = low;
  in_p = 0;
  strict_overflow_p = false;
  is_bool = false;
  if (exp == NULL_TREE)
    is_bool = true;
  else if (TYPE_PRECISION (TREE_TYPE (exp)) == 1)
    {
      if (TYPE_UNSIGNED (TREE_TYPE (exp)))
	is_bool = true;
      else
	return;
    }
  else if (TREE_CODE (TREE_TYPE (exp)) == BOOLEAN_TYPE)
    is_bool = true;

  while (1)
    {
      enum tree_code code;
      tree arg0, arg1, exp_type;
      tree nexp;
      location_t loc;

      if (exp != NULL_TREE)
	{
	  if (TREE_CODE (exp) != SSA_NAME
	      || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (exp))
	    break;

	  stmt = SSA_NAME_DEF_STMT (exp);
	  if (!is_gimple_assign (stmt))
	    break;

	  code = gimple_assign_rhs_code (stmt);
	  arg0 = gimple_assign_rhs1 (stmt);
	  arg1 = gimple_assign_rhs2 (stmt);
	  exp_type = TREE_TYPE (exp);
	}
      else
	{
	  code = gimple_cond_code (stmt);
	  arg0 = gimple_cond_lhs (stmt);
	  arg1 = gimple_cond_rhs (stmt);
	  exp_type = boolean_type_node;
	}

      if (TREE_CODE (arg0) != SSA_NAME
	  || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (arg0))
	break;
      loc = gimple_location (stmt);
      switch (code)
	{
	case BIT_NOT_EXPR:
	  if (TREE_CODE (TREE_TYPE (exp)) == BOOLEAN_TYPE
	      /* Ensure the range is either +[-,0], +[0,0],
		 -[-,0], -[0,0] or +[1,-], +[1,1], -[1,-] or
		 -[1,1].  If it is e.g. +[-,-] or -[-,-]
		 or similar expression of unconditional true or
		 false, it should not be negated.  */
	      && ((high && integer_zerop (high))
		  || (low && integer_onep (low))))
	    {
	      in_p = !in_p;
	      exp = arg0;
	      continue;
	    }
	  break;
	case SSA_NAME:
	  exp = arg0;
	  continue;
	CASE_CONVERT:
	  if (is_bool)
	    {
	      if ((TYPE_PRECISION (exp_type) == 1
		   || TREE_CODE (exp_type) == BOOLEAN_TYPE)
		  && TYPE_PRECISION (TREE_TYPE (arg0)) > 1)
		return;
	    }
	  else if (TYPE_PRECISION (TREE_TYPE (arg0)) == 1)
	    {
	      if (TYPE_UNSIGNED (TREE_TYPE (arg0)))
		is_bool = true;
	      else
		return;
	    }
	  else if (TREE_CODE (TREE_TYPE (arg0)) == BOOLEAN_TYPE)
	    is_bool = true;
	  goto do_default;
	case EQ_EXPR:
	case NE_EXPR:
	case LT_EXPR:
	case LE_EXPR:
	case GE_EXPR:
	case GT_EXPR:
	  is_bool = true;
	  /* FALLTHRU */
	default:
	  if (!is_bool)
	    return;
	do_default:
	  nexp = make_range_step (loc, code, arg0, arg1, exp_type,
				  &low, &high, &in_p,
				  &strict_overflow_p);
	  if (nexp != NULL_TREE)
	    {
	      exp = nexp;
	      gcc_assert (TREE_CODE (exp) == SSA_NAME);
	      continue;
	    }
	  break;
	}
      break;
    }
  if (is_bool)
    {
      r->exp = exp;
      r->in_p = in_p;
      r->low = low;
      r->high = high;
      r->strict_overflow_p = strict_overflow_p;
    }
}

/* Comparison function for qsort.  Sort entries
   without SSA_NAME exp first, then with SSA_NAMEs sorted
   by increasing SSA_NAME_VERSION, and for the same SSA_NAMEs
   by increasing ->low and if ->low is the same, by increasing
   ->high.  ->low == NULL_TREE means minimum, ->high == NULL_TREE
   maximum.  */

static int
range_entry_cmp (const void *a, const void *b)
{
  const struct range_entry *p = (const struct range_entry *) a;
  const struct range_entry *q = (const struct range_entry *) b;

  if (p->exp != NULL_TREE && TREE_CODE (p->exp) == SSA_NAME)
    {
      if (q->exp != NULL_TREE && TREE_CODE (q->exp) == SSA_NAME)
	{
	  /* Group range_entries for the same SSA_NAME together.  */
	  if (SSA_NAME_VERSION (p->exp) < SSA_NAME_VERSION (q->exp))
	    return -1;
	  else if (SSA_NAME_VERSION (p->exp) > SSA_NAME_VERSION (q->exp))
	    return 1;
	  /* If ->low is different, NULL low goes first, then by
	     ascending low.  */
	  if (p->low != NULL_TREE)
	    {
	      if (q->low != NULL_TREE)
		{
		  tree tem = fold_binary (LT_EXPR, boolean_type_node,
					  p->low, q->low);
		  if (tem && integer_onep (tem))
		    return -1;
		  tem = fold_binary (GT_EXPR, boolean_type_node,
				     p->low, q->low);
		  if (tem && integer_onep (tem))
		    return 1;
		}
	      else
		return 1;
	    }
	  else if (q->low != NULL_TREE)
	    return -1;
	  /* If ->high is different, NULL high goes last, before that by
	     ascending high.  */
	  if (p->high != NULL_TREE)
	    {
	      if (q->high != NULL_TREE)
		{
		  tree tem = fold_binary (LT_EXPR, boolean_type_node,
					  p->high, q->high);
		  if (tem && integer_onep (tem))
		    return -1;
		  tem = fold_binary (GT_EXPR, boolean_type_node,
				     p->high, q->high);
		  if (tem && integer_onep (tem))
		    return 1;
		}
	      else
		return -1;
	    }
	  else if (q->high != NULL_TREE)
	    return 1;
	  /* If both ranges are the same, sort below by ascending idx.  */
	}
      else
	return 1;
    }
  else if (q->exp != NULL_TREE && TREE_CODE (q->exp) == SSA_NAME)
    return -1;

  if (p->idx < q->idx)
    return -1;
  else
    {
      gcc_checking_assert (p->idx > q->idx);
      return 1;
    }
}

/* Helper function for update_range_test.  Force EXPR into an SSA_NAME,
   insert needed statements BEFORE or after GSI.  */

static tree
force_into_ssa_name (gimple_stmt_iterator *gsi, tree expr, bool before)
{
  enum gsi_iterator_update m = before ? GSI_SAME_STMT : GSI_CONTINUE_LINKING;
  tree ret = force_gimple_operand_gsi (gsi, expr, true, NULL_TREE, before, m);
  if (TREE_CODE (ret) != SSA_NAME)
    {
      gimple *g = gimple_build_assign (make_ssa_name (TREE_TYPE (ret)), ret);
      if (before)
	gsi_insert_before (gsi, g, GSI_SAME_STMT);
      else
	gsi_insert_after (gsi, g, GSI_CONTINUE_LINKING);
      ret = gimple_assign_lhs (g);
    }
  return ret;
}

/* Helper routine of optimize_range_test.
   [EXP, IN_P, LOW, HIGH, STRICT_OVERFLOW_P] is a merged range for
   RANGE and OTHERRANGE through OTHERRANGE + COUNT - 1 ranges,
   OPCODE and OPS are arguments of optimize_range_tests.  If OTHERRANGE
   is NULL, OTHERRANGEP should not be and then OTHERRANGEP points to
   an array of COUNT pointers to other ranges.  Return
   true if the range merge has been successful.
   If OPCODE is ERROR_MARK, this is called from within
   maybe_optimize_range_tests and is performing inter-bb range optimization.
   In that case, whether an op is BIT_AND_EXPR or BIT_IOR_EXPR is found in
   oe->rank.  */

static bool
update_range_test (struct range_entry *range, struct range_entry *otherrange,
		   struct range_entry **otherrangep,
		   unsigned int count, enum tree_code opcode,
		   vec<operand_entry *> *ops, tree exp, gimple_seq seq,
		   bool in_p, tree low, tree high, bool strict_overflow_p)
{
  unsigned int idx = range->idx;
  struct range_entry *swap_with = NULL;
  basic_block rewrite_bb_first = NULL, rewrite_bb_last = NULL;
  if (opcode == ERROR_MARK)
    {
      /* For inter-bb range test optimization, pick from the range tests
	 the one which is tested in the earliest condition (one dominating
	 the others), because otherwise there could be some UB (e.g. signed
	 overflow) in following bbs that we'd expose which wasn't there in
	 the original program.  See PR104196.  */
      basic_block orig_range_bb = BASIC_BLOCK_FOR_FN (cfun, (*ops)[idx]->id);
      basic_block range_bb = orig_range_bb;
      for (unsigned int i = 0; i < count; i++)
	{
	  struct range_entry *this_range;
	  if (otherrange)
	    this_range = otherrange + i;
	  else
	    this_range = otherrangep[i];
	  operand_entry *oe = (*ops)[this_range->idx];
	  basic_block this_bb = BASIC_BLOCK_FOR_FN (cfun, oe->id);
	  if (range_bb != this_bb
	      && dominated_by_p (CDI_DOMINATORS, range_bb, this_bb))
	    {
	      swap_with = this_range;
	      range_bb = this_bb;
	      idx = this_range->idx;
	    }
	}
      /* If seq is non-NULL, it can contain statements that use SSA_NAMEs
	 only defined in later blocks.  In this case we can't move the
	 merged comparison earlier, so instead check if there are any stmts
	 that might trigger signed integer overflow in between and rewrite
	 them.  But only after we check if the optimization is possible.  */
      if (seq && swap_with)
	{
	  rewrite_bb_first = range_bb;
	  rewrite_bb_last = orig_range_bb;
	  idx = range->idx;
	  swap_with = NULL;
	}
    }
  operand_entry *oe = (*ops)[idx];
  tree op = oe->op;
  gimple *stmt = op ? SSA_NAME_DEF_STMT (op)
		    : last_stmt (BASIC_BLOCK_FOR_FN (cfun, oe->id));
  location_t loc = gimple_location (stmt);
  tree optype = op ? TREE_TYPE (op) : boolean_type_node;
  tree tem = build_range_check (loc, optype, unshare_expr (exp),
				in_p, low, high);
  enum warn_strict_overflow_code wc = WARN_STRICT_OVERFLOW_COMPARISON;
  gimple_stmt_iterator gsi;
  unsigned int i, uid;

  if (tem == NULL_TREE)
    return false;

  /* If op is default def SSA_NAME, there is no place to insert the
     new comparison.  Give up, unless we can use OP itself as the
     range test.  */
  if (op && SSA_NAME_IS_DEFAULT_DEF (op))
    {
      if (op == range->exp
	  && ((TYPE_PRECISION (optype) == 1 && TYPE_UNSIGNED (optype))
	      || TREE_CODE (optype) == BOOLEAN_TYPE)
	  && (op == tem
	      || (TREE_CODE (tem) == EQ_EXPR
		  && TREE_OPERAND (tem, 0) == op
		  && integer_onep (TREE_OPERAND (tem, 1))))
	  && opcode != BIT_IOR_EXPR
	  && (opcode != ERROR_MARK || oe->rank != BIT_IOR_EXPR))
	{
	  stmt = NULL;
	  tem = op;
	}
      else
	return false;
    }

  if (swap_with)
    std::swap (range->idx, swap_with->idx);

  if (strict_overflow_p && issue_strict_overflow_warning (wc))
    warning_at (loc, OPT_Wstrict_overflow,
		"assuming signed overflow does not occur "
		"when simplifying range test");

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      struct range_entry *r;
      fprintf (dump_file, "Optimizing range tests ");
      dump_range_entry (dump_file, range, false);
      for (i = 0; i < count; i++)
	{
	  if (otherrange)
	    r = otherrange + i;
	  else
	    r = otherrangep[i];
	  if (r->exp
	      && r->exp != range->exp
	      && TREE_CODE (r->exp) == SSA_NAME)
	    {
	      fprintf (dump_file, " and ");
	      dump_range_entry (dump_file, r, false);
	    }
	  else
	    {
	      fprintf (dump_file, " and");
	      dump_range_entry (dump_file, r, true);
	    }
	}
      fprintf (dump_file, "\n into ");
      print_generic_expr (dump_file, tem);
      fprintf (dump_file, "\n");
    }

  /* In inter-bb range optimization mode, if we have a seq, we can't
     move the merged comparison to the earliest bb from the comparisons
     being replaced, so instead rewrite stmts that could trigger signed
     integer overflow.  */
  for (basic_block bb = rewrite_bb_last;
       bb != rewrite_bb_first; bb = single_pred (bb))
    for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);
	if (is_gimple_assign (stmt))
	  if (tree lhs = gimple_assign_lhs (stmt))
	    if ((INTEGRAL_TYPE_P (TREE_TYPE (lhs))
		 || POINTER_TYPE_P (TREE_TYPE (lhs)))
		&& TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (lhs)))
	      {
		enum tree_code code = gimple_assign_rhs_code (stmt);
		if (arith_code_with_undefined_signed_overflow (code))
		  {
		    gimple_stmt_iterator gsip = gsi;
		    gimple_stmt_iterator gsin = gsi;
		    gsi_prev (&gsip);
		    gsi_next (&gsin);
		    rewrite_to_defined_overflow (stmt, true);
		    unsigned uid = gimple_uid (stmt);
		    if (gsi_end_p (gsip))
		      gsip = gsi_after_labels (bb);
		    else
		      gsi_next (&gsip);
		    for (; gsi_stmt (gsip) != gsi_stmt (gsin);
			 gsi_next (&gsip))
		      gimple_set_uid (gsi_stmt (gsip), uid);
		  }
	      }
      }

  if (opcode == BIT_IOR_EXPR
      || (opcode == ERROR_MARK && oe->rank == BIT_IOR_EXPR))
    tem = invert_truthvalue_loc (loc, tem);

  tem = fold_convert_loc (loc, optype, tem);
  if (stmt)
    {
      gsi = gsi_for_stmt (stmt);
      uid = gimple_uid (stmt);
    }
  else
    {
      gsi = gsi_none ();
      uid = 0;
    }
  if (stmt == NULL)
    gcc_checking_assert (tem == op);
  /* In rare cases range->exp can be equal to lhs of stmt.
     In that case we have to insert after the stmt rather then before
     it.  If stmt is a PHI, insert it at the start of the basic block.  */
  else if (op != range->exp)
    {
      gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
      tem = force_into_ssa_name (&gsi, tem, true);
      gsi_prev (&gsi);
    }
  else if (gimple_code (stmt) != GIMPLE_PHI)
    {
      gsi_insert_seq_after (&gsi, seq, GSI_CONTINUE_LINKING);
      tem = force_into_ssa_name (&gsi, tem, false);
    }
  else
    {
      gsi = gsi_after_labels (gimple_bb (stmt));
      if (!gsi_end_p (gsi))
	uid = gimple_uid (gsi_stmt (gsi));
      else
	{
	  gsi = gsi_start_bb (gimple_bb (stmt));
	  uid = 1;
	  while (!gsi_end_p (gsi))
	    {
	      uid = gimple_uid (gsi_stmt (gsi));
	      gsi_next (&gsi);
	    }
	}
      gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
      tem = force_into_ssa_name (&gsi, tem, true);
      if (gsi_end_p (gsi))
	gsi = gsi_last_bb (gimple_bb (stmt));
      else
	gsi_prev (&gsi);
    }
  for (; !gsi_end_p (gsi); gsi_prev (&gsi))
    if (gimple_uid (gsi_stmt (gsi)))
      break;
    else
      gimple_set_uid (gsi_stmt (gsi), uid);

  oe->op = tem;
  range->exp = exp;
  range->low = low;
  range->high = high;
  range->in_p = in_p;
  range->strict_overflow_p = false;

  for (i = 0; i < count; i++)
    {
      if (otherrange)
	range = otherrange + i;
      else
	range = otherrangep[i];
      oe = (*ops)[range->idx];
      /* Now change all the other range test immediate uses, so that
	 those tests will be optimized away.  */
      if (opcode == ERROR_MARK)
	{
	  if (oe->op)
	    oe->op = build_int_cst (TREE_TYPE (oe->op),
				    oe->rank == BIT_IOR_EXPR ? 0 : 1);
	  else
	    oe->op = (oe->rank == BIT_IOR_EXPR
		      ? boolean_false_node : boolean_true_node);
	}
      else
	oe->op = error_mark_node;
      range->exp = NULL_TREE;
      range->low = NULL_TREE;
      range->high = NULL_TREE;
    }
  return true;
}

/* Optimize X == CST1 || X == CST2
   if popcount (CST1 ^ CST2) == 1 into
   (X & ~(CST1 ^ CST2)) == (CST1 & ~(CST1 ^ CST2)).
   Similarly for ranges.  E.g.
   X != 2 && X != 3 && X != 10 && X != 11
   will be transformed by the previous optimization into
   !((X - 2U) <= 1U || (X - 10U) <= 1U)
   and this loop can transform that into
   !(((X & ~8) - 2U) <= 1U).  */

static bool
optimize_range_tests_xor (enum tree_code opcode, tree type,
			  tree lowi, tree lowj, tree highi, tree highj,
			  vec<operand_entry *> *ops,
			  struct range_entry *rangei,
			  struct range_entry *rangej)
{
  tree lowxor, highxor, tem, exp;
  /* Check lowi ^ lowj == highi ^ highj and
     popcount (lowi ^ lowj) == 1.  */
  lowxor = fold_binary (BIT_XOR_EXPR, type, lowi, lowj);
  if (lowxor == NULL_TREE || TREE_CODE (lowxor) != INTEGER_CST)
    return false;
  if (!integer_pow2p (lowxor))
    return false;
  highxor = fold_binary (BIT_XOR_EXPR, type, highi, highj);
  if (!tree_int_cst_equal (lowxor, highxor))
    return false;

  exp = rangei->exp;
  scalar_int_mode mode = as_a <scalar_int_mode> (TYPE_MODE (type));
  int prec = GET_MODE_PRECISION (mode);
  if (TYPE_PRECISION (type) < prec
      || (wi::to_wide (TYPE_MIN_VALUE (type))
	  != wi::min_value (prec, TYPE_SIGN (type)))
      || (wi::to_wide (TYPE_MAX_VALUE (type))
	  != wi::max_value (prec, TYPE_SIGN (type))))
    {
      type = build_nonstandard_integer_type (prec, TYPE_UNSIGNED (type));
      exp = fold_convert (type, exp);
      lowxor = fold_convert (type, lowxor);
      lowi = fold_convert (type, lowi);
      highi = fold_convert (type, highi);
    }
  tem = fold_build1 (BIT_NOT_EXPR, type, lowxor);
  exp = fold_build2 (BIT_AND_EXPR, type, exp, tem);
  lowj = fold_build2 (BIT_AND_EXPR, type, lowi, tem);
  highj = fold_build2 (BIT_AND_EXPR, type, highi, tem);
  if (update_range_test (rangei, rangej, NULL, 1, opcode, ops, exp,
			 NULL, rangei->in_p, lowj, highj,
			 rangei->strict_overflow_p
			 || rangej->strict_overflow_p))
    return true;
  return false;
}

/* Optimize X == CST1 || X == CST2
   if popcount (CST2 - CST1) == 1 into
   ((X - CST1) & ~(CST2 - CST1)) == 0.
   Similarly for ranges.  E.g.
   X == 43 || X == 76 || X == 44 || X == 78 || X == 77 || X == 46
   || X == 75 || X == 45
   will be transformed by the previous optimization into
   (X - 43U) <= 3U || (X - 75U) <= 3U
   and this loop can transform that into
   ((X - 43U) & ~(75U - 43U)) <= 3U.  */
static bool
optimize_range_tests_diff (enum tree_code opcode, tree type,
			   tree lowi, tree lowj, tree highi, tree highj,
			   vec<operand_entry *> *ops,
			   struct range_entry *rangei,
			   struct range_entry *rangej)
{
  tree tem1, tem2, mask;
  /* Check highi - lowi == highj - lowj.  */
  tem1 = fold_binary (MINUS_EXPR, type, highi, lowi);
  if (tem1 == NULL_TREE || TREE_CODE (tem1) != INTEGER_CST)
    return false;
  tem2 = fold_binary (MINUS_EXPR, type, highj, lowj);
  if (!tree_int_cst_equal (tem1, tem2))
    return false;
  /* Check popcount (lowj - lowi) == 1.  */
  tem1 = fold_binary (MINUS_EXPR, type, lowj, lowi);
  if (tem1 == NULL_TREE || TREE_CODE (tem1) != INTEGER_CST)
    return false;
  if (!integer_pow2p (tem1))
    return false;

  scalar_int_mode mode = as_a <scalar_int_mode> (TYPE_MODE (type));
  int prec = GET_MODE_PRECISION (mode);
  if (TYPE_PRECISION (type) < prec
      || (wi::to_wide (TYPE_MIN_VALUE (type))
	  != wi::min_value (prec, TYPE_SIGN (type)))
      || (wi::to_wide (TYPE_MAX_VALUE (type))
	  != wi::max_value (prec, TYPE_SIGN (type))))
    type = build_nonstandard_integer_type (prec, 1);
  else
    type = unsigned_type_for (type);
  tem1 = fold_convert (type, tem1);
  tem2 = fold_convert (type, tem2);
  lowi = fold_convert (type, lowi);
  mask = fold_build1 (BIT_NOT_EXPR, type, tem1);
  tem1 = fold_build2 (MINUS_EXPR, type,
		      fold_convert (type, rangei->exp), lowi);
  tem1 = fold_build2 (BIT_AND_EXPR, type, tem1, mask);
  lowj = build_int_cst (type, 0);
  if (update_range_test (rangei, rangej, NULL, 1, opcode, ops, tem1,
			 NULL, rangei->in_p, lowj, tem2,
			 rangei->strict_overflow_p
			 || rangej->strict_overflow_p))
    return true;
  return false;
}

/* It does some common checks for function optimize_range_tests_xor and
   optimize_range_tests_diff.
   If OPTIMIZE_XOR is TRUE, it calls optimize_range_tests_xor.
   Else it calls optimize_range_tests_diff.  */

static bool
optimize_range_tests_1 (enum tree_code opcode, int first, int length,
			bool optimize_xor, vec<operand_entry *> *ops,
			struct range_entry *ranges)
{
  int i, j;
  bool any_changes = false;
  for (i = first; i < length; i++)
    {
      tree lowi, highi, lowj, highj, type, tem;

      if (ranges[i].exp == NULL_TREE || ranges[i].in_p)
	continue;
      type = TREE_TYPE (ranges[i].exp);
      if (!INTEGRAL_TYPE_P (type))
	continue;
      lowi = ranges[i].low;
      if (lowi == NULL_TREE)
	lowi = TYPE_MIN_VALUE (type);
      highi = ranges[i].high;
      if (highi == NULL_TREE)
	continue;
      for (j = i + 1; j < length && j < i + 64; j++)
	{
	  bool changes;
	  if (ranges[i].exp != ranges[j].exp || ranges[j].in_p)
	    continue;
	  lowj = ranges[j].low;
	  if (lowj == NULL_TREE)
	    continue;
	  highj = ranges[j].high;
	  if (highj == NULL_TREE)
	    highj = TYPE_MAX_VALUE (type);
	  /* Check lowj > highi.  */
	  tem = fold_binary (GT_EXPR, boolean_type_node,
			     lowj, highi);
	  if (tem == NULL_TREE || !integer_onep (tem))
	    continue;
	  if (optimize_xor)
	    changes = optimize_range_tests_xor (opcode, type, lowi, lowj,
						highi, highj, ops,
						ranges + i, ranges + j);
	  else
	    changes = optimize_range_tests_diff (opcode, type, lowi, lowj,
						 highi, highj, ops,
						 ranges + i, ranges + j);
	  if (changes)
	    {
	      any_changes = true;
	      break;
	    }
	}
    }
  return any_changes;
}

/* Helper function of optimize_range_tests_to_bit_test.  Handle a single
   range, EXP, LOW, HIGH, compute bit mask of bits to test and return
   EXP on success, NULL otherwise.  */

static tree
extract_bit_test_mask (tree exp, int prec, tree totallow, tree low, tree high,
		       wide_int *mask, tree *totallowp)
{
  tree tem = int_const_binop (MINUS_EXPR, high, low);
  if (tem == NULL_TREE
      || TREE_CODE (tem) != INTEGER_CST
      || TREE_OVERFLOW (tem)
      || tree_int_cst_sgn (tem) == -1
      || compare_tree_int (tem, prec) != -1)
    return NULL_TREE;

  unsigned HOST_WIDE_INT max = tree_to_uhwi (tem) + 1;
  *mask = wi::shifted_mask (0, max, false, prec);
  if (TREE_CODE (exp) == BIT_AND_EXPR
      && TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST)
    {
      widest_int msk = wi::to_widest (TREE_OPERAND (exp, 1));
      msk = wi::zext (~msk, TYPE_PRECISION (TREE_TYPE (exp)));
      if (wi::popcount (msk) == 1
	  && wi::ltu_p (msk, prec - max))
	{
	  *mask |= wi::shifted_mask (msk.to_uhwi (), max, false, prec);
	  max += msk.to_uhwi ();
	  exp = TREE_OPERAND (exp, 0);
	  if (integer_zerop (low)
	      && TREE_CODE (exp) == PLUS_EXPR
	      && TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST)
	    {
	      tree ret = TREE_OPERAND (exp, 0);
	      STRIP_NOPS (ret);
	      widest_int bias
	        = wi::neg (wi::sext (wi::to_widest (TREE_OPERAND (exp, 1)),
				     TYPE_PRECISION (TREE_TYPE (low))));
	      tree tbias = wide_int_to_tree (TREE_TYPE (ret), bias);
	      if (totallowp)
		{
		  *totallowp = tbias;
		  return ret;
		}
	      else if (!tree_int_cst_lt (totallow, tbias))
		return NULL_TREE;
	      bias = wi::to_widest (tbias);
	      bias -= wi::to_widest (totallow);
	      if (bias >= 0 && bias < prec - max)
		{
		  *mask = wi::lshift (*mask, bias);
		  return ret;
		}
	    }
	}
    }
  if (totallowp)
    return exp;
  if (!tree_int_cst_lt (totallow, low))
    return exp;
  tem = int_const_binop (MINUS_EXPR, low, totallow);
  if (tem == NULL_TREE
      || TREE_CODE (tem) != INTEGER_CST
      || TREE_OVERFLOW (tem)
      || compare_tree_int (tem, prec - max) == 1)
    return NULL_TREE;

  *mask = wi::lshift (*mask, wi::to_widest (tem));
  return exp;
}

/* Attempt to optimize small range tests using bit test.
   E.g.
   X != 43 && X != 76 && X != 44 && X != 78 && X != 49
   && X != 77 && X != 46 && X != 75 && X != 45 && X != 82
   has been by earlier optimizations optimized into:
   ((X - 43U) & ~32U) > 3U && X != 49 && X != 82
   As all the 43 through 82 range is less than 64 numbers,
   for 64-bit word targets optimize that into:
   (X - 43U) > 40U && ((1 << (X - 43U)) & 0x8F0000004FULL) == 0  */

static bool
optimize_range_tests_to_bit_test (enum tree_code opcode, int first, int length,
				  vec<operand_entry *> *ops,
				  struct range_entry *ranges)
{
  int i, j;
  bool any_changes = false;
  int prec = GET_MODE_BITSIZE (word_mode);
  auto_vec<struct range_entry *, 64> candidates;

  for (i = first; i < length - 1; i++)
    {
      tree lowi, highi, lowj, highj, type;

      if (ranges[i].exp == NULL_TREE || ranges[i].in_p)
	continue;
      type = TREE_TYPE (ranges[i].exp);
      if (!INTEGRAL_TYPE_P (type))
	continue;
      lowi = ranges[i].low;
      if (lowi == NULL_TREE)
	lowi = TYPE_MIN_VALUE (type);
      highi = ranges[i].high;
      if (highi == NULL_TREE)
	continue;
      wide_int mask;
      tree exp = extract_bit_test_mask (ranges[i].exp, prec, lowi, lowi,
					highi, &mask, &lowi);
      if (exp == NULL_TREE)
	continue;
      bool strict_overflow_p = ranges[i].strict_overflow_p;
      candidates.truncate (0);
      int end = MIN (i + 64, length);
      for (j = i + 1; j < end; j++)
	{
	  tree exp2;
	  if (ranges[j].exp == NULL_TREE || ranges[j].in_p)
	    continue;
	  if (ranges[j].exp == exp)
	    ;
	  else if (TREE_CODE (ranges[j].exp) == BIT_AND_EXPR)
	    {
	      exp2 = TREE_OPERAND (ranges[j].exp, 0);
	      if (exp2 == exp)
		;
	      else if (TREE_CODE (exp2) == PLUS_EXPR)
		{
		  exp2 = TREE_OPERAND (exp2, 0);
		  STRIP_NOPS (exp2);
		  if (exp2 != exp)
		    continue;
		}
	      else
		continue;
	    }
	  else
	    continue;
	  lowj = ranges[j].low;
	  if (lowj == NULL_TREE)
	    continue;
	  highj = ranges[j].high;
	  if (highj == NULL_TREE)
	    highj = TYPE_MAX_VALUE (type);
	  wide_int mask2;
	  exp2 = extract_bit_test_mask (ranges[j].exp, prec, lowi, lowj,
					highj, &mask2, NULL);
	  if (exp2 != exp)
	    continue;
	  mask |= mask2;
	  strict_overflow_p |= ranges[j].strict_overflow_p;
	  candidates.safe_push (&ranges[j]);
	}

      /* If every possible relative value of the expression is a valid shift
	 amount, then we can merge the entry test in the bit test.  In this
	 case, if we would need otherwise 2 or more comparisons, then use
	 the bit test; in the other cases, the threshold is 3 comparisons.  */
      bool entry_test_needed;
      value_range r;
      if (TREE_CODE (exp) == SSA_NAME
	  && get_range_query (cfun)->range_of_expr (r, exp)
	  && r.kind () == VR_RANGE
	  && wi::leu_p (r.upper_bound () - r.lower_bound (), prec - 1))
	{
	  wide_int min = r.lower_bound ();
	  wide_int ilowi = wi::to_wide (lowi);
	  if (wi::lt_p (min, ilowi, TYPE_SIGN (TREE_TYPE (lowi))))
	    {
	      lowi = wide_int_to_tree (TREE_TYPE (lowi), min);
	      mask = wi::lshift (mask, ilowi - min);
	    }
	  else if (wi::gt_p (min, ilowi, TYPE_SIGN (TREE_TYPE (lowi))))
	    {
	      lowi = wide_int_to_tree (TREE_TYPE (lowi), min);
	      mask = wi::lrshift (mask, min - ilowi);
	    }
	  entry_test_needed = false;
	}
      else
	entry_test_needed = true;
      if (candidates.length () >= (entry_test_needed ? 2 : 1))
	{
	  tree high = wide_int_to_tree (TREE_TYPE (lowi),
					wi::to_widest (lowi)
					+ prec - 1 - wi::clz (mask));
	  operand_entry *oe = (*ops)[ranges[i].idx];
	  tree op = oe->op;
	  gimple *stmt = op ? SSA_NAME_DEF_STMT (op)
			    : last_stmt (BASIC_BLOCK_FOR_FN (cfun, oe->id));
	  location_t loc = gimple_location (stmt);
	  tree optype = op ? TREE_TYPE (op) : boolean_type_node;

	  /* See if it isn't cheaper to pretend the minimum value of the
	     range is 0, if maximum value is small enough.
	     We can avoid then subtraction of the minimum value, but the
	     mask constant could be perhaps more expensive.  */
	  if (compare_tree_int (lowi, 0) > 0
	      && compare_tree_int (high, prec) < 0)
	    {
	      int cost_diff;
	      HOST_WIDE_INT m = tree_to_uhwi (lowi);
	      rtx reg = gen_raw_REG (word_mode, 10000);
	      bool speed_p = optimize_bb_for_speed_p (gimple_bb (stmt));
	      cost_diff = set_src_cost (gen_rtx_PLUS (word_mode, reg,
						      GEN_INT (-m)),
					word_mode, speed_p);
	      rtx r = immed_wide_int_const (mask, word_mode);
	      cost_diff += set_src_cost (gen_rtx_AND (word_mode, reg, r),
					 word_mode, speed_p);
	      r = immed_wide_int_const (wi::lshift (mask, m), word_mode);
	      cost_diff -= set_src_cost (gen_rtx_AND (word_mode, reg, r),
					 word_mode, speed_p);
	      if (cost_diff > 0)
		{
		  mask = wi::lshift (mask, m);
		  lowi = build_zero_cst (TREE_TYPE (lowi));
		}
	    }

	  tree tem;
	  if (entry_test_needed)
	    {
	      tem = build_range_check (loc, optype, unshare_expr (exp),
				       false, lowi, high);
	      if (tem == NULL_TREE || is_gimple_val (tem))
		continue;
	    }
	  else
	    tem = NULL_TREE;
	  tree etype = unsigned_type_for (TREE_TYPE (exp));
	  exp = fold_build2_loc (loc, MINUS_EXPR, etype,
				 fold_convert_loc (loc, etype, exp),
				 fold_convert_loc (loc, etype, lowi));
	  exp = fold_convert_loc (loc, integer_type_node, exp);
	  tree word_type = lang_hooks.types.type_for_mode (word_mode, 1);
	  exp = fold_build2_loc (loc, LSHIFT_EXPR, word_type,
				 build_int_cst (word_type, 1), exp);
	  exp = fold_build2_loc (loc, BIT_AND_EXPR, word_type, exp,
				 wide_int_to_tree (word_type, mask));
	  exp = fold_build2_loc (loc, EQ_EXPR, optype, exp,
				 build_zero_cst (word_type));
	  if (is_gimple_val (exp))
	    continue;

	  /* The shift might have undefined behavior if TEM is true,
	     but reassociate_bb isn't prepared to have basic blocks
	     split when it is running.  So, temporarily emit a code
	     with BIT_IOR_EXPR instead of &&, and fix it up in
	     branch_fixup.  */
	  gimple_seq seq = NULL;
	  if (tem)
	    {
	      tem = force_gimple_operand (tem, &seq, true, NULL_TREE);
	      gcc_assert (TREE_CODE (tem) == SSA_NAME);
	      gimple_set_visited (SSA_NAME_DEF_STMT (tem), true);
	    }
	  gimple_seq seq2;
	  exp = force_gimple_operand (exp, &seq2, true, NULL_TREE);
	  gimple_seq_add_seq_without_update (&seq, seq2);
	  gcc_assert (TREE_CODE (exp) == SSA_NAME);
	  gimple_set_visited (SSA_NAME_DEF_STMT (exp), true);
	  if (tem)
	    {
	      gimple *g = gimple_build_assign (make_ssa_name (optype),
					       BIT_IOR_EXPR, tem, exp);
	      gimple_set_location (g, loc);
	      gimple_seq_add_stmt_without_update (&seq, g);
	      exp = gimple_assign_lhs (g);
	    }
	  tree val = build_zero_cst (optype);
	  if (update_range_test (&ranges[i], NULL, candidates.address (),
				 candidates.length (), opcode, ops, exp,
				 seq, false, val, val, strict_overflow_p))
	    {
	      any_changes = true;
	      if (tem)
		reassoc_branch_fixups.safe_push (tem);
	    }
	  else
	    gimple_seq_discard (seq);
	}
    }
  return any_changes;
}

/* Optimize x != 0 && y != 0 && z != 0 into (x | y | z) != 0
   and similarly x != -1 && y != -1 && y != -1 into (x & y & z) != -1.
   Also, handle x < C && y < C && z < C where C is power of two as
   (x | y | z) < C.  And also handle signed x < 0 && y < 0 && z < 0
   as (x | y | z) < 0.  */

static bool
optimize_range_tests_cmp_bitwise (enum tree_code opcode, int first, int length,
				  vec<operand_entry *> *ops,
				  struct range_entry *ranges)
{
  int i;
  unsigned int b;
  bool any_changes = false;
  auto_vec<int, 128> buckets;
  auto_vec<int, 32> chains;
  auto_vec<struct range_entry *, 32> candidates;

  for (i = first; i < length; i++)
    {
      int idx;

      if (ranges[i].exp == NULL_TREE
	  || TREE_CODE (ranges[i].exp) != SSA_NAME
	  || TYPE_PRECISION (TREE_TYPE (ranges[i].exp)) <= 1
	  || TREE_CODE (TREE_TYPE (ranges[i].exp)) == BOOLEAN_TYPE)
	continue;

      if (ranges[i].low != NULL_TREE
	  && ranges[i].high != NULL_TREE
	  && ranges[i].in_p
	  && tree_int_cst_equal (ranges[i].low, ranges[i].high))
	{
	  idx = !integer_zerop (ranges[i].low);
	  if (idx && !integer_all_onesp (ranges[i].low))
	    continue;
	}
      else if (ranges[i].high != NULL_TREE
	       && TREE_CODE (ranges[i].high) == INTEGER_CST
	       && ranges[i].in_p)
	{
	  wide_int w = wi::to_wide (ranges[i].high);
	  int prec = TYPE_PRECISION (TREE_TYPE (ranges[i].exp));
	  int l = wi::clz (w);
	  idx = 2;
	  if (l <= 0
	      || l >= prec
	      || w != wi::mask (prec - l, false, prec))
	    continue;
	  if (!((TYPE_UNSIGNED (TREE_TYPE (ranges[i].exp))
		 && ranges[i].low == NULL_TREE)
		|| (ranges[i].low
		    && integer_zerop (ranges[i].low))))
	    continue;
	}
      else if (ranges[i].high == NULL_TREE
	       && ranges[i].low != NULL_TREE
	       /* Perform this optimization only in the last
		  reassoc pass, as it interferes with the reassociation
		  itself or could also with VRP etc. which might not
		  be able to virtually undo the optimization.  */
	       && !reassoc_insert_powi_p
	       && !TYPE_UNSIGNED (TREE_TYPE (ranges[i].exp))
	       && integer_zerop (ranges[i].low))
	idx = 3;
      else
	continue;

      b = TYPE_PRECISION (TREE_TYPE (ranges[i].exp)) * 4 + idx;
      if (buckets.length () <= b)
	buckets.safe_grow_cleared (b + 1, true);
      if (chains.length () <= (unsigned) i)
	chains.safe_grow (i + 1, true);
      chains[i] = buckets[b];
      buckets[b] = i + 1;
    }

  FOR_EACH_VEC_ELT (buckets, b, i)
    if (i && chains[i - 1])
      {
	int j, k = i;
	if ((b % 4) == 2)
	  {
	    /* When ranges[X - 1].high + 1 is a power of two,
	       we need to process the same bucket up to
	       precision - 1 times, each time split the entries
	       with the same high bound into one chain and the
	       rest into another one to be processed later.  */
	    int this_prev = i;
	    int other_prev = 0;
	    for (j = chains[i - 1]; j; j = chains[j - 1])
	      {
		if (tree_int_cst_equal (ranges[i - 1].high,
					ranges[j - 1].high))
		  {
		    chains[this_prev - 1] = j;
		    this_prev = j;
		  }
		else if (other_prev == 0)
		  {
		    buckets[b] = j;
		    other_prev = j;
		  }
		else
		  {
		    chains[other_prev - 1] = j;
		    other_prev = j;
		  }
	      }
	    chains[this_prev - 1] = 0;
	    if (other_prev)
	      chains[other_prev - 1] = 0;
	    if (chains[i - 1] == 0)
	      {
		if (other_prev)
		  b--;
		continue;
	      }
	  }
	for (j = chains[i - 1]; j; j = chains[j - 1])
	  {
	    gimple *gk = SSA_NAME_DEF_STMT (ranges[k - 1].exp);
	    gimple *gj = SSA_NAME_DEF_STMT (ranges[j - 1].exp);
	    if (reassoc_stmt_dominates_stmt_p (gk, gj))
	      k = j;
	  }
	tree type1 = TREE_TYPE (ranges[k - 1].exp);
	tree type2 = NULL_TREE;
	bool strict_overflow_p = false;
	candidates.truncate (0);
	if (POINTER_TYPE_P (type1) || TREE_CODE (type1) == OFFSET_TYPE)
	  type1 = pointer_sized_int_node;
	for (j = i; j; j = chains[j - 1])
	  {
	    tree type = TREE_TYPE (ranges[j - 1].exp);
	    strict_overflow_p |= ranges[j - 1].strict_overflow_p;
	    if (POINTER_TYPE_P (type) || TREE_CODE (type) == OFFSET_TYPE)
	      type = pointer_sized_int_node;
	    if ((b % 4) == 3)
	      {
		/* For the signed < 0 cases, the types should be
		   really compatible (all signed with the same precision,
		   instead put ranges that have different in_p from
		   k first.  */
		if (!useless_type_conversion_p (type1, type))
		  continue;
		if (ranges[j - 1].in_p != ranges[k - 1].in_p)
		  candidates.safe_push (&ranges[j - 1]);
		type2 = type1;
		continue;
	      }
	    if (j == k
		|| useless_type_conversion_p (type1, type))
	      ;
	    else if (type2 == NULL_TREE
		     || useless_type_conversion_p (type2, type))
	      {
		if (type2 == NULL_TREE)
		  type2 = type;
		candidates.safe_push (&ranges[j - 1]);
	      }
	  }
	unsigned l = candidates.length ();
	for (j = i; j; j = chains[j - 1])
	  {
	    tree type = TREE_TYPE (ranges[j - 1].exp);
	    if (j == k)
	      continue;
	    if (POINTER_TYPE_P (type) || TREE_CODE (type) == OFFSET_TYPE)
	      type = pointer_sized_int_node;
	    if ((b % 4) == 3)
	      {
		if (!useless_type_conversion_p (type1, type))
		  continue;
		if (ranges[j - 1].in_p == ranges[k - 1].in_p)
		  candidates.safe_push (&ranges[j - 1]);
		continue;
	      }
	    if (useless_type_conversion_p (type1, type))
	      ;
	    else if (type2 == NULL_TREE
		     || useless_type_conversion_p (type2, type))
	      continue;
	    candidates.safe_push (&ranges[j - 1]);
	  }
	gimple_seq seq = NULL;
	tree op = NULL_TREE;
	unsigned int id;
	struct range_entry *r;
	candidates.safe_push (&ranges[k - 1]);
	FOR_EACH_VEC_ELT (candidates, id, r)
	  {
	    gimple *g;
	    enum tree_code code;
	    if (id == 0)
	      {
		op = r->exp;
		continue;
	      }
	    if (id == l
		|| POINTER_TYPE_P (TREE_TYPE (op))
		|| TREE_CODE (TREE_TYPE (op)) == OFFSET_TYPE)
	      {
		code = (b % 4) == 3 ? BIT_NOT_EXPR : NOP_EXPR;
		tree type3 = id >= l ? type1 : pointer_sized_int_node;
		if (code == BIT_NOT_EXPR
		    && TREE_CODE (TREE_TYPE (op)) == OFFSET_TYPE)
		  {
		    g = gimple_build_assign (make_ssa_name (type3),
					     NOP_EXPR, op);
		    gimple_seq_add_stmt_without_update (&seq, g);
		    op = gimple_assign_lhs (g);
		  }
		g = gimple_build_assign (make_ssa_name (type3), code, op);
		gimple_seq_add_stmt_without_update (&seq, g);
		op = gimple_assign_lhs (g);
	      }
	    tree type = TREE_TYPE (r->exp);
	    tree exp = r->exp;
	    if (POINTER_TYPE_P (type)
		|| TREE_CODE (type) == OFFSET_TYPE
		|| (id >= l && !useless_type_conversion_p (type1, type)))
	      {
		tree type3 = id >= l ? type1 : pointer_sized_int_node;
		g = gimple_build_assign (make_ssa_name (type3), NOP_EXPR, exp);
		gimple_seq_add_stmt_without_update (&seq, g);
		exp = gimple_assign_lhs (g);
	      }
	    if ((b % 4) == 3)
	      code = r->in_p ? BIT_IOR_EXPR : BIT_AND_EXPR;
	    else
	      code = (b % 4) == 1 ? BIT_AND_EXPR : BIT_IOR_EXPR;
	    g = gimple_build_assign (make_ssa_name (id >= l ? type1 : type2),
				     code, op, exp);
	    gimple_seq_add_stmt_without_update (&seq, g);
	    op = gimple_assign_lhs (g);
	  }
	type1 = TREE_TYPE (ranges[k - 1].exp);
	if (POINTER_TYPE_P (type1) || TREE_CODE (type1) == OFFSET_TYPE)
	  {
	    gimple *g
	      = gimple_build_assign (make_ssa_name (type1), NOP_EXPR, op);
	    gimple_seq_add_stmt_without_update (&seq, g);
	    op = gimple_assign_lhs (g);
	  }
	candidates.pop ();
	if (update_range_test (&ranges[k - 1], NULL, candidates.address (),
			       candidates.length (), opcode, ops, op,
			       seq, ranges[k - 1].in_p, ranges[k - 1].low,
			       ranges[k - 1].high, strict_overflow_p))
	  any_changes = true;
	else
	  gimple_seq_discard (seq);
	if ((b % 4) == 2 && buckets[b] != i)
	  /* There is more work to do for this bucket.  */
	  b--;
      }

  return any_changes;
}

/* Attempt to optimize for signed a and b where b is known to be >= 0:
   a >= 0 && a < b into (unsigned) a < (unsigned) b
   a >= 0 && a <= b into (unsigned) a <= (unsigned) b  */

static bool
optimize_range_tests_var_bound (enum tree_code opcode, int first, int length,
				vec<operand_entry *> *ops,
				struct range_entry *ranges,
				basic_block first_bb)
{
  int i;
  bool any_changes = false;
  hash_map<tree, int> *map = NULL;

  for (i = first; i < length; i++)
    {
      if (ranges[i].exp == NULL_TREE
	  || TREE_CODE (ranges[i].exp) != SSA_NAME
	  || !ranges[i].in_p)
	continue;

      tree type = TREE_TYPE (ranges[i].exp);
      if (!INTEGRAL_TYPE_P (type)
	  || TYPE_UNSIGNED (type)
	  || ranges[i].low == NULL_TREE
	  || !integer_zerop (ranges[i].low)
	  || ranges[i].high != NULL_TREE)
	continue;
      /* EXP >= 0 here.  */
      if (map == NULL)
	map = new hash_map <tree, int>;
      map->put (ranges[i].exp, i);
    }

  if (map == NULL)
    return false;

  for (i = 0; i < length; i++)
    {
      bool in_p = ranges[i].in_p;
      if (ranges[i].low == NULL_TREE
	  || ranges[i].high == NULL_TREE)
	continue;
      if (!integer_zerop (ranges[i].low)
	  || !integer_zerop (ranges[i].high))
	{
	  if (ranges[i].exp
	      && TYPE_PRECISION (TREE_TYPE (ranges[i].exp)) == 1
	      && TYPE_UNSIGNED (TREE_TYPE (ranges[i].exp))
	      && integer_onep (ranges[i].low)
	      && integer_onep (ranges[i].high))
	    in_p = !in_p;
	  else
	    continue;
	}

      gimple *stmt;
      tree_code ccode;
      tree rhs1, rhs2;
      if (ranges[i].exp)
	{
	  if (TREE_CODE (ranges[i].exp) != SSA_NAME)
	    continue;
	  stmt = SSA_NAME_DEF_STMT (ranges[i].exp);
	  if (!is_gimple_assign (stmt))
	    continue;
	  ccode = gimple_assign_rhs_code (stmt);
	  rhs1 = gimple_assign_rhs1 (stmt);
	  rhs2 = gimple_assign_rhs2 (stmt);
	}
      else
	{
	  operand_entry *oe = (*ops)[ranges[i].idx];
	  stmt = last_stmt (BASIC_BLOCK_FOR_FN (cfun, oe->id));
	  if (gimple_code (stmt) != GIMPLE_COND)
	    continue;
	  ccode = gimple_cond_code (stmt);
	  rhs1 = gimple_cond_lhs (stmt);
	  rhs2 = gimple_cond_rhs (stmt);
	}

      if (TREE_CODE (rhs1) != SSA_NAME
	  || rhs2 == NULL_TREE
	  || TREE_CODE (rhs2) != SSA_NAME)
	continue;

      switch (ccode)
	{
	case GT_EXPR:
	case GE_EXPR:
	case LT_EXPR:
	case LE_EXPR:
	  break;
	default:
	  continue;
	}
      if (in_p)
	ccode = invert_tree_comparison (ccode, false);
      switch (ccode)
	{
	case GT_EXPR:
	case GE_EXPR:
	  std::swap (rhs1, rhs2);
	  ccode = swap_tree_comparison (ccode);
	  break;
	case LT_EXPR:
	case LE_EXPR:
	  break;
	default:
	  gcc_unreachable ();
	}

      int *idx = map->get (rhs1);
      if (idx == NULL)
	continue;

      /* maybe_optimize_range_tests allows statements without side-effects
	 in the basic blocks as long as they are consumed in the same bb.
	 Make sure rhs2's def stmt is not among them, otherwise we can't
	 use safely get_nonzero_bits on it.  E.g. in:
	  # RANGE [-83, 1] NONZERO 173
	  # k_32 = PHI <k_47(13), k_12(9)>
	 ...
	  if (k_32 >= 0)
	    goto <bb 5>; [26.46%]
	  else
	    goto <bb 9>; [73.54%]

	  <bb 5> [local count: 140323371]:
	  # RANGE [0, 1] NONZERO 1
	  _5 = (int) k_32;
	  # RANGE [0, 4] NONZERO 4
	  _21 = _5 << 2;
	  # RANGE [0, 4] NONZERO 4
	  iftmp.0_44 = (char) _21;
	  if (k_32 < iftmp.0_44)
	    goto <bb 6>; [84.48%]
	  else
	    goto <bb 9>; [15.52%]
	 the ranges on _5/_21/iftmp.0_44 are flow sensitive, assume that
	 k_32 >= 0.  If we'd optimize k_32 >= 0 to true and k_32 < iftmp.0_44
	 to (unsigned) k_32 < (unsigned) iftmp.0_44, then we would execute
	 those stmts even for negative k_32 and the value ranges would be no
	 longer guaranteed and so the optimization would be invalid.  */
      while (opcode == ERROR_MARK)
	{
	  gimple *g = SSA_NAME_DEF_STMT (rhs2);
	  basic_block bb2 = gimple_bb (g);
	  if (bb2
	      && bb2 != first_bb
	      && dominated_by_p (CDI_DOMINATORS, bb2, first_bb))
	    {
	      /* As an exception, handle a few common cases.  */
	      if (gimple_assign_cast_p (g)
		  && INTEGRAL_TYPE_P (TREE_TYPE (gimple_assign_rhs1 (g))))
		{
		  tree op0 = gimple_assign_rhs1 (g);
		  if (TYPE_UNSIGNED (TREE_TYPE (op0))
		      && (TYPE_PRECISION (TREE_TYPE (rhs2))
			  > TYPE_PRECISION (TREE_TYPE (op0))))
		    /* Zero-extension is always ok.  */
		    break;
		  else if (TYPE_PRECISION (TREE_TYPE (rhs2))
			   == TYPE_PRECISION (TREE_TYPE (op0))
			   && TREE_CODE (op0) == SSA_NAME)
		    {
		      /* Cast from signed to unsigned or vice versa.  Retry
			 with the op0 as new rhs2.  */
		      rhs2 = op0;
		      continue;
		    }
		}
	      else if (is_gimple_assign (g)
		       && gimple_assign_rhs_code (g) == BIT_AND_EXPR
		       && TREE_CODE (gimple_assign_rhs2 (g)) == INTEGER_CST
		       && !wi::neg_p (wi::to_wide (gimple_assign_rhs2 (g))))
		/* Masking with INTEGER_CST with MSB clear is always ok
		   too.  */
		break;
	      rhs2 = NULL_TREE;
	    }
	  break;
	}
      if (rhs2 == NULL_TREE)
	continue;

      wide_int nz = get_nonzero_bits (rhs2);
      if (wi::neg_p (nz))
	continue;

      /* We have EXP < RHS2 or EXP <= RHS2 where EXP >= 0
	 and RHS2 is known to be RHS2 >= 0.  */
      tree utype = unsigned_type_for (TREE_TYPE (rhs1));

      enum warn_strict_overflow_code wc = WARN_STRICT_OVERFLOW_COMPARISON;
      if ((ranges[*idx].strict_overflow_p
	   || ranges[i].strict_overflow_p)
	  && issue_strict_overflow_warning (wc))
	warning_at (gimple_location (stmt), OPT_Wstrict_overflow,
		    "assuming signed overflow does not occur "
		    "when simplifying range test");

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  struct range_entry *r = &ranges[*idx];
	  fprintf (dump_file, "Optimizing range test ");
	  print_generic_expr (dump_file, r->exp);
	  fprintf (dump_file, " +[");
	  print_generic_expr (dump_file, r->low);
	  fprintf (dump_file, ", ");
	  print_generic_expr (dump_file, r->high);
	  fprintf (dump_file, "] and comparison ");
	  print_generic_expr (dump_file, rhs1);
	  fprintf (dump_file, " %s ", op_symbol_code (ccode));
	  print_generic_expr (dump_file, rhs2);
	  fprintf (dump_file, "\n into (");
	  print_generic_expr (dump_file, utype);
	  fprintf (dump_file, ") ");
	  print_generic_expr (dump_file, rhs1);
	  fprintf (dump_file, " %s (", op_symbol_code (ccode));
	  print_generic_expr (dump_file, utype);
	  fprintf (dump_file, ") ");
	  print_generic_expr (dump_file, rhs2);
	  fprintf (dump_file, "\n");
	}

      operand_entry *oe = (*ops)[ranges[i].idx];
      ranges[i].in_p = 0;
      if (opcode == BIT_IOR_EXPR
	  || (opcode == ERROR_MARK && oe->rank == BIT_IOR_EXPR))
	{
	  ranges[i].in_p = 1;
	  ccode = invert_tree_comparison (ccode, false);
	}

      unsigned int uid = gimple_uid (stmt);
      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      gimple *g = gimple_build_assign (make_ssa_name (utype), NOP_EXPR, rhs1);
      gimple_set_uid (g, uid);
      rhs1 = gimple_assign_lhs (g);
      gsi_insert_before (&gsi, g, GSI_SAME_STMT);
      if (!useless_type_conversion_p (utype, TREE_TYPE (rhs2)))
	{
	  g = gimple_build_assign (make_ssa_name (utype), NOP_EXPR, rhs2);
	  gimple_set_uid (g, uid);
	  rhs2 = gimple_assign_lhs (g);
	  gsi_insert_before (&gsi, g, GSI_SAME_STMT);
	}
      if (tree_swap_operands_p (rhs1, rhs2))
	{
	  std::swap (rhs1, rhs2);
	  ccode = swap_tree_comparison (ccode);
	}
      if (gimple_code (stmt) == GIMPLE_COND)
	{
	  gcond *c = as_a <gcond *> (stmt);
	  gimple_cond_set_code (c, ccode);
	  gimple_cond_set_lhs (c, rhs1);
	  gimple_cond_set_rhs (c, rhs2);
	  update_stmt (stmt);
	}
      else
	{
	  tree ctype = oe->op ? TREE_TYPE (oe->op) : boolean_type_node;
	  if (!INTEGRAL_TYPE_P (ctype)
	      || (TREE_CODE (ctype) != BOOLEAN_TYPE
		  && TYPE_PRECISION (ctype) != 1))
	    ctype = boolean_type_node;
	  g = gimple_build_assign (make_ssa_name (ctype), ccode, rhs1, rhs2);
	  gimple_set_uid (g, uid);
	  gsi_insert_before (&gsi, g, GSI_SAME_STMT);
	  if (oe->op && ctype != TREE_TYPE (oe->op))
	    {
	      g = gimple_build_assign (make_ssa_name (TREE_TYPE (oe->op)),
				       NOP_EXPR, gimple_assign_lhs (g));
	      gimple_set_uid (g, uid);
	      gsi_insert_before (&gsi, g, GSI_SAME_STMT);
	    }
	  ranges[i].exp = gimple_assign_lhs (g);
	  oe->op = ranges[i].exp;
	  ranges[i].low = build_zero_cst (TREE_TYPE (ranges[i].exp));
	  ranges[i].high = ranges[i].low;
	}
      ranges[i].strict_overflow_p = false;
      oe = (*ops)[ranges[*idx].idx];
      /* Now change all the other range test immediate uses, so that
	 those tests will be optimized away.  */
      if (opcode == ERROR_MARK)
	{
	  if (oe->op)
	    oe->op = build_int_cst (TREE_TYPE (oe->op),
				    oe->rank == BIT_IOR_EXPR ? 0 : 1);
	  else
	    oe->op = (oe->rank == BIT_IOR_EXPR
		      ? boolean_false_node : boolean_true_node);
	}
      else
	oe->op = error_mark_node;
      ranges[*idx].exp = NULL_TREE;
      ranges[*idx].low = NULL_TREE;
      ranges[*idx].high = NULL_TREE;
      any_changes = true;
    }

  delete map;
  return any_changes;
}

/* Optimize range tests, similarly how fold_range_test optimizes
   it on trees.  The tree code for the binary
   operation between all the operands is OPCODE.
   If OPCODE is ERROR_MARK, optimize_range_tests is called from within
   maybe_optimize_range_tests for inter-bb range optimization.
   In that case if oe->op is NULL, oe->id is bb->index whose
   GIMPLE_COND is && or ||ed into the test, and oe->rank says
   the actual opcode.
   FIRST_BB is the first basic block if OPCODE is ERROR_MARK.  */

static bool
optimize_range_tests (enum tree_code opcode,
		      vec<operand_entry *> *ops, basic_block first_bb)
{
  unsigned int length = ops->length (), i, j, first;
  operand_entry *oe;
  struct range_entry *ranges;
  bool any_changes = false;

  if (length == 1)
    return false;

  ranges = XNEWVEC (struct range_entry, length);
  for (i = 0; i < length; i++)
    {
      oe = (*ops)[i];
      ranges[i].idx = i;
      init_range_entry (ranges + i, oe->op,
			oe->op
			? NULL
			: last_stmt (BASIC_BLOCK_FOR_FN (cfun, oe->id)));
      /* For | invert it now, we will invert it again before emitting
	 the optimized expression.  */
      if (opcode == BIT_IOR_EXPR
	  || (opcode == ERROR_MARK && oe->rank == BIT_IOR_EXPR))
	ranges[i].in_p = !ranges[i].in_p;
    }

  qsort (ranges, length, sizeof (*ranges), range_entry_cmp);
  for (i = 0; i < length; i++)
    if (ranges[i].exp != NULL_TREE && TREE_CODE (ranges[i].exp) == SSA_NAME)
      break;

  /* Try to merge ranges.  */
  for (first = i; i < length; i++)
    {
      tree low = ranges[i].low;
      tree high = ranges[i].high;
      int in_p = ranges[i].in_p;
      bool strict_overflow_p = ranges[i].strict_overflow_p;
      int update_fail_count = 0;

      for (j = i + 1; j < length; j++)
	{
	  if (ranges[i].exp != ranges[j].exp)
	    break;
	  if (!merge_ranges (&in_p, &low, &high, in_p, low, high,
			     ranges[j].in_p, ranges[j].low, ranges[j].high))
	    break;
	  strict_overflow_p |= ranges[j].strict_overflow_p;
	}

      if (j == i + 1)
	continue;

      if (update_range_test (ranges + i, ranges + i + 1, NULL, j - i - 1,
			     opcode, ops, ranges[i].exp, NULL, in_p,
			     low, high, strict_overflow_p))
	{
	  i = j - 1;
	  any_changes = true;
	}
      /* Avoid quadratic complexity if all merge_ranges calls would succeed,
	 while update_range_test would fail.  */
      else if (update_fail_count == 64)
	i = j - 1;
      else
	++update_fail_count;
    }

  any_changes |= optimize_range_tests_1 (opcode, first, length, true,
					 ops, ranges);

  if (BRANCH_COST (optimize_function_for_speed_p (cfun), false) >= 2)
    any_changes |= optimize_range_tests_1 (opcode, first, length, false,
					   ops, ranges);
  if (lshift_cheap_p (optimize_function_for_speed_p (cfun)))
    any_changes |= optimize_range_tests_to_bit_test (opcode, first, length,
						     ops, ranges);
  any_changes |= optimize_range_tests_var_bound (opcode, first, length, ops,
						 ranges, first_bb);
  any_changes |= optimize_range_tests_cmp_bitwise (opcode, first, length,
						   ops, ranges);

  if (any_changes && opcode != ERROR_MARK)
    {
      j = 0;
      FOR_EACH_VEC_ELT (*ops, i, oe)
	{
	  if (oe->op == error_mark_node)
	    continue;
	  else if (i != j)
	    (*ops)[j] = oe;
	  j++;
	}
      ops->truncate (j);
    }

  XDELETEVEC (ranges);
  return any_changes;
}

/* A subroutine of optimize_vec_cond_expr to extract and canonicalize
   the operands of the VEC_COND_EXPR.  Returns ERROR_MARK on failure,
   otherwise the comparison code.  TYPE is a return value that is set
   to type of comparison.  */

static tree_code
ovce_extract_ops (tree var, gassign **rets, bool *reti, tree *type,
		  tree *lhs, tree *rhs, gassign **vcond)
{
  if (TREE_CODE (var) != SSA_NAME)
    return ERROR_MARK;

  gassign *stmt = dyn_cast <gassign *> (SSA_NAME_DEF_STMT (var));
  if (stmt == NULL)
    return ERROR_MARK;
  if (vcond)
    *vcond = stmt;

  /* ??? If we start creating more COND_EXPR, we could perform
     this same optimization with them.	For now, simplify.  */
  if (gimple_assign_rhs_code (stmt) != VEC_COND_EXPR)
    return ERROR_MARK;

  tree cond = gimple_assign_rhs1 (stmt);
  tree_code cmp = TREE_CODE (cond);
  if (cmp != SSA_NAME)
    return ERROR_MARK;

  gassign *assign = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (cond));
  if (assign == NULL
      || TREE_CODE_CLASS (gimple_assign_rhs_code (assign)) != tcc_comparison)
    return ERROR_MARK;

  cmp = gimple_assign_rhs_code (assign);
  if (lhs)
    *lhs = gimple_assign_rhs1 (assign);
  if (rhs)
    *rhs = gimple_assign_rhs2 (assign);

  /* ??? For now, allow only canonical true and false result vectors.
     We could expand this to other constants should the need arise,
     but at the moment we don't create them.  */
  tree t = gimple_assign_rhs2 (stmt);
  tree f = gimple_assign_rhs3 (stmt);
  bool inv;
  if (integer_all_onesp (t))
    inv = false;
  else if (integer_all_onesp (f))
    {
      cmp = invert_tree_comparison (cmp, false);
      inv = true;
    }
  else
    return ERROR_MARK;
  if (!integer_zerop (f))
    return ERROR_MARK;

  /* Success!  */
  if (rets)
    *rets = assign;
  if (reti)
    *reti = inv;
  if (type)
    *type = TREE_TYPE (cond);
  return cmp;
}

/* Optimize the condition of VEC_COND_EXPRs which have been combined
   with OPCODE (either BIT_AND_EXPR or BIT_IOR_EXPR).  */

static bool
optimize_vec_cond_expr (tree_code opcode, vec<operand_entry *> *ops)
{
  unsigned int length = ops->length (), i, j;
  bool any_changes = false;

  if (length == 1)
    return false;

  for (i = 0; i < length; ++i)
    {
      tree elt0 = (*ops)[i]->op;

      gassign *stmt0, *vcond0;
      bool invert;
      tree type, lhs0, rhs0;
      tree_code cmp0 = ovce_extract_ops (elt0, &stmt0, &invert, &type, &lhs0,
					 &rhs0, &vcond0);
      if (cmp0 == ERROR_MARK)
	continue;

      for (j = i + 1; j < length; ++j)
	{
	  tree &elt1 = (*ops)[j]->op;

	  gassign *stmt1, *vcond1;
	  tree lhs1, rhs1;
	  tree_code cmp1 = ovce_extract_ops (elt1, &stmt1, NULL, NULL, &lhs1,
					     &rhs1, &vcond1);
	  if (cmp1 == ERROR_MARK)
	    continue;

	  tree comb;
	  if (opcode == BIT_AND_EXPR)
	    comb = maybe_fold_and_comparisons (type, cmp0, lhs0, rhs0,
					       cmp1, lhs1, rhs1);
	  else if (opcode == BIT_IOR_EXPR)
	    comb = maybe_fold_or_comparisons (type, cmp0, lhs0, rhs0,
					      cmp1, lhs1, rhs1);
	  else
	    gcc_unreachable ();
	  if (comb == NULL)
	    continue;

	  /* Success! */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Transforming ");
	      print_generic_expr (dump_file, gimple_assign_lhs (stmt0));
	      fprintf (dump_file, " %c ", opcode == BIT_AND_EXPR ? '&' : '|');
	      print_generic_expr (dump_file, gimple_assign_lhs (stmt1));
	      fprintf (dump_file, " into ");
	      print_generic_expr (dump_file, comb);
	      fputc ('\n', dump_file);
	    }

	  gimple_stmt_iterator gsi = gsi_for_stmt (vcond0);
	  tree exp = force_gimple_operand_gsi (&gsi, comb, true, NULL_TREE,
					       true, GSI_SAME_STMT);
	  if (invert)
	    swap_ssa_operands (vcond0, gimple_assign_rhs2_ptr (vcond0),
			       gimple_assign_rhs3_ptr (vcond0));
	  gimple_assign_set_rhs1 (vcond0, exp);
	  update_stmt (vcond0);

	  elt1 = error_mark_node;
	  any_changes = true;
	}
    }

  if (any_changes)
    {
      operand_entry *oe;
      j = 0;
      FOR_EACH_VEC_ELT (*ops, i, oe)
	{
	  if (oe->op == error_mark_node)
	    continue;
	  else if (i != j)
	    (*ops)[j] = oe;
	  j++;
	}
      ops->truncate (j);
    }

  return any_changes;
}

/* Return true if STMT is a cast like:
   <bb N>:
   ...
   _123 = (int) _234;

   <bb M>:
   # _345 = PHI <_123(N), 1(...), 1(...)>
   where _234 has bool type, _123 has single use and
   bb N has a single successor M.  This is commonly used in
   the last block of a range test.

   Also Return true if STMT is tcc_compare like:
   <bb N>:
   ...
   _234 = a_2(D) == 2;

   <bb M>:
   # _345 = PHI <_234(N), 1(...), 1(...)>
   _346 = (int) _345;
   where _234 has booltype, single use and
   bb N has a single successor M.  This is commonly used in
   the last block of a range test.  */

static bool
final_range_test_p (gimple *stmt)
{
  basic_block bb, rhs_bb, lhs_bb;
  edge e;
  tree lhs, rhs;
  use_operand_p use_p;
  gimple *use_stmt;

  if (!gimple_assign_cast_p (stmt)
      && (!is_gimple_assign (stmt)
	  || (TREE_CODE_CLASS (gimple_assign_rhs_code (stmt))
	      != tcc_comparison)))
    return false;
  bb = gimple_bb (stmt);
  if (!single_succ_p (bb))
    return false;
  e = single_succ_edge (bb);
  if (e->flags & EDGE_COMPLEX)
    return false;

  lhs = gimple_assign_lhs (stmt);
  rhs = gimple_assign_rhs1 (stmt);
  if (gimple_assign_cast_p (stmt)
      && (!INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	  || TREE_CODE (rhs) != SSA_NAME
	  || TREE_CODE (TREE_TYPE (rhs)) != BOOLEAN_TYPE))
    return false;

  if (!gimple_assign_cast_p (stmt)
      && (TREE_CODE (TREE_TYPE (lhs)) != BOOLEAN_TYPE))
      return false;

  /* Test whether lhs is consumed only by a PHI in the only successor bb.  */
  if (!single_imm_use (lhs, &use_p, &use_stmt))
    return false;

  if (gimple_code (use_stmt) != GIMPLE_PHI
      || gimple_bb (use_stmt) != e->dest)
    return false;

  /* And that the rhs is defined in the same loop.  */
  if (gimple_assign_cast_p (stmt))
    {
      if (TREE_CODE (rhs) != SSA_NAME
	  || !(rhs_bb = gimple_bb (SSA_NAME_DEF_STMT (rhs)))
	  || !flow_bb_inside_loop_p (loop_containing_stmt (stmt), rhs_bb))
	return false;
    }
  else
    {
      if (TREE_CODE (lhs) != SSA_NAME
	  || !(lhs_bb = gimple_bb (SSA_NAME_DEF_STMT (lhs)))
	  || !flow_bb_inside_loop_p (loop_containing_stmt (stmt), lhs_bb))
	return false;
    }

  return true;
}

/* Return true if BB is suitable basic block for inter-bb range test
   optimization.  If BACKWARD is true, BB should be the only predecessor
   of TEST_BB, and *OTHER_BB is either NULL and filled by the routine,
   or compared with to find a common basic block to which all conditions
   branch to if true resp. false.  If BACKWARD is false, TEST_BB should
   be the only predecessor of BB.  *TEST_SWAPPED_P is set to true if
   TEST_BB is a bb ending in condition where the edge to non-*OTHER_BB
   block points to an empty block that falls through into *OTHER_BB and
   the phi args match that path.  */

static bool
suitable_cond_bb (basic_block bb, basic_block test_bb, basic_block *other_bb,
		  bool *test_swapped_p, bool backward)
{
  edge_iterator ei, ei2;
  edge e, e2;
  gimple *stmt;
  gphi_iterator gsi;
  bool other_edge_seen = false;
  bool is_cond;

  if (test_bb == bb)
    return false;
  /* Check last stmt first.  */
  stmt = last_stmt (bb);
  if (stmt == NULL
      || (gimple_code (stmt) != GIMPLE_COND
	  && (backward || !final_range_test_p (stmt)))
      || gimple_visited_p (stmt)
      || stmt_could_throw_p (cfun, stmt)
      || *other_bb == bb)
    return false;
  is_cond = gimple_code (stmt) == GIMPLE_COND;
  if (is_cond)
    {
      /* If last stmt is GIMPLE_COND, verify that one of the succ edges
	 goes to the next bb (if BACKWARD, it is TEST_BB), and the other
	 to *OTHER_BB (if not set yet, try to find it out).  */
      if (EDGE_COUNT (bb->succs) != 2)
	return false;
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (!(e->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
	    return false;
	  if (e->dest == test_bb)
	    {
	      if (backward)
		continue;
	      else
		return false;
	    }
	  if (e->dest == bb)
	    return false;
	  if (*other_bb == NULL)
	    {
	      FOR_EACH_EDGE (e2, ei2, test_bb->succs)
		if (!(e2->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
		  return false;
		else if (e->dest == e2->dest)
		  *other_bb = e->dest;
	      if (*other_bb == NULL)
		return false;
	    }
	  if (e->dest == *other_bb)
	    other_edge_seen = true;
	  else if (backward)
	    return false;
	}
      if (*other_bb == NULL || !other_edge_seen)
	return false;
    }
  else if (single_succ (bb) != *other_bb)
    return false;

  /* Now check all PHIs of *OTHER_BB.  */
  e = find_edge (bb, *other_bb);
  e2 = find_edge (test_bb, *other_bb);
 retry:;
  for (gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      /* If both BB and TEST_BB end with GIMPLE_COND, all PHI arguments
	 corresponding to BB and TEST_BB predecessor must be the same.  */
      if (!operand_equal_p (gimple_phi_arg_def (phi, e->dest_idx),
			    gimple_phi_arg_def (phi, e2->dest_idx), 0))
	{
	  /* Otherwise, if one of the blocks doesn't end with GIMPLE_COND,
	     one of the PHIs should have the lhs of the last stmt in
	     that block as PHI arg and that PHI should have 0 or 1
	     corresponding to it in all other range test basic blocks
	     considered.  */
	  if (!is_cond)
	    {
	      if (gimple_phi_arg_def (phi, e->dest_idx)
		  == gimple_assign_lhs (stmt)
		  && (integer_zerop (gimple_phi_arg_def (phi, e2->dest_idx))
		      || integer_onep (gimple_phi_arg_def (phi,
							   e2->dest_idx))))
		continue;
	    }
	  else
	    {
	      gimple *test_last = last_stmt (test_bb);
	      if (gimple_code (test_last) == GIMPLE_COND)
		{
		  if (backward ? e2->src != test_bb : e->src != bb)
		    return false;

		  /* For last_bb, handle also:
		     if (x_3(D) == 3)
		       goto <bb 6>; [34.00%]
		     else
		       goto <bb 7>; [66.00%]

		     <bb 6> [local count: 79512730]:

		     <bb 7> [local count: 1073741824]:
		     # prephitmp_7 = PHI <1(3), 1(4), 0(5), 1(2), 1(6)>
		     where bb 7 is *OTHER_BB, but the PHI values from the
		     earlier bbs match the path through the empty bb
		     in between.  */
		  edge e3;
		  if (backward)
		    e3 = EDGE_SUCC (test_bb,
				    e2 == EDGE_SUCC (test_bb, 0) ? 1 : 0);
		  else
		    e3 = EDGE_SUCC (bb,
				    e == EDGE_SUCC (bb, 0) ? 1 : 0);
		  if (empty_block_p (e3->dest)
		      && single_succ_p (e3->dest)
		      && single_succ (e3->dest) == *other_bb
		      && single_pred_p (e3->dest)
		      && single_succ_edge (e3->dest)->flags == EDGE_FALLTHRU)
		    {
		      if (backward)
			e2 = single_succ_edge (e3->dest);
		      else
			e = single_succ_edge (e3->dest);
		      if (test_swapped_p)
			*test_swapped_p = true;
		      goto retry;
		    }
		}
	      else if (gimple_phi_arg_def (phi, e2->dest_idx)
		       == gimple_assign_lhs (test_last)
		       && (integer_zerop (gimple_phi_arg_def (phi,
							      e->dest_idx))
			   || integer_onep (gimple_phi_arg_def (phi,
								e->dest_idx))))
		continue;
	    }

	  return false;
	}
    }
  return true;
}

/* Return true if BB doesn't have side-effects that would disallow
   range test optimization, all SSA_NAMEs set in the bb are consumed
   in the bb and there are no PHIs.  */

bool
no_side_effect_bb (basic_block bb)
{
  gimple_stmt_iterator gsi;
  gimple *last;

  if (!gimple_seq_empty_p (phi_nodes (bb)))
    return false;
  last = last_stmt (bb);
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      tree lhs;
      imm_use_iterator imm_iter;
      use_operand_p use_p;

      if (is_gimple_debug (stmt))
	continue;
      if (gimple_has_side_effects (stmt))
	return false;
      if (stmt == last)
	return true;
      if (!is_gimple_assign (stmt))
	return false;
      lhs = gimple_assign_lhs (stmt);
      if (TREE_CODE (lhs) != SSA_NAME)
	return false;
      if (gimple_assign_rhs_could_trap_p (stmt))
	return false;
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs)
	{
	  gimple *use_stmt = USE_STMT (use_p);
	  if (is_gimple_debug (use_stmt))
	    continue;
	  if (gimple_bb (use_stmt) != bb)
	    return false;
	}
    }
  return false;
}

/* If VAR is set by CODE (BIT_{AND,IOR}_EXPR) which is reassociable,
   return true and fill in *OPS recursively.  */

static bool
get_ops (tree var, enum tree_code code, vec<operand_entry *> *ops,
	 class loop *loop)
{
  gimple *stmt = SSA_NAME_DEF_STMT (var);
  tree rhs[2];
  int i;

  if (!is_reassociable_op (stmt, code, loop))
    return false;

  rhs[0] = gimple_assign_rhs1 (stmt);
  rhs[1] = gimple_assign_rhs2 (stmt);
  gimple_set_visited (stmt, true);
  for (i = 0; i < 2; i++)
    if (TREE_CODE (rhs[i]) == SSA_NAME
	&& !get_ops (rhs[i], code, ops, loop)
	&& has_single_use (rhs[i]))
      {
	operand_entry *oe = operand_entry_pool.allocate ();

	oe->op = rhs[i];
	oe->rank = code;
	oe->id = 0;
	oe->count = 1;
	oe->stmt_to_insert = NULL;
	ops->safe_push (oe);
      }
  return true;
}

/* Find the ops that were added by get_ops starting from VAR, see if
   they were changed during update_range_test and if yes, create new
   stmts.  */

static tree
update_ops (tree var, enum tree_code code, const vec<operand_entry *> &ops,
	    unsigned int *pidx, class loop *loop)
{
  gimple *stmt = SSA_NAME_DEF_STMT (var);
  tree rhs[4];
  int i;

  if (!is_reassociable_op (stmt, code, loop))
    return NULL;

  rhs[0] = gimple_assign_rhs1 (stmt);
  rhs[1] = gimple_assign_rhs2 (stmt);
  rhs[2] = rhs[0];
  rhs[3] = rhs[1];
  for (i = 0; i < 2; i++)
    if (TREE_CODE (rhs[i]) == SSA_NAME)
      {
	rhs[2 + i] = update_ops (rhs[i], code, ops, pidx, loop);
	if (rhs[2 + i] == NULL_TREE)
	  {
	    if (has_single_use (rhs[i]))
	      rhs[2 + i] = ops[(*pidx)++]->op;
	    else
	      rhs[2 + i] = rhs[i];
	  }
      }
  if ((rhs[2] != rhs[0] || rhs[3] != rhs[1])
      && (rhs[2] != rhs[1] || rhs[3] != rhs[0]))
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      var = make_ssa_name (TREE_TYPE (var));
      gassign *g = gimple_build_assign (var, gimple_assign_rhs_code (stmt),
					rhs[2], rhs[3]);
      gimple_set_uid (g, gimple_uid (stmt));
      gimple_set_visited (g, true);
      gsi_insert_before (&gsi, g, GSI_SAME_STMT);
    }
  return var;
}

/* Structure to track the initial value passed to get_ops and
   the range in the ops vector for each basic block.  */

struct inter_bb_range_test_entry
{
  tree op;
  unsigned int first_idx, last_idx;
};

/* Inter-bb range test optimization.

   Returns TRUE if a gimple conditional is optimized to a true/false,
   otherwise return FALSE.

   This indicates to the caller that it should run a CFG cleanup pass
   once reassociation is completed.  */

static bool
maybe_optimize_range_tests (gimple *stmt)
{
  basic_block first_bb = gimple_bb (stmt);
  basic_block last_bb = first_bb;
  basic_block other_bb = NULL;
  basic_block bb;
  edge_iterator ei;
  edge e;
  auto_vec<operand_entry *> ops;
  auto_vec<inter_bb_range_test_entry> bbinfo;
  bool any_changes = false;
  bool cfg_cleanup_needed = false;

  /* Consider only basic blocks that end with GIMPLE_COND or
     a cast statement satisfying final_range_test_p.  All
     but the last bb in the first_bb .. last_bb range
     should end with GIMPLE_COND.  */
  if (gimple_code (stmt) == GIMPLE_COND)
    {
      if (EDGE_COUNT (first_bb->succs) != 2)
	return cfg_cleanup_needed;
    }
  else if (final_range_test_p (stmt))
    other_bb = single_succ (first_bb);
  else
    return cfg_cleanup_needed;

  if (stmt_could_throw_p (cfun, stmt))
    return cfg_cleanup_needed;

  /* As relative ordering of post-dominator sons isn't fixed,
     maybe_optimize_range_tests can be called first on any
     bb in the range we want to optimize.  So, start searching
     backwards, if first_bb can be set to a predecessor.  */
  while (single_pred_p (first_bb))
    {
      basic_block pred_bb = single_pred (first_bb);
      if (!suitable_cond_bb (pred_bb, first_bb, &other_bb, NULL, true))
	break;
      if (!no_side_effect_bb (first_bb))
	break;
      first_bb = pred_bb;
    }
  /* If first_bb is last_bb, other_bb hasn't been computed yet.
     Before starting forward search in last_bb successors, find
     out the other_bb.  */
  if (first_bb == last_bb)
    {
      other_bb = NULL;
      /* As non-GIMPLE_COND last stmt always terminates the range,
	 if forward search didn't discover anything, just give up.  */
      if (gimple_code (stmt) != GIMPLE_COND)
	return cfg_cleanup_needed;
      /* Look at both successors.  Either it ends with a GIMPLE_COND
	 and satisfies suitable_cond_bb, or ends with a cast and
	 other_bb is that cast's successor.  */
      FOR_EACH_EDGE (e, ei, first_bb->succs)
	if (!(e->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE))
	    || e->dest == first_bb)
	  return cfg_cleanup_needed;
	else if (single_pred_p (e->dest))
	  {
	    stmt = last_stmt (e->dest);
	    if (stmt
		&& gimple_code (stmt) == GIMPLE_COND
		&& EDGE_COUNT (e->dest->succs) == 2)
	      {
		if (suitable_cond_bb (first_bb, e->dest, &other_bb,
				      NULL, true))
		  break;
		else
		  other_bb = NULL;
	      }
	    else if (stmt
		     && final_range_test_p (stmt)
		     && find_edge (first_bb, single_succ (e->dest)))
	      {
		other_bb = single_succ (e->dest);
		if (other_bb == first_bb)
		  other_bb = NULL;
	      }
	  }
      if (other_bb == NULL)
	return cfg_cleanup_needed;
    }
  /* Now do the forward search, moving last_bb to successor bbs
     that aren't other_bb.  */
  while (EDGE_COUNT (last_bb->succs) == 2)
    {
      FOR_EACH_EDGE (e, ei, last_bb->succs)
	if (e->dest != other_bb)
	  break;
      if (e == NULL)
	break;
      if (!single_pred_p (e->dest))
	break;
      if (!suitable_cond_bb (e->dest, last_bb, &other_bb, NULL, false))
	break;
      if (!no_side_effect_bb (e->dest))
	break;
      last_bb = e->dest;
    }
  if (first_bb == last_bb)
    return cfg_cleanup_needed;
  /* Here basic blocks first_bb through last_bb's predecessor
     end with GIMPLE_COND, all of them have one of the edges to
     other_bb and another to another block in the range,
     all blocks except first_bb don't have side-effects and
     last_bb ends with either GIMPLE_COND, or cast satisfying
     final_range_test_p.  */
  for (bb = last_bb; ; bb = single_pred (bb))
    {
      enum tree_code code;
      tree lhs, rhs;
      inter_bb_range_test_entry bb_ent;

      bb_ent.op = NULL_TREE;
      bb_ent.first_idx = ops.length ();
      bb_ent.last_idx = bb_ent.first_idx;
      e = find_edge (bb, other_bb);
      stmt = last_stmt (bb);
      gimple_set_visited (stmt, true);
      if (gimple_code (stmt) != GIMPLE_COND)
	{
	  use_operand_p use_p;
	  gimple *phi;
	  edge e2;
	  unsigned int d;

	  lhs = gimple_assign_lhs (stmt);
	  rhs = gimple_assign_rhs1 (stmt);
	  gcc_assert (bb == last_bb);

	  /* stmt is
	     _123 = (int) _234;
	     OR
	     _234 = a_2(D) == 2;

	     followed by:
	     <bb M>:
	     # _345 = PHI <_123(N), 1(...), 1(...)>

	     or 0 instead of 1.  If it is 0, the _234
	     range test is anded together with all the
	     other range tests, if it is 1, it is ored with
	     them.  */
	  single_imm_use (lhs, &use_p, &phi);
	  gcc_assert (gimple_code (phi) == GIMPLE_PHI);
	  e2 = find_edge (first_bb, other_bb);
	  d = e2->dest_idx;
	  gcc_assert (gimple_phi_arg_def (phi, e->dest_idx) == lhs);
	  if (integer_zerop (gimple_phi_arg_def (phi, d)))
	    code = BIT_AND_EXPR;
	  else
	    {
	      gcc_checking_assert (integer_onep (gimple_phi_arg_def (phi, d)));
	      code = BIT_IOR_EXPR;
	    }

	  /* If _234 SSA_NAME_DEF_STMT is
	     _234 = _567 | _789;
	     (or &, corresponding to 1/0 in the phi arguments,
	     push into ops the individual range test arguments
	     of the bitwise or resp. and, recursively.  */
	  if (TREE_CODE (rhs) == SSA_NAME
	      && (TREE_CODE_CLASS (gimple_assign_rhs_code (stmt))
		  != tcc_comparison)
	      && !get_ops (rhs, code, &ops,
			   loop_containing_stmt (stmt))
	      && has_single_use (rhs))
	    {
	      /* Otherwise, push the _234 range test itself.  */
	      operand_entry *oe = operand_entry_pool.allocate ();

	      oe->op = rhs;
	      oe->rank = code;
	      oe->id = 0;
	      oe->count = 1;
	      oe->stmt_to_insert = NULL;
	      ops.safe_push (oe);
	      bb_ent.last_idx++;
	      bb_ent.op = rhs;
	    }
	  else if (is_gimple_assign (stmt)
		   && (TREE_CODE_CLASS (gimple_assign_rhs_code (stmt))
		       == tcc_comparison)
		   && !get_ops (lhs, code, &ops,
				loop_containing_stmt (stmt))
		   && has_single_use (lhs))
	    {
	      operand_entry *oe = operand_entry_pool.allocate ();
	      oe->op = lhs;
	      oe->rank = code;
	      oe->id = 0;
	      oe->count = 1;
	      ops.safe_push (oe);
	      bb_ent.last_idx++;
	      bb_ent.op = lhs;
	    }
	  else
	    {
	      bb_ent.last_idx = ops.length ();
	      bb_ent.op = rhs;
	    }
	  bbinfo.safe_push (bb_ent);
	  for (unsigned int i = bb_ent.first_idx; i < bb_ent.last_idx; ++i)
	    ops[i]->id = bb->index;
	  continue;
	}
      else if (bb == last_bb)
	{
	  /* For last_bb, handle also:
	     if (x_3(D) == 3)
	       goto <bb 6>; [34.00%]
	     else
	       goto <bb 7>; [66.00%]

	     <bb 6> [local count: 79512730]:

	     <bb 7> [local count: 1073741824]:
	     # prephitmp_7 = PHI <1(3), 1(4), 0(5), 1(2), 1(6)>
	     where bb 7 is OTHER_BB, but the PHI values from the
	     earlier bbs match the path through the empty bb
	     in between.  */
	  bool test_swapped_p = false;
	  bool ok = suitable_cond_bb (single_pred (last_bb), last_bb,
				      &other_bb, &test_swapped_p, true);
	  gcc_assert (ok);
	  if (test_swapped_p)
	    e = EDGE_SUCC (bb, e == EDGE_SUCC (bb, 0) ? 1 : 0);
	}
      /* Otherwise stmt is GIMPLE_COND.  */
      code = gimple_cond_code (stmt);
      lhs = gimple_cond_lhs (stmt);
      rhs = gimple_cond_rhs (stmt);
      if (TREE_CODE (lhs) == SSA_NAME
	  && INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	  && ((code != EQ_EXPR && code != NE_EXPR)
	      || rhs != boolean_false_node
		 /* Either push into ops the individual bitwise
		    or resp. and operands, depending on which
		    edge is other_bb.  */
	      || !get_ops (lhs, (((e->flags & EDGE_TRUE_VALUE) == 0)
				 ^ (code == EQ_EXPR))
				? BIT_AND_EXPR : BIT_IOR_EXPR, &ops,
			   loop_containing_stmt (stmt))))
	{
	  /* Or push the GIMPLE_COND stmt itself.  */
	  operand_entry *oe = operand_entry_pool.allocate ();

	  oe->op = NULL;
	  oe->rank = (e->flags & EDGE_TRUE_VALUE)
		     ? BIT_IOR_EXPR : BIT_AND_EXPR;
	  /* oe->op = NULL signs that there is no SSA_NAME
	     for the range test, and oe->id instead is the
	     basic block number, at which's end the GIMPLE_COND
	     is.  */
	  oe->id = bb->index;
	  oe->count = 1;
	  oe->stmt_to_insert = NULL;
	  ops.safe_push (oe);
	  bb_ent.op = NULL;
	  bb_ent.last_idx++;
	}
      else if (ops.length () > bb_ent.first_idx)
	{
	  bb_ent.op = lhs;
	  bb_ent.last_idx = ops.length ();
	}
      bbinfo.safe_push (bb_ent);
      for (unsigned int i = bb_ent.first_idx; i < bb_ent.last_idx; ++i)
	ops[i]->id = bb->index;
      if (bb == first_bb)
	break;
    }
  if (ops.length () > 1)
    any_changes = optimize_range_tests (ERROR_MARK, &ops, first_bb);
  if (any_changes)
    {
      unsigned int idx, max_idx = 0;
      /* update_ops relies on has_single_use predicates returning the
	 same values as it did during get_ops earlier.  Additionally it
	 never removes statements, only adds new ones and it should walk
	 from the single imm use and check the predicate already before
	 making those changes.
	 On the other side, the handling of GIMPLE_COND directly can turn
	 previously multiply used SSA_NAMEs into single use SSA_NAMEs, so
	 it needs to be done in a separate loop afterwards.  */
      for (bb = last_bb, idx = 0; ; bb = single_pred (bb), idx++)
	{
	  if (bbinfo[idx].first_idx < bbinfo[idx].last_idx
	      && bbinfo[idx].op != NULL_TREE)
	    {
	      tree new_op;

	      max_idx = idx;
	      stmt = last_stmt (bb);
	      new_op = update_ops (bbinfo[idx].op,
				   (enum tree_code)
				   ops[bbinfo[idx].first_idx]->rank,
				   ops, &bbinfo[idx].first_idx,
				   loop_containing_stmt (stmt));
	      if (new_op == NULL_TREE)
		{
		  gcc_assert (bb == last_bb);
		  new_op = ops[bbinfo[idx].first_idx++]->op;
		}
	      if (bbinfo[idx].op != new_op)
		{
		  imm_use_iterator iter;
		  use_operand_p use_p;
		  gimple *use_stmt, *cast_or_tcc_cmp_stmt = NULL;

		  FOR_EACH_IMM_USE_STMT (use_stmt, iter, bbinfo[idx].op)
		    if (is_gimple_debug (use_stmt))
		      continue;
		    else if (gimple_code (use_stmt) == GIMPLE_COND
			     || gimple_code (use_stmt) == GIMPLE_PHI)
		      FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
			SET_USE (use_p, new_op);
		    else if ((is_gimple_assign (use_stmt)
			      && (TREE_CODE_CLASS
				  (gimple_assign_rhs_code (use_stmt))
				  == tcc_comparison)))
		      cast_or_tcc_cmp_stmt = use_stmt;
		    else if (gimple_assign_cast_p (use_stmt))
		      cast_or_tcc_cmp_stmt = use_stmt;
		    else
		      gcc_unreachable ();

		  if (cast_or_tcc_cmp_stmt)
		    {
		      gcc_assert (bb == last_bb);
		      tree lhs = gimple_assign_lhs (cast_or_tcc_cmp_stmt);
		      tree new_lhs = make_ssa_name (TREE_TYPE (lhs));
		      enum tree_code rhs_code
			= gimple_assign_cast_p (cast_or_tcc_cmp_stmt)
			? gimple_assign_rhs_code (cast_or_tcc_cmp_stmt)
			: CONVERT_EXPR;
		      gassign *g;
		      if (is_gimple_min_invariant (new_op))
			{
			  new_op = fold_convert (TREE_TYPE (lhs), new_op);
			  g = gimple_build_assign (new_lhs, new_op);
			}
		      else
			g = gimple_build_assign (new_lhs, rhs_code, new_op);
		      gimple_stmt_iterator gsi
			= gsi_for_stmt (cast_or_tcc_cmp_stmt);
		      gimple_set_uid (g, gimple_uid (cast_or_tcc_cmp_stmt));
		      gimple_set_visited (g, true);
		      gsi_insert_before (&gsi, g, GSI_SAME_STMT);
		      FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
			if (is_gimple_debug (use_stmt))
			  continue;
			else if (gimple_code (use_stmt) == GIMPLE_COND
				 || gimple_code (use_stmt) == GIMPLE_PHI)
			  FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
			    SET_USE (use_p, new_lhs);
			else
			  gcc_unreachable ();
		    }
		}
	    }
	  if (bb == first_bb)
	    break;
	}
      for (bb = last_bb, idx = 0; ; bb = single_pred (bb), idx++)
	{
	  if (bbinfo[idx].first_idx < bbinfo[idx].last_idx
	      && bbinfo[idx].op == NULL_TREE
	      && ops[bbinfo[idx].first_idx]->op != NULL_TREE)
	    {
	      gcond *cond_stmt = as_a <gcond *> (last_stmt (bb));

	      if (idx > max_idx)
		max_idx = idx;

	      /* If we collapse the conditional to a true/false
		 condition, then bubble that knowledge up to our caller.  */
	      if (integer_zerop (ops[bbinfo[idx].first_idx]->op))
		{
		  gimple_cond_make_false (cond_stmt);
		  cfg_cleanup_needed = true;
		}
	      else if (integer_onep (ops[bbinfo[idx].first_idx]->op))
		{
		  gimple_cond_make_true (cond_stmt);
		  cfg_cleanup_needed = true;
		}
	      else
		{
		  gimple_cond_set_code (cond_stmt, NE_EXPR);
		  gimple_cond_set_lhs (cond_stmt,
				       ops[bbinfo[idx].first_idx]->op);
		  gimple_cond_set_rhs (cond_stmt, boolean_false_node);
		}
	      update_stmt (cond_stmt);
	    }
	  if (bb == first_bb)
	    break;
	}

      /* The above changes could result in basic blocks after the first
	 modified one, up to and including last_bb, to be executed even if
	 they would not be in the original program.  If the value ranges of
	 assignment lhs' in those bbs were dependent on the conditions
	 guarding those basic blocks which now can change, the VRs might
	 be incorrect.  As no_side_effect_bb should ensure those SSA_NAMEs
	 are only used within the same bb, it should be not a big deal if
	 we just reset all the VRs in those bbs.  See PR68671.  */
      for (bb = last_bb, idx = 0; idx < max_idx; bb = single_pred (bb), idx++)
	reset_flow_sensitive_info_in_bb (bb);
    }
  return cfg_cleanup_needed;
}

/* Return true if OPERAND is defined by a PHI node which uses the LHS
   of STMT in it's operands.  This is also known as a "destructive
   update" operation.  */

static bool
is_phi_for_stmt (gimple *stmt, tree operand)
{
  gimple *def_stmt;
  gphi *def_phi;
  tree lhs;
  use_operand_p arg_p;
  ssa_op_iter i;

  if (TREE_CODE (operand) != SSA_NAME)
    return false;

  lhs = gimple_assign_lhs (stmt);

  def_stmt = SSA_NAME_DEF_STMT (operand);
  def_phi = dyn_cast <gphi *> (def_stmt);
  if (!def_phi)
    return false;

  FOR_EACH_PHI_ARG (arg_p, def_phi, i, SSA_OP_USE)
    if (lhs == USE_FROM_PTR (arg_p))
      return true;
  return false;
}

/* Remove def stmt of VAR if VAR has zero uses and recurse
   on rhs1 operand if so.  */

static void
remove_visited_stmt_chain (tree var)
{
  gimple *stmt;
  gimple_stmt_iterator gsi;

  while (1)
    {
      if (TREE_CODE (var) != SSA_NAME || !has_zero_uses (var))
	return;
      stmt = SSA_NAME_DEF_STMT (var);
      if (is_gimple_assign (stmt) && gimple_visited_p (stmt))
	{
	  var = gimple_assign_rhs1 (stmt);
	  gsi = gsi_for_stmt (stmt);
	  reassoc_remove_stmt (&gsi);
	  release_defs (stmt);
	}
      else
	return;
    }
}

/* This function checks three consequtive operands in
   passed operands vector OPS starting from OPINDEX and
   swaps two operands if it is profitable for binary operation
   consuming OPINDEX + 1 abnd OPINDEX + 2 operands.

   We pair ops with the same rank if possible.

   The alternative we try is to see if STMT is a destructive
   update style statement, which is like:
   b = phi (a, ...)
   a = c + b;
   In that case, we want to use the destructive update form to
   expose the possible vectorizer sum reduction opportunity.
   In that case, the third operand will be the phi node. This
   check is not performed if STMT is null.

   We could, of course, try to be better as noted above, and do a
   lot of work to try to find these opportunities in >3 operand
   cases, but it is unlikely to be worth it.  */

static void
swap_ops_for_binary_stmt (const vec<operand_entry *> &ops,
			  unsigned int opindex, gimple *stmt)
{
  operand_entry *oe1, *oe2, *oe3;

  oe1 = ops[opindex];
  oe2 = ops[opindex + 1];
  oe3 = ops[opindex + 2];

  if ((oe1->rank == oe2->rank
       && oe2->rank != oe3->rank)
      || (stmt && is_phi_for_stmt (stmt, oe3->op)
	  && !is_phi_for_stmt (stmt, oe1->op)
	  && !is_phi_for_stmt (stmt, oe2->op)))
    std::swap (*oe1, *oe3);
  else if ((oe1->rank == oe3->rank
	    && oe2->rank != oe3->rank)
	   || (stmt && is_phi_for_stmt (stmt, oe2->op)
	       && !is_phi_for_stmt (stmt, oe1->op)
	       && !is_phi_for_stmt (stmt, oe3->op)))
    std::swap (*oe1, *oe2);
}

/* If definition of RHS1 or RHS2 dominates STMT, return the later of those
   two definitions, otherwise return STMT.  Sets INSERT_BEFORE to indicate
   whether RHS1 op RHS2 can be inserted before or needs to be inserted
   after the returned stmt.  */

static inline gimple *
find_insert_point (gimple *stmt, tree rhs1, tree rhs2, bool &insert_before)
{
  insert_before = true;
  if (TREE_CODE (rhs1) == SSA_NAME
      && reassoc_stmt_dominates_stmt_p (stmt, SSA_NAME_DEF_STMT (rhs1)))
    {
      stmt = SSA_NAME_DEF_STMT (rhs1);
      insert_before = false;
    }
  if (TREE_CODE (rhs2) == SSA_NAME
      && reassoc_stmt_dominates_stmt_p (stmt, SSA_NAME_DEF_STMT (rhs2)))
    {
      stmt = SSA_NAME_DEF_STMT (rhs2);
      insert_before = false;
    }
  return stmt;
}

/* If the stmt that defines operand has to be inserted, insert it
   before the use.  */
static void
insert_stmt_before_use (gimple *stmt, gimple *stmt_to_insert)
{
  gcc_assert (is_gimple_assign (stmt_to_insert));
  tree rhs1 = gimple_assign_rhs1 (stmt_to_insert);
  tree rhs2 = gimple_assign_rhs2 (stmt_to_insert);
  bool insert_before;
  gimple *insert_point = find_insert_point (stmt, rhs1, rhs2, insert_before);
  gimple_stmt_iterator gsi = gsi_for_stmt (insert_point);
  gimple_set_uid (stmt_to_insert, gimple_uid (insert_point));

  /* If the insert point is not stmt, then insert_point would be
     the point where operand rhs1 or rhs2 is defined. In this case,
     stmt_to_insert has to be inserted afterwards. This would
     only happen when the stmt insertion point is flexible. */
  if (insert_before)
    gsi_insert_before (&gsi, stmt_to_insert, GSI_NEW_STMT);
  else
    insert_stmt_after (stmt_to_insert, insert_point);
}


/* Recursively rewrite our linearized statements so that the operators
   match those in OPS[OPINDEX], putting the computation in rank
   order.  Return new lhs.
   CHANGED is true if we shouldn't reuse the lhs SSA_NAME both in
   the current stmt and during recursive invocations.
   NEXT_CHANGED is true if we shouldn't reuse the lhs SSA_NAME in
   recursive invocations.  */

static tree
rewrite_expr_tree (gimple *stmt, enum tree_code rhs_code, unsigned int opindex,
		   const vec<operand_entry *> &ops, bool changed,
		   bool next_changed)
{
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  operand_entry *oe;

  /* The final recursion case for this function is that you have
     exactly two operations left.
     If we had exactly one op in the entire list to start with, we
     would have never called this function, and the tail recursion
     rewrites them one at a time.  */
  if (opindex + 2 == ops.length ())
    {
      operand_entry *oe1, *oe2;

      oe1 = ops[opindex];
      oe2 = ops[opindex + 1];

      if (rhs1 != oe1->op || rhs2 != oe2->op)
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	  unsigned int uid = gimple_uid (stmt);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Transforming ");
	      print_gimple_stmt (dump_file, stmt, 0);
	    }

	  /* If the stmt that defines operand has to be inserted, insert it
	     before the use.  */
	  if (oe1->stmt_to_insert)
	    insert_stmt_before_use (stmt, oe1->stmt_to_insert);
	  if (oe2->stmt_to_insert)
	    insert_stmt_before_use (stmt, oe2->stmt_to_insert);
	  /* Even when changed is false, reassociation could have e.g. removed
	     some redundant operations, so unless we are just swapping the
	     arguments or unless there is no change at all (then we just
	     return lhs), force creation of a new SSA_NAME.  */
	  if (changed || ((rhs1 != oe2->op || rhs2 != oe1->op) && opindex))
	    {
	      bool insert_before;
	      gimple *insert_point
		= find_insert_point (stmt, oe1->op, oe2->op, insert_before);
	      lhs = make_ssa_name (TREE_TYPE (lhs));
	      stmt
		= gimple_build_assign (lhs, rhs_code,
				       oe1->op, oe2->op);
	      gimple_set_uid (stmt, uid);
	      gimple_set_visited (stmt, true);
	      if (insert_before)
		gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
	      else
		insert_stmt_after (stmt, insert_point);
	    }
	  else
	    {
	      bool insert_before;
	      gcc_checking_assert (find_insert_point (stmt, oe1->op, oe2->op,
						      insert_before)
				   == stmt);
	      gimple_assign_set_rhs1 (stmt, oe1->op);
	      gimple_assign_set_rhs2 (stmt, oe2->op);
	      update_stmt (stmt);
	    }

	  if (rhs1 != oe1->op && rhs1 != oe2->op)
	    remove_visited_stmt_chain (rhs1);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, " into ");
	      print_gimple_stmt (dump_file, stmt, 0);
	    }
	}
      return lhs;
    }

  /* If we hit here, we should have 3 or more ops left.  */
  gcc_assert (opindex + 2 < ops.length ());

  /* Rewrite the next operator.  */
  oe = ops[opindex];

  /* If the stmt that defines operand has to be inserted, insert it
     before the use.  */
  if (oe->stmt_to_insert)
    insert_stmt_before_use (stmt, oe->stmt_to_insert);

  /* Recurse on the LHS of the binary operator, which is guaranteed to
     be the non-leaf side.  */
  tree new_rhs1
    = rewrite_expr_tree (SSA_NAME_DEF_STMT (rhs1), rhs_code, opindex + 1, ops,
			 changed || oe->op != rhs2 || next_changed,
			 false);

  if (oe->op != rhs2 || new_rhs1 != rhs1)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Transforming ");
	  print_gimple_stmt (dump_file, stmt, 0);
	}

      /* If changed is false, this is either opindex == 0
	 or all outer rhs2's were equal to corresponding oe->op,
	 and powi_result is NULL.
	 That means lhs is equivalent before and after reassociation.
	 Otherwise ensure the old lhs SSA_NAME is not reused and
	 create a new stmt as well, so that any debug stmts will be
	 properly adjusted.  */
      if (changed)
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	  unsigned int uid = gimple_uid (stmt);
	  bool insert_before;
	  gimple *insert_point = find_insert_point (stmt, new_rhs1, oe->op,
						    insert_before);

	  lhs = make_ssa_name (TREE_TYPE (lhs));
	  stmt = gimple_build_assign (lhs, rhs_code,
				      new_rhs1, oe->op);
	  gimple_set_uid (stmt, uid);
	  gimple_set_visited (stmt, true);
	  if (insert_before)
	    gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
	  else
	    insert_stmt_after (stmt, insert_point);
	}
      else
	{
	  bool insert_before;
	  gcc_checking_assert (find_insert_point (stmt, new_rhs1, oe->op,
						  insert_before)
			       == stmt);
	  gimple_assign_set_rhs1 (stmt, new_rhs1);
	  gimple_assign_set_rhs2 (stmt, oe->op);
	  update_stmt (stmt);
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, " into ");
	  print_gimple_stmt (dump_file, stmt, 0);
	}
    }
  return lhs;
}

/* Find out how many cycles we need to compute statements chain.
   OPS_NUM holds number os statements in a chain.  CPU_WIDTH is a
   maximum number of independent statements we may execute per cycle.  */

static int
get_required_cycles (int ops_num, int cpu_width)
{
  int res;
  int elog;
  unsigned int rest;

  /* While we have more than 2 * cpu_width operands
     we may reduce number of operands by cpu_width
     per cycle.  */
  res = ops_num / (2 * cpu_width);

  /* Remained operands count may be reduced twice per cycle
     until we have only one operand.  */
  rest = (unsigned)(ops_num - res * cpu_width);
  elog = exact_log2 (rest);
  if (elog >= 0)
    res += elog;
  else
    res += floor_log2 (rest) + 1;

  return res;
}

/* Returns an optimal number of registers to use for computation of
   given statements.  */

static int
get_reassociation_width (int ops_num, enum tree_code opc,
			 machine_mode mode)
{
  int param_width = param_tree_reassoc_width;
  int width;
  int width_min;
  int cycles_best;

  if (param_width > 0)
    width = param_width;
  else
    width = targetm.sched.reassociation_width (opc, mode);

  if (width == 1)
    return width;

  /* Get the minimal time required for sequence computation.  */
  cycles_best = get_required_cycles (ops_num, width);

  /* Check if we may use less width and still compute sequence for
     the same time.  It will allow us to reduce registers usage.
     get_required_cycles is monotonically increasing with lower width
     so we can perform a binary search for the minimal width that still
     results in the optimal cycle count.  */
  width_min = 1;
  while (width > width_min)
    {
      int width_mid = (width + width_min) / 2;

      if (get_required_cycles (ops_num, width_mid) == cycles_best)
	width = width_mid;
      else if (width_min < width_mid)
	width_min = width_mid;
      else
	break;
    }

  return width;
}

/* Recursively rewrite our linearized statements so that the operators
   match those in OPS[OPINDEX], putting the computation in rank
   order and trying to allow operations to be executed in
   parallel.  */

static void
rewrite_expr_tree_parallel (gassign *stmt, int width,
			    const vec<operand_entry *> &ops)
{
  enum tree_code opcode = gimple_assign_rhs_code (stmt);
  int op_num = ops.length ();
  gcc_assert (op_num > 0);
  int stmt_num = op_num - 1;
  gimple **stmts = XALLOCAVEC (gimple *, stmt_num);
  int op_index = op_num - 1;
  int stmt_index = 0;
  int ready_stmts_end = 0;
  int i = 0;
  gimple *stmt1 = NULL, *stmt2 = NULL;
  tree last_rhs1 = gimple_assign_rhs1 (stmt);

  /* We start expression rewriting from the top statements.
     So, in this loop we create a full list of statements
     we will work with.  */
  stmts[stmt_num - 1] = stmt;
  for (i = stmt_num - 2; i >= 0; i--)
    stmts[i] = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmts[i+1]));

  for (i = 0; i < stmt_num; i++)
    {
      tree op1, op2;

      /* Determine whether we should use results of
	 already handled statements or not.  */
      if (ready_stmts_end == 0
	  && (i - stmt_index >= width || op_index < 1))
	ready_stmts_end = i;

      /* Now we choose operands for the next statement.  Non zero
	 value in ready_stmts_end means here that we should use
	 the result of already generated statements as new operand.  */
      if (ready_stmts_end > 0)
	{
	  op1 = gimple_assign_lhs (stmts[stmt_index++]);
	  if (ready_stmts_end > stmt_index)
	    op2 = gimple_assign_lhs (stmts[stmt_index++]);
	  else if (op_index >= 0)
	    {
	      operand_entry *oe = ops[op_index--];
	      stmt2 = oe->stmt_to_insert;
	      op2 = oe->op;
	    }
	  else
	    {
	      gcc_assert (stmt_index < i);
	      op2 = gimple_assign_lhs (stmts[stmt_index++]);
	    }

	  if (stmt_index >= ready_stmts_end)
	    ready_stmts_end = 0;
	}
      else
	{
	  if (op_index > 1)
	    swap_ops_for_binary_stmt (ops, op_index - 2, NULL);
	  operand_entry *oe2 = ops[op_index--];
	  operand_entry *oe1 = ops[op_index--];
	  op2 = oe2->op;
	  stmt2 = oe2->stmt_to_insert;
	  op1 = oe1->op;
	  stmt1 = oe1->stmt_to_insert;
	}

      /* If we emit the last statement then we should put
	 operands into the last statement.  It will also
	 break the loop.  */
      if (op_index < 0 && stmt_index == i)
	i = stmt_num - 1;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Transforming ");
	  print_gimple_stmt (dump_file, stmts[i], 0);
	}

      /* If the stmt that defines operand has to be inserted, insert it
	 before the use.  */
      if (stmt1)
	insert_stmt_before_use (stmts[i], stmt1);
      if (stmt2)
	insert_stmt_before_use (stmts[i], stmt2);
      stmt1 = stmt2 = NULL;

      /* We keep original statement only for the last one.  All
	 others are recreated.  */
      if (i == stmt_num - 1)
	{
	  gimple_assign_set_rhs1 (stmts[i], op1);
	  gimple_assign_set_rhs2 (stmts[i], op2);
	  update_stmt (stmts[i]);
	}
      else
	{
	  stmts[i] = build_and_add_sum (TREE_TYPE (last_rhs1), op1, op2, opcode);
	  gimple_set_visited (stmts[i], true);
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, " into ");
	  print_gimple_stmt (dump_file, stmts[i], 0);
	}
    }

  remove_visited_stmt_chain (last_rhs1);
}

/* Transform STMT, which is really (A +B) + (C + D) into the left
   linear form, ((A+B)+C)+D.
   Recurse on D if necessary.  */

static void
linearize_expr (gimple *stmt)
{
  gimple_stmt_iterator gsi;
  gimple *binlhs = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));
  gimple *binrhs = SSA_NAME_DEF_STMT (gimple_assign_rhs2 (stmt));
  gimple *oldbinrhs = binrhs;
  enum tree_code rhscode = gimple_assign_rhs_code (stmt);
  gimple *newbinrhs = NULL;
  class loop *loop = loop_containing_stmt (stmt);
  tree lhs = gimple_assign_lhs (stmt);

  gcc_assert (is_reassociable_op (binlhs, rhscode, loop)
	      && is_reassociable_op (binrhs, rhscode, loop));

  gsi = gsi_for_stmt (stmt);

  gimple_assign_set_rhs2 (stmt, gimple_assign_rhs1 (binrhs));
  binrhs = gimple_build_assign (make_ssa_name (TREE_TYPE (lhs)),
				gimple_assign_rhs_code (binrhs),
				gimple_assign_lhs (binlhs),
				gimple_assign_rhs2 (binrhs));
  gimple_assign_set_rhs1 (stmt, gimple_assign_lhs (binrhs));
  gsi_insert_before (&gsi, binrhs, GSI_SAME_STMT);
  gimple_set_uid (binrhs, gimple_uid (stmt));

  if (TREE_CODE (gimple_assign_rhs2 (stmt)) == SSA_NAME)
    newbinrhs = SSA_NAME_DEF_STMT (gimple_assign_rhs2 (stmt));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Linearized: ");
      print_gimple_stmt (dump_file, stmt, 0);
    }

  reassociate_stats.linearized++;
  update_stmt (stmt);

  gsi = gsi_for_stmt (oldbinrhs);
  reassoc_remove_stmt (&gsi);
  release_defs (oldbinrhs);

  gimple_set_visited (stmt, true);
  gimple_set_visited (binlhs, true);
  gimple_set_visited (binrhs, true);

  /* Tail recurse on the new rhs if it still needs reassociation.  */
  if (newbinrhs && is_reassociable_op (newbinrhs, rhscode, loop))
    /* ??? This should probably be linearize_expr (newbinrhs) but I don't
	   want to change the algorithm while converting to tuples.  */
    linearize_expr (stmt);
}

/* If LHS has a single immediate use that is a GIMPLE_ASSIGN statement, return
   it.  Otherwise, return NULL.  */

static gimple *
get_single_immediate_use (tree lhs)
{
  use_operand_p immuse;
  gimple *immusestmt;

  if (TREE_CODE (lhs) == SSA_NAME
      && single_imm_use (lhs, &immuse, &immusestmt)
      && is_gimple_assign (immusestmt))
    return immusestmt;

  return NULL;
}

/* Recursively negate the value of TONEGATE, and return the SSA_NAME
   representing the negated value.  Insertions of any necessary
   instructions go before GSI.
   This function is recursive in that, if you hand it "a_5" as the
   value to negate, and a_5 is defined by "a_5 = b_3 + b_4", it will
   transform b_3 + b_4 into a_5 = -b_3 + -b_4.  */

static tree
negate_value (tree tonegate, gimple_stmt_iterator *gsip)
{
  gimple *negatedefstmt = NULL;
  tree resultofnegate;
  gimple_stmt_iterator gsi;
  unsigned int uid;

  /* If we are trying to negate a name, defined by an add, negate the
     add operands instead.  */
  if (TREE_CODE (tonegate) == SSA_NAME)
    negatedefstmt = SSA_NAME_DEF_STMT (tonegate);
  if (TREE_CODE (tonegate) == SSA_NAME
      && is_gimple_assign (negatedefstmt)
      && TREE_CODE (gimple_assign_lhs (negatedefstmt)) == SSA_NAME
      && has_single_use (gimple_assign_lhs (negatedefstmt))
      && gimple_assign_rhs_code (negatedefstmt) == PLUS_EXPR)
    {
      tree rhs1 = gimple_assign_rhs1 (negatedefstmt);
      tree rhs2 = gimple_assign_rhs2 (negatedefstmt);
      tree lhs = gimple_assign_lhs (negatedefstmt);
      gimple *g;

      gsi = gsi_for_stmt (negatedefstmt);
      rhs1 = negate_value (rhs1, &gsi);

      gsi = gsi_for_stmt (negatedefstmt);
      rhs2 = negate_value (rhs2, &gsi);

      gsi = gsi_for_stmt (negatedefstmt);
      lhs = make_ssa_name (TREE_TYPE (lhs));
      gimple_set_visited (negatedefstmt, true);
      g = gimple_build_assign (lhs, PLUS_EXPR, rhs1, rhs2);
      gimple_set_uid (g, gimple_uid (negatedefstmt));
      gsi_insert_before (&gsi, g, GSI_SAME_STMT);
      return lhs;
    }

  tonegate = fold_build1 (NEGATE_EXPR, TREE_TYPE (tonegate), tonegate);
  resultofnegate = force_gimple_operand_gsi (gsip, tonegate, true,
					     NULL_TREE, true, GSI_SAME_STMT);
  gsi = *gsip;
  uid = gimple_uid (gsi_stmt (gsi));
  for (gsi_prev (&gsi); !gsi_end_p (gsi); gsi_prev (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (gimple_uid (stmt) != 0)
	break;
      gimple_set_uid (stmt, uid);
    }
  return resultofnegate;
}

/* Return true if we should break up the subtract in STMT into an add
   with negate.  This is true when we the subtract operands are really
   adds, or the subtract itself is used in an add expression.  In
   either case, breaking up the subtract into an add with negate
   exposes the adds to reassociation.  */

static bool
should_break_up_subtract (gimple *stmt)
{
  tree lhs = gimple_assign_lhs (stmt);
  tree binlhs = gimple_assign_rhs1 (stmt);
  tree binrhs = gimple_assign_rhs2 (stmt);
  gimple *immusestmt;
  class loop *loop = loop_containing_stmt (stmt);

  if (TREE_CODE (binlhs) == SSA_NAME
      && is_reassociable_op (SSA_NAME_DEF_STMT (binlhs), PLUS_EXPR, loop))
    return true;

  if (TREE_CODE (binrhs) == SSA_NAME
      && is_reassociable_op (SSA_NAME_DEF_STMT (binrhs), PLUS_EXPR, loop))
    return true;

  if (TREE_CODE (lhs) == SSA_NAME
      && (immusestmt = get_single_immediate_use (lhs))
      && is_gimple_assign (immusestmt)
      && (gimple_assign_rhs_code (immusestmt) == PLUS_EXPR
	  || (gimple_assign_rhs_code (immusestmt) == MINUS_EXPR
	      && gimple_assign_rhs1 (immusestmt) == lhs)
	  || gimple_assign_rhs_code (immusestmt) == MULT_EXPR))
    return true;
  return false;
}

/* Transform STMT from A - B into A + -B.  */

static void
break_up_subtract (gimple *stmt, gimple_stmt_iterator *gsip)
{
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Breaking up subtract ");
      print_gimple_stmt (dump_file, stmt, 0);
    }

  rhs2 = negate_value (rhs2, gsip);
  gimple_assign_set_rhs_with_ops (gsip, PLUS_EXPR, rhs1, rhs2);
  update_stmt (stmt);
}

/* Determine whether STMT is a builtin call that raises an SSA name
   to an integer power and has only one use.  If so, and this is early
   reassociation and unsafe math optimizations are permitted, place
   the SSA name in *BASE and the exponent in *EXPONENT, and return TRUE.
   If any of these conditions does not hold, return FALSE.  */

static bool
acceptable_pow_call (gcall *stmt, tree *base, HOST_WIDE_INT *exponent)
{
  tree arg1;
  REAL_VALUE_TYPE c, cint;

  switch (gimple_call_combined_fn (stmt))
    {
    CASE_CFN_POW:
      if (flag_errno_math)
	return false;

      *base = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);

      if (TREE_CODE (arg1) != REAL_CST)
	return false;

      c = TREE_REAL_CST (arg1);

      if (REAL_EXP (&c) > HOST_BITS_PER_WIDE_INT)
	return false;

      *exponent = real_to_integer (&c);
      real_from_integer (&cint, VOIDmode, *exponent, SIGNED);
      if (!real_identical (&c, &cint))
	return false;

      break;

    CASE_CFN_POWI:
      *base = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);

      if (!tree_fits_shwi_p (arg1))
	return false;

      *exponent = tree_to_shwi (arg1);
      break;

    default:
      return false;
    }

  /* Expanding negative exponents is generally unproductive, so we don't
     complicate matters with those.  Exponents of zero and one should
     have been handled by expression folding.  */
  if (*exponent < 2 || TREE_CODE (*base) != SSA_NAME)
    return false;

  return true;
}

/* Try to derive and add operand entry for OP to *OPS.  Return false if
   unsuccessful.  */

static bool
try_special_add_to_ops (vec<operand_entry *> *ops,
			enum tree_code code,
			tree op, gimple* def_stmt)
{
  tree base = NULL_TREE;
  HOST_WIDE_INT exponent = 0;

  if (TREE_CODE (op) != SSA_NAME
      || ! has_single_use (op))
    return false;

  if (code == MULT_EXPR
      && reassoc_insert_powi_p
      && flag_unsafe_math_optimizations
      && is_gimple_call (def_stmt)
      && acceptable_pow_call (as_a <gcall *> (def_stmt), &base, &exponent))
    {
      add_repeat_to_ops_vec (ops, base, exponent);
      gimple_set_visited (def_stmt, true);
      return true;
    }
  else if (code == MULT_EXPR
	   && is_gimple_assign (def_stmt)
	   && gimple_assign_rhs_code (def_stmt) == NEGATE_EXPR
	   && !HONOR_SNANS (TREE_TYPE (op))
	   && (!HONOR_SIGNED_ZEROS (TREE_TYPE (op))
	       || !COMPLEX_FLOAT_TYPE_P (TREE_TYPE (op)))
	   && (!FLOAT_TYPE_P (TREE_TYPE (op))
	       || !DECIMAL_FLOAT_MODE_P (element_mode (op))))
    {
      tree rhs1 = gimple_assign_rhs1 (def_stmt);
      tree cst = build_minus_one_cst (TREE_TYPE (op));
      add_to_ops_vec (ops, rhs1);
      add_to_ops_vec (ops, cst);
      gimple_set_visited (def_stmt, true);
      return true;
    }

  return false;
}

/* Recursively linearize a binary expression that is the RHS of STMT.
   Place the operands of the expression tree in the vector named OPS.  */

static void
linearize_expr_tree (vec<operand_entry *> *ops, gimple *stmt,
		     bool is_associative, bool set_visited)
{
  tree binlhs = gimple_assign_rhs1 (stmt);
  tree binrhs = gimple_assign_rhs2 (stmt);
  gimple *binlhsdef = NULL, *binrhsdef = NULL;
  bool binlhsisreassoc = false;
  bool binrhsisreassoc = false;
  enum tree_code rhscode = gimple_assign_rhs_code (stmt);
  class loop *loop = loop_containing_stmt (stmt);

  if (set_visited)
    gimple_set_visited (stmt, true);

  if (TREE_CODE (binlhs) == SSA_NAME)
    {
      binlhsdef = SSA_NAME_DEF_STMT (binlhs);
      binlhsisreassoc = (is_reassociable_op (binlhsdef, rhscode, loop)
			 && !stmt_could_throw_p (cfun, binlhsdef));
    }

  if (TREE_CODE (binrhs) == SSA_NAME)
    {
      binrhsdef = SSA_NAME_DEF_STMT (binrhs);
      binrhsisreassoc = (is_reassociable_op (binrhsdef, rhscode, loop)
			 && !stmt_could_throw_p (cfun, binrhsdef));
    }

  /* If the LHS is not reassociable, but the RHS is, we need to swap
     them.  If neither is reassociable, there is nothing we can do, so
     just put them in the ops vector.  If the LHS is reassociable,
     linearize it.  If both are reassociable, then linearize the RHS
     and the LHS.  */

  if (!binlhsisreassoc)
    {
      /* If this is not a associative operation like division, give up.  */
      if (!is_associative)
	{
	  add_to_ops_vec (ops, binrhs);
	  return;
	}

      if (!binrhsisreassoc)
	{
	  bool swap = false;
	  if (try_special_add_to_ops (ops, rhscode, binrhs, binrhsdef))
	    /* If we add ops for the rhs we expect to be able to recurse
	       to it via the lhs during expression rewrite so swap
	       operands.  */
	    swap = true;
	  else
	    add_to_ops_vec (ops, binrhs);

	  if (!try_special_add_to_ops (ops, rhscode, binlhs, binlhsdef))
	    add_to_ops_vec (ops, binlhs);

	  if (!swap)
	    return;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "swapping operands of ");
	  print_gimple_stmt (dump_file, stmt, 0);
	}

      swap_ssa_operands (stmt,
			 gimple_assign_rhs1_ptr (stmt),
			 gimple_assign_rhs2_ptr (stmt));
      update_stmt (stmt);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, " is now ");
	  print_gimple_stmt (dump_file, stmt, 0);
	}
      if (!binrhsisreassoc)
	return;

      /* We want to make it so the lhs is always the reassociative op,
	 so swap.  */
      std::swap (binlhs, binrhs);
    }
  else if (binrhsisreassoc)
    {
      linearize_expr (stmt);
      binlhs = gimple_assign_rhs1 (stmt);
      binrhs = gimple_assign_rhs2 (stmt);
    }

  gcc_assert (TREE_CODE (binrhs) != SSA_NAME
	      || !is_reassociable_op (SSA_NAME_DEF_STMT (binrhs),
				      rhscode, loop));
  linearize_expr_tree (ops, SSA_NAME_DEF_STMT (binlhs),
		       is_associative, set_visited);

  if (!try_special_add_to_ops (ops, rhscode, binrhs, binrhsdef))
    add_to_ops_vec (ops, binrhs);
}

/* Repropagate the negates back into subtracts, since no other pass
   currently does it.  */

static void
repropagate_negates (void)
{
  unsigned int i = 0;
  tree negate;

  FOR_EACH_VEC_ELT (plus_negates, i, negate)
    {
      gimple *user = get_single_immediate_use (negate);
      if (!user || !is_gimple_assign (user))
	continue;

      tree negateop = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (negate));
      if (TREE_CODE (negateop) == SSA_NAME
	  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (negateop))
	continue;

      /* The negate operand can be either operand of a PLUS_EXPR
	 (it can be the LHS if the RHS is a constant for example).

	 Force the negate operand to the RHS of the PLUS_EXPR, then
	 transform the PLUS_EXPR into a MINUS_EXPR.  */
      if (gimple_assign_rhs_code (user) == PLUS_EXPR)
	{
	  /* If the negated operand appears on the LHS of the
	     PLUS_EXPR, exchange the operands of the PLUS_EXPR
	     to force the negated operand to the RHS of the PLUS_EXPR.  */
	  if (gimple_assign_rhs1 (user) == negate)
	    {
	      swap_ssa_operands (user,
				 gimple_assign_rhs1_ptr (user),
				 gimple_assign_rhs2_ptr (user));
	    }

	  /* Now transform the PLUS_EXPR into a MINUS_EXPR and replace
	     the RHS of the PLUS_EXPR with the operand of the NEGATE_EXPR.  */
	  if (gimple_assign_rhs2 (user) == negate)
	    {
	      tree rhs1 = gimple_assign_rhs1 (user);
	      gimple_stmt_iterator gsi = gsi_for_stmt (user);
	      gimple_assign_set_rhs_with_ops (&gsi, MINUS_EXPR, rhs1,
					      negateop);
	      update_stmt (user);
	    }
	}
      else if (gimple_assign_rhs_code (user) == MINUS_EXPR)
	{
	  if (gimple_assign_rhs1 (user) == negate)
	    {
	      /* We have
		   x = -negateop
		   y = x - b
		 which we transform into
		   x = negateop + b
		   y = -x .
		 This pushes down the negate which we possibly can merge
		 into some other operation, hence insert it into the
		 plus_negates vector.  */
	      gimple *feed = SSA_NAME_DEF_STMT (negate);
	      tree b = gimple_assign_rhs2 (user);
	      gimple_stmt_iterator gsi = gsi_for_stmt (feed);
	      gimple_stmt_iterator gsi2 = gsi_for_stmt (user);
	      tree x = make_ssa_name (TREE_TYPE (gimple_assign_lhs (feed)));
	      gimple *g = gimple_build_assign (x, PLUS_EXPR, negateop, b);
	      gsi_insert_before (&gsi2, g, GSI_SAME_STMT);
	      gimple_assign_set_rhs_with_ops (&gsi2, NEGATE_EXPR, x);
	      user = gsi_stmt (gsi2);
	      update_stmt (user);
	      reassoc_remove_stmt (&gsi);
	      release_defs (feed);
	      plus_negates.safe_push (gimple_assign_lhs (user));
	    }
	  else
	    {
	      /* Transform "x = -negateop; y = b - x" into "y = b + negateop",
		 getting rid of one operation.  */
	      tree rhs1 = gimple_assign_rhs1 (user);
	      gimple_stmt_iterator gsi = gsi_for_stmt (user);
	      gimple_assign_set_rhs_with_ops (&gsi, PLUS_EXPR, rhs1, negateop);
	      update_stmt (gsi_stmt (gsi));
	    }
	}
    }
}

/* Break up subtract operations in block BB.

   We do this top down because we don't know whether the subtract is
   part of a possible chain of reassociation except at the top.

   IE given
   d = f + g
   c = a + e
   b = c - d
   q = b - r
   k = t - q

   we want to break up k = t - q, but we won't until we've transformed q
   = b - r, which won't be broken up until we transform b = c - d.

   En passant, clear the GIMPLE visited flag on every statement
   and set UIDs within each basic block.  */

static void
break_up_subtract_bb (basic_block bb)
{
  gimple_stmt_iterator gsi;
  basic_block son;
  unsigned int uid = 1;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      gimple_set_visited (stmt, false);
      gimple_set_uid (stmt, uid++);

      if (!is_gimple_assign (stmt)
	  || !can_reassociate_type_p (TREE_TYPE (gimple_assign_lhs (stmt)))
	  || !can_reassociate_op_p (gimple_assign_lhs (stmt)))
	continue;

      /* Look for simple gimple subtract operations.  */
      if (gimple_assign_rhs_code (stmt) == MINUS_EXPR)
	{
	  if (!can_reassociate_op_p (gimple_assign_rhs1 (stmt))
	      || !can_reassociate_op_p (gimple_assign_rhs2 (stmt)))
	    continue;

	  /* Check for a subtract used only in an addition.  If this
	     is the case, transform it into add of a negate for better
	     reassociation.  IE transform C = A-B into C = A + -B if C
	     is only used in an addition.  */
	  if (should_break_up_subtract (stmt))
	    break_up_subtract (stmt, &gsi);
	}
      else if (gimple_assign_rhs_code (stmt) == NEGATE_EXPR
	       && can_reassociate_op_p (gimple_assign_rhs1 (stmt)))
	plus_negates.safe_push (gimple_assign_lhs (stmt));
    }
  for (son = first_dom_son (CDI_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    break_up_subtract_bb (son);
}

/* Used for repeated factor analysis.  */
struct repeat_factor
{
  /* An SSA name that occurs in a multiply chain.  */
  tree factor;

  /* Cached rank of the factor.  */
  unsigned rank;

  /* Number of occurrences of the factor in the chain.  */
  HOST_WIDE_INT count;

  /* An SSA name representing the product of this factor and
     all factors appearing later in the repeated factor vector.  */
  tree repr;
};


static vec<repeat_factor> repeat_factor_vec;

/* Used for sorting the repeat factor vector.  Sort primarily by
   ascending occurrence count, secondarily by descending rank.  */

static int
compare_repeat_factors (const void *x1, const void *x2)
{
  const repeat_factor *rf1 = (const repeat_factor *) x1;
  const repeat_factor *rf2 = (const repeat_factor *) x2;

  if (rf1->count != rf2->count)
    return rf1->count - rf2->count;

  return rf2->rank - rf1->rank;
}

/* Look for repeated operands in OPS in the multiply tree rooted at
   STMT.  Replace them with an optimal sequence of multiplies and powi
   builtin calls, and remove the used operands from OPS.  Return an
   SSA name representing the value of the replacement sequence.  */

static tree
attempt_builtin_powi (gimple *stmt, vec<operand_entry *> *ops)
{
  unsigned i, j, vec_len;
  int ii;
  operand_entry *oe;
  repeat_factor *rf1, *rf2;
  repeat_factor rfnew;
  tree result = NULL_TREE;
  tree target_ssa, iter_result;
  tree type = TREE_TYPE (gimple_get_lhs (stmt));
  tree powi_fndecl = mathfn_built_in (type, BUILT_IN_POWI);
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  gimple *mul_stmt, *pow_stmt;

  /* Nothing to do if BUILT_IN_POWI doesn't exist for this type and
     target, unless type is integral.  */
  if (!powi_fndecl && !INTEGRAL_TYPE_P (type))
    return NULL_TREE;

  /* Allocate the repeated factor vector.  */
  repeat_factor_vec.create (10);

  /* Scan the OPS vector for all SSA names in the product and build
     up a vector of occurrence counts for each factor.  */
  FOR_EACH_VEC_ELT (*ops, i, oe)
    {
      if (TREE_CODE (oe->op) == SSA_NAME)
	{
	  FOR_EACH_VEC_ELT (repeat_factor_vec, j, rf1)
	    {
	      if (rf1->factor == oe->op)
		{
		  rf1->count += oe->count;
		  break;
		}
	    }

	  if (j >= repeat_factor_vec.length ())
	    {
	      rfnew.factor = oe->op;
	      rfnew.rank = oe->rank;
	      rfnew.count = oe->count;
	      rfnew.repr = NULL_TREE;
	      repeat_factor_vec.safe_push (rfnew);
	    }
	}
    }

  /* Sort the repeated factor vector by (a) increasing occurrence count,
     and (b) decreasing rank.  */
  repeat_factor_vec.qsort (compare_repeat_factors);

  /* It is generally best to combine as many base factors as possible
     into a product before applying __builtin_powi to the result.
     However, the sort order chosen for the repeated factor vector
     allows us to cache partial results for the product of the base
     factors for subsequent use.  When we already have a cached partial
     result from a previous iteration, it is best to make use of it
     before looking for another __builtin_pow opportunity.

     As an example, consider x * x * y * y * y * z * z * z * z.
     We want to first compose the product x * y * z, raise it to the
     second power, then multiply this by y * z, and finally multiply
     by z.  This can be done in 5 multiplies provided we cache y * z
     for use in both expressions:

        t1 = y * z
	t2 = t1 * x
	t3 = t2 * t2
	t4 = t1 * t3
	result = t4 * z

     If we instead ignored the cached y * z and first multiplied by
     the __builtin_pow opportunity z * z, we would get the inferior:

        t1 = y * z
	t2 = t1 * x
	t3 = t2 * t2
	t4 = z * z
	t5 = t3 * t4
        result = t5 * y  */

  vec_len = repeat_factor_vec.length ();
  
  /* Repeatedly look for opportunities to create a builtin_powi call.  */
  while (true)
    {
      HOST_WIDE_INT power;

      /* First look for the largest cached product of factors from
	 preceding iterations.  If found, create a builtin_powi for
	 it if the minimum occurrence count for its factors is at
	 least 2, or just use this cached product as our next 
	 multiplicand if the minimum occurrence count is 1.  */
      FOR_EACH_VEC_ELT (repeat_factor_vec, j, rf1)
	{
	  if (rf1->repr && rf1->count > 0)
	    break;
	}

      if (j < vec_len)
	{
	  power = rf1->count;

	  if (power == 1)
	    {
	      iter_result = rf1->repr;

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  unsigned elt;
		  repeat_factor *rf;
		  fputs ("Multiplying by cached product ", dump_file);
		  for (elt = j; elt < vec_len; elt++)
		    {
		      rf = &repeat_factor_vec[elt];
		      print_generic_expr (dump_file, rf->factor);
		      if (elt < vec_len - 1)
			fputs (" * ", dump_file);
		    }
		  fputs ("\n", dump_file);
		}
	    }
	  else
	    {
	      if (INTEGRAL_TYPE_P (type))
		{
		  gcc_assert (power > 1);
		  gimple_stmt_iterator gsip = gsi;
		  gsi_prev (&gsip);
		  iter_result = powi_as_mults (&gsi, gimple_location (stmt),
					       rf1->repr, power);
		  gimple_stmt_iterator gsic = gsi;
		  while (gsi_stmt (gsic) != gsi_stmt (gsip))
		    {
		      gimple_set_uid (gsi_stmt (gsic), gimple_uid (stmt));
		      gimple_set_visited (gsi_stmt (gsic), true);
		      gsi_prev (&gsic);
		    }
		}
	      else
		{
		  iter_result = make_temp_ssa_name (type, NULL, "reassocpow");
		  pow_stmt
		    = gimple_build_call (powi_fndecl, 2, rf1->repr,
					 build_int_cst (integer_type_node,
							power));
		  gimple_call_set_lhs (pow_stmt, iter_result);
		  gimple_set_location (pow_stmt, gimple_location (stmt));
		  gimple_set_uid (pow_stmt, gimple_uid (stmt));
		  gsi_insert_before (&gsi, pow_stmt, GSI_SAME_STMT);
		}

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  unsigned elt;
		  repeat_factor *rf;
		  fputs ("Building __builtin_pow call for cached product (",
			 dump_file);
		  for (elt = j; elt < vec_len; elt++)
		    {
		      rf = &repeat_factor_vec[elt];
		      print_generic_expr (dump_file, rf->factor);
		      if (elt < vec_len - 1)
			fputs (" * ", dump_file);
		    }
		  fprintf (dump_file, ")^" HOST_WIDE_INT_PRINT_DEC"\n",
			   power);
		}
	    }
	}
      else
	{
	  /* Otherwise, find the first factor in the repeated factor
	     vector whose occurrence count is at least 2.  If no such
	     factor exists, there are no builtin_powi opportunities
	     remaining.  */
	  FOR_EACH_VEC_ELT (repeat_factor_vec, j, rf1)
	    {
	      if (rf1->count >= 2)
		break;
	    }

	  if (j >= vec_len)
	    break;

	  power = rf1->count;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      unsigned elt;
	      repeat_factor *rf;
	      fputs ("Building __builtin_pow call for (", dump_file);
	      for (elt = j; elt < vec_len; elt++)
		{
		  rf = &repeat_factor_vec[elt];
		  print_generic_expr (dump_file, rf->factor);
		  if (elt < vec_len - 1)
		    fputs (" * ", dump_file);
		}
	      fprintf (dump_file, ")^" HOST_WIDE_INT_PRINT_DEC"\n", power);
	    }

	  reassociate_stats.pows_created++;

	  /* Visit each element of the vector in reverse order (so that
	     high-occurrence elements are visited first, and within the
	     same occurrence count, lower-ranked elements are visited
	     first).  Form a linear product of all elements in this order
	     whose occurrencce count is at least that of element J.
	     Record the SSA name representing the product of each element
	     with all subsequent elements in the vector.  */
	  if (j == vec_len - 1)
	    rf1->repr = rf1->factor;
	  else
	    {
	      for (ii = vec_len - 2; ii >= (int)j; ii--)
		{
		  tree op1, op2;

		  rf1 = &repeat_factor_vec[ii];
		  rf2 = &repeat_factor_vec[ii + 1];

		  /* Init the last factor's representative to be itself.  */
		  if (!rf2->repr)
		    rf2->repr = rf2->factor;

		  op1 = rf1->factor;
		  op2 = rf2->repr;

		  target_ssa = make_temp_ssa_name (type, NULL, "reassocpow");
		  mul_stmt = gimple_build_assign (target_ssa, MULT_EXPR,
						  op1, op2);
		  gimple_set_location (mul_stmt, gimple_location (stmt));
		  gimple_set_uid (mul_stmt, gimple_uid (stmt));
		  gsi_insert_before (&gsi, mul_stmt, GSI_SAME_STMT);
		  rf1->repr = target_ssa;

		  /* Don't reprocess the multiply we just introduced.  */
		  gimple_set_visited (mul_stmt, true);
		}
	    }

	  /* Form a call to __builtin_powi for the maximum product
	     just formed, raised to the power obtained earlier.  */
	  rf1 = &repeat_factor_vec[j];
	  if (INTEGRAL_TYPE_P (type))
	    {
	      gcc_assert (power > 1);
	      gimple_stmt_iterator gsip = gsi;
	      gsi_prev (&gsip);
	      iter_result = powi_as_mults (&gsi, gimple_location (stmt),
					   rf1->repr, power);
	      gimple_stmt_iterator gsic = gsi;
	      while (gsi_stmt (gsic) != gsi_stmt (gsip))
		{
		  gimple_set_uid (gsi_stmt (gsic), gimple_uid (stmt));
		  gimple_set_visited (gsi_stmt (gsic), true);
		  gsi_prev (&gsic);
		}
	    }
	  else
	    {
	      iter_result = make_temp_ssa_name (type, NULL, "reassocpow");
	      pow_stmt = gimple_build_call (powi_fndecl, 2, rf1->repr,
					    build_int_cst (integer_type_node,
							   power));
	      gimple_call_set_lhs (pow_stmt, iter_result);
	      gimple_set_location (pow_stmt, gimple_location (stmt));
	      gimple_set_uid (pow_stmt, gimple_uid (stmt));
	      gsi_insert_before (&gsi, pow_stmt, GSI_SAME_STMT);
	    }
	}

      /* If we previously formed at least one other builtin_powi call,
	 form the product of this one and those others.  */
      if (result)
	{
	  tree new_result = make_temp_ssa_name (type, NULL, "reassocpow");
	  mul_stmt = gimple_build_assign (new_result, MULT_EXPR,
					  result, iter_result);
	  gimple_set_location (mul_stmt, gimple_location (stmt));
	  gimple_set_uid (mul_stmt, gimple_uid (stmt));
	  gsi_insert_before (&gsi, mul_stmt, GSI_SAME_STMT);
	  gimple_set_visited (mul_stmt, true);
	  result = new_result;
	}
      else
	result = iter_result;

      /* Decrement the occurrence count of each element in the product
	 by the count found above, and remove this many copies of each
	 factor from OPS.  */
      for (i = j; i < vec_len; i++)
	{
	  unsigned k = power;
	  unsigned n;

	  rf1 = &repeat_factor_vec[i];
	  rf1->count -= power;
	  
	  FOR_EACH_VEC_ELT_REVERSE (*ops, n, oe)
	    {
	      if (oe->op == rf1->factor)
		{
		  if (oe->count <= k)
		    {
		      ops->ordered_remove (n);
		      k -= oe->count;

		      if (k == 0)
			break;
		    }
		  else
		    {
		      oe->count -= k;
		      break;
		    }
		}
	    }
	}
    }

  /* At this point all elements in the repeated factor vector have a
     remaining occurrence count of 0 or 1, and those with a count of 1
     don't have cached representatives.  Re-sort the ops vector and
     clean up.  */
  ops->qsort (sort_by_operand_rank);
  repeat_factor_vec.release ();

  /* Return the final product computed herein.  Note that there may
     still be some elements with single occurrence count left in OPS;
     those will be handled by the normal reassociation logic.  */
  return result;
}

/* Attempt to optimize
   CST1 * copysign (CST2, y) -> copysign (CST1 * CST2, y) if CST1 > 0, or
   CST1 * copysign (CST2, y) -> -copysign (CST1 * CST2, y) if CST1 < 0.  */

static void
attempt_builtin_copysign (vec<operand_entry *> *ops)
{
  operand_entry *oe;
  unsigned int i;
  unsigned int length = ops->length ();
  tree cst = ops->last ()->op;

  if (length == 1 || TREE_CODE (cst) != REAL_CST)
    return;

  FOR_EACH_VEC_ELT (*ops, i, oe)
    {
      if (TREE_CODE (oe->op) == SSA_NAME
	  && has_single_use (oe->op))
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (oe->op);
	  if (gcall *old_call = dyn_cast <gcall *> (def_stmt))
	    {
	      tree arg0, arg1;
	      switch (gimple_call_combined_fn (old_call))
		{
		CASE_CFN_COPYSIGN:
		CASE_CFN_COPYSIGN_FN:
		  arg0 = gimple_call_arg (old_call, 0);
		  arg1 = gimple_call_arg (old_call, 1);
		  /* The first argument of copysign must be a constant,
		     otherwise there's nothing to do.  */
		  if (TREE_CODE (arg0) == REAL_CST)
		    {
		      tree type = TREE_TYPE (arg0);
		      tree mul = const_binop (MULT_EXPR, type, cst, arg0);
		      /* If we couldn't fold to a single constant, skip it.
			 That happens e.g. for inexact multiplication when
			 -frounding-math.  */
		      if (mul == NULL_TREE)
			break;
		      /* Instead of adjusting OLD_CALL, let's build a new
			 call to not leak the LHS and prevent keeping bogus
			 debug statements.  DCE will clean up the old call.  */
		      gcall *new_call;
		      if (gimple_call_internal_p (old_call))
			new_call = gimple_build_call_internal
			  (IFN_COPYSIGN, 2, mul, arg1);
		      else
			new_call = gimple_build_call
			  (gimple_call_fndecl (old_call), 2, mul, arg1);
		      tree lhs = make_ssa_name (type);
		      gimple_call_set_lhs (new_call, lhs);
		      gimple_set_location (new_call,
					   gimple_location (old_call));
		      insert_stmt_after (new_call, old_call);
		      /* We've used the constant, get rid of it.  */
		      ops->pop ();
		      bool cst1_neg = real_isneg (TREE_REAL_CST_PTR (cst));
		      /* Handle the CST1 < 0 case by negating the result.  */
		      if (cst1_neg)
			{
			  tree negrhs = make_ssa_name (TREE_TYPE (lhs));
			  gimple *negate_stmt
			    = gimple_build_assign (negrhs, NEGATE_EXPR, lhs);
			  insert_stmt_after (negate_stmt, new_call);
			  oe->op = negrhs;
			}
		      else
			oe->op = lhs;
		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  fprintf (dump_file, "Optimizing copysign: ");
			  print_generic_expr (dump_file, cst);
			  fprintf (dump_file, " * COPYSIGN (");
			  print_generic_expr (dump_file, arg0);
			  fprintf (dump_file, ", ");
			  print_generic_expr (dump_file, arg1);
			  fprintf (dump_file, ") into %sCOPYSIGN (",
				   cst1_neg ? "-" : "");
			  print_generic_expr (dump_file, mul);
			  fprintf (dump_file, ", ");
			  print_generic_expr (dump_file, arg1);
			  fprintf (dump_file, "\n");
			}
		      return;
		    }
		  break;
		default:
		  break;
		}
	    }
	}
    }
}

/* Transform STMT at *GSI into a copy by replacing its rhs with NEW_RHS.  */

static void
transform_stmt_to_copy (gimple_stmt_iterator *gsi, gimple *stmt, tree new_rhs)
{
  tree rhs1;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Transforming ");
      print_gimple_stmt (dump_file, stmt, 0);
    }

  rhs1 = gimple_assign_rhs1 (stmt);
  gimple_assign_set_rhs_from_tree (gsi, new_rhs);
  update_stmt (stmt);
  remove_visited_stmt_chain (rhs1);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, " into ");
      print_gimple_stmt (dump_file, stmt, 0);
    }
}

/* Transform STMT at *GSI into a multiply of RHS1 and RHS2.  */

static void
transform_stmt_to_multiply (gimple_stmt_iterator *gsi, gimple *stmt,
			    tree rhs1, tree rhs2)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Transforming ");
      print_gimple_stmt (dump_file, stmt, 0);
    }

  gimple_assign_set_rhs_with_ops (gsi, MULT_EXPR, rhs1, rhs2);
  update_stmt (gsi_stmt (*gsi));
  remove_visited_stmt_chain (rhs1);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, " into ");
      print_gimple_stmt (dump_file, stmt, 0);
    }
}

/* Reassociate expressions in basic block BB and its post-dominator as
   children.

   Bubble up return status from maybe_optimize_range_tests.  */

static bool
reassociate_bb (basic_block bb)
{
  gimple_stmt_iterator gsi;
  basic_block son;
  gimple *stmt = last_stmt (bb);
  bool cfg_cleanup_needed = false;

  if (stmt && !gimple_visited_p (stmt))
    cfg_cleanup_needed |= maybe_optimize_range_tests (stmt);

  bool do_prev = false;
  for (gsi = gsi_last_bb (bb);
       !gsi_end_p (gsi); do_prev ? gsi_prev (&gsi) : (void) 0)
    {
      do_prev = true;
      stmt = gsi_stmt (gsi);

      if (is_gimple_assign (stmt)
	  && !stmt_could_throw_p (cfun, stmt))
	{
	  tree lhs, rhs1, rhs2;
	  enum tree_code rhs_code = gimple_assign_rhs_code (stmt);

	  /* If this was part of an already processed statement,
	     we don't need to touch it again. */
	  if (gimple_visited_p (stmt))
	    {
	      /* This statement might have become dead because of previous
		 reassociations.  */
	      if (has_zero_uses (gimple_get_lhs (stmt)))
		{
		  reassoc_remove_stmt (&gsi);
		  release_defs (stmt);
		  /* We might end up removing the last stmt above which
		     places the iterator to the end of the sequence.
		     Reset it to the last stmt in this case and make sure
		     we don't do gsi_prev in that case.  */
		  if (gsi_end_p (gsi))
		    {
		      gsi = gsi_last_bb (bb);
		      do_prev = false;
		    }
		}
	      continue;
	    }

	  /* If this is not a gimple binary expression, there is
	     nothing for us to do with it.  */
	  if (get_gimple_rhs_class (rhs_code) != GIMPLE_BINARY_RHS)
	    continue;

	  lhs = gimple_assign_lhs (stmt);
	  rhs1 = gimple_assign_rhs1 (stmt);
	  rhs2 = gimple_assign_rhs2 (stmt);

	  /* For non-bit or min/max operations we can't associate
	     all types.  Verify that here.  */
	  if ((rhs_code != BIT_IOR_EXPR
	       && rhs_code != BIT_AND_EXPR
	       && rhs_code != BIT_XOR_EXPR
	       && rhs_code != MIN_EXPR
	       && rhs_code != MAX_EXPR
	       && !can_reassociate_type_p (TREE_TYPE (lhs)))
	      || !can_reassociate_op_p (rhs1)
	      || !can_reassociate_op_p (rhs2))
	    continue;

	  if (associative_tree_code (rhs_code))
	    {
	      auto_vec<operand_entry *> ops;
	      tree powi_result = NULL_TREE;
	      bool is_vector = VECTOR_TYPE_P (TREE_TYPE (lhs));

	      /* There may be no immediate uses left by the time we
		 get here because we may have eliminated them all.  */
	      if (TREE_CODE (lhs) == SSA_NAME && has_zero_uses (lhs))
		continue;

	      gimple_set_visited (stmt, true);
	      linearize_expr_tree (&ops, stmt, true, true);
	      ops.qsort (sort_by_operand_rank);
	      int orig_len = ops.length ();
	      optimize_ops_list (rhs_code, &ops);
	      if (undistribute_ops_list (rhs_code, &ops,
					 loop_containing_stmt (stmt)))
		{
		  ops.qsort (sort_by_operand_rank);
		  optimize_ops_list (rhs_code, &ops);
		}
	      if (undistribute_bitref_for_vector (rhs_code, &ops,
						  loop_containing_stmt (stmt)))
		{
		  ops.qsort (sort_by_operand_rank);
		  optimize_ops_list (rhs_code, &ops);
		}
	      if (rhs_code == PLUS_EXPR
		  && transform_add_to_multiply (&ops))
		ops.qsort (sort_by_operand_rank);

	      if (rhs_code == BIT_IOR_EXPR || rhs_code == BIT_AND_EXPR)
		{
		  if (is_vector)
		    optimize_vec_cond_expr (rhs_code, &ops);
		  else
		    optimize_range_tests (rhs_code, &ops, NULL);
	        }

	      if (rhs_code == MULT_EXPR && !is_vector)
	        {
		  attempt_builtin_copysign (&ops);

		  if (reassoc_insert_powi_p
		      && (flag_unsafe_math_optimizations
			  || (INTEGRAL_TYPE_P (TREE_TYPE (lhs)))))
		    powi_result = attempt_builtin_powi (stmt, &ops);
		}

	      operand_entry *last;
	      bool negate_result = false;
	      if (ops.length () > 1
		  && rhs_code == MULT_EXPR)
		{
		  last = ops.last ();
		  if ((integer_minus_onep (last->op)
		       || real_minus_onep (last->op))
		      && !HONOR_SNANS (TREE_TYPE (lhs))
		      && (!HONOR_SIGNED_ZEROS (TREE_TYPE (lhs))
			  || !COMPLEX_FLOAT_TYPE_P (TREE_TYPE (lhs))))
		    {
		      ops.pop ();
		      negate_result = true;
		    }
		}

	      tree new_lhs = lhs;
	      /* If the operand vector is now empty, all operands were 
		 consumed by the __builtin_powi optimization.  */
	      if (ops.length () == 0)
		transform_stmt_to_copy (&gsi, stmt, powi_result);
	      else if (ops.length () == 1)
		{
		  tree last_op = ops.last ()->op;

		  /* If the stmt that defines operand has to be inserted, insert it
		     before the use.  */
		  if (ops.last ()->stmt_to_insert)
		    insert_stmt_before_use (stmt, ops.last ()->stmt_to_insert);
		  if (powi_result)
		    transform_stmt_to_multiply (&gsi, stmt, last_op,
						powi_result);
		  else
		    transform_stmt_to_copy (&gsi, stmt, last_op);
		}
	      else
		{
		  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));
		  int ops_num = ops.length ();
		  int width;

		  /* For binary bit operations, if there are at least 3
		     operands and the last operand in OPS is a constant,
		     move it to the front.  This helps ensure that we generate
		     (X & Y) & C rather than (X & C) & Y.  The former will
		     often match a canonical bit test when we get to RTL.  */
		  if (ops.length () > 2
		      && (rhs_code == BIT_AND_EXPR
		          || rhs_code == BIT_IOR_EXPR
		          || rhs_code == BIT_XOR_EXPR)
		      && TREE_CODE (ops.last ()->op) == INTEGER_CST)
		    std::swap (*ops[0], *ops[ops_num - 1]);

		  /* Only rewrite the expression tree to parallel in the
		     last reassoc pass to avoid useless work back-and-forth
		     with initial linearization.  */
		  if (!reassoc_insert_powi_p
		      && ops.length () > 3
		      && (width = get_reassociation_width (ops_num, rhs_code,
							   mode)) > 1)
		    {
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file,
				 "Width = %d was chosen for reassociation\n",
				 width);
		      rewrite_expr_tree_parallel (as_a <gassign *> (stmt),
						  width, ops);
		    }
		  else
                    {
                      /* When there are three operands left, we want
                         to make sure the ones that get the double
                         binary op are chosen wisely.  */
                      int len = ops.length ();
                      if (len >= 3)
                        swap_ops_for_binary_stmt (ops, len - 3, stmt);

		      new_lhs = rewrite_expr_tree (stmt, rhs_code, 0, ops,
						   powi_result != NULL
						   || negate_result,
						   len != orig_len);
                    }

		  /* If we combined some repeated factors into a 
		     __builtin_powi call, multiply that result by the
		     reassociated operands.  */
		  if (powi_result)
		    {
		      gimple *mul_stmt, *lhs_stmt = SSA_NAME_DEF_STMT (lhs);
		      tree type = TREE_TYPE (lhs);
		      tree target_ssa = make_temp_ssa_name (type, NULL,
							    "reassocpow");
		      gimple_set_lhs (lhs_stmt, target_ssa);
		      update_stmt (lhs_stmt);
		      if (lhs != new_lhs)
			{
			  target_ssa = new_lhs;
			  new_lhs = lhs;
			}
		      mul_stmt = gimple_build_assign (lhs, MULT_EXPR,
						      powi_result, target_ssa);
		      gimple_set_location (mul_stmt, gimple_location (stmt));
		      gimple_set_uid (mul_stmt, gimple_uid (stmt));
		      gsi_insert_after (&gsi, mul_stmt, GSI_NEW_STMT);
		    }
		}

	      if (negate_result)
		{
		  stmt = SSA_NAME_DEF_STMT (lhs);
		  tree tmp = make_ssa_name (TREE_TYPE (lhs));
		  gimple_set_lhs (stmt, tmp);
		  if (lhs != new_lhs)
		    tmp = new_lhs;
		  gassign *neg_stmt = gimple_build_assign (lhs, NEGATE_EXPR,
							   tmp);
		  gimple_set_uid (neg_stmt, gimple_uid (stmt));
		  gsi_insert_after (&gsi, neg_stmt, GSI_NEW_STMT);
		  update_stmt (stmt);
		}
	    }
	}
    }
  for (son = first_dom_son (CDI_POST_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_POST_DOMINATORS, son))
    cfg_cleanup_needed |= reassociate_bb (son);

  return cfg_cleanup_needed;
}

/* Add jumps around shifts for range tests turned into bit tests.
   For each SSA_NAME VAR we have code like:
   VAR = ...; // final stmt of range comparison
   // bit test here...;
   OTHERVAR = ...; // final stmt of the bit test sequence
   RES = VAR | OTHERVAR;
   Turn the above into:
   VAR = ...;
   if (VAR != 0)
     goto <l3>;
   else
     goto <l2>;
   <l2>:
   // bit test here...;
   OTHERVAR = ...;
   <l3>:
   # RES = PHI<1(l1), OTHERVAR(l2)>;  */

static void
branch_fixup (void)
{
  tree var;
  unsigned int i;

  FOR_EACH_VEC_ELT (reassoc_branch_fixups, i, var)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (var);
      gimple *use_stmt;
      use_operand_p use;
      bool ok = single_imm_use (var, &use, &use_stmt);
      gcc_assert (ok
		  && is_gimple_assign (use_stmt)
		  && gimple_assign_rhs_code (use_stmt) == BIT_IOR_EXPR
		  && gimple_bb (def_stmt) == gimple_bb (use_stmt));

      basic_block cond_bb = gimple_bb (def_stmt);
      basic_block then_bb = split_block (cond_bb, def_stmt)->dest;
      basic_block merge_bb = split_block (then_bb, use_stmt)->dest;

      gimple_stmt_iterator gsi = gsi_for_stmt (def_stmt);
      gimple *g = gimple_build_cond (NE_EXPR, var,
				     build_zero_cst (TREE_TYPE (var)),
				     NULL_TREE, NULL_TREE);
      location_t loc = gimple_location (use_stmt);
      gimple_set_location (g, loc);
      gsi_insert_after (&gsi, g, GSI_NEW_STMT);

      edge etrue = make_edge (cond_bb, merge_bb, EDGE_TRUE_VALUE);
      etrue->probability = profile_probability::even ();
      edge efalse = find_edge (cond_bb, then_bb);
      efalse->flags = EDGE_FALSE_VALUE;
      efalse->probability -= etrue->probability;
      then_bb->count -= etrue->count ();

      tree othervar = NULL_TREE;
      if (gimple_assign_rhs1 (use_stmt) == var)
	othervar = gimple_assign_rhs2 (use_stmt);
      else if (gimple_assign_rhs2 (use_stmt) == var)
	othervar = gimple_assign_rhs1 (use_stmt);
      else
	gcc_unreachable ();
      tree lhs = gimple_assign_lhs (use_stmt);
      gphi *phi = create_phi_node (lhs, merge_bb);
      add_phi_arg (phi, build_one_cst (TREE_TYPE (lhs)), etrue, loc);
      add_phi_arg (phi, othervar, single_succ_edge (then_bb), loc);
      gsi = gsi_for_stmt (use_stmt);
      gsi_remove (&gsi, true);

      set_immediate_dominator (CDI_DOMINATORS, merge_bb, cond_bb);
      set_immediate_dominator (CDI_POST_DOMINATORS, cond_bb, merge_bb);
    }
  reassoc_branch_fixups.release ();
}

void dump_ops_vector (FILE *file, vec<operand_entry *> ops);
void debug_ops_vector (vec<operand_entry *> ops);

/* Dump the operand entry vector OPS to FILE.  */

void
dump_ops_vector (FILE *file, vec<operand_entry *> ops)
{
  operand_entry *oe;
  unsigned int i;

  FOR_EACH_VEC_ELT (ops, i, oe)
    {
      fprintf (file, "Op %d -> rank: %d, tree: ", i, oe->rank);
      print_generic_expr (file, oe->op);
      fprintf (file, "\n");
    }
}

/* Dump the operand entry vector OPS to STDERR.  */

DEBUG_FUNCTION void
debug_ops_vector (vec<operand_entry *> ops)
{
  dump_ops_vector (stderr, ops);
}

/* Bubble up return status from reassociate_bb.  */

static bool
do_reassoc (void)
{
  break_up_subtract_bb (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  return reassociate_bb (EXIT_BLOCK_PTR_FOR_FN (cfun));
}

/* Initialize the reassociation pass.  */

static void
init_reassoc (void)
{
  int i;
  int64_t rank = 2;
  int *bbs = XNEWVEC (int, n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS);

  /* Find the loops, so that we can prevent moving calculations in
     them.  */
  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);

  memset (&reassociate_stats, 0, sizeof (reassociate_stats));

  next_operand_entry_id = 0;

  /* Reverse RPO (Reverse Post Order) will give us something where
     deeper loops come later.  */
  pre_and_rev_post_order_compute (NULL, bbs, false);
  bb_rank = XCNEWVEC (int64_t, last_basic_block_for_fn (cfun));
  operand_rank = new hash_map<tree, int64_t>;

  /* Give each default definition a distinct rank.  This includes
     parameters and the static chain.  Walk backwards over all
     SSA names so that we get proper rank ordering according
     to tree_swap_operands_p.  */
  for (i = num_ssa_names - 1; i > 0; --i)
    {
      tree name = ssa_name (i);
      if (name && SSA_NAME_IS_DEFAULT_DEF (name))
	insert_operand_rank (name, ++rank);
    }

  /* Set up rank for each BB  */
  for (i = 0; i < n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS; i++)
    bb_rank[bbs[i]] = ++rank << 16;

  free (bbs);
  calculate_dominance_info (CDI_POST_DOMINATORS);
  plus_negates = vNULL;
}

/* Cleanup after the reassociation pass, and print stats if
   requested.  */

static void
fini_reassoc (void)
{
  statistics_counter_event (cfun, "Linearized",
			    reassociate_stats.linearized);
  statistics_counter_event (cfun, "Constants eliminated",
			    reassociate_stats.constants_eliminated);
  statistics_counter_event (cfun, "Ops eliminated",
			    reassociate_stats.ops_eliminated);
  statistics_counter_event (cfun, "Statements rewritten",
			    reassociate_stats.rewritten);
  statistics_counter_event (cfun, "Built-in pow[i] calls encountered",
			    reassociate_stats.pows_encountered);
  statistics_counter_event (cfun, "Built-in powi calls created",
			    reassociate_stats.pows_created);

  delete operand_rank;
  bitmap_clear (biased_names);
  operand_entry_pool.release ();
  free (bb_rank);
  plus_negates.release ();
  free_dominance_info (CDI_POST_DOMINATORS);
  loop_optimizer_finalize ();
}

/* Gate and execute functions for Reassociation.  If INSERT_POWI_P, enable
   insertion of __builtin_powi calls.

   Returns TODO_cfg_cleanup if a CFG cleanup pass is desired due to
   optimization of a gimple conditional.  Otherwise returns zero.  */

static unsigned int
execute_reassoc (bool insert_powi_p, bool bias_loop_carried_phi_ranks_p)
{
  reassoc_insert_powi_p = insert_powi_p;
  reassoc_bias_loop_carried_phi_ranks_p = bias_loop_carried_phi_ranks_p;

  init_reassoc ();

  bool cfg_cleanup_needed;
  cfg_cleanup_needed = do_reassoc ();
  repropagate_negates ();
  branch_fixup ();

  fini_reassoc ();
  return cfg_cleanup_needed ? TODO_cleanup_cfg : 0;
}

namespace {

const pass_data pass_data_reassoc =
{
  GIMPLE_PASS, /* type */
  "reassoc", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_REASSOC, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa_only_virtuals, /* todo_flags_finish */
};

class pass_reassoc : public gimple_opt_pass
{
public:
  pass_reassoc (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_reassoc, ctxt), insert_powi_p (false)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override { return new pass_reassoc (m_ctxt); }
  void set_pass_param (unsigned int n, bool param) final override
    {
      gcc_assert (n == 0);
      insert_powi_p = param;
      bias_loop_carried_phi_ranks_p = !param;
    }
  bool gate (function *) final override { return flag_tree_reassoc != 0; }
  unsigned int execute (function *) final override
  {
    return execute_reassoc (insert_powi_p, bias_loop_carried_phi_ranks_p);
  }

 private:
  /* Enable insertion of __builtin_powi calls during execute_reassoc.  See
     point 3a in the pass header comment.  */
  bool insert_powi_p;
  bool bias_loop_carried_phi_ranks_p;
}; // class pass_reassoc

} // anon namespace

gimple_opt_pass *
make_pass_reassoc (gcc::context *ctxt)
{
  return new pass_reassoc (ctxt);
}
