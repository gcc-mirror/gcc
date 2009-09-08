/* Reassociation for trees.
   Copyright (C) 2005, 2007, 2008, 2009 Free Software Foundation, Inc.
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
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-inline.h"
#include "tree-flow.h"
#include "gimple.h"
#include "tree-dump.h"
#include "timevar.h"
#include "tree-iterator.h"
#include "tree-pass.h"
#include "alloc-pool.h"
#include "vec.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "cfgloop.h"
#include "flags.h"

/*  This is a simple global reassociation pass.  It is, in part, based
    on the LLVM pass of the same name (They do some things more/less
    than we do, in different orders, etc).

    It consists of five steps:

    1. Breaking up subtract operations into addition + negate, where
    it would promote the reassociation of adds.

    2. Left linearization of the expression trees, so that (A+B)+(C+D)
    becomes (((A+B)+C)+D), which is easier for us to rewrite later.
    During linearization, we place the operands of the binary
    expressions into a vector of operand_entry_t

    3. Optimization of the operand lists, eliminating things like a +
    -a, a & a, etc.

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


/* Statistics */
static struct
{
  int linearized;
  int constants_eliminated;
  int ops_eliminated;
  int rewritten;
} reassociate_stats;

/* Operator, rank pair.  */
typedef struct operand_entry
{
  unsigned int rank;
  tree op;
} *operand_entry_t;

static alloc_pool operand_entry_pool;


/* Starting rank number for a given basic block, so that we can rank
   operations using unmovable instructions in that BB based on the bb
   depth.  */
static long *bb_rank;

/* Operand->rank hashtable.  */
static struct pointer_map_t *operand_rank;


/* Look up the operand rank structure for expression E.  */

static inline long
find_operand_rank (tree e)
{
  void **slot = pointer_map_contains (operand_rank, e);
  return slot ? (long) (intptr_t) *slot : -1;
}

/* Insert {E,RANK} into the operand rank hashtable.  */

static inline void
insert_operand_rank (tree e, long rank)
{
  void **slot;
  gcc_assert (rank > 0);
  slot = pointer_map_insert (operand_rank, e);
  gcc_assert (!*slot);
  *slot = (void *) (intptr_t) rank;
}

/* Given an expression E, return the rank of the expression.  */

static long
get_rank (tree e)
{
  /* Constants have rank 0.  */
  if (is_gimple_min_invariant (e))
    return 0;

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

  if (TREE_CODE (e) == SSA_NAME)
    {
      gimple stmt;
      long rank, maxrank;
      int i, n;

      if (TREE_CODE (SSA_NAME_VAR (e)) == PARM_DECL
	  && SSA_NAME_IS_DEFAULT_DEF (e))
	return find_operand_rank (e);

      stmt = SSA_NAME_DEF_STMT (e);
      if (gimple_bb (stmt) == NULL)
	return 0;

      if (!is_gimple_assign (stmt)
	  || gimple_vdef (stmt))
	return bb_rank[gimple_bb (stmt)->index];

      /* If we already have a rank for this expression, use that.  */
      rank = find_operand_rank (e);
      if (rank != -1)
	return rank;

      /* Otherwise, find the maximum rank for the operands, or the bb
	 rank, whichever is less.   */
      rank = 0;
      maxrank = bb_rank[gimple_bb(stmt)->index];
      if (gimple_assign_single_p (stmt))
	{
	  tree rhs = gimple_assign_rhs1 (stmt);
	  n = TREE_OPERAND_LENGTH (rhs);
	  if (n == 0)
	    rank = MAX (rank, get_rank (rhs));
	  else
	    {
	      for (i = 0;
		   i < n && TREE_OPERAND (rhs, i) && rank != maxrank; i++)
		rank = MAX(rank, get_rank (TREE_OPERAND (rhs, i)));
	    }
	}
      else
	{
	  n = gimple_num_ops (stmt);
	  for (i = 1; i < n && rank != maxrank; i++)
	    {
	      gcc_assert (gimple_op (stmt, i));
	      rank = MAX(rank, get_rank (gimple_op (stmt, i)));
	    }
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Rank for ");
	  print_generic_expr (dump_file, e, 0);
	  fprintf (dump_file, " is %ld\n", (rank + 1));
	}

      /* Note the rank in the hashtable so we don't recompute it.  */
      insert_operand_rank (e, (rank + 1));
      return (rank + 1);
    }

  /* Globals, etc,  are rank 0 */
  return 0;
}

DEF_VEC_P(operand_entry_t);
DEF_VEC_ALLOC_P(operand_entry_t, heap);

/* We want integer ones to end up last no matter what, since they are
   the ones we can do the most with.  */
#define INTEGER_CONST_TYPE 1 << 3
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
    return FLOAT_CONST_TYPE;
  else
    return OTHER_CONST_TYPE;
}

/* qsort comparison function to sort operand entries PA and PB by rank
   so that the sorted array is ordered by rank in decreasing order.  */
static int
sort_by_operand_rank (const void *pa, const void *pb)
{
  const operand_entry_t oea = *(const operand_entry_t *)pa;
  const operand_entry_t oeb = *(const operand_entry_t *)pb;

  /* It's nicer for optimize_expression if constants that are likely
     to fold when added/multiplied//whatever are put next to each
     other.  Since all constants have rank 0, order them by type.  */
  if (oeb->rank == 0 &&  oea->rank == 0)
    return constant_type (oeb->op) - constant_type (oea->op);

  /* Lastly, make sure the versions that are the same go next to each
     other.  We use SSA_NAME_VERSION because it's stable.  */
  if ((oeb->rank - oea->rank == 0)
      && TREE_CODE (oea->op) == SSA_NAME
      && TREE_CODE (oeb->op) == SSA_NAME)
    return SSA_NAME_VERSION (oeb->op) - SSA_NAME_VERSION (oea->op);

  return oeb->rank - oea->rank;
}

/* Add an operand entry to *OPS for the tree operand OP.  */

static void
add_to_ops_vec (VEC(operand_entry_t, heap) **ops, tree op)
{
  operand_entry_t oe = (operand_entry_t) pool_alloc (operand_entry_pool);

  oe->op = op;
  oe->rank = get_rank (op);
  VEC_safe_push (operand_entry_t, heap, *ops, oe);
}

/* Return true if STMT is reassociable operation containing a binary
   operation with tree code CODE, and is inside LOOP.  */

static bool
is_reassociable_op (gimple stmt, enum tree_code code, struct loop *loop)
{
  basic_block bb = gimple_bb (stmt);

  if (gimple_bb (stmt) == NULL)
    return false;

  if (!flow_bb_inside_loop_p (loop, bb))
    return false;

  if (is_gimple_assign (stmt)
      && gimple_assign_rhs_code (stmt) == code
      && has_single_use (gimple_assign_lhs (stmt)))
    return true;

  return false;
}


/* Given NAME, if NAME is defined by a unary operation OPCODE, return the
   operand of the negate operation.  Otherwise, return NULL.  */

static tree
get_unary_op (tree name, enum tree_code opcode)
{
  gimple stmt = SSA_NAME_DEF_STMT (name);

  if (!is_gimple_assign (stmt))
    return NULL_TREE;

  if (gimple_assign_rhs_code (stmt) == opcode)
    return gimple_assign_rhs1 (stmt);
  return NULL_TREE;
}

/* If CURR and LAST are a pair of ops that OPCODE allows us to
   eliminate through equivalences, do so, remove them from OPS, and
   return true.  Otherwise, return false.  */

static bool
eliminate_duplicate_pair (enum tree_code opcode,
			  VEC (operand_entry_t, heap) **ops,
			  bool *all_done,
			  unsigned int i,
			  operand_entry_t curr,
			  operand_entry_t last)
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
	      print_generic_expr (dump_file, curr->op, 0);
	      fprintf (dump_file, " [&|minmax] ");
	      print_generic_expr (dump_file, last->op, 0);
	      fprintf (dump_file, " -> ");
	      print_generic_stmt (dump_file, last->op, 0);
	    }

	  VEC_ordered_remove (operand_entry_t, *ops, i);
	  reassociate_stats.ops_eliminated ++;

	  return true;

	case BIT_XOR_EXPR:
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Equivalence: ");
	      print_generic_expr (dump_file, curr->op, 0);
	      fprintf (dump_file, " ^ ");
	      print_generic_expr (dump_file, last->op, 0);
	      fprintf (dump_file, " -> nothing\n");
	    }

	  reassociate_stats.ops_eliminated += 2;

	  if (VEC_length (operand_entry_t, *ops) == 2)
	    {
	      VEC_free (operand_entry_t, heap, *ops);
	      *ops = NULL;
	      add_to_ops_vec (ops, fold_convert (TREE_TYPE (last->op), 
						 integer_zero_node));
	      *all_done = true;
	    }
	  else
	    {
	      VEC_ordered_remove (operand_entry_t, *ops, i-1);
	      VEC_ordered_remove (operand_entry_t, *ops, i-1);
	    }

	  return true;

	default:
	  break;
	}
    }
  return false;
}

/* If OPCODE is PLUS_EXPR, CURR->OP is really a negate expression,
   look in OPS for a corresponding positive operation to cancel it
   out.  If we find one, remove the other from OPS, replace
   OPS[CURRINDEX] with 0, and return true.  Otherwise, return
   false. */

static bool
eliminate_plus_minus_pair (enum tree_code opcode,
			   VEC (operand_entry_t, heap) **ops,
			   unsigned int currindex,
			   operand_entry_t curr)
{
  tree negateop;
  unsigned int i;
  operand_entry_t oe;

  if (opcode != PLUS_EXPR || TREE_CODE (curr->op) != SSA_NAME)
    return false;

  negateop = get_unary_op (curr->op, NEGATE_EXPR);
  if (negateop == NULL_TREE)
    return false;

  /* Any non-negated version will have a rank that is one less than
     the current rank.  So once we hit those ranks, if we don't find
     one, we can stop.  */

  for (i = currindex + 1;
       VEC_iterate (operand_entry_t, *ops, i, oe)
       && oe->rank >= curr->rank - 1 ;
       i++)
    {
      if (oe->op == negateop)
	{

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Equivalence: ");
	      print_generic_expr (dump_file, negateop, 0);
	      fprintf (dump_file, " + -");
	      print_generic_expr (dump_file, oe->op, 0);
	      fprintf (dump_file, " -> 0\n");
	    }

	  VEC_ordered_remove (operand_entry_t, *ops, i);
	  add_to_ops_vec (ops, fold_convert(TREE_TYPE (oe->op), 
					    integer_zero_node));
	  VEC_ordered_remove (operand_entry_t, *ops, currindex);
	  reassociate_stats.ops_eliminated ++;

	  return true;
	}
    }

  return false;
}

/* If OPCODE is BIT_IOR_EXPR, BIT_AND_EXPR, and, CURR->OP is really a
   bitwise not expression, look in OPS for a corresponding operand to
   cancel it out.  If we find one, remove the other from OPS, replace
   OPS[CURRINDEX] with 0, and return true.  Otherwise, return
   false. */

static bool
eliminate_not_pairs (enum tree_code opcode,
		     VEC (operand_entry_t, heap) **ops,
		     unsigned int currindex,
		     operand_entry_t curr)
{
  tree notop;
  unsigned int i;
  operand_entry_t oe;

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
       VEC_iterate (operand_entry_t, *ops, i, oe)
       && oe->rank >= curr->rank - 1;
       i++)
    {
      if (oe->op == notop)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Equivalence: ");
	      print_generic_expr (dump_file, notop, 0);
	      if (opcode == BIT_AND_EXPR)
		fprintf (dump_file, " & ~");
	      else if (opcode == BIT_IOR_EXPR)
		fprintf (dump_file, " | ~");
	      print_generic_expr (dump_file, oe->op, 0);
	      if (opcode == BIT_AND_EXPR)
		fprintf (dump_file, " -> 0\n");
	      else if (opcode == BIT_IOR_EXPR)
		fprintf (dump_file, " -> -1\n");
	    }

	  if (opcode == BIT_AND_EXPR)
	    oe->op = fold_convert (TREE_TYPE (oe->op), integer_zero_node);
	  else if (opcode == BIT_IOR_EXPR)
	    oe->op = build_low_bits_mask (TREE_TYPE (oe->op),
					  TYPE_PRECISION (TREE_TYPE (oe->op)));

	  reassociate_stats.ops_eliminated 
	    += VEC_length (operand_entry_t, *ops) - 1;
	  VEC_free (operand_entry_t, heap, *ops);
	  *ops = NULL;
	  VEC_safe_push (operand_entry_t, heap, *ops, oe);
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
			   VEC(operand_entry_t, heap) **ops)
{
  operand_entry_t oelast = VEC_last (operand_entry_t, *ops);
  tree type = TREE_TYPE (oelast->op);

  if (oelast->rank == 0
      && (INTEGRAL_TYPE_P (type) || FLOAT_TYPE_P (type)))
    {
      switch (opcode)
	{
	case BIT_AND_EXPR:
	  if (integer_zerop (oelast->op))
	    {
	      if (VEC_length (operand_entry_t, *ops) != 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Found & 0, removing all other ops\n");

		  reassociate_stats.ops_eliminated 
		    += VEC_length (operand_entry_t, *ops) - 1;
		  
		  VEC_free (operand_entry_t, heap, *ops);
		  *ops = NULL;
		  VEC_safe_push (operand_entry_t, heap, *ops, oelast);
		  return;
		}
	    }
	  else if (integer_all_onesp (oelast->op))
	    {
	      if (VEC_length (operand_entry_t, *ops) != 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Found & -1, removing\n");
		  VEC_pop (operand_entry_t, *ops);
		  reassociate_stats.ops_eliminated++;
		}
	    }
	  break;
	case BIT_IOR_EXPR:
	  if (integer_all_onesp (oelast->op))
	    {
	      if (VEC_length (operand_entry_t, *ops) != 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Found | -1, removing all other ops\n");

		  reassociate_stats.ops_eliminated 
		    += VEC_length (operand_entry_t, *ops) - 1;
		  
		  VEC_free (operand_entry_t, heap, *ops);
		  *ops = NULL;
		  VEC_safe_push (operand_entry_t, heap, *ops, oelast);
		  return;
		}
	    }	  
	  else if (integer_zerop (oelast->op))
	    {
	      if (VEC_length (operand_entry_t, *ops) != 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Found | 0, removing\n");
		  VEC_pop (operand_entry_t, *ops);
		  reassociate_stats.ops_eliminated++;
		}
	    }
	  break;
	case MULT_EXPR:
	  if (integer_zerop (oelast->op)
	      || (FLOAT_TYPE_P (type)
		  && !HONOR_NANS (TYPE_MODE (type))
		  && !HONOR_SIGNED_ZEROS (TYPE_MODE (type))
		  && real_zerop (oelast->op)))
	    {
	      if (VEC_length (operand_entry_t, *ops) != 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Found * 0, removing all other ops\n");
		  
		  reassociate_stats.ops_eliminated 
		    += VEC_length (operand_entry_t, *ops) - 1;
		  VEC_free (operand_entry_t, heap, *ops);
		  *ops = NULL;
		  VEC_safe_push (operand_entry_t, heap, *ops, oelast);
		  return;
		}
	    }
	  else if (integer_onep (oelast->op)
		   || (FLOAT_TYPE_P (type)
		       && !HONOR_SNANS (TYPE_MODE (type))
		       && real_onep (oelast->op)))
	    {
	      if (VEC_length (operand_entry_t, *ops) != 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Found * 1, removing\n");
		  VEC_pop (operand_entry_t, *ops);
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
		  && fold_real_zero_addition_p (type, oelast->op,
						opcode == MINUS_EXPR)))
	    {
	      if (VEC_length (operand_entry_t, *ops) != 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Found [|^+] 0, removing\n");
		  VEC_pop (operand_entry_t, *ops);
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


static void linearize_expr_tree (VEC(operand_entry_t, heap) **, gimple,
				 bool, bool);

/* Structure for tracking and counting operands.  */
typedef struct oecount_s {
  int cnt;
  enum tree_code oecode;
  tree op;
} oecount;

DEF_VEC_O(oecount);
DEF_VEC_ALLOC_O(oecount,heap);

/* The heap for the oecount hashtable and the sorted list of operands.  */
static VEC (oecount, heap) *cvec;

/* Hash function for oecount.  */

static hashval_t
oecount_hash (const void *p)
{
  const oecount *c = VEC_index (oecount, cvec, (size_t)p - 42);
  return htab_hash_pointer (c->op) ^ (hashval_t)c->oecode;
}

/* Comparison function for oecount.  */

static int
oecount_eq (const void *p1, const void *p2)
{
  const oecount *c1 = VEC_index (oecount, cvec, (size_t)p1 - 42);
  const oecount *c2 = VEC_index (oecount, cvec, (size_t)p2 - 42);
  return (c1->oecode == c2->oecode
	  && c1->op == c2->op);
}

/* Comparison function for qsort sorting oecount elements by count.  */

static int
oecount_cmp (const void *p1, const void *p2)
{
  const oecount *c1 = (const oecount *)p1;
  const oecount *c2 = (const oecount *)p2;
  return c1->cnt - c2->cnt;
}

/* Walks the linear chain with result *DEF searching for an operation
   with operand OP and code OPCODE removing that from the chain.  *DEF
   is updated if there is only one operand but no operation left.  */

static void
zero_one_operation (tree *def, enum tree_code opcode, tree op)
{
  gimple stmt = SSA_NAME_DEF_STMT (*def);

  do
    {
      tree name = gimple_assign_rhs1 (stmt);

      /* If this is the operation we look for and one of the operands
         is ours simply propagate the other operand into the stmts
	 single use.  */
      if (gimple_assign_rhs_code (stmt) == opcode
	  && (name == op
	      || gimple_assign_rhs2 (stmt) == op))
	{
	  gimple use_stmt;
	  use_operand_p use;
	  gimple_stmt_iterator gsi;
	  if (name == op)
	    name = gimple_assign_rhs2 (stmt);
	  gcc_assert (has_single_use (gimple_assign_lhs (stmt)));
	  single_imm_use (gimple_assign_lhs (stmt), &use, &use_stmt);
	  if (gimple_assign_lhs (stmt) == *def)
	    *def = name;
	  SET_USE (use, name);
	  if (TREE_CODE (name) != SSA_NAME)
	    update_stmt (use_stmt);
	  gsi = gsi_for_stmt (stmt);
	  gsi_remove (&gsi, true);
	  release_defs (stmt);
	  return;
	}

      /* Continue walking the chain.  */
      gcc_assert (name != op
		  && TREE_CODE (name) == SSA_NAME);
      stmt = SSA_NAME_DEF_STMT (name);
    }
  while (1);
}

/* Builds one statement performing OP1 OPCODE OP2 using TMPVAR for
   the result.  Places the statement after the definition of either
   OP1 or OP2.  Returns the new statement.  */

static gimple
build_and_add_sum (tree tmpvar, tree op1, tree op2, enum tree_code opcode)
{
  gimple op1def = NULL, op2def = NULL;
  gimple_stmt_iterator gsi;
  tree op;
  gimple sum;

  /* Create the addition statement.  */
  sum = gimple_build_assign_with_ops (opcode, tmpvar, op1, op2);
  op = make_ssa_name (tmpvar, sum);
  gimple_assign_set_lhs (sum, op);

  /* Find an insertion place and insert.  */
  if (TREE_CODE (op1) == SSA_NAME)
    op1def = SSA_NAME_DEF_STMT (op1);
  if (TREE_CODE (op2) == SSA_NAME)
    op2def = SSA_NAME_DEF_STMT (op2);
  if ((!op1def || gimple_nop_p (op1def))
      && (!op2def || gimple_nop_p (op2def)))
    {
      gsi = gsi_start_bb (single_succ (ENTRY_BLOCK_PTR));
      gsi_insert_before (&gsi, sum, GSI_NEW_STMT);
    }
  else if ((!op1def || gimple_nop_p (op1def))
	   || (op2def && !gimple_nop_p (op2def)
	       && stmt_dominates_stmt_p (op1def, op2def)))
    {
      if (gimple_code (op2def) == GIMPLE_PHI)
	{
	  gsi = gsi_start_bb (gimple_bb (op2def));
	  gsi_insert_before (&gsi, sum, GSI_NEW_STMT);
	}
      else
	{
	  if (!stmt_ends_bb_p (op2def))
	    {
	      gsi = gsi_for_stmt (op2def);
	      gsi_insert_after (&gsi, sum, GSI_NEW_STMT);
	    }
	  else
	    {
	      edge e;
	      edge_iterator ei;

	      FOR_EACH_EDGE (e, ei, gimple_bb (op2def)->succs)
		if (e->flags & EDGE_FALLTHRU)
		  gsi_insert_on_edge_immediate (e, sum);
	    }
	}
    }
  else
    {
      if (gimple_code (op1def) == GIMPLE_PHI)
	{
	  gsi = gsi_start_bb (gimple_bb (op1def));
	  gsi_insert_before (&gsi, sum, GSI_NEW_STMT);
	}
      else
	{
	  if (!stmt_ends_bb_p (op1def))
	    {
	      gsi = gsi_for_stmt (op1def);
	      gsi_insert_after (&gsi, sum, GSI_NEW_STMT);
	    }
	  else
	    {
	      edge e;
	      edge_iterator ei;

	      FOR_EACH_EDGE (e, ei, gimple_bb (op1def)->succs)
		if (e->flags & EDGE_FALLTHRU)
		  gsi_insert_on_edge_immediate (e, sum);
	    }
	}
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
      these candidates, counting the number of occurences of (operand, code)
      pairs in all of the candidates chains.

    - Third we sort the (operand, code) pairs by number of occurence and
      process them starting with the pair with the most uses.

      * For each such pair we walk the candidates again to build a
        second candidate bitmap noting all multiplication/division chains
	that have at least one occurence of (operand, code).

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
		       VEC (operand_entry_t, heap) **ops, struct loop *loop)
{
  unsigned int length = VEC_length (operand_entry_t, *ops);
  operand_entry_t oe1;
  unsigned i, j;
  sbitmap candidates, candidates2;
  unsigned nr_candidates, nr_candidates2;
  sbitmap_iterator sbi0;
  VEC (operand_entry_t, heap) **subops;
  htab_t ctable;
  bool changed = false;

  if (length <= 1
      || opcode != PLUS_EXPR)
    return false;

  /* Build a list of candidates to process.  */
  candidates = sbitmap_alloc (length);
  sbitmap_zero (candidates);
  nr_candidates = 0;
  for (i = 0; VEC_iterate (operand_entry_t, *ops, i, oe1); ++i)
    {
      enum tree_code dcode;
      gimple oe1def;

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

      SET_BIT (candidates, i);
      nr_candidates++;
    }

  if (nr_candidates < 2)
    {
      sbitmap_free (candidates);
      return false;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "searching for un-distribute opportunities ");
      print_generic_expr (dump_file,
	VEC_index (operand_entry_t, *ops,
		   sbitmap_first_set_bit (candidates))->op, 0);
      fprintf (dump_file, " %d\n", nr_candidates);
    }

  /* Build linearized sub-operand lists and the counting table.  */
  cvec = NULL;
  ctable = htab_create (15, oecount_hash, oecount_eq, NULL);
  subops = XCNEWVEC (VEC (operand_entry_t, heap) *,
		     VEC_length (operand_entry_t, *ops));
  EXECUTE_IF_SET_IN_SBITMAP (candidates, 0, i, sbi0)
    {
      gimple oedef;
      enum tree_code oecode;
      unsigned j;

      oedef = SSA_NAME_DEF_STMT (VEC_index (operand_entry_t, *ops, i)->op);
      oecode = gimple_assign_rhs_code (oedef);
      linearize_expr_tree (&subops[i], oedef,
			   associative_tree_code (oecode), false);

      for (j = 0; VEC_iterate (operand_entry_t, subops[i], j, oe1); ++j)
	{
	  oecount c;
	  void **slot;
	  size_t idx;
	  c.oecode = oecode;
	  c.cnt = 1;
	  c.op = oe1->op;
	  VEC_safe_push (oecount, heap, cvec, &c);
	  idx = VEC_length (oecount, cvec) + 41;
	  slot = htab_find_slot (ctable, (void *)idx, INSERT);
	  if (!*slot)
	    {
	      *slot = (void *)idx;
	    }
	  else
	    {
	      VEC_pop (oecount, cvec);
	      VEC_index (oecount, cvec, (size_t)*slot - 42)->cnt++;
	    }
	}
    }
  htab_delete (ctable);

  /* Sort the counting table.  */
  qsort (VEC_address (oecount, cvec), VEC_length (oecount, cvec),
	 sizeof (oecount), oecount_cmp);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      oecount *c;
      fprintf (dump_file, "Candidates:\n");
      for (j = 0; VEC_iterate (oecount, cvec, j, c); ++j)
	{
	  fprintf (dump_file, "  %u %s: ", c->cnt,
		   c->oecode == MULT_EXPR
		   ? "*" : c->oecode == RDIV_EXPR ? "/" : "?");
	  print_generic_expr (dump_file, c->op, 0);
	  fprintf (dump_file, "\n");
	}
    }

  /* Process the (operand, code) pairs in order of most occurence.  */
  candidates2 = sbitmap_alloc (length);
  while (!VEC_empty (oecount, cvec))
    {
      oecount *c = VEC_last (oecount, cvec);
      if (c->cnt < 2)
	break;

      /* Now collect the operands in the outer chain that contain
         the common operand in their inner chain.  */
      sbitmap_zero (candidates2);
      nr_candidates2 = 0;
      EXECUTE_IF_SET_IN_SBITMAP (candidates, 0, i, sbi0)
	{
	  gimple oedef;
	  enum tree_code oecode;
	  unsigned j;
	  tree op = VEC_index (operand_entry_t, *ops, i)->op;

	  /* If we undistributed in this chain already this may be
	     a constant.  */
	  if (TREE_CODE (op) != SSA_NAME)
	    continue;

	  oedef = SSA_NAME_DEF_STMT (op);
	  oecode = gimple_assign_rhs_code (oedef);
	  if (oecode != c->oecode)
	    continue;

	  for (j = 0; VEC_iterate (operand_entry_t, subops[i], j, oe1); ++j)
	    {
	      if (oe1->op == c->op)
		{
		  SET_BIT (candidates2, i);
		  ++nr_candidates2;
		  break;
		}
	    }
	}

      if (nr_candidates2 >= 2)
	{
	  operand_entry_t oe1, oe2;
	  tree tmpvar;
	  gimple prod;
	  int first = sbitmap_first_set_bit (candidates2);

	  /* Build the new addition chain.  */
	  oe1 = VEC_index (operand_entry_t, *ops, first);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Building (");
	      print_generic_expr (dump_file, oe1->op, 0);
	    }
	  tmpvar = create_tmp_var (TREE_TYPE (oe1->op), NULL);
	  add_referenced_var (tmpvar);
	  zero_one_operation (&oe1->op, c->oecode, c->op);
	  EXECUTE_IF_SET_IN_SBITMAP (candidates2, first+1, i, sbi0)
	    {
	      gimple sum;
	      oe2 = VEC_index (operand_entry_t, *ops, i);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, " + ");
		  print_generic_expr (dump_file, oe2->op, 0);
		}
	      zero_one_operation (&oe2->op, c->oecode, c->op);
	      sum = build_and_add_sum (tmpvar, oe1->op, oe2->op, opcode);
	      oe2->op = fold_convert (TREE_TYPE (oe2->op), integer_zero_node);
	      oe2->rank = 0;
	      oe1->op = gimple_get_lhs (sum);
	    }

	  /* Apply the multiplication/division.  */
	  prod = build_and_add_sum (tmpvar, oe1->op, c->op, c->oecode);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, ") %s ", c->oecode == MULT_EXPR ? "*" : "/");
	      print_generic_expr (dump_file, c->op, 0);
	      fprintf (dump_file, "\n");
	    }

	  /* Record it in the addition chain and disable further
	     undistribution with this op.  */
	  oe1->op = gimple_assign_lhs (prod);
	  oe1->rank = get_rank (oe1->op);
	  VEC_free (operand_entry_t, heap, subops[first]);

	  changed = true;
	}

      VEC_pop (oecount, cvec);
    }

  for (i = 0; i < VEC_length (operand_entry_t, *ops); ++i)
    VEC_free (operand_entry_t, heap, subops[i]);
  free (subops);
  VEC_free (oecount, heap, cvec);
  sbitmap_free (candidates);
  sbitmap_free (candidates2);

  return changed;
}


/* Perform various identities and other optimizations on the list of
   operand entries, stored in OPS.  The tree code for the binary
   operation between all the operands is OPCODE.  */

static void
optimize_ops_list (enum tree_code opcode,
		   VEC (operand_entry_t, heap) **ops)
{
  unsigned int length = VEC_length (operand_entry_t, *ops);
  unsigned int i;
  operand_entry_t oe;
  operand_entry_t oelast = NULL;
  bool iterate = false;

  if (length == 1)
    return;

  oelast = VEC_last (operand_entry_t, *ops);

  /* If the last two are constants, pop the constants off, merge them
     and try the next two.  */
  if (oelast->rank == 0 && is_gimple_min_invariant (oelast->op))
    {
      operand_entry_t oelm1 = VEC_index (operand_entry_t, *ops, length - 2);

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

	      VEC_pop (operand_entry_t, *ops);
	      VEC_pop (operand_entry_t, *ops);

	      add_to_ops_vec (ops, folded);
	      reassociate_stats.constants_eliminated++;

	      optimize_ops_list (opcode, ops);
	      return;
	    }
	}
    }

  eliminate_using_constants (opcode, ops);
  oelast = NULL;

  for (i = 0; VEC_iterate (operand_entry_t, *ops, i, oe);)
    {
      bool done = false;

      if (eliminate_not_pairs (opcode, ops, i, oe))
	return;
      if (eliminate_duplicate_pair (opcode, ops, &done, i, oe, oelast)
	  || (!done && eliminate_plus_minus_pair (opcode, ops, i, oe)))
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

  length  = VEC_length (operand_entry_t, *ops);
  oelast = VEC_last (operand_entry_t, *ops);

  if (iterate)
    optimize_ops_list (opcode, ops);
}

/* Return true if OPERAND is defined by a PHI node which uses the LHS
   of STMT in it's operands.  This is also known as a "destructive
   update" operation.  */

static bool
is_phi_for_stmt (gimple stmt, tree operand)
{
  gimple def_stmt;
  tree lhs;
  use_operand_p arg_p;
  ssa_op_iter i;

  if (TREE_CODE (operand) != SSA_NAME)
    return false;

  lhs = gimple_assign_lhs (stmt);

  def_stmt = SSA_NAME_DEF_STMT (operand);
  if (gimple_code (def_stmt) != GIMPLE_PHI)
    return false;

  FOR_EACH_PHI_ARG (arg_p, def_stmt, i, SSA_OP_USE)
    if (lhs == USE_FROM_PTR (arg_p))
      return true;
  return false;
}

/* Remove def stmt of VAR if VAR has zero uses and recurse
   on rhs1 operand if so.  */

static void
remove_visited_stmt_chain (tree var)
{
  gimple stmt;
  gimple_stmt_iterator gsi;

  while (1)
    {
      if (TREE_CODE (var) != SSA_NAME || !has_zero_uses (var))
	return;
      stmt = SSA_NAME_DEF_STMT (var);
      if (!is_gimple_assign (stmt)
	  || !gimple_visited_p (stmt))
	return;
      var = gimple_assign_rhs1 (stmt);
      gsi = gsi_for_stmt (stmt);
      gsi_remove (&gsi, true);
      release_defs (stmt);
    }
}

/* Recursively rewrite our linearized statements so that the operators
   match those in OPS[OPINDEX], putting the computation in rank
   order.  */

static void
rewrite_expr_tree (gimple stmt, unsigned int opindex,
		   VEC(operand_entry_t, heap) * ops, bool moved)
{
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);
  operand_entry_t oe;

  /* If we have three operands left, then we want to make sure the one
     that gets the double binary op are the ones with the same rank.

     The alternative we try is to see if this is a destructive
     update style statement, which is like:
     b = phi (a, ...)
     a = c + b;
     In that case, we want to use the destructive update form to
     expose the possible vectorizer sum reduction opportunity.
     In that case, the third operand will be the phi node.

     We could, of course, try to be better as noted above, and do a
     lot of work to try to find these opportunities in >3 operand
     cases, but it is unlikely to be worth it.  */
  if (opindex + 3 == VEC_length (operand_entry_t, ops))
    {
      operand_entry_t oe1, oe2, oe3;

      oe1 = VEC_index (operand_entry_t, ops, opindex);
      oe2 = VEC_index (operand_entry_t, ops, opindex + 1);
      oe3 = VEC_index (operand_entry_t, ops, opindex + 2);

      if ((oe1->rank == oe2->rank
	   && oe2->rank != oe3->rank)
	  || (is_phi_for_stmt (stmt, oe3->op)
	      && !is_phi_for_stmt (stmt, oe1->op)
	      && !is_phi_for_stmt (stmt, oe2->op)))
	{
	  struct operand_entry temp = *oe3;
	  oe3->op = oe1->op;
	  oe3->rank = oe1->rank;
	  oe1->op = temp.op;
	  oe1->rank= temp.rank;
	}
      else if ((oe1->rank == oe3->rank
		&& oe2->rank != oe3->rank)
	       || (is_phi_for_stmt (stmt, oe2->op)
		   && !is_phi_for_stmt (stmt, oe1->op)
		   && !is_phi_for_stmt (stmt, oe3->op)))
	{
	  struct operand_entry temp = *oe2;
	  oe2->op = oe1->op;
	  oe2->rank = oe1->rank;
	  oe1->op = temp.op;
	  oe1->rank= temp.rank;
	}
    }

  /* The final recursion case for this function is that you have
     exactly two operations left.
     If we had one exactly one op in the entire list to start with, we
     would have never called this function, and the tail recursion
     rewrites them one at a time.  */
  if (opindex + 2 == VEC_length (operand_entry_t, ops))
    {
      operand_entry_t oe1, oe2;

      oe1 = VEC_index (operand_entry_t, ops, opindex);
      oe2 = VEC_index (operand_entry_t, ops, opindex + 1);

      if (rhs1 != oe1->op || rhs2 != oe2->op)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Transforming ");
	      print_gimple_stmt (dump_file, stmt, 0, 0);
	    }

	  gimple_assign_set_rhs1 (stmt, oe1->op);
	  gimple_assign_set_rhs2 (stmt, oe2->op);
	  update_stmt (stmt);
	  if (rhs1 != oe1->op && rhs1 != oe2->op)
	    remove_visited_stmt_chain (rhs1);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, " into ");
	      print_gimple_stmt (dump_file, stmt, 0, 0);
	    }

	}
      return;
    }

  /* If we hit here, we should have 3 or more ops left.  */
  gcc_assert (opindex + 2 < VEC_length (operand_entry_t, ops));

  /* Rewrite the next operator.  */
  oe = VEC_index (operand_entry_t, ops, opindex);

  if (oe->op != rhs2)
    {
      if (!moved)
	{
	  gimple_stmt_iterator gsinow, gsirhs1;
	  gimple stmt1 = stmt, stmt2;
	  unsigned int count;

	  gsinow = gsi_for_stmt (stmt);
	  count = VEC_length (operand_entry_t, ops) - opindex - 2;
	  while (count-- != 0)
	    {
	      stmt2 = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt1));
	      gsirhs1 = gsi_for_stmt (stmt2);
	      propagate_defs_into_debug_stmts (stmt2, gimple_bb (stmt), &gsinow);
	      gsi_move_before (&gsirhs1, &gsinow);
	      gsi_prev (&gsinow);
	      stmt1 = stmt2;
	    }
	  moved = true;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Transforming ");
	  print_gimple_stmt (dump_file, stmt, 0, 0);
	}

      gimple_assign_set_rhs2 (stmt, oe->op);
      update_stmt (stmt);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, " into ");
	  print_gimple_stmt (dump_file, stmt, 0, 0);
	}
    }
  /* Recurse on the LHS of the binary operator, which is guaranteed to
     be the non-leaf side.  */
  rewrite_expr_tree (SSA_NAME_DEF_STMT (rhs1), opindex + 1, ops, moved);
}

/* Transform STMT, which is really (A +B) + (C + D) into the left
   linear form, ((A+B)+C)+D.
   Recurse on D if necessary.  */

static void
linearize_expr (gimple stmt)
{
  gimple_stmt_iterator gsinow, gsirhs;
  gimple binlhs = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));
  gimple binrhs = SSA_NAME_DEF_STMT (gimple_assign_rhs2 (stmt));
  enum tree_code rhscode = gimple_assign_rhs_code (stmt);
  gimple newbinrhs = NULL;
  struct loop *loop = loop_containing_stmt (stmt);

  gcc_assert (is_reassociable_op (binlhs, rhscode, loop)
	      && is_reassociable_op (binrhs, rhscode, loop));

  gsinow = gsi_for_stmt (stmt);
  gsirhs = gsi_for_stmt (binrhs);
  propagate_defs_into_debug_stmts (binrhs, gimple_bb (stmt), &gsinow);
  gsi_move_before (&gsirhs, &gsinow);

  gimple_assign_set_rhs2 (stmt, gimple_assign_rhs1 (binrhs));
  gimple_assign_set_rhs1 (binrhs, gimple_assign_lhs (binlhs));
  gimple_assign_set_rhs1 (stmt, gimple_assign_lhs (binrhs));

  if (TREE_CODE (gimple_assign_rhs2 (stmt)) == SSA_NAME)
    newbinrhs = SSA_NAME_DEF_STMT (gimple_assign_rhs2 (stmt));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Linearized: ");
      print_gimple_stmt (dump_file, stmt, 0, 0);
    }

  reassociate_stats.linearized++;
  update_stmt (binrhs);
  update_stmt (binlhs);
  update_stmt (stmt);

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

static gimple
get_single_immediate_use (tree lhs)
{
  use_operand_p immuse;
  gimple immusestmt;

  if (TREE_CODE (lhs) == SSA_NAME
      && single_imm_use (lhs, &immuse, &immusestmt)
      && is_gimple_assign (immusestmt))
    return immusestmt;

  return NULL;
}

static VEC(tree, heap) *broken_up_subtracts;

/* Recursively negate the value of TONEGATE, and return the SSA_NAME
   representing the negated value.  Insertions of any necessary
   instructions go before GSI.
   This function is recursive in that, if you hand it "a_5" as the
   value to negate, and a_5 is defined by "a_5 = b_3 + b_4", it will
   transform b_3 + b_4 into a_5 = -b_3 + -b_4.  */

static tree
negate_value (tree tonegate, gimple_stmt_iterator *gsi)
{
  gimple negatedefstmt= NULL;
  tree resultofnegate;

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
      gimple_stmt_iterator gsi;
      tree rhs1 = gimple_assign_rhs1 (negatedefstmt);
      tree rhs2 = gimple_assign_rhs2 (negatedefstmt);

      gsi = gsi_for_stmt (negatedefstmt);
      rhs1 = negate_value (rhs1, &gsi);
      gimple_assign_set_rhs1 (negatedefstmt, rhs1);

      gsi = gsi_for_stmt (negatedefstmt);
      rhs2 = negate_value (rhs2, &gsi);
      gimple_assign_set_rhs2 (negatedefstmt, rhs2);

      update_stmt (negatedefstmt);
      return gimple_assign_lhs (negatedefstmt);
    }

  tonegate = fold_build1 (NEGATE_EXPR, TREE_TYPE (tonegate), tonegate);
  resultofnegate = force_gimple_operand_gsi (gsi, tonegate, true,
					     NULL_TREE, true, GSI_SAME_STMT);
  VEC_safe_push (tree, heap, broken_up_subtracts, resultofnegate);
  return resultofnegate;
}

/* Return true if we should break up the subtract in STMT into an add
   with negate.  This is true when we the subtract operands are really
   adds, or the subtract itself is used in an add expression.  In
   either case, breaking up the subtract into an add with negate
   exposes the adds to reassociation.  */

static bool
should_break_up_subtract (gimple stmt)
{
  tree lhs = gimple_assign_lhs (stmt);
  tree binlhs = gimple_assign_rhs1 (stmt);
  tree binrhs = gimple_assign_rhs2 (stmt);
  gimple immusestmt;
  struct loop *loop = loop_containing_stmt (stmt);

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
	  ||  gimple_assign_rhs_code (immusestmt) == MULT_EXPR))
    return true;
  return false;
}

/* Transform STMT from A - B into A + -B.  */

static void
break_up_subtract (gimple stmt, gimple_stmt_iterator *gsip)
{
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Breaking up subtract ");
      print_gimple_stmt (dump_file, stmt, 0, 0);
    }

  rhs2 = negate_value (rhs2, gsip);
  gimple_assign_set_rhs_with_ops (gsip, PLUS_EXPR, rhs1, rhs2);
  update_stmt (stmt);
}

/* Recursively linearize a binary expression that is the RHS of STMT.
   Place the operands of the expression tree in the vector named OPS.  */

static void
linearize_expr_tree (VEC(operand_entry_t, heap) **ops, gimple stmt,
		     bool is_associative, bool set_visited)
{
  tree binlhs = gimple_assign_rhs1 (stmt);
  tree binrhs = gimple_assign_rhs2 (stmt);
  gimple binlhsdef, binrhsdef;
  bool binlhsisreassoc = false;
  bool binrhsisreassoc = false;
  enum tree_code rhscode = gimple_assign_rhs_code (stmt);
  struct loop *loop = loop_containing_stmt (stmt);

  if (set_visited)
    gimple_set_visited (stmt, true);

  if (TREE_CODE (binlhs) == SSA_NAME)
    {
      binlhsdef = SSA_NAME_DEF_STMT (binlhs);
      binlhsisreassoc = is_reassociable_op (binlhsdef, rhscode, loop);
    }

  if (TREE_CODE (binrhs) == SSA_NAME)
    {
      binrhsdef = SSA_NAME_DEF_STMT (binrhs);
      binrhsisreassoc = is_reassociable_op (binrhsdef, rhscode, loop);
    }

  /* If the LHS is not reassociable, but the RHS is, we need to swap
     them.  If neither is reassociable, there is nothing we can do, so
     just put them in the ops vector.  If the LHS is reassociable,
     linearize it.  If both are reassociable, then linearize the RHS
     and the LHS.  */

  if (!binlhsisreassoc)
    {
      tree temp;

      /* If this is not a associative operation like division, give up.  */
      if (!is_associative)
	{
	  add_to_ops_vec (ops, binrhs);
	  return;
	}

      if (!binrhsisreassoc)
	{
	  add_to_ops_vec (ops, binrhs);
	  add_to_ops_vec (ops, binlhs);
	  return;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "swapping operands of ");
	  print_gimple_stmt (dump_file, stmt, 0, 0);
	}

      swap_tree_operands (stmt,
			  gimple_assign_rhs1_ptr (stmt),
			  gimple_assign_rhs2_ptr (stmt));
      update_stmt (stmt);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, " is now ");
	  print_gimple_stmt (dump_file, stmt, 0, 0);
	}

      /* We want to make it so the lhs is always the reassociative op,
	 so swap.  */
      temp = binlhs;
      binlhs = binrhs;
      binrhs = temp;
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
  add_to_ops_vec (ops, binrhs);
}

/* Repropagate the negates back into subtracts, since no other pass
   currently does it.  */

static void
repropagate_negates (void)
{
  unsigned int i = 0;
  tree negate;

  for (i = 0; VEC_iterate (tree, broken_up_subtracts, i, negate); i++)
    {
      gimple user = get_single_immediate_use (negate);

      /* The negate operand can be either operand of a PLUS_EXPR
	 (it can be the LHS if the RHS is a constant for example).

	 Force the negate operand to the RHS of the PLUS_EXPR, then
	 transform the PLUS_EXPR into a MINUS_EXPR.  */
      if (user
	  && is_gimple_assign (user)
	  && gimple_assign_rhs_code (user) == PLUS_EXPR)
	{
	  /* If the negated operand appears on the LHS of the
	     PLUS_EXPR, exchange the operands of the PLUS_EXPR
	     to force the negated operand to the RHS of the PLUS_EXPR.  */
	  if (gimple_assign_rhs1 (user) == negate)
	    {
	      swap_tree_operands (user,
				  gimple_assign_rhs1_ptr (user),
				  gimple_assign_rhs2_ptr (user));
	    }

	  /* Now transform the PLUS_EXPR into a MINUS_EXPR and replace
	     the RHS of the PLUS_EXPR with the operand of the NEGATE_EXPR.  */
	  if (gimple_assign_rhs2 (user) == negate)
	    {
	      tree rhs1 = gimple_assign_rhs1 (user);
	      tree rhs2 = get_unary_op (negate, NEGATE_EXPR);
	      gimple_stmt_iterator gsi = gsi_for_stmt (user);
	      gimple_assign_set_rhs_with_ops (&gsi, MINUS_EXPR, rhs1, rhs2);
	      update_stmt (user);
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

   En passant, clear the GIMPLE visited flag on every statement.  */

static void
break_up_subtract_bb (basic_block bb)
{
  gimple_stmt_iterator gsi;
  basic_block son;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      gimple_set_visited (stmt, false);

      /* Look for simple gimple subtract operations.  */
      if (is_gimple_assign (stmt)
	  && gimple_assign_rhs_code (stmt) == MINUS_EXPR)
	{
	  tree lhs = gimple_assign_lhs (stmt);
	  tree rhs1 = gimple_assign_rhs1 (stmt);
	  tree rhs2 = gimple_assign_rhs2 (stmt);

	  /* If associative-math we can do reassociation for
	     non-integral types.  Or, we can do reassociation for
	     non-saturating fixed-point types.  */
	  if ((!INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	       || !INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
	       || !INTEGRAL_TYPE_P (TREE_TYPE (rhs2)))
	      && (!SCALAR_FLOAT_TYPE_P (TREE_TYPE (lhs))
		  || !SCALAR_FLOAT_TYPE_P (TREE_TYPE(rhs1))
		  || !SCALAR_FLOAT_TYPE_P (TREE_TYPE(rhs2))
		  || !flag_associative_math)
	      && (!NON_SAT_FIXED_POINT_TYPE_P (TREE_TYPE (lhs))
		  || !NON_SAT_FIXED_POINT_TYPE_P (TREE_TYPE(rhs1))
		  || !NON_SAT_FIXED_POINT_TYPE_P (TREE_TYPE(rhs2))))
	    continue;

	  /* Check for a subtract used only in an addition.  If this
	     is the case, transform it into add of a negate for better
	     reassociation.  IE transform C = A-B into C = A + -B if C
	     is only used in an addition.  */
	  if (should_break_up_subtract (stmt))
	    break_up_subtract (stmt, &gsi);
	}
    }
  for (son = first_dom_son (CDI_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    break_up_subtract_bb (son);
}

/* Reassociate expressions in basic block BB and its post-dominator as
   children.  */

static void
reassociate_bb (basic_block bb)
{
  gimple_stmt_iterator gsi;
  basic_block son;

  for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);

      if (is_gimple_assign (stmt))
	{
	  tree lhs, rhs1, rhs2;
	  enum tree_code rhs_code = gimple_assign_rhs_code (stmt);

	  /* If this is not a gimple binary expression, there is
	     nothing for us to do with it.  */
	  if (get_gimple_rhs_class (rhs_code) != GIMPLE_BINARY_RHS)
	    continue;

	  /* If this was part of an already processed statement,
	     we don't need to touch it again. */
	  if (gimple_visited_p (stmt))
	    {
	      /* This statement might have become dead because of previous
		 reassociations.  */
	      if (has_zero_uses (gimple_get_lhs (stmt)))
		{
		  gsi_remove (&gsi, true);
		  release_defs (stmt);
		  /* We might end up removing the last stmt above which
		     places the iterator to the end of the sequence.
		     Reset it to the last stmt in this case which might
		     be the end of the sequence as well if we removed
		     the last statement of the sequence.  In which case
		     we need to bail out.  */
		  if (gsi_end_p (gsi))
		    {
		      gsi = gsi_last_bb (bb);
		      if (gsi_end_p (gsi))
			break;
		    }
		}
	      continue;
	    }

	  lhs = gimple_assign_lhs (stmt);
	  rhs1 = gimple_assign_rhs1 (stmt);
	  rhs2 = gimple_assign_rhs2 (stmt);

	  /* If associative-math we can do reassociation for
	     non-integral types.  Or, we can do reassociation for
	     non-saturating fixed-point types.  */
	  if ((!INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	       || !INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
	       || !INTEGRAL_TYPE_P (TREE_TYPE (rhs2)))
	      && (!SCALAR_FLOAT_TYPE_P (TREE_TYPE (lhs))
		  || !SCALAR_FLOAT_TYPE_P (TREE_TYPE(rhs1))
		  || !SCALAR_FLOAT_TYPE_P (TREE_TYPE(rhs2))
		  || !flag_associative_math)
	      && (!NON_SAT_FIXED_POINT_TYPE_P (TREE_TYPE (lhs))
		  || !NON_SAT_FIXED_POINT_TYPE_P (TREE_TYPE(rhs1))
		  || !NON_SAT_FIXED_POINT_TYPE_P (TREE_TYPE(rhs2))))
	    continue;

	  if (associative_tree_code (rhs_code))
	    {
	      VEC(operand_entry_t, heap) *ops = NULL;

	      /* There may be no immediate uses left by the time we
		 get here because we may have eliminated them all.  */
	      if (TREE_CODE (lhs) == SSA_NAME && has_zero_uses (lhs))
		continue;

	      gimple_set_visited (stmt, true);
	      linearize_expr_tree (&ops, stmt, true, true);
	      qsort (VEC_address (operand_entry_t, ops),
		     VEC_length (operand_entry_t, ops),
		     sizeof (operand_entry_t),
		     sort_by_operand_rank);
	      optimize_ops_list (rhs_code, &ops);
	      if (undistribute_ops_list (rhs_code, &ops,
					 loop_containing_stmt (stmt)))
		{
		  qsort (VEC_address (operand_entry_t, ops),
			 VEC_length (operand_entry_t, ops),
			 sizeof (operand_entry_t),
			 sort_by_operand_rank);
		  optimize_ops_list (rhs_code, &ops);
		}

	      if (VEC_length (operand_entry_t, ops) == 1)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Transforming ");
		      print_gimple_stmt (dump_file, stmt, 0, 0);
		    }

		  rhs1 = gimple_assign_rhs1 (stmt);
		  gimple_assign_set_rhs_from_tree (&gsi,
						   VEC_last (operand_entry_t,
							     ops)->op);
		  update_stmt (stmt);
		  remove_visited_stmt_chain (rhs1);

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, " into ");
		      print_gimple_stmt (dump_file, stmt, 0, 0);
		    }
		}
	      else
		rewrite_expr_tree (stmt, 0, ops, false);

	      VEC_free (operand_entry_t, heap, ops);
	    }
	}
    }
  for (son = first_dom_son (CDI_POST_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_POST_DOMINATORS, son))
    reassociate_bb (son);
}

void dump_ops_vector (FILE *file, VEC (operand_entry_t, heap) *ops);
void debug_ops_vector (VEC (operand_entry_t, heap) *ops);

/* Dump the operand entry vector OPS to FILE.  */

void
dump_ops_vector (FILE *file, VEC (operand_entry_t, heap) *ops)
{
  operand_entry_t oe;
  unsigned int i;

  for (i = 0; VEC_iterate (operand_entry_t, ops, i, oe); i++)
    {
      fprintf (file, "Op %d -> rank: %d, tree: ", i, oe->rank);
      print_generic_expr (file, oe->op, 0);
    }
}

/* Dump the operand entry vector OPS to STDERR.  */

void
debug_ops_vector (VEC (operand_entry_t, heap) *ops)
{
  dump_ops_vector (stderr, ops);
}

static void
do_reassoc (void)
{
  break_up_subtract_bb (ENTRY_BLOCK_PTR);
  reassociate_bb (EXIT_BLOCK_PTR);
}

/* Initialize the reassociation pass.  */

static void
init_reassoc (void)
{
  int i;
  long rank = 2;
  tree param;
  int *bbs = XNEWVEC (int, last_basic_block + 1);

  /* Find the loops, so that we can prevent moving calculations in
     them.  */
  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);

  memset (&reassociate_stats, 0, sizeof (reassociate_stats));

  operand_entry_pool = create_alloc_pool ("operand entry pool",
					  sizeof (struct operand_entry), 30);

  /* Reverse RPO (Reverse Post Order) will give us something where
     deeper loops come later.  */
  pre_and_rev_post_order_compute (NULL, bbs, false);
  bb_rank = XCNEWVEC (long, last_basic_block + 1);
  operand_rank = pointer_map_create ();

  /* Give each argument a distinct rank.   */
  for (param = DECL_ARGUMENTS (current_function_decl);
       param;
       param = TREE_CHAIN (param))
    {
      if (gimple_default_def (cfun, param) != NULL)
	{
	  tree def = gimple_default_def (cfun, param);
	  insert_operand_rank (def, ++rank);
	}
    }

  /* Give the chain decl a distinct rank. */
  if (cfun->static_chain_decl != NULL)
    {
      tree def = gimple_default_def (cfun, cfun->static_chain_decl);
      if (def != NULL)
	insert_operand_rank (def, ++rank);
    }

  /* Set up rank for each BB  */
  for (i = 0; i < n_basic_blocks - NUM_FIXED_BLOCKS; i++)
    bb_rank[bbs[i]] = ++rank  << 16;

  free (bbs);
  calculate_dominance_info (CDI_POST_DOMINATORS);
  broken_up_subtracts = NULL;
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

  pointer_map_destroy (operand_rank);
  free_alloc_pool (operand_entry_pool);
  free (bb_rank);
  VEC_free (tree, heap, broken_up_subtracts);
  free_dominance_info (CDI_POST_DOMINATORS);
  loop_optimizer_finalize ();
}

/* Gate and execute functions for Reassociation.  */

static unsigned int
execute_reassoc (void)
{
  init_reassoc ();

  do_reassoc ();
  repropagate_negates ();

  fini_reassoc ();
  return 0;
}

static bool
gate_tree_ssa_reassoc (void)
{
  return flag_tree_reassoc != 0;
}

struct gimple_opt_pass pass_reassoc =
{
 {
  GIMPLE_PASS,
  "reassoc",				/* name */
  gate_tree_ssa_reassoc,		/* gate */
  execute_reassoc,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_REASSOC,			/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_ggc_collect | TODO_verify_ssa /* todo_flags_finish */
 }
};

