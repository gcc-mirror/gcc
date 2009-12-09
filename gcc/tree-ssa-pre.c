/* SSA-PRE for trees.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dan@dberlin.org> and Steven Bosscher
   <stevenb@suse.de>

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
#include "fibheap.h"
#include "hashtab.h"
#include "tree-iterator.h"
#include "real.h"
#include "alloc-pool.h"
#include "obstack.h"
#include "tree-pass.h"
#include "flags.h"
#include "bitmap.h"
#include "langhooks.h"
#include "cfgloop.h"
#include "tree-ssa-sccvn.h"
#include "tree-scalar-evolution.h"
#include "params.h"
#include "dbgcnt.h"

/* TODO:

   1. Avail sets can be shared by making an avail_find_leader that
      walks up the dominator tree and looks in those avail sets.
      This might affect code optimality, it's unclear right now.
   2. Strength reduction can be performed by anticipating expressions
      we can repair later on.
   3. We can do back-substitution or smarter value numbering to catch
      commutative expressions split up over multiple statements.
*/

/* For ease of terminology, "expression node" in the below refers to
   every expression node but GIMPLE_ASSIGN, because GIMPLE_ASSIGNs
   represent the actual statement containing the expressions we care about,
   and we cache the value number by putting it in the expression.  */

/* Basic algorithm

   First we walk the statements to generate the AVAIL sets, the
   EXP_GEN sets, and the tmp_gen sets.  EXP_GEN sets represent the
   generation of values/expressions by a given block.  We use them
   when computing the ANTIC sets.  The AVAIL sets consist of
   SSA_NAME's that represent values, so we know what values are
   available in what blocks.  AVAIL is a forward dataflow problem.  In
   SSA, values are never killed, so we don't need a kill set, or a
   fixpoint iteration, in order to calculate the AVAIL sets.  In
   traditional parlance, AVAIL sets tell us the downsafety of the
   expressions/values.

   Next, we generate the ANTIC sets.  These sets represent the
   anticipatable expressions.  ANTIC is a backwards dataflow
   problem.  An expression is anticipatable in a given block if it could
   be generated in that block.  This means that if we had to perform
   an insertion in that block, of the value of that expression, we
   could.  Calculating the ANTIC sets requires phi translation of
   expressions, because the flow goes backwards through phis.  We must
   iterate to a fixpoint of the ANTIC sets, because we have a kill
   set.  Even in SSA form, values are not live over the entire
   function, only from their definition point onwards.  So we have to
   remove values from the ANTIC set once we go past the definition
   point of the leaders that make them up.
   compute_antic/compute_antic_aux performs this computation.

   Third, we perform insertions to make partially redundant
   expressions fully redundant.

   An expression is partially redundant (excluding partial
   anticipation) if:

   1. It is AVAIL in some, but not all, of the predecessors of a
      given block.
   2. It is ANTIC in all the predecessors.

   In order to make it fully redundant, we insert the expression into
   the predecessors where it is not available, but is ANTIC.

   For the partial anticipation case, we only perform insertion if it
   is partially anticipated in some block, and fully available in all
   of the predecessors.

   insert/insert_aux/do_regular_insertion/do_partial_partial_insertion
   performs these steps.

   Fourth, we eliminate fully redundant expressions.
   This is a simple statement walk that replaces redundant
   calculations with the now available values.  */

/* Representations of value numbers:

   Value numbers are represented by a representative SSA_NAME.  We
   will create fake SSA_NAME's in situations where we need a
   representative but do not have one (because it is a complex
   expression).  In order to facilitate storing the value numbers in
   bitmaps, and keep the number of wasted SSA_NAME's down, we also
   associate a value_id with each value number, and create full blown
   ssa_name's only where we actually need them (IE in operands of
   existing expressions).

   Theoretically you could replace all the value_id's with
   SSA_NAME_VERSION, but this would allocate a large number of
   SSA_NAME's (which are each > 30 bytes) just to get a 4 byte number.
   It would also require an additional indirection at each point we
   use the value id.  */

/* Representation of expressions on value numbers:

   Expressions consisting of value numbers are represented the same
   way as our VN internally represents them, with an additional
   "pre_expr" wrapping around them in order to facilitate storing all
   of the expressions in the same sets.  */

/* Representation of sets:

   The dataflow sets do not need to be sorted in any particular order
   for the majority of their lifetime, are simply represented as two
   bitmaps, one that keeps track of values present in the set, and one
   that keeps track of expressions present in the set.

   When we need them in topological order, we produce it on demand by
   transforming the bitmap into an array and sorting it into topo
   order.  */

/* Type of expression, used to know which member of the PRE_EXPR union
   is valid.  */

enum pre_expr_kind
{
    NAME,
    NARY,
    REFERENCE,
    CONSTANT
};

typedef union pre_expr_union_d
{
  tree name;
  tree constant;
  vn_nary_op_t nary;
  vn_reference_t reference;
} pre_expr_union;

typedef struct pre_expr_d
{
  enum pre_expr_kind kind;
  unsigned int id;
  pre_expr_union u;
} *pre_expr;

#define PRE_EXPR_NAME(e) (e)->u.name
#define PRE_EXPR_NARY(e) (e)->u.nary
#define PRE_EXPR_REFERENCE(e) (e)->u.reference
#define PRE_EXPR_CONSTANT(e) (e)->u.constant

static int
pre_expr_eq (const void *p1, const void *p2)
{
  const struct pre_expr_d *e1 = (const struct pre_expr_d *) p1;
  const struct pre_expr_d *e2 = (const struct pre_expr_d *) p2;

  if (e1->kind != e2->kind)
    return false;

  switch (e1->kind)
    {
    case CONSTANT:
      return vn_constant_eq_with_type (PRE_EXPR_CONSTANT (e1),
				       PRE_EXPR_CONSTANT (e2));
    case NAME:
      return PRE_EXPR_NAME (e1) == PRE_EXPR_NAME (e2);
    case NARY:
      return vn_nary_op_eq (PRE_EXPR_NARY (e1), PRE_EXPR_NARY (e2));
    case REFERENCE:
      return vn_reference_eq (PRE_EXPR_REFERENCE (e1),
			      PRE_EXPR_REFERENCE (e2));
    default:
      abort();
    }
}

static hashval_t
pre_expr_hash (const void *p1)
{
  const struct pre_expr_d *e = (const struct pre_expr_d *) p1;
  switch (e->kind)
    {
    case CONSTANT:
      return vn_hash_constant_with_type (PRE_EXPR_CONSTANT (e));
    case NAME:
      return iterative_hash_hashval_t (SSA_NAME_VERSION (PRE_EXPR_NAME (e)), 0);
    case NARY:
      return PRE_EXPR_NARY (e)->hashcode;
    case REFERENCE:
      return PRE_EXPR_REFERENCE (e)->hashcode;
    default:
      abort ();
    }
}


/* Next global expression id number.  */
static unsigned int next_expression_id;

/* Mapping from expression to id number we can use in bitmap sets.  */
DEF_VEC_P (pre_expr);
DEF_VEC_ALLOC_P (pre_expr, heap);
static VEC(pre_expr, heap) *expressions;
static htab_t expression_to_id;

/* Allocate an expression id for EXPR.  */

static inline unsigned int
alloc_expression_id (pre_expr expr)
{
  void **slot;
  /* Make sure we won't overflow. */
  gcc_assert (next_expression_id + 1 > next_expression_id);
  expr->id = next_expression_id++;
  VEC_safe_push (pre_expr, heap, expressions, expr);
  slot = htab_find_slot (expression_to_id, expr, INSERT);
  gcc_assert (!*slot);
  *slot = expr;
  return next_expression_id - 1;
}

/* Return the expression id for tree EXPR.  */

static inline unsigned int
get_expression_id (const pre_expr expr)
{
  return expr->id;
}

static inline unsigned int
lookup_expression_id (const pre_expr expr)
{
  void **slot;

  slot = htab_find_slot (expression_to_id, expr, NO_INSERT);
  if (!slot)
    return 0;
  return ((pre_expr)*slot)->id;
}

/* Return the existing expression id for EXPR, or create one if one
   does not exist yet.  */

static inline unsigned int
get_or_alloc_expression_id (pre_expr expr)
{
  unsigned int id = lookup_expression_id (expr);
  if (id == 0)
    return alloc_expression_id (expr);
  return expr->id = id;
}

/* Return the expression that has expression id ID */

static inline pre_expr
expression_for_id (unsigned int id)
{
  return VEC_index (pre_expr, expressions, id);
}

/* Free the expression id field in all of our expressions,
   and then destroy the expressions array.  */

static void
clear_expression_ids (void)
{
  VEC_free (pre_expr, heap, expressions);
}

static alloc_pool pre_expr_pool;

/* Given an SSA_NAME NAME, get or create a pre_expr to represent it.  */

static pre_expr
get_or_alloc_expr_for_name (tree name)
{
  pre_expr result = (pre_expr) pool_alloc (pre_expr_pool);
  unsigned int result_id;

  result->kind = NAME;
  result->id = 0;
  PRE_EXPR_NAME (result) = name;
  result_id = lookup_expression_id (result);
  if (result_id != 0)
    {
      pool_free (pre_expr_pool, result);
      result = expression_for_id (result_id);
      return result;
    }
  get_or_alloc_expression_id (result);
  return result;
}

static bool in_fre = false;

/* An unordered bitmap set.  One bitmap tracks values, the other,
   expressions.  */
typedef struct bitmap_set
{
  bitmap expressions;
  bitmap values;
} *bitmap_set_t;

#define FOR_EACH_EXPR_ID_IN_SET(set, id, bi)		\
  EXECUTE_IF_SET_IN_BITMAP((set)->expressions, 0, (id), (bi))

#define FOR_EACH_VALUE_ID_IN_SET(set, id, bi)		\
  EXECUTE_IF_SET_IN_BITMAP((set)->values, 0, (id), (bi))

/* Mapping from value id to expressions with that value_id.  */
DEF_VEC_P (bitmap_set_t);
DEF_VEC_ALLOC_P (bitmap_set_t, heap);
static VEC(bitmap_set_t, heap) *value_expressions;

/* Sets that we need to keep track of.  */
typedef struct bb_bitmap_sets
{
  /* The EXP_GEN set, which represents expressions/values generated in
     a basic block.  */
  bitmap_set_t exp_gen;

  /* The PHI_GEN set, which represents PHI results generated in a
     basic block.  */
  bitmap_set_t phi_gen;

  /* The TMP_GEN set, which represents results/temporaries generated
     in a basic block. IE the LHS of an expression.  */
  bitmap_set_t tmp_gen;

  /* The AVAIL_OUT set, which represents which values are available in
     a given basic block.  */
  bitmap_set_t avail_out;

  /* The ANTIC_IN set, which represents which values are anticipatable
     in a given basic block.  */
  bitmap_set_t antic_in;

  /* The PA_IN set, which represents which values are
     partially anticipatable in a given basic block.  */
  bitmap_set_t pa_in;

  /* The NEW_SETS set, which is used during insertion to augment the
     AVAIL_OUT set of blocks with the new insertions performed during
     the current iteration.  */
  bitmap_set_t new_sets;

  /* A cache for value_dies_in_block_x.  */
  bitmap expr_dies;

  /* True if we have visited this block during ANTIC calculation.  */
  unsigned int visited:1;

  /* True we have deferred processing this block during ANTIC
     calculation until its successor is processed.  */
  unsigned int deferred : 1;
} *bb_value_sets_t;

#define EXP_GEN(BB)	((bb_value_sets_t) ((BB)->aux))->exp_gen
#define PHI_GEN(BB)	((bb_value_sets_t) ((BB)->aux))->phi_gen
#define TMP_GEN(BB)	((bb_value_sets_t) ((BB)->aux))->tmp_gen
#define AVAIL_OUT(BB)	((bb_value_sets_t) ((BB)->aux))->avail_out
#define ANTIC_IN(BB)	((bb_value_sets_t) ((BB)->aux))->antic_in
#define PA_IN(BB)	((bb_value_sets_t) ((BB)->aux))->pa_in
#define NEW_SETS(BB)	((bb_value_sets_t) ((BB)->aux))->new_sets
#define EXPR_DIES(BB)	((bb_value_sets_t) ((BB)->aux))->expr_dies
#define BB_VISITED(BB)	((bb_value_sets_t) ((BB)->aux))->visited
#define BB_DEFERRED(BB) ((bb_value_sets_t) ((BB)->aux))->deferred


/* Basic block list in postorder.  */
static int *postorder;

/* This structure is used to keep track of statistics on what
   optimization PRE was able to perform.  */
static struct
{
  /* The number of RHS computations eliminated by PRE.  */
  int eliminations;

  /* The number of new expressions/temporaries generated by PRE.  */
  int insertions;

  /* The number of inserts found due to partial anticipation  */
  int pa_insert;

  /* The number of new PHI nodes added by PRE.  */
  int phis;

  /* The number of values found constant.  */
  int constified;

} pre_stats;

static bool do_partial_partial;
static pre_expr bitmap_find_leader (bitmap_set_t, unsigned int, gimple);
static void bitmap_value_insert_into_set (bitmap_set_t, pre_expr);
static void bitmap_value_replace_in_set (bitmap_set_t, pre_expr);
static void bitmap_set_copy (bitmap_set_t, bitmap_set_t);
static bool bitmap_set_contains_value (bitmap_set_t, unsigned int);
static void bitmap_insert_into_set (bitmap_set_t, pre_expr);
static void bitmap_insert_into_set_1 (bitmap_set_t, pre_expr, bool);
static bitmap_set_t bitmap_set_new (void);
static tree create_expression_by_pieces (basic_block, pre_expr, gimple_seq *,
					 gimple, tree);
static tree find_or_generate_expression (basic_block, pre_expr, gimple_seq *,
					 gimple);
static unsigned int get_expr_value_id (pre_expr);

/* We can add and remove elements and entries to and from sets
   and hash tables, so we use alloc pools for them.  */

static alloc_pool bitmap_set_pool;
static bitmap_obstack grand_bitmap_obstack;

/* To avoid adding 300 temporary variables when we only need one, we
   only create one temporary variable, on demand, and build ssa names
   off that.  We do have to change the variable if the types don't
   match the current variable's type.  */
static tree pretemp;
static tree storetemp;
static tree prephitemp;

/* Set of blocks with statements that have had its EH information
   cleaned up.  */
static bitmap need_eh_cleanup;

/* Which expressions have been seen during a given phi translation.  */
static bitmap seen_during_translate;

/* The phi_translate_table caches phi translations for a given
   expression and predecessor.  */

static htab_t phi_translate_table;

/* A three tuple {e, pred, v} used to cache phi translations in the
   phi_translate_table.  */

typedef struct expr_pred_trans_d
{
  /* The expression.  */
  pre_expr e;

  /* The predecessor block along which we translated the expression.  */
  basic_block pred;

  /* The value that resulted from the translation.  */
  pre_expr v;

  /* The hashcode for the expression, pred pair. This is cached for
     speed reasons.  */
  hashval_t hashcode;
} *expr_pred_trans_t;
typedef const struct expr_pred_trans_d *const_expr_pred_trans_t;

/* Return the hash value for a phi translation table entry.  */

static hashval_t
expr_pred_trans_hash (const void *p)
{
  const_expr_pred_trans_t const ve = (const_expr_pred_trans_t) p;
  return ve->hashcode;
}

/* Return true if two phi translation table entries are the same.
   P1 and P2 should point to the expr_pred_trans_t's to be compared.*/

static int
expr_pred_trans_eq (const void *p1, const void *p2)
{
  const_expr_pred_trans_t const ve1 = (const_expr_pred_trans_t) p1;
  const_expr_pred_trans_t const ve2 = (const_expr_pred_trans_t) p2;
  basic_block b1 = ve1->pred;
  basic_block b2 = ve2->pred;

  /* If they are not translations for the same basic block, they can't
     be equal.  */
  if (b1 != b2)
    return false;
  return pre_expr_eq (ve1->e, ve2->e);
}

/* Search in the phi translation table for the translation of
   expression E in basic block PRED.
   Return the translated value, if found, NULL otherwise.  */

static inline pre_expr
phi_trans_lookup (pre_expr e, basic_block pred)
{
  void **slot;
  struct expr_pred_trans_d ept;

  ept.e = e;
  ept.pred = pred;
  ept.hashcode = iterative_hash_hashval_t (pre_expr_hash (e), pred->index);
  slot = htab_find_slot_with_hash (phi_translate_table, &ept, ept.hashcode,
				   NO_INSERT);
  if (!slot)
    return NULL;
  else
    return ((expr_pred_trans_t) *slot)->v;
}


/* Add the tuple mapping from {expression E, basic block PRED} to
   value V, to the phi translation table.  */

static inline void
phi_trans_add (pre_expr e, pre_expr v, basic_block pred)
{
  void **slot;
  expr_pred_trans_t new_pair = XNEW (struct expr_pred_trans_d);
  new_pair->e = e;
  new_pair->pred = pred;
  new_pair->v = v;
  new_pair->hashcode = iterative_hash_hashval_t (pre_expr_hash (e),
						 pred->index);

  slot = htab_find_slot_with_hash (phi_translate_table, new_pair,
				   new_pair->hashcode, INSERT);
  if (*slot)
    free (*slot);
  *slot = (void *) new_pair;
}


/* Add expression E to the expression set of value id V.  */

void
add_to_value (unsigned int v, pre_expr e)
{
  bitmap_set_t set;

  gcc_assert (get_expr_value_id (e) == v);

  if (v >= VEC_length (bitmap_set_t, value_expressions))
    {
      VEC_safe_grow_cleared (bitmap_set_t, heap, value_expressions,
			     v + 1);
    }

  set = VEC_index (bitmap_set_t, value_expressions, v);
  if (!set)
    {
      set = bitmap_set_new ();
      VEC_replace (bitmap_set_t, value_expressions, v, set);
    }

  bitmap_insert_into_set_1 (set, e, true);
}

/* Create a new bitmap set and return it.  */

static bitmap_set_t
bitmap_set_new (void)
{
  bitmap_set_t ret = (bitmap_set_t) pool_alloc (bitmap_set_pool);
  ret->expressions = BITMAP_ALLOC (&grand_bitmap_obstack);
  ret->values = BITMAP_ALLOC (&grand_bitmap_obstack);
  return ret;
}

/* Return the value id for a PRE expression EXPR.  */

static unsigned int
get_expr_value_id (pre_expr expr)
{
  switch (expr->kind)
    {
    case CONSTANT:
      {
	unsigned int id;
	id = get_constant_value_id (PRE_EXPR_CONSTANT (expr));
	if (id == 0)
	  {
	    id = get_or_alloc_constant_value_id (PRE_EXPR_CONSTANT (expr));
	    add_to_value (id, expr);
	  }
	return id;
      }
    case NAME:
      return VN_INFO (PRE_EXPR_NAME (expr))->value_id;
    case NARY:
      return PRE_EXPR_NARY (expr)->value_id;
    case REFERENCE:
      return PRE_EXPR_REFERENCE (expr)->value_id;
    default:
      gcc_unreachable ();
    }
}

/* Remove an expression EXPR from a bitmapped set.  */

static void
bitmap_remove_from_set (bitmap_set_t set, pre_expr expr)
{
  unsigned int val  = get_expr_value_id (expr);
  if (!value_id_constant_p (val))
    {
      bitmap_clear_bit (set->values, val);
      bitmap_clear_bit (set->expressions, get_expression_id (expr));
    }
}

static void
bitmap_insert_into_set_1 (bitmap_set_t set, pre_expr expr,
			  bool allow_constants)
{
  unsigned int val  = get_expr_value_id (expr);
  if (allow_constants || !value_id_constant_p (val))
    {
      /* We specifically expect this and only this function to be able to
	 insert constants into a set.  */
      bitmap_set_bit (set->values, val);
      bitmap_set_bit (set->expressions, get_or_alloc_expression_id (expr));
    }
}

/* Insert an expression EXPR into a bitmapped set.  */

static void
bitmap_insert_into_set (bitmap_set_t set, pre_expr expr)
{
  bitmap_insert_into_set_1 (set, expr, false);
}

/* Copy a bitmapped set ORIG, into bitmapped set DEST.  */

static void
bitmap_set_copy (bitmap_set_t dest, bitmap_set_t orig)
{
  bitmap_copy (dest->expressions, orig->expressions);
  bitmap_copy (dest->values, orig->values);
}


/* Free memory used up by SET.  */
static void
bitmap_set_free (bitmap_set_t set)
{
  BITMAP_FREE (set->expressions);
  BITMAP_FREE (set->values);
}


/* Generate an topological-ordered array of bitmap set SET.  */

static VEC(pre_expr, heap) *
sorted_array_from_bitmap_set (bitmap_set_t set)
{
  unsigned int i, j;
  bitmap_iterator bi, bj;
  VEC(pre_expr, heap) *result = NULL;

  FOR_EACH_VALUE_ID_IN_SET (set, i, bi)
    {
      /* The number of expressions having a given value is usually
	 relatively small.  Thus, rather than making a vector of all
	 the expressions and sorting it by value-id, we walk the values
	 and check in the reverse mapping that tells us what expressions
	 have a given value, to filter those in our set.  As a result,
	 the expressions are inserted in value-id order, which means
	 topological order.

	 If this is somehow a significant lose for some cases, we can
	 choose which set to walk based on the set size.  */
      bitmap_set_t exprset = VEC_index (bitmap_set_t, value_expressions, i);
      FOR_EACH_EXPR_ID_IN_SET (exprset, j, bj)
	{
	  if (bitmap_bit_p (set->expressions, j))
	    VEC_safe_push (pre_expr, heap, result, expression_for_id (j));
        }
    }

  return result;
}

/* Perform bitmapped set operation DEST &= ORIG.  */

static void
bitmap_set_and (bitmap_set_t dest, bitmap_set_t orig)
{
  bitmap_iterator bi;
  unsigned int i;

  if (dest != orig)
    {
      bitmap temp = BITMAP_ALLOC (&grand_bitmap_obstack);

      bitmap_and_into (dest->values, orig->values);
      bitmap_copy (temp, dest->expressions);
      EXECUTE_IF_SET_IN_BITMAP (temp, 0, i, bi)
	{
	  pre_expr expr = expression_for_id (i);
	  unsigned int value_id = get_expr_value_id (expr);
	  if (!bitmap_bit_p (dest->values, value_id))
	    bitmap_clear_bit (dest->expressions, i);
	}
      BITMAP_FREE (temp);
    }
}

/* Subtract all values and expressions contained in ORIG from DEST.  */

static bitmap_set_t
bitmap_set_subtract (bitmap_set_t dest, bitmap_set_t orig)
{
  bitmap_set_t result = bitmap_set_new ();
  bitmap_iterator bi;
  unsigned int i;

  bitmap_and_compl (result->expressions, dest->expressions,
		    orig->expressions);

  FOR_EACH_EXPR_ID_IN_SET (result, i, bi)
    {
      pre_expr expr = expression_for_id (i);
      unsigned int value_id = get_expr_value_id (expr);
      bitmap_set_bit (result->values, value_id);
    }

  return result;
}

/* Subtract all the values in bitmap set B from bitmap set A.  */

static void
bitmap_set_subtract_values (bitmap_set_t a, bitmap_set_t b)
{
  unsigned int i;
  bitmap_iterator bi;
  bitmap temp = BITMAP_ALLOC (&grand_bitmap_obstack);

  bitmap_copy (temp, a->expressions);
  EXECUTE_IF_SET_IN_BITMAP (temp, 0, i, bi)
    {
      pre_expr expr = expression_for_id (i);
      if (bitmap_set_contains_value (b, get_expr_value_id (expr)))
	bitmap_remove_from_set (a, expr);
    }
  BITMAP_FREE (temp);
}


/* Return true if bitmapped set SET contains the value VALUE_ID.  */

static bool
bitmap_set_contains_value (bitmap_set_t set, unsigned int value_id)
{
  if (value_id_constant_p (value_id))
    return true;

  if (!set || bitmap_empty_p (set->expressions))
    return false;

  return bitmap_bit_p (set->values, value_id);
}

static inline bool
bitmap_set_contains_expr (bitmap_set_t set, const pre_expr expr)
{
  return bitmap_bit_p (set->expressions, get_expression_id (expr));
}

/* Replace an instance of value LOOKFOR with expression EXPR in SET.  */

static void
bitmap_set_replace_value (bitmap_set_t set, unsigned int lookfor,
			  const pre_expr expr)
{
  bitmap_set_t exprset;
  unsigned int i;
  bitmap_iterator bi;

  if (value_id_constant_p (lookfor))
    return;

  if (!bitmap_set_contains_value (set, lookfor))
    return;

  /* The number of expressions having a given value is usually
     significantly less than the total number of expressions in SET.
     Thus, rather than check, for each expression in SET, whether it
     has the value LOOKFOR, we walk the reverse mapping that tells us
     what expressions have a given value, and see if any of those
     expressions are in our set.  For large testcases, this is about
     5-10x faster than walking the bitmap.  If this is somehow a
     significant lose for some cases, we can choose which set to walk
     based on the set size.  */
  exprset = VEC_index (bitmap_set_t, value_expressions, lookfor);
  FOR_EACH_EXPR_ID_IN_SET (exprset, i, bi)
    {
      if (bitmap_bit_p (set->expressions, i))
	{
	  bitmap_clear_bit (set->expressions, i);
	  bitmap_set_bit (set->expressions, get_expression_id (expr));
	  return;
	}
    }
}

/* Return true if two bitmap sets are equal.  */

static bool
bitmap_set_equal (bitmap_set_t a, bitmap_set_t b)
{
  return bitmap_equal_p (a->values, b->values);
}

/* Replace an instance of EXPR's VALUE with EXPR in SET if it exists,
   and add it otherwise.  */

static void
bitmap_value_replace_in_set (bitmap_set_t set, pre_expr expr)
{
  unsigned int val = get_expr_value_id (expr);

  if (bitmap_set_contains_value (set, val))
    bitmap_set_replace_value (set, val, expr);
  else
    bitmap_insert_into_set (set, expr);
}

/* Insert EXPR into SET if EXPR's value is not already present in
   SET.  */

static void
bitmap_value_insert_into_set (bitmap_set_t set, pre_expr expr)
{
  unsigned int val = get_expr_value_id (expr);

  if (value_id_constant_p (val))
    return;

  if (!bitmap_set_contains_value (set, val))
    bitmap_insert_into_set (set, expr);
}

/* Print out EXPR to outfile.  */

static void
print_pre_expr (FILE *outfile, const pre_expr expr)
{
  switch (expr->kind)
    {
    case CONSTANT:
      print_generic_expr (outfile, PRE_EXPR_CONSTANT (expr), 0);
      break;
    case NAME:
      print_generic_expr (outfile, PRE_EXPR_NAME (expr), 0);
      break;
    case NARY:
      {
	unsigned int i;
	vn_nary_op_t nary = PRE_EXPR_NARY (expr);
	fprintf (outfile, "{%s,", tree_code_name [nary->opcode]);
	for (i = 0; i < nary->length; i++)
	  {
	    print_generic_expr (outfile, nary->op[i], 0);
	    if (i != (unsigned) nary->length - 1)
	      fprintf (outfile, ",");
	  }
	fprintf (outfile, "}");
      }
      break;

    case REFERENCE:
      {
	vn_reference_op_t vro;
	unsigned int i;
	vn_reference_t ref = PRE_EXPR_REFERENCE (expr);
	fprintf (outfile, "{");
	for (i = 0;
	     VEC_iterate (vn_reference_op_s, ref->operands, i, vro);
	     i++)
	  {
	    bool closebrace = false;
	    if (vro->opcode != SSA_NAME
		&& TREE_CODE_CLASS (vro->opcode) != tcc_declaration)
	      {
		fprintf (outfile, "%s", tree_code_name [vro->opcode]);
		if (vro->op0)
		  {
		    fprintf (outfile, "<");
		    closebrace = true;
		  }
	      }
	    if (vro->op0)
	      {
		print_generic_expr (outfile, vro->op0, 0);
		if (vro->op1)
		  {
		    fprintf (outfile, ",");
		    print_generic_expr (outfile, vro->op1, 0);
		  }
		if (vro->op2)
		  {
		    fprintf (outfile, ",");
		    print_generic_expr (outfile, vro->op2, 0);
		  }
	      }
	    if (closebrace)
		fprintf (outfile, ">");
	    if (i != VEC_length (vn_reference_op_s, ref->operands) - 1)
	      fprintf (outfile, ",");
	  }
	fprintf (outfile, "}");
	if (ref->vuse)
	  {
	    fprintf (outfile, "@");
	    print_generic_expr (outfile, ref->vuse, 0);
	  }
      }
      break;
    }
}
void debug_pre_expr (pre_expr);

/* Like print_pre_expr but always prints to stderr.  */
void
debug_pre_expr (pre_expr e)
{
  print_pre_expr (stderr, e);
  fprintf (stderr, "\n");
}

/* Print out SET to OUTFILE.  */

static void
print_bitmap_set (FILE *outfile, bitmap_set_t set,
		  const char *setname, int blockindex)
{
  fprintf (outfile, "%s[%d] := { ", setname, blockindex);
  if (set)
    {
      bool first = true;
      unsigned i;
      bitmap_iterator bi;

      FOR_EACH_EXPR_ID_IN_SET (set, i, bi)
	{
	  const pre_expr expr = expression_for_id (i);

	  if (!first)
	    fprintf (outfile, ", ");
	  first = false;
	  print_pre_expr (outfile, expr);

	  fprintf (outfile, " (%04d)", get_expr_value_id (expr));
	}
    }
  fprintf (outfile, " }\n");
}

void debug_bitmap_set (bitmap_set_t);

void
debug_bitmap_set (bitmap_set_t set)
{
  print_bitmap_set (stderr, set, "debug", 0);
}

/* Print out the expressions that have VAL to OUTFILE.  */

void
print_value_expressions (FILE *outfile, unsigned int val)
{
  bitmap_set_t set = VEC_index (bitmap_set_t, value_expressions, val);
  if (set)
    {
      char s[10];
      sprintf (s, "%04d", val);
      print_bitmap_set (outfile, set, s, 0);
    }
}


void
debug_value_expressions (unsigned int val)
{
  print_value_expressions (stderr, val);
}

/* Given a CONSTANT, allocate a new CONSTANT type PRE_EXPR to
   represent it.  */

static pre_expr
get_or_alloc_expr_for_constant (tree constant)
{
  unsigned int result_id;
  unsigned int value_id;
  pre_expr newexpr = (pre_expr) pool_alloc (pre_expr_pool);
  newexpr->kind = CONSTANT;
  PRE_EXPR_CONSTANT (newexpr) = constant;
  result_id = lookup_expression_id (newexpr);
  if (result_id != 0)
    {
      pool_free (pre_expr_pool, newexpr);
      newexpr = expression_for_id (result_id);
      return newexpr;
    }
  value_id = get_or_alloc_constant_value_id (constant);
  get_or_alloc_expression_id (newexpr);
  add_to_value (value_id, newexpr);
  return newexpr;
}

/* Given a value id V, find the actual tree representing the constant
   value if there is one, and return it. Return NULL if we can't find
   a constant.  */

static tree
get_constant_for_value_id (unsigned int v)
{
  if (value_id_constant_p (v))
    {
      unsigned int i;
      bitmap_iterator bi;
      bitmap_set_t exprset = VEC_index (bitmap_set_t, value_expressions, v);

      FOR_EACH_EXPR_ID_IN_SET (exprset, i, bi)
	{
	  pre_expr expr = expression_for_id (i);
	  if (expr->kind == CONSTANT)
	    return PRE_EXPR_CONSTANT (expr);
	}
    }
  return NULL;
}

/* Get or allocate a pre_expr for a piece of GIMPLE, and return it.
   Currently only supports constants and SSA_NAMES.  */
static pre_expr
get_or_alloc_expr_for (tree t)
{
  if (TREE_CODE (t) == SSA_NAME)
    return get_or_alloc_expr_for_name (t);
  else if (is_gimple_min_invariant (t))
    return get_or_alloc_expr_for_constant (t);
  else
    {
      /* More complex expressions can result from SCCVN expression
	 simplification that inserts values for them.  As they all
	 do not have VOPs the get handled by the nary ops struct.  */
      vn_nary_op_t result;
      unsigned int result_id;
      vn_nary_op_lookup (t, &result);
      if (result != NULL)
	{
	  pre_expr e = (pre_expr) pool_alloc (pre_expr_pool);
	  e->kind = NARY;
	  PRE_EXPR_NARY (e) = result;
	  result_id = lookup_expression_id (e);
	  if (result_id != 0)
	    {
	      pool_free (pre_expr_pool, e);
	      e = expression_for_id (result_id);
	      return e;
	    }
	  alloc_expression_id (e);
	  return e;
	}
    }
  return NULL;
}

/* Return the folded version of T if T, when folded, is a gimple
   min_invariant.  Otherwise, return T.  */

static pre_expr
fully_constant_expression (pre_expr e)
{
  switch (e->kind)
    {
    case CONSTANT:
      return e;
    case NARY:
      {
	vn_nary_op_t nary = PRE_EXPR_NARY (e);
	switch (TREE_CODE_CLASS (nary->opcode))
	  {
	  case tcc_expression:
	    if (nary->opcode == TRUTH_NOT_EXPR)
	      goto do_unary;
	    if (nary->opcode != TRUTH_AND_EXPR
		&& nary->opcode != TRUTH_OR_EXPR
		&& nary->opcode != TRUTH_XOR_EXPR)
	      return e;
	    /* Fallthrough.  */
	  case tcc_binary:
	  case tcc_comparison:
	    {
	      /* We have to go from trees to pre exprs to value ids to
		 constants.  */
	      tree naryop0 = nary->op[0];
	      tree naryop1 = nary->op[1];
	      tree result;
	      if (!is_gimple_min_invariant (naryop0))
		{
		  pre_expr rep0 = get_or_alloc_expr_for (naryop0);
		  unsigned int vrep0 = get_expr_value_id (rep0);
		  tree const0 = get_constant_for_value_id (vrep0);
		  if (const0)
		    naryop0 = fold_convert (TREE_TYPE (naryop0), const0);
		}
	      if (!is_gimple_min_invariant (naryop1))
		{
		  pre_expr rep1 = get_or_alloc_expr_for (naryop1);
		  unsigned int vrep1 = get_expr_value_id (rep1);
		  tree const1 = get_constant_for_value_id (vrep1);
		  if (const1)
		    naryop1 = fold_convert (TREE_TYPE (naryop1), const1);
		}
	      result = fold_binary (nary->opcode, nary->type,
				    naryop0, naryop1);
	      if (result && is_gimple_min_invariant (result))
		return get_or_alloc_expr_for_constant (result);
	      /* We might have simplified the expression to a
		 SSA_NAME for example from x_1 * 1.  But we cannot
		 insert a PHI for x_1 unconditionally as x_1 might
		 not be available readily.  */
	      return e;
	    }
	  case tcc_reference:
	    if (nary->opcode != REALPART_EXPR
		&& nary->opcode != IMAGPART_EXPR
		&& nary->opcode != VIEW_CONVERT_EXPR)
	      return e;
	    /* Fallthrough.  */
	  case tcc_unary:
do_unary:
	    {
	      /* We have to go from trees to pre exprs to value ids to
		 constants.  */
	      tree naryop0 = nary->op[0];
	      tree const0, result;
	      if (is_gimple_min_invariant (naryop0))
		const0 = naryop0;
	      else
		{
		  pre_expr rep0 = get_or_alloc_expr_for (naryop0);
		  unsigned int vrep0 = get_expr_value_id (rep0);
		  const0 = get_constant_for_value_id (vrep0);
		}
	      result = NULL;
	      if (const0)
		{
		  tree type1 = TREE_TYPE (nary->op[0]);
		  const0 = fold_convert (type1, const0);
		  result = fold_unary (nary->opcode, nary->type, const0);
		}
	      if (result && is_gimple_min_invariant (result))
		return get_or_alloc_expr_for_constant (result);
	      return e;
	    }
	  default:
	    return e;
	  }
      }
    case REFERENCE:
      {
	vn_reference_t ref = PRE_EXPR_REFERENCE (e);
	VEC (vn_reference_op_s, heap) *operands = ref->operands;
	vn_reference_op_t op;

	/* Try to simplify the translated expression if it is
	   a call to a builtin function with at most two arguments.  */
	op = VEC_index (vn_reference_op_s, operands, 0);
	if (op->opcode == CALL_EXPR
	    && TREE_CODE (op->op0) == ADDR_EXPR
	    && TREE_CODE (TREE_OPERAND (op->op0, 0)) == FUNCTION_DECL
	    && DECL_BUILT_IN (TREE_OPERAND (op->op0, 0))
	    && VEC_length (vn_reference_op_s, operands) >= 2
	    && VEC_length (vn_reference_op_s, operands) <= 3)
	  {
	    vn_reference_op_t arg0, arg1 = NULL;
	    bool anyconst = false;
	    arg0 = VEC_index (vn_reference_op_s, operands, 1);
	    if (VEC_length (vn_reference_op_s, operands) > 2)
	      arg1 = VEC_index (vn_reference_op_s, operands, 2);
	    if (TREE_CODE_CLASS (arg0->opcode) == tcc_constant
		|| (arg0->opcode == ADDR_EXPR
		    && is_gimple_min_invariant (arg0->op0)))
	      anyconst = true;
	    if (arg1
		&& (TREE_CODE_CLASS (arg1->opcode) == tcc_constant
		    || (arg1->opcode == ADDR_EXPR
			&& is_gimple_min_invariant (arg1->op0))))
	      anyconst = true;
	    if (anyconst)
	      {
		tree folded = build_call_expr (TREE_OPERAND (op->op0, 0),
					       arg1 ? 2 : 1,
					       arg0->op0,
					       arg1 ? arg1->op0 : NULL);
		if (folded
		    && TREE_CODE (folded) == NOP_EXPR)
		  folded = TREE_OPERAND (folded, 0);
		if (folded
		    && is_gimple_min_invariant (folded))
		  return get_or_alloc_expr_for_constant (folded);
	      }
	  }
	  return e;
	}
    default:
      return e;
    }
  return e;
}

/* Translate the VUSE backwards through phi nodes in PHIBLOCK, so that
   it has the value it would have in BLOCK.  Set *SAME_VALID to true
   in case the new vuse doesn't change the value id of the OPERANDS.  */

static tree
translate_vuse_through_block (VEC (vn_reference_op_s, heap) *operands,
			      alias_set_type set, tree type, tree vuse,
			      basic_block phiblock,
			      basic_block block, bool *same_valid)
{
  gimple phi = SSA_NAME_DEF_STMT (vuse);
  ao_ref ref;
  edge e = NULL;
  bool use_oracle;

  *same_valid = true;

  if (gimple_bb (phi) != phiblock)
    return vuse;

  use_oracle = ao_ref_init_from_vn_reference (&ref, set, type, operands);

  /* Use the alias-oracle to find either the PHI node in this block,
     the first VUSE used in this block that is equivalent to vuse or
     the first VUSE which definition in this block kills the value.  */
  if (gimple_code (phi) == GIMPLE_PHI)
    e = find_edge (block, phiblock);
  else if (use_oracle)
    while (!stmt_may_clobber_ref_p_1 (phi, &ref))
      {
	vuse = gimple_vuse (phi);
	phi = SSA_NAME_DEF_STMT (vuse);
	if (gimple_bb (phi) != phiblock)
	  return vuse;
	if (gimple_code (phi) == GIMPLE_PHI)
	  {
	    e = find_edge (block, phiblock);
	    break;
	  }
      }
  else
    return NULL_TREE;

  if (e)
    {
      if (use_oracle)
	{
	  bitmap visited = NULL;
	  /* Try to find a vuse that dominates this phi node by skipping
	     non-clobbering statements.  */
	  vuse = get_continuation_for_phi (phi, &ref, &visited);
	  if (visited)
	    BITMAP_FREE (visited);
	}
      else
	vuse = NULL_TREE;
      if (!vuse)
	{
	  /* If we didn't find any, the value ID can't stay the same,
	     but return the translated vuse.  */
	  *same_valid = false;
	  vuse = PHI_ARG_DEF (phi, e->dest_idx);
	}
      /* ??? We would like to return vuse here as this is the canonical
         upmost vdef that this reference is associated with.  But during
	 insertion of the references into the hash tables we only ever
	 directly insert with their direct gimple_vuse, hence returning
	 something else would make us not find the other expression.  */
      return PHI_ARG_DEF (phi, e->dest_idx);
    }

  return NULL_TREE;
}

/* Like bitmap_find_leader, but checks for the value existing in SET1 *or*
   SET2.  This is used to avoid making a set consisting of the union
   of PA_IN and ANTIC_IN during insert.  */

static inline pre_expr
find_leader_in_sets (unsigned int val, bitmap_set_t set1, bitmap_set_t set2)
{
  pre_expr result;

  result = bitmap_find_leader (set1, val, NULL);
  if (!result && set2)
    result = bitmap_find_leader (set2, val, NULL);
  return result;
}

/* Get the tree type for our PRE expression e.  */

static tree
get_expr_type (const pre_expr e)
{
  switch (e->kind)
    {
    case NAME:
      return TREE_TYPE (PRE_EXPR_NAME (e));
    case CONSTANT:
      return TREE_TYPE (PRE_EXPR_CONSTANT (e));
    case REFERENCE:
      return PRE_EXPR_REFERENCE (e)->type;
    case NARY:
      return PRE_EXPR_NARY (e)->type;
    }
  gcc_unreachable();
}

/* Get a representative SSA_NAME for a given expression.
   Since all of our sub-expressions are treated as values, we require
   them to be SSA_NAME's for simplicity.
   Prior versions of GVNPRE used to use "value handles" here, so that
   an expression would be VH.11 + VH.10 instead of d_3 + e_6.  In
   either case, the operands are really values (IE we do not expect
   them to be usable without finding leaders).  */

static tree
get_representative_for (const pre_expr e)
{
  tree exprtype;
  tree name;
  unsigned int value_id = get_expr_value_id (e);

  switch (e->kind)
    {
    case NAME:
      return PRE_EXPR_NAME (e);
    case CONSTANT:
      return PRE_EXPR_CONSTANT (e);
    case NARY:
    case REFERENCE:
      {
	/* Go through all of the expressions representing this value
	   and pick out an SSA_NAME.  */
	unsigned int i;
	bitmap_iterator bi;
	bitmap_set_t exprs = VEC_index (bitmap_set_t, value_expressions,
					value_id);
	FOR_EACH_EXPR_ID_IN_SET (exprs, i, bi)
	  {
	    pre_expr rep = expression_for_id (i);
	    if (rep->kind == NAME)
	      return PRE_EXPR_NAME (rep);
	  }
      }
      break;
    }
  /* If we reached here we couldn't find an SSA_NAME.  This can
     happen when we've discovered a value that has never appeared in
     the program as set to an SSA_NAME, most likely as the result of
     phi translation.  */
  if (dump_file)
    {
      fprintf (dump_file,
	       "Could not find SSA_NAME representative for expression:");
      print_pre_expr (dump_file, e);
      fprintf (dump_file, "\n");
    }

  exprtype = get_expr_type (e);

  /* Build and insert the assignment of the end result to the temporary
     that we will return.  */
  if (!pretemp || exprtype != TREE_TYPE (pretemp))
    {
      pretemp = create_tmp_var (exprtype, "pretmp");
      get_var_ann (pretemp);
    }

  name = make_ssa_name (pretemp, gimple_build_nop ());
  VN_INFO_GET (name)->value_id = value_id;
  if (e->kind == CONSTANT)
    VN_INFO (name)->valnum = PRE_EXPR_CONSTANT (e);
  else
    VN_INFO (name)->valnum = name;

  add_to_value (value_id, get_or_alloc_expr_for_name (name));
  if (dump_file)
    {
      fprintf (dump_file, "Created SSA_NAME representative ");
      print_generic_expr (dump_file, name, 0);
      fprintf (dump_file, " for expression:");
      print_pre_expr (dump_file, e);
      fprintf (dump_file, "\n");
    }

  return name;
}




/* Translate EXPR using phis in PHIBLOCK, so that it has the values of
   the phis in PRED.  SEEN is a bitmap saying which expression we have
   translated since we started translation of the toplevel expression.
   Return NULL if we can't find a leader for each part of the
   translated expression.  */

static pre_expr
phi_translate_1 (pre_expr expr, bitmap_set_t set1, bitmap_set_t set2,
		 basic_block pred, basic_block phiblock, bitmap seen)
{
  pre_expr oldexpr = expr;
  pre_expr phitrans;

  if (!expr)
    return NULL;

  if (value_id_constant_p (get_expr_value_id (expr)))
    return expr;

  phitrans = phi_trans_lookup (expr, pred);
  if (phitrans)
    return phitrans;

  /* Prevent cycles when we have recursively dependent leaders.  This
     can only happen when phi translating the maximal set.  */
  if (seen)
    {
      unsigned int expr_id = get_expression_id (expr);
      if (bitmap_bit_p (seen, expr_id))
	return NULL;
      bitmap_set_bit (seen, expr_id);
    }

  switch (expr->kind)
    {
      /* Constants contain no values that need translation.  */
    case CONSTANT:
      return expr;

    case NARY:
      {
	unsigned int i;
	bool changed = false;
	vn_nary_op_t nary = PRE_EXPR_NARY (expr);
	struct vn_nary_op_s newnary;
	/* The NARY structure is only guaranteed to have been
	   allocated to the nary->length operands.  */
	memcpy (&newnary, nary, (sizeof (struct vn_nary_op_s)
				 - sizeof (tree) * (4 - nary->length)));

	for (i = 0; i < newnary.length; i++)
	  {
	    if (TREE_CODE (newnary.op[i]) != SSA_NAME)
	      continue;
	    else
	      {
                pre_expr leader, result;
                bitmap temp = BITMAP_ALLOC (&grand_bitmap_obstack);
		unsigned int op_val_id = VN_INFO (newnary.op[i])->value_id;

                bitmap_copy (temp, seen);
		leader = find_leader_in_sets (op_val_id, set1, set2);
                result = phi_translate_1 (leader, set1, set2,
						   pred, phiblock, seen);
                bitmap_copy (seen, temp);
                BITMAP_FREE (temp);

		if (result && result != leader)
		  {
		    tree name = get_representative_for (result);
		    if (!name)
		      return NULL;
		    newnary.op[i] = name;
		  }
		else if (!result)
		  return NULL;

		changed |= newnary.op[i] != nary->op[i];
	      }
	  }
	if (changed)
	  {
	    pre_expr constant;

	    tree result = vn_nary_op_lookup_pieces (newnary.length,
						    newnary.opcode,
						    newnary.type,
						    newnary.op[0],
						    newnary.op[1],
						    newnary.op[2],
						    newnary.op[3],
						    &nary);
	    unsigned int new_val_id;

	    expr = (pre_expr) pool_alloc (pre_expr_pool);
	    expr->kind = NARY;
	    expr->id = 0;
	    if (result && is_gimple_min_invariant (result))
	      return get_or_alloc_expr_for_constant (result);


	    if (nary)
	      {
		PRE_EXPR_NARY (expr) = nary;
		constant = fully_constant_expression (expr);
		if (constant != expr)
		  return constant;

		new_val_id = nary->value_id;
		get_or_alloc_expression_id (expr);
	      }
	    else
	      {
		new_val_id = get_next_value_id ();
		VEC_safe_grow_cleared (bitmap_set_t, heap,
				       value_expressions,
				       get_max_value_id() + 1);
		nary = vn_nary_op_insert_pieces (newnary.length,
						 newnary.opcode,
						 newnary.type,
						 newnary.op[0],
						 newnary.op[1],
						 newnary.op[2],
						 newnary.op[3],
						 result, new_val_id);
		PRE_EXPR_NARY (expr) = nary;
		constant = fully_constant_expression (expr);
		if (constant != expr)
		  return constant;
		get_or_alloc_expression_id (expr);
	      }
	    add_to_value (new_val_id, expr);
	  }
	phi_trans_add (oldexpr, expr, pred);
	return expr;
      }
      break;

    case REFERENCE:
      {
	vn_reference_t ref = PRE_EXPR_REFERENCE (expr);
	VEC (vn_reference_op_s, heap) *operands = ref->operands;
	tree vuse = ref->vuse;
	tree newvuse = vuse;
	VEC (vn_reference_op_s, heap) *newoperands = NULL;
	bool changed = false, same_valid = true;
	unsigned int i, j;
	vn_reference_op_t operand;
	vn_reference_t newref;

	for (i = 0, j = 0;
	     VEC_iterate (vn_reference_op_s, operands, i, operand); i++, j++)
	  {
	    pre_expr opresult;
	    pre_expr leader;
	    tree oldop0 = operand->op0;
	    tree oldop1 = operand->op1;
	    tree oldop2 = operand->op2;
	    tree op0 = oldop0;
	    tree op1 = oldop1;
	    tree op2 = oldop2;
	    tree type = operand->type;
	    vn_reference_op_s newop = *operand;

	    if (op0 && TREE_CODE (op0) == SSA_NAME)
	      {
		unsigned int op_val_id = VN_INFO (op0)->value_id;
		leader = find_leader_in_sets (op_val_id, set1, set2);
		opresult = phi_translate_1 (leader, set1, set2,
					    pred, phiblock, seen);
		if (opresult && opresult != leader)
		  {
		    tree name = get_representative_for (opresult);
		    if (!name)
		      break;
		    op0 = name;
		  }
		else if (!opresult)
		  break;
	      }
	    changed |= op0 != oldop0;

	    if (op1 && TREE_CODE (op1) == SSA_NAME)
	      {
		unsigned int op_val_id = VN_INFO (op1)->value_id;
		leader = find_leader_in_sets (op_val_id, set1, set2);
		opresult = phi_translate_1 (leader, set1, set2,
					    pred, phiblock, seen);
		if (opresult && opresult != leader)
		  {
		    tree name = get_representative_for (opresult);
		    if (!name)
		      break;
		    op1 = name;
		  }
		else if (!opresult)
		  break;
	      }
	    /* We can't possibly insert these.  */
	    else if (op1 && !is_gimple_min_invariant (op1))
	      break;
	    changed |= op1 != oldop1;
	    if (op2 && TREE_CODE (op2) == SSA_NAME)
	      {
		unsigned int op_val_id = VN_INFO (op2)->value_id;
		leader = find_leader_in_sets (op_val_id, set1, set2);
		opresult = phi_translate_1 (leader, set1, set2,
					    pred, phiblock, seen);
		if (opresult && opresult != leader)
		  {
		    tree name = get_representative_for (opresult);
		    if (!name)
		      break;
		    op2 = name;
		  }
		else if (!opresult)
		  break;
	      }
	    /* We can't possibly insert these.  */
	    else if (op2 && !is_gimple_min_invariant (op2))
	      break;
	    changed |= op2 != oldop2;

	    if (!newoperands)
	      newoperands = VEC_copy (vn_reference_op_s, heap, operands);
	    /* We may have changed from an SSA_NAME to a constant */
	    if (newop.opcode == SSA_NAME && TREE_CODE (op0) != SSA_NAME)
	      newop.opcode = TREE_CODE (op0);
	    newop.type = type;
	    newop.op0 = op0;
	    newop.op1 = op1;
	    newop.op2 = op2;
	    VEC_replace (vn_reference_op_s, newoperands, j, &newop);
	    /* If it transforms from an SSA_NAME to an address, fold with
	       a preceding indirect reference.  */
	    if (j > 0 && op0 && TREE_CODE (op0) == ADDR_EXPR
		&& VEC_index (vn_reference_op_s,
			      newoperands, j - 1)->opcode == INDIRECT_REF)
	      vn_reference_fold_indirect (&newoperands, &j);
	  }
	if (i != VEC_length (vn_reference_op_s, operands))
	  {
	    if (newoperands)
	      VEC_free (vn_reference_op_s, heap, newoperands);
	    return NULL;
	  }

	if (vuse)
	  {
	    newvuse = translate_vuse_through_block (newoperands,
						    ref->set, ref->type,
						    vuse, phiblock, pred,
						    &same_valid);
	    if (newvuse == NULL_TREE)
	      {
		VEC_free (vn_reference_op_s, heap, newoperands);
		return NULL;
	      }
	  }

	if (changed || newvuse != vuse)
	  {
	    unsigned int new_val_id;
	    pre_expr constant;

	    tree result = vn_reference_lookup_pieces (newvuse, ref->set,
						      ref->type,
						      newoperands,
						      &newref, true);
	    if (newref)
	      VEC_free (vn_reference_op_s, heap, newoperands);

	    if (result && is_gimple_min_invariant (result))
	      {
	        gcc_assert (!newoperands);
	        return get_or_alloc_expr_for_constant (result);
	      }

	    expr = (pre_expr) pool_alloc (pre_expr_pool);
	    expr->kind = REFERENCE;
	    expr->id = 0;

	    if (newref)
	      {
		PRE_EXPR_REFERENCE (expr) = newref;
		constant = fully_constant_expression (expr);
		if (constant != expr)
		  return constant;

		new_val_id = newref->value_id;
		get_or_alloc_expression_id (expr);
	      }
	    else
	      {
		if (changed || !same_valid)
		  {
		    new_val_id = get_next_value_id ();
		    VEC_safe_grow_cleared (bitmap_set_t, heap,
					   value_expressions,
					   get_max_value_id() + 1);
		  }
		else
		  new_val_id = ref->value_id;
		newref = vn_reference_insert_pieces (newvuse, ref->set,
						     ref->type,
						     newoperands,
						     result, new_val_id);
		newoperands = NULL;
		PRE_EXPR_REFERENCE (expr) = newref;
		constant = fully_constant_expression (expr);
		if (constant != expr)
		  return constant;
		get_or_alloc_expression_id (expr);
	      }
	    add_to_value (new_val_id, expr);
	  }
	VEC_free (vn_reference_op_s, heap, newoperands);
	phi_trans_add (oldexpr, expr, pred);
	return expr;
      }
      break;

    case NAME:
      {
	gimple phi = NULL;
	edge e;
	gimple def_stmt;
	tree name = PRE_EXPR_NAME (expr);

	def_stmt = SSA_NAME_DEF_STMT (name);
	if (gimple_code (def_stmt) == GIMPLE_PHI
	    && gimple_bb (def_stmt) == phiblock)
	  phi = def_stmt;
	else
	  return expr;

	e = find_edge (pred, gimple_bb (phi));
	if (e)
	  {
	    tree def = PHI_ARG_DEF (phi, e->dest_idx);
	    pre_expr newexpr;

	    if (TREE_CODE (def) == SSA_NAME)
	      def = VN_INFO (def)->valnum;

	    /* Handle constant. */
	    if (is_gimple_min_invariant (def))
	      return get_or_alloc_expr_for_constant (def);

	    if (TREE_CODE (def) == SSA_NAME && ssa_undefined_value_p (def))
	      return NULL;

	    newexpr = get_or_alloc_expr_for_name (def);
	    return newexpr;
	  }
      }
      return expr;

    default:
      gcc_unreachable ();
    }
}

/* Translate EXPR using phis in PHIBLOCK, so that it has the values of
   the phis in PRED.
   Return NULL if we can't find a leader for each part of the
   translated expression.  */

static pre_expr
phi_translate (pre_expr expr, bitmap_set_t set1, bitmap_set_t set2,
	       basic_block pred, basic_block phiblock)
{
  bitmap_clear (seen_during_translate);
  return phi_translate_1 (expr, set1, set2, pred, phiblock,
			  seen_during_translate);
}

/* For each expression in SET, translate the values through phi nodes
   in PHIBLOCK using edge PHIBLOCK->PRED, and store the resulting
   expressions in DEST.  */

static void
phi_translate_set (bitmap_set_t dest, bitmap_set_t set, basic_block pred,
		   basic_block phiblock)
{
  VEC (pre_expr, heap) *exprs;
  pre_expr expr;
  int i;

  if (!phi_nodes (phiblock))
    {
      bitmap_set_copy (dest, set);
      return;
    }

  exprs = sorted_array_from_bitmap_set (set);
  for (i = 0; VEC_iterate (pre_expr, exprs, i, expr); i++)
    {
      pre_expr translated;
      translated = phi_translate (expr, set, NULL, pred, phiblock);

      /* Don't add empty translations to the cache  */
      if (translated)
	phi_trans_add (expr, translated, pred);

      if (translated != NULL)
	bitmap_value_insert_into_set (dest, translated);
    }
  VEC_free (pre_expr, heap, exprs);
}

/* Find the leader for a value (i.e., the name representing that
   value) in a given set, and return it.  If STMT is non-NULL it
   makes sure the defining statement for the leader dominates it.
   Return NULL if no leader is found.  */

static pre_expr
bitmap_find_leader (bitmap_set_t set, unsigned int val, gimple stmt)
{
  if (value_id_constant_p (val))
    {
      unsigned int i;
      bitmap_iterator bi;
      bitmap_set_t exprset = VEC_index (bitmap_set_t, value_expressions, val);

      FOR_EACH_EXPR_ID_IN_SET (exprset, i, bi)
	{
	  pre_expr expr = expression_for_id (i);
	  if (expr->kind == CONSTANT)
	    return expr;
	}
    }
  if (bitmap_set_contains_value (set, val))
    {
      /* Rather than walk the entire bitmap of expressions, and see
	 whether any of them has the value we are looking for, we look
	 at the reverse mapping, which tells us the set of expressions
	 that have a given value (IE value->expressions with that
	 value) and see if any of those expressions are in our set.
	 The number of expressions per value is usually significantly
	 less than the number of expressions in the set.  In fact, for
	 large testcases, doing it this way is roughly 5-10x faster
	 than walking the bitmap.
	 If this is somehow a significant lose for some cases, we can
	 choose which set to walk based on which set is smaller.  */
      unsigned int i;
      bitmap_iterator bi;
      bitmap_set_t exprset = VEC_index (bitmap_set_t, value_expressions, val);

      EXECUTE_IF_AND_IN_BITMAP (exprset->expressions,
				set->expressions, 0, i, bi)
	{
	  pre_expr val = expression_for_id (i);
	  /* At the point where stmt is not null, there should always
	     be an SSA_NAME first in the list of expressions.  */
	  if (stmt)
	    {
	      gimple def_stmt = SSA_NAME_DEF_STMT (PRE_EXPR_NAME (val));
	      if (gimple_code (def_stmt) != GIMPLE_PHI
		  && gimple_bb (def_stmt) == gimple_bb (stmt)
		  && gimple_uid (def_stmt) >= gimple_uid (stmt))
		continue;
	    }
	  return val;
	}
    }
  return NULL;
}

/* Determine if EXPR, a memory expression, is ANTIC_IN at the top of
   BLOCK by seeing if it is not killed in the block.  Note that we are
   only determining whether there is a store that kills it.  Because
   of the order in which clean iterates over values, we are guaranteed
   that altered operands will have caused us to be eliminated from the
   ANTIC_IN set already.  */

static bool
value_dies_in_block_x (pre_expr expr, basic_block block)
{
  tree vuse = PRE_EXPR_REFERENCE (expr)->vuse;
  vn_reference_t refx = PRE_EXPR_REFERENCE (expr);
  gimple def;
  gimple_stmt_iterator gsi;
  unsigned id = get_expression_id (expr);
  bool res = false;
  ao_ref ref;

  if (!vuse)
    return false;

  /* Lookup a previously calculated result.  */
  if (EXPR_DIES (block)
      && bitmap_bit_p (EXPR_DIES (block), id * 2))
    return bitmap_bit_p (EXPR_DIES (block), id * 2 + 1);

  /* A memory expression {e, VUSE} dies in the block if there is a
     statement that may clobber e.  If, starting statement walk from the
     top of the basic block, a statement uses VUSE there can be no kill
     inbetween that use and the original statement that loaded {e, VUSE},
     so we can stop walking.  */
  ref.base = NULL_TREE;
  for (gsi = gsi_start_bb (block); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      tree def_vuse, def_vdef;
      def = gsi_stmt (gsi);
      def_vuse = gimple_vuse (def);
      def_vdef = gimple_vdef (def);

      /* Not a memory statement.  */
      if (!def_vuse)
	continue;

      /* Not a may-def.  */
      if (!def_vdef)
	{
	  /* A load with the same VUSE, we're done.  */
	  if (def_vuse == vuse)
	    break;

	  continue;
	}

      /* Init ref only if we really need it.  */
      if (ref.base == NULL_TREE
	  && !ao_ref_init_from_vn_reference (&ref, refx->set, refx->type,
					     refx->operands))
	{
	  res = true;
	  break;
	}
      /* If the statement may clobber expr, it dies.  */
      if (stmt_may_clobber_ref_p_1 (def, &ref))
	{
	  res = true;
	  break;
	}
    }

  /* Remember the result.  */
  if (!EXPR_DIES (block))
    EXPR_DIES (block) = BITMAP_ALLOC (&grand_bitmap_obstack);
  bitmap_set_bit (EXPR_DIES (block), id * 2);
  if (res)
    bitmap_set_bit (EXPR_DIES (block), id * 2 + 1);

  return res;
}


#define union_contains_value(SET1, SET2, VAL)			\
  (bitmap_set_contains_value ((SET1), (VAL))			\
   || ((SET2) && bitmap_set_contains_value ((SET2), (VAL))))

/* Determine if vn_reference_op_t VRO is legal in SET1 U SET2.
 */
static bool
vro_valid_in_sets (bitmap_set_t set1, bitmap_set_t set2,
		   vn_reference_op_t vro)
{
  if (vro->op0 && TREE_CODE (vro->op0) == SSA_NAME)
    {
      struct pre_expr_d temp;
      temp.kind = NAME;
      temp.id = 0;
      PRE_EXPR_NAME (&temp) = vro->op0;
      temp.id = lookup_expression_id (&temp);
      if (temp.id == 0)
	return false;
      if (!union_contains_value (set1, set2,
				 get_expr_value_id (&temp)))
	return false;
    }
  if (vro->op1 && TREE_CODE (vro->op1) == SSA_NAME)
    {
      struct pre_expr_d temp;
      temp.kind = NAME;
      temp.id = 0;
      PRE_EXPR_NAME (&temp) = vro->op1;
      temp.id = lookup_expression_id (&temp);
      if (temp.id == 0)
	return false;
      if (!union_contains_value (set1, set2,
				 get_expr_value_id (&temp)))
	return false;
    }

  if (vro->op2 && TREE_CODE (vro->op2) == SSA_NAME)
    {
      struct pre_expr_d temp;
      temp.kind = NAME;
      temp.id = 0;
      PRE_EXPR_NAME (&temp) = vro->op2;
      temp.id = lookup_expression_id (&temp);
      if (temp.id == 0)
	return false;
      if (!union_contains_value (set1, set2,
				 get_expr_value_id (&temp)))
	return false;
    }

  return true;
}

/* Determine if the expression EXPR is valid in SET1 U SET2.
   ONLY SET2 CAN BE NULL.
   This means that we have a leader for each part of the expression
   (if it consists of values), or the expression is an SSA_NAME.
   For loads/calls, we also see if the vuse is killed in this block.  */

static bool
valid_in_sets (bitmap_set_t set1, bitmap_set_t set2, pre_expr expr,
	       basic_block block)
{
  switch (expr->kind)
    {
    case NAME:
      return bitmap_set_contains_expr (AVAIL_OUT (block), expr);
    case NARY:
      {
	unsigned int i;
	vn_nary_op_t nary = PRE_EXPR_NARY (expr);
	for (i = 0; i < nary->length; i++)
	  {
	    if (TREE_CODE (nary->op[i]) == SSA_NAME)
	      {
		struct pre_expr_d temp;
		temp.kind = NAME;
		temp.id = 0;
		PRE_EXPR_NAME (&temp) = nary->op[i];
		temp.id = lookup_expression_id (&temp);
		if (temp.id == 0)
		  return false;
		if (!union_contains_value (set1, set2,
					   get_expr_value_id (&temp)))
		  return false;
	      }
	  }
	return true;
      }
      break;
    case REFERENCE:
      {
	vn_reference_t ref = PRE_EXPR_REFERENCE (expr);
	vn_reference_op_t vro;
	unsigned int i;

	for (i = 0; VEC_iterate (vn_reference_op_s, ref->operands, i, vro); i++)
	  {
	    if (!vro_valid_in_sets (set1, set2, vro))
	      return false;
	  }
	if (ref->vuse)
	  {
	    gimple def_stmt = SSA_NAME_DEF_STMT (ref->vuse);
	    if (!gimple_nop_p (def_stmt)
		&& gimple_bb (def_stmt) != block
		&& !dominated_by_p (CDI_DOMINATORS,
				    block, gimple_bb (def_stmt)))
	      return false;
	  }
	return !value_dies_in_block_x (expr, block);
      }
    default:
      gcc_unreachable ();
    }
}

/* Clean the set of expressions that are no longer valid in SET1 or
   SET2.  This means expressions that are made up of values we have no
   leaders for in SET1 or SET2.  This version is used for partial
   anticipation, which means it is not valid in either ANTIC_IN or
   PA_IN.  */

static void
dependent_clean (bitmap_set_t set1, bitmap_set_t set2, basic_block block)
{
  VEC (pre_expr, heap) *exprs = sorted_array_from_bitmap_set (set1);
  pre_expr expr;
  int i;

  for (i = 0; VEC_iterate (pre_expr, exprs, i, expr); i++)
    {
      if (!valid_in_sets (set1, set2, expr, block))
	bitmap_remove_from_set (set1, expr);
    }
  VEC_free (pre_expr, heap, exprs);
}

/* Clean the set of expressions that are no longer valid in SET.  This
   means expressions that are made up of values we have no leaders for
   in SET.  */

static void
clean (bitmap_set_t set, basic_block block)
{
  VEC (pre_expr, heap) *exprs = sorted_array_from_bitmap_set (set);
  pre_expr expr;
  int i;

  for (i = 0; VEC_iterate (pre_expr, exprs, i, expr); i++)
    {
      if (!valid_in_sets (set, NULL, expr, block))
	bitmap_remove_from_set (set, expr);
    }
  VEC_free (pre_expr, heap, exprs);
}

static sbitmap has_abnormal_preds;

/* List of blocks that may have changed during ANTIC computation and
   thus need to be iterated over.  */

static sbitmap changed_blocks;

/* Decide whether to defer a block for a later iteration, or PHI
   translate SOURCE to DEST using phis in PHIBLOCK.  Return false if we
   should defer the block, and true if we processed it.  */

static bool
defer_or_phi_translate_block (bitmap_set_t dest, bitmap_set_t source,
			      basic_block block, basic_block phiblock)
{
  if (!BB_VISITED (phiblock))
    {
      SET_BIT (changed_blocks, block->index);
      BB_VISITED (block) = 0;
      BB_DEFERRED (block) = 1;
      return false;
    }
  else
    phi_translate_set (dest, source, block, phiblock);
  return true;
}

/* Compute the ANTIC set for BLOCK.

   If succs(BLOCK) > 1 then
     ANTIC_OUT[BLOCK] = intersection of ANTIC_IN[b] for all succ(BLOCK)
   else if succs(BLOCK) == 1 then
     ANTIC_OUT[BLOCK] = phi_translate (ANTIC_IN[succ(BLOCK)])

   ANTIC_IN[BLOCK] = clean(ANTIC_OUT[BLOCK] U EXP_GEN[BLOCK] - TMP_GEN[BLOCK])
*/

static bool
compute_antic_aux (basic_block block, bool block_has_abnormal_pred_edge)
{
  bool changed = false;
  bitmap_set_t S, old, ANTIC_OUT;
  bitmap_iterator bi;
  unsigned int bii;
  edge e;
  edge_iterator ei;

  old = ANTIC_OUT = S = NULL;
  BB_VISITED (block) = 1;

  /* If any edges from predecessors are abnormal, antic_in is empty,
     so do nothing.  */
  if (block_has_abnormal_pred_edge)
    goto maybe_dump_sets;

  old = ANTIC_IN (block);
  ANTIC_OUT = bitmap_set_new ();

  /* If the block has no successors, ANTIC_OUT is empty.  */
  if (EDGE_COUNT (block->succs) == 0)
    ;
  /* If we have one successor, we could have some phi nodes to
     translate through.  */
  else if (single_succ_p (block))
    {
      basic_block succ_bb = single_succ (block);

      /* We trade iterations of the dataflow equations for having to
	 phi translate the maximal set, which is incredibly slow
	 (since the maximal set often has 300+ members, even when you
	 have a small number of blocks).
	 Basically, we defer the computation of ANTIC for this block
	 until we have processed it's successor, which will inevitably
	 have a *much* smaller set of values to phi translate once
	 clean has been run on it.
	 The cost of doing this is that we technically perform more
	 iterations, however, they are lower cost iterations.

	 Timings for PRE on tramp3d-v4:
	 without maximal set fix: 11 seconds
	 with maximal set fix/without deferring: 26 seconds
	 with maximal set fix/with deferring: 11 seconds
     */

      if (!defer_or_phi_translate_block (ANTIC_OUT, ANTIC_IN (succ_bb),
					block, succ_bb))
	{
	  changed = true;
	  goto maybe_dump_sets;
	}
    }
  /* If we have multiple successors, we take the intersection of all of
     them.  Note that in the case of loop exit phi nodes, we may have
     phis to translate through.  */
  else
    {
      VEC(basic_block, heap) * worklist;
      size_t i;
      basic_block bprime, first = NULL;

      worklist = VEC_alloc (basic_block, heap, EDGE_COUNT (block->succs));
      FOR_EACH_EDGE (e, ei, block->succs)
	{
	  if (!first
	      && BB_VISITED (e->dest))
	    first = e->dest;
	  else if (BB_VISITED (e->dest))
	    VEC_quick_push (basic_block, worklist, e->dest);
	}

      /* Of multiple successors we have to have visited one already.  */
      if (!first)
	{
	  SET_BIT (changed_blocks, block->index);
	  BB_VISITED (block) = 0;
	  BB_DEFERRED (block) = 1;
	  changed = true;
	  VEC_free (basic_block, heap, worklist);
	  goto maybe_dump_sets;
	}

      if (phi_nodes (first))
	phi_translate_set (ANTIC_OUT, ANTIC_IN (first), block, first);
      else
	bitmap_set_copy (ANTIC_OUT, ANTIC_IN (first));

      for (i = 0; VEC_iterate (basic_block, worklist, i, bprime); i++)
	{
	  if (phi_nodes (bprime))
	    {
	      bitmap_set_t tmp = bitmap_set_new ();
	      phi_translate_set (tmp, ANTIC_IN (bprime), block, bprime);
	      bitmap_set_and (ANTIC_OUT, tmp);
	      bitmap_set_free (tmp);
	    }
	  else
	    bitmap_set_and (ANTIC_OUT, ANTIC_IN (bprime));
	}
      VEC_free (basic_block, heap, worklist);
    }

  /* Generate ANTIC_OUT - TMP_GEN.  */
  S = bitmap_set_subtract (ANTIC_OUT, TMP_GEN (block));

  /* Start ANTIC_IN with EXP_GEN - TMP_GEN.  */
  ANTIC_IN (block) = bitmap_set_subtract (EXP_GEN (block),
					  TMP_GEN (block));

  /* Then union in the ANTIC_OUT - TMP_GEN values,
     to get ANTIC_OUT U EXP_GEN - TMP_GEN */
  FOR_EACH_EXPR_ID_IN_SET (S, bii, bi)
    bitmap_value_insert_into_set (ANTIC_IN (block),
				  expression_for_id (bii));

  clean (ANTIC_IN (block), block);

  /* !old->expressions can happen when we deferred a block.  */
  if (!old->expressions || !bitmap_set_equal (old, ANTIC_IN (block)))
    {
      changed = true;
      SET_BIT (changed_blocks, block->index);
      FOR_EACH_EDGE (e, ei, block->preds)
	SET_BIT (changed_blocks, e->src->index);
    }
  else
    RESET_BIT (changed_blocks, block->index);

 maybe_dump_sets:
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (!BB_DEFERRED (block) || BB_VISITED (block))
	{
	  if (ANTIC_OUT)
	    print_bitmap_set (dump_file, ANTIC_OUT, "ANTIC_OUT", block->index);

	  print_bitmap_set (dump_file, ANTIC_IN (block), "ANTIC_IN",
			    block->index);

	  if (S)
	    print_bitmap_set (dump_file, S, "S", block->index);
	}
      else
	{
	  fprintf (dump_file,
		   "Block %d was deferred for a future iteration.\n",
		   block->index);
	}
    }
  if (old)
    bitmap_set_free (old);
  if (S)
    bitmap_set_free (S);
  if (ANTIC_OUT)
    bitmap_set_free (ANTIC_OUT);
  return changed;
}

/* Compute PARTIAL_ANTIC for BLOCK.

   If succs(BLOCK) > 1 then
     PA_OUT[BLOCK] = value wise union of PA_IN[b] + all ANTIC_IN not
     in ANTIC_OUT for all succ(BLOCK)
   else if succs(BLOCK) == 1 then
     PA_OUT[BLOCK] = phi_translate (PA_IN[succ(BLOCK)])

   PA_IN[BLOCK] = dependent_clean(PA_OUT[BLOCK] - TMP_GEN[BLOCK]
				  - ANTIC_IN[BLOCK])

*/
static bool
compute_partial_antic_aux (basic_block block,
			   bool block_has_abnormal_pred_edge)
{
  bool changed = false;
  bitmap_set_t old_PA_IN;
  bitmap_set_t PA_OUT;
  edge e;
  edge_iterator ei;
  unsigned long max_pa = PARAM_VALUE (PARAM_MAX_PARTIAL_ANTIC_LENGTH);

  old_PA_IN = PA_OUT = NULL;

  /* If any edges from predecessors are abnormal, antic_in is empty,
     so do nothing.  */
  if (block_has_abnormal_pred_edge)
    goto maybe_dump_sets;

  /* If there are too many partially anticipatable values in the
     block, phi_translate_set can take an exponential time: stop
     before the translation starts.  */
  if (max_pa
      && single_succ_p (block)
      && bitmap_count_bits (PA_IN (single_succ (block))->values) > max_pa)
    goto maybe_dump_sets;

  old_PA_IN = PA_IN (block);
  PA_OUT = bitmap_set_new ();

  /* If the block has no successors, ANTIC_OUT is empty.  */
  if (EDGE_COUNT (block->succs) == 0)
    ;
  /* If we have one successor, we could have some phi nodes to
     translate through.  Note that we can't phi translate across DFS
     back edges in partial antic, because it uses a union operation on
     the successors.  For recurrences like IV's, we will end up
     generating a new value in the set on each go around (i + 3 (VH.1)
     VH.1 + 1 (VH.2), VH.2 + 1 (VH.3), etc), forever.  */
  else if (single_succ_p (block))
    {
      basic_block succ = single_succ (block);
      if (!(single_succ_edge (block)->flags & EDGE_DFS_BACK))
	phi_translate_set (PA_OUT, PA_IN (succ), block, succ);
    }
  /* If we have multiple successors, we take the union of all of
     them.  */
  else
    {
      VEC(basic_block, heap) * worklist;
      size_t i;
      basic_block bprime;

      worklist = VEC_alloc (basic_block, heap, EDGE_COUNT (block->succs));
      FOR_EACH_EDGE (e, ei, block->succs)
	{
	  if (e->flags & EDGE_DFS_BACK)
	    continue;
	  VEC_quick_push (basic_block, worklist, e->dest);
	}
      if (VEC_length (basic_block, worklist) > 0)
	{
	  for (i = 0; VEC_iterate (basic_block, worklist, i, bprime); i++)
	    {
	      unsigned int i;
	      bitmap_iterator bi;

	      FOR_EACH_EXPR_ID_IN_SET (ANTIC_IN (bprime), i, bi)
		bitmap_value_insert_into_set (PA_OUT,
					      expression_for_id (i));
	      if (phi_nodes (bprime))
		{
		  bitmap_set_t pa_in = bitmap_set_new ();
		  phi_translate_set (pa_in, PA_IN (bprime), block, bprime);
		  FOR_EACH_EXPR_ID_IN_SET (pa_in, i, bi)
		    bitmap_value_insert_into_set (PA_OUT,
						  expression_for_id (i));
		  bitmap_set_free (pa_in);
		}
	      else
		FOR_EACH_EXPR_ID_IN_SET (PA_IN (bprime), i, bi)
		  bitmap_value_insert_into_set (PA_OUT,
						expression_for_id (i));
	    }
	}
      VEC_free (basic_block, heap, worklist);
    }

  /* PA_IN starts with PA_OUT - TMP_GEN.
     Then we subtract things from ANTIC_IN.  */
  PA_IN (block) = bitmap_set_subtract (PA_OUT, TMP_GEN (block));

  /* For partial antic, we want to put back in the phi results, since
     we will properly avoid making them partially antic over backedges.  */
  bitmap_ior_into (PA_IN (block)->values, PHI_GEN (block)->values);
  bitmap_ior_into (PA_IN (block)->expressions, PHI_GEN (block)->expressions);

  /* PA_IN[block] = PA_IN[block] - ANTIC_IN[block] */
  bitmap_set_subtract_values (PA_IN (block), ANTIC_IN (block));

  dependent_clean (PA_IN (block), ANTIC_IN (block), block);

  if (!bitmap_set_equal (old_PA_IN, PA_IN (block)))
    {
      changed = true;
      SET_BIT (changed_blocks, block->index);
      FOR_EACH_EDGE (e, ei, block->preds)
	SET_BIT (changed_blocks, e->src->index);
    }
  else
    RESET_BIT (changed_blocks, block->index);

 maybe_dump_sets:
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (PA_OUT)
	print_bitmap_set (dump_file, PA_OUT, "PA_OUT", block->index);

      print_bitmap_set (dump_file, PA_IN (block), "PA_IN", block->index);
    }
  if (old_PA_IN)
    bitmap_set_free (old_PA_IN);
  if (PA_OUT)
    bitmap_set_free (PA_OUT);
  return changed;
}

/* Compute ANTIC and partial ANTIC sets.  */

static void
compute_antic (void)
{
  bool changed = true;
  int num_iterations = 0;
  basic_block block;
  int i;

  /* If any predecessor edges are abnormal, we punt, so antic_in is empty.
     We pre-build the map of blocks with incoming abnormal edges here.  */
  has_abnormal_preds = sbitmap_alloc (last_basic_block);
  sbitmap_zero (has_abnormal_preds);

  FOR_EACH_BB (block)
    {
      edge_iterator ei;
      edge e;

      FOR_EACH_EDGE (e, ei, block->preds)
	{
	  e->flags &= ~EDGE_DFS_BACK;
	  if (e->flags & EDGE_ABNORMAL)
	    {
	      SET_BIT (has_abnormal_preds, block->index);
	      break;
	    }
	}

      BB_VISITED (block) = 0;
      BB_DEFERRED (block) = 0;
      /* While we are here, give empty ANTIC_IN sets to each block.  */
      ANTIC_IN (block) = bitmap_set_new ();
      PA_IN (block) = bitmap_set_new ();
    }

  /* At the exit block we anticipate nothing.  */
  ANTIC_IN (EXIT_BLOCK_PTR) = bitmap_set_new ();
  BB_VISITED (EXIT_BLOCK_PTR) = 1;
  PA_IN (EXIT_BLOCK_PTR) = bitmap_set_new ();

  changed_blocks = sbitmap_alloc (last_basic_block + 1);
  sbitmap_ones (changed_blocks);
  while (changed)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Starting iteration %d\n", num_iterations);
      num_iterations++;
      changed = false;
      for (i = 0; i < n_basic_blocks - NUM_FIXED_BLOCKS; i++)
	{
	  if (TEST_BIT (changed_blocks, postorder[i]))
	    {
	      basic_block block = BASIC_BLOCK (postorder[i]);
	      changed |= compute_antic_aux (block,
					    TEST_BIT (has_abnormal_preds,
						      block->index));
	    }
	}
#ifdef ENABLE_CHECKING
      /* Theoretically possible, but *highly* unlikely.  */
      gcc_assert (num_iterations < 500);
#endif
    }

  statistics_histogram_event (cfun, "compute_antic iterations",
			      num_iterations);

  if (do_partial_partial)
    {
      sbitmap_ones (changed_blocks);
      mark_dfs_back_edges ();
      num_iterations = 0;
      changed = true;
      while (changed)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Starting iteration %d\n", num_iterations);
	  num_iterations++;
	  changed = false;
	  for (i = 0; i < n_basic_blocks - NUM_FIXED_BLOCKS; i++)
	    {
	      if (TEST_BIT (changed_blocks, postorder[i]))
		{
		  basic_block block = BASIC_BLOCK (postorder[i]);
		  changed
		    |= compute_partial_antic_aux (block,
						  TEST_BIT (has_abnormal_preds,
							    block->index));
		}
	    }
#ifdef ENABLE_CHECKING
	  /* Theoretically possible, but *highly* unlikely.  */
	  gcc_assert (num_iterations < 500);
#endif
	}
      statistics_histogram_event (cfun, "compute_partial_antic iterations",
				  num_iterations);
    }
  sbitmap_free (has_abnormal_preds);
  sbitmap_free (changed_blocks);
}

/* Return true if we can value number the call in STMT.  This is true
   if we have a pure or constant call.  */

static bool
can_value_number_call (gimple stmt)
{
  if (gimple_call_flags (stmt) & (ECF_PURE | ECF_CONST))
    return true;
  return false;
}

/* Return true if OP is a tree which we can perform PRE on.
   This may not match the operations we can value number, but in
   a perfect world would.  */

static bool
can_PRE_operation (tree op)
{
  return UNARY_CLASS_P (op)
    || BINARY_CLASS_P (op)
    || COMPARISON_CLASS_P (op)
    || TREE_CODE (op) == INDIRECT_REF
    || TREE_CODE (op) == COMPONENT_REF
    || TREE_CODE (op) == VIEW_CONVERT_EXPR
    || TREE_CODE (op) == CALL_EXPR
    || TREE_CODE (op) == ARRAY_REF;
}


/* Inserted expressions are placed onto this worklist, which is used
   for performing quick dead code elimination of insertions we made
   that didn't turn out to be necessary.   */
static VEC(gimple,heap) *inserted_exprs;
static bitmap inserted_phi_names;

/* Pool allocated fake store expressions are placed onto this
   worklist, which, after performing dead code elimination, is walked
   to see which expressions need to be put into GC'able memory  */
static VEC(gimple, heap) *need_creation;

/* The actual worker for create_component_ref_by_pieces.  */

static tree
create_component_ref_by_pieces_1 (basic_block block, vn_reference_t ref,
				  unsigned int *operand, gimple_seq *stmts,
				  gimple domstmt)
{
  vn_reference_op_t currop = VEC_index (vn_reference_op_s, ref->operands,
					*operand);
  tree genop;
  ++*operand;
  switch (currop->opcode)
    {
    case CALL_EXPR:
      {
	tree folded, sc = currop->op1;
	unsigned int nargs = 0;
	tree *args = XNEWVEC (tree, VEC_length (vn_reference_op_s,
						ref->operands) - 1);
	while (*operand < VEC_length (vn_reference_op_s, ref->operands))
	  {
	    args[nargs] = create_component_ref_by_pieces_1 (block, ref,
							    operand, stmts,
							    domstmt);
	    nargs++;
	  }
	folded = build_call_array (currop->type,
				   TREE_CODE (currop->op0) == FUNCTION_DECL
				   ? build_fold_addr_expr (currop->op0)
				   : currop->op0,
				   nargs, args);
	free (args);
	if (sc)
	  {
	    pre_expr scexpr = get_or_alloc_expr_for (sc);
	    sc = find_or_generate_expression (block, scexpr, stmts, domstmt);
	    if (!sc)
	      return NULL_TREE;
	    CALL_EXPR_STATIC_CHAIN (folded) = sc;
	  }
	return folded;
      }
      break;
    case TARGET_MEM_REF:
      {
	vn_reference_op_t nextop = VEC_index (vn_reference_op_s, ref->operands,
					      *operand);
	pre_expr op0expr;
	tree genop0 = NULL_TREE;
	tree baseop = create_component_ref_by_pieces_1 (block, ref, operand,
							stmts, domstmt);
	if (!baseop)
	  return NULL_TREE;
	if (currop->op0)
	  {
	    op0expr = get_or_alloc_expr_for (currop->op0);
	    genop0 = find_or_generate_expression (block, op0expr,
						  stmts, domstmt);
	    if (!genop0)
	      return NULL_TREE;
	  }
	if (DECL_P (baseop))
	  return build6 (TARGET_MEM_REF, currop->type,
			 baseop, NULL_TREE,
			 genop0, currop->op1, currop->op2,
			 unshare_expr (nextop->op1));
	else
	  return build6 (TARGET_MEM_REF, currop->type,
			 NULL_TREE, baseop,
			 genop0, currop->op1, currop->op2,
			 unshare_expr (nextop->op1));
      }
      break;
    case ADDR_EXPR:
      if (currop->op0)
	{
	  gcc_assert (is_gimple_min_invariant (currop->op0));
	  return currop->op0;
	}
      /* Fallthrough.  */
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case VIEW_CONVERT_EXPR:
      {
	tree folded;
	tree genop0 = create_component_ref_by_pieces_1 (block, ref,
							operand,
							stmts, domstmt);
	if (!genop0)
	  return NULL_TREE;
	folded = fold_build1 (currop->opcode, currop->type,
			      genop0);
	return folded;
      }
      break;
    case ALIGN_INDIRECT_REF:
    case MISALIGNED_INDIRECT_REF:
    case INDIRECT_REF:
      {
	tree folded;
	tree genop1 = create_component_ref_by_pieces_1 (block, ref,
							operand,
							stmts, domstmt);
	if (!genop1)
	  return NULL_TREE;
	genop1 = fold_convert (build_pointer_type (currop->type),
			       genop1);

	if (currop->opcode == MISALIGNED_INDIRECT_REF)
	  folded = fold_build2 (currop->opcode, currop->type,
				genop1, currop->op1);
	else
	  folded = fold_build1 (currop->opcode, currop->type,
				genop1);
	return folded;
      }
      break;
    case BIT_FIELD_REF:
      {
	tree folded;
	tree genop0 = create_component_ref_by_pieces_1 (block, ref, operand,
							stmts, domstmt);
	pre_expr op1expr = get_or_alloc_expr_for (currop->op0);
	pre_expr op2expr = get_or_alloc_expr_for (currop->op1);
	tree genop1;
	tree genop2;

	if (!genop0)
	  return NULL_TREE;
	genop1 = find_or_generate_expression (block, op1expr, stmts, domstmt);
	if (!genop1)
	  return NULL_TREE;
	genop2 = find_or_generate_expression (block, op2expr, stmts, domstmt);
	if (!genop2)
	  return NULL_TREE;
	folded = fold_build3 (BIT_FIELD_REF, currop->type, genop0, genop1,
			      genop2);
	return folded;
      }

      /* For array ref vn_reference_op's, operand 1 of the array ref
	 is op0 of the reference op and operand 3 of the array ref is
	 op1.  */
    case ARRAY_RANGE_REF:
    case ARRAY_REF:
      {
	tree genop0;
	tree genop1 = currop->op0;
	pre_expr op1expr;
	tree genop2 = currop->op1;
	pre_expr op2expr;
	tree genop3 = currop->op2;
	pre_expr op3expr;
	genop0 = create_component_ref_by_pieces_1 (block, ref, operand,
						   stmts, domstmt);
	if (!genop0)
	  return NULL_TREE;
	op1expr = get_or_alloc_expr_for (genop1);
	genop1 = find_or_generate_expression (block, op1expr, stmts, domstmt);
	if (!genop1)
	  return NULL_TREE;
	if (genop2)
	  {
	    op2expr = get_or_alloc_expr_for (genop2);
	    genop2 = find_or_generate_expression (block, op2expr, stmts,
						  domstmt);
	    if (!genop2)
	      return NULL_TREE;
	  }
	if (genop3)
	  {
	    tree elmt_type = TREE_TYPE (TREE_TYPE (genop0));
	    genop3 = size_binop (EXACT_DIV_EXPR, genop3,
				 size_int (TYPE_ALIGN_UNIT (elmt_type)));
	    op3expr = get_or_alloc_expr_for (genop3);
	    genop3 = find_or_generate_expression (block, op3expr, stmts,
						  domstmt);
	    if (!genop3)
	      return NULL_TREE;
	  }
	return build4 (currop->opcode, currop->type, genop0, genop1,
		       genop2, genop3);
      }
    case COMPONENT_REF:
      {
	tree op0;
	tree op1;
	tree genop2 = currop->op1;
	pre_expr op2expr;
	op0 = create_component_ref_by_pieces_1 (block, ref, operand,
						stmts, domstmt);
	if (!op0)
	  return NULL_TREE;
	/* op1 should be a FIELD_DECL, which are represented by
	   themselves.  */
	op1 = currop->op0;
	if (genop2)
	  {
	    op2expr = get_or_alloc_expr_for (genop2);
	    genop2 = find_or_generate_expression (block, op2expr, stmts,
						  domstmt);
	    if (!genop2)
	      return NULL_TREE;
	  }

	return fold_build3 (COMPONENT_REF, TREE_TYPE (op1), op0, op1,
			    genop2);
      }
      break;
    case SSA_NAME:
      {
	pre_expr op0expr = get_or_alloc_expr_for (currop->op0);
	genop = find_or_generate_expression (block, op0expr, stmts, domstmt);
	return genop;
      }
    case STRING_CST:
    case INTEGER_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
    case REAL_CST:
    case CONSTRUCTOR:
    case VAR_DECL:
    case PARM_DECL:
    case CONST_DECL:
    case RESULT_DECL:
    case FUNCTION_DECL:
      return currop->op0;

    default:
      gcc_unreachable ();
    }
}

/* For COMPONENT_REF's and ARRAY_REF's, we can't have any intermediates for the
   COMPONENT_REF or INDIRECT_REF or ARRAY_REF portion, because we'd end up with
   trying to rename aggregates into ssa form directly, which is a no no.

   Thus, this routine doesn't create temporaries, it just builds a
   single access expression for the array, calling
   find_or_generate_expression to build the innermost pieces.

   This function is a subroutine of create_expression_by_pieces, and
   should not be called on it's own unless you really know what you
   are doing.  */

static tree
create_component_ref_by_pieces (basic_block block, vn_reference_t ref,
				gimple_seq *stmts, gimple domstmt)
{
  unsigned int op = 0;
  return create_component_ref_by_pieces_1 (block, ref, &op, stmts, domstmt);
}

/* Find a leader for an expression, or generate one using
   create_expression_by_pieces if it's ANTIC but
   complex.
   BLOCK is the basic_block we are looking for leaders in.
   EXPR is the expression to find a leader or generate for.
   STMTS is the statement list to put the inserted expressions on.
   Returns the SSA_NAME of the LHS of the generated expression or the
   leader.
   DOMSTMT if non-NULL is a statement that should be dominated by
   all uses in the generated expression.  If DOMSTMT is non-NULL this
   routine can fail and return NULL_TREE.  Otherwise it will assert
   on failure.  */

static tree
find_or_generate_expression (basic_block block, pre_expr expr,
			     gimple_seq *stmts, gimple domstmt)
{
  pre_expr leader = bitmap_find_leader (AVAIL_OUT (block),
					get_expr_value_id (expr), domstmt);
  tree genop = NULL;
  if (leader)
    {
      if (leader->kind == NAME)
	genop = PRE_EXPR_NAME (leader);
      else if (leader->kind == CONSTANT)
	genop = PRE_EXPR_CONSTANT (leader);
    }

  /* If it's still NULL, it must be a complex expression, so generate
     it recursively.  Not so for FRE though.  */
  if (genop == NULL
      && !in_fre)
    {
      bitmap_set_t exprset;
      unsigned int lookfor = get_expr_value_id (expr);
      bool handled = false;
      bitmap_iterator bi;
      unsigned int i;

      exprset = VEC_index (bitmap_set_t, value_expressions, lookfor);
      FOR_EACH_EXPR_ID_IN_SET (exprset, i, bi)
	{
	  pre_expr temp = expression_for_id (i);
	  if (temp->kind != NAME)
	    {
	      handled = true;
	      genop = create_expression_by_pieces (block, temp, stmts,
						   domstmt,
						   get_expr_type (expr));
	      break;
	    }
	}
      if (!handled && domstmt)
	return NULL_TREE;

      gcc_assert (handled);
    }
  return genop;
}

#define NECESSARY GF_PLF_1

/* Create an expression in pieces, so that we can handle very complex
   expressions that may be ANTIC, but not necessary GIMPLE.
   BLOCK is the basic block the expression will be inserted into,
   EXPR is the expression to insert (in value form)
   STMTS is a statement list to append the necessary insertions into.

   This function will die if we hit some value that shouldn't be
   ANTIC but is (IE there is no leader for it, or its components).
   This function may also generate expressions that are themselves
   partially or fully redundant.  Those that are will be either made
   fully redundant during the next iteration of insert (for partially
   redundant ones), or eliminated by eliminate (for fully redundant
   ones).

   If DOMSTMT is non-NULL then we make sure that all uses in the
   expressions dominate that statement.  In this case the function
   can return NULL_TREE to signal failure.  */

static tree
create_expression_by_pieces (basic_block block, pre_expr expr,
			     gimple_seq *stmts, gimple domstmt, tree type)
{
  tree temp, name;
  tree folded;
  gimple_seq forced_stmts = NULL;
  unsigned int value_id;
  gimple_stmt_iterator gsi;
  tree exprtype = type ? type : get_expr_type (expr);
  pre_expr nameexpr;
  gimple newstmt;

  switch (expr->kind)
    {
      /* We may hit the NAME/CONSTANT case if we have to convert types
	 that value numbering saw through.  */
    case NAME:
      folded = PRE_EXPR_NAME (expr);
      break;
    case CONSTANT:
      folded = PRE_EXPR_CONSTANT (expr);
      break;
    case REFERENCE:
      {
	vn_reference_t ref = PRE_EXPR_REFERENCE (expr);
	folded = create_component_ref_by_pieces (block, ref, stmts, domstmt);
      }
      break;
    case NARY:
      {
	vn_nary_op_t nary = PRE_EXPR_NARY (expr);
	switch (nary->length)
	  {
	  case 2:
	    {
	      pre_expr op1 = get_or_alloc_expr_for (nary->op[0]);
	      pre_expr op2 = get_or_alloc_expr_for (nary->op[1]);
	      tree genop1 = find_or_generate_expression (block, op1,
							 stmts, domstmt);
	      tree genop2 = find_or_generate_expression (block, op2,
							 stmts, domstmt);
	      if (!genop1 || !genop2)
		return NULL_TREE;
	      genop1 = fold_convert (TREE_TYPE (nary->op[0]),
				     genop1);
	      /* Ensure op2 is a sizetype for POINTER_PLUS_EXPR.  It
		 may be a constant with the wrong type.  */
	      if (nary->opcode == POINTER_PLUS_EXPR)
		genop2 = fold_convert (sizetype, genop2);
	      else
		genop2 = fold_convert (TREE_TYPE (nary->op[1]), genop2);

	      folded = fold_build2 (nary->opcode, nary->type,
				    genop1, genop2);
	    }
	    break;
	  case 1:
	    {
	      pre_expr op1 = get_or_alloc_expr_for (nary->op[0]);
	      tree genop1 = find_or_generate_expression (block, op1,
							 stmts, domstmt);
	      if (!genop1)
		return NULL_TREE;
	      genop1 = fold_convert (TREE_TYPE (nary->op[0]), genop1);

	      folded = fold_build1 (nary->opcode, nary->type,
				    genop1);
	    }
	    break;
	  default:
	    return NULL_TREE;
	  }
      }
      break;
    default:
      return NULL_TREE;
    }

  if (!useless_type_conversion_p (exprtype, TREE_TYPE (folded)))
    folded = fold_convert (exprtype, folded);

  /* Force the generated expression to be a sequence of GIMPLE
     statements.
     We have to call unshare_expr because force_gimple_operand may
     modify the tree we pass to it.  */
  folded = force_gimple_operand (unshare_expr (folded), &forced_stmts,
				 false, NULL);

  /* If we have any intermediate expressions to the value sets, add them
     to the value sets and chain them in the instruction stream.  */
  if (forced_stmts)
    {
      gsi = gsi_start (forced_stmts);
      for (; !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  tree forcedname = gimple_get_lhs (stmt);
	  pre_expr nameexpr;

	  VEC_safe_push (gimple, heap, inserted_exprs, stmt);
	  if (TREE_CODE (forcedname) == SSA_NAME)
	    {
	      VN_INFO_GET (forcedname)->valnum = forcedname;
	      VN_INFO (forcedname)->value_id = get_next_value_id ();
	      nameexpr = get_or_alloc_expr_for_name (forcedname);
	      add_to_value (VN_INFO (forcedname)->value_id, nameexpr);
	      if (!in_fre)
		bitmap_value_replace_in_set (NEW_SETS (block), nameexpr);
	      bitmap_value_replace_in_set (AVAIL_OUT (block), nameexpr);
	    }
	  mark_symbols_for_renaming (stmt);
	}
      gimple_seq_add_seq (stmts, forced_stmts);
    }

  /* Build and insert the assignment of the end result to the temporary
     that we will return.  */
  if (!pretemp || exprtype != TREE_TYPE (pretemp))
    {
      pretemp = create_tmp_var (exprtype, "pretmp");
      get_var_ann (pretemp);
    }

  temp = pretemp;
  add_referenced_var (temp);

  if (TREE_CODE (exprtype) == COMPLEX_TYPE
      || TREE_CODE (exprtype) == VECTOR_TYPE)
    DECL_GIMPLE_REG_P (temp) = 1;

  newstmt = gimple_build_assign (temp, folded);
  name = make_ssa_name (temp, newstmt);
  gimple_assign_set_lhs (newstmt, name);
  gimple_set_plf (newstmt, NECESSARY, false);

  gimple_seq_add_stmt (stmts, newstmt);
  VEC_safe_push (gimple, heap, inserted_exprs, newstmt);

  /* All the symbols in NEWEXPR should be put into SSA form.  */
  mark_symbols_for_renaming (newstmt);

  /* Add a value number to the temporary.
     The value may already exist in either NEW_SETS, or AVAIL_OUT, because
     we are creating the expression by pieces, and this particular piece of
     the expression may have been represented.  There is no harm in replacing
     here.  */
  VN_INFO_GET (name)->valnum = name;
  value_id = get_expr_value_id (expr);
  VN_INFO (name)->value_id = value_id;
  nameexpr = get_or_alloc_expr_for_name (name);
  add_to_value (value_id, nameexpr);
  if (!in_fre)
    bitmap_value_replace_in_set (NEW_SETS (block), nameexpr);
  bitmap_value_replace_in_set (AVAIL_OUT (block), nameexpr);

  pre_stats.insertions++;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Inserted ");
      print_gimple_stmt (dump_file, newstmt, 0, 0);
      fprintf (dump_file, " in predecessor %d\n", block->index);
    }

  return name;
}


/* Returns true if we want to inhibit the insertions of PHI nodes
   for the given EXPR for basic block BB (a member of a loop).
   We want to do this, when we fear that the induction variable we
   create might inhibit vectorization.  */

static bool
inhibit_phi_insertion (basic_block bb, pre_expr expr)
{
  vn_reference_t vr = PRE_EXPR_REFERENCE (expr);
  VEC (vn_reference_op_s, heap) *ops = vr->operands;
  vn_reference_op_t op;
  unsigned i;

  /* If we aren't going to vectorize we don't inhibit anything.  */
  if (!flag_tree_vectorize)
    return false;

  /* Otherwise we inhibit the insertion when the address of the
     memory reference is a simple induction variable.  In other
     cases the vectorizer won't do anything anyway (either it's
     loop invariant or a complicated expression).  */
  for (i = 0; VEC_iterate (vn_reference_op_s, ops, i, op); ++i)
    {
      switch (op->opcode)
	{
	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  if (TREE_CODE (op->op0) != SSA_NAME)
	    break;
	  /* Fallthru.  */
	case SSA_NAME:
	  {
	    basic_block defbb = gimple_bb (SSA_NAME_DEF_STMT (op->op0));
	    affine_iv iv;
	    /* Default defs are loop invariant.  */
	    if (!defbb)
	      break;
	    /* Defined outside this loop, also loop invariant.  */
	    if (!flow_bb_inside_loop_p (bb->loop_father, defbb))
	      break;
	    /* If it's a simple induction variable inhibit insertion,
	       the vectorizer might be interested in this one.  */
	    if (simple_iv (bb->loop_father, bb->loop_father,
			   op->op0, &iv, true))
	      return true;
	    /* No simple IV, vectorizer can't do anything, hence no
	       reason to inhibit the transformation for this operand.  */
	    break;
	  }
	default:
	  break;
	}
    }
  return false;
}

/* Insert the to-be-made-available values of expression EXPRNUM for each
   predecessor, stored in AVAIL, into the predecessors of BLOCK, and
   merge the result with a phi node, given the same value number as
   NODE.  Return true if we have inserted new stuff.  */

static bool
insert_into_preds_of_block (basic_block block, unsigned int exprnum,
			    pre_expr *avail)
{
  pre_expr expr = expression_for_id (exprnum);
  pre_expr newphi;
  unsigned int val = get_expr_value_id (expr);
  edge pred;
  bool insertions = false;
  bool nophi = false;
  basic_block bprime;
  pre_expr eprime;
  edge_iterator ei;
  tree type = get_expr_type (expr);
  tree temp;
  gimple phi;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Found partial redundancy for expression ");
      print_pre_expr (dump_file, expr);
      fprintf (dump_file, " (%04d)\n", val);
    }

  /* Make sure we aren't creating an induction variable.  */
  if (block->loop_depth > 0 && EDGE_COUNT (block->preds) == 2)
    {
      bool firstinsideloop = false;
      bool secondinsideloop = false;
      firstinsideloop = flow_bb_inside_loop_p (block->loop_father,
					       EDGE_PRED (block, 0)->src);
      secondinsideloop = flow_bb_inside_loop_p (block->loop_father,
						EDGE_PRED (block, 1)->src);
      /* Induction variables only have one edge inside the loop.  */
      if ((firstinsideloop ^ secondinsideloop)
	  && (expr->kind != REFERENCE
	      || inhibit_phi_insertion (block, expr)))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Skipping insertion of phi for partial redundancy: Looks like an induction variable\n");
	  nophi = true;
	}
    }

  /* Make sure we are not inserting trapping expressions.  */
  FOR_EACH_EDGE (pred, ei, block->preds)
    {
      bprime = pred->src;
      eprime = avail[bprime->index];
      if (eprime->kind == NARY
	  && vn_nary_may_trap (PRE_EXPR_NARY (eprime)))
	return false;
    }

  /* Make the necessary insertions.  */
  FOR_EACH_EDGE (pred, ei, block->preds)
    {
      gimple_seq stmts = NULL;
      tree builtexpr;
      bprime = pred->src;
      eprime = avail[bprime->index];

      if (eprime->kind != NAME && eprime->kind != CONSTANT)
	{
	  builtexpr = create_expression_by_pieces (bprime,
						   eprime,
						   &stmts, NULL,
						   type);
	  gcc_assert (!(pred->flags & EDGE_ABNORMAL));
	  gsi_insert_seq_on_edge (pred, stmts);
	  avail[bprime->index] = get_or_alloc_expr_for_name (builtexpr);
	  insertions = true;
	}
      else if (eprime->kind == CONSTANT)
	{
	  /* Constants may not have the right type, fold_convert
	     should give us back a constant with the right type.
	  */
	  tree constant = PRE_EXPR_CONSTANT (eprime);
	  if (!useless_type_conversion_p (type, TREE_TYPE (constant)))
	    {
	      tree builtexpr = fold_convert (type, constant);
	      if (!is_gimple_min_invariant (builtexpr))
		{
		  tree forcedexpr = force_gimple_operand (builtexpr,
							  &stmts, true,
							  NULL);
		  if (!is_gimple_min_invariant (forcedexpr))
		    {
		      if (forcedexpr != builtexpr)
			{
			  VN_INFO_GET (forcedexpr)->valnum = PRE_EXPR_CONSTANT (eprime);
			  VN_INFO (forcedexpr)->value_id = get_expr_value_id (eprime);
			}
		      if (stmts)
			{
			  gimple_stmt_iterator gsi;
			  gsi = gsi_start (stmts);
			  for (; !gsi_end_p (gsi); gsi_next (&gsi))
			    {
			      gimple stmt = gsi_stmt (gsi);
			      VEC_safe_push (gimple, heap, inserted_exprs, stmt);
			      gimple_set_plf (stmt, NECESSARY, false);
			    }
			  gsi_insert_seq_on_edge (pred, stmts);
			}
		      avail[bprime->index] = get_or_alloc_expr_for_name (forcedexpr);
		    }
		}
	    }
	}
      else if (eprime->kind == NAME)
	{
	  /* We may have to do a conversion because our value
	     numbering can look through types in certain cases, but
	     our IL requires all operands of a phi node have the same
	     type.  */
	  tree name = PRE_EXPR_NAME (eprime);
	  if (!useless_type_conversion_p (type, TREE_TYPE (name)))
	    {
	      tree builtexpr;
	      tree forcedexpr;
	      builtexpr = fold_convert (type, name);
	      forcedexpr = force_gimple_operand (builtexpr,
						 &stmts, true,
						 NULL);

	      if (forcedexpr != name)
		{
		  VN_INFO_GET (forcedexpr)->valnum = VN_INFO (name)->valnum;
		  VN_INFO (forcedexpr)->value_id = VN_INFO (name)->value_id;
		}

	      if (stmts)
		{
		  gimple_stmt_iterator gsi;
		  gsi = gsi_start (stmts);
		  for (; !gsi_end_p (gsi); gsi_next (&gsi))
		    {
		      gimple stmt = gsi_stmt (gsi);
		      VEC_safe_push (gimple, heap, inserted_exprs, stmt);
		      gimple_set_plf (stmt, NECESSARY, false);
		    }
		  gsi_insert_seq_on_edge (pred, stmts);
		}
	      avail[bprime->index] = get_or_alloc_expr_for_name (forcedexpr);
	    }
	}
    }
  /* If we didn't want a phi node, and we made insertions, we still have
     inserted new stuff, and thus return true.  If we didn't want a phi node,
     and didn't make insertions, we haven't added anything new, so return
     false.  */
  if (nophi && insertions)
    return true;
  else if (nophi && !insertions)
    return false;

  /* Now build a phi for the new variable.  */
  if (!prephitemp || TREE_TYPE (prephitemp) != type)
    {
      prephitemp = create_tmp_var (type, "prephitmp");
      get_var_ann (prephitemp);
    }

  temp = prephitemp;
  add_referenced_var (temp);

  if (TREE_CODE (type) == COMPLEX_TYPE
      || TREE_CODE (type) == VECTOR_TYPE)
    DECL_GIMPLE_REG_P (temp) = 1;
  phi = create_phi_node (temp, block);

  gimple_set_plf (phi, NECESSARY, false);
  VN_INFO_GET (gimple_phi_result (phi))->valnum = gimple_phi_result (phi);
  VN_INFO (gimple_phi_result (phi))->value_id = val;
  VEC_safe_push (gimple, heap, inserted_exprs, phi);
  bitmap_set_bit (inserted_phi_names,
		  SSA_NAME_VERSION (gimple_phi_result (phi)));
  FOR_EACH_EDGE (pred, ei, block->preds)
    {
      pre_expr ae = avail[pred->src->index];
      gcc_assert (get_expr_type (ae) == type
		  || useless_type_conversion_p (type, get_expr_type (ae)));
      if (ae->kind == CONSTANT)
	add_phi_arg (phi, PRE_EXPR_CONSTANT (ae), pred, UNKNOWN_LOCATION);
      else
	add_phi_arg (phi, PRE_EXPR_NAME (avail[pred->src->index]), pred,
		     UNKNOWN_LOCATION);
    }

  newphi = get_or_alloc_expr_for_name (gimple_phi_result (phi));
  add_to_value (val, newphi);

  /* The value should *not* exist in PHI_GEN, or else we wouldn't be doing
     this insertion, since we test for the existence of this value in PHI_GEN
     before proceeding with the partial redundancy checks in insert_aux.

     The value may exist in AVAIL_OUT, in particular, it could be represented
     by the expression we are trying to eliminate, in which case we want the
     replacement to occur.  If it's not existing in AVAIL_OUT, we want it
     inserted there.

     Similarly, to the PHI_GEN case, the value should not exist in NEW_SETS of
     this block, because if it did, it would have existed in our dominator's
     AVAIL_OUT, and would have been skipped due to the full redundancy check.
  */

  bitmap_insert_into_set (PHI_GEN (block), newphi);
  bitmap_value_replace_in_set (AVAIL_OUT (block),
			       newphi);
  bitmap_insert_into_set (NEW_SETS (block),
			  newphi);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Created phi ");
      print_gimple_stmt (dump_file, phi, 0, 0);
      fprintf (dump_file, " in block %d\n", block->index);
    }
  pre_stats.phis++;
  return true;
}



/* Perform insertion of partially redundant values.
   For BLOCK, do the following:
   1.  Propagate the NEW_SETS of the dominator into the current block.
   If the block has multiple predecessors,
       2a. Iterate over the ANTIC expressions for the block to see if
	   any of them are partially redundant.
       2b. If so, insert them into the necessary predecessors to make
	   the expression fully redundant.
       2c. Insert a new PHI merging the values of the predecessors.
       2d. Insert the new PHI, and the new expressions, into the
	   NEW_SETS set.
   3. Recursively call ourselves on the dominator children of BLOCK.

   Steps 1, 2a, and 3 are done by insert_aux. 2b, 2c and 2d are done by
   do_regular_insertion and do_partial_insertion.

*/

static bool
do_regular_insertion (basic_block block, basic_block dom)
{
  bool new_stuff = false;
  VEC (pre_expr, heap) *exprs = sorted_array_from_bitmap_set (ANTIC_IN (block));
  pre_expr expr;
  int i;

  for (i = 0; VEC_iterate (pre_expr, exprs, i, expr); i++)
    {
      if (expr->kind != NAME)
	{
	  pre_expr *avail;
	  unsigned int val;
	  bool by_some = false;
	  bool cant_insert = false;
	  bool all_same = true;
	  pre_expr first_s = NULL;
	  edge pred;
	  basic_block bprime;
	  pre_expr eprime = NULL;
	  edge_iterator ei;
	  pre_expr edoubleprime = NULL;
	  bool do_insertion = false;

	  val = get_expr_value_id (expr);
	  if (bitmap_set_contains_value (PHI_GEN (block), val))
	    continue;
	  if (bitmap_set_contains_value (AVAIL_OUT (dom), val))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Found fully redundant value\n");
	      continue;
	    }

	  avail = XCNEWVEC (pre_expr, last_basic_block);
	  FOR_EACH_EDGE (pred, ei, block->preds)
	    {
	      unsigned int vprime;

	      /* We should never run insertion for the exit block
	         and so not come across fake pred edges.  */
	      gcc_assert (!(pred->flags & EDGE_FAKE));
	      bprime = pred->src;
	      eprime = phi_translate (expr, ANTIC_IN (block), NULL,
				      bprime, block);

	      /* eprime will generally only be NULL if the
		 value of the expression, translated
		 through the PHI for this predecessor, is
		 undefined.  If that is the case, we can't
		 make the expression fully redundant,
		 because its value is undefined along a
		 predecessor path.  We can thus break out
		 early because it doesn't matter what the
		 rest of the results are.  */
	      if (eprime == NULL)
		{
		  cant_insert = true;
		  break;
		}

	      eprime = fully_constant_expression (eprime);
	      vprime = get_expr_value_id (eprime);
	      edoubleprime = bitmap_find_leader (AVAIL_OUT (bprime),
						 vprime, NULL);
	      if (edoubleprime == NULL)
		{
		  avail[bprime->index] = eprime;
		  all_same = false;
		}
	      else
		{
		  avail[bprime->index] = edoubleprime;
		  by_some = true;
		  /* We want to perform insertions to remove a redundancy on
		     a path in the CFG we want to optimize for speed.  */
		  if (optimize_edge_for_speed_p (pred))
		    do_insertion = true;
		  if (first_s == NULL)
		    first_s = edoubleprime;
		  else if (!pre_expr_eq (first_s, edoubleprime))
		    all_same = false;
		}
	    }
	  /* If we can insert it, it's not the same value
	     already existing along every predecessor, and
	     it's defined by some predecessor, it is
	     partially redundant.  */
	  if (!cant_insert && !all_same && by_some && do_insertion
	      && dbg_cnt (treepre_insert))
	    {
	      if (insert_into_preds_of_block (block, get_expression_id (expr),
					      avail))
		new_stuff = true;
	    }
	  /* If all edges produce the same value and that value is
	     an invariant, then the PHI has the same value on all
	     edges.  Note this.  */
	  else if (!cant_insert && all_same && eprime
		   && (edoubleprime->kind == CONSTANT
		       || edoubleprime->kind == NAME)
		   && !value_id_constant_p (val))
	    {
	      unsigned int j;
	      bitmap_iterator bi;
	      bitmap_set_t exprset = VEC_index (bitmap_set_t,
						value_expressions, val);

	      unsigned int new_val = get_expr_value_id (edoubleprime);
	      FOR_EACH_EXPR_ID_IN_SET (exprset, j, bi)
		{
		  pre_expr expr = expression_for_id (j);

		  if (expr->kind == NAME)
		    {
		      vn_ssa_aux_t info = VN_INFO (PRE_EXPR_NAME (expr));
		      /* Just reset the value id and valnum so it is
			 the same as the constant we have discovered.  */
		      if (edoubleprime->kind == CONSTANT)
			{
			  info->valnum = PRE_EXPR_CONSTANT (edoubleprime);
			  pre_stats.constified++;
			}
		      else
			info->valnum = VN_INFO (PRE_EXPR_NAME (edoubleprime))->valnum;
		      info->value_id = new_val;
		    }
		}
	    }
	  free (avail);
	}
    }

  VEC_free (pre_expr, heap, exprs);
  return new_stuff;
}


/* Perform insertion for partially anticipatable expressions.  There
   is only one case we will perform insertion for these.  This case is
   if the expression is partially anticipatable, and fully available.
   In this case, we know that putting it earlier will enable us to
   remove the later computation.  */


static bool
do_partial_partial_insertion (basic_block block, basic_block dom)
{
  bool new_stuff = false;
  VEC (pre_expr, heap) *exprs = sorted_array_from_bitmap_set (PA_IN (block));
  pre_expr expr;
  int i;

  for (i = 0; VEC_iterate (pre_expr, exprs, i, expr); i++)
    {
      if (expr->kind != NAME)
	{
	  pre_expr *avail;
	  unsigned int val;
	  bool by_all = true;
	  bool cant_insert = false;
	  edge pred;
	  basic_block bprime;
	  pre_expr eprime = NULL;
	  edge_iterator ei;

	  val = get_expr_value_id (expr);
	  if (bitmap_set_contains_value (PHI_GEN (block), val))
	    continue;
	  if (bitmap_set_contains_value (AVAIL_OUT (dom), val))
	    continue;

	  avail = XCNEWVEC (pre_expr, last_basic_block);
	  FOR_EACH_EDGE (pred, ei, block->preds)
	    {
	      unsigned int vprime;
	      pre_expr edoubleprime;

	      /* We should never run insertion for the exit block
	         and so not come across fake pred edges.  */
	      gcc_assert (!(pred->flags & EDGE_FAKE));
	      bprime = pred->src;
	      eprime = phi_translate (expr, ANTIC_IN (block),
				      PA_IN (block),
				      bprime, block);

	      /* eprime will generally only be NULL if the
		 value of the expression, translated
		 through the PHI for this predecessor, is
		 undefined.  If that is the case, we can't
		 make the expression fully redundant,
		 because its value is undefined along a
		 predecessor path.  We can thus break out
		 early because it doesn't matter what the
		 rest of the results are.  */
	      if (eprime == NULL)
		{
		  cant_insert = true;
		  break;
		}

	      eprime = fully_constant_expression (eprime);
	      vprime = get_expr_value_id (eprime);
	      edoubleprime = bitmap_find_leader (AVAIL_OUT (bprime),
						 vprime, NULL);
	      if (edoubleprime == NULL)
		{
		  by_all = false;
		  break;
		}
	      else
		avail[bprime->index] = edoubleprime;

	    }

	  /* If we can insert it, it's not the same value
	     already existing along every predecessor, and
	     it's defined by some predecessor, it is
	     partially redundant.  */
	  if (!cant_insert && by_all && dbg_cnt (treepre_insert))
	    {
	      pre_stats.pa_insert++;
	      if (insert_into_preds_of_block (block, get_expression_id (expr),
					      avail))
		new_stuff = true;
	    }
	  free (avail);
	}
    }

  VEC_free (pre_expr, heap, exprs);
  return new_stuff;
}

static bool
insert_aux (basic_block block)
{
  basic_block son;
  bool new_stuff = false;

  if (block)
    {
      basic_block dom;
      dom = get_immediate_dominator (CDI_DOMINATORS, block);
      if (dom)
	{
	  unsigned i;
	  bitmap_iterator bi;
	  bitmap_set_t newset = NEW_SETS (dom);
	  if (newset)
	    {
	      /* Note that we need to value_replace both NEW_SETS, and
		 AVAIL_OUT. For both the case of NEW_SETS, the value may be
		 represented by some non-simple expression here that we want
		 to replace it with.  */
	      FOR_EACH_EXPR_ID_IN_SET (newset, i, bi)
		{
		  pre_expr expr = expression_for_id (i);
		  bitmap_value_replace_in_set (NEW_SETS (block), expr);
		  bitmap_value_replace_in_set (AVAIL_OUT (block), expr);
		}
	    }
	  if (!single_pred_p (block))
	    {
	      new_stuff |= do_regular_insertion (block, dom);
	      if (do_partial_partial)
		new_stuff |= do_partial_partial_insertion (block, dom);
	    }
	}
    }
  for (son = first_dom_son (CDI_DOMINATORS, block);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    {
      new_stuff |= insert_aux (son);
    }

  return new_stuff;
}

/* Perform insertion of partially redundant values.  */

static void
insert (void)
{
  bool new_stuff = true;
  basic_block bb;
  int num_iterations = 0;

  FOR_ALL_BB (bb)
    NEW_SETS (bb) = bitmap_set_new ();

  while (new_stuff)
    {
      num_iterations++;
      new_stuff = insert_aux (ENTRY_BLOCK_PTR);
    }
  statistics_histogram_event (cfun, "insert iterations", num_iterations);
}


/* Add OP to EXP_GEN (block), and possibly to the maximal set.  */

static void
add_to_exp_gen (basic_block block, tree op)
{
  if (!in_fre)
    {
      pre_expr result;
      if (TREE_CODE (op) == SSA_NAME && ssa_undefined_value_p (op))
	return;
      result = get_or_alloc_expr_for_name (op);
      bitmap_value_insert_into_set (EXP_GEN (block), result);
    }
}

/* Create value ids for PHI in BLOCK.  */

static void
make_values_for_phi (gimple phi, basic_block block)
{
  tree result = gimple_phi_result (phi);

  /* We have no need for virtual phis, as they don't represent
     actual computations.  */
  if (is_gimple_reg (result))
    {
      pre_expr e = get_or_alloc_expr_for_name (result);
      add_to_value (get_expr_value_id (e), e);
      bitmap_insert_into_set (PHI_GEN (block), e);
      bitmap_value_insert_into_set (AVAIL_OUT (block), e);
      if (!in_fre)
	{
	  unsigned i;
	  for (i = 0; i < gimple_phi_num_args (phi); ++i)
	    {
	      tree arg = gimple_phi_arg_def (phi, i);
	      if (TREE_CODE (arg) == SSA_NAME)
		{
		  e = get_or_alloc_expr_for_name (arg);
		  add_to_value (get_expr_value_id (e), e);
		}
	    }
	}
    }
}

/* Compute the AVAIL set for all basic blocks.

   This function performs value numbering of the statements in each basic
   block.  The AVAIL sets are built from information we glean while doing
   this value numbering, since the AVAIL sets contain only one entry per
   value.

   AVAIL_IN[BLOCK] = AVAIL_OUT[dom(BLOCK)].
   AVAIL_OUT[BLOCK] = AVAIL_IN[BLOCK] U PHI_GEN[BLOCK] U TMP_GEN[BLOCK].  */

static void
compute_avail (void)
{

  basic_block block, son;
  basic_block *worklist;
  size_t sp = 0;
  unsigned i;

  /* We pretend that default definitions are defined in the entry block.
     This includes function arguments and the static chain decl.  */
  for (i = 1; i < num_ssa_names; ++i)
    {
      tree name = ssa_name (i);
      pre_expr e;
      if (!name
	  || !SSA_NAME_IS_DEFAULT_DEF (name)
	  || has_zero_uses (name)
	  || !is_gimple_reg (name))
	continue;

      e = get_or_alloc_expr_for_name (name);
      add_to_value (get_expr_value_id (e), e);
      if (!in_fre)
	bitmap_insert_into_set (TMP_GEN (ENTRY_BLOCK_PTR), e);
      bitmap_value_insert_into_set (AVAIL_OUT (ENTRY_BLOCK_PTR), e);
    }

  /* Allocate the worklist.  */
  worklist = XNEWVEC (basic_block, n_basic_blocks);

  /* Seed the algorithm by putting the dominator children of the entry
     block on the worklist.  */
  for (son = first_dom_son (CDI_DOMINATORS, ENTRY_BLOCK_PTR);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    worklist[sp++] = son;

  /* Loop until the worklist is empty.  */
  while (sp)
    {
      gimple_stmt_iterator gsi;
      gimple stmt;
      basic_block dom;
      unsigned int stmt_uid = 1;

      /* Pick a block from the worklist.  */
      block = worklist[--sp];

      /* Initially, the set of available values in BLOCK is that of
	 its immediate dominator.  */
      dom = get_immediate_dominator (CDI_DOMINATORS, block);
      if (dom)
	bitmap_set_copy (AVAIL_OUT (block), AVAIL_OUT (dom));

      /* Generate values for PHI nodes.  */
      for (gsi = gsi_start_phis (block); !gsi_end_p (gsi); gsi_next (&gsi))
	make_values_for_phi (gsi_stmt (gsi), block);

      /* Now compute value numbers and populate value sets with all
	 the expressions computed in BLOCK.  */
      for (gsi = gsi_start_bb (block); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  ssa_op_iter iter;
	  tree op;

	  stmt = gsi_stmt (gsi);
	  gimple_set_uid (stmt, stmt_uid++);

	  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_DEF)
	    {
	      pre_expr e = get_or_alloc_expr_for_name (op);

	      add_to_value (get_expr_value_id (e), e);
	      if (!in_fre)
		bitmap_insert_into_set (TMP_GEN (block), e);
	      bitmap_value_insert_into_set (AVAIL_OUT (block), e);
	    }

	  if (gimple_has_volatile_ops (stmt)
	      || stmt_could_throw_p (stmt))
	    continue;

	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_RETURN:
	      FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE)
		add_to_exp_gen (block, op);
	      continue;

	    case GIMPLE_CALL:
	      {
		vn_reference_t ref;
		unsigned int i;
		vn_reference_op_t vro;
		pre_expr result = NULL;
		VEC(vn_reference_op_s, heap) *ops = NULL;

		if (!can_value_number_call (stmt))
		  continue;

		copy_reference_ops_from_call (stmt, &ops);
		vn_reference_lookup_pieces (gimple_vuse (stmt), 0,
					    gimple_expr_type (stmt),
					    ops, &ref, false);
		VEC_free (vn_reference_op_s, heap, ops);
		if (!ref)
		  continue;

		for (i = 0; VEC_iterate (vn_reference_op_s,
					 ref->operands, i,
					 vro); i++)
		  {
		    if (vro->op0 && TREE_CODE (vro->op0) == SSA_NAME)
		      add_to_exp_gen (block, vro->op0);
		    if (vro->op1 && TREE_CODE (vro->op1) == SSA_NAME)
		      add_to_exp_gen (block, vro->op1);
		    if (vro->op2 && TREE_CODE (vro->op2) == SSA_NAME)
		      add_to_exp_gen (block, vro->op2);
		  }
		result = (pre_expr) pool_alloc (pre_expr_pool);
		result->kind = REFERENCE;
		result->id = 0;
		PRE_EXPR_REFERENCE (result) = ref;

		get_or_alloc_expression_id (result);
		add_to_value (get_expr_value_id (result), result);
		if (!in_fre)
		  bitmap_value_insert_into_set (EXP_GEN (block), result);
		continue;
	      }

	    case GIMPLE_ASSIGN:
	      {
		pre_expr result = NULL;
		switch (TREE_CODE_CLASS (gimple_assign_rhs_code (stmt)))
		  {
		  case tcc_unary:
		  case tcc_binary:
		  case tcc_comparison:
		    {
		      vn_nary_op_t nary;
		      unsigned int i;

		      vn_nary_op_lookup_pieces (gimple_num_ops (stmt) - 1,
						gimple_assign_rhs_code (stmt),
						gimple_expr_type (stmt),
						gimple_assign_rhs1 (stmt),
						gimple_assign_rhs2 (stmt),
						NULL_TREE, NULL_TREE, &nary);

		      if (!nary)
			continue;

		      for (i = 0; i < nary->length; i++)
			if (TREE_CODE (nary->op[i]) == SSA_NAME)
			  add_to_exp_gen (block, nary->op[i]);

		      result = (pre_expr) pool_alloc (pre_expr_pool);
		      result->kind = NARY;
		      result->id = 0;
		      PRE_EXPR_NARY (result) = nary;
		      break;
		    }

		  case tcc_declaration:
		  case tcc_reference:
		    {
		      vn_reference_t ref;
		      unsigned int i;
		      vn_reference_op_t vro;

		      vn_reference_lookup (gimple_assign_rhs1 (stmt),
					   gimple_vuse (stmt),
					   true, &ref);
		      if (!ref)
			continue;

		      for (i = 0; VEC_iterate (vn_reference_op_s,
					       ref->operands, i,
					       vro); i++)
			{
			  if (vro->op0 && TREE_CODE (vro->op0) == SSA_NAME)
			    add_to_exp_gen (block, vro->op0);
			  if (vro->op1 && TREE_CODE (vro->op1) == SSA_NAME)
			    add_to_exp_gen (block, vro->op1);
			  if (vro->op2 && TREE_CODE (vro->op2) == SSA_NAME)
			    add_to_exp_gen (block, vro->op2);
			}
		      result = (pre_expr) pool_alloc (pre_expr_pool);
		      result->kind = REFERENCE;
		      result->id = 0;
		      PRE_EXPR_REFERENCE (result) = ref;
		      break;
		    }

		  default:
		    /* For any other statement that we don't
		       recognize, simply add all referenced
		       SSA_NAMEs to EXP_GEN.  */
		    FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE)
		      add_to_exp_gen (block, op);
		    continue;
		  }

		get_or_alloc_expression_id (result);
		add_to_value (get_expr_value_id (result), result);
		if (!in_fre)
		  bitmap_value_insert_into_set (EXP_GEN (block), result);

		continue;
	      }
	    default:
	      break;
	    }
	}

      /* Put the dominator children of BLOCK on the worklist of blocks
	 to compute available sets for.  */
      for (son = first_dom_son (CDI_DOMINATORS, block);
	   son;
	   son = next_dom_son (CDI_DOMINATORS, son))
	worklist[sp++] = son;
    }

  free (worklist);
}

/* Insert the expression for SSA_VN that SCCVN thought would be simpler
   than the available expressions for it.  The insertion point is
   right before the first use in STMT.  Returns the SSA_NAME that should
   be used for replacement.  */

static tree
do_SCCVN_insertion (gimple stmt, tree ssa_vn)
{
  basic_block bb = gimple_bb (stmt);
  gimple_stmt_iterator gsi;
  gimple_seq stmts = NULL;
  tree expr;
  pre_expr e;

  /* First create a value expression from the expression we want
     to insert and associate it with the value handle for SSA_VN.  */
  e = get_or_alloc_expr_for (vn_get_expr_for (ssa_vn));
  if (e == NULL)
    return NULL_TREE;

  /* Then use create_expression_by_pieces to generate a valid
     expression to insert at this point of the IL stream.  */
  expr = create_expression_by_pieces (bb, e, &stmts, stmt, NULL);
  if (expr == NULL_TREE)
    return NULL_TREE;
  gsi = gsi_for_stmt (stmt);
  gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);

  return expr;
}

/* Eliminate fully redundant computations.  */

static unsigned int
eliminate (void)
{
  VEC (gimple, heap) *to_remove = NULL;
  basic_block b;
  unsigned int todo = 0;
  gimple_stmt_iterator gsi;
  gimple stmt;
  unsigned i;

  FOR_EACH_BB (b)
    {
      for (gsi = gsi_start_bb (b); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  stmt = gsi_stmt (gsi);

	  /* Lookup the RHS of the expression, see if we have an
	     available computation for it.  If so, replace the RHS with
	     the available computation.  */
	  if (gimple_has_lhs (stmt)
	      && TREE_CODE (gimple_get_lhs (stmt)) == SSA_NAME
	      && !gimple_assign_ssa_name_copy_p (stmt)
	      && (!gimple_assign_single_p (stmt)
		  || !is_gimple_min_invariant (gimple_assign_rhs1 (stmt)))
	      && !gimple_has_volatile_ops  (stmt)
	      && !has_zero_uses (gimple_get_lhs (stmt)))
	    {
	      tree lhs = gimple_get_lhs (stmt);
	      tree rhs = NULL_TREE;
	      tree sprime = NULL;
	      pre_expr lhsexpr = get_or_alloc_expr_for_name (lhs);
	      pre_expr sprimeexpr;

	      if (gimple_assign_single_p (stmt))
		rhs = gimple_assign_rhs1 (stmt);

	      sprimeexpr = bitmap_find_leader (AVAIL_OUT (b),
					       get_expr_value_id (lhsexpr),
					       NULL);

	      if (sprimeexpr)
		{
		  if (sprimeexpr->kind == CONSTANT)
		    sprime = PRE_EXPR_CONSTANT (sprimeexpr);
		  else if (sprimeexpr->kind == NAME)
		    sprime = PRE_EXPR_NAME (sprimeexpr);
		  else
		    gcc_unreachable ();
		}

	      /* If there is no existing leader but SCCVN knows this
		 value is constant, use that constant.  */
	      if (!sprime && is_gimple_min_invariant (VN_INFO (lhs)->valnum))
		{
		  sprime = VN_INFO (lhs)->valnum;
		  if (!useless_type_conversion_p (TREE_TYPE (lhs),
						  TREE_TYPE (sprime)))
		    sprime = fold_convert (TREE_TYPE (lhs), sprime);

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Replaced ");
		      print_gimple_expr (dump_file, stmt, 0, 0);
		      fprintf (dump_file, " with ");
		      print_generic_expr (dump_file, sprime, 0);
		      fprintf (dump_file, " in ");
		      print_gimple_stmt (dump_file, stmt, 0, 0);
		    }
		  pre_stats.eliminations++;
		  propagate_tree_value_into_stmt (&gsi, sprime);
		  stmt = gsi_stmt (gsi);
		  update_stmt (stmt);
		  continue;
		}

	      /* If there is no existing usable leader but SCCVN thinks
		 it has an expression it wants to use as replacement,
		 insert that.  */
	      if (!sprime || sprime == lhs)
		{
		  tree val = VN_INFO (lhs)->valnum;
		  if (val != VN_TOP
		      && TREE_CODE (val) == SSA_NAME
		      && VN_INFO (val)->needs_insertion
		      && can_PRE_operation (vn_get_expr_for (val)))
		    sprime = do_SCCVN_insertion (stmt, val);
		}
	      if (sprime
		  && sprime != lhs
		  && (rhs == NULL_TREE
		      || TREE_CODE (rhs) != SSA_NAME
		      || may_propagate_copy (rhs, sprime)))
		{
		  gcc_assert (sprime != rhs);

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Replaced ");
		      print_gimple_expr (dump_file, stmt, 0, 0);
		      fprintf (dump_file, " with ");
		      print_generic_expr (dump_file, sprime, 0);
		      fprintf (dump_file, " in ");
		      print_gimple_stmt (dump_file, stmt, 0, 0);
		    }

		  if (TREE_CODE (sprime) == SSA_NAME)
		    gimple_set_plf (SSA_NAME_DEF_STMT (sprime),
				    NECESSARY, true);
		  /* We need to make sure the new and old types actually match,
		     which may require adding a simple cast, which fold_convert
		     will do for us.  */
		  if ((!rhs || TREE_CODE (rhs) != SSA_NAME)
		      && !useless_type_conversion_p (gimple_expr_type (stmt),
						     TREE_TYPE (sprime)))
		    sprime = fold_convert (gimple_expr_type (stmt), sprime);

		  pre_stats.eliminations++;
		  propagate_tree_value_into_stmt (&gsi, sprime);
		  stmt = gsi_stmt (gsi);
		  update_stmt (stmt);

		  /* If we removed EH side effects from the statement, clean
		     its EH information.  */
		  if (maybe_clean_or_replace_eh_stmt (stmt, stmt))
		    {
		      bitmap_set_bit (need_eh_cleanup,
				      gimple_bb (stmt)->index);
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file, "  Removed EH side effects.\n");
		    }
		}
	    }
	  /* If the statement is a scalar store, see if the expression
	     has the same value number as its rhs.  If so, the store is
	     dead.  */
	  else if (gimple_assign_single_p (stmt)
		   && !is_gimple_reg (gimple_assign_lhs (stmt))
		   && (TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
		       || is_gimple_min_invariant (gimple_assign_rhs1 (stmt))))
	    {
	      tree rhs = gimple_assign_rhs1 (stmt);
	      tree val;
	      val = vn_reference_lookup (gimple_assign_lhs (stmt),
					 gimple_vuse (stmt), true, NULL);
	      if (TREE_CODE (rhs) == SSA_NAME)
		rhs = VN_INFO (rhs)->valnum;
	      if (val
		  && operand_equal_p (val, rhs, 0))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Deleted redundant store ");
		      print_gimple_stmt (dump_file, stmt, 0, 0);
		    }

		  /* Queue stmt for removal.  */
		  VEC_safe_push (gimple, heap, to_remove, stmt);
		}
	    }
	  /* Visit COND_EXPRs and fold the comparison with the
	     available value-numbers.  */
	  else if (gimple_code (stmt) == GIMPLE_COND)
	    {
	      tree op0 = gimple_cond_lhs (stmt);
	      tree op1 = gimple_cond_rhs (stmt);
	      tree result;

	      if (TREE_CODE (op0) == SSA_NAME)
		op0 = VN_INFO (op0)->valnum;
	      if (TREE_CODE (op1) == SSA_NAME)
		op1 = VN_INFO (op1)->valnum;
	      result = fold_binary (gimple_cond_code (stmt), boolean_type_node,
				    op0, op1);
	      if (result && TREE_CODE (result) == INTEGER_CST)
		{
		  if (integer_zerop (result))
		    gimple_cond_make_false (stmt);
		  else
		    gimple_cond_make_true (stmt);
		  update_stmt (stmt);
		  todo = TODO_cleanup_cfg;
		}
	    }
	  /* Visit indirect calls and turn them into direct calls if
	     possible.  */
	  if (gimple_code (stmt) == GIMPLE_CALL
	      && TREE_CODE (gimple_call_fn (stmt)) == SSA_NAME)
	    {
	      tree fn = VN_INFO (gimple_call_fn (stmt))->valnum;
	      if (TREE_CODE (fn) == ADDR_EXPR
		  && TREE_CODE (TREE_OPERAND (fn, 0)) == FUNCTION_DECL)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Replacing call target with ");
		      print_generic_expr (dump_file, fn, 0);
		      fprintf (dump_file, " in ");
		      print_gimple_stmt (dump_file, stmt, 0, 0);
		    }

		  gimple_call_set_fn (stmt, fn);
		  update_stmt (stmt);
		  if (maybe_clean_or_replace_eh_stmt (stmt, stmt))
		    {
		      bitmap_set_bit (need_eh_cleanup,
				      gimple_bb (stmt)->index);
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file, "  Removed EH side effects.\n");
		    }

		  /* Changing an indirect call to a direct call may
		     have exposed different semantics.  This may
		     require an SSA update.  */
		  todo |= TODO_update_ssa_only_virtuals;
		}
	    }
	}

      for (gsi = gsi_start_phis (b); !gsi_end_p (gsi);)
	{
	  gimple stmt, phi = gsi_stmt (gsi);
	  tree sprime = NULL_TREE, res = PHI_RESULT (phi);
	  pre_expr sprimeexpr, resexpr;
	  gimple_stmt_iterator gsi2;

	  /* We want to perform redundant PHI elimination.  Do so by
	     replacing the PHI with a single copy if possible.
	     Do not touch inserted, single-argument or virtual PHIs.  */
	  if (gimple_phi_num_args (phi) == 1
	      || !is_gimple_reg (res)
	      || bitmap_bit_p (inserted_phi_names, SSA_NAME_VERSION (res)))
	    {
	      gsi_next (&gsi);
	      continue;
	    }

	  resexpr = get_or_alloc_expr_for_name (res);
	  sprimeexpr = bitmap_find_leader (AVAIL_OUT (b),
					   get_expr_value_id (resexpr), NULL);
	  if (sprimeexpr)
	    {
	      if (sprimeexpr->kind == CONSTANT)
		sprime = PRE_EXPR_CONSTANT (sprimeexpr);
	      else if (sprimeexpr->kind == NAME)
		sprime = PRE_EXPR_NAME (sprimeexpr);
	      else
		gcc_unreachable ();
	    }
	  if (!sprimeexpr
	      || sprime == res)
	    {
	      gsi_next (&gsi);
	      continue;
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Replaced redundant PHI node defining ");
	      print_generic_expr (dump_file, res, 0);
	      fprintf (dump_file, " with ");
	      print_generic_expr (dump_file, sprime, 0);
	      fprintf (dump_file, "\n");
	    }

	  remove_phi_node (&gsi, false);

	  if (!useless_type_conversion_p (TREE_TYPE (res), TREE_TYPE (sprime)))
	    sprime = fold_convert (TREE_TYPE (res), sprime);
	  stmt = gimple_build_assign (res, sprime);
	  SSA_NAME_DEF_STMT (res) = stmt;
	  if (TREE_CODE (sprime) == SSA_NAME)
	    gimple_set_plf (SSA_NAME_DEF_STMT (sprime),
			    NECESSARY, true);
	  gsi2 = gsi_after_labels (b);
	  gsi_insert_before (&gsi2, stmt, GSI_NEW_STMT);
	  /* Queue the copy for eventual removal.  */
	  VEC_safe_push (gimple, heap, to_remove, stmt);
	  pre_stats.eliminations++;
	}
    }

  /* We cannot remove stmts during BB walk, especially not release SSA
     names there as this confuses the VN machinery.  The stmts ending
     up in to_remove are either stores or simple copies.  */
  for (i = 0; VEC_iterate (gimple, to_remove, i, stmt); ++i)
    {
      tree lhs = gimple_assign_lhs (stmt);
      use_operand_p use_p;
      gimple use_stmt;

      /* If there is a single use only, propagate the equivalency
	 instead of keeping the copy.  */
      if (TREE_CODE (lhs) == SSA_NAME
	  && single_imm_use (lhs, &use_p, &use_stmt)
	  && may_propagate_copy (USE_FROM_PTR (use_p),
				 gimple_assign_rhs1 (stmt)))
	{
	  SET_USE (use_p, gimple_assign_rhs1 (stmt));
	  update_stmt (use_stmt);
	}

      /* If this is a store or a now unused copy, remove it.  */
      if (TREE_CODE (lhs) != SSA_NAME
	  || has_zero_uses (lhs))
	{
	  gsi = gsi_for_stmt (stmt);
	  unlink_stmt_vdef (stmt);
	  gsi_remove (&gsi, true);
	  release_defs (stmt);
	}
    }
  VEC_free (gimple, heap, to_remove);

  return todo;
}

/* Borrow a bit of tree-ssa-dce.c for the moment.
   XXX: In 4.1, we should be able to just run a DCE pass after PRE, though
   this may be a bit faster, and we may want critical edges kept split.  */

/* If OP's defining statement has not already been determined to be necessary,
   mark that statement necessary. Return the stmt, if it is newly
   necessary.  */

static inline gimple
mark_operand_necessary (tree op)
{
  gimple stmt;

  gcc_assert (op);

  if (TREE_CODE (op) != SSA_NAME)
    return NULL;

  stmt = SSA_NAME_DEF_STMT (op);
  gcc_assert (stmt);

  if (gimple_plf (stmt, NECESSARY)
      || gimple_nop_p (stmt))
    return NULL;

  gimple_set_plf (stmt, NECESSARY, true);
  return stmt;
}

/* Because we don't follow exactly the standard PRE algorithm, and decide not
   to insert PHI nodes sometimes, and because value numbering of casts isn't
   perfect, we sometimes end up inserting dead code.   This simple DCE-like
   pass removes any insertions we made that weren't actually used.  */

static void
remove_dead_inserted_code (void)
{
  VEC(gimple,heap) *worklist = NULL;
  int i;
  gimple t;

  worklist = VEC_alloc (gimple, heap, VEC_length (gimple, inserted_exprs));
  for (i = 0; VEC_iterate (gimple, inserted_exprs, i, t); i++)
    {
      if (gimple_plf (t, NECESSARY))
	VEC_quick_push (gimple, worklist, t);
    }
  while (VEC_length (gimple, worklist) > 0)
    {
      t = VEC_pop (gimple, worklist);

      /* PHI nodes are somewhat special in that each PHI alternative has
	 data and control dependencies.  All the statements feeding the
	 PHI node's arguments are always necessary. */
      if (gimple_code (t) == GIMPLE_PHI)
	{
	  unsigned k;

	  VEC_reserve (gimple, heap, worklist, gimple_phi_num_args (t));
	  for (k = 0; k < gimple_phi_num_args (t); k++)
	    {
	      tree arg = PHI_ARG_DEF (t, k);
	      if (TREE_CODE (arg) == SSA_NAME)
		{
		  gimple n = mark_operand_necessary (arg);
		  if (n)
		    VEC_quick_push (gimple, worklist, n);
		}
	    }
	}
      else
	{
	  /* Propagate through the operands.  Examine all the USE, VUSE and
	     VDEF operands in this statement.  Mark all the statements
	     which feed this statement's uses as necessary.  */
	  ssa_op_iter iter;
	  tree use;

	  /* The operands of VDEF expressions are also needed as they
	     represent potential definitions that may reach this
	     statement (VDEF operands allow us to follow def-def
	     links).  */

	  FOR_EACH_SSA_TREE_OPERAND (use, t, iter, SSA_OP_ALL_USES)
	    {
	      gimple n = mark_operand_necessary (use);
	      if (n)
		VEC_safe_push (gimple, heap, worklist, n);
	    }
	}
    }

  for (i = 0; VEC_iterate (gimple, inserted_exprs, i, t); i++)
    {
      if (!gimple_plf (t, NECESSARY))
	{
	  gimple_stmt_iterator gsi;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Removing unnecessary insertion:");
	      print_gimple_stmt (dump_file, t, 0, 0);
	    }

	  gsi = gsi_for_stmt (t);
	  if (gimple_code (t) == GIMPLE_PHI)
	    remove_phi_node (&gsi, true);
	  else
	    {
	      gsi_remove (&gsi, true);
	      release_defs (t);
	    }
	}
    }
  VEC_free (gimple, heap, worklist);
}

/* Initialize data structures used by PRE.  */

static void
init_pre (bool do_fre)
{
  basic_block bb;

  next_expression_id = 1;
  expressions = NULL;
  VEC_safe_push (pre_expr, heap, expressions, NULL);
  value_expressions = VEC_alloc (bitmap_set_t, heap, get_max_value_id () + 1);
  VEC_safe_grow_cleared (bitmap_set_t, heap, value_expressions,
			 get_max_value_id() + 1);

  in_fre = do_fre;

  inserted_exprs = NULL;
  need_creation = NULL;
  pretemp = NULL_TREE;
  storetemp = NULL_TREE;
  prephitemp = NULL_TREE;

  connect_infinite_loops_to_exit ();
  memset (&pre_stats, 0, sizeof (pre_stats));


  postorder = XNEWVEC (int, n_basic_blocks - NUM_FIXED_BLOCKS);
  post_order_compute (postorder, false, false);

  FOR_ALL_BB (bb)
    bb->aux = XCNEWVEC (struct bb_bitmap_sets, 1);

  calculate_dominance_info (CDI_POST_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);

  bitmap_obstack_initialize (&grand_bitmap_obstack);
  inserted_phi_names = BITMAP_ALLOC (&grand_bitmap_obstack);
  phi_translate_table = htab_create (5110, expr_pred_trans_hash,
				     expr_pred_trans_eq, free);
  expression_to_id = htab_create (num_ssa_names * 3,
				  pre_expr_hash,
				  pre_expr_eq, NULL);
  seen_during_translate = BITMAP_ALLOC (&grand_bitmap_obstack);
  bitmap_set_pool = create_alloc_pool ("Bitmap sets",
				       sizeof (struct bitmap_set), 30);
  pre_expr_pool = create_alloc_pool ("pre_expr nodes",
				     sizeof (struct pre_expr_d), 30);
  FOR_ALL_BB (bb)
    {
      EXP_GEN (bb) = bitmap_set_new ();
      PHI_GEN (bb) = bitmap_set_new ();
      TMP_GEN (bb) = bitmap_set_new ();
      AVAIL_OUT (bb) = bitmap_set_new ();
    }

  need_eh_cleanup = BITMAP_ALLOC (NULL);
}


/* Deallocate data structures used by PRE.  */

static void
fini_pre (bool do_fre)
{
  basic_block bb;

  free (postorder);
  VEC_free (bitmap_set_t, heap, value_expressions);
  VEC_free (gimple, heap, inserted_exprs);
  VEC_free (gimple, heap, need_creation);
  bitmap_obstack_release (&grand_bitmap_obstack);
  free_alloc_pool (bitmap_set_pool);
  free_alloc_pool (pre_expr_pool);
  htab_delete (phi_translate_table);
  htab_delete (expression_to_id);

  FOR_ALL_BB (bb)
    {
      free (bb->aux);
      bb->aux = NULL;
    }

  free_dominance_info (CDI_POST_DOMINATORS);

  if (!bitmap_empty_p (need_eh_cleanup))
    {
      gimple_purge_all_dead_eh_edges (need_eh_cleanup);
      cleanup_tree_cfg ();
    }

  BITMAP_FREE (need_eh_cleanup);

  if (!do_fre)
    loop_optimizer_finalize ();
}

/* Main entry point to the SSA-PRE pass.  DO_FRE is true if the caller
   only wants to do full redundancy elimination.  */

static unsigned int
execute_pre (bool do_fre)
{
  unsigned int todo = 0;

  do_partial_partial = optimize > 2 && optimize_function_for_speed_p (cfun);

  /* This has to happen before SCCVN runs because
     loop_optimizer_init may create new phis, etc.  */
  if (!do_fre)
    loop_optimizer_init (LOOPS_NORMAL);

  if (!run_scc_vn (do_fre))
    {
      if (!do_fre)
	{
	  remove_dead_inserted_code ();
	  loop_optimizer_finalize ();
	}

      return 0;
    }
  init_pre (do_fre);
  scev_initialize ();


  /* Collect and value number expressions computed in each basic block.  */
  compute_avail ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      basic_block bb;

      FOR_ALL_BB (bb)
	{
	  print_bitmap_set (dump_file, EXP_GEN (bb), "exp_gen", bb->index);
	  print_bitmap_set (dump_file, PHI_GEN (bb), "phi_gen", bb->index);
	  print_bitmap_set (dump_file, TMP_GEN (bb), "tmp_gen", bb->index);
	  print_bitmap_set (dump_file, AVAIL_OUT (bb), "avail_out", bb->index);
	}
    }

  /* Insert can get quite slow on an incredibly large number of basic
     blocks due to some quadratic behavior.  Until this behavior is
     fixed, don't run it when he have an incredibly large number of
     bb's.  If we aren't going to run insert, there is no point in
     computing ANTIC, either, even though it's plenty fast.  */
  if (!do_fre && n_basic_blocks < 4000)
    {
      compute_antic ();
      insert ();
    }

  /* Remove all the redundant expressions.  */
  todo |= eliminate ();

  statistics_counter_event (cfun, "Insertions", pre_stats.insertions);
  statistics_counter_event (cfun, "PA inserted", pre_stats.pa_insert);
  statistics_counter_event (cfun, "New PHIs", pre_stats.phis);
  statistics_counter_event (cfun, "Eliminated", pre_stats.eliminations);
  statistics_counter_event (cfun, "Constified", pre_stats.constified);

  /* Make sure to remove fake edges before committing our inserts.
     This makes sure we don't end up with extra critical edges that
     we would need to split.  */
  remove_fake_exit_edges ();
  gsi_commit_edge_inserts ();

  clear_expression_ids ();
  free_scc_vn ();
  if (!do_fre)
    remove_dead_inserted_code ();

  scev_finalize ();
  fini_pre (do_fre);

  return todo;
}

/* Gate and execute functions for PRE.  */

static unsigned int
do_pre (void)
{
  return execute_pre (false);
}

static bool
gate_pre (void)
{
  return flag_tree_pre != 0;
}

struct gimple_opt_pass pass_pre =
{
 {
  GIMPLE_PASS,
  "pre",				/* name */
  gate_pre,				/* gate */
  do_pre,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_PRE,				/* tv_id */
  PROP_no_crit_edges | PROP_cfg
    | PROP_ssa,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  TODO_rebuild_alias,			/* todo_flags_start */
  TODO_update_ssa_only_virtuals | TODO_dump_func | TODO_ggc_collect
  | TODO_verify_ssa /* todo_flags_finish */
 }
};


/* Gate and execute functions for FRE.  */

static unsigned int
execute_fre (void)
{
  return execute_pre (true);
}

static bool
gate_fre (void)
{
  return flag_tree_fre != 0;
}

struct gimple_opt_pass pass_fre =
{
 {
  GIMPLE_PASS,
  "fre",				/* name */
  gate_fre,				/* gate */
  execute_fre,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_FRE,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_ggc_collect | TODO_verify_ssa /* todo_flags_finish */
 }
};
