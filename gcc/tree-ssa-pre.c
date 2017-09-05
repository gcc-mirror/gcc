/* Full and partial redundancy elimination and code hoisting on SSA GIMPLE.
   Copyright (C) 2001-2017 Free Software Foundation, Inc.
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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "tree-ssa-sccvn.h"
#include "tree-scalar-evolution.h"
#include "params.h"
#include "dbgcnt.h"
#include "domwalk.h"
#include "tree-ssa-propagate.h"
#include "ipa-utils.h"
#include "tree-cfgcleanup.h"
#include "langhooks.h"
#include "alias.h"

/* Even though this file is called tree-ssa-pre.c, we actually
   implement a bit more than just PRE here.  All of them piggy-back
   on GVN which is implemented in tree-ssa-sccvn.c.

     1. Full Redundancy Elimination (FRE)
	This is the elimination phase of GVN.

     2. Partial Redundancy Elimination (PRE)
	This is adds computation of AVAIL_OUT and ANTIC_IN and
	doing expression insertion to form GVN-PRE.

     3. Code hoisting
	This optimization uses the ANTIC_IN sets computed for PRE
	to move expressions further up than PRE would do, to make
	multiple computations of the same value fully redundant.
	This pass is explained below (after the explanation of the
	basic algorithm for PRE).
*/

/* TODO:

   1. Avail sets can be shared by making an avail_find_leader that
      walks up the dominator tree and looks in those avail sets.
      This might affect code optimality, it's unclear right now.
      Currently the AVAIL_OUT sets are the remaining quadraticness in
      memory of GVN-PRE.
   2. Strength reduction can be performed by anticipating expressions
      we can repair later on.
   3. We can do back-substitution or smarter value numbering to catch
      commutative expressions split up over multiple statements.
*/

/* For ease of terminology, "expression node" in the below refers to
   every expression node but GIMPLE_ASSIGN, because GIMPLE_ASSIGNs
   represent the actual statement containing the expressions we care about,
   and we cache the value number by putting it in the expression.  */

/* Basic algorithm for Partial Redundancy Elimination:

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

   When optimizing for size, we only eliminate the partial redundancy
   if we need to insert in only one predecessor.  This avoids almost
   completely the code size increase that PRE usually causes.

   For the partial anticipation case, we only perform insertion if it
   is partially anticipated in some block, and fully available in all
   of the predecessors.

   do_pre_regular_insertion/do_pre_partial_partial_insertion
   performs these steps, driven by insert/insert_aux.

   Fourth, we eliminate fully redundant expressions.
   This is a simple statement walk that replaces redundant
   calculations with the now available values.  */

/* Basic algorithm for Code Hoisting:

   Code hoisting is: Moving value computations up in the control flow
   graph to make multiple copies redundant.  Typically this is a size
   optimization, but there are cases where it also is helpful for speed.

   A simple code hoisting algorithm is implemented that piggy-backs on
   the PRE infrastructure.  For code hoisting, we have to know ANTIC_OUT
   which is effectively ANTIC_IN - AVAIL_OUT.  The latter two have to be
   computed for PRE, and we can use them to perform a limited version of
   code hoisting, too.

   For the purpose of this implementation, a value is hoistable to a basic
   block B if the following properties are met:

   1. The value is in ANTIC_IN(B) -- the value will be computed on all
      paths from B to function exit and it can be computed in B);

   2. The value is not in AVAIL_OUT(B) -- there would be no need to
      compute the value again and make it available twice;

   3. All successors of B are dominated by B -- makes sure that inserting
      a computation of the value in B will make the remaining
      computations fully redundant;

   4. At least one successor has the value in AVAIL_OUT -- to avoid
      hoisting values up too far;

   5. There are at least two successors of B -- hoisting in straight
      line code is pointless.

   The third condition is not strictly necessary, but it would complicate
   the hoisting pass a lot.  In fact, I don't know of any code hoisting
   algorithm that does not have this requirement.  Fortunately, experiments
   have show that most candidate hoistable values are in regions that meet
   this condition (e.g. diamond-shape regions).

   The forth condition is necessary to avoid hoisting things up too far
   away from the uses of the value.  Nothing else limits the algorithm
   from hoisting everything up as far as ANTIC_IN allows.  Experiments
   with SPEC and CSiBE have shown that hoisting up too far results in more
   spilling, less benefits for code size, and worse benchmark scores.
   Fortunately, in practice most of the interesting hoisting opportunities
   are caught despite this limitation.

   For hoistable values that meet all conditions, expressions are inserted
   to make the calculation of the hoistable value fully redundant.  We
   perform code hoisting insertions after each round of PRE insertions,
   because code hoisting never exposes new PRE opportunities, but PRE can
   create new code hoisting opportunities.

   The code hoisting algorithm is implemented in do_hoist_insert, driven
   by insert/insert_aux.  */

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

union pre_expr_union
{
  tree name;
  tree constant;
  vn_nary_op_t nary;
  vn_reference_t reference;
};

typedef struct pre_expr_d : nofree_ptr_hash <pre_expr_d>
{
  enum pre_expr_kind kind;
  unsigned int id;
  pre_expr_union u;

  /* hash_table support.  */
  static inline hashval_t hash (const pre_expr_d *);
  static inline int equal (const pre_expr_d *, const pre_expr_d *);
} *pre_expr;

#define PRE_EXPR_NAME(e) (e)->u.name
#define PRE_EXPR_NARY(e) (e)->u.nary
#define PRE_EXPR_REFERENCE(e) (e)->u.reference
#define PRE_EXPR_CONSTANT(e) (e)->u.constant

/* Compare E1 and E1 for equality.  */

inline int
pre_expr_d::equal (const pre_expr_d *e1, const pre_expr_d *e2)
{
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
      gcc_unreachable ();
    }
}

/* Hash E.  */

inline hashval_t
pre_expr_d::hash (const pre_expr_d *e)
{
  switch (e->kind)
    {
    case CONSTANT:
      return vn_hash_constant_with_type (PRE_EXPR_CONSTANT (e));
    case NAME:
      return SSA_NAME_VERSION (PRE_EXPR_NAME (e));
    case NARY:
      return PRE_EXPR_NARY (e)->hashcode;
    case REFERENCE:
      return PRE_EXPR_REFERENCE (e)->hashcode;
    default:
      gcc_unreachable ();
    }
}

/* Next global expression id number.  */
static unsigned int next_expression_id;

/* Mapping from expression to id number we can use in bitmap sets.  */
static vec<pre_expr> expressions;
static hash_table<pre_expr_d> *expression_to_id;
static vec<unsigned> name_to_id;

/* Allocate an expression id for EXPR.  */

static inline unsigned int
alloc_expression_id (pre_expr expr)
{
  struct pre_expr_d **slot;
  /* Make sure we won't overflow. */
  gcc_assert (next_expression_id + 1 > next_expression_id);
  expr->id = next_expression_id++;
  expressions.safe_push (expr);
  if (expr->kind == NAME)
    {
      unsigned version = SSA_NAME_VERSION (PRE_EXPR_NAME (expr));
      /* vec::safe_grow_cleared allocates no headroom.  Avoid frequent
	 re-allocations by using vec::reserve upfront.  */
      unsigned old_len = name_to_id.length ();
      name_to_id.reserve (num_ssa_names - old_len);
      name_to_id.quick_grow_cleared (num_ssa_names);
      gcc_assert (name_to_id[version] == 0);
      name_to_id[version] = expr->id;
    }
  else
    {
      slot = expression_to_id->find_slot (expr, INSERT);
      gcc_assert (!*slot);
      *slot = expr;
    }
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
  struct pre_expr_d **slot;

  if (expr->kind == NAME)
    {
      unsigned version = SSA_NAME_VERSION (PRE_EXPR_NAME (expr));
      if (name_to_id.length () <= version)
	return 0;
      return name_to_id[version];
    }
  else
    {
      slot = expression_to_id->find_slot (expr, NO_INSERT);
      if (!slot)
	return 0;
      return ((pre_expr)*slot)->id;
    }
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
  return expressions[id];
}

/* Free the expression id field in all of our expressions,
   and then destroy the expressions array.  */

static void
clear_expression_ids (void)
{
  expressions.release ();
}

static object_allocator<pre_expr_d> pre_expr_pool ("pre_expr nodes");

/* Given an SSA_NAME NAME, get or create a pre_expr to represent it.  */

static pre_expr
get_or_alloc_expr_for_name (tree name)
{
  struct pre_expr_d expr;
  pre_expr result;
  unsigned int result_id;

  expr.kind = NAME;
  expr.id = 0;
  PRE_EXPR_NAME (&expr) = name;
  result_id = lookup_expression_id (&expr);
  if (result_id != 0)
    return expression_for_id (result_id);

  result = pre_expr_pool.allocate ();
  result->kind = NAME;
  PRE_EXPR_NAME (result) = name;
  alloc_expression_id (result);
  return result;
}

/* An unordered bitmap set.  One bitmap tracks values, the other,
   expressions.  */
typedef struct bitmap_set
{
  bitmap_head expressions;
  bitmap_head values;
} *bitmap_set_t;

#define FOR_EACH_EXPR_ID_IN_SET(set, id, bi)		\
  EXECUTE_IF_SET_IN_BITMAP (&(set)->expressions, 0, (id), (bi))

#define FOR_EACH_VALUE_ID_IN_SET(set, id, bi)		\
  EXECUTE_IF_SET_IN_BITMAP (&(set)->values, 0, (id), (bi))

/* Mapping from value id to expressions with that value_id.  */
static vec<bitmap> value_expressions;

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

  /* The live virtual operand on successor edges.  */
  tree vop_on_exit;

  /* True if we have visited this block during ANTIC calculation.  */
  unsigned int visited : 1;

  /* True when the block contains a call that might not return.  */
  unsigned int contains_may_not_return_call : 1;
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
#define BB_MAY_NOTRETURN(BB) ((bb_value_sets_t) ((BB)->aux))->contains_may_not_return_call
#define BB_LIVE_VOP_ON_EXIT(BB) ((bb_value_sets_t) ((BB)->aux))->vop_on_exit


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

  /* The number of inserts made for code hoisting.  */
  int hoist_insert;

  /* The number of new PHI nodes added by PRE.  */
  int phis;
} pre_stats;

static bool do_partial_partial;
static pre_expr bitmap_find_leader (bitmap_set_t, unsigned int);
static void bitmap_value_insert_into_set (bitmap_set_t, pre_expr);
static void bitmap_value_replace_in_set (bitmap_set_t, pre_expr);
static void bitmap_set_copy (bitmap_set_t, bitmap_set_t);
static void bitmap_set_and (bitmap_set_t, bitmap_set_t);
static bool bitmap_set_contains_value (bitmap_set_t, unsigned int);
static void bitmap_insert_into_set (bitmap_set_t, pre_expr);
static bitmap_set_t bitmap_set_new (void);
static tree create_expression_by_pieces (basic_block, pre_expr, gimple_seq *,
					 tree);
static tree find_or_generate_expression (basic_block, tree, gimple_seq *);
static unsigned int get_expr_value_id (pre_expr);

/* We can add and remove elements and entries to and from sets
   and hash tables, so we use alloc pools for them.  */

static object_allocator<bitmap_set> bitmap_set_pool ("Bitmap sets");
static bitmap_obstack grand_bitmap_obstack;

/* Set of blocks with statements that have had their EH properties changed.  */
static bitmap need_eh_cleanup;

/* Set of blocks with statements that have had their AB properties changed.  */
static bitmap need_ab_cleanup;

/* A three tuple {e, pred, v} used to cache phi translations in the
   phi_translate_table.  */

typedef struct expr_pred_trans_d : free_ptr_hash<expr_pred_trans_d>
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

  /* hash_table support.  */
  static inline hashval_t hash (const expr_pred_trans_d *);
  static inline int equal (const expr_pred_trans_d *, const expr_pred_trans_d *);
} *expr_pred_trans_t;
typedef const struct expr_pred_trans_d *const_expr_pred_trans_t;

inline hashval_t
expr_pred_trans_d::hash (const expr_pred_trans_d *e)
{
  return e->hashcode;
}

inline int
expr_pred_trans_d::equal (const expr_pred_trans_d *ve1,
			  const expr_pred_trans_d *ve2)
{
  basic_block b1 = ve1->pred;
  basic_block b2 = ve2->pred;

  /* If they are not translations for the same basic block, they can't
     be equal.  */
  if (b1 != b2)
    return false;
  return pre_expr_d::equal (ve1->e, ve2->e);
}

/* The phi_translate_table caches phi translations for a given
   expression and predecessor.  */
static hash_table<expr_pred_trans_d> *phi_translate_table;

/* Add the tuple mapping from {expression E, basic block PRED} to
   the phi translation table and return whether it pre-existed.  */

static inline bool
phi_trans_add (expr_pred_trans_t *entry, pre_expr e, basic_block pred)
{
  expr_pred_trans_t *slot;
  expr_pred_trans_d tem;
  hashval_t hash = iterative_hash_hashval_t (pre_expr_d::hash (e),
					     pred->index);
  tem.e = e;
  tem.pred = pred;
  tem.hashcode = hash;
  slot = phi_translate_table->find_slot_with_hash (&tem, hash, INSERT);
  if (*slot)
    {
      *entry = *slot;
      return true;
    }

  *entry = *slot = XNEW (struct expr_pred_trans_d);
  (*entry)->e = e;
  (*entry)->pred = pred;
  (*entry)->hashcode = hash;
  return false;
}


/* Add expression E to the expression set of value id V.  */

static void
add_to_value (unsigned int v, pre_expr e)
{
  bitmap set;

  gcc_checking_assert (get_expr_value_id (e) == v);

  if (v >= value_expressions.length ())
    {
      value_expressions.safe_grow_cleared (v + 1);
    }

  set = value_expressions[v];
  if (!set)
    {
      set = BITMAP_ALLOC (&grand_bitmap_obstack);
      value_expressions[v] = set;
    }

  bitmap_set_bit (set, get_or_alloc_expression_id (e));
}

/* Create a new bitmap set and return it.  */

static bitmap_set_t
bitmap_set_new (void)
{
  bitmap_set_t ret = bitmap_set_pool.allocate ();
  bitmap_initialize (&ret->expressions, &grand_bitmap_obstack);
  bitmap_initialize (&ret->values, &grand_bitmap_obstack);
  return ret;
}

/* Return the value id for a PRE expression EXPR.  */

static unsigned int
get_expr_value_id (pre_expr expr)
{
  unsigned int id;
  switch (expr->kind)
    {
    case CONSTANT:
      id = get_constant_value_id (PRE_EXPR_CONSTANT (expr));
      break;
    case NAME:
      id = VN_INFO (PRE_EXPR_NAME (expr))->value_id;
      break;
    case NARY:
      id = PRE_EXPR_NARY (expr)->value_id;
      break;
    case REFERENCE:
      id = PRE_EXPR_REFERENCE (expr)->value_id;
      break;
    default:
      gcc_unreachable ();
    }
  /* ???  We cannot assert that expr has a value-id (it can be 0), because
     we assign value-ids only to expressions that have a result
     in set_hashtable_value_ids.  */
  return id;
}

/* Return a SCCVN valnum (SSA name or constant) for the PRE value-id VAL.  */

static tree
sccvn_valnum_from_value_id (unsigned int val)
{
  bitmap_iterator bi;
  unsigned int i;
  bitmap exprset = value_expressions[val];
  EXECUTE_IF_SET_IN_BITMAP (exprset, 0, i, bi)
    {
      pre_expr vexpr = expression_for_id (i);
      if (vexpr->kind == NAME)
	return VN_INFO (PRE_EXPR_NAME (vexpr))->valnum;
      else if (vexpr->kind == CONSTANT)
	return PRE_EXPR_CONSTANT (vexpr);
    }
  return NULL_TREE;
}

/* Remove an expression EXPR from a bitmapped set.  */

static void
bitmap_remove_from_set (bitmap_set_t set, pre_expr expr)
{
  unsigned int val  = get_expr_value_id (expr);
  if (!value_id_constant_p (val))
    {
      bitmap_clear_bit (&set->values, val);
      bitmap_clear_bit (&set->expressions, get_expression_id (expr));
    }
}

/* Insert an expression EXPR into a bitmapped set.  */

static void
bitmap_insert_into_set (bitmap_set_t set, pre_expr expr)
{
  unsigned int val = get_expr_value_id (expr);
  if (! value_id_constant_p (val))
    {
      /* Note this is the only function causing multiple expressions
         for the same value to appear in a set.  This is needed for
	 TMP_GEN, PHI_GEN and NEW_SETs.  */
      bitmap_set_bit (&set->values, val);
      bitmap_set_bit (&set->expressions, get_or_alloc_expression_id (expr));
    }
}

/* Copy a bitmapped set ORIG, into bitmapped set DEST.  */

static void
bitmap_set_copy (bitmap_set_t dest, bitmap_set_t orig)
{
  bitmap_copy (&dest->expressions, &orig->expressions);
  bitmap_copy (&dest->values, &orig->values);
}


/* Free memory used up by SET.  */
static void
bitmap_set_free (bitmap_set_t set)
{
  bitmap_clear (&set->expressions);
  bitmap_clear (&set->values);
}


/* Generate an topological-ordered array of bitmap set SET.  */

static vec<pre_expr> 
sorted_array_from_bitmap_set (bitmap_set_t set)
{
  unsigned int i, j;
  bitmap_iterator bi, bj;
  vec<pre_expr> result;

  /* Pre-allocate enough space for the array.  */
  result.create (bitmap_count_bits (&set->expressions));

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
      bitmap exprset = value_expressions[i];
      EXECUTE_IF_SET_IN_BITMAP (exprset, 0, j, bj)
	{
	  if (bitmap_bit_p (&set->expressions, j))
	    result.quick_push (expression_for_id (j));
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
      bitmap_and_into (&dest->values, &orig->values);

      unsigned int to_clear = -1U;
      FOR_EACH_EXPR_ID_IN_SET (dest, i, bi)
	{
	  if (to_clear != -1U)
	    {
	      bitmap_clear_bit (&dest->expressions, to_clear);
	      to_clear = -1U;
	    }
	  pre_expr expr = expression_for_id (i);
	  unsigned int value_id = get_expr_value_id (expr);
	  if (!bitmap_bit_p (&dest->values, value_id))
	    to_clear = i;
	}
      if (to_clear != -1U)
	bitmap_clear_bit (&dest->expressions, to_clear);
    }
}

/* Subtract all expressions contained in ORIG from DEST.  */

static bitmap_set_t
bitmap_set_subtract (bitmap_set_t dest, bitmap_set_t orig)
{
  bitmap_set_t result = bitmap_set_new ();
  bitmap_iterator bi;
  unsigned int i;

  bitmap_and_compl (&result->expressions, &dest->expressions,
		    &orig->expressions);

  FOR_EACH_EXPR_ID_IN_SET (result, i, bi)
    {
      pre_expr expr = expression_for_id (i);
      unsigned int value_id = get_expr_value_id (expr);
      bitmap_set_bit (&result->values, value_id);
    }

  return result;
}

/* Subtract all values in bitmap set B from bitmap set A.  */

static void
bitmap_set_subtract_values (bitmap_set_t a, bitmap_set_t b)
{
  unsigned int i;
  bitmap_iterator bi;
  pre_expr to_remove = NULL;
  FOR_EACH_EXPR_ID_IN_SET (a, i, bi)
    {
      if (to_remove)
	{
	  bitmap_remove_from_set (a, to_remove);
	  to_remove = NULL;
	}
      pre_expr expr = expression_for_id (i);
      if (bitmap_set_contains_value (b, get_expr_value_id (expr)))
	to_remove = expr;
    }
  if (to_remove)
    bitmap_remove_from_set (a, to_remove);
}


/* Return true if bitmapped set SET contains the value VALUE_ID.  */

static bool
bitmap_set_contains_value (bitmap_set_t set, unsigned int value_id)
{
  if (value_id_constant_p (value_id))
    return true;

  if (!set || bitmap_empty_p (&set->expressions))
    return false;

  return bitmap_bit_p (&set->values, value_id);
}

static inline bool
bitmap_set_contains_expr (bitmap_set_t set, const pre_expr expr)
{
  return bitmap_bit_p (&set->expressions, get_expression_id (expr));
}

/* Replace an instance of value LOOKFOR with expression EXPR in SET.  */

static void
bitmap_set_replace_value (bitmap_set_t set, unsigned int lookfor,
			  const pre_expr expr)
{
  bitmap exprset;
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
  exprset = value_expressions[lookfor];
  EXECUTE_IF_SET_IN_BITMAP (exprset, 0, i, bi)
    {
      if (bitmap_clear_bit (&set->expressions, i))
	{
	  bitmap_set_bit (&set->expressions, get_expression_id (expr));
	  return;
	}
    }

  gcc_unreachable ();
}

/* Return true if two bitmap sets are equal.  */

static bool
bitmap_set_equal (bitmap_set_t a, bitmap_set_t b)
{
  return bitmap_equal_p (&a->values, &b->values);
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

  gcc_checking_assert (expr->id == get_or_alloc_expression_id (expr));

  /* Constant values are always considered to be part of the set.  */
  if (value_id_constant_p (val))
    return;

  /* If the value membership changed, add the expression.  */
  if (bitmap_set_bit (&set->values, val))
    bitmap_set_bit (&set->expressions, expr->id);
}

/* Print out EXPR to outfile.  */

static void
print_pre_expr (FILE *outfile, const pre_expr expr)
{
  if (! expr)
    {
      fprintf (outfile, "NULL");
      return;
    }
  switch (expr->kind)
    {
    case CONSTANT:
      print_generic_expr (outfile, PRE_EXPR_CONSTANT (expr));
      break;
    case NAME:
      print_generic_expr (outfile, PRE_EXPR_NAME (expr));
      break;
    case NARY:
      {
	unsigned int i;
	vn_nary_op_t nary = PRE_EXPR_NARY (expr);
	fprintf (outfile, "{%s,", get_tree_code_name (nary->opcode));
	for (i = 0; i < nary->length; i++)
	  {
	    print_generic_expr (outfile, nary->op[i]);
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
	     ref->operands.iterate (i, &vro);
	     i++)
	  {
	    bool closebrace = false;
	    if (vro->opcode != SSA_NAME
		&& TREE_CODE_CLASS (vro->opcode) != tcc_declaration)
	      {
		fprintf (outfile, "%s", get_tree_code_name (vro->opcode));
		if (vro->op0)
		  {
		    fprintf (outfile, "<");
		    closebrace = true;
		  }
	      }
	    if (vro->op0)
	      {
		print_generic_expr (outfile, vro->op0);
		if (vro->op1)
		  {
		    fprintf (outfile, ",");
		    print_generic_expr (outfile, vro->op1);
		  }
		if (vro->op2)
		  {
		    fprintf (outfile, ",");
		    print_generic_expr (outfile, vro->op2);
		  }
	      }
	    if (closebrace)
		fprintf (outfile, ">");
	    if (i != ref->operands.length () - 1)
	      fprintf (outfile, ",");
	  }
	fprintf (outfile, "}");
	if (ref->vuse)
	  {
	    fprintf (outfile, "@");
	    print_generic_expr (outfile, ref->vuse);
	  }
      }
      break;
    }
}
void debug_pre_expr (pre_expr);

/* Like print_pre_expr but always prints to stderr.  */
DEBUG_FUNCTION void
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

DEBUG_FUNCTION void
debug_bitmap_set (bitmap_set_t set)
{
  print_bitmap_set (stderr, set, "debug", 0);
}

void debug_bitmap_sets_for (basic_block);

DEBUG_FUNCTION void
debug_bitmap_sets_for (basic_block bb)
{
  print_bitmap_set (stderr, AVAIL_OUT (bb), "avail_out", bb->index);
  print_bitmap_set (stderr, EXP_GEN (bb), "exp_gen", bb->index);
  print_bitmap_set (stderr, PHI_GEN (bb), "phi_gen", bb->index);
  print_bitmap_set (stderr, TMP_GEN (bb), "tmp_gen", bb->index);
  print_bitmap_set (stderr, ANTIC_IN (bb), "antic_in", bb->index);
  if (do_partial_partial)
    print_bitmap_set (stderr, PA_IN (bb), "pa_in", bb->index);
  print_bitmap_set (stderr, NEW_SETS (bb), "new_sets", bb->index);
}

/* Print out the expressions that have VAL to OUTFILE.  */

static void
print_value_expressions (FILE *outfile, unsigned int val)
{
  bitmap set = value_expressions[val];
  if (set)
    {
      bitmap_set x;
      char s[10];
      sprintf (s, "%04d", val);
      x.expressions = *set;
      print_bitmap_set (outfile, &x, s, 0);
    }
}


DEBUG_FUNCTION void
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
  struct pre_expr_d expr;
  pre_expr newexpr;

  expr.kind = CONSTANT;
  PRE_EXPR_CONSTANT (&expr) = constant;
  result_id = lookup_expression_id (&expr);
  if (result_id != 0)
    return expression_for_id (result_id);

  newexpr = pre_expr_pool.allocate ();
  newexpr->kind = CONSTANT;
  PRE_EXPR_CONSTANT (newexpr) = constant;
  alloc_expression_id (newexpr);
  value_id = get_or_alloc_constant_value_id (constant);
  add_to_value (value_id, newexpr);
  return newexpr;
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
  gcc_unreachable ();
}

/* Return the folded version of T if T, when folded, is a gimple
   min_invariant or an SSA name.  Otherwise, return T.  */

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
	tree res = vn_nary_simplify (nary);
	if (!res)
	  return e;
	if (is_gimple_min_invariant (res))
	  return get_or_alloc_expr_for_constant (res);
	if (TREE_CODE (res) == SSA_NAME)
	  return get_or_alloc_expr_for_name (res);
	return e;
      }
    case REFERENCE:
      {
	vn_reference_t ref = PRE_EXPR_REFERENCE (e);
	tree folded;
	if ((folded = fully_constant_vn_reference_p (ref)))
	  return get_or_alloc_expr_for_constant (folded);
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
translate_vuse_through_block (vec<vn_reference_op_s> operands,
			      alias_set_type set, tree type, tree vuse,
			      basic_block phiblock,
			      basic_block block, bool *same_valid)
{
  gimple *phi = SSA_NAME_DEF_STMT (vuse);
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
	  unsigned int cnt;
	  /* Try to find a vuse that dominates this phi node by skipping
	     non-clobbering statements.  */
	  vuse = get_continuation_for_phi (phi, &ref, &cnt, &visited, false,
					   NULL, NULL);
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
   SET2 *or* SET3.  This is used to avoid making a set consisting of the union
   of PA_IN and ANTIC_IN during insert and phi-translation.  */

static inline pre_expr
find_leader_in_sets (unsigned int val, bitmap_set_t set1, bitmap_set_t set2,
		     bitmap_set_t set3 = NULL)
{
  pre_expr result;

  result = bitmap_find_leader (set1, val);
  if (!result && set2)
    result = bitmap_find_leader (set2, val);
  if (!result && set3)
    result = bitmap_find_leader (set3, val);
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
  gcc_unreachable ();
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
  tree name;
  unsigned int value_id = get_expr_value_id (e);

  switch (e->kind)
    {
    case NAME:
      return VN_INFO (PRE_EXPR_NAME (e))->valnum;
    case CONSTANT:
      return PRE_EXPR_CONSTANT (e);
    case NARY:
    case REFERENCE:
      {
	/* Go through all of the expressions representing this value
	   and pick out an SSA_NAME.  */
	unsigned int i;
	bitmap_iterator bi;
	bitmap exprs = value_expressions[value_id];
	EXECUTE_IF_SET_IN_BITMAP (exprs, 0, i, bi)
	  {
	    pre_expr rep = expression_for_id (i);
	    if (rep->kind == NAME)
	      return VN_INFO (PRE_EXPR_NAME (rep))->valnum;
	    else if (rep->kind == CONSTANT)
	      return PRE_EXPR_CONSTANT (rep);
	  }
      }
      break;
    }

  /* If we reached here we couldn't find an SSA_NAME.  This can
     happen when we've discovered a value that has never appeared in
     the program as set to an SSA_NAME, as the result of phi translation.
     Create one here.
     ???  We should be able to re-use this when we insert the statement
     to compute it.  */
  name = make_temp_ssa_name (get_expr_type (e), gimple_build_nop (), "pretmp");
  VN_INFO_GET (name)->value_id = value_id;
  VN_INFO (name)->valnum = name;
  /* ???  For now mark this SSA name for release by SCCVN.  */
  VN_INFO (name)->needs_insertion = true;
  add_to_value (value_id, get_or_alloc_expr_for_name (name));
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Created SSA_NAME representative ");
      print_generic_expr (dump_file, name);
      fprintf (dump_file, " for expression:");
      print_pre_expr (dump_file, e);
      fprintf (dump_file, " (%04d)\n", value_id);
    }

  return name;
}



static pre_expr
phi_translate (pre_expr expr, bitmap_set_t set1, bitmap_set_t set2,
	       basic_block pred, basic_block phiblock);

/* Translate EXPR using phis in PHIBLOCK, so that it has the values of
   the phis in PRED.  Return NULL if we can't find a leader for each part
   of the translated expression.  */

static pre_expr
phi_translate_1 (pre_expr expr, bitmap_set_t set1, bitmap_set_t set2,
		 basic_block pred, basic_block phiblock)
{
  switch (expr->kind)
    {
    case NARY:
      {
	unsigned int i;
	bool changed = false;
	vn_nary_op_t nary = PRE_EXPR_NARY (expr);
	vn_nary_op_t newnary = XALLOCAVAR (struct vn_nary_op_s,
					   sizeof_vn_nary_op (nary->length));
	memcpy (newnary, nary, sizeof_vn_nary_op (nary->length));

	for (i = 0; i < newnary->length; i++)
	  {
	    if (TREE_CODE (newnary->op[i]) != SSA_NAME)
	      continue;
	    else
	      {
                pre_expr leader, result;
		unsigned int op_val_id = VN_INFO (newnary->op[i])->value_id;
		leader = find_leader_in_sets (op_val_id, set1, set2);
                result = phi_translate (leader, set1, set2, pred, phiblock);
		if (result && result != leader)
		  newnary->op[i] = get_representative_for (result);
		else if (!result)
		  return NULL;

		changed |= newnary->op[i] != nary->op[i];
	      }
	  }
	if (changed)
	  {
	    pre_expr constant;
	    unsigned int new_val_id;

	    PRE_EXPR_NARY (expr) = newnary;
	    constant = fully_constant_expression (expr);
	    PRE_EXPR_NARY (expr) = nary;
	    if (constant != expr)
	      {
		/* For non-CONSTANTs we have to make sure we can eventually
		   insert the expression.  Which means we need to have a
		   leader for it.  */
		if (constant->kind != CONSTANT)
		  {
		    /* Do not allow simplifications to non-constants over
		       backedges as this will likely result in a loop PHI node
		       to be inserted and increased register pressure.
		       See PR77498 - this avoids doing predcoms work in
		       a less efficient way.  */
		    if (find_edge (pred, phiblock)->flags & EDGE_DFS_BACK)
		      ;
		    else
		      {
			unsigned value_id = get_expr_value_id (constant);
			constant = find_leader_in_sets (value_id, set1, set2,
							AVAIL_OUT (pred));
			if (constant)
			  return constant;
		      }
		  }
		else
		  return constant;
	      }

	    tree result = vn_nary_op_lookup_pieces (newnary->length,
						    newnary->opcode,
						    newnary->type,
						    &newnary->op[0],
						    &nary);
	    if (result && is_gimple_min_invariant (result))
	      return get_or_alloc_expr_for_constant (result);

	    expr = pre_expr_pool.allocate ();
	    expr->kind = NARY;
	    expr->id = 0;
	    if (nary)
	      {
		PRE_EXPR_NARY (expr) = nary;
		new_val_id = nary->value_id;
		get_or_alloc_expression_id (expr);
		/* When we end up re-using a value number make sure that
		   doesn't have unrelated (which we can't check here)
		   range or points-to info on it.  */
		if (result
		    && INTEGRAL_TYPE_P (TREE_TYPE (result))
		    && SSA_NAME_RANGE_INFO (result)
		    && ! SSA_NAME_IS_DEFAULT_DEF (result))
		  {
		    if (! VN_INFO (result)->info.range_info)
		      {
			VN_INFO (result)->info.range_info
			  = SSA_NAME_RANGE_INFO (result);
			VN_INFO (result)->range_info_anti_range_p
			  = SSA_NAME_ANTI_RANGE_P (result);
		      }
		    if (dump_file && (dump_flags & TDF_DETAILS))
		      {
			fprintf (dump_file, "clearing range info of ");
			print_generic_expr (dump_file, result);
			fprintf (dump_file, "\n");
		      }
		    SSA_NAME_RANGE_INFO (result) = NULL;
		  }
		else if (result
			 && POINTER_TYPE_P (TREE_TYPE (result))
			 && SSA_NAME_PTR_INFO (result)
			 && ! SSA_NAME_IS_DEFAULT_DEF (result))
		  {
		    if (! VN_INFO (result)->info.ptr_info)
		      VN_INFO (result)->info.ptr_info
			= SSA_NAME_PTR_INFO (result);
		    if (dump_file && (dump_flags & TDF_DETAILS))
		      {
			fprintf (dump_file, "clearing points-to info of ");
			print_generic_expr (dump_file, result);
			fprintf (dump_file, "\n");
		      }
		    SSA_NAME_PTR_INFO (result) = NULL;
		  }
	      }
	    else
	      {
		new_val_id = get_next_value_id ();
		value_expressions.safe_grow_cleared (get_max_value_id () + 1);
		nary = vn_nary_op_insert_pieces (newnary->length,
						 newnary->opcode,
						 newnary->type,
						 &newnary->op[0],
						 result, new_val_id);
		PRE_EXPR_NARY (expr) = nary;
		get_or_alloc_expression_id (expr);
	      }
	    add_to_value (new_val_id, expr);
	  }
	return expr;
      }
      break;

    case REFERENCE:
      {
	vn_reference_t ref = PRE_EXPR_REFERENCE (expr);
	vec<vn_reference_op_s> operands = ref->operands;
	tree vuse = ref->vuse;
	tree newvuse = vuse;
	vec<vn_reference_op_s> newoperands = vNULL;
	bool changed = false, same_valid = true;
	unsigned int i, n;
	vn_reference_op_t operand;
	vn_reference_t newref;

	for (i = 0; operands.iterate (i, &operand); i++)
	  {
	    pre_expr opresult;
	    pre_expr leader;
	    tree op[3];
	    tree type = operand->type;
	    vn_reference_op_s newop = *operand;
	    op[0] = operand->op0;
	    op[1] = operand->op1;
	    op[2] = operand->op2;
	    for (n = 0; n < 3; ++n)
	      {
		unsigned int op_val_id;
		if (!op[n])
		  continue;
		if (TREE_CODE (op[n]) != SSA_NAME)
		  {
		    /* We can't possibly insert these.  */
		    if (n != 0
			&& !is_gimple_min_invariant (op[n]))
		      break;
		    continue;
		  }
		op_val_id = VN_INFO (op[n])->value_id;
		leader = find_leader_in_sets (op_val_id, set1, set2);
		opresult = phi_translate (leader, set1, set2, pred, phiblock);
		if (opresult && opresult != leader)
		  {
		    tree name = get_representative_for (opresult);
		    changed |= name != op[n];
		    op[n] = name;
		  }
		else if (!opresult)
		  break;
	      }
	    if (n != 3)
	      {
		newoperands.release ();
		return NULL;
	      }
	    if (!changed)
	      continue;
	    if (!newoperands.exists ())
	      newoperands = operands.copy ();
	    /* We may have changed from an SSA_NAME to a constant */
	    if (newop.opcode == SSA_NAME && TREE_CODE (op[0]) != SSA_NAME)
	      newop.opcode = TREE_CODE (op[0]);
	    newop.type = type;
	    newop.op0 = op[0];
	    newop.op1 = op[1];
	    newop.op2 = op[2];
	    newoperands[i] = newop;
	  }
	gcc_checking_assert (i == operands.length ());

	if (vuse)
	  {
	    newvuse = translate_vuse_through_block (newoperands.exists ()
						    ? newoperands : operands,
						    ref->set, ref->type,
						    vuse, phiblock, pred,
						    &same_valid);
	    if (newvuse == NULL_TREE)
	      {
		newoperands.release ();
		return NULL;
	      }
	  }

	if (changed || newvuse != vuse)
	  {
	    unsigned int new_val_id;
	    pre_expr constant;

	    tree result = vn_reference_lookup_pieces (newvuse, ref->set,
						      ref->type,
						      newoperands.exists ()
						      ? newoperands : operands,
						      &newref, VN_WALK);
	    if (result)
	      newoperands.release ();

	    /* We can always insert constants, so if we have a partial
	       redundant constant load of another type try to translate it
	       to a constant of appropriate type.  */
	    if (result && is_gimple_min_invariant (result))
	      {
		tree tem = result;
		if (!useless_type_conversion_p (ref->type, TREE_TYPE (result)))
		  {
		    tem = fold_unary (VIEW_CONVERT_EXPR, ref->type, result);
		    if (tem && !is_gimple_min_invariant (tem))
		      tem = NULL_TREE;
		  }
		if (tem)
		  return get_or_alloc_expr_for_constant (tem);
	      }

	    /* If we'd have to convert things we would need to validate
	       if we can insert the translated expression.  So fail
	       here for now - we cannot insert an alias with a different
	       type in the VN tables either, as that would assert.  */
	    if (result
		&& !useless_type_conversion_p (ref->type, TREE_TYPE (result)))
	      return NULL;
	    else if (!result && newref
		     && !useless_type_conversion_p (ref->type, newref->type))
	      {
		newoperands.release ();
		return NULL;
	      }

	    expr = pre_expr_pool.allocate ();
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
		    value_expressions.safe_grow_cleared
		      (get_max_value_id () + 1);
		  }
		else
		  new_val_id = ref->value_id;
		if (!newoperands.exists ())
		  newoperands = operands.copy ();
		newref = vn_reference_insert_pieces (newvuse, ref->set,
						     ref->type,
						     newoperands,
						     result, new_val_id);
		newoperands = vNULL;
		PRE_EXPR_REFERENCE (expr) = newref;
		constant = fully_constant_expression (expr);
		if (constant != expr)
		  return constant;
		get_or_alloc_expression_id (expr);
	      }
	    add_to_value (new_val_id, expr);
	  }
	newoperands.release ();
	return expr;
      }
      break;

    case NAME:
      {
	tree name = PRE_EXPR_NAME (expr);
	gimple *def_stmt = SSA_NAME_DEF_STMT (name);
	/* If the SSA name is defined by a PHI node in this block,
	   translate it.  */
	if (gimple_code (def_stmt) == GIMPLE_PHI
	    && gimple_bb (def_stmt) == phiblock)
	  {
	    edge e = find_edge (pred, gimple_bb (def_stmt));
	    tree def = PHI_ARG_DEF (def_stmt, e->dest_idx);

	    /* Handle constant. */
	    if (is_gimple_min_invariant (def))
	      return get_or_alloc_expr_for_constant (def);

	    return get_or_alloc_expr_for_name (def);
	  }
	/* Otherwise return it unchanged - it will get removed if its
	   value is not available in PREDs AVAIL_OUT set of expressions
	   by the subtraction of TMP_GEN.  */
	return expr;
      }

    default:
      gcc_unreachable ();
    }
}

/* Wrapper around phi_translate_1 providing caching functionality.  */

static pre_expr
phi_translate (pre_expr expr, bitmap_set_t set1, bitmap_set_t set2,
	       basic_block pred, basic_block phiblock)
{
  expr_pred_trans_t slot = NULL;
  pre_expr phitrans;

  if (!expr)
    return NULL;

  /* Constants contain no values that need translation.  */
  if (expr->kind == CONSTANT)
    return expr;

  if (value_id_constant_p (get_expr_value_id (expr)))
    return expr;

  /* Don't add translations of NAMEs as those are cheap to translate.  */
  if (expr->kind != NAME)
    {
      if (phi_trans_add (&slot, expr, pred))
	return slot->v;
      /* Store NULL for the value we want to return in the case of
	 recursing.  */
      slot->v = NULL;
    }

  /* Translate.  */
  phitrans = phi_translate_1 (expr, set1, set2, pred, phiblock);

  if (slot)
    {
      if (phitrans)
	slot->v = phitrans;
      else
	/* Remove failed translations again, they cause insert
	   iteration to not pick up new opportunities reliably.  */
	phi_translate_table->remove_elt_with_hash (slot, slot->hashcode);
    }

  return phitrans;
}


/* For each expression in SET, translate the values through phi nodes
   in PHIBLOCK using edge PHIBLOCK->PRED, and store the resulting
   expressions in DEST.  */

static void
phi_translate_set (bitmap_set_t dest, bitmap_set_t set, basic_block pred,
		   basic_block phiblock)
{
  vec<pre_expr> exprs;
  pre_expr expr;
  int i;

  if (gimple_seq_empty_p (phi_nodes (phiblock)))
    {
      bitmap_set_copy (dest, set);
      return;
    }

  exprs = sorted_array_from_bitmap_set (set);
  FOR_EACH_VEC_ELT (exprs, i, expr)
    {
      pre_expr translated;
      translated = phi_translate (expr, set, NULL, pred, phiblock);
      if (!translated)
	continue;

      /* We might end up with multiple expressions from SET being
	 translated to the same value.  In this case we do not want
	 to retain the NARY or REFERENCE expression but prefer a NAME
	 which would be the leader.  */
      if (translated->kind == NAME)
	bitmap_value_replace_in_set (dest, translated);
      else
	bitmap_value_insert_into_set (dest, translated);
    }
  exprs.release ();
}

/* Find the leader for a value (i.e., the name representing that
   value) in a given set, and return it.  Return NULL if no leader
   is found.  */

static pre_expr
bitmap_find_leader (bitmap_set_t set, unsigned int val)
{
  if (value_id_constant_p (val))
    {
      unsigned int i;
      bitmap_iterator bi;
      bitmap exprset = value_expressions[val];

      EXECUTE_IF_SET_IN_BITMAP (exprset, 0, i, bi)
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
      bitmap exprset = value_expressions[val];

      EXECUTE_IF_AND_IN_BITMAP (exprset, &set->expressions, 0, i, bi)
	return expression_for_id (i);
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
  gimple *def;
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


/* Determine if OP is valid in SET1 U SET2, which it is when the union
   contains its value-id.  */

static bool
op_valid_in_sets (bitmap_set_t set1, bitmap_set_t set2, tree op)
{
  if (op && TREE_CODE (op) == SSA_NAME)
    {
      unsigned int value_id = VN_INFO (op)->value_id;
      if (!(bitmap_set_contains_value (set1, value_id)
	    || (set2 && bitmap_set_contains_value  (set2, value_id))))
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
valid_in_sets (bitmap_set_t set1, bitmap_set_t set2, pre_expr expr)
{
  switch (expr->kind)
    {
    case NAME:
      /* By construction all NAMEs are available.  Non-available
	 NAMEs are removed by subtracting TMP_GEN from the sets.  */
      return true;
    case NARY:
      {
	unsigned int i;
	vn_nary_op_t nary = PRE_EXPR_NARY (expr);
	for (i = 0; i < nary->length; i++)
	  if (!op_valid_in_sets (set1, set2, nary->op[i]))
	    return false;
	return true;
      }
      break;
    case REFERENCE:
      {
	vn_reference_t ref = PRE_EXPR_REFERENCE (expr);
	vn_reference_op_t vro;
	unsigned int i;

	FOR_EACH_VEC_ELT (ref->operands, i, vro)
	  {
	    if (!op_valid_in_sets (set1, set2, vro->op0)
		|| !op_valid_in_sets (set1, set2, vro->op1)
		|| !op_valid_in_sets (set1, set2, vro->op2))
	      return false;
	  }
	return true;
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
dependent_clean (bitmap_set_t set1, bitmap_set_t set2)
{
  vec<pre_expr> exprs = sorted_array_from_bitmap_set (set1);
  pre_expr expr;
  int i;

  FOR_EACH_VEC_ELT (exprs, i, expr)
    {
      if (!valid_in_sets (set1, set2, expr))
	bitmap_remove_from_set (set1, expr);
    }
  exprs.release ();
}

/* Clean the set of expressions that are no longer valid in SET.  This
   means expressions that are made up of values we have no leaders for
   in SET.  */

static void
clean (bitmap_set_t set)
{
  vec<pre_expr> exprs = sorted_array_from_bitmap_set (set);
  pre_expr expr;
  int i;

  FOR_EACH_VEC_ELT (exprs, i, expr)
    {
      if (!valid_in_sets (set, NULL, expr))
	bitmap_remove_from_set (set, expr);
    }
  exprs.release ();
}

/* Clean the set of expressions that are no longer valid in SET because
   they are clobbered in BLOCK or because they trap and may not be executed.  */

static void
prune_clobbered_mems (bitmap_set_t set, basic_block block)
{
  bitmap_iterator bi;
  unsigned i;
  pre_expr to_remove = NULL;

  FOR_EACH_EXPR_ID_IN_SET (set, i, bi)
    {
      /* Remove queued expr.  */
      if (to_remove)
	{
	  bitmap_remove_from_set (set, to_remove);
	  to_remove = NULL;
	}

      pre_expr expr = expression_for_id (i);
      if (expr->kind == REFERENCE)
	{
	  vn_reference_t ref = PRE_EXPR_REFERENCE (expr);
	  if (ref->vuse)
	    {
	      gimple *def_stmt = SSA_NAME_DEF_STMT (ref->vuse);
	      if (!gimple_nop_p (def_stmt)
		  && ((gimple_bb (def_stmt) != block
		       && !dominated_by_p (CDI_DOMINATORS,
					   block, gimple_bb (def_stmt)))
		      || (gimple_bb (def_stmt) == block
			  && value_dies_in_block_x (expr, block))))
		to_remove = expr;
	    }
	}
      else if (expr->kind == NARY)
	{
	  vn_nary_op_t nary = PRE_EXPR_NARY (expr);
	  /* If the NARY may trap make sure the block does not contain
	     a possible exit point.
	     ???  This is overly conservative if we translate AVAIL_OUT
	     as the available expression might be after the exit point.  */
	  if (BB_MAY_NOTRETURN (block)
	      && vn_nary_may_trap (nary))
	    to_remove = expr;
	}
    }

  /* Remove queued expr.  */
  if (to_remove)
    bitmap_remove_from_set (set, to_remove);
}

static sbitmap has_abnormal_preds;

/* Compute the ANTIC set for BLOCK.

   If succs(BLOCK) > 1 then
     ANTIC_OUT[BLOCK] = intersection of ANTIC_IN[b] for all succ(BLOCK)
   else if succs(BLOCK) == 1 then
     ANTIC_OUT[BLOCK] = phi_translate (ANTIC_IN[succ(BLOCK)])

   ANTIC_IN[BLOCK] = clean(ANTIC_OUT[BLOCK] U EXP_GEN[BLOCK] - TMP_GEN[BLOCK])

   Note that clean() is deferred until after the iteration.  */

static bool
compute_antic_aux (basic_block block, bool block_has_abnormal_pred_edge)
{
  bitmap_set_t S, old, ANTIC_OUT;
  bitmap_iterator bi;
  unsigned int bii;
  edge e;
  edge_iterator ei;

  bool changed = ! BB_VISITED (block);
  BB_VISITED (block) = 1;
  old = ANTIC_OUT = S = NULL;

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
      gcc_assert (BB_VISITED (succ_bb));
      phi_translate_set (ANTIC_OUT, ANTIC_IN (succ_bb), block, succ_bb);
    }
  /* If we have multiple successors, we take the intersection of all of
     them.  Note that in the case of loop exit phi nodes, we may have
     phis to translate through.  */
  else
    {
      size_t i;
      basic_block bprime, first = NULL;

      auto_vec<basic_block> worklist (EDGE_COUNT (block->succs));
      FOR_EACH_EDGE (e, ei, block->succs)
	{
	  if (!first
	      && BB_VISITED (e->dest))
	    first = e->dest;
	  else if (BB_VISITED (e->dest))
	    worklist.quick_push (e->dest);
	  else
	    {
	      /* Unvisited successors get their ANTIC_IN replaced by the
		 maximal set to arrive at a maximum ANTIC_IN solution.
		 We can ignore them in the intersection operation and thus
		 need not explicitely represent that maximum solution.  */
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "ANTIC_IN is MAX on %d->%d\n",
			 e->src->index, e->dest->index);
	    }
	}

      /* Of multiple successors we have to have visited one already
         which is guaranteed by iteration order.  */
      gcc_assert (first != NULL);

      phi_translate_set (ANTIC_OUT, ANTIC_IN (first), block, first);

      FOR_EACH_VEC_ELT (worklist, i, bprime)
	{
	  if (!gimple_seq_empty_p (phi_nodes (bprime)))
	    {
	      bitmap_set_t tmp = bitmap_set_new ();
	      phi_translate_set (tmp, ANTIC_IN (bprime), block, bprime);
	      bitmap_set_and (ANTIC_OUT, tmp);
	      bitmap_set_free (tmp);
	    }
	  else
	    bitmap_set_and (ANTIC_OUT, ANTIC_IN (bprime));
	}
    }

  /* Prune expressions that are clobbered in block and thus become
     invalid if translated from ANTIC_OUT to ANTIC_IN.  */
  prune_clobbered_mems (ANTIC_OUT, block);

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

  /* clean (ANTIC_IN (block)) is defered to after the iteration converged
     because it can cause non-convergence, see for example PR81181.  */

  if (!bitmap_set_equal (old, ANTIC_IN (block)))
    changed = true;

 maybe_dump_sets:
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (ANTIC_OUT)
	print_bitmap_set (dump_file, ANTIC_OUT, "ANTIC_OUT", block->index);

      if (changed)
	fprintf (dump_file, "[changed] ");
      print_bitmap_set (dump_file, ANTIC_IN (block), "ANTIC_IN",
			block->index);

      if (S)
	print_bitmap_set (dump_file, S, "S", block->index);
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
static void
compute_partial_antic_aux (basic_block block,
			   bool block_has_abnormal_pred_edge)
{
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
      && bitmap_count_bits (&PA_IN (single_succ (block))->values) > max_pa)
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
      size_t i;
      basic_block bprime;

      auto_vec<basic_block> worklist (EDGE_COUNT (block->succs));
      FOR_EACH_EDGE (e, ei, block->succs)
	{
	  if (e->flags & EDGE_DFS_BACK)
	    continue;
	  worklist.quick_push (e->dest);
	}
      if (worklist.length () > 0)
	{
	  FOR_EACH_VEC_ELT (worklist, i, bprime)
	    {
	      unsigned int i;
	      bitmap_iterator bi;

	      FOR_EACH_EXPR_ID_IN_SET (ANTIC_IN (bprime), i, bi)
		bitmap_value_insert_into_set (PA_OUT,
					      expression_for_id (i));
	      if (!gimple_seq_empty_p (phi_nodes (bprime)))
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
    }

  /* Prune expressions that are clobbered in block and thus become
     invalid if translated from PA_OUT to PA_IN.  */
  prune_clobbered_mems (PA_OUT, block);

  /* PA_IN starts with PA_OUT - TMP_GEN.
     Then we subtract things from ANTIC_IN.  */
  PA_IN (block) = bitmap_set_subtract (PA_OUT, TMP_GEN (block));

  /* For partial antic, we want to put back in the phi results, since
     we will properly avoid making them partially antic over backedges.  */
  bitmap_ior_into (&PA_IN (block)->values, &PHI_GEN (block)->values);
  bitmap_ior_into (&PA_IN (block)->expressions, &PHI_GEN (block)->expressions);

  /* PA_IN[block] = PA_IN[block] - ANTIC_IN[block] */
  bitmap_set_subtract_values (PA_IN (block), ANTIC_IN (block));

  dependent_clean (PA_IN (block), ANTIC_IN (block));

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
}

/* Compute ANTIC and partial ANTIC sets.  */

static void
compute_antic (void)
{
  bool changed = true;
  int num_iterations = 0;
  basic_block block;
  int i;
  edge_iterator ei;
  edge e;

  /* If any predecessor edges are abnormal, we punt, so antic_in is empty.
     We pre-build the map of blocks with incoming abnormal edges here.  */
  has_abnormal_preds = sbitmap_alloc (last_basic_block_for_fn (cfun));
  bitmap_clear (has_abnormal_preds);

  FOR_ALL_BB_FN (block, cfun)
    {
      BB_VISITED (block) = 0;

      FOR_EACH_EDGE (e, ei, block->preds)
	if (e->flags & EDGE_ABNORMAL)
	  {
	    bitmap_set_bit (has_abnormal_preds, block->index);
	    break;
	  }

      /* While we are here, give empty ANTIC_IN sets to each block.  */
      ANTIC_IN (block) = bitmap_set_new ();
      if (do_partial_partial)
	PA_IN (block) = bitmap_set_new ();
    }

  /* At the exit block we anticipate nothing.  */
  BB_VISITED (EXIT_BLOCK_PTR_FOR_FN (cfun)) = 1;

  /* For ANTIC computation we need a postorder that also guarantees that
     a block with a single successor is visited after its successor.
     RPO on the inverted CFG has this property.  */
  auto_vec<int, 20> postorder;
  inverted_post_order_compute (&postorder);

  auto_sbitmap worklist (last_basic_block_for_fn (cfun) + 1);
  bitmap_clear (worklist);
  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    bitmap_set_bit (worklist, e->src->index);
  while (changed)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Starting iteration %d\n", num_iterations);
      /* ???  We need to clear our PHI translation cache here as the
         ANTIC sets shrink and we restrict valid translations to
	 those having operands with leaders in ANTIC.  Same below
	 for PA ANTIC computation.  */
      num_iterations++;
      changed = false;
      for (i = postorder.length () - 1; i >= 0; i--)
	{
	  if (bitmap_bit_p (worklist, postorder[i]))
	    {
	      basic_block block = BASIC_BLOCK_FOR_FN (cfun, postorder[i]);
	      bitmap_clear_bit (worklist, block->index);
	      if (compute_antic_aux (block,
				     bitmap_bit_p (has_abnormal_preds,
						   block->index)))
		{
		  FOR_EACH_EDGE (e, ei, block->preds)
		    bitmap_set_bit (worklist, e->src->index);
		  changed = true;
		}
	    }
	}
      /* Theoretically possible, but *highly* unlikely.  */
      gcc_checking_assert (num_iterations < 500);
    }

  /* We have to clean after the dataflow problem converged as cleaning
     can cause non-convergence because it is based on expressions
     rather than values.  */
  FOR_EACH_BB_FN (block, cfun)
    clean (ANTIC_IN (block));

  statistics_histogram_event (cfun, "compute_antic iterations",
			      num_iterations);

  if (do_partial_partial)
    {
      /* For partial antic we ignore backedges and thus we do not need
         to perform any iteration when we process blocks in postorder.  */
      int postorder_num
	= pre_and_rev_post_order_compute (NULL, postorder.address (), false);
      for (i = postorder_num - 1 ; i >= 0; i--)
	{
	  basic_block block = BASIC_BLOCK_FOR_FN (cfun, postorder[i]);
	  compute_partial_antic_aux (block,
				     bitmap_bit_p (has_abnormal_preds,
						   block->index));
	}
    }

  sbitmap_free (has_abnormal_preds);
}


/* Inserted expressions are placed onto this worklist, which is used
   for performing quick dead code elimination of insertions we made
   that didn't turn out to be necessary.   */
static bitmap inserted_exprs;

/* The actual worker for create_component_ref_by_pieces.  */

static tree
create_component_ref_by_pieces_1 (basic_block block, vn_reference_t ref,
				  unsigned int *operand, gimple_seq *stmts)
{
  vn_reference_op_t currop = &ref->operands[*operand];
  tree genop;
  ++*operand;
  switch (currop->opcode)
    {
    case CALL_EXPR:
      gcc_unreachable ();

    case MEM_REF:
      {
	tree baseop = create_component_ref_by_pieces_1 (block, ref, operand,
							stmts);
	if (!baseop)
	  return NULL_TREE;
	tree offset = currop->op0;
	if (TREE_CODE (baseop) == ADDR_EXPR
	    && handled_component_p (TREE_OPERAND (baseop, 0)))
	  {
	    HOST_WIDE_INT off;
	    tree base;
	    base = get_addr_base_and_unit_offset (TREE_OPERAND (baseop, 0),
						  &off);
	    gcc_assert (base);
	    offset = int_const_binop (PLUS_EXPR, offset,
				      build_int_cst (TREE_TYPE (offset),
						     off));
	    baseop = build_fold_addr_expr (base);
	  }
	genop = build2 (MEM_REF, currop->type, baseop, offset);
	MR_DEPENDENCE_CLIQUE (genop) = currop->clique;
	MR_DEPENDENCE_BASE (genop) = currop->base;
	REF_REVERSE_STORAGE_ORDER (genop) = currop->reverse;
	return genop;
      }

    case TARGET_MEM_REF:
      {
	tree genop0 = NULL_TREE, genop1 = NULL_TREE;
	vn_reference_op_t nextop = &ref->operands[++*operand];
	tree baseop = create_component_ref_by_pieces_1 (block, ref, operand,
							stmts);
	if (!baseop)
	  return NULL_TREE;
	if (currop->op0)
	  {
	    genop0 = find_or_generate_expression (block, currop->op0, stmts);
	    if (!genop0)
	      return NULL_TREE;
	  }
	if (nextop->op0)
	  {
	    genop1 = find_or_generate_expression (block, nextop->op0, stmts);
	    if (!genop1)
	      return NULL_TREE;
	  }
	genop = build5 (TARGET_MEM_REF, currop->type,
			baseop, currop->op2, genop0, currop->op1, genop1);

	MR_DEPENDENCE_CLIQUE (genop) = currop->clique;
	MR_DEPENDENCE_BASE (genop) = currop->base;
	return genop;
      }

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
	tree genop0 = create_component_ref_by_pieces_1 (block, ref, operand,
							stmts);
	if (!genop0)
	  return NULL_TREE;
	return fold_build1 (currop->opcode, currop->type, genop0);
      }

    case WITH_SIZE_EXPR:
      {
	tree genop0 = create_component_ref_by_pieces_1 (block, ref, operand,
							stmts);
	if (!genop0)
	  return NULL_TREE;
	tree genop1 = find_or_generate_expression (block, currop->op0, stmts);
	if (!genop1)
	  return NULL_TREE;
	return fold_build2 (currop->opcode, currop->type, genop0, genop1);
      }

    case BIT_FIELD_REF:
      {
	tree genop0 = create_component_ref_by_pieces_1 (block, ref, operand,
							stmts);
	if (!genop0)
	  return NULL_TREE;
	tree op1 = currop->op0;
	tree op2 = currop->op1;
	tree t = build3 (BIT_FIELD_REF, currop->type, genop0, op1, op2);
	REF_REVERSE_STORAGE_ORDER (t) = currop->reverse;
	return fold (t);
      }

      /* For array ref vn_reference_op's, operand 1 of the array ref
	 is op0 of the reference op and operand 3 of the array ref is
	 op1.  */
    case ARRAY_RANGE_REF:
    case ARRAY_REF:
      {
	tree genop0;
	tree genop1 = currop->op0;
	tree genop2 = currop->op1;
	tree genop3 = currop->op2;
	genop0 = create_component_ref_by_pieces_1 (block, ref, operand,
						   stmts);
	if (!genop0)
	  return NULL_TREE;
	genop1 = find_or_generate_expression (block, genop1, stmts);
	if (!genop1)
	  return NULL_TREE;
	if (genop2)
	  {
	    tree domain_type = TYPE_DOMAIN (TREE_TYPE (genop0));
	    /* Drop zero minimum index if redundant.  */
	    if (integer_zerop (genop2)
		&& (!domain_type
		    || integer_zerop (TYPE_MIN_VALUE (domain_type))))
	      genop2 = NULL_TREE;
	    else
	      {
		genop2 = find_or_generate_expression (block, genop2, stmts);
		if (!genop2)
		  return NULL_TREE;
	      }
	  }
	if (genop3)
	  {
	    tree elmt_type = TREE_TYPE (TREE_TYPE (genop0));
	    /* We can't always put a size in units of the element alignment
	       here as the element alignment may be not visible.  See
	       PR43783.  Simply drop the element size for constant
	       sizes.  */
	    if (TREE_CODE (genop3) == INTEGER_CST
		&& TREE_CODE (TYPE_SIZE_UNIT (elmt_type)) == INTEGER_CST
		&& wi::eq_p (wi::to_offset (TYPE_SIZE_UNIT (elmt_type)),
			     (wi::to_offset (genop3)
			      * vn_ref_op_align_unit (currop))))
	      genop3 = NULL_TREE;
	    else
	      {
		genop3 = find_or_generate_expression (block, genop3, stmts);
		if (!genop3)
		  return NULL_TREE;
	      }
	  }
	return build4 (currop->opcode, currop->type, genop0, genop1,
		       genop2, genop3);
      }
    case COMPONENT_REF:
      {
	tree op0;
	tree op1;
	tree genop2 = currop->op1;
	op0 = create_component_ref_by_pieces_1 (block, ref, operand, stmts);
	if (!op0)
	  return NULL_TREE;
	/* op1 should be a FIELD_DECL, which are represented by themselves.  */
	op1 = currop->op0;
	if (genop2)
	  {
	    genop2 = find_or_generate_expression (block, genop2, stmts);
	    if (!genop2)
	      return NULL_TREE;
	  }
	return fold_build3 (COMPONENT_REF, TREE_TYPE (op1), op0, op1, genop2);
      }

    case SSA_NAME:
      {
	genop = find_or_generate_expression (block, currop->op0, stmts);
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
   COMPONENT_REF or MEM_REF or ARRAY_REF portion, because we'd end up with
   trying to rename aggregates into ssa form directly, which is a no no.

   Thus, this routine doesn't create temporaries, it just builds a
   single access expression for the array, calling
   find_or_generate_expression to build the innermost pieces.

   This function is a subroutine of create_expression_by_pieces, and
   should not be called on it's own unless you really know what you
   are doing.  */

static tree
create_component_ref_by_pieces (basic_block block, vn_reference_t ref,
				gimple_seq *stmts)
{
  unsigned int op = 0;
  return create_component_ref_by_pieces_1 (block, ref, &op, stmts);
}

/* Find a simple leader for an expression, or generate one using
   create_expression_by_pieces from a NARY expression for the value.
   BLOCK is the basic_block we are looking for leaders in.
   OP is the tree expression to find a leader for or generate.
   Returns the leader or NULL_TREE on failure.  */

static tree
find_or_generate_expression (basic_block block, tree op, gimple_seq *stmts)
{
  pre_expr expr = get_or_alloc_expr_for (op);
  unsigned int lookfor = get_expr_value_id (expr);
  pre_expr leader = bitmap_find_leader (AVAIL_OUT (block), lookfor);
  if (leader)
    {
      if (leader->kind == NAME)
	return PRE_EXPR_NAME (leader);
      else if (leader->kind == CONSTANT)
	return PRE_EXPR_CONSTANT (leader);

      /* Defer.  */
      return NULL_TREE;
    }

  /* It must be a complex expression, so generate it recursively.  Note
     that this is only necessary to handle gcc.dg/tree-ssa/ssa-pre28.c
     where the insert algorithm fails to insert a required expression.  */
  bitmap exprset = value_expressions[lookfor];
  bitmap_iterator bi;
  unsigned int i;
  EXECUTE_IF_SET_IN_BITMAP (exprset, 0, i, bi)
    {
      pre_expr temp = expression_for_id (i);
      /* We cannot insert random REFERENCE expressions at arbitrary
	 places.  We can insert NARYs which eventually re-materializes
	 its operand values.  */
      if (temp->kind == NARY)
	return create_expression_by_pieces (block, temp, stmts,
					    get_expr_type (expr));
    }

  /* Defer.  */
  return NULL_TREE;
}

#define NECESSARY GF_PLF_1

/* Create an expression in pieces, so that we can handle very complex
   expressions that may be ANTIC, but not necessary GIMPLE.
   BLOCK is the basic block the expression will be inserted into,
   EXPR is the expression to insert (in value form)
   STMTS is a statement list to append the necessary insertions into.

   This function will die if we hit some value that shouldn't be
   ANTIC but is (IE there is no leader for it, or its components).
   The function returns NULL_TREE in case a different antic expression
   has to be inserted first.
   This function may also generate expressions that are themselves
   partially or fully redundant.  Those that are will be either made
   fully redundant during the next iteration of insert (for partially
   redundant ones), or eliminated by eliminate (for fully redundant
   ones).  */

static tree
create_expression_by_pieces (basic_block block, pre_expr expr,
			     gimple_seq *stmts, tree type)
{
  tree name;
  tree folded;
  gimple_seq forced_stmts = NULL;
  unsigned int value_id;
  gimple_stmt_iterator gsi;
  tree exprtype = type ? type : get_expr_type (expr);
  pre_expr nameexpr;
  gassign *newstmt;

  switch (expr->kind)
    {
    /* We may hit the NAME/CONSTANT case if we have to convert types
       that value numbering saw through.  */
    case NAME:
      folded = PRE_EXPR_NAME (expr);
      if (useless_type_conversion_p (exprtype, TREE_TYPE (folded)))
	return folded;
      break;
    case CONSTANT:
      { 
	folded = PRE_EXPR_CONSTANT (expr);
	tree tem = fold_convert (exprtype, folded);
	if (is_gimple_min_invariant (tem))
	  return tem;
	break;
      }
    case REFERENCE:
      if (PRE_EXPR_REFERENCE (expr)->operands[0].opcode == CALL_EXPR)
	{
	  vn_reference_t ref = PRE_EXPR_REFERENCE (expr);
	  unsigned int operand = 1;
	  vn_reference_op_t currop = &ref->operands[0];
	  tree sc = NULL_TREE;
	  tree fn;
	  if (TREE_CODE (currop->op0) == FUNCTION_DECL)
	    fn = currop->op0;
	  else
	    fn = find_or_generate_expression (block, currop->op0, stmts);
	  if (!fn)
	    return NULL_TREE;
	  if (currop->op1)
	    {
	      sc = find_or_generate_expression (block, currop->op1, stmts);
	      if (!sc)
		return NULL_TREE;
	    }
	  auto_vec<tree> args (ref->operands.length () - 1);
	  while (operand < ref->operands.length ())
	    {
	      tree arg = create_component_ref_by_pieces_1 (block, ref,
							   &operand, stmts);
	      if (!arg)
		return NULL_TREE;
	      args.quick_push (arg);
	    }
	  gcall *call
	    = gimple_build_call_vec ((TREE_CODE (fn) == FUNCTION_DECL
				      ? build_fold_addr_expr (fn) : fn), args);
	  gimple_call_set_with_bounds (call, currop->with_bounds);
	  if (sc)
	    gimple_call_set_chain (call, sc);
	  tree forcedname = make_ssa_name (currop->type);
	  gimple_call_set_lhs (call, forcedname);
	  gimple_set_vuse (call, BB_LIVE_VOP_ON_EXIT (block));
	  gimple_seq_add_stmt_without_update (&forced_stmts, call);
	  folded = forcedname;
	}
      else
	{
	  folded = create_component_ref_by_pieces (block,
						   PRE_EXPR_REFERENCE (expr),
						   stmts);
	  if (!folded)
	    return NULL_TREE;
	  name = make_temp_ssa_name (exprtype, NULL, "pretmp");
	  newstmt = gimple_build_assign (name, folded);
	  gimple_seq_add_stmt_without_update (&forced_stmts, newstmt);
	  gimple_set_vuse (newstmt, BB_LIVE_VOP_ON_EXIT (block));
	  folded = name;
	}
      break;
    case NARY:
      {
	vn_nary_op_t nary = PRE_EXPR_NARY (expr);
	tree *genop = XALLOCAVEC (tree, nary->length);
	unsigned i;
	for (i = 0; i < nary->length; ++i)
	  {
	    genop[i] = find_or_generate_expression (block, nary->op[i], stmts);
	    if (!genop[i])
	      return NULL_TREE;
	    /* Ensure genop[] is properly typed for POINTER_PLUS_EXPR.  It
	       may have conversions stripped.  */
	    if (nary->opcode == POINTER_PLUS_EXPR)
	      {
		if (i == 0)
		  genop[i] = gimple_convert (&forced_stmts,
					     nary->type, genop[i]);
		else if (i == 1)
		  genop[i] = gimple_convert (&forced_stmts,
					     sizetype, genop[i]);
	      }
	    else
	      genop[i] = gimple_convert (&forced_stmts,
					 TREE_TYPE (nary->op[i]), genop[i]);
	  }
	if (nary->opcode == CONSTRUCTOR)
	  {
	    vec<constructor_elt, va_gc> *elts = NULL;
	    for (i = 0; i < nary->length; ++i)
	      CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE, genop[i]);
	    folded = build_constructor (nary->type, elts);
	    name = make_temp_ssa_name (exprtype, NULL, "pretmp");
	    newstmt = gimple_build_assign (name, folded);
	    gimple_seq_add_stmt_without_update (&forced_stmts, newstmt);
	    folded = name;
	  }
	else
	  {
	    switch (nary->length)
	      {
	      case 1:
		folded = gimple_build (&forced_stmts, nary->opcode, nary->type,
				       genop[0]);
		break;
	      case 2:
		folded = gimple_build (&forced_stmts, nary->opcode, nary->type,
				       genop[0], genop[1]);
		break;
	      case 3:
		folded = gimple_build (&forced_stmts, nary->opcode, nary->type,
				       genop[0], genop[1], genop[2]);
		break;
	      default:
		gcc_unreachable ();
	      }
	  }
      }
      break;
    default:
      gcc_unreachable ();
    }

  folded = gimple_convert (&forced_stmts, exprtype, folded);

  /* If there is nothing to insert, return the simplified result.  */
  if (gimple_seq_empty_p (forced_stmts))
    return folded;
  /* If we simplified to a constant return it and discard eventually
     built stmts.  */
  if (is_gimple_min_invariant (folded))
    {
      gimple_seq_discard (forced_stmts);
      return folded;
    }
  /* Likewise if we simplified to sth not queued for insertion.  */
  bool found = false;
  gsi = gsi_last (forced_stmts);
  for (; !gsi_end_p (gsi); gsi_prev (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      tree forcedname = gimple_get_lhs (stmt);
      if (forcedname == folded)
	{
	  found = true;
	  break;
	}
    }
  if (! found)
    {
      gimple_seq_discard (forced_stmts);
      return folded;
    }
  gcc_assert (TREE_CODE (folded) == SSA_NAME);

  /* If we have any intermediate expressions to the value sets, add them
     to the value sets and chain them in the instruction stream.  */
  if (forced_stmts)
    {
      gsi = gsi_start (forced_stmts);
      for (; !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  tree forcedname = gimple_get_lhs (stmt);
	  pre_expr nameexpr;

	  if (forcedname != folded)
	    {
	      VN_INFO_GET (forcedname)->valnum = forcedname;
	      VN_INFO (forcedname)->value_id = get_next_value_id ();
	      nameexpr = get_or_alloc_expr_for_name (forcedname);
	      add_to_value (VN_INFO (forcedname)->value_id, nameexpr);
	      bitmap_value_replace_in_set (NEW_SETS (block), nameexpr);
	      bitmap_value_replace_in_set (AVAIL_OUT (block), nameexpr);
	    }

	  bitmap_set_bit (inserted_exprs, SSA_NAME_VERSION (forcedname));
	  gimple_set_plf (stmt, NECESSARY, false);
	}
      gimple_seq_add_seq (stmts, forced_stmts);
    }

  name = folded;

  /* Fold the last statement.  */
  gsi = gsi_last (*stmts);
  if (fold_stmt_inplace (&gsi))
    update_stmt (gsi_stmt (gsi));

  /* Add a value number to the temporary.
     The value may already exist in either NEW_SETS, or AVAIL_OUT, because
     we are creating the expression by pieces, and this particular piece of
     the expression may have been represented.  There is no harm in replacing
     here.  */
  value_id = get_expr_value_id (expr);
  VN_INFO_GET (name)->value_id = value_id;
  VN_INFO (name)->valnum = sccvn_valnum_from_value_id (value_id);
  if (VN_INFO (name)->valnum == NULL_TREE)
    VN_INFO (name)->valnum = name;
  gcc_assert (VN_INFO (name)->valnum != NULL_TREE);
  nameexpr = get_or_alloc_expr_for_name (name);
  add_to_value (value_id, nameexpr);
  if (NEW_SETS (block))
    bitmap_value_replace_in_set (NEW_SETS (block), nameexpr);
  bitmap_value_replace_in_set (AVAIL_OUT (block), nameexpr);

  pre_stats.insertions++;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Inserted ");
      print_gimple_stmt (dump_file, gsi_stmt (gsi_last (*stmts)), 0);
      fprintf (dump_file, " in predecessor %d (%04d)\n",
	       block->index, value_id);
    }

  return name;
}


/* Insert the to-be-made-available values of expression EXPRNUM for each
   predecessor, stored in AVAIL, into the predecessors of BLOCK, and
   merge the result with a phi node, given the same value number as
   NODE.  Return true if we have inserted new stuff.  */

static bool
insert_into_preds_of_block (basic_block block, unsigned int exprnum,
			    vec<pre_expr> avail)
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
  gphi *phi;

  /* Make sure we aren't creating an induction variable.  */
  if (bb_loop_depth (block) > 0 && EDGE_COUNT (block->preds) == 2)
    {
      bool firstinsideloop = false;
      bool secondinsideloop = false;
      firstinsideloop = flow_bb_inside_loop_p (block->loop_father,
					       EDGE_PRED (block, 0)->src);
      secondinsideloop = flow_bb_inside_loop_p (block->loop_father,
						EDGE_PRED (block, 1)->src);
      /* Induction variables only have one edge inside the loop.  */
      if ((firstinsideloop ^ secondinsideloop)
	  && expr->kind != REFERENCE)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Skipping insertion of phi for partial redundancy: Looks like an induction variable\n");
	  nophi = true;
	}
    }

  /* Make the necessary insertions.  */
  FOR_EACH_EDGE (pred, ei, block->preds)
    {
      gimple_seq stmts = NULL;
      tree builtexpr;
      bprime = pred->src;
      eprime = avail[pred->dest_idx];
      builtexpr = create_expression_by_pieces (bprime, eprime,
					       &stmts, type);
      gcc_assert (!(pred->flags & EDGE_ABNORMAL));
      if (!gimple_seq_empty_p (stmts))
	{
	  gsi_insert_seq_on_edge (pred, stmts);
	  insertions = true;
	}
      if (!builtexpr)
	{
	  /* We cannot insert a PHI node if we failed to insert
	     on one edge.  */
	  nophi = true;
	  continue;
	}
      if (is_gimple_min_invariant (builtexpr))
	avail[pred->dest_idx] = get_or_alloc_expr_for_constant (builtexpr);
      else
	avail[pred->dest_idx] = get_or_alloc_expr_for_name (builtexpr);
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
  temp = make_temp_ssa_name (type, NULL, "prephitmp");
  phi = create_phi_node (temp, block);

  gimple_set_plf (phi, NECESSARY, false);
  VN_INFO_GET (temp)->value_id = val;
  VN_INFO (temp)->valnum = sccvn_valnum_from_value_id (val);
  if (VN_INFO (temp)->valnum == NULL_TREE)
    VN_INFO (temp)->valnum = temp;
  bitmap_set_bit (inserted_exprs, SSA_NAME_VERSION (temp));
  FOR_EACH_EDGE (pred, ei, block->preds)
    {
      pre_expr ae = avail[pred->dest_idx];
      gcc_assert (get_expr_type (ae) == type
		  || useless_type_conversion_p (type, get_expr_type (ae)));
      if (ae->kind == CONSTANT)
	add_phi_arg (phi, unshare_expr (PRE_EXPR_CONSTANT (ae)),
		     pred, UNKNOWN_LOCATION);
      else
	add_phi_arg (phi, PRE_EXPR_NAME (ae), pred, UNKNOWN_LOCATION);
    }

  newphi = get_or_alloc_expr_for_name (temp);
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

  /* If we insert a PHI node for a conversion of another PHI node
     in the same basic-block try to preserve range information.
     This is important so that followup loop passes receive optimal
     number of iteration analysis results.  See PR61743.  */
  if (expr->kind == NARY
      && CONVERT_EXPR_CODE_P (expr->u.nary->opcode)
      && TREE_CODE (expr->u.nary->op[0]) == SSA_NAME
      && gimple_bb (SSA_NAME_DEF_STMT (expr->u.nary->op[0])) == block
      && INTEGRAL_TYPE_P (type)
      && INTEGRAL_TYPE_P (TREE_TYPE (expr->u.nary->op[0]))
      && (TYPE_PRECISION (type)
	  >= TYPE_PRECISION (TREE_TYPE (expr->u.nary->op[0])))
      && SSA_NAME_RANGE_INFO (expr->u.nary->op[0]))
    {
      wide_int min, max;
      if (get_range_info (expr->u.nary->op[0], &min, &max) == VR_RANGE
	  && !wi::neg_p (min, SIGNED)
	  && !wi::neg_p (max, SIGNED))
	/* Just handle extension and sign-changes of all-positive ranges.  */
	set_range_info (temp,
			SSA_NAME_RANGE_TYPE (expr->u.nary->op[0]),
			wide_int_storage::from (min, TYPE_PRECISION (type),
						TYPE_SIGN (type)),
			wide_int_storage::from (max, TYPE_PRECISION (type),
						TYPE_SIGN (type)));
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Created phi ");
      print_gimple_stmt (dump_file, phi, 0);
      fprintf (dump_file, " in block %d (%04d)\n", block->index, val);
    }
  pre_stats.phis++;
  return true;
}



/* Perform insertion of partially redundant or hoistable values.
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
   If the block has multiple successors,
       3a. Iterate over the ANTIC values for the block to see if
	   any of them are good candidates for hoisting.
       3b. If so, insert expressions computing the values in BLOCK,
	   and add the new expressions into the NEW_SETS set.
   4. Recursively call ourselves on the dominator children of BLOCK.

   Steps 1, 2a, and 4 are done by insert_aux. 2b, 2c and 2d are done by
   do_pre_regular_insertion and do_partial_insertion.  3a and 3b are
   done in do_hoist_insertion.
*/

static bool
do_pre_regular_insertion (basic_block block, basic_block dom)
{
  bool new_stuff = false;
  vec<pre_expr> exprs;
  pre_expr expr;
  auto_vec<pre_expr> avail;
  int i;

  exprs = sorted_array_from_bitmap_set (ANTIC_IN (block));
  avail.safe_grow (EDGE_COUNT (block->preds));

  FOR_EACH_VEC_ELT (exprs, i, expr)
    {
      if (expr->kind == NARY
	  || expr->kind == REFERENCE)
	{
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
		{
		  fprintf (dump_file, "Found fully redundant value: ");
		  print_pre_expr (dump_file, expr);
		  fprintf (dump_file, "\n");
		}
	      continue;
	    }

	  FOR_EACH_EDGE (pred, ei, block->preds)
	    {
	      unsigned int vprime;

	      /* We should never run insertion for the exit block
	         and so not come across fake pred edges.  */
	      gcc_assert (!(pred->flags & EDGE_FAKE));
	      bprime = pred->src;
	      /* We are looking at ANTIC_OUT of bprime.  */
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
		  avail[pred->dest_idx] = NULL;
		  cant_insert = true;
		  break;
		}

	      vprime = get_expr_value_id (eprime);
	      edoubleprime = bitmap_find_leader (AVAIL_OUT (bprime),
						 vprime);
	      if (edoubleprime == NULL)
		{
		  avail[pred->dest_idx] = eprime;
		  all_same = false;
		}
	      else
		{
		  avail[pred->dest_idx] = edoubleprime;
		  by_some = true;
		  /* We want to perform insertions to remove a redundancy on
		     a path in the CFG we want to optimize for speed.  */
		  if (optimize_edge_for_speed_p (pred))
		    do_insertion = true;
		  if (first_s == NULL)
		    first_s = edoubleprime;
		  else if (!pre_expr_d::equal (first_s, edoubleprime))
		    all_same = false;
		}
	    }
	  /* If we can insert it, it's not the same value
	     already existing along every predecessor, and
	     it's defined by some predecessor, it is
	     partially redundant.  */
	  if (!cant_insert && !all_same && by_some)
	    {
	      if (!do_insertion)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Skipping partial redundancy for "
			       "expression ");
		      print_pre_expr (dump_file, expr);
		      fprintf (dump_file, " (%04d), no redundancy on to be "
			       "optimized for speed edge\n", val);
		    }
		}
	      else if (dbg_cnt (treepre_insert))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Found partial redundancy for "
			       "expression ");
		      print_pre_expr (dump_file, expr);
		      fprintf (dump_file, " (%04d)\n",
			       get_expr_value_id (expr));
		    }
		  if (insert_into_preds_of_block (block,
						  get_expression_id (expr),
						  avail))
		    new_stuff = true;
		}
	    }
	  /* If all edges produce the same value and that value is
	     an invariant, then the PHI has the same value on all
	     edges.  Note this.  */
	  else if (!cant_insert && all_same)
	    {
	      gcc_assert (edoubleprime->kind == CONSTANT
			  || edoubleprime->kind == NAME);

	      tree temp = make_temp_ssa_name (get_expr_type (expr),
					      NULL, "pretmp");
	      gassign *assign
		= gimple_build_assign (temp,
				       edoubleprime->kind == CONSTANT ?
				       PRE_EXPR_CONSTANT (edoubleprime) :
				       PRE_EXPR_NAME (edoubleprime));
	      gimple_stmt_iterator gsi = gsi_after_labels (block);
	      gsi_insert_before (&gsi, assign, GSI_NEW_STMT);

	      gimple_set_plf (assign, NECESSARY, false);
	      VN_INFO_GET (temp)->value_id = val;
	      VN_INFO (temp)->valnum = sccvn_valnum_from_value_id (val);
	      if (VN_INFO (temp)->valnum == NULL_TREE)
		VN_INFO (temp)->valnum = temp;
	      bitmap_set_bit (inserted_exprs, SSA_NAME_VERSION (temp));
	      pre_expr newe = get_or_alloc_expr_for_name (temp);
	      add_to_value (val, newe);
	      bitmap_value_replace_in_set (AVAIL_OUT (block), newe);
	      bitmap_insert_into_set (NEW_SETS (block), newe);
	    }
	}
    }

  exprs.release ();
  return new_stuff;
}


/* Perform insertion for partially anticipatable expressions.  There
   is only one case we will perform insertion for these.  This case is
   if the expression is partially anticipatable, and fully available.
   In this case, we know that putting it earlier will enable us to
   remove the later computation.  */

static bool
do_pre_partial_partial_insertion (basic_block block, basic_block dom)
{
  bool new_stuff = false;
  vec<pre_expr> exprs;
  pre_expr expr;
  auto_vec<pre_expr> avail;
  int i;

  exprs = sorted_array_from_bitmap_set (PA_IN (block));
  avail.safe_grow (EDGE_COUNT (block->preds));

  FOR_EACH_VEC_ELT (exprs, i, expr)
    {
      if (expr->kind == NARY
	  || expr->kind == REFERENCE)
	{
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
		  avail[pred->dest_idx] = NULL;
		  cant_insert = true;
		  break;
		}

	      vprime = get_expr_value_id (eprime);
	      edoubleprime = bitmap_find_leader (AVAIL_OUT (bprime), vprime);
	      avail[pred->dest_idx] = edoubleprime;
	      if (edoubleprime == NULL)
		{
		  by_all = false;
		  break;
		}
	    }

	  /* If we can insert it, it's not the same value
	     already existing along every predecessor, and
	     it's defined by some predecessor, it is
	     partially redundant.  */
	  if (!cant_insert && by_all)
	    {
	      edge succ;
	      bool do_insertion = false;

	      /* Insert only if we can remove a later expression on a path
		 that we want to optimize for speed.
		 The phi node that we will be inserting in BLOCK is not free,
		 and inserting it for the sake of !optimize_for_speed successor
		 may cause regressions on the speed path.  */
	      FOR_EACH_EDGE (succ, ei, block->succs)
		{
		  if (bitmap_set_contains_value (PA_IN (succ->dest), val)
		      || bitmap_set_contains_value (ANTIC_IN (succ->dest), val))
		    {
		      if (optimize_edge_for_speed_p (succ))
			do_insertion = true;
		    }
		}

	      if (!do_insertion)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Skipping partial partial redundancy "
			       "for expression ");
		      print_pre_expr (dump_file, expr);
		      fprintf (dump_file, " (%04d), not (partially) anticipated "
			       "on any to be optimized for speed edges\n", val);
		    }
		}
	      else if (dbg_cnt (treepre_insert))
		{
		  pre_stats.pa_insert++;
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Found partial partial redundancy "
			       "for expression ");
		      print_pre_expr (dump_file, expr);
		      fprintf (dump_file, " (%04d)\n",
			       get_expr_value_id (expr));
		    }
		  if (insert_into_preds_of_block (block,
						  get_expression_id (expr),
						  avail))
		    new_stuff = true;
		}	   
	    } 
	}
    }

  exprs.release ();
  return new_stuff;
}

/* Insert expressions in BLOCK to compute hoistable values up.
   Return TRUE if something was inserted, otherwise return FALSE.
   The caller has to make sure that BLOCK has at least two successors.  */

static bool
do_hoist_insertion (basic_block block)
{
  edge e;
  edge_iterator ei;
  bool new_stuff = false;
  unsigned i;
  gimple_stmt_iterator last;

  /* At least two successors, or else...  */
  gcc_assert (EDGE_COUNT (block->succs) >= 2);

  /* Check that all successors of BLOCK are dominated by block.
     We could use dominated_by_p() for this, but actually there is a much
     quicker check: any successor that is dominated by BLOCK can't have
     more than one predecessor edge.  */
  FOR_EACH_EDGE (e, ei, block->succs)
    if (! single_pred_p (e->dest))
      return false;

  /* Determine the insertion point.  If we cannot safely insert before
     the last stmt if we'd have to, bail out.  */
  last = gsi_last_bb (block);
  if (!gsi_end_p (last)
      && !is_ctrl_stmt (gsi_stmt (last))
      && stmt_ends_bb_p (gsi_stmt (last)))
    return false;

  /* Compute the set of hoistable expressions from ANTIC_IN.  First compute
     hoistable values.  */
  bitmap_set hoistable_set;

  /* A hoistable value must be in ANTIC_IN(block)
     but not in AVAIL_OUT(BLOCK).  */
  bitmap_initialize (&hoistable_set.values, &grand_bitmap_obstack);
  bitmap_and_compl (&hoistable_set.values,
		    &ANTIC_IN (block)->values, &AVAIL_OUT (block)->values);

  /* Short-cut for a common case: hoistable_set is empty.  */
  if (bitmap_empty_p (&hoistable_set.values))
    return false;

  /* Compute which of the hoistable values is in AVAIL_OUT of
     at least one of the successors of BLOCK.  */
  bitmap_head availout_in_some;
  bitmap_initialize (&availout_in_some, &grand_bitmap_obstack);
  FOR_EACH_EDGE (e, ei, block->succs)
    /* Do not consider expressions solely because their availability
       on loop exits.  They'd be ANTIC-IN throughout the whole loop
       and thus effectively hoisted across loops by combination of
       PRE and hoisting.  */
    if (! loop_exit_edge_p (block->loop_father, e))
      bitmap_ior_and_into (&availout_in_some, &hoistable_set.values,
			   &AVAIL_OUT (e->dest)->values);
  bitmap_clear (&hoistable_set.values);

  /* Short-cut for a common case: availout_in_some is empty.  */
  if (bitmap_empty_p (&availout_in_some))
    return false;

  /* Hack hoitable_set in-place so we can use sorted_array_from_bitmap_set.  */
  hoistable_set.values = availout_in_some;
  hoistable_set.expressions = ANTIC_IN (block)->expressions;

  /* Now finally construct the topological-ordered expression set.  */
  vec<pre_expr> exprs = sorted_array_from_bitmap_set (&hoistable_set);

  bitmap_clear (&hoistable_set.values);

  /* If there are candidate values for hoisting, insert expressions
     strategically to make the hoistable expressions fully redundant.  */
  pre_expr expr;
  FOR_EACH_VEC_ELT (exprs, i, expr)
    {
      /* While we try to sort expressions topologically above the
         sorting doesn't work out perfectly.  Catch expressions we
	 already inserted.  */
      unsigned int value_id = get_expr_value_id (expr);
      if (bitmap_set_contains_value (AVAIL_OUT (block), value_id))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file,
		       "Already inserted expression for ");
	      print_pre_expr (dump_file, expr);
	      fprintf (dump_file, " (%04d)\n", value_id);
	    }
	  continue;
	}

      /* OK, we should hoist this value.  Perform the transformation.  */
      pre_stats.hoist_insert++;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file,
		   "Inserting expression in block %d for code hoisting: ",
		   block->index);
	  print_pre_expr (dump_file, expr);
	  fprintf (dump_file, " (%04d)\n", value_id);
	}

      gimple_seq stmts = NULL;
      tree res = create_expression_by_pieces (block, expr, &stmts,
					      get_expr_type (expr));

      /* Do not return true if expression creation ultimately
         did not insert any statements.  */
      if (gimple_seq_empty_p (stmts))
	res = NULL_TREE;
      else
	{
	  if (gsi_end_p (last) || is_ctrl_stmt (gsi_stmt (last)))
	    gsi_insert_seq_before (&last, stmts, GSI_SAME_STMT);
	  else
	    gsi_insert_seq_after (&last, stmts, GSI_NEW_STMT);
	}

      /* Make sure to not return true if expression creation ultimately
         failed but also make sure to insert any stmts produced as they
	 are tracked in inserted_exprs.  */
      if (! res)
	continue;

      new_stuff = true;
    }

  exprs.release ();

  return new_stuff;
}

/* Do a dominator walk on the control flow graph, and insert computations
   of values as necessary for PRE and hoisting.  */

static bool
insert_aux (basic_block block, bool do_pre, bool do_hoist)
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
	  bitmap_set_t newset;

	  /* First, update the AVAIL_OUT set with anything we may have
	     inserted higher up in the dominator tree.  */
	  newset = NEW_SETS (dom);
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

	  /* Insert expressions for partial redundancies.  */
	  if (do_pre && !single_pred_p (block))
	    {
	      new_stuff |= do_pre_regular_insertion (block, dom);
	      if (do_partial_partial)
		new_stuff |= do_pre_partial_partial_insertion (block, dom);
	    }

	  /* Insert expressions for hoisting.  */
	  if (do_hoist && EDGE_COUNT (block->succs) >= 2)
	    new_stuff |= do_hoist_insertion (block);
	}
    }
  for (son = first_dom_son (CDI_DOMINATORS, block);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    {
      new_stuff |= insert_aux (son, do_pre, do_hoist);
    }

  return new_stuff;
}

/* Perform insertion of partially redundant and hoistable values.  */

static void
insert (void)
{
  bool new_stuff = true;
  basic_block bb;
  int num_iterations = 0;

  FOR_ALL_BB_FN (bb, cfun)
    NEW_SETS (bb) = bitmap_set_new ();

  while (new_stuff)
    {
      num_iterations++;
      if (dump_file && dump_flags & TDF_DETAILS)
	fprintf (dump_file, "Starting insert iteration %d\n", num_iterations);
      new_stuff = insert_aux (ENTRY_BLOCK_PTR_FOR_FN (cfun), flag_tree_pre,
			      flag_code_hoisting);

      /* Clear the NEW sets before the next iteration.  We have already
         fully propagated its contents.  */
      if (new_stuff)
	FOR_ALL_BB_FN (bb, cfun)
	  bitmap_set_free (NEW_SETS (bb));
    }
  statistics_histogram_event (cfun, "insert iterations", num_iterations);
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
  tree name;

  /* We pretend that default definitions are defined in the entry block.
     This includes function arguments and the static chain decl.  */
  FOR_EACH_SSA_NAME (i, name, cfun)
    {
      pre_expr e;
      if (!SSA_NAME_IS_DEFAULT_DEF (name)
	  || has_zero_uses (name)
	  || virtual_operand_p (name))
	continue;

      e = get_or_alloc_expr_for_name (name);
      add_to_value (get_expr_value_id (e), e);
      bitmap_insert_into_set (TMP_GEN (ENTRY_BLOCK_PTR_FOR_FN (cfun)), e);
      bitmap_value_insert_into_set (AVAIL_OUT (ENTRY_BLOCK_PTR_FOR_FN (cfun)),
				    e);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      print_bitmap_set (dump_file, TMP_GEN (ENTRY_BLOCK_PTR_FOR_FN (cfun)),
			"tmp_gen", ENTRY_BLOCK);
      print_bitmap_set (dump_file, AVAIL_OUT (ENTRY_BLOCK_PTR_FOR_FN (cfun)),
			"avail_out", ENTRY_BLOCK);
    }

  /* Allocate the worklist.  */
  worklist = XNEWVEC (basic_block, n_basic_blocks_for_fn (cfun));

  /* Seed the algorithm by putting the dominator children of the entry
     block on the worklist.  */
  for (son = first_dom_son (CDI_DOMINATORS, ENTRY_BLOCK_PTR_FOR_FN (cfun));
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    worklist[sp++] = son;

  BB_LIVE_VOP_ON_EXIT (ENTRY_BLOCK_PTR_FOR_FN (cfun))
    = ssa_default_def (cfun, gimple_vop (cfun));

  /* Loop until the worklist is empty.  */
  while (sp)
    {
      gimple *stmt;
      basic_block dom;

      /* Pick a block from the worklist.  */
      block = worklist[--sp];

      /* Initially, the set of available values in BLOCK is that of
	 its immediate dominator.  */
      dom = get_immediate_dominator (CDI_DOMINATORS, block);
      if (dom)
	{
	  bitmap_set_copy (AVAIL_OUT (block), AVAIL_OUT (dom));
	  BB_LIVE_VOP_ON_EXIT (block) = BB_LIVE_VOP_ON_EXIT (dom);
	}

      /* Generate values for PHI nodes.  */
      for (gphi_iterator gsi = gsi_start_phis (block); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  tree result = gimple_phi_result (gsi.phi ());

	  /* We have no need for virtual phis, as they don't represent
	     actual computations.  */
	  if (virtual_operand_p (result))
	    {
	      BB_LIVE_VOP_ON_EXIT (block) = result;
	      continue;
	    }

	  pre_expr e = get_or_alloc_expr_for_name (result);
	  add_to_value (get_expr_value_id (e), e);
	  bitmap_value_insert_into_set (AVAIL_OUT (block), e);
	  bitmap_insert_into_set (PHI_GEN (block), e);
	}

      BB_MAY_NOTRETURN (block) = 0;

      /* Now compute value numbers and populate value sets with all
	 the expressions computed in BLOCK.  */
      for (gimple_stmt_iterator gsi = gsi_start_bb (block); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  ssa_op_iter iter;
	  tree op;

	  stmt = gsi_stmt (gsi);

	  /* Cache whether the basic-block has any non-visible side-effect
	     or control flow.
	     If this isn't a call or it is the last stmt in the
	     basic-block then the CFG represents things correctly.  */
	  if (is_gimple_call (stmt) && !stmt_ends_bb_p (stmt))
	    {
	      /* Non-looping const functions always return normally.
		 Otherwise the call might not return or have side-effects
		 that forbids hoisting possibly trapping expressions
		 before it.  */
	      int flags = gimple_call_flags (stmt);
	      if (!(flags & ECF_CONST)
		  || (flags & ECF_LOOPING_CONST_OR_PURE))
		BB_MAY_NOTRETURN (block) = 1;
	    }

	  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_DEF)
	    {
	      pre_expr e = get_or_alloc_expr_for_name (op);

	      add_to_value (get_expr_value_id (e), e);
	      bitmap_insert_into_set (TMP_GEN (block), e);
	      bitmap_value_insert_into_set (AVAIL_OUT (block), e);
	    }

	  if (gimple_vdef (stmt))
	    BB_LIVE_VOP_ON_EXIT (block) = gimple_vdef (stmt);

	  if (gimple_has_side_effects (stmt)
	      || stmt_could_throw_p (stmt)
	      || is_gimple_debug (stmt))
	    continue;

	  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE)
	    {
	      if (ssa_undefined_value_p (op))
		continue;
	      pre_expr e = get_or_alloc_expr_for_name (op);
	      bitmap_value_insert_into_set (EXP_GEN (block), e);
	    }

	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_RETURN:
	      continue;

	    case GIMPLE_CALL:
	      {
		vn_reference_t ref;
		vn_reference_s ref1;
		pre_expr result = NULL;

		/* We can value number only calls to real functions.  */
		if (gimple_call_internal_p (stmt))
		  continue;

		vn_reference_lookup_call (as_a <gcall *> (stmt), &ref, &ref1);
		if (!ref)
		  continue;

		/* If the value of the call is not invalidated in
		   this block until it is computed, add the expression
		   to EXP_GEN.  */
		if (!gimple_vuse (stmt)
		    || gimple_code
		         (SSA_NAME_DEF_STMT (gimple_vuse (stmt))) == GIMPLE_PHI
		    || gimple_bb (SSA_NAME_DEF_STMT
				    (gimple_vuse (stmt))) != block)
		  {
		    result = pre_expr_pool.allocate ();
		    result->kind = REFERENCE;
		    result->id = 0;
		    PRE_EXPR_REFERENCE (result) = ref;

		    get_or_alloc_expression_id (result);
		    add_to_value (get_expr_value_id (result), result);
		    bitmap_value_insert_into_set (EXP_GEN (block), result);
		  }
		continue;
	      }

	    case GIMPLE_ASSIGN:
	      {
		pre_expr result = NULL;
		switch (vn_get_stmt_kind (stmt))
		  {
		  case VN_NARY:
		    {
		      enum tree_code code = gimple_assign_rhs_code (stmt);
		      vn_nary_op_t nary;

		      /* COND_EXPR and VEC_COND_EXPR are awkward in
			 that they contain an embedded complex expression.
			 Don't even try to shove those through PRE.  */
		      if (code == COND_EXPR
			  || code == VEC_COND_EXPR)
			continue;

		      vn_nary_op_lookup_stmt (stmt, &nary);
		      if (!nary)
			continue;

		      /* If the NARY traps and there was a preceding
		         point in the block that might not return avoid
			 adding the nary to EXP_GEN.  */
		      if (BB_MAY_NOTRETURN (block)
			  && vn_nary_may_trap (nary))
			continue;

		      result = pre_expr_pool.allocate ();
		      result->kind = NARY;
		      result->id = 0;
		      PRE_EXPR_NARY (result) = nary;
		      break;
		    }

		  case VN_REFERENCE:
		    {
		      tree rhs1 = gimple_assign_rhs1 (stmt);
		      alias_set_type set = get_alias_set (rhs1);
		      vec<vn_reference_op_s> operands
			= vn_reference_operands_for_lookup (rhs1);
		      vn_reference_t ref;
		      vn_reference_lookup_pieces (gimple_vuse (stmt), set,
						  TREE_TYPE (rhs1),
						  operands, &ref, VN_WALK);
		      if (!ref)
			{
			  operands.release ();
			  continue;
			}

		      /* If the value of the reference is not invalidated in
			 this block until it is computed, add the expression
			 to EXP_GEN.  */
		      if (gimple_vuse (stmt))
			{
			  gimple *def_stmt;
			  bool ok = true;
			  def_stmt = SSA_NAME_DEF_STMT (gimple_vuse (stmt));
			  while (!gimple_nop_p (def_stmt)
				 && gimple_code (def_stmt) != GIMPLE_PHI
				 && gimple_bb (def_stmt) == block)
			    {
			      if (stmt_may_clobber_ref_p
				    (def_stmt, gimple_assign_rhs1 (stmt)))
				{
				  ok = false;
				  break;
				}
			      def_stmt
				= SSA_NAME_DEF_STMT (gimple_vuse (def_stmt));
			    }
			  if (!ok)
			    {
			      operands.release ();
			      continue;
			    }
			}

		      /* If the load was value-numbered to another
			 load make sure we do not use its expression
			 for insertion if it wouldn't be a valid
			 replacement.  */
		      /* At the momemt we have a testcase
			 for hoist insertion of aligned vs. misaligned
			 variants in gcc.dg/torture/pr65270-1.c thus
			 with just alignment to be considered we can
			 simply replace the expression in the hashtable
			 with the most conservative one.  */
		      vn_reference_op_t ref1 = &ref->operands.last ();
		      while (ref1->opcode != TARGET_MEM_REF
			     && ref1->opcode != MEM_REF
			     && ref1 != &ref->operands[0])
			--ref1;
		      vn_reference_op_t ref2 = &operands.last ();
		      while (ref2->opcode != TARGET_MEM_REF
			     && ref2->opcode != MEM_REF
			     && ref2 != &operands[0])
			--ref2;
		      if ((ref1->opcode == TARGET_MEM_REF
			   || ref1->opcode == MEM_REF)
			  && (TYPE_ALIGN (ref1->type)
			      > TYPE_ALIGN (ref2->type)))
			ref1->type
			  = build_aligned_type (ref1->type,
						TYPE_ALIGN (ref2->type));
		      /* TBAA behavior is an obvious part so make sure
		         that the hashtable one covers this as well
			 by adjusting the ref alias set and its base.  */
		      if (ref->set == set
			  || alias_set_subset_of (set, ref->set))
			;
		      else if (alias_set_subset_of (ref->set, set))
			{
			  ref->set = set;
			  if (ref1->opcode == MEM_REF)
			    ref1->op0 = wide_int_to_tree (TREE_TYPE (ref2->op0),
							  ref1->op0);
			  else
			    ref1->op2 = wide_int_to_tree (TREE_TYPE (ref2->op2),
							  ref1->op2);
			}
		      else
			{
			  ref->set = 0;
			  if (ref1->opcode == MEM_REF)
			    ref1->op0 = wide_int_to_tree (ptr_type_node,
							  ref1->op0);
			  else
			    ref1->op2 = wide_int_to_tree (ptr_type_node,
							  ref1->op2);
			}
		      operands.release ();

		      result = pre_expr_pool.allocate ();
		      result->kind = REFERENCE;
		      result->id = 0;
		      PRE_EXPR_REFERENCE (result) = ref;
		      break;
		    }

		  default:
		    continue;
		  }

		get_or_alloc_expression_id (result);
		add_to_value (get_expr_value_id (result), result);
		bitmap_value_insert_into_set (EXP_GEN (block), result);
		continue;
	      }
	    default:
	      break;
	    }
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  print_bitmap_set (dump_file, EXP_GEN (block),
			    "exp_gen", block->index);
	  print_bitmap_set (dump_file, PHI_GEN (block),
			    "phi_gen", block->index);
	  print_bitmap_set (dump_file, TMP_GEN (block),
			    "tmp_gen", block->index);
	  print_bitmap_set (dump_file, AVAIL_OUT (block),
			    "avail_out", block->index);
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


/* Local state for the eliminate domwalk.  */
static vec<gimple *> el_to_remove;
static vec<gimple *> el_to_fixup;
static unsigned int el_todo;
static vec<tree> el_avail;
static vec<tree> el_avail_stack;

/* Return a leader for OP that is available at the current point of the
   eliminate domwalk.  */

static tree
eliminate_avail (tree op)
{
  tree valnum = VN_INFO (op)->valnum;
  if (TREE_CODE (valnum) == SSA_NAME)
    {
      if (SSA_NAME_IS_DEFAULT_DEF (valnum))
	return valnum;
      if (el_avail.length () > SSA_NAME_VERSION (valnum))
	return el_avail[SSA_NAME_VERSION (valnum)];
    }
  else if (is_gimple_min_invariant (valnum))
    return valnum;
  return NULL_TREE;
}

/* At the current point of the eliminate domwalk make OP available.  */

static void
eliminate_push_avail (tree op)
{
  tree valnum = VN_INFO (op)->valnum;
  if (TREE_CODE (valnum) == SSA_NAME)
    {
      if (el_avail.length () <= SSA_NAME_VERSION (valnum))
	el_avail.safe_grow_cleared (SSA_NAME_VERSION (valnum) + 1);
      tree pushop = op;
      if (el_avail[SSA_NAME_VERSION (valnum)])
	pushop = el_avail[SSA_NAME_VERSION (valnum)];
      el_avail_stack.safe_push (pushop);
      el_avail[SSA_NAME_VERSION (valnum)] = op;
    }
}

/* Insert the expression recorded by SCCVN for VAL at *GSI.  Returns
   the leader for the expression if insertion was successful.  */

static tree
eliminate_insert (gimple_stmt_iterator *gsi, tree val)
{
  /* We can insert a sequence with a single assignment only.  */
  gimple_seq stmts = VN_INFO (val)->expr;
  if (!gimple_seq_singleton_p (stmts))
    return NULL_TREE;
  gassign *stmt = dyn_cast <gassign *> (gimple_seq_first_stmt (stmts));
  if (!stmt
      || (!CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt))
	  && gimple_assign_rhs_code (stmt) != VIEW_CONVERT_EXPR
	  && gimple_assign_rhs_code (stmt) != BIT_FIELD_REF
	  && (gimple_assign_rhs_code (stmt) != BIT_AND_EXPR
	      || TREE_CODE (gimple_assign_rhs2 (stmt)) != INTEGER_CST)))
    return NULL_TREE;

  tree op = gimple_assign_rhs1 (stmt);
  if (gimple_assign_rhs_code (stmt) == VIEW_CONVERT_EXPR
      || gimple_assign_rhs_code (stmt) == BIT_FIELD_REF)
    op = TREE_OPERAND (op, 0);
  tree leader = TREE_CODE (op) == SSA_NAME ? eliminate_avail (op) : op;
  if (!leader)
    return NULL_TREE;

  tree res;
  stmts = NULL;
  if (gimple_assign_rhs_code (stmt) == BIT_FIELD_REF)
    res = gimple_build (&stmts, BIT_FIELD_REF,
			TREE_TYPE (val), leader,
			TREE_OPERAND (gimple_assign_rhs1 (stmt), 1),
			TREE_OPERAND (gimple_assign_rhs1 (stmt), 2));
  else if (gimple_assign_rhs_code (stmt) == BIT_AND_EXPR)
    res = gimple_build (&stmts, BIT_AND_EXPR,
			TREE_TYPE (val), leader, gimple_assign_rhs2 (stmt));
  else
    res = gimple_build (&stmts, gimple_assign_rhs_code (stmt),
			TREE_TYPE (val), leader);
  if (TREE_CODE (res) != SSA_NAME
      || SSA_NAME_IS_DEFAULT_DEF (res)
      || gimple_bb (SSA_NAME_DEF_STMT (res)))
    {
      gimple_seq_discard (stmts);

      /* During propagation we have to treat SSA info conservatively
         and thus we can end up simplifying the inserted expression
	 at elimination time to sth not defined in stmts.  */
      /* But then this is a redundancy we failed to detect.  Which means
         res now has two values.  That doesn't play well with how
	 we track availability here, so give up.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (TREE_CODE (res) == SSA_NAME)
	    res = eliminate_avail (res);
	  if (res)
	    {
	      fprintf (dump_file, "Failed to insert expression for value ");
	      print_generic_expr (dump_file, val);
	      fprintf (dump_file, " which is really fully redundant to ");
	      print_generic_expr (dump_file, res);
	      fprintf (dump_file, "\n");
	    }
	}

      return NULL_TREE;
    }
  else
    {
      gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
      VN_INFO_GET (res)->valnum = val;

      if (TREE_CODE (leader) == SSA_NAME)
	gimple_set_plf (SSA_NAME_DEF_STMT (leader), NECESSARY, true);
    }

  pre_stats.insertions++;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Inserted ");
      print_gimple_stmt (dump_file, SSA_NAME_DEF_STMT (res), 0);
    }

  return res;
}

class eliminate_dom_walker : public dom_walker
{
public:
  eliminate_dom_walker (cdi_direction direction, bool do_pre_)
      : dom_walker (direction), do_pre (do_pre_) {}

  virtual edge before_dom_children (basic_block);
  virtual void after_dom_children (basic_block);

  bool do_pre;
};

/* Perform elimination for the basic-block B during the domwalk.  */

edge
eliminate_dom_walker::before_dom_children (basic_block b)
{
  /* Mark new bb.  */
  el_avail_stack.safe_push (NULL_TREE);

  /* Skip unreachable blocks marked unreachable during the SCCVN domwalk.  */
  edge_iterator ei;
  edge e;
  FOR_EACH_EDGE (e, ei, b->preds)
    if (e->flags & EDGE_EXECUTABLE)
      break;
  if (! e)
    return NULL;

  for (gphi_iterator gsi = gsi_start_phis (b); !gsi_end_p (gsi);)
    {
      gphi *phi = gsi.phi ();
      tree res = PHI_RESULT (phi);

      if (virtual_operand_p (res))
	{
	  gsi_next (&gsi);
	  continue;
	}

      tree sprime = eliminate_avail (res);
      if (sprime
	  && sprime != res)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Replaced redundant PHI node defining ");
	      print_generic_expr (dump_file, res);
	      fprintf (dump_file, " with ");
	      print_generic_expr (dump_file, sprime);
	      fprintf (dump_file, "\n");
	    }

	  /* If we inserted this PHI node ourself, it's not an elimination.  */
	  if (inserted_exprs
	      && bitmap_bit_p (inserted_exprs, SSA_NAME_VERSION (res)))
	    pre_stats.phis--;
	  else
	    pre_stats.eliminations++;

	  /* If we will propagate into all uses don't bother to do
	     anything.  */
	  if (may_propagate_copy (res, sprime))
	    {
	      /* Mark the PHI for removal.  */
	      el_to_remove.safe_push (phi);
	      gsi_next (&gsi);
	      continue;
	    }

	  remove_phi_node (&gsi, false);

	  if (inserted_exprs
	      && !bitmap_bit_p (inserted_exprs, SSA_NAME_VERSION (res))
	      && TREE_CODE (sprime) == SSA_NAME)
	    gimple_set_plf (SSA_NAME_DEF_STMT (sprime), NECESSARY, true);

	  if (!useless_type_conversion_p (TREE_TYPE (res), TREE_TYPE (sprime)))
	    sprime = fold_convert (TREE_TYPE (res), sprime);
	  gimple *stmt = gimple_build_assign (res, sprime);
	  /* ???  It cannot yet be necessary (DOM walk).  */
	  gimple_set_plf (stmt, NECESSARY, gimple_plf (phi, NECESSARY));

	  gimple_stmt_iterator gsi2 = gsi_after_labels (b);
	  gsi_insert_before (&gsi2, stmt, GSI_NEW_STMT);
	  continue;
	}

      eliminate_push_avail (res);
      gsi_next (&gsi);
    }

  for (gimple_stmt_iterator gsi = gsi_start_bb (b);
       !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      tree sprime = NULL_TREE;
      gimple *stmt = gsi_stmt (gsi);
      tree lhs = gimple_get_lhs (stmt);
      if (lhs && TREE_CODE (lhs) == SSA_NAME
	  && !gimple_has_volatile_ops (stmt)
	  /* See PR43491.  Do not replace a global register variable when
	     it is a the RHS of an assignment.  Do replace local register
	     variables since gcc does not guarantee a local variable will
	     be allocated in register.
	     ???  The fix isn't effective here.  This should instead
	     be ensured by not value-numbering them the same but treating
	     them like volatiles?  */
	  && !(gimple_assign_single_p (stmt)
	       && (TREE_CODE (gimple_assign_rhs1 (stmt)) == VAR_DECL
		   && DECL_HARD_REGISTER (gimple_assign_rhs1 (stmt))
		   && is_global_var (gimple_assign_rhs1 (stmt)))))
	{
	  sprime = eliminate_avail (lhs);
	  if (!sprime)
	    {
	      /* If there is no existing usable leader but SCCVN thinks
		 it has an expression it wants to use as replacement,
		 insert that.  */
	      tree val = VN_INFO (lhs)->valnum;
	      if (val != VN_TOP
		  && TREE_CODE (val) == SSA_NAME
		  && VN_INFO (val)->needs_insertion
		  && VN_INFO (val)->expr != NULL
		  && (sprime = eliminate_insert (&gsi, val)) != NULL_TREE)
		eliminate_push_avail (sprime);
	    }

	  /* If this now constitutes a copy duplicate points-to
	     and range info appropriately.  This is especially
	     important for inserted code.  See tree-ssa-copy.c
	     for similar code.  */
	  if (sprime
	      && TREE_CODE (sprime) == SSA_NAME)
	    {
	      basic_block sprime_b = gimple_bb (SSA_NAME_DEF_STMT (sprime));
	      if (POINTER_TYPE_P (TREE_TYPE (lhs))
		  && VN_INFO_PTR_INFO (lhs)
		  && ! VN_INFO_PTR_INFO (sprime))
		{
		  duplicate_ssa_name_ptr_info (sprime,
					       VN_INFO_PTR_INFO (lhs));
		  if (b != sprime_b)
		    mark_ptr_info_alignment_unknown
			(SSA_NAME_PTR_INFO (sprime));
		}
	      else if (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
		       && VN_INFO_RANGE_INFO (lhs)
		       && ! VN_INFO_RANGE_INFO (sprime)
		       && b == sprime_b)
		duplicate_ssa_name_range_info (sprime,
					       VN_INFO_RANGE_TYPE (lhs),
					       VN_INFO_RANGE_INFO (lhs));
	    }

	  /* Inhibit the use of an inserted PHI on a loop header when
	     the address of the memory reference is a simple induction
	     variable.  In other cases the vectorizer won't do anything
	     anyway (either it's loop invariant or a complicated
	     expression).  */
	  if (sprime
	      && TREE_CODE (sprime) == SSA_NAME
	      && do_pre
	      && (flag_tree_loop_vectorize || flag_tree_parallelize_loops > 1)
	      && loop_outer (b->loop_father)
	      && has_zero_uses (sprime)
	      && bitmap_bit_p (inserted_exprs, SSA_NAME_VERSION (sprime))
	      && gimple_assign_load_p (stmt))
	    {
	      gimple *def_stmt = SSA_NAME_DEF_STMT (sprime);
	      basic_block def_bb = gimple_bb (def_stmt);
	      if (gimple_code (def_stmt) == GIMPLE_PHI
		  && def_bb->loop_father->header == def_bb)
		{
		  loop_p loop = def_bb->loop_father;
		  ssa_op_iter iter;
		  tree op;
		  bool found = false;
		  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE)
		    {
		      affine_iv iv;
		      def_bb = gimple_bb (SSA_NAME_DEF_STMT (op));
		      if (def_bb
			  && flow_bb_inside_loop_p (loop, def_bb)
			  && simple_iv (loop, loop, op, &iv, true))
			{
			  found = true;
			  break;
			}
		    }
		  if (found)
		    {
		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  fprintf (dump_file, "Not replacing ");
			  print_gimple_expr (dump_file, stmt, 0);
			  fprintf (dump_file, " with ");
			  print_generic_expr (dump_file, sprime);
			  fprintf (dump_file, " which would add a loop"
				   " carried dependence to loop %d\n",
				   loop->num);
			}
		      /* Don't keep sprime available.  */
		      sprime = NULL_TREE;
		    }
		}
	    }

	  if (sprime)
	    {
	      /* If we can propagate the value computed for LHS into
		 all uses don't bother doing anything with this stmt.  */
	      if (may_propagate_copy (lhs, sprime))
		{
		  /* Mark it for removal.  */
		  el_to_remove.safe_push (stmt);

		  /* ???  Don't count copy/constant propagations.  */
		  if (gimple_assign_single_p (stmt)
		      && (TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
			  || gimple_assign_rhs1 (stmt) == sprime))
		    continue;

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Replaced ");
		      print_gimple_expr (dump_file, stmt, 0);
		      fprintf (dump_file, " with ");
		      print_generic_expr (dump_file, sprime);
		      fprintf (dump_file, " in all uses of ");
		      print_gimple_stmt (dump_file, stmt, 0);
		    }

		  pre_stats.eliminations++;
		  continue;
		}

	      /* If this is an assignment from our leader (which
	         happens in the case the value-number is a constant)
		 then there is nothing to do.  */
	      if (gimple_assign_single_p (stmt)
		  && sprime == gimple_assign_rhs1 (stmt))
		continue;

	      /* Else replace its RHS.  */
	      bool can_make_abnormal_goto
		  = is_gimple_call (stmt)
		  && stmt_can_make_abnormal_goto (stmt);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Replaced ");
		  print_gimple_expr (dump_file, stmt, 0);
		  fprintf (dump_file, " with ");
		  print_generic_expr (dump_file, sprime);
		  fprintf (dump_file, " in ");
		  print_gimple_stmt (dump_file, stmt, 0);
		}

	      if (TREE_CODE (sprime) == SSA_NAME)
		gimple_set_plf (SSA_NAME_DEF_STMT (sprime),
				NECESSARY, true);

	      pre_stats.eliminations++;
	      gimple *orig_stmt = stmt;
	      if (!useless_type_conversion_p (TREE_TYPE (lhs),
					      TREE_TYPE (sprime)))
		sprime = fold_convert (TREE_TYPE (lhs), sprime);
	      tree vdef = gimple_vdef (stmt);
	      tree vuse = gimple_vuse (stmt);
	      propagate_tree_value_into_stmt (&gsi, sprime);
	      stmt = gsi_stmt (gsi);
	      update_stmt (stmt);
	      if (vdef != gimple_vdef (stmt))
		VN_INFO (vdef)->valnum = vuse;

	      /* If we removed EH side-effects from the statement, clean
		 its EH information.  */
	      if (maybe_clean_or_replace_eh_stmt (orig_stmt, stmt))
		{
		  bitmap_set_bit (need_eh_cleanup,
				  gimple_bb (stmt)->index);
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "  Removed EH side-effects.\n");
		}

	      /* Likewise for AB side-effects.  */
	      if (can_make_abnormal_goto
		  && !stmt_can_make_abnormal_goto (stmt))
		{
		  bitmap_set_bit (need_ab_cleanup,
				  gimple_bb (stmt)->index);
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "  Removed AB side-effects.\n");
		}

	      continue;
	    }
	}

      /* If the statement is a scalar store, see if the expression
         has the same value number as its rhs.  If so, the store is
         dead.  */
      if (gimple_assign_single_p (stmt)
	  && !gimple_has_volatile_ops (stmt)
	  && !is_gimple_reg (gimple_assign_lhs (stmt))
	  && (TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
	      || is_gimple_min_invariant (gimple_assign_rhs1 (stmt))))
	{
	  tree val;
	  tree rhs = gimple_assign_rhs1 (stmt);
	  vn_reference_t vnresult;
	  val = vn_reference_lookup (lhs, gimple_vuse (stmt), VN_WALKREWRITE,
				     &vnresult, false);
	  if (TREE_CODE (rhs) == SSA_NAME)
	    rhs = VN_INFO (rhs)->valnum;
	  if (val
	      && operand_equal_p (val, rhs, 0))
	    {
	      /* We can only remove the later store if the former aliases
		 at least all accesses the later one does or if the store
		 was to readonly memory storing the same value.  */
	      alias_set_type set = get_alias_set (lhs);
	      if (! vnresult
		  || vnresult->set == set
		  || alias_set_subset_of (set, vnresult->set))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Deleted redundant store ");
		      print_gimple_stmt (dump_file, stmt, 0);
		    }

		  /* Queue stmt for removal.  */
		  el_to_remove.safe_push (stmt);
		  continue;
		}
	    }
	}

      /* If this is a control statement value numbering left edges
	 unexecuted on force the condition in a way consistent with
	 that.  */
      if (gcond *cond = dyn_cast <gcond *> (stmt))
	{
	  if ((EDGE_SUCC (b, 0)->flags & EDGE_EXECUTABLE)
	      ^ (EDGE_SUCC (b, 1)->flags & EDGE_EXECUTABLE))
	    {
              if (dump_file && (dump_flags & TDF_DETAILS))
                {
                  fprintf (dump_file, "Removing unexecutable edge from ");
		  print_gimple_stmt (dump_file, stmt, 0);
                }
	      if (((EDGE_SUCC (b, 0)->flags & EDGE_TRUE_VALUE) != 0)
		  == ((EDGE_SUCC (b, 0)->flags & EDGE_EXECUTABLE) != 0))
		gimple_cond_make_true (cond);
	      else
		gimple_cond_make_false (cond);
	      update_stmt (cond);
	      el_todo |= TODO_cleanup_cfg;
	      continue;
	    }
	}

      bool can_make_abnormal_goto = stmt_can_make_abnormal_goto (stmt);
      bool was_noreturn = (is_gimple_call (stmt)
			   && gimple_call_noreturn_p (stmt));
      tree vdef = gimple_vdef (stmt);
      tree vuse = gimple_vuse (stmt);

      /* If we didn't replace the whole stmt (or propagate the result
         into all uses), replace all uses on this stmt with their
	 leaders.  */
      bool modified = false;
      use_operand_p use_p;
      ssa_op_iter iter;
      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
	{
	  tree use = USE_FROM_PTR (use_p);
	  /* ???  The call code above leaves stmt operands un-updated.  */
	  if (TREE_CODE (use) != SSA_NAME)
	    continue;
	  tree sprime = eliminate_avail (use);
	  if (sprime && sprime != use
	      && may_propagate_copy (use, sprime)
	      /* We substitute into debug stmts to avoid excessive
	         debug temporaries created by removed stmts, but we need
		 to avoid doing so for inserted sprimes as we never want
		 to create debug temporaries for them.  */
	      && (!inserted_exprs
		  || TREE_CODE (sprime) != SSA_NAME
		  || !is_gimple_debug (stmt)
		  || !bitmap_bit_p (inserted_exprs, SSA_NAME_VERSION (sprime))))
	    {
	      propagate_value (use_p, sprime);
	      modified = true;
	      if (TREE_CODE (sprime) == SSA_NAME
		  && !is_gimple_debug (stmt))
		gimple_set_plf (SSA_NAME_DEF_STMT (sprime),
				NECESSARY, true);
	    }
	}

      /* Fold the stmt if modified, this canonicalizes MEM_REFs we propagated
         into which is a requirement for the IPA devirt machinery.  */
      gimple *old_stmt = stmt;
      if (modified)
	{
	  /* If a formerly non-invariant ADDR_EXPR is turned into an
	     invariant one it was on a separate stmt.  */
	  if (gimple_assign_single_p (stmt)
	      && TREE_CODE (gimple_assign_rhs1 (stmt)) == ADDR_EXPR)
	    recompute_tree_invariant_for_addr_expr (gimple_assign_rhs1 (stmt));
	  gimple_stmt_iterator prev = gsi;
	  gsi_prev (&prev);
	  if (fold_stmt (&gsi))
	    {
	      /* fold_stmt may have created new stmts inbetween
		 the previous stmt and the folded stmt.  Mark
		 all defs created there as varying to not confuse
		 the SCCVN machinery as we're using that even during
		 elimination.  */
	      if (gsi_end_p (prev))
		prev = gsi_start_bb (b);
	      else
		gsi_next (&prev);
	      if (gsi_stmt (prev) != gsi_stmt (gsi))
		do
		  {
		    tree def;
		    ssa_op_iter dit;
		    FOR_EACH_SSA_TREE_OPERAND (def, gsi_stmt (prev),
					       dit, SSA_OP_ALL_DEFS)
		      /* As existing DEFs may move between stmts
			 we have to guard VN_INFO_GET.  */
		      if (! has_VN_INFO (def))
			VN_INFO_GET (def)->valnum = def;
		    if (gsi_stmt (prev) == gsi_stmt (gsi))
		      break;
		    gsi_next (&prev);
		  }
		while (1);
	    }
	  stmt = gsi_stmt (gsi);
	  /* In case we folded the stmt away schedule the NOP for removal.  */
	  if (gimple_nop_p (stmt))
	    el_to_remove.safe_push (stmt);
	}

      /* Visit indirect calls and turn them into direct calls if
	 possible using the devirtualization machinery.  Do this before
	 checking for required EH/abnormal/noreturn cleanup as devird
	 may expose more of those.  */
      if (gcall *call_stmt = dyn_cast <gcall *> (stmt))
	{
	  tree fn = gimple_call_fn (call_stmt);
	  if (fn
	      && flag_devirtualize
	      && virtual_method_call_p (fn))
	    {
	      tree otr_type = obj_type_ref_class (fn);
	      unsigned HOST_WIDE_INT otr_tok
		= tree_to_uhwi (OBJ_TYPE_REF_TOKEN (fn));
	      tree instance;
	      ipa_polymorphic_call_context context (current_function_decl,
						    fn, stmt, &instance);
	      context.get_dynamic_type (instance, OBJ_TYPE_REF_OBJECT (fn),
					otr_type, stmt);
	      bool final;
	      vec <cgraph_node *> targets
		= possible_polymorphic_call_targets (obj_type_ref_class (fn),
						     otr_tok, context, &final);
	      if (dump_file)
		dump_possible_polymorphic_call_targets (dump_file, 
							obj_type_ref_class (fn),
							otr_tok, context);
	      if (final && targets.length () <= 1 && dbg_cnt (devirt))
		{
		  tree fn;
		  if (targets.length () == 1)
		    fn = targets[0]->decl;
		  else
		    fn = builtin_decl_implicit (BUILT_IN_UNREACHABLE);
		  if (dump_enabled_p ())
		    {
		      location_t loc = gimple_location (stmt);
		      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
				       "converting indirect call to "
				       "function %s\n",
				       lang_hooks.decl_printable_name (fn, 2));
		    }
		  gimple_call_set_fndecl (call_stmt, fn);
		  /* If changing the call to __builtin_unreachable
		     or similar noreturn function, adjust gimple_call_fntype
		     too.  */
		  if (gimple_call_noreturn_p (call_stmt)
		      && VOID_TYPE_P (TREE_TYPE (TREE_TYPE (fn)))
		      && TYPE_ARG_TYPES (TREE_TYPE (fn))
		      && (TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fn)))
			  == void_type_node))
		    gimple_call_set_fntype (call_stmt, TREE_TYPE (fn));
		  maybe_remove_unused_call_args (cfun, call_stmt);
		  modified = true;
		}
	    }
	}

      if (modified)
	{
	  /* When changing a call into a noreturn call, cfg cleanup
	     is needed to fix up the noreturn call.  */
	  if (!was_noreturn
	      && is_gimple_call (stmt) && gimple_call_noreturn_p (stmt))
	    el_to_fixup.safe_push  (stmt);
	  /* When changing a condition or switch into one we know what
	     edge will be executed, schedule a cfg cleanup.  */
	  if ((gimple_code (stmt) == GIMPLE_COND
	       && (gimple_cond_true_p (as_a <gcond *> (stmt))
		   || gimple_cond_false_p (as_a <gcond *> (stmt))))
	      || (gimple_code (stmt) == GIMPLE_SWITCH
		  && TREE_CODE (gimple_switch_index
				  (as_a <gswitch *> (stmt))) == INTEGER_CST))
	    el_todo |= TODO_cleanup_cfg;
	  /* If we removed EH side-effects from the statement, clean
	     its EH information.  */
	  if (maybe_clean_or_replace_eh_stmt (old_stmt, stmt))
	    {
	      bitmap_set_bit (need_eh_cleanup,
			      gimple_bb (stmt)->index);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  Removed EH side-effects.\n");
	    }
	  /* Likewise for AB side-effects.  */
	  if (can_make_abnormal_goto
	      && !stmt_can_make_abnormal_goto (stmt))
	    {
	      bitmap_set_bit (need_ab_cleanup,
			      gimple_bb (stmt)->index);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  Removed AB side-effects.\n");
	    }
	  update_stmt (stmt);
	  if (vdef != gimple_vdef (stmt))
	    VN_INFO (vdef)->valnum = vuse;
	}

      /* Make new values available - for fully redundant LHS we
         continue with the next stmt above and skip this.  */
      def_operand_p defp;
      FOR_EACH_SSA_DEF_OPERAND (defp, stmt, iter, SSA_OP_DEF)
	eliminate_push_avail (DEF_FROM_PTR (defp));
    }

  /* Replace destination PHI arguments.  */
  FOR_EACH_EDGE (e, ei, b->succs)
    if (e->flags & EDGE_EXECUTABLE)
      for (gphi_iterator gsi = gsi_start_phis (e->dest);
	   !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  use_operand_p use_p = PHI_ARG_DEF_PTR_FROM_EDGE (phi, e);
	  tree arg = USE_FROM_PTR (use_p);
	  if (TREE_CODE (arg) != SSA_NAME
	      || virtual_operand_p (arg))
	    continue;
	  tree sprime = eliminate_avail (arg);
	  if (sprime && may_propagate_copy (arg, sprime))
	    {
	      propagate_value (use_p, sprime);
	      if (TREE_CODE (sprime) == SSA_NAME)
		gimple_set_plf (SSA_NAME_DEF_STMT (sprime), NECESSARY, true);
	    }
	}
  return NULL;
}

/* Make no longer available leaders no longer available.  */

void
eliminate_dom_walker::after_dom_children (basic_block)
{
  tree entry;
  while ((entry = el_avail_stack.pop ()) != NULL_TREE)
    {
      tree valnum = VN_INFO (entry)->valnum;
      tree old = el_avail[SSA_NAME_VERSION (valnum)];
      if (old == entry)
	el_avail[SSA_NAME_VERSION (valnum)] = NULL_TREE;
      else
	el_avail[SSA_NAME_VERSION (valnum)] = entry;
    }
}

/* Eliminate fully redundant computations.  */

static unsigned int
eliminate (bool do_pre)
{
  need_eh_cleanup = BITMAP_ALLOC (NULL);
  need_ab_cleanup = BITMAP_ALLOC (NULL);

  el_to_remove.create (0);
  el_to_fixup.create (0);
  el_todo = 0;
  el_avail.create (num_ssa_names);
  el_avail_stack.create (0);

  eliminate_dom_walker (CDI_DOMINATORS,
			do_pre).walk (cfun->cfg->x_entry_block_ptr);

  el_avail.release ();
  el_avail_stack.release ();

  return el_todo;
}

/* Perform CFG cleanups made necessary by elimination.  */

static unsigned 
fini_eliminate (void)
{
  gimple_stmt_iterator gsi;
  gimple *stmt;
  unsigned todo = 0;

  /* We cannot remove stmts during BB walk, especially not release SSA
     names there as this confuses the VN machinery.  The stmts ending
     up in el_to_remove are either stores or simple copies.
     Remove stmts in reverse order to make debug stmt creation possible.  */
  while (!el_to_remove.is_empty ())
    {
      stmt = el_to_remove.pop ();

      tree lhs;
      if (gimple_code (stmt) == GIMPLE_PHI)
	lhs = gimple_phi_result (stmt);
      else
	lhs = gimple_get_lhs (stmt);

      if (inserted_exprs
	  && lhs
	  && TREE_CODE (lhs) == SSA_NAME
	  && bitmap_bit_p (inserted_exprs, SSA_NAME_VERSION (lhs)))
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Removing dead stmt ");
	  print_gimple_stmt (dump_file, stmt, 0, 0);
	}

      gsi = gsi_for_stmt (stmt);
      if (gimple_code (stmt) == GIMPLE_PHI)
	remove_phi_node (&gsi, true);
      else
	{
	  basic_block bb = gimple_bb (stmt);
	  unlink_stmt_vdef (stmt);
	  if (gsi_remove (&gsi, true))
	    bitmap_set_bit (need_eh_cleanup, bb->index);
	  if (is_gimple_call (stmt) && stmt_can_make_abnormal_goto (stmt))
	    bitmap_set_bit (need_ab_cleanup, bb->index);
	  release_defs (stmt);
	}

      /* Removing a stmt may expose a forwarder block.  */
      todo |= TODO_cleanup_cfg;
    }
  el_to_remove.release ();

  /* Fixup stmts that became noreturn calls.  This may require splitting
     blocks and thus isn't possible during the dominator walk.  Do this
     in reverse order so we don't inadvertedly remove a stmt we want to
     fixup by visiting a dominating now noreturn call first.  */
  while (!el_to_fixup.is_empty ())
    {
      stmt = el_to_fixup.pop ();

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Fixing up noreturn call ");
	  print_gimple_stmt (dump_file, stmt, 0);
	}

      if (fixup_noreturn_call (stmt))
	todo |= TODO_cleanup_cfg;
    }
  el_to_fixup.release ();

  bool do_eh_cleanup = !bitmap_empty_p (need_eh_cleanup);
  bool do_ab_cleanup = !bitmap_empty_p (need_ab_cleanup);

  if (do_eh_cleanup)
    gimple_purge_all_dead_eh_edges (need_eh_cleanup);

  if (do_ab_cleanup)
    gimple_purge_all_dead_abnormal_call_edges (need_ab_cleanup);

  BITMAP_FREE (need_eh_cleanup);
  BITMAP_FREE (need_ab_cleanup);

  if (do_eh_cleanup || do_ab_cleanup)
    todo |= TODO_cleanup_cfg;
  return todo;
}

/* Borrow a bit of tree-ssa-dce.c for the moment.
   XXX: In 4.1, we should be able to just run a DCE pass after PRE, though
   this may be a bit faster, and we may want critical edges kept split.  */

/* If OP's defining statement has not already been determined to be necessary,
   mark that statement necessary. Return the stmt, if it is newly
   necessary.  */

static inline gimple *
mark_operand_necessary (tree op)
{
  gimple *stmt;

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
  unsigned i;
  bitmap_iterator bi;
  gimple *t;

  auto_bitmap worklist;
  EXECUTE_IF_SET_IN_BITMAP (inserted_exprs, 0, i, bi)
    {
      t = SSA_NAME_DEF_STMT (ssa_name (i));
      if (gimple_plf (t, NECESSARY))
	bitmap_set_bit (worklist, i);
    }
  while (!bitmap_empty_p (worklist))
    {
      i = bitmap_first_set_bit (worklist);
      bitmap_clear_bit (worklist, i);
      t = SSA_NAME_DEF_STMT (ssa_name (i));

      /* PHI nodes are somewhat special in that each PHI alternative has
	 data and control dependencies.  All the statements feeding the
	 PHI node's arguments are always necessary. */
      if (gimple_code (t) == GIMPLE_PHI)
	{
	  unsigned k;

	  for (k = 0; k < gimple_phi_num_args (t); k++)
	    {
	      tree arg = PHI_ARG_DEF (t, k);
	      if (TREE_CODE (arg) == SSA_NAME)
		{
		  gimple *n = mark_operand_necessary (arg);
		  if (n)
		    bitmap_set_bit (worklist, SSA_NAME_VERSION (arg));
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
	      gimple *n = mark_operand_necessary (use);
	      if (n)
		bitmap_set_bit (worklist, SSA_NAME_VERSION (use));
	    }
	}
    }

  unsigned int to_clear = -1U;
  EXECUTE_IF_SET_IN_BITMAP (inserted_exprs, 0, i, bi)
    {
      if (to_clear != -1U)
	{
	  bitmap_clear_bit (inserted_exprs, to_clear);
	  to_clear = -1U;
	}
      t = SSA_NAME_DEF_STMT (ssa_name (i));
      if (!gimple_plf (t, NECESSARY))
	{
	  gimple_stmt_iterator gsi;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Removing unnecessary insertion:");
	      print_gimple_stmt (dump_file, t, 0);
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
      else
	/* eliminate_fini will skip stmts marked for removal if we
	   already removed it and uses inserted_exprs for this, so
	   clear those we didn't end up removing.  */
	to_clear = i;
    }
  if (to_clear != -1U)
    bitmap_clear_bit (inserted_exprs, to_clear);
}


/* Initialize data structures used by PRE.  */

static void
init_pre (void)
{
  basic_block bb;

  next_expression_id = 1;
  expressions.create (0);
  expressions.safe_push (NULL);
  value_expressions.create (get_max_value_id () + 1);
  value_expressions.safe_grow_cleared (get_max_value_id () + 1);
  name_to_id.create (0);

  inserted_exprs = BITMAP_ALLOC (NULL);

  connect_infinite_loops_to_exit ();
  memset (&pre_stats, 0, sizeof (pre_stats));

  alloc_aux_for_blocks (sizeof (struct bb_bitmap_sets));

  calculate_dominance_info (CDI_DOMINATORS);

  bitmap_obstack_initialize (&grand_bitmap_obstack);
  phi_translate_table = new hash_table<expr_pred_trans_d> (5110);
  expression_to_id = new hash_table<pre_expr_d> (num_ssa_names * 3);
  FOR_ALL_BB_FN (bb, cfun)
    {
      EXP_GEN (bb) = bitmap_set_new ();
      PHI_GEN (bb) = bitmap_set_new ();
      TMP_GEN (bb) = bitmap_set_new ();
      AVAIL_OUT (bb) = bitmap_set_new ();
    }
}


/* Deallocate data structures used by PRE.  */

static void
fini_pre ()
{
  value_expressions.release ();
  BITMAP_FREE (inserted_exprs);
  bitmap_obstack_release (&grand_bitmap_obstack);
  bitmap_set_pool.release ();
  pre_expr_pool.release ();
  delete phi_translate_table;
  phi_translate_table = NULL;
  delete expression_to_id;
  expression_to_id = NULL;
  name_to_id.release ();

  free_aux_for_blocks ();
}

namespace {

const pass_data pass_data_pre =
{
  GIMPLE_PASS, /* type */
  "pre", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_PRE, /* tv_id */
  /* PROP_no_crit_edges is ensured by placing pass_split_crit_edges before
     pass_pre.  */
  ( PROP_no_crit_edges | PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  PROP_no_crit_edges, /* properties_destroyed */
  TODO_rebuild_alias, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_pre : public gimple_opt_pass
{
public:
  pass_pre (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_pre, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    { return flag_tree_pre != 0 || flag_code_hoisting != 0; }
  virtual unsigned int execute (function *);

}; // class pass_pre

unsigned int
pass_pre::execute (function *fun)
{
  unsigned int todo = 0;

  do_partial_partial =
    flag_tree_partial_pre && optimize_function_for_speed_p (fun);

  /* This has to happen before SCCVN runs because
     loop_optimizer_init may create new phis, etc.  */
  loop_optimizer_init (LOOPS_NORMAL);

  run_scc_vn (VN_WALK);

  init_pre ();
  scev_initialize ();

  /* Collect and value number expressions computed in each basic block.  */
  compute_avail ();

  /* Insert can get quite slow on an incredibly large number of basic
     blocks due to some quadratic behavior.  Until this behavior is
     fixed, don't run it when he have an incredibly large number of
     bb's.  If we aren't going to run insert, there is no point in
     computing ANTIC, either, even though it's plenty fast.  */
  if (n_basic_blocks_for_fn (fun) < 4000)
    {
      compute_antic ();
      insert ();
    }

  /* Make sure to remove fake edges before committing our inserts.
     This makes sure we don't end up with extra critical edges that
     we would need to split.  */
  remove_fake_exit_edges ();
  gsi_commit_edge_inserts ();

  /* Eliminate folds statements which might (should not...) end up
     not keeping virtual operands up-to-date.  */
  gcc_assert (!need_ssa_update_p (fun));

  /* Remove all the redundant expressions.  */
  todo |= eliminate (true);

  statistics_counter_event (fun, "Insertions", pre_stats.insertions);
  statistics_counter_event (fun, "PA inserted", pre_stats.pa_insert);
  statistics_counter_event (fun, "HOIST inserted", pre_stats.hoist_insert);
  statistics_counter_event (fun, "New PHIs", pre_stats.phis);
  statistics_counter_event (fun, "Eliminated", pre_stats.eliminations);

  clear_expression_ids ();
  remove_dead_inserted_code ();

  scev_finalize ();
  todo |= fini_eliminate ();
  fini_pre ();
  loop_optimizer_finalize ();

  /* Restore SSA info before tail-merging as that resets it as well.  */
  scc_vn_restore_ssa_info ();

  /* TODO: tail_merge_optimize may merge all predecessors of a block, in which
     case we can merge the block with the remaining predecessor of the block.
     It should either:
     - call merge_blocks after each tail merge iteration
     - call merge_blocks after all tail merge iterations
     - mark TODO_cleanup_cfg when necessary
     - share the cfg cleanup with fini_pre.  */
  todo |= tail_merge_optimize (todo);

  free_scc_vn ();

  /* Tail merging invalidates the virtual SSA web, together with
     cfg-cleanup opportunities exposed by PRE this will wreck the
     SSA updating machinery.  So make sure to run update-ssa
     manually, before eventually scheduling cfg-cleanup as part of
     the todo.  */
  update_ssa (TODO_update_ssa_only_virtuals);

  return todo;
}

} // anon namespace

gimple_opt_pass *
make_pass_pre (gcc::context *ctxt)
{
  return new pass_pre (ctxt);
}

namespace {

const pass_data pass_data_fre =
{
  GIMPLE_PASS, /* type */
  "fre", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_FRE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_fre : public gimple_opt_pass
{
public:
  pass_fre (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_fre, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_fre (m_ctxt); }
  virtual bool gate (function *) { return flag_tree_fre != 0; }
  virtual unsigned int execute (function *);

}; // class pass_fre

unsigned int
pass_fre::execute (function *fun)
{
  unsigned int todo = 0;

  run_scc_vn (VN_WALKREWRITE);

  memset (&pre_stats, 0, sizeof (pre_stats));

  /* Remove all the redundant expressions.  */
  todo |= eliminate (false);

  todo |= fini_eliminate ();

  scc_vn_restore_ssa_info ();
  free_scc_vn ();

  statistics_counter_event (fun, "Insertions", pre_stats.insertions);
  statistics_counter_event (fun, "Eliminated", pre_stats.eliminations);

  return todo;
}

} // anon namespace

gimple_opt_pass *
make_pass_fre (gcc::context *ctxt)
{
  return new pass_fre (ctxt);
}
