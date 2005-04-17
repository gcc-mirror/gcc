/* SSA-PRE for trees.
   Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dan@dberlin.org> and Steven Bosscher
   <stevenb@suse.de> 

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "errors.h"
#include "ggc.h"
#include "tree.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-inline.h"
#include "tree-flow.h"
#include "tree-gimple.h"
#include "tree-dump.h"
#include "timevar.h"
#include "fibheap.h"
#include "hashtab.h"
#include "tree-iterator.h"
#include "real.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "flags.h"
#include "bitmap.h"
#include "langhooks.h"
#include "cfgloop.h"

/* TODO:
   
   1. Avail sets can be shared by making an avail_find_leader that
      walks up the dominator tree and looks in those avail sets.
      This might affect code optimality, it's unclear right now.
   2. Load motion can be performed by value numbering the loads the
      same as we do other expressions.  This requires iterative
      hashing the vuses into the values.  Right now we simply assign
      a new value every time we see a statement with a vuse.
   3. Strength reduction can be performed by anticipating expressions
      we can repair later on.
   4. We can do back-substitution or smarter value numbering to catch
      commutative expressions split up over multiple statements.
*/   

/* For ease of terminology, "expression node" in the below refers to
   every expression node but MODIFY_EXPR, because MODIFY_EXPR's represent
   the actual statement containing the expressions we care about, and
   we cache the value number by putting it in the expression.  */

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
   problem.An expression is anticipatable in a given block if it could
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
   insert/insert_aux performs this insertion.

   Fourth, we eliminate fully redundant expressions.
   This is a simple statement walk that replaces redundant
   calculations  with the now available values.  */

/* Representations of value numbers:

   Value numbers are represented using the "value handle" approach.
   This means that each SSA_NAME (and for other reasons to be
   disclosed in a moment, expression nodes) has a value handle that
   can be retrieved through get_value_handle.  This value handle, *is*
   the value number of the SSA_NAME.  You can pointer compare the
   value handles for equivalence purposes.

   For debugging reasons, the value handle is internally more than
   just a number, it is a VAR_DECL named "value.x", where x is a
   unique number for each value number in use.  This allows
   expressions with SSA_NAMES replaced by value handles to still be
   pretty printed in a sane way.  They simply print as "value.3 *
   value.5", etc.  

   Expression nodes have value handles associated with them as a
   cache.  Otherwise, we'd have to look them up again in the hash
   table This makes significant difference (factor of two or more) on
   some test cases.  They can be thrown away after the pass is
   finished.  */

/* Representation of expressions on value numbers: 

   In some portions of this code, you will notice we allocate "fake"
   analogues to the expression we are value numbering, and replace the
   operands with the values of the expression.  Since we work on
   values, and not just names, we canonicalize expressions to value
   expressions for use in the ANTIC sets, the EXP_GEN set, etc.  

   This is theoretically unnecessary, it just saves a bunch of
   repeated get_value_handle and find_leader calls in the remainder of
   the code, trading off temporary memory usage for speed.  The tree
   nodes aren't actually creating more garbage, since they are
   allocated in a special pools which are thrown away at the end of
   this pass.  

   All of this also means that if you print the EXP_GEN or ANTIC sets,
   you will see "value.5 + value.7" in the set, instead of "a_55 +
   b_66" or something.  The only thing that actually cares about
   seeing the value leaders is phi translation, and it needs to be
   able to find the leader for a value in an arbitrary block, so this
   "value expression" form is perfect for it (otherwise you'd do
   get_value_handle->find_leader->translate->get_value_handle->find_leader).*/


/* Representation of sets:

   There are currently two types of sets used, hopefully to be unified soon.
   The AVAIL sets do not need to be sorted in any particular order,
   and thus, are simply represented as two bitmaps, one that keeps
   track of values present in the set, and one that keeps track of
   expressions present in the set.
   
   The other sets are represented as doubly linked lists kept in topological
   order, with an optional supporting bitmap of values present in the
   set.  The sets represent values, and the elements can be values or
   expressions.  The elements can appear in different sets, but each
   element can only appear once in each set.

   Since each node in the set represents a value, we also want to be
   able to map expression, set pairs to something that tells us
   whether the value is present is a set.  We use a per-set bitmap for
   that.  The value handles also point to a linked list of the
   expressions they represent via a tree annotation.  This is mainly
   useful only for debugging, since we don't do identity lookups.  */


/* A value set element.  Basically a single linked list of
   expressions/values.  */
typedef struct value_set_node
{
  /* An expression.  */
  tree expr;

  /* A pointer to the next element of the value set.  */
  struct value_set_node *next;
} *value_set_node_t;


/* A value set.  This is a singly linked list of value_set_node
   elements with a possible bitmap that tells us what values exist in
   the set.  This set must be kept in topologically sorted order.  */
typedef struct value_set
{
  /* The head of the list.  Used for iterating over the list in
     order.  */
  value_set_node_t head;

  /* The tail of the list.  Used for tail insertions, which are
     necessary to keep the set in topologically sorted order because
     of how the set is built.  */
  value_set_node_t tail;
  
  /* The length of the list.  */
  size_t length;
  
  /* True if the set is indexed, which means it contains a backing
     bitmap for quick determination of whether certain values exist in the
     set.  */
  bool indexed;
  
  /* The bitmap of values that exist in the set.  May be NULL in an
     empty or non-indexed set.  */
  bitmap values;
  
} *value_set_t;


/* An unordered bitmap set.  One bitmap tracks values, the other,
   expressions.  */
typedef struct bitmap_set
{
  bitmap expressions;
  bitmap values;
} *bitmap_set_t;

/* Sets that we need to keep track of.  */
typedef struct bb_value_sets
{
  /* The EXP_GEN set, which represents expressions/values generated in
     a basic block.  */
  value_set_t exp_gen;

  /* The PHI_GEN set, which represents PHI results generated in a
     basic block.  */
  bitmap_set_t phi_gen;

  /* The TMP_GEN set, which represents results/temporaries generated
     in a basic block. IE the LHS of an expression.  */
  bitmap_set_t tmp_gen;

  /* The AVAIL_OUT set, which represents which values are available in
     a given basic block.  */
  bitmap_set_t avail_out;

  /* The ANTIC_IN set, which represents which values are anticiptable
     in a given basic block.  */
  value_set_t antic_in;

  /* The NEW_SETS set, which is used during insertion to augment the
     AVAIL_OUT set of blocks with the new insertions performed during
     the current iteration.  */
  bitmap_set_t new_sets;
} *bb_value_sets_t;

#define EXP_GEN(BB)	((bb_value_sets_t) ((BB)->aux))->exp_gen
#define PHI_GEN(BB)	((bb_value_sets_t) ((BB)->aux))->phi_gen
#define TMP_GEN(BB)	((bb_value_sets_t) ((BB)->aux))->tmp_gen
#define AVAIL_OUT(BB)	((bb_value_sets_t) ((BB)->aux))->avail_out
#define ANTIC_IN(BB)	((bb_value_sets_t) ((BB)->aux))->antic_in
#define NEW_SETS(BB)	((bb_value_sets_t) ((BB)->aux))->new_sets

/* This structure is used to keep track of statistics on what
   optimization PRE was able to perform.  */
static struct
{
  /* The number of RHS computations eliminated by PRE.  */
  int eliminations;

  /* The number of new expressions/temporaries generated by PRE.  */
  int insertions;

  /* The number of new PHI nodes added by PRE.  */
  int phis;
  
  /* The number of values found constant.  */
  int constified;
  
} pre_stats;


static tree bitmap_find_leader (bitmap_set_t, tree);
static tree find_leader (value_set_t, tree);
static void value_insert_into_set (value_set_t, tree);
static void bitmap_value_insert_into_set (bitmap_set_t, tree);
static void bitmap_value_replace_in_set (bitmap_set_t, tree);
static void insert_into_set (value_set_t, tree);
static void bitmap_set_copy (bitmap_set_t, bitmap_set_t);
static bool bitmap_set_contains_value (bitmap_set_t, tree);
static bitmap_set_t bitmap_set_new (void);
static value_set_t set_new  (bool);
static bool is_undefined_value (tree);
static tree create_expression_by_pieces (basic_block, tree, tree);


/* We can add and remove elements and entries to and from sets
   and hash tables, so we use alloc pools for them.  */

static alloc_pool value_set_pool;
static alloc_pool bitmap_set_pool;
static alloc_pool value_set_node_pool;
static alloc_pool binary_node_pool;
static alloc_pool unary_node_pool;
static alloc_pool reference_node_pool;
static bitmap_obstack grand_bitmap_obstack;

/* Set of blocks with statements that have had its EH information
   cleaned up.  */
static bitmap need_eh_cleanup;

/* The phi_translate_table caches phi translations for a given
   expression and predecessor.  */

static htab_t phi_translate_table;

/* A three tuple {e, pred, v} used to cache phi translations in the
   phi_translate_table.  */

typedef struct expr_pred_trans_d
{
  /* The expression.  */
  tree e;

  /* The predecessor block along which we translated the expression.  */
  basic_block pred;

  /* The value that resulted from the translation.  */
  tree v;

  /* The hashcode for the expression, pred pair. This is cached for
     speed reasons.  */
  hashval_t hashcode;
} *expr_pred_trans_t;

/* Return the hash value for a phi translation table entry.  */

static hashval_t
expr_pred_trans_hash (const void *p)
{
  const expr_pred_trans_t ve = (expr_pred_trans_t) p;
  return ve->hashcode;
}

/* Return true if two phi translation table entries are the same.
   P1 and P2 should point to the expr_pred_trans_t's to be compared.*/

static int
expr_pred_trans_eq (const void *p1, const void *p2)
{
  const expr_pred_trans_t ve1 = (expr_pred_trans_t) p1;
  const expr_pred_trans_t ve2 = (expr_pred_trans_t) p2;
  basic_block b1 = ve1->pred;
  basic_block b2 = ve2->pred;

  
  /* If they are not translations for the same basic block, they can't
     be equal.  */
  if (b1 != b2)
    return false;

  /* If they are for the same basic block, determine if the
     expressions are equal.  */  
  if (expressions_equal_p (ve1->e, ve2->e))
    return true;
  
  return false;
}

/* Search in the phi translation table for the translation of
   expression E in basic block PRED. Return the translated value, if
   found, NULL otherwise.  */ 

static inline tree
phi_trans_lookup (tree e, basic_block pred)
{
  void **slot;
  struct expr_pred_trans_d ept;
  ept.e = e;
  ept.pred = pred;
  ept.hashcode = vn_compute (e, (unsigned long) pred, NULL);
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
phi_trans_add (tree e, tree v, basic_block pred)
{
  void **slot;
  expr_pred_trans_t new_pair = xmalloc (sizeof (*new_pair));
  new_pair->e = e;
  new_pair->pred = pred;
  new_pair->v = v;
  new_pair->hashcode = vn_compute (e, (unsigned long) pred, NULL);
  slot = htab_find_slot_with_hash (phi_translate_table, new_pair,
				   new_pair->hashcode, INSERT);
  if (*slot)
    free (*slot);
  *slot = (void *) new_pair;
}


/* Add expression E to the expression set of value V.  */

void
add_to_value (tree v, tree e)
{
  /* Constants have no expression sets.  */
  if (is_gimple_min_invariant (v))
    return;

  if (VALUE_HANDLE_EXPR_SET (v) == NULL)
    VALUE_HANDLE_EXPR_SET (v) = set_new (false);

  insert_into_set (VALUE_HANDLE_EXPR_SET (v), e);
}


/* Return true if value V exists in the bitmap for SET.  */

static inline bool
value_exists_in_set_bitmap (value_set_t set, tree v)
{
  if (!set->values)
    return false;

  return bitmap_bit_p (set->values, VALUE_HANDLE_ID (v));
}


/* Remove value V from the bitmap for SET.  */

static void
value_remove_from_set_bitmap (value_set_t set, tree v)
{
  gcc_assert (set->indexed);

  if (!set->values)
    return;

  bitmap_clear_bit (set->values, VALUE_HANDLE_ID (v));
}


/* Insert the value number V into the bitmap of values existing in
   SET.  */

static inline void
value_insert_into_set_bitmap (value_set_t set, tree v)
{
  gcc_assert (set->indexed);

  if (set->values == NULL)
    set->values = BITMAP_ALLOC (&grand_bitmap_obstack);

  bitmap_set_bit (set->values, VALUE_HANDLE_ID (v));
}


/* Create a new bitmap set and return it.  */

static bitmap_set_t 
bitmap_set_new (void)
{
  bitmap_set_t ret = pool_alloc (bitmap_set_pool);
  ret->expressions = BITMAP_ALLOC (&grand_bitmap_obstack);
  ret->values = BITMAP_ALLOC (&grand_bitmap_obstack);
  return ret;
}

/* Create a new set.  */

static value_set_t
set_new  (bool indexed)
{
  value_set_t ret;
  ret = pool_alloc (value_set_pool);
  ret->head = ret->tail = NULL;
  ret->length = 0;
  ret->indexed = indexed;
  ret->values = NULL;
  return ret;
}

/* Insert an expression EXPR into a bitmapped set.  */

static void
bitmap_insert_into_set (bitmap_set_t set, tree expr)
{
  tree val;
  /* XXX: For now, we only let SSA_NAMES into the bitmap sets.  */
  gcc_assert (TREE_CODE (expr) == SSA_NAME);
  val = get_value_handle (expr);
  
  gcc_assert (val);
  if (!is_gimple_min_invariant (val))
  {
    bitmap_set_bit (set->values, VALUE_HANDLE_ID (val));
    bitmap_set_bit (set->expressions, SSA_NAME_VERSION (expr));
  }
}

/* Insert EXPR into SET.  */

static void
insert_into_set (value_set_t set, tree expr)
{
  value_set_node_t newnode = pool_alloc (value_set_node_pool);
  tree val = get_value_handle (expr);
  gcc_assert (val);
  
  if (is_gimple_min_invariant (val))
    return;

  /* For indexed sets, insert the value into the set value bitmap.
     For all sets, add it to the linked list and increment the list
     length.  */
  if (set->indexed)
    value_insert_into_set_bitmap (set, val);

  newnode->next = NULL;
  newnode->expr = expr;
  set->length ++;
  if (set->head == NULL)
    {
      set->head = set->tail = newnode;
    }
  else
    {
      set->tail->next = newnode;
      set->tail = newnode;
    }
}

/* Copy a bitmapped set ORIG, into bitmapped set DEST.  */

static void
bitmap_set_copy (bitmap_set_t dest, bitmap_set_t orig)
{
  bitmap_copy (dest->expressions, orig->expressions);
  bitmap_copy (dest->values, orig->values);
}

/* Copy the set ORIG to the set DEST.  */

static void
set_copy (value_set_t dest, value_set_t orig)
{
  value_set_node_t node;
 
  if (!orig || !orig->head)
    return;

  for (node = orig->head;
       node;
       node = node->next)
    {
      insert_into_set (dest, node->expr);
    }
}

/* Remove EXPR from SET.  */

static void
set_remove (value_set_t set, tree expr)
{
  value_set_node_t node, prev;

  /* Remove the value of EXPR from the bitmap, decrement the set
     length, and remove it from the actual double linked list.  */ 
  value_remove_from_set_bitmap (set, get_value_handle (expr));
  set->length--;
  prev = NULL;
  for (node = set->head; 
       node != NULL; 
       prev = node, node = node->next)
    {
      if (node->expr == expr)
	{
	  if (prev == NULL)
	    set->head = node->next;
	  else
	    prev->next= node->next;
 
	  if (node == set->tail)
	    set->tail = prev;
	  pool_free (value_set_node_pool, node);
	  return;
	}
    }
}

/* Return true if SET contains the value VAL.  */

static bool
set_contains_value (value_set_t set, tree val)
{
  /* All constants are in every set.  */
  if (is_gimple_min_invariant (val))
    return true;
  
  if (set->length == 0)
    return false;
  
  return value_exists_in_set_bitmap (set, val);
}

/* Return true if bitmapped set SET contains the expression EXPR.  */
static bool
bitmap_set_contains (bitmap_set_t set, tree expr)
{
  /* All constants are in every set.  */
  if (is_gimple_min_invariant (get_value_handle (expr)))
    return true;

  /* XXX: Bitmapped sets only contain SSA_NAME's for now.  */
  if (TREE_CODE (expr) != SSA_NAME)
    return false;
  return bitmap_bit_p (set->expressions, SSA_NAME_VERSION (expr));
}

  
/* Return true if bitmapped set SET contains the value VAL.  */

static bool
bitmap_set_contains_value (bitmap_set_t set, tree val)
{
  if (is_gimple_min_invariant (val))
    return true;
  return bitmap_bit_p (set->values, VALUE_HANDLE_ID (val));
}

/* Replace an instance of value LOOKFOR with expression EXPR in SET.  */

static void
bitmap_set_replace_value (bitmap_set_t set, tree lookfor, tree expr)
{
  value_set_t exprset;
  value_set_node_t node;
  if (is_gimple_min_invariant (lookfor))
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
  exprset = VALUE_HANDLE_EXPR_SET (lookfor);
  for (node = exprset->head; node; node = node->next)
    {
      if (TREE_CODE (node->expr) == SSA_NAME)
	{
	  if (bitmap_bit_p (set->expressions, SSA_NAME_VERSION (node->expr)))
	    {
	      bitmap_clear_bit (set->expressions, SSA_NAME_VERSION (node->expr));
	      bitmap_set_bit (set->expressions, SSA_NAME_VERSION (expr));
	      return;
	    }
	}
    }
}

/* Subtract bitmapped set B from value set A, and return the new set.  */

static value_set_t
bitmap_set_subtract_from_value_set (value_set_t a, bitmap_set_t b,
				    bool indexed)
{
  value_set_t ret = set_new (indexed);
  value_set_node_t node;
  for (node = a->head;
       node;
       node = node->next)
    {
      if (!bitmap_set_contains (b, node->expr))
	insert_into_set (ret, node->expr);
    }
  return ret;
}

/* Return true if two sets are equal.  */

static bool
set_equal (value_set_t a, value_set_t b)
{
  value_set_node_t node;

  if (a->length != b->length)
    return false;
  for (node = a->head;
       node;
       node = node->next)
    {
      if (!set_contains_value (b, get_value_handle (node->expr)))
	return false;
    }
  return true;
}

/* Replace an instance of EXPR's VALUE with EXPR in SET if it exists,
   and add it otherwise. */

static void
bitmap_value_replace_in_set (bitmap_set_t set, tree expr)
{
  tree val = get_value_handle (expr);
  if (bitmap_set_contains_value (set, val))
    bitmap_set_replace_value (set, val, expr);
  else
    bitmap_insert_into_set (set, expr);
}

/* Insert EXPR into SET if EXPR's value is not already present in
   SET.  */

static void
bitmap_value_insert_into_set (bitmap_set_t set, tree expr)
{
  tree val = get_value_handle (expr);

  if (is_gimple_min_invariant (val))
    return;
  
  if (!bitmap_set_contains_value (set, val))
    bitmap_insert_into_set (set, expr);
}

/* Insert the value for EXPR into SET, if it doesn't exist already.  */

static void
value_insert_into_set (value_set_t set, tree expr)
{
  tree val = get_value_handle (expr);

  /* Constant and invariant values exist everywhere, and thus,
     actually keeping them in the sets is pointless.  */
  if (is_gimple_min_invariant (val))
    return;

  if (!set_contains_value (set, val))
    insert_into_set (set, expr);
}


/* Print out SET to OUTFILE.  */

static void
bitmap_print_value_set (FILE *outfile, bitmap_set_t set,
			const char *setname, int blockindex)
{
  fprintf (outfile, "%s[%d] := { ", setname, blockindex);
  if (set)
    {
      bool first = true;
      unsigned i;
      bitmap_iterator bi;

      EXECUTE_IF_SET_IN_BITMAP (set->expressions, 0, i, bi)
	{
	  if (!first)
	    fprintf (outfile, ", ");
	  first = false;
	  print_generic_expr (outfile, ssa_name (i), 0);
	
	  fprintf (outfile, " (");
	  print_generic_expr (outfile, get_value_handle (ssa_name (i)), 0);
	  fprintf (outfile, ") ");
	}
    }
  fprintf (outfile, " }\n");
}
/* Print out the value_set SET to OUTFILE.  */

static void
print_value_set (FILE *outfile, value_set_t set,
		 const char *setname, int blockindex)
{
  value_set_node_t node;
  fprintf (outfile, "%s[%d] := { ", setname, blockindex);
  if (set)
    {
      for (node = set->head;
	   node;
	   node = node->next)
	{
	  print_generic_expr (outfile, node->expr, 0);
	  
	  fprintf (outfile, " (");
	  print_generic_expr (outfile, get_value_handle (node->expr), 0);
	  fprintf (outfile, ") ");
		     
	  if (node->next)
	    fprintf (outfile, ", ");
	}
    }

  fprintf (outfile, " }\n");
}

/* Print out the expressions that have VAL to OUTFILE.  */

void
print_value_expressions (FILE *outfile, tree val)
{
  if (VALUE_HANDLE_EXPR_SET (val))
    {
      char s[10];
      sprintf (s, "VH.%04d", VALUE_HANDLE_ID (val));
      print_value_set (outfile, VALUE_HANDLE_EXPR_SET (val), s, 0);
    }
}


void
debug_value_expressions (tree val)
{
  print_value_expressions (stderr, val);
}

  
void debug_value_set (value_set_t, const char *, int);

void
debug_value_set (value_set_t set, const char *setname, int blockindex)
{
  print_value_set (stderr, set, setname, blockindex);
}

/* Translate EXPR using phis in PHIBLOCK, so that it has the values of
   the phis in PRED.  Return NULL if we can't find a leader for each
   part of the translated expression.  */

static tree
phi_translate (tree expr, value_set_t set, basic_block pred,
	       basic_block phiblock)
{
  tree phitrans = NULL;
  tree oldexpr = expr;
  
  if (expr == NULL)
    return NULL;

  if (is_gimple_min_invariant (expr))
    return expr;

  /* Phi translations of a given expression don't change.  */
  phitrans = phi_trans_lookup (expr, pred);
  if (phitrans)
    return phitrans;
  
  switch (TREE_CODE_CLASS (TREE_CODE (expr)))
    {
    case tcc_reference:
      /* XXX: Until we have PRE of loads working, none will be ANTIC.  */
      return NULL;

    case tcc_binary:
      {
	tree oldop1 = TREE_OPERAND (expr, 0);
	tree oldop2 = TREE_OPERAND (expr, 1);
	tree newop1;
	tree newop2;
	tree newexpr;
	
	newop1 = phi_translate (find_leader (set, oldop1),
				set, pred, phiblock);
	if (newop1 == NULL)
	  return NULL;
	newop2 = phi_translate (find_leader (set, oldop2),
				set, pred, phiblock);
	if (newop2 == NULL)
	  return NULL;
	if (newop1 != oldop1 || newop2 != oldop2)
	  {
	    newexpr = pool_alloc (binary_node_pool);
	    memcpy (newexpr, expr, tree_size (expr));
	    create_tree_ann (newexpr);
	    TREE_OPERAND (newexpr, 0) = newop1 == oldop1 ? oldop1 : get_value_handle (newop1);
	    TREE_OPERAND (newexpr, 1) = newop2 == oldop2 ? oldop2 : get_value_handle (newop2);
	    vn_lookup_or_add (newexpr, NULL);
	    expr = newexpr;
	    phi_trans_add (oldexpr, newexpr, pred);	    
	  }
      }
      return expr;

    case tcc_unary:
      {
	tree oldop1 = TREE_OPERAND (expr, 0);
	tree newop1;
	tree newexpr;

	newop1 = phi_translate (find_leader (set, oldop1),
				set, pred, phiblock);
	if (newop1 == NULL)
	  return NULL;
	if (newop1 != oldop1)
	  {
	    newexpr = pool_alloc (unary_node_pool);
	    memcpy (newexpr, expr, tree_size (expr));
	    create_tree_ann (newexpr);	 
	    TREE_OPERAND (newexpr, 0) = get_value_handle (newop1);
	    vn_lookup_or_add (newexpr, NULL);
	    expr = newexpr;
	    phi_trans_add (oldexpr, newexpr, pred);
	  }
      }
      return expr;

    case tcc_exceptional:
      {
	tree phi = NULL;
	edge e;
	gcc_assert (TREE_CODE (expr) == SSA_NAME);
	if (TREE_CODE (SSA_NAME_DEF_STMT (expr)) == PHI_NODE)
	  phi = SSA_NAME_DEF_STMT (expr);
	else
	  return expr;
	
	e = find_edge (pred, bb_for_stmt (phi));
	if (e)
	  {
	    if (is_undefined_value (PHI_ARG_DEF (phi, e->dest_idx)))
	      return NULL;
	    vn_lookup_or_add (PHI_ARG_DEF (phi, e->dest_idx), NULL);
	    return PHI_ARG_DEF (phi, e->dest_idx);
	  }
      }
      return expr;

    default:
      gcc_unreachable ();
    }
}

static void
phi_translate_set (value_set_t dest, value_set_t set, basic_block pred,
		   basic_block phiblock)
{
  value_set_node_t node;
  for (node = set->head;
       node;
       node = node->next)
    {
      tree translated;
      translated = phi_translate (node->expr, set, pred, phiblock);
      phi_trans_add (node->expr, translated, pred);
      
      if (translated != NULL)
	value_insert_into_set (dest, translated);
    } 
}

/* Find the leader for a value (i.e., the name representing that
   value) in a given set, and return it.  Return NULL if no leader is
   found.  */

static tree
bitmap_find_leader (bitmap_set_t set, tree val)
{
  if (val == NULL)
    return NULL;
  
  if (is_gimple_min_invariant (val))
    return val;
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
      value_set_t exprset;
      value_set_node_t node;
      exprset = VALUE_HANDLE_EXPR_SET (val);
      for (node = exprset->head; node; node = node->next)
	{
	  if (TREE_CODE (node->expr) == SSA_NAME)
	    {
	      if (bitmap_bit_p (set->expressions, 
				SSA_NAME_VERSION (node->expr)))
		return node->expr;
	    }
	}
    }
  return NULL;
}

	
/* Find the leader for a value (i.e., the name representing that
   value) in a given set, and return it.  Return NULL if no leader is
   found.  */

static tree
find_leader (value_set_t set, tree val)
{
  value_set_node_t node;

  if (val == NULL)
    return NULL;

  /* Constants represent themselves.  */
  if (is_gimple_min_invariant (val))
    return val;

  if (set->length == 0)
    return NULL;
  
  if (value_exists_in_set_bitmap (set, val))
    {
      for (node = set->head;
	   node;
	   node = node->next)
	{
	  if (get_value_handle (node->expr) == val)
	    return node->expr;
	}
    }

  return NULL;
}

/* Determine if the expression EXPR is valid in SET.  This means that
   we have a leader for each part of the expression (if it consists of
   values), or the expression is an SSA_NAME.  

   NB:  We never should run into a case where we have SSA_NAME +
   SSA_NAME or SSA_NAME + value.  The sets valid_in_set is called on,
   the ANTIC sets, will only ever have SSA_NAME's or binary value
   expression (IE VALUE1 + VALUE2)  */

static bool
valid_in_set (value_set_t set, tree expr)
{
  switch (TREE_CODE_CLASS (TREE_CODE (expr)))
    {
    case tcc_binary:
      {
	tree op1 = TREE_OPERAND (expr, 0);
	tree op2 = TREE_OPERAND (expr, 1);
	return set_contains_value (set, op1) && set_contains_value (set, op2);
      }

    case tcc_unary:
      {
	tree op1 = TREE_OPERAND (expr, 0);
	return set_contains_value (set, op1);
      }

    case tcc_reference:
      /* XXX: Until PRE of loads works, no reference nodes are ANTIC.  */
      return false;

    case tcc_exceptional:
      gcc_assert (TREE_CODE (expr) == SSA_NAME);
      return true;

    default:
      /* No other cases should be encountered.  */
      gcc_unreachable (); 
   }
}

/* Clean the set of expressions that are no longer valid in SET.  This
   means expressions that are made up of values we have no leaders for
   in SET.  */

static void
clean (value_set_t set)
{
  value_set_node_t node;
  value_set_node_t next;
  node = set->head;
  while (node)
    {
      next = node->next;
      if (!valid_in_set (set, node->expr))	
	set_remove (set, node->expr);
      node = next;
    }
}

DEF_VEC_MALLOC_P (basic_block);
sbitmap has_abnormal_preds;

/* Compute the ANTIC set for BLOCK.

   If succs(BLOCK) > 1 then
     ANTIC_OUT[BLOCK] = intersection of ANTIC_IN[b] for all succ(BLOCK)
   else if succs(BLOCK) == 1 then
     ANTIC_OUT[BLOCK] = phi_translate (ANTIC_IN[succ(BLOCK)])

   ANTIC_IN[BLOCK] = clean(ANTIC_OUT[BLOCK] U EXP_GEN[BLOCK] - TMP_GEN[BLOCK])

   XXX: It would be nice to either write a set_clear, and use it for
   ANTIC_OUT, or to mark the antic_out set as deleted at the end
   of this routine, so that the pool can hand the same memory back out
   again for the next ANTIC_OUT.  */

static bool
compute_antic_aux (basic_block block, bool block_has_abnormal_pred_edge)
{
  basic_block son;
  bool changed = false;
  value_set_t S, old, ANTIC_OUT;
  value_set_node_t node;

  ANTIC_OUT = S = NULL;

  /* If any edges from predecessors are abnormal, antic_in is empty,
     so do nothing.  */
  if (block_has_abnormal_pred_edge)
    goto maybe_dump_sets;

  old = set_new (false);
  set_copy (old, ANTIC_IN (block));
  ANTIC_OUT = set_new (true);

  /* If the block has no successors, ANTIC_OUT is empty.  */
  if (EDGE_COUNT (block->succs) == 0)
    ;
  /* If we have one successor, we could have some phi nodes to
     translate through.  */
  else if (EDGE_COUNT (block->succs) == 1)
    {
      phi_translate_set (ANTIC_OUT, ANTIC_IN(EDGE_SUCC (block, 0)->dest),
			 block, EDGE_SUCC (block, 0)->dest);
    }
  /* If we have multiple successors, we take the intersection of all of
     them.  */
  else
    {
      VEC (basic_block) * worklist;
      edge e;
      size_t i;
      basic_block bprime, first;
      edge_iterator ei;

      worklist = VEC_alloc (basic_block, 2);
      FOR_EACH_EDGE (e, ei, block->succs)
	VEC_safe_push (basic_block, worklist, e->dest);
      first = VEC_index (basic_block, worklist, 0);
      set_copy (ANTIC_OUT, ANTIC_IN (first));

      for (i = 1; VEC_iterate (basic_block, worklist, i, bprime); i++)
	{
	  node = ANTIC_OUT->head;
	  while (node)
	    {
	      tree val;
	      value_set_node_t next = node->next;
	      val = get_value_handle (node->expr);
	      if (!set_contains_value (ANTIC_IN (bprime), val))
		set_remove (ANTIC_OUT, node->expr);
	      node = next;
	    }
	}
      VEC_free (basic_block, worklist);
    }

  /* Generate ANTIC_OUT - TMP_GEN.  */
  S = bitmap_set_subtract_from_value_set (ANTIC_OUT, TMP_GEN (block), false);

  /* Start ANTIC_IN with EXP_GEN - TMP_GEN */
  ANTIC_IN (block) = bitmap_set_subtract_from_value_set (EXP_GEN (block), 
							 TMP_GEN (block),
							 true);

  /* Then union in the ANTIC_OUT - TMP_GEN values,
     to get ANTIC_OUT U EXP_GEN - TMP_GEN */
  for (node = S->head; node; node = node->next)
    value_insert_into_set (ANTIC_IN (block), node->expr);

  clean (ANTIC_IN (block));
  if (!set_equal (old, ANTIC_IN (block)))
    changed = true;

 maybe_dump_sets:
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (ANTIC_OUT)
	print_value_set (dump_file, ANTIC_OUT, "ANTIC_OUT", block->index);
      print_value_set (dump_file, ANTIC_IN (block), "ANTIC_IN", block->index);
      if (S)
	print_value_set (dump_file, S, "S", block->index);
    }

  for (son = first_dom_son (CDI_POST_DOMINATORS, block);
       son;
       son = next_dom_son (CDI_POST_DOMINATORS, son))
    {
      changed |= compute_antic_aux (son,
				    TEST_BIT (has_abnormal_preds, son->index));
    }
  return changed;
}

/* Compute ANTIC sets.  */

static void
compute_antic (void)
{
  bool changed = true;
  int num_iterations = 0;
  basic_block block;

  /* If any predecessor edges are abnormal, we punt, so antic_in is empty.
     We pre-build the map of blocks with incoming abnormal edges here.  */
  has_abnormal_preds = sbitmap_alloc (last_basic_block);
  sbitmap_zero (has_abnormal_preds);
  FOR_EACH_BB (block)
    {
      edge_iterator ei;
      edge e;

      FOR_EACH_EDGE (e, ei, block->preds)
	if (e->flags & EDGE_ABNORMAL)
	  {
	    SET_BIT (has_abnormal_preds, block->index);
	    break;
	  }

      /* While we are here, give empty ANTIC_IN sets to each block.  */
      ANTIC_IN (block) = set_new (true);
    }
  /* At the exit block we anticipate nothing.  */
  ANTIC_IN (EXIT_BLOCK_PTR) = set_new (true);

  while (changed)
    {
      num_iterations++;
      changed = false;
      changed = compute_antic_aux (EXIT_BLOCK_PTR, false);
    }

  sbitmap_free (has_abnormal_preds);

  if (dump_file && (dump_flags & TDF_STATS))
    fprintf (dump_file, "compute_antic required %d iterations\n", num_iterations);
}

static VEC(tree_on_heap) *inserted_exprs;
/* Find a leader for an expression, or generate one using
   create_expression_by_pieces if it's ANTIC but
   complex.  
   BLOCK is the basic_block we are looking for leaders in.
   EXPR is the expression to find a leader or generate for. 
   STMTS is the statement list to put the inserted expressions on.
   Returns the SSA_NAME of the LHS of the generated expression or the
   leader.  */

static tree
find_or_generate_expression (basic_block block, tree expr, tree stmts)
{
  tree genop = bitmap_find_leader (AVAIL_OUT (block), expr);

  /* If it's still NULL, see if it is a complex expression, and if
     so, generate it recursively, otherwise, abort, because it's
     not really .  */
  if (genop == NULL)
    {
      genop = VALUE_HANDLE_EXPR_SET (expr)->head->expr;
      gcc_assert (UNARY_CLASS_P (genop)
		  || BINARY_CLASS_P (genop)
		  || REFERENCE_CLASS_P (genop));
      genop = create_expression_by_pieces (block, genop, stmts);
    }
  return genop;
}

#define NECESSARY(stmt)		stmt->common.asm_written_flag  
/* Create an expression in pieces, so that we can handle very complex
   expressions that may be ANTIC, but not necessary GIMPLE.  
   BLOCK is the basic block the expression will be inserted into,
   EXPR is the expression to insert (in value form)
   STMTS is a statement list to append the necessary insertions into.

   This function will abort if we hit some value that shouldn't be
   ANTIC but is (IE there is no leader for it, or its components).
   This function may also generate expressions that are themselves
   partially or fully redundant.  Those that are will be either made
   fully redundant during the next iteration of insert (for partially
   redundant ones), or eliminated by eliminate (for fully redundant
   ones).  */

static tree
create_expression_by_pieces (basic_block block, tree expr, tree stmts)
{
  tree name = NULL_TREE;
  tree newexpr = NULL_TREE;
  tree v;
  
  switch (TREE_CODE_CLASS (TREE_CODE (expr)))
    {
    case tcc_binary:
      {
	tree_stmt_iterator tsi;
	tree forced_stmts;
	tree genop1, genop2;
	tree temp;
	tree folded;
	tree op1 = TREE_OPERAND (expr, 0);
	tree op2 = TREE_OPERAND (expr, 1);
	genop1 = find_or_generate_expression (block, op1, stmts);
	genop2 = find_or_generate_expression (block, op2, stmts);
	temp = create_tmp_var (TREE_TYPE (expr), "pretmp");
	add_referenced_tmp_var (temp);
	
	folded = fold (build (TREE_CODE (expr), TREE_TYPE (expr), 
			      genop1, genop2));
	newexpr = force_gimple_operand (folded, &forced_stmts, false, NULL);
	if (forced_stmts)
	  {
	    tsi = tsi_start (forced_stmts);
	    for (; !tsi_end_p (tsi); tsi_next (&tsi))
	      {
		tree stmt = tsi_stmt (tsi);
		tree forcedname = TREE_OPERAND (stmt, 0);
		tree forcedexpr = TREE_OPERAND (stmt, 1);
		tree val = vn_lookup_or_add (forcedexpr, NULL);
		vn_add (forcedname, val, NULL);		
		bitmap_value_replace_in_set (NEW_SETS (block), forcedname); 
		bitmap_value_replace_in_set (AVAIL_OUT (block), forcedname);
	      }

	    tsi = tsi_last (stmts);
	    tsi_link_after (&tsi, forced_stmts, TSI_CONTINUE_LINKING);
	  }
	newexpr = build (MODIFY_EXPR, TREE_TYPE (expr),
			 temp, newexpr);
	NECESSARY (newexpr) = 0;
	name = make_ssa_name (temp, newexpr);
	TREE_OPERAND (newexpr, 0) = name;
	tsi = tsi_last (stmts);
	tsi_link_after (&tsi, newexpr, TSI_CONTINUE_LINKING);
	VEC_safe_push (tree_on_heap, inserted_exprs, newexpr);
	pre_stats.insertions++;
	break;
      }
    case tcc_unary:
      {
	tree_stmt_iterator tsi;
	tree forced_stmts = NULL;
	tree genop1;
	tree temp;
	tree folded;
	tree op1 = TREE_OPERAND (expr, 0);
	genop1 = find_or_generate_expression (block, op1, stmts);
	temp = create_tmp_var (TREE_TYPE (expr), "pretmp");
	add_referenced_tmp_var (temp);
	folded = fold (build (TREE_CODE (expr), TREE_TYPE (expr), 
			      genop1));
	/* If the generated operand  is already GIMPLE min_invariant
	   just use it instead of calling force_gimple_operand on it,
	   since that may make it not invariant by copying it into an
	   assignment.  */
	if (!is_gimple_min_invariant (genop1))
	  newexpr = force_gimple_operand (folded, &forced_stmts, false, NULL);
	else
	  newexpr = genop1;
	if (forced_stmts)
	  {
	    tsi = tsi_start (forced_stmts);
	    for (; !tsi_end_p (tsi); tsi_next (&tsi))
	      {
		tree stmt = tsi_stmt (tsi);
		tree forcedname = TREE_OPERAND (stmt, 0);
		tree forcedexpr = TREE_OPERAND (stmt, 1);
		tree val = vn_lookup_or_add (forcedexpr, NULL);
		vn_add (forcedname, val, NULL);		
		bitmap_value_replace_in_set (NEW_SETS (block), forcedname); 
		bitmap_value_replace_in_set (AVAIL_OUT (block), forcedname);
	      }
	    tsi = tsi_last (stmts);
	    tsi_link_after (&tsi, forced_stmts, TSI_CONTINUE_LINKING);
	  }
	newexpr = build (MODIFY_EXPR, TREE_TYPE (expr),
			 temp, newexpr);
	name = make_ssa_name (temp, newexpr);
	TREE_OPERAND (newexpr, 0) = name;
	NECESSARY (newexpr) = 0;
	tsi = tsi_last (stmts);
	tsi_link_after (&tsi, newexpr, TSI_CONTINUE_LINKING);
	VEC_safe_push (tree_on_heap, inserted_exprs, newexpr);
	pre_stats.insertions++;

	break;
      }
    default:
      gcc_unreachable ();
      
    }
  v = get_value_handle (expr);
  vn_add (name, v, NULL);

  /* The value may already exist in either NEW_SETS, or AVAIL_OUT, because
     we are creating the expression by pieces, and this particular piece of
     the expression may have been represented.  There is no harm in replacing
     here.  */
  bitmap_value_replace_in_set (NEW_SETS (block), name); 
  bitmap_value_replace_in_set (AVAIL_OUT (block), name);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {				    
      fprintf (dump_file, "Inserted ");
      print_generic_expr (dump_file, newexpr, 0);
      fprintf (dump_file, " in predecessor %d\n", block->index);
    }
  return name;
}

/* Return the folded version of T if T, when folded, is a gimple
   min_invariant.  Otherwise, return T. */ 

static tree
fully_constant_expression (tree t)
{  
  tree folded;
  folded = fold (t);
  if (folded && is_gimple_min_invariant (folded))
    return folded;
  return t;
}

/* Insert the to-be-made-available values of NODE for each predecessor, stored
   in AVAIL, into the predecessors of BLOCK, and merge the result with a phi
   node, given the same value handle as NODE.  The prefix of the phi node is
   given with TMPNAME.  Return true if we have inserted new stuff.  */

static bool
insert_into_preds_of_block (basic_block block, value_set_node_t node,
			    tree *avail, const char *tmpname)
{
  tree val = get_value_handle (node->expr);
  edge pred;
  bool insertions = false;
  bool nophi = false;
  basic_block bprime;
  tree eprime;
  edge_iterator ei;
  tree type = TREE_TYPE (avail[EDGE_PRED (block, 0)->src->index]);
  tree temp;
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Found partial redundancy for expression ");
      print_generic_expr (dump_file, node->expr, 0);
      fprintf (dump_file, "\n");
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
      if (firstinsideloop ^ secondinsideloop)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Skipping insertion of phi for partial redundancy: Looks like an induction variable\n");
	  nophi = true;
	}
    }
	  

  /* Make the necessary insertions.  */
  FOR_EACH_EDGE (pred, ei, block->preds)
    {
      tree stmts = alloc_stmt_list ();
      tree builtexpr;
      bprime = pred->src;
      eprime = avail[bprime->index];
      if (BINARY_CLASS_P (eprime)
	  || UNARY_CLASS_P (eprime))
	{
	  builtexpr = create_expression_by_pieces (bprime,
						   eprime,
						   stmts);
	  bsi_insert_on_edge (pred, stmts);
	  avail[bprime->index] = builtexpr;
	  insertions = true;
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
  temp = create_tmp_var (type, tmpname);
  add_referenced_tmp_var (temp);
  temp = create_phi_node (temp, block);
  NECESSARY (temp) = 0; 
  VEC_safe_push (tree_on_heap, inserted_exprs, temp);
  FOR_EACH_EDGE (pred, ei, block->preds)
    add_phi_arg (temp, avail[pred->src->index], pred);
  
  vn_add (PHI_RESULT (temp), val, NULL);
  
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

  bitmap_insert_into_set (PHI_GEN (block),
			  PHI_RESULT (temp));
  bitmap_value_replace_in_set (AVAIL_OUT (block), 
			       PHI_RESULT (temp));
  bitmap_insert_into_set (NEW_SETS (block),
			  PHI_RESULT (temp));
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Created phi ");
      print_generic_expr (dump_file, temp, 0);
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

*/

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
	      EXECUTE_IF_SET_IN_BITMAP (newset->expressions, 0, i, bi)
		{
		  bitmap_value_replace_in_set (NEW_SETS (block), ssa_name (i));
		  bitmap_value_replace_in_set (AVAIL_OUT (block), ssa_name (i));
		}
	    }
	  if (EDGE_COUNT (block->preds) > 1)
	    {
	      value_set_node_t node;
	      for (node = ANTIC_IN (block)->head;
		   node;
		   node = node->next)
		{
		  if (BINARY_CLASS_P (node->expr)
		      || UNARY_CLASS_P (node->expr))
		    {
		      tree *avail;
		      tree val;
		      bool by_some = false;
		      bool cant_insert = false;
		      bool all_same = true;
		      tree first_s = NULL;
		      edge pred;
		      basic_block bprime;
		      tree eprime = NULL_TREE;
		      edge_iterator ei;

		      val = get_value_handle (node->expr);
		      if (bitmap_set_contains_value (PHI_GEN (block), val))
			continue; 
		      if (bitmap_set_contains_value (AVAIL_OUT (dom), val))
			{
			  if (dump_file && (dump_flags & TDF_DETAILS))
			    fprintf (dump_file, "Found fully redundant value\n");
			  continue;
			}
					      
		      avail = xcalloc (last_basic_block, sizeof (tree));
		      FOR_EACH_EDGE (pred, ei, block->preds)
			{
			  tree vprime;
			  tree edoubleprime;

			  /* This can happen in the very weird case
			     that our fake infinite loop edges have caused a
			     critical edge to appear.  */
			  if (EDGE_CRITICAL_P (pred))
			    {
			      cant_insert = true;
			      break;
			    }
			  bprime = pred->src;
			  eprime = phi_translate (node->expr,
						  ANTIC_IN (block),
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
			  vprime = get_value_handle (eprime);
			  gcc_assert (vprime);
			  edoubleprime = bitmap_find_leader (AVAIL_OUT (bprime),
							     vprime);
			  if (edoubleprime == NULL)
			    {
			      avail[bprime->index] = eprime;
			      all_same = false;
			    }
			  else
			    {
			      avail[bprime->index] = edoubleprime;
			      by_some = true; 
			      if (first_s == NULL)
				first_s = edoubleprime;
			      else if (!operand_equal_p (first_s, edoubleprime,
							 0))
				all_same = false;
			    }
			}
		      /* If we can insert it, it's not the same value
			 already existing along every predecessor, and
			 it's defined by some predecessor, it is
			 partially redundant.  */
		      if (!cant_insert && !all_same && by_some)
			{
 			  if (insert_into_preds_of_block (block, node, avail, 
 							  "prephitmp"))
 			    new_stuff = true;
			}
		      /* If all edges produce the same value and that value is
			 an invariant, then the PHI has the same value on all
			 edges.  Note this.  */
		      else if (!cant_insert && all_same && eprime 
			       && is_gimple_min_invariant (eprime)
			       && !is_gimple_min_invariant (val))
			{
			  value_set_t exprset = VALUE_HANDLE_EXPR_SET (val);
			  value_set_node_t node;
			  for (node = exprset->head; node; node = node->next)
 			    {
			      if (TREE_CODE (node->expr) == SSA_NAME)
				{				  
				  vn_add (node->expr, eprime, NULL);
				  pre_stats.constified++;
				}
 			    }
			}
		      free (avail);
		    }
		}
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
      new_stuff = false;
      new_stuff = insert_aux (ENTRY_BLOCK_PTR);
    }
  if (num_iterations > 2 && dump_file && (dump_flags & TDF_STATS))
    fprintf (dump_file, "insert required %d iterations\n", num_iterations);
}


/* Return true if VAR is an SSA variable with no defining statement in
   this procedure, *AND* isn't a live-on-entry parameter.  */

static bool
is_undefined_value (tree expr)
{
  return (TREE_CODE (expr) == SSA_NAME
          && IS_EMPTY_STMT (SSA_NAME_DEF_STMT (expr))
	  /* PARM_DECLs and hard registers are always defined.  */
	  && TREE_CODE (SSA_NAME_VAR (expr)) != PARM_DECL);
}


/* Given an SSA variable VAR and an expression EXPR, compute the value
   number for EXPR and create a value handle (VAL) for it.  If VAR and
   EXPR are not the same, associate VAL with VAR.  Finally, add VAR to
   S1 and its value handle to S2.

   VUSES represent the virtual use operands associated with EXPR (if
   any). They are used when computing the hash value for EXPR.  */

static inline void
add_to_sets (tree var, tree expr, vuse_optype vuses, bitmap_set_t s1,
	     bitmap_set_t s2)
{
  tree val = vn_lookup_or_add (expr, vuses);

  /* VAR and EXPR may be the same when processing statements for which
     we are not computing value numbers (e.g., non-assignments, or
     statements that make aliased stores).  In those cases, we are
     only interested in making VAR available as its own value.  */
  if (var != expr)
    vn_add (var, val, NULL);

  if (s1)
    bitmap_insert_into_set (s1, var);
  bitmap_value_insert_into_set (s2, var);
}


/* Given a unary or binary expression EXPR, create and return a new
   expression with the same structure as EXPR but with its operands
   replaced with the value handles of each of the operands of EXPR.
   Insert EXPR's operands into the EXP_GEN set for BLOCK.

   VUSES represent the virtual use operands associated with EXPR (if
   any). They are used when computing the hash value for EXPR.  */

static inline tree
create_value_expr_from (tree expr, basic_block block, vuse_optype vuses)
{
  int i;
  enum tree_code code = TREE_CODE (expr);
  tree vexpr;

  gcc_assert (TREE_CODE_CLASS (code) == tcc_unary
	      || TREE_CODE_CLASS (code) == tcc_binary
	      || TREE_CODE_CLASS (code) == tcc_reference);

  if (TREE_CODE_CLASS (code) == tcc_unary)
    vexpr = pool_alloc (unary_node_pool);
  else if (TREE_CODE_CLASS (code) == tcc_reference)
    vexpr = pool_alloc (reference_node_pool);
  else
    vexpr = pool_alloc (binary_node_pool);

  memcpy (vexpr, expr, tree_size (expr));

  for (i = 0; i < TREE_CODE_LENGTH (code); i++)
    {
      tree op = TREE_OPERAND (expr, i);
      if (op != NULL)
	{
	  tree val = vn_lookup_or_add (op, vuses);
	  if (!is_undefined_value (op))
	    value_insert_into_set (EXP_GEN (block), op);
	  if (TREE_CODE (val) == VALUE_HANDLE)
	    TREE_TYPE (val) = TREE_TYPE (TREE_OPERAND (vexpr, i));
	  TREE_OPERAND (vexpr, i) = val;
	}
    }

  return vexpr;
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
  tree param;

  /* For arguments with default definitions, we pretend they are
     defined in the entry block.  */
  for (param = DECL_ARGUMENTS (current_function_decl);
       param;
       param = TREE_CHAIN (param))
    {
      if (default_def (param) != NULL)
	{
	  tree val;
	  tree def = default_def (param);
	  val = vn_lookup_or_add (def, NULL);
	  bitmap_insert_into_set (TMP_GEN (ENTRY_BLOCK_PTR), def);
	  bitmap_value_insert_into_set (AVAIL_OUT (ENTRY_BLOCK_PTR), def);
	}
    }

  /* Allocate the worklist.  */
  worklist = xmalloc (sizeof (basic_block) * n_basic_blocks);

  /* Seed the algorithm by putting the dominator children of the entry
     block on the worklist.  */
  for (son = first_dom_son (CDI_DOMINATORS, ENTRY_BLOCK_PTR);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    worklist[sp++] = son;

  /* Loop until the worklist is empty.  */
  while (sp)
    {
      block_stmt_iterator bsi;
      tree stmt, phi;
      basic_block dom;

      /* Pick a block from the worklist.  */
      block = worklist[--sp];

      /* Initially, the set of available values in BLOCK is that of
	 its immediate dominator.  */
      dom = get_immediate_dominator (CDI_DOMINATORS, block);
      if (dom)
	bitmap_set_copy (AVAIL_OUT (block), AVAIL_OUT (dom));

      /* Generate values for PHI nodes.  */
      for (phi = phi_nodes (block); phi; phi = PHI_CHAIN (phi))
	/* We have no need for virtual phis, as they don't represent
	   actual computations.  */
	if (is_gimple_reg (PHI_RESULT (phi)))
	  add_to_sets (PHI_RESULT (phi), PHI_RESULT (phi), NULL,
		       PHI_GEN (block), AVAIL_OUT (block));

      /* Now compute value numbers and populate value sets with all
	 the expressions computed in BLOCK.  */
      for (bsi = bsi_start (block); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  stmt_ann_t ann;
	  size_t j;

	  stmt = bsi_stmt (bsi);
	  ann = stmt_ann (stmt);
	  get_stmt_operands (stmt);

	  /* We are only interested in assignments of the form
	     X_i = EXPR, where EXPR represents an "interesting"
	     computation, it has no volatile operands and X_i
	     doesn't flow through an abnormal edge.  */
	  if (TREE_CODE (stmt) == MODIFY_EXPR
	      && !ann->has_volatile_ops
	      && TREE_CODE (TREE_OPERAND (stmt, 0)) == SSA_NAME
	      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (TREE_OPERAND (stmt, 0)))
	    {
	      tree lhs = TREE_OPERAND (stmt, 0);
	      tree rhs = TREE_OPERAND (stmt, 1);
	      vuse_optype vuses = STMT_VUSE_OPS (stmt);

	      STRIP_USELESS_TYPE_CONVERSION (rhs);
	      if (TREE_CODE (rhs) == SSA_NAME
		  || is_gimple_min_invariant (rhs))
		{
		  /* Compute a value number for the RHS of the statement
		     and add its value to the AVAIL_OUT set for the block.
		     Add the LHS to TMP_GEN.  */
		  add_to_sets (lhs, rhs, vuses, TMP_GEN (block), 
			       AVAIL_OUT (block));
		  
		  if (TREE_CODE (rhs) == SSA_NAME
		      && !is_undefined_value (rhs))
		    value_insert_into_set (EXP_GEN (block), rhs);
		  continue;
		}	   
	      else if (UNARY_CLASS_P (rhs) || BINARY_CLASS_P (rhs)
		       || TREE_CODE (rhs) == INDIRECT_REF)
		{
		  /* For binary, unary, and reference expressions,
		     create a duplicate expression with the operands
		     replaced with the value handles of the original
		     RHS.  */
		  tree newt = create_value_expr_from (rhs, block, vuses);
		  add_to_sets (lhs, newt, vuses, TMP_GEN (block),
			       AVAIL_OUT (block));
		  value_insert_into_set (EXP_GEN (block), newt);
		  continue;
		}
	    }

	  /* For any other statement that we don't recognize, simply
	     make the names generated by the statement available in
	     AVAIL_OUT and TMP_GEN.  */
	  for (j = 0; j < NUM_DEFS (STMT_DEF_OPS (stmt)); j++)
	    {
	      tree def = DEF_OP (STMT_DEF_OPS (stmt), j);
	      add_to_sets (def, def, NULL, TMP_GEN (block),
			    AVAIL_OUT (block));
	    }

	  for (j = 0; j < NUM_USES (STMT_USE_OPS (stmt)); j++)
	    {
	      tree use = USE_OP (STMT_USE_OPS (stmt), j);
	      add_to_sets (use, use, NULL, NULL, AVAIL_OUT (block));
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


/* Eliminate fully redundant computations.  */

static void
eliminate (void)
{
  basic_block b;

  FOR_EACH_BB (b)
    {
      block_stmt_iterator i;
      
      for (i = bsi_start (b); !bsi_end_p (i); bsi_next (&i))
        {
          tree stmt = bsi_stmt (i);

	  /* Lookup the RHS of the expression, see if we have an
	     available computation for it.  If so, replace the RHS with
	     the available computation.  */
	  if (TREE_CODE (stmt) == MODIFY_EXPR
	      && TREE_CODE (TREE_OPERAND (stmt, 0)) == SSA_NAME
	      && TREE_CODE (TREE_OPERAND (stmt ,1)) != SSA_NAME
	      && !is_gimple_min_invariant (TREE_OPERAND (stmt, 1))
	      && !stmt_ann (stmt)->has_volatile_ops)
	    {
	      tree lhs = TREE_OPERAND (stmt, 0);
	      tree *rhs_p = &TREE_OPERAND (stmt, 1);
	      tree sprime;

	      sprime = bitmap_find_leader (AVAIL_OUT (b),
					   vn_lookup (lhs, NULL));
	      if (sprime 
		  && sprime != lhs
		  && (TREE_CODE (*rhs_p) != SSA_NAME
		      || may_propagate_copy (*rhs_p, sprime)))
		{
		  gcc_assert (sprime != *rhs_p);

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Replaced ");
		      print_generic_expr (dump_file, *rhs_p, 0);
		      fprintf (dump_file, " with ");
		      print_generic_expr (dump_file, sprime, 0);
		      fprintf (dump_file, " in ");
		      print_generic_stmt (dump_file, stmt, 0);
		    }
		  if (TREE_CODE (sprime) == SSA_NAME) 
		    NECESSARY (SSA_NAME_DEF_STMT (sprime)) = 1;
		  pre_stats.eliminations++;
		  propagate_tree_value (rhs_p, sprime);
		  modify_stmt (stmt);

		  /* If we removed EH side effects from the statement, clean
		     its EH information.  */
		  if (maybe_clean_eh_stmt (stmt))
		    {
		      bitmap_set_bit (need_eh_cleanup,
				      bb_for_stmt (stmt)->index);
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file, "  Removed EH side effects.\n");
		    }
		}
	    }
        }
    }
}

/* Borrow a bit of tree-ssa-dce.c for the moment.
   XXX: In 4.1, we should be able to just run a DCE pass after PRE, though
   this may be a bit faster, and we may want critical edges kept split.  */

/* If OP's defining statement has not already been determined to be necessary,
   mark that statement necessary. and place it on the WORKLIST.  */ 

static inline void
mark_operand_necessary (tree op, VEC(tree_on_heap) **worklist)
{
  tree stmt;
  int ver;

  gcc_assert (op);

  ver = SSA_NAME_VERSION (op);

  stmt = SSA_NAME_DEF_STMT (op);
  gcc_assert (stmt);

  if (NECESSARY (stmt)
      || IS_EMPTY_STMT (stmt))
    return;

  NECESSARY (stmt) = 1;
  VEC_safe_push (tree_on_heap, *worklist, stmt);
}

/* Because we don't follow exactly the standard PRE algorithm, and decide not
   to insert PHI nodes sometimes, and because value numbering of casts isn't
   perfect, we sometimes end up inserting dead code.   This simple DCE-like
   pass removes any insertions we made that weren't actually used.  */

static void
remove_dead_inserted_code (void)
{
  VEC (tree_on_heap) *worklist = NULL;
  int i;
  tree t;

  for (i = 0; VEC_iterate (tree_on_heap, inserted_exprs, i, t); i++)
    {
      if (NECESSARY (t))
	VEC_safe_push (tree_on_heap, worklist, t);
    }
  while (VEC_length (tree_on_heap, worklist) > 0)
    {
      t = VEC_pop (tree_on_heap, worklist);
      if (TREE_CODE (t) == PHI_NODE)
	{
	  /* PHI nodes are somewhat special in that each PHI alternative has
	     data and control dependencies.  All the statements feeding the
	     PHI node's arguments are always necessary.  In aggressive mode,
	     we also consider the control dependent edges leading to the
	     predecessor block associated with each PHI alternative as
	     necessary.  */
	  int k;
	  for (k = 0; k < PHI_NUM_ARGS (t); k++)
            {
	      tree arg = PHI_ARG_DEF (t, k);
	      if (TREE_CODE (arg) == SSA_NAME)
		mark_operand_necessary (arg, &worklist);
	    }
	}
      else
	{
	  /* Propagate through the operands.  Examine all the USE, VUSE and
	     V_MAY_DEF operands in this statement.  Mark all the statements 
	     which feed this statement's uses as necessary.  */
	  ssa_op_iter iter;
	  tree use;

	  get_stmt_operands (t);

	  /* The operands of V_MAY_DEF expressions are also needed as they
	     represent potential definitions that may reach this
	     statement (V_MAY_DEF operands allow us to follow def-def 
	     links).  */

	  FOR_EACH_SSA_TREE_OPERAND (use, t, iter, SSA_OP_ALL_USES)
	    mark_operand_necessary (use, &worklist);
	}
    }
  for (i = 0; VEC_iterate (tree_on_heap, inserted_exprs, i, t); i++)
    {
      if (!NECESSARY (t))
	{
	  block_stmt_iterator bsi;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Removing unnecessary insertion:");
	      print_generic_stmt (dump_file, t, 0);
	    }
	  if (TREE_CODE (t) == PHI_NODE)
	    {
	      remove_phi_node (t, NULL, bb_for_stmt (t));
	    }
	  else
	    {
	      bsi = bsi_for_stmt (t);
	      bsi_remove (&bsi);
	    }
	}
    }
  VEC_free (tree_on_heap, worklist);
}
/* Initialize data structures used by PRE.  */

static void
init_pre (bool do_fre)
{
  basic_block bb;

  inserted_exprs = NULL;
  vn_init ();
  if (!do_fre)
    current_loops = loop_optimizer_init (dump_file);
  connect_infinite_loops_to_exit ();
  memset (&pre_stats, 0, sizeof (pre_stats));

  /* If block 0 has more than one predecessor, it means that its PHI
     nodes will have arguments coming from block -1.  This creates
     problems for several places in PRE that keep local arrays indexed
     by block number.  To prevent this, we split the edge coming from
     ENTRY_BLOCK_PTR (FIXME, if ENTRY_BLOCK_PTR had an index number
     different than -1 we wouldn't have to hack this.  tree-ssa-dce.c
     needs a similar change).  */
  if (EDGE_COUNT (EDGE_SUCC (ENTRY_BLOCK_PTR, 0)->dest->preds) > 1)
    if (!(EDGE_SUCC (ENTRY_BLOCK_PTR, 0)->flags & EDGE_ABNORMAL))
      split_edge (EDGE_SUCC (ENTRY_BLOCK_PTR, 0));

  FOR_ALL_BB (bb)
    bb->aux = xcalloc (1, sizeof (struct bb_value_sets));

  bitmap_obstack_initialize (&grand_bitmap_obstack);
  phi_translate_table = htab_create (511, expr_pred_trans_hash,
				     expr_pred_trans_eq, free);
  value_set_pool = create_alloc_pool ("Value sets",
				      sizeof (struct value_set), 30);
  bitmap_set_pool = create_alloc_pool ("Bitmap sets",
				       sizeof (struct bitmap_set), 30);
  value_set_node_pool = create_alloc_pool ("Value set nodes",
				           sizeof (struct value_set_node), 30);
  calculate_dominance_info (CDI_POST_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);
  binary_node_pool = create_alloc_pool ("Binary tree nodes",
				        tree_code_size (PLUS_EXPR), 30);
  unary_node_pool = create_alloc_pool ("Unary tree nodes",
				       tree_code_size (NEGATE_EXPR), 30);
  reference_node_pool = create_alloc_pool ("Reference tree nodes",
					   tree_code_size (ARRAY_REF), 30);
  FOR_ALL_BB (bb)
    {
      EXP_GEN (bb) = set_new (true);
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
  unsigned int i;

  VEC_free (tree_on_heap, inserted_exprs);
  bitmap_obstack_release (&grand_bitmap_obstack);
  free_alloc_pool (value_set_pool);
  free_alloc_pool (bitmap_set_pool);
  free_alloc_pool (value_set_node_pool);
  free_alloc_pool (binary_node_pool);
  free_alloc_pool (reference_node_pool);
  free_alloc_pool (unary_node_pool);
  htab_delete (phi_translate_table);
  remove_fake_exit_edges ();

  FOR_ALL_BB (bb)
    {
      free (bb->aux);
      bb->aux = NULL;
    }

  free_dominance_info (CDI_POST_DOMINATORS);
  vn_delete ();

  if (!bitmap_empty_p (need_eh_cleanup))
    {
      tree_purge_all_dead_eh_edges (need_eh_cleanup);
      cleanup_tree_cfg ();
    }

  BITMAP_FREE (need_eh_cleanup);

  /* Wipe out pointers to VALUE_HANDLEs.  In the not terribly distant
     future we will want them to be persistent though.  */
  for (i = 0; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);

      if (!name)
	continue;

      if (SSA_NAME_VALUE (name)
	  && TREE_CODE (SSA_NAME_VALUE (name)) == VALUE_HANDLE)
	SSA_NAME_VALUE (name) = NULL;
    }
  if (!do_fre && current_loops)
    {
      loop_optimizer_finalize (current_loops, dump_file);
      current_loops = NULL;
    }
}


/* Main entry point to the SSA-PRE pass.  DO_FRE is true if the caller
   only wants to do full redundancy elimination.  */

static void
execute_pre (bool do_fre)
{
  init_pre (do_fre);

  /* Collect and value number expressions computed in each basic block.  */
  compute_avail ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      basic_block bb;

      FOR_ALL_BB (bb)
	{
	  print_value_set (dump_file, EXP_GEN (bb), "exp_gen", bb->index);
	  bitmap_print_value_set (dump_file, TMP_GEN (bb), "tmp_gen", 
				  bb->index);
	  bitmap_print_value_set (dump_file, AVAIL_OUT (bb), "avail_out", 
				  bb->index);
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
  eliminate ();


  if (dump_file && (dump_flags & TDF_STATS))
    {
      fprintf (dump_file, "Insertions:%d\n", pre_stats.insertions);
      fprintf (dump_file, "New PHIs:%d\n", pre_stats.phis);
      fprintf (dump_file, "Eliminated:%d\n", pre_stats.eliminations);
      fprintf (dump_file, "Constified:%d\n", pre_stats.constified);
    }
  
  bsi_commit_edge_inserts ();
  if (!do_fre)
    remove_dead_inserted_code ();
  fini_pre (do_fre);

}


/* Gate and execute functions for PRE.  */

static void
do_pre (void)
{
  execute_pre (false);
}

static bool
gate_pre (void)
{
  return flag_tree_pre != 0;
}

struct tree_opt_pass pass_pre =
{
  "pre",				/* name */
  gate_pre,				/* gate */
  do_pre,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_PRE,				/* tv_id */
  PROP_no_crit_edges | PROP_cfg
    | PROP_ssa | PROP_alias,		/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_ggc_collect | TODO_verify_ssa, /* todo_flags_finish */
  0					/* letter */
};


/* Gate and execute functions for FRE.  */

static void
do_fre (void)
{
  execute_pre (true);
}

static bool
gate_fre (void)
{
  return flag_tree_fre != 0;
}

struct tree_opt_pass pass_fre =
{
  "fre",				/* name */
  gate_fre,				/* gate */
  do_fre,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_FRE,				/* tv_id */
  PROP_cfg | PROP_ssa | PROP_alias,	/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_ggc_collect | TODO_verify_ssa, /* todo_flags_finish */
  0					/* letter */
};
