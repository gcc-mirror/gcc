/* SSA-PRE for trees.
   Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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

/* These RTL headers are needed for basic-block.h.  */
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
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
#include "splay-tree.h"
#include "bitmap.h"
#include "langhooks.h"
/* TODO:
   
   1. Implement load value numbering.
   2. Speed up insert_aux so that we can use it all the time.  It
      spends most of it's time in quadratic value replacement.
   3. Avail sets can be shared by making an avail_find_leader that
      walks up the dominator tree and looks in those avail sets.
      This might affect code optimality, it's unclear right now.
   4. Load motion can be performed by value numbering the loads the
      same as we do other expressions.  This requires iterative
      hashing the vuses into the values.  Right now we simply assign
      a new value every time we see a statement with a vuse.
   5. Strength reduction can be performed by anticipating expressions
      we can repair later on.
*/   

/* For ease of terminology, "expression node" in the below refers to
   every expression node but MODIFY_EXPR, because MODIFY_EXPR's represent
   the actual statement containing the expressions we care about, and
   we cache the value number by putting it in the expression.  */

/* Basic algorithm
   
   First we walk the statements to generate the AVAIL sets, the EXP_GEN
   sets, and the tmp_gen sets.  AVAIL is a forward dataflow
   problem. EXP_GEN sets represent the generation of
   values/expressions by a given block.  We use them when computing
   the ANTIC sets.  The AVAIL sets consist of SSA_NAME's that
   represent values, so we know what values are available in what
   blocks.  In SSA, values are never killed, so we don't need a kill
   set, or a fixpoint iteration, in order to calculate the AVAIL sets.
   In traditional parlance, AVAIL sets tell us the downsafety of the
   expressions/values.
   
   Next, we generate the ANTIC sets.  ANTIC is a backwards dataflow
   problem.  These sets represent the anticipatable expressions.  An
   expression is anticipatable in a given block if it could be
   generated in that block.  This means that if we had to perform an
   insertion in that block, of the value of that expression, we could.
   Calculating the ANTIC sets requires phi translation of expressions,
   because the flow goes backwards through phis.  We must iterate to a
   fixpoint of the ANTIC sets, because we have a kill set.
   Even in SSA form, values are not live over the entire function,
   only from their definition point onwards.  So we have to remove
   values from the ANTIC set once we go past the definition point of
   the leaders that make them up.  compute_antic/compute_antic_aux
   performs this computation.

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
   disclosed in a moment, expression nodes and constant nodes) has a
   value handle that can be retrieved through get_value_handle.  This
   value handle, *is* the value number of the SSA_NAME.  You can
   pointer compare the value handles for equivalence purposes.

   For debugging reasons, the value handle is internally more than
   just a number, it is a VAR_DECL named "value.x", where x is a
   unique number for each value number in use.  This allows
   expressions with SSA_NAMES replaced by value handles to still be
   pretty printed in a sane way.  They simply print as "value.3 *
   value.5", etc.  

   Expression nodes have value handles associated with them as a
   cache.  Otherwise, we'd have to look them up again in the hash
   table This makes significant difference (factor of two or more) on
   some test cases.  They can be thrown away after the Constants have
   value handles associated with them so that they aren't special
   cased everywhere, and for consistency sake. This may be changed
   depending on memory usage vs code maintenance tradeoff.  */

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

   Sets are represented as doubly linked lists kept in topological
   order, with an optional supporting bitmap of values present in the
   set.  The sets represent values, and the elements can be constants,
   values, or expressions.  The elements can appear in different sets,
   but each element can only appear once in each set.

   Since each node in the set represents a value, we also want to be
   able to map expression, set pairs to something that tells us
   whether the value is present is a set.  We use a per-set bitmap for
   that.  The value handles also point to a linked list of the
   expressions they represent via a tree annotation.  This is mainly
   useful only for debugging, since we don't do identity lookups.  */


/* A value set element.  Basically a single linked list of
   expressions/constants/values.  */
typedef struct value_set_node
{
  tree expr;
  struct value_set_node *next;
} *value_set_node_t;


/* A value set, which is the head of the linked list, and we also keep
   the tail because we have to append for the topolofical sort.  */
typedef struct value_set
{
  value_set_node_t head;
  value_set_node_t tail;
  size_t length;
  bool indexed;
  bitmap values;
  
} *value_set_t;

/* All of the following sets, except for TMP_GEN, are indexed.
   TMP_GEN is only ever iterated over, we never check what values
   exist in it.  */
typedef struct bb_value_sets
{
  value_set_t exp_gen;
  value_set_t phi_gen;
  value_set_t tmp_gen;
  value_set_t avail_out;
  value_set_t antic_in;
  value_set_t new_sets;
} *bb_value_sets_t;

#define EXP_GEN(BB)	((bb_value_sets_t) ((BB)->aux))->exp_gen
#define PHI_GEN(BB)	((bb_value_sets_t) ((BB)->aux))->phi_gen
#define TMP_GEN(BB)	((bb_value_sets_t) ((BB)->aux))->tmp_gen
#define AVAIL_OUT(BB)	((bb_value_sets_t) ((BB)->aux))->avail_out
#define ANTIC_IN(BB)	((bb_value_sets_t) ((BB)->aux))->antic_in
#define NEW_SETS(BB)	((bb_value_sets_t) ((BB)->aux))->new_sets

static struct
{
  int eliminations;
  int insertions;
  int phis;
} pre_stats;

static tree find_leader (value_set_t, tree);
static void value_insert_into_set (value_set_t, tree);
static void insert_into_set (value_set_t, tree);
static void add_to_value (tree, tree);
static value_set_t set_new  (bool);
static bool is_undefined_value (tree);

/* We can add and remove elements and entries to and from sets
   and hash tables, so we use alloc pools for them.  */

static alloc_pool value_set_pool;
static alloc_pool value_set_node_pool;
static alloc_pool binary_node_pool;
static alloc_pool unary_node_pool;

/* The value table that maps expressions to values.  */
static htab_t value_table;

/* The phi_translate_table caches phi translations for a given
   expression and predecessor.  */
static htab_t phi_translate_table;


/* Map expressions to values.  These are simple pairs of expressions
   and the values they represent.  To find the value represented by
   an expression, we use a hash table where the elements are {e,v}
   pairs, and the expression is the key.  */

typedef struct val_expr_pair_d
{
  tree v, e;
  hashval_t hashcode;
} *val_expr_pair_t;


/* Hash a {v,e} pair.  We really only hash the expression.  */

static hashval_t
val_expr_pair_hash (const void *p)
{
  const val_expr_pair_t ve = (val_expr_pair_t) p;
  return ve->hashcode;
}


/* Are {e2,v2} and {e1,v1} the same?  Again, only the expression
   matters.  */

static int
val_expr_pair_expr_eq (const void *p1, const void *p2)
{
  const val_expr_pair_t ve1 = (val_expr_pair_t) p1;
  const val_expr_pair_t ve2 = (val_expr_pair_t) p2;
  tree e1 = ve1->e;
  tree e2 = ve2->e;
  tree te1;
  tree te2;
  if (e1 == e2)
    return true;

  te1 = TREE_TYPE (e1);
  te2 = TREE_TYPE (e2);
  if (TREE_CODE (e1) == TREE_CODE (e2) 
      && (te1 == te2 || lang_hooks.types_compatible_p (te1, te2))
      && operand_equal_p (e1, e2, 0))
    return true;

  return false;
}


/* Get the value handle of EXPR.  This is the only correct way to get
   the value handle for a "thing".  */

tree
get_value_handle (tree expr)
{
  /* We should never see these.  */
  if (DECL_P (expr))
    abort ();
  else if (TREE_CODE (expr) == SSA_NAME)
    {
      return SSA_NAME_VALUE (expr);
    }
  else if (TREE_CODE_CLASS (TREE_CODE (expr)) == 'c')
    {
      cst_ann_t ann = cst_ann (expr);  
      if (ann)
	return ann->common.value_handle;
      return NULL;
    }
  else if (EXPR_P (expr))
    {
      expr_ann_t ann = expr_ann (expr);
      if (ann)
	return ann->common.value_handle;
      return NULL;
    }
  abort ();
}


/* Set the value handle for E to V */
   
void
set_value_handle (tree e, tree v)
{
  if (DECL_P (e))
    abort ();
  else if (TREE_CODE (e) == SSA_NAME)
    SSA_NAME_VALUE (e) = v;
  else if (TREE_CODE_CLASS (TREE_CODE (e)) == 'c')
    get_cst_ann (e)->common.value_handle = v;
  else if (EXPR_P (e))
    get_expr_ann (e)->common.value_handle = v;
}

/* A three tuple {e, pred, v} used to cache phi translations in the
   phi_translate_table.  */

typedef struct expr_pred_trans_d
{
  tree e;
  basic_block pred;
  tree v;
  hashval_t hashcode;
} *expr_pred_trans_t;

/* Return the hash value for a phi translation table entry.  */

static hashval_t
expr_pred_trans_hash (const void *p)
{
  const expr_pred_trans_t ve = (expr_pred_trans_t) p;
  return ve->hashcode;
}

/* Return true if two phi translation table entries are the same.  */

static int
expr_pred_trans_eq (const void *p1, const void *p2)
{
  const expr_pred_trans_t ve1 = (expr_pred_trans_t) p1;
  const expr_pred_trans_t ve2 = (expr_pred_trans_t) p2;
  tree e1 = ve1->e;
  tree e2 = ve2->e;
  basic_block b1 = ve1->pred;
  basic_block b2 = ve2->pred;
  tree te1;
  tree te2;

  if (b1 != b2)
    return false;

  if (e1 == e2)
    return true;
  
  te1 = TREE_TYPE (e1);
  te2 = TREE_TYPE (e2);

  if (TREE_CODE (e1) == TREE_CODE (e2) 
      && (te1 == te2 || lang_hooks.types_compatible_p (te1, te2))
      && operand_equal_p (e1, e2, 0))
    return true;
  
  return false;
}

/* Search in the phi translation table for the translation of E in
   PRED. Return the translated value, if found, NULL otherwise.  */

static inline tree
phi_trans_lookup (tree e, basic_block pred)
{
  void **slot;
  struct expr_pred_trans_d ugly;
  ugly.e = e;
  ugly.pred = pred;
  ugly.hashcode = iterative_hash_expr (e, (unsigned long) pred);
  slot = htab_find_slot_with_hash (phi_translate_table, &ugly, ugly.hashcode,
				   NO_INSERT);
  if (!slot)
    return NULL;
  else
    return ((expr_pred_trans_t) *slot)->v;
}


/* Add the tuple mapping {e, pred}->v to the phi translation table.  */

static inline void
phi_trans_add (tree e, tree v, basic_block pred)
{
  void **slot;
  expr_pred_trans_t new_pair = xmalloc (sizeof (*new_pair));
  new_pair->e = e;
  new_pair->pred = pred;
  new_pair->v = v;
  new_pair->hashcode = iterative_hash_expr (e, (unsigned long) pred);
  slot = htab_find_slot_with_hash (phi_translate_table, new_pair,
				   new_pair->hashcode, INSERT);
  if (*slot)
    free (*slot);
  *slot = (void *) new_pair;
}

/* Search in TABLE for an existing instance of expression E,
   and return its value, or NULL if none has been set.  */

static inline tree
lookup (htab_t table, tree e)
{
  void **slot;
  struct val_expr_pair_d ugly = {NULL, NULL, 0};
  ugly.e = e;
  ugly.hashcode = iterative_hash_expr (e,0); 
  slot = htab_find_slot_with_hash (table, &ugly, ugly.hashcode, NO_INSERT);
  if (!slot)
    return NULL_TREE;
  else
    return ((val_expr_pair_t) *slot)->v;
}

/* Add E to the expression set of V.  */

static inline void
add_to_value (tree v, tree e)
{
#if DEBUG_VALUE_EXPRESSIONS
  var_ann_t va = var_ann (v);
#endif
  /* For values representing numerical constants, we mark
     TREE_CONSTANT as true and set the tree chain to the actual
     constant.  This is because unlike values involving expressions,
     which are only available to use where the expressions are live, a
     constant can be remade anywhere, and thus, is available
     everywhere.  */
  if (TREE_CODE_CLASS (TREE_CODE (e)) == 'c')
    {
      TREE_CONSTANT (v) = true;
      TREE_CHAIN (v) = e;
    }
  else if (is_gimple_min_invariant (e))
    {
      TREE_CONSTANT (v) = true;
      TREE_CHAIN (v) = e;
    }
#if DEBUG_VALUE_EXPRESSIONS
  if (va->expr_set == NULL)
    va->expr_set = set_new (false);
  insert_into_set (va->expr_set, e);
#endif
}

/* Insert E into TABLE with value V, and add E to the value set for V.  */

static inline void
add (htab_t table, tree e, tree v)
{

  void **slot;
  val_expr_pair_t new_pair = xmalloc (sizeof (struct val_expr_pair_d));
  new_pair->e = e;
  new_pair->v = v;
  new_pair->hashcode = iterative_hash_expr (e, 0);
  slot = htab_find_slot_with_hash (table, new_pair, new_pair->hashcode,
				   INSERT);
  if (*slot)
    free (*slot);
  *slot = (void *) new_pair;
  set_value_handle (e, v);

  add_to_value (v, e);
 
}

static int pre_uid;

/* Create a new value handle for EXPR.  */
static tree
create_new_value (tree expr)
{
  tree a = create_tmp_var_raw (TREE_TYPE (expr), "value");
  create_var_ann (a);
  var_ann (a)->uid = pre_uid++;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {     
      fprintf (dump_file, "Created value ");
      print_generic_expr (dump_file, a, dump_flags);
      fprintf (dump_file, " for ");
      print_generic_expr (dump_file, expr, dump_flags);
      fprintf (dump_file, "\n");
    }
  return a;
}

/* Like lookup, but adds V as the value for E if E does not have a value.  */
static inline tree
lookup_or_add (htab_t table, tree e)
{
  tree x = lookup (table, e);
  if (x == NULL_TREE)
    {
      tree v;
      v = create_new_value (e);
      add (table, e, v);
      x = v;
    }
  set_value_handle (e, x);
  return x;
}

  
/* Search in the bitmap for SET to see if E exists.  */

static inline bool
value_exists_in_set_bitmap (value_set_t set, tree e)
{
  if (TREE_CODE (e) != VAR_DECL)
    abort ();

  if (!set->values)
    return false;
  return bitmap_bit_p (set->values, get_var_ann (e)->uid);
}

/* Remove E from the bitmap for SET.  */

static void
value_remove_from_set_bitmap (value_set_t set, tree e)
{
  if (TREE_CODE (e) != VAR_DECL)
    abort ();
#ifdef ENABLE_CHECKING
  if (!set->indexed)
    abort ();
#endif
  if (!set->values)
    return;
  bitmap_clear_bit (set->values, get_var_ann (e)->uid);
}


/* Insert the value number E into the bitmap of values existing in
   SET.  */

static inline void
value_insert_into_set_bitmap (value_set_t set, tree e)
{
  if (TREE_CODE (e) != VAR_DECL)
    abort ();  
#ifdef ENABLE_CHECKING
  if (!set->indexed)
    abort ();
#endif
  if (set->values == NULL)
    {
      set->values = BITMAP_GGC_ALLOC ();
      bitmap_clear (set->values);
    }
  bitmap_set_bit (set->values, get_var_ann (e)->uid);
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


/* Insert EXPR into SET.  */

static void
insert_into_set (value_set_t set, tree expr)
{
  value_set_node_t newnode = pool_alloc (value_set_node_pool);
  tree val = get_value_handle (expr);
  if (DECL_P (expr))
    abort ();
  
  if (val == NULL)
    abort ();

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
  /* This is only referring to the flag above that we set on
     values referring to numerical constants, because we know that we
     are dealing with one of the value handles we created.  */
  if (TREE_CONSTANT (val))
    return true;
  
  if (set->length == 0)
    return false;
  
  return value_exists_in_set_bitmap (set, val);
}

/* Replace the leader for the value LOOKFOR in SET with EXPR.  */

static void
set_replace_value (value_set_t set, tree lookfor, tree expr)
{
  value_set_node_t node = set->head;

  /* The lookup is probably more expensive than walking the linked
     list when we have only a small number of nodes.  */
  if (!set_contains_value (set, lookfor))
    return;

  for (node = set->head;
       node;
       node = node->next)
    {
      if (get_value_handle (node->expr) == lookfor)
	{
	  node->expr = expr;
	  return;
	}
    }
}

/* Return true if the set contains expression (not value) EXPR.  */

static bool
set_contains (value_set_t set, tree expr)
{
  value_set_node_t node;
  
  for (node = set->head;
       node;
       node = node->next)
    {
      if (operand_equal_p (node->expr, expr, 0))
	return true;
    }
  return false;
}

/* Subtract set B from set A, and return the new set.  */

static value_set_t
set_subtract (value_set_t a, value_set_t b, bool indexed)
{
  value_set_t ret = set_new (indexed);
  value_set_node_t node;
  for (node = a->head;
       node;
       node = node->next)
    {
      if (!set_contains (b, node->expr))
	insert_into_set (ret, node->expr);
    }
  return ret;
}

/* Return true if two sets are equal. */

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

/* Replace the value for EXPR in SET with EXPR.  */
static void
value_replace_in_set (value_set_t set, tree expr)
{
  tree val = get_value_handle (expr);

  if (set->length == 0)
    return;
  
  set_replace_value (set, val, expr);
}

/* Insert the value for EXPR into SET, if it doesn't exist already.  */

static void
value_insert_into_set (value_set_t set, tree expr)
{
  tree val = get_value_handle (expr);

  /* Constant values exist everywhere.  */
  if (TREE_CONSTANT (val))
    return;

  if (!set_contains_value (set, val))
    insert_into_set (set, expr);
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
  var_ann_t va = var_ann (val);
  if (va && va->expr_set)
    print_value_set (outfile, va->expr_set, 
		     IDENTIFIER_POINTER (DECL_NAME (val)), 0);
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
phi_translate (tree expr, value_set_t set,  basic_block pred,
	       basic_block phiblock)
{
  tree phitrans = NULL;
  tree oldexpr = expr;
  
  if (expr == NULL)
    return NULL;

  /* Phi translations of a given expression don't change,  */
  phitrans = phi_trans_lookup (expr, pred);
  if (phitrans)
    return phitrans;
  
  
  switch (TREE_CODE_CLASS (TREE_CODE (expr)))
    {
    case '2':
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
	    create_expr_ann (newexpr);
	    TREE_OPERAND (newexpr, 0) = newop1 == oldop1 ? oldop1 : get_value_handle (newop1);
	    TREE_OPERAND (newexpr, 1) = newop2 == oldop2 ? oldop2 : get_value_handle (newop2);
	    lookup_or_add (value_table, newexpr);
	    expr = newexpr;
	    phi_trans_add (oldexpr, newexpr, pred);	    
	  }
      }
      break;
    case '1':
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
	    create_expr_ann (newexpr);
	    TREE_OPERAND (newexpr, 0) = get_value_handle (newop1);
	    lookup_or_add (value_table, newexpr);
	    expr = newexpr;
	    phi_trans_add (oldexpr, newexpr, pred);
	  }
      }
      break;
    case 'd':
      abort ();
    case 'x':
      {
	tree phi = NULL;
	int i;
	if (TREE_CODE (expr) != SSA_NAME)
	  abort ();
	if (TREE_CODE (SSA_NAME_DEF_STMT (expr)) == PHI_NODE)
	  phi = SSA_NAME_DEF_STMT (expr);
	else
	  return expr;
	
	for (i = 0; i < PHI_NUM_ARGS (phi); i++)
	  if (PHI_ARG_EDGE (phi, i)->src == pred)
	    {
	      tree val;
	      if (is_undefined_value (PHI_ARG_DEF (phi, i)))
		return NULL;
	      val = lookup_or_add (value_table, PHI_ARG_DEF (phi, i));
	      return PHI_ARG_DEF (phi, i);
	    }
      }
      break;
    }
  return expr;
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

/* Find the leader for a value (IE the name representing that
   value) in a given set, and return it.  Return NULL if no leader is
   found.  */

static tree
find_leader (value_set_t set, tree val)
{
  value_set_node_t node;

  if (val == NULL)
    return NULL;

  if (TREE_CONSTANT (val))
    return TREE_CHAIN (val);

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
    case '2':
      {
	tree op1 = TREE_OPERAND (expr, 0);
	tree op2 = TREE_OPERAND (expr, 1);
	return set_contains_value (set, op1) && set_contains_value (set, op2);
      }
      break;
    case '1':
      {
	tree op1 = TREE_OPERAND (expr, 0);
	return set_contains_value (set, op1);
      }
      break;
    case 'x':
      {
	if (TREE_CODE (expr) == SSA_NAME)
	  return true;
	abort ();
      }
    case 'c':
      abort ();
    }
  return false;
}

/* Clean the set of expressions that are no longer valid in the
   specified set.  This means expressions that are made up of values
   we have no leaders for in the current set, etc.  */

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

/* Compute the ANTIC set for BLOCK.

ANTIC_OUT[BLOCK] = intersection of ANTIC_IN[b] for all succ(BLOCK), if
succs(BLOCK) > 1
ANTIC_OUT[BLOCK] = phi_translate (ANTIC_IN[succ(BLOCK)]) if
succs(BLOCK) == 1

ANTIC_IN[BLOCK] = clean(ANTIC_OUT[BLOCK] U EXP_GEN[BLOCK] -
TMP_GEN[BLOCK])

Iterate until fixpointed.

XXX: It would be nice to either write a set_clear, and use it for
antic_out, or to mark the antic_out set as deleted at the end
of this routine, so that the pool can hand the same memory back out
again for the next antic_out.  */


static bool
compute_antic_aux (basic_block block)
{
  basic_block son;
  edge e;
  bool changed = false;
  value_set_t S, old, ANTIC_OUT;
  value_set_node_t node;
  
  ANTIC_OUT = S = NULL;
  /* If any edges from predecessors are abnormal, antic_in is empty, so
     punt.  Remember that the block has an incoming abnormal edge by
     setting the BB_VISITED flag.  */
  if (! (block->flags & BB_VISITED))
    {
      for (e = block->pred; e; e = e->pred_next)
 	if (e->flags & EDGE_ABNORMAL)
 	  {
 	    block->flags |= BB_VISITED;
 	    break;
 	  }
    }
  if (block->flags & BB_VISITED)
    {
      S = NULL;
      goto visit_sons;
    }
  

  old = set_new (false);
  set_copy (old, ANTIC_IN (block));
  ANTIC_OUT = set_new (true);

  /* If the block has no successors, ANTIC_OUT is empty, because it is
     the exit block.  */
  if (block->succ == NULL);

  /* If we have one successor, we could have some phi nodes to
     translate through.  */
  else if (block->succ->succ_next == NULL)
    {
      phi_translate_set (ANTIC_OUT, ANTIC_IN(block->succ->dest),
			 block, block->succ->dest);
    }
  /* If we have multiple successors, we take the intersection of all of
     them.  */
  else
    {
      varray_type worklist;
      edge e;
      size_t i;
      basic_block bprime, first;

      VARRAY_BB_INIT (worklist, 1, "succ");
      e = block->succ;
      while (e)
	{
	  VARRAY_PUSH_BB (worklist, e->dest);
	  e = e->succ_next;
	}
      first = VARRAY_BB (worklist, 0);
      set_copy (ANTIC_OUT, ANTIC_IN (first));

      for (i = 1; i < VARRAY_ACTIVE_SIZE (worklist); i++)
	{
	  bprime = VARRAY_BB (worklist, i);
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
      VARRAY_CLEAR (worklist);
    }

  /* Generate ANTIC_OUT - TMP_GEN */
  S = set_subtract (ANTIC_OUT, TMP_GEN (block), false);

  /* Start ANTIC_IN with EXP_GEN - TMP_GEN */
  ANTIC_IN (block) = set_subtract (EXP_GEN (block),TMP_GEN (block), true);
  
  /* Then union in the ANTIC_OUT - TMP_GEN values, to get ANTIC_OUT U
     EXP_GEN - TMP_GEN */
  for (node = S->head;
       node;
       node = node->next)
    {
      value_insert_into_set (ANTIC_IN (block), node->expr);
    }
  clean (ANTIC_IN (block));

  if (!set_equal (old, ANTIC_IN (block)))
    changed = true;

 visit_sons:
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
      changed |= compute_antic_aux (son);
    }
  return changed;
}

/* Compute ANTIC sets.  */

static void
compute_antic (void)
{
  bool changed = true;
  basic_block bb;
  int num_iterations = 0;
  FOR_ALL_BB (bb)
    {
      ANTIC_IN (bb) = set_new (true);
      bb->flags &= ~BB_VISITED;
    }

  while (changed)
    {
      num_iterations++;
      changed = false;
      changed = compute_antic_aux (EXIT_BLOCK_PTR);
    }
  if (num_iterations > 2 && dump_file && (dump_flags & TDF_STATS))
    fprintf (dump_file, "compute_antic required %d iterations\n", num_iterations);
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
      value_set_node_t e;
      basic_block dom;
      dom = get_immediate_dominator (CDI_DOMINATORS, block);
      if (dom)
	{
	  e = NEW_SETS (dom)->head;
	  while (e)
	    {
	      insert_into_set (NEW_SETS (block), e->expr);
	      value_replace_in_set (AVAIL_OUT (block), e->expr);
	      e = e->next;
	    }
	  if (block->pred->pred_next)
	    {
	      value_set_node_t node;
	      for (node = ANTIC_IN (block)->head;
		   node;
		   node = node->next)
		{
		  if (TREE_CODE_CLASS (TREE_CODE (node->expr)) == '2'
		      || TREE_CODE_CLASS (TREE_CODE (node->expr)) == '1')
		    {
		      tree *avail;
		      tree val;
		      bool by_some = false;
		      bool all_same = true;
		      tree first_s = NULL;
		      edge pred;
		      basic_block bprime;
		      tree eprime;
		      val = get_value_handle (node->expr);
		      if (set_contains_value (PHI_GEN (block), val))
			continue; 
		      if (set_contains_value (AVAIL_OUT (dom), val))
			{
			  if (dump_file && (dump_flags & TDF_DETAILS))
			    fprintf (dump_file, "Found fully redundant value\n");
			  continue;
			}
		    
		    
		       avail = xcalloc (last_basic_block, sizeof (tree));
		      for (pred = block->pred;
			   pred;
			   pred = pred->pred_next)
			{
			  tree vprime;
			  tree edoubleprime;
			  bprime = pred->src;
			  eprime = phi_translate (node->expr,
						  ANTIC_IN (block),
						  bprime, block);
			  if (eprime == NULL)
			    continue;

			  vprime = get_value_handle (eprime);
			  if (!vprime)
			    abort ();			  
			  edoubleprime = find_leader (AVAIL_OUT (bprime),
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
			      else if (first_s != edoubleprime)
				all_same = false;
			      if (first_s != edoubleprime 
				  && operand_equal_p (first_s, edoubleprime, 0))
				abort ();
			    }
			}

		      if (!all_same && by_some)
			{
			  tree temp;
			  tree type = TREE_TYPE (avail[block->pred->src->index]);
			  tree v;

			  if (dump_file && (dump_flags & TDF_DETAILS))
			    {
			      fprintf (dump_file, "Found partial redundancy for expression ");
			      print_generic_expr (dump_file, node->expr, 0);
			      fprintf (dump_file, "\n");
			    }

			  /* Make the necessary insertions. */
			  for (pred = block->pred;
			       pred;
			       pred = pred->pred_next)
			    {
			      bprime = pred->src;
			      eprime = avail[bprime->index];
			      if (TREE_CODE_CLASS (TREE_CODE (eprime)) == '2')
				{
				  tree s1, s2;
				  tree newexpr;
				  s1 = find_leader (AVAIL_OUT (bprime),
						    TREE_OPERAND (eprime, 0));
				  /* Depending on the order we process
				     DOM branches in, the value may
				     not have propagated to all the
				     dom children yet during this
				     iteration.  In this case, the
				     value will always be in the
				     NEW_SETS for *our* dominator */
				  if (!s1)
				    s1 = find_leader (NEW_SETS (dom),
						      TREE_OPERAND (eprime, 0));
				  if (!s1)
				    abort ();
				  
				  s2 = find_leader (AVAIL_OUT (bprime),
						    TREE_OPERAND (eprime, 1));
				  if (!s2)
				    s2 = find_leader (NEW_SETS (dom),
						      TREE_OPERAND (eprime, 1));
				  if (!s2)
				    abort ();
				  
				  temp = create_tmp_var (TREE_TYPE (eprime),
							 "pretmp");
				  add_referenced_tmp_var (temp);
				  newexpr = build (TREE_CODE (eprime),
						   TREE_TYPE (eprime),
						   s1, s2);
				  newexpr = build (MODIFY_EXPR, 
						   TREE_TYPE (eprime),
						   temp, newexpr);
				  temp = make_ssa_name (temp, newexpr);
				  TREE_OPERAND (newexpr, 0) = temp;
				  bsi_insert_on_edge (pred, newexpr);
				  bsi_commit_edge_inserts (NULL);
				  
				  if (dump_file && (dump_flags & TDF_DETAILS))
				    {				    
				      fprintf (dump_file, "Inserted ");
				      print_generic_expr (dump_file, newexpr, 0);
				      fprintf (dump_file, " in predecessor %d\n", pred->src->index);
				    }
				  pre_stats.insertions++;
				  v = lookup_or_add (value_table, eprime);
				  add (value_table, temp, v);
				  insert_into_set (NEW_SETS (bprime), temp);
				  value_insert_into_set (AVAIL_OUT (bprime), 
							 temp);
				  avail[bprime->index] = temp;
				}
			      else if (TREE_CODE_CLASS (TREE_CODE (eprime)) == '1')
				{
				  tree s1;
				  tree newexpr;
				  s1 = find_leader (AVAIL_OUT (bprime),
						    TREE_OPERAND (eprime, 0));
				  /* Depending on the order we process
				     DOM branches in, the value may not have
				     propagated to all the dom
				     children yet in the current
				     iteration, but it will be in
				     NEW_SETS if it is not yet
				     propagated.  */
				     
				  if (!s1)
				    s1 = find_leader (NEW_SETS (dom),
						      TREE_OPERAND (eprime, 0));
				  if (!s1)
				    abort ();
				  
				  temp = create_tmp_var (TREE_TYPE (eprime),
							 "pretmp");
				  add_referenced_tmp_var (temp);
				  newexpr = build (TREE_CODE (eprime),
						   TREE_TYPE (eprime),
						   s1);
				  newexpr = build (MODIFY_EXPR, 
						   TREE_TYPE (eprime),
						   temp, newexpr);
				  temp = make_ssa_name (temp, newexpr);
				  TREE_OPERAND (newexpr, 0) = temp;
				  bsi_insert_on_edge (pred, newexpr);
				  bsi_commit_edge_inserts (NULL);
				  
				  if (dump_file && (dump_flags & TDF_DETAILS))
				    {				    
				      fprintf (dump_file, "Inserted ");
				      print_generic_expr (dump_file, newexpr, 0);
				      fprintf (dump_file, " in predecessor %d\n", pred->src->index);
				    }
				  pre_stats.insertions++;
				  v = lookup_or_add (value_table, eprime);
				  add (value_table, temp, v);
				  insert_into_set (NEW_SETS (bprime), temp);
				  value_insert_into_set (AVAIL_OUT (bprime), 
							 temp);
				  avail[bprime->index] = temp;
				}
			    }	        
			  /* Now build a phi for the new variable.  */
			  temp = create_tmp_var (type, "prephitmp");
			  add_referenced_tmp_var (temp);
			  temp = create_phi_node (temp, block);
			  add (value_table, PHI_RESULT (temp), val);

#if 0
			  if (!set_contains_value (AVAIL_OUT (block), val))
			    insert_into_set (AVAIL_OUT (block), 
					     PHI_RESULT (temp));
			  else
#endif
			    value_replace_in_set (AVAIL_OUT (block), 
						 PHI_RESULT (temp));
			  for (pred = block->pred;
			       pred;
			       pred = pred->pred_next)
			    {
			      add_phi_arg (&temp, avail[pred->src->index],
					   pred);
			    }
			  if (dump_file && (dump_flags & TDF_DETAILS))
			    {
			      fprintf (dump_file, "Created phi ");
			      print_generic_expr (dump_file, temp, 0);
			      fprintf (dump_file, " in block %d\n", block->index);
			    }
			  pre_stats.phis++;
			  new_stuff = true;
			  insert_into_set (NEW_SETS (block),
					   PHI_RESULT (temp));
			  insert_into_set (PHI_GEN (block),
					   PHI_RESULT (temp));
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
    NEW_SETS (bb) = set_new (true);
  
  while (new_stuff)
    {
      num_iterations++;
      new_stuff = false;
      new_stuff = insert_aux (ENTRY_BLOCK_PTR);
    }
  if (num_iterations > 2 && dump_file && (dump_flags & TDF_STATS))
    fprintf (dump_file, "insert required %d iterations\n", num_iterations);
}

/* Return true if EXPR has no defining statement in this procedure,
   *AND* isn't a live-on-entry parameter.  */
static bool
is_undefined_value (tree expr)
{  
  
#ifdef ENABLE_CHECKING
  /* We should never be handed DECL's  */
  if (DECL_P (expr))
    abort ();
#endif
  if (TREE_CODE (expr) == SSA_NAME)
    {
      /* XXX: Is this the correct test?  */
      if (TREE_CODE (SSA_NAME_VAR (expr)) == PARM_DECL)
	return false;
      if (IS_EMPTY_STMT (SSA_NAME_DEF_STMT (expr)))
	return true;
    }
  return false;
}

/* Compute the AVAIL set for BLOCK.
   This function performs value numbering of the statements in BLOCK. 
   The AVAIL sets are built from information we glean while doing this
   value numbering, since the AVAIL sets contain only entry per
   value.

   
   AVAIL_IN[BLOCK] = AVAIL_OUT[dom(BLOCK)].
   AVAIL_OUT[BLOCK] = AVAIL_IN[BLOCK] U PHI_GEN[BLOCK] U
   TMP_GEN[BLOCK].
*/

static void
compute_avail (basic_block block)
{
  basic_block son;
  
  /* For arguments with default definitions, we pretend they are
     defined in the entry block.  */
  if (block == ENTRY_BLOCK_PTR)
    {
      tree param;
      for (param = DECL_ARGUMENTS (current_function_decl);
	   param;
	   param = TREE_CHAIN (param))
	{
	  if (default_def (param) != NULL)
	    {
	      tree val;
	      tree def = default_def (param);
	      val = lookup_or_add (value_table, def);
	      insert_into_set (TMP_GEN (block), def);
	      value_insert_into_set (AVAIL_OUT (block), def);
	    }
	}
    }
  else if (block)
    {
      block_stmt_iterator bsi;
      tree stmt, phi;
      basic_block dom;

      dom = get_immediate_dominator (CDI_DOMINATORS, block);
      if (dom)
	set_copy (AVAIL_OUT (block), AVAIL_OUT (dom));
      for (phi = phi_nodes (block); phi; phi = TREE_CHAIN (phi))
	{
	  /* Ignore virtual PHIs until we can do PRE on expressions
	     with virtual operands.  */
	  if (!is_gimple_reg (SSA_NAME_VAR (PHI_RESULT (phi))))
	    continue;

	  lookup_or_add (value_table, PHI_RESULT (phi));
	  value_insert_into_set (AVAIL_OUT (block), PHI_RESULT (phi));
	  insert_into_set (PHI_GEN (block), PHI_RESULT (phi));
	}

      for (bsi = bsi_start (block); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  tree op0, op1;
	  stmt = bsi_stmt (bsi);
	  get_stmt_operands (stmt);
	  
	  if (NUM_VUSES (STMT_VUSE_OPS (stmt))
	      || NUM_V_MUST_DEFS (STMT_V_MUST_DEF_OPS (stmt))
	      || NUM_V_MAY_DEFS (STMT_V_MAY_DEF_OPS (stmt))
	      || stmt_ann (stmt)->has_volatile_ops)
	    {
	      size_t j;
	      for (j = 0; j < NUM_DEFS (STMT_DEF_OPS (stmt)); j++)
		{
		  tree def = DEF_OP (STMT_DEF_OPS (stmt), j);
		  lookup_or_add (value_table, def);
		  insert_into_set (TMP_GEN (block), def);
		  value_insert_into_set (AVAIL_OUT (block), def);
		}
	      continue;
	    }
	  else if (TREE_CODE (stmt) == RETURN_EXPR
		   && TREE_OPERAND (stmt, 0)
		   && TREE_CODE (TREE_OPERAND (stmt, 0)) == MODIFY_EXPR)
	    stmt = TREE_OPERAND (stmt, 0);
	  
	  if (TREE_CODE (stmt) == MODIFY_EXPR)
	    {
	      op0 = TREE_OPERAND (stmt, 0);
	      if (TREE_CODE (op0) != SSA_NAME)
		continue;
	      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op0))
		continue;
	      op1 = TREE_OPERAND (stmt, 1);
	      if (TREE_CODE_CLASS (TREE_CODE (op1)) == 'c')
		{
		  add (value_table, op0, lookup_or_add (value_table, op1));
		  insert_into_set (TMP_GEN (block), op0);
		  value_insert_into_set (AVAIL_OUT (block), op0);
		}
	      else if (TREE_CODE_CLASS (TREE_CODE (op1)) == '2')
		{
		  tree bop1, bop2;
		  tree val, val1, val2;
		  tree newt;
		  bop1 = TREE_OPERAND (op1, 0);
		  bop2 = TREE_OPERAND (op1, 1);
		  val1 = lookup_or_add (value_table, bop1);
		  val2 = lookup_or_add (value_table, bop2);
 
		  newt = pool_alloc (binary_node_pool);
		  memcpy (newt, op1, tree_size (op1));
		  TREE_OPERAND (newt, 0) = val1;
		  TREE_OPERAND (newt, 1) = val2;
		  val = lookup_or_add (value_table, newt);
		  add (value_table, op0, val);
		  if (!is_undefined_value (bop1))
		    value_insert_into_set (EXP_GEN (block), bop1);
		  if (!is_undefined_value (bop2))
		    value_insert_into_set (EXP_GEN (block), bop2);
		  value_insert_into_set (EXP_GEN (block), newt);
		  insert_into_set (TMP_GEN (block), op0);
		  value_insert_into_set (AVAIL_OUT (block), op0);  
		}
	      else if (TREE_CODE_CLASS (TREE_CODE (op1)) == '1')
		{
		  tree uop;
		  tree val, val1;
		  tree newt;
		  uop = TREE_OPERAND (op1, 0);
		  val1 = lookup_or_add (value_table, uop);
		  newt = pool_alloc (unary_node_pool);
		  memcpy (newt, op1, tree_size (op1));
		  TREE_OPERAND (newt, 0) = val1;
		  val = lookup_or_add (value_table, newt);
		  add (value_table, op0, val);
		  if (!is_undefined_value (uop))
		    value_insert_into_set (EXP_GEN (block), uop);
		  value_insert_into_set (EXP_GEN (block), newt);
		  insert_into_set (TMP_GEN (block), op0);
		  value_insert_into_set (AVAIL_OUT (block), op0);
		}
	      else if (TREE_CODE (op1) == SSA_NAME)
		{
		  tree val = lookup_or_add (value_table, op1);
		  add (value_table, op0, val);
		  if (!is_undefined_value (op1))
		    value_insert_into_set (EXP_GEN (block), op1);
		  insert_into_set (TMP_GEN (block), op0);
		  value_insert_into_set (AVAIL_OUT (block), op0);
		}
	      else
		{
		  size_t j;
		  for (j = 0; j < NUM_DEFS (STMT_DEF_OPS (stmt)); j++)
		    {
		      tree def = DEF_OP (STMT_DEF_OPS (stmt), j);
		      lookup_or_add (value_table, def);
		      insert_into_set (TMP_GEN (block), def);
		      value_insert_into_set (AVAIL_OUT (block), def);
		      value_insert_into_set (AVAIL_OUT (block), op0);
		    }
		}
	    }
	  else
	    {
	      size_t j;
	      for (j = 0; j < NUM_DEFS (STMT_DEF_OPS (stmt)); j++)
		{
		  tree def = DEF_OP (STMT_DEF_OPS (stmt), j);
		  lookup_or_add (value_table, def);
		  insert_into_set (TMP_GEN (block), def);
		  value_insert_into_set (AVAIL_OUT (block), def);
		}
	    }
	}
    }
  for (son = first_dom_son (CDI_DOMINATORS, block);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    compute_avail (son);

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

          if (NUM_VUSES (STMT_VUSE_OPS (stmt))
              || NUM_V_MUST_DEFS (STMT_V_MUST_DEF_OPS (stmt))
	      || NUM_V_MAY_DEFS (STMT_V_MAY_DEF_OPS (stmt))
	      || stmt_ann (stmt)->has_volatile_ops)
            continue;
          /* Lookup the RHS of the expression, see if we have an
	     available computation for it. If so, replace the RHS with
	     the available computation.  */
	  if (TREE_CODE (stmt) == MODIFY_EXPR)
            {
              tree t = TREE_OPERAND (stmt, 0);
              tree expr = TREE_OPERAND (stmt, 1);
              tree sprime;
	      /* There is no point in eliminating NOP_EXPR, it isn't
		 supposed to generate any code.  */
	      if (TREE_CODE (expr) == NOP_EXPR
		  || (TREE_CODE_CLASS (TREE_CODE (expr)) != '2' 
		   && TREE_CODE_CLASS (TREE_CODE (expr)) != '1'))
		continue;
	      sprime = find_leader (AVAIL_OUT (b),
				    lookup (value_table, t));
              if (sprime 
		  && sprime != t 
		  && may_propagate_copy (sprime, TREE_OPERAND (stmt, 1)))
                {
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Replaced ");
		      print_generic_expr (dump_file, expr, 0);
		      fprintf (dump_file, " with ");
		      print_generic_expr (dump_file, sprime, 0);
		      fprintf (dump_file, " in ");
		      print_generic_stmt (dump_file, stmt, 0);
		    }
		  pre_stats.eliminations++;
                  propagate_value (&TREE_OPERAND (stmt, 1), sprime);
                  modify_stmt (stmt);
                }
            }

        }
    }
}

/* Main entry point to the SSA-PRE pass.

   PHASE indicates which dump file from the DUMP_FILES array to use when
   dumping debugging information.  */

static void
execute_pre (void)
{
  size_t tsize;
  basic_block bb;
  pre_uid = num_referenced_vars;
  memset (&pre_stats, 0, sizeof (pre_stats));
  FOR_ALL_BB (bb)
    {
      bb->aux = xcalloc (1, sizeof (struct bb_value_sets));
    }
  phi_translate_table = htab_create (511, expr_pred_trans_hash,
				     expr_pred_trans_eq,
				     free);
  value_table = htab_create (511, val_expr_pair_hash,
			     val_expr_pair_expr_eq, free);
  value_set_pool = create_alloc_pool ("Value sets",
				      sizeof (struct value_set), 30);
  value_set_node_pool = create_alloc_pool ("Value set nodes",
				       sizeof (struct value_set_node), 30);
  calculate_dominance_info (CDI_POST_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);
  tsize = tree_size (build (PLUS_EXPR, void_type_node, NULL_TREE,
			    NULL_TREE));
  binary_node_pool = create_alloc_pool ("Binary tree nodes", tsize, 30);
  tsize = tree_size (build1 (NEGATE_EXPR, void_type_node, NULL_TREE));
  unary_node_pool = create_alloc_pool ("Unary tree nodes", tsize, 30);

  FOR_ALL_BB (bb)
    {
      EXP_GEN (bb) = set_new (true);
      PHI_GEN (bb) = set_new (true);
      TMP_GEN (bb) = set_new (false);
      AVAIL_OUT (bb) = set_new (true);
    }

  compute_avail (ENTRY_BLOCK_PTR);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      FOR_ALL_BB (bb)
	{
	  print_value_set (dump_file, EXP_GEN (bb), "exp_gen", bb->index);
	  print_value_set (dump_file, TMP_GEN (bb), "tmp_gen", bb->index);
	  print_value_set (dump_file, AVAIL_OUT (bb), "avail_out", bb->index);
	}
    }

  /* Insert can get quite slow on an incredibly large number of basic
     blocks due to some quadratic behavior.  Until this behavior is
     fixed, don't run it when he have an incredibly large number of
     bb's.  If we aren't going to run insert, there is no point in
     computing ANTIC, either, even though it's plenty fast.  */
 
  if (n_basic_blocks < 4000)
    {
      compute_antic ();
      
      insert ();
    }
  eliminate ();
  
  if (dump_file && (dump_flags & TDF_STATS))
    {
      fprintf (dump_file, "Insertions:%d\n", pre_stats.insertions);
      fprintf (dump_file, "New PHIs:%d\n", pre_stats.phis);
      fprintf (dump_file, "Eliminated:%d\n", pre_stats.eliminations);
    }

  free_alloc_pool (value_set_pool);
  free_alloc_pool (value_set_node_pool);
  free_alloc_pool (binary_node_pool);
  free_alloc_pool (unary_node_pool);
  htab_delete (value_table);
  htab_delete (phi_translate_table);
  
  FOR_ALL_BB (bb)
    {
      free (bb->aux);
      bb->aux = NULL;
    }
  free_dominance_info (CDI_POST_DOMINATORS);
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
  execute_pre,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_PRE,				/* tv_id */
  PROP_no_crit_edges | PROP_cfg | PROP_ssa,/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_ggc_collect | TODO_verify_ssa /* todo_flags_finish */
};
