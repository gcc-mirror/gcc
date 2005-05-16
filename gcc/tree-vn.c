/* Value Numbering routines for tree expressions.
   Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dan@dberlin.org>, Steven Bosscher
   <stevenb@suse.de> and Diego Novillo <dnovillo@redhat.com>

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
#include "ggc.h"
#include "tree.h"
#include "tree-flow.h"
#include "hashtab.h"
#include "langhooks.h"
#include "tree-pass.h"
#include "tree-dump.h"
#include "diagnostic.h"

/* The value table that maps expressions to values.  */
static htab_t value_table;

/* Map expressions to values.  These are simple pairs of expressions
   and the values they represent.  To find the value represented by
   an expression, we use a hash table where the elements are {e,v}
   pairs, and the expression is the key.  */
typedef struct val_expr_pair_d
{
  /* Value handle.  */
  tree v;

  /* Associated expression.  */
  tree e;

  /* for comparing Virtual uses in E.  */
  tree stmt;

  /* E's hash value.  */
  hashval_t hashcode;
} *val_expr_pair_t;

static void set_value_handle (tree e, tree v);


/* Create and return a new value handle node of type TYPE.  */

static tree
make_value_handle (tree type)
{
  static unsigned int id = 0;
  tree vh;

  vh = build0 (VALUE_HANDLE, type);
  VALUE_HANDLE_ID (vh) = id++;
  return vh;
}


/* Given an expression EXPR, compute a hash value number using the
   code of the expression, its real operands and virtual operands (if
   any).
   
   VAL can be used to iterate by passing previous value numbers (it is
   used by iterative_hash_expr).

   STMT is the stmt associated with EXPR for comparing virtual operands.  */

hashval_t
vn_compute (tree expr, hashval_t val, tree stmt)
{
  ssa_op_iter iter;
  tree vuse;

  /* EXPR must not be a statement.  We are only interested in value
     numbering expressions on the RHS of assignments.  */
  gcc_assert (expr);
  gcc_assert (!expr->common.ann
	      || expr->common.ann->common.type != STMT_ANN);

  val = iterative_hash_expr (expr, val);

  /* If the expression has virtual uses, incorporate them into the
     hash value computed for EXPR.  */
  if (stmt)
    FOR_EACH_SSA_TREE_OPERAND (vuse, stmt, iter, SSA_OP_VUSE)
      val = iterative_hash_expr (vuse,  val);

  return val;
}


/* Compare two expressions E1 and E2 and return true if they are
   equal.  */

bool
expressions_equal_p (tree e1, tree e2)
{
  tree te1, te2;
  
  if (e1 == e2)
    return true;

  te1 = TREE_TYPE (e1);
  te2 = TREE_TYPE (e2);

  if (TREE_CODE (e1) == TREE_LIST && TREE_CODE (e2) == TREE_LIST)
    {
      tree lop1 = e1;
      tree lop2 = e2;
      for (lop1 = e1, lop2 = e2;
	   lop1 || lop2;
	   lop1 = TREE_CHAIN (lop1), lop2 = TREE_CHAIN (lop2))
	{
	  if (!lop1 || !lop2)
	    return false;
	  if (!expressions_equal_p (TREE_VALUE (lop1), TREE_VALUE (lop2)))
	    return false;
	}
      return true;

    }
  else if (TREE_CODE (e1) == TREE_CODE (e2) 
	   && (te1 == te2 || lang_hooks.types_compatible_p (te1, te2))
	   && operand_equal_p (e1, e2, OEP_PURE_SAME))
    return true;

  return false;
}


/* Hash a {v,e} pair that is pointed to by P.
   The hashcode is cached in the val_expr_pair, so we just return
   that.  */

static hashval_t
val_expr_pair_hash (const void *p)
{
  const val_expr_pair_t ve = (val_expr_pair_t) p;
  return ve->hashcode;
}


/* Given two val_expr_pair_t's, return true if they represent the same
   expression, false otherwise.
   P1 and P2 should point to the val_expr_pair_t's to be compared.  */

static int
val_expr_pair_expr_eq (const void *p1, const void *p2)
{
  bool ret;
  const val_expr_pair_t ve1 = (val_expr_pair_t) p1;
  const val_expr_pair_t ve2 = (val_expr_pair_t) p2;

  if (! expressions_equal_p (ve1->e, ve2->e))
    return false;

  ret = compare_ssa_operands_equal (ve1->stmt, ve2->stmt, SSA_OP_VUSE);
  return ret;
}


/* Set the value handle for expression E to value V.  */
   
static void
set_value_handle (tree e, tree v)
{
  if (TREE_CODE (e) == SSA_NAME)
    SSA_NAME_VALUE (e) = v;
  else if (EXPR_P (e) || DECL_P (e) || TREE_CODE (e) == TREE_LIST)
    get_tree_ann (e)->common.value_handle = v;
  else
    /* Do nothing.  Constants are their own value handles.  */
    gcc_assert (is_gimple_min_invariant (e));
}


/* Insert EXPR into VALUE_TABLE with value VAL, and add expression
   EXPR to the value set for value VAL.  STMT represents the stmt
   associated with EXPR.  It is used when computing a hash value for EXPR.  */

void
vn_add (tree expr, tree val, tree stmt)
{
  void **slot;
  val_expr_pair_t new_pair;
  
  new_pair = xmalloc (sizeof (struct val_expr_pair_d));
  new_pair->e = expr;
  new_pair->v = val;
  new_pair->stmt = stmt;
  new_pair->hashcode = vn_compute (expr, 0, stmt);
  slot = htab_find_slot_with_hash (value_table, new_pair, new_pair->hashcode,
				   INSERT);
  if (*slot)
    free (*slot);
  *slot = (void *) new_pair;

  set_value_handle (expr, val);
  add_to_value (val, expr);
}


/* Search in VALUE_TABLE for an existing instance of expression EXPR,
   and return its value, or NULL if none has been set.  STMT
   represents the stmt associated with EXPR.  It is used when computing the 
   hash value for EXPR.  */

tree
vn_lookup (tree expr, tree stmt)
{
  void **slot;
  struct val_expr_pair_d vep = {NULL, NULL, NULL, 0};

  /* Constants are their own value.  */
  if (is_gimple_min_invariant (expr))
    return expr;

  vep.e = expr;
  vep.stmt = stmt;
  vep.hashcode = vn_compute (expr, 0, stmt); 
  slot = htab_find_slot_with_hash (value_table, &vep, vep.hashcode, NO_INSERT);
  if (!slot)
    return NULL_TREE;
  else
    return ((val_expr_pair_t) *slot)->v;
}


/* Like vn_lookup, but creates a new value for expression EXPR, if
   EXPR doesn't already have a value.  Return the existing/created
   value for EXPR.  STMT represents the stmt associated with EXPR.  It is used
   when computing the hash value for EXPR.  */

tree
vn_lookup_or_add (tree expr, tree stmt)
{
  tree v = vn_lookup (expr, stmt);
  if (v == NULL_TREE)
    {
      v = make_value_handle (TREE_TYPE (expr));

      if (dump_file && (dump_flags & TDF_DETAILS))
	{     
	  fprintf (dump_file, "Created value ");
	  print_generic_expr (dump_file, v, dump_flags);
	  fprintf (dump_file, " for ");
	  print_generic_expr (dump_file, expr, dump_flags);
	  fprintf (dump_file, "\n");
	}

      vn_add (expr, v, stmt);
    }

  set_value_handle (expr, v);

  return v;
}


/* Get the value handle of EXPR.  This is the only correct way to get
   the value handle for a "thing".  If EXPR does not have a value
   handle associated, it returns NULL_TREE.  
   NB: If EXPR is min_invariant, this function is *required* to return EXPR.  */

tree
get_value_handle (tree expr)
{

  if (is_gimple_min_invariant (expr))
    return expr;

  if (TREE_CODE (expr) == SSA_NAME)
    return SSA_NAME_VALUE (expr);
  else if (EXPR_P (expr) || DECL_P (expr) || TREE_CODE (expr) == TREE_LIST)
    {
      tree_ann_t ann = tree_ann (expr);
      return ((ann) ? ann->common.value_handle : NULL_TREE);
    }
  else
    gcc_unreachable ();
}


/* Initialize data structures used in value numbering.  */

void
vn_init (void)
{
  value_table = htab_create (511, val_expr_pair_hash,
			     val_expr_pair_expr_eq, free);
}


/* Delete data used for value numbering.  */

void
vn_delete (void)
{
  htab_delete (value_table);
  value_table = NULL;
}
