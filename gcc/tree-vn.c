/* Value Numbering routines for tree expressions.
   Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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
  tree v, e;
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


/* Given an expression or statement P, compute a hash value number using the
   code of the expression and its real operands.  */

hashval_t
vn_compute (tree expr, hashval_t val)
{
  val = iterative_hash_expr (expr, val);
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

  if (TREE_CODE (e1) == TREE_CODE (e2) 
      && (te1 == te2 || lang_hooks.types_compatible_p (te1, te2))
      && operand_equal_p (e1, e2, 0))
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
  const val_expr_pair_t ve1 = (val_expr_pair_t) p1;
  const val_expr_pair_t ve2 = (val_expr_pair_t) p2;

  if (expressions_equal_p (ve1->e, ve2->e))
    return true;
  
  return false;
}


/* Set the value handle for expression E to value V */
   
static void
set_value_handle (tree e, tree v)
{
  if (TREE_CODE (e) == SSA_NAME)
    SSA_NAME_VALUE (e) = v;
  else if (EXPR_P (e) || DECL_P (e))
    get_tree_ann (e)->common.value_handle = v;
  else if (TREE_CODE_CLASS (TREE_CODE (e)) == 'c')
    /* Do nothing.  Constants are their own value handles.  */
    ;
  else
    abort ();
}


/* Insert E into VALUE_TABLE with value V, and add expression E to the
   value set for value V.  */

void
vn_add (tree e, tree v)
{
  void **slot;
  val_expr_pair_t new_pair = xmalloc (sizeof (struct val_expr_pair_d));
  new_pair->e = e;
  new_pair->v = v;
  new_pair->hashcode = vn_compute (e, 0);
  slot = htab_find_slot_with_hash (value_table, new_pair, new_pair->hashcode,
				   INSERT);
  if (*slot)
    free (*slot);
  *slot = (void *) new_pair;
  set_value_handle (e, v);

  add_to_value (v, e);
}


/* Search in VALUE_TABLE for an existing instance of expression E, and
   return its value, or NULL if none has been set.  */

tree
vn_lookup (tree e)
{
  void **slot;
  struct val_expr_pair_d vep = {NULL, NULL, 0};

  if (TREE_CODE_CLASS (TREE_CODE (e)) == 'c')
    return e;
  vep.e = e;
  vep.hashcode = vn_compute (e, 0); 
  slot = htab_find_slot_with_hash (value_table, &vep, vep.hashcode, NO_INSERT);
  if (!slot)
    return NULL_TREE;
  else
    return ((val_expr_pair_t) *slot)->v;
}


/* Like vn_lookup, but creates a new value for expression E if E doesn't
   already have a value.  Return the existing/created value for E.  */

tree
vn_lookup_or_add (tree e)
{
  tree x = vn_lookup (e);
  if (x == NULL_TREE)
    {
      tree v = make_value_handle (TREE_TYPE (e));

      if (dump_file && (dump_flags & TDF_DETAILS))
	{     
	  fprintf (dump_file, "Created value ");
	  print_generic_expr (dump_file, v, dump_flags);
	  fprintf (dump_file, " for ");
	  print_generic_expr (dump_file, e, dump_flags);
	  fprintf (dump_file, "\n");
	}

      vn_add (e, v);
      x = v;
    }

  set_value_handle (e, x);

  return x;
}


/* Get the value handle of EXPR.  This is the only correct way to get
   the value handle for a "thing".  If EXPR does not have a value
   handle associated, it generates and returns a new one.  */

tree
get_value_handle (tree expr)
{
  if (TREE_CODE (expr) == SSA_NAME)
    return SSA_NAME_VALUE (expr);
  else if (TREE_CODE_CLASS (TREE_CODE (expr)) == 'c')
    return expr;
  else if (EXPR_P (expr) || DECL_P (expr))
    {
      tree_ann_t ann = tree_ann (expr);
      return ((ann) ? ann->common.value_handle : NULL_TREE);
    }

  abort ();
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
