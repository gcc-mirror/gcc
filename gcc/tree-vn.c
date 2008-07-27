/* Value Numbering routines for tree expressions.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2007 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dan@dberlin.org>, Steven Bosscher
   <stevenb@suse.de> and Diego Novillo <dnovillo@redhat.com>

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
#include "tree-flow.h"
#include "hashtab.h"
#include "langhooks.h"
#include "tree-pass.h"
#include "tree-dump.h"
#include "diagnostic.h"
#include "tree-ssa-sccvn.h"

/* Most of this is PRE specific.  The real grunt work is done in
   tree-ssa-sccvn.c.  This is where the lookup and insertion
   functions, etc, can be found */

/* Create and return a new value handle node of type TYPE.  */

tree
make_value_handle (tree type)
{
  static unsigned int id = 0;
  tree vh;

  vh = build0 (VALUE_HANDLE, type);
  VALUE_HANDLE_ID (vh) = id++;
  return vh;
}



/* Compare two expressions E1 and E2 and return true if they are equal.  */

bool
expressions_equal_p (tree e1, tree e2)
{
  tree te1, te2;

  /* The obvious case.  */
  if (e1 == e2)
    return true;

  /* If only one of them is null, they cannot be equal.  */
  if (!e1 || !e2)
    return false;

  /* Recurse on elements of lists.  */
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

  te1 = TREE_TYPE (e1);
  te2 = TREE_TYPE (e2);

  /* Now perform the actual comparison.  */
  if (TREE_CODE (e1) == TREE_CODE (e2)
      && (te1 == te2 || types_compatible_p (te1, te2))
      && operand_equal_p (e1, e2, OEP_PURE_SAME))
    return true;

  return false;
}

/* Set the value handle for expression E to value V.  */

void
set_value_handle (tree e, tree v)
{
  if (TREE_CODE (e) == SSA_NAME)
    SSA_NAME_VALUE (e) = v;
  else if (EXPR_P (e) || DECL_P (e) || TREE_CODE (e) == TREE_LIST
	   || GIMPLE_STMT_P (e)
	   || TREE_CODE (e) == CONSTRUCTOR)
    get_tree_common_ann (e)->value_handle = v;
  else
    /* Do nothing.  Constants are their own value handles.  */
    gcc_assert (is_gimple_min_invariant (e));
}

/* Print out the "Created value <x> for <Y>" statement to the
   dump_file.
   This is factored because both versions of lookup use it, and it
   obscures the real work going on in those functions.  */

static void
print_creation_to_file (tree v, tree expr, VEC (tree, gc) *vuses)
{
  fprintf (dump_file, "Created value ");
  print_generic_expr (dump_file, v, dump_flags);
  fprintf (dump_file, " for ");
  print_generic_expr (dump_file, expr, dump_flags);

  if (vuses && VEC_length (tree, vuses) != 0)
    {
      size_t i;
      tree vuse;

      fprintf (dump_file, " vuses: (");
      for (i = 0; VEC_iterate (tree, vuses, i, vuse); i++)
	{
	  print_generic_expr (dump_file, vuse, dump_flags);
	  if (VEC_length (tree, vuses) - 1 != i)
	    fprintf (dump_file, ",");
	}
      fprintf (dump_file, ")");
    }
  fprintf (dump_file, "\n");
}


/* Sort the VUSE array so that we can do equality comparisons
   quicker on two vuse vecs.  */

void
sort_vuses (VEC (tree,gc) *vuses)
{
  if (VEC_length (tree, vuses) > 1)
    qsort (VEC_address (tree, vuses),
	   VEC_length (tree, vuses),
	   sizeof (tree),
	   operand_build_cmp);
}

/* Sort the VUSE array so that we can do equality comparisons
   quicker on two vuse vecs.  */

void
sort_vuses_heap (VEC (tree,heap) *vuses)
{
  if (VEC_length (tree, vuses) > 1)
    qsort (VEC_address (tree, vuses),
	   VEC_length (tree, vuses),
	   sizeof (tree),
	   operand_build_cmp);
}
/* Insert EXPR into VALUE_TABLE with value VAL, and add expression
   EXPR to the value set for value VAL.  */

void
vn_add (tree expr, tree val)
{
  switch (TREE_CODE_CLASS (TREE_CODE (expr)))
    {
    case tcc_comparison:
    case tcc_binary:
      vn_binary_op_insert (expr, val);
      break;
    case tcc_unary:
      vn_unary_op_insert (expr, val);
      break;
      /* In the case of array-refs of constants, for example, we can
	 end up with no vuses.  */
    case tcc_reference:
      vn_reference_insert (expr, val, NULL);
      break;
      /* The *only* time CALL_EXPR should appear here is
	 when it has no vuses.  */
    case tcc_vl_exp:
    case tcc_exceptional:
    case tcc_expression:
    case tcc_declaration:
      if (TREE_CODE (expr) == CALL_EXPR || DECL_P (expr))
	{
	  vn_reference_insert (expr, val, NULL);
	  break;
	}
      else if (TREE_CODE (expr) == SSA_NAME)
	{
	  SSA_NAME_VALUE (expr) = val;
	  break;
	}
      switch (TREE_CODE (expr))
	{
	case ADDR_EXPR:
	case TRUTH_AND_EXPR:
	case TRUTH_OR_EXPR:
	case TRUTH_XOR_EXPR:
	case TRUTH_NOT_EXPR:
	  vn_unary_op_insert (expr, val);
	  break;
	default:
	  gcc_unreachable ();
	}
      break;
    default:
      gcc_unreachable ();
    }
  set_value_handle (expr, val);
  if (TREE_CODE (val) == VALUE_HANDLE)
    add_to_value (val, expr);
}

/* Insert EXPR into the value numbering tables.  with value VAL, and
   add expression EXPR to the value set for value VAL.  VUSES
   represents the virtual use operands associated with EXPR.  It is
   used when computing a hash value for EXPR.  */

void
vn_add_with_vuses (tree expr, tree val, VEC (tree, gc) *vuses)
{
  if (!vuses)
    {
      vn_add (expr, val);
      return;
    }
  vn_reference_insert (expr, val, vuses);

  set_value_handle (expr, val);
  if (TREE_CODE (val) == VALUE_HANDLE)
    add_to_value (val, expr);
}


/* Lookup EXPR in the value numbering tables and return the result, if
   we have one.  */

tree
vn_lookup (tree expr)
{
  /* Constants are their own value.  */
  if (is_gimple_min_invariant (expr) || TREE_CODE (expr) == FIELD_DECL)
    return expr;

  switch (TREE_CODE_CLASS (TREE_CODE (expr)))
    {
    case tcc_comparison:
    case tcc_binary:
      return vn_binary_op_lookup (expr);
    case tcc_unary:
      return vn_unary_op_lookup (expr);
      break;
      /* In the case of array-refs of constants, for example, we can
	 end up with no vuses.  */
    case tcc_reference:
      return vn_reference_lookup (expr, NULL);
      break;
      /* It is possible to have CALL_EXPR with no vuses for things
	 like "cos", and these will fall into vn_lookup.   */
    case tcc_vl_exp:
    case tcc_exceptional:
    case tcc_expression:
    case tcc_declaration:
      if (TREE_CODE (expr) == CALL_EXPR || DECL_P (expr))
	return vn_reference_lookup (expr, NULL);
      else if (TREE_CODE (expr) == SSA_NAME)
	return SSA_NAME_VALUE (expr);
      switch (TREE_CODE (expr))
	{
	case ADDR_EXPR:
	case TRUTH_AND_EXPR:
	case TRUTH_OR_EXPR:
	case TRUTH_XOR_EXPR:
	case TRUTH_NOT_EXPR:
	  return vn_unary_op_lookup (expr);
	default:
	  gcc_unreachable ();
	}
      break;
    default:
      gcc_unreachable ();
    }
  return NULL;
}

/* Search in the value numbering tables for an existing instance of
   expression EXPR,  and return its value, or NULL if none has been set.  STMT
   represents the stmt associated with EXPR.  It is used when computing the
   hash value for EXPR for reference operations.  */

tree
vn_lookup_with_stmt (tree expr, tree stmt)
{
  if (stmt == NULL)
    return vn_lookup (expr);

  /* Constants are their own value.  */
  if (is_gimple_min_invariant (expr) || TREE_CODE (expr) == FIELD_DECL)
    return expr;

  return vn_lookup_with_vuses (expr, shared_vuses_from_stmt (stmt));
}

/* Search in VALUE_TABLE for an existing instance of expression EXPR,
   and return its value, or NULL if none has been set.  VUSES is the
   list of virtual use operands associated with EXPR.  It is used when
   computing the hash value for EXPR.  */

tree
vn_lookup_with_vuses (tree expr, VEC (tree, gc) *vuses)
{
  if (!vuses || !VEC_length (tree, vuses))
    return vn_lookup (expr);

  if (is_gimple_min_invariant (expr) || TREE_CODE (expr) == FIELD_DECL)
    return expr;

  return vn_reference_lookup (expr, vuses);
}

static tree
create_value_handle_for_expr (tree expr, VEC(tree, gc) *vuses)
{
  tree v;

  v = make_value_handle (TREE_TYPE (expr));

  if (dump_file && (dump_flags & TDF_DETAILS))
    print_creation_to_file (v, expr, vuses);
  return v;
}

/* Like vn_lookup, but creates a new value for the operation if one
   does not exist.  */

tree
vn_lookup_or_add (tree expr)
{
  tree v = vn_lookup (expr);

  if (v == NULL_TREE)
    {
      v = create_value_handle_for_expr (expr, NULL);
      vn_add (expr, v);
    }
  else
    set_value_handle (expr, v);

  return v;
}

/* Like vn_lookup, but handles reference operations as well by using
   STMT to get the set of vuses.  */

tree
vn_lookup_or_add_with_stmt (tree expr, tree stmt)
{
  tree v;
  if (!stmt)
    return vn_lookup_or_add (expr);

  v = vn_lookup_with_stmt (expr, stmt);
  if (v == NULL_TREE)
    {
      VEC (tree, gc) *vuses = copy_vuses_from_stmt (stmt);
      v = create_value_handle_for_expr (expr, vuses);
      vn_add_with_vuses (expr, v, vuses);
    }
  else
    set_value_handle (expr, v);

  return v;
}

/* Like vn_lookup, but creates a new value for expression EXPR, if
   EXPR doesn't already have a value.  Return the existing/created
   value for EXPR.  STMT represents the stmt associated with EXPR.  It is used
   when computing the hash value for EXPR.  */

tree
vn_lookup_or_add_with_vuses (tree expr, VEC (tree, gc) *vuses)
{
  tree v;

  if (!vuses || VEC_length (tree, vuses) == 0)
    return vn_lookup_or_add (expr);

  v = vn_lookup_with_vuses (expr, vuses);
  if (v == NULL_TREE)
    {
      v = create_value_handle_for_expr (expr, vuses);
      vn_add_with_vuses (expr, v, vuses);
    }
  else
    set_value_handle (expr, v);

  return v;
}

