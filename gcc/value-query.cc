/* Support routines for value queries.
   Copyright (C) 2020-2022 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com> and
   Andrew MacLeod <amacleod@redhat.com>.

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
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "tree-pretty-print.h"
#include "fold-const.h"
#include "value-range-equiv.h"
#include "value-query.h"
#include "alloc-pool.h"
#include "gimple-range.h"
#include "value-range-storage.h"

// value_query default methods.

tree
value_query::value_on_edge (edge, tree expr)
{
  return value_of_expr (expr);
}

tree
value_query::value_of_stmt (gimple *stmt, tree name)
{
  if (!name)
    name = gimple_get_lhs (stmt);

  gcc_checking_assert (!name || name == gimple_get_lhs (stmt));

  if (name)
    return value_of_expr (name);
  return NULL_TREE;
}

// range_query default methods.

bool
range_query::range_on_edge (vrange &r, edge, tree expr)
{
  return range_of_expr (r, expr);
}

bool
range_query::range_of_stmt (vrange &r, gimple *stmt, tree name)
{
  if (!name)
    name = gimple_get_lhs (stmt);

  gcc_checking_assert (!name || name == gimple_get_lhs (stmt));

  if (name)
    return range_of_expr (r, name);
  return false;
}

tree
range_query::value_of_expr (tree expr, gimple *stmt)
{
  tree t;

  if (!Value_Range::supports_type_p (TREE_TYPE (expr)))
    return NULL_TREE;

  Value_Range r (TREE_TYPE (expr));

  if (range_of_expr (r, expr, stmt))
    {
      // A constant used in an unreachable block oftens returns as UNDEFINED.
      // If the result is undefined, check the global value for a constant.
      if (r.undefined_p ())
	range_of_expr (r, expr);
      if (r.singleton_p (&t))
	return t;
    }
  return NULL_TREE;
}

tree
range_query::value_on_edge (edge e, tree expr)
{
  tree t;

  if (!Value_Range::supports_type_p (TREE_TYPE (expr)))
    return NULL_TREE;
  Value_Range r (TREE_TYPE (expr));
  if (range_on_edge (r, e, expr))
    {
      // A constant used in an unreachable block oftens returns as UNDEFINED.
      // If the result is undefined, check the global value for a constant.
      if (r.undefined_p ())
	range_of_expr (r, expr);
      if (r.singleton_p (&t))
	return t;
    }
  return NULL_TREE;

}

tree
range_query::value_of_stmt (gimple *stmt, tree name)
{
  tree t;

  if (!name)
    name = gimple_get_lhs (stmt);

  gcc_checking_assert (!name || name == gimple_get_lhs (stmt));

  if (!name || !Value_Range::supports_type_p (TREE_TYPE (name)))
    return NULL_TREE;
  Value_Range r (TREE_TYPE (name));
  if (range_of_stmt (r, stmt, name) && r.singleton_p (&t))
    return t;
  return NULL_TREE;

}

void
range_query::dump (FILE *)
{
}

// valuation_query support routines for value_range_equiv's.

class equiv_allocator : public object_allocator<value_range_equiv>
{
public:
  equiv_allocator ()
    : object_allocator<value_range_equiv> ("equiv_allocator pool") { }
};

value_range_equiv *
range_query::allocate_value_range_equiv ()
{
  return new (equiv_alloc->allocate ()) value_range_equiv;
}

void
range_query::free_value_range_equiv (value_range_equiv *v)
{
  equiv_alloc->remove (v);
}

const class value_range_equiv *
range_query::get_value_range (const_tree expr, gimple *stmt)
{
  gcc_checking_assert (value_range_equiv::supports_p (TREE_TYPE (expr)));
  int_range_max r;
  if (range_of_expr (r, const_cast<tree> (expr), stmt))
    return new (equiv_alloc->allocate ()) value_range_equiv (r);
  return new (equiv_alloc->allocate ()) value_range_equiv (TREE_TYPE (expr));
}

range_query::range_query ()
{
  equiv_alloc = new equiv_allocator;
  m_oracle = NULL;
}

range_query::~range_query ()
{
  equiv_alloc->release ();
  delete equiv_alloc;
}

// Return a range in R for the tree EXPR.  Return true if a range is
// representable, and UNDEFINED/false if not.

bool
range_query::get_tree_range (vrange &r, tree expr, gimple *stmt)
{
  tree type;
  if (TYPE_P (expr))
    type = expr;
  else
    type = TREE_TYPE (expr);

  if (!Value_Range::supports_type_p (type))
    {
      r.set_undefined ();
      return false;
    }
  if (expr == type)
    {
      r.set_varying (type);
      return true;
    }
  switch (TREE_CODE (expr))
    {
    case INTEGER_CST:
    case REAL_CST:
      if (TREE_OVERFLOW_P (expr))
	expr = drop_tree_overflow (expr);
      r.set (expr, expr);
      return true;

    case SSA_NAME:
      gimple_range_global (r, expr);
      return true;

    case ADDR_EXPR:
      {
	// Handle &var which can show up in phi arguments.
	bool ov;
	if (tree_single_nonzero_warnv_p (expr, &ov))
	  {
	    r.set_nonzero (type);
	    return true;
	  }
	break;
      }

    default:
      break;
    }
  if (BINARY_CLASS_P (expr))
    {
      range_op_handler op (TREE_CODE (expr), type);
      if (op)
	{
	  Value_Range r0 (TREE_TYPE (TREE_OPERAND (expr, 0)));
	  Value_Range r1 (TREE_TYPE (TREE_OPERAND (expr, 1)));
	  range_of_expr (r0, TREE_OPERAND (expr, 0), stmt);
	  range_of_expr (r1, TREE_OPERAND (expr, 1), stmt);
	  op.fold_range (r, type, r0, r1);
	}
      else
	r.set_varying (type);
      return true;
    }
  if (UNARY_CLASS_P (expr))
    {
      range_op_handler op (TREE_CODE (expr), type);
      tree op0_type = TREE_TYPE (TREE_OPERAND (expr, 0));
      if (op && Value_Range::supports_type_p (op0_type))
	{
	  Value_Range r0 (TREE_TYPE (TREE_OPERAND (expr, 0)));
	  Value_Range r1 (type);
	  r1.set_varying (type);
	  range_of_expr (r0, TREE_OPERAND (expr, 0), stmt);
	  op.fold_range (r, type, r0, r1);
	}
      else
	r.set_varying (type);
      return true;
    }
  r.set_varying (type);
  return true;
}

// Return the range for NAME from SSA_NAME_RANGE_INFO.

static inline void
get_ssa_name_range_info (vrange &r, const_tree name)
{
  tree type = TREE_TYPE (name);
  gcc_checking_assert (!POINTER_TYPE_P (type));
  gcc_checking_assert (TREE_CODE (name) == SSA_NAME);

  void *ri = SSA_NAME_RANGE_INFO (name);

  if (ri)
    {
      vrange_storage vstore (NULL);
      vstore.get_vrange (ri, r, TREE_TYPE (name));
    }
  else
    r.set_varying (type);
}

// Return nonnull attribute of pointer NAME from SSA_NAME_PTR_INFO.

static inline bool
get_ssa_name_ptr_info_nonnull (const_tree name)
{
  gcc_assert (POINTER_TYPE_P (TREE_TYPE (name)));
  struct ptr_info_def *pi = SSA_NAME_PTR_INFO (name);
  if (pi == NULL)
    return false;
  /* TODO Now pt->null is conservatively set to true in PTA
     analysis. vrp is the only pass (including ipa-vrp)
     that clears pt.null via set_ptr_nonnull when it knows
     for sure. PTA will preserves the pt.null value set by VRP.

     When PTA analysis is improved, pt.anything, pt.nonlocal
     and pt.escaped may also has to be considered before
     deciding that pointer cannot point to NULL.  */
  return !pi->pt.null;
}

// Update the global range for NAME into the SSA_RANGE_NAME_INFO and
// Return the legacy global range for NAME if it has one, otherwise
// return VARYING.

static void
get_range_global (vrange &r, tree name)
{
  tree type = TREE_TYPE (name);

  if (SSA_NAME_IS_DEFAULT_DEF (name))
    {
      tree sym = SSA_NAME_VAR (name);
      // Adapted from vr_values::get_lattice_entry().
      // Use a range from an SSA_NAME's available range.
      if (TREE_CODE (sym) == PARM_DECL)
	{
	  // Try to use the "nonnull" attribute to create ~[0, 0]
	  // anti-ranges for pointers.  Note that this is only valid with
	  // default definitions of PARM_DECLs.
	  if (POINTER_TYPE_P (type)
	      && ((cfun && nonnull_arg_p (sym))
		  || get_ssa_name_ptr_info_nonnull (name)))
	    r.set_nonzero (type);
	  else if (INTEGRAL_TYPE_P (type))
	    {
	      get_ssa_name_range_info (r, name);
	      if (r.undefined_p ())
		r.set_varying (type);
	    }
	  else
	    r.set_varying (type);
	}
      // If this is a local automatic with no definition, use undefined.
      else if (TREE_CODE (sym) != RESULT_DECL)
	r.set_undefined ();
      else
	r.set_varying (type);
   }
  else if (!POINTER_TYPE_P (type) && SSA_NAME_RANGE_INFO (name))
    {
      get_ssa_name_range_info (r, name);
      if (r.undefined_p ())
	r.set_varying (type);
    }
  else if (POINTER_TYPE_P (type) && SSA_NAME_PTR_INFO (name))
    {
      if (get_ssa_name_ptr_info_nonnull (name))
	r.set_nonzero (type);
      else
	r.set_varying (type);
    }
  else
    r.set_varying (type);
}

// This is where the ranger picks up global info to seed initial
// requests.  It is a slightly restricted version of
// get_range_global() above.
//
// The reason for the difference is that we can always pick the
// default definition of an SSA with no adverse effects, but for other
// SSAs, if we pick things up to early, we may prematurely eliminate
// builtin_unreachables.
//
// Without this restriction, the test in g++.dg/tree-ssa/pr61034.C has
// all of its unreachable calls removed too early.
//
// See discussion here:
// https://gcc.gnu.org/pipermail/gcc-patches/2021-June/571709.html

void
gimple_range_global (vrange &r, tree name)
{
  tree type = TREE_TYPE (name);
  gcc_checking_assert (TREE_CODE (name) == SSA_NAME);

  if (SSA_NAME_IS_DEFAULT_DEF (name) || (cfun && cfun->after_inlining)
      || is_a<gphi *> (SSA_NAME_DEF_STMT (name)))
    {
      get_range_global (r, name);
      return;
    }
  r.set_varying (type);
}

// ----------------------------------------------
// global_range_query implementation.

global_range_query global_ranges;

bool
global_range_query::range_of_expr (vrange &r, tree expr, gimple *stmt)
{
  if (!gimple_range_ssa_p (expr))
    return get_tree_range (r, expr, stmt);

  get_range_global (r, expr);

  return true;
}

// Return any known relation between SSA1 and SSA2 before stmt S is executed.
// If GET_RANGE is true, query the range of both operands first to ensure
// the defintions have been processed and any relations have be created.

relation_kind
range_query::query_relation (gimple *s, tree ssa1, tree ssa2, bool get_range)
{
  if (!m_oracle || TREE_CODE (ssa1) != SSA_NAME || TREE_CODE (ssa2) != SSA_NAME)
    return VREL_VARYING;

  // Ensure ssa1 and ssa2 have both been evaluated.
  if (get_range)
    {
      Value_Range tmp1 (TREE_TYPE (ssa1));
      Value_Range tmp2 (TREE_TYPE (ssa2));
      range_of_expr (tmp1, ssa1, s);
      range_of_expr (tmp2, ssa2, s);
    }
  return m_oracle->query_relation (gimple_bb (s), ssa1, ssa2);
}

// Return any known relation between SSA1 and SSA2 on edge E.
// If GET_RANGE is true, query the range of both operands first to ensure
// the defintions have been processed and any relations have be created.

relation_kind
range_query::query_relation (edge e, tree ssa1, tree ssa2, bool get_range)
{
  basic_block bb;
  if (!m_oracle || TREE_CODE (ssa1) != SSA_NAME || TREE_CODE (ssa2) != SSA_NAME)
    return VREL_VARYING;

  // Use destination block if it has a single predecessor, and this picks
  // up any relation on the edge.
  // Otherwise choose the src edge and the result is the same as on-exit.
  if (!single_pred_p (e->dest))
    bb = e->src;
  else
    bb = e->dest;

  // Ensure ssa1 and ssa2 have both been evaluated.
  if (get_range)
    {
      Value_Range tmp (TREE_TYPE (ssa1));
      range_on_edge (tmp, e, ssa1);
      range_on_edge (tmp, e, ssa2);
    }
  return m_oracle->query_relation (bb, ssa1, ssa2);
}
