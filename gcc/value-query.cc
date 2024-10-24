/* Support routines for value queries.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.
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
#include "value-query.h"
#include "alloc-pool.h"
#include "gimple-range.h"
#include "value-range-storage.h"

// range_query default methods.

bool
range_query::range_on_edge (vrange &r, edge, tree expr)
{
  return range_of_expr (r, expr);
}

bool
range_query::range_on_entry (vrange &r, basic_block, tree expr)
{
  return range_of_expr (r, expr);
}

bool
range_query::range_on_exit (vrange &r, basic_block, tree expr)
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

// If the range of expr EXPR at STMT is a single value, return it.
// Otherwise return NULL_TREE.

tree
range_query::value_of_expr (tree expr, gimple *stmt)
{
  tree t;

  if (!value_range::supports_type_p (TREE_TYPE (expr)))
    return NULL_TREE;

  value_range r (TREE_TYPE (expr));

  if (range_of_expr (r, expr, stmt))
    {
      // A constant used in an unreachable block often returns as UNDEFINED.
      // If the result is undefined, check the global value for a constant.
      if (r.undefined_p ())
	range_of_expr (r, expr);
      if (r.singleton_p (&t))
	return t;
    }
  return NULL_TREE;
}

// If the range on edge E for EXPR is a single value, return it.
// Otherwise return NULL_TREE.

tree
range_query::value_on_edge (edge e, tree expr)
{
  tree t;

  if (!value_range::supports_type_p (TREE_TYPE (expr)))
    return NULL_TREE;
  value_range r (TREE_TYPE (expr));
  if (range_on_edge (r, e, expr))
    {
      // A constant used in an unreachable block often returns as UNDEFINED.
      // If the result is undefined, check the global value for a constant.
      if (r.undefined_p ())
	range_of_expr (r, expr);
      if (r.singleton_p (&t))
	return t;
    }
  return NULL_TREE;
}

// If the range of STMT for NAME is a single value, return it.
// Otherwise return NULL_TREE.

tree
range_query::value_of_stmt (gimple *stmt, tree name)
{
  tree t;

  if (!name)
    name = gimple_get_lhs (stmt);

  gcc_checking_assert (!name || name == gimple_get_lhs (stmt));

  if (!name || !value_range::supports_type_p (TREE_TYPE (name)))
    return NULL_TREE;
  value_range r (TREE_TYPE (name));
  if (range_of_stmt (r, stmt, name) && r.singleton_p (&t))
    return t;
  return NULL_TREE;
}

// If the range on entry to BB for EXPR is a single value, return it.
// Otherwise return NULL_TREE.

tree
range_query::value_on_entry (basic_block bb, tree expr)
{
  tree t;

  gcc_checking_assert (bb);
  if (!value_range::supports_type_p (TREE_TYPE (expr)))
    return NULL_TREE;

  value_range r (TREE_TYPE (expr));

  if (range_on_entry (r, bb, expr) && r.singleton_p (&t))
    return t;
  return NULL_TREE;
}

// If the range on exit to BB for EXPR is a single value, return it.
// Otherwise return NULL_TREE.

tree
range_query::value_on_exit (basic_block bb, tree expr)
{
  tree t;

  gcc_checking_assert (bb);
  if (!value_range::supports_type_p (TREE_TYPE (expr)))
    return NULL_TREE;

  value_range r (TREE_TYPE (expr));

  if (range_on_exit (r, bb, expr) && r.singleton_p (&t))
    return t;
  return NULL_TREE;
}

void
range_query::dump (FILE *)
{
}

// Default oracle for all range queries.  This contains no storage and thus
// can be used anywhere.
relation_oracle default_relation_oracle;
infer_range_oracle default_infer_oracle;
gimple_outgoing_range default_gori;

void
range_query::create_gori (int not_executable_flag, int sw_max_edges)
{
  gcc_checking_assert (m_gori == &default_gori);
  gcc_checking_assert (m_map == NULL);
  m_map = new gori_map ();
  gcc_checking_assert (m_map);
  m_gori = new gori_compute (*m_map, not_executable_flag, sw_max_edges);
  gcc_checking_assert (m_gori);
}

void
range_query::destroy_gori ()
{
  if (m_gori && m_gori != &default_gori)
    delete m_gori;
  if (m_map)
    delete m_map;
  m_map = NULL;
  m_gori= &default_gori;
}

void
range_query::create_infer_oracle (bool do_search)
{
  gcc_checking_assert (m_infer == &default_infer_oracle);
  m_infer = new infer_range_manager (do_search);
  gcc_checking_assert (m_infer);
}

void
range_query::destroy_infer_oracle ()
{
  if (m_infer && m_infer != &default_infer_oracle)
    delete m_infer;
  m_infer = &default_infer_oracle;
}

// Create dominance based range oracle for the current query if dom info is
// available.  DO_TRANS_P indicates whether transitive relations should
// be created.  This can cost more in compile time.

void
range_query::create_relation_oracle (bool do_trans_p)
{
  gcc_checking_assert (this != &global_ranges);
  gcc_checking_assert (m_relation == &default_relation_oracle);

  if (!dom_info_available_p (CDI_DOMINATORS))
    return;
  m_relation = new dom_oracle (do_trans_p);
  gcc_checking_assert (m_relation);
}

// Destroy any relation oracle that was created.

void
range_query::destroy_relation_oracle ()
{
  // m_relation can be NULL if a derived range_query class took care of
  // disposing its own oracle.
  if (m_relation && m_relation != &default_relation_oracle)
    {
      delete m_relation;
      m_relation = &default_relation_oracle;
    }
}

void
range_query::share_query (range_query &q)
{
  m_relation = q.m_relation;
  m_infer = q.m_infer;
  m_gori = q.m_gori;
  m_map = q.m_map;
  m_shared_copy_p = true;
}

range_query::range_query ()
{
  m_relation = &default_relation_oracle;
  m_infer = &default_infer_oracle;
  m_gori = &default_gori;
  m_map = NULL;
  m_shared_copy_p = false;
}

range_query::~range_query ()
{
  // Do not destroy anything if this is a shared copy.
  if (m_shared_copy_p)
    return;
  destroy_gori ();
  destroy_infer_oracle ();
  destroy_relation_oracle ();
}

// This routine will invoke the equivalent of range_of_expr on
// either a gimple statement STMT, on entry to block BBENTRY, or on
// exit from block BBEXIT.   Only one of these 3 fields may be set.
// It is valid for none of them to be set, in wqhich case there is no context.

bool
range_query::invoke_range_of_expr (vrange &r, tree expr, gimple *stmt,
				   basic_block bbentry, basic_block bbexit)
{
  if (bbentry)
    {
      gcc_checking_assert (!stmt && !bbexit);
      return range_on_entry (r, bbentry, expr);
    }
  if (bbexit)
    {
      gcc_checking_assert (!stmt);
      return range_on_exit (r, bbexit, expr);
    }

  return range_of_expr (r, expr, stmt);
}

// Return a range in R for the tree EXPR.  The context can be either a STMT,
// or on entry to block BBENTRY or exit from block BBEXIT.
// Return true if a range is representable, and UNDEFINED/false if not.

bool
range_query::get_tree_range (vrange &r, tree expr, gimple *stmt,
			     basic_block bbentry, basic_block bbexit)
{
  tree type;
  if (TYPE_P (expr))
    type = expr;
  else
    type = TREE_TYPE (expr);

  if (!value_range::supports_type_p (type))
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
      {
	if (TREE_OVERFLOW_P (expr))
	  expr = drop_tree_overflow (expr);
	r.set (expr, expr);
	return true;
      }

    case REAL_CST:
      {
	frange &f = as_a <frange> (r);
	REAL_VALUE_TYPE *rv = TREE_REAL_CST_PTR (expr);
	if (real_isnan (rv))
	  {
	    bool sign = real_isneg (rv);
	    f.set_nan (TREE_TYPE (expr), sign);
	  }
	else
	  {
	    nan_state nan (false);
	    f.set (TREE_TYPE (expr), *rv, *rv, nan);
	  }
	return true;
      }

    case SSA_NAME:
      // If this is not an abnormal or virtual ssa, invoke range_of_expr.
      if (gimple_range_ssa_p (expr))
	return invoke_range_of_expr (r, expr, stmt, bbentry, bbexit);
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
      if (POLY_INT_CST_P (expr))
	{
	  unsigned int precision = TYPE_PRECISION (type);
	  r.set_varying (type);
	  r.update_bitmask ({ wi::zero (precision), get_nonzero_bits (expr) });
	  return true;
	}
      break;
    }
  if (BINARY_CLASS_P (expr) || COMPARISON_CLASS_P (expr))
    {
      tree op0 = TREE_OPERAND (expr, 0);
      tree op1 = TREE_OPERAND (expr, 1);
      if (COMPARISON_CLASS_P (expr)
	  && !value_range::supports_type_p (TREE_TYPE (op0)))
	return false;
      range_op_handler op (TREE_CODE (expr));
      if (op)
	{
	  value_range r0 (TREE_TYPE (op0));
	  value_range r1 (TREE_TYPE (op1));
	  invoke_range_of_expr (r0, op0, stmt, bbentry, bbexit);
	  invoke_range_of_expr (r1, op1, stmt, bbentry, bbexit);
	  if (!op.fold_range (r, type, r0, r1))
	    r.set_varying (type);
	}
      else
	r.set_varying (type);
      return true;
    }
  if (UNARY_CLASS_P (expr))
    {
      range_op_handler op (TREE_CODE (expr));
      tree op0_type = TREE_TYPE (TREE_OPERAND (expr, 0));
      if (op && value_range::supports_type_p (op0_type))
	{
	  value_range r0 (TREE_TYPE (TREE_OPERAND (expr, 0)));
	  value_range r1 (type);
	  r1.set_varying (type);
	  invoke_range_of_expr (r0, TREE_OPERAND (expr, 0), stmt, bbentry,
				bbexit);
	  if (!op.fold_range (r, type, r0, r1))
	    r.set_varying (type);
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

  vrange_storage *ri = SSA_NAME_RANGE_INFO (name);

  if (ri)
    ri->get_vrange (r, TREE_TYPE (name));
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
// See discussion here regarding why there use to be a wrapper function:
// https://gcc.gnu.org/pipermail/gcc-patches/2021-June/571709.html
// Legacy EVRP has been removed, leaving just this function.

void
gimple_range_global (vrange &r, tree name, struct function *fun)
{
  tree type = TREE_TYPE (name);
  gcc_checking_assert (TREE_CODE (name) == SSA_NAME);

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
	      && ((cfun && fun == cfun && nonnull_arg_p (sym))
		  || get_ssa_name_ptr_info_nonnull (name)))
	    r.set_nonzero (type);
	  else if (!POINTER_TYPE_P (type))
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

// ----------------------------------------------
// global_range_query implementation.

global_range_query global_ranges;

bool
global_range_query::range_of_expr (vrange &r, tree expr, gimple *stmt)
{
  if (!gimple_range_ssa_p (expr))
    return get_tree_range (r, expr, stmt);

  gimple_range_global (r, expr);

  return true;
}
