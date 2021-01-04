/* Support routines for value queries.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// value_query default methods.

tree
value_query::value_on_edge (edge, tree name)
{
  return value_of_expr (name);
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
range_query::range_on_edge (irange &r, edge, tree name)
{
  return range_of_expr (r, name);
}

bool
range_query::range_of_stmt (irange &r, gimple *stmt, tree name)
{
  if (!name)
    name = gimple_get_lhs (stmt);

  gcc_checking_assert (!name || name == gimple_get_lhs (stmt));

  if (name)
    return range_of_expr (r, name);
  return false;
}

tree
range_query::value_of_expr (tree name, gimple *stmt)
{
  tree t;
  int_range_max r;

  if (!irange::supports_type_p (TREE_TYPE (name)))
    return NULL_TREE;

  if (range_of_expr (r, name, stmt))
    {
      // A constant used in an unreachable block oftens returns as UNDEFINED.
      // If the result is undefined, check the global value for a constant.
      if (r.undefined_p ())
	range_of_expr (r, name);
      if (r.singleton_p (&t))
	return t;
    }
  return NULL_TREE;
}

tree
range_query::value_on_edge (edge e, tree name)
{
  tree t;
  int_range_max r;

  if (!irange::supports_type_p (TREE_TYPE (name)))
    return NULL_TREE;
  if (range_on_edge (r, e, name))
    {
      // A constant used in an unreachable block oftens returns as UNDEFINED.
      // If the result is undefined, check the global value for a constant.
      if (r.undefined_p ())
	range_of_expr (r, name);
      if (r.singleton_p (&t))
	return t;
    }
  return NULL_TREE;

}

tree
range_query::value_of_stmt (gimple *stmt, tree name)
{
  tree t;
  int_range_max r;

  if (!name)
    name = gimple_get_lhs (stmt);

  gcc_checking_assert (!name || name == gimple_get_lhs (stmt));

  if (!name || !irange::supports_type_p (TREE_TYPE (name)))
    return NULL_TREE;
  if (range_of_stmt (r, stmt, name) && r.singleton_p (&t))
    return t;
  return NULL_TREE;

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
  int_range_max r;
  if (range_of_expr (r, const_cast<tree> (expr), stmt))
    return new (equiv_alloc->allocate ()) value_range_equiv (r);
  return new (equiv_alloc->allocate ()) value_range_equiv (TREE_TYPE (expr));
}

range_query::range_query ()
{
  equiv_alloc = new equiv_allocator;
}

range_query::~range_query ()
{
  equiv_alloc->release ();
  delete equiv_alloc;
}
