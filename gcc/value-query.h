/* Support routines for value queries.
   Copyright (C) 2020-2025 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com> and
   Andrew Macleod <amacleod@redhat.com>.

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

#ifndef GCC_QUERY_H
#define GCC_QUERY_H

#include "value-relation.h"

// The value_query class is used by optimization passes that require
// valueizing SSA names in terms of a tree value, but have no need
// for ranges.
//
// value_of_expr must be provided.  The default for value_on_edge and
// value_of_stmt is to call value_of_expr.
//
// This implies the valuation is global in nature.  If a pass can make
// use of more specific information, it can override the other queries.
//
// Proper usage of the correct query in passes will enable other
// valuation mechanisms to produce more precise results.

// The range_query class is used by optimization passes which are
// range aware.
//
// range_of_expr must be provided.  The default for range_on_edge and
// range_of_stmt is to call range_of_expr.  If a pass can make use of
// more specific information, then it can override the other queries.
//
// The default for the value_* routines is to call the equivalent
// range_* routines, check if the range is a singleton, and return it
// if so.
//
// The get_value_range method is currently provided for compatibility
// with vr-values.  It will be deprecated when possible.

class range_query
{
public:
  range_query ();
  virtual ~range_query ();

  virtual tree value_of_expr (tree expr, gimple * = NULL);
  virtual tree value_on_edge (edge, tree expr);
  virtual tree value_of_stmt (gimple *, tree name = NULL);
  virtual tree value_on_entry (basic_block, tree expr);
  virtual tree value_on_exit (basic_block, tree expr);

  // These are the range equivalents of the value_* methods.  Instead
  // of returning a singleton, they calculate a range and return it in
  // R.  TRUE is returned on success or FALSE if no range was found.
  //
  // Note that range_of_expr must always return TRUE unless ranges are
  // unsupported for EXPR's type (supports_type_p is false).
  virtual bool range_of_expr (vrange &r, tree expr, gimple * = NULL) = 0;
  virtual bool range_on_edge (vrange &r, edge, tree expr);
  virtual bool range_of_stmt (vrange &r, gimple *, tree name = NULL);
  virtual bool range_on_entry (vrange &r, basic_block bb, tree expr);
  virtual bool range_on_exit (vrange &r, basic_block bb, tree expr);

  inline class relation_oracle &relation () const  { return *m_relation; }
  void create_relation_oracle (bool do_trans_p = true);
  void destroy_relation_oracle ();

  inline class infer_range_oracle &infer_oracle () const { return *m_infer; }
  void create_infer_oracle (range_query *q = NULL, bool do_search = true);
  void destroy_infer_oracle ();

  inline class gimple_outgoing_range &gori () const { return *m_gori; }
  inline class gori_map *gori_ssa () const { return m_map; }
  void create_gori (int not_executable_flag = 0, int sw_max_edges = INT_MAX);
  void destroy_gori ();

  virtual void dump (FILE *);

protected:
  bool get_tree_range (vrange &v, tree expr, gimple *stmt,
		       basic_block bbentry = NULL, basic_block bbexit = NULL);
  bool invoke_range_of_expr (vrange &v, tree expr, gimple *stmt,
			     basic_block bbentry, basic_block bbexit);
  bool get_arith_expr_range (vrange &r, tree expr, gimple *stmt);
  relation_oracle *m_relation;
  infer_range_oracle *m_infer;
  gimple_outgoing_range *m_gori;
  gori_map *m_map;
  // When multiple related range queries wish to share oracles.
  // This is an internal interface
  void share_query (range_query &q);
  bool m_shared_copy_p;

};

// Global ranges for SSA names using SSA_NAME_RANGE_INFO.

class global_range_query : public range_query
{
public:
  bool range_of_expr (vrange &r, tree expr, gimple * = NULL) override;
};

extern global_range_query global_ranges;

inline range_query *
get_global_range_query ()
{
  return &global_ranges;
}

/* Returns the currently active range access class.  When there is no active
   range class, global ranges are used.  Never returns null.  */

ATTRIBUTE_RETURNS_NONNULL inline range_query *
get_range_query (const struct function *fun)
{
  return (fun && fun->x_range_query) ? fun->x_range_query : &global_ranges;
}

// Query the global range of NAME in function F.  Default to cfun.
extern void gimple_range_global (vrange &v, tree name,
				 struct function *f = cfun);
#endif // GCC_QUERY_H
