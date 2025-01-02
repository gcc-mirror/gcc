/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2016-2025 Free Software Foundation, Inc.

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

#ifndef GCC_VR_VALUES_H
#define GCC_VR_VALUES_H

#include "value-query.h"

// Abstract class to return a range for a given SSA.

// Class to simplify a statement using range information.

class simplify_using_ranges
{
public:
  simplify_using_ranges (range_query *query = NULL,
			 int not_executable_flag = 0);
  ~simplify_using_ranges ();
  bool simplify (gimple_stmt_iterator *);
  bool fold_cond (gcond *);
private:
  void legacy_fold_cond (gcond *, edge *);
  tree legacy_fold_cond_overflow (gimple *stmt);
  tree fold_cond_with_ops (tree_code, tree, tree, gimple *s);
  bool simplify_casted_compare (tree_code &cond_code, tree &op0, tree &op1);
  bool simplify_truth_ops_using_ranges (gimple_stmt_iterator *, gimple *);
  bool simplify_div_or_mod_using_ranges (gimple_stmt_iterator *, gimple *);
  bool simplify_abs_using_ranges (gimple_stmt_iterator *, gimple *);
  bool simplify_bit_ops_using_ranges (gimple_stmt_iterator *, gimple *);
  bool simplify_min_or_max_using_ranges (gimple_stmt_iterator *, gimple *);
  bool simplify_cond_using_ranges_1 (gcond *);
  bool simplify_compare_using_ranges_1 (tree_code &, tree &, tree &, gimple *);
  bool simplify_compare_assign_using_ranges_1 (gimple_stmt_iterator *, gimple *);
  bool simplify_switch_using_ranges (gswitch *);
  bool simplify_float_conversion_using_ranges (gimple_stmt_iterator *,
					       gimple *);
  bool simplify_internal_call_using_ranges (gimple_stmt_iterator *, gimple *);

  bool two_valued_val_range_p (tree, tree *, tree *, gimple *);
  bool op_with_boolean_value_range_p (tree, gimple *);
  void set_and_propagate_unexecutable (edge e);
  void cleanup_edges_and_switches (void);

  /* Vectors of edges that need removing and switch statements that
     need updating.  It is expected that a pass using the simplification
     routines will, at the end of the pass, clean up the edges and
     switch statements.  The class dtor will try to detect cases
     that do not follow that expectation.  */
  struct switch_update {
    gswitch *stmt;
    tree vec;
  };

  vec<edge> to_remove_edges;
  vec<switch_update> to_update_switch_stmts;
  class range_query *query;
  int m_not_executable_flag;   // Non zero if not_executable flag exists.
  vec<edge> m_flag_set_edges;  // List of edges with flag to be cleared.
};

extern bool range_fits_type_p (const irange *vr,
			       unsigned dest_precision, signop dest_sgn);
extern bool range_of_var_in_loop (vrange &, tree var, class loop *, gimple *,
				  range_query *);

#endif /* GCC_VR_VALUES_H */
