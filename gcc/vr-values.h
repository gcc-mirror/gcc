/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2016-2020 Free Software Foundation, Inc.

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

#include "value-range-equiv.h"

// Class to simplify a statement using range information.
//
// The constructor takes a full vr_values, but all it needs is
// get_value_range() from it.  This class could be made to work with
// any range repository.

class simplify_using_ranges
{
public:
  simplify_using_ranges (class vr_values *);
  ~simplify_using_ranges ();
  bool simplify (gimple_stmt_iterator *);

  // ?? These should be cleaned, merged, and made private.
  tree vrp_evaluate_conditional (tree_code, tree, tree, gimple *);
  void vrp_visit_cond_stmt (gcond *, edge *);
  tree vrp_evaluate_conditional_warnv_with_ops (enum tree_code,
						tree, tree, bool,
						bool *, bool *);

private:
  const value_range_equiv *get_value_range (const_tree op);
  bool simplify_truth_ops_using_ranges (gimple_stmt_iterator *, gimple *);
  bool simplify_div_or_mod_using_ranges (gimple_stmt_iterator *, gimple *);
  bool simplify_abs_using_ranges (gimple_stmt_iterator *, gimple *);
  bool simplify_bit_ops_using_ranges (gimple_stmt_iterator *, gimple *);
  bool simplify_min_or_max_using_ranges (gimple_stmt_iterator *, gimple *);
  bool simplify_cond_using_ranges_1 (gcond *);
  bool fold_cond (gcond *);
  bool simplify_switch_using_ranges (gswitch *);
  bool simplify_float_conversion_using_ranges (gimple_stmt_iterator *,
					       gimple *);
  bool simplify_internal_call_using_ranges (gimple_stmt_iterator *, gimple *);

  bool two_valued_val_range_p (tree, tree *, tree *);
  bool op_with_boolean_value_range_p (tree);
  tree compare_name_with_value (enum tree_code, tree, tree, bool *, bool);
  tree compare_names (enum tree_code, tree, tree, bool *);
  const value_range_equiv *get_vr_for_comparison (int, value_range_equiv *);
  tree vrp_evaluate_conditional_warnv_with_ops_using_ranges (enum tree_code,
							     tree, tree,
							     bool *);
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
  class vr_values *store;
};

/* The VR_VALUES class holds the current view of range information
   for all the SSA_NAMEs in the IL.

   It can be used to hold context sensitive range information during
   a dominator walk or it may be used to hold range information in the
   standard VRP pass as ranges are propagated through the lattice to a
   steady state.

   This information is independent of the range information that gets
   attached to SSA_NAMEs.  A pass such as VRP may choose to transfer
   the global information it produces into global range information that
   gets attached to an SSA_NAME.  It's unclear how useful that global
   information will be in a world where we can compute context sensitive
   range information fast or perform on-demand queries.  */
class vr_values
{
 public:
  vr_values (void);
  ~vr_values (void);

  const value_range_equiv *get_value_range (const_tree);
  void set_vr_value (tree, value_range_equiv *);
  value_range_equiv *swap_vr_value (tree, value_range_equiv *);

  void set_def_to_varying (const_tree);
  void set_defs_to_varying (gimple *);
  bool update_value_range (const_tree, value_range_equiv *);
  tree op_with_constant_singleton_value_range (tree);
  void adjust_range_with_scev (value_range_equiv *, class loop *,
			       gimple *, tree);
  void dump_all_value_ranges (FILE *);

  void extract_range_for_var_from_comparison_expr (tree, enum tree_code,
						   tree, tree,
						   value_range_equiv *);
  void extract_range_from_phi_node (gphi *, value_range_equiv *);
  void extract_range_basic (value_range_equiv *, gimple *);
  void extract_range_from_stmt (gimple *, edge *, tree *, value_range_equiv *);

  /* Indicate that propagation through the lattice is complete.  */
  void set_lattice_propagation_complete (void) { values_propagated = true; }

  /* Allocate a new value_range object.  */
  value_range_equiv *allocate_value_range_equiv (void)
    { return vrp_value_range_pool.allocate (); }
  void free_value_range (value_range_equiv *vr)
    { vrp_value_range_pool.remove (vr); }

 private:
  value_range_equiv *get_lattice_entry (const_tree);
  bool vrp_stmt_computes_nonzero (gimple *);
  void extract_range_from_assignment (value_range_equiv *, gassign *);
  void extract_range_from_assert (value_range_equiv *, tree);
  void extract_range_from_ssa_name (value_range_equiv *, tree);
  void extract_range_from_binary_expr (value_range_equiv *, enum tree_code,
				       tree, tree, tree);
  void extract_range_from_unary_expr (value_range_equiv *, enum tree_code,
				      tree, tree);
  void extract_range_from_cond_expr (value_range_equiv *, gassign *);
  void extract_range_from_comparison (value_range_equiv *, enum tree_code,
				      tree, tree, tree);
  void vrp_visit_assignment_or_call (gimple*, tree *, value_range_equiv *);
  void vrp_visit_switch_stmt (gswitch *, edge *);

  /* Allocation pools for value_range objects.  */
  object_allocator<value_range_equiv> vrp_value_range_pool;

  /* This probably belongs in the lattice rather than in here.  */
  bool values_propagated;

  /* Allocations for equivalences all come from this obstack.  */
  bitmap_obstack vrp_equiv_obstack;

  /* Value range array.  After propagation, VR_VALUE[I] holds the range
     of values that SSA name N_I may take.  */
  unsigned int num_vr_values;
  value_range_equiv **vr_value;

  /* For a PHI node which sets SSA name N_I, VR_COUNTS[I] holds the
     number of executable edges we saw the last time we visited the
     node.  */
  int *vr_phi_edge_counts;
  simplify_using_ranges simplifier;
};

inline const value_range_equiv *
simplify_using_ranges::get_value_range (const_tree op)
{
  return store->get_value_range (op);
}

extern tree get_output_for_vrp (gimple *);

// FIXME: Move this to tree-vrp.c.
void simplify_cond_using_ranges_2 (class vr_values *, gcond *);

#endif /* GCC_VR_VALUES_H */
