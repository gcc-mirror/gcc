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

#ifndef GCC_TREE_VRP_H
#define GCC_TREE_VRP_H

#include "value-range.h"

/* Note value_range_equiv cannot currently be used with GC memory,
   only value_range is fully set up for this.  */
class GTY((user)) value_range_equiv : public value_range
{
 public:
  value_range_equiv ();
  value_range_equiv (const value_range &);
  /* Deep-copies equiv bitmap argument.  */
  value_range_equiv (tree, tree, bitmap = NULL, value_range_kind = VR_RANGE);

  /* Shallow-copies equiv bitmap.  */
  value_range_equiv (const value_range_equiv &) /* = delete */;
  /* Shallow-copies equiv bitmap.  */
  value_range_equiv& operator=(const value_range_equiv &) /* = delete */;

  /* Move equiv bitmap from source range.  */
  void move (value_range_equiv *);

  /* Leaves equiv bitmap alone.  */
  void update (tree, tree, value_range_kind = VR_RANGE);
  /* Deep-copies equiv bitmap argument.  */
  void set (tree, tree, bitmap = NULL, value_range_kind = VR_RANGE);
  void set (tree);

  bool operator== (const value_range_equiv &) const /* = delete */;
  bool operator!= (const value_range_equiv &) const /* = delete */;
  void intersect (const value_range_equiv *);
  void union_ (const value_range_equiv *);
  bool equal_p (const value_range_equiv &, bool ignore_equivs) const;

  /* Types of value ranges.  */
  void set_undefined ();
  void set_varying (tree);

  /* Equivalence bitmap methods.  */
  bitmap equiv () const;
  void equiv_clear ();
  void equiv_add (const_tree, const value_range_equiv *,
		  bitmap_obstack * = NULL);

  /* Misc methods.  */
  void deep_copy (const value_range_equiv *);
  void dump (FILE *) const;
  void dump () const;

 private:
  /* Deep-copies bitmap argument.  */
  void set_equiv (bitmap);
  void check ();

  /* Set of SSA names whose value ranges are equivalent to this one.
     This set is only valid when TYPE is VR_RANGE or VR_ANTI_RANGE.  */
  bitmap m_equiv;
};

inline
value_range_equiv::value_range_equiv ()
  : value_range ()
{
  m_equiv = NULL;
}

inline bitmap
value_range_equiv::equiv () const
{
  return m_equiv;
}

extern void dump_value_range (FILE *, const value_range_equiv *);

struct assert_info
{
  /* Predicate code for the ASSERT_EXPR.  Must be COMPARISON_CLASS_P.  */
  enum tree_code comp_code;

  /* Name to register the assert for.  */
  tree name;

  /* Value being compared against.  */
  tree val;

  /* Expression to compare.  */
  tree expr;
};

extern void register_edge_assert_for (tree, edge, enum tree_code,
				      tree, tree, vec<assert_info> &);
extern bool stmt_interesting_for_vrp (gimple *);
extern bool infer_value_range (gimple *, tree, tree_code *, tree *);

extern bool range_int_cst_p (const value_range *);

extern int compare_values (tree, tree);
extern int compare_values_warnv (tree, tree, bool *);
extern int operand_less_p (tree, tree);

void range_fold_unary_expr (value_range *, enum tree_code, tree type,
			    const value_range *, tree op0_type);
void range_fold_binary_expr (value_range *, enum tree_code, tree type,
			     const value_range *, const value_range *);

extern enum value_range_kind intersect_range_with_nonzero_bits
  (enum value_range_kind, wide_int *, wide_int *, const wide_int &, signop);

extern bool find_case_label_range (gswitch *, tree, tree, size_t *, size_t *);
extern bool find_case_label_index (gswitch *, size_t, tree, size_t *);
extern bool overflow_comparison_p (tree_code, tree, tree, bool, tree *);
extern tree get_single_symbol (tree, bool *, tree *);
extern void maybe_set_nonzero_bits (edge, tree);
extern value_range_kind determine_value_range (tree, wide_int *, wide_int *);

#endif /* GCC_TREE_VRP_H */
