/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2016-2019 Free Software Foundation, Inc.

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

/* Types of value ranges.  */
enum value_range_kind
{
  /* Empty range.  */
  VR_UNDEFINED,
  /* Range spans the entire domain.  */
  VR_VARYING,
  /* Range is [MIN, MAX].  */
  VR_RANGE,
  /* Range is ~[MIN, MAX].  */
  VR_ANTI_RANGE,
  /* Range is a nice guy.  */
  VR_LAST
};

/* Range of values that can be associated with an SSA_NAME after VRP
   has executed.  */
class GTY((for_user)) value_range
{
  friend void range_tests ();
public:
  value_range ();
  value_range (tree, tree, value_range_kind = VR_RANGE);
  value_range (tree type, const wide_int &, const wide_int &,
	       value_range_kind = VR_RANGE);
  value_range (tree type);

  void set (tree, tree, value_range_kind = VR_RANGE);
  void set (tree);
  void set_nonzero (tree);
  void set_zero (tree);

  enum value_range_kind kind () const;
  tree min () const;
  tree max () const;

  /* Types of value ranges.  */
  bool symbolic_p () const;
  bool constant_p () const;
  bool undefined_p () const;
  bool varying_p () const;
  void set_varying (tree type);
  void set_undefined ();

  void union_ (const value_range *);
  void intersect (const value_range *);
  void union_ (const value_range &);
  void intersect (const value_range &);

  bool operator== (const value_range &) const;
  bool operator!= (const value_range &) const /* = delete */;
  bool equal_p (const value_range &) const;

  /* Misc methods.  */
  tree type () const;
  bool may_contain_p (tree) const;
  bool zero_p () const;
  bool nonzero_p () const;
  bool singleton_p (tree *result = NULL) const;
  void dump (FILE *) const;
  void dump () const;

  static bool supports_type_p (tree);
  value_range normalize_symbolics () const;
  value_range normalize_addresses () const;

  static const unsigned int m_max_pairs = 2;
  bool contains_p (tree) const;
  unsigned num_pairs () const;
  wide_int lower_bound (unsigned = 0) const;
  wide_int upper_bound (unsigned) const;
  wide_int upper_bound () const;
  void invert ();

protected:
  void check ();
  static value_range union_helper (const value_range *, const value_range *);
  static value_range intersect_helper (const value_range *,
				       const value_range *);

  enum value_range_kind m_kind;

  tree m_min;
  tree m_max;

  friend void gt_ggc_mx_value_range (void *);
  friend void gt_pch_p_11value_range (void *, void *,
				      gt_pointer_operator, void *);
  friend void gt_pch_nx_value_range (void *);
  friend void gt_ggc_mx (value_range &);
  friend void gt_ggc_mx (value_range *&);
  friend void gt_pch_nx (value_range &);
  friend void gt_pch_nx (value_range *, gt_pointer_operator, void *);

private:
  int value_inside_range (tree) const;
};

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
value_range::value_range ()
{
  m_kind = VR_UNDEFINED;
  m_min = m_max = NULL;
}

inline
value_range_equiv::value_range_equiv ()
  : value_range ()
{
  m_equiv = NULL;
}

/* Return the kind of this range.  */

inline value_range_kind
value_range::kind () const
{
  return m_kind;
}

inline bitmap
value_range_equiv::equiv () const
{
  return m_equiv;
}

/* Return the lower bound.  */

inline tree
value_range::min () const
{
  return m_min;
}

/* Return the upper bound.  */

inline tree
value_range::max () const
{
  return m_max;
}

/* Return TRUE if range spans the entire possible domain.  */

inline bool
value_range::varying_p () const
{
  return m_kind == VR_VARYING;
}

/* Return TRUE if range is undefined (essentially the empty set).  */

inline bool
value_range::undefined_p () const
{
  return m_kind == VR_UNDEFINED;
}

/* Return TRUE if range is the constant zero.  */

inline bool
value_range::zero_p () const
{
  return (m_kind == VR_RANGE
	  && integer_zerop (m_min)
	  && integer_zerop (m_max));
}

extern void dump_value_range (FILE *, const value_range_equiv *);
extern void dump_value_range (FILE *, const value_range *);

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

// Return true if TYPE is a valid type for value_range to operate on.
// Otherwise return FALSE.

inline bool
value_range::supports_type_p (tree type)
{
  if (type && (INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type)))
    return type;
  return false;
}

extern void register_edge_assert_for (tree, edge, enum tree_code,
				      tree, tree, vec<assert_info> &);
extern bool stmt_interesting_for_vrp (gimple *);
extern bool infer_value_range (gimple *, tree, tree_code *, tree *);

extern bool range_int_cst_p (const value_range *);

extern int compare_values (tree, tree);
extern int compare_values_warnv (tree, tree, bool *);
extern int operand_less_p (tree, tree);
extern bool vrp_val_is_min (const_tree);
extern bool vrp_val_is_max (const_tree);

extern tree vrp_val_min (const_tree);
extern tree vrp_val_max (const_tree);

void range_fold_unary_expr (value_range *, enum tree_code, tree type,
			    const value_range *, tree op0_type);
void range_fold_binary_expr (value_range *, enum tree_code, tree type,
			     const value_range *, const value_range *);

extern bool vrp_operand_equal_p (const_tree, const_tree);
extern enum value_range_kind intersect_range_with_nonzero_bits
  (enum value_range_kind, wide_int *, wide_int *, const wide_int &, signop);

extern bool find_case_label_range (gswitch *, tree, tree, size_t *, size_t *);
extern bool find_case_label_index (gswitch *, size_t, tree, size_t *);
extern bool overflow_comparison_p (tree_code, tree, tree, bool, tree *);
extern tree get_single_symbol (tree, bool *, tree *);
extern void maybe_set_nonzero_bits (edge, tree);
extern value_range_kind determine_value_range (tree, wide_int *, wide_int *);

/* Return TRUE if range is nonzero.  */

inline bool
value_range::nonzero_p () const
{
  if (m_kind == VR_ANTI_RANGE
      && !TYPE_UNSIGNED (type ())
      && integer_zerop (m_min)
      && integer_zerop (m_max))
    return true;

  return (m_kind == VR_RANGE
	  && TYPE_UNSIGNED (type ())
	  && integer_onep (m_min)
	  && vrp_val_is_max (m_max));
}

/* Return TRUE if *VR includes the value zero.  */

inline bool
range_includes_zero_p (const value_range *vr)
{
  if (vr->undefined_p ())
    return false;

  if (vr->varying_p ())
    return true;

  return vr->may_contain_p (build_zero_cst (vr->type ()));
}

#endif /* GCC_TREE_VRP_H */
