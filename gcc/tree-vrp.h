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
class GTY((for_user)) value_range_base
{
public:
  value_range_base ();
  value_range_base (value_range_kind, tree, tree);

  void set (value_range_kind, tree, tree);
  void set (tree);
  void set_nonnull (tree);
  void set_null (tree);

  enum value_range_kind kind () const;
  tree min () const;
  tree max () const;

  /* Types of value ranges.  */
  bool symbolic_p () const;
  bool constant_p () const;
  bool undefined_p () const;
  bool varying_p () const;
  void set_varying ();
  void set_undefined ();

  void union_ (const value_range_base *);

  bool operator== (const value_range_base &) const /* = delete */;
  bool operator!= (const value_range_base &) const /* = delete */;
  bool equal_p (const value_range_base &) const;

  /* Misc methods.  */
  tree type () const;
  bool may_contain_p (tree) const;
  void set_and_canonicalize (enum value_range_kind, tree, tree);
  bool zero_p () const;
  bool singleton_p (tree *result = NULL) const;
  void dump (FILE *) const;

protected:
  void check ();
  static value_range_base union_helper (const value_range_base *,
					const value_range_base *);

  enum value_range_kind m_kind;

  tree m_min;
  tree m_max;

  friend void gt_ggc_mx_value_range_base (void *);
  friend void gt_pch_p_16value_range_base (void *, void *,
					   gt_pointer_operator, void *);
  friend void gt_pch_nx_value_range_base (void *);
  friend void gt_ggc_mx (value_range_base &);
  friend void gt_ggc_mx (value_range_base *&);
  friend void gt_pch_nx (value_range_base &);
  friend void gt_pch_nx (value_range_base *, gt_pointer_operator, void *);
};

/* Note value_range cannot currently be used with GC memory, only
   value_range_base is fully set up for this.  */
class GTY((user)) value_range : public value_range_base
{
 public:
  value_range ();
  value_range (const value_range_base &);
  /* Deep-copies equiv bitmap argument.  */
  value_range (value_range_kind, tree, tree, bitmap = NULL);

  /* Shallow-copies equiv bitmap.  */
  value_range (const value_range &) /* = delete */;
  /* Shallow-copies equiv bitmap.  */
  value_range& operator=(const value_range&) /* = delete */;

  /* Move equiv bitmap from source range.  */
  void move (value_range *);

  /* Leaves equiv bitmap alone.  */
  void update (value_range_kind, tree, tree);
  /* Deep-copies equiv bitmap argument.  */
  void set (value_range_kind, tree, tree, bitmap = NULL);
  void set (tree);
  void set_nonnull (tree);
  void set_null (tree);

  bool operator== (const value_range &) const /* = delete */;
  bool operator!= (const value_range &) const /* = delete */;
  void intersect (const value_range *);
  void union_ (const value_range *);
  bool equal_p (const value_range &, bool ignore_equivs) const;

  /* Types of value ranges.  */
  void set_undefined ();
  void set_varying ();

  /* Equivalence bitmap methods.  */
  bitmap equiv () const;
  void equiv_clear ();
  void equiv_add (const_tree, const value_range *, bitmap_obstack * = NULL);

  /* Misc methods.  */
  void deep_copy (const value_range *);
  void set_and_canonicalize (enum value_range_kind, tree, tree, bitmap = NULL);
  void dump (FILE *) const;

 private:
  /* Deep-copies bitmap argument.  */
  void set_equiv (bitmap);
  void check ();
  void intersect_helper (value_range *, const value_range *);

  /* Set of SSA names whose value ranges are equivalent to this one.
     This set is only valid when TYPE is VR_RANGE or VR_ANTI_RANGE.  */
  bitmap m_equiv;
};

inline
value_range_base::value_range_base ()
{
  m_kind = VR_UNDEFINED;
  m_min = m_max = NULL;
}

inline
value_range::value_range ()
  : value_range_base ()
{
  m_equiv = NULL;
}

/* Return the kind of this range.  */

inline value_range_kind
value_range_base::kind () const
{
  return m_kind;
}

inline bitmap
value_range::equiv () const
{
  return m_equiv;
}

/* Return the lower bound.  */

inline tree
value_range_base::min () const
{
  return m_min;
}

/* Return the upper bound.  */

inline tree
value_range_base::max () const
{
  return m_max;
}

/* Return TRUE if range spans the entire possible domain.  */

inline bool
value_range_base::varying_p () const
{
  return m_kind == VR_VARYING;
}

/* Return TRUE if range is undefined (essentially the empty set).  */

inline bool
value_range_base::undefined_p () const
{
  return m_kind == VR_UNDEFINED;
}

/* Return TRUE if range is the constant zero.  */

inline bool
value_range_base::zero_p () const
{
  return (m_kind == VR_RANGE
	  && integer_zerop (m_min)
	  && integer_zerop (m_max));
}

extern void dump_value_range (FILE *, const value_range *);
extern void dump_value_range (FILE *, const value_range_base *);

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
extern bool range_includes_p (const value_range_base *, HOST_WIDE_INT);
extern bool infer_value_range (gimple *, tree, tree_code *, tree *);

extern bool vrp_bitmap_equal_p (const_bitmap, const_bitmap);

extern tree value_range_constant_singleton (const value_range_base *);
extern bool range_int_cst_p (const value_range_base *);
extern bool range_int_cst_singleton_p (const value_range_base *);

extern int compare_values (tree, tree);
extern int compare_values_warnv (tree, tree, bool *);
extern int operand_less_p (tree, tree);
extern bool vrp_val_is_min (const_tree);
extern bool vrp_val_is_max (const_tree);
extern int value_inside_range (tree, tree, tree);

extern tree vrp_val_min (const_tree);
extern tree vrp_val_max (const_tree);

extern void extract_range_from_unary_expr (value_range_base *vr,
					   enum tree_code code,
					   tree type,
					   const value_range_base *vr0_,
					   tree op0_type);
extern void extract_range_from_binary_expr (value_range_base *,
					    enum tree_code,
					    tree, const value_range_base *,
					    const value_range_base *);

extern bool vrp_operand_equal_p (const_tree, const_tree);
extern enum value_range_kind intersect_range_with_nonzero_bits
  (enum value_range_kind, wide_int *, wide_int *, const wide_int &, signop);
extern bool vrp_set_zero_nonzero_bits (const tree, const value_range_base *,
				       wide_int *, wide_int *);

extern bool find_case_label_range (gswitch *, tree, tree, size_t *, size_t *);
extern bool find_case_label_index (gswitch *, size_t, tree, size_t *);
extern bool overflow_comparison_p (tree_code, tree, tree, bool, tree *);
extern tree get_single_symbol (tree, bool *, tree *);
extern void maybe_set_nonzero_bits (edge, tree);
extern value_range_kind determine_value_range (tree, wide_int *, wide_int *);

/* Return TRUE if *VR includes the value zero.  */

inline bool
range_includes_zero_p (const value_range_base *vr)
{
  return range_includes_p (vr, 0);
}

#endif /* GCC_TREE_VRP_H */
