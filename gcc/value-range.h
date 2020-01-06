/* Support routines for value ranges.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

#ifndef GCC_VALUE_RANGE_H
#define GCC_VALUE_RANGE_H

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

// Range of values that can be associated with an SSA_NAME.

class GTY((for_user)) value_range
{
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
  void normalize_symbolics ();
  void normalize_addresses ();

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

  friend void gt_ggc_mx_value_range (void *);
  friend void gt_pch_p_11value_range (void *, void *,
				      gt_pointer_operator, void *);
  friend void gt_pch_nx_value_range (void *);
  friend void gt_ggc_mx (value_range &);
  friend void gt_ggc_mx (value_range *&);
  friend void gt_pch_nx (value_range &);
  friend void gt_pch_nx (value_range *, gt_pointer_operator, void *);

  enum value_range_kind m_kind;
  tree m_min;
  tree m_max;

private:
  int value_inside_range (tree) const;
};

extern bool range_has_numeric_bounds_p (const value_range *);
extern bool ranges_from_anti_range (const value_range *,
				    value_range *, value_range *);
extern void dump_value_range (FILE *, const value_range *);
extern bool vrp_val_is_min (const_tree);
extern bool vrp_val_is_max (const_tree);
extern tree vrp_val_min (const_tree);
extern tree vrp_val_max (const_tree);
extern bool vrp_operand_equal_p (const_tree, const_tree);

inline
value_range::value_range ()
{
  m_kind = VR_UNDEFINED;
  m_min = m_max = NULL;
}

inline value_range_kind
value_range::kind () const
{
  return m_kind;
}

inline tree
value_range::type () const
{
  return TREE_TYPE (min ());
}

inline tree
value_range::min () const
{
  return m_min;
}

inline tree
value_range::max () const
{
  return m_max;
}

inline bool
value_range::varying_p () const
{
  return m_kind == VR_VARYING;
}

inline bool
value_range::undefined_p () const
{
  return m_kind == VR_UNDEFINED;
}

inline bool
value_range::zero_p () const
{
  return (m_kind == VR_RANGE
	  && integer_zerop (m_min)
	  && integer_zerop (m_max));
}

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

inline bool
value_range::supports_type_p (tree type)
{
  if (type && (INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type)))
    return type;
  return false;
}

inline bool
range_includes_zero_p (const value_range *vr)
{
  if (vr->undefined_p ())
    return false;

  if (vr->varying_p ())
    return true;

  return vr->may_contain_p (build_zero_cst (vr->type ()));
}

#endif // GCC_VALUE_RANGE_H
