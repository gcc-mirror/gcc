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

// Helper for is_a<> to distinguish between irange and widest_irange.

enum irange_discriminator
{
  IRANGE_KIND_UNKNOWN,
  IRANGE_KIND_INT,
  IRANGE_KIND_WIDEST_INT
};

// Range of values that can be associated with an SSA_NAME.
//
// This is the base class without any storage.

class irange
{
public:
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

  void union_ (const irange *);
  void intersect (const irange *);
  void union_ (const irange &);
  void intersect (const irange &);

  irange& operator= (const irange &);
  bool operator== (const irange &) const;
  bool operator!= (const irange &r) const { return !(*this == r); }
  bool equal_p (const irange &) const;

  /* Misc methods.  */
  tree type () const;
  bool may_contain_p (tree) const;
  bool zero_p () const;
  bool nonzero_p () const;
  bool singleton_p (tree *result = NULL) const;
  void dump (FILE * = stderr) const;
  void simple_dump (FILE *) const;

  static bool supports_type_p (tree);
  void normalize_symbolics ();
  void normalize_addresses ();

  bool contains_p (tree) const;
  unsigned num_pairs () const;
  wide_int lower_bound (unsigned = 0) const;
  wide_int upper_bound (unsigned) const;
  wide_int upper_bound () const;
  void invert ();

protected:
  void check ();
  // Returns true for an old-school value_range with anti ranges.
  bool simple_ranges_p () const { return m_max_ranges == 1; }
  irange (tree *, unsigned);
  irange (tree *, unsigned, const irange &);

private:
  int value_inside_range (tree) const;

  void intersect_from_wide_ints (const wide_int &, const wide_int &);
  bool maybe_anti_range (const irange &) const;
  void multi_range_set_anti_range (tree, tree);
  void multi_range_union (const irange &);
  void multi_range_intersect (const irange &);
  tree tree_lower_bound (unsigned = 0) const;
  tree tree_upper_bound (unsigned) const;

  bool compatible_copy_p (const irange &) const;
  void copy_compatible_range (const irange &);
  void copy_simple_range (const irange &);

public:
  ENUM_BITFIELD(irange_discriminator) m_discriminator : 8;
protected:
  unsigned char m_num_ranges;
  unsigned char m_max_ranges;
  ENUM_BITFIELD(value_range_kind) m_kind : 8;

  tree *m_base;
};

// int_range<N> describes an irange with N pairs of ranges.

template<unsigned N>
class GTY((user)) int_range : public irange
{
public:
  int_range ();
  int_range (tree, tree, value_range_kind = VR_RANGE);
  int_range (tree type, const wide_int &, const wide_int &,
	     value_range_kind = VR_RANGE);
  int_range (tree type);
  int_range (const int_range &);
  int_range (const irange &);
  int_range& operator= (const int_range &);
private:
  template <unsigned X> friend void gt_ggc_mx (int_range<X> *);
  template <unsigned X> friend void gt_pch_nx (int_range<X> *);
  template <unsigned X> friend void gt_pch_nx (int_range<X> *,
					       gt_pointer_operator, void *);
  /* ?? hash-traits.h has its own extern for these, which is causing
     them to never be picked up by the templates.  For now, define
     elsewhere.  */
  //template<unsigned X> friend void gt_ggc_mx (int_range<X> *&);
  //template<unsigned X> friend void gt_pch_nx (int_range<X> *&);
  friend void gt_ggc_mx (int_range<1> *&);
  friend void gt_pch_nx (int_range<1> *&);

  tree m_ranges[N*2];
};

// This is a special int_range<> with only one pair, plus
// VR_ANTI_RANGE magic to describe slightly more than can be described
// in one pair.  It is described in the code as a "simple range" (as
// opposed to multi-ranges which have multiple sub-ranges).  It is
// provided for backward compatibility with the more limited,
// traditional value_range's.
//
// simple_ranges_p() returns true for value_range's.
//
// There are copy methods to seamlessly copy to/fro multi-ranges.

typedef int_range<1> value_range;

// An irange with "unlimited" sub-ranges.  In reality we are limited
// by the number of values that fit in an `m_num_ranges'.
//
// A widest_irange starts with a handful of sub-ranges in local
// storage and will grow into the heap as necessary.

class widest_irange : public irange
{
public:
  widest_irange ();
  widest_irange (tree, tree, value_range_kind = VR_RANGE);
  widest_irange (tree, const wide_int &, const wide_int &,
		 value_range_kind = VR_RANGE);
  widest_irange (tree type);
  widest_irange (const widest_irange &);
  widest_irange (const irange &);
  ~widest_irange ();
  widest_irange& operator= (const widest_irange &);
  void resize_if_needed (unsigned);
#if CHECKING_P
  static void stats_dump (FILE *);
#endif

private:
  static const unsigned m_sub_ranges_in_local_storage = 5;
  void init_widest_irange ();

  // Memory usage stats.
  void stats_register_use (void);
  static int stats_used_buckets[11];

  tree *m_blob;
  tree m_ranges[m_sub_ranges_in_local_storage*2];
};

value_range union_helper (const value_range *, const value_range *);
value_range intersect_helper (const value_range *, const value_range *);
extern bool range_has_numeric_bounds_p (const irange *);
extern bool ranges_from_anti_range (const value_range *,
				    value_range *, value_range *);
extern void dump_value_range (FILE *, const irange *);
extern void dump_value_range_stats (FILE *);
extern bool vrp_val_is_min (const_tree);
extern bool vrp_val_is_max (const_tree);
extern tree vrp_val_min (const_tree);
extern tree vrp_val_max (const_tree);
extern bool vrp_operand_equal_p (const_tree, const_tree);

template<unsigned N>
inline
int_range<N>::int_range ()
  : irange (m_ranges, N)
{
  m_kind = VR_UNDEFINED;
  m_num_ranges = 0;
}

inline value_range_kind
irange::kind () const
{
  if (simple_ranges_p ())
    return m_kind;

  if (undefined_p ())
    return VR_UNDEFINED;

  if (varying_p ())
    return VR_VARYING;

  if (m_kind == VR_ANTI_RANGE)
    {
      // VR_ANTI_RANGE's are only valid for symbolics.
      gcc_checking_assert (m_num_ranges == 1);
      gcc_checking_assert (!range_has_numeric_bounds_p (this));
      return VR_ANTI_RANGE;
    }

  return VR_RANGE;
}

inline tree
irange::type () const
{
  gcc_checking_assert (!undefined_p ());
  return TREE_TYPE (m_base[0]);
}

inline tree
irange::tree_lower_bound (unsigned i) const
{
  return m_base[i * 2];
}

inline tree
irange::tree_upper_bound (unsigned i) const
{
  return m_base[i * 2 + 1];
}

inline tree
irange::min () const
{
  return tree_lower_bound (0);
}

inline tree
irange::max () const
{
  if (m_num_ranges)
    return tree_upper_bound (m_num_ranges - 1);
  return NULL;
}

inline bool
irange::varying_p () const
{
  if (simple_ranges_p ())
    return m_kind == VR_VARYING;

  return (m_num_ranges == 1
	  && vrp_val_is_min (m_base[0])
	  && vrp_val_is_max (m_base[1]));
}

inline bool
irange::undefined_p () const
{
  if (simple_ranges_p ())
    {
      gcc_checking_assert (m_kind != VR_UNDEFINED || m_num_ranges == 0);
      return m_kind == VR_UNDEFINED;
    }
  return m_num_ranges == 0;
}

inline bool
irange::zero_p () const
{
  if (m_num_ranges == 1
      && integer_zerop (tree_lower_bound (0))
      && integer_zerop (tree_upper_bound (0)))
    {
      gcc_checking_assert (!simple_ranges_p () || m_kind == VR_RANGE);
      return true;
    }
  return false;
}

inline bool
irange::nonzero_p () const
{
  if (undefined_p ())
    return false;

  tree zero = build_zero_cst (type ());
  return *this == int_range<1> (zero, zero, VR_ANTI_RANGE);
}

inline bool
irange::supports_type_p (tree type)
{
  if (type && (INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type)))
    return type;
  return false;
}

inline bool
range_includes_zero_p (const irange *vr)
{
  if (vr->undefined_p ())
    return false;

  if (vr->varying_p ())
    return true;

  return vr->may_contain_p (build_zero_cst (vr->type ()));
}

template<unsigned N>
static inline void
gt_ggc_mx (int_range<N> *x)
{
  for (unsigned i = 0; i < N; ++i)
    {
      gt_ggc_mx (x->m_ranges[i * 2]);
      gt_ggc_mx (x->m_ranges[i * 2 + 1]);
    }
}

template<unsigned N>
static inline void
gt_pch_nx (int_range<N> *x)
{
  for (unsigned i = 0; i < N; ++i)
    {
      gt_pch_nx (x->m_ranges[i * 2]);
      gt_pch_nx (x->m_ranges[i * 2 + 1]);
    }
}

template<unsigned N>
static inline void
gt_pch_nx (int_range<N> *x, gt_pointer_operator op, void *cookie)
{
  for (unsigned i = 0; i < N; ++i)
    {
      op (&x->m_ranges[i * 2], cookie);
      op (&x->m_ranges[i * 2 + 1], cookie);
    }
}

#endif // GCC_VALUE_RANGE_H
