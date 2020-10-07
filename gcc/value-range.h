/* Support routines for value ranges.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#ifndef GCC_VALUE_RANGE_H
#define GCC_VALUE_RANGE_H

// Types of value ranges.
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
//
// This is the base class without any storage.

class irange
{
  friend class irange_allocator;
public:
  // In-place setters.
  void set (tree, tree, value_range_kind = VR_RANGE);
  void set_nonzero (tree);
  void set_zero (tree);
  void set_varying (tree type);
  void set_undefined ();

  // Range types.
  static bool supports_type_p (tree);
  tree type () const;

  // Iteration over sub-ranges.
  unsigned num_pairs () const;
  wide_int lower_bound (unsigned = 0) const;
  wide_int upper_bound (unsigned) const;
  wide_int upper_bound () const;

  // Predicates.
  bool zero_p () const;
  bool nonzero_p () const;
  bool undefined_p () const;
  bool varying_p () const;
  bool singleton_p (tree *result = NULL) const;
  bool contains_p (tree) const;

  // In-place operators.
  void union_ (const irange &);
  void intersect (const irange &);
  void invert ();

  // Operator overloads.
  irange& operator= (const irange &);
  bool operator== (const irange &) const;
  bool operator!= (const irange &r) const { return !(*this == r); }

  // Misc methods.
  bool fits_p (const irange &r) { return m_max_ranges >= r.num_pairs (); }
  void dump (FILE * = stderr) const;

  // Deprecated legacy public methods.
  enum value_range_kind kind () const;		// DEPRECATED
  tree min () const;				// DEPRECATED
  tree max () const;				// DEPRECATED
  bool symbolic_p () const;			// DEPRECATED
  bool constant_p () const;			// DEPRECATED
  void normalize_symbolics ();			// DEPRECATED
  void normalize_addresses ();			// DEPRECATED
  bool may_contain_p (tree) const;		// DEPRECATED
  void set (tree);				// DEPRECATED
  bool equal_p (const irange &) const;		// DEPRECATED
  void union_ (const class irange *);		// DEPRECATED
  void intersect (const irange *);		// DEPRECATED

protected:
  irange (tree *, unsigned);
  // potential promotion to public?
  tree tree_lower_bound (unsigned = 0) const;
  tree tree_upper_bound (unsigned) const;
  tree tree_upper_bound () const;

   // In-place operators.
  void irange_union (const irange &);
  void irange_intersect (const irange &);
  void irange_set (tree, tree);
  void irange_set_anti_range (tree, tree);

  bool swap_out_of_order_endpoints (tree &min, tree &max, value_range_kind &);
  bool normalize_min_max (tree type, tree min, tree max, value_range_kind);

  bool legacy_mode_p () const;
  bool legacy_equal_p (const irange &) const;
  void legacy_union (irange *, const irange *);
  void legacy_intersect (irange *, const irange *);
  void verify_range ();
  unsigned legacy_num_pairs () const;
  wide_int legacy_lower_bound (unsigned = 0) const;
  wide_int legacy_upper_bound (unsigned) const;
  int value_inside_range (tree) const;
  bool maybe_anti_range () const;
  void copy_legacy_range (const irange &);

private:
  unsigned char m_num_ranges;
  unsigned char m_max_ranges;
  ENUM_BITFIELD(value_range_kind) m_kind : 8;
  tree *m_base;
};

// Here we describe an irange with N pairs of ranges.  The storage for
// the pairs is embedded in the class as an array.

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
  // ?? hash-traits.h has its own extern for these, which is causing
  // them to never be picked up by the templates.  For now, define
  // elsewhere.
  //template<unsigned X> friend void gt_ggc_mx (int_range<X> *&);
  //template<unsigned X> friend void gt_pch_nx (int_range<X> *&);
  friend void gt_ggc_mx (int_range<1> *&);
  friend void gt_pch_nx (int_range<1> *&);

  tree m_ranges[N*2];
};

// This is a special int_range<1> with only one pair, plus
// VR_ANTI_RANGE magic to describe slightly more than can be described
// in one pair.  It is described in the code as a "legacy range" (as
// opposed to multi-ranges which have multiple sub-ranges).  It is
// provided for backward compatibility with code that has not been
// converted to multi-range irange's.
//
// There are copy operators to seamlessly copy to/fro multi-ranges.
typedef int_range<1> value_range;

// This is an "infinite" precision irange for use in temporary
// calculations.
typedef int_range<255> int_range_max;

// Returns true for an old-school value_range as described above.
inline bool
irange::legacy_mode_p () const
{
  return m_max_ranges == 1;
}

extern bool range_has_numeric_bounds_p (const irange *);
extern bool ranges_from_anti_range (const value_range *,
				    value_range *, value_range *);
extern void dump_value_range (FILE *, const irange *);
extern bool vrp_val_is_min (const_tree);
extern bool vrp_val_is_max (const_tree);
extern bool vrp_operand_equal_p (const_tree, const_tree);

inline value_range_kind
irange::kind () const
{
  if (legacy_mode_p ())
    return m_kind;

  if (undefined_p ())
    return VR_UNDEFINED;

  if (varying_p ())
    return VR_VARYING;

  return VR_RANGE;
}

// Number of sub-ranges in a range.

inline unsigned
irange::num_pairs () const
{
  if (!legacy_mode_p ())
    return m_num_ranges;
  else
    return legacy_num_pairs ();
}

inline tree
irange::type () const
{
  gcc_checking_assert (!undefined_p ());
  return TREE_TYPE (m_base[0]);
}

// Return the lower bound of a sub-range expressed as a tree.  PAIR is
// the sub-range in question.

inline tree
irange::tree_lower_bound (unsigned pair) const
{
  return m_base[pair * 2];
}

// Return the upper bound of a sub-range expressed as a tree.  PAIR is
// the sub-range in question.

inline tree
irange::tree_upper_bound (unsigned pair) const
{
  return m_base[pair * 2 + 1];
}

// Return the highest bound of a range expressed as a tree.

inline tree
irange::tree_upper_bound () const
{
  gcc_checking_assert (m_num_ranges);
  return tree_upper_bound (m_num_ranges - 1);
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
    return tree_upper_bound ();
  else
    return NULL;
}

inline bool
irange::varying_p () const
{
  if (legacy_mode_p ())
    return m_kind == VR_VARYING;

  if (m_num_ranges != 1)
    return false;

  tree l = m_base[0];
  tree u = m_base[1];
  tree t = TREE_TYPE (l);
  if (INTEGRAL_TYPE_P (t))
    return l == TYPE_MIN_VALUE (t) && u == TYPE_MAX_VALUE (t);
  if (POINTER_TYPE_P (t))
    return wi::to_wide (l) == 0
	   && wi::to_wide (u) == wi::max_value (TYPE_PRECISION (t),
						TYPE_SIGN (t));
  return true;

}

inline bool
irange::undefined_p () const
{
  if (!legacy_mode_p ())
    return m_num_ranges == 0;

  if (CHECKING_P && legacy_mode_p ())
    {
      if (m_kind == VR_UNDEFINED)
	gcc_checking_assert (m_num_ranges == 0);
      else
	gcc_checking_assert (m_num_ranges != 0);
    }
  return m_kind == VR_UNDEFINED;
}

inline bool
irange::zero_p () const
{
  return (m_kind == VR_RANGE && m_num_ranges == 1
	  && integer_zerop (tree_lower_bound (0))
	  && integer_zerop (tree_upper_bound (0)));
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
inline void
gt_ggc_mx (int_range<N> *x)
{
  for (unsigned i = 0; i < N; ++i)
    {
      gt_ggc_mx (x->m_ranges[i * 2]);
      gt_ggc_mx (x->m_ranges[i * 2 + 1]);
    }
}

template<unsigned N>
inline void
gt_pch_nx (int_range<N> *x)
{
  for (unsigned i = 0; i < N; ++i)
    {
      gt_pch_nx (x->m_ranges[i * 2]);
      gt_pch_nx (x->m_ranges[i * 2 + 1]);
    }
}

template<unsigned N>
inline void
gt_pch_nx (int_range<N> *x, gt_pointer_operator op, void *cookie)
{
  for (unsigned i = 0; i < N; ++i)
    {
      op (&x->m_ranges[i * 2], cookie);
      op (&x->m_ranges[i * 2 + 1], cookie);
    }
}

// Constructors for irange

inline
irange::irange (tree *base, unsigned nranges)
{
  m_base = base;
  m_num_ranges = 0;
  m_max_ranges = nranges;
  if (legacy_mode_p ())
    m_kind = VR_UNDEFINED;
  else
    m_kind = VR_RANGE;
}

// Constructors for int_range<>.

template<unsigned N>
inline
int_range<N>::int_range ()
  : irange (m_ranges, N)
{
}

template<unsigned N>
int_range<N>::int_range (const int_range &other)
  : irange (m_ranges, N)
{
  irange::operator= (other);
}

template<unsigned N>
int_range<N>::int_range (tree min, tree max, value_range_kind kind)
  : irange (m_ranges, N)
{
  irange::set (min, max, kind);
}

template<unsigned N>
int_range<N>::int_range (tree type)
  : irange (m_ranges, N)
{
  set_varying (type);
}

template<unsigned N>
int_range<N>::int_range (tree type, const wide_int &wmin, const wide_int &wmax,
			 value_range_kind kind)
  : irange (m_ranges, N)
{
  tree min = wide_int_to_tree (type, wmin);
  tree max = wide_int_to_tree (type, wmax);
  set (min, max, kind);
}

template<unsigned N>
int_range<N>::int_range (const irange &other)
  : irange (m_ranges, N)
{
  irange::operator= (other);
}

template<unsigned N>
int_range<N>&
int_range<N>::operator= (const int_range &src)
{
  irange::operator= (src);
  return *this;
}

inline void
irange::set (tree val)
{
  set (val, val);
}

inline void
irange::set_undefined ()
{
  m_num_ranges = 0;
  if (legacy_mode_p ())
    m_kind = VR_UNDEFINED;
}

inline void
irange::set_varying (tree type)
{
  if (legacy_mode_p ())
    m_kind = VR_VARYING;

  m_num_ranges = 1;
  if (INTEGRAL_TYPE_P (type))
    {
      m_base[0] = TYPE_MIN_VALUE (type);
      m_base[1] = TYPE_MAX_VALUE (type);
    }
  else if (POINTER_TYPE_P (type))
    {
      m_base[0] = build_int_cst (type, 0);
      m_base[1] = build_int_cst (type, -1);
    }
  else
    m_base[0] = m_base[1] = error_mark_node;
}

inline bool
irange::operator== (const irange &r) const
{
  return equal_p (r);
}

// Return the lower bound of a sub-range.  PAIR is the sub-range in
// question.

inline wide_int
irange::lower_bound (unsigned pair) const
{
  if (legacy_mode_p ())
    return legacy_lower_bound (pair);
  gcc_checking_assert (!undefined_p ());
  gcc_checking_assert (pair + 1 <= num_pairs ());
  return wi::to_wide (tree_lower_bound (pair));
}

// Return the upper bound of a sub-range.  PAIR is the sub-range in
// question.

inline wide_int
irange::upper_bound (unsigned pair) const
{
  if (legacy_mode_p ())
    return legacy_upper_bound (pair);
  gcc_checking_assert (!undefined_p ());
  gcc_checking_assert (pair + 1 <= num_pairs ());
  return wi::to_wide (tree_upper_bound (pair));
}

// Return the highest bound of a range.

inline wide_int
irange::upper_bound () const
{
  unsigned pairs = num_pairs ();
  gcc_checking_assert (pairs > 0);
  return upper_bound (pairs - 1);
}

inline void
irange::union_ (const irange &r)
{
  dump_flags_t m_flags = dump_flags;
  dump_flags &= ~TDF_DETAILS;
  irange::union_ (&r);
  dump_flags = m_flags;
}

inline void
irange::intersect (const irange &r)
{
  dump_flags_t m_flags = dump_flags;
  dump_flags &= ~TDF_DETAILS;
  irange::intersect (&r);
  dump_flags = m_flags;
}

// Set value range VR to a nonzero range of type TYPE.

inline void
irange::set_nonzero (tree type)
{
  tree zero = build_int_cst (type, 0);
  if (legacy_mode_p ())
    set (zero, zero, VR_ANTI_RANGE);
  else
    irange_set_anti_range (zero, zero);
}

// Set value range VR to a ZERO range of type TYPE.

inline void
irange::set_zero (tree type)
{
  tree z = build_int_cst (type, 0);
  if (legacy_mode_p ())
    set (z);
  else
    irange_set (z, z);
}

// Normalize [MIN, MAX] into VARYING and ~[MIN, MAX] into UNDEFINED.
//
// Avoid using TYPE_{MIN,MAX}_VALUE because -fstrict-enums can
// restrict those to a subset of what actually fits in the type.
// Instead use the extremes of the type precision which will allow
// compare_range_with_value() to check if a value is inside a range,
// whereas if we used TYPE_*_VAL, said function would just punt upon
// seeing a VARYING.

inline bool
irange::normalize_min_max (tree type, tree min, tree max,
			   value_range_kind kind)
{
  unsigned prec = TYPE_PRECISION (type);
  signop sign = TYPE_SIGN (type);
  if (wi::eq_p (wi::to_wide (min), wi::min_value (prec, sign))
      && wi::eq_p (wi::to_wide (max), wi::max_value (prec, sign)))
    {
      if (kind == VR_RANGE)
	set_varying (type);
      else if (kind == VR_ANTI_RANGE)
	set_undefined ();
      else
	gcc_unreachable ();
      return true;
    }
  return false;
}

// Return the maximum value for TYPE.

inline tree
vrp_val_max (const_tree type)
{
  if (INTEGRAL_TYPE_P (type))
    return TYPE_MAX_VALUE (type);
  if (POINTER_TYPE_P (type))
    {
      wide_int max = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));
      return wide_int_to_tree (const_cast<tree> (type), max);
    }
  return NULL_TREE;
}

// Return the minimum value for TYPE.

inline tree
vrp_val_min (const_tree type)
{
  if (INTEGRAL_TYPE_P (type))
    return TYPE_MIN_VALUE (type);
  if (POINTER_TYPE_P (type))
    return build_zero_cst (const_cast<tree> (type));
  return NULL_TREE;
}

// This is the irange storage class.  It is used to allocate the
// minimum amount of storage needed for a given irange.  Storage is
// automatically freed at destruction of the storage class.
//
// It is meant for long term storage, as opposed to int_range_max
// which is meant for intermediate temporary results on the stack.
//
// The newly allocated irange is initialized to the empty set
// (undefined_p() is true).

class irange_allocator
{
public:
  irange_allocator ();
  ~irange_allocator ();
  // Return a new range with NUM_PAIRS.
  irange *allocate (unsigned num_pairs);
  // Return a copy of SRC with the minimum amount of sub-ranges needed
  // to represent it.
  irange *allocate (const irange &src);
private:
  DISABLE_COPY_AND_ASSIGN (irange_allocator);
  struct obstack m_obstack;
};

inline
irange_allocator::irange_allocator ()
{
  obstack_init (&m_obstack);
}

inline
irange_allocator::~irange_allocator ()
{
  obstack_free (&m_obstack, NULL);
}

// Return a new range with NUM_PAIRS.

inline irange *
irange_allocator::allocate (unsigned num_pairs)
{
  // Never allocate 0 pairs.
  // Don't allocate 1 either, or we get legacy value_range's.
  if (num_pairs < 2)
    num_pairs = 2;

  struct newir {
    irange range;
    tree mem[2];
  };
  size_t nbytes = (sizeof (newir) + sizeof (tree) * 2 * (num_pairs - 1));
  struct newir *r = (newir *) obstack_alloc (&m_obstack, nbytes);
  return new (r) irange (r->mem, num_pairs);
}

inline irange *
irange_allocator::allocate (const irange &src)
{
  irange *r = allocate (src.num_pairs ());
  *r = src;
  return r;
}

#endif // GCC_VALUE_RANGE_H
