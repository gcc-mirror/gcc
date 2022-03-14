/* Support routines for value ranges.
   Copyright (C) 2019-2022 Free Software Foundation, Inc.
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

class irange;

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

// Discriminator between different vrange types.

enum value_range_discriminator
{
  // Range holds an integer or pointer.
  VR_IRANGE,
  // Range holds an unsupported type.
  VR_UNKNOWN
};

// Abstract class for ranges of any of the supported types.

class vrange
{
  template <typename T> friend bool is_a (vrange &);
public:
  virtual void set (tree, tree, value_range_kind = VR_RANGE) = 0;
  virtual tree type () const = 0;
  virtual void set_varying (tree type) = 0;
  virtual void set_undefined () = 0;
  virtual void dump (FILE * = stderr) const = 0;
  virtual bool union_ (const vrange &) = 0;
  virtual bool intersect (const vrange &) = 0;
  virtual bool singleton_p (tree *result = NULL) const;
  virtual bool contains_p (tree cst) const;
  virtual bool zero_p () const = 0;
  virtual bool nonzero_p () const = 0;
  virtual void set_nonzero (tree type) = 0;
  virtual void set_zero (tree type) = 0;
  virtual void set_nonnegative (tree type) = 0;
  virtual bool fits_p (const vrange &r) const = 0;

  static bool supports_type_p (tree);

  bool varying_p () const;
  bool undefined_p () const;
  vrange& operator= (const vrange &);
  bool operator== (const vrange &) const;
  bool operator!= (const vrange &r) const { return !(*this == r); }

  enum value_range_kind kind () const;		// DEPRECATED
  void debug () const;

protected:
  ENUM_BITFIELD(value_range_kind) m_kind : 8;
  ENUM_BITFIELD(value_range_discriminator) m_discriminator : 4;
};

// An integer range without any storage.

class GTY((user)) irange : public vrange
{
  friend class irange_allocator;
public:
  // In-place setters.
  virtual void set (tree, tree, value_range_kind = VR_RANGE) override;
  virtual void set_nonzero (tree type) override;
  virtual void set_zero (tree type) override;
  virtual void set_nonnegative (tree type) override;
  virtual void set_varying (tree type) override;
  virtual void set_undefined () override;

  // Range types.
  static bool supports_type_p (tree);
  virtual tree type () const override;

  // Iteration over sub-ranges.
  unsigned num_pairs () const;
  wide_int lower_bound (unsigned = 0) const;
  wide_int upper_bound (unsigned) const;
  wide_int upper_bound () const;

  // Predicates.
  virtual bool zero_p () const override;
  virtual bool nonzero_p () const override;
  virtual bool singleton_p (tree *result = NULL) const override;
  virtual bool contains_p (tree cst) const override;

  // In-place operators.
  virtual bool union_ (const vrange &) override;
  virtual bool intersect (const vrange &) override;
  void invert ();

  // Operator overloads.
  irange& operator= (const irange &);
  bool operator== (const irange &) const;
  bool operator!= (const irange &r) const { return !(*this == r); }

  // Misc methods.
  virtual bool fits_p (const vrange &r) const override;
  virtual void dump (FILE * = stderr) const override;

  // Deprecated legacy public methods.
  tree min () const;				// DEPRECATED
  tree max () const;				// DEPRECATED
  bool symbolic_p () const;			// DEPRECATED
  bool constant_p () const;			// DEPRECATED
  void normalize_symbolics ();			// DEPRECATED
  void normalize_addresses ();			// DEPRECATED
  bool may_contain_p (tree) const;		// DEPRECATED
  void set (tree);				// DEPRECATED
  bool equal_p (const irange &) const;		// DEPRECATED
  bool legacy_verbose_union_ (const class irange *);	// DEPRECATED
  bool legacy_verbose_intersect (const irange *);	// DEPRECATED

protected:
  irange (tree *, unsigned);
  // potential promotion to public?
  tree tree_lower_bound (unsigned = 0) const;
  tree tree_upper_bound (unsigned) const;
  tree tree_upper_bound () const;

   // In-place operators.
  bool irange_union (const irange &);
  bool irange_intersect (const irange &);
  void irange_set (tree, tree);
  void irange_set_anti_range (tree, tree);
  bool irange_contains_p (const irange &) const;
  bool irange_single_pair_union (const irange &r);

  void normalize_kind ();

  bool legacy_mode_p () const;
  bool legacy_equal_p (const irange &) const;
  void legacy_union (irange *, const irange *);
  void legacy_intersect (irange *, const irange *);
  void verify_range ();
  wide_int legacy_lower_bound (unsigned = 0) const;
  wide_int legacy_upper_bound (unsigned) const;
  int value_inside_range (tree) const;
  bool maybe_anti_range () const;
  void copy_to_legacy (const irange &);
  void copy_legacy_to_multi_range (const irange &);

private:
  friend void gt_ggc_mx (irange *);
  friend void gt_pch_nx (irange *);
  friend void gt_pch_nx (irange *, gt_pointer_operator, void *);

  void irange_set_1bit_anti_range (tree, tree);
  bool varying_compatible_p () const;

  bool intersect (const wide_int& lb, const wide_int& ub);
  unsigned char m_num_ranges;
  unsigned char m_max_ranges;
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

  // ?? These stubs are for ipa-prop.cc which use a value_range in a
  // hash_traits.  hash-traits.h defines an extern of gt_ggc_mx (T &)
  // instead of picking up the gt_ggc_mx (T *) version.
  friend void gt_ggc_mx (int_range<1> *&);
  friend void gt_pch_nx (int_range<1> *&);

  tree m_ranges[N*2];
};

// Unsupported temporaries may be created by ranger before it's known
// they're unsupported, or by vr_values::get_value_range.  All
// operations except construction cause a trap.

class unsupported_range : public vrange
{
public:
  unsupported_range ();
  virtual void set (tree, tree, value_range_kind) override;
  virtual tree type () const override;
  virtual void set_varying (tree type) override;
  virtual void set_undefined () override;
  virtual void dump (FILE *) const override;
  virtual bool union_ (const vrange &) override;
  virtual bool intersect (const vrange &) override;
  virtual bool zero_p () const override;
  virtual bool nonzero_p () const override;
  virtual void set_nonzero (tree) override;
  virtual void set_zero (tree) override;
  virtual void set_nonnegative (tree) override;
  virtual bool fits_p (const vrange &) const override;
};

// Traits to implement vrange is_a<> and as_a<>.

template<typename T>
struct vrange_traits
{
  // Default to something unusable.
  typedef void range_type;
};

template<>
struct vrange_traits<irange>
{
  typedef irange range_type;
};

template <typename T>
inline bool
is_a (vrange &v)
{
  gcc_unreachable ();
  return false;
}

template <typename T>
inline bool
is_a (const vrange &v)
{
  // Reuse is_a <vrange> to implement the const version.
  const T &derived = static_cast<const T &> (v);
  return is_a <T> (const_cast<T &> (derived));
}

template <typename T>
inline T &
as_a (vrange &v)
{
  typedef typename vrange_traits<T>::range_type range_type;
  gcc_checking_assert (is_a <range_type> (v));
  return static_cast <range_type &> (v);
}

template <typename T>
inline const T &
as_a (const vrange &v)
{
  typedef typename vrange_traits<T>::range_type range_type;
  gcc_checking_assert (is_a <range_type> (v));
  return static_cast <const range_type &> (v);
}

// Specializations for the different range types.

template <>
inline bool
is_a <irange> (vrange &v)
{
  return v.m_discriminator == VR_IRANGE;
}

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
extern void dump_value_range (FILE *, const vrange *);
extern bool vrp_val_is_min (const_tree);
extern bool vrp_val_is_max (const_tree);
extern bool vrp_operand_equal_p (const_tree, const_tree);

inline value_range_kind
vrange::kind () const
{
  return m_kind;
}

// Number of sub-ranges in a range.

inline unsigned
irange::num_pairs () const
{
  if (m_kind == VR_ANTI_RANGE)
    return constant_p () ? 2 : 1;
  else
    return m_num_ranges;
}

inline tree
irange::type () const
{
  gcc_checking_assert (m_num_ranges > 0);
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
irange::varying_compatible_p () const
{
  if (m_num_ranges != 1)
    return false;

  tree l = m_base[0];
  tree u = m_base[1];
  tree t = TREE_TYPE (l);

  if (m_kind == VR_VARYING && t == error_mark_node)
    return true;

  unsigned prec = TYPE_PRECISION (t);
  signop sign = TYPE_SIGN (t);
  if (INTEGRAL_TYPE_P (t))
    return (wi::to_wide (l) == wi::min_value (prec, sign)
	    && wi::to_wide (u) == wi::max_value (prec, sign));
  if (POINTER_TYPE_P (t))
    return (wi::to_wide (l) == 0
	    && wi::to_wide (u) == wi::max_value (prec, sign));
  return true;
}

inline bool
vrange::varying_p () const
{
  return m_kind == VR_VARYING;
}

inline bool
vrange::undefined_p () const
{
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

inline void
gt_ggc_mx (irange *x)
{
  for (unsigned i = 0; i < x->m_num_ranges; ++i)
    {
      gt_ggc_mx (x->m_base[i * 2]);
      gt_ggc_mx (x->m_base[i * 2 + 1]);
    }
}

inline void
gt_pch_nx (irange *x)
{
  for (unsigned i = 0; i < x->m_num_ranges; ++i)
    {
      gt_pch_nx (x->m_base[i * 2]);
      gt_pch_nx (x->m_base[i * 2 + 1]);
    }
}

inline void
gt_pch_nx (irange *x, gt_pointer_operator op, void *cookie)
{
  for (unsigned i = 0; i < x->m_num_ranges; ++i)
    {
      op (&x->m_base[i * 2], NULL, cookie);
      op (&x->m_base[i * 2 + 1], NULL, cookie);
    }
}

template<unsigned N>
inline void
gt_ggc_mx (int_range<N> *x)
{
  gt_ggc_mx ((irange *) x);
}

template<unsigned N>
inline void
gt_pch_nx (int_range<N> *x)
{
  gt_pch_nx ((irange *) x);
}

template<unsigned N>
inline void
gt_pch_nx (int_range<N> *x, gt_pointer_operator op, void *cookie)
{
  gt_pch_nx ((irange *) x, op, cookie);
}

// Constructors for irange

inline
irange::irange (tree *base, unsigned nranges)
{
  m_discriminator = VR_IRANGE;
  m_base = base;
  m_max_ranges = nranges;
  set_undefined ();
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
  m_kind = VR_UNDEFINED;
  m_num_ranges = 0;
}

inline void
irange::set_varying (tree type)
{
  m_kind = VR_VARYING;
  m_num_ranges = 1;

  if (INTEGRAL_TYPE_P (type))
    {
      // Strict enum's require varying to be not TYPE_MIN/MAX, but rather
      // min_value and max_value.
      wide_int min = wi::min_value (TYPE_PRECISION (type), TYPE_SIGN (type));
      wide_int max = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));
      if (wi::eq_p (max, wi::to_wide (TYPE_MAX_VALUE (type)))
	  && wi::eq_p (min, wi::to_wide (TYPE_MIN_VALUE (type))))
	{
	  m_base[0] = TYPE_MIN_VALUE (type);
	  m_base[1] = TYPE_MAX_VALUE (type);
	}
      else
	{
	  m_base[0] = wide_int_to_tree (type, min);
	  m_base[1] = wide_int_to_tree (type, max);
	}
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
  gcc_checking_assert (m_num_ranges > 0);
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
  gcc_checking_assert (m_num_ranges > 0);
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

inline bool
irange::union_ (const vrange &r)
{
  dump_flags_t m_flags = dump_flags;
  dump_flags &= ~TDF_DETAILS;
  bool ret = irange::legacy_verbose_union_ (&as_a <irange> (r));
  dump_flags = m_flags;
  return ret;
}

inline bool
irange::intersect (const vrange &r)
{
  dump_flags_t m_flags = dump_flags;
  dump_flags &= ~TDF_DETAILS;
  bool ret = irange::legacy_verbose_intersect (&as_a <irange> (r));
  dump_flags = m_flags;
  return ret;
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

// Normalize a range to VARYING or UNDEFINED if possible.

inline void
irange::normalize_kind ()
{
  if (m_num_ranges == 0)
    m_kind = VR_UNDEFINED;
  else if (varying_compatible_p ())
    {
      if (m_kind == VR_RANGE)
	m_kind = VR_VARYING;
      else if (m_kind == VR_ANTI_RANGE)
	set_undefined ();
      else
	gcc_unreachable ();
    }
}

inline bool
vrange::supports_type_p (tree type)
{
  return irange::supports_type_p (type);
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
  void *get_memory (unsigned num_bytes);
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

// Provide a hunk of memory from the obstack.
inline void *
irange_allocator::get_memory (unsigned num_bytes)
{
  void *r = obstack_alloc (&m_obstack, num_bytes);
  return r;
}

// Return a new range with NUM_PAIRS.

inline irange *
irange_allocator::allocate (unsigned num_pairs)
{
  // Never allocate 0 pairs.
  // Don't allocate 1 either, or we get legacy value_range's.
  if (num_pairs < 2)
    num_pairs = 2;

  size_t nbytes = sizeof (tree) * 2 * num_pairs;

  // Allocate the irange and required memory for the vector.
  void *r = obstack_alloc (&m_obstack, sizeof (irange));
  tree *mem = (tree *) obstack_alloc (&m_obstack, nbytes);
  return new (r) irange (mem, num_pairs);
}

inline irange *
irange_allocator::allocate (const irange &src)
{
  irange *r = allocate (src.num_pairs ());
  *r = src;
  return r;
}

#endif // GCC_VALUE_RANGE_H
