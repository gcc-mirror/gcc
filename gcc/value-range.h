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
  // Floating point range.
  VR_FRANGE,
  // Range holds an unsupported type.
  VR_UNKNOWN
};

// Abstract class for ranges of any of the supported types.
//
// To query what types ranger and the entire ecosystem can support,
// use Value_Range::supports_type_p(tree type).  This is a static
// method available independently of any vrange object.
//
// To query what a given vrange variant can support, use:
//    irange::supports_p ()
//    frange::supports_p ()
//    etc
//
// To query what a range object can support, use:
//    void foo (vrange &v, irange &i, frange &f)
//    {
//	if (v.supports_type_p (type)) ...
//	if (i.supports_type_p (type)) ...
//	if (f.supports_type_p (type)) ...
//    }

class vrange
{
  template <typename T> friend bool is_a (vrange &);
  friend class Value_Range;
public:
  virtual void accept (const class vrange_visitor &v) const = 0;
  virtual void set (tree, tree, value_range_kind = VR_RANGE);
  virtual tree type () const;
  virtual bool supports_type_p (const_tree type) const;
  virtual void set_varying (tree type);
  virtual void set_undefined ();
  virtual bool union_ (const vrange &);
  virtual bool intersect (const vrange &);
  virtual bool singleton_p (tree *result = NULL) const;
  virtual bool contains_p (tree cst) const;
  virtual bool zero_p () const;
  virtual bool nonzero_p () const;
  virtual void set_nonzero (tree type);
  virtual void set_zero (tree type);
  virtual void set_nonnegative (tree type);
  virtual bool fits_p (const vrange &r) const;

  bool varying_p () const;
  bool undefined_p () const;
  vrange& operator= (const vrange &);
  bool operator== (const vrange &) const;
  bool operator!= (const vrange &r) const { return !(*this == r); }
  void dump (FILE *) const;

  enum value_range_kind kind () const;		// DEPRECATED

protected:
  ENUM_BITFIELD(value_range_kind) m_kind : 8;
  ENUM_BITFIELD(value_range_discriminator) m_discriminator : 4;
};

// An integer range without any storage.

class GTY((user)) irange : public vrange
{
  friend class vrange_allocator;
  friend class irange_storage_slot; // For legacy_mode_p checks.
public:
  // In-place setters.
  virtual void set (tree, tree, value_range_kind = VR_RANGE) override;
  virtual void set_nonzero (tree type) override;
  virtual void set_zero (tree type) override;
  virtual void set_nonnegative (tree type) override;
  virtual void set_varying (tree type) override;
  virtual void set_undefined () override;

  // Range types.
  static bool supports_p (const_tree type);
  virtual bool supports_type_p (const_tree type) const override;
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
  virtual void accept (const vrange_visitor &v) const override;

  // Nonzero masks.
  wide_int get_nonzero_bits () const;
  void set_nonzero_bits (const wide_int_ref &bits);

  // Deprecated legacy public methods.
  tree min () const;				// DEPRECATED
  tree max () const;				// DEPRECATED
  bool symbolic_p () const;			// DEPRECATED
  bool constant_p () const;			// DEPRECATED
  void normalize_symbolics ();			// DEPRECATED
  void normalize_addresses ();			// DEPRECATED
  bool may_contain_p (tree) const;		// DEPRECATED
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
  void set_nonzero_bits (tree mask);
  bool intersect_nonzero_bits (const irange &r);
  bool union_nonzero_bits (const irange &r);

  bool intersect (const wide_int& lb, const wide_int& ub);
  unsigned char m_num_ranges;
  unsigned char m_max_ranges;
  tree m_nonzero_mask;
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
  virtual ~int_range () = default;
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
// they're unsupported, or by vr_values::get_value_range.

class unsupported_range : public vrange
{
public:
  unsupported_range ()
  {
    m_discriminator = VR_UNKNOWN;
    set_undefined ();
  }
  virtual void set_undefined () final override
  {
    m_kind = VR_UNDEFINED;
  }
  virtual void accept (const vrange_visitor &v) const override;
};

// Floating point property to represent possible values of a NAN, INF, etc.

class fp_prop
{
public:
  enum kind {
    UNDEFINED	= 0x0,		// Prop is impossible.
    YES		= 0x1,		// Prop is definitely set.
    NO		= 0x2,		// Prop is definitely not set.
    VARYING	= (YES | NO)	// Prop may hold.
  };
  fp_prop (kind f) : m_kind (f) { }
  bool varying_p () const { return m_kind == VARYING; }
  bool undefined_p () const { return m_kind == UNDEFINED; }
  bool yes_p () const { return m_kind == YES; }
  bool no_p () const { return m_kind == NO; }
private:
  unsigned char m_kind : 2;
};

// Accessors for individual FP properties.

#define FP_PROP_ACCESSOR(NAME) \
  void NAME##_set_varying () { u.bits.NAME = fp_prop::VARYING; }	\
  void NAME##_set_yes () { u.bits.NAME = fp_prop::YES; }	\
  void NAME##_set_no () { u.bits.NAME = fp_prop::NO; }	\
  bool NAME##_varying_p () const { return u.bits.NAME == fp_prop::VARYING; } \
  bool NAME##_undefined_p () const { return u.bits.NAME == fp_prop::UNDEFINED; } \
  bool NAME##_yes_p () const { return u.bits.NAME == fp_prop::YES; }	\
  bool NAME##_no_p () const { return u.bits.NAME == fp_prop::NO; } \
  fp_prop get_##NAME () const				   \
  { return fp_prop ((fp_prop::kind) u.bits.NAME); } \
  void set_##NAME (fp_prop::kind f) { u.bits.NAME = f; }

// Aggregate of all the FP properties in an frange packed into one
// structure to save space.  Using explicit fp_prop's in the frange,
// would take one byte per property because of padding.  Instead, we
// can save all properties into one byte.

class frange_props
{
public:
  frange_props () { set_varying (); }
  void set_varying () { u.bytes = 0xff; }
  void set_undefined () { u.bytes = 0; }
  bool varying_p () { return u.bytes == 0xff; }
  bool undefined_p () { return u.bytes == 0; }
  bool union_ (const frange_props &other);
  bool intersect (const frange_props &other);
  bool operator== (const frange_props &other) const;
  FP_PROP_ACCESSOR(nan)
  FP_PROP_ACCESSOR(inf)
  FP_PROP_ACCESSOR(ninf)
private:
  union {
    struct {
      unsigned char nan : 2;
      unsigned char inf : 2;
      unsigned char ninf : 2;
    } bits;
    unsigned char bytes;
  } u;
};

// Accessors for getting/setting all FP properties at once.

#define FRANGE_PROP_ACCESSOR(NAME)				\
  fp_prop get_##NAME () const { return m_props.get_##NAME (); }	\
  void set_##NAME (fp_prop::kind f)				\
  {								\
    m_props.set_##NAME (f);					\
    normalize_kind ();						\
  }

// A floating point range.

class frange : public vrange
{
  friend class frange_storage_slot;
public:
  frange ();
  frange (const frange &);
  static bool supports_p (const_tree type)
  {
    return SCALAR_FLOAT_TYPE_P (type);
  }
  virtual tree type () const override;
  virtual void set (tree, tree, value_range_kind = VR_RANGE) override;
  virtual void set_varying (tree type) override;
  virtual void set_undefined () override;
  virtual bool union_ (const vrange &) override;
  virtual bool intersect (const vrange &) override;
  virtual bool supports_type_p (const_tree type) const override;
  virtual void accept (const vrange_visitor &v) const override;
  frange& operator= (const frange &);
  bool operator== (const frange &) const;
  bool operator!= (const frange &r) const { return !(*this == r); }

  // Each fp_prop can be accessed with get_PROP() and set_PROP().
  FRANGE_PROP_ACCESSOR(nan)
  FRANGE_PROP_ACCESSOR(inf)
  FRANGE_PROP_ACCESSOR(ninf)
private:
  void verify_range ();
  bool normalize_kind ();

  frange_props m_props;
  tree m_type;
};

// is_a<> and as_a<> implementation for vrange.

// Anything we haven't specialized is a hard fail.
template <typename T>
inline bool
is_a (vrange &)
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
  gcc_checking_assert (is_a <T> (v));
  return static_cast <T &> (v);
}

template <typename T>
inline const T &
as_a (const vrange &v)
{
  gcc_checking_assert (is_a <T> (v));
  return static_cast <const T &> (v);
}

// Specializations for the different range types.

template <>
inline bool
is_a <irange> (vrange &v)
{
  return v.m_discriminator == VR_IRANGE;
}

template <>
inline bool
is_a <frange> (vrange &v)
{
  return v.m_discriminator == VR_FRANGE;
}

class vrange_visitor
{
public:
  virtual void visit (const irange &) const { }
  virtual void visit (const frange &) const { }
  virtual void visit (const unsupported_range &) const { }
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

// This is an "infinite" precision range object for use in temporary
// calculations for any of the handled types.  The object can be
// transparently used as a vrange.

class Value_Range
{
public:
  Value_Range ();
  Value_Range (const vrange &r);
  Value_Range (tree type);
  Value_Range (const Value_Range &);
  void set_type (tree type);
  vrange& operator= (const vrange &);
  bool operator== (const Value_Range &r) const;
  bool operator!= (const Value_Range &r) const;
  operator vrange &();
  operator const vrange &() const;
  void dump (FILE *) const;
  static bool supports_type_p (const_tree type);

  // Convenience methods for vrange compatability.
  void set (tree min, tree max, value_range_kind kind = VR_RANGE)
    { return m_vrange->set (min, max, kind); }
  tree type () { return m_vrange->type (); }
  enum value_range_kind kind () { return m_vrange->kind (); }
  bool varying_p () const { return m_vrange->varying_p (); }
  bool undefined_p () const { return m_vrange->undefined_p (); }
  void set_varying (tree type) { m_vrange->set_varying (type); }
  void set_undefined () { m_vrange->set_undefined (); }
  bool union_ (const vrange &r) { return m_vrange->union_ (r); }
  bool intersect (const vrange &r) { return m_vrange->intersect (r); }
  bool singleton_p (tree *result = NULL) const
    { return m_vrange->singleton_p (result); }
  bool zero_p () const { return m_vrange->zero_p (); }
  wide_int lower_bound () const; // For irange/prange compatability.
  wide_int upper_bound () const; // For irange/prange compatability.
  void accept (const vrange_visitor &v) const { m_vrange->accept (v); }
private:
  void init (tree type);
  unsupported_range m_unsupported;
  vrange *m_vrange;
  int_range_max m_irange;
  frange m_frange;
};

inline
Value_Range::Value_Range ()
{
  m_vrange = &m_unsupported;
}

// Copy constructor from a vrange.

inline
Value_Range::Value_Range (const vrange &r)
{
  *this = r;
}

// Copy constructor from a TYPE.  The range of the temporary is set to
// UNDEFINED.

inline
Value_Range::Value_Range (tree type)
{
  init (type);
}

inline
Value_Range::Value_Range (const Value_Range &r)
{
  m_vrange = r.m_vrange;
}

// Initialize object so it is possible to store temporaries of TYPE
// into it.

inline void
Value_Range::init (tree type)
{
  gcc_checking_assert (TYPE_P (type));

  if (irange::supports_p (type))
    m_vrange = &m_irange;
  else if (frange::supports_p (type))
    m_vrange = &m_frange;
  else
    m_vrange = &m_unsupported;
}

// Set the temporary to allow storing temporaries of TYPE.  The range
// of the temporary is set to UNDEFINED.

inline void
Value_Range::set_type (tree type)
{
  init (type);
  m_vrange->set_undefined ();
}

// Assignment operator for temporaries.  Copying incompatible types is
// allowed.

inline vrange &
Value_Range::operator= (const vrange &r)
{
  if (is_a <irange> (r))
    {
      m_irange = as_a <irange> (r);
      m_vrange = &m_irange;
    }
  else if (is_a <frange> (r))
    {
      m_frange = as_a <frange> (r);
      m_vrange = &m_frange;
    }
  else
    gcc_unreachable ();

  return *m_vrange;
}

inline bool
Value_Range::operator== (const Value_Range &r) const
{
  return *m_vrange == *r.m_vrange;
}

inline bool
Value_Range::operator!= (const Value_Range &r) const
{
  return *m_vrange != *r.m_vrange;
}

inline
Value_Range::operator vrange &()
{
  return *m_vrange;
}

inline
Value_Range::operator const vrange &() const
{
  return *m_vrange;
}

// Return TRUE if TYPE is supported by the vrange infrastructure.

inline bool
Value_Range::supports_type_p (const_tree type)
{
  return irange::supports_p (type) || frange::supports_p (type);
}

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
	    && wi::to_wide (u) == wi::max_value (prec, sign)
	    && !m_nonzero_mask);
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
irange::supports_p (const_tree type)
{
  return INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type);
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
  if (x->m_nonzero_mask)
    gt_ggc_mx (x->m_nonzero_mask);
}

inline void
gt_pch_nx (irange *x)
{
  for (unsigned i = 0; i < x->m_num_ranges; ++i)
    {
      gt_pch_nx (x->m_base[i * 2]);
      gt_pch_nx (x->m_base[i * 2 + 1]);
    }
  if (x->m_nonzero_mask)
    gt_pch_nx (x->m_nonzero_mask);
}

inline void
gt_pch_nx (irange *x, gt_pointer_operator op, void *cookie)
{
  for (unsigned i = 0; i < x->m_num_ranges; ++i)
    {
      op (&x->m_base[i * 2], NULL, cookie);
      op (&x->m_base[i * 2 + 1], NULL, cookie);
    }
  if (x->m_nonzero_mask)
    op (&x->m_nonzero_mask, NULL, cookie);
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
irange::set_undefined ()
{
  m_kind = VR_UNDEFINED;
  m_num_ranges = 0;
  m_nonzero_mask = NULL;
}

inline void
irange::set_varying (tree type)
{
  m_kind = VR_VARYING;
  m_num_ranges = 1;
  m_nonzero_mask = NULL;

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
    set (z, z);
  else
    irange_set (z, z);
}

// Normalize a range to VARYING or UNDEFINED if possible.

inline void
irange::normalize_kind ()
{
  if (m_num_ranges == 0)
    set_undefined ();
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
  if (frange::supports_p (type))
    {
      REAL_VALUE_TYPE real;
      real_inf (&real);
      return build_real (const_cast <tree> (type), real);
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
  if (frange::supports_p (type))
    {
      REAL_VALUE_TYPE real, real_ninf;
      real_inf (&real);
      real_ninf = real_value_negate (&real);
      return build_real (const_cast <tree> (type), real_ninf);
    }
  return NULL_TREE;
}

// Supporting methods for frange.

inline bool
frange_props::operator== (const frange_props &other) const
{
  return u.bytes == other.u.bytes;
}

inline bool
frange_props::union_ (const frange_props &other)
{
  unsigned char saved = u.bytes;
  u.bytes |= other.u.bytes;
  return u.bytes != saved;
}

inline bool
frange_props::intersect (const frange_props &other)
{
  unsigned char saved = u.bytes;
  u.bytes &= other.u.bytes;
  return u.bytes != saved;
}

inline
frange::frange ()
{
  m_discriminator = VR_FRANGE;
  set_undefined ();
}

inline
frange::frange (const frange &src)
{
  m_discriminator = VR_FRANGE;
  *this = src;
}

inline tree
frange::type () const
{
  return m_type;
}

inline void
frange::set_varying (tree type)
{
  m_kind = VR_VARYING;
  m_type = type;
  m_props.set_varying ();
}

inline void
frange::set_undefined ()
{
  m_kind = VR_UNDEFINED;
  m_type = NULL;
  m_props.set_undefined ();
}

#endif // GCC_VALUE_RANGE_H
