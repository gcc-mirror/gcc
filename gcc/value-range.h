/* Support routines for value ranges.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.
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
  /* Range is a NAN.  */
  VR_NAN,
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

class GTY((user)) vrange
{
  template <typename T> friend bool is_a (vrange &);
  friend class Value_Range;
  friend void streamer_write_vrange (struct output_block *, const vrange &);
  friend class range_op_handler;
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
protected:
  vrange (enum value_range_discriminator d) : m_discriminator (d) { }
  ENUM_BITFIELD(value_range_kind) m_kind : 8;
  const ENUM_BITFIELD(value_range_discriminator) m_discriminator : 4;
};

namespace inchash
{
  extern void add_vrange (const vrange &, hash &, unsigned flags = 0);
}

// A pair of values representing the known bits in a range.  Zero bits
// in MASK cover constant values.  Set bits in MASK cover unknown
// values.  VALUE are the known bits.
//
// Set bits in MASK (no meaningful information) must have their
// corresponding bits in VALUE cleared, as this speeds up union and
// intersect.

class irange_bitmask
{
public:
  irange_bitmask () { /* uninitialized */ }
  irange_bitmask (unsigned prec) { set_unknown (prec); }
  irange_bitmask (const wide_int &value, const wide_int &mask);
  wide_int value () const { return m_value; }
  wide_int mask () const { return m_mask; }
  void set_unknown (unsigned prec);
  bool unknown_p () const;
  unsigned get_precision () const;
  bool union_ (const irange_bitmask &src);
  bool intersect (const irange_bitmask &src);
  bool operator== (const irange_bitmask &src) const;
  bool operator!= (const irange_bitmask &src) const { return !(*this == src); }
  void verify_mask () const;
  void dump (FILE *) const;

  // Convenience functions for nonzero bitmask compatibility.
  wide_int get_nonzero_bits () const;
  void set_nonzero_bits (const wide_int &bits);
private:
  wide_int m_value;
  wide_int m_mask;
};

inline void
irange_bitmask::set_unknown (unsigned prec)
{
  m_value = wi::zero (prec);
  m_mask = wi::minus_one (prec);
  if (flag_checking)
    verify_mask ();
}

// Return TRUE if THIS does not have any meaningful information.

inline bool
irange_bitmask::unknown_p () const
{
  return m_mask == -1;
}

inline
irange_bitmask::irange_bitmask (const wide_int &value, const wide_int &mask)
{
  m_value = value;
  m_mask = mask;
  if (flag_checking)
    verify_mask ();
}

inline unsigned
irange_bitmask::get_precision () const
{
  return m_mask.get_precision ();
}

// The following two functions are meant for backwards compatability
// with the nonzero bitmask.  A cleared bit means the value must be 0.
// A set bit means we have no information for the bit.

// Return the nonzero bits.
inline wide_int
irange_bitmask::get_nonzero_bits () const
{
  return m_value | m_mask;
}

// Set the bitmask to the nonzero bits in BITS.
inline void
irange_bitmask::set_nonzero_bits (const wide_int &bits)
{
  m_value = wi::zero (bits.get_precision ());
  m_mask = bits;
  if (flag_checking)
    verify_mask ();
}

inline bool
irange_bitmask::operator== (const irange_bitmask &src) const
{
  bool unknown1 = unknown_p ();
  bool unknown2 = src.unknown_p ();
  if (unknown1 || unknown2)
    return unknown1 == unknown2;
  return m_value == src.m_value && m_mask == src.m_mask;
}

inline bool
irange_bitmask::union_ (const irange_bitmask &orig_src)
{
  // Normalize mask.
  irange_bitmask src (orig_src.m_value & ~orig_src.m_mask, orig_src.m_mask);
  m_value &= ~m_mask;

  irange_bitmask save (*this);
  m_mask = (m_mask | src.m_mask) | (m_value ^ src.m_value);
  m_value = m_value & src.m_value;
  if (flag_checking)
    verify_mask ();
  return *this != save;
}

inline bool
irange_bitmask::intersect (const irange_bitmask &orig_src)
{
  // Normalize mask.
  irange_bitmask src (orig_src.m_value & ~orig_src.m_mask, orig_src.m_mask);
  m_value &= ~m_mask;

  irange_bitmask save (*this);
  // If we have two known bits that are incompatible, the resulting
  // bit is undefined.  It is unclear whether we should set the entire
  // range to UNDEFINED, or just a subset of it.  For now, set the
  // entire bitmask to unknown (VARYING).
  if (wi::bit_and (~(m_mask | src.m_mask),
		   m_value ^ src.m_value) != 0)
    {
      unsigned prec = m_mask.get_precision ();
      m_mask = wi::minus_one (prec);
      m_value = wi::zero (prec);
    }
  else
    {
      m_mask = m_mask & src.m_mask;
      m_value = m_value | src.m_value;
    }
  if (flag_checking)
    verify_mask ();
  return *this != save;
}

// An integer range without any storage.

class GTY((user)) irange : public vrange
{
  friend value_range_kind get_legacy_range (const irange &, tree &, tree &);
  friend class irange_storage;
  friend class vrange_printer;
public:
  // In-place setters.
  void set (tree type, const wide_int &, const wide_int &,
	    value_range_kind = VR_RANGE);
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
  bool singleton_p (wide_int &) const;
  bool contains_p (const wide_int &) const;

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

  void update_bitmask (const irange_bitmask &);
  irange_bitmask get_bitmask () const;
  // Nonzero masks.
  wide_int get_nonzero_bits () const;
  void set_nonzero_bits (const wide_int &bits);

protected:
  void maybe_resize (int needed);
  virtual void set (tree, tree, value_range_kind = VR_RANGE) override;
  virtual bool contains_p (tree cst) const override;
  irange (wide_int *, unsigned nranges, bool resizable);

   // In-place operators.
  bool irange_contains_p (const irange &) const;
  bool irange_single_pair_union (const irange &r);

  void normalize_kind ();

  void verify_range ();

  // Hard limit on max ranges allowed.
  static const int HARD_MAX_RANGES = 255;
private:
  friend void gt_ggc_mx (irange *);
  friend void gt_pch_nx (irange *);
  friend void gt_pch_nx (irange *, gt_pointer_operator, void *);

  bool varying_compatible_p () const;
  bool intersect_bitmask (const irange &r);
  bool union_bitmask (const irange &r);
  irange_bitmask get_bitmask_from_range () const;
  bool set_range_from_bitmask ();

  bool intersect (const wide_int& lb, const wide_int& ub);
  unsigned char m_num_ranges;
  bool m_resizable;
  unsigned char m_max_ranges;
  tree m_type;
  irange_bitmask m_bitmask;
protected:
  wide_int *m_base;
};

// Here we describe an irange with N pairs of ranges.  The storage for
// the pairs is embedded in the class as an array.
//
// If RESIZABLE is true, the storage will be resized on the heap when
// the number of ranges needed goes past N up to a max of
// HARD_MAX_RANGES.  This new storage is freed upon destruction.

template<unsigned N, bool RESIZABLE = false>
class GTY((user)) int_range : public irange
{
public:
  int_range ();
  int_range (tree type, const wide_int &, const wide_int &,
	     value_range_kind = VR_RANGE);
  int_range (tree type);
  int_range (const int_range &);
  int_range (const irange &);
  virtual ~int_range ();
  int_range& operator= (const int_range &);
protected:
  int_range (tree, tree, value_range_kind = VR_RANGE);
private:
  wide_int m_ranges[N*2];
};

// Unsupported temporaries may be created by ranger before it's known
// they're unsupported, or by vr_values::get_value_range.

class unsupported_range : public vrange
{
public:
  unsupported_range ()
    : vrange (VR_UNKNOWN)
  {
    set_undefined ();
  }
  virtual void set_undefined () final override
  {
    m_kind = VR_UNDEFINED;
  }
  virtual void accept (const vrange_visitor &v) const override;
};

// The NAN state as an opaque object.

class nan_state
{
public:
  nan_state (bool);
  nan_state (bool pos_nan, bool neg_nan);
  bool neg_p () const;
  bool pos_p () const;
private:
  bool m_pos_nan;
  bool m_neg_nan;
};

// Set NAN state to +-NAN if NAN_P is true.  Otherwise set NAN state
// to false.

inline
nan_state::nan_state (bool nan_p)
{
  m_pos_nan = nan_p;
  m_neg_nan = nan_p;
}

// Constructor initializing the object to +NAN if POS_NAN is set, -NAN
// if NEG_NAN is set, or +-NAN if both are set.  Otherwise POS_NAN and
// NEG_NAN are clear, and the object cannot be a NAN.

inline
nan_state::nan_state (bool pos_nan, bool neg_nan)
{
  m_pos_nan = pos_nan;
  m_neg_nan = neg_nan;
}

// Return if +NAN is possible.

inline bool
nan_state::pos_p () const
{
  return m_pos_nan;
}

// Return if -NAN is possible.

inline bool
nan_state::neg_p () const
{
  return m_neg_nan;
}

// A floating point range.
//
// The representation is a type with a couple of endpoints, unioned
// with the set of { -NAN, +Nan }.

class GTY((user)) frange : public vrange
{
  friend class frange_storage;
  friend class vrange_printer;
  friend void gt_ggc_mx (frange *);
  friend void gt_pch_nx (frange *);
  friend void gt_pch_nx (frange *, gt_pointer_operator, void *);
public:
  frange ();
  frange (const frange &);
  frange (tree, tree, value_range_kind = VR_RANGE);
  frange (tree type);
  frange (tree type, const REAL_VALUE_TYPE &min, const REAL_VALUE_TYPE &max,
	  value_range_kind = VR_RANGE);
  static bool supports_p (const_tree type)
  {
    // ?? Decimal floats can have multiple representations for the
    // same number.  Supporting them may be as simple as just
    // disabling them in singleton_p.  No clue.
    return SCALAR_FLOAT_TYPE_P (type) && !DECIMAL_FLOAT_TYPE_P (type);
  }
  virtual tree type () const override;
  void set (tree type, const REAL_VALUE_TYPE &, const REAL_VALUE_TYPE &,
	    value_range_kind = VR_RANGE);
  void set (tree type, const REAL_VALUE_TYPE &, const REAL_VALUE_TYPE &,
	    const nan_state &, value_range_kind = VR_RANGE);
  void set_nan (tree type);
  void set_nan (tree type, bool sign);
  void set_nan (tree type, const nan_state &);
  virtual void set_varying (tree type) override;
  virtual void set_undefined () override;
  virtual bool union_ (const vrange &) override;
  virtual bool intersect (const vrange &) override;
  bool contains_p (const REAL_VALUE_TYPE &) const;
  virtual bool singleton_p (tree *result = NULL) const override;
  bool singleton_p (REAL_VALUE_TYPE &r) const;
  virtual bool supports_type_p (const_tree type) const override;
  virtual void accept (const vrange_visitor &v) const override;
  virtual bool zero_p () const override;
  virtual bool nonzero_p () const override;
  virtual void set_nonzero (tree type) override;
  virtual void set_zero (tree type) override;
  virtual void set_nonnegative (tree type) override;
  frange& operator= (const frange &);
  bool operator== (const frange &) const;
  bool operator!= (const frange &r) const { return !(*this == r); }
  const REAL_VALUE_TYPE &lower_bound () const;
  const REAL_VALUE_TYPE &upper_bound () const;
  nan_state get_nan_state () const;
  void update_nan ();
  void update_nan (bool sign);
  void update_nan (tree) = delete; // Disallow silent conversion to bool.
  void update_nan (const nan_state &);
  void clear_nan ();
  void flush_denormals_to_zero ();

  // fpclassify like API
  bool known_isfinite () const;
  bool known_isnan () const;
  bool known_isinf () const;
  bool maybe_isnan () const;
  bool maybe_isnan (bool sign) const;
  bool maybe_isinf () const;
  bool signbit_p (bool &signbit) const;
  bool nan_signbit_p (bool &signbit) const;

protected:
  virtual bool contains_p (tree cst) const override;
  virtual void set (tree, tree, value_range_kind = VR_RANGE) override;

private:
  bool internal_singleton_p (REAL_VALUE_TYPE * = NULL) const;
  void verify_range ();
  bool normalize_kind ();
  bool union_nans (const frange &);
  bool intersect_nans (const frange &);
  bool combine_zeros (const frange &, bool union_p);

  tree m_type;
  REAL_VALUE_TYPE m_min;
  REAL_VALUE_TYPE m_max;
  bool m_pos_nan;
  bool m_neg_nan;
};

inline const REAL_VALUE_TYPE &
frange::lower_bound () const
{
  gcc_checking_assert (!undefined_p () && !known_isnan ());
  return m_min;
}

inline const REAL_VALUE_TYPE &
frange::upper_bound () const
{
  gcc_checking_assert (!undefined_p () && !known_isnan ());
  return m_max;
}

// Return the NAN state.

inline nan_state
frange::get_nan_state () const
{
  return nan_state (m_pos_nan, m_neg_nan);
}

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

template <>
inline bool
is_a <unsupported_range> (vrange &v)
{
  return v.m_discriminator == VR_UNKNOWN;
}

// For resizable ranges, resize the range up to HARD_MAX_RANGES if the
// NEEDED pairs is greater than the current capacity of the range.

inline void
irange::maybe_resize (int needed)
{
  if (!m_resizable || m_max_ranges == HARD_MAX_RANGES)
    return;

  if (needed > m_max_ranges)
    {
      m_max_ranges = HARD_MAX_RANGES;
      wide_int *newmem = new wide_int[m_max_ranges * 2];
      memcpy (newmem, m_base, sizeof (wide_int) * num_pairs () * 2);
      m_base = newmem;
    }
}

template<unsigned N, bool RESIZABLE>
inline
int_range<N, RESIZABLE>::~int_range ()
{
  if (RESIZABLE && m_base != m_ranges)
    delete[] m_base;
}

// This is an "infinite" precision irange for use in temporary
// calculations.  It starts with a sensible default covering 99% of
// uses, and goes up to HARD_MAX_RANGES when needed.  Any allocated
// storage is freed upon destruction.
typedef int_range<3, /*RESIZABLE=*/true> int_range_max;

class vrange_visitor
{
public:
  virtual void visit (const irange &) const { }
  virtual void visit (const frange &) const { }
  virtual void visit (const unsupported_range &) const { }
};

typedef int_range<2> value_range;

// This is an "infinite" precision range object for use in temporary
// calculations for any of the handled types.  The object can be
// transparently used as a vrange.

class Value_Range
{
public:
  Value_Range ();
  Value_Range (const vrange &r);
  Value_Range (tree type);
  Value_Range (tree, tree, value_range_kind kind = VR_RANGE);
  Value_Range (const Value_Range &);
  void set_type (tree type);
  vrange& operator= (const vrange &);
  Value_Range& operator= (const Value_Range &);
  bool operator== (const Value_Range &r) const;
  bool operator!= (const Value_Range &r) const;
  operator vrange &();
  operator const vrange &() const;
  void dump (FILE *) const;
  static bool supports_type_p (const_tree type);

  // Convenience methods for vrange compatibility.
  tree type () { return m_vrange->type (); }
  bool varying_p () const { return m_vrange->varying_p (); }
  bool undefined_p () const { return m_vrange->undefined_p (); }
  void set_varying (tree type) { init (type); m_vrange->set_varying (type); }
  void set_undefined () { m_vrange->set_undefined (); }
  bool union_ (const vrange &r) { return m_vrange->union_ (r); }
  bool intersect (const vrange &r) { return m_vrange->intersect (r); }
  bool contains_p (tree cst) const { return m_vrange->contains_p (cst); }
  bool singleton_p (tree *result = NULL) const
    { return m_vrange->singleton_p (result); }
  void set_zero (tree type) { init (type); return m_vrange->set_zero (type); }
  void set_nonzero (tree type)
    { init (type); return m_vrange->set_nonzero (type); }
  bool nonzero_p () const { return m_vrange->nonzero_p (); }
  bool zero_p () const { return m_vrange->zero_p (); }
  wide_int lower_bound () const; // For irange/prange comparability.
  wide_int upper_bound () const; // For irange/prange comparability.
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
Value_Range::Value_Range (tree min, tree max, value_range_kind kind)
{
  init (TREE_TYPE (min));
  m_vrange->set (min, max, kind);
}

inline
Value_Range::Value_Range (const Value_Range &r)
{
  *this = *r.m_vrange;
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
  else if (is_a <unsupported_range> (r))
    {
      m_unsupported = as_a <unsupported_range> (r);
      m_vrange = &m_unsupported;
    }
  else
    gcc_unreachable ();

  return *m_vrange;
}

inline Value_Range &
Value_Range::operator= (const Value_Range &r)
{
  if (r.m_vrange == &r.m_irange)
    {
      m_irange = r.m_irange;
      m_vrange = &m_irange;
    }
  else if (r.m_vrange == &r.m_frange)
    {
      m_frange = r.m_frange;
      m_vrange = &m_frange;
    }
  else if (r.m_vrange == &r.m_unsupported)
    {
      m_unsupported = r.m_unsupported;
      m_vrange = &m_unsupported;
    }
  else
    gcc_unreachable ();

  return *this;
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

extern value_range_kind get_legacy_range (const irange &, tree &min, tree &max);
extern void dump_value_range (FILE *, const vrange *);
extern bool vrp_operand_equal_p (const_tree, const_tree);
inline REAL_VALUE_TYPE frange_val_min (const_tree type);
inline REAL_VALUE_TYPE frange_val_max (const_tree type);

// Number of sub-ranges in a range.

inline unsigned
irange::num_pairs () const
{
  return m_num_ranges;
}

inline tree
irange::type () const
{
  gcc_checking_assert (m_num_ranges > 0);
  return m_type;
}

inline bool
irange::varying_compatible_p () const
{
  if (m_num_ranges != 1)
    return false;

  const wide_int &l = m_base[0];
  const wide_int &u = m_base[1];
  tree t = m_type;

  if (m_kind == VR_VARYING && t == error_mark_node)
    return true;

  unsigned prec = TYPE_PRECISION (t);
  signop sign = TYPE_SIGN (t);
  if (INTEGRAL_TYPE_P (t) || POINTER_TYPE_P (t))
    return (l == wi::min_value (prec, sign)
	    && u == wi::max_value (prec, sign)
	    && m_bitmask.unknown_p ());
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
	  && lower_bound (0) == 0
	  && upper_bound (0) == 0);
}

inline bool
irange::nonzero_p () const
{
  if (undefined_p ())
    return false;

  wide_int zero = wi::zero (TYPE_PRECISION (type ()));
  return *this == int_range<2> (type (), zero, zero, VR_ANTI_RANGE);
}

inline bool
irange::supports_p (const_tree type)
{
  return INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type);
}

inline bool
irange::contains_p (tree cst) const
{
  return contains_p (wi::to_wide (cst));
}

inline bool
range_includes_zero_p (const irange *vr)
{
  if (vr->undefined_p ())
    return false;

  if (vr->varying_p ())
    return true;

  wide_int zero = wi::zero (TYPE_PRECISION (vr->type ()));
  return vr->contains_p (zero);
}

extern void gt_ggc_mx (vrange *);
extern void gt_pch_nx (vrange *);
extern void gt_pch_nx (vrange *, gt_pointer_operator, void *);
extern void gt_ggc_mx (irange *);
extern void gt_pch_nx (irange *);
extern void gt_pch_nx (irange *, gt_pointer_operator, void *);
extern void gt_ggc_mx (frange *);
extern void gt_pch_nx (frange *);
extern void gt_pch_nx (frange *, gt_pointer_operator, void *);

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
irange::irange (wide_int *base, unsigned nranges, bool resizable)
  : vrange (VR_IRANGE),
    m_resizable (resizable),
    m_max_ranges (nranges)
{
  m_base = base;
  set_undefined ();
}

// Constructors for int_range<>.

template<unsigned N, bool RESIZABLE>
inline
int_range<N, RESIZABLE>::int_range ()
  : irange (m_ranges, N, RESIZABLE)
{
}

template<unsigned N, bool RESIZABLE>
int_range<N, RESIZABLE>::int_range (const int_range &other)
  : irange (m_ranges, N, RESIZABLE)
{
  irange::operator= (other);
}

template<unsigned N, bool RESIZABLE>
int_range<N, RESIZABLE>::int_range (tree min, tree max, value_range_kind kind)
  : irange (m_ranges, N, RESIZABLE)
{
  irange::set (min, max, kind);
}

template<unsigned N, bool RESIZABLE>
int_range<N, RESIZABLE>::int_range (tree type)
  : irange (m_ranges, N, RESIZABLE)
{
  set_varying (type);
}

template<unsigned N, bool RESIZABLE>
int_range<N, RESIZABLE>::int_range (tree type, const wide_int &wmin, const wide_int &wmax,
			 value_range_kind kind)
  : irange (m_ranges, N, RESIZABLE)
{
  set (type, wmin, wmax, kind);
}

template<unsigned N, bool RESIZABLE>
int_range<N, RESIZABLE>::int_range (const irange &other)
  : irange (m_ranges, N, RESIZABLE)
{
  irange::operator= (other);
}

template<unsigned N, bool RESIZABLE>
int_range<N, RESIZABLE>&
int_range<N, RESIZABLE>::operator= (const int_range &src)
{
  irange::operator= (src);
  return *this;
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
  m_bitmask.set_unknown (TYPE_PRECISION (type));

  if (INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type))
    {
      m_type = type;
      // Strict enum's require varying to be not TYPE_MIN/MAX, but rather
      // min_value and max_value.
      m_base[0] = wi::min_value (TYPE_PRECISION (type), TYPE_SIGN (type));
      m_base[1] = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));
    }
  else
    m_type = error_mark_node;
}

// Return the lower bound of a sub-range.  PAIR is the sub-range in
// question.

inline wide_int
irange::lower_bound (unsigned pair) const
{
  gcc_checking_assert (m_num_ranges > 0);
  gcc_checking_assert (pair + 1 <= num_pairs ());
  return m_base[pair * 2];
}

// Return the upper bound of a sub-range.  PAIR is the sub-range in
// question.

inline wide_int
irange::upper_bound (unsigned pair) const
{
  gcc_checking_assert (m_num_ranges > 0);
  gcc_checking_assert (pair + 1 <= num_pairs ());
  return m_base[pair * 2 + 1];
}

// Return the highest bound of a range.

inline wide_int
irange::upper_bound () const
{
  unsigned pairs = num_pairs ();
  gcc_checking_assert (pairs > 0);
  return upper_bound (pairs - 1);
}

// Set value range VR to a nonzero range of type TYPE.

inline void
irange::set_nonzero (tree type)
{
  unsigned prec = TYPE_PRECISION (type);

  if (TYPE_UNSIGNED (type))
    {
      m_type = type;
      m_kind = VR_RANGE;
      m_base[0] = wi::one (prec);
      m_base[1] = wi::minus_one (prec);
      m_bitmask.set_unknown (prec);
      m_num_ranges = 1;

      if (flag_checking)
	verify_range ();
    }
  else
    {
      wide_int zero = wi::zero (prec);
      set (type, zero, zero, VR_ANTI_RANGE);
    }
}

// Set value range VR to a ZERO range of type TYPE.

inline void
irange::set_zero (tree type)
{
  wide_int zero = wi::zero (TYPE_PRECISION (type));
  set (type, zero, zero);
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
    }
  if (flag_checking)
    verify_range ();
}

inline bool
contains_zero_p (const irange &r)
{
  if (r.undefined_p ())
    return true;

  wide_int zero = wi::zero (TYPE_PRECISION (r.type ()));
  return r.contains_p (zero);
}

inline wide_int
irange_val_min (const_tree type)
{
  gcc_checking_assert (irange::supports_p (type));
  return wi::min_value (TYPE_PRECISION (type), TYPE_SIGN (type));
}

inline wide_int
irange_val_max (const_tree type)
{
  gcc_checking_assert (irange::supports_p (type));
  return wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));
}

inline
frange::frange ()
  : vrange (VR_FRANGE)
{
  set_undefined ();
}

inline
frange::frange (const frange &src)
  : vrange (VR_FRANGE)
{
  *this = src;
}

inline
frange::frange (tree type)
  : vrange (VR_FRANGE)
{
  set_varying (type);
}

// frange constructor from REAL_VALUE_TYPE endpoints.

inline
frange::frange (tree type,
		const REAL_VALUE_TYPE &min, const REAL_VALUE_TYPE &max,
		value_range_kind kind)
  : vrange (VR_FRANGE)
{
  set (type, min, max, kind);
}

// frange constructor from trees.

inline
frange::frange (tree min, tree max, value_range_kind kind)
  : vrange (VR_FRANGE)
{
  set (min, max, kind);
}

inline tree
frange::type () const
{
  gcc_checking_assert (!undefined_p ());
  return m_type;
}

inline void
frange::set_varying (tree type)
{
  m_kind = VR_VARYING;
  m_type = type;
  m_min = frange_val_min (type);
  m_max = frange_val_max (type);
  if (HONOR_NANS (m_type))
    {
      m_pos_nan = true;
      m_neg_nan = true;
    }
  else
    {
      m_pos_nan = false;
      m_neg_nan = false;
    }
}

inline void
frange::set_undefined ()
{
  m_kind = VR_UNDEFINED;
  m_type = NULL;
  m_pos_nan = false;
  m_neg_nan = false;
  // m_min and m_min are uninitialized as they are REAL_VALUE_TYPE ??.
  if (flag_checking)
    verify_range ();
}

// Set the NAN bit and adjust the range.

inline void
frange::update_nan ()
{
  gcc_checking_assert (!undefined_p ());
  if (HONOR_NANS (m_type))
    {
      m_pos_nan = true;
      m_neg_nan = true;
      normalize_kind ();
      if (flag_checking)
	verify_range ();
    }
}

// Like above, but set the sign of the NAN.

inline void
frange::update_nan (bool sign)
{
  gcc_checking_assert (!undefined_p ());
  if (HONOR_NANS (m_type))
    {
      m_pos_nan = !sign;
      m_neg_nan = sign;
      normalize_kind ();
      if (flag_checking)
	verify_range ();
    }
}

inline bool
frange::contains_p (tree cst) const
{
  return contains_p (*TREE_REAL_CST_PTR (cst));
}

// Clear the NAN bit and adjust the range.

inline void
frange::clear_nan ()
{
  gcc_checking_assert (!undefined_p ());
  m_pos_nan = false;
  m_neg_nan = false;
  normalize_kind ();
  if (flag_checking)
    verify_range ();
}

// Set R to maximum representable value for TYPE.

inline REAL_VALUE_TYPE
real_max_representable (const_tree type)
{
  REAL_VALUE_TYPE r;
  char buf[128];
  get_max_float (REAL_MODE_FORMAT (TYPE_MODE (type)),
		 buf, sizeof (buf), false);
  int res = real_from_string (&r, buf);
  gcc_checking_assert (!res);
  return r;
}

// Return the minimum representable value for TYPE.

inline REAL_VALUE_TYPE
real_min_representable (const_tree type)
{
  REAL_VALUE_TYPE r = real_max_representable (type);
  r = real_value_negate (&r);
  return r;
}

// Return the minimum value for TYPE.

inline REAL_VALUE_TYPE
frange_val_min (const_tree type)
{
  if (HONOR_INFINITIES (type))
    return dconstninf;
  else
    return real_min_representable (type);
}

// Return the maximum value for TYPE.

inline REAL_VALUE_TYPE
frange_val_max (const_tree type)
{
  if (HONOR_INFINITIES (type))
    return dconstinf;
  else
    return real_max_representable (type);
}

// Return TRUE if R is the minimum value for TYPE.

inline bool
frange_val_is_min (const REAL_VALUE_TYPE &r, const_tree type)
{
  REAL_VALUE_TYPE min = frange_val_min (type);
  return real_identical (&min, &r);
}

// Return TRUE if R is the max value for TYPE.

inline bool
frange_val_is_max (const REAL_VALUE_TYPE &r, const_tree type)
{
  REAL_VALUE_TYPE max = frange_val_max (type);
  return real_identical (&max, &r);
}

// Build a NAN with a state of NAN.

inline void
frange::set_nan (tree type, const nan_state &nan)
{
  gcc_checking_assert (nan.pos_p () || nan.neg_p ());
  if (HONOR_NANS (type))
    {
      m_kind = VR_NAN;
      m_type = type;
      m_neg_nan = nan.neg_p ();
      m_pos_nan = nan.pos_p ();
      if (flag_checking)
	verify_range ();
    }
  else
    set_undefined ();
}

// Build a signless NAN of type TYPE.

inline void
frange::set_nan (tree type)
{
  nan_state nan (true);
  set_nan (type, nan);
}

// Build a NAN of type TYPE with SIGN.

inline void
frange::set_nan (tree type, bool sign)
{
  nan_state nan (/*pos=*/!sign, /*neg=*/sign);
  set_nan (type, nan);
}

// Return TRUE if range is known to be finite.

inline bool
frange::known_isfinite () const
{
  if (undefined_p () || varying_p () || m_kind == VR_ANTI_RANGE)
    return false;
  return (!maybe_isnan () && !real_isinf (&m_min) && !real_isinf (&m_max));
}

// Return TRUE if range may be infinite.

inline bool
frange::maybe_isinf () const
{
  if (undefined_p () || m_kind == VR_ANTI_RANGE || m_kind == VR_NAN)
    return false;
  if (varying_p ())
    return true;
  return real_isinf (&m_min) || real_isinf (&m_max);
}

// Return TRUE if range is known to be the [-INF,-INF] or [+INF,+INF].

inline bool
frange::known_isinf () const
{
  return (m_kind == VR_RANGE
	  && !maybe_isnan ()
	  && real_identical (&m_min, &m_max)
	  && real_isinf (&m_min));
}

// Return TRUE if range is possibly a NAN.

inline bool
frange::maybe_isnan () const
{
  if (undefined_p ())
    return false;
  return m_pos_nan || m_neg_nan;
}

// Return TRUE if range is possibly a NAN with SIGN.

inline bool
frange::maybe_isnan (bool sign) const
{
  if (undefined_p ())
    return false;
  if (sign)
    return m_neg_nan;
  return m_pos_nan;
}

// Return TRUE if range is a +NAN or -NAN.

inline bool
frange::known_isnan () const
{
  return m_kind == VR_NAN;
}

// If the signbit for the range is known, set it in SIGNBIT and return
// TRUE.

inline bool
frange::signbit_p (bool &signbit) const
{
  if (undefined_p ())
    return false;

  // NAN with unknown sign.
  if (m_pos_nan && m_neg_nan)
    return false;
  // No NAN.
  if (!m_pos_nan && !m_neg_nan)
    {
      if (m_min.sign == m_max.sign)
	{
	  signbit = m_min.sign;
	  return true;
	}
      return false;
    }
  // NAN with known sign.
  bool nan_sign = m_neg_nan;
  if (known_isnan ()
      || (nan_sign == m_min.sign && nan_sign == m_max.sign))
    {
      signbit = nan_sign;
      return true;
    }
  return false;
}

// If range has a NAN with a known sign, set it in SIGNBIT and return
// TRUE.

inline bool
frange::nan_signbit_p (bool &signbit) const
{
  if (undefined_p ())
    return false;

  if (m_pos_nan == m_neg_nan)
    return false;

  signbit = m_neg_nan;
  return true;
}

void frange_nextafter (enum machine_mode, REAL_VALUE_TYPE &,
		       const REAL_VALUE_TYPE &);
void frange_arithmetic (enum tree_code, tree, REAL_VALUE_TYPE &,
			const REAL_VALUE_TYPE &, const REAL_VALUE_TYPE &,
			const REAL_VALUE_TYPE &);

#endif // GCC_VALUE_RANGE_H
