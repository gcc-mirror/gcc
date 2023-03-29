/* Support routines for vrange storage.
   Copyright (C) 2022-2023 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>.

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

#ifndef GCC_VALUE_RANGE_STORAGE_H
#define GCC_VALUE_RANGE_STORAGE_H

// This class is used to allocate the minimum amount of storage needed
// for a given range.  Storage is automatically freed at destruction
// of the class.

class vrange_allocator
{
public:
  vrange_allocator () { }
  virtual ~vrange_allocator () { }
  // Allocate a range of TYPE.
  vrange *alloc_vrange (tree type);
  // Allocate a memory block of BYTES.
  virtual void *alloc (unsigned bytes) = 0;
  virtual void free (void *p) = 0;
  // Return a clone of SRC.
  template <typename T> T *clone (const T &src);
private:
  irange *alloc_irange (unsigned pairs);
  frange *alloc_frange ();
  void operator= (const vrange_allocator &) = delete;
};

// This class is used to allocate chunks of memory that can store
// ranges as memory efficiently as possible.  It is meant to be used
// when long term storage of a range is needed.  The class can be used
// with any vrange_allocator (i.e. alloca or GC).

class vrange_storage
{
public:
  vrange_storage (vrange_allocator *alloc) : m_alloc (alloc) { }
  void *alloc_slot (const vrange &r);
  void free (void *slot) { m_alloc->free (slot); }
  void get_vrange (const void *slot, vrange &r, tree type);
  void set_vrange (void *slot, const vrange &r);
  static bool fits_p (const void *slot, const vrange &r);
private:
  DISABLE_COPY_AND_ASSIGN (vrange_storage);
  vrange_allocator *m_alloc;
};

// A chunk of memory pointing to an irange storage.

class GTY ((variable_size)) irange_storage_slot
{
public:
  static irange_storage_slot *alloc_slot (vrange_allocator &, const irange &r);
  void set_irange (const irange &r);
  void get_irange (irange &r, tree type) const;
  wide_int get_nonzero_bits () const { return m_ints[0]; }
  bool fits_p (const irange &r) const;
  static size_t size (const irange &r);
  void dump () const;
private:
  DISABLE_COPY_AND_ASSIGN (irange_storage_slot);
  friend void gt_ggc_mx_irange_storage_slot (void *);
  friend void gt_pch_p_19irange_storage_slot (void *, void *,
					      gt_pointer_operator, void *);
  friend void gt_pch_nx_irange_storage_slot (void *);

  // This is the maximum number of wide_int's allowed in the trailing
  // ints structure, without going over 16 bytes (128 bits) in the
  // control word that precedes the HOST_WIDE_INTs in
  // trailing_wide_ints::m_val[].
  static const unsigned MAX_INTS = 12;

  // Maximum number of range pairs we can handle, considering the
  // nonzero bits take one wide_int.
  static const unsigned MAX_PAIRS = (MAX_INTS - 1) / 2;

  // Constructor is private to disallow stack initialization.  Use
  // alloc_slot() to create objects.
  irange_storage_slot (const irange &r);

  static unsigned num_wide_ints_needed (const irange &r);

  trailing_wide_ints<MAX_INTS> m_ints;
};

// A chunk of memory to store an frange to long term memory.

class GTY (()) frange_storage_slot
{
 public:
  static frange_storage_slot *alloc_slot (vrange_allocator &, const frange &r);
  void set_frange (const frange &r);
  void get_frange (frange &r, tree type) const;
  bool fits_p (const frange &) const;
 private:
  frange_storage_slot (const frange &r) { set_frange (r); }
  DISABLE_COPY_AND_ASSIGN (frange_storage_slot);

  enum value_range_kind m_kind;
  REAL_VALUE_TYPE m_min;
  REAL_VALUE_TYPE m_max;
  bool m_pos_nan;
  bool m_neg_nan;
};

class obstack_vrange_allocator final: public vrange_allocator
{
public:
  obstack_vrange_allocator ()
  {
    obstack_init (&m_obstack);
  }
  virtual ~obstack_vrange_allocator () final override
  {
    obstack_free (&m_obstack, NULL);
  }
  virtual void *alloc (unsigned bytes) final override
  {
    return obstack_alloc (&m_obstack, bytes);
  }
  virtual void free (void *) final override { }
private:
  obstack m_obstack;
};

class ggc_vrange_allocator final: public vrange_allocator
{
public:
  ggc_vrange_allocator () { }
  virtual ~ggc_vrange_allocator () final override { }
  virtual void *alloc (unsigned bytes) final override
  {
    return ggc_internal_alloc (bytes);
  }
  virtual void free (void *p) final override
  {
    return ggc_free (p);
  }
};

// Return a new range to hold ranges of TYPE.  The newly allocated
// range is initialized to VR_UNDEFINED.

inline vrange *
vrange_allocator::alloc_vrange (tree type)
{
  if (irange::supports_p (type))
    return alloc_irange (2);
  if (frange::supports_p (type))
    return alloc_frange ();
  return NULL;
  gcc_unreachable ();
}

// Return a new range with NUM_PAIRS.

inline irange *
vrange_allocator::alloc_irange (unsigned num_pairs)
{
  // Never allocate 0 pairs.
  // Don't allocate 1 either, or we get legacy value_range's.
  if (num_pairs < 2)
    num_pairs = 2;

  size_t nbytes = sizeof (tree) * 2 * num_pairs;

  // Allocate the irange and required memory for the vector.
  void *r = alloc (sizeof (irange));
  tree *mem = static_cast <tree *> (alloc (nbytes));
  return new (r) irange (mem, num_pairs);
}

inline frange *
vrange_allocator::alloc_frange ()
{
  void *r = alloc (sizeof (frange));
  return new (r) frange ();
}

// Return a clone of an irange.

template <>
inline irange *
vrange_allocator::clone <irange> (const irange &src)
{
  irange *r = alloc_irange (src.num_pairs ());
  *r = src;
  return r;
}

// Return a clone of an frange.

template <>
inline frange *
vrange_allocator::clone <frange> (const frange &src)
{
  frange *r = alloc_frange ();
  *r = src;
  return r;
}

// Return a clone of a vrange.

template <>
inline vrange *
vrange_allocator::clone <vrange> (const vrange &src)
{
  if (is_a <irange> (src))
    return clone <irange> (as_a <irange> (src));
  if (is_a <frange> (src))
    return clone <frange> (as_a <frange> (src));
  return NULL;
  gcc_unreachable ();
}

#endif // GCC_VALUE_RANGE_STORAGE_H
