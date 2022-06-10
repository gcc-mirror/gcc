/* Support routines for vrange storage.
   Copyright (C) 2022 Free Software Foundation, Inc.
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


// INTERNAL USE ONLY.  The remaining interfaces are only exposed for
// the GTY machinery to play nice with tree_ssa_name.

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
  // control word that preceeds the HOST_WIDE_INTs in
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

#endif // GCC_VALUE_RANGE_STORAGE_H
