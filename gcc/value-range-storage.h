/* Support routines for vrange storage.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
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
// ranges as memory efficiently as possible.

class vrange_allocator
{
public:
  // Use GC memory when GC is true, otherwise use obstacks.
  vrange_allocator (bool gc = false);
  ~vrange_allocator ();
  class vrange_storage *clone (const vrange &r);
  vrange_storage *clone_varying (tree type);
  vrange_storage *clone_undefined (tree type);
  void *alloc (size_t size);
  void free (void *);
private:
  DISABLE_COPY_AND_ASSIGN (vrange_allocator);
  class vrange_internal_alloc *m_alloc;
};

// Efficient memory storage for a vrange.
//
// The GTY marker here does nothing but get gengtype to generate the
// ggc_test_and_set_mark calls.  We ignore the derived classes, since
// they don't contain any pointers.

class GTY(()) vrange_storage
{
public:
  static vrange_storage *alloc (vrange_internal_alloc &, const vrange &);
  void get_vrange (vrange &r, tree type) const;
  void set_vrange (const vrange &r);
  bool fits_p (const vrange &r) const;
  bool equal_p (const vrange &r) const;
protected:
  // Stack initialization disallowed.
  vrange_storage () { }
};

// Efficient memory storage for an irange.

class irange_storage : public vrange_storage
{
public:
  static irange_storage *alloc (vrange_internal_alloc &, const irange &);
  void set_irange (const irange &r);
  void get_irange (irange &r, tree type) const;
  bool equal_p (const irange &r) const;
  bool fits_p (const irange &r) const;
  void dump () const;
private:
  DISABLE_COPY_AND_ASSIGN (irange_storage);
  static size_t size (const irange &r);
  const unsigned short *lengths_address () const;
  unsigned short *write_lengths_address ();
  friend void gt_ggc_mx_irange_storage (void *);
  friend void gt_pch_p_14irange_storage (void *, void *,
					      gt_pointer_operator, void *);
  friend void gt_pch_nx_irange_storage (void *);

  // The shared precision of each number.
  unsigned short m_precision;

  // The max number of sub-ranges that fit in this storage.
  const unsigned char m_max_ranges;

  // The number of stored sub-ranges.
  unsigned char m_num_ranges;

  enum value_range_kind m_kind : 3;

  // The length of this is m_num_ranges * 2 + 2 to accomodate the bitmask.
  HOST_WIDE_INT m_val[1];

  // Another variable-length part of the structure following the HWIs.
  // This is the length of each wide_int in m_val.
  //
  // unsigned short m_len[];

  irange_storage (const irange &r);
};

// Efficient memory storage for an frange.

class frange_storage : public vrange_storage
{
 public:
  static frange_storage *alloc (vrange_internal_alloc &, const frange &r);
  void set_frange (const frange &r);
  void get_frange (frange &r, tree type) const;
  bool equal_p (const frange &r) const;
  bool fits_p (const frange &) const;
 private:
  frange_storage (const frange &r) { set_frange (r); }
  DISABLE_COPY_AND_ASSIGN (frange_storage);

  enum value_range_kind m_kind;
  REAL_VALUE_TYPE m_min;
  REAL_VALUE_TYPE m_max;
  bool m_pos_nan;
  bool m_neg_nan;
};

extern vrange_storage *ggc_alloc_vrange_storage (tree type);
extern vrange_storage *ggc_alloc_vrange_storage (const vrange &);

#endif // GCC_VALUE_RANGE_STORAGE_H
