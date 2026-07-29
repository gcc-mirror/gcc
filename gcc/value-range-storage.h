/* Support routines for vrange storage.
   Copyright (C) 2022-2026 Free Software Foundation, Inc.
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
  class vrange_storage *clone (const vrange &r, bool shared_p = true);
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

class GTY((desc ("%h.m_discriminator"), tag("VR_UNKNOWN"))) vrange_storage
{
public:
  static vrange_storage *alloc (vrange_internal_alloc &, const vrange &,
				bool shared_p = true);
  void get_vrange (vrange &r, tree type) const;
  void set_vrange (const vrange &r);
  bool fits_p (const vrange &r) const;
  bool equal_p (const vrange &r) const;

  // Stack initialization disallowed.
  vrange_storage (enum value_range_discriminator d) : m_discriminator (d) { }
  const enum value_range_discriminator m_discriminator : 4;
};

// Efficient memory storage for an irange.

class GTY((tag ("VR_IRANGE"))) irange_storage: public vrange_storage
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

  // The shared precision of each number.
  unsigned short m_precision;

  // The max number of sub-ranges that fit in this storage.
  const unsigned char m_max_ranges;

  // The number of stored sub-ranges.
  unsigned char m_num_ranges;

  enum value_range_kind m_kind : 3;

  // The length of this is m_num_ranges * 2 + 2 to accommodate the bitmask.
  HOST_WIDE_INT m_val[1];

  // Another variable-length part of the structure following the HWIs.
  // This is the length of each wide_int in m_val.
  //
  // unsigned short m_len[];

  irange_storage (const irange &r);
};


// A prange_kind summarizes some common variations for a prange, and is used
// in a prange_storage clas for efficiency.

enum prange_kind { PR_UNDEFINED,	// VR_UNDEFINED
		   PR_VARYING,		// VR_VARYING
		   PR_ZERO,		// [0, 0]
		   PR_NONZERO,		// [1, +INF] (May have bitmask)
		   PR_FULL,		// [0, +INF] (Must have bitmask)
		   PR_OTHER };		// [x, y]    (MAy have bitmask)

// Maximum number of words that may be allocated by a prange_storage class.
const unsigned int PRANGE_STORAGE_NINTS = 4;

// Efficient memory storage for a prange.
class GTY((tag ("VR_PRANGE"))) prange_storage : public vrange_storage
{
public:
  friend void gt_ggc_mx_vrange_storage(void *);
  friend void gt_pch_nx_vrange_storage(void *);
  friend void gt_pch_p_14vrange_storage(void *, void *, gt_pointer_operator,
					void *);
  static prange_storage *alloc (vrange_internal_alloc &, const prange &,
				bool shared_p = true);
  void set_prange (const prange &r);
  void get_prange (prange &r, tree type) const;
  bool equal_p (const prange &r) const;
  bool fits_p (const prange &r) const;
  void dump () const;

private:
  DISABLE_COPY_AND_ASSIGN (prange_storage);
  prange_storage (const prange &r);
  static enum prange_kind prange_format (const prange &r, unsigned &num_words);

  enum prange_kind m_kind;
  bool m_has_bitmask;
  bool m_points_to_p;
  tree m_pt;

  // We don't use TRAILING_WIDE_INT_ACCESSOR because the getters here
  // must be const.  Perhaps TRAILING_WIDE_INT_ACCESSOR could be made
  // const and return wide_int instead of trailing_wide_int.
  wide_int get_word (unsigned i, tree) const
    { return m_trailing_ints[i]; }
  template <typename T> void set_word (unsigned i, const T &x, tree)
    { m_trailing_ints[i] = x; }

  trailing_wide_ints<PRANGE_STORAGE_NINTS> m_trailing_ints;
};

// Efficient memory storage for an frange.

class GTY((tag ("VR_FRANGE"))) frange_storage : public vrange_storage
{
 public:
  static frange_storage *alloc (vrange_internal_alloc &, const frange &r);
  void set_frange (const frange &r);
  void get_frange (frange &r, tree type) const;
  bool equal_p (const frange &r) const;
  bool fits_p (const frange &) const;
 private:
  frange_storage (const frange &r);
  DISABLE_COPY_AND_ASSIGN (frange_storage);
  static size_t size (const frange &r);

  enum value_range_kind m_kind;
  // The max number of sub-ranges that fit in this storage.
  const unsigned char m_max_ranges;
  unsigned char m_num_ranges;
  bool m_pos_nan;
  bool m_neg_nan;
  frange_pair m_pairs[1];
};

extern vrange_storage *ggc_alloc_vrange_storage (tree type);
extern vrange_storage *ggc_alloc_vrange_storage (const vrange &,
						 bool shared_p = true);

#endif // GCC_VALUE_RANGE_STORAGE_H
