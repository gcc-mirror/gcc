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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "tree-pretty-print.h"
#include "fold-const.h"
#include "gimple-range.h"
#include "value-range-storage.h"

// Return a newly allocated slot holding R, or NULL if storing a range
// of R's type is not supported.

void *
vrange_storage::alloc_slot (const vrange &r)
{
  gcc_checking_assert (m_alloc);

  if (is_a <irange> (r))
    return irange_storage_slot::alloc_slot (*m_alloc, as_a <irange> (r));
  if (is_a <frange> (r))
    return frange_storage_slot::alloc_slot (*m_alloc, as_a <frange> (r));
  return NULL;
}

// Set SLOT to R.

void
vrange_storage::set_vrange (void *slot, const vrange &r)
{
  if (is_a <irange> (r))
    {
      irange_storage_slot *s = static_cast <irange_storage_slot *> (slot);
      gcc_checking_assert (s->fits_p (as_a <irange> (r)));
      s->set_irange (as_a <irange> (r));
    }
  else if (is_a <frange> (r))
    {
      frange_storage_slot *s = static_cast <frange_storage_slot *> (slot);
      gcc_checking_assert (s->fits_p (as_a <frange> (r)));
      s->set_frange (as_a <frange> (r));
    }
  else
    gcc_unreachable ();
}

// Restore R from SLOT.  TYPE is the type of R.

void
vrange_storage::get_vrange (const void *slot, vrange &r, tree type)
{
  if (is_a <irange> (r))
    {
      const irange_storage_slot *s
	= static_cast <const irange_storage_slot *> (slot);
      s->get_irange (as_a <irange> (r), type);
    }
  else if (is_a <frange> (r))
    {
      const frange_storage_slot *s
	= static_cast <const frange_storage_slot *> (slot);
      s->get_frange (as_a <frange> (r), type);
    }
  else
    gcc_unreachable ();
}

// Return TRUE if SLOT can fit R.

bool
vrange_storage::fits_p (const void *slot, const vrange &r)
{
  if (is_a <irange> (r))
    {
      const irange_storage_slot *s
	= static_cast <const irange_storage_slot *> (slot);
      return s->fits_p (as_a <irange> (r));
    }
  if (is_a <frange> (r))
    {
      const frange_storage_slot *s
	= static_cast <const frange_storage_slot *> (slot);
      return s->fits_p (as_a <frange> (r));
    }
  gcc_unreachable ();
  return false;
}

// Factory that creates a new irange_storage_slot object containing R.
// This is the only way to construct an irange slot as stack creation
// is disallowed.

irange_storage_slot *
irange_storage_slot::alloc_slot (vrange_allocator &allocator, const irange &r)
{
  size_t size = irange_storage_slot::size (r);
  irange_storage_slot *p
    = static_cast <irange_storage_slot *> (allocator.alloc (size));
  new (p) irange_storage_slot (r);
  return p;
}

// Initialize the current slot with R.

irange_storage_slot::irange_storage_slot (const irange &r)
{
  gcc_checking_assert (!r.undefined_p ());

  unsigned prec = TYPE_PRECISION (r.type ());
  unsigned n = num_wide_ints_needed (r);
  if (n > MAX_INTS)
    {
      int_range<MAX_PAIRS> squash (r);
      m_ints.set_precision (prec, num_wide_ints_needed (squash));
      set_irange (squash);
    }
  else
    {
      m_ints.set_precision (prec, n);
      set_irange (r);
    }
}

// Store R into the current slot.

void
irange_storage_slot::set_irange (const irange &r)
{
  gcc_checking_assert (fits_p (r));

  m_ints[0] = r.get_nonzero_bits ();

  unsigned pairs = r.num_pairs ();
  for (unsigned i = 0; i < pairs; ++i)
    {
      m_ints[i*2 + 1] = r.lower_bound (i);
      m_ints[i*2 + 2] = r.upper_bound (i);
    }
}

// Restore a range of TYPE from the current slot into R.

void
irange_storage_slot::get_irange (irange &r, tree type) const
{
  gcc_checking_assert (TYPE_PRECISION (type) == m_ints.get_precision ());

  r.set_undefined ();
  unsigned nelements = m_ints.num_elements ();
  for (unsigned i = 1; i < nelements; i += 2)
    {
      int_range<2> tmp (type, m_ints[i], m_ints[i + 1]);
      r.union_ (tmp);
    }
  r.set_nonzero_bits (get_nonzero_bits ());
}

// Return the size in bytes to allocate a slot that can hold R.

size_t
irange_storage_slot::size (const irange &r)
{
  gcc_checking_assert (!r.undefined_p ());

  unsigned prec = TYPE_PRECISION (r.type ());
  unsigned n = num_wide_ints_needed (r);
  if (n > MAX_INTS)
    n = MAX_INTS;
  return (sizeof (irange_storage_slot)
	  + trailing_wide_ints<MAX_INTS>::extra_size (prec, n));
}

// Return the number of wide ints needed to represent R.

unsigned int
irange_storage_slot::num_wide_ints_needed (const irange &r)
{
  return r.num_pairs () * 2 + 1;
}

// Return TRUE if R fits in the current slot.

bool
irange_storage_slot::fits_p (const irange &r) const
{
  return m_ints.num_elements () >= num_wide_ints_needed (r);
}

// Dump the current slot.

void
irange_storage_slot::dump () const
{
  fprintf (stderr, "raw irange_storage_slot:\n");
  for (unsigned i = 1; i < m_ints.num_elements (); i += 2)
    {
      m_ints[i].dump ();
      m_ints[i + 1].dump ();
    }
  fprintf (stderr, "NONZERO ");
  wide_int nz = get_nonzero_bits ();
  nz.dump ();
}

DEBUG_FUNCTION void
debug (const irange_storage_slot &storage)
{
  storage.dump ();
  fprintf (stderr, "\n");
}

// Implementation of frange_storage_slot.

frange_storage_slot *
frange_storage_slot::alloc_slot (vrange_allocator &allocator, const frange &r)
{
  size_t size = sizeof (frange_storage_slot);
  frange_storage_slot *p
    = static_cast <frange_storage_slot *> (allocator.alloc (size));
  new (p) frange_storage_slot (r);
  return p;
}

void
frange_storage_slot::set_frange (const frange &r)
{
  gcc_checking_assert (fits_p (r));
  gcc_checking_assert (!r.undefined_p ());

  m_kind = r.m_kind;
  m_min = r.m_min;
  m_max = r.m_max;
  m_pos_nan = r.m_pos_nan;
  m_neg_nan = r.m_neg_nan;
}

void
frange_storage_slot::get_frange (frange &r, tree type) const
{
  gcc_checking_assert (r.supports_type_p (type));

  // Handle explicit NANs.
  if (m_kind == VR_NAN)
    {
      if (HONOR_NANS (type))
	{
	  if (m_pos_nan && m_neg_nan)
	    r.set_nan (type);
	  else
	    r.set_nan (type, m_neg_nan);
	}
      else
	r.set_undefined ();
      return;
    }

  // We use the constructor to create the new range instead of writing
  // out the bits into the frange directly, because the global range
  // being read may be being inlined into a function with different
  // restrictions as when it was originally written.  We want to make
  // sure the resulting range is canonicalized correctly for the new
  // consumer.
  r = frange (type, m_min, m_max, m_kind);

  // The constructor will set the NAN bits for HONOR_NANS, but we must
  // make sure to set the NAN sign if known.
  if (HONOR_NANS (type) && (m_pos_nan ^ m_neg_nan) == 1)
    r.update_nan (m_neg_nan);
  else if (!m_pos_nan && !m_neg_nan)
    r.clear_nan ();
}

bool
frange_storage_slot::fits_p (const frange &) const
{
  return true;
}
