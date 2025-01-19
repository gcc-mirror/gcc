/* Support routines for vrange storage.
   Copyright (C) 2022-2025 Free Software Foundation, Inc.
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

// Generic memory allocator to share one interface between GC and
// obstack allocators.

class vrange_internal_alloc
{
public:
  vrange_internal_alloc () { }
  virtual ~vrange_internal_alloc () { }
  virtual void *alloc (size_t size) = 0;
  virtual void free (void *) = 0;
private:
  DISABLE_COPY_AND_ASSIGN (vrange_internal_alloc);
};

class vrange_obstack_alloc final: public vrange_internal_alloc
{
public:
  vrange_obstack_alloc ()
  {
    obstack_init (&m_obstack);
  }
  virtual ~vrange_obstack_alloc () final override
  {
    obstack_free (&m_obstack, NULL);
  }
  virtual void *alloc (size_t size) final override
  {
    return obstack_alloc (&m_obstack, size);
  }
  virtual void free (void *) final override { }
private:
  obstack m_obstack;
};

class vrange_ggc_alloc final: public vrange_internal_alloc
{
public:
  vrange_ggc_alloc () { }
  virtual ~vrange_ggc_alloc () final override { }
  virtual void *alloc (size_t size) final override
  {
    return ggc_internal_alloc (size);
  }
  virtual void free (void *p) final override
  {
    return ggc_free (p);
  }
};

vrange_allocator::vrange_allocator (bool gc)
{
  if (gc)
    m_alloc = new vrange_ggc_alloc;
  else
    m_alloc = new vrange_obstack_alloc;
}

vrange_allocator::~vrange_allocator ()
{
  delete m_alloc;
}

void *
vrange_allocator::alloc (size_t size)
{
  return m_alloc->alloc (size);
}

void
vrange_allocator::free (void *p)
{
  m_alloc->free (p);
}

// Allocate a new vrange_storage object initialized to R and return
// it.

vrange_storage *
vrange_allocator::clone (const vrange &r)
{
  return vrange_storage::alloc (*m_alloc, r);
}

vrange_storage *
vrange_allocator::clone_varying (tree type)
{
  if (irange::supports_p (type))
    return irange_storage::alloc (*m_alloc, int_range <1> (type));
  if (prange::supports_p (type))
    return prange_storage::alloc (*m_alloc, prange (type));
  if (frange::supports_p (type))
    return frange_storage::alloc (*m_alloc, frange (type));
  return NULL;
}

vrange_storage *
vrange_allocator::clone_undefined (tree type)
{
  if (irange::supports_p (type))
    return irange_storage::alloc (*m_alloc, int_range<1> ());
  if (prange::supports_p (type))
    return prange_storage::alloc (*m_alloc, prange ());
  if (frange::supports_p (type))
    return frange_storage::alloc  (*m_alloc, frange ());
  return NULL;
}

// Allocate a new vrange_storage object initialized to R and return
// it.  Return NULL if R is unsupported.

vrange_storage *
vrange_storage::alloc (vrange_internal_alloc &allocator, const vrange &r)
{
  if (is_a <irange> (r))
    return irange_storage::alloc (allocator, as_a <irange> (r));
  if (is_a <prange> (r))
    return prange_storage::alloc (allocator, as_a <prange> (r));
  if (is_a <frange> (r))
    return frange_storage::alloc (allocator, as_a <frange> (r));
  return NULL;
}

// Set storage to R.

void
vrange_storage::set_vrange (const vrange &r)
{
  if (is_a <irange> (r))
    {
      irange_storage *s = static_cast <irange_storage *> (this);
      gcc_checking_assert (s->fits_p (as_a <irange> (r)));
      s->set_irange (as_a <irange> (r));
    }
  else if (is_a <prange> (r))
    {
      prange_storage *s = static_cast <prange_storage *> (this);
      gcc_checking_assert (s->fits_p (as_a <prange> (r)));
      s->set_prange (as_a <prange> (r));
    }
  else if (is_a <frange> (r))
    {
      frange_storage *s = static_cast <frange_storage *> (this);
      gcc_checking_assert (s->fits_p (as_a <frange> (r)));
      s->set_frange (as_a <frange> (r));
    }
  else
    gcc_unreachable ();

  // Verify that reading back from the cache didn't drop bits.
  if (flag_checking
      // FIXME: Avoid checking frange, as it currently pessimizes some ranges:
      //
      // gfortran.dg/pr49472.f90 pessimizes [0.0, 1.0] into [-0.0, 1.0].
      && !is_a <frange> (r)
      && !r.undefined_p ())
    {
      value_range tmp (r);
      get_vrange (tmp, r.type ());
      gcc_checking_assert (tmp == r);
    }
}

// Restore R from storage.

void
vrange_storage::get_vrange (vrange &r, tree type) const
{
  if (is_a <irange> (r))
    {
      const irange_storage *s = static_cast <const irange_storage *> (this);
      s->get_irange (as_a <irange> (r), type);
    }
  else if (is_a <prange> (r))
    {
      const prange_storage *s = static_cast <const prange_storage *> (this);
      s->get_prange (as_a <prange> (r), type);
    }
  else if (is_a <frange> (r))
    {
      const frange_storage *s = static_cast <const frange_storage *> (this);
      s->get_frange (as_a <frange> (r), type);
    }
  else
    gcc_unreachable ();
}

// Return TRUE if storage can fit R.

bool
vrange_storage::fits_p (const vrange &r) const
{
  if (is_a <irange> (r))
    {
      const irange_storage *s = static_cast <const irange_storage *> (this);
      return s->fits_p (as_a <irange> (r));
    }
  if (is_a <prange> (r))
    {
      const prange_storage *s = static_cast <const prange_storage *> (this);
      return s->fits_p (as_a <prange> (r));
    }
  if (is_a <frange> (r))
    {
      const frange_storage *s = static_cast <const frange_storage *> (this);
      return s->fits_p (as_a <frange> (r));
    }
  gcc_unreachable ();
  return false;
}

// Return TRUE if the range in storage is equal to R.  It is the
// caller's responsibility to verify that the type of the range in
// storage matches that of R.

bool
vrange_storage::equal_p (const vrange &r) const
{
  if (is_a <irange> (r))
    {
      const irange_storage *s = static_cast <const irange_storage *> (this);
      return s->equal_p (as_a <irange> (r));
    }
  if (is_a <prange> (r))
    {
      const prange_storage *s = static_cast <const prange_storage *> (this);
      return s->equal_p (as_a <prange> (r));
    }
  if (is_a <frange> (r))
    {
      const frange_storage *s = static_cast <const frange_storage *> (this);
      return s->equal_p (as_a <frange> (r));
    }
  gcc_unreachable ();
}

//============================================================================
// irange_storage implementation
//============================================================================

unsigned short *
irange_storage::write_lengths_address ()
{
  return (unsigned short *)&m_val[(m_num_ranges * 2 + 2)
				  * WIDE_INT_MAX_HWIS (m_precision)];
}

const unsigned short *
irange_storage::lengths_address () const
{
  return const_cast <irange_storage *> (this)->write_lengths_address ();
}

// Allocate a new irange_storage object initialized to R.

irange_storage *
irange_storage::alloc (vrange_internal_alloc &allocator, const irange &r)
{
  size_t size = irange_storage::size (r);
  irange_storage *p = static_cast <irange_storage *> (allocator.alloc (size));
  new (p) irange_storage (r);
  return p;
}

// Initialize the storage with R.

irange_storage::irange_storage (const irange &r)
  : m_max_ranges (r.num_pairs ())
{
  m_num_ranges = m_max_ranges;
  set_irange (r);
}

static inline void
write_wide_int (HOST_WIDE_INT *&val, unsigned short *&len, const wide_int &w)
{
  *len = w.get_len ();
  for (unsigned i = 0; i < *len; ++i)
    *val++ = w.elt (i);
  ++len;
}

// Store R into the current storage.

void
irange_storage::set_irange (const irange &r)
{
  gcc_checking_assert (fits_p (r));

  if (r.undefined_p ())
    {
      m_kind = VR_UNDEFINED;
      return;
    }
  if (r.varying_p ())
    {
      m_kind = VR_VARYING;
      return;
    }

  m_precision = TYPE_PRECISION (r.type ());
  m_num_ranges = r.num_pairs ();
  m_kind = VR_RANGE;

  HOST_WIDE_INT *val = &m_val[0];
  unsigned short *len = write_lengths_address ();

  for (unsigned i = 0; i < r.num_pairs (); ++i)
    {
      write_wide_int (val, len, r.lower_bound (i));
      write_wide_int (val, len, r.upper_bound (i));
    }

  // TODO: We could avoid streaming out the value if the mask is -1.
  irange_bitmask bm = r.m_bitmask;
  write_wide_int (val, len, bm.value ());
  write_wide_int (val, len, bm.mask ());
}

static inline void
read_wide_int (wide_int &w,
	       const HOST_WIDE_INT *val, unsigned short len, unsigned prec)
{
  trailing_wide_int_storage stow (prec, &len,
				  const_cast <HOST_WIDE_INT *> (val));
  w = trailing_wide_int (stow);
}

// Restore a range of TYPE from storage into R.

void
irange_storage::get_irange (irange &r, tree type) const
{
  if (m_kind == VR_UNDEFINED)
    {
      r.set_undefined ();
      return;
    }
  if (m_kind == VR_VARYING)
    {
      r.set_varying (type);
      return;
    }

  gcc_checking_assert (TYPE_PRECISION (type) == m_precision);
  const HOST_WIDE_INT *val = &m_val[0];
  const unsigned short *len = lengths_address ();

  // Handle the common case where R can fit the new range.
  if (r.m_max_ranges >= m_num_ranges)
    {
      r.m_kind = VR_RANGE;
      r.m_num_ranges = m_num_ranges;
      r.m_type = type;
      for (unsigned i = 0; i < m_num_ranges * 2; ++i)
	{
	  read_wide_int (r.m_base[i], val, *len, m_precision);
	  val += *len++;
	}
    }
  // Otherwise build the range piecewise.
  else
    {
      r.set_undefined ();
      for (unsigned i = 0; i < m_num_ranges; ++i)
	{
	  wide_int lb, ub;
	  read_wide_int (lb, val, *len, m_precision);
	  val += *len++;
	  read_wide_int (ub, val, *len, m_precision);
	  val += *len++;
	  int_range<1> tmp (type, lb, ub);
	  r.union_ (tmp);
	}
    }

  wide_int bits_value, bits_mask;
  read_wide_int (bits_value, val, *len, m_precision);
  val += *len++;
  read_wide_int (bits_mask, val, *len, m_precision);
  r.m_bitmask = irange_bitmask (bits_value, bits_mask);
  if (r.m_kind == VR_VARYING)
    r.m_kind = VR_RANGE;

  if (flag_checking)
    r.verify_range ();
}

bool
irange_storage::equal_p (const irange &r) const
{
  if (m_kind == VR_UNDEFINED || r.undefined_p ())
    return m_kind == r.m_kind;
  if (m_kind == VR_VARYING || r.varying_p ())
    return m_kind == r.m_kind;

  // ?? We could make this faster by doing the comparison in place,
  // without going through get_irange.
  int_range_max tmp;
  get_irange (tmp, r.type ());
  return tmp == r;
}

// Return the size in bytes to allocate storage that can hold R.

size_t
irange_storage::size (const irange &r)
{
  if (r.undefined_p ())
    return sizeof (irange_storage);

  unsigned prec = TYPE_PRECISION (r.type ());
  unsigned n = r.num_pairs () * 2 + 2;
  unsigned hwi_size = ((n * WIDE_INT_MAX_HWIS (prec) - 1)
		       * sizeof (HOST_WIDE_INT));
  unsigned len_size = n * sizeof (unsigned short);
  return sizeof (irange_storage) + hwi_size + len_size;
}

// Return TRUE if R fits in the current storage.

bool
irange_storage::fits_p (const irange &r) const
{
  return m_max_ranges >= r.num_pairs ();
}

void
irange_storage::dump () const
{
  fprintf (stderr, "irange_storage (prec=%d, ranges=%d):\n",
	   m_precision, m_num_ranges);

  if (m_num_ranges == 0)
    return;

  const HOST_WIDE_INT *val = &m_val[0];
  const unsigned short *len = lengths_address ();
  int i, j;

  fprintf (stderr, "  lengths = [ ");
  for (i = 0; i < m_num_ranges * 2 + 2; ++i)
    fprintf (stderr, "%d ", len[i]);
  fprintf (stderr, "]\n");

  for (i = 0; i < m_num_ranges; ++i)
    {
      for (j = 0; j < *len; ++j)
	fprintf (stderr, "  [PAIR %d] LB " HOST_WIDE_INT_PRINT_DEC "\n", i,
		 *val++);
      ++len;
      for (j = 0; j < *len; ++j)
	fprintf (stderr, "  [PAIR %d] UB " HOST_WIDE_INT_PRINT_DEC "\n", i,
		 *val++);
      ++len;
    }

  // Dump value/mask pair.
  for (j = 0; j < *len; ++j)
    fprintf (stderr, "  [VALUE] " HOST_WIDE_INT_PRINT_DEC "\n", *val++);
  ++len;
  for (j = 0; j < *len; ++j)
    fprintf (stderr, "  [MASK] " HOST_WIDE_INT_PRINT_DEC "\n", *val++);
}

DEBUG_FUNCTION void
debug (const irange_storage &storage)
{
  storage.dump ();
  fprintf (stderr, "\n");
}

//============================================================================
// frange_storage implementation
//============================================================================

// Allocate a new frange_storage object initialized to R.

frange_storage *
frange_storage::alloc (vrange_internal_alloc &allocator, const frange &r)
{
  size_t size = sizeof (frange_storage);
  frange_storage *p = static_cast <frange_storage *> (allocator.alloc (size));
  new (p) frange_storage (r);
  return p;
}

void
frange_storage::set_frange (const frange &r)
{
  gcc_checking_assert (fits_p (r));

  m_kind = r.m_kind;
  m_min = r.m_min;
  m_max = r.m_max;
  m_pos_nan = r.m_pos_nan;
  m_neg_nan = r.m_neg_nan;
}

void
frange_storage::get_frange (frange &r, tree type) const
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
  if (m_kind == VR_UNDEFINED)
    {
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
frange_storage::equal_p (const frange &r) const
{
  if (r.undefined_p ())
    return m_kind == VR_UNDEFINED;

  frange tmp;
  get_frange (tmp, r.type ());
  return tmp == r;
}

bool
frange_storage::fits_p (const frange &) const
{
  return true;
}

//============================================================================
// prange_storage implementation
//============================================================================

prange_storage *
prange_storage::alloc (vrange_internal_alloc &allocator, const prange &r)
{
  size_t size = sizeof (prange_storage);
  if (!r.undefined_p ())
    {
      unsigned prec = TYPE_PRECISION (r.type ());
      size += trailing_wide_ints<NINTS>::extra_size (prec);
    }
  prange_storage *p = static_cast <prange_storage *> (allocator.alloc (size));
  new (p) prange_storage (r);
  return p;
}

// Initialize the storage with R.

prange_storage::prange_storage (const prange &r)
{
  // It is the caller's responsibility to allocate enough space such
  // that the precision fits.
  if (r.undefined_p ())
    // Undefined ranges do not require any extra space for trailing
    // wide ints.
    m_trailing_ints.set_precision (0);
  else
    m_trailing_ints.set_precision (TYPE_PRECISION (r.type ()));

  set_prange (r);
}

void
prange_storage::set_prange (const prange &r)
{
  if (r.undefined_p ())
    m_kind = VR_UNDEFINED;
  else if (r.varying_p ())
    m_kind = VR_VARYING;
  else
    {
      m_kind = VR_RANGE;
      set_low (r.lower_bound ());
      set_high (r.upper_bound ());
      irange_bitmask bm = r.m_bitmask;
      set_value (bm.value ());
      set_mask (bm.mask ());
    }
}

void
prange_storage::get_prange (prange &r, tree type) const
{
  gcc_checking_assert (r.supports_type_p (type));

  if (m_kind == VR_UNDEFINED)
    r.set_undefined ();
  else if (m_kind == VR_VARYING)
    r.set_varying (type);
  else
    {
      gcc_checking_assert (m_kind == VR_RANGE);
      gcc_checking_assert (TYPE_PRECISION (type) == m_trailing_ints.get_precision ());
      r.m_kind = VR_RANGE;
      r.m_type = type;
      r.m_min = get_low ();
      r.m_max = get_high ();
      r.m_bitmask = irange_bitmask (get_value (), get_mask ());
      if (flag_checking)
	r.verify_range ();
    }
}

bool
prange_storage::equal_p (const prange &r) const
{
  if (r.undefined_p ())
    return m_kind == VR_UNDEFINED;

  prange tmp;
  get_prange (tmp, r.type ());
  return tmp == r;
}

bool
prange_storage::fits_p (const prange &r) const
{
  // Undefined ranges always fit, because they don't store anything in
  // the trailing wide ints.
  if (r.undefined_p ())
    return true;

  return TYPE_PRECISION (r.type ()) <= m_trailing_ints.get_precision ();
}


static vrange_allocator ggc_vrange_allocator (true);

vrange_storage *ggc_alloc_vrange_storage (tree type)
{
  return ggc_vrange_allocator.clone_varying (type);
}

vrange_storage *ggc_alloc_vrange_storage (const vrange &r)
{
  return ggc_vrange_allocator.clone (r);
}
