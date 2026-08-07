/* Support routines for value ranges.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
   Major hacks by Aldy Hernandez <aldyh@redhat.com> and
   Andrew MacLeod <amacleod@redhat.com>.

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
#include "value-range-pretty-print.h"
#include "value-range-storage.h"
#include "fold-const.h"
#include "gimple-range.h"
#include "tree-dfa.h"
#include "tree-affine.h"

// Return the bitmask inherent in a range :   TYPE [MIN, MAX].
// This used to be get_bitmask_from_range ().

irange_bitmask::irange_bitmask (tree type,
				const wide_int &min, const wide_int &max)
{
  unsigned prec = TYPE_PRECISION (type);
  // All the bits of a singleton are known.
  if (min == max)
    {
      m_mask = wi::zero (prec);
      m_value = min;
    }
  else
    {
      wide_int xorv = min ^ max;
      // Mask will have leading zeros for all leading bits that are
      // common, both zeros and ones.
      m_mask = wi::mask (prec - wi::clz (xorv), false, prec);
      // Now set value to those bits which are known, and zero the rest.
      m_value = ~m_mask & min;
    }
}

// Return a range in R of TYPE for this bitmask which encompasses
// a set of valid values which are allowable for this bitmask/value
// combination.  If false is returned, no range was set.

bool
irange_bitmask::range_from_mask (irange &r, tree type) const
{
  if (unknown_p ())
    return false;

  gcc_checking_assert ((value () & mask ()) == 0);
  unsigned popcount = wi::popcount (mask ());

  // For 0, 1 or 2 bits set, create a range with only the allowed values.
  if (popcount <= 2)
    {
      // VALUE is always a valid range.
      r.set (type, value (), value ());
      // If there are bits in mask, (VALUE | MASK) is also valid.
      if (popcount >= 1)
	r.union_ (int_range<1> (type, value () | mask (), value () | mask ()));
      // If there are 2 bits set, add the other 2 possible values.
      if (popcount == 2)
	{
	  // Extract the two 1-bit masks into lb and ub.
	  wide_int lb = mask () & -mask ();		// Lowest set bit.
	  wide_int ub = mask () & (mask () - 1);	// The other bit.
	  r.union_ (int_range<1> (type, value () | lb, value () | lb));
	  r.union_ (int_range<1> (type, value () | ub, value () | ub));
	}
      return true;
    }

  // Otherwise, calculate the valid range allowed by the bitmask.
  int prec = TYPE_PRECISION (type);
  wide_int ub = mask () | value ();
  wide_int sign_bit = wi::one (prec) << (prec - 1);
  wide_int sign_mask = mask () & sign_bit;
  wide_int sign_value = value () & sign_bit;
  // Create a lower and upper bound.
  // If unsigned, or the sign is known to be positive, create [lb, ub]
  if (TYPE_SIGN (type) == UNSIGNED || (sign_mask == 0 && sign_value == 0))
    r.set (type, value (), mask () | value ());
  // If the sign bit is KNOWN to be 1, we have a completely negative range.
  else if (sign_mask == 0 && sign_value != 0)
    r.set (type, value (), value () | (mask () & ~sign_bit));
  else
    {
      // Otherwise there are 2 ranges, a negative and positive interval.
      wide_int neg_base = value () | sign_bit;
      wide_int pos_mask = mask () & ~sign_bit;
      r.set  (type, neg_base , neg_base | pos_mask);
      r.union_ (int_range<1> (type, value (), value () | pos_mask));
    }

  // If the mask doesn't have a trailing zero, there is nothing else to filter.
  int z = wi::ctz (mask ());
  if (z == 0)
    return true;

  // Remove the [0, X] values which the trailing-zero mask rules out.
  // For example, if z == 4, the mask is 0xFFF0, and the lowest 4 bits
  // define the range [0, 15]. Only (value & low_mask) is allowed.
  ub = (wi::one (prec) << z) - 1;  // Upper bound of range.
  int_range<4> mask_range (type, wi::zero (prec), ub);
  // Remove the valid value from the excluded range and form an anti-range.
  wide_int allow = value () & ub;
  mask_range.intersect (int_range<2> (type, allow, allow, VR_ANTI_RANGE));
  bool res = mask_range.invert ();
  gcc_checking_assert (res);
  r.intersect (mask_range);

  if (TYPE_SIGN (type) == SIGNED)
    {
      // For signed negative values, find the lowest value with trailing zeros.
      // This forms a range such as [-512, -1] for z=9.
      wide_int lb = -(wi::one (prec) << z);
      int_range<4> mask_range (type, lb, wi::minus_one (prec));
      // Remove the one allowed value from that set.
      wide_int allow = value () | lb;
      mask_range.intersect (int_range<2> (type, allow, allow, VR_ANTI_RANGE));
      res = mask_range.invert ();
      gcc_checking_assert (res);
      r.intersect (mask_range);
    }
  return true;
}


void
irange::accept (const vrange_visitor &v) const
{
  v.visit (*this);
}

void
value_range::dump (FILE *out) const
{
  if (m_vrange)
    m_vrange->dump (out);
  else
    fprintf (out, "NULL");
}

void
value_range::print (pretty_printer *pp) const
{
  if (m_vrange)
    {
      vrange_printer vrange_pp (pp);
      m_vrange->accept (vrange_pp);
    }
  else
    pp_string (pp, "NULL");
}

DEBUG_FUNCTION void
debug (const value_range &r)
{
  r.dump (stderr);
  fprintf (stderr, "\n");
}

DEBUG_FUNCTION void
debug (const irange_bitmask &bm)
{
  bm.dump (stderr);
  fprintf (stderr, "\n");
}

// Definitions for unsupported_range.

void
unsupported_range::accept (const vrange_visitor &v) const
{
  v.visit (*this);
}

void
vrange::update_bitmask (const class irange_bitmask &)
{
}

irange_bitmask
vrange::get_bitmask () const
{
  // Return all unknown bits for the given precision.
  return irange_bitmask (TYPE_PRECISION (type ()));
}

bool
unsupported_range::contains_p (tree) const
{
  return varying_p ();
}

bool
unsupported_range::singleton_p (tree *) const
{
  return false;
}

void
unsupported_range::set (tree min, tree, value_range_kind)
{
  set_varying (TREE_TYPE (min));
}

tree
unsupported_range::type () const
{
  return void_type_node;
}

bool
unsupported_range::supports_type_p (const_tree) const
{
  return false;
}

void
unsupported_range::set_undefined ()
{
  m_kind = VR_UNDEFINED;
}

void
unsupported_range::set_varying (tree)
{
  m_kind = VR_VARYING;
}

bool
unsupported_range::union_ (const vrange &v)
{
  const unsupported_range &r = as_a <unsupported_range> (v);

  if (r.undefined_p () || varying_p ())
    return false;
  if (undefined_p () || r.varying_p ())
    {
      operator= (r);
      return true;
    }
  gcc_unreachable ();
  return false;
}

bool
unsupported_range::intersect (const vrange &v)
{
  const unsupported_range &r = as_a <unsupported_range> (v);

  if (undefined_p () || r.varying_p ())
    return false;
  if (r.undefined_p ())
    {
      set_undefined ();
      return true;
    }
  if (varying_p ())
    {
      operator= (r);
      return true;
    }
  gcc_unreachable ();
  return false;
}

bool
unsupported_range::zero_p () const
{
  return false;
}

bool
unsupported_range::contains_zero_p () const
{
  return varying_p ();
}

void
unsupported_range::set_nonzero (tree type)
{
  set_varying (type);
}

void
unsupported_range::set_zero (tree type)
{
  set_varying (type);
}

void
unsupported_range::set_nonnegative (tree type)
{
  set_varying (type);
}

bool
unsupported_range::fits_p (const vrange &) const
{
  return true;
}

unsupported_range &
unsupported_range::operator= (const unsupported_range &r)
{
  if (r.undefined_p ())
    set_undefined ();
  else if (r.varying_p ())
    set_varying (void_type_node);
  else
    gcc_unreachable ();
  return *this;
}

tree
unsupported_range::lbound () const
{
  return NULL;
}

tree
unsupported_range::ubound () const
{
  return NULL;
}

// Assignment operator for generic ranges.  Copying incompatible types
// is not allowed.

vrange &
vrange::operator= (const vrange &src)
{
  if (is_a <irange> (src))
    as_a <irange> (*this) = as_a <irange> (src);
  else if (is_a <prange> (src))
    as_a <prange> (*this) = as_a <prange> (src);
  else if (is_a <frange> (src))
    as_a <frange> (*this) = as_a <frange> (src);
  else
    {
      gcc_checking_assert (is_a <unsupported_range> (src));
      m_kind = src.m_kind;
    }
  return *this;
}

// Equality operator for generic ranges.

bool
vrange::operator== (const vrange &src) const
{
  if (is_a <irange> (src))
    return as_a <irange> (*this) == as_a <irange> (src);
  if (is_a <prange> (src))
    return as_a <prange> (*this) == as_a <prange> (src);
  if (is_a <frange> (src))
    return as_a <frange> (*this) == as_a <frange> (src);
  gcc_unreachable ();
}

// Wrapper for vrange_printer to dump a range to a file.

void
vrange::dump (FILE *file) const
{
  pretty_printer pp;
  pp_needs_newline (&pp) = true;
  pp.set_output_stream (file);
  vrange_printer vrange_pp (&pp);
  this->accept (vrange_pp);
  pp_flush (&pp);
}

void
irange_bitmask::dump (FILE *file) const
{
  char buf[WIDE_INT_PRINT_BUFFER_SIZE], *p;
  pretty_printer pp;

  pp_needs_newline (&pp) = true;
  pp.set_output_stream (file);
  pp_string (&pp, "MASK ");
  unsigned len_mask, len_val;
  if (print_hex_buf_size (m_mask, &len_mask)
      | print_hex_buf_size (m_value, &len_val))
    p = XALLOCAVEC (char, MAX (len_mask, len_val));
  else
    p = buf;
  print_hex (m_mask, p);
  pp_string (&pp, p);
  pp_string (&pp, " VALUE ");
  print_hex (m_value, p);
  pp_string (&pp, p);
  pp_flush (&pp);
}

namespace inchash
{

void
add_vrange (const vrange &v, inchash::hash &hstate,
	     unsigned int)
{
  if (v.undefined_p ())
    {
      hstate.add_int (VR_UNDEFINED);
      return;
    }
  // Types are ignored throughout to inhibit two ranges being equal
  // but having different hash values.  This can happen when two
  // ranges are equal and their types are different (but
  // types_compatible_p is true).
  if (is_a <irange> (v))
    {
      const irange &r = as_a <irange> (v);
      if (r.varying_p ())
	hstate.add_int (VR_VARYING);
      else
	hstate.add_int (VR_RANGE);
      for (unsigned i = 0; i < r.num_pairs (); ++i)
	{
	  hstate.add_wide_int (r.lower_bound (i));
	  hstate.add_wide_int (r.upper_bound (i));
	}
      irange_bitmask bm = r.get_bitmask ();
      hstate.add_wide_int (bm.value ());
      hstate.add_wide_int (bm.mask ());
      return;
    }
  if (is_a <prange> (v))
    {
      const prange &r = as_a <prange> (v);
      if (r.varying_p ())
	hstate.add_int (VR_VARYING);
      else
	{
	  hstate.add_int (VR_RANGE);
	  hstate.add_wide_int (r.lower_bound ());
	  hstate.add_wide_int (r.upper_bound ());
	  irange_bitmask bm = r.get_bitmask ();
	  hstate.add_wide_int (bm.value ());
	  hstate.add_wide_int (bm.mask ());
	  bool flag = false;
	  tree tmp = r.pt_invariant ();
	  if (tmp)
	    flag = true;
	  else
	    tmp = r.pt_invariant_away ();
	  hstate.add_ptr (tmp);
	  hstate.add_flag (flag);
	}
      return;
    }
  if (is_a <frange> (v))
    {
      const frange &r = as_a <frange> (v);
      if (r.known_isnan ())
	hstate.add_int (VR_NAN);
      else
	{
	  hstate.add_int (r.varying_p () ? VR_VARYING : VR_RANGE);
	  hstate.add_real_value (r.lower_bound ());
	  hstate.add_real_value (r.upper_bound ());
	}
      nan_state nan = r.get_nan_state ();
      hstate.add_int (nan.pos_p ());
      hstate.add_int (nan.neg_p ());
      return;
    }
  gcc_unreachable ();
}

} //namespace inchash

bool
irange::nonnegative_p () const
{
  return wi::ge_p (lower_bound (), 0, TYPE_SIGN (type ()));
}

bool
irange::nonpositive_p () const
{
  return wi::le_p (upper_bound (), 0, TYPE_SIGN (type ()));
}

bool
irange::supports_type_p (const_tree type) const
{
  return supports_p (type);
}

// Return TRUE if R fits in THIS.

bool
irange::fits_p (const vrange &r) const
{
  return m_max_ranges >= as_a <irange> (r).num_pairs ();
}

void
irange::set_nonnegative (tree type)
{
  set (type,
       wi::zero (TYPE_PRECISION (type)),
       wi::to_wide (TYPE_MAX_VALUE (type)));
}


// Set the points to info for EXPR if possible.  POINTS_TO_P is true if it
// points to EXPR, and FALSE if it points away.

void
prange::set_pt (tree expr, bool points_to_p)
{
  gcc_checking_assert (m_kind != VR_UNDEFINED);
  gcc_checking_assert (!expr || TREE_CODE (expr) != SSA_NAME);

  m_pt = NULL_TREE;
  m_points_to_p = false;

  // A zero range means no points-to info.
  if (zero_p ())
    return;

  // No points to initially may make this VARYING.
  if (varying_compatible_p ())
    set_varying (type ());
  else
    m_kind = VR_RANGE;

  if (!expr)
    return;

  gcc_checking_assert (TREE_CODE (expr) == ADDR_EXPR);

  // Ensure only constants get through for now.
  if (!is_gimple_min_invariant (expr))
    return;

  aff_tree offset;
  poly_widest_int size;
  tree obj = TREE_OPERAND (expr, 0);
  tree base = get_inner_reference_aff (obj, &offset, &size);

  if (!base)
    return;
  if (!offset.offset.is_constant ())
    return;
  if (!size.is_constant ())
    return;

  m_pt = expr;
  m_points_to_p = points_to_p;
  m_kind = VR_RANGE;
}

// Return object/allocation the pointer refers into, otherwise NULL_TREE.

tree
prange::pt_base () const
{
  if (!m_pt)
    return NULL_TREE;

  aff_tree off;
  poly_widest_int sz;

  gcc_checking_assert (m_pt);
  return get_inner_reference_aff (m_pt, &off, &sz);
}

// Return possible byte offset range from BASE.

void
prange::pt_offset (irange &r) const
{
  aff_tree off;
  poly_widest_int sz;

  gcc_checking_assert (m_pt);

  get_inner_reference_aff (m_pt, &off, &sz);
  gcc_checking_assert (off.offset.is_constant ());

  widest_int w = off.offset.coeffs[0];
  wide_int w2 = wi::to_wide (wide_int_to_tree (sizetype, w));
  r.set (sizetype, w2, w2);
}

// Return possible size range of the referenced object.

void
prange::pt_size (irange &r) const
{
  aff_tree off;
  poly_widest_int sz;

  gcc_checking_assert (m_pt);

  get_inner_reference_aff (m_pt, &off, &sz);
  gcc_checking_assert (sz.is_constant ());

  widest_int w = sz.coeffs[0];
  wide_int w2 = wi::to_wide (wide_int_to_tree (sizetype, w));
  r.set (sizetype, w2, w2);
}
// Prange implementation.

void
prange::accept (const vrange_visitor &v) const
{
  v.visit (*this);
}

void
prange::set_nonnegative (tree type)
{
  set (type,
       wi::zero (TYPE_PRECISION (type)),
       wi::max_value (TYPE_PRECISION (type), UNSIGNED));
}

void
prange::set (tree min, tree max, value_range_kind kind)
{
  return set (TREE_TYPE (min), wi::to_wide (min), wi::to_wide (max), kind);
}

void
prange::set (tree type, const wide_int &min, const wide_int &max,
	     value_range_kind kind)
{
  if (kind == VR_UNDEFINED)
    {
      set_undefined ();
      return;
    }
  if (kind == VR_VARYING)
    {
      set_varying (type);
      return;
    }
  if (kind == VR_ANTI_RANGE)
    {
      gcc_checking_assert (min == 0 && max == 0);
      set_nonzero (type);
      return;
    }
  m_type = type;
  m_min = min;
  m_max = max;
  set_pt_unknown ();

  if (m_min == 0 && m_max == -1)
    {
      m_kind = VR_VARYING;
      m_bitmask.set_unknown (TYPE_PRECISION (type));
      if (flag_checking)
	verify_range ();
      return;
    }

  m_kind = VR_RANGE;
  m_bitmask = irange_bitmask (type, min, max);
  if (flag_checking)
    verify_range ();
}

bool
prange::contains_p (const wide_int &w) const
{
  if (undefined_p ())
    return false;

  if (varying_p ())
    return true;

  return (wi::le_p (lower_bound (), w, UNSIGNED)
	  && wi::ge_p (upper_bound (), w, UNSIGNED));
}

bool
prange::singleton_p (tree *result) const
{
  if (m_kind == VR_RANGE && lower_bound () == upper_bound ())
    {
      if (result)
	*result = wide_int_to_tree (type (), m_min);
      return true;
    }
  return false;
}

tree
prange::lbound () const
{
  return wide_int_to_tree (type (), m_min);
}

tree
prange::ubound () const
{
  return wide_int_to_tree (type (), m_max);
}

bool
prange::union_ (const vrange &v)
{
  const prange &r = as_a <prange> (v);

  if (r.undefined_p ())
    return false;
  if (undefined_p ())
    {
      *this = r;
      if (flag_checking)
	verify_range ();
      return true;
    }
  if (varying_p ())
    return false;
  if (r.varying_p ())
    {
      set_varying (type ());
      return true;
    }

  wide_int new_lb = wi::min (r.lower_bound (), lower_bound (), UNSIGNED);
  wide_int new_ub = wi::max (r.upper_bound (), upper_bound (), UNSIGNED);
  prange new_range (type (), new_lb, new_ub);
  new_range.m_bitmask.union_ (m_bitmask);
  new_range.m_bitmask.union_ (r.m_bitmask);

  // Keep it simple, either both point to the same thing or both
  // do not point to the same thing, or we drop the points to info.
  if (pt_equal_p (r))
    new_range.set_pt (*this);

  if (new_range.varying_compatible_p ())
    {
      set_varying (type ());
      return true;
    }
  if (flag_checking)
    new_range.verify_range ();
  if (new_range == *this)
    return false;
  *this = new_range;
  return true;
}

bool
prange::intersect (const vrange &v)
{
  const prange &r = as_a <prange> (v);
  gcc_checking_assert (undefined_p () || r.undefined_p ()
		       || range_compatible_p (type (), r.type ()));

  if (undefined_p ())
    return false;
  if (r.undefined_p ())
    {
      set_undefined ();
      return true;
    }
  if (r.varying_p ())
    return false;
  if (varying_p ())
    {
      *this = r;
      return true;
    }

  // If this points to and away, results are undefined,
  if (pt_inverted_p (r))
    {
      set_undefined ();
      return true;
    }

  prange save = *this;
  m_min = wi::max (r.lower_bound (), lower_bound (), UNSIGNED);
  m_max = wi::min (r.upper_bound (), upper_bound (), UNSIGNED);
  if (wi::gt_p (m_min, m_max, UNSIGNED))
    {
      set_undefined ();
      return true;
    }

  // Intersect all bitmasks: the old one, the new one, and the other operand's.
  irange_bitmask new_bitmask (m_type, m_min, m_max);
  if (!m_bitmask.intersect (new_bitmask))
    set_undefined ();
  else if (!m_bitmask.intersect (r.m_bitmask))
    set_undefined ();
  // If only one object points to something, that is the intersection.
  else if (pt_unknown_p () && !r.pt_unknown_p ())
    set_pt (r);
  else if (!pt_unknown_p () && !r.pt_unknown_p ())
    {
      // If both point to something, we want to be careful.  Without aliasing
      // 2 different values can point to the same thing, so UNDEFINED is
      // not appropriate, but we want to keep the rule that intersection
      // never becomes larger.
      // If the other object points to something specific, and this one does
      // not, use the specific one. Otherwise leave the range as is.
      if (pt_invariant_away () && r.pt_invariant ())
	set_pt (r);
    }

  //  If this evolves to zero, clear all points-to info.
  if (zero_p () && !pt_unknown_p ())
    set_pt_unknown ();

  if (varying_compatible_p ())
    {
      set_varying (type ());
      return true;
    }

  if (flag_checking)
    verify_range ();
  if (*this == save)
    return false;
  return true;
}

prange &
prange::operator= (const prange &src)
{
  m_type = src.m_type;
  m_kind = src.m_kind;
  m_min = src.m_min;
  m_max = src.m_max;
  m_bitmask = src.m_bitmask;
  set_pt (src);
  if (flag_checking)
    verify_range ();
  return *this;
}

bool
prange::operator== (const prange &src) const
{
  if (m_kind == src.m_kind)
    {
      if (undefined_p ())
	return true;

      if (varying_p ())
	return types_compatible_p (type (), src.type ());

      if (!pt_equal_p (src))
	return false;

      return (m_min == src.m_min && m_max == src.m_max
	      && m_bitmask == src.m_bitmask);
    }
  return false;
}


// Return the inverse of a range.  Return false if thre is no invert
// calculatable.

bool
prange::invert ()
{
  if (undefined_p () || varying_p ())
    return false;

  // Invert the points_to object. If that worked, this is done.
  if (pt_invert ())
    return true;
  else
    set_pt_unknown ();

  wide_int new_lb, new_ub;
  unsigned prec = TYPE_PRECISION (type ());
  wide_int type_min = wi::zero (prec);
  wide_int type_max = wi::max_value (prec, UNSIGNED);
  wi::overflow_type ovf;

  if (lower_bound () == type_min)
    {
      new_lb = wi::add (upper_bound (), 1, UNSIGNED, &ovf);
      if (ovf)
	new_lb = type_min;
      new_ub = type_max;
      set (type (), new_lb, new_ub);
    }
  else if (upper_bound () == type_max)
    {
      wi::overflow_type ovf;
      new_lb = type_min;
      new_ub = wi::sub (lower_bound (), 1, UNSIGNED, &ovf);
      if (ovf)
	new_ub = type_max;
      set (type (), new_lb, new_ub);
    }
  else
    set_varying (type ());
  return true;
}

void
prange::verify_range () const
{
  gcc_checking_assert (m_discriminator == VR_PRANGE);

  if (m_kind == VR_UNDEFINED)
    {
      gcc_checking_assert (pt_unknown_p ());
      return;
    }

  gcc_checking_assert (supports_p (type ()));

  if (m_kind == VR_VARYING)
    {
      gcc_checking_assert (varying_compatible_p ());
      return;
    }
  gcc_checking_assert (!varying_compatible_p ());
  gcc_checking_assert (m_kind == VR_RANGE);
  if (!pt_unknown_p ())
    {
      gcc_checking_assert (!varying_p ());
      gcc_checking_assert (!undefined_p ());
      gcc_checking_assert (!zero_p ());
    }
}

void
prange::update_bitmask (const irange_bitmask &bm)
{
  gcc_checking_assert (!undefined_p ());

  // If all the bits are known, this is a singleton.
  if (bm.mask () == 0)
    {
      set (type (), bm.value (), bm.value ());
      return;
    }

  // Drop VARYINGs with known bits to a plain range.
  if (m_kind == VR_VARYING && !bm.unknown_p ())
    m_kind = VR_RANGE;

  m_bitmask = bm;
  if (varying_compatible_p ())
    m_kind = VR_VARYING;

  if (flag_checking)
    verify_range ();
}


// Frange implementation.

void
frange::accept (const vrange_visitor &v) const
{
  v.visit (*this);
}

bool
frange::fits_p (const vrange &) const
{
  return true;
}

// Compare two range endpoints.
//
// In IEEE -0.0 and +0.0 equal for comparison purposes, but as endpoints they
// are distinct.  Order -0.0 strictly below +0.0 and use this rather than
// real_less/real_compare, and the signed zeros stop needing a special case.

static int
frange_cmp (const REAL_VALUE_TYPE &a, const REAL_VALUE_TYPE &b)
{
  gcc_checking_assert (!real_isnan (&a) && !real_isnan (&b));

  if (real_less (&a, &b))
    return -1;
  if (real_less (&b, &a))
    return 1;
  if (real_iszero (&a) && real_iszero (&b))
    {
      bool nega = real_isneg (&a);
      bool negb = real_isneg (&b);
      if (nega && !negb)
	return -1;
      if (!nega && negb)
	return 1;
    }
  return 0;
}

static inline const REAL_VALUE_TYPE &
frange_min (const REAL_VALUE_TYPE &a, const REAL_VALUE_TYPE &b)
{
  return frange_cmp (a, b) <= 0 ? a : b;
}

static inline const REAL_VALUE_TYPE &
frange_max (const REAL_VALUE_TYPE &a, const REAL_VALUE_TYPE &b)
{
  return frange_cmp (a, b) >= 0 ? a : b;
}

// Return TRUE if [..., A_MAX] and [B_MIN, ...] can be fused into one interval,
// either because they overlap or because no representable value exists between
// them.  The latter is how -0.0 and +0.0 abut: there is nothing in between, so
// [x, -0.0] U [+0.0, y] is really [x, y].

static bool
frange_fusible_p (machine_mode mode, const REAL_VALUE_TYPE &a_max,
		  const REAL_VALUE_TYPE &b_min)
{
  if (frange_cmp (b_min, a_max) <= 0)
    return true;
  REAL_VALUE_TYPE next = a_max;
  frange_nextafter (mode, next, dconstinf);
  return frange_cmp (b_min, next) <= 0;
}

// Flush denormal endpoints to the appropriate 0.0.

void
frange::flush_denormals_to_zero ()
{
  if (undefined_p () || known_isnan ())
    return;

  machine_mode mode = TYPE_MODE (type ());
  frange_pair pairs[MAX_PAIRS];
  unsigned n = m_num_ranges;

  // Flush a denormal endpoint to a zero of the same sign: a +denormal lower
  // bound to +0.0, and a -denormal upper bound to -0.0.  Then set_pairs, via
  // canonicalize_zeros, rewrites the sign to whatever the flags make
  // canonical.  For example, under !HONOR_SIGNED_ZEROS (-fno-signed-zeros) a
  // range reaching zero must hold both signs of it, so:
  //
  //     [ +DENORMAL, 5.0 ]  flushes to  [ -0.0, 5.0 ]
  //
  // keeping contains_p (-0.0) true; under HONOR_SIGNED_ZEROS the sign stands
  // and it stays [ +0.0, 5.0 ].
  for (unsigned i = 0; i < n; ++i)
    {
      pairs[i] = m_pairs[i];
      if (real_isdenormal (&pairs[i].max, mode) && real_isneg (&pairs[i].max))
	pairs[i].max = dconstm0;
      if (real_isdenormal (&pairs[i].min, mode) && !real_isneg (&pairs[i].min))
	pairs[i].min = dconst0;
    }

  set_pairs (pairs, n);
}

// Canonicalize the signed zeros of a sub-range according with what the target
// and flags want:
//
//   !MODE_HAS_SIGNED_ZEROS: the mode has no signed zero, so any zero is +0.0.
//
//   !HONOR_SIGNED_ZEROS: the two zeros are one value, so widen the range to
//   include both signs of it.
//
//   Otherwise the sign is a real distinction, and we keep it.

void
frange::canonicalize_zeros (frange_pair &p)
{
  if (!MODE_HAS_SIGNED_ZEROS (TYPE_MODE (m_type)))
    {
      if (real_iszero (&p.min, 1))
	p.min.sign = 0;
      if (real_iszero (&p.max, 1))
	p.max.sign = 0;
    }
  else if (!HONOR_SIGNED_ZEROS (m_type))
    {
      if (real_iszero (&p.max, 1))
	p.max.sign = 0;
      if (real_iszero (&p.min, 0))
	p.min.sign = 1;
    }
}

// Sort, fuse and install the N intervals in PAIRS as this range's sub-ranges.
//
// Fusing merges intervals that overlap or abut.  If more than MAX_PAIRS still
// survive, the last slot swallows the surplus.

void
frange::set_pairs (frange_pair *pairs, unsigned n)
{
  gcc_checking_assert (n > 0);
  machine_mode mode = TYPE_MODE (m_type);

  // Sort by lower bound.  N is tiny (at most 2 * MAX_PAIRS).
  for (unsigned i = 0; i + 1 < n; ++i)
    for (unsigned j = i + 1; j < n; ++j)
      if (frange_cmp (pairs[j].min, pairs[i].min) < 0)
	std::swap (pairs[i], pairs[j]);

  // Fuse overlapping and abutting intervals.
  unsigned k = 0;
  for (unsigned i = 1; i < n; ++i)
    {
      if (frange_fusible_p (mode, pairs[k].max, pairs[i].min))
	{
	  if (frange_cmp (pairs[i].max, pairs[k].max) > 0)
	    pairs[k].max = pairs[i].max;
	}
      else
	pairs[++k] = pairs[i];
    }
  n = k + 1;

  // Only MAX_PAIRS fit.  Like irange, keep the first pieces and let the last
  // slot swallow the rest.
  if (n > MAX_PAIRS)
    {
      pairs[MAX_PAIRS - 1].max = pairs[n - 1].max;
      n = MAX_PAIRS;
    }

  m_kind = VR_RANGE;
  m_num_ranges = n;
  for (unsigned i = 0; i < n; ++i)
    {
      m_pairs[i] = pairs[i];
      canonicalize_zeros (m_pairs[i]);
    }

  normalize_kind ();
  if (flag_checking)
    verify_range ();
}

// Set the range to everything except the closed interval [MIN, MAX], which
// takes two sub-ranges:
//
//	[-INF, prev (MIN)] U [next (MAX), +INF]
//
// Either half falls away when the excluded interval reaches the edge of the
// domain, and if it covers the entire domain.

void
frange::set_excluding (tree type, const REAL_VALUE_TYPE &min,
		       const REAL_VALUE_TYPE &max, const nan_state &nan)
{
  gcc_checking_assert (frange_cmp (min, max) <= 0);

  machine_mode mode = TYPE_MODE (type);
  REAL_VALUE_TYPE dom_min = frange_val_min (type);
  REAL_VALUE_TYPE dom_max = frange_val_max (type);
  frange_pair pairs[MAX_PAIRS];
  unsigned n = 0;

  // PREV is the largest value below MIN, so DOM_MIN <= PREV whenever there is
  // anything below MIN at all.  Likewise for NEXT above MAX.
  if (frange_cmp (dom_min, min) < 0)
    {
      REAL_VALUE_TYPE prev = min;
      frange_nextafter (mode, prev, dconstninf);
      pairs[n++] = { dom_min, prev };
    }
  if (frange_cmp (max, dom_max) < 0)
    {
      REAL_VALUE_TYPE next = max;
      frange_nextafter (mode, next, dconstinf);
      pairs[n++] = { next, dom_max };
    }

  // The excluded interval covered the entire domain.
  if (n == 0)
    {
      if (HONOR_NANS (type) && (nan.pos_p () || nan.neg_p ()))
	set_nan (type, nan);
      else
	set_undefined ();
      return;
    }

  set (type, pairs[0].min, pairs[0].max, nan);
  if (n == 2)
    {
      frange tmp;
      tmp.set (type, pairs[1].min, pairs[1].max, nan);
      union_ (tmp);
    }
}

// Setter for franges.

void
frange::set (tree type,
	     const REAL_VALUE_TYPE &min, const REAL_VALUE_TYPE &max,
	     const nan_state &nan, value_range_kind kind)
{
  // VARYING and UNDEFINED go through set_varying() and set_undefined()
  // respectively, like we do for irange.
  gcc_checking_assert (kind == VR_RANGE || kind == VR_ANTI_RANGE);
  gcc_checking_assert (!real_isnan (&min) && !real_isnan (&max));

  if (kind == VR_ANTI_RANGE)
    {
      set_excluding (type, min, max, nan);
      return;
    }

  m_kind = kind;
  m_type = type;
  m_num_ranges = 1;
  m_pairs[0].min = min;
  m_pairs[0].max = max;
  if (HONOR_NANS (m_type))
    {
      m_pos_nan = nan.pos_p ();
      m_neg_nan = nan.neg_p ();
    }
  else
    {
      m_pos_nan = false;
      m_neg_nan = false;
    }

  canonicalize_zeros (m_pairs[0]);

  // For -ffinite-math-only we can drop ranges outside the
  // representable numbers to min/max for the type.
  if (!HONOR_INFINITIES (m_type))
    {
      REAL_VALUE_TYPE min_repr = frange_val_min (m_type);
      REAL_VALUE_TYPE max_repr = frange_val_max (m_type);
      if (real_less (&m_pairs[0].min, &min_repr))
	m_pairs[0].min = min_repr;
      else if (real_less (&max_repr, &m_pairs[0].min))
	m_pairs[0].min = max_repr;
      if (real_less (&max_repr, &m_pairs[0].max))
	m_pairs[0].max = max_repr;
      else if (real_less (&m_pairs[0].max, &min_repr))
	m_pairs[0].max = min_repr;
    }

  // Check for swapped ranges.
  gcc_checking_assert (real_compare (LE_EXPR, &min, &max));

  normalize_kind ();
}

// Setter for an frange defaulting the NAN possibility to +-NAN when
// HONOR_NANS.

void
frange::set (tree type,
	     const REAL_VALUE_TYPE &min, const REAL_VALUE_TYPE &max,
	     value_range_kind kind)
{
  set (type, min, max, nan_state (true), kind);
}

void
frange::set (tree min, tree max, value_range_kind kind)
{
  set (TREE_TYPE (min),
       *TREE_REAL_CST_PTR (min), *TREE_REAL_CST_PTR (max), kind);
}

// Normalize range to VARYING or UNDEFINED, or vice versa.  Return
// TRUE if anything changed.
//
// A range with no known properties can be dropped to VARYING.
// Similarly, a VARYING with any properties should be dropped to a
// VR_RANGE.  Normalizing ranges upon changing them ensures there is
// only one representation for a given range.

bool
frange::normalize_kind ()
{
  if (m_kind == VR_RANGE
      && m_num_ranges == 1
      && frange_val_is_min (m_pairs[0].min, m_type)
      && frange_val_is_max (m_pairs[0].max, m_type))
    {
      if (!HONOR_NANS (m_type) || (m_pos_nan && m_neg_nan))
	{
	  set_varying (m_type);
	  return true;
	}
    }
  else if (m_kind == VR_VARYING)
    {
      if (HONOR_NANS (m_type) && (!m_pos_nan || !m_neg_nan))
	{
	  m_kind = VR_RANGE;
	  m_num_ranges = 1;
	  m_pairs[0].min = frange_val_min (m_type);
	  m_pairs[0].max = frange_val_max (m_type);
	  if (flag_checking)
	    verify_range ();
	  return true;
	}
    }
  else if (m_kind == VR_NAN && !m_pos_nan && !m_neg_nan)
    set_undefined ();
  return false;
}

// Union two ranges when one is known to be a NAN.

bool
frange::union_nans (const frange &r)
{
  gcc_checking_assert (known_isnan () || r.known_isnan ());

  bool changed = false;
  if (known_isnan () && m_kind != r.m_kind)
    {
      m_kind = r.m_kind;
      m_num_ranges = r.m_num_ranges;
      for (unsigned i = 0; i < r.m_num_ranges; ++i)
	m_pairs[i] = r.m_pairs[i];
      changed = true;
    }
  if (m_pos_nan != r.m_pos_nan || m_neg_nan != r.m_neg_nan)
    {
      m_pos_nan |= r.m_pos_nan;
      m_neg_nan |= r.m_neg_nan;
      changed = true;
    }
  if (changed)
    {
      normalize_kind ();
      return true;
    }
  return false;
}

bool
frange::union_ (const vrange &v)
{
  const frange &r = as_a <frange> (v);

  if (r.undefined_p () || varying_p ())
    return false;
  if (undefined_p () || r.varying_p ())
    {
      *this = r;
      return true;
    }

  // Combine NAN info.
  if (known_isnan () || r.known_isnan ())
    return union_nans (r);

  frange save = *this;
  m_pos_nan |= r.m_pos_nan;
  m_neg_nan |= r.m_neg_nan;

  // Throw both operands' sub-ranges into the pot as set_pairs will
  // canonicalize things and hand us back at most MAX_PAIRS.
  //
  // NOTE: Both operands are already sorted and disjoint, so a merge could
  // combine them in O(n) like irange::union_ rather than have set_pairs
  // re-sort.  Not worth it while MAX_PAIRS is tiny; revisit if it grows.
  frange_pair pairs[2 * MAX_PAIRS];
  unsigned n = 0;
  for (unsigned i = 0; i < save.m_num_ranges; ++i)
    pairs[n++] = save.m_pairs[i];
  for (unsigned i = 0; i < r.m_num_ranges; ++i)
    pairs[n++] = r.m_pairs[i];

  set_pairs (pairs, n);
  return *this != save;
}

// Intersect two ranges when one is known to be a NAN.

bool
frange::intersect_nans (const frange &r)
{
  gcc_checking_assert (known_isnan () || r.known_isnan ());

  m_pos_nan &= r.m_pos_nan;
  m_neg_nan &= r.m_neg_nan;
  if (maybe_isnan ())
    set_nan (m_type, get_nan_state ());
  else
    set_undefined ();
  return true;
}

bool
frange::intersect (const vrange &v)
{
  const frange &r = as_a <frange> (v);

  if (undefined_p () || r.varying_p ())
    return false;
  if (r.undefined_p ())
    {
      set_undefined ();
      return true;
    }
  if (varying_p ())
    {
      *this = r;
      return true;
    }

  // Combine NAN info.
  if (known_isnan () || r.known_isnan ())
    return intersect_nans (r);

  frange save = *this;
  m_pos_nan &= r.m_pos_nan;
  m_neg_nan &= r.m_neg_nan;

  // Meet every sub-range against every other.  Two sorted, disjoint sets of at
  // most MAX_PAIRS each cannot yield more than MAX_PAIRS^2 pieces.
  //
  // NOTE: Since both operands are sorted, a merge-style meet like irange would
  // be O(n) and leave set_pairs nothing to sort.  Not worth it while MAX_PAIRS
  // is tiny; revisit if it grows.
  frange_pair pairs[MAX_PAIRS * MAX_PAIRS];
  unsigned n = 0;
  for (unsigned i = 0; i < save.m_num_ranges; ++i)
    for (unsigned j = 0; j < r.m_num_ranges; ++j)
      {
	const REAL_VALUE_TYPE &min
	  = frange_max (save.m_pairs[i].min, r.m_pairs[j].min);
	const REAL_VALUE_TYPE &max
	  = frange_min (save.m_pairs[i].max, r.m_pairs[j].max);
	// A reversed interval means these two do not overlap.  This also
	// catches [+0.0, -0.0], which is empty rather than nonsensical.
	if (frange_cmp (min, max) <= 0)
	  pairs[n++] = { min, max };
      }

  // Nothing but a possible NAN survives.
  if (n == 0)
    {
      if (maybe_isnan ())
	set_nan (m_type, get_nan_state ());
      else
	set_undefined ();
      return true;
    }

  set_pairs (pairs, n);
  return *this != save;
}

frange &
frange::operator= (const frange &src)
{
  m_kind = src.m_kind;
  m_type = src.m_type;
  m_num_ranges = src.m_num_ranges;
  for (unsigned i = 0; i < src.m_num_ranges; ++i)
    m_pairs[i] = src.m_pairs[i];
  m_pos_nan = src.m_pos_nan;
  m_neg_nan = src.m_neg_nan;

  if (flag_checking)
    verify_range ();
  return *this;
}

bool
frange::operator== (const frange &src) const
{
  if (m_kind == src.m_kind)
    {
      if (undefined_p ())
	return true;

      if (varying_p ())
	return types_compatible_p (m_type, src.m_type);

      bool nan1 = known_isnan ();
      bool nan2 = src.known_isnan ();
      if (nan1 || nan2)
	{
	  if (nan1 && nan2)
	    return (m_pos_nan == src.m_pos_nan
		    && m_neg_nan == src.m_neg_nan);
	  return false;
	}

      if (m_num_ranges != src.m_num_ranges)
	return false;
      for (unsigned i = 0; i < m_num_ranges; ++i)
	if (!real_identical (&m_pairs[i].min, &src.m_pairs[i].min)
	    || !real_identical (&m_pairs[i].max, &src.m_pairs[i].max))
	  return false;

      return (m_pos_nan == src.m_pos_nan
	      && m_neg_nan == src.m_neg_nan
	      && types_compatible_p (m_type, src.m_type));
    }
  return false;
}

// Return TRUE if range contains R.

bool
frange::contains_p (const REAL_VALUE_TYPE &r) const
{
  gcc_checking_assert (m_kind != VR_ANTI_RANGE);

  if (undefined_p ())
    return false;

  if (varying_p ())
    return true;

  if (real_isnan (&r))
    {
      // No NAN in range.
      if (!m_pos_nan && !m_neg_nan)
	return false;
      // Both +NAN and -NAN are present.
      if (m_pos_nan && m_neg_nan)
	return true;
      return m_neg_nan == r.sign;
    }
  if (known_isnan ())
    return false;

  for (unsigned i = 0; i < m_num_ranges; ++i)
    if (frange_cmp (r, m_pairs[i].min) >= 0
	&& frange_cmp (r, m_pairs[i].max) <= 0)
      return true;

  return false;
}

// If range is a singleton, place it in RESULT and return TRUE.  If
// RESULT is NULL, just return TRUE.
//
// A NAN can never be a singleton.

bool
frange::internal_singleton_p (REAL_VALUE_TYPE *result) const
{
  if (m_kind == VR_RANGE
      && m_num_ranges == 1
      && real_identical (&m_pairs[0].min, &m_pairs[0].max))
    {
      // Return false for any singleton that may be a NAN.
      if (HONOR_NANS (m_type) && maybe_isnan ())
	return false;

      if (MODE_COMPOSITE_P (TYPE_MODE (m_type)))
	{
	  // For IBM long doubles, if the value is +-Inf or is exactly
	  // representable in double, the other double could be +0.0
	  // or -0.0.  Since this means there is more than one way to
	  // represent a value, return false to avoid propagating it.
	  // See libgcc/config/rs6000/ibm-ldouble-format for details.
	  if (real_isinf (&m_pairs[0].min))
	    return false;
	  REAL_VALUE_TYPE r;
	  real_convert (&r, DFmode, &m_pairs[0].min);
	  if (real_identical (&r, &m_pairs[0].min))
	    return false;
	}

      if (result)
	*result = m_pairs[0].min;
      return true;
    }
  return false;
}

bool
frange::singleton_p (tree *result) const
{
  if (internal_singleton_p ())
    {
      if (result)
	*result = build_real (m_type, m_pairs[0].min);
      return true;
    }
  return false;
}

bool
frange::singleton_p (REAL_VALUE_TYPE &r) const
{
  return internal_singleton_p (&r);
}

bool
frange::supports_type_p (const_tree type) const
{
  return supports_p (type);
}

void
frange::verify_range () const
{
  if (!undefined_p ())
    gcc_checking_assert (HONOR_NANS (m_type) || !maybe_isnan ());
  switch (m_kind)
    {
    case VR_UNDEFINED:
      gcc_checking_assert (!m_type);
      return;
    case VR_VARYING:
      gcc_checking_assert (m_type);
      gcc_checking_assert (m_num_ranges == 1);
      gcc_checking_assert (frange_val_is_min (m_pairs[0].min, m_type));
      gcc_checking_assert (frange_val_is_max (m_pairs[0].max, m_type));
      if (HONOR_NANS (m_type))
	gcc_checking_assert (m_pos_nan && m_neg_nan);
      else
	gcc_checking_assert (!m_pos_nan && !m_neg_nan);
      return;
    case VR_RANGE:
      gcc_checking_assert (m_type);
      gcc_checking_assert (m_num_ranges >= 1 && m_num_ranges <= MAX_PAIRS);
      break;
    case VR_NAN:
      gcc_checking_assert (m_type);
      gcc_checking_assert (m_pos_nan || m_neg_nan);
      return;
    default:
      gcc_unreachable ();
    }

  for (unsigned i = 0; i < m_num_ranges; ++i)
    {
      // NANs cannot appear in the endpoints of a range.
      gcc_checking_assert (!real_isnan (&m_pairs[i].min)
			   && !real_isnan (&m_pairs[i].max));

      // Make sure we don't have swapped ranges.
      // This also catches [ +0.0, -0.0].
      gcc_checking_assert (frange_cmp (m_pairs[i].min, m_pairs[i].max) <= 0);

      // A zero endpoint must carry its canonical sign.  Every producer runs
      // canonicalize_zeros, so a zero bound can only descend from a canonical
      // one.
      if (!MODE_HAS_SIGNED_ZEROS (TYPE_MODE (m_type)))
	gcc_checking_assert (!real_iszero (&m_pairs[i].min, 1)
			     && !real_iszero (&m_pairs[i].max, 1));
      else if (!HONOR_SIGNED_ZEROS (m_type))
	gcc_checking_assert (!real_iszero (&m_pairs[i].min, 0)
			     && !real_iszero (&m_pairs[i].max, 1));
    }

  // Sub-ranges are sorted and separated by at least one representable value.
  for (unsigned i = 1; i < m_num_ranges; ++i)
    gcc_checking_assert (!frange_fusible_p (TYPE_MODE (m_type),
					    m_pairs[i - 1].max,
					    m_pairs[i].min));

  // If all the properties are clear, we better not span the entire
  // domain, because that would make us varying.
  if (m_num_ranges == 1 && m_pos_nan && m_neg_nan)
    gcc_checking_assert (!frange_val_is_min (m_pairs[0].min, m_type)
			 || !frange_val_is_max (m_pairs[0].max, m_type));
}

void
frange::set_nonzero (tree type)
{
  set (type, dconstm0, dconst0, VR_ANTI_RANGE);
}

// Return TRUE if the range contains zero (+0.0 or -0.0).

bool
frange::contains_zero_p () const
{
  return contains_p (dconst0) || contains_p (dconstm0);
}

// Set range to [+0.0, +0.0] if honoring signed zeros, or [0.0, 0.0]
// otherwise.

void
frange::set_zero (tree type)
{
  if (HONOR_SIGNED_ZEROS (type))
    {
      set (type, dconstm0, dconst0);
      clear_nan ();
    }
  else
    set (type, dconst0, dconst0);
}

// Return TRUE for any zero regardless of sign.

bool
frange::zero_p () const
{
  return (m_kind == VR_RANGE
	  && m_num_ranges == 1
	  && real_iszero (&m_pairs[0].min)
	  && real_iszero (&m_pairs[0].max));
}

// Set the range to non-negative numbers, that is [+0.0, +INF].
//
// The NAN in the resulting range (if HONOR_NANS) has a varying sign
// as there are no guarantees in IEEE 754 wrt to the sign of a NAN,
// except for copy, abs, and copysign.  It is the responsibility of
// the caller to set the NAN's sign if desired.

void
frange::set_nonnegative (tree type)
{
  set (type, dconst0, frange_val_max (type));
}

tree
frange::lbound () const
{
  return build_real (type (), lower_bound ());
}

tree
frange::ubound () const
{
  return build_real (type (), upper_bound ());
}

/* Widen a single bound of a sub-range by 1ulp (or 0.5ulp) in the direction of
   DIR.  */

static REAL_VALUE_TYPE
float_widen_bound (tree type, const REAL_VALUE_TYPE &bound,
		   const REAL_VALUE_TYPE &dir)
{
  REAL_VALUE_TYPE res = bound;
  if (!real_isfinite (&bound) && real_isneg (&bound) == real_isneg (&dir))
    return res;
  frange_nextafter (TYPE_MODE (type), res, dir);
  if (real_isinf (&res))
    {
      /* For +-DBL_MAX, instead of +-Inf use nexttoward (+-DBL_MAX, +-LDBL_MAX)
	 in a hypothetical wider type with the same mantissa precision but
	 larger exponent range; it is outside of range of double values, but
	 makes it clear it is just one ulp larger rather than infinite amount
	 larger.  */
      res = real_isneg (&dir) ? dconstm1 : dconst1;
      SET_REAL_EXP (&res, FLOAT_MODE_FORMAT (TYPE_MODE (type))->emax + 1);
    }
  if (!flag_rounding_math
      && !MODE_COMPOSITE_P (TYPE_MODE (type))
      && real_isfinite (&bound))
    {
      /* If not -frounding-math nor IBM double double, actually widen
	 just by 0.5ulp rather than 1ulp.  */
      REAL_VALUE_TYPE tem;
      real_arithmetic (&tem, PLUS_EXPR, &bound, &res);
      real_arithmetic (&res, RDIV_EXPR, &tem, &dconst2);
    }
  return res;
}

/* Extend the *this range by 1ulp in each direction.  For op1_range
   or op2_range of binary operations just computing the inverse
   operation on ranges isn't sufficient.  Consider e.g.
   [1., 1.] = op1 + [1., 1.].  op1's range is not [0., 0.], but
   [-0x1.0p-54, 0x1.0p-53] (when not -frounding-math), any value for
   which adding 1. to it results in 1. after rounding to nearest.
   So, for op1_range/op2_range extend the lhs range by 1ulp (or 0.5ulp)
   in each direction.  See PR109008 for more details.  */

void
frange::widen (tree type)
{
  if (known_isnan ())
    return;
  /* Temporarily disable -ffinite-math-only, so that frange::set doesn't
     reduce the range back to real_min_representable (type) as lower bound
     or real_max_representable (type) as upper bound.  */
  bool save_flag_finite_math_only = flag_finite_math_only;
  flag_finite_math_only = false;
  unsigned j = 0;
  for (unsigned i = 0; i < num_pairs (); ++i)
    {
      REAL_VALUE_TYPE lb = float_widen_bound (type, lower_bound (i),
					      dconstninf);
      REAL_VALUE_TYPE ub = float_widen_bound (type, upper_bound (i),
					      dconstinf);
      /* The result of float_widen_bound is often not representable in
	 type (could be smaller by 1ulp from representable finite minimum,
	 0.5ulp from some representable finite value or 1ulp larger than
	 representable finite maximum).  On such values calling e.g.
	 frange_nextafter doesn't work properly, so avoid merging the
	 pairs with union_ because that calls frange_fusible_p etc.
	 This range is often just something that should have the
	 real values passed to frange_arithmetic etc. and have the result
	 of that converted to something actually representable in the
	 type.  See PR126641 and PR109008.  As lhs should have been
	 canonicalized before, the slightly adjusted range should have
	 similar properties, just merge pairs where max would be >= than
	 min of the next pair.  */
      if (j && !real_less (&m_pairs[j - 1].max, &lb))
	m_pairs[j - 1].max = ub;
      else
	{
	  m_pairs[j].min = lb;
	  m_pairs[j].max = ub;
	  ++j;
	}
    }
  m_num_ranges = j;
  flag_finite_math_only = save_flag_finite_math_only;
}

// Here we copy between any two irange's.

irange &
irange::operator= (const irange &src)
{
  int needed = src.num_pairs ();
  maybe_resize (needed);

  unsigned x;
  unsigned lim = src.m_num_ranges;
  if (lim > m_max_ranges)
    lim = m_max_ranges;

  for (x = 0; x < lim * 2; ++x)
    m_base[x] = src.m_base[x];

  // If the range didn't fit, the last range should cover the rest.
  if (lim != src.m_num_ranges)
    m_base[x - 1] = src.m_base[src.m_num_ranges * 2 - 1];

  m_num_ranges = lim;
  m_type = src.m_type;
  m_kind = src.m_kind;
  m_bitmask = src.m_bitmask;
  if (m_max_ranges == 1)
    normalize_kind ();
  if (flag_checking)
    verify_range ();
  return *this;
}

static value_range_kind
get_legacy_range (const irange &r, tree &min, tree &max)
{
  if (r.undefined_p ())
    {
      min = NULL_TREE;
      max = NULL_TREE;
      return VR_UNDEFINED;
    }

  tree type = r.type ();
  if (r.varying_p ())
    {
      min = wide_int_to_tree (type, r.lower_bound ());
      max = wide_int_to_tree (type, r.upper_bound ());
      return VR_VARYING;
    }

  unsigned int precision = TYPE_PRECISION (type);
  signop sign = TYPE_SIGN (type);
  if (r.num_pairs () > 1
      && precision > 1
      && r.lower_bound () == wi::min_value (precision, sign)
      && r.upper_bound () == wi::max_value (precision, sign))
    {
      int_range<3> inv (r);
      inv.invert ();
      min = wide_int_to_tree (type, inv.lower_bound (0));
      max = wide_int_to_tree (type, inv.upper_bound (0));
      return VR_ANTI_RANGE;
    }

  min = wide_int_to_tree (type, r.lower_bound ());
  max = wide_int_to_tree (type, r.upper_bound ());
  return VR_RANGE;
}

static value_range_kind
get_legacy_range (const prange &r, tree &min, tree &max)
{
  if (r.undefined_p ())
    {
      min = NULL_TREE;
      max = NULL_TREE;
      return VR_UNDEFINED;
    }

  tree type = r.type ();
  if (r.varying_p ())
    {
      min = r.lbound ();
      max = r.ubound ();
      return VR_VARYING;
    }
  if (r.zero_p ())
    {
      min = max = r.lbound ();
      return VR_RANGE;
    }
  prange nonzero (type);
  nonzero.set_nonzero (type);
  if (r.lower_bound () == nonzero.lower_bound ()
      && r.upper_bound () == nonzero.upper_bound ())
    {
      min = max = build_zero_cst (type);
      return VR_ANTI_RANGE;
    }
  min = r.lbound ();
  max = r.ubound ();
  return VR_RANGE;
}

// Given a range in V, return an old-style legacy range consisting of
// a value_range_kind with a MIN/MAX.  This is to maintain
// compatibility with passes that still depend on VR_ANTI_RANGE, and
// only works for integers and pointers.

value_range_kind
get_legacy_range (const vrange &v, tree &min, tree &max)
{
  if (is_a <irange> (v))
    return get_legacy_range (as_a <irange> (v), min, max);

  return get_legacy_range (as_a <prange> (v), min, max);
}

/* Set value range to the canonical form of {VRTYPE, MIN, MAX, EQUIV}.
   This means adjusting VRTYPE, MIN and MAX representing the case of a
   wrapping range with MAX < MIN covering [MIN, type_max] U [type_min, MAX]
   as anti-rage ~[MAX+1, MIN-1].  Likewise for wrapping anti-ranges.
   In corner cases where MAX+1 or MIN-1 wraps this will fall back
   to varying.
   This routine exists to ease canonicalization in the case where we
   extract ranges from var + CST op limit.  */

void
irange::set (tree type, const wide_int &min, const wide_int &max,
	     value_range_kind kind)
{
  unsigned prec = TYPE_PRECISION (type);
  signop sign = TYPE_SIGN (type);
  wide_int min_value = wi::min_value (prec, sign);
  wide_int max_value = wi::max_value (prec, sign);

  m_type = type;
  m_bitmask.set_unknown (prec);

  if (kind == VR_RANGE)
    {
      m_base[0] = min;
      m_base[1] = max;
      m_num_ranges = 1;
      if (min == min_value && max == max_value)
	m_kind = VR_VARYING;
      else
	m_kind = VR_RANGE;
    }
  else
    {
      gcc_checking_assert (kind == VR_ANTI_RANGE);
      gcc_checking_assert (m_max_ranges > 1);

      m_kind = VR_UNDEFINED;
      m_num_ranges = 0;
      wi::overflow_type ovf;
      wide_int lim;
      if (sign == SIGNED)
	lim = wi::add (min, -1, sign, &ovf);
      else
	lim = wi::sub (min, 1, sign, &ovf);

      if (!ovf)
	{
	  m_kind = VR_RANGE;
	  m_base[0] = min_value;
	  m_base[1] = lim;
	  ++m_num_ranges;
	}
      if (sign == SIGNED)
	lim = wi::sub (max, -1, sign, &ovf);
      else
	lim = wi::add (max, 1, sign, &ovf);
      if (!ovf)
	{
	  m_kind = VR_RANGE;
	  m_base[m_num_ranges * 2] = lim;
	  m_base[m_num_ranges * 2 + 1] = max_value;
	  ++m_num_ranges;
	}
    }

  if (flag_checking)
    verify_range ();
}

void
irange::set (tree min, tree max, value_range_kind kind)
{
  if (POLY_INT_CST_P (min) || POLY_INT_CST_P (max))
    {
      set_varying (TREE_TYPE (min));
      return;
    }

  gcc_checking_assert (TREE_CODE (min) == INTEGER_CST);
  gcc_checking_assert (TREE_CODE (max) == INTEGER_CST);

  return set (TREE_TYPE (min), wi::to_wide (min), wi::to_wide (max), kind);
}

// Check the validity of the range.

void
irange::verify_range () const
{
  gcc_checking_assert (m_discriminator == VR_IRANGE);
  if (m_kind == VR_UNDEFINED)
    {
      gcc_checking_assert (m_num_ranges == 0);
      return;
    }
  gcc_checking_assert (supports_p (type ()));
  gcc_checking_assert (m_num_ranges <= m_max_ranges);

  // Legacy allowed these to represent VARYING for unknown types.
  // Leave this in for now, until all users are converted.  Eventually
  // we should abort in set_varying.
  if (m_kind == VR_VARYING && m_type == error_mark_node)
    return;

  unsigned prec = TYPE_PRECISION (m_type);
  if (m_kind == VR_VARYING)
    {
      gcc_checking_assert (m_bitmask.unknown_p ());
      gcc_checking_assert (m_num_ranges == 1);
      gcc_checking_assert (varying_compatible_p ());
      gcc_checking_assert (lower_bound ().get_precision () == prec);
      gcc_checking_assert (upper_bound ().get_precision () == prec);
      return;
    }
  gcc_checking_assert (m_num_ranges != 0);
  gcc_checking_assert (!varying_compatible_p ());
  for (unsigned i = 0; i < m_num_ranges; ++i)
    {
      wide_int lb = lower_bound (i);
      wide_int ub = upper_bound (i);
      gcc_checking_assert (lb.get_precision () == prec);
      gcc_checking_assert (ub.get_precision () == prec);
      int c = wi::cmp (lb, ub, TYPE_SIGN (m_type));
      gcc_checking_assert (c == 0 || c == -1);
      // Previous UB should be lower than LB
      if (i > 0)
	gcc_checking_assert (wi::lt_p (upper_bound (i - 1),
				       lb,
				       TYPE_SIGN (m_type)));
    }
  m_bitmask.verify_mask ();
}

bool
irange::operator== (const irange &other) const
{
  if (m_num_ranges != other.m_num_ranges)
    return false;

  if (m_num_ranges == 0)
    return true;

  signop sign1 = TYPE_SIGN (type ());
  signop sign2 = TYPE_SIGN (other.type ());

  for (unsigned i = 0; i < m_num_ranges; ++i)
    {
      widest_int lb = widest_int::from (lower_bound (i), sign1);
      widest_int ub = widest_int::from (upper_bound (i), sign1);
      widest_int lb_other = widest_int::from (other.lower_bound (i), sign2);
      widest_int ub_other = widest_int::from (other.upper_bound (i), sign2);
      if (lb != lb_other || ub != ub_other)
	return false;
    }

  irange_bitmask bm1 = get_bitmask ();
  irange_bitmask bm2 = other.get_bitmask ();
  widest_int tmp1 = widest_int::from (bm1.mask (), sign1);
  widest_int tmp2 = widest_int::from (bm2.mask (), sign2);
  if (tmp1 != tmp2)
    return false;
  if (bm1.unknown_p ())
    return true;
  tmp1 = widest_int::from (bm1.value (), sign1);
  tmp2 = widest_int::from (bm2.value (), sign2);
  return tmp1 == tmp2;
}

/* If range is a singleton, place it in RESULT and return TRUE.  */

bool
irange::singleton_p (tree *result) const
{
  if (num_pairs () == 1 && lower_bound () == upper_bound ())
    {
      if (result)
	*result = wide_int_to_tree (type (), lower_bound ());
      return true;
    }
  return false;
}

bool
irange::singleton_p (wide_int &w) const
{
  if (num_pairs () == 1 && lower_bound () == upper_bound ())
    {
      w = lower_bound ();
      return true;
    }
  return false;
}

/* Return 1 if CST is inside value range.
	  0 if CST is not inside value range.

   Benchmark compile/20001226-1.c compilation time after changing this
   function.  */

bool
irange::contains_p (const wide_int &cst) const
{
  if (undefined_p ())
    return false;

  // Check if the known bits in bitmask exclude CST.
  if (!m_bitmask.member_p (cst))
    return false;

  signop sign = TYPE_SIGN (type ());
  for (unsigned r = 0; r < m_num_ranges; ++r)
    {
      if (wi::lt_p (cst, lower_bound (r), sign))
	return false;
      if (wi::le_p (cst, upper_bound (r), sign))
	return true;
    }

  return false;
}

// Perform an efficient union with R when both ranges have only a single pair.
// Excluded are VARYING and UNDEFINED ranges.

bool
irange::irange_single_pair_union (const irange &r)
{
  gcc_checking_assert (!undefined_p () && !varying_p ());
  gcc_checking_assert (!r.undefined_p () && !varying_p ());

  signop sign = TYPE_SIGN (m_type);
  // Check if current lower bound is also the new lower bound.
  if (wi::le_p (m_base[0], r.m_base[0], sign))
    {
      // If current upper bound is new upper bound, we're done.
      if (wi::le_p (r.m_base[1], m_base[1], sign))
	return union_bitmask (r);
      // Otherwise R has the new upper bound.
      // Check for overlap/touching ranges, or single target range.
      if (m_max_ranges == 1
	  || (widest_int::from (m_base[1], sign) + 1
	      >= widest_int::from (r.m_base[0], TYPE_SIGN (r.m_type))))
	m_base[1] = r.m_base[1];
      else
	{
	  // This is a dual range result.
	  m_base[2] = r.m_base[0];
	  m_base[3] = r.m_base[1];
	  m_num_ranges = 2;
	}
      // The range has been altered, so normalize it even if nothing
      // changed in the mask.
      if (!union_bitmask (r))
	normalize_kind ();
      if (flag_checking)
	verify_range ();
      return true;
    }

  // Set the new lower bound to R's lower bound.
  wide_int lb = m_base[0];
  m_base[0] = r.m_base[0];

  // If R fully contains THIS range, just set the upper bound.
  if (wi::ge_p (r.m_base[1], m_base[1], sign))
    m_base[1] = r.m_base[1];
  // Check for overlapping ranges, or target limited to a single range.
  else if (m_max_ranges == 1
	   || (widest_int::from (r.m_base[1], TYPE_SIGN (r.m_type)) + 1
	       >= widest_int::from (lb, sign)))
    ;
  else
    {
      // Left with 2 pairs.
      m_num_ranges = 2;
      m_base[2] = lb;
      m_base[3] = m_base[1];
      m_base[1] = r.m_base[1];
    }
  // The range has been altered, so normalize it even if nothing
  // changed in the mask.
  if (!union_bitmask (r))
    normalize_kind ();
  if (flag_checking)
    verify_range ();
  return true;
}

// Append R to this range, knowing that R occurs after all of these subranges.
// Return TRUE as something must have changed.

bool
irange::union_append (const irange &r)
{
  // Check if the first range in R is an immediate successor to the last
  // range, thus requiring a merge.
  signop sign = TYPE_SIGN (m_type);
  wide_int lb = r.lower_bound ();
  wide_int ub = upper_bound ();
  unsigned start = 0;
  if (widest_int::from (ub, sign) + 1
      == widest_int::from (lb, sign))
    {
      m_base[m_num_ranges * 2 - 1] = r.m_base[1];
      start = 1;
    }
  maybe_resize (m_num_ranges + r.m_num_ranges - start);
  for ( ; start < r.m_num_ranges; start++)
    {
      // Merge the last ranges if it exceeds the maximum size.
      if (m_num_ranges + 1 > m_max_ranges)
	{
	  m_base[m_max_ranges * 2 - 1] = r.m_base[r.m_num_ranges * 2 - 1];
	  break;
	}
      m_base[m_num_ranges * 2] = r.m_base[start * 2];
      m_base[m_num_ranges * 2 + 1] = r.m_base[start * 2 + 1];
      m_num_ranges++;
    }

  if (!union_bitmask (r))
    normalize_kind ();
  if (flag_checking)
    verify_range ();
  return true;
}

// Return TRUE if anything changes.

bool
irange::union_ (const vrange &v)
{
  const irange &r = as_a <irange> (v);

  if (r.undefined_p ())
    return false;

  if (undefined_p ())
    {
      operator= (r);
      if (flag_checking)
	verify_range ();
      return true;
    }

  if (varying_p ())
    return false;

  if (r.varying_p ())
    {
      set_varying (type ());
      return true;
    }

  // Special case one range union one range.
  if (m_num_ranges == 1 && r.m_num_ranges == 1)
    return irange_single_pair_union (r);

  signop sign = TYPE_SIGN (m_type);
  // Check for an append to the end.
  if (m_kind == VR_RANGE && wi::gt_p (r.lower_bound (), upper_bound (), sign))
    return union_append (r);

  // If this ranges fully contains R, then we need do nothing.
  if (irange_contains_p (r))
    return union_bitmask (r);

  // Do not worry about merging and such by reserving twice as many
  // pairs as needed, and then simply sort the 2 ranges into this
  // intermediate form.
  //
  // The intermediate result will have the property that the beginning
  // of each range is <= the beginning of the next range.  There may
  // be overlapping ranges at this point.  I.e. this would be valid
  // [-20, 10], [-10, 0], [0, 20], [40, 90] as it satisfies this
  // constraint : -20 < -10 < 0 < 40.  When the range is rebuilt into r,
  // the merge is performed.
  //
  // [Xi,Yi]..[Xn,Yn]  U  [Xj,Yj]..[Xm,Ym]   -->  [Xk,Yk]..[Xp,Yp]
  auto_vec<wide_int, 20> res (m_num_ranges * 2 + r.m_num_ranges * 2);
  unsigned i = 0, j = 0, k = 0;

  while (i < m_num_ranges * 2 && j < r.m_num_ranges * 2)
    {
      // lower of Xi and Xj is the lowest point.
      if (widest_int::from (m_base[i], sign)
	  <= widest_int::from (r.m_base[j], sign))
	{
	  res.quick_push (m_base[i]);
	  res.quick_push (m_base[i + 1]);
	  k += 2;
	  i += 2;
	}
      else
	{
	  res.quick_push (r.m_base[j]);
	  res.quick_push (r.m_base[j + 1]);
	  k += 2;
	  j += 2;
	}
    }
  for ( ; i < m_num_ranges * 2; i += 2)
    {
      res.quick_push (m_base[i]);
      res.quick_push (m_base[i + 1]);
      k += 2;
    }
  for ( ; j < r.m_num_ranges * 2; j += 2)
    {
      res.quick_push (r.m_base[j]);
      res.quick_push (r.m_base[j + 1]);
      k += 2;
    }

  // Now normalize the vector removing any overlaps.
  i = 2;
  for (j = 2; j < k ; j += 2)
    {
      // Current upper+1 is >= lower bound next pair, then we merge ranges.
      if (widest_int::from (res[i - 1], sign) + 1
	  >= widest_int::from (res[j], sign))
	{
	  // New upper bounds is greater of current or the next one.
	  if (widest_int::from (res[j + 1], sign)
	      > widest_int::from (res[i - 1], sign))
	    res[i - 1] = res[j + 1];
	}
      else
	{
	  // This is a new distinct range, but no point in copying it
	  // if it is already in the right place.
	  if (i != j)
	    {
	      res[i++] = res[j];
	      res[i++] = res[j + 1];
	    }
	  else
	    i += 2;
	}
    }

  // At this point, the vector should have i ranges, none overlapping.
  // Now it simply needs to be copied, and if there are too many
  // ranges, merge some.  We wont do any analysis as to what the
  // "best" merges are, simply combine the final ranges into one.
  maybe_resize (i / 2);
  if (i > m_max_ranges * 2)
    {
      res[m_max_ranges * 2 - 1] = res[i - 1];
      i = m_max_ranges * 2;
    }

  for (j = 0; j < i ; j++)
    m_base[j] = res [j];
  m_num_ranges = i / 2;

  m_kind = VR_RANGE;
  // The range has been altered, so normalize it even if nothing
  // changed in the mask.
  if (!union_bitmask (r))
    normalize_kind ();
  if (flag_checking)
    verify_range ();
  return true;
}

// Return TRUE if THIS fully contains R.  No undefined or varying cases.

bool
irange::irange_contains_p (const irange &r) const
{
  gcc_checking_assert (!undefined_p () && !varying_p ());
  gcc_checking_assert (!r.undefined_p () && !varying_p ());

  // Check singletons directly which will include any bitmasks.
  wide_int rl;
  if (r.singleton_p (rl))
    return contains_p (rl);

  // In order for THIS to fully contain R, all of the pairs within R must
  // be fully contained by the pairs in this object.
  signop sign = TYPE_SIGN (m_type);
  unsigned ri = 0;
  unsigned i = 0;
  rl = r.m_base[0];
  wide_int ru = r.m_base[1];
  wide_int l = m_base[0];
  wide_int u = m_base[1];
  while (1)
    {
      // If r is contained within this range, move to the next R
      if (wi::ge_p (rl, l, sign)
	  && wi::le_p (ru, u, sign))
	{
	  // This pair is OK, Either done, or bump to the next.
	  if (++ri >= r.num_pairs ())
	    return true;
	  rl = r.m_base[ri * 2];
	  ru = r.m_base[ri * 2 + 1];
	  continue;
	}
      // Otherwise, check if this's pair occurs before R's.
      if (wi::lt_p (u, rl, sign))
	{
	  // There's still at least one pair of R left.
	  if (++i >= num_pairs ())
	    return false;
	  l = m_base[i * 2];
	  u = m_base[i * 2 + 1];
	  continue;
	}
      return false;
    }
  return false;
}


// Return TRUE if anything changes.

bool
irange::intersect (const vrange &v)
{
  const irange &r = as_a <irange> (v);
  gcc_checking_assert (undefined_p () || r.undefined_p ()
		       || range_compatible_p (type (), r.type ()));

  if (undefined_p ())
    return false;
  if (r.undefined_p ())
    {
      set_undefined ();
      return true;
    }
  if (r.varying_p ())
    return false;
  if (varying_p ())
    {
      operator= (r);
      return true;
    }

  if (r.num_pairs () == 1)
    {
      bool res = intersect (r.lower_bound (), r.upper_bound ());
      if (undefined_p ())
	return true;

      res |= intersect_bitmask (r);
      if (res)
	normalize_kind ();
      return res;
    }

  // If either range is a singleton and the other range does not contain
  // it, the result is undefined.
  wide_int val;
  if ((singleton_p (val) && !r.contains_p (val))
      || (r.singleton_p (val) && !contains_p (val)))
    {
      set_undefined ();
      return true;
    }

  // If R fully contains this, then intersection will change nothing.
  if (r.irange_contains_p (*this))
    return intersect_bitmask (r);

  // ?? We could probably come up with something smarter than the
  // worst case scenario here.
  int needed = num_pairs () + r.num_pairs ();
  maybe_resize (needed);

  signop sign = TYPE_SIGN (m_type);
  unsigned bld_pair = 0;
  unsigned bld_lim = m_max_ranges;
  int_range_max r2 (*this);
  unsigned r2_lim = r2.num_pairs ();
  unsigned i2 = 0;
  bool need_snapping = !m_bitmask.unknown_p ();
  for (unsigned i = 0; i < r.num_pairs (); )
    {
      // If r1's upper is < r2's lower, we can skip r1's pair.
      wide_int ru = r.m_base[i * 2 + 1];
      wide_int r2l = r2.m_base[i2 * 2];
      if (wi::lt_p (ru, r2l, sign))
	{
	  i++;
	  continue;
	}
      // Likewise, skip r2's pair if its excluded.
      wide_int r2u = r2.m_base[i2 * 2 + 1];
      wide_int rl = r.m_base[i * 2];
      if (wi::lt_p (r2u, rl, sign))
	{
	  i2++;
	  if (i2 < r2_lim)
	    continue;
	  // No more r2, break.
	  break;
	}

      // Must be some overlap.  Find the highest of the lower bounds,
      // and set it, unless the build limits lower bounds is already
      // set.
      if (bld_pair < bld_lim)
	{
	  if (wi::ge_p (rl, r2l, sign))
	    m_base[bld_pair * 2] = rl;
	  else
	    m_base[bld_pair * 2] = r2l;
	}
      else
	// Decrease the index to use the existing lower bound, and
	// set a new upper for this pair.
	bld_pair--;

      // Changes to false if the last value in i2's range is consumed.
      bool more = true;
      // ...and choose the lower of the upper bounds.
      if (wi::le_p (ru, r2u, sign))
	{
	  m_base[bld_pair * 2 + 1] = ru;
	  // Move past the r1 pair and keep trying.
	  i++;
	}
      else
	{
	  m_base[bld_pair * 2 + 1] = r2u;
	  i2++;
	  // No more r2, break the loop when done.
	  if (i2 >= r2_lim)
	    more = false;
	}
      // Now snap these ranges to the bitmask, if there is one.
      if (need_snapping)
	{
	  bool ovf;
	  wide_int lb, ub;
	  if (snap (m_base[bld_pair * 2], m_base[bld_pair * 2 + 1],
		    lb, ub, ovf))
	    {
	      // If the new subrange does not fit the mask, skip it.
	      if (ovf)
		{
		  if (!more)
		    break;
		  continue;
		}
	      // Otherwise adjust the pair.
	      m_base[bld_pair * 2] = lb;
	      m_base[bld_pair * 2 + 1] = ub;
	    }
	}
      // Current pair now satisfies any mask, ready for another pair.
      bld_pair++;
      if (!more)
	break;
    }

  // At the exit of this loop, it is one of 2 things:
  // ran out of r1, or r2, but either means we are done.
  m_num_ranges = bld_pair;
  if (m_num_ranges == 0)
    {
      set_undefined ();
      return true;
    }

  m_kind = VR_RANGE;
  // The range has been altered, so normalize it even if nothing
  // changed in the mask.
  if (!intersect_bitmask (r))
    normalize_kind ();
  if (flag_checking)
    verify_range ();
  return true;
}


// Multirange intersect for a specified wide_int [lb, ub] range.
// Return TRUE if intersect changed anything.
//
// NOTE: It is the caller's responsibility to intersect the mask.

bool
irange::intersect (const wide_int& lb, const wide_int& ub)
{
  // Undefined remains undefined.
  if (undefined_p ())
    return false;

  tree range_type = type();
  signop sign = TYPE_SIGN (range_type);

  gcc_checking_assert (TYPE_PRECISION (range_type) == wi::get_precision (lb));
  gcc_checking_assert (TYPE_PRECISION (range_type) == wi::get_precision (ub));

  // If this range is fully contained, then intersection will do nothing.
  if (wi::ge_p (lower_bound (), lb, sign)
      && wi::le_p (upper_bound (), ub, sign))
    return false;

  unsigned bld_index = 0;
  unsigned pair_lim = num_pairs ();
  for (unsigned i = 0; i < pair_lim; i++)
    {
      wide_int pairl = m_base[i * 2];
      wide_int pairu = m_base[i * 2 + 1];
      // Once UB is less than a pairs lower bound, we're done.
      if (wi::lt_p (ub, pairl, sign))
	break;
      // if LB is greater than this pairs upper, this pair is excluded.
      if (wi::lt_p (pairu, lb, sign))
	continue;

      // Must be some overlap.  Find the highest of the lower bounds,
      // and set it
      if (wi::gt_p (lb, pairl, sign))
	m_base[bld_index * 2] = lb;
      else
	m_base[bld_index * 2] = pairl;

      // ...and choose the lower of the upper bounds and if the base pair
      // has the lower upper bound, need to check next pair too.
      if (wi::lt_p (ub, pairu, sign))
	{
	  m_base[bld_index++ * 2 + 1] = ub;
	  break;
	}
      else
	m_base[bld_index++ * 2 + 1] = pairu;
    }

  m_num_ranges = bld_index;
  if (m_num_ranges == 0)
    {
      set_undefined ();
      return true;
    }

  m_kind = VR_RANGE;
  // The caller must normalize and verify the range, as the bitmask
  // still needs to be handled.
  return true;
}


// Signed 1-bits are strange.  You can't subtract 1, because you can't
// represent the number 1.  This works around that for the invert routine.

static wide_int inline
subtract_one (const wide_int &x, tree type, wi::overflow_type &overflow)
{
  if (TYPE_SIGN (type) == SIGNED)
    return wi::add (x, -1, SIGNED, &overflow);
  else
    return wi::sub (x, 1, UNSIGNED, &overflow);
}

// The analogous function for adding 1.

static wide_int inline
add_one (const wide_int &x, tree type, wi::overflow_type &overflow)
{
  if (TYPE_SIGN (type) == SIGNED)
    return wi::sub (x, -1, SIGNED, &overflow);
  else
    return wi::add (x, 1, UNSIGNED, &overflow);
}

// Return the inverse of a range.  Return false if thre is no invert
// calculatable.

bool
irange::invert ()
{
  // UNDEFINED cannot be converted to varying because there is no type
  // assocaited.  Callers need to handle these cases.
  // Its also ambiguous.. VARYING inverted could also arguably be VARYING
  // in some cases. Likewise with UNDEFINED.
  if (undefined_p () || varying_p ())
    return false;

  // We always need one more set of bounds to represent an inverse, so
  // if we're at the limit, we can't properly represent things.
  //
  // For instance, to represent the inverse of a 2 sub-range set
  // [5, 10][20, 30], we would need a 3 sub-range set
  // [-MIN, 4][11, 19][31, MAX].
  //
  // In this case, return false.
  //
  // However, if any of the extremes of the range are -MIN/+MAX, we
  // know we will not need an extra bound.  For example:
  //
  // 	INVERT([-MIN,20][30,40]) => [21,29][41,+MAX]
  // 	INVERT([-MIN,20][30,MAX]) => [21,29]
  tree ttype = type ();
  unsigned prec = TYPE_PRECISION (ttype);
  signop sign = TYPE_SIGN (ttype);
  wide_int type_min = wi::min_value (prec, sign);
  wide_int type_max = wi::max_value (prec, sign);
  m_bitmask.set_unknown (prec);

  // At this point, we need one extra sub-range to represent the
  // inverse.
  maybe_resize (m_num_ranges + 1);

  // The algorithm is as follows.  To calculate INVERT ([a,b][c,d]), we
  // generate [-MIN, a-1][b+1, c-1][d+1, MAX].
  //
  // If there is an over/underflow in the calculation for any
  // sub-range, we eliminate that subrange.  This allows us to easily
  // calculate INVERT([-MIN, 5]) with: [-MIN, -MIN-1][6, MAX].  And since
  // we eliminate the underflow, only [6, MAX] remains.
  unsigned i = 0;
  wi::overflow_type ovf;
  // Construct leftmost range.
  int_range_max orig_range (*this);
  unsigned nitems = 0;
  wide_int tmp;
  // If this is going to underflow on the MINUS 1, don't even bother
  // checking.  This also handles subtracting one from an unsigned 0,
  // which doesn't set the underflow bit.
  if (type_min != orig_range.lower_bound ())
    {
      m_base[nitems++] = type_min;
      tmp = subtract_one (orig_range.lower_bound (), ttype, ovf);
      m_base[nitems++] = tmp;
      if (ovf)
	nitems = 0;
    }
  i++;
  // Construct middle ranges if applicable.
  if (orig_range.num_pairs () > 1)
    {
      unsigned j = i;
      for (; j < (orig_range.num_pairs () * 2) - 1; j += 2)
	{
	  // The middle ranges cannot have MAX/MIN, so there's no need
	  // to check for unsigned overflow on the +1 and -1 here.
	  tmp = wi::add (orig_range.m_base[j], 1, sign, &ovf);
	  m_base[nitems++] = tmp;
	  tmp = subtract_one (orig_range.m_base[j + 1], ttype, ovf);
	  m_base[nitems++] = tmp;
	  if (ovf)
	    nitems -= 2;
	}
      i = j;
    }
  // Construct rightmost range.
  //
  // However, if this will overflow on the PLUS 1, don't even bother.
  // This also handles adding one to an unsigned MAX, which doesn't
  // set the overflow bit.
  if (type_max != orig_range.m_base[i])
    {
      tmp = add_one (orig_range.m_base[i], ttype, ovf);
      if (!ovf)
	{
	  // Check to see if this inversion is going to work.
	  if (nitems / 2 >= m_max_ranges)
	    {
	      // No room for the extra field, so revert to the original value
	      // and return false.
	      *this = orig_range;
	      return false;
	    }
	  m_base[nitems++] = tmp;
	  m_base[nitems++] = type_max;
	}
    }
  m_num_ranges = nitems / 2;

  // We disallow undefined or varying coming in, so the result can
  // only be a VR_RANGE.
  gcc_checking_assert (m_kind == VR_RANGE);

  if (flag_checking)
    verify_range ();
  return true;
}

// This routine will take the bounds [LB, UB], and apply the bitmask to those
// values such that both bounds satisfy the bitmask.  TRUE is returned
// if either bound changes, and they are returned as [NEW_LB, NEW_UB].
// If there is an overflow, or if (NEW_UB < NEW_LB), then the entire bound is
// to be removed as none of the values are valid.   This is indicated by
// teturning TRUE in OVF.   False indicates the bounds are fine.
//   ie,   [4, 14] MASK 0xFFFE  VALUE 0x1
// means all values must be odd, the new bounds returned will be [5, 13] with
// OVF set to FALSE.
//   ie,   [4, 4] MASK 0xFFFE  VALUE 0x1
// would return TRUE and OVF == TRUE.  The entire subrange should be removed.

bool
irange::snap (const wide_int &lb, const wide_int &ub,
	      wide_int &new_lb, wide_int &new_ub, bool &ovf)
{
  ovf = false;
  int z = wi::ctz (m_bitmask.mask ());
  if (z == 0)
    return false;

  // Shortcircuit check for values that are already good.
  if ((((lb ^ m_bitmask.value ()) | (ub ^ m_bitmask.value ()))
       & ~m_bitmask.mask ()) == 0)
    return false;

  const wide_int step = (wi::one (TYPE_PRECISION (type ())) << z);
  const wide_int match_mask = step - 1;
  const wide_int value = m_bitmask.value () & match_mask;

  wide_int rem_lb = lb & match_mask;
  wide_int offset = (value - rem_lb) & match_mask;
  new_lb = lb + offset;
  // Check for overflows at +INF
  if (wi::lt_p (new_lb, lb, TYPE_SIGN (type ())))
    {
      ovf = true;
      return true;
    }

  wide_int rem_ub = ub & match_mask;
  wide_int offset_ub = (rem_ub - value) & match_mask;
  new_ub = ub - offset_ub;
  // Check for underflows at -INF
  if (wi::gt_p (new_ub, ub, TYPE_SIGN (type ())))
    {
      ovf = true;
      return true;
    }

  // If inverted range is invalid, set overflow to TRUE.
  if (wi::lt_p (new_ub, new_lb, TYPE_SIGN (type ())))
    {
      ovf = true;
      return true;
    }
  return (new_lb != lb) || (new_ub != ub);
}

// This method loops through the subranges in THIS, and adjusts any bounds
// to satisfy the constraints of the BITMASK.  If a subrange is invalid,
// it is removed.   TRUE is returned if there were any changes.

bool
irange::snap_subranges ()
{
  bool changed = false;
  int_range_max invalid;
  unsigned x;
  wide_int lb, ub;
  for (x = 0; x < m_num_ranges; x++)
    {
      bool ovf;
      if (snap (lower_bound (x), upper_bound (x), lb, ub, ovf))
	{
	  changed = true;
	  // Check if this subrange is to be completely removed.
	  if (ovf)
	    {
	      int_range<1> tmp (type (), lower_bound (x), upper_bound (x));
	      invalid.union_ (tmp);
	      continue;
	    }
	  if (lower_bound (x) != lb)
	    m_base[x * 2] = lb;
	  if (upper_bound (x) != ub)
	    m_base[x * 2 + 1] = ub;
	}
    }
  // Remove any subranges which are no invalid.
  if (!invalid.undefined_p ())
    {
      bool res = invalid.invert ();
      gcc_checking_assert (res);
      intersect (invalid);
    }
  return changed;
}

// If the bitmask has a range representation, intersect this range with
// the bitmasks range.  Then ensure all endpoints match the bitmask.
// Return TRUE if the range changes at all.

bool
irange::set_range_from_bitmask ()
{
  gcc_checking_assert (!undefined_p ());
  // Snap subranmges when bitmask is first set.
  snap_subranges ();
  if (undefined_p ())
    return true;

  // Calculate the set of ranges valid for the bitmask.
  int_range_max allow;
  if (!m_bitmask.range_from_mask (allow, m_type))
    return false;
  // And intersect that set of ranges with the current set.
  return intersect (allow);
}

void
irange::update_bitmask (const irange_bitmask &bm)
{
  gcc_checking_assert (!undefined_p ());

  // If masks are the same, there is no change.
  if (m_bitmask == bm)
    return;

  // Drop VARYINGs with known bits to a plain range.
  if (m_kind == VR_VARYING && !bm.unknown_p ())
    m_kind = VR_RANGE;

  m_bitmask = bm;
  if (!set_range_from_bitmask ())
    normalize_kind ();
  if (flag_checking)
    verify_range ();
}

// Return the bitmask of known bits that includes the bitmask inherent
// in the range.

irange_bitmask
irange::get_bitmask () const
{
  gcc_checking_assert (!undefined_p ());

  // The mask inherent in the range is calculated on-demand.  For
  // example, [0,255] does not have known bits set by default.  This
  // saves us considerable time, because setting it at creation incurs
  // a large penalty for irange::set.  At the time of writing there
  // was a 5% slowdown in VRP if we kept the mask precisely up to date
  // at all times.  Instead, we default to -1 and set it when
  // explicitly requested.  However, this function will always return
  // the correct mask.
  //
  // This also means that the mask may have a finer granularity than
  // the range and thus contradict it.  Think of the mask as an
  // enhancement to the range.  For example:
  //
  // [3, 1000] MASK 0xfffffffe VALUE 0x0
  //
  // 3 is in the range endpoints, but is excluded per the known 0 bits
  // in the mask.
  //
  // See also the note in irange_bitmask::intersect.
  irange_bitmask bm (type (), lower_bound (), upper_bound ());
  if (!m_bitmask.unknown_p ())
    {
      // If the new intersection is unknown, it means there are inconsistent
      // bits, so simply return the original bitmask.
      if (!bm.intersect (m_bitmask))
	return m_bitmask;
    }
  return bm;
}

// Set the nonzero bits in R into THIS.  Return TRUE and
// normalize the range if anything changed.

void
vrange::set_nonzero_bits (const wide_int &bits)
{
  gcc_checking_assert (!undefined_p ());
  irange_bitmask bm (wi::zero (TYPE_PRECISION (type ())), bits);
  update_bitmask (bm);
}

// Return the nonzero bits in R.

wide_int
vrange::get_nonzero_bits () const
{
  gcc_checking_assert (!undefined_p ());
  irange_bitmask bm = get_bitmask ();
  return bm.value () | bm.mask ();
}

// Intersect the bitmask in R into THIS and normalize the range.
// Return TRUE if the intersection changed anything.

bool
irange::intersect_bitmask (const irange &r)
{
  gcc_checking_assert (!undefined_p () && !r.undefined_p ());

  // If the bitmasks are the same, do nothing.
  if (m_bitmask == r.m_bitmask)
    return false;

  irange_bitmask bm = get_bitmask ();
  irange_bitmask save = bm;
  if (!bm.intersect (r.get_bitmask ()))
    {
      set_undefined ();
      return true;
    }

  // If the new mask is the same, there is no change.
  if (m_bitmask == bm)
    return false;

  m_bitmask = bm;
  if (!set_range_from_bitmask ())
    normalize_kind ();
  if (flag_checking)
    verify_range ();
  return true;
}

// Union the bitmask in R into THIS.  Return TRUE and normalize the
// range if anything changed.

bool
irange::union_bitmask (const irange &r)
{
  gcc_checking_assert (!undefined_p () && !r.undefined_p ());

  if (m_bitmask == r.m_bitmask)
    return false;

  irange_bitmask bm = get_bitmask ();
  irange_bitmask save = bm;
  bm.union_ (r.get_bitmask ());
  if (save == bm && (!bm.unknown_p () || m_bitmask.unknown_p ()))
    return false;

  m_bitmask = bm;

  // Updating m_bitmask may still yield a semantic bitmask (as
  // returned by get_bitmask) which is functionally equivalent to what
  // we originally had.  In which case, there's still no change.
  if (save == get_bitmask ())
    return false;

  // No need to call set_range_from_mask, because we'll never
  // narrow the range.  Besides, it would cause endless recursion
  // because of the union_ in set_range_from_mask.
  normalize_kind ();
  return true;
}

tree
irange::lbound () const
{
  return wide_int_to_tree (type (), lower_bound ());
}

tree
irange::ubound () const
{
  return wide_int_to_tree (type (), upper_bound ());
}

void
irange_bitmask::verify_mask () const
{
  gcc_assert (m_value.get_precision () == m_mask.get_precision ());
  gcc_checking_assert (wi::bit_and (m_mask, m_value) == 0);
}

void
dump_value_range (FILE *file, const vrange *vr)
{
  vr->dump (file);
}

DEBUG_FUNCTION void
debug (const vrange *vr)
{
  dump_value_range (stderr, vr);
  fprintf (stderr, "\n");
}

DEBUG_FUNCTION void
debug (const vrange &vr)
{
  debug (&vr);
}

/* Return true, if VAL1 and VAL2 are equal values for VRP purposes.  */

bool
vrp_operand_equal_p (const_tree val1, const_tree val2)
{
  if (val1 == val2)
    return true;
  if (!val1 || !val2 || !operand_equal_p (val1, val2, 0))
    return false;
  return true;
}

#define DEFINE_INT_RANGE_INSTANCE(N)					\
  template int_range<N>::int_range(tree_node *,				\
				   const wide_int &,			\
				   const wide_int &,			\
				   value_range_kind);			\
  template int_range<N>::int_range(tree);				\
  template int_range<N>::int_range(const irange &);		\
  template int_range<N>::int_range(const int_range &);			\
  template int_range<N>& int_range<N>::operator= (const int_range &);

DEFINE_INT_RANGE_INSTANCE(1)
DEFINE_INT_RANGE_INSTANCE(2)
DEFINE_INT_RANGE_INSTANCE(3)
DEFINE_INT_RANGE_INSTANCE(255)

#if CHECKING_P
#include "selftest.h"

#define INT(x) wi::shwi ((x), TYPE_PRECISION (integer_type_node))
#define UINT(x) wi::uhwi ((x), TYPE_PRECISION (unsigned_type_node))
#define SCHAR(x) wi::shwi ((x), TYPE_PRECISION (signed_char_type_node))

namespace selftest
{

static int_range<2>
range (tree type, int a, int b, value_range_kind kind = VR_RANGE)
{
  wide_int w1, w2;
  if (TYPE_UNSIGNED (type))
    {
      w1 = wi::uhwi (a, TYPE_PRECISION (type));
      w2 = wi::uhwi (b, TYPE_PRECISION (type));
    }
  else
    {
      w1 = wi::shwi (a, TYPE_PRECISION (type));
      w2 = wi::shwi (b, TYPE_PRECISION (type));
    }
  return int_range<2> (type, w1, w2, kind);
}

static int_range<2>
range_int (int a, int b, value_range_kind kind = VR_RANGE)
{
  return range (integer_type_node, a, b, kind);
}

static int_range<2>
range_uint (int a, int b, value_range_kind kind = VR_RANGE)
{
  return range (unsigned_type_node, a, b, kind);
}

static int_range<2>
range_uint128 (int a, int b, value_range_kind kind = VR_RANGE)
{
  tree u128_type_node = build_nonstandard_integer_type (128, 1);
  return range (u128_type_node, a, b, kind);
}

static int_range<2>
range_uchar (int a, int b, value_range_kind kind = VR_RANGE)
{
  return range (unsigned_char_type_node, a, b, kind);
}

static int_range<2>
range_char (int a, int b, value_range_kind kind = VR_RANGE)
{
  return range (signed_char_type_node, a, b, kind);
}

static int_range<3>
build_range3 (int a, int b, int c, int d, int e, int f)
{
  int_range<3> i1 = range_int (a, b);
  int_range<3> i2 = range_int (c, d);
  int_range<3> i3 = range_int (e, f);
  i1.union_ (i2);
  i1.union_ (i3);
  return i1;
}

static void
range_tests_irange3 ()
{
  int_range<3> r0, r1, r2;
  int_range<3> i1, i2, i3;

  // ([10,20] U [5,8]) U [1,3] ==> [1,3][5,8][10,20].
  r0 = range_int (10, 20);
  r1 = range_int (5, 8);
  r0.union_ (r1);
  r1 = range_int (1, 3);
  r0.union_ (r1);
  ASSERT_TRUE (r0 == build_range3 (1, 3, 5, 8, 10, 20));

  // [1,3][5,8][10,20] U [-5,0] => [-5,3][5,8][10,20].
  r1 = range_int (-5, 0);
  r0.union_ (r1);
  ASSERT_TRUE (r0 == build_range3 (-5, 3, 5, 8, 10, 20));

  // [10,20][30,40] U [50,60] ==> [10,20][30,40][50,60].
  r1 = range_int (50, 60);
  r0 = range_int (10, 20);
  r0.union_ (range_int (30, 40));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == build_range3 (10, 20, 30, 40, 50, 60));
  // [10,20][30,40][50,60] U [70, 80] ==> [10,20][30,40][50,60][70,80].
  r1 = range_int (70, 80);
  r0.union_ (r1);

  r2 = build_range3 (10, 20, 30, 40, 50, 60);
  r2.union_ (range_int (70, 80));
  ASSERT_TRUE (r0 == r2);

  // [10,20][30,40][50,60] U [6,35] => [6,40][50,60].
  r0 = build_range3 (10, 20, 30, 40, 50, 60);
  r1 = range_int (6, 35);
  r0.union_ (r1);
  r1 = range_int (6, 40);
  r1.union_ (range_int (50, 60));
  ASSERT_TRUE (r0 == r1);

  // [10,20][30,40][50,60] U [6,60] => [6,60].
  r0 = build_range3 (10, 20, 30, 40, 50, 60);
  r1 = range_int (6, 60);
  r0.union_ (r1);
  ASSERT_TRUE (r0 == range_int (6, 60));

  // [10,20][30,40][50,60] U [6,70] => [6,70].
  r0 = build_range3 (10, 20, 30, 40, 50, 60);
  r1 = range_int (6, 70);
  r0.union_ (r1);
  ASSERT_TRUE (r0 == range_int (6, 70));

  // [10,20][30,40][50,60] U [35,70] => [10,20][30,70].
  r0 = build_range3 (10, 20, 30, 40, 50, 60);
  r1 = range_int (35, 70);
  r0.union_ (r1);
  r1 = range_int (10, 20);
  r1.union_ (range_int (30, 70));
  ASSERT_TRUE (r0 == r1);

  // [10,20][30,40][50,60] U [15,35] => [10,40][50,60].
  r0 = build_range3 (10, 20, 30, 40, 50, 60);
  r1 = range_int (15, 35);
  r0.union_ (r1);
  r1 = range_int (10, 40);
  r1.union_ (range_int (50, 60));
  ASSERT_TRUE (r0 == r1);

  // [10,20][30,40][50,60] U [35,35] => [10,20][30,40][50,60].
  r0 = build_range3 (10, 20, 30, 40, 50, 60);
  r1 = range_int (35, 35);
  r0.union_ (r1);
  ASSERT_TRUE (r0 == build_range3 (10, 20, 30, 40, 50, 60));
}

static void
range_tests_int_range_max ()
{
  int_range_max big;
  unsigned int nrange;

  // Build a huge multi-range range.
  for (nrange = 0; nrange < 50; ++nrange)
    {
      int_range<1> tmp = range_int (nrange*10, nrange *10 + 5);
      big.union_ (tmp);
    }
  ASSERT_TRUE (big.num_pairs () == nrange);

  // Verify that we can copy it without loosing precision.
  int_range_max copy (big);
  ASSERT_TRUE (copy.num_pairs () == nrange);

  // Inverting it should produce one more sub-range.
  big.invert ();
  ASSERT_TRUE (big.num_pairs () == nrange + 1);

  int_range<1> tmp = range_int (5, 37);
  big.intersect (tmp);
  ASSERT_TRUE (big.num_pairs () == 4);

  // Cannot resize tmp, and the invert does not fit,
  ASSERT_FALSE (tmp.invert ());

  // Test that [10,10][20,20] does NOT contain 15.
  {
    int_range_max i1 = range_int (10, 10);
    int_range_max i2 = range_int (20, 20);
    i1.union_ (i2);
    ASSERT_FALSE (i1.contains_p (INT (15)));
  }
}

// Simulate -fstrict-enums where the domain of a type is less than the
// underlying type.

static void
range_tests_strict_enum ()
{
  // The enum can only hold [0, 3].
  tree rtype = copy_node (unsigned_type_node);
  TYPE_MIN_VALUE (rtype) = build_int_cstu (rtype, 0);
  TYPE_MAX_VALUE (rtype) = build_int_cstu (rtype, 3);

  // Test that even though vr1 covers the strict enum domain ([0, 3]),
  // it does not cover the domain of the underlying type.
  int_range<1> vr1 = range (rtype, 0, 1);
  int_range<1> vr2 = range (rtype, 2, 3);
  vr1.union_ (vr2);
  ASSERT_TRUE (vr1 == range (rtype, 0, 3));
  ASSERT_FALSE (vr1.varying_p ());

  // Test that copying to a multi-range does not change things.
  int_range<2> ir1 (vr1);
  ASSERT_TRUE (ir1 == vr1);
  ASSERT_FALSE (ir1.varying_p ());

  // The same test as above, but using TYPE_{MIN,MAX}_VALUE instead of [0,3].
  vr1 = int_range<2> (rtype,
		      wi::to_wide (TYPE_MIN_VALUE (rtype)),
		      wi::to_wide (TYPE_MAX_VALUE (rtype)));
  ir1 = vr1;
  ASSERT_TRUE (ir1 == vr1);
  ASSERT_FALSE (ir1.varying_p ());
}

// Test that range bounds are "snapped" to where they are expected to be.

static void
assert_snap_result (int lb_val, int ub_val,
		    int expected_lb, int expected_ub,
		    unsigned mask_val, unsigned value_val,
		    tree type)
{
  wide_int lb = wi::shwi (lb_val, TYPE_PRECISION (type));
  wide_int ub = wi::shwi (ub_val, TYPE_PRECISION (type));
  wide_int new_lb, new_ub;

  irange_bitmask bm (wi::uhwi (value_val, TYPE_PRECISION (type)),
		     wi::uhwi (mask_val, TYPE_PRECISION (type)));

  int_range_max r (type);
  r.set (type, lb, ub);
  r.update_bitmask (bm);

  if (TYPE_SIGN (type) == SIGNED && expected_ub < expected_lb)
    gcc_checking_assert (r.undefined_p ());
  else if (TYPE_SIGN (type) == UNSIGNED
	   && ((unsigned)expected_ub < (unsigned)expected_lb))
    gcc_checking_assert (r.undefined_p ());
  else
    {
      gcc_checking_assert (wi::eq_p (r.lower_bound (),
				     wi::shwi (expected_lb,
					       TYPE_PRECISION (type))));
      gcc_checking_assert (wi::eq_p (r.upper_bound (),
				     wi::shwi (expected_ub,
					       TYPE_PRECISION (type))));
    }
}


// Run a selection of tests that confirm, bounds are snapped as expected.
// We only test individual pairs, multiple pairs use the same snapping
// routine as single pairs.

static void
test_irange_snap_bounds ()
{
  tree u32 = unsigned_type_node;
  tree s32 = integer_type_node;
  tree s8 = build_nonstandard_integer_type (8, /*unsigned=*/ 0);
  tree s1 = build_nonstandard_integer_type (1, /*unsigned=*/ 0);
  tree u1 = build_nonstandard_integer_type (1, /*unsigned=*/ 1);

  // Basic aligned range: even-only
  assert_snap_result (5, 15, 6, 14, 0xE, 0x0, u32);
  // Singleton that doesn't match mask: undefined.
  assert_snap_result (7, 7, 1, 0, 0xFFFFFFFE, 0x0, u32);
  // 8-bit signed char, mask 0xF0 (i.e. step of 16).
  assert_snap_result (-100, 100, -96, 96, 0xF0, 0x00, s8);
  // Already aligned range: no change.
  assert_snap_result (0, 240, 0, 240, 0xF0, 0x00, u32);
  // Negative range, step 16 alignment (s32).
  assert_snap_result (-123, -17, -112, -32, 0xFFFFFFF0, 0x00, s32);
  // Negative range, step 16 alignment (trailing-zero aligned mask).
  assert_snap_result (-123, -17, -112, -32, 0xFFFFFFF0, 0x00, s32);
  // s8, 16-alignment mask, value = 0 (valid).
  assert_snap_result (-50, 10, -48, 0, 0xF0, 0x00, s8);
  // No values in range [-3,2] match alignment except 0.
  assert_snap_result (-3, 2, 0, 0, 0xF8, 0x00, s8);
  // No values in range [-3,2] match alignment — undefined.
  assert_snap_result (-3, 2, 1, 0, 0xF8, 0x04, s8);
  // Already aligned range: no change.
  assert_snap_result (0, 240, 0, 240, 0xF0, 0x00, s32);
  // 1-bit signed: only -1 allowed (0b1).
  assert_snap_result (-1, 0, -1, -1, 0x00, 0x01, s1);
  // 1-bit signed: only 0 allowed (0b0).
  assert_snap_result (-1, 0, 0, 0, 0x00, 0x00, s1);
  // 1-bit signed: no match (invalid case).
  assert_snap_result (-1, -1, 1, 0, 0x00, 0x00, s1);
  // 1-bit signed: no match (invalid case).
  assert_snap_result (0, 0, 1, 0, 0x00, 0x01, s1);
  // 1-bit unsigned: only 1 allowed.
  assert_snap_result (0, 1, 1, 1, 0x00, 0x01, u1);
  // 1-bit unsigned: only 0 allowed.
  assert_snap_result (0, 1, 0, 0, 0x00, 0x00, u1);
  // 1-bit unsigned: no match (invalid case).
  assert_snap_result (1, 1, 1, 0, 0x00, 0x00, u1);
  // 1-bit unsigned: no match (invalid case).
  assert_snap_result (0, 0, 1, 0, 0x00, 0x01, u1);
  // Unsigned: Near overflow, even alignment.
  assert_snap_result (UINT_MAX - 6, UINT_MAX, UINT_MAX - 5, UINT_MAX - 1,
		      0xFFFFFFFE, 0x00, u32);
  // Unsigned: Wraparound-like range — no valid snapped values.
  assert_snap_result (UINT_MAX - 5, UINT_MAX, 1, 0, 0xFFFFFFF0, 0x00, u32);
  // Signed: Near INT_MAX, 8-aligned.
  assert_snap_result (INT_MAX - 18, INT_MAX, INT_MAX - 15, INT_MAX - 7,
		      0xFFFFFFF8, 0x00, s32);
  // Signed: Near INT_MIN, 16-aligned.
  assert_snap_result (INT_MIN, INT_MIN + 30, INT_MIN, INT_MIN + 16,
		      0xFFFFFFF0, 0x00, s32);
  // Signed: Full domain, 4-aligned.
  assert_snap_result (-128, 127, -128, 124, 0xFC, 0x00, s8);
  // Singleton at INT_MIN that doesn’t match alignment — undefined
  assert_snap_result (INT_MIN, INT_MIN, 1, 0, 0xFFFFFFFE, 0x01, s32);
  // Range at INT_MIN that doesn’t match alignment — undefined.
  assert_snap_result (INT_MIN, INT_MIN + 10, 1, 0, 0xFFFFFFF0, 0x0F, s32);
  // Unsigned: Full domain, 256-aligned.
  assert_snap_result (0, UINT_MAX, 0, UINT_MAX & ~255, 0xFFFFFF00, 0x00, u32);
}

static void
range_tests_misc ()
{
  bool res;
  tree u128_type = build_nonstandard_integer_type (128, /*unsigned=*/1);
  int_range<2> i1, i2, i3;
  int_range<2> r0, r1, rold;

  // Test 1-bit signed integer union.
  // [-1,-1] U [0,0] = VARYING.
  tree one_bit_type = build_nonstandard_integer_type (1, 0);
  wide_int one_bit_min = irange_val_min (one_bit_type);
  wide_int one_bit_max = irange_val_max (one_bit_type);
  {
    int_range<2> min = int_range<2> (one_bit_type, one_bit_min, one_bit_min);
    int_range<2> max = int_range<2> (one_bit_type, one_bit_max, one_bit_max);
    max.union_ (min);
    ASSERT_TRUE (max.varying_p ());
  }
  // Test that we can set a range of true+false for a 1-bit signed int.
  r0 = range_true_and_false (one_bit_type);

  // Test inversion of 1-bit signed integers.
  {
    int_range<2> min = int_range<2> (one_bit_type, one_bit_min, one_bit_min);
    int_range<2> max = int_range<2> (one_bit_type, one_bit_max, one_bit_max);
    int_range<2> t;
    t = min;
    res = t.invert ();
    ASSERT_TRUE (res && t == max);
    t = max;
    res = t.invert ();
    ASSERT_TRUE (res && t == min);
  }

  // Test that NOT(255) is [0..254] in 8-bit land.
  int_range<1> not_255 = range_uchar (255, 255, VR_ANTI_RANGE);
  ASSERT_TRUE (not_255 == range_uchar (0, 254));

  // Test that NOT(0) is [1..255] in 8-bit land.
  int_range<2> not_zero;
  not_zero.set_nonzero (unsigned_char_type_node);
  ASSERT_TRUE (not_zero == range_uchar (1, 255));

  // Check that [0,127][0x..ffffff80,0x..ffffff]
  //  => ~[128, 0x..ffffff7f].
  r0 = range_uint128 (0, 127);
  wide_int high = wi::minus_one (128);
  // low = -1 - 127 => 0x..ffffff80.
  wide_int low = wi::sub (high, wi::uhwi (127, 128));
  r1 = int_range<1> (u128_type, low, high); // [0x..ffffff80, 0x..ffffffff]
  // r0 = [0,127][0x..ffffff80,0x..fffffff].
  r0.union_ (r1);
  // r1 = [128, 0x..ffffff7f].
  r1 = int_range<1> (u128_type,
		     wi::uhwi (128, 128),
		     wi::sub (wi::minus_one (128), wi::uhwi (128, 128)));
  res = r0.invert ();
  ASSERT_TRUE (res && r0 == r1);

  r0.set_varying (integer_type_node);
  wide_int minint = r0.lower_bound ();
  wide_int maxint = r0.upper_bound ();

  r0.set_varying (short_integer_type_node);

  r0.set_varying (unsigned_type_node);
  wide_int maxuint = r0.upper_bound ();

  // Check that ~[0,5] => [6,MAX] for unsigned int.
  r0 = range_uint (0, 5);
  res = r0.invert ();
  ASSERT_TRUE (res);
  ASSERT_TRUE (r0 == int_range<1> (unsigned_type_node,
				   wi::uhwi (6, TYPE_PRECISION (unsigned_type_node)),
				   maxuint));

  // Check that ~[10,MAX] => [0,9] for unsigned int.
  r0 = int_range<1> (unsigned_type_node,
		     wi::uhwi (10, TYPE_PRECISION (unsigned_type_node)),
		     maxuint);
  res = r0.invert ();
  ASSERT_TRUE (res && r0 == range_uint (0, 9));

  // Check that ~[0,5] => [6,MAX] for unsigned 128-bit numbers.
  r0 = range_uint128 (0, 5, VR_ANTI_RANGE);
  r1 = int_range<1> (u128_type, wi::uhwi (6, 128), wi::minus_one (128));
  ASSERT_TRUE (r0 == r1);

  // Check that [~5] is really [-MIN,4][6,MAX].
  r0 = range_int (5, 5, VR_ANTI_RANGE);
  r1 = int_range<1> (integer_type_node, minint, INT (4));
  r1.union_ (int_range<1> (integer_type_node, INT (6), maxint));
  ASSERT_FALSE (r1.undefined_p ());
  ASSERT_TRUE (r0 == r1);

  r1 = range_int (5, 5);
  int_range<2> r2 (r1);
  ASSERT_TRUE (r1 == r2);

  r1 = range_int (5, 10);

  r1 = range_int (5, 10);
  ASSERT_TRUE (r1.contains_p (INT (7)));

  r1 = range_char (0, 20);
  ASSERT_TRUE (r1.contains_p (SCHAR(15)));
  ASSERT_FALSE (r1.contains_p (SCHAR(300)));

  // NOT([10,20]) ==> [-MIN,9][21,MAX].
  r0 = r1 = range_int (10, 20);
  r2 = int_range<1> (integer_type_node, minint, INT(9));
  r2.union_ (int_range<1> (integer_type_node, INT(21), maxint));
  ASSERT_FALSE (r2.undefined_p ());
  res = r1.invert ();
  ASSERT_TRUE (res && r1 == r2);
  // Test that NOT(NOT(x)) == x.
  res = r2.invert ();
  ASSERT_TRUE (res && r0 == r2);

  // Test that booleans and their inverse work as expected.
  r0.set_zero (boolean_type_node);
  ASSERT_TRUE (r0 == range_false ());
  res = r0.invert ();
  ASSERT_TRUE (res && r0 == range_true ());

  // Make sure NULL and non-NULL of pointer types work, and that
  // inverses of them are consistent.
  tree voidp = build_pointer_type (void_type_node);
  prange p0;
  p0.set_zero (voidp);
  prange p1 = p0;
  res = p0.invert ();
  ASSERT_TRUE (res);
  res = p0.invert ();
  ASSERT_TRUE (res && p0 == p1);

  // The intersection of:
  //    [0, +INF] MASK 0xff..00 VALUE 0xf8
  //    [0, +INF] MASK 0xff..00 VALUE 0x00
  // is [0, +INF] MASK 0xff..ff VALUE 0x00, which is VARYING.
  // Test that we normalized to VARYING.
  unsigned prec = TYPE_PRECISION (voidp);
  p0.set_varying (voidp);
  wide_int mask = wi::mask (8, true, prec);
  wide_int value = wi::uhwi (0xf8, prec);
  irange_bitmask bm (wi::uhwi (0xf8, prec), mask);
  p0.update_bitmask (bm);
  p1.set_varying (voidp);
  bm = irange_bitmask (wi::zero (prec), mask);
  p1.update_bitmask (bm);
  p0.intersect (p1);

  // [10,20] U [15, 30] => [10, 30].
  r0 = range_int (10, 20);
  r1 = range_int (15, 30);
  r0.union_ (r1);
  ASSERT_TRUE (r0 == range_int (10, 30));

  // [15,40] U [] => [15,40].
  r0 = range_int (15, 40);
  r1.set_undefined ();
  r0.union_ (r1);
  ASSERT_TRUE (r0 == range_int (15, 40));

  // [10,20] U [10,10] => [10,20].
  r0 = range_int (10, 20);
  r1 = range_int (10, 10);
  r0.union_ (r1);
  ASSERT_TRUE (r0 == range_int (10, 20));

  // [10,20] U [9,9] => [9,20].
  r0 = range_int (10, 20);
  r1 = range_int (9, 9);
  r0.union_ (r1);
  ASSERT_TRUE (r0 == range_int (9, 20));

  // [10,20] ^ [15,30] => [15,20].
  r0 = range_int (10, 20);
  r1 = range_int (15, 30);
  r0.intersect (r1);
  ASSERT_TRUE (r0 == range_int (15, 20));

  // Test the internal sanity of wide_int's wrt HWIs.
  ASSERT_TRUE (wi::max_value (TYPE_PRECISION (boolean_type_node),
			      TYPE_SIGN (boolean_type_node))
	       == wi::uhwi (1, TYPE_PRECISION (boolean_type_node)));

  // Test zero_p().
  r0 = range_int (0, 0);
  ASSERT_TRUE (r0.zero_p ());

  // Test contains_zero_p().
  r0 = range_int (0, 0);
  res = r0.invert ();
  ASSERT_TRUE (res);
  ASSERT_FALSE (r0.contains_zero_p ());

  // r0 = ~[1,1]
  r0 = range_int (1, 1, VR_ANTI_RANGE);
  // r1 = ~[3,3]
  r1 = range_int (3, 3, VR_ANTI_RANGE);

  // vv = [0,0][2,2][4, MAX]
  int_range<3> vv = r0;
  vv.intersect (r1);

  ASSERT_TRUE (vv.contains_p (UINT (2)));
  ASSERT_TRUE (vv.num_pairs () == 3);

  r0 = range_int (1, 1);
  // And union it with  [0,0][2,2][4,MAX] multi range
  r0.union_ (vv);
  // The result should be [0,2][4,MAX], or ~[3,3]  but it must contain 2
  ASSERT_TRUE (r0.contains_p (INT (2)));
}

static void
range_tests_nonzero_bits ()
{
  int_range<8> r0, r1;

  // Adding nonzero bits to a varying drops the varying.
  r0.set_varying (integer_type_node);
  r0.set_nonzero_bits (INT (255));
  ASSERT_TRUE (!r0.varying_p ());

  // Test contains_p with nonzero bits.
  r0.set_zero (integer_type_node);
  ASSERT_TRUE (r0.contains_p (INT (0)));
  ASSERT_FALSE (r0.contains_p (INT (1)));
  r0.set_nonzero_bits (INT (0xfe));
  ASSERT_FALSE (r0.contains_p (INT (0x100)));
  ASSERT_FALSE (r0.contains_p (INT (0x3)));

  // Union of nonzero bits.
  r0.set_varying (integer_type_node);
  r0.set_nonzero_bits (INT (0xf0));
  r1.set_varying (integer_type_node);
  r1.set_nonzero_bits (INT (0xf));
  r0.union_ (r1);
  ASSERT_TRUE (r0.get_nonzero_bits () == 0xff);

  // Intersect of nonzero bits.
  r0 = range_int (0, 255);
  r0.set_nonzero_bits (INT (0xfe));
  r1.set_varying (integer_type_node);
  r1.set_nonzero_bits (INT (0xf0));
  r0.intersect (r1);
  ASSERT_TRUE (r0.get_nonzero_bits () == 0xf0);

  // Intersect where the mask of nonzero bits is implicit from the range.
  r0.set_varying (integer_type_node);
  r1 = range_int (0, 255);
  r0.intersect (r1);
  ASSERT_TRUE (r0.get_nonzero_bits () == 0xff);

  // Test that setting a nonzero bit of 1 does not pessimize the range.
  r0.set_zero (integer_type_node);
  r0.set_nonzero_bits (INT (1));
  ASSERT_TRUE (r0.zero_p ());

  // Now test that range bounds are snapped to match bitmask alignments.
  test_irange_snap_bounds ();
}

// Build an frange from string endpoints.

static inline frange
frange_float (const char *lb, const char *ub, tree type = float_type_node)
{
  REAL_VALUE_TYPE min, max;
  gcc_assert (real_from_string (&min, lb) == 0);
  gcc_assert (real_from_string (&max, ub) == 0);
  return frange (type, min, max);
}

// Build the REAL_VALUE_TYPE for the string S.

static REAL_VALUE_TYPE
real_from_str (const char *s)
{
  REAL_VALUE_TYPE r;
  gcc_assert (real_from_string (&r, s) == 0);
  return r;
}

static void
range_tests_sub_ranges ()
{
  frange r0, r1;

  // A union of two disjoint intervals keeps both.
  r0 = frange_float ("3", "5");
  r1 = frange_float ("10", "12");
  r0.union_ (r1);
  ASSERT_EQ (r0.num_pairs (), 2);
  ASSERT_TRUE (r0.contains_p (real_from_str ("4")));
  ASSERT_TRUE (r0.contains_p (real_from_str ("11")));
  ASSERT_FALSE (r0.contains_p (real_from_str ("7")));

  REAL_VALUE_TYPE three = real_from_str ("3");
  REAL_VALUE_TYPE twelve = real_from_str ("12");
  ASSERT_TRUE (real_identical (&r0.lower_bound (), &three));
  ASSERT_TRUE (real_identical (&r0.upper_bound (), &twelve));

  // Intersecting away one side leaves a single interval again.
  r1 = frange_float ("0", "6");
  r0.intersect (r1);
  ASSERT_EQ (r0.num_pairs (), 1);
  ASSERT_TRUE (r0.contains_p (real_from_str ("4")));
  ASSERT_FALSE (r0.contains_p (real_from_str ("11")));

  // Overlapping intervals fuse rather than leave a gap.
  r0 = frange_float ("3", "8");
  r1 = frange_float ("5", "12");
  r0.union_ (r1);
  ASSERT_EQ (r0.num_pairs (), 1);
  ASSERT_TRUE (r0.contains_p (real_from_str ("7")));

  if (frange::MAX_PAIRS == 2)
    {
      // When more pieces arrive than fit, the last slot swallows the tail:
      // [0,1] stays and [3,4], [100,101] merge into [3,101].
      r0 = frange_float ("0", "1");
      r1 = frange_float ("100", "101");
      r0.union_ (r1);
      r1 = frange_float ("3", "4");
      r0.union_ (r1);
      ASSERT_EQ (r0.num_pairs (), 2);
      ASSERT_TRUE (r0.contains_p (real_from_str ("50")));
      ASSERT_TRUE (r0.contains_p (real_from_str ("3.5")));
      ASSERT_TRUE (r0.contains_p (real_from_str ("100.5")));
    }

  // Equality accounts for the sub-ranges.
  r0 = frange_float ("3", "5");
  r1 = frange_float ("10", "12");
  r0.union_ (r1);
  r1 = frange_float ("3", "12");
  ASSERT_NE (r0, r1);

  // Intersecting every piece away, with the NAN cleared, leaves UNDEFINED.
  r0 = frange_float ("3", "5");
  r1 = frange_float ("10", "12");
  r0.union_ (r1);
  r0.clear_nan ();
  r1 = frange_float ("20", "25");
  r1.clear_nan ();
  r0.intersect (r1);
  ASSERT_TRUE (r0.undefined_p ());
}

// Build a range that excludes the single point C.

static frange
frange_float_excluding (const char *c)
{
  REAL_VALUE_TYPE r = real_from_str (c);
  frange f;
  f.set (float_type_node, r, r, VR_ANTI_RANGE);
  return f;
}

static void
range_tests_excluding ()
{
  frange r0, r1;

  // "x != 1.0" is two sub-ranges with 1.0 missing.
  r0 = frange_float_excluding ("1.0");
  ASSERT_FALSE (r0.varying_p ());
  ASSERT_FALSE (r0.undefined_p ());
  ASSERT_EQ (r0.num_pairs (), 2);
  ASSERT_FALSE (r0.contains_p (real_from_str ("1.0")));
  ASSERT_TRUE (r0.contains_p (real_from_str ("2.0")));
  ASSERT_TRUE (r0.contains_p (real_from_str ("0.0")));
  ASSERT_TRUE (r0.contains_p (real_from_str ("-1.0")));
  ASSERT_FALSE (r0.singleton_p ());
  // A NAN compares unequal to everything, so this says nothing about NANs.
  if (HONOR_NANS (float_type_node))
    ASSERT_TRUE (r0.maybe_isnan ());
  // The extremes still span the domain.
  REAL_VALUE_TYPE dom_min = frange_val_min (float_type_node);
  REAL_VALUE_TYPE dom_max = frange_val_max (float_type_node);
  ASSERT_TRUE (real_identical (&r0.lower_bound (), &dom_min));
  ASSERT_TRUE (real_identical (&r0.upper_bound (), &dom_max));

  // Any constant, not just 0.0 or 1.0.
  r0 = frange_float_excluding ("5.5");
  ASSERT_EQ (r0.num_pairs (), 2);
  ASSERT_FALSE (r0.contains_p (real_from_str ("5.5")));
  ASSERT_TRUE (r0.contains_p (real_from_str ("5.4")));

  // "x != 1.0" met with [1.0, 1.0] is empty.
  r0 = frange_float_excluding ("1.0");
  r1 = frange_float ("1.0", "1.0");
  r1.clear_nan ();
  r0.intersect (r1);
  ASSERT_TRUE (r0.undefined_p ());

  // Excluding a point outside a range changes nothing.
  r0 = frange_float ("3.0", "5.0");
  r0.clear_nan ();
  r1 = frange_float_excluding ("1.0");
  r0.intersect (r1);
  ASSERT_EQ (r0.num_pairs (), 1);
  ASSERT_TRUE (r0.contains_p (real_from_str ("3.0")));
  ASSERT_TRUE (r0.contains_p (real_from_str ("5.0")));

  // Union puts the point back.
  r0 = frange_float_excluding ("1.0");
  r1 = frange_float ("1.0", "1.0");
  r0.union_ (r1);
  ASSERT_TRUE (r0.varying_p ());

  // Two different exclusions cannot both be held.
  r0 = frange_float_excluding ("1.0");
  r1 = frange_float_excluding ("2.0");
  r0.union_ (r1);
  ASSERT_TRUE (r0.varying_p ());

  // Nor can an intersection hold both.
  r0 = frange_float_excluding ("1.0");
  r1 = frange_float_excluding ("2.0");
  r0.intersect (r1);
  ASSERT_TRUE (r0.contains_p (real_from_str ("0.0")));
  ASSERT_TRUE (r0.contains_p (real_from_str ("3.0")));

  // Equality accounts for the gap.
  r0 = frange_float_excluding ("1.0");
  r1 = frange_float_excluding ("2.0");
  ASSERT_NE (r0, r1);
  r1 = frange_float_excluding ("1.0");
  ASSERT_EQ (r0, r1);
}

static void
range_tests_sub_ranges_zero ()
{
  frange r0, r1;

  // "x != 0.0" must exclude BOTH zeros, since -0.0 == 0.0 and so "x != 0.0" is
  // false for either.  The seam lands on the denormals either side of zero,
  // which falls out of nextafter with no special case.
  r0 = frange_float_excluding ("0.0");
  ASSERT_EQ (r0.num_pairs (), 2);
  ASSERT_FALSE (r0.contains_p (dconst0));
  ASSERT_FALSE (r0.contains_p (dconstm0));
  ASSERT_TRUE (r0.contains_p (real_from_str ("1.0")));
  ASSERT_TRUE (r0.contains_p (real_from_str ("-1.0")));

  // Excluding zero from [-0.0, 5.0] eats the lower end entirely.
  r0.set_nonzero (float_type_node);
  ASSERT_FALSE (r0.contains_zero_p ());
  ASSERT_FALSE (r0.contains_p (dconst0));
  ASSERT_FALSE (r0.contains_p (dconstm0));

  // A NAN is not a zero, so clearing the NAN leaves the range nonzero.
  r0.clear_nan ();
  ASSERT_FALSE (r0.contains_zero_p ());

  // A range that avoids zero does not contain zero.
  r0 = frange_float ("1.0", "10.0");
  ASSERT_FALSE (r0.contains_zero_p ());

  // Excluding zero from [-0.0, 5.0] leaves (0, 5], which does not contain zero.
  r0 = frange_float ("-0.0", "5.0");
  r0.clear_nan ();
  r1 = frange_float_excluding ("0.0");
  r0.intersect (r1);
  ASSERT_EQ (r0.num_pairs (), 1);
  ASSERT_FALSE (r0.contains_zero_p ());
  ASSERT_FALSE (r0.contains_p (dconst0));
  ASSERT_FALSE (r0.contains_p (dconstm0));
  ASSERT_TRUE (r0.contains_p (real_from_str ("5.0")));

  // -0.0 and +0.0 abut: nothing is representable between them, so the two
  // halves fuse into one interval rather than leaving a gap.
  r0 = frange_float ("-5", "-0.0");
  r0.clear_nan ();
  r1 = frange_float ("0.0", "5");
  r1.clear_nan ();
  r0.union_ (r1);
  ASSERT_EQ (r0.num_pairs (), 1);
  ASSERT_TRUE (r0.contains_p (dconst0));
  ASSERT_TRUE (r0.contains_p (dconstm0));
}

// A cached frange must come back with every sub-range intact.

static void
range_tests_sub_ranges_storage ()
{
  vrange_allocator alloc (false);

  // A two-piece range comes back as two pieces, unchanged.
  frange r0 = frange_float ("3", "5");
  frange r1 = frange_float ("10", "12");
  r0.union_ (r1);
  ASSERT_EQ (r0.num_pairs (), 2);

  vrange_storage *slot = alloc.clone (r0);
  frange r2;
  slot->get_vrange (r2, float_type_node);
  ASSERT_EQ (r2.num_pairs (), 2);
  ASSERT_EQ (r2, r0);
}

// NANs and sub-ranges: unioning in a NAN keeps the intervals, while
// intersecting the intervals away collapses to a plain NAN with a single
// pair.

static void
range_tests_sub_ranges_nan ()
{
  frange r0, r1;

  // Union with a NAN keeps both intervals and gains the NAN.
  r0 = frange_float ("3", "5");
  r1 = frange_float ("10", "12");
  r0.union_ (r1);
  r0.clear_nan ();
  r1.set_nan (float_type_node);
  r0.union_ (r1);
  ASSERT_EQ (r0.num_pairs (), 2);
  ASSERT_TRUE (r0.maybe_isnan ());

  // Intersecting the intervals away leaves just the NAN.
  r0 = frange_float ("3", "5");
  r1 = frange_float ("10", "12");
  r0.union_ (r1);
  r1 = frange_float ("20", "25");
  r0.intersect (r1);
  ASSERT_TRUE (r0.known_isnan ());
  ASSERT_EQ (r0.num_pairs (), 1);
}

static void
range_tests_nan ()
{
  frange r0, r1;
  REAL_VALUE_TYPE q, r;
  bool signbit;

  // Equal ranges but with differing NAN bits are not equal.
  if (HONOR_NANS (float_type_node))
    {
      r1 = frange_float ("10", "12");
      r0 = r1;
      ASSERT_EQ (r0, r1);
      r0.clear_nan ();
      ASSERT_NE (r0, r1);
      r0.update_nan ();
      ASSERT_EQ (r0, r1);

      // [10, 20] NAN ^ [30, 40] NAN = NAN.
      r0 = frange_float ("10", "20");
      r1 = frange_float ("30", "40");
      r0.intersect (r1);
      ASSERT_TRUE (r0.known_isnan ());

      // [3,5] U [5,10] NAN = ... NAN
      r0 = frange_float ("3", "5");
      r0.clear_nan ();
      r1 = frange_float ("5", "10");
      r0.union_ (r1);
      ASSERT_TRUE (r0.maybe_isnan ());
    }

  // [5,6] U NAN = [5,6] NAN.
  r0 = frange_float ("5", "6");
  r0.clear_nan ();
  r1.set_nan (float_type_node);
  r0.union_ (r1);
  real_from_string (&q, "5");
  real_from_string (&r, "6");
  ASSERT_TRUE (real_identical (&q, &r0.lower_bound ()));
  ASSERT_TRUE (real_identical (&r, &r0.upper_bound ()));
  ASSERT_TRUE (r0.maybe_isnan ());

  // NAN U NAN = NAN
  r0.set_nan (float_type_node);
  r1.set_nan (float_type_node);
  r0.union_ (r1);
  ASSERT_TRUE (r0.known_isnan ());

  // [INF, INF] NAN ^ NAN = NAN
  r0.set_nan (float_type_node);
  r1 = frange_float ("+Inf", "+Inf");
  if (!HONOR_NANS (float_type_node))
    r1.update_nan ();
  r0.intersect (r1);
  ASSERT_TRUE (r0.known_isnan ());

  // NAN ^ NAN = NAN
  r0.set_nan (float_type_node);
  r1.set_nan (float_type_node);
  r0.intersect (r1);
  ASSERT_TRUE (r0.known_isnan ());

  // +NAN ^ -NAN = UNDEFINED
  r0.set_nan (float_type_node, false);
  r1.set_nan (float_type_node, true);
  r0.intersect (r1);
  ASSERT_TRUE (r0.undefined_p ());

  // VARYING ^ NAN = NAN.
  r0.set_nan (float_type_node);
  r1.set_varying (float_type_node);
  r0.intersect (r1);
  ASSERT_TRUE (r0.known_isnan ());

  // [3,4] ^ NAN = UNDEFINED.
  r0 = frange_float ("3", "4");
  r0.clear_nan ();
  r1.set_nan (float_type_node);
  r0.intersect (r1);
  ASSERT_TRUE (r0.undefined_p ());

  // [-3, 5] ^ NAN = UNDEFINED
  r0 = frange_float ("-3", "5");
  r0.clear_nan ();
  r1.set_nan (float_type_node);
  r0.intersect (r1);
  ASSERT_TRUE (r0.undefined_p ());

  // Setting the NAN bit to yes does not make us a known NAN.
  r0.set_varying (float_type_node);
  r0.update_nan ();
  ASSERT_FALSE (r0.known_isnan ());

  // NAN is in a VARYING.
  r0.set_varying (float_type_node);
  real_nan (&r, "", 1, TYPE_MODE (float_type_node));
  REAL_VALUE_TYPE nan = r;
  ASSERT_TRUE (r0.contains_p (nan));

  // -NAN is in a VARYING.
  r0.set_varying (float_type_node);
  q = real_value_negate (&r);
  REAL_VALUE_TYPE neg_nan = q;
  ASSERT_TRUE (r0.contains_p (neg_nan));

  // Clearing the NAN on a [] NAN is the empty set.
  r0.set_nan (float_type_node);
  r0.clear_nan ();
  ASSERT_TRUE (r0.undefined_p ());

  // [10,20] NAN ^ [21,25] NAN = [NAN]
  r0 = frange_float ("10", "20");
  r0.update_nan ();
  r1 = frange_float ("21", "25");
  r1.update_nan ();
  r0.intersect (r1);
  ASSERT_TRUE (r0.known_isnan ());

  // NAN U [5,6] should be [5,6] +-NAN.
  r0.set_nan (float_type_node);
  r1 = frange_float ("5", "6");
  r1.clear_nan ();
  r0.union_ (r1);
  real_from_string (&q, "5");
  real_from_string (&r, "6");
  ASSERT_TRUE (real_identical (&q, &r0.lower_bound ()));
  ASSERT_TRUE (real_identical (&r, &r0.upper_bound ()));
  ASSERT_TRUE (!r0.signbit_p (signbit));
  ASSERT_TRUE (r0.maybe_isnan ());

  // NAN U NAN shouldn't change anything.
  r0.set_nan (float_type_node);
  r1.set_nan (float_type_node);
  ASSERT_FALSE (r0.union_ (r1));

  // [3,5] NAN U NAN shouldn't change anything.
  r0 = frange_float ("3", "5");
  r1.set_nan (float_type_node);
  ASSERT_FALSE (r0.union_ (r1));

  // [3,5] U NAN *does* trigger a change.
  r0 = frange_float ("3", "5");
  r0.clear_nan ();
  r1.set_nan (float_type_node);
  ASSERT_TRUE (r0.union_ (r1));
}

static void
range_tests_signed_zeros ()
{
  REAL_VALUE_TYPE zero = dconst0;
  REAL_VALUE_TYPE neg_zero = zero;
  neg_zero.sign = 1;
  frange r0, r1;
  bool signbit;

  // [0,0] contains [0,0] but not [-0,-0] and vice versa.
  r0 = frange_float ("0.0", "0.0");
  r1 = frange_float ("-0.0", "-0.0");
  ASSERT_TRUE (r0.contains_p (zero));
  ASSERT_TRUE (!r0.contains_p (neg_zero));
  ASSERT_TRUE (r1.contains_p (neg_zero));
  ASSERT_TRUE (!r1.contains_p (zero));

  // Test contains_p() when we know the sign of the zero.
  r0 = frange_float ("0.0", "0.0");
  ASSERT_TRUE (r0.contains_p (zero));
  ASSERT_FALSE (r0.contains_p (neg_zero));
  r0 = frange_float ("-0.0", "-0.0");
  ASSERT_TRUE (r0.contains_p (neg_zero));
  ASSERT_FALSE (r0.contains_p (zero));

  r0 = frange_float ("-0.0", "0.0");
  ASSERT_TRUE (r0.contains_p (neg_zero));
  ASSERT_TRUE (r0.contains_p (zero));

  r0 = frange_float ("-3", "5");
  ASSERT_TRUE (r0.contains_p (neg_zero));
  ASSERT_TRUE (r0.contains_p (zero));

  // The intersection of zeros that differ in sign is a NAN (or
  // undefined if not honoring NANs).
  r0 = frange_float ("-0.0", "-0.0");
  r1 = frange_float ("0.0", "0.0");
  r0.intersect (r1);
  if (HONOR_NANS (float_type_node))
    ASSERT_TRUE (r0.known_isnan ());
  else
    ASSERT_TRUE (r0.undefined_p ());

  // The union of zeros that differ in sign is a zero with unknown sign.
  r0 = frange_float ("0.0", "0.0");
  r1 = frange_float ("-0.0", "-0.0");
  r0.union_ (r1);
  ASSERT_TRUE (r0.zero_p () && !r0.signbit_p (signbit));

  // [-0, +0] has an unknown sign.
  r0 = frange_float ("-0.0", "0.0");
  ASSERT_TRUE (r0.zero_p () && !r0.signbit_p (signbit));

  // [-0, +0] ^ [0, 0] is [0, 0]
  r0 = frange_float ("-0.0", "0.0");
  r1 = frange_float ("0.0", "0.0");
  r0.intersect (r1);
  ASSERT_TRUE (r0.zero_p ());

  r0 = frange_float ("+0", "5");
  r0.clear_nan ();
  ASSERT_TRUE (r0.signbit_p (signbit) && !signbit);

  r0 = frange_float ("-0", "5");
  r0.clear_nan ();
  ASSERT_TRUE (!r0.signbit_p (signbit));

  r0 = frange_float ("-0", "10");
  r1 = frange_float ("0", "5");
  r0.intersect (r1);
  ASSERT_TRUE (real_iszero (&r0.lower_bound (), false));

  r0 = frange_float ("-0", "5");
  r1 = frange_float ("0", "5");
  r0.union_ (r1);
  ASSERT_TRUE (real_iszero (&r0.lower_bound (), true));

  r0 = frange_float ("-5", "-0");
  r0.update_nan ();
  r1 = frange_float ("0", "0");
  r1.update_nan ();
  r0.intersect (r1);
  if (HONOR_NANS (float_type_node))
    ASSERT_TRUE (r0.known_isnan ());
  else
    ASSERT_TRUE (r0.undefined_p ());

  r0.set_nonnegative (float_type_node);
  if (HONOR_NANS (float_type_node))
    ASSERT_TRUE (r0.maybe_isnan ());

  // Numbers containing zero should have an unknown SIGNBIT.
  r0 = frange_float ("0", "10");
  r0.clear_nan ();
  ASSERT_TRUE (r0.signbit_p (signbit) && !signbit);
}

static void
range_tests_signbit ()
{
  frange r0, r1;
  bool signbit;

  // Negative numbers should have the SIGNBIT set.
  r0 = frange_float ("-5", "-1");
  r0.clear_nan ();
  ASSERT_TRUE (r0.signbit_p (signbit) && signbit);
  // Positive numbers should have the SIGNBIT clear.
  r0 = frange_float ("1", "10");
  r0.clear_nan ();
  ASSERT_TRUE (r0.signbit_p (signbit) && !signbit);
  // Numbers spanning both positive and negative should have an
  // unknown SIGNBIT.
  r0 = frange_float ("-10", "10");
  r0.clear_nan ();
  ASSERT_TRUE (!r0.signbit_p (signbit));
  r0.set_varying (float_type_node);
  ASSERT_TRUE (!r0.signbit_p (signbit));
}

static void
range_tests_flush_denormals ()
{
  // We need -0.0 to exist for any of this to mean anything.
  if (!MODE_HAS_SIGNED_ZEROS (TYPE_MODE (float_type_node)))
    return;

  int save_flag = flag_signed_zeros;
  flag_signed_zeros = 0;

  // Flushing a positive denormal lower bound to zero must canonicalize that
  // zero to -0.0 to agree with set().
  frange flushed = frange_float ("1e-40", "5");
  flushed.clear_nan ();
  flushed.flush_denormals_to_zero ();

  frange built = frange_float ("0", "5");
  built.clear_nan ();

  ASSERT_TRUE (flushed == built);
  ASSERT_TRUE (flushed.contains_p (dconstm0));
  ASSERT_TRUE (flushed.contains_p (dconst0));

  flag_signed_zeros = save_flag;
}

static void
range_tests_floats ()
{
  frange r0, r1;

  if (HONOR_NANS (float_type_node))
    {
      range_tests_nan ();
      range_tests_sub_ranges_nan ();
    }
  range_tests_signbit ();
  range_tests_flush_denormals ();
  range_tests_sub_ranges ();
  range_tests_sub_ranges_storage ();
  range_tests_excluding ();

  if (HONOR_SIGNED_ZEROS (float_type_node))
    {
      range_tests_signed_zeros ();
      range_tests_sub_ranges_zero ();
    }

  // A range of [-INF,+INF] is actually VARYING if no other properties
  // are set.
  r0 = frange_float ("-Inf", "+Inf");
  ASSERT_TRUE (r0.varying_p ());
  // ...unless it has some special property...
  if (HONOR_NANS (r0.type ()))
    {
      r0.clear_nan ();
      ASSERT_FALSE (r0.varying_p ());
    }

  // For most architectures, where float and double are different
  // sizes, having the same endpoints does not necessarily mean the
  // ranges are equal.
  if (!types_compatible_p (float_type_node, double_type_node))
    {
      r0 = frange_float ("3.0", "3.0", float_type_node);
      r1 = frange_float ("3.0", "3.0", double_type_node);
      ASSERT_NE (r0, r1);
    }

  // [3,5] U [10,12] = [3,5][10,12]
  r0 = frange_float ("3", "5");
  r1 = frange_float ("10", "12");
  r0.union_ (r1);
  ASSERT_EQ (r0.num_pairs (), 2);
  ASSERT_NE (r0, frange_float ("3", "12"));

  // [5,10] U [4,8] = [4,10]
  r0 = frange_float ("5", "10");
  r1 = frange_float ("4", "8");
  r0.union_ (r1);
  ASSERT_EQ (r0, frange_float ("4", "10"));

  // [3,5] U [4,10] = [3,10]
  r0 = frange_float ("3", "5");
  r1 = frange_float ("4", "10");
  r0.union_ (r1);
  ASSERT_EQ (r0, frange_float ("3", "10"));

  // [4,10] U [5,11] = [4,11]
  r0 = frange_float ("4", "10");
  r1 = frange_float ("5", "11");
  r0.union_ (r1);
  ASSERT_EQ (r0, frange_float ("4", "11"));

  // [3,12] ^ [10,12] = [10,12].
  r0 = frange_float ("3", "12");
  r1 = frange_float ("10", "12");
  r0.intersect (r1);
  ASSERT_EQ (r0, frange_float ("10", "12"));

  // [10,12] ^ [11,11] = [11,11]
  r0 = frange_float ("10", "12");
  r1 = frange_float ("11", "11");
  r0.intersect (r1);
  ASSERT_EQ (r0, frange_float ("11", "11"));

  // [10,20] ^ [5,15] = [10,15]
  r0 = frange_float ("10", "20");
  r1 = frange_float ("5",  "15");
  r0.intersect (r1);
  ASSERT_EQ (r0, frange_float ("10", "15"));

  // [10,20] ^ [15,25] = [15,20]
  r0 = frange_float ("10", "20");
  r1 = frange_float ("15", "25");
  r0.intersect (r1);
  ASSERT_EQ (r0, frange_float ("15", "20"));

  // [10,20] ^ [21,25] = []
  r0 = frange_float ("10", "20");
  r0.clear_nan ();
  r1 = frange_float ("21", "25");
  r1.clear_nan ();
  r0.intersect (r1);
  ASSERT_TRUE (r0.undefined_p ());

  if (HONOR_INFINITIES (float_type_node))
    {
      // Make sure [-Inf, -Inf] doesn't get normalized.
      r0 = frange_float ("-Inf", "-Inf");
      ASSERT_TRUE (real_isinf (&r0.lower_bound (), true));
      ASSERT_TRUE (real_isinf (&r0.upper_bound (), true));
    }

  // Test that reading back a global range yields the same result as
  // what we wrote into it.
  tree ssa = make_temp_ssa_name (float_type_node, NULL, "blah");
  r0.set_varying (float_type_node);
  r0.clear_nan ();
  set_range_info (ssa, r0);
  get_global_range_query ()->range_of_expr (r1, ssa);
  ASSERT_EQ (r0, r1);
}

// Run floating range tests for various combinations of NAN and INF
// support.

static void
range_tests_floats_various ()
{
  int save_finite_math_only = flag_finite_math_only;

  // Test -ffinite-math-only.
  flag_finite_math_only = 1;
  range_tests_floats ();
  // Test -fno-finite-math-only.
  flag_finite_math_only = 0;
  range_tests_floats ();

  flag_finite_math_only = save_finite_math_only;
}

void
range_tests ()
{
  range_tests_irange3 ();
  range_tests_int_range_max ();
  range_tests_strict_enum ();
  range_tests_nonzero_bits ();
  range_tests_floats_various ();
  range_tests_misc ();
}

} // namespace selftest

#endif // CHECKING_P
