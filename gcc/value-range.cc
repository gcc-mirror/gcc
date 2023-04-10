/* Support routines for value ranges.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.
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
#include "fold-const.h"
#include "gimple-range.h"

void
irange::accept (const vrange_visitor &v) const
{
  v.visit (*this);
}

void
unsupported_range::accept (const vrange_visitor &v) const
{
  v.visit (*this);
}

// Convenience function only available for integers and pointers.

wide_int
Value_Range::lower_bound () const
{
  if (is_a <irange> (*m_vrange))
    return as_a <irange> (*m_vrange).lower_bound ();
  gcc_unreachable ();
}

// Convenience function only available for integers and pointers.

wide_int
Value_Range::upper_bound () const
{
  if (is_a <irange> (*m_vrange))
    return as_a <irange> (*m_vrange).upper_bound ();
  gcc_unreachable ();
}

void
Value_Range::dump (FILE *out) const
{
  if (m_vrange)
    m_vrange->dump (out);
  else
    fprintf (out, "NULL");
}

DEBUG_FUNCTION void
debug (const Value_Range &r)
{
  r.dump (stderr);
  fprintf (stderr, "\n");
}

// Default vrange definitions.

bool
vrange::contains_p (tree) const
{
  return varying_p ();
}

bool
vrange::singleton_p (tree *) const
{
  return false;
}

void
vrange::set (tree min, tree, value_range_kind)
{
  set_varying (TREE_TYPE (min));
}

tree
vrange::type () const
{
  return void_type_node;
}

bool
vrange::supports_type_p (const_tree) const
{
  return false;
}

void
vrange::set_undefined ()
{
  m_kind = VR_UNDEFINED;
}

void
vrange::set_varying (tree)
{
  m_kind = VR_VARYING;
}

bool
vrange::union_ (const vrange &r)
{
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
vrange::intersect (const vrange &r)
{
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
vrange::zero_p () const
{
  return false;
}

bool
vrange::nonzero_p () const
{
  return false;
}

void
vrange::set_nonzero (tree type)
{
  set_varying (type);
}

void
vrange::set_zero (tree type)
{
  set_varying (type);
}

void
vrange::set_nonnegative (tree type)
{
  set_varying (type);
}

bool
vrange::fits_p (const vrange &) const
{
  return true;
}

// Assignment operator for generic ranges.  Copying incompatible types
// is not allowed.

vrange &
vrange::operator= (const vrange &src)
{
  if (is_a <irange> (src))
    as_a <irange> (*this) = as_a <irange> (src);
  else if (is_a <frange> (src))
    as_a <frange> (*this) = as_a <frange> (src);
  else
    gcc_unreachable ();
  return *this;
}

// Equality operator for generic ranges.

bool
vrange::operator== (const vrange &src) const
{
  if (is_a <irange> (src))
    return as_a <irange> (*this) == as_a <irange> (src);
  if (is_a <frange> (src))
    return as_a <frange> (*this) == as_a <frange> (src);
  gcc_unreachable ();
}

// Wrapper for vrange_printer to dump a range to a file.

void
vrange::dump (FILE *file) const
{
  pretty_printer buffer;
  pp_needs_newline (&buffer) = true;
  buffer.buffer->stream = file;
  vrange_printer vrange_pp (&buffer);
  this->accept (vrange_pp);
  pp_flush (&buffer);
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
  set (build_int_cst (type, 0), TYPE_MAX_VALUE (type));
}

void
frange::accept (const vrange_visitor &v) const
{
  v.visit (*this);
}

// Flush denormal endpoints to the appropriate 0.0.

void
frange::flush_denormals_to_zero ()
{
  if (undefined_p () || known_isnan ())
    return;

  machine_mode mode = TYPE_MODE (type ());
  // Flush [x, -DENORMAL] to [x, -0.0].
  if (real_isdenormal (&m_max, mode) && real_isneg (&m_max))
    {
      m_max = dconst0;
      if (HONOR_SIGNED_ZEROS (m_type))
	m_max.sign = 1;
    }
  // Flush [+DENORMAL, x] to [+0.0, x].
  if (real_isdenormal (&m_min, mode) && !real_isneg (&m_min))
    m_min = dconst0;
}

// Setter for franges.

void
frange::set (tree type,
	     const REAL_VALUE_TYPE &min, const REAL_VALUE_TYPE &max,
	     const nan_state &nan, value_range_kind kind)
{
  switch (kind)
    {
    case VR_UNDEFINED:
      set_undefined ();
      return;
    case VR_VARYING:
    case VR_ANTI_RANGE:
      set_varying (type);
      return;
    case VR_RANGE:
      break;
    default:
      gcc_unreachable ();
    }

  // Handle NANs.
  if (real_isnan (&min) || real_isnan (&max))
    {
      gcc_checking_assert (real_identical (&min, &max));
      bool sign = real_isneg (&min);
      set_nan (type, sign);
      return;
    }

  m_kind = kind;
  m_type = type;
  m_min = min;
  m_max = max;
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

  if (!MODE_HAS_SIGNED_ZEROS (TYPE_MODE (m_type)))
    {
      if (real_iszero (&m_min, 1))
	m_min.sign = 0;
      if (real_iszero (&m_max, 1))
	m_max.sign = 0;
    }
  else if (!HONOR_SIGNED_ZEROS (m_type))
    {
      if (real_iszero (&m_max, 1))
	m_max.sign = 0;
      if (real_iszero (&m_min, 0))
	m_min.sign = 1;
    }

  // For -ffinite-math-only we can drop ranges outside the
  // representable numbers to min/max for the type.
  if (!HONOR_INFINITIES (m_type))
    {
      REAL_VALUE_TYPE min_repr = frange_val_min (m_type);
      REAL_VALUE_TYPE max_repr = frange_val_max (m_type);
      if (real_less (&m_min, &min_repr))
	m_min = min_repr;
      else if (real_less (&max_repr, &m_min))
	m_min = max_repr;
      if (real_less (&max_repr, &m_max))
	m_max = max_repr;
      else if (real_less (&m_max, &min_repr))
	m_max = min_repr;
    }

  // Check for swapped ranges.
  gcc_checking_assert (real_compare (LE_EXPR, &min, &max));

  normalize_kind ();

  if (flag_checking)
    verify_range ();
}

// Setter for an frange defaulting the NAN possibility to +-NAN when
// HONOR_NANS.

void
frange::set (tree type,
	     const REAL_VALUE_TYPE &min, const REAL_VALUE_TYPE &max,
	     value_range_kind kind)
{
  nan_state nan;
  set (type, min, max, nan, kind);
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
      && frange_val_is_min (m_min, m_type)
      && frange_val_is_max (m_max, m_type))
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
	  m_min = frange_val_min (m_type);
	  m_max = frange_val_max (m_type);
	  return true;
	}
    }
  else if (m_kind == VR_NAN && !m_pos_nan && !m_neg_nan)
    set_undefined ();
  return false;
}

// Union or intersect the zero endpoints of two ranges.  For example:
//   [-0,  x] U [+0,  x] => [-0,  x]
//   [ x, -0] U [ x, +0] => [ x, +0]
//   [-0,  x] ^ [+0,  x] => [+0,  x]
//   [ x, -0] ^ [ x, +0] => [ x, -0]
//
// UNION_P is true when performing a union, or false when intersecting.

bool
frange::combine_zeros (const frange &r, bool union_p)
{
  gcc_checking_assert (!undefined_p () && !known_isnan ());

  bool changed = false;
  if (real_iszero (&m_min) && real_iszero (&r.m_min)
      && real_isneg (&m_min) != real_isneg (&r.m_min))
    {
      m_min.sign = union_p;
      changed = true;
    }
  if (real_iszero (&m_max) && real_iszero (&r.m_max)
      && real_isneg (&m_max) != real_isneg (&r.m_max))
    {
      m_max.sign = !union_p;
      changed = true;
    }
  // If the signs are swapped, the resulting range is empty.
  if (m_min.sign == 0 && m_max.sign == 1)
    {
      if (maybe_isnan ())
	m_kind = VR_NAN;
      else
	set_undefined ();
      changed = true;
    }
  return changed;
}

// Union two ranges when one is known to be a NAN.

bool
frange::union_nans (const frange &r)
{
  gcc_checking_assert (known_isnan () || r.known_isnan ());

  if (known_isnan ())
    {
      m_kind = r.m_kind;
      m_min = r.m_min;
      m_max = r.m_max;
    }
  m_pos_nan |= r.m_pos_nan;
  m_neg_nan |= r.m_neg_nan;
  normalize_kind ();
  if (flag_checking)
    verify_range ();
  return true;
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
  bool changed = false;
  if (m_pos_nan != r.m_pos_nan || m_neg_nan != r.m_neg_nan)
    {
      m_pos_nan |= r.m_pos_nan;
      m_neg_nan |= r.m_neg_nan;
      changed = true;
    }

  // Combine endpoints.
  if (real_less (&r.m_min, &m_min))
    {
      m_min = r.m_min;
      changed = true;
    }
  if (real_less (&m_max, &r.m_max))
    {
      m_max = r.m_max;
      changed = true;
    }

  if (HONOR_SIGNED_ZEROS (m_type))
    changed |= combine_zeros (r, true);

  changed |= normalize_kind ();
  if (flag_checking)
    verify_range ();
  return changed;
}

// Intersect two ranges when one is known to be a NAN.

bool
frange::intersect_nans (const frange &r)
{
  gcc_checking_assert (known_isnan () || r.known_isnan ());

  m_pos_nan &= r.m_pos_nan;
  m_neg_nan &= r.m_neg_nan;
  if (maybe_isnan ())
    m_kind = VR_NAN;
  else
    set_undefined ();
  if (flag_checking)
    verify_range ();
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
  bool changed = false;
  if (m_pos_nan != r.m_pos_nan || m_neg_nan != r.m_neg_nan)
    {
      m_pos_nan &= r.m_pos_nan;
      m_neg_nan &= r.m_neg_nan;
      changed = true;
    }

  // Combine endpoints.
  if (real_less (&m_min, &r.m_min))
    {
      m_min = r.m_min;
      changed = true;
    }
  if (real_less (&r.m_max, &m_max))
    {
      m_max = r.m_max;
      changed = true;
    }
  // If the endpoints are swapped, the resulting range is empty.
  if (real_less (&m_max, &m_min))
    {
      if (maybe_isnan ())
	m_kind = VR_NAN;
      else
	set_undefined ();
      if (flag_checking)
	verify_range ();
      return true;
    }

  if (HONOR_SIGNED_ZEROS (m_type))
    changed |= combine_zeros (r, false);

  changed |= normalize_kind ();
  if (flag_checking)
    verify_range ();
  return changed;
}

frange &
frange::operator= (const frange &src)
{
  m_kind = src.m_kind;
  m_type = src.m_type;
  m_min = src.m_min;
  m_max = src.m_max;
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

      if (known_isnan () || src.known_isnan ())
	return false;

      return (real_identical (&m_min, &src.m_min)
	      && real_identical (&m_max, &src.m_max)
	      && m_pos_nan == src.m_pos_nan
	      && m_neg_nan == src.m_neg_nan
	      && types_compatible_p (m_type, src.m_type));
    }
  return false;
}

// Return TRUE if range contains the TREE_REAL_CST_PTR in CST.

bool
frange::contains_p (tree cst) const
{
  gcc_checking_assert (m_kind != VR_ANTI_RANGE);
  const REAL_VALUE_TYPE *rv = TREE_REAL_CST_PTR (cst);

  if (undefined_p ())
    return false;

  if (varying_p ())
    return true;

  if (real_isnan (rv))
    {
      // No NAN in range.
      if (!m_pos_nan && !m_neg_nan)
	return false;
      // Both +NAN and -NAN are present.
      if (m_pos_nan && m_neg_nan)
	return true;
      return m_neg_nan == rv->sign;
    }
  if (known_isnan ())
    return false;

  if (real_compare (GE_EXPR, rv, &m_min) && real_compare (LE_EXPR, rv, &m_max))
    {
      // Make sure the signs are equal for signed zeros.
      if (HONOR_SIGNED_ZEROS (m_type) && real_iszero (rv))
	return rv->sign == m_min.sign || rv->sign == m_max.sign;
      return true;
    }
  return false;
}

// If range is a singleton, place it in RESULT and return TRUE.  If
// RESULT is NULL, just return TRUE.
//
// A NAN can never be a singleton.

bool
frange::singleton_p (tree *result) const
{
  if (m_kind == VR_RANGE && real_identical (&m_min, &m_max))
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
	  if (real_isinf (&m_min))
	    return false;
	  REAL_VALUE_TYPE r;
	  real_convert (&r, DFmode, &m_min);
	  if (real_identical (&r, &m_min))
	    return false;
	}

      if (result)
	*result = build_real (m_type, m_min);
      return true;
    }
  return false;
}

bool
frange::supports_type_p (const_tree type) const
{
  return supports_p (type);
}

void
frange::verify_range ()
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
      gcc_checking_assert (frange_val_is_min (m_min, m_type));
      gcc_checking_assert (frange_val_is_max (m_max, m_type));
      if (HONOR_NANS (m_type))
	gcc_checking_assert (m_pos_nan && m_neg_nan);
      else
	gcc_checking_assert (!m_pos_nan && !m_neg_nan);
      return;
    case VR_RANGE:
      gcc_checking_assert (m_type);
      break;
    case VR_NAN:
      gcc_checking_assert (m_type);
      gcc_checking_assert (m_pos_nan || m_neg_nan);
      return;
    default:
      gcc_unreachable ();
    }

  // NANs cannot appear in the endpoints of a range.
  gcc_checking_assert (!real_isnan (&m_min) && !real_isnan (&m_max));

  // Make sure we don't have swapped ranges.
  gcc_checking_assert (!real_less (&m_max, &m_min));

  // [ +0.0, -0.0 ] is nonsensical.
  gcc_checking_assert (!(real_iszero (&m_min, 0) && real_iszero (&m_max, 1)));

  // If all the properties are clear, we better not span the entire
  // domain, because that would make us varying.
  if (m_pos_nan && m_neg_nan)
    gcc_checking_assert (!frange_val_is_min (m_min, m_type)
			 || !frange_val_is_max (m_max, m_type));
}

// We can't do much with nonzeros yet.
void
frange::set_nonzero (tree type)
{
  set_varying (type);
}

// We can't do much with nonzeros yet.
bool
frange::nonzero_p () const
{
  return false;
}

// Set range to [+0.0, +0.0] if honoring signed zeros, or [0.0, 0.0]
// otherwise.

void
frange::set_zero (tree type)
{
  if (HONOR_SIGNED_ZEROS (type))
    {
      REAL_VALUE_TYPE dconstm0 = dconst0;
      dconstm0.sign = 1;
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
	  && real_iszero (&m_min)
	  && real_iszero (&m_max));
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

// Here we copy between any two irange's.  The ranges can be legacy or
// multi-ranges, and copying between any combination works correctly.

irange &
irange::operator= (const irange &src)
{
  if (legacy_mode_p ())
    {
      copy_to_legacy (src);
      return *this;
    }
  if (src.legacy_mode_p ())
    {
      copy_legacy_to_multi_range (src);
      return *this;
    }

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
  m_kind = src.m_kind;
  m_nonzero_mask = src.m_nonzero_mask;
  if (flag_checking)
    verify_range ();
  return *this;
}

// Return TRUE if range is a multi-range that can be represented as a
// VR_ANTI_RANGE.

bool
irange::maybe_anti_range () const
{
  tree ttype = type ();
  unsigned int precision = TYPE_PRECISION (ttype);
  signop sign = TYPE_SIGN (ttype);
  return (num_pairs () > 1
	  && precision > 1
	  && lower_bound () == wi::min_value (precision, sign)
	  && upper_bound () == wi::max_value (precision, sign));
}

void
irange::copy_legacy_to_multi_range (const irange &src)
{
  gcc_checking_assert (src.legacy_mode_p ());
  gcc_checking_assert (!legacy_mode_p ());
  if (src.undefined_p ())
    set_undefined ();
  else if (src.varying_p ())
    set_varying (src.type ());
  else
    {
      if (range_has_numeric_bounds_p (&src))
	set (src.min (), src.max (), src.kind ());
      else
	{
	  value_range cst (src);
	  cst.normalize_symbolics ();
	  gcc_checking_assert (cst.varying_p () || cst.kind () == VR_RANGE);
	  set (cst.min (), cst.max ());
	}
    }
}

// Copy any type of irange into a legacy.

void
irange::copy_to_legacy (const irange &src)
{
  gcc_checking_assert (legacy_mode_p ());
  // Handle legacy to legacy and other things that are easy to copy.
  if (src.legacy_mode_p () || src.varying_p () || src.undefined_p ())
    {
      m_num_ranges = src.m_num_ranges;
      m_base[0] = src.m_base[0];
      m_base[1] = src.m_base[1];
      m_kind = src.m_kind;
      m_nonzero_mask = src.m_nonzero_mask;
      return;
    }
  // Copy multi-range to legacy.
  if (src.maybe_anti_range ())
    {
      int_range<3> r (src);
      r.invert ();
      // Use tree variants to save on tree -> wi -> tree conversions.
      set (r.tree_lower_bound (0), r.tree_upper_bound (0), VR_ANTI_RANGE);
    }
  else
    set (src.tree_lower_bound (), src.tree_upper_bound ());
}

// Swap MIN/MAX if they are out of order and adjust KIND appropriately.

static void
swap_out_of_order_endpoints (tree &min, tree &max, value_range_kind &kind)
{
  gcc_checking_assert (kind != VR_UNDEFINED);
  if (kind == VR_VARYING)
    return;
  /* Wrong order for min and max, to swap them and the VR type we need
     to adjust them.  */
  if (tree_int_cst_lt (max, min))
    {
      tree one, tmp;

      /* For one bit precision if max < min, then the swapped
	 range covers all values, so for VR_RANGE it is varying and
	 for VR_ANTI_RANGE empty range, so drop to varying as well.  */
      if (TYPE_PRECISION (TREE_TYPE (min)) == 1)
	{
	  kind = VR_VARYING;
	  return;
	}

      one = build_int_cst (TREE_TYPE (min), 1);
      tmp = int_const_binop (PLUS_EXPR, max, one);
      max = int_const_binop (MINUS_EXPR, min, one);
      min = tmp;

      /* There's one corner case, if we had [C+1, C] before we now have
	 that again.  But this represents an empty value range, so drop
	 to varying in this case.  */
      if (tree_int_cst_lt (max, min))
	{
	  kind = VR_VARYING;
	  return;
	}
      kind = kind == VR_RANGE ? VR_ANTI_RANGE : VR_RANGE;
    }
}

void
irange::irange_set (tree min, tree max)
{
  gcc_checking_assert (!POLY_INT_CST_P (min));
  gcc_checking_assert (!POLY_INT_CST_P (max));

  m_base[0] = min;
  m_base[1] = max;
  m_num_ranges = 1;
  m_kind = VR_RANGE;
  m_nonzero_mask = NULL;
  normalize_kind ();

  if (flag_checking)
    verify_range ();
}

void
irange::irange_set_1bit_anti_range (tree min, tree max)
{
  tree type = TREE_TYPE (min);
  gcc_checking_assert (TYPE_PRECISION (type) == 1);

  if (operand_equal_p (min, max))
    {
      // Since these are 1-bit quantities, they can only be [MIN,MIN]
      // or [MAX,MAX].
      if (vrp_val_is_min (min))
	min = max = vrp_val_max (type);
      else
	min = max = vrp_val_min (type);
      set (min, max);
    }
  else
    {
      // The only alternative is [MIN,MAX], which is the empty range.
      gcc_checking_assert (vrp_val_is_min (min));
      gcc_checking_assert (vrp_val_is_max (max));
      set_undefined ();
    }
  if (flag_checking)
    verify_range ();
}

void
irange::irange_set_anti_range (tree min, tree max)
{
  gcc_checking_assert (!POLY_INT_CST_P (min));
  gcc_checking_assert (!POLY_INT_CST_P (max));

  if (TYPE_PRECISION (TREE_TYPE (min)) == 1)
    {
      irange_set_1bit_anti_range (min, max);
      return;
    }

  // set an anti-range
  tree type = TREE_TYPE (min);
  signop sign = TYPE_SIGN (type);
  int_range<2> type_range (type);
  // Calculate INVERSE([I,J]) as [-MIN, I-1][J+1, +MAX].
  m_num_ranges = 0;
  wi::overflow_type ovf;

  wide_int w_min = wi::to_wide (min);
  if (wi::ne_p (w_min, type_range.lower_bound ()))
    {
      wide_int lim1 = wi::sub (w_min, 1, sign, &ovf);
      gcc_checking_assert (ovf != wi::OVF_OVERFLOW);
      m_base[0] = type_range.tree_lower_bound (0);
      m_base[1] = wide_int_to_tree (type, lim1);
      m_num_ranges = 1;
    }
  wide_int w_max = wi::to_wide (max);
  if (wi::ne_p (w_max, type_range.upper_bound ()))
    {
      wide_int lim2 = wi::add (w_max, 1, sign, &ovf);
      gcc_checking_assert (ovf != wi::OVF_OVERFLOW);
      m_base[m_num_ranges * 2] = wide_int_to_tree (type, lim2);
      m_base[m_num_ranges * 2 + 1] = type_range.tree_upper_bound (0);
      ++m_num_ranges;
    }

  m_kind = VR_RANGE;
  m_nonzero_mask = NULL;
  normalize_kind ();

  if (flag_checking)
    verify_range ();
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
irange::set (tree min, tree max, value_range_kind kind)
{
  if (kind == VR_UNDEFINED)
    {
      irange::set_undefined ();
      return;
    }

  if (kind == VR_VARYING
      || POLY_INT_CST_P (min)
      || POLY_INT_CST_P (max))
    {
      set_varying (TREE_TYPE (min));
      return;
    }

  if (TREE_OVERFLOW_P (min))
    min = drop_tree_overflow (min);
  if (TREE_OVERFLOW_P (max))
    max = drop_tree_overflow (max);

  if (!legacy_mode_p ())
    {
      if (kind == VR_RANGE)
	irange_set (min, max);
      else
	{
	  gcc_checking_assert (kind == VR_ANTI_RANGE);
	  irange_set_anti_range (min, max);
	}
      return;
    }
  // Nothing to canonicalize for symbolic ranges.
  if (TREE_CODE (min) != INTEGER_CST
      || TREE_CODE (max) != INTEGER_CST)
    {
      m_kind = kind;
      m_base[0] = min;
      m_base[1] = max;
      m_num_ranges = 1;
      m_nonzero_mask = NULL;
      return;
    }

  swap_out_of_order_endpoints (min, max, kind);
  if (kind == VR_VARYING)
    {
      set_varying (TREE_TYPE (min));
      return;
    }

  // Anti-ranges that can be represented as ranges should be so.
  if (kind == VR_ANTI_RANGE)
    {
      bool is_min = vrp_val_is_min (min);
      bool is_max = vrp_val_is_max (max);

      if (is_min && is_max)
	{
	  // Fall through.  This will either be normalized as
	  // VR_UNDEFINED if the anti-range spans the entire
	  // precision, or it will remain an VR_ANTI_RANGE in the case
	  // of an -fstrict-enum where [MIN,MAX] is less than the span
	  // of underlying precision.
	}
      else if (TYPE_PRECISION (TREE_TYPE (min)) == 1)
	{
	  irange_set_1bit_anti_range (min, max);
	  return;
	}
      else if (is_min)
        {
	  tree one = build_int_cst (TREE_TYPE (max), 1);
	  min = int_const_binop (PLUS_EXPR, max, one);
	  max = vrp_val_max (TREE_TYPE (max));
	  kind = VR_RANGE;
        }
      else if (is_max)
        {
	  tree one = build_int_cst (TREE_TYPE (min), 1);
	  max = int_const_binop (MINUS_EXPR, min, one);
	  min = vrp_val_min (TREE_TYPE (min));
	  kind = VR_RANGE;
        }
    }

  m_kind = kind;
  m_base[0] = min;
  m_base[1] = max;
  m_num_ranges = 1;
  m_nonzero_mask = NULL;
  normalize_kind ();
  if (flag_checking)
    verify_range ();
}

// Check the validity of the range.

void
irange::verify_range ()
{
  gcc_checking_assert (m_discriminator == VR_IRANGE);
  if (m_kind == VR_UNDEFINED)
    {
      gcc_checking_assert (m_num_ranges == 0);
      return;
    }
  if (m_kind == VR_VARYING)
    {
      gcc_checking_assert (!m_nonzero_mask
			   || wi::to_wide (m_nonzero_mask) == -1);
      gcc_checking_assert (m_num_ranges == 1);
      gcc_checking_assert (varying_compatible_p ());
      return;
    }
  if (!legacy_mode_p ())
    {
      gcc_checking_assert (m_num_ranges != 0);
      gcc_checking_assert (!varying_compatible_p ());
      for (unsigned i = 0; i < m_num_ranges; ++i)
	{
	  tree lb = tree_lower_bound (i);
	  tree ub = tree_upper_bound (i);
	  int c = compare_values (lb, ub);
	  gcc_checking_assert (c == 0 || c == -1);
	}
      return;
    }
  if (m_kind == VR_RANGE || m_kind == VR_ANTI_RANGE)
    {
      gcc_checking_assert (m_num_ranges == 1);
      int cmp = compare_values (tree_lower_bound (0), tree_upper_bound (0));
      gcc_checking_assert (cmp == 0 || cmp == -1 || cmp == -2);
    }
}

// Return the lower bound for a sub-range.  PAIR is the sub-range in
// question.

wide_int
irange::legacy_lower_bound (unsigned pair) const
{
  gcc_checking_assert (legacy_mode_p ());
  if (symbolic_p ())
    {
      value_range numeric_range (*this);
      numeric_range.normalize_symbolics ();
      return numeric_range.legacy_lower_bound (pair);
    }
  gcc_checking_assert (m_num_ranges > 0);
  gcc_checking_assert (pair + 1 <= num_pairs ());
  if (m_kind == VR_ANTI_RANGE)
    {
      tree typ = type (), t;
      if (pair == 1 || vrp_val_is_min (min ()))
	t = wide_int_to_tree (typ, wi::to_wide (max ()) + 1);
      else
	t = vrp_val_min (typ);
      return wi::to_wide (t);
    }
 return wi::to_wide (tree_lower_bound (pair));
}

// Return the upper bound for a sub-range.  PAIR is the sub-range in
// question.

wide_int
irange::legacy_upper_bound (unsigned pair) const
{
  gcc_checking_assert (legacy_mode_p ());
  if (symbolic_p ())
    {
      value_range numeric_range (*this);
      numeric_range.normalize_symbolics ();
      return numeric_range.legacy_upper_bound (pair);
    }
  gcc_checking_assert (m_num_ranges > 0);
  gcc_checking_assert (pair + 1 <= num_pairs ());
  if (m_kind == VR_ANTI_RANGE)
    {
      tree typ = type (), t;
      if (pair == 1 || vrp_val_is_min (min ()))
	t = vrp_val_max (typ);
      else
	t = wide_int_to_tree (typ, wi::to_wide (min ()) - 1);
      return wi::to_wide (t);
    }
  return wi::to_wide (tree_upper_bound (pair));
}

bool
irange::legacy_equal_p (const irange &other) const
{
  gcc_checking_assert (legacy_mode_p () && other.legacy_mode_p ());

  if (m_kind != other.m_kind)
   return false;
  if (m_kind == VR_UNDEFINED)
    return true;
  if (m_kind == VR_VARYING)
    return range_compatible_p (type (), other.type ());
  return (vrp_operand_equal_p (tree_lower_bound (0),
			       other.tree_lower_bound (0))
	  && vrp_operand_equal_p (tree_upper_bound (0),
				  other.tree_upper_bound (0))
	  && (widest_int::from (get_nonzero_bits (),
				TYPE_SIGN (type ()))
	      == widest_int::from (other.get_nonzero_bits (),
				   TYPE_SIGN (other.type ()))));
}

bool
irange::operator== (const irange &other) const
{
  if (legacy_mode_p ())
    {
      if (other.legacy_mode_p ())
	return legacy_equal_p (other);
      value_range tmp (other);
      return legacy_equal_p (tmp);
    }
  if (other.legacy_mode_p ())
    {
      value_range tmp2 (*this);
      return tmp2.legacy_equal_p (other);
    }

  if (m_num_ranges != other.m_num_ranges)
    return false;

  if (m_num_ranges == 0)
    return true;

  for (unsigned i = 0; i < m_num_ranges; ++i)
    {
      tree lb = tree_lower_bound (i);
      tree ub = tree_upper_bound (i);
      tree lb_other = other.tree_lower_bound (i);
      tree ub_other = other.tree_upper_bound (i);
      if (!operand_equal_p (lb, lb_other, 0)
	  || !operand_equal_p (ub, ub_other, 0))
	return false;
    }
  widest_int nz1 = widest_int::from (get_nonzero_bits (),
				     TYPE_SIGN (type ()));
  widest_int nz2 = widest_int::from (other.get_nonzero_bits (),
				     TYPE_SIGN (other.type ()));
  return nz1 == nz2;
}

/* Return TRUE if this is a symbolic range.  */

bool
irange::symbolic_p () const
{
  return (m_num_ranges > 0
	  && (!is_gimple_min_invariant (min ())
	      || !is_gimple_min_invariant (max ())));
}

/* Return TRUE if this is a constant range.  */

bool
irange::constant_p () const
{
  return (m_num_ranges > 0
	  && TREE_CODE (min ()) == INTEGER_CST
	  && TREE_CODE (max ()) == INTEGER_CST);
}

/* If range is a singleton, place it in RESULT and return TRUE.
   Note: A singleton can be any gimple invariant, not just constants.
   So, [&x, &x] counts as a singleton.  */

bool
irange::singleton_p (tree *result) const
{
  if (!legacy_mode_p ())
    {
      if (num_pairs () == 1 && (wi::to_wide (tree_lower_bound ())
				== wi::to_wide (tree_upper_bound ())))
	{
	  if (result)
	    *result = tree_lower_bound ();
	  return true;
	}
      return false;
    }
  if (m_kind == VR_ANTI_RANGE)
    {
      if (nonzero_p ())
	{
	  if (TYPE_PRECISION (type ()) == 1)
	    {
	      if (result)
		*result = max ();
	      return true;
	    }
	  return false;
	}
      if (num_pairs () == 1)
	{
	  value_range vr0, vr1;
	  ranges_from_anti_range ((const value_range *) this, &vr0, &vr1);
	  return vr0.singleton_p (result);
	}
    }
  // Catches non-numeric extremes as well.
  if (m_kind == VR_RANGE
      && vrp_operand_equal_p (min (), max ())
      && is_gimple_min_invariant (min ()))
    {
      if (result)
        *result = min ();
      return true;
    }
  return false;
}

/* Return 1 if VAL is inside value range.
	  0 if VAL is not inside value range.
	 -2 if we cannot tell either way.

   Benchmark compile/20001226-1.c compilation time after changing this
   function.  */

int
irange::value_inside_range (tree val) const
{
  if (varying_p ())
    return 1;

  if (undefined_p ())
    return 0;

  if (!legacy_mode_p () && TREE_CODE (val) == INTEGER_CST)
    return contains_p (val);

  int cmp1 = operand_less_p (val, min ());
  if (cmp1 == -2)
    return -2;
  if (cmp1 == 1)
    return m_kind != VR_RANGE;

  int cmp2 = operand_less_p (max (), val);
  if (cmp2 == -2)
    return -2;

  if (m_kind == VR_RANGE)
    return !cmp2;
  else
    return !!cmp2;
}

/* Return TRUE if it is possible that range contains VAL.  */

bool
irange::may_contain_p (tree val) const
{
  return value_inside_range (val) != 0;
}

/* Return TRUE if range contains INTEGER_CST.  */
/* Return 1 if VAL is inside value range.
	  0 if VAL is not inside value range.

   Benchmark compile/20001226-1.c compilation time after changing this
   function.  */


bool
irange::contains_p (tree cst) const
{
  if (undefined_p ())
    return false;

  if (legacy_mode_p ())
    {
      gcc_checking_assert (TREE_CODE (cst) == INTEGER_CST);
      if (symbolic_p ())
	{
	  value_range numeric_range (*this);
	  numeric_range.normalize_symbolics ();
	  return numeric_range.contains_p (cst);
	}
      return value_inside_range (cst) == 1;
    }

  gcc_checking_assert (TREE_CODE (cst) == INTEGER_CST);

  // See if we can exclude CST based on the nonzero bits.
  if (m_nonzero_mask)
    {
      wide_int cstw = wi::to_wide (cst);
      if (cstw != 0 && wi::bit_and (wi::to_wide (m_nonzero_mask), cstw) == 0)
	return false;
    }

  signop sign = TYPE_SIGN (TREE_TYPE (cst));
  wide_int v = wi::to_wide (cst);
  for (unsigned r = 0; r < m_num_ranges; ++r)
    {
      if (wi::lt_p (v, lower_bound (r), sign))
	return false;
      if (wi::le_p (v, upper_bound (r), sign))
	return true;
    }

  return false;
}


/* Normalize addresses into constants.  */

void
irange::normalize_addresses ()
{
  if (undefined_p ())
    return;

  if (!POINTER_TYPE_P (type ()) || range_has_numeric_bounds_p (this))
    return;

  if (!range_includes_zero_p (this))
    {
      gcc_checking_assert (TREE_CODE (min ()) == ADDR_EXPR
			   || TREE_CODE (max ()) == ADDR_EXPR);
      set_nonzero (type ());
      return;
    }
  set_varying (type ());
}

/* Normalize symbolics and addresses into constants.  */

void
irange::normalize_symbolics ()
{
  if (varying_p () || undefined_p ())
    return;

  tree ttype = type ();
  bool min_symbolic = !is_gimple_min_invariant (min ());
  bool max_symbolic = !is_gimple_min_invariant (max ());
  if (!min_symbolic && !max_symbolic)
    {
      normalize_addresses ();
      return;
    }

  // [SYM, SYM] -> VARYING
  if (min_symbolic && max_symbolic)
    {
      set_varying (ttype);
      return;
    }
  if (kind () == VR_RANGE)
    {
      // [SYM, NUM] -> [-MIN, NUM]
      if (min_symbolic)
	{
	  set (vrp_val_min (ttype), max ());
	  return;
	}
      // [NUM, SYM] -> [NUM, +MAX]
      set (min (), vrp_val_max (ttype));
      return;
    }
  gcc_checking_assert (kind () == VR_ANTI_RANGE);
  // ~[SYM, NUM] -> [NUM + 1, +MAX]
  if (min_symbolic)
    {
      if (!vrp_val_is_max (max ()))
	{
	  tree n = wide_int_to_tree (ttype, wi::to_wide (max ()) + 1);
	  set (n, vrp_val_max (ttype));
	  return;
	}
      set_varying (ttype);
      return;
    }
  // ~[NUM, SYM] -> [-MIN, NUM - 1]
  if (!vrp_val_is_min (min ()))
    {
      tree n = wide_int_to_tree (ttype, wi::to_wide (min ()) - 1);
      set (vrp_val_min (ttype), n);
      return;
    }
  set_varying (ttype);
}

/* Intersect the two value-ranges { *VR0TYPE, *VR0MIN, *VR0MAX } and
   { VR1TYPE, VR0MIN, VR0MAX } and store the result
   in { *VR0TYPE, *VR0MIN, *VR0MAX }.  This may not be the smallest
   possible such range.  The resulting range is not canonicalized.  */

static void
intersect_ranges (enum value_range_kind *vr0type,
		  tree *vr0min, tree *vr0max,
		  enum value_range_kind vr1type,
		  tree vr1min, tree vr1max)
{
  bool mineq = vrp_operand_equal_p (*vr0min, vr1min);
  bool maxeq = vrp_operand_equal_p (*vr0max, vr1max);

  /* [] is vr0, () is vr1 in the following classification comments.  */
  if (mineq && maxeq)
    {
      /* [(  )] */
      if (*vr0type == vr1type)
	/* Nothing to do for equal ranges.  */
	;
      else if ((*vr0type == VR_RANGE
		&& vr1type == VR_ANTI_RANGE)
	       || (*vr0type == VR_ANTI_RANGE
		   && vr1type == VR_RANGE))
	{
	  /* For anti-range with range intersection the result is empty.  */
	  *vr0type = VR_UNDEFINED;
	  *vr0min = NULL_TREE;
	  *vr0max = NULL_TREE;
	}
      else
	gcc_unreachable ();
    }
  else if (operand_less_p (*vr0max, vr1min) == 1
	   || operand_less_p (vr1max, *vr0min) == 1)
    {
      /* [ ] ( ) or ( ) [ ]
	 If the ranges have an empty intersection, the result of the
	 intersect operation is the range for intersecting an
	 anti-range with a range or empty when intersecting two ranges.  */
      if (*vr0type == VR_RANGE
	  && vr1type == VR_ANTI_RANGE)
	;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  *vr0type = vr1type;
	  *vr0min = vr1min;
	  *vr0max = vr1max;
	}
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_RANGE)
	{
	  *vr0type = VR_UNDEFINED;
	  *vr0min = NULL_TREE;
	  *vr0max = NULL_TREE;
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  /* If the anti-ranges are adjacent to each other merge them.  */
	  if (TREE_CODE (*vr0max) == INTEGER_CST
	      && TREE_CODE (vr1min) == INTEGER_CST
	      && operand_less_p (*vr0max, vr1min) == 1
	      && integer_onep (int_const_binop (MINUS_EXPR,
						vr1min, *vr0max)))
	    *vr0max = vr1max;
	  else if (TREE_CODE (vr1max) == INTEGER_CST
		   && TREE_CODE (*vr0min) == INTEGER_CST
		   && operand_less_p (vr1max, *vr0min) == 1
		   && integer_onep (int_const_binop (MINUS_EXPR,
						     *vr0min, vr1max)))
	    *vr0min = vr1min;
	  /* Else arbitrarily take VR0.  */
	}
    }
  else if ((maxeq || operand_less_p (vr1max, *vr0max) == 1)
	   && (mineq || operand_less_p (*vr0min, vr1min) == 1))
    {
      /* [ (  ) ] or [(  ) ] or [ (  )] */
      if (*vr0type == VR_RANGE
	  && vr1type == VR_RANGE)
	{
	  /* If both are ranges the result is the inner one.  */
	  *vr0type = vr1type;
	  *vr0min = vr1min;
	  *vr0max = vr1max;
	}
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  /* Choose the right gap if the left one is empty.  */
	  if (mineq)
	    {
	      if (TREE_CODE (vr1max) != INTEGER_CST)
		*vr0min = vr1max;
	      else if (TYPE_PRECISION (TREE_TYPE (vr1max)) == 1
		       && !TYPE_UNSIGNED (TREE_TYPE (vr1max)))
		*vr0min
		  = int_const_binop (MINUS_EXPR, vr1max,
				     build_int_cst (TREE_TYPE (vr1max), -1));
	      else
		*vr0min
		  = int_const_binop (PLUS_EXPR, vr1max,
				     build_int_cst (TREE_TYPE (vr1max), 1));
	    }
	  /* Choose the left gap if the right one is empty.  */
	  else if (maxeq)
	    {
	      if (TREE_CODE (vr1min) != INTEGER_CST)
		*vr0max = vr1min;
	      else if (TYPE_PRECISION (TREE_TYPE (vr1min)) == 1
		       && !TYPE_UNSIGNED (TREE_TYPE (vr1min)))
		*vr0max
		  = int_const_binop (PLUS_EXPR, vr1min,
				     build_int_cst (TREE_TYPE (vr1min), -1));
	      else
		*vr0max
		  = int_const_binop (MINUS_EXPR, vr1min,
				     build_int_cst (TREE_TYPE (vr1min), 1));
	    }
	  /* Choose the anti-range if the range is effectively varying.  */
	  else if (vrp_val_is_min (*vr0min)
		   && vrp_val_is_max (*vr0max))
	    {
	      *vr0type = vr1type;
	      *vr0min = vr1min;
	      *vr0max = vr1max;
	    }
	  /* Else choose the range.  */
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_ANTI_RANGE)
	/* If both are anti-ranges the result is the outer one.  */
	;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  /* The intersection is empty.  */
	  *vr0type = VR_UNDEFINED;
	  *vr0min = NULL_TREE;
	  *vr0max = NULL_TREE;
	}
      else
	gcc_unreachable ();
    }
  else if ((maxeq || operand_less_p (*vr0max, vr1max) == 1)
	   && (mineq || operand_less_p (vr1min, *vr0min) == 1))
    {
      /* ( [  ] ) or ([  ] ) or ( [  ]) */
      if (*vr0type == VR_RANGE
	  && vr1type == VR_RANGE)
	/* Choose the inner range.  */
	;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  /* Choose the right gap if the left is empty.  */
	  if (mineq)
	    {
	      *vr0type = VR_RANGE;
	      if (TREE_CODE (*vr0max) != INTEGER_CST)
		*vr0min = *vr0max;
	      else if (TYPE_PRECISION (TREE_TYPE (*vr0max)) == 1
		       && !TYPE_UNSIGNED (TREE_TYPE (*vr0max)))
		*vr0min
		  = int_const_binop (MINUS_EXPR, *vr0max,
				     build_int_cst (TREE_TYPE (*vr0max), -1));
	      else
		*vr0min
		  = int_const_binop (PLUS_EXPR, *vr0max,
				     build_int_cst (TREE_TYPE (*vr0max), 1));
	      *vr0max = vr1max;
	    }
	  /* Choose the left gap if the right is empty.  */
	  else if (maxeq)
	    {
	      *vr0type = VR_RANGE;
	      if (TREE_CODE (*vr0min) != INTEGER_CST)
		*vr0max = *vr0min;
	      else if (TYPE_PRECISION (TREE_TYPE (*vr0min)) == 1
		       && !TYPE_UNSIGNED (TREE_TYPE (*vr0min)))
		*vr0max
		  = int_const_binop (PLUS_EXPR, *vr0min,
				     build_int_cst (TREE_TYPE (*vr0min), -1));
	      else
		*vr0max
		  = int_const_binop (MINUS_EXPR, *vr0min,
				     build_int_cst (TREE_TYPE (*vr0min), 1));
	      *vr0min = vr1min;
	    }
	  /* Choose the anti-range if the range is effectively varying.  */
	  else if (vrp_val_is_min (vr1min)
		   && vrp_val_is_max (vr1max))
	    ;
	  /* Choose the anti-range if it is ~[0,0], that range is special
	     enough to special case when vr1's range is relatively wide.
	     At least for types bigger than int - this covers pointers
	     and arguments to functions like ctz.  */
	  else if (*vr0min == *vr0max
		   && integer_zerop (*vr0min)
		   && ((TYPE_PRECISION (TREE_TYPE (*vr0min))
			>= TYPE_PRECISION (integer_type_node))
		       || POINTER_TYPE_P (TREE_TYPE (*vr0min)))
		   && TREE_CODE (vr1max) == INTEGER_CST
		   && TREE_CODE (vr1min) == INTEGER_CST
		   && (wi::clz (wi::to_wide (vr1max) - wi::to_wide (vr1min))
		       < TYPE_PRECISION (TREE_TYPE (*vr0min)) / 2))
	    ;
	  /* Else choose the range.  */
	  else
	    {
	      *vr0type = vr1type;
	      *vr0min = vr1min;
	      *vr0max = vr1max;
	    }
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  /* If both are anti-ranges the result is the outer one.  */
	  *vr0type = vr1type;
	  *vr0min = vr1min;
	  *vr0max = vr1max;
	}
      else if (vr1type == VR_ANTI_RANGE
	       && *vr0type == VR_RANGE)
	{
	  /* The intersection is empty.  */
	  *vr0type = VR_UNDEFINED;
	  *vr0min = NULL_TREE;
	  *vr0max = NULL_TREE;
	}
      else
	gcc_unreachable ();
    }
  else if ((operand_less_p (vr1min, *vr0max) == 1
	    || operand_equal_p (vr1min, *vr0max, 0))
	   && operand_less_p (*vr0min, vr1min) == 1
	   && operand_less_p (*vr0max, vr1max) == 1)
    {
      /* [  (  ]  ) or [  ](  ) */
      if (*vr0type == VR_ANTI_RANGE
	  && vr1type == VR_ANTI_RANGE)
	*vr0max = vr1max;
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_RANGE)
	*vr0min = vr1min;
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  if (TREE_CODE (vr1min) == INTEGER_CST)
	    *vr0max = int_const_binop (MINUS_EXPR, vr1min,
				       build_int_cst (TREE_TYPE (vr1min), 1));
	  else
	    *vr0max = vr1min;
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  *vr0type = VR_RANGE;
	  if (TREE_CODE (*vr0max) == INTEGER_CST)
	    *vr0min = int_const_binop (PLUS_EXPR, *vr0max,
				       build_int_cst (TREE_TYPE (*vr0max), 1));
	  else
	    *vr0min = *vr0max;
	  *vr0max = vr1max;
	}
      else
	gcc_unreachable ();
    }
  else if ((operand_less_p (*vr0min, vr1max) == 1
	    || operand_equal_p (*vr0min, vr1max, 0))
	   && operand_less_p (vr1min, *vr0min) == 1
	   && operand_less_p (vr1max, *vr0max) == 1)
    {
      /* (  [  )  ] or (  )[  ] */
      if (*vr0type == VR_ANTI_RANGE
	  && vr1type == VR_ANTI_RANGE)
	*vr0min = vr1min;
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_RANGE)
	*vr0max = vr1max;
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  if (TREE_CODE (vr1max) == INTEGER_CST)
	    *vr0min = int_const_binop (PLUS_EXPR, vr1max,
				       build_int_cst (TREE_TYPE (vr1max), 1));
	  else
	    *vr0min = vr1max;
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  *vr0type = VR_RANGE;
	  if (TREE_CODE (*vr0min) == INTEGER_CST)
	    *vr0max = int_const_binop (MINUS_EXPR, *vr0min,
				       build_int_cst (TREE_TYPE (*vr0min), 1));
	  else
	    *vr0max = *vr0min;
	  *vr0min = vr1min;
	}
      else
	gcc_unreachable ();
    }

  /* If we know the intersection is empty, there's no need to
     conservatively add anything else to the set.  */
  if (*vr0type == VR_UNDEFINED)
    return;

  /* As a fallback simply use { *VRTYPE, *VR0MIN, *VR0MAX } as
     result for the intersection.  That's always a conservative
     correct estimate unless VR1 is a constant singleton range
     in which case we choose that.  */
  if (vr1type == VR_RANGE
      && is_gimple_min_invariant (vr1min)
      && vrp_operand_equal_p (vr1min, vr1max))
    {
      *vr0type = vr1type;
      *vr0min = vr1min;
      *vr0max = vr1max;
    }
}

/* Helper for the intersection operation for value ranges.  Given two
   ranges VR0 and VR1, set VR0 to the intersection of both ranges.
   This may not be the smallest possible such range.  */

void
irange::legacy_intersect (irange *vr0, const irange *vr1)
{
  gcc_checking_assert (vr0->legacy_mode_p ());
  gcc_checking_assert (vr1->legacy_mode_p ());
  /* If either range is VR_VARYING the other one wins.  */
  if (vr1->varying_p ())
    return;
  if (vr0->varying_p ())
    {
      vr0->set (vr1->min (), vr1->max (), vr1->kind ());
      return;
    }

  /* When either range is VR_UNDEFINED the resulting range is
     VR_UNDEFINED, too.  */
  if (vr0->undefined_p ())
    return;
  if (vr1->undefined_p ())
    {
      vr0->set_undefined ();
      return;
    }

  value_range_kind vr0kind = vr0->kind ();
  tree vr0min = vr0->min ();
  tree vr0max = vr0->max ();

  intersect_ranges (&vr0kind, &vr0min, &vr0max,
		    vr1->kind (), vr1->min (), vr1->max ());

  /* Make sure to canonicalize the result though as the inversion of a
     VR_RANGE can still be a VR_RANGE.  */
  if (vr0kind == VR_UNDEFINED)
    vr0->set_undefined ();
  else if (vr0kind == VR_VARYING)
    {
      /* If we failed, use the original VR0.  */
      return;
    }
  else
    vr0->set (vr0min, vr0max, vr0kind);
}

/* Union the two value-ranges { *VR0TYPE, *VR0MIN, *VR0MAX } and
   { VR1TYPE, VR0MIN, VR0MAX } and store the result
   in { *VR0TYPE, *VR0MIN, *VR0MAX }.  This may not be the smallest
   possible such range.  The resulting range is not canonicalized.  */

static void
union_ranges (enum value_range_kind *vr0type,
	      tree *vr0min, tree *vr0max,
	      enum value_range_kind vr1type,
	      tree vr1min, tree vr1max)
{
  int cmpmin = compare_values (*vr0min, vr1min);
  int cmpmax = compare_values (*vr0max, vr1max);
  bool mineq = cmpmin == 0;
  bool maxeq = cmpmax == 0;

  /* [] is vr0, () is vr1 in the following classification comments.  */
  if (mineq && maxeq)
    {
      /* [(  )] */
      if (*vr0type == vr1type)
	/* Nothing to do for equal ranges.  */
	;
      else if ((*vr0type == VR_RANGE
		&& vr1type == VR_ANTI_RANGE)
	       || (*vr0type == VR_ANTI_RANGE
		   && vr1type == VR_RANGE))
	{
	  /* For anti-range with range union the result is varying.  */
	  goto give_up;
	}
      else
	gcc_unreachable ();
    }
  else if (operand_less_p (*vr0max, vr1min) == 1
	   || operand_less_p (vr1max, *vr0min) == 1)
    {
      /* [ ] ( ) or ( ) [ ]
	 If the ranges have an empty intersection, result of the union
	 operation is the anti-range or if both are anti-ranges
	 it covers all.  */
      if (*vr0type == VR_ANTI_RANGE
	  && vr1type == VR_ANTI_RANGE)
	goto give_up;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	;
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  *vr0type = vr1type;
	  *vr0min = vr1min;
	  *vr0max = vr1max;
	}
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_RANGE)
	{
	  /* The result is the convex hull of both ranges.  */
	  if (operand_less_p (*vr0max, vr1min) == 1)
	    {
	      /* If the result can be an anti-range, create one.  */
	      if (TREE_CODE (*vr0max) == INTEGER_CST
		  && TREE_CODE (vr1min) == INTEGER_CST
		  && vrp_val_is_min (*vr0min)
		  && vrp_val_is_max (vr1max))
		{
		  tree min = int_const_binop (PLUS_EXPR,
					      *vr0max,
					      build_int_cst (TREE_TYPE (*vr0max), 1));
		  tree max = int_const_binop (MINUS_EXPR,
					      vr1min,
					      build_int_cst (TREE_TYPE (vr1min), 1));
		  if (!operand_less_p (max, min))
		    {
		      *vr0type = VR_ANTI_RANGE;
		      *vr0min = min;
		      *vr0max = max;
		    }
		  else
		    *vr0max = vr1max;
		}
	      else
		*vr0max = vr1max;
	    }
	  else
	    {
	      /* If the result can be an anti-range, create one.  */
	      if (TREE_CODE (vr1max) == INTEGER_CST
		  && TREE_CODE (*vr0min) == INTEGER_CST
		  && vrp_val_is_min (vr1min)
		  && vrp_val_is_max (*vr0max))
		{
		  tree min = int_const_binop (PLUS_EXPR,
					      vr1max,
					      build_int_cst (TREE_TYPE (vr1max), 1));
		  tree max = int_const_binop (MINUS_EXPR,
					      *vr0min,
					      build_int_cst (TREE_TYPE (*vr0min), 1));
		  if (!operand_less_p (max, min))
		    {
		      *vr0type = VR_ANTI_RANGE;
		      *vr0min = min;
		      *vr0max = max;
		    }
		  else
		    *vr0min = vr1min;
		}
	      else
		*vr0min = vr1min;
	    }
	}
      else
	gcc_unreachable ();
    }
  else if ((maxeq || cmpmax == 1)
	   && (mineq || cmpmin == -1))
    {
      /* [ (  ) ] or [(  ) ] or [ (  )] */
      if (*vr0type == VR_RANGE
	  && vr1type == VR_RANGE)
	;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  *vr0type = vr1type;
	  *vr0min = vr1min;
	  *vr0max = vr1max;
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  /* Arbitrarily choose the right or left gap.  */
	  if (!mineq && TREE_CODE (vr1min) == INTEGER_CST)
	    *vr0max = int_const_binop (MINUS_EXPR, vr1min,
				       build_int_cst (TREE_TYPE (vr1min), 1));
	  else if (!maxeq && TREE_CODE (vr1max) == INTEGER_CST)
	    *vr0min = int_const_binop (PLUS_EXPR, vr1max,
				       build_int_cst (TREE_TYPE (vr1max), 1));
	  else
	    goto give_up;
	}
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	/* The result covers everything.  */
	goto give_up;
      else
	gcc_unreachable ();
    }
  else if ((maxeq || cmpmax == -1)
	   && (mineq || cmpmin == 1))
    {
      /* ( [  ] ) or ([  ] ) or ( [  ]) */
      if (*vr0type == VR_RANGE
	  && vr1type == VR_RANGE)
	{
	  *vr0type = vr1type;
	  *vr0min = vr1min;
	  *vr0max = vr1max;
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_ANTI_RANGE)
	;
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  *vr0type = VR_ANTI_RANGE;
	  if (!mineq && TREE_CODE (*vr0min) == INTEGER_CST)
	    {
	      *vr0max = int_const_binop (MINUS_EXPR, *vr0min,
					 build_int_cst (TREE_TYPE (*vr0min), 1));
	      *vr0min = vr1min;
	    }
	  else if (!maxeq && TREE_CODE (*vr0max) == INTEGER_CST)
	    {
	      *vr0min = int_const_binop (PLUS_EXPR, *vr0max,
					 build_int_cst (TREE_TYPE (*vr0max), 1));
	      *vr0max = vr1max;
	    }
	  else
	    goto give_up;
	}
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	/* The result covers everything.  */
	goto give_up;
      else
	gcc_unreachable ();
    }
  else if (cmpmin == -1
	   && cmpmax == -1
	   && (operand_less_p (vr1min, *vr0max) == 1
	       || operand_equal_p (vr1min, *vr0max, 0)))
    {
      /* [  (  ]  ) or [   ](   ) */
      if (*vr0type == VR_RANGE
	  && vr1type == VR_RANGE)
	*vr0max = vr1max;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_ANTI_RANGE)
	*vr0min = vr1min;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  if (TREE_CODE (vr1min) == INTEGER_CST)
	    *vr0max = int_const_binop (MINUS_EXPR, vr1min,
				       build_int_cst (TREE_TYPE (vr1min), 1));
	  else
	    goto give_up;
	}
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  if (TREE_CODE (*vr0max) == INTEGER_CST)
	    {
	      *vr0type = vr1type;
	      *vr0min = int_const_binop (PLUS_EXPR, *vr0max,
					 build_int_cst (TREE_TYPE (*vr0max), 1));
	      *vr0max = vr1max;
	    }
	  else
	    goto give_up;
	}
      else
	gcc_unreachable ();
    }
  else if (cmpmin == 1
	   && cmpmax == 1
	   && (operand_less_p (*vr0min, vr1max) == 1
	       || operand_equal_p (*vr0min, vr1max, 0)))
    {
      /* (  [  )  ] or (   )[   ] */
      if (*vr0type == VR_RANGE
	  && vr1type == VR_RANGE)
	*vr0min = vr1min;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_ANTI_RANGE)
	*vr0max = vr1max;
      else if (*vr0type == VR_ANTI_RANGE
	       && vr1type == VR_RANGE)
	{
	  if (TREE_CODE (vr1max) == INTEGER_CST)
	    *vr0min = int_const_binop (PLUS_EXPR, vr1max,
				       build_int_cst (TREE_TYPE (vr1max), 1));
	  else
	    goto give_up;
	}
      else if (*vr0type == VR_RANGE
	       && vr1type == VR_ANTI_RANGE)
	{
	  if (TREE_CODE (*vr0min) == INTEGER_CST)
	    {
	      *vr0type = vr1type;
	      *vr0max = int_const_binop (MINUS_EXPR, *vr0min,
					 build_int_cst (TREE_TYPE (*vr0min), 1));
	      *vr0min = vr1min;
	    }
	  else
	    goto give_up;
	}
      else
	gcc_unreachable ();
    }
  else
    goto give_up;

  return;

give_up:
  *vr0type = VR_VARYING;
  *vr0min = NULL_TREE;
  *vr0max = NULL_TREE;
}

/* Helper for meet operation for value ranges.  Given two ranges VR0
   and VR1, set VR0 to the union of both ranges.  This may not be the
   smallest possible such range.  */

void
irange::legacy_union (irange *vr0, const irange *vr1)
{
  gcc_checking_assert (vr0->legacy_mode_p ());
  gcc_checking_assert (vr1->legacy_mode_p ());

  /* VR0 has the resulting range if VR1 is undefined or VR0 is varying.  */
  if (vr1->undefined_p ()
      || vr0->varying_p ())
    return;

  /* VR1 has the resulting range if VR0 is undefined or VR1 is varying.  */
  if (vr0->undefined_p ())
    {
      vr0->set (vr1->min (), vr1->max (), vr1->kind ());
      return;
    }

  if (vr1->varying_p ())
    {
      vr0->set_varying (vr1->type ());
      return;
    }

  value_range_kind vr0kind = vr0->kind ();
  tree vr0min = vr0->min ();
  tree vr0max = vr0->max ();

  union_ranges (&vr0kind, &vr0min, &vr0max,
		vr1->kind (), vr1->min (), vr1->max ());

  if (vr0kind == VR_UNDEFINED)
    vr0->set_undefined ();
  else if (vr0kind == VR_VARYING)
    {
      /* Failed to find an efficient meet.  Before giving up and
	 setting the result to VARYING, see if we can at least derive
	 a non-zero range.  */
      if (range_includes_zero_p (vr0) == 0
	  && range_includes_zero_p (vr1) == 0)
	vr0->set_nonzero (vr0->type ());
      else
	vr0->set_varying (vr0->type ());
    }
  else
    vr0->set (vr0min, vr0max, vr0kind);
}

/* Meet operation for value ranges.  Given two value ranges VR0 and
   VR1, store in VR0 a range that contains both VR0 and VR1.  This
   may not be the smallest possible such range.
   Return TRUE if the original value changes.  */

bool
irange::legacy_verbose_union_ (const irange *other)
{
  if (legacy_mode_p ())
    {
      if (!other->legacy_mode_p ())
	{
	  int_range<1> tmp = *other;
	  legacy_union (this, &tmp);
	  return true;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Meeting\n  ");
	  dump_value_range (dump_file, this);
	  fprintf (dump_file, "\nand\n  ");
	  dump_value_range (dump_file, other);
	  fprintf (dump_file, "\n");
	}

      legacy_union (this, other);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "to\n  ");
	  dump_value_range (dump_file, this);
	  fprintf (dump_file, "\n");
	}
      return true;
    }

  if (other->legacy_mode_p ())
    {
      int_range<2> wider = *other;
      return irange_union (wider);
    }
  else
    return irange_union (*other);
}

bool
irange::legacy_verbose_intersect (const irange *other)
{
  if (legacy_mode_p ())
    {
      if (!other->legacy_mode_p ())
	{
	  int_range<1> tmp = *other;
	  legacy_intersect (this, &tmp);
	  return true;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Intersecting\n  ");
	  dump_value_range (dump_file, this);
	  fprintf (dump_file, "\nand\n  ");
	  dump_value_range (dump_file, other);
	  fprintf (dump_file, "\n");
	}

      legacy_intersect (this, other);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "to\n  ");
	  dump_value_range (dump_file, this);
	  fprintf (dump_file, "\n");
	}
      return true;
    }

  if (other->legacy_mode_p ())
    {
      int_range<2> wider;
      wider = *other;
      return irange_intersect (wider);
    }
  else
    return irange_intersect (*other);
}

// Perform an efficient union with R when both ranges have only a single pair.
// Excluded are VARYING and UNDEFINED ranges.

bool
irange::irange_single_pair_union (const irange &r)
{
  gcc_checking_assert (!undefined_p () && !varying_p ());
  gcc_checking_assert (!r.undefined_p () && !varying_p ());

  signop sign = TYPE_SIGN (TREE_TYPE (m_base[0]));
  // Check if current lower bound is also the new lower bound.
  if (wi::le_p (wi::to_wide (m_base[0]), wi::to_wide (r.m_base[0]), sign))
    {
      // If current upper bound is new upper bound, we're done.
      if (wi::le_p (wi::to_wide (r.m_base[1]), wi::to_wide (m_base[1]), sign))
	return union_nonzero_bits (r);
      // Otherwise R has the new upper bound.
      // Check for overlap/touching ranges, or single target range.
      if (m_max_ranges == 1
	  || wi::to_widest (m_base[1]) + 1 >= wi::to_widest (r.m_base[0]))
	m_base[1] = r.m_base[1];
      else
	{
	  // This is a dual range result.
	  m_base[2] = r.m_base[0];
	  m_base[3] = r.m_base[1];
	  m_num_ranges = 2;
	}
      union_nonzero_bits (r);
      return true;
    }

  // Set the new lower bound to R's lower bound.
  tree lb = m_base[0];
  m_base[0] = r.m_base[0];

  // If R fully contains THIS range, just set the upper bound.
  if (wi::ge_p (wi::to_wide (r.m_base[1]), wi::to_wide (m_base[1]), sign))
    m_base[1] = r.m_base[1];
  // Check for overlapping ranges, or target limited to a single range.
  else if (m_max_ranges == 1
	   || wi::to_widest (r.m_base[1]) + 1 >= wi::to_widest (lb))
    ;
  else
    {
      // Left with 2 pairs.
      m_num_ranges = 2;
      m_base[2] = lb;
      m_base[3] = m_base[1];
      m_base[1] = r.m_base[1];
    }
  union_nonzero_bits (r);
  return true;
}

// union_ for multi-ranges.

bool
irange::irange_union (const irange &r)
{
  gcc_checking_assert (!legacy_mode_p () && !r.legacy_mode_p ());

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

  // If this ranges fully contains R, then we need do nothing.
  if (irange_contains_p (r))
    return union_nonzero_bits (r);

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
  auto_vec<tree, 20> res (m_num_ranges * 2 + r.m_num_ranges * 2);
  unsigned i = 0, j = 0, k = 0;

  while (i < m_num_ranges * 2 && j < r.m_num_ranges * 2)
    {
      // lower of Xi and Xj is the lowest point.
      if (wi::to_widest (m_base[i]) <= wi::to_widest (r.m_base[j]))
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
      if (wi::to_widest (res[i - 1]) + 1 >= wi::to_widest (res[j]))
	{
	  // New upper bounds is greater of current or the next one.
	  if (wi::to_widest (res[j + 1]) > wi::to_widest (res[i - 1]))
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
  if (i > m_max_ranges * 2)
    {
      res[m_max_ranges * 2 - 1] = res[i - 1];
      i = m_max_ranges * 2;
    }

  for (j = 0; j < i ; j++)
    m_base[j] = res [j];
  m_num_ranges = i / 2;

  m_kind = VR_RANGE;
  union_nonzero_bits (r);
  return true;
}

// Return TRUE if THIS fully contains R.  No undefined or varying cases.

bool
irange::irange_contains_p (const irange &r) const
{
  gcc_checking_assert (!undefined_p () && !varying_p ());
  gcc_checking_assert (!r.undefined_p () && !varying_p ());

  // In order for THIS to fully contain R, all of the pairs within R must
  // be fully contained by the pairs in this object.
  signop sign = TYPE_SIGN (TREE_TYPE(m_base[0]));
  unsigned ri = 0;
  unsigned i = 0;
  tree rl = r.m_base[0];
  tree ru = r.m_base[1];
  tree l = m_base[0];
  tree u = m_base[1];
  while (1)
    {
      // If r is contained within this range, move to the next R
      if (wi::ge_p (wi::to_wide (rl), wi::to_wide (l), sign)
	  && wi::le_p (wi::to_wide (ru), wi::to_wide (u), sign))
	{
	  // This pair is OK, Either done, or bump to the next.
	  if (++ri >= r.num_pairs ())
	    return true;
	  rl = r.m_base[ri * 2];
	  ru = r.m_base[ri * 2 + 1];
	  continue;
	}
      // Otherwise, check if this's pair occurs before R's.
      if (wi::lt_p (wi::to_wide (u), wi::to_wide (rl), sign))
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


// Intersect for multi-ranges.  Return TRUE if anything changes.

bool
irange::irange_intersect (const irange &r)
{
  gcc_checking_assert (!legacy_mode_p () && !r.legacy_mode_p ());
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

      res |= intersect_nonzero_bits (r);
      return res;
    }

  // If R fully contains this, then intersection will change nothing.
  if (r.irange_contains_p (*this))
    return intersect_nonzero_bits (r);

  signop sign = TYPE_SIGN (TREE_TYPE(m_base[0]));
  unsigned bld_pair = 0;
  unsigned bld_lim = m_max_ranges;
  int_range_max r2 (*this);
  unsigned r2_lim = r2.num_pairs ();
  unsigned i2 = 0;
  for (unsigned i = 0; i < r.num_pairs (); )
    {
      // If r1's upper is < r2's lower, we can skip r1's pair.
      tree ru = r.m_base[i * 2 + 1];
      tree r2l = r2.m_base[i2 * 2];
      if (wi::lt_p (wi::to_wide (ru), wi::to_wide (r2l), sign))
	{
	  i++;
	  continue;
	}
      // Likewise, skip r2's pair if its excluded.
      tree r2u = r2.m_base[i2 * 2 + 1];
      tree rl = r.m_base[i * 2];
      if (wi::lt_p (wi::to_wide (r2u), wi::to_wide (rl), sign))
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
	  if (wi::ge_p (wi::to_wide (rl), wi::to_wide (r2l), sign))
	    m_base[bld_pair * 2] = rl;
	  else
	    m_base[bld_pair * 2] = r2l;
	}
      else
	// Decrease and set a new upper.
	bld_pair--;

      // ...and choose the lower of the upper bounds.
      if (wi::le_p (wi::to_wide (ru), wi::to_wide (r2u), sign))
	{
	  m_base[bld_pair * 2 + 1] = ru;
	  bld_pair++;
	  // Move past the r1 pair and keep trying.
	  i++;
	  continue;
	}
      else
	{
	  m_base[bld_pair * 2 + 1] = r2u;
	  bld_pair++;
	  i2++;
	  if (i2 < r2_lim)
	    continue;
	  // No more r2, break.
	  break;
	}
      // r2 has the higher lower bound.
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
  intersect_nonzero_bits (r);
  return true;
}


// Multirange intersect for a specified wide_int [lb, ub] range.
// Return TRUE if intersect changed anything.
//
// NOTE: It is the caller's responsibility to intersect the nonzero masks.

bool
irange::intersect (const wide_int& lb, const wide_int& ub)
{
  // Undefined remains undefined.
  if (undefined_p ())
    return false;

  if (legacy_mode_p ())
    {
      intersect (int_range<1> (type (), lb, ub));
      return true;
    }

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
      tree pairl = m_base[i * 2];
      tree pairu = m_base[i * 2 + 1];
      // Once UB is less than a pairs lower bound, we're done.
      if (wi::lt_p (ub, wi::to_wide (pairl), sign))
	break;
      // if LB is greater than this pairs upper, this pair is excluded.
      if (wi::lt_p (wi::to_wide (pairu), lb, sign))
	continue;

      // Must be some overlap.  Find the highest of the lower bounds,
      // and set it
      if (wi::gt_p (lb, wi::to_wide (pairl), sign))
	m_base[bld_index * 2] = wide_int_to_tree (range_type, lb);
      else
	m_base[bld_index * 2] = pairl;

      // ...and choose the lower of the upper bounds and if the base pair
      // has the lower upper bound, need to check next pair too.
      if (wi::lt_p (ub, wi::to_wide (pairu), sign))
	{
	  m_base[bld_index++ * 2 + 1] = wide_int_to_tree (range_type, ub);
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
  // No need to call normalize_kind(), as the caller will do this
  // while intersecting the nonzero mask.
  if (flag_checking)
    verify_range ();
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

// Return the inverse of a range.

void
irange::invert ()
{
  if (legacy_mode_p ())
    {
      // We can't just invert VR_RANGE and VR_ANTI_RANGE because we may
      // create non-canonical ranges.  Use the constructors instead.
      if (m_kind == VR_RANGE)
	*this = value_range (min (), max (), VR_ANTI_RANGE);
      else if (m_kind == VR_ANTI_RANGE)
	*this = value_range (min (), max ());
      else
	gcc_unreachable ();
      return;
    }

  gcc_checking_assert (!undefined_p () && !varying_p ());

  // We always need one more set of bounds to represent an inverse, so
  // if we're at the limit, we can't properly represent things.
  //
  // For instance, to represent the inverse of a 2 sub-range set
  // [5, 10][20, 30], we would need a 3 sub-range set
  // [-MIN, 4][11, 19][31, MAX].
  //
  // In this case, return the most conservative thing.
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
  m_nonzero_mask = NULL;
  if (m_num_ranges == m_max_ranges
      && lower_bound () != type_min
      && upper_bound () != type_max)
    {
      m_base[1] = wide_int_to_tree (ttype, type_max);
      m_num_ranges = 1;
      return;
    }
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
      m_base[nitems++] = wide_int_to_tree (ttype, type_min);
      tmp = subtract_one (orig_range.lower_bound (), ttype, ovf);
      m_base[nitems++] = wide_int_to_tree (ttype, tmp);
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
	  tmp = wi::add (wi::to_wide (orig_range.m_base[j]), 1, sign, &ovf);
	  m_base[nitems++] = wide_int_to_tree (ttype, tmp);
	  tmp = subtract_one (wi::to_wide (orig_range.m_base[j + 1]),
			      ttype, ovf);
	  m_base[nitems++] = wide_int_to_tree (ttype, tmp);
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
  if (type_max != wi::to_wide (orig_range.m_base[i]))
    {
      tmp = add_one (wi::to_wide (orig_range.m_base[i]), ttype, ovf);
      m_base[nitems++] = wide_int_to_tree (ttype, tmp);
      m_base[nitems++] = wide_int_to_tree (ttype, type_max);
      if (ovf)
	nitems -= 2;
    }
  m_num_ranges = nitems / 2;

  // We disallow undefined or varying coming in, so the result can
  // only be a VR_RANGE.
  gcc_checking_assert (m_kind == VR_RANGE);

  if (flag_checking)
    verify_range ();
}

// Return the nonzero bits inherent in the range.

wide_int
irange::get_nonzero_bits_from_range () const
{
  // For legacy symbolics.
  if (!constant_p ())
    return wi::shwi (-1, TYPE_PRECISION (type ()));

  wide_int min = lower_bound ();
  wide_int max = upper_bound ();
  wide_int xorv = min ^ max;
  if (xorv != 0)
    {
      unsigned prec = TYPE_PRECISION (type ());
      xorv = wi::mask (prec - wi::clz (xorv), false, prec);
    }
  return min | xorv;
}

// If the the nonzero mask can be trivially converted to a range, do
// so and return TRUE.

bool
irange::set_range_from_nonzero_bits ()
{
  gcc_checking_assert (!undefined_p ());
  if (!m_nonzero_mask)
    return false;
  unsigned popcount = wi::popcount (wi::to_wide (m_nonzero_mask));

  // If we have only one bit set in the mask, we can figure out the
  // range immediately.
  if (popcount == 1)
    {
      // Make sure we don't pessimize the range.
      if (!contains_p (m_nonzero_mask))
	return false;

      bool has_zero = contains_p (build_zero_cst (type ()));
      tree nz = m_nonzero_mask;
      set (nz, nz);
      m_nonzero_mask = nz;
      if (has_zero)
	{
	  int_range<2> zero;
	  zero.set_zero (type ());
	  union_ (zero);
	}
      return true;
    }
  else if (popcount == 0)
    {
      set_zero (type ());
      return true;
    }
  return false;
}

void
irange::set_nonzero_bits (const wide_int_ref &bits)
{
  gcc_checking_assert (!undefined_p ());
  unsigned prec = TYPE_PRECISION (type ());

  if (bits == -1)
    {
      m_nonzero_mask = NULL;
      normalize_kind ();
      if (flag_checking)
	verify_range ();
      return;
    }

  // Drop VARYINGs with a nonzero mask to a plain range.
  if (m_kind == VR_VARYING && bits != -1)
    m_kind = VR_RANGE;

  wide_int nz = wide_int::from (bits, prec, TYPE_SIGN (type ()));
  m_nonzero_mask = wide_int_to_tree (type (), nz);
  if (set_range_from_nonzero_bits ())
    return;

  normalize_kind ();
  if (flag_checking)
    verify_range ();
}

// Return the nonzero bitmask.  This will return the nonzero bits plus
// the nonzero bits inherent in the range.

wide_int
irange::get_nonzero_bits () const
{
  gcc_checking_assert (!undefined_p ());
  // The nonzero mask inherent in the range is calculated on-demand.
  // For example, [0,255] does not have a 0xff nonzero mask by default
  // (unless manually set).  This saves us considerable time, because
  // setting it at creation incurs a large penalty for irange::set.
  // At the time of writing there was a 5% slowdown in VRP if we kept
  // the mask precisely up to date at all times.  Instead, we default
  // to -1 and set it when explicitly requested.  However, this
  // function will always return the correct mask.
  if (m_nonzero_mask)
    return wi::to_wide (m_nonzero_mask) & get_nonzero_bits_from_range ();
  else
    return get_nonzero_bits_from_range ();
}

// Convert tree mask to wide_int.  Returns -1 for NULL masks.

inline wide_int
mask_to_wi (tree mask, tree type)
{
  if (mask)
    return wi::to_wide (mask);
  else
    return wi::shwi (-1, TYPE_PRECISION (type));
}

// Intersect the nonzero bits in R into THIS and normalize the range.
// Return TRUE if the intersection changed anything.

bool
irange::intersect_nonzero_bits (const irange &r)
{
  gcc_checking_assert (!undefined_p () && !r.undefined_p ());

  if (!m_nonzero_mask && !r.m_nonzero_mask)
    {
      normalize_kind ();
      if (flag_checking)
	verify_range ();
      return false;
    }

  bool changed = false;
  tree t = type ();
  if (mask_to_wi (m_nonzero_mask, t) != mask_to_wi (r.m_nonzero_mask, t))
    {
      wide_int nz = get_nonzero_bits () & r.get_nonzero_bits ();
      // If the nonzero bits did not change, return false.
      if (nz == get_nonzero_bits ())
	return false;

      m_nonzero_mask = wide_int_to_tree (t, nz);
      if (set_range_from_nonzero_bits ())
	return true;
      changed = true;
    }
  normalize_kind ();
  if (flag_checking)
    verify_range ();
  return changed;
}

// Union the nonzero bits in R into THIS and normalize the range.
// Return TRUE if the union changed anything.

bool
irange::union_nonzero_bits (const irange &r)
{
  gcc_checking_assert (!undefined_p () && !r.undefined_p ());

  if (!m_nonzero_mask && !r.m_nonzero_mask)
    {
      normalize_kind ();
      if (flag_checking)
	verify_range ();
      return false;
    }

  bool changed = false;
  tree t = type ();
  if (mask_to_wi (m_nonzero_mask, t) != mask_to_wi (r.m_nonzero_mask, t))
    {
      wide_int nz = get_nonzero_bits () | r.get_nonzero_bits ();
      m_nonzero_mask = wide_int_to_tree (t, nz);
      // No need to call set_range_from_nonzero_bits, because we'll
      // never narrow the range.  Besides, it would cause endless
      // recursion because of the union_ in
      // set_range_from_nonzero_bits.
      changed = true;
    }
  normalize_kind ();
  if (flag_checking)
    verify_range ();
  return changed;
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

DEBUG_FUNCTION void
debug (const value_range *vr)
{
  dump_value_range (stderr, vr);
  fprintf (stderr, "\n");
}

DEBUG_FUNCTION void
debug (const value_range &vr)
{
  dump_value_range (stderr, &vr);
  fprintf (stderr, "\n");
}

/* Create two value-ranges in *VR0 and *VR1 from the anti-range *AR
   so that *VR0 U *VR1 == *AR.  Returns true if that is possible,
   false otherwise.  If *AR can be represented with a single range
   *VR1 will be VR_UNDEFINED.  */

bool
ranges_from_anti_range (const value_range *ar,
			value_range *vr0, value_range *vr1)
{
  tree type = ar->type ();

  vr0->set_undefined ();
  vr1->set_undefined ();

  /* As a future improvement, we could handle ~[0, A] as: [-INF, -1] U
     [A+1, +INF].  Not sure if this helps in practice, though.  */

  if (ar->kind () != VR_ANTI_RANGE
      || TREE_CODE (ar->min ()) != INTEGER_CST
      || TREE_CODE (ar->max ()) != INTEGER_CST
      || !vrp_val_min (type)
      || !vrp_val_max (type))
    return false;

  if (tree_int_cst_lt (vrp_val_min (type), ar->min ()))
    vr0->set (vrp_val_min (type),
	      wide_int_to_tree (type, wi::to_wide (ar->min ()) - 1));
  if (tree_int_cst_lt (ar->max (), vrp_val_max (type)))
    vr1->set (wide_int_to_tree (type, wi::to_wide (ar->max ()) + 1),
	      vrp_val_max (type));
  if (vr0->undefined_p ())
    {
      *vr0 = *vr1;
      vr1->set_undefined ();
    }

  return !vr0->undefined_p ();
}

bool
range_has_numeric_bounds_p (const irange *vr)
{
  return (!vr->undefined_p ()
	  && TREE_CODE (vr->min ()) == INTEGER_CST
	  && TREE_CODE (vr->max ()) == INTEGER_CST);
}

/* Return whether VAL is equal to the maximum value of its type.
   We can't do a simple equality comparison with TYPE_MAX_VALUE because
   C typedefs and Ada subtypes can produce types whose TYPE_MAX_VALUE
   is not == to the integer constant with the same value in the type.  */

bool
vrp_val_is_max (const_tree val)
{
  tree type_max = vrp_val_max (TREE_TYPE (val));
  return (val == type_max
	  || (type_max != NULL_TREE
	      && operand_equal_p (val, type_max, 0)));
}

/* Return whether VAL is equal to the minimum value of its type.  */

bool
vrp_val_is_min (const_tree val)
{
  tree type_min = vrp_val_min (TREE_TYPE (val));
  return (val == type_min
	  || (type_min != NULL_TREE
	      && operand_equal_p (val, type_min, 0)));
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

// ?? These stubs are for ipa-prop.cc which use a value_range in a
// hash_traits.  hash-traits.h defines an extern of gt_ggc_mx (T &)
// instead of picking up the gt_ggc_mx (T *) version.
void
gt_pch_nx (int_range<1> *&x)
{
  return gt_pch_nx ((irange *) x);
}

void
gt_ggc_mx (int_range<1> *&x)
{
  return gt_ggc_mx ((irange *) x);
}

#define DEFINE_INT_RANGE_INSTANCE(N)					\
  template int_range<N>::int_range(tree, tree, value_range_kind);	\
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

namespace selftest
{
#define INT(N) build_int_cst (integer_type_node, (N))
#define UINT(N) build_int_cstu (unsigned_type_node, (N))
#define UINT128(N) build_int_cstu (u128_type, (N))
#define UCHAR(N) build_int_cstu (unsigned_char_type_node, (N))
#define SCHAR(N) build_int_cst (signed_char_type_node, (N))

static int_range<3>
build_range3 (int a, int b, int c, int d, int e, int f)
{
  int_range<3> i1 (INT (a), INT (b));
  int_range<3> i2 (INT (c), INT (d));
  int_range<3> i3 (INT (e), INT (f));
  i1.union_ (i2);
  i1.union_ (i3);
  return i1;
}

static void
range_tests_irange3 ()
{
  typedef int_range<3> int_range3;
  int_range3 r0, r1, r2;
  int_range3 i1, i2, i3;

  // ([10,20] U [5,8]) U [1,3] ==> [1,3][5,8][10,20].
  r0 = int_range3 (INT (10), INT (20));
  r1 = int_range3 (INT (5), INT (8));
  r0.union_ (r1);
  r1 = int_range3 (INT (1), INT (3));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == build_range3 (1, 3, 5, 8, 10, 20));

  // [1,3][5,8][10,20] U [-5,0] => [-5,3][5,8][10,20].
  r1 = int_range3 (INT (-5), INT (0));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == build_range3 (-5, 3, 5, 8, 10, 20));

  // [10,20][30,40] U [50,60] ==> [10,20][30,40][50,60].
  r1 = int_range3 (INT (50), INT (60));
  r0 = int_range3 (INT (10), INT (20));
  r0.union_ (int_range3 (INT (30), INT (40)));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == build_range3 (10, 20, 30, 40, 50, 60));
  // [10,20][30,40][50,60] U [70, 80] ==> [10,20][30,40][50,60][70,80].
  r1 = int_range3 (INT (70), INT (80));
  r0.union_ (r1);

  r2 = build_range3 (10, 20, 30, 40, 50, 60);
  r2.union_ (int_range3 (INT (70), INT (80)));
  ASSERT_TRUE (r0 == r2);

  // [10,20][30,40][50,60] U [6,35] => [6,40][50,60].
  r0 = build_range3 (10, 20, 30, 40, 50, 60);
  r1 = int_range3 (INT (6), INT (35));
  r0.union_ (r1);
  r1 = int_range3 (INT (6), INT (40));
  r1.union_ (int_range3 (INT (50), INT (60)));
  ASSERT_TRUE (r0 == r1);

  // [10,20][30,40][50,60] U [6,60] => [6,60].
  r0 = build_range3 (10, 20, 30, 40, 50, 60);
  r1 = int_range3 (INT (6), INT (60));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == int_range3 (INT (6), INT (60)));

  // [10,20][30,40][50,60] U [6,70] => [6,70].
  r0 = build_range3 (10, 20, 30, 40, 50, 60);
  r1 = int_range3 (INT (6), INT (70));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == int_range3 (INT (6), INT (70)));

  // [10,20][30,40][50,60] U [35,70] => [10,20][30,70].
  r0 = build_range3 (10, 20, 30, 40, 50, 60);
  r1 = int_range3 (INT (35), INT (70));
  r0.union_ (r1);
  r1 = int_range3 (INT (10), INT (20));
  r1.union_ (int_range3 (INT (30), INT (70)));
  ASSERT_TRUE (r0 == r1);

  // [10,20][30,40][50,60] U [15,35] => [10,40][50,60].
  r0 = build_range3 (10, 20, 30, 40, 50, 60);
  r1 = int_range3 (INT (15), INT (35));
  r0.union_ (r1);
  r1 = int_range3 (INT (10), INT (40));
  r1.union_ (int_range3 (INT (50), INT (60)));
  ASSERT_TRUE (r0 == r1);

  // [10,20][30,40][50,60] U [35,35] => [10,20][30,40][50,60].
  r0 = build_range3 (10, 20, 30, 40, 50, 60);
  r1 = int_range3 (INT (35), INT (35));
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
      int_range<1> tmp (INT (nrange*10), INT (nrange*10 + 5));
      big.union_ (tmp);
    }
  ASSERT_TRUE (big.num_pairs () == nrange);

  // Verify that we can copy it without loosing precision.
  int_range_max copy (big);
  ASSERT_TRUE (copy.num_pairs () == nrange);

  // Inverting it should produce one more sub-range.
  big.invert ();
  ASSERT_TRUE (big.num_pairs () == nrange + 1);

  int_range<1> tmp (INT (5), INT (37));
  big.intersect (tmp);
  ASSERT_TRUE (big.num_pairs () == 4);

  // Test that [10,10][20,20] does NOT contain 15.
  {
    int_range_max i1 (build_int_cst (integer_type_node, 10),
		      build_int_cst (integer_type_node, 10));
    int_range_max i2 (build_int_cst (integer_type_node, 20),
		      build_int_cst (integer_type_node, 20));
    i1.union_ (i2);
    ASSERT_FALSE (i1.contains_p (build_int_cst (integer_type_node, 15)));
  }
}

static void
range_tests_legacy ()
{
  // Test truncating copy to int_range<1>.
  int_range<3> big = build_range3 (10, 20, 30, 40, 50, 60);
  int_range<1> small = big;
  ASSERT_TRUE (small == int_range<1> (INT (10), INT (60)));

  // Test truncating copy to int_range<2>.
  int_range<2> medium = big;
  ASSERT_TRUE (!medium.undefined_p ());

  // Test that a truncating copy of [MIN,20][22,40][80,MAX]
  // ends up as a conservative anti-range of ~[21,21].
  big = int_range<3> (vrp_val_min (integer_type_node), INT (20));
  big.union_ (int_range<1> (INT (22), INT (40)));
  big.union_ (int_range<1> (INT (80), vrp_val_max (integer_type_node)));
  small = big;
  ASSERT_TRUE (small == int_range<1> (INT (21), INT (21), VR_ANTI_RANGE));

  // Copying a legacy symbolic to an int_range should normalize the
  // symbolic at copy time.
  {
    tree ssa = make_ssa_name (integer_type_node);
    value_range legacy_range (ssa, INT (25));
    int_range<2> copy = legacy_range;
    ASSERT_TRUE (copy == int_range<2>  (vrp_val_min (integer_type_node),
					INT (25)));

    // Test that copying ~[abc_23, abc_23] to a multi-range yields varying.
    legacy_range = value_range (ssa, ssa, VR_ANTI_RANGE);
    copy = legacy_range;
    ASSERT_TRUE (copy.varying_p ());
  }

  // VARYING of different sizes should not be equal.
  tree big_type = build_nonstandard_integer_type (32, 1);
  tree small_type = build_nonstandard_integer_type (16, 1);
  int_range_max r0 (big_type);
  int_range_max r1 (small_type);
  ASSERT_TRUE (r0 != r1);
  value_range vr0 (big_type);
  int_range_max vr1 (small_type);
  ASSERT_TRUE (vr0 != vr1);
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
  int_range<1> vr1 (build_int_cstu (rtype, 0), build_int_cstu (rtype, 1));
  int_range<1> vr2 (build_int_cstu (rtype, 2), build_int_cstu (rtype, 3));
  vr1.union_ (vr2);
  ASSERT_TRUE (vr1 == int_range<1> (build_int_cstu (rtype, 0),
				    build_int_cstu (rtype, 3)));
  ASSERT_FALSE (vr1.varying_p ());

  // Test that copying to a multi-range does not change things.
  int_range<2> ir1 (vr1);
  ASSERT_TRUE (ir1 == vr1);
  ASSERT_FALSE (ir1.varying_p ());

  // The same test as above, but using TYPE_{MIN,MAX}_VALUE instead of [0,3].
  vr1 = int_range<1> (TYPE_MIN_VALUE (rtype), TYPE_MAX_VALUE (rtype));
  ir1 = vr1;
  ASSERT_TRUE (ir1 == vr1);
  ASSERT_FALSE (ir1.varying_p ());
}

static void
range_tests_misc ()
{
  tree u128_type = build_nonstandard_integer_type (128, /*unsigned=*/1);
  int_range<1> i1, i2, i3;
  int_range<1> r0, r1, rold;

  // Test 1-bit signed integer union.
  // [-1,-1] U [0,0] = VARYING.
  tree one_bit_type = build_nonstandard_integer_type (1, 0);
  tree one_bit_min = vrp_val_min (one_bit_type);
  tree one_bit_max = vrp_val_max (one_bit_type);
  {
    int_range<2> min (one_bit_min, one_bit_min);
    int_range<2> max (one_bit_max, one_bit_max);
    max.union_ (min);
    ASSERT_TRUE (max.varying_p ());
  }
  // Test that we can set a range of true+false for a 1-bit signed int.
  r0 = range_true_and_false (one_bit_type);

  // Test inversion of 1-bit signed integers.
  {
    int_range<2> min (one_bit_min, one_bit_min);
    int_range<2> max (one_bit_max, one_bit_max);
    int_range<2> t;
    t = min;
    t.invert ();
    ASSERT_TRUE (t == max);
    t = max;
    t.invert ();
    ASSERT_TRUE (t == min);
  }

  // Test that NOT(255) is [0..254] in 8-bit land.
  int_range<1> not_255 (UCHAR (255), UCHAR (255), VR_ANTI_RANGE);
  ASSERT_TRUE (not_255 == int_range<1> (UCHAR (0), UCHAR (254)));

  // Test that NOT(0) is [1..255] in 8-bit land.
  int_range<1> not_zero = range_nonzero (unsigned_char_type_node);
  ASSERT_TRUE (not_zero == int_range<1> (UCHAR (1), UCHAR (255)));

  // Check that [0,127][0x..ffffff80,0x..ffffff]
  //  => ~[128, 0x..ffffff7f].
  r0 = int_range<1> (UINT128 (0), UINT128 (127));
  tree high = build_minus_one_cst (u128_type);
  // low = -1 - 127 => 0x..ffffff80.
  tree low = fold_build2 (MINUS_EXPR, u128_type, high, UINT128(127));
  r1 = int_range<1> (low, high); // [0x..ffffff80, 0x..ffffffff]
  // r0 = [0,127][0x..ffffff80,0x..fffffff].
  r0.union_ (r1);
  // r1 = [128, 0x..ffffff7f].
  r1 = int_range<1> (UINT128(128),
		     fold_build2 (MINUS_EXPR, u128_type,
				  build_minus_one_cst (u128_type),
				  UINT128(128)));
  r0.invert ();
  ASSERT_TRUE (r0 == r1);

  r0.set_varying (integer_type_node);
  tree minint = wide_int_to_tree (integer_type_node, r0.lower_bound ());
  tree maxint = wide_int_to_tree (integer_type_node, r0.upper_bound ());

  r0.set_varying (short_integer_type_node);

  r0.set_varying (unsigned_type_node);
  tree maxuint = wide_int_to_tree (unsigned_type_node, r0.upper_bound ());

  // Check that ~[0,5] => [6,MAX] for unsigned int.
  r0 = int_range<1> (UINT (0), UINT (5));
  r0.invert ();
  ASSERT_TRUE (r0 == int_range<1> (UINT(6), maxuint));

  // Check that ~[10,MAX] => [0,9] for unsigned int.
  r0 = int_range<1> (UINT(10), maxuint);
  r0.invert ();
  ASSERT_TRUE (r0 == int_range<1> (UINT (0), UINT (9)));

  // Check that ~[0,5] => [6,MAX] for unsigned 128-bit numbers.
  r0 = int_range<1> (UINT128 (0), UINT128 (5), VR_ANTI_RANGE);
  r1 = int_range<1> (UINT128(6), build_minus_one_cst (u128_type));
  ASSERT_TRUE (r0 == r1);

  // Check that [~5] is really [-MIN,4][6,MAX].
  r0 = int_range<1> (INT (5), INT (5), VR_ANTI_RANGE);
  r1 = int_range<1> (minint, INT (4));
  r1.union_ (int_range<1> (INT (6), maxint));
  ASSERT_FALSE (r1.undefined_p ());
  ASSERT_TRUE (r0 == r1);

  r1 = int_range<1> (INT (5), INT (5));
  int_range<1> r2 (r1);
  ASSERT_TRUE (r1 == r2);

  r1 = int_range<1> (INT (5), INT (10));

  r1 = int_range<1> (integer_type_node,
		     wi::to_wide (INT (5)), wi::to_wide (INT (10)));
  ASSERT_TRUE (r1.contains_p (INT (7)));

  r1 = int_range<1> (SCHAR (0), SCHAR (20));
  ASSERT_TRUE (r1.contains_p (SCHAR(15)));
  ASSERT_FALSE (r1.contains_p (SCHAR(300)));

  // NOT([10,20]) ==> [-MIN,9][21,MAX].
  r0 = r1 = int_range<1> (INT (10), INT (20));
  r2 = int_range<1> (minint, INT(9));
  r2.union_ (int_range<1> (INT(21), maxint));
  ASSERT_FALSE (r2.undefined_p ());
  r1.invert ();
  ASSERT_TRUE (r1 == r2);
  // Test that NOT(NOT(x)) == x.
  r2.invert ();
  ASSERT_TRUE (r0 == r2);

  // Test that booleans and their inverse work as expected.
  r0 = range_zero (boolean_type_node);
  ASSERT_TRUE (r0 == int_range<1> (build_zero_cst (boolean_type_node),
				   build_zero_cst (boolean_type_node)));
  r0.invert ();
  ASSERT_TRUE (r0 == int_range<1> (build_one_cst (boolean_type_node),
				   build_one_cst (boolean_type_node)));

  // Make sure NULL and non-NULL of pointer types work, and that
  // inverses of them are consistent.
  tree voidp = build_pointer_type (void_type_node);
  r0 = range_zero (voidp);
  r1 = r0;
  r0.invert ();
  r0.invert ();
  ASSERT_TRUE (r0 == r1);

  // [10,20] U [15, 30] => [10, 30].
  r0 = int_range<1> (INT (10), INT (20));
  r1 = int_range<1> (INT (15), INT (30));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == int_range<1> (INT (10), INT (30)));

  // [15,40] U [] => [15,40].
  r0 = int_range<1> (INT (15), INT (40));
  r1.set_undefined ();
  r0.union_ (r1);
  ASSERT_TRUE (r0 == int_range<1> (INT (15), INT (40)));

  // [10,20] U [10,10] => [10,20].
  r0 = int_range<1> (INT (10), INT (20));
  r1 = int_range<1> (INT (10), INT (10));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == int_range<1> (INT (10), INT (20)));

  // [10,20] U [9,9] => [9,20].
  r0 = int_range<1> (INT (10), INT (20));
  r1 = int_range<1> (INT (9), INT (9));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == int_range<1> (INT (9), INT (20)));

  // [10,20] ^ [15,30] => [15,20].
  r0 = int_range<1> (INT (10), INT (20));
  r1 = int_range<1> (INT (15), INT (30));
  r0.intersect (r1);
  ASSERT_TRUE (r0 == int_range<1> (INT (15), INT (20)));

  // Test the internal sanity of wide_int's wrt HWIs.
  ASSERT_TRUE (wi::max_value (TYPE_PRECISION (boolean_type_node),
			      TYPE_SIGN (boolean_type_node))
	       == wi::uhwi (1, TYPE_PRECISION (boolean_type_node)));

  // Test zero_p().
  r0 = int_range<1> (INT (0), INT (0));
  ASSERT_TRUE (r0.zero_p ());

  // Test nonzero_p().
  r0 = int_range<1> (INT (0), INT (0));
  r0.invert ();
  ASSERT_TRUE (r0.nonzero_p ());

  // test legacy interaction
  // r0 = ~[1,1]
  r0 = int_range<1> (UINT (1), UINT (1), VR_ANTI_RANGE);
  // r1 = ~[3,3]
  r1 = int_range<1> (UINT (3), UINT (3), VR_ANTI_RANGE);

  // vv = [0,0][2,2][4, MAX]
  int_range<3> vv = r0;
  vv.intersect (r1);

  ASSERT_TRUE (vv.contains_p (UINT (2)));
  ASSERT_TRUE (vv.num_pairs () == 3);

  // create r0 as legacy [1,1]
  r0 = int_range<1> (UINT (1), UINT (1));
  // And union it with  [0,0][2,2][4,MAX] multi range
  r0.union_ (vv);
  // The result should be [0,2][4,MAX], or ~[3,3]  but it must contain 2
  ASSERT_TRUE (r0.contains_p (UINT (2)));
}

static void
range_tests_nonzero_bits ()
{
  int_range<2> r0, r1;

  // Adding nonzero bits to a varying drops the varying.
  r0.set_varying (integer_type_node);
  r0.set_nonzero_bits (255);
  ASSERT_TRUE (!r0.varying_p ());
  // Dropping the nonzero bits brings us back to varying.
  r0.set_nonzero_bits (-1);
  ASSERT_TRUE (r0.varying_p ());

  // Test contains_p with nonzero bits.
  r0.set_zero (integer_type_node);
  ASSERT_TRUE (r0.contains_p (INT (0)));
  ASSERT_FALSE (r0.contains_p (INT (1)));
  r0.set_nonzero_bits (0xfe);
  ASSERT_FALSE (r0.contains_p (INT (0x100)));
  ASSERT_FALSE (r0.contains_p (INT (0x3)));

  // Union of nonzero bits.
  r0.set_varying (integer_type_node);
  r0.set_nonzero_bits (0xf0);
  r1.set_varying (integer_type_node);
  r1.set_nonzero_bits (0xf);
  r0.union_ (r1);
  ASSERT_TRUE (r0.get_nonzero_bits () == 0xff);

  // Intersect of nonzero bits.
  r0.set (INT (0), INT (255));
  r0.set_nonzero_bits (0xfe);
  r1.set_varying (integer_type_node);
  r1.set_nonzero_bits (0xf0);
  r0.intersect (r1);
  ASSERT_TRUE (r0.get_nonzero_bits () == 0xf0);

  // Intersect where the mask of nonzero bits is implicit from the range.
  r0.set_varying (integer_type_node);
  r1.set (INT (0), INT (255));
  r0.intersect (r1);
  ASSERT_TRUE (r0.get_nonzero_bits () == 0xff);

  // The union of a mask of 0xff..ffff00 with a mask of 0xff spans the
  // entire domain, and makes the range a varying.
  r0.set_varying (integer_type_node);
  wide_int x = wi::shwi (0xff, TYPE_PRECISION (integer_type_node));
  x = wi::bit_not (x);
  r0.set_nonzero_bits (x); 	// 0xff..ff00
  r1.set_varying (integer_type_node);
  r1.set_nonzero_bits (0xff);
  r0.union_ (r1);
  ASSERT_TRUE (r0.varying_p ());

  // Test that setting a nonzero bit of 1 does not pessimize the range.
  r0.set_zero (integer_type_node);
  r0.set_nonzero_bits (1);
  ASSERT_TRUE (r0.zero_p ());
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

  // NAN ranges are not equal to each other.
  r0.set_nan (float_type_node);
  r1 = r0;
  ASSERT_FALSE (r0 == r1);
  ASSERT_FALSE (r0 == r0);
  ASSERT_TRUE (r0 != r0);

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
  tree nan = build_real (float_type_node, r);
  ASSERT_TRUE (r0.contains_p (nan));

  // -NAN is in a VARYING.
  r0.set_varying (float_type_node);
  q = real_value_negate (&r);
  tree neg_nan = build_real (float_type_node, q);
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
}

static void
range_tests_signed_zeros ()
{
  tree zero = build_zero_cst (float_type_node);
  tree neg_zero = fold_build1 (NEGATE_EXPR, float_type_node, zero);
  frange r0, r1;
  bool signbit;

  // [0,0] contains [0,0] but not [-0,-0] and vice versa.
  r0 = frange (zero, zero);
  r1 = frange (neg_zero, neg_zero);
  ASSERT_TRUE (r0.contains_p (zero));
  ASSERT_TRUE (!r0.contains_p (neg_zero));
  ASSERT_TRUE (r1.contains_p (neg_zero));
  ASSERT_TRUE (!r1.contains_p (zero));

  // Test contains_p() when we know the sign of the zero.
  r0 = frange (zero, zero);
  ASSERT_TRUE (r0.contains_p (zero));
  ASSERT_FALSE (r0.contains_p (neg_zero));
  r0 = frange (neg_zero, neg_zero);
  ASSERT_TRUE (r0.contains_p (neg_zero));
  ASSERT_FALSE (r0.contains_p (zero));

  r0 = frange (neg_zero, zero);
  ASSERT_TRUE (r0.contains_p (neg_zero));
  ASSERT_TRUE (r0.contains_p (zero));

  r0 = frange_float ("-3", "5");
  ASSERT_TRUE (r0.contains_p (neg_zero));
  ASSERT_TRUE (r0.contains_p (zero));

  // The intersection of zeros that differ in sign is a NAN (or
  // undefined if not honoring NANs).
  r0 = frange (neg_zero, neg_zero);
  r1 = frange (zero, zero);
  r0.intersect (r1);
  if (HONOR_NANS (float_type_node))
    ASSERT_TRUE (r0.known_isnan ());
  else
    ASSERT_TRUE (r0.undefined_p ());

  // The union of zeros that differ in sign is a zero with unknown sign.
  r0 = frange (zero, zero);
  r1 = frange (neg_zero, neg_zero);
  r0.union_ (r1);
  ASSERT_TRUE (r0.zero_p () && !r0.signbit_p (signbit));

  // [-0, +0] has an unknown sign.
  r0 = frange (neg_zero, zero);
  ASSERT_TRUE (r0.zero_p () && !r0.signbit_p (signbit));

  // [-0, +0] ^ [0, 0] is [0, 0]
  r0 = frange (neg_zero, zero);
  r1 = frange (zero, zero);
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
range_tests_floats ()
{
  frange r0, r1;

  if (HONOR_NANS (float_type_node))
    range_tests_nan ();
  range_tests_signbit ();

  if (HONOR_SIGNED_ZEROS (float_type_node))
    range_tests_signed_zeros ();

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

  // [3,5] U [10,12] = [3,12].
  r0 = frange_float ("3", "5");
  r1 = frange_float ("10", "12");
  r0.union_ (r1);
  ASSERT_EQ (r0, frange_float ("3", "12"));

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
  range_tests_legacy ();
  range_tests_irange3 ();
  range_tests_int_range_max ();
  range_tests_strict_enum ();
  range_tests_nonzero_bits ();
  range_tests_floats_various ();
  range_tests_misc ();
}

} // namespace selftest

#endif // CHECKING_P
