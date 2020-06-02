/* Support routines for value ranges.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

// Abstract vrange implementation.

unsigned
vrange::num_pairs () const
{
  return m_num_ranges;
}

vrange&
vrange::operator= (const vrange &src)
{
  if (compatible_copy_p (src))
    copy_compatible_range (src);
  else if (simple_ranges_p () != src.simple_ranges_p ())
    copy_simple_range (src);
  else
    copy_incompatible_range (src);
  return *this;
}

bool
vrange::compatible_copy_p (const vrange &src) const
{
  if (src.undefined_p () || src.varying_p ())
    return true;

  // Symbolics may be copied straight because there's only one
  // representation for them.
  const irange *int_range = as_a <const irange *> (&src);
  if (int_range && !range_has_numeric_bounds_p (int_range))
    return true;

  if (simple_ranges_p () != src.simple_ranges_p ())
    return false;

  return m_max_ranges >= src.m_num_ranges;
}

void
vrange::copy_compatible_range (const vrange &src)
{
  if (src.undefined_p ())
    set_undefined ();
  else if (src.varying_p ())
    set_varying (src.type ());
  else
    {
      m_kind = src.m_kind;
      m_num_ranges = src.m_num_ranges;
      for (unsigned i = 0; i < src.m_num_ranges; ++i)
	{
	  m_base[i * 2] = src.m_base[i * 2];
	  m_base[i * 2 + 1] = src.m_base[i * 2 + 1];
	}
    }
}

void
vrange::copy_incompatible_range (const vrange &src)
{
  set_undefined ();
  union_ (src);
}

bool
irange::maybe_anti_range (const irange &src) const
{
  tree ttype = src.type ();
  unsigned int precision = TYPE_PRECISION (ttype);
  signop sign = TYPE_SIGN (ttype);
  wide_int type_min = wi::min_value (precision, sign);
  wide_int type_max = wi::max_value (precision, sign);
  wide_int lb = src.lower_bound ();
  wide_int ub = src.upper_bound ();
  return (src.num_pairs () > 1
	  && precision > 1
	  && lb == type_min
	  && ub == type_max);
}

void
irange::copy_simple_range (const vrange &vsrc)
{
  const irange *src = as_a <const irange *> (&vsrc);
  gcc_checking_assert (src->simple_ranges_p () != simple_ranges_p ());

  if (src->undefined_p ())
    set_undefined ();
  else if (src->varying_p ())
    set_varying (src->type ());
  else if (src->kind () == VR_ANTI_RANGE)
    {
      tree lb = src->min (), ub = src->max ();
      set (lb, ub, VR_ANTI_RANGE);
    }
  else if (maybe_anti_range (*src))
    {
      widest_irange tmp = *src;
      tmp.invert ();
      set (wide_int_to_tree (src->type (), tmp.lower_bound (0)),
	   wide_int_to_tree (src->type (), tmp.upper_bound (0)),
	   VR_ANTI_RANGE);
    }
  else
    {
      wide_int lb = src->lower_bound (), ub = src->upper_bound ();
      set (wide_int_to_tree (src->type (), lb),
	   wide_int_to_tree (src->type (), ub),
	   src->kind ());
    }
}

// irange constructors

irange::irange (tree *base, unsigned nranges)
{
  m_kind = VR_UNDEFINED;
  m_discriminator = VRANGE_KIND_INT;
  m_base = base;
  m_num_ranges = 0;
  m_max_ranges = nranges;
}

irange::irange (tree *base, unsigned nranges, const irange &other)
{
  m_discriminator = VRANGE_KIND_INT;
  m_base = base;
  m_max_ranges = nranges;
  // Uses the generic vrange copy constructor to copy without sharing.
  *this = other;
}

// int_range constructors.

template<unsigned N>
int_range<N>::int_range (const int_range &other)
  : irange (m_ranges, N, other)
{
}

template<unsigned N>
int_range<N>::int_range (tree min, tree max, value_range_kind kind)
  : irange (m_ranges, N)
{
  set (min, max, kind);
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
  : irange (m_ranges, N, other)
{
}

template<unsigned N>
int_range<N>&
int_range<N>::operator= (const int_range &src)
{
  vrange::operator= (src);
  return *this;
}

// widest_irange implementation

widest_irange::widest_irange ()
  : irange (m_ranges, m_sub_ranges_in_local_storage)
{
  init_widest_irange ();
}

widest_irange::widest_irange (const widest_irange &other)
  : irange (m_ranges, m_sub_ranges_in_local_storage)
{
  init_widest_irange ();
  resize_if_needed (other.num_pairs ());
  vrange::operator= (other);
}

widest_irange::widest_irange (tree min, tree max, value_range_kind kind)
  : irange (m_ranges, m_sub_ranges_in_local_storage)
{
  init_widest_irange ();
  set (min, max, kind);
}

widest_irange::widest_irange (tree type)
  : irange (m_ranges, m_sub_ranges_in_local_storage)
{
  init_widest_irange ();
  set_varying (type);
}

widest_irange::widest_irange (tree type,
			      const wide_int &wmin, const wide_int &wmax,
			      value_range_kind kind)
  : irange (m_ranges, m_sub_ranges_in_local_storage)
{
  init_widest_irange ();
  tree min = wide_int_to_tree (type, wmin);
  tree max = wide_int_to_tree (type, wmax);
  set (min, max, kind);
}

widest_irange::widest_irange (const irange &other)
  : irange (m_ranges, m_sub_ranges_in_local_storage)
{
  init_widest_irange ();
  resize_if_needed (other.num_pairs ());
  vrange::operator= (other);
}

widest_irange::~widest_irange ()
{
  if (CHECKING_P)
    stats_register_use ();
  if (m_blob)
    free (m_blob);
}

widest_irange &
widest_irange::operator= (const widest_irange &src)
{
  vrange::operator= (src);
  return *this;
}

void
widest_irange::init_widest_irange ()
{
  m_blob = NULL;
}

void
widest_irange::resize_if_needed (unsigned nranges)
{
  if (m_max_ranges >= nranges)
    return;

  // We're about to double the size.  Bail if it won't fit.
  if (nranges * 2 > sizeof (m_max_ranges) * 255)
    return;

  bool must_initialize = m_blob == NULL;
  m_max_ranges = nranges * 2;
  unsigned alloc_size = m_max_ranges * sizeof (*m_blob) * 2;
  m_blob = (tree *) xrealloc (m_blob, alloc_size);
  m_base = m_blob;
  if (must_initialize)
    {
      for (unsigned i = 0; i < m_num_ranges; ++i)
	{
	  m_blob[i * 2] = m_ranges[i * 2];
	  m_blob[i * 2 + 1] = m_ranges[i * 2 + 1];
	}
    }
}

void
widest_irange::union_ (const vrange &other)
{
  unsigned size = num_pairs () + other.num_pairs ();
  resize_if_needed (size);
  irange::union_ (other);
}

void
widest_irange::invert ()
{
  unsigned size = num_pairs () + 1;
  resize_if_needed (size);
  irange::invert ();
}

int widest_irange::stats_used_buckets[11];

void
widest_irange::stats_register_use (void)
{
  int n = num_pairs ();
  if (n < 10)
    stats_used_buckets[n]++;
  else
    stats_used_buckets[10]++;
}

void
widest_irange::stats_dump (FILE *file)
{
  fprintf (file, "\nwidest_irange stats:\n");
  for (int i = 0; i < 11; ++i)
    {
      if (stats_used_buckets[i] == 0)
	continue;
      if (i < 10)
	fprintf (file, "%2d sub-ranges: %d\n", i, stats_used_buckets[i]);
      else
	fprintf (file, "10+ sub-ranges: %d\n", stats_used_buckets[i]);
    }
}

void
dump_value_range_stats (FILE *file)
{
  widest_irange::stats_dump (file);
}

void
vrange::set_undefined ()
{
  if (simple_ranges_p ())
    m_kind = VR_UNDEFINED;
  else m_kind = VR_RANGE;
  m_num_ranges = 0;
}

void
vrange::set_varying (tree type)
{
  if (simple_ranges_p ())
    m_kind = VR_VARYING;
  else
    m_kind = VR_RANGE;
  m_num_ranges = 1;
  m_base[0] = vrp_val_min (type);
  m_base[1] = vrp_val_max (type);
  if (!m_base[0])
    /* We can't do anything range-wise with these types.  */
    m_base[0] = m_base[1] = error_mark_node;
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
  /* Use the canonical setters for VR_UNDEFINED and VR_VARYING.  */
  if (kind == VR_UNDEFINED)
    {
      set_undefined ();
      return;
    }
  else if (kind == VR_VARYING)
    {
      gcc_assert (TREE_TYPE (min) == TREE_TYPE (max));
      tree typ = TREE_TYPE (min);
      if (supports_type_p (typ))
	{
	  gcc_assert (vrp_val_min (typ));
	  gcc_assert (vrp_val_max (typ));
	}
      set_varying (typ);
      return;
    }

  /* Convert POLY_INT_CST bounds into worst-case INTEGER_CST bounds.  */
  if (POLY_INT_CST_P (min))
    {
      tree type_min = vrp_val_min (TREE_TYPE (min));
      widest_int lb
	= constant_lower_bound_with_limit (wi::to_poly_widest (min),
					   wi::to_widest (type_min));
      min = wide_int_to_tree (TREE_TYPE (min), lb);
    }
  if (POLY_INT_CST_P (max))
    {
      tree type_max = vrp_val_max (TREE_TYPE (max));
      widest_int ub
	= constant_upper_bound_with_limit (wi::to_poly_widest (max),
					   wi::to_widest (type_max));
      max = wide_int_to_tree (TREE_TYPE (max), ub);
    }

  /* Nothing to canonicalize for symbolic ranges.  */
  if (TREE_CODE (min) != INTEGER_CST
      || TREE_CODE (max) != INTEGER_CST)
    {
      m_kind = kind;
      m_base[0] = min;
      m_base[1] = max;
      m_num_ranges = 1;
      return;
    }

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
	  set_varying (TREE_TYPE (min));
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
	  set_varying (TREE_TYPE (min));
	  return;
	}

      kind = kind == VR_RANGE ? VR_ANTI_RANGE : VR_RANGE;
    }

  tree type = TREE_TYPE (min);

  /* Anti-ranges that can be represented as ranges should be so.  */
  if (kind == VR_ANTI_RANGE)
    {
      if (!simple_ranges_p ())
	{
	  multi_range_set_anti_range (min, max);
	  return;
	}
      /* For -fstrict-enums we may receive out-of-range ranges so consider
         values < -INF and values > INF as -INF/INF as well.  */
      bool is_min = vrp_val_is_min (min);
      bool is_max = vrp_val_is_max (max);

      if (is_min && is_max)
	{
	  /* We cannot deal with empty ranges, drop to varying.
	     ???  This could be VR_UNDEFINED instead.  */
	  set_varying (type);
	  return;
	}
      else if (TYPE_PRECISION (TREE_TYPE (min)) == 1
	       && (is_min || is_max))
	{
	  /* Non-empty boolean ranges can always be represented
	     as a singleton range.  */
	  if (is_min)
	    min = max = vrp_val_max (TREE_TYPE (min));
	  else
	    min = max = vrp_val_min (TREE_TYPE (min));
	  kind = VR_RANGE;
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

  /* Normalize [MIN, MAX] into VARYING and ~[MIN, MAX] into UNDEFINED.

     Avoid using TYPE_{MIN,MAX}_VALUE because -fstrict-enums can
     restrict those to a subset of what actually fits in the type.
     Instead use the extremes of the type precision which will allow
     compare_range_with_value() to check if a value is inside a range,
     whereas if we used TYPE_*_VAL, said function would just punt
     upon seeing a VARYING.  */
  unsigned prec = TYPE_PRECISION (type);
  signop sign = TYPE_SIGN (type);
  if (wi::eq_p (wi::to_wide (min), wi::min_value (prec, sign))
      && wi::eq_p (wi::to_wide (max), wi::max_value (prec, sign)))
    {
      if (kind == VR_RANGE)
	set_varying (type);
      else if (kind == VR_ANTI_RANGE)
	set_undefined ();
      else
	gcc_unreachable ();
      return;
    }

  /* Do not drop [-INF(OVF), +INF(OVF)] to varying.  (OVF) has to be sticky
     to make sure VRP iteration terminates, otherwise we can get into
     oscillations.  */

  m_kind = kind;
  m_base[0] = min;
  m_base[1] = max;
  m_num_ranges = 1;
  if (flag_checking)
    check ();
}

void
irange::multi_range_set_anti_range (tree min, tree max)
{
  // Calculate INVERSE([I,J]) as [-MIN, I-1][J+1, +MAX].
  tree type = TREE_TYPE (min);
  tree type_min = vrp_val_min (type);
  tree type_max = vrp_val_max (type);
  m_num_ranges = 0;

  if (!operand_equal_p (min, type_min, 0))
    {
      tree one = build_int_cst (type, 1);
      tree min_minus_one = int_const_binop (MINUS_EXPR, min, one);
      if (!TREE_OVERFLOW (min_minus_one))
	{
	  m_base[0] = type_min;
	  m_base[1] = min_minus_one;
	  m_num_ranges = 1;
	}
    }
  if (!operand_equal_p (max, type_max, 0))
    {
      tree one = build_int_cst (type, 1);
      tree max_plus_one = int_const_binop (PLUS_EXPR, max, one);
      if (!TREE_OVERFLOW (max_plus_one))
	{
	  m_base[m_num_ranges * 2] = max_plus_one;
	  m_base[m_num_ranges * 2 + 1] = type_max;
	  ++m_num_ranges;
	}
    }
  if (m_num_ranges)
    m_kind = VR_RANGE;
  else
    {
      // If we have no ranges, we tried to calculate the inverse
      // of [-MIN, +MAX] which is actually the empty set.
      gcc_checking_assert (m_num_ranges == 0);
      m_kind = VR_RANGE;
    }
}

void
irange::set (tree val)
{
  gcc_assert (TREE_CODE (val) == SSA_NAME || is_gimple_min_invariant (val));
  if (TREE_OVERFLOW_P (val))
    val = drop_tree_overflow (val);
  set (val, val);
}

/* Set value range VR to a nonzero range of type TYPE.  */

void
irange::set_nonzero (tree type)
{
  tree zero = build_int_cst (type, 0);
  set (zero, zero, VR_ANTI_RANGE);
}

/* Set value range VR to a ZERO range of type TYPE.  */

void
irange::set_zero (tree type)
{
  set (build_int_cst (type, 0));
}

/* Check the validity of the range.  */

void
irange::check ()
{
  switch (m_kind)
    {
    case VR_ANTI_RANGE:
      gcc_assert (simple_ranges_p () || !range_has_numeric_bounds_p (this));
      gcc_fallthrough ();
    case VR_RANGE:
      {
	gcc_assert (m_num_ranges > 0 || !simple_ranges_p ());

	for (unsigned i = 0; i < m_num_ranges; ++i)
	  {
	    tree lb = tree_lower_bound (i);
	    tree ub = tree_upper_bound (i);
	    int cmp = compare_values (lb, ub);
	    gcc_assert (cmp == 0 || cmp == -1 || cmp == -2);
	  }
	if (simple_ranges_p ())
	  {
	    /* Creating ~[-MIN, +MAX] is stupid because that would be
	       the empty set.  */
	    if (INTEGRAL_TYPE_P (TREE_TYPE (min ())) && m_kind == VR_ANTI_RANGE)
	      gcc_assert (!vrp_val_is_min (min ()) || !vrp_val_is_max (max ()));
	  }
	break;
      }
    case VR_UNDEFINED:
      gcc_assert (m_num_ranges == 0);
      break;
    case VR_VARYING:
      gcc_assert (m_num_ranges == 1);
      break;
    default:
      gcc_unreachable ();
    }
}

/* Return the number of sub-ranges in a range.  */

unsigned
irange::num_pairs () const
{
  if (undefined_p ())
    return 0;
  if (varying_p ())
    return 1;
  if (symbolic_p ())
    {
      value_range numeric_range (*this);
      numeric_range.normalize_symbolics ();
      return numeric_range.num_pairs ();
    }
  if (m_kind == VR_ANTI_RANGE)
    {
      // ~[MIN, X] has one sub-range of [X+1, MAX], and
      // ~[X, MAX] has one sub-range of [MIN, X-1].
      if (vrp_val_is_min (min ()) || vrp_val_is_max (max ()))
	return 1;
      return 2;
    }
  return m_num_ranges;
}

/* Return the lower bound for a sub-range.  PAIR is the sub-range in
   question.  */

wide_int
irange::lower_bound (unsigned pair) const
{
  if (symbolic_p ())
    {
      value_range numeric_range (*this);
      numeric_range.normalize_symbolics ();
      return numeric_range.lower_bound (pair);
    }

  gcc_checking_assert (!undefined_p ());
  gcc_checking_assert (pair + 1 <= num_pairs ());
  tree t = NULL;
  if (m_kind == VR_ANTI_RANGE)
    {
      tree typ = type ();
      if (pair == 1 || vrp_val_is_min (min ()))
	t = wide_int_to_tree (typ, wi::to_wide (max ()) + 1);
      else
	t = vrp_val_min (typ);
    }
  else
    t = tree_lower_bound (pair);
  return wi::to_wide (t);
}

/* Return the upper bound for a sub-range.  PAIR is the sub-range in
   question.  */

wide_int
irange::upper_bound (unsigned pair) const
{
  if (symbolic_p ())
    {
      value_range numeric_range (*this);
      numeric_range.normalize_symbolics ();
      return numeric_range.upper_bound (pair);
    }

  gcc_checking_assert (!undefined_p ());
  gcc_checking_assert (pair + 1 <= num_pairs ());
  tree t = NULL;
  if (m_kind == VR_ANTI_RANGE)
    {
      tree typ = type ();
      if (pair == 1 || vrp_val_is_min (min ()))
	t = vrp_val_max (typ);
      else
	t = wide_int_to_tree (typ, wi::to_wide (min ()) - 1);
    }
  else
    t = tree_upper_bound (pair);
  return wi::to_wide (t);
}

/* Return the highest bound in a range.  */

wide_int
irange::upper_bound () const
{
  unsigned pairs = num_pairs ();
  gcc_checking_assert (pairs > 0);
  return upper_bound (pairs - 1);
}

bool
irange::equal_p (const irange &other) const
{
  if (simple_ranges_p () != other.simple_ranges_p ())
    {
      widest_irange i = *this;
      widest_irange j = other;
      return i.equal_p (j);
    }

  if (m_kind != other.m_kind || m_num_ranges != other.m_num_ranges)
    return false;

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
  return true;
}

bool
irange::operator== (const irange &r) const
{
  return equal_p (r);
}

/* If range is a singleton, place it in RESULT and return TRUE.
   Note: A singleton can be any gimple invariant, not just constants.
   So, [&x, &x] counts as a singleton.  */
/* Return TRUE if this is a symbolic range.  */

bool
irange::symbolic_p () const
{
  return (!varying_p ()
	  && !undefined_p ()
	  && (!is_gimple_min_invariant (min ())
	      || !is_gimple_min_invariant (max ())));
}

/* NOTE: This is not the inverse of symbolic_p because the range
   could also be varying or undefined.  Ideally they should be inverse
   of each other, with varying only applying to symbolics.  Varying of
   constants would be represented as [-MIN, +MAX].  */

bool
irange::constant_p () const
{
  return (!varying_p ()
	  && !undefined_p ()
	  && TREE_CODE (min ()) == INTEGER_CST
	  && TREE_CODE (max ()) == INTEGER_CST);
}

bool
irange::singleton_p (tree *result) const
{
  if (!simple_ranges_p ())
    {
      if (num_pairs () == 1
	  && operand_equal_p (min (), max (), 0))
	{
	  if (result)
	    *result = min ();
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
  int cmp1, cmp2;

  if (varying_p ())
    return 1;

  if (undefined_p ())
    return 0;

  /* For constants we can just intersect and avoid using VR_ANTI_RANGE
     code further below.  */
  if (TREE_CODE (val) == INTEGER_CST && constant_p ())
    {
      widest_irange v (val, val);
      v.intersect (this);
      return v == value_range (val, val) ? 1 : 0;
    }

  cmp1 = operand_less_p (val, min ());
  if (cmp1 == -2)
    return -2;
  if (cmp1 == 1)
    return m_kind != VR_RANGE;

  cmp2 = operand_less_p (max (), val);
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

bool
irange::contains_p (tree cst) const
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
	   && operand_less_p (*vr0min, vr1min) == 1)
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
	   && operand_less_p (vr1min, *vr0min) == 1)
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
   value ranges VR0 and VR1, return the intersection of the two
   ranges.  This may not be the smallest possible such range.  */

value_range
intersect_helper (const value_range *vr0, const value_range *vr1)
{
  /* If either range is VR_VARYING the other one wins.  */
  if (vr1->varying_p ())
    return *vr0;
  if (vr0->varying_p ())
    return *vr1;

  /* When either range is VR_UNDEFINED the resulting range is
     VR_UNDEFINED, too.  */
  if (vr0->undefined_p ())
    return *vr0;
  if (vr1->undefined_p ())
    return *vr1;

  value_range_kind vr0kind = vr0->kind ();
  tree vr0min = vr0->min ();
  tree vr0max = vr0->max ();
  intersect_ranges (&vr0kind, &vr0min, &vr0max,
		    vr1->kind (), vr1->min (), vr1->max ());
  /* Make sure to canonicalize the result though as the inversion of a
     VR_RANGE can still be a VR_RANGE.  Work on a temporary so we can
     fall back to vr0 when this turns things to varying.  */
  value_range tem;
  if (vr0kind == VR_UNDEFINED)
    tem.set_undefined ();
  else if (vr0kind == VR_VARYING)
    tem.set_varying (vr0->type ());
  else
    tem.set (vr0min, vr0max, vr0kind);
  /* If that failed, use the saved original VR0.  */
  if (tem.varying_p ())
    return *vr0;

  return tem;
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

/* Helper for meet operation for value ranges.  Given two value ranges VR0 and
   VR1, return a range that contains both VR0 and VR1.  This may not be the
   smallest possible such range.  */

value_range
union_helper (const value_range *vr0, const value_range *vr1)
{
  /* VR0 has the resulting range if VR1 is undefined or VR0 is varying.  */
  if (vr1->undefined_p ()
      || vr0->varying_p ())
    return *vr0;

  /* VR1 has the resulting range if VR0 is undefined or VR1 is varying.  */
  if (vr0->undefined_p ()
      || vr1->varying_p ())
    return *vr1;

  value_range_kind vr0kind = vr0->kind ();
  tree vr0min = vr0->min ();
  tree vr0max = vr0->max ();
  union_ranges (&vr0kind, &vr0min, &vr0max,
		vr1->kind (), vr1->min (), vr1->max ());

  /* Work on a temporary so we can still use vr0 when union returns varying.  */
  value_range tem;
  if (vr0kind == VR_UNDEFINED)
    tem.set_undefined ();
  else if (vr0kind == VR_VARYING)
    tem.set_varying (vr0->type ());
  else
    tem.set (vr0min, vr0max, vr0kind);

  /* Failed to find an efficient meet.  Before giving up and setting
     the result to VARYING, see if we can at least derive a useful
     anti-range.  */
  if (tem.varying_p ()
      && range_includes_zero_p (vr0) == 0
      && range_includes_zero_p (vr1) == 0)
    {
      tem.set_nonzero (vr0->type ());
      return tem;
    }

  return tem;
}

/* Meet operation for value ranges.  Given two value ranges VR0 and
   VR1, store in VR0 a range that contains both VR0 and VR1.  This
   may not be the smallest possible such range.  */

void
irange::union_ (const irange *other)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Meeting\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\nand\n  ");
      dump_value_range (dump_file, other);
      fprintf (dump_file, "\n");
    }
  if (simple_ranges_p ())
    {
      int_range<1> small;
      if (!other->simple_ranges_p ())
	{
	  /* If a simple range was requested on the LHS, do the entire
	     operation in simple mode, because the RHS may be a
	     symbolic, and we only know how to deal with those in
	     simple mode.  */
	  small = *other;
	  other = &small;
	}
      *this = union_helper ((value_range *) this,
			    (const value_range *) other);
    }
  else
    {
      if (other->simple_ranges_p ())
	multi_range_union (widest_irange (*other));
      else
	multi_range_union (*other);
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "to\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\n");
    }
}

class disable_dump_details
{
public:
  disable_dump_details ()
  {
    m_flags = dump_flags;
    dump_flags &= ~TDF_DETAILS;
  }
  ~disable_dump_details ()
  {
    dump_flags = m_flags;
  }
private:
  dump_flags_t m_flags;
};

/* Range union, but for references.  */

void
irange::union_ (const vrange &vr)
{
  disable_dump_details details;
  const irange *other = as_a <const irange *> (&vr);
  union_ (other);
}

void
irange::intersect (const irange *other)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Intersecting\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\nand\n  ");
      dump_value_range (dump_file, other);
      fprintf (dump_file, "\n");
    }
  if (simple_ranges_p ())
    {
      int_range<1> small;
      if (!other->simple_ranges_p ())
	{
	  /* If a simple range was requested on the LHS, do the entire
	     operation in simple mode, because the RHS may be a
	     symbolic, and we only know how to deal with those in
	     simple mode.

	     FIXME: Squishing [10,10][20,20] into a value_range, will
	     yield [10,20].  If this result is then used with
	     contains_p(15), the result may be wrong.  Revisit this.  */
	  small = *other;
	  other = &small;
	}
      *this = intersect_helper ((value_range *) this,
				(const value_range *) other);
    }
  else
    {
      if (other->simple_ranges_p ())
	multi_range_intersect (widest_irange (*other));
      else
	multi_range_intersect (*other);
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "to\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\n");
    }
}

/* Range intersect, but for references.  */

void
irange::intersect (const vrange &vr)
{
  disable_dump_details details;
  const irange *other = as_a <const irange *> (&vr);
  intersect (other);
}

void
irange::multi_range_union (const irange &r)
{
  if (r.undefined_p ())
    return;

  // Do not worry about merging and such by reserving twice as many
  // pairs as needed, and then simply sorting the 2 ranges into this
  // intermediate form.
  //
  // The intermediate result will have the property that the beginning
  // of each range is <= the beginning of the next range.  There may
  // be overlapping ranges at this point.  I.e. this would be valid
  // [-20, 10], [-10, 0], [0, 20], [40, 90] as it satisfies this
  // contraint : -20 < -10 < 0 < 40 When the range is rebuilt into r,
  // the merge is performed.
  //
  // [Xi,Yi]..[Xn,Yn]  U  [Xj,Yj]..[Xm,Ym]   -->  [Xk,Yk]..[Xp,Yp]
  tree ttype = r.type ();
  signop sign = TYPE_SIGN (ttype);
  // ?? We may need something faster than vectors here, not sure.
  auto_vec<wide_int, 8> res;
  wide_int u1 ;
  wi::overflow_type ovf;
  unsigned i = 0, j = 0, k = 0;

  while (i < m_num_ranges * 2 && j < r.m_num_ranges * 2)
    {
      // lower of Xi and Xj is the lowest point.
      if (wi::le_p (wi::to_wide (m_base[i]), wi::to_wide (r.m_base[j]), sign))
	{
	  res.safe_push (wi::to_wide (m_base[i]));
	  res.safe_push (wi::to_wide (m_base[i + 1]));
	  k += 2;
	  i += 2;
	}
      else
	{
	  res.safe_push (wi::to_wide (r.m_base[j]));
	  res.safe_push (wi::to_wide (r.m_base[j + 1]));
	  k += 2;
	  j += 2;
	}
    }
  for ( ; i < m_num_ranges * 2; i += 2)
    {
      res.safe_push (wi::to_wide (m_base[i]));
      res.safe_push (wi::to_wide (m_base[i + 1]));
      k += 2;
    }
  for ( ; j < r.m_num_ranges * 2; j += 2)
    {
      res.safe_push (wi::to_wide (r.m_base[j]));
      res.safe_push (wi::to_wide (r.m_base[j + 1]));
      k += 2;
    }

  // Now normalize the vector removing any overlaps.
  i = 2;
  int prec = TYPE_PRECISION (ttype);
  wide_int max_val = wi::max_value (prec, sign);
  for (j = 2; j < k ; j += 2)
    {
      if (res[i - 1] == max_val)
	break;
      u1 = wi::add (res[i - 1], 1, sign, &ovf);

      // Overflow indicates we are at MAX already.
      // A wide int bug requires the previous max_val check
      // trigger: gcc.c-torture/compile/pr80443.c  with -O3
      if (ovf == wi::OVF_OVERFLOW)
	break;

      // Current upper+1 is >= lower bound next pair, then we merge ranges.
      if (wi::ge_p (u1, res[j], sign))
	{
	  // New upper bounds is greater of current or the next one.
	  if (wi::gt_p (res[j + 1], res [i - 1], sign))
	    res [i - 1] = res[j + 1];
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

  // At this point, the vector should have i ranges, none
  // overlapping. Now it simply needs to be copied, and if there are
  // too many ranges, merge some.  We wont do any analysis as to what
  // the "best" merges are, simply combine the final ranges into one.
  if (i > m_max_ranges * 2)
    {
      res[m_max_ranges * 2 - 1] = res[i - 1];
      i = m_max_ranges * 2;
    }

  for (j = 0; j < i ; j++)
    m_base[j] = wide_int_to_tree (ttype, res [j]);
  m_num_ranges = i / 2;
  m_kind = VR_RANGE;

  if (flag_checking)
    check ();
}

void
irange::intersect_from_wide_ints (const wide_int &x, const wide_int &y)
{
  if (undefined_p ())
    return;

  unsigned pos = 0;
  signop sign = TYPE_SIGN (type ());
  for (unsigned i = 0; i < num_pairs (); ++i)
    {
      wide_int newlo = wi::max (lower_bound (i), x, sign);
      wide_int newhi = wi::min (upper_bound (i), y, sign);
      if (wi::gt_p (newlo, newhi, sign))
	{
	  // If the new sub-range doesn't make sense, it's an
	  // impossible range and must be kept out of the result.
	}
      else
	{
	  m_base[pos++] = wide_int_to_tree (type (), newlo);
	  m_base[pos++] = wide_int_to_tree (type (), newhi);
	}
    }
  m_num_ranges = pos / 2;
  if (flag_checking)
    check ();
}

void
irange::multi_range_intersect (const irange &r)
{
  if (undefined_p ())
    return;
  if (r.undefined_p ())
    {
      set_undefined ();
      return;
    }

  // The algorithm is as follows.
  //
  // Intersect each sub-range of R with all of ORIG_RANGE one at a time, and
  // join/union the results of these intersections together.  I.e:
  //
  //   [10,20][30,40][50,60] ^ [15,25][38,51][55,70]
  //
  // Step 1: [10,20][30,40][50,60] ^ [15,25] => [15,20]
  // Step 2: [10,20][30,40][50,60] ^ [38,51] => [38,40]
  // Step 3: [10,20][30,40][50,60] ^ [55,70] => [55,60]
  // Final:  [15,20] U [38,40] U [55,60] => [15,20][38,40][55,60]
  widest_irange orig_range (*this);
  set_undefined ();
  for (unsigned i = 0; i < r.num_pairs (); ++i)
    {
      // ?? Ughh.  Come up with something less stupid.
      widest_irange tmp (orig_range);
      wide_int lb = r.lower_bound (i);
      wide_int ub = r.upper_bound (i);
      tmp.intersect_from_wide_ints (lb, ub);
      union_ (tmp);
    }
}

static wide_int inline
subtract_one (const wide_int &x, tree type, wi::overflow_type &overflow)
{
  // A signed 1-bit bit-field, has a range of [-1,0] so subtracting +1
  // overflows, since +1 is unrepresentable.  This is why we have an
  // addition of -1 here.
  if (TYPE_SIGN (type) == SIGNED)
    return wi::add (x, -1 , SIGNED, &overflow);
  else
    return wi::sub (x, 1, UNSIGNED, &overflow);
}

/* Return the inverse of a range.  */

void
irange::invert ()
{
  if (simple_ranges_p ())
    {
      /* We can't just invert VR_RANGE and VR_ANTI_RANGE because we may
	 create non-canonical ranges.  Use the constructors instead.  */
      if (m_kind == VR_RANGE)
	*this = value_range (min (), max (), VR_ANTI_RANGE);
      else if (m_kind == VR_ANTI_RANGE)
	*this = value_range (min (), max ());
      else
	gcc_unreachable ();
      return;
    }

  if (undefined_p ())
    return;

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
  widest_irange orig_range (*this);
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
      tmp = wi::add (wi::to_wide (orig_range.m_base[i]), 1, sign, &ovf);
      m_base[nitems++] = wide_int_to_tree (ttype, tmp);
      m_base[nitems++] = wide_int_to_tree (ttype, type_max);
      if (ovf)
	nitems -= 2;
    }
  m_num_ranges = nitems / 2;

  if (flag_checking)
    check ();
}

void
irange::simple_dump (FILE *file) const
{
  if (undefined_p ())
    fprintf (file, "UNDEFINED");
  else if (m_kind == VR_RANGE || m_kind == VR_ANTI_RANGE)
    {
      tree ttype = type ();

      print_generic_expr (file, ttype);
      fprintf (file, " ");

      fprintf (file, "%s[", (m_kind == VR_ANTI_RANGE) ? "~" : "");

      if (INTEGRAL_TYPE_P (ttype)
	  && !TYPE_UNSIGNED (ttype)
	  && vrp_val_is_min (min ())
	  && TYPE_PRECISION (ttype) != 1)
	fprintf (file, "-INF");
      else
	print_generic_expr (file, min ());

      fprintf (file, ", ");

      if (supports_type_p (ttype)
	  && vrp_val_is_max (max ())
	  && TYPE_PRECISION (ttype) != 1)
	fprintf (file, "+INF");
      else
	print_generic_expr (file, max ());

      fprintf (file, "]");
    }
  else if (varying_p ())
    {
      print_generic_expr (file, type ());
      fprintf (file, " VARYING");
    }
  else
    gcc_unreachable ();
}

static void
dump_bound_with_infinite_markers (FILE *file, tree bound)
{
  tree type = TREE_TYPE (bound);
  if (INTEGRAL_TYPE_P (type)
      && !TYPE_UNSIGNED (type)
      && vrp_val_is_min (bound)
      && TYPE_PRECISION (type) != 1)
    fprintf (file, "-INF");
  else if (vrp_val_is_max (bound)
	   && TYPE_PRECISION (type) != 1)
    fprintf (file, "+INF");
  else
    print_generic_expr (file, bound);
}

void
vrange::dump (FILE *file) const
{
  const irange *ir = as_a <const irange *> (this);
  if (ir && simple_ranges_p ())
    {
      ir->simple_dump (file);
      return;
    }
  if (undefined_p ())
    {
      fprintf (file, "UNDEFINED");
      return;
    }
  print_generic_expr (file, type ());
  fprintf (file, " ");
  if (varying_p ())
    fprintf (file, "VARYING");
  else if (m_kind == VR_RANGE)
    {
      for (unsigned i = 0; i < m_num_ranges; ++i)
	{
	  tree lb = m_base[i * 2];
	  tree ub = m_base[i * 2 + 1];
	  fprintf (file, "[");
	  dump_bound_with_infinite_markers (file, lb);
	  fprintf (file, ", ");
	  dump_bound_with_infinite_markers (file, ub);
	  fprintf (file, "]");
	}
    }
  else if (m_kind == VR_ANTI_RANGE)
    {
      gcc_checking_assert (m_num_ranges == 1);
      gcc_checking_assert (!range_has_numeric_bounds_p (ir));
      tree lb = m_base[0];
      tree ub = m_base[1];
      fprintf (file, "~[");
      dump_bound_with_infinite_markers (file, lb);
      fprintf (file, ", ");
      dump_bound_with_infinite_markers (file, ub);
      fprintf (file, "]");
    }
  else
    gcc_unreachable ();
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

/* Return the maximum value for TYPE.  */

tree
vrp_val_max (const_tree type)
{
  if (INTEGRAL_TYPE_P (type))
    return TYPE_MAX_VALUE (type);
  if (POINTER_TYPE_P (type))
    {
      wide_int max = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));
      return wide_int_to_tree (const_cast<tree> (type), max);
    }
  return NULL_TREE;
}

/* Return the minimum value for TYPE.  */

tree
vrp_val_min (const_tree type)
{
  if (INTEGRAL_TYPE_P (type))
    return TYPE_MIN_VALUE (type);
  if (POINTER_TYPE_P (type))
    return build_zero_cst (const_cast<tree> (type));
  return NULL_TREE;
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

#define DEFINE_INT_RANGX_GC_STUBS(N)		\
  void						\
  gt_pch_nx (int_range<N> *&x)			\
  {						\
    for (unsigned i = 0; i < N; ++i)		\
      {						\
	gt_pch_nx (x->m_ranges[i * 2]);		\
	gt_pch_nx (x->m_ranges[i * 2 + 1]);	\
      }		  		       		\
  }						\
						\
  void						\
  gt_ggc_mx (int_range<N> *&x)			\
  {	    	       				\
    for (unsigned i = 0; i < N; ++i)		\
      {						\
	  gt_ggc_mx (x->m_ranges[i * 2]);	\
	  gt_ggc_mx (x->m_ranges[i * 2 + 1]);	\
      }						\
  }

#define DEFINE_INT_RANGX_INSTANCE(N)					\
  template int_range<N>::int_range(tree, tree, value_range_kind);	\
  template int_range<N>::int_range(tree_node *,				\
				   const wide_int &,			\
				   const wide_int &,			\
				   value_range_kind);			\
  template int_range<N>::int_range(tree);				\
  template int_range<N>::int_range(const irange &);			\
  template int_range<N>::int_range(const int_range &);			\
  template int_range<N>& int_range<N>::operator= (const int_range &);

DEFINE_INT_RANGX_INSTANCE(1)
DEFINE_INT_RANGX_INSTANCE(2)
DEFINE_INT_RANGX_INSTANCE(3)
DEFINE_INT_RANGX_GC_STUBS(1)
