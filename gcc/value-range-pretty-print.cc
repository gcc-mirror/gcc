/* Pretty print support for value ranges.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
#include "value-range-pretty-print.h"

void
vrange_printer::visit (const unsupported_range &r) const
{
  pp_string (pp, "[unsupported_range] ");
  if (r.undefined_p ())
    {
      pp_string (pp, "UNDEFINED");
      return;
    }
  if (r.varying_p ())
    {
      pp_string (pp, "VARYING");
      return;
    }
  gcc_unreachable ();
}

void
vrange_printer::visit (const irange &r) const
{
  pp_string (pp, "[irange] ");
  if (r.undefined_p ())
    {
      pp_string (pp, "UNDEFINED");
      return;
    }
  dump_generic_node (pp, r.type (), 0, TDF_NONE | TDF_NOUID, false);
  pp_character (pp, ' ');
  if (r.varying_p ())
    {
      pp_string (pp, "VARYING");
      return;
    }
  for (unsigned i = 0; i < r.num_pairs (); ++i)
    {
      pp_character (pp, '[');
      print_irange_bound (r.lower_bound (i), r.type ());
      pp_string (pp, ", ");
      print_irange_bound (r.upper_bound (i), r.type ());
      pp_character (pp, ']');
    }
 print_irange_bitmasks (r);
}

void
vrange_printer::print_irange_bound (const wide_int &bound, tree type) const
{
  wide_int type_min = wi::min_value (TYPE_PRECISION (type), TYPE_SIGN (type));
  wide_int type_max = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));

  if (INTEGRAL_TYPE_P (type)
      && !TYPE_UNSIGNED (type)
      && bound == type_min
      && TYPE_PRECISION (type) != 1)
    pp_string (pp, "-INF");
  else if (bound == type_max && TYPE_PRECISION (type) != 1)
    pp_string (pp, "+INF");
  else
    pp_wide_int (pp, bound, TYPE_SIGN (type));
}

void
vrange_printer::print_irange_bitmasks (const irange &r) const
{
  irange_bitmask bm = r.m_bitmask;
  if (bm.unknown_p ())
    return;

  pp_string (pp, " MASK ");
  char buf[WIDE_INT_PRINT_BUFFER_SIZE], *p;
  unsigned len_mask, len_val;
  if (print_hex_buf_size (bm.mask (), &len_mask)
      | print_hex_buf_size (bm.value (), &len_val))
    p = XALLOCAVEC (char, MAX (len_mask, len_val));
  else
    p = buf;
  print_hex (bm.mask (), p);
  pp_string (pp, p);
  pp_string (pp, " VALUE ");
  print_hex (bm.value (), p);
  pp_string (pp, p);
}

void
vrange_printer::print_real_value (tree type, const REAL_VALUE_TYPE &r) const
{
  char s[100];
  real_to_decimal_for_mode (s, &r, sizeof (s), 0, 1, TYPE_MODE (type));
  pp_string (pp, s);
  if (!DECIMAL_FLOAT_TYPE_P (type)
      // real_to_hexadecimal prints infinities and NAN as text.  No
      // need to print them twice.
      && !real_isinf (&r)
      && !real_isnan (&r))
    {
      real_to_hexadecimal (s, &r, sizeof (s), 0, 1);
      pp_printf (pp, " (%s)", s);
    }
}

// Print an frange.

void
vrange_printer::visit (const frange &r) const
{
  pp_string (pp, "[frange] ");
  if (r.undefined_p ())
    {
      pp_string (pp, "UNDEFINED");
      return;
    }
  tree type = r.type ();
  dump_generic_node (pp, type, 0, TDF_NONE, false);
  pp_string (pp, " ");
  if (r.varying_p ())
    {
      pp_string (pp, "VARYING");
      print_frange_nan (r);
      return;
    }
  pp_character (pp, '[');
  bool has_endpoints = !r.known_isnan ();
  if (has_endpoints)
    {
      print_real_value (type, r.lower_bound ());
      pp_string (pp, ", ");
      print_real_value (type, r.upper_bound ());
    }
  pp_character (pp, ']');
  print_frange_nan (r);
}

// Print the NAN info for an frange.

void
vrange_printer::print_frange_nan (const frange &r) const
{
  if (r.maybe_isnan ())
    {
      if (r.m_pos_nan && r.m_neg_nan)
	{
	  pp_string (pp, " +-NAN");
	  return;
	}
      bool nan_sign = r.m_neg_nan;
      if (nan_sign)
	pp_string (pp, " -NAN");
      else
	pp_string (pp, " +NAN");
    }
}
