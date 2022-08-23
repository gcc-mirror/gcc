/* Pretty print support for value ranges.
   Copyright (C) 2019-2022 Free Software Foundation, Inc.
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
  dump_generic_node (pp, r.type (), 0, TDF_NONE, false);
  pp_character (pp, ' ');
  if (r.varying_p ())
    {
      pp_string (pp, "VARYING");
      return;
    }
  // Handle legacy symbolics.
  if (!r.constant_p ())
    {
      if (r.kind () == VR_ANTI_RANGE)
	pp_character (pp, '~');
      pp_character (pp, '[');
      dump_generic_node (pp, r.min (), 0, TDF_NONE, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, r.max (), 0, TDF_NONE, false);
      pp_character (pp, ']');
      print_irange_bitmasks (r);
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
  wide_int nz = r.get_nonzero_bits ();
  if (nz == -1)
    return;

  pp_string (pp, " NONZERO ");
  char buf[WIDE_INT_PRINT_BUFFER_SIZE];
  print_hex (nz, buf);
  pp_string (pp, buf);
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
  dump_generic_node (pp, r.type (), 0, TDF_NONE, false);
  pp_string (pp, " ");
  if (r.varying_p ())
    {
      pp_string (pp, "VARYING");
      return;
    }
  print_frange_prop ("NAN", r.get_nan ());
  print_frange_prop ("INF", r.get_inf ());
  print_frange_prop ("NINF", r.get_ninf ());
}

// Print the FP properties in an frange.

void
vrange_printer::print_frange_prop (const char *str, const fp_prop &prop) const
{
  if (prop.varying_p ())
    return;

  if (prop.yes_p ())
    pp_string (pp, str);
  else if (prop.no_p ())
    {
      pp_character (pp, '!');
      pp_string (pp, str);
    }
  pp_character (pp, ' ');
}
