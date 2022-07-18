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
 for (unsigned i = 0; i < r.num_pairs (); ++i)
    {
      tree lb = wide_int_to_tree (r.type (), r.lower_bound (i));
      tree ub = wide_int_to_tree (r.type (), r.upper_bound (i));
      pp_character (pp, '[');
      print_irange_bound (lb);
      pp_string (pp, ", ");
      print_irange_bound (ub);
      pp_character (pp, ']');
    }
 print_irange_bitmasks (r);
}

void
vrange_printer::print_irange_bound (tree bound) const
{
  tree type = TREE_TYPE (bound);
  wide_int type_min = wi::min_value (TYPE_PRECISION (type), TYPE_SIGN (type));
  wide_int type_max = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));

  if (INTEGRAL_TYPE_P (type)
      && !TYPE_UNSIGNED (type)
      && TREE_CODE (bound) == INTEGER_CST
      && wi::to_wide (bound) == type_min
      && TYPE_PRECISION (type) != 1)
    pp_string (pp, "-INF");
  else if (TREE_CODE (bound) == INTEGER_CST
	   && wi::to_wide (bound) == type_max
	   && TYPE_PRECISION (type) != 1)
    pp_string (pp, "+INF");
  else
    dump_generic_node (pp, bound, 0, TDF_NONE, false);
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
