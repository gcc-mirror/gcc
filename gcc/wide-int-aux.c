/* wide-int routines for trees and ranges.
   Copyright (C) 2018 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

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
#include "insn-codes.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "wide-int.h"
#include "range.h"
#include "range-op.h"
#include "tree-vrp.h"
#include "fold-const.h"

static void
choose_min_max (signop s, wide_int& min, wide_int& max, wide_int& w0,
		wide_int& w1, wide_int& w2, wide_int& w3)

{
  // Order pairs w0,w1  and w2,w3.
  if (wi::gt_p (w0, w1, s))
    std::swap (w0, w1);
  if (wi::gt_p (w2, w3, s))
    std::swap (w2, w3);

  // Then choose min and max from the ordered pairs.
  min = wi::min (w0, w2, s);
  max = wi::max (w1, w3, s);
}

bool
do_cross_product (enum tree_code code, signop s, wide_int& lb, wide_int& ub,
		  const wide_int& lh_lb, const wide_int& lh_ub,
		  const wide_int& rh_lb, const wide_int& rh_ub) 
{
  wi::overflow_type ov;
  wide_int cp1, cp2, cp3, cp4;

  // Compute the 4 cross operations, bailing if an overflow occurs.
  
  if (!wide_int_binop (cp1, code, lh_lb, rh_lb, s, &ov) || ov)
    return false;

  if (wi::eq_p (lh_lb, lh_ub))
    cp3 = cp1;
  else
    if (!wide_int_binop (cp3, code, lh_ub, rh_lb, s, &ov) || ov)
      return false;

  if (wi::eq_p (rh_lb, rh_ub))
    cp2 = cp1;
  else
    if (!wide_int_binop (cp2, code, lh_lb, rh_ub, s, &ov) || ov)
      return false;

  if (wi::eq_p (lh_lb, lh_ub))
    cp4 = cp2;
  else
    if (!wide_int_binop (cp4, code, lh_ub, rh_ub, s, &ov) || ov)
      return false;

  // Order properly and add to the range.
  choose_min_max (s, lb, ub, cp1, cp2, cp3, cp4);
  return true;
}
