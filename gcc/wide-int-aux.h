/* Header file for wide int auxillary routines.
   Copyright (C) 2018 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef WIDE_INT_AUX_H
#define WIDE_INT_AUX_H


extern bool wide_int_binop (enum tree_code, wide_int& res, const wide_int& arg1,
			    const wide_int& arg2, signop sign, bool& overflow);
extern bool range_binop (enum tree_code code, wide_int& res,
			 const wide_int& arg1, const wide_int& arg2,
			 signop sign, bool& overflow, bool ov_undefined);

extern void choose_min_max (signop s, wide_int& min, wide_int& max,
			    wide_int& w0, wide_int& w1, wide_int& w2,
			    wide_int& w3);
extern bool do_cross_product (enum tree_code code, signop s, wide_int& lb,
			      wide_int& ub, const wide_int& lh_lb,
			      const wide_int& lh_ub, const wide_int& rh_lb,
			      const wide_int& rh_ub);

#endif /* WIDE_INT_AUX_H */
