/* Declarations of tree-ssa-strlen API.

   Copyright (C) 2018-2020 Free Software Foundation, Inc.

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

#ifndef GCC_TREE_SSA_STRLEN_H
#define GCC_TREE_SSA_STRLEN_H

extern bool is_strlen_related_p (tree, tree);
extern bool maybe_diag_stxncpy_trunc (gimple_stmt_iterator, tree, tree);
extern tree set_strlen_range (tree, wide_int, wide_int, tree = NULL_TREE);

class vr_values;
extern tree get_range (tree, wide_int[2], const vr_values * = NULL);

struct c_strlen_data;
extern void get_range_strlen_dynamic (tree , c_strlen_data *, const vr_values *);

/* APIs internal to strlen pass.  Defined in gimple-ssa-sprintf.c.  */
extern bool handle_printf_call (gimple_stmt_iterator *,  const vr_values *);

#endif   // GCC_TREE_SSA_STRLEN_H
