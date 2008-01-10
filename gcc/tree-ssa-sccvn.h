/* Tree SCC value numbering
   Copyright (C) 2007 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef TREE_SSA_SCCVN_H
#define TREE_SSA_SCCVN_H

/* TOP of the VN lattice.  */
extern tree VN_TOP;

typedef struct vn_ssa_aux
{
  /* Value number. This may be an SSA name or a constant.  */
  tree valnum;
  /* Representative expression, if not a direct constant. */
  tree expr;

  /* SCC information.  */
  unsigned int dfsnum;
  unsigned int low;
  unsigned visited : 1;
  unsigned on_sccstack : 1;

  /* Whether the representative expression contains constants.  */
  unsigned has_constants : 1;
  /* Whether the SSA_NAME has been value numbered already.  This is
     only saying whether visit_use has been called on it at least
     once.  It cannot be used to avoid visitation for SSA_NAME's
     involved in non-singleton SCC's.  */
  unsigned use_processed : 1;
} *vn_ssa_aux_t;

/* Return the value numbering info for an SSA_NAME.  */
extern vn_ssa_aux_t VN_INFO (tree);
extern vn_ssa_aux_t VN_INFO_GET (tree);
bool run_scc_vn (void);
void free_scc_vn (void);
void switch_to_PRE_table (void);
tree vn_binary_op_lookup (tree);
void vn_binary_op_insert (tree, tree);
tree vn_unary_op_lookup (tree);
void vn_unary_op_insert (tree, tree);
tree vn_reference_lookup (tree, VEC (tree, gc) *);
void vn_reference_insert (tree, tree, VEC (tree, gc) *);
VEC (tree, gc) *shared_vuses_from_stmt (tree);
VEC (tree, gc) *copy_vuses_from_stmt (tree);


#endif /* TREE_SSA_SCCVN_H  */
