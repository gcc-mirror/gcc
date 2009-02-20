/* Tree based points-to analysis
   Copyright (C) 2002, 2003, 2007, 2008 Free Software Foundation, Inc.
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

#ifndef TREE_SSA_STRUCTALIAS_H
#define TREE_SSA_STRUCTALIAS_H

struct constraint;
typedef struct constraint *constraint_t;

/* In tree-ssa-alias.c.  */
enum escape_type is_escape_site (gimple);
void update_mem_sym_stats_from_stmt (tree, gimple, long, long);

/* In tree-ssa-structalias.c.  */
extern void compute_points_to_sets (void);
extern void delete_points_to_sets (void);
extern void dump_constraint (FILE *, constraint_t);
extern void dump_constraint_edge (FILE *, constraint_t);
extern void dump_constraints (FILE *);
extern void dump_constraint_graph (FILE *);
extern void debug_constraint (constraint_t);
extern void debug_constraints (void);
extern void debug_constraint_graph (void);
extern void dump_solution_for_var (FILE *, unsigned int);
extern void debug_solution_for_var (unsigned int);
extern void dump_sa_points_to_info (FILE *);
extern void debug_sa_points_to_info (void);

#endif /* TREE_SSA_STRUCTALIAS_H  */
