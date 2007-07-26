/* Tree based points-to analysis
   Copyright (C) 2002, 2003, 2007 Free Software Foundation, Inc.
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

/* True if the data pointed to by PTR can alias anything.  */
#define PTR_IS_REF_ALL(PTR) TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (PTR))

struct constraint;
typedef struct constraint *constraint_t;

/* Alias information used by compute_may_aliases and its helpers.  */
struct alias_info
{
  /* SSA names visited while collecting points-to information.  If bit I
     is set, it means that SSA variable with version I has already been
     visited.  */
  sbitmap ssa_names_visited;

  /* Array of SSA_NAME pointers processed by the points-to collector.  */
  VEC(tree,heap) *processed_ptrs;

  /* ADDRESSABLE_VARS contains all the global variables and locals that
     have had their address taken.  */
  struct alias_map_d **addressable_vars;
  size_t num_addressable_vars;

  /* POINTERS contains all the _DECL pointers with unique memory tags
     that have been referenced in the program.  */
  struct alias_map_d **pointers;
  size_t num_pointers;

  /* Variables that have been written to directly (i.e., not through a
     pointer dereference).  */
  struct pointer_set_t *written_vars;

  /* Pointers that have been used in an indirect store operation.  */
  struct pointer_set_t *dereferenced_ptrs_store;

  /* Pointers that have been used in an indirect load operation.  */
  struct pointer_set_t *dereferenced_ptrs_load;

  /* Memory tag for all the PTR_IS_REF_ALL pointers.  */
  tree ref_all_symbol_mem_tag;
};

/* In tree-ssa-alias.c.  */
enum escape_type is_escape_site (tree);
void update_mem_sym_stats_from_stmt (tree, tree, long, long);

/* In tree-ssa-structalias.c.  */
extern void compute_points_to_sets (struct alias_info *);
extern void delete_points_to_sets (void);
extern void dump_constraint (FILE *, constraint_t);
extern void dump_constraints (FILE *);
extern void debug_constraint (constraint_t);
extern void debug_constraints (void);
extern void dump_solution_for_var (FILE *, unsigned int);
extern void debug_solution_for_var (unsigned int);
extern void dump_sa_points_to_info (FILE *);
extern void debug_sa_points_to_info (void);
extern void set_used_smts (void);

#endif /* TREE_SSA_STRUCTALIAS_H  */
