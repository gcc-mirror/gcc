/* Tree based points-to analysis
   Copyright (C) 2005-2026 Free Software Foundation, Inc.
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

/* NOTE: This file declares the internal interface of the points-to analyzer.
   Outward-facing function declarations can be found in tree-ssa-alias.h.  */

#ifndef TREE_SSA_STRUCTALIAS_H
#define TREE_SSA_STRUCTALIAS_H

namespace pointer_analysis {

enum constraint_expr_type {SCALAR, DEREF, ADDRESSOF};

/* Static IDs for the special variables.  Variable ID zero is unused
   and used as terminator for the sub-variable chain.  */
enum { nothing_id = 1, anything_id = 2, string_id = 3,
       escaped_id = 4, nonlocal_id = 5, escaped_return_id = 6,
       storedanything_id = 7, integer_id = 8 };

/* In IPA mode there are varinfos for different aspects of reach
   function designator.  One for the points-to set of the return
   value, one for the variables that are clobbered by the function,
   one for its uses and one for each parameter (including a single
   glob for remaining variadic arguments).  */

enum { fi_clobbers = 1, fi_uses = 2,
       fi_static_chain = 3, fi_result = 4, fi_parm_base = 5 };

/* Use 0x8000... as special unknown offset.  */
#define UNKNOWN_OFFSET HOST_WIDE_INT_MIN

/* An expression that appears in a constraint.  */

struct constraint_expr
{
  /* Constraint type.  */
  constraint_expr_type type;

  /* Variable we are referring to in the constraint.  */
  unsigned int var;

  /* Offset, in bits, of this constraint from the beginning of
     variables it ends up referring to.

     IOW, in a deref constraint, we would deref, get the result set,
     then add OFFSET to each member.  */
  HOST_WIDE_INT offset;
};
typedef struct constraint_expr ce_s;

/* Our set constraints are made up of two constraint expressions, one
   LHS, and one RHS.

   As described in the introduction in tree-ssa-structalias.cc, our set
   constraints each represent an operation between set valued variables.
*/
struct constraint
{
  struct constraint_expr lhs;
  struct constraint_expr rhs;
};
typedef struct constraint *constraint_t;

struct variable_info
{
  /* ID of this variable.  */
  unsigned int id;

  /* True if this is a variable created by the constraint analysis, such as
     heap variables and constraints we had to break up.  */
  unsigned int is_artificial_var : 1;

  /* True if this is a special variable whose solution set should not be
     changed.  */
  unsigned int is_special_var : 1;

  /* True for variables whose size is not known or variable.  */
  unsigned int is_unknown_size_var : 1;

  /* True for (sub-)fields that represent a whole variable.  */
  unsigned int is_full_var : 1;

  /* True if this is a heap variable.  */
  unsigned int is_heap_var : 1;

  /* True if this is a register variable.  */
  unsigned int is_reg_var : 1;

  /* True if this field may contain pointers.  */
  unsigned int may_have_pointers : 1;

  /* True if this field has only restrict qualified pointers.  */
  unsigned int only_restrict_pointers : 1;

  /* True if this represents a heap var created for a restrict qualified
     pointer.  */
  unsigned int is_restrict_var : 1;

  /* True if this represents a global variable.  */
  unsigned int is_global_var : 1;

  /* True if this represents a module escape point for IPA analysis.  */
  unsigned int is_ipa_escape_point : 1;

  /* True if this represents a IPA function info.  */
  unsigned int is_fn_info : 1;

  /* True if this appears as RHS in a ADDRESSOF constraint.  */
  unsigned int address_taken : 1;

  /* ???  Store somewhere better.  */
  unsigned short ruid;

  /* The ID of the variable for the next field in this structure
     or zero for the last field in this structure.  */
  unsigned next;

  /* The ID of the variable for the first field in this structure.  */
  unsigned head;

  /* Offset of this variable, in bits, from the base variable.  */
  unsigned HOST_WIDE_INT offset;

  /* Size of the variable, in bits.  */
  unsigned HOST_WIDE_INT size;

  /* Full size of the base variable, in bits.  */
  unsigned HOST_WIDE_INT fullsize;

  /* In IPA mode the shadow UID in case the variable needs to be duplicated in
     the final points-to solution because it reaches its containing
     function recursively.  Zero if none is needed.  */
  unsigned int shadow_var_uid;

  /* Name of this variable.  */
  const char *name;

  /* Tree that this variable is associated with.  */
  tree decl;

  /* Points-to set for this variable.  */
  bitmap solution;

  /* Old points-to set for this variable.  */
  bitmap oldsolution;
};
typedef struct variable_info *varinfo_t;

struct constraint_stats
{
  unsigned int total_vars;
  unsigned int nonpointer_vars;
  unsigned int unified_vars_static;
  unsigned int unified_vars_dynamic;
  unsigned int iterations;
  unsigned int num_edges;
  unsigned int num_implicit_edges;
  unsigned int num_avoided_edges;
  unsigned int points_to_sets_created;
};

extern bool use_field_sensitive;
extern int in_ipa_mode;

extern struct constraint_stats stats;

extern bitmap_obstack pta_obstack;
extern bitmap_obstack oldpta_obstack;

extern vec<varinfo_t> varmap;
extern vec<constraint_t> constraints;
extern unsigned int *var_rep;


/* Return the varmap element N.  */

inline varinfo_t
get_varinfo (unsigned int n)
{
  return varmap[n];
}

/* Return the next variable in the list of sub-variables of VI
   or NULL if VI is the last sub-variable.  */

inline varinfo_t
vi_next (varinfo_t vi)
{
  return get_varinfo (vi->next);
}

varinfo_t first_vi_for_offset (varinfo_t start,
			       unsigned HOST_WIDE_INT offset);
varinfo_t first_or_preceding_vi_for_offset (varinfo_t start,
					    unsigned HOST_WIDE_INT offset);
void determine_global_memory_access (gcall *, bool *, bool *, bool *);
bool fndecl_maybe_in_other_partition (tree);
varinfo_t new_var_info (tree t, const char *name, bool add_id);
void dump_constraint (FILE *file, constraint_t c);
void dump_constraints (FILE *file, int from);
void dump_solution_for_var (FILE *file, unsigned int var);
void dump_sa_stats (FILE *outfile);
void dump_sa_points_to_info (FILE *outfile);
void dump_varinfo (FILE *file, varinfo_t vi);
void dump_varmap (FILE *file);
void debug_constraint (constraint_t);
void debug_constraints (void);
void debug_solution_for_var (unsigned int);
void debug_sa_points_to_info (void);
void debug_varinfo (varinfo_t);
void debug_varmap (void);

} // namespace pointer_analysis

#endif /* TREE_SSA_STRUCTALIAS_H  */
