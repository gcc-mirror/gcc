/* SSA name expresssons routines
   Copyright (C) 2013-2022 Free Software Foundation, Inc.

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

#ifndef GCC_TREE_SSANAMES_H
#define GCC_TREE_SSANAMES_H

/* Aliasing information for SSA_NAMEs representing pointer variables.  */

struct GTY(()) ptr_info_def
{
  /* The points-to solution.  */
  struct pt_solution pt;

  /* Alignment and misalignment of the pointer in bytes.  Together
     align and misalign specify low known bits of the pointer.
     ptr & (align - 1) == misalign.  */

  /* When known, this is the power-of-two byte alignment of the object this
     pointer points into.  This is usually DECL_ALIGN_UNIT for decls and
     MALLOC_ABI_ALIGNMENT for allocated storage.  When the alignment is not
     known, it is zero.  Do not access directly but use functions
     get_ptr_info_alignment, set_ptr_info_alignment,
     mark_ptr_info_alignment_unknown and similar.  */
  unsigned int align;

  /* When alignment is known, the byte offset this pointer differs from the
     above alignment.  Access only through the same helper functions as align
     above.  */
  unsigned int misalign;
};


#define SSANAMES(fun) (fun)->gimple_df->ssa_names
#define DEFAULT_DEFS(fun) (fun)->gimple_df->default_defs

#define num_ssa_names (vec_safe_length (cfun->gimple_df->ssa_names))
#define ssa_name(i) ((*cfun->gimple_df->ssa_names)[(i)])

#define FOR_EACH_SSA_NAME(I, VAR, FN)					\
  for (I = 1; SSANAMES (FN)->iterate (I, &VAR); ++I)			\
    if (VAR)

/* Sets the value range to SSA.  */
extern bool set_range_info (tree, const vrange &);
extern void set_nonzero_bits (tree, const wide_int_ref &);
extern wide_int get_nonzero_bits (const_tree);
extern bool ssa_name_has_boolean_range (tree);
extern void init_ssanames (struct function *, int);
extern void fini_ssanames (struct function *);
extern void ssanames_print_statistics (void);
extern tree make_ssa_name_fn (struct function *, tree, gimple *,
			      unsigned int version = 0);
extern void init_ssa_name_imm_use (tree);
extern void release_ssa_name_fn (struct function *, tree);
extern bool get_ptr_info_alignment (struct ptr_info_def *, unsigned int *,
				    unsigned int *);
extern void mark_ptr_info_alignment_unknown (struct ptr_info_def *);
extern void set_ptr_info_alignment (struct ptr_info_def *, unsigned int,
				    unsigned int);
extern void adjust_ptr_info_misalignment (struct ptr_info_def *, poly_uint64);
extern struct ptr_info_def *get_ptr_info (tree);
extern void set_ptr_nonnull (tree);

extern tree copy_ssa_name_fn (struct function *, tree, gimple *);
extern void duplicate_ssa_name_ptr_info (tree, struct ptr_info_def *);
extern tree duplicate_ssa_name_fn (struct function *, tree, gimple *);
extern void duplicate_ssa_name_range_info (tree dest, tree src);
extern void reset_flow_sensitive_info (tree);
extern void reset_flow_sensitive_info_in_bb (basic_block);
extern void release_defs (gimple *);
extern void replace_ssa_name_symbol (tree, tree);
extern void flush_ssaname_freelist (void);


/* Return an SSA_NAME node for variable VAR defined in statement STMT
   in function cfun.  */

static inline tree
make_ssa_name (tree var, gimple *stmt = NULL)
{
  return make_ssa_name_fn (cfun, var, stmt);
}

/* Return an SSA_NAME node using the template SSA name NAME defined in
   statement STMT in function cfun.  */

static inline tree
copy_ssa_name (tree var, gimple *stmt = NULL)
{
  return copy_ssa_name_fn (cfun, var, stmt);
}

/*  Creates a duplicate of a SSA name NAME tobe defined by statement STMT
    in function cfun.  */

static inline tree
duplicate_ssa_name (tree var, gimple *stmt)
{
  return duplicate_ssa_name_fn (cfun, var, stmt);
}

/* Release the SSA name NAME used in function cfun.  */

static inline void
release_ssa_name (tree name)
{
  release_ssa_name_fn (cfun, name);
}

/* Return an anonymous SSA_NAME node for type TYPE defined in statement STMT
   in function cfun.  Arrange so that it uses NAME in dumps.  */

static inline tree
make_temp_ssa_name (tree type, gimple *stmt, const char *name)
{
  tree ssa_name;
  gcc_checking_assert (TYPE_P (type));
  ssa_name = make_ssa_name_fn (cfun, type, stmt);
  SET_SSA_NAME_VAR_OR_IDENTIFIER (ssa_name, get_identifier (name));
  return ssa_name;
}


#endif /* GCC_TREE_SSANAMES_H */
