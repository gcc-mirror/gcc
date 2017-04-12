/* Routines for reading trees from a file stream.

   Copyright (C) 2011-2017 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@google.com>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "stringpool.h"
#include "tree-streamer.h"
#include "cgraph.h"
#include "builtins.h"
#include "ipa-chkp.h"
#include "gomp-constants.h"
#include "asan.h"


/* Read a STRING_CST from the string table in DATA_IN using input
   block IB.  */

tree
streamer_read_string_cst (struct data_in *data_in, struct lto_input_block *ib)
{
  unsigned int len;
  const char * ptr;

  ptr = streamer_read_indexed_string (data_in, ib, &len);
  if (!ptr)
    return NULL;
  return build_string (len, ptr);
}


/* Read an IDENTIFIER from the string table in DATA_IN using input
   block IB.  */

static tree
input_identifier (struct data_in *data_in, struct lto_input_block *ib)
{
  unsigned int len;
  const char *ptr;

  ptr = streamer_read_indexed_string (data_in, ib, &len);
  if (!ptr)
    return NULL;
  return get_identifier_with_length (ptr, len);
}


/* Read a chain of tree nodes from input block IB. DATA_IN contains
   tables and descriptors for the file being read.  */

tree
streamer_read_chain (struct lto_input_block *ib, struct data_in *data_in)
{
  tree first, prev, curr;

  /* The chain is written as NULL terminated list of trees.  */
  first = prev = NULL_TREE;
  do
    {
      curr = stream_read_tree (ib, data_in);
      if (prev)
	TREE_CHAIN (prev) = curr;
      else
	first = curr;

      prev = curr;
    }
  while (curr);

  return first;
}


/* Unpack all the non-pointer fields of the TS_BASE structure of
   expression EXPR from bitpack BP.  */

static inline void
unpack_ts_base_value_fields (struct bitpack_d *bp, tree expr)
{
  /* Note that the code for EXPR has already been unpacked to create EXPR in
     streamer_alloc_tree.  */
  if (!TYPE_P (expr))
    {
      TREE_SIDE_EFFECTS (expr) = (unsigned) bp_unpack_value (bp, 1);
      TREE_CONSTANT (expr) = (unsigned) bp_unpack_value (bp, 1);
      TREE_READONLY (expr) = (unsigned) bp_unpack_value (bp, 1);

      /* TREE_PUBLIC is used on types to indicate that the type
	 has a TYPE_CACHED_VALUES vector.  This is not streamed out,
	 so we skip it here.  */
      TREE_PUBLIC (expr) = (unsigned) bp_unpack_value (bp, 1);
    }
  else
    bp_unpack_value (bp, 4);
  TREE_ADDRESSABLE (expr) = (unsigned) bp_unpack_value (bp, 1);
  TREE_THIS_VOLATILE (expr) = (unsigned) bp_unpack_value (bp, 1);
  if (DECL_P (expr))
    {
      DECL_UNSIGNED (expr) = (unsigned) bp_unpack_value (bp, 1);
      DECL_NAMELESS (expr) = (unsigned) bp_unpack_value (bp, 1);
    }
  else if (TYPE_P (expr))
    TYPE_UNSIGNED (expr) = (unsigned) bp_unpack_value (bp, 1);
  else
    bp_unpack_value (bp, 1);
  TREE_ASM_WRITTEN (expr) = (unsigned) bp_unpack_value (bp, 1);
  if (TYPE_P (expr))
    TYPE_ARTIFICIAL (expr) = (unsigned) bp_unpack_value (bp, 1);
  else
    TREE_NO_WARNING (expr) = (unsigned) bp_unpack_value (bp, 1);
  TREE_NOTHROW (expr) = (unsigned) bp_unpack_value (bp, 1);
  TREE_STATIC (expr) = (unsigned) bp_unpack_value (bp, 1);
  if (TREE_CODE (expr) != TREE_BINFO)
    TREE_PRIVATE (expr) = (unsigned) bp_unpack_value (bp, 1);
  else
    bp_unpack_value (bp, 1);
  TREE_PROTECTED (expr) = (unsigned) bp_unpack_value (bp, 1);
  TREE_DEPRECATED (expr) = (unsigned) bp_unpack_value (bp, 1);
  if (TYPE_P (expr))
    {
      if (AGGREGATE_TYPE_P (expr))
	TYPE_REVERSE_STORAGE_ORDER (expr) = (unsigned) bp_unpack_value (bp, 1);
      else
	TYPE_SATURATING (expr) = (unsigned) bp_unpack_value (bp, 1);
      TYPE_ADDR_SPACE (expr) = (unsigned) bp_unpack_value (bp, 8);
    }
  else if (TREE_CODE (expr) == BIT_FIELD_REF || TREE_CODE (expr) == MEM_REF)
    {
      REF_REVERSE_STORAGE_ORDER (expr) = (unsigned) bp_unpack_value (bp, 1);
      bp_unpack_value (bp, 8);
    }
  else if (TREE_CODE (expr) == SSA_NAME)
    {
      SSA_NAME_IS_DEFAULT_DEF (expr) = (unsigned) bp_unpack_value (bp, 1);
      bp_unpack_value (bp, 8);
    }
  else
    bp_unpack_value (bp, 9);
}


/* Unpack all the non-pointer fields of the TS_INT_CST structure of
   expression EXPR from bitpack BP.  */

static void
unpack_ts_int_cst_value_fields (struct bitpack_d *bp, tree expr)
{
  int i;
  for (i = 0; i < TREE_INT_CST_EXT_NUNITS (expr); i++)
    TREE_INT_CST_ELT (expr, i) = bp_unpack_var_len_int (bp);
}


/* Unpack all the non-pointer fields of the TS_REAL_CST structure of
   expression EXPR from bitpack BP.  */

static void
unpack_ts_real_cst_value_fields (struct bitpack_d *bp, tree expr)
{
  unsigned i;
  REAL_VALUE_TYPE r;
  REAL_VALUE_TYPE *rp;

  /* Clear all bits of the real value type so that we can later do
     bitwise comparisons to see if two values are the same.  */
  memset (&r, 0, sizeof r);
  r.cl = (unsigned) bp_unpack_value (bp, 2);
  r.decimal = (unsigned) bp_unpack_value (bp, 1);
  r.sign = (unsigned) bp_unpack_value (bp, 1);
  r.signalling = (unsigned) bp_unpack_value (bp, 1);
  r.canonical = (unsigned) bp_unpack_value (bp, 1);
  r.uexp = (unsigned) bp_unpack_value (bp, EXP_BITS);
  for (i = 0; i < SIGSZ; i++)
    r.sig[i] = (unsigned long) bp_unpack_value (bp, HOST_BITS_PER_LONG);

  rp = ggc_alloc<real_value> ();
  memcpy (rp, &r, sizeof (REAL_VALUE_TYPE));
  TREE_REAL_CST_PTR (expr) = rp;
}


/* Unpack all the non-pointer fields of the TS_FIXED_CST structure of
   expression EXPR from bitpack BP.  */

static void
unpack_ts_fixed_cst_value_fields (struct bitpack_d *bp, tree expr)
{
  FIXED_VALUE_TYPE *fp = ggc_alloc<fixed_value> ();
  fp->mode = bp_unpack_machine_mode (bp);
  fp->data.low = bp_unpack_var_len_int (bp);
  fp->data.high = bp_unpack_var_len_int (bp);
  TREE_FIXED_CST_PTR (expr) = fp;
}

/* Unpack all the non-pointer fields of the TS_DECL_COMMON structure
   of expression EXPR from bitpack BP.  */

static void
unpack_ts_decl_common_value_fields (struct bitpack_d *bp, tree expr)
{
  SET_DECL_MODE (expr, bp_unpack_machine_mode (bp));
  DECL_NONLOCAL (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_VIRTUAL_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_IGNORED_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_ABSTRACT_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_ARTIFICIAL (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_USER_ALIGN (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_PRESERVE_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_EXTERNAL (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_GIMPLE_REG_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  SET_DECL_ALIGN (expr, (unsigned) bp_unpack_var_len_unsigned (bp));
#ifdef ACCEL_COMPILER
  if (DECL_ALIGN (expr) > targetm.absolute_biggest_alignment)
    SET_DECL_ALIGN (expr, targetm.absolute_biggest_alignment);
#endif
  if (TREE_CODE (expr) == LABEL_DECL)
    {
      EH_LANDING_PAD_NR (expr) = (int) bp_unpack_var_len_unsigned (bp);

      /* Always assume an initial value of -1 for LABEL_DECL_UID to
	 force gimple_set_bb to recreate label_to_block_map.  */
      LABEL_DECL_UID (expr) = -1;
    }

  if (TREE_CODE (expr) == FIELD_DECL)
    {
      DECL_PACKED (expr) = (unsigned) bp_unpack_value (bp, 1);
      DECL_NONADDRESSABLE_P (expr) = (unsigned) bp_unpack_value (bp, 1);
      expr->decl_common.off_align = bp_unpack_value (bp, 8);
    }

  if (VAR_P (expr))
    {
      DECL_HAS_DEBUG_EXPR_P (expr) = (unsigned) bp_unpack_value (bp, 1);
      DECL_NONLOCAL_FRAME (expr) = (unsigned) bp_unpack_value (bp, 1);
    }

  if (TREE_CODE (expr) == RESULT_DECL
      || TREE_CODE (expr) == PARM_DECL
      || VAR_P (expr))
    {
      DECL_BY_REFERENCE (expr) = (unsigned) bp_unpack_value (bp, 1);
      if (VAR_P (expr) || TREE_CODE (expr) == PARM_DECL)
	DECL_HAS_VALUE_EXPR_P (expr) = (unsigned) bp_unpack_value (bp, 1);
    }
}


/* Unpack all the non-pointer fields of the TS_DECL_WRTL structure
   of expression EXPR from bitpack BP.  */

static void
unpack_ts_decl_wrtl_value_fields (struct bitpack_d *bp, tree expr)
{
  DECL_REGISTER (expr) = (unsigned) bp_unpack_value (bp, 1);
}


/* Unpack all the non-pointer fields of the TS_DECL_WITH_VIS structure
   of expression EXPR from bitpack BP.  */

static void
unpack_ts_decl_with_vis_value_fields (struct bitpack_d *bp, tree expr)
{
  DECL_COMMON (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_DLLIMPORT_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_WEAK (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_SEEN_IN_BIND_EXPR_P (expr) = (unsigned) bp_unpack_value (bp,  1);
  DECL_COMDAT (expr) = (unsigned) bp_unpack_value (bp,  1);
  DECL_VISIBILITY (expr) = (enum symbol_visibility) bp_unpack_value (bp,  2);
  DECL_VISIBILITY_SPECIFIED (expr) = (unsigned) bp_unpack_value (bp,  1);

  if (VAR_P (expr))
    {
      DECL_HARD_REGISTER (expr) = (unsigned) bp_unpack_value (bp, 1);
      DECL_IN_CONSTANT_POOL (expr) = (unsigned) bp_unpack_value (bp, 1);
    }

  if (TREE_CODE (expr) == FUNCTION_DECL)
    {
      DECL_FINAL_P (expr) = (unsigned) bp_unpack_value (bp, 1);
      DECL_CXX_CONSTRUCTOR_P (expr) = (unsigned) bp_unpack_value (bp, 1);
      DECL_CXX_DESTRUCTOR_P (expr) = (unsigned) bp_unpack_value (bp, 1);
    }
}


/* Unpack all the non-pointer fields of the TS_FUNCTION_DECL structure
   of expression EXPR from bitpack BP.  */

static void
unpack_ts_function_decl_value_fields (struct bitpack_d *bp, tree expr)
{
  DECL_BUILT_IN_CLASS (expr) = bp_unpack_enum (bp, built_in_class,
					       BUILT_IN_LAST);
  DECL_STATIC_CONSTRUCTOR (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_STATIC_DESTRUCTOR (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_UNINLINABLE (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_POSSIBLY_INLINED (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_IS_NOVOPS (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_IS_RETURNS_TWICE (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_IS_MALLOC (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_IS_OPERATOR_NEW (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_DECLARED_INLINE_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_STATIC_CHAIN (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_NO_INLINE_WARNING_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (expr)
    			= (unsigned) bp_unpack_value (bp, 1);
  DECL_NO_LIMIT_STACK (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_DISREGARD_INLINE_LIMITS (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_PURE_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  DECL_LOOPING_CONST_OR_PURE_P (expr) = (unsigned) bp_unpack_value (bp, 1);
  if (DECL_BUILT_IN_CLASS (expr) != NOT_BUILT_IN)
    {
      DECL_FUNCTION_CODE (expr) = (enum built_in_function) bp_unpack_value (bp,
	                                                                    12);
      if (DECL_BUILT_IN_CLASS (expr) == BUILT_IN_NORMAL
	  && DECL_FUNCTION_CODE (expr) >= END_BUILTINS)
	fatal_error (input_location,
		     "machine independent builtin code out of range");
      else if (DECL_BUILT_IN_CLASS (expr) == BUILT_IN_MD)
	{
          tree result = targetm.builtin_decl (DECL_FUNCTION_CODE (expr), true);
	  if (!result || result == error_mark_node)
	    fatal_error (input_location,
			 "target specific builtin not available");
	}
    }
}


/* Unpack all the non-pointer fields of the TS_TYPE_COMMON structure
   of expression EXPR from bitpack BP.  */

static void
unpack_ts_type_common_value_fields (struct bitpack_d *bp, tree expr)
{
  machine_mode mode;

  mode = bp_unpack_machine_mode (bp);
  SET_TYPE_MODE (expr, mode);
  TYPE_STRING_FLAG (expr) = (unsigned) bp_unpack_value (bp, 1);
  /* TYPE_NO_FORCE_BLK is private to stor-layout and need
     no streaming.  */
  TYPE_NEEDS_CONSTRUCTING (expr) = (unsigned) bp_unpack_value (bp, 1);
  TYPE_PACKED (expr) = (unsigned) bp_unpack_value (bp, 1);
  TYPE_RESTRICT (expr) = (unsigned) bp_unpack_value (bp, 1);
  TYPE_USER_ALIGN (expr) = (unsigned) bp_unpack_value (bp, 1);
  TYPE_READONLY (expr) = (unsigned) bp_unpack_value (bp, 1);
  if (RECORD_OR_UNION_TYPE_P (expr))
    {
      TYPE_TRANSPARENT_AGGR (expr) = (unsigned) bp_unpack_value (bp, 1);
      TYPE_FINAL_P (expr) = (unsigned) bp_unpack_value (bp, 1);
    }
  else if (TREE_CODE (expr) == ARRAY_TYPE)
    TYPE_NONALIASED_COMPONENT (expr) = (unsigned) bp_unpack_value (bp, 1);
  if (AGGREGATE_TYPE_P (expr))
    TYPE_TYPELESS_STORAGE (expr) = (unsigned) bp_unpack_value (bp, 1);
  TYPE_PRECISION (expr) = bp_unpack_var_len_unsigned (bp);
  SET_TYPE_ALIGN (expr, bp_unpack_var_len_unsigned (bp));
#ifdef ACCEL_COMPILER
  if (TYPE_ALIGN (expr) > targetm.absolute_biggest_alignment)
    SET_TYPE_ALIGN (expr, targetm.absolute_biggest_alignment);
#endif
}


/* Unpack all the non-pointer fields of the TS_BLOCK structure
   of expression EXPR from bitpack BP.  */

static void
unpack_ts_block_value_fields (struct data_in *data_in,
			      struct bitpack_d *bp, tree expr)
{
  BLOCK_ABSTRACT (expr) = (unsigned) bp_unpack_value (bp, 1);
  /* BLOCK_NUMBER is recomputed.  */
  stream_input_location (&BLOCK_SOURCE_LOCATION (expr), bp, data_in);
}

/* Unpack all the non-pointer fields of the TS_TRANSLATION_UNIT_DECL
   structure of expression EXPR from bitpack BP.  */

static void
unpack_ts_translation_unit_decl_value_fields (struct data_in *data_in,
					      struct bitpack_d *bp, tree expr)
{
  TRANSLATION_UNIT_LANGUAGE (expr) = xstrdup (bp_unpack_string (data_in, bp));
  vec_safe_push (all_translation_units, expr);
}


/* Unpack all the non-pointer fields of the TS_OMP_CLAUSE
   structure of expression EXPR from bitpack BP.  */

static void
unpack_ts_omp_clause_value_fields (struct data_in *data_in,
				   struct bitpack_d *bp, tree expr)
{
  stream_input_location (&OMP_CLAUSE_LOCATION (expr), bp, data_in);
  switch (OMP_CLAUSE_CODE (expr))
    {
    case OMP_CLAUSE_DEFAULT:
      OMP_CLAUSE_DEFAULT_KIND (expr)
	= bp_unpack_enum (bp, omp_clause_default_kind,
			  OMP_CLAUSE_DEFAULT_LAST);
      break;
    case OMP_CLAUSE_SCHEDULE:
      OMP_CLAUSE_SCHEDULE_KIND (expr)
	= bp_unpack_enum (bp, omp_clause_schedule_kind,
			  OMP_CLAUSE_SCHEDULE_LAST);
      break;
    case OMP_CLAUSE_DEPEND:
      OMP_CLAUSE_DEPEND_KIND (expr)
	= bp_unpack_enum (bp, omp_clause_depend_kind, OMP_CLAUSE_DEPEND_LAST);
      break;
    case OMP_CLAUSE_MAP:
      OMP_CLAUSE_SET_MAP_KIND (expr, bp_unpack_enum (bp, gomp_map_kind,
						     GOMP_MAP_LAST));
      break;
    case OMP_CLAUSE_PROC_BIND:
      OMP_CLAUSE_PROC_BIND_KIND (expr)
	= bp_unpack_enum (bp, omp_clause_proc_bind_kind,
			  OMP_CLAUSE_PROC_BIND_LAST);
      break;
    case OMP_CLAUSE_REDUCTION:
      OMP_CLAUSE_REDUCTION_CODE (expr)
	= bp_unpack_enum (bp, tree_code, MAX_TREE_CODES);
      break;
    default:
      break;
    }
}


/* Read all the language-independent bitfield values for EXPR from IB.
   Return the partially unpacked bitpack so the caller can unpack any other
   bitfield values that the writer may have written.  */

void
streamer_read_tree_bitfields (struct lto_input_block *ib,
			      struct data_in *data_in, tree expr)
{
  enum tree_code code;
  struct bitpack_d bp;

  /* Read the bitpack of non-pointer values from IB.  */
  bp = streamer_read_bitpack (ib);

  /* The first word in BP contains the code of the tree that we
     are about to read.  */
  code = (enum tree_code) bp_unpack_value (&bp, 16);
  lto_tag_check (lto_tree_code_to_tag (code),
		 lto_tree_code_to_tag (TREE_CODE (expr)));

  /* Note that all these functions are highly sensitive to changes in
     the types and sizes of each of the fields being packed.  */
  unpack_ts_base_value_fields (&bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_INT_CST))
    unpack_ts_int_cst_value_fields (&bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_REAL_CST))
    unpack_ts_real_cst_value_fields (&bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_FIXED_CST))
    unpack_ts_fixed_cst_value_fields (&bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    stream_input_location (&DECL_SOURCE_LOCATION (expr), &bp, data_in);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    unpack_ts_decl_common_value_fields (&bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WRTL))
    unpack_ts_decl_wrtl_value_fields (&bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    unpack_ts_decl_with_vis_value_fields (&bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    unpack_ts_function_decl_value_fields (&bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    unpack_ts_type_common_value_fields (&bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_EXP))
    {
      stream_input_location (&EXPR_CHECK (expr)->exp.locus, &bp, data_in);
      if (code == MEM_REF
	  || code == TARGET_MEM_REF)
	{
	  MR_DEPENDENCE_CLIQUE (expr)
	    = (unsigned)bp_unpack_value (&bp, sizeof (short) * 8);
	  if (MR_DEPENDENCE_CLIQUE (expr) != 0)
	    MR_DEPENDENCE_BASE (expr)
	      = (unsigned)bp_unpack_value (&bp, sizeof (short) * 8);
	}
    }

  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    unpack_ts_block_value_fields (data_in, &bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TRANSLATION_UNIT_DECL))
    unpack_ts_translation_unit_decl_value_fields (data_in, &bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_OPTIMIZATION))
    cl_optimization_stream_in (&bp, TREE_OPTIMIZATION (expr));

  if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    {
      unsigned HOST_WIDE_INT length = bp_unpack_var_len_unsigned (&bp);
      if (length > 0)
	vec_safe_grow (BINFO_BASE_ACCESSES (expr), length);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    {
      unsigned HOST_WIDE_INT length = bp_unpack_var_len_unsigned (&bp);
      if (length > 0)
	vec_safe_grow (CONSTRUCTOR_ELTS (expr), length);
    }

#ifndef ACCEL_COMPILER
  if (CODE_CONTAINS_STRUCT (code, TS_TARGET_OPTION))
    {
      cl_target_option_stream_in (data_in, &bp, TREE_TARGET_OPTION (expr));
      if (targetm.target_option.post_stream_in)
	targetm.target_option.post_stream_in (TREE_TARGET_OPTION (expr));
    }
#endif

  if (code == OMP_CLAUSE)
    unpack_ts_omp_clause_value_fields (data_in, &bp, expr);
}


/* Materialize a new tree from input block IB using descriptors in
   DATA_IN.  The code for the new tree should match TAG.  Store in
   *IX_P the index into the reader cache where the new tree is stored.  */

tree
streamer_alloc_tree (struct lto_input_block *ib, struct data_in *data_in,
		     enum LTO_tags tag)
{
  enum tree_code code;
  tree result;
#ifdef LTO_STREAMER_DEBUG
  HOST_WIDE_INT orig_address_in_writer;
#endif

  result = NULL_TREE;

#ifdef LTO_STREAMER_DEBUG
  /* Read the word representing the memory address for the tree
     as it was written by the writer.  This is useful when
     debugging differences between the writer and reader.  */
  orig_address_in_writer = streamer_read_hwi (ib);
  gcc_assert ((intptr_t) orig_address_in_writer == orig_address_in_writer);
#endif

  code = lto_tag_to_tree_code (tag);

  /* We should never see an SSA_NAME tree.  Only the version numbers of
     SSA names are ever written out.  See input_ssa_names.  */
  gcc_assert (code != SSA_NAME);

  /* Instantiate a new tree using the header data.  */
  if (CODE_CONTAINS_STRUCT (code, TS_STRING))
    result = streamer_read_string_cst (data_in, ib);
  else if (CODE_CONTAINS_STRUCT (code, TS_IDENTIFIER))
    result = input_identifier (data_in, ib);
  else if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    {
      HOST_WIDE_INT len = streamer_read_hwi (ib);
      result = make_tree_vec (len);
    }
  else if (CODE_CONTAINS_STRUCT (code, TS_VECTOR))
    {
      HOST_WIDE_INT len = streamer_read_hwi (ib);
      result = make_vector (len);
    }
  else if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    {
      unsigned HOST_WIDE_INT len = streamer_read_uhwi (ib);
      result = make_tree_binfo (len);
    }
  else if (CODE_CONTAINS_STRUCT (code, TS_INT_CST))
    {
      unsigned HOST_WIDE_INT len = streamer_read_uhwi (ib);
      unsigned HOST_WIDE_INT ext_len = streamer_read_uhwi (ib);
      result = make_int_cst (len, ext_len);
    }
  else if (code == CALL_EXPR)
    {
      unsigned HOST_WIDE_INT nargs = streamer_read_uhwi (ib);
      return build_vl_exp (CALL_EXPR, nargs + 3);
    }
  else if (code == OMP_CLAUSE)
    {
      enum omp_clause_code subcode
	= (enum omp_clause_code) streamer_read_uhwi (ib);
      return build_omp_clause (UNKNOWN_LOCATION, subcode);
    }
  else
    {
      /* For all other nodes, materialize the tree with a raw
	 make_node call.  */
      result = make_node (code);
    }

#ifdef LTO_STREAMER_DEBUG
  /* Store the original address of the tree as seen by the writer
     in RESULT's aux field.  This is useful when debugging streaming
     problems.  This way, a debugging session can be started on
     both writer and reader with a breakpoint using this address
     value in both.  */
  lto_orig_address_map (result, (intptr_t) orig_address_in_writer);
#endif

  return result;
}


/* Read all pointer fields in the TS_COMMON structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */


static void
lto_input_ts_common_tree_pointers (struct lto_input_block *ib,
				   struct data_in *data_in, tree expr)
{
  if (TREE_CODE (expr) != IDENTIFIER_NODE)
    TREE_TYPE (expr) = stream_read_tree (ib, data_in);
}


/* Read all pointer fields in the TS_VECTOR structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_vector_tree_pointers (struct lto_input_block *ib,
				   struct data_in *data_in, tree expr)
{
  unsigned i;
  for (i = 0; i < VECTOR_CST_NELTS (expr); ++i)
    VECTOR_CST_ELT (expr, i) = stream_read_tree (ib, data_in);
}


/* Read all pointer fields in the TS_COMPLEX structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_complex_tree_pointers (struct lto_input_block *ib,
				    struct data_in *data_in, tree expr)
{
  TREE_REALPART (expr) = stream_read_tree (ib, data_in);
  TREE_IMAGPART (expr) = stream_read_tree (ib, data_in);
}


/* Read all pointer fields in the TS_DECL_MINIMAL structure of EXPR
   from input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_decl_minimal_tree_pointers (struct lto_input_block *ib,
					 struct data_in *data_in, tree expr)
{
  DECL_NAME (expr) = stream_read_tree (ib, data_in);
  DECL_CONTEXT (expr) = stream_read_tree (ib, data_in);
}


/* Read all pointer fields in the TS_DECL_COMMON structure of EXPR from
   input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_decl_common_tree_pointers (struct lto_input_block *ib,
					struct data_in *data_in, tree expr)
{
  DECL_SIZE (expr) = stream_read_tree (ib, data_in);
  DECL_SIZE_UNIT (expr) = stream_read_tree (ib, data_in);
  DECL_ATTRIBUTES (expr) = stream_read_tree (ib, data_in);

  /* Do not stream DECL_ABSTRACT_ORIGIN.  We cannot handle debug information
     for early inlining so drop it on the floor instead of ICEing in
     dwarf2out.c.  */

  if ((VAR_P (expr) || TREE_CODE (expr) == PARM_DECL)
      && DECL_HAS_VALUE_EXPR_P (expr))
    SET_DECL_VALUE_EXPR (expr, stream_read_tree (ib, data_in));

  if (VAR_P (expr))
    {
      tree dexpr = stream_read_tree (ib, data_in);
      if (dexpr)
	SET_DECL_DEBUG_EXPR (expr, dexpr);
    }
}


/* Read all pointer fields in the TS_DECL_NON_COMMON structure of
   EXPR from input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_decl_non_common_tree_pointers (struct lto_input_block *ib,
					    struct data_in *data_in, tree expr)
{
  if (TREE_CODE (expr) == TYPE_DECL)
    DECL_ORIGINAL_TYPE (expr) = stream_read_tree (ib, data_in);
}


/* Read all pointer fields in the TS_DECL_WITH_VIS structure of EXPR
   from input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_decl_with_vis_tree_pointers (struct lto_input_block *ib,
				          struct data_in *data_in, tree expr)
{
  tree id;

  id = stream_read_tree (ib, data_in);
  if (id)
    {
      gcc_assert (TREE_CODE (id) == IDENTIFIER_NODE);
      SET_DECL_ASSEMBLER_NAME (expr, id);
    }
}


/* Read all pointer fields in the TS_FIELD_DECL structure of EXPR from
   input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_field_decl_tree_pointers (struct lto_input_block *ib,
				       struct data_in *data_in, tree expr)
{
  DECL_FIELD_OFFSET (expr) = stream_read_tree (ib, data_in);
  DECL_BIT_FIELD_TYPE (expr) = stream_read_tree (ib, data_in);
  DECL_BIT_FIELD_REPRESENTATIVE (expr) = stream_read_tree (ib, data_in);
  DECL_FIELD_BIT_OFFSET (expr) = stream_read_tree (ib, data_in);
  DECL_FCONTEXT (expr) = stream_read_tree (ib, data_in);
}


/* Read all pointer fields in the TS_FUNCTION_DECL structure of EXPR
   from input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_function_decl_tree_pointers (struct lto_input_block *ib,
					  struct data_in *data_in, tree expr)
{
  DECL_VINDEX (expr) = stream_read_tree (ib, data_in);
  /* DECL_STRUCT_FUNCTION is loaded on demand by cgraph_get_body.  */
  DECL_FUNCTION_PERSONALITY (expr) = stream_read_tree (ib, data_in);
#ifndef ACCEL_COMPILER
  DECL_FUNCTION_SPECIFIC_TARGET (expr) = stream_read_tree (ib, data_in);
#endif
  DECL_FUNCTION_SPECIFIC_OPTIMIZATION (expr) = stream_read_tree (ib, data_in);

  /* If the file contains a function with an EH personality set,
     then it was compiled with -fexceptions.  In that case, initialize
     the backend EH machinery.  */
  if (DECL_FUNCTION_PERSONALITY (expr))
    lto_init_eh ();
}


/* Read all pointer fields in the TS_TYPE_COMMON structure of EXPR from
   input block IB.  DATA_IN contains tables and descriptors for the file
   being read.  */

static void
lto_input_ts_type_common_tree_pointers (struct lto_input_block *ib,
					struct data_in *data_in, tree expr)
{
  TYPE_SIZE (expr) = stream_read_tree (ib, data_in);
  TYPE_SIZE_UNIT (expr) = stream_read_tree (ib, data_in);
  TYPE_ATTRIBUTES (expr) = stream_read_tree (ib, data_in);
  TYPE_NAME (expr) = stream_read_tree (ib, data_in);
  /* Do not stream TYPE_POINTER_TO or TYPE_REFERENCE_TO.  They will be
     reconstructed during fixup.  */
  /* Do not stream TYPE_NEXT_VARIANT, we reconstruct the variant lists
     during fixup.  */
  TYPE_MAIN_VARIANT (expr) = stream_read_tree (ib, data_in);
  TYPE_CONTEXT (expr) = stream_read_tree (ib, data_in);
  /* TYPE_CANONICAL gets re-computed during type merging.  */
  TYPE_CANONICAL (expr) = NULL_TREE;
  TYPE_STUB_DECL (expr) = stream_read_tree (ib, data_in);
}

/* Read all pointer fields in the TS_TYPE_NON_COMMON structure of EXPR
   from input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_type_non_common_tree_pointers (struct lto_input_block *ib,
					    struct data_in *data_in,
					    tree expr)
{
  if (TREE_CODE (expr) == ENUMERAL_TYPE)
    TYPE_VALUES (expr) = stream_read_tree (ib, data_in);
  else if (TREE_CODE (expr) == ARRAY_TYPE)
    TYPE_DOMAIN (expr) = stream_read_tree (ib, data_in);
  else if (RECORD_OR_UNION_TYPE_P (expr))
    TYPE_FIELDS (expr) = streamer_read_chain (ib, data_in);
  else if (TREE_CODE (expr) == FUNCTION_TYPE
	   || TREE_CODE (expr) == METHOD_TYPE)
    TYPE_ARG_TYPES (expr) = stream_read_tree (ib, data_in);

  if (!POINTER_TYPE_P (expr))
    TYPE_MINVAL (expr) = stream_read_tree (ib, data_in);
  TYPE_MAXVAL (expr) = stream_read_tree (ib, data_in);
  if (RECORD_OR_UNION_TYPE_P (expr))
    TYPE_BINFO (expr) = stream_read_tree (ib, data_in);
}


/* Read all pointer fields in the TS_LIST structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_list_tree_pointers (struct lto_input_block *ib,
				 struct data_in *data_in, tree expr)
{
  TREE_PURPOSE (expr) = stream_read_tree (ib, data_in);
  TREE_VALUE (expr) = stream_read_tree (ib, data_in);
  TREE_CHAIN (expr) = stream_read_tree (ib, data_in);
}


/* Read all pointer fields in the TS_VEC structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_vec_tree_pointers (struct lto_input_block *ib,
				struct data_in *data_in, tree expr)
{
  int i;

  /* Note that TREE_VEC_LENGTH was read by streamer_alloc_tree to
     instantiate EXPR.  */
  for (i = 0; i < TREE_VEC_LENGTH (expr); i++)
    TREE_VEC_ELT (expr, i) = stream_read_tree (ib, data_in);
}


/* Read all pointer fields in the TS_EXP structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */


static void
lto_input_ts_exp_tree_pointers (struct lto_input_block *ib,
			        struct data_in *data_in, tree expr)
{
  int i;
  tree block;

  for (i = 0; i < TREE_OPERAND_LENGTH (expr); i++)
    TREE_OPERAND (expr, i) = stream_read_tree (ib, data_in);

  block = stream_read_tree (ib, data_in);

  /* TODO: Block is stored in the locus information.  It may make more sense to
     to make it go via the location cache.  */
  if (block)
    {
      data_in->location_cache.apply_location_cache ();
      TREE_SET_BLOCK (expr, block);
    }
}


/* Read all pointer fields in the TS_BLOCK structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_block_tree_pointers (struct lto_input_block *ib,
				  struct data_in *data_in, tree expr)
{
  BLOCK_VARS (expr) = streamer_read_chain (ib, data_in);

  BLOCK_SUPERCONTEXT (expr) = stream_read_tree (ib, data_in);

  /* Stream BLOCK_ABSTRACT_ORIGIN and BLOCK_SOURCE_LOCATION for
     the limited cases we can handle - those that represent inlined
     function scopes.  For the rest them on the floor instead of ICEing in
     dwarf2out.c.  */
  BLOCK_ABSTRACT_ORIGIN (expr) = stream_read_tree (ib, data_in);
  /* Do not stream BLOCK_NONLOCALIZED_VARS.  We cannot handle debug information
     for early inlined BLOCKs so drop it on the floor instead of ICEing in
     dwarf2out.c.  */

  /* BLOCK_FRAGMENT_ORIGIN and BLOCK_FRAGMENT_CHAIN is not live at LTO
     streaming time.  */

  /* We re-compute BLOCK_SUBBLOCKS of our parent here instead
     of streaming it.  For non-BLOCK BLOCK_SUPERCONTEXTs we still
     stream the child relationship explicitly.  */
  if (BLOCK_SUPERCONTEXT (expr)
      && TREE_CODE (BLOCK_SUPERCONTEXT (expr)) == BLOCK)
    {
      BLOCK_CHAIN (expr) = BLOCK_SUBBLOCKS (BLOCK_SUPERCONTEXT (expr));
      BLOCK_SUBBLOCKS (BLOCK_SUPERCONTEXT (expr)) = expr;
    }

  /* The global block is rooted at the TU decl.  Hook it here to
     avoid the need to stream in this block during WPA time.  */
  else if (BLOCK_SUPERCONTEXT (expr)
	   && TREE_CODE (BLOCK_SUPERCONTEXT (expr)) == TRANSLATION_UNIT_DECL)
    DECL_INITIAL (BLOCK_SUPERCONTEXT (expr)) = expr;

  /* The function-level block is connected at the time we read in
     function bodies for the same reason.  */
}


/* Read all pointer fields in the TS_BINFO structure of EXPR from input
   block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_binfo_tree_pointers (struct lto_input_block *ib,
				  struct data_in *data_in, tree expr)
{
  unsigned i;
  tree t;

  /* Note that the number of slots in EXPR was read in
     streamer_alloc_tree when instantiating EXPR.  However, the
     vector is empty so we cannot rely on vec::length to know how many
     elements to read.  So, this list is emitted as a 0-terminated
     list on the writer side.  */
  do
    {
      t = stream_read_tree (ib, data_in);
      if (t)
	BINFO_BASE_BINFOS (expr)->quick_push (t);
    }
  while (t);

  BINFO_OFFSET (expr) = stream_read_tree (ib, data_in);
  BINFO_VTABLE (expr) = stream_read_tree (ib, data_in);
  BINFO_VPTR_FIELD (expr) = stream_read_tree (ib, data_in);

  /* The vector of BINFO_BASE_ACCESSES is pre-allocated during
     unpacking the bitfield section.  */
  for (i = 0; i < vec_safe_length (BINFO_BASE_ACCESSES (expr)); i++)
    {
      tree a = stream_read_tree (ib, data_in);
      (*BINFO_BASE_ACCESSES (expr))[i] = a;
    }
  /* Do not walk BINFO_INHERITANCE_CHAIN, BINFO_SUBVTT_INDEX
     and BINFO_VPTR_INDEX; these are used by C++ FE only.  */
}


/* Read all pointer fields in the TS_CONSTRUCTOR structure of EXPR from
   input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_constructor_tree_pointers (struct lto_input_block *ib,
				        struct data_in *data_in, tree expr)
{
  unsigned i;

  for (i = 0; i < CONSTRUCTOR_NELTS (expr); i++)
    {
      constructor_elt e;
      e.index = stream_read_tree (ib, data_in);
      e.value = stream_read_tree (ib, data_in);
      (*CONSTRUCTOR_ELTS (expr))[i] = e;
    }
}


/* Read all pointer fields in the TS_OMP_CLAUSE structure of EXPR from
   input block IB.  DATA_IN contains tables and descriptors for the
   file being read.  */

static void
lto_input_ts_omp_clause_tree_pointers (struct lto_input_block *ib,
				       struct data_in *data_in, tree expr)
{
  int i;

  for (i = 0; i < omp_clause_num_ops[OMP_CLAUSE_CODE (expr)]; i++)
    OMP_CLAUSE_OPERAND (expr, i) = stream_read_tree (ib, data_in);
  OMP_CLAUSE_CHAIN (expr) = stream_read_tree (ib, data_in);
}


/* Read all pointer fields in EXPR from input block IB.  DATA_IN
   contains tables and descriptors for the file being read.  */

void
streamer_read_tree_body (struct lto_input_block *ib, struct data_in *data_in,
			 tree expr)
{
  enum tree_code code;

  code = TREE_CODE (expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    lto_input_ts_common_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_VECTOR))
    lto_input_ts_vector_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_COMPLEX))
    lto_input_ts_complex_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    lto_input_ts_decl_minimal_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    lto_input_ts_decl_common_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_NON_COMMON))
    lto_input_ts_decl_non_common_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    lto_input_ts_decl_with_vis_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_FIELD_DECL))
    lto_input_ts_field_decl_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    lto_input_ts_function_decl_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    lto_input_ts_type_common_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON))
    lto_input_ts_type_non_common_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_LIST))
    lto_input_ts_list_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    lto_input_ts_vec_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_EXP))
    lto_input_ts_exp_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    lto_input_ts_block_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    lto_input_ts_binfo_tree_pointers (ib, data_in, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    lto_input_ts_constructor_tree_pointers (ib, data_in, expr);

  if (code == OMP_CLAUSE)
    lto_input_ts_omp_clause_tree_pointers (ib, data_in, expr);
}


/* Read an index IX from input block IB and return the tree node at
   DATA_IN->FILE_DATA->GLOBALS_INDEX[IX].  */

tree
streamer_get_pickled_tree (struct lto_input_block *ib, struct data_in *data_in)
{
  unsigned HOST_WIDE_INT ix;
  tree result;
  enum LTO_tags expected_tag;

  ix = streamer_read_uhwi (ib);
  expected_tag = streamer_read_enum (ib, LTO_tags, LTO_NUM_TAGS);

  result = streamer_tree_cache_get_tree (data_in->reader_cache, ix);
  gcc_assert (result
              && TREE_CODE (result) == lto_tag_to_tree_code (expected_tag));

  return result;
}
