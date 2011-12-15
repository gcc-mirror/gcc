/* Routines for emitting trees to a file stream.

   Copyright 2011 Free Software Foundation, Inc.
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
#include "diagnostic.h"
#include "tree.h"
#include "tree-streamer.h"
#include "data-streamer.h"
#include "streamer-hooks.h"

/* Output the STRING constant to the string
   table in OB.  Then put the index onto the INDEX_STREAM.  */

void
streamer_write_string_cst (struct output_block *ob,
			   struct lto_output_stream *index_stream,
			   tree string)
{
  streamer_write_string_with_length (ob, index_stream,
				     string ? TREE_STRING_POINTER (string)
					    : NULL,
				     string ? TREE_STRING_LENGTH (string) : 0,
				     true);
}


/* Output the identifier ID to the string
   table in OB.  Then put the index onto the INDEX_STREAM.  */

static void
write_identifier (struct output_block *ob,
		   struct lto_output_stream *index_stream,
		   tree id)
{
  streamer_write_string_with_length (ob, index_stream,
				     IDENTIFIER_POINTER (id),
				     IDENTIFIER_LENGTH (id),
				     true);
}


/* Pack all the non-pointer fields of the TS_BASE structure of
   expression EXPR into bitpack BP.  */

static void
pack_ts_base_value_fields (struct bitpack_d *bp, tree expr)
{
  bp_pack_value (bp, TREE_CODE (expr), 16);
  if (!TYPE_P (expr))
    {
      bp_pack_value (bp, TREE_SIDE_EFFECTS (expr), 1);
      bp_pack_value (bp, TREE_CONSTANT (expr), 1);
      bp_pack_value (bp, TREE_READONLY (expr), 1);

      /* TREE_PUBLIC is used on types to indicate that the type
	 has a TYPE_CACHED_VALUES vector.  This is not streamed out,
	 so we skip it here.  */
      bp_pack_value (bp, TREE_PUBLIC (expr), 1);
    }
  else
    bp_pack_value (bp, 0, 4);
  bp_pack_value (bp, TREE_ADDRESSABLE (expr), 1);
  bp_pack_value (bp, TREE_THIS_VOLATILE (expr), 1);
  if (DECL_P (expr))
    bp_pack_value (bp, DECL_UNSIGNED (expr), 1);
  else if (TYPE_P (expr))
    bp_pack_value (bp, TYPE_UNSIGNED (expr), 1);
  else
    bp_pack_value (bp, 0, 1);
  /* We write debug info two times, do not confuse the second one.  */
  bp_pack_value (bp, TYPE_P (expr) ? 0 : TREE_ASM_WRITTEN (expr), 1);
  if (TYPE_P (expr))
    bp_pack_value (bp, TYPE_ARTIFICIAL (expr), 1);
  else
    bp_pack_value (bp, TREE_NO_WARNING (expr), 1);
  bp_pack_value (bp, TREE_USED (expr), 1);
  bp_pack_value (bp, TREE_NOTHROW (expr), 1);
  bp_pack_value (bp, TREE_STATIC (expr), 1);
  bp_pack_value (bp, TREE_PRIVATE (expr), 1);
  bp_pack_value (bp, TREE_PROTECTED (expr), 1);
  bp_pack_value (bp, TREE_DEPRECATED (expr), 1);
  if (TYPE_P (expr))
    {
      bp_pack_value (bp, TYPE_SATURATING (expr), 1);
      bp_pack_value (bp, TYPE_ADDR_SPACE (expr), 8);
    }
  else if (TREE_CODE (expr) == SSA_NAME)
    bp_pack_value (bp, SSA_NAME_IS_DEFAULT_DEF (expr), 1);
  else
    bp_pack_value (bp, 0, 1);
}


/* Pack all the non-pointer fields of the TS_REAL_CST structure of
   expression EXPR into bitpack BP.  */

static void
pack_ts_real_cst_value_fields (struct bitpack_d *bp, tree expr)
{
  unsigned i;
  REAL_VALUE_TYPE r;

  r = TREE_REAL_CST (expr);
  bp_pack_value (bp, r.cl, 2);
  bp_pack_value (bp, r.decimal, 1);
  bp_pack_value (bp, r.sign, 1);
  bp_pack_value (bp, r.signalling, 1);
  bp_pack_value (bp, r.canonical, 1);
  bp_pack_value (bp, r.uexp, EXP_BITS);
  for (i = 0; i < SIGSZ; i++)
    bp_pack_value (bp, r.sig[i], HOST_BITS_PER_LONG);
}


/* Pack all the non-pointer fields of the TS_FIXED_CST structure of
   expression EXPR into bitpack BP.  */

static void
pack_ts_fixed_cst_value_fields (struct bitpack_d *bp, tree expr)
{
  struct fixed_value fv = TREE_FIXED_CST (expr);
  bp_pack_enum (bp, machine_mode, MAX_MACHINE_MODE, fv.mode);
  bp_pack_var_len_int (bp, fv.data.low);
  bp_pack_var_len_int (bp, fv.data.high);
}


/* Pack all the non-pointer fields of the TS_DECL_COMMON structure
   of expression EXPR into bitpack BP.  */

static void
pack_ts_decl_common_value_fields (struct bitpack_d *bp, tree expr)
{
  bp_pack_enum (bp, machine_mode, MAX_MACHINE_MODE, DECL_MODE (expr));
  bp_pack_value (bp, DECL_NONLOCAL (expr), 1);
  bp_pack_value (bp, DECL_VIRTUAL_P (expr), 1);
  bp_pack_value (bp, DECL_IGNORED_P (expr), 1);
  bp_pack_value (bp, DECL_ABSTRACT (expr), 1);
  bp_pack_value (bp, DECL_ARTIFICIAL (expr), 1);
  bp_pack_value (bp, DECL_USER_ALIGN (expr), 1);
  bp_pack_value (bp, DECL_PRESERVE_P (expr), 1);
  bp_pack_value (bp, DECL_DEBUG_EXPR_IS_FROM (expr), 1);
  bp_pack_value (bp, DECL_EXTERNAL (expr), 1);
  bp_pack_value (bp, DECL_GIMPLE_REG_P (expr), 1);
  bp_pack_var_len_unsigned (bp, DECL_ALIGN (expr));

  if (TREE_CODE (expr) == LABEL_DECL)
    {
      /* Note that we do not write LABEL_DECL_UID.  The reader will
	 always assume an initial value of -1 so that the
	 label_to_block_map is recreated by gimple_set_bb.  */
      bp_pack_value (bp, DECL_ERROR_ISSUED (expr), 1);
      bp_pack_var_len_unsigned (bp, EH_LANDING_PAD_NR (expr));
    }

  if (TREE_CODE (expr) == FIELD_DECL)
    {
      bp_pack_value (bp, DECL_PACKED (expr), 1);
      bp_pack_value (bp, DECL_NONADDRESSABLE_P (expr), 1);
      bp_pack_value (bp, expr->decl_common.off_align, 8);
    }

  if (TREE_CODE (expr) == RESULT_DECL
      || TREE_CODE (expr) == PARM_DECL
      || TREE_CODE (expr) == VAR_DECL)
    {
      bp_pack_value (bp, DECL_BY_REFERENCE (expr), 1);
      if (TREE_CODE (expr) == VAR_DECL
	  || TREE_CODE (expr) == PARM_DECL)
	bp_pack_value (bp, DECL_HAS_VALUE_EXPR_P (expr), 1);
      bp_pack_value (bp, DECL_RESTRICTED_P (expr), 1);
    }
}


/* Pack all the non-pointer fields of the TS_DECL_WRTL structure
   of expression EXPR into bitpack BP.  */

static void
pack_ts_decl_wrtl_value_fields (struct bitpack_d *bp, tree expr)
{
  bp_pack_value (bp, DECL_REGISTER (expr), 1);
}


/* Pack all the non-pointer fields of the TS_DECL_WITH_VIS structure
   of expression EXPR into bitpack BP.  */

static void
pack_ts_decl_with_vis_value_fields (struct bitpack_d *bp, tree expr)
{
  bp_pack_value (bp, DECL_DEFER_OUTPUT (expr), 1);
  bp_pack_value (bp, DECL_COMMON (expr), 1);
  bp_pack_value (bp, DECL_DLLIMPORT_P (expr), 1);
  bp_pack_value (bp, DECL_WEAK (expr), 1);
  bp_pack_value (bp, DECL_SEEN_IN_BIND_EXPR_P (expr),  1);
  bp_pack_value (bp, DECL_COMDAT (expr),  1);
  bp_pack_value (bp, DECL_VISIBILITY (expr),  2);
  bp_pack_value (bp, DECL_VISIBILITY_SPECIFIED (expr),  1);

  if (TREE_CODE (expr) == VAR_DECL)
    {
      bp_pack_value (bp, DECL_HARD_REGISTER (expr), 1);
      bp_pack_value (bp, DECL_IN_TEXT_SECTION (expr), 1);
      bp_pack_value (bp, DECL_IN_CONSTANT_POOL (expr), 1);
      bp_pack_value (bp, DECL_TLS_MODEL (expr),  3);
    }

  if (VAR_OR_FUNCTION_DECL_P (expr))
    bp_pack_var_len_unsigned (bp, DECL_INIT_PRIORITY (expr));
}


/* Pack all the non-pointer fields of the TS_FUNCTION_DECL structure
   of expression EXPR into bitpack BP.  */

static void
pack_ts_function_decl_value_fields (struct bitpack_d *bp, tree expr)
{
  /* For normal/md builtins we only write the class and code, so they
     should never be handled here.  */
  gcc_assert (!streamer_handle_as_builtin_p (expr));

  bp_pack_enum (bp, built_in_class, BUILT_IN_LAST,
		DECL_BUILT_IN_CLASS (expr));
  bp_pack_value (bp, DECL_STATIC_CONSTRUCTOR (expr), 1);
  bp_pack_value (bp, DECL_STATIC_DESTRUCTOR (expr), 1);
  bp_pack_value (bp, DECL_UNINLINABLE (expr), 1);
  bp_pack_value (bp, DECL_POSSIBLY_INLINED (expr), 1);
  bp_pack_value (bp, DECL_IS_NOVOPS (expr), 1);
  bp_pack_value (bp, DECL_IS_RETURNS_TWICE (expr), 1);
  bp_pack_value (bp, DECL_IS_MALLOC (expr), 1);
  bp_pack_value (bp, DECL_IS_OPERATOR_NEW (expr), 1);
  bp_pack_value (bp, DECL_DECLARED_INLINE_P (expr), 1);
  bp_pack_value (bp, DECL_STATIC_CHAIN (expr), 1);
  bp_pack_value (bp, DECL_NO_INLINE_WARNING_P (expr), 1);
  bp_pack_value (bp, DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (expr), 1);
  bp_pack_value (bp, DECL_NO_LIMIT_STACK (expr), 1);
  bp_pack_value (bp, DECL_DISREGARD_INLINE_LIMITS (expr), 1);
  bp_pack_value (bp, DECL_PURE_P (expr), 1);
  bp_pack_value (bp, DECL_LOOPING_CONST_OR_PURE_P (expr), 1);
  if (DECL_BUILT_IN_CLASS (expr) != NOT_BUILT_IN)
    bp_pack_value (bp, DECL_FUNCTION_CODE (expr), 11);
  if (DECL_STATIC_DESTRUCTOR (expr))
    bp_pack_var_len_unsigned (bp, DECL_FINI_PRIORITY (expr));
}


/* Pack all the non-pointer fields of the TS_TYPE_COMMON structure
   of expression EXPR into bitpack BP.  */

static void
pack_ts_type_common_value_fields (struct bitpack_d *bp, tree expr)
{
  bp_pack_enum (bp, machine_mode, MAX_MACHINE_MODE, TYPE_MODE (expr));
  bp_pack_value (bp, TYPE_STRING_FLAG (expr), 1);
  bp_pack_value (bp, TYPE_NO_FORCE_BLK (expr), 1);
  bp_pack_value (bp, TYPE_NEEDS_CONSTRUCTING (expr), 1);
  if (RECORD_OR_UNION_TYPE_P (expr))
    bp_pack_value (bp, TYPE_TRANSPARENT_AGGR (expr), 1);
  bp_pack_value (bp, TYPE_PACKED (expr), 1);
  bp_pack_value (bp, TYPE_RESTRICT (expr), 1);
  bp_pack_value (bp, TYPE_CONTAINS_PLACEHOLDER_INTERNAL (expr), 2);
  bp_pack_value (bp, TYPE_USER_ALIGN (expr), 1);
  bp_pack_value (bp, TYPE_READONLY (expr), 1);
  bp_pack_var_len_unsigned (bp, TYPE_PRECISION (expr));
  bp_pack_var_len_unsigned (bp, TYPE_ALIGN (expr));
  bp_pack_var_len_int (bp, TYPE_ALIAS_SET (expr) == 0 ? 0 : -1);
}


/* Pack all the non-pointer fields of the TS_BLOCK structure
   of expression EXPR into bitpack BP.  */

static void
pack_ts_block_value_fields (struct bitpack_d *bp, tree expr)
{
  bp_pack_value (bp, BLOCK_ABSTRACT (expr), 1);
  /* BLOCK_NUMBER is recomputed.  */
}

/* Pack all the non-pointer fields of the TS_TRANSLATION_UNIT_DECL structure
   of expression EXPR into bitpack BP.  */

static void
pack_ts_translation_unit_decl_value_fields (struct bitpack_d *bp ATTRIBUTE_UNUSED, tree expr ATTRIBUTE_UNUSED)
{
}


/* Pack all the bitfields in EXPR into a bit pack.  */

void
streamer_pack_tree_bitfields (struct bitpack_d *bp, tree expr)
{
  enum tree_code code;

  code = TREE_CODE (expr);

  /* Note that all these functions are highly sensitive to changes in
     the types and sizes of each of the fields being packed.  */
  pack_ts_base_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_REAL_CST))
    pack_ts_real_cst_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_FIXED_CST))
    pack_ts_fixed_cst_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    pack_ts_decl_common_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WRTL))
    pack_ts_decl_wrtl_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    pack_ts_decl_with_vis_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    pack_ts_function_decl_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    pack_ts_type_common_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    pack_ts_block_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TRANSLATION_UNIT_DECL))
    pack_ts_translation_unit_decl_value_fields (bp, expr);
}


/* Write the code and class of builtin EXPR to output block OB.  IX is
   the index into the streamer cache where EXPR is stored.*/

void
streamer_write_builtin (struct output_block *ob, tree expr)
{
  gcc_assert (streamer_handle_as_builtin_p (expr));

  if (DECL_BUILT_IN_CLASS (expr) == BUILT_IN_MD
      && !targetm.builtin_decl)
    sorry ("tree bytecode streams do not support machine specific builtin "
	   "functions on this target");

  streamer_write_record_start (ob, LTO_builtin_decl);
  streamer_write_enum (ob->main_stream, built_in_class, BUILT_IN_LAST,
		       DECL_BUILT_IN_CLASS (expr));
  streamer_write_uhwi (ob, DECL_FUNCTION_CODE (expr));

  if (DECL_ASSEMBLER_NAME_SET_P (expr))
    {
      /* When the assembler name of a builtin gets a user name,
	 the new name is always prefixed with '*' by
	 set_builtin_user_assembler_name.  So, to prevent the
	 reader side from adding a second '*', we omit it here.  */
      const char *str = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (expr));
      if (strlen (str) > 1 && str[0] == '*')
	streamer_write_string (ob, ob->main_stream, &str[1], true);
      else
	streamer_write_string (ob, ob->main_stream, NULL, true);
    }
  else
    streamer_write_string (ob, ob->main_stream, NULL, true);
}


/* Emit the chain of tree nodes starting at T.  OB is the output block
   to write to.  REF_P is true if chain elements should be emitted
   as references.  */

void
streamer_write_chain (struct output_block *ob, tree t, bool ref_p)
{
  int i, count;

  count = list_length (t);
  streamer_write_hwi (ob, count);
  for (i = 0; i < count; i++)
    {
      tree saved_chain;

      /* Clear TREE_CHAIN to avoid blindly recursing into the rest
	 of the list.  */
      saved_chain = TREE_CHAIN (t);
      TREE_CHAIN (t) = NULL_TREE;

      /* We avoid outputting external vars or functions by reference
	 to the global decls section as we do not want to have them
	 enter decl merging.  This is, of course, only for the call
	 for streaming BLOCK_VARS, but other callers are safe.  */
      stream_write_tree (ob, t,
			 ref_p && !(VAR_OR_FUNCTION_DECL_P (t)
				    && DECL_EXTERNAL (t)));

      TREE_CHAIN (t) = saved_chain;
      t = TREE_CHAIN (t);
    }
}


/* Write all pointer fields in the TS_COMMON structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
write_ts_common_tree_pointers (struct output_block *ob, tree expr, bool ref_p)
{
  if (TREE_CODE (expr) != IDENTIFIER_NODE)
    stream_write_tree (ob, TREE_TYPE (expr), ref_p);
}


/* Write all pointer fields in the TS_VECTOR structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
write_ts_vector_tree_pointers (struct output_block *ob, tree expr, bool ref_p)
{
  streamer_write_chain (ob, TREE_VECTOR_CST_ELTS (expr), ref_p);
}


/* Write all pointer fields in the TS_COMPLEX structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
write_ts_complex_tree_pointers (struct output_block *ob, tree expr, bool ref_p)
{
  stream_write_tree (ob, TREE_REALPART (expr), ref_p);
  stream_write_tree (ob, TREE_IMAGPART (expr), ref_p);
}


/* Write all pointer fields in the TS_DECL_MINIMAL structure of EXPR
   to output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
write_ts_decl_minimal_tree_pointers (struct output_block *ob, tree expr,
				     bool ref_p)
{
  stream_write_tree (ob, DECL_NAME (expr), ref_p);
  stream_write_tree (ob, DECL_CONTEXT (expr), ref_p);
  lto_output_location (ob, DECL_SOURCE_LOCATION (expr));
}


/* Write all pointer fields in the TS_DECL_COMMON structure of EXPR to
   output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
write_ts_decl_common_tree_pointers (struct output_block *ob, tree expr,
				    bool ref_p)
{
  stream_write_tree (ob, DECL_SIZE (expr), ref_p);
  stream_write_tree (ob, DECL_SIZE_UNIT (expr), ref_p);

  /* Note, DECL_INITIAL is not handled here.  Since DECL_INITIAL needs
     special handling in LTO, it must be handled by streamer hooks.  */

  stream_write_tree (ob, DECL_ATTRIBUTES (expr), ref_p);

  /* Do not stream DECL_ABSTRACT_ORIGIN.  We cannot handle debug information
     for early inlining so drop it on the floor instead of ICEing in
     dwarf2out.c.  */

  if (TREE_CODE (expr) == PARM_DECL)
    streamer_write_chain (ob, TREE_CHAIN (expr), ref_p);

  if ((TREE_CODE (expr) == VAR_DECL
       || TREE_CODE (expr) == PARM_DECL)
      && DECL_HAS_VALUE_EXPR_P (expr))
    stream_write_tree (ob, DECL_VALUE_EXPR (expr), ref_p);

  if (TREE_CODE (expr) == VAR_DECL)
    stream_write_tree (ob, DECL_DEBUG_EXPR (expr), ref_p);
}


/* Write all pointer fields in the TS_DECL_NON_COMMON structure of
   EXPR to output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
write_ts_decl_non_common_tree_pointers (struct output_block *ob, tree expr,
				        bool ref_p)
{
  if (TREE_CODE (expr) == FUNCTION_DECL)
    {
      stream_write_tree (ob, DECL_ARGUMENTS (expr), ref_p);
      stream_write_tree (ob, DECL_RESULT (expr), ref_p);
    }
  else if (TREE_CODE (expr) == TYPE_DECL)
    stream_write_tree (ob, DECL_ORIGINAL_TYPE (expr), ref_p);
  stream_write_tree (ob, DECL_VINDEX (expr), ref_p);
}


/* Write all pointer fields in the TS_DECL_WITH_VIS structure of EXPR
   to output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
write_ts_decl_with_vis_tree_pointers (struct output_block *ob, tree expr,
			              bool ref_p)
{
  /* Make sure we don't inadvertently set the assembler name.  */
  if (DECL_ASSEMBLER_NAME_SET_P (expr))
    stream_write_tree (ob, DECL_ASSEMBLER_NAME (expr), ref_p);
  else
    stream_write_tree (ob, NULL_TREE, false);

  stream_write_tree (ob, DECL_SECTION_NAME (expr), ref_p);
  stream_write_tree (ob, DECL_COMDAT_GROUP (expr), ref_p);
}


/* Write all pointer fields in the TS_FIELD_DECL structure of EXPR to
   output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
write_ts_field_decl_tree_pointers (struct output_block *ob, tree expr,
				   bool ref_p)
{
  stream_write_tree (ob, DECL_FIELD_OFFSET (expr), ref_p);
  stream_write_tree (ob, DECL_BIT_FIELD_TYPE (expr), ref_p);
  stream_write_tree (ob, DECL_QUALIFIER (expr), ref_p);
  stream_write_tree (ob, DECL_FIELD_BIT_OFFSET (expr), ref_p);
  stream_write_tree (ob, DECL_FCONTEXT (expr), ref_p);
  streamer_write_chain (ob, TREE_CHAIN (expr), ref_p);
}


/* Write all pointer fields in the TS_FUNCTION_DECL structure of EXPR
   to output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
write_ts_function_decl_tree_pointers (struct output_block *ob, tree expr,
				      bool ref_p)
{
  /* DECL_STRUCT_FUNCTION is handled by lto_output_function.  FIXME lto,
     maybe it should be handled here?  */
  stream_write_tree (ob, DECL_FUNCTION_PERSONALITY (expr), ref_p);
  stream_write_tree (ob, DECL_FUNCTION_SPECIFIC_TARGET (expr), ref_p);
  stream_write_tree (ob, DECL_FUNCTION_SPECIFIC_OPTIMIZATION (expr), ref_p);
}


/* Write all pointer fields in the TS_TYPE_COMMON structure of EXPR to
   output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
write_ts_type_common_tree_pointers (struct output_block *ob, tree expr,
				    bool ref_p)
{
  stream_write_tree (ob, TYPE_SIZE (expr), ref_p);
  stream_write_tree (ob, TYPE_SIZE_UNIT (expr), ref_p);
  stream_write_tree (ob, TYPE_ATTRIBUTES (expr), ref_p);
  stream_write_tree (ob, TYPE_NAME (expr), ref_p);
  /* Do not stream TYPE_POINTER_TO or TYPE_REFERENCE_TO.  They will be
     reconstructed during fixup.  */
  /* Do not stream TYPE_NEXT_VARIANT, we reconstruct the variant lists
     during fixup.  */
  stream_write_tree (ob, TYPE_MAIN_VARIANT (expr), ref_p);
  stream_write_tree (ob, TYPE_CONTEXT (expr), ref_p);
  /* TYPE_CANONICAL is re-computed during type merging, so no need
     to stream it here.  */
  stream_write_tree (ob, TYPE_STUB_DECL (expr), ref_p);
}

/* Write all pointer fields in the TS_TYPE_NON_COMMON structure of EXPR
   to output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
write_ts_type_non_common_tree_pointers (struct output_block *ob, tree expr,
					bool ref_p)
{
  if (TREE_CODE (expr) == ENUMERAL_TYPE)
    stream_write_tree (ob, TYPE_VALUES (expr), ref_p);
  else if (TREE_CODE (expr) == ARRAY_TYPE)
    stream_write_tree (ob, TYPE_DOMAIN (expr), ref_p);
  else if (RECORD_OR_UNION_TYPE_P (expr))
    stream_write_tree (ob, TYPE_FIELDS (expr), ref_p);
  else if (TREE_CODE (expr) == FUNCTION_TYPE
	   || TREE_CODE (expr) == METHOD_TYPE)
    stream_write_tree (ob, TYPE_ARG_TYPES (expr), ref_p);

  if (!POINTER_TYPE_P (expr))
    stream_write_tree (ob, TYPE_MINVAL (expr), ref_p);
  stream_write_tree (ob, TYPE_MAXVAL (expr), ref_p);
  if (RECORD_OR_UNION_TYPE_P (expr))
    stream_write_tree (ob, TYPE_BINFO (expr), ref_p);
}


/* Write all pointer fields in the TS_LIST structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
write_ts_list_tree_pointers (struct output_block *ob, tree expr, bool ref_p)
{
  stream_write_tree (ob, TREE_PURPOSE (expr), ref_p);
  stream_write_tree (ob, TREE_VALUE (expr), ref_p);
  streamer_write_chain (ob, TREE_CHAIN (expr), ref_p);
}


/* Write all pointer fields in the TS_VEC structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
write_ts_vec_tree_pointers (struct output_block *ob, tree expr, bool ref_p)
{
  int i;

  /* Note that the number of slots for EXPR has already been emitted
     in EXPR's header (see streamer_write_tree_header).  */
  for (i = 0; i < TREE_VEC_LENGTH (expr); i++)
    stream_write_tree (ob, TREE_VEC_ELT (expr, i), ref_p);
}


/* Write all pointer fields in the TS_EXP structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
write_ts_exp_tree_pointers (struct output_block *ob, tree expr, bool ref_p)
{
  int i;

  streamer_write_hwi (ob, TREE_OPERAND_LENGTH (expr));
  for (i = 0; i < TREE_OPERAND_LENGTH (expr); i++)
    stream_write_tree (ob, TREE_OPERAND (expr, i), ref_p);
  lto_output_location (ob, EXPR_LOCATION (expr));
  stream_write_tree (ob, TREE_BLOCK (expr), ref_p);
}


/* Write all pointer fields in the TS_BLOCK structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
write_ts_block_tree_pointers (struct output_block *ob, tree expr, bool ref_p)
{
  /* Do not stream BLOCK_SOURCE_LOCATION.  We cannot handle debug information
     for early inlining so drop it on the floor instead of ICEing in
     dwarf2out.c.  */
  streamer_write_chain (ob, BLOCK_VARS (expr), ref_p);

  /* Do not stream BLOCK_NONLOCALIZED_VARS.  We cannot handle debug information
     for early inlining so drop it on the floor instead of ICEing in
     dwarf2out.c.  */

  stream_write_tree (ob, BLOCK_SUPERCONTEXT (expr), ref_p);
  /* Do not stream BLOCK_ABSTRACT_ORIGIN.  We cannot handle debug information
     for early inlining so drop it on the floor instead of ICEing in
     dwarf2out.c.  */
  stream_write_tree (ob, BLOCK_FRAGMENT_ORIGIN (expr), ref_p);
  stream_write_tree (ob, BLOCK_FRAGMENT_CHAIN (expr), ref_p);
  /* Do not output BLOCK_SUBBLOCKS.  Instead on streaming-in this
     list is re-constructed from BLOCK_SUPERCONTEXT.  */
}


/* Write all pointer fields in the TS_BINFO structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
write_ts_binfo_tree_pointers (struct output_block *ob, tree expr, bool ref_p)
{
  unsigned i;
  tree t;

  /* Note that the number of BINFO slots has already been emitted in
     EXPR's header (see streamer_write_tree_header) because this length
     is needed to build the empty BINFO node on the reader side.  */
  FOR_EACH_VEC_ELT (tree, BINFO_BASE_BINFOS (expr), i, t)
    stream_write_tree (ob, t, ref_p);
  stream_write_tree (ob, NULL_TREE, false);

  stream_write_tree (ob, BINFO_OFFSET (expr), ref_p);
  stream_write_tree (ob, BINFO_VTABLE (expr), ref_p);
  stream_write_tree (ob, BINFO_VPTR_FIELD (expr), ref_p);

  streamer_write_uhwi (ob, VEC_length (tree, BINFO_BASE_ACCESSES (expr)));
  FOR_EACH_VEC_ELT (tree, BINFO_BASE_ACCESSES (expr), i, t)
    stream_write_tree (ob, t, ref_p);

  stream_write_tree (ob, BINFO_INHERITANCE_CHAIN (expr), ref_p);
  stream_write_tree (ob, BINFO_SUBVTT_INDEX (expr), ref_p);
  stream_write_tree (ob, BINFO_VPTR_INDEX (expr), ref_p);
}


/* Write all pointer fields in the TS_CONSTRUCTOR structure of EXPR to
   output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
write_ts_constructor_tree_pointers (struct output_block *ob, tree expr,
				    bool ref_p)
{
  unsigned i;
  tree index, value;

  streamer_write_uhwi (ob, CONSTRUCTOR_NELTS (expr));
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (expr), i, index, value)
    {
      stream_write_tree (ob, index, ref_p);
      stream_write_tree (ob, value, ref_p);
    }
}

/* Write a TS_TARGET_OPTION tree in EXPR to OB.  */

static void
write_ts_target_option (struct output_block *ob, tree expr)
{
  struct cl_target_option *t = TREE_TARGET_OPTION (expr);
  struct bitpack_d bp;
  unsigned i, len;

  /* The cl_target_option is target specific and generated by the options
     awk script, so we just recreate a byte-by-byte copy here. */

  bp = bitpack_create (ob->main_stream);
  len = sizeof (struct cl_target_option);
  for (i = 0; i < len; i++)
    bp_pack_value (&bp, ((unsigned char *)t)[i], 8);
  /* Catch struct size mismatches between reader and writer. */
  bp_pack_value (&bp, 0x12345678, 32);
  streamer_write_bitpack (&bp);
}

/* Write a TS_TRANSLATION_UNIT_DECL tree in EXPR to OB.  */

static void
write_ts_translation_unit_decl_tree_pointers (struct output_block *ob,
					      tree expr)
{
  streamer_write_string (ob, ob->main_stream,
			 TRANSLATION_UNIT_LANGUAGE (expr), true);
}

/* Write all pointer fields in EXPR to output block OB.  If REF_P is true,
   the leaves of EXPR are emitted as references.  */

void
streamer_write_tree_body (struct output_block *ob, tree expr, bool ref_p)
{
  enum tree_code code;

  code = TREE_CODE (expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    write_ts_common_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_VECTOR))
    write_ts_vector_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_COMPLEX))
    write_ts_complex_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    write_ts_decl_minimal_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    write_ts_decl_common_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_NON_COMMON))
    write_ts_decl_non_common_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    write_ts_decl_with_vis_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_FIELD_DECL))
    write_ts_field_decl_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    write_ts_function_decl_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    write_ts_type_common_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON))
    write_ts_type_non_common_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_LIST))
    write_ts_list_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    write_ts_vec_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_EXP))
    write_ts_exp_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    write_ts_block_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    write_ts_binfo_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    write_ts_constructor_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_TARGET_OPTION))
    write_ts_target_option (ob, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TRANSLATION_UNIT_DECL))
    write_ts_translation_unit_decl_tree_pointers (ob, expr);
}


/* Emit header information for tree EXPR to output block OB.  The header
   contains everything needed to instantiate an empty skeleton for
   EXPR on the reading side.  IX is the index into the streamer cache
   where EXPR is stored.  */

void
streamer_write_tree_header (struct output_block *ob, tree expr)
{
  enum LTO_tags tag;
  enum tree_code code;

  /* We should not see any tree nodes not handled by the streamer.  */
  code = TREE_CODE (expr);

  /* The header of a tree node consists of its tag, the size of
     the node, and any other information needed to instantiate
     EXPR on the reading side (such as the number of slots in
     variable sized nodes).  */
  tag = lto_tree_code_to_tag (code);
  streamer_write_record_start (ob, tag);

  /* The following will cause bootstrap miscomparisons.  Enable with care.  */
#ifdef LTO_STREAMER_DEBUG
  /* This is used mainly for debugging purposes.  When the reader
     and the writer do not agree on a streamed node, the pointer
     value for EXPR can be used to track down the differences in
     the debugger.  */
  gcc_assert ((HOST_WIDEST_INT) (intptr_t) expr == (intptr_t) expr);
  streamer_write_hwi (ob, (HOST_WIDEST_INT) (intptr_t) expr);
#endif

  /* The text in strings and identifiers are completely emitted in
     the header.  */
  if (CODE_CONTAINS_STRUCT (code, TS_STRING))
    streamer_write_string_cst (ob, ob->main_stream, expr);
  else if (CODE_CONTAINS_STRUCT (code, TS_IDENTIFIER))
    write_identifier (ob, ob->main_stream, expr);
  else if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    streamer_write_hwi (ob, TREE_VEC_LENGTH (expr));
  else if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    streamer_write_uhwi (ob, BINFO_N_BASE_BINFOS (expr));
  else if (TREE_CODE (expr) == CALL_EXPR)
    streamer_write_uhwi (ob, call_expr_nargs (expr));
}


/* Emit the integer constant CST to output block OB.  If REF_P is true,
   CST's type will be emitted as a reference.  */

void
streamer_write_integer_cst (struct output_block *ob, tree cst, bool ref_p)
{
  streamer_write_record_start (ob, lto_tree_code_to_tag (INTEGER_CST));
  stream_write_tree (ob, TREE_TYPE (cst), ref_p);
  streamer_write_char_stream (ob->main_stream, TREE_OVERFLOW_P (cst));
  streamer_write_uhwi (ob, TREE_INT_CST_LOW (cst));
  streamer_write_uhwi (ob, TREE_INT_CST_HIGH (cst));
}
