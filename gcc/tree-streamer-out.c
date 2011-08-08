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

static void
output_string_cst (struct output_block *ob,
		   struct lto_output_stream *index_stream,
		   tree string)
{
  lto_output_string_with_length (ob, index_stream,
				 TREE_STRING_POINTER (string),
				 TREE_STRING_LENGTH (string),
				 true);
}


/* Output the identifier ID to the string
   table in OB.  Then put the index onto the INDEX_STREAM.  */

static void
output_identifier (struct output_block *ob,
		   struct lto_output_stream *index_stream,
		   tree id)
{
  lto_output_string_with_length (ob, index_stream,
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
    bp_pack_value (bp, TYPE_SATURATING (expr), 1);
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
  gcc_assert (!lto_stream_as_builtin_p (expr));

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

/* Pack all the non-pointer fields in EXPR into a bit pack.  */

static void
pack_value_fields (struct bitpack_d *bp, tree expr)
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

  if (streamer_hooks.pack_value_fields)
    streamer_hooks.pack_value_fields (bp, expr);
}


/* If REF_P is true, emit a reference to EXPR in output block OB,
   otherwise emit the physical representation of EXPR in OB.  */

static inline void
lto_output_tree_or_ref (struct output_block *ob, tree expr, bool ref_p)
{
  if (ref_p)
    lto_output_tree_ref (ob, expr);
  else
    lto_output_tree (ob, expr, false);
}


/* Write the code and class of builtin EXPR to output block OB.  IX is
   the index into the streamer cache where EXPR is stored.*/

static void
lto_output_builtin_tree (struct output_block *ob, tree expr)
{
  gcc_assert (lto_stream_as_builtin_p (expr));

  if (DECL_BUILT_IN_CLASS (expr) == BUILT_IN_MD
      && !targetm.builtin_decl)
    sorry ("gimple bytecode streams do not support machine specific builtin "
	   "functions on this target");

  output_record_start (ob, LTO_builtin_decl);
  lto_output_enum (ob->main_stream, built_in_class, BUILT_IN_LAST,
		   DECL_BUILT_IN_CLASS (expr));
  output_uleb128 (ob, DECL_FUNCTION_CODE (expr));

  if (DECL_ASSEMBLER_NAME_SET_P (expr))
    {
      /* When the assembler name of a builtin gets a user name,
	 the new name is always prefixed with '*' by
	 set_builtin_user_assembler_name.  So, to prevent the
	 reader side from adding a second '*', we omit it here.  */
      const char *str = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (expr));
      if (strlen (str) > 1 && str[0] == '*')
	lto_output_string (ob, ob->main_stream, &str[1], true);
      else
	lto_output_string (ob, ob->main_stream, NULL, true);
    }
  else
    lto_output_string (ob, ob->main_stream, NULL, true);
}


/* GIMPLE hook for writing GIMPLE-specific parts of trees.  OB, EXPR
   and REF_P are as in lto_write_tree.  */

void
lto_streamer_write_tree (struct output_block *ob, tree expr, bool ref_p)
{
  if (DECL_P (expr)
      && TREE_CODE (expr) != FUNCTION_DECL
      && TREE_CODE (expr) != TRANSLATION_UNIT_DECL)
    {
      /* Handle DECL_INITIAL for symbols.  */
      tree initial = DECL_INITIAL (expr);
      if (TREE_CODE (expr) == VAR_DECL
	  && (TREE_STATIC (expr) || DECL_EXTERNAL (expr))
	  && initial)
	{
	  lto_varpool_encoder_t varpool_encoder;
	  struct varpool_node *vnode;

	  varpool_encoder = ob->decl_state->varpool_node_encoder;
	  vnode = varpool_get_node (expr);
	  if (!vnode)
	    initial = error_mark_node;
	  else if (!lto_varpool_encoder_encode_initializer_p (varpool_encoder,
							      vnode))
	    initial = NULL;
	}

      lto_output_tree_or_ref (ob, initial, ref_p);
    }
}


/* Emit the chain of tree nodes starting at T.  OB is the output block
   to write to.  REF_P is true if chain elements should be emitted
   as references.  */

static void
lto_output_chain (struct output_block *ob, tree t, bool ref_p)
{
  int i, count;

  count = list_length (t);
  output_sleb128 (ob, count);
  for (i = 0; i < count; i++)
    {
      tree saved_chain;

      /* Clear TREE_CHAIN to avoid blindly recursing into the rest
	 of the list.  */
      saved_chain = TREE_CHAIN (t);
      TREE_CHAIN (t) = NULL_TREE;

      lto_output_tree_or_ref (ob, t, ref_p);

      TREE_CHAIN (t) = saved_chain;
      t = TREE_CHAIN (t);
    }
}


/* Write all pointer fields in the TS_COMMON structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_common_tree_pointers (struct output_block *ob, tree expr,
				    bool ref_p)
{
  if (TREE_CODE (expr) != IDENTIFIER_NODE)
    lto_output_tree_or_ref (ob, TREE_TYPE (expr), ref_p);
}


/* Write all pointer fields in the TS_VECTOR structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_vector_tree_pointers (struct output_block *ob, tree expr,
				    bool ref_p)
{
  lto_output_chain (ob, TREE_VECTOR_CST_ELTS (expr), ref_p);
}


/* Write all pointer fields in the TS_COMPLEX structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_complex_tree_pointers (struct output_block *ob, tree expr,
				     bool ref_p)
{
  lto_output_tree_or_ref (ob, TREE_REALPART (expr), ref_p);
  lto_output_tree_or_ref (ob, TREE_IMAGPART (expr), ref_p);
}


/* Write all pointer fields in the TS_DECL_MINIMAL structure of EXPR
   to output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_decl_minimal_tree_pointers (struct output_block *ob, tree expr,
					  bool ref_p)
{
  lto_output_tree_or_ref (ob, DECL_NAME (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_CONTEXT (expr), ref_p);
  lto_output_location (ob, DECL_SOURCE_LOCATION (expr));
}


/* Write all pointer fields in the TS_DECL_COMMON structure of EXPR to
   output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_decl_common_tree_pointers (struct output_block *ob, tree expr,
					 bool ref_p)
{
  lto_output_tree_or_ref (ob, DECL_SIZE (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_SIZE_UNIT (expr), ref_p);

  /* Note, DECL_INITIAL is not handled here.  Since DECL_INITIAL needs
     special handling in LTO, it must be handled by streamer hooks.  */

  lto_output_tree_or_ref (ob, DECL_ATTRIBUTES (expr), ref_p);

  /* Do not stream DECL_ABSTRACT_ORIGIN.  We cannot handle debug information
     for early inlining so drop it on the floor instead of ICEing in
     dwarf2out.c.  */

  if (TREE_CODE (expr) == PARM_DECL)
    lto_output_chain (ob, TREE_CHAIN (expr), ref_p);

  if ((TREE_CODE (expr) == VAR_DECL
       || TREE_CODE (expr) == PARM_DECL)
      && DECL_HAS_VALUE_EXPR_P (expr))
    lto_output_tree_or_ref (ob, DECL_VALUE_EXPR (expr), ref_p);

  if (TREE_CODE (expr) == VAR_DECL)
    lto_output_tree_or_ref (ob, DECL_DEBUG_EXPR (expr), ref_p);
}


/* Write all pointer fields in the TS_DECL_NON_COMMON structure of
   EXPR to output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_decl_non_common_tree_pointers (struct output_block *ob,
					     tree expr, bool ref_p)
{
  if (TREE_CODE (expr) == FUNCTION_DECL)
    {
      lto_output_tree_or_ref (ob, DECL_ARGUMENTS (expr), ref_p);
      lto_output_tree_or_ref (ob, DECL_RESULT (expr), ref_p);
    }
  lto_output_tree_or_ref (ob, DECL_VINDEX (expr), ref_p);
}


/* Write all pointer fields in the TS_DECL_WITH_VIS structure of EXPR
   to output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_decl_with_vis_tree_pointers (struct output_block *ob, tree expr,
					   bool ref_p)
{
  /* Make sure we don't inadvertently set the assembler name.  */
  if (DECL_ASSEMBLER_NAME_SET_P (expr))
    lto_output_tree_or_ref (ob, DECL_ASSEMBLER_NAME (expr), ref_p);
  else
    output_record_start (ob, LTO_null);

  lto_output_tree_or_ref (ob, DECL_SECTION_NAME (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_COMDAT_GROUP (expr), ref_p);
}


/* Write all pointer fields in the TS_FIELD_DECL structure of EXPR to
   output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_field_decl_tree_pointers (struct output_block *ob, tree expr,
					bool ref_p)
{
  lto_output_tree_or_ref (ob, DECL_FIELD_OFFSET (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_BIT_FIELD_TYPE (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_QUALIFIER (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_FIELD_BIT_OFFSET (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_FCONTEXT (expr), ref_p);
  lto_output_chain (ob, TREE_CHAIN (expr), ref_p);
}


/* Write all pointer fields in the TS_FUNCTION_DECL structure of EXPR
   to output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_function_decl_tree_pointers (struct output_block *ob, tree expr,
					   bool ref_p)
{
  /* DECL_STRUCT_FUNCTION is handled by lto_output_function.  FIXME lto,
     maybe it should be handled here?  */
  lto_output_tree_or_ref (ob, DECL_FUNCTION_PERSONALITY (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_FUNCTION_SPECIFIC_TARGET (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_FUNCTION_SPECIFIC_OPTIMIZATION (expr),
			  ref_p);
}


/* Write all pointer fields in the TS_TYPE_COMMON structure of EXPR to
   output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_type_common_tree_pointers (struct output_block *ob, tree expr,
					 bool ref_p)
{
  lto_output_tree_or_ref (ob, TYPE_SIZE (expr), ref_p);
  lto_output_tree_or_ref (ob, TYPE_SIZE_UNIT (expr), ref_p);
  lto_output_tree_or_ref (ob, TYPE_ATTRIBUTES (expr), ref_p);
  lto_output_tree_or_ref (ob, TYPE_NAME (expr), ref_p);
  /* Do not stream TYPE_POINTER_TO or TYPE_REFERENCE_TO.  They will be
     reconstructed during fixup.  */
  /* Do not stream TYPE_NEXT_VARIANT, we reconstruct the variant lists
     during fixup.  */
  lto_output_tree_or_ref (ob, TYPE_MAIN_VARIANT (expr), ref_p);
  lto_output_tree_or_ref (ob, TYPE_CONTEXT (expr), ref_p);
  /* TYPE_CANONICAL is re-computed during type merging, so no need
     to stream it here.  */
  lto_output_tree_or_ref (ob, TYPE_STUB_DECL (expr), ref_p);
}

/* Write all pointer fields in the TS_TYPE_NON_COMMON structure of EXPR
   to output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_type_non_common_tree_pointers (struct output_block *ob,
					     tree expr, bool ref_p)
{
  if (TREE_CODE (expr) == ENUMERAL_TYPE)
    lto_output_tree_or_ref (ob, TYPE_VALUES (expr), ref_p);
  else if (TREE_CODE (expr) == ARRAY_TYPE)
    lto_output_tree_or_ref (ob, TYPE_DOMAIN (expr), ref_p);
  else if (RECORD_OR_UNION_TYPE_P (expr))
    lto_output_tree_or_ref (ob, TYPE_FIELDS (expr), ref_p);
  else if (TREE_CODE (expr) == FUNCTION_TYPE
	   || TREE_CODE (expr) == METHOD_TYPE)
    lto_output_tree_or_ref (ob, TYPE_ARG_TYPES (expr), ref_p);

  if (!POINTER_TYPE_P (expr))
    lto_output_tree_or_ref (ob, TYPE_MINVAL (expr), ref_p);
  lto_output_tree_or_ref (ob, TYPE_MAXVAL (expr), ref_p);
  if (RECORD_OR_UNION_TYPE_P (expr))
    lto_output_tree_or_ref (ob, TYPE_BINFO (expr), ref_p);
}


/* Write all pointer fields in the TS_LIST structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_list_tree_pointers (struct output_block *ob, tree expr,
				  bool ref_p)
{
  lto_output_tree_or_ref (ob, TREE_PURPOSE (expr), ref_p);
  lto_output_tree_or_ref (ob, TREE_VALUE (expr), ref_p);
  lto_output_chain (ob, TREE_CHAIN (expr), ref_p);
}


/* Write all pointer fields in the TS_VEC structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_vec_tree_pointers (struct output_block *ob, tree expr, bool ref_p)
{
  int i;

  /* Note that the number of slots for EXPR has already been emitted
     in EXPR's header (see lto_output_tree_header).  */
  for (i = 0; i < TREE_VEC_LENGTH (expr); i++)
    lto_output_tree_or_ref (ob, TREE_VEC_ELT (expr, i), ref_p);
}


/* Write all pointer fields in the TS_EXP structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_exp_tree_pointers (struct output_block *ob, tree expr, bool ref_p)
{
  int i;

  output_sleb128 (ob, TREE_OPERAND_LENGTH (expr));
  for (i = 0; i < TREE_OPERAND_LENGTH (expr); i++)
    lto_output_tree_or_ref (ob, TREE_OPERAND (expr, i), ref_p);
  lto_output_location (ob, EXPR_LOCATION (expr));
  lto_output_tree_or_ref (ob, TREE_BLOCK (expr), ref_p);
}


/* Write all pointer fields in the TS_BLOCK structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_block_tree_pointers (struct output_block *ob, tree expr,
				   bool ref_p)
{
  /* Do not stream BLOCK_SOURCE_LOCATION.  We cannot handle debug information
     for early inlining so drop it on the floor instead of ICEing in
     dwarf2out.c.  */
  lto_output_chain (ob, BLOCK_VARS (expr), ref_p);

  /* Do not stream BLOCK_NONLOCALIZED_VARS.  We cannot handle debug information
     for early inlining so drop it on the floor instead of ICEing in
     dwarf2out.c.  */

  lto_output_tree_or_ref (ob, BLOCK_SUPERCONTEXT (expr), ref_p);
  /* Do not stream BLOCK_ABSTRACT_ORIGIN.  We cannot handle debug information
     for early inlining so drop it on the floor instead of ICEing in
     dwarf2out.c.  */
  lto_output_tree_or_ref (ob, BLOCK_FRAGMENT_ORIGIN (expr), ref_p);
  lto_output_tree_or_ref (ob, BLOCK_FRAGMENT_CHAIN (expr), ref_p);
  /* Do not output BLOCK_SUBBLOCKS.  Instead on streaming-in this
     list is re-constructed from BLOCK_SUPERCONTEXT.  */
}


/* Write all pointer fields in the TS_BINFO structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_binfo_tree_pointers (struct output_block *ob, tree expr,
				   bool ref_p)
{
  unsigned i;
  tree t;

  /* Note that the number of BINFO slots has already been emitted in
     EXPR's header (see lto_output_tree_header) because this length
     is needed to build the empty BINFO node on the reader side.  */
  FOR_EACH_VEC_ELT (tree, BINFO_BASE_BINFOS (expr), i, t)
    lto_output_tree_or_ref (ob, t, ref_p);
  output_record_start (ob, LTO_null);

  lto_output_tree_or_ref (ob, BINFO_OFFSET (expr), ref_p);
  lto_output_tree_or_ref (ob, BINFO_VTABLE (expr), ref_p);
  /* BINFO_VIRTUALS is used to drive type based devirtualizatoin.  It often links
     together large portions of programs making it harder to partition.  Becuase
     devirtualization is interesting before inlining, only, there is no real
     need to ship it into ltrans partition.  */
  lto_output_tree_or_ref (ob, flag_wpa ? NULL : BINFO_VIRTUALS (expr), ref_p);
  lto_output_tree_or_ref (ob, BINFO_VPTR_FIELD (expr), ref_p);

  output_uleb128 (ob, VEC_length (tree, BINFO_BASE_ACCESSES (expr)));
  FOR_EACH_VEC_ELT (tree, BINFO_BASE_ACCESSES (expr), i, t)
    lto_output_tree_or_ref (ob, t, ref_p);

  lto_output_tree_or_ref (ob, BINFO_INHERITANCE_CHAIN (expr), ref_p);
  lto_output_tree_or_ref (ob, BINFO_SUBVTT_INDEX (expr), ref_p);
  lto_output_tree_or_ref (ob, BINFO_VPTR_INDEX (expr), ref_p);
}


/* Write all pointer fields in the TS_CONSTRUCTOR structure of EXPR to
   output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_constructor_tree_pointers (struct output_block *ob, tree expr,
					 bool ref_p)
{
  unsigned i;
  tree index, value;

  output_uleb128 (ob, CONSTRUCTOR_NELTS (expr));
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (expr), i, index, value)
    {
      lto_output_tree_or_ref (ob, index, ref_p);
      lto_output_tree_or_ref (ob, value, ref_p);
    }
}

/* Write a TS_TARGET_OPTION tree in EXPR to OB.  */

static void
lto_output_ts_target_option (struct output_block *ob, tree expr)
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
  lto_output_bitpack (&bp);
}

/* Write a TS_TRANSLATION_UNIT_DECL tree in EXPR to OB.  */

static void
lto_output_ts_translation_unit_decl_tree_pointers (struct output_block *ob,
						   tree expr)
{
  lto_output_string (ob, ob->main_stream,
		     TRANSLATION_UNIT_LANGUAGE (expr), true);
}

/* Helper for lto_output_tree.  Write all pointer fields in EXPR to output
   block OB.  If REF_P is true, the leaves of EXPR are emitted as
   references.  */

static void
lto_output_tree_pointers (struct output_block *ob, tree expr, bool ref_p)
{
  enum tree_code code;

  code = TREE_CODE (expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    lto_output_ts_common_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_VECTOR))
    lto_output_ts_vector_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_COMPLEX))
    lto_output_ts_complex_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    lto_output_ts_decl_minimal_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    lto_output_ts_decl_common_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_NON_COMMON))
    lto_output_ts_decl_non_common_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    lto_output_ts_decl_with_vis_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_FIELD_DECL))
    lto_output_ts_field_decl_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    lto_output_ts_function_decl_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    lto_output_ts_type_common_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON))
    lto_output_ts_type_non_common_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_LIST))
    lto_output_ts_list_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    lto_output_ts_vec_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_EXP))
    lto_output_ts_exp_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    lto_output_ts_block_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    lto_output_ts_binfo_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    lto_output_ts_constructor_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_TARGET_OPTION))
    lto_output_ts_target_option (ob, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TRANSLATION_UNIT_DECL))
    lto_output_ts_translation_unit_decl_tree_pointers (ob, expr);
}


/* Emit header information for tree EXPR to output block OB.  The header
   contains everything needed to instantiate an empty skeleton for
   EXPR on the reading side.  IX is the index into the streamer cache
   where EXPR is stored.  REF_P is as in lto_output_tree.  */

static void
lto_output_tree_header (struct output_block *ob, tree expr)
{
  enum LTO_tags tag;
  enum tree_code code;

  /* We should not see any tree nodes not handled by the streamer.  */
  code = TREE_CODE (expr);
  if (!streamer_hooks.is_streamable (expr))
    internal_error ("tree code %qs is not supported in %s streams",
		    tree_code_name[code], streamer_hooks.name);

  /* The header of a tree node consists of its tag, the size of
     the node, and any other information needed to instantiate
     EXPR on the reading side (such as the number of slots in
     variable sized nodes).  */
  tag = lto_tree_code_to_tag (code);
  output_record_start (ob, tag);

  /* The following will cause bootstrap miscomparisons.  Enable with care.  */
#ifdef LTO_STREAMER_DEBUG
  /* This is used mainly for debugging purposes.  When the reader
     and the writer do not agree on a streamed node, the pointer
     value for EXPR can be used to track down the differences in
     the debugger.  */
  gcc_assert ((HOST_WIDEST_INT) (intptr_t) expr == (intptr_t) expr);
  output_sleb128 (ob, (HOST_WIDEST_INT) (intptr_t) expr);
#endif

  /* The text in strings and identifiers are completely emitted in
     the header.  */
  if (CODE_CONTAINS_STRUCT (code, TS_STRING))
    output_string_cst (ob, ob->main_stream, expr);
  else if (CODE_CONTAINS_STRUCT (code, TS_IDENTIFIER))
    output_identifier (ob, ob->main_stream, expr);
  else if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    output_sleb128 (ob, TREE_VEC_LENGTH (expr));
  else if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    output_uleb128 (ob, BINFO_N_BASE_BINFOS (expr));

  /* Allow the streamer to write any streamer-specific information
     needed to instantiate the node when reading.  */
  if (streamer_hooks.output_tree_header)
    streamer_hooks.output_tree_header (ob, expr);
}


/* Emit the integer constant CST to output block OB.  If REF_P is true,
   CST's type will be emitted as a reference.  */

static void
lto_output_integer_cst (struct output_block *ob, tree cst, bool ref_p)
{
  output_record_start (ob, lto_tree_code_to_tag (INTEGER_CST));
  lto_output_tree_or_ref (ob, TREE_TYPE (cst), ref_p);
  lto_output_1_stream (ob->main_stream, TREE_OVERFLOW_P (cst));
  output_uleb128 (ob, TREE_INT_CST_LOW (cst));
  output_uleb128 (ob, TREE_INT_CST_HIGH (cst));
}


/* Write a physical representation of tree node EXPR to output block
   OB.  If REF_P is true, the leaves of EXPR are emitted as references
   via lto_output_tree_ref.  IX is the index into the streamer cache
   where EXPR is stored.  */

static void
lto_write_tree (struct output_block *ob, tree expr, bool ref_p)
{
  struct bitpack_d bp;

  /* Write the header, containing everything needed to materialize
     EXPR on the reading side.  */
  lto_output_tree_header (ob, expr);

  /* Pack all the non-pointer fields in EXPR into a bitpack and write
     the resulting bitpack.  */
  bp = bitpack_create (ob->main_stream);
  pack_value_fields (&bp, expr);
  lto_output_bitpack (&bp);

  /* Write all the pointer fields in EXPR.  */
  lto_output_tree_pointers (ob, expr, ref_p);

  /* Call back into the streaming module to see if it needs to write
     anything that was not written by the common streamer.  */
  if (streamer_hooks.write_tree)
    streamer_hooks.write_tree (ob, expr, ref_p);

  /* Mark the end of EXPR.  */
  output_zero (ob);
}


/* Emit the physical representation of tree node EXPR to output block
   OB.  If REF_P is true, the leaves of EXPR are emitted as references
   via lto_output_tree_ref.  */

void
lto_output_tree (struct output_block *ob, tree expr, bool ref_p)
{
  unsigned ix;
  bool existed_p;

  if (expr == NULL_TREE)
    {
      output_record_start (ob, LTO_null);
      return;
    }

  /* INTEGER_CST nodes are special because they need their original type
     to be materialized by the reader (to implement TYPE_CACHED_VALUES).  */
  if (TREE_CODE (expr) == INTEGER_CST)
    {
      lto_output_integer_cst (ob, expr, ref_p);
      return;
    }

  existed_p = lto_streamer_cache_insert (ob->writer_cache, expr, &ix);
  if (existed_p)
    {
      /* If a node has already been streamed out, make sure that
	 we don't write it more than once.  Otherwise, the reader
	 will instantiate two different nodes for the same object.  */
      output_record_start (ob, LTO_tree_pickle_reference);
      output_uleb128 (ob, ix);
      lto_output_enum (ob->main_stream, LTO_tags, LTO_NUM_TAGS,
		       lto_tree_code_to_tag (TREE_CODE (expr)));
    }
  else if (lto_stream_as_builtin_p (expr))
    {
      /* MD and NORMAL builtins do not need to be written out
	 completely as they are always instantiated by the
	 compiler on startup.  The only builtins that need to
	 be written out are BUILT_IN_FRONTEND.  For all other
	 builtins, we simply write the class and code.  */
      lto_output_builtin_tree (ob, expr);
    }
  else
    {
      /* This is the first time we see EXPR, write its fields
	 to OB.  */
      lto_write_tree (ob, expr, ref_p);
    }
}
