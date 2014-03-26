/* UndefinedBehaviorSanitizer, undefined behavior detector.
   Copyright (C) 2013-2014 Free Software Foundation, Inc.
   Contributed by Marek Polacek <polacek@redhat.com>

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
#include "tree.h"
#include "stor-layout.h"
#include "stringpool.h"
#include "cgraph.h"
#include "tree-pass.h"
#include "tree-ssa-alias.h"
#include "tree-pretty-print.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "gimple-walk.h"
#include "hashtab.h"
#include "output.h"
#include "tm_p.h"
#include "toplev.h"
#include "cfgloop.h"
#include "ubsan.h"
#include "c-family/c-common.h"
#include "rtl.h"
#include "expr.h"
#include "tree-ssanames.h"
#include "asan.h"
#include "gimplify-me.h"

/* Map from a tree to a VAR_DECL tree.  */

struct GTY(()) tree_type_map {
  struct tree_map_base type;
  tree decl;
};

#define tree_type_map_eq tree_map_base_eq
#define tree_type_map_marked_p tree_map_base_marked_p

/* Hash from a tree in a tree_type_map.  */

unsigned int
tree_type_map_hash (const void *item)
{
  return TYPE_UID (((const struct tree_type_map *)item)->type.from);
}

static GTY ((if_marked ("tree_type_map_marked_p"), param_is (struct tree_type_map)))
     htab_t decl_tree_for_type;

/* Lookup a VAR_DECL for TYPE, and return it if we find one.  */

static tree
decl_for_type_lookup (tree type)
{
  /* If the hash table is not initialized yet, create it now.  */
  if (decl_tree_for_type == NULL)
    {
      decl_tree_for_type = htab_create_ggc (10, tree_type_map_hash,
					    tree_type_map_eq, 0);
      /* That also means we don't have to bother with the lookup.  */
      return NULL_TREE;
    }

  struct tree_type_map *h, in;
  in.type.from = type;

  h = (struct tree_type_map *)
      htab_find_with_hash (decl_tree_for_type, &in, TYPE_UID (type));
  return h ? h->decl : NULL_TREE;
}

/* Insert a mapping TYPE->DECL in the VAR_DECL for type hashtable.  */

static void
decl_for_type_insert (tree type, tree decl)
{
  struct tree_type_map *h;
  void **slot;

  h = ggc_alloc_tree_type_map ();
  h->type.from = type;
  h->decl = decl;
  slot = htab_find_slot_with_hash (decl_tree_for_type, h, TYPE_UID (type),
                                  INSERT);
  *(struct tree_type_map **) slot = h;
}

/* Helper routine, which encodes a value in the pointer_sized_int_node.
   Arguments with precision <= POINTER_SIZE are passed directly,
   the rest is passed by reference.  T is a value we are to encode.
   IN_EXPAND_P is true if this function is called during expansion.  */

tree
ubsan_encode_value (tree t, bool in_expand_p)
{
  tree type = TREE_TYPE (t);
  const unsigned int bitsize = GET_MODE_BITSIZE (TYPE_MODE (type));
  if (bitsize <= POINTER_SIZE)
    switch (TREE_CODE (type))
      {
      case BOOLEAN_TYPE:
      case ENUMERAL_TYPE:
      case INTEGER_TYPE:
	return fold_build1 (NOP_EXPR, pointer_sized_int_node, t);
      case REAL_TYPE:
	{
	  tree itype = build_nonstandard_integer_type (bitsize, true);
	  t = fold_build1 (VIEW_CONVERT_EXPR, itype, t);
	  return fold_convert (pointer_sized_int_node, t);
	}
      default:
	gcc_unreachable ();
      }
  else
    {
      if (!DECL_P (t) || !TREE_ADDRESSABLE (t))
	{
	  /* The reason for this is that we don't want to pessimize
	     code by making vars unnecessarily addressable.  */
	  tree var = create_tmp_var (type, NULL);
	  tree tem = build2 (MODIFY_EXPR, void_type_node, var, t);
	  if (in_expand_p)
	    {
	      rtx mem
		= assign_stack_temp_for_type (TYPE_MODE (type),
					      GET_MODE_SIZE (TYPE_MODE (type)),
					      type);
	      SET_DECL_RTL (var, mem);
	      expand_assignment (var, t, false);
	      return build_fold_addr_expr (var);
	    }
	  t = build_fold_addr_expr (var);
	  return build2 (COMPOUND_EXPR, TREE_TYPE (t), tem, t);
	}
      else
	return build_fold_addr_expr (t);
    }
}

/* Build
   struct __ubsan_type_descriptor
   {
     unsigned short __typekind;
     unsigned short __typeinfo;
     char __typename[];
   }
   type.  */

static tree
ubsan_type_descriptor_type (void)
{
  static const char *field_names[3]
    = { "__typekind", "__typeinfo", "__typename" };
  tree fields[3], ret;
  tree itype = build_range_type (sizetype, size_zero_node, NULL_TREE);
  tree flex_arr_type = build_array_type (char_type_node, itype);

  ret = make_node (RECORD_TYPE);
  for (int i = 0; i < 3; i++)
    {
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			      get_identifier (field_names[i]),
			      (i == 2) ? flex_arr_type
			      : short_unsigned_type_node);
      DECL_CONTEXT (fields[i]) = ret;
      if (i)
	DECL_CHAIN (fields[i - 1]) = fields[i];
    }
  TYPE_FIELDS (ret) = fields[0];
  TYPE_NAME (ret) = get_identifier ("__ubsan_type_descriptor");
  layout_type (ret);
  return ret;
}

/* Build
   struct __ubsan_source_location
   {
     const char *__filename;
     unsigned int __line;
     unsigned int __column;
   }
   type.  */

static tree
ubsan_source_location_type (void)
{
  static const char *field_names[3]
    = { "__filename", "__line", "__column" };
  tree fields[3], ret;
  tree const_char_type = build_qualified_type (char_type_node,
					       TYPE_QUAL_CONST);

  ret = make_node (RECORD_TYPE);
  for (int i = 0; i < 3; i++)
    {
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			      get_identifier (field_names[i]),
			      (i == 0) ? build_pointer_type (const_char_type)
			      : unsigned_type_node);
      DECL_CONTEXT (fields[i]) = ret;
      if (i)
	DECL_CHAIN (fields[i - 1]) = fields[i];
    }
  TYPE_FIELDS (ret) = fields[0];
  TYPE_NAME (ret) = get_identifier ("__ubsan_source_location");
  layout_type (ret);
  return ret;
}

/* Helper routine that returns a CONSTRUCTOR of __ubsan_source_location
   type with its fields filled from a location_t LOC.  */

static tree
ubsan_source_location (location_t loc)
{
  expanded_location xloc;
  tree type = ubsan_source_location_type ();

  xloc = expand_location (loc);

  /* Fill in the values from LOC.  */
  size_t len = strlen (xloc.file);
  tree str = build_string (len + 1, xloc.file);
  TREE_TYPE (str) = build_array_type (char_type_node,
				      build_index_type (size_int (len)));
  TREE_READONLY (str) = 1;
  TREE_STATIC (str) = 1;
  str = build_fold_addr_expr (str);
  tree ctor = build_constructor_va (type, 3, NULL_TREE, str, NULL_TREE,
				    build_int_cst (unsigned_type_node,
						   xloc.line), NULL_TREE,
				    build_int_cst (unsigned_type_node,
						   xloc.column));
  TREE_CONSTANT (ctor) = 1;
  TREE_STATIC (ctor) = 1;

  return ctor;
}

/* This routine returns a magic number for TYPE.  */

static unsigned short
get_ubsan_type_info_for_type (tree type)
{
  gcc_assert (TYPE_SIZE (type) && tree_fits_uhwi_p (TYPE_SIZE (type)));
  int prec = exact_log2 (tree_to_uhwi (TYPE_SIZE (type)));
  gcc_assert (prec != -1);
  return (prec << 1) | !TYPE_UNSIGNED (type);
}

/* Helper routine that returns ADDR_EXPR of a VAR_DECL of a type
   descriptor.  It first looks into the hash table; if not found,
   create the VAR_DECL, put it into the hash table and return the
   ADDR_EXPR of it.  TYPE describes a particular type.  WANT_POINTER_TYPE_P
   means whether we are interested in the pointer type and not the pointer
   itself.  */

tree
ubsan_type_descriptor (tree type, bool want_pointer_type_p)
{
  /* See through any typedefs.  */
  type = TYPE_MAIN_VARIANT (type);

  tree decl = decl_for_type_lookup (type);
  /* It is possible that some of the earlier created DECLs were found
     unused, in that case they weren't emitted and varpool_get_node
     returns NULL node on them.  But now we really need them.  Thus,
     renew them here.  */
  if (decl != NULL_TREE && varpool_get_node (decl))
    return build_fold_addr_expr (decl);

  tree dtype = ubsan_type_descriptor_type ();
  tree type2 = type;
  const char *tname = NULL;
  char *pretty_name;
  unsigned char deref_depth = 0;
  unsigned short tkind, tinfo;

  /* Get the name of the type, or the name of the pointer type.  */
  if (want_pointer_type_p)
    {
      gcc_assert (POINTER_TYPE_P (type));
      type2 = TREE_TYPE (type);

      /* Remove any '*' operators from TYPE.  */
      while (POINTER_TYPE_P (type2))
        deref_depth++, type2 = TREE_TYPE (type2);

      if (TREE_CODE (type2) == METHOD_TYPE)
        type2 = TYPE_METHOD_BASETYPE (type2);
    }

  /* If an array, get its type.  */
  type2 = strip_array_types (type2);

  if (TYPE_NAME (type2) != NULL)
    {
      if (TREE_CODE (TYPE_NAME (type2)) == IDENTIFIER_NODE)
	tname = IDENTIFIER_POINTER (TYPE_NAME (type2));
      else if (DECL_NAME (TYPE_NAME (type2)) != NULL)
	tname = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type2)));
    }

  if (tname == NULL)
    /* We weren't able to determine the type name.  */
    tname = "<unknown>";

  /* Decorate the type name with '', '*', "struct", or "union".  */
  pretty_name = (char *) alloca (strlen (tname) + 16 + deref_depth);
  if (want_pointer_type_p)
    {
      int pos = sprintf (pretty_name, "'%s%s%s%s%s%s%s",
			 TYPE_VOLATILE (type2) ? "volatile " : "",
			 TYPE_READONLY (type2) ? "const " : "",
			 TYPE_RESTRICT (type2) ? "restrict " : "",
			 TYPE_ATOMIC (type2) ? "_Atomic " : "",
			 TREE_CODE (type2) == RECORD_TYPE
			 ? "struct "
			 : TREE_CODE (type2) == UNION_TYPE
			   ? "union " : "", tname,
			 deref_depth == 0 ? "" : " ");
      while (deref_depth-- > 0)
        pretty_name[pos++] = '*';
      pretty_name[pos++] = '\'';
      pretty_name[pos] = '\0';
    }
  else
    sprintf (pretty_name, "'%s'", tname);

  switch (TREE_CODE (type))
    {
    case BOOLEAN_TYPE:
    case ENUMERAL_TYPE:
    case INTEGER_TYPE:
      tkind = 0x0000;
      break;
    case REAL_TYPE:
      tkind = 0x0001;
      break;
    default:
      tkind = 0xffff;
      break;
    }
  tinfo = get_ubsan_type_info_for_type (type);

  /* Create a new VAR_DECL of type descriptor.  */
  char tmp_name[32];
  static unsigned int type_var_id_num;
  ASM_GENERATE_INTERNAL_LABEL (tmp_name, "Lubsan_type", type_var_id_num++);
  decl = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier (tmp_name),
			  dtype);
  TREE_STATIC (decl) = 1;
  TREE_PUBLIC (decl) = 0;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  DECL_EXTERNAL (decl) = 0;

  size_t len = strlen (pretty_name);
  tree str = build_string (len + 1, pretty_name);
  TREE_TYPE (str) = build_array_type (char_type_node,
				      build_index_type (size_int (len)));
  TREE_READONLY (str) = 1;
  TREE_STATIC (str) = 1;
  tree ctor = build_constructor_va (dtype, 3, NULL_TREE,
				    build_int_cst (short_unsigned_type_node,
						   tkind), NULL_TREE,
				    build_int_cst (short_unsigned_type_node,
						   tinfo), NULL_TREE, str);
  TREE_CONSTANT (ctor) = 1;
  TREE_STATIC (ctor) = 1;
  DECL_INITIAL (decl) = ctor;
  varpool_finalize_decl (decl);

  /* Save the VAR_DECL into the hash table.  */
  decl_for_type_insert (type, decl);

  return build_fold_addr_expr (decl);
}

/* Create a structure for the ubsan library.  NAME is a name of the new
   structure.  The arguments in ... are of __ubsan_type_descriptor type
   and there are at most two of them.  MISMATCH are data used by ubsan
   pointer checking.  */

tree
ubsan_create_data (const char *name, location_t loc,
		   const struct ubsan_mismatch_data *mismatch, ...)
{
  va_list args;
  tree ret, t;
  tree fields[5];
  vec<tree, va_gc> *saved_args = NULL;
  size_t i = 0;

  /* Firstly, create a pointer to type descriptor type.  */
  tree td_type = ubsan_type_descriptor_type ();
  TYPE_READONLY (td_type) = 1;
  td_type = build_pointer_type (td_type);
  loc = LOCATION_LOCUS (loc);

  /* Create the structure type.  */
  ret = make_node (RECORD_TYPE);
  if (loc != UNKNOWN_LOCATION)
    {
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE,
			      ubsan_source_location_type ());
      DECL_CONTEXT (fields[i]) = ret;
      i++;
    }

  va_start (args, mismatch);
  for (t = va_arg (args, tree); t != NULL_TREE;
       i++, t = va_arg (args, tree))
    {
      gcc_checking_assert (i < 3);
      /* Save the tree arguments for later use.  */
      vec_safe_push (saved_args, t);
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE,
			      td_type);
      DECL_CONTEXT (fields[i]) = ret;
      if (i)
	DECL_CHAIN (fields[i - 1]) = fields[i];
    }
  va_end (args);

  if (mismatch != NULL)
    {
      /* We have to add two more decls.  */
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE,
			      pointer_sized_int_node);
      DECL_CONTEXT (fields[i]) = ret;
      DECL_CHAIN (fields[i - 1]) = fields[i];
      i++;

      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE,
			      unsigned_char_type_node);
      DECL_CONTEXT (fields[i]) = ret;
      DECL_CHAIN (fields[i - 1]) = fields[i];
      i++;
    }

  TYPE_FIELDS (ret) = fields[0];
  TYPE_NAME (ret) = get_identifier (name);
  layout_type (ret);

  /* Now, fill in the type.  */
  char tmp_name[32];
  static unsigned int ubsan_var_id_num;
  ASM_GENERATE_INTERNAL_LABEL (tmp_name, "Lubsan_data", ubsan_var_id_num++);
  tree var = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier (tmp_name),
			 ret);
  TREE_STATIC (var) = 1;
  TREE_PUBLIC (var) = 0;
  DECL_ARTIFICIAL (var) = 1;
  DECL_IGNORED_P (var) = 1;
  DECL_EXTERNAL (var) = 0;

  vec<constructor_elt, va_gc> *v;
  vec_alloc (v, i);
  tree ctor = build_constructor (ret, v);

  /* If desirable, set the __ubsan_source_location element.  */
  if (loc != UNKNOWN_LOCATION)
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, ubsan_source_location (loc));

  size_t nelts = vec_safe_length (saved_args);
  for (i = 0; i < nelts; i++)
    {
      t = (*saved_args)[i];
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, t);
    }

  if (mismatch != NULL)
    {
      /* Append the pointer data.  */
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, mismatch->align);
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, mismatch->ckind);
    }

  TREE_CONSTANT (ctor) = 1;
  TREE_STATIC (ctor) = 1;
  DECL_INITIAL (var) = ctor;
  varpool_finalize_decl (var);

  return var;
}

/* Instrument the __builtin_unreachable call.  We just call the libubsan
   routine instead.  */

tree
ubsan_instrument_unreachable (location_t loc)
{
  initialize_sanitizer_builtins ();
  tree data = ubsan_create_data ("__ubsan_unreachable_data", loc, NULL,
				 NULL_TREE);
  tree t = builtin_decl_explicit (BUILT_IN_UBSAN_HANDLE_BUILTIN_UNREACHABLE);
  return build_call_expr_loc (loc, t, 1, build_fold_addr_expr_loc (loc, data));
}

/* Return true if T is a call to a libubsan routine.  */

bool
is_ubsan_builtin_p (tree t)
{
  gcc_checking_assert (TREE_CODE (t) == FUNCTION_DECL);
  return strncmp (IDENTIFIER_POINTER (DECL_NAME (t)),
		  "__builtin___ubsan_", 18) == 0;
}

/* Expand UBSAN_NULL internal call.  */

void
ubsan_expand_null_ifn (gimple_stmt_iterator gsi)
{
  gimple stmt = gsi_stmt (gsi);
  location_t loc = gimple_location (stmt);
  gcc_assert (gimple_call_num_args (stmt) == 2);
  tree ptr = gimple_call_arg (stmt, 0);
  tree ckind = gimple_call_arg (stmt, 1);

  basic_block cur_bb = gsi_bb (gsi);

  /* Split the original block holding the pointer dereference.  */
  edge e = split_block (cur_bb, stmt);

  /* Get a hold on the 'condition block', the 'then block' and the
     'else block'.  */
  basic_block cond_bb = e->src;
  basic_block fallthru_bb = e->dest;
  basic_block then_bb = create_empty_bb (cond_bb);
  if (current_loops)
    {
      add_bb_to_loop (then_bb, cond_bb->loop_father);
      loops_state_set (LOOPS_NEED_FIXUP);
    }

  /* Make an edge coming from the 'cond block' into the 'then block';
     this edge is unlikely taken, so set up the probability accordingly.  */
  e = make_edge (cond_bb, then_bb, EDGE_TRUE_VALUE);
  e->probability = PROB_VERY_UNLIKELY;

  /* Connect 'then block' with the 'else block'.  This is needed
     as the ubsan routines we call in the 'then block' are not noreturn.
     The 'then block' only has one outcoming edge.  */
  make_single_succ_edge (then_bb, fallthru_bb, EDGE_FALLTHRU);

  /* Set up the fallthrough basic block.  */
  e = find_edge (cond_bb, fallthru_bb);
  e->flags = EDGE_FALSE_VALUE;
  e->count = cond_bb->count;
  e->probability = REG_BR_PROB_BASE - PROB_VERY_UNLIKELY;

  /* Update dominance info for the newly created then_bb; note that
     fallthru_bb's dominance info has already been updated by
     split_bock.  */
  if (dom_info_available_p (CDI_DOMINATORS))
    set_immediate_dominator (CDI_DOMINATORS, then_bb, cond_bb);

  /* Put the ubsan builtin call into the newly created BB.  */
  tree fn = builtin_decl_implicit (BUILT_IN_UBSAN_HANDLE_TYPE_MISMATCH);
  const struct ubsan_mismatch_data m
    = { build_zero_cst (pointer_sized_int_node), ckind };
  tree data = ubsan_create_data ("__ubsan_null_data",
				 loc, &m,
				 ubsan_type_descriptor (TREE_TYPE (ptr), true),
				 NULL_TREE);
  data = build_fold_addr_expr_loc (loc, data);
  gimple g = gimple_build_call (fn, 2, data,
				build_zero_cst (pointer_sized_int_node));
  gimple_set_location (g, loc);
  gimple_stmt_iterator gsi2 = gsi_start_bb (then_bb);
  gsi_insert_after (&gsi2, g, GSI_NEW_STMT);

  /* Unlink the UBSAN_NULLs vops before replacing it.  */
  unlink_stmt_vdef (stmt);

  g = gimple_build_cond (EQ_EXPR, ptr, build_int_cst (TREE_TYPE (ptr), 0),
			 NULL_TREE, NULL_TREE);
  gimple_set_location (g, loc);

  /* Replace the UBSAN_NULL with a GIMPLE_COND stmt.  */
  gsi_replace (&gsi, g, false);
}

/* Instrument a member call.  We check whether 'this' is NULL.  */

static void
instrument_member_call (gimple_stmt_iterator *iter)
{
  tree this_parm = gimple_call_arg (gsi_stmt (*iter), 0);
  tree kind = build_int_cst (unsigned_char_type_node, UBSAN_MEMBER_CALL);
  gimple g = gimple_build_call_internal (IFN_UBSAN_NULL, 2, this_parm, kind);
  gimple_set_location (g, gimple_location (gsi_stmt (*iter)));
  gsi_insert_before (iter, g, GSI_SAME_STMT);
}

/* Instrument a memory reference.  T is the pointer, IS_LHS says
   whether the pointer is on the left hand side of the assignment.  */

static void
instrument_mem_ref (tree t, gimple_stmt_iterator *iter, bool is_lhs)
{
  enum ubsan_null_ckind ikind = is_lhs ? UBSAN_STORE_OF : UBSAN_LOAD_OF;
  if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (TREE_TYPE (t))))
    ikind = UBSAN_MEMBER_ACCESS;
  tree kind = build_int_cst (unsigned_char_type_node, ikind);
  gimple g = gimple_build_call_internal (IFN_UBSAN_NULL, 2, t, kind);
  gimple_set_location (g, gimple_location (gsi_stmt (*iter)));
  gsi_insert_before (iter, g, GSI_SAME_STMT);
}

/* Perform the pointer instrumentation.  */

static void
instrument_null (gimple_stmt_iterator gsi, bool is_lhs)
{
  gimple stmt = gsi_stmt (gsi);
  tree t = is_lhs ? gimple_get_lhs (stmt) : gimple_assign_rhs1 (stmt);
  t = get_base_address (t);
  const enum tree_code code = TREE_CODE (t);
  if (code == MEM_REF
      && TREE_CODE (TREE_OPERAND (t, 0)) == SSA_NAME)
    instrument_mem_ref (TREE_OPERAND (t, 0), &gsi, is_lhs);
  else if (code == ADDR_EXPR
	   && POINTER_TYPE_P (TREE_TYPE (t))
	   && TREE_CODE (TREE_TYPE (TREE_TYPE (t))) == METHOD_TYPE)
    instrument_member_call (&gsi);
}

/* Build an ubsan builtin call for the signed-integer-overflow
   sanitization.  CODE says what kind of builtin are we building,
   LOC is a location, LHSTYPE is the type of LHS, OP0 and OP1
   are operands of the binary operation.  */

tree
ubsan_build_overflow_builtin (tree_code code, location_t loc, tree lhstype,
			      tree op0, tree op1)
{
  tree data = ubsan_create_data ("__ubsan_overflow_data", loc, NULL,
				 ubsan_type_descriptor (lhstype, false),
				 NULL_TREE);
  enum built_in_function fn_code;

  switch (code)
    {
    case PLUS_EXPR:
      fn_code = BUILT_IN_UBSAN_HANDLE_ADD_OVERFLOW;
      break;
    case MINUS_EXPR:
      fn_code = BUILT_IN_UBSAN_HANDLE_SUB_OVERFLOW;
      break;
    case MULT_EXPR:
      fn_code = BUILT_IN_UBSAN_HANDLE_MUL_OVERFLOW;
      break;
    case NEGATE_EXPR:
      fn_code = BUILT_IN_UBSAN_HANDLE_NEGATE_OVERFLOW;
      break;
    default:
      gcc_unreachable ();
    }
  tree fn = builtin_decl_explicit (fn_code);
  return build_call_expr_loc (loc, fn, 2 + (code != NEGATE_EXPR),
			      build_fold_addr_expr_loc (loc, data),
			      ubsan_encode_value (op0, true),
			      op1 ? ubsan_encode_value (op1, true)
				  : NULL_TREE);
}

/* Perform the signed integer instrumentation.  GSI is the iterator
   pointing at statement we are trying to instrument.  */

static void
instrument_si_overflow (gimple_stmt_iterator gsi)
{
  gimple stmt = gsi_stmt (gsi);
  tree_code code = gimple_assign_rhs_code (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  tree lhstype = TREE_TYPE (lhs);
  tree a, b;
  gimple g;

  /* If this is not a signed operation, don't instrument anything here.
     Also punt on bit-fields.  */
  if (!INTEGRAL_TYPE_P (lhstype)
      || TYPE_OVERFLOW_WRAPS (lhstype)
      || GET_MODE_BITSIZE (TYPE_MODE (lhstype)) != TYPE_PRECISION (lhstype))
    return;

  switch (code)
    {
    case MINUS_EXPR:
    case PLUS_EXPR:
    case MULT_EXPR:
      /* Transform
	 i = u {+,-,*} 5;
	 into
	 i = UBSAN_CHECK_{ADD,SUB,MUL} (u, 5);  */
      a = gimple_assign_rhs1 (stmt);
      b = gimple_assign_rhs2 (stmt);
      g = gimple_build_call_internal (code == PLUS_EXPR
				      ? IFN_UBSAN_CHECK_ADD
				      : code == MINUS_EXPR
				      ? IFN_UBSAN_CHECK_SUB
				      : IFN_UBSAN_CHECK_MUL, 2, a, b);
      gimple_call_set_lhs (g, lhs);
      gsi_replace (&gsi, g, false);
      break;
    case NEGATE_EXPR:
      /* Represent i = -u;
	 as
	 i = UBSAN_CHECK_SUB (0, u);  */
      a = build_int_cst (lhstype, 0);
      b = gimple_assign_rhs1 (stmt);
      g = gimple_build_call_internal (IFN_UBSAN_CHECK_SUB, 2, a, b);
      gimple_call_set_lhs (g, lhs);
      gsi_replace (&gsi, g, false);
      break;
    case ABS_EXPR:
      /* Transform i = ABS_EXPR<u>;
	 into
	 _N = UBSAN_CHECK_SUB (0, u);
	 i = ABS_EXPR<_N>;  */
      a = build_int_cst (lhstype, 0);
      b = gimple_assign_rhs1 (stmt);
      g = gimple_build_call_internal (IFN_UBSAN_CHECK_SUB, 2, a, b);
      a = make_ssa_name (lhstype, NULL);
      gimple_call_set_lhs (g, a);
      gimple_set_location (g, gimple_location (stmt));
      gsi_insert_before (&gsi, g, GSI_SAME_STMT);
      gimple_assign_set_rhs1 (stmt, a);
      update_stmt (stmt);
      break;
    default:
      break;
    }
}

/* Instrument loads from (non-bitfield) bool and C++ enum values
   to check if the memory value is outside of the range of the valid
   type values.  */

static void
instrument_bool_enum_load (gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);
  tree rhs = gimple_assign_rhs1 (stmt);
  tree type = TREE_TYPE (rhs);
  tree minv = NULL_TREE, maxv = NULL_TREE;

  if (TREE_CODE (type) == BOOLEAN_TYPE && (flag_sanitize & SANITIZE_BOOL))
    {
      minv = boolean_false_node;
      maxv = boolean_true_node;
    }
  else if (TREE_CODE (type) == ENUMERAL_TYPE
	   && (flag_sanitize & SANITIZE_ENUM)
	   && TREE_TYPE (type) != NULL_TREE
	   && TREE_CODE (TREE_TYPE (type)) == INTEGER_TYPE
	   && (TYPE_PRECISION (TREE_TYPE (type))
	       < GET_MODE_PRECISION (TYPE_MODE (type))))
    {
      minv = TYPE_MIN_VALUE (TREE_TYPE (type));
      maxv = TYPE_MAX_VALUE (TREE_TYPE (type));
    }
  else
    return;

  int modebitsize = GET_MODE_BITSIZE (TYPE_MODE (type));
  HOST_WIDE_INT bitsize, bitpos;
  tree offset;
  enum machine_mode mode;
  int volatilep = 0, unsignedp = 0;
  tree base = get_inner_reference (rhs, &bitsize, &bitpos, &offset, &mode,
				   &unsignedp, &volatilep, false);
  tree utype = build_nonstandard_integer_type (modebitsize, 1);

  if ((TREE_CODE (base) == VAR_DECL && DECL_HARD_REGISTER (base))
      || (bitpos % modebitsize) != 0
      || bitsize != modebitsize
      || GET_MODE_BITSIZE (TYPE_MODE (utype)) != modebitsize
      || TREE_CODE (gimple_assign_lhs (stmt)) != SSA_NAME)
    return;

  location_t loc = gimple_location (stmt);
  tree ptype = build_pointer_type (TREE_TYPE (rhs));
  tree atype = reference_alias_ptr_type (rhs);
  gimple g = gimple_build_assign (make_ssa_name (ptype, NULL),
				  build_fold_addr_expr (rhs));
  gimple_set_location (g, loc);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
  tree mem = build2 (MEM_REF, utype, gimple_assign_lhs (g),
		     build_int_cst (atype, 0));
  tree urhs = make_ssa_name (utype, NULL);
  g = gimple_build_assign (urhs, mem);
  gimple_set_location (g, loc);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
  minv = fold_convert (utype, minv);
  maxv = fold_convert (utype, maxv);
  if (!integer_zerop (minv))
    {
      g = gimple_build_assign_with_ops (MINUS_EXPR,
					make_ssa_name (utype, NULL),
					urhs, minv);
      gimple_set_location (g, loc);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
    }

  gimple_stmt_iterator gsi2 = *gsi;
  basic_block then_bb, fallthru_bb;
  *gsi = create_cond_insert_point (gsi, true, false, true,
				   &then_bb, &fallthru_bb);
  g = gimple_build_cond (GT_EXPR, gimple_assign_lhs (g),
			 int_const_binop (MINUS_EXPR, maxv, minv),
			 NULL_TREE, NULL_TREE);
  gimple_set_location (g, loc);
  gsi_insert_after (gsi, g, GSI_NEW_STMT);

  gimple_assign_set_rhs_with_ops (&gsi2, NOP_EXPR, urhs, NULL_TREE);
  update_stmt (stmt);

  tree data = ubsan_create_data ("__ubsan_invalid_value_data",
				 loc, NULL,
				 ubsan_type_descriptor (type, false),
				 NULL_TREE);
  data = build_fold_addr_expr_loc (loc, data);
  tree fn = builtin_decl_explicit (BUILT_IN_UBSAN_HANDLE_LOAD_INVALID_VALUE);

  gsi2 = gsi_after_labels (then_bb);
  tree val = force_gimple_operand_gsi (&gsi2, ubsan_encode_value (urhs),
				       true, NULL_TREE, true, GSI_SAME_STMT);
  g = gimple_build_call (fn, 2, data, val);
  gimple_set_location (g, loc);
  gsi_insert_before (&gsi2, g, GSI_SAME_STMT);
}

/* Gate and execute functions for ubsan pass.  */

static unsigned int
ubsan_pass (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;

  initialize_sanitizer_builtins ();

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
	{
	  gimple stmt = gsi_stmt (gsi);
	  if (is_gimple_debug (stmt) || gimple_clobber_p (stmt))
	    {
	      gsi_next (&gsi);
	      continue;
	    }

	  if ((flag_sanitize & SANITIZE_SI_OVERFLOW)
	      && is_gimple_assign (stmt))
	    instrument_si_overflow (gsi);

	  if (flag_sanitize & SANITIZE_NULL)
	    {
	      if (gimple_store_p (stmt))
		instrument_null (gsi, true);
	      if (gimple_assign_load_p (stmt))
		instrument_null (gsi, false);
	    }

	  if (flag_sanitize & (SANITIZE_BOOL | SANITIZE_ENUM)
	      && gimple_assign_load_p (stmt))
	    instrument_bool_enum_load (&gsi);

	  gsi_next (&gsi);
	}
    }
  return 0;
}

static bool
gate_ubsan (void)
{
  return flag_sanitize & (SANITIZE_NULL | SANITIZE_SI_OVERFLOW
			  | SANITIZE_BOOL | SANITIZE_ENUM);
}

namespace {

const pass_data pass_data_ubsan =
{
  GIMPLE_PASS, /* type */
  "ubsan", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_UBSAN, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_ubsan : public gimple_opt_pass
{
public:
  pass_ubsan (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_ubsan, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_ubsan (); }
  unsigned int execute () { return ubsan_pass (); }

}; // class pass_ubsan

} // anon namespace

gimple_opt_pass *
make_pass_ubsan (gcc::context *ctxt)
{
  return new pass_ubsan (ctxt);
}

#include "gt-ubsan.h"
