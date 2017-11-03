/* UndefinedBehaviorSanitizer, undefined behavior detector.
   Copyright (C) 2013-2017 Free Software Foundation, Inc.
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
#include "backend.h"
#include "rtl.h"
#include "c-family/c-common.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "memmodel.h"
#include "tm_p.h"
#include "ssa.h"
#include "cgraph.h"
#include "tree-pretty-print.h"
#include "stor-layout.h"
#include "cfganal.h"
#include "gimple-iterator.h"
#include "output.h"
#include "cfgloop.h"
#include "ubsan.h"
#include "expr.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "gimplify-me.h"
#include "dfp.h"
#include "builtins.h"
#include "tree-object-size.h"
#include "tree-cfg.h"
#include "gimple-fold.h"
#include "varasm.h"

/* Map from a tree to a VAR_DECL tree.  */

struct GTY((for_user)) tree_type_map {
  struct tree_map_base type;
  tree decl;
};

struct tree_type_map_cache_hasher : ggc_cache_ptr_hash<tree_type_map>
{
  static inline hashval_t
  hash (tree_type_map *t)
  {
    return TYPE_UID (t->type.from);
  }

  static inline bool
  equal (tree_type_map *a, tree_type_map *b)
  {
    return a->type.from == b->type.from;
  }

  static int
  keep_cache_entry (tree_type_map *&m)
  {
    return ggc_marked_p (m->type.from);
  }
};

static GTY ((cache))
     hash_table<tree_type_map_cache_hasher> *decl_tree_for_type;

/* Lookup a VAR_DECL for TYPE, and return it if we find one.  */

static tree
decl_for_type_lookup (tree type)
{
  /* If the hash table is not initialized yet, create it now.  */
  if (decl_tree_for_type == NULL)
    {
      decl_tree_for_type
	= hash_table<tree_type_map_cache_hasher>::create_ggc (10);
      /* That also means we don't have to bother with the lookup.  */
      return NULL_TREE;
    }

  struct tree_type_map *h, in;
  in.type.from = type;

  h = decl_tree_for_type->find_with_hash (&in, TYPE_UID (type));
  return h ? h->decl : NULL_TREE;
}

/* Insert a mapping TYPE->DECL in the VAR_DECL for type hashtable.  */

static void
decl_for_type_insert (tree type, tree decl)
{
  struct tree_type_map *h;

  h = ggc_alloc<tree_type_map> ();
  h->type.from = type;
  h->decl = decl;
  *decl_tree_for_type->find_slot_with_hash (h, TYPE_UID (type), INSERT) = h;
}

/* Helper routine, which encodes a value in the pointer_sized_int_node.
   Arguments with precision <= POINTER_SIZE are passed directly,
   the rest is passed by reference.  T is a value we are to encode.
   PHASE determines when this function is called.  */

tree
ubsan_encode_value (tree t, enum ubsan_encode_value_phase phase)
{
  tree type = TREE_TYPE (t);
  scalar_mode mode = SCALAR_TYPE_MODE (type);
  const unsigned int bitsize = GET_MODE_BITSIZE (mode);
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
	  tree var;
	  if (phase != UBSAN_ENCODE_VALUE_GENERIC)
	    {
	      var = create_tmp_var (type);
	      mark_addressable (var);
	    }
	  else
	    {
	      var = create_tmp_var_raw (type);
	      TREE_ADDRESSABLE (var) = 1;
	      DECL_CONTEXT (var) = current_function_decl;
	    }
	  if (phase == UBSAN_ENCODE_VALUE_RTL)
	    {
	      rtx mem = assign_stack_temp_for_type (mode, GET_MODE_SIZE (mode),
						    type);
	      SET_DECL_RTL (var, mem);
	      expand_assignment (var, t, false);
	      return build_fold_addr_expr (var);
	    }
	  if (phase != UBSAN_ENCODE_VALUE_GENERIC)
	    {
	      tree tem = build2 (MODIFY_EXPR, void_type_node, var, t);
	      t = build_fold_addr_expr (var);
	      return build2 (COMPOUND_EXPR, TREE_TYPE (t), tem, t);
	    }
	  else
	    {
	      var = build4 (TARGET_EXPR, type, var, t, NULL_TREE, NULL_TREE);
	      return build_fold_addr_expr (var);
	    }
	}
      else
	return build_fold_addr_expr (t);
    }
}

/* Cached ubsan_get_type_descriptor_type () return value.  */
static GTY(()) tree ubsan_type_descriptor_type;

/* Build
   struct __ubsan_type_descriptor
   {
     unsigned short __typekind;
     unsigned short __typeinfo;
     char __typename[];
   }
   type.  */

static tree
ubsan_get_type_descriptor_type (void)
{
  static const char *field_names[3]
    = { "__typekind", "__typeinfo", "__typename" };
  tree fields[3], ret;

  if (ubsan_type_descriptor_type)
    return ubsan_type_descriptor_type;

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
  tree type_decl = build_decl (input_location, TYPE_DECL,
			       get_identifier ("__ubsan_type_descriptor"),
			       ret);
  DECL_IGNORED_P (type_decl) = 1;
  DECL_ARTIFICIAL (type_decl) = 1;
  TYPE_FIELDS (ret) = fields[0];
  TYPE_NAME (ret) = type_decl;
  TYPE_STUB_DECL (ret) = type_decl;
  layout_type (ret);
  ubsan_type_descriptor_type = ret;
  return ret;
}

/* Cached ubsan_get_source_location_type () return value.  */
static GTY(()) tree ubsan_source_location_type;

/* Build
   struct __ubsan_source_location
   {
     const char *__filename;
     unsigned int __line;
     unsigned int __column;
   }
   type.  */

tree
ubsan_get_source_location_type (void)
{
  static const char *field_names[3]
    = { "__filename", "__line", "__column" };
  tree fields[3], ret;
  if (ubsan_source_location_type)
    return ubsan_source_location_type;

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
  tree type_decl = build_decl (input_location, TYPE_DECL,
			       get_identifier ("__ubsan_source_location"),
			       ret);
  DECL_IGNORED_P (type_decl) = 1;
  DECL_ARTIFICIAL (type_decl) = 1;
  TYPE_FIELDS (ret) = fields[0];
  TYPE_NAME (ret) = type_decl;
  TYPE_STUB_DECL (ret) = type_decl;
  layout_type (ret);
  ubsan_source_location_type = ret;
  return ret;
}

/* Helper routine that returns a CONSTRUCTOR of __ubsan_source_location
   type with its fields filled from a location_t LOC.  */

static tree
ubsan_source_location (location_t loc)
{
  expanded_location xloc;
  tree type = ubsan_get_source_location_type ();

  xloc = expand_location (loc);
  tree str;
  if (xloc.file == NULL)
    {
      str = build_int_cst (ptr_type_node, 0);
      xloc.line = 0;
      xloc.column = 0;
    }
  else
    {
      /* Fill in the values from LOC.  */
      size_t len = strlen (xloc.file) + 1;
      str = build_string (len, xloc.file);
      TREE_TYPE (str) = build_array_type_nelts (char_type_node, len);
      TREE_READONLY (str) = 1;
      TREE_STATIC (str) = 1;
      str = build_fold_addr_expr (str);
    }
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
  if (TREE_CODE (type) == REAL_TYPE)
    return tree_to_uhwi (TYPE_SIZE (type));
  else if (INTEGRAL_TYPE_P (type))
    {
      int prec = exact_log2 (tree_to_uhwi (TYPE_SIZE (type)));
      gcc_assert (prec != -1);
      return (prec << 1) | !TYPE_UNSIGNED (type);
    }
  else
    return 0;
}

/* Counters for internal labels.  ubsan_ids[0] for Lubsan_type,
   ubsan_ids[1] for Lubsan_data labels.  */
static GTY(()) unsigned int ubsan_ids[2];

/* Helper routine that returns ADDR_EXPR of a VAR_DECL of a type
   descriptor.  It first looks into the hash table; if not found,
   create the VAR_DECL, put it into the hash table and return the
   ADDR_EXPR of it.  TYPE describes a particular type.  PSTYLE is
   an enum controlling how we want to print the type.  */

tree
ubsan_type_descriptor (tree type, enum ubsan_print_style pstyle)
{
  /* See through any typedefs.  */
  type = TYPE_MAIN_VARIANT (type);

  tree decl = decl_for_type_lookup (type);
  /* It is possible that some of the earlier created DECLs were found
     unused, in that case they weren't emitted and varpool_node::get
     returns NULL node on them.  But now we really need them.  Thus,
     renew them here.  */
  if (decl != NULL_TREE && varpool_node::get (decl))
    return build_fold_addr_expr (decl);

  tree dtype = ubsan_get_type_descriptor_type ();
  tree type2 = type;
  const char *tname = NULL;
  pretty_printer pretty_name;
  unsigned char deref_depth = 0;
  unsigned short tkind, tinfo;

  /* Get the name of the type, or the name of the pointer type.  */
  if (pstyle == UBSAN_PRINT_POINTER)
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

  if (pstyle == UBSAN_PRINT_ARRAY)
    {
      while (POINTER_TYPE_P (type2))
        deref_depth++, type2 = TREE_TYPE (type2);
    }

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

  tree eltype = type;
  if (pstyle == UBSAN_PRINT_POINTER)
    {
      pp_printf (&pretty_name, "'%s%s%s%s%s%s%s",
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
	pp_star (&pretty_name);
      pp_quote (&pretty_name);
    }
  else if (pstyle == UBSAN_PRINT_ARRAY)
    {
      /* Pretty print the array dimensions.  */
      gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
      tree t = type;
      pp_printf (&pretty_name, "'%s ", tname);
      while (deref_depth-- > 0)
	pp_star (&pretty_name);
      while (TREE_CODE (t) == ARRAY_TYPE)
	{
	  pp_left_bracket (&pretty_name);
	  tree dom = TYPE_DOMAIN (t);
	  if (dom != NULL_TREE
	      && TYPE_MAX_VALUE (dom) != NULL_TREE
	      && TREE_CODE (TYPE_MAX_VALUE (dom)) == INTEGER_CST)
	    {
	      if (tree_fits_uhwi_p (TYPE_MAX_VALUE (dom))
		  && tree_to_uhwi (TYPE_MAX_VALUE (dom)) + 1 != 0)
		pp_printf (&pretty_name, HOST_WIDE_INT_PRINT_DEC,
			    tree_to_uhwi (TYPE_MAX_VALUE (dom)) + 1);
	      else
		pp_wide_int (&pretty_name,
			     wi::add (wi::to_widest (TYPE_MAX_VALUE (dom)), 1),
			     TYPE_SIGN (TREE_TYPE (dom)));
	    }
	  else
	    /* ??? We can't determine the variable name; print VLA unspec.  */
	    pp_star (&pretty_name);
	  pp_right_bracket (&pretty_name);
	  t = TREE_TYPE (t);
	}
      pp_quote (&pretty_name);

      /* Save the tree with stripped types.  */
      eltype = t;
    }
  else
    pp_printf (&pretty_name, "'%s'", tname);

  switch (TREE_CODE (eltype))
    {
    case BOOLEAN_TYPE:
    case ENUMERAL_TYPE:
    case INTEGER_TYPE:
      tkind = 0x0000;
      break;
    case REAL_TYPE:
      /* FIXME: libubsan right now only supports float, double and
	 long double type formats.  */
      if (TYPE_MODE (eltype) == TYPE_MODE (float_type_node)
	  || TYPE_MODE (eltype) == TYPE_MODE (double_type_node)
	  || TYPE_MODE (eltype) == TYPE_MODE (long_double_type_node))
	tkind = 0x0001;
      else
	tkind = 0xffff;
      break;
    default:
      tkind = 0xffff;
      break;
    }
  tinfo = get_ubsan_type_info_for_type (eltype);

  /* Create a new VAR_DECL of type descriptor.  */
  const char *tmp = pp_formatted_text (&pretty_name);
  size_t len = strlen (tmp) + 1;
  tree str = build_string (len, tmp);
  TREE_TYPE (str) = build_array_type_nelts (char_type_node, len);
  TREE_READONLY (str) = 1;
  TREE_STATIC (str) = 1;

  char tmp_name[32];
  ASM_GENERATE_INTERNAL_LABEL (tmp_name, "Lubsan_type", ubsan_ids[0]++);
  decl = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier (tmp_name),
		     dtype);
  TREE_STATIC (decl) = 1;
  TREE_PUBLIC (decl) = 0;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  DECL_EXTERNAL (decl) = 0;
  DECL_SIZE (decl)
    = size_binop (PLUS_EXPR, DECL_SIZE (decl), TYPE_SIZE (TREE_TYPE (str)));
  DECL_SIZE_UNIT (decl)
    = size_binop (PLUS_EXPR, DECL_SIZE_UNIT (decl),
		  TYPE_SIZE_UNIT (TREE_TYPE (str)));

  tree ctor = build_constructor_va (dtype, 3, NULL_TREE,
				    build_int_cst (short_unsigned_type_node,
						   tkind), NULL_TREE,
				    build_int_cst (short_unsigned_type_node,
						   tinfo), NULL_TREE, str);
  TREE_CONSTANT (ctor) = 1;
  TREE_STATIC (ctor) = 1;
  DECL_INITIAL (decl) = ctor;
  varpool_node::finalize_decl (decl);

  /* Save the VAR_DECL into the hash table.  */
  decl_for_type_insert (type, decl);

  return build_fold_addr_expr (decl);
}

/* Create a structure for the ubsan library.  NAME is a name of the new
   structure.  LOCCNT is number of locations, PLOC points to array of
   locations.  The arguments in ... are of __ubsan_type_descriptor type
   and there are at most two of them, followed by NULL_TREE, followed
   by optional extra arguments and another NULL_TREE.  */

tree
ubsan_create_data (const char *name, int loccnt, const location_t *ploc, ...)
{
  va_list args;
  tree ret, t;
  tree fields[6];
  vec<tree, va_gc> *saved_args = NULL;
  size_t i = 0;
  int j;

  /* It is possible that PCH zapped table with definitions of sanitizer
     builtins.  Reinitialize them if needed.  */
  initialize_sanitizer_builtins ();

  /* Firstly, create a pointer to type descriptor type.  */
  tree td_type = ubsan_get_type_descriptor_type ();
  td_type = build_pointer_type (td_type);

  /* Create the structure type.  */
  ret = make_node (RECORD_TYPE);
  for (j = 0; j < loccnt; j++)
    {
      gcc_checking_assert (i < 2);
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE,
			      ubsan_get_source_location_type ());
      DECL_CONTEXT (fields[i]) = ret;
      if (i)
	DECL_CHAIN (fields[i - 1]) = fields[i];
      i++;
    }

  va_start (args, ploc);
  for (t = va_arg (args, tree); t != NULL_TREE;
       i++, t = va_arg (args, tree))
    {
      gcc_checking_assert (i < 4);
      /* Save the tree arguments for later use.  */
      vec_safe_push (saved_args, t);
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE,
			      td_type);
      DECL_CONTEXT (fields[i]) = ret;
      if (i)
	DECL_CHAIN (fields[i - 1]) = fields[i];
    }

  for (t = va_arg (args, tree); t != NULL_TREE;
       i++, t = va_arg (args, tree))
    {
      gcc_checking_assert (i < 6);
      /* Save the tree arguments for later use.  */
      vec_safe_push (saved_args, t);
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE,
			      TREE_TYPE (t));
      DECL_CONTEXT (fields[i]) = ret;
      if (i)
	DECL_CHAIN (fields[i - 1]) = fields[i];
    }
  va_end (args);

  tree type_decl = build_decl (input_location, TYPE_DECL,
			       get_identifier (name), ret);
  DECL_IGNORED_P (type_decl) = 1;
  DECL_ARTIFICIAL (type_decl) = 1;
  TYPE_FIELDS (ret) = fields[0];
  TYPE_NAME (ret) = type_decl;
  TYPE_STUB_DECL (ret) = type_decl;
  layout_type (ret);

  /* Now, fill in the type.  */
  char tmp_name[32];
  ASM_GENERATE_INTERNAL_LABEL (tmp_name, "Lubsan_data", ubsan_ids[1]++);
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
  for (j = 0; j < loccnt; j++)
    {
      location_t loc = LOCATION_LOCUS (ploc[j]);
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, ubsan_source_location (loc));
    } 

  size_t nelts = vec_safe_length (saved_args);
  for (i = 0; i < nelts; i++)
    {
      t = (*saved_args)[i];
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, t);
    }

  TREE_CONSTANT (ctor) = 1;
  TREE_STATIC (ctor) = 1;
  DECL_INITIAL (var) = ctor;
  varpool_node::finalize_decl (var);

  return var;
}

/* Instrument the __builtin_unreachable call.  We just call the libubsan
   routine instead.  */

bool
ubsan_instrument_unreachable (gimple_stmt_iterator *gsi)
{
  gimple *g;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  if (flag_sanitize_undefined_trap_on_error)
    g = gimple_build_call (builtin_decl_explicit (BUILT_IN_TRAP), 0);
  else
    {
      tree data = ubsan_create_data ("__ubsan_unreachable_data", 1, &loc,
				     NULL_TREE, NULL_TREE);
      data = build_fold_addr_expr_loc (loc, data);
      tree fn
	= builtin_decl_explicit (BUILT_IN_UBSAN_HANDLE_BUILTIN_UNREACHABLE);
      g = gimple_build_call (fn, 1, data);
    }
  gimple_set_location (g, loc);
  gsi_replace (gsi, g, false);
  return false;
}

/* Return true if T is a call to a libubsan routine.  */

bool
is_ubsan_builtin_p (tree t)
{
  return TREE_CODE (t) == FUNCTION_DECL
	 && DECL_BUILT_IN_CLASS (t) == BUILT_IN_NORMAL
	 && strncmp (IDENTIFIER_POINTER (DECL_NAME (t)),
		     "__builtin___ubsan_", 18) == 0;
}

/* Create a callgraph edge for statement STMT.  */

static void
ubsan_create_edge (gimple *stmt)
{
  gcall *call_stmt = dyn_cast <gcall *> (stmt);
  basic_block bb = gimple_bb (stmt);
  int freq = compute_call_stmt_bb_frequency (current_function_decl, bb);
  cgraph_node *node = cgraph_node::get (current_function_decl);
  tree decl = gimple_call_fndecl (call_stmt);
  if (decl)
    node->create_edge (cgraph_node::get_create (decl), call_stmt, bb->count,
		       freq);
}

/* Expand the UBSAN_BOUNDS special builtin function.  */

bool
ubsan_expand_bounds_ifn (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  location_t loc = gimple_location (stmt);
  gcc_assert (gimple_call_num_args (stmt) == 3);

  /* Pick up the arguments of the UBSAN_BOUNDS call.  */
  tree type = TREE_TYPE (TREE_TYPE (gimple_call_arg (stmt, 0)));
  tree index = gimple_call_arg (stmt, 1);
  tree orig_index = index;
  tree bound = gimple_call_arg (stmt, 2);

  gimple_stmt_iterator gsi_orig = *gsi;

  /* Create condition "if (index > bound)".  */
  basic_block then_bb, fallthru_bb;
  gimple_stmt_iterator cond_insert_point
    = create_cond_insert_point (gsi, false, false, true,
				&then_bb, &fallthru_bb);
  index = fold_convert (TREE_TYPE (bound), index);
  index = force_gimple_operand_gsi (&cond_insert_point, index,
				    true, NULL_TREE,
				    false, GSI_NEW_STMT);
  gimple *g = gimple_build_cond (GT_EXPR, index, bound, NULL_TREE, NULL_TREE);
  gimple_set_location (g, loc);
  gsi_insert_after (&cond_insert_point, g, GSI_NEW_STMT);

  /* Generate __ubsan_handle_out_of_bounds call.  */
  *gsi = gsi_after_labels (then_bb);
  if (flag_sanitize_undefined_trap_on_error)
    g = gimple_build_call (builtin_decl_explicit (BUILT_IN_TRAP), 0);
  else
    {
      tree data
	= ubsan_create_data ("__ubsan_out_of_bounds_data", 1, &loc,
			     ubsan_type_descriptor (type, UBSAN_PRINT_ARRAY),
			     ubsan_type_descriptor (TREE_TYPE (orig_index)),
			     NULL_TREE, NULL_TREE);
      data = build_fold_addr_expr_loc (loc, data);
      enum built_in_function bcode
	= (flag_sanitize_recover & SANITIZE_BOUNDS)
	  ? BUILT_IN_UBSAN_HANDLE_OUT_OF_BOUNDS
	  : BUILT_IN_UBSAN_HANDLE_OUT_OF_BOUNDS_ABORT;
      tree fn = builtin_decl_explicit (bcode);
      tree val = ubsan_encode_value (orig_index, UBSAN_ENCODE_VALUE_GIMPLE);
      val = force_gimple_operand_gsi (gsi, val, true, NULL_TREE, true,
				      GSI_SAME_STMT);
      g = gimple_build_call (fn, 2, data, val);
    }
  gimple_set_location (g, loc);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  /* Get rid of the UBSAN_BOUNDS call from the IR.  */
  unlink_stmt_vdef (stmt);
  gsi_remove (&gsi_orig, true);

  /* Point GSI to next logical statement.  */
  *gsi = gsi_start_bb (fallthru_bb);
  return true;
}

/* Expand UBSAN_NULL internal call.  The type is kept on the ckind
   argument which is a constant, because the middle-end treats pointer
   conversions as useless and therefore the type of the first argument
   could be changed to any other pointer type.  */

bool
ubsan_expand_null_ifn (gimple_stmt_iterator *gsip)
{
  gimple_stmt_iterator gsi = *gsip;
  gimple *stmt = gsi_stmt (gsi);
  location_t loc = gimple_location (stmt);
  gcc_assert (gimple_call_num_args (stmt) == 3);
  tree ptr = gimple_call_arg (stmt, 0);
  tree ckind = gimple_call_arg (stmt, 1);
  tree align = gimple_call_arg (stmt, 2);
  tree check_align = NULL_TREE;
  bool check_null;

  basic_block cur_bb = gsi_bb (gsi);

  gimple *g;
  if (!integer_zerop (align))
    {
      unsigned int ptralign = get_pointer_alignment (ptr) / BITS_PER_UNIT;
      if (compare_tree_int (align, ptralign) == 1)
	{
	  check_align = make_ssa_name (pointer_sized_int_node);
	  g = gimple_build_assign (check_align, NOP_EXPR, ptr);
	  gimple_set_location (g, loc);
	  gsi_insert_before (&gsi, g, GSI_SAME_STMT);
	}
    }
  check_null = sanitize_flags_p (SANITIZE_NULL);

  if (check_align == NULL_TREE && !check_null)
    {
      gsi_remove (gsip, true);
      /* Unlink the UBSAN_NULLs vops before replacing it.  */
      unlink_stmt_vdef (stmt);
      return true;
    }

  /* Split the original block holding the pointer dereference.  */
  edge e = split_block (cur_bb, stmt);

  /* Get a hold on the 'condition block', the 'then block' and the
     'else block'.  */
  basic_block cond_bb = e->src;
  basic_block fallthru_bb = e->dest;
  basic_block then_bb = create_empty_bb (cond_bb);
  add_bb_to_loop (then_bb, cond_bb->loop_father);
  loops_state_set (LOOPS_NEED_FIXUP);

  /* Make an edge coming from the 'cond block' into the 'then block';
     this edge is unlikely taken, so set up the probability accordingly.  */
  e = make_edge (cond_bb, then_bb, EDGE_TRUE_VALUE);
  e->probability = profile_probability::very_unlikely ();
  then_bb->count = e->count ();

  /* Connect 'then block' with the 'else block'.  This is needed
     as the ubsan routines we call in the 'then block' are not noreturn.
     The 'then block' only has one outcoming edge.  */
  make_single_succ_edge (then_bb, fallthru_bb, EDGE_FALLTHRU);

  /* Set up the fallthrough basic block.  */
  e = find_edge (cond_bb, fallthru_bb);
  e->flags = EDGE_FALSE_VALUE;
  e->probability = profile_probability::very_likely ();

  /* Update dominance info for the newly created then_bb; note that
     fallthru_bb's dominance info has already been updated by
     split_block.  */
  if (dom_info_available_p (CDI_DOMINATORS))
    set_immediate_dominator (CDI_DOMINATORS, then_bb, cond_bb);

  /* Put the ubsan builtin call into the newly created BB.  */
  if (flag_sanitize_undefined_trap_on_error)
    g = gimple_build_call (builtin_decl_implicit (BUILT_IN_TRAP), 0);
  else
    {
      enum built_in_function bcode
	= (flag_sanitize_recover & ((check_align ? SANITIZE_ALIGNMENT : 0)
				    | (check_null ? SANITIZE_NULL : 0)))
	  ? BUILT_IN_UBSAN_HANDLE_TYPE_MISMATCH_V1
	  : BUILT_IN_UBSAN_HANDLE_TYPE_MISMATCH_V1_ABORT;
      tree fn = builtin_decl_implicit (bcode);
      int align_log = tree_log2 (align);
      tree data
	= ubsan_create_data ("__ubsan_null_data", 1, &loc,
			     ubsan_type_descriptor (TREE_TYPE (ckind),
						    UBSAN_PRINT_POINTER),
			     NULL_TREE,
			     build_int_cst (unsigned_char_type_node,
					    MAX (align_log, 0)),
			     fold_convert (unsigned_char_type_node, ckind),
			     NULL_TREE);
      data = build_fold_addr_expr_loc (loc, data);
      g = gimple_build_call (fn, 2, data,
			     check_align ? check_align
			     : build_zero_cst (pointer_sized_int_node));
    }
  gimple_stmt_iterator gsi2 = gsi_start_bb (then_bb);
  gimple_set_location (g, loc);
  gsi_insert_after (&gsi2, g, GSI_NEW_STMT);

  /* Unlink the UBSAN_NULLs vops before replacing it.  */
  unlink_stmt_vdef (stmt);

  if (check_null)
    {
      g = gimple_build_cond (EQ_EXPR, ptr, build_int_cst (TREE_TYPE (ptr), 0),
			     NULL_TREE, NULL_TREE);
      gimple_set_location (g, loc);

      /* Replace the UBSAN_NULL with a GIMPLE_COND stmt.  */
      gsi_replace (&gsi, g, false);
      stmt = g;
    }

  if (check_align)
    {
      if (check_null)
	{
	  /* Split the block with the condition again.  */
	  e = split_block (cond_bb, stmt);
	  basic_block cond1_bb = e->src;
	  basic_block cond2_bb = e->dest;

	  /* Make an edge coming from the 'cond1 block' into the 'then block';
	     this edge is unlikely taken, so set up the probability
	     accordingly.  */
	  e = make_edge (cond1_bb, then_bb, EDGE_TRUE_VALUE);
	  e->probability = profile_probability::very_unlikely ();

	  /* Set up the fallthrough basic block.  */
	  e = find_edge (cond1_bb, cond2_bb);
	  e->flags = EDGE_FALSE_VALUE;
	  e->probability = profile_probability::very_likely ();

	  /* Update dominance info.  */
	  if (dom_info_available_p (CDI_DOMINATORS))
	    {
	      set_immediate_dominator (CDI_DOMINATORS, fallthru_bb, cond1_bb);
	      set_immediate_dominator (CDI_DOMINATORS, then_bb, cond1_bb);
	    }

	  gsi2 = gsi_start_bb (cond2_bb);
	}

      tree mask = build_int_cst (pointer_sized_int_node,
				 tree_to_uhwi (align) - 1);
      g = gimple_build_assign (make_ssa_name (pointer_sized_int_node),
			       BIT_AND_EXPR, check_align, mask);
      gimple_set_location (g, loc);
      if (check_null)
	gsi_insert_after (&gsi2, g, GSI_NEW_STMT);
      else
	gsi_insert_before (&gsi, g, GSI_SAME_STMT);

      g = gimple_build_cond (NE_EXPR, gimple_assign_lhs (g),
			     build_int_cst (pointer_sized_int_node, 0),
			     NULL_TREE, NULL_TREE);
      gimple_set_location (g, loc);
      if (check_null)
	gsi_insert_after (&gsi2, g, GSI_NEW_STMT);
      else
	/* Replace the UBSAN_NULL with a GIMPLE_COND stmt.  */
	gsi_replace (&gsi, g, false);
    }
  return false;
}

#define OBJSZ_MAX_OFFSET (1024 * 16)

/* Expand UBSAN_OBJECT_SIZE internal call.  */

bool
ubsan_expand_objsize_ifn (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  location_t loc = gimple_location (stmt);
  gcc_assert (gimple_call_num_args (stmt) == 4);

  tree ptr = gimple_call_arg (stmt, 0);
  tree offset = gimple_call_arg (stmt, 1);
  tree size = gimple_call_arg (stmt, 2);
  tree ckind = gimple_call_arg (stmt, 3);
  gimple_stmt_iterator gsi_orig = *gsi;
  gimple *g;

  /* See if we can discard the check.  */
  if (TREE_CODE (size) != INTEGER_CST
      || integer_all_onesp (size))
    /* Yes, __builtin_object_size couldn't determine the
       object size.  */;
  else if (TREE_CODE (offset) == INTEGER_CST
	   && wi::to_widest (offset) >= -OBJSZ_MAX_OFFSET
	   && wi::to_widest (offset) <= -1)
    /* The offset is in range [-16K, -1].  */;
  else
    {
      /* if (offset > objsize) */
      basic_block then_bb, fallthru_bb;
      gimple_stmt_iterator cond_insert_point
	= create_cond_insert_point (gsi, false, false, true,
				    &then_bb, &fallthru_bb);
      g = gimple_build_cond (GT_EXPR, offset, size, NULL_TREE, NULL_TREE);
      gimple_set_location (g, loc);
      gsi_insert_after (&cond_insert_point, g, GSI_NEW_STMT);

      /* If the offset is small enough, we don't need the second
	 run-time check.  */
      if (TREE_CODE (offset) == INTEGER_CST
	  && wi::to_widest (offset) >= 0
	  && wi::to_widest (offset) <= OBJSZ_MAX_OFFSET)
	*gsi = gsi_after_labels (then_bb);
      else
	{
	  /* Don't issue run-time error if (ptr > ptr + offset).  That
	     may happen when computing a POINTER_PLUS_EXPR.  */
	  basic_block then2_bb, fallthru2_bb;

	  gimple_stmt_iterator gsi2 = gsi_after_labels (then_bb);
	  cond_insert_point = create_cond_insert_point (&gsi2, false, false,
							true, &then2_bb,
							&fallthru2_bb);
	  /* Convert the pointer to an integer type.  */
	  tree p = make_ssa_name (pointer_sized_int_node);
	  g = gimple_build_assign (p, NOP_EXPR, ptr);
	  gimple_set_location (g, loc);
	  gsi_insert_before (&cond_insert_point, g, GSI_NEW_STMT);
	  p = gimple_assign_lhs (g);
	  /* Compute ptr + offset.  */
	  g = gimple_build_assign (make_ssa_name (pointer_sized_int_node),
				   PLUS_EXPR, p, offset);
	  gimple_set_location (g, loc);
	  gsi_insert_after (&cond_insert_point, g, GSI_NEW_STMT);
	  /* Now build the conditional and put it into the IR.  */
	  g = gimple_build_cond (LE_EXPR, p, gimple_assign_lhs (g),
				 NULL_TREE, NULL_TREE);
	  gimple_set_location (g, loc);
	  gsi_insert_after (&cond_insert_point, g, GSI_NEW_STMT);
	  *gsi = gsi_after_labels (then2_bb);
	}

      /* Generate __ubsan_handle_type_mismatch call.  */
      if (flag_sanitize_undefined_trap_on_error)
	g = gimple_build_call (builtin_decl_explicit (BUILT_IN_TRAP), 0);
      else
	{
	  tree data
	    = ubsan_create_data ("__ubsan_objsz_data", 1, &loc,
				 ubsan_type_descriptor (TREE_TYPE (ptr),
							UBSAN_PRINT_POINTER),
				 NULL_TREE,
				 build_zero_cst (unsigned_char_type_node),
				 ckind,
				 NULL_TREE);
	  data = build_fold_addr_expr_loc (loc, data);
	  enum built_in_function bcode
	    = (flag_sanitize_recover & SANITIZE_OBJECT_SIZE)
	      ? BUILT_IN_UBSAN_HANDLE_TYPE_MISMATCH_V1
	      : BUILT_IN_UBSAN_HANDLE_TYPE_MISMATCH_V1_ABORT;
	  tree p = make_ssa_name (pointer_sized_int_node);
	  g = gimple_build_assign (p, NOP_EXPR, ptr);
	  gimple_set_location (g, loc);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	  g = gimple_build_call (builtin_decl_explicit (bcode), 2, data, p);
	}
      gimple_set_location (g, loc);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);

      /* Point GSI to next logical statement.  */
      *gsi = gsi_start_bb (fallthru_bb);

      /* Get rid of the UBSAN_OBJECT_SIZE call from the IR.  */
      unlink_stmt_vdef (stmt);
      gsi_remove (&gsi_orig, true);
      return true;
    }

  /* Get rid of the UBSAN_OBJECT_SIZE call from the IR.  */
  unlink_stmt_vdef (stmt);
  gsi_remove (gsi, true);
  return true;
}

/* Expand UBSAN_PTR internal call.  */

bool
ubsan_expand_ptr_ifn (gimple_stmt_iterator *gsip)
{
  gimple_stmt_iterator gsi = *gsip;
  gimple *stmt = gsi_stmt (gsi);
  location_t loc = gimple_location (stmt);
  gcc_assert (gimple_call_num_args (stmt) == 2);
  tree ptr = gimple_call_arg (stmt, 0);
  tree off = gimple_call_arg (stmt, 1);

  if (integer_zerop (off))
    {
      gsi_remove (gsip, true);
      unlink_stmt_vdef (stmt);
      return true;
    }

  basic_block cur_bb = gsi_bb (gsi);
  tree ptrplusoff = make_ssa_name (pointer_sized_int_node);
  tree ptri = make_ssa_name (pointer_sized_int_node);
  int pos_neg = get_range_pos_neg (off);

  /* Split the original block holding the pointer dereference.  */
  edge e = split_block (cur_bb, stmt);

  /* Get a hold on the 'condition block', the 'then block' and the
     'else block'.  */
  basic_block cond_bb = e->src;
  basic_block fallthru_bb = e->dest;
  basic_block then_bb = create_empty_bb (cond_bb);
  basic_block cond_pos_bb = NULL, cond_neg_bb = NULL;
  add_bb_to_loop (then_bb, cond_bb->loop_father);
  loops_state_set (LOOPS_NEED_FIXUP);

  /* Set up the fallthrough basic block.  */
  e->flags = EDGE_FALSE_VALUE;
  if (pos_neg != 3)
    {
      e->probability = profile_probability::very_likely ();

      /* Connect 'then block' with the 'else block'.  This is needed
	 as the ubsan routines we call in the 'then block' are not noreturn.
	 The 'then block' only has one outcoming edge.  */
      make_single_succ_edge (then_bb, fallthru_bb, EDGE_FALLTHRU);

      /* Make an edge coming from the 'cond block' into the 'then block';
	 this edge is unlikely taken, so set up the probability
	 accordingly.  */
      e = make_edge (cond_bb, then_bb, EDGE_TRUE_VALUE);
      e->probability = profile_probability::very_unlikely ();
      then_bb->count = e->count ();
    }
  else
    {
      e->probability = profile_probability::even ();

      e = split_block (fallthru_bb, (gimple *) NULL);
      cond_neg_bb = e->src;
      fallthru_bb = e->dest;
      e->probability = profile_probability::very_likely ();
      e->flags = EDGE_FALSE_VALUE;

      e = make_edge (cond_neg_bb, then_bb, EDGE_TRUE_VALUE);
      e->probability = profile_probability::very_unlikely ();
      then_bb->count = e->count ();

      cond_pos_bb = create_empty_bb (cond_bb);
      add_bb_to_loop (cond_pos_bb, cond_bb->loop_father);

      e = make_edge (cond_bb, cond_pos_bb, EDGE_TRUE_VALUE);
      e->probability = profile_probability::even ();
      cond_pos_bb->count = e->count ();

      e = make_edge (cond_pos_bb, then_bb, EDGE_TRUE_VALUE);
      e->probability = profile_probability::very_unlikely ();

      e = make_edge (cond_pos_bb, fallthru_bb, EDGE_FALSE_VALUE);
      e->probability = profile_probability::very_likely ();

      make_single_succ_edge (then_bb, fallthru_bb, EDGE_FALLTHRU);
    }

  gimple *g = gimple_build_assign (ptri, NOP_EXPR, ptr);
  gimple_set_location (g, loc);
  gsi_insert_before (&gsi, g, GSI_SAME_STMT);
  g = gimple_build_assign (ptrplusoff, PLUS_EXPR, ptri, off);
  gimple_set_location (g, loc);
  gsi_insert_before (&gsi, g, GSI_SAME_STMT);

  /* Update dominance info for the newly created then_bb; note that
     fallthru_bb's dominance info has already been updated by
     split_block.  */
  if (dom_info_available_p (CDI_DOMINATORS))
    {
      set_immediate_dominator (CDI_DOMINATORS, then_bb, cond_bb);
      if (pos_neg == 3)
	{
	  set_immediate_dominator (CDI_DOMINATORS, cond_pos_bb, cond_bb);
	  set_immediate_dominator (CDI_DOMINATORS, fallthru_bb, cond_bb);
	}
    }

  /* Put the ubsan builtin call into the newly created BB.  */
  if (flag_sanitize_undefined_trap_on_error)
    g = gimple_build_call (builtin_decl_implicit (BUILT_IN_TRAP), 0);
  else
    {
      enum built_in_function bcode
	= (flag_sanitize_recover & SANITIZE_POINTER_OVERFLOW)
	  ? BUILT_IN_UBSAN_HANDLE_POINTER_OVERFLOW
	  : BUILT_IN_UBSAN_HANDLE_POINTER_OVERFLOW_ABORT;
      tree fn = builtin_decl_implicit (bcode);
      tree data
	= ubsan_create_data ("__ubsan_ptrovf_data", 1, &loc,
			     NULL_TREE, NULL_TREE);
      data = build_fold_addr_expr_loc (loc, data);
      g = gimple_build_call (fn, 3, data, ptr, ptrplusoff);
    }
  gimple_stmt_iterator gsi2 = gsi_start_bb (then_bb);
  gimple_set_location (g, loc);
  gsi_insert_after (&gsi2, g, GSI_NEW_STMT);

  /* Unlink the UBSAN_PTRs vops before replacing it.  */
  unlink_stmt_vdef (stmt);

  if (TREE_CODE (off) == INTEGER_CST)
    g = gimple_build_cond (wi::neg_p (wi::to_wide (off)) ? LT_EXPR : GE_EXPR,
			   ptri, fold_build1 (NEGATE_EXPR, sizetype, off),
			   NULL_TREE, NULL_TREE);
  else if (pos_neg != 3)
    g = gimple_build_cond (pos_neg == 1 ? LT_EXPR : GT_EXPR,
			   ptrplusoff, ptri, NULL_TREE, NULL_TREE);
  else
    {
      gsi2 = gsi_start_bb (cond_pos_bb);
      g = gimple_build_cond (LT_EXPR, ptrplusoff, ptri, NULL_TREE, NULL_TREE);
      gimple_set_location (g, loc);
      gsi_insert_after (&gsi2, g, GSI_NEW_STMT);

      gsi2 = gsi_start_bb (cond_neg_bb);
      g = gimple_build_cond (GT_EXPR, ptrplusoff, ptri, NULL_TREE, NULL_TREE);
      gimple_set_location (g, loc);
      gsi_insert_after (&gsi2, g, GSI_NEW_STMT);

      gimple_seq seq = NULL;
      tree t = gimple_build (&seq, loc, NOP_EXPR, ssizetype, off);
      t = gimple_build (&seq, loc, GE_EXPR, boolean_type_node,
			t, ssize_int (0));
      gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
      g = gimple_build_cond (NE_EXPR, t, boolean_false_node,
			     NULL_TREE, NULL_TREE);
    }
  gimple_set_location (g, loc);
  /* Replace the UBSAN_PTR with a GIMPLE_COND stmt.  */
  gsi_replace (&gsi, g, false);
  return false;
}


/* Cached __ubsan_vptr_type_cache decl.  */
static GTY(()) tree ubsan_vptr_type_cache_decl;

/* Expand UBSAN_VPTR internal call.  The type is kept on the ckind
   argument which is a constant, because the middle-end treats pointer
   conversions as useless and therefore the type of the first argument
   could be changed to any other pointer type.  */

bool
ubsan_expand_vptr_ifn (gimple_stmt_iterator *gsip)
{
  gimple_stmt_iterator gsi = *gsip;
  gimple *stmt = gsi_stmt (gsi);
  location_t loc = gimple_location (stmt);
  gcc_assert (gimple_call_num_args (stmt) == 5);
  tree op = gimple_call_arg (stmt, 0);
  tree vptr = gimple_call_arg (stmt, 1);
  tree str_hash = gimple_call_arg (stmt, 2);
  tree ti_decl_addr = gimple_call_arg (stmt, 3);
  tree ckind_tree = gimple_call_arg (stmt, 4);
  ubsan_null_ckind ckind = (ubsan_null_ckind) tree_to_uhwi (ckind_tree);
  tree type = TREE_TYPE (TREE_TYPE (ckind_tree));
  gimple *g;
  basic_block fallthru_bb = NULL;

  if (ckind == UBSAN_DOWNCAST_POINTER)
    {
      /* Guard everything with if (op != NULL) { ... }.  */
      basic_block then_bb;
      gimple_stmt_iterator cond_insert_point
	= create_cond_insert_point (gsip, false, false, true,
				    &then_bb, &fallthru_bb);
      g = gimple_build_cond (NE_EXPR, op, build_zero_cst (TREE_TYPE (op)),
			     NULL_TREE, NULL_TREE);
      gimple_set_location (g, loc);
      gsi_insert_after (&cond_insert_point, g, GSI_NEW_STMT);
      *gsip = gsi_after_labels (then_bb);
      gsi_remove (&gsi, false);
      gsi_insert_before (gsip, stmt, GSI_NEW_STMT);
      gsi = *gsip;
    }

  tree htype = TREE_TYPE (str_hash);
  tree cst = wide_int_to_tree (htype,
			       wi::uhwi (((uint64_t) 0x9ddfea08 << 32)
			       | 0xeb382d69, 64));
  g = gimple_build_assign (make_ssa_name (htype), BIT_XOR_EXPR,
			   vptr, str_hash);
  gimple_set_location (g, loc);
  gsi_insert_before (gsip, g, GSI_SAME_STMT);
  g = gimple_build_assign (make_ssa_name (htype), MULT_EXPR,
			   gimple_assign_lhs (g), cst);
  gimple_set_location (g, loc);
  gsi_insert_before (gsip, g, GSI_SAME_STMT);
  tree t1 = gimple_assign_lhs (g);
  g = gimple_build_assign (make_ssa_name (htype), LSHIFT_EXPR,
			   t1, build_int_cst (integer_type_node, 47));
  gimple_set_location (g, loc);
  tree t2 = gimple_assign_lhs (g);
  gsi_insert_before (gsip, g, GSI_SAME_STMT);
  g = gimple_build_assign (make_ssa_name (htype), BIT_XOR_EXPR,
			   vptr, t1);
  gimple_set_location (g, loc);
  gsi_insert_before (gsip, g, GSI_SAME_STMT);
  g = gimple_build_assign (make_ssa_name (htype), BIT_XOR_EXPR,
			   t2, gimple_assign_lhs (g));
  gimple_set_location (g, loc);
  gsi_insert_before (gsip, g, GSI_SAME_STMT);
  g = gimple_build_assign (make_ssa_name (htype), MULT_EXPR,
			   gimple_assign_lhs (g), cst);
  gimple_set_location (g, loc);
  gsi_insert_before (gsip, g, GSI_SAME_STMT);
  tree t3 = gimple_assign_lhs (g);
  g = gimple_build_assign (make_ssa_name (htype), LSHIFT_EXPR,
			   t3, build_int_cst (integer_type_node, 47));
  gimple_set_location (g, loc);
  gsi_insert_before (gsip, g, GSI_SAME_STMT);
  g = gimple_build_assign (make_ssa_name (htype), BIT_XOR_EXPR,
			   t3, gimple_assign_lhs (g));
  gimple_set_location (g, loc);
  gsi_insert_before (gsip, g, GSI_SAME_STMT);
  g = gimple_build_assign (make_ssa_name (htype), MULT_EXPR,
			   gimple_assign_lhs (g), cst);
  gimple_set_location (g, loc);
  gsi_insert_before (gsip, g, GSI_SAME_STMT);
  if (!useless_type_conversion_p (pointer_sized_int_node, htype))
    {
      g = gimple_build_assign (make_ssa_name (pointer_sized_int_node),
			       NOP_EXPR, gimple_assign_lhs (g));
      gimple_set_location (g, loc);
      gsi_insert_before (gsip, g, GSI_SAME_STMT);
    }
  tree hash = gimple_assign_lhs (g);

  if (ubsan_vptr_type_cache_decl == NULL_TREE)
    {
      tree atype = build_array_type_nelts (pointer_sized_int_node, 128);
      tree array = build_decl (UNKNOWN_LOCATION, VAR_DECL,
			       get_identifier ("__ubsan_vptr_type_cache"),
			       atype);
      DECL_ARTIFICIAL (array) = 1;
      DECL_IGNORED_P (array) = 1;
      TREE_PUBLIC (array) = 1;
      TREE_STATIC (array) = 1;
      DECL_EXTERNAL (array) = 1;
      DECL_VISIBILITY (array) = VISIBILITY_DEFAULT;
      DECL_VISIBILITY_SPECIFIED (array) = 1;
      varpool_node::finalize_decl (array);
      ubsan_vptr_type_cache_decl = array;
   }

  g = gimple_build_assign (make_ssa_name (pointer_sized_int_node),
			   BIT_AND_EXPR, hash,
			   build_int_cst (pointer_sized_int_node, 127));
  gimple_set_location (g, loc);
  gsi_insert_before (gsip, g, GSI_SAME_STMT);

  tree c = build4_loc (loc, ARRAY_REF, pointer_sized_int_node,
		       ubsan_vptr_type_cache_decl, gimple_assign_lhs (g),
		       NULL_TREE, NULL_TREE);
  g = gimple_build_assign (make_ssa_name (pointer_sized_int_node),
			   ARRAY_REF, c);
  gimple_set_location (g, loc);
  gsi_insert_before (gsip, g, GSI_SAME_STMT);

  basic_block then_bb, fallthru2_bb;
  gimple_stmt_iterator cond_insert_point
    = create_cond_insert_point (gsip, false, false, true,
				&then_bb, &fallthru2_bb);
  g = gimple_build_cond (NE_EXPR, gimple_assign_lhs (g), hash,
			 NULL_TREE, NULL_TREE);
  gimple_set_location (g, loc);
  gsi_insert_after (&cond_insert_point, g, GSI_NEW_STMT);
  *gsip = gsi_after_labels (then_bb);
  if (fallthru_bb == NULL)
    fallthru_bb = fallthru2_bb;

  tree data
    = ubsan_create_data ("__ubsan_vptr_data", 1, &loc,
			 ubsan_type_descriptor (type), NULL_TREE, ti_decl_addr,
			 build_int_cst (unsigned_char_type_node, ckind),
			 NULL_TREE);
  data = build_fold_addr_expr_loc (loc, data);
  enum built_in_function bcode
    = (flag_sanitize_recover & SANITIZE_VPTR)
      ? BUILT_IN_UBSAN_HANDLE_DYNAMIC_TYPE_CACHE_MISS
      : BUILT_IN_UBSAN_HANDLE_DYNAMIC_TYPE_CACHE_MISS_ABORT;

  g = gimple_build_call (builtin_decl_explicit (bcode), 3, data, op, hash);
  gimple_set_location (g, loc);
  gsi_insert_before (gsip, g, GSI_SAME_STMT);

  /* Point GSI to next logical statement.  */
  *gsip = gsi_start_bb (fallthru_bb);

  /* Get rid of the UBSAN_VPTR call from the IR.  */
  unlink_stmt_vdef (stmt);
  gsi_remove (&gsi, true);
  return true;
}

/* Instrument a memory reference.  BASE is the base of MEM, IS_LHS says
   whether the pointer is on the left hand side of the assignment.  */

static void
instrument_mem_ref (tree mem, tree base, gimple_stmt_iterator *iter,
		    bool is_lhs)
{
  enum ubsan_null_ckind ikind = is_lhs ? UBSAN_STORE_OF : UBSAN_LOAD_OF;
  unsigned int align = 0;
  if (sanitize_flags_p (SANITIZE_ALIGNMENT))
    {
      align = min_align_of_type (TREE_TYPE (base));
      if (align <= 1)
	align = 0;
    }
  if (align == 0 && !sanitize_flags_p (SANITIZE_NULL))
    return;
  tree t = TREE_OPERAND (base, 0);
  if (!POINTER_TYPE_P (TREE_TYPE (t)))
    return;
  if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (base)) && mem != base)
    ikind = UBSAN_MEMBER_ACCESS;
  tree kind = build_int_cst (build_pointer_type (TREE_TYPE (base)), ikind);
  tree alignt = build_int_cst (pointer_sized_int_node, align);
  gcall *g = gimple_build_call_internal (IFN_UBSAN_NULL, 3, t, kind, alignt);
  gimple_set_location (g, gimple_location (gsi_stmt (*iter)));
  gsi_insert_before (iter, g, GSI_SAME_STMT);
}

/* Perform the pointer instrumentation.  */

static void
instrument_null (gimple_stmt_iterator gsi, tree t, bool is_lhs)
{
  /* Handle also e.g. &s->i.  */
  if (TREE_CODE (t) == ADDR_EXPR)
    t = TREE_OPERAND (t, 0);
  tree base = get_base_address (t);
  if (base != NULL_TREE
      && TREE_CODE (base) == MEM_REF
      && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME)
    instrument_mem_ref (t, base, &gsi, is_lhs);
}

/* Instrument pointer arithmetics PTR p+ OFF.  */

static void
instrument_pointer_overflow (gimple_stmt_iterator *gsi, tree ptr, tree off)
{
  if (TYPE_PRECISION (sizetype) != POINTER_SIZE)
    return;
  gcall *g = gimple_build_call_internal (IFN_UBSAN_PTR, 2, ptr, off);
  gimple_set_location (g, gimple_location (gsi_stmt (*gsi)));
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
}

/* Instrument pointer arithmetics if any.  */

static void
maybe_instrument_pointer_overflow (gimple_stmt_iterator *gsi, tree t)
{
  if (TYPE_PRECISION (sizetype) != POINTER_SIZE)
    return;

  /* Handle also e.g. &s->i.  */
  if (TREE_CODE (t) == ADDR_EXPR)
    t = TREE_OPERAND (t, 0);

  if (!handled_component_p (t) && TREE_CODE (t) != MEM_REF)
    return;

  HOST_WIDE_INT bitsize, bitpos, bytepos;
  tree offset;
  machine_mode mode;
  int volatilep = 0, reversep, unsignedp = 0;
  tree inner = get_inner_reference (t, &bitsize, &bitpos, &offset, &mode,
				    &unsignedp, &reversep, &volatilep);
  tree moff = NULL_TREE;

  bool decl_p = DECL_P (inner);
  tree base;
  if (decl_p)
    {
      if (DECL_REGISTER (inner))
	return;
      base = inner;
      /* If BASE is a fixed size automatic variable or
	 global variable defined in the current TU and bitpos
	 fits, don't instrument anything.  */
      if (offset == NULL_TREE
	  && bitpos > 0
	  && (VAR_P (base)
	      || TREE_CODE (base) == PARM_DECL
	      || TREE_CODE (base) == RESULT_DECL)
	  && DECL_SIZE (base)
	  && TREE_CODE (DECL_SIZE (base)) == INTEGER_CST
	  && compare_tree_int (DECL_SIZE (base), bitpos) >= 0
	  && (!is_global_var (base) || decl_binds_to_current_def_p (base)))
	return;
    }
  else if (TREE_CODE (inner) == MEM_REF)
    {
      base = TREE_OPERAND (inner, 0);
      if (TREE_CODE (base) == ADDR_EXPR
	  && DECL_P (TREE_OPERAND (base, 0))
	  && !TREE_ADDRESSABLE (TREE_OPERAND (base, 0))
	  && !is_global_var (TREE_OPERAND (base, 0)))
	return;
      moff = TREE_OPERAND (inner, 1);
      if (integer_zerop (moff))
	moff = NULL_TREE;
    }
  else
    return;

  if (!POINTER_TYPE_P (TREE_TYPE (base)) && !DECL_P (base))
    return;
  bytepos = bitpos / BITS_PER_UNIT;
  if (offset == NULL_TREE && bytepos == 0 && moff == NULL_TREE)
    return;

  tree base_addr = base;
  if (decl_p)
    base_addr = build1 (ADDR_EXPR,
			build_pointer_type (TREE_TYPE (base)), base);
  t = offset;
  if (bytepos)
    {
      if (t)
	t = fold_build2 (PLUS_EXPR, TREE_TYPE (t), t,
			 build_int_cst (TREE_TYPE (t), bytepos));
      else
	t = size_int (bytepos);
    }
  if (moff)
    {
      if (t)
	t = fold_build2 (PLUS_EXPR, TREE_TYPE (t), t,
			 fold_convert (TREE_TYPE (t), moff));
      else
	t = fold_convert (sizetype, moff);
    }
  t = force_gimple_operand_gsi (gsi, t, true, NULL_TREE, true,
				GSI_SAME_STMT);
  base_addr = force_gimple_operand_gsi (gsi, base_addr, true, NULL_TREE, true,
					GSI_SAME_STMT);
  instrument_pointer_overflow (gsi, base_addr, t);
}

/* Build an ubsan builtin call for the signed-integer-overflow
   sanitization.  CODE says what kind of builtin are we building,
   LOC is a location, LHSTYPE is the type of LHS, OP0 and OP1
   are operands of the binary operation.  */

tree
ubsan_build_overflow_builtin (tree_code code, location_t loc, tree lhstype,
			      tree op0, tree op1, tree *datap)
{
  if (flag_sanitize_undefined_trap_on_error)
    return build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TRAP), 0);

  tree data;
  if (datap && *datap)
    data = *datap;
  else
    data = ubsan_create_data ("__ubsan_overflow_data", 1, &loc,
			      ubsan_type_descriptor (lhstype), NULL_TREE,
			      NULL_TREE);
  if (datap)
    *datap = data;
  enum built_in_function fn_code;

  switch (code)
    {
    case PLUS_EXPR:
      fn_code = (flag_sanitize_recover & SANITIZE_SI_OVERFLOW)
		? BUILT_IN_UBSAN_HANDLE_ADD_OVERFLOW
		: BUILT_IN_UBSAN_HANDLE_ADD_OVERFLOW_ABORT;
      break;
    case MINUS_EXPR:
      fn_code = (flag_sanitize_recover & SANITIZE_SI_OVERFLOW)
		? BUILT_IN_UBSAN_HANDLE_SUB_OVERFLOW
		: BUILT_IN_UBSAN_HANDLE_SUB_OVERFLOW_ABORT;
      break;
    case MULT_EXPR:
      fn_code = (flag_sanitize_recover & SANITIZE_SI_OVERFLOW)
		? BUILT_IN_UBSAN_HANDLE_MUL_OVERFLOW
		: BUILT_IN_UBSAN_HANDLE_MUL_OVERFLOW_ABORT;
      break;
    case NEGATE_EXPR:
      fn_code = (flag_sanitize_recover & SANITIZE_SI_OVERFLOW)
		? BUILT_IN_UBSAN_HANDLE_NEGATE_OVERFLOW
		: BUILT_IN_UBSAN_HANDLE_NEGATE_OVERFLOW_ABORT;
      break;
    default:
      gcc_unreachable ();
    }
  tree fn = builtin_decl_explicit (fn_code);
  return build_call_expr_loc (loc, fn, 2 + (code != NEGATE_EXPR),
			      build_fold_addr_expr_loc (loc, data),
			      ubsan_encode_value (op0, UBSAN_ENCODE_VALUE_RTL),
			      op1
			      ? ubsan_encode_value (op1,
						    UBSAN_ENCODE_VALUE_RTL)
			      : NULL_TREE);
}

/* Perform the signed integer instrumentation.  GSI is the iterator
   pointing at statement we are trying to instrument.  */

static void
instrument_si_overflow (gimple_stmt_iterator gsi)
{
  gimple *stmt = gsi_stmt (gsi);
  tree_code code = gimple_assign_rhs_code (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  tree lhstype = TREE_TYPE (lhs);
  tree lhsinner = VECTOR_TYPE_P (lhstype) ? TREE_TYPE (lhstype) : lhstype;
  tree a, b;
  gimple *g;

  /* If this is not a signed operation, don't instrument anything here.
     Also punt on bit-fields.  */
  if (!INTEGRAL_TYPE_P (lhsinner)
      || TYPE_OVERFLOW_WRAPS (lhsinner)
      || GET_MODE_BITSIZE (TYPE_MODE (lhsinner)) != TYPE_PRECISION (lhsinner))
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
      gsi_replace (&gsi, g, true);
      break;
    case NEGATE_EXPR:
      /* Represent i = -u;
	 as
	 i = UBSAN_CHECK_SUB (0, u);  */
      a = build_zero_cst (lhstype);
      b = gimple_assign_rhs1 (stmt);
      g = gimple_build_call_internal (IFN_UBSAN_CHECK_SUB, 2, a, b);
      gimple_call_set_lhs (g, lhs);
      gsi_replace (&gsi, g, true);
      break;
    case ABS_EXPR:
      /* Transform i = ABS_EXPR<u>;
	 into
	 _N = UBSAN_CHECK_SUB (0, u);
	 i = ABS_EXPR<_N>;  */
      a = build_zero_cst (lhstype);
      b = gimple_assign_rhs1 (stmt);
      g = gimple_build_call_internal (IFN_UBSAN_CHECK_SUB, 2, a, b);
      a = make_ssa_name (lhstype);
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
  gimple *stmt = gsi_stmt (*gsi);
  tree rhs = gimple_assign_rhs1 (stmt);
  tree type = TREE_TYPE (rhs);
  tree minv = NULL_TREE, maxv = NULL_TREE;

  if (TREE_CODE (type) == BOOLEAN_TYPE
      && sanitize_flags_p (SANITIZE_BOOL))
    {
      minv = boolean_false_node;
      maxv = boolean_true_node;
    }
  else if (TREE_CODE (type) == ENUMERAL_TYPE
	   && sanitize_flags_p (SANITIZE_ENUM)
	   && TREE_TYPE (type) != NULL_TREE
	   && TREE_CODE (TREE_TYPE (type)) == INTEGER_TYPE
	   && (TYPE_PRECISION (TREE_TYPE (type))
	       < GET_MODE_PRECISION (SCALAR_INT_TYPE_MODE (type))))
    {
      minv = TYPE_MIN_VALUE (TREE_TYPE (type));
      maxv = TYPE_MAX_VALUE (TREE_TYPE (type));
    }
  else
    return;

  int modebitsize = GET_MODE_BITSIZE (SCALAR_INT_TYPE_MODE (type));
  HOST_WIDE_INT bitsize, bitpos;
  tree offset;
  machine_mode mode;
  int volatilep = 0, reversep, unsignedp = 0;
  tree base = get_inner_reference (rhs, &bitsize, &bitpos, &offset, &mode,
				   &unsignedp, &reversep, &volatilep);
  tree utype = build_nonstandard_integer_type (modebitsize, 1);

  if ((VAR_P (base) && DECL_HARD_REGISTER (base))
      || (bitpos % modebitsize) != 0
      || bitsize != modebitsize
      || GET_MODE_BITSIZE (SCALAR_INT_TYPE_MODE (utype)) != modebitsize
      || TREE_CODE (gimple_assign_lhs (stmt)) != SSA_NAME)
    return;

  bool ends_bb = stmt_ends_bb_p (stmt);
  location_t loc = gimple_location (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  tree ptype = build_pointer_type (TREE_TYPE (rhs));
  tree atype = reference_alias_ptr_type (rhs);
  gimple *g = gimple_build_assign (make_ssa_name (ptype),
				  build_fold_addr_expr (rhs));
  gimple_set_location (g, loc);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
  tree mem = build2 (MEM_REF, utype, gimple_assign_lhs (g),
		     build_int_cst (atype, 0));
  tree urhs = make_ssa_name (utype);
  if (ends_bb)
    {
      gimple_assign_set_lhs (stmt, urhs);
      g = gimple_build_assign (lhs, NOP_EXPR, urhs);
      gimple_set_location (g, loc);
      edge e = find_fallthru_edge (gimple_bb (stmt)->succs);
      gsi_insert_on_edge_immediate (e, g);
      gimple_assign_set_rhs_from_tree (gsi, mem);
      update_stmt (stmt);
      *gsi = gsi_for_stmt (g);
      g = stmt;
    }
  else
    {
      g = gimple_build_assign (urhs, mem);
      gimple_set_location (g, loc);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
    }
  minv = fold_convert (utype, minv);
  maxv = fold_convert (utype, maxv);
  if (!integer_zerop (minv))
    {
      g = gimple_build_assign (make_ssa_name (utype), MINUS_EXPR, urhs, minv);
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

  if (!ends_bb)
    {
      gimple_assign_set_rhs_with_ops (&gsi2, NOP_EXPR, urhs);
      update_stmt (stmt);
    }

  gsi2 = gsi_after_labels (then_bb);
  if (flag_sanitize_undefined_trap_on_error)
    g = gimple_build_call (builtin_decl_explicit (BUILT_IN_TRAP), 0);
  else
    {
      tree data = ubsan_create_data ("__ubsan_invalid_value_data", 1, &loc,
				     ubsan_type_descriptor (type), NULL_TREE,
				     NULL_TREE);
      data = build_fold_addr_expr_loc (loc, data);
      enum built_in_function bcode
	= (flag_sanitize_recover & (TREE_CODE (type) == BOOLEAN_TYPE
				    ? SANITIZE_BOOL : SANITIZE_ENUM))
	  ? BUILT_IN_UBSAN_HANDLE_LOAD_INVALID_VALUE
	  : BUILT_IN_UBSAN_HANDLE_LOAD_INVALID_VALUE_ABORT;
      tree fn = builtin_decl_explicit (bcode);

      tree val = ubsan_encode_value (urhs, UBSAN_ENCODE_VALUE_GIMPLE);
      val = force_gimple_operand_gsi (&gsi2, val, true, NULL_TREE, true,
				      GSI_SAME_STMT);
      g = gimple_build_call (fn, 2, data, val);
    }
  gimple_set_location (g, loc);
  gsi_insert_before (&gsi2, g, GSI_SAME_STMT);
  ubsan_create_edge (g);
  *gsi = gsi_for_stmt (stmt);
}

/* Determine if we can propagate given LOCATION to ubsan_data descriptor to use
   new style handlers.  Libubsan uses heuristics to destinguish between old and
   new styles and relies on these properties for filename:

   a) Location's filename must not be NULL.
   b) Location's filename must not be equal to "".
   c) Location's filename must not be equal to "\1".
   d) First two bytes of filename must not contain '\xff' symbol.  */

static bool
ubsan_use_new_style_p (location_t loc)
{
  if (loc == UNKNOWN_LOCATION)
    return false;

  expanded_location xloc = expand_location (loc);
  if (xloc.file == NULL || strncmp (xloc.file, "\1", 2) == 0
      || xloc.file[0] == '\0' || xloc.file[0] == '\xff'
      || xloc.file[1] == '\xff')
    return false;

  return true;
}

/* Instrument float point-to-integer conversion.  TYPE is an integer type of
   destination, EXPR is floating-point expression.  */

tree
ubsan_instrument_float_cast (location_t loc, tree type, tree expr)
{
  tree expr_type = TREE_TYPE (expr);
  tree t, tt, fn, min, max;
  machine_mode mode = TYPE_MODE (expr_type);
  int prec = TYPE_PRECISION (type);
  bool uns_p = TYPE_UNSIGNED (type);
  if (loc == UNKNOWN_LOCATION)
    loc = input_location;

  /* Float to integer conversion first truncates toward zero, so
     even signed char c = 127.875f; is not problematic.
     Therefore, we should complain only if EXPR is unordered or smaller
     or equal than TYPE_MIN_VALUE - 1.0 or greater or equal than
     TYPE_MAX_VALUE + 1.0.  */
  if (REAL_MODE_FORMAT (mode)->b == 2)
    {
      /* For maximum, TYPE_MAX_VALUE might not be representable
	 in EXPR_TYPE, e.g. if TYPE is 64-bit long long and
	 EXPR_TYPE is IEEE single float, but TYPE_MAX_VALUE + 1.0 is
	 either representable or infinity.  */
      REAL_VALUE_TYPE maxval = dconst1;
      SET_REAL_EXP (&maxval, REAL_EXP (&maxval) + prec - !uns_p);
      real_convert (&maxval, mode, &maxval);
      max = build_real (expr_type, maxval);

      /* For unsigned, assume -1.0 is always representable.  */
      if (uns_p)
	min = build_minus_one_cst (expr_type);
      else
	{
	  /* TYPE_MIN_VALUE is generally representable (or -inf),
	     but TYPE_MIN_VALUE - 1.0 might not be.  */
	  REAL_VALUE_TYPE minval = dconstm1, minval2;
	  SET_REAL_EXP (&minval, REAL_EXP (&minval) + prec - 1);
	  real_convert (&minval, mode, &minval);
	  real_arithmetic (&minval2, MINUS_EXPR, &minval, &dconst1);
	  real_convert (&minval2, mode, &minval2);
	  if (real_compare (EQ_EXPR, &minval, &minval2)
	      && !real_isinf (&minval))
	    {
	      /* If TYPE_MIN_VALUE - 1.0 is not representable and
		 rounds to TYPE_MIN_VALUE, we need to subtract
		 more.  As REAL_MODE_FORMAT (mode)->p is the number
		 of base digits, we want to subtract a number that
		 will be 1 << (REAL_MODE_FORMAT (mode)->p - 1)
		 times smaller than minval.  */
	      minval2 = dconst1;
	      gcc_assert (prec > REAL_MODE_FORMAT (mode)->p);
	      SET_REAL_EXP (&minval2,
			    REAL_EXP (&minval2) + prec - 1
			    - REAL_MODE_FORMAT (mode)->p + 1);
	      real_arithmetic (&minval2, MINUS_EXPR, &minval, &minval2);
	      real_convert (&minval2, mode, &minval2);
	    }
	  min = build_real (expr_type, minval2);
	}
    }
  else if (REAL_MODE_FORMAT (mode)->b == 10)
    {
      /* For _Decimal128 up to 34 decimal digits, - sign,
	 dot, e, exponent.  */
      char buf[64];
      mpfr_t m;
      int p = REAL_MODE_FORMAT (mode)->p;
      REAL_VALUE_TYPE maxval, minval;

      /* Use mpfr_snprintf rounding to compute the smallest
	 representable decimal number greater or equal than
	 1 << (prec - !uns_p).  */
      mpfr_init2 (m, prec + 2);
      mpfr_set_ui_2exp (m, 1, prec - !uns_p, GMP_RNDN);
      mpfr_snprintf (buf, sizeof buf, "%.*RUe", p - 1, m);
      decimal_real_from_string (&maxval, buf);
      max = build_real (expr_type, maxval);

      /* For unsigned, assume -1.0 is always representable.  */
      if (uns_p)
	min = build_minus_one_cst (expr_type);
      else
	{
	  /* Use mpfr_snprintf rounding to compute the largest
	     representable decimal number less or equal than
	     (-1 << (prec - 1)) - 1.  */
	  mpfr_set_si_2exp (m, -1, prec - 1, GMP_RNDN);
	  mpfr_sub_ui (m, m, 1, GMP_RNDN);
	  mpfr_snprintf (buf, sizeof buf, "%.*RDe", p - 1, m);
	  decimal_real_from_string (&minval, buf);
	  min = build_real (expr_type, minval);
	}
      mpfr_clear (m);
    }
  else
    return NULL_TREE;

  t = fold_build2 (UNLE_EXPR, boolean_type_node, expr, min);
  tt = fold_build2 (UNGE_EXPR, boolean_type_node, expr, max);
  t = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, t, tt);
  if (integer_zerop (t))
    return NULL_TREE;

  if (flag_sanitize_undefined_trap_on_error)
    fn = build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TRAP), 0);
  else
    {
      location_t *loc_ptr = NULL;
      unsigned num_locations = 0;
      /* Figure out if we can propagate location to ubsan_data and use new
         style handlers in libubsan.  */
      if (ubsan_use_new_style_p (loc))
	{
	  loc_ptr = &loc;
	  num_locations = 1;
	}
      /* Create the __ubsan_handle_float_cast_overflow fn call.  */
      tree data = ubsan_create_data ("__ubsan_float_cast_overflow_data",
				     num_locations, loc_ptr,
				     ubsan_type_descriptor (expr_type),
				     ubsan_type_descriptor (type), NULL_TREE,
				     NULL_TREE);
      enum built_in_function bcode
	= (flag_sanitize_recover & SANITIZE_FLOAT_CAST)
	  ? BUILT_IN_UBSAN_HANDLE_FLOAT_CAST_OVERFLOW
	  : BUILT_IN_UBSAN_HANDLE_FLOAT_CAST_OVERFLOW_ABORT;
      fn = builtin_decl_explicit (bcode);
      fn = build_call_expr_loc (loc, fn, 2,
				build_fold_addr_expr_loc (loc, data),
				ubsan_encode_value (expr));
    }

  return fold_build3 (COND_EXPR, void_type_node, t, fn, integer_zero_node);
}

/* Instrument values passed to function arguments with nonnull attribute.  */

static void
instrument_nonnull_arg (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  location_t loc[2];
  /* infer_nonnull_range needs flag_delete_null_pointer_checks set,
     while for nonnull sanitization it is clear.  */
  int save_flag_delete_null_pointer_checks = flag_delete_null_pointer_checks;
  flag_delete_null_pointer_checks = 1;
  loc[0] = gimple_location (stmt);
  loc[1] = UNKNOWN_LOCATION;
  for (unsigned int i = 0; i < gimple_call_num_args (stmt); i++)
    {
      tree arg = gimple_call_arg (stmt, i);
      if (POINTER_TYPE_P (TREE_TYPE (arg))
	  && infer_nonnull_range_by_attribute (stmt, arg))
	{
	  gimple *g;
	  if (!is_gimple_val (arg))
	    {
	      g = gimple_build_assign (make_ssa_name (TREE_TYPE (arg)), arg);
	      gimple_set_location (g, loc[0]);
	      gsi_insert_before (gsi, g, GSI_SAME_STMT);
	      arg = gimple_assign_lhs (g);
	    }

	  basic_block then_bb, fallthru_bb;
	  *gsi = create_cond_insert_point (gsi, true, false, true,
					   &then_bb, &fallthru_bb);
	  g = gimple_build_cond (EQ_EXPR, arg,
				 build_zero_cst (TREE_TYPE (arg)),
				 NULL_TREE, NULL_TREE);
	  gimple_set_location (g, loc[0]);
	  gsi_insert_after (gsi, g, GSI_NEW_STMT);

	  *gsi = gsi_after_labels (then_bb);
	  if (flag_sanitize_undefined_trap_on_error)
	    g = gimple_build_call (builtin_decl_explicit (BUILT_IN_TRAP), 0);
	  else
	    {
	      tree data = ubsan_create_data ("__ubsan_nonnull_arg_data",
					     2, loc, NULL_TREE,
					     build_int_cst (integer_type_node,
							    i + 1),
					     NULL_TREE);
	      data = build_fold_addr_expr_loc (loc[0], data);
	      enum built_in_function bcode
		= (flag_sanitize_recover & SANITIZE_NONNULL_ATTRIBUTE)
		  ? BUILT_IN_UBSAN_HANDLE_NONNULL_ARG
		  : BUILT_IN_UBSAN_HANDLE_NONNULL_ARG_ABORT;
	      tree fn = builtin_decl_explicit (bcode);

	      g = gimple_build_call (fn, 1, data);
	    }
	  gimple_set_location (g, loc[0]);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	  ubsan_create_edge (g);
	}
      *gsi = gsi_for_stmt (stmt);
    }
  flag_delete_null_pointer_checks = save_flag_delete_null_pointer_checks;
}

/* Instrument returns in functions with returns_nonnull attribute.  */

static void
instrument_nonnull_return (gimple_stmt_iterator *gsi)
{
  greturn *stmt = as_a <greturn *> (gsi_stmt (*gsi));
  location_t loc[2];
  tree arg = gimple_return_retval (stmt);
  /* infer_nonnull_range needs flag_delete_null_pointer_checks set,
     while for nonnull return sanitization it is clear.  */
  int save_flag_delete_null_pointer_checks = flag_delete_null_pointer_checks;
  flag_delete_null_pointer_checks = 1;
  loc[0] = gimple_location (stmt);
  loc[1] = UNKNOWN_LOCATION;
  if (arg
      && POINTER_TYPE_P (TREE_TYPE (arg))
      && is_gimple_val (arg)
      && infer_nonnull_range_by_attribute (stmt, arg))
    {
      basic_block then_bb, fallthru_bb;
      *gsi = create_cond_insert_point (gsi, true, false, true,
				       &then_bb, &fallthru_bb);
      gimple *g = gimple_build_cond (EQ_EXPR, arg,
				    build_zero_cst (TREE_TYPE (arg)),
				    NULL_TREE, NULL_TREE);
      gimple_set_location (g, loc[0]);
      gsi_insert_after (gsi, g, GSI_NEW_STMT);

      *gsi = gsi_after_labels (then_bb);
      if (flag_sanitize_undefined_trap_on_error)
	g = gimple_build_call (builtin_decl_explicit (BUILT_IN_TRAP), 0);
      else
	{
	  tree data = ubsan_create_data ("__ubsan_nonnull_return_data",
					 1, &loc[1], NULL_TREE, NULL_TREE);
	  data = build_fold_addr_expr_loc (loc[0], data);
	  tree data2 = ubsan_create_data ("__ubsan_nonnull_return_data",
					  1, &loc[0], NULL_TREE, NULL_TREE);
	  data2 = build_fold_addr_expr_loc (loc[0], data2);
	  enum built_in_function bcode
	    = (flag_sanitize_recover & SANITIZE_RETURNS_NONNULL_ATTRIBUTE)
	      ? BUILT_IN_UBSAN_HANDLE_NONNULL_RETURN_V1
	      : BUILT_IN_UBSAN_HANDLE_NONNULL_RETURN_V1_ABORT;
	  tree fn = builtin_decl_explicit (bcode);

	  g = gimple_build_call (fn, 2, data, data2);
	}
      gimple_set_location (g, loc[0]);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
      ubsan_create_edge (g);
      *gsi = gsi_for_stmt (stmt);
    }
  flag_delete_null_pointer_checks = save_flag_delete_null_pointer_checks;
}

/* Instrument memory references.  Here we check whether the pointer
   points to an out-of-bounds location.  */

static void
instrument_object_size (gimple_stmt_iterator *gsi, tree t, bool is_lhs)
{
  gimple *stmt = gsi_stmt (*gsi);
  location_t loc = gimple_location (stmt);
  tree type;
  tree index = NULL_TREE;
  HOST_WIDE_INT size_in_bytes;

  type = TREE_TYPE (t);
  if (VOID_TYPE_P (type))
    return;

  switch (TREE_CODE (t))
    {
    case COMPONENT_REF:
      if (TREE_CODE (t) == COMPONENT_REF
	  && DECL_BIT_FIELD_REPRESENTATIVE (TREE_OPERAND (t, 1)) != NULL_TREE)
	{
	  tree repr = DECL_BIT_FIELD_REPRESENTATIVE (TREE_OPERAND (t, 1));
	  t = build3 (COMPONENT_REF, TREE_TYPE (repr), TREE_OPERAND (t, 0),
		      repr, TREE_OPERAND (t, 2));
	}
      break;
    case ARRAY_REF:
      index = TREE_OPERAND (t, 1);
      break;
    case INDIRECT_REF:
    case MEM_REF:
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      break;
    default:
      return;
    }

  size_in_bytes = int_size_in_bytes (type);
  if (size_in_bytes <= 0)
    return;

  HOST_WIDE_INT bitsize, bitpos;
  tree offset;
  machine_mode mode;
  int volatilep = 0, reversep, unsignedp = 0;
  tree inner = get_inner_reference (t, &bitsize, &bitpos, &offset, &mode,
				    &unsignedp, &reversep, &volatilep);

  if (bitpos % BITS_PER_UNIT != 0
      || bitsize != size_in_bytes * BITS_PER_UNIT)
    return;

  bool decl_p = DECL_P (inner);
  tree base;
  if (decl_p)
    {
      if (DECL_REGISTER (inner))
	return;
      base = inner;
    }
  else if (TREE_CODE (inner) == MEM_REF)
    base = TREE_OPERAND (inner, 0);
  else
    return;
  tree ptr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (t)), t);

  while (TREE_CODE (base) == SSA_NAME)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (base);
      if (gimple_assign_ssa_name_copy_p (def_stmt)
	  || (gimple_assign_cast_p (def_stmt)
	      && POINTER_TYPE_P (TREE_TYPE (gimple_assign_rhs1 (def_stmt))))
	  || (is_gimple_assign (def_stmt)
	      && gimple_assign_rhs_code (def_stmt) == POINTER_PLUS_EXPR))
	{
	  tree rhs1 = gimple_assign_rhs1 (def_stmt);
	  if (TREE_CODE (rhs1) == SSA_NAME
	      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs1))
	    break;
	  else
	    base = rhs1;
	}
      else
	break;
    }

  if (!POINTER_TYPE_P (TREE_TYPE (base)) && !DECL_P (base))
    return;

  tree sizet;
  tree base_addr = base;
  gimple *bos_stmt = NULL;
  if (decl_p)
    base_addr = build1 (ADDR_EXPR,
			build_pointer_type (TREE_TYPE (base)), base);
  unsigned HOST_WIDE_INT size;
  if (compute_builtin_object_size (base_addr, 0, &size))
    sizet = build_int_cst (sizetype, size);
  else if (optimize)
    {
      if (LOCATION_LOCUS (loc) == UNKNOWN_LOCATION)
	loc = input_location;
      /* Generate __builtin_object_size call.  */
      sizet = builtin_decl_explicit (BUILT_IN_OBJECT_SIZE);
      sizet = build_call_expr_loc (loc, sizet, 2, base_addr,
				   integer_zero_node);
      sizet = force_gimple_operand_gsi (gsi, sizet, false, NULL_TREE, true,
					GSI_SAME_STMT);
      /* If the call above didn't end up being an integer constant, go one
	 statement back and get the __builtin_object_size stmt.  Save it,
	 we might need it later.  */
      if (SSA_VAR_P (sizet))
	{
	  gsi_prev (gsi);
	  bos_stmt = gsi_stmt (*gsi);

	  /* Move on to where we were.  */
	  gsi_next (gsi);
	}
    }
  else
    return;

  /* Generate UBSAN_OBJECT_SIZE (ptr, ptr+sizeof(*ptr)-base, objsize, ckind)
     call.  */
  /* ptr + sizeof (*ptr) - base */
  t = fold_build2 (MINUS_EXPR, sizetype,
		   fold_convert (pointer_sized_int_node, ptr),
		   fold_convert (pointer_sized_int_node, base_addr));
  t = fold_build2 (PLUS_EXPR, sizetype, t, TYPE_SIZE_UNIT (type));

  /* Perhaps we can omit the check.  */
  if (TREE_CODE (t) == INTEGER_CST
      && TREE_CODE (sizet) == INTEGER_CST
      && tree_int_cst_le (t, sizet))
    return;

  if (index != NULL_TREE
      && TREE_CODE (index) == SSA_NAME
      && TREE_CODE (sizet) == INTEGER_CST)
    {
      gimple *def = SSA_NAME_DEF_STMT (index);
      if (is_gimple_assign (def)
	  && gimple_assign_rhs_code (def) == BIT_AND_EXPR
	  && TREE_CODE (gimple_assign_rhs2 (def)) == INTEGER_CST)
	{
	  tree cst = gimple_assign_rhs2 (def);
	  tree sz = fold_build2 (EXACT_DIV_EXPR, sizetype, sizet,
				 TYPE_SIZE_UNIT (type));
	  if (tree_int_cst_sgn (cst) >= 0
	      && tree_int_cst_lt (cst, sz))
	    return;
	}
    }

  if (bos_stmt && gimple_call_builtin_p (bos_stmt, BUILT_IN_OBJECT_SIZE))
    ubsan_create_edge (bos_stmt);

  /* We have to emit the check.  */
  t = force_gimple_operand_gsi (gsi, t, true, NULL_TREE, true,
				GSI_SAME_STMT);
  ptr = force_gimple_operand_gsi (gsi, ptr, true, NULL_TREE, true,
				  GSI_SAME_STMT);
  tree ckind = build_int_cst (unsigned_char_type_node,
			      is_lhs ? UBSAN_STORE_OF : UBSAN_LOAD_OF);
  gimple *g = gimple_build_call_internal (IFN_UBSAN_OBJECT_SIZE, 4,
					 ptr, t, sizet, ckind);
  gimple_set_location (g, loc);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
}

/* Instrument values passed to builtin functions.  */

static void
instrument_builtin (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  location_t loc = gimple_location (stmt);
  tree arg;
  enum built_in_function fcode
    = DECL_FUNCTION_CODE (gimple_call_fndecl (stmt));
  int kind = 0;
  switch (fcode)
    {
    CASE_INT_FN (BUILT_IN_CLZ):
      kind = 1;
      gcc_fallthrough ();
    CASE_INT_FN (BUILT_IN_CTZ):
      arg = gimple_call_arg (stmt, 0);
      if (!integer_nonzerop (arg))
	{
	  gimple *g;
	  if (!is_gimple_val (arg))
	    {
	      g = gimple_build_assign (make_ssa_name (TREE_TYPE (arg)), arg);
	      gimple_set_location (g, loc);
	      gsi_insert_before (gsi, g, GSI_SAME_STMT);
	      arg = gimple_assign_lhs (g);
	    }

	  basic_block then_bb, fallthru_bb;
	  *gsi = create_cond_insert_point (gsi, true, false, true,
					   &then_bb, &fallthru_bb);
	  g = gimple_build_cond (EQ_EXPR, arg,
				 build_zero_cst (TREE_TYPE (arg)),
				 NULL_TREE, NULL_TREE);
	  gimple_set_location (g, loc);
	  gsi_insert_after (gsi, g, GSI_NEW_STMT);

	  *gsi = gsi_after_labels (then_bb);
	  if (flag_sanitize_undefined_trap_on_error)
	    g = gimple_build_call (builtin_decl_explicit (BUILT_IN_TRAP), 0);
	  else
	    {
	      tree t = build_int_cst (unsigned_char_type_node, kind);
	      tree data = ubsan_create_data ("__ubsan_builtin_data",
					     1, &loc, NULL_TREE, t, NULL_TREE);
	      data = build_fold_addr_expr_loc (loc, data);
	      enum built_in_function bcode
		= (flag_sanitize_recover & SANITIZE_BUILTIN)
		  ? BUILT_IN_UBSAN_HANDLE_INVALID_BUILTIN
		  : BUILT_IN_UBSAN_HANDLE_INVALID_BUILTIN_ABORT;
	      tree fn = builtin_decl_explicit (bcode);

	      g = gimple_build_call (fn, 1, data);
	    }
	  gimple_set_location (g, loc);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	  ubsan_create_edge (g);
	}
      *gsi = gsi_for_stmt (stmt);
      break;
    default:
      break;
    }
}

namespace {

const pass_data pass_data_ubsan =
{
  GIMPLE_PASS, /* type */
  "ubsan", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
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
  virtual bool gate (function *)
    {
      return sanitize_flags_p ((SANITIZE_NULL | SANITIZE_SI_OVERFLOW
				| SANITIZE_BOOL | SANITIZE_ENUM
				| SANITIZE_ALIGNMENT
				| SANITIZE_NONNULL_ATTRIBUTE
				| SANITIZE_RETURNS_NONNULL_ATTRIBUTE
				| SANITIZE_OBJECT_SIZE
				| SANITIZE_POINTER_OVERFLOW
				| SANITIZE_BUILTIN));
    }

  virtual unsigned int execute (function *);

}; // class pass_ubsan

unsigned int
pass_ubsan::execute (function *fun)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  unsigned int ret = 0;

  initialize_sanitizer_builtins ();

  FOR_EACH_BB_FN (bb, fun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (is_gimple_debug (stmt) || gimple_clobber_p (stmt))
	    {
	      gsi_next (&gsi);
	      continue;
	    }

	  if ((sanitize_flags_p (SANITIZE_SI_OVERFLOW, fun->decl))
	      && is_gimple_assign (stmt))
	    instrument_si_overflow (gsi);

	  if (sanitize_flags_p (SANITIZE_NULL | SANITIZE_ALIGNMENT, fun->decl))
	    {
	      if (gimple_store_p (stmt))
		instrument_null (gsi, gimple_get_lhs (stmt), true);
	      if (gimple_assign_single_p (stmt))
		instrument_null (gsi, gimple_assign_rhs1 (stmt), false);
	      if (is_gimple_call (stmt))
		{
		  unsigned args_num = gimple_call_num_args (stmt);
		  for (unsigned i = 0; i < args_num; ++i)
		    {
		      tree arg = gimple_call_arg (stmt, i);
		      if (is_gimple_reg (arg) || is_gimple_min_invariant (arg))
			continue;
		      instrument_null (gsi, arg, false);
		    }
		}
	    }

	  if (sanitize_flags_p (SANITIZE_BOOL | SANITIZE_ENUM, fun->decl)
	      && gimple_assign_load_p (stmt))
	    {
	      instrument_bool_enum_load (&gsi);
	      bb = gimple_bb (stmt);
	    }

	  if (sanitize_flags_p (SANITIZE_NONNULL_ATTRIBUTE, fun->decl)
	      && is_gimple_call (stmt)
	      && !gimple_call_internal_p (stmt))
	    {
	      instrument_nonnull_arg (&gsi);
	      bb = gimple_bb (stmt);
	    }

	  if (sanitize_flags_p (SANITIZE_BUILTIN, fun->decl)
	      && gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
	    {
	      instrument_builtin (&gsi);
	      bb = gimple_bb (stmt);
	    }

	  if (sanitize_flags_p (SANITIZE_RETURNS_NONNULL_ATTRIBUTE, fun->decl)
	      && gimple_code (stmt) == GIMPLE_RETURN)
	    {
	      instrument_nonnull_return (&gsi);
	      bb = gimple_bb (stmt);
	    }

	  if (sanitize_flags_p (SANITIZE_OBJECT_SIZE, fun->decl))
	    {
	      if (gimple_store_p (stmt))
		instrument_object_size (&gsi, gimple_get_lhs (stmt), true);
	      if (gimple_assign_load_p (stmt))
		instrument_object_size (&gsi, gimple_assign_rhs1 (stmt),
					false);
	      if (is_gimple_call (stmt))
		{
		  unsigned args_num = gimple_call_num_args (stmt);
		  for (unsigned i = 0; i < args_num; ++i)
		    {
		      tree arg = gimple_call_arg (stmt, i);
		      if (is_gimple_reg (arg) || is_gimple_min_invariant (arg))
			continue;
		      instrument_object_size (&gsi, arg, false);
		    }
		}
	    }

	  if (sanitize_flags_p (SANITIZE_POINTER_OVERFLOW, fun->decl))
	    {
	      if (is_gimple_assign (stmt)
		  && gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
		instrument_pointer_overflow (&gsi,
					     gimple_assign_rhs1 (stmt),
					     gimple_assign_rhs2 (stmt));
	      if (gimple_store_p (stmt))
		maybe_instrument_pointer_overflow (&gsi,
						   gimple_get_lhs (stmt));
	      if (gimple_assign_single_p (stmt))
		maybe_instrument_pointer_overflow (&gsi,
						   gimple_assign_rhs1 (stmt));
	      if (is_gimple_call (stmt))
		{
		  unsigned args_num = gimple_call_num_args (stmt);
		  for (unsigned i = 0; i < args_num; ++i)
		    {
		      tree arg = gimple_call_arg (stmt, i);
		      if (is_gimple_reg (arg))
			continue;
		      maybe_instrument_pointer_overflow (&gsi, arg);
		    }
		}
	    }

	  gsi_next (&gsi);
	}
      if (gimple_purge_dead_eh_edges (bb))
	ret = TODO_cleanup_cfg;
    }
  return ret;
}

} // anon namespace

gimple_opt_pass *
make_pass_ubsan (gcc::context *ctxt)
{
  return new pass_ubsan (ctxt);
}

#include "gt-ubsan.h"
