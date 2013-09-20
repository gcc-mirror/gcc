/* UndefinedBehaviorSanitizer, undefined behavior detector.
   Copyright (C) 2013 Free Software Foundation, Inc.
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
#include "cgraph.h"
#include "gimple.h"
#include "hashtab.h"
#include "pointer-set.h"
#include "output.h"
#include "tm_p.h"
#include "toplev.h"
#include "ubsan.h"
#include "c-family/c-common.h"

/* Map from a tree to a VAR_DECL tree.  */

struct GTY(()) tree_type_map {
  struct tree_map_base type;
  tree decl;
};

#define tree_type_map_eq tree_map_base_eq
#define tree_type_map_hash tree_map_base_hash
#define tree_type_map_marked_p tree_map_base_marked_p

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
   the rest is passed by reference.  T is a value we are to encode.  */

tree
ubsan_encode_value (tree t)
{
  tree type = TREE_TYPE (t);
  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
      if (TYPE_PRECISION (type) <= POINTER_SIZE)
	return fold_build1 (NOP_EXPR, pointer_sized_int_node, t);
      else
	return build_fold_addr_expr (t);
    case REAL_TYPE:
      {
	unsigned int bitsize = GET_MODE_BITSIZE (TYPE_MODE (type));
	if (bitsize <= POINTER_SIZE)
	  {
	    tree itype = build_nonstandard_integer_type (bitsize, true);
	    t = fold_build1 (VIEW_CONVERT_EXPR, itype, t);
	    return fold_convert (pointer_sized_int_node, t);
	  }
	else
	  {
	    if (!TREE_ADDRESSABLE (t))
	      {
		/* The reason for this is that we don't want to pessimize
		   code by making vars unnecessarily addressable.  */
		tree var = create_tmp_var (TREE_TYPE (t), NULL);
		tree tem = build2 (MODIFY_EXPR, void_type_node, var, t);
		t = build_fold_addr_expr (var);
		return build2 (COMPOUND_EXPR, TREE_TYPE (t), tem, t);
	      }
	    else
	      return build_fold_addr_expr (t);
	  }
      }
    default:
      gcc_unreachable ();
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
  str = build_fold_addr_expr_loc (loc, str);
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
  gcc_assert (TYPE_SIZE (type) && host_integerp (TYPE_SIZE (type), 1));
  int prec = exact_log2 (tree_low_cst (TYPE_SIZE (type), 1));
  gcc_assert (prec != -1);
  return (prec << 1) | !TYPE_UNSIGNED (type);
}

/* Helper routine that returns ADDR_EXPR of a VAR_DECL of a type
   descriptor.  It first looks into the pointer map; if not found,
   create the VAR_DECL, put it into the pointer map and return the
   ADDR_EXPR of it.  TYPE describes a particular type.  */

tree
ubsan_type_descriptor (tree type)
{
  /* See through any typedefs.  */
  type = TYPE_MAIN_VARIANT (type);

  tree decl = decl_for_type_lookup (type);
  if (decl != NULL_TREE)
    return decl;

  tree dtype = ubsan_type_descriptor_type ();
  const char *tname;
  unsigned short tkind, tinfo;

  /* At least for INTEGER_TYPE/REAL_TYPE/COMPLEX_TYPE, this should work.
     ??? For e.g. type_unsigned_for (type), the TYPE_NAME would be NULL.  */
  if (TYPE_NAME (type) != NULL)
    tname = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
  else
    tname = "<unknown>";
  if (TREE_CODE (type) == INTEGER_TYPE)
    {
      /* For INTEGER_TYPE, this is 0x0000.  */
      tkind = 0x000;
      tinfo = get_ubsan_type_info_for_type (type);
    }
  else if (TREE_CODE (type) == REAL_TYPE)
    /* We don't have float support yet.  */
    gcc_unreachable ();
  else
    gcc_unreachable ();

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

  size_t len = strlen (tname);
  tree str = build_string (len + 1, tname);
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
  rest_of_decl_compilation (decl, 1, 0);

  /* Save the address of the VAR_DECL into the pointer map.  */
  decl = build_fold_addr_expr (decl);
  decl_for_type_insert (type, decl);

  return decl;
}

/* Create a structure for the ubsan library.  NAME is a name of the new
   structure.  The arguments in ... are of __ubsan_type_descriptor type
   and there are at most two of them.  */

tree
ubsan_create_data (const char *name, location_t loc, ...)
{
  va_list args;
  tree ret, t;
  tree fields[3];
  vec<tree, va_gc> *saved_args = NULL;
  size_t i = 0;

  /* Firstly, create a pointer to type descriptor type.  */
  tree td_type = ubsan_type_descriptor_type ();
  TYPE_READONLY (td_type) = 1;
  td_type = build_pointer_type (td_type);

  /* Create the structure type.  */
  ret = make_node (RECORD_TYPE);
  if (loc != UNKNOWN_LOCATION)
    {
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE,
			      ubsan_source_location_type ());
      DECL_CONTEXT (fields[i]) = ret;
      i++;
    }

  va_start (args, loc);
  for (t = va_arg (args, tree); t != NULL_TREE;
       i++, t = va_arg (args, tree))
    {
      gcc_checking_assert (i < 3);
      /* Save the tree argument for later use.  */
      vec_safe_push (saved_args, t);
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE,
			      td_type);
      DECL_CONTEXT (fields[i]) = ret;
      if (i)
	DECL_CHAIN (fields[i - 1]) = fields[i];
    }
  TYPE_FIELDS (ret) = fields[0];
  TYPE_NAME (ret) = get_identifier (name);
  layout_type (ret);
  va_end (args);

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

  TREE_CONSTANT (ctor) = 1;
  TREE_STATIC (ctor) = 1;
  DECL_INITIAL (var) = ctor;
  rest_of_decl_compilation (var, 1, 0);

  return var;
}

/* Instrument the __builtin_unreachable call.  We just call the libubsan
   routine instead.  */

tree
ubsan_instrument_unreachable (location_t loc)
{
  tree data = ubsan_create_data ("__ubsan_unreachable_data", loc, NULL_TREE);
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

#include "gt-ubsan.h"
