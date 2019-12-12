/* Library interface to C front end
   Copyright (C) 2014-2019 Free Software Foundation, Inc.

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

#include <cc1plugin-config.h>

#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#include "../gcc/config.h"

#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#include "gcc-plugin.h"
#include "system.h"
#include "coretypes.h"
#include "stringpool.h"

#include "gcc-interface.h"
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "options.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "c-tree.h"
#include "toplev.h"
#include "timevar.h"
#include "hash-table.h"
#include "tm.h"
#include "c-family/c-pragma.h"
#include "c-lang.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"

#include "callbacks.hh"
#include "connection.hh"
#include "marshall-c.hh"
#include "rpc.hh"

#ifdef __GNUC__
#pragma GCC visibility push(default)
#endif
int plugin_is_GPL_compatible;
#ifdef __GNUC__
#pragma GCC visibility pop
#endif



// This is put into the lang hooks when the plugin starts.

static void
plugin_print_error_function (diagnostic_context *context, const char *file,
			     diagnostic_info *diagnostic)
{
  if (current_function_decl != NULL_TREE
      && DECL_NAME (current_function_decl) != NULL_TREE
      && strcmp (IDENTIFIER_POINTER (DECL_NAME (current_function_decl)),
		 GCC_FE_WRAPPER_FUNCTION) == 0)
    return;
  lhd_print_error_function (context, file, diagnostic);
}



static unsigned long long
convert_out (tree t)
{
  return (unsigned long long) (uintptr_t) t;
}

static tree
convert_in (unsigned long long v)
{
  return (tree) (uintptr_t) v;
}



struct decl_addr_value
{
  tree decl;
  tree address;
};

struct decl_addr_hasher : free_ptr_hash<decl_addr_value>
{
  static inline hashval_t hash (const decl_addr_value *);
  static inline bool equal (const decl_addr_value *, const decl_addr_value *);
};

inline hashval_t
decl_addr_hasher::hash (const decl_addr_value *e)
{
  return IDENTIFIER_HASH_VALUE (DECL_NAME (e->decl));
}

inline bool
decl_addr_hasher::equal (const decl_addr_value *p1, const decl_addr_value *p2)
{
  return p1->decl == p2->decl;
}



struct string_hasher : nofree_ptr_hash<const char>
{
  static inline hashval_t hash (const char *s)
  {
    return htab_hash_string (s);
  }

  static inline bool equal (const char *p1, const char *p2)
  {
    return strcmp (p1, p2) == 0;
  }
};



// A wrapper for pushdecl that doesn't let gdb have a chance to
// instantiate a symbol.

static void
pushdecl_safe (tree decl)
{
  void (*save) (enum c_oracle_request, tree identifier);

  save = c_binding_oracle;
  c_binding_oracle = NULL;
  pushdecl (decl);
  c_binding_oracle = save;
}



struct plugin_context : public cc1_plugin::connection
{
  plugin_context (int fd);

  // Map decls to addresses.
  hash_table<decl_addr_hasher> address_map;

  // A collection of trees that are preserved for the GC.
  hash_table< nofree_ptr_hash<tree_node> > preserved;

  // File name cache.
  hash_table<string_hasher> file_names;

  // Perform GC marking.
  void mark ();

  // Preserve a tree during the plugin's operation.
  tree preserve (tree t)
  {
    tree_node **slot = preserved.find_slot (t, INSERT);
    *slot = t;
    return t;
  }

  location_t get_location_t (const char *filename,
			     unsigned int line_number)
  {
    if (filename == NULL)
      return UNKNOWN_LOCATION;

    filename = intern_filename (filename);
    linemap_add (line_table, LC_ENTER, false, filename, line_number);
    location_t loc = linemap_line_start (line_table, line_number, 0);
    linemap_add (line_table, LC_LEAVE, false, NULL, 0);
    return loc;
  }

private:

  // Add a file name to FILE_NAMES and return the canonical copy.
  const char *intern_filename (const char *filename)
  {
    const char **slot = file_names.find_slot (filename, INSERT);
    if (*slot == NULL)
      {
	/* The file name must live as long as the line map, which
	   effectively means as long as this compilation.  So, we copy
	   the string here but never free it.  */
	*slot = xstrdup (filename);
      }
    return *slot;
  }
};

static plugin_context *current_context;



plugin_context::plugin_context (int fd)
  : cc1_plugin::connection (fd),
    address_map (30),
    preserved (30),
    file_names (30)
{
}

void
plugin_context::mark ()
{
  for (hash_table<decl_addr_hasher>::iterator it = address_map.begin ();
       it != address_map.end ();
       ++it)
    {
      ggc_mark ((*it)->decl);
      ggc_mark ((*it)->address);
    }

  for (hash_table< nofree_ptr_hash<tree_node> >::iterator
	 it = preserved.begin (); it != preserved.end (); ++it)
    ggc_mark (&*it);
}

static void
plugin_binding_oracle (enum c_oracle_request kind, tree identifier)
{
  enum gcc_c_oracle_request request;

  gcc_assert (current_context != NULL);

  switch (kind)
    {
    case C_ORACLE_SYMBOL:
      request = GCC_C_ORACLE_SYMBOL;
      break;
    case C_ORACLE_TAG:
      request = GCC_C_ORACLE_TAG;
      break;
    case C_ORACLE_LABEL:
      request = GCC_C_ORACLE_LABEL;
      break;
    default:
      abort ();
    }

  int ignore;
  cc1_plugin::call (current_context, "binding_oracle", &ignore,
		    request, IDENTIFIER_POINTER (identifier));
}

static void
plugin_pragma_user_expression (cpp_reader *)
{
  c_binding_oracle = plugin_binding_oracle;
}

static void
plugin_init_extra_pragmas (void *, void *)
{
  c_register_pragma ("GCC", "user_expression", plugin_pragma_user_expression);
}



// Maybe rewrite a decl to its address.
static tree
address_rewriter (tree *in, int *walk_subtrees, void *arg)
{
  plugin_context *ctx = (plugin_context *) arg;

  if (!DECL_P (*in) || DECL_NAME (*in) == NULL_TREE)
    return NULL_TREE;

  decl_addr_value value;
  value.decl = *in;
  decl_addr_value *found_value = ctx->address_map.find (&value);
  if (found_value != NULL)
    ;
  else if (DECL_IS_BUILTIN (*in))
    {
      gcc_address address;

      if (!cc1_plugin::call (ctx, "address_oracle", &address,
			     IDENTIFIER_POINTER (DECL_NAME (*in))))
	return NULL_TREE;
      if (address == 0)
	return NULL_TREE;

      // Insert the decl into the address map in case it is referenced
      // again.
      value.address = build_int_cst_type (ptr_type_node, address);
      decl_addr_value **slot = ctx->address_map.find_slot (&value, INSERT);
      gcc_assert (*slot == NULL);
      *slot
	= static_cast<decl_addr_value *> (xmalloc (sizeof (decl_addr_value)));
      **slot = value;
      found_value = *slot;
    }
  else
    return NULL_TREE;

  if (found_value->address != error_mark_node)
    {
      // We have an address for the decl, so rewrite the tree.
      tree ptr_type = build_pointer_type (TREE_TYPE (*in));
      *in = fold_build1 (INDIRECT_REF, TREE_TYPE (*in),
			 fold_build1 (CONVERT_EXPR, ptr_type,
				      found_value->address));
    }

  *walk_subtrees = 0;

  return NULL_TREE;
}

// When generating code for gdb, we want to be able to use absolute
// addresses to refer to otherwise external objects that gdb knows
// about.  gdb passes in these addresses when building decls, and then
// before gimplification we go through the trees, rewriting uses to
// the equivalent of "*(TYPE *) ADDR".
static void
rewrite_decls_to_addresses (void *function_in, void *)
{
  tree function = (tree) function_in;

  // Do nothing if we're not in gdb.
  if (current_context == NULL)
    return;

  walk_tree (&DECL_SAVED_TREE (function), address_rewriter, current_context,
	     NULL);
}



gcc_decl
plugin_build_decl (cc1_plugin::connection *self,
		   const char *name,
		   enum gcc_c_symbol_kind sym_kind,
		   gcc_type sym_type_in,
		   const char *substitution_name,
		   gcc_address address,
		   const char *filename,
		   unsigned int line_number)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree identifier = get_identifier (name);
  enum tree_code code;
  tree decl;
  tree sym_type = convert_in (sym_type_in);

  switch (sym_kind)
    {
    case GCC_C_SYMBOL_FUNCTION:
      code = FUNCTION_DECL;
      break;

    case GCC_C_SYMBOL_VARIABLE:
      code = VAR_DECL;
      break;

    case GCC_C_SYMBOL_TYPEDEF:
      code = TYPE_DECL;
      break;

    case GCC_C_SYMBOL_LABEL:
      // FIXME: we aren't ready to handle labels yet.
      // It isn't clear how to translate them properly
      // and in any case a "goto" isn't likely to work.
      return convert_out (error_mark_node);

    default:
      abort ();
    }

  location_t loc = ctx->get_location_t (filename, line_number);

  decl = build_decl (loc, code, identifier, sym_type);
  TREE_USED (decl) = 1;
  TREE_ADDRESSABLE (decl) = 1;

  if (sym_kind != GCC_C_SYMBOL_TYPEDEF)
    {
      decl_addr_value value;

      DECL_EXTERNAL (decl) = 1;
      value.decl = decl;
      if (substitution_name != NULL)
	{
	  // If the translator gave us a name without a binding,
	  // we can just substitute error_mark_node, since we know the
	  // translator will be reporting an error anyhow.
	  value.address
	    = lookup_name (get_identifier (substitution_name));
	  if (value.address == NULL_TREE)
	    value.address = error_mark_node;
	}
      else
	value.address = build_int_cst_type (ptr_type_node, address);
      decl_addr_value **slot = ctx->address_map.find_slot (&value, INSERT);
      gcc_assert (*slot == NULL);
      *slot
	= static_cast<decl_addr_value *> (xmalloc (sizeof (decl_addr_value)));
      **slot = value;
    }

  return convert_out (ctx->preserve (decl));
}

int
plugin_bind (cc1_plugin::connection *,
	     gcc_decl decl_in, int is_global)
{
  tree decl = convert_in (decl_in);
  c_bind (DECL_SOURCE_LOCATION (decl), decl, is_global);
  rest_of_decl_compilation (decl, is_global, 0);
  return 1;
}

int
plugin_tagbind (cc1_plugin::connection *self,
		const char *name, gcc_type tagged_type,
		const char *filename, unsigned int line_number)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree t = convert_in (tagged_type), x;
  c_pushtag (ctx->get_location_t (filename, line_number),
	     get_identifier (name), t);

  /* Propagate the newly-added type name so that previously-created
     variant types are not disconnected from their main variants.  */
  for (x = TYPE_MAIN_VARIANT (t); x; x = TYPE_NEXT_VARIANT (x))
    TYPE_NAME (x) = TYPE_NAME (t);

  return 1;
}

gcc_type
plugin_build_pointer_type (cc1_plugin::connection *,
			   gcc_type base_type)
{
  // No need to preserve a pointer type as the base type is preserved.
  return convert_out (build_pointer_type (convert_in (base_type)));
}

// TYPE_NAME needs to be a valid pointer, even if there is no name available.

static tree
build_anonymous_node (enum tree_code code)
{
  tree node = make_node (code);
  tree type_decl = build_decl (input_location, TYPE_DECL, NULL_TREE, node);
  TYPE_NAME (node) = type_decl;
  TYPE_STUB_DECL (node) = type_decl;
  return node;
}

gcc_type
plugin_build_record_type (cc1_plugin::connection *self)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  return convert_out (ctx->preserve (build_anonymous_node (RECORD_TYPE)));
}

gcc_type
plugin_build_union_type (cc1_plugin::connection *self)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  return convert_out (ctx->preserve (build_anonymous_node (UNION_TYPE)));
}

int
plugin_build_add_field (cc1_plugin::connection *,
			gcc_type record_or_union_type_in,
			const char *field_name,
			gcc_type field_type_in,
			unsigned long bitsize,
			unsigned long bitpos)
{
  tree record_or_union_type = convert_in (record_or_union_type_in);
  tree field_type = convert_in (field_type_in);

  gcc_assert (TREE_CODE (record_or_union_type) == RECORD_TYPE
	      || TREE_CODE (record_or_union_type) == UNION_TYPE);

  /* Note that gdb does not preserve the location of field decls, so
     we can't provide a decent location here.  */
  tree decl = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			  get_identifier (field_name), field_type);
  DECL_FIELD_CONTEXT (decl) = record_or_union_type;

  if (TREE_CODE (field_type) == INTEGER_TYPE
      && TYPE_PRECISION (field_type) != bitsize)
    {
      DECL_BIT_FIELD_TYPE (decl) = field_type;
      TREE_TYPE (decl)
	= c_build_bitfield_integer_type (bitsize, TYPE_UNSIGNED (field_type));
    }

  SET_DECL_MODE (decl, TYPE_MODE (TREE_TYPE (decl)));

  // There's no way to recover this from DWARF.
  SET_DECL_OFFSET_ALIGN (decl, TYPE_PRECISION (pointer_sized_int_node));

  tree pos = bitsize_int (bitpos);
  pos_from_bit (&DECL_FIELD_OFFSET (decl), &DECL_FIELD_BIT_OFFSET (decl),
		DECL_OFFSET_ALIGN (decl), pos);

  DECL_SIZE (decl) = bitsize_int (bitsize);
  DECL_SIZE_UNIT (decl) = size_int ((bitsize + BITS_PER_UNIT - 1)
				    / BITS_PER_UNIT);

  DECL_CHAIN (decl) = TYPE_FIELDS (record_or_union_type);
  TYPE_FIELDS (record_or_union_type) = decl;

  return 1;
}

int
plugin_finish_record_or_union (cc1_plugin::connection *,
			       gcc_type record_or_union_type_in,
			       unsigned long size_in_bytes)
{
  tree record_or_union_type = convert_in (record_or_union_type_in);

  gcc_assert (TREE_CODE (record_or_union_type) == RECORD_TYPE
	      || TREE_CODE (record_or_union_type) == UNION_TYPE);

  /* We built the field list in reverse order, so fix it now.  */
  TYPE_FIELDS (record_or_union_type)
    = nreverse (TYPE_FIELDS (record_or_union_type));

  if (TREE_CODE (record_or_union_type) == UNION_TYPE)
    {
      /* Unions can just be handled by the generic code.  */
      layout_type (record_or_union_type);
    }
  else
    {
      // FIXME there's no way to get this from DWARF,
      // or even, it seems, a particularly good way to deduce it.
      SET_TYPE_ALIGN (record_or_union_type,
		      TYPE_PRECISION (pointer_sized_int_node));

      TYPE_SIZE (record_or_union_type) = bitsize_int (size_in_bytes
						      * BITS_PER_UNIT);
      TYPE_SIZE_UNIT (record_or_union_type) = size_int (size_in_bytes);

      compute_record_mode (record_or_union_type);
      finish_bitfield_layout (record_or_union_type);
      // FIXME we have no idea about TYPE_PACKED
    }

  tree t = record_or_union_type, x;
  for (x = TYPE_MAIN_VARIANT (t); x; x = TYPE_NEXT_VARIANT (x))
    {
      /* Like finish_struct, update the qualified variant types.  */
      TYPE_FIELDS (x) = TYPE_FIELDS (t);
      TYPE_LANG_SPECIFIC (x) = TYPE_LANG_SPECIFIC (t);
      C_TYPE_FIELDS_READONLY (x) = C_TYPE_FIELDS_READONLY (t);
      C_TYPE_FIELDS_VOLATILE (x) = C_TYPE_FIELDS_VOLATILE (t);
      C_TYPE_VARIABLE_SIZE (x) = C_TYPE_VARIABLE_SIZE (t);
      /* We copy these fields too.  */
      SET_TYPE_ALIGN (x, TYPE_ALIGN (t));
      TYPE_SIZE (x) = TYPE_SIZE (t);
      TYPE_SIZE_UNIT (x) = TYPE_SIZE_UNIT (t);
      if (x != record_or_union_type)
	compute_record_mode (x);
    }

  return 1;
}

gcc_type
plugin_build_enum_type (cc1_plugin::connection *self,
			gcc_type underlying_int_type_in)
{
  tree underlying_int_type = convert_in (underlying_int_type_in);

  if (underlying_int_type == error_mark_node)
    return convert_out (error_mark_node);

  tree result = build_anonymous_node (ENUMERAL_TYPE);

  TYPE_PRECISION (result) = TYPE_PRECISION (underlying_int_type);
  TYPE_UNSIGNED (result) = TYPE_UNSIGNED (underlying_int_type);

  plugin_context *ctx = static_cast<plugin_context *> (self);
  return convert_out (ctx->preserve (result));
}

int
plugin_build_add_enum_constant (cc1_plugin::connection *,
				gcc_type enum_type_in,
				const char *name,
				unsigned long value)
{
  tree cst, decl, cons;
  tree enum_type = convert_in (enum_type_in);

  gcc_assert (TREE_CODE (enum_type) == ENUMERAL_TYPE);

  cst = build_int_cst (enum_type, value);
  /* Note that gdb does not preserve the location of enum constants,
     so we can't provide a decent location here.  */
  decl = build_decl (BUILTINS_LOCATION, CONST_DECL,
		     get_identifier (name), enum_type);
  DECL_INITIAL (decl) = cst;
  pushdecl_safe (decl);

  cons = tree_cons (DECL_NAME (decl), cst, TYPE_VALUES (enum_type));
  TYPE_VALUES (enum_type) = cons;

  return 1;
}

int
plugin_finish_enum_type (cc1_plugin::connection *,
			 gcc_type enum_type_in)
{
  tree enum_type = convert_in (enum_type_in);
  tree minnode, maxnode, iter;

  iter = TYPE_VALUES (enum_type);
  minnode = maxnode = TREE_VALUE (iter);
  for (iter = TREE_CHAIN (iter);
       iter != NULL_TREE;
       iter = TREE_CHAIN (iter))
    {
      tree value = TREE_VALUE (iter);
      if (tree_int_cst_lt (maxnode, value))
	maxnode = value;
      if (tree_int_cst_lt (value, minnode))
	minnode = value;
    }
  TYPE_MIN_VALUE (enum_type) = minnode;
  TYPE_MAX_VALUE (enum_type) = maxnode;

  layout_type (enum_type);

  return 1;
}

gcc_type
plugin_build_function_type (cc1_plugin::connection *self,
			    gcc_type return_type_in,
			    const struct gcc_type_array *argument_types_in,
			    int is_varargs)
{
  tree *argument_types;
  tree return_type = convert_in (return_type_in);
  tree result;

  argument_types = new tree[argument_types_in->n_elements];
  for (int i = 0; i < argument_types_in->n_elements; ++i)
    argument_types[i] = convert_in (argument_types_in->elements[i]);

  if (is_varargs)
    result = build_varargs_function_type_array (return_type,
						argument_types_in->n_elements,
						argument_types);
  else
    result = build_function_type_array (return_type,
					argument_types_in->n_elements,
					argument_types);

  delete[] argument_types;

  plugin_context *ctx = static_cast<plugin_context *> (self);
  return convert_out (ctx->preserve (result));
}

/* Return a builtin type associated with BUILTIN_NAME.  */

static tree
safe_lookup_builtin_type (const char *builtin_name)
{
  tree result = NULL_TREE;

  if (!builtin_name)
    return result;

  result = identifier_global_value (get_identifier (builtin_name));

  if (!result)
    return result;

  gcc_assert (TREE_CODE (result) == TYPE_DECL);
  result = TREE_TYPE (result);
  return result;
}

static gcc_type
plugin_int_check (cc1_plugin::connection *self,
		  int is_unsigned, unsigned long size_in_bytes,
		  tree result)
{
  if (result == NULL_TREE)
    result = error_mark_node;
  else
    {
      gcc_assert (!TYPE_UNSIGNED (result) == !is_unsigned);
      gcc_assert (TREE_CODE (TYPE_SIZE (result)) == INTEGER_CST);
      gcc_assert (TYPE_PRECISION (result) == BITS_PER_UNIT * size_in_bytes);

      plugin_context *ctx = static_cast<plugin_context *> (self);
      ctx->preserve (result);
    }
  return convert_out (result);
}

gcc_type
plugin_int_type_v0 (cc1_plugin::connection *self,
		    int is_unsigned, unsigned long size_in_bytes)
{
  tree result = c_common_type_for_size (BITS_PER_UNIT * size_in_bytes,
					is_unsigned);

  return plugin_int_check (self, is_unsigned, size_in_bytes, result);
}

gcc_type
plugin_int_type (cc1_plugin::connection *self,
		 int is_unsigned, unsigned long size_in_bytes,
		 const char *builtin_name)
{
  if (!builtin_name)
    return plugin_int_type_v0 (self, is_unsigned, size_in_bytes);

  tree result = safe_lookup_builtin_type (builtin_name);
  gcc_assert (!result || TREE_CODE (result) == INTEGER_TYPE);

  return plugin_int_check (self, is_unsigned, size_in_bytes, result);
}

gcc_type
plugin_char_type (cc1_plugin::connection *)
{
  return convert_out (char_type_node);
}

gcc_type
plugin_float_type_v0 (cc1_plugin::connection *,
		   unsigned long size_in_bytes)
{
  if (BITS_PER_UNIT * size_in_bytes == TYPE_PRECISION (float_type_node))
    return convert_out (float_type_node);
  if (BITS_PER_UNIT * size_in_bytes == TYPE_PRECISION (double_type_node))
    return convert_out (double_type_node);
  if (BITS_PER_UNIT * size_in_bytes == TYPE_PRECISION (long_double_type_node))
    return convert_out (long_double_type_node);
  return convert_out (error_mark_node);
}

gcc_type
plugin_float_type (cc1_plugin::connection *self,
		   unsigned long size_in_bytes,
		   const char *builtin_name)
{
  if (!builtin_name)
    return plugin_float_type_v0 (self, size_in_bytes);

  tree result = safe_lookup_builtin_type (builtin_name);

  if (!result)
    return convert_out (error_mark_node);

  gcc_assert (TREE_CODE (result) == REAL_TYPE);
  gcc_assert (BITS_PER_UNIT * size_in_bytes == TYPE_PRECISION (result));

  return convert_out (result);
}

gcc_type
plugin_void_type (cc1_plugin::connection *)
{
  return convert_out (void_type_node);
}

gcc_type
plugin_bool_type (cc1_plugin::connection *)
{
  return convert_out (boolean_type_node);
}

gcc_type
plugin_build_array_type (cc1_plugin::connection *self,
			 gcc_type element_type_in, int num_elements)
{
  tree element_type = convert_in (element_type_in);
  tree result;

  if (num_elements == -1)
    result = build_array_type (element_type, NULL_TREE);
  else
    result = build_array_type_nelts (element_type, num_elements);

  plugin_context *ctx = static_cast<plugin_context *> (self);
  return convert_out (ctx->preserve (result));
}

gcc_type
plugin_build_vla_array_type (cc1_plugin::connection *self,
			     gcc_type element_type_in,
			     const char *upper_bound_name)
{
  tree element_type = convert_in (element_type_in);
  tree upper_bound = lookup_name (get_identifier (upper_bound_name));
  tree range = build_index_type (upper_bound);

  tree result = build_array_type (element_type, range);
  C_TYPE_VARIABLE_SIZE (result) = 1;

  plugin_context *ctx = static_cast<plugin_context *> (self);
  return convert_out (ctx->preserve (result));
}

gcc_type
plugin_build_qualified_type (cc1_plugin::connection *,
			     gcc_type unqualified_type_in,
			     enum gcc_qualifiers qualifiers)
{
  tree unqualified_type = convert_in (unqualified_type_in);
  int quals = 0;

  if ((qualifiers & GCC_QUALIFIER_CONST) != 0)
    quals |= TYPE_QUAL_CONST;
  if ((qualifiers & GCC_QUALIFIER_VOLATILE) != 0)
    quals |= TYPE_QUAL_VOLATILE;
  if ((qualifiers & GCC_QUALIFIER_RESTRICT) != 0)
    quals |= TYPE_QUAL_RESTRICT;

  return convert_out (build_qualified_type (unqualified_type, quals));
}

gcc_type
plugin_build_complex_type (cc1_plugin::connection *self,
			   gcc_type base_type)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  return convert_out (ctx->preserve (build_complex_type (convert_in (base_type))));
}

gcc_type
plugin_build_vector_type (cc1_plugin::connection *self,
			  gcc_type base_type, int nunits)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  return convert_out (ctx->preserve (build_vector_type (convert_in (base_type),
							nunits)));
}

int
plugin_build_constant (cc1_plugin::connection *self, gcc_type type_in,
		       const char *name, unsigned long value,
		       const char *filename, unsigned int line_number)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree cst, decl;
  tree type = convert_in (type_in);

  cst = build_int_cst (type, value);
  decl = build_decl (ctx->get_location_t (filename, line_number),
		     CONST_DECL, get_identifier (name), type);
  DECL_INITIAL (decl) = cst;
  pushdecl_safe (decl);

  return 1;
}

gcc_type
plugin_error (cc1_plugin::connection *,
	      const char *message)
{
  error ("%s", message);
  return convert_out (error_mark_node);
}



// Perform GC marking.

static void
gc_mark (void *, void *)
{
  if (current_context != NULL)
    current_context->mark ();
}

#ifdef __GNUC__
#pragma GCC visibility push(default)
#endif

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *)
{
  long fd = -1;
  for (int i = 0; i < plugin_info->argc; ++i)
    {
      if (strcmp (plugin_info->argv[i].key, "fd") == 0)
	{
	  char *tail;
	  errno = 0;
	  fd = strtol (plugin_info->argv[i].value, &tail, 0);
	  if (*tail != '\0' || errno != 0)
	    fatal_error (input_location,
			 "%s: invalid file descriptor argument to plugin",
			 plugin_info->base_name);
	  break;
	}
    }
  if (fd == -1)
    fatal_error (input_location,
		 "%s: required plugin argument %<fd%> is missing",
		 plugin_info->base_name);

  current_context = new plugin_context (fd);

  // Handshake.
  cc1_plugin::protocol_int version;
  if (!current_context->require ('H')
      || ! ::cc1_plugin::unmarshall (current_context, &version))
    fatal_error (input_location,
		 "%s: handshake failed", plugin_info->base_name);
  if (version != GCC_C_FE_VERSION_1)
    fatal_error (input_location,
		 "%s: unknown version in handshake", plugin_info->base_name);

  register_callback (plugin_info->base_name, PLUGIN_PRAGMAS,
		     plugin_init_extra_pragmas, NULL);
  register_callback (plugin_info->base_name, PLUGIN_PRE_GENERICIZE,
		     rewrite_decls_to_addresses, NULL);
  register_callback (plugin_info->base_name, PLUGIN_GGC_MARKING,
		     gc_mark, NULL);

  lang_hooks.print_error_function = plugin_print_error_function;

#define GCC_METHOD0(R, N)			\
  {						\
    cc1_plugin::callback_ftype *fun		\
      = cc1_plugin::callback<R, plugin_ ## N>;	\
    current_context->add_callback (# N, fun);	\
  }
#define GCC_METHOD1(R, N, A)				\
  {							\
    cc1_plugin::callback_ftype *fun			\
      = cc1_plugin::callback<R, A, plugin_ ## N>;	\
    current_context->add_callback (# N, fun);		\
  }
#define GCC_METHOD2(R, N, A, B)				\
  {							\
    cc1_plugin::callback_ftype *fun			\
      = cc1_plugin::callback<R, A, B, plugin_ ## N>;	\
    current_context->add_callback (# N, fun);		\
  }
#define GCC_METHOD3(R, N, A, B, C)			\
  {							\
    cc1_plugin::callback_ftype *fun			\
      = cc1_plugin::callback<R, A, B, C, plugin_ ## N>;	\
    current_context->add_callback (# N, fun);		\
  }
#define GCC_METHOD4(R, N, A, B, C, D)		\
  {						\
    cc1_plugin::callback_ftype *fun		\
      = cc1_plugin::callback<R, A, B, C, D,	\
			     plugin_ ## N>;	\
    current_context->add_callback (# N, fun);	\
  }
#define GCC_METHOD5(R, N, A, B, C, D, E)	\
  {						\
    cc1_plugin::callback_ftype *fun		\
      = cc1_plugin::callback<R, A, B, C, D, E,	\
			     plugin_ ## N>;	\
    current_context->add_callback (# N, fun);	\
  }
#define GCC_METHOD7(R, N, A, B, C, D, E, F, G)		\
  {							\
    cc1_plugin::callback_ftype *fun			\
      = cc1_plugin::callback<R, A, B, C, D, E, F, G,	\
			     plugin_ ## N>;		\
    current_context->add_callback (# N, fun);		\
  }

#include "gcc-c-fe.def"

#undef GCC_METHOD0
#undef GCC_METHOD1
#undef GCC_METHOD2
#undef GCC_METHOD3
#undef GCC_METHOD4
#undef GCC_METHOD5
#undef GCC_METHOD7

  return 0;
}
