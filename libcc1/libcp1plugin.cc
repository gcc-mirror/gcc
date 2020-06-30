/* Library interface to C++ front end.
   Copyright (C) 2014-2020 Free Software Foundation, Inc.

   This file is part of GCC.  As it interacts with GDB through libcc1,
   they all become a single program as regards the GNU GPL's requirements.

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
#include "cp-tree.h"
#include "toplev.h"
#include "timevar.h"
#include "hash-table.h"
#include "tm.h"
#include "c-family/c-pragma.h"
// #include "c-lang.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "decl.h"
#include "function.h"
#undef cfun // we want to assign to it, and function.h won't let us

#include "callbacks.hh"
#include "connection.hh"
#include "marshall-cp.hh"
#include "rpc.hh"

#ifdef __GNUC__
#pragma GCC visibility push(default)
#endif
int plugin_is_GPL_compatible;
#ifdef __GNUC__
#pragma GCC visibility pop
#endif



static int ATTRIBUTE_UNUSED
check_symbol_mask[GCC_CP_SYMBOL_MASK >= GCC_CP_SYMBOL_END ? 1 : -1];

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
  return DECL_UID (e->decl);
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
plugin_binding_oracle (enum cp_oracle_request kind, tree identifier)
{
  enum gcc_cp_oracle_request request;

  gcc_assert (current_context != NULL);

  switch (kind)
    {
    case CP_ORACLE_IDENTIFIER:
      request = GCC_CP_ORACLE_IDENTIFIER;
      break;
    default:
      abort ();
    }

  int ignore;
  cc1_plugin::call (current_context, "binding_oracle", &ignore,
		    request, IDENTIFIER_POINTER (identifier));
}

static int push_count;

/* at_function_scope_p () tests cfun, indicating we're actually
   compiling the function, but we don't even set it when pretending to
   enter a function scope.  We use this distinction to tell these two
   cases apart: we don't want to define e.g. class names in the user
   expression function's scope, when they're local to the original
   function, because they'd get the wrong linkage name.  */

static bool
at_fake_function_scope_p ()
{
  return (!cfun || cfun->decl != current_function_decl)
    && current_scope () == current_function_decl;
}

static void
push_fake_function (tree fndecl, scope_kind kind = sk_function_parms)
{
  current_function_decl = fndecl;
  begin_scope (kind, fndecl);
  ++function_depth;
  begin_scope (sk_block, NULL);
}

static void
pop_scope ()
{
  if (toplevel_bindings_p () && current_namespace == global_namespace)
    pop_from_top_level ();
  else if (at_namespace_scope_p ())
    pop_namespace ();
  else if (at_class_scope_p ())
    popclass ();
  else
    {
      gcc_assert (at_fake_function_scope_p ());
      gcc_assert (!at_function_scope_p ());
      gcc_assert (current_binding_level->kind == sk_block
		  && current_binding_level->this_entity == NULL);
      leave_scope ();
      --function_depth;
      gcc_assert (current_binding_level->this_entity
		  == current_function_decl);
      leave_scope ();
      current_function_decl = NULL;
      for (cp_binding_level *scope = current_binding_level;
	   scope; scope = scope->level_chain)
	if (scope->kind == sk_function_parms)
	  {
	    current_function_decl = scope->this_entity;
	    break;
	  }
    }
}

static void
supplement_binding (cxx_binding *binding, tree decl)
{
  /* FIXME: this is pretty much a copy of supplement_binding_1 in
     ../gcc/cp/name-lookup.c; the few replaced/removed bits are marked
     with "// _1:".  */
  tree bval = binding->value;
  bool ok = true;
  tree target_bval = strip_using_decl (bval);
  tree target_decl = strip_using_decl (decl);

  if (TREE_CODE (target_decl) == TYPE_DECL && DECL_ARTIFICIAL (target_decl)
      && target_decl != target_bval
      && (TREE_CODE (target_bval) != TYPE_DECL
	  /* We allow pushing an enum multiple times in a class
	     template in order to handle late matching of underlying
	     type on an opaque-enum-declaration followed by an
	     enum-specifier.  */
	  || (processing_template_decl
	      && TREE_CODE (TREE_TYPE (target_decl)) == ENUMERAL_TYPE
	      && TREE_CODE (TREE_TYPE (target_bval)) == ENUMERAL_TYPE
	      && (dependent_type_p (ENUM_UNDERLYING_TYPE
				    (TREE_TYPE (target_decl)))
		  || dependent_type_p (ENUM_UNDERLYING_TYPE
				       (TREE_TYPE (target_bval)))))))
    /* The new name is the type name.  */
    binding->type = decl;
  else if (/* TARGET_BVAL is null when push_class_level_binding moves
	      an inherited type-binding out of the way to make room
	      for a new value binding.  */
	   !target_bval
	   /* TARGET_BVAL is error_mark_node when TARGET_DECL's name
	      has been used in a non-class scope prior declaration.
	      In that case, we should have already issued a
	      diagnostic; for graceful error recovery purpose, pretend
	      this was the intended declaration for that name.  */
	   || target_bval == error_mark_node
	   /* If TARGET_BVAL is anticipated but has not yet been
	      declared, pretend it is not there at all.  */
	   || (TREE_CODE (target_bval) == FUNCTION_DECL
	       && DECL_ANTICIPATED (target_bval)
	       && !DECL_HIDDEN_FRIEND_P (target_bval)))
    binding->value = decl;
  else if (TREE_CODE (target_bval) == TYPE_DECL
	   && DECL_ARTIFICIAL (target_bval)
	   && target_decl != target_bval
	   && (TREE_CODE (target_decl) != TYPE_DECL
	       || same_type_p (TREE_TYPE (target_decl),
			       TREE_TYPE (target_bval))))
    {
      /* The old binding was a type name.  It was placed in
	 VALUE field because it was thought, at the point it was
	 declared, to be the only entity with such a name.  Move the
	 type name into the type slot; it is now hidden by the new
	 binding.  */
      binding->type = bval;
      binding->value = decl;
      binding->value_is_inherited = false;
    }
  else if (TREE_CODE (target_bval) == TYPE_DECL
	   && TREE_CODE (target_decl) == TYPE_DECL
	   && DECL_NAME (target_decl) == DECL_NAME (target_bval)
	   && binding->scope->kind != sk_class
	   && (same_type_p (TREE_TYPE (target_decl), TREE_TYPE (target_bval))
	       /* If either type involves template parameters, we must
		  wait until instantiation.  */
	       || uses_template_parms (TREE_TYPE (target_decl))
	       || uses_template_parms (TREE_TYPE (target_bval))))
    /* We have two typedef-names, both naming the same type to have
       the same name.  In general, this is OK because of:

	 [dcl.typedef]

	 In a given scope, a typedef specifier can be used to redefine
	 the name of any type declared in that scope to refer to the
	 type to which it already refers.

       However, in class scopes, this rule does not apply due to the
       stricter language in [class.mem] prohibiting redeclarations of
       members.  */
    ok = false;
  /* There can be two block-scope declarations of the same variable,
     so long as they are `extern' declarations.  However, there cannot
     be two declarations of the same static data member:

       [class.mem]

       A member shall not be declared twice in the
       member-specification.  */
  else if (VAR_P (target_decl)
	   && VAR_P (target_bval)
	   && DECL_EXTERNAL (target_decl) && DECL_EXTERNAL (target_bval)
	   && !DECL_CLASS_SCOPE_P (target_decl))
    {
      duplicate_decls (decl, binding->value, /*newdecl_is_friend=*/false);
      ok = false;
    }
  else if (TREE_CODE (decl) == NAMESPACE_DECL
	   && TREE_CODE (bval) == NAMESPACE_DECL
	   && DECL_NAMESPACE_ALIAS (decl)
	   && DECL_NAMESPACE_ALIAS (bval)
	   && ORIGINAL_NAMESPACE (bval) == ORIGINAL_NAMESPACE (decl))
    /* [namespace.alias]

      In a declarative region, a namespace-alias-definition can be
      used to redefine a namespace-alias declared in that declarative
      region to refer only to the namespace to which it already
      refers.  */
    ok = false;
  else
    {
      // _1: diagnose_name_conflict (decl, bval);
      ok = false;
    }

  gcc_assert (ok); // _1: return ok;
}

static void
reactivate_decl (tree decl, cp_binding_level *b)
{
  bool in_function_p = TREE_CODE (b->this_entity) == FUNCTION_DECL;
  gcc_assert (in_function_p
	      || (b == current_binding_level
		  && !at_class_scope_p ()));

  tree id = DECL_NAME (decl);
  tree type = NULL_TREE;
  if (TREE_CODE (decl) == TYPE_DECL)
    type = TREE_TYPE (decl);

  if (type && TYPE_NAME (type) == decl
      && (RECORD_OR_UNION_CODE_P (TREE_CODE (type))
	  || TREE_CODE (type) == ENUMERAL_TYPE))
    {
      gcc_assert (in_function_p && DECL_CONTEXT (decl) == b->this_entity);
      type = TREE_TYPE (decl);
    }
  else
    {
      gcc_assert (DECL_CONTEXT (decl) == b->this_entity
		  || DECL_CONTEXT (decl) == global_namespace
		  || TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL);
      type = NULL_TREE;
    }

  /* Adjust IDENTIFIER_BINDING to what it would have been if we were
     at binding level B.  Save the binding chain up to that point in
     [binding, *chainp), and take note of the outermost bindings found
     before B.  */
  cxx_binding *binding = IDENTIFIER_BINDING (id), **chainp = NULL;
  tree *shadowing_type_p = NULL;
  if (binding)
    {
      cp_binding_level *bc = current_binding_level;
      for (cxx_binding *prev_binding = binding;
	   prev_binding; prev_binding = prev_binding->previous)
	{
	  while (bc != b && bc != prev_binding->scope)
	    bc = bc->level_chain;
	  if (bc == b)
	    {
	      if (!chainp)
		binding = NULL;
	      break;
	    }
	  chainp = &prev_binding->previous;
	  if (type)
	    for (tree tshadow = prev_binding->scope->type_shadowed;
		 tshadow; tshadow = TREE_CHAIN (tshadow))
	      if (TREE_PURPOSE (tshadow) == id)
		{
		  shadowing_type_p = &TREE_VALUE (tshadow);
		  break;
		}
	}
    }
  if (chainp)
    {
      IDENTIFIER_BINDING (id) = *chainp;
      *chainp = NULL;
    }

  /* Like push_local_binding, supplement or add a binding to the
     desired level.  */
  if (IDENTIFIER_BINDING (id) && IDENTIFIER_BINDING (id)->scope == b)
    supplement_binding (IDENTIFIER_BINDING (id), decl);
  else
    push_binding (id, decl, b);

  /* Now restore the binding chain we'd temporarily removed.  */
  if (chainp)
    {
      *chainp = IDENTIFIER_BINDING (id);
      IDENTIFIER_BINDING (id) = binding;

      if (type)
	{
	  /* Insert the new type binding in the shadowing_type_p
	     TREE_VALUE chain.  */
	  tree shadowed_type = NULL_TREE;
	  if (shadowing_type_p)
	    {
	      shadowed_type = *shadowing_type_p;
	      *shadowing_type_p = type;
	    }

	  b->type_shadowed = tree_cons (id, shadowed_type, b->type_shadowed);
	  TREE_TYPE (b->type_shadowed) = type;
	}
    }
  else if (type)
    {
      /* Our new binding is the active one, so shadow the earlier
	 binding.  */
      b->type_shadowed = tree_cons (id, REAL_IDENTIFIER_TYPE_VALUE (id),
				    b->type_shadowed);
      TREE_TYPE (b->type_shadowed) = type;
      SET_IDENTIFIER_TYPE_VALUE (id, type);
    }

  /* Record that we have a binding for ID, like add_decl_to_level.  */
  tree node = build_tree_list (NULL_TREE, decl);
  TREE_CHAIN (node) = b->names;
  b->names = node;
}

static void
plugin_pragma_push_user_expression (cpp_reader *)
{
  if (push_count++)
    return;

  gcc_assert (!current_class_ptr);
  gcc_assert (!current_class_ref);

  gcc_assert (!cp_binding_oracle);
  cp_binding_oracle = plugin_binding_oracle;

  /* Make the function containing the user expression a global
     friend, so as to bypass access controls in it.  */
  if (at_function_scope_p ())
    set_global_friend (current_function_decl);

  gcc_assert (at_function_scope_p ());
  function *save_cfun = cfun;
  cp_binding_level *orig_binding_level = current_binding_level;
  {
    int success;
    cc1_plugin::call (current_context, "enter_scope", &success);
  }
  gcc_assert (at_fake_function_scope_p () || at_function_scope_p ());

  function *unchanged_cfun = cfun;
  tree changed_func_decl = current_function_decl;

  gcc_assert (current_class_type == DECL_CONTEXT (current_function_decl)
	      || !(RECORD_OR_UNION_CODE_P
		   (TREE_CODE (DECL_CONTEXT (current_function_decl)))));
  push_fake_function (save_cfun->decl, sk_block);
  current_class_type = NULL_TREE;
  if (unchanged_cfun)
    {
      /* If we get here, GDB did NOT change the context.  */
      gcc_assert (cfun == save_cfun);
      gcc_assert (at_function_scope_p ());
      gcc_assert (orig_binding_level
		  == current_binding_level->level_chain->level_chain);
    }
  else
    {
      cfun = save_cfun;
      gcc_assert (at_function_scope_p ());

      cp_binding_level *b = current_binding_level->level_chain;
      gcc_assert (b->this_entity == cfun->decl);

      /* Reactivate local names from the previous context.  Use
	 IDENTIFIER_MARKED to avoid reactivating shadowed names.  */
      for (cp_binding_level *level = orig_binding_level;;)
	{
	  for (tree name = level->names;
	       name; name = TREE_CHAIN (name))
	    {
	      tree decl = name;
	      if (TREE_CODE (decl) == TREE_LIST)
		decl = TREE_VALUE (decl);
	      if (IDENTIFIER_MARKED (DECL_NAME (decl)))
		continue;
	      IDENTIFIER_MARKED (DECL_NAME (decl)) = 1;
	      reactivate_decl (decl, b);
	    }
	  if (level->kind == sk_function_parms
	      && level->this_entity == cfun->decl)
	    break;
	  gcc_assert (!level->this_entity);
	  level = level->level_chain;
	}

      /* Now, clear the markers.  */
      for (tree name = b->names; name; name = TREE_CHAIN (name))
	{
	  tree decl = name;
	  if (TREE_CODE (decl) == TREE_LIST)
	    decl = TREE_VALUE (decl);
	  gcc_assert (IDENTIFIER_MARKED (DECL_NAME (decl)));
	  IDENTIFIER_MARKED (DECL_NAME (decl)) = 0;
	}
    }

  if (unchanged_cfun || DECL_NONSTATIC_MEMBER_FUNCTION_P (changed_func_decl))
    {
      /* Check whether the oracle supplies us with a "this", and if
	 so, arrange for data members and this itself to be
	 usable.  */
      tree this_val = lookup_name (get_identifier ("this"));
      current_class_ref = !this_val ? NULL_TREE
	: cp_build_indirect_ref (input_location, this_val, RO_NULL,
				 tf_warning_or_error);
      current_class_ptr = this_val;
    }
}

static void
plugin_pragma_pop_user_expression (cpp_reader *)
{
  if (--push_count)
    return;

  gcc_assert (cp_binding_oracle);

  gcc_assert (at_function_scope_p ());
  function *save_cfun = cfun;
  current_class_ptr = NULL_TREE;
  current_class_ref = NULL_TREE;

  cfun = NULL;
  pop_scope ();
  if (RECORD_OR_UNION_CODE_P (TREE_CODE (DECL_CONTEXT (current_function_decl))))
    current_class_type = DECL_CONTEXT (current_function_decl);
  {
    int success;
    cc1_plugin::call (current_context, "leave_scope", &success);
  }
  if (!cfun)
    cfun = save_cfun;
  else
    gcc_assert (cfun == save_cfun);

  cp_binding_oracle = NULL;
  gcc_assert (at_function_scope_p ());
}

static void
plugin_init_extra_pragmas (void *, void *)
{
  c_register_pragma ("GCC", "push_user_expression", plugin_pragma_push_user_expression);
  c_register_pragma ("GCC", "pop_user_expression", plugin_pragma_pop_user_expression);
  /* FIXME: this one should go once we get GDB to use push and pop.  */
  c_register_pragma ("GCC", "user_expression", plugin_pragma_push_user_expression);
}



static decl_addr_value
build_decl_addr_value (tree decl, gcc_address address)
{
  decl_addr_value value = {
    decl,
    build_int_cst_type (ptr_type_node, address)
  };
  return value;
}

static decl_addr_value *
record_decl_address (plugin_context *ctx, decl_addr_value value)
{
  decl_addr_value **slot = ctx->address_map.find_slot (&value, INSERT);
  gcc_assert (*slot == NULL);
  *slot
    = static_cast<decl_addr_value *> (xmalloc (sizeof (decl_addr_value)));
  **slot = value;
  /* We don't want GCC to warn about e.g. static functions
     without a code definition.  */
  TREE_NO_WARNING (value.decl) = 1;
  return *slot;
}

// Maybe rewrite a decl to its address.
static tree
address_rewriter (tree *in, int *walk_subtrees, void *arg)
{
  plugin_context *ctx = (plugin_context *) arg;

  if (!DECL_P (*in)
      || TREE_CODE (*in) == NAMESPACE_DECL
      || DECL_NAME (*in) == NULL_TREE)
    return NULL_TREE;

  decl_addr_value value;
  value.decl = *in;
  decl_addr_value *found_value = ctx->address_map.find (&value);
  if (found_value != NULL)
    ;
  else if (HAS_DECL_ASSEMBLER_NAME_P (*in))
    {
      gcc_address address;

      if (!cc1_plugin::call (ctx, "address_oracle", &address,
			     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (*in))))
	return NULL_TREE;
      if (address == 0)
	return NULL_TREE;

      // Insert the decl into the address map in case it is referenced
      // again.
      value = build_decl_addr_value (value.decl, address);
      found_value = record_decl_address (ctx, value);
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



static inline tree
safe_push_template_decl (tree decl)
{
  void (*save_oracle) (enum cp_oracle_request, tree identifier);

  save_oracle = cp_binding_oracle;
  cp_binding_oracle = NULL;

  tree ret = push_template_decl (decl);

  cp_binding_oracle = save_oracle;

  return ret;
}

static inline tree
safe_pushtag (tree name, tree type, tag_scope scope)
{
  void (*save_oracle) (enum cp_oracle_request, tree identifier);

  save_oracle = cp_binding_oracle;
  cp_binding_oracle = NULL;

  tree ret = pushtag (name, type, scope);

  cp_binding_oracle = save_oracle;

  return ret;
}

static inline tree
safe_pushdecl_maybe_friend (tree decl, bool is_friend)
{
  void (*save_oracle) (enum cp_oracle_request, tree identifier);

  save_oracle = cp_binding_oracle;
  cp_binding_oracle = NULL;

  tree ret = pushdecl (decl, is_friend);

  cp_binding_oracle = save_oracle;

  return ret;
}



int
plugin_push_namespace (cc1_plugin::connection *,
		       const char *name)
{
  if (name && !*name)
    push_to_top_level ();
  else
    push_namespace (name ? get_identifier (name) : NULL);

  return 1;
}

int
plugin_push_class (cc1_plugin::connection *,
		   gcc_type type_in)
{
  tree type = convert_in (type_in);
  gcc_assert (RECORD_OR_UNION_CODE_P (TREE_CODE (type)));
  gcc_assert (TYPE_CONTEXT (type) == FROB_CONTEXT (current_scope ()));

  pushclass (type);

  return 1;
}

int
plugin_push_function (cc1_plugin::connection *,
		      gcc_decl function_decl_in)
{
  tree fndecl = convert_in (function_decl_in);
  gcc_assert (TREE_CODE (fndecl) == FUNCTION_DECL);
  gcc_assert (DECL_CONTEXT (fndecl) == FROB_CONTEXT (current_scope ()));

  push_fake_function (fndecl);

  return 1;
}

int
plugin_pop_binding_level (cc1_plugin::connection *)
{
  pop_scope ();
  return 1;
}

int
plugin_reactivate_decl (cc1_plugin::connection *,
			gcc_decl decl_in,
			gcc_decl scope_in)
{
  tree decl = convert_in (decl_in);
  tree scope = convert_in (scope_in);
  gcc_assert (TREE_CODE (decl) == VAR_DECL
	      || TREE_CODE (decl) == FUNCTION_DECL
	      || TREE_CODE (decl) == TYPE_DECL);
  cp_binding_level *b;
  if (scope)
    {
      gcc_assert (TREE_CODE (scope) == FUNCTION_DECL);
      for (b = current_binding_level;
	   b->this_entity != scope;
	   b = b->level_chain)
	gcc_assert (b->this_entity != global_namespace);
    }
  else
    {
      gcc_assert (!at_class_scope_p ());
      b = current_binding_level;
    }

  reactivate_decl (decl, b);
  return 1;
}

static tree
get_current_scope ()
{
  tree decl;

  if (at_namespace_scope_p ())
    decl = current_namespace;
  else if (at_class_scope_p ())
    decl = TYPE_NAME (current_class_type);
  else if (at_fake_function_scope_p () || at_function_scope_p ())
    decl = current_function_decl;
  else
    gcc_unreachable ();

  return decl;
}

gcc_decl
plugin_get_current_binding_level_decl (cc1_plugin::connection *)
{
  tree decl = get_current_scope ();

  return convert_out (decl);
}

int
plugin_make_namespace_inline (cc1_plugin::connection *)
{
  tree inline_ns = current_namespace;

  gcc_assert (toplevel_bindings_p ());
  gcc_assert (inline_ns != global_namespace);

  tree parent_ns = CP_DECL_CONTEXT (inline_ns);

  if (DECL_NAMESPACE_INLINE_P (inline_ns))
    return 0;

  DECL_NAMESPACE_INLINE_P (inline_ns) = true;
  vec_safe_push (DECL_NAMESPACE_INLINEES (parent_ns), inline_ns);

  return 1;
}

int
plugin_add_using_namespace (cc1_plugin::connection *,
			    gcc_decl used_ns_in)
{
  tree used_ns = convert_in (used_ns_in);

  gcc_assert (TREE_CODE (used_ns) == NAMESPACE_DECL);

  finish_using_directive (used_ns, NULL_TREE);

  return 1;
}

int
plugin_add_namespace_alias (cc1_plugin::connection *,
			    const char *id,
			    gcc_decl target_in)
{
  tree name = get_identifier (id);
  tree target = convert_in (target_in);

  do_namespace_alias (name, target);

  return 1;
}

static inline void
set_access_flags (tree decl, enum gcc_cp_symbol_kind flags)
{
  gcc_assert (!(flags & GCC_CP_ACCESS_MASK) == !DECL_CLASS_SCOPE_P (decl));

  switch (flags & GCC_CP_ACCESS_MASK)
    {
    case GCC_CP_ACCESS_PRIVATE:
      TREE_PRIVATE (decl) = true;
      current_access_specifier = access_private_node;
      break;

    case GCC_CP_ACCESS_PROTECTED:
      TREE_PROTECTED (decl) = true;
      current_access_specifier = access_protected_node;
      break;

    case GCC_CP_ACCESS_PUBLIC:
      current_access_specifier = access_public_node;
      break;

    default:
      break;
    }
}

int
plugin_add_using_decl (cc1_plugin::connection *,
		       enum gcc_cp_symbol_kind flags,
		       gcc_decl target_in)
{
  tree target = convert_in (target_in);
  gcc_assert ((flags & GCC_CP_SYMBOL_MASK) == GCC_CP_SYMBOL_USING);
  gcc_assert (!(flags & GCC_CP_FLAG_MASK));
  enum gcc_cp_symbol_kind acc_flags;
  acc_flags = (enum gcc_cp_symbol_kind) (flags & GCC_CP_ACCESS_MASK);

  gcc_assert (!template_parm_scope_p ());

  bool class_member_p = at_class_scope_p ();
  gcc_assert (!(acc_flags & GCC_CP_ACCESS_MASK) == !class_member_p);

  tree identifier = DECL_NAME (target);
  tree tcontext = DECL_CONTEXT (target);

  if (UNSCOPED_ENUM_P (tcontext))
    tcontext = CP_TYPE_CONTEXT (tcontext);

  if (class_member_p)
    {
      tree decl = do_class_using_decl (tcontext, identifier);

      set_access_flags (decl, flags);

      finish_member_declaration (decl);
    }
  else
    {
      /* We can't be at local scope.  */
      gcc_assert (at_namespace_scope_p ());
      finish_nonmember_using_decl (tcontext, identifier);
    }

  return 1;
}

static tree
build_named_class_type (enum tree_code code,
			tree id,
			location_t loc)
{
  /* See at_fake_function_scope_p.  */
  gcc_assert (!at_function_scope_p ());
  tree type = make_class_type (code);
  tree type_decl = build_decl (loc, TYPE_DECL, id, type);
  TYPE_NAME (type) = type_decl;
  TYPE_STUB_DECL (type) = type_decl;
  DECL_CONTEXT (type_decl) = TYPE_CONTEXT (type);

  return type_decl;
}

/* Abuse an unused field of the dummy template parms entry to hold the
   parm list.  */
#define TP_PARM_LIST TREE_TYPE (current_template_parms)

gcc_decl
plugin_build_decl (cc1_plugin::connection *self,
		   const char *name,
		   enum gcc_cp_symbol_kind sym_kind,
		   gcc_type sym_type_in,
		   const char *substitution_name,
		   gcc_address address,
		   const char *filename,
		   unsigned int line_number)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  gcc_assert (!name || !strchr (name, ':')); // FIXME: this can go eventually.

  enum tree_code code;
  tree decl;
  tree sym_type = convert_in (sym_type_in);
  enum gcc_cp_symbol_kind sym_flags;
  sym_flags = (enum gcc_cp_symbol_kind) (sym_kind & GCC_CP_FLAG_MASK);
  enum gcc_cp_symbol_kind acc_flags;
  acc_flags = (enum gcc_cp_symbol_kind) (sym_kind & GCC_CP_ACCESS_MASK);
  sym_kind = (enum gcc_cp_symbol_kind) (sym_kind & GCC_CP_SYMBOL_MASK);

  switch (sym_kind)
    {
    case GCC_CP_SYMBOL_FUNCTION:
      code = FUNCTION_DECL;
      gcc_assert (!(sym_flags & ~GCC_CP_FLAG_MASK_FUNCTION));
      break;

    case GCC_CP_SYMBOL_VARIABLE:
      code = VAR_DECL;
      gcc_assert (!(sym_flags & ~GCC_CP_FLAG_MASK_VARIABLE));
      break;

    case GCC_CP_SYMBOL_TYPEDEF:
      code = TYPE_DECL;
      gcc_assert (!sym_flags);
      break;

    case GCC_CP_SYMBOL_CLASS:
      code = RECORD_TYPE;
      gcc_assert (!(sym_flags & ~GCC_CP_FLAG_MASK_CLASS));
      gcc_assert (!sym_type);
      break;

    case GCC_CP_SYMBOL_UNION:
      code = UNION_TYPE;
      gcc_assert (!sym_flags);
      gcc_assert (!sym_type);
      break;

    default:
      gcc_unreachable ();
    }

  bool template_decl_p = template_parm_scope_p ();

  if (template_decl_p)
    {
      gcc_assert (code == FUNCTION_DECL || code == RECORD_TYPE
		  || code == TYPE_DECL);

      /* Finish the template parm list that started this template parm.  */
      end_template_parm_list (TP_PARM_LIST);

      gcc_assert (!address);
      gcc_assert (!substitution_name);
    }

  location_t loc = ctx->get_location_t (filename, line_number);
  bool class_member_p = at_class_scope_p ();
  bool ctor = false, dtor = false, assop = false;
  tree_code opcode = ERROR_MARK;

  gcc_assert (!(acc_flags & GCC_CP_ACCESS_MASK) == !class_member_p);

  tree identifier;
  if (code != FUNCTION_DECL
      || !(sym_flags & GCC_CP_FLAG_SPECIAL_FUNCTION))
    {
      if (name)
	identifier = get_identifier (name);
      else
	{
	  gcc_assert (RECORD_OR_UNION_CODE_P (code));
	  identifier = make_anon_name ();
	}
    }

  if (code == FUNCTION_DECL)
    {
      if (sym_flags & GCC_CP_FLAG_SPECIAL_FUNCTION)
	{
#define CHARS2(f,s) (((unsigned char)f << CHAR_BIT) | (unsigned char)s)
	  switch (CHARS2 (name[0], name[1]))
	    {
	    case CHARS2 ('C', 0x0): // ctor base declaration
	    case CHARS2 ('C', ' '):
	    case CHARS2 ('C', '1'):
	    case CHARS2 ('C', '2'):
	    case CHARS2 ('C', '4'):
	      ctor = true;
	    cdtor:
	      gcc_assert (!address);
	      gcc_assert (!substitution_name);
	      identifier = DECL_NAME (TYPE_NAME (current_class_type));
	      break;
	    case CHARS2 ('D', 0x0): // dtor base declaration
	    case CHARS2 ('D', ' '):
	    case CHARS2 ('D', '0'):
	    case CHARS2 ('D', '1'):
	    case CHARS2 ('D', '2'):
	    case CHARS2 ('D', '4'):
	      gcc_assert (!template_decl_p);
	      dtor = true;
	      goto cdtor;
	    case CHARS2 ('n', 'w'): // operator new
	      opcode = NEW_EXPR;
	      break;
	    case CHARS2 ('n', 'a'): // operator new[]
	      opcode = VEC_NEW_EXPR;
	      break;
	    case CHARS2 ('d', 'l'): // operator delete
	      opcode = DELETE_EXPR;
	      break;
	    case CHARS2 ('d', 'a'): // operator delete[]
	      opcode = VEC_DELETE_EXPR;
	      break;
	    case CHARS2 ('p', 's'): // operator + (unary)
	      opcode = PLUS_EXPR;
	      break;
	    case CHARS2 ('n', 'g'): // operator - (unary)
	      opcode = MINUS_EXPR;
	      break;
	    case CHARS2 ('a', 'd'): // operator & (unary)
	      opcode = BIT_AND_EXPR;
	      break;
	    case CHARS2 ('d', 'e'): // operator * (unary)
	      opcode = MULT_EXPR;
	      break;
	    case CHARS2 ('c', 'o'): // operator ~
	      opcode = BIT_NOT_EXPR;
	      break;
	    case CHARS2 ('p', 'l'): // operator +
	      opcode = PLUS_EXPR;
	      break;
	    case CHARS2 ('m', 'i'): // operator -
	      opcode = MINUS_EXPR;
	      break;
	    case CHARS2 ('m', 'l'): // operator *
	      opcode = MULT_EXPR;
	      break;
	    case CHARS2 ('d', 'v'): // operator /
	      opcode = TRUNC_DIV_EXPR;
	      break;
	    case CHARS2 ('r', 'm'): // operator %
	      opcode = TRUNC_MOD_EXPR;
	      break;
	    case CHARS2 ('a', 'n'): // operator &
	      opcode = BIT_AND_EXPR;
	      break;
	    case CHARS2 ('o', 'r'): // operator |
	      opcode = BIT_IOR_EXPR;
	      break;
	    case CHARS2 ('e', 'o'): // operator ^
	      opcode = BIT_XOR_EXPR;
	      break;
	    case CHARS2 ('a', 'S'): // operator =
	      opcode = NOP_EXPR;
	      assop = true;
	      break;
	    case CHARS2 ('p', 'L'): // operator +=
	      opcode = PLUS_EXPR;
	      assop = true;
	      break;
	    case CHARS2 ('m', 'I'): // operator -=
	      opcode = MINUS_EXPR;
	      assop = true;
	      break;
	    case CHARS2 ('m', 'L'): // operator *=
	      opcode = MULT_EXPR;
	      assop = true;
	      break;
	    case CHARS2 ('d', 'V'): // operator /=
	      opcode = TRUNC_DIV_EXPR;
	      assop = true;
	      break;
	    case CHARS2 ('r', 'M'): // operator %=
	      opcode = TRUNC_MOD_EXPR;
	      assop = true;
	      break;
	    case CHARS2 ('a', 'N'): // operator &=
	      opcode = BIT_AND_EXPR;
	      assop = true;
	      break;
	    case CHARS2 ('o', 'R'): // operator |=
	      opcode = BIT_IOR_EXPR;
	      assop = true;
	      break;
	    case CHARS2 ('e', 'O'): // operator ^=
	      opcode = BIT_XOR_EXPR;
	      assop = true;
	      break;
	    case CHARS2 ('l', 's'): // operator <<
	      opcode = LSHIFT_EXPR;
	      break;
	    case CHARS2 ('r', 's'): // operator >>
	      opcode = RSHIFT_EXPR;
	      break;
	    case CHARS2 ('l', 'S'): // operator <<=
	      opcode = LSHIFT_EXPR;
	      assop = true;
	      break;
	    case CHARS2 ('r', 'S'): // operator >>=
	      opcode = RSHIFT_EXPR;
	      assop = true;
	      break;
	    case CHARS2 ('e', 'q'): // operator ==
	      opcode = EQ_EXPR;
	      break;
	    case CHARS2 ('n', 'e'): // operator !=
	      opcode = NE_EXPR;
	      break;
	    case CHARS2 ('l', 't'): // operator <
	      opcode = LT_EXPR;
	      break;
	    case CHARS2 ('g', 't'): // operator >
	      opcode = GT_EXPR;
	      break;
	    case CHARS2 ('l', 'e'): // operator <=
	      opcode = LE_EXPR;
	      break;
	    case CHARS2 ('g', 'e'): // operator >=
	      opcode = GE_EXPR;
	      break;
	    case CHARS2 ('n', 't'): // operator !
	      opcode = TRUTH_NOT_EXPR;
	      break;
	    case CHARS2 ('a', 'a'): // operator &&
	      opcode = TRUTH_ANDIF_EXPR;
	      break;
	    case CHARS2 ('o', 'o'): // operator ||
	      opcode = TRUTH_ORIF_EXPR;
	      break;
	    case CHARS2 ('p', 'p'): // operator ++
	      opcode = POSTINCREMENT_EXPR;
	      break;
	    case CHARS2 ('m', 'm'): // operator --
	      /* This stands for either one as an operator name, and
		 "pp" and "mm" stand for POST??CREMENT, but for some
		 reason the parser uses this opcode name for
		 operator--; let's follow their practice.  */
	      opcode = PREDECREMENT_EXPR;
	      break;
	    case CHARS2 ('c', 'm'): // operator ,
	      opcode = COMPOUND_EXPR;
	      break;
	    case CHARS2 ('p', 'm'): // operator ->*
	      opcode = MEMBER_REF;
	      break;
	    case CHARS2 ('p', 't'): // operator ->
	      opcode = COMPONENT_REF;
	      break;
	    case CHARS2 ('c', 'l'): // operator ()
	      opcode = CALL_EXPR;
	      break;
	    case CHARS2 ('i', 'x'): // operator []
	      opcode = ARRAY_REF;
	      break;
	    case CHARS2 ('c', 'v'): // operator <T> (conversion operator)
	      identifier = make_conv_op_name (TREE_TYPE (sym_type));
	      break;
	      // C++11-only:
	    case CHARS2 ('l', 'i'): // operator "" <id>
	      {
		char *id = (char *)name + 2;
		bool freeid = false;
		if (*id >= '0' && *id <= '9')
		  {
		    unsigned len = 0;
		    do
		      {
			len *= 10;
			len += id[0] - '0';
			id++;
		      }
		    while (*id && *id >= '0' && *id <= '9');
		    id = xstrndup (id, len);
		    freeid = true;
		  }
		identifier = cp_literal_operator_id (id);
		if (freeid)
		  free (id);
	      }
	      break;
	    case CHARS2 ('q', 'u'): // ternary operator, not overloadable.
	    default:
	      gcc_unreachable ();
	    }

	  if (opcode != ERROR_MARK)
	    identifier = ovl_op_identifier (assop, opcode);
	}
      decl = build_lang_decl_loc (loc, code, identifier, sym_type);
      /* FIXME: current_lang_name is lang_name_c while compiling an
	 extern "C" function, and we haven't switched to a global
	 context at this point, and this breaks function
	 overloading.  */
      SET_DECL_LANGUAGE (decl, lang_cplusplus);
      if (TREE_CODE (sym_type) == METHOD_TYPE)
	DECL_ARGUMENTS (decl) = build_this_parm (decl, current_class_type,
						 cp_type_quals (sym_type));
      for (tree arg = TREE_CODE (sym_type) == METHOD_TYPE
	     ? TREE_CHAIN (TYPE_ARG_TYPES (sym_type))
	     : TYPE_ARG_TYPES (sym_type);
	   arg && arg != void_list_node;
	   arg = TREE_CHAIN (arg))
	{
	  tree parm = cp_build_parm_decl (decl, NULL_TREE, TREE_VALUE (arg));
	  DECL_CHAIN (parm) = DECL_ARGUMENTS (decl);
	  DECL_ARGUMENTS (decl) = parm;
	}
      DECL_ARGUMENTS (decl) = nreverse (DECL_ARGUMENTS (decl));
      if (class_member_p)
	{
	  if (TREE_CODE (sym_type) == FUNCTION_TYPE)
	    DECL_STATIC_FUNCTION_P (decl) = 1;
	  if (sym_flags & GCC_CP_FLAG_VIRTUAL_FUNCTION)
	    {
	      DECL_VIRTUAL_P (decl) = 1;
	      if (sym_flags & GCC_CP_FLAG_PURE_VIRTUAL_FUNCTION)
		DECL_PURE_VIRTUAL_P (decl) = 1;
	      if (sym_flags & GCC_CP_FLAG_FINAL_VIRTUAL_FUNCTION)
		DECL_FINAL_P (decl) = 1;
	    }
	  else
	    gcc_assert (!(sym_flags & (GCC_CP_FLAG_PURE_VIRTUAL_FUNCTION
				       | GCC_CP_FLAG_FINAL_VIRTUAL_FUNCTION)));
	}
      else
	{
	  gcc_assert (!(sym_flags & (GCC_CP_FLAG_VIRTUAL_FUNCTION
				     | GCC_CP_FLAG_PURE_VIRTUAL_FUNCTION
				     | GCC_CP_FLAG_FINAL_VIRTUAL_FUNCTION)));
	  gcc_assert (!ctor && !dtor && !assop);
	}
      if (sym_flags & GCC_CP_FLAG_EXPLICIT_FUNCTION)
	DECL_NONCONVERTING_P (decl) = 1;
      if (sym_flags & GCC_CP_FLAG_DEFAULTED_FUNCTION)
	{
	  DECL_INITIAL (decl) = ridpointers[(int)RID_DEFAULT];
	  DECL_DEFAULTED_FN (decl) = 1;
	}
      if (sym_flags & GCC_CP_FLAG_DELETED_FUNCTION)
	{
	  // DECL_INITIAL (decl) = ridpointers[(int)RID_DELETE];
	  DECL_DELETED_FN (decl) = 1;
	  DECL_DECLARED_INLINE_P (decl) = 1;
	  DECL_INITIAL (decl) = error_mark_node;
	}

      if (ctor)
	DECL_CXX_CONSTRUCTOR_P (decl) = 1;
      else if (dtor)
	DECL_CXX_DESTRUCTOR_P (decl) = 1;
      else if ((sym_flags & GCC_CP_FLAG_SPECIAL_FUNCTION)
	       && opcode != ERROR_MARK)
	DECL_OVERLOADED_OPERATOR_CODE_RAW (decl) = ovl_op_mapping[opcode];
    }
  else if (RECORD_OR_UNION_CODE_P (code))
    {
      decl = build_named_class_type (code, identifier, loc);
      tree type = TREE_TYPE (decl);

      if (code == RECORD_TYPE
	  && !(sym_flags & GCC_CP_FLAG_CLASS_IS_STRUCT))
	CLASSTYPE_DECLARED_CLASS (type) = true;
    }
  else if (class_member_p)
    {
      decl = build_lang_decl_loc (loc, code, identifier, sym_type);

      if (TREE_CODE (decl) == VAR_DECL)
	{
	  DECL_THIS_STATIC (decl) = 1;
	  // The remainder of this block does the same as:
	  // set_linkage_for_static_data_member (decl);
	  TREE_PUBLIC (decl) = 1;
	  TREE_STATIC (decl) = 1;
	  DECL_INTERFACE_KNOWN (decl) = 1;

	  // FIXME: sym_flags & GCC_CP_FLAG_THREAD_LOCAL_VARIABLE
	  gcc_assert (!(sym_flags & GCC_CP_FLAG_THREAD_LOCAL_VARIABLE));

	  if (sym_flags & GCC_CP_FLAG_CONSTEXPR_VARIABLE)
	    DECL_DECLARED_CONSTEXPR_P (decl) = true;
	}
    }
  else
    {
      decl = build_decl (loc, code, identifier, sym_type);

      if (TREE_CODE (decl) == VAR_DECL)
	{
	  // FIXME: sym_flags & GCC_CP_FLAG_THREAD_LOCAL_VARIABLE
	  gcc_assert (!(sym_flags & GCC_CP_FLAG_THREAD_LOCAL_VARIABLE));

	  if (sym_flags & GCC_CP_FLAG_CONSTEXPR_VARIABLE)
	    DECL_DECLARED_CONSTEXPR_P (decl) = true;
	}
    }
  TREE_USED (decl) = 1;
  TREE_ADDRESSABLE (decl) = 1;

  if (class_member_p)
    DECL_CONTEXT (decl) = FROB_CONTEXT (current_class_type);
  else if (at_namespace_scope_p ())
    DECL_CONTEXT (decl) = FROB_CONTEXT (current_decl_namespace ());

  set_access_flags (decl, acc_flags);

  /* If this is the typedef that names an otherwise anonymous type,
     propagate the typedef name to the type.  In normal compilation,
     this is done in grokdeclarator.  */
  if (sym_kind == GCC_CP_SYMBOL_TYPEDEF
      && !template_decl_p
      && DECL_CONTEXT (decl) == TYPE_CONTEXT (sym_type)
      && TYPE_UNNAMED_P (sym_type))
    name_unnamed_type (sym_type, decl);

  if (sym_kind != GCC_CP_SYMBOL_TYPEDEF
      && sym_kind != GCC_CP_SYMBOL_CLASS
      && sym_kind != GCC_CP_SYMBOL_UNION
      && !template_decl_p && !ctor && !dtor)
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
      else if (address)
	value.address = build_int_cst_type (ptr_type_node, address);
      else
	value.address = NULL;
      if (value.address)
	record_decl_address (ctx, value);
    }

  if (class_member_p && code == FUNCTION_DECL)
    {
      if (ctor || dtor)
	maybe_retrofit_in_chrg (decl);

      grok_special_member_properties (decl);
    }

  if (template_decl_p)
    {
      if (RECORD_OR_UNION_CODE_P (code))
	safe_pushtag (identifier, TREE_TYPE (decl), ts_current);
      else
	decl = safe_push_template_decl (decl);

      tree tdecl = NULL_TREE;
      if (class_member_p)
	tdecl = finish_member_template_decl (decl);

      end_template_decl ();

      /* We only support one level of templates, because we only
	 support declaring generics; actual definitions are only of
	 specializations.  */
      gcc_assert (!template_parm_scope_p ());

      if (class_member_p)
	finish_member_declaration (tdecl);
    }
  else if (RECORD_OR_UNION_CODE_P (code))
    safe_pushtag (identifier, TREE_TYPE (decl), ts_current);
  else if (class_member_p)
    finish_member_declaration (decl);
  else
    decl = safe_pushdecl_maybe_friend (decl, false);

  if ((ctor || dtor)
      /* Don't crash after a duplicate declaration of a cdtor.  */
      && TYPE_FIELDS (current_class_type) == decl)
    {
      /* ctors and dtors clones are chained after DECL.
	 However, we create the clones before TYPE_METHODS is
	 reversed.  We test for cloned methods after reversal,
	 however, and the test requires the clones to follow
	 DECL.  So, we reverse the chain of clones now, so
	 that it will come out in the right order after
	 reversal.  */
      tree save = DECL_CHAIN (decl);
      DECL_CHAIN (decl) = NULL_TREE;
      clone_cdtor (decl, /*update_methods=*/true);
      gcc_assert (TYPE_FIELDS (current_class_type) == decl);
      TYPE_FIELDS (current_class_type)
	= nreverse (TYPE_FIELDS (current_class_type));
      DECL_CHAIN (decl) = save;
    }

  rest_of_decl_compilation (decl, toplevel_bindings_p (), 0);

  return convert_out (ctx->preserve (decl));
}

gcc_decl
plugin_define_cdtor_clone (cc1_plugin::connection *self,
			   const char *name,
			   gcc_decl cdtor_in,
			   gcc_address address)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree decl = convert_in (cdtor_in);
  bool ctor = false;
  bool dtor = false;
  tree identifier;

  switch (CHARS2 (name[0], name[1]))
    {
    case CHARS2 ('C', '1'): // in-charge constructor
      identifier = complete_ctor_identifier;
      ctor = true;
      break;
    case CHARS2 ('C', '2'): // not-in-charge constructor
      identifier = base_ctor_identifier;
      ctor = true;
      break;
    case CHARS2 ('C', '4'):
      identifier = ctor_identifier; // unified constructor
      ctor = true;
      break;
    case CHARS2 ('D', '0'): // deleting destructor
      identifier = deleting_dtor_identifier;
      dtor = true;
      break;
    case CHARS2 ('D', '1'): // in-charge destructor
      identifier = complete_dtor_identifier;
      dtor = true;
      break;
    case CHARS2 ('D', '2'): // not-in-charge destructor
      identifier = base_dtor_identifier;
      dtor = true;
      break;
    case CHARS2 ('D', '4'):
      identifier = dtor_identifier; // unified destructor
      dtor = true;
      break;

    default:
      gcc_unreachable ();
    }

  gcc_assert (!ctor != !dtor);
  gcc_assert (ctor
	      ? (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (decl)
		 && DECL_NAME (decl) == ctor_identifier)
	      : (DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (decl)
		 && DECL_NAME (decl) == dtor_identifier));

  while (decl && DECL_NAME (decl) != identifier)
    {
      decl = DECL_CHAIN (decl);
      if (decl && !DECL_CLONED_FUNCTION_P (decl))
	decl = NULL_TREE;
    }
  gcc_assert (decl);

  record_decl_address (ctx, build_decl_addr_value (decl, address));

  return convert_out (decl);
}

int
plugin_add_friend (cc1_plugin::connection * /* self */,
		   gcc_decl decl_in,
		   gcc_type type_in)
{
  tree decl = convert_in (decl_in);
  tree type = convert_in (type_in);

  gcc_assert (type || at_class_scope_p ());

  if (!type)
    type = current_class_type;
  else
    gcc_assert (TREE_CODE (type) == RECORD_TYPE);

  if (TYPE_P (decl))
    make_friend_class (type, TREE_TYPE (decl), true);
  else
    {
      DECL_FRIEND_P (decl) = true;
      add_friend (type, decl, true);
    }

  return 1;
}

gcc_type
plugin_build_pointer_type (cc1_plugin::connection *,
			   gcc_type base_type)
{
  // No need to preserve a pointer type as the base type is preserved.
  return convert_out (build_pointer_type (convert_in (base_type)));
}

gcc_type
plugin_build_reference_type (cc1_plugin::connection *,
			     gcc_type base_type_in,
			     enum gcc_cp_ref_qualifiers rquals)
{
  bool rval;

  switch (rquals)
    {
    case GCC_CP_REF_QUAL_LVALUE:
      rval = false;
      break;
    case GCC_CP_REF_QUAL_RVALUE:
      rval = true;
      break;
    case GCC_CP_REF_QUAL_NONE:
    default:
      gcc_unreachable ();
    }

  tree rtype = cp_build_reference_type (convert_in (base_type_in), rval);

  return convert_out (rtype);
}

static tree
start_class_def (tree type,
		 const gcc_vbase_array *base_classes)
{
  tree bases = NULL;
  if (base_classes)
    {
      for (int i = 0; i < base_classes->n_elements; i++)
	{
	  tree access;

	  gcc_assert ((base_classes->flags[i] & GCC_CP_SYMBOL_MASK)
		      == GCC_CP_SYMBOL_BASECLASS);

	  switch (base_classes->flags[i] & GCC_CP_ACCESS_MASK)
	    {
	    case GCC_CP_ACCESS_PRIVATE:
	      access = ridpointers[(int)RID_PRIVATE];
	      break;

	    case GCC_CP_ACCESS_PROTECTED:
	      access = ridpointers[(int)RID_PROTECTED];
	      break;

	    case GCC_CP_ACCESS_PUBLIC:
	      access = ridpointers[(int)RID_PUBLIC];
	      break;

	    default:
	      gcc_unreachable ();
	    }

	  tree base = finish_base_specifier
	    (convert_in (base_classes->elements[i]), access,
	     (base_classes->flags[i] & GCC_CP_FLAG_BASECLASS_VIRTUAL) != 0);
	  TREE_CHAIN (base) = bases;
	  bases = base;
	}
      bases = nreverse (bases);
    }
  xref_basetypes (type, bases);
  begin_class_definition (type);
  return type;
}

gcc_type
plugin_start_class_type (cc1_plugin::connection *self,
			 gcc_decl typedecl_in,
			 const gcc_vbase_array *base_classes,
			 const char *filename,
			 unsigned int line_number)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  location_t loc = ctx->get_location_t (filename, line_number);
  tree typedecl = convert_in (typedecl_in);
  tree type = TREE_TYPE (typedecl);

  gcc_assert (RECORD_OR_UNION_CODE_P (TREE_CODE (type)));
  gcc_assert (!COMPLETE_TYPE_P (type));

  DECL_SOURCE_LOCATION (typedecl) = loc;

  tree result = start_class_def (type, base_classes);

  return convert_out (ctx->preserve (result));
}

gcc_type
plugin_start_closure_class_type (cc1_plugin::connection *self,
				 int discriminator,
				 gcc_decl extra_scope_in,
				 enum gcc_cp_symbol_kind flags,
				 const char *filename,
				 unsigned int line_number)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree extra_scope = convert_in (extra_scope_in);

  gcc_assert ((flags & GCC_CP_SYMBOL_MASK) == GCC_CP_SYMBOL_LAMBDA_CLOSURE);
  gcc_assert ((flags & (~(GCC_CP_SYMBOL_MASK | GCC_CP_ACCESS_MASK))) == 0);

  gcc_assert (!(flags & GCC_CP_ACCESS_MASK) == !at_class_scope_p ());

  /* See at_fake_function_scope_p.  */
  gcc_assert (!at_function_scope_p ());

  if (extra_scope)
    {
      if (TREE_CODE (extra_scope) == PARM_DECL)
	{
	  gcc_assert (at_fake_function_scope_p ());
	  /* Check that the given extra_scope is one of the parameters of
	     the current function.  */
	  for (tree parm = DECL_ARGUMENTS (current_function_decl);
	       ; parm = DECL_CHAIN (parm))
	    {
	      gcc_assert (parm);
	      if (parm == extra_scope)
		break;
	    }
	}
      else if (TREE_CODE (extra_scope) == FIELD_DECL)
	{
	  gcc_assert (at_class_scope_p ());
	  gcc_assert (DECL_CONTEXT (extra_scope) == current_class_type);
	}
      else
	/* FIXME: does this ever really occur?  */
	gcc_assert (TREE_CODE (extra_scope) == VAR_DECL);
    }

  tree lambda_expr = build_lambda_expr ();

  LAMBDA_EXPR_LOCATION (lambda_expr) = ctx->get_location_t (filename,
							    line_number);

  tree type = begin_lambda_type (lambda_expr);

  /* Instead of calling record_lambda_scope, do this:  */
  LAMBDA_EXPR_EXTRA_SCOPE (lambda_expr) = extra_scope;
  LAMBDA_EXPR_DISCRIMINATOR (lambda_expr) = discriminator;

  tree decl = TYPE_NAME (type);
  determine_visibility (decl);
  set_access_flags (decl, flags);

  return convert_out (ctx->preserve (type));
}

gcc_expr
plugin_build_lambda_expr (cc1_plugin::connection *self,
			  gcc_type closure_type_in)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree closure_type = convert_in (closure_type_in);

  gcc_assert (LAMBDA_TYPE_P (closure_type));

  tree lambda_expr = CLASSTYPE_LAMBDA_EXPR (closure_type);

  tree lambda_object = build_lambda_object (lambda_expr);

  return convert_out (ctx->preserve (lambda_object));
}

gcc_decl
plugin_build_field (cc1_plugin::connection *,
		    const char *field_name,
		    gcc_type field_type_in,
		    enum gcc_cp_symbol_kind flags,
		    unsigned long bitsize,
		    unsigned long bitpos)
{
  tree record_or_union_type = current_class_type;
  tree field_type = convert_in (field_type_in);

  gcc_assert (at_class_scope_p ());
  gcc_assert (RECORD_OR_UNION_CODE_P (TREE_CODE (record_or_union_type)));
  gcc_assert ((flags & GCC_CP_SYMBOL_MASK) == GCC_CP_SYMBOL_FIELD);
  gcc_assert ((flags & (~(GCC_CP_SYMBOL_MASK | GCC_CP_ACCESS_MASK
			  | GCC_CP_FLAG_MASK_FIELD))) == 0);
  gcc_assert ((flags & GCC_CP_ACCESS_MASK));

  /* Note that gdb does not preserve the location of field decls, so
     we can't provide a decent location here.  */
  tree decl = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			  get_identifier (field_name), field_type);
  DECL_FIELD_CONTEXT (decl) = record_or_union_type;

  set_access_flags (decl, flags);

  if ((flags & GCC_CP_FLAG_FIELD_MUTABLE) != 0)
    DECL_MUTABLE_P (decl) = 1;

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

  return convert_out (decl);
}

int
plugin_finish_class_type (cc1_plugin::connection *,
			  unsigned long size_in_bytes)
{
  tree record_or_union_type = current_class_type;

  gcc_assert (RECORD_OR_UNION_CODE_P (TREE_CODE (record_or_union_type)));

  finish_struct (record_or_union_type, NULL);

  gcc_assert (compare_tree_int (TYPE_SIZE_UNIT (record_or_union_type),
				size_in_bytes) == 0);

  return 1;
}

gcc_type
plugin_start_enum_type (cc1_plugin::connection *self,
			const char *name,
			gcc_type underlying_int_type_in,
			enum gcc_cp_symbol_kind flags,
			const char *filename,
			unsigned int line_number)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree underlying_int_type = convert_in (underlying_int_type_in);

  gcc_assert ((flags & GCC_CP_SYMBOL_MASK) == GCC_CP_SYMBOL_ENUM);
  gcc_assert ((flags & (~(GCC_CP_SYMBOL_MASK | GCC_CP_ACCESS_MASK
			  | GCC_CP_FLAG_MASK_ENUM))) == 0);
  gcc_assert (!(flags & GCC_CP_ACCESS_MASK) == !at_class_scope_p ());

  if (underlying_int_type == error_mark_node)
    return convert_out (error_mark_node);

  bool is_new_type = false;

  tree id = name ? get_identifier (name) : make_anon_name ();

  tree type = start_enum (id, NULL_TREE,
			  underlying_int_type,
			  /* attributes = */ NULL_TREE,
			  !!(flags & GCC_CP_FLAG_ENUM_SCOPED), &is_new_type);

  gcc_assert (is_new_type);

  location_t loc = ctx->get_location_t (filename, line_number);
  tree type_decl = TYPE_NAME (type);
  DECL_SOURCE_LOCATION (type_decl) = loc;
  SET_OPAQUE_ENUM_P (type, false);

  set_access_flags (type_decl, flags);

  return convert_out (ctx->preserve (type));
}

gcc_decl
plugin_build_enum_constant (cc1_plugin::connection *,
			    gcc_type enum_type_in,
			    const char *name,
			    unsigned long value)
{
  tree enum_type = convert_in (enum_type_in);

  gcc_assert (TREE_CODE (enum_type) == ENUMERAL_TYPE);

  build_enumerator (get_identifier (name), build_int_cst (enum_type, value),
		    enum_type, NULL_TREE, BUILTINS_LOCATION);

  return convert_out (TREE_VALUE (TYPE_VALUES (enum_type)));
}

int
plugin_finish_enum_type (cc1_plugin::connection *,
			 gcc_type enum_type_in)
{
  tree enum_type = convert_in (enum_type_in);

  finish_enum_value_list (enum_type);
  finish_enum (enum_type);

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

#if 0

gcc_type
plugin_add_function_default_args (cc1_plugin::connection *self,
				  gcc_type function_type_in,
				  const struct gcc_cp_function_args *defaults)
{
  tree function_type = convert_in (function_type_in);

  gcc_assert (TREE_CODE (function_type) == FUNCTION_TYPE);

  if (!defaults || !defaults->n_elements)
    return function_type_in;

  tree pargs = TYPE_ARG_TYPES (function_type);
  tree nargs = NULL_TREE;

  /* Build a reversed copy of the list of default-less arguments in
     NARGS.  At the end of the loop, PARGS will point to the end of
     the argument list, or to the first argument that had a default
     value.  */
  while (pargs && TREE_VALUE (pargs) != void_list_node
	 && !TREE_PURPOSE (pargs))
    {
      nargs = tree_cons (NULL_TREE, TREE_VALUE (pargs), nargs);
      pargs = TREE_CHAIN (pargs);
    }

  /* Set the defaults in the now-leading NARGS, taking into account
     that NARGS is reversed but DEFAULTS->elements isn't.  */
  tree ndargs = nargs;
  int i = defaults->n_elements;
  while (i--)
    {
      gcc_assert (ndargs);
      tree deflt = convert_in (defaults->elements[i]);
      if (!deflt)
	deflt = error_mark_node;
      TREE_PURPOSE (ndargs) = deflt;
      ndargs = TREE_CHAIN (ndargs);
    }

  /* Finally, reverse NARGS, and append the remaining PARGS that
     already had defaults.  */
  nargs = nreverse (nargs);
  nargs = chainon (nargs, pargs);

  tree result = build_function_type (TREE_TYPE (function_type), nargs);

  /* Copy exceptions, attributes and whatnot.  */
  result = build_exception_variant (result,
				    TYPE_RAISES_EXCEPTIONS (function_type));
  result = cp_build_type_attribute_variant (result,
					    TYPE_ATTRIBUTES (function_type));

  plugin_context *ctx = static_cast<plugin_context *> (self);
  return convert_out (ctx->preserve (result));
}

int
plugin_set_deferred_function_default_args (cc1_plugin::connection *,
					   gcc_decl function_in,
					   const struct gcc_cp_function_args
					   *defaults)
{
  tree function = convert_in (function_in);

  gcc_assert (TREE_CODE (function) == FUNCTION_DECL);

  if (!defaults || !defaults->n_elements)
    return 1;

  tree arg = FUNCTION_FIRST_USER_PARMTYPE (function);

  for (int i = 0; i < defaults->n_elements; i++)
    {
      while (arg && TREE_PURPOSE (arg) != error_mark_node)
	arg = TREE_CHAIN (arg);

      if (!arg)
	return 0;

      TREE_PURPOSE (arg) = convert_in (defaults->elements[i]);
      arg = TREE_CHAIN (arg);
    }

  return 1;
}

#endif

gcc_decl
plugin_get_function_parameter_decl (cc1_plugin::connection *,
				    gcc_decl function_in,
				    int index)
{
  tree function = convert_in (function_in);

  gcc_assert (TREE_CODE (function) == FUNCTION_DECL);

  if (index == -1)
    {
      gcc_assert (TREE_CODE (TREE_TYPE (function)) == METHOD_TYPE);

      return convert_out (DECL_ARGUMENTS (function));
    }

  gcc_assert (index >= 0);

  tree args = FUNCTION_FIRST_USER_PARM (function);

  for (int i = 0; args && i < index; i++)
    args = DECL_CHAIN (args);

  return convert_out (args);
}

gcc_type
plugin_build_exception_spec_variant (cc1_plugin::connection *self,
				     gcc_type function_type_in,
				     const struct gcc_type_array *except_types_in)
{
  tree function_type = convert_in (function_type_in);
  tree except_types = NULL_TREE;

  if (!except_types_in)
    except_types = noexcept_false_spec;
  else if (!except_types_in->n_elements)
    except_types = empty_except_spec;
  else
    for (int i = 0; i < except_types_in->n_elements; i++)
      except_types = add_exception_specifier (except_types,
					      convert_in
					      (except_types_in->elements[i]),
					      0);

  function_type = build_exception_variant (function_type,
					   except_types);

  plugin_context *ctx = static_cast<plugin_context *> (self);
  return convert_out (ctx->preserve (function_type));
}

gcc_type
plugin_build_method_type (cc1_plugin::connection *self,
			  gcc_type class_type_in,
			  gcc_type func_type_in,
			  enum gcc_cp_qualifiers quals_in,
			  enum gcc_cp_ref_qualifiers rquals_in)
{
  tree class_type = convert_in (class_type_in);
  tree func_type = convert_in (func_type_in);
  cp_cv_quals quals = 0;
  cp_ref_qualifier rquals;

  if ((quals_in & GCC_CP_QUALIFIER_CONST) != 0)
    quals |= TYPE_QUAL_CONST;
  if ((quals_in & GCC_CP_QUALIFIER_VOLATILE) != 0)
    quals |= TYPE_QUAL_VOLATILE;
  gcc_assert ((quals_in & GCC_CP_QUALIFIER_RESTRICT) == 0);

  switch (rquals_in)
    {
    case GCC_CP_REF_QUAL_NONE:
      rquals = REF_QUAL_NONE;
      break;
    case GCC_CP_REF_QUAL_LVALUE:
      rquals = REF_QUAL_LVALUE;
      break;
    case GCC_CP_REF_QUAL_RVALUE:
      rquals = REF_QUAL_RVALUE;
      break;
    default:
      gcc_unreachable ();
    }

  tree method_type = class_type
    ? build_memfn_type (func_type, class_type, quals, rquals)
    : apply_memfn_quals (func_type, quals, rquals);

  plugin_context *ctx = static_cast<plugin_context *> (self);
  return convert_out (ctx->preserve (method_type));
}

gcc_type
plugin_build_pointer_to_member_type (cc1_plugin::connection *self,
				     gcc_type class_type_in,
				     gcc_type member_type_in)
{
  tree class_type = convert_in (class_type_in);
  tree member_type = convert_in (member_type_in);

  tree memptr_type = build_ptrmem_type (class_type, member_type);

  plugin_context *ctx = static_cast<plugin_context *> (self);
  return convert_out (ctx->preserve (memptr_type));
}

int
plugin_start_template_decl (cc1_plugin::connection *)
{
  begin_template_parm_list ();

  TP_PARM_LIST = NULL_TREE;

  return 1;
}

gcc_decl
plugin_get_type_decl (cc1_plugin::connection *,
		      gcc_type type_in)
{
  tree type = convert_in (type_in);

  tree name = TYPE_NAME (type);
  gcc_assert (name);

  return convert_out (name);
}

gcc_type
plugin_get_decl_type (cc1_plugin::connection *,
		      gcc_decl decl_in)
{
  tree decl = convert_in (decl_in);

  tree type = TREE_TYPE (decl);
  gcc_assert (type);

  return convert_out (type);
}

gcc_type
plugin_build_type_template_parameter (cc1_plugin::connection *self,
				      const char *id,
				      int /* bool */ pack_p,
				      gcc_type default_type,
				      const char *filename,
				      unsigned int line_number)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  location_t loc = ctx->get_location_t (filename, line_number);

  gcc_assert (template_parm_scope_p ());

  tree parm = finish_template_type_parm (class_type_node, get_identifier (id));
  parm = build_tree_list (convert_in (default_type), parm);

  gcc_assert (!(pack_p && default_type));

  /* Create a type and a decl for the type parm, and add the decl to
     TP_PARM_LIST.  */
  TP_PARM_LIST = process_template_parm (TP_PARM_LIST, loc, parm,
					/* is_non_type = */ false, pack_p);

  /* Locate the decl of the newly-added, processed template parm.  */
  parm = TREE_VALUE (tree_last (TP_PARM_LIST));

  /* Return its type.  */
  return convert_out (ctx->preserve (TREE_TYPE (parm)));
}

gcc_utempl
plugin_build_template_template_parameter (cc1_plugin::connection *self,
					  const char *id,
					  int /* bool */ pack_p,
					  gcc_utempl default_templ,
					  const char *filename,
					  unsigned int line_number)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  location_t loc = ctx->get_location_t (filename, line_number);

  gcc_assert (template_parm_scope_p ());

  /* Finish the template parm list that started this template parm.  */
  end_template_parm_list (TP_PARM_LIST);

  gcc_assert (template_parm_scope_p ());

  tree parm = finish_template_template_parm (class_type_node,
					     get_identifier (id));
  parm = build_tree_list (convert_in (default_templ), parm);

  gcc_assert (!(pack_p && default_templ));

  /* Create a type and a decl for the template parm, and add the decl
     to TP_PARM_LIST.  */
  TP_PARM_LIST = process_template_parm (TP_PARM_LIST, loc, parm,
					/* is_non_type = */ false, pack_p);

  /* Locate the decl of the newly-added, processed template parm.  */
  parm = TREE_VALUE (tree_last (TP_PARM_LIST));

  return convert_out (ctx->preserve (parm));
}

gcc_decl
plugin_build_value_template_parameter (cc1_plugin::connection *self,
				       gcc_type type,
				       const char *id,
				       gcc_expr default_value,
				       const char *filename,
				       unsigned int line_number)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  location_t loc = ctx->get_location_t (filename, line_number);

  gcc_assert (template_parm_scope_p ());

  cp_declarator declarator;
  memset (&declarator, 0, sizeof (declarator));
  // &declarator = make_id_declarator (NULL, get_identifier (id), sfk_none):
  declarator.kind = cdk_id;
  declarator.u.id.qualifying_scope = NULL;
  declarator.u.id.unqualified_name = get_identifier (id);
  declarator.u.id.sfk = sfk_none;

  cp_decl_specifier_seq declspec;
  memset (&declspec, 0, sizeof (declspec));
  // cp_parser_set_decl_spec_type (&declspec, convert_in (type), -token-, false):
  declspec.any_specifiers_p = declspec.any_type_specifiers_p = true;
  declspec.type = convert_in (type);
  declspec.locations[ds_type_spec] = loc;

  tree parm = grokdeclarator (&declarator, &declspec, TPARM, 0, 0);
  parm = build_tree_list (convert_in (default_value), parm);

  /* Create a type and a decl for the template parm, and add the decl
     to TP_PARM_LIST.  */
  TP_PARM_LIST = process_template_parm (TP_PARM_LIST, loc, parm,
					/* is_non_type = */ true, false);

  /* Locate the decl of the newly-added, processed template parm.  */
  parm = TREE_VALUE (tree_last (TP_PARM_LIST));

  return convert_out (ctx->preserve (parm));
}

static tree
targlist (const gcc_cp_template_args *targs)
{
  int n = targs->n_elements;
  tree vec = make_tree_vec (n);
  while (n--)
    {
      switch (targs->kinds[n])
	{
	case GCC_CP_TPARG_VALUE:
	  TREE_VEC_ELT (vec, n) = convert_in (targs->elements[n].value);
	  break;
	case GCC_CP_TPARG_CLASS:
	  TREE_VEC_ELT (vec, n) = convert_in (targs->elements[n].type);
	  break;
	case GCC_CP_TPARG_TEMPL:
	  TREE_VEC_ELT (vec, n) = convert_in (targs->elements[n].templ);
	  break;
	case GCC_CP_TPARG_PACK:
	  TREE_VEC_ELT (vec, n) = convert_in (targs->elements[n].pack);
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  return vec;
}

gcc_type
plugin_build_dependent_typename (cc1_plugin::connection *self,
				 gcc_type enclosing_type,
				 const char *id,
				 const gcc_cp_template_args *targs)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree type = convert_in (enclosing_type);
  tree name = get_identifier (id);
  if (targs)
    name = build_min_nt_loc (/*loc=*/0, TEMPLATE_ID_EXPR,
			     name, targlist (targs));
  tree res = make_typename_type (type, name, typename_type,
				 /*complain=*/tf_error);
  return convert_out (ctx->preserve (res));
}

gcc_utempl
plugin_build_dependent_class_template (cc1_plugin::connection *self,
				       gcc_type enclosing_type,
				       const char *id)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree type = convert_in (enclosing_type);
  tree name = get_identifier (id);
  tree res = make_unbound_class_template (type, name, NULL_TREE,
					  /*complain=*/tf_error);
  return convert_out (ctx->preserve (res));
}

gcc_type
plugin_build_dependent_type_template_id (cc1_plugin::connection *self,
					 gcc_utempl template_decl,
					 const gcc_cp_template_args *targs)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree type = convert_in (template_decl);
  tree decl = finish_template_type (type, targlist (targs),
				    /*entering_scope=*/false);
  return convert_out (ctx->preserve (TREE_TYPE (decl)));
}

gcc_expr
plugin_build_dependent_expr (cc1_plugin::connection *self,
			     gcc_decl enclosing_scope,
			     enum gcc_cp_symbol_kind flags,
			     const char *name,
			     gcc_type conv_type_in,
			     const gcc_cp_template_args *targs)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree scope = convert_in (enclosing_scope);
  tree conv_type = convert_in (conv_type_in);
  tree identifier;

  if (TREE_CODE (scope) != NAMESPACE_DECL)
    {
      tree type = TREE_TYPE (scope);
      gcc_assert (TYPE_NAME (type) == scope);
      scope = type;
    }

  if (flags == (GCC_CP_SYMBOL_FUNCTION | GCC_CP_FLAG_SPECIAL_FUNCTION))
    {
      bool assop = false, convop = false;
      tree_code opcode = ERROR_MARK;

      switch (CHARS2 (name[0], name[1]))
	{
	case CHARS2 ('C', 0x0): // ctor base declaration
	case CHARS2 ('C', ' '):
	case CHARS2 ('C', '1'):
	case CHARS2 ('C', '2'):
	case CHARS2 ('C', '4'):
	  identifier = ctor_identifier;
	  break;
	case CHARS2 ('D', 0x0): // dtor base declaration
	case CHARS2 ('D', ' '):
	case CHARS2 ('D', '0'):
	case CHARS2 ('D', '1'):
	case CHARS2 ('D', '2'):
	case CHARS2 ('D', '4'):
	  gcc_assert (!targs);
	  identifier = dtor_identifier;
	  break;
	case CHARS2 ('n', 'w'): // operator new
	  opcode = NEW_EXPR;
	  break;
	case CHARS2 ('n', 'a'): // operator new[]
	  opcode = VEC_NEW_EXPR;
	  break;
	case CHARS2 ('d', 'l'): // operator delete
	  opcode = DELETE_EXPR;
	  break;
	case CHARS2 ('d', 'a'): // operator delete[]
	  opcode = VEC_DELETE_EXPR;
	  break;
	case CHARS2 ('p', 's'): // operator + (unary)
	  opcode = PLUS_EXPR;
	  break;
	case CHARS2 ('n', 'g'): // operator - (unary)
	  opcode = MINUS_EXPR;
	  break;
	case CHARS2 ('a', 'd'): // operator & (unary)
	  opcode = BIT_AND_EXPR;
	  break;
	case CHARS2 ('d', 'e'): // operator * (unary)
	  opcode = MULT_EXPR;
	  break;
	case CHARS2 ('c', 'o'): // operator ~
	  opcode = BIT_NOT_EXPR;
	  break;
	case CHARS2 ('p', 'l'): // operator +
	  opcode = PLUS_EXPR;
	  break;
	case CHARS2 ('m', 'i'): // operator -
	  opcode = MINUS_EXPR;
	  break;
	case CHARS2 ('m', 'l'): // operator *
	  opcode = MULT_EXPR;
	  break;
	case CHARS2 ('d', 'v'): // operator /
	  opcode = TRUNC_DIV_EXPR;
	  break;
	case CHARS2 ('r', 'm'): // operator %
	  opcode = TRUNC_MOD_EXPR;
	  break;
	case CHARS2 ('a', 'n'): // operator &
	  opcode = BIT_AND_EXPR;
	  break;
	case CHARS2 ('o', 'r'): // operator |
	  opcode = BIT_IOR_EXPR;
	  break;
	case CHARS2 ('e', 'o'): // operator ^
	  opcode = BIT_XOR_EXPR;
	  break;
	case CHARS2 ('a', 'S'): // operator =
	  opcode = NOP_EXPR;
	  assop = true;
	  break;
	case CHARS2 ('p', 'L'): // operator +=
	  opcode = PLUS_EXPR;
	  assop = true;
	  break;
	case CHARS2 ('m', 'I'): // operator -=
	  opcode = MINUS_EXPR;
	  assop = true;
	  break;
	case CHARS2 ('m', 'L'): // operator *=
	  opcode = MULT_EXPR;
	  assop = true;
	  break;
	case CHARS2 ('d', 'V'): // operator /=
	  opcode = TRUNC_DIV_EXPR;
	  assop = true;
	  break;
	case CHARS2 ('r', 'M'): // operator %=
	  opcode = TRUNC_MOD_EXPR;
	  assop = true;
	  break;
	case CHARS2 ('a', 'N'): // operator &=
	  opcode = BIT_AND_EXPR;
	  assop = true;
	  break;
	case CHARS2 ('o', 'R'): // operator |=
	  opcode = BIT_IOR_EXPR;
	  assop = true;
	  break;
	case CHARS2 ('e', 'O'): // operator ^=
	  opcode = BIT_XOR_EXPR;
	  assop = true;
	  break;
	case CHARS2 ('l', 's'): // operator <<
	  opcode = LSHIFT_EXPR;
	  break;
	case CHARS2 ('r', 's'): // operator >>
	  opcode = RSHIFT_EXPR;
	  break;
	case CHARS2 ('l', 'S'): // operator <<=
	  opcode = LSHIFT_EXPR;
	  assop = true;
	  break;
	case CHARS2 ('r', 'S'): // operator >>=
	  opcode = RSHIFT_EXPR;
	  assop = true;
	  break;
	case CHARS2 ('e', 'q'): // operator ==
	  opcode = EQ_EXPR;
	  break;
	case CHARS2 ('n', 'e'): // operator !=
	  opcode = NE_EXPR;
	  break;
	case CHARS2 ('l', 't'): // operator <
	  opcode = LT_EXPR;
	  break;
	case CHARS2 ('g', 't'): // operator >
	  opcode = GT_EXPR;
	  break;
	case CHARS2 ('l', 'e'): // operator <=
	  opcode = LE_EXPR;
	  break;
	case CHARS2 ('g', 'e'): // operator >=
	  opcode = GE_EXPR;
	  break;
	case CHARS2 ('n', 't'): // operator !
	  opcode = TRUTH_NOT_EXPR;
	  break;
	case CHARS2 ('a', 'a'): // operator &&
	  opcode = TRUTH_ANDIF_EXPR;
	  break;
	case CHARS2 ('o', 'o'): // operator ||
	  opcode = TRUTH_ORIF_EXPR;
	  break;
	case CHARS2 ('p', 'p'): // operator ++
	  opcode = POSTINCREMENT_EXPR;
	  break;
	case CHARS2 ('m', 'm'): // operator --
	  opcode = PREDECREMENT_EXPR;
	  break;
	case CHARS2 ('c', 'm'): // operator ,
	  opcode = COMPOUND_EXPR;
	  break;
	case CHARS2 ('p', 'm'): // operator ->*
	  opcode = MEMBER_REF;
	  break;
	case CHARS2 ('p', 't'): // operator ->
	  opcode = COMPONENT_REF;
	  break;
	case CHARS2 ('c', 'l'): // operator ()
	  opcode = CALL_EXPR;
	  break;
	case CHARS2 ('i', 'x'): // operator []
	  opcode = ARRAY_REF;
	  break;
	case CHARS2 ('c', 'v'): // operator <T> (conversion operator)
	  convop = true;
	  identifier = make_conv_op_name (conv_type);
	  break;
	  // C++11-only:
	case CHARS2 ('l', 'i'): // operator "" <id>
	  {
	    char *id = (char *)name + 2;
	    bool freeid = false;
	    if (*id >= '0' && *id <= '9')
	      {
		unsigned len = 0;
		do
		  {
		    len *= 10;
		    len += id[0] - '0';
		    id++;
		  }
		while (*id && *id >= '0' && *id <= '9');
		id = xstrndup (id, len);
		freeid = true;
	      }
	    identifier = cp_literal_operator_id (id);
	    if (freeid)
	      free (id);
	  }
	  break;
	case CHARS2 ('q', 'u'): // ternary operator, not overloadable.
	default:
	  gcc_unreachable ();
	}

      gcc_assert (convop || !conv_type);

      if (opcode != ERROR_MARK)
	identifier = ovl_op_identifier (assop, opcode);

      gcc_assert (identifier);
    }
  else
    {
      gcc_assert (flags == GCC_CP_SYMBOL_MASK);
      gcc_assert (!conv_type);
      identifier = get_identifier (name);
    }
  tree res = identifier;
  if (!scope)
    res = lookup_name_real (res, 0, 0, true, 0, 0);
  else if (!TYPE_P (scope) || !dependent_scope_p (scope))
    {
      res = lookup_qualified_name (scope, res, false, true);
      /* We've already resolved the name in the scope, so skip the
	 build_qualified_name call below.  */
      scope = NULL;
    }
  if (targs)
    res = lookup_template_function (res, targlist (targs));
  if (scope)
    res = build_qualified_name (NULL_TREE, scope, res, !!targs);
  return convert_out (ctx->preserve (res));
}

gcc_expr
plugin_build_literal_expr (cc1_plugin::connection *self,
			   gcc_type type, unsigned long value)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree t = convert_in (type);
  tree val = build_int_cst_type (t, (unsigned HOST_WIDE_INT) value);
  return convert_out (ctx->preserve (val));
}

gcc_expr
plugin_build_decl_expr (cc1_plugin::connection *self,
			gcc_decl decl_in,
			int qualified_p)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree decl = convert_in (decl_in);
  gcc_assert (DECL_P (decl));
  tree result = decl;
  if (qualified_p)
    {
      gcc_assert (DECL_CLASS_SCOPE_P (decl));
      result = build_offset_ref (DECL_CONTEXT (decl), decl,
				 /*address_p=*/true, tf_error);
    }
  return convert_out (ctx->preserve (result));
}

gcc_expr
plugin_build_unary_expr (cc1_plugin::connection *self,
			 const char *unary_op,
			 gcc_expr operand)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree op0 = convert_in (operand);
  tree_code opcode = ERROR_MARK;
  bool global_scope_p = false;

 once_more:
  switch (CHARS2 (unary_op[0], unary_op[1]))
    {
    case CHARS2 ('p', 's'): // operator + (unary)
      opcode = UNARY_PLUS_EXPR;
      break;
    case CHARS2 ('n', 'g'): // operator - (unary)
      opcode = NEGATE_EXPR;
      break;
    case CHARS2 ('a', 'd'): // operator & (unary)
      opcode = ADDR_EXPR;
      break;
    case CHARS2 ('d', 'e'): // operator * (unary)
      opcode = INDIRECT_REF;
      break;
    case CHARS2 ('c', 'o'): // operator ~
      opcode = BIT_NOT_EXPR;
      break;
    case CHARS2 ('n', 't'): // operator !
      opcode = TRUTH_NOT_EXPR;
      break;
    case CHARS2 ('p', 'p'): // operator ++
      opcode = unary_op[2] == '_' ? PREINCREMENT_EXPR : POSTINCREMENT_EXPR;
      break;
    case CHARS2 ('m', 'm'): // operator --
      opcode = unary_op[2] == '_' ? PREDECREMENT_EXPR : POSTDECREMENT_EXPR;
      break;
    case CHARS2 ('n', 'x'): // noexcept
      opcode = NOEXCEPT_EXPR;
      break;
    case CHARS2 ('t', 'w'): // throw
      gcc_assert (op0);
      opcode = THROW_EXPR;
      break;
    case CHARS2 ('t', 'r'): // rethrow
      gcc_assert (!op0);
      opcode = THROW_EXPR;
      break;
    case CHARS2 ('t', 'e'): // typeid (value)
      opcode = TYPEID_EXPR;
      break;
    case CHARS2 ('s', 'z'): // sizeof (value)
      opcode = SIZEOF_EXPR;
      break;
    case CHARS2 ('a', 'z'): // alignof (value)
      opcode = ALIGNOF_EXPR;
      break;
    case CHARS2 ('g', 's'): // global scope (for delete, delete[])
      gcc_assert (!global_scope_p);
      global_scope_p = true;
      unary_op += 2;
      goto once_more;
    case CHARS2 ('d', 'l'): // delete
      opcode = DELETE_EXPR;
      break;
    case CHARS2 ('d', 'a'): // delete[]
      opcode = VEC_DELETE_EXPR;
      break;
    case CHARS2 ('s', 'p'): // pack...
      opcode = EXPR_PACK_EXPANSION;
      break;
    case CHARS2 ('s', 'Z'): // sizeof...(pack)
      opcode = TYPE_PACK_EXPANSION; // Not really, but let's use its code.
      break;

      /* FIXME: __real__, __imag__?  */

    default:
      gcc_unreachable ();
    }

  gcc_assert (!global_scope_p
	      || opcode == DELETE_EXPR || opcode == VEC_DELETE_EXPR);

  processing_template_decl++;
  bool template_dependent_p = op0
    && (type_dependent_expression_p (op0)
	|| value_dependent_expression_p (op0));
  if (!template_dependent_p)
    processing_template_decl--;

  tree result;

  gcc_assert (op0 || opcode == THROW_EXPR);

  switch (opcode)
    {
    case NOEXCEPT_EXPR:
      result = finish_noexcept_expr (op0, tf_error);
      break;

    case THROW_EXPR:
      result = build_throw (input_location, op0);
      break;

    case TYPEID_EXPR:
      result = build_typeid (op0, tf_error);
      break;

    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
      result = cxx_sizeof_or_alignof_expr (input_location,
					   op0, opcode, true);
      break;

    case DELETE_EXPR:
    case VEC_DELETE_EXPR:
      result = delete_sanity (input_location, op0, NULL_TREE,
			      opcode == VEC_DELETE_EXPR,
			      global_scope_p, tf_error);
      break;

    case EXPR_PACK_EXPANSION:
      result = make_pack_expansion (op0);
      break;

      // We're using this for sizeof...(pack).  */
    case TYPE_PACK_EXPANSION:
      result = make_pack_expansion (op0);
      PACK_EXPANSION_SIZEOF_P (result) = true;
      break;

    default:
      result = build_x_unary_op (/*loc=*/0, opcode, op0, tf_error);
      break;
    }

  if (template_dependent_p)
    processing_template_decl--;

  return convert_out (ctx->preserve (result));
}

gcc_expr
plugin_build_binary_expr (cc1_plugin::connection *self,
			  const char *binary_op,
			  gcc_expr operand1,
			  gcc_expr operand2)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree op0 = convert_in (operand1);
  tree op1 = convert_in (operand2);
  tree_code opcode = ERROR_MARK;

  switch (CHARS2 (binary_op[0], binary_op[1]))
    {
    case CHARS2 ('p', 'l'): // operator +
      opcode = PLUS_EXPR;
      break;
    case CHARS2 ('m', 'i'): // operator -
      opcode = MINUS_EXPR;
      break;
    case CHARS2 ('m', 'l'): // operator *
      opcode = MULT_EXPR;
      break;
    case CHARS2 ('d', 'v'): // operator /
      opcode = TRUNC_DIV_EXPR;
      break;
    case CHARS2 ('r', 'm'): // operator %
      opcode = TRUNC_MOD_EXPR;
      break;
    case CHARS2 ('a', 'n'): // operator &
      opcode = BIT_AND_EXPR;
      break;
    case CHARS2 ('o', 'r'): // operator |
      opcode = BIT_IOR_EXPR;
      break;
    case CHARS2 ('e', 'o'): // operator ^
      opcode = BIT_XOR_EXPR;
      break;
    case CHARS2 ('l', 's'): // operator <<
      opcode = LSHIFT_EXPR;
      break;
    case CHARS2 ('r', 's'): // operator >>
      opcode = RSHIFT_EXPR;
      break;
    case CHARS2 ('e', 'q'): // operator ==
      opcode = EQ_EXPR;
      break;
    case CHARS2 ('n', 'e'): // operator !=
      opcode = NE_EXPR;
      break;
    case CHARS2 ('l', 't'): // operator <
      opcode = LT_EXPR;
      break;
    case CHARS2 ('g', 't'): // operator >
      opcode = GT_EXPR;
      break;
    case CHARS2 ('l', 'e'): // operator <=
      opcode = LE_EXPR;
      break;
    case CHARS2 ('g', 'e'): // operator >=
      opcode = GE_EXPR;
      break;
    case CHARS2 ('a', 'a'): // operator &&
      opcode = TRUTH_ANDIF_EXPR;
      break;
    case CHARS2 ('o', 'o'): // operator ||
      opcode = TRUTH_ORIF_EXPR;
      break;
    case CHARS2 ('c', 'm'): // operator ,
      opcode = COMPOUND_EXPR;
      break;
    case CHARS2 ('p', 'm'): // operator ->*
      opcode = MEMBER_REF;
      break;
    case CHARS2 ('p', 't'): // operator ->
      opcode = INDIRECT_REF; // Not really!  This will stand for
			     // INDIRECT_REF followed by COMPONENT_REF
			     // later on.
      break;
    case CHARS2 ('i', 'x'): // operator []
      opcode = ARRAY_REF;
      break;
    case CHARS2 ('d', 's'): // operator .*
      opcode = DOTSTAR_EXPR;
      break;
    case CHARS2 ('d', 't'): // operator .
      opcode = COMPONENT_REF;
      break;

    default:
      gcc_unreachable ();
    }

  processing_template_decl++;
  bool template_dependent_p = type_dependent_expression_p (op0)
    || value_dependent_expression_p (op0)
    || type_dependent_expression_p (op1)
    || value_dependent_expression_p (op1);
  if (!template_dependent_p)
    processing_template_decl--;

  tree result;

  switch (opcode)
    {
    case INDIRECT_REF: // This is actually a "->".
      op0 = build_x_arrow (/*loc=*/0, op0, tf_error);
      /* Fall through.  */
    case COMPONENT_REF:
      result = finish_class_member_access_expr (op0, op1,
						/*template_p=*/false,
						tf_error);
      break;

    default:
      result = build_x_binary_op (/*loc=*/0, opcode, op0, ERROR_MARK,
				  op1, ERROR_MARK, NULL, tf_error);
      break;
    }

  if (template_dependent_p)
    processing_template_decl--;

  return convert_out (ctx->preserve (result));
}

gcc_expr
plugin_build_ternary_expr (cc1_plugin::connection *self,
			   const char *ternary_op,
			   gcc_expr operand1,
			   gcc_expr operand2,
			   gcc_expr operand3)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree op0 = convert_in (operand1);
  tree op1 = convert_in (operand2);
  tree op2 = convert_in (operand3);
  gcc_assert (CHARS2 (ternary_op[0], ternary_op[1])
	      == CHARS2 ('q', 'u')); // ternary operator

  processing_template_decl++;
  bool template_dependent_p = type_dependent_expression_p (op0)
    || value_dependent_expression_p (op0)
    || type_dependent_expression_p (op1)
    || value_dependent_expression_p (op1)
    || type_dependent_expression_p (op2)
    || value_dependent_expression_p (op2);
  if (!template_dependent_p)
    processing_template_decl--;

  tree val = build_x_conditional_expr (/*loc=*/0, op0, op1, op2, tf_error);

  if (template_dependent_p)
    processing_template_decl--;

  return convert_out (ctx->preserve (val));
}

gcc_expr
plugin_build_unary_type_expr (cc1_plugin::connection *self,
			      const char *unary_op,
			      gcc_type operand)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree type = convert_in (operand);
  tree_code opcode = ERROR_MARK;

  switch (CHARS2 (unary_op[0], unary_op[1]))
    {
    case CHARS2 ('t', 'i'): // typeid (type)
      opcode = TYPEID_EXPR;
      break;

    case CHARS2 ('s', 't'): // sizeof (type)
      opcode = SIZEOF_EXPR;
      break;
    case CHARS2 ('a', 't'): // alignof (type)
      opcode = ALIGNOF_EXPR;
      break;

    case CHARS2 ('s', 'Z'): // sizeof...(pack)
      opcode = TYPE_PACK_EXPANSION; // Not really, but let's use its code.
      break;

      // FIXME: do we have to handle "sp", for the size of a captured
      // template parameter pack from an alias template, taking
      // multiple template arguments?

    default:
      gcc_unreachable ();
    }

  processing_template_decl++;
  bool template_dependent_p = dependent_type_p (type);
  if (!template_dependent_p)
    processing_template_decl--;

  tree result;

  switch (opcode)
    {
    case TYPEID_EXPR:
      result = get_typeid (type, tf_error);
      break;

      // We're using this for sizeof...(pack).  */
    case TYPE_PACK_EXPANSION:
      result = make_pack_expansion (type);
      PACK_EXPANSION_SIZEOF_P (result) = true;
      break;

    default:
      /* Use the C++11 alignof semantics.  */
      result = cxx_sizeof_or_alignof_type (input_location, type,
					   opcode, true, true);
    }

  if (template_dependent_p)
    processing_template_decl--;

  return convert_out (ctx->preserve (result));
}

gcc_expr
plugin_build_cast_expr (cc1_plugin::connection *self,
			const char *binary_op,
			gcc_type operand1,
			gcc_expr operand2)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree (*build_cast)(location_t loc, tree type, tree expr,
		     tsubst_flags_t complain) = NULL;
  tree type = convert_in (operand1);
  tree expr = convert_in (operand2);

  switch (CHARS2 (binary_op[0], binary_op[1]))
    {
    case CHARS2 ('d', 'c'): // dynamic_cast
      build_cast = build_dynamic_cast;
      break;

    case CHARS2 ('s', 'c'): // static_cast
      build_cast = build_static_cast;
      break;

    case CHARS2 ('c', 'c'): // const_cast
      build_cast = build_const_cast;
      break;

    case CHARS2 ('r', 'c'): // reinterpret_cast
      build_cast = build_reinterpret_cast;
      break;

    case CHARS2 ('c', 'v'): // C cast, conversion with one argument
      build_cast = cp_build_c_cast;
      break;

    default:
      gcc_unreachable ();
    }

  processing_template_decl++;
  bool template_dependent_p = dependent_type_p (type)
    || type_dependent_expression_p (expr)
    || value_dependent_expression_p (expr);
  if (!template_dependent_p)
    processing_template_decl--;

  tree val = build_cast (input_location, type, expr, tf_error);

  if (template_dependent_p)
    processing_template_decl--;

  return convert_out (ctx->preserve (val));
}

static inline vec<tree, va_gc> *
args_to_tree_vec (const struct gcc_cp_function_args *args_in)
{
  vec<tree, va_gc> *args = make_tree_vector ();
  for (int i = 0; i < args_in->n_elements; i++)
    vec_safe_push (args, convert_in (args_in->elements[i]));
  return args;
}

static inline tree
args_to_tree_list (const struct gcc_cp_function_args *args_in)
{
  tree args, *tail = &args;
  for (int i = 0; i < args_in->n_elements; i++)
    {
      *tail = build_tree_list (NULL, convert_in (args_in->elements[i]));
      tail = &TREE_CHAIN (*tail);
    }
  return args;
}

static inline vec<constructor_elt, va_gc> *
args_to_ctor_elts (const struct gcc_cp_function_args *args_in)
{
  vec<constructor_elt, va_gc> *args = NULL;
  for (int i = 0; i < args_in->n_elements; i++)
    CONSTRUCTOR_APPEND_ELT (args, NULL_TREE, convert_in (args_in->elements[i]));
  return args;
}

gcc_expr
plugin_build_expression_list_expr (cc1_plugin::connection *self,
				   const char *conv_op,
				   gcc_type type_in,
				   const struct gcc_cp_function_args *values_in)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree type = convert_in (type_in);
  tree args;
  tree result;

  switch (CHARS2 (conv_op[0], conv_op[1]))
    {
    case CHARS2 ('c', 'v'): // conversion with parenthesized expression list
      gcc_assert (TYPE_P (type));
      args = args_to_tree_list (values_in);
      result = build_functional_cast (input_location, type, args, tf_error);
      break;

    case CHARS2 ('t', 'l'): // conversion with braced expression list
      gcc_assert (type);
      gcc_assert (TYPE_P (type));
      args = make_node (CONSTRUCTOR);
      CONSTRUCTOR_ELTS (args) = args_to_ctor_elts (values_in);
      CONSTRUCTOR_IS_DIRECT_INIT (args) = 1;
      result = finish_compound_literal (type, args, tf_error);
      break;

    case CHARS2 ('i', 'l'): // untyped braced expression list
      gcc_assert (!type);
      result = make_node (CONSTRUCTOR);
      CONSTRUCTOR_ELTS (result) = args_to_ctor_elts (values_in);
      break;

    default:
      gcc_unreachable ();
    }

  return convert_out (ctx->preserve (result));
}

gcc_expr
plugin_build_new_expr (cc1_plugin::connection *self,
		       const char *new_op,
		       const struct gcc_cp_function_args *placement_in,
		       gcc_type type_in,
		       const struct gcc_cp_function_args *initializer_in)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree type = convert_in (type_in);
  vec<tree, va_gc> *placement = NULL, *initializer = NULL;
  bool global_scope_p = false;
  tree nelts = NULL;

  if (placement_in)
    placement = args_to_tree_vec (placement_in);
  if (initializer_in)
    initializer = args_to_tree_vec (initializer_in);

  gcc_assert (TYPE_P (type));

 once_more:
  switch (CHARS2 (new_op[0], new_op[1]))
    {
    case CHARS2 ('g', 's'):
      gcc_assert (!global_scope_p);
      global_scope_p = true;
      new_op += 2;
      goto once_more;

    case CHARS2 ('n', 'w'): // non-array new
      gcc_assert (TREE_CODE (type) != ARRAY_TYPE);
      break;

    case CHARS2 ('n', 'a'): // array new
      gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
      gcc_assert (TYPE_DOMAIN (type));
      {
	// Compute the length of the outermost array type, then discard it.
	tree maxelt = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
	tree eltype = TREE_TYPE (maxelt);
	tree onecst = integer_one_node;

	processing_template_decl++;
	bool template_dependent_p = value_dependent_expression_p (maxelt)
	  || type_dependent_expression_p (maxelt);
	if (!template_dependent_p)
	  {
	    processing_template_decl--;
	    onecst = fold_convert (eltype, onecst);
	  }

	nelts = fold_build2 (PLUS_EXPR, eltype, nelts, onecst);

	if (template_dependent_p)
	  processing_template_decl--;

	type = TREE_TYPE (type);
      }
      break;

    default:
      gcc_unreachable ();
    }

  processing_template_decl++;
  bool template_dependent_p = dependent_type_p (type)
    || value_dependent_expression_p (nelts)
    || (placement
	&& any_type_dependent_arguments_p (placement))
    || (initializer
	&& any_type_dependent_arguments_p (initializer));
  if (!template_dependent_p)
    processing_template_decl--;

  tree result = build_new (input_location, &placement, type, nelts,
			   &initializer, global_scope_p, tf_error);

  if (template_dependent_p)
    processing_template_decl--;

  if (placement != NULL)
    release_tree_vector (placement);
  if (initializer != NULL)
    release_tree_vector (initializer);

  return convert_out (ctx->preserve (result));
}

gcc_expr
plugin_build_call_expr (cc1_plugin::connection *self,
			gcc_expr callable_in, int qualified_p,
			const struct gcc_cp_function_args *args_in)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree callable = convert_in (callable_in);
  tree call_expr;

  vec<tree, va_gc> *args = args_to_tree_vec (args_in);

  bool koenig_p = false;
  if (!qualified_p && !args->is_empty ())
    {
      if (identifier_p (callable))
	koenig_p = true;
      else if (is_overloaded_fn (callable))
	{
	  tree fn = get_first_fn (callable);
	  fn = STRIP_TEMPLATE (fn);

	  if (!DECL_FUNCTION_MEMBER_P (fn)
	      && !DECL_LOCAL_FUNCTION_P (fn))
	    koenig_p = true;
	}
    }

  if (koenig_p && !any_type_dependent_arguments_p (args))
    callable = perform_koenig_lookup (callable, args, tf_none);

  if (TREE_CODE (callable) == COMPONENT_REF)
    {
      tree object = TREE_OPERAND (callable, 0);
      tree memfn = TREE_OPERAND (callable, 1);

      if (type_dependent_expression_p (object)
	  || (!BASELINK_P (memfn) && TREE_CODE (memfn) != FIELD_DECL)
	  || type_dependent_expression_p (memfn)
	  || any_type_dependent_arguments_p (args))
	call_expr = build_nt_call_vec (callable, args);
      else if (BASELINK_P (memfn))
	call_expr = build_new_method_call (object, memfn, &args, NULL_TREE,
					   qualified_p
					   ? LOOKUP_NORMAL|LOOKUP_NONVIRTUAL
					   : LOOKUP_NORMAL,
					   NULL, tf_none);
      else
	call_expr = finish_call_expr (callable, &args, false, false, tf_none);
    }
  else if (TREE_CODE (callable) == OFFSET_REF
	   || TREE_CODE (callable) == MEMBER_REF
	   || TREE_CODE (callable) == DOTSTAR_EXPR)
    call_expr = build_offset_ref_call_from_tree (callable, &args, tf_none);
  else
    call_expr = finish_call_expr (callable, &args,
				  !!qualified_p, koenig_p, tf_none);

  release_tree_vector (args);
  return convert_out (ctx->preserve (call_expr));
}

gcc_type
plugin_get_expr_type (cc1_plugin::connection *self,
		      gcc_expr operand)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree op0 = convert_in (operand);
  tree type;
  if (op0)
    type = TREE_TYPE (op0);
  else
    type = make_decltype_auto ();
  return convert_out (ctx->preserve (type));
}

gcc_decl
plugin_build_function_template_specialization (cc1_plugin::connection *self,
					       gcc_decl template_decl,
					       const gcc_cp_template_args *targs,
					       gcc_address address,
					       const char *filename,
					       unsigned int line_number)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  location_t loc = ctx->get_location_t (filename, line_number);
  tree name = convert_in (template_decl);
  tree targsl = targlist (targs);

  tree decl = tsubst (name, targsl, tf_error, NULL_TREE);
  DECL_SOURCE_LOCATION (decl) = loc;

  record_decl_address (ctx, build_decl_addr_value (decl, address));

  return convert_out (ctx->preserve (decl));
}

gcc_decl
plugin_build_class_template_specialization (cc1_plugin::connection *self,
					    gcc_decl template_decl,
					    const gcc_cp_template_args *args,
					    const char *filename,
					    unsigned int line_number)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  location_t loc = ctx->get_location_t (filename, line_number);
  tree name = convert_in (template_decl);

  tree tdecl = finish_template_type (name, targlist (args), false);;
  DECL_SOURCE_LOCATION (tdecl) = loc;

  return convert_out (ctx->preserve (tdecl));
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

gcc_type
plugin_get_int_type (cc1_plugin::connection *self,
		     int is_unsigned, unsigned long size_in_bytes,
		     const char *builtin_name)
{
  tree result;

  if (builtin_name)
    {
      result = safe_lookup_builtin_type (builtin_name);
      gcc_assert (!result || TREE_CODE (result) == INTEGER_TYPE);
    }
  else
    result = c_common_type_for_size (BITS_PER_UNIT * size_in_bytes,
				     is_unsigned);

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
plugin_get_char_type (cc1_plugin::connection *)
{
  return convert_out (char_type_node);
}

gcc_type
plugin_get_float_type (cc1_plugin::connection *,
		       unsigned long size_in_bytes,
		       const char *builtin_name)
{
  if (builtin_name)
    {
      tree result = safe_lookup_builtin_type (builtin_name);

      if (!result)
	return convert_out (error_mark_node);

      gcc_assert (TREE_CODE (result) == REAL_TYPE);
      gcc_assert (BITS_PER_UNIT * size_in_bytes == TYPE_PRECISION (result));

      return convert_out (result);
    }

  if (BITS_PER_UNIT * size_in_bytes == TYPE_PRECISION (float_type_node))
    return convert_out (float_type_node);
  if (BITS_PER_UNIT * size_in_bytes == TYPE_PRECISION (double_type_node))
    return convert_out (double_type_node);
  if (BITS_PER_UNIT * size_in_bytes == TYPE_PRECISION (long_double_type_node))
    return convert_out (long_double_type_node);
  return convert_out (error_mark_node);
}

gcc_type
plugin_get_void_type (cc1_plugin::connection *)
{
  return convert_out (void_type_node);
}

gcc_type
plugin_get_bool_type (cc1_plugin::connection *)
{
  return convert_out (boolean_type_node);
}

gcc_type
plugin_get_nullptr_type (cc1_plugin::connection *)
{
  return convert_out (nullptr_type_node);
}

gcc_expr
plugin_get_nullptr_constant (cc1_plugin::connection *)
{
  return convert_out (nullptr_node);
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
plugin_build_dependent_array_type (cc1_plugin::connection *self,
				   gcc_type element_type_in,
				   gcc_expr num_elements_in)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree element_type = convert_in (element_type_in);
  tree size = convert_in (num_elements_in);
  tree name = get_identifier ("dependent array type");

  processing_template_decl++;
  bool template_dependent_p = dependent_type_p (element_type)
    || type_dependent_expression_p (size)
    || value_dependent_expression_p (size);
  if (!template_dependent_p)
    processing_template_decl--;

  tree itype = compute_array_index_type (name, size, tf_error);
  tree type = build_cplus_array_type (element_type, itype);

  if (template_dependent_p)
    processing_template_decl--;

  return convert_out (ctx->preserve (type));
}

gcc_type
plugin_build_vla_array_type (cc1_plugin::connection *self,
			     gcc_type element_type_in,
			     const char *upper_bound_name)
{
  tree element_type = convert_in (element_type_in);
  tree upper_bound = lookup_name (get_identifier (upper_bound_name));
  tree size = fold_build2 (PLUS_EXPR, TREE_TYPE (upper_bound), upper_bound,
			   build_one_cst (TREE_TYPE (upper_bound)));
  tree range = compute_array_index_type (NULL_TREE, size,
					 tf_error);

  tree result = build_cplus_array_type (element_type, range);

  plugin_context *ctx = static_cast<plugin_context *> (self);
  return convert_out (ctx->preserve (result));
}

gcc_type
plugin_build_qualified_type (cc1_plugin::connection *,
			     gcc_type unqualified_type_in,
			     enum gcc_cp_qualifiers qualifiers)
{
  tree unqualified_type = convert_in (unqualified_type_in);
  cp_cv_quals quals = 0;

  if ((qualifiers & GCC_CP_QUALIFIER_CONST) != 0)
    quals |= TYPE_QUAL_CONST;
  if ((qualifiers & GCC_CP_QUALIFIER_VOLATILE) != 0)
    quals |= TYPE_QUAL_VOLATILE;
  if ((qualifiers & GCC_CP_QUALIFIER_RESTRICT) != 0)
    quals |= TYPE_QUAL_RESTRICT;

  gcc_assert ((TREE_CODE (unqualified_type) != METHOD_TYPE
	       && TREE_CODE (unqualified_type) != REFERENCE_TYPE)
	      || quals == 0);

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
  if (!TYPE_READONLY (type))
    type = build_qualified_type (type, TYPE_QUAL_CONST);
  decl = build_decl (ctx->get_location_t (filename, line_number),
		     VAR_DECL, get_identifier (name), type);
  TREE_STATIC (decl) = 1;
  TREE_READONLY (decl) = 1;
  cp_finish_decl (decl, cst, true, NULL, LOOKUP_ONLYCONVERTING);
  safe_pushdecl_maybe_friend (decl, false);

  return 1;
}

gcc_type
plugin_error (cc1_plugin::connection *,
	      const char *message)
{
  error ("%s", message);
  return convert_out (error_mark_node);
}

int
plugin_add_static_assert (cc1_plugin::connection *self,
			  gcc_expr condition_in,
			  const char *errormsg,
			  const char *filename,
			  unsigned int line_number)
{
  plugin_context *ctx = static_cast<plugin_context *> (self);
  tree condition = convert_in (condition_in);

  if (!errormsg)
    errormsg = "";

  tree message = build_string (strlen (errormsg) + 1, errormsg);

  TREE_TYPE (message) = char_array_type_node;
  fix_string_type (message);

  location_t loc = ctx->get_location_t (filename, line_number);

  bool member_p = at_class_scope_p ();

  finish_static_assert (condition, message, loc, member_p);

  return 1;
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
  if (version != GCC_CP_FE_VERSION_0)
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

#include "gcc-cp-fe.def"

#undef GCC_METHOD0
#undef GCC_METHOD1
#undef GCC_METHOD2
#undef GCC_METHOD3
#undef GCC_METHOD4
#undef GCC_METHOD5
#undef GCC_METHOD7

  return 0;
}
