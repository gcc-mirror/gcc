/* Callgraph handling code.
   Copyright (C) 2003-2013 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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
#include "tm.h"
#include "tree.h"
#include "cgraph.h"
#include "langhooks.h"
#include "diagnostic-core.h"
#include "hashtab.h"
#include "ggc.h"
#include "timevar.h"
#include "debug.h"
#include "target.h"
#include "output.h"
#include "gimple.h"
#include "flags.h"

/* List of hooks triggered on varpool_node events.  */
struct varpool_node_hook_list {
  varpool_node_hook hook;
  void *data;
  struct varpool_node_hook_list *next;
};

/* List of hooks triggered when a node is removed.  */
struct varpool_node_hook_list *first_varpool_node_removal_hook;
/* List of hooks triggered when an variable is inserted.  */
struct varpool_node_hook_list *first_varpool_variable_insertion_hook;

/* Register HOOK to be called with DATA on each removed node.  */
struct varpool_node_hook_list *
varpool_add_node_removal_hook (varpool_node_hook hook, void *data)
{
  struct varpool_node_hook_list *entry;
  struct varpool_node_hook_list **ptr = &first_varpool_node_removal_hook;

  entry = (struct varpool_node_hook_list *) xmalloc (sizeof (*entry));
  entry->hook = hook;
  entry->data = data;
  entry->next = NULL;
  while (*ptr)
    ptr = &(*ptr)->next;
  *ptr = entry;
  return entry;
}

/* Remove ENTRY from the list of hooks called on removing nodes.  */
void
varpool_remove_node_removal_hook (struct varpool_node_hook_list *entry)
{
  struct varpool_node_hook_list **ptr = &first_varpool_node_removal_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Call all node removal hooks.  */
static void
varpool_call_node_removal_hooks (struct varpool_node *node)
{
  struct varpool_node_hook_list *entry = first_varpool_node_removal_hook;
  while (entry)
  {
    entry->hook (node, entry->data);
    entry = entry->next;
  }
}

/* Register HOOK to be called with DATA on each inserted node.  */
struct varpool_node_hook_list *
varpool_add_variable_insertion_hook (varpool_node_hook hook, void *data)
{
  struct varpool_node_hook_list *entry;
  struct varpool_node_hook_list **ptr = &first_varpool_variable_insertion_hook;

  entry = (struct varpool_node_hook_list *) xmalloc (sizeof (*entry));
  entry->hook = hook;
  entry->data = data;
  entry->next = NULL;
  while (*ptr)
    ptr = &(*ptr)->next;
  *ptr = entry;
  return entry;
}

/* Remove ENTRY from the list of hooks called on inserted nodes.  */
void
varpool_remove_variable_insertion_hook (struct varpool_node_hook_list *entry)
{
  struct varpool_node_hook_list **ptr = &first_varpool_variable_insertion_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Call all node insertion hooks.  */
void
varpool_call_variable_insertion_hooks (struct varpool_node *node)
{
  struct varpool_node_hook_list *entry = first_varpool_variable_insertion_hook;
  while (entry)
  {
    entry->hook (node, entry->data);
    entry = entry->next;
  }
}

/* Allocate new callgraph node and insert it into basic data structures.  */

struct varpool_node *
varpool_create_empty_node (void)
{   
  struct varpool_node *node = ggc_alloc_cleared_varpool_node ();
  node->symbol.type = SYMTAB_VARIABLE;
  return node;
}   

/* Return varpool node assigned to DECL.  Create new one when needed.  */
struct varpool_node *
varpool_node_for_decl (tree decl)
{
  struct varpool_node *node = varpool_get_node (decl);
  gcc_checking_assert (TREE_CODE (decl) == VAR_DECL);
  if (node)
    return node;

  node = varpool_create_empty_node ();
  node->symbol.decl = decl;
  symtab_register_node ((symtab_node)node);
  return node;
}

/* Remove node from the varpool.  */
void
varpool_remove_node (struct varpool_node *node)
{
  tree init;
  varpool_call_node_removal_hooks (node);
  symtab_unregister_node ((symtab_node)node);

  /* Because we remove references from external functions before final compilation,
     we may end up removing useful constructors.
     FIXME: We probably want to trace boundaries better.  */
  if ((init = ctor_for_folding (node->symbol.decl)) == error_mark_node)
    varpool_remove_initializer (node);
  else
    DECL_INITIAL (node->symbol.decl) = init;
  ggc_free (node);
}

/* Renove node initializer when it is no longer needed.  */
void
varpool_remove_initializer (struct varpool_node *node)
{
  if (DECL_INITIAL (node->symbol.decl)
      && !DECL_IN_CONSTANT_POOL (node->symbol.decl)
      /* Keep vtables for BINFO folding.  */
      && !DECL_VIRTUAL_P (node->symbol.decl)
      /* FIXME: http://gcc.gnu.org/PR55395 */
      && debug_info_level == DINFO_LEVEL_NONE
      /* When doing declaration merging we have duplicate
	 entries for given decl.  Do not attempt to remove
	 the boides, or we will end up remiving
	 wrong one.  */
      && cgraph_state != CGRAPH_LTO_STREAMING)
    DECL_INITIAL (node->symbol.decl) = error_mark_node;
}

/* Dump given cgraph node.  */
void
dump_varpool_node (FILE *f, struct varpool_node *node)
{
  dump_symtab_base (f, (symtab_node)node);
  fprintf (f, "  Availability: %s\n",
	   cgraph_function_flags_ready
	   ? cgraph_availability_names[cgraph_variable_initializer_availability (node)]
	   : "not-ready");
  fprintf (f, "  Varpool flags:");
  if (DECL_INITIAL (node->symbol.decl))
    fprintf (f, " initialized");
  if (node->output)
    fprintf (f, " output");
  if (TREE_READONLY (node->symbol.decl))
    fprintf (f, " read-only");
  if (ctor_for_folding (node->symbol.decl) != error_mark_node)
    fprintf (f, " const-value-known");
  fprintf (f, "\n");
}

/* Dump the variable pool.  */
void
dump_varpool (FILE *f)
{
  struct varpool_node *node;

  fprintf (f, "variable pool:\n\n");
  FOR_EACH_VARIABLE (node)
    dump_varpool_node (f, node);
}

/* Dump the variable pool to stderr.  */

DEBUG_FUNCTION void
debug_varpool (void)
{
  dump_varpool (stderr);
}

/* Given an assembler name, lookup node.  */
struct varpool_node *
varpool_node_for_asm (tree asmname)
{
  if (symtab_node node = symtab_node_for_asm (asmname))
    return dyn_cast <varpool_node> (node);
  else
    return NULL;
}

/* Return if DECL is constant and its initial value is known (so we can do
   constant folding using DECL_INITIAL (decl)).
   Return ERROR_MARK_NODE when value is unknown.  */

tree
ctor_for_folding (tree decl)
{
  struct varpool_node *node, *real_node;
  tree real_decl;

  if (TREE_CODE (decl) != VAR_DECL
      && TREE_CODE (decl) != CONST_DECL)
    return error_mark_node;

  if (TREE_CODE (decl) == CONST_DECL
      || DECL_IN_CONSTANT_POOL (decl))
    return DECL_INITIAL (decl);

  if (TREE_THIS_VOLATILE (decl))
    return error_mark_node;

  /* Do not care about automatic variables.  Those are never initialized
     anyway, because gimplifier exapnds the code*/
  if (!TREE_STATIC (decl) && !DECL_EXTERNAL (decl))
    {
      gcc_assert (!TREE_PUBLIC (decl));
      return error_mark_node;
    }

  gcc_assert (TREE_CODE (decl) == VAR_DECL);

  node = varpool_get_node (decl);
  if (node)
    {
      real_node = varpool_variable_node (node);
      real_decl = real_node->symbol.decl;
    }
  else
    real_decl = decl;

  /* See if we are dealing with alias.
     In most cases alias is just alternative symbol pointing to a given
     constructor.  This allows us to use interposition rules of DECL
     constructor of REAL_NODE.  However weakrefs are special by being just
     alternative name of their target (if defined).  */
  if (decl != real_decl)
    {
      gcc_assert (!DECL_INITIAL (decl)
		  || DECL_INITIAL (decl) == error_mark_node);
      if (lookup_attribute ("weakref", DECL_ATTRIBUTES (decl)))
	{
	  node = varpool_alias_target (node);
	  decl = node->symbol.decl;
	}
    }

  /* Vtables are defined by their types and must match no matter of interposition
     rules.  */
  if (DECL_VIRTUAL_P (real_decl))
    {
      gcc_checking_assert (TREE_READONLY (real_decl));
      return DECL_INITIAL (real_decl);
    }

  /* If thre is no constructor, we have nothing to do.  */
  if (DECL_INITIAL (real_decl) == error_mark_node)
    return error_mark_node;

  /* Non-readonly alias of readonly variable is also de-facto readonly,
     because the variable itself is in readonly section.  
     We also honnor READONLY flag on alias assuming that user knows
     what he is doing.  */
  if (!TREE_READONLY (decl) && !TREE_READONLY (real_decl))
    return error_mark_node;

  /* Variables declared 'const' without an initializer
     have zero as the initializer if they may not be
     overridden at link or run time.  */
  if (!DECL_INITIAL (real_decl)
      && (DECL_EXTERNAL (decl) || decl_replaceable_p (decl)))
    return error_mark_node;

  /* Variables declared `const' with an initializer are considered
     to not be overwritable with different initializer by default. 

     ??? Previously we behaved so for scalar variables but not for array
     accesses.  */
  return DECL_INITIAL (real_decl);
}

/* Add the variable DECL to the varpool.
   Unlike varpool_finalize_decl function is intended to be used
   by middle end and allows insertion of new variable at arbitrary point
   of compilation.  */
void
varpool_add_new_variable (tree decl)
{
  struct varpool_node *node;
  varpool_finalize_decl (decl);
  node = varpool_node_for_decl (decl);
  varpool_call_variable_insertion_hooks (node);
  if (varpool_externally_visible_p (node))
    node->symbol.externally_visible = true;
}

/* Return variable availability.  See cgraph.h for description of individual
   return values.  */
enum availability
cgraph_variable_initializer_availability (struct varpool_node *node)
{
  gcc_assert (cgraph_function_flags_ready);
  if (!node->symbol.definition)
    return AVAIL_NOT_AVAILABLE;
  if (!TREE_PUBLIC (node->symbol.decl))
    return AVAIL_AVAILABLE;
  if (DECL_IN_CONSTANT_POOL (node->symbol.decl)
      || DECL_VIRTUAL_P (node->symbol.decl))
    return AVAIL_AVAILABLE;
  if (node->symbol.alias && node->symbol.weakref)
    {
      enum availability avail;

      cgraph_variable_initializer_availability
	      (varpool_variable_node (node, &avail));
      return avail;
    }
  /* If the variable can be overwritten, return OVERWRITABLE.  Takes
     care of at least one notable extension - the COMDAT variables
     used to share template instantiations in C++.  */
  if (decl_replaceable_p (node->symbol.decl)
      || DECL_EXTERNAL (node->symbol.decl))
    return AVAIL_OVERWRITABLE;
  return AVAIL_AVAILABLE;
}

void
varpool_analyze_node (struct varpool_node *node)
{
  tree decl = node->symbol.decl;

  /* When reading back varpool at LTO time, we re-construct the queue in order
     to have "needed" list right by inserting all needed nodes into varpool.
     We however don't want to re-analyze already analyzed nodes.  */
  if (!node->symbol.analyzed)
    {
      gcc_assert (!in_lto_p || cgraph_function_flags_ready);
      /* Compute the alignment early so function body expanders are
	 already informed about increased alignment.  */
      align_variable (decl, 0);
    }
  if (node->symbol.alias)
    symtab_resolve_alias
       ((symtab_node) node, (symtab_node) varpool_get_node (node->symbol.alias_target));
  else if (DECL_INITIAL (decl))
    record_references_in_initializer (decl, node->symbol.analyzed);
  node->symbol.analyzed = true;
}

/* Assemble thunks and aliases associated to NODE.  */

static void
assemble_aliases (struct varpool_node *node)
{
  int i;
  struct ipa_ref *ref;
  for (i = 0; ipa_ref_list_referring_iterate (&node->symbol.ref_list, i, ref); i++)
    if (ref->use == IPA_REF_ALIAS)
      {
	struct varpool_node *alias = ipa_ref_referring_varpool_node (ref);
	do_assemble_alias (alias->symbol.decl,
			   DECL_ASSEMBLER_NAME (node->symbol.decl));
	assemble_aliases (alias);
      }
}

/* Output one variable, if necessary.  Return whether we output it.  */

bool
varpool_assemble_decl (struct varpool_node *node)
{
  tree decl = node->symbol.decl;

  /* Aliases are outout when their target is produced or by
     output_weakrefs.  */
  if (node->symbol.alias)
    return false;

  /* Constant pool is output from RTL land when the reference
     survive till this level.  */
  if (DECL_IN_CONSTANT_POOL (decl) && TREE_ASM_WRITTEN (decl))
    return false;

  /* Decls with VALUE_EXPR should not be in the varpool at all.  They
     are not real variables, but just info for debugging and codegen.
     Unfortunately at the moment emutls is not updating varpool correctly
     after turning real vars into value_expr vars.  */
  if (DECL_HAS_VALUE_EXPR_P (decl)
      && !targetm.have_tls)
    return false;

  /* Hard register vars do not need to be output.  */
  if (DECL_HARD_REGISTER (decl))
    return false;

  gcc_checking_assert (!TREE_ASM_WRITTEN (decl)
		       && TREE_CODE (decl) == VAR_DECL
		       && !DECL_HAS_VALUE_EXPR_P (decl));

  if (!node->symbol.in_other_partition
      && !DECL_EXTERNAL (decl))
    {
      assemble_variable (decl, 0, 1, 0);
      gcc_assert (TREE_ASM_WRITTEN (decl));
      node->symbol.definition = true;
      assemble_aliases (node);
      return true;
    }

  return false;
}

/* Add NODE to queue starting at FIRST. 
   The queue is linked via AUX pointers and terminated by pointer to 1.  */

static void
enqueue_node (struct varpool_node *node, struct varpool_node **first)
{
  if (node->symbol.aux)
    return;
  gcc_checking_assert (*first);
  node->symbol.aux = *first;
  *first = node;
}

/* Optimization of function bodies might've rendered some variables as
   unnecessary so we want to avoid these from being compiled.  Re-do
   reachability starting from variables that are either externally visible
   or was referred from the asm output routines.  */

static void
varpool_remove_unreferenced_decls (void)
{
  struct varpool_node *next, *node;
  struct varpool_node *first = (struct varpool_node *)(void *)1;
  int i;
  struct ipa_ref *ref;

  if (seen_error ())
    return;

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "Trivially needed variables:");
  FOR_EACH_DEFINED_VARIABLE (node)
    {
      if (node->symbol.analyzed
	  && (!varpool_can_remove_if_no_refs (node)
	      /* We just expanded all function bodies.  See if any of
		 them needed the variable.  */
	      || DECL_RTL_SET_P (node->symbol.decl)))
	{
	  enqueue_node (node, &first);
          if (cgraph_dump_file)
	    fprintf (cgraph_dump_file, " %s", varpool_node_asm_name (node));
	}
    }
  while (first != (struct varpool_node *)(void *)1)
    {
      node = first;
      first = (struct varpool_node *)first->symbol.aux;

      if (node->symbol.same_comdat_group)
	{
	  symtab_node next;
	  for (next = node->symbol.same_comdat_group;
	       next != (symtab_node)node;
	       next = next->symbol.same_comdat_group)
	    {
	      varpool_node *vnext = dyn_cast <varpool_node> (next);
	      if (vnext && vnext->symbol.analyzed)
		enqueue_node (vnext, &first);
	    }
	}
      for (i = 0; ipa_ref_list_reference_iterate (&node->symbol.ref_list, i, ref); i++)
	{
	  varpool_node *vnode = dyn_cast <varpool_node> (ref->referred);
	  if (vnode
	      && (!DECL_EXTERNAL (ref->referred->symbol.decl)
		  || vnode->symbol.alias)
	      && vnode->symbol.analyzed)
	    enqueue_node (vnode, &first);
	}
    }
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\nRemoving variables:");
  for (node = varpool_first_defined_variable (); node; node = next)
    {
      next = varpool_next_defined_variable (node);
      if (!node->symbol.aux)
	{
          if (cgraph_dump_file)
	    fprintf (cgraph_dump_file, " %s", varpool_node_asm_name (node));
	  varpool_remove_node (node);
	}
    }
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\n");
}

/* For variables in named sections make sure get_variable_section
   is called before we switch to those sections.  Then section
   conflicts between read-only and read-only requiring relocations
   sections can be resolved.  */
void
varpool_finalize_named_section_flags (struct varpool_node *node)
{
  if (!TREE_ASM_WRITTEN (node->symbol.decl)
      && !node->symbol.alias
      && !node->symbol.in_other_partition
      && !DECL_EXTERNAL (node->symbol.decl)
      && TREE_CODE (node->symbol.decl) == VAR_DECL
      && !DECL_HAS_VALUE_EXPR_P (node->symbol.decl)
      && DECL_SECTION_NAME (node->symbol.decl))
    get_variable_section (node->symbol.decl, false);
}

/* Output all variables enqueued to be assembled.  */
bool
varpool_output_variables (void)
{
  bool changed = false;
  struct varpool_node *node;

  if (seen_error ())
    return false;

  varpool_remove_unreferenced_decls ();

  timevar_push (TV_VAROUT);

  FOR_EACH_DEFINED_VARIABLE (node)
    varpool_finalize_named_section_flags (node);

  FOR_EACH_DEFINED_VARIABLE (node)
    if (varpool_assemble_decl (node))
      changed = true;
  timevar_pop (TV_VAROUT);
  return changed;
}

/* Create a new global variable of type TYPE.  */
tree
add_new_static_var (tree type)
{
  tree new_decl;
  struct varpool_node *new_node;

  new_decl = create_tmp_var_raw (type, NULL);
  DECL_NAME (new_decl) = create_tmp_var_name (NULL);
  TREE_READONLY (new_decl) = 0;
  TREE_STATIC (new_decl) = 1;
  TREE_USED (new_decl) = 1;
  DECL_CONTEXT (new_decl) = NULL_TREE;
  DECL_ABSTRACT (new_decl) = 0;
  lang_hooks.dup_lang_specific_decl (new_decl);
  new_node = varpool_node_for_decl (new_decl);
  varpool_finalize_decl (new_decl);

  return new_node->symbol.decl;
}

/* Attempt to mark ALIAS as an alias to DECL.  Return TRUE if successful.
   Extra name aliases are output whenever DECL is output.  */

struct varpool_node *
varpool_create_variable_alias (tree alias, tree decl)
{
  struct varpool_node *alias_node;

  gcc_assert (TREE_CODE (decl) == VAR_DECL);
  gcc_assert (TREE_CODE (alias) == VAR_DECL);
  alias_node = varpool_node_for_decl (alias);
  alias_node->symbol.alias = true;
  alias_node->symbol.definition = true;
  alias_node->symbol.alias_target = decl;
  if (lookup_attribute ("weakref", DECL_ATTRIBUTES (alias)) != NULL)
    alias_node->symbol.weakref = true;
  return alias_node;
}

/* Attempt to mark ALIAS as an alias to DECL.  Return TRUE if successful.
   Extra name aliases are output whenever DECL is output.  */

struct varpool_node *
varpool_extra_name_alias (tree alias, tree decl)
{
  struct varpool_node *alias_node;

#ifndef ASM_OUTPUT_DEF
  /* If aliases aren't supported by the assembler, fail.  */
  return NULL;
#endif
  alias_node = varpool_create_variable_alias (alias, decl);
  alias_node->symbol.cpp_implicit_alias = true;

  /* Extra name alias mechanizm creates aliases really late
     via DECL_ASSEMBLER_NAME mechanizm.
     This is unfortunate because they are not going through the
     standard channels.  Ensure they get output.  */
  if (cpp_implicit_aliases_done)
    symtab_resolve_alias ((symtab_node)alias_node,
			  (symtab_node)varpool_node_for_decl (decl));
  return alias_node;
}

/* Call calback on NODE and aliases associated to NODE. 
   When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
   skipped. */

bool
varpool_for_node_and_aliases (struct varpool_node *node,
			      bool (*callback) (struct varpool_node *, void *),
			      void *data,
			      bool include_overwritable)
{
  int i;
  struct ipa_ref *ref;

  if (callback (node, data))
    return true;
  for (i = 0; ipa_ref_list_referring_iterate (&node->symbol.ref_list, i, ref); i++)
    if (ref->use == IPA_REF_ALIAS)
      {
	struct varpool_node *alias = ipa_ref_referring_varpool_node (ref);
	if (include_overwritable
	    || cgraph_variable_initializer_availability (alias) > AVAIL_OVERWRITABLE)
          if (varpool_for_node_and_aliases (alias, callback, data,
					   include_overwritable))
	    return true;
      }
  return false;
}
