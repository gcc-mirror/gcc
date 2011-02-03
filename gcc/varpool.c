/* Callgraph handling code.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2010, 2011
   Free Software Foundation, Inc.
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
#include "tree-flow.h"
#include "flags.h"

/*  This file contains basic routines manipulating variable pool.

    Varpool acts as interface in between the front-end and middle-end
    and drives the decision process on what variables and when are
    going to be compiled.

    The varpool nodes are allocated lazily for declarations
    either by frontend or at callgraph construction time.
    All variables supposed to be output into final file needs to be
    explicitly marked by frontend via VARPOOL_FINALIZE_DECL function.  */

/* Hash table used to convert declarations into nodes.  */
static GTY((param_is (struct varpool_node))) htab_t varpool_hash;

/* The linked list of cgraph varpool nodes.
   Linked via node->next pointer.  */
struct varpool_node *varpool_nodes;

/* Queue of cgraph nodes scheduled to be lowered and output.
   The queue is maintained via mark_needed_node, linked via node->next_needed
   pointer.

   LAST_NEEDED_NODE points to the end of queue, so it can be
   maintained in forward order.  GTY is needed to make it friendly to
   PCH.

   During compilation we construct the queue of needed variables
   twice: first time it is during cgraph construction, second time it is at the
   end of compilation in VARPOOL_REMOVE_UNREFERENCED_DECLS so we can avoid
   optimized out variables being output.

   Each variable is thus first analyzed and then later possibly output.
   FIRST_UNANALYZED_NODE points to first node in queue that was not analyzed
   yet and is moved via VARPOOL_ANALYZE_PENDING_DECLS.  */

struct varpool_node *varpool_nodes_queue;
static GTY(()) struct varpool_node *varpool_last_needed_node;
static GTY(()) struct varpool_node *varpool_first_unanalyzed_node;

/* Lists all assembled variables to be sent to debugger output later on.  */
static GTY(()) struct varpool_node *varpool_assembled_nodes_queue;

/* Return name of the node used in debug output.  */
const char *
varpool_node_name (struct varpool_node *node)
{
  return lang_hooks.decl_printable_name (node->decl, 2);
}

/* Returns a hash code for P.  */
static hashval_t
hash_varpool_node (const void *p)
{
  const struct varpool_node *n = (const struct varpool_node *) p;
  return (hashval_t) DECL_UID (n->decl);
}

/* Returns nonzero if P1 and P2 are equal.  */
static int
eq_varpool_node (const void *p1, const void *p2)
{
  const struct varpool_node *n1 =
    (const struct varpool_node *) p1;
  const struct varpool_node *n2 =
    (const struct varpool_node *) p2;
  return DECL_UID (n1->decl) == DECL_UID (n2->decl);
}

/* Return varpool node assigned to DECL without creating new one.  */
struct varpool_node *
varpool_get_node (const_tree decl)
{
  struct varpool_node key, **slot;

  gcc_assert (TREE_CODE (decl) == VAR_DECL
	      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)));

  if (!varpool_hash)
    return NULL;
  key.decl = CONST_CAST2 (tree, const_tree, decl);
  slot = (struct varpool_node **)
    htab_find_slot (varpool_hash, &key, NO_INSERT);
  if (!slot)
    return NULL;
  return *slot;
}

/* Return varpool node assigned to DECL.  Create new one when needed.  */
struct varpool_node *
varpool_node (tree decl)
{
  struct varpool_node key, *node, **slot;

  gcc_assert (TREE_CODE (decl) == VAR_DECL
	      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)));

  if (!varpool_hash)
    varpool_hash = htab_create_ggc (10, hash_varpool_node,
					   eq_varpool_node, NULL);
  key.decl = decl;
  slot = (struct varpool_node **)
    htab_find_slot (varpool_hash, &key, INSERT);
  if (*slot)
    return *slot;
  node = ggc_alloc_cleared_varpool_node ();
  node->decl = decl;
  node->order = cgraph_order++;
  node->next = varpool_nodes;
  ipa_empty_ref_list (&node->ref_list);
  if (varpool_nodes)
    varpool_nodes->prev = node;
  varpool_nodes = node;
  *slot = node;
  return node;
}

/* Remove node from the varpool.  */
void
varpool_remove_node (struct varpool_node *node)
{
  void **slot;
  slot = htab_find_slot (varpool_hash, node, NO_INSERT);
  gcc_assert (*slot == node);
  htab_clear_slot (varpool_hash, slot);
  gcc_assert (!varpool_assembled_nodes_queue);
  if (!node->alias)
    while (node->extra_name)
      varpool_remove_node (node->extra_name);
  if (node->next)
    node->next->prev = node->prev;
  if (node->prev)
    node->prev->next = node->next;
  else
    {
      if (node->alias && node->extra_name)
	{
          gcc_assert (node->extra_name->extra_name == node);
	  node->extra_name->extra_name = node->next;
	}
      else
	{
          gcc_assert (varpool_nodes == node);
          varpool_nodes = node->next;
	}
    }
  if (varpool_first_unanalyzed_node == node)
    varpool_first_unanalyzed_node = node->next_needed;
  if (node->next_needed)
    node->next_needed->prev_needed = node->prev_needed;
  else if (node->prev_needed)
    {
      gcc_assert (varpool_last_needed_node);
      varpool_last_needed_node = node->prev_needed;
    }
  if (node->prev_needed)
    node->prev_needed->next_needed = node->next_needed;
  else if (node->next_needed)
    {
      gcc_assert (varpool_nodes_queue == node);
      varpool_nodes_queue = node->next_needed;
    }
  if (node->same_comdat_group)
    {
      struct varpool_node *prev;
      for (prev = node->same_comdat_group;
	   prev->same_comdat_group != node;
	   prev = prev->same_comdat_group)
	;
      if (node->same_comdat_group == prev)
	prev->same_comdat_group = NULL;
      else
	prev->same_comdat_group = node->same_comdat_group;
      node->same_comdat_group = NULL;
    }
  ipa_remove_all_references (&node->ref_list);
  ipa_remove_all_refering (&node->ref_list);
  ggc_free (node);
}

/* Dump given cgraph node.  */
void
dump_varpool_node (FILE *f, struct varpool_node *node)
{
  fprintf (f, "%s:", varpool_node_name (node));
  fprintf (f, " availability:%s",
	   cgraph_function_flags_ready
	   ? cgraph_availability_names[cgraph_variable_initializer_availability (node)]
	   : "not-ready");
  if (DECL_ASSEMBLER_NAME_SET_P (node->decl))
    fprintf (f, " (asm: %s)", IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (node->decl)));
  if (DECL_INITIAL (node->decl))
    fprintf (f, " initialized");
  if (TREE_ASM_WRITTEN (node->decl))
    fprintf (f, " (asm written)");
  if (node->needed)
    fprintf (f, " needed");
  if (node->analyzed)
    fprintf (f, " analyzed");
  if (node->finalized)
    fprintf (f, " finalized");
  if (node->output)
    fprintf (f, " output");
  if (node->externally_visible)
    fprintf (f, " externally_visible");
  if (node->resolution != LDPR_UNKNOWN)
    fprintf (f, " %s",
 	     ld_plugin_symbol_resolution_names[(int)node->resolution]);
  if (node->in_other_partition)
    fprintf (f, " in_other_partition");
  else if (node->used_from_other_partition)
    fprintf (f, " used_from_other_partition");
  fprintf (f, "\n");
  fprintf (f, "  References: ");
  ipa_dump_references (f, &node->ref_list);
  fprintf (f, "  Refering this var: ");
  ipa_dump_refering (f, &node->ref_list);
}

/* Dump the variable pool.  */
void
dump_varpool (FILE *f)
{
  struct varpool_node *node;

  fprintf (f, "variable pool:\n\n");
  for (node = varpool_nodes; node; node = node->next)
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
  struct varpool_node *node;

  for (node = varpool_nodes; node ; node = node->next)
    if (decl_assembler_name_equal (node->decl, asmname))
      return node;

  return NULL;
}

/* Helper function for finalization code - add node into lists so it will
   be analyzed and compiled.  */
static void
varpool_enqueue_needed_node (struct varpool_node *node)
{
  if (varpool_last_needed_node)
    {
      varpool_last_needed_node->next_needed = node;
      node->prev_needed = varpool_last_needed_node;
    }
  varpool_last_needed_node = node;
  node->next_needed = NULL;
  if (!varpool_nodes_queue)
    varpool_nodes_queue = node;
  if (!varpool_first_unanalyzed_node)
    varpool_first_unanalyzed_node = node;
  notice_global_symbol (node->decl);
}

/* Notify finalize_compilation_unit that given node is reachable
   or needed.  */
void
varpool_mark_needed_node (struct varpool_node *node)
{
  if (node->alias && node->extra_name)
    node = node->extra_name;
  if (!node->needed && node->finalized
      && !TREE_ASM_WRITTEN (node->decl))
    varpool_enqueue_needed_node (node);
  node->needed = 1;
}

/* Reset the queue of needed nodes.  */
void
varpool_reset_queue (void)
{
  varpool_last_needed_node = NULL;
  varpool_nodes_queue = NULL;
  varpool_first_unanalyzed_node = NULL;
}

/* Determine if variable DECL is needed.  That is, visible to something
   either outside this translation unit, something magic in the system
   configury */
bool
decide_is_variable_needed (struct varpool_node *node, tree decl)
{
  /* If the user told us it is used, then it must be so.  */
  if (node->force_output)
    return true;

  gcc_assert (!DECL_EXTERNAL (decl));

  /* Externally visible variables must be output.  The exception is
     COMDAT variables that must be output only when they are needed.  */
  if (TREE_PUBLIC (decl)
      && !DECL_COMDAT (decl)
      && !DECL_EXTERNAL (decl))
    return true;

  /* When not reordering top level variables, we have to assume that
     we are going to keep everything.  */
  if (!flag_toplevel_reorder)
    return true;
  return false;
}

/* Return if DECL is constant and its initial value is known (so we can do
   constant folding using DECL_INITIAL (decl)).  */

bool
const_value_known_p (tree decl)
{
  if (TREE_CODE (decl) != VAR_DECL
      &&TREE_CODE (decl) != CONST_DECL)
    return false;

  if (TREE_CODE (decl) == CONST_DECL
      || DECL_IN_CONSTANT_POOL (decl))
    return true;

  gcc_assert (TREE_CODE (decl) == VAR_DECL);

  if (!TREE_READONLY (decl) || TREE_THIS_VOLATILE (decl))
    return false;

  /* Gimplifier takes away constructors of local vars  */
  if (!TREE_STATIC (decl) && !DECL_EXTERNAL (decl))
    return DECL_INITIAL (decl) != NULL;

  gcc_assert (TREE_STATIC (decl) || DECL_EXTERNAL (decl));

  /* Variables declared 'const' without an initializer
     have zero as the initializer if they may not be
     overridden at link or run time.  */
  if (!DECL_INITIAL (decl)
      && (DECL_EXTERNAL (decl)
	  || decl_replaceable_p (decl)))
    return false;

  /* Variables declared `const' with an initializer are considered
     to not be overwritable with different initializer by default. 

     ??? Previously we behaved so for scalar variables but not for array
     accesses.  */
  return true;
}

/* Mark DECL as finalized.  By finalizing the declaration, frontend instruct the
   middle end to output the variable to asm file, if needed or externally
   visible.  */
void
varpool_finalize_decl (tree decl)
{
  struct varpool_node *node = varpool_node (decl);

  gcc_assert (TREE_STATIC (decl));

  /* The first declaration of a variable that comes through this function
     decides whether it is global (in C, has external linkage)
     or local (in C, has internal linkage).  So do nothing more
     if this function has already run.  */
  if (node->finalized)
    {
      if (cgraph_global_info_ready)
	varpool_assemble_pending_decls ();
      return;
    }
  if (node->needed)
    varpool_enqueue_needed_node (node);
  node->finalized = true;
  if (TREE_THIS_VOLATILE (decl) || DECL_PRESERVE_P (decl))
    node->force_output = true;

  if (decide_is_variable_needed (node, decl))
    varpool_mark_needed_node (node);
  if (cgraph_global_info_ready)
    varpool_assemble_pending_decls ();
}

/* Return variable availability.  See cgraph.h for description of individual
   return values.  */
enum availability
cgraph_variable_initializer_availability (struct varpool_node *node)
{
  gcc_assert (cgraph_function_flags_ready);
  if (!node->finalized)
    return AVAIL_NOT_AVAILABLE;
  if (!TREE_PUBLIC (node->decl))
    return AVAIL_AVAILABLE;
  /* If the variable can be overwritten, return OVERWRITABLE.  Takes
     care of at least two notable extensions - the COMDAT variables
     used to share template instantiations in C++.  */
  if (!decl_replaceable_p (node->decl))
    return AVAIL_OVERWRITABLE;
  return AVAIL_AVAILABLE;
}

/* Walk the decls we marked as necessary and see if they reference new
   variables or functions and add them into the worklists.  */
bool
varpool_analyze_pending_decls (void)
{
  bool changed = false;

  timevar_push (TV_VARPOOL);
  while (varpool_first_unanalyzed_node)
    {
      struct varpool_node *node = varpool_first_unanalyzed_node, *next;
      tree decl = node->decl;
      bool analyzed = node->analyzed;

      varpool_first_unanalyzed_node->analyzed = true;

      varpool_first_unanalyzed_node = varpool_first_unanalyzed_node->next_needed;

      /* When reading back varpool at LTO time, we re-construct the queue in order
         to have "needed" list right by inserting all needed nodes into varpool.
	 We however don't want to re-analyze already analyzed nodes.  */
      if (!analyzed)
	{
	  gcc_assert (!in_lto_p || cgraph_function_flags_ready);
          /* Compute the alignment early so function body expanders are
	     already informed about increased alignment.  */
          align_variable (decl, 0);
	}
      if (DECL_INITIAL (decl))
	record_references_in_initializer (decl, analyzed);
      if (node->same_comdat_group)
	{
	  for (next = node->same_comdat_group;
	       next != node;
	       next = next->same_comdat_group)
	    varpool_mark_needed_node (next);
	}
      changed = true;
    }
  timevar_pop (TV_VARPOOL);
  return changed;
}

/* Output one variable, if necessary.  Return whether we output it.  */
bool
varpool_assemble_decl (struct varpool_node *node)
{
  tree decl = node->decl;

  if (!TREE_ASM_WRITTEN (decl)
      && !node->alias
      && !node->in_other_partition
      && !DECL_EXTERNAL (decl)
      && (TREE_CODE (decl) != VAR_DECL || !DECL_HAS_VALUE_EXPR_P (decl)))
    {
      assemble_variable (decl, 0, 1, 0);
      if (TREE_ASM_WRITTEN (decl))
	{
	  struct varpool_node *alias;

	  node->next_needed = varpool_assembled_nodes_queue;
	  node->prev_needed = NULL;
	  if (varpool_assembled_nodes_queue)
	    varpool_assembled_nodes_queue->prev_needed = node;
	  varpool_assembled_nodes_queue = node;
	  node->finalized = 1;

	  /* Also emit any extra name aliases.  */
	  for (alias = node->extra_name; alias; alias = alias->next)
	    {
	      /* Update linkage fields in case they've changed.  */
	      DECL_WEAK (alias->decl) = DECL_WEAK (decl);
	      TREE_PUBLIC (alias->decl) = TREE_PUBLIC (decl);
	      DECL_VISIBILITY (alias->decl) = DECL_VISIBILITY (decl);
	      assemble_alias (alias->decl, DECL_ASSEMBLER_NAME (decl));
	    }

	  return true;
	}
    }

  return false;
}

/* Optimization of function bodies might've rendered some variables as
   unnecessary so we want to avoid these from being compiled.

   This is done by pruning the queue and keeping only the variables that
   really appear needed (ie they are either externally visible or referenced
   by compiled function). Re-doing the reachability analysis on variables
   brings back the remaining variables referenced by these.  */
void
varpool_remove_unreferenced_decls (void)
{
  struct varpool_node *next, *node = varpool_nodes_queue;

  varpool_reset_queue ();

  if (seen_error ())
    return;

  while (node)
    {
      next = node->next_needed;
      node->needed = 0;

      if (node->analyzed
	  && (!varpool_can_remove_if_no_refs (node)
	      /* We just expanded all function bodies.  See if any of
		 them needed the variable.  */
	      || DECL_RTL_SET_P (node->decl)))
	varpool_mark_needed_node (node);

      node = next;
    }
  /* Make sure we mark alias targets as used targets.  */
  finish_aliases_1 ();
  varpool_analyze_pending_decls ();
}

/* For variables in named sections make sure get_variable_section
   is called before we switch to those sections.  Then section
   conflicts between read-only and read-only requiring relocations
   sections can be resolved.  */
void
varpool_finalize_named_section_flags (struct varpool_node *node)
{
  if (!TREE_ASM_WRITTEN (node->decl)
      && !node->alias
      && !node->in_other_partition
      && !DECL_EXTERNAL (node->decl)
      && TREE_CODE (node->decl) == VAR_DECL
      && !DECL_HAS_VALUE_EXPR_P (node->decl)
      && DECL_SECTION_NAME (node->decl))
    get_variable_section (node->decl, false);
}

/* Output all variables enqueued to be assembled.  */
bool
varpool_assemble_pending_decls (void)
{
  bool changed = false;
  struct varpool_node *node;

  if (seen_error ())
    return false;

  timevar_push (TV_VAROUT);
  /* EH might mark decls as needed during expansion.  This should be safe since
     we don't create references to new function, but it should not be used
     elsewhere.  */
  varpool_analyze_pending_decls ();

  for (node = varpool_nodes_queue; node; node = node->next_needed)
    varpool_finalize_named_section_flags (node);

  while (varpool_nodes_queue)
    {
      struct varpool_node *node = varpool_nodes_queue;

      varpool_nodes_queue = varpool_nodes_queue->next_needed;
      if (varpool_assemble_decl (node))
	changed = true;
      else
	{
	  node->prev_needed = NULL;
          node->next_needed = NULL;
	}
    }
  /* varpool_nodes_queue is now empty, clear the pointer to the last element
     in the queue.  */
  varpool_last_needed_node = NULL;
  timevar_pop (TV_VAROUT);
  return changed;
}

/* Remove all elements from the queue so we can re-use it for debug output.  */
void
varpool_empty_needed_queue (void)
{
  /* EH might mark decls as needed during expansion.  This should be safe since
     we don't create references to new function, but it should not be used
     elsewhere.  */
  varpool_analyze_pending_decls ();

  while (varpool_nodes_queue)
    {
      struct varpool_node *node = varpool_nodes_queue;
      varpool_nodes_queue = varpool_nodes_queue->next_needed;
      node->next_needed = NULL;
      node->prev_needed = NULL;
    }
  /* varpool_nodes_queue is now empty, clear the pointer to the last element
     in the queue.  */
  varpool_last_needed_node = NULL;
}

/* Create a new global variable of type TYPE.  */
tree
add_new_static_var (tree type)
{
  tree new_decl;
  struct varpool_node *new_node;

  new_decl = create_tmp_var (type, NULL);
  DECL_NAME (new_decl) = create_tmp_var_name (NULL);
  TREE_READONLY (new_decl) = 0;
  TREE_STATIC (new_decl) = 1;
  TREE_USED (new_decl) = 1;
  DECL_CONTEXT (new_decl) = NULL_TREE;
  DECL_ABSTRACT (new_decl) = 0;
  lang_hooks.dup_lang_specific_decl (new_decl);
  create_var_ann (new_decl);
  new_node = varpool_node (new_decl);
  varpool_mark_needed_node (new_node);
  add_referenced_var (new_decl);
  varpool_finalize_decl (new_decl);

  return new_node->decl;
}

/* Attempt to mark ALIAS as an alias to DECL.  Return TRUE if successful.
   Extra name aliases are output whenever DECL is output.  */

struct varpool_node *
varpool_extra_name_alias (tree alias, tree decl)
{
  struct varpool_node key, *alias_node, *decl_node, **slot;

#ifndef ASM_OUTPUT_DEF
  /* If aliases aren't supported by the assembler, fail.  */
  return false;
#endif

  gcc_assert (TREE_CODE (decl) == VAR_DECL);
  gcc_assert (TREE_CODE (alias) == VAR_DECL);
  /* Make sure the hash table has been created.  */
  decl_node = varpool_node (decl);

  key.decl = alias;

  slot = (struct varpool_node **) htab_find_slot (varpool_hash, &key, INSERT);

  /* If the varpool_node has been already created, fail.  */
  if (*slot)
    return NULL;

  alias_node = ggc_alloc_cleared_varpool_node ();
  alias_node->decl = alias;
  alias_node->alias = 1;
  alias_node->extra_name = decl_node;
  alias_node->next = decl_node->extra_name;
  ipa_empty_ref_list (&alias_node->ref_list);
  if (decl_node->extra_name)
    decl_node->extra_name->prev = alias_node;
  decl_node->extra_name = alias_node;
  *slot = alias_node;
  return alias_node;
}

/* Return true when NODE is known to be used from other (non-LTO) object file.
   Known only when doing LTO via linker plugin.  */

bool
varpool_used_from_object_file_p (struct varpool_node *node)
{
  struct varpool_node *alias;

  if (!TREE_PUBLIC (node->decl))
    return false;
  if (resolution_used_from_other_file_p (node->resolution))
    return true;
  for (alias = node->extra_name; alias; alias = alias->next)
    if (TREE_PUBLIC (alias->decl)
	&& resolution_used_from_other_file_p (alias->resolution))
      return true;
  return false;
}

#include "gt-varpool.h"
