/* Callgraph handling code.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008
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
#include "diagnostic.h"
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

/* Return varpool node assigned to DECL.  Create new one when needed.  */
struct varpool_node *
varpool_node (tree decl)
{
  struct varpool_node key, *node, **slot;

  gcc_assert (DECL_P (decl) && TREE_CODE (decl) != FUNCTION_DECL);

  if (!varpool_hash)
    varpool_hash = htab_create_ggc (10, hash_varpool_node,
					   eq_varpool_node, NULL);
  key.decl = decl;
  slot = (struct varpool_node **)
    htab_find_slot (varpool_hash, &key, INSERT);
  if (*slot)
    return *slot;
  node = GGC_CNEW (struct varpool_node);
  node->decl = decl;
  node->order = cgraph_order++;
  node->next = varpool_nodes;
  varpool_nodes = node;
  *slot = node;
  return node;
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
  if (DECL_INITIAL (node->decl))
    fprintf (f, " initialized");
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
  fprintf (f, "\n");
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

void
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
    varpool_last_needed_node->next_needed = node;
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
static void
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
  if ((node->externally_visible && !DECL_COMDAT (decl))
      || node->force_output)
    return true;

  /* ??? If the assembler name is set by hand, it is possible to assemble
     the name later after finalizing the function and the fact is noticed
     in assemble_name then.  This is arguably a bug.  */
  if (DECL_ASSEMBLER_NAME_SET_P (decl)
      && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
    return true;

  /* Externally visible variables must be output.  The exception is
     COMDAT variables that must be output only when they are needed.  */
  if (TREE_PUBLIC (decl)
      && !flag_whole_program
      && !flag_lto
      && !flag_whopr
      && !DECL_COMDAT (decl)
      && !DECL_EXTERNAL (decl))
    return true;

  /* When emulating tls, we actually see references to the control
     variable, rather than the user-level variable.  */
  if (!targetm.have_tls
      && TREE_CODE (decl) == VAR_DECL
      && DECL_THREAD_LOCAL_P (decl))
    {
      tree control = emutls_decl (decl);
      if (decide_is_variable_needed (varpool_node (control), control))
	return true;
    }

  /* When not reordering top level variables, we have to assume that
     we are going to keep everything.  */
  if (flag_toplevel_reorder)
    return false;

  /* We want to emit COMDAT variables only when absolutely necessary.  */
  if (DECL_COMDAT (decl))
    return false;
  return true;
}

/* Mark DECL as finalized.  By finalizing the declaration, frontend instruct the
   middle end to output the variable to asm file, if needed or externally
   visible.  */
void
varpool_finalize_decl (tree decl)
{
  struct varpool_node *node = varpool_node (decl);

  /* FIXME: We don't really stream varpool datastructure and instead rebuild it
     by varpool_finalize_decl.  This is not quite correct since this way we can't
     attach any info to varpool.  Eventually we will want to stream varpool nodes
     and the flags.

     For the moment just prevent analysis of varpool nodes to happen again, so
     we will re-try to compute "address_taken" flag of varpool that breaks
     in presence of clones.  */
  if (in_lto_p)
    node->analyzed = true;

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

  if (decide_is_variable_needed (node, decl))
    varpool_mark_needed_node (node);
  /* Since we reclaim unreachable nodes at the end of every language
     level unit, we need to be conservative about possible entry points
     there.  */
  else if (TREE_PUBLIC (decl) && !DECL_COMDAT (decl) && !DECL_EXTERNAL (decl))
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
  if (!(*targetm.binds_local_p) (node->decl) && !DECL_COMDAT (node->decl))
    return AVAIL_OVERWRITABLE;
  return AVAIL_AVAILABLE;
}

/* Walk the decls we marked as necessary and see if they reference new
   variables or functions and add them into the worklists.  */
bool
varpool_analyze_pending_decls (void)
{
  bool changed = false;
  timevar_push (TV_CGRAPH);

  while (varpool_first_unanalyzed_node)
    {
      tree decl = varpool_first_unanalyzed_node->decl;
      bool analyzed = varpool_first_unanalyzed_node->analyzed;

      varpool_first_unanalyzed_node->analyzed = true;

      varpool_first_unanalyzed_node = varpool_first_unanalyzed_node->next_needed;

      /* When reading back varpool at LTO time, we re-construct the queue in order
         to have "needed" list right by inserting all needed nodes into varpool.
	 We however don't want to re-analyze already analyzed nodes.  */
      if (!analyzed)
	{
	  gcc_assert (!in_lto_p);
          /* Compute the alignment early so function body expanders are
	     already informed about increased alignment.  */
          align_variable (decl, 0);
	}
      if (DECL_INITIAL (decl))
	record_references_in_initializer (decl, analyzed);
      changed = true;
    }
  timevar_pop (TV_CGRAPH);
  return changed;
}

/* Output one variable, if necessary.  Return whether we output it.  */
bool
varpool_assemble_decl (struct varpool_node *node)
{
  tree decl = node->decl;

  if (!TREE_ASM_WRITTEN (decl)
      && !node->alias
      && !DECL_EXTERNAL (decl)
      && (TREE_CODE (decl) != VAR_DECL || !DECL_HAS_VALUE_EXPR_P (decl)))
    {
      assemble_variable (decl, 0, 1, 0);
      if (TREE_ASM_WRITTEN (decl))
	{
	  struct varpool_node *alias;

	  node->next_needed = varpool_assembled_nodes_queue;
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

  if (errorcount || sorrycount)
    return;

  while (node)
    {
      tree decl = node->decl;
      next = node->next_needed;
      node->needed = 0;

      if (node->finalized
	  && (decide_is_variable_needed (node, decl)
	      /* ??? Cgraph does not yet rule the world with an iron hand,
		 and does not control the emission of debug information.
		 After a variable has its DECL_RTL set, we must assume that
		 it may be referenced by the debug information, and we can
		 no longer elide it.  */
	      || DECL_RTL_SET_P (decl)))
	varpool_mark_needed_node (node);

      node = next;
    }
  /* Make sure we mark alias targets as used targets.  */
  finish_aliases_1 ();
  varpool_analyze_pending_decls ();
}

/* Output all variables enqueued to be assembled.  */
bool
varpool_assemble_pending_decls (void)
{
  bool changed = false;

  if (errorcount || sorrycount)
    return false;

  /* EH might mark decls as needed during expansion.  This should be safe since
     we don't create references to new function, but it should not be used
     elsewhere.  */
  varpool_analyze_pending_decls ();

  while (varpool_nodes_queue)
    {
      struct varpool_node *node = varpool_nodes_queue;

      varpool_nodes_queue = varpool_nodes_queue->next_needed;
      if (varpool_assemble_decl (node))
	changed = true;
      else
        node->next_needed = NULL;
    }
  /* varpool_nodes_queue is now empty, clear the pointer to the last element
     in the queue.  */
  varpool_last_needed_node = NULL;
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

bool
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
    return false;

  alias_node = GGC_CNEW (struct varpool_node);
  alias_node->decl = alias;
  alias_node->alias = 1;
  alias_node->extra_name = decl_node;
  alias_node->next = decl_node->extra_name;
  decl_node->extra_name = alias_node;
  *slot = alias_node;
  return true;
}

#include "gt-varpool.h"
