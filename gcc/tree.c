/* Language-independent node constructors for parse phase of GNU compiler.
   Copyright (C) 1987, 88, 92-97, 1998 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* This file contains the low level primitives for operating on tree nodes,
   including allocation, list operations, interning of identifiers,
   construction of data type nodes and statement nodes,
   and construction of type conversion nodes.  It also contains
   tables index by tree code that describe how to take apart
   nodes of that code.

   It is intended to be language-independent, but occasionally
   calls language-dependent routines defined (for C) in typecheck.c.

   The low-level allocation routines oballoc and permalloc
   are used also for allocating many other kinds of objects
   by all passes of the compiler.  */

#include "config.h"
#include <setjmp.h>
#include "flags.h"
#include "tree.h"
#include "except.h"
#include "function.h"
#include "obstack.h"
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* Tree nodes of permanent duration are allocated in this obstack.
   They are the identifier nodes, and everything outside of
   the bodies and parameters of function definitions.  */

struct obstack permanent_obstack;

/* The initial RTL, and all ..._TYPE nodes, in a function
   are allocated in this obstack.  Usually they are freed at the
   end of the function, but if the function is inline they are saved.
   For top-level functions, this is maybepermanent_obstack.
   Separate obstacks are made for nested functions.  */

struct obstack *function_maybepermanent_obstack;

/* This is the function_maybepermanent_obstack for top-level functions.  */

struct obstack maybepermanent_obstack;

/* This is a list of function_maybepermanent_obstacks for top-level inline
   functions that are compiled in the middle of compiling other functions.  */

struct simple_obstack_stack *toplev_inline_obstacks;

/* Former elements of toplev_inline_obstacks that have been recycled.  */

struct simple_obstack_stack *extra_inline_obstacks;

/* This is a list of function_maybepermanent_obstacks for inline functions
   nested in the current function that were compiled in the middle of
   compiling other functions.  */

struct simple_obstack_stack *inline_obstacks;

/* The contents of the current function definition are allocated
   in this obstack, and all are freed at the end of the function.
   For top-level functions, this is temporary_obstack.
   Separate obstacks are made for nested functions.  */

struct obstack *function_obstack;

/* This is used for reading initializers of global variables.  */

struct obstack temporary_obstack;

/* The tree nodes of an expression are allocated
   in this obstack, and all are freed at the end of the expression.  */

struct obstack momentary_obstack;

/* The tree nodes of a declarator are allocated
   in this obstack, and all are freed when the declarator
   has been parsed.  */

static struct obstack temp_decl_obstack;

/* This points at either permanent_obstack
   or the current function_maybepermanent_obstack.  */

struct obstack *saveable_obstack;

/* This is same as saveable_obstack during parse and expansion phase;
   it points to the current function's obstack during optimization.
   This is the obstack to be used for creating rtl objects.  */

struct obstack *rtl_obstack;

/* This points at either permanent_obstack or the current function_obstack.  */

struct obstack *current_obstack;

/* This points at either permanent_obstack or the current function_obstack
   or momentary_obstack.  */

struct obstack *expression_obstack;

/* Stack of obstack selections for push_obstacks and pop_obstacks.  */

struct obstack_stack
{
  struct obstack_stack *next;
  struct obstack *current;
  struct obstack *saveable;
  struct obstack *expression;
  struct obstack *rtl;
};

struct obstack_stack *obstack_stack;

/* Obstack for allocating struct obstack_stack entries.  */

static struct obstack obstack_stack_obstack;

/* Addresses of first objects in some obstacks.
   This is for freeing their entire contents.  */
char *maybepermanent_firstobj;
char *temporary_firstobj;
char *momentary_firstobj;
char *temp_decl_firstobj;

/* This is used to preserve objects (mainly array initializers) that need to
   live until the end of the current function, but no further.  */
char *momentary_function_firstobj;

/* Nonzero means all ..._TYPE nodes should be allocated permanently.  */

int all_types_permanent;

/* Stack of places to restore the momentary obstack back to.  */
   
struct momentary_level
{
  /* Pointer back to previous such level.  */
  struct momentary_level *prev;
  /* First object allocated within this level.  */
  char *base;
  /* Value of expression_obstack saved at entry to this level.  */
  struct obstack *obstack;
};

struct momentary_level *momentary_stack;

/* Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r, <, 1, 2 and e.  See tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

char *standard_tree_code_type[] = {
#include "tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

int standard_tree_code_length[] = {
#include "tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

char *standard_tree_code_name[] = {
#include "tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r, e, <, 1 and 2.  See tree.def for details.  */

char **tree_code_type;

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

int *tree_code_length;

/* Table indexed by tree code giving name of tree code, as a string.  */

char **tree_code_name;

/* Statistics-gathering stuff.  */
typedef enum
{
  d_kind,
  t_kind,
  b_kind,
  s_kind,
  r_kind,
  e_kind,
  c_kind,
  id_kind,
  op_id_kind,
  perm_list_kind,
  temp_list_kind,
  vec_kind,
  x_kind,
  lang_decl,
  lang_type,
  all_kinds
} tree_node_kind;

int tree_node_counts[(int)all_kinds];
int tree_node_sizes[(int)all_kinds];
int id_string_size = 0;

char *tree_node_kind_names[] = {
  "decls",
  "types",
  "blocks",
  "stmts",
  "refs",
  "exprs",
  "constants",
  "identifiers",
  "op_identifiers",
  "perm_tree_lists",
  "temp_tree_lists",
  "vecs",
  "random kinds",
  "lang_decl kinds",
  "lang_type kinds"
};

/* Hash table for uniquizing IDENTIFIER_NODEs by name.  */

#define MAX_HASH_TABLE 1009
static tree hash_table[MAX_HASH_TABLE];	/* id hash buckets */

/* 0 while creating built-in identifiers.  */
static int do_identifier_warnings;

/* Unique id for next decl created.  */
static int next_decl_uid;
/* Unique id for next type created.  */
static int next_type_uid = 1;

/* Here is how primitive or already-canonicalized types' hash
   codes are made.  */
#define TYPE_HASH(TYPE) ((HOST_WIDE_INT) (TYPE) & 0777777)

extern char *mode_name[];

void gcc_obstack_init ();

/* Init the principal obstacks.  */

void
init_obstacks ()
{
  gcc_obstack_init (&obstack_stack_obstack);
  gcc_obstack_init (&permanent_obstack);

  gcc_obstack_init (&temporary_obstack);
  temporary_firstobj = (char *) obstack_alloc (&temporary_obstack, 0);
  gcc_obstack_init (&momentary_obstack);
  momentary_firstobj = (char *) obstack_alloc (&momentary_obstack, 0);
  momentary_function_firstobj = momentary_firstobj;
  gcc_obstack_init (&maybepermanent_obstack);
  maybepermanent_firstobj
    = (char *) obstack_alloc (&maybepermanent_obstack, 0);
  gcc_obstack_init (&temp_decl_obstack);
  temp_decl_firstobj = (char *) obstack_alloc (&temp_decl_obstack, 0);

  function_obstack = &temporary_obstack;
  function_maybepermanent_obstack = &maybepermanent_obstack;
  current_obstack = &permanent_obstack;
  expression_obstack = &permanent_obstack;
  rtl_obstack = saveable_obstack = &permanent_obstack;

  /* Init the hash table of identifiers.  */
  bzero ((char *) hash_table, sizeof hash_table);
}

void
gcc_obstack_init (obstack)
     struct obstack *obstack;
{
  /* Let particular systems override the size of a chunk.  */
#ifndef OBSTACK_CHUNK_SIZE
#define OBSTACK_CHUNK_SIZE 0
#endif
  /* Let them override the alloc and free routines too.  */
#ifndef OBSTACK_CHUNK_ALLOC
#define OBSTACK_CHUNK_ALLOC xmalloc
#endif
#ifndef OBSTACK_CHUNK_FREE
#define OBSTACK_CHUNK_FREE free
#endif
  _obstack_begin (obstack, OBSTACK_CHUNK_SIZE, 0,
		  (void *(*) ()) OBSTACK_CHUNK_ALLOC,
		  (void (*) ()) OBSTACK_CHUNK_FREE);
}

/* Save all variables describing the current status into the structure *P.
   This is used before starting a nested function.

   CONTEXT is the decl_function_context for the function we're about to
   compile; if it isn't current_function_decl, we have to play some games.  */

void
save_tree_status (p, context)
     struct function *p;
     tree context;
{
  p->all_types_permanent = all_types_permanent;
  p->momentary_stack = momentary_stack;
  p->maybepermanent_firstobj = maybepermanent_firstobj;
  p->temporary_firstobj = temporary_firstobj;
  p->momentary_firstobj = momentary_firstobj;
  p->momentary_function_firstobj = momentary_function_firstobj;
  p->function_obstack = function_obstack;
  p->function_maybepermanent_obstack = function_maybepermanent_obstack;
  p->current_obstack = current_obstack;
  p->expression_obstack = expression_obstack;
  p->saveable_obstack = saveable_obstack;
  p->rtl_obstack = rtl_obstack;
  p->inline_obstacks = inline_obstacks;

  if (context == current_function_decl)
    /* Objects that need to be saved in this function can be in the nonsaved
       obstack of the enclosing function since they can't possibly be needed
       once it has returned.  */
    function_maybepermanent_obstack = function_obstack;
  else
    {
      /* We're compiling a function which isn't nested in the current
         function.  We need to create a new maybepermanent_obstack for this
         function, since it can't go onto any of the existing obstacks.  */
      struct simple_obstack_stack **head;
      struct simple_obstack_stack *current;

      if (context == NULL_TREE)
	head = &toplev_inline_obstacks;
      else
	{
	  struct function *f = find_function_data (context);
	  head = &f->inline_obstacks;
	}

      if (context == NULL_TREE && extra_inline_obstacks)
	{
	  current = extra_inline_obstacks;
	  extra_inline_obstacks = current->next;
	}
      else
	{
	  current = ((struct simple_obstack_stack *)
		     xmalloc (sizeof (struct simple_obstack_stack)));

	  current->obstack
	    = (struct obstack *) xmalloc (sizeof (struct obstack));
	  gcc_obstack_init (current->obstack);
	}

      function_maybepermanent_obstack = current->obstack;

      current->next = *head;
      *head = current;
    }      

  maybepermanent_firstobj
    = (char *) obstack_finish (function_maybepermanent_obstack);

  function_obstack = (struct obstack *) xmalloc (sizeof (struct obstack));
  gcc_obstack_init (function_obstack);

  current_obstack = &permanent_obstack;
  expression_obstack = &permanent_obstack;
  rtl_obstack = saveable_obstack = &permanent_obstack;

  temporary_firstobj = (char *) obstack_alloc (&temporary_obstack, 0);
  momentary_firstobj = (char *) obstack_finish (&momentary_obstack);
  momentary_function_firstobj = momentary_firstobj;
}

/* Restore all variables describing the current status from the structure *P.
   This is used after a nested function.  */

void
restore_tree_status (p, context)
     struct function *p;
     tree context;
{
  all_types_permanent = p->all_types_permanent;
  momentary_stack = p->momentary_stack;

  obstack_free (&momentary_obstack, momentary_function_firstobj);

  /* Free saveable storage used by the function just compiled and not
     saved.

     CAUTION: This is in function_obstack of the containing function.
     So we must be sure that we never allocate from that obstack during
     the compilation of a nested function if we expect it to survive
     past the nested function's end.  */
  obstack_free (function_maybepermanent_obstack, maybepermanent_firstobj);

  /* If we were compiling a toplevel function, we can free this space now.  */
  if (context == NULL_TREE)
    {
      obstack_free (&temporary_obstack, temporary_firstobj);
      obstack_free (&momentary_obstack, momentary_function_firstobj);
    }

  /* If we were compiling a toplevel function that we don't actually want
     to save anything from, return the obstack to the pool.  */
  if (context == NULL_TREE
      && obstack_empty_p (function_maybepermanent_obstack))
    {
      struct simple_obstack_stack *current, **p = &toplev_inline_obstacks;

      while ((*p)->obstack != function_maybepermanent_obstack)
	p = &((*p)->next);
      current = *p;
      *p = current->next;

      current->next = extra_inline_obstacks;
      extra_inline_obstacks = current;
    }

  obstack_free (function_obstack, 0);
  free (function_obstack);

  temporary_firstobj = p->temporary_firstobj;
  momentary_firstobj = p->momentary_firstobj;
  momentary_function_firstobj = p->momentary_function_firstobj;
  maybepermanent_firstobj = p->maybepermanent_firstobj;
  function_obstack = p->function_obstack;
  function_maybepermanent_obstack = p->function_maybepermanent_obstack;
  current_obstack = p->current_obstack;
  expression_obstack = p->expression_obstack;
  saveable_obstack = p->saveable_obstack;
  rtl_obstack = p->rtl_obstack;
  inline_obstacks = p->inline_obstacks;
}

/* Start allocating on the temporary (per function) obstack.
   This is done in start_function before parsing the function body,
   and before each initialization at top level, and to go back
   to temporary allocation after doing permanent_allocation.  */

void
temporary_allocation ()
{
  /* Note that function_obstack at top level points to temporary_obstack.
     But within a nested function context, it is a separate obstack.  */
  current_obstack = function_obstack;
  expression_obstack = function_obstack;
  rtl_obstack = saveable_obstack = function_maybepermanent_obstack;
  momentary_stack = 0;
  inline_obstacks = 0;
}

/* Start allocating on the permanent obstack but don't
   free the temporary data.  After calling this, call
   `permanent_allocation' to fully resume permanent allocation status.  */

void
end_temporary_allocation ()
{
  current_obstack = &permanent_obstack;
  expression_obstack = &permanent_obstack;
  rtl_obstack = saveable_obstack = &permanent_obstack;
}

/* Resume allocating on the temporary obstack, undoing
   effects of `end_temporary_allocation'.  */

void
resume_temporary_allocation ()
{
  current_obstack = function_obstack;
  expression_obstack = function_obstack;
  rtl_obstack = saveable_obstack = function_maybepermanent_obstack;
}

/* While doing temporary allocation, switch to allocating in such a
   way as to save all nodes if the function is inlined.  Call
   resume_temporary_allocation to go back to ordinary temporary
   allocation.  */

void
saveable_allocation ()
{
  /* Note that function_obstack at top level points to temporary_obstack.
     But within a nested function context, it is a separate obstack.  */
  expression_obstack = current_obstack = saveable_obstack;
}

/* Switch to current obstack CURRENT and maybepermanent obstack SAVEABLE,
   recording the previously current obstacks on a stack.
   This does not free any storage in any obstack.  */

void
push_obstacks (current, saveable)
     struct obstack *current, *saveable;
{
  struct obstack_stack *p
    = (struct obstack_stack *) obstack_alloc (&obstack_stack_obstack,
					      (sizeof (struct obstack_stack)));

  p->current = current_obstack;
  p->saveable = saveable_obstack;
  p->expression = expression_obstack;
  p->rtl = rtl_obstack;
  p->next = obstack_stack;
  obstack_stack = p;

  current_obstack = current;
  expression_obstack = current;
  rtl_obstack = saveable_obstack = saveable;
}

/* Save the current set of obstacks, but don't change them.  */

void
push_obstacks_nochange ()
{
  struct obstack_stack *p
    = (struct obstack_stack *) obstack_alloc (&obstack_stack_obstack,
					      (sizeof (struct obstack_stack)));

  p->current = current_obstack;
  p->saveable = saveable_obstack;
  p->expression = expression_obstack;
  p->rtl = rtl_obstack;
  p->next = obstack_stack;
  obstack_stack = p;
}

/* Pop the obstack selection stack.  */

void
pop_obstacks ()
{
  struct obstack_stack *p = obstack_stack;
  obstack_stack = p->next;

  current_obstack = p->current;
  saveable_obstack = p->saveable;
  expression_obstack = p->expression;
  rtl_obstack = p->rtl;

  obstack_free (&obstack_stack_obstack, p);
}

/* Nonzero if temporary allocation is currently in effect.
   Zero if currently doing permanent allocation.  */

int
allocation_temporary_p ()
{
  return current_obstack != &permanent_obstack;
}

/* Go back to allocating on the permanent obstack
   and free everything in the temporary obstack.

   FUNCTION_END is true only if we have just finished compiling a function.
   In that case, we also free preserved initial values on the momentary
   obstack.  */

void
permanent_allocation (function_end)
     int function_end;
{
  /* Free up previous temporary obstack data */
  obstack_free (&temporary_obstack, temporary_firstobj);
  if (function_end)
    {
      obstack_free (&momentary_obstack, momentary_function_firstobj);
      momentary_firstobj = momentary_function_firstobj;
    }
  else
    obstack_free (&momentary_obstack, momentary_firstobj);
  obstack_free (function_maybepermanent_obstack, maybepermanent_firstobj);
  obstack_free (&temp_decl_obstack, temp_decl_firstobj);

  /* Free up the maybepermanent_obstacks for any of our nested functions
     which were compiled at a lower level.  */
  while (inline_obstacks)
    {
      struct simple_obstack_stack *current = inline_obstacks;
      inline_obstacks = current->next;
      obstack_free (current->obstack, 0);
      free (current->obstack);
      free (current);
    }

  current_obstack = &permanent_obstack;
  expression_obstack = &permanent_obstack;
  rtl_obstack = saveable_obstack = &permanent_obstack;
}

/* Save permanently everything on the maybepermanent_obstack.  */

void
preserve_data ()
{
  maybepermanent_firstobj
    = (char *) obstack_alloc (function_maybepermanent_obstack, 0);
}

void
preserve_initializer ()
{
  struct momentary_level *tem;
  char *old_momentary;

  temporary_firstobj
    = (char *) obstack_alloc (&temporary_obstack, 0);
  maybepermanent_firstobj
    = (char *) obstack_alloc (function_maybepermanent_obstack, 0);

  old_momentary = momentary_firstobj;
  momentary_firstobj
    = (char *) obstack_alloc (&momentary_obstack, 0);
  if (momentary_firstobj != old_momentary)
    for (tem = momentary_stack; tem; tem = tem->prev)
      tem->base = momentary_firstobj;
}

/* Start allocating new rtl in current_obstack.
   Use resume_temporary_allocation
   to go back to allocating rtl in saveable_obstack.  */

void
rtl_in_current_obstack ()
{
  rtl_obstack = current_obstack;
}

/* Start allocating rtl from saveable_obstack.  Intended to be used after
   a call to push_obstacks_nochange.  */

void
rtl_in_saveable_obstack ()
{
  rtl_obstack = saveable_obstack;
}

/* Allocate SIZE bytes in the current obstack
   and return a pointer to them.
   In practice the current obstack is always the temporary one.  */

char *
oballoc (size)
     int size;
{
  return (char *) obstack_alloc (current_obstack, size);
}

/* Free the object PTR in the current obstack
   as well as everything allocated since PTR.
   In practice the current obstack is always the temporary one.  */

void
obfree (ptr)
     char *ptr;
{
  obstack_free (current_obstack, ptr);
}

/* Allocate SIZE bytes in the permanent obstack
   and return a pointer to them.  */

char *
permalloc (size)
     int size;
{
  return (char *) obstack_alloc (&permanent_obstack, size);
}

/* Allocate NELEM items of SIZE bytes in the permanent obstack
   and return a pointer to them.  The storage is cleared before
   returning the value.  */

char *
perm_calloc (nelem, size)
     int nelem;
     long size;
{
  char *rval = (char *) obstack_alloc (&permanent_obstack, nelem * size);
  bzero (rval, nelem * size);
  return rval;
}

/* Allocate SIZE bytes in the saveable obstack
   and return a pointer to them.  */

char *
savealloc (size)
     int size;
{
  return (char *) obstack_alloc (saveable_obstack, size);
}

/* Allocate SIZE bytes in the expression obstack
   and return a pointer to them.  */

char *
expralloc (size)
     int size;
{
  return (char *) obstack_alloc (expression_obstack, size);
}

/* Print out which obstack an object is in.  */

void
print_obstack_name (object, file, prefix)
     char *object;
     FILE *file;
     char *prefix;
{
  struct obstack *obstack = NULL;
  char *obstack_name = NULL;
  struct function *p;

  for (p = outer_function_chain; p; p = p->next)
    {
      if (_obstack_allocated_p (p->function_obstack, object))
	{
	  obstack = p->function_obstack;
	  obstack_name = "containing function obstack";
	}
      if (_obstack_allocated_p (p->function_maybepermanent_obstack, object))
	{
	  obstack = p->function_maybepermanent_obstack;
	  obstack_name = "containing function maybepermanent obstack";
	}
    }

  if (_obstack_allocated_p (&obstack_stack_obstack, object))
    {
      obstack = &obstack_stack_obstack;
      obstack_name = "obstack_stack_obstack";
    }
  else if (_obstack_allocated_p (function_obstack, object))
    {
      obstack = function_obstack;
      obstack_name = "function obstack";
    }
  else if (_obstack_allocated_p (&permanent_obstack, object))
    {
      obstack = &permanent_obstack;
      obstack_name = "permanent_obstack";
    }
  else if (_obstack_allocated_p (&momentary_obstack, object))
    {
      obstack = &momentary_obstack;
      obstack_name = "momentary_obstack";
    }
  else if (_obstack_allocated_p (function_maybepermanent_obstack, object))
    {
      obstack = function_maybepermanent_obstack;
      obstack_name = "function maybepermanent obstack";
    }
  else if (_obstack_allocated_p (&temp_decl_obstack, object))
    {
      obstack = &temp_decl_obstack;
      obstack_name = "temp_decl_obstack";
    }

  /* Check to see if the object is in the free area of the obstack.  */
  if (obstack != NULL)
    {
      if (object >= obstack->next_free
	  && object < obstack->chunk_limit)
	fprintf (file, "%s in free portion of obstack %s",
		 prefix, obstack_name);
      else
	fprintf (file, "%s allocated from %s", prefix, obstack_name);
    }
  else
    fprintf (file, "%s not allocated from any obstack", prefix);
}

void
debug_obstack (object)
     char *object;
{
  print_obstack_name (object, stderr, "object");
  fprintf (stderr, ".\n");
}

/* Return 1 if OBJ is in the permanent obstack.
   This is slow, and should be used only for debugging.
   Use TREE_PERMANENT for other purposes.  */

int
object_permanent_p (obj)
     tree obj;
{
  return _obstack_allocated_p (&permanent_obstack, obj);
}

/* Start a level of momentary allocation.
   In C, each compound statement has its own level
   and that level is freed at the end of each statement.
   All expression nodes are allocated in the momentary allocation level.  */

void
push_momentary ()
{
  struct momentary_level *tem
    = (struct momentary_level *) obstack_alloc (&momentary_obstack,
						sizeof (struct momentary_level));
  tem->prev = momentary_stack;
  tem->base = (char *) obstack_base (&momentary_obstack);
  tem->obstack = expression_obstack;
  momentary_stack = tem;
  expression_obstack = &momentary_obstack;
}

/* Set things up so the next clear_momentary will only clear memory
   past our present position in momentary_obstack.  */

void
preserve_momentary ()
{
  momentary_stack->base = (char *) obstack_base (&momentary_obstack);
}

/* Free all the storage in the current momentary-allocation level.
   In C, this happens at the end of each statement.  */

void
clear_momentary ()
{
  obstack_free (&momentary_obstack, momentary_stack->base);
}

/* Discard a level of momentary allocation.
   In C, this happens at the end of each compound statement.
   Restore the status of expression node allocation
   that was in effect before this level was created.  */

void
pop_momentary ()
{
  struct momentary_level *tem = momentary_stack;
  momentary_stack = tem->prev;
  expression_obstack = tem->obstack;
  /* We can't free TEM from the momentary_obstack, because there might
     be objects above it which have been saved.  We can free back to the
     stack of the level we are popping off though.  */
  obstack_free (&momentary_obstack, tem->base);
}

/* Pop back to the previous level of momentary allocation,
   but don't free any momentary data just yet.  */

void
pop_momentary_nofree ()
{
  struct momentary_level *tem = momentary_stack;
  momentary_stack = tem->prev;
  expression_obstack = tem->obstack;
}

/* Call when starting to parse a declaration:
   make expressions in the declaration last the length of the function.
   Returns an argument that should be passed to resume_momentary later.  */

int
suspend_momentary ()
{
  register int tem = expression_obstack == &momentary_obstack;
  expression_obstack = saveable_obstack;
  return tem;
}

/* Call when finished parsing a declaration:
   restore the treatment of node-allocation that was
   in effect before the suspension.
   YES should be the value previously returned by suspend_momentary.  */

void
resume_momentary (yes)
     int yes;
{
  if (yes)
    expression_obstack = &momentary_obstack;
}

/* Init the tables indexed by tree code.
   Note that languages can add to these tables to define their own codes.  */

void
init_tree_codes ()
{
  tree_code_type = (char **) xmalloc (sizeof (standard_tree_code_type));
  tree_code_length = (int *) xmalloc (sizeof (standard_tree_code_length));
  tree_code_name = (char **) xmalloc (sizeof (standard_tree_code_name));
  bcopy ((char *) standard_tree_code_type, (char *) tree_code_type,
	 sizeof (standard_tree_code_type));
  bcopy ((char *) standard_tree_code_length, (char *) tree_code_length,
	 sizeof (standard_tree_code_length));
  bcopy ((char *) standard_tree_code_name, (char *) tree_code_name,
	 sizeof (standard_tree_code_name));
}

/* Return a newly allocated node of code CODE.
   Initialize the node's unique id and its TREE_PERMANENT flag.
   For decl and type nodes, some other fields are initialized.
   The rest of the node is initialized to zero.

   Achoo!  I got a code in the node.  */

tree
make_node (code)
     enum tree_code code;
{
  register tree t;
  register int type = TREE_CODE_CLASS (code);
  register int length;
  register struct obstack *obstack = current_obstack;
  register int i;
  register tree_node_kind kind;

  switch (type)
    {
    case 'd':  /* A decl node */
#ifdef GATHER_STATISTICS
      kind = d_kind;
#endif
      length = sizeof (struct tree_decl);
      /* All decls in an inline function need to be saved.  */
      if (obstack != &permanent_obstack)
	obstack = saveable_obstack;

      /* PARM_DECLs go on the context of the parent. If this is a nested
	 function, then we must allocate the PARM_DECL on the parent's
	 obstack, so that they will live to the end of the parent's
	 closing brace.  This is necessary in case we try to inline the
	 function into its parent.

	 PARM_DECLs of top-level functions do not have this problem.  However,
	 we allocate them where we put the FUNCTION_DECL for languages such as
	 Ada that need to consult some flags in the PARM_DECLs of the function
	 when calling it. 

	 See comment in restore_tree_status for why we can't put this
	 in function_obstack.  */
      if (code == PARM_DECL && obstack != &permanent_obstack)
	{
	  tree context = 0;
	  if (current_function_decl)
	    context = decl_function_context (current_function_decl);

	  if (context)
	    obstack
	      = find_function_data (context)->function_maybepermanent_obstack;
	}
      break;

    case 't':  /* a type node */
#ifdef GATHER_STATISTICS
      kind = t_kind;
#endif
      length = sizeof (struct tree_type);
      /* All data types are put where we can preserve them if nec.  */
      if (obstack != &permanent_obstack)
	obstack = all_types_permanent ? &permanent_obstack : saveable_obstack;
      break;

    case 'b':  /* a lexical block */
#ifdef GATHER_STATISTICS
      kind = b_kind;
#endif
      length = sizeof (struct tree_block);
      /* All BLOCK nodes are put where we can preserve them if nec.  */
      if (obstack != &permanent_obstack)
	obstack = saveable_obstack;
      break;

    case 's':  /* an expression with side effects */
#ifdef GATHER_STATISTICS
      kind = s_kind;
      goto usual_kind;
#endif
    case 'r':  /* a reference */
#ifdef GATHER_STATISTICS
      kind = r_kind;
      goto usual_kind;
#endif
    case 'e':  /* an expression */
    case '<':  /* a comparison expression */
    case '1':  /* a unary arithmetic expression */
    case '2':  /* a binary arithmetic expression */
#ifdef GATHER_STATISTICS
      kind = e_kind;
    usual_kind:
#endif
      obstack = expression_obstack;
      /* All BIND_EXPR nodes are put where we can preserve them if nec.  */
      if (code == BIND_EXPR && obstack != &permanent_obstack)
	obstack = saveable_obstack;
      length = sizeof (struct tree_exp)
	+ (tree_code_length[(int) code] - 1) * sizeof (char *);
      break;

    case 'c':  /* a constant */
#ifdef GATHER_STATISTICS
      kind = c_kind;
#endif
      obstack = expression_obstack;

      /* We can't use tree_code_length for INTEGER_CST, since the number of
	 words is machine-dependent due to varying length of HOST_WIDE_INT,
	 which might be wider than a pointer (e.g., long long).  Similarly
	 for REAL_CST, since the number of words is machine-dependent due
	 to varying size and alignment of `double'.  */

      if (code == INTEGER_CST)
	length = sizeof (struct tree_int_cst);
      else if (code == REAL_CST)
	length = sizeof (struct tree_real_cst);
      else
	length = sizeof (struct tree_common)
	  + tree_code_length[(int) code] * sizeof (char *);
      break;

    case 'x':  /* something random, like an identifier.  */
#ifdef GATHER_STATISTICS
      if (code == IDENTIFIER_NODE)
	kind = id_kind;
      else if (code == OP_IDENTIFIER)
	kind = op_id_kind;
      else if (code == TREE_VEC)
	kind = vec_kind;
      else
	kind = x_kind;
#endif
      length = sizeof (struct tree_common)
	+ tree_code_length[(int) code] * sizeof (char *);
      /* Identifier nodes are always permanent since they are
	 unique in a compiler run.  */
      if (code == IDENTIFIER_NODE) obstack = &permanent_obstack;
      break;

    default:
      abort ();
    }

  t = (tree) obstack_alloc (obstack, length);

#ifdef GATHER_STATISTICS
  tree_node_counts[(int)kind]++;
  tree_node_sizes[(int)kind] += length;
#endif

  /* Clear a word at a time.  */
  for (i = (length / sizeof (int)) - 1; i >= 0; i--)
    ((int *) t)[i] = 0;
  /* Clear any extra bytes.  */
  for (i = length / sizeof (int) * sizeof (int); i < length; i++)
    ((char *) t)[i] = 0;

  TREE_SET_CODE (t, code);
  if (obstack == &permanent_obstack)
    TREE_PERMANENT (t) = 1;

  switch (type)
    {
    case 's':
      TREE_SIDE_EFFECTS (t) = 1;
      TREE_TYPE (t) = void_type_node;
      break;

    case 'd':
      if (code != FUNCTION_DECL)
	DECL_ALIGN (t) = 1;
      DECL_IN_SYSTEM_HEADER (t)
	= in_system_header && (obstack == &permanent_obstack);
      DECL_SOURCE_LINE (t) = lineno;
      DECL_SOURCE_FILE (t) = (input_filename) ? input_filename : "<built-in>";
      DECL_UID (t) = next_decl_uid++;
      break;

    case 't':
      TYPE_UID (t) = next_type_uid++;
      TYPE_ALIGN (t) = 1;
      TYPE_MAIN_VARIANT (t) = t;
      TYPE_OBSTACK (t) = obstack;
      TYPE_ATTRIBUTES (t) = NULL_TREE;
#ifdef SET_DEFAULT_TYPE_ATTRIBUTES
      SET_DEFAULT_TYPE_ATTRIBUTES (t);
#endif
      break;

    case 'c':
      TREE_CONSTANT (t) = 1;
      break;
    }

  return t;
}

/* Return a new node with the same contents as NODE
   except that its TREE_CHAIN is zero and it has a fresh uid.  */

tree
copy_node (node)
     tree node;
{
  register tree t;
  register enum tree_code code = TREE_CODE (node);
  register int length;
  register int i;

  switch (TREE_CODE_CLASS (code))
    {
    case 'd':  /* A decl node */
      length = sizeof (struct tree_decl);
      break;

    case 't':  /* a type node */
      length = sizeof (struct tree_type);
      break;

    case 'b':  /* a lexical block node */
      length = sizeof (struct tree_block);
      break;

    case 'r':  /* a reference */
    case 'e':  /* an expression */
    case 's':  /* an expression with side effects */
    case '<':  /* a comparison expression */
    case '1':  /* a unary arithmetic expression */
    case '2':  /* a binary arithmetic expression */
      length = sizeof (struct tree_exp)
	+ (tree_code_length[(int) code] - 1) * sizeof (char *);
      break;

    case 'c':  /* a constant */
      /* We can't use tree_code_length for INTEGER_CST, since the number of
	 words is machine-dependent due to varying length of HOST_WIDE_INT,
	 which might be wider than a pointer (e.g., long long).  Similarly
	 for REAL_CST, since the number of words is machine-dependent due
	 to varying size and alignment of `double'.  */
      if (code == INTEGER_CST)
	length = sizeof (struct tree_int_cst);
      else if (code == REAL_CST)
	length = sizeof (struct tree_real_cst);
      else
	length = (sizeof (struct tree_common)
		  + tree_code_length[(int) code] * sizeof (char *));
      break;

    case 'x':  /* something random, like an identifier.  */
      length = sizeof (struct tree_common)
	+ tree_code_length[(int) code] * sizeof (char *);
      if (code == TREE_VEC)
	length += (TREE_VEC_LENGTH (node) - 1) * sizeof (char *);
    }

  t = (tree) obstack_alloc (current_obstack, length);

  for (i = (length / sizeof (int)) - 1; i >= 0; i--)
    ((int *) t)[i] = ((int *) node)[i];
  /* Clear any extra bytes.  */
  for (i = length / sizeof (int) * sizeof (int); i < length; i++)
    ((char *) t)[i] = ((char *) node)[i];

  TREE_CHAIN (t) = 0;
  TREE_ASM_WRITTEN (t) = 0;

  if (TREE_CODE_CLASS (code) == 'd')
    DECL_UID (t) = next_decl_uid++;
  else if (TREE_CODE_CLASS (code) == 't')
    {
      TYPE_UID (t) = next_type_uid++;
      TYPE_OBSTACK (t) = current_obstack;

      /* The following is so that the debug code for
	 the copy is different from the original type.
	 The two statements usually duplicate each other
	 (because they clear fields of the same union),
	 but the optimizer should catch that.  */
      TYPE_SYMTAB_POINTER (t) = 0;
      TYPE_SYMTAB_ADDRESS (t) = 0;
    }

  TREE_PERMANENT (t) = (current_obstack == &permanent_obstack);

  return t;
}

/* Return a copy of a chain of nodes, chained through the TREE_CHAIN field.
   For example, this can copy a list made of TREE_LIST nodes.  */

tree
copy_list (list)
     tree list;
{
  tree head;
  register tree prev, next;

  if (list == 0)
    return 0;

  head = prev = copy_node (list);
  next = TREE_CHAIN (list);
  while (next)
    {
      TREE_CHAIN (prev) = copy_node (next);
      prev = TREE_CHAIN (prev);
      next = TREE_CHAIN (next);
    }
  return head;
}

#define HASHBITS 30

/* Return an IDENTIFIER_NODE whose name is TEXT (a null-terminated string).
   If an identifier with that name has previously been referred to,
   the same node is returned this time.  */

tree
get_identifier (text)
     register char *text;
{
  register int hi;
  register int i;
  register tree idp;
  register int len, hash_len;

  /* Compute length of text in len.  */
  for (len = 0; text[len]; len++);

  /* Decide how much of that length to hash on */
  hash_len = len;
  if (warn_id_clash && len > id_clash_len)
    hash_len = id_clash_len;

  /* Compute hash code */
  hi = hash_len * 613 + (unsigned) text[0];
  for (i = 1; i < hash_len; i += 2)
    hi = ((hi * 613) + (unsigned) (text[i]));

  hi &= (1 << HASHBITS) - 1;
  hi %= MAX_HASH_TABLE;
  
  /* Search table for identifier */
  for (idp = hash_table[hi]; idp; idp = TREE_CHAIN (idp))
    if (IDENTIFIER_LENGTH (idp) == len
	&& IDENTIFIER_POINTER (idp)[0] == text[0]
	&& !bcmp (IDENTIFIER_POINTER (idp), text, len))
      return idp;		/* <-- return if found */

  /* Not found; optionally warn about a similar identifier */
  if (warn_id_clash && do_identifier_warnings && len >= id_clash_len)
    for (idp = hash_table[hi]; idp; idp = TREE_CHAIN (idp))
      if (!strncmp (IDENTIFIER_POINTER (idp), text, id_clash_len))
	{
	  warning ("`%s' and `%s' identical in first %d characters",
		   IDENTIFIER_POINTER (idp), text, id_clash_len);
	  break;
	}

  if (tree_code_length[(int) IDENTIFIER_NODE] < 0)
    abort ();			/* set_identifier_size hasn't been called.  */

  /* Not found, create one, add to chain */
  idp = make_node (IDENTIFIER_NODE);
  IDENTIFIER_LENGTH (idp) = len;
#ifdef GATHER_STATISTICS
  id_string_size += len;
#endif

  IDENTIFIER_POINTER (idp) = obstack_copy0 (&permanent_obstack, text, len);

  TREE_CHAIN (idp) = hash_table[hi];
  hash_table[hi] = idp;
  return idp;			/* <-- return if created */
}

/* If an identifier with the name TEXT (a null-terminated string) has
   previously been referred to, return that node; otherwise return
   NULL_TREE.  */

tree
maybe_get_identifier (text)
     register char *text;
{
  register int hi;
  register int i;
  register tree idp;
  register int len, hash_len;

  /* Compute length of text in len.  */
  for (len = 0; text[len]; len++);

  /* Decide how much of that length to hash on */
  hash_len = len;
  if (warn_id_clash && len > id_clash_len)
    hash_len = id_clash_len;

  /* Compute hash code */
  hi = hash_len * 613 + (unsigned) text[0];
  for (i = 1; i < hash_len; i += 2)
    hi = ((hi * 613) + (unsigned) (text[i]));

  hi &= (1 << HASHBITS) - 1;
  hi %= MAX_HASH_TABLE;
  
  /* Search table for identifier */
  for (idp = hash_table[hi]; idp; idp = TREE_CHAIN (idp))
    if (IDENTIFIER_LENGTH (idp) == len
	&& IDENTIFIER_POINTER (idp)[0] == text[0]
	&& !bcmp (IDENTIFIER_POINTER (idp), text, len))
      return idp;		/* <-- return if found */

  return NULL_TREE;
}

/* Enable warnings on similar identifiers (if requested).
   Done after the built-in identifiers are created.  */

void
start_identifier_warnings ()
{
  do_identifier_warnings = 1;
}

/* Record the size of an identifier node for the language in use.
   SIZE is the total size in bytes.
   This is called by the language-specific files.  This must be
   called before allocating any identifiers.  */

void
set_identifier_size (size)
     int size;
{
  tree_code_length[(int) IDENTIFIER_NODE]
    = (size - sizeof (struct tree_common)) / sizeof (tree);
}

/* Return a newly constructed INTEGER_CST node whose constant value
   is specified by the two ints LOW and HI.
   The TREE_TYPE is set to `int'. 

   This function should be used via the `build_int_2' macro.  */

tree
build_int_2_wide (low, hi)
     HOST_WIDE_INT low, hi;
{
  register tree t = make_node (INTEGER_CST);
  TREE_INT_CST_LOW (t) = low;
  TREE_INT_CST_HIGH (t) = hi;
  TREE_TYPE (t) = integer_type_node;
  return t;
}

/* Return a new REAL_CST node whose type is TYPE and value is D.  */

tree
build_real (type, d)
     tree type;
     REAL_VALUE_TYPE d;
{
  tree v;
  int overflow = 0;

  /* Check for valid float value for this type on this target machine;
     if not, can print error message and store a valid value in D.  */
#ifdef CHECK_FLOAT_VALUE
  CHECK_FLOAT_VALUE (TYPE_MODE (type), d, overflow);
#endif

  v = make_node (REAL_CST);
  TREE_TYPE (v) = type;
  TREE_REAL_CST (v) = d;
  TREE_OVERFLOW (v) = TREE_CONSTANT_OVERFLOW (v) = overflow;
  return v;
}

/* Return a new REAL_CST node whose type is TYPE
   and whose value is the integer value of the INTEGER_CST node I.  */

#if !defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)

REAL_VALUE_TYPE
real_value_from_int_cst (type, i)
     tree type, i;
{
  REAL_VALUE_TYPE d;
  REAL_VALUE_TYPE e;
  /* Some 386 compilers mishandle unsigned int to float conversions,
     so introduce a temporary variable E to avoid those bugs.  */

#ifdef REAL_ARITHMETIC
  if (! TREE_UNSIGNED (TREE_TYPE (i)))
    REAL_VALUE_FROM_INT (d, TREE_INT_CST_LOW (i), TREE_INT_CST_HIGH (i),
			 TYPE_MODE (type));
  else
    REAL_VALUE_FROM_UNSIGNED_INT (d, TREE_INT_CST_LOW (i),
				  TREE_INT_CST_HIGH (i), TYPE_MODE (type));
#else /* not REAL_ARITHMETIC */
  if (TREE_INT_CST_HIGH (i) < 0 && ! TREE_UNSIGNED (TREE_TYPE (i)))
    {
      d = (double) (~ TREE_INT_CST_HIGH (i));
      e = ((double) ((HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2))
	    * (double) ((HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2)));
      d *= e;
      e = (double) (unsigned HOST_WIDE_INT) (~ TREE_INT_CST_LOW (i));
      d += e;
      d = (- d - 1.0);
    }
  else
    {
      d = (double) (unsigned HOST_WIDE_INT) TREE_INT_CST_HIGH (i);
      e = ((double) ((HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2))
	    * (double) ((HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2)));
      d *= e;
      e = (double) (unsigned HOST_WIDE_INT) TREE_INT_CST_LOW (i);
      d += e;
    }
#endif /* not REAL_ARITHMETIC */
  return d;
}

/* This function can't be implemented if we can't do arithmetic
   on the float representation.  */

tree
build_real_from_int_cst (type, i)
     tree type;
     tree i;
{
  tree v;
  int overflow = TREE_OVERFLOW (i);
  REAL_VALUE_TYPE d;
  jmp_buf float_error;

  v = make_node (REAL_CST);
  TREE_TYPE (v) = type;

  if (setjmp (float_error))
    {
      d = dconst0;
      overflow = 1;
      goto got_it;
    }

  set_float_handler (float_error);

#ifdef REAL_ARITHMETIC
  d = real_value_from_int_cst (type, i);
#else
  d = REAL_VALUE_TRUNCATE (TYPE_MODE (type),
			   real_value_from_int_cst (type, i));
#endif

  /* Check for valid float value for this type on this target machine.  */

 got_it:
  set_float_handler (NULL_PTR);

#ifdef CHECK_FLOAT_VALUE
  CHECK_FLOAT_VALUE (TYPE_MODE (type), d, overflow);
#endif

  TREE_REAL_CST (v) = d;
  TREE_OVERFLOW (v) = TREE_CONSTANT_OVERFLOW (v) = overflow;
  return v;
}

#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */

/* Return a newly constructed STRING_CST node whose value is
   the LEN characters at STR.
   The TREE_TYPE is not initialized.  */

tree
build_string (len, str)
     int len;
     char *str;
{
  /* Put the string in saveable_obstack since it will be placed in the RTL
     for an "asm" statement and will also be kept around a while if
     deferring constant output in varasm.c.  */

  register tree s = make_node (STRING_CST);
  TREE_STRING_LENGTH (s) = len;
  TREE_STRING_POINTER (s) = obstack_copy0 (saveable_obstack, str, len);
  return s;
}

/* Return a newly constructed COMPLEX_CST node whose value is
   specified by the real and imaginary parts REAL and IMAG.
   Both REAL and IMAG should be constant nodes.  TYPE, if specified,
   will be the type of the COMPLEX_CST; otherwise a new type will be made.  */

tree
build_complex (type, real, imag)
     tree type;
     tree real, imag;
{
  register tree t = make_node (COMPLEX_CST);

  TREE_REALPART (t) = real;
  TREE_IMAGPART (t) = imag;
  TREE_TYPE (t) = type ? type : build_complex_type (TREE_TYPE (real));
  TREE_OVERFLOW (t) = TREE_OVERFLOW (real) | TREE_OVERFLOW (imag);
  TREE_CONSTANT_OVERFLOW (t)
    = TREE_CONSTANT_OVERFLOW (real) | TREE_CONSTANT_OVERFLOW (imag);
  return t;
}

/* Build a newly constructed TREE_VEC node of length LEN.  */

tree
make_tree_vec (len)
     int len;
{
  register tree t;
  register int length = (len-1) * sizeof (tree) + sizeof (struct tree_vec);
  register struct obstack *obstack = current_obstack;
  register int i;

#ifdef GATHER_STATISTICS
  tree_node_counts[(int)vec_kind]++;
  tree_node_sizes[(int)vec_kind] += length;
#endif

  t = (tree) obstack_alloc (obstack, length);

  for (i = (length / sizeof (int)) - 1; i >= 0; i--)
    ((int *) t)[i] = 0;

  TREE_SET_CODE (t, TREE_VEC);
  TREE_VEC_LENGTH (t) = len;
  if (obstack == &permanent_obstack)
    TREE_PERMANENT (t) = 1;

  return t;
}

/* Return 1 if EXPR is the integer constant zero or a complex constant
   of zero.  */

int
integer_zerop (expr)
     tree expr;
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == INTEGER_CST
	   && ! TREE_CONSTANT_OVERFLOW (expr)
	   && TREE_INT_CST_LOW (expr) == 0
	   && TREE_INT_CST_HIGH (expr) == 0)
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && integer_zerop (TREE_REALPART (expr))
	      && integer_zerop (TREE_IMAGPART (expr))));
}

/* Return 1 if EXPR is the integer constant one or the corresponding
   complex constant.  */

int
integer_onep (expr)
     tree expr;
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == INTEGER_CST
	   && ! TREE_CONSTANT_OVERFLOW (expr)
	   && TREE_INT_CST_LOW (expr) == 1
	   && TREE_INT_CST_HIGH (expr) == 0)
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && integer_onep (TREE_REALPART (expr))
	      && integer_zerop (TREE_IMAGPART (expr))));
}

/* Return 1 if EXPR is an integer containing all 1's in as much precision as
   it contains.  Likewise for the corresponding complex constant.  */

int
integer_all_onesp (expr)
     tree expr;
{
  register int prec;
  register int uns;

  STRIP_NOPS (expr);

  if (TREE_CODE (expr) == COMPLEX_CST
      && integer_all_onesp (TREE_REALPART (expr))
      && integer_zerop (TREE_IMAGPART (expr)))
    return 1;

  else if (TREE_CODE (expr) != INTEGER_CST
	   || TREE_CONSTANT_OVERFLOW (expr))
    return 0;

  uns = TREE_UNSIGNED (TREE_TYPE (expr));
  if (!uns)
    return TREE_INT_CST_LOW (expr) == -1 && TREE_INT_CST_HIGH (expr) == -1;

  /* Note that using TYPE_PRECISION here is wrong.  We care about the
     actual bits, not the (arbitrary) range of the type.  */
  prec = GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (expr)));
  if (prec >= HOST_BITS_PER_WIDE_INT)
    {
      int high_value, shift_amount;

      shift_amount = prec - HOST_BITS_PER_WIDE_INT;

      if (shift_amount > HOST_BITS_PER_WIDE_INT)
	/* Can not handle precisions greater than twice the host int size.  */
	abort ();
      else if (shift_amount == HOST_BITS_PER_WIDE_INT)
	/* Shifting by the host word size is undefined according to the ANSI
	   standard, so we must handle this as a special case.  */
	high_value = -1;
      else
	high_value = ((HOST_WIDE_INT) 1 << shift_amount) - 1;

      return TREE_INT_CST_LOW (expr) == -1
	&& TREE_INT_CST_HIGH (expr) == high_value;
    }
  else
    return TREE_INT_CST_LOW (expr) == ((HOST_WIDE_INT) 1 << prec) - 1;
}

/* Return 1 if EXPR is an integer constant that is a power of 2 (i.e., has only
   one bit on).  */

int
integer_pow2p (expr)
     tree expr;
{
  int prec;
  HOST_WIDE_INT high, low;

  STRIP_NOPS (expr);

  if (TREE_CODE (expr) == COMPLEX_CST
      && integer_pow2p (TREE_REALPART (expr))
      && integer_zerop (TREE_IMAGPART (expr)))
    return 1;

  if (TREE_CODE (expr) != INTEGER_CST || TREE_CONSTANT_OVERFLOW (expr))
    return 0;

  prec = (POINTER_TYPE_P (TREE_TYPE (expr))
	  ? POINTER_SIZE : TYPE_PRECISION (TREE_TYPE (expr)));
  high = TREE_INT_CST_HIGH (expr);
  low = TREE_INT_CST_LOW (expr);

  /* First clear all bits that are beyond the type's precision in case
     we've been sign extended.  */

  if (prec == 2 * HOST_BITS_PER_WIDE_INT)
    ;
  else if (prec > HOST_BITS_PER_WIDE_INT)
    high &= ~((HOST_WIDE_INT) (-1) << (prec - HOST_BITS_PER_WIDE_INT));
  else
    {
      high = 0;
      if (prec < HOST_BITS_PER_WIDE_INT)
	low &= ~((HOST_WIDE_INT) (-1) << prec);
    }

  if (high == 0 && low == 0)
    return 0;

  return ((high == 0 && (low & (low - 1)) == 0)
	  || (low == 0 && (high & (high - 1)) == 0));
}

/* Return the power of two represented by a tree node known to be a
   power of two.  */

int
tree_log2 (expr)
     tree expr;
{
  int prec;
  HOST_WIDE_INT high, low;

  STRIP_NOPS (expr);

  if (TREE_CODE (expr) == COMPLEX_CST)
    return tree_log2 (TREE_REALPART (expr));

  prec = (POINTER_TYPE_P (TREE_TYPE (expr))
	  ? POINTER_SIZE : TYPE_PRECISION (TREE_TYPE (expr)));

  high = TREE_INT_CST_HIGH (expr);
  low = TREE_INT_CST_LOW (expr);

  /* First clear all bits that are beyond the type's precision in case
     we've been sign extended.  */

  if (prec == 2 * HOST_BITS_PER_WIDE_INT)
    ;
  else if (prec > HOST_BITS_PER_WIDE_INT)
    high &= ~((HOST_WIDE_INT) (-1) << (prec - HOST_BITS_PER_WIDE_INT));
  else
    {
      high = 0;
      if (prec < HOST_BITS_PER_WIDE_INT)
	low &= ~((HOST_WIDE_INT) (-1) << prec);
    }

  return (high != 0 ? HOST_BITS_PER_WIDE_INT + exact_log2 (high)
	  :  exact_log2 (low));
}

/* Return 1 if EXPR is the real constant zero.  */

int
real_zerop (expr)
     tree expr;
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == REAL_CST
	   && ! TREE_CONSTANT_OVERFLOW (expr)
	   && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), dconst0))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && real_zerop (TREE_REALPART (expr))
	      && real_zerop (TREE_IMAGPART (expr))));
}

/* Return 1 if EXPR is the real constant one in real or complex form.  */

int
real_onep (expr)
     tree expr;
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == REAL_CST
	   && ! TREE_CONSTANT_OVERFLOW (expr)
	   && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), dconst1))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && real_onep (TREE_REALPART (expr))
	      && real_zerop (TREE_IMAGPART (expr))));
}

/* Return 1 if EXPR is the real constant two.  */

int
real_twop (expr)
     tree expr;
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == REAL_CST
	   && ! TREE_CONSTANT_OVERFLOW (expr)
	   && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), dconst2))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && real_twop (TREE_REALPART (expr))
	      && real_zerop (TREE_IMAGPART (expr))));
}

/* Nonzero if EXP is a constant or a cast of a constant.  */
 
int
really_constant_p (exp)
     tree exp;
{
  /* This is not quite the same as STRIP_NOPS.  It does more.  */
  while (TREE_CODE (exp) == NOP_EXPR
	 || TREE_CODE (exp) == CONVERT_EXPR
	 || TREE_CODE (exp) == NON_LVALUE_EXPR)
    exp = TREE_OPERAND (exp, 0);
  return TREE_CONSTANT (exp);
}

/* Return first list element whose TREE_VALUE is ELEM.
   Return 0 if ELEM is not in LIST.  */

tree
value_member (elem, list)
     tree elem, list;
{
  while (list)
    {
      if (elem == TREE_VALUE (list))
	return list;
      list = TREE_CHAIN (list);
    }
  return NULL_TREE;
}

/* Return first list element whose TREE_PURPOSE is ELEM.
   Return 0 if ELEM is not in LIST.  */

tree
purpose_member (elem, list)
     tree elem, list;
{
  while (list)
    {
      if (elem == TREE_PURPOSE (list))
	return list;
      list = TREE_CHAIN (list);
    }
  return NULL_TREE;
}

/* Return first list element whose BINFO_TYPE is ELEM.
   Return 0 if ELEM is not in LIST.  */

tree
binfo_member (elem, list)
     tree elem, list;
{
  while (list)
    {
      if (elem == BINFO_TYPE (list))
	return list;
      list = TREE_CHAIN (list);
    }
  return NULL_TREE;
}

/* Return nonzero if ELEM is part of the chain CHAIN.  */

int
chain_member (elem, chain)
     tree elem, chain;
{
  while (chain)
    {
      if (elem == chain)
	return 1;
      chain = TREE_CHAIN (chain);
    }

  return 0;
}

/* Return nonzero if ELEM is equal to TREE_VALUE (CHAIN) for any piece of
   chain CHAIN.  */
/* ??? This function was added for machine specific attributes but is no
   longer used.  It could be deleted if we could confirm all front ends
   don't use it.  */

int
chain_member_value (elem, chain)
     tree elem, chain;
{
  while (chain)
    {
      if (elem == TREE_VALUE (chain))
	return 1;
      chain = TREE_CHAIN (chain);
    }

  return 0;
}

/* Return nonzero if ELEM is equal to TREE_PURPOSE (CHAIN)
   for any piece of chain CHAIN.  */
/* ??? This function was added for machine specific attributes but is no
   longer used.  It could be deleted if we could confirm all front ends
   don't use it.  */

int
chain_member_purpose (elem, chain)
     tree elem, chain;
{
  while (chain)
    {
      if (elem == TREE_PURPOSE (chain))
	return 1;
      chain = TREE_CHAIN (chain);
    }

  return 0;
}

/* Return the length of a chain of nodes chained through TREE_CHAIN.
   We expect a null pointer to mark the end of the chain.
   This is the Lisp primitive `length'.  */

int
list_length (t)
     tree t;
{
  register tree tail;
  register int len = 0;

  for (tail = t; tail; tail = TREE_CHAIN (tail))
    len++;

  return len;
}

/* Concatenate two chains of nodes (chained through TREE_CHAIN)
   by modifying the last node in chain 1 to point to chain 2.
   This is the Lisp primitive `nconc'.  */

tree
chainon (op1, op2)
     tree op1, op2;
{

  if (op1)
    {
      register tree t1;
      register tree t2;

      for (t1 = op1; TREE_CHAIN (t1); t1 = TREE_CHAIN (t1))
	;
      TREE_CHAIN (t1) = op2;
      for (t2 = op2; t2; t2 = TREE_CHAIN (t2))
        if (t2 == t1)
          abort ();  /* Circularity created.  */
      return op1;
    }
  else return op2;
}

/* Return the last node in a chain of nodes (chained through TREE_CHAIN).  */

tree
tree_last (chain)
     register tree chain;
{
  register tree next;
  if (chain)
    while (next = TREE_CHAIN (chain))
      chain = next;
  return chain;
}

/* Reverse the order of elements in the chain T,
   and return the new head of the chain (old last element).  */

tree
nreverse (t)
     tree t;
{
  register tree prev = 0, decl, next;
  for (decl = t; decl; decl = next)
    {
      next = TREE_CHAIN (decl);
      TREE_CHAIN (decl) = prev;
      prev = decl;
    }
  return prev;
}

/* Given a chain CHAIN of tree nodes,
   construct and return a list of those nodes.  */

tree
listify (chain)
     tree chain;
{
  tree result = NULL_TREE;
  tree in_tail = chain;
  tree out_tail = NULL_TREE;

  while (in_tail)
    {
      tree next = tree_cons (NULL_TREE, in_tail, NULL_TREE);
      if (out_tail)
	TREE_CHAIN (out_tail) = next;
      else
	result = next;
      out_tail = next;
      in_tail = TREE_CHAIN (in_tail);
    }

  return result;
}

/* Return a newly created TREE_LIST node whose
   purpose and value fields are PARM and VALUE.  */

tree
build_tree_list (parm, value)
     tree parm, value;
{
  register tree t = make_node (TREE_LIST);
  TREE_PURPOSE (t) = parm;
  TREE_VALUE (t) = value;
  return t;
}

/* Similar, but build on the temp_decl_obstack.  */

tree
build_decl_list (parm, value)
     tree parm, value;
{
  register tree node;
  register struct obstack *ambient_obstack = current_obstack;
  current_obstack = &temp_decl_obstack;
  node = build_tree_list (parm, value);
  current_obstack = ambient_obstack;
  return node;
}

/* Similar, but build on the expression_obstack.  */

tree
build_expr_list (parm, value)
     tree parm, value;
{
  register tree node;
  register struct obstack *ambient_obstack = current_obstack;
  current_obstack = expression_obstack;
  node = build_tree_list (parm, value);
  current_obstack = ambient_obstack;
  return node;
}

/* Return a newly created TREE_LIST node whose
   purpose and value fields are PARM and VALUE
   and whose TREE_CHAIN is CHAIN.  */

tree
tree_cons (purpose, value, chain)
     tree purpose, value, chain;
{
#if 0
  register tree node = make_node (TREE_LIST);
#else
  register int i;
  register tree node = (tree) obstack_alloc (current_obstack, sizeof (struct tree_list));
#ifdef GATHER_STATISTICS
  tree_node_counts[(int)x_kind]++;
  tree_node_sizes[(int)x_kind] += sizeof (struct tree_list);
#endif

  for (i = (sizeof (struct tree_common) / sizeof (int)) - 1; i >= 0; i--)
    ((int *) node)[i] = 0;

  TREE_SET_CODE (node, TREE_LIST);
  if (current_obstack == &permanent_obstack)
    TREE_PERMANENT (node) = 1;
#endif

  TREE_CHAIN (node) = chain;
  TREE_PURPOSE (node) = purpose;
  TREE_VALUE (node) = value;
  return node;
}

/* Similar, but build on the temp_decl_obstack.  */

tree
decl_tree_cons (purpose, value, chain)
     tree purpose, value, chain;
{
  register tree node;
  register struct obstack *ambient_obstack = current_obstack;
  current_obstack = &temp_decl_obstack;
  node = tree_cons (purpose, value, chain);
  current_obstack = ambient_obstack;
  return node;
}

/* Similar, but build on the expression_obstack.  */

tree
expr_tree_cons (purpose, value, chain)
     tree purpose, value, chain;
{
  register tree node;
  register struct obstack *ambient_obstack = current_obstack;
  current_obstack = expression_obstack;
  node = tree_cons (purpose, value, chain);
  current_obstack = ambient_obstack;
  return node;
}

/* Same as `tree_cons' but make a permanent object.  */

tree
perm_tree_cons (purpose, value, chain)
     tree purpose, value, chain;
{
  register tree node;
  register struct obstack *ambient_obstack = current_obstack;
  current_obstack = &permanent_obstack;

  node = tree_cons (purpose, value, chain);
  current_obstack = ambient_obstack;
  return node;
}

/* Same as `tree_cons', but make this node temporary, regardless.  */

tree
temp_tree_cons (purpose, value, chain)
     tree purpose, value, chain;
{
  register tree node;
  register struct obstack *ambient_obstack = current_obstack;
  current_obstack = &temporary_obstack;

  node = tree_cons (purpose, value, chain);
  current_obstack = ambient_obstack;
  return node;
}

/* Same as `tree_cons', but save this node if the function's RTL is saved.  */

tree
saveable_tree_cons (purpose, value, chain)
     tree purpose, value, chain;
{
  register tree node;
  register struct obstack *ambient_obstack = current_obstack;
  current_obstack = saveable_obstack;

  node = tree_cons (purpose, value, chain);
  current_obstack = ambient_obstack;
  return node;
}

/* Return the size nominally occupied by an object of type TYPE
   when it resides in memory.  The value is measured in units of bytes,
   and its data type is that normally used for type sizes
   (which is the first type created by make_signed_type or
   make_unsigned_type).  */

tree
size_in_bytes (type)
     tree type;
{
  tree t;

  if (type == error_mark_node)
    return integer_zero_node;
  type = TYPE_MAIN_VARIANT (type);
  if (TYPE_SIZE (type) == 0)
    {
      incomplete_type_error (NULL_TREE, type);
      return integer_zero_node;
    }
  t = size_binop (CEIL_DIV_EXPR, TYPE_SIZE (type),
		  size_int (BITS_PER_UNIT));
  if (TREE_CODE (t) == INTEGER_CST)
    force_fit_type (t, 0);
  return t;
}

/* Return the size of TYPE (in bytes) as a wide integer
   or return -1 if the size can vary or is larger than an integer.  */

HOST_WIDE_INT
int_size_in_bytes (type)
     tree type;
{
  tree t;

  if (type == error_mark_node)
    return 0;

  type = TYPE_MAIN_VARIANT (type);
  if (TYPE_SIZE (type) == 0
      || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
    return -1;

  if (TREE_INT_CST_HIGH (TYPE_SIZE (type)) == 0)
    return ((TREE_INT_CST_LOW (TYPE_SIZE (type)) + BITS_PER_UNIT - 1)
	  / BITS_PER_UNIT);

  t = size_binop (CEIL_DIV_EXPR, TYPE_SIZE (type), size_int (BITS_PER_UNIT));
  if (TREE_CODE (t) != INTEGER_CST || TREE_INT_CST_HIGH (t) != 0)
    return -1;

  return TREE_INT_CST_LOW (t);
}

/* Return, as a tree node, the number of elements for TYPE (which is an
   ARRAY_TYPE) minus one. This counts only elements of the top array.

   Don't let any SAVE_EXPRs escape; if we are called as part of a cleanup
   action, they would get unsaved.  */

tree
array_type_nelts (type)
     tree type;
{
  tree index_type, min, max;

  /* If they did it with unspecified bounds, then we should have already
     given an error about it before we got here.  */
  if (! TYPE_DOMAIN (type))
    return error_mark_node;

  index_type = TYPE_DOMAIN (type);
  min = TYPE_MIN_VALUE (index_type);
  max = TYPE_MAX_VALUE (index_type);

  if (! TREE_CONSTANT (min))
    {
      STRIP_NOPS (min);
      if (TREE_CODE (min) == SAVE_EXPR)
	min = build (RTL_EXPR, TREE_TYPE (TYPE_MIN_VALUE (index_type)), 0,
		     SAVE_EXPR_RTL (min));
      else
	min = TYPE_MIN_VALUE (index_type);
    }

  if (! TREE_CONSTANT (max))
    {
      STRIP_NOPS (max);
      if (TREE_CODE (max) == SAVE_EXPR)
	max = build (RTL_EXPR, TREE_TYPE (TYPE_MAX_VALUE (index_type)), 0,
		     SAVE_EXPR_RTL (max));
      else
	max = TYPE_MAX_VALUE (index_type);
    }

  return (integer_zerop (min)
	  ? max
	  : fold (build (MINUS_EXPR, TREE_TYPE (max), max, min)));
}

/* Return nonzero if arg is static -- a reference to an object in
   static storage.  This is not the same as the C meaning of `static'.  */

int
staticp (arg)
     tree arg;
{
  switch (TREE_CODE (arg))
    {
    case FUNCTION_DECL:
      /* Nested functions aren't static, since taking their address
	 involves a trampoline.  */
       return decl_function_context (arg) == 0 || DECL_NO_STATIC_CHAIN (arg);
    case VAR_DECL:
      return TREE_STATIC (arg) || DECL_EXTERNAL (arg);

    case CONSTRUCTOR:
      return TREE_STATIC (arg);

    case STRING_CST:
      return 1;

      /* If we are referencing a bitfield, we can't evaluate an
	 ADDR_EXPR at compile time and so it isn't a constant.  */
    case COMPONENT_REF:
      return (! DECL_BIT_FIELD (TREE_OPERAND (arg, 1))
	      && staticp (TREE_OPERAND (arg, 0)));

    case BIT_FIELD_REF:
      return 0;

#if 0
       /* This case is technically correct, but results in setting
	  TREE_CONSTANT on ADDR_EXPRs that cannot be evaluated at
	  compile time.  */
    case INDIRECT_REF:
      return TREE_CONSTANT (TREE_OPERAND (arg, 0));
#endif

    case ARRAY_REF:
      if (TREE_CODE (TYPE_SIZE (TREE_TYPE (arg))) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg, 1)) == INTEGER_CST)
	return staticp (TREE_OPERAND (arg, 0));

    default:
      return 0;
    }
}

/* Wrap a SAVE_EXPR around EXPR, if appropriate.
   Do this to any expression which may be used in more than one place,
   but must be evaluated only once.

   Normally, expand_expr would reevaluate the expression each time.
   Calling save_expr produces something that is evaluated and recorded
   the first time expand_expr is called on it.  Subsequent calls to
   expand_expr just reuse the recorded value.

   The call to expand_expr that generates code that actually computes
   the value is the first call *at compile time*.  Subsequent calls
   *at compile time* generate code to use the saved value.
   This produces correct result provided that *at run time* control
   always flows through the insns made by the first expand_expr
   before reaching the other places where the save_expr was evaluated.
   You, the caller of save_expr, must make sure this is so.

   Constants, and certain read-only nodes, are returned with no
   SAVE_EXPR because that is safe.  Expressions containing placeholders
   are not touched; see tree.def for an explanation of what these
   are used for.  */

tree
save_expr (expr)
     tree expr;
{
  register tree t = fold (expr);

  /* We don't care about whether this can be used as an lvalue in this
     context.  */
  while (TREE_CODE (t) == NON_LVALUE_EXPR)
    t = TREE_OPERAND (t, 0);

  /* If the tree evaluates to a constant, then we don't want to hide that
     fact (i.e. this allows further folding, and direct checks for constants).
     However, a read-only object that has side effects cannot be bypassed.
     Since it is no problem to reevaluate literals, we just return the 
     literal node.  */

  if (TREE_CONSTANT (t) || (TREE_READONLY (t) && ! TREE_SIDE_EFFECTS (t))
      || TREE_CODE (t) == SAVE_EXPR || TREE_CODE (t) == ERROR_MARK)
    return t;

  /* If T contains a PLACEHOLDER_EXPR, we must evaluate it each time, since
     it means that the size or offset of some field of an object depends on
     the value within another field.

     Note that it must not be the case that T contains both a PLACEHOLDER_EXPR
     and some variable since it would then need to be both evaluated once and
     evaluated more than once.  Front-ends must assure this case cannot
     happen by surrounding any such subexpressions in their own SAVE_EXPR
     and forcing evaluation at the proper time.  */
  if (contains_placeholder_p (t))
    return t;

  t = build (SAVE_EXPR, TREE_TYPE (expr), t, current_function_decl, NULL_TREE);

  /* This expression might be placed ahead of a jump to ensure that the
     value was computed on both sides of the jump.  So make sure it isn't
     eliminated as dead.  */
  TREE_SIDE_EFFECTS (t) = 1;
  return t;
}

/* Arrange for an expression to be expanded multiple independent
   times.  This is useful for cleanup actions, as the backend can
   expand them multiple times in different places.  */

tree
unsave_expr (expr)
     tree expr;
{
  tree t;

  /* If this is already protected, no sense in protecting it again.  */
  if (TREE_CODE (expr) == UNSAVE_EXPR)
    return expr;

  t = build1 (UNSAVE_EXPR, TREE_TYPE (expr), expr);
  TREE_SIDE_EFFECTS (t) = TREE_SIDE_EFFECTS (expr);
  return t;
}

/* Modify a tree in place so that all the evaluate only once things
   are cleared out.  Return the EXPR given.  */

tree
unsave_expr_now (expr)
     tree expr;
{
  enum tree_code code;
  register int i;
  int first_rtl;

  if (expr == NULL_TREE)
    return expr;

  code = TREE_CODE (expr);
  first_rtl = tree_code_length [(int) code];
  switch (code)
    {
    case SAVE_EXPR:
      SAVE_EXPR_RTL (expr) = 0;
      first_rtl = 2;
      break;

    case TARGET_EXPR:
      TREE_OPERAND (expr, 1) = TREE_OPERAND (expr, 3);
      TREE_OPERAND (expr, 3) = NULL_TREE;
      break;
      
    case RTL_EXPR:
      /* I don't yet know how to emit a sequence multiple times.  */
      if (RTL_EXPR_SEQUENCE (expr) != 0)
	abort ();
      first_rtl = 0;
      break;

    case CALL_EXPR:
      CALL_EXPR_RTL (expr) = 0;
      if (TREE_OPERAND (expr, 1)
	  && TREE_CODE (TREE_OPERAND (expr, 1)) == TREE_LIST)
	{
	  tree exp = TREE_OPERAND (expr, 1);
	  while (exp)
	    {
	      unsave_expr_now (TREE_VALUE (exp));
	      exp = TREE_CHAIN (exp);
	    }
	}
      first_rtl = 2;
      break;

    case WITH_CLEANUP_EXPR:
      /* Should be defined to be 2.  */
      first_rtl = 1;
      break;

    case METHOD_CALL_EXPR:
      first_rtl = 3;
      break;

    default:
      break;
    }

  switch (TREE_CODE_CLASS (code))
    {
    case 'c':  /* a constant */
    case 't':  /* a type node */
    case 'x':  /* something random, like an identifier or an ERROR_MARK.  */
    case 'd':  /* A decl node */
    case 'b':  /* A block node */
      return expr;

    case 'e':  /* an expression */
    case 'r':  /* a reference */
    case 's':  /* an expression with side effects */
    case '<':  /* a comparison expression */
    case '2':  /* a binary arithmetic expression */
    case '1':  /* a unary arithmetic expression */
      for (i = first_rtl - 1; i >= 0; i--)
	unsave_expr_now (TREE_OPERAND (expr, i));
      return expr;

    default:
      abort ();
    }
}

/* Return 1 if EXP contains a PLACEHOLDER_EXPR; i.e., if it represents a size
   or offset that depends on a field within a record.  */

int
contains_placeholder_p (exp)
     tree exp;
{
  register enum tree_code code = TREE_CODE (exp);
  int result;

  /* If we have a WITH_RECORD_EXPR, it "cancels" any PLACEHOLDER_EXPR
     in it since it is supplying a value for it.  */
  if (code == WITH_RECORD_EXPR)
    return 0;
  else if (code == PLACEHOLDER_EXPR)
    return 1;

  switch (TREE_CODE_CLASS (code))
    {
    case 'r':
      /* Don't look at any PLACEHOLDER_EXPRs that might be in index or bit
	 position computations since they will be converted into a
	 WITH_RECORD_EXPR involving the reference, which will assume
	 here will be valid.  */
      return contains_placeholder_p (TREE_OPERAND (exp, 0));

    case 'x':
      if (code == TREE_LIST)
	return (contains_placeholder_p (TREE_VALUE (exp))
		|| (TREE_CHAIN (exp) != 0
		    && contains_placeholder_p (TREE_CHAIN (exp))));
      break;
					
    case '1':
    case '2':  case '<':
    case 'e':
      switch (code)
	{
	case COMPOUND_EXPR:
	  /* Ignoring the first operand isn't quite right, but works best. */
	  return contains_placeholder_p (TREE_OPERAND (exp, 1));

	case RTL_EXPR:
	case CONSTRUCTOR:
	  return 0;

	case COND_EXPR:
	  return (contains_placeholder_p (TREE_OPERAND (exp, 0))
		  || contains_placeholder_p (TREE_OPERAND (exp, 1))
		  || contains_placeholder_p (TREE_OPERAND (exp, 2)));

	case SAVE_EXPR:
	  /* If we already know this doesn't have a placeholder, don't
	     check again.  */
	  if (SAVE_EXPR_NOPLACEHOLDER (exp) || SAVE_EXPR_RTL (exp) != 0)
	    return 0;

	  SAVE_EXPR_NOPLACEHOLDER (exp) = 1;
	  result = contains_placeholder_p (TREE_OPERAND (exp, 0));
	  if (result)
	    SAVE_EXPR_NOPLACEHOLDER (exp) = 0;

	  return result;

	case CALL_EXPR:
	  return (TREE_OPERAND (exp, 1) != 0
		  && contains_placeholder_p (TREE_OPERAND (exp, 1)));

	default:
	  break;
	}

      switch (tree_code_length[(int) code])
	{
	case 1:
	  return contains_placeholder_p (TREE_OPERAND (exp, 0));
	case 2:
	  return (contains_placeholder_p (TREE_OPERAND (exp, 0))
		  || contains_placeholder_p (TREE_OPERAND (exp, 1)));
	default:
	  return 0;
	}

    default:
      return 0;
    }
}

/* Given a tree EXP, a FIELD_DECL F, and a replacement value R,
   return a tree with all occurrences of references to F in a
   PLACEHOLDER_EXPR replaced by R.   Note that we assume here that EXP
   contains only arithmetic expressions or a CALL_EXPR with a
   PLACEHOLDER_EXPR occurring only in its arglist.  */

tree
substitute_in_expr (exp, f, r)
     tree exp;
     tree f;
     tree r;
{
  enum tree_code code = TREE_CODE (exp);
  tree op0, op1, op2;
  tree new;
  tree inner;

  switch (TREE_CODE_CLASS (code))
    {
    case 'c':
    case 'd':
      return exp;

    case 'x':
      if (code == PLACEHOLDER_EXPR)
	return exp;
      else if (code == TREE_LIST)
	{
	  op0 = (TREE_CHAIN (exp) == 0
		 ? 0 : substitute_in_expr (TREE_CHAIN (exp), f, r));
	  op1 = substitute_in_expr (TREE_VALUE (exp), f, r);
	  if (op0 == TREE_CHAIN (exp) && op1 == TREE_VALUE (exp))
	    return exp;

	  return tree_cons (TREE_PURPOSE (exp), op1, op0);
	}

      abort ();

    case '1':
    case '2':
    case '<':
    case 'e':
      switch (tree_code_length[(int) code])
	{
	case 1:
	  op0 = substitute_in_expr (TREE_OPERAND (exp, 0), f, r);
	  if (op0 == TREE_OPERAND (exp, 0))
	    return exp;
	  
	  new = fold (build1 (code, TREE_TYPE (exp), op0));
	  break;

	case 2:
	  /* An RTL_EXPR cannot contain a PLACEHOLDER_EXPR; a CONSTRUCTOR
	     could, but we don't support it.  */
	  if (code == RTL_EXPR)
	    return exp;
	  else if (code == CONSTRUCTOR)
	    abort ();

	  op0 = substitute_in_expr (TREE_OPERAND (exp, 0), f, r);
	  op1 = substitute_in_expr (TREE_OPERAND (exp, 1), f, r);
	  if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1))
	    return exp;

	  new = fold (build (code, TREE_TYPE (exp), op0, op1));
	  break;

	case 3:
	  /* It cannot be that anything inside a SAVE_EXPR contains a
	     PLACEHOLDER_EXPR.  */
	  if (code == SAVE_EXPR)
	    return exp;

	  else if (code == CALL_EXPR)
	    {
	      op1 = substitute_in_expr (TREE_OPERAND (exp, 1), f, r);
	      if (op1 == TREE_OPERAND (exp, 1))
		return exp;

	      return build (code, TREE_TYPE (exp),
			    TREE_OPERAND (exp, 0), op1, NULL_TREE);
	    }

	  else if (code != COND_EXPR)
	    abort ();

	  op0 = substitute_in_expr (TREE_OPERAND (exp, 0), f, r);
	  op1 = substitute_in_expr (TREE_OPERAND (exp, 1), f, r);
	  op2 = substitute_in_expr (TREE_OPERAND (exp, 2), f, r);
	  if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
	      && op2 == TREE_OPERAND (exp, 2))
	    return exp;

	  new = fold (build (code, TREE_TYPE (exp), op0, op1, op2));
	  break;

	default:
	  abort ();
	}

      break;

    case 'r':
      switch (code)
	{
	case COMPONENT_REF:
	  /* If this expression is getting a value from a PLACEHOLDER_EXPR
	     and it is the right field, replace it with R.  */
	  for (inner = TREE_OPERAND (exp, 0);
	       TREE_CODE_CLASS (TREE_CODE (inner)) == 'r';
	       inner = TREE_OPERAND (inner, 0))
	    ;
	  if (TREE_CODE (inner) == PLACEHOLDER_EXPR
	      && TREE_OPERAND (exp, 1) == f)
	    return r;

	  /* If this expression hasn't been completed let, leave it 
	     alone.  */
	  if (TREE_CODE (inner) == PLACEHOLDER_EXPR
	      && TREE_TYPE (inner) == 0)
	    return exp;

	  op0 = substitute_in_expr (TREE_OPERAND (exp, 0), f, r);
	  if (op0 == TREE_OPERAND (exp, 0))
	    return exp;

	  new = fold (build (code, TREE_TYPE (exp), op0,
			     TREE_OPERAND (exp, 1)));
	  break;

	case BIT_FIELD_REF:
	  op0 = substitute_in_expr (TREE_OPERAND (exp, 0), f, r);
	  op1 = substitute_in_expr (TREE_OPERAND (exp, 1), f, r);
	  op2 = substitute_in_expr (TREE_OPERAND (exp, 2), f, r);
	  if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
	      && op2 == TREE_OPERAND (exp, 2))
	    return exp;

	  new = fold (build (code, TREE_TYPE (exp), op0, op1, op2));
	  break;

	case INDIRECT_REF:
	case BUFFER_REF:
	  op0 = substitute_in_expr (TREE_OPERAND (exp, 0), f, r);
	  if (op0 == TREE_OPERAND (exp, 0))
	    return exp;

	  new = fold (build1 (code, TREE_TYPE (exp), op0));
	  break;

	default:
	  abort ();
	}
      break;
      
    default:
      abort ();
    }

  TREE_READONLY (new) = TREE_READONLY (exp);
  return new;
}

/* Stabilize a reference so that we can use it any number of times
   without causing its operands to be evaluated more than once.
   Returns the stabilized reference.  This works by means of save_expr,
   so see the caveats in the comments about save_expr.

   Also allows conversion expressions whose operands are references.
   Any other kind of expression is returned unchanged.  */

tree
stabilize_reference (ref)
     tree ref;
{
  register tree result;
  register enum tree_code code = TREE_CODE (ref);

  switch (code)
    {
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      /* No action is needed in this case.  */
      return ref;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_CEIL_EXPR:
      result = build_nt (code, stabilize_reference (TREE_OPERAND (ref, 0)));
      break;

    case INDIRECT_REF:
      result = build_nt (INDIRECT_REF,
			 stabilize_reference_1 (TREE_OPERAND (ref, 0)));
      break;

    case COMPONENT_REF:
      result = build_nt (COMPONENT_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 TREE_OPERAND (ref, 1));
      break;

    case BIT_FIELD_REF:
      result = build_nt (BIT_FIELD_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 1)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 2)));
      break;

    case ARRAY_REF:
      result = build_nt (ARRAY_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 1)));
      break;

    case COMPOUND_EXPR:
      /* We cannot wrap the first expression in a SAVE_EXPR, as then
	 it wouldn't be ignored.  This matters when dealing with
	 volatiles.  */
      return stabilize_reference_1 (ref);

    case RTL_EXPR:
      result = build1 (INDIRECT_REF, TREE_TYPE (ref),
		       save_expr (build1 (ADDR_EXPR,
					  build_pointer_type (TREE_TYPE (ref)),
					  ref)));
      break;


      /* If arg isn't a kind of lvalue we recognize, make no change.
	 Caller should recognize the error for an invalid lvalue.  */
    default:
      return ref;

    case ERROR_MARK:
      return error_mark_node;
    }

  TREE_TYPE (result) = TREE_TYPE (ref);
  TREE_READONLY (result) = TREE_READONLY (ref);
  TREE_SIDE_EFFECTS (result) = TREE_SIDE_EFFECTS (ref);
  TREE_THIS_VOLATILE (result) = TREE_THIS_VOLATILE (ref);
  TREE_RAISES (result) = TREE_RAISES (ref);

  return result;
}

/* Subroutine of stabilize_reference; this is called for subtrees of
   references.  Any expression with side-effects must be put in a SAVE_EXPR
   to ensure that it is only evaluated once.

   We don't put SAVE_EXPR nodes around everything, because assigning very
   simple expressions to temporaries causes us to miss good opportunities
   for optimizations.  Among other things, the opportunity to fold in the
   addition of a constant into an addressing mode often gets lost, e.g.
   "y[i+1] += x;".  In general, we take the approach that we should not make
   an assignment unless we are forced into it - i.e., that any non-side effect
   operator should be allowed, and that cse should take care of coalescing
   multiple utterances of the same expression should that prove fruitful.  */

tree
stabilize_reference_1 (e)
     tree e;
{
  register tree result;
  register enum tree_code code = TREE_CODE (e);

  /* We cannot ignore const expressions because it might be a reference
     to a const array but whose index contains side-effects.  But we can
     ignore things that are actual constant or that already have been
     handled by this function.  */

  if (TREE_CONSTANT (e) || code == SAVE_EXPR)
    return e;

  switch (TREE_CODE_CLASS (code))
    {
    case 'x':
    case 't':
    case 'd':
    case 'b':
    case '<':
    case 's':
    case 'e':
    case 'r':
      /* If the expression has side-effects, then encase it in a SAVE_EXPR
	 so that it will only be evaluated once.  */
      /* The reference (r) and comparison (<) classes could be handled as
	 below, but it is generally faster to only evaluate them once.  */
      if (TREE_SIDE_EFFECTS (e))
	return save_expr (e);
      return e;

    case 'c':
      /* Constants need no processing.  In fact, we should never reach
	 here.  */
      return e;
      
    case '2':
      /* Division is slow and tends to be compiled with jumps,
	 especially the division by powers of 2 that is often
	 found inside of an array reference.  So do it just once.  */
      if (code == TRUNC_DIV_EXPR || code == TRUNC_MOD_EXPR
	  || code == FLOOR_DIV_EXPR || code == FLOOR_MOD_EXPR
	  || code == CEIL_DIV_EXPR || code == CEIL_MOD_EXPR
	  || code == ROUND_DIV_EXPR || code == ROUND_MOD_EXPR)
	return save_expr (e);
      /* Recursively stabilize each operand.  */
      result = build_nt (code, stabilize_reference_1 (TREE_OPERAND (e, 0)),
			 stabilize_reference_1 (TREE_OPERAND (e, 1)));
      break;

    case '1':
      /* Recursively stabilize each operand.  */
      result = build_nt (code, stabilize_reference_1 (TREE_OPERAND (e, 0)));
      break;

    default:
      abort ();
    }
  
  TREE_TYPE (result) = TREE_TYPE (e);
  TREE_READONLY (result) = TREE_READONLY (e);
  TREE_SIDE_EFFECTS (result) = TREE_SIDE_EFFECTS (e);
  TREE_THIS_VOLATILE (result) = TREE_THIS_VOLATILE (e);
  TREE_RAISES (result) = TREE_RAISES (e);

  return result;
}

/* Low-level constructors for expressions.  */

/* Build an expression of code CODE, data type TYPE,
   and operands as specified by the arguments ARG1 and following arguments.
   Expressions and reference nodes can be created this way.
   Constants, decls, types and misc nodes cannot be.  */

tree
build VPROTO((enum tree_code code, tree tt, ...))
{
#ifndef __STDC__
  enum tree_code code;
  tree tt;
#endif
  va_list p;
  register tree t;
  register int length;
  register int i;

  VA_START (p, tt);

#ifndef __STDC__
  code = va_arg (p, enum tree_code);
  tt = va_arg (p, tree);
#endif

  t = make_node (code);
  length = tree_code_length[(int) code];
  TREE_TYPE (t) = tt;

  if (length == 2)
    {
      /* This is equivalent to the loop below, but faster.  */
      register tree arg0 = va_arg (p, tree);
      register tree arg1 = va_arg (p, tree);
      TREE_OPERAND (t, 0) = arg0;
      TREE_OPERAND (t, 1) = arg1;
      if ((arg0 && TREE_SIDE_EFFECTS (arg0))
	  || (arg1 && TREE_SIDE_EFFECTS (arg1)))
	TREE_SIDE_EFFECTS (t) = 1;
      TREE_RAISES (t)
	= (arg0 && TREE_RAISES (arg0)) || (arg1 && TREE_RAISES (arg1));
    }
  else if (length == 1)
    {
      register tree arg0 = va_arg (p, tree);

      /* Call build1 for this!  */
      if (TREE_CODE_CLASS (code) != 's')
	abort ();
      TREE_OPERAND (t, 0) = arg0;
      if (arg0 && TREE_SIDE_EFFECTS (arg0))
	TREE_SIDE_EFFECTS (t) = 1;
      TREE_RAISES (t) = (arg0 && TREE_RAISES (arg0));
    }
  else
    {
      for (i = 0; i < length; i++)
	{
	  register tree operand = va_arg (p, tree);
	  TREE_OPERAND (t, i) = operand;
	  if (operand)
	    {
	      if (TREE_SIDE_EFFECTS (operand))
		TREE_SIDE_EFFECTS (t) = 1;
	      if (TREE_RAISES (operand))
		TREE_RAISES (t) = 1;
	    }
	}
    }
  va_end (p);
  return t;
}

/* Same as above, but only builds for unary operators.
   Saves lions share of calls to `build'; cuts down use
   of varargs, which is expensive for RISC machines.  */

tree
build1 (code, type, node)
     enum tree_code code;
     tree type;
     tree node;
{
  register struct obstack *obstack = expression_obstack;
  register int i, length;
  register tree_node_kind kind;
  register tree t;

#ifdef GATHER_STATISTICS
  if (TREE_CODE_CLASS (code) == 'r')
    kind = r_kind;
  else
    kind = e_kind;
#endif

  length = sizeof (struct tree_exp);

  t = (tree) obstack_alloc (obstack, length);

#ifdef GATHER_STATISTICS
  tree_node_counts[(int)kind]++;
  tree_node_sizes[(int)kind] += length;
#endif

  for (i = (length / sizeof (int)) - 1; i >= 0; i--)
    ((int *) t)[i] = 0;

  TREE_TYPE (t) = type;
  TREE_SET_CODE (t, code);

  if (obstack == &permanent_obstack)
    TREE_PERMANENT (t) = 1;

  TREE_OPERAND (t, 0) = node;
  if (node)
    {
      if (TREE_SIDE_EFFECTS (node))
	TREE_SIDE_EFFECTS (t) = 1;
      if (TREE_RAISES (node))
	TREE_RAISES (t) = 1;
    }

  return t;
}

/* Similar except don't specify the TREE_TYPE
   and leave the TREE_SIDE_EFFECTS as 0.
   It is permissible for arguments to be null,
   or even garbage if their values do not matter.  */

tree
build_nt VPROTO((enum tree_code code, ...))
{
#ifndef __STDC__
  enum tree_code code;
#endif
  va_list p;
  register tree t;
  register int length;
  register int i;

  VA_START (p, code);

#ifndef __STDC__
  code = va_arg (p, enum tree_code);
#endif

  t = make_node (code);
  length = tree_code_length[(int) code];

  for (i = 0; i < length; i++)
    TREE_OPERAND (t, i) = va_arg (p, tree);

  va_end (p);
  return t;
}

/* Similar to `build_nt', except we build
   on the temp_decl_obstack, regardless.  */

tree
build_parse_node VPROTO((enum tree_code code, ...))
{
#ifndef __STDC__
  enum tree_code code;
#endif
  register struct obstack *ambient_obstack = expression_obstack;
  va_list p;
  register tree t;
  register int length;
  register int i;

  VA_START (p, code);

#ifndef __STDC__
  code = va_arg (p, enum tree_code);
#endif

  expression_obstack = &temp_decl_obstack;

  t = make_node (code);
  length = tree_code_length[(int) code];

  for (i = 0; i < length; i++)
    TREE_OPERAND (t, i) = va_arg (p, tree);

  va_end (p);
  expression_obstack = ambient_obstack;
  return t;
}

#if 0
/* Commented out because this wants to be done very
   differently.  See cp-lex.c.  */
tree
build_op_identifier (op1, op2)
     tree op1, op2;
{
  register tree t = make_node (OP_IDENTIFIER);
  TREE_PURPOSE (t) = op1;
  TREE_VALUE (t) = op2;
  return t;
}
#endif

/* Create a DECL_... node of code CODE, name NAME and data type TYPE.
   We do NOT enter this node in any sort of symbol table.

   layout_decl is used to set up the decl's storage layout.
   Other slots are initialized to 0 or null pointers.  */

tree
build_decl (code, name, type)
     enum tree_code code;
     tree name, type;
{
  register tree t;

  t = make_node (code);

/*  if (type == error_mark_node)
    type = integer_type_node; */
/* That is not done, deliberately, so that having error_mark_node
   as the type can suppress useless errors in the use of this variable.  */

  DECL_NAME (t) = name;
  DECL_ASSEMBLER_NAME (t) = name;
  TREE_TYPE (t) = type;

  if (code == VAR_DECL || code == PARM_DECL || code == RESULT_DECL)
    layout_decl (t, 0);
  else if (code == FUNCTION_DECL)
    DECL_MODE (t) = FUNCTION_MODE;

  return t;
}

/* BLOCK nodes are used to represent the structure of binding contours
   and declarations, once those contours have been exited and their contents
   compiled.  This information is used for outputting debugging info.  */

tree
build_block (vars, tags, subblocks, supercontext, chain)
     tree vars, tags, subblocks, supercontext, chain;
{
  register tree block = make_node (BLOCK);
  BLOCK_VARS (block) = vars;
  BLOCK_TYPE_TAGS (block) = tags;
  BLOCK_SUBBLOCKS (block) = subblocks;
  BLOCK_SUPERCONTEXT (block) = supercontext;
  BLOCK_CHAIN (block) = chain;
  return block;
}

/* Return a declaration like DDECL except that its DECL_MACHINE_ATTRIBUTE
   is ATTRIBUTE.  */

tree
build_decl_attribute_variant (ddecl, attribute)
     tree ddecl, attribute;
{
  DECL_MACHINE_ATTRIBUTES (ddecl) = attribute;
  return ddecl;
}

/* Return a type like TTYPE except that its TYPE_ATTRIBUTE
   is ATTRIBUTE.

   Record such modified types already made so we don't make duplicates.  */

tree
build_type_attribute_variant (ttype, attribute)
     tree ttype, attribute;
{
  if ( ! attribute_list_equal (TYPE_ATTRIBUTES (ttype), attribute))
    {
      register int hashcode;
      register struct obstack *ambient_obstack = current_obstack;
      tree ntype;

      if (ambient_obstack != &permanent_obstack)
        current_obstack = TYPE_OBSTACK (ttype);

      ntype = copy_node (ttype);
      current_obstack = ambient_obstack;

      TYPE_POINTER_TO (ntype) = 0;
      TYPE_REFERENCE_TO (ntype) = 0;
      TYPE_ATTRIBUTES (ntype) = attribute;

      /* Create a new main variant of TYPE.  */
      TYPE_MAIN_VARIANT (ntype) = ntype;
      TYPE_NEXT_VARIANT (ntype) = 0;
      TYPE_READONLY (ntype) = TYPE_VOLATILE (ntype) = 0;

      hashcode = TYPE_HASH (TREE_CODE (ntype))
		 + TYPE_HASH (TREE_TYPE (ntype))
		 + attribute_hash_list (attribute);

      switch (TREE_CODE (ntype))
        {
	case FUNCTION_TYPE:
	  hashcode += TYPE_HASH (TYPE_ARG_TYPES (ntype));
	  break;
	case ARRAY_TYPE:
	  hashcode += TYPE_HASH (TYPE_DOMAIN (ntype));
	  break;
	case INTEGER_TYPE:
	  hashcode += TYPE_HASH (TYPE_MAX_VALUE (ntype));
	  break;
	case REAL_TYPE:
	  hashcode += TYPE_HASH (TYPE_PRECISION (ntype));
	  break;
	default:
	  break;
        }

      ntype = type_hash_canon (hashcode, ntype);
      ttype = build_type_variant (ntype, TYPE_READONLY (ttype),
				  TYPE_VOLATILE (ttype));
    }

  return ttype;
}

/* Return a 1 if ATTR_NAME and ATTR_ARGS is valid for either declaration DECL
   or type TYPE and 0 otherwise.  Validity is determined the configuration
   macros VALID_MACHINE_DECL_ATTRIBUTE and VALID_MACHINE_TYPE_ATTRIBUTE.  */

int
valid_machine_attribute (attr_name, attr_args, decl, type)
     tree attr_name, attr_args;
     tree decl;
     tree type;
{
  int valid = 0;
  tree decl_attr_list = decl != 0 ? DECL_MACHINE_ATTRIBUTES (decl) : 0;
  tree type_attr_list = TYPE_ATTRIBUTES (type);

  if (TREE_CODE (attr_name) != IDENTIFIER_NODE)
    abort ();

#ifdef VALID_MACHINE_DECL_ATTRIBUTE
  if (decl != 0
      && VALID_MACHINE_DECL_ATTRIBUTE (decl, decl_attr_list, attr_name, attr_args))
    {
      tree attr = lookup_attribute (IDENTIFIER_POINTER (attr_name),
				    decl_attr_list);

      if (attr != NULL_TREE)
	{
	  /* Override existing arguments.  Declarations are unique so we can
	     modify this in place.  */
	  TREE_VALUE (attr) = attr_args;
	}
      else
	{
	  decl_attr_list = tree_cons (attr_name, attr_args, decl_attr_list);
	  decl = build_decl_attribute_variant (decl, decl_attr_list);
	}

      valid = 1;
    }
#endif

#ifdef VALID_MACHINE_TYPE_ATTRIBUTE
  if (VALID_MACHINE_TYPE_ATTRIBUTE (type, type_attr_list, attr_name, attr_args))
    {
      tree attr = lookup_attribute (IDENTIFIER_POINTER (attr_name),
				    type_attr_list);

      if (attr != NULL_TREE)
	{
	  /* Override existing arguments.
	     ??? This currently works since attribute arguments are not
	     included in `attribute_hash_list'.  Something more complicated
	     may be needed in the future.  */
	  TREE_VALUE (attr) = attr_args;
	}
      else
	{
	  type_attr_list = tree_cons (attr_name, attr_args, type_attr_list);
	  type = build_type_attribute_variant (type, type_attr_list);
	}
      if (decl != 0)
	TREE_TYPE (decl) = type;
      valid = 1;
    }

  /* Handle putting a type attribute on pointer-to-function-type by putting
     the attribute on the function type.  */
  else if (POINTER_TYPE_P (type)
	   && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE
	   && VALID_MACHINE_TYPE_ATTRIBUTE (TREE_TYPE (type), type_attr_list,
					    attr_name, attr_args))
    {
      tree inner_type = TREE_TYPE (type);
      tree inner_attr_list = TYPE_ATTRIBUTES (inner_type);
      tree attr = lookup_attribute (IDENTIFIER_POINTER (attr_name),
				    type_attr_list);

      if (attr != NULL_TREE)
	TREE_VALUE (attr) = attr_args;
      else
	{
	  inner_attr_list = tree_cons (attr_name, attr_args, inner_attr_list);
	  inner_type = build_type_attribute_variant (inner_type,
						     inner_attr_list);
	}

      if (decl != 0)
	TREE_TYPE (decl) = build_pointer_type (inner_type);

      valid = 1;
    }
#endif

  return valid;
}

/* Return non-zero if IDENT is a valid name for attribute ATTR,
   or zero if not.

   We try both `text' and `__text__', ATTR may be either one.  */
/* ??? It might be a reasonable simplification to require ATTR to be only
   `text'.  One might then also require attribute lists to be stored in
   their canonicalized form.  */

int
is_attribute_p (attr, ident)
     char *attr;
     tree ident;
{
  int ident_len, attr_len;
  char *p;

  if (TREE_CODE (ident) != IDENTIFIER_NODE)
    return 0;

  if (strcmp (attr, IDENTIFIER_POINTER (ident)) == 0)
    return 1;

  p = IDENTIFIER_POINTER (ident);
  ident_len = strlen (p);
  attr_len = strlen (attr);

  /* If ATTR is `__text__', IDENT must be `text'; and vice versa.  */
  if (attr[0] == '_')
    {
      if (attr[1] != '_'
	  || attr[attr_len - 2] != '_'
	  || attr[attr_len - 1] != '_')
	abort ();
      if (ident_len == attr_len - 4
	  && strncmp (attr + 2, p, attr_len - 4) == 0)
	return 1;
    }
  else
    {
      if (ident_len == attr_len + 4
	  && p[0] == '_' && p[1] == '_'
	  && p[ident_len - 2] == '_' && p[ident_len - 1] == '_'
	  && strncmp (attr, p + 2, attr_len) == 0)
	return 1;
    }

  return 0;
}

/* Given an attribute name and a list of attributes, return a pointer to the
   attribute's list element if the attribute is part of the list, or NULL_TREE
   if not found.  */

tree
lookup_attribute (attr_name, list)
     char *attr_name;
     tree list;
{
  tree l;

  for (l = list; l; l = TREE_CHAIN (l))
    {
      if (TREE_CODE (TREE_PURPOSE (l)) != IDENTIFIER_NODE)
	abort ();
      if (is_attribute_p (attr_name, TREE_PURPOSE (l)))
	return l;
    }

  return NULL_TREE;
}

/* Return an attribute list that is the union of a1 and a2.  */

tree
merge_attributes (a1, a2)
     register tree a1, a2;
{
  tree attributes;

  /* Either one unset?  Take the set one.  */

  if (! (attributes = a1))
    attributes = a2;

  /* One that completely contains the other?  Take it.  */

  else if (a2 && ! attribute_list_contained (a1, a2))
    if (attribute_list_contained (a2, a1))
      attributes = a2;
    else
      {
	/* Pick the longest list, and hang on the other list.  */
	/* ??? For the moment we punt on the issue of attrs with args.  */

	if (list_length (a1) < list_length (a2))
	  attributes = a2, a2 = a1;

	for (; a2; a2 = TREE_CHAIN (a2))
	  if (lookup_attribute (IDENTIFIER_POINTER (TREE_PURPOSE (a2)),
				attributes) == NULL_TREE)
	    {
	      a1 = copy_node (a2);
	      TREE_CHAIN (a1) = attributes;
	      attributes = a1;
	    }
      }
  return attributes;
}

/* Return a type like TYPE except that its TYPE_READONLY is CONSTP
   and its TYPE_VOLATILE is VOLATILEP.

   Such variant types already made are recorded so that duplicates
   are not made.

   A variant types should never be used as the type of an expression.
   Always copy the variant information into the TREE_READONLY
   and TREE_THIS_VOLATILE of the expression, and then give the expression
   as its type the "main variant", the variant whose TYPE_READONLY
   and TYPE_VOLATILE are zero.  Use TYPE_MAIN_VARIANT to find the
   main variant.  */

tree
build_type_variant (type, constp, volatilep)
     tree type;
     int constp, volatilep;
{
  register tree t;

  /* Treat any nonzero argument as 1.  */
  constp = !!constp;
  volatilep = !!volatilep;

  /* Search the chain of variants to see if there is already one there just
     like the one we need to have.  If so, use that existing one.  We must
     preserve the TYPE_NAME, since there is code that depends on this.  */

  for (t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
    if (constp == TYPE_READONLY (t) && volatilep == TYPE_VOLATILE (t)
	&& TYPE_NAME (t) == TYPE_NAME (type))
      return t;

  /* We need a new one.  */

  t = build_type_copy (type);
  TYPE_READONLY (t) = constp;
  TYPE_VOLATILE (t) = volatilep;

  return t;
}

/* Create a new variant of TYPE, equivalent but distinct.
   This is so the caller can modify it.  */

tree
build_type_copy (type)
     tree type;
{
  register tree t, m = TYPE_MAIN_VARIANT (type);
  register struct obstack *ambient_obstack = current_obstack;

  current_obstack = TYPE_OBSTACK (type);
  t = copy_node (type);
  current_obstack = ambient_obstack;

  TYPE_POINTER_TO (t) = 0;
  TYPE_REFERENCE_TO (t) = 0;

  /* Add this type to the chain of variants of TYPE.  */
  TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (m);
  TYPE_NEXT_VARIANT (m) = t;

  return t;
}

/* Hashing of types so that we don't make duplicates.
   The entry point is `type_hash_canon'.  */

/* Each hash table slot is a bucket containing a chain
   of these structures.  */

struct type_hash
{
  struct type_hash *next;	/* Next structure in the bucket.  */
  int hashcode;			/* Hash code of this type.  */
  tree type;			/* The type recorded here.  */
};

/* Now here is the hash table.  When recording a type, it is added
   to the slot whose index is the hash code mod the table size.
   Note that the hash table is used for several kinds of types
   (function types, array types and array index range types, for now).
   While all these live in the same table, they are completely independent,
   and the hash code is computed differently for each of these.  */

#define TYPE_HASH_SIZE 59
struct type_hash *type_hash_table[TYPE_HASH_SIZE];

/* Compute a hash code for a list of types (chain of TREE_LIST nodes
   with types in the TREE_VALUE slots), by adding the hash codes
   of the individual types.  */

int
type_hash_list (list)
     tree list;
{
  register int hashcode;
  register tree tail;
  for (hashcode = 0, tail = list; tail; tail = TREE_CHAIN (tail))
    hashcode += TYPE_HASH (TREE_VALUE (tail));
  return hashcode;
}

/* Look in the type hash table for a type isomorphic to TYPE.
   If one is found, return it.  Otherwise return 0.  */

tree
type_hash_lookup (hashcode, type)
     int hashcode;
     tree type;
{
  register struct type_hash *h;
  for (h = type_hash_table[hashcode % TYPE_HASH_SIZE]; h; h = h->next)
    if (h->hashcode == hashcode
	&& TREE_CODE (h->type) == TREE_CODE (type)
	&& TREE_TYPE (h->type) == TREE_TYPE (type)
        && attribute_list_equal (TYPE_ATTRIBUTES (h->type),
				   TYPE_ATTRIBUTES (type))
	&& (TYPE_MAX_VALUE (h->type) == TYPE_MAX_VALUE (type)
	    || tree_int_cst_equal (TYPE_MAX_VALUE (h->type),
				   TYPE_MAX_VALUE (type)))
	&& (TYPE_MIN_VALUE (h->type) == TYPE_MIN_VALUE (type)
	    || tree_int_cst_equal (TYPE_MIN_VALUE (h->type),
				   TYPE_MIN_VALUE (type)))
	/* Note that TYPE_DOMAIN is TYPE_ARG_TYPES for FUNCTION_TYPE.  */
	&& (TYPE_DOMAIN (h->type) == TYPE_DOMAIN (type)
	    || (TYPE_DOMAIN (h->type)
		&& TREE_CODE (TYPE_DOMAIN (h->type)) == TREE_LIST
		&& TYPE_DOMAIN (type)
		&& TREE_CODE (TYPE_DOMAIN (type)) == TREE_LIST
		&& type_list_equal (TYPE_DOMAIN (h->type),
				    TYPE_DOMAIN (type)))))
      return h->type;
  return 0;
}

/* Add an entry to the type-hash-table
   for a type TYPE whose hash code is HASHCODE.  */

void
type_hash_add (hashcode, type)
     int hashcode;
     tree type;
{
  register struct type_hash *h;

  h = (struct type_hash *) oballoc (sizeof (struct type_hash));
  h->hashcode = hashcode;
  h->type = type;
  h->next = type_hash_table[hashcode % TYPE_HASH_SIZE];
  type_hash_table[hashcode % TYPE_HASH_SIZE] = h;
}

/* Given TYPE, and HASHCODE its hash code, return the canonical
   object for an identical type if one already exists.
   Otherwise, return TYPE, and record it as the canonical object
   if it is a permanent object.

   To use this function, first create a type of the sort you want.
   Then compute its hash code from the fields of the type that
   make it different from other similar types.
   Then call this function and use the value.
   This function frees the type you pass in if it is a duplicate.  */

/* Set to 1 to debug without canonicalization.  Never set by program.  */
int debug_no_type_hash = 0;

tree
type_hash_canon (hashcode, type)
     int hashcode;
     tree type;
{
  tree t1;

  if (debug_no_type_hash)
    return type;

  t1 = type_hash_lookup (hashcode, type);
  if (t1 != 0)
    {
      obstack_free (TYPE_OBSTACK (type), type);
#ifdef GATHER_STATISTICS
      tree_node_counts[(int)t_kind]--;
      tree_node_sizes[(int)t_kind] -= sizeof (struct tree_type);
#endif
      return t1;
    }

  /* If this is a permanent type, record it for later reuse.  */
  if (TREE_PERMANENT (type))
    type_hash_add (hashcode, type);

  return type;
}

/* Compute a hash code for a list of attributes (chain of TREE_LIST nodes
   with names in the TREE_PURPOSE slots and args in the TREE_VALUE slots),
   by adding the hash codes of the individual attributes.  */

int
attribute_hash_list (list)
     tree list;
{
  register int hashcode;
  register tree tail;
  for (hashcode = 0, tail = list; tail; tail = TREE_CHAIN (tail))
    /* ??? Do we want to add in TREE_VALUE too? */
    hashcode += TYPE_HASH (TREE_PURPOSE (tail));
  return hashcode;
}

/* Given two lists of attributes, return true if list l2 is
   equivalent to l1.  */

int
attribute_list_equal (l1, l2)
     tree l1, l2;
{
   return attribute_list_contained (l1, l2)
	  && attribute_list_contained (l2, l1);
}

/* Given two lists of attributes, return true if list L2 is
   completely contained within L1.  */
/* ??? This would be faster if attribute names were stored in a canonicalized
   form.  Otherwise, if L1 uses `foo' and L2 uses `__foo__', the long method
   must be used to show these elements are equivalent (which they are).  */
/* ??? It's not clear that attributes with arguments will always be handled
   correctly.  */

int
attribute_list_contained (l1, l2)
     tree l1, l2;
{
  register tree t1, t2;

  /* First check the obvious, maybe the lists are identical.  */
  if (l1 == l2)
     return 1;

  /* Maybe the lists are similar.  */
  for (t1 = l1, t2 = l2;
       t1 && t2
        && TREE_PURPOSE (t1) == TREE_PURPOSE (t2)
        && TREE_VALUE (t1) == TREE_VALUE (t2);
       t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2));

  /* Maybe the lists are equal.  */
  if (t1 == 0 && t2 == 0)
     return 1;

  for (; t2; t2 = TREE_CHAIN (t2))
    {
      tree attr
	= lookup_attribute (IDENTIFIER_POINTER (TREE_PURPOSE (t2)), l1);

      if (attr == NULL_TREE)
	return 0;
      if (simple_cst_equal (TREE_VALUE (t2), TREE_VALUE (attr)) != 1)
	return 0;
    }

  return 1;
}

/* Given two lists of types
   (chains of TREE_LIST nodes with types in the TREE_VALUE slots)
   return 1 if the lists contain the same types in the same order.
   Also, the TREE_PURPOSEs must match.  */

int
type_list_equal (l1, l2)
     tree l1, l2;
{
  register tree t1, t2;

  for (t1 = l1, t2 = l2; t1 && t2; t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2))
    if (TREE_VALUE (t1) != TREE_VALUE (t2)
	|| (TREE_PURPOSE (t1) != TREE_PURPOSE (t2)
	    && ! (1 == simple_cst_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2))
		  && (TREE_TYPE (TREE_PURPOSE (t1))
		      == TREE_TYPE (TREE_PURPOSE (t2))))))
      return 0;

  return t1 == t2;
}

/* Nonzero if integer constants T1 and T2
   represent the same constant value.  */

int
tree_int_cst_equal (t1, t2)
     tree t1, t2;
{
  if (t1 == t2)
    return 1;
  if (t1 == 0 || t2 == 0)
    return 0;
  if (TREE_CODE (t1) == INTEGER_CST
      && TREE_CODE (t2) == INTEGER_CST
      && TREE_INT_CST_LOW (t1) == TREE_INT_CST_LOW (t2)
      && TREE_INT_CST_HIGH (t1) == TREE_INT_CST_HIGH (t2))
    return 1;
  return 0;
}

/* Nonzero if integer constants T1 and T2 represent values that satisfy <.
   The precise way of comparison depends on their data type.  */

int
tree_int_cst_lt (t1, t2)
     tree t1, t2;
{
  if (t1 == t2)
    return 0;

  if (!TREE_UNSIGNED (TREE_TYPE (t1)))
    return INT_CST_LT (t1, t2);
  return INT_CST_LT_UNSIGNED (t1, t2);
}

/* Return an indication of the sign of the integer constant T.
   The return value is -1 if T < 0, 0 if T == 0, and 1 if T > 0.
   Note that -1 will never be returned it T's type is unsigned.  */

int
tree_int_cst_sgn (t)
     tree t;
{
  if (TREE_INT_CST_LOW (t) == 0 && TREE_INT_CST_HIGH (t) == 0)
    return 0;
  else if (TREE_UNSIGNED (TREE_TYPE (t)))
    return 1;
  else if (TREE_INT_CST_HIGH (t) < 0)
    return -1;
  else
    return 1;
}

/* Compare two constructor-element-type constants.  Return 1 if the lists
   are known to be equal; otherwise return 0.  */

int
simple_cst_list_equal (l1, l2)
     tree l1, l2;
{
  while (l1 != NULL_TREE && l2 != NULL_TREE)
    {
      if (simple_cst_equal (TREE_VALUE (l1), TREE_VALUE (l2)) != 1)
	return 0;

      l1 = TREE_CHAIN (l1);
      l2 = TREE_CHAIN (l2);
    }

  return (l1 == l2);
}

/* Return truthvalue of whether T1 is the same tree structure as T2.
   Return 1 if they are the same.
   Return 0 if they are understandably different.
   Return -1 if either contains tree structure not understood by
   this function.  */

int
simple_cst_equal (t1, t2)
     tree t1, t2;
{
  register enum tree_code code1, code2;
  int cmp;

  if (t1 == t2)
    return 1;
  if (t1 == 0 || t2 == 0)
    return 0;

  code1 = TREE_CODE (t1);
  code2 = TREE_CODE (t2);

  if (code1 == NOP_EXPR || code1 == CONVERT_EXPR || code1 == NON_LVALUE_EXPR)
    if (code2 == NOP_EXPR || code2 == CONVERT_EXPR || code2 == NON_LVALUE_EXPR)
      return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
    else
      return simple_cst_equal (TREE_OPERAND (t1, 0), t2);
  else if (code2 == NOP_EXPR || code2 == CONVERT_EXPR
	   || code2 == NON_LVALUE_EXPR)
    return simple_cst_equal (t1, TREE_OPERAND (t2, 0));

  if (code1 != code2)
    return 0;

  switch (code1)
    {
    case INTEGER_CST:
      return TREE_INT_CST_LOW (t1) == TREE_INT_CST_LOW (t2)
	&& TREE_INT_CST_HIGH (t1) == TREE_INT_CST_HIGH (t2);

    case REAL_CST:
      return REAL_VALUES_IDENTICAL (TREE_REAL_CST (t1), TREE_REAL_CST (t2));

    case STRING_CST:
      return TREE_STRING_LENGTH (t1) == TREE_STRING_LENGTH (t2)
	&& !bcmp (TREE_STRING_POINTER (t1), TREE_STRING_POINTER (t2),
		  TREE_STRING_LENGTH (t1));

    case CONSTRUCTOR:
      abort ();

    case SAVE_EXPR:
      return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

    case CALL_EXPR:
      cmp = simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      if (cmp <= 0)
	return cmp;
      return simple_cst_list_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));

    case TARGET_EXPR:
      /* Special case: if either target is an unallocated VAR_DECL,
	 it means that it's going to be unified with whatever the
	 TARGET_EXPR is really supposed to initialize, so treat it
	 as being equivalent to anything.  */
      if ((TREE_CODE (TREE_OPERAND (t1, 0)) == VAR_DECL
	   && DECL_NAME (TREE_OPERAND (t1, 0)) == NULL_TREE
	   && DECL_RTL (TREE_OPERAND (t1, 0)) == 0)
	  || (TREE_CODE (TREE_OPERAND (t2, 0)) == VAR_DECL
	      && DECL_NAME (TREE_OPERAND (t2, 0)) == NULL_TREE
	      && DECL_RTL (TREE_OPERAND (t2, 0)) == 0))
	cmp = 1;
      else
	cmp = simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      if (cmp <= 0)
	return cmp;
      return simple_cst_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));

    case WITH_CLEANUP_EXPR:
      cmp = simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      if (cmp <= 0)
	return cmp;
      return simple_cst_equal (TREE_OPERAND (t1, 2), TREE_OPERAND (t1, 2));

    case COMPONENT_REF:
      if (TREE_OPERAND (t1, 1) == TREE_OPERAND (t2, 1))
	return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      return 0;

    case VAR_DECL:
    case PARM_DECL:
    case CONST_DECL:
    case FUNCTION_DECL:
      return 0;
      
    default:
      break;
    }

  /* This general rule works for most tree codes.  All exceptions should be
     handled above.  If this is a language-specific tree code, we can't
     trust what might be in the operand, so say we don't know
     the situation.  */
  if ((int) code1
      >= sizeof standard_tree_code_type / sizeof standard_tree_code_type[0])
    return -1;

  switch (TREE_CODE_CLASS (code1))
    {
      int i;
    case '1':
    case '2':
    case '<':
    case 'e':
    case 'r':
    case 's':
      cmp = 1;
      for (i=0; i<tree_code_length[(int) code1]; ++i)
	{
	  cmp = simple_cst_equal (TREE_OPERAND (t1, i), TREE_OPERAND (t2, i));
	  if (cmp <= 0)
	    return cmp;
	}
      return cmp;

    default:
      return -1;
    }
}

/* Constructors for pointer, array and function types.
   (RECORD_TYPE, UNION_TYPE and ENUMERAL_TYPE nodes are
   constructed by language-dependent code, not here.)  */

/* Construct, lay out and return the type of pointers to TO_TYPE.
   If such a type has already been constructed, reuse it.  */

tree
build_pointer_type (to_type)
     tree to_type;
{
  register tree t = TYPE_POINTER_TO (to_type);

  /* First, if we already have a type for pointers to TO_TYPE, use it.  */

  if (t)
    return t;

  /* We need a new one.  Put this in the same obstack as TO_TYPE.   */
  push_obstacks (TYPE_OBSTACK (to_type), TYPE_OBSTACK (to_type));
  t = make_node (POINTER_TYPE);
  pop_obstacks ();

  TREE_TYPE (t) = to_type;

  /* Record this type as the pointer to TO_TYPE.  */
  TYPE_POINTER_TO (to_type) = t;

  /* Lay out the type.  This function has many callers that are concerned
     with expression-construction, and this simplifies them all.
     Also, it guarantees the TYPE_SIZE is in the same obstack as the type.  */
  layout_type (t);

  return t;
}

/* Create a type of integers to be the TYPE_DOMAIN of an ARRAY_TYPE.
   MAXVAL should be the maximum value in the domain
   (one less than the length of the array).

   The maximum value that MAXVAL can have is INT_MAX for a HOST_WIDE_INT.
   We don't enforce this limit, that is up to caller (e.g. language front end).
   The limit exists because the result is a signed type and we don't handle
   sizes that use more than one HOST_WIDE_INT.  */

tree
build_index_type (maxval)
     tree maxval;
{
  register tree itype = make_node (INTEGER_TYPE);

  TYPE_PRECISION (itype) = TYPE_PRECISION (sizetype);
  TYPE_MIN_VALUE (itype) = size_zero_node;

  push_obstacks (TYPE_OBSTACK (itype), TYPE_OBSTACK (itype));
  TYPE_MAX_VALUE (itype) = convert (sizetype, maxval);
  pop_obstacks ();

  TYPE_MODE (itype) = TYPE_MODE (sizetype);
  TYPE_SIZE (itype) = TYPE_SIZE (sizetype);
  TYPE_ALIGN (itype) = TYPE_ALIGN (sizetype);
  if (TREE_CODE (maxval) == INTEGER_CST)
    {
      int maxint = (int) TREE_INT_CST_LOW (maxval);
      /* If the domain should be empty, make sure the maxval
	 remains -1 and is not spoiled by truncation.  */
      if (INT_CST_LT (maxval, integer_zero_node))
	{
	  TYPE_MAX_VALUE (itype) = build_int_2 (-1, -1);
	  TREE_TYPE (TYPE_MAX_VALUE (itype)) = sizetype;
	}
      return type_hash_canon (maxint < 0 ? ~maxint : maxint, itype);
    }
  else
    return itype;
}

/* Create a range of some discrete type TYPE (an INTEGER_TYPE,
   ENUMERAL_TYPE, BOOLEAN_TYPE, or CHAR_TYPE), with
   low bound LOWVAL and high bound HIGHVAL.
   if TYPE==NULL_TREE, sizetype is used.  */

tree
build_range_type (type, lowval, highval)
     tree type, lowval, highval;
{
  register tree itype = make_node (INTEGER_TYPE);

  TREE_TYPE (itype) = type;
  if (type == NULL_TREE)
    type = sizetype;

  push_obstacks (TYPE_OBSTACK (itype), TYPE_OBSTACK (itype));
  TYPE_MIN_VALUE (itype) = convert (type, lowval);
  TYPE_MAX_VALUE (itype) = convert (type, highval);
  pop_obstacks ();

  TYPE_PRECISION (itype) = TYPE_PRECISION (type);
  TYPE_MODE (itype) = TYPE_MODE (type);
  TYPE_SIZE (itype) = TYPE_SIZE (type);
  TYPE_ALIGN (itype) = TYPE_ALIGN (type);
  if ((TREE_CODE (lowval) == INTEGER_CST)
      && (TREE_CODE (highval) == INTEGER_CST))
    {
      HOST_WIDE_INT highint = TREE_INT_CST_LOW (highval);
      HOST_WIDE_INT lowint = TREE_INT_CST_LOW (lowval);
      int maxint = (int) (highint - lowint);
      return type_hash_canon (maxint < 0 ? ~maxint : maxint, itype);
    }
  else
    return itype;
}

/* Just like build_index_type, but takes lowval and highval instead
   of just highval (maxval).  */

tree
build_index_2_type (lowval,highval)
     tree lowval, highval;
{
  return build_range_type (NULL_TREE, lowval, highval);
}

/* Return nonzero iff ITYPE1 and ITYPE2 are equal (in the LISP sense).
   Needed because when index types are not hashed, equal index types
   built at different times appear distinct, even though structurally,
   they are not.  */

int
index_type_equal (itype1, itype2)
     tree itype1, itype2;
{
  if (TREE_CODE (itype1) != TREE_CODE (itype2))
    return 0;
  if (TREE_CODE (itype1) == INTEGER_TYPE)
    {
      if (TYPE_PRECISION (itype1) != TYPE_PRECISION (itype2)
	  || TYPE_MODE (itype1) != TYPE_MODE (itype2)
	  || simple_cst_equal (TYPE_SIZE (itype1), TYPE_SIZE (itype2)) != 1
	  || TYPE_ALIGN (itype1) != TYPE_ALIGN (itype2))
	return 0;
      if (1 == simple_cst_equal (TYPE_MIN_VALUE (itype1),
				 TYPE_MIN_VALUE (itype2))
	  && 1 == simple_cst_equal (TYPE_MAX_VALUE (itype1),
				    TYPE_MAX_VALUE (itype2)))
	return 1;
    }

  return 0;
}

/* Construct, lay out and return the type of arrays of elements with ELT_TYPE
   and number of elements specified by the range of values of INDEX_TYPE.
   If such a type has already been constructed, reuse it.  */

tree
build_array_type (elt_type, index_type)
     tree elt_type, index_type;
{
  register tree t;
  int hashcode;

  if (TREE_CODE (elt_type) == FUNCTION_TYPE)
    {
      error ("arrays of functions are not meaningful");
      elt_type = integer_type_node;
    }

  /* Make sure TYPE_POINTER_TO (elt_type) is filled in.  */
  build_pointer_type (elt_type);

  /* Allocate the array after the pointer type,
     in case we free it in type_hash_canon.  */
  t = make_node (ARRAY_TYPE);
  TREE_TYPE (t) = elt_type;
  TYPE_DOMAIN (t) = index_type;

  if (index_type == 0)
    {
      return t;
    }

  hashcode = TYPE_HASH (elt_type) + TYPE_HASH (index_type);
  t = type_hash_canon (hashcode, t);

  if (TYPE_SIZE (t) == 0)
    layout_type (t);
  return t;
}

/* Construct, lay out and return
   the type of functions returning type VALUE_TYPE
   given arguments of types ARG_TYPES.
   ARG_TYPES is a chain of TREE_LIST nodes whose TREE_VALUEs
   are data type nodes for the arguments of the function.
   If such a type has already been constructed, reuse it.  */

tree
build_function_type (value_type, arg_types)
     tree value_type, arg_types;
{
  register tree t;
  int hashcode;

  if (TREE_CODE (value_type) == FUNCTION_TYPE)
    {
      error ("function return type cannot be function");
      value_type = integer_type_node;
    }

  /* Make a node of the sort we want.  */
  t = make_node (FUNCTION_TYPE);
  TREE_TYPE (t) = value_type;
  TYPE_ARG_TYPES (t) = arg_types;

  /* If we already have such a type, use the old one and free this one.  */
  hashcode = TYPE_HASH (value_type) + type_hash_list (arg_types);
  t = type_hash_canon (hashcode, t);

  if (TYPE_SIZE (t) == 0)
    layout_type (t);
  return t;
}

/* Build the node for the type of references-to-TO_TYPE.  */

tree
build_reference_type (to_type)
     tree to_type;
{
  register tree t = TYPE_REFERENCE_TO (to_type);

  /* First, if we already have a type for pointers to TO_TYPE, use it.  */

  if (t)
    return t;

  /* We need a new one.  Put this in the same obstack as TO_TYPE.   */
  push_obstacks (TYPE_OBSTACK (to_type), TYPE_OBSTACK (to_type));
  t = make_node (REFERENCE_TYPE);
  pop_obstacks ();

  TREE_TYPE (t) = to_type;

  /* Record this type as the pointer to TO_TYPE.  */
  TYPE_REFERENCE_TO (to_type) = t;

  layout_type (t);

  return t;
}

/* Construct, lay out and return the type of methods belonging to class
   BASETYPE and whose arguments and values are described by TYPE.
   If that type exists already, reuse it.
   TYPE must be a FUNCTION_TYPE node.  */

tree
build_method_type (basetype, type)
     tree basetype, type;
{
  register tree t;
  int hashcode;

  /* Make a node of the sort we want.  */
  t = make_node (METHOD_TYPE);

  if (TREE_CODE (type) != FUNCTION_TYPE)
    abort ();

  TYPE_METHOD_BASETYPE (t) = TYPE_MAIN_VARIANT (basetype);
  TREE_TYPE (t) = TREE_TYPE (type);

  /* The actual arglist for this function includes a "hidden" argument
     which is "this".  Put it into the list of argument types.  */

  TYPE_ARG_TYPES (t)
    = tree_cons (NULL_TREE,
		 build_pointer_type (basetype), TYPE_ARG_TYPES (type));

  /* If we already have such a type, use the old one and free this one.  */
  hashcode = TYPE_HASH (basetype) + TYPE_HASH (type);
  t = type_hash_canon (hashcode, t);

  if (TYPE_SIZE (t) == 0)
    layout_type (t);

  return t;
}

/* Construct, lay out and return the type of offsets to a value
   of type TYPE, within an object of type BASETYPE.
   If a suitable offset type exists already, reuse it.  */

tree
build_offset_type (basetype, type)
     tree basetype, type;
{
  register tree t;
  int hashcode;

  /* Make a node of the sort we want.  */
  t = make_node (OFFSET_TYPE);

  TYPE_OFFSET_BASETYPE (t) = TYPE_MAIN_VARIANT (basetype);
  TREE_TYPE (t) = type;

  /* If we already have such a type, use the old one and free this one.  */
  hashcode = TYPE_HASH (basetype) + TYPE_HASH (type);
  t = type_hash_canon (hashcode, t);

  if (TYPE_SIZE (t) == 0)
    layout_type (t);

  return t;
}

/* Create a complex type whose components are COMPONENT_TYPE.  */

tree
build_complex_type (component_type)
     tree component_type;
{
  register tree t;
  int hashcode;

  /* Make a node of the sort we want.  */
  t = make_node (COMPLEX_TYPE);

  TREE_TYPE (t) = TYPE_MAIN_VARIANT (component_type);
  TYPE_VOLATILE (t) = TYPE_VOLATILE (component_type);
  TYPE_READONLY (t) = TYPE_READONLY (component_type);

  /* If we already have such a type, use the old one and free this one.  */
  hashcode = TYPE_HASH (component_type);
  t = type_hash_canon (hashcode, t);

  if (TYPE_SIZE (t) == 0)
    layout_type (t);

  return t;
}

/* Return OP, stripped of any conversions to wider types as much as is safe.
   Converting the value back to OP's type makes a value equivalent to OP.

   If FOR_TYPE is nonzero, we return a value which, if converted to
   type FOR_TYPE, would be equivalent to converting OP to type FOR_TYPE.

   If FOR_TYPE is nonzero, unaligned bit-field references may be changed to the
   narrowest type that can hold the value, even if they don't exactly fit.
   Otherwise, bit-field references are changed to a narrower type
   only if they can be fetched directly from memory in that type.

   OP must have integer, real or enumeral type.  Pointers are not allowed!

   There are some cases where the obvious value we could return
   would regenerate to OP if converted to OP's type, 
   but would not extend like OP to wider types.
   If FOR_TYPE indicates such extension is contemplated, we eschew such values.
   For example, if OP is (unsigned short)(signed char)-1,
   we avoid returning (signed char)-1 if FOR_TYPE is int,
   even though extending that to an unsigned short would regenerate OP,
   since the result of extending (signed char)-1 to (int)
   is different from (int) OP.  */

tree
get_unwidened (op, for_type)
     register tree op;
     tree for_type;
{
  /* Set UNS initially if converting OP to FOR_TYPE is a zero-extension.  */
  /* TYPE_PRECISION is safe in place of type_precision since
     pointer types are not allowed.  */
  register tree type = TREE_TYPE (op);
  register unsigned final_prec
    = TYPE_PRECISION (for_type != 0 ? for_type : type);
  register int uns
    = (for_type != 0 && for_type != type
       && final_prec > TYPE_PRECISION (type)
       && TREE_UNSIGNED (type));
  register tree win = op;

  while (TREE_CODE (op) == NOP_EXPR)
    {
      register int bitschange
	= TYPE_PRECISION (TREE_TYPE (op))
	  - TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op, 0)));

      /* Truncations are many-one so cannot be removed.
	 Unless we are later going to truncate down even farther.  */
      if (bitschange < 0
	  && final_prec > TYPE_PRECISION (TREE_TYPE (op)))
	break;

      /* See what's inside this conversion.  If we decide to strip it,
	 we will set WIN.  */
      op = TREE_OPERAND (op, 0);

      /* If we have not stripped any zero-extensions (uns is 0),
	 we can strip any kind of extension.
	 If we have previously stripped a zero-extension,
	 only zero-extensions can safely be stripped.
	 Any extension can be stripped if the bits it would produce
	 are all going to be discarded later by truncating to FOR_TYPE.  */

      if (bitschange > 0)
	{
	  if (! uns || final_prec <= TYPE_PRECISION (TREE_TYPE (op)))
	    win = op;
	  /* TREE_UNSIGNED says whether this is a zero-extension.
	     Let's avoid computing it if it does not affect WIN
	     and if UNS will not be needed again.  */
	  if ((uns || TREE_CODE (op) == NOP_EXPR)
	      && TREE_UNSIGNED (TREE_TYPE (op)))
	    {
	      uns = 1;
	      win = op;
	    }
	}
    }

  if (TREE_CODE (op) == COMPONENT_REF
      /* Since type_for_size always gives an integer type.  */
      && TREE_CODE (type) != REAL_TYPE
      /* Don't crash if field not laid out yet.  */
      && DECL_SIZE (TREE_OPERAND (op, 1)) != 0)
    {
      unsigned innerprec = TREE_INT_CST_LOW (DECL_SIZE (TREE_OPERAND (op, 1)));
      type = type_for_size (innerprec, TREE_UNSIGNED (TREE_OPERAND (op, 1)));

      /* We can get this structure field in the narrowest type it fits in.
	 If FOR_TYPE is 0, do this only for a field that matches the
	 narrower type exactly and is aligned for it
	 The resulting extension to its nominal type (a fullword type)
	 must fit the same conditions as for other extensions.  */

      if (innerprec < TYPE_PRECISION (TREE_TYPE (op))
	  && (for_type || ! DECL_BIT_FIELD (TREE_OPERAND (op, 1)))
	  && (! uns || final_prec <= innerprec
	      || TREE_UNSIGNED (TREE_OPERAND (op, 1)))
	  && type != 0)
	{
	  win = build (COMPONENT_REF, type, TREE_OPERAND (op, 0),
		       TREE_OPERAND (op, 1));
	  TREE_SIDE_EFFECTS (win) = TREE_SIDE_EFFECTS (op);
	  TREE_THIS_VOLATILE (win) = TREE_THIS_VOLATILE (op);
	  TREE_RAISES (win) = TREE_RAISES (op);
	}
    }
  return win;
}

/* Return OP or a simpler expression for a narrower value
   which can be sign-extended or zero-extended to give back OP.
   Store in *UNSIGNEDP_PTR either 1 if the value should be zero-extended
   or 0 if the value should be sign-extended.  */

tree
get_narrower (op, unsignedp_ptr)
     register tree op;
     int *unsignedp_ptr;
{
  register int uns = 0;
  int first = 1;
  register tree win = op;

  while (TREE_CODE (op) == NOP_EXPR)
    {
      register int bitschange
	= TYPE_PRECISION (TREE_TYPE (op))
	  - TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op, 0)));

      /* Truncations are many-one so cannot be removed.  */
      if (bitschange < 0)
	break;

      /* See what's inside this conversion.  If we decide to strip it,
	 we will set WIN.  */
      op = TREE_OPERAND (op, 0);

      if (bitschange > 0)
	{
	  /* An extension: the outermost one can be stripped,
	     but remember whether it is zero or sign extension.  */
	  if (first)
	    uns = TREE_UNSIGNED (TREE_TYPE (op));
	  /* Otherwise, if a sign extension has been stripped,
	     only sign extensions can now be stripped;
	     if a zero extension has been stripped, only zero-extensions.  */
	  else if (uns != TREE_UNSIGNED (TREE_TYPE (op)))
	    break;
	  first = 0;
	}
      else /* bitschange == 0 */
	{
	  /* A change in nominal type can always be stripped, but we must
	     preserve the unsignedness.  */
	  if (first)
	    uns = TREE_UNSIGNED (TREE_TYPE (op));
	  first = 0;
	}

      win = op;
    }

  if (TREE_CODE (op) == COMPONENT_REF
      /* Since type_for_size always gives an integer type.  */
      && TREE_CODE (TREE_TYPE (op)) != REAL_TYPE)
    {
      unsigned innerprec = TREE_INT_CST_LOW (DECL_SIZE (TREE_OPERAND (op, 1)));
      tree type = type_for_size (innerprec, TREE_UNSIGNED (op));

      /* We can get this structure field in a narrower type that fits it,
	 but the resulting extension to its nominal type (a fullword type)
	 must satisfy the same conditions as for other extensions.

	 Do this only for fields that are aligned (not bit-fields),
	 because when bit-field insns will be used there is no
	 advantage in doing this.  */

      if (innerprec < TYPE_PRECISION (TREE_TYPE (op))
	  && ! DECL_BIT_FIELD (TREE_OPERAND (op, 1))
	  && (first || uns == TREE_UNSIGNED (TREE_OPERAND (op, 1)))
	  && type != 0)
	{
	  if (first)
	    uns = TREE_UNSIGNED (TREE_OPERAND (op, 1));
	  win = build (COMPONENT_REF, type, TREE_OPERAND (op, 0),
		       TREE_OPERAND (op, 1));
	  TREE_SIDE_EFFECTS (win) = TREE_SIDE_EFFECTS (op);
	  TREE_THIS_VOLATILE (win) = TREE_THIS_VOLATILE (op);
	  TREE_RAISES (win) = TREE_RAISES (op);
	}
    }
  *unsignedp_ptr = uns;
  return win;
}

/* Return the precision of a type, for arithmetic purposes.
   Supports all types on which arithmetic is possible
   (including pointer types).
   It's not clear yet what will be right for complex types.  */

int
type_precision (type)
     register tree type;
{
  return ((TREE_CODE (type) == INTEGER_TYPE
	   || TREE_CODE (type) == ENUMERAL_TYPE
	   || TREE_CODE (type) == REAL_TYPE)
	  ? TYPE_PRECISION (type) : POINTER_SIZE);
}

/* Nonzero if integer constant C has a value that is permissible
   for type TYPE (an INTEGER_TYPE).  */

int
int_fits_type_p (c, type)
     tree c, type;
{
  if (TREE_UNSIGNED (type))
    return (! (TREE_CODE (TYPE_MAX_VALUE (type)) == INTEGER_CST
	       && INT_CST_LT_UNSIGNED (TYPE_MAX_VALUE (type), c))
	    && ! (TREE_CODE (TYPE_MIN_VALUE (type)) == INTEGER_CST
		  && INT_CST_LT_UNSIGNED (c, TYPE_MIN_VALUE (type)))
	    /* Negative ints never fit unsigned types.  */
	    && ! (TREE_INT_CST_HIGH (c) < 0
		  && ! TREE_UNSIGNED (TREE_TYPE (c))));
  else
    return (! (TREE_CODE (TYPE_MAX_VALUE (type)) == INTEGER_CST
	       && INT_CST_LT (TYPE_MAX_VALUE (type), c))
	    && ! (TREE_CODE (TYPE_MIN_VALUE (type)) == INTEGER_CST
		  && INT_CST_LT (c, TYPE_MIN_VALUE (type)))
	    /* Unsigned ints with top bit set never fit signed types.  */
	    && ! (TREE_INT_CST_HIGH (c) < 0
		  && TREE_UNSIGNED (TREE_TYPE (c))));
}

/* Return the innermost context enclosing DECL that is
   a FUNCTION_DECL, or zero if none.  */

tree
decl_function_context (decl)
     tree decl;
{
  tree context;

  if (TREE_CODE (decl) == ERROR_MARK)
    return 0;

  if (TREE_CODE (decl) == SAVE_EXPR)
    context = SAVE_EXPR_CONTEXT (decl);
  else
    context = DECL_CONTEXT (decl);

  while (context && TREE_CODE (context) != FUNCTION_DECL)
    {
      if (TREE_CODE (context) == RECORD_TYPE
	  || TREE_CODE (context) == UNION_TYPE
	  || TREE_CODE (context) == QUAL_UNION_TYPE)
	context = TYPE_CONTEXT (context);
      else if (TREE_CODE (context) == TYPE_DECL)
	context = DECL_CONTEXT (context);
      else if (TREE_CODE (context) == BLOCK)
	context = BLOCK_SUPERCONTEXT (context);
      else
	/* Unhandled CONTEXT !?  */
	abort ();
    }

  return context;
}

/* Return the innermost context enclosing DECL that is
   a RECORD_TYPE, UNION_TYPE or QUAL_UNION_TYPE, or zero if none.
   TYPE_DECLs and FUNCTION_DECLs are transparent to this function.  */

tree
decl_type_context (decl)
     tree decl;
{
  tree context = DECL_CONTEXT (decl);

  while (context)
    {
      if (TREE_CODE (context) == RECORD_TYPE
	  || TREE_CODE (context) == UNION_TYPE
	  || TREE_CODE (context) == QUAL_UNION_TYPE)
	return context;
      if (TREE_CODE (context) == TYPE_DECL
	  || TREE_CODE (context) == FUNCTION_DECL)
	context = DECL_CONTEXT (context);
      else if (TREE_CODE (context) == BLOCK)
	context = BLOCK_SUPERCONTEXT (context);
      else
	/* Unhandled CONTEXT!?  */
	abort ();
    }
  return NULL_TREE;
}

/* Print debugging information about the size of the
   toplev_inline_obstacks.  */

void
print_inline_obstack_statistics ()
{
  struct simple_obstack_stack *current = toplev_inline_obstacks;
  int n_obstacks = 0;
  int n_alloc = 0;
  int n_chunks = 0;

  for (; current; current = current->next, ++n_obstacks)
    {
      struct obstack *o = current->obstack;
      struct _obstack_chunk *chunk = o->chunk;

      n_alloc += o->next_free - chunk->contents;
      chunk = chunk->prev;
      ++n_chunks;
      for (; chunk; chunk = chunk->prev, ++n_chunks)
	n_alloc += chunk->limit - &chunk->contents[0];
    }
  fprintf (stderr, "inline obstacks: %d obstacks, %d bytes, %d chunks\n",
	   n_obstacks, n_alloc, n_chunks);
}

/* Print debugging information about the obstack O, named STR.  */

void
print_obstack_statistics (str, o)
     char *str;
     struct obstack *o;
{
  struct _obstack_chunk *chunk = o->chunk;
  int n_chunks = 1;
  int n_alloc = 0;

  n_alloc += o->next_free - chunk->contents;
  chunk = chunk->prev;
  while (chunk)
    {
      n_chunks += 1;
      n_alloc += chunk->limit - &chunk->contents[0];
      chunk = chunk->prev;
    }
  fprintf (stderr, "obstack %s: %d bytes, %d chunks\n",
	   str, n_alloc, n_chunks);
}

/* Print debugging information about tree nodes generated during the compile,
   and any language-specific information.  */

void
dump_tree_statistics ()
{
  int i;
  int total_nodes, total_bytes;

  fprintf (stderr, "\n??? tree nodes created\n\n");
#ifdef GATHER_STATISTICS
  fprintf (stderr, "Kind                  Nodes     Bytes\n");
  fprintf (stderr, "-------------------------------------\n");
  total_nodes = total_bytes = 0;
  for (i = 0; i < (int) all_kinds; i++)
    {
      fprintf (stderr, "%-20s %6d %9d\n", tree_node_kind_names[i],
	       tree_node_counts[i], tree_node_sizes[i]);
      total_nodes += tree_node_counts[i];
      total_bytes += tree_node_sizes[i];
    }
  fprintf (stderr, "%-20s        %9d\n", "identifier names", id_string_size);
  fprintf (stderr, "-------------------------------------\n");
  fprintf (stderr, "%-20s %6d %9d\n", "Total", total_nodes, total_bytes);
  fprintf (stderr, "-------------------------------------\n");
#else
  fprintf (stderr, "(No per-node statistics)\n");
#endif
  print_obstack_statistics ("permanent_obstack", &permanent_obstack);
  print_obstack_statistics ("maybepermanent_obstack", &maybepermanent_obstack);
  print_obstack_statistics ("temporary_obstack", &temporary_obstack);
  print_obstack_statistics ("momentary_obstack", &momentary_obstack);
  print_obstack_statistics ("temp_decl_obstack", &temp_decl_obstack);
  print_inline_obstack_statistics ();
  print_lang_statistics ();
}

#define FILE_FUNCTION_PREFIX_LEN 9

#ifndef NO_DOLLAR_IN_LABEL
#define FILE_FUNCTION_FORMAT "_GLOBAL_$D$%s"
#else /* NO_DOLLAR_IN_LABEL */
#ifndef NO_DOT_IN_LABEL
#define FILE_FUNCTION_FORMAT "_GLOBAL_.D.%s"
#else /* NO_DOT_IN_LABEL */
#define FILE_FUNCTION_FORMAT "_GLOBAL__D_%s"
#endif	/* NO_DOT_IN_LABEL */
#endif	/* NO_DOLLAR_IN_LABEL */

extern char * first_global_object_name;

/* If KIND=='I', return a suitable global initializer (constructor) name.
   If KIND=='D', return a suitable global clean-up (destructor) name.  */

tree
get_file_function_name (kind)
     int kind;
{
  char *buf;
  register char *p;

  if (first_global_object_name)
    p = first_global_object_name;
  else if (main_input_filename)
    p = main_input_filename;
  else
    p = input_filename;

  buf = (char *) alloca (sizeof (FILE_FUNCTION_FORMAT) + strlen (p));

  /* Set up the name of the file-level functions we may need.  */
  /* Use a global object (which is already required to be unique over
     the program) rather than the file name (which imposes extra
     constraints).  -- Raeburn@MIT.EDU, 10 Jan 1990.  */
  sprintf (buf, FILE_FUNCTION_FORMAT, p);

  /* Don't need to pull weird characters out of global names.  */
  if (p != first_global_object_name)
    {
      for (p = buf+11; *p; p++)
	if (! ((*p >= '0' && *p <= '9')
#if 0 /* we always want labels, which are valid C++ identifiers (+ `$') */
#ifndef ASM_IDENTIFY_GCC	/* this is required if `.' is invalid -- k. raeburn */
	       || *p == '.'
#endif
#endif
#ifndef NO_DOLLAR_IN_LABEL	/* this for `$'; unlikely, but... -- kr */
	       || *p == '$'
#endif
#ifndef NO_DOT_IN_LABEL		/* this for `.'; unlikely, but...  */
	       || *p == '.'
#endif
	       || (*p >= 'A' && *p <= 'Z')
	       || (*p >= 'a' && *p <= 'z')))
	  *p = '_';
    }

  buf[FILE_FUNCTION_PREFIX_LEN] = kind;

  return get_identifier (buf);
}

/* Expand (the constant part of) a SET_TYPE CONSTRUCTOR node.
   The result is placed in BUFFER (which has length BIT_SIZE),
   with one bit in each char ('\000' or '\001').

   If the constructor is constant, NULL_TREE is returned.
   Otherwise, a TREE_LIST of the non-constant elements is emitted.  */

tree
get_set_constructor_bits (init, buffer, bit_size)
     tree init;
     char *buffer;
     int bit_size;
{
  int i;
  tree vals;
  HOST_WIDE_INT domain_min
    = TREE_INT_CST_LOW (TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (init))));
  tree non_const_bits = NULL_TREE;
  for (i = 0; i < bit_size; i++)
    buffer[i] = 0;

  for (vals = TREE_OPERAND (init, 1); 
       vals != NULL_TREE; vals = TREE_CHAIN (vals))
    {
      if (TREE_CODE (TREE_VALUE (vals)) != INTEGER_CST
	  || (TREE_PURPOSE (vals) != NULL_TREE
	      && TREE_CODE (TREE_PURPOSE (vals)) != INTEGER_CST))
	non_const_bits
	  = tree_cons (TREE_PURPOSE (vals), TREE_VALUE (vals), non_const_bits);
      else if (TREE_PURPOSE (vals) != NULL_TREE)
	{
	  /* Set a range of bits to ones.  */
	  HOST_WIDE_INT lo_index
	    = TREE_INT_CST_LOW (TREE_PURPOSE (vals)) - domain_min;
	  HOST_WIDE_INT hi_index
	    = TREE_INT_CST_LOW (TREE_VALUE (vals)) - domain_min;
	  if (lo_index < 0 || lo_index >= bit_size
	    || hi_index < 0 || hi_index >= bit_size)
	    abort ();
	  for ( ; lo_index <= hi_index; lo_index++)
	    buffer[lo_index] = 1;
	}
      else
	{
	  /* Set a single bit to one.  */
	  HOST_WIDE_INT index
	    = TREE_INT_CST_LOW (TREE_VALUE (vals)) - domain_min;
	  if (index < 0 || index >= bit_size)
	    {
	      error ("invalid initializer for bit string");
	      return NULL_TREE;
	    }
	  buffer[index] = 1;
	}
    }
  return non_const_bits;
}

/* Expand (the constant part of) a SET_TYPE CONSTRUCTOR node.
   The result is placed in BUFFER (which is an array of bytes).
   If the constructor is constant, NULL_TREE is returned.
   Otherwise, a TREE_LIST of the non-constant elements is emitted.  */

tree
get_set_constructor_bytes (init, buffer, wd_size)
     tree init;
     unsigned char *buffer;
     int wd_size;
{
  int i;
  tree vals = TREE_OPERAND (init, 1);
  int set_word_size = BITS_PER_UNIT;
  int bit_size = wd_size * set_word_size;
  int bit_pos = 0;
  unsigned char *bytep = buffer;
  char *bit_buffer = (char *) alloca(bit_size);
  tree non_const_bits = get_set_constructor_bits (init, bit_buffer, bit_size);

  for (i = 0; i < wd_size; i++)
    buffer[i] = 0;

  for (i = 0; i < bit_size; i++)
    {
      if (bit_buffer[i])
	{
	  if (BYTES_BIG_ENDIAN)
	    *bytep |= (1 << (set_word_size - 1 - bit_pos));
	  else
	    *bytep |= 1 << bit_pos;
	}
      bit_pos++;
      if (bit_pos >= set_word_size)
	bit_pos = 0, bytep++;
    }
  return non_const_bits;
}
