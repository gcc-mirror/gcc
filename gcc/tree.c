/* Language-independent node constructors for parse phase of GNU compiler.
   Copyright (C) 1987, 1988, 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


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
#include "flags.h"
#include "tree.h"
#include "function.h"
#include "obstack.h"
#include "gvarargs.h"
#include <stdio.h>

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

extern char *mode_name[];

void gcc_obstack_init ();
static tree stabilize_reference_1 ();

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
  bzero (hash_table, sizeof hash_table);
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
   This is used before starting a nested function.  */

void
save_tree_status (p)
     struct function *p;
{
  p->all_types_permanent = all_types_permanent;
  p->momentary_stack = momentary_stack;
  p->maybepermanent_firstobj = maybepermanent_firstobj;
  p->momentary_firstobj = momentary_firstobj;
  p->function_obstack = function_obstack;
  p->function_maybepermanent_obstack = function_maybepermanent_obstack;
  p->current_obstack = current_obstack;
  p->expression_obstack = expression_obstack;
  p->saveable_obstack = saveable_obstack;
  p->rtl_obstack = rtl_obstack;

  function_obstack = (struct obstack *) xmalloc (sizeof (struct obstack));
  gcc_obstack_init (function_obstack);

  function_maybepermanent_obstack
    = (struct obstack *) xmalloc (sizeof (struct obstack));
  gcc_obstack_init (function_maybepermanent_obstack);

  current_obstack = &permanent_obstack;
  expression_obstack = &permanent_obstack;
  rtl_obstack = saveable_obstack = &permanent_obstack;

  momentary_firstobj = (char *) obstack_finish (&momentary_obstack);
  maybepermanent_firstobj
    = (char *) obstack_finish (function_maybepermanent_obstack);
}

/* Restore all variables describing the current status from the structure *P.
   This is used after a nested function.  */

void
restore_tree_status (p)
     struct function *p;
{
  all_types_permanent = p->all_types_permanent;
  momentary_stack = p->momentary_stack;

  obstack_free (&momentary_obstack, momentary_firstobj);
  obstack_free (function_obstack, 0);
  obstack_free (function_maybepermanent_obstack, 0);
  free (function_obstack);

  momentary_firstobj = p->momentary_firstobj;
  maybepermanent_firstobj = p->maybepermanent_firstobj;
  function_obstack = p->function_obstack;
  function_maybepermanent_obstack = p->function_maybepermanent_obstack;
  current_obstack = p->current_obstack;
  expression_obstack = p->expression_obstack;
  saveable_obstack = p->saveable_obstack;
  rtl_obstack = p->rtl_obstack;
}

/* Start allocating on the temporary (per function) obstack.
   This is done in start_function before parsing the function body,
   and before each initialization at top level, and to go back
   to temporary allocation after doing end_temporary_allocation.  */

void
temporary_allocation ()
{
  /* Note that function_obstack at top level points to temporary_obstack.
     But within a nested function context, it is a separate obstack.  */
  current_obstack = function_obstack;
  expression_obstack = function_obstack;
  rtl_obstack = saveable_obstack = function_maybepermanent_obstack;
  momentary_stack = 0;
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
   This is done in finish_function after fully compiling a function.  */

void
permanent_allocation ()
{
  /* Free up previous temporary obstack data */
  obstack_free (&temporary_obstack, temporary_firstobj);
  obstack_free (&momentary_obstack, momentary_firstobj);
  obstack_free (&maybepermanent_obstack, maybepermanent_firstobj);
  obstack_free (&temp_decl_obstack, temp_decl_firstobj);

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
  temporary_firstobj
    = (char *) obstack_alloc (&temporary_obstack, 0);
  momentary_firstobj
    = (char *) obstack_alloc (&momentary_obstack, 0);
  maybepermanent_firstobj
    = (char *) obstack_alloc (function_maybepermanent_obstack, 0);
}

/* Start allocating new rtl in current_obstack.
   Use resume_temporary_allocation
   to go back to allocating rtl in saveable_obstack.  */

void
rtl_in_current_obstack ()
{
  rtl_obstack = current_obstack;
}

/* Temporarily allocate rtl from saveable_obstack.  Return 1 if we were
   previously allocating it from current_obstack.  */

int
rtl_in_saveable_obstack ()
{
  if (rtl_obstack == current_obstack)
    {
      rtl_obstack = saveable_obstack;
      return 1;
    }
  else
    return 0;
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

/* Print out which obstack an object is in.  */

void
debug_obstack (object)
     char *object;
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

  /* Check to see if the object is in the free area of the obstack. */
  if (obstack != NULL)
    {
      if (object >= obstack->next_free
	  && object < obstack->chunk_limit)
	fprintf (stderr, "object in free portion of obstack %s.\n",
		 obstack_name);
      else
	fprintf (stderr, "object allocated from %s.\n", obstack_name);
    }
  else
    fprintf (stderr, "object not allocated from any obstack.\n");
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
  obstack_free (&momentary_obstack, tem);
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
  bcopy (standard_tree_code_type, tree_code_type,
	 sizeof (standard_tree_code_type));
  bcopy (standard_tree_code_length, tree_code_length,
	 sizeof (standard_tree_code_length));
  bcopy (standard_tree_code_name, tree_code_name,
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
      /* PARM_DECLs always go on saveable_obstack, not permanent,
	 even though we may make them before the function turns
	 on temporary allocation.  */
      else if (code == PARM_DECL)
	obstack = function_maybepermanent_obstack;
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
      /* We can't use tree_code_length for this, since the number of words
	 is machine-dependent due to varying alignment of `double'.  */
      if (code == REAL_CST)
	{
	  length = sizeof (struct tree_real_cst);
	  break;
	}

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
      {
	static unsigned next_type_uid = 1;

	TYPE_UID (t) = next_type_uid++;
      }
      TYPE_ALIGN (t) = 1;
      TYPE_MAIN_VARIANT (t) = t;
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
      /* We can't use tree_code_length for this, since the number of words
	 is machine-dependent due to varying alignment of `double'.  */
      if (code == REAL_CST)
	{
	  length = sizeof (struct tree_real_cst);
	  break;
	}

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
  hi = hash_len * 613 + (unsigned)text[0];
  for (i = 1; i < hash_len; i += 2)
    hi = ((hi * 613) + (unsigned)(text[i]));

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

  /* Check for valid float value for this type on this target machine;
     if not, can print error message and store a valid value in D.  */
#ifdef CHECK_FLOAT_VALUE
  CHECK_FLOAT_VALUE (TYPE_MODE (type), d);
#endif

  v = make_node (REAL_CST);
  TREE_TYPE (v) = type;
  TREE_REAL_CST (v) = d;
  return v;
}

/* Return a new REAL_CST node whose type is TYPE
   and whose value is the integer value of the INTEGER_CST node I.  */

#if !defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)

REAL_VALUE_TYPE
real_value_from_int_cst (i)
     tree i;
{
  REAL_VALUE_TYPE d;
#ifdef REAL_ARITHMETIC
  REAL_VALUE_FROM_INT (d, TREE_INT_CST_LOW (i), TREE_INT_CST_HIGH (i));
#else /* not REAL_ARITHMETIC */
  if (TREE_INT_CST_HIGH (i) < 0 && ! TREE_UNSIGNED (TREE_TYPE (i)))
    {
      d = (double) (~ TREE_INT_CST_HIGH (i));
      d *= ((double) ((HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2))
	    * (double) ((HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2)));
      d += (double) (unsigned HOST_WIDE_INT) (~ TREE_INT_CST_LOW (i));
      d = (- d - 1.0);
    }
  else
    {
      d = (double) (unsigned HOST_WIDE_INT) TREE_INT_CST_HIGH (i);
      d *= ((double) ((HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2))
	    * (double) ((HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2)));
      d += (double) (unsigned HOST_WIDE_INT) TREE_INT_CST_LOW (i);
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
  REAL_VALUE_TYPE d;

  v = make_node (REAL_CST);
  TREE_TYPE (v) = type;

  d = REAL_VALUE_TRUNCATE (TYPE_MODE (type), real_value_from_int_cst (i));
  /* Check for valid float value for this type on this target machine;
     if not, can print error message and store a valid value in D.  */
#ifdef CHECK_FLOAT_VALUE
  CHECK_FLOAT_VALUE (TYPE_MODE (type), d);
#endif

  TREE_REAL_CST (v) = d;
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
  register tree s = make_node (STRING_CST);
  TREE_STRING_LENGTH (s) = len;
  TREE_STRING_POINTER (s) = obstack_copy0 (saveable_obstack, str, len);
  return s;
}

/* Return a newly constructed COMPLEX_CST node whose value is
   specified by the real and imaginary parts REAL and IMAG.
   Both REAL and IMAG should be constant nodes.
   The TREE_TYPE is not initialized.  */

tree
build_complex (real, imag)
     tree real, imag;
{
  register tree t = make_node (COMPLEX_CST);
  TREE_REALPART (t) = real;
  TREE_IMAGPART (t) = imag;
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

/* Return 1 if EXPR is the integer constant zero.  */

int
integer_zerop (expr)
     tree expr;
{
  STRIP_NOPS (expr);

  return (TREE_CODE (expr) == INTEGER_CST
	  && TREE_INT_CST_LOW (expr) == 0
	  && TREE_INT_CST_HIGH (expr) == 0);
}

/* Return 1 if EXPR is the integer constant one.  */

int
integer_onep (expr)
     tree expr;
{
  STRIP_NOPS (expr);

  return (TREE_CODE (expr) == INTEGER_CST
	  && TREE_INT_CST_LOW (expr) == 1
	  && TREE_INT_CST_HIGH (expr) == 0);
}

/* Return 1 if EXPR is an integer containing all 1's
   in as much precision as it contains.  */

int
integer_all_onesp (expr)
     tree expr;
{
  register int prec;
  register int uns;

  STRIP_NOPS (expr);

  if (TREE_CODE (expr) != INTEGER_CST)
    return 0;

  uns = TREE_UNSIGNED (TREE_TYPE (expr));
  if (!uns)
    return TREE_INT_CST_LOW (expr) == -1 && TREE_INT_CST_HIGH (expr) == -1;

  prec = TYPE_PRECISION (TREE_TYPE (expr));
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
  HOST_WIDE_INT high, low;

  STRIP_NOPS (expr);

  if (TREE_CODE (expr) != INTEGER_CST)
    return 0;

  high = TREE_INT_CST_HIGH (expr);
  low = TREE_INT_CST_LOW (expr);

  if (high == 0 && low == 0)
    return 0;

  return ((high == 0 && (low & (low - 1)) == 0)
	  || (low == 0 && (high & (high - 1)) == 0));
}

/* Return 1 if EXPR is the real constant zero.  */

int
real_zerop (expr)
     tree expr;
{
  STRIP_NOPS (expr);

  return (TREE_CODE (expr) == REAL_CST
	  && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), dconst0));
}

/* Return 1 if EXPR is the real constant one.  */

int
real_onep (expr)
     tree expr;
{
  STRIP_NOPS (expr);

  return (TREE_CODE (expr) == REAL_CST
	  && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), dconst1));
}

/* Return 1 if EXPR is the real constant two.  */

int
real_twop (expr)
     tree expr;
{
  STRIP_NOPS (expr);

  return (TREE_CODE (expr) == REAL_CST
	  && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), dconst2));
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
   Return 0 if ELEM is not it LIST.  */

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
   Return 0 if ELEM is not it LIST.  */

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
   Return 0 if ELEM is not it LIST.  */

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
  tree t;

  if (op1)
    {
      for (t = op1; TREE_CHAIN (t); t = TREE_CHAIN (t))
	if (t == op2) abort ();	/* Circularity being created */
      if (t == op2) abort ();	/* Circularity being created */
      TREE_CHAIN (t) = op2;
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
  if (type == error_mark_node)
    return integer_zero_node;
  type = TYPE_MAIN_VARIANT (type);
  if (TYPE_SIZE (type) == 0)
    {
      incomplete_type_error (NULL_TREE, type);
      return integer_zero_node;
    }
  return size_binop (CEIL_DIV_EXPR, TYPE_SIZE (type),
		     size_int (BITS_PER_UNIT));
}

/* Return the size of TYPE (in bytes) as an integer,
   or return -1 if the size can vary.  */

int
int_size_in_bytes (type)
     tree type;
{
  int size;
  if (type == error_mark_node)
    return 0;
  type = TYPE_MAIN_VARIANT (type);
  if (TYPE_SIZE (type) == 0)
    return -1;
  if (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
    return -1;
  size = TREE_INT_CST_LOW (TYPE_SIZE (type));
  return (size + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
}

/* Return, as an INTEGER_CST node, the number of elements for
   TYPE (which is an ARRAY_TYPE) minus one. 
   This counts only elements of the top array.  */

tree
array_type_nelts (type)
     tree type;
{
  tree index_type = TYPE_DOMAIN (type);
  return (tree_int_cst_equal (TYPE_MIN_VALUE (index_type), integer_zero_node)
	  ? TYPE_MAX_VALUE (index_type)
	  : fold (build (MINUS_EXPR, integer_type_node,
			 TYPE_MAX_VALUE (index_type),
			 TYPE_MIN_VALUE (index_type))));
}

/* Return nonzero if arg is static -- a reference to an object in
   static storage.  This is not the same as the C meaning of `static'.  */

int
staticp (arg)
     tree arg;
{
  switch (TREE_CODE (arg))
    {
    case VAR_DECL:
    case FUNCTION_DECL:
    case CONSTRUCTOR:
      return TREE_STATIC (arg) || DECL_EXTERNAL (arg);

    case STRING_CST:
      return 1;

    case COMPONENT_REF:
    case BIT_FIELD_REF:
      return staticp (TREE_OPERAND (arg, 0));

    case INDIRECT_REF:
      return TREE_CONSTANT (TREE_OPERAND (arg, 0));

    case ARRAY_REF:
      if (TREE_CODE (TYPE_SIZE (TREE_TYPE (arg))) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg, 1)) == INTEGER_CST)
	return staticp (TREE_OPERAND (arg, 0));
    }

  return 0;
}

/* This should be applied to any node which may be used in more than one place,
   but must be evaluated only once.  Normally, the code generator would
   reevaluate the node each time; this forces it to compute it once and save
   the result.  This is done by encapsulating the node in a SAVE_EXPR.  */

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
     literal node. */

  if (TREE_CONSTANT (t) || (TREE_READONLY (t) && ! TREE_SIDE_EFFECTS (t))
      || TREE_CODE (t) == SAVE_EXPR)
    return t;

  t = build (SAVE_EXPR, TREE_TYPE (expr), t, current_function_decl, NULL_TREE);

  /* This expression might be placed ahead of a jump to ensure that the
     value was computed on both sides of the jump.  So make sure it isn't
     eliminated as dead.  */
  TREE_SIDE_EFFECTS (t) = 1;
  return t;
}

/* Stabilize a reference so that we can use it any number of times
   without causing its operands to be evaluated more than once.
   Returns the stabilized reference.

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

static tree
stabilize_reference_1 (e)
     tree e;
{
  register tree result;
  register int length;
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
      /* Recursively stabilize each operand.  */
      result = build_nt (code, stabilize_reference_1 (TREE_OPERAND (e, 0)),
			 stabilize_reference_1 (TREE_OPERAND (e, 1)));
      break;

    case '1':
      /* Recursively stabilize each operand.  */
      result = build_nt (code, stabilize_reference_1 (TREE_OPERAND (e, 0)));
      break;
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
build (va_alist)
     va_dcl
{
  va_list p;
  enum tree_code code;
  register tree t;
  register int length;
  register int i;

  va_start (p);

  code = va_arg (p, enum tree_code);
  t = make_node (code);
  length = tree_code_length[(int) code];
  TREE_TYPE (t) = va_arg (p, tree);

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
  register struct obstack *obstack = current_obstack;
  register int i, length;
  register tree_node_kind kind;
  register tree t;

#ifdef GATHER_STATISTICS
  if (TREE_CODE_CLASS (code) == 'r')
    kind = r_kind;
  else
    kind = e_kind;
#endif

  obstack = expression_obstack;
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
build_nt (va_alist)
     va_dcl
{
  va_list p;
  register enum tree_code code;
  register tree t;
  register int length;
  register int i;

  va_start (p);

  code = va_arg (p, enum tree_code);
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
build_parse_node (va_alist)
     va_dcl
{
  register struct obstack *ambient_obstack = expression_obstack;
  va_list p;
  register enum tree_code code;
  register tree t;
  register int length;
  register int i;

  expression_obstack = &temp_decl_obstack;

  va_start (p);

  code = va_arg (p, enum tree_code);
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
  register tree t, m = TYPE_MAIN_VARIANT (type);
  register struct obstack *ambient_obstack = current_obstack;

  /* Treat any nonzero argument as 1.  */
  constp = !!constp;
  volatilep = !!volatilep;

  /* If not generating auxiliary info, search the chain of variants to see
     if there is already one there just like the one we need to have.  If so,
     use that existing one.

     We don't do this in the case where we are generating aux info because
     in that case we want each typedef names to get it's own distinct type
     node, even if the type of this new typedef is the same as some other
     (existing) type.  */

  if (!flag_gen_aux_info)
    for (t = m; t; t = TYPE_NEXT_VARIANT (t))
      if (constp == TYPE_READONLY (t) && volatilep == TYPE_VOLATILE (t))
        return t;

  /* We need a new one.  */
  current_obstack
    = TREE_PERMANENT (type) ? &permanent_obstack : saveable_obstack;

  t = copy_node (type);
  TYPE_READONLY (t) = constp;
  TYPE_VOLATILE (t) = volatilep;
  TYPE_POINTER_TO (t) = 0;
  TYPE_REFERENCE_TO (t) = 0;

  /* Add this type to the chain of variants of TYPE.  */
  TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (m);
  TYPE_NEXT_VARIANT (m) = t;

  current_obstack = ambient_obstack;
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

  current_obstack
    = TREE_PERMANENT (type) ? &permanent_obstack : saveable_obstack;

  t = copy_node (type);
  TYPE_POINTER_TO (t) = 0;
  TYPE_REFERENCE_TO (t) = 0;

  /* Add this type to the chain of variants of TYPE.  */
  TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (m);
  TYPE_NEXT_VARIANT (m) = t;

  current_obstack = ambient_obstack;
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

/* Here is how primitive or already-canonicalized types' hash
   codes are made.  */
#define TYPE_HASH(TYPE) ((HOST_WIDE_INT) (TYPE) & 0777777)

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
	&& (TYPE_MAX_VALUE (h->type) == TYPE_MAX_VALUE (type)
	    || tree_int_cst_equal (TYPE_MAX_VALUE (h->type),
				   TYPE_MAX_VALUE (type)))
	&& (TYPE_MIN_VALUE (h->type) == TYPE_MIN_VALUE (type)
	    || tree_int_cst_equal (TYPE_MIN_VALUE (h->type),
				   TYPE_MIN_VALUE (type)))
	&& (TYPE_DOMAIN (h->type) == TYPE_DOMAIN (type)
	    || (TYPE_DOMAIN (h->type)
		&& TREE_CODE (TYPE_DOMAIN (h->type)) == TREE_LIST
		&& TYPE_DOMAIN (type)
		&& TREE_CODE (TYPE_DOMAIN (type)) == TREE_LIST
		&& type_list_equal (TYPE_DOMAIN (h->type), TYPE_DOMAIN (type)))))
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
      struct obstack *o
	= TREE_PERMANENT (type) ? &permanent_obstack : saveable_obstack;
      obstack_free (o, type);
#ifdef GATHER_STATISTICS
      tree_node_counts[(int)t_kind]--;
      tree_node_sizes[(int)t_kind] -= sizeof (struct tree_type);
#endif
      return t1;
    }

  /* If this is a new type, record it for later reuse.  */
  if (current_obstack == &permanent_obstack)
    type_hash_add (hashcode, type);

  return type;
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
    {
      if (TREE_VALUE (t1) != TREE_VALUE (t2))
	return 0;
      if (TREE_PURPOSE (t1) != TREE_PURPOSE (t2))
	{
	  int cmp = simple_cst_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2));
	  if (cmp < 0)
	    abort ();
	  if (cmp == 0)
	    return 0;
	}
    }

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

/* Compare two constructor-element-type constants.  */
int
simple_cst_list_equal (l1, l2)
     tree l1, l2;
{
  while (l1 != NULL_TREE && l2 != NULL_TREE)
    {
      int cmp = simple_cst_equal (TREE_VALUE (l1), TREE_VALUE (l2));
      if (cmp < 0)
	abort ();
      if (cmp == 0)
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
      return REAL_VALUES_EQUAL (TREE_REAL_CST (t1), TREE_REAL_CST (t2));

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

    case BIT_FIELD_REF:
      return (simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0))
	      && simple_cst_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1))
	      && simple_cst_equal (TREE_OPERAND (t1, 2), TREE_OPERAND (t2, 2)));

    case VAR_DECL:
    case PARM_DECL:
    case CONST_DECL:
    case FUNCTION_DECL:
      return 0;

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      cmp = simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      if (cmp <= 0)
	return cmp;
      return simple_cst_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));

    case NEGATE_EXPR:
    case ADDR_EXPR:
    case REFERENCE_EXPR:
    case INDIRECT_REF:
      return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

    default:
#if 0
      return lang_simple_cst_equal (t1, t2);
#else
      return -1;
#endif
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
  register struct obstack *ambient_obstack = current_obstack;
  register struct obstack *ambient_saveable_obstack = saveable_obstack;

  /* First, if we already have a type for pointers to TO_TYPE, use it.  */

  if (t)
    return t;

  /* We need a new one.  If TO_TYPE is permanent, make this permanent too.  */
  if (TREE_PERMANENT (to_type))
    {
      current_obstack = &permanent_obstack;
      saveable_obstack = &permanent_obstack;
    }

  t = make_node (POINTER_TYPE);
  TREE_TYPE (t) = to_type;

  /* Record this type as the pointer to TO_TYPE.  */
  TYPE_POINTER_TO (to_type) = t;

  /* Lay out the type.  This function has many callers that are concerned
     with expression-construction, and this simplifies them all.
     Also, it guarantees the TYPE_SIZE is permanent if the type is.  */
  layout_type (t);

  current_obstack = ambient_obstack;
  saveable_obstack = ambient_saveable_obstack;
  return t;
}

/* Create a type of integers to be the TYPE_DOMAIN of an ARRAY_TYPE.
   MAXVAL should be the maximum value in the domain
   (one less than the length of the array).  */

tree
build_index_type (maxval)
     tree maxval;
{
  register tree itype = make_node (INTEGER_TYPE);
  TYPE_PRECISION (itype) = TYPE_PRECISION (sizetype);
  TYPE_MIN_VALUE (itype) = build_int_2 (0, 0);
  TREE_TYPE (TYPE_MIN_VALUE (itype)) = sizetype;
  TYPE_MAX_VALUE (itype) = convert (sizetype, maxval);
  TYPE_MODE (itype) = TYPE_MODE (sizetype);
  TYPE_SIZE (itype) = TYPE_SIZE (sizetype);
  TYPE_ALIGN (itype) = TYPE_ALIGN (sizetype);
  if (TREE_CODE (maxval) == INTEGER_CST)
    {
      int maxint = (int) TREE_INT_CST_LOW (maxval);
      return type_hash_canon (maxint < 0 ? ~maxint : maxint, itype);
    }
  else
    return itype;
}

/* Just like build_index_type, but takes lowval and highval instead
   of just highval (maxval). */

tree
build_index_2_type (lowval,highval)
     tree lowval, highval;
{
  register tree itype = make_node (INTEGER_TYPE);
  TYPE_PRECISION (itype) = TYPE_PRECISION (sizetype);
  TYPE_MIN_VALUE (itype) = convert (sizetype, lowval);
  TYPE_MAX_VALUE (itype) = convert (sizetype, highval);
  TYPE_MODE (itype) = TYPE_MODE (sizetype);
  TYPE_SIZE (itype) = TYPE_SIZE (sizetype);
  TYPE_ALIGN (itype) = TYPE_ALIGN (sizetype);
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
	  || ! simple_cst_equal (TYPE_SIZE (itype1), TYPE_SIZE (itype2))
	  || TYPE_ALIGN (itype1) != TYPE_ALIGN (itype2))
	return 0;
      if (simple_cst_equal (TYPE_MIN_VALUE (itype1), TYPE_MIN_VALUE (itype2))
	  && simple_cst_equal (TYPE_MAX_VALUE (itype1), TYPE_MAX_VALUE (itype2)))
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
    return t;

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

  if (TREE_CODE (value_type) == FUNCTION_TYPE
      || TREE_CODE (value_type) == ARRAY_TYPE)
    {
      error ("function return type cannot be function or array");
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
  register struct obstack *ambient_obstack = current_obstack;
  register struct obstack *ambient_saveable_obstack = saveable_obstack;

  /* First, if we already have a type for pointers to TO_TYPE, use it.  */

  if (t)
    return t;

  /* We need a new one.  If TO_TYPE is permanent, make this permanent too.  */
  if (TREE_PERMANENT (to_type))
    {
      current_obstack = &permanent_obstack;
      saveable_obstack = &permanent_obstack;
    }

  t = make_node (REFERENCE_TYPE);
  TREE_TYPE (t) = to_type;

  /* Record this type as the pointer to TO_TYPE.  */
  TYPE_REFERENCE_TO (to_type) = t;

  layout_type (t);

  current_obstack = ambient_obstack;
  saveable_obstack = ambient_saveable_obstack;
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

/* Construct, lay out and return the type of methods belonging to class
   BASETYPE and whose arguments and values are described by TYPE.
   If that type exists already, reuse it.
   TYPE must be a FUNCTION_TYPE node.  */

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
      && TREE_CODE (type) != REAL_TYPE)
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
      /* A change in nominal type can always be stripped.  */

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
    return (!INT_CST_LT_UNSIGNED (TYPE_MAX_VALUE (type), c)
	    && !INT_CST_LT_UNSIGNED (c, TYPE_MIN_VALUE (type))
	    && (TREE_INT_CST_HIGH (c) >= 0 || TREE_UNSIGNED (TREE_TYPE (c))));
  else
    return (!INT_CST_LT (TYPE_MAX_VALUE (type), c)
	    && !INT_CST_LT (c, TYPE_MIN_VALUE (type))
	    && (TREE_INT_CST_HIGH (c) >= 0 || !TREE_UNSIGNED (TREE_TYPE (c))));
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
	  || TREE_CODE (context) == UNION_TYPE)
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
   a RECORD_TYPE or UNION_TYPE, or zero if none.
   TYPE_DECLs and FUNCTION_DECLs are transparent to this function.  */

tree
decl_type_context (decl)
     tree decl;
{
  tree context = DECL_CONTEXT (decl);

  while (context)
    {
      if (TREE_CODE (context) == RECORD_TYPE
	  || TREE_CODE (context) == UNION_TYPE)
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

void
print_obstack_statistics (str, o)
     char *str;
     struct obstack *o;
{
  struct _obstack_chunk *chunk = o->chunk;
  int n_chunks = 0;
  int n_alloc = 0;

  while (chunk)
    {
      n_chunks += 1;
      n_alloc += chunk->limit - &chunk->contents[0];
      chunk = chunk->prev;
    }
  fprintf (stderr, "obstack %s: %d bytes, %d chunks\n",
	   str, n_alloc, n_chunks);
}
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
  print_lang_statistics ();
}
