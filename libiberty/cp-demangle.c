/* Demangler for IA64 / g++ standard C++ ABI.
   Copyright (C) 2000 CodeSourcery LLC.
   Written by Alex Samuel <samuel@codesourcery.com>. 

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. 
*/

/* This file implements demangling of C++ names mangled according to
   the IA64 / g++ standard C++ ABI.  Use the cp_demangle function to
   demangle a mangled name, or compile with the preprocessor macro
   STANDALONE_DEMANGLER defined to create a demangling filter
   executable.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "ansidecl.h"
#include "libiberty.h"
#include "dyn-string.h"
#include "demangle.h"

/* If CP_DEMANGLE_DEBUG is defined, a trace of the grammar evaluation,
   and other debugging output, will be generated. */
#ifdef CP_DEMANGLE_DEBUG
#define DEMANGLE_TRACE(PRODUCTION, DM)                                  \
  fprintf (stderr, " -> %-24s at position %3d\n",                       \
           (PRODUCTION), current_position (DM));
#else
#define DEMANGLE_TRACE(PRODUCTION, DM)
#endif

/* Don't include <ctype.h>, to prevent additional unresolved symbols
   from being dragged into the C++ runtime library.  */
#define IS_DIGIT(CHAR) ((CHAR) >= '0' && (CHAR) <= '9')
#define IS_ALPHA(CHAR)                                                  \
  (((CHAR) >= 'a' && (CHAR) <= 'z')                                     \
   || ((CHAR) >= 'A' && (CHAR) <= 'Z'))

/* If flag_verbose is zero, some simplifications will be made to the
   output to make it easier to read and supress details that are
   generally not of interest to the average C++ programmer.
   Otherwise, the demangled representation will attempt to convey as
   much information as the mangled form.  */
static int flag_verbose;

/* If flag_strict is non-zero, demangle strictly according to the
   specification -- don't demangle special g++ manglings.  */
static int flag_strict;

/* String_list_t is an extended form of dyn_string_t which provides a link
   field.  A string_list_t may safely be cast to and used as a
   dyn_string_t.  */

struct string_list_def
{
  struct dyn_string string;
  struct string_list_def *next;
};

typedef struct string_list_def *string_list_t;

/* Data structure representing a potential substitution.  */

struct substitution_def
{
  /* The demangled text of the substitution.  */
  dyn_string_t text;

  /* The template parameter that this represents, indexed from zero.
     If this is not a template paramter number, the value is
     NOT_TEMPLATE_PARM.  */
  int template_parm_number;

  /* Whether this substitution represents a template item.  */
  int template_p : 1;
};

#define NOT_TEMPLATE_PARM (-1)

/* Data structure representing a template argument list.  */

struct template_arg_list_def
{
  /* The next (lower) template argument list in the stack of currently
     active template arguments.  */
  struct template_arg_list_def *next;

  /* The first element in the list of template arguments in
     left-to-right order.  */
  string_list_t first_argument;

  /* The last element in the arguments lists.  */
  string_list_t last_argument;
};

typedef struct template_arg_list_def *template_arg_list_t;

/* Data structure to maintain the state of the current demangling.  */

struct demangling_def
{
  /* The full mangled name being mangled.  */
  const char *name;

  /* Pointer into name at the current position.  */
  const char *next;

  /* Stack for strings containing demangled result generated so far.
     Text is emitted to the topmost (first) string.  */
  string_list_t result;

  /* The number of presently available substitutions.  */
  int num_substitutions;

  /* The allocated size of the substitutions array.  */
  int substitutions_allocated;

  /* An array of available substitutions.  The number of elements in
     the array is given by num_substitions, and the allocated array
     size in substitutions_size.  

     The most recent substition is at the end, so

       - `S_'  corresponds to substititutions[num_substitutions - 1] 
       - `S0_' corresponds to substititutions[num_substitutions - 2]

     etc. */
  struct substitution_def *substitutions;

  /* The stack of template argument lists.  */
  template_arg_list_t template_arg_lists;

  /* The most recently demangled source-name.  */
  dyn_string_t last_source_name;
};

typedef struct demangling_def *demangling_t;

/* This type is the standard return code from most functions.  Values
   other than STATUS_OK contain descriptive messages.  */
typedef const char *status_t;

/* Special values that can be used as a status_t.  */
#define STATUS_OK                       NULL
#define STATUS_ERROR                    "Error."
#define STATUS_UNIMPLEMENTED            "Unimplemented."
#define STATUS_INTERNAL_ERROR           "Internal error."

/* This status code indicates a failure in malloc or realloc.  */
static const char* const status_allocation_failed = "Allocation failed.";
#define STATUS_ALLOCATION_FAILED        status_allocation_failed

/* Non-zero if STATUS indicates that no error has occurred.  */
#define STATUS_NO_ERROR(STATUS)         ((STATUS) == STATUS_OK)

/* Evaluate EXPR, which must produce a status_t.  If the status code
   indicates an error, return from the current function with that
   status code.  */
#define RETURN_IF_ERROR(EXPR)                                           \
  do                                                                    \
    {                                                                   \
      status_t s = EXPR;                                                \
      if (!STATUS_NO_ERROR (s))                                         \
	return s;                                                       \
    }                                                                   \
  while (0)

static status_t int_to_dyn_string 
  PARAMS ((int, dyn_string_t));
static string_list_t string_list_new
  PARAMS ((int));
static void string_list_delete
  PARAMS ((string_list_t));
static status_t result_close_template_list 
  PARAMS ((demangling_t));
static status_t result_push
  PARAMS ((demangling_t));
static string_list_t result_pop
  PARAMS ((demangling_t));
static int substitution_start
  PARAMS ((demangling_t));
static status_t substitution_add
  PARAMS ((demangling_t, int, int, int));
static dyn_string_t substitution_get
  PARAMS ((demangling_t, int, int *));
#ifdef CP_DEMANGLE_DEBUG
static void substitutions_print 
  PARAMS ((demangling_t, FILE *));
#endif
static template_arg_list_t template_arg_list_new
  PARAMS ((void));
static void template_arg_list_delete
  PARAMS ((template_arg_list_t));
static void template_arg_list_add_arg 
  PARAMS ((template_arg_list_t, string_list_t));
static string_list_t template_arg_list_get_arg
  PARAMS ((template_arg_list_t, int));
static void push_template_arg_list
  PARAMS ((demangling_t, template_arg_list_t));
static void pop_to_template_arg_list
  PARAMS ((demangling_t, template_arg_list_t));
#ifdef CP_DEMANGLE_DEBUG
static void template_arg_list_print
  PARAMS ((template_arg_list_t, FILE *));
#endif
static template_arg_list_t current_template_arg_list
  PARAMS ((demangling_t));
static demangling_t demangling_new
  PARAMS ((const char *));
static void demangling_delete 
  PARAMS ((demangling_t));

/* The last character of DS.  Warning: DS is evaluated twice.  */
#define dyn_string_last_char(DS)                                        \
  (dyn_string_buf (DS)[dyn_string_length (DS) - 1])

/* Append a space character (` ') to DS if it does not already end
   with one.  Evaluates to 1 on success, or 0 on allocation failure.  */
#define dyn_string_append_space(DS)                                     \
      ((dyn_string_length (DS) > 0                                      \
        && dyn_string_last_char (DS) != ' ')                            \
       ? dyn_string_append_char ((DS), ' ')                             \
       : 1)

/* Returns the index of the current position in the mangled name.  */
#define current_position(DM)    ((DM)->next - (DM)->name)

/* Returns the character at the current position of the mangled name.  */
#define peek_char(DM)           (*((DM)->next))

/* Returns the character one past the current position of the mangled
   name.  */
#define peek_char_next(DM)                                              \
  (peek_char (DM) == '\0' ? '\0' : (*((DM)->next + 1)))

/* Returns the character at the current position, and advances the
   current position to the next character.  */
#define next_char(DM)           (*((DM)->next)++)

/* Returns non-zero if the current position is the end of the mangled
   name, i.e. one past the last character.  */
#define end_of_name_p(DM)       (peek_char (DM) == '\0')

/* Advances the current position by one character.  */
#define advance_char(DM)        (++(DM)->next)

/* Returns the string containing the current demangled result.  */
#define result_string(DM)       (&(DM)->result->string)

/* Appends a dyn_string_t to the demangled result.  */
#define result_append_string(DM, STRING)                                \
  (dyn_string_append (&(DM)->result->string, (STRING))                  \
   ? STATUS_OK : STATUS_ALLOCATION_FAILED)

/* Appends NUL-terminated string CSTR to the demangled result.  */
#define result_append(DM, CSTR)                                         \
  (dyn_string_append_cstr (&(DM)->result->string, (CSTR))               \
   ? STATUS_OK : STATUS_ALLOCATION_FAILED)

/* Appends character CHAR to the demangled result.  */
#define result_append_char(DM, CHAR)                                    \
  (dyn_string_append_char (&(DM)->result->string, (CHAR))               \
   ? STATUS_OK : STATUS_ALLOCATION_FAILED)

/* The length of the current demangled result.  */
#define result_length(DM)                                               \
  dyn_string_length (&(DM)->result->string)

/* Appends a space to the demangled result if the last character is
   not a space.  */
#define result_append_space(DM)                                         \
  (dyn_string_append_space (&(DM)->result->string)                      \
   ? STATUS_OK : STATUS_ALLOCATION_FAILED)

/* Appends a base 10 representation of VALUE to DS.  STATUS_OK on
   success.  On failure, deletes DS and returns an error code.  */

static status_t
int_to_dyn_string (value, ds)
     int value;
     dyn_string_t ds;
{
  int i;
  int mask = 1;

  /* Handle zero up front.  */
  if (value == 0)
    {
      if (!dyn_string_append_char (ds, '0'))
	return STATUS_ALLOCATION_FAILED;
      return STATUS_OK;
    }

  /* For negative numbers, emit a minus sign.  */
  if (value < 0)
    {
      if (!dyn_string_append_char (ds, '-'))
	return STATUS_ALLOCATION_FAILED;
      value = -value;
    }
  
  /* Find the power of 10 of the first digit.  */
  i = value;
  while (i > 9)
    {
      mask *= 10;
      i /= 10;
    }

  /* Write the digits.  */
  while (mask > 0)
    {
      int digit = value / mask;

      if (!dyn_string_append_char (ds, '0' + digit))
	return STATUS_ALLOCATION_FAILED;

      value -= digit * mask;
      mask /= 10;
    }

  return STATUS_OK;
}

/* Creates a new string list node.  The contents of the string are
   empty, but the initial buffer allocation is LENGTH.  The string
   list node should be deleted with string_list_delete.  Returns NULL
   if allocation fails.  */

static string_list_t 
string_list_new (length)
     int length;
{
  string_list_t s = (string_list_t) malloc (sizeof (struct string_list_def));
  if (s == NULL)
    return NULL;
  if (!dyn_string_init ((dyn_string_t) s, length))
    return NULL;
  return s;
}  

/* Deletes the entire string list starting at NODE.  */

static void
string_list_delete (node)
     string_list_t node;
{
  while (node != NULL)
    {
      string_list_t next = node->next;
      free (node);
      node = next;
    }
}

/* Appends a greater-than character to the demangled result.  If the
   last character is a greater-than character, a space is inserted
   first, so that the two greater-than characters don't look like a
   right shift token.  */

static status_t
result_close_template_list (dm)
     demangling_t dm;
{
  dyn_string_t s = &dm->result->string;

  /* Add a space if the last character is already a closing angle
     bracket, so that a nested template arg list doesn't look like
     it's closed with a right-shift operator.  */
  if (dyn_string_last_char (s) == '>')
    {
      if (!dyn_string_append_char (s, ' '))
	return STATUS_ALLOCATION_FAILED;
    }

  /* Add closing angle brackets.  */
  if (!dyn_string_append_char (s, '>'))
    return STATUS_ALLOCATION_FAILED;

  return STATUS_OK;
}

/* Allocates and pushes a new string onto the demangled results stack
   for DM.  Subsequent demangling with DM will emit to the new string.
   Returns STATUS_OK on success, STATUS_ALLOCATION_FAILED on
   allocation failure.  */

static status_t
result_push (dm)
     demangling_t dm;
{
  string_list_t new_string = string_list_new (0);
  if (new_string == NULL)
    /* Allocation failed.  */
    return STATUS_ALLOCATION_FAILED;

  /* Link the new string to the front of the list of result strings.  */
  new_string->next = (string_list_t) dm->result;
  dm->result = new_string;
  return STATUS_OK;
}

/* Removes and returns the topmost element on the demangled results
   stack for DM.  The caller assumes ownership for the returned
   string.  */

static string_list_t
result_pop (dm)
     demangling_t dm;
{
  string_list_t top = dm->result;
  dm->result = top->next;
  return top;
}

/* Returns the start position of a fragment of the demangled result
   that will be a substitution candidate.  Should be called at the
   start of productions that can add substitutions.  */

static int
substitution_start (dm)
     demangling_t dm;
{
  return result_length (dm);
}

/* Adds the suffix of the current demangled result of DM starting at
   START_POSITION as a potential substitution.  If TEMPLATE_P is
   non-zero, this potential substitution is a template-id.  

   If TEMPLATE_PARM_NUMBER is not NOT_TEMPLATE_PARM, the substitution
   is for that particular <template-param>, and is distinct from other
   otherwise-identical types and other <template-param>s with
   different indices.  */

static status_t
substitution_add (dm, start_position, template_p, template_parm_number)
     demangling_t dm;
     int start_position;
     int template_p;
     int template_parm_number;
{
  dyn_string_t result = result_string (dm);
  dyn_string_t substitution = dyn_string_new (0);
  int i;

  if (substitution == NULL)
    return STATUS_ALLOCATION_FAILED;

  /* Extract the substring of the current demangling result that
     represents the subsitution candidate.  */
  if (!dyn_string_substring (substitution, 
			     result, start_position, result_length (dm)))
    {
      dyn_string_delete (substitution);
      return STATUS_ALLOCATION_FAILED;
    }

  /* Check whether SUBSTITUTION already occurs.  */
  for (i = 0; i < dm->num_substitutions; ++i)
    if (dyn_string_eq (dm->substitutions[i].text, substitution)
	&& dm->substitutions[i].template_parm_number == template_parm_number)
      /* Found SUBSTITUTION already present.  */
      {
	/* Callers expect this function to take ownership of
	   SUBSTITUTION, so delete it.  */
	dyn_string_delete (substitution);
	return STATUS_OK;
      }

  /* If there's no room for the new entry, grow the array.  */
  if (dm->substitutions_allocated == dm->num_substitutions)
    {
      size_t new_array_size;
      dm->substitutions_allocated *= 2;
      new_array_size = 
	sizeof (struct substitution_def) * dm->substitutions_allocated;

      dm->substitutions = (struct substitution_def *)
	realloc (dm->substitutions, new_array_size);
      if (dm->substitutions == NULL)
	/* Realloc failed.  */
	{
	  dyn_string_delete (substitution);
	  return STATUS_ALLOCATION_FAILED;
	}
    }

  /* Add the substitution to the array.  */
  dm->substitutions[i].text = substitution;
  dm->substitutions[i].template_p = template_p;
  dm->substitutions[i].template_parm_number = template_parm_number;
  ++dm->num_substitutions;

#ifdef CP_DEMANGLE_DEBUG
  substitutions_print (dm, stderr);
#endif

  return STATUS_OK;
}

/* Returns the Nth-most-recent substitution.  Sets *TEMPLATE_P to
   non-zero if the substitution is a template-id, zero otherwise.  
   N is numbered from zero.  DM retains ownership of the returned
   string.  If N is negative, or equal to or greater than the current
   number of substitution candidates, returns NULL.  */

static dyn_string_t
substitution_get (dm, n, template_p)
     demangling_t dm;
     int n;
     int *template_p;
{
  struct substitution_def *sub;

  /* Make sure N is in the valid range.  */
  if (n < 0 || n >= dm->num_substitutions)
    return NULL;

  sub = &(dm->substitutions[n]);
  *template_p = sub->template_p;
  return sub->text;
}

#ifdef CP_DEMANGLE_DEBUG
/* Debugging routine to print the current substitutions to FP.  */

static void
substitutions_print (dm, fp)
     demangling_t dm;
     FILE *fp;
{
  int seq_id;
  int num = dm->num_substitutions;

  fprintf (fp, "SUBSTITUTIONS:\n");
  for (seq_id = -1; seq_id < num - 1; ++seq_id)
    {
      int template_p;
      dyn_string_t text = substitution_get (dm, seq_id + 1, &template_p);

      if (seq_id == -1)
	fprintf (fp, " S_ ");
      else
	fprintf (fp, " S%d_", seq_id);
      fprintf (fp, " %c: %s\n", template_p ? '*' : ' ', dyn_string_buf (text));
    }
}

#endif /* CP_DEMANGLE_DEBUG */

/* Creates a new template argument list.  Returns NULL if allocation
   fails.  */

static template_arg_list_t
template_arg_list_new ()
{
  template_arg_list_t new_list =
    (template_arg_list_t) malloc (sizeof (struct template_arg_list_def));
  if (new_list == NULL)
    return NULL;
  /* Initialize the new list to have no arguments.  */
  new_list->first_argument = NULL;
  new_list->last_argument = NULL;
  /* Return the new list.  */
  return new_list;
}

/* Deletes a template argument list and the template arguments it
   contains.  */

static void
template_arg_list_delete (list)
     template_arg_list_t list;
{
  /* If there are any arguments on LIST, delete them.  */
  if (list->first_argument != NULL)
    string_list_delete (list->first_argument);
  /* Delete LIST.  */
  free (list);
}

/* Adds ARG to the template argument list ARG_LIST.  */

static void 
template_arg_list_add_arg (arg_list, arg)
     template_arg_list_t arg_list;
     string_list_t arg;
{
  if (arg_list->first_argument == NULL)
    /* If there were no arguments before, ARG is the first one.  */
    arg_list->first_argument = arg;
  else
    /* Make ARG the last argument on the list.  */
    arg_list->last_argument->next = arg;
  /* Make ARG the last on the list.  */
  arg_list->last_argument = arg;
  arg->next = NULL;
}

/* Returns the template arugment at position INDEX in template
   argument list ARG_LIST.  */

static string_list_t
template_arg_list_get_arg (arg_list, index)
     template_arg_list_t arg_list;
     int index;
{
  string_list_t arg = arg_list->first_argument;
  /* Scan down the list of arguments to find the one at position
     INDEX.  */
  while (index--)
    {
      arg = arg->next;
      if (arg == NULL)
	/* Ran out of arguments before INDEX hit zero.  That's an
	   error.  */
	return NULL;
    }
  /* Return the argument at position INDEX.  */
  return arg;
}

/* Pushes ARG_LIST onto the top of the template argument list stack.  */

static void
push_template_arg_list (dm, arg_list)
     demangling_t dm;
     template_arg_list_t arg_list;
{
  arg_list->next = dm->template_arg_lists;
  dm->template_arg_lists = arg_list;
#ifdef CP_DEMANGLE_DEBUG
  fprintf (stderr, " ** pushing template arg list\n");
  template_arg_list_print (arg_list, stderr);
#endif 
}

/* Pops and deletes elements on the template argument list stack until
   arg_list is the topmost element.  If arg_list is NULL, all elements
   are popped and deleted.  */

static void
pop_to_template_arg_list (dm, arg_list)
     demangling_t dm;
     template_arg_list_t arg_list;
{
  while (dm->template_arg_lists != arg_list)
    {
      template_arg_list_t top = dm->template_arg_lists;
      /* Disconnect the topmost element from the list.  */
      dm->template_arg_lists = top->next;
      /* Delete the popped element.  */
      template_arg_list_delete (top);
#ifdef CP_DEMANGLE_DEBUG
      fprintf (stderr, " ** removing template arg list\n");
#endif
    }
}

#ifdef CP_DEMANGLE_DEBUG

/* Prints the contents of ARG_LIST to FP.  */

static void
template_arg_list_print (arg_list, fp)
  template_arg_list_t arg_list;
  FILE *fp;
{
  string_list_t arg;
  int index = -1;

  fprintf (fp, "TEMPLATE ARGUMENT LIST:\n");
  for (arg = arg_list->first_argument; arg != NULL; arg = arg->next)
    {
      if (index == -1)
	fprintf (fp, " T_  : ");
      else
	fprintf (fp, " T%d_ : ", index);
      ++index;
      fprintf (fp, "%s\n", dyn_string_buf ((dyn_string_t) arg));
    }
}

#endif /* CP_DEMANGLE_DEBUG */

/* Returns the topmost element on the stack of template argument
   lists.  If there is no list of template arguments, returns NULL.  */

static template_arg_list_t
current_template_arg_list (dm)
     demangling_t dm;
{
  return dm->template_arg_lists;
}

/* Allocates a demangling_t object for demangling mangled NAME.  A new
   result must be pushed before the returned object can be used.
   Returns NULL if allocation fails.  */

static demangling_t
demangling_new (name)
     const char *name;
{
  demangling_t dm;
  dm = (demangling_t) malloc (sizeof (struct demangling_def));
  if (dm == NULL)
    return NULL;

  dm->name = name;
  dm->next = name;
  dm->result = NULL;
  dm->num_substitutions = 0;
  dm->substitutions_allocated = 10;
  dm->template_arg_lists = NULL;
  dm->last_source_name = dyn_string_new (0);
  if (dm->last_source_name == NULL)
    return NULL;
  dm->substitutions = (struct substitution_def *)
    malloc (dm->substitutions_allocated * sizeof (struct substitution_def));
  if (dm->substitutions == NULL)
    {
      dyn_string_delete (dm->last_source_name);
      return NULL;
    }

  return dm;
}

/* Deallocates a demangling_t object and all memory associated with
   it.  */

static void
demangling_delete (dm)
     demangling_t dm;
{
  int i;
  template_arg_list_t arg_list = dm->template_arg_lists;

  /* Delete the stack of template argument lists.  */
  while (arg_list != NULL)
    {
      template_arg_list_t next = arg_list->next;
      template_arg_list_delete (arg_list);
      arg_list = next;
    }
  /* Delete the list of substitutions.  */
  for (i = dm->num_substitutions; --i >= 0; )
    dyn_string_delete (dm->substitutions[i].text);
  free (dm->substitutions);
  /* Delete the demangled result.  */
  string_list_delete (dm->result);
  /* Delete the stored identifier name.  */
  dyn_string_delete (dm->last_source_name);
  /* Delete the context object itself.  */
  free (dm);
}

/* These functions demangle an alternative of the corresponding
   production in the mangling spec.  The first argument of each is a
   demangling context structure for the current demangling
   operation.  Most emit demangled text directly to the topmost result
   string on the result string stack in the demangling context
   structure.  */

static status_t demangle_char
  PARAMS ((demangling_t, int));
static status_t demangle_mangled_name 
  PARAMS ((demangling_t));
static status_t demangle_encoding
  PARAMS ((demangling_t));
static status_t demangle_name
  PARAMS ((demangling_t, int *));
static status_t demangle_nested_name
  PARAMS ((demangling_t, int *));
static status_t demangle_prefix
  PARAMS ((demangling_t, int *));
static status_t demangle_unqualified_name
  PARAMS ((demangling_t));
static status_t demangle_source_name
  PARAMS ((demangling_t));
static status_t demangle_number
  PARAMS ((demangling_t, int *, int, int));
static status_t demangle_number_literally
  PARAMS ((demangling_t, dyn_string_t, int, int));
static status_t demangle_identifier
  PARAMS ((demangling_t, int, dyn_string_t));
static status_t demangle_operator_name
  PARAMS ((demangling_t, int, int *));
static status_t demangle_special_name
  PARAMS ((demangling_t));
static status_t demangle_ctor_dtor_name
  PARAMS ((demangling_t));
static status_t demangle_type_ptr
  PARAMS ((demangling_t));
static status_t demangle_type
  PARAMS ((demangling_t));
static status_t demangle_CV_qualifiers
  PARAMS ((demangling_t, dyn_string_t));
static status_t demangle_builtin_type
  PARAMS ((demangling_t));
static status_t demangle_function_type
  PARAMS ((demangling_t, int));
static status_t demangle_bare_function_type
  PARAMS ((demangling_t, int));
static status_t demangle_class_enum_type
  PARAMS ((demangling_t, int *));
static status_t demangle_array_type
  PARAMS ((demangling_t));
static status_t demangle_template_param
  PARAMS ((demangling_t, int *));
static status_t demangle_template_args
  PARAMS ((demangling_t));
static status_t demangle_literal
  PARAMS ((demangling_t));
static status_t demangle_template_arg
  PARAMS ((demangling_t));
static status_t demangle_expression
  PARAMS ((demangling_t));
static status_t demangle_scope_expression
  PARAMS ((demangling_t));
static status_t demangle_expr_primary
  PARAMS ((demangling_t));
static status_t demangle_substitution
  PARAMS ((demangling_t, int *, int *));
static status_t demangle_local_name
  PARAMS ((demangling_t));
static status_t demangle_discriminator 
  PARAMS ((demangling_t, int));
static status_t cp_demangle
  PARAMS ((const char *, dyn_string_t));
static status_t cp_demangle_type
  PARAMS ((const char*, dyn_string_t));

/* When passed to demangle_bare_function_type, indicates that the
   function's return type is not encoded before its parameter types.  */
#define BFT_NO_RETURN_TYPE    -1

/* Check that the next character is C.  If so, consume it.  If not,
   return an error.  */

static status_t
demangle_char (dm, c)
     demangling_t dm;
     int c;
{
  static char *error_message = NULL;

  if (peek_char (dm) == c)
    {
      advance_char (dm);
      return STATUS_OK;
    }
  else
    {
      if (error_message == NULL)
	error_message = strdup ("Expected ?");
      error_message[9] = c;
      return error_message;
    }
}

/* Demangles and emits a <mangled-name>.  

    <mangled-name>      ::= _Z <encoding>  */

static status_t
demangle_mangled_name (dm)
     demangling_t dm;
{
  DEMANGLE_TRACE ("mangled-name", dm);
  RETURN_IF_ERROR (demangle_char (dm, '_'));
  RETURN_IF_ERROR (demangle_char (dm, 'Z'));
  RETURN_IF_ERROR (demangle_encoding (dm));
  return STATUS_OK;
}

/* Demangles and emits an <encoding>.  

    <encoding>		::= <function name> <bare-function-type>
			::= <data name>
			::= <special-name>  */

static status_t
demangle_encoding (dm)
     demangling_t dm;
{
  int template_p;
  int start_position;
  template_arg_list_t old_arg_list = current_template_arg_list (dm);
  char peek = peek_char (dm);

  DEMANGLE_TRACE ("encoding", dm);
  
  /* Remember where the name starts.  If it turns out to be a template
     function, we'll have to insert the return type here.  */
  start_position = result_length (dm);

  if (peek == 'G' || peek == 'T')
    RETURN_IF_ERROR (demangle_special_name (dm));
  else
    {
      /* Now demangle the name.  */
      RETURN_IF_ERROR (demangle_name (dm, &template_p));

      /* If there's anything left, the name was a function name, with
	 maybe its return type, and its parameters types, following.  */
      if (!end_of_name_p (dm) 
	  && peek_char (dm) != 'E')
	{
	  if (template_p)
	    /* Template functions have their return type encoded.  The
	       return type should be inserted at start_position.  */
	    RETURN_IF_ERROR 
	      (demangle_bare_function_type (dm, start_position));
	  else
	    /* Non-template functions don't have their return type
	       encoded.  */
	    RETURN_IF_ERROR 
	      (demangle_bare_function_type (dm, BFT_NO_RETURN_TYPE)); 
	}
    }

  /* Pop off template argument lists that were built during the
     mangling of this name, to restore the old template context.  */
  pop_to_template_arg_list (dm, old_arg_list);

  return STATUS_OK;
}

/* Demangles and emits a <name>.

    <name>              ::= <unscoped-name>
                        ::= <unscoped-template-name> <template-args>
			::= <nested-name>
                        ::= <local-name>

    <unscoped-name>     ::= <unqualified-name>
			::= St <unqualified-name>   # ::std::

    <unscoped-template-name>    
                        ::= <unscoped-name>
                        ::= <substitution>  */

static status_t
demangle_name (dm, template_p)
     demangling_t dm;
     int *template_p;
{
  int special_std_substitution;
  int start = substitution_start (dm);

  DEMANGLE_TRACE ("name", dm);

  switch (peek_char (dm))
    {
    case 'N':
      /* This is a <nested-name>.  */
      RETURN_IF_ERROR (demangle_nested_name (dm, template_p));
      break;

    case 'Z':
      RETURN_IF_ERROR (demangle_local_name (dm));
      break;

    case 'S':
      /* The `St' substitution allows a name nested in std:: to appear
	 without being enclosed in a nested name.  */
      if (peek_char_next (dm) == 't') 
	{
	  (void) next_char (dm);
	  (void) next_char (dm);
	  RETURN_IF_ERROR (result_append (dm, "std::"));
	  RETURN_IF_ERROR (demangle_unqualified_name (dm));
	}
      else
	{
	  RETURN_IF_ERROR (demangle_substitution (dm, template_p,
						  &special_std_substitution));
	  if (special_std_substitution)
	    {
	      /* This was the magic `std::' substitution.  We can have
		 a <nested-name> or one of the unscoped names
		 following.  */
	      RETURN_IF_ERROR (result_append (dm, "::"));
	      RETURN_IF_ERROR (demangle_name (dm, template_p));
	    }
	}
      /* Check if a template argument list immediately follows.
	 If so, then we just demangled an <unqualified-template-name>.  */
      if (peek_char (dm) == 'I') 
	{
	  RETURN_IF_ERROR (substitution_add (dm, start, 0, 
					     NOT_TEMPLATE_PARM));
	  RETURN_IF_ERROR (demangle_template_args (dm));
	}
      break;

    default:
      /* This is an <unscoped-name> or <unscoped-template-name>.  */
      RETURN_IF_ERROR (demangle_unqualified_name (dm));

      /* If the <unqualified-name> is followed by template args, this
	 is an <unscoped-template-name>.  */
      if (peek_char (dm) == 'I')
	{
	  /* Add a substitution for the unqualified template name.  */
	  RETURN_IF_ERROR (substitution_add (dm, start, 0, 
					     NOT_TEMPLATE_PARM));

	  RETURN_IF_ERROR (demangle_template_args (dm));
	  *template_p = 1;
	}
      else
	*template_p = 0;

      break;
    }

  return STATUS_OK;
}

/* Demangles and emits a <nested-name>. 

    <nested-name>       ::= N [<CV-qualifiers>] <prefix> <component> E  */

static status_t
demangle_nested_name (dm, template_p)
     demangling_t dm;
     int *template_p;
{
  char peek;

  DEMANGLE_TRACE ("nested-name", dm);

  RETURN_IF_ERROR (demangle_char (dm, 'N'));

  peek = peek_char (dm);
  if (peek == 'r' || peek == 'V' || peek == 'K')
    {
      status_t status;

      /* Snarf up and emit CV qualifiers.  */
      dyn_string_t cv_qualifiers = dyn_string_new (24);
      if (cv_qualifiers == NULL)
	return STATUS_ALLOCATION_FAILED;

      demangle_CV_qualifiers (dm, cv_qualifiers);
      status = result_append_string (dm, cv_qualifiers);
      dyn_string_delete (cv_qualifiers);
      RETURN_IF_ERROR (status);
      RETURN_IF_ERROR (result_append_space (dm));
    }
  
  RETURN_IF_ERROR (demangle_prefix (dm, template_p));
  /* No need to demangle the final <component>; demangle_prefix will
     handle it.  */
  RETURN_IF_ERROR (demangle_char (dm, 'E'));

  return STATUS_OK;
}

/* Demangles and emits a <prefix>.

    <prefix>            ::= <prefix> <component>
                        ::= <template-prefix> <template-args>
			::= # empty
			::= <substitution>

    <template-prefix>   ::= <prefix>
                        ::= <substitution>

    <component>         ::= <unqualified-name>
                        ::= <local-name>  */

static status_t
demangle_prefix (dm, template_p)
     demangling_t dm;
     int *template_p;
{
  int start = substitution_start (dm);
  int nested = 0;

  /* TEMPLATE_P is updated as we decend the nesting chain.  After
     <template-args>, it is set to non-zero; after everything else it
     is set to zero.  */

  DEMANGLE_TRACE ("prefix", dm);

  while (1)
    {
      char peek;
      int unused;

      if (end_of_name_p (dm))
	return "Unexpected end of name in <compound-name>.";

      peek = peek_char (dm);
      
      if (IS_DIGIT ((unsigned char) peek)
	  || (peek >= 'a' && peek <= 'z')
	  || peek == 'C' || peek == 'D'
	  || peek == 'S')
	{
	  /* We have another level of scope qualification.  */
	  if (nested)
	    RETURN_IF_ERROR (result_append (dm, "::"));
	  else
	    nested = 1;

	  if (peek == 'S')
	    /* The substitution determines whether this is a
	       template-id.   */
	    RETURN_IF_ERROR (demangle_substitution (dm, template_p, 
						    &unused));
	  else
	    {
	      RETURN_IF_ERROR (demangle_unqualified_name (dm));
	      *template_p = 0;
	    }
	}
      else if (peek == 'Z')
	RETURN_IF_ERROR (demangle_local_name (dm));
      else if (peek == 'I')
	{
	  if (*template_p)
	    return STATUS_INTERNAL_ERROR;
	  /* The template name is a substitution candidate.  */
	  RETURN_IF_ERROR (substitution_add (dm, start, 0, NOT_TEMPLATE_PARM));
	  RETURN_IF_ERROR (demangle_template_args (dm));
	  *template_p = 1;
	}
      else if (peek == 'E')
	/* All done.  */
	return STATUS_OK;
      else
	return "Unexpected character in <compound-name>.";

      /* Add a new substitution for the prefix thus far.  */
      RETURN_IF_ERROR (substitution_add (dm, start, *template_p, 
					 NOT_TEMPLATE_PARM));
    }
}

/* Demangles and emits an <unqualified-name>.  If the
   <unqualified-name> is a function and the first element in the
   argument list should be taken to be its return type,
   ENCODE_RETURN_TYPE is non-zero.

    <unqualified-name>  ::= <operator-name>
			::= <special-name>  
			::= <source-name>  */

static status_t
demangle_unqualified_name (dm)
     demangling_t dm;
{
  char peek = peek_char (dm);

  DEMANGLE_TRACE ("unqualified-name", dm);

  if (IS_DIGIT ((unsigned char) peek))
    RETURN_IF_ERROR (demangle_source_name (dm));
  else if (peek >= 'a' && peek <= 'z')
    {
      int num_args;
      RETURN_IF_ERROR (demangle_operator_name (dm, 0, &num_args));
    }
  else if (peek == 'C' || peek == 'D')
    RETURN_IF_ERROR (demangle_ctor_dtor_name (dm));
  else
    return "Unexpected character in <unqualified-name>.";

  return STATUS_OK;
}

/* Demangles and emits <source-name>.  

    <source-name> ::= <length number> <identifier>  */

static status_t
demangle_source_name (dm)
     demangling_t dm;
{
  int length;

  DEMANGLE_TRACE ("source-name", dm);

  /* Decode the length of the identifier.  */
  RETURN_IF_ERROR (demangle_number (dm, &length, 10, 0));
  if (length == 0)
    return "Zero length in <source-name>.";

  /* Now the identifier itself.  It's placed into last_source_name,
     where it can be used to build a constructor or destructor name.  */
  RETURN_IF_ERROR (demangle_identifier (dm, length, 
					dm->last_source_name));

  /* Emit it.  */
  RETURN_IF_ERROR (result_append_string (dm, dm->last_source_name));

  return STATUS_OK;
}

/* Demangles a number, either a <number> or a <positive-number> at the
   current position, consuming all consecutive digit characters.  Sets
   *VALUE to the resulting numberand returns STATUS_OK.  The number is
   interpreted as BASE, which must be either 10 or 36.  If IS_SIGNED
   is non-zero, negative numbers -- prefixed with `n' -- are accepted.

    <number> ::= [n] <positive-number>

    <positive-number> ::= <decimal integer>  */

static status_t
demangle_number (dm, value, base, is_signed)
     demangling_t dm;
     int *value;
     int base;
     int is_signed;
{
  dyn_string_t number = dyn_string_new (10);

  DEMANGLE_TRACE ("number", dm);

  if (number == NULL)
    return STATUS_ALLOCATION_FAILED;

  demangle_number_literally (dm, number, base, is_signed);
  *value = strtol (dyn_string_buf (number), NULL, base);
  dyn_string_delete (number);

  return STATUS_OK;
}

/* Demangles a number at the current position.  The digits (and minus
   sign, if present) that make up the number are appended to STR.
   Only base-BASE digits are accepted; BASE must be either 10 or 36.
   If IS_SIGNED, negative numbers -- prefixed with `n' -- are
   accepted.  Does not consume a trailing underscore or other
   terminating character.  */

static status_t
demangle_number_literally (dm, str, base, is_signed)
     demangling_t dm;
     dyn_string_t str;
     int base;
     int is_signed;
{
  DEMANGLE_TRACE ("number*", dm);

  if (base != 10 && base != 36)
    return STATUS_INTERNAL_ERROR;

  /* An `n' denotes a negative number.  */
  if (is_signed && peek_char (dm) == 'n')
    {
      /* Skip past the n.  */
      advance_char (dm);
      /* The normal way to write a negative number is with a minus
	 sign.  */
      if (!dyn_string_append_char (str, '-'))
	return STATUS_ALLOCATION_FAILED;
    }

  /* Loop until we hit a non-digit.  */
  while (1)
    {
      char peek = peek_char (dm);
      if (IS_DIGIT ((unsigned char) peek)
	  || (base == 36 && peek >= 'A' && peek <= 'Z'))
	{
	  /* Accumulate digits.  */
	  if (!dyn_string_append_char (str, next_char (dm)))
	    return STATUS_ALLOCATION_FAILED;
	}
      else
	/* Not a digit?  All done.  */
	break;
    }

  return STATUS_OK;
}

/* Demangles an identifier at the current position of LENGTH
   characters and places it in IDENTIFIER.  */

static status_t
demangle_identifier (dm, length, identifier)
     demangling_t dm;
     int length;
     dyn_string_t identifier;
{
  DEMANGLE_TRACE ("identifier", dm);

  dyn_string_clear (identifier);
  if (!dyn_string_resize (identifier, length))
    return STATUS_ALLOCATION_FAILED;

  while (length-- > 0)
    {
      if (end_of_name_p (dm))
	return "Unexpected end of name in <identifier>.";
      if (!dyn_string_append_char (identifier, next_char (dm)))
	return STATUS_ALLOCATION_FAILED;
    }

  return STATUS_OK;
}

/* Demangles and emits an <operator-name>.  If SHORT_NAME is non-zero,
   the short form is emitted; otherwise the full source form
   (`operator +' etc.) is emitted.  *NUM_ARGS is set to the number of
   operands that the operator takes.  

    <operator-name>
                  ::= nw        # new           
                  ::= na        # new[]
                  ::= dl        # delete        
                  ::= da        # delete[]      
		  ::= ps        # + (unary)
                  ::= ng        # - (unary)     
                  ::= ad        # & (unary)     
                  ::= de        # * (unary)     
                  ::= co        # ~             
                  ::= pl        # +             
                  ::= mi        # -             
                  ::= ml        # *             
                  ::= dv        # /             
                  ::= rm        # %             
                  ::= an        # &             
                  ::= or        # |             
                  ::= eo        # ^             
                  ::= aS        # =             
                  ::= pL        # +=            
                  ::= mI        # -=            
                  ::= mL        # *=            
                  ::= dV        # /=            
                  ::= rM        # %=            
                  ::= aN        # &=            
                  ::= oR        # |=            
                  ::= eO        # ^=            
                  ::= ls        # <<            
                  ::= rs        # >>            
                  ::= lS        # <<=           
                  ::= rS        # >>=           
                  ::= eq        # ==            
                  ::= ne        # !=            
                  ::= lt        # <             
                  ::= gt        # >             
                  ::= le        # <=            
                  ::= ge        # >=            
                  ::= nt        # !             
                  ::= aa        # &&            
                  ::= oo        # ||            
                  ::= pp        # ++            
                  ::= mm        # --            
                  ::= cm        # ,             
                  ::= pm        # ->*           
                  ::= pt        # ->            
                  ::= cl        # ()            
                  ::= ix        # []            
                  ::= qu        # ?
                  ::= sz        # sizeof 
                  ::= cv <type> # cast        
                  ::= vx <source-name>  # vendor extended operator  */

static status_t
demangle_operator_name (dm, short_name, num_args)
     demangling_t dm;
     int short_name;
     int *num_args;
{
  struct operator_code
  {
    /* The mangled code for this operator.  */
    const char *code;
    /* The source name of this operator.  */
    const char *name;
    /* The number of arguments this operator takes.  */
    int num_args;
  };

  static const struct operator_code operators[] = 
  {
    { "aN", "&="       , 2 },
    { "aS", "="        , 2 },
    { "aa", "&&"       , 2 },
    { "ad", "&"        , 1 },
    { "an", "&"        , 2 },
    { "cl", "()"       , 0 },
    { "cm", ","        , 2 },
    { "co", "~"        , 1 },
    { "dV", "/="       , 2 },
    { "da", " delete[]", 1 },
    { "de", "*"        , 1 },
    { "dl", " delete"  , 1 },
    { "dv", "/"        , 2 },
    { "eO", "^="       , 2 },
    { "eo", "^"        , 2 },
    { "eq", "=="       , 2 },
    { "ge", ">="       , 2 },
    { "gt", ">"        , 2 },
    { "ix", "[]"       , 2 },
    { "lS", "<<="      , 2 },
    { "le", "<="       , 2 },
    { "ls", "<<"       , 2 },
    { "lt", "<"        , 2 },
    { "mI", "-="       , 2 },
    { "mL", "*="       , 2 },
    { "mi", "-"        , 2 },
    { "ml", "*"        , 2 },
    { "mm", "--"       , 1 },
    { "na", " new[]"   , 1 },
    { "ne", "!="       , 2 },
    { "ng", "-"        , 1 },
    { "nt", "!"        , 1 },
    { "nw", " new"     , 1 },
    { "oR", "|="       , 2 },
    { "oo", "||"       , 2 },
    { "or", "|"        , 2 },
    { "pL", "+="       , 2 },
    { "pl", "+"        , 2 },
    { "pm", "->*"      , 2 },
    { "pp", "++"       , 1 },
    { "ps", "+"        , 1 },
    { "qu", "?"        , 3 },
    { "rM", "%="       , 2 },
    { "rS", ">>="      , 2 },
    { "rm", "%"        , 2 },
    { "rs", ">>"       , 2 },
    { "sz", " sizeof"  , 1 }
  };

  const int num_operators = 
    sizeof (operators) / sizeof (struct operator_code);

  int c0 = next_char (dm);
  int c1 = next_char (dm);
  const struct operator_code* p1 = operators;
  const struct operator_code* p2 = operators + num_operators;

  DEMANGLE_TRACE ("operator-name", dm);

  /* Is this a vendor extended operator?  */
  if (c0 == 'v' && c1 == 'x')
    {
      RETURN_IF_ERROR (result_append (dm, "operator"));
      RETURN_IF_ERROR (demangle_source_name (dm));
      *num_args = 0;
      return STATUS_OK;
    }

  /* Is this a conversion operator?  */
  if (c0 == 'c' && c1 == 'v')
    {
      RETURN_IF_ERROR (result_append (dm, "operator "));
      /* Demangle the converted-to type.  */
      RETURN_IF_ERROR (demangle_type (dm));
      *num_args = 0;
      return STATUS_OK;
    }

  /* Perform a binary search for the operator code.  */
  while (1)
    {
      const struct operator_code* p = p1 + (p2 - p1) / 2;
      char match0 = p->code[0];
      char match1 = p->code[1];

      if (c0 == match0 && c1 == match1)
	/* Found it.  */
	{
	  if (!short_name)
	    RETURN_IF_ERROR (result_append (dm, "operator"));
	  RETURN_IF_ERROR (result_append (dm, p->name));
	  *num_args = p->num_args;

	  return STATUS_OK;
	}

      if (p == p1)
	/* Couldn't find it.  */
	return "Unknown code in <operator-name>.";

      /* Try again.  */
      if (c0 < match0 || (c0 == match0 && c1 < match1))
	p2 = p;
      else
	p1 = p;
    }
}

/* Demangles and emits a <special-name>.  

    <special-name> ::= GV <object name>   # Guard variable
                   ::= Th[n] <offset number> _ <base name> <base encoding>
                                          # non-virtual base override thunk
                   ::= Tv[n] <offset number> _ <vcall offset number> 
                         _ <base encoding>
                                          # virtual base override thunk
                   ::= TV <type>          # virtual table
                   ::= TT <type>          # VTT
                   ::= TI <type>          # typeinfo structure
		   ::= TS <type>          # typeinfo name  

   Also demangles the special g++ manglings,

    <special-name> ::= CT <type> <offset number> _ <base type>
                                          # construction vtable
		   ::= TF <type>	  # typeinfo function (old ABI only)
		   ::= TJ <type>	  # java Class structure  */

static status_t
demangle_special_name (dm)
     demangling_t dm;
{
  dyn_string_t number;
  int unused;
  char peek = peek_char (dm);

  DEMANGLE_TRACE ("special-name", dm);

  if (peek == 'G')
    {
      /* A guard variable name.  Consume the G.  */
      advance_char (dm);
      RETURN_IF_ERROR (demangle_char (dm, 'V'));
      RETURN_IF_ERROR (result_append (dm, "guard variable for "));
      RETURN_IF_ERROR (demangle_name (dm, &unused));
    }
  else if (peek == 'T')
    {
      status_t status = STATUS_OK;

      /* Other C++ implementation miscellania.  Consume the T.  */
      advance_char (dm);

      switch (peek_char (dm))
	{
	case 'V':
	  /* Virtual table.  */
	  advance_char (dm);
	  RETURN_IF_ERROR (result_append (dm, "vtable for "));
	  RETURN_IF_ERROR (demangle_type (dm));
	  break;

	case 'T':
	  /* VTT structure.  */
	  advance_char (dm);
	  RETURN_IF_ERROR (result_append (dm, "VTT for "));
	  RETURN_IF_ERROR (demangle_type (dm));
	  break;

	case 'I':
	  /* Typeinfo structure.  */
	  advance_char (dm);
	  RETURN_IF_ERROR (result_append (dm, "typeinfo for "));
	  RETURN_IF_ERROR (demangle_type (dm));
	  break;

	case 'F':
	  /* Typeinfo function.  Used only in old ABI with new mangling.  */
	  advance_char (dm);
	  RETURN_IF_ERROR (result_append (dm, "typeinfo fn for "));
	  RETURN_IF_ERROR (demangle_type (dm));
	  break;

	case 'S':
	  /* Character string containing type name, used in typeinfo. */
	  advance_char (dm);
	  RETURN_IF_ERROR (result_append (dm, "typeinfo name for "));
	  RETURN_IF_ERROR (demangle_type (dm));
	  break;

	case 'J':
	  /* The java Class variable corresponding to a C++ class.  */
	  advance_char (dm);
	  RETURN_IF_ERROR (result_append (dm, "java Class for "));
	  RETURN_IF_ERROR (demangle_type (dm));
	  break;

	case 'h':
	  /* Non-virtual thunk.  */
	  advance_char (dm);
	  RETURN_IF_ERROR (result_append (dm, "non-virtual thunk"));
	  /* Demangle and emit the offset.  */
	  number = dyn_string_new (4);
	  if (number == NULL)
	    return STATUS_ALLOCATION_FAILED;
	  demangle_number_literally (dm, number, 10, 1);
	  /* Don't display the offset unless in verbose mode.  */
	  if (flag_verbose)
	    {
	      status = result_append_char (dm, ' ');
	      if (STATUS_NO_ERROR (status))
		status = result_append_string (dm, number);
	    }
	  dyn_string_delete (number);
	  RETURN_IF_ERROR (status);
	  /* Demangle the separator.  */
	  RETURN_IF_ERROR (demangle_char (dm, '_'));
	  /* Demangle and emit the target name and function type.  */
	  RETURN_IF_ERROR (result_append (dm, " to "));
	  RETURN_IF_ERROR (demangle_encoding (dm));
	  break;

	case 'v':
	  /* Virtual thunk.  */
	  advance_char (dm);
	  RETURN_IF_ERROR (result_append (dm, "virtual thunk "));
	  /* Demangle and emit the offset.  */
	  number = dyn_string_new (4);
	  if (number == NULL)
	    return STATUS_ALLOCATION_FAILED;
	  demangle_number_literally (dm, number, 10, 1);
	  /* Don't display the offset unless in verbose mode.  */
	  if (flag_verbose)
	    {
	      status = result_append_string (dm, number);
	      if (STATUS_NO_ERROR (status))
		result_append_char (dm, ' ');
	    }
	  dyn_string_delete (number);
	  RETURN_IF_ERROR (status);
	  /* Demangle the separator.  */
	  RETURN_IF_ERROR (demangle_char (dm, '_'));
	  /* Demangle and emit the vcall offset.  */
	  number = dyn_string_new (4);
	  if (number == NULL)
	    return STATUS_ALLOCATION_FAILED;
	  demangle_number_literally (dm, number, 10, 1);
	  /* Don't display the vcall offset unless in verbose mode.  */
	  if (flag_verbose)
	    {
	      status = result_append_string (dm, number);
	      if (STATUS_NO_ERROR (status))
		status = result_append_char (dm, ' ');
	    }
	  dyn_string_delete (number);
	  RETURN_IF_ERROR (status);
	  /* Demangle the separator.  */
	  RETURN_IF_ERROR (demangle_char (dm, '_'));
	  /* Demangle and emit the target function.  */
	  RETURN_IF_ERROR (result_append (dm, "to "));
	  RETURN_IF_ERROR (demangle_encoding (dm));
	  break;

	case 'C':
	  /* TC is a special g++ mangling for a construction vtable. */
	  if (!flag_strict)
	    {
	      dyn_string_t derived_type;

	      advance_char (dm);
	      RETURN_IF_ERROR (result_append (dm, "construction vtable for "));

	      /* Demangle the derived type off to the side.  */
	      RETURN_IF_ERROR (result_push (dm));
	      RETURN_IF_ERROR (demangle_type (dm));
	      derived_type = (dyn_string_t) result_pop (dm);

	      /* Demangle the offset.  */
	      number = dyn_string_new (4);
	      if (number == NULL)
		{
		  dyn_string_delete (derived_type);
		  return STATUS_ALLOCATION_FAILED;
		}
	      demangle_number_literally (dm, number, 10, 1);
	      /* Demangle the underscore separator.  */
	      status = demangle_char (dm, '_');

	      /* Demangle the base type.  */
	      if (STATUS_NO_ERROR (status))
		status = demangle_type (dm);

	      /* Emit the derived type.  */
	      if (STATUS_NO_ERROR (status))
		status = result_append (dm, "-in-");
	      if (STATUS_NO_ERROR (status))
		status = result_append_string (dm, derived_type);
	      dyn_string_delete (derived_type);

	      /* Don't display the offset unless in verbose mode.  */
	      if (flag_verbose)
		{
		  status = result_append_char (dm, ' ');
		  if (STATUS_NO_ERROR (status))
		    result_append_string (dm, number);
		}
	      dyn_string_delete (number);
	      RETURN_IF_ERROR (status);
	      break;
	    }
	  /* If flag_strict, fall through.  */

	default:
	  return "Unrecognized <special-name>.";
	}
    }
  else
    return STATUS_ERROR;

  return STATUS_OK;
}

/* Demangles and emits a <ctor-dtor-name>.  
   
    <ctor-dtor-name>
                   ::= C1  # complete object (in-charge) ctor
                   ::= C2  # base object (not-in-charge) ctor
                   ::= C3  # complete object (in-charge) allocating ctor
                   ::= C4  # base object (not-in-charge) allocating ctor
                   ::= D0  # deleting (in-charge) dtor
                   ::= D1  # complete object (in-charge) dtor
                   ::= D2  # base object (not-in-charge) dtor  */

static status_t
demangle_ctor_dtor_name (dm)
     demangling_t dm;
{
  static const char *const ctor_flavors[] = 
  {
    "in-charge",
    "not-in-charge",
    "in-charge allocating",
    "not-in-charge allocating"
  };
  static const char *const dtor_flavors[] = 
  {
    "in-charge deleting",
    "in-charge",
    "not-in-charge"
  };

  int flavor;
  char peek = peek_char (dm);

  DEMANGLE_TRACE ("ctor-dtor-name", dm);
  
  if (peek == 'C')
    {
      /* A constructor name.  Consume the C.  */
      advance_char (dm);
      if (peek_char (dm) < '1' || peek_char (dm) > '4')
	return "Unrecognized constructor.";
      RETURN_IF_ERROR (result_append_string (dm, dm->last_source_name));
      /* Print the flavor of the constructor if in verbose mode.  */
      flavor = next_char (dm) - '1';
      if (flag_verbose)
	{
	  RETURN_IF_ERROR (result_append (dm, "["));
	  RETURN_IF_ERROR (result_append (dm, ctor_flavors[flavor]));
	  RETURN_IF_ERROR (result_append_char (dm, ']'));
	}
    }
  else if (peek == 'D')
    {
      /* A destructor name.  Consume the D.  */
      advance_char (dm);
      if (peek_char (dm) < '0' || peek_char (dm) > '2')
	return "Unrecognized destructor.";
      RETURN_IF_ERROR (result_append_char (dm, '~'));
      RETURN_IF_ERROR (result_append_string (dm, dm->last_source_name));
      /* Print the flavor of the destructor if in verbose mode.  */
      flavor = next_char (dm) - '0';
      if (flag_verbose)
	{
	  RETURN_IF_ERROR (result_append (dm, " ["));
	  RETURN_IF_ERROR (result_append (dm, dtor_flavors[flavor]));
	  RETURN_IF_ERROR (result_append_char (dm, ']'));
	}
    }
  else
    return STATUS_ERROR;

  return STATUS_OK;
}

/* Handle pointer, reference, and pointer-to-member cases for
   demangle_type.  All consecutive `P's, `R's, and 'M's are joined to
   build a pointer/reference type.  We snarf all these, plus the
   following <type>, all at once since we need to know whether we have
   a pointer to data or pointer to function to construct the right
   output syntax.  C++'s pointer syntax is hairy.  

     <type> ::= P <type>
            ::= R <type>
            ::= <pointer-to-member-type>

     <pointer-to-member-type> ::= M </class/ type> </member/ type>  */

static status_t
demangle_type_ptr (dm)
     demangling_t dm;
{
  char next;
  status_t status;

  /* Collect pointer symbols into this string.  */
  dyn_string_t symbols = dyn_string_new (10);

  DEMANGLE_TRACE ("type*", dm);

  if (symbols == NULL)
    return STATUS_ALLOCATION_FAILED;

  /* Scan forward, collecting pointers and references into symbols,
     until we hit something else.  Then emit the type.  */
  while (1)
    {
      next = peek_char (dm);
      if (next == 'P')
	{
	  if (!dyn_string_append_char (symbols, '*'))
	    return STATUS_ALLOCATION_FAILED;
	  advance_char (dm);
	}
      else if (next == 'R')
	{
	  if (!dyn_string_append_char (symbols, '&'))
	    return STATUS_ALLOCATION_FAILED;
	  advance_char (dm);
	}
      else if (next == 'M')
	{
	  /* Pointer-to-member.  */
	  dyn_string_t class_type;

	  /* Eat the 'M'.  */
	  advance_char (dm);

	  /* Capture the type of which this is a pointer-to-member.  */
	  RETURN_IF_ERROR (result_push (dm));
	  RETURN_IF_ERROR (demangle_type (dm));
	  class_type = (dyn_string_t) result_pop (dm);

	  /* Build the pointer-to-member notation.  It comes before
	     other pointer and reference qualifiers -- */
	  if (!dyn_string_prepend_cstr (symbols, "::*"))
	    return STATUS_ALLOCATION_FAILED;
	  if (!dyn_string_prepend (symbols, class_type))
	    return STATUS_ALLOCATION_FAILED;
	  dyn_string_delete (class_type);

	  if (peek_char (dm) == 'F')
	    continue;

	  /* Demangle the type of the pointed-to member.  */
	  status = demangle_type (dm);
	  /* Make it pretty.  */
	  if (STATUS_NO_ERROR (status))
	    status = result_append_space (dm);
	  /* Add the pointer-to-member syntax, and other pointer and
	     reference symbols.  */
	  if (STATUS_NO_ERROR (status))
	    status = result_append_string (dm, symbols);
	  /* Clean up.  */
	  dyn_string_delete (symbols);

	  RETURN_IF_ERROR (status);
	  return STATUS_OK;
	}
      else if (next == 'F')
	{
	  /* Ooh, tricky, a pointer-to-function.  */
	  int position = result_length (dm);
	  status = result_append_char (dm, '(');
	  if (STATUS_NO_ERROR (status))
	    status = result_append_string (dm, symbols);
	  if (STATUS_NO_ERROR (status))
	    status = result_append_char (dm, ')');
	  dyn_string_delete (symbols);
	  RETURN_IF_ERROR (status);

	  RETURN_IF_ERROR (demangle_function_type (dm, position));
	  return STATUS_OK;
	}
      else
	{
	  /* No more pointe or reference tokens.  Finish up.  */
	  status = demangle_type (dm);

	  if (STATUS_NO_ERROR (status))
	    status = result_append_string (dm, symbols);
	  dyn_string_delete (symbols);
	  RETURN_IF_ERROR (status);

	  RETURN_IF_ERROR (status);
	  return STATUS_OK;
	}
    }
}

/* Demangles and emits a <type>.  

    <type> ::= <builtin-type>
	   ::= <function-type>
	   ::= <class-enum-type>
	   ::= <array-type>
	   ::= <pointer-to-member-type>
	   ::= <template-param>
           ::= <CV-qualifiers> <type>
	   ::= P <type>   # pointer-to
	   ::= R <type>   # reference-to
	   ::= C <type>   # complex pair (C 2000)
	   ::= G <type>   # imaginary (C 2000)
	   ::= U <source-name> <type>     # vendor extended type qualifier
	   ::= <substitution>  */

static status_t
demangle_type (dm)
     demangling_t dm;
{
  int start = substitution_start (dm);
  char peek = peek_char (dm);
  char peek_next;
  int template_p = 0;
  int special_std_substitution;
  int is_builtin_type = 0;
  template_arg_list_t old_arg_list = current_template_arg_list (dm);
  int template_parm = NOT_TEMPLATE_PARM;

  DEMANGLE_TRACE ("type", dm);

  /* A <class-enum-type> can start with a digit (a <source-name>), an
     N (a <nested-name>), or a Z (a <local-name>).  */
  if (IS_DIGIT ((unsigned char) peek) || peek == 'N' || peek == 'Z')
    RETURN_IF_ERROR (demangle_class_enum_type (dm, &template_p));
  else if (peek >= 'a' && peek <= 'z')
    {
      RETURN_IF_ERROR (demangle_builtin_type (dm));
      is_builtin_type = 1;
    }
  else
    switch (peek)
      {
      case 'r':
      case 'V':
      case 'K':
	{
	  status_t status;
	  dyn_string_t cv_qualifiers = dyn_string_new (24);

	  if (cv_qualifiers == NULL)
	    return STATUS_ALLOCATION_FAILED;

	  demangle_CV_qualifiers (dm, cv_qualifiers);

	  /* If the qualifiers apply to a pointer or reference, they
	     need to come after the whole qualified type.  */
	  if (peek_char (dm) == 'P' || peek_char (dm) == 'R')
	    {
	      status = demangle_type (dm);
	      if (STATUS_NO_ERROR (status))
		status = result_append_space (dm);
	      if (STATUS_NO_ERROR (status))
		status = result_append_string (dm, cv_qualifiers);
	    }
	  /* Otherwise, the qualifiers come first.  */
	  else
	    {
	      status = result_append_string (dm, cv_qualifiers);
	      if (STATUS_NO_ERROR (status))
		status = result_append_space (dm);
	      if (STATUS_NO_ERROR (status))
		status = demangle_type (dm);
	    }

	  dyn_string_delete (cv_qualifiers);
	  RETURN_IF_ERROR (status);
	}
	break;

      case 'F':
	return "Non-pointer or -reference function type.";

      case 'A':
	RETURN_IF_ERROR (demangle_array_type (dm));
	break;

      case 'T':
	RETURN_IF_ERROR (demangle_template_param (dm, &template_parm));
	break;

      case 'S':
	/* First check if this is a special substitution.  If it is,
	   this is a <class-enum-type>.  Special substitutions have a
	   letter following the `S'; other substitutions have a digit
	   or underscore.  */
	peek_next = peek_char_next (dm);
	if (IS_DIGIT (peek_next) || peek_next == '_')
	  RETURN_IF_ERROR (demangle_substitution (dm, &template_p,
						  &special_std_substitution));
	else
	  demangle_class_enum_type (dm, &template_p);
	break;

      case 'P':
      case 'R':
      case 'M':
	RETURN_IF_ERROR (demangle_type_ptr (dm));
	break;

      case 'C':
	/* A C99 complex type.  */
	RETURN_IF_ERROR (result_append (dm, "complex "));
	advance_char (dm);
	RETURN_IF_ERROR (demangle_type (dm));
	break;

      case 'G':
	/* A C99 imaginary type.  */
	RETURN_IF_ERROR (result_append (dm, "imaginary "));
	advance_char (dm);
	RETURN_IF_ERROR (demangle_type (dm));
	break;

      case 'U':
	/* Vendor extended type qualifier.  */
	advance_char (dm);
	RETURN_IF_ERROR (demangle_source_name (dm));
	RETURN_IF_ERROR (result_append_char (dm, ' '));
	RETURN_IF_ERROR (demangle_type (dm));
	break;

      default:
	return "Unexpected character in <type>.";
      }

  /* Unqualified builin types are not substitution candidates.  */
  if (!is_builtin_type)
    /* Add a new substitution for the type. If this type was a
       <template-param>, pass its index since from the point of
       substitutions, a <template-param> token is a substitution
       candidate distinct from the type that is substituted for it.  */
    RETURN_IF_ERROR (substitution_add (dm, start, template_p, template_parm));

  /* Pop off template argument lists added during mangling of this
     type.  */
  pop_to_template_arg_list (dm, old_arg_list);

  return STATUS_OK;
}

/* C++ source names of builtin types, indexed by the mangled code
   letter's position in the alphabet ('a' -> 0, 'b' -> 1, etc).  */
static const char *const builtin_type_names[26] = 
{
  "signed char",              /* a */
  "bool",                     /* b */
  "char",                     /* c */
  "double",                   /* d */
  "long double",              /* e */
  "float",                    /* f */
  "__float128",               /* g */
  "unsigned char",            /* h */
  "int",                      /* i */
  "unsigned",                 /* j */
  NULL,                       /* k */
  "long",                     /* l */
  "unsigned long",            /* m */
  "__int128",                 /* n */
  "unsigned __int128",        /* o */
  NULL,                       /* p */
  NULL,                       /* q */
  NULL,                       /* r */
  "short",                    /* s */
  "unsigned short",           /* t */
  NULL,                       /* u */
  "void",                     /* v */
  "wchar_t",                  /* w */
  "long long",                /* x */
  "unsigned long long",       /* y */
  "..."                       /* z */
};

/* Demangles and emits a <builtin-type>.  

    <builtin-type> ::= v  # void
		   ::= w  # wchar_t
		   ::= b  # bool
		   ::= c  # char
		   ::= a  # signed char
		   ::= h  # unsigned char
		   ::= s  # short
		   ::= t  # unsigned short
		   ::= i  # int
		   ::= j  # unsigned int
		   ::= l  # long
		   ::= m  # unsigned long
		   ::= x  # long long, __int64
		   ::= y  # unsigned long long, __int64
		   ::= n  # __int128
		   ::= o  # unsigned __int128
		   ::= f  # float
		   ::= d  # double
		   ::= e  # long double, __float80
		   ::= g  # __float128
		   ::= z  # ellipsis
		   ::= u <source-name>    # vendor extended type  */

static status_t
demangle_builtin_type (dm)
     demangling_t dm;
{

  char code = peek_char (dm);

  DEMANGLE_TRACE ("builtin-type", dm);

  if (code == 'u')
    {
      advance_char (dm);
      RETURN_IF_ERROR (demangle_source_name (dm));
      return STATUS_OK;
    }
  else if (code >= 'a' && code <= 'z')
    {
      const char *type_name = builtin_type_names[code - 'a'];
      if (type_name == NULL)
	return "Unrecognized <builtin-type> code.";

      RETURN_IF_ERROR (result_append (dm, type_name));
      advance_char (dm);
      return STATUS_OK;
    }
  else
    return "Non-alphabetic <builtin-type> code.";
}

/* Demangles all consecutive CV-qualifiers (const, volatile, and
   restrict) at the current position.  The qualifiers are appended to
   QUALIFIERS.  Returns STATUS_OK.  */

static status_t
demangle_CV_qualifiers (dm, qualifiers)
     demangling_t dm;
     dyn_string_t qualifiers;
{
  DEMANGLE_TRACE ("CV-qualifiers", dm);

  while (1)
    {
      switch (peek_char (dm))
	{
	case 'r':
	  if (!dyn_string_append_space (qualifiers))
	    return STATUS_ALLOCATION_FAILED;
	  if (!dyn_string_append_cstr (qualifiers, "restrict"))
	    return STATUS_ALLOCATION_FAILED;
	  break;

	case 'V':
	  if (!dyn_string_append_space (qualifiers))
	    return STATUS_ALLOCATION_FAILED;
	  if (!dyn_string_append_cstr (qualifiers, "volatile"))
	    return STATUS_ALLOCATION_FAILED;
	  break;

	case 'K':
	  if (!dyn_string_append_space (qualifiers))
	    return STATUS_ALLOCATION_FAILED;
	  if (!dyn_string_append_cstr (qualifiers, "const"))
	    return STATUS_ALLOCATION_FAILED;
	  break;

	default:
	  return STATUS_OK;
	}

      advance_char (dm);
    }
}

/* Demangles and emits a <function-type> FUNCTION_NAME_POS is the
   position in the result string of the start of the function
   identifier, at which the function's return type will be inserted.  

    <function-type> ::= F [Y] <bare-function-type> E  */

static status_t
demangle_function_type (dm, function_name_pos)
     demangling_t dm;
     int function_name_pos;
{
  DEMANGLE_TRACE ("function-type", dm);
  RETURN_IF_ERROR (demangle_char (dm, 'F'));  
  if (peek_char (dm) == 'Y')
    {
      /* Indicate this function has C linkage if in verbose mode.  */
      if (flag_verbose)
	RETURN_IF_ERROR (result_append (dm, " [extern \"C\"] "));
      advance_char (dm);
    }
  RETURN_IF_ERROR (demangle_bare_function_type (dm, function_name_pos));
  RETURN_IF_ERROR (demangle_char (dm, 'E'));
  return STATUS_OK;
}

/* Demangles and emits a <bare-function-type>.  RETURN_TYPE_POS is the
   position in the result string at which the function return type
   should be inserted.  If RETURN_TYPE_POS is BFT_NO_RETURN_TYPE, the
   function's return type is assumed not to be encoded.  

    <bare-function-type> ::= <signature type>+  */

static status_t
demangle_bare_function_type (dm, return_type_pos)
     demangling_t dm;
     int return_type_pos;
{
  /* Sequence is the index of the current function parameter, counting
     from zero.  The value -1 denotes the return type.  */
  int sequence = 
    (return_type_pos == BFT_NO_RETURN_TYPE ? 0 : -1);

  DEMANGLE_TRACE ("bare-function-type", dm);

  RETURN_IF_ERROR (result_append_char (dm, '('));
  while (!end_of_name_p (dm) && peek_char (dm) != 'E')
    {
      if (sequence == -1)
	/* We're decoding the function's return type.  */
	{
	  dyn_string_t return_type;
	  status_t status = STATUS_OK;

	  /* Decode the return type off to the side.  */
	  RETURN_IF_ERROR (result_push (dm));
	  RETURN_IF_ERROR (demangle_type (dm));
	  return_type = (dyn_string_t) result_pop (dm);

	  /* Add a space to the end of the type.  Insert the return
             type where we've been asked to. */
	  if (!dyn_string_append_space (return_type) 
	      || !dyn_string_insert (result_string (dm), return_type_pos, 
				     return_type))
	    status = STATUS_ALLOCATION_FAILED;

	  dyn_string_delete (return_type);
	  RETURN_IF_ERROR (status);
	}
      else 
	{
	  /* Skip `void' parameter types.  One should only occur as
	     the only type in a parameter list; in that case, we want
	     to print `foo ()' instead of `foo (void)'.  */
	  if (peek_char (dm) == 'v')
	    {
	      /* Consume the v.  */
	      advance_char (dm);
	      continue;
	    }
	  /* Separate parameter types by commas.  */
	  if (sequence > 0)
	    RETURN_IF_ERROR (result_append (dm, ", "));
	  /* Demangle the type.  */
	  RETURN_IF_ERROR (demangle_type (dm));
	}

      ++sequence;
    }
  RETURN_IF_ERROR (result_append_char (dm, ')'));

  return STATUS_OK;
}

/* Demangles and emits a <class-enum-type>.  *TEMPLATE_P is set to
   non-zero if the type is a template-id, zero otherwise.  

    <class-enum-type> ::= <name>  */

static status_t
demangle_class_enum_type (dm, template_p)
     demangling_t dm;
     int *template_p;
{
  DEMANGLE_TRACE ("class-enum-type", dm);

  RETURN_IF_ERROR (demangle_name (dm, template_p));
  return STATUS_OK;
}

/* Demangles and emits an <array-type>.  

    <array-type> ::= A [<dimension number>] _ <element type>  */

static status_t
demangle_array_type (dm)
     demangling_t dm;
{
  status_t status;
  dyn_string_t array_size = dyn_string_new (10);

  if (array_size == NULL)
    return STATUS_ALLOCATION_FAILED;

  status = demangle_char (dm, 'A');

  /* Demangle the array size into array_size.  */
  if (STATUS_NO_ERROR (status))
    status = demangle_number_literally (dm, array_size, 10, 0);

  /* Demangle the base type of the array.  */
  if (STATUS_NO_ERROR (status))
    status = demangle_char (dm, '_');
  if (STATUS_NO_ERROR (status))
    status = demangle_type (dm);

  /* Emit the array dimension syntax.  */
  if (STATUS_NO_ERROR (status))
    status = result_append_char (dm, '[');
  if (STATUS_NO_ERROR (status))
    status = result_append_string (dm, array_size);
  if (STATUS_NO_ERROR (status))
    status = result_append_char (dm, ']');
  dyn_string_delete (array_size);
  
  RETURN_IF_ERROR (status);

  return STATUS_OK;
}

/* Demangles and emits a <template-param>.  The zero-indexed position
   in the parameter list is placed in *TEMPLATE_PARM_NUMBER.  

    <template-param> ::= T_       # first template parameter
                     ::= T <parameter-2 number> _  */

static status_t
demangle_template_param (dm, template_parm_number)
     demangling_t dm;
     int *template_parm_number;
{
  int parm_number;
  template_arg_list_t current_arg_list = current_template_arg_list (dm);
  string_list_t arg;

  DEMANGLE_TRACE ("template-param", dm);

  /* Make sure there is a template argmust list in which to look up
     this parameter reference.  */
  if (current_arg_list == NULL)
    return "Template parameter outside of template.";

  RETURN_IF_ERROR (demangle_char (dm, 'T'));
  if (peek_char (dm) == '_')
    parm_number = 0;
  else
    {
      RETURN_IF_ERROR (demangle_number (dm, &parm_number, 10, 0));
      ++parm_number;
    }
  RETURN_IF_ERROR (demangle_char (dm, '_'));

  arg = template_arg_list_get_arg (current_arg_list, parm_number);
  if (arg == NULL)
    /* parm_number exceeded the number of arguments in the current
       template argument list.  */
    return "Template parameter number out of bounds.";
  RETURN_IF_ERROR (result_append_string (dm, (dyn_string_t) arg));

  if (peek_char (dm) == 'I')
    RETURN_IF_ERROR (demangle_template_args (dm));

  *template_parm_number = parm_number;
  return STATUS_OK;
}

/* Demangles and emits a <template-args>.  

    <template-args> ::= I <template-arg>+ E  */

static status_t
demangle_template_args (dm)
     demangling_t dm;
{
  int first = 1;
  dyn_string_t old_last_source_name;
  template_arg_list_t arg_list = template_arg_list_new ();

  if (arg_list == NULL)
    return STATUS_ALLOCATION_FAILED;

  /* Preserve the most recently demangled source name.  */
  old_last_source_name = dm->last_source_name;
  dm->last_source_name = dyn_string_new (0);

  DEMANGLE_TRACE ("template-args", dm);

  if (dm->last_source_name == NULL)
    return STATUS_ALLOCATION_FAILED;

  RETURN_IF_ERROR (demangle_char (dm, 'I'));
  RETURN_IF_ERROR (result_append_char (dm, '<'));
  do
    {
      string_list_t arg;

      if (first)
	first = 0;
      else
	RETURN_IF_ERROR (result_append (dm, ", "));

      /* Capture the template arg.  */
      RETURN_IF_ERROR (result_push (dm));
      RETURN_IF_ERROR (demangle_template_arg (dm));
      arg = result_pop (dm);

      /* Emit it in the demangled name.  */
      RETURN_IF_ERROR (result_append_string (dm, (dyn_string_t) arg));

      /* Save it for use in expanding <template-param>s.  */
      template_arg_list_add_arg (arg_list, arg);
    }
  while (peek_char (dm) != 'E');
  /* Append the '>'.  */
  RETURN_IF_ERROR (result_close_template_list (dm));

  /* Consume the 'E'.  */
  advance_char (dm);

  /* Restore the most recent demangled source name.  */
  dyn_string_delete (dm->last_source_name);
  dm->last_source_name = old_last_source_name;

  /* Push the list onto the top of the stack of template argument
     lists, so that arguments from it are used from now on when
     expanding <template-param>s.  */
  push_template_arg_list (dm, arg_list);

  return STATUS_OK;
}

/* This function, which does not correspond to a production in the
   mangling spec, handles the `literal' production for both
   <template-arg> and <expr-primary>.  It does not expect or consume
   the initial `L' or final `E'.  The demangling is given by:

     <literal> ::= <type> </value/ number>

   and the emitted output is `(type)number'.  */

static status_t
demangle_literal (dm)
     demangling_t dm;
{
  char peek = peek_char (dm);
  dyn_string_t value_string;
  status_t status;

  DEMANGLE_TRACE ("literal", dm);

  if (!flag_verbose && peek >= 'a' && peek <= 'z')
    {
      /* If not in verbose mode and this is a builtin type, see if we
	 can produce simpler numerical output.  In particular, for
	 integer types shorter than `long', just write the number
	 without type information; for bools, write `true' or `false'.
	 Other refinements could be made here too.  */

      /* This constant string is used to map from <builtin-type> codes
	 (26 letters of the alphabet) to codes that determine how the 
	 value will be displayed.  The codes are:
	   b: display as bool
	   i: display as int
           l: display as long
	 A space means the value will be represented using cast
	 notation. */
      static const char *const code_map = "ibi    iii ll     ii  i  ";

      char code = code_map[peek - 'a'];
      /* FIXME: Implement demangling of floats and doubles.  */
      if (code == 'u')
	return STATUS_UNIMPLEMENTED;
      if (code == 'b')
	{
	  /* It's a boolean.  */
	  char value;

	  /* Consume the b.  */
	  advance_char (dm);
	  /* Look at the next character.  It should be 0 or 1,
	     corresponding to false or true, respectively.  */
	  value = peek_char (dm);
	  if (value == '0')
	    RETURN_IF_ERROR (result_append (dm, "false"));
	  else if (value == '1')
	    RETURN_IF_ERROR (result_append (dm, "true"));
	  else
	    return "Unrecognized bool constant.";
	  /* Consume the 0 or 1.  */
	  advance_char (dm);
	  return STATUS_OK;
	}
      else if (code == 'i' || code == 'l')
	{
	  /* It's an integer or long.  */

	  /* Consume the type character.  */
	  advance_char (dm);

	  /* Demangle the number and write it out.  */
	  value_string = dyn_string_new (0);
	  status = demangle_number_literally (dm, value_string, 10, 1);
	  if (STATUS_NO_ERROR (status))
	    status = result_append_string (dm, value_string);
	  /* For long integers, append an l.  */
	  if (code == 'l' && STATUS_NO_ERROR (status))
	    status = result_append_char (dm, code);
	  dyn_string_delete (value_string);

	  RETURN_IF_ERROR (status);
	  return STATUS_OK;
	}
      /* ...else code == ' ', so fall through to represent this
	 literal's type explicitly using cast syntax.  */
    }

  RETURN_IF_ERROR (result_append_char (dm, '('));
  RETURN_IF_ERROR (demangle_type (dm));
  RETURN_IF_ERROR (result_append_char (dm, ')'));

  value_string = dyn_string_new (0);
  if (value_string == NULL)
    return STATUS_ALLOCATION_FAILED;

  status = demangle_number_literally (dm, value_string, 10, 1);
  if (STATUS_NO_ERROR (status))
    status = result_append_string (dm, value_string);
  dyn_string_delete (value_string);
  RETURN_IF_ERROR (status);

  return STATUS_OK;
}

/* Demangles and emits a <template-arg>.  

    <template-arg> ::= <type>                     # type
                   ::= L <type> <value number> E  # literal
                   ::= LZ <encoding> E            # external name
                   ::= X <expression> E           # expression  */

static status_t
demangle_template_arg (dm)
     demangling_t dm;
{
  DEMANGLE_TRACE ("template-arg", dm);

  switch (peek_char (dm))
    {
    case 'L':
      advance_char (dm);

      if (peek_char (dm) == 'Z')
	{
	  /* External name.  */
	  advance_char (dm);
	  /* FIXME: Standard is contradictory here.  */
	  RETURN_IF_ERROR (demangle_encoding (dm));
	}
      else
	RETURN_IF_ERROR (demangle_literal (dm));
      RETURN_IF_ERROR (demangle_char (dm, 'E'));
      break;

    case 'X':
      /* Expression.  */
      advance_char (dm);
      RETURN_IF_ERROR (demangle_expression (dm));
      break;

    default:
      RETURN_IF_ERROR (demangle_type (dm));
      break;
    }

  return STATUS_OK;
}

/* Demangles and emits an <expression>.

    <expression> ::= <unary operator-name> <expression>
		 ::= <binary operator-name> <expression> <expression>
		 ::= <expr-primary>  
                 ::= <scope-expression>  */

static status_t
demangle_expression (dm)
     demangling_t dm;
{
  char peek = peek_char (dm);

  DEMANGLE_TRACE ("expression", dm);

  if (peek == 'L' || peek == 'T')
    RETURN_IF_ERROR (demangle_expr_primary (dm));
  else if (peek == 's' && peek_char_next (dm) == 'r')
    RETURN_IF_ERROR (demangle_scope_expression (dm));
  else
    /* An operator expression.  */
    {
      int num_args;
      status_t status = STATUS_OK;
      dyn_string_t operator_name;

      /* We have an operator name.  Since we want to output binary
	 operations in infix notation, capture the operator name
	 first.  */
      RETURN_IF_ERROR (result_push (dm));
      RETURN_IF_ERROR (demangle_operator_name (dm, 1, &num_args));
      operator_name = (dyn_string_t) result_pop (dm);

      /* If it's binary, do an operand first.  */
      if (num_args > 1)
	{
	  status = result_append_char (dm, '(');
	  if (STATUS_NO_ERROR (status))
	    status = demangle_expression (dm);
	  if (STATUS_NO_ERROR (status))
	    status = result_append_char (dm, ')');
	}

      /* Emit the operator.  */  
      if (STATUS_NO_ERROR (status))
	status = result_append_string (dm, operator_name);
      dyn_string_delete (operator_name);
      RETURN_IF_ERROR (status);
      
      /* Emit its second (if binary) or only (if unary) operand.  */
      RETURN_IF_ERROR (result_append_char (dm, '('));
      RETURN_IF_ERROR (demangle_expression (dm));
      RETURN_IF_ERROR (result_append_char (dm, ')'));

      /* The ternary operator takes a third operand.  */
      if (num_args == 3)
	{
	  RETURN_IF_ERROR (result_append (dm, ":("));
	  RETURN_IF_ERROR (demangle_expression (dm));
	  RETURN_IF_ERROR (result_append_char (dm, ')'));
	}
    }

  return STATUS_OK;
}

/* Demangles and emits a <scope-expression>.  

    <scope-expression> ::= sr <qualifying type> <source-name>
                       ::= sr <qualifying type> <encoding>  */

static status_t
demangle_scope_expression (dm)
     demangling_t dm;
{
  RETURN_IF_ERROR (demangle_char (dm, 's'));
  RETURN_IF_ERROR (demangle_char (dm, 'r'));
  RETURN_IF_ERROR (demangle_type (dm));
  RETURN_IF_ERROR (result_append (dm, "::"));
  RETURN_IF_ERROR (demangle_encoding (dm));
  return STATUS_OK;
}

/* Demangles and emits an <expr-primary>.  

    <expr-primary> ::= <template-param>
		   ::= L <type> <value number> E  # literal
		   ::= L <mangled-name> E         # external name  */

static status_t
demangle_expr_primary (dm)
     demangling_t dm;
{
  char peek = peek_char (dm);
  int unused;

  DEMANGLE_TRACE ("expr-primary", dm);

  if (peek == 'T')
    RETURN_IF_ERROR (demangle_template_param (dm, &unused));
  else if (peek == 'L')
    {
      /* Consume the `L'.  */
      advance_char (dm);
      peek = peek_char (dm);

      if (peek == '_')
	RETURN_IF_ERROR (demangle_mangled_name (dm));
      else
	RETURN_IF_ERROR (demangle_literal (dm));

      RETURN_IF_ERROR (demangle_char (dm, 'E'));
    }
  else
    return STATUS_ERROR;

  return STATUS_OK;
}

/* Demangles and emits a <substitution>.  Sets *TEMPLATE_P to non-zero
   if the substitution is the name of a template, zero otherwise.  If
   the substitution token is St, which corresponds to the `::std::'
   namespace and can appear in a non-nested name, sets
   *SPECIAL_STD_SUBSTITUTION to non-zero; zero otherwise.  

     <substitution> ::= S <seq-id> _
                    ::= S_

                    ::= St   # ::std::
                    ::= Sa   # ::std::allocator
                    ::= Sb   # ::std::basic_string
                    ::= Ss   # ::std::basic_string<char,
				    		   ::std::char_traits<char>,
						   ::std::allocator<char> >
                    ::= Si   # ::std::basic_istream<char,  
                                                    std::char_traits<char> >
                    ::= So   # ::std::basic_ostream<char,  
                                                    std::char_traits<char> >
                    ::= Sd   # ::std::basic_iostream<char, 
                                                     std::char_traits<char> >
*/

static status_t
demangle_substitution (dm, template_p, special_std_substitution)
     demangling_t dm;
     int *template_p;
     int *special_std_substitution;
{
  int seq_id;
  int peek;
  dyn_string_t text;

  DEMANGLE_TRACE ("substitution", dm);

  RETURN_IF_ERROR (demangle_char (dm, 'S'));
  *special_std_substitution = 0;

  /* Scan the substitution sequence index.  A missing number denotes
     the first index.  */
  peek = peek_char (dm);
  if (peek == '_')
    seq_id = -1;
  /* If the following character is 0-9 or a capital letter, interpret
     the sequence up to the next underscore as a base-36 substitution
     index.  */
  else if (IS_DIGIT ((unsigned char) peek) 
	   || (peek >= 'A' && peek <= 'Z'))
    RETURN_IF_ERROR (demangle_number (dm, &seq_id, 36, 0));
  else 
    {
      const char *new_last_source_name = NULL;

      switch (peek)
	{
	case 't':
	  RETURN_IF_ERROR (result_append (dm, "std"));
	  *special_std_substitution = 1;
	  break;

	case 'a':
	  RETURN_IF_ERROR (result_append (dm, "std::allocator"));
	  new_last_source_name = "allocator";
	  *template_p = 1;
	  break;

	case 'b':
	  RETURN_IF_ERROR (result_append (dm, "std::basic_string"));
	  new_last_source_name = "basic_string";
	  *template_p = 1;
	  break;
	  
	case 's':
	  if (!flag_verbose)
	    {
	      RETURN_IF_ERROR (result_append (dm, "std::string"));
	      new_last_source_name = "string";
	    }
	  else
	    {
	      RETURN_IF_ERROR (result_append (dm, "std::basic_string<char, std::char_traits<char>, std::allocator<char> >"));
	      new_last_source_name = "basic_string";
	    }
	  *template_p = 0;
	  break;

	case 'i':
	  if (!flag_verbose)
	    {
	      RETURN_IF_ERROR (result_append (dm, "std::istream"));
	      new_last_source_name = "istream";
	    }
	  else
	    {
	      RETURN_IF_ERROR (result_append (dm, "std::basic_istream<char, std::char_traints<char> >"));
	      new_last_source_name = "basic_istream";
	    }
	  *template_p = 0;
	  break;

	case 'o':
	  if (!flag_verbose)
	    {
	      RETURN_IF_ERROR (result_append (dm, "std::ostream"));
	      new_last_source_name = "ostream";
	    }
	  else
	    {
	      RETURN_IF_ERROR (result_append (dm, "std::basic_ostream<char, std::char_traits<char> >"));
	      new_last_source_name = "basic_ostream";
	    }
	  *template_p = 0;
	  break;

	case 'd':
	  if (!flag_verbose) 
	    {
	      RETURN_IF_ERROR (result_append (dm, "std::iostream"));
	      new_last_source_name = "iostream";
	    }
	  else
	    {
	      RETURN_IF_ERROR (result_append (dm, "std::basic_iostream<char, std::char_traits<char> >"));
	      new_last_source_name = "basic_iostream";
	    }
	  *template_p = 0;
	  break;

	default:
	  return "Unrecognized <substitution>.";
	}
      
      /* Consume the character we just processed.  */
      advance_char (dm);

      if (new_last_source_name != NULL)
	{
	  if (!dyn_string_copy_cstr (dm->last_source_name, 
				     new_last_source_name))
	    return STATUS_ALLOCATION_FAILED;
	}

      return STATUS_OK;
    }

  /* Look up the substitution text.  Since `S_' is the most recent
     substitution, `S0_' is the second-most-recent, etc., shift the
     numbering by one.  */
  text = substitution_get (dm, seq_id + 1, template_p);
  if (text == NULL) 
    return "Substitution number out of range.";

  /* Emit the substitution text.  */
  RETURN_IF_ERROR (result_append_string (dm, text));

  RETURN_IF_ERROR (demangle_char (dm, '_'));
  return STATUS_OK;
}

/* Demangles and emits a <local-name>.  

    <local-name> := Z <function encoding> E <entity name> [<discriminator>]
                 := Z <function encoding> E s [<discriminator>]  */

static status_t
demangle_local_name (dm)
     demangling_t dm;
{
  DEMANGLE_TRACE ("local-name", dm);

  RETURN_IF_ERROR (demangle_char (dm, 'Z'));
  RETURN_IF_ERROR (demangle_encoding (dm));
  RETURN_IF_ERROR (demangle_char (dm, 'E'));
  RETURN_IF_ERROR (result_append (dm, "'s "));

  if (peek_char (dm) == 's')
    {
      /* Local character string literal.  */
      RETURN_IF_ERROR (result_append (dm, "string literal"));
      /* Consume the s.  */
      advance_char (dm);
      RETURN_IF_ERROR (demangle_discriminator (dm, 0));
    }
  else
    {
      int unused;
      RETURN_IF_ERROR (result_append (dm, "local "));
      /* Local name for some other entity.  Demangle its name.  */
      RETURN_IF_ERROR (demangle_name (dm, &unused));
      RETURN_IF_ERROR (demangle_discriminator (dm, 1));
     }

   return STATUS_OK;
 }

 /* Optimonally demangles and emits a <discriminator>.  If there is no
    <discriminator> at the current position in the mangled string, the
    descriminator is assumed to be zero.  Emit the discriminator number
    in parentheses, unless SUPPRESS_FIRST is non-zero and the
    discriminator is zero.  

     <discriminator> ::= _ <number>  */

static status_t
demangle_discriminator (dm, suppress_first)
     demangling_t dm;
     int suppress_first;
{
  /* Output for <discriminator>s to the demangled name is completely
     supressed if not in verbose mode.  */

  if (peek_char (dm) == '_')
    {
      /* Consume the underscore.  */
      advance_char (dm);
      if (flag_verbose)
	RETURN_IF_ERROR (result_append (dm, " [#"));
      /* Check if there's a number following the underscore.  */
      if (IS_DIGIT ((unsigned char) peek_char (dm)))
	{
	  int discriminator;
	  /* Demangle the number.  */
	  RETURN_IF_ERROR (demangle_number (dm, &discriminator, 10, 0));
	  if (flag_verbose)
	    /* Write the discriminator.  The mangled number is two
	       less than the discriminator ordinal, counting from
	       zero.  */
	    RETURN_IF_ERROR (int_to_dyn_string (discriminator + 2, 
						(dyn_string_t) dm->result));
	}
      else
	{
	  if (flag_verbose)
	    /* A missing digit correspond to one.  */
	    RETURN_IF_ERROR (result_append_char (dm, '1'));
	}
      if (flag_verbose)
	RETURN_IF_ERROR (result_append_char (dm, ']'));
    }
  else if (!suppress_first)
    {
      if (flag_verbose)
	RETURN_IF_ERROR (result_append (dm, " [#0]"));
    }

  return STATUS_OK;
}

/* Demangle NAME into RESULT, which must be an initialized
   dyn_string_t.  On success, returns STATUS_OK.  On failure, returns
   an error message, and the contents of RESULT are unchanged.  */

static status_t
cp_demangle (name, result)
     const char *name;
     dyn_string_t result;
{
  status_t status;
  int length = strlen (name);

  if (length > 2 && name[0] == '_' && name[1] == 'Z')
    {
      demangling_t dm = demangling_new (name);
      if (dm == NULL)
	return STATUS_ALLOCATION_FAILED;

      status = result_push (dm);
      if (status != STATUS_OK)
	{
	  demangling_delete (dm);
	  return status;
	}

      status = demangle_mangled_name (dm);
      if (STATUS_NO_ERROR (status))
	{
	  dyn_string_t demangled = (dyn_string_t) result_pop (dm);
	  if (!dyn_string_copy (result, demangled))
	    return STATUS_ALLOCATION_FAILED;
	  dyn_string_delete (demangled);
	}
      
      demangling_delete (dm);
    }
  else
    {
      /* It's evidently not a mangled C++ name.  It could be the name
	 of something with C linkage, though, so just copy NAME into
	 RESULT.  */
      if (!dyn_string_copy_cstr (result, name))
	return STATUS_ALLOCATION_FAILED;
      status = STATUS_OK;
    }

  return status; 
}

/* Demangle TYPE_NAME into RESULT, which must be an initialized
   dyn_string_t.  On success, returns STATUS_OK.  On failiure, returns
   an error message, and the contents of RESULT are unchanged.  */

static status_t
cp_demangle_type (type_name, result)
     const char* type_name;
     dyn_string_t result;
{
  status_t status;
  demangling_t dm = demangling_new (type_name);
  
  if (dm == NULL)
    return STATUS_ALLOCATION_FAILED;

  /* Demangle the type name.  The demangled name is stored in dm.  */
  status = result_push (dm);
  if (status != STATUS_OK)
    {
      demangling_delete (dm);
      return status;
    }

  status = demangle_type (dm);

  if (STATUS_NO_ERROR (status))
    {
      /* The demangling succeeded.  Pop the result out of dm and copy
	 it into RESULT.  */
      dyn_string_t demangled = (dyn_string_t) result_pop (dm);
      if (!dyn_string_copy (result, demangled))
	return STATUS_ALLOCATION_FAILED;
      dyn_string_delete (demangled);
    }

  /* Clean up.  */
  demangling_delete (dm);

  return status;
}


#ifdef IN_LIBGCC2

extern char *__cxa_demangle PARAMS ((const char *, char *, size_t *, int *));

/* ABI-mandated entry point in the C++ runtime library for performing
   demangling.  MANGLED_NAME is a NUL-terminated character string
   containing the name to be demangled.  

   OUTPUT_BUFFER is a region of memory, allocated with malloc, of
   *LENGTH bytes, into which the demangled name is stored.  If
   OUTPUT_BUFFER is not long enough, it is expanded using realloc.
   OUTPUT_BUFFER may instead be NULL; in that case, the demangled name
   is placed in a region of memory allocated with malloc.  

   If LENGTH is non-NULL, the length of the buffer conaining the
   demangled name, is placed in *LENGTH.  

   The return value is a pointer to the start of the NUL-terminated
   demangled name, or NULL if the demangling fails.  The caller is
   responsible for deallocating this memory using free.  

   *STATUS is set to one of the following values:
      0: The demangling operation succeeded.
     -1: A memory allocation failiure occurred.
     -2: MANGLED_NAME is not a valid name under the C++ ABI mangling rules.
     -3: One of the arguments is invalid.

   The demagling is performed using the C++ ABI mangling rules, with
   GNU extensions.  */

char *
__cxa_demangle (mangled_name, output_buffer, length, status)
     const char *mangled_name;
     char *output_buffer;
     size_t *length;
     int *status;
{
  struct dyn_string demangled_name;
  status_t result;

  if (status == NULL)
    return NULL;

  if (mangled_name == NULL) {
    *status = -3;
    return NULL;
  }

  /* Did the caller provide a buffer for the demangled name?  */
  if (output_buffer == NULL) {
    /* No; dyn_string will malloc a buffer for us.  */
    if (!dyn_string_init (&demangled_name, 0)) 
      {
	*status = -1;
	return NULL;
      }
  }
  else {
    /* Yes.  Check that the length was provided.  */
    if (length == NULL) {
      *status = -3;
      return NULL;
    }
    /* Install the buffer into a dyn_string.  */
    demangled_name.allocated = *length;
    demangled_name.length = 0;
    demangled_name.s = output_buffer;
  }

  if (mangled_name[0] == '_' && mangled_name[1] == 'Z')
    /* MANGLED_NAME apprears to be a function or variable name.
       Demangle it accordingly.  */
    result = cp_demangle (mangled_name, &demangled_name);
  else
    /* Try to demangled MANGLED_NAME as the name of a type.  */
    result = cp_demangle_type (mangled_name, &demangled_name);

  if (result == STATUS_OK) 
    /* The demangling succeeded.  */
    {
      /* If LENGTH isn't NULL, store the allocated buffer length
	 there; the buffer may have been realloced by dyn_string
	 functions.  */
      if (length != NULL)
	*length = demangled_name.allocated;
      /* The operation was a success.  */
      *status = 0;
      return dyn_string_buf (&demangled_name);
    }
  else if (result == STATUS_ALLOCATION_FAILED)
    /* A call to malloc or realloc failed during the demangling
       operation.  */
    {
      *status = -1;
      return NULL;
    }
  else
    /* The demangling failed for another reason, most probably because
       MANGLED_NAME isn't a valid mangled name.  */
    {
      /* If the buffer containing the demangled name wasn't provided
	 by the caller, free it.  */
      if (output_buffer == NULL)
	free (dyn_string_buf (&demangled_name));
      *status = -2;
      return NULL;
    }
}

#else /* !IN_LIBGCC2 */

/* Variant entry point for integration with the existing cplus-dem
   demangler.  Attempts to demangle MANGLED.  If the demangling
   succeeds, returns a buffer, allocated with malloc, containing the
   demangled name.  The caller must deallocate the buffer using free.
   If the demangling failes, returns NULL.  */

char *
cplus_demangle_new_abi (mangled)
     const char* mangled;
{
  /* Create a dyn_string to hold the demangled name.  */
  dyn_string_t demangled = dyn_string_new (0);
  /* Attempt the demangling.  */
  status_t status = cp_demangle ((char *) mangled, demangled);
  if (STATUS_NO_ERROR (status))
    /* Demangling succeeded.  */
    {
      /* Grab the demangled result from the dyn_string.  It was
	 allocated with malloc, so we can return it directly.  */
      char *return_value = dyn_string_release (demangled);
      /* Hand back the demangled name.  */
      return return_value;
    }
  else if (status == STATUS_ALLOCATION_FAILED)
    {
      fprintf (stderr, "Memory allocation failed.\n");
      abort ();
    }
  else
    /* Demangling failed.  */
    {
      dyn_string_delete (demangled);
      return NULL;
    }
}

#endif /* IN_LIBGCC2 */

#ifdef STANDALONE_DEMANGLER

#include "getopt.h"

static void print_usage
  PARAMS ((FILE* fp, int exit_value));

/* Non-zero if CHAR is a character than can occur in a mangled name.  */
#define is_mangled_char(CHAR)                                           \
  (IS_ALPHA (CHAR) || IS_DIGIT (CHAR) || (CHAR) == '_')

/* The name of this program, as invoked.  */
const char* program_name;

/* Prints usage summary to FP and then exits with EXIT_VALUE.  */

static void
print_usage (fp, exit_value)
     FILE* fp;
     int exit_value;
{
  fprintf (fp, "Usage: %s [options] [names ...]\n", program_name);
  fprintf (fp, "Options:\n", program_name);
  fprintf (fp, "  -h,--help       Display this message.\n");
  fprintf (fp, "  -s,--strict     Demangle standard names only.\n");
  fprintf (fp, "  -v,--verbose    Produce verbose demanglings.\n");
  fprintf (fp, "If names are provided, they are demangled.  Otherwise filters standard input.\n");

  exit (exit_value);
}

/* Option specification for getopt_long.  */
static struct option long_options[] = 
{
  { "help",    no_argument, NULL, 'h' },
  { "strict",  no_argument, NULL, 's' },
  { "verbose", no_argument, NULL, 'v' },
  { NULL,      no_argument, NULL, 0   },
};

/* Main entry for a demangling filter executable.  It will demangle
   its command line arguments, if any.  If none are provided, it will
   filter stdin to stdout, replacing any recognized mangled C++ names
   with their demangled equivalents.  */

int
main (argc, argv)
     int argc;
     char *argv[];
{
  status_t status;
  int i;
  int opt_char;

  /* Use the program name of this program, as invoked.  */
  program_name = argv[0];

  /* Parse options.  */
  do 
    {
      opt_char = getopt_long (argc, argv, "hsv", long_options, NULL);
      switch (opt_char)
	{
	case '?':  /* Unrecognized option.  */
	  print_usage (stderr, 1);
	  break;

	case 'h':
	  print_usage (stdout, 0);
	  break;

	case 's':
	  flag_strict = 1;
	  break;

	case 'v':
	  flag_verbose = 1;
	  break;
	}
    }
  while (opt_char != -1);

  if (optind == argc) 
    /* No command line arguments were provided.  Filter stdin.  */
    {
      dyn_string_t mangled = dyn_string_new (3);
      dyn_string_t demangled = dyn_string_new (0);
      status_t status;

      /* Read all of input.  */
      while (!feof (stdin))
	{
	  char c = getchar ();

	  /* The first character of a mangled name is an underscore.  */
	  if (feof (stdin))
	    break;
	  if (c != '_')
	    {
	      /* It's not a mangled name.  Print the character and go
		 on.  */
	      putchar (c);
	      continue;
	    }
	  c = getchar ();
	  
	  /* The second character of a mangled name is a capital `Z'.  */
	  if (feof (stdin))
	    break;
	  if (c != 'Z')
	    {
	      /* It's not a mangled name.  Print the previous
		 underscore, the `Z', and go on.  */
	      putchar ('_');
	      putchar (c);
	      continue;
	    }

	  /* Start keeping track of the candidate mangled name.  */
	  dyn_string_append_char (mangled, '_');
	  dyn_string_append_char (mangled, 'Z');

	  /* Pile characters into mangled until we hit one that can't
	     occur in a mangled name.  */
	  c = getchar ();
	  while (!feof (stdin) && is_mangled_char (c))
	    {
	      dyn_string_append_char (mangled, c);
	      if (feof (stdin))
		break;
	      c = getchar ();
	    }

	  /* Attempt to demangle the name.  */
	  status = cp_demangle (dyn_string_buf (mangled), demangled);

	  /* If the demangling succeeded, great!  Print out the
	     demangled version.  */
	  if (STATUS_NO_ERROR (status))
	    fputs (dyn_string_buf (demangled), stdout);
	  /* Abort on allocation failures.  */
	  else if (status == STATUS_ALLOCATION_FAILED)
	    {
	      fprintf (stderr, "Memory allocation failed.\n");
	      abort ();
	    }
	  /* Otherwise, it might not have been a mangled name.  Just
	     print out the original text.  */
	  else
	    fputs (dyn_string_buf (mangled), stdout);

	  /* If we haven't hit EOF yet, we've read one character that
	     can't occur in a mangled name, so print it out.  */
	  if (!feof (stdin))
	    putchar (c);

	  /* Clear the candidate mangled name, to start afresh next
	     time we hit a `_Z'.  */
	  dyn_string_clear (mangled);
	}

      dyn_string_delete (mangled);
      dyn_string_delete (demangled);
    }
  else
    /* Demangle command line arguments.  */
    {
      dyn_string_t result = dyn_string_new (0);

      /* Loop over command line arguments.  */
      for (i = optind; i < argc; ++i)
	{
	  /* Attempt to demangle.  */
	  status = cp_demangle (argv[i], result);

	  /* If it worked, print the demangled name.  */
	  if (STATUS_NO_ERROR (status))
	    printf ("%s\n", dyn_string_buf (result));
	  /* Abort on allocaiton failures.  */
	  else if (status == STATUS_ALLOCATION_FAILED)
	    {
	      fprintf (stderr, "Memory allocaiton failed.\n");
	      abort ();
	    }
	  /* If not, print the error message to stderr instead.  */
	  else 
	    fprintf (stderr, "%s\n", status);
	}
      dyn_string_delete (result);
    }

  return 0;
}

#endif /* STANDALONE_DEMANGLER */
