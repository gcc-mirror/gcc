/*

    TREELANG Compiler back end interface (treetree.c)
    Called by the parser.

    If you want a working example of how to write a front end to GCC,
    you are in the right place.

    Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
    1999, 2000, 2001, 2002, 2003, Free Software Foundation, Inc.

    This code is based on toy.c written by Richard Kenner.

    It was later modified by Jonathan Bartlett whose changes have all
    been removed (by Tim Josling).

    Various bits and pieces were cloned from the GCC main tree, as
    GCC evolved, for COBOLForGCC, by Tim Josling.

    It was adapted to TREELANG by Tim Josling 2001.

    ---------------------------------------------------------------------------

    This program is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 2, or (at your option) any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, 59 Temple Place - Suite 330,
    Boston, MA 02111-1307, USA.

    In other words, you are welcome to use, share and improve this program.
    You are forbidden to forbid anyone else to use, share and improve
    what you give them.   Help stamp out software-hoarding!

    ---------------------------------------------------------------------------

 */

/*
  Assumption: garbage collection is never called implicitly.  It will
  not be called 'at any time' when short of memory.  It will only be
  called explicitly at the end of each function.  This removes the
  need for a *lot* of bother to ensure everything is in the mark trees
  at all times.  */

  /* Note it is OK to use GCC extensions such as long long in a compiler front end.
     This is because the GCC front ends are built using GCC. */

/* GCC headers.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "output.h"
#include "rtl.h"
#include "ggc.h"
#include "toplev.h"
#include "varray.h"
#include "langhooks-def.h"
#include "langhooks.h"
#include "target.h"

#include "treelang.h"
#include "treetree.h"
#include "opts.h"

extern int option_main;
extern char **file_names;

/* Types expected by gcc's garbage collector.
   These types exist to allow language front-ends to
   add extra information in gcc's parse tree data structure.
   But the treelang front end doesn't use them -- it has
   its own parse tree data structure.
   We define them here only to satisfy gcc's garbage collector.  */

/* Language-specific identifier information.  */

struct lang_identifier GTY(())
{
  struct tree_identifier common;
};

/* Language-specific tree node information.  */

union lang_tree_node 
  GTY((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE")))
{
  union tree_node GTY ((tag ("0"), 
			desc ("tree_node_structure (&%h)"))) 
    generic;
  struct lang_identifier GTY ((tag ("1"))) identifier;
};

/* Language-specific type information.  */

struct lang_type GTY(())
{
  char junk; /* dummy field to ensure struct is not empty */
};

/* Language-specific declaration information.  */

struct lang_decl GTY(())
{
  char junk; /* dummy field to ensure struct is not empty */
};

struct language_function GTY(())
{
  char junk; /* dummy field to ensure struct is not empty */
};

static tree tree_lang_truthvalue_conversion (tree expr);
static bool tree_mark_addressable (tree exp);
static tree tree_lang_type_for_size (unsigned precision, int unsignedp);
static tree tree_lang_type_for_mode (enum machine_mode mode, int unsignedp);
static tree tree_lang_unsigned_type (tree type_node);
static tree tree_lang_signed_type (tree type_node);
static tree tree_lang_signed_or_unsigned_type (int unsignedp, tree type);

/* XXX these should be static */
void pushlevel (int ignore);
tree poplevel (int keep, int reverse, int functionbody);
int global_bindings_p (void);
void insert_block (tree block);
void set_block (tree block);
tree pushdecl (tree decl);
tree getdecls (void);
int kept_level_p (void);

static void tree_push_type_decl (tree id, tree type_node);
static void tree_push_atomic_type_decl (tree id, tree type_node);

/* The front end language hooks (addresses of code for this front
   end).  These are not really very language-dependent, i.e.
   treelang, C, Mercury, etc. can all use almost the same definitions.  */

#undef LANG_HOOKS_TRUTHVALUE_CONVERSION
#define LANG_HOOKS_TRUTHVALUE_CONVERSION tree_lang_truthvalue_conversion
#undef LANG_HOOKS_MARK_ADDRESSABLE
#define LANG_HOOKS_MARK_ADDRESSABLE tree_mark_addressable
#undef LANG_HOOKS_SIGNED_TYPE
#define LANG_HOOKS_SIGNED_TYPE tree_lang_signed_type
#undef LANG_HOOKS_UNSIGNED_TYPE
#define LANG_HOOKS_UNSIGNED_TYPE tree_lang_unsigned_type
#undef LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE
#define LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE tree_lang_signed_or_unsigned_type
#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE tree_lang_type_for_mode
#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE tree_lang_type_for_size
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE treelang_parse_file

/* Hook routines and data unique to treelang.  */

#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT treelang_init
#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME	"GNU treelang"
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH		treelang_finish
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS  treelang_init_options
#undef LANG_HOOKS_HANDLE_OPTION
#define LANG_HOOKS_HANDLE_OPTION treelang_handle_option
const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* Tree code type/name/code tables.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

const char tree_code_type[] = {
#include "tree.def"
  'x'
};
#undef DEFTREECODE

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

const unsigned char tree_code_length[] = {
#include "tree.def"
  0
};
#undef DEFTREECODE

#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

const char *const tree_code_name[] = {
#include "tree.def"
  "@@dummy"
};
#undef DEFTREECODE

/* Number of bits in int and char - accessed by front end.  */

unsigned int tree_code_int_size = SIZEOF_INT * HOST_BITS_PER_CHAR;

unsigned int tree_code_char_size = HOST_BITS_PER_CHAR;

/* Return the tree stuff for this type TYPE_NUM.  */

tree
tree_code_get_type (int type_num)
{
  switch (type_num)
    {
    case SIGNED_CHAR:
      return signed_char_type_node;

    case UNSIGNED_CHAR:
      return unsigned_char_type_node;

    case SIGNED_INT:
      return integer_type_node;

    case UNSIGNED_INT:
      return unsigned_type_node;

    case VOID_TYPE:
      return void_type_node;

    default:
      abort ();
    }
}

/* Output the code for the start of an if statement.  The test
   expression is EXP (true if not zero), and the stmt occurred at line
   LINENO in file FILENAME.  */

void
tree_code_if_start (tree exp, location_t loc)
{
  tree cond_exp;
  cond_exp = build (NE_EXPR,
                 TREE_TYPE (exp),
                 exp,
                 build1 (CONVERT_EXPR, TREE_TYPE (exp), integer_zero_node));
  emit_line_note (loc); /* Output the line number information.  */
  expand_start_cond (cond_exp, /* Exit-able if nonzero.  */ 0);
}

/* Output the code for the else of an if statement.  The else occurred
   at line LINENO in file FILENAME.  */

void
tree_code_if_else (location_t loc)
{
  emit_line_note (loc); /* Output the line number information.  */
  expand_start_else ();
}

/* Output the code for the end_if an if statement.  The end_if (final brace) occurred
   at line LINENO in file FILENAME.  */

void
tree_code_if_end (location_t loc)
{
  emit_line_note (loc); /* Output the line number information.  */
  expand_end_cond ();
}

/* Create a function.  The prototype name is NAME, storage class is
   STORAGE_CLASS, type of return variable is RET_TYPE, parameter lists
   is PARMS, returns decl for this function.  */

tree
tree_code_create_function_prototype (unsigned char* chars,
				     unsigned int storage_class,
				     unsigned int ret_type,
				     struct prod_token_parm_item* parms,
				     location_t loc)
{

  tree id;
  struct prod_token_parm_item* parm;
  tree type_list = NULL_TREE;
  tree type_node;
  tree fn_type;
  tree fn_decl;

  /* Build the type.  */
  id = get_identifier ((const char*)chars);
  for (parm = parms; parm; parm = parm->tp.par.next)
    {
      if (parm->category != parameter_category)
        abort ();
      type_node = get_type_for_numeric_type (parm->type);
      type_list = tree_cons (NULL_TREE, type_node, type_list);
    }
  /* Last parm if void indicates fixed length list (as opposed to
     printf style va_* list).  */
  type_list = tree_cons (NULL_TREE, void_type_node, type_list);
  /* The back end needs them in reverse order.  */
  type_list = nreverse (type_list);

  type_node = get_type_for_numeric_type (ret_type);
  fn_type = build_function_type (type_node, type_list);

  id = get_identifier ((const char*)chars);
  fn_decl = build_decl (FUNCTION_DECL, id, fn_type);

  DECL_CONTEXT (fn_decl) = NULL_TREE; /* Nested functions not supported here.  */
  DECL_SOURCE_LOCATION (fn_decl) = loc;

  TREE_USED (fn_decl) = 1;

  /* Real name (optional).  */
  SET_DECL_ASSEMBLER_NAME (fn_decl, DECL_NAME (fn_decl));

  TREE_PUBLIC (fn_decl) = 0;
  DECL_EXTERNAL (fn_decl) = 0;
  TREE_STATIC (fn_decl) = 0;
  switch (storage_class)
    {
    case STATIC_STORAGE:
      TREE_PUBLIC (fn_decl) = 0;
      break;

    case EXTERNAL_DEFINITION_STORAGE:
      TREE_PUBLIC (fn_decl) = 1;
      TREE_STATIC (fn_decl) = 0;
      DECL_EXTERNAL (fn_decl) = 0;
      break;

    case EXTERNAL_REFERENCE_STORAGE:
      TREE_PUBLIC (fn_decl) = 0;
      DECL_EXTERNAL (fn_decl) = 1;
      break;


    case AUTOMATIC_STORAGE:
    default:
      abort ();
    }

  /* Process declaration of function defined elsewhere.  */
  rest_of_decl_compilation (fn_decl, NULL, 1, 0);

  return fn_decl;
}


/* Output code for start of function; the decl of the function is in
    PREV_SAVED (as created by tree_code_create_function_prototype),
    the function is at line number LINENO in file FILENAME.  The
    parameter details are in the lists PARMS. Returns nothing.  */
void
tree_code_create_function_initial (tree prev_saved,
				   location_t loc,
				   struct prod_token_parm_item* parms)
{
  tree fn_decl;
  tree param_decl;
  tree next_param;
  tree first_param;
  tree parm_decl;
  tree parm_list;
  tree resultdecl;
  struct prod_token_parm_item* this_parm;
  struct prod_token_parm_item* parm;

  fn_decl = prev_saved;
  if (!fn_decl)
    abort ();

  /* Output message if not -quiet.  */
  announce_function (fn_decl);

  /* This has something to do with forcing output also.  */
  pushdecl (fn_decl);

  /* Set current function for error msgs etc.  */
  current_function_decl = fn_decl;
  DECL_INITIAL (fn_decl) = error_mark_node;

  DECL_SOURCE_LOCATION (fn_decl) = loc;

  /* Prepare creation of rtl for a new function.  */

  resultdecl = DECL_RESULT (fn_decl) 
    = build_decl (RESULT_DECL, NULL_TREE, TREE_TYPE (TREE_TYPE (fn_decl)));
  DECL_CONTEXT (DECL_RESULT (fn_decl)) = fn_decl;
  DECL_SOURCE_LOCATION (resultdecl) = loc;

  /* Work out the size. ??? is this needed.  */
  layout_decl (DECL_RESULT (fn_decl), 0);

  /* Make the argument variable decls.  */
  parm_list = NULL_TREE;
  for (parm = parms; parm; parm = parm->tp.par.next)
    {
      parm_decl = build_decl (PARM_DECL, get_identifier
                              ((const char*) (parm->tp.par.variable_name)),
                              get_type_for_numeric_type (parm->type));

      /* Some languages have different nominal and real types.  */
      DECL_ARG_TYPE (parm_decl) = TREE_TYPE (parm_decl);
      if (!DECL_ARG_TYPE (parm_decl))
        abort ();
      if (!fn_decl)
        abort ();
      DECL_CONTEXT (parm_decl) = fn_decl;
      DECL_SOURCE_LOCATION (parm_decl) = loc;
      parm_list = chainon (parm_decl, parm_list);
    }

  /* Back into reverse order as the back end likes them.  */
  parm_list = nreverse (parm_list);

  DECL_ARGUMENTS (fn_decl) = parm_list;

  /* Save the decls for use when the args are referred to.  */
  for (param_decl = DECL_ARGUMENTS (fn_decl),
         this_parm = parms;
       param_decl;
       param_decl = TREE_CHAIN (param_decl),
         this_parm = this_parm->tp.par.next)
    {
      if (!this_parm)
        abort (); /* Too few.  */
      *this_parm->tp.par.where_to_put_var_tree = param_decl;
    }
  if (this_parm)
    abort (); /* Too many.  */

  /* Output the decl rtl (not the rtl for the function code).  ???.
     If the function is not defined in this file, when should you
     execute this?  */
  make_decl_rtl (fn_decl, NULL);

  init_function_start (fn_decl);

  /* Create rtl for startup code of function, such as saving registers.  */

  expand_function_start (fn_decl, 0);

  /* Function.c requires a push at the start of the function. that
     looks like a bug to me but let's make it happy.  */

  (*lang_hooks.decls.pushlevel) (0);

  /* Create rtl for the start of a new scope.  */

  expand_start_bindings (2);

  /* Put the parameters into the symbol table.  */

  for (first_param = param_decl = nreverse (DECL_ARGUMENTS (fn_decl));
       param_decl;
       param_decl = next_param)
    {
      next_param = TREE_CHAIN (param_decl);
      TREE_CHAIN (param_decl) = NULL;
      /* layout_decl (param_decl, 0);  Already done in build_decl tej 13/4/2002.  */
      pushdecl (param_decl);
      if (DECL_CONTEXT (param_decl) != current_function_decl)
        abort ();
    }

  /* Store back the PARM_DECL nodes.  They appear in the right order.  */
  DECL_ARGUMENTS (fn_decl) = getdecls ();

  /* Force it to be output, else may be solely inlined.  */
  TREE_ADDRESSABLE (fn_decl) = 1;

  /* Stop -O3 from deleting it.  */
  TREE_USED (fn_decl) = 1;

  /* Add a new level to the debugger symbol table.  */

  (*lang_hooks.decls.pushlevel) (0);

  /* Create rtl for the start of a new scope.  */

  expand_start_bindings (0);

  emit_line_note (loc); /* Output the line number information.  */
}

/* Wrapup a function contained in file FILENAME, ending at line LINENO.  */
void
tree_code_create_function_wrapup (location_t loc)
{
  tree block;
  tree fn_decl;

  fn_decl = current_function_decl;

  emit_line_note (loc); /* Output the line number information.  */

  /* Get completely built level from debugger symbol table.  */

  block = (*lang_hooks.decls.poplevel) (1, 0, 0);

  /* Emit rtl for end of scope.  */

  expand_end_bindings (block, 0, 1);

  /* Emit rtl for end of function.  */

  expand_function_end ();

  /* Pop the level.  */

  block = (*lang_hooks.decls.poplevel) (1, 0, 1);

  /* And attach it to the function.  */

  DECL_INITIAL (fn_decl) = block;

  /* Emit rtl for end of scope.  */

  expand_end_bindings (block, 0, 1);

  /* Call optimization and convert optimized rtl to assembly code.  */

  rest_of_compilation (fn_decl);

  /* We are not inside of any scope now.  */

  current_function_decl = NULL_TREE;
}

/*
   Create a variable.

   The storage class is STORAGE_CLASS (eg LOCAL).
   The name is CHARS/LENGTH.
   The type is EXPRESSION_TYPE (eg UNSIGNED_TYPE).
   The init tree is INIT.
*/

tree
tree_code_create_variable (unsigned int storage_class,
			   unsigned char* chars,
			   unsigned int length,
			   unsigned int expression_type,
			   tree init,
			   location_t loc)
{
  tree var_type;
  tree var_id;
  tree var_decl;

  /* 1. Build the type.  */
  var_type = get_type_for_numeric_type (expression_type);

  /* 2. Build the name.  */
  if (chars[length] != 0)
    abort (); /* Should be null terminated.  */

  var_id = get_identifier ((const char*)chars);

  /* 3. Build the decl and set up init.  */
  var_decl = build_decl (VAR_DECL, var_id, var_type);

  /* 3a. Initialization.  */
  if (init)
    DECL_INITIAL (var_decl) = build1 (CONVERT_EXPR, var_type, init);
  else
    DECL_INITIAL (var_decl) = NULL_TREE;

  /* 4. Compute size etc.  */
  layout_decl (var_decl, 0);

  if (TYPE_SIZE (var_type) == 0)
    abort (); /* Did not calculate size.  */

  DECL_CONTEXT (var_decl) = current_function_decl;

  DECL_SOURCE_LOCATION (var_decl) = loc;

  /* Set the storage mode and whether only visible in the same file.  */
  switch (storage_class)
    {
    case STATIC_STORAGE:
      TREE_STATIC (var_decl) = 1;
      TREE_PUBLIC (var_decl) = 0;
      break;

    case AUTOMATIC_STORAGE:
      TREE_STATIC (var_decl) = 0;
      TREE_PUBLIC (var_decl) = 0;
      break;

    case EXTERNAL_DEFINITION_STORAGE:
      TREE_STATIC (var_decl) = 0;
      TREE_PUBLIC (var_decl) = 1;
      break;

    case EXTERNAL_REFERENCE_STORAGE:
      DECL_EXTERNAL (var_decl) = 1;
      TREE_PUBLIC (var_decl) = 0;
      break;

    default:
      abort ();
    }

  /* This should really only be set if the variable is used.  */
  TREE_USED (var_decl) = 1;

  /* Expand declaration and initial value if any.  */

  if (TREE_STATIC (var_decl))
    rest_of_decl_compilation (var_decl, 0, 0, 0);
  else
    {
      expand_decl (var_decl);
      if (DECL_INITIAL (var_decl))
        expand_decl_init (var_decl);
    }

  return pushdecl (copy_node (var_decl));

}


/* Generate code for return statement.  Type is in TYPE, expression
   is in EXP if present.  */

void
tree_code_generate_return (tree type, tree exp)
{
  tree setret;
  tree param;

  for (param = DECL_ARGUMENTS (current_function_decl);
       param;
       param = TREE_CHAIN (param))
    {
      if (DECL_CONTEXT (param) != current_function_decl)
        abort ();
    }

  if (exp)
    {
      setret = build (MODIFY_EXPR, type, DECL_RESULT (current_function_decl),
                     build1 (CONVERT_EXPR, type, exp));
      TREE_SIDE_EFFECTS (setret) = 1;
      TREE_USED (setret) = 1;
      expand_expr_stmt (setret);
    }
  expand_return (DECL_RESULT (current_function_decl));
}

/* Output the code for this expression statement CODE.  */


void
tree_code_output_expression_statement (tree code, location_t loc)
{
  /* Output the line number information.  */
  emit_line_note (loc);
  TREE_USED (code) = 1;
  TREE_SIDE_EFFECTS (code) = 1;
  expand_expr_stmt (code);
}

/* Return a tree for a constant integer value in the token TOK.  No
   size checking is done.  */

tree
tree_code_get_integer_value (unsigned char* chars, unsigned int length)
{
  long long int val = 0;
  unsigned int ix;
  unsigned int start = 0;
  int negative = 1;
  switch (chars[0])
    {
    case (unsigned char)'-':
      negative = -1;
      start = 1;
      break;

    case (unsigned char)'+':
      start = 1;
      break;

    default:
      break;
    }
  for (ix = start; ix < length; ix++)
    val = val * 10 + chars[ix] - (unsigned char)'0';
  val = val*negative;
  return build_int_2 (val & 0xffffffff, (val >> 32) & 0xffffffff);
}

/* Return the tree for an expresssion, type EXP_TYPE (see treetree.h)
   with tree type TYPE and with operands1 OP1, OP2 (maybe), OP3 (maybe).  */
tree
tree_code_get_expression (unsigned int exp_type,
                          tree type, tree op1, tree op2,
			  tree op3 ATTRIBUTE_UNUSED)
{
  tree ret1;
  int operator;

  switch (exp_type)
    {
    case EXP_ASSIGN:
      if (!op1 || !op2)
        abort ();
      operator = MODIFY_EXPR;
      ret1 = build (operator, type,
                 op1,
                 build1 (CONVERT_EXPR, type, op2));

      break;

    case EXP_PLUS:
      operator = PLUS_EXPR;
      goto binary_expression;

    case EXP_MINUS:
      operator = MINUS_EXPR;
      goto binary_expression;

    case EXP_EQUALS:
      operator = EQ_EXPR;
      goto binary_expression;

      /* Expand a binary expression.  Ensure the operands are the right type.  */
    binary_expression:
      if (!op1 || !op2)
        abort ();
      ret1  =  build (operator, type,
                   build1 (CONVERT_EXPR, type, op1),
                   build1 (CONVERT_EXPR, type, op2));
      break;

      /* Reference to a variable.  This is dead easy, just return the
         decl for the variable.  If the TYPE is different than the
         variable type, convert it.  */
    case EXP_REFERENCE:
      if (!op1)
        abort ();
      if (type == TREE_TYPE (op1))
        ret1 = op1;
      else
        ret1 = build1 (CONVERT_EXPR, type, op1);
      break;

    case EXP_FUNCTION_INVOCATION:
      if (!op1 || !op2)
        abort ();
      {
        tree fun_ptr;
        fun_ptr = build1 (ADDR_EXPR, build_pointer_type (type), op1);
        ret1 = build (CALL_EXPR, type, fun_ptr, nreverse (op2));
      }
      break;

    default:
      abort ();
    }

  return ret1;
}

/* Init parameter list and return empty list.  */

tree
tree_code_init_parameters (void)
{
  return NULL_TREE;
}

/* Add a parameter EXP whose expression type is EXP_PROTO to list
   LIST, returning the new list.  */

tree
tree_code_add_parameter (tree list, tree proto_exp, tree exp)
{
  tree new_exp;
  new_exp = tree_cons (NULL_TREE,
                    build1 (CONVERT_EXPR, TREE_TYPE (proto_exp), exp),
                    NULL_TREE);
  if (!list)
    return new_exp;
  return chainon (new_exp, list);
}

/* Get the tree type for this type whose number is NUMERIC_TYPE.  */

tree
get_type_for_numeric_type (unsigned int numeric_type)
{

  int size1;
  int sign1;
  switch (numeric_type)
    {
    case VOID_TYPE:
      return void_type_node;

    case SIGNED_INT:
      size1 = tree_code_int_size;
      sign1 = 1;
      break;

    case UNSIGNED_INT:
      size1 = tree_code_int_size;
      sign1 = 0;
      break;

    case SIGNED_CHAR:
      size1 = tree_code_char_size;
      sign1 = 1;
      break;

    case UNSIGNED_CHAR:
      size1 = tree_code_char_size;
      sign1 = 0;
      break;

    default:
      abort ();
    }

  return tree_code_get_numeric_type (size1, sign1);

}

/* Return tree representing a numeric type of size SIZE1 bits and
   signed if SIGN1 !=  0.  */
tree
tree_code_get_numeric_type (unsigned int size1, unsigned int sign1)
{
  tree ret1;
  if (!size1)
    abort ();
  if (size1 == tree_code_int_size)
    {
      if (sign1)
        ret1 = integer_type_node;
      else
        ret1 = unsigned_type_node;
    }
  else
    if (size1 == tree_code_char_size)
      {
        if (sign1)
          ret1 = signed_char_type_node;
        else
          ret1 = unsigned_char_type_node;
      }
    else
      abort ();

  return ret1;
}

/* Get a stringpool entry for a string S of length L.  This is needed
   because the GTY routines don't mark strings, forcing you to put
   them into stringpool, which is never freed.  */

const char*
get_string (const char *s, size_t l)
{
  tree t;
  t = get_identifier_with_length (s, l);
  return IDENTIFIER_POINTER(t);
}
  
/* Save typing debug_tree all the time. Dump a tree T pretty and
   concise.  */

void dt (tree t);

void
dt (tree t)
{
  debug_tree (t);
}

/* Routines Expected by gcc:  */

/* These are used to build types for various sizes.  The code below
   is a simplified version of that of GNAT.  */

#ifndef MAX_BITS_PER_WORD
#define MAX_BITS_PER_WORD  BITS_PER_WORD
#endif

/* This variable keeps a table for types for each precision so that we only 
   allocate each of them once. Signed and unsigned types are kept separate.  */
static GTY(()) tree signed_and_unsigned_types[MAX_BITS_PER_WORD + 1][2];

/* XXX is this definition OK? */
static tree
tree_lang_truthvalue_conversion (tree expr)
{
  return expr;
}

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Value is 1 if successful.  
   
   This implementation was copied from c-decl.c. */

static bool
tree_mark_addressable (tree exp)
{
  register tree x = exp;
  while (1)
    switch (TREE_CODE (x))
      {
      case COMPONENT_REF:
      case ADDR_EXPR:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	x = TREE_OPERAND (x, 0);
	break;
  
      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return 1;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x)
	    && DECL_NONLOCAL (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		error ("global register variable `%s' used in nested function",
		       IDENTIFIER_POINTER (DECL_NAME (x)));
		return 0;
	      }
	    pedwarn ("register variable `%s' used in nested function",
		     IDENTIFIER_POINTER (DECL_NAME (x)));
	  }
	else if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		error ("address of global register variable `%s' requested",
		       IDENTIFIER_POINTER (DECL_NAME (x)));
		return 0;
	      }

	    pedwarn ("address of register variable `%s' requested",
		     IDENTIFIER_POINTER (DECL_NAME (x)));
	  }
	put_var_into_stack (x, /*rescan=*/ true);

	/* drops in */
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;

      default:
	return 1;
    }
}
  
/* Return an integer type with the number of bits of precision given by  
   PRECISION.  UNSIGNEDP is nonzero if the type is unsigned; otherwise
   it is a signed type.  */
  
static tree
tree_lang_type_for_size (unsigned precision, int unsignedp)
{
  tree t;

  if (precision <= MAX_BITS_PER_WORD
      && signed_and_unsigned_types[precision][unsignedp] != 0)
    return signed_and_unsigned_types[precision][unsignedp];

  if (unsignedp)
    t = signed_and_unsigned_types[precision][1]
      = make_unsigned_type (precision);
  else
    t = signed_and_unsigned_types[precision][0]
      = make_signed_type (precision);
  
  return t;
}

/* Return a data type that has machine mode MODE.  UNSIGNEDP selects
   an unsigned type; otherwise a signed type is returned.  */

static tree
tree_lang_type_for_mode (enum machine_mode mode, int unsignedp)
{
  return tree_lang_type_for_size (GET_MODE_BITSIZE (mode), unsignedp);
}

/* Return the unsigned version of a TYPE_NODE, a scalar type.  */

static tree
tree_lang_unsigned_type (tree type_node)
{
  return tree_lang_type_for_size (TYPE_PRECISION (type_node), 1);
}

/* Return the signed version of a TYPE_NODE, a scalar type.  */

static tree
tree_lang_signed_type (tree type_node)
{
  return tree_lang_type_for_size (TYPE_PRECISION (type_node), 0);
}

/* Return a type the same as TYPE except unsigned or signed according to
   UNSIGNEDP.  */

static tree
tree_lang_signed_or_unsigned_type (int unsignedp, tree type)
{
  if (! INTEGRAL_TYPE_P (type) || TREE_UNSIGNED (type) == unsignedp)
    return type;
  else
    return tree_lang_type_for_size (TYPE_PRECISION (type), unsignedp);
}

/* These functions and variables deal with binding contours.  We only
   need these functions for the list of PARM_DECLs, but we leave the
   functions more general; these are a simplified version of the
   functions from GNAT.  */

/* For each binding contour we allocate a binding_level structure which records
   the entities defined or declared in that contour. Contours include:

	the global one
	one for each subprogram definition
	one for each compound statement (declare block)

   Binding contours are used to create GCC tree BLOCK nodes.  */

struct binding_level
{
  /* A chain of ..._DECL nodes for all variables, constants, functions,
     parameters and type declarations.  These ..._DECL nodes are chained
     through the TREE_CHAIN field. Note that these ..._DECL nodes are stored
     in the reverse of the order supplied to be compatible with the
     back-end.  */
  tree names;
  /* For each level (except the global one), a chain of BLOCK nodes for all
     the levels that were entered and exited one level down from this one.  */
  tree blocks;
  /* The back end may need, for its own internal processing, to create a BLOCK
     node. This field is set aside for this purpose. If this field is non-null
     when the level is popped, i.e. when poplevel is invoked, we will use such
     block instead of creating a new one from the 'names' field, that is the
     ..._DECL nodes accumulated so far.  Typically the routine 'pushlevel'
     will be called before setting this field, so that if the front-end had
     inserted ..._DECL nodes in the current block they will not be lost.   */
  tree block_created_by_back_end;
  /* The binding level containing this one (the enclosing binding level). */
  struct binding_level *level_chain;
};

/* The binding level currently in effect.  */
static struct binding_level *current_binding_level = NULL;

/* The outermost binding level. This binding level is created when the
   compiler is started and it will exist through the entire compilation.  */
static struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */
static struct binding_level clear_binding_level = {NULL, NULL, NULL, NULL};

/* Return non-zero if we are currently in the global binding level.  */

int
global_bindings_p (void)
{
  return current_binding_level == global_binding_level ? -1 : 0;
}

/* Return the list of declarations in the current level. Note that this list
   is in reverse order (it has to be so for back-end compatibility).  */

tree
getdecls (void)
{
  return current_binding_level->names;
}

/* Nonzero if the current level needs to have a BLOCK made.  */

int
kept_level_p (void)
{
  return (current_binding_level->names != 0);
}

/* Enter a new binding level. The input parameter is ignored, but has to be
   specified for back-end compatibility.  */

void
pushlevel (int ignore ATTRIBUTE_UNUSED)
{
  struct binding_level *newlevel = xmalloc (sizeof (struct binding_level));

  *newlevel = clear_binding_level;

  /* Add this level to the front of the chain (stack) of levels that are
     active.  */
  newlevel->level_chain = current_binding_level;
  current_binding_level = newlevel;
}

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP is nonzero, this level had explicit declarations, so
   and create a "block" (a BLOCK node) for the level
   to record its declarations and subblocks for symbol table output.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the BLOCK.  */

tree
poplevel (int keep, int reverse, int functionbody)
{
  /* Points to a BLOCK tree node. This is the BLOCK node construted for the
     binding level that we are about to exit and which is returned by this
     routine.  */
  tree block_node = NULL_TREE;
  tree decl_chain;
  tree subblock_chain = current_binding_level->blocks;
  tree subblock_node;
  tree block_created_by_back_end;

  /* Reverse the list of *_DECL nodes if desired.  Note that the ..._DECL
     nodes chained through the `names' field of current_binding_level are in
     reverse order except for PARM_DECL node, which are explicitely stored in
     the right order.  */
  decl_chain = (reverse) ? nreverse (current_binding_level->names)
			 : current_binding_level->names;

  block_created_by_back_end = current_binding_level->block_created_by_back_end;
  if (block_created_by_back_end != 0)
    {
      block_node = block_created_by_back_end;

      /* Check if we are about to discard some information that was gathered
	 by the front-end. Nameley check if the back-end created a new block 
	 without calling pushlevel first. To understand why things are lost
	 just look at the next case (i.e. no block created by back-end.  */
      if ((keep || functionbody) && (decl_chain || subblock_chain))
	abort ();
    }

  /* If there were any declarations in the current binding level, or if this
     binding level is a function body, or if there are any nested blocks then
     create a BLOCK node to record them for the life of this function.  */
  else if (keep || functionbody)
    block_node = build_block (keep ? decl_chain : 0, 0, subblock_chain, 0, 0);

  /* Record the BLOCK node just built as the subblock its enclosing scope.  */
  for (subblock_node = subblock_chain; subblock_node;
       subblock_node = TREE_CHAIN (subblock_node))
    BLOCK_SUPERCONTEXT (subblock_node) = block_node;

  /* Clear out the meanings of the local variables of this level.  */

  for (subblock_node = decl_chain; subblock_node;
       subblock_node = TREE_CHAIN (subblock_node))
    if (DECL_NAME (subblock_node) != 0)
      /* If the identifier was used or addressed via a local extern decl,  
	 don't forget that fact.   */
      if (DECL_EXTERNAL (subblock_node))
	{
	  if (TREE_USED (subblock_node))
	    TREE_USED (DECL_NAME (subblock_node)) = 1;
	  if (TREE_ADDRESSABLE (subblock_node))
	    TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (subblock_node)) = 1;
	}

  /* Pop the current level.  */
  current_binding_level = current_binding_level->level_chain;

  if (functionbody)
    {
      /* This is the top level block of a function. The ..._DECL chain stored
	 in BLOCK_VARS are the function's parameters (PARM_DECL nodes). Don't
	 leave them in the BLOCK because they are found in the FUNCTION_DECL
	 instead.  */
      DECL_INITIAL (current_function_decl) = block_node;
      BLOCK_VARS (block_node) = 0;
    }
  else if (block_node)
    {
      if (block_created_by_back_end == NULL)
	current_binding_level->blocks
	  = chainon (current_binding_level->blocks, block_node);
    }

  /* If we did not make a block for the level just exited, any blocks made for
     inner levels (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks of something
     else.  */
  else if (subblock_chain)
    current_binding_level->blocks
      = chainon (current_binding_level->blocks, subblock_chain);
  if (block_node)
    TREE_USED (block_node) = 1;

  return block_node;
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */

void
insert_block (tree block)
{
  TREE_USED (block) = 1;
  current_binding_level->blocks
    = chainon (current_binding_level->blocks, block);
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */

void
set_block (tree block)
{
  current_binding_level->block_created_by_back_end = block;
}

/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node. */

tree
pushdecl (tree decl)
{
  /* External objects aren't nested, other objects may be.  */
    
  if ((DECL_EXTERNAL (decl)) || (decl==current_function_decl))
    DECL_CONTEXT (decl) = 0;
  else
    DECL_CONTEXT (decl) = current_function_decl;

  /* Put the declaration on the list.  The list of declarations is in reverse
     order. The list will be reversed later if necessary.  This needs to be
     this way for compatibility with the back-end.  */

  TREE_CHAIN (decl) = current_binding_level->names;
  current_binding_level->names = decl;

  /* For the declartion of a type, set its name if it is not already set. */

  if (TREE_CODE (decl) == TYPE_DECL
      && TYPE_NAME (TREE_TYPE (decl)) == 0)
    TYPE_NAME (TREE_TYPE (decl)) = DECL_NAME (decl);

  return decl;
}


static void
tree_push_type_decl(tree id, tree type_node)
{
  tree decl = build_decl (TYPE_DECL, id, type_node);
  TYPE_NAME (type_node) = decl;
  TYPE_STUB_DECL (type_node) = decl;
  pushdecl (decl);
}

/* push_atomic_type_decl() ensures that the type's type is itself. 
   Needed for DBX.  Must only be used for atomic types,
   not for e.g. pointer or array types.  */

static void
tree_push_atomic_type_decl(tree id, tree type_node)
{
  TREE_TYPE (type_node) = type_node;
  tree_push_type_decl (id, type_node);
}

#define NULL_BINDING_LEVEL (struct binding_level *) NULL                        

/* Create the predefined scalar types of C,
   and some nodes representing standard constants (0, 1, (void *) 0).
   Initialize the global binding level.
   Make definitions for built-in primitive functions.  */

void
treelang_init_decl_processing (void)
{
  current_function_decl = NULL;
  current_binding_level = NULL_BINDING_LEVEL;
  pushlevel (0);	/* make the binding_level structure for global names */
  global_binding_level = current_binding_level;

  build_common_tree_nodes (flag_signed_char);

  /* set standard type names */

  /* Define `int' and `char' first so that dbx will output them first.  */

  tree_push_atomic_type_decl (get_identifier ("int"), integer_type_node);
  tree_push_atomic_type_decl (get_identifier ("char"), char_type_node);
  tree_push_atomic_type_decl (get_identifier ("long int"),
			      long_integer_type_node);
  tree_push_atomic_type_decl (get_identifier ("unsigned int"),
			      unsigned_type_node);
  tree_push_atomic_type_decl (get_identifier ("long unsigned int"),
			      long_unsigned_type_node);
  tree_push_atomic_type_decl (get_identifier ("long long int"),
			      long_long_integer_type_node);
  tree_push_atomic_type_decl (get_identifier ("long long unsigned int"),
			      long_long_unsigned_type_node);
  tree_push_atomic_type_decl (get_identifier ("short int"),
			      short_integer_type_node);
  tree_push_atomic_type_decl (get_identifier ("short unsigned int"),
			      short_unsigned_type_node);
  tree_push_atomic_type_decl (get_identifier ("signed char"),
			      signed_char_type_node);
  tree_push_atomic_type_decl (get_identifier ("unsigned char"),
			      unsigned_char_type_node);
  tree_push_atomic_type_decl (NULL_TREE, intQI_type_node);
  tree_push_atomic_type_decl (NULL_TREE, intHI_type_node);
  tree_push_atomic_type_decl (NULL_TREE, intSI_type_node);
  tree_push_atomic_type_decl (NULL_TREE, intDI_type_node);
#if HOST_BITS_PER_WIDE_INT >= 64
  tree_push_atomic_type_decl (NULL_TREE, intTI_type_node);
#endif
  tree_push_atomic_type_decl (NULL_TREE, unsigned_intQI_type_node);
  tree_push_atomic_type_decl (NULL_TREE, unsigned_intHI_type_node);
  tree_push_atomic_type_decl (NULL_TREE, unsigned_intSI_type_node);
  tree_push_atomic_type_decl (NULL_TREE, unsigned_intDI_type_node);
#if HOST_BITS_PER_WIDE_INT >= 64
  tree_push_atomic_type_decl (NULL_TREE, unsigned_intTI_type_node);
#endif
  
  size_type_node = make_unsigned_type (POINTER_SIZE);
  tree_push_atomic_type_decl (get_identifier ("size_t"), size_type_node);
  set_sizetype (size_type_node);

  build_common_tree_nodes_2 (/* short_double= */ 0);

  tree_push_atomic_type_decl (get_identifier ("float"), float_type_node);
  tree_push_atomic_type_decl (get_identifier ("double"), double_type_node);
  tree_push_atomic_type_decl (get_identifier ("long double"), long_double_type_node);
  tree_push_atomic_type_decl (get_identifier ("void"), void_type_node);

  /* Add any target-specific builtin functions.  */
  (*targetm.init_builtins) ();

  pedantic_lvalues = pedantic;
}

/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  If
   ATTRS is nonzero, use that for the function's attribute list.

   copied from gcc/c-decl.c
*/

tree
builtin_function (const char *name, tree type, int function_code,
		  enum built_in_class class, const char *library_name,
		  tree attrs)
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  if (library_name)
    SET_DECL_ASSEMBLER_NAME (decl, get_identifier (library_name));
  make_decl_rtl (decl, NULL);
  pushdecl (decl);
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = function_code;

  /* Possibly apply some default attributes to this built-in function.  */
  if (attrs)
    decl_attributes (&decl, attrs, ATTR_FLAG_BUILT_IN);
  else
    decl_attributes (&decl, NULL_TREE, 0);

  return decl;
}

#include "debug.h" /* for debug_hooks, needed by gt-treelang-treetree.h */
#include "gt-treelang-treetree.h"
