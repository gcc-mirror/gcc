/* 

    TREELANG Compiler back end interface (treetree.c)
    Called by the parser.

    If you want a working example of how to write a front end to GCC,
    you are in the right place.

    Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
    2001, 2002 Free Software Foundation, Inc.

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

/* Standard/OS headers.  */

#include <stdlib.h>
#include <unistd.h>
#include "safe-ctype.h"
#include <errno.h>
#include <stdarg.h>
#include <limits.h>
#include <string.h>

#include <fcntl.h>
#include <getopt.h>
#include <stdio.h>

/* GCC headers.  */

#include "config.h"
#include "ansidecl.h"
#include "system.h"
#include "tree.h"
#include "flags.h"
#include "output.h"
#include "c-tree.h"
#include "rtl.h"
#include "ggc.h"
#include "toplev.h"
#include "varray.h"
#include "langhooks-def.h"
#include "langhooks.h"

#include "treelang.h"
#include "treetree.h"

extern int option_main;
extern char **file_names;

/* The front end language hooks (addresses of code for this front
   end).  Mostly just use the C routines.  */

#undef LANG_HOOKS_TRUTHVALUE_CONVERSION
#define LANG_HOOKS_TRUTHVALUE_CONVERSION c_common_truthvalue_conversion
#undef LANG_HOOKS_MARK_ADDRESSABLE
#define LANG_HOOKS_MARK_ADDRESSABLE c_mark_addressable
#undef LANG_HOOKS_SIGNED_TYPE
#define LANG_HOOKS_SIGNED_TYPE c_common_signed_type
#undef LANG_HOOKS_UNSIGNED_TYPE
#define LANG_HOOKS_UNSIGNED_TYPE c_common_unsigned_type
#undef LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE
#define LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE c_common_signed_or_unsigned_type
#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE c_common_type_for_mode
#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE c_common_type_for_size
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE treelang_parse_file
#undef LANG_HOOKS_COMMON_ATTRIBUTE_TABLE
#define LANG_HOOKS_COMMON_ATTRIBUTE_TABLE c_common_attribute_table
#undef LANG_HOOKS_FORMAT_ATTRIBUTE_TABLE
#define LANG_HOOKS_FORMAT_ATTRIBUTE_TABLE c_common_format_attribute_table
#undef LANG_HOOKS_INSERT_DEFAULT_ATTRIBUTES
#define LANG_HOOKS_INSERT_DEFAULT_ATTRIBUTES c_insert_default_attributes

/* Hook routines and data unique to treelang.  */

#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT treelang_init
#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME	"GNU treelang"
#undef LANG_HOOKS_FINISH 
#define LANG_HOOKS_FINISH		treelang_finish
#undef LANG_HOOKS_DECODE_OPTION	
#define LANG_HOOKS_DECODE_OPTION treelang_decode_option
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

unsigned int tree_code_int_size = 0;
unsigned int tree_code_char_size = 0;

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
tree_code_if_start (tree exp, unsigned char* filename, int lineno)
{
  tree cond_exp;
  cond_exp = build (NE_EXPR, 
                 TREE_TYPE (exp), 
                 exp, 
                 build1 (CONVERT_EXPR, TREE_TYPE (exp), integer_zero_node));
  emit_line_note ((const char *)filename, lineno); /* Output the line number information.  */
  expand_start_cond (cond_exp, /* Exit-able if nonzero.  */ 0);
}

/* Output the code for the else of an if statement.  The else occurred
   at line LINENO in file FILENAME.  */

void 
tree_code_if_else (unsigned char* filename, int lineno)
{
  emit_line_note ((const char *)filename, lineno); /* Output the line number information.  */
  expand_start_else ();
}

/* Output the code for the end_if an if statement.  The end_if (final brace) occurred
   at line LINENO in file FILENAME.  */

void 
tree_code_if_end (unsigned char* filename, int lineno)
{
  emit_line_note ((const char *)filename, lineno); /* Output the line number information.  */
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
                                    unsigned char* filename,
                                    int lineno)
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
  DECL_SOURCE_FILE (fn_decl) = (const char *)filename;
 /*  if (lineno > 1000000)
    ; */ /* Probably the line # is rubbish because someone forgot to set
    the line number - and unfortunately impossible line #s are used as
    magic flags at various times. The longest known function for
    example is about 550,000 lines (it was written in COBOL).  */
  DECL_SOURCE_LINE (fn_decl) = lineno;

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
                                  unsigned char* filename,
                                  int lineno,
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

  DECL_SOURCE_FILE (fn_decl) = (const char *)filename;
  DECL_SOURCE_LINE (fn_decl) = lineno;

  /* Prepare creation of rtl for a new function.  */
  
  resultdecl = DECL_RESULT (fn_decl) = build_decl (RESULT_DECL, NULL_TREE, TREE_TYPE (TREE_TYPE (fn_decl)));
  DECL_CONTEXT (DECL_RESULT (fn_decl)) = fn_decl;
  DECL_SOURCE_FILE (resultdecl) = (const char *)filename;
  DECL_SOURCE_LINE (resultdecl) = lineno;
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
      DECL_SOURCE_FILE (parm_decl) = (const char *)filename;
      DECL_SOURCE_LINE (parm_decl) = lineno;
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

  /* Use filename/lineno from above.  */
  init_function_start (fn_decl, (const char *)filename, lineno); 
  
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
  
  emit_line_note ((const char *)filename, lineno); /* Output the line number information.  */
}

/* Wrapup a function contained in file FILENAME, ending at line LINENO.  */
void 
tree_code_create_function_wrapup (unsigned char* filename,
                                 int lineno)
{
  tree block;
  tree fn_decl;

  fn_decl = current_function_decl;
  
  emit_line_note ((const char *)filename, lineno); /* Output the line number information.  */

  /* Get completely built level from debugger symbol table.  */
  
  block = (*lang_hooks.decls.poplevel) (1, 0, 0);
  
  /* Emit rtl for end of scope.  */
  
  expand_end_bindings (block, 0, 1);
  
  /* Emit rtl for end of function.  */
  
  expand_function_end ((const char *)filename, lineno, 0);
  
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
                               unsigned char* filename,
                               int lineno)
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

  DECL_SOURCE_FILE (var_decl) = (const char *)filename;
  DECL_SOURCE_LINE (var_decl) = lineno;

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
tree_code_output_expression_statement (tree code, 
                                       unsigned char* filename, int lineno)
{
  /* Output the line number information.  */
  emit_line_note ((const char *)filename, lineno); 
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
                          tree type, tree op1, tree op2, tree op3 ATTRIBUTE_UNUSED)
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

/* Garbage Collection.  */

/* Callback to mark storage M as used always.  */

void
tree_ggc_storage_always_used (void * m)
{
  void **mm; /* Actually M is a pointer to a pointer to the memory.  */
  mm = (void**)m;

  if (*mm)
    ggc_mark (*mm);
} 

/* Following  from c-lang.c.  */

/* Used by c-typeck.c (build_external_ref), but only for objc.  */

tree
lookup_objc_ivar (tree id ATTRIBUTE_UNUSED)
{
  return 0;
}

/* Dummy routines called from c code. Save copying c-decl.c, c-common.c etc.  */

tree
objc_is_id (tree arg ATTRIBUTE_UNUSED)
{
  return 0;
}

void
check_function_format (int *status ATTRIBUTE_UNUSED,
                       tree attrs ATTRIBUTE_UNUSED,
                       tree params ATTRIBUTE_UNUSED)
{
  return;
}

/* Tell the c code we are not objective C.  */

int
objc_comptypes (tree lhs ATTRIBUTE_UNUSED, 
                tree rhs ATTRIBUTE_UNUSED, 
                int reflexive ATTRIBUTE_UNUSED)
{
  return 0;
}

/* Should not be called for treelang.   */

tree
build_stmt VPARAMS ((enum tree_code code  ATTRIBUTE_UNUSED, ...))
{
  abort ();
}

/* Should not be called for treelang.   */

tree
add_stmt (tree t ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

tree
build_return_stmt (tree expr ATTRIBUTE_UNUSED)
{
  abort ();
}

/* C warning, ignore.  */

void
pedwarn_c99 VPARAMS ((const char *msgid ATTRIBUTE_UNUSED, ...))
{
  return;
}

/* Should not be called for treelang.   */

tree
build_case_label (tree low_value ATTRIBUTE_UNUSED,
                  tree high_value ATTRIBUTE_UNUSED,
                  tree label_decl ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

void
emit_local_var (tree decl ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

void
expand_stmt (tree t ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

cpp_reader *
cpp_create_reader (enum c_lang lang ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

const char *
init_c_lex (const char *filename ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

void init_pragma (void);

void
init_pragma ()
{
  abort ();
}

/* Should not be called for treelang.   */

int
cpp_finish (cpp_reader *pfile ATTRIBUTE_UNUSED, FILE *f ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

unsigned int
cpp_errors (cpp_reader *pfile ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Dummy called by C.   */

tree
handle_format_attribute (tree *node ATTRIBUTE_UNUSED,
                         tree name ATTRIBUTE_UNUSED,
                         tree args ATTRIBUTE_UNUSED,
                         int flags ATTRIBUTE_UNUSED,
                         bool *no_add_attrs ATTRIBUTE_UNUSED)
{
  return NULL_TREE; 
}

/* Should not be called for treelang.   */

tree
handle_format_arg_attribute (tree *node ATTRIBUTE_UNUSED,
     tree name ATTRIBUTE_UNUSED,
     tree args ATTRIBUTE_UNUSED,
     int flags ATTRIBUTE_UNUSED,
     bool *no_add_attrs ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

int
cpp_handle_option (cpp_reader *pfile ATTRIBUTE_UNUSED,
     int argc ATTRIBUTE_UNUSED,
     char **argv ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

void 
cpp_assert (cpp_reader * cr ATTRIBUTE_UNUSED, 
            const char *s ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

void
set_Wformat (int setting ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Used for objective C.  */

void
objc_check_decl (tree decl ATTRIBUTE_UNUSED);

void
objc_check_decl (tree decl ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Tell the c code we are not objective C.  */

tree
objc_message_selector (void);

tree
objc_message_selector ()
{
  return 0;
}

/* Should not be called for treelang.   */

void
gen_aux_info_record (tree fndecl ATTRIBUTE_UNUSED,
                     int is_definition ATTRIBUTE_UNUSED,
                     int is_implicit ATTRIBUTE_UNUSED,
                     int is_prototyped ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang, but it is.   */

void
c_parse_init ()
{
  return;
}

/* Should not be called for treelang.   */

void maybe_apply_pragma_weak (tree decl);

void
maybe_apply_pragma_weak (tree decl ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

void
add_decl_stmt (tree decl ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

tree
maybe_apply_renaming_pragma (tree decl, tree asmname);

/* Should not be called for treelang.   */

tree
maybe_apply_renaming_pragma (tree decl ATTRIBUTE_UNUSED, tree asmname ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

void
begin_stmt_tree (tree *t ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

void
finish_stmt_tree (tree *t ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

int
defer_fn (tree fn ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

cpp_options 
*cpp_get_options (cpp_reader * cr ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Should not be called for treelang.   */

void 
cpp_define (cpp_reader * cr ATTRIBUTE_UNUSED, const char * c ATTRIBUTE_UNUSED)
{
  abort ();  
}

/* Should not be called for treelang.   */

cpp_callbacks *
cpp_get_callbacks (cpp_reader * cr ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Create the predefined scalar types of C,
   and some nodes representing standard constants (0, 1, (void *) 0).
   Initialize the global binding level.
   Make definitions for built-in primitive functions.  */

  /* `unsigned long' is the standard type for sizeof.
     Note that stddef.h uses `unsigned long',
     and this must agree, even if long and int are the same size.  */

/* The reserved keyword table.  */
struct resword
{
  const char *word;
  ENUM_BITFIELD(rid) rid : 16;
  unsigned int disable   : 16;
};

static const struct resword reswords[] =
{
  { "_Bool",		RID_BOOL,	0 },
  { "_Complex",		RID_COMPLEX,	0 },
  { "__FUNCTION__",	RID_FUNCTION_NAME, 0 },
  { "__PRETTY_FUNCTION__", RID_PRETTY_FUNCTION_NAME, 0 },
  { "__alignof",	RID_ALIGNOF,	0 },
  { "__alignof__",	RID_ALIGNOF,	0 },
  { "__asm",		RID_ASM,	0 },
  { "__asm__",		RID_ASM,	0 },
  { "__attribute",	RID_ATTRIBUTE,	0 },
  { "__attribute__",	RID_ATTRIBUTE,	0 },
  { "__bounded",	RID_BOUNDED,	0 },
  { "__bounded__",	RID_BOUNDED,	0 },
  { "__builtin_choose_expr", RID_CHOOSE_EXPR, 0 },
  { "__builtin_types_compatible_p", RID_TYPES_COMPATIBLE_P, 0 },
  { "__builtin_va_arg",	RID_VA_ARG,	0 },
  { "__complex",	RID_COMPLEX,	0 },
  { "__complex__",	RID_COMPLEX,	0 },
  { "__const",		RID_CONST,	0 },
  { "__const__",	RID_CONST,	0 },
  { "__extension__",	RID_EXTENSION,	0 },
  { "__func__",		RID_C99_FUNCTION_NAME, 0 },
  { "__imag",		RID_IMAGPART,	0 },
  { "__imag__",		RID_IMAGPART,	0 },
  { "__inline",		RID_INLINE,	0 },
  { "__inline__",	RID_INLINE,	0 },
  { "__label__",	RID_LABEL,	0 },
  { "__ptrbase",	RID_PTRBASE,	0 },
  { "__ptrbase__",	RID_PTRBASE,	0 },
  { "__ptrextent",	RID_PTREXTENT,	0 },
  { "__ptrextent__",	RID_PTREXTENT,	0 },
  { "__ptrvalue",	RID_PTRVALUE,	0 },
  { "__ptrvalue__",	RID_PTRVALUE,	0 },
  { "__real",		RID_REALPART,	0 },
  { "__real__",		RID_REALPART,	0 },
  { "__restrict",	RID_RESTRICT,	0 },
  { "__restrict__",	RID_RESTRICT,	0 },
  { "__signed",		RID_SIGNED,	0 },
  { "__signed__",	RID_SIGNED,	0 },
  { "__typeof",		RID_TYPEOF,	0 },
  { "__typeof__",	RID_TYPEOF,	0 },
  { "__unbounded",	RID_UNBOUNDED,	0 },
  { "__unbounded__",	RID_UNBOUNDED,	0 },
  { "__volatile",	RID_VOLATILE,	0 },
  { "__volatile__",	RID_VOLATILE,	0 },
  { "asm",		RID_ASM,	0 },
  { "auto",		RID_AUTO,	0 },
  { "break",		RID_BREAK,	0 },
  { "case",		RID_CASE,	0 },
  { "char",		RID_CHAR,	0 },
  { "const",		RID_CONST,	0 },
  { "continue",		RID_CONTINUE,	0 },
  { "default",		RID_DEFAULT,	0 },
  { "do",		RID_DO,		0 },
  { "double",		RID_DOUBLE,	0 },
  { "else",		RID_ELSE,	0 },
  { "enum",		RID_ENUM,	0 },
  { "extern",		RID_EXTERN,	0 },
  { "float",		RID_FLOAT,	0 },
  { "for",		RID_FOR,	0 },
  { "goto",		RID_GOTO,	0 },
  { "if",		RID_IF,		0 },
  { "inline",		RID_INLINE,	0 },
  { "int",		RID_INT,	0 },
  { "long",		RID_LONG,	0 },
  { "register",		RID_REGISTER,	0 },
  { "restrict",		RID_RESTRICT,	0 },
  { "return",		RID_RETURN,	0 },
  { "short",		RID_SHORT,	0 },
  { "signed",		RID_SIGNED,	0 },
  { "sizeof",		RID_SIZEOF,	0 },
  { "static",		RID_STATIC,	0 },
  { "struct",		RID_STRUCT,	0 },
  { "switch",		RID_SWITCH,	0 },
  { "typedef",		RID_TYPEDEF,	0 },
  { "typeof",		RID_TYPEOF,	0 },
  { "union",		RID_UNION,	0 },
  { "unsigned",		RID_UNSIGNED,	0 },
  { "void",		RID_VOID,	0 },
  { "volatile",		RID_VOLATILE,	0 },
  { "while",		RID_WHILE,	0 },
};
#define N_reswords (sizeof reswords / sizeof (struct resword))

/* Init enough to allow the C decl code to work, then clean up
   afterwards.  */

void
treelang_init_decl_processing ()
{
  unsigned int i;
  tree id;

  /* It is not necessary to register ridpointers as a GC root, because
     all the trees it points to are permanently interned in the
     get_identifier hash anyway.  */
  ridpointers = (tree *) xcalloc ((int) RID_MAX, sizeof (tree));
  
  for (i = 0; i < N_reswords; i++)
    {
      id = get_identifier (reswords[i].word);
      C_RID_CODE (id) = reswords[i].rid;
      C_IS_RESERVED_WORD (id) = 1;
      ridpointers [(int) reswords[i].rid] = id;
    }

  c_init_decl_processing ();

  /* ix86_return_pops_args takes the type of these so need to patch
     their own type as themselves.  */

  for (i = 0; i < itk_none; i++)
    {
      if (integer_types[i])
        TREE_TYPE (integer_types [i]) = integer_types[i];
    }

  /* Probably these ones too.  */
  TREE_TYPE (float_type_node) = float_type_node;
  TREE_TYPE (double_type_node) = double_type_node;
  TREE_TYPE (long_double_type_node) = long_double_type_node;

}

/* Save typing debug_tree all the time. Dump a tree T pretty and
   concise.  */

void dt (tree t);

void
dt (tree t)
{
  debug_tree (t);
}
