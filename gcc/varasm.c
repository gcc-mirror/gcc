/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 1987, 1988, 1989, 1992, 1993 Free Software Foundation, Inc.

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


/* This file handles generation of all the assembler code
   *except* the instructions of a function.
   This includes declarations of variables and their initial values.

   We also output the assembler code for constants stored in memory
   and are responsible for combining constants with the same value.  */

#include <stdio.h>
#include <setjmp.h>
/* #include <stab.h> */
#include "config.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "defaults.h"
#include "real.h"

#include "obstack.h"

#ifdef XCOFF_DEBUGGING_INFO
#include "xcoffout.h"
#endif

#ifndef ASM_STABS_OP
#define ASM_STABS_OP ".stabs"
#endif

/* This macro gets just the user-specified name
   out of the string in a SYMBOL_REF.  On most machines,
   we discard the * if any and that's all.  */
#ifndef STRIP_NAME_ENCODING
#define STRIP_NAME_ENCODING(VAR,SYMBOL_NAME) \
  (VAR) = ((SYMBOL_NAME) + ((SYMBOL_NAME)[0] == '*'))
#endif

/* File in which assembler code is being written.  */

extern FILE *asm_out_file;

/* The (assembler) name of the first globally-visible object output.  */
char *first_global_object_name;

extern struct obstack *current_obstack;
extern struct obstack *saveable_obstack;
extern struct obstack permanent_obstack;
#define obstack_chunk_alloc xmalloc

/* Number for making the label on the next
   constant that is stored in memory.  */

int const_labelno;

/* Number for making the label on the next
   static variable internal to a function.  */

int var_labelno;

/* Nonzero if at least one function definition has been seen.  */
static int function_defined;

extern FILE *asm_out_file;

static char *compare_constant_1 ();
static void record_constant_1 ();
void output_constant_pool ();
void assemble_name ();
int output_addressed_constants ();
void output_constant ();
void output_constructor ();
void text_section ();
void readonly_data_section ();
void data_section ();

#ifdef EXTRA_SECTIONS
static enum in_section {no_section, in_text, in_data, EXTRA_SECTIONS} in_section
  = no_section;
#else
static enum in_section {no_section, in_text, in_data} in_section
  = no_section;
#endif

/* Define functions like text_section for any extra sections.  */
#ifdef EXTRA_SECTION_FUNCTIONS
EXTRA_SECTION_FUNCTIONS
#endif

/* Tell assembler to switch to text section.  */

void
text_section ()
{
  if (in_section != in_text)
    {
      fprintf (asm_out_file, "%s\n", TEXT_SECTION_ASM_OP);
      in_section = in_text;
    }
}

/* Tell assembler to switch to data section.  */

void
data_section ()
{
  if (in_section != in_data)
    {
      if (flag_shared_data)
	{
#ifdef SHARED_SECTION_ASM_OP
	  fprintf (asm_out_file, "%s\n", SHARED_SECTION_ASM_OP);
#else
	  fprintf (asm_out_file, "%s\n", DATA_SECTION_ASM_OP);
#endif
	}
      else
	fprintf (asm_out_file, "%s\n", DATA_SECTION_ASM_OP);

      in_section = in_data;
    }
}

/* Tell assembler to switch to read-only data section.  This is normally
   the text section.  */

void
readonly_data_section ()
{
#ifdef READONLY_DATA_SECTION
  READONLY_DATA_SECTION ();  /* Note this can call data_section.  */
#else
  text_section ();
#endif
}

/* Determine if we're in the text section. */

int
in_text_section ()
{
  return in_section == in_text;
}

/* Create the rtl to represent a function, for a function definition.
   DECL is a FUNCTION_DECL node which describes which function.
   The rtl is stored into DECL.  */

void
make_function_rtl (decl)
     tree decl;
{
  char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  /* Rename a nested function to avoid conflicts.  */
  if (decl_function_context (decl) != 0
      && DECL_INITIAL (decl) != 0
      && DECL_RTL (decl) == 0)
    {
      char *label;

      name = IDENTIFIER_POINTER (DECL_NAME (decl));
      ASM_FORMAT_PRIVATE_NAME (label, name, var_labelno);
      name = obstack_copy0 (saveable_obstack, label, strlen (label));
      var_labelno++;
    }

  if (DECL_RTL (decl) == 0)
    {
      DECL_RTL (decl)
	= gen_rtx (MEM, DECL_MODE (decl),
		   gen_rtx (SYMBOL_REF, Pmode, name));

      /* Optionally set flags or add text to the name to record information
	 such as that it is a function name.  If the name is changed, the macro
	 ASM_OUTPUT_LABELREF will have to know how to strip this information.
	 And if it finds a * at the beginning after doing so, it must handle
	 that too.  */
#ifdef ENCODE_SECTION_INFO
      ENCODE_SECTION_INFO (decl);
#endif
    }

  /* Record at least one function has been defined.  */
  function_defined = 1;
}

/* Given NAME, a putative register name, discard any customary prefixes.  */

static char *
strip_reg_name (name)
     char *name;
{
#ifdef REGISTER_PREFIX
  if (!strncmp (name, REGISTER_PREFIX, strlen (REGISTER_PREFIX)))
    name += strlen (REGISTER_PREFIX);
#endif
  if (name[0] == '%' || name[0] == '#')
    name++;
  return name;
}

/* Decode an `asm' spec for a declaration as a register name.
   Return the register number, or -1 if nothing specified,
   or -2 if the ASMSPEC is not `cc' or `memory' and is not recognized,
   or -3 if ASMSPEC is `cc' and is not recognized,
   or -4 if ASMSPEC is `memory' and is not recognized.
   Accept an exact spelling or a decimal number.
   Prefixes such as % are optional.  */

int
decode_reg_name (asmspec)
     char *asmspec;
{
  if (asmspec != 0)
    {
      int i;

      /* Get rid of confusing prefixes.  */
      asmspec = strip_reg_name (asmspec);
	
      /* Allow a decimal number as a "register name".  */
      for (i = strlen (asmspec) - 1; i >= 0; i--)
	if (! (asmspec[i] >= '0' && asmspec[i] <= '9'))
	  break;
      if (asmspec[0] != 0 && i < 0)
	{
	  i = atoi (asmspec);
	  if (i < FIRST_PSEUDO_REGISTER && i >= 0)
	    return i;
	  else
	    return -2;
	}

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (reg_names[i][0]
	    && ! strcmp (asmspec, strip_reg_name (reg_names[i])))
	  return i;

#ifdef ADDITIONAL_REGISTER_NAMES
      {
	static struct { char *name; int number; } table[]
	  = ADDITIONAL_REGISTER_NAMES;

	for (i = 0; i < sizeof (table) / sizeof (table[0]); i++)
	  if (! strcmp (asmspec, table[i].name))
	    return table[i].number;
      }
#endif /* ADDITIONAL_REGISTER_NAMES */

      if (!strcmp (asmspec, "memory"))
	return -4;

      if (!strcmp (asmspec, "cc"))
	return -3;

      return -2;
    }

  return -1;
}

/* Create the DECL_RTL for a declaration for a static or external variable
   or static or external function.
   ASMSPEC, if not 0, is the string which the user specified
   as the assembler symbol name.
   TOP_LEVEL is nonzero if this is a file-scope variable.

   This is never called for PARM_DECL nodes.  */

void
make_decl_rtl (decl, asmspec, top_level)
     tree decl;
     char *asmspec;
     int top_level;
{
  register char *name;
  int reg_number = decode_reg_name (asmspec);

  if (DECL_ASSEMBLER_NAME (decl) != NULL_TREE)
    name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  if (reg_number == -2)
    {
      /* ASMSPEC is given, and not the name of a register.  */
      name = (char *) obstack_alloc (saveable_obstack,
				     strlen (asmspec) + 2);
      name[0] = '*';
      strcpy (&name[1], asmspec);
    }

  /* For a duplicate declaration, we can be called twice on the
     same DECL node.  Don't alter the RTL already made
     unless the old mode is wrong (which can happen when
     the previous rtl was made when the type was incomplete).  */
  if (DECL_RTL (decl) == 0
      || GET_MODE (DECL_RTL (decl)) != DECL_MODE (decl))
    {
      DECL_RTL (decl) = 0;

      /* First detect errors in declaring global registers.  */
      if (DECL_REGISTER (decl) && reg_number == -1)
	error_with_decl (decl,
			 "register name not specified for `%s'");
      else if (DECL_REGISTER (decl) && reg_number < 0)
	error_with_decl (decl,
			 "invalid register name for `%s'");
      else if ((reg_number >= 0 || reg_number == -3) && ! DECL_REGISTER (decl))
	error_with_decl (decl,
			 "register name given for non-register variable `%s'");
      else if (DECL_REGISTER (decl) && TREE_CODE (decl) == FUNCTION_DECL)
	error ("function declared `register'");
      else if (DECL_REGISTER (decl) && TYPE_MODE (TREE_TYPE (decl)) == BLKmode)
	error_with_decl (decl, "data type of `%s' isn't suitable for a register");
      else if (DECL_REGISTER (decl)
	       && ! HARD_REGNO_MODE_OK (reg_number, TYPE_MODE (TREE_TYPE (decl))))
	error_with_decl (decl, "register number for `%s' isn't suitable for the data type");
      /* Now handle properly declared static register variables.  */
      else if (DECL_REGISTER (decl))
	{
	  int nregs;
#if 0 /* yylex should print the warning for this */
	  if (pedantic)
	    pedwarn ("ANSI C forbids global register variables");
#endif
	  if (DECL_INITIAL (decl) != 0 && top_level)
	    {
	      DECL_INITIAL (decl) = 0;
	      error ("global register variable has initial value");
	    }
	  if (fixed_regs[reg_number] == 0
	      && function_defined && top_level)
	    error ("global register variable follows a function definition");
	  if (TREE_THIS_VOLATILE (decl))
	    warning ("volatile register variables don't work as you might wish");

	  /* If the user specified one of the eliminables registers here,
	     e.g., FRAME_POINTER_REGNUM, we don't want to get this variable
	     confused with that register and be eliminated.  Although this
	     usage is somewhat suspect, we nevertheless use the following
	     kludge to avoid setting DECL_RTL to frame_pointer_rtx.  */

	  DECL_RTL (decl)
	    = gen_rtx (REG, DECL_MODE (decl), FIRST_PSEUDO_REGISTER);
	  REGNO (DECL_RTL (decl)) = reg_number;
	  REG_USERVAR_P (DECL_RTL (decl)) = 1;

	  if (top_level)
	    {
	      /* Make this register fixed, so not usable for anything else.  */
	      nregs = HARD_REGNO_NREGS (reg_number, DECL_MODE (decl));
	      while (nregs > 0)
		global_regs[reg_number + --nregs] = 1;
	      init_reg_sets_1 ();
	    }
	}

      /* Now handle ordinary static variables and functions (in memory).
	 Also handle vars declared register invalidly.  */
      if (DECL_RTL (decl) == 0)
	{
	  /* Can't use just the variable's own name for a variable
	     whose scope is less than the whole file.
	     Concatenate a distinguishing number.  */
	  if (!top_level && !DECL_EXTERNAL (decl) && asmspec == 0)
	    {
	      char *label;

	      ASM_FORMAT_PRIVATE_NAME (label, name, var_labelno);
	      name = obstack_copy0 (saveable_obstack, label, strlen (label));
	      var_labelno++;
	    }

	  DECL_RTL (decl) = gen_rtx (MEM, DECL_MODE (decl),
				     gen_rtx (SYMBOL_REF, Pmode, name));
	  if (TREE_THIS_VOLATILE (decl)
	    || (flag_volatile_global && TREE_CODE (decl) == VAR_DECL
		&& TREE_PUBLIC (decl)))
	    MEM_VOLATILE_P (DECL_RTL (decl)) = 1;
	  if (TREE_READONLY (decl))
	    RTX_UNCHANGING_P (DECL_RTL (decl)) = 1;
	  MEM_IN_STRUCT_P (DECL_RTL (decl))
	    = (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
	       || TREE_CODE (TREE_TYPE (decl)) == RECORD_TYPE
	       || TREE_CODE (TREE_TYPE (decl)) == UNION_TYPE
	       || TREE_CODE (TREE_TYPE (decl)) == QUAL_UNION_TYPE);

	  /* Optionally set flags or add text to the name to record information
	     such as that it is a function name.
	     If the name is changed, the macro ASM_OUTPUT_LABELREF
	     will have to know how to strip this information.
	     And if it finds a * at the beginning after doing so,
	     it must handle that too.  */
#ifdef ENCODE_SECTION_INFO
	  ENCODE_SECTION_INFO (decl);
#endif
	}
    }
}

/* Make the rtl for variable VAR be volatile.
   Use this only for static variables.  */

void
make_var_volatile (var)
     tree var;
{
  if (GET_CODE (DECL_RTL (var)) != MEM)
    abort ();

  MEM_VOLATILE_P (DECL_RTL (var)) = 1;
}

/* Output alignment directive to align for constant expression EXP.  */

void
assemble_constant_align (exp)
     tree exp;
{
  int align;

  /* Align the location counter as required by EXP's data type.  */
  align = TYPE_ALIGN (TREE_TYPE (exp));
#ifdef CONSTANT_ALIGNMENT
  align = CONSTANT_ALIGNMENT (exp, align);
#endif

  if (align > BITS_PER_UNIT)
    ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (align / BITS_PER_UNIT));
}

/* Output a string of literal assembler code
   for an `asm' keyword used between functions.  */

void
assemble_asm (string)
     tree string;
{
  app_enable ();

  if (TREE_CODE (string) == ADDR_EXPR)
    string = TREE_OPERAND (string, 0);

  fprintf (asm_out_file, "\t%s\n", TREE_STRING_POINTER (string));
}

#if 0 /* This should no longer be needed, because
	 flag_gnu_linker should be 0 on these systems,
	 which should prevent any output
	 if ASM_OUTPUT_CONSTRUCTOR and ASM_OUTPUT_DESTRUCTOR are absent.  */
#if !(defined(DBX_DEBUGGING_INFO) && !defined(FASCIST_ASSEMBLER))
#ifndef ASM_OUTPUT_CONSTRUCTOR
#define ASM_OUTPUT_CONSTRUCTOR(file, name)
#endif
#ifndef ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(file, name)
#endif
#endif
#endif /* 0 */

/* Record an element in the table of global destructors.
   How this is done depends on what sort of assembler and linker
   are in use.

   NAME should be the name of a global function to be called
   at exit time.  This name is output using assemble_name.  */

void
assemble_destructor (name)
     char *name;
{
#ifdef ASM_OUTPUT_DESTRUCTOR
  ASM_OUTPUT_DESTRUCTOR (asm_out_file, name);
#else
  if (flag_gnu_linker)
    {
      /* Now tell GNU LD that this is part of the static destructor set.  */
      /* This code works for any machine provided you use GNU as/ld.  */
      fprintf (asm_out_file, "%s \"___DTOR_LIST__\",22,0,0,", ASM_STABS_OP);
      assemble_name (asm_out_file, name);
      fputc ('\n', asm_out_file);
    }
#endif
}

/* Likewise for global constructors.  */

void
assemble_constructor (name)
     char *name;
{
#ifdef ASM_OUTPUT_CONSTRUCTOR
  ASM_OUTPUT_CONSTRUCTOR (asm_out_file, name);
#else
  if (flag_gnu_linker)
    {
      /* Now tell GNU LD that this is part of the static constructor set.  */
      /* This code works for any machine provided you use GNU as/ld.  */
      fprintf (asm_out_file, "%s \"___CTOR_LIST__\",22,0,0,", ASM_STABS_OP);
      assemble_name (asm_out_file, name);
      fputc ('\n', asm_out_file);
    }
#endif
}

/* Likewise for entries we want to record for garbage collection.
   Garbage collection is still under development.  */

void
assemble_gc_entry (name)
     char *name;
{
#ifdef ASM_OUTPUT_GC_ENTRY
  ASM_OUTPUT_GC_ENTRY (asm_out_file, name);
#else
  if (flag_gnu_linker)
    {
      /* Now tell GNU LD that this is part of the static constructor set.  */
      fprintf (asm_out_file, "%s \"___PTR_LIST__\",22,0,0,", ASM_STABS_OP);
      assemble_name (asm_out_file, name);
      fputc ('\n', asm_out_file);
    }
#endif
}

/* Output assembler code for the constant pool of a function and associated
   with defining the name of the function.  DECL describes the function.
   NAME is the function's name.  For the constant pool, we use the current
   constant pool data.  */

void
assemble_start_function (decl, fnname)
     tree decl;
     char *fnname;
{
  int align;

  /* The following code does not need preprocessing in the assembler.  */

  app_disable ();

  output_constant_pool (fnname, decl);

  text_section ();


  /* Tell assembler to move to target machine's alignment for functions.  */
  align = floor_log2 (FUNCTION_BOUNDARY / BITS_PER_UNIT);
  if (align > 0)
    ASM_OUTPUT_ALIGN (asm_out_file, align);

#ifdef ASM_OUTPUT_FUNCTION_PREFIX
  ASM_OUTPUT_FUNCTION_PREFIX (asm_out_file, fnname);
#endif

#ifdef SDB_DEBUGGING_INFO
  /* Output SDB definition of the function.  */
  if (write_symbols == SDB_DEBUG)
    sdbout_mark_begin_function ();
#endif

#ifdef DBX_DEBUGGING_INFO
  /* Output DBX definition of the function.  */
  if (write_symbols == DBX_DEBUG)
    dbxout_begin_function (decl);
#endif

  /* Make function name accessible from other files, if appropriate.  */

  if (TREE_PUBLIC (decl))
    {
      if (!first_global_object_name)
	STRIP_NAME_ENCODING (first_global_object_name, fnname);
      ASM_GLOBALIZE_LABEL (asm_out_file, fnname);
    }

  /* Do any machine/system dependent processing of the function name */
#ifdef ASM_DECLARE_FUNCTION_NAME
  ASM_DECLARE_FUNCTION_NAME (asm_out_file, fnname, current_function_decl);
#else
  /* Standard thing is just output label for the function.  */
  ASM_OUTPUT_LABEL (asm_out_file, fnname);
#endif /* ASM_DECLARE_FUNCTION_NAME */
}

/* Output assembler code associated with defining the size of the
   function.  DECL describes the function.  NAME is the function's name.  */

void
assemble_end_function (decl, fnname)
     tree decl;
     char *fnname;
{
#ifdef ASM_DECLARE_FUNCTION_SIZE
  ASM_DECLARE_FUNCTION_SIZE (asm_out_file, fnname, decl);
#endif
}

/* Assemble code to leave SIZE bytes of zeros.  */

void
assemble_zeros (size)
     int size;
{
#ifdef ASM_NO_SKIP_IN_TEXT
  /* The `space' pseudo in the text section outputs nop insns rather than 0s,
     so we must output 0s explicitly in the text section.  */
  if (ASM_NO_SKIP_IN_TEXT && in_text_section ())
    {
      int i;

      for (i = 0; i < size - 20; i += 20)
	{
#ifdef ASM_BYTE_OP
	  fprintf (asm_out_file,
		   "%s 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n", ASM_BYTE_OP);
#else
	  fprintf (asm_out_file,
		   "\tbyte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n");
#endif
	}
      if (i < size)
        {
#ifdef ASM_BYTE_OP
	  fprintf (asm_out_file, "%s 0", ASM_BYTE_OP);
#else
	  fprintf (asm_out_file, "\tbyte 0");
#endif
	  i++;
	  for (; i < size; i++)
	    fprintf (asm_out_file, ",0");
	  fprintf (asm_out_file, "\n");
	}
    }
  else
#endif
    if (size > 0)
      ASM_OUTPUT_SKIP (asm_out_file, size);
}

/* Assemble a string constant with the specified C string as contents.  */

void
assemble_string (p, size)
     char *p;
     int size;
{
  register int i;
  int pos = 0;
  int maximum = 2000;

  /* If the string is very long, split it up.  */

  while (pos < size)
    {
      int thissize = size - pos;
      if (thissize > maximum)
	thissize = maximum;

      ASM_OUTPUT_ASCII (asm_out_file, p, thissize);

      pos += thissize;
      p += thissize;
    }
}

/* Assemble everything that is needed for a variable or function declaration.
   Not used for automatic variables, and not used for function definitions.
   Should not be called for variables of incomplete structure type.

   TOP_LEVEL is nonzero if this variable has file scope.
   AT_END is nonzero if this is the special handling, at end of compilation,
   to define things that have had only tentative definitions.  */

void
assemble_variable (decl, top_level, at_end)
     tree decl;
     int top_level;
     int at_end;
{
  register char *name;
  int align;
  tree size_tree;
  int reloc = 0;

  if (GET_CODE (DECL_RTL (decl)) == REG)
    {
      /* Do output symbol info for global register variables, but do nothing
	 else for them.  */

      if (TREE_ASM_WRITTEN (decl))
	return;
      TREE_ASM_WRITTEN (decl) = 1;

#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
      /* File-scope global variables are output here.  */
      if ((write_symbols == DBX_DEBUG || write_symbols == XCOFF_DEBUG)
	  && top_level)
	dbxout_symbol (decl, 0);
#endif
#ifdef SDB_DEBUGGING_INFO
      if (write_symbols == SDB_DEBUG && top_level
	  /* Leave initialized global vars for end of compilation;
	     see comment in compile_file.  */
	  && (TREE_PUBLIC (decl) == 0 || DECL_INITIAL (decl) == 0))
	sdbout_symbol (decl, 0);
#endif

      /* Don't output any DWARF debugging information for variables here.
	 In the case of local variables, the information for them is output
	 when we do our recursive traversal of the tree representation for
	 the entire containing function.  In the case of file-scope variables,
	 we output information for all of them at the very end of compilation
	 while we are doing our final traversal of the chain of file-scope
	 declarations.  */

      return;
    }

  /* Normally no need to say anything here for external references,
     since assemble_external is called by the langauge-specific code
     when a declaration is first seen.  */

  if (DECL_EXTERNAL (decl))
    return;

  /* Output no assembler code for a function declaration.
     Only definitions of functions output anything.  */

  if (TREE_CODE (decl) == FUNCTION_DECL)
    return;

  /* If type was incomplete when the variable was declared,
     see if it is complete now.  */

  if (DECL_SIZE (decl) == 0)
    layout_decl (decl, 0);

  /* Still incomplete => don't allocate it; treat the tentative defn
     (which is what it must have been) as an `extern' reference.  */

  if (DECL_SIZE (decl) == 0)
    {
      error_with_file_and_line (DECL_SOURCE_FILE (decl),
				DECL_SOURCE_LINE (decl),
				"storage size of `%s' isn't known",
				IDENTIFIER_POINTER (DECL_NAME (decl)));
      return;
    }

  /* The first declaration of a variable that comes through this function
     decides whether it is global (in C, has external linkage)
     or local (in C, has internal linkage).  So do nothing more
     if this function has already run.  */

  if (TREE_ASM_WRITTEN (decl))
    return;

  TREE_ASM_WRITTEN (decl) = 1;

#ifdef DBX_DEBUGGING_INFO
  /* File-scope global variables are output here.  */
  if (write_symbols == DBX_DEBUG && top_level)
    dbxout_symbol (decl, 0);
#endif
#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG && top_level
      /* Leave initialized global vars for end of compilation;
	 see comment in compile_file.  */
      && (TREE_PUBLIC (decl) == 0 || DECL_INITIAL (decl) == 0))
    sdbout_symbol (decl, 0);
#endif

  /* Don't output any DWARF debugging information for variables here.
     In the case of local variables, the information for them is output
     when we do our recursive traversal of the tree representation for
     the entire containing function.  In the case of file-scope variables,
     we output information for all of them at the very end of compilation
     while we are doing our final traversal of the chain of file-scope
     declarations.  */

  /* If storage size is erroneously variable, just continue.
     Error message was already made.  */

  if (TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
    goto finish;

  app_disable ();

  /* This is better than explicit arithmetic, since it avoids overflow.  */
  size_tree = size_binop (CEIL_DIV_EXPR,
			  DECL_SIZE (decl), size_int (BITS_PER_UNIT));

  if (TREE_INT_CST_HIGH (size_tree) != 0)
    {
      error_with_decl (decl, "size of variable `%s' is too large");
      goto finish;
    }

  name = XSTR (XEXP (DECL_RTL (decl), 0), 0);

  /* Handle uninitialized definitions.  */

  /* ANSI specifies that a tentative definition which is not merged with
     a non-tentative definition behaves exactly like a definition with an
     initializer equal to zero.  (Section 3.7.2)
     -fno-common gives strict ANSI behavior.  Usually you don't want it.  */
  if (! flag_no_common
      && (DECL_INITIAL (decl) == 0 || DECL_INITIAL (decl) == error_mark_node))
    {
      int size = TREE_INT_CST_LOW (size_tree);
      int rounded = size;

      if (TREE_INT_CST_HIGH (size_tree) != 0)
	error_with_decl (decl, "size of variable `%s' is too large");
      /* Don't allocate zero bytes of common,
	 since that means "undefined external" in the linker.  */
      if (size == 0) rounded = 1;
      /* Round size up to multiple of BIGGEST_ALIGNMENT bits
	 so that each uninitialized object starts on such a boundary.  */
      rounded += (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1;
      rounded = (rounded / (BIGGEST_ALIGNMENT / BITS_PER_UNIT)
		 * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));
#if 0
      if (flag_shared_data)
	data_section ();
#endif
      if (TREE_PUBLIC (decl))
	{
#ifdef ASM_OUTPUT_SHARED_COMMON
	  if (flag_shared_data)
	    ASM_OUTPUT_SHARED_COMMON (asm_out_file, name, size, rounded);
	  else
#endif
#ifdef ASM_OUTPUT_ALIGNED_COMMON
	    ASM_OUTPUT_ALIGNED_COMMON (asm_out_file, name, size,
				       DECL_ALIGN (decl));
#else
	    ASM_OUTPUT_COMMON (asm_out_file, name, size, rounded);
#endif
	}
      else
	{
#ifdef ASM_OUTPUT_SHARED_LOCAL
	  if (flag_shared_data)
	    ASM_OUTPUT_SHARED_LOCAL (asm_out_file, name, size, rounded);
	  else
#endif
#ifdef ASM_OUTPUT_ALIGNED_LOCAL
	    ASM_OUTPUT_ALIGNED_LOCAL (asm_out_file, name, size,
				      DECL_ALIGN (decl));
#else
	    ASM_OUTPUT_LOCAL (asm_out_file, name, size, rounded);
#endif
	}
      goto finish;
    }

  /* Handle initialized definitions.  */

  /* First make the assembler name(s) global if appropriate.  */
  if (TREE_PUBLIC (decl) && DECL_NAME (decl))
    {
      if (!first_global_object_name)
	STRIP_NAME_ENCODING(first_global_object_name, name);
      ASM_GLOBALIZE_LABEL (asm_out_file, name);
    }
#if 0
  for (d = equivalents; d; d = TREE_CHAIN (d))
    {
      tree e = TREE_VALUE (d);
      if (TREE_PUBLIC (e) && DECL_NAME (e))
	ASM_GLOBALIZE_LABEL (asm_out_file,
			     XSTR (XEXP (DECL_RTL (e), 0), 0));
    }
#endif

  /* Output any data that we will need to use the address of.  */
  if (DECL_INITIAL (decl))
    reloc = output_addressed_constants (DECL_INITIAL (decl));

  /* Switch to the proper section for this data.  */
#ifdef SELECT_SECTION
  SELECT_SECTION (decl, reloc);
#else
  if (TREE_READONLY (decl)
      && ! TREE_THIS_VOLATILE (decl)
      && ! (flag_pic && reloc))
    readonly_data_section ();
  else
    data_section ();
#endif

  /* Compute and output the alignment of this data.  */

  align = DECL_ALIGN (decl);
  /* Some object file formats have a maximum alignment which they support.
     In particular, a.out format supports a maximum alignment of 4.  */
#ifndef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT BIGGEST_ALIGNMENT
#endif
  if (align > MAX_OFILE_ALIGNMENT)
    {
      warning_with_decl (decl,
	  "alignment of `%s' is greater than maximum object file alignment");
      align = MAX_OFILE_ALIGNMENT;
    }
#ifdef DATA_ALIGNMENT
  /* On some machines, it is good to increase alignment sometimes.  */
  align = DATA_ALIGNMENT (TREE_TYPE (decl), align);
#endif
#ifdef CONSTANT_ALIGNMENT
  if (DECL_INITIAL (decl))
    align = CONSTANT_ALIGNMENT (DECL_INITIAL (decl), align);
#endif

  /* Reset the alignment in case we have made it tighter, so we can benefit
     from it in get_pointer_alignment.  */
  DECL_ALIGN (decl) = align;

  if (align > BITS_PER_UNIT)
    ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (align / BITS_PER_UNIT));

  /* Do any machine/system dependent processing of the object.  */
#ifdef ASM_DECLARE_OBJECT_NAME
  ASM_DECLARE_OBJECT_NAME (asm_out_file, name, decl);
#else
  /* Standard thing is just output label for the object.  */
  ASM_OUTPUT_LABEL (asm_out_file, name);
#endif /* ASM_DECLARE_OBJECT_NAME */

#if 0
  for (d = equivalents; d; d = TREE_CHAIN (d))
    {
      tree e = TREE_VALUE (d);
      ASM_OUTPUT_LABEL (asm_out_file, XSTR (XEXP (DECL_RTL (e), 0), 0));
    }
#endif

  if (DECL_INITIAL (decl))
    /* Output the actual data.  */
    output_constant (DECL_INITIAL (decl),
		     int_size_in_bytes (TREE_TYPE (decl)));
  else
    /* Leave space for it.  */
    assemble_zeros (int_size_in_bytes (TREE_TYPE (decl)));

 finish:
#ifdef XCOFF_DEBUGGING_INFO
  /* Unfortunately, the IBM assembler cannot handle stabx before the actual
     declaration.  When something like ".stabx  "aa:S-2",aa,133,0" is emitted 
     and `aa' hasn't been output yet, the assembler generates a stab entry with
     a value of zero, in addition to creating an unnecessary external entry
     for `aa'.  Hence, we must postpone dbxout_symbol to here at the end.  */

  /* File-scope global variables are output here.  */
  if (write_symbols == XCOFF_DEBUG && top_level)
    dbxout_symbol (decl, 0);
#else
  /* There must be a statement after a label.  */
  ;
#endif
}

/* Output something to declare an external symbol to the assembler.
   (Most assemblers don't need this, so we normally output nothing.)
   Do nothing if DECL is not external.  */

void
assemble_external (decl)
     tree decl;
{
#ifdef ASM_OUTPUT_EXTERNAL
  if (TREE_CODE_CLASS (TREE_CODE (decl)) == 'd'
      && DECL_EXTERNAL (decl) && TREE_PUBLIC (decl))
    {
      rtx rtl = DECL_RTL (decl);

      if (GET_CODE (rtl) == MEM && GET_CODE (XEXP (rtl, 0)) == SYMBOL_REF
	  && ! SYMBOL_REF_USED (XEXP (rtl, 0)))
	{
	  /* Some systems do require some output.  */
	  SYMBOL_REF_USED (XEXP (rtl, 0)) = 1;
	  ASM_OUTPUT_EXTERNAL (asm_out_file, decl, XSTR (XEXP (rtl, 0), 0));
	}
    }
#endif
}

/* Similar, for calling a library function FUN.  */

void
assemble_external_libcall (fun)
     rtx fun;
{
#ifdef ASM_OUTPUT_EXTERNAL_LIBCALL
  /* Declare library function name external when first used, if nec.  */
  if (! SYMBOL_REF_USED (fun))
    {
      SYMBOL_REF_USED (fun) = 1;
      ASM_OUTPUT_EXTERNAL_LIBCALL (asm_out_file, fun);
    }
#endif
}

/* Declare the label NAME global.  */

void
assemble_global (name)
     char *name;
{
  ASM_GLOBALIZE_LABEL (asm_out_file, name);
}

/* Assemble a label named NAME.  */

void
assemble_label (name)
     char *name;
{
  ASM_OUTPUT_LABEL (asm_out_file, name);
}

/* Output to FILE a reference to the assembler name of a C-level name NAME.
   If NAME starts with a *, the rest of NAME is output verbatim.
   Otherwise NAME is transformed in an implementation-defined way
   (usually by the addition of an underscore).
   Many macros in the tm file are defined to call this function.  */

void
assemble_name (file, name)
     FILE *file;
     char *name;
{
  if (name[0] == '*')
    fputs (&name[1], file);
  else
    ASM_OUTPUT_LABELREF (file, name);
}

/* Allocate SIZE bytes writable static space with a gensym name
   and return an RTX to refer to its address.  */

rtx
assemble_static_space (size)
     int size;
{
  char name[12];
  char *namestring;
  rtx x;
  /* Round size up to multiple of BIGGEST_ALIGNMENT bits
     so that each uninitialized object starts on such a boundary.  */
  int rounded = ((size + (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1)
		 / (BIGGEST_ALIGNMENT / BITS_PER_UNIT)
		 * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));

#if 0
  if (flag_shared_data)
    data_section ();
#endif

  ASM_GENERATE_INTERNAL_LABEL (name, "LF", const_labelno);
  ++const_labelno;

  namestring = (char *) obstack_alloc (saveable_obstack,
				       strlen (name) + 2);
  strcpy (namestring, name);

  x = gen_rtx (SYMBOL_REF, Pmode, namestring);
#ifdef ASM_OUTPUT_ALIGNED_LOCAL
  ASM_OUTPUT_ALIGNED_LOCAL (asm_out_file, name, size, BIGGEST_ALIGNMENT);
#else
  ASM_OUTPUT_LOCAL (asm_out_file, name, size, rounded);
#endif
  return x;
}

/* Assemble the static constant template for function entry trampolines.
   This is done at most once per compilation.
   Returns an RTX for the address of the template.  */

rtx
assemble_trampoline_template ()
{
  char label[256];
  char *name;
  int align;

  /* By default, put trampoline templates in read-only data section.  */

#ifdef TRAMPOLINE_SECTION
  TRAMPOLINE_SECTION ();
#else
  readonly_data_section ();
#endif

  /* Write the assembler code to define one.  */
  align = floor_log2 (FUNCTION_BOUNDARY / BITS_PER_UNIT);
  if (align > 0)
    ASM_OUTPUT_ALIGN (asm_out_file, align);

  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LTRAMP", 0);
  TRAMPOLINE_TEMPLATE (asm_out_file);

  /* Record the rtl to refer to it.  */
  ASM_GENERATE_INTERNAL_LABEL (label, "LTRAMP", 0);
  name
    = (char *) obstack_copy0 (&permanent_obstack, label, strlen (label));
  return gen_rtx (SYMBOL_REF, Pmode, name);
}

/* Assemble the integer constant X into an object of SIZE bytes.
   X must be either a CONST_INT or CONST_DOUBLE.

   Return 1 if we were able to output the constant, otherwise 0.  If FORCE is
   non-zero, abort if we can't output the constant.  */

int
assemble_integer (x, size, force)
     rtx x;
     int size;
     int force;
{
  /* First try to use the standard 1, 2, 4, 8, and 16 byte
     ASM_OUTPUT... macros. */

  switch (size)
    {
#ifdef ASM_OUTPUT_CHAR
    case 1:
      ASM_OUTPUT_CHAR (asm_out_file, x);
      return 1;
#endif

#ifdef ASM_OUTPUT_SHORT
    case 2:
      ASM_OUTPUT_SHORT (asm_out_file, x);
      return 1;
#endif

#ifdef ASM_OUTPUT_INT
    case 4:
      ASM_OUTPUT_INT (asm_out_file, x);
      return 1;
#endif

#ifdef ASM_OUTPUT_DOUBLE_INT
    case 8:
      ASM_OUTPUT_DOUBLE_INT (asm_out_file, x);
      return 1;
#endif

#ifdef ASM_OUTPUT_QUADRUPLE_INT
    case 16:
      ASM_OUTPUT_QUADRUPLE_INT (asm_out_file, x);
      return 1;
#endif
    }

  /* If we couldn't do it that way, there are two other possibilities: First,
     if the machine can output an explicit byte and this is a 1 byte constant,
     we can use ASM_OUTPUT_BYTE.  */

#ifdef ASM_OUTPUT_BYTE
  if (size == 1 && GET_CODE (x) == CONST_INT)
    {
      ASM_OUTPUT_BYTE (asm_out_file, INTVAL (x));
      return 1;
    }
#endif

  /* Finally, if SIZE is larger than a single word, try to output the constant
     one word at a time.  */

  if (size > UNITS_PER_WORD)
    {
      int i;
      enum machine_mode mode
	= mode_for_size (size * BITS_PER_UNIT, MODE_INT, 0);
      rtx word;

      for (i = 0; i < size / UNITS_PER_WORD; i++)
	{
	  word = operand_subword (x, i, 0, mode);

	  if (word == 0)
	    break;

	  if (! assemble_integer (word, UNITS_PER_WORD, 0))
	    break;
	}

      if (i == size / UNITS_PER_WORD)
	return 1;
      /* If we output at least one word and then could not finish,
	 there is no valid way to continue.  */
      if (i > 0)
	abort ();
    }

  if (force)
    abort ();

  return 0;
}

/* Assemble the floating-point constant D into an object of size MODE.  */

void
assemble_real (d, mode)
     REAL_VALUE_TYPE d;
     enum machine_mode mode;
{
  jmp_buf output_constant_handler;

  if (setjmp (output_constant_handler))
    {
      error ("floating point trap outputting a constant");
#ifdef REAL_IS_NOT_DOUBLE
      bzero (&d, sizeof d);
      d = dconst0;
#else
      d = 0;
#endif
    }

  set_float_handler (output_constant_handler);

  switch (mode)
    {
#ifdef ASM_OUTPUT_BYTE_FLOAT
    case QFmode:
      ASM_OUTPUT_BYTE_FLOAT (asm_out_file, d);
      break;
#endif
#ifdef ASM_OUTPUT_SHORT_FLOAT
    case HFmode:
      ASM_OUTPUT_SHORT_FLOAT (asm_out_file, d);
      break;
#endif
#ifdef ASM_OUTPUT_FLOAT
    case SFmode:
      ASM_OUTPUT_FLOAT (asm_out_file, d);
      break;
#endif

#ifdef ASM_OUTPUT_DOUBLE
    case DFmode:
      ASM_OUTPUT_DOUBLE (asm_out_file, d);
      break;
#endif

#ifdef ASM_OUTPUT_LONG_DOUBLE
    case XFmode:
    case TFmode:
      ASM_OUTPUT_LONG_DOUBLE (asm_out_file, d);
      break;
#endif

    default:
      abort ();
    }

  set_float_handler (NULL_PTR);
}

/* Here we combine duplicate floating constants to make
   CONST_DOUBLE rtx's, and force those out to memory when necessary.  */

/* Chain of all CONST_DOUBLE rtx's constructed for the current function.
   They are chained through the CONST_DOUBLE_CHAIN.
   A CONST_DOUBLE rtx has CONST_DOUBLE_MEM != cc0_rtx iff it is on this chain.
   In that case, CONST_DOUBLE_MEM is either a MEM,
   or const0_rtx if no MEM has been made for this CONST_DOUBLE yet.

   (CONST_DOUBLE_MEM is used only for top-level functions.
   See force_const_mem for explanation.)  */

static rtx const_double_chain;

/* Return a CONST_DOUBLE for a value specified as a pair of ints.
   For an integer, I0 is the low-order word and I1 is the high-order word.
   For a real number, I0 is the word with the low address
   and I1 is the word with the high address.  */

rtx
immed_double_const (i0, i1, mode)
     HOST_WIDE_INT i0, i1;
     enum machine_mode mode;
{
  register rtx r;
  int in_current_obstack;

  if (GET_MODE_CLASS (mode) == MODE_INT
      || GET_MODE_CLASS (mode) == MODE_PARTIAL_INT)
    {
      /* We clear out all bits that don't belong in MODE, unless they and our
	 sign bit are all one.  So we get either a reasonable negative value
	 or a reasonable unsigned value for this mode.  */
      int width = GET_MODE_BITSIZE (mode);
      if (width < HOST_BITS_PER_WIDE_INT
	  && ((i0 & ((HOST_WIDE_INT) (-1) << (width - 1)))
	      != ((HOST_WIDE_INT) (-1) << (width - 1))))
	i0 &= ((HOST_WIDE_INT) 1 << width) - 1, i1 = 0;
      else if (width == HOST_BITS_PER_WIDE_INT
	       && ! (i1 == ~0 && i0 < 0))
	i1 = 0;
      else if (width > 2 * HOST_BITS_PER_WIDE_INT)
	/* We cannot represent this value as a constant.  */
	abort ();

      /* If MODE fits within HOST_BITS_PER_WIDE_INT, always use a CONST_INT.

	 ??? Strictly speaking, this is wrong if we create a CONST_INT
	 for a large unsigned constant with the size of MODE being
	 HOST_BITS_PER_WIDE_INT and later try to interpret that constant in a
	 wider mode.  In that case we will mis-interpret it as a negative
	 number.

	 Unfortunately, the only alternative is to make a CONST_DOUBLE
	 for any constant in any mode if it is an unsigned constant larger
	 than the maximum signed integer in an int on the host.  However,
	 doing this will break everyone that always expects to see a CONST_INT
	 for SImode and smaller.

	 We have always been making CONST_INTs in this case, so nothing new
	 is being broken.  */

      if (width <= HOST_BITS_PER_WIDE_INT)
	i1 = (i0 < 0) ? ~0 : 0;

      /* If this integer fits in one word, return a CONST_INT.  */
      if ((i1 == 0 && i0 >= 0)
	  || (i1 == ~0 && i0 < 0))
	return GEN_INT (i0);

      /* We use VOIDmode for integers.  */
      mode = VOIDmode;
    }

  /* Search the chain for an existing CONST_DOUBLE with the right value.
     If one is found, return it.  */

  for (r = const_double_chain; r; r = CONST_DOUBLE_CHAIN (r))
    if (CONST_DOUBLE_LOW (r) == i0 && CONST_DOUBLE_HIGH (r) == i1
	&& GET_MODE (r) == mode)
      return r;

  /* No; make a new one and add it to the chain.

     We may be called by an optimizer which may be discarding any memory
     allocated during its processing (such as combine and loop).  However,
     we will be leaving this constant on the chain, so we cannot tolerate
     freed memory.  So switch to saveable_obstack for this allocation
     and then switch back if we were in current_obstack.  */

  push_obstacks_nochange ();
  rtl_in_saveable_obstack ();
  r = gen_rtx (CONST_DOUBLE, mode, 0, i0, i1);
  pop_obstacks ();

  /* Don't touch const_double_chain in nested function;
     see force_const_mem.  */
  if (outer_function_chain == 0)
    {
      CONST_DOUBLE_CHAIN (r) = const_double_chain;
      const_double_chain = r;
    }

  /* Store const0_rtx in mem-slot since this CONST_DOUBLE is on the chain.
     Actual use of mem-slot is only through force_const_mem.  */

  CONST_DOUBLE_MEM (r) = const0_rtx;

  return r;
}

/* Return a CONST_DOUBLE for a specified `double' value
   and machine mode.  */

rtx
immed_real_const_1 (d, mode)
     REAL_VALUE_TYPE d;
     enum machine_mode mode;
{
  union real_extract u;
  register rtx r;
  int in_current_obstack;

  /* Get the desired `double' value as a sequence of ints
     since that is how they are stored in a CONST_DOUBLE.  */

  u.d = d;

  /* Detect special cases.  */

  /* Avoid REAL_VALUES_EQUAL here in order to distinguish minus zero.  */
  if (!bcmp (&dconst0, &d, sizeof d))
    return CONST0_RTX (mode);
  else if (REAL_VALUES_EQUAL (dconst1, d))
    return CONST1_RTX (mode);

  if (sizeof u == 2 * sizeof (HOST_WIDE_INT))
    return immed_double_const (u.i[0], u.i[1], mode);

  /* The rest of this function handles the case where
     a float value requires more than 2 ints of space.
     It will be deleted as dead code on machines that don't need it.  */

  /* Search the chain for an existing CONST_DOUBLE with the right value.
     If one is found, return it.  */

  for (r = const_double_chain; r; r = CONST_DOUBLE_CHAIN (r))
    if (! bcmp (&CONST_DOUBLE_LOW (r), &u, sizeof u)
	&& GET_MODE (r) == mode)
      return r;

  /* No; make a new one and add it to the chain.

     We may be called by an optimizer which may be discarding any memory
     allocated during its processing (such as combine and loop).  However,
     we will be leaving this constant on the chain, so we cannot tolerate
     freed memory.  So switch to saveable_obstack for this allocation
     and then switch back if we were in current_obstack.  */

  push_obstacks_nochange ();
  rtl_in_saveable_obstack ();
  r = rtx_alloc (CONST_DOUBLE);
  PUT_MODE (r, mode);
  bcopy (&u, &CONST_DOUBLE_LOW (r), sizeof u);
  pop_obstacks ();

  /* Don't touch const_double_chain in nested function;
     see force_const_mem.  */
  if (outer_function_chain == 0)
    {
      CONST_DOUBLE_CHAIN (r) = const_double_chain;
      const_double_chain = r;
    }

  /* Store const0_rtx in CONST_DOUBLE_MEM since this CONST_DOUBLE is on the
     chain, but has not been allocated memory.  Actual use of CONST_DOUBLE_MEM
     is only through force_const_mem.  */

  CONST_DOUBLE_MEM (r) = const0_rtx;

  return r;
}

/* Return a CONST_DOUBLE rtx for a value specified by EXP,
   which must be a REAL_CST tree node.  */

rtx
immed_real_const (exp)
     tree exp;
{
  return immed_real_const_1 (TREE_REAL_CST (exp), TYPE_MODE (TREE_TYPE (exp)));
}

/* At the end of a function, forget the memory-constants
   previously made for CONST_DOUBLEs.  Mark them as not on real_constant_chain.
   Also clear out real_constant_chain and clear out all the chain-pointers.  */

void
clear_const_double_mem ()
{
  register rtx r, next;

  /* Don't touch CONST_DOUBLE_MEM for nested functions.
     See force_const_mem for explanation.  */
  if (outer_function_chain != 0)
    return;

  for (r = const_double_chain; r; r = next)
    {
      next = CONST_DOUBLE_CHAIN (r);
      CONST_DOUBLE_CHAIN (r) = 0;
      CONST_DOUBLE_MEM (r) = cc0_rtx;
    }
  const_double_chain = 0;
}

/* Given an expression EXP with a constant value,
   reduce it to the sum of an assembler symbol and an integer.
   Store them both in the structure *VALUE.
   Abort if EXP does not reduce.  */

struct addr_const
{
  rtx base;
  HOST_WIDE_INT offset;
};

static void
decode_addr_const (exp, value)
     tree exp;
     struct addr_const *value;
{
  register tree target = TREE_OPERAND (exp, 0);
  register int offset = 0;
  register rtx x;

  while (1)
    {
      if (TREE_CODE (target) == COMPONENT_REF
	  && (TREE_CODE (DECL_FIELD_BITPOS (TREE_OPERAND (target, 1)))
	      == INTEGER_CST))
	{
	  offset += TREE_INT_CST_LOW (DECL_FIELD_BITPOS (TREE_OPERAND (target, 1))) / BITS_PER_UNIT;
	  target = TREE_OPERAND (target, 0);
	}
      else if (TREE_CODE (target) == ARRAY_REF)
	{
	  if (TREE_CODE (TREE_OPERAND (target, 1)) != INTEGER_CST
	      || TREE_CODE (TYPE_SIZE (TREE_TYPE (target))) != INTEGER_CST)
	    abort ();
	  offset += ((TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (target)))
		      * TREE_INT_CST_LOW (TREE_OPERAND (target, 1)))
		     / BITS_PER_UNIT);
	  target = TREE_OPERAND (target, 0);
	}
      else
	break;
    }

  switch (TREE_CODE (target))
    {
    case VAR_DECL:
    case FUNCTION_DECL:
      x = DECL_RTL (target);
      break;

    case LABEL_DECL:
      x = gen_rtx (MEM, FUNCTION_MODE,
		   gen_rtx (LABEL_REF, VOIDmode,
			    label_rtx (TREE_OPERAND (exp, 0))));
      break;

    case REAL_CST:
    case STRING_CST:
    case COMPLEX_CST:
    case CONSTRUCTOR:
      x = TREE_CST_RTL (target);
      break;

    default:
      abort ();
    }

  if (GET_CODE (x) != MEM)
    abort ();
  x = XEXP (x, 0);

  value->base = x;
  value->offset = offset;
}

/* Uniquize all constants that appear in memory.
   Each constant in memory thus far output is recorded
   in `const_hash_table' with a `struct constant_descriptor'
   that contains a polish representation of the value of
   the constant.

   We cannot store the trees in the hash table
   because the trees may be temporary.  */

struct constant_descriptor
{
  struct constant_descriptor *next;
  char *label;
  char contents[1];
};

#define HASHBITS 30
#define MAX_HASH_TABLE 1009
static struct constant_descriptor *const_hash_table[MAX_HASH_TABLE];

/* Compute a hash code for a constant expression.  */

int
const_hash (exp)
     tree exp;
{
  register char *p;
  register int len, hi, i;
  register enum tree_code code = TREE_CODE (exp);

  if (code == INTEGER_CST)
    {
      p = (char *) &TREE_INT_CST_LOW (exp);
      len = 2 * sizeof TREE_INT_CST_LOW (exp);
    }
  else if (code == REAL_CST)
    {
      p = (char *) &TREE_REAL_CST (exp);
      len = sizeof TREE_REAL_CST (exp);
    }
  else if (code == STRING_CST)
    p = TREE_STRING_POINTER (exp), len = TREE_STRING_LENGTH (exp);
  else if (code == COMPLEX_CST)
    return const_hash (TREE_REALPART (exp)) * 5
      + const_hash (TREE_IMAGPART (exp));
  else if (code == CONSTRUCTOR)
    {
      register tree link;

      /* For record type, include the type in the hashing.
	 We do not do so for array types
	 because (1) the sizes of the elements are sufficient
	 and (2) distinct array types can have the same constructor.
	 Instead, we include the array size because the constructor could
	 be shorter.  */
      if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE)
	hi = ((HOST_WIDE_INT) TREE_TYPE (exp) & ((1 << HASHBITS) - 1))
	  % MAX_HASH_TABLE;
      else
	hi = ((5 + int_size_in_bytes (TREE_TYPE (exp)))
	       & ((1 << HASHBITS) - 1)) % MAX_HASH_TABLE;

      for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	if (TREE_VALUE (link))
	  hi = (hi * 603 + const_hash (TREE_VALUE (link))) % MAX_HASH_TABLE;

      return hi;
    }
  else if (code == ADDR_EXPR)
    {
      struct addr_const value;
      decode_addr_const (exp, &value);
      if (GET_CODE (value.base) == SYMBOL_REF)
	{
	  /* Don't hash the address of the SYMBOL_REF;
	     only use the offset and the symbol name.  */
	  hi = value.offset;
	  p = XSTR (value.base, 0);
	  for (i = 0; p[i] != 0; i++)
	    hi = ((hi * 613) + (unsigned)(p[i]));
	}
      else if (GET_CODE (value.base) == LABEL_REF)
	hi = value.offset + CODE_LABEL_NUMBER (XEXP (value.base, 0)) * 13;

      hi &= (1 << HASHBITS) - 1;
      hi %= MAX_HASH_TABLE;
      return hi;
    }
  else if (code == PLUS_EXPR || code == MINUS_EXPR)
    return const_hash (TREE_OPERAND (exp, 0)) * 9
      +  const_hash (TREE_OPERAND (exp, 1));
  else if (code == NOP_EXPR || code == CONVERT_EXPR)
    return const_hash (TREE_OPERAND (exp, 0)) * 7 + 2;

  /* Compute hashing function */
  hi = len;
  for (i = 0; i < len; i++)
    hi = ((hi * 613) + (unsigned)(p[i]));

  hi &= (1 << HASHBITS) - 1;
  hi %= MAX_HASH_TABLE;
  return hi;
}

/* Compare a constant expression EXP with a constant-descriptor DESC.
   Return 1 if DESC describes a constant with the same value as EXP.  */

static int
compare_constant (exp, desc)
     tree exp;
     struct constant_descriptor *desc;
{
  return 0 != compare_constant_1 (exp, desc->contents);
}

/* Compare constant expression EXP with a substring P of a constant descriptor.
   If they match, return a pointer to the end of the substring matched.
   If they do not match, return 0.

   Since descriptors are written in polish prefix notation,
   this function can be used recursively to test one operand of EXP
   against a subdescriptor, and if it succeeds it returns the
   address of the subdescriptor for the next operand.  */

static char *
compare_constant_1 (exp, p)
     tree exp;
     char *p;
{
  register char *strp;
  register int len;
  register enum tree_code code = TREE_CODE (exp);

  if (code != (enum tree_code) *p++)
    return 0;

  if (code == INTEGER_CST)
    {
      /* Integer constants are the same only if the same width of type.  */
      if (*p++ != TYPE_PRECISION (TREE_TYPE (exp)))
	return 0;
      strp = (char *) &TREE_INT_CST_LOW (exp);
      len = 2 * sizeof TREE_INT_CST_LOW (exp);
    }
  else if (code == REAL_CST)
    {
      /* Real constants are the same only if the same width of type.  */
      if (*p++ != TYPE_PRECISION (TREE_TYPE (exp)))
	return 0;
      strp = (char *) &TREE_REAL_CST (exp);
      len = sizeof TREE_REAL_CST (exp);
    }
  else if (code == STRING_CST)
    {
      if (flag_writable_strings)
	return 0;
      strp = TREE_STRING_POINTER (exp);
      len = TREE_STRING_LENGTH (exp);
      if (bcmp (&TREE_STRING_LENGTH (exp), p,
		sizeof TREE_STRING_LENGTH (exp)))
	return 0;
      p += sizeof TREE_STRING_LENGTH (exp);
    }
  else if (code == COMPLEX_CST)
    {
      p = compare_constant_1 (TREE_REALPART (exp), p);
      if (p == 0) return 0;
      p = compare_constant_1 (TREE_IMAGPART (exp), p);
      return p;
    }
  else if (code == CONSTRUCTOR)
    {
      register tree link;
      int length = list_length (CONSTRUCTOR_ELTS (exp));
      tree type;

      if (bcmp (&length, p, sizeof length))
	return 0;
      p += sizeof length;

      /* For record constructors, insist that the types match.
	 For arrays, just verify both constructors are for arrays.  */
      if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE)
	type = TREE_TYPE (exp);
      else
	type = 0;
      if (bcmp (&type, p, sizeof type))
	return 0;
      p += sizeof type;

      /* For arrays, insist that the size in bytes match.  */
      if (TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE)
	{
	  int size = int_size_in_bytes (TREE_TYPE (exp));
	  if (bcmp (&size, p, sizeof size))
	    return 0;
	  p += sizeof size;
	}

      for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	{
	  if (TREE_VALUE (link))
	    {
	      if ((p = compare_constant_1 (TREE_VALUE (link), p)) == 0)
		return 0;
	    }
	  else
	    {
	      tree zero = 0;

	      if (bcmp (&zero, p, sizeof zero))
		return 0;
	      p += sizeof zero;
	    }
	}

      return p;
    }
  else if (code == ADDR_EXPR)
    {
      struct addr_const value;
      decode_addr_const (exp, &value);
      strp = (char *) &value.offset;
      len = sizeof value.offset;
      /* Compare the offset.  */
      while (--len >= 0)
	if (*p++ != *strp++)
	  return 0;
      /* Compare symbol name.  */
      strp = XSTR (value.base, 0);
      len = strlen (strp) + 1;
    }
  else if (code == PLUS_EXPR || code == MINUS_EXPR)
    {
      p = compare_constant_1 (TREE_OPERAND (exp, 0), p);
      if (p == 0) return 0;
      p = compare_constant_1 (TREE_OPERAND (exp, 1), p);
      return p;
    }
  else if (code == NOP_EXPR || code == CONVERT_EXPR)
    {
      p = compare_constant_1 (TREE_OPERAND (exp, 0), p);
      return p;
    }

  /* Compare constant contents.  */
  while (--len >= 0)
    if (*p++ != *strp++)
      return 0;

  return p;
}

/* Construct a constant descriptor for the expression EXP.
   It is up to the caller to enter the descriptor in the hash table.  */

static struct constant_descriptor *
record_constant (exp)
     tree exp;
{
  struct constant_descriptor *next = 0;
  char *label = 0;

  /* Make a struct constant_descriptor.  The first two pointers will
     be filled in later.  Here we just leave space for them.  */

  obstack_grow (&permanent_obstack, (char *) &next, sizeof next);
  obstack_grow (&permanent_obstack, (char *) &label, sizeof label);
  record_constant_1 (exp);
  return (struct constant_descriptor *) obstack_finish (&permanent_obstack);
}

/* Add a description of constant expression EXP
   to the object growing in `permanent_obstack'.
   No need to return its address; the caller will get that
   from the obstack when the object is complete.  */

static void
record_constant_1 (exp)
     tree exp;
{
  register char *strp;
  register int len;
  register enum tree_code code = TREE_CODE (exp);

  obstack_1grow (&permanent_obstack, (unsigned int) code);

  if (code == INTEGER_CST)
    {
      obstack_1grow (&permanent_obstack, TYPE_PRECISION (TREE_TYPE (exp)));
      strp = (char *) &TREE_INT_CST_LOW (exp);
      len = 2 * sizeof TREE_INT_CST_LOW (exp);
    }
  else if (code == REAL_CST)
    {
      obstack_1grow (&permanent_obstack, TYPE_PRECISION (TREE_TYPE (exp)));
      strp = (char *) &TREE_REAL_CST (exp);
      len = sizeof TREE_REAL_CST (exp);
    }
  else if (code == STRING_CST)
    {
      if (flag_writable_strings)
	return;
      strp = TREE_STRING_POINTER (exp);
      len = TREE_STRING_LENGTH (exp);
      obstack_grow (&permanent_obstack, (char *) &TREE_STRING_LENGTH (exp),
		    sizeof TREE_STRING_LENGTH (exp));
    }
  else if (code == COMPLEX_CST)
    {
      record_constant_1 (TREE_REALPART (exp));
      record_constant_1 (TREE_IMAGPART (exp));
      return;
    }
  else if (code == CONSTRUCTOR)
    {
      register tree link;
      int length = list_length (CONSTRUCTOR_ELTS (exp));
      tree type;

      obstack_grow (&permanent_obstack, (char *) &length, sizeof length);

      /* For record constructors, insist that the types match.
	 For arrays, just verify both constructors are for arrays.  */
      if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE)
	type = TREE_TYPE (exp);
      else
	type = 0;
      obstack_grow (&permanent_obstack, (char *) &type, sizeof type);

      /* For arrays, insist that the size in bytes match.  */
      if (TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE)
	{
	  int size = int_size_in_bytes (TREE_TYPE (exp));
	  obstack_grow (&permanent_obstack, (char *) &size, sizeof size);
	}

      for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	{
	  if (TREE_VALUE (link))
	    record_constant_1 (TREE_VALUE (link));
	  else
	    {
	      tree zero = 0;

	      obstack_grow (&permanent_obstack, (char *) &zero, sizeof zero);
	    }
	}

      return;
    }
  else if (code == ADDR_EXPR)
    {
      struct addr_const value;
      decode_addr_const (exp, &value);
      /* Record the offset.  */
      obstack_grow (&permanent_obstack,
		    (char *) &value.offset, sizeof value.offset);
      /* Record the symbol name.  */
      obstack_grow (&permanent_obstack, XSTR (value.base, 0),
		    strlen (XSTR (value.base, 0)) + 1);
      return;
    }
  else if (code == PLUS_EXPR || code == MINUS_EXPR)
    {
      record_constant_1 (TREE_OPERAND (exp, 0));
      record_constant_1 (TREE_OPERAND (exp, 1));
      return;
    }
  else if (code == NOP_EXPR || code == CONVERT_EXPR)
    {
      record_constant_1 (TREE_OPERAND (exp, 0));
      return;
    }

  /* Record constant contents.  */
  obstack_grow (&permanent_obstack, strp, len);
}

/* Return an rtx representing a reference to constant data in memory
   for the constant expression EXP.
   If assembler code for such a constant has already been output,
   return an rtx to refer to it.
   Otherwise, output such a constant in memory and generate
   an rtx for it.  The TREE_CST_RTL of EXP is set up to point to that rtx.
   The const_hash_table records which constants already have label strings.  */

rtx
output_constant_def (exp)
     tree exp;
{
  register int hash, align;
  register struct constant_descriptor *desc;
  char label[256];
  char *found = 0;
  int reloc;
  register rtx def;

  if (TREE_CODE (exp) == INTEGER_CST)
    abort ();			/* No TREE_CST_RTL slot in these.  */

  if (TREE_CST_RTL (exp))
    return TREE_CST_RTL (exp);

  /* Make sure any other constants whose addresses appear in EXP
     are assigned label numbers.  */

  reloc = output_addressed_constants (exp);

  /* Compute hash code of EXP.  Search the descriptors for that hash code
     to see if any of them describes EXP.  If yes, the descriptor records
     the label number already assigned.  */

  hash = const_hash (exp) % MAX_HASH_TABLE;

  for (desc = const_hash_table[hash]; desc; desc = desc->next)
    if (compare_constant (exp, desc))
      {
	found = desc->label;
	break;
      }

  if (found == 0)
    {
      /* No constant equal to EXP is known to have been output.
	 Make a constant descriptor to enter EXP in the hash table.
	 Assign the label number and record it in the descriptor for
	 future calls to this function to find.  */

      /* Create a string containing the label name, in LABEL.  */
      ASM_GENERATE_INTERNAL_LABEL (label, "LC", const_labelno);

      desc = record_constant (exp);
      desc->next = const_hash_table[hash];
      desc->label
	= (char *) obstack_copy0 (&permanent_obstack, label, strlen (label));
      const_hash_table[hash] = desc;
    }

  /* We have a symbol name; construct the SYMBOL_REF and the MEM.  */

  push_obstacks_nochange ();
  if (TREE_PERMANENT (exp))
    end_temporary_allocation ();

  def = gen_rtx (SYMBOL_REF, Pmode, desc->label);

  TREE_CST_RTL (exp)
    = gen_rtx (MEM, TYPE_MODE (TREE_TYPE (exp)), def);
  RTX_UNCHANGING_P (TREE_CST_RTL (exp)) = 1;
  if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE
      || TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE)
    MEM_IN_STRUCT_P (TREE_CST_RTL (exp)) = 1;

  pop_obstacks ();

  /* Optionally set flags or add text to the name to record information
     such as that it is a function name.  If the name is changed, the macro
     ASM_OUTPUT_LABELREF will have to know how to strip this information.
     And if it finds a * at the beginning after doing so, it must handle
     that too.  */
#ifdef ENCODE_SECTION_INFO
  ENCODE_SECTION_INFO (exp);
#endif

  if (found == 0)
    {
      /* Now output assembler code to define that label
	 and follow it with the data of EXP.  */

      /* First switch to text section, except for writable strings.  */
#ifdef SELECT_SECTION
      SELECT_SECTION (exp, reloc);
#else
      if (((TREE_CODE (exp) == STRING_CST) && flag_writable_strings)
	  || (flag_pic && reloc))
	data_section ();
      else
	readonly_data_section ();
#endif

      /* Align the location counter as required by EXP's data type.  */
      align = TYPE_ALIGN (TREE_TYPE (exp));
#ifdef CONSTANT_ALIGNMENT
      align = CONSTANT_ALIGNMENT (exp, align);
#endif

      if (align > BITS_PER_UNIT)
	ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (align / BITS_PER_UNIT));

      /* Output the label itself.  */
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LC", const_labelno);

      /* Output the value of EXP.  */
      output_constant (exp,
		       (TREE_CODE (exp) == STRING_CST
			? TREE_STRING_LENGTH (exp)
			: int_size_in_bytes (TREE_TYPE (exp))));

      ++const_labelno;
    }

  return TREE_CST_RTL (exp);
}

/* Similar hash facility for making memory-constants
   from constant rtl-expressions.  It is used on RISC machines
   where immediate integer arguments and constant addresses are restricted
   so that such constants must be stored in memory.

   This pool of constants is reinitialized for each function
   so each function gets its own constants-pool that comes right before it.

   All structures allocated here are discarded when functions are saved for
   inlining, so they do not need to be allocated permanently.  */

#define MAX_RTX_HASH_TABLE 61
static struct constant_descriptor **const_rtx_hash_table;

/* Structure to represent sufficient information about a constant so that
   it can be output when the constant pool is output, so that function
   integration can be done, and to simplify handling on machines that reference
   constant pool as base+displacement.  */

struct pool_constant
{
  struct constant_descriptor *desc;
  struct pool_constant *next;
  enum machine_mode mode;
  rtx constant;
  int labelno;
  int align;
  int offset;
};

/* Pointers to first and last constant in pool.  */

static struct pool_constant *first_pool, *last_pool;

/* Current offset in constant pool (does not include any machine-specific
   header.  */

static int pool_offset;

/* Structure used to maintain hash table mapping symbols used to their
   corresponding constants.  */

struct pool_sym
{
  char *label;
  struct pool_constant *pool;
  struct pool_sym *next;
};

static struct pool_sym **const_rtx_sym_hash_table;

/* Hash code for a SYMBOL_REF with CONSTANT_POOL_ADDRESS_P true.
   The argument is XSTR (... , 0)  */

#define SYMHASH(LABEL)	\
  ((((HOST_WIDE_INT) (LABEL)) & ((1 << HASHBITS) - 1))  % MAX_RTX_HASH_TABLE)

/* Initialize constant pool hashing for next function.  */

void
init_const_rtx_hash_table ()
{
  const_rtx_hash_table
    = ((struct constant_descriptor **)
       oballoc (MAX_RTX_HASH_TABLE * sizeof (struct constant_descriptor *)));
  const_rtx_sym_hash_table
    = ((struct pool_sym **)
       oballoc (MAX_RTX_HASH_TABLE * sizeof (struct pool_sym *)));
  bzero (const_rtx_hash_table,
	 MAX_RTX_HASH_TABLE * sizeof (struct constant_descriptor *));
  bzero (const_rtx_sym_hash_table,
	 MAX_RTX_HASH_TABLE * sizeof (struct pool_sym *));

  first_pool = last_pool = 0;
  pool_offset = 0;
}

/* Save and restore it for a nested function.  */

void
save_varasm_status (p)
     struct function *p;
{
  p->const_rtx_hash_table = const_rtx_hash_table;
  p->const_rtx_sym_hash_table = const_rtx_sym_hash_table;
  p->first_pool = first_pool;
  p->last_pool = last_pool;
  p->pool_offset = pool_offset;
}

void
restore_varasm_status (p)
     struct function *p;
{
  const_rtx_hash_table = p->const_rtx_hash_table;
  const_rtx_sym_hash_table = p->const_rtx_sym_hash_table;
  first_pool = p->first_pool;
  last_pool = p->last_pool;
  pool_offset = p->pool_offset;
}

enum kind { RTX_DOUBLE, RTX_INT };

struct rtx_const
{
#ifdef ONLY_INT_FIELDS
  unsigned int kind : 16;
  unsigned int mode : 16;
#else
  enum kind kind : 16;
  enum machine_mode mode : 16;
#endif
  union {
    union real_extract du;
    struct addr_const addr;
  } un;
};

/* Express an rtx for a constant integer (perhaps symbolic)
   as the sum of a symbol or label plus an explicit integer.
   They are stored into VALUE.  */

static void
decode_rtx_const (mode, x, value)
     enum machine_mode mode;
     rtx x;
     struct rtx_const *value;
{
  /* Clear the whole structure, including any gaps.  */

  {
    int *p = (int *) value;
    int *end = (int *) (value + 1);
    while (p < end)
      *p++ = 0;
  }

  value->kind = RTX_INT;	/* Most usual kind. */
  value->mode = mode;

  switch (GET_CODE (x))
    {
    case CONST_DOUBLE:
      value->kind = RTX_DOUBLE;
      value->mode = GET_MODE (x);
      bcopy (&CONST_DOUBLE_LOW (x), &value->un.du, sizeof value->un.du);
      break;

    case CONST_INT:
      value->un.addr.offset = INTVAL (x);
      break;

    case SYMBOL_REF:
    case LABEL_REF:
    case PC:
      value->un.addr.base = x;
      break;

    case CONST:
      x = XEXP (x, 0);
      if (GET_CODE (x) == PLUS)
	{
	  value->un.addr.base = XEXP (x, 0);
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	    abort ();
	  value->un.addr.offset = INTVAL (XEXP (x, 1));
	}
      else if (GET_CODE (x) == MINUS)
	{
	  value->un.addr.base = XEXP (x, 0);
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	    abort ();
	  value->un.addr.offset = - INTVAL (XEXP (x, 1));
	}
      else
	abort ();
      break;

    default:
      abort ();
    }

  if (value->kind == RTX_INT && value->un.addr.base != 0)
    switch (GET_CODE (value->un.addr.base))
      {
      case SYMBOL_REF:
      case LABEL_REF:
	/* Use the string's address, not the SYMBOL_REF's address,
	   for the sake of addresses of library routines.
	   For a LABEL_REF, compare labels.  */
	value->un.addr.base = XEXP (value->un.addr.base, 0);
      }
}

/* Given a MINUS expression, simplify it if both sides
   include the same symbol.  */

rtx
simplify_subtraction (x)
     rtx x;
{
  struct rtx_const val0, val1;

  decode_rtx_const (GET_MODE (x), XEXP (x, 0), &val0);
  decode_rtx_const (GET_MODE (x), XEXP (x, 1), &val1);

  if (val0.un.addr.base == val1.un.addr.base)
    return GEN_INT (val0.un.addr.offset - val1.un.addr.offset);
  return x;
}

/* Compute a hash code for a constant RTL expression.  */

int
const_hash_rtx (mode, x)
     enum machine_mode mode;
     rtx x;
{
  register int hi, i;

  struct rtx_const value;
  decode_rtx_const (mode, x, &value);

  /* Compute hashing function */
  hi = 0;
  for (i = 0; i < sizeof value / sizeof (int); i++)
    hi += ((int *) &value)[i];

  hi &= (1 << HASHBITS) - 1;
  hi %= MAX_RTX_HASH_TABLE;
  return hi;
}

/* Compare a constant rtl object X with a constant-descriptor DESC.
   Return 1 if DESC describes a constant with the same value as X.  */

static int
compare_constant_rtx (mode, x, desc)
     enum machine_mode mode;
     rtx x;
     struct constant_descriptor *desc;
{
  register int *p = (int *) desc->contents;
  register int *strp;
  register int len;
  struct rtx_const value;

  decode_rtx_const (mode, x, &value);
  strp = (int *) &value;
  len = sizeof value / sizeof (int);

  /* Compare constant contents.  */
  while (--len >= 0)
    if (*p++ != *strp++)
      return 0;

  return 1;
}

/* Construct a constant descriptor for the rtl-expression X.
   It is up to the caller to enter the descriptor in the hash table.  */

static struct constant_descriptor *
record_constant_rtx (mode, x)
     enum machine_mode mode;
     rtx x;
{
  struct constant_descriptor *ptr;
  char *label;
  struct rtx_const value;

  decode_rtx_const (mode, x, &value);

  obstack_grow (current_obstack, &ptr, sizeof ptr);
  obstack_grow (current_obstack, &label, sizeof label);

  /* Record constant contents.  */
  obstack_grow (current_obstack, &value, sizeof value);

  return (struct constant_descriptor *) obstack_finish (current_obstack);
}

/* Given a constant rtx X, make (or find) a memory constant for its value
   and return a MEM rtx to refer to it in memory.  */

rtx
force_const_mem (mode, x)
     enum machine_mode mode;
     rtx x;
{
  register int hash;
  register struct constant_descriptor *desc;
  char label[256];
  char *found = 0;
  rtx def;

  /* If we want this CONST_DOUBLE in the same mode as it is in memory
     (this will always be true for floating CONST_DOUBLEs that have been
     placed in memory, but not for VOIDmode (integer) CONST_DOUBLEs),
     use the previous copy.  Otherwise, make a new one.  Note that in
     the unlikely event that this same CONST_DOUBLE is used in two different
     modes in an alternating fashion, we will allocate a lot of different
     memory locations, but this should be extremely rare.  */

  /* Don't use CONST_DOUBLE_MEM in a nested function.
     Nested functions have their own constant pools,
     so they can't share the same values in CONST_DOUBLE_MEM
     with the containing function.  */
  if (outer_function_chain == 0)
    if (GET_CODE (x) == CONST_DOUBLE
	&& GET_CODE (CONST_DOUBLE_MEM (x)) == MEM
	&& GET_MODE (CONST_DOUBLE_MEM (x)) == mode)
      return CONST_DOUBLE_MEM (x);

  /* Compute hash code of X.  Search the descriptors for that hash code
     to see if any of them describes X.  If yes, the descriptor records
     the label number already assigned.  */

  hash = const_hash_rtx (mode, x);

  for (desc = const_rtx_hash_table[hash]; desc; desc = desc->next)
    if (compare_constant_rtx (mode, x, desc))
      {
	found = desc->label;
	break;
      }

  if (found == 0)
    {
      register struct pool_constant *pool;
      register struct pool_sym *sym;
      int align;

      /* No constant equal to X is known to have been output.
	 Make a constant descriptor to enter X in the hash table.
	 Assign the label number and record it in the descriptor for
	 future calls to this function to find.  */

      desc = record_constant_rtx (mode, x);
      desc->next = const_rtx_hash_table[hash];
      const_rtx_hash_table[hash] = desc;

      /* Align the location counter as required by EXP's data type.  */
      align = (mode == VOIDmode) ? UNITS_PER_WORD : GET_MODE_SIZE (mode);
      if (align > BIGGEST_ALIGNMENT / BITS_PER_UNIT)
	align = BIGGEST_ALIGNMENT / BITS_PER_UNIT;

      pool_offset += align - 1;
      pool_offset &= ~ (align - 1);

      /* Allocate a pool constant descriptor, fill it in, and chain it in.  */

      pool = (struct pool_constant *) oballoc (sizeof (struct pool_constant));
      pool->desc = desc;
      pool->constant = x;
      pool->mode = mode;
      pool->labelno = const_labelno;
      pool->align = align;
      pool->offset = pool_offset;
      pool->next = 0;

      if (last_pool == 0)
	first_pool = pool;
      else
	last_pool->next = pool;

      last_pool = pool;
      pool_offset += GET_MODE_SIZE (mode);

      /* Create a string containing the label name, in LABEL.  */
      ASM_GENERATE_INTERNAL_LABEL (label, "LC", const_labelno);

      ++const_labelno;

      desc->label = found
	= (char *) obstack_copy0 (saveable_obstack, label, strlen (label));

      /* Add label to symbol hash table.  */
      hash = SYMHASH (found);
      sym = (struct pool_sym *) oballoc (sizeof (struct pool_sym));
      sym->label = found;
      sym->pool = pool;
      sym->next = const_rtx_sym_hash_table[hash];
      const_rtx_sym_hash_table[hash] = sym;
    }

  /* We have a symbol name; construct the SYMBOL_REF and the MEM.  */

  def = gen_rtx (MEM, mode, gen_rtx (SYMBOL_REF, Pmode, found));

  RTX_UNCHANGING_P (def) = 1;
  /* Mark the symbol_ref as belonging to this constants pool.  */
  CONSTANT_POOL_ADDRESS_P (XEXP (def, 0)) = 1;
  current_function_uses_const_pool = 1;

  if (outer_function_chain == 0)
    if (GET_CODE (x) == CONST_DOUBLE)
      {
	if (CONST_DOUBLE_MEM (x) == cc0_rtx)
	  {
	    CONST_DOUBLE_CHAIN (x) = const_double_chain;
	    const_double_chain = x;
	  }
	CONST_DOUBLE_MEM (x) = def;
      }

  return def;
}

/* Given a SYMBOL_REF with CONSTANT_POOL_ADDRESS_P true, return a pointer to
   the corresponding pool_constant structure.  */

static struct pool_constant *
find_pool_constant (addr)
     rtx addr;
{
  struct pool_sym *sym;
  char *label = XSTR (addr, 0);

  for (sym = const_rtx_sym_hash_table[SYMHASH (label)]; sym; sym = sym->next)
    if (sym->label == label)
      return sym->pool;

  abort ();
}

/* Given a constant pool SYMBOL_REF, return the corresponding constant.  */

rtx
get_pool_constant (addr)
     rtx addr;
{
  return (find_pool_constant (addr))->constant;
}

/* Similar, return the mode.  */

enum machine_mode
get_pool_mode (addr)
     rtx addr;
{
  return (find_pool_constant (addr))->mode;
}

/* Similar, return the offset in the constant pool.  */

int
get_pool_offset (addr)
     rtx addr;
{
  return (find_pool_constant (addr))->offset;
}

/* Return the size of the constant pool.  */

int
get_pool_size ()
{
  return pool_offset;
}

/* Write all the constants in the constant pool.  */

void
output_constant_pool (fnname, fndecl)
     char *fnname;
     tree fndecl;
{
  struct pool_constant *pool;
  rtx x;
  union real_extract u;

#ifdef ASM_OUTPUT_POOL_PROLOGUE
  ASM_OUTPUT_POOL_PROLOGUE (asm_out_file, fnname, fndecl, pool_offset);
#endif

  for (pool = first_pool; pool; pool = pool->next)
    {
      x = pool->constant;

      /* See if X is a LABEL_REF (or a CONST referring to a LABEL_REF)
	 whose CODE_LABEL has been deleted.  This can occur if a jump table
	 is eliminated by optimization.  If so, write a constant of zero
	 instead.  Note that this can also happen by turning the
	 CODE_LABEL into a NOTE.  */
      if (((GET_CODE (x) == LABEL_REF
	    && (INSN_DELETED_P (XEXP (x, 0))
		|| GET_CODE (XEXP (x, 0)) == NOTE)))
	  || (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == LABEL_REF
	      && (INSN_DELETED_P (XEXP (XEXP (XEXP (x, 0), 0), 0))
		  || GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == NOTE)))
	x = const0_rtx;

      /* First switch to correct section.  */
#ifdef SELECT_RTX_SECTION
      SELECT_RTX_SECTION (pool->mode, x);
#else
      readonly_data_section ();
#endif

#ifdef ASM_OUTPUT_SPECIAL_POOL_ENTRY
      ASM_OUTPUT_SPECIAL_POOL_ENTRY (asm_out_file, x, pool->mode,
				     pool->align, pool->labelno, done);
#endif

      if (pool->align > 1)
	ASM_OUTPUT_ALIGN (asm_out_file, exact_log2 (pool->align));

      /* Output the label.  */
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LC", pool->labelno);

      /* Output the value of the constant itself.  */
      switch (GET_MODE_CLASS (pool->mode))
	{
	case MODE_FLOAT:
	  if (GET_CODE (x) != CONST_DOUBLE)
	    abort ();

	  bcopy (&CONST_DOUBLE_LOW (x), &u, sizeof u);
	  assemble_real (u.d, pool->mode);
	  break;

	case MODE_INT:
	case MODE_PARTIAL_INT:
	  assemble_integer (x, GET_MODE_SIZE (pool->mode), 1);
	  break;

	default:
	  abort ();
	}

    done: ;
    }

  /* Done with this pool.  */
  first_pool = last_pool = 0;
}

/* Find all the constants whose addresses are referenced inside of EXP,
   and make sure assembler code with a label has been output for each one.
   Indicate whether an ADDR_EXPR has been encountered.  */

int
output_addressed_constants (exp)
     tree exp;
{
  int reloc = 0;

  switch (TREE_CODE (exp))
    {
    case ADDR_EXPR:
      {
	register tree constant = TREE_OPERAND (exp, 0);

	while (TREE_CODE (constant) == COMPONENT_REF)
	  {
	    constant = TREE_OPERAND (constant, 0);
	  }

	if (TREE_CODE_CLASS (TREE_CODE (constant)) == 'c'
	    || TREE_CODE (constant) == CONSTRUCTOR)
	  /* No need to do anything here
	     for addresses of variables or functions.  */
	  output_constant_def (constant);
      }
      reloc = 1;
      break;

    case PLUS_EXPR:
    case MINUS_EXPR:
      reloc = output_addressed_constants (TREE_OPERAND (exp, 0));
      reloc |= output_addressed_constants (TREE_OPERAND (exp, 1));
      break;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      reloc = output_addressed_constants (TREE_OPERAND (exp, 0));
      break;

    case CONSTRUCTOR:
      {
	register tree link;
	for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	  if (TREE_VALUE (link) != 0)
	    reloc |= output_addressed_constants (TREE_VALUE (link));
      }
      break;

    case ERROR_MARK:
      break;
    }
  return reloc;
}

/* Output assembler code for constant EXP to FILE, with no label.
   This includes the pseudo-op such as ".int" or ".byte", and a newline.
   Assumes output_addressed_constants has been done on EXP already.

   Generate exactly SIZE bytes of assembler data, padding at the end
   with zeros if necessary.  SIZE must always be specified.

   SIZE is important for structure constructors,
   since trailing members may have been omitted from the constructor.
   It is also important for initialization of arrays from string constants
   since the full length of the string constant might not be wanted.
   It is also needed for initialization of unions, where the initializer's
   type is just one member, and that may not be as long as the union.

   There a case in which we would fail to output exactly SIZE bytes:
   for a structure constructor that wants to produce more than SIZE bytes.
   But such constructors will never be generated for any possible input.  */

void
output_constant (exp, size)
     register tree exp;
     register int size;
{
  register enum tree_code code = TREE_CODE (TREE_TYPE (exp));
  rtx x;

  if (size == 0)
    return;

  /* Allow a constructor with no elements for any data type.
     This means to fill the space with zeros.  */
  if (TREE_CODE (exp) == CONSTRUCTOR && CONSTRUCTOR_ELTS (exp) == 0)
    {
      assemble_zeros (size);
      return;
    }

  /* Eliminate the NOP_EXPR that makes a cast not be an lvalue.
     That way we get the constant (we hope) inside it.  */
  if (TREE_CODE (exp) == NOP_EXPR
      && TREE_TYPE (exp) == TREE_TYPE (TREE_OPERAND (exp, 0)))
    exp = TREE_OPERAND (exp, 0);

  switch (code)
    {
    case CHAR_TYPE:
    case BOOLEAN_TYPE:
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* ??? What about       (int)((float)(int)&foo + 4)    */
      while (TREE_CODE (exp) == NOP_EXPR || TREE_CODE (exp) == CONVERT_EXPR
	     || TREE_CODE (exp) == NON_LVALUE_EXPR)
	exp = TREE_OPERAND (exp, 0);

      if (! assemble_integer (expand_expr (exp, NULL_RTX, VOIDmode,
					   EXPAND_INITIALIZER),
			      size, 0))
	error ("initializer for integer value is too complicated");
      size = 0;
      break;

    case REAL_TYPE:
      if (TREE_CODE (exp) != REAL_CST)
	error ("initializer for floating value is not a floating constant");

      assemble_real (TREE_REAL_CST (exp),
		     mode_for_size (size * BITS_PER_UNIT, MODE_FLOAT, 0));
      size = 0;
      break;

    case COMPLEX_TYPE:
      output_constant (TREE_REALPART (exp), size / 2);
      output_constant (TREE_IMAGPART (exp), size / 2);
      size -= (size / 2) * 2;
      break;

    case ARRAY_TYPE:
      if (TREE_CODE (exp) == CONSTRUCTOR)
	{
	  output_constructor (exp, size);
	  return;
	}
      else if (TREE_CODE (exp) == STRING_CST)
	{
	  int excess = 0;

	  if (size > TREE_STRING_LENGTH (exp))
	    {
	      excess = size - TREE_STRING_LENGTH (exp);
	      size = TREE_STRING_LENGTH (exp);
	    }

	  assemble_string (TREE_STRING_POINTER (exp), size);
	  size = excess;
	}
      else
	abort ();
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      if (TREE_CODE (exp) == CONSTRUCTOR)
	output_constructor (exp, size);
      else
	abort ();
      return;
    }

  if (size > 0)
    assemble_zeros (size);
}

/* Subroutine of output_constant, used for CONSTRUCTORs
   (aggregate constants).
   Generate at least SIZE bytes, padding if necessary.  */

void
output_constructor (exp, size)
     tree exp;
     int size;
{
  register tree link, field = 0;
  /* Number of bytes output or skipped so far.
     In other words, current position within the constructor.  */
  int total_bytes = 0;
  /* Non-zero means BYTE contains part of a byte, to be output.  */
  int byte_buffer_in_use = 0;
  register int byte;

  if (HOST_BITS_PER_WIDE_INT < BITS_PER_UNIT)
    abort ();

  if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE)
    field = TYPE_FIELDS (TREE_TYPE (exp));

  /* As LINK goes through the elements of the constant,
     FIELD goes through the structure fields, if the constant is a structure.
     if the constant is a union, then we override this,
     by getting the field from the TREE_LIST element.
     But the constant could also be an array.  Then FIELD is zero.  */
  for (link = CONSTRUCTOR_ELTS (exp);
       link;
       link = TREE_CHAIN (link),
       field = field ? TREE_CHAIN (field) : 0)
    {
      tree val = TREE_VALUE (link);
      /* the element in a union constructor specifies the proper field.  */
      if (TREE_PURPOSE (link) != 0)
	field = TREE_PURPOSE (link);

      /* Eliminate the marker that makes a cast not be an lvalue.  */
      if (val != 0)
	STRIP_NOPS (val);

      if (field == 0 || !DECL_BIT_FIELD (field))
	{
	  register int fieldsize;
	  /* Since this structure is static,
	     we know the positions are constant.  */
	  int bitpos = (field ? (TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field))
				 / BITS_PER_UNIT)
			: 0);

	  /* An element that is not a bit-field.
	     Output any buffered-up bit-fields preceding it.  */
	  if (byte_buffer_in_use)
	    {
	      ASM_OUTPUT_BYTE (asm_out_file, byte);
	      total_bytes++;
	      byte_buffer_in_use = 0;
	    }

	  /* Advance to offset of this element.
	     Note no alignment needed in an array, since that is guaranteed
	     if each element has the proper size.  */
	  if (field != 0 && bitpos != total_bytes)
	    {
	      assemble_zeros (bitpos - total_bytes);
	      total_bytes = bitpos;
	    }

	  /* Determine size this element should occupy.  */
	  if (field)
	    {
	      if (TREE_CODE (DECL_SIZE (field)) != INTEGER_CST)
		abort ();
	      if (TREE_INT_CST_LOW (DECL_SIZE (field)) > 100000)
		{
		  /* This avoids overflow trouble.  */
		  tree size_tree = size_binop (CEIL_DIV_EXPR,
					       DECL_SIZE (field),
					       size_int (BITS_PER_UNIT));
		  fieldsize = TREE_INT_CST_LOW (size_tree);
		}
	      else
		{
		  fieldsize = TREE_INT_CST_LOW (DECL_SIZE (field));
		  fieldsize = (fieldsize + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
		}
	    }
	  else
	    fieldsize = int_size_in_bytes (TREE_TYPE (TREE_TYPE (exp)));

	  /* Output the element's initial value.  */
	  if (val == 0)
	    assemble_zeros (fieldsize);
	  else
	    output_constant (val, fieldsize);

	  /* Count its size.  */
	  total_bytes += fieldsize;
	}
      else if (val != 0 && TREE_CODE (val) != INTEGER_CST)
	error ("invalid initial value for member `%s'",
	       IDENTIFIER_POINTER (DECL_NAME (field)));
      else
	{
	  /* Element that is a bit-field.  */

	  int next_offset = TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field));
	  int end_offset
	    = (next_offset + TREE_INT_CST_LOW (DECL_SIZE (field)));

	  if (val == 0)
	    val = integer_zero_node;

	  /* If this field does not start in this (or, next) byte,
	     skip some bytes.  */
	  if (next_offset / BITS_PER_UNIT != total_bytes)
	    {
	      /* Output remnant of any bit field in previous bytes.  */
	      if (byte_buffer_in_use)
		{
		  ASM_OUTPUT_BYTE (asm_out_file, byte);
		  total_bytes++;
		  byte_buffer_in_use = 0;
		}

	      /* If still not at proper byte, advance to there.  */
	      if (next_offset / BITS_PER_UNIT != total_bytes)
		{
		  assemble_zeros (next_offset / BITS_PER_UNIT - total_bytes);
		  total_bytes = next_offset / BITS_PER_UNIT;
		}
	    }

	  if (! byte_buffer_in_use)
	    byte = 0;

	  /* We must split the element into pieces that fall within
	     separate bytes, and combine each byte with previous or
	     following bit-fields.  */

	  /* next_offset is the offset n fbits from the beginning of
	     the structure to the next bit of this element to be processed.
	     end_offset is the offset of the first bit past the end of
	     this element.  */
	  while (next_offset < end_offset)
	    {
	      int this_time;
	      int shift, value;
	      int next_byte = next_offset / BITS_PER_UNIT;
	      int next_bit = next_offset % BITS_PER_UNIT;

	      /* Advance from byte to byte
		 within this element when necessary.  */
	      while (next_byte != total_bytes)
		{
		  ASM_OUTPUT_BYTE (asm_out_file, byte);
		  total_bytes++;
		  byte = 0;
		}

	      /* Number of bits we can process at once
		 (all part of the same byte).  */
	      this_time = MIN (end_offset - next_offset,
			       BITS_PER_UNIT - next_bit);
#if BYTES_BIG_ENDIAN
	      /* On big-endian machine, take the most significant bits
		 first (of the bits that are significant)
		 and put them into bytes from the most significant end.  */
	      shift = end_offset - next_offset - this_time;
	      /* Don't try to take a bunch of bits that cross
		 the word boundary in the INTEGER_CST.  */
	      if (shift < HOST_BITS_PER_WIDE_INT
		  && shift + this_time > HOST_BITS_PER_WIDE_INT)
		{
		  this_time -= (HOST_BITS_PER_WIDE_INT - shift);
		  shift = HOST_BITS_PER_WIDE_INT;
		}

	      /* Now get the bits from the appropriate constant word.  */
	      if (shift < HOST_BITS_PER_WIDE_INT)
		{
		  value = TREE_INT_CST_LOW (val);
		}
	      else if (shift < 2 * HOST_BITS_PER_WIDE_INT)
		{
		  value = TREE_INT_CST_HIGH (val);
		  shift -= HOST_BITS_PER_WIDE_INT;
		}
	      else
		abort ();
	      byte |= (((value >> shift)
			& (((HOST_WIDE_INT) 1 << this_time) - 1))
		       << (BITS_PER_UNIT - this_time - next_bit));
#else
	      /* On little-endian machines,
		 take first the least significant bits of the value
		 and pack them starting at the least significant
		 bits of the bytes.  */
	      shift = (next_offset
		       - TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field)));
	      /* Don't try to take a bunch of bits that cross
		 the word boundary in the INTEGER_CST.  */
	      if (shift < HOST_BITS_PER_WIDE_INT
		  && shift + this_time > HOST_BITS_PER_WIDE_INT)
		{
		  this_time -= (HOST_BITS_PER_WIDE_INT - shift);
		  shift = HOST_BITS_PER_WIDE_INT;
		}

	      /* Now get the bits from the appropriate constant word.  */
	      if (shift < HOST_BITS_PER_INT)
		value = TREE_INT_CST_LOW (val);
	      else if (shift < 2 * HOST_BITS_PER_WIDE_INT)
		{
		  value = TREE_INT_CST_HIGH (val);
		  shift -= HOST_BITS_PER_WIDE_INT;
		}
	      else
		abort ();
	      byte |= ((value >> shift)
		       & (((HOST_WIDE_INT) 1 << this_time) - 1)) << next_bit;
#endif
	      next_offset += this_time;
	      byte_buffer_in_use = 1;
	    }
	}
    }
  if (byte_buffer_in_use)
    {
      ASM_OUTPUT_BYTE (asm_out_file, byte);
      total_bytes++;
    }
  if (total_bytes < size)
    assemble_zeros (size - total_bytes);
}
