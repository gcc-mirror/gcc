/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 1987, 88, 89, 92-99, 2000 Free Software Foundation, Inc.

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


/* This file handles generation of all the assembler code
   *except* the instructions of a function.
   This includes declarations of variables and their initial values.

   We also output the assembler code for constants stored in memory
   and are responsible for combining constants with the same value.  */

#include "config.h"
#include "system.h"
#include <setjmp.h>
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "output.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "defaults.h"
#include "real.h"
#include "toplev.h"
#include "dbxout.h"
#include "sdbout.h"
#include "obstack.h"
#include "c-pragma.h"
#include "ggc.h"
#include "tm_p.h"

#ifdef XCOFF_DEBUGGING_INFO
#include "xcoffout.h"
#endif

#ifndef TRAMPOLINE_ALIGNMENT
#define TRAMPOLINE_ALIGNMENT FUNCTION_BOUNDARY
#endif

#ifndef ASM_STABS_OP
#define ASM_STABS_OP ".stabs"
#endif

/* Define the prefix to use when check_memory_usage_flag is enable.  */
#ifdef NO_DOLLAR_IN_LABEL
#ifdef NO_DOT_IN_LABEL
#define CHKR_PREFIX "chkr_prefix_"
#else /* !NO_DOT_IN_LABEL */
#define CHKR_PREFIX "chkr."
#endif 
#else /* !NO_DOLLAR_IN_LABEL */
#define CHKR_PREFIX "chkr$"
#endif
#define CHKR_PREFIX_SIZE (sizeof (CHKR_PREFIX) - 1)

/* File in which assembler code is being written.  */

extern FILE *asm_out_file;

/* The (assembler) name of the first globally-visible object output.  */
char *first_global_object_name;
char *weak_global_object_name;

extern struct obstack *current_obstack;
extern struct obstack *saveable_obstack;
extern struct obstack *rtl_obstack;
extern struct obstack permanent_obstack;
#define obstack_chunk_alloc xmalloc

struct addr_const;
struct constant_descriptor;
struct rtx_const;
struct pool_constant;

#define MAX_RTX_HASH_TABLE 61

struct varasm_status
{
  /* Hash facility for making memory-constants
     from constant rtl-expressions.  It is used on RISC machines
     where immediate integer arguments and constant addresses are restricted
     so that such constants must be stored in memory.

     This pool of constants is reinitialized for each function
     so each function gets its own constants-pool that comes right before
     it.  */
  struct constant_descriptor **x_const_rtx_hash_table;
  struct pool_sym **x_const_rtx_sym_hash_table;

  /* Pointers to first and last constant in pool.  */
  struct pool_constant *x_first_pool, *x_last_pool;

  /* Current offset in constant pool (does not include any machine-specific
     header.  */
  int x_pool_offset;

  /* Chain of all CONST_DOUBLE rtx's constructed for the current function.
     They are chained through the CONST_DOUBLE_CHAIN.
     A CONST_DOUBLE rtx has CONST_DOUBLE_MEM != cc0_rtx iff it is on this chain.
     In that case, CONST_DOUBLE_MEM is either a MEM,
     or const0_rtx if no MEM has been made for this CONST_DOUBLE yet.  */
  rtx x_const_double_chain;
};

#define const_rtx_hash_table (cfun->varasm->x_const_rtx_hash_table)
#define const_rtx_sym_hash_table (cfun->varasm->x_const_rtx_sym_hash_table)
#define first_pool (cfun->varasm->x_first_pool)
#define last_pool (cfun->varasm->x_last_pool)
#define pool_offset (cfun->varasm->x_pool_offset)
#define const_double_chain (cfun->varasm->x_const_double_chain)

/* Number for making the label on the next
   constant that is stored in memory.  */

int const_labelno;

/* Number for making the label on the next
   static variable internal to a function.  */

int var_labelno;

/* Carry information from ASM_DECLARE_OBJECT_NAME
   to ASM_FINISH_DECLARE_OBJECT.  */

int size_directive_output;

/* The last decl for which assemble_variable was called,
   if it did ASM_DECLARE_OBJECT_NAME.
   If the last call to assemble_variable didn't do that,
   this holds 0.  */

tree last_assemble_variable_decl;

static const char *strip_reg_name	PARAMS ((const char *));
static int contains_pointers_p		PARAMS ((tree));
static void decode_addr_const		PARAMS ((tree, struct addr_const *));
static int const_hash			PARAMS ((tree));
static int compare_constant		PARAMS ((tree,
					       struct constant_descriptor *));
static char *compare_constant_1		PARAMS ((tree, char *));
static struct constant_descriptor *record_constant PARAMS ((tree));
static void record_constant_1		PARAMS ((tree));
static tree copy_constant		PARAMS ((tree));
static void output_constant_def_contents  PARAMS ((tree, int, int));
static void decode_rtx_const		PARAMS ((enum machine_mode, rtx,
					       struct rtx_const *));
static int const_hash_rtx		PARAMS ((enum machine_mode, rtx));
static int compare_constant_rtx		PARAMS ((enum machine_mode, rtx,
					       struct constant_descriptor *));
static struct constant_descriptor *record_constant_rtx PARAMS ((enum machine_mode,
							      rtx));
static struct pool_constant *find_pool_constant PARAMS ((struct function *, rtx));
static void mark_constant_pool		PARAMS ((void));
static void mark_constants		PARAMS ((rtx));
static int output_addressed_constants	PARAMS ((tree));
static void output_after_function_constants PARAMS ((void));
static void output_constructor		PARAMS ((tree, int));
#ifdef ASM_WEAKEN_LABEL
static void remove_from_pending_weak_list	PARAMS ((char *));
#endif
#ifdef ASM_OUTPUT_BSS
static void asm_output_bss		PARAMS ((FILE *, tree, char *, int, int));
#endif
#ifdef BSS_SECTION_ASM_OP
#ifdef ASM_OUTPUT_ALIGNED_BSS
static void asm_output_aligned_bss	PARAMS ((FILE *, tree, char *, int, int));
#endif
#endif /* BSS_SECTION_ASM_OP */
static void mark_pool_constant          PARAMS ((struct pool_constant *));
static void mark_pool_sym_hash_table	PARAMS ((struct pool_sym **));
static void mark_const_hash_entry	PARAMS ((void *));
static void asm_emit_uninitialised	PARAMS ((tree, char *, int, int));

static enum in_section { no_section, in_text, in_data, in_named
#ifdef BSS_SECTION_ASM_OP
  , in_bss
#endif
#ifdef EH_FRAME_SECTION_ASM_OP
  , in_eh_frame
#endif
#ifdef EXTRA_SECTIONS
  , EXTRA_SECTIONS
#endif
} in_section = no_section;

/* Return a non-zero value if DECL has a section attribute.  */
#ifndef IN_NAMED_SECTION
#define IN_NAMED_SECTION(DECL) \
  ((TREE_CODE (DECL) == FUNCTION_DECL || TREE_CODE (DECL) == VAR_DECL) \
   && DECL_SECTION_NAME (DECL) != NULL_TREE)
#endif
     
/* Text of section name when in_section == in_named.  */
static char *in_named_name;

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
/* Tell assembler to ALWAYS switch to data section, in case
   it's not sure where it it.  */

void
force_data_section ()
{
  in_section = no_section;
  data_section ();
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

/* Determine if we're in the text section.  */

int
in_text_section ()
{
  return in_section == in_text;
}

/* Determine if we're in the data section.  */

int
in_data_section ()
{
  return in_section == in_data;
}

/* Tell assembler to change to section NAME for DECL.
   If DECL is NULL, just switch to section NAME.
   If NAME is NULL, get the name from DECL.
   If RELOC is 1, the initializer for DECL contains relocs.  */

void
named_section (decl, name, reloc)
     tree decl;
     const char *name;
     int reloc ATTRIBUTE_UNUSED;
{
  if (decl != NULL_TREE
      && TREE_CODE_CLASS (TREE_CODE (decl)) != 'd')
    abort ();
  if (name == NULL)
    name = TREE_STRING_POINTER (DECL_SECTION_NAME (decl));

  if (in_section != in_named || strcmp (name, in_named_name))
    {
#ifdef ASM_OUTPUT_SECTION_NAME
      ASM_OUTPUT_SECTION_NAME (asm_out_file, decl, name, reloc);
#else
      /* Section attributes are not supported if this macro isn't provided -
	 some host formats don't support them at all.  The front-end should
	 already have flagged this as an error.  */
      abort ();
#endif

      in_named_name = ggc_alloc_string (name, -1);
      in_section = in_named;
    }
}

#ifdef ASM_OUTPUT_SECTION_NAME
#ifndef UNIQUE_SECTION
#define UNIQUE_SECTION(DECL,RELOC)				\
do {								\
  int len;							\
  const char *name;						\
  char *string;							\
								\
  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (DECL));	\
  /* Strip off any encoding in name.  */			\
  STRIP_NAME_ENCODING (name, name);				\
								\
  len = strlen (name) + 1;					\
  string = alloca (len + 1);					\
  sprintf (string, ".%s", name);				\
								\
  DECL_SECTION_NAME (DECL) = build_string (len, string);	\
} while (0)
#endif
#ifndef UNIQUE_SECTION_P
#define UNIQUE_SECTION_P(DECL) 0
#endif
#endif

#ifdef BSS_SECTION_ASM_OP

/* Tell the assembler to switch to the bss section.  */

void
bss_section ()
{
  if (in_section != in_bss)
    {
#ifdef SHARED_BSS_SECTION_ASM_OP
      if (flag_shared_data)
	fprintf (asm_out_file, "%s\n", SHARED_BSS_SECTION_ASM_OP);
      else
#endif
	fprintf (asm_out_file, "%s\n", BSS_SECTION_ASM_OP);

      in_section = in_bss;
    }
}

#ifdef ASM_OUTPUT_BSS

/* Utility function for ASM_OUTPUT_BSS for targets to use if
   they don't support alignments in .bss.
   ??? It is believed that this function will work in most cases so such
   support is localized here.  */

static void
asm_output_bss (file, decl, name, size, rounded)
     FILE *file;
     tree decl ATTRIBUTE_UNUSED;
     char *name;
     int size ATTRIBUTE_UNUSED, rounded;
{
  ASM_GLOBALIZE_LABEL (file, name);
  bss_section ();
#ifdef ASM_DECLARE_OBJECT_NAME
  last_assemble_variable_decl = decl;
  ASM_DECLARE_OBJECT_NAME (file, name, decl);
#else
  /* Standard thing is just output label for the object.  */
  ASM_OUTPUT_LABEL (file, name);
#endif /* ASM_DECLARE_OBJECT_NAME */
  ASM_OUTPUT_SKIP (file, rounded);
}

#endif

#ifdef ASM_OUTPUT_ALIGNED_BSS

/* Utility function for targets to use in implementing
   ASM_OUTPUT_ALIGNED_BSS.
   ??? It is believed that this function will work in most cases so such
   support is localized here.  */

static void
asm_output_aligned_bss (file, decl, name, size, align)
     FILE *file;
     tree decl;
     char *name;
     int size, align;
{
  ASM_GLOBALIZE_LABEL (file, name);
  bss_section ();
  ASM_OUTPUT_ALIGN (file, floor_log2 (align / BITS_PER_UNIT));
#ifdef ASM_DECLARE_OBJECT_NAME
  last_assemble_variable_decl = decl;
  ASM_DECLARE_OBJECT_NAME (file, name, decl);
#else
  /* Standard thing is just output label for the object.  */
  ASM_OUTPUT_LABEL (file, name);
#endif /* ASM_DECLARE_OBJECT_NAME */
  ASM_OUTPUT_SKIP (file, size ? size : 1);
}

#endif

#endif /* BSS_SECTION_ASM_OP */

#ifdef EH_FRAME_SECTION_ASM_OP
void
eh_frame_section ()
{
  if (in_section != in_eh_frame)
    {
      fprintf (asm_out_file, "%s\n", EH_FRAME_SECTION_ASM_OP);
      in_section = in_eh_frame;
    }
} 
#endif

/* Switch to the section for function DECL.

   If DECL is NULL_TREE, switch to the text section.
   ??? It's not clear that we will ever be passed NULL_TREE, but it's
   safer to handle it.  */

void
function_section (decl)
     tree decl;
{
  if (decl != NULL_TREE
      && DECL_SECTION_NAME (decl) != NULL_TREE)
    named_section (decl, (char *) 0, 0);
  else
    text_section ();
}

/* Switch to section for variable DECL.

   RELOC is the `reloc' argument to SELECT_SECTION.  */

void
variable_section (decl, reloc)
     tree decl;
     int reloc;
{
  if (IN_NAMED_SECTION (decl))
    named_section (decl, NULL, reloc);
  else
    {
      /* C++ can have const variables that get initialized from constructors,
	 and thus can not be in a readonly section.  We prevent this by
	 verifying that the initial value is constant for objects put in a
	 readonly section.

	 error_mark_node is used by the C front end to indicate that the
	 initializer has not been seen yet.  In this case, we assume that
	 the initializer must be constant.

	 C++ uses error_mark_node for variables that have complicated
	 initializers, but these variables go in BSS so we won't be called
	 for them.  */

#ifdef SELECT_SECTION
      SELECT_SECTION (decl, reloc);
#else
      if (DECL_READONLY_SECTION (decl, reloc))
	readonly_data_section ();
      else
	data_section ();
#endif
    }
}

/* Tell assembler to switch to the section for the exception handling
   table.  */

void
exception_section ()
{
#if defined (EXCEPTION_SECTION)
  EXCEPTION_SECTION ();
#else
#ifdef ASM_OUTPUT_SECTION_NAME
  named_section (NULL_TREE, ".gcc_except_table", 0);
#else
  if (flag_pic)
    data_section ();
  else
    readonly_data_section ();
#endif
#endif
}

/* Create the rtl to represent a function, for a function definition.
   DECL is a FUNCTION_DECL node which describes which function.
   The rtl is stored into DECL.  */

void
make_function_rtl (decl)
     tree decl;
{
  char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  char *new_name = name;

  /* Rename a nested function to avoid conflicts.  */
  if (decl_function_context (decl) != 0
      && DECL_INITIAL (decl) != 0
      && DECL_RTL (decl) == 0)
    {
      char *label;

      name = IDENTIFIER_POINTER (DECL_NAME (decl));
      ASM_FORMAT_PRIVATE_NAME (label, name, var_labelno);
      name = ggc_alloc_string (label, -1);
      var_labelno++;
    }
  else
    {
      /* When -fprefix-function-name is used, every function name is
         prefixed.  Even static functions are prefixed because they
         could be declared latter.  Note that a nested function name
         is not prefixed.  */
      if (flag_prefix_function_name)
        {
	  size_t name_len = strlen (name);

          new_name = ggc_alloc_string (NULL, name_len + CHKR_PREFIX_SIZE);
	  memcpy (new_name, CHKR_PREFIX, CHKR_PREFIX_SIZE);
	  memcpy (new_name + CHKR_PREFIX_SIZE, name, name_len + 1);
          name = new_name;
        }
    }

  if (DECL_RTL (decl) == 0)
    {
      DECL_RTL (decl)
	= gen_rtx_MEM (DECL_MODE (decl),
		       gen_rtx_SYMBOL_REF (Pmode, name));

      /* Optionally set flags or add text to the name to record information
	 such as that it is a function name.  If the name is changed, the macro
	 ASM_OUTPUT_LABELREF will have to know how to strip this information.  */
#ifdef ENCODE_SECTION_INFO
      ENCODE_SECTION_INFO (decl);
#endif
    }
  else
    {
      /* ??? Another way to do this would be to do what halfpic.c does
	 and maintain a hashed table of such critters.  */
      /* ??? Another way to do this would be to pass a flag bit to
	 ENCODE_SECTION_INFO saying whether this is a new decl or not.  */
      /* Let the target reassign the RTL if it wants.
	 This is necessary, for example, when one machine specific
	 decl attribute overrides another.  */
#ifdef REDO_SECTION_INFO_P
      if (REDO_SECTION_INFO_P (decl))
	ENCODE_SECTION_INFO (decl);
#endif
    }
}

/* Given NAME, a putative register name, discard any customary prefixes.  */

static const char *
strip_reg_name (name)
  const char *name;
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
  const char *asmspec;
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
	static struct { const char *name; int number; } table[]
	  = ADDITIONAL_REGISTER_NAMES;

	for (i = 0; i < (int)(sizeof (table) / sizeof (table[0])); i++)
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
     const char *asmspec;
     int top_level;
{
  register char *name = 0;
  int reg_number;

  reg_number = decode_reg_name (asmspec);

  if (DECL_ASSEMBLER_NAME (decl) != NULL_TREE)
    name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  if (reg_number == -2)
    {
      /* ASMSPEC is given, and not the name of a register.  */
      size_t len = strlen (asmspec);

      name = ggc_alloc_string (NULL, len + 1);
      name[0] = '*';
      memcpy (&name[1], asmspec, len + 1);
    }

  /* For a duplicate declaration, we can be called twice on the
     same DECL node.  Don't discard the RTL already made.  */
  if (DECL_RTL (decl) == 0)
    {
      /* First detect errors in declaring global registers.  */
      if (TREE_CODE (decl) != FUNCTION_DECL
	  && DECL_REGISTER (decl) && reg_number == -1)
	error_with_decl (decl,
			 "register name not specified for `%s'");
      else if (TREE_CODE (decl) != FUNCTION_DECL
	       && DECL_REGISTER (decl) && reg_number < 0)
	error_with_decl (decl,
			 "invalid register name for `%s'");
      else if ((reg_number >= 0 || reg_number == -3)
	       && (TREE_CODE (decl) == FUNCTION_DECL
		   && ! DECL_REGISTER (decl)))
	error_with_decl (decl,
			 "register name given for non-register variable `%s'");
      else if (TREE_CODE (decl) != FUNCTION_DECL
	       && DECL_REGISTER (decl)
	       && TYPE_MODE (TREE_TYPE (decl)) == BLKmode)
	error_with_decl (decl,
			 "data type of `%s' isn't suitable for a register");
      else if (TREE_CODE (decl) != FUNCTION_DECL && DECL_REGISTER (decl)
	       && ! HARD_REGNO_MODE_OK (reg_number,
					TYPE_MODE (TREE_TYPE (decl))))
	error_with_decl (decl,
			 "register number for `%s' isn't suitable for data type");
      /* Now handle properly declared static register variables.  */
      else if (TREE_CODE (decl) != FUNCTION_DECL && DECL_REGISTER (decl))
	{
	  int nregs;

	  if (DECL_INITIAL (decl) != 0 && top_level)
	    {
	      DECL_INITIAL (decl) = 0;
	      error ("global register variable has initial value");
	    }
	  if (TREE_THIS_VOLATILE (decl))
	    warning ("volatile register variables don't work as you might wish");

	  /* If the user specified one of the eliminables registers here,
	     e.g., FRAME_POINTER_REGNUM, we don't want to get this variable
	     confused with that register and be eliminated.  Although this
	     usage is somewhat suspect, we nevertheless use the following
	     kludge to avoid setting DECL_RTL to frame_pointer_rtx.  */

	  DECL_RTL (decl)
	    = gen_rtx_REG (DECL_MODE (decl), FIRST_PSEUDO_REGISTER);
	  REGNO (DECL_RTL (decl)) = reg_number;
	  REG_USERVAR_P (DECL_RTL (decl)) = 1;

	  if (top_level)
	    {
	      /* Make this register global, so not usable for anything
		 else.  */
#ifdef ASM_DECLARE_REGISTER_GLOBAL
	      ASM_DECLARE_REGISTER_GLOBAL (asm_out_file, decl, reg_number, name);
#endif
	      nregs = HARD_REGNO_NREGS (reg_number, DECL_MODE (decl));
	      while (nregs > 0)
		globalize_reg (reg_number + --nregs);
	    }
	}
      /* Specifying a section attribute on a variable forces it into a
         non-.bss section, and thus it cannot be common. */
      else if (TREE_CODE (decl) == VAR_DECL
	       && DECL_SECTION_NAME (decl) != NULL_TREE
	       && DECL_INITIAL (decl) == NULL_TREE
	       && DECL_COMMON (decl))
          DECL_COMMON (decl) = 0;

      /* Now handle ordinary static variables and functions (in memory).
	 Also handle vars declared register invalidly.  */
      if (DECL_RTL (decl) == 0)
	{
	  /* Can't use just the variable's own name for a variable
	     whose scope is less than the whole file.
	     Concatenate a distinguishing number.  */
	  if (!top_level && !TREE_PUBLIC (decl) && asmspec == 0)
	    {
	      char *label;

	      ASM_FORMAT_PRIVATE_NAME (label, name, var_labelno);
	      name = ggc_alloc_string (label, -1);
	      var_labelno++;
	    }

	  if (name == 0)
	    abort ();

	  /* When -fprefix-function-name is used, the functions
	     names are prefixed.  Only nested function names are not
	     prefixed.  */
	  if (flag_prefix_function_name && TREE_CODE (decl) == FUNCTION_DECL)
	    {
	      size_t name_len = strlen (name);
	      char *new_name;

	      new_name = ggc_alloc_string (NULL, name_len + CHKR_PREFIX_SIZE);
	      memcpy (new_name, CHKR_PREFIX, CHKR_PREFIX_SIZE);
	      memcpy (new_name + CHKR_PREFIX_SIZE, name, name_len + 1);
	      name = new_name;
	    }

	  DECL_RTL (decl) = gen_rtx_MEM (DECL_MODE (decl),
					 gen_rtx_SYMBOL_REF (Pmode, name));
	  MEM_ALIAS_SET (DECL_RTL (decl)) = get_alias_set (decl);

	  /* If this variable is to be treated as volatile, show its
	     tree node has side effects.  If it has side effects, either
	     because of this test or from TREE_THIS_VOLATILE also
	     being set, show the MEM is volatile.  */
	  if (flag_volatile_global && TREE_CODE (decl) == VAR_DECL
	      && TREE_PUBLIC (decl))
	    TREE_SIDE_EFFECTS (decl) = 1;
	  else if (flag_volatile_static && TREE_CODE (decl) == VAR_DECL
	       && (TREE_PUBLIC (decl) || TREE_STATIC (decl)))
	    TREE_SIDE_EFFECTS (decl) = 1;

	  if (TREE_SIDE_EFFECTS (decl))
	    MEM_VOLATILE_P (DECL_RTL (decl)) = 1;

	  if (TREE_READONLY (decl))
	    RTX_UNCHANGING_P (DECL_RTL (decl)) = 1;
	  MEM_SET_IN_STRUCT_P (DECL_RTL (decl),
			       AGGREGATE_TYPE_P (TREE_TYPE (decl)));

	  /* Optionally set flags or add text to the name to record information
	     such as that it is a function name.
	     If the name is changed, the macro ASM_OUTPUT_LABELREF
	     will have to know how to strip this information.  */
#ifdef ENCODE_SECTION_INFO
	  ENCODE_SECTION_INFO (decl);
#endif
	}
    }
  else
    {
      /* If the old RTL had the wrong mode, fix the mode.  */
      if (GET_MODE (DECL_RTL (decl)) != DECL_MODE (decl))
	{
	  rtx rtl = DECL_RTL (decl);
	  PUT_MODE (rtl, DECL_MODE (decl));
	}

      /* ??? Another way to do this would be to do what halfpic.c does
	 and maintain a hashed table of such critters.  */
      /* ??? Another way to do this would be to pass a flag bit to
	 ENCODE_SECTION_INFO saying whether this is a new decl or not.  */
      /* Let the target reassign the RTL if it wants.
	 This is necessary, for example, when one machine specific
	 decl attribute overrides another.  */
#ifdef REDO_SECTION_INFO_P
      if (REDO_SECTION_INFO_P (decl))
	ENCODE_SECTION_INFO (decl);
#endif
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
     const char *name;
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
     const char *name;
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
     const char *name;
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

/* CONSTANT_POOL_BEFORE_FUNCTION may be defined as an expression with
   a non-zero value if the constant pool should be output before the
   start of the function, or a zero value if the pool should output
   after the end of the function.  The default is to put it before the
   start.  */

#ifndef CONSTANT_POOL_BEFORE_FUNCTION
#define CONSTANT_POOL_BEFORE_FUNCTION 1
#endif

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

  if (CONSTANT_POOL_BEFORE_FUNCTION)
    output_constant_pool (fnname, decl);

#ifdef ASM_OUTPUT_SECTION_NAME
  /* If the function is to be put in its own section and it's not in a section
     already, indicate so.  */
  if ((flag_function_sections
       && DECL_SECTION_NAME (decl) == NULL_TREE)
      || UNIQUE_SECTION_P (decl))
    UNIQUE_SECTION (decl, 0);
#endif

  function_section (decl);

  /* Tell assembler to move to target machine's alignment for functions.  */
  align = floor_log2 (FUNCTION_BOUNDARY / BITS_PER_UNIT);
  if (align > 0)
    ASM_OUTPUT_ALIGN (asm_out_file, align);

  /* Handle a user-specified function alignment.
     Note that we still need to align to FUNCTION_BOUNDARY, as above,
     because ASM_OUTPUT_MAX_SKIP_ALIGN might not do any alignment at all.  */
  if (align_functions_log > align)
    {
#ifdef ASM_OUTPUT_MAX_SKIP_ALIGN
      ASM_OUTPUT_MAX_SKIP_ALIGN (asm_out_file, 
				 align_functions_log, align_functions-1);
#else
      ASM_OUTPUT_ALIGN (asm_out_file, align_functions_log);
#endif
    }

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
      if (! first_global_object_name)
	{
	  const char *p;
	  char **name;

	  if (! DECL_WEAK (decl) && ! DECL_ONE_ONLY (decl))
	    name = &first_global_object_name;
	  else
	    name = &weak_global_object_name;

	  STRIP_NAME_ENCODING (p, fnname);
	  *name = permalloc (strlen (p) + 1);
	  strcpy (*name, p);
	}

#ifdef ASM_WEAKEN_LABEL
      if (DECL_WEAK (decl))
	{
	  ASM_WEAKEN_LABEL (asm_out_file, fnname);
	  /* Remove this function from the pending weak list so that
	     we do not emit multiple .weak directives for it.  */
	  remove_from_pending_weak_list
	    (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));
	}
      else
#endif
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
     const char *fnname;
{
#ifdef ASM_DECLARE_FUNCTION_SIZE
  ASM_DECLARE_FUNCTION_SIZE (asm_out_file, fnname, decl);
#endif
  if (! CONSTANT_POOL_BEFORE_FUNCTION)
    {
      output_constant_pool (fnname, decl);
      function_section (decl);	/* need to switch back */
    }

  /* Output any constants which should appear after the function.  */
  output_after_function_constants ();
}

/* Assemble code to leave SIZE bytes of zeros.  */

void
assemble_zeros (size)
     int size;
{
  /* Do no output if -fsyntax-only.  */
  if (flag_syntax_only)
    return;

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

/* Assemble an alignment pseudo op for an ALIGN-bit boundary.  */

void
assemble_align (align)
     int align;
{
  if (align > BITS_PER_UNIT)
    ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (align / BITS_PER_UNIT));
}

/* Assemble a string constant with the specified C string as contents.  */

void
assemble_string (p, size)
     const char *p;
     int size;
{
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


#if defined  ASM_OUTPUT_ALIGNED_DECL_LOCAL
#define ASM_EMIT_LOCAL(decl, name, size, rounded) \
  ASM_OUTPUT_ALIGNED_DECL_LOCAL (asm_out_file, decl, name, size, DECL_ALIGN (decl))
#else
#if defined  ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_EMIT_LOCAL(decl, name, size, rounded) \
  ASM_OUTPUT_ALIGNED_LOCAL (asm_out_file, name, size, DECL_ALIGN (decl))
#else
#define ASM_EMIT_LOCAL(decl, name, size, rounded) \
  ASM_OUTPUT_LOCAL (asm_out_file, name, size, rounded)
#endif
#endif

#if defined ASM_OUTPUT_ALIGNED_BSS
#define ASM_EMIT_BSS(decl, name, size, rounded) \
  ASM_OUTPUT_ALIGNED_BSS (asm_out_file, decl, name, size, DECL_ALIGN (decl))
#else
#if defined ASM_OUTPUT_BSS
#define ASM_EMIT_BSS(decl, name, size, rounded) \
  ASM_OUTPUT_BSS (asm_out_file, decl, name, size, rounded)
#else
#undef  ASM_EMIT_BSS
#endif
#endif

#if defined ASM_OUTPUT_ALIGNED_DECL_COMMON
#define ASM_EMIT_COMMON(decl, name, size, rounded) \
  ASM_OUTPUT_ALIGNED_DECL_COMMON (asm_out_file, decl, name, size, DECL_ALIGN (decl))
#else
#if defined ASM_OUTPUT_ALIGNED_COMMON
#define ASM_EMIT_COMMON(decl, name, size, rounded) \
  ASM_OUTPUT_ALIGNED_COMMON (asm_out_file, name, size, DECL_ALIGN (decl))
#else
#define ASM_EMIT_COMMON(decl, name, size, rounded) \
  ASM_OUTPUT_COMMON (asm_out_file, name, size, rounded)
#endif
#endif

static void
asm_emit_uninitialised (decl, name, size, rounded)
     tree decl;
     char * name;
     int size ATTRIBUTE_UNUSED;
     int rounded ATTRIBUTE_UNUSED;
{
  enum
  {
    asm_dest_common,
    asm_dest_bss,
    asm_dest_local
  }
  destination = asm_dest_local;
  
  if (TREE_PUBLIC (decl))
    {
#if defined ASM_EMIT_BSS
      if (! DECL_COMMON (decl))
	destination = asm_dest_bss;
      else
#endif      
	destination = asm_dest_common;
    }

  if (flag_shared_data)
    {
      switch (destination)
	{
#ifdef ASM_OUTPUT_SHARED_BSS
	case asm_dest_bss:
	  ASM_OUTPUT_SHARED_BSS (asm_out_file, decl, name, size, rounded);
	  return;
#endif
#ifdef ASM_OUTPUT_SHARED_COMMON
	case asm_dest_common:
	  ASM_OUTPUT_SHARED_COMMON (asm_out_file, name, size, rounded);
	  return;
#endif
#ifdef ASM_OUTPUT_SHARED_LOCAL
	case asm_dest_local:
	  ASM_OUTPUT_SHARED_LOCAL (asm_out_file, name, size, rounded);
	  return;
#endif
	default:
	  break;
	}
    }

#ifdef ASM_OUTPUT_SECTION_NAME
  /* We already know that DECL_SECTION_NAME() == NULL.  */
  if (flag_data_sections != 0 || UNIQUE_SECTION_P (decl))
    UNIQUE_SECTION (decl, NULL);
#endif
  
  switch (destination)
    {
#ifdef ASM_EMIT_BSS
    case asm_dest_bss:
      ASM_EMIT_BSS (decl, name, size, rounded);
      break;
#endif
    case asm_dest_common:
      ASM_EMIT_COMMON (decl, name, size, rounded);
      break;
    case asm_dest_local:
      ASM_EMIT_LOCAL (decl, name, size, rounded);
      break;
    default:
      abort ();
    }

  return;
}

/* Assemble everything that is needed for a variable or function declaration.
   Not used for automatic variables, and not used for function definitions.
   Should not be called for variables of incomplete structure type.

   TOP_LEVEL is nonzero if this variable has file scope.
   AT_END is nonzero if this is the special handling, at end of compilation,
   to define things that have had only tentative definitions.
   DONT_OUTPUT_DATA if nonzero means don't actually output the
   initial value (that will be done by the caller).  */

void
assemble_variable (decl, top_level, at_end, dont_output_data)
     tree decl;
     int top_level ATTRIBUTE_UNUSED;
     int at_end ATTRIBUTE_UNUSED;
     int dont_output_data;
{
  register char *name;
  unsigned int align;
  tree size_tree = NULL_TREE;
  int reloc = 0;
  enum in_section saved_in_section;

  last_assemble_variable_decl = 0;

  if (GET_CODE (DECL_RTL (decl)) == REG)
    {
      /* Do output symbol info for global register variables, but do nothing
	 else for them.  */

      if (TREE_ASM_WRITTEN (decl))
	return;
      TREE_ASM_WRITTEN (decl) = 1;

      /* Do no output if -fsyntax-only.  */
      if (flag_syntax_only)
	return;

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
     since assemble_external is called by the language-specific code
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

  if (!dont_output_data && DECL_SIZE (decl) == 0)
    {
      error_with_file_and_line (DECL_SOURCE_FILE (decl),
				DECL_SOURCE_LINE (decl),
				"storage size of `%s' isn't known",
				IDENTIFIER_POINTER (DECL_NAME (decl)));
      TREE_ASM_WRITTEN (decl) = 1;
      return;
    }

  /* The first declaration of a variable that comes through this function
     decides whether it is global (in C, has external linkage)
     or local (in C, has internal linkage).  So do nothing more
     if this function has already run.  */

  if (TREE_ASM_WRITTEN (decl))
    return;

  TREE_ASM_WRITTEN (decl) = 1;

  /* Do no output if -fsyntax-only.  */
  if (flag_syntax_only)
    return;

  app_disable ();

  if (! dont_output_data)
    {
      int size;

      if (TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
	goto finish;

      /* This is better than explicit arithmetic, since it avoids overflow.  */
      size_tree = size_binop (CEIL_DIV_EXPR,
			      DECL_SIZE (decl), size_int (BITS_PER_UNIT));

      size = TREE_INT_CST_LOW (size_tree);
      if (TREE_INT_CST_HIGH (size_tree) != 0
	  || size != TREE_INT_CST_LOW (size_tree))
	{
	  error_with_decl (decl, "size of variable `%s' is too large");
	  goto finish;
	}
    }

  name = XSTR (XEXP (DECL_RTL (decl), 0), 0);

  if (TREE_PUBLIC (decl) && DECL_NAME (decl)
      && ! first_global_object_name
      && ! (DECL_COMMON (decl) && (DECL_INITIAL (decl) == 0
				   || DECL_INITIAL (decl) == error_mark_node))
      && ! DECL_WEAK (decl)
      && ! DECL_ONE_ONLY (decl))
    {
      const char *p;

      STRIP_NAME_ENCODING (p, name);
      first_global_object_name = permalloc (strlen (p) + 1);
      strcpy (first_global_object_name, p);
    }

  /* Compute the alignment of this data.  */

  align = DECL_ALIGN (decl);

  /* In the case for initialing an array whose length isn't specified,
     where we have not yet been able to do the layout,
     figure out the proper alignment now.  */
  if (dont_output_data && DECL_SIZE (decl) == 0
      && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
    align = MAX (align, TYPE_ALIGN (TREE_TYPE (TREE_TYPE (decl))));

  /* Some object file formats have a maximum alignment which they support.
     In particular, a.out format supports a maximum alignment of 4.  */
#ifndef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT BIGGEST_ALIGNMENT
#endif
  if (align > MAX_OFILE_ALIGNMENT)
    {
      warning_with_decl (decl,
	"alignment of `%s' is greater than maximum object file alignment. Using %d.",
                    MAX_OFILE_ALIGNMENT/BITS_PER_UNIT);
      align = MAX_OFILE_ALIGNMENT;
    }

  /* On some machines, it is good to increase alignment sometimes.  */
#ifdef DATA_ALIGNMENT
  align = DATA_ALIGNMENT (TREE_TYPE (decl), align);
#endif
#ifdef CONSTANT_ALIGNMENT
  if (DECL_INITIAL (decl) != 0 && DECL_INITIAL (decl) != error_mark_node)
    align = CONSTANT_ALIGNMENT (DECL_INITIAL (decl), align);
#endif

  /* Reset the alignment in case we have made it tighter, so we can benefit
     from it in get_pointer_alignment.  */
  DECL_ALIGN (decl) = align;

  /* Handle uninitialized definitions.  */

  if ((DECL_INITIAL (decl) == 0 || DECL_INITIAL (decl) == error_mark_node)
      /* If the target can't output uninitialized but not common global data
	 in .bss, then we have to use .data.  */
#if ! defined ASM_EMIT_BSS
      && DECL_COMMON (decl)
#endif
      && DECL_SECTION_NAME (decl) == NULL_TREE
      && ! dont_output_data)
    {
      int size = TREE_INT_CST_LOW (size_tree);
      int rounded = size;

      /* Don't allocate zero bytes of common,
	 since that means "undefined external" in the linker.  */
      if (size == 0) rounded = 1;
      /* Round size up to multiple of BIGGEST_ALIGNMENT bits
	 so that each uninitialized object starts on such a boundary.  */
      rounded += (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1;
      rounded = (rounded / (BIGGEST_ALIGNMENT / BITS_PER_UNIT)
		 * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));
      
#if !defined(ASM_OUTPUT_ALIGNED_COMMON) && !defined(ASM_OUTPUT_ALIGNED_BSS)
      if ((DECL_ALIGN (decl) / BITS_PER_UNIT) > (unsigned int) rounded)
         warning_with_decl 
           (decl, "requested alignment for %s is greater than implemented alignment of %d.",rounded);
#endif
       
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

#if 0 /* ??? We should either delete this or add a comment describing what
	 it was intended to do and why we shouldn't delete it.  */
      if (flag_shared_data)
	data_section ();
#endif
      asm_emit_uninitialised (decl, name, size, rounded);

      goto finish;
    }

  /* Handle initialized definitions.
     Also handle uninitialized global definitions if -fno-common and the
     target doesn't support ASM_OUTPUT_BSS.  */

  /* First make the assembler name(s) global if appropriate.  */
  if (TREE_PUBLIC (decl) && DECL_NAME (decl))
    {
#ifdef ASM_WEAKEN_LABEL
      if (DECL_WEAK (decl)) 
	{
	  ASM_WEAKEN_LABEL (asm_out_file, name);
	   /* Remove this variable from the pending weak list so that
	      we do not emit multiple .weak directives for it.  */
	  remove_from_pending_weak_list
	    (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));
	}
      else
#endif
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
  if (DECL_INITIAL (decl) == error_mark_node)
    reloc = contains_pointers_p (TREE_TYPE (decl));
  else if (DECL_INITIAL (decl))
    reloc = output_addressed_constants (DECL_INITIAL (decl));

#ifdef ASM_OUTPUT_SECTION_NAME
  if ((flag_data_sections != 0 && DECL_SECTION_NAME (decl) == NULL_TREE)
      || UNIQUE_SECTION_P (decl))
    UNIQUE_SECTION (decl, reloc);
#endif

  /* Switch to the appropriate section.  */
  variable_section (decl, reloc);

  /* dbxout.c needs to know this.  */
  if (in_text_section ())
    DECL_IN_TEXT_SECTION (decl) = 1;

  /* Record current section so we can restore it if dbxout.c clobbers it.  */
  saved_in_section = in_section;

  /* Output the dbx info now that we have chosen the section.  */

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

  /* If the debugging output changed sections, reselect the section
     that's supposed to be selected.  */
  if (in_section != saved_in_section)
    variable_section (decl, reloc);

  /* Output the alignment of this data.  */
  if (align > BITS_PER_UNIT)
    ASM_OUTPUT_ALIGN (asm_out_file,
		      floor_log2 (DECL_ALIGN (decl) / BITS_PER_UNIT));

  /* Do any machine/system dependent processing of the object.  */
#ifdef ASM_DECLARE_OBJECT_NAME
  last_assemble_variable_decl = decl;
  ASM_DECLARE_OBJECT_NAME (asm_out_file, name, decl);
#else
  /* Standard thing is just output label for the object.  */
  ASM_OUTPUT_LABEL (asm_out_file, name);
#endif /* ASM_DECLARE_OBJECT_NAME */

  if (!dont_output_data)
    {
      if (DECL_INITIAL (decl))
	/* Output the actual data.  */
	output_constant (DECL_INITIAL (decl), TREE_INT_CST_LOW (size_tree));
      else
	/* Leave space for it.  */
	assemble_zeros (TREE_INT_CST_LOW (size_tree));
    }

 finish:
#ifdef XCOFF_DEBUGGING_INFO
  /* Unfortunately, the IBM assembler cannot handle stabx before the actual
     declaration.  When something like ".stabx  "aa:S-2",aa,133,0" is emitted 
     and `aa' hasn't been output yet, the assembler generates a stab entry with
     a value of zero, in addition to creating an unnecessary external entry
     for `aa'.  Hence, we must postpone dbxout_symbol to here at the end.  */

  /* File-scope global variables are output here.  */
  if (write_symbols == XCOFF_DEBUG && top_level)
    {
      saved_in_section = in_section;

      dbxout_symbol (decl, 0);

      if (in_section != saved_in_section)
	variable_section (decl, reloc);
    }
#else
  /* There must be a statement after a label.  */
  ;
#endif
}

/* Return 1 if type TYPE contains any pointers.  */

static int
contains_pointers_p (type)
     tree type;
{
  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* I'm not sure whether OFFSET_TYPE needs this treatment,
	 so I'll play safe and return 1.  */
    case OFFSET_TYPE:
      return 1;

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree fields;
	/* For a type that has fields, see if the fields have pointers.  */
	for (fields = TYPE_FIELDS (type); fields; fields = TREE_CHAIN (fields))
	  if (TREE_CODE (fields) == FIELD_DECL
	      && contains_pointers_p (TREE_TYPE (fields)))
	    return 1;
	return 0;
      }

    case ARRAY_TYPE:
      /* An array type contains pointers if its element type does.  */
      return contains_pointers_p (TREE_TYPE (type));

    default:
      return 0;
    }
}

/* Output something to declare an external symbol to the assembler.
   (Most assemblers don't need this, so we normally output nothing.)
   Do nothing if DECL is not external.  */

void
assemble_external (decl)
     tree decl ATTRIBUTE_UNUSED;
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
     rtx fun ATTRIBUTE_UNUSED;
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
     const char *name;
{
  ASM_GLOBALIZE_LABEL (asm_out_file, name);
}

/* Assemble a label named NAME.  */

void
assemble_label (name)
     const char *name;
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
     const char *name;
{
  const char *real_name;
  tree id;

  STRIP_NAME_ENCODING (real_name, name);
  if (flag_prefix_function_name 
      && ! bcmp (real_name, CHKR_PREFIX, CHKR_PREFIX_SIZE))
    real_name = real_name + CHKR_PREFIX_SIZE;

  id = maybe_get_identifier (real_name);
  if (id)
    TREE_SYMBOL_REFERENCED (id) = 1;

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

#if 0
  if (flag_shared_data)
    data_section ();
#endif

  ASM_GENERATE_INTERNAL_LABEL (name, "LF", const_labelno);
  ++const_labelno;
  namestring = ggc_alloc_string (name, -1);

  x = gen_rtx_SYMBOL_REF (Pmode, namestring);

#ifdef ASM_OUTPUT_ALIGNED_DECL_LOCAL
  ASM_OUTPUT_ALIGNED_DECL_LOCAL (asm_out_file, NULL_TREE, name, size,
				 BIGGEST_ALIGNMENT);
#else
#ifdef ASM_OUTPUT_ALIGNED_LOCAL
  ASM_OUTPUT_ALIGNED_LOCAL (asm_out_file, name, size, BIGGEST_ALIGNMENT);
#else
  {
    /* Round size up to multiple of BIGGEST_ALIGNMENT bits
       so that each uninitialized object starts on such a boundary.  */
    /* Variable `rounded' might or might not be used in ASM_OUTPUT_LOCAL. */
    int rounded ATTRIBUTE_UNUSED
      = ((size + (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1)
	 / (BIGGEST_ALIGNMENT / BITS_PER_UNIT)
	 * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));
    ASM_OUTPUT_LOCAL (asm_out_file, name, size, rounded);
  }
#endif
#endif
  return x;
}

/* Assemble the static constant template for function entry trampolines.
   This is done at most once per compilation.
   Returns an RTX for the address of the template.  */

#ifdef TRAMPOLINE_TEMPLATE
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
  align = floor_log2 (TRAMPOLINE_ALIGNMENT / BITS_PER_UNIT);
  if (align > 0)
    ASM_OUTPUT_ALIGN (asm_out_file, align);

  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LTRAMP", 0);
  TRAMPOLINE_TEMPLATE (asm_out_file);

  /* Record the rtl to refer to it.  */
  ASM_GENERATE_INTERNAL_LABEL (label, "LTRAMP", 0);
  name = ggc_alloc_string (label, -1);
  return gen_rtx_SYMBOL_REF (Pmode, name);
}
#endif

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
     ASM_OUTPUT... macros.  */

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
      bzero ((char *) &d, sizeof d);
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
#ifdef ASM_OUTPUT_THREE_QUARTER_FLOAT
    case TQFmode:
      ASM_OUTPUT_THREE_QUARTER_FLOAT (asm_out_file, d);
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

/* Return a CONST_DOUBLE or CONST_INT for a value specified as a pair of ints.
   For an integer, I0 is the low-order word and I1 is the high-order word.
   For a real number, I0 is the word with the low address
   and I1 is the word with the high address.  */

rtx
immed_double_const (i0, i1, mode)
     HOST_WIDE_INT i0, i1;
     enum machine_mode mode;
{
  register rtx r;

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

      /* If this would be an entire word for the target, but is not for
	 the host, then sign-extend on the host so that the number will look
	 the same way on the host that it would on the target.

	 For example, when building a 64 bit alpha hosted 32 bit sparc
	 targeted compiler, then we want the 32 bit unsigned value -1 to be
	 represented as a 64 bit value -1, and not as 0x00000000ffffffff.
	 The later confuses the sparc backend.  */

      if (BITS_PER_WORD < HOST_BITS_PER_WIDE_INT && BITS_PER_WORD == width
	  && (i0 & ((HOST_WIDE_INT) 1 << (width - 1))))
	i0 |= ((HOST_WIDE_INT) (-1) << width);

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
	i1 = (i0 < 0) ? ~(HOST_WIDE_INT) 0 : 0;

      /* If this integer fits in one word, return a CONST_INT.  */
      if ((i1 == 0 && i0 >= 0)
	  || (i1 == ~0 && i0 < 0))
	return GEN_INT (i0);

      /* We use VOIDmode for integers.  */
      mode = VOIDmode;
    }

  /* Search the chain for an existing CONST_DOUBLE with the right value.
     If one is found, return it.  */
  if (cfun != 0)
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
  r = gen_rtx_CONST_DOUBLE (mode, NULL_RTX, i0, i1);
  pop_obstacks ();

  /* Don't touch const_double_chain if not inside any function.  */
  if (current_function_decl != 0)
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

  /* Get the desired `double' value as a sequence of ints
     since that is how they are stored in a CONST_DOUBLE.  */

  u.d = d;

  /* Detect special cases.  */

  if (REAL_VALUES_IDENTICAL (dconst0, d))
    return CONST0_RTX (mode);
  /* Check for NaN first, because some ports (specifically the i386) do not
     emit correct ieee-fp code by default, and thus will generate a core
     dump here if we pass a NaN to REAL_VALUES_EQUAL and if REAL_VALUES_EQUAL
     does a floating point comparison.  */
  else if (! REAL_VALUE_ISNAN (d) && REAL_VALUES_EQUAL (dconst1, d))
    return CONST1_RTX (mode);

  if (sizeof u == sizeof (HOST_WIDE_INT))
    return immed_double_const (u.i[0], 0, mode);
  if (sizeof u == 2 * sizeof (HOST_WIDE_INT))
    return immed_double_const (u.i[0], u.i[1], mode);

  /* The rest of this function handles the case where
     a float value requires more than 2 ints of space.
     It will be deleted as dead code on machines that don't need it.  */

  /* Search the chain for an existing CONST_DOUBLE with the right value.
     If one is found, return it.  */
  if (cfun != 0)
    for (r = const_double_chain; r; r = CONST_DOUBLE_CHAIN (r))
      if (! bcmp ((char *) &CONST_DOUBLE_LOW (r), (char *) &u, sizeof u)
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
  pop_obstacks ();
  PUT_MODE (r, mode);
  bcopy ((char *) &u, (char *) &CONST_DOUBLE_LOW (r), sizeof u);

  /* Don't touch const_double_chain if not inside any function.  */
  if (current_function_decl != 0)
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
      x = gen_rtx_MEM (FUNCTION_MODE,
		       gen_rtx_LABEL_REF (VOIDmode,
					  label_rtx (TREE_OPERAND (exp, 0))));
      break;

    case REAL_CST:
    case STRING_CST:
    case COMPLEX_CST:
    case CONSTRUCTOR:
    case INTEGER_CST:
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
  rtx rtl;
  char contents[1];
};

#define HASHBITS 30
#define MAX_HASH_TABLE 1009
static struct constant_descriptor *const_hash_table[MAX_HASH_TABLE];

/* Mark a const_hash_table descriptor for GC.  */

static void 
mark_const_hash_entry (ptr)
     void *ptr;
{
  struct constant_descriptor *desc = * (struct constant_descriptor **) ptr;

  while (desc)
    {
      ggc_mark_string (desc->label);
      ggc_mark_rtx (desc->rtl);
      desc = desc->next;
    }
}

/* Compute a hash code for a constant expression.  */

static int
const_hash (exp)
     tree exp;
{
  register char *p;
  register int len, hi, i;
  register enum tree_code code = TREE_CODE (exp);

  /* Either set P and LEN to the address and len of something to hash and
     exit the switch or return a value.  */

  switch (code)
    {
    case INTEGER_CST:
      p = (char *) &TREE_INT_CST_LOW (exp);
      len = 2 * sizeof TREE_INT_CST_LOW (exp);
      break;

    case REAL_CST:
      p = (char *) &TREE_REAL_CST (exp);
      len = sizeof TREE_REAL_CST (exp);
      break;

    case STRING_CST:
      p = TREE_STRING_POINTER (exp);
      len = TREE_STRING_LENGTH (exp);
      break;

    case COMPLEX_CST:
      return (const_hash (TREE_REALPART (exp)) * 5
	      + const_hash (TREE_IMAGPART (exp)));

    case CONSTRUCTOR:
      if (TREE_CODE (TREE_TYPE (exp)) == SET_TYPE)
	{
	  len = int_size_in_bytes (TREE_TYPE (exp));
	  p = (char *) alloca (len);
	  get_set_constructor_bytes (exp, (unsigned char *) p, len);
	  break;
	}
      else
	{
	  register tree link;

	  /* For record type, include the type in the hashing.
	     We do not do so for array types
	     because (1) the sizes of the elements are sufficient
	     and (2) distinct array types can have the same constructor.
	     Instead, we include the array size because the constructor could
	     be shorter.  */
	  if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE)
	    hi = ((unsigned long) TREE_TYPE (exp) & ((1 << HASHBITS) - 1))
	      % MAX_HASH_TABLE;
	  else
	    hi = ((5 + int_size_in_bytes (TREE_TYPE (exp)))
		  & ((1 << HASHBITS) - 1)) % MAX_HASH_TABLE;

	  for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	    if (TREE_VALUE (link))
	      hi
		= (hi * 603 + const_hash (TREE_VALUE (link))) % MAX_HASH_TABLE;

	  return hi;
	}

    case ADDR_EXPR:
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
	      hi = ((hi * 613) + (unsigned) (p[i]));
	  }
	else if (GET_CODE (value.base) == LABEL_REF)
	  hi = value.offset + CODE_LABEL_NUMBER (XEXP (value.base, 0)) * 13;
	else
	  abort();

	hi &= (1 << HASHBITS) - 1;
	hi %= MAX_HASH_TABLE;
      }
      return hi;

    case PLUS_EXPR:
    case MINUS_EXPR:
      return (const_hash (TREE_OPERAND (exp, 0)) * 9
	      + const_hash (TREE_OPERAND (exp, 1)));

    case NOP_EXPR:
    case CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      return const_hash (TREE_OPERAND (exp, 0)) * 7 + 2;
      
    default:
      abort ();
    }

  /* Compute hashing function */
  hi = len;
  for (i = 0; i < len; i++)
    hi = ((hi * 613) + (unsigned) (p[i]));

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

  /* Either set STRP, P and LEN to pointers and length to compare and exit the
     switch, or return the result of the comparison.  */

  switch (code)
    {
    case INTEGER_CST:
      /* Integer constants are the same only if the same width of type.  */
      if (*p++ != TYPE_PRECISION (TREE_TYPE (exp)))
	return 0;

      strp = (char *) &TREE_INT_CST_LOW (exp);
      len = 2 * sizeof TREE_INT_CST_LOW (exp);
      break;

    case REAL_CST:
      /* Real constants are the same only if the same width of type.  */
      if (*p++ != TYPE_PRECISION (TREE_TYPE (exp)))
	return 0;

      strp = (char *) &TREE_REAL_CST (exp);
      len = sizeof TREE_REAL_CST (exp);
      break;

    case STRING_CST:
      if (flag_writable_strings)
	return 0;

      if (*p++ != TYPE_MODE (TREE_TYPE (exp)))
	return 0;

      strp = TREE_STRING_POINTER (exp);
      len = TREE_STRING_LENGTH (exp);
      if (bcmp ((char *) &TREE_STRING_LENGTH (exp), p,
		sizeof TREE_STRING_LENGTH (exp)))
	return 0;

      p += sizeof TREE_STRING_LENGTH (exp);
      break;

    case COMPLEX_CST:
      p = compare_constant_1 (TREE_REALPART (exp), p);
      if (p == 0)
	return 0;

      return compare_constant_1 (TREE_IMAGPART (exp), p);

    case CONSTRUCTOR:
      if (TREE_CODE (TREE_TYPE (exp)) == SET_TYPE)
	{
	  int xlen = len = int_size_in_bytes (TREE_TYPE (exp));

	  strp = (char *) alloca (len);
	  get_set_constructor_bytes (exp, (unsigned char *) strp, len);
	  if (bcmp ((char *) &xlen, p, sizeof xlen))
	    return 0;

	  p += sizeof xlen;
	  break;
	}
      else
	{
	  register tree link;
	  int length = list_length (CONSTRUCTOR_ELTS (exp));
	  tree type;
	  enum machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
	  int have_purpose = 0;

	  for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	    if (TREE_PURPOSE (link))
	      have_purpose = 1;

	  if (bcmp ((char *) &length, p, sizeof length))
	    return 0;

	  p += sizeof length;

	  /* For record constructors, insist that the types match.
	     For arrays, just verify both constructors are for arrays. 
	     Then insist that either both or none have any TREE_PURPOSE
	     values.  */
	  if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE)
	    type = TREE_TYPE (exp);
	  else
	    type = 0;

	  if (bcmp ((char *) &type, p, sizeof type))
	    return 0;

	  if (TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE)
	    {
	      if (bcmp ((char *) &mode, p, sizeof mode))
		return 0;

	      p += sizeof mode;
	    }

	  p += sizeof type;

	  if (bcmp ((char *) &have_purpose, p, sizeof have_purpose))
	    return 0;

	  p += sizeof have_purpose;

	  /* For arrays, insist that the size in bytes match.  */
	  if (TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE)
	    {
	      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (exp));

	      if (bcmp ((char *) &size, p, sizeof size))
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

		  if (bcmp ((char *) &zero, p, sizeof zero))
		    return 0;

		  p += sizeof zero;
		}

	      if (TREE_PURPOSE (link)
		  && TREE_CODE (TREE_PURPOSE (link)) == FIELD_DECL)
		{
		  if (bcmp ((char *) &TREE_PURPOSE (link), p,
			    sizeof TREE_PURPOSE (link)))
		    return 0;

		  p += sizeof TREE_PURPOSE (link);
		}
	      else if (TREE_PURPOSE (link))
		{
		  if ((p = compare_constant_1 (TREE_PURPOSE (link), p)) == 0)
		    return 0;
		}
	      else if (have_purpose)
		{
		  int zero = 0;

		  if (bcmp ((char *) &zero, p, sizeof zero))
		    return 0;

		  p += sizeof zero;
		}
	    }

	  return p;
	}

    case ADDR_EXPR:
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
      break;

    case PLUS_EXPR:
    case MINUS_EXPR:
    case RANGE_EXPR:
      p = compare_constant_1 (TREE_OPERAND (exp, 0), p);
      if (p == 0)
	return 0;

      return compare_constant_1 (TREE_OPERAND (exp, 1), p);

    case NOP_EXPR:
    case CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      return compare_constant_1 (TREE_OPERAND (exp, 0), p);

    default:
      abort ();
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
  rtx rtl = 0;

  /* Make a struct constant_descriptor.  The first three pointers will
     be filled in later.  Here we just leave space for them.  */

  obstack_grow (&permanent_obstack, (char *) &next, sizeof next);
  obstack_grow (&permanent_obstack, (char *) &label, sizeof label);
  obstack_grow (&permanent_obstack, (char *) &rtl, sizeof rtl);
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

  switch (code)
    {
    case INTEGER_CST:
      obstack_1grow (&permanent_obstack, TYPE_PRECISION (TREE_TYPE (exp)));
      strp = (char *) &TREE_INT_CST_LOW (exp);
      len = 2 * sizeof TREE_INT_CST_LOW (exp);
      break;

    case REAL_CST:
      obstack_1grow (&permanent_obstack, TYPE_PRECISION (TREE_TYPE (exp)));
      strp = (char *) &TREE_REAL_CST (exp);
      len = sizeof TREE_REAL_CST (exp);
      break;

    case STRING_CST:
      if (flag_writable_strings)
	return;

      obstack_1grow (&permanent_obstack, TYPE_MODE (TREE_TYPE (exp)));
      strp = TREE_STRING_POINTER (exp);
      len = TREE_STRING_LENGTH (exp);
      obstack_grow (&permanent_obstack, (char *) &TREE_STRING_LENGTH (exp),
		    sizeof TREE_STRING_LENGTH (exp));
      break;

    case COMPLEX_CST:
      record_constant_1 (TREE_REALPART (exp));
      record_constant_1 (TREE_IMAGPART (exp));
      return;

    case CONSTRUCTOR:
      if (TREE_CODE (TREE_TYPE (exp)) == SET_TYPE)
	{
	  int nbytes = int_size_in_bytes (TREE_TYPE (exp));
	  obstack_grow (&permanent_obstack, &nbytes, sizeof (nbytes));
	  obstack_blank (&permanent_obstack, nbytes);
	  get_set_constructor_bytes
	    (exp, (unsigned char *) permanent_obstack.next_free-nbytes,
	     nbytes);
	  return;
	}
      else
	{
	  register tree link;
	  int length = list_length (CONSTRUCTOR_ELTS (exp));
	  enum machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
	  tree type;
	  int have_purpose = 0;

	  for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	    if (TREE_PURPOSE (link))
	      have_purpose = 1;

	  obstack_grow (&permanent_obstack, (char *) &length, sizeof length);

	  /* For record constructors, insist that the types match.
	     For arrays, just verify both constructors are for arrays
	     of the same mode.  Then insist that either both or none
	     have any TREE_PURPOSE values.  */
	  if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE)
	    type = TREE_TYPE (exp);
	  else
	    type = 0;

	  obstack_grow (&permanent_obstack, (char *) &type, sizeof type);
	  if (TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE)
	    obstack_grow (&permanent_obstack, &mode, sizeof mode);
			  
	  obstack_grow (&permanent_obstack, (char *) &have_purpose,
			sizeof have_purpose);

	  /* For arrays, insist that the size in bytes match.  */
	  if (TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE)
	    {
	      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (exp));
	      obstack_grow (&permanent_obstack, (char *) &size, sizeof size);
	    }

	  for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	    {
	      if (TREE_VALUE (link))
		record_constant_1 (TREE_VALUE (link));
	      else
		{
		  tree zero = 0;

		  obstack_grow (&permanent_obstack,
				(char *) &zero, sizeof zero);
		}

	      if (TREE_PURPOSE (link)
		  && TREE_CODE (TREE_PURPOSE (link)) == FIELD_DECL)
		obstack_grow (&permanent_obstack,
			      (char *) &TREE_PURPOSE (link),
			      sizeof TREE_PURPOSE (link));
	      else if (TREE_PURPOSE (link))
		record_constant_1 (TREE_PURPOSE (link));
	      else if (have_purpose)
		{
		  int zero = 0;

		  obstack_grow (&permanent_obstack,
				(char *) &zero, sizeof zero);
		}
	    }
	}
      return;

    case ADDR_EXPR:
      {
	struct addr_const value;

	decode_addr_const (exp, &value);
	/* Record the offset.  */
	obstack_grow (&permanent_obstack,
		      (char *) &value.offset, sizeof value.offset);
	/* Record the symbol name.  */
	obstack_grow (&permanent_obstack, XSTR (value.base, 0),
		      strlen (XSTR (value.base, 0)) + 1);
      }
      return;

    case PLUS_EXPR:
    case MINUS_EXPR:
    case RANGE_EXPR:
      record_constant_1 (TREE_OPERAND (exp, 0));
      record_constant_1 (TREE_OPERAND (exp, 1));
      return;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      record_constant_1 (TREE_OPERAND (exp, 0));
      return;

    default:
      abort ();
    }

  /* Record constant contents.  */
  obstack_grow (&permanent_obstack, strp, len);
}

/* Record a list of constant expressions that were passed to
   output_constant_def but that could not be output right away.  */

struct deferred_constant
{
  struct deferred_constant *next;
  tree exp;
  int reloc;
  int labelno;
};

static struct deferred_constant *deferred_constants;

/* Another list of constants which should be output after the
   function.  */
static struct deferred_constant *after_function_constants;

/* Nonzero means defer output of addressed subconstants
   (i.e., those for which output_constant_def is called.)  */
static int defer_addressed_constants_flag;

/* Start deferring output of subconstants.  */

void
defer_addressed_constants ()
{
  defer_addressed_constants_flag++;
}

/* Stop deferring output of subconstants,
   and output now all those that have been deferred.  */

void
output_deferred_addressed_constants ()
{
  struct deferred_constant *p, *next;

  defer_addressed_constants_flag--;

  if (defer_addressed_constants_flag > 0)
    return;

  for (p = deferred_constants; p; p = next)
    {
      output_constant_def_contents (p->exp, p->reloc, p->labelno);
      next = p->next;
      free (p);
    }

  deferred_constants = 0;
}

/* Output any constants which should appear after a function.  */

static void
output_after_function_constants ()
{
  struct deferred_constant *p, *next;

  for (p = after_function_constants; p; p = next)
    {
      output_constant_def_contents (p->exp, p->reloc, p->labelno);
      next = p->next;
      free (p);
    }

  after_function_constants = 0;
}

/* Make a copy of the whole tree structure for a constant.
   This handles the same types of nodes that compare_constant
   and record_constant handle.  */

static tree
copy_constant (exp)
     tree exp;
{
  switch (TREE_CODE (exp))
    {
    case ADDR_EXPR:
      /* For ADDR_EXPR, we do not want to copy the decl whose address
	 is requested.  We do want to copy constants though.  */
      if (TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, 0))) == 'c')
	return build1 (TREE_CODE (exp), TREE_TYPE (exp),
		       copy_constant (TREE_OPERAND (exp, 0)));
      else
	return copy_node (exp);

    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
      return copy_node (exp);

    case COMPLEX_CST:
      return build_complex (TREE_TYPE (exp),
			    copy_constant (TREE_REALPART (exp)),
			    copy_constant (TREE_IMAGPART (exp)));

    case PLUS_EXPR:
    case MINUS_EXPR:
      return build (TREE_CODE (exp), TREE_TYPE (exp),
		    copy_constant (TREE_OPERAND (exp, 0)),
		    copy_constant (TREE_OPERAND (exp, 1)));

    case NOP_EXPR:
    case CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      return build1 (TREE_CODE (exp), TREE_TYPE (exp),
		     copy_constant (TREE_OPERAND (exp, 0)));

    case CONSTRUCTOR:
      {
	tree copy = copy_node (exp);
	tree list = copy_list (CONSTRUCTOR_ELTS (exp));
	tree tail;

	CONSTRUCTOR_ELTS (copy) = list;
	for (tail = list; tail; tail = TREE_CHAIN (tail))
	  TREE_VALUE (tail) = copy_constant (TREE_VALUE (tail));
	if (TREE_CODE (TREE_TYPE (exp)) == SET_TYPE)
	  for (tail = list; tail; tail = TREE_CHAIN (tail))
	    TREE_PURPOSE (tail) = copy_constant (TREE_PURPOSE (tail));

	return copy;
      }

    default:
      abort ();
    }
}

/* Return an rtx representing a reference to constant data in memory
   for the constant expression EXP.

   If assembler code for such a constant has already been output,
   return an rtx to refer to it.
   Otherwise, output such a constant in memory (or defer it for later)
   and generate an rtx for it.

   The TREE_CST_RTL of EXP is set up to point to that rtx.
   The const_hash_table records which constants already have label strings.  */

rtx
output_constant_def (exp)
     tree exp;
{
  register int hash;
  register struct constant_descriptor *desc;
  char label[256];
  int reloc;
  int found = 1;

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
      break;
      
  if (desc == 0)
    {
      /* No constant equal to EXP is known to have been output.
	 Make a constant descriptor to enter EXP in the hash table.
	 Assign the label number and record it in the descriptor for
	 future calls to this function to find.  */
	  
      /* Create a string containing the label name, in LABEL.  */
      ASM_GENERATE_INTERNAL_LABEL (label, "LC", const_labelno);

      desc = record_constant (exp);
      desc->next = const_hash_table[hash];
      desc->label = ggc_alloc_string (label, -1);
      const_hash_table[hash] = desc;
  
      /* We have a symbol name; construct the SYMBOL_REF and the MEM
	 in the permanent obstack.  We could also construct this in the
	 obstack of EXP and put it into TREE_CST_RTL, but we have no way
	 of knowing what obstack it is (e.g., it might be in a function
	 obstack of a function we are nested inside).  */

      push_obstacks_nochange ();
      end_temporary_allocation ();

      desc->rtl
	= gen_rtx_MEM (TYPE_MODE (TREE_TYPE (exp)),
		       gen_rtx_SYMBOL_REF (Pmode, desc->label));

      RTX_UNCHANGING_P (desc->rtl) = 1;
      if (AGGREGATE_TYPE_P (TREE_TYPE (exp)))
	MEM_SET_IN_STRUCT_P (desc->rtl, 1);

      pop_obstacks ();

      found = 0;
    }

  TREE_CST_RTL (exp) = desc->rtl;

  /* Optionally set flags or add text to the name to record information
     such as that it is a function name.  If the name is changed, the macro
     ASM_OUTPUT_LABELREF will have to know how to strip this information.  */
#ifdef ENCODE_SECTION_INFO
  ENCODE_SECTION_INFO (exp);
#endif

  /* If this is the first time we've seen this particular constant,
     output it (or defer its output for later).  */
  if (! found)
    {
      int after_function = 0;

#ifdef CONSTANT_AFTER_FUNCTION_P
      if (current_function_decl != 0
	  && CONSTANT_AFTER_FUNCTION_P (exp))
	after_function = 1;
#endif

      if (defer_addressed_constants_flag || after_function)
	{
	  struct deferred_constant *p;
	  p = (struct deferred_constant *) xmalloc (sizeof (struct deferred_constant));

	  push_obstacks_nochange ();
	  suspend_momentary ();
	  p->exp = copy_constant (exp);
	  pop_obstacks ();
	  p->reloc = reloc;
	  p->labelno = const_labelno++;
	  if (after_function)
	    {
	      p->next = after_function_constants;
	      after_function_constants = p;
	    }
	  else
	    {
	      p->next = deferred_constants;
	      deferred_constants = p;
	    }
	}
      else
	{
	  /* Do no output if -fsyntax-only.  */
	  if (! flag_syntax_only)
	    output_constant_def_contents (exp, reloc, const_labelno);
	  ++const_labelno;
	}
    }

  return TREE_CST_RTL (exp);
}

/* Now output assembler code to define the label for EXP,
   and follow it with the data of EXP.  */

static void
output_constant_def_contents (exp, reloc, labelno)
     tree exp;
     int reloc;
     int labelno;
{
  int align;

  if (IN_NAMED_SECTION (exp))
    named_section (exp, NULL, reloc);
  else
    {
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
    }

  /* Align the location counter as required by EXP's data type.  */
  align = TYPE_ALIGN (TREE_TYPE (exp));
#ifdef CONSTANT_ALIGNMENT
  align = CONSTANT_ALIGNMENT (exp, align);
#endif

  if (align > BITS_PER_UNIT)
    ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (align / BITS_PER_UNIT));

  /* Output the label itself.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LC", labelno);

  /* Output the value of EXP.  */
  output_constant (exp,
		   (TREE_CODE (exp) == STRING_CST
		    ? TREE_STRING_LENGTH (exp)
		    : int_size_in_bytes (TREE_TYPE (exp))));

}

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
  int mark;
};

/* Structure used to maintain hash table mapping symbols used to their
   corresponding constants.  */

struct pool_sym
{
  char *label;
  struct pool_constant *pool;
  struct pool_sym *next;
};

/* Hash code for a SYMBOL_REF with CONSTANT_POOL_ADDRESS_P true.
   The argument is XSTR (... , 0)  */

#define SYMHASH(LABEL)	\
  ((((unsigned long) (LABEL)) & ((1 << HASHBITS) - 1))  % MAX_RTX_HASH_TABLE)

/* Initialize constant pool hashing for a new function.  */

void
init_varasm_status (f)
     struct function *f;
{
  struct varasm_status *p;
  p = (struct varasm_status *) xmalloc (sizeof (struct varasm_status));
  f->varasm = p;
  p->x_const_rtx_hash_table
    = ((struct constant_descriptor **)
       xmalloc (MAX_RTX_HASH_TABLE * sizeof (struct constant_descriptor *)));
  p->x_const_rtx_sym_hash_table
    = ((struct pool_sym **)
       xmalloc (MAX_RTX_HASH_TABLE * sizeof (struct pool_sym *)));
  bzero ((char *) p->x_const_rtx_hash_table,
	 MAX_RTX_HASH_TABLE * sizeof (struct constant_descriptor *));
  bzero ((char *) p->x_const_rtx_sym_hash_table,
	 MAX_RTX_HASH_TABLE * sizeof (struct pool_sym *));

  p->x_first_pool = p->x_last_pool = 0;
  p->x_pool_offset = 0;
  p->x_const_double_chain = 0;
}

/* Mark PC for GC.  */

static void 
mark_pool_constant (pc)
     struct pool_constant *pc;
{
  while (pc)
    {
      ggc_mark_rtx (pc->constant);
      pc = pc->next;
    }
}

/* Mark PPS for GC.  */

static void
mark_pool_sym_hash_table (pps)
     struct pool_sym **pps;
{
  struct pool_sym *ps;
  int i;

  for (i = 0; i < MAX_RTX_HASH_TABLE; ++i)
    for (ps = pps[i]; ps ; ps = ps->next)
      ggc_mark_string (ps->label);
}

/* Mark P for GC.  */

void
mark_varasm_status (p)
     struct varasm_status *p;
{
  if (p == NULL)
    return;

  mark_pool_constant (p->x_first_pool);
  mark_pool_sym_hash_table (p->x_const_rtx_sym_hash_table);
  ggc_mark_rtx (p->x_const_double_chain);
}

/* Clear out all parts of the state in F that can safely be discarded
   after the function has been compiled, to let garbage collection
   reclaim the memory.  */

void
free_varasm_status (f)
     struct function *f;
{
  struct varasm_status *p;

  p = f->varasm;
  free (p->x_const_rtx_hash_table);
  free (p->x_const_rtx_sym_hash_table);
  free (p);
  f->varasm = NULL;
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
    struct {HOST_WIDE_INT high, low;} di;
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

  value->kind = RTX_INT;	/* Most usual kind.  */
  value->mode = mode;

  switch (GET_CODE (x))
    {
    case CONST_DOUBLE:
      value->kind = RTX_DOUBLE;
      if (GET_MODE (x) != VOIDmode)
	{
	  value->mode = GET_MODE (x);
	  bcopy ((char *) &CONST_DOUBLE_LOW (x),
		 (char *) &value->un.du, sizeof value->un.du);
	}
      else
	{
	  value->un.di.low = CONST_DOUBLE_LOW (x);
	  value->un.di.high = CONST_DOUBLE_HIGH (x);
	}
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
	/* Use the string's address, not the SYMBOL_REF's address,
	   for the sake of addresses of library routines.  */
	value->un.addr.base = (rtx) XSTR (value->un.addr.base, 0);
	break;

      case LABEL_REF:
	/* For a LABEL_REF, compare labels.  */
	value->un.addr.base = XEXP (value->un.addr.base, 0);
	
      default:
	break;
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

static int
const_hash_rtx (mode, x)
     enum machine_mode mode;
     rtx x;
{
  register int hi;
  register size_t i;

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
  rtx rtl;
  struct rtx_const value;

  decode_rtx_const (mode, x, &value);

  /* Put these things in the saveable obstack so we can ensure it won't
     be freed if we are called from combine or some other phase that discards
     memory allocated from function_obstack (current_obstack).  */
  obstack_grow (saveable_obstack, &ptr, sizeof ptr);
  obstack_grow (saveable_obstack, &label, sizeof label);
  obstack_grow (saveable_obstack, &rtl, sizeof rtl);

  /* Record constant contents.  */
  obstack_grow (saveable_obstack, &value, sizeof value);

  return (struct constant_descriptor *) obstack_finish (saveable_obstack);
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
#ifdef CONSTANT_ALIGNMENT
      align = CONSTANT_ALIGNMENT (make_tree (type_for_mode (mode, 0), x),
				 align * BITS_PER_UNIT) / BITS_PER_UNIT;
#endif

      pool_offset += align - 1;
      pool_offset &= ~ (align - 1);

      /* If RTL is not being placed into the saveable obstack, make a
	 copy of X that is in the saveable obstack in case we are
	 being called from combine or some other phase that discards
	 memory it allocates.  We used to only do this if it is a
	 CONST; however, reload can allocate a CONST_INT when
	 eliminating registers.  */
      if (rtl_obstack != saveable_obstack
	  && (GET_CODE (x) == CONST || GET_CODE (x) == CONST_INT))
	{
	  push_obstacks_nochange ();
	  rtl_in_saveable_obstack ();

	  if (GET_CODE (x) == CONST)
	    x = gen_rtx_CONST (GET_MODE (x), 
			       gen_rtx_PLUS (GET_MODE (x), 
					     XEXP (XEXP (x, 0), 0),
					     XEXP (XEXP (x, 0), 1)));
	  else
	    x = GEN_INT (INTVAL (x));

	  pop_obstacks ();
	}

      /* Allocate a pool constant descriptor, fill it in, and chain it in.  */

      pool = (struct pool_constant *) savealloc (sizeof (struct pool_constant));
      pool->desc = desc;
      pool->constant = x;
      pool->mode = mode;
      pool->labelno = const_labelno;
      pool->align = align;
      pool->offset = pool_offset;
      pool->mark = 1;
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

      desc->label = found = ggc_alloc_string (label, -1);

      /* Add label to symbol hash table.  */
      hash = SYMHASH (found);
      sym = (struct pool_sym *) savealloc (sizeof (struct pool_sym));
      sym->label = found;
      sym->pool = pool;
      sym->next = const_rtx_sym_hash_table[hash];
      const_rtx_sym_hash_table[hash] = sym;
    }

  /* We have a symbol name; construct the SYMBOL_REF and the MEM.  */

  def = gen_rtx_MEM (mode, gen_rtx_SYMBOL_REF (Pmode, found));

  RTX_UNCHANGING_P (def) = 1;
  /* Mark the symbol_ref as belonging to this constants pool.  */
  CONSTANT_POOL_ADDRESS_P (XEXP (def, 0)) = 1;
  current_function_uses_const_pool = 1;

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
find_pool_constant (f, addr)
     struct function *f;
     rtx addr;
{
  struct pool_sym *sym;
  char *label = XSTR (addr, 0);

  for (sym = f->varasm->x_const_rtx_sym_hash_table[SYMHASH (label)]; sym; sym = sym->next)
    if (sym->label == label)
      return sym->pool;

  abort ();
}

/* Given a constant pool SYMBOL_REF, return the corresponding constant.  */

rtx
get_pool_constant (addr)
     rtx addr;
{
  return (find_pool_constant (cfun, addr))->constant;
}

/* Likewise, but for the constant pool of a specific function.  */

rtx
get_pool_constant_for_function (f, addr)
     struct function *f;
     rtx addr;
{
  return (find_pool_constant (f, addr))->constant;
}

/* Similar, return the mode.  */

enum machine_mode
get_pool_mode (addr)
     rtx addr;
{
  return (find_pool_constant (cfun, addr))->mode;
}

enum machine_mode
get_pool_mode_for_function (f, addr)
     struct function *f;
     rtx addr;
{
  return (find_pool_constant (f, addr))->mode;
}

/* Similar, return the offset in the constant pool.  */

int
get_pool_offset (addr)
     rtx addr;
{
  return (find_pool_constant (cfun, addr))->offset;
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
  const char *fnname ATTRIBUTE_UNUSED;
  tree fndecl ATTRIBUTE_UNUSED;
{
  struct pool_constant *pool;
  rtx x;
  union real_extract u;

  /* It is possible for gcc to call force_const_mem and then to later
     discard the instructions which refer to the constant.  In such a
     case we do not need to output the constant.  */
  mark_constant_pool ();

#ifdef ASM_OUTPUT_POOL_PROLOGUE
  ASM_OUTPUT_POOL_PROLOGUE (asm_out_file, fnname, fndecl, pool_offset);
#endif

  for (pool = first_pool; pool; pool = pool->next)
    {
      x = pool->constant;

      if (! pool->mark)
	continue;

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
	ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (pool->align));

      /* Output the label.  */
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LC", pool->labelno);

      /* Output the value of the constant itself.  */
      switch (GET_MODE_CLASS (pool->mode))
	{
	case MODE_FLOAT:
	  if (GET_CODE (x) != CONST_DOUBLE)
	    abort ();

	  bcopy ((char *) &CONST_DOUBLE_LOW (x), (char *) &u, sizeof u);
	  assemble_real (u.d, pool->mode);
	  break;

	case MODE_INT:
	case MODE_PARTIAL_INT:
	  assemble_integer (x, GET_MODE_SIZE (pool->mode), 1);
	  break;

	default:
	  abort ();
	}

#ifdef ASM_OUTPUT_SPECIAL_POOL_ENTRY
    done: ;
#endif

    }

#ifdef ASM_OUTPUT_POOL_EPILOGUE
  ASM_OUTPUT_POOL_EPILOGUE (asm_out_file, fnname, fndecl, pool_offset);
#endif

  /* Done with this pool.  */
  first_pool = last_pool = 0;
}

/* Look through the instructions for this function, and mark all the
   entries in the constant pool which are actually being used.  */

static void
mark_constant_pool ()
{
  register rtx insn;
  struct pool_constant *pool;

  if (first_pool == 0)
    return;

  for (pool = first_pool; pool; pool = pool->next)
    pool->mark = 0;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
      mark_constants (PATTERN (insn));

  for (insn = current_function_epilogue_delay_list;
       insn;
       insn = XEXP (insn, 1))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
      mark_constants (PATTERN (insn));

  /* It's possible that the only reference to a symbol is in a symbol
     that's in the constant pool.  This happens in Fortran under some
     situations.  (When the constant contains the address of another
     constant, and only the first is used directly in an insn.) 
     This is potentially suboptimal if there's ever a possibility of
     backwards (in pool order) 2'd level references.  However, it's
     not clear that 2'd level references can happen. */
  for (pool = first_pool; pool; pool = pool->next)
    {
      struct pool_sym *sym;
      char *label;

      /* skip unmarked entries; no insn refers to them. */
      if (!pool->mark)
	  continue;

      /* Skip everything except SYMBOL_REFs.  */
      if (GET_CODE (pool->constant) != SYMBOL_REF)
	continue;
      label = XSTR (pool->constant, 0);

      /* Be sure the symbol's value is marked. */
      for (sym = const_rtx_sym_hash_table[SYMHASH (label)]; sym; 
           sym = sym->next)
	  if (sym->label == label)
	    sym->pool->mark = 1;
      /* If we didn't find it, there's something truly wrong here, but it
	 will be announced by the assembler. */
    }
}

static void
mark_constants (x)
     register rtx x;
{
  register int i;
  register const char *format_ptr;

  if (x == 0)
    return;

  if (GET_CODE (x) == SYMBOL_REF)
    {
      if (CONSTANT_POOL_ADDRESS_P (x))
	find_pool_constant (cfun, x)->mark = 1;
      return;
    }
  /* Never search inside a CONST_DOUBLE, because CONST_DOUBLE_MEM may be
     a MEM, but does not constitute a use of that MEM.  */
  else if (GET_CODE (x) == CONST_DOUBLE)
    return;

  /* Insns may appear inside a SEQUENCE.  Only check the patterns of
     insns, not any notes that may be attached.  We don't want to mark
     a constant just because it happens to appear in a REG_EQUIV note.  */
  if (GET_RTX_CLASS (GET_CODE (x)) == 'i')
    {
      mark_constants (PATTERN (x));
      return;
    }

  format_ptr = GET_RTX_FORMAT (GET_CODE (x));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (x)); i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
	  mark_constants (XEXP (x, i));
	  break;

	case 'E':
	  if (XVEC (x, i) != 0)
	    {
	      register int j;

	      for (j = 0; j < XVECLEN (x, i); j++)
		mark_constants (XVECEXP (x, i, j));
	    }
	  break;

	case 'S':
	case 's':
	case '0':
	case 'i':
	case 'w':
	case 'n':
	case 'u':
	  break;

	default:
	  abort ();
	}
    }
}

/* Find all the constants whose addresses are referenced inside of EXP,
   and make sure assembler code with a label has been output for each one.
   Indicate whether an ADDR_EXPR has been encountered.  */

static int
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

    default:
      break;
    }
  return reloc;
}

/* Return nonzero if VALUE is a valid constant-valued expression
   for use in initializing a static variable; one that can be an
   element of a "constant" initializer.

   Return null_pointer_node if the value is absolute;
   if it is relocatable, return the variable that determines the relocation.
   We assume that VALUE has been folded as much as possible;
   therefore, we do not need to check for such things as
   arithmetic-combinations of integers.  */

tree
initializer_constant_valid_p (value, endtype)
     tree value;
     tree endtype;
{
  switch (TREE_CODE (value))
    {
    case CONSTRUCTOR:
      if ((TREE_CODE (TREE_TYPE (value)) == UNION_TYPE
	   || TREE_CODE (TREE_TYPE (value)) == RECORD_TYPE)
	  && TREE_CONSTANT (value)
	  && CONSTRUCTOR_ELTS (value))
	return
	  initializer_constant_valid_p (TREE_VALUE (CONSTRUCTOR_ELTS (value)),
					endtype);
	
      return TREE_STATIC (value) ? null_pointer_node : 0;

    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
    case COMPLEX_CST:
      return null_pointer_node;

    case ADDR_EXPR:
      return TREE_OPERAND (value, 0);

    case NON_LVALUE_EXPR:
      return initializer_constant_valid_p (TREE_OPERAND (value, 0), endtype);

    case CONVERT_EXPR:
    case NOP_EXPR:
      /* Allow conversions between pointer types.  */
      if (POINTER_TYPE_P (TREE_TYPE (value))
	  && POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (value, 0))))
	return initializer_constant_valid_p (TREE_OPERAND (value, 0), endtype);

      /* Allow conversions between real types.  */
      if (FLOAT_TYPE_P (TREE_TYPE (value))
	  && FLOAT_TYPE_P (TREE_TYPE (TREE_OPERAND (value, 0))))
	return initializer_constant_valid_p (TREE_OPERAND (value, 0), endtype);

      /* Allow length-preserving conversions between integer types.  */
      if (INTEGRAL_TYPE_P (TREE_TYPE (value))
	  && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (value, 0)))
	  && (TYPE_PRECISION (TREE_TYPE (value))
	      == TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (value, 0)))))
	return initializer_constant_valid_p (TREE_OPERAND (value, 0), endtype);

      /* Allow conversions between other integer types only if
	 explicit value.  */
      if (INTEGRAL_TYPE_P (TREE_TYPE (value))
	  && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (value, 0))))
	{
	  tree inner = initializer_constant_valid_p (TREE_OPERAND (value, 0),
						     endtype);
	  if (inner == null_pointer_node)
	    return null_pointer_node;
	  break;
	}

      /* Allow (int) &foo provided int is as wide as a pointer.  */
      if (INTEGRAL_TYPE_P (TREE_TYPE (value))
	  && POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (value, 0)))
	  && (TYPE_PRECISION (TREE_TYPE (value))
	      >= TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (value, 0)))))
	return initializer_constant_valid_p (TREE_OPERAND (value, 0),
					     endtype);

      /* Likewise conversions from int to pointers, but also allow
	 conversions from 0.  */
      if (POINTER_TYPE_P (TREE_TYPE (value))
	  && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (value, 0))))
	{
	  if (integer_zerop (TREE_OPERAND (value, 0)))
	    return null_pointer_node;
	  else if (TYPE_PRECISION (TREE_TYPE (value))
		   <= TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (value, 0))))
	    return initializer_constant_valid_p (TREE_OPERAND (value, 0),
						 endtype);
	}

      /* Allow conversions to union types if the value inside is okay.  */
      if (TREE_CODE (TREE_TYPE (value)) == UNION_TYPE)
	return initializer_constant_valid_p (TREE_OPERAND (value, 0),
					     endtype);
      break;

    case PLUS_EXPR:
      if (! INTEGRAL_TYPE_P (endtype)
	  || TYPE_PRECISION (endtype) >= POINTER_SIZE)
        {
	  tree valid0 = initializer_constant_valid_p (TREE_OPERAND (value, 0),
						      endtype);
	  tree valid1 = initializer_constant_valid_p (TREE_OPERAND (value, 1),
						      endtype);
	  /* If either term is absolute, use the other terms relocation.  */
	  if (valid0 == null_pointer_node)
	    return valid1;
	  if (valid1 == null_pointer_node)
	    return valid0;
        }
      break;

    case MINUS_EXPR:
      if (! INTEGRAL_TYPE_P (endtype)
	  || TYPE_PRECISION (endtype) >= POINTER_SIZE)
	{
	  tree valid0 = initializer_constant_valid_p (TREE_OPERAND (value, 0),
						      endtype);
	  tree valid1 = initializer_constant_valid_p (TREE_OPERAND (value, 1),
						      endtype);
	  /* Win if second argument is absolute.  */
	  if (valid1 == null_pointer_node)
	    return valid0;
	  /* Win if both arguments have the same relocation.
	     Then the value is absolute.  */
	  if (valid0 == valid1 && valid0 != 0)
	    return null_pointer_node;
	}

      /* Support differences between labels.  */
      if (INTEGRAL_TYPE_P (endtype))
	{
	  tree op0, op1;
	  op0 = TREE_OPERAND (value, 0);
	  op1 = TREE_OPERAND (value, 1);
	  STRIP_NOPS (op0);
	  STRIP_NOPS (op1);

	  if (TREE_CODE (op0) == ADDR_EXPR
	      && TREE_CODE (TREE_OPERAND (op0, 0)) == LABEL_DECL
	      && TREE_CODE (op1) == ADDR_EXPR
	      && TREE_CODE (TREE_OPERAND (op1, 0)) == LABEL_DECL)
	    return null_pointer_node;
	}
      break;

    default:
      break;
    }

  return 0;
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

  /* Some front-ends use constants other than the standard
     language-indepdent varieties, but which may still be output
     directly.  Give the front-end a chance to convert EXP to a
     language-independent representation.  */
  if (lang_expand_constant)
    exp = (*lang_expand_constant) (exp);

  if (size == 0 || flag_syntax_only)
    return;

  /* Eliminate the NON_LVALUE_EXPR_EXPR that makes a cast not be an lvalue.
     That way we get the constant (we hope) inside it.  Also, strip off any
     NOP_EXPR that converts between two record, union, array, or set types.  */
  while ((TREE_CODE (exp) == NOP_EXPR 
	  && (TREE_TYPE (exp) == TREE_TYPE (TREE_OPERAND (exp, 0))
	      || AGGREGATE_TYPE_P (TREE_TYPE (exp))))
	 || TREE_CODE (exp) == NON_LVALUE_EXPR)
    exp = TREE_OPERAND (exp, 0);

  /* Allow a constructor with no elements for any data type.
     This means to fill the space with zeros.  */
  if (TREE_CODE (exp) == CONSTRUCTOR && CONSTRUCTOR_ELTS (exp) == 0)
    {
      assemble_zeros (size);
      return;
    }

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

    case SET_TYPE:
      if (TREE_CODE (exp) == INTEGER_CST)
	assemble_integer (expand_expr (exp, NULL_RTX,
				       VOIDmode, EXPAND_INITIALIZER),
			  size, 1);
      else if (TREE_CODE (exp) == CONSTRUCTOR)
	{
	  unsigned char *buffer = (unsigned char *) alloca (size);
	  if (get_set_constructor_bytes (exp, buffer, size))
	    abort ();
	  assemble_string ((char *) buffer, size);
	}
      else
	error ("unknown set constructor type");
      return;

    default:
      break; /* ??? */
    }

  if (size > 0)
    assemble_zeros (size);
}


/* Subroutine of output_constant, used for CONSTRUCTORs
   (aggregate constants).
   Generate at least SIZE bytes, padding if necessary.  */

static void
output_constructor (exp, size)
     tree exp;
     int size;
{
  register tree link, field = 0;
  HOST_WIDE_INT min_index = 0;
  /* Number of bytes output or skipped so far.
     In other words, current position within the constructor.  */
  int total_bytes = 0;
  /* Non-zero means BYTE contains part of a byte, to be output.  */
  int byte_buffer_in_use = 0;
  register int byte = 0;

  if (HOST_BITS_PER_WIDE_INT < BITS_PER_UNIT)
    abort ();

  if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE)
    field = TYPE_FIELDS (TREE_TYPE (exp));

  if (TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE
      && TYPE_DOMAIN (TREE_TYPE (exp)) != 0)
    min_index
      = TREE_INT_CST_LOW (TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (exp))));

  /* As LINK goes through the elements of the constant,
     FIELD goes through the structure fields, if the constant is a structure.
     if the constant is a union, then we override this,
     by getting the field from the TREE_LIST element.
     But the constant could also be an array.  Then FIELD is zero.

     There is always a maximum of one element in the chain LINK for unions
     (even if the initializer in a source program incorrectly contains
     more one). */
  for (link = CONSTRUCTOR_ELTS (exp);
       link;
       link = TREE_CHAIN (link),
       field = field ? TREE_CHAIN (field) : 0)
    {
      tree val = TREE_VALUE (link);
      tree index = 0;

      /* the element in a union constructor specifies the proper field.  */

      if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE
	  || TREE_CODE (TREE_TYPE (exp)) == UNION_TYPE)
	{
	  /* if available, use the type given by link */
	  if (TREE_PURPOSE (link) != 0)
	    field = TREE_PURPOSE (link);
	}

      if (TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE)
	index = TREE_PURPOSE (link);

      /* Eliminate the marker that makes a cast not be an lvalue.  */
      if (val != 0)
	STRIP_NOPS (val);

      if (index && TREE_CODE (index) == RANGE_EXPR)
	{
	  register int fieldsize
	    = int_size_in_bytes (TREE_TYPE (TREE_TYPE (exp)));
	  HOST_WIDE_INT lo_index = TREE_INT_CST_LOW (TREE_OPERAND (index, 0));
	  HOST_WIDE_INT hi_index = TREE_INT_CST_LOW (TREE_OPERAND (index, 1));
	  HOST_WIDE_INT index;
	  for (index = lo_index; index <= hi_index; index++)
	    {
	      /* Output the element's initial value.  */
	      if (val == 0)
		assemble_zeros (fieldsize);
	      else
		output_constant (val, fieldsize);

	      /* Count its size.  */
	      total_bytes += fieldsize;
	    }
	}
      else if (field == 0 || !DECL_BIT_FIELD (field))
	{
	  /* An element that is not a bit-field.  */

	  register int fieldsize;
	  /* Since this structure is static,
	     we know the positions are constant.  */
	  int bitpos = (field ? (TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field))
				 / BITS_PER_UNIT)
			: 0);
	  if (index != 0)
	    bitpos = (TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (val)))
		      / BITS_PER_UNIT
		      * (TREE_INT_CST_LOW (index) - min_index));

	  /* Output any buffered-up bit-fields preceding this element.  */
	  if (byte_buffer_in_use)
	    {
	      ASM_OUTPUT_BYTE (asm_out_file, byte);
	      total_bytes++;
	      byte_buffer_in_use = 0;
	    }

	  /* Advance to offset of this element.
	     Note no alignment needed in an array, since that is guaranteed
	     if each element has the proper size.  */
	  if ((field != 0 || index != 0) && bitpos != total_bytes)
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
	      int shift;
	      HOST_WIDE_INT value;
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
	      if (BYTES_BIG_ENDIAN)
		{
		  /* On big-endian machine, take the most significant bits
		     first (of the bits that are significant)
		     and put them into bytes from the most significant end.  */
		  shift = end_offset - next_offset - this_time;
		  /* Don't try to take a bunch of bits that cross
		     the word boundary in the INTEGER_CST. We can
		     only select bits from the LOW or HIGH part
		     not from both.  */
		  if (shift < HOST_BITS_PER_WIDE_INT
		      && shift + this_time > HOST_BITS_PER_WIDE_INT)
		    {
		      this_time = shift + this_time - HOST_BITS_PER_WIDE_INT;
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
		  /* Get the result. This works only when:
		     1 <= this_time <= HOST_BITS_PER_WIDE_INT.  */
		  byte |= (((value >> shift)
			    & (((HOST_WIDE_INT) 2 << (this_time - 1)) - 1))
			   << (BITS_PER_UNIT - this_time - next_bit));
		}
	      else
		{
		  /* On little-endian machines,
		     take first the least significant bits of the value
		     and pack them starting at the least significant
		     bits of the bytes.  */
		  shift = (next_offset
			   - TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field)));
		  /* Don't try to take a bunch of bits that cross
		     the word boundary in the INTEGER_CST. We can
		     only select bits from the LOW or HIGH part
		     not from both.  */
		  if (shift < HOST_BITS_PER_WIDE_INT
		      && shift + this_time > HOST_BITS_PER_WIDE_INT)
		    {
		      this_time = (HOST_BITS_PER_WIDE_INT - shift);
		    }

		  /* Now get the bits from the appropriate constant word.  */
		  if (shift < HOST_BITS_PER_WIDE_INT)
		    value = TREE_INT_CST_LOW (val);
		  else if (shift < 2 * HOST_BITS_PER_WIDE_INT)
		    {
		      value = TREE_INT_CST_HIGH (val);
		      shift -= HOST_BITS_PER_WIDE_INT;
		    }
		  else
		    abort ();
		  /* Get the result. This works only when:
		     1 <= this_time <= HOST_BITS_PER_WIDE_INT.  */
		  byte |= (((value >> shift)
			    & (((HOST_WIDE_INT) 2 << (this_time - 1)) - 1))
			   << next_bit);
		}
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

#ifdef HANDLE_PRAGMA_WEAK
/* Add function NAME to the weak symbols list.  VALUE is a weak alias
   associatd with NAME.  */
   
int
add_weak (name, value)
     char *name;
     char *value;
{
  struct weak_syms *weak;

  weak = (struct weak_syms *) permalloc (sizeof (struct weak_syms));

  if (weak == NULL)
    return 0;

  weak->next = weak_decls;
  weak->name = name;
  weak->value = value;
  weak_decls = weak;

  return 1;
}
#endif /* HANDLE_PRAGMA_WEAK */

/* Declare DECL to be a weak symbol.  */

void
declare_weak (decl)
     tree decl;
{
  if (! TREE_PUBLIC (decl))
    error_with_decl (decl, "weak declaration of `%s' must be public");
  else if (TREE_ASM_WRITTEN (decl))
    error_with_decl (decl, "weak declaration of `%s' must precede definition");
  else if (SUPPORTS_WEAK)
    DECL_WEAK (decl) = 1;
#ifdef HANDLE_PRAGMA_WEAK
   add_weak (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)), NULL);
#endif
}

/* Emit any pending weak declarations.  */

#ifdef HANDLE_PRAGMA_WEAK
struct weak_syms * weak_decls;
#endif

void
weak_finish ()
{
#ifdef HANDLE_PRAGMA_WEAK
  if (HANDLE_PRAGMA_WEAK)
    {
      struct weak_syms *t;
      for (t = weak_decls; t; t = t->next)
	{
	  if (t->name)
	    {
	      ASM_WEAKEN_LABEL (asm_out_file, t->name);
	      if (t->value)
		ASM_OUTPUT_DEF (asm_out_file, t->name, t->value);
	    }
	}
    }
#endif
}

/* Remove NAME from the pending list of weak symbols.  This prevents
   the compiler from emitting multiple .weak directives which confuses
   some assemblers.  */
#ifdef ASM_WEAKEN_LABEL
static void
remove_from_pending_weak_list (name)
     char *name ATTRIBUTE_UNUSED;
{
#ifdef HANDLE_PRAGMA_WEAK
  if (HANDLE_PRAGMA_WEAK)
    {
      struct weak_syms *t;
      for (t = weak_decls; t; t = t->next)
	{
	  if (t->name && strcmp (name, t->name) == 0)
	    t->name = NULL;
	}
    }
#endif
}
#endif

void
assemble_alias (decl, target)
     tree decl, target ATTRIBUTE_UNUSED;
{
  char *name;

  make_decl_rtl (decl, (char *) 0, 1);
  name = XSTR (XEXP (DECL_RTL (decl), 0), 0);

#ifdef ASM_OUTPUT_DEF
  /* Make name accessible from other files, if appropriate.  */

  if (TREE_PUBLIC (decl))
    {
#ifdef ASM_WEAKEN_LABEL
      if (DECL_WEAK (decl))
 	{
	  ASM_WEAKEN_LABEL (asm_out_file, name);
	  /* Remove this function from the pending weak list so that
	     we do not emit multiple .weak directives for it.  */
	  remove_from_pending_weak_list
	    (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));
	}
      else
#endif
	ASM_GLOBALIZE_LABEL (asm_out_file, name);
    }

#ifdef ASM_OUTPUT_DEF_FROM_DECLS
  ASM_OUTPUT_DEF_FROM_DECLS (asm_out_file, decl, target);
#else
  ASM_OUTPUT_DEF (asm_out_file, name, IDENTIFIER_POINTER (target));
#endif
  TREE_ASM_WRITTEN (decl) = 1;
#else
#ifdef ASM_OUTPUT_WEAK_ALIAS
  if (! DECL_WEAK (decl))
    warning ("only weak aliases are supported in this configuration");

  ASM_OUTPUT_WEAK_ALIAS (asm_out_file, name, IDENTIFIER_POINTER (target));
  TREE_ASM_WRITTEN (decl) = 1;
#else
  warning ("alias definitions not supported in this configuration; ignored");
#endif
#endif
}

/* This determines whether or not we support link-once semantics.  */
#ifndef SUPPORTS_ONE_ONLY
#ifdef MAKE_DECL_ONE_ONLY
#define SUPPORTS_ONE_ONLY 1
#else
#define SUPPORTS_ONE_ONLY 0
#endif
#endif

/* Returns 1 if the target configuration supports defining public symbols
   so that one of them will be chosen at link time instead of generating a
   multiply-defined symbol error, whether through the use of weak symbols or
   a target-specific mechanism for having duplicates discarded.  */

int
supports_one_only ()
{
  if (SUPPORTS_ONE_ONLY)
    return 1;
  return SUPPORTS_WEAK;
}

/* Set up DECL as a public symbol that can be defined in multiple
   translation units without generating a linker error.  */

void
make_decl_one_only (decl)
     tree decl;
{
  if (TREE_CODE (decl) != VAR_DECL && TREE_CODE (decl) != FUNCTION_DECL)
    abort ();

  TREE_PUBLIC (decl) = 1;

  if (TREE_CODE (decl) == VAR_DECL
      && (DECL_INITIAL (decl) == 0 || DECL_INITIAL (decl) == error_mark_node))
    DECL_COMMON (decl) = 1;
  else if (SUPPORTS_ONE_ONLY)
    {
#ifdef MAKE_DECL_ONE_ONLY
      MAKE_DECL_ONE_ONLY (decl);
#endif
      DECL_ONE_ONLY (decl) = 1;
    }
  else if (SUPPORTS_WEAK)
    DECL_WEAK (decl) = 1;
  else
    abort ();
}

void
init_varasm_once ()
{
  ggc_add_root (const_hash_table, MAX_HASH_TABLE, sizeof const_hash_table[0],
		mark_const_hash_entry);
  ggc_add_string_root (&in_named_name, 1);
}
