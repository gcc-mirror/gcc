/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997,
   1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


/* This file handles generation of all the assembler code
   *except* the instructions of a function.
   This includes declarations of variables and their initial values.

   We also output the assembler code for constants stored in memory
   and are responsible for combining constants with the same value.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "output.h"
#include "real.h"
#include "toplev.h"
#include "obstack.h"
#include "hashtab.h"
#include "c-pragma.h"
#include "ggc.h"
#include "langhooks.h"
#include "tm_p.h"
#include "debug.h"
#include "target.h"

#ifdef XCOFF_DEBUGGING_INFO
#include "xcoffout.h"		/* Needed for external data
				   declarations for e.g. AIX 4.x.  */
#endif

#ifndef TRAMPOLINE_ALIGNMENT
#define TRAMPOLINE_ALIGNMENT FUNCTION_BOUNDARY
#endif

#ifndef ASM_STABS_OP
#define ASM_STABS_OP "\t.stabs\t"
#endif

/* The (assembler) name of the first globally-visible object output.  */
const char *first_global_object_name;
const char *weak_global_object_name;

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
  struct pool_constant **x_const_rtx_sym_hash_table;

  /* Pointers to first and last constant in pool.  */
  struct pool_constant *x_first_pool, *x_last_pool;

  /* Current offset in constant pool (does not include any machine-specific
     header).  */
  HOST_WIDE_INT x_pool_offset;

  /* Chain of all CONST_DOUBLE rtx's constructed for the current function.
     They are chained through the CONST_DOUBLE_CHAIN.  */
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

/* RTX_UNCHANGING_P in a MEM can mean it is stored into, for initialization.
   So giving constant the alias set for the type will allow such
   initializations to appear to conflict with the load of the constant.  We
   avoid this by giving all constants an alias set for just constants.
   Since there will be no stores to that alias set, nothing will ever
   conflict with them.  */

static HOST_WIDE_INT const_alias_set;

static const char *strip_reg_name	PARAMS ((const char *));
static int contains_pointers_p		PARAMS ((tree));
static void decode_addr_const		PARAMS ((tree, struct addr_const *));
static int const_hash			PARAMS ((tree));
static int compare_constant		PARAMS ((tree,
					       struct constant_descriptor *));
static const unsigned char *compare_constant_1  PARAMS ((tree, const unsigned char *));
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
static int mark_constant		PARAMS ((rtx *current_rtx, void *data));
static int output_addressed_constants	PARAMS ((tree));
static void output_after_function_constants PARAMS ((void));
static unsigned HOST_WIDE_INT array_size_for_constructor PARAMS ((tree));
static unsigned min_align		PARAMS ((unsigned, unsigned));
static void output_constructor		PARAMS ((tree, HOST_WIDE_INT,
						 unsigned int));
static void globalize_decl		PARAMS ((tree));
static int in_named_entry_eq		PARAMS ((const PTR, const PTR));
static hashval_t in_named_entry_hash	PARAMS ((const PTR));
#ifdef ASM_OUTPUT_BSS
static void asm_output_bss		PARAMS ((FILE *, tree, const char *, int, int));
#endif
#ifdef BSS_SECTION_ASM_OP
#ifdef ASM_OUTPUT_ALIGNED_BSS
static void asm_output_aligned_bss	PARAMS ((FILE *, tree, const char *,
						 int, int));
#endif
#endif /* BSS_SECTION_ASM_OP */
static void mark_pool_constant          PARAMS ((struct pool_constant *));
static void mark_const_hash_entry	PARAMS ((void *));
static int mark_const_str_htab_1	PARAMS ((void **, void *));
static void mark_const_str_htab		PARAMS ((void *));
static hashval_t const_str_htab_hash	PARAMS ((const void *x));
static int const_str_htab_eq		PARAMS ((const void *x, const void *y));
static void const_str_htab_del		PARAMS ((void *));
static void asm_emit_uninitialised	PARAMS ((tree, const char*, int, int));
static void resolve_unique_section	PARAMS ((tree, int, int));
static void mark_weak                   PARAMS ((tree));

static enum in_section { no_section, in_text, in_data, in_named
#ifdef BSS_SECTION_ASM_OP
  , in_bss
#endif
#ifdef CTORS_SECTION_ASM_OP
  , in_ctors
#endif
#ifdef DTORS_SECTION_ASM_OP
  , in_dtors
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
static const char *in_named_name;

/* Hash table of flags that have been used for a particular named section.  */

struct in_named_entry
{
  const char *name;
  unsigned int flags;
  bool declared;
};

static htab_t in_named_htab;

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
#ifdef TEXT_SECTION
      TEXT_SECTION ();
#else
      fprintf (asm_out_file, "%s\n", TEXT_SECTION_ASM_OP);
#endif
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
   it's not sure where it is.  */

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

/* Helper routines for maintaining in_named_htab.  */

static int
in_named_entry_eq (p1, p2)
     const PTR p1;
     const PTR p2;
{
  const struct in_named_entry *old = p1;
  const char *new = p2;

  return strcmp (old->name, new) == 0;
}

static hashval_t
in_named_entry_hash (p)
     const PTR p;
{
  const struct in_named_entry *old = p;
  return htab_hash_string (old->name);
}

/* If SECTION has been seen before as a named section, return the flags
   that were used.  Otherwise, return 0.  Note, that 0 is a perfectly valid
   set of flags for a section to have, so 0 does not mean that the section
   has not been seen.  */

unsigned int
get_named_section_flags (section)
     const char *section;
{
  struct in_named_entry **slot;

  slot = (struct in_named_entry**)
    htab_find_slot_with_hash (in_named_htab, section,
			      htab_hash_string (section), NO_INSERT);

  return slot ? (*slot)->flags : 0;
}

/* Returns true if the section has been declared before.   Sets internal
   flag on this section in in_named_hash so subsequent calls on this 
   section will return false.  */

bool
named_section_first_declaration (name)
     const char *name;
{
  struct in_named_entry **slot;

  slot = (struct in_named_entry**)
    htab_find_slot_with_hash (in_named_htab, name, 
			      htab_hash_string (name), NO_INSERT);
  if (! (*slot)->declared)
    {
      (*slot)->declared = true;
      return true;
    }
  else 
    {
      return false;
    }
}


/* Record FLAGS for SECTION.  If SECTION was previously recorded with a
   different set of flags, return false.  */

bool
set_named_section_flags (section, flags)
     const char *section;
     unsigned int flags;
{
  struct in_named_entry **slot, *entry;

  slot = (struct in_named_entry**)
    htab_find_slot_with_hash (in_named_htab, section,
			      htab_hash_string (section), INSERT);
  entry = *slot;

  if (!entry)
    {
      entry = (struct in_named_entry *) xmalloc (sizeof (*entry));
      *slot = entry;
      entry->name = ggc_strdup (section);
      entry->flags = flags;
      entry->declared = false;
    }
  else if (entry->flags != flags)
    return false;

  return true;
}

/* Tell assembler to change to section NAME with attributes FLAGS.  */

void
named_section_flags (name, flags)
     const char *name;
     unsigned int flags;
{
  if (in_section != in_named || strcmp (name, in_named_name) != 0)
    {
      if (! set_named_section_flags (name, flags))
	abort ();

      (* targetm.asm_out.named_section) (name, flags);

      if (flags & SECTION_FORGET)
	in_section = no_section;
      else
	{
	  in_named_name = ggc_strdup (name);
	  in_section = in_named;
	}
    }
}

/* Tell assembler to change to section NAME for DECL.
   If DECL is NULL, just switch to section NAME.
   If NAME is NULL, get the name from DECL.
   If RELOC is 1, the initializer for DECL contains relocs.  */

void
named_section (decl, name, reloc)
     tree decl;
     const char *name;
     int reloc;
{
  unsigned int flags;

  if (decl != NULL_TREE && !DECL_P (decl))
    abort ();
  if (name == NULL)
    name = TREE_STRING_POINTER (DECL_SECTION_NAME (decl));

  flags = (* targetm.section_type_flags) (decl, name, reloc);

  /* Sanity check user variables for flag changes.  Non-user
     section flag changes will abort in named_section_flags.
     However, don't complain if SECTION_OVERRIDE is set.
     We trust that the setter knows that it is safe to ignore
     the default flags for this decl.  */
  if (decl && ! set_named_section_flags (name, flags))
    {
      flags = get_named_section_flags (name);
      if ((flags & SECTION_OVERRIDE) == 0)
	error_with_decl (decl, "%s causes a section type conflict");
    }

  named_section_flags (name, flags);
}

/* If required, set DECL_SECTION_NAME to a unique name.  */

static void
resolve_unique_section (decl, reloc, flag_function_or_data_sections)
     tree decl;
     int reloc ATTRIBUTE_UNUSED;
     int flag_function_or_data_sections;
{
  if (DECL_SECTION_NAME (decl) == NULL_TREE
      && (flag_function_or_data_sections
	  || (targetm.have_named_sections
	      && DECL_ONE_ONLY (decl))))
    UNIQUE_SECTION (decl, reloc);
}

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
     const char *name;
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
  ASM_OUTPUT_SKIP (file, rounded ? rounded : 1);
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
     tree decl ATTRIBUTE_UNUSED;
     const char *name;
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
      SELECT_SECTION (decl, reloc, DECL_ALIGN (decl));
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
default_exception_section ()
{
  if (targetm.have_named_sections)
    named_section (NULL_TREE, ".gcc_except_table", 0);
  else if (flag_pic)
    data_section ();
  else
    readonly_data_section ();
}

/* Tell assembler to switch to the section for string merging.  */

void
mergeable_string_section (decl, align, flags)
  tree decl ATTRIBUTE_UNUSED;
  unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED;
  unsigned int flags ATTRIBUTE_UNUSED;
{
#ifdef HAVE_GAS_SHF_MERGE
  if (flag_merge_constants
      && TREE_CODE (decl) == STRING_CST
      && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
      && align <= 256
      && TREE_STRING_LENGTH (decl) >= int_size_in_bytes (TREE_TYPE (decl)))
    {
      enum machine_mode mode;
      unsigned int modesize;
      const char *str;
      int i, j, len, unit;
      char name[30];

      mode = TYPE_MODE (TREE_TYPE (TREE_TYPE (decl)));
      modesize = GET_MODE_BITSIZE (mode);
      if (modesize >= 8 && modesize <= 256
	  && (modesize & (modesize - 1)) == 0)
	{
	  if (align < modesize)
	    align = modesize;

	  str = TREE_STRING_POINTER (decl);
	  len = TREE_STRING_LENGTH (decl);
	  unit = GET_MODE_SIZE (mode);

	  /* Check for embedded NUL characters.  */
	  for (i = 0; i < len; i += unit)
	    {
	      for (j = 0; j < unit; j++)
		if (str [i + j] != '\0')
		  break;
	      if (j == unit)
		break;
	    }
	  if (i == len - unit)
	    {
	      sprintf (name, ".rodata.str%d.%d", modesize / 8,
		       (int) (align / 8));
	      flags |= (modesize / 8) | SECTION_MERGE | SECTION_STRINGS;
	      if (!i && modesize < align)
		{
		  /* A "" string with requested alignment greater than
		     character size might cause a problem:
		     if some other string required even bigger
		     alignment than "", then linker might think the
		     "" is just part of padding after some other string
		     and not put it into the hash table initially.
		     But this means "" could have smaller alignment
		     than requested.  */
#ifdef ASM_OUTPUT_SECTION_START
		  named_section_flags (name, flags);
		  ASM_OUTPUT_SECTION_START (asm_out_file);
#else
		  readonly_data_section ();
#endif
		  return;
		}

	      named_section_flags (name, flags);
	      return;
	    }
	}
    }
#endif
  readonly_data_section ();
}  

/* Tell assembler to switch to the section for constant merging.  */

void
mergeable_constant_section (mode, align, flags)
  enum machine_mode mode ATTRIBUTE_UNUSED;
  unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED;
  unsigned int flags ATTRIBUTE_UNUSED;
{
#ifdef HAVE_GAS_SHF_MERGE
  unsigned int modesize = GET_MODE_BITSIZE (mode);

  if (flag_merge_constants
      && mode != VOIDmode
      && mode != BLKmode
      && modesize <= align
      && align >= 8
      && align <= 256
      && (align & (align - 1)) == 0)
    {
      char name[24];

      sprintf (name, ".rodata.cst%d", (int) (align / 8));
      flags |= (align / 8) | SECTION_MERGE;
      named_section_flags (name, flags);
      return;
    }            
#endif
  readonly_data_section ();
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
	if (! ISDIGIT (asmspec[i]))
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
	static const struct { const char *const name; const int number; } table[]
	  = ADDITIONAL_REGISTER_NAMES;

	for (i = 0; i < (int) ARRAY_SIZE (table); i++)
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

/* Create the DECL_RTL for a VAR_DECL or FUNCTION_DECL.  DECL should
   have static storage duration.  In other words, it should not be an
   automatic variable, including PARM_DECLs.

   There is, however, one exception: this function handles variables
   explicitly placed in a particular register by the user.

   ASMSPEC, if not 0, is the string which the user specified as the
   assembler symbol name.

   This is never called for PARM_DECL nodes.  */

void
make_decl_rtl (decl, asmspec)
     tree decl;
     const char *asmspec;
{
  int top_level = (DECL_CONTEXT (decl) == NULL_TREE);
  const char *name = 0;
  const char *new_name = 0;
  int reg_number;
  rtx x;

  /* Check that we are not being given an automatic variable.  */
  /* A weak alias has TREE_PUBLIC set but not the other bits.  */
  if (TREE_CODE (decl) == PARM_DECL
      || TREE_CODE (decl) == RESULT_DECL
      || (TREE_CODE (decl) == VAR_DECL
	  && !TREE_STATIC (decl)
	  && !TREE_PUBLIC (decl)
	  && !DECL_EXTERNAL (decl)
	  && !DECL_REGISTER (decl)))
    abort ();
  /* And that we were not given a type or a label.  */
  else if (TREE_CODE (decl) == TYPE_DECL
	   || TREE_CODE (decl) == LABEL_DECL)
    abort ();

  /* For a duplicate declaration, we can be called twice on the
     same DECL node.  Don't discard the RTL already made.  */
  if (DECL_RTL_SET_P (decl))
    {
      /* If the old RTL had the wrong mode, fix the mode.  */
      if (GET_MODE (DECL_RTL (decl)) != DECL_MODE (decl))
	SET_DECL_RTL (decl, adjust_address_nv (DECL_RTL (decl),
					       DECL_MODE (decl), 0));

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
      return;
    }

  new_name = name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  reg_number = decode_reg_name (asmspec);
  if (reg_number == -2)
    {
      /* ASMSPEC is given, and not the name of a register.  Mark the
	 name with a star so assemble_name won't munge it.  */
      char *starred = alloca (strlen (asmspec) + 2);
      starred[0] = '*';
      strcpy (starred + 1, asmspec);
      new_name = starred;
    }

  if (TREE_CODE (decl) != FUNCTION_DECL && DECL_REGISTER (decl))
    {
      /* First detect errors in declaring global registers.  */
      if (reg_number == -1)
	error_with_decl (decl, "register name not specified for `%s'");
      else if (reg_number < 0)
	error_with_decl (decl, "invalid register name for `%s'");
      else if (TYPE_MODE (TREE_TYPE (decl)) == BLKmode)
	error_with_decl (decl,
			 "data type of `%s' isn't suitable for a register");
      else if (! HARD_REGNO_MODE_OK (reg_number, TYPE_MODE (TREE_TYPE (decl))))
	error_with_decl (decl,
			 "register specified for `%s' isn't suitable for data type");
      /* Now handle properly declared static register variables.  */
      else
	{
	  int nregs;

	  if (DECL_INITIAL (decl) != 0 && TREE_STATIC (decl))
	    {
	      DECL_INITIAL (decl) = 0;
	      error ("global register variable has initial value");
	    }
	  if (TREE_THIS_VOLATILE (decl))
	    warning ("volatile register variables don't work as you might wish");

	  /* If the user specified one of the eliminables registers here,
	     e.g., FRAME_POINTER_REGNUM, we don't want to get this variable
	     confused with that register and be eliminated.  This usage is
	     somewhat suspect...  */

	  SET_DECL_RTL (decl, gen_rtx_raw_REG (DECL_MODE (decl), reg_number));
	  ORIGINAL_REGNO (DECL_RTL (decl)) = reg_number;
	  REG_USERVAR_P (DECL_RTL (decl)) = 1;

	  if (TREE_STATIC (decl))
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

	  /* As a register variable, it has no section.  */
	  return;
	}
    }

  /* Now handle ordinary static variables and functions (in memory).
     Also handle vars declared register invalidly.  */

  if (reg_number >= 0 || reg_number == -3)
    error_with_decl (decl,
		     "register name given for non-register variable `%s'");

  /* Specifying a section attribute on a variable forces it into a
     non-.bss section, and thus it cannot be common.  */
  if (TREE_CODE (decl) == VAR_DECL
      && DECL_SECTION_NAME (decl) != NULL_TREE
      && DECL_INITIAL (decl) == NULL_TREE
      && DECL_COMMON (decl))
    DECL_COMMON (decl) = 0;

  /* Can't use just the variable's own name for a variable
     whose scope is less than the whole file, unless it's a member
     of a local class (which will already be unambiguous).
     Concatenate a distinguishing number.  */
  if (!top_level && !TREE_PUBLIC (decl)
      && ! (DECL_CONTEXT (decl) && TYPE_P (DECL_CONTEXT (decl)))
      && asmspec == 0
      && name == IDENTIFIER_POINTER (DECL_NAME (decl)))
    {
      char *label;

      ASM_FORMAT_PRIVATE_NAME (label, name, var_labelno);
      var_labelno++;
      new_name = label;
    }

  if (name != new_name)
    {
      SET_DECL_ASSEMBLER_NAME (decl, get_identifier (new_name));
      name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
    }

  /* If this variable is to be treated as volatile, show its
     tree node has side effects.  */
  if ((flag_volatile_global && TREE_CODE (decl) == VAR_DECL
       && TREE_PUBLIC (decl))
      || ((flag_volatile_static && TREE_CODE (decl) == VAR_DECL
	   && (TREE_PUBLIC (decl) || TREE_STATIC (decl)))))
    TREE_SIDE_EFFECTS (decl) = 1;

  x = gen_rtx_MEM (DECL_MODE (decl), gen_rtx_SYMBOL_REF (Pmode, name));
  SYMBOL_REF_WEAK (XEXP (x, 0)) = DECL_WEAK (decl);
  if (TREE_CODE (decl) != FUNCTION_DECL)
    set_mem_attributes (x, decl, 1);
  SET_DECL_RTL (decl, x);

  /* Optionally set flags or add text to the name to record information
     such as that it is a function name.
     If the name is changed, the macro ASM_OUTPUT_LABELREF
     will have to know how to strip this information.  */
#ifdef ENCODE_SECTION_INFO
  ENCODE_SECTION_INFO (decl);
#endif
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
    {
      ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (align / BITS_PER_UNIT));
    }
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

/* Record an element in the table of global destructors.  SYMBOL is
   a SYMBOL_REF of the function to be called; PRIORITY is a number
   between 0 and MAX_INIT_PRIORITY.  */

void
default_stabs_asm_out_destructor (symbol, priority)
     rtx symbol;
     int priority ATTRIBUTE_UNUSED;
{
  /* Tell GNU LD that this is part of the static destructor set.
     This will work for any system that uses stabs, most usefully
     aout systems.  */
  fprintf (asm_out_file, "%s\"___DTOR_LIST__\",22,0,0,", ASM_STABS_OP);
  assemble_name (asm_out_file, XSTR (symbol, 0));
  fputc ('\n', asm_out_file);
}

void
default_named_section_asm_out_destructor (symbol, priority)
     rtx symbol;
     int priority;
{
  const char *section = ".dtors";
  char buf[16];

  /* ??? This only works reliably with the GNU linker.  */
  if (priority != DEFAULT_INIT_PRIORITY)
    {
      sprintf (buf, ".dtors.%.5u",
	       /* Invert the numbering so the linker puts us in the proper
		  order; constructors are run from right to left, and the
		  linker sorts in increasing order.  */
	       MAX_INIT_PRIORITY - priority);
      section = buf;
    }

  named_section_flags (section, SECTION_WRITE);
  assemble_align (POINTER_SIZE);
  assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
}

#ifdef DTORS_SECTION_ASM_OP
void
dtors_section ()
{
  if (in_section != in_dtors)
    {
      in_section = in_dtors;
      fputs (DTORS_SECTION_ASM_OP, asm_out_file);
      fputc ('\n', asm_out_file);
    }
}

void
default_dtor_section_asm_out_destructor (symbol, priority)
     rtx symbol;
     int priority ATTRIBUTE_UNUSED;
{
  dtors_section ();
  assemble_align (POINTER_SIZE);
  assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
}
#endif

/* Likewise for global constructors.  */

void
default_stabs_asm_out_constructor (symbol, priority)
     rtx symbol;
     int priority ATTRIBUTE_UNUSED;
{
  /* Tell GNU LD that this is part of the static destructor set.
     This will work for any system that uses stabs, most usefully
     aout systems.  */
  fprintf (asm_out_file, "%s\"___CTOR_LIST__\",22,0,0,", ASM_STABS_OP);
  assemble_name (asm_out_file, XSTR (symbol, 0));
  fputc ('\n', asm_out_file);
}

void
default_named_section_asm_out_constructor (symbol, priority)
     rtx symbol;
     int priority;
{
  const char *section = ".ctors";
  char buf[16];

  /* ??? This only works reliably with the GNU linker.  */
  if (priority != DEFAULT_INIT_PRIORITY)
    {
      sprintf (buf, ".ctors.%.5u",
	       /* Invert the numbering so the linker puts us in the proper
		  order; constructors are run from right to left, and the
		  linker sorts in increasing order.  */
	       MAX_INIT_PRIORITY - priority);
      section = buf;
    }

  named_section_flags (section, SECTION_WRITE);
  assemble_align (POINTER_SIZE);
  assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
}

#ifdef CTORS_SECTION_ASM_OP
void
ctors_section ()
{
  if (in_section != in_ctors)
    {
      in_section = in_ctors;
      fputs (CTORS_SECTION_ASM_OP, asm_out_file);
      fputc ('\n', asm_out_file);
    }
}

void
default_ctor_section_asm_out_constructor (symbol, priority)
     rtx symbol;
     int priority ATTRIBUTE_UNUSED;
{
  ctors_section ();
  assemble_align (POINTER_SIZE);
  assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
}
#endif

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
     const char *fnname;
{
  int align;

  /* The following code does not need preprocessing in the assembler.  */

  app_disable ();

  if (CONSTANT_POOL_BEFORE_FUNCTION)
    output_constant_pool (fnname, decl);

  resolve_unique_section (decl, 0, flag_function_sections);
  function_section (decl);

  /* Tell assembler to move to target machine's alignment for functions.  */
  align = floor_log2 (FUNCTION_BOUNDARY / BITS_PER_UNIT);
  if (align > 0)
    {
      ASM_OUTPUT_ALIGN (asm_out_file, align);
    }

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

  (*debug_hooks->begin_function) (decl);

  /* Make function name accessible from other files, if appropriate.  */

  if (TREE_PUBLIC (decl))
    {
      if (! first_global_object_name)
	{
	  const char *p;
	  char *name;

	  STRIP_NAME_ENCODING (p, fnname);
	  name = permalloc (strlen (p) + 1);
	  strcpy (name, p);

	  if (! DECL_WEAK (decl) && ! DECL_ONE_ONLY (decl))
	    first_global_object_name = name;
	  else
	    weak_global_object_name = name;
	}

      globalize_decl (decl);
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
      for (i = 0; i < size; i++)
	assemble_integer (const0_rtx, 1, BITS_PER_UNIT, 1);
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
    {
      ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (align / BITS_PER_UNIT));
    }
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
     const char * name;
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

  if (destination == asm_dest_bss)
    globalize_decl (decl);
  resolve_unique_section (decl, 0, flag_data_sections);

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
  const char *name;
  unsigned int align;
  int reloc = 0;
  rtx decl_rtl;

  last_assemble_variable_decl = 0;

  /* Normally no need to say anything here for external references,
     since assemble_external is called by the language-specific code
     when a declaration is first seen.  */

  if (DECL_EXTERNAL (decl))
    return;

  /* Output no assembler code for a function declaration.
     Only definitions of functions output anything.  */

  if (TREE_CODE (decl) == FUNCTION_DECL)
    return;

  /* Do nothing for global register variables.  */
  if (DECL_RTL_SET_P (decl) && GET_CODE (DECL_RTL (decl)) == REG)
    {
      TREE_ASM_WRITTEN (decl) = 1;
      return;
    }

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

  /* Make sure ENCODE_SECTION_INFO is invoked before we set ASM_WRITTEN.  */
  decl_rtl = DECL_RTL (decl);

  TREE_ASM_WRITTEN (decl) = 1;

  /* Do no output if -fsyntax-only.  */
  if (flag_syntax_only)
    return;

  app_disable ();

  if (! dont_output_data
      && ! host_integerp (DECL_SIZE_UNIT (decl), 1))
    {
      error_with_decl (decl, "size of variable `%s' is too large");
      return;
    }

  name = XSTR (XEXP (decl_rtl, 0), 0);
  if (TREE_PUBLIC (decl) && DECL_NAME (decl)
      && ! first_global_object_name
      && ! (DECL_COMMON (decl) && (DECL_INITIAL (decl) == 0
				   || DECL_INITIAL (decl) == error_mark_node))
      && ! DECL_WEAK (decl)
      && ! DECL_ONE_ONLY (decl))
    {
      const char *p;
      char *xname;

      STRIP_NAME_ENCODING (p, name);
      xname = permalloc (strlen (p) + 1);
      strcpy (xname, p);
      first_global_object_name = xname;
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
	"alignment of `%s' is greater than maximum object file alignment. Using %d",
                    MAX_OFILE_ALIGNMENT/BITS_PER_UNIT);
      align = MAX_OFILE_ALIGNMENT;
    }

  /* On some machines, it is good to increase alignment sometimes.  */
  if (! DECL_USER_ALIGN (decl))
    {
#ifdef DATA_ALIGNMENT
      align = DATA_ALIGNMENT (TREE_TYPE (decl), align);
#endif
#ifdef CONSTANT_ALIGNMENT
      if (DECL_INITIAL (decl) != 0 && DECL_INITIAL (decl) != error_mark_node)
        align = CONSTANT_ALIGNMENT (DECL_INITIAL (decl), align);
#endif
    }

  /* Reset the alignment in case we have made it tighter, so we can benefit
     from it in get_pointer_alignment.  */
  DECL_ALIGN (decl) = align;
  set_mem_align (decl_rtl, align);

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
      unsigned HOST_WIDE_INT size = tree_low_cst (DECL_SIZE_UNIT (decl), 1);
      unsigned HOST_WIDE_INT rounded = size;

      /* Don't allocate zero bytes of common,
	 since that means "undefined external" in the linker.  */
      if (size == 0)
	rounded = 1;

      /* Round size up to multiple of BIGGEST_ALIGNMENT bits
	 so that each uninitialized object starts on such a boundary.  */
      rounded += (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1;
      rounded = (rounded / (BIGGEST_ALIGNMENT / BITS_PER_UNIT)
		 * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));

/* Don't continue this line--convex cc version 4.1 would lose.  */
#if !defined(ASM_OUTPUT_ALIGNED_COMMON) && !defined(ASM_OUTPUT_ALIGNED_DECL_COMMON) && !defined(ASM_OUTPUT_ALIGNED_BSS)
      if ((unsigned HOST_WIDE_INT) DECL_ALIGN (decl) / BITS_PER_UNIT > rounded)
         warning_with_decl
           (decl, "requested alignment for %s is greater than implemented alignment of %d",rounded);
#endif

      asm_emit_uninitialised (decl, name, size, rounded);

      return;
    }

  /* Handle initialized definitions.
     Also handle uninitialized global definitions if -fno-common and the
     target doesn't support ASM_OUTPUT_BSS.  */

  /* First make the assembler name(s) global if appropriate.  */
  if (TREE_PUBLIC (decl) && DECL_NAME (decl))
    globalize_decl (decl);

  /* Output any data that we will need to use the address of.  */
  if (DECL_INITIAL (decl) == error_mark_node)
    reloc = contains_pointers_p (TREE_TYPE (decl)) ? 3 : 0;
  else if (DECL_INITIAL (decl))
    reloc = output_addressed_constants (DECL_INITIAL (decl));

  /* Switch to the appropriate section.  */
  resolve_unique_section (decl, reloc, flag_data_sections);
  variable_section (decl, reloc);

  /* dbxout.c needs to know this.  */
  if (in_text_section ())
    DECL_IN_TEXT_SECTION (decl) = 1;

  /* Output the alignment of this data.  */
  if (align > BITS_PER_UNIT)
    {
      ASM_OUTPUT_ALIGN (asm_out_file,
			floor_log2 (DECL_ALIGN (decl) / BITS_PER_UNIT));
    }

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
      if (DECL_INITIAL (decl) && DECL_INITIAL (decl) != error_mark_node)
	/* Output the actual data.  */
	output_constant (DECL_INITIAL (decl),
			 tree_low_cst (DECL_SIZE_UNIT (decl), 1),
			 align);
      else
	/* Leave space for it.  */
	assemble_zeros (tree_low_cst (DECL_SIZE_UNIT (decl), 1));
    }
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
  /* Because most platforms do not define ASM_OUTPUT_EXTERNAL, the
     main body of this code is only rarely exercised.  To provide some
     testing, on all platforms, we make sure that the ASM_OUT_FILE is
     open.  If it's not, we should not be calling this function.  */
  if (!asm_out_file)
    abort ();

#ifdef ASM_OUTPUT_EXTERNAL
  if (DECL_P (decl) && DECL_EXTERNAL (decl) && TREE_PUBLIC (decl))
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
     const char *name ATTRIBUTE_UNUSED;
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
  const char *namestring;
  rtx x;

#if 0
  if (flag_shared_data)
    data_section ();
#endif

  ASM_GENERATE_INTERNAL_LABEL (name, "LF", const_labelno);
  ++const_labelno;
  namestring = ggc_strdup (name);

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
    /* Variable `rounded' might or might not be used in ASM_OUTPUT_LOCAL.  */
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
  const char *name;
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
    {
      ASM_OUTPUT_ALIGN (asm_out_file, align);
    }

  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LTRAMP", 0);
  TRAMPOLINE_TEMPLATE (asm_out_file);

  /* Record the rtl to refer to it.  */
  ASM_GENERATE_INTERNAL_LABEL (label, "LTRAMP", 0);
  name = ggc_strdup (label);
  return gen_rtx_SYMBOL_REF (Pmode, name);
}
#endif

/* A and B are either alignments or offsets.  Return the minimum alignment
   that may be assumed after adding the two together.  */

static inline unsigned
min_align (a, b)
     unsigned int a, b;
{
  return (a | b) & -(a | b);
}

/* Return the assembler directive for creating a given kind of integer
   object.  SIZE is the number of bytes in the object and ALIGNED_P
   indicates whether it is known to be aligned.  Return NULL if the
   assembly dialect has no such directive.

   The returned string should be printed at the start of a new line and
   be followed immediately by the object's initial value.  */

const char *
integer_asm_op (size, aligned_p)
     int size;
     int aligned_p;
{
  struct asm_int_op *ops;

  if (aligned_p)
    ops = &targetm.asm_out.aligned_op;
  else
    ops = &targetm.asm_out.unaligned_op;

  switch (size)
    {
    case 1:
      return targetm.asm_out.byte_op;
    case 2:
      return ops->hi;
    case 4:
      return ops->si;
    case 8:
      return ops->di;
    case 16:
      return ops->ti;
    default:
      return NULL;
    }
}

/* Use directive OP to assemble an integer object X.  Print OP at the
   start of the line, followed immediately by the value of X.  */

void
assemble_integer_with_op (op, x)
     const char *op;
     rtx x;
{
  fputs (op, asm_out_file);
  output_addr_const (asm_out_file, x);
  fputc ('\n', asm_out_file);
}

/* The default implementation of the asm_out.integer target hook.  */

bool
default_assemble_integer (x, size, aligned_p)
     rtx x ATTRIBUTE_UNUSED;
     unsigned int size ATTRIBUTE_UNUSED;
     int aligned_p ATTRIBUTE_UNUSED;
{
  const char *op = integer_asm_op (size, aligned_p);
  return op && (assemble_integer_with_op (op, x), true);
}

/* Assemble the integer constant X into an object of SIZE bytes.  ALIGN is
   the alignment of the integer in bits.  Return 1 if we were able to output
   the constant, otherwise 0.  If FORCE is non-zero, abort if we can't output
   the constant.  */

bool
assemble_integer (x, size, align, force)
     rtx x;
     unsigned int size;
     unsigned int align;
     int force;
{
  int aligned_p;

  aligned_p = (align >= MIN (size * BITS_PER_UNIT, BIGGEST_ALIGNMENT));

  /* See if the target hook can handle this kind of object.  */
  if ((*targetm.asm_out.integer) (x, size, aligned_p))
    return true;

  /* If the object is a multi-byte one, try splitting it up.  Split
     it into words it if is multi-word, otherwise split it into bytes.  */
  if (size > 1)
    {
      enum machine_mode omode, imode;
      unsigned int subalign;
      unsigned int subsize, i;

      subsize = size > UNITS_PER_WORD? UNITS_PER_WORD : 1;
      subalign = MIN (align, subsize * BITS_PER_UNIT);
      omode = mode_for_size (subsize * BITS_PER_UNIT, MODE_INT, 0);
      imode = mode_for_size (size * BITS_PER_UNIT, MODE_INT, 0);

      for (i = 0; i < size; i += subsize)
	{
	  rtx partial = simplify_subreg (omode, x, imode, i);
	  if (!partial || !assemble_integer (partial, subsize, subalign, 0))
	    break;
	}
      if (i == size)
	return true;

      /* If we've printed some of it, but not all of it, there's no going
	 back now.  */
      if (i > 0)
	abort ();
    }

  if (force)
    abort ();

  return false;
}

void
assemble_real (d, mode, align)
     REAL_VALUE_TYPE d;
     enum machine_mode mode;
     unsigned int align;
{
  long data[4];
  long l;
  unsigned int nalign = min_align (align, 32);

  switch (BITS_PER_UNIT)
    {
    case 8:
      switch (mode)
	{
	case SFmode:
	  REAL_VALUE_TO_TARGET_SINGLE (d, l);
	  assemble_integer (GEN_INT (l), 4, align, 1);
	  break;
	case DFmode:
	  REAL_VALUE_TO_TARGET_DOUBLE (d, data);
	  assemble_integer (GEN_INT (data[0]), 4, align, 1);
	  assemble_integer (GEN_INT (data[1]), 4, nalign, 1);
	  break;
	case XFmode:
	  REAL_VALUE_TO_TARGET_LONG_DOUBLE (d, data);
	  assemble_integer (GEN_INT (data[0]), 4, align, 1);
	  assemble_integer (GEN_INT (data[1]), 4, nalign, 1);
	  assemble_integer (GEN_INT (data[2]), 4, nalign, 1);
	  break;
	case TFmode:
	  REAL_VALUE_TO_TARGET_LONG_DOUBLE (d, data);
	  assemble_integer (GEN_INT (data[0]), 4, align, 1);
	  assemble_integer (GEN_INT (data[1]), 4, nalign, 1);
	  assemble_integer (GEN_INT (data[2]), 4, nalign, 1);
	  assemble_integer (GEN_INT (data[3]), 4, nalign, 1);
	  break;
	default:
	  abort ();
	}
      break;

    case 16:
      switch (mode)
	{
	case HFmode:
	  REAL_VALUE_TO_TARGET_SINGLE (d, l);
	  assemble_integer (GEN_INT (l), 2, align, 1);
	  break;
	case TQFmode:
	  REAL_VALUE_TO_TARGET_DOUBLE (d, data);
	  assemble_integer (GEN_INT (data[0]), 2, align, 1);
	  assemble_integer (GEN_INT (data[1]), 1, nalign, 1);
	  break;
	default:
	  abort ();
	}
      break;

    case 32:
      switch (mode)
	{
	case QFmode:
	  REAL_VALUE_TO_TARGET_SINGLE (d, l);
	  assemble_integer (GEN_INT (l), 1, align, 1);
	  break;
	case HFmode:
	  REAL_VALUE_TO_TARGET_DOUBLE (d, data);
	  assemble_integer (GEN_INT (data[0]), 1, align, 1);
	  assemble_integer (GEN_INT (data[1]), 1, nalign, 1);
	  break;
	default:
	  abort ();
	}
      break;

    default:
      abort ();
    }
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
  rtx r;

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

      if (width < HOST_BITS_PER_WIDE_INT
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

  /* No; make a new one and add it to the chain.  */
  r = gen_rtx_CONST_DOUBLE (mode, i0, i1);

  /* Don't touch const_double_chain if not inside any function.  */
  if (current_function_decl != 0)
    {
      CONST_DOUBLE_CHAIN (r) = const_double_chain;
      const_double_chain = r;
    }

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
  rtx r;

  /* Get the desired `double' value as a sequence of ints
     since that is how they are stored in a CONST_DOUBLE.  */

  u.d = d;

  /* Detect special cases.  Check for NaN first, because some ports
     (specifically the i386) do not emit correct ieee-fp code by default, and
     thus will generate a core dump here if we pass a NaN to REAL_VALUES_EQUAL
     and if REAL_VALUES_EQUAL does a floating point comparison.  */
  if (! REAL_VALUE_ISNAN (d) && REAL_VALUES_IDENTICAL (dconst0, d))
    return CONST0_RTX (mode);
  else if (! REAL_VALUE_ISNAN (d) && REAL_VALUES_EQUAL (dconst1, d))
    return CONST1_RTX (mode);
  else if (! REAL_VALUE_ISNAN (d) && REAL_VALUES_EQUAL (dconst2, d))
    return CONST2_RTX (mode);

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
      if (! memcmp ((char *) &CONST_DOUBLE_LOW (r), (char *) &u, sizeof u)
	  && GET_MODE (r) == mode)
	return r;

  /* No; make a new one and add it to the chain.

     We may be called by an optimizer which may be discarding any memory
     allocated during its processing (such as combine and loop).  However,
     we will be leaving this constant on the chain, so we cannot tolerate
     freed memory.  */
  r = rtx_alloc (CONST_DOUBLE);
  PUT_MODE (r, mode);
  memcpy ((char *) &CONST_DOUBLE_LOW (r), (char *) &u, sizeof u);

  /* If we aren't inside a function, don't put r on the
     const_double_chain.  */
  if (current_function_decl != 0)
    {
      CONST_DOUBLE_CHAIN (r) = const_double_chain;
      const_double_chain = r;
    }
  else
    CONST_DOUBLE_CHAIN (r) = NULL_RTX;

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
  rtx r, next;

  for (r = const_double_chain; r; r = next)
    {
      next = CONST_DOUBLE_CHAIN (r);
      CONST_DOUBLE_CHAIN (r) = 0;
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
  tree target = TREE_OPERAND (exp, 0);
  int offset = 0;
  rtx x;

  while (1)
    {
      if (TREE_CODE (target) == COMPONENT_REF
	  && host_integerp (byte_position (TREE_OPERAND (target, 1)), 0))

	{
	  offset += int_byte_position (TREE_OPERAND (target, 1));
	  target = TREE_OPERAND (target, 0);
	}
      else if (TREE_CODE (target) == ARRAY_REF
	       || TREE_CODE (target) == ARRAY_RANGE_REF)
	{
	  offset += (tree_low_cst (TYPE_SIZE_UNIT (TREE_TYPE (target)), 1)
		     * tree_low_cst (TREE_OPERAND (target, 1), 0));
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
      /* This constant should have been output already, but we can't simply
	 use TREE_CST_RTL since INTEGER_CST doesn't have one.  */
      x = output_constant_def (target, 1);
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

/* We do RTX_UNSPEC + XINT (blah), so nothing can go after RTX_UNSPEC.  */
enum kind { RTX_UNKNOWN, RTX_DOUBLE, RTX_INT, RTX_VECTOR, RTX_UNSPEC };
struct rtx_const
{
  ENUM_BITFIELD(kind) kind : 16;
  ENUM_BITFIELD(machine_mode) mode : 16;
  union {
    union real_extract du;
    struct addr_const addr;
    struct {HOST_WIDE_INT high, low;} di;

    /* The max vector size we have is 8 wide.  This should be enough.  */
    HOST_WIDE_INT veclo[16];
    HOST_WIDE_INT vechi[16];
  } un;
};

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
  const char *label;
  rtx rtl;
  /* Make sure the data is reasonably aligned.  */
  union
  {
    unsigned char contents[1];
#ifdef HAVE_LONG_DOUBLE
    long double d;
#else
    double d;
#endif
  } u;
};

#define HASHBITS 30
#define MAX_HASH_TABLE 1009
static struct constant_descriptor *const_hash_table[MAX_HASH_TABLE];

/* We maintain a hash table of STRING_CST values.  Unless we are asked to force
   out a string constant, we defer output of the constants until we know
   they are actually used.  This will be if something takes its address or if
   there is a usage of the string in the RTL of a function.  */

#define STRHASH(x) ((hashval_t) ((long) (x) >> 3))

struct deferred_string
{
  const char *label;
  tree exp;
  int labelno;
};

static htab_t const_str_htab;

/* Mark a const_hash_table descriptor for GC.  */

static void
mark_const_hash_entry (ptr)
     void *ptr;
{
  struct constant_descriptor *desc = * (struct constant_descriptor **) ptr;

  while (desc)
    {
      ggc_mark_rtx (desc->rtl);
      desc = desc->next;
    }
}

/* Mark the hash-table element X (which is really a pointer to an
   struct deferred_string *).  */

static int
mark_const_str_htab_1 (x, data)
     void **x;
     void *data ATTRIBUTE_UNUSED;
{
  ggc_mark_tree (((struct deferred_string *) *x)->exp);
  return 1;
}

/* Mark a const_str_htab for GC.  */

static void
mark_const_str_htab (htab)
     void *htab;
{
  htab_traverse (*((htab_t *) htab), mark_const_str_htab_1, NULL);
}

/* Returns a hash code for X (which is a really a
   struct deferred_string *).  */

static hashval_t
const_str_htab_hash (x)
     const void *x;
{
  return STRHASH (((const struct deferred_string *) x)->label);
}

/* Returns non-zero if the value represented by X (which is really a
   struct deferred_string *) is the same as that given by Y
   (which is really a char *).  */

static int
const_str_htab_eq (x, y)
     const void *x;
     const void *y;
{
  return (((const struct deferred_string *) x)->label == (const char *) y);
}

/* Delete the hash table entry dfsp.  */

static void
const_str_htab_del (dfsp)
    void *dfsp;
{
  free (dfsp);
}

/* Compute a hash code for a constant expression.  */

static int
const_hash (exp)
     tree exp;
{
  const char *p;
  int len, hi, i;
  enum tree_code code = TREE_CODE (exp);

  /* Either set P and LEN to the address and len of something to hash and
     exit the switch or return a value.  */

  switch (code)
    {
    case INTEGER_CST:
      p = (char *) &TREE_INT_CST (exp);
      len = sizeof TREE_INT_CST (exp);
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
	  char *tmp;

	  len = int_size_in_bytes (TREE_TYPE (exp));
	  tmp = (char *) alloca (len);
	  get_set_constructor_bytes (exp, (unsigned char *) tmp, len);
	  p = tmp;
	  break;
	}
      else
	{
	  tree link;

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
    case FDESC_EXPR:
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
	  abort ();

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
      /* A language specific constant. Just hash the code.  */
      return (int) code % MAX_HASH_TABLE;
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
  return 0 != compare_constant_1 (exp, desc->u.contents);
}

/* Compare constant expression EXP with a substring P of a constant descriptor.
   If they match, return a pointer to the end of the substring matched.
   If they do not match, return 0.

   Since descriptors are written in polish prefix notation,
   this function can be used recursively to test one operand of EXP
   against a subdescriptor, and if it succeeds it returns the
   address of the subdescriptor for the next operand.  */

static const unsigned char *
compare_constant_1 (exp, p)
     tree exp;
     const unsigned char *p;
{
  const unsigned char *strp;
  int len;
  enum tree_code code = TREE_CODE (exp);

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

      strp = (unsigned char *) &TREE_INT_CST (exp);
      len = sizeof TREE_INT_CST (exp);
      break;

    case REAL_CST:
      /* Real constants are the same only if the same width of type.  */
      if (*p++ != TYPE_PRECISION (TREE_TYPE (exp)))
	return 0;

      strp = (unsigned char *) &TREE_REAL_CST (exp);
      len = sizeof TREE_REAL_CST (exp);
      break;

    case STRING_CST:
      if (flag_writable_strings)
	return 0;

      if ((enum machine_mode) *p++ != TYPE_MODE (TREE_TYPE (exp)))
	return 0;

      strp = (const unsigned char *) TREE_STRING_POINTER (exp);
      len = TREE_STRING_LENGTH (exp);
      if (memcmp ((char *) &TREE_STRING_LENGTH (exp), p,
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
	  unsigned char *tmp = (unsigned char *) alloca (len);

	  get_set_constructor_bytes (exp, tmp, len);
	  strp = (unsigned char *) tmp;
	  if (memcmp ((char *) &xlen, p, sizeof xlen))
	    return 0;

	  p += sizeof xlen;
	  break;
	}
      else
	{
	  tree link;
	  int length = list_length (CONSTRUCTOR_ELTS (exp));
	  tree type;
	  enum machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
	  int have_purpose = 0;

	  for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	    if (TREE_PURPOSE (link))
	      have_purpose = 1;

	  if (memcmp ((char *) &length, p, sizeof length))
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

	  if (memcmp ((char *) &type, p, sizeof type))
	    return 0;

	  if (TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE)
	    {
	      if (memcmp ((char *) &mode, p, sizeof mode))
		return 0;

	      p += sizeof mode;
	    }

	  p += sizeof type;

	  if (memcmp ((char *) &have_purpose, p, sizeof have_purpose))
	    return 0;

	  p += sizeof have_purpose;

	  /* For arrays, insist that the size in bytes match.  */
	  if (TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE)
	    {
	      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (exp));

	      if (memcmp ((char *) &size, p, sizeof size))
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

		  if (memcmp ((char *) &zero, p, sizeof zero))
		    return 0;

		  p += sizeof zero;
		}

	      if (TREE_PURPOSE (link)
		  && TREE_CODE (TREE_PURPOSE (link)) == FIELD_DECL)
		{
		  if (memcmp ((char *) &TREE_PURPOSE (link), p,
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

		  if (memcmp ((char *) &zero, p, sizeof zero))
		    return 0;

		  p += sizeof zero;
		}
	    }

	  return p;
	}

    case ADDR_EXPR:
    case FDESC_EXPR:
      {
	struct addr_const value;

	decode_addr_const (exp, &value);
	strp = (unsigned char *) &value.offset;
	len = sizeof value.offset;
	/* Compare the offset.  */
	while (--len >= 0)
	  if (*p++ != *strp++)
	    return 0;

	/* Compare symbol name.  */
	strp = (const unsigned char *) XSTR (value.base, 0);
	len = strlen ((const char *) strp) + 1;
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
      {
	tree new = (*lang_hooks.expand_constant) (exp);

	if (new != exp)
          return compare_constant_1 (new, p);
	else
	  return 0;
      }
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
  int pad;

  /* Make a struct constant_descriptor.  The first three pointers will
     be filled in later.  Here we just leave space for them.  */

  obstack_grow (&permanent_obstack, (char *) &next, sizeof next);
  obstack_grow (&permanent_obstack, (char *) &label, sizeof label);
  obstack_grow (&permanent_obstack, (char *) &rtl, sizeof rtl);

  /* Align the descriptor for the data payload.  */
  pad = (offsetof (struct constant_descriptor, u)
	 - offsetof(struct constant_descriptor, rtl)
	 - sizeof(next->rtl));
  if (pad > 0)
    obstack_blank (&permanent_obstack, pad);

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
  const unsigned char *strp;
  int len;
  enum tree_code code = TREE_CODE (exp);

  obstack_1grow (&permanent_obstack, (unsigned int) code);

  switch (code)
    {
    case INTEGER_CST:
      obstack_1grow (&permanent_obstack, TYPE_PRECISION (TREE_TYPE (exp)));
      strp = (unsigned char *) &TREE_INT_CST (exp);
      len = sizeof TREE_INT_CST (exp);
      break;

    case REAL_CST:
      obstack_1grow (&permanent_obstack, TYPE_PRECISION (TREE_TYPE (exp)));
      strp = (unsigned char *) &TREE_REAL_CST (exp);
      len = sizeof TREE_REAL_CST (exp);
      break;

    case STRING_CST:
      if (flag_writable_strings)
	return;

      obstack_1grow (&permanent_obstack, TYPE_MODE (TREE_TYPE (exp)));
      strp = (const unsigned char *) TREE_STRING_POINTER (exp);
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
	  tree link;
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

	switch (GET_CODE (value.base))
	  {
	  case SYMBOL_REF:
	    /* Record the symbol name.  */
	    obstack_grow (&permanent_obstack, XSTR (value.base, 0),
			  strlen (XSTR (value.base, 0)) + 1);
	    break;
	  case LABEL_REF:
	    /* Record the address of the CODE_LABEL.  It may not have
	       been emitted yet, so it's UID may be zero.  But pointer
	       identity is good enough.  */
	    obstack_grow (&permanent_obstack, &XEXP (value.base, 0),
			  sizeof (rtx));
	    break;
	  default:
	    abort ();
	  }
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
      {
	tree new = (*lang_hooks.expand_constant) (exp);

	if (new != exp)
          record_constant_1 (new);
	return;
      }
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

   If DEFER is non-zero, the output of string constants can be deferred
   and output only if referenced in the function after all optimizations.

   The TREE_CST_RTL of EXP is set up to point to that rtx.
   The const_hash_table records which constants already have label strings.  */

rtx
output_constant_def (exp, defer)
     tree exp;
     int defer;
{
  int hash;
  struct constant_descriptor *desc;
  struct deferred_string **defstr;
  char label[256];
  int reloc;
  int found = 1;
  int after_function = 0;
  int labelno = -1;
  rtx rtl;

  /* We can't just use the saved RTL if this is a defererred string constant
     and we are not to defer anymode.  */
  if (TREE_CODE (exp) != INTEGER_CST && TREE_CST_RTL (exp)
      && (defer || !STRING_POOL_ADDRESS_P (XEXP (TREE_CST_RTL (exp), 0))))
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
      labelno = const_labelno++;
      ASM_GENERATE_INTERNAL_LABEL (label, "LC", labelno);

      desc = record_constant (exp);
      desc->next = const_hash_table[hash];
      desc->label = ggc_strdup (label);
      const_hash_table[hash] = desc;

      /* We have a symbol name; construct the SYMBOL_REF and the MEM.  */
      rtl = desc->rtl
	= gen_rtx_MEM (TYPE_MODE (TREE_TYPE (exp)),
		       gen_rtx_SYMBOL_REF (Pmode, desc->label));

      set_mem_attributes (rtl, exp, 1);
      set_mem_alias_set (rtl, 0);
      set_mem_alias_set (rtl, const_alias_set);

      found = 0;
    }
  else
    rtl = desc->rtl;

  if (TREE_CODE (exp) != INTEGER_CST)
    TREE_CST_RTL (exp) = rtl;

  /* Optionally set flags or add text to the name to record information
     such as that it is a function name.  If the name is changed, the macro
     ASM_OUTPUT_LABELREF will have to know how to strip this information.  */
#ifdef ENCODE_SECTION_INFO
  /* A previously-processed constant would already have section info
     encoded in it.  */
  if (! found)
    {
      /* Take care not to invoque ENCODE_SECTION_INFO for constants
	 which don't have a TREE_CST_RTL.  */
      if (TREE_CODE (exp) != INTEGER_CST)
	ENCODE_SECTION_INFO (exp);
      
      desc->rtl = rtl;
      desc->label = XSTR (XEXP (desc->rtl, 0), 0);
    }
#endif

#ifdef CONSTANT_AFTER_FUNCTION_P
  if (current_function_decl != 0
      && CONSTANT_AFTER_FUNCTION_P (exp))
    after_function = 1;
#endif

  if (found
      && STRING_POOL_ADDRESS_P (XEXP (rtl, 0))
      && (!defer || defer_addressed_constants_flag || after_function))
    {
      defstr = (struct deferred_string **)
	htab_find_slot_with_hash (const_str_htab, desc->label,
				  STRHASH (desc->label), NO_INSERT);
      if (defstr)
	{
	  /* If the string is currently deferred but we need to output it now,
	     remove it from deferred string hash table.  */
	  found = 0;
	  labelno = (*defstr)->labelno;
	  STRING_POOL_ADDRESS_P (XEXP (rtl, 0)) = 0;
	  htab_clear_slot (const_str_htab, (void **) defstr);
	}
    }

  /* If this is the first time we've seen this particular constant,
     output it (or defer its output for later).  */
  if (! found)
    {
      if (defer_addressed_constants_flag || after_function)
	{
	  struct deferred_constant *p
	    = (struct deferred_constant *)
	      xmalloc (sizeof (struct deferred_constant));

	  p->exp = copy_constant (exp);
	  p->reloc = reloc;
	  p->labelno = labelno;
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
	    {
	      if (TREE_CODE (exp) != STRING_CST
		  || !defer
		  || flag_writable_strings
		  || (defstr = (struct deferred_string **)
			       htab_find_slot_with_hash (const_str_htab,
							 desc->label,
							 STRHASH (desc->label),
							 INSERT)) == NULL)
		output_constant_def_contents (exp, reloc, labelno);
	      else
		{
		  struct deferred_string *p;

		  p = (struct deferred_string *)
		      xmalloc (sizeof (struct deferred_string));

		  p->exp = copy_constant (exp);
		  p->label = desc->label;
		  p->labelno = labelno;
		  *defstr = p;
		  STRING_POOL_ADDRESS_P (XEXP (rtl, 0)) = 1;
		}
	    }
	}
    }

  return rtl;
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

  /* Align the location counter as required by EXP's data type.  */
  align = TYPE_ALIGN (TREE_TYPE (exp));
#ifdef CONSTANT_ALIGNMENT
  align = CONSTANT_ALIGNMENT (exp, align);
#endif

  if (IN_NAMED_SECTION (exp))
    named_section (exp, NULL, reloc);
  else
    {
      /* First switch to text section, except for writable strings.  */
#ifdef SELECT_SECTION
      SELECT_SECTION (exp, reloc, align);
#else
      if (((TREE_CODE (exp) == STRING_CST) && flag_writable_strings)
	  || (flag_pic && reloc))
	data_section ();
      else
	readonly_data_section ();
#endif
    }

  if (align > BITS_PER_UNIT)
    {
      ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (align / BITS_PER_UNIT));
    }

  /* Output the label itself.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LC", labelno);

  /* Output the value of EXP.  */
  output_constant (exp,
		   (TREE_CODE (exp) == STRING_CST
		    ? MAX (TREE_STRING_LENGTH (exp),
			   int_size_in_bytes (TREE_TYPE (exp)))
		    : int_size_in_bytes (TREE_TYPE (exp))),
		   align);

}

/* Structure to represent sufficient information about a constant so that
   it can be output when the constant pool is output, so that function
   integration can be done, and to simplify handling on machines that reference
   constant pool as base+displacement.  */

struct pool_constant
{
  struct constant_descriptor *desc;
  struct pool_constant *next, *next_sym;
  rtx constant;
  enum machine_mode mode;
  int labelno;
  unsigned int align;
  HOST_WIDE_INT offset;
  int mark;
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
       xcalloc (MAX_RTX_HASH_TABLE, sizeof (struct constant_descriptor *)));
  p->x_const_rtx_sym_hash_table
    = ((struct pool_constant **)
       xcalloc (MAX_RTX_HASH_TABLE, sizeof (struct pool_constant *)));

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
      ggc_mark (pc);
      ggc_mark_rtx (pc->constant);
      ggc_mark_rtx (pc->desc->rtl);
      pc = pc->next;
    }
}

/* Mark P for GC.  */

void
mark_varasm_status (p)
     struct varasm_status *p;
{
  if (p == NULL)
    return;

  mark_pool_constant (p->x_first_pool);
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
  int i;

  p = f->varasm;

  /* Clear out the hash tables.  */
  for (i = 0; i < MAX_RTX_HASH_TABLE; ++i)
    {
      struct constant_descriptor *cd;

      cd = p->x_const_rtx_hash_table[i];
      while (cd)
	{
	  struct constant_descriptor *next = cd->next;

	  free (cd);
	  cd = next;
	}
    }

  free (p->x_const_rtx_hash_table);
  free (p->x_const_rtx_sym_hash_table);
  free (p);

  f->varasm = NULL;
}


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
  memset (value, 0, sizeof (struct rtx_const));

  value->kind = RTX_INT;	/* Most usual kind.  */
  value->mode = mode;

  switch (GET_CODE (x))
    {
    case CONST_DOUBLE:
      value->kind = RTX_DOUBLE;
      if (GET_MODE (x) != VOIDmode)
	{
	  value->mode = GET_MODE (x);
	  memcpy ((char *) &value->un.du,
		  (char *) &CONST_DOUBLE_LOW (x), sizeof value->un.du);
	}
      else
	{
	  value->un.di.low = CONST_DOUBLE_LOW (x);
	  value->un.di.high = CONST_DOUBLE_HIGH (x);
	}
      break;

    case CONST_VECTOR:
      {
	int units, i;
	rtx elt;

	units = CONST_VECTOR_NUNITS (x);
	value->kind = RTX_VECTOR;
	value->mode = mode;

	for (i = 0; i < units; ++i)
	  {
	    elt = CONST_VECTOR_ELT (x, i);
	    if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
	      {
		value->un.veclo[i] = (HOST_WIDE_INT) INTVAL (elt);
		value->un.vechi[i] = 0;
	      }
	    else if (GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT)
	      {
		value->un.veclo[i] = (HOST_WIDE_INT) CONST_DOUBLE_LOW (elt);
		value->un.vechi[i] = (HOST_WIDE_INT) CONST_DOUBLE_HIGH (elt);
	      }
	    else
	      abort ();
	  }
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
      if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  value->un.addr.base = XEXP (x, 0);
	  value->un.addr.offset = INTVAL (XEXP (x, 1));
	}
      else if (GET_CODE (x) == MINUS && GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  value->un.addr.base = XEXP (x, 0);
	  value->un.addr.offset = - INTVAL (XEXP (x, 1));
	}
      else
	{
	  value->un.addr.base = x;
	  value->un.addr.offset = 0;
	}
      break;

    default:
      value->kind = RTX_UNKNOWN;
      break;
    }

  if (value->kind == RTX_INT && value->un.addr.base != 0
      && GET_CODE (value->un.addr.base) == UNSPEC)
    {      
      /* For a simple UNSPEC, the base is set to the
	 operand, the kind field is set to the index of
	 the unspec expression. 
	 Together with the code below, in case that
	 the operand is a SYMBOL_REF or LABEL_REF, 
	 the address of the string or the code_label 
	 is taken as base.  */
      if (XVECLEN (value->un.addr.base, 0) == 1)
        {
	  value->kind = RTX_UNSPEC + XINT (value->un.addr.base, 1);
	  value->un.addr.base = XVECEXP (value->un.addr.base, 0, 0);
	}
    }

  if (value->kind > RTX_DOUBLE && value->un.addr.base != 0)
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

  if (val0.kind > RTX_DOUBLE
      && val0.kind == val1.kind
      && val0.un.addr.base == val1.un.addr.base)
    return GEN_INT (val0.un.addr.offset - val1.un.addr.offset);

  return x;
}

/* Compute a hash code for a constant RTL expression.  */

static int
const_hash_rtx (mode, x)
     enum machine_mode mode;
     rtx x;
{
  int hi;
  size_t i;

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
  int *p = (int *) desc->u.contents;
  int *strp;
  int len;
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

  ptr = ((struct constant_descriptor *)
	 xcalloc (1, (offsetof (struct constant_descriptor, u)
		      + sizeof (struct rtx_const))));
  decode_rtx_const (mode, x, (struct rtx_const *) ptr->u.contents);

  return ptr;
}

/* Given a constant rtx X, return a MEM for the location in memory at which
   this constant has been placed.  Return 0 if it not has been placed yet.  */

rtx
mem_for_const_double (x)
     rtx x;
{
  enum machine_mode mode = GET_MODE (x);
  struct constant_descriptor *desc;

  for (desc = const_rtx_hash_table[const_hash_rtx (mode, x)]; desc;
       desc = desc->next)
    if (compare_constant_rtx (mode, x, desc))
      return desc->rtl;

  return 0;
}
  
/* Given a constant rtx X, make (or find) a memory constant for its value
   and return a MEM rtx to refer to it in memory.  */

rtx
force_const_mem (mode, x)
     enum machine_mode mode;
     rtx x;
{
  int hash;
  struct constant_descriptor *desc;
  char label[256];
  rtx def;
  struct pool_constant *pool;
  unsigned int align;

  /* Compute hash code of X.  Search the descriptors for that hash code
     to see if any of them describes X.  If yes, we have an rtx to use.  */
  hash = const_hash_rtx (mode, x);
  for (desc = const_rtx_hash_table[hash]; desc; desc = desc->next)
    if (compare_constant_rtx (mode, x, desc))
      return desc->rtl;

  /* No constant equal to X is known to have been output.
     Make a constant descriptor to enter X in the hash table
     and make a MEM for it.  */
  desc = record_constant_rtx (mode, x);
  desc->next = const_rtx_hash_table[hash];
  const_rtx_hash_table[hash] = desc;
  
  /* Align the location counter as required by EXP's data type.  */
  align = GET_MODE_ALIGNMENT (mode == VOIDmode ? word_mode : mode);
#ifdef CONSTANT_ALIGNMENT
  align = CONSTANT_ALIGNMENT (make_tree (type_for_mode (mode, 0), x), align);
#endif

  pool_offset += (align / BITS_PER_UNIT) - 1;
  pool_offset &= ~ ((align / BITS_PER_UNIT) - 1);

  if (GET_CODE (x) == LABEL_REF)
    LABEL_PRESERVE_P (XEXP (x, 0)) = 1;

  /* Allocate a pool constant descriptor, fill it in, and chain it in.  */
  pool = (struct pool_constant *) ggc_alloc (sizeof (struct pool_constant));
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

  /* Construct the SYMBOL_REF and the MEM.  */

  pool->desc->rtl = def
    = gen_rtx_MEM (mode, gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (label)));
  set_mem_alias_set (def, const_alias_set);
  set_mem_attributes (def, type_for_mode (mode, 0), 1);
  RTX_UNCHANGING_P (def) = 1;

  /* Add label to symbol hash table.  */
  hash = SYMHASH (XSTR (XEXP (def, 0), 0));
  pool->next_sym = const_rtx_sym_hash_table[hash];
  const_rtx_sym_hash_table[hash] = pool;

  /* Mark the symbol_ref as belonging to this constants pool.  */
  CONSTANT_POOL_ADDRESS_P (XEXP (def, 0)) = 1;
  current_function_uses_const_pool = 1;

  return def;
}

/* Given a SYMBOL_REF with CONSTANT_POOL_ADDRESS_P true, return a pointer to
   the corresponding pool_constant structure.  */

static struct pool_constant *
find_pool_constant (f, addr)
     struct function *f;
     rtx addr;
{
  struct pool_constant *pool;
  const char *label = XSTR (addr, 0);

  for (pool = f->varasm->x_const_rtx_sym_hash_table[SYMHASH (label)]; pool;
       pool = pool->next_sym)
    if (XSTR (XEXP (pool->desc->rtl, 0), 0) == label)
      return pool;

  abort ();
}

/* Given a constant pool SYMBOL_REF, return the corresponding constant.  */

rtx
get_pool_constant (addr)
     rtx addr;
{
  return (find_pool_constant (cfun, addr))->constant;
}

/* Given a constant pool SYMBOL_REF, return the corresponding constant
   and whether it has been output or not.  */

rtx
get_pool_constant_mark (addr, pmarked)
     rtx addr;
     bool *pmarked;
{
  struct pool_constant *pool = find_pool_constant (cfun, addr);
  *pmarked = (pool->mark != 0);
  return pool->constant;
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
      rtx tmp;

      x = pool->constant;

      if (! pool->mark)
	continue;

      /* See if X is a LABEL_REF (or a CONST referring to a LABEL_REF)
	 whose CODE_LABEL has been deleted.  This can occur if a jump table
	 is eliminated by optimization.  If so, write a constant of zero
	 instead.  Note that this can also happen by turning the
	 CODE_LABEL into a NOTE.  */
      /* ??? This seems completely and utterly wrong.  Certainly it's
	 not true for NOTE_INSN_DELETED_LABEL, but I disbelieve proper
	 functioning even with INSN_DELETED_P and friends.  */

      tmp = x;
      switch (GET_CODE (x))
	{
	case CONST:
	  if (GET_CODE (XEXP (x, 0)) != PLUS
	      || GET_CODE (XEXP (XEXP (x, 0), 0)) != LABEL_REF)
	    break;
	  tmp = XEXP (XEXP (x, 0), 0);
	  /* FALLTHRU */

	case LABEL_REF:
	  tmp = XEXP (x, 0);
	  if (INSN_DELETED_P (tmp)
	      || (GET_CODE (tmp) == NOTE
		  && NOTE_LINE_NUMBER (tmp) == NOTE_INSN_DELETED))
	    {
	      abort ();
	      x = const0_rtx;
	    }
	  break;

	default:
	  break;
	}

      /* First switch to correct section.  */
#ifdef SELECT_RTX_SECTION
      SELECT_RTX_SECTION (pool->mode, x, pool->align);
#else
      readonly_data_section ();
#endif

#ifdef ASM_OUTPUT_SPECIAL_POOL_ENTRY
      ASM_OUTPUT_SPECIAL_POOL_ENTRY (asm_out_file, x, pool->mode,
				     pool->align, pool->labelno, done);
#endif

      assemble_align (pool->align);

      /* Output the label.  */
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LC", pool->labelno);

      /* Output the value of the constant itself.  */
      switch (GET_MODE_CLASS (pool->mode))
	{
	case MODE_FLOAT:
	  if (GET_CODE (x) != CONST_DOUBLE)
	    abort ();

	  memcpy ((char *) &u, (char *) &CONST_DOUBLE_LOW (x), sizeof u);
	  assemble_real (u.d, pool->mode, pool->align);
	  break;

	case MODE_INT:
	case MODE_PARTIAL_INT:
	  assemble_integer (x, GET_MODE_SIZE (pool->mode), pool->align, 1);
	  break;

	case MODE_VECTOR_FLOAT:
	  {
	    int i, units;
	    rtx elt;

	    if (GET_CODE (x) != CONST_VECTOR)
	      abort ();

	    units = CONST_VECTOR_NUNITS (x);

	    for (i = 0; i < units; i++)
	      {
		elt = CONST_VECTOR_ELT (x, i);
		memcpy ((char *) &u,
			(char *) &CONST_DOUBLE_LOW (elt),
			sizeof u);
		assemble_real (u.d, GET_MODE_INNER (pool->mode), pool->align);
	      }
	  }
	  break;

        case MODE_VECTOR_INT:
	  {
	    int i, units;
	    rtx elt;

	    if (GET_CODE (x) != CONST_VECTOR)
	      abort ();

	    units = CONST_VECTOR_NUNITS (x);

	    for (i = 0; i < units; i++)
	      {
		elt = CONST_VECTOR_ELT (x, i);
		assemble_integer (elt, GET_MODE_UNIT_SIZE (pool->mode),
				  pool->align, 1);
	      }
	  }
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
   entries in the constant pool which are actually being used.
   Emit used deferred strings.  */

static void
mark_constant_pool ()
{
  rtx insn;
  struct pool_constant *pool;

  if (first_pool == 0 && htab_elements (const_str_htab) == 0)
    return;

  for (pool = first_pool; pool; pool = pool->next)
    pool->mark = 0;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      mark_constants (PATTERN (insn));

  for (insn = current_function_epilogue_delay_list;
       insn;
       insn = XEXP (insn, 1))
    if (INSN_P (insn))
      mark_constants (PATTERN (insn));
}

/* Look through appropriate parts of X, marking all entries in the
   constant pool which are actually being used.  Entries that are only
   referenced by other constants are also marked as used.  Emit
   deferred strings that are used.  */

static void
mark_constants (x)
     rtx x;
{
  int i;
  const char *format_ptr;

  if (x == 0)
    return;

  if (GET_CODE (x) == SYMBOL_REF)
    {
      mark_constant (&x, NULL);
      return;
    }

  /* Insns may appear inside a SEQUENCE.  Only check the patterns of
     insns, not any notes that may be attached.  We don't want to mark
     a constant just because it happens to appear in a REG_EQUIV note.  */
  if (INSN_P (x))
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
	      int j;

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

/* Given a SYMBOL_REF CURRENT_RTX, mark it and all constants it refers
   to as used.  Emit referenced deferred strings.  This function can
   be used with for_each_rtx to mark all SYMBOL_REFs in an rtx.  */

static int
mark_constant (current_rtx, data)
     rtx *current_rtx;
     void *data ATTRIBUTE_UNUSED;
{
  rtx x = *current_rtx;

  if (x == NULL_RTX)
    return 0;

  else if (GET_CODE (x) == SYMBOL_REF)
    {
      if (CONSTANT_POOL_ADDRESS_P (x))
	{
	  struct pool_constant *pool = find_pool_constant (cfun, x);
	  if (pool->mark == 0) {
	    pool->mark = 1;
	    for_each_rtx (&(pool->constant), &mark_constant, NULL);
	  }
	  else
	    return -1;
	}
      else if (STRING_POOL_ADDRESS_P (x))
	{
	  struct deferred_string **defstr;

	  defstr = (struct deferred_string **)
	    htab_find_slot_with_hash (const_str_htab, XSTR (x, 0),
				      STRHASH (XSTR (x, 0)), NO_INSERT);
	  if (defstr)
	    {
	      struct deferred_string *p = *defstr;

	      STRING_POOL_ADDRESS_P (x) = 0;
	      output_constant_def_contents (p->exp, 0, p->labelno);
	      htab_clear_slot (const_str_htab, (void **) defstr);
	    }
	}
    }
  return 0;
}

/* Find all the constants whose addresses are referenced inside of EXP,
   and make sure assembler code with a label has been output for each one.
   Indicate whether an ADDR_EXPR has been encountered.  */

static int
output_addressed_constants (exp)
     tree exp;
{
  int reloc = 0;
  tree tem;

  /* Give the front-end a chance to convert VALUE to something that
     looks more like a constant to the back-end.  */
  exp = (*lang_hooks.expand_constant) (exp);

  switch (TREE_CODE (exp))
    {
    case ADDR_EXPR:
    case FDESC_EXPR:
      /* Go inside any operations that get_inner_reference can handle and see
	 if what's inside is a constant: no need to do anything here for
	 addresses of variables or functions.  */
      for (tem = TREE_OPERAND (exp, 0); handled_component_p (tem);
	   tem = TREE_OPERAND (tem, 0))
	;

      if (TREE_CODE_CLASS (TREE_CODE (tem)) == 'c'
	    || TREE_CODE (tem) == CONSTRUCTOR)
	  output_constant_def (tem, 0);

      if (TREE_PUBLIC (tem))
	reloc |= 2;
      else
	reloc |= 1;
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
      for (tem = CONSTRUCTOR_ELTS (exp); tem; tem = TREE_CHAIN (tem))
	if (TREE_VALUE (tem) != 0)
	    reloc |= output_addressed_constants (TREE_VALUE (tem));

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
  /* Give the front-end a chance to convert VALUE to something that
     looks more like a constant to the back-end.  */
  value = (*lang_hooks.expand_constant) (value);

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
    case VECTOR_CST:
    case REAL_CST:
    case STRING_CST:
    case COMPLEX_CST:
      return null_pointer_node;

    case ADDR_EXPR:
    case FDESC_EXPR:
      return staticp (TREE_OPERAND (value, 0)) ? TREE_OPERAND (value, 0) : 0;

    case VIEW_CONVERT_EXPR:
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

	  /* Since GCC guarantees that string constants are unique in the
	     generated code, a subtraction between two copies of the same
	     constant string is absolute.  */
	  if (valid0 && TREE_CODE (valid0) == STRING_CST &&
	      valid1 && TREE_CODE (valid1) == STRING_CST &&
	      TREE_STRING_POINTER (valid0) == TREE_STRING_POINTER (valid1))
	    return null_pointer_node;
	}

      /* Support differences between labels.  */
      if (INTEGRAL_TYPE_P (endtype))
	{
	  tree op0, op1;
	  op0 = TREE_OPERAND (value, 0);
	  op1 = TREE_OPERAND (value, 1);

	  /* Like STRIP_NOPS except allow the operand mode to widen.
	     This works around a feature of fold that simplfies
	     (int)(p1 - p2) to ((int)p1 - (int)p2) under the theory
	     that the narrower operation is cheaper.  */

	  while (TREE_CODE (op0) == NOP_EXPR
		 || TREE_CODE (op0) == CONVERT_EXPR
		 || TREE_CODE (op0) == NON_LVALUE_EXPR)
	    {
	      tree inner = TREE_OPERAND (op0, 0);
	      if (inner == error_mark_node
	          || ! INTEGRAL_MODE_P (TYPE_MODE (TREE_TYPE (inner)))
		  || (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (op0)))
		      > GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (inner)))))
		break;
	      op0 = inner;
	    }

	  while (TREE_CODE (op1) == NOP_EXPR
		 || TREE_CODE (op1) == CONVERT_EXPR
		 || TREE_CODE (op1) == NON_LVALUE_EXPR)
	    {
	      tree inner = TREE_OPERAND (op1, 0);
	      if (inner == error_mark_node
	          || ! INTEGRAL_MODE_P (TYPE_MODE (TREE_TYPE (inner)))
		  || (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (op1)))
		      > GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (inner)))))
		break;
	      op1 = inner;
	    }

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
   But such constructors will never be generated for any possible input.

   ALIGN is the alignment of the data in bits.  */

void
output_constant (exp, size, align)
     tree exp;
     HOST_WIDE_INT size;
     unsigned int align;
{
  enum tree_code code;
  HOST_WIDE_INT thissize;

  /* Some front-ends use constants other than the standard language-indepdent
     varieties, but which may still be output directly.  Give the front-end a
     chance to convert EXP to a language-independent representation.  */
  exp = (*lang_hooks.expand_constant) (exp);

  if (size == 0 || flag_syntax_only)
    return;

  /* Eliminate any conversions since we'll be outputting the underlying
     constant.  */
  while (TREE_CODE (exp) == NOP_EXPR || TREE_CODE (exp) == CONVERT_EXPR
	 || TREE_CODE (exp) == NON_LVALUE_EXPR
	 || TREE_CODE (exp) == VIEW_CONVERT_EXPR)
    exp = TREE_OPERAND (exp, 0);

  code = TREE_CODE (TREE_TYPE (exp));
  thissize = int_size_in_bytes (TREE_TYPE (exp));

  /* Allow a constructor with no elements for any data type.
     This means to fill the space with zeros.  */
  if (TREE_CODE (exp) == CONSTRUCTOR && CONSTRUCTOR_ELTS (exp) == 0)
    {
      assemble_zeros (size);
      return;
    }

  if (TREE_CODE (exp) == FDESC_EXPR)
    {
#ifdef ASM_OUTPUT_FDESC
      HOST_WIDE_INT part = tree_low_cst (TREE_OPERAND (exp, 1), 0);
      tree decl = TREE_OPERAND (exp, 0);
      ASM_OUTPUT_FDESC (asm_out_file, decl, part);
#else
      abort ();
#endif
      return;
    }

  /* Now output the underlying data.  If we've handling the padding, return.
     Otherwise, break and ensure THISSIZE is the size written.  */
  switch (code)
    {
    case CHAR_TYPE:
    case BOOLEAN_TYPE:
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      if (! assemble_integer (expand_expr (exp, NULL_RTX, VOIDmode,
					   EXPAND_INITIALIZER),
			      size, align, 0))
	error ("initializer for integer value is too complicated");
      break;

    case REAL_TYPE:
      if (TREE_CODE (exp) != REAL_CST)
	error ("initializer for floating value is not a floating constant");

      assemble_real (TREE_REAL_CST (exp),
		     mode_for_size (size * BITS_PER_UNIT, MODE_FLOAT, 0),
		     align);
      break;

    case COMPLEX_TYPE:
      output_constant (TREE_REALPART (exp), thissize / 2, align);
      output_constant (TREE_IMAGPART (exp), thissize / 2,
		       min_align (align, BITS_PER_UNIT * (thissize / 2)));
      break;

    case ARRAY_TYPE:
    case VECTOR_TYPE:
      if (TREE_CODE (exp) == CONSTRUCTOR)
	{
	  output_constructor (exp, size, align);
	  return;
	}
      else if (TREE_CODE (exp) == STRING_CST)
	{
	  thissize = MIN (TREE_STRING_LENGTH (exp), size);
	  assemble_string (TREE_STRING_POINTER (exp), thissize);
	}
      else
	abort ();
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      if (TREE_CODE (exp) == CONSTRUCTOR)
	output_constructor (exp, size, align);
      else
	abort ();
      return;

    case SET_TYPE:
      if (TREE_CODE (exp) == INTEGER_CST)
	assemble_integer (expand_expr (exp, NULL_RTX,
				       VOIDmode, EXPAND_INITIALIZER),
			 thissize, align, 1);
      else if (TREE_CODE (exp) == CONSTRUCTOR)
	{
	  unsigned char *buffer = (unsigned char *) alloca (thissize);
	  if (get_set_constructor_bytes (exp, buffer, thissize))
	    abort ();
	  assemble_string ((char *) buffer, thissize);
	}
      else
	error ("unknown set constructor type");
      return;

    case ERROR_MARK:
      return;

    default:
      abort ();
    }

  size -= thissize;
  if (size > 0)
    assemble_zeros (size);
}


/* Subroutine of output_constructor, used for computing the size of
   arrays of unspecified length.  VAL must be a CONSTRUCTOR of an array
   type with an unspecified upper bound.  */

static unsigned HOST_WIDE_INT
array_size_for_constructor (val)
     tree val;
{
  tree max_index, i;

  /* This code used to attempt to handle string constants that are not
     arrays of single-bytes, but nothing else does, so there's no point in
     doing it here.  */
  if (TREE_CODE (val) == STRING_CST)
    return TREE_STRING_LENGTH (val);

  max_index = NULL_TREE;
  for (i = CONSTRUCTOR_ELTS (val); i ; i = TREE_CHAIN (i))
    {
      tree index = TREE_PURPOSE (i);

      if (TREE_CODE (index) == RANGE_EXPR)
	index = TREE_OPERAND (index, 1);
      if (max_index == NULL_TREE || tree_int_cst_lt (max_index, index))
	max_index = index;
    }

  if (max_index == NULL_TREE)
    return 0;

  /* Compute the total number of array elements.  */
  i = size_binop (MINUS_EXPR, convert (sizetype, max_index),
		  convert (sizetype,
			   TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (val)))));
  i = size_binop (PLUS_EXPR, i, convert (sizetype, integer_one_node));

  /* Multiply by the array element unit size to find number of bytes.  */
  i = size_binop (MULT_EXPR, i, TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (val))));

  return tree_low_cst (i, 1);
}

/* Subroutine of output_constant, used for CONSTRUCTORs (aggregate constants).
   Generate at least SIZE bytes, padding if necessary.  */

static void
output_constructor (exp, size, align)
     tree exp;
     HOST_WIDE_INT size;
     unsigned int align;
{
  tree type = TREE_TYPE (exp);
  tree link, field = 0;
  tree min_index = 0;
  /* Number of bytes output or skipped so far.
     In other words, current position within the constructor.  */
  HOST_WIDE_INT total_bytes = 0;
  /* Non-zero means BYTE contains part of a byte, to be output.  */
  int byte_buffer_in_use = 0;
  int byte = 0;

  if (HOST_BITS_PER_WIDE_INT < BITS_PER_UNIT)
    abort ();

  if (TREE_CODE (type) == RECORD_TYPE)
    field = TYPE_FIELDS (type);

  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type) != 0)
    min_index = TYPE_MIN_VALUE (TYPE_DOMAIN (type));

  /* As LINK goes through the elements of the constant,
     FIELD goes through the structure fields, if the constant is a structure.
     if the constant is a union, then we override this,
     by getting the field from the TREE_LIST element.
     But the constant could also be an array.  Then FIELD is zero.

     There is always a maximum of one element in the chain LINK for unions
     (even if the initializer in a source program incorrectly contains
     more one).  */
  for (link = CONSTRUCTOR_ELTS (exp);
       link;
       link = TREE_CHAIN (link),
       field = field ? TREE_CHAIN (field) : 0)
    {
      tree val = TREE_VALUE (link);
      tree index = 0;

      /* The element in a union constructor specifies the proper field
	 or index.  */
      if ((TREE_CODE (type) == RECORD_TYPE || TREE_CODE (type) == UNION_TYPE
	   || TREE_CODE (type) == QUAL_UNION_TYPE)
	  && TREE_PURPOSE (link) != 0)
	field = TREE_PURPOSE (link);

      else if (TREE_CODE (type) == ARRAY_TYPE)
	index = TREE_PURPOSE (link);

      /* Eliminate the marker that makes a cast not be an lvalue.  */
      if (val != 0)
	STRIP_NOPS (val);

      if (index && TREE_CODE (index) == RANGE_EXPR)
	{
	  unsigned HOST_WIDE_INT fieldsize
	    = int_size_in_bytes (TREE_TYPE (type));
	  HOST_WIDE_INT lo_index = tree_low_cst (TREE_OPERAND (index, 0), 0);
	  HOST_WIDE_INT hi_index = tree_low_cst (TREE_OPERAND (index, 1), 0);
	  HOST_WIDE_INT index;
	  unsigned int align2 = min_align (align, fieldsize * BITS_PER_UNIT);

	  for (index = lo_index; index <= hi_index; index++)
	    {
	      /* Output the element's initial value.  */
	      if (val == 0)
		assemble_zeros (fieldsize);
	      else
		output_constant (val, fieldsize, align2);

	      /* Count its size.  */
	      total_bytes += fieldsize;
	    }
	}
      else if (field == 0 || !DECL_BIT_FIELD (field))
	{
	  /* An element that is not a bit-field.  */

	  unsigned HOST_WIDE_INT fieldsize;
	  /* Since this structure is static,
	     we know the positions are constant.  */
	  HOST_WIDE_INT pos = field ? int_byte_position (field) : 0;
	  unsigned int align2;

	  if (index != 0)
	    pos = (tree_low_cst (TYPE_SIZE_UNIT (TREE_TYPE (val)), 1)
		   * (tree_low_cst (index, 0) - tree_low_cst (min_index, 0)));

	  /* Output any buffered-up bit-fields preceding this element.  */
	  if (byte_buffer_in_use)
	    {
	      assemble_integer (GEN_INT (byte), 1, BITS_PER_UNIT, 1);
	      total_bytes++;
	      byte_buffer_in_use = 0;
	    }

	  /* Advance to offset of this element.
	     Note no alignment needed in an array, since that is guaranteed
	     if each element has the proper size.  */
	  if ((field != 0 || index != 0) && pos != total_bytes)
	    {
	      assemble_zeros (pos - total_bytes);
	      total_bytes = pos;
	    }

	  /* Find the alignment of this element.  */
	  align2 = min_align (align, BITS_PER_UNIT * pos);

	  /* Determine size this element should occupy.  */
	  if (field)
	    {
	      fieldsize = 0;

	      /* If this is an array with an unspecified upper bound,
		 the initializer determines the size.  */
	      /* ??? This ought to only checked if DECL_SIZE_UNIT is NULL,
		 but we cannot do this until the deprecated support for
		 initializing zero-length array members is removed.  */
	      if (TREE_CODE (TREE_TYPE (field)) == ARRAY_TYPE
		  && TYPE_DOMAIN (TREE_TYPE (field))
		  && ! TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (field))))
		{
		  fieldsize = array_size_for_constructor (val);
		  /* Given a non-empty initialization, this field had
		     better be last.  */
		  if (fieldsize != 0 && TREE_CHAIN (field) != NULL_TREE)
		    abort ();
		}
	      else if (DECL_SIZE_UNIT (field))
		{
		  /* ??? This can't be right.  If the decl size overflows
		     a host integer we will silently emit no data.  */
		  if (host_integerp (DECL_SIZE_UNIT (field), 1))
		    fieldsize = tree_low_cst (DECL_SIZE_UNIT (field), 1);
		}
	    }
	  else
	    fieldsize = int_size_in_bytes (TREE_TYPE (type));

	  /* Output the element's initial value.  */
	  if (val == 0)
	    assemble_zeros (fieldsize);
	  else
	    output_constant (val, fieldsize, align2);

	  /* Count its size.  */
	  total_bytes += fieldsize;
	}
      else if (val != 0 && TREE_CODE (val) != INTEGER_CST)
	error ("invalid initial value for member `%s'",
	       IDENTIFIER_POINTER (DECL_NAME (field)));
      else
	{
	  /* Element that is a bit-field.  */

	  HOST_WIDE_INT next_offset = int_bit_position (field);
	  HOST_WIDE_INT end_offset
	    = (next_offset + tree_low_cst (DECL_SIZE (field), 1));

	  if (val == 0)
	    val = integer_zero_node;

	  /* If this field does not start in this (or, next) byte,
	     skip some bytes.  */
	  if (next_offset / BITS_PER_UNIT != total_bytes)
	    {
	      /* Output remnant of any bit field in previous bytes.  */
	      if (byte_buffer_in_use)
		{
		  assemble_integer (GEN_INT (byte), 1, BITS_PER_UNIT, 1);
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
	      HOST_WIDE_INT next_byte = next_offset / BITS_PER_UNIT;
	      HOST_WIDE_INT next_bit = next_offset % BITS_PER_UNIT;

	      /* Advance from byte to byte
		 within this element when necessary.  */
	      while (next_byte != total_bytes)
		{
		  assemble_integer (GEN_INT (byte), 1, BITS_PER_UNIT, 1);
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
			   << (BITS_PER_UNIT - this_time - next_bit));
		}
	      else
		{
		  /* On little-endian machines,
		     take first the least significant bits of the value
		     and pack them starting at the least significant
		     bits of the bytes.  */
		  shift = next_offset - int_bit_position (field);

		  /* Don't try to take a bunch of bits that cross
		     the word boundary in the INTEGER_CST. We can
		     only select bits from the LOW or HIGH part
		     not from both.  */
		  if (shift < HOST_BITS_PER_WIDE_INT
		      && shift + this_time > HOST_BITS_PER_WIDE_INT)
		    this_time = (HOST_BITS_PER_WIDE_INT - shift);

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
      assemble_integer (GEN_INT (byte), 1, BITS_PER_UNIT, 1);
      total_bytes++;
    }

  if (total_bytes < size)
    assemble_zeros (size - total_bytes);
}

/* This TREE_LIST contains any weak symbol declarations waiting
   to be emitted.  */
static tree weak_decls;

/* Mark DECL as weak.  */

static void
mark_weak (decl)
     tree decl;
{
  DECL_WEAK (decl) = 1;

  if (DECL_RTL_SET_P (decl)
      && GET_CODE (DECL_RTL (decl)) == MEM
      && XEXP (DECL_RTL (decl), 0)
      && GET_CODE (XEXP (DECL_RTL (decl), 0)) == SYMBOL_REF)
    SYMBOL_REF_WEAK (XEXP (DECL_RTL (decl), 0)) = 1;
}
 
/* Merge weak status between NEWDECL and OLDDECL.  */

void
merge_weak (newdecl, olddecl)
     tree newdecl;
     tree olddecl;
{
  if (DECL_WEAK (newdecl) == DECL_WEAK (olddecl))
    return;

  if (DECL_WEAK (newdecl))
    {
      tree wd;
      
      /* NEWDECL is weak, but OLDDECL is not.  */

      /* If we already output the OLDDECL, we're in trouble; we can't
	 go back and make it weak.  This error cannot caught in
	 declare_weak because the NEWDECL and OLDDECL was not yet
	 been merged; therefore, TREE_ASM_WRITTEN was not set.  */
      if (TREE_ASM_WRITTEN (olddecl))
	error_with_decl (newdecl, 
			 "weak declaration of `%s' must precede definition");

      /* If we've already generated rtl referencing OLDDECL, we may
	 have done so in a way that will not function properly with
	 a weak symbol.  */
      else if (TREE_USED (olddecl)
	       && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (olddecl)))
	warning_with_decl (newdecl, "weak declaration of `%s' after first use results in unspecified behavior");

      if (SUPPORTS_WEAK)
	{
	  /* We put the NEWDECL on the weak_decls list at some point.
	     Replace it with the OLDDECL.  */
	  for (wd = weak_decls; wd; wd = TREE_CHAIN (wd))
	    if (TREE_VALUE (wd) == newdecl)
	      {
		TREE_VALUE (wd) = olddecl;
		break;
	      }
	  /* We may not find the entry on the list.  If NEWDECL is a
	     weak alias, then we will have already called
	     globalize_decl to remove the entry; in that case, we do
	     not need to do anything.  */
	}

      /* Make the OLDDECL weak; it's OLDDECL that we'll be keeping.  */
      mark_weak (olddecl);
    }
  else
    /* OLDDECL was weak, but NEWDECL was not explicitly marked as
       weak.  Just update NEWDECL to indicate that it's weak too.  */
    mark_weak (newdecl);
}

/* Declare DECL to be a weak symbol.  */

void
declare_weak (decl)
     tree decl;
{
  if (! TREE_PUBLIC (decl))
    error_with_decl (decl, "weak declaration of `%s' must be public");
  else if (TREE_CODE (decl) == FUNCTION_DECL && TREE_ASM_WRITTEN (decl))
    error_with_decl (decl, "weak declaration of `%s' must precede definition");
  else if (SUPPORTS_WEAK)
    {
      if (! DECL_WEAK (decl))
	weak_decls = tree_cons (NULL, decl, weak_decls);
    }
  else
    warning_with_decl (decl, "weak declaration of `%s' not supported");

  mark_weak (decl);
}

/* Emit any pending weak declarations.  */

void
weak_finish ()
{
  tree t;

  for (t = weak_decls; t ; t = TREE_CHAIN (t))
    {
      tree decl = TREE_VALUE (t);
      const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

      if (! TREE_USED (decl))
	continue;

#ifdef ASM_WEAKEN_DECL
      ASM_WEAKEN_DECL (asm_out_file, decl, name, NULL);
#else
#ifdef ASM_WEAKEN_LABEL
      ASM_WEAKEN_LABEL (asm_out_file, name);
#else
#ifdef ASM_OUTPUT_WEAK_ALIAS
      warning ("only weak aliases are supported in this configuration");
      return;
#endif
#endif
#endif
    }
}

/* Emit the assembly bits to indicate that DECL is globally visible.  */

static void
globalize_decl (decl)
     tree decl;
{
  const char *name = XSTR (XEXP (DECL_RTL (decl), 0), 0);

#if defined (ASM_WEAKEN_LABEL) || defined (ASM_WEAKEN_DECL)
  if (DECL_WEAK (decl))
    {
      tree *p, t;

#ifdef ASM_WEAKEN_DECL
      ASM_WEAKEN_DECL (asm_out_file, decl, name, 0);
#else
      ASM_WEAKEN_LABEL (asm_out_file, name);
#endif

      /* Remove this function from the pending weak list so that
	 we do not emit multiple .weak directives for it.  */
      for (p = &weak_decls; (t = *p) ; )
	{
	  if (DECL_ASSEMBLER_NAME (decl) == DECL_ASSEMBLER_NAME (TREE_VALUE (t)))
	    *p = TREE_CHAIN (t);
	  else
	    p = &TREE_CHAIN (t);
	}
      return;
    }
#endif

  ASM_GLOBALIZE_LABEL (asm_out_file, name);
}

/* Emit an assembler directive to make the symbol for DECL an alias to
   the symbol for TARGET.  */

void
assemble_alias (decl, target)
     tree decl, target ATTRIBUTE_UNUSED;
{
  const char *name;

  /* We must force creation of DECL_RTL for debug info generation, even though
     we don't use it here.  */
  make_decl_rtl (decl, NULL);

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

#ifdef ASM_OUTPUT_DEF
  /* Make name accessible from other files, if appropriate.  */
  if (TREE_PUBLIC (decl))
    globalize_decl (decl);

#ifdef ASM_OUTPUT_DEF_FROM_DECLS
  ASM_OUTPUT_DEF_FROM_DECLS (asm_out_file, decl, target);
#else
  ASM_OUTPUT_DEF (asm_out_file, name, IDENTIFIER_POINTER (target));
#endif
#else /* !ASM_OUTPUT_DEF */
#if defined (ASM_OUTPUT_WEAK_ALIAS) || defined (ASM_WEAKEN_DECL)
  if (! DECL_WEAK (decl))
    warning ("only weak aliases are supported in this configuration");

#ifdef ASM_WEAKEN_DECL
  ASM_WEAKEN_DECL (asm_out_file, decl, name, IDENTIFIER_POINTER (target));
#else
  ASM_OUTPUT_WEAK_ALIAS (asm_out_file, name, IDENTIFIER_POINTER (target));
#endif
#else
  warning ("alias definitions not supported in this configuration; ignored");
#endif
#endif

  TREE_USED (decl) = 1;
  TREE_ASM_WRITTEN (decl) = 1;
  TREE_ASM_WRITTEN (DECL_ASSEMBLER_NAME (decl)) = 1;
}

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
  const_str_htab = htab_create (128, const_str_htab_hash, const_str_htab_eq,
  				const_str_htab_del);
  in_named_htab = htab_create (31, in_named_entry_hash,
			       in_named_entry_eq, NULL);

  ggc_add_root (const_hash_table, MAX_HASH_TABLE, sizeof const_hash_table[0],
		mark_const_hash_entry);
  ggc_add_root (&const_str_htab, 1, sizeof const_str_htab,
		mark_const_str_htab);
  ggc_add_tree_root (&weak_decls, 1);

  const_alias_set = new_alias_set ();
}

/* Select a set of attributes for section NAME based on the properties
   of DECL and whether or not RELOC indicates that DECL's initializer
   might contain runtime relocations.

   We make the section read-only and executable for a function decl,
   read-only for a const data decl, and writable for a non-const data decl.  */

unsigned int
default_section_type_flags (decl, name, reloc)
     tree decl;
     const char *name;
     int reloc;
{
  unsigned int flags;

  if (decl && TREE_CODE (decl) == FUNCTION_DECL)
    flags = SECTION_CODE;
  else if (decl && DECL_READONLY_SECTION (decl, reloc))
    flags = 0;
  else
    flags = SECTION_WRITE;

  if (decl && DECL_ONE_ONLY (decl))
    flags |= SECTION_LINKONCE;

  if (strcmp (name, ".bss") == 0
      || strncmp (name, ".bss.", 5) == 0
      || strncmp (name, ".gnu.linkonce.b.", 16) == 0
      || strcmp (name, ".sbss") == 0
      || strncmp (name, ".sbss.", 6) == 0
      || strncmp (name, ".gnu.linkonce.sb.", 17) == 0)
    flags |= SECTION_BSS;

  return flags;
}

/* Output assembly to switch to section NAME with attribute FLAGS.
   Four variants for common object file formats.  */

void
default_no_named_section (name, flags)
     const char *name ATTRIBUTE_UNUSED;
     unsigned int flags ATTRIBUTE_UNUSED;
{
  /* Some object formats don't support named sections at all.  The
     front-end should already have flagged this as an error.  */
  abort ();
}

void
default_elf_asm_named_section (name, flags)
     const char *name;
     unsigned int flags;
{
  char flagchars[10], *f = flagchars;
  const char *type;

  if (! named_section_first_declaration (name))
    {
      fprintf (asm_out_file, "\t.section\t%s\n", name);
      return;
    }

  if (!(flags & SECTION_DEBUG))
    *f++ = 'a';
  if (flags & SECTION_WRITE)
    *f++ = 'w';
  if (flags & SECTION_CODE)
    *f++ = 'x';
  if (flags & SECTION_SMALL)
    *f++ = 's';
  if (flags & SECTION_MERGE)
    *f++ = 'M';
  if (flags & SECTION_STRINGS)
    *f++ = 'S';
  *f = '\0';

  if (flags & SECTION_BSS)
    type = "nobits";
  else
    type = "progbits";

  if (flags & SECTION_ENTSIZE)
    fprintf (asm_out_file, "\t.section\t%s,\"%s\",@%s,%d\n",
	     name, flagchars, type, flags & SECTION_ENTSIZE);
  else
    fprintf (asm_out_file, "\t.section\t%s,\"%s\",@%s\n",
	     name, flagchars, type);
}

void
default_coff_asm_named_section (name, flags)
     const char *name;
     unsigned int flags;
{
  char flagchars[8], *f = flagchars;

  if (flags & SECTION_WRITE)
    *f++ = 'w';
  if (flags & SECTION_CODE)
    *f++ = 'x';
  *f = '\0';

  fprintf (asm_out_file, "\t.section\t%s,\"%s\"\n", name, flagchars);
}

void
default_pe_asm_named_section (name, flags)
     const char *name;
     unsigned int flags;
{
  default_coff_asm_named_section (name, flags);

  if (flags & SECTION_LINKONCE)
    {
      /* Functions may have been compiled at various levels of
         optimization so we can't use `same_size' here.
         Instead, have the linker pick one.  */
      fprintf (asm_out_file, "\t.linkonce %s\n",
	       (flags & SECTION_CODE ? "discard" : "same_size"));
    }
}

/* Used for vtable gc in GNU binutils.  Record that the pointer at OFFSET
   from SYMBOL is used in all classes derived from SYMBOL.  */

void
assemble_vtable_entry (symbol, offset)
     rtx symbol;
     HOST_WIDE_INT offset;
{
  fputs ("\t.vtable_entry ", asm_out_file);
  output_addr_const (asm_out_file, symbol);
  fputs (", ", asm_out_file);
  fprintf (asm_out_file, HOST_WIDE_INT_PRINT_DEC, offset);
  fputc ('\n', asm_out_file);
}

/* Used for vtable gc in GNU binutils.  Record the class hierarchy by noting
   that the vtable symbol CHILD is derived from the vtable symbol PARENT.  */

void
assemble_vtable_inherit (child, parent)
     rtx child, parent;
{
  fputs ("\t.vtable_inherit ", asm_out_file);
  output_addr_const (asm_out_file, child);
  fputs (", ", asm_out_file);
  output_addr_const (asm_out_file, parent);
  fputc ('\n', asm_out_file);
}
