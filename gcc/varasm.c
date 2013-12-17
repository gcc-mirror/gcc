/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 1987-2013 Free Software Foundation, Inc.

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


/* This file handles generation of all the assembler code
   *except* the instructions of a function.
   This includes declarations of variables and their initial values.

   We also output the assembler code for constants stored in memory
   and are responsible for combining constants with the same value.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "stor-layout.h"
#include "stringpool.h"
#include "gcc-symtab.h"
#include "varasm.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "output.h"
#include "diagnostic-core.h"
#include "hashtab.h"
#include "ggc.h"
#include "langhooks.h"
#include "tm_p.h"
#include "debug.h"
#include "target.h"
#include "common/common-target.h"
#include "targhooks.h"
#include "cgraph.h"
#include "pointer-set.h"
#include "asan.h"
#include "basic-block.h"

#ifdef XCOFF_DEBUGGING_INFO
#include "xcoffout.h"		/* Needed for external data
				   declarations for e.g. AIX 4.x.  */
#endif

/* The (assembler) name of the first globally-visible object output.  */
extern GTY(()) const char *first_global_object_name;
extern GTY(()) const char *weak_global_object_name;

const char *first_global_object_name;
const char *weak_global_object_name;

struct addr_const;
struct constant_descriptor_rtx;
struct rtx_constant_pool;

#define n_deferred_constants (crtl->varasm.deferred_constants)

/* Number for making the label on the next
   constant that is stored in memory.  */

static GTY(()) int const_labelno;

/* Carry information from ASM_DECLARE_OBJECT_NAME
   to ASM_FINISH_DECLARE_OBJECT.  */

int size_directive_output;

/* The last decl for which assemble_variable was called,
   if it did ASM_DECLARE_OBJECT_NAME.
   If the last call to assemble_variable didn't do that,
   this holds 0.  */

tree last_assemble_variable_decl;

/* The following global variable indicates if the first basic block
   in a function belongs to the cold partition or not.  */

bool first_function_block_is_cold;

/* We give all constants their own alias set.  Perhaps redundant with
   MEM_READONLY_P, but pre-dates it.  */

static alias_set_type const_alias_set;

/* Whether we saw any functions with no_split_stack.  */

static bool saw_no_split_stack;

static const char *strip_reg_name (const char *);
static int contains_pointers_p (tree);
#ifdef ASM_OUTPUT_EXTERNAL
static bool incorporeal_function_p (tree);
#endif
static void decode_addr_const (tree, struct addr_const *);
static hashval_t const_desc_hash (const void *);
static int const_desc_eq (const void *, const void *);
static hashval_t const_hash_1 (const tree);
static int compare_constant (const tree, const tree);
static tree copy_constant (tree);
static void output_constant_def_contents (rtx);
static void output_addressed_constants (tree);
static unsigned HOST_WIDE_INT array_size_for_constructor (tree);
static unsigned min_align (unsigned, unsigned);
static void globalize_decl (tree);
static bool decl_readonly_section_1 (enum section_category);
#ifdef BSS_SECTION_ASM_OP
#ifdef ASM_OUTPUT_ALIGNED_BSS
static void asm_output_aligned_bss (FILE *, tree, const char *,
				    unsigned HOST_WIDE_INT, int)
     ATTRIBUTE_UNUSED;
#endif
#endif /* BSS_SECTION_ASM_OP */
static void mark_weak (tree);
static void output_constant_pool (const char *, tree);

/* Well-known sections, each one associated with some sort of *_ASM_OP.  */
section *text_section;
section *data_section;
section *readonly_data_section;
section *sdata_section;
section *ctors_section;
section *dtors_section;
section *bss_section;
section *sbss_section;

/* Various forms of common section.  All are guaranteed to be nonnull.  */
section *tls_comm_section;
section *comm_section;
section *lcomm_section;

/* A SECTION_NOSWITCH section used for declaring global BSS variables.
   May be null.  */
section *bss_noswitch_section;

/* The section that holds the main exception table, when known.  The section
   is set either by the target's init_sections hook or by the first call to
   switch_to_exception_section.  */
section *exception_section;

/* The section that holds the DWARF2 frame unwind information, when known.
   The section is set either by the target's init_sections hook or by the
   first call to switch_to_eh_frame_section.  */
section *eh_frame_section;

/* asm_out_file's current section.  This is NULL if no section has yet
   been selected or if we lose track of what the current section is.  */
section *in_section;

/* True if code for the current function is currently being directed
   at the cold section.  */
bool in_cold_section_p;

/* A linked list of all the unnamed sections.  */
static GTY(()) section *unnamed_sections;

/* Return a nonzero value if DECL has a section attribute.  */
#define IN_NAMED_SECTION(DECL) \
  ((TREE_CODE (DECL) == FUNCTION_DECL || TREE_CODE (DECL) == VAR_DECL) \
   && DECL_SECTION_NAME (DECL) != NULL_TREE)

/* Hash table of named sections.  */
static GTY((param_is (section))) htab_t section_htab;

/* A table of object_blocks, indexed by section.  */
static GTY((param_is (struct object_block))) htab_t object_block_htab;

/* The next number to use for internal anchor labels.  */
static GTY(()) int anchor_labelno;

/* A pool of constants that can be shared between functions.  */
static GTY(()) struct rtx_constant_pool *shared_constant_pool;

/* Helper routines for maintaining section_htab.  */

static int
section_entry_eq (const void *p1, const void *p2)
{
  const section *old = (const section *) p1;
  const char *new_name = (const char *) p2;

  return strcmp (old->named.name, new_name) == 0;
}

static hashval_t
section_entry_hash (const void *p)
{
  const section *old = (const section *) p;
  return htab_hash_string (old->named.name);
}

/* Return a hash value for section SECT.  */

static hashval_t
hash_section (section *sect)
{
  if (sect->common.flags & SECTION_NAMED)
    return htab_hash_string (sect->named.name);
  return sect->common.flags;
}

/* Helper routines for maintaining object_block_htab.  */

static int
object_block_entry_eq (const void *p1, const void *p2)
{
  const struct object_block *old = (const struct object_block *) p1;
  const section *new_section = (const section *) p2;

  return old->sect == new_section;
}

static hashval_t
object_block_entry_hash (const void *p)
{
  const struct object_block *old = (const struct object_block *) p;
  return hash_section (old->sect);
}

/* Return a new unnamed section with the given fields.  */

section *
get_unnamed_section (unsigned int flags, void (*callback) (const void *),
		     const void *data)
{
  section *sect;

  sect = ggc_alloc_section ();
  sect->unnamed.common.flags = flags | SECTION_UNNAMED;
  sect->unnamed.callback = callback;
  sect->unnamed.data = data;
  sect->unnamed.next = unnamed_sections;

  unnamed_sections = sect;
  return sect;
}

/* Return a SECTION_NOSWITCH section with the given fields.  */

static section *
get_noswitch_section (unsigned int flags, noswitch_section_callback callback)
{
  section *sect;

  sect = ggc_alloc_section ();
  sect->noswitch.common.flags = flags | SECTION_NOSWITCH;
  sect->noswitch.callback = callback;

  return sect;
}

/* Return the named section structure associated with NAME.  Create
   a new section with the given fields if no such structure exists.  */

section *
get_section (const char *name, unsigned int flags, tree decl)
{
  section *sect, **slot;

  slot = (section **)
    htab_find_slot_with_hash (section_htab, name,
			      htab_hash_string (name), INSERT);
  flags |= SECTION_NAMED;
  if (*slot == NULL)
    {
      sect = ggc_alloc_section ();
      sect->named.common.flags = flags;
      sect->named.name = ggc_strdup (name);
      sect->named.decl = decl;
      *slot = sect;
    }
  else
    {
      sect = *slot;
      if ((sect->common.flags & ~SECTION_DECLARED) != flags
	  && ((sect->common.flags | flags) & SECTION_OVERRIDE) == 0)
	{
	  /* It is fine if one of the section flags is
	     SECTION_WRITE | SECTION_RELRO and the other has none of these
	     flags (i.e. read-only) in named sections and either the
	     section hasn't been declared yet or has been declared as writable.
	     In that case just make sure the resulting flags are
	     SECTION_WRITE | SECTION_RELRO, ie. writable only because of
	     relocations.  */
	  if (((sect->common.flags ^ flags) & (SECTION_WRITE | SECTION_RELRO))
	      == (SECTION_WRITE | SECTION_RELRO)
	      && (sect->common.flags
		  & ~(SECTION_DECLARED | SECTION_WRITE | SECTION_RELRO))
		 == (flags & ~(SECTION_WRITE | SECTION_RELRO))
	      && ((sect->common.flags & SECTION_DECLARED) == 0
		  || (sect->common.flags & SECTION_WRITE)))
	    {
	      sect->common.flags |= (SECTION_WRITE | SECTION_RELRO);
	      return sect;
	    }
	  /* Sanity check user variables for flag changes.  */
	  if (sect->named.decl != NULL
	      && DECL_P (sect->named.decl)
	      && decl != sect->named.decl)
	    {
	      if (decl != NULL && DECL_P (decl))
		error ("%+D causes a section type conflict with %D",
		       decl, sect->named.decl);
	      else
		error ("section type conflict with %D", sect->named.decl);
	      inform (DECL_SOURCE_LOCATION (sect->named.decl),
		      "%qD was declared here", sect->named.decl);
	    }
	  else if (decl != NULL && DECL_P (decl))
	    error ("%+D causes a section type conflict", decl);
	  else
	    error ("section type conflict");
	  /* Make sure we don't error about one section multiple times.  */
	  sect->common.flags |= SECTION_OVERRIDE;
	}
    }
  return sect;
}

/* Return true if the current compilation mode benefits from having
   objects grouped into blocks.  */

static bool
use_object_blocks_p (void)
{
  return flag_section_anchors;
}

/* Return the object_block structure for section SECT.  Create a new
   structure if we haven't created one already.  Return null if SECT
   itself is null.  */

static struct object_block *
get_block_for_section (section *sect)
{
  struct object_block *block;
  void **slot;

  if (sect == NULL)
    return NULL;

  slot = htab_find_slot_with_hash (object_block_htab, sect,
				   hash_section (sect), INSERT);
  block = (struct object_block *) *slot;
  if (block == NULL)
    {
      block = ggc_alloc_cleared_object_block ();
      block->sect = sect;
      *slot = block;
    }
  return block;
}

/* Create a symbol with label LABEL and place it at byte offset
   OFFSET in BLOCK.  OFFSET can be negative if the symbol's offset
   is not yet known.  LABEL must be a garbage-collected string.  */

static rtx
create_block_symbol (const char *label, struct object_block *block,
		     HOST_WIDE_INT offset)
{
  rtx symbol;
  unsigned int size;

  /* Create the extended SYMBOL_REF.  */
  size = RTX_HDR_SIZE + sizeof (struct block_symbol);
  symbol = ggc_alloc_rtx_def (size);

  /* Initialize the normal SYMBOL_REF fields.  */
  memset (symbol, 0, size);
  PUT_CODE (symbol, SYMBOL_REF);
  PUT_MODE (symbol, Pmode);
  XSTR (symbol, 0) = label;
  SYMBOL_REF_FLAGS (symbol) = SYMBOL_FLAG_HAS_BLOCK_INFO;

  /* Initialize the block_symbol stuff.  */
  SYMBOL_REF_BLOCK (symbol) = block;
  SYMBOL_REF_BLOCK_OFFSET (symbol) = offset;

  return symbol;
}

/* Return a section with a particular name and with whatever SECTION_*
   flags section_type_flags deems appropriate.  The name of the section
   is taken from NAME if nonnull, otherwise it is taken from DECL's
   DECL_SECTION_NAME.  DECL is the decl associated with the section
   (see the section comment for details) and RELOC is as for
   section_type_flags.  */

section *
get_named_section (tree decl, const char *name, int reloc)
{
  unsigned int flags;

  if (name == NULL)
    {
      gcc_assert (decl && DECL_P (decl) && DECL_SECTION_NAME (decl));
      name = TREE_STRING_POINTER (DECL_SECTION_NAME (decl));
    }

  flags = targetm.section_type_flags (decl, name, reloc);
  return get_section (name, flags, decl);
}

/* If required, set DECL_SECTION_NAME to a unique name.  */

void
resolve_unique_section (tree decl, int reloc ATTRIBUTE_UNUSED,
			int flag_function_or_data_sections)
{
  if (DECL_SECTION_NAME (decl) == NULL_TREE
      && targetm_common.have_named_sections
      && (flag_function_or_data_sections
	  || DECL_ONE_ONLY (decl)))
    {
      targetm.asm_out.unique_section (decl, reloc);
      DECL_HAS_IMPLICIT_SECTION_NAME_P (decl) = true;
    }
}

#ifdef BSS_SECTION_ASM_OP

#ifdef ASM_OUTPUT_ALIGNED_BSS

/* Utility function for targets to use in implementing
   ASM_OUTPUT_ALIGNED_BSS.
   ??? It is believed that this function will work in most cases so such
   support is localized here.  */

static void
asm_output_aligned_bss (FILE *file, tree decl ATTRIBUTE_UNUSED,
			const char *name, unsigned HOST_WIDE_INT size,
			int align)
{
  switch_to_section (bss_section);
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

#ifndef USE_SELECT_SECTION_FOR_FUNCTIONS
/* Return the hot section for function DECL.  Return text_section for
   null DECLs.  */

static section *
hot_function_section (tree decl)
{
  if (decl != NULL_TREE
      && DECL_SECTION_NAME (decl) != NULL_TREE
      && targetm_common.have_named_sections)
    return get_named_section (decl, NULL, 0);
  else
    return text_section;
}
#endif

/* Return section for TEXT_SECTION_NAME if DECL or DECL_SECTION_NAME (DECL)
   is NULL.

   When DECL_SECTION_NAME is non-NULL and it is implicit section and
   NAMED_SECTION_SUFFIX is non-NULL, then produce section called
   concatenate the name with NAMED_SECTION_SUFFIX.
   Otherwise produce "TEXT_SECTION_NAME.IMPLICIT_NAME".  */

section *
get_named_text_section (tree decl,
		        const char *text_section_name,
		        const char *named_section_suffix)
{
  if (decl && DECL_SECTION_NAME (decl))
    {
      if (named_section_suffix)
	{
	  tree dsn = DECL_SECTION_NAME (decl);
	  const char *stripped_name;
	  char *name, *buffer;

	  name = (char *) alloca (TREE_STRING_LENGTH (dsn) + 1);
	  memcpy (name, TREE_STRING_POINTER (dsn),
		  TREE_STRING_LENGTH (dsn) + 1);

	  stripped_name = targetm.strip_name_encoding (name);

	  buffer = ACONCAT ((stripped_name, named_section_suffix, NULL));
	  return get_named_section (decl, buffer, 0);
	}
      else if (DECL_HAS_IMPLICIT_SECTION_NAME_P (decl))
	{
	  const char *name;

	  /* Do not try to split gnu_linkonce functions.  This gets somewhat
	     slipperly.  */
	  if (DECL_ONE_ONLY (decl) && !HAVE_COMDAT_GROUP)
	    return NULL;
	  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
	  name = targetm.strip_name_encoding (name);
	  return get_named_section (decl, ACONCAT ((text_section_name, ".",
				                   name, NULL)), 0);
	}
      else
	return NULL;
    }
  return get_named_section (decl, text_section_name, 0);
}

/* Choose named function section based on its frequency.  */

section *
default_function_section (tree decl, enum node_frequency freq,
			  bool startup, bool exit)
{
#if defined HAVE_LD_EH_GC_SECTIONS && defined HAVE_LD_EH_GC_SECTIONS_BUG
  /* Old GNU linkers have buggy --gc-section support, which sometimes
     results in .gcc_except_table* sections being garbage collected.  */
  if (decl
      && DECL_SECTION_NAME (decl)
      && DECL_HAS_IMPLICIT_SECTION_NAME_P (decl))
    return NULL;
#endif

  if (!flag_reorder_functions
      || !targetm_common.have_named_sections)
    return NULL;
  /* Startup code should go to startup subsection unless it is
     unlikely executed (this happens especially with function splitting
     where we can split away unnecessary parts of static constructors.  */
  if (startup && freq != NODE_FREQUENCY_UNLIKELY_EXECUTED)
  {
    /* If we do have a profile or(and) LTO phase is executed, we do not need
       these ELF section.  */
    if (!in_lto_p || !flag_profile_values)
      return get_named_text_section (decl, ".text.startup", NULL);
    else
      return NULL;
  }

  /* Similarly for exit.  */
  if (exit && freq != NODE_FREQUENCY_UNLIKELY_EXECUTED)
    return get_named_text_section (decl, ".text.exit", NULL);

  /* Group cold functions together, similarly for hot code.  */
  switch (freq)
    {
      case NODE_FREQUENCY_UNLIKELY_EXECUTED:
	return get_named_text_section (decl, ".text.unlikely", NULL);
      case NODE_FREQUENCY_HOT:
        /* If we do have a profile or(and) LTO phase is executed, we do not need
           these ELF section.  */
        if (!in_lto_p || !flag_profile_values)
          return get_named_text_section (decl, ".text.hot", NULL);
      default:
	return NULL;
    }
}

/* Return the section for function DECL.

   If DECL is NULL_TREE, return the text section.  We can be passed
   NULL_TREE under some circumstances by dbxout.c at least.

   If FORCE_COLD is true, return cold function section ignoring
   the frequency info of cgraph_node.  */

static section *
function_section_1 (tree decl, bool force_cold)
{
  section *section = NULL;
  enum node_frequency freq = NODE_FREQUENCY_NORMAL;
  bool startup = false, exit = false;

  if (decl)
    {
      struct cgraph_node *node = cgraph_get_node (decl);

      if (node)
	{
	  freq = node->frequency;
	  startup = node->only_called_at_startup;
	  exit = node->only_called_at_exit;
	}
    }
  if (force_cold)
    freq = NODE_FREQUENCY_UNLIKELY_EXECUTED;

#ifdef USE_SELECT_SECTION_FOR_FUNCTIONS
  if (decl != NULL_TREE
      && DECL_SECTION_NAME (decl) != NULL_TREE)
    {
      if (targetm.asm_out.function_section)
	section = targetm.asm_out.function_section (decl, freq,
						    startup, exit);
      if (section)
	return section;
      return get_named_section (decl, NULL, 0);
    }
  else
    return targetm.asm_out.select_section
	    (decl, freq == NODE_FREQUENCY_UNLIKELY_EXECUTED,
	     DECL_ALIGN (decl));
#else
  if (targetm.asm_out.function_section)
    section = targetm.asm_out.function_section (decl, freq, startup, exit);
  if (section)
    return section;
  return hot_function_section (decl);
#endif
}

/* Return the section for function DECL.

   If DECL is NULL_TREE, return the text section.  We can be passed
   NULL_TREE under some circumstances by dbxout.c at least.  */

section *
function_section (tree decl)
{
  /* Handle cases where function splitting code decides
     to put function entry point into unlikely executed section
     despite the fact that the function itself is not cold
     (i.e. it is called rarely but contains a hot loop that is
     better to live in hot subsection for the code locality).  */
  return function_section_1 (decl,
			     first_function_block_is_cold);
}

/* Return the section for the current function, take IN_COLD_SECTION_P
   into account.  */

section *
current_function_section (void)
{
  return function_section_1 (current_function_decl, in_cold_section_p);
}

/* Tell assembler to switch to unlikely-to-be-executed text section.  */

section *
unlikely_text_section (void)
{
  return function_section_1 (current_function_decl, true);
}

/* When called within a function context, return true if the function
   has been assigned a cold text section and if SECT is that section.
   When called outside a function context, return true if SECT is the
   default cold section.  */

bool
unlikely_text_section_p (section *sect)
{
  return sect == function_section_1 (current_function_decl, true);
}

/* Return the read-only data section associated with function DECL.  */

section *
default_function_rodata_section (tree decl)
{
  if (decl != NULL_TREE && DECL_SECTION_NAME (decl))
    {
      const char *name = TREE_STRING_POINTER (DECL_SECTION_NAME (decl));

      if (DECL_ONE_ONLY (decl) && HAVE_COMDAT_GROUP)
        {
	  const char *dot;
	  size_t len;
	  char* rname;

	  dot = strchr (name + 1, '.');
	  if (!dot)
	    dot = name;
	  len = strlen (dot) + 8;
	  rname = (char *) alloca (len);

	  strcpy (rname, ".rodata");
	  strcat (rname, dot);
	  return get_section (rname, SECTION_LINKONCE, decl);
	}
      /* For .gnu.linkonce.t.foo we want to use .gnu.linkonce.r.foo.  */
      else if (DECL_ONE_ONLY (decl)
	       && strncmp (name, ".gnu.linkonce.t.", 16) == 0)
	{
	  size_t len = strlen (name) + 1;
	  char *rname = (char *) alloca (len);

	  memcpy (rname, name, len);
	  rname[14] = 'r';
	  return get_section (rname, SECTION_LINKONCE, decl);
	}
      /* For .text.foo we want to use .rodata.foo.  */
      else if (flag_function_sections && flag_data_sections
	       && strncmp (name, ".text.", 6) == 0)
	{
	  size_t len = strlen (name) + 1;
	  char *rname = (char *) alloca (len + 2);

	  memcpy (rname, ".rodata", 7);
	  memcpy (rname + 7, name + 5, len - 5);
	  return get_section (rname, 0, decl);
	}
    }

  return readonly_data_section;
}

/* Return the read-only data section associated with function DECL
   for targets where that section should be always the single
   readonly data section.  */

section *
default_no_function_rodata_section (tree decl ATTRIBUTE_UNUSED)
{
  return readonly_data_section;
}

/* Return the section to use for string merging.  */

static section *
mergeable_string_section (tree decl ATTRIBUTE_UNUSED,
			  unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED,
			  unsigned int flags ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT len;

  if (HAVE_GAS_SHF_MERGE && flag_merge_constants
      && TREE_CODE (decl) == STRING_CST
      && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
      && align <= 256
      && (len = int_size_in_bytes (TREE_TYPE (decl))) > 0
      && TREE_STRING_LENGTH (decl) >= len)
    {
      enum machine_mode mode;
      unsigned int modesize;
      const char *str;
      HOST_WIDE_INT i;
      int j, unit;
      const char *prefix = targetm.asm_out.mergeable_rodata_prefix;
      char *name = (char *) alloca (strlen (prefix) + 30);

      mode = TYPE_MODE (TREE_TYPE (TREE_TYPE (decl)));
      modesize = GET_MODE_BITSIZE (mode);
      if (modesize >= 8 && modesize <= 256
	  && (modesize & (modesize - 1)) == 0)
	{
	  if (align < modesize)
	    align = modesize;

	  str = TREE_STRING_POINTER (decl);
	  unit = GET_MODE_SIZE (mode);

	  /* Check for embedded NUL characters.  */
	  for (i = 0; i < len; i += unit)
	    {
	      for (j = 0; j < unit; j++)
		if (str[i + j] != '\0')
		  break;
	      if (j == unit)
		break;
	    }
	  if (i == len - unit)
	    {
	      sprintf (name, "%s.str%d.%d", prefix,
		       modesize / 8, (int) (align / 8));
	      flags |= (modesize / 8) | SECTION_MERGE | SECTION_STRINGS;
	      return get_section (name, flags, NULL);
	    }
	}
    }

  return readonly_data_section;
}

/* Return the section to use for constant merging.  */

section *
mergeable_constant_section (enum machine_mode mode ATTRIBUTE_UNUSED,
			    unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED,
			    unsigned int flags ATTRIBUTE_UNUSED)
{
  unsigned int modesize = GET_MODE_BITSIZE (mode);

  if (HAVE_GAS_SHF_MERGE && flag_merge_constants
      && mode != VOIDmode
      && mode != BLKmode
      && modesize <= align
      && align >= 8
      && align <= 256
      && (align & (align - 1)) == 0)
    {
      const char *prefix = targetm.asm_out.mergeable_rodata_prefix;
      char *name = (char *) alloca (strlen (prefix) + 30);

      sprintf (name, "%s.cst%d", prefix, (int) (align / 8));
      flags |= (align / 8) | SECTION_MERGE;
      return get_section (name, flags, NULL);
    }
  return readonly_data_section;
}

/* Given NAME, a putative register name, discard any customary prefixes.  */

static const char *
strip_reg_name (const char *name)
{
#ifdef REGISTER_PREFIX
  if (!strncmp (name, REGISTER_PREFIX, strlen (REGISTER_PREFIX)))
    name += strlen (REGISTER_PREFIX);
#endif
  if (name[0] == '%' || name[0] == '#')
    name++;
  return name;
}

/* The user has asked for a DECL to have a particular name.  Set (or
   change) it in such a way that we don't prefix an underscore to
   it.  */
void
set_user_assembler_name (tree decl, const char *name)
{
  char *starred = (char *) alloca (strlen (name) + 2);
  starred[0] = '*';
  strcpy (starred + 1, name);
  change_decl_assembler_name (decl, get_identifier (starred));
  SET_DECL_RTL (decl, NULL_RTX);
}

/* Decode an `asm' spec for a declaration as a register name.
   Return the register number, or -1 if nothing specified,
   or -2 if the ASMSPEC is not `cc' or `memory' and is not recognized,
   or -3 if ASMSPEC is `cc' and is not recognized,
   or -4 if ASMSPEC is `memory' and is not recognized.
   Accept an exact spelling or a decimal number.
   Prefixes such as % are optional.  */

int
decode_reg_name_and_count (const char *asmspec, int *pnregs)
{
  /* Presume just one register is clobbered.  */
  *pnregs = 1;

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

#ifdef OVERLAPPING_REGISTER_NAMES
      {
	static const struct
	{
	  const char *const name;
	  const int number;
	  const int nregs;
	} table[] = OVERLAPPING_REGISTER_NAMES;

	for (i = 0; i < (int) ARRAY_SIZE (table); i++)
	  if (table[i].name[0]
	      && ! strcmp (asmspec, table[i].name))
	    {
	      *pnregs = table[i].nregs;
	      return table[i].number;
	    }
      }
#endif /* OVERLAPPING_REGISTER_NAMES */

#ifdef ADDITIONAL_REGISTER_NAMES
      {
	static const struct { const char *const name; const int number; } table[]
	  = ADDITIONAL_REGISTER_NAMES;

	for (i = 0; i < (int) ARRAY_SIZE (table); i++)
	  if (table[i].name[0]
	      && ! strcmp (asmspec, table[i].name))
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

int
decode_reg_name (const char *name)
{
  int count;
  return decode_reg_name_and_count (name, &count);
}


/* Return true if DECL's initializer is suitable for a BSS section.  */

bool
bss_initializer_p (const_tree decl)
{
  return (DECL_INITIAL (decl) == NULL
	  || DECL_INITIAL (decl) == error_mark_node
	  || (flag_zero_initialized_in_bss
	      /* Leave constant zeroes in .rodata so they
		 can be shared.  */
	      && !TREE_READONLY (decl)
	      && initializer_zerop (DECL_INITIAL (decl))));
}

/* Compute the alignment of variable specified by DECL.
   DONT_OUTPUT_DATA is from assemble_variable.  */

void
align_variable (tree decl, bool dont_output_data)
{
  unsigned int align = DECL_ALIGN (decl);

  /* In the case for initialing an array whose length isn't specified,
     where we have not yet been able to do the layout,
     figure out the proper alignment now.  */
  if (dont_output_data && DECL_SIZE (decl) == 0
      && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
    align = MAX (align, TYPE_ALIGN (TREE_TYPE (TREE_TYPE (decl))));

  /* Some object file formats have a maximum alignment which they support.
     In particular, a.out format supports a maximum alignment of 4.  */
  if (align > MAX_OFILE_ALIGNMENT)
    {
      error ("alignment of %q+D is greater than maximum object "
	     "file alignment %d", decl,
	     MAX_OFILE_ALIGNMENT/BITS_PER_UNIT);
      align = MAX_OFILE_ALIGNMENT;
    }

  if (! DECL_USER_ALIGN (decl))
    {
#ifdef DATA_ABI_ALIGNMENT
      unsigned int data_abi_align
	= DATA_ABI_ALIGNMENT (TREE_TYPE (decl), align);
      /* For backwards compatibility, don't assume the ABI alignment for
	 TLS variables.  */
      if (! DECL_THREAD_LOCAL_P (decl) || data_abi_align <= BITS_PER_WORD)
	align = data_abi_align;
#endif

      /* On some machines, it is good to increase alignment sometimes.
	 But as DECL_ALIGN is used both for actually emitting the variable
	 and for code accessing the variable as guaranteed alignment, we
	 can only increase the alignment if it is a performance optimization
	 if the references to it must bind to the current definition.  */
      if (decl_binds_to_current_def_p (decl))
	{
#ifdef DATA_ALIGNMENT
	  unsigned int data_align = DATA_ALIGNMENT (TREE_TYPE (decl), align);
	  /* Don't increase alignment too much for TLS variables - TLS space
	     is too precious.  */
	  if (! DECL_THREAD_LOCAL_P (decl) || data_align <= BITS_PER_WORD)
	    align = data_align;
#endif
#ifdef CONSTANT_ALIGNMENT
	  if (DECL_INITIAL (decl) != 0
	      && DECL_INITIAL (decl) != error_mark_node)
	    {
	      unsigned int const_align
		= CONSTANT_ALIGNMENT (DECL_INITIAL (decl), align);
	      /* Don't increase alignment too much for TLS variables - TLS
		 space is too precious.  */
	      if (! DECL_THREAD_LOCAL_P (decl) || const_align <= BITS_PER_WORD)
		align = const_align;
	    }
#endif
	}
    }

  /* Reset the alignment in case we have made it tighter, so we can benefit
     from it in get_pointer_alignment.  */
  DECL_ALIGN (decl) = align;
}

/* Return DECL_ALIGN (decl), possibly increased for optimization purposes
   beyond what align_variable returned.  */

static unsigned int
get_variable_align (tree decl)
{
  unsigned int align = DECL_ALIGN (decl);

  /* For user aligned vars or static vars align_variable already did
     everything.  */
  if (DECL_USER_ALIGN (decl) || !TREE_PUBLIC (decl))
    return align;

#ifdef DATA_ABI_ALIGNMENT
  if (DECL_THREAD_LOCAL_P (decl))
    align = DATA_ABI_ALIGNMENT (TREE_TYPE (decl), align);
#endif

  /* For decls that bind to the current definition, align_variable
     did also everything, except for not assuming ABI required alignment
     of TLS variables.  For other vars, increase the alignment here
     as an optimization.  */
  if (!decl_binds_to_current_def_p (decl))
    {
      /* On some machines, it is good to increase alignment sometimes.  */
#ifdef DATA_ALIGNMENT
      unsigned int data_align = DATA_ALIGNMENT (TREE_TYPE (decl), align);
      /* Don't increase alignment too much for TLS variables - TLS space
         is too precious.  */
      if (! DECL_THREAD_LOCAL_P (decl) || data_align <= BITS_PER_WORD)
	align = data_align;
#endif
#ifdef CONSTANT_ALIGNMENT
      if (DECL_INITIAL (decl) != 0 && DECL_INITIAL (decl) != error_mark_node)
	{
	  unsigned int const_align = CONSTANT_ALIGNMENT (DECL_INITIAL (decl),
							 align);
	  /* Don't increase alignment too much for TLS variables - TLS space
	     is too precious.  */
	  if (! DECL_THREAD_LOCAL_P (decl) || const_align <= BITS_PER_WORD)
	    align = const_align;
	}
#endif
    }

  return align;
}

/* Return the section into which the given VAR_DECL or CONST_DECL
   should be placed.  PREFER_NOSWITCH_P is true if a noswitch
   section should be used wherever possible.  */

section *
get_variable_section (tree decl, bool prefer_noswitch_p)
{
  addr_space_t as = ADDR_SPACE_GENERIC;
  int reloc;

  if (TREE_TYPE (decl) != error_mark_node)
    as = TYPE_ADDR_SPACE (TREE_TYPE (decl));

  if (DECL_COMMON (decl))
    {
      /* If the decl has been given an explicit section name, or it resides
	 in a non-generic address space, then it isn't common, and shouldn't
	 be handled as such.  */
      gcc_assert (DECL_SECTION_NAME (decl) == NULL
		  && ADDR_SPACE_GENERIC_P (as));
      if (DECL_THREAD_LOCAL_P (decl))
	return tls_comm_section;
      else if (TREE_PUBLIC (decl) && bss_initializer_p (decl))
	return comm_section;
    }

  if (DECL_INITIAL (decl) == error_mark_node)
    reloc = contains_pointers_p (TREE_TYPE (decl)) ? 3 : 0;
  else if (DECL_INITIAL (decl))
    reloc = compute_reloc_for_constant (DECL_INITIAL (decl));
  else
    reloc = 0;

  resolve_unique_section (decl, reloc, flag_data_sections);
  if (IN_NAMED_SECTION (decl))
    return get_named_section (decl, NULL, reloc);

  if (ADDR_SPACE_GENERIC_P (as)
      && !DECL_THREAD_LOCAL_P (decl)
      && !(prefer_noswitch_p && targetm.have_switchable_bss_sections)
      && bss_initializer_p (decl))
    {
      if (!TREE_PUBLIC (decl)
	  && !((flag_sanitize & SANITIZE_ADDRESS)
	       && asan_protect_global (decl)))
	return lcomm_section;
      if (bss_noswitch_section)
	return bss_noswitch_section;
    }

  return targetm.asm_out.select_section (decl, reloc,
					 get_variable_align (decl));
}

/* Return the block into which object_block DECL should be placed.  */

static struct object_block *
get_block_for_decl (tree decl)
{
  section *sect;

  if (TREE_CODE (decl) == VAR_DECL)
    {
      /* The object must be defined in this translation unit.  */
      if (DECL_EXTERNAL (decl))
	return NULL;

      /* There's no point using object blocks for something that is
	 isolated by definition.  */
      if (DECL_ONE_ONLY (decl))
	return NULL;
    }

  /* We can only calculate block offsets if the decl has a known
     constant size.  */
  if (DECL_SIZE_UNIT (decl) == NULL)
    return NULL;
  if (!tree_fits_uhwi_p (DECL_SIZE_UNIT (decl)))
    return NULL;

  /* Find out which section should contain DECL.  We cannot put it into
     an object block if it requires a standalone definition.  */
  if (TREE_CODE (decl) == VAR_DECL)
      align_variable (decl, 0);
  sect = get_variable_section (decl, true);
  if (SECTION_STYLE (sect) == SECTION_NOSWITCH)
    return NULL;

  return get_block_for_section (sect);
}

/* Make sure block symbol SYMBOL is in block BLOCK.  */

static void
change_symbol_block (rtx symbol, struct object_block *block)
{
  if (block != SYMBOL_REF_BLOCK (symbol))
    {
      gcc_assert (SYMBOL_REF_BLOCK_OFFSET (symbol) < 0);
      SYMBOL_REF_BLOCK (symbol) = block;
    }
}

/* Return true if it is possible to put DECL in an object_block.  */

static bool
use_blocks_for_decl_p (tree decl)
{
  /* Only data DECLs can be placed into object blocks.  */
  if (TREE_CODE (decl) != VAR_DECL && TREE_CODE (decl) != CONST_DECL)
    return false;

  /* Detect decls created by dw2_force_const_mem.  Such decls are
     special because DECL_INITIAL doesn't specify the decl's true value.
     dw2_output_indirect_constants will instead call assemble_variable
     with dont_output_data set to 1 and then print the contents itself.  */
  if (DECL_INITIAL (decl) == decl)
    return false;

  /* If this decl is an alias, then we don't want to emit a
     definition.  */
  if (lookup_attribute ("alias", DECL_ATTRIBUTES (decl)))
    return false;

  return targetm.use_blocks_for_decl_p (decl);
}

/* Create the DECL_RTL for a VAR_DECL or FUNCTION_DECL.  DECL should
   have static storage duration.  In other words, it should not be an
   automatic variable, including PARM_DECLs.

   There is, however, one exception: this function handles variables
   explicitly placed in a particular register by the user.

   This is never called for PARM_DECL nodes.  */

void
make_decl_rtl (tree decl)
{
  const char *name = 0;
  int reg_number;
  rtx x;

  /* Check that we are not being given an automatic variable.  */
  gcc_assert (TREE_CODE (decl) != PARM_DECL
	      && TREE_CODE (decl) != RESULT_DECL);

  /* A weak alias has TREE_PUBLIC set but not the other bits.  */
  gcc_assert (TREE_CODE (decl) != VAR_DECL
	      || TREE_STATIC (decl)
	      || TREE_PUBLIC (decl)
	      || DECL_EXTERNAL (decl)
	      || DECL_REGISTER (decl));

  /* And that we were not given a type or a label.  */
  gcc_assert (TREE_CODE (decl) != TYPE_DECL
	      && TREE_CODE (decl) != LABEL_DECL);

  /* For a duplicate declaration, we can be called twice on the
     same DECL node.  Don't discard the RTL already made.  */
  if (DECL_RTL_SET_P (decl))
    {
      /* If the old RTL had the wrong mode, fix the mode.  */
      x = DECL_RTL (decl);
      if (GET_MODE (x) != DECL_MODE (decl))
	SET_DECL_RTL (decl, adjust_address_nv (x, DECL_MODE (decl), 0));

      if (TREE_CODE (decl) != FUNCTION_DECL && DECL_REGISTER (decl))
	return;

      /* ??? Another way to do this would be to maintain a hashed
	 table of such critters.  Instead of adding stuff to a DECL
	 to give certain attributes to it, we could use an external
	 hash map from DECL to set of attributes.  */

      /* Let the target reassign the RTL if it wants.
	 This is necessary, for example, when one machine specific
	 decl attribute overrides another.  */
      targetm.encode_section_info (decl, DECL_RTL (decl), false);

      /* If the symbol has a SYMBOL_REF_BLOCK field, update it based
	 on the new decl information.  */
      if (MEM_P (x)
	  && GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	  && SYMBOL_REF_HAS_BLOCK_INFO_P (XEXP (x, 0)))
	change_symbol_block (XEXP (x, 0), get_block_for_decl (decl));

      return;
    }

  /* If this variable belongs to the global constant pool, retrieve the
     pre-computed RTL or recompute it in LTO mode.  */
  if (TREE_CODE (decl) == VAR_DECL && DECL_IN_CONSTANT_POOL (decl))
    {
      SET_DECL_RTL (decl, output_constant_def (DECL_INITIAL (decl), 1));
      return;
    }

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  if (name[0] != '*' && TREE_CODE (decl) != FUNCTION_DECL
      && DECL_REGISTER (decl))
    {
      error ("register name not specified for %q+D", decl);
    }
  else if (TREE_CODE (decl) != FUNCTION_DECL && DECL_REGISTER (decl))
    {
      const char *asmspec = name+1;
      enum machine_mode mode = DECL_MODE (decl);
      reg_number = decode_reg_name (asmspec);
      /* First detect errors in declaring global registers.  */
      if (reg_number == -1)
	error ("register name not specified for %q+D", decl);
      else if (reg_number < 0)
	error ("invalid register name for %q+D", decl);
      else if (mode == BLKmode)
	error ("data type of %q+D isn%'t suitable for a register",
	       decl);
      else if (!in_hard_reg_set_p (accessible_reg_set, mode, reg_number))
	error ("the register specified for %q+D cannot be accessed"
	       " by the current target", decl);
      else if (!in_hard_reg_set_p (operand_reg_set, mode, reg_number))
	error ("the register specified for %q+D is not general enough"
	       " to be used as a register variable", decl);
      else if (!HARD_REGNO_MODE_OK (reg_number, mode))
	error ("register specified for %q+D isn%'t suitable for data type",
               decl);
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
	    warning (OPT_Wvolatile_register_var,
		     "optimization may eliminate reads and/or "
		     "writes to register variables");

	  /* If the user specified one of the eliminables registers here,
	     e.g., FRAME_POINTER_REGNUM, we don't want to get this variable
	     confused with that register and be eliminated.  This usage is
	     somewhat suspect...  */

	  SET_DECL_RTL (decl, gen_rtx_raw_REG (mode, reg_number));
	  ORIGINAL_REGNO (DECL_RTL (decl)) = reg_number;
	  REG_USERVAR_P (DECL_RTL (decl)) = 1;

	  if (TREE_STATIC (decl))
	    {
	      /* Make this register global, so not usable for anything
		 else.  */
#ifdef ASM_DECLARE_REGISTER_GLOBAL
	      name = IDENTIFIER_POINTER (DECL_NAME (decl));
	      ASM_DECLARE_REGISTER_GLOBAL (asm_out_file, decl, reg_number, name);
#endif
	      nregs = hard_regno_nregs[reg_number][mode];
	      while (nregs > 0)
		globalize_reg (decl, reg_number + --nregs);
	    }

	  /* As a register variable, it has no section.  */
	  return;
	}
    }
  /* Now handle ordinary static variables and functions (in memory).
     Also handle vars declared register invalidly.  */
  else if (name[0] == '*')
  {
#ifdef REGISTER_PREFIX
    if (strlen (REGISTER_PREFIX) != 0)
      {
	reg_number = decode_reg_name (name);
	if (reg_number >= 0 || reg_number == -3)
	  error ("register name given for non-register variable %q+D", decl);
      }
#endif
  }

  /* Specifying a section attribute on a variable forces it into a
     non-.bss section, and thus it cannot be common.  */
  /* FIXME: In general this code should not be necessary because
     visibility pass is doing the same work.  But notice_global_symbol
     is called early and it needs to make DECL_RTL to get the name.
     we take care of recomputing the DECL_RTL after visibility is changed.  */
  if (TREE_CODE (decl) == VAR_DECL
      && DECL_SECTION_NAME (decl) != NULL_TREE
      && DECL_INITIAL (decl) == NULL_TREE
      && DECL_COMMON (decl))
    DECL_COMMON (decl) = 0;

  /* Variables can't be both common and weak.  */
  if (TREE_CODE (decl) == VAR_DECL && DECL_WEAK (decl))
    DECL_COMMON (decl) = 0;

  if (use_object_blocks_p () && use_blocks_for_decl_p (decl))
    x = create_block_symbol (name, get_block_for_decl (decl), -1);
  else
    {
      enum machine_mode address_mode = Pmode;
      if (TREE_TYPE (decl) != error_mark_node)
	{
	  addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (decl));
	  address_mode = targetm.addr_space.address_mode (as);
	}
      x = gen_rtx_SYMBOL_REF (address_mode, name);
    }
  SYMBOL_REF_WEAK (x) = DECL_WEAK (decl);
  SET_SYMBOL_REF_DECL (x, decl);

  x = gen_rtx_MEM (DECL_MODE (decl), x);
  if (TREE_CODE (decl) != FUNCTION_DECL)
    set_mem_attributes (x, decl, 1);
  SET_DECL_RTL (decl, x);

  /* Optionally set flags or add text to the name to record information
     such as that it is a function name.
     If the name is changed, the macro ASM_OUTPUT_LABELREF
     will have to know how to strip this information.  */
  targetm.encode_section_info (decl, DECL_RTL (decl), true);
}

/* Like make_decl_rtl, but inhibit creation of new alias sets when
   calling make_decl_rtl.  Also, reset DECL_RTL before returning the
   rtl.  */

rtx
make_decl_rtl_for_debug (tree decl)
{
  unsigned int save_aliasing_flag;
  rtx rtl;

  if (DECL_RTL_SET_P (decl))
    return DECL_RTL (decl);

  /* Kludge alert!  Somewhere down the call chain, make_decl_rtl will
     call new_alias_set.  If running with -fcompare-debug, sometimes
     we do not want to create alias sets that will throw the alias
     numbers off in the comparison dumps.  So... clearing
     flag_strict_aliasing will keep new_alias_set() from creating a
     new set.  */
  save_aliasing_flag = flag_strict_aliasing;
  flag_strict_aliasing = 0;

  rtl = DECL_RTL (decl);
  /* Reset DECL_RTL back, as various parts of the compiler expects
     DECL_RTL set meaning it is actually going to be output.  */
  SET_DECL_RTL (decl, NULL);

  flag_strict_aliasing = save_aliasing_flag;
  return rtl;
}

/* Output a string of literal assembler code
   for an `asm' keyword used between functions.  */

void
assemble_asm (tree string)
{
  const char *p;
  app_enable ();

  if (TREE_CODE (string) == ADDR_EXPR)
    string = TREE_OPERAND (string, 0);

  p = TREE_STRING_POINTER (string);
  fprintf (asm_out_file, "%s%s\n", p[0] == '\t' ? "" : "\t", p);
}

/* Write the address of the entity given by SYMBOL to SEC.  */
void
assemble_addr_to_section (rtx symbol, section *sec)
{
  switch_to_section (sec);
  assemble_align (POINTER_SIZE);
  assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
}

/* Return the numbered .ctors.N (if CONSTRUCTOR_P) or .dtors.N (if
   not) section for PRIORITY.  */
section *
get_cdtor_priority_section (int priority, bool constructor_p)
{
  char buf[16];

  /* ??? This only works reliably with the GNU linker.  */
  sprintf (buf, "%s.%.5u",
	   constructor_p ? ".ctors" : ".dtors",
	   /* Invert the numbering so the linker puts us in the proper
	      order; constructors are run from right to left, and the
	      linker sorts in increasing order.  */
	   MAX_INIT_PRIORITY - priority);
  return get_section (buf, SECTION_WRITE, NULL);
}

void
default_named_section_asm_out_destructor (rtx symbol, int priority)
{
  section *sec;

  if (priority != DEFAULT_INIT_PRIORITY)
    sec = get_cdtor_priority_section (priority,
				      /*constructor_p=*/false);
  else
    sec = get_section (".dtors", SECTION_WRITE, NULL);

  assemble_addr_to_section (symbol, sec);
}

#ifdef DTORS_SECTION_ASM_OP
void
default_dtor_section_asm_out_destructor (rtx symbol,
					 int priority ATTRIBUTE_UNUSED)
{
  assemble_addr_to_section (symbol, dtors_section);
}
#endif

void
default_named_section_asm_out_constructor (rtx symbol, int priority)
{
  section *sec;

  if (priority != DEFAULT_INIT_PRIORITY)
    sec = get_cdtor_priority_section (priority,
				      /*constructor_p=*/true);
  else
    sec = get_section (".ctors", SECTION_WRITE, NULL);

  assemble_addr_to_section (symbol, sec);
}

#ifdef CTORS_SECTION_ASM_OP
void
default_ctor_section_asm_out_constructor (rtx symbol,
					  int priority ATTRIBUTE_UNUSED)
{
  assemble_addr_to_section (symbol, ctors_section);
}
#endif

/* CONSTANT_POOL_BEFORE_FUNCTION may be defined as an expression with
   a nonzero value if the constant pool should be output before the
   start of the function, or a zero value if the pool should output
   after the end of the function.  The default is to put it before the
   start.  */

#ifndef CONSTANT_POOL_BEFORE_FUNCTION
#define CONSTANT_POOL_BEFORE_FUNCTION 1
#endif

/* DECL is an object (either VAR_DECL or FUNCTION_DECL) which is going
   to be output to assembler.
   Set first_global_object_name and weak_global_object_name as appropriate.  */

void
notice_global_symbol (tree decl)
{
  const char **type = &first_global_object_name;

  if (first_global_object_name
      || !TREE_PUBLIC (decl)
      || DECL_EXTERNAL (decl)
      || !DECL_NAME (decl)
      || (TREE_CODE (decl) != FUNCTION_DECL
	  && (TREE_CODE (decl) != VAR_DECL
	      || (DECL_COMMON (decl)
		  && (DECL_INITIAL (decl) == 0
		      || DECL_INITIAL (decl) == error_mark_node))))
      || !MEM_P (DECL_RTL (decl)))
    return;

  /* We win when global object is found, but it is useful to know about weak
     symbol as well so we can produce nicer unique names.  */
  if (DECL_WEAK (decl) || DECL_ONE_ONLY (decl) || flag_shlib)
    type = &weak_global_object_name;

  if (!*type)
    {
      const char *p;
      const char *name;
      rtx decl_rtl = DECL_RTL (decl);

      p = targetm.strip_name_encoding (XSTR (XEXP (decl_rtl, 0), 0));
      name = ggc_strdup (p);

      *type = name;
    }
}

/* If not using flag_reorder_blocks_and_partition, decide early whether the
   current function goes into the cold section, so that targets can use
   current_function_section during RTL expansion.  DECL describes the
   function.  */

void
decide_function_section (tree decl)
{
  first_function_block_is_cold = false;

  if (flag_reorder_blocks_and_partition)
    /* We will decide in assemble_start_function.  */
    return;

 if (DECL_SECTION_NAME (decl))
    {
      struct cgraph_node *node = cgraph_get_node (current_function_decl);
      /* Calls to function_section rely on first_function_block_is_cold
	 being accurate.  */
      first_function_block_is_cold = (node
				      && node->frequency
				      == NODE_FREQUENCY_UNLIKELY_EXECUTED);
    }

  in_cold_section_p = first_function_block_is_cold;
}

/* Output assembler code for the constant pool of a function and associated
   with defining the name of the function.  DECL describes the function.
   NAME is the function's name.  For the constant pool, we use the current
   constant pool data.  */

void
assemble_start_function (tree decl, const char *fnname)
{
  int align;
  char tmp_label[100];
  bool hot_label_written = false;

  if (flag_reorder_blocks_and_partition)
    {
      ASM_GENERATE_INTERNAL_LABEL (tmp_label, "LHOTB", const_labelno);
      crtl->subsections.hot_section_label = ggc_strdup (tmp_label);
      ASM_GENERATE_INTERNAL_LABEL (tmp_label, "LCOLDB", const_labelno);
      crtl->subsections.cold_section_label = ggc_strdup (tmp_label);
      ASM_GENERATE_INTERNAL_LABEL (tmp_label, "LHOTE", const_labelno);
      crtl->subsections.hot_section_end_label = ggc_strdup (tmp_label);
      ASM_GENERATE_INTERNAL_LABEL (tmp_label, "LCOLDE", const_labelno);
      crtl->subsections.cold_section_end_label = ggc_strdup (tmp_label);
      const_labelno++;
    }
  else
    {
      crtl->subsections.hot_section_label = NULL;
      crtl->subsections.cold_section_label = NULL;
      crtl->subsections.hot_section_end_label = NULL;
      crtl->subsections.cold_section_end_label = NULL;
    }

  /* The following code does not need preprocessing in the assembler.  */

  app_disable ();

  if (CONSTANT_POOL_BEFORE_FUNCTION)
    output_constant_pool (fnname, decl);

  /* Make sure the not and cold text (code) sections are properly
     aligned.  This is necessary here in the case where the function
     has both hot and cold sections, because we don't want to re-set
     the alignment when the section switch happens mid-function.  */

  if (flag_reorder_blocks_and_partition)
    {
      first_function_block_is_cold = false;

      switch_to_section (unlikely_text_section ());
      assemble_align (DECL_ALIGN (decl));
      ASM_OUTPUT_LABEL (asm_out_file, crtl->subsections.cold_section_label);

      /* When the function starts with a cold section, we need to explicitly
	 align the hot section and write out the hot section label.
	 But if the current function is a thunk, we do not have a CFG.  */
      if (!cfun->is_thunk
	  && BB_PARTITION (ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb) == BB_COLD_PARTITION)
	{
	  switch_to_section (text_section);
	  assemble_align (DECL_ALIGN (decl));
	  ASM_OUTPUT_LABEL (asm_out_file, crtl->subsections.hot_section_label);
	  hot_label_written = true;
	  first_function_block_is_cold = true;
	}
      in_cold_section_p = first_function_block_is_cold;
    }


  /* Switch to the correct text section for the start of the function.  */

  switch_to_section (function_section (decl));
  if (flag_reorder_blocks_and_partition
      && !hot_label_written)
    ASM_OUTPUT_LABEL (asm_out_file, crtl->subsections.hot_section_label);

  /* Tell assembler to move to target machine's alignment for functions.  */
  align = floor_log2 (DECL_ALIGN (decl) / BITS_PER_UNIT);
  if (align > 0)
    {
      ASM_OUTPUT_ALIGN (asm_out_file, align);
    }

  /* Handle a user-specified function alignment.
     Note that we still need to align to DECL_ALIGN, as above,
     because ASM_OUTPUT_MAX_SKIP_ALIGN might not do any alignment at all.  */
  if (! DECL_USER_ALIGN (decl)
      && align_functions_log > align
      && optimize_function_for_speed_p (cfun))
    {
#ifdef ASM_OUTPUT_MAX_SKIP_ALIGN
      ASM_OUTPUT_MAX_SKIP_ALIGN (asm_out_file,
				 align_functions_log, align_functions - 1);
#else
      ASM_OUTPUT_ALIGN (asm_out_file, align_functions_log);
#endif
    }

#ifdef ASM_OUTPUT_FUNCTION_PREFIX
  ASM_OUTPUT_FUNCTION_PREFIX (asm_out_file, fnname);
#endif

  if (!DECL_IGNORED_P (decl))
    (*debug_hooks->begin_function) (decl);

  /* Make function name accessible from other files, if appropriate.  */

  if (TREE_PUBLIC (decl))
    {
      notice_global_symbol (decl);

      globalize_decl (decl);

      maybe_assemble_visibility (decl);
    }

  if (DECL_PRESERVE_P (decl))
    targetm.asm_out.mark_decl_preserved (fnname);

  /* Do any machine/system dependent processing of the function name.  */
#ifdef ASM_DECLARE_FUNCTION_NAME
  ASM_DECLARE_FUNCTION_NAME (asm_out_file, fnname, current_function_decl);
#else
  /* Standard thing is just output label for the function.  */
  ASM_OUTPUT_FUNCTION_LABEL (asm_out_file, fnname, current_function_decl);
#endif /* ASM_DECLARE_FUNCTION_NAME */

  if (lookup_attribute ("no_split_stack", DECL_ATTRIBUTES (decl)))
    saw_no_split_stack = true;
}

/* Output assembler code associated with defining the size of the
   function.  DECL describes the function.  NAME is the function's name.  */

void
assemble_end_function (tree decl, const char *fnname ATTRIBUTE_UNUSED)
{
#ifdef ASM_DECLARE_FUNCTION_SIZE
  /* We could have switched section in the middle of the function.  */
  if (flag_reorder_blocks_and_partition)
    switch_to_section (function_section (decl));
  ASM_DECLARE_FUNCTION_SIZE (asm_out_file, fnname, decl);
#endif
  if (! CONSTANT_POOL_BEFORE_FUNCTION)
    {
      output_constant_pool (fnname, decl);
      switch_to_section (function_section (decl)); /* need to switch back */
    }
  /* Output labels for end of hot/cold text sections (to be used by
     debug info.)  */
  if (flag_reorder_blocks_and_partition)
    {
      section *save_text_section;

      save_text_section = in_section;
      switch_to_section (unlikely_text_section ());
      ASM_OUTPUT_LABEL (asm_out_file, crtl->subsections.cold_section_end_label);
      if (first_function_block_is_cold)
	switch_to_section (text_section);
      else
	switch_to_section (function_section (decl));
      ASM_OUTPUT_LABEL (asm_out_file, crtl->subsections.hot_section_end_label);
      switch_to_section (save_text_section);
    }
}

/* Assemble code to leave SIZE bytes of zeros.  */

void
assemble_zeros (unsigned HOST_WIDE_INT size)
{
  /* Do no output if -fsyntax-only.  */
  if (flag_syntax_only)
    return;

#ifdef ASM_NO_SKIP_IN_TEXT
  /* The `space' pseudo in the text section outputs nop insns rather than 0s,
     so we must output 0s explicitly in the text section.  */
  if (ASM_NO_SKIP_IN_TEXT && (in_section->common.flags & SECTION_CODE) != 0)
    {
      unsigned HOST_WIDE_INT i;
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
assemble_align (int align)
{
  if (align > BITS_PER_UNIT)
    {
      ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (align / BITS_PER_UNIT));
    }
}

/* Assemble a string constant with the specified C string as contents.  */

void
assemble_string (const char *p, int size)
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


/* A noswitch_section_callback for lcomm_section.  */

static bool
emit_local (tree decl ATTRIBUTE_UNUSED,
	    const char *name ATTRIBUTE_UNUSED,
	    unsigned HOST_WIDE_INT size ATTRIBUTE_UNUSED,
	    unsigned HOST_WIDE_INT rounded ATTRIBUTE_UNUSED)
{
#if defined ASM_OUTPUT_ALIGNED_DECL_LOCAL
  ASM_OUTPUT_ALIGNED_DECL_LOCAL (asm_out_file, decl, name,
				 size, DECL_ALIGN (decl));
  return true;
#elif defined ASM_OUTPUT_ALIGNED_LOCAL
  ASM_OUTPUT_ALIGNED_LOCAL (asm_out_file, name, size, DECL_ALIGN (decl));
  return true;
#else
  ASM_OUTPUT_LOCAL (asm_out_file, name, size, rounded);
  return false;
#endif
}

/* A noswitch_section_callback for bss_noswitch_section.  */

#if defined ASM_OUTPUT_ALIGNED_BSS
static bool
emit_bss (tree decl ATTRIBUTE_UNUSED,
	  const char *name ATTRIBUTE_UNUSED,
	  unsigned HOST_WIDE_INT size ATTRIBUTE_UNUSED,
	  unsigned HOST_WIDE_INT rounded ATTRIBUTE_UNUSED)
{
#if defined ASM_OUTPUT_ALIGNED_BSS
  ASM_OUTPUT_ALIGNED_BSS (asm_out_file, decl, name, size,
			  get_variable_align (decl));
  return true;
#endif
}
#endif

/* A noswitch_section_callback for comm_section.  */

static bool
emit_common (tree decl ATTRIBUTE_UNUSED,
	     const char *name ATTRIBUTE_UNUSED,
	     unsigned HOST_WIDE_INT size ATTRIBUTE_UNUSED,
	     unsigned HOST_WIDE_INT rounded ATTRIBUTE_UNUSED)
{
#if defined ASM_OUTPUT_ALIGNED_DECL_COMMON
  ASM_OUTPUT_ALIGNED_DECL_COMMON (asm_out_file, decl, name,
				  size, get_variable_align (decl));
  return true;
#elif defined ASM_OUTPUT_ALIGNED_COMMON
  ASM_OUTPUT_ALIGNED_COMMON (asm_out_file, name, size,
			     get_variable_align (decl));
  return true;
#else
  ASM_OUTPUT_COMMON (asm_out_file, name, size, rounded);
  return false;
#endif
}

/* A noswitch_section_callback for tls_comm_section.  */

static bool
emit_tls_common (tree decl ATTRIBUTE_UNUSED,
		 const char *name ATTRIBUTE_UNUSED,
		 unsigned HOST_WIDE_INT size ATTRIBUTE_UNUSED,
		 unsigned HOST_WIDE_INT rounded ATTRIBUTE_UNUSED)
{
#ifdef ASM_OUTPUT_TLS_COMMON
  ASM_OUTPUT_TLS_COMMON (asm_out_file, decl, name, size);
  return true;
#else
  sorry ("thread-local COMMON data not implemented");
  return true;
#endif
}

/* Assemble DECL given that it belongs in SECTION_NOSWITCH section SECT.
   NAME is the name of DECL's SYMBOL_REF.  */

static void
assemble_noswitch_variable (tree decl, const char *name, section *sect,
			    unsigned int align)
{
  unsigned HOST_WIDE_INT size, rounded;

  size = tree_to_uhwi (DECL_SIZE_UNIT (decl));
  rounded = size;

  if ((flag_sanitize & SANITIZE_ADDRESS) && asan_protect_global (decl))
    size += asan_red_zone_size (size);

  /* Don't allocate zero bytes of common,
     since that means "undefined external" in the linker.  */
  if (size == 0)
    rounded = 1;

  /* Round size up to multiple of BIGGEST_ALIGNMENT bits
     so that each uninitialized object starts on such a boundary.  */
  rounded += (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1;
  rounded = (rounded / (BIGGEST_ALIGNMENT / BITS_PER_UNIT)
	     * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));

  if (!sect->noswitch.callback (decl, name, size, rounded)
      && (unsigned HOST_WIDE_INT) (align / BITS_PER_UNIT) > rounded)
    error ("requested alignment for %q+D is greater than "
	   "implemented alignment of %wu", decl, rounded);
}

/* A subroutine of assemble_variable.  Output the label and contents of
   DECL, whose address is a SYMBOL_REF with name NAME.  DONT_OUTPUT_DATA
   is as for assemble_variable.  */

static void
assemble_variable_contents (tree decl, const char *name,
			    bool dont_output_data)
{
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
      if (DECL_INITIAL (decl)
	  && DECL_INITIAL (decl) != error_mark_node
	  && !initializer_zerop (DECL_INITIAL (decl)))
	/* Output the actual data.  */
	output_constant (DECL_INITIAL (decl),
			 tree_to_uhwi (DECL_SIZE_UNIT (decl)),
			 get_variable_align (decl));
      else
	/* Leave space for it.  */
	assemble_zeros (tree_to_uhwi (DECL_SIZE_UNIT (decl)));
    }
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
assemble_variable (tree decl, int top_level ATTRIBUTE_UNUSED,
		   int at_end ATTRIBUTE_UNUSED, int dont_output_data)
{
  const char *name;
  rtx decl_rtl, symbol;
  section *sect;
  unsigned int align;
  bool asan_protected = false;

  /* This function is supposed to handle VARIABLES.  Ensure we have one.  */
  gcc_assert (TREE_CODE (decl) == VAR_DECL);

  /* Emulated TLS had better not get this far.  */
  gcc_checking_assert (targetm.have_tls || !DECL_THREAD_LOCAL_P (decl));

  last_assemble_variable_decl = 0;

  /* Normally no need to say anything here for external references,
     since assemble_external is called by the language-specific code
     when a declaration is first seen.  */

  if (DECL_EXTERNAL (decl))
    return;

  /* Do nothing for global register variables.  */
  if (DECL_RTL_SET_P (decl) && REG_P (DECL_RTL (decl)))
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
      error ("storage size of %q+D isn%'t known", decl);
      TREE_ASM_WRITTEN (decl) = 1;
      return;
    }

  /* The first declaration of a variable that comes through this function
     decides whether it is global (in C, has external linkage)
     or local (in C, has internal linkage).  So do nothing more
     if this function has already run.  */

  if (TREE_ASM_WRITTEN (decl))
    return;

  /* Make sure targetm.encode_section_info is invoked before we set
     ASM_WRITTEN.  */
  decl_rtl = DECL_RTL (decl);

  TREE_ASM_WRITTEN (decl) = 1;

  /* Do no output if -fsyntax-only.  */
  if (flag_syntax_only)
    return;

  if (! dont_output_data
      && ! valid_constant_size_p (DECL_SIZE_UNIT (decl)))
    {
      error ("size of variable %q+D is too large", decl);
      return;
    }

  gcc_assert (MEM_P (decl_rtl));
  gcc_assert (GET_CODE (XEXP (decl_rtl, 0)) == SYMBOL_REF);
  symbol = XEXP (decl_rtl, 0);

  /* If this symbol belongs to the tree constant pool, output the constant
     if it hasn't already been written.  */
  if (TREE_CONSTANT_POOL_ADDRESS_P (symbol))
    {
      tree decl = SYMBOL_REF_DECL (symbol);
      if (!TREE_ASM_WRITTEN (DECL_INITIAL (decl)))
	output_constant_def_contents (symbol);
      return;
    }

  app_disable ();

  name = XSTR (symbol, 0);
  if (TREE_PUBLIC (decl) && DECL_NAME (decl))
    notice_global_symbol (decl);

  /* Compute the alignment of this data.  */

  align_variable (decl, dont_output_data);

  if ((flag_sanitize & SANITIZE_ADDRESS)
      && asan_protect_global (decl))
    {
      asan_protected = true;
      DECL_ALIGN (decl) = MAX (DECL_ALIGN (decl), 
                               ASAN_RED_ZONE_SIZE * BITS_PER_UNIT);
    }

  set_mem_align (decl_rtl, DECL_ALIGN (decl));

  align = get_variable_align (decl);

  if (TREE_PUBLIC (decl))
    maybe_assemble_visibility (decl);

  if (DECL_PRESERVE_P (decl))
    targetm.asm_out.mark_decl_preserved (name);

  /* First make the assembler name(s) global if appropriate.  */
  sect = get_variable_section (decl, false);
  if (TREE_PUBLIC (decl)
      && (sect->common.flags & SECTION_COMMON) == 0)
    globalize_decl (decl);

  /* Output any data that we will need to use the address of.  */
  if (DECL_INITIAL (decl) && DECL_INITIAL (decl) != error_mark_node)
    output_addressed_constants (DECL_INITIAL (decl));

  /* dbxout.c needs to know this.  */
  if (sect && (sect->common.flags & SECTION_CODE) != 0)
    DECL_IN_TEXT_SECTION (decl) = 1;

  /* If the decl is part of an object_block, make sure that the decl
     has been positioned within its block, but do not write out its
     definition yet.  output_object_blocks will do that later.  */
  if (SYMBOL_REF_HAS_BLOCK_INFO_P (symbol) && SYMBOL_REF_BLOCK (symbol))
    {
      gcc_assert (!dont_output_data);
      place_block_symbol (symbol);
    }
  else if (SECTION_STYLE (sect) == SECTION_NOSWITCH)
    assemble_noswitch_variable (decl, name, sect, align);
  else
    {
      /* The following bit of code ensures that vtable_map 
         variables are not only in the comdat section, but that
         each variable has its own unique comdat name.  If this
         code is removed, the variables end up in the same section
         with a single comdat name.

         FIXME:  resolve_unique_section needs to deal better with
         decls with both DECL_SECTION_NAME and DECL_ONE_ONLY.  Once
         that is fixed, this if-else statement can be replaced with
         a single call to "switch_to_section (sect)".  */
      if (sect->named.name
	  && (strcmp (sect->named.name, ".vtable_map_vars") == 0))
	{
#if defined (OBJECT_FORMAT_ELF)
          targetm.asm_out.named_section (sect->named.name,
					 sect->named.common.flags
				         | SECTION_LINKONCE,
			    	         DECL_NAME (decl));
          in_section = sect;
#else
          switch_to_section (sect);
#endif
        }
      else
	switch_to_section (sect);
      if (align > BITS_PER_UNIT)
	ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (align / BITS_PER_UNIT));
      assemble_variable_contents (decl, name, dont_output_data);
      if (asan_protected)
	{
	  unsigned HOST_WIDE_INT int size
	    = tree_to_uhwi (DECL_SIZE_UNIT (decl));
	  assemble_zeros (asan_red_zone_size (size));
	}
    }
}


/* Given a function declaration (FN_DECL), this function assembles the
   function into the .preinit_array section.  */

void
assemble_vtv_preinit_initializer (tree fn_decl)
{
  section *sect;
  unsigned flags = SECTION_WRITE;
  rtx symbol = XEXP (DECL_RTL (fn_decl), 0);

  flags |= SECTION_NOTYPE;
  sect = get_section (".preinit_array", flags, fn_decl);
  switch_to_section (sect);
  assemble_addr_to_section (symbol, sect);
}

/* Return 1 if type TYPE contains any pointers.  */

static int
contains_pointers_p (tree type)
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
	for (fields = TYPE_FIELDS (type); fields; fields = DECL_CHAIN (fields))
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

/* We delay assemble_external processing until
   the compilation unit is finalized.  This is the best we can do for
   right now (i.e. stage 3 of GCC 4.0) - the right thing is to delay
   it all the way to final.  See PR 17982 for further discussion.  */
static GTY(()) tree pending_assemble_externals;

#ifdef ASM_OUTPUT_EXTERNAL
/* Some targets delay some output to final using TARGET_ASM_FILE_END.
   As a result, assemble_external can be called after the list of externals
   is processed and the pointer set destroyed.  */
static bool pending_assemble_externals_processed;

/* Avoid O(external_decls**2) lookups in the pending_assemble_externals
   TREE_LIST in assemble_external.  */
static struct pointer_set_t *pending_assemble_externals_set;

/* True if DECL is a function decl for which no out-of-line copy exists.
   It is assumed that DECL's assembler name has been set.  */

static bool
incorporeal_function_p (tree decl)
{
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_BUILT_IN (decl))
    {
      const char *name;

      if (DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL
	  && (DECL_FUNCTION_CODE (decl) == BUILT_IN_ALLOCA
	      || DECL_FUNCTION_CODE (decl) == BUILT_IN_ALLOCA_WITH_ALIGN))
	return true;

      name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
      /* Atomic or sync builtins which have survived this far will be
	 resolved externally and therefore are not incorporeal.  */
      if (strncmp (name, "__builtin_", 10) == 0)
	return true;
    }
  return false;
}

/* Actually do the tests to determine if this is necessary, and invoke
   ASM_OUTPUT_EXTERNAL.  */
static void
assemble_external_real (tree decl)
{
  rtx rtl = DECL_RTL (decl);

  if (MEM_P (rtl) && GET_CODE (XEXP (rtl, 0)) == SYMBOL_REF
      && !SYMBOL_REF_USED (XEXP (rtl, 0))
      && !incorporeal_function_p (decl))
    {
      /* Some systems do require some output.  */
      SYMBOL_REF_USED (XEXP (rtl, 0)) = 1;
      ASM_OUTPUT_EXTERNAL (asm_out_file, decl, XSTR (XEXP (rtl, 0), 0));
    }
}
#endif

void
process_pending_assemble_externals (void)
{
#ifdef ASM_OUTPUT_EXTERNAL
  tree list;
  for (list = pending_assemble_externals; list; list = TREE_CHAIN (list))
    assemble_external_real (TREE_VALUE (list));

  pending_assemble_externals = 0;
  pending_assemble_externals_processed = true;
  pointer_set_destroy (pending_assemble_externals_set);
#endif
}

/* This TREE_LIST contains any weak symbol declarations waiting
   to be emitted.  */
static GTY(()) tree weak_decls;

/* Output something to declare an external symbol to the assembler,
   and qualifiers such as weakness.  (Most assemblers don't need
   extern declaration, so we normally output nothing.)  Do nothing if
   DECL is not external.  */

void
assemble_external (tree decl ATTRIBUTE_UNUSED)
{
  /*  Make sure that the ASM_OUT_FILE is open.
      If it's not, we should not be calling this function.  */
  gcc_assert (asm_out_file);

  /* In a perfect world, the following condition would be true.
     Sadly, the Java and Go front ends emit assembly *from the front end*,
     bypassing the call graph.  See PR52739.  Fix before GCC 4.8.  */
#if 0
  /* This function should only be called if we are expanding, or have
     expanded, to RTL.
     Ideally, only final.c would be calling this function, but it is
     not clear whether that would break things somehow.  See PR 17982
     for further discussion.  */
  gcc_assert (cgraph_state == CGRAPH_STATE_EXPANSION
	      || cgraph_state == CGRAPH_STATE_FINISHED);
#endif

  if (!DECL_P (decl) || !DECL_EXTERNAL (decl) || !TREE_PUBLIC (decl))
    return;

  /* We want to output annotation for weak and external symbols at
     very last to check if they are references or not.  */

  if (TARGET_SUPPORTS_WEAK
      && DECL_WEAK (decl)
      /* TREE_STATIC is a weird and abused creature which is not
	 generally the right test for whether an entity has been
	 locally emitted, inlined or otherwise not-really-extern, but
	 for declarations that can be weak, it happens to be
	 match.  */
      && !TREE_STATIC (decl)
      && lookup_attribute ("weak", DECL_ATTRIBUTES (decl))
      && value_member (decl, weak_decls) == NULL_TREE)
    weak_decls = tree_cons (NULL, decl, weak_decls);

#ifdef ASM_OUTPUT_EXTERNAL
  if (pending_assemble_externals_processed)
    {
      assemble_external_real (decl);
      return;
    }

  if (! pointer_set_insert (pending_assemble_externals_set, decl))
    pending_assemble_externals = tree_cons (NULL, decl,
					    pending_assemble_externals);
#endif
}

/* Similar, for calling a library function FUN.  */

void
assemble_external_libcall (rtx fun)
{
  /* Declare library function name external when first used, if nec.  */
  if (! SYMBOL_REF_USED (fun))
    {
      SYMBOL_REF_USED (fun) = 1;
      targetm.asm_out.external_libcall (fun);
    }
}

/* Assemble a label named NAME.  */

void
assemble_label (FILE *file, const char *name)
{
  ASM_OUTPUT_LABEL (file, name);
}

/* Set the symbol_referenced flag for ID.  */
void
mark_referenced (tree id)
{
  TREE_SYMBOL_REFERENCED (id) = 1;
}

/* Set the symbol_referenced flag for DECL and notify callgraph.  */
void
mark_decl_referenced (tree decl)
{
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* Extern inline functions don't become needed when referenced.
	 If we know a method will be emitted in other TU and no new
	 functions can be marked reachable, just use the external
	 definition.  */
      struct cgraph_node *node = cgraph_get_create_node (decl);
      if (!DECL_EXTERNAL (decl)
	  && !node->definition)
	cgraph_mark_force_output_node (node);
    }
  else if (TREE_CODE (decl) == VAR_DECL)
    {
      varpool_node *node = varpool_node_for_decl (decl);
      /* C++ frontend use mark_decl_references to force COMDAT variables
         to be output that might appear dead otherwise.  */
      node->force_output = true;
    }
  /* else do nothing - we can get various sorts of CST nodes here,
     which do not need to be marked.  */
}


/* Follow the IDENTIFIER_TRANSPARENT_ALIAS chain starting at *ALIAS
   until we find an identifier that is not itself a transparent alias.
   Modify the alias passed to it by reference (and all aliases on the
   way to the ultimate target), such that they do not have to be
   followed again, and return the ultimate target of the alias
   chain.  */

static inline tree
ultimate_transparent_alias_target (tree *alias)
{
  tree target = *alias;

  if (IDENTIFIER_TRANSPARENT_ALIAS (target))
    {
      gcc_assert (TREE_CHAIN (target));
      target = ultimate_transparent_alias_target (&TREE_CHAIN (target));
      gcc_assert (! IDENTIFIER_TRANSPARENT_ALIAS (target)
		  && ! TREE_CHAIN (target));
      *alias = target;
    }

  return target;
}

/* Output to FILE (an assembly file) a reference to NAME.  If NAME
   starts with a *, the rest of NAME is output verbatim.  Otherwise
   NAME is transformed in a target-specific way (usually by the
   addition of an underscore).  */

void
assemble_name_raw (FILE *file, const char *name)
{
  if (name[0] == '*')
    fputs (&name[1], file);
  else
    ASM_OUTPUT_LABELREF (file, name);
}

/* Like assemble_name_raw, but should be used when NAME might refer to
   an entity that is also represented as a tree (like a function or
   variable).  If NAME does refer to such an entity, that entity will
   be marked as referenced.  */

void
assemble_name (FILE *file, const char *name)
{
  const char *real_name;
  tree id;

  real_name = targetm.strip_name_encoding (name);

  id = maybe_get_identifier (real_name);
  if (id)
    {
      tree id_orig = id;

      mark_referenced (id);
      ultimate_transparent_alias_target (&id);
      if (id != id_orig)
	name = IDENTIFIER_POINTER (id);
      gcc_assert (! TREE_CHAIN (id));
    }

  assemble_name_raw (file, name);
}

/* Allocate SIZE bytes writable static space with a gensym name
   and return an RTX to refer to its address.  */

rtx
assemble_static_space (unsigned HOST_WIDE_INT size)
{
  char name[12];
  const char *namestring;
  rtx x;

  ASM_GENERATE_INTERNAL_LABEL (name, "LF", const_labelno);
  ++const_labelno;
  namestring = ggc_strdup (name);

  x = gen_rtx_SYMBOL_REF (Pmode, namestring);
  SYMBOL_REF_FLAGS (x) = SYMBOL_FLAG_LOCAL;

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
    unsigned HOST_WIDE_INT rounded ATTRIBUTE_UNUSED
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

static GTY(()) rtx initial_trampoline;

rtx
assemble_trampoline_template (void)
{
  char label[256];
  const char *name;
  int align;
  rtx symbol;

  gcc_assert (targetm.asm_out.trampoline_template != NULL);

  if (initial_trampoline)
    return initial_trampoline;

  /* By default, put trampoline templates in read-only data section.  */

#ifdef TRAMPOLINE_SECTION
  switch_to_section (TRAMPOLINE_SECTION);
#else
  switch_to_section (readonly_data_section);
#endif

  /* Write the assembler code to define one.  */
  align = floor_log2 (TRAMPOLINE_ALIGNMENT / BITS_PER_UNIT);
  if (align > 0)
    ASM_OUTPUT_ALIGN (asm_out_file, align);

  targetm.asm_out.internal_label (asm_out_file, "LTRAMP", 0);
  targetm.asm_out.trampoline_template (asm_out_file);

  /* Record the rtl to refer to it.  */
  ASM_GENERATE_INTERNAL_LABEL (label, "LTRAMP", 0);
  name = ggc_strdup (label);
  symbol = gen_rtx_SYMBOL_REF (Pmode, name);
  SYMBOL_REF_FLAGS (symbol) = SYMBOL_FLAG_LOCAL;

  initial_trampoline = gen_const_mem (BLKmode, symbol);
  set_mem_align (initial_trampoline, TRAMPOLINE_ALIGNMENT);
  set_mem_size (initial_trampoline, TRAMPOLINE_SIZE);

  return initial_trampoline;
}

/* A and B are either alignments or offsets.  Return the minimum alignment
   that may be assumed after adding the two together.  */

static inline unsigned
min_align (unsigned int a, unsigned int b)
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
integer_asm_op (int size, int aligned_p)
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
assemble_integer_with_op (const char *op, rtx x)
{
  fputs (op, asm_out_file);
  output_addr_const (asm_out_file, x);
  fputc ('\n', asm_out_file);
}

/* The default implementation of the asm_out.integer target hook.  */

bool
default_assemble_integer (rtx x ATTRIBUTE_UNUSED,
			  unsigned int size ATTRIBUTE_UNUSED,
			  int aligned_p ATTRIBUTE_UNUSED)
{
  const char *op = integer_asm_op (size, aligned_p);
  /* Avoid GAS bugs for large values.  Specifically negative values whose
     absolute value fits in a bfd_vma, but not in a bfd_signed_vma.  */
  if (size > UNITS_PER_WORD && size > POINTER_SIZE / BITS_PER_UNIT)
    return false;
  return op && (assemble_integer_with_op (op, x), true);
}

/* Assemble the integer constant X into an object of SIZE bytes.  ALIGN is
   the alignment of the integer in bits.  Return 1 if we were able to output
   the constant, otherwise 0.  We must be able to output the constant,
   if FORCE is nonzero.  */

bool
assemble_integer (rtx x, unsigned int size, unsigned int align, int force)
{
  int aligned_p;

  aligned_p = (align >= MIN (size * BITS_PER_UNIT, BIGGEST_ALIGNMENT));

  /* See if the target hook can handle this kind of object.  */
  if (targetm.asm_out.integer (x, size, aligned_p))
    return true;

  /* If the object is a multi-byte one, try splitting it up.  Split
     it into words it if is multi-word, otherwise split it into bytes.  */
  if (size > 1)
    {
      enum machine_mode omode, imode;
      unsigned int subalign;
      unsigned int subsize, i;
      enum mode_class mclass;

      subsize = size > UNITS_PER_WORD? UNITS_PER_WORD : 1;
      subalign = MIN (align, subsize * BITS_PER_UNIT);
      if (GET_CODE (x) == CONST_FIXED)
	mclass = GET_MODE_CLASS (GET_MODE (x));
      else
	mclass = MODE_INT;

      omode = mode_for_size (subsize * BITS_PER_UNIT, mclass, 0);
      imode = mode_for_size (size * BITS_PER_UNIT, mclass, 0);

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
      gcc_assert (!i);
    }

  gcc_assert (!force);

  return false;
}

void
assemble_real (REAL_VALUE_TYPE d, enum machine_mode mode, unsigned int align)
{
  long data[4] = {0, 0, 0, 0};
  int i;
  int bitsize, nelts, nunits, units_per;

  /* This is hairy.  We have a quantity of known size.  real_to_target
     will put it into an array of *host* longs, 32 bits per element
     (even if long is more than 32 bits).  We need to determine the
     number of array elements that are occupied (nelts) and the number
     of *target* min-addressable units that will be occupied in the
     object file (nunits).  We cannot assume that 32 divides the
     mode's bitsize (size * BITS_PER_UNIT) evenly.

     size * BITS_PER_UNIT is used here to make sure that padding bits
     (which might appear at either end of the value; real_to_target
     will include the padding bits in its output array) are included.  */

  nunits = GET_MODE_SIZE (mode);
  bitsize = nunits * BITS_PER_UNIT;
  nelts = CEIL (bitsize, 32);
  units_per = 32 / BITS_PER_UNIT;

  real_to_target (data, &d, mode);

  /* Put out the first word with the specified alignment.  */
  assemble_integer (GEN_INT (data[0]), MIN (nunits, units_per), align, 1);
  nunits -= units_per;

  /* Subsequent words need only 32-bit alignment.  */
  align = min_align (align, 32);

  for (i = 1; i < nelts; i++)
    {
      assemble_integer (GEN_INT (data[i]), MIN (nunits, units_per), align, 1);
      nunits -= units_per;
    }
}

/* Given an expression EXP with a constant value,
   reduce it to the sum of an assembler symbol and an integer.
   Store them both in the structure *VALUE.
   EXP must be reducible.  */

struct addr_const {
  rtx base;
  HOST_WIDE_INT offset;
};

static void
decode_addr_const (tree exp, struct addr_const *value)
{
  tree target = TREE_OPERAND (exp, 0);
  int offset = 0;
  rtx x;

  while (1)
    {
      if (TREE_CODE (target) == COMPONENT_REF
	  && tree_fits_shwi_p (byte_position (TREE_OPERAND (target, 1))))
	{
	  offset += int_byte_position (TREE_OPERAND (target, 1));
	  target = TREE_OPERAND (target, 0);
	}
      else if (TREE_CODE (target) == ARRAY_REF
	       || TREE_CODE (target) == ARRAY_RANGE_REF)
	{
	  offset += (tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (target)))
		     * tree_to_shwi (TREE_OPERAND (target, 1)));
	  target = TREE_OPERAND (target, 0);
	}
      else if (TREE_CODE (target) == MEM_REF
	       && TREE_CODE (TREE_OPERAND (target, 0)) == ADDR_EXPR)
	{
	  offset += mem_ref_offset (target).low;
	  target = TREE_OPERAND (TREE_OPERAND (target, 0), 0);
	}
      else if (TREE_CODE (target) == INDIRECT_REF
	       && TREE_CODE (TREE_OPERAND (target, 0)) == NOP_EXPR
	       && TREE_CODE (TREE_OPERAND (TREE_OPERAND (target, 0), 0))
		  == ADDR_EXPR)
	target = TREE_OPERAND (TREE_OPERAND (TREE_OPERAND (target, 0), 0), 0);
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
		       gen_rtx_LABEL_REF (Pmode, force_label_rtx (target)));
      break;

    case REAL_CST:
    case FIXED_CST:
    case STRING_CST:
    case COMPLEX_CST:
    case CONSTRUCTOR:
    case INTEGER_CST:
      x = output_constant_def (target, 1);
      break;

    default:
      gcc_unreachable ();
    }

  gcc_assert (MEM_P (x));
  x = XEXP (x, 0);

  value->base = x;
  value->offset = offset;
}


static GTY((param_is (struct constant_descriptor_tree)))
     htab_t const_desc_htab;

static void maybe_output_constant_def_contents (struct constant_descriptor_tree *, int);

/* Constant pool accessor function.  */

htab_t
constant_pool_htab (void)
{
  return const_desc_htab;
}

/* Compute a hash code for a constant expression.  */

static hashval_t
const_desc_hash (const void *ptr)
{
  return ((const struct constant_descriptor_tree *)ptr)->hash;
}

static hashval_t
const_hash_1 (const tree exp)
{
  const char *p;
  hashval_t hi;
  int len, i;
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
      return real_hash (TREE_REAL_CST_PTR (exp));

    case FIXED_CST:
      return fixed_hash (TREE_FIXED_CST_PTR (exp));

    case STRING_CST:
      p = TREE_STRING_POINTER (exp);
      len = TREE_STRING_LENGTH (exp);
      break;

    case COMPLEX_CST:
      return (const_hash_1 (TREE_REALPART (exp)) * 5
	      + const_hash_1 (TREE_IMAGPART (exp)));

    case VECTOR_CST:
      {
	unsigned i;

	hi = 7 + VECTOR_CST_NELTS (exp);

	for (i = 0; i < VECTOR_CST_NELTS (exp); ++i)
	  hi = hi * 563 + const_hash_1 (VECTOR_CST_ELT (exp, i));

	return hi;
      }

    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT idx;
	tree value;

	hi = 5 + int_size_in_bytes (TREE_TYPE (exp));

	FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (exp), idx, value)
	  if (value)
	    hi = hi * 603 + const_hash_1 (value);

	return hi;
      }

    case ADDR_EXPR:
    case FDESC_EXPR:
      {
	struct addr_const value;

	decode_addr_const (exp, &value);
	switch (GET_CODE (value.base))
	  {
	  case SYMBOL_REF:
	    /* Don't hash the address of the SYMBOL_REF;
	       only use the offset and the symbol name.  */
	    hi = value.offset;
	    p = XSTR (value.base, 0);
	    for (i = 0; p[i] != 0; i++)
	      hi = ((hi * 613) + (unsigned) (p[i]));
	    break;

	  case LABEL_REF:
	    hi = value.offset + CODE_LABEL_NUMBER (XEXP (value.base, 0)) * 13;
	    break;

	  default:
	    gcc_unreachable ();
	  }
      }
      return hi;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      return (const_hash_1 (TREE_OPERAND (exp, 0)) * 9
	      + const_hash_1 (TREE_OPERAND (exp, 1)));

    CASE_CONVERT:
      return const_hash_1 (TREE_OPERAND (exp, 0)) * 7 + 2;

    default:
      /* A language specific constant. Just hash the code.  */
      return code;
    }

  /* Compute hashing function.  */
  hi = len;
  for (i = 0; i < len; i++)
    hi = ((hi * 613) + (unsigned) (p[i]));

  return hi;
}

/* Wrapper of compare_constant, for the htab interface.  */
static int
const_desc_eq (const void *p1, const void *p2)
{
  const struct constant_descriptor_tree *const c1
    = (const struct constant_descriptor_tree *) p1;
  const struct constant_descriptor_tree *const c2
    = (const struct constant_descriptor_tree *) p2;
  if (c1->hash != c2->hash)
    return 0;
  return compare_constant (c1->value, c2->value);
}

/* Compare t1 and t2, and return 1 only if they are known to result in
   the same bit pattern on output.  */

static int
compare_constant (const tree t1, const tree t2)
{
  enum tree_code typecode;

  if (t1 == NULL_TREE)
    return t2 == NULL_TREE;
  if (t2 == NULL_TREE)
    return 0;

  if (TREE_CODE (t1) != TREE_CODE (t2))
    return 0;

  switch (TREE_CODE (t1))
    {
    case INTEGER_CST:
      /* Integer constants are the same only if the same width of type.  */
      if (TYPE_PRECISION (TREE_TYPE (t1)) != TYPE_PRECISION (TREE_TYPE (t2)))
	return 0;
      if (TYPE_MODE (TREE_TYPE (t1)) != TYPE_MODE (TREE_TYPE (t2)))
	return 0;
      return tree_int_cst_equal (t1, t2);

    case REAL_CST:
      /* Real constants are the same only if the same width of type.  */
      if (TYPE_PRECISION (TREE_TYPE (t1)) != TYPE_PRECISION (TREE_TYPE (t2)))
	return 0;

      return REAL_VALUES_IDENTICAL (TREE_REAL_CST (t1), TREE_REAL_CST (t2));

    case FIXED_CST:
      /* Fixed constants are the same only if the same width of type.  */
      if (TYPE_PRECISION (TREE_TYPE (t1)) != TYPE_PRECISION (TREE_TYPE (t2)))
	return 0;

      return FIXED_VALUES_IDENTICAL (TREE_FIXED_CST (t1), TREE_FIXED_CST (t2));

    case STRING_CST:
      if (TYPE_MODE (TREE_TYPE (t1)) != TYPE_MODE (TREE_TYPE (t2)))
	return 0;

      return (TREE_STRING_LENGTH (t1) == TREE_STRING_LENGTH (t2)
	      && ! memcmp (TREE_STRING_POINTER (t1), TREE_STRING_POINTER (t2),
			 TREE_STRING_LENGTH (t1)));

    case COMPLEX_CST:
      return (compare_constant (TREE_REALPART (t1), TREE_REALPART (t2))
	      && compare_constant (TREE_IMAGPART (t1), TREE_IMAGPART (t2)));

    case VECTOR_CST:
      {
	unsigned i;

        if (VECTOR_CST_NELTS (t1) != VECTOR_CST_NELTS (t2))
	  return 0;

	for (i = 0; i < VECTOR_CST_NELTS (t1); ++i)
	  if (!compare_constant (VECTOR_CST_ELT (t1, i),
				 VECTOR_CST_ELT (t2, i)))
	    return 0;

	return 1;
      }

    case CONSTRUCTOR:
      {
	vec<constructor_elt, va_gc> *v1, *v2;
	unsigned HOST_WIDE_INT idx;

	typecode = TREE_CODE (TREE_TYPE (t1));
	if (typecode != TREE_CODE (TREE_TYPE (t2)))
	  return 0;

	if (typecode == ARRAY_TYPE)
	  {
	    HOST_WIDE_INT size_1 = int_size_in_bytes (TREE_TYPE (t1));
	    /* For arrays, check that the sizes all match.  */
	    if (TYPE_MODE (TREE_TYPE (t1)) != TYPE_MODE (TREE_TYPE (t2))
		|| size_1 == -1
		|| size_1 != int_size_in_bytes (TREE_TYPE (t2)))
	      return 0;
	  }
	else
	  {
	    /* For record and union constructors, require exact type
               equality.  */
	    if (TREE_TYPE (t1) != TREE_TYPE (t2))
	      return 0;
	  }

	v1 = CONSTRUCTOR_ELTS (t1);
	v2 = CONSTRUCTOR_ELTS (t2);
	if (vec_safe_length (v1) != vec_safe_length (v2))
	  return 0;

	for (idx = 0; idx < vec_safe_length (v1); ++idx)
	  {
	    constructor_elt *c1 = &(*v1)[idx];
	    constructor_elt *c2 = &(*v2)[idx];

	    /* Check that each value is the same...  */
	    if (!compare_constant (c1->value, c2->value))
	      return 0;
	    /* ... and that they apply to the same fields!  */
	    if (typecode == ARRAY_TYPE)
	      {
		if (!compare_constant (c1->index, c2->index))
		  return 0;
	      }
	    else
	      {
		if (c1->index != c2->index)
		  return 0;
	      }
	  }

	return 1;
      }

    case ADDR_EXPR:
    case FDESC_EXPR:
      {
	struct addr_const value1, value2;
	enum rtx_code code;
	int ret;

	decode_addr_const (t1, &value1);
	decode_addr_const (t2, &value2);

	if (value1.offset != value2.offset)
	  return 0;

	code = GET_CODE (value1.base);
	if (code != GET_CODE (value2.base))
	  return 0;

	switch (code)
	  {
	  case SYMBOL_REF:
	    ret = (strcmp (XSTR (value1.base, 0), XSTR (value2.base, 0)) == 0);
	    break;

	  case LABEL_REF:
	    ret = (CODE_LABEL_NUMBER (XEXP (value1.base, 0))
	           == CODE_LABEL_NUMBER (XEXP (value2.base, 0)));
	    break;

	  default:
	    gcc_unreachable ();
	  }
	return ret;
      }

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
    case RANGE_EXPR:
      return (compare_constant (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0))
	      && compare_constant (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1)));

    CASE_CONVERT:
    case VIEW_CONVERT_EXPR:
      return compare_constant (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

    default:
      return 0;
    }

  gcc_unreachable ();
}

/* Make a copy of the whole tree structure for a constant.  This
   handles the same types of nodes that compare_constant handles.  */

static tree
copy_constant (tree exp)
{
  switch (TREE_CODE (exp))
    {
    case ADDR_EXPR:
      /* For ADDR_EXPR, we do not want to copy the decl whose address
	 is requested.  We do want to copy constants though.  */
      if (CONSTANT_CLASS_P (TREE_OPERAND (exp, 0)))
	return build1 (TREE_CODE (exp), TREE_TYPE (exp),
		       copy_constant (TREE_OPERAND (exp, 0)));
      else
	return copy_node (exp);

    case INTEGER_CST:
    case REAL_CST:
    case FIXED_CST:
    case STRING_CST:
      return copy_node (exp);

    case COMPLEX_CST:
      return build_complex (TREE_TYPE (exp),
			    copy_constant (TREE_REALPART (exp)),
			    copy_constant (TREE_IMAGPART (exp)));

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      return build2 (TREE_CODE (exp), TREE_TYPE (exp),
		     copy_constant (TREE_OPERAND (exp, 0)),
		     copy_constant (TREE_OPERAND (exp, 1)));

    CASE_CONVERT:
    case VIEW_CONVERT_EXPR:
      return build1 (TREE_CODE (exp), TREE_TYPE (exp),
		     copy_constant (TREE_OPERAND (exp, 0)));

    case VECTOR_CST:
      return build_vector (TREE_TYPE (exp), VECTOR_CST_ELTS (exp));

    case CONSTRUCTOR:
      {
	tree copy = copy_node (exp);
	vec<constructor_elt, va_gc> *v;
	unsigned HOST_WIDE_INT idx;
	tree purpose, value;

	vec_alloc (v, vec_safe_length (CONSTRUCTOR_ELTS (exp)));
	FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (exp), idx, purpose, value)
	  {
	    constructor_elt ce = {purpose, copy_constant (value)};
	    v->quick_push (ce);
	  }
	CONSTRUCTOR_ELTS (copy) = v;
	return copy;
      }

    default:
      gcc_unreachable ();
    }
}

/* Return the section into which constant EXP should be placed.  */

static section *
get_constant_section (tree exp, unsigned int align)
{
  return targetm.asm_out.select_section (exp,
					 compute_reloc_for_constant (exp),
					 align);
}

/* Return the size of constant EXP in bytes.  */

static HOST_WIDE_INT
get_constant_size (tree exp)
{
  HOST_WIDE_INT size;

  size = int_size_in_bytes (TREE_TYPE (exp));
  if (TREE_CODE (exp) == STRING_CST)
    size = MAX (TREE_STRING_LENGTH (exp), size);
  return size;
}

/* Subroutine of output_constant_def:
   No constant equal to EXP is known to have been output.
   Make a constant descriptor to enter EXP in the hash table.
   Assign the label number and construct RTL to refer to the
   constant's location in memory.
   Caller is responsible for updating the hash table.  */

static struct constant_descriptor_tree *
build_constant_desc (tree exp)
{
  struct constant_descriptor_tree *desc;
  rtx symbol, rtl;
  char label[256];
  int labelno;
  tree decl;

  desc = ggc_alloc_constant_descriptor_tree ();
  desc->value = copy_constant (exp);

  /* Create a string containing the label name, in LABEL.  */
  labelno = const_labelno++;
  ASM_GENERATE_INTERNAL_LABEL (label, "LC", labelno);

  /* Construct the VAR_DECL associated with the constant.  */
  decl = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier (label),
		     TREE_TYPE (exp));
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  TREE_READONLY (decl) = 1;
  TREE_STATIC (decl) = 1;
  TREE_ADDRESSABLE (decl) = 1;
  /* We don't set the RTL yet as this would cause varpool to assume that the
     variable is referenced.  Moreover, it would just be dropped in LTO mode.
     Instead we set the flag that will be recognized in make_decl_rtl.  */
  DECL_IN_CONSTANT_POOL (decl) = 1;
  DECL_INITIAL (decl) = desc->value;
  /* ??? CONSTANT_ALIGNMENT hasn't been updated for vector types on most
     architectures so use DATA_ALIGNMENT as well, except for strings.  */
  if (TREE_CODE (exp) == STRING_CST)
    {
#ifdef CONSTANT_ALIGNMENT
      DECL_ALIGN (decl) = CONSTANT_ALIGNMENT (exp, DECL_ALIGN (decl));
#endif
    }
  else
    align_variable (decl, 0);

  /* Now construct the SYMBOL_REF and the MEM.  */
  if (use_object_blocks_p ())
    {
      section *sect = get_constant_section (exp, DECL_ALIGN (decl));
      symbol = create_block_symbol (ggc_strdup (label),
				    get_block_for_section (sect), -1);
    }
  else
    symbol = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (label));
  SYMBOL_REF_FLAGS (symbol) |= SYMBOL_FLAG_LOCAL;
  SET_SYMBOL_REF_DECL (symbol, decl);
  TREE_CONSTANT_POOL_ADDRESS_P (symbol) = 1;

  rtl = gen_const_mem (TYPE_MODE (TREE_TYPE (exp)), symbol);
  set_mem_attributes (rtl, exp, 1);
  set_mem_alias_set (rtl, 0);
  set_mem_alias_set (rtl, const_alias_set);

  /* We cannot share RTX'es in pool entries.
     Mark this piece of RTL as required for unsharing.  */
  RTX_FLAG (rtl, used) = 1;

  /* Set flags or add text to the name to record information, such as
     that it is a local symbol.  If the name is changed, the macro
     ASM_OUTPUT_LABELREF will have to know how to strip this
     information.  This call might invalidate our local variable
     SYMBOL; we can't use it afterward.  */
  targetm.encode_section_info (exp, rtl, true);

  desc->rtl = rtl;

  return desc;
}

/* Return an rtx representing a reference to constant data in memory
   for the constant expression EXP.

   If assembler code for such a constant has already been output,
   return an rtx to refer to it.
   Otherwise, output such a constant in memory
   and generate an rtx for it.

   If DEFER is nonzero, this constant can be deferred and output only
   if referenced in the function after all optimizations.

   `const_desc_table' records which constants already have label strings.  */

rtx
output_constant_def (tree exp, int defer)
{
  struct constant_descriptor_tree *desc;
  struct constant_descriptor_tree key;
  void **loc;

  /* Look up EXP in the table of constant descriptors.  If we didn't find
     it, create a new one.  */
  key.value = exp;
  key.hash = const_hash_1 (exp);
  loc = htab_find_slot_with_hash (const_desc_htab, &key, key.hash, INSERT);

  desc = (struct constant_descriptor_tree *) *loc;
  if (desc == 0)
    {
      desc = build_constant_desc (exp);
      desc->hash = key.hash;
      *loc = desc;
    }

  maybe_output_constant_def_contents (desc, defer);
  return desc->rtl;
}

/* Subroutine of output_constant_def: Decide whether or not we need to
   output the constant DESC now, and if so, do it.  */
static void
maybe_output_constant_def_contents (struct constant_descriptor_tree *desc,
				    int defer)
{
  rtx symbol = XEXP (desc->rtl, 0);
  tree exp = desc->value;

  if (flag_syntax_only)
    return;

  if (TREE_ASM_WRITTEN (exp))
    /* Already output; don't do it again.  */
    return;

  /* We can always defer constants as long as the context allows
     doing so.  */
  if (defer)
    {
      /* Increment n_deferred_constants if it exists.  It needs to be at
	 least as large as the number of constants actually referred to
	 by the function.  If it's too small we'll stop looking too early
	 and fail to emit constants; if it's too large we'll only look
	 through the entire function when we could have stopped earlier.  */
      if (cfun)
	n_deferred_constants++;
      return;
    }

  output_constant_def_contents (symbol);
}

/* Subroutine of output_constant_def_contents.  Output the definition
   of constant EXP, which is pointed to by label LABEL.  ALIGN is the
   constant's alignment in bits.  */

static void
assemble_constant_contents (tree exp, const char *label, unsigned int align)
{
  HOST_WIDE_INT size;

  size = get_constant_size (exp);

  /* Do any machine/system dependent processing of the constant.  */
  targetm.asm_out.declare_constant_name (asm_out_file, label, exp, size);

  /* Output the value of EXP.  */
  output_constant (exp, size, align);
}

/* We must output the constant data referred to by SYMBOL; do so.  */

static void
output_constant_def_contents (rtx symbol)
{
  tree decl = SYMBOL_REF_DECL (symbol);
  tree exp = DECL_INITIAL (decl);
  unsigned int align;
  bool asan_protected = false;

  /* Make sure any other constants whose addresses appear in EXP
     are assigned label numbers.  */
  output_addressed_constants (exp);

  /* We are no longer deferring this constant.  */
  TREE_ASM_WRITTEN (decl) = TREE_ASM_WRITTEN (exp) = 1;

  if ((flag_sanitize & SANITIZE_ADDRESS)
      && TREE_CODE (exp) == STRING_CST
      && asan_protect_global (exp))
    {
      asan_protected = true;
      DECL_ALIGN (decl) = MAX (DECL_ALIGN (decl),
			       ASAN_RED_ZONE_SIZE * BITS_PER_UNIT);
    }

  /* If the constant is part of an object block, make sure that the
     decl has been positioned within its block, but do not write out
     its definition yet.  output_object_blocks will do that later.  */
  if (SYMBOL_REF_HAS_BLOCK_INFO_P (symbol) && SYMBOL_REF_BLOCK (symbol))
    place_block_symbol (symbol);
  else
    {
      align = DECL_ALIGN (decl);
      switch_to_section (get_constant_section (exp, align));
      if (align > BITS_PER_UNIT)
	ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (align / BITS_PER_UNIT));
      assemble_constant_contents (exp, XSTR (symbol, 0), align);
      if (asan_protected)
	{
	  HOST_WIDE_INT size = get_constant_size (exp);
	  assemble_zeros (asan_red_zone_size (size));
	}
    }
}

/* Look up EXP in the table of constant descriptors.  Return the rtl
   if it has been emitted, else null.  */

rtx
lookup_constant_def (tree exp)
{
  struct constant_descriptor_tree *desc;
  struct constant_descriptor_tree key;

  key.value = exp;
  key.hash = const_hash_1 (exp);
  desc = (struct constant_descriptor_tree *)
    htab_find_with_hash (const_desc_htab, &key, key.hash);

  return (desc ? desc->rtl : NULL_RTX);
}

/* Return a tree representing a reference to constant data in memory
   for the constant expression EXP.

   This is the counterpart of output_constant_def at the Tree level.  */

tree
tree_output_constant_def (tree exp)
{
  struct constant_descriptor_tree *desc, key;
  void **loc;
  tree decl;

  /* Look up EXP in the table of constant descriptors.  If we didn't find
     it, create a new one.  */
  key.value = exp;
  key.hash = const_hash_1 (exp);
  loc = htab_find_slot_with_hash (const_desc_htab, &key, key.hash, INSERT);

  desc = (struct constant_descriptor_tree *) *loc;
  if (desc == 0)
    {
      desc = build_constant_desc (exp);
      desc->hash = key.hash;
      *loc = desc;
    }

  decl = SYMBOL_REF_DECL (XEXP (desc->rtl, 0));
  varpool_finalize_decl (decl);
  return decl;
}

/* Used in the hash tables to avoid outputting the same constant
   twice.  Unlike 'struct constant_descriptor_tree', RTX constants
   are output once per function, not once per file.  */
/* ??? Only a few targets need per-function constant pools.  Most
   can use one per-file pool.  Should add a targetm bit to tell the
   difference.  */

struct GTY(()) rtx_constant_pool {
  /* Pointers to first and last constant in pool, as ordered by offset.  */
  struct constant_descriptor_rtx *first;
  struct constant_descriptor_rtx *last;

  /* Hash facility for making memory-constants from constant rtl-expressions.
     It is used on RISC machines where immediate integer arguments and
     constant addresses are restricted so that such constants must be stored
     in memory.  */
  htab_t GTY((param_is (struct constant_descriptor_rtx))) const_rtx_htab;

  /* Current offset in constant pool (does not include any
     machine-specific header).  */
  HOST_WIDE_INT offset;
};

struct GTY((chain_next ("%h.next"))) constant_descriptor_rtx {
  struct constant_descriptor_rtx *next;
  rtx mem;
  rtx sym;
  rtx constant;
  HOST_WIDE_INT offset;
  hashval_t hash;
  enum machine_mode mode;
  unsigned int align;
  int labelno;
  int mark;
};

/* Hash and compare functions for const_rtx_htab.  */

static hashval_t
const_desc_rtx_hash (const void *ptr)
{
  const struct constant_descriptor_rtx *const desc
    = (const struct constant_descriptor_rtx *) ptr;
  return desc->hash;
}

static int
const_desc_rtx_eq (const void *a, const void *b)
{
  const struct constant_descriptor_rtx *const x
    = (const struct constant_descriptor_rtx *) a;
  const struct constant_descriptor_rtx *const y
    = (const struct constant_descriptor_rtx *) b;

  if (x->mode != y->mode)
    return 0;
  return rtx_equal_p (x->constant, y->constant);
}

/* This is the worker function for const_rtx_hash, called via for_each_rtx.  */

static int
const_rtx_hash_1 (rtx *xp, void *data)
{
  unsigned HOST_WIDE_INT hwi;
  enum machine_mode mode;
  enum rtx_code code;
  hashval_t h, *hp;
  rtx x;

  x = *xp;
  code = GET_CODE (x);
  mode = GET_MODE (x);
  h = (hashval_t) code * 1048573 + mode;

  switch (code)
    {
    case CONST_INT:
      hwi = INTVAL (x);
    fold_hwi:
      {
	int shift = sizeof (hashval_t) * CHAR_BIT;
	const int n = sizeof (HOST_WIDE_INT) / sizeof (hashval_t);
	int i;

	h ^= (hashval_t) hwi;
	for (i = 1; i < n; ++i)
	  {
	    hwi >>= shift;
	    h ^= (hashval_t) hwi;
	  }
      }
      break;

    case CONST_DOUBLE:
      if (mode == VOIDmode)
	{
	  hwi = CONST_DOUBLE_LOW (x) ^ CONST_DOUBLE_HIGH (x);
	  goto fold_hwi;
	}
      else
	h ^= real_hash (CONST_DOUBLE_REAL_VALUE (x));
      break;

    case CONST_FIXED:
      h ^= fixed_hash (CONST_FIXED_VALUE (x));
      break;

    case CONST_VECTOR:
      {
	int i;
	for (i = XVECLEN (x, 0); i-- > 0; )
	  h = h * 251 + const_rtx_hash_1 (&XVECEXP (x, 0, i), data);
      }
      break;

    case SYMBOL_REF:
      h ^= htab_hash_string (XSTR (x, 0));
      break;

    case LABEL_REF:
      h = h * 251 + CODE_LABEL_NUMBER (XEXP (x, 0));
      break;

    case UNSPEC:
    case UNSPEC_VOLATILE:
      h = h * 251 + XINT (x, 1);
      break;

    default:
      break;
    }

  hp = (hashval_t *) data;
  *hp = *hp * 509 + h;
  return 0;
}

/* Compute a hash value for X, which should be a constant.  */

static hashval_t
const_rtx_hash (rtx x)
{
  hashval_t h = 0;
  for_each_rtx (&x, const_rtx_hash_1, &h);
  return h;
}


/* Create and return a new rtx constant pool.  */

static struct rtx_constant_pool *
create_constant_pool (void)
{
  struct rtx_constant_pool *pool;

  pool = ggc_alloc_rtx_constant_pool ();
  pool->const_rtx_htab = htab_create_ggc (31, const_desc_rtx_hash,
					  const_desc_rtx_eq, NULL);
  pool->first = NULL;
  pool->last = NULL;
  pool->offset = 0;
  return pool;
}

/* Initialize constant pool hashing for a new function.  */

void
init_varasm_status (void)
{
  crtl->varasm.pool = create_constant_pool ();
  crtl->varasm.deferred_constants = 0;
}

/* Given a MINUS expression, simplify it if both sides
   include the same symbol.  */

rtx
simplify_subtraction (rtx x)
{
  rtx r = simplify_rtx (x);
  return r ? r : x;
}

/* Given a constant rtx X, make (or find) a memory constant for its value
   and return a MEM rtx to refer to it in memory.  */

rtx
force_const_mem (enum machine_mode mode, rtx x)
{
  struct constant_descriptor_rtx *desc, tmp;
  struct rtx_constant_pool *pool;
  char label[256];
  rtx def, symbol;
  hashval_t hash;
  unsigned int align;
  void **slot;

  /* If we're not allowed to drop X into the constant pool, don't.  */
  if (targetm.cannot_force_const_mem (mode, x))
    return NULL_RTX;

  /* Record that this function has used a constant pool entry.  */
  crtl->uses_const_pool = 1;

  /* Decide which pool to use.  */
  pool = (targetm.use_blocks_for_constant_p (mode, x)
	  ? shared_constant_pool
	  : crtl->varasm.pool);

  /* Lookup the value in the hashtable.  */
  tmp.constant = x;
  tmp.mode = mode;
  hash = const_rtx_hash (x);
  slot = htab_find_slot_with_hash (pool->const_rtx_htab, &tmp, hash, INSERT);
  desc = (struct constant_descriptor_rtx *) *slot;

  /* If the constant was already present, return its memory.  */
  if (desc)
    return copy_rtx (desc->mem);

  /* Otherwise, create a new descriptor.  */
  desc = ggc_alloc_constant_descriptor_rtx ();
  *slot = desc;

  /* Align the location counter as required by EXP's data type.  */
  align = GET_MODE_ALIGNMENT (mode == VOIDmode ? word_mode : mode);
#ifdef CONSTANT_ALIGNMENT
  {
    tree type = lang_hooks.types.type_for_mode (mode, 0);
    if (type != NULL_TREE)
      align = CONSTANT_ALIGNMENT (make_tree (type, x), align);
  }
#endif

  pool->offset += (align / BITS_PER_UNIT) - 1;
  pool->offset &= ~ ((align / BITS_PER_UNIT) - 1);

  desc->next = NULL;
  desc->constant = copy_rtx (tmp.constant);
  desc->offset = pool->offset;
  desc->hash = hash;
  desc->mode = mode;
  desc->align = align;
  desc->labelno = const_labelno;
  desc->mark = 0;

  pool->offset += GET_MODE_SIZE (mode);
  if (pool->last)
    pool->last->next = desc;
  else
    pool->first = pool->last = desc;
  pool->last = desc;

  /* Create a string containing the label name, in LABEL.  */
  ASM_GENERATE_INTERNAL_LABEL (label, "LC", const_labelno);
  ++const_labelno;

  /* Construct the SYMBOL_REF.  Make sure to mark it as belonging to
     the constants pool.  */
  if (use_object_blocks_p () && targetm.use_blocks_for_constant_p (mode, x))
    {
      section *sect = targetm.asm_out.select_rtx_section (mode, x, align);
      symbol = create_block_symbol (ggc_strdup (label),
				    get_block_for_section (sect), -1);
    }
  else
    symbol = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (label));
  desc->sym = symbol;
  SYMBOL_REF_FLAGS (symbol) |= SYMBOL_FLAG_LOCAL;
  CONSTANT_POOL_ADDRESS_P (symbol) = 1;
  SET_SYMBOL_REF_CONSTANT (symbol, desc);

  /* Construct the MEM.  */
  desc->mem = def = gen_const_mem (mode, symbol);
  set_mem_attributes (def, lang_hooks.types.type_for_mode (mode, 0), 1);
  set_mem_align (def, align);

  /* If we're dropping a label to the constant pool, make sure we
     don't delete it.  */
  if (GET_CODE (x) == LABEL_REF)
    LABEL_PRESERVE_P (XEXP (x, 0)) = 1;

  return copy_rtx (def);
}

/* Given a constant pool SYMBOL_REF, return the corresponding constant.  */

rtx
get_pool_constant (rtx addr)
{
  return SYMBOL_REF_CONSTANT (addr)->constant;
}

/* Given a constant pool SYMBOL_REF, return the corresponding constant
   and whether it has been output or not.  */

rtx
get_pool_constant_mark (rtx addr, bool *pmarked)
{
  struct constant_descriptor_rtx *desc;

  desc = SYMBOL_REF_CONSTANT (addr);
  *pmarked = (desc->mark != 0);
  return desc->constant;
}

/* Similar, return the mode.  */

enum machine_mode
get_pool_mode (const_rtx addr)
{
  return SYMBOL_REF_CONSTANT (addr)->mode;
}

/* Return the size of the constant pool.  */

int
get_pool_size (void)
{
  return crtl->varasm.pool->offset;
}

/* Worker function for output_constant_pool_1.  Emit assembly for X
   in MODE with known alignment ALIGN.  */

static void
output_constant_pool_2 (enum machine_mode mode, rtx x, unsigned int align)
{
  switch (GET_MODE_CLASS (mode))
    {
    case MODE_FLOAT:
    case MODE_DECIMAL_FLOAT:
      {
	REAL_VALUE_TYPE r;

	gcc_assert (CONST_DOUBLE_AS_FLOAT_P (x));
	REAL_VALUE_FROM_CONST_DOUBLE (r, x);
	assemble_real (r, mode, align);
	break;
      }

    case MODE_INT:
    case MODE_PARTIAL_INT:
    case MODE_FRACT:
    case MODE_UFRACT:
    case MODE_ACCUM:
    case MODE_UACCUM:
      assemble_integer (x, GET_MODE_SIZE (mode), align, 1);
      break;

    case MODE_VECTOR_FLOAT:
    case MODE_VECTOR_INT:
    case MODE_VECTOR_FRACT:
    case MODE_VECTOR_UFRACT:
    case MODE_VECTOR_ACCUM:
    case MODE_VECTOR_UACCUM:
      {
	int i, units;
        enum machine_mode submode = GET_MODE_INNER (mode);
	unsigned int subalign = MIN (align, GET_MODE_BITSIZE (submode));

	gcc_assert (GET_CODE (x) == CONST_VECTOR);
	units = CONST_VECTOR_NUNITS (x);

	for (i = 0; i < units; i++)
	  {
	    rtx elt = CONST_VECTOR_ELT (x, i);
	    output_constant_pool_2 (submode, elt, i ? subalign : align);
	  }
      }
      break;

    default:
      gcc_unreachable ();
    }
}

/* Worker function for output_constant_pool.  Emit constant DESC,
   giving it ALIGN bits of alignment.  */

static void
output_constant_pool_1 (struct constant_descriptor_rtx *desc,
			unsigned int align)
{
  rtx x, tmp;

  x = desc->constant;

  /* See if X is a LABEL_REF (or a CONST referring to a LABEL_REF)
     whose CODE_LABEL has been deleted.  This can occur if a jump table
     is eliminated by optimization.  If so, write a constant of zero
     instead.  Note that this can also happen by turning the
     CODE_LABEL into a NOTE.  */
  /* ??? This seems completely and utterly wrong.  Certainly it's
     not true for NOTE_INSN_DELETED_LABEL, but I disbelieve proper
     functioning even with INSN_DELETED_P and friends.  */

  tmp = x;
  switch (GET_CODE (tmp))
    {
    case CONST:
      if (GET_CODE (XEXP (tmp, 0)) != PLUS
	  || GET_CODE (XEXP (XEXP (tmp, 0), 0)) != LABEL_REF)
	break;
      tmp = XEXP (XEXP (tmp, 0), 0);
      /* FALLTHRU  */

    case LABEL_REF:
      tmp = XEXP (tmp, 0);
      gcc_assert (!INSN_DELETED_P (tmp));
      gcc_assert (!NOTE_P (tmp)
		  || NOTE_KIND (tmp) != NOTE_INSN_DELETED);
      break;

    default:
      break;
    }

#ifdef ASM_OUTPUT_SPECIAL_POOL_ENTRY
  ASM_OUTPUT_SPECIAL_POOL_ENTRY (asm_out_file, x, desc->mode,
				 align, desc->labelno, done);
#endif

  assemble_align (align);

  /* Output the label.  */
  targetm.asm_out.internal_label (asm_out_file, "LC", desc->labelno);

  /* Output the data.  */
  output_constant_pool_2 (desc->mode, x, align);

  /* Make sure all constants in SECTION_MERGE and not SECTION_STRINGS
     sections have proper size.  */
  if (align > GET_MODE_BITSIZE (desc->mode)
      && in_section
      && (in_section->common.flags & SECTION_MERGE))
    assemble_align (align);

#ifdef ASM_OUTPUT_SPECIAL_POOL_ENTRY
 done:
#endif
  return;
}

/* Given a SYMBOL_REF CURRENT_RTX, mark it and all constants it refers
   to as used.  Emit referenced deferred strings.  This function can
   be used with for_each_rtx to mark all SYMBOL_REFs in an rtx.  */

static int
mark_constant (rtx *current_rtx, void *data ATTRIBUTE_UNUSED)
{
  rtx x = *current_rtx;

  if (x == NULL_RTX || GET_CODE (x) != SYMBOL_REF)
    return 0;

  if (CONSTANT_POOL_ADDRESS_P (x))
    {
      struct constant_descriptor_rtx *desc = SYMBOL_REF_CONSTANT (x);
      if (desc->mark == 0)
	{
	  desc->mark = 1;
	  for_each_rtx (&desc->constant, mark_constant, NULL);
	}
    }
  else if (TREE_CONSTANT_POOL_ADDRESS_P (x))
    {
      tree decl = SYMBOL_REF_DECL (x);
      if (!TREE_ASM_WRITTEN (DECL_INITIAL (decl)))
	{
	  n_deferred_constants--;
	  output_constant_def_contents (x);
	}
    }

  return -1;
}

/* Look through appropriate parts of INSN, marking all entries in the
   constant pool which are actually being used.  Entries that are only
   referenced by other constants are also marked as used.  Emit
   deferred strings that are used.  */

static void
mark_constants (rtx insn)
{
  if (!INSN_P (insn))
    return;

  /* Insns may appear inside a SEQUENCE.  Only check the patterns of
     insns, not any notes that may be attached.  We don't want to mark
     a constant just because it happens to appear in a REG_EQUIV note.  */
  if (GET_CODE (PATTERN (insn)) == SEQUENCE)
    {
      rtx seq = PATTERN (insn);
      int i, n = XVECLEN (seq, 0);
      for (i = 0; i < n; ++i)
	{
	  rtx subinsn = XVECEXP (seq, 0, i);
	  if (INSN_P (subinsn))
	    for_each_rtx (&PATTERN (subinsn), mark_constant, NULL);
	}
    }
  else
    for_each_rtx (&PATTERN (insn), mark_constant, NULL);
}

/* Look through the instructions for this function, and mark all the
   entries in POOL which are actually being used.  Emit deferred constants
   which have indeed been used.  */

static void
mark_constant_pool (void)
{
  rtx insn;

  if (!crtl->uses_const_pool && n_deferred_constants == 0)
    return;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    mark_constants (insn);
}

/* Write all the constants in POOL.  */

static void
output_constant_pool_contents (struct rtx_constant_pool *pool)
{
  struct constant_descriptor_rtx *desc;

  for (desc = pool->first; desc ; desc = desc->next)
    if (desc->mark)
      {
	/* If the constant is part of an object_block, make sure that
	   the constant has been positioned within its block, but do not
	   write out its definition yet.  output_object_blocks will do
	   that later.  */
	if (SYMBOL_REF_HAS_BLOCK_INFO_P (desc->sym)
	    && SYMBOL_REF_BLOCK (desc->sym))
	  place_block_symbol (desc->sym);
	else
	  {
	    switch_to_section (targetm.asm_out.select_rtx_section
			       (desc->mode, desc->constant, desc->align));
	    output_constant_pool_1 (desc, desc->align);
	  }
      }
}

/* Mark all constants that are used in the current function, then write
   out the function's private constant pool.  */

static void
output_constant_pool (const char *fnname ATTRIBUTE_UNUSED,
		      tree fndecl ATTRIBUTE_UNUSED)
{
  struct rtx_constant_pool *pool = crtl->varasm.pool;

  /* It is possible for gcc to call force_const_mem and then to later
     discard the instructions which refer to the constant.  In such a
     case we do not need to output the constant.  */
  mark_constant_pool ();

#ifdef ASM_OUTPUT_POOL_PROLOGUE
  ASM_OUTPUT_POOL_PROLOGUE (asm_out_file, fnname, fndecl, pool->offset);
#endif

  output_constant_pool_contents (pool);

#ifdef ASM_OUTPUT_POOL_EPILOGUE
  ASM_OUTPUT_POOL_EPILOGUE (asm_out_file, fnname, fndecl, pool->offset);
#endif
}

/* Write the contents of the shared constant pool.  */

void
output_shared_constant_pool (void)
{
  output_constant_pool_contents (shared_constant_pool);
}

/* Determine what kind of relocations EXP may need.  */

int
compute_reloc_for_constant (tree exp)
{
  int reloc = 0, reloc2;
  tree tem;

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

      if (TREE_CODE (tem) == MEM_REF
	  && TREE_CODE (TREE_OPERAND (tem, 0)) == ADDR_EXPR)
	{
	  reloc = compute_reloc_for_constant (TREE_OPERAND (tem, 0));
	  break;
	}

      if (TREE_PUBLIC (tem))
	reloc |= 2;
      else
	reloc |= 1;
      break;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
      reloc = compute_reloc_for_constant (TREE_OPERAND (exp, 0));
      reloc |= compute_reloc_for_constant (TREE_OPERAND (exp, 1));
      break;

    case MINUS_EXPR:
      reloc = compute_reloc_for_constant (TREE_OPERAND (exp, 0));
      reloc2 = compute_reloc_for_constant (TREE_OPERAND (exp, 1));
      /* The difference of two local labels is computable at link time.  */
      if (reloc == 1 && reloc2 == 1)
	reloc = 0;
      else
	reloc |= reloc2;
      break;

    CASE_CONVERT:
    case VIEW_CONVERT_EXPR:
      reloc = compute_reloc_for_constant (TREE_OPERAND (exp, 0));
      break;

    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT idx;
	FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (exp), idx, tem)
	  if (tem != 0)
	    reloc |= compute_reloc_for_constant (tem);
      }
      break;

    default:
      break;
    }
  return reloc;
}

/* Find all the constants whose addresses are referenced inside of EXP,
   and make sure assembler code with a label has been output for each one.
   Indicate whether an ADDR_EXPR has been encountered.  */

static void
output_addressed_constants (tree exp)
{
  tree tem;

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

      /* If we have an initialized CONST_DECL, retrieve the initializer.  */
      if (TREE_CODE (tem) == CONST_DECL && DECL_INITIAL (tem))
	tem = DECL_INITIAL (tem);

      if (CONSTANT_CLASS_P (tem) || TREE_CODE (tem) == CONSTRUCTOR)
	output_constant_def (tem, 0);

      if (TREE_CODE (tem) == MEM_REF)
	output_addressed_constants (TREE_OPERAND (tem, 0));
      break;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      output_addressed_constants (TREE_OPERAND (exp, 1));
      /* Fall through.  */

    CASE_CONVERT:
    case VIEW_CONVERT_EXPR:
      output_addressed_constants (TREE_OPERAND (exp, 0));
      break;

    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT idx;
	FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (exp), idx, tem)
	  if (tem != 0)
	    output_addressed_constants (tem);
      }
      break;

    default:
      break;
    }
}

/* Whether a constructor CTOR is a valid static constant initializer if all
   its elements are.  This used to be internal to initializer_constant_valid_p
   and has been exposed to let other functions like categorize_ctor_elements
   evaluate the property while walking a constructor for other purposes.  */

bool
constructor_static_from_elts_p (const_tree ctor)
{
  return (TREE_CONSTANT (ctor)
	  && (TREE_CODE (TREE_TYPE (ctor)) == UNION_TYPE
	      || TREE_CODE (TREE_TYPE (ctor)) == RECORD_TYPE
	      || TREE_CODE (TREE_TYPE (ctor)) == ARRAY_TYPE));
}

static tree initializer_constant_valid_p_1 (tree value, tree endtype,
					    tree *cache);

/* A subroutine of initializer_constant_valid_p.  VALUE is a MINUS_EXPR,
   PLUS_EXPR or POINTER_PLUS_EXPR.  This looks for cases of VALUE
   which are valid when ENDTYPE is an integer of any size; in
   particular, this does not accept a pointer minus a constant.  This
   returns null_pointer_node if the VALUE is an absolute constant
   which can be used to initialize a static variable.  Otherwise it
   returns NULL.  */

static tree
narrowing_initializer_constant_valid_p (tree value, tree endtype, tree *cache)
{
  tree op0, op1;

  if (!INTEGRAL_TYPE_P (endtype))
    return NULL_TREE;

  op0 = TREE_OPERAND (value, 0);
  op1 = TREE_OPERAND (value, 1);

  /* Like STRIP_NOPS except allow the operand mode to widen.  This
     works around a feature of fold that simplifies (int)(p1 - p2) to
     ((int)p1 - (int)p2) under the theory that the narrower operation
     is cheaper.  */

  while (CONVERT_EXPR_P (op0)
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

  while (CONVERT_EXPR_P (op1)
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

  op0 = initializer_constant_valid_p_1 (op0, endtype, cache);
  if (!op0)
    return NULL_TREE;

  op1 = initializer_constant_valid_p_1 (op1, endtype,
					cache ? cache + 2 : NULL);
  /* Both initializers must be known.  */
  if (op1)
    {
      if (op0 == op1
	  && (op0 == null_pointer_node
	      || TREE_CODE (value) == MINUS_EXPR))
	return null_pointer_node;

      /* Support differences between labels.  */
      if (TREE_CODE (op0) == LABEL_DECL
	  && TREE_CODE (op1) == LABEL_DECL)
	return null_pointer_node;

      if (TREE_CODE (op0) == STRING_CST
	  && TREE_CODE (op1) == STRING_CST
	  && operand_equal_p (op0, op1, 1))
	return null_pointer_node;
    }

  return NULL_TREE;
}

/* Helper function of initializer_constant_valid_p.
   Return nonzero if VALUE is a valid constant-valued expression
   for use in initializing a static variable; one that can be an
   element of a "constant" initializer.

   Return null_pointer_node if the value is absolute;
   if it is relocatable, return the variable that determines the relocation.
   We assume that VALUE has been folded as much as possible;
   therefore, we do not need to check for such things as
   arithmetic-combinations of integers.

   Use CACHE (pointer to 2 tree values) for caching if non-NULL.  */

static tree
initializer_constant_valid_p_1 (tree value, tree endtype, tree *cache)
{
  tree ret;

  switch (TREE_CODE (value))
    {
    case CONSTRUCTOR:
      if (constructor_static_from_elts_p (value))
	{
	  unsigned HOST_WIDE_INT idx;
	  tree elt;
	  bool absolute = true;

	  if (cache && cache[0] == value)
	    return cache[1];
	  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (value), idx, elt)
	    {
	      tree reloc;
	      reloc = initializer_constant_valid_p_1 (elt, TREE_TYPE (elt),
						      NULL);
	      if (!reloc)
		{
		  if (cache)
		    {
		      cache[0] = value;
		      cache[1] = NULL_TREE;
		    }
		  return NULL_TREE;
		}
	      if (reloc != null_pointer_node)
		absolute = false;
	    }
	  /* For a non-absolute relocation, there is no single
	     variable that can be "the variable that determines the
	     relocation."  */
	  if (cache)
	    {
	      cache[0] = value;
	      cache[1] = absolute ? null_pointer_node : error_mark_node;
	    }
	  return absolute ? null_pointer_node : error_mark_node;
	}

      return TREE_STATIC (value) ? null_pointer_node : NULL_TREE;

    case INTEGER_CST:
    case VECTOR_CST:
    case REAL_CST:
    case FIXED_CST:
    case STRING_CST:
    case COMPLEX_CST:
      return null_pointer_node;

    case ADDR_EXPR:
    case FDESC_EXPR:
      {
	tree op0 = staticp (TREE_OPERAND (value, 0));
	if (op0)
	  {
	    /* "&(*a).f" is like unto pointer arithmetic.  If "a" turns out
	       to be a constant, this is old-skool offsetof-like nonsense.  */
	    if (TREE_CODE (op0) == INDIRECT_REF
		&& TREE_CONSTANT (TREE_OPERAND (op0, 0)))
	      return null_pointer_node;
	    /* Taking the address of a nested function involves a trampoline,
	       unless we don't need or want one.  */
	    if (TREE_CODE (op0) == FUNCTION_DECL
		&& DECL_STATIC_CHAIN (op0)
		&& !TREE_NO_TRAMPOLINE (value))
	      return NULL_TREE;
	    /* "&{...}" requires a temporary to hold the constructed
	       object.  */
	    if (TREE_CODE (op0) == CONSTRUCTOR)
	      return NULL_TREE;
	  }
	return op0;
      }

    case NON_LVALUE_EXPR:
      return initializer_constant_valid_p_1 (TREE_OPERAND (value, 0),
					     endtype, cache);

    case VIEW_CONVERT_EXPR:
      {
	tree src = TREE_OPERAND (value, 0);
	tree src_type = TREE_TYPE (src);
	tree dest_type = TREE_TYPE (value);

	/* Allow view-conversions from aggregate to non-aggregate type only
	   if the bit pattern is fully preserved afterwards; otherwise, the
	   RTL expander won't be able to apply a subsequent transformation
	   to the underlying constructor.  */
	if (AGGREGATE_TYPE_P (src_type) && !AGGREGATE_TYPE_P (dest_type))
	  {
	    if (TYPE_MODE (endtype) == TYPE_MODE (dest_type))
	      return initializer_constant_valid_p_1 (src, endtype, cache);
	    else
	      return NULL_TREE;
	  }

	/* Allow all other kinds of view-conversion.  */
	return initializer_constant_valid_p_1 (src, endtype, cache);
      }

    CASE_CONVERT:
      {
	tree src = TREE_OPERAND (value, 0);
	tree src_type = TREE_TYPE (src);
	tree dest_type = TREE_TYPE (value);

	/* Allow conversions between pointer types, floating-point
	   types, and offset types.  */
	if ((POINTER_TYPE_P (dest_type) && POINTER_TYPE_P (src_type))
	    || (FLOAT_TYPE_P (dest_type) && FLOAT_TYPE_P (src_type))
	    || (TREE_CODE (dest_type) == OFFSET_TYPE
		&& TREE_CODE (src_type) == OFFSET_TYPE))
	  return initializer_constant_valid_p_1 (src, endtype, cache);

	/* Allow length-preserving conversions between integer types.  */
	if (INTEGRAL_TYPE_P (dest_type) && INTEGRAL_TYPE_P (src_type)
	    && (TYPE_PRECISION (dest_type) == TYPE_PRECISION (src_type)))
	  return initializer_constant_valid_p_1 (src, endtype, cache);

	/* Allow conversions between other integer types only if
	   explicit value.  */
	if (INTEGRAL_TYPE_P (dest_type) && INTEGRAL_TYPE_P (src_type))
	  {
	    tree inner = initializer_constant_valid_p_1 (src, endtype, cache);
	    if (inner == null_pointer_node)
	      return null_pointer_node;
	    break;
	  }

	/* Allow (int) &foo provided int is as wide as a pointer.  */
	if (INTEGRAL_TYPE_P (dest_type) && POINTER_TYPE_P (src_type)
	    && (TYPE_PRECISION (dest_type) >= TYPE_PRECISION (src_type)))
	  return initializer_constant_valid_p_1 (src, endtype, cache);

	/* Likewise conversions from int to pointers, but also allow
	   conversions from 0.  */
	if ((POINTER_TYPE_P (dest_type)
	     || TREE_CODE (dest_type) == OFFSET_TYPE)
	    && INTEGRAL_TYPE_P (src_type))
	  {
	    if (TREE_CODE (src) == INTEGER_CST
		&& TYPE_PRECISION (dest_type) >= TYPE_PRECISION (src_type))
	      return null_pointer_node;
	    if (integer_zerop (src))
	      return null_pointer_node;
	    else if (TYPE_PRECISION (dest_type) <= TYPE_PRECISION (src_type))
	      return initializer_constant_valid_p_1 (src, endtype, cache);
	  }

	/* Allow conversions to struct or union types if the value
	   inside is okay.  */
	if (TREE_CODE (dest_type) == RECORD_TYPE
	    || TREE_CODE (dest_type) == UNION_TYPE)
	  return initializer_constant_valid_p_1 (src, endtype, cache);
      }
      break;

    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
      /* Any valid floating-point constants will have been folded by now;
	 with -frounding-math we hit this with addition of two constants.  */
      if (TREE_CODE (endtype) == REAL_TYPE)
	return NULL_TREE;
      if (cache && cache[0] == value)
	return cache[1];
      if (! INTEGRAL_TYPE_P (endtype)
	  || TYPE_PRECISION (endtype) >= TYPE_PRECISION (TREE_TYPE (value)))
	{
	  tree ncache[4] = { NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE };
	  tree valid0
	    = initializer_constant_valid_p_1 (TREE_OPERAND (value, 0),
					      endtype, ncache);
	  tree valid1
	    = initializer_constant_valid_p_1 (TREE_OPERAND (value, 1),
					      endtype, ncache + 2);
	  /* If either term is absolute, use the other term's relocation.  */
	  if (valid0 == null_pointer_node)
	    ret = valid1;
	  else if (valid1 == null_pointer_node)
	    ret = valid0;
	  /* Support narrowing pointer differences.  */
	  else
	    ret = narrowing_initializer_constant_valid_p (value, endtype,
							  ncache);
	}
      else
      /* Support narrowing pointer differences.  */
	ret = narrowing_initializer_constant_valid_p (value, endtype, NULL);
      if (cache)
	{
	  cache[0] = value;
	  cache[1] = ret;
	}
      return ret;

    case MINUS_EXPR:
      if (TREE_CODE (endtype) == REAL_TYPE)
	return NULL_TREE;
      if (cache && cache[0] == value)
	return cache[1];
      if (! INTEGRAL_TYPE_P (endtype)
	  || TYPE_PRECISION (endtype) >= TYPE_PRECISION (TREE_TYPE (value)))
	{
	  tree ncache[4] = { NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE };
	  tree valid0
	    = initializer_constant_valid_p_1 (TREE_OPERAND (value, 0),
					      endtype, ncache);
	  tree valid1
	    = initializer_constant_valid_p_1 (TREE_OPERAND (value, 1),
					      endtype, ncache + 2);
	  /* Win if second argument is absolute.  */
	  if (valid1 == null_pointer_node)
	    ret = valid0;
	  /* Win if both arguments have the same relocation.
	     Then the value is absolute.  */
	  else if (valid0 == valid1 && valid0 != 0)
	    ret = null_pointer_node;
	  /* Since GCC guarantees that string constants are unique in the
	     generated code, a subtraction between two copies of the same
	     constant string is absolute.  */
	  else if (valid0 && TREE_CODE (valid0) == STRING_CST
		   && valid1 && TREE_CODE (valid1) == STRING_CST
		   && operand_equal_p (valid0, valid1, 1))
	    ret = null_pointer_node;
	  /* Support narrowing differences.  */
	  else
	    ret = narrowing_initializer_constant_valid_p (value, endtype,
							  ncache);
	}
      else
	/* Support narrowing differences.  */
	ret = narrowing_initializer_constant_valid_p (value, endtype, NULL);
      if (cache)
	{
	  cache[0] = value;
	  cache[1] = ret;
	}
      return ret;

    default:
      break;
    }

  return NULL_TREE;
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
initializer_constant_valid_p (tree value, tree endtype)
{
  return initializer_constant_valid_p_1 (value, endtype, NULL);
}

/* Return true if VALUE is a valid constant-valued expression
   for use in initializing a static bit-field; one that can be
   an element of a "constant" initializer.  */

bool
initializer_constant_valid_for_bitfield_p (tree value)
{
  /* For bitfields we support integer constants or possibly nested aggregates
     of such.  */
  switch (TREE_CODE (value))
    {
    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT idx;
	tree elt;

	FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (value), idx, elt)
	  if (!initializer_constant_valid_for_bitfield_p (elt))
	    return false;
	return true;
      }

    case INTEGER_CST:
    case REAL_CST:
      return true;

    case VIEW_CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      return
	initializer_constant_valid_for_bitfield_p (TREE_OPERAND (value, 0));

    default:
      break;
    }

  return false;
}

/* output_constructor outer state of relevance in recursive calls, typically
   for nested aggregate bitfields.  */

typedef struct {
  unsigned int bit_offset;  /* current position in ...  */
  int byte;                 /* ... the outer byte buffer.  */
} oc_outer_state;

static unsigned HOST_WIDE_INT
  output_constructor (tree, unsigned HOST_WIDE_INT, unsigned int,
		      oc_outer_state *);

/* Output assembler code for constant EXP, with no label.
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
output_constant (tree exp, unsigned HOST_WIDE_INT size, unsigned int align)
{
  enum tree_code code;
  unsigned HOST_WIDE_INT thissize;

  if (size == 0 || flag_syntax_only)
    return;

  /* See if we're trying to initialize a pointer in a non-default mode
     to the address of some declaration somewhere.  If the target says
     the mode is valid for pointers, assume the target has a way of
     resolving it.  */
  if (TREE_CODE (exp) == NOP_EXPR
      && POINTER_TYPE_P (TREE_TYPE (exp))
      && targetm.addr_space.valid_pointer_mode
	   (TYPE_MODE (TREE_TYPE (exp)),
	    TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (exp)))))
    {
      tree saved_type = TREE_TYPE (exp);

      /* Peel off any intermediate conversions-to-pointer for valid
	 pointer modes.  */
      while (TREE_CODE (exp) == NOP_EXPR
	     && POINTER_TYPE_P (TREE_TYPE (exp))
	     && targetm.addr_space.valid_pointer_mode
		  (TYPE_MODE (TREE_TYPE (exp)),
		   TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (exp)))))
	exp = TREE_OPERAND (exp, 0);

      /* If what we're left with is the address of something, we can
	 convert the address to the final type and output it that
	 way.  */
      if (TREE_CODE (exp) == ADDR_EXPR)
	exp = build1 (ADDR_EXPR, saved_type, TREE_OPERAND (exp, 0));
      /* Likewise for constant ints.  */
      else if (TREE_CODE (exp) == INTEGER_CST)
	exp = build_int_cst_wide (saved_type, TREE_INT_CST_LOW (exp),
				  TREE_INT_CST_HIGH (exp));

    }

  /* Eliminate any conversions since we'll be outputting the underlying
     constant.  */
  while (CONVERT_EXPR_P (exp)
	 || TREE_CODE (exp) == NON_LVALUE_EXPR
	 || TREE_CODE (exp) == VIEW_CONVERT_EXPR)
    {
      HOST_WIDE_INT type_size = int_size_in_bytes (TREE_TYPE (exp));
      HOST_WIDE_INT op_size = int_size_in_bytes (TREE_TYPE (TREE_OPERAND (exp, 0)));

      /* Make sure eliminating the conversion is really a no-op, except with
	 VIEW_CONVERT_EXPRs to allow for wild Ada unchecked conversions and
	 union types to allow for Ada unchecked unions.  */
      if (type_size > op_size
	  && TREE_CODE (exp) != VIEW_CONVERT_EXPR
	  && TREE_CODE (TREE_TYPE (exp)) != UNION_TYPE)
	/* Keep the conversion. */
	break;
      else
	exp = TREE_OPERAND (exp, 0);
    }

  code = TREE_CODE (TREE_TYPE (exp));
  thissize = int_size_in_bytes (TREE_TYPE (exp));

  /* Allow a constructor with no elements for any data type.
     This means to fill the space with zeros.  */
  if (TREE_CODE (exp) == CONSTRUCTOR
      && vec_safe_is_empty (CONSTRUCTOR_ELTS (exp)))
    {
      assemble_zeros (size);
      return;
    }

  if (TREE_CODE (exp) == FDESC_EXPR)
    {
#ifdef ASM_OUTPUT_FDESC
      HOST_WIDE_INT part = tree_to_shwi (TREE_OPERAND (exp, 1));
      tree decl = TREE_OPERAND (exp, 0);
      ASM_OUTPUT_FDESC (asm_out_file, decl, part);
#else
      gcc_unreachable ();
#endif
      return;
    }

  /* Now output the underlying data.  If we've handling the padding, return.
     Otherwise, break and ensure SIZE is the size written.  */
  switch (code)
    {
    case BOOLEAN_TYPE:
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case OFFSET_TYPE:
    case FIXED_POINT_TYPE:
    case NULLPTR_TYPE:
      if (! assemble_integer (expand_expr (exp, NULL_RTX, VOIDmode,
					   EXPAND_INITIALIZER),
			      MIN (size, thissize), align, 0))
	error ("initializer for integer/fixed-point value is too complicated");
      break;

    case REAL_TYPE:
      if (TREE_CODE (exp) != REAL_CST)
	error ("initializer for floating value is not a floating constant");
      else
	assemble_real (TREE_REAL_CST (exp), TYPE_MODE (TREE_TYPE (exp)), align);
      break;

    case COMPLEX_TYPE:
      output_constant (TREE_REALPART (exp), thissize / 2, align);
      output_constant (TREE_IMAGPART (exp), thissize / 2,
		       min_align (align, BITS_PER_UNIT * (thissize / 2)));
      break;

    case ARRAY_TYPE:
    case VECTOR_TYPE:
      switch (TREE_CODE (exp))
	{
	case CONSTRUCTOR:
	  output_constructor (exp, size, align, NULL);
	  return;
	case STRING_CST:
	  thissize
	    = MIN ((unsigned HOST_WIDE_INT)TREE_STRING_LENGTH (exp), size);
	  assemble_string (TREE_STRING_POINTER (exp), thissize);
	  break;
	case VECTOR_CST:
	  {
	    enum machine_mode inner = TYPE_MODE (TREE_TYPE (TREE_TYPE (exp)));
	    unsigned int nalign = MIN (align, GET_MODE_ALIGNMENT (inner));
	    int elt_size = GET_MODE_SIZE (inner);
	    output_constant (VECTOR_CST_ELT (exp, 0), elt_size, align);
	    thissize = elt_size;
	    for (unsigned int i = 1; i < VECTOR_CST_NELTS (exp); i++)
	      {
		output_constant (VECTOR_CST_ELT (exp, i), elt_size, nalign);
		thissize += elt_size;
	      }
	    break;
	  }
	default:
	  gcc_unreachable ();
	}
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      gcc_assert (TREE_CODE (exp) == CONSTRUCTOR);
      output_constructor (exp, size, align, NULL);
      return;

    case ERROR_MARK:
      return;

    default:
      gcc_unreachable ();
    }

  if (size > thissize)
    assemble_zeros (size - thissize);
}


/* Subroutine of output_constructor, used for computing the size of
   arrays of unspecified length.  VAL must be a CONSTRUCTOR of an array
   type with an unspecified upper bound.  */

static unsigned HOST_WIDE_INT
array_size_for_constructor (tree val)
{
  tree max_index;
  unsigned HOST_WIDE_INT cnt;
  tree index, value, tmp;
  double_int i;

  /* This code used to attempt to handle string constants that are not
     arrays of single-bytes, but nothing else does, so there's no point in
     doing it here.  */
  if (TREE_CODE (val) == STRING_CST)
    return TREE_STRING_LENGTH (val);

  max_index = NULL_TREE;
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (val), cnt, index, value)
    {
      if (TREE_CODE (index) == RANGE_EXPR)
	index = TREE_OPERAND (index, 1);
      if (max_index == NULL_TREE || tree_int_cst_lt (max_index, index))
	max_index = index;
    }

  if (max_index == NULL_TREE)
    return 0;

  /* Compute the total number of array elements.  */
  tmp = TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (val)));
  i = tree_to_double_int (max_index) - tree_to_double_int (tmp);
  i += double_int_one;

  /* Multiply by the array element unit size to find number of bytes.  */
  i *= tree_to_double_int (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (val))));

  gcc_assert (i.fits_uhwi ());
  return i.low;
}

/* Other datastructures + helpers for output_constructor.  */

/* output_constructor local state to support interaction with helpers.  */

typedef struct {

  /* Received arguments.  */
  tree exp;                     /* Constructor expression.  */
  tree type;                    /* Type of constructor expression.  */
  unsigned HOST_WIDE_INT size;  /* # bytes to output - pad if necessary.  */
  unsigned int align;           /* Known initial alignment.  */
  tree min_index;               /* Lower bound if specified for an array.  */

  /* Output processing state.  */
  HOST_WIDE_INT total_bytes;  /* # bytes output so far / current position.  */
  int byte;                   /* Part of a bitfield byte yet to be output.  */
  int last_relative_index;    /* Implicit or explicit index of the last
				 array element output within a bitfield.  */
  bool byte_buffer_in_use;    /* Whether BYTE is in use.  */

  /* Current element.  */
  tree field;      /* Current field decl in a record.  */
  tree val;        /* Current element value.  */
  tree index;      /* Current element index.  */

} oc_local_state;

/* Helper for output_constructor.  From the current LOCAL state, output a
   RANGE_EXPR element.  */

static void
output_constructor_array_range (oc_local_state *local)
{
  unsigned HOST_WIDE_INT fieldsize
    = int_size_in_bytes (TREE_TYPE (local->type));

  HOST_WIDE_INT lo_index
    = tree_to_shwi (TREE_OPERAND (local->index, 0));
  HOST_WIDE_INT hi_index
    = tree_to_shwi (TREE_OPERAND (local->index, 1));
  HOST_WIDE_INT index;

  unsigned int align2
    = min_align (local->align, fieldsize * BITS_PER_UNIT);

  for (index = lo_index; index <= hi_index; index++)
    {
      /* Output the element's initial value.  */
      if (local->val == NULL_TREE)
	assemble_zeros (fieldsize);
      else
	output_constant (local->val, fieldsize, align2);

      /* Count its size.  */
      local->total_bytes += fieldsize;
    }
}

/* Helper for output_constructor.  From the current LOCAL state, output a
   field element that is not true bitfield or part of an outer one.  */

static void
output_constructor_regular_field (oc_local_state *local)
{
  /* Field size and position.  Since this structure is static, we know the
     positions are constant.  */
  unsigned HOST_WIDE_INT fieldsize;
  HOST_WIDE_INT fieldpos;

  unsigned int align2;

  if (local->index != NULL_TREE)
    {
      /* Perform the index calculation in modulo arithmetic but
	 sign-extend the result because Ada has negative DECL_FIELD_OFFSETs
	 but we are using an unsigned sizetype.  */
      unsigned prec = TYPE_PRECISION (sizetype);
      double_int idx = tree_to_double_int (local->index)
		       - tree_to_double_int (local->min_index);
      idx = idx.sext (prec);
      fieldpos = (tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (local->val)))
		  * idx.low);
    }
  else if (local->field != NULL_TREE)
    fieldpos = int_byte_position (local->field);
  else
    fieldpos = 0;

  /* Output any buffered-up bit-fields preceding this element.  */
  if (local->byte_buffer_in_use)
    {
      assemble_integer (GEN_INT (local->byte), 1, BITS_PER_UNIT, 1);
      local->total_bytes++;
      local->byte_buffer_in_use = false;
    }

  /* Advance to offset of this element.
     Note no alignment needed in an array, since that is guaranteed
     if each element has the proper size.  */
  if ((local->field != NULL_TREE || local->index != NULL_TREE)
      && fieldpos != local->total_bytes)
    {
      gcc_assert (fieldpos >= local->total_bytes);
      assemble_zeros (fieldpos - local->total_bytes);
      local->total_bytes = fieldpos;
    }

  /* Find the alignment of this element.  */
  align2 = min_align (local->align, BITS_PER_UNIT * fieldpos);

  /* Determine size this element should occupy.  */
  if (local->field)
    {
      fieldsize = 0;

      /* If this is an array with an unspecified upper bound,
	 the initializer determines the size.  */
      /* ??? This ought to only checked if DECL_SIZE_UNIT is NULL,
	 but we cannot do this until the deprecated support for
	 initializing zero-length array members is removed.  */
      if (TREE_CODE (TREE_TYPE (local->field)) == ARRAY_TYPE
	  && TYPE_DOMAIN (TREE_TYPE (local->field))
	  && ! TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (local->field))))
	{
	  fieldsize = array_size_for_constructor (local->val);
	  /* Given a non-empty initialization, this field had
	     better be last.  */
	  gcc_assert (!fieldsize || !DECL_CHAIN (local->field));
	}
      else
	fieldsize = tree_to_uhwi (DECL_SIZE_UNIT (local->field));
    }
  else
    fieldsize = int_size_in_bytes (TREE_TYPE (local->type));

  /* Output the element's initial value.  */
  if (local->val == NULL_TREE)
    assemble_zeros (fieldsize);
  else
    output_constant (local->val, fieldsize, align2);

  /* Count its size.  */
  local->total_bytes += fieldsize;
}

/* Helper for output_constructor.  From the LOCAL state, output an element
   that is a true bitfield or part of an outer one.  BIT_OFFSET is the offset
   from the start of a possibly ongoing outer byte buffer.  */

static void
output_constructor_bitfield (oc_local_state *local, unsigned int bit_offset)
{
  /* Bit size of this element.  */
  HOST_WIDE_INT ebitsize
    = (local->field
       ? tree_to_uhwi (DECL_SIZE (local->field))
       : tree_to_uhwi (TYPE_SIZE (TREE_TYPE (local->type))));

  /* Relative index of this element if this is an array component.  */
  HOST_WIDE_INT relative_index
    = (!local->field
       ? (local->index
	  ? (tree_to_shwi (local->index)
	     - tree_to_shwi (local->min_index))
	  : local->last_relative_index + 1)
       : 0);

  /* Bit position of this element from the start of the containing
     constructor.  */
  HOST_WIDE_INT constructor_relative_ebitpos
      = (local->field
	 ? int_bit_position (local->field)
	 : ebitsize * relative_index);

  /* Bit position of this element from the start of a possibly ongoing
     outer byte buffer.  */
  HOST_WIDE_INT byte_relative_ebitpos
    = bit_offset + constructor_relative_ebitpos;

  /* From the start of a possibly ongoing outer byte buffer, offsets to
     the first bit of this element and to the first bit past the end of
     this element.  */
  HOST_WIDE_INT next_offset = byte_relative_ebitpos;
  HOST_WIDE_INT end_offset = byte_relative_ebitpos + ebitsize;

  local->last_relative_index = relative_index;

  if (local->val == NULL_TREE)
    local->val = integer_zero_node;

  while (TREE_CODE (local->val) == VIEW_CONVERT_EXPR
	 || TREE_CODE (local->val) == NON_LVALUE_EXPR)
    local->val = TREE_OPERAND (local->val, 0);

  if (TREE_CODE (local->val) != INTEGER_CST
      && TREE_CODE (local->val) != CONSTRUCTOR)
    {
      error ("invalid initial value for member %qE", DECL_NAME (local->field));
      return;
    }

  /* If this field does not start in this (or next) byte, skip some bytes.  */
  if (next_offset / BITS_PER_UNIT != local->total_bytes)
    {
      /* Output remnant of any bit field in previous bytes.  */
      if (local->byte_buffer_in_use)
	{
	  assemble_integer (GEN_INT (local->byte), 1, BITS_PER_UNIT, 1);
	  local->total_bytes++;
	  local->byte_buffer_in_use = false;
	}

      /* If still not at proper byte, advance to there.  */
      if (next_offset / BITS_PER_UNIT != local->total_bytes)
	{
	  gcc_assert (next_offset / BITS_PER_UNIT >= local->total_bytes);
	  assemble_zeros (next_offset / BITS_PER_UNIT - local->total_bytes);
	  local->total_bytes = next_offset / BITS_PER_UNIT;
	}
    }

  /* Set up the buffer if necessary.  */
  if (!local->byte_buffer_in_use)
    {
      local->byte = 0;
      if (ebitsize > 0)
	local->byte_buffer_in_use = true;
    }

  /* If this is nested constructor, recurse passing the bit offset and the
     pending data, then retrieve the new pending data afterwards.  */
  if (TREE_CODE (local->val) == CONSTRUCTOR)
    {
      oc_outer_state temp_state;
      temp_state.bit_offset = next_offset % BITS_PER_UNIT;
      temp_state.byte = local->byte;
      local->total_bytes
	  += output_constructor (local->val, 0, 0, &temp_state);
      local->byte = temp_state.byte;
      return;
    }

  /* Otherwise, we must split the element into pieces that fall within
     separate bytes, and combine each byte with previous or following
     bit-fields.  */
  while (next_offset < end_offset)
    {
      int this_time;
      int shift;
      HOST_WIDE_INT value;
      HOST_WIDE_INT next_byte = next_offset / BITS_PER_UNIT;
      HOST_WIDE_INT next_bit = next_offset % BITS_PER_UNIT;

      /* Advance from byte to byte within this element when necessary.  */
      while (next_byte != local->total_bytes)
	{
	  assemble_integer (GEN_INT (local->byte), 1, BITS_PER_UNIT, 1);
	  local->total_bytes++;
	  local->byte = 0;
	}

      /* Number of bits we can process at once (all part of the same byte).  */
      this_time = MIN (end_offset - next_offset, BITS_PER_UNIT - next_bit);
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
	    value = TREE_INT_CST_LOW (local->val);
	  else
	    {
	      gcc_assert (shift < HOST_BITS_PER_DOUBLE_INT);
	      value = TREE_INT_CST_HIGH (local->val);
	      shift -= HOST_BITS_PER_WIDE_INT;
	    }

	  /* Get the result. This works only when:
	     1 <= this_time <= HOST_BITS_PER_WIDE_INT.  */
	  local->byte |= (((value >> shift)
			   & (((HOST_WIDE_INT) 2 << (this_time - 1)) - 1))
			  << (BITS_PER_UNIT - this_time - next_bit));
	}
      else
	{
	  /* On little-endian machines,
	     take first the least significant bits of the value
	     and pack them starting at the least significant
	     bits of the bytes.  */
	  shift = next_offset - byte_relative_ebitpos;

	  /* Don't try to take a bunch of bits that cross
	     the word boundary in the INTEGER_CST. We can
	     only select bits from the LOW or HIGH part
	     not from both.  */
	  if (shift < HOST_BITS_PER_WIDE_INT
	      && shift + this_time > HOST_BITS_PER_WIDE_INT)
	    this_time = (HOST_BITS_PER_WIDE_INT - shift);

	  /* Now get the bits from the appropriate constant word.  */
	  if (shift < HOST_BITS_PER_WIDE_INT)
	    value = TREE_INT_CST_LOW (local->val);
	  else
	    {
	      gcc_assert (shift < HOST_BITS_PER_DOUBLE_INT);
	      value = TREE_INT_CST_HIGH (local->val);
	      shift -= HOST_BITS_PER_WIDE_INT;
	    }

	  /* Get the result. This works only when:
	     1 <= this_time <= HOST_BITS_PER_WIDE_INT.  */
	  local->byte |= (((value >> shift)
			   & (((HOST_WIDE_INT) 2 << (this_time - 1)) - 1))
			  << next_bit);
	}

      next_offset += this_time;
      local->byte_buffer_in_use = true;
    }
}

/* Subroutine of output_constant, used for CONSTRUCTORs (aggregate constants).
   Generate at least SIZE bytes, padding if necessary.  OUTER designates the
   caller output state of relevance in recursive invocations.  */

static unsigned HOST_WIDE_INT
output_constructor (tree exp, unsigned HOST_WIDE_INT size,
		    unsigned int align, oc_outer_state *outer)
{
  unsigned HOST_WIDE_INT cnt;
  constructor_elt *ce;

  oc_local_state local;

  /* Setup our local state to communicate with helpers.  */
  local.exp = exp;
  local.type = TREE_TYPE (exp);
  local.size = size;
  local.align = align;
  if (TREE_CODE (local.type) == ARRAY_TYPE && TYPE_DOMAIN (local.type))
    local.min_index = TYPE_MIN_VALUE (TYPE_DOMAIN (local.type));
  else
    local.min_index = NULL_TREE;

  local.total_bytes = 0;
  local.byte_buffer_in_use = outer != NULL;
  local.byte = outer ? outer->byte : 0;
  local.last_relative_index = -1;

  gcc_assert (HOST_BITS_PER_WIDE_INT >= BITS_PER_UNIT);

  /* As CE goes through the elements of the constant, FIELD goes through the
     structure fields if the constant is a structure.  If the constant is a
     union, we override this by getting the field from the TREE_LIST element.
     But the constant could also be an array.  Then FIELD is zero.

     There is always a maximum of one element in the chain LINK for unions
     (even if the initializer in a source program incorrectly contains
     more one).  */

  if (TREE_CODE (local.type) == RECORD_TYPE)
    local.field = TYPE_FIELDS (local.type);
  else
    local.field = NULL_TREE;

  for (cnt = 0;
       vec_safe_iterate (CONSTRUCTOR_ELTS (exp), cnt, &ce);
       cnt++, local.field = local.field ? DECL_CHAIN (local.field) : 0)
    {
      local.val = ce->value;
      local.index = NULL_TREE;

      /* The element in a union constructor specifies the proper field
	 or index.  */
      if (RECORD_OR_UNION_TYPE_P (local.type) && ce->index != NULL_TREE)
	local.field = ce->index;

      else if (TREE_CODE (local.type) == ARRAY_TYPE)
	local.index = ce->index;

      if (local.field && flag_verbose_asm)
	fprintf (asm_out_file, "%s %s:\n",
		 ASM_COMMENT_START,
		 DECL_NAME (local.field)
		 ? IDENTIFIER_POINTER (DECL_NAME (local.field))
		 : "<anonymous>");

      /* Eliminate the marker that makes a cast not be an lvalue.  */
      if (local.val != NULL_TREE)
	STRIP_NOPS (local.val);

      /* Output the current element, using the appropriate helper ...  */

      /* For an array slice not part of an outer bitfield.  */
      if (!outer
	  && local.index != NULL_TREE
	  && TREE_CODE (local.index) == RANGE_EXPR)
	output_constructor_array_range (&local);

      /* For a field that is neither a true bitfield nor part of an outer one,
	 known to be at least byte aligned and multiple-of-bytes long.  */
      else if (!outer
	       && (local.field == NULL_TREE
		   || !CONSTRUCTOR_BITFIELD_P (local.field)))
	output_constructor_regular_field (&local);

      /* For a true bitfield or part of an outer one.  Only INTEGER_CSTs are
	 supported for scalar fields, so we may need to convert first.  */
      else
        {
	  if (TREE_CODE (local.val) == REAL_CST)
	    local.val
	      = fold_unary (VIEW_CONVERT_EXPR,
			    build_nonstandard_integer_type
			    (TYPE_PRECISION (TREE_TYPE (local.val)), 0),
			    local.val);
	  output_constructor_bitfield (&local, outer ? outer->bit_offset : 0);
	}
    }

  /* If we are not at toplevel, save the pending data for our caller.
     Otherwise output the pending data and padding zeros as needed. */
  if (outer)
    outer->byte = local.byte;
  else
    {
      if (local.byte_buffer_in_use)
	{
	  assemble_integer (GEN_INT (local.byte), 1, BITS_PER_UNIT, 1);
	  local.total_bytes++;
	}

      if ((unsigned HOST_WIDE_INT)local.total_bytes < local.size)
	{
	  assemble_zeros (local.size - local.total_bytes);
	  local.total_bytes = local.size;
	}
    }

  return local.total_bytes;
}

/* Mark DECL as weak.  */

static void
mark_weak (tree decl)
{
  DECL_WEAK (decl) = 1;

  if (DECL_RTL_SET_P (decl)
      && MEM_P (DECL_RTL (decl))
      && XEXP (DECL_RTL (decl), 0)
      && GET_CODE (XEXP (DECL_RTL (decl), 0)) == SYMBOL_REF)
    SYMBOL_REF_WEAK (XEXP (DECL_RTL (decl), 0)) = 1;
}

/* Merge weak status between NEWDECL and OLDDECL.  */

void
merge_weak (tree newdecl, tree olddecl)
{
  if (DECL_WEAK (newdecl) == DECL_WEAK (olddecl))
    {
      if (DECL_WEAK (newdecl) && TARGET_SUPPORTS_WEAK)
        {
          tree *pwd;
          /* We put the NEWDECL on the weak_decls list at some point
             and OLDDECL as well.  Keep just OLDDECL on the list.  */
	  for (pwd = &weak_decls; *pwd; pwd = &TREE_CHAIN (*pwd))
	    if (TREE_VALUE (*pwd) == newdecl)
	      {
	        *pwd = TREE_CHAIN (*pwd);
		break;
	      }
        }
      return;
    }

  if (DECL_WEAK (newdecl))
    {
      tree wd;

      /* NEWDECL is weak, but OLDDECL is not.  */

      /* If we already output the OLDDECL, we're in trouble; we can't
	 go back and make it weak.  This should never happen in
	 unit-at-a-time compilation.  */
      gcc_assert (!TREE_ASM_WRITTEN (olddecl));

      /* If we've already generated rtl referencing OLDDECL, we may
	 have done so in a way that will not function properly with
	 a weak symbol.  Again in unit-at-a-time this should be
	 impossible.  */
      gcc_assert (!TREE_USED (olddecl)
	          || !TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (olddecl)));

      if (TARGET_SUPPORTS_WEAK)
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
declare_weak (tree decl)
{
  gcc_assert (TREE_CODE (decl) != FUNCTION_DECL || !TREE_ASM_WRITTEN (decl));
  if (! TREE_PUBLIC (decl))
    error ("weak declaration of %q+D must be public", decl);
  else if (!TARGET_SUPPORTS_WEAK)
    warning (0, "weak declaration of %q+D not supported", decl);

  mark_weak (decl);
  if (!lookup_attribute ("weak", DECL_ATTRIBUTES (decl)))
    DECL_ATTRIBUTES (decl)
      = tree_cons (get_identifier ("weak"), NULL, DECL_ATTRIBUTES (decl));
}

static void
weak_finish_1 (tree decl)
{
#if defined (ASM_WEAKEN_DECL) || defined (ASM_WEAKEN_LABEL)
  const char *const name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
#endif

  if (! TREE_USED (decl))
    return;

#ifdef ASM_WEAKEN_DECL
  ASM_WEAKEN_DECL (asm_out_file, decl, name, NULL);
#else
#ifdef ASM_WEAKEN_LABEL
  ASM_WEAKEN_LABEL (asm_out_file, name);
#else
#ifdef ASM_OUTPUT_WEAK_ALIAS
  {
    static bool warn_once = 0;
    if (! warn_once)
      {
	warning (0, "only weak aliases are supported in this configuration");
	warn_once = 1;
      }
    return;
  }
#endif
#endif
#endif
}

/* Fiven an assembly name, find the decl it is associated with.  */
static tree
find_decl (tree target)
{
  symtab_node *node = symtab_node_for_asm (target);
  if (node)
    return node->decl;
  return NULL_TREE;
}

/* This TREE_LIST contains weakref targets.  */

static GTY(()) tree weakref_targets;

/* Emit any pending weak declarations.  */

void
weak_finish (void)
{
  tree t;

  for (t = weakref_targets; t; t = TREE_CHAIN (t))
    {
      tree alias_decl = TREE_PURPOSE (t);
      tree target = ultimate_transparent_alias_target (&TREE_VALUE (t));

      if (! TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (alias_decl)))
	/* Remove alias_decl from the weak list, but leave entries for
	   the target alone.  */
	target = NULL_TREE;
#ifndef ASM_OUTPUT_WEAKREF
      else if (! TREE_SYMBOL_REFERENCED (target))
	{
	  /* Use ASM_WEAKEN_LABEL only if ASM_WEAKEN_DECL is not
	     defined, otherwise we and weak_finish_1 would use
	     different macros.  */
# if defined ASM_WEAKEN_LABEL && ! defined ASM_WEAKEN_DECL
	  ASM_WEAKEN_LABEL (asm_out_file, IDENTIFIER_POINTER (target));
# else
	  tree decl = find_decl (target);

	  if (! decl)
	    {
	      decl = build_decl (DECL_SOURCE_LOCATION (alias_decl),
				 TREE_CODE (alias_decl), target,
				 TREE_TYPE (alias_decl));

	      DECL_EXTERNAL (decl) = 1;
	      TREE_PUBLIC (decl) = 1;
	      DECL_ARTIFICIAL (decl) = 1;
	      TREE_NOTHROW (decl) = TREE_NOTHROW (alias_decl);
	      TREE_USED (decl) = 1;
	    }

	  weak_finish_1 (decl);
# endif
	}
#endif

      {
	tree *p;
	tree t2;

	/* Remove the alias and the target from the pending weak list
	   so that we do not emit any .weak directives for the former,
	   nor multiple .weak directives for the latter.  */
	for (p = &weak_decls; (t2 = *p) ; )
	  {
	    if (TREE_VALUE (t2) == alias_decl
		|| target == DECL_ASSEMBLER_NAME (TREE_VALUE (t2)))
	      *p = TREE_CHAIN (t2);
	    else
	      p = &TREE_CHAIN (t2);
	  }

	/* Remove other weakrefs to the same target, to speed things up.  */
	for (p = &TREE_CHAIN (t); (t2 = *p) ; )
	  {
	    if (target == ultimate_transparent_alias_target (&TREE_VALUE (t2)))
	      *p = TREE_CHAIN (t2);
	    else
	      p = &TREE_CHAIN (t2);
	  }
      }
    }

  for (t = weak_decls; t; t = TREE_CHAIN (t))
    {
      tree decl = TREE_VALUE (t);

      weak_finish_1 (decl);
    }
}

/* Emit the assembly bits to indicate that DECL is globally visible.  */

static void
globalize_decl (tree decl)
{

#if defined (ASM_WEAKEN_LABEL) || defined (ASM_WEAKEN_DECL)
  if (DECL_WEAK (decl))
    {
      const char *name = XSTR (XEXP (DECL_RTL (decl), 0), 0);
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

      /* Remove weakrefs to the same target from the pending weakref
	 list, for the same reason.  */
      for (p = &weakref_targets; (t = *p) ; )
	{
	  if (DECL_ASSEMBLER_NAME (decl)
	      == ultimate_transparent_alias_target (&TREE_VALUE (t)))
	    *p = TREE_CHAIN (t);
	  else
	    p = &TREE_CHAIN (t);
	}

      return;
    }
#endif

  targetm.asm_out.globalize_decl_name (asm_out_file, decl);
}

vec<alias_pair, va_gc> *alias_pairs;

/* Output the assembler code for a define (equate) using ASM_OUTPUT_DEF
   or ASM_OUTPUT_DEF_FROM_DECLS.  The function defines the symbol whose
   tree node is DECL to have the value of the tree node TARGET.  */

void
do_assemble_alias (tree decl, tree target)
{
  /* Emulated TLS had better not get this var.  */
  gcc_assert (!(!targetm.have_tls
		&& TREE_CODE (decl) == VAR_DECL
		&& DECL_THREAD_LOCAL_P (decl)));

  if (TREE_ASM_WRITTEN (decl))
    return;

  /* We must force creation of DECL_RTL for debug info generation, even though
     we don't use it here.  */
  make_decl_rtl (decl);

  TREE_ASM_WRITTEN (decl) = 1;
  TREE_ASM_WRITTEN (DECL_ASSEMBLER_NAME (decl)) = 1;

  if (lookup_attribute ("weakref", DECL_ATTRIBUTES (decl)))
    {
      ultimate_transparent_alias_target (&target);

      if (!TREE_SYMBOL_REFERENCED (target))
	weakref_targets = tree_cons (decl, target, weakref_targets);

#ifdef ASM_OUTPUT_WEAKREF
      ASM_OUTPUT_WEAKREF (asm_out_file, decl,
			  IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)),
			  IDENTIFIER_POINTER (target));
#else
      if (!TARGET_SUPPORTS_WEAK)
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "weakref is not supported in this configuration");
	  return;
	}
#endif
      return;
    }

#ifdef ASM_OUTPUT_DEF
  /* Make name accessible from other files, if appropriate.  */

  if (TREE_PUBLIC (decl))
    {
      globalize_decl (decl);
      maybe_assemble_visibility (decl);
    }
  if (lookup_attribute ("ifunc", DECL_ATTRIBUTES (decl)))
    {
#if defined (ASM_OUTPUT_TYPE_DIRECTIVE)
      if (targetm.has_ifunc_p ())
	ASM_OUTPUT_TYPE_DIRECTIVE
	  (asm_out_file, IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)),
	   IFUNC_ASM_TYPE);
      else
#endif
	error_at (DECL_SOURCE_LOCATION (decl),
		  "ifunc is not supported on this target");
    }

# ifdef ASM_OUTPUT_DEF_FROM_DECLS
  ASM_OUTPUT_DEF_FROM_DECLS (asm_out_file, decl, target);
# else
  ASM_OUTPUT_DEF (asm_out_file,
		  IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)),
		  IDENTIFIER_POINTER (target));
# endif
#elif defined (ASM_OUTPUT_WEAK_ALIAS) || defined (ASM_WEAKEN_DECL)
  {
    const char *name;
    tree *p, t;

    name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
# ifdef ASM_WEAKEN_DECL
    ASM_WEAKEN_DECL (asm_out_file, decl, name, IDENTIFIER_POINTER (target));
# else
    ASM_OUTPUT_WEAK_ALIAS (asm_out_file, name, IDENTIFIER_POINTER (target));
# endif
    /* Remove this function from the pending weak list so that
       we do not emit multiple .weak directives for it.  */
    for (p = &weak_decls; (t = *p) ; )
      if (DECL_ASSEMBLER_NAME (decl) == DECL_ASSEMBLER_NAME (TREE_VALUE (t)))
	*p = TREE_CHAIN (t);
      else
	p = &TREE_CHAIN (t);

    /* Remove weakrefs to the same target from the pending weakref
       list, for the same reason.  */
    for (p = &weakref_targets; (t = *p) ; )
      {
	if (DECL_ASSEMBLER_NAME (decl)
	    == ultimate_transparent_alias_target (&TREE_VALUE (t)))
	  *p = TREE_CHAIN (t);
	else
	  p = &TREE_CHAIN (t);
      }
  }
#endif
}

/* Emit an assembler directive to make the symbol for DECL an alias to
   the symbol for TARGET.  */

void
assemble_alias (tree decl, tree target)
{
  tree target_decl;

  if (lookup_attribute ("weakref", DECL_ATTRIBUTES (decl)))
    {
      tree alias = DECL_ASSEMBLER_NAME (decl);

      ultimate_transparent_alias_target (&target);

      if (alias == target)
	error ("weakref %q+D ultimately targets itself", decl);
      if (TREE_PUBLIC (decl))
	error ("weakref %q+D must have static linkage", decl);
    }
  else
    {
#if !defined (ASM_OUTPUT_DEF)
# if !defined(ASM_OUTPUT_WEAK_ALIAS) && !defined (ASM_WEAKEN_DECL)
      error_at (DECL_SOURCE_LOCATION (decl),
		"alias definitions not supported in this configuration");
      return;
# else
      if (!DECL_WEAK (decl))
	{
	  if (lookup_attribute ("ifunc", DECL_ATTRIBUTES (decl)))
	    error_at (DECL_SOURCE_LOCATION (decl),
		      "ifunc is not supported in this configuration");
	  else
	    error_at (DECL_SOURCE_LOCATION (decl),
		      "only weak aliases are supported in this configuration");
	  return;
	}
# endif
#endif
    }
  TREE_USED (decl) = 1;

  /* Allow aliases to aliases.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    cgraph_get_create_node (decl)->alias = true;
  else
    varpool_node_for_decl (decl)->alias = true;

  /* If the target has already been emitted, we don't have to queue the
     alias.  This saves a tad of memory.  */
  if (cgraph_global_info_ready)
    target_decl = find_decl (target);
  else
    target_decl= NULL;
  if ((target_decl && TREE_ASM_WRITTEN (target_decl))
      || cgraph_state >= CGRAPH_STATE_EXPANSION)
    do_assemble_alias (decl, target);
  else
    {
      alias_pair p = {decl, target};
      vec_safe_push (alias_pairs, p);
    }
}

/* Record and output a table of translations from original function
   to its transaction aware clone.  Note that tm_pure functions are
   considered to be their own clone.  */

static GTY((if_marked ("tree_map_marked_p"), param_is (struct tree_map)))
     htab_t tm_clone_hash;

void
record_tm_clone_pair (tree o, tree n)
{
  struct tree_map **slot, *h;

  if (tm_clone_hash == NULL)
    tm_clone_hash = htab_create_ggc (32, tree_map_hash, tree_map_eq, 0);

  h = ggc_alloc_tree_map ();
  h->hash = htab_hash_pointer (o);
  h->base.from = o;
  h->to = n;

  slot = (struct tree_map **)
    htab_find_slot_with_hash (tm_clone_hash, h, h->hash, INSERT);
  *slot = h;
}

tree
get_tm_clone_pair (tree o)
{
  if (tm_clone_hash)
    {
      struct tree_map *h, in;

      in.base.from = o;
      in.hash = htab_hash_pointer (o);
      h = (struct tree_map *) htab_find_with_hash (tm_clone_hash,
						   &in, in.hash);
      if (h)
	return h->to;
    }
  return NULL_TREE;
}

typedef struct tm_alias_pair
{
  unsigned int uid;
  tree from;
  tree to;
} tm_alias_pair;


/* Helper function for finish_tm_clone_pairs.  Dump a hash table entry
   into a VEC in INFO.  */

static int
dump_tm_clone_to_vec (void **slot, void *info)
{
  struct tree_map *map = (struct tree_map *) *slot;
  vec<tm_alias_pair> *tm_alias_pairs = (vec<tm_alias_pair> *) info;
  tm_alias_pair p = {DECL_UID (map->base.from), map->base.from, map->to};
  tm_alias_pairs->safe_push (p);
  return 1;
}

/* Dump the actual pairs to the .tm_clone_table section.  */

static void
dump_tm_clone_pairs (vec<tm_alias_pair> tm_alias_pairs)
{
  unsigned i;
  tm_alias_pair *p;
  bool switched = false;

  FOR_EACH_VEC_ELT (tm_alias_pairs, i, p)
    {
      tree src = p->from;
      tree dst = p->to;
      struct cgraph_node *src_n = cgraph_get_node (src);
      struct cgraph_node *dst_n = cgraph_get_node (dst);

      /* The function ipa_tm_create_version() marks the clone as needed if
	 the original function was needed.  But we also mark the clone as
	 needed if we ever called the clone indirectly through
	 TM_GETTMCLONE.  If neither of these are true, we didn't generate
	 a clone, and we didn't call it indirectly... no sense keeping it
	 in the clone table.  */
      if (!dst_n || !dst_n->definition)
	continue;

      /* This covers the case where we have optimized the original
	 function away, and only access the transactional clone.  */
      if (!src_n || !src_n->definition)
	continue;

      if (!switched)
	{
	  switch_to_section (targetm.asm_out.tm_clone_table_section ());
	  assemble_align (POINTER_SIZE);
	  switched = true;
	}

      assemble_integer (XEXP (DECL_RTL (src), 0),
			POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
      assemble_integer (XEXP (DECL_RTL (dst), 0),
			POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
    }
}

/* Provide a default for the tm_clone_table section.  */

section *
default_clone_table_section (void)
{
  return get_named_section (NULL, ".tm_clone_table", 3);
}

/* Helper comparison function for qsorting by the DECL_UID stored in
   alias_pair->emitted_diags.  */

static int
tm_alias_pair_cmp (const void *x, const void *y)
{
  const tm_alias_pair *p1 = (const tm_alias_pair *) x;
  const tm_alias_pair *p2 = (const tm_alias_pair *) y;
  if (p1->uid < p2->uid)
    return -1;
  if (p1->uid > p2->uid)
    return 1;
  return 0;
}

void
finish_tm_clone_pairs (void)
{
  vec<tm_alias_pair> tm_alias_pairs = vNULL;

  if (tm_clone_hash == NULL)
    return;

  /* We need a determenistic order for the .tm_clone_table, otherwise
     we will get bootstrap comparison failures, so dump the hash table
     to a vector, sort it, and dump the vector.  */

  /* Dump the hashtable to a vector.  */
  htab_traverse_noresize (tm_clone_hash, dump_tm_clone_to_vec,
			  (void *) &tm_alias_pairs);
  /* Sort it.  */
  tm_alias_pairs.qsort (tm_alias_pair_cmp);

  /* Dump it.  */
  dump_tm_clone_pairs (tm_alias_pairs);

  htab_delete (tm_clone_hash);
  tm_clone_hash = NULL;
  tm_alias_pairs.release ();
}


/* Emit an assembler directive to set symbol for DECL visibility to
   the visibility type VIS, which must not be VISIBILITY_DEFAULT.  */

void
default_assemble_visibility (tree decl ATTRIBUTE_UNUSED,
			     int vis ATTRIBUTE_UNUSED)
{
#ifdef HAVE_GAS_HIDDEN
  static const char * const visibility_types[] = {
    NULL, "protected", "hidden", "internal"
  };

  const char *name, *type;

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  type = visibility_types[vis];

  fprintf (asm_out_file, "\t.%s\t", type);
  assemble_name (asm_out_file, name);
  fprintf (asm_out_file, "\n");
#else
  if (!DECL_ARTIFICIAL (decl))
    warning (OPT_Wattributes, "visibility attribute not supported "
	     "in this configuration; ignored");
#endif
}

/* A helper function to call assemble_visibility when needed for a decl.  */

int
maybe_assemble_visibility (tree decl)
{
  enum symbol_visibility vis = DECL_VISIBILITY (decl);

  if (vis != VISIBILITY_DEFAULT)
    {
      targetm.asm_out.assemble_visibility (decl, vis);
      return 1;
    }
  else
    return 0;
}

/* Returns 1 if the target configuration supports defining public symbols
   so that one of them will be chosen at link time instead of generating a
   multiply-defined symbol error, whether through the use of weak symbols or
   a target-specific mechanism for having duplicates discarded.  */

int
supports_one_only (void)
{
  if (SUPPORTS_ONE_ONLY)
    return 1;
  return TARGET_SUPPORTS_WEAK;
}

/* Set up DECL as a public symbol that can be defined in multiple
   translation units without generating a linker error.  */

void
make_decl_one_only (tree decl, tree comdat_group)
{
  gcc_assert (TREE_CODE (decl) == VAR_DECL
	      || TREE_CODE (decl) == FUNCTION_DECL);

  TREE_PUBLIC (decl) = 1;

  if (SUPPORTS_ONE_ONLY)
    {
#ifdef MAKE_DECL_ONE_ONLY
      MAKE_DECL_ONE_ONLY (decl);
#endif
      DECL_COMDAT_GROUP (decl) = comdat_group;
    }
  else if (TREE_CODE (decl) == VAR_DECL
      && (DECL_INITIAL (decl) == 0 || DECL_INITIAL (decl) == error_mark_node))
    DECL_COMMON (decl) = 1;
  else
    {
      gcc_assert (TARGET_SUPPORTS_WEAK);
      DECL_WEAK (decl) = 1;
    }
}

void
init_varasm_once (void)
{
  section_htab = htab_create_ggc (31, section_entry_hash,
				  section_entry_eq, NULL);
  object_block_htab = htab_create_ggc (31, object_block_entry_hash,
				       object_block_entry_eq, NULL);
  const_desc_htab = htab_create_ggc (1009, const_desc_hash,
				     const_desc_eq, NULL);

  const_alias_set = new_alias_set ();
  shared_constant_pool = create_constant_pool ();

#ifdef TEXT_SECTION_ASM_OP
  text_section = get_unnamed_section (SECTION_CODE, output_section_asm_op,
				      TEXT_SECTION_ASM_OP);
#endif

#ifdef DATA_SECTION_ASM_OP
  data_section = get_unnamed_section (SECTION_WRITE, output_section_asm_op,
				      DATA_SECTION_ASM_OP);
#endif

#ifdef SDATA_SECTION_ASM_OP
  sdata_section = get_unnamed_section (SECTION_WRITE, output_section_asm_op,
				       SDATA_SECTION_ASM_OP);
#endif

#ifdef READONLY_DATA_SECTION_ASM_OP
  readonly_data_section = get_unnamed_section (0, output_section_asm_op,
					       READONLY_DATA_SECTION_ASM_OP);
#endif

#ifdef CTORS_SECTION_ASM_OP
  ctors_section = get_unnamed_section (0, output_section_asm_op,
				       CTORS_SECTION_ASM_OP);
#endif

#ifdef DTORS_SECTION_ASM_OP
  dtors_section = get_unnamed_section (0, output_section_asm_op,
				       DTORS_SECTION_ASM_OP);
#endif

#ifdef BSS_SECTION_ASM_OP
  bss_section = get_unnamed_section (SECTION_WRITE | SECTION_BSS,
				     output_section_asm_op,
				     BSS_SECTION_ASM_OP);
#endif

#ifdef SBSS_SECTION_ASM_OP
  sbss_section = get_unnamed_section (SECTION_WRITE | SECTION_BSS,
				      output_section_asm_op,
				      SBSS_SECTION_ASM_OP);
#endif

  tls_comm_section = get_noswitch_section (SECTION_WRITE | SECTION_BSS
					   | SECTION_COMMON, emit_tls_common);
  lcomm_section = get_noswitch_section (SECTION_WRITE | SECTION_BSS
					| SECTION_COMMON, emit_local);
  comm_section = get_noswitch_section (SECTION_WRITE | SECTION_BSS
				       | SECTION_COMMON, emit_common);

#if defined ASM_OUTPUT_ALIGNED_BSS
  bss_noswitch_section = get_noswitch_section (SECTION_WRITE | SECTION_BSS,
					       emit_bss);
#endif

  targetm.asm_out.init_sections ();

  if (readonly_data_section == NULL)
    readonly_data_section = text_section;

#ifdef ASM_OUTPUT_EXTERNAL
  pending_assemble_externals_set = pointer_set_create ();
#endif
}

enum tls_model
decl_default_tls_model (const_tree decl)
{
  enum tls_model kind;
  bool is_local;

  is_local = targetm.binds_local_p (decl);
  if (!flag_shlib)
    {
      if (is_local)
	kind = TLS_MODEL_LOCAL_EXEC;
      else
	kind = TLS_MODEL_INITIAL_EXEC;
    }

  /* Local dynamic is inefficient when we're not combining the
     parts of the address.  */
  else if (optimize && is_local)
    kind = TLS_MODEL_LOCAL_DYNAMIC;
  else
    kind = TLS_MODEL_GLOBAL_DYNAMIC;
  if (kind < flag_tls_default)
    kind = flag_tls_default;

  return kind;
}

/* Select a set of attributes for section NAME based on the properties
   of DECL and whether or not RELOC indicates that DECL's initializer
   might contain runtime relocations.

   We make the section read-only and executable for a function decl,
   read-only for a const data decl, and writable for a non-const data decl.  */

unsigned int
default_section_type_flags (tree decl, const char *name, int reloc)
{
  unsigned int flags;

  if (decl && TREE_CODE (decl) == FUNCTION_DECL)
    flags = SECTION_CODE;
  else if (decl)
    {
      enum section_category category
	= categorize_decl_for_section (decl, reloc);
      if (decl_readonly_section_1 (category))
	flags = 0;
      else if (category == SECCAT_DATA_REL_RO
	       || category == SECCAT_DATA_REL_RO_LOCAL)
	flags = SECTION_WRITE | SECTION_RELRO;
      else
	flags = SECTION_WRITE;
    }
  else
    {
      flags = SECTION_WRITE;
      if (strcmp (name, ".data.rel.ro") == 0
	  || strcmp (name, ".data.rel.ro.local") == 0)
	flags |= SECTION_RELRO;
    }

  if (decl && DECL_P (decl) && DECL_ONE_ONLY (decl))
    flags |= SECTION_LINKONCE;

  if (strcmp (name, ".vtable_map_vars") == 0)
    flags |= SECTION_LINKONCE;

  if (decl && TREE_CODE (decl) == VAR_DECL && DECL_THREAD_LOCAL_P (decl))
    flags |= SECTION_TLS | SECTION_WRITE;

  if (strcmp (name, ".bss") == 0
      || strncmp (name, ".bss.", 5) == 0
      || strncmp (name, ".gnu.linkonce.b.", 16) == 0
      || strcmp (name, ".sbss") == 0
      || strncmp (name, ".sbss.", 6) == 0
      || strncmp (name, ".gnu.linkonce.sb.", 17) == 0)
    flags |= SECTION_BSS;

  if (strcmp (name, ".tdata") == 0
      || strncmp (name, ".tdata.", 7) == 0
      || strncmp (name, ".gnu.linkonce.td.", 17) == 0)
    flags |= SECTION_TLS;

  if (strcmp (name, ".tbss") == 0
      || strncmp (name, ".tbss.", 6) == 0
      || strncmp (name, ".gnu.linkonce.tb.", 17) == 0)
    flags |= SECTION_TLS | SECTION_BSS;

  /* These three sections have special ELF types.  They are neither
     SHT_PROGBITS nor SHT_NOBITS, so when changing sections we don't
     want to print a section type (@progbits or @nobits).  If someone
     is silly enough to emit code or TLS variables to one of these
     sections, then don't handle them specially.  */
  if (!(flags & (SECTION_CODE | SECTION_BSS | SECTION_TLS))
      && (strcmp (name, ".init_array") == 0
	  || strcmp (name, ".fini_array") == 0
	  || strcmp (name, ".preinit_array") == 0))
    flags |= SECTION_NOTYPE;

  return flags;
}

/* Return true if the target supports some form of global BSS,
   either through bss_noswitch_section, or by selecting a BSS
   section in TARGET_ASM_SELECT_SECTION.  */

bool
have_global_bss_p (void)
{
  return bss_noswitch_section || targetm.have_switchable_bss_sections;
}

/* Output assembly to switch to section NAME with attribute FLAGS.
   Four variants for common object file formats.  */

void
default_no_named_section (const char *name ATTRIBUTE_UNUSED,
			  unsigned int flags ATTRIBUTE_UNUSED,
			  tree decl ATTRIBUTE_UNUSED)
{
  /* Some object formats don't support named sections at all.  The
     front-end should already have flagged this as an error.  */
  gcc_unreachable ();
}

#ifndef TLS_SECTION_ASM_FLAG
#define TLS_SECTION_ASM_FLAG 'T'
#endif

void
default_elf_asm_named_section (const char *name, unsigned int flags,
			       tree decl ATTRIBUTE_UNUSED)
{
  char flagchars[10], *f = flagchars;

  /* If we have already declared this section, we can use an
     abbreviated form to switch back to it -- unless this section is
     part of a COMDAT groups, in which case GAS requires the full
     declaration every time.  */
  if (!(HAVE_COMDAT_GROUP && (flags & SECTION_LINKONCE))
      && (flags & SECTION_DECLARED))
    {
      fprintf (asm_out_file, "\t.section\t%s\n", name);
      return;
    }

  if (!(flags & SECTION_DEBUG))
    *f++ = 'a';
  if (flags & SECTION_EXCLUDE)
    *f++ = 'e';
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
  if (flags & SECTION_TLS)
    *f++ = TLS_SECTION_ASM_FLAG;
  if (HAVE_COMDAT_GROUP && (flags & SECTION_LINKONCE))
    *f++ = 'G';
  *f = '\0';

  fprintf (asm_out_file, "\t.section\t%s,\"%s\"", name, flagchars);

  if (!(flags & SECTION_NOTYPE))
    {
      const char *type;
      const char *format;

      if (flags & SECTION_BSS)
	type = "nobits";
      else
	type = "progbits";

      format = ",@%s";
      /* On platforms that use "@" as the assembly comment character,
	 use "%" instead.  */
      if (strcmp (ASM_COMMENT_START, "@") == 0)
	format = ",%%%s";
      fprintf (asm_out_file, format, type);

      if (flags & SECTION_ENTSIZE)
	fprintf (asm_out_file, ",%d", flags & SECTION_ENTSIZE);
      if (HAVE_COMDAT_GROUP && (flags & SECTION_LINKONCE))
	{
	  if (TREE_CODE (decl) == IDENTIFIER_NODE)
	    fprintf (asm_out_file, ",%s,comdat", IDENTIFIER_POINTER (decl));
	  else
	    fprintf (asm_out_file, ",%s,comdat",
		     IDENTIFIER_POINTER (DECL_COMDAT_GROUP (decl)));
	}
    }

  putc ('\n', asm_out_file);
}

void
default_coff_asm_named_section (const char *name, unsigned int flags,
				tree decl ATTRIBUTE_UNUSED)
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
default_pe_asm_named_section (const char *name, unsigned int flags,
			      tree decl)
{
  default_coff_asm_named_section (name, flags, decl);

  if (flags & SECTION_LINKONCE)
    {
      /* Functions may have been compiled at various levels of
         optimization so we can't use `same_size' here.
         Instead, have the linker pick one.  */
      fprintf (asm_out_file, "\t.linkonce %s\n",
	       (flags & SECTION_CODE ? "discard" : "same_size"));
    }
}

/* The lame default section selector.  */

section *
default_select_section (tree decl, int reloc,
			unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED)
{
  if (DECL_P (decl))
    {
      if (decl_readonly_section (decl, reloc))
	return readonly_data_section;
    }
  else if (TREE_CODE (decl) == CONSTRUCTOR)
    {
      if (! ((flag_pic && reloc)
	     || !TREE_READONLY (decl)
	     || TREE_SIDE_EFFECTS (decl)
	     || !TREE_CONSTANT (decl)))
	return readonly_data_section;
    }
  else if (TREE_CODE (decl) == STRING_CST)
    return readonly_data_section;
  else if (! (flag_pic && reloc))
    return readonly_data_section;

  return data_section;
}

enum section_category
categorize_decl_for_section (const_tree decl, int reloc)
{
  enum section_category ret;

  if (TREE_CODE (decl) == FUNCTION_DECL)
    return SECCAT_TEXT;
  else if (TREE_CODE (decl) == STRING_CST)
    {
      if ((flag_sanitize & SANITIZE_ADDRESS)
	  && asan_protect_global (CONST_CAST_TREE (decl)))
      /* or !flag_merge_constants */
        return SECCAT_RODATA;
      else
	return SECCAT_RODATA_MERGE_STR;
    }
  else if (TREE_CODE (decl) == VAR_DECL)
    {
      if (bss_initializer_p (decl))
	ret = SECCAT_BSS;
      else if (! TREE_READONLY (decl)
	       || TREE_SIDE_EFFECTS (decl)
	       || ! TREE_CONSTANT (DECL_INITIAL (decl)))
	{
	  /* Here the reloc_rw_mask is not testing whether the section should
	     be read-only or not, but whether the dynamic link will have to
	     do something.  If so, we wish to segregate the data in order to
	     minimize cache misses inside the dynamic linker.  */
	  if (reloc & targetm.asm_out.reloc_rw_mask ())
	    ret = reloc == 1 ? SECCAT_DATA_REL_LOCAL : SECCAT_DATA_REL;
	  else
	    ret = SECCAT_DATA;
	}
      else if (reloc & targetm.asm_out.reloc_rw_mask ())
	ret = reloc == 1 ? SECCAT_DATA_REL_RO_LOCAL : SECCAT_DATA_REL_RO;
      else if (reloc || flag_merge_constants < 2
	       || ((flag_sanitize & SANITIZE_ADDRESS)
		   && asan_protect_global (CONST_CAST_TREE (decl))))
	/* C and C++ don't allow different variables to share the same
	   location.  -fmerge-all-constants allows even that (at the
	   expense of not conforming).  */
	ret = SECCAT_RODATA;
      else if (TREE_CODE (DECL_INITIAL (decl)) == STRING_CST)
	ret = SECCAT_RODATA_MERGE_STR_INIT;
      else
	ret = SECCAT_RODATA_MERGE_CONST;
    }
  else if (TREE_CODE (decl) == CONSTRUCTOR)
    {
      if ((reloc & targetm.asm_out.reloc_rw_mask ())
	  || TREE_SIDE_EFFECTS (decl)
	  || ! TREE_CONSTANT (decl))
	ret = SECCAT_DATA;
      else
	ret = SECCAT_RODATA;
    }
  else
    ret = SECCAT_RODATA;

  /* There are no read-only thread-local sections.  */
  if (TREE_CODE (decl) == VAR_DECL && DECL_THREAD_LOCAL_P (decl))
    {
      /* Note that this would be *just* SECCAT_BSS, except that there's
	 no concept of a read-only thread-local-data section.  */
      if (ret == SECCAT_BSS
	       || (flag_zero_initialized_in_bss
		   && initializer_zerop (DECL_INITIAL (decl))))
	ret = SECCAT_TBSS;
      else
	ret = SECCAT_TDATA;
    }

  /* If the target uses small data sections, select it.  */
  else if (targetm.in_small_data_p (decl))
    {
      if (ret == SECCAT_BSS)
	ret = SECCAT_SBSS;
      else if (targetm.have_srodata_section && ret == SECCAT_RODATA)
	ret = SECCAT_SRODATA;
      else
	ret = SECCAT_SDATA;
    }

  return ret;
}

static bool
decl_readonly_section_1 (enum section_category category)
{
  switch (category)
    {
    case SECCAT_RODATA:
    case SECCAT_RODATA_MERGE_STR:
    case SECCAT_RODATA_MERGE_STR_INIT:
    case SECCAT_RODATA_MERGE_CONST:
    case SECCAT_SRODATA:
      return true;
    default:
      return false;
    }
}

bool
decl_readonly_section (const_tree decl, int reloc)
{
  return decl_readonly_section_1 (categorize_decl_for_section (decl, reloc));
}

/* Select a section based on the above categorization.  */

section *
default_elf_select_section (tree decl, int reloc,
			    unsigned HOST_WIDE_INT align)
{
  const char *sname;
  switch (categorize_decl_for_section (decl, reloc))
    {
    case SECCAT_TEXT:
      /* We're not supposed to be called on FUNCTION_DECLs.  */
      gcc_unreachable ();
    case SECCAT_RODATA:
      return readonly_data_section;
    case SECCAT_RODATA_MERGE_STR:
      return mergeable_string_section (decl, align, 0);
    case SECCAT_RODATA_MERGE_STR_INIT:
      return mergeable_string_section (DECL_INITIAL (decl), align, 0);
    case SECCAT_RODATA_MERGE_CONST:
      return mergeable_constant_section (DECL_MODE (decl), align, 0);
    case SECCAT_SRODATA:
      sname = ".sdata2";
      break;
    case SECCAT_DATA:
      return data_section;
    case SECCAT_DATA_REL:
      sname = ".data.rel";
      break;
    case SECCAT_DATA_REL_LOCAL:
      sname = ".data.rel.local";
      break;
    case SECCAT_DATA_REL_RO:
      sname = ".data.rel.ro";
      break;
    case SECCAT_DATA_REL_RO_LOCAL:
      sname = ".data.rel.ro.local";
      break;
    case SECCAT_SDATA:
      sname = ".sdata";
      break;
    case SECCAT_TDATA:
      sname = ".tdata";
      break;
    case SECCAT_BSS:
      if (bss_section)
	return bss_section;
      sname = ".bss";
      break;
    case SECCAT_SBSS:
      sname = ".sbss";
      break;
    case SECCAT_TBSS:
      sname = ".tbss";
      break;
    default:
      gcc_unreachable ();
    }

  return get_named_section (decl, sname, reloc);
}

/* Construct a unique section name based on the decl name and the
   categorization performed above.  */

void
default_unique_section (tree decl, int reloc)
{
  /* We only need to use .gnu.linkonce if we don't have COMDAT groups.  */
  bool one_only = DECL_ONE_ONLY (decl) && !HAVE_COMDAT_GROUP;
  const char *prefix, *name, *linkonce;
  char *string;

  switch (categorize_decl_for_section (decl, reloc))
    {
    case SECCAT_TEXT:
      prefix = one_only ? ".t" : ".text";
      break;
    case SECCAT_RODATA:
    case SECCAT_RODATA_MERGE_STR:
    case SECCAT_RODATA_MERGE_STR_INIT:
    case SECCAT_RODATA_MERGE_CONST:
      prefix = one_only ? ".r" : ".rodata";
      break;
    case SECCAT_SRODATA:
      prefix = one_only ? ".s2" : ".sdata2";
      break;
    case SECCAT_DATA:
      prefix = one_only ? ".d" : ".data";
      break;
    case SECCAT_DATA_REL:
      prefix = one_only ? ".d.rel" : ".data.rel";
      break;
    case SECCAT_DATA_REL_LOCAL:
      prefix = one_only ? ".d.rel.local" : ".data.rel.local";
      break;
    case SECCAT_DATA_REL_RO:
      prefix = one_only ? ".d.rel.ro" : ".data.rel.ro";
      break;
    case SECCAT_DATA_REL_RO_LOCAL:
      prefix = one_only ? ".d.rel.ro.local" : ".data.rel.ro.local";
      break;
    case SECCAT_SDATA:
      prefix = one_only ? ".s" : ".sdata";
      break;
    case SECCAT_BSS:
      prefix = one_only ? ".b" : ".bss";
      break;
    case SECCAT_SBSS:
      prefix = one_only ? ".sb" : ".sbss";
      break;
    case SECCAT_TDATA:
      prefix = one_only ? ".td" : ".tdata";
      break;
    case SECCAT_TBSS:
      prefix = one_only ? ".tb" : ".tbss";
      break;
    default:
      gcc_unreachable ();
    }

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  name = targetm.strip_name_encoding (name);

  /* If we're using one_only, then there needs to be a .gnu.linkonce
     prefix to the section name.  */
  linkonce = one_only ? ".gnu.linkonce" : "";

  string = ACONCAT ((linkonce, prefix, ".", name, NULL));

  DECL_SECTION_NAME (decl) = build_string (strlen (string), string);
}

/* Like compute_reloc_for_constant, except for an RTX.  The return value
   is a mask for which bit 1 indicates a global relocation, and bit 0
   indicates a local relocation.  */

static int
compute_reloc_for_rtx_1 (rtx *xp, void *data)
{
  int *preloc = (int *) data;
  rtx x = *xp;

  switch (GET_CODE (x))
    {
    case SYMBOL_REF:
      *preloc |= SYMBOL_REF_LOCAL_P (x) ? 1 : 2;
      break;
    case LABEL_REF:
      *preloc |= 1;
      break;
    default:
      break;
    }

  return 0;
}

static int
compute_reloc_for_rtx (rtx x)
{
  int reloc;

  switch (GET_CODE (x))
    {
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      reloc = 0;
      for_each_rtx (&x, compute_reloc_for_rtx_1, &reloc);
      return reloc;

    default:
      return 0;
    }
}

section *
default_select_rtx_section (enum machine_mode mode ATTRIBUTE_UNUSED,
			    rtx x,
			    unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED)
{
  if (compute_reloc_for_rtx (x) & targetm.asm_out.reloc_rw_mask ())
    return data_section;
  else
    return readonly_data_section;
}

section *
default_elf_select_rtx_section (enum machine_mode mode, rtx x,
				unsigned HOST_WIDE_INT align)
{
  int reloc = compute_reloc_for_rtx (x);

  /* ??? Handle small data here somehow.  */

  if (reloc & targetm.asm_out.reloc_rw_mask ())
    {
      if (reloc == 1)
	return get_named_section (NULL, ".data.rel.ro.local", 1);
      else
	return get_named_section (NULL, ".data.rel.ro", 3);
    }

  return mergeable_constant_section (mode, align, 0);
}

/* Set the generally applicable flags on the SYMBOL_REF for EXP.  */

void
default_encode_section_info (tree decl, rtx rtl, int first ATTRIBUTE_UNUSED)
{
  rtx symbol;
  int flags;

  /* Careful not to prod global register variables.  */
  if (!MEM_P (rtl))
    return;
  symbol = XEXP (rtl, 0);
  if (GET_CODE (symbol) != SYMBOL_REF)
    return;

  flags = SYMBOL_REF_FLAGS (symbol) & SYMBOL_FLAG_HAS_BLOCK_INFO;
  if (TREE_CODE (decl) == FUNCTION_DECL)
    flags |= SYMBOL_FLAG_FUNCTION;
  if (targetm.binds_local_p (decl))
    flags |= SYMBOL_FLAG_LOCAL;
  if (TREE_CODE (decl) == VAR_DECL && DECL_THREAD_LOCAL_P (decl))
    flags |= DECL_TLS_MODEL (decl) << SYMBOL_FLAG_TLS_SHIFT;
  else if (targetm.in_small_data_p (decl))
    flags |= SYMBOL_FLAG_SMALL;
  /* ??? Why is DECL_EXTERNAL ever set for non-PUBLIC names?  Without
     being PUBLIC, the thing *must* be defined in this translation unit.
     Prevent this buglet from being propagated into rtl code as well.  */
  if (DECL_P (decl) && DECL_EXTERNAL (decl) && TREE_PUBLIC (decl))
    flags |= SYMBOL_FLAG_EXTERNAL;

  SYMBOL_REF_FLAGS (symbol) = flags;
}

/* By default, we do nothing for encode_section_info, so we need not
   do anything but discard the '*' marker.  */

const char *
default_strip_name_encoding (const char *str)
{
  return str + (*str == '*');
}

#ifdef ASM_OUTPUT_DEF
/* The default implementation of TARGET_ASM_OUTPUT_ANCHOR.  Define the
   anchor relative to ".", the current section position.  */

void
default_asm_output_anchor (rtx symbol)
{
  char buffer[100];

  sprintf (buffer, "*. + " HOST_WIDE_INT_PRINT_DEC,
	   SYMBOL_REF_BLOCK_OFFSET (symbol));
  ASM_OUTPUT_DEF (asm_out_file, XSTR (symbol, 0), buffer);
}
#endif

/* The default implementation of TARGET_USE_ANCHORS_FOR_SYMBOL_P.  */

bool
default_use_anchors_for_symbol_p (const_rtx symbol)
{
  section *sect;
  tree decl;

  /* Don't use anchors for mergeable sections.  The linker might move
     the objects around.  */
  sect = SYMBOL_REF_BLOCK (symbol)->sect;
  if (sect->common.flags & SECTION_MERGE)
    return false;

  /* Don't use anchors for small data sections.  The small data register
     acts as an anchor for such sections.  */
  if (sect->common.flags & SECTION_SMALL)
    return false;

  decl = SYMBOL_REF_DECL (symbol);
  if (decl && DECL_P (decl))
    {
      /* Don't use section anchors for decls that might be defined or
	 usurped by other modules.  */
      if (TREE_PUBLIC (decl) && !decl_binds_to_current_def_p (decl))
	return false;

      /* Don't use section anchors for decls that will be placed in a
	 small data section.  */
      /* ??? Ideally, this check would be redundant with the SECTION_SMALL
	 one above.  The problem is that we only use SECTION_SMALL for
	 sections that should be marked as small in the section directive.  */
      if (targetm.in_small_data_p (decl))
	return false;
    }
  return true;
}

/* Return true when RESOLUTION indicate that symbol will be bound to the
   definition provided by current .o file.  */

static bool
resolution_to_local_definition_p (enum ld_plugin_symbol_resolution resolution)
{
  return (resolution == LDPR_PREVAILING_DEF
	  || resolution == LDPR_PREVAILING_DEF_IRONLY_EXP
	  || resolution == LDPR_PREVAILING_DEF_IRONLY);
}

/* Return true when RESOLUTION indicate that symbol will be bound locally
   within current executable or DSO.  */

static bool
resolution_local_p (enum ld_plugin_symbol_resolution resolution)
{
  return (resolution == LDPR_PREVAILING_DEF
	  || resolution == LDPR_PREVAILING_DEF_IRONLY
	  || resolution == LDPR_PREVAILING_DEF_IRONLY_EXP
	  || resolution == LDPR_PREEMPTED_REG
	  || resolution == LDPR_PREEMPTED_IR
	  || resolution == LDPR_RESOLVED_IR
	  || resolution == LDPR_RESOLVED_EXEC);
}

/* Assume ELF-ish defaults, since that's pretty much the most liberal
   wrt cross-module name binding.  */

bool
default_binds_local_p (const_tree exp)
{
  return default_binds_local_p_1 (exp, flag_shlib);
}

bool
default_binds_local_p_1 (const_tree exp, int shlib)
{
  bool local_p;
  bool resolved_locally = false;
  bool resolved_to_local_def = false;

  /* With resolution file in hands, take look into resolutions.
     We can't just return true for resolved_locally symbols,
     because dynamic linking might overwrite symbols
     in shared libraries.  */
  if (TREE_CODE (exp) == VAR_DECL && TREE_PUBLIC (exp)
      && (TREE_STATIC (exp) || DECL_EXTERNAL (exp)))
    {
      varpool_node *vnode = varpool_get_node (exp);
      if (vnode && resolution_local_p (vnode->resolution))
	resolved_locally = true;
      if (vnode
	  && resolution_to_local_definition_p (vnode->resolution))
	resolved_to_local_def = true;
    }
  else if (TREE_CODE (exp) == FUNCTION_DECL && TREE_PUBLIC (exp))
    {
      struct cgraph_node *node = cgraph_get_node (exp);
      if (node
	  && resolution_local_p (node->resolution))
	resolved_locally = true;
      if (node
	  && resolution_to_local_definition_p (node->resolution))
	resolved_to_local_def = true;
    }

  /* A non-decl is an entry in the constant pool.  */
  if (!DECL_P (exp))
    local_p = true;
  /* Weakrefs may not bind locally, even though the weakref itself is always
     static and therefore local.  Similarly, the resolver for ifunc functions
     might resolve to a non-local function.
     FIXME: We can resolve the weakref case more curefuly by looking at the
     weakref alias.  */
  else if (lookup_attribute ("weakref", DECL_ATTRIBUTES (exp))
	   || (TREE_CODE (exp) == FUNCTION_DECL
	       && lookup_attribute ("ifunc", DECL_ATTRIBUTES (exp))))
    local_p = false;
  /* Static variables are always local.  */
  else if (! TREE_PUBLIC (exp))
    local_p = true;
  /* A variable is local if the user has said explicitly that it will
     be.  */
  else if ((DECL_VISIBILITY_SPECIFIED (exp)
	    || resolved_to_local_def)
	   && DECL_VISIBILITY (exp) != VISIBILITY_DEFAULT)
    local_p = true;
  /* Variables defined outside this object might not be local.  */
  else if (DECL_EXTERNAL (exp) && !resolved_locally)
    local_p = false;
  /* If defined in this object and visibility is not default, must be
     local.  */
  else if (DECL_VISIBILITY (exp) != VISIBILITY_DEFAULT)
    local_p = true;
  /* Default visibility weak data can be overridden by a strong symbol
     in another module and so are not local.  */
  else if (DECL_WEAK (exp)
	   && !resolved_locally)
    local_p = false;
  /* If PIC, then assume that any global name can be overridden by
     symbols resolved from other modules.  */
  else if (shlib)
    local_p = false;
  /* Uninitialized COMMON variable may be unified with symbols
     resolved from other modules.  */
  else if (DECL_COMMON (exp)
	   && !resolved_locally
	   && (DECL_INITIAL (exp) == NULL
	       || DECL_INITIAL (exp) == error_mark_node))
    local_p = false;
  /* Otherwise we're left with initialized (or non-common) global data
     which is of necessity defined locally.  */
  else
    local_p = true;

  return local_p;
}

/* Return true when references to DECL must bind to current definition in
   final executable.

   The condition is usually equivalent to whether the function binds to the
   current module (shared library or executable), that is to binds_local_p.
   We use this fact to avoid need for another target hook and implement
   the logic using binds_local_p and just special cases where
   decl_binds_to_current_def_p is stronger than binds_local_p.  In particular
   the weak definitions (that can be overwritten at linktime by other
   definition from different object file) and when resolution info is available
   we simply use the knowledge passed to us by linker plugin.  */
bool
decl_binds_to_current_def_p (tree decl)
{
  gcc_assert (DECL_P (decl));
  if (!targetm.binds_local_p (decl))
    return false;
  if (!TREE_PUBLIC (decl))
    return true;
  /* When resolution is available, just use it.  */
  if (TREE_CODE (decl) == VAR_DECL
      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
    {
      varpool_node *vnode = varpool_get_node (decl);
      if (vnode
	  && vnode->resolution != LDPR_UNKNOWN)
	return resolution_to_local_definition_p (vnode->resolution);
    }
  else if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      struct cgraph_node *node = cgraph_get_node (decl);
      if (node
	  && node->resolution != LDPR_UNKNOWN)
	return resolution_to_local_definition_p (node->resolution);
    }
  /* Otherwise we have to assume the worst for DECL_WEAK (hidden weaks
     binds locally but still can be overwritten), DECL_COMMON (can be merged
     with a non-common definition somewhere in the same module) or
     DECL_EXTERNAL.
     This rely on fact that binds_local_p behave as decl_replaceable_p
     for all other declaration types.  */
  if (DECL_WEAK (decl))
    return false;
  if (DECL_COMMON (decl)
      && (DECL_INITIAL (decl) == NULL
	  || DECL_INITIAL (decl) == error_mark_node))
    return false;
  if (DECL_EXTERNAL (decl))
    return false;
  return true;
}

/* A replaceable function or variable is one which may be replaced
   at link-time with an entirely different definition, provided that the
   replacement has the same type.  For example, functions declared
   with __attribute__((weak)) on most systems are replaceable.

   COMDAT functions are not replaceable, since all definitions of the
   function must be equivalent.  It is important that COMDAT functions
   not be treated as replaceable so that use of C++ template
   instantiations is not penalized.  */

bool
decl_replaceable_p (tree decl)
{
  gcc_assert (DECL_P (decl));
  if (!TREE_PUBLIC (decl) || DECL_COMDAT (decl))
    return false;
  return !decl_binds_to_current_def_p (decl);
}

/* Default function to output code that will globalize a label.  A
   target must define GLOBAL_ASM_OP or provide its own function to
   globalize a label.  */
#ifdef GLOBAL_ASM_OP
void
default_globalize_label (FILE * stream, const char *name)
{
  fputs (GLOBAL_ASM_OP, stream);
  assemble_name (stream, name);
  putc ('\n', stream);
}
#endif /* GLOBAL_ASM_OP */

/* Default function to output code that will globalize a declaration.  */
void
default_globalize_decl_name (FILE * stream, tree decl)
{
  const char *name = XSTR (XEXP (DECL_RTL (decl), 0), 0);
  targetm.asm_out.globalize_label (stream, name);
}

/* Default function to output a label for unwind information.  The
   default is to do nothing.  A target that needs nonlocal labels for
   unwind information must provide its own function to do this.  */
void
default_emit_unwind_label (FILE * stream ATTRIBUTE_UNUSED,
			   tree decl ATTRIBUTE_UNUSED,
			   int for_eh ATTRIBUTE_UNUSED,
			   int empty ATTRIBUTE_UNUSED)
{
}

/* Default function to output a label to divide up the exception table.
   The default is to do nothing.  A target that needs/wants to divide
   up the table must provide it's own function to do this.  */
void
default_emit_except_table_label (FILE * stream ATTRIBUTE_UNUSED)
{
}

/* This is how to output an internal numbered label where PREFIX is
   the class of label and LABELNO is the number within the class.  */

void
default_generate_internal_label (char *buf, const char *prefix,
				 unsigned long labelno)
{
  ASM_GENERATE_INTERNAL_LABEL (buf, prefix, labelno);
}

/* This is how to output an internal numbered label where PREFIX is
   the class of label and LABELNO is the number within the class.  */

void
default_internal_label (FILE *stream, const char *prefix,
			unsigned long labelno)
{
  char *const buf = (char *) alloca (40 + strlen (prefix));
  ASM_GENERATE_INTERNAL_LABEL (buf, prefix, labelno);
  ASM_OUTPUT_INTERNAL_LABEL (stream, buf);
}


/* The default implementation of ASM_DECLARE_CONSTANT_NAME.  */

void
default_asm_declare_constant_name (FILE *file, const char *name,
				   const_tree exp ATTRIBUTE_UNUSED,
				   HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  assemble_label (file, name);
}

/* This is the default behavior at the beginning of a file.  It's
   controlled by two other target-hook toggles.  */
void
default_file_start (void)
{
  if (targetm.asm_file_start_app_off
      && !(flag_verbose_asm || flag_debug_asm || flag_dump_rtl_in_asm))
    fputs (ASM_APP_OFF, asm_out_file);

  if (targetm.asm_file_start_file_directive)
    output_file_directive (asm_out_file, main_input_filename);
}

/* This is a generic routine suitable for use as TARGET_ASM_FILE_END
   which emits a special section directive used to indicate whether or
   not this object file needs an executable stack.  This is primarily
   a GNU extension to ELF but could be used on other targets.  */

int trampolines_created;

void
file_end_indicate_exec_stack (void)
{
  unsigned int flags = SECTION_DEBUG;
  if (trampolines_created)
    flags |= SECTION_CODE;

  switch_to_section (get_section (".note.GNU-stack", flags, NULL));
}

/* Emit a special section directive to indicate that this object file
   was compiled with -fsplit-stack.  This is used to let the linker
   detect calls between split-stack code and non-split-stack code, so
   that it can modify the split-stack code to allocate a sufficiently
   large stack.  We emit another special section if there are any
   functions in this file which have the no_split_stack attribute, to
   prevent the linker from warning about being unable to convert the
   functions if they call non-split-stack code.  */

void
file_end_indicate_split_stack (void)
{
  if (flag_split_stack)
    {
      switch_to_section (get_section (".note.GNU-split-stack", SECTION_DEBUG,
				      NULL));
      if (saw_no_split_stack)
	switch_to_section (get_section (".note.GNU-no-split-stack",
					SECTION_DEBUG, NULL));
    }
}

/* Output DIRECTIVE (a C string) followed by a newline.  This is used as
   a get_unnamed_section callback.  */

void
output_section_asm_op (const void *directive)
{
  fprintf (asm_out_file, "%s\n", (const char *) directive);
}

/* Emit assembly code to switch to section NEW_SECTION.  Do nothing if
   the current section is NEW_SECTION.  */

void
switch_to_section (section *new_section)
{
  if (in_section == new_section)
    return;

  if (new_section->common.flags & SECTION_FORGET)
    in_section = NULL;
  else
    in_section = new_section;

  switch (SECTION_STYLE (new_section))
    {
    case SECTION_NAMED:
      targetm.asm_out.named_section (new_section->named.name,
				     new_section->named.common.flags,
				     new_section->named.decl);
      break;

    case SECTION_UNNAMED:
      new_section->unnamed.callback (new_section->unnamed.data);
      break;

    case SECTION_NOSWITCH:
      gcc_unreachable ();
      break;
    }

  new_section->common.flags |= SECTION_DECLARED;
}

/* If block symbol SYMBOL has not yet been assigned an offset, place
   it at the end of its block.  */

void
place_block_symbol (rtx symbol)
{
  unsigned HOST_WIDE_INT size, mask, offset;
  struct constant_descriptor_rtx *desc;
  unsigned int alignment;
  struct object_block *block;
  tree decl;

  gcc_assert (SYMBOL_REF_BLOCK (symbol));
  if (SYMBOL_REF_BLOCK_OFFSET (symbol) >= 0)
    return;

  /* Work out the symbol's size and alignment.  */
  if (CONSTANT_POOL_ADDRESS_P (symbol))
    {
      desc = SYMBOL_REF_CONSTANT (symbol);
      alignment = desc->align;
      size = GET_MODE_SIZE (desc->mode);
    }
  else if (TREE_CONSTANT_POOL_ADDRESS_P (symbol))
    {
      decl = SYMBOL_REF_DECL (symbol);
      alignment = DECL_ALIGN (decl);
      size = get_constant_size (DECL_INITIAL (decl));
      if ((flag_sanitize & SANITIZE_ADDRESS)
	  && TREE_CODE (DECL_INITIAL (decl)) == STRING_CST
	  && asan_protect_global (DECL_INITIAL (decl)))
	size += asan_red_zone_size (size);
    }
  else
    {
      decl = SYMBOL_REF_DECL (symbol);
      alignment = get_variable_align (decl);
      size = tree_to_uhwi (DECL_SIZE_UNIT (decl));
      if ((flag_sanitize & SANITIZE_ADDRESS)
	  && asan_protect_global (decl))
	{
	  size += asan_red_zone_size (size);
	  alignment = MAX (alignment,
			   ASAN_RED_ZONE_SIZE * BITS_PER_UNIT);
	}
    }

  /* Calculate the object's offset from the start of the block.  */
  block = SYMBOL_REF_BLOCK (symbol);
  mask = alignment / BITS_PER_UNIT - 1;
  offset = (block->size + mask) & ~mask;
  SYMBOL_REF_BLOCK_OFFSET (symbol) = offset;

  /* Record the block's new alignment and size.  */
  block->alignment = MAX (block->alignment, alignment);
  block->size = offset + size;

  vec_safe_push (block->objects, symbol);
}

/* Return the anchor that should be used to address byte offset OFFSET
   from the first object in BLOCK.  MODEL is the TLS model used
   to access it.  */

rtx
get_section_anchor (struct object_block *block, HOST_WIDE_INT offset,
		    enum tls_model model)
{
  char label[100];
  unsigned int begin, middle, end;
  unsigned HOST_WIDE_INT min_offset, max_offset, range, bias, delta;
  rtx anchor;

  /* Work out the anchor's offset.  Use an offset of 0 for the first
     anchor so that we don't pessimize the case where we take the address
     of a variable at the beginning of the block.  This is particularly
     useful when a block has only one variable assigned to it.

     We try to place anchors RANGE bytes apart, so there can then be
     anchors at +/-RANGE, +/-2 * RANGE, and so on, up to the limits of
     a ptr_mode offset.  With some target settings, the lowest such
     anchor might be out of range for the lowest ptr_mode offset;
     likewise the highest anchor for the highest offset.  Use anchors
     at the extreme ends of the ptr_mode range in such cases.

     All arithmetic uses unsigned integers in order to avoid
     signed overflow.  */
  max_offset = (unsigned HOST_WIDE_INT) targetm.max_anchor_offset;
  min_offset = (unsigned HOST_WIDE_INT) targetm.min_anchor_offset;
  range = max_offset - min_offset + 1;
  if (range == 0)
    offset = 0;
  else
    {
      bias = 1 << (GET_MODE_BITSIZE (ptr_mode) - 1);
      if (offset < 0)
	{
	  delta = -(unsigned HOST_WIDE_INT) offset + max_offset;
	  delta -= delta % range;
	  if (delta > bias)
	    delta = bias;
	  offset = (HOST_WIDE_INT) (-delta);
	}
      else
	{
	  delta = (unsigned HOST_WIDE_INT) offset - min_offset;
	  delta -= delta % range;
	  if (delta > bias - 1)
	    delta = bias - 1;
	  offset = (HOST_WIDE_INT) delta;
	}
    }

  /* Do a binary search to see if there's already an anchor we can use.
     Set BEGIN to the new anchor's index if not.  */
  begin = 0;
  end = vec_safe_length (block->anchors);
  while (begin != end)
    {
      middle = (end + begin) / 2;
      anchor = (*block->anchors)[middle];
      if (SYMBOL_REF_BLOCK_OFFSET (anchor) > offset)
	end = middle;
      else if (SYMBOL_REF_BLOCK_OFFSET (anchor) < offset)
	begin = middle + 1;
      else if (SYMBOL_REF_TLS_MODEL (anchor) > model)
	end = middle;
      else if (SYMBOL_REF_TLS_MODEL (anchor) < model)
	begin = middle + 1;
      else
	return anchor;
    }

  /* Create a new anchor with a unique label.  */
  ASM_GENERATE_INTERNAL_LABEL (label, "LANCHOR", anchor_labelno++);
  anchor = create_block_symbol (ggc_strdup (label), block, offset);
  SYMBOL_REF_FLAGS (anchor) |= SYMBOL_FLAG_LOCAL | SYMBOL_FLAG_ANCHOR;
  SYMBOL_REF_FLAGS (anchor) |= model << SYMBOL_FLAG_TLS_SHIFT;

  /* Insert it at index BEGIN.  */
  vec_safe_insert (block->anchors, begin, anchor);
  return anchor;
}

/* Output the objects in BLOCK.  */

static void
output_object_block (struct object_block *block)
{
  struct constant_descriptor_rtx *desc;
  unsigned int i;
  HOST_WIDE_INT offset;
  tree decl;
  rtx symbol;

  if (!block->objects)
    return;

  /* Switch to the section and make sure that the first byte is
     suitably aligned.  */
  switch_to_section (block->sect);
  assemble_align (block->alignment);

  /* Define the values of all anchors relative to the current section
     position.  */
  FOR_EACH_VEC_SAFE_ELT (block->anchors, i, symbol)
    targetm.asm_out.output_anchor (symbol);

  /* Output the objects themselves.  */
  offset = 0;
  FOR_EACH_VEC_ELT (*block->objects, i, symbol)
    {
      /* Move to the object's offset, padding with zeros if necessary.  */
      assemble_zeros (SYMBOL_REF_BLOCK_OFFSET (symbol) - offset);
      offset = SYMBOL_REF_BLOCK_OFFSET (symbol);
      if (CONSTANT_POOL_ADDRESS_P (symbol))
	{
	  desc = SYMBOL_REF_CONSTANT (symbol);
	  output_constant_pool_1 (desc, 1);
	  offset += GET_MODE_SIZE (desc->mode);
	}
      else if (TREE_CONSTANT_POOL_ADDRESS_P (symbol))
	{
	  HOST_WIDE_INT size;
	  decl = SYMBOL_REF_DECL (symbol);
	  assemble_constant_contents (DECL_INITIAL (decl), XSTR (symbol, 0),
				      DECL_ALIGN (decl));
	  size = get_constant_size (DECL_INITIAL (decl));
	  offset += size;
	  if ((flag_sanitize & SANITIZE_ADDRESS)
	      && TREE_CODE (DECL_INITIAL (decl)) == STRING_CST
	      && asan_protect_global (DECL_INITIAL (decl)))
	    {
	      size = asan_red_zone_size (size);
	      assemble_zeros (size);
	      offset += size;
	    }
	}
      else
	{
	  HOST_WIDE_INT size;
	  decl = SYMBOL_REF_DECL (symbol);
	  assemble_variable_contents (decl, XSTR (symbol, 0), false);
	  size = tree_to_uhwi (DECL_SIZE_UNIT (decl));
	  offset += size;
	  if ((flag_sanitize & SANITIZE_ADDRESS)
	      && asan_protect_global (decl))
	    {
	      size = asan_red_zone_size (size);
	      assemble_zeros (size);
	      offset += size;
	    }
	}
    }
}

/* A htab_traverse callback used to call output_object_block for
   each member of object_block_htab.  */

static int
output_object_block_htab (void **slot, void *data ATTRIBUTE_UNUSED)
{
  output_object_block ((struct object_block *) (*slot));
  return 1;
}

/* Output the definitions of all object_blocks.  */

void
output_object_blocks (void)
{
  htab_traverse (object_block_htab, output_object_block_htab, NULL);
}

/* This function provides a possible implementation of the
   TARGET_ASM_RECORD_GCC_SWITCHES target hook for ELF targets.  When triggered
   by -frecord-gcc-switches it creates a new mergeable, string section in the
   assembler output file called TARGET_ASM_RECORD_GCC_SWITCHES_SECTION which
   contains the switches in ASCII format.

   FIXME: This code does not correctly handle double quote characters
   that appear inside strings, (it strips them rather than preserving them).
   FIXME: ASM_OUTPUT_ASCII, as defined in config/elfos.h will not emit NUL
   characters - instead it treats them as sub-string separators.  Since
   we want to emit NUL strings terminators into the object file we have to use
   ASM_OUTPUT_SKIP.  */

int
elf_record_gcc_switches (print_switch_type type, const char * name)
{
  switch (type)
    {
    case SWITCH_TYPE_PASSED:
      ASM_OUTPUT_ASCII (asm_out_file, name, strlen (name));
      ASM_OUTPUT_SKIP (asm_out_file, (unsigned HOST_WIDE_INT) 1);
      break;

    case SWITCH_TYPE_DESCRIPTIVE:
      if (name == NULL)
	{
	  /* Distinguish between invocations where name is NULL.  */
	  static bool started = false;

	  if (!started)
	    {
	      section * sec;

	      sec = get_section (targetm.asm_out.record_gcc_switches_section,
				 SECTION_DEBUG
				 | SECTION_MERGE
				 | SECTION_STRINGS
				 | (SECTION_ENTSIZE & 1),
				 NULL);
	      switch_to_section (sec);
	      started = true;
	    }
	}

    default:
      break;
    }

  /* The return value is currently ignored by the caller, but must be 0.
     For -fverbose-asm the return value would be the number of characters
     emitted into the assembler file.  */
  return 0;
}

/* Emit text to declare externally defined symbols. It is needed to
   properly support non-default visibility.  */
void
default_elf_asm_output_external (FILE *file ATTRIBUTE_UNUSED,
				 tree decl,
				 const char *name ATTRIBUTE_UNUSED)
{
  /* We output the name if and only if TREE_SYMBOL_REFERENCED is
     set in order to avoid putting out names that are never really
     used. */
  if (TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))
      && targetm.binds_local_p (decl))
    maybe_assemble_visibility (decl);
}

/* The default hook for TARGET_ASM_OUTPUT_SOURCE_FILENAME.  */

void
default_asm_output_source_filename (FILE *file, const char *name)
{
#ifdef ASM_OUTPUT_SOURCE_FILENAME
  ASM_OUTPUT_SOURCE_FILENAME (file, name);
#else
  fprintf (file, "\t.file\t");
  output_quoted_string (file, name);
  putc ('\n', file);
#endif
}

/* Output a file name in the form wanted by System V.  */

void
output_file_directive (FILE *asm_file, const char *input_name)
{
  int len;
  const char *na;

  if (input_name == NULL)
    input_name = "<stdin>";
  else
    input_name = remap_debug_filename (input_name);

  len = strlen (input_name);
  na = input_name + len;

  /* NA gets INPUT_NAME sans directory names.  */
  while (na > input_name)
    {
      if (IS_DIR_SEPARATOR (na[-1]))
	break;
      na--;
    }

  targetm.asm_out.output_source_filename (asm_file, na);
}

/* Create a DEBUG_EXPR_DECL / DEBUG_EXPR pair from RTL expression
   EXP.  */
rtx
make_debug_expr_from_rtl (const_rtx exp)
{
  tree ddecl = make_node (DEBUG_EXPR_DECL), type;
  enum machine_mode mode = GET_MODE (exp);
  rtx dval;

  DECL_ARTIFICIAL (ddecl) = 1;
  if (REG_P (exp) && REG_EXPR (exp))
    type = TREE_TYPE (REG_EXPR (exp));
  else if (MEM_P (exp) && MEM_EXPR (exp))
    type = TREE_TYPE (MEM_EXPR (exp));
  else
    type = NULL_TREE;
  if (type && TYPE_MODE (type) == mode)
    TREE_TYPE (ddecl) = type;
  else
    TREE_TYPE (ddecl) = lang_hooks.types.type_for_mode (mode, 1);
  DECL_MODE (ddecl) = mode;
  dval = gen_rtx_DEBUG_EXPR (mode);
  DEBUG_EXPR_TREE_DECL (dval) = ddecl;
  SET_DECL_RTL (ddecl, dval);
  return dval;
}

#ifdef ELF_ASCII_ESCAPES
/* Default ASM_OUTPUT_LIMITED_STRING for ELF targets.  */

void
default_elf_asm_output_limited_string (FILE *f, const char *s)
{
  int escape;
  unsigned char c;

  fputs (STRING_ASM_OP, f);
  putc ('"', f);
  while (*s != '\0')
    {
      c = *s;
      escape = ELF_ASCII_ESCAPES[c];
      switch (escape)
	{
	case 0:
	  putc (c, f);
	  break;
	case 1:
	  /* TODO: Print in hex with fast function, important for -flto. */
	  fprintf (f, "\\%03o", c);
	  break;
	default:
	  putc ('\\', f);
	  putc (escape, f);
	  break;
	}
      s++;
    }
  putc ('\"', f);
  putc ('\n', f);
}

/* Default ASM_OUTPUT_ASCII for ELF targets.  */

void
default_elf_asm_output_ascii (FILE *f, const char *s, unsigned int len)
{
  const char *limit = s + len;
  const char *last_null = NULL;
  unsigned bytes_in_chunk = 0;
  unsigned char c;
  int escape;

  for (; s < limit; s++)
    {
      const char *p;

      if (bytes_in_chunk >= 60)
	{
	  putc ('\"', f);
	  putc ('\n', f);
	  bytes_in_chunk = 0;
	}

      if (s > last_null)
	{
	  for (p = s; p < limit && *p != '\0'; p++)
	    continue;
	  last_null = p;
	}
      else
	p = last_null;

      if (p < limit && (p - s) <= (long) ELF_STRING_LIMIT)
	{
	  if (bytes_in_chunk > 0)
	    {
	      putc ('\"', f);
	      putc ('\n', f);
	      bytes_in_chunk = 0;
	    }

	  default_elf_asm_output_limited_string (f, s);
	  s = p;
	}
      else
	{
	  if (bytes_in_chunk == 0)
	    fputs (ASCII_DATA_ASM_OP "\"", f);

	  c = *s;
	  escape = ELF_ASCII_ESCAPES[c];
	  switch (escape)
	    {
	    case 0:
	      putc (c, f);
	      bytes_in_chunk++;
	      break;
	    case 1:
	      /* TODO: Print in hex with fast function, important for -flto. */
	      fprintf (f, "\\%03o", c);
	      bytes_in_chunk += 4;
	      break;
	    default:
	      putc ('\\', f);
	      putc (escape, f);
	      bytes_in_chunk += 2;
	      break;
	    }

	}
    }

  if (bytes_in_chunk > 0)
    {
      putc ('\"', f);
      putc ('\n', f);
    }
}
#endif

static GTY(()) section *elf_init_array_section;
static GTY(()) section *elf_fini_array_section;

static section *
get_elf_initfini_array_priority_section (int priority,
					 bool constructor_p)
{
  section *sec;
  if (priority != DEFAULT_INIT_PRIORITY)
    {
      char buf[18];
      sprintf (buf, "%s.%.5u", 
	       constructor_p ? ".init_array" : ".fini_array",
	       priority);
      sec = get_section (buf, SECTION_WRITE | SECTION_NOTYPE, NULL_TREE);
    }
  else
    {
      if (constructor_p)
	{
	  if (elf_init_array_section == NULL)
	    elf_init_array_section
	      = get_section (".init_array",
			     SECTION_WRITE | SECTION_NOTYPE, NULL_TREE);
	  sec = elf_init_array_section;
	}
      else
	{
	  if (elf_fini_array_section == NULL)
	    elf_fini_array_section
	      = get_section (".fini_array",
			     SECTION_WRITE | SECTION_NOTYPE, NULL_TREE);
	  sec = elf_fini_array_section;
	}
    }
  return sec;
}

/* Use .init_array section for constructors. */

void
default_elf_init_array_asm_out_constructor (rtx symbol, int priority)
{
  section *sec = get_elf_initfini_array_priority_section (priority,
							  true);
  assemble_addr_to_section (symbol, sec);
}

/* Use .fini_array section for destructors. */

void
default_elf_fini_array_asm_out_destructor (rtx symbol, int priority)
{
  section *sec = get_elf_initfini_array_priority_section (priority,
							  false);
  assemble_addr_to_section (symbol, sec);
}

/* Default TARGET_ASM_OUTPUT_IDENT hook.

   This is a bit of a cheat.  The real default is a no-op, but this
   hook is the default for all targets with a .ident directive.  */

void
default_asm_output_ident_directive (const char *ident_str)
{
  const char *ident_asm_op = "\t.ident\t";

  /* If we are still in the front end, do not write out the string
     to asm_out_file.  Instead, add a fake top-level asm statement.
     This allows the front ends to use this hook without actually
     writing to asm_out_file, to handle #ident or Pragma Ident.  */
  if (cgraph_state == CGRAPH_STATE_PARSING)
    {
      char *buf = ACONCAT ((ident_asm_op, "\"", ident_str, "\"\n", NULL));
      add_asm_node (build_string (strlen (buf), buf));
    }
  else
    fprintf (asm_out_file, "%s\"%s\"\n", ident_asm_op, ident_str);
}

#include "gt-varasm.h"
