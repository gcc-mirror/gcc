/* XCOFF definitions.  These are needed in dbxout.c, final.c,
   and xcoffout.h. 
   Copyright (C) 1998 Free Software Foundation, Inc.

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


#define ASM_STABS_OP ".stabx"

/* Tags and typedefs are C_DECL in XCOFF, not C_LSYM.  */

#define DBX_TYPE_DECL_STABS_CODE N_DECL

/* Use the XCOFF predefined type numbers.  */

/* ??? According to metin, typedef stabx must go in text control section,
   but he did not make this changes everywhere where such typedef stabx
   can be emitted, so it is really needed or not?  */

#define DBX_OUTPUT_STANDARD_TYPES(SYMS)		\
{						\
  text_section ();				\
  xcoff_output_standard_types (SYMS);		\
}

/* Any type with a negative type index has already been output.  */

#define DBX_TYPE_DEFINED(TYPE) (TYPE_SYMTAB_ADDRESS (TYPE) < 0)

/* Must use N_STSYM for static const variables (those in the text section)
   instead of N_FUN.  */

#define DBX_STATIC_CONST_VAR_CODE N_STSYM

/* For static variables, output code to define the start of a static block.

   ??? The IBM rs6000/AIX assembler has a bug that causes bss block debug
   info to be occasionally lost.  A simple example is this:
	int a; static int b;
   The commands `gcc -g -c tmp.c; dump -t tmp.o' gives
[10]	m   0x00000016         1     0    0x8f  0x0000            .bs
[11]	m   0x00000000         1     0    0x90  0x0000            .es
...
[21]	m   0x00000000        -2     0    0x85  0x0000            b:S-1
   which is wrong.  The `b:S-1' must be between the `.bs' and `.es'.
   We can apparently work around the problem by forcing the text section
   (even if we are already in the text section) immediately before outputting
   the `.bs'.  This should be fixed in the next major AIX release (3.3?).  */

#define DBX_STATIC_BLOCK_START(ASMFILE,CODE)				\
{									\
  if ((CODE) == N_STSYM)						\
    fprintf ((ASMFILE), "\t.bs\t%s[RW]\n", xcoff_private_data_section_name);\
  else if ((CODE) == N_LCSYM)						\
    {									\
      fprintf ((ASMFILE), "%s\n", TEXT_SECTION_ASM_OP);			\
      fprintf ((ASMFILE), "\t.bs\t%s\n", xcoff_bss_section_name);	\
    }									\
}

/* For static variables, output code to define the end of a static block.  */

#define DBX_STATIC_BLOCK_END(ASMFILE,CODE)				\
{									\
  if ((CODE) == N_STSYM || (CODE) == N_LCSYM)				\
    fputs ("\t.es\n", (ASMFILE));					\
}

/* We must use N_RPYSM instead of N_RSYM for register parameters.  */

#define DBX_REGPARM_STABS_CODE N_RPSYM

/* We must use 'R' instead of 'P' for register parameters.  */

#define DBX_REGPARM_STABS_LETTER 'R'

/* Define our own finish symbol function, since xcoff stabs have their
   own different format.  */

#define DBX_FINISH_SYMBOL(SYM)					\
{								\
  if (current_sym_addr && current_sym_code == N_FUN)		\
    fprintf (asmfile, "\",.");					\
  else								\
    fprintf (asmfile, "\",");					\
  /* If we are writing a function name, we must ensure that	\
     there is no storage-class suffix on the name.  */		\
  if (current_sym_addr && current_sym_code == N_FUN		\
      && GET_CODE (current_sym_addr) == SYMBOL_REF)		\
    {								\
      char *_p = XSTR (current_sym_addr, 0);			\
      if (*_p == '*')						\
	fprintf (asmfile, "%s", _p+1);				\
      else							\
        for (; *_p != '[' && *_p; _p++)				\
	  fprintf (asmfile, "%c", *_p);				\
    }								\
  else if (current_sym_addr)					\
    output_addr_const (asmfile, current_sym_addr);		\
  else if (current_sym_code == N_GSYM)				\
    assemble_name (asmfile, XSTR (XEXP (DECL_RTL (sym), 0), 0)); \
  else								\
    fprintf (asmfile, "%d", current_sym_value);			\
  fprintf (asmfile, ",%d,0\n", stab_to_sclass (current_sym_code)); \
}

/* These are IBM XCOFF extensions we need to reference in dbxout.c
   and xcoffout.c.  */

/* AIX XCOFF uses this for typedefs.  This can have any value, since it is
   only used for translation into a C_DECL storage class.  */
#ifndef N_DECL
#define N_DECL 0x8c
#endif
/* AIX XCOFF uses this for parameters passed in registers.  This can have
   any value, since it is only used for translation into a C_RPSYM storage
   class.  */
#ifndef N_RPSYM
#define N_RPSYM 0x8e
#endif

/* Name of the current include file.  */

extern char *xcoff_current_include_file;

/* Names of bss and data sections.  These should be unique names for each
   compilation unit.  */

extern char *xcoff_bss_section_name;
extern char *xcoff_private_data_section_name;
extern char *xcoff_read_only_section_name;

/* Last source file name mentioned in a NOTE insn.  */

extern char *xcoff_lastfile;

/* Don't write out path name for main source file.  */
#define DBX_OUTPUT_MAIN_SOURCE_DIRECTORY(FILE,FILENAME)

/* Write out main source file name using ".file" rather than ".stabs".
   We don't actually do this here, because the assembler gets confused if there
   is more than one .file directive.  ASM_FILE_START in config/rs6000/rs6000.h
   is already emitting a .file directory, so we don't output one here also.
   Initialize xcoff_lastfile.  */
#define DBX_OUTPUT_MAIN_SOURCE_FILENAME(FILE,FILENAME) \
  xcoff_lastfile = (FILENAME)

/* If we are still in an include file, its end must be marked.  */
#define DBX_OUTPUT_MAIN_SOURCE_FILE_END(FILE, FILENAME)	\
{							\
  if (xcoff_current_include_file)			\
    {							\
      fputs ("\t.ei\t", (FILE));			\
      output_quoted_string ((FILE), xcoff_current_include_file);	\
      putc ('\n', (FILE));				\
      xcoff_current_include_file = NULL;		\
    }							\
}

/* Do not break .stabs pseudos into continuations.  */
#define DBX_CONTIN_LENGTH 0

/* Don't try to use the `x' type-cross-reference character in DBX data.
   Also has the consequence of putting each struct, union or enum
   into a separate .stabs, containing only cross-refs to the others.  */
#define DBX_NO_XREFS

/* We must put stabs in the text section.  If we don't the assembler
   won't handle them correctly; it will sometimes put stabs where gdb
   can't find them.  */

#define DEBUG_SYMS_TEXT

/* Prototype functions in xcoffout.c. */

extern int stab_to_sclass			PARAMS ((int));
#ifdef BUFSIZ
extern void xcoffout_begin_function		PARAMS ((FILE *, int));
extern void xcoffout_begin_block		PARAMS ((FILE *, int, int));
extern void xcoffout_end_epilogue		PARAMS ((FILE *));
extern void xcoffout_end_function		PARAMS ((FILE *, int));
extern void xcoffout_end_block			PARAMS ((FILE *, int, int));
#endif /* BUFSIZ */

#ifdef TREE_CODE
extern void xcoff_output_standard_types		PARAMS ((tree));
#ifdef BUFSIZ
extern void xcoffout_declare_function		PARAMS ((FILE *, tree, char *));
#endif /* BUFSIZ */
#endif /* TREE_CODE */

#ifdef RTX_CODE
#ifdef BUFSIZ
extern void xcoffout_source_line		PARAMS ((FILE *, char *, rtx));
#endif /* BUFSIZ */
#endif /* RTX_CODE */
