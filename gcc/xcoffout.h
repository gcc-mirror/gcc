/* XCOFF definitions.  These are needed in dbxout.c, final.c,
   and xcoffout.h.  */

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

/* For static variables, output code to define the start of a static block.  */

#define DBX_STATIC_BLOCK_START(ASMFILE,CODE)				\
{									\
  if ((CODE) == N_STSYM)						\
    fprintf ((ASMFILE), "\t.bs\t%s[RW]\n", xcoff_private_data_section_name);\
  else if ((CODE) == N_LCSYM)						\
    fprintf ((ASMFILE), "\t.bs\t%s\n", xcoff_bss_section_name);		\
}

/* For static variables, output code to define the end of a static block.  */

#define DBX_STATIC_BLOCK_END(ASMFILE,CODE)				\
{									\
  if (current_sym_code == N_STSYM || current_sym_code == N_LCSYM)	\
    fprintf (asmfile, "\t.es\n");					\
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
      char *_p;							\
      for (_p = XSTR (current_sym_addr, 0); *_p != '[' && *_p; _p++) \
	fprintf (asmfile, "%c", *_p);				\
    }								\
  else if (current_sym_addr)					\
    output_addr_const (asmfile, current_sym_addr);		\
  else if (current_sym_code == N_GSYM)				\
    fprintf (asmfile, "%s", IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (SYM))); \
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

/* The line number of the beginning of the current function.
   xcoffout.c needs this so that it can output relative linenumbers.  */

extern int xcoff_begin_function_line;

/* Name of the current include file.  */

extern char *xcoff_current_include_file;

/* Name of the current function file.  This is the file the `.bf' is
   emitted from.  In case a line is emitted from a different file,
   (by including that file of course), then the line number will be
   absolute.  */

extern char *xcoff_current_function_file;

/* Names of bss and data sections.  These should be unique names for each
   compilation unit.  */

extern char *xcoff_bss_section_name;
extern char *xcoff_private_data_section_name;
extern char *xcoff_read_only_section_name;

/* Don't write out path name for main source file.  */
#define DBX_OUTPUT_MAIN_SOURCE_DIRECTORY(FILE,FILENAME)

/* Write out main source file name using ".file" rather than ".stabs".  */
#define DBX_OUTPUT_MAIN_SOURCE_FILENAME(FILE,FILENAME) \
  fprintf (FILE, "\t.file\t\"%s\"\n", FILENAME);

#define ABS_OR_RELATIVE_LINENO(LINENO)		\
((xcoff_current_include_file			\
  && xcoff_current_include_file != xcoff_current_function_file)	\
 ? (LINENO) : (LINENO) - xcoff_begin_function_line)

/* Output source line numbers via ".line" rather than ".stabd".  */
#define ASM_OUTPUT_SOURCE_LINE(FILE,LINENUM) \
  do {						\
    if (xcoff_begin_function_line >= 0)		\
      fprintf (FILE, "\t.line\t%d\n", ABS_OR_RELATIVE_LINENO (LINENUM)); \
  } while (0)

/* We don't want to emit source file names in dbx style. */
#define DBX_OUTPUT_SOURCE_FILENAME(FILE, FILENAME)	\
{							\
  if (xcoff_current_include_file)			\
    fprintf (FILE, "\t.ei\t\"%s\"\n", xcoff_current_include_file);\
  if (strcmp (main_input_filename, FILENAME))		\
    {							\
      fprintf (FILE, "\t.bi\t\"%s\"\n", FILENAME);	\
      xcoff_current_include_file = FILENAME;		\
    }							\
  else							\
    xcoff_current_include_file = NULL;			\
}

/* If we are still in an include file, its end must be marked.  */
#define DBX_OUTPUT_MAIN_SOURCE_FILE_END(FILE, FILENAME)	\
{							\
  if (xcoff_current_include_file)			\
    {							\
      fprintf ((FILE), "\t.ei\t\"%s\"\n",		\
	       xcoff_current_include_file);		\
      xcoff_current_include_file = NULL;		\
    }							\
}

/* Do not break .stabs pseudos into continuations.  */
#define DBX_CONTIN_LENGTH 0

/* Don't try to use the `x' type-cross-reference character in DBX data.
   Also has the consequence of putting each struct, union or enum
   into a separate .stabs, containing only cross-refs to the others.  */
#define DBX_NO_XREFS
