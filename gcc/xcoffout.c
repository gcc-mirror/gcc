/* Output xcoff-format symbol table information from GNU compiler.
   Copyright (C) 1992, 94, 95, 97-99, 2000 Free Software Foundation, Inc.

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


/* Output xcoff-format symbol table data.  The main functionality is contained
   in dbxout.c.  This file implements the sdbout-like parts of the xcoff
   interface.  Many functions are very similar to their counterparts in
   sdbout.c.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "flags.h"
#include "toplev.h"
#include "output.h"
#include "ggc.h"

#ifdef XCOFF_DEBUGGING_INFO

/* This defines the C_* storage classes.  */
#include <dbxstclass.h>

#include "xcoffout.h"
#include "dbxout.h"

#if defined (USG) || !defined (HAVE_STAB_H)
#include "gstab.h"
#else
#include <stab.h>

/* This is a GNU extension we need to reference in this file.  */
#ifndef N_CATCH
#define N_CATCH 0x54
#endif
#endif

/* Line number of beginning of current function, minus one.
   Negative means not in a function or not using xcoff.  */

static int xcoff_begin_function_line = -1;
static int xcoff_inlining = 0;

/* Name of the current include file.  */

char *xcoff_current_include_file;

/* Name of the current function file.  This is the file the `.bf' is
   emitted from.  In case a line is emitted from a different file,
   (by including that file of course), then the line number will be
   absolute.  */

static char *xcoff_current_function_file;

/* Names of bss and data sections.  These should be unique names for each
   compilation unit.  */

char *xcoff_bss_section_name;
char *xcoff_private_data_section_name;
char *xcoff_read_only_section_name;

/* Last source file name mentioned in a NOTE insn.  */

char *xcoff_lastfile;

/* Macro definitions used below.  */

#define ABS_OR_RELATIVE_LINENO(LINENO)		\
((xcoff_inlining) ? (LINENO) : (LINENO) - xcoff_begin_function_line)

/* Output source line numbers via ".line" rather than ".stabd".  */
#define ASM_OUTPUT_SOURCE_LINE(FILE,LINENUM) \
  do {						\
    if (xcoff_begin_function_line >= 0)		\
      fprintf (FILE, "\t.line\t%d\n", ABS_OR_RELATIVE_LINENO (LINENUM)); \
  } while (0)

#define ASM_OUTPUT_LFB(FILE,LINENUM) \
{						\
  if (xcoff_begin_function_line == -1)		\
    {						\
      xcoff_begin_function_line = (LINENUM) - 1;\
      fprintf (FILE, "\t.bf\t%d\n", (LINENUM));	\
    }						\
  xcoff_current_function_file			\
    = (xcoff_current_include_file		\
       ? xcoff_current_include_file : main_input_filename); \
}

#define ASM_OUTPUT_LFE(FILE,LINENUM) \
  do {						\
    fprintf (FILE, "\t.ef\t%d\n", (LINENUM));	\
    xcoff_begin_function_line = -1;		\
  } while (0)

#define ASM_OUTPUT_LBB(FILE,LINENUM,BLOCKNUM) \
  fprintf (FILE, "\t.bb\t%d\n", ABS_OR_RELATIVE_LINENO (LINENUM))

#define ASM_OUTPUT_LBE(FILE,LINENUM,BLOCKNUM) \
  fprintf (FILE, "\t.eb\t%d\n", ABS_OR_RELATIVE_LINENO (LINENUM))

static void assign_type_number		PARAMS ((tree, char *, int));
static void xcoffout_block		PARAMS ((tree, int, tree));

/* Support routines for XCOFF debugging info.  */

/* Assign NUMBER as the stabx type number for the type described by NAME.
   Search all decls in the list SYMS to find the type NAME.  */

static void
assign_type_number (syms, name, number)
     tree syms;
     char *name;
     int number;
{
  tree decl;

  for (decl = syms; decl; decl = TREE_CHAIN (decl))
    if (DECL_NAME (decl)
	&& strcmp (IDENTIFIER_POINTER (DECL_NAME (decl)), name) == 0)
      {
	TREE_ASM_WRITTEN (decl) = 1;
	TYPE_SYMTAB_ADDRESS (TREE_TYPE (decl)) = number;
      }
}

/* Setup gcc primitive types to use the XCOFF built-in type numbers where
   possible.  */

void
xcoff_output_standard_types (syms)
     tree syms;
{
  /* Handle built-in C types here.  */

  assign_type_number (syms, "int", -1);
  assign_type_number (syms, "char", -2);
  assign_type_number (syms, "short int", -3);
  assign_type_number (syms, "long int", (TARGET_64BIT ? -31 : -4));
  assign_type_number (syms, "unsigned char", -5);
  assign_type_number (syms, "signed char", -6);
  assign_type_number (syms, "short unsigned int", -7);
  assign_type_number (syms, "unsigned int", -8);
  /* No such type "unsigned".  */
  assign_type_number (syms, "long unsigned int", (TARGET_64BIT ? -32 : -10));
  assign_type_number (syms, "void", -11);
  assign_type_number (syms, "float", -12);
  assign_type_number (syms, "double", -13);
  assign_type_number (syms, "long double", -14);
  /* Pascal and Fortran types run from -15 to -29.  */
  assign_type_number (syms, "wchar", -30);
  assign_type_number (syms, "long long int", -31);
  assign_type_number (syms, "long long unsigned int", -32);
  /* Additional Fortran types run from -33 to -37.  */

  /* ??? Should also handle built-in C++ and Obj-C types.  There perhaps
     aren't any that C doesn't already have.  */
}

/* Print an error message for unrecognized stab codes.  */

#define UNKNOWN_STAB(STR)	\
   do { \
     error ("Unknown stab %s: : 0x%x\n", STR, stab);	\
     fflush (stderr);	\
   } while (0)

/* Conversion routine from BSD stabs to AIX storage classes.  */

int
stab_to_sclass (stab)
     int stab;
{
  switch (stab)
    {
    case N_GSYM:
      return C_GSYM;

    case N_FNAME:
      UNKNOWN_STAB ("N_FNAME"); 
      abort();

    case N_FUN:
      return C_FUN;

    case N_STSYM:
    case N_LCSYM:
      return C_STSYM;

#ifdef N_MAIN
    case N_MAIN:
      UNKNOWN_STAB ("N_MAIN"); 
      abort ();
#endif

    case N_RSYM:
      return C_RSYM;

    case N_SSYM:
      UNKNOWN_STAB ("N_SSYM"); 
      abort ();

    case N_RPSYM:
      return C_RPSYM;

    case N_PSYM:
      return C_PSYM;
    case N_LSYM:
      return C_LSYM;
    case N_DECL:
      return C_DECL;
    case N_ENTRY:
      return C_ENTRY;

    case N_SO:
      UNKNOWN_STAB ("N_SO"); 
      abort ();

    case N_SOL:
      UNKNOWN_STAB ("N_SOL"); 
      abort ();

    case N_SLINE:
      UNKNOWN_STAB ("N_SLINE"); 
      abort ();

#ifdef N_DSLINE
    case N_DSLINE:
      UNKNOWN_STAB ("N_DSLINE"); 
      abort ();
#endif

#ifdef N_BSLINE
    case N_BSLINE:
      UNKNOWN_STAB ("N_BSLINE"); 
      abort ();
#endif
#if 0
      /* This has the same value as N_BSLINE.  */
    case N_BROWS:
      UNKNOWN_STAB ("N_BROWS"); 
      abort ();
#endif

#ifdef N_BINCL
    case N_BINCL:
      UNKNOWN_STAB ("N_BINCL"); 
      abort ();
#endif

#ifdef N_EINCL
    case N_EINCL:
      UNKNOWN_STAB ("N_EINCL"); 
      abort ();
#endif

#ifdef N_EXCL
    case N_EXCL:
      UNKNOWN_STAB ("N_EXCL"); 
      abort ();
#endif

    case N_LBRAC:
      UNKNOWN_STAB ("N_LBRAC"); 
      abort ();

    case N_RBRAC:
      UNKNOWN_STAB ("N_RBRAC"); 
      abort ();

    case N_BCOMM:
      return C_BCOMM;
    case N_ECOMM:
      return C_ECOMM;
    case N_ECOML:
      return C_ECOML;

    case N_LENG:
      UNKNOWN_STAB ("N_LENG"); 
      abort ();

    case N_PC:
      UNKNOWN_STAB ("N_PC"); 
      abort ();

#ifdef N_M2C
    case N_M2C:
      UNKNOWN_STAB ("N_M2C"); 
      abort ();
#endif

#ifdef N_SCOPE
    case N_SCOPE:
      UNKNOWN_STAB ("N_SCOPE"); 
      abort ();
#endif

    case N_CATCH:
      UNKNOWN_STAB ("N_CATCH"); 
      abort ();

    default:
      UNKNOWN_STAB ("default"); 
      abort ();
  }
}

/* Output debugging info to FILE to switch to sourcefile FILENAME.
   INLINE_P is true if this is from an inlined function.  */

void
xcoffout_source_file (file, filename, inline_p)
     FILE *file;
     char *filename;
     int inline_p;
{
  if (filename
      && (xcoff_lastfile == 0 || strcmp (filename, xcoff_lastfile)
	  || (inline_p && ! xcoff_inlining)
	  || (! inline_p && xcoff_inlining)))
    {
      if (xcoff_current_include_file)
	{
	  fprintf (file, "\t.ei\t");
	  output_quoted_string (file, xcoff_current_include_file);
	  fprintf (file, "\n");
	  xcoff_current_include_file = NULL;
	}
	xcoff_inlining=inline_p;
      if (strcmp (main_input_filename, filename) || inline_p)
	{
	  fprintf (file, "\t.bi\t");
	  output_quoted_string (file, filename);
	  fprintf (file, "\n");
	  xcoff_current_include_file = filename;
	}
      
      if (!xcoff_lastfile)
	ggc_add_string_root (&xcoff_lastfile, 1);

      xcoff_lastfile = filename;
    }
}

/* Output a line number symbol entry into output stream FILE,
   for source file FILENAME and line number NOTE.  */

void
xcoffout_source_line (file, filename, note)
     FILE *file;
     char *filename;
     rtx note;
{
  xcoffout_source_file (file, filename, RTX_INTEGRATED_P (note));

  ASM_OUTPUT_SOURCE_LINE (file, NOTE_LINE_NUMBER (note));
}

/* Output the symbols defined in block number DO_BLOCK.
   Set NEXT_BLOCK_NUMBER to 0 before calling.

   This function works by walking the tree structure of blocks,
   counting blocks until it finds the desired block.  */

static int do_block = 0;

static int next_block_number;

static void
xcoffout_block (block, depth, args)
     register tree block;
     int depth;
     tree args;
{
  while (block)
    {
      /* Ignore blocks never expanded or otherwise marked as real.  */
      if (TREE_USED (block))
	{
	  /* When we reach the specified block, output its symbols.  */
	  if (next_block_number == do_block)
	    {
	      /* Output the syms of the block.  */
	      if (debug_info_level != DINFO_LEVEL_TERSE || depth == 0)
		dbxout_syms (BLOCK_VARS (block));
	      if (args)
		dbxout_reg_parms (args);

	      /* We are now done with the block.  Don't go to inner blocks.  */
	      return;
	    }
	  /* If we are past the specified block, stop the scan.  */
	  else if (next_block_number >= do_block)
	    return;

	  next_block_number++;

	  /* Output the subblocks.  */
	  xcoffout_block (BLOCK_SUBBLOCKS (block), depth + 1, NULL_TREE);
	}
      block = BLOCK_CHAIN (block);
    }
}

/* Describe the beginning of an internal block within a function.
   Also output descriptions of variables defined in this block.

   N is the number of the block, by order of beginning, counting from 1,
   and not counting the outermost (function top-level) block.
   The blocks match the BLOCKs in DECL_INITIAL (current_function_decl),
   if the count starts at 0 for the outermost one.  */

void
xcoffout_begin_block (file, line, n)
     FILE *file;
     int line;
     int n;
{
  tree decl = current_function_decl;

  
  /* The IBM AIX compiler does not emit a .bb for the function level scope,
     so we avoid it here also.  */
  if (n != 1)
    ASM_OUTPUT_LBB (file, line, n);

  do_block = n;
  next_block_number = 0;
  xcoffout_block (DECL_INITIAL (decl), 0, DECL_ARGUMENTS (decl));
}

/* Describe the end line-number of an internal block within a function.  */

void
xcoffout_end_block (file, line, n)
     FILE *file;
     int line;
     int n;
{
  if (n != 1)
    ASM_OUTPUT_LBE (file, line, n);
}

/* Called at beginning of function (before prologue).
   Declare function as needed for debugging.  */

void
xcoffout_declare_function (file, decl, name)
     FILE *file;
     tree decl;
     char *name;
{
  char *n = name;
  int i;

  if (*n == '*')
    n++;
  else
    for (i = 0; name[i]; ++i)
      {
	if (name[i] == '[')
	  {
	    n = (char *) alloca (i + 1);
	    strncpy (n, name, i);
	    n[i] = '\0';
	    break;
	  }
      }

  /* Any pending .bi or .ei must occur before the .function pseudo op.
     Otherwise debuggers will think that the function is in the previous
     file and/or at the wrong line number.  */
  xcoffout_source_file (file, DECL_SOURCE_FILE (decl), 0);
  dbxout_symbol (decl, 0);

  /* .function NAME, TOP, MAPPING, TYPE, SIZE
     16 and 044 are placeholders for backwards compatibility */
  fprintf (file, "\t.function .%s,.%s,16,044,FE..%s-.%s\n", n, n, n, n);
}

/* Called at beginning of function body (after prologue).
   Record the function's starting line number, so we can output
   relative line numbers for the other lines.
   Record the file name that this function is contained in.  */

void
xcoffout_begin_function (file, last_linenum)
     FILE *file;
     int last_linenum;
{
  ASM_OUTPUT_LFB (file, last_linenum);
  dbxout_parms (DECL_ARGUMENTS (current_function_decl));

  /* Emit the symbols for the outermost BLOCK's variables.  sdbout.c does this
     in sdbout_begin_block, but there is no guarantee that there will be any
     inner block 1, so we must do it here.  This gives a result similar to
     dbxout, so it does make some sense.  */
  do_block = 0;
  next_block_number = 0;
  xcoffout_block (DECL_INITIAL (current_function_decl), 0,
		  DECL_ARGUMENTS (current_function_decl));

  ASM_OUTPUT_SOURCE_LINE (file, last_linenum);
}

/* Called at end of function (before epilogue).
   Describe end of outermost block.  */

void
xcoffout_end_function (file, last_linenum)
     FILE *file;
     int last_linenum;
{
  ASM_OUTPUT_LFE (file, last_linenum);
}

/* Output xcoff info for the absolute end of a function.
   Called after the epilogue is output.  */

void
xcoffout_end_epilogue (file)
     FILE *file;
{
  /* We need to pass the correct function size to .function, otherwise,
     the xas assembler can't figure out the correct size for the function
     aux entry.  So, we emit a label after the last instruction which can
     be used by the .function pseudo op to calculate the function size.  */

  char *fname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
  if (*fname == '*')
    ++fname;
  fprintf (file, "FE..");
  ASM_OUTPUT_LABEL (file, fname);
}
#endif /* XCOFF_DEBUGGING_INFO */
