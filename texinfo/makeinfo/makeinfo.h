/* makeinfo.h -- Declarations for Makeinfo.
   $Id: makeinfo.h,v 1.1.1.2 1998/03/22 20:43:08 law Exp $

   Copyright (C) 1996, 97 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

   Written by Brian Fox (bfox@ai.mit.edu). */

/* Why, oh why, did I ever listen to rms when he said:
   "Don't make lots of small files, just make one big one!"  I've
   regretted it ever since with this program, and with readline.
   bfox@ai.mit.edu Thu Jul 11 07:54:32 1996 */

#if !defined (MAKEINFO_H)
#define MAKEINFO_H

#if defined (COMPILING_MAKEINFO)
#  define DECLARE(type, var, init) type var = init
#else
#  define DECLARE(type, var, init)  extern type var
#endif

enum insertion_type
{
  cartouche, defcv, deffn, defivar, defmac, defmethod,
  defop, defopt, defspec, deftp, deftypefn, deftypefun,
  deftypemethod, deftypevar, deftypevr, defun, defvar,
  defvr, detailmenu, direntry, display, enumerate, example,
  flushleft, flushright, format, ftable, group, ifclear,
  ifinfo, ifnothtml, ifnottex, ifset, itemize, lisp, menu,
  multitable, quotation, smallexample, smalllisp, table, vtable,
  bad_type
};

DECLARE (int, insertion_level, 0);

#if defined (COMPILING_MAKEINFO)
char *insertion_type_names[] =
{
  "cartouche", "defcv", "deffn", "defivar", "defmac", "defmethod",
  "defop", "defopt", "defspec", "deftp", "deftypefn", "deftypefun",
  "deftypemethod", "deftypevar", "deftypevr", "defun", "defvar",
  "defvr", "detailmenu", "direntry", "display", "enumerate", "example",
  "flushleft", "flushright", "format", "ftable", "group", "ifclear",
  "ifinfo", "ifnothtml", "ifnottex", "ifset", "itemize", "lisp", "menu",
  "multitable", "quotation", "smallexample", "smalllisp", "table", "vtable",
  "bad_type"
};
#endif

typedef struct istack_elt
{
  struct istack_elt *next;
  char *item_function;
  char *filename;
  int line_number;
  int filling_enabled;
  int indented_fill;
  enum insertion_type insertion;
  int inhibited;
  int in_fixed_width_font;
} INSERTION_ELT;

DECLARE (INSERTION_ELT *, insertion_stack, (INSERTION_ELT *)NULL);

/* Current output stream. */
DECLARE (FILE *, output_stream, (FILE *)NULL);

/* Output paragraph buffer. */
DECLARE (unsigned char *, output_paragraph, (unsigned char *)NULL);

/* Offset into OUTPUT_PARAGRAPH. */
DECLARE (int, output_paragraph_offset, 0);

/* The output paragraph "cursor" horizontal position. */
DECLARE (int, output_column, 0);

/* Non-zero means output_paragraph contains text. */
DECLARE (int, paragraph_is_open, 0);

/* The amount of indentation to apply at the start of each line. */
DECLARE (int, current_indent, 0);

/* nonzero if we are currently processing a multitable command */
DECLARE (int, multitable_active, 0);

/* The column at which long lines are broken. */
DECLARE (int, fill_column, 72);

/* The current input file state. */
DECLARE (char *, input_filename, (char *)NULL);
DECLARE (char *, input_text, (char *)NULL);
DECLARE (int, size_of_input_text, 0);
DECLARE (int, input_text_offset, 0);
DECLARE (int, line_number, 0);

#define curchar() input_text[input_text_offset]
/* **************************************************************** */
/*                                                                  */
/*                            Global Defines                        */
/*                                                                  */
/* **************************************************************** */

/* Error levels */
#define NO_ERROR 0
#define SYNTAX   2
#define FATAL    4

/* C's standard macros don't check to make sure that the characters being
   changed are within range.  So I have to check explicitly. */

/* GNU Library doesn't have toupper().  Until GNU gets this fixed, I will
   have to do it. */
#ifndef toupper
#define toupper(c) ((c) - 32)
#endif

#define coerce_to_upper(c) ((islower(c) ? toupper(c) : (c)))
#define coerce_to_lower(c) ((isupper(c) ? tolower(c) : (c)))

#define control_character_bit 0x40 /* %01000000, must be off. */
#define meta_character_bit 0x080/* %10000000, must be on.  */
#define CTL(c) ((c) & (~control_character_bit))
#define UNCTL(c) coerce_to_upper(((c)|control_character_bit))
#define META(c) ((c) | (meta_character_bit))
#define UNMETA(c) ((c) & (~meta_character_bit))

#define whitespace(c) (((c) == '\t') || ((c) == ' '))
#define sentence_ender(c) ((c) == '.' || (c) == '?' || (c) == '!')
#define cr_or_whitespace(c) (((c) == '\t') || ((c) == ' ') || ((c) == '\n'))

#ifndef isletter
#define isletter(c) (((c) >= 'A' && (c) <= 'Z') || ((c) >= 'a' && (c) <= 'z'))
#endif

#ifndef isupper
#define isupper(c) ((c) >= 'A' && (c) <= 'Z')
#endif

#ifndef isdigit
#define isdigit(c)  ((c) >= '0' && (c) <= '9')
#endif

#ifndef digit_value
#define digit_value(c) ((c) - '0')
#endif

#define member(c, s) (strchr (s, c) != NULL)

#define COMMAND_PREFIX '@'

/* Stuff for splitting large files. */
#define SPLIT_SIZE_THRESHOLD 70000  /* What's good enough for Stallman... */
#define DEFAULT_SPLIT_SIZE 50000    /* Is probably good enough for me. */

DECLARE (int, splitting, 1);    /* Defaults to true for now. */

typedef void COMMAND_FUNCTION (); /* So I can say COMMAND_FUNCTION *foo; */

#define command_char(c) ((!whitespace(c)) && \
                         ((c) != '\n') && \
                         ((c) != '{') && \
                         ((c) != '}') && \
                         ((c) != '='))

#define skip_whitespace() \
     while ((input_text_offset != size_of_input_text) && \
             whitespace (curchar())) \
       input_text_offset++

#define skip_whitespace_and_newlines() \
  do { \
   while ((input_text_offset != size_of_input_text) && \
          (whitespace (curchar ()) || (curchar () == '\n'))) \
      { \
         if (curchar () == '\n') \
           line_number++; \
         input_text_offset++; \
      } \
   } while (0)

#endif /* !MAKEINFO_H */
