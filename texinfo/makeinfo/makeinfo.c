/* Makeinfo -- convert Texinfo source files into Info files.
   $Id: makeinfo.c,v 1.11 1998/07/06 21:58:00 law Exp $

   Copyright (C) 1987, 92, 93, 94, 95, 96, 97, 98
   Free Software Foundation, Inc.

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

   Makeinfo was authored by Brian Fox (bfox@ai.mit.edu). */

/* Indent #pragma so that older Cpp's don't try to parse it. */
#ifdef _AIX
 #pragma alloca
#endif /* _AIX */

int major_version = 1;
int minor_version = 68;

#include "system.h"
#include "getopt.h"

#ifdef TM_IN_SYS_TIME
#include <sys/time.h>
#else
#include <time.h>
#endif /* !TM_IN_SYS_TIME */

#ifdef __GNUC__
# undef alloca
# define alloca __builtin_alloca
#else
# ifdef HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifndef _AIX
char *alloca ();
#  endif
# endif
#endif

/* We'd like to take advantage of _doprnt if it's around, a la error.c,
   but then we'd have no VA_SPRINTF.  */
#if HAVE_VPRINTF
# if __STDC__
#  include <stdarg.h>
#  define VA_START(args, lastarg) va_start(args, lastarg)
# else
#  include <varargs.h>
#  define VA_START(args, lastarg) va_start(args)
# endif
# define VA_FPRINTF(file, fmt, ap) vfprintf (file, fmt, ap)
# define VA_SPRINTF(str, fmt, ap) vsprintf (str, fmt, ap)
#else /* not HAVE_VPRINTF */
# define VA_START(args, lastarg)
# define va_alist a1, a2, a3, a4, a5, a6, a7, a8
# define va_dcl char *a1, *a2, *a3, *a4, *a5, *a6, *a7, *a8;
# define va_end(args)
#endif

/* You can change some of the behavior of Makeinfo by changing the
   following defines: */

/* Define INDENT_PARAGRAPHS_IN_TABLE if you want the paragraphs which
   appear within an @table, @ftable, or @itemize environment to have
   standard paragraph indentation.  Without this, such paragraphs have
   no starting indentation. */
/* #define INDENT_PARAGRAPHS_IN_TABLE */

/* Define DEFAULT_INDENTATION_INCREMENT as an integer which is the amount
   that @example should increase indentation by.  This incremement is used
   for all insertions which indent the enclosed text. */
#define DEFAULT_INDENTATION_INCREMENT 5

/* Define PARAGRAPH_START_INDENT to be the amount of indentation that
   the first lines of paragraphs receive by default, where no other
   value has been specified.  Users can change this value on the command
   line, with the --paragraph-indent option, or within the texinfo file,
   with the @paragraphindent command. */
#define PARAGRAPH_START_INDENT 3

/* Define DEFAULT_PARAGRAPH_SPACING as the number of blank lines that you
   wish to appear between paragraphs.  A value of 1 creates a single blank
   line between paragraphs.  Paragraphs are defined by 2 or more consecutive
   newlines in the input file (i.e., one or more blank lines). */
#define DEFAULT_PARAGRAPH_SPACING 1

/* Define HAVE_MACROS to enable the macro facility of Texinfo.  Using this
   facility, users can create their own command procedures with
   arguments.   Must always be defined.  */
#define HAVE_MACROS


#define COMPILING_MAKEINFO
#include "makeinfo.h"

/* Nonzero means that we are currently hacking the insides of an
   insertion which would use a fixed width font. */
static int in_fixed_width_font = 0;

/* Nonzero means that start_paragraph () MUST be called before we pay
   any attention to close_paragraph () calls. */
int must_start_paragraph = 0;

/* Nonzero means a string is in execution, as opposed to a file. */
static int executing_string = 0;

/* Nonzero means a macro string is in execution, as opposed to a file. */
static int me_executing_string = 0;

#if defined (HAVE_MACROS)
/* If non-NULL, this is an output stream to write the full macro expansion
   of the input text to.  The result is another texinfo file, but
   missing @include, @infoinclude, @macro, and macro invocations.  Instead,
   all of the text is placed within the file. */
FILE *macro_expansion_output_stream = (FILE *)NULL;
char *macro_expansion_filename;

/* Here is a structure used to remember input text strings and offsets
   within them. */
typedef struct {
  char *pointer;                /* Pointer to the input text. */
  int offset;                   /* Offset of the last character output. */
} ITEXT;

static ITEXT **itext_info = (ITEXT **)NULL;
static int itext_size = 0;

/* Nonzero means to inhibit writing macro expansions to the output
   stream, because it has already been written. */
int me_inhibit_expansion = 0;

ITEXT *remember_itext ();
void forget_itext (), me_append_before_this_command ();
void append_to_expansion_output (), write_region_to_macro_output ();
void maybe_write_itext (), me_execute_string ();
#endif /* HAVE_MACROS */


/* **************************************************************** */
/*                                                                  */
/*                          Global Variables                        */
/*                                                                  */
/* **************************************************************** */

/* Global pointer to argv[0]. */
char *progname;

/* Return nonzero if STRING is the text at input_text + input_text_offset,
   else zero. */
#define looking_at(string) \
  (strncmp (input_text + input_text_offset, string, strlen (string)) == 0)

/* And writing to the output. */

/* The output file name. */
char *output_filename = (char *)NULL;
char *pretty_output_filename;

/* Name of the output file that the user elected to pass on the command line.
   Such a name overrides any name found with the @setfilename command. */
char *command_output_filename = (char *)NULL;

/* A colon separated list of directories to search for files included
   with @include.  This can be controlled with the `-I' option to makeinfo. */
char *include_files_path = (char *)NULL;

/* Position in the output file. */
int output_position;

#define INITIAL_PARAGRAPH_SPACE 5000
int paragraph_buffer_len = INITIAL_PARAGRAPH_SPACE;

/* Nonzero indicates that filling will take place on long lines. */
int filling_enabled = 1;

/* Nonzero means that words are not to be split, even in long lines.  This
   gets changed for cm_w (). */
int non_splitting_words = 0;

/* Nonzero indicates that filling a line also indents the new line. */
int indented_fill = 0;

/* The amount of indentation to add at the starts of paragraphs.
   0 means don't change existing indentation at paragraph starts.
   > 0 is amount to indent new paragraphs by.
   < 0 means indent to column zero by removing indentation if necessary.

   This is normally zero, but some people prefer paragraph starts to be
   somewhat more indented than paragraph bodies.  A pretty value for
   this is 3. */
int paragraph_start_indent = PARAGRAPH_START_INDENT;

/* Nonzero means that the use of paragraph_start_indent is inhibited.
   @example uses this to line up the left columns of the example text.
   A negative value for this variable is incremented each time it is used.
   @noindent uses this to inhibit indentation for a single paragraph.  */
int inhibit_paragraph_indentation = 0;

/* Indentation that is pending insertion.  We have this for hacking lines
   which look blank, but contain whitespace.  We want to treat those as
   blank lines. */
int pending_indent = 0;

/* The amount that indentation increases/decreases by. */
int default_indentation_increment = DEFAULT_INDENTATION_INCREMENT;

/* Nonzero indicates that indentation is temporarily turned off. */
int no_indent = 1;

/* Nonzero means forcing output text to be flushright. */
int force_flush_right = 0;

/* Nonzero means that the footnote style for this document was set on
   the command line, which overrides any other settings. */
int footnote_style_preset = 0;

/* Nonzero means that we automatically number footnotes that have no
   specified marker. */
int number_footnotes = 1;

/* The current footnote number in this node.  Each time a new node is
   started this is reset to 1. */
int current_footnote_number = 1;

/* Command name in the process of being hacked. */
char *command;

/* The index in our internal command table of the currently
   executing command. */
int command_index;

/* A search string which is used to find a line defining a node. */
char node_search_string[] =
  { '\n', COMMAND_PREFIX, 'n', 'o', 'd', 'e', ' ', 0 };

/* A search string which is used to find a line defining a menu. */
char menu_search_string[] =
  { '\n', COMMAND_PREFIX, 'm', 'e', 'n', 'u', 0 };

/* A search string which is used to find the first @setfilename. */
char setfilename_search[] =
  { COMMAND_PREFIX,
      's', 'e', 't', 'f', 'i', 'l', 'e', 'n', 'a', 'm', 'e', 0 };

/* A stack of file information records.  If a new file is read in with
   "@input", we remember the old input file state on this stack. */
typedef struct fstack
{
  struct fstack *next;
  char *filename;
  char *text;
  int size;
  int offset;
  int line_number;
} FSTACK;

FSTACK *filestack = (FSTACK *) NULL;

/* Stuff for nodes. */
/* The current nodes node name. */
char *current_node = (char *)NULL;

/* The current nodes section level. */
int current_section = 0;

/* The filename of the current input file.  This is never freed. */
char *node_filename = (char *)NULL;

/* What we remember for each node. */
typedef struct tentry
{
  struct tentry *next_ent;
  char *node;           /* name of this node. */
  char *prev;           /* name of "Prev:" for this node. */
  char *next;           /* name of "Next:" for this node. */
  char *up;             /* name of "Up:" for this node.   */
  int position;         /* output file position of this node. */
  int line_no;          /* defining line in source file. */
  char *filename;       /* The file that this node was found in. */
  int touched;          /* Nonzero means this node has been referenced. */
  int flags;            /* Room for growth.  Right now, contains 1 bit. */
} TAG_ENTRY;

/* If node-a has a "Next" for node-b, but node-b has no "Prev" for node-a,
   we turn on this flag bit in node-b's tag entry.  This means that when
   it is time to validate node-b, we don't report an additional error
   if there was no "Prev" field. */
#define PREV_ERROR 0x1
#define NEXT_ERROR 0x2
#define UP_ERROR   0x4
#define NO_WARN    0x8
#define IS_TOP     0x10

TAG_ENTRY *tag_table = (TAG_ENTRY *) NULL;

/* Values for calling handle_variable_internal (). */
#define SET     1
#define CLEAR   2
#define IFSET   3
#define IFCLEAR 4

#if defined (HAVE_MACROS)
#define ME_RECURSE      0x01
#define ME_QUOTE_ARG    0x02

/* Macro definitions for user-defined commands. */
typedef struct {
  char *name;                   /* Name of the macro. */
  char **arglist;               /* Args to replace when executing. */
  char *body;                   /* Macro body. */
  char *source_file;            /* File where this macro is defined. */
  int source_lineno;            /* Line number within FILENAME. */
  int inhibited;                /* Nonzero means make find_macro () fail. */
  int flags;                    /* ME_RECURSE, ME_QUOTE_ARG, etc. */
} MACRO_DEF;

void add_macro (), execute_macro ();
MACRO_DEF *find_macro (), *delete_macro ();
#endif /* HAVE_MACROS */

/* Menu reference, *note reference, and validation hacking. */

/* The various references that we know about. */
enum reftype
{
  menu_reference, followed_reference
};

/* A structure to remember references with.  A reference to a node is
   either an entry in a menu, or a cross-reference made with [px]ref. */
typedef struct node_ref
{
  struct node_ref *next;
  char *node;                   /* Name of node referred to. */
  char *containing_node;        /* Name of node containing this reference. */
  int line_no;                  /* Line number where the reference occurs. */
  int section;                  /* Section level where the reference occurs. */
  char *filename;               /* Name of file where the reference occurs. */
  enum reftype type;            /* Type of reference, either menu or note. */
} NODE_REF;

/* The linked list of such structures. */
NODE_REF *node_references = (NODE_REF *) NULL;

/* Flag which tells us whether to examine menu lines or not. */
int in_menu = 0;

/* Flag which tells us how to examine menu lines. */
int in_detailmenu = 0;

/* Nonzero means that we have seen "@top" once already. */
int top_node_seen = 0;

/* Nonzero means that we have seen a non-"@top" node already. */
int non_top_node_seen = 0;

/* Flags controlling the operation of the program. */

/* Default is to remove output if there were errors.  */
int force = 0;

/* Default is to notify users of bad choices. */
int print_warnings = 1;

/* Default is to check node references. */
int validating = 1;

/* Nonzero means do not output "Node: Foo" for node separations. */
int no_headers = 0;

/* Number of errors that we tolerate on a given fileset. */
int max_error_level = 100;

/* Maximum number of references to a single node before complaining. */
int reference_warning_limit = 1000;

/* Nonzero means print out information about what is going on when it
   is going on. */
int verbose_mode = 0;

/* Nonzero means to be relaxed about the input file.  This is useful when
   we can successfully format the input, but it doesn't strictly match our
   somewhat pedantic ideas of correctness.  Right now, it affects what
   @table and @itemize do without arguments. */
int allow_lax_format = 0;

/* The list of commands that we hack in texinfo.  Each one
   has an associated function.  When the command is encountered in the
   text, the associated function is called with START as the argument.
   If the function expects arguments in braces, it remembers itself on
   the stack.  When the corresponding close brace is encountered, the
   function is called with END as the argument. */

#define START 0
#define END 1

typedef struct brace_element
{
  struct brace_element *next;
  COMMAND_FUNCTION *proc;
  int pos, line;
  int in_fixed_width_font;
} BRACE_ELEMENT;

BRACE_ELEMENT *brace_stack = (BRACE_ELEMENT *) NULL;

extern void do_multitable ();

void print_version_info ();
void usage ();
void push_node_filename (), pop_node_filename ();
void remember_error (), flush_file_stack ();
void convert_from_stream (), convert_from_file (), convert_from_loaded_file ();
void init_internals (), init_paragraph (), init_brace_stack ();
void init_insertion_stack (), init_indices ();
void init_tag_table (), write_tag_table (), write_tag_table_internal ();
void validate_file (), validate_other_references (), split_file ();
void free_node_references (), do_enumeration (), handle_variable ();
void handle_variable_internal ();
void normalize_node_name ();
void undefindex (), top_defindex (), gen_defindex ();
void define_user_command ();
void free_pending_notes (), output_pending_notes ();

char **get_brace_args ();
char *expansion ();
int array_len ();
void free_array ();
static int end_of_sentence_p ();
static void isolate_nodename ();
void reader_loop (), read_command ();
void remember_brace (), remember_brace_1 ();
void pop_and_call_brace (), discard_braces ();
void add_word (), add_char (), insert (), flush_output ();
void insert_string ();
void close_paragraph_with_lines (), close_paragraph ();
void ignore_blank_line ();
void do_flush_right_indentation (), discard_insertions ();
void start_paragraph (), indent ();
#if defined (VA_FPRINTF) && __STDC__
/* Unfortunately we must use prototypes if we are to use <stdarg.h>.  */
void add_word_args (char *, ...);
void execute_string (char *, ...);
#else
void add_word_args ();
void execute_string ();
#endif /* will not use prototypes */

void insert_self (), insert_space (), cm_ignore_line ();

void
  cm_TeX (), cm_asterisk (), cm_bullet (), cm_cite (),
  cm_code (), cm_copyright (), cm_ctrl (), cm_dfn (), cm_dircategory (),
  cm_direntry (), cm_dots (), cm_emph (), cm_enddots (),
  cm_kbd (), cm_key (), cm_no_op (), cm_no_op_line_arg (),
  cm_not_fixed_width (), cm_strong (), cm_var_sc (), cm_w (), cm_image ();

/* Sectioning.  */
void
  cm_chapter (), cm_unnumbered (), cm_appendix (), cm_top (),
  cm_section (), cm_unnumberedsec (), cm_appendixsec (),
  cm_subsection (), cm_unnumberedsubsec (), cm_appendixsubsec (),
  cm_subsubsection (), cm_unnumberedsubsubsec (), cm_appendixsubsubsec (),
  cm_heading (), cm_chapheading (), cm_subheading (), cm_subsubheading (),
  cm_majorheading (), cm_raisesections (), cm_lowersections ();

/* All @def... commands map to cm_defun, most accent commands map to
   cm_accent, most non-English letters map to cm_special_char.  */
void cm_defun (), cm_accent (), cm_special_char (), cm_dotless ();

void
  cm_node (), cm_menu (), cm_xref (), cm_ftable (), cm_vtable (), cm_pxref (),
  cm_inforef (), cm_uref (), cm_email (), cm_quotation (),
  cm_display (), cm_itemize (),
  cm_enumerate (), cm_tab (), cm_table (), cm_itemx (), cm_noindent (),
  cm_setfilename (), cm_br (), cm_sp (), cm_page (), cm_group (),
  cm_center (), cm_include (), cm_bye (), cm_item (), cm_end (),
  cm_ifinfo (), cm_ifnothtml (), cm_ifnottex (), cm_kindex (), cm_cindex (),
  cm_findex (), cm_pindex (), cm_vindex (), cm_tindex (),
  cm_synindex (), cm_printindex (), cm_minus (), cm_footnote (),
  cm_example (), cm_smallexample (), cm_lisp (), cm_format (), cm_exdent (),
  cm_defindex (), cm_defcodeindex (), cm_result (), cm_expansion (),
  cm_equiv (), cm_print (), cm_error (), cm_point (), cm_today (),
  cm_flushleft (), cm_flushright (), cm_smalllisp (), cm_finalout (),
  cm_cartouche (), cm_detailmenu (), cm_multitable ();

/* Conditionals. */
void cm_set (), cm_clear (), cm_ifset (), cm_ifclear ();
void cm_value (), cm_ifeq ();

#if defined (HAVE_MACROS)
/* Define a user-defined command which is simple substitution. */
void cm_macro (), cm_unmacro ();
#endif /* HAVE_MACROS */

/* Options. */
void cm_paragraphindent (), cm_footnotestyle ();

/* Internals. */
void command_name_condition (), misplaced_brace (), cm_obsolete (),
     cm_ideprecated ();

typedef struct
{
  char *name;
  COMMAND_FUNCTION *proc;
  int argument_in_braces;
} COMMAND;

/* Stuff for defining commands on the fly. */
COMMAND **user_command_array = (COMMAND **) NULL;
int user_command_array_len = 0;

#define NO_BRACE_ARGS 0
#define BRACE_ARGS 1

static COMMAND command_table[] = {
  { "\t", insert_space, NO_BRACE_ARGS },
  { "\n", insert_space, NO_BRACE_ARGS },
  { " ", insert_self, NO_BRACE_ARGS },
  { "!", insert_self, NO_BRACE_ARGS },
  { "\"", insert_self, NO_BRACE_ARGS },
  { "'", insert_self, NO_BRACE_ARGS },
  { "*", cm_asterisk, NO_BRACE_ARGS },
  { ",", cm_accent, BRACE_ARGS },
  { "-", cm_no_op, NO_BRACE_ARGS },
  { ".", insert_self, NO_BRACE_ARGS },
  { ":", cm_no_op, NO_BRACE_ARGS },
  { "=", insert_self, NO_BRACE_ARGS },
  { "?", insert_self, NO_BRACE_ARGS },
  { "@", insert_self, NO_BRACE_ARGS },
  { "^", insert_self, NO_BRACE_ARGS },
  { "`", insert_self, NO_BRACE_ARGS },
  { "{", insert_self, NO_BRACE_ARGS },
  { "|", cm_no_op, NO_BRACE_ARGS },
  { "}", insert_self, NO_BRACE_ARGS },
  { "~", insert_self, NO_BRACE_ARGS },
  { "AA", insert_self, BRACE_ARGS },
  { "AE", insert_self, BRACE_ARGS },
  { "H", cm_accent, BRACE_ARGS },
  { "L", cm_special_char, BRACE_ARGS },
  { "O", cm_special_char, BRACE_ARGS },
  { "OE", insert_self, BRACE_ARGS },
  { "TeX", cm_TeX, BRACE_ARGS },
  { "aa", insert_self, BRACE_ARGS },
  { "ae", insert_self, BRACE_ARGS },
  { "appendix", cm_appendix, NO_BRACE_ARGS },
  { "appendixsection", cm_appendixsec, NO_BRACE_ARGS },
  { "appendixsec", cm_appendixsec, NO_BRACE_ARGS },
  { "appendixsubsec", cm_appendixsubsec, NO_BRACE_ARGS },
  { "appendixsubsubsec", cm_appendixsubsubsec, NO_BRACE_ARGS },
  { "asis", cm_no_op, BRACE_ARGS },
  { "b", cm_not_fixed_width, BRACE_ARGS },
  { "bullet", cm_bullet, BRACE_ARGS },
  { "bye", cm_bye, NO_BRACE_ARGS },
  { "c", cm_ignore_line, NO_BRACE_ARGS },
  { "cartouche", cm_cartouche, NO_BRACE_ARGS },
  { "center", cm_center, NO_BRACE_ARGS },
  { "centerchap", cm_unnumbered, NO_BRACE_ARGS },
  { "chapheading", cm_chapheading, NO_BRACE_ARGS },
  { "chapter", cm_chapter, NO_BRACE_ARGS },
  { "cindex", cm_cindex, NO_BRACE_ARGS },
  { "cite", cm_cite, BRACE_ARGS },
  { "clear", cm_clear, NO_BRACE_ARGS },
  { "code", cm_code, BRACE_ARGS },
  { "comment", cm_ignore_line, NO_BRACE_ARGS },
  { "contents", cm_no_op, NO_BRACE_ARGS },
  { "copyright", cm_copyright, BRACE_ARGS },
  { "ctrl", cm_obsolete, BRACE_ARGS },
  { "defcodeindex", cm_defcodeindex, NO_BRACE_ARGS },
  { "defindex", cm_defindex, NO_BRACE_ARGS },
/* The `def' commands. */
  { "defcv", cm_defun, NO_BRACE_ARGS },
  { "defcvx", cm_defun, NO_BRACE_ARGS },
  { "deffn", cm_defun, NO_BRACE_ARGS },
  { "deffnx", cm_defun, NO_BRACE_ARGS },
  { "defivar", cm_defun, NO_BRACE_ARGS },
  { "defivarx", cm_defun, NO_BRACE_ARGS },
  { "defmac", cm_defun, NO_BRACE_ARGS },
  { "defmacx", cm_defun, NO_BRACE_ARGS },
  { "defmethod", cm_defun, NO_BRACE_ARGS },
  { "defmethodx", cm_defun, NO_BRACE_ARGS },
  { "defop", cm_defun, NO_BRACE_ARGS },
  { "defopt", cm_defun, NO_BRACE_ARGS },
  { "defoptx", cm_defun, NO_BRACE_ARGS },
  { "defopx", cm_defun, NO_BRACE_ARGS },
  { "defspec", cm_defun, NO_BRACE_ARGS },
  { "defspecx", cm_defun, NO_BRACE_ARGS },
  { "deftp", cm_defun, NO_BRACE_ARGS },
  { "deftpx", cm_defun, NO_BRACE_ARGS },
  { "deftypefn", cm_defun, NO_BRACE_ARGS },
  { "deftypefnx", cm_defun, NO_BRACE_ARGS },
  { "deftypefun", cm_defun, NO_BRACE_ARGS },
  { "deftypefunx", cm_defun, NO_BRACE_ARGS },
  { "deftypemethod", cm_defun, NO_BRACE_ARGS },
  { "deftypemethodx", cm_defun, NO_BRACE_ARGS },
  { "deftypevar", cm_defun, NO_BRACE_ARGS },
  { "deftypevarx", cm_defun, NO_BRACE_ARGS },
  { "deftypevr", cm_defun, NO_BRACE_ARGS },
  { "deftypevrx", cm_defun, NO_BRACE_ARGS },
  { "defun", cm_defun, NO_BRACE_ARGS },
  { "defunx", cm_defun, NO_BRACE_ARGS },
  { "defvar", cm_defun, NO_BRACE_ARGS },
  { "defvarx", cm_defun, NO_BRACE_ARGS },
  { "defvr", cm_defun, NO_BRACE_ARGS },
  { "defvrx", cm_defun, NO_BRACE_ARGS },
/* The end of the `def' commands. */
  { "detailmenu", cm_detailmenu, NO_BRACE_ARGS },
  { "dfn", cm_dfn, BRACE_ARGS },
  { "dircategory", cm_dircategory, NO_BRACE_ARGS },
  { "direntry", cm_direntry, NO_BRACE_ARGS },
  { "display", cm_display, NO_BRACE_ARGS },
  { "dmn", cm_no_op, BRACE_ARGS },
  { "dotaccent", cm_accent, BRACE_ARGS },
  { "dotless", cm_dotless, BRACE_ARGS },
  { "dots", cm_dots, BRACE_ARGS },
  { "email", cm_email, BRACE_ARGS },
  { "emph", cm_emph, BRACE_ARGS },
  { "end", cm_end, NO_BRACE_ARGS },
  { "enddots", cm_enddots, BRACE_ARGS },
  { "enumerate", cm_enumerate, NO_BRACE_ARGS },
  { "equiv", cm_equiv, BRACE_ARGS },
  { "error", cm_error, BRACE_ARGS },
  { "example", cm_example, NO_BRACE_ARGS },
  { "exclamdown", cm_special_char, BRACE_ARGS },
  { "exdent", cm_exdent, NO_BRACE_ARGS },
  { "expansion", cm_expansion, BRACE_ARGS },
  { "file", cm_code, BRACE_ARGS },
  { "finalout", cm_no_op, NO_BRACE_ARGS },
  { "findex", cm_findex, NO_BRACE_ARGS },
  { "flushleft", cm_flushleft, NO_BRACE_ARGS },
  { "flushright", cm_flushright, NO_BRACE_ARGS },
  { "footnote", cm_footnote, NO_BRACE_ARGS}, /* self-arg eater */
  { "footnotestyle", cm_footnotestyle, NO_BRACE_ARGS },
  { "format", cm_format, NO_BRACE_ARGS },
  { "ftable", cm_ftable, NO_BRACE_ARGS },
  { "group", cm_group, NO_BRACE_ARGS },
  { "heading", cm_heading, NO_BRACE_ARGS },
  { "headings", cm_ignore_line, NO_BRACE_ARGS },
  { "html", command_name_condition, NO_BRACE_ARGS },
  { "hyphenation", cm_no_op, BRACE_ARGS },
  { "i", cm_not_fixed_width, BRACE_ARGS },
  { "ifclear", cm_ifclear, NO_BRACE_ARGS },
  { "ifeq", cm_ifeq, NO_BRACE_ARGS },
  { "ifhtml", command_name_condition, NO_BRACE_ARGS },
  { "ifinfo", cm_ifinfo, NO_BRACE_ARGS },
  { "ifnothtml", cm_ifnothtml, NO_BRACE_ARGS },
  { "ifnotinfo", command_name_condition, NO_BRACE_ARGS },
  { "ifnottex", cm_ifnottex, NO_BRACE_ARGS },
  { "ifset", cm_ifset, NO_BRACE_ARGS },
  { "iftex", command_name_condition, NO_BRACE_ARGS },
  { "ignore", command_name_condition, NO_BRACE_ARGS },
  { "image", cm_image, BRACE_ARGS },
  { "include", cm_include, NO_BRACE_ARGS },
  { "inforef", cm_inforef, BRACE_ARGS },
  { "item", cm_item, NO_BRACE_ARGS },
  { "itemize", cm_itemize, NO_BRACE_ARGS },
  { "itemx", cm_itemx, NO_BRACE_ARGS },
  { "kbd", cm_kbd, BRACE_ARGS },
  { "kbdinputstyle", cm_no_op_line_arg, NO_BRACE_ARGS },
  { "key", cm_key, BRACE_ARGS },
  { "kindex", cm_kindex, NO_BRACE_ARGS },
  { "l", cm_special_char, BRACE_ARGS },
  { "lisp", cm_lisp, NO_BRACE_ARGS },
  { "lowersections", cm_lowersections, NO_BRACE_ARGS },
  { "macro", cm_macro, NO_BRACE_ARGS },
  { "majorheading", cm_majorheading, NO_BRACE_ARGS },
  { "math", cm_no_op, BRACE_ARGS },
  { "menu", cm_menu, NO_BRACE_ARGS },
  { "minus", cm_minus, BRACE_ARGS },
  { "multitable", cm_multitable, NO_BRACE_ARGS },
  { "need", cm_ignore_line, NO_BRACE_ARGS },
  { "node", cm_node, NO_BRACE_ARGS },
  { "noindent", cm_noindent, NO_BRACE_ARGS },
  { "nwnode", cm_node, NO_BRACE_ARGS },
  { "o", cm_special_char, BRACE_ARGS },
  { "oe", insert_self, BRACE_ARGS },
  { "page", cm_no_op, NO_BRACE_ARGS },
  { "paragraphindent", cm_paragraphindent, NO_BRACE_ARGS },
  { "pindex", cm_pindex, NO_BRACE_ARGS },
  { "point", cm_point, BRACE_ARGS },
  { "pounds", cm_special_char, BRACE_ARGS },
  { "print", cm_print, BRACE_ARGS },
  { "printindex", cm_printindex, NO_BRACE_ARGS },
  { "pxref", cm_pxref, BRACE_ARGS },
  { "questiondown", cm_special_char, BRACE_ARGS },
  { "quotation", cm_quotation, NO_BRACE_ARGS },
  { "r", cm_not_fixed_width, BRACE_ARGS },
  { "raisesections", cm_raisesections, NO_BRACE_ARGS },
  { "ref", cm_xref, BRACE_ARGS },
  { "refill", cm_no_op, NO_BRACE_ARGS },
  { "result", cm_result, BRACE_ARGS },
  { "ringaccent", cm_accent, BRACE_ARGS },
  { "samp", cm_code, BRACE_ARGS },
  { "sc", cm_var_sc, BRACE_ARGS },
  { "section", cm_section, NO_BRACE_ARGS },
  { "set", cm_set, NO_BRACE_ARGS },
  { "setchapternewpage", cm_ignore_line, NO_BRACE_ARGS },
  { "setchapterstyle", cm_obsolete, NO_BRACE_ARGS },
  { "setfilename", cm_setfilename, NO_BRACE_ARGS },
  { "settitle", cm_ignore_line, NO_BRACE_ARGS },
  { "shortcontents", cm_no_op, NO_BRACE_ARGS },
  { "shorttitlepage", cm_ignore_line, NO_BRACE_ARGS },
  { "smallbook", cm_ignore_line, NO_BRACE_ARGS },
  { "smallexample", cm_smallexample, NO_BRACE_ARGS },
  { "smalllisp", cm_smalllisp, NO_BRACE_ARGS },
  { "sp", cm_sp, NO_BRACE_ARGS },
  { "ss", insert_self, BRACE_ARGS },
  { "strong", cm_strong, BRACE_ARGS },
  { "subheading", cm_subheading, NO_BRACE_ARGS },
  { "subsection", cm_subsection, NO_BRACE_ARGS },
  { "subsubheading", cm_subsubheading, NO_BRACE_ARGS },
  { "subsubsection", cm_subsubsection, NO_BRACE_ARGS },
  { "summarycontents", cm_no_op, NO_BRACE_ARGS },
  { "syncodeindex", cm_synindex, NO_BRACE_ARGS },
  { "synindex", cm_synindex, NO_BRACE_ARGS },
  { "t", cm_no_op, BRACE_ARGS },
  { "tab", cm_tab, NO_BRACE_ARGS },
  { "table", cm_table, NO_BRACE_ARGS },
  { "tex", command_name_condition, NO_BRACE_ARGS },
  { "tieaccent", cm_accent, BRACE_ARGS },
  { "tindex", cm_tindex, NO_BRACE_ARGS },
  { "titlefont", cm_not_fixed_width, BRACE_ARGS },
  { "titlepage", command_name_condition, NO_BRACE_ARGS },
  { "today", cm_today, BRACE_ARGS },
  { "top", cm_top, NO_BRACE_ARGS  },
  { "u", cm_accent, BRACE_ARGS },
  { "ubaraccent", cm_accent, BRACE_ARGS },
  { "udotaccent", cm_accent, BRACE_ARGS },
#if defined (HAVE_MACROS)
  { "unmacro", cm_unmacro, NO_BRACE_ARGS },
#endif
  { "unnumbered", cm_unnumbered, NO_BRACE_ARGS },
  { "unnumberedsec", cm_unnumberedsec, NO_BRACE_ARGS },
  { "unnumberedsubsec", cm_unnumberedsubsec, NO_BRACE_ARGS },
  { "unnumberedsubsubsec", cm_unnumberedsubsubsec, NO_BRACE_ARGS },
  { "uref", cm_uref, BRACE_ARGS },
  { "url", cm_code, BRACE_ARGS },
  { "v", cm_accent, BRACE_ARGS },
  { "value", cm_value, BRACE_ARGS },
  { "var", cm_var_sc, BRACE_ARGS },
  { "vindex", cm_vindex, NO_BRACE_ARGS },
  { "vtable", cm_vtable, NO_BRACE_ARGS },
  { "w", cm_w, BRACE_ARGS },
  { "xref", cm_xref, BRACE_ARGS },

  /* Deprecated commands.  These used to be for italics.  */
  { "iappendix", cm_ideprecated, NO_BRACE_ARGS },
  { "iappendixsec", cm_ideprecated, NO_BRACE_ARGS },
  { "iappendixsection", cm_ideprecated, NO_BRACE_ARGS },
  { "iappendixsubsec", cm_ideprecated, NO_BRACE_ARGS },
  { "iappendixsubsubsec", cm_ideprecated, NO_BRACE_ARGS },
  { "ichapter", cm_ideprecated, NO_BRACE_ARGS },
  { "isection", cm_ideprecated, NO_BRACE_ARGS },
  { "isubsection", cm_ideprecated, NO_BRACE_ARGS },
  { "isubsubsection", cm_ideprecated, NO_BRACE_ARGS },
  { "iunnumbered", cm_ideprecated, NO_BRACE_ARGS },
  { "iunnumberedsec", cm_ideprecated, NO_BRACE_ARGS },
  { "iunnumberedsubsec", cm_ideprecated, NO_BRACE_ARGS },
  { "iunnumberedsubsubsec", cm_ideprecated, NO_BRACE_ARGS },

  /* Now @include does what this was used to. */
  { "infoinclude", cm_obsolete, NO_BRACE_ARGS },
  { "titlespec", cm_obsolete, NO_BRACE_ARGS },

  { NULL, NULL, NO_BRACE_ARGS }
};

struct option long_options[] =
{
  { "error-limit", 1, 0, 'e' },                 /* formerly -el */
  { "fill-column", 1, 0, 'f' },                 /* formerly -fc */
  { "footnote-style", 1, 0, 's' },              /* formerly -ft */
  { "force", 0, 0, 'F' },                       /* do not remove output */
  { "no-headers", 0, &no_headers, 1 },          /* do not output Node: foo */
  { "no-pointer-validate", 0, &validating, 0 }, /* formerly -nv */
  { "no-validate", 0, &validating, 0 },         /* formerly -nv */
  { "no-split", 0, &splitting, 0 },             /* formerly -ns */
  { "no-warn", 0, &print_warnings, 0 },         /* formerly -nw */
  { "macro-expand", 1, 0, 'E' },
  { "number-footnotes", 0, &number_footnotes, 1 },
  { "no-number-footnotes", 0, &number_footnotes, 0 },
  { "output", 1, 0, 'o' },
  { "paragraph-indent", 1, 0, 'p' },            /* formerly -pi */
  { "reference-limit", 1, 0, 'r' },             /* formerly -rl */
  { "verbose", 0, &verbose_mode, 1 },           /* formerly -verbose */
  { "help", 0, 0, 'h' },
  { "version", 0, 0, 'V' },
  {NULL, 0, NULL, 0}
};

/* **************************************************************** */
/*                                                                  */
/*                      Error Handling                              */
/*                                                                  */
/* **************************************************************** */

/* Number of errors encountered. */
int errors_printed = 0;

/* Print the last error gotten from the file system. */
int
fs_error (filename)
     char *filename;
{
  remember_error ();
  perror (filename);
  return (0);
}

/* Print an error message, and return false. */
void
#if defined (VA_FPRINTF) && __STDC__
error (char *format, ...)
#else
error (format, va_alist)
     char *format;
     va_dcl
#endif
{
#ifdef VA_FPRINTF
  va_list ap;
#endif

  remember_error ();

  VA_START (ap, format);
#ifdef VA_FPRINTF
  VA_FPRINTF (stderr, format, ap);
#else
  fprintf (stderr, format, a1, a2, a3, a4, a5, a6, a7, a8);
#endif /* not VA_FPRINTF */
  va_end (ap);

  putc ('\n', stderr);
}

/* Just like error (), but print the line number as well. */
void
#if defined (VA_FPRINTF) && __STDC__
line_error (char *format, ...)
#else
line_error (format, va_alist)
   char *format;
   va_dcl
#endif
{
#ifdef VA_FPRINTF
  va_list ap;
#endif

  remember_error ();
  fprintf (stderr, "%s:%d: ", input_filename, line_number);

  VA_START (ap, format);
#ifdef VA_FPRINTF
  VA_FPRINTF (stderr, format, ap);
#else
  fprintf (stderr, format, a1, a2, a3, a4, a5, a6, a7, a8);
#endif /* not VA_FPRINTF */
  va_end (ap);

  fprintf (stderr, ".\n");
}

void
#if defined (VA_FPRINTF) && __STDC__
warning (char *format, ...)
#else
warning (format, va_alist)
     char *format;
     va_dcl
#endif
{
#ifdef VA_FPRINTF
  va_list ap;
#endif

  if (print_warnings)
    {
      fprintf (stderr, _("%s:%d: warning: "), input_filename, line_number);

      VA_START (ap, format);
#ifdef VA_FPRINTF
      VA_FPRINTF (stderr, format, ap);
#else
      fprintf (stderr, format, a1, a2, a3, a4, a5, a6, a7, a8);
#endif /* not VA_FPRINTF */
      va_end (ap);

      fprintf (stderr, ".\n");
    }
}


/* Remember that an error has been printed.  If more than
   max_error_level have been printed, then exit the program. */
void
remember_error ()
{
  errors_printed++;
  if (max_error_level && (errors_printed > max_error_level))
    {
      fprintf (stderr, _("Too many errors!  Gave up.\n"));
      flush_file_stack ();
      cm_bye ();
      exit (FATAL);
    }
}

/* **************************************************************** */
/*                                                                  */
/*                      Main ()  Start of code                      */
/*                                                                  */
/* **************************************************************** */

/* For each file mentioned in the command line, process it, turning
   Texinfo commands into wonderfully formatted output text. */
int
main (argc, argv)
     int argc;
     char **argv;
{
  extern int errors_printed;
  char *filename_part ();
  int c, ind;
  int reading_from_stdin = 0;

  /* The name of this program is the last filename in argv[0]. */
  progname = filename_part (argv[0]);

#ifdef HAVE_SETLOCALE
  /* Do not use LC_ALL, because LC_NUMERIC screws up the scanf parsing
     of the argument to @multicolumn.  */
  setlocale (LC_TIME, "");
#ifdef HAVE_LC_MESSAGES
  setlocale (LC_MESSAGES, "");
#endif
#endif

  /* Set the text message domain.  */
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);

  /* Parse argument flags from the input line. */
  while ((c = getopt_long (argc, argv, "D:e:E:f:I:o:p:P:r:s:U:V",
                           long_options, &ind)) != EOF)
    {
      if (c == 0 && long_options[ind].flag == 0)
        c = long_options[ind].val;

      switch (c)
        {
        case 'D':
        case 'U':
          /* User specified variable to set or clear. */
          handle_variable_internal ((c == 'D') ? SET : CLEAR, optarg);
          break;

        case 'e':
          /* User specified error level. */
          if (sscanf (optarg, "%d", &max_error_level) != 1)
            {
              fprintf (stderr,
                      _("%s: %s arg must be numeric, not `%s'.\n"),
                      "--error-limit", progname, optarg);
              usage (stderr, FATAL);
            }
          break;

        case 'E':
          /* User specified a macro expansion output file. */
          if (!macro_expansion_output_stream)
            {
              macro_expansion_filename = optarg;
              macro_expansion_output_stream
                = strcmp (optarg, "-") == 0 ? stdout : fopen (optarg, "w");
              if (!macro_expansion_output_stream)
                error (_("Couldn't open macro expansion output `%s'"), optarg);
            }
          else
            error (_("Cannot specify more than one macro expansion output"));
          break;

        case 'f':
          /* User specified fill_column. */
          if (sscanf (optarg, "%d", &fill_column) != 1)
            {
              fprintf (stderr,
                       _("%s: %s arg must be numeric, not `%s'.\n"), 
                       "--fill-column", progname, optarg);
              usage (FATAL);
            }
          break;

        case 'F':
          force++; /* Do not remove erroneous output.  */
          break;
          
        case 'h':
          usage (NO_ERROR);
          break;

        case 'I':
          /* Append user-specified dir to include file path. */
          if (!include_files_path)
            include_files_path = xstrdup (".");

          include_files_path = (char *)
            xrealloc (include_files_path,
                      2 + strlen (include_files_path) + strlen (optarg));
          strcat (include_files_path, ":");
          strcat (include_files_path, optarg);
          break;

        case 'o':
          /* User specified output file. */
          command_output_filename = xstrdup (optarg);
          break;

        case 'p':
          /* User specified paragraph indent (paragraph_start_index). */
          if (set_paragraph_indent (optarg) < 0)
            {
              fprintf (stderr,
   _("%s: --paragraph-indent arg must be numeric/`none'/`asis', not `%s'.\n"), 
                       progname, optarg);
              usage (FATAL);
            }
          break;

        case 'P':
          /* Prepend user-specified include dir to include path. */
          if (!include_files_path)
            {
              include_files_path = xstrdup (optarg);
              include_files_path = (char *) xrealloc (include_files_path,
                           strlen (include_files_path) + 3); /* 3 for ":.\0" */
              strcat (include_files_path, ":.");
            }
          else
            {
              char *tmp = xstrdup (include_files_path);
              include_files_path = (char *) xrealloc (include_files_path,
          strlen (include_files_path) + strlen (optarg) + 2); /* 2 for ":\0" */
              strcpy (include_files_path, optarg);
              strcat (include_files_path, ":");
              strcat (include_files_path, tmp);
              free (tmp);
            }
          break;
 
        case 'r':
          /* User specified reference warning limit. */
          if (sscanf (optarg, "%d", &reference_warning_limit) != 1)
            {
              fprintf (stderr,
                     _("%s: %s arg must be numeric, not `%s'.\n"), 
                     "--reference-limit", progname, optarg);
              usage (FATAL);
            }
          break;

        case 's':
          /* User specified footnote style. */
          if (set_footnote_style (optarg) < 0)
            {
              fprintf (stderr,
          _("%s: --footnote-style arg must be `separate' or `end', not `%s'.\n"), 
                       progname, optarg);
              usage (FATAL);
            }
          footnote_style_preset = 1;
          break;

        case 'V':
          /* User requested version info. */
          print_version_info ();
	  printf (_("Copyright (C) %s Free Software Foundation, Inc.\n\
There is NO warranty.  You may redistribute this software\n\
under the terms of the GNU General Public License.\n\
For more information about these matters, see the files named COPYING.\n"),
		  "1998");
          exit (NO_ERROR);
          break;

        case '?':
          usage (FATAL);
          break;
        }
    }

  if (optind == argc)
    {
      /* Check to see if input is a file.  If so, process that. */
      if (!isatty (fileno (stdin)))
        reading_from_stdin = 1;
      else
        {
          fprintf (stderr, _("%s: missing file argument.\n"), progname);
          usage (FATAL);
        }
    }

  /* If the user has specified --no-headers, this should imply --no-split.
     Do that here.  I think it might also imply that we should ignore the
     setfilename at the top of the file, but this might break some FSF things,
     so I will hold off on that. */
  if (no_headers)
    {
      splitting = 0;

      /* If the user has not specified an output file, use stdout. */
      if (!command_output_filename)
        command_output_filename = xstrdup ("-");
    }

  if (verbose_mode)
    print_version_info ();

  /* Remaining arguments are file names of texinfo files.
     Convert them, one by one. */
  if (!reading_from_stdin)
    {
      while (optind != argc)
        convert_from_file (argv[optind++]);
    }
  else
    convert_from_stream (stdin, "stdin");

  if (errors_printed)
    return (SYNTAX);
  else
    return (NO_ERROR);
}

/* Display the version info of this invocation of Makeinfo. */
void
print_version_info ()
{
  printf ("makeinfo (GNU %s %s) %d.%d\n", PACKAGE, VERSION,
          major_version, minor_version);
}

/* If EXIT_VALUE is zero, print the full usage message to stdout.
   Otherwise, just say to use --help for more info.
   Then exit with EXIT_VALUE. */
void
usage (exit_value)
     int exit_value;
{
  if (exit_value != 0)
    fprintf (stderr, _("Try `%s --help' for more information.\n"), progname);
  else
    printf (_("Usage: %s [OPTION]... TEXINFO-FILE...\n\
\n\
Translate Texinfo source documentation to a format suitable for reading\n\
with GNU Info.\n\
\n\
Options:\n\
-D VAR                 define a variable, as with @set.\n\
-E MACRO-OFILE         process macros only, output texinfo source.\n\
-I DIR                 append DIR to the @include directory search path.\n\
-P DIR                 prepend DIR to the @include directory search path.\n\
-U VAR                 undefine a variable, as with @clear.\n\
--error-limit NUM      quit after NUM errors (default %d).\n\
--fill-column NUM      break lines at NUM characters (default %d).\n\
--footnote-style STYLE output footnotes according to STYLE:\n\
                         `separate' to place footnotes in their own node,\n\
                         `end' to place the footnotes at the end of\n\
                         the node in which they are defined (the default).\n\
--force                preserve output even if errors.\n\
--help                 display this help and exit.\n\
--no-validate          suppress node cross-reference validation.\n\
--no-warn              suppress warnings (but not errors).\n\
--no-split             suppress splitting of large files.\n\
--no-headers           suppress node separators and Node: Foo headers.\n\
--output FILE, -o FILE output to FILE, and ignore any @setfilename.\n\
--paragraph-indent VAL indent paragraphs with VAL spaces (default %d).\n\
                         if VAL is `none', do not indent; if VAL is `asis',\n\
                         preserve any existing indentation.\n\
--reference-limit NUM  complain about at most NUM references (default %d).\n\
--verbose              report about what is being done.\n\
--version              display version information and exit.\n\
\n\
Email bug reports to bug-texinfo@gnu.org.\n\
"),
           progname, max_error_level, fill_column,
           paragraph_start_indent, reference_warning_limit);
  exit (exit_value);
}

/* Manipulating Lists */

typedef struct generic_list {
  struct generic_list *next;
} GENERIC_LIST;

/* Reverse the chain of structures in LIST.  Output the new head
   of the chain.  You should always assign the output value of this
   function to something, or you will lose the chain. */
GENERIC_LIST *
reverse_list (list)
     register GENERIC_LIST *list;
{
  register GENERIC_LIST *next;
  register GENERIC_LIST *prev = (GENERIC_LIST *) NULL;

  while (list)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return (prev);
}

/* Pushing and Popping Files */

/* Find and load the file named FILENAME.  Return a pointer to
   the loaded file, or NULL if it can't be loaded. */
char *
find_and_load (filename)
     char *filename;
{
  struct stat fileinfo;
  long file_size;
  int file = -1, count = 0;
  char *fullpath, *result, *get_file_info_in_path ();

  result = fullpath = (char *)NULL;

  fullpath = get_file_info_in_path (filename, include_files_path, &fileinfo);

  if (!fullpath)
    goto error_exit;

  filename = fullpath;
  file_size = (long) fileinfo.st_size;

  file = open (filename, O_RDONLY);
  if (file < 0)
    goto error_exit;

  /* Load the file, with enough room for a newline and a null. */
  result = xmalloc (file_size + 2);

  /* VMS stat lies about the st_size value.  The actual number of
     readable bytes is always less than this value.  The arcane
     mysteries of VMS/RMS are too much to probe, so this hack
    suffices to make things work. */
#if defined (VMS) || defined (WIN32)
#ifdef VMS
  while ((n = read (file, result + count, file_size)) > 0)
#else /* WIN32 */
  while ((n = read (file, result + count, 1)) > 0)
#endif /* WIN32 */
    count += n;
  if (n == -1)
#else /* !VMS && !WIN32 */
    count = file_size;
    if (read (file, result, file_size) != file_size)
#endif /* !VMS && !WIN32 */
  error_exit:
    {
      if (result)
        free (result);

      if (fullpath)
        free (fullpath);

      if (file != -1)
        close (file);

      return ((char *) NULL);
    }
  close (file);

  /* Set the globals to the new file. */
  input_text = result;
  size_of_input_text = count;
  input_filename = fullpath;
  node_filename = xstrdup (fullpath);
  input_text_offset = 0;
  line_number = 1;
  /* Not strictly necessary.  This magic prevents read_token () from doing
     extra unnecessary work each time it is called (that is a lot of times).
     SIZE_OF_INPUT_TEXT is one past the actual end of the text. */
  input_text[size_of_input_text] = '\n';
  /* This, on the other hand, is always necessary.  */
  input_text[size_of_input_text+1] = 0;
  return (result);
}

/* Save the state of the current input file. */
void
pushfile ()
{
  FSTACK *newstack = (FSTACK *) xmalloc (sizeof (FSTACK));
  newstack->filename = input_filename;
  newstack->text = input_text;
  newstack->size = size_of_input_text;
  newstack->offset = input_text_offset;
  newstack->line_number = line_number;
  newstack->next = filestack;

  filestack = newstack;
  push_node_filename ();
}

/* Make the current file globals be what is on top of the file stack. */
void
popfile ()
{
  FSTACK *tos = filestack;

  if (!tos)
    abort ();                   /* My fault.  I wonder what I did? */

#if defined (HAVE_MACROS)
  if (macro_expansion_output_stream)
    {
      maybe_write_itext (input_text, input_text_offset);
      forget_itext (input_text);
    }
#endif /* HAVE_MACROS */

  /* Pop the stack. */
  filestack = filestack->next;

  /* Make sure that commands with braces have been satisfied. */
  if (!executing_string && !me_executing_string)
    discard_braces ();

  /* Get the top of the stack into the globals. */
  input_filename = tos->filename;
  input_text = tos->text;
  size_of_input_text = tos->size;
  input_text_offset = tos->offset;
  line_number = tos->line_number;
  free (tos);

  /* Go back to the (now) current node. */
  pop_node_filename ();
}

/* Flush all open files on the file stack. */
void
flush_file_stack ()
{
  while (filestack)
    {
      char *fname = input_filename;
      char *text = input_text;
      popfile ();
      free (fname);
      free (text);
    }
}

int node_filename_stack_index = 0;
int node_filename_stack_size = 0;
char **node_filename_stack = (char **)NULL;

void
push_node_filename ()
{
  if (node_filename_stack_index + 1 > node_filename_stack_size)
    node_filename_stack = (char **)xrealloc
    (node_filename_stack, (node_filename_stack_size += 10) * sizeof (char *));

  node_filename_stack[node_filename_stack_index] = node_filename;
  node_filename_stack_index++;
}

void
pop_node_filename ()
{
  node_filename = node_filename_stack[--node_filename_stack_index];
}

/* Return just the simple part of the filename; i.e. the
   filename without the path information, or extensions.
   This conses up a new string. */
char *
filename_part (filename)
     char *filename;
{
  char *basename;

  basename = strrchr (filename, '/');
  if (!basename)
    basename = filename;
  else
    basename++;

  basename = xstrdup (basename);
#if defined (REMOVE_OUTPUT_EXTENSIONS)

  /* See if there is an extension to remove.  If so, remove it. */
  {
    char *temp;

    temp = strrchr (basename, '.');
    if (temp)
      *temp = 0;
  }
#endif /* REMOVE_OUTPUT_EXTENSIONS */
  return (basename);
}

/* Return the pathname part of filename.  This can be NULL. */
char *
pathname_part (filename)
     char *filename;
{
  char *expand_filename ();
  char *result = (char *) NULL;
  register int i;

  filename = expand_filename (filename, "");

  i = strlen (filename) - 1;

  while (i && filename[i] != '/')
    i--;
  if (filename[i] == '/')
    i++;

  if (i)
    {
      result = (char *)xmalloc (1 + i);
      strncpy (result, filename, i);
      result[i] = 0;
    }
  free (filename);
  return (result);
}

char *
filename_non_directory (name)
     char *name;
{
  register int i;

  for (i = strlen (name) - 1; i; i--)
    if (name[i] == '/')
      return (xstrdup (name + i + 1));

  return (xstrdup (name));
}

/* Return the expansion of FILENAME. */
char *
expand_filename (filename, input_name)
     char *filename, *input_name;
{
  register int i;
  char *full_pathname ();

  if (filename)
    filename = full_pathname (filename);
  else
    {
      filename = filename_non_directory (input_name);

      if (!*filename)
        {
          free (filename);
          filename = xstrdup ("noname.texi");
        }

      for (i = strlen (filename) - 1; i; i--)
        if (filename[i] == '.')
          break;

      if (!i)
        i = strlen (filename);

      if (i + 6 > (strlen (filename)))
        filename = (char *)xrealloc (filename, i + 6);
      strcpy (filename + i, ".info");
      return (filename);
    }
        
  if (filename[0] == '.' || filename[0] == '/')
    return (filename);

  if (filename[0] != '/' && input_name[0] == '/')
    {
      /* Make it so that relative names work. */
      char *result;
      
      i = strlen (input_name) - 1;

      result = (char *)xmalloc (1 + strlen (input_name) + strlen (filename));
      strcpy (result, input_name);

      while (result[i] != '/' && i)
        i--;

      if (result[i] == '/')
        i++;

      strcpy (&result[i], filename);
      free (filename);
      return (result);
    }
  return (filename);
}

/* Return the full path to FILENAME. */
char *
full_pathname (filename)
     char *filename;
{
  int initial_character;
  char *result;

  /* No filename given? */
  if (!filename || !(initial_character = *filename))
    return (xstrdup (""));
  
  /* Already absolute? */
  if ((initial_character == '/') ||
      ((strncmp (filename, "./", 2) == 0) ||
       (strncmp (filename, "../", 3) == 0)))
    return (xstrdup (filename));

  if (initial_character != '~')
    {
      char *localdir;

      localdir = (char *)xmalloc (1025);
#if defined (HAVE_GETCWD)
      if (!getcwd (localdir, 1024))
#else  /*  !HAVE_GETCWD */
        if (!getwd (localdir))
#endif /* !HAVE_GETCWD */
          {
            fprintf (stderr, _("%s: getwd: %s, %s\n"),
                     progname, filename, localdir);
            exit (1);
          }

      strcat (localdir, "/");
      strcat (localdir, filename);
      result = xstrdup (localdir);
      free (localdir);
    }
  else
    {
#ifndef WIN32
      if (filename[1] == '/')
        {
          /* Return the concatenation of the environment variable HOME
             and the rest of the string. */
          char *temp_home;

          temp_home = (char *) getenv ("HOME");
          result = (char *)xmalloc (strlen (&filename[1])
                                    + 1
                                    + temp_home ? strlen (temp_home)
                                    : 0);
          *result = 0;

          if (temp_home)
            strcpy (result, temp_home);

          strcat (result, &filename[1]);
        }
      else
        {
          struct passwd *user_entry;
          int i, c;
          char *username = (char *)xmalloc (257);

          for (i = 1; (c = filename[i]); i++)
            {
              if (c == '/')
                break;
              else
                username[i - 1] = c;
            }
          if (c)
            username[i - 1] = 0;

          user_entry = getpwnam (username);

          if (!user_entry)
            return (xstrdup (filename));

          result = (char *)xmalloc (1 + strlen (user_entry->pw_dir)
                                    + strlen (&filename[i]));
          strcpy (result, user_entry->pw_dir);
          strcat (result, &filename[i]);
        }
    }
#endif /* not WIN32 */
  return (result);
}

char *
output_name_from_input_name (name)
     char *name;
{
  return (expand_filename ((char *)NULL, name));
}

/* **************************************************************** */
/*                                                                  */
/*                      Hacking Tokens and Strings                  */
/*                                                                  */
/* **************************************************************** */

/* Return the next token as a string pointer.  We cons the string. */
char *
read_token ()
{
  int i, character;
  char *result;

  /* If the first character to be read is self-delimiting, then that
     is the command itself. */
  character = curchar ();
  if (self_delimiting (character))
    {
      input_text_offset++;

      if (character == '\n')
        line_number++;

      result = xstrdup (" ");
      *result = character;
      return (result);
    }

  for (i = 0; ((input_text_offset != size_of_input_text)
               && (character = curchar ())
               && command_char (character));
       i++, input_text_offset++);
  result = (char *)xmalloc (i + 1);
  memcpy (result, &input_text[input_text_offset - i], i);
  result[i] = 0;
  return (result);
}

/* Return nonzero if CHARACTER is self-delimiting. */
int
self_delimiting (character)
     int character;
{
  /* @; and @\ are not Texinfo commands, but they are listed here
     anyway.  I don't know why.  --karl, 10aug96.  */
  return member (character, "~{|}`^\\@?=;:.-,*\'\" !\n\t");
}

/* Clear whitespace from the front and end of string. */
void
canon_white (string)
     char *string;
{
  int len = strlen (string);
  int x;

  if (!len)
    return;

  for (x = 0; x < len; x++)
    {
      if (!cr_or_whitespace (string[x]))
        {
          strcpy (string, string + x);
          break;
        }
    }
  len = strlen (string);
  if (len)
    len--;
  while (len > -1 && cr_or_whitespace (string[len]))
    len--;
  string[len + 1] = 0;
}

/* Bash STRING, replacing all whitespace with just one space. */
void
fix_whitespace (string)
     char *string;
{
  char *temp = (char *)xmalloc (strlen (string) + 1);
  int string_index = 0;
  int temp_index = 0;
  int c;

  canon_white (string);

  while (string[string_index])
    {
      c = temp[temp_index++] = string[string_index++];

      if (c == ' ' || c == '\n' || c == '\t')
        {
          temp[temp_index - 1] = ' ';
          while ((c = string[string_index]) && (c == ' ' ||
                                                c == '\t' ||
                                                c == '\n'))
            string_index++;
        }
    }
  temp[temp_index] = 0;
  strcpy (string, temp);
  free (temp);
}

/* Discard text until the desired string is found.  The string is
   included in the discarded text. */
void
discard_until (string)
     char *string;
{
  int temp = search_forward (string, input_text_offset);

  int tt = (temp < 0) ? size_of_input_text : temp + strlen (string);
  int from = input_text_offset;

  /* Find out what line we are on. */
  while (from != tt)
    if (input_text[from++] == '\n')
      line_number++;

  if (temp < 0)
    {
      input_text_offset = size_of_input_text - strlen (string);

      if (strcmp (string, "\n") != 0)
        {
          line_error (_("Expected `%s'"), string);
          return;
        }
    }
  else
    input_text_offset = temp;

  input_text_offset += strlen (string);
}

/* Read characters from the file until we are at MATCH.
   Place the characters read into STRING.
   On exit input_text_offset is after the match string.
   Return the offset where the string starts. */
int
get_until (match, string)
     char *match, **string;
{
  int len, current_point, x, new_point, tem;

  current_point = x = input_text_offset;
  new_point = search_forward (match, input_text_offset);

  if (new_point < 0)
    new_point = size_of_input_text;
  len = new_point - current_point;

  /* Keep track of which line number we are at. */
  tem = new_point + (strlen (match) - 1);
  while (x != tem)
    if (input_text[x++] == '\n')
      line_number++;

  *string = (char *)xmalloc (len + 1);

  memcpy (*string, &input_text[current_point], len);
  (*string)[len] = 0;

  /* Now leave input_text_offset in a consistent state. */
  input_text_offset = tem;

  if (input_text_offset > size_of_input_text)
    input_text_offset = size_of_input_text;

  return (new_point);
}

/* Read characters from the file until we are at MATCH or end of line.
   Place the characters read into STRING.  */
void
get_until_in_line (expand, match, string)
     int expand;
     char *match, **string;
{
  int real_bottom = size_of_input_text;
  int limit = search_forward ("\n", input_text_offset);
  if (limit < 0)
    limit = size_of_input_text;

  /* Replace input_text[input_text_offset .. limit-1] with its macro
     expansion (actually, we expand all commands).  This allows the node
     names themselves to be constructed via a macro, as in:
        @macro foo{p, q}
        Together: \p\ & \q\.
        @end macro

        @node @foo{A,B}, next, prev, top
     
     Otherwise, the `,' separating the macro args A and B is taken as
     the node argument separator, so the node name is `@foo{A'.  This
     expansion is only necessary on the first call, since we expand the
     whole line then.
     
     Furthermore, if we're executing a string, don't do it -- we'll end
     up shrinking the execution string which is currently aliased to
     `input_text', so it might get moved, and not updated in the
     `execution_strings' array.  This happens when processing the
     (synthetic) Overview-Footnotes node in the Texinfo manual.  */

  if (expand && !executing_string && !me_executing_string)
    {
      char *xp;
      unsigned xp_len, new_len;
      
      /* Get original string from input.  */
      unsigned raw_len = limit - input_text_offset;
      char *str = xmalloc (raw_len + 1);
      strncpy (str, input_text + input_text_offset, raw_len);
      str[raw_len] = 0;
      
      /* Expand it.  */
      xp = expansion (str, 0);
      xp_len = strlen (xp);
      free (str);
      
      /* Plunk the expansion into the middle of `input_text' --
         which is terminated by a newline, not a null.  */
      str = xmalloc (real_bottom - limit + 1);
      strncpy (str, input_text + limit, real_bottom - limit + 1);
      new_len = input_text_offset + xp_len + real_bottom - limit + 1;
      input_text = xrealloc (input_text, new_len);
      strcpy (input_text + input_text_offset, xp);
      strncpy (input_text + input_text_offset + xp_len, str,
	       real_bottom - limit + 1);
      free (str);
      free (xp);
      
      limit += xp_len - raw_len;
      real_bottom += xp_len - raw_len;
    }

  size_of_input_text = limit;
  get_until (match, string);
  size_of_input_text = real_bottom;
}

void
get_rest_of_line (string)
     char **string;
{
  get_until ("\n", string);
  canon_white (*string);

  if (curchar () == '\n')       /* as opposed to the end of the file... */
    {
      line_number++;
      input_text_offset++;
    }
}

/* Backup the input pointer to the previous character, keeping track
   of the current line number. */
void
backup_input_pointer ()
{
  if (input_text_offset)
    {
      input_text_offset--;
      if (curchar () == '\n')
        line_number--;
    }
}

/* Read characters from the file until we are at MATCH or closing brace.
   Place the characters read into STRING.  */
void
get_until_in_braces (match, string)
     char *match, **string;
{
  char *temp;
  int i, brace = 0;
  int match_len = strlen (match);

  for (i = input_text_offset; i < size_of_input_text; i++)
    {
      if (input_text[i] == '{')
        brace++;
      else if (input_text[i] == '}')
        brace--;
      else if (input_text[i] == '\n')
        line_number++;

      if (brace < 0 ||
          (brace == 0 && strncmp (input_text + i, match, match_len) == 0))
        break;
    }

  match_len = i - input_text_offset;
  temp = (char *)xmalloc (2 + match_len);
  strncpy (temp, input_text + input_text_offset, match_len);
  temp[match_len] = 0;
  input_text_offset = i;
  *string = temp;
}

/* **************************************************************** */
/*                                                                  */
/*                      Converting the File                         */
/*                                                                  */
/* **************************************************************** */

/* Convert the file named by NAME.  The output is saved on the file
   named as the argument to the @setfilename command. */
static char *suffixes[] = {
  ".texinfo",
  ".texi",
  ".txinfo",
  "",
  (char *)NULL
};

void
initialize_conversion ()
{
  init_tag_table ();
  init_indices ();
  init_internals ();
  init_paragraph ();

  /* This is used for splitting the output file and for doing section
     headings.  It was previously initialized in `init_paragraph', but its
     use there loses with the `init_paragraph' calls done by the
     multitable code; the tag indices get reset to zero.  */
  output_position = 0;
}

/* We read in multiples of 4k, simply because it is a typical pipe size
   on unix systems. */
#define READ_BUFFER_GROWTH (4 * 4096)

/* Convert the Texinfo file coming from the open stream STREAM.  Assume the
   source of the stream is named NAME. */
void
convert_from_stream (stream, name)
     FILE *stream;
     char *name;
{
  char *buffer = (char *)NULL;
  int buffer_offset = 0, buffer_size = 0;

  initialize_conversion ();

  /* Read until the end of the stream.  This isn't strictly correct, since
     the texinfo input may end before the stream ends, but it is a quick
     working hueristic. */
  while (!feof (stream))
    {
      int count;

      if (buffer_offset + (READ_BUFFER_GROWTH + 1) >= buffer_size)
        buffer = (char *)
          xrealloc (buffer, (buffer_size += READ_BUFFER_GROWTH));

      count = fread (buffer + buffer_offset, 1, READ_BUFFER_GROWTH, stream);

      if (count < 0)
        {
          perror (name);
          exit (FATAL);
        }

      buffer_offset += count;
      if (count == 0)
        break;
    }

  /* Set the globals to the new file. */
  input_text = buffer;
  size_of_input_text = buffer_offset;
  input_filename = xstrdup (name);
  node_filename = xstrdup (name);
  input_text_offset = 0;
  line_number = 1;

  /* Not strictly necessary.  This magic prevents read_token () from doing
     extra unnecessary work each time it is called (that is a lot of times).
     The SIZE_OF_INPUT_TEXT is one past the actual end of the text. */
  input_text[size_of_input_text] = '\n';

  convert_from_loaded_file (name);
}

void
convert_from_file (name)
     char *name;
{
  register int i;
  char *filename = (char *)xmalloc (strlen (name) + 50);

  initialize_conversion ();

  /* Try to load the file specified by NAME, concatenated with our
     various suffixes.  Prefer files like `makeinfo.texi' to
     `makeinfo'.  */
  for (i = 0; suffixes[i]; i++)
    {
      strcpy (filename, name);
      strcat (filename, suffixes[i]);

      if (find_and_load (filename))
        break;

      if (!suffixes[i][0] && strrchr (filename, '.'))
        {
          fs_error (filename);
          free (filename);
          return;
        }
    }

  if (!suffixes[i])
    {
      fs_error (name);
      free (filename);
      return;
    }

  input_filename = filename;

  convert_from_loaded_file (name);
}
  
void
convert_from_loaded_file (name)
     char *name;
{
  char *expand_filename (), *filename_part ();
  char *real_output_filename = (char *)NULL;

#if defined (HAVE_MACROS)
  remember_itext (input_text, 0);
#endif /* HAVE_MACROS */

  /* Search this file looking for the special string which starts conversion.
     Once found, we may truly begin. */
  input_text_offset = 0;
  while (input_text_offset >= 0)
    {
      input_text_offset =
        search_forward (setfilename_search, input_text_offset);

      if ((input_text_offset == 0) ||
          ((input_text_offset > 0) &&
           (input_text[input_text_offset -1] == '\n')))
        break;
      else if (input_text_offset > 0)
        input_text_offset++;
    }

  if (input_text_offset < 0)
    {
      if (!command_output_filename)
        {
#if defined (REQUIRE_SETFILENAME)
          error (_("No `%s' found in `%s'"), setfilename_search, name);
          goto finished;
#else
          register int i, end_of_first_line;

          /* Find the end of the first line in the file. */
          for (i = 0; i < size_of_input_text - 1; i++)
            if (input_text[i] == '\n')
              break;

          end_of_first_line = i + 1;

          input_text_offset = 0;

          for (i = 0; i < end_of_first_line; i++)
            {
              if ((input_text[i] == '\\') &&
                  (strncmp (input_text + i + 1, "include", 7) == 0))
                {
                  input_text_offset = end_of_first_line;
                  break;
                }
            }
          command_output_filename = output_name_from_input_name (name);
#endif /* !REQUIRE_SETFILENAME */
        }
    }
  else
    input_text_offset += strlen (setfilename_search);

  if (!command_output_filename)
    get_until ("\n", &output_filename);
  else
    {
      if (input_text_offset != -1)
        discard_until ("\n");
      else
        input_text_offset = 0;

      real_output_filename = output_filename = command_output_filename;
      command_output_filename = (char *)NULL;
    }

  canon_white (output_filename);

  if (real_output_filename && strcmp (real_output_filename, "-") == 0)
    {
      if (macro_expansion_filename
          && strcmp (macro_expansion_filename, "-") == 0)
        {
          fprintf (stderr, _("%s: Skipping macro expansion to stdout as Info output is going there.\n"),
                   progname);
          macro_expansion_output_stream = NULL;
        }
      real_output_filename = xstrdup (real_output_filename);
      output_stream = stdout;
      splitting = 0;            /* Cannot split when writing to stdout. */
    }
  else
    {
      if (!real_output_filename)
        real_output_filename = expand_filename (output_filename, name);
      else
        real_output_filename = xstrdup (real_output_filename);

      output_stream = fopen (real_output_filename, "w");
    }

  if (output_stream != stdout)
    printf (_("Making %s file `%s' from `%s'.\n"),
            no_headers ? "text" : "info", output_filename, input_filename);

  if (output_stream == NULL)
    {
      fs_error (real_output_filename);
      goto finished;
    }

  /* Make the displayable filename from output_filename.  Only the base
     portion of the filename need be displayed. */
  if (output_stream != stdout)
    pretty_output_filename = filename_part (output_filename);
  else
    pretty_output_filename = xstrdup ("stdout");

  /* For this file only, count the number of newlines from the top of
     the file to here.  This way, we keep track of line numbers for
     error reporting.  Line_number starts at 1, since the user isn't
     zero-based. */
  {
    int temp = 0;
    line_number = 1;
    while (temp != input_text_offset)
      if (input_text[temp++] == '\n')
        line_number++;
  }

  if (!no_headers)
    {
     add_word_args (_("This is Info file %s, produced by Makeinfo version %d.%d"),
                     output_filename, major_version, minor_version);
     add_word_args (_(" from the input file %s.\n"), input_filename);
    }

  close_paragraph ();
  reader_loop ();

finished:
  discard_insertions (0);
  close_paragraph ();
  flush_file_stack ();

#if defined (HAVE_MACROS)
  if (macro_expansion_output_stream)
    {
      fclose (macro_expansion_output_stream);
      if (errors_printed && !force
          && strcmp (macro_expansion_filename, "-") != 0
          && strcmp (macro_expansion_filename, "/dev/null") != 0)
        {
          fprintf (stderr, _("%s: Removing macro output file `%s' due to errors; use --force to preserve.\n"),
                   progname, macro_expansion_filename);
          if (unlink (macro_expansion_filename) < 0)
            perror (macro_expansion_filename);
        }
    }
#endif /* HAVE_MACROS */

  if (output_stream)
    {
      output_pending_notes ();
      free_pending_notes ();
      if (tag_table != NULL)
        {
          tag_table = (TAG_ENTRY *) reverse_list (tag_table);
          if (!no_headers)
            write_tag_table ();
        }

      if (output_stream != stdout)
        fclose (output_stream);

      /* If validating, then validate the entire file right now. */
      if (validating)
        validate_file (tag_table);

      if (splitting && (!errors_printed || force))
        split_file (real_output_filename, 0);
      else if (errors_printed && !force
               && strcmp (real_output_filename, "-") != 0
               && strcmp (real_output_filename, "/dev/null") != 0)
        { /* If there were errors, and no --force, remove the output.  */
          fprintf (stderr, _("%s: Removing output file `%s' due to errors; use --force to preserve.\n"),
                   progname, real_output_filename);
          if (unlink (real_output_filename) < 0)
            perror (real_output_filename);
        }
    }
  free (real_output_filename);
}

void
free_and_clear (pointer)
     char **pointer;
{
  if (*pointer)
    {
      free (*pointer);
      *pointer = (char *) NULL;
    }
}

 /* Initialize some state. */
void
init_internals ()
{
  free_and_clear (&output_filename);
  free_and_clear (&command);
  free_and_clear (&input_filename);
  free_node_references ();
  init_insertion_stack ();
  init_brace_stack ();
  current_node = NULL; /* sometimes already freed */
  command_index = 0;
  in_menu = 0;
  in_detailmenu = 0;
  top_node_seen = 0;
  non_top_node_seen = 0;
}

void
init_paragraph ()
{
  free_and_clear (&output_paragraph);
  output_paragraph = (unsigned char *)xmalloc (paragraph_buffer_len);
  output_paragraph[0] = 0;
  output_paragraph_offset = 0;
  output_column = 0;
  paragraph_is_open = 0;
  current_indent = 0;
}

/* Okay, we are ready to start the conversion.  Call the reader on
   some text, and fill the text as it is output.  Handle commands by
   remembering things like open braces and the current file position on a
   stack, and when the corresponding close brace is found, you can call
   the function with the proper arguments. */
void
reader_loop ()
{
  int character;
  int done = 0;
  int dash_count = 0;

  while (!done)
    {
      if (input_text_offset >= size_of_input_text)
        break;

      character = curchar ();

      if (!in_fixed_width_font &&
          (character == '\'' || character == '`') &&
          input_text[input_text_offset + 1] == character)
        {
          input_text_offset++;
          character = '"';
        }

      if (character == '-')
        {
          dash_count++;
          if (dash_count == 2 && !in_fixed_width_font)
            {
              input_text_offset++;
              continue;
            }
        }
      else
        {
          dash_count = 0;
        }

      /* If this is a whitespace character, then check to see if the line
         is blank.  If so, advance to the carriage return. */
      if (whitespace (character))
        {
          register int i = input_text_offset + 1;

          while (i < size_of_input_text && whitespace (input_text[i]))
            i++;

          if (i == size_of_input_text || input_text[i] == '\n')
            {
              if (i == size_of_input_text)
                i--;

              input_text_offset = i;
              character = curchar ();
            }
        }

      if (character == '\n')
        {
          line_number++;

          /* Check for a menu entry here, since the "escape sequence"
             that begins menu entries is "\n* ". */
          if (in_menu && input_text_offset + 1 < size_of_input_text)
            {
              char *glean_node_from_menu (), *tem;

              /* Note that the value of TEM is discarded, since it is
                 gauranteed to be NULL when glean_node_from_menu () is
                 called with a Nonzero argument. */
              if (!in_detailmenu)
                tem = glean_node_from_menu (1);
            }
        }

      switch (character)
        {
        case COMMAND_PREFIX:
          read_command ();
          break;

        case '{':
          /* Special case.  I'm not supposed to see this character by itself.
             If I do, it means there is a syntax error in the input text.
             Report the error here, but remember this brace on the stack so
             you can ignore its partner. */

          line_error (_("Misplaced %c"), '{');
          remember_brace (misplaced_brace);

          /* Don't advance input_text_offset since this happens in
             remember_brace ().
             input_text_offset++;
           */
          break;

        case '}':
          pop_and_call_brace ();
          input_text_offset++;
          break;

        default:
          add_char (character);
          input_text_offset++;
        }
    }
#if defined (HAVE_MACROS)
  if (macro_expansion_output_stream)
    maybe_write_itext (input_text, input_text_offset);
#endif /* HAVE_MACROS */
}

/* Find the command corresponding to STRING.  If the command
   is found, return a pointer to the data structure.  Otherwise
   return (-1). */
COMMAND *
get_command_entry (string)
     char *string;
{
  register int i;

  for (i = 0; command_table[i].name; i++)
    if (strcmp (command_table[i].name, string) == 0)
      return (&command_table[i]);

  /* This command is not in our predefined command table.  Perhaps
     it is a user defined command. */
  for (i = 0; i < user_command_array_len; i++)
    if (user_command_array[i] &&
        (strcmp (user_command_array[i]->name, string) == 0))
      return (user_command_array[i]);

  /* We never heard of this command. */
  return ((COMMAND *) -1);
}

/* input_text_offset is right at the command prefix character.
   Read the next token to determine what to do. */
void
read_command ()
{
  COMMAND *entry;

  input_text_offset++;
  free_and_clear (&command);
  command = read_token ();

#if defined (HAVE_MACROS)
  /* Check to see if this command is a macro.  If so, execute it here. */
  {
    MACRO_DEF *def;

    def = find_macro (command);

    if (def)
      {
        /* We disallow recursive use of a macro call.  Inhibit the expansion
           of this macro during the life of its execution. */
        if (!(def->flags & ME_RECURSE))
          def->inhibited = 1;

        execute_macro (def);

        if (!(def->flags & ME_RECURSE))
          def->inhibited = 0;

        return;
      }
    }
#endif /* HAVE_MACROS */

  entry = get_command_entry (command);
  if (entry == (COMMAND *)-1)
    {
      line_error (_("Unknown command `%s'"), command);
      return;
    }

  if (entry->argument_in_braces)
    remember_brace (entry->proc);

  (*(entry->proc)) (START, output_paragraph_offset, 0);
}

/* Return the string which invokes PROC; a pointer to a function. */
char *
find_proc_name (proc)
     COMMAND_FUNCTION *proc;
{
  register int i;

  for (i = 0; command_table[i].name; i++)
    if (proc == command_table[i].proc)
      return command_table[i].name;
  return _("NO_NAME!");
}

void
init_brace_stack ()
{
  brace_stack = (BRACE_ELEMENT *) NULL;
}

void
remember_brace (proc)
     COMMAND_FUNCTION *proc;
{
  if (curchar () != '{')
    line_error (_("%c%s expected `{...}'"), COMMAND_PREFIX, command);
  else
    input_text_offset++;
  remember_brace_1 (proc, output_paragraph_offset);
}

/* Remember the current output position here.  Save PROC
   along with it so you can call it later. */
void
remember_brace_1 (proc, position)
     COMMAND_FUNCTION *proc;
     int position;
{
  BRACE_ELEMENT *new = (BRACE_ELEMENT *) xmalloc (sizeof (BRACE_ELEMENT));
  new->next = brace_stack;
  new->proc = proc;
  new->pos = position;
  new->line = line_number;
  new->in_fixed_width_font = in_fixed_width_font;
  brace_stack = new;
}

/* Pop the top of the brace stack, and call the associated function
   with the args END and POS. */
void
pop_and_call_brace ()
{
  BRACE_ELEMENT *temp;
  COMMAND_FUNCTION *proc;
  int pos;

  if (brace_stack == (BRACE_ELEMENT *) NULL)
    {
      line_error (_("Unmatched }"));
      return;
    }

  pos = brace_stack->pos;
  proc = brace_stack->proc;
  in_fixed_width_font = brace_stack->in_fixed_width_font;
  temp = brace_stack->next;
  free (brace_stack);
  brace_stack = temp;

  (*proc) (END, pos, output_paragraph_offset);
}

/* Shift all of the markers in `brace_stack' by AMOUNT. */
void
adjust_braces_following (here, amount)
     int here, amount;
{
  register BRACE_ELEMENT *stack = brace_stack;

  while (stack)
    {
      if (stack->pos >= here)
        stack->pos += amount;
      stack = stack->next;
    }
}

/* You call discard_braces () when you shouldn't have any braces on the stack.
   I used to think that this happens for commands that don't take arguments
   in braces, but that was wrong because of things like @code{foo @@}.  So now
   I only detect it at the beginning of nodes. */
void
discard_braces ()
{
  if (!brace_stack)
    return;

  while (brace_stack)
    {
      if (brace_stack->proc != misplaced_brace)
        {
          char *proc_name;
          int temp_line_number = line_number;

          line_number = brace_stack->line;
          proc_name = find_proc_name (brace_stack->proc);
          line_error (_("%c%s missing close brace"), COMMAND_PREFIX, proc_name);
          line_number = temp_line_number;
          pop_and_call_brace ();
        }
      else
        {
          BRACE_ELEMENT *temp;
          temp = brace_stack->next;
          free (brace_stack);
          brace_stack = temp;
        }
    }
}

int
get_char_len (character)
     int character;
{
  /* Return the printed length of the character. */
  int len;

  switch (character)
    {
    case '\t':
      len = (output_column + 8) & 0xf7;
      if (len > fill_column)
        len = fill_column - output_column;
      else
        len = len - output_column;
      break;

    case '\n':
      len = fill_column - output_column;
      break;

    default:
      /* ASCII control characters appear as two characters in the output
         (e.g., ^A).  But characters with the high bit set are just one
         on suitable terminals, so don't count them as two for line
         breaking purposes.  */
      if (0 <= character && character < ' ')
        len = 2;
      else
        len = 1;
    }
  return (len);
}

void
#if defined (VA_FPRINTF) && __STDC__
add_word_args (char *format, ...)
#else
add_word_args (format, va_alist)
    char *format;
    va_dcl
#endif
{
  char buffer[1000];
#ifdef VA_FPRINTF
  va_list ap;
#endif

  VA_START (ap, format);
#ifdef VA_SPRINTF
  VA_SPRINTF (buffer, format, ap);
#else
  sprintf (buffer, format, a1, a2, a3, a4, a5, a6, a7, a8);
#endif /* not VA_SPRINTF */
  va_end (ap);
  add_word (buffer);
}

/* Add STRING to output_paragraph. */
void
add_word (string)
     char *string;
{
  while (*string)
    add_char (*string++);
}

/* Nonzero if the last character inserted has the syntax class of NEWLINE. */
int last_char_was_newline = 1;

/* The actual last inserted character.  Note that this may be something
   other than NEWLINE even if last_char_was_newline is 1. */
int last_inserted_character = 0;

/* Nonzero means that a newline character has already been
   inserted, so close_paragraph () should insert one less. */
int line_already_broken = 0;

/* When nonzero we have finished an insertion (see `end_insertion') and we
   want to ignore false continued paragraph closings. */
int insertion_paragraph_closed = 0;

/* Nonzero means attempt to make all of the lines have fill_column width. */
int do_justification = 0;

/* Add the character to the current paragraph.  If filling_enabled is
   nonzero, then do filling as well. */
void
add_char (character)
     int character;
{
  /* If we are avoiding outputting headers, and we are currently
     in a menu, then simply return. */
  if (no_headers && (in_menu || in_detailmenu))
    return;

  /* If we are adding a character now, then we don't have to
     ignore close_paragraph () calls any more. */
  if (must_start_paragraph && character != '\n')
    {
      must_start_paragraph = 0;
      line_already_broken = 0;  /* The line is no longer broken. */
      if (current_indent > output_column)
        {
          indent (current_indent - output_column);
          output_column = current_indent;
        }
    }

  if (non_splitting_words && member (character, " \t\n"))
    character = ' ' | 0x80;

  insertion_paragraph_closed = 0;

  switch (character)
    {
    case '\n':
      if (!filling_enabled)
        {
          insert ('\n');

          if (force_flush_right)
            {
              close_paragraph ();
              /* Hack to force single blank lines out in this mode. */
              flush_output ();
            }

          output_column = 0;

          if (!no_indent && paragraph_is_open)
            indent (output_column = current_indent);
          break;
        }
      else /* CHARACTER is newline, and filling is enabled. */
        {
          if (end_of_sentence_p ())
            {
              insert (' ');
              output_column++;
              last_inserted_character = character;
            }
        }

      if (last_char_was_newline)
        {
          close_paragraph ();
          pending_indent = 0;
        }
      else
        {
          last_char_was_newline = 1;
          insert (' ');
          output_column++;
        }
      break;

    default:
      {
        int len = get_char_len (character);
        int suppress_insert = 0;

        if ((character == ' ') && (last_char_was_newline))
          {
            if (!paragraph_is_open)
              {
                pending_indent++;
                return;
              }
          }

        if (!paragraph_is_open)
          {
            start_paragraph ();

            /* If the paragraph is supposed to be indented a certain way,
               then discard all of the pending whitespace.  Otherwise, we
               let the whitespace stay. */
            if (!paragraph_start_indent)
              indent (pending_indent);
            pending_indent = 0;
          }

        if ((output_column += len) > fill_column)
          {
            if (filling_enabled)
              {
                int temp = output_paragraph_offset;
                while (--temp > 0 && output_paragraph[temp] != '\n')
                  {
                    /* If we have found a space, we have the place to break
                       the line. */
                    if (output_paragraph[temp] == ' ')
                      {
                        /* Remove trailing whitespace from output. */
                        while (temp && whitespace (output_paragraph[temp - 1]))
                          temp--;

                        output_paragraph[temp++] = '\n';

                        /* We have correctly broken the line where we want
                           to.  What we don't want is spaces following where
                           we have decided to break the line.  We get rid of
                           them. */
                        {
                          int t1 = temp;

                          for (;; t1++)
                            {
                              if (t1 == output_paragraph_offset)
                                {
                                  if (whitespace (character))
                                    suppress_insert = 1;
                                  break;
                                }
                              if (!whitespace (output_paragraph[t1]))
                                break;
                            }

                          if (t1 != temp)
                            {
                              adjust_braces_following (temp, (- (t1 - temp)));
                              strncpy ((char *) &output_paragraph[temp],
                                       (char *) &output_paragraph[t1],
                                       (output_paragraph_offset - t1));
                              output_paragraph_offset -= (t1 - temp);
                            }
                        }

                        /* Filled, but now indent if that is right. */
                        if (indented_fill && current_indent)
                          {
                            int buffer_len = ((output_paragraph_offset - temp)
                                              + current_indent);
                            char *temp_buffer = (char *)xmalloc (buffer_len);
                            int indentation = 0;

                            /* We have to shift any markers that are in
                               front of the wrap point. */
                            adjust_braces_following (temp, current_indent);

                            while (current_indent > 0 &&
                                   indentation != current_indent)
                              temp_buffer[indentation++] = ' ';

                            strncpy ((char *) &temp_buffer[current_indent],
                                     (char *) &output_paragraph[temp],
                                     buffer_len - current_indent);

                            if (output_paragraph_offset + buffer_len
                                >= paragraph_buffer_len)
                              {
                                unsigned char *tt = xrealloc
                                  (output_paragraph,
                                   (paragraph_buffer_len += buffer_len));
                                output_paragraph = tt;
                              }
                            strncpy ((char *) &output_paragraph[temp],
                                     temp_buffer, buffer_len);
                            output_paragraph_offset += current_indent;
                            free (temp_buffer);
                          }
                        output_column = 0;
                        while (temp < output_paragraph_offset)
                          output_column +=
                            get_char_len (output_paragraph[temp++]);
                        output_column += len;
                        break;
                      }
                  }
              }
          }

        if (!suppress_insert)
          {
            insert (character);
            last_inserted_character = character;
          }
        last_char_was_newline = 0;
        line_already_broken = 0;
      }
    }
}

/* Insert CHARACTER into `output_paragraph'. */
void
insert (character)
     int character;
{
  output_paragraph[output_paragraph_offset++] = character;
  if (output_paragraph_offset == paragraph_buffer_len)
    {
      output_paragraph =
        xrealloc (output_paragraph, (paragraph_buffer_len += 100));
    }
}

/* Insert the null-terminated string STRING into `output_paragraph'.  */
void
insert_string (string)
     char *string;
{
  while (*string)
    insert (*string++);
}


/* Sentences might have these characters after the period (or whatever).  */
#define post_sentence(c) ((c) == ')' || (c) == '\'' || (c) == '"' \
                          || (c) == ']')

/* Return true if at an end-of-sentence character, possibly followed by
   post-sentence punctuation to ignore.  */
static int
end_of_sentence_p ()
{
  int loc = output_paragraph_offset - 1;
  while (loc > 0 && post_sentence (output_paragraph[loc]))
    loc--;
  return sentence_ender (output_paragraph[loc]);
}


/* Remove up to COUNT characters of whitespace from the
   current output line.  If COUNT is less than zero,
   then remove until none left. */
void
kill_self_indent (count)
     int count;
{
  /* Handle infinite case first. */
  if (count < 0)
    {
      output_column = 0;
      while (output_paragraph_offset)
        {
          if (whitespace (output_paragraph[output_paragraph_offset - 1]))
            output_paragraph_offset--;
          else
            break;
        }
    }
  else
    {
      while (output_paragraph_offset && count--)
        if (whitespace (output_paragraph[output_paragraph_offset - 1]))
          output_paragraph_offset--;
        else
          break;
    }
}

/* Nonzero means do not honor calls to flush_output (). */
static int flushing_ignored = 0;

/* Prevent calls to flush_output () from having any effect. */
void
inhibit_output_flushing ()
{
  flushing_ignored++;
}

/* Allow calls to flush_output () to write the paragraph data. */
void
uninhibit_output_flushing ()
{
  flushing_ignored--;
}

void
flush_output ()
{
  register int i;

  if (!output_paragraph_offset || flushing_ignored)
    return;

  for (i = 0; i < output_paragraph_offset; i++)
    {
      /* If we turned on the 8th bit for a space
         inside @w, turn it back off for output.  */
      if (output_paragraph[i] & meta_character_bit)
        {
          int temp = UNMETA (output_paragraph[i]);
          if (temp == ' ')
            output_paragraph[i] &= 0x7f;
        }
    }

  fwrite (output_paragraph, 1, output_paragraph_offset, output_stream);

  output_position += output_paragraph_offset;
  output_paragraph_offset = 0;
}

/* How to close a paragraph controlling the number of lines between
   this one and the last one. */

/* Paragraph spacing is controlled by this variable.  It is the number of
   blank lines that you wish to appear between paragraphs.  A value of
   1 creates a single blank line between paragraphs. */
int paragraph_spacing = DEFAULT_PARAGRAPH_SPACING;

/* Close the current paragraph, leaving no blank lines between them. */
void
close_single_paragraph ()
{
  close_paragraph_with_lines (0);
}

/* Close a paragraph after an insertion has ended. */
void
close_insertion_paragraph ()
{
  if (!insertion_paragraph_closed)
    {
      /* Close the current paragraph, breaking the line. */
      close_single_paragraph ();

      /* Start a new paragraph, with the correct indentation for the now
         current insertion level (one above the one that we are ending). */
      start_paragraph ();

      /* Tell `close_paragraph' that the previous line has already been
         broken, so it should insert one less newline. */
      line_already_broken = 1;

      /* Tell functions such as `add_char' we've already found a newline. */
      ignore_blank_line ();
    }
  else
    {
      /* If the insertion paragraph is closed already, then we are seeing
         two `@end' commands in a row.  Note that the first one we saw was
         handled in the first part of this if-then-else clause, and at that
         time `start_paragraph' was called, partially to handle the proper
         indentation of the current line.  However, the indentation level
         may have just changed again, so we may have to outdent the current
         line to the new indentation level. */
      if (current_indent < output_column)
        kill_self_indent (output_column - current_indent);
    }

  insertion_paragraph_closed = 1;
}

void
close_paragraph_with_lines (lines)
     int lines;
{
  int old_spacing = paragraph_spacing;
  paragraph_spacing = lines;
  close_paragraph ();
  paragraph_spacing = old_spacing;
}

/* Close the currently open paragraph. */
void
close_paragraph ()
{
  register int i;

  /* The insertion paragraph is no longer closed. */
  insertion_paragraph_closed = 0;

  if (paragraph_is_open && !must_start_paragraph)
    {
      register int tindex, c;

      tindex = output_paragraph_offset;

      /* Back up to last non-newline/space character, forcing all such
         subsequent characters to be newlines.  This isn't strictly
         necessary, but a couple of functions use the presence of a newline
         to make decisions. */
      for (tindex = output_paragraph_offset - 1; tindex >= 0; --tindex)
        {
          c = output_paragraph[tindex];

          if (c == ' '|| c == '\n')
            output_paragraph[tindex] = '\n';
          else
            break;
        }

      /* All trailing whitespace is ignored. */
      output_paragraph_offset = ++tindex;

      /* Break the line if that is appropriate. */
      if (paragraph_spacing >= 0)
        insert ('\n');

      /* Add as many blank lines as is specified in `paragraph_spacing'. */
      if (!force_flush_right)
        {
          for (i = 0; i < (paragraph_spacing - line_already_broken); i++)
            insert ('\n');
        }

      /* If we are doing flush right indentation, then do it now
         on the paragraph (really a single line). */
      if (force_flush_right)
        do_flush_right_indentation ();

      flush_output ();
      paragraph_is_open = 0;
      no_indent = 0;
      output_column = 0;
    }
  ignore_blank_line ();
}

/* Make the last line just read look as if it were only a newline. */
void
ignore_blank_line ()
{
  last_inserted_character = '\n';
  last_char_was_newline = 1;
}

/* Align the end of the text in output_paragraph with fill_column. */
void
do_flush_right_indentation ()
{
  char *temp;
  int temp_len;

  kill_self_indent (-1);

  if (output_paragraph[0] != '\n')
    {
      output_paragraph[output_paragraph_offset] = 0;

      if (output_paragraph_offset < fill_column)
        {
          register int i;

          if (fill_column >= paragraph_buffer_len)
            output_paragraph =
              xrealloc (output_paragraph,
                        (paragraph_buffer_len += fill_column));

          temp_len = strlen ((char *)output_paragraph);
          temp = (char *)xmalloc (temp_len + 1);
          memcpy (temp, (char *)output_paragraph, temp_len);

          for (i = 0; i < fill_column - output_paragraph_offset; i++)
            output_paragraph[i] = ' ';

          memcpy ((char *)output_paragraph + i, temp, temp_len);
          free (temp);
          output_paragraph_offset = fill_column;
        }
    }
}

/* Begin a new paragraph. */
void
start_paragraph ()
{
  /* First close existing one. */
  if (paragraph_is_open)
    close_paragraph ();

  /* In either case, the insertion paragraph is no longer closed. */
  insertion_paragraph_closed = 0;

  /* However, the paragraph is open! */
  paragraph_is_open = 1;

  /* If we MUST_START_PARAGRAPH, that simply means that start_paragraph ()
     had to be called before we would allow any other paragraph operations
     to have an effect. */
  if (!must_start_paragraph)
    {
      int amount_to_indent = 0;

      /* If doing indentation, then insert the appropriate amount. */
      if (!no_indent)
        {
          if (inhibit_paragraph_indentation)
            {
              amount_to_indent = current_indent;
              if (inhibit_paragraph_indentation < 0)
                inhibit_paragraph_indentation++;
            }
          else if (paragraph_start_indent < 0)
            amount_to_indent = current_indent;
          else
            amount_to_indent = current_indent + paragraph_start_indent;

          if (amount_to_indent >= output_column)
            {
              amount_to_indent -= output_column;
              indent (amount_to_indent);
              output_column += amount_to_indent;
            }
        }
    }
  else
    must_start_paragraph = 0;
}

/* Insert the indentation specified by AMOUNT. */
void
indent (amount)
     int amount;
{
  register BRACE_ELEMENT *elt = brace_stack;

  /* For every START_POS saved within the brace stack which will be affected
     by this indentation, bump that start pos forward. */
  while (elt)
    {
      if (elt->pos >= output_paragraph_offset)
        elt->pos += amount;
      elt = elt->next;
    }

  while (--amount >= 0)
    insert (' ');
}

/* Search forward for STRING in input_text.
   FROM says where to start. */
int
search_forward (string, from)
     char *string;
     int from;
{
  int len = strlen (string);

  while (from < size_of_input_text)
    {
      if (strncmp (input_text + from, string, len) == 0)
        return (from);
      from++;
    }
  return (-1);
}

/* Whoops, Unix doesn't have strcasecmp. */

/* Case independent string compare. */
#if !defined (HAVE_STRCASECMP)
int
strcasecmp (string1, string2)
     char *string1, *string2;
{
  char ch1, ch2;

  for (;;)
    {
      ch1 = *string1++;
      ch2 = *string2++;

      if (!(ch1 | ch2))
        return (0);

      ch1 = coerce_to_upper (ch1);
      ch2 = coerce_to_upper (ch2);

      if (ch1 != ch2)
        return (ch1 - ch2);
    }
}
#endif /* !HAVE_STRCASECMP */

void
init_insertion_stack ()
{
  insertion_stack = (INSERTION_ELT *) NULL;
}

/* Return the type of the current insertion. */
enum insertion_type
current_insertion_type ()
{
  if (!insertion_level)
    return (bad_type);
  else
    return (insertion_stack->insertion);
}

/* Return a pointer to the string which is the function to wrap around
   items. */
char *
current_item_function ()
{
  register int level, done;
  register INSERTION_ELT *elt;

  level = insertion_level;
  elt = insertion_stack;
  done = 0;

  /* Skip down through the stack until we find a non-conditional insertion. */
  while (!done && (elt != NULL))
    {
      switch (elt->insertion)
        {
        case ifinfo:
        case ifnothtml:
        case ifnottex:
        case ifset:
        case ifclear:
        case cartouche:
          elt = elt->next;
          level--;
          break;

        default:
          done = 1;
        }
    }

  if (!level)
    return ((char *) NULL);
  else
    return (elt->item_function);
}

char *
get_item_function ()
{
  char *item_function;
  get_rest_of_line (&item_function);
  backup_input_pointer ();
  return (item_function);
}

 /* Push the state of the current insertion on the stack. */
void
push_insertion (type, item_function)
     enum insertion_type type;
     char *item_function;
{
  INSERTION_ELT *new = (INSERTION_ELT *) xmalloc (sizeof (INSERTION_ELT));

  new->item_function = item_function;
  new->filling_enabled = filling_enabled;
  new->indented_fill = indented_fill;
  new->insertion = type;
  new->line_number = line_number;
  new->filename = xstrdup (input_filename);
  new->inhibited = inhibit_paragraph_indentation;
  new->in_fixed_width_font = in_fixed_width_font;
  new->next = insertion_stack;
  insertion_stack = new;
  insertion_level++;
}

 /* Pop the value on top of the insertion stack into the
    global variables. */
void
pop_insertion ()
{
  INSERTION_ELT *temp = insertion_stack;

  if (temp == (INSERTION_ELT *) NULL)
    return;

  in_fixed_width_font = temp->in_fixed_width_font;
  inhibit_paragraph_indentation = temp->inhibited;
  filling_enabled = temp->filling_enabled;
  indented_fill = temp->indented_fill;
  free_and_clear (&(temp->item_function));
  free_and_clear (&(temp->filename));
  insertion_stack = insertion_stack->next;
  free (temp);
  insertion_level--;
}

 /* Return a pointer to the print name of this
    enumerated type. */
char *
insertion_type_pname (type)
     enum insertion_type type;
{
  if ((int) type < (int) bad_type)
    return (insertion_type_names[(int) type]);
  else
    return (_("Broken-Type in insertion_type_pname"));
}

/* Return the insertion_type associated with NAME.
   If the type is not one of the known ones, return BAD_TYPE. */
enum insertion_type
find_type_from_name (name)
     char *name;
{
  int index = 0;
  while (index < (int) bad_type)
    {
      if (strcmp (name, insertion_type_names[index]) == 0)
        return (enum insertion_type) index;
      index++;
    }
  return (bad_type);
}

int
defun_insertion (type)
     enum insertion_type type;
{
  return
    ((type == deffn)
     || (type == defun)
     || (type == defmac)
     || (type == defspec)
     || (type == defvr)
     || (type == defvar)
     || (type == defopt)
     || (type == deftypefn)
     || (type == deftypefun)
     || (type == deftypevr)
     || (type == deftypevar)
     || (type == defcv)
     || (type == defivar)
     || (type == defop)
     || (type == defmethod)
     || (type == deftypemethod)
     || (type == deftp));
}

/* MAX_NS is the maximum nesting level for enumerations.  I picked 100
   which seemed reasonable.  This doesn't control the number of items,
   just the number of nested lists. */
#define max_stack_depth 100
#define ENUM_DIGITS 1
#define ENUM_ALPHA  2
typedef struct {
  int enumtype;
  int enumval;
} DIGIT_ALPHA;

DIGIT_ALPHA enumstack[max_stack_depth];
int enumstack_offset = 0;
int current_enumval = 1;
int current_enumtype = ENUM_DIGITS;
char *enumeration_arg = (char *)NULL;

void
start_enumerating (at, type)
     int at, type;
{
  if ((enumstack_offset + 1) == max_stack_depth)
    {
      line_error (_("Enumeration stack overflow"));
      return;
    }
  enumstack[enumstack_offset].enumtype = current_enumtype;
  enumstack[enumstack_offset].enumval = current_enumval;
  enumstack_offset++;
  current_enumval = at;
  current_enumtype = type;
}

void
stop_enumerating ()
{
  --enumstack_offset;
  if (enumstack_offset < 0)
    enumstack_offset = 0;

  current_enumval = enumstack[enumstack_offset].enumval;
  current_enumtype = enumstack[enumstack_offset].enumtype;
}

/* Place a letter or digits into the output stream. */
void
enumerate_item ()
{
  char temp[10];

  if (current_enumtype == ENUM_ALPHA)
    {
      if (current_enumval == ('z' + 1) || current_enumval == ('Z' + 1))
        {
          current_enumval = ((current_enumval - 1) == 'z' ? 'a' : 'A');
          warning (_("lettering overflow, restarting at %c"), current_enumval);
        }
      sprintf (temp, "%c. ", current_enumval);
    }
  else
    sprintf (temp, "%d. ", current_enumval);

  indent (output_column += (current_indent - strlen (temp)));
  add_word (temp);
  current_enumval++;
}

/* This is where the work for all the "insertion" style
   commands is done.  A huge switch statement handles the
   various setups, and generic code is on both sides. */
void
begin_insertion (type)
     enum insertion_type type;
{
  int no_discard = 0;

  if (defun_insertion (type))
    {
      push_insertion (type, xstrdup (""));
      no_discard++;
    }
  else
    push_insertion (type, get_item_function ());

  switch (type)
    {
    case menu:
      if (!no_headers)
        close_paragraph ();

      filling_enabled = no_indent = 0;
      inhibit_paragraph_indentation = 1;

      if (!no_headers)
        add_word (_("* Menu:\n"));

      in_menu++;
      no_discard++;
      break;

    case detailmenu:
      if (!in_menu)
        {
          if (!no_headers)
            close_paragraph ();

          filling_enabled = no_indent = 0;
          inhibit_paragraph_indentation = 1;

          no_discard++;
        }

      in_detailmenu++;
      break;

    case direntry:
      close_single_paragraph ();
      filling_enabled = no_indent = 0;
      inhibit_paragraph_indentation = 1;
      insert_string ("START-INFO-DIR-ENTRY\n");
      break;

      /* I think @quotation is meant to do filling.
         If you don't want filling, then use @display. */
    case quotation:
      close_single_paragraph ();
      last_char_was_newline = no_indent = 0;
      indented_fill = filling_enabled = 1;
      inhibit_paragraph_indentation = 1;
      current_indent += default_indentation_increment;
      break;

    case display:
    case example:
    case smallexample:
    case lisp:
    case smalllisp:
      /* Just like @example, but no indentation. */
    case format:
      close_single_paragraph ();
      inhibit_paragraph_indentation = 1;
      in_fixed_width_font++;
      filling_enabled = 0;
      last_char_was_newline = 0;
      if (type != format)
        current_indent += default_indentation_increment;
      break;

    case multitable:
      do_multitable ();
      break;

    case table:
    case ftable:
    case vtable:
    case itemize:
      close_single_paragraph ();
      current_indent += default_indentation_increment;
      filling_enabled = indented_fill = 1;
#if defined (INDENT_PARAGRAPHS_IN_TABLE)
      inhibit_paragraph_indentation = 0;
#else
      inhibit_paragraph_indentation = 1;
#endif /* !INDENT_PARAGRAPHS_IN_TABLE */

      /* Make things work for losers who forget the itemize syntax. */
      if (allow_lax_format && (type == itemize))
        {
          if (!(*insertion_stack->item_function))
            {
              free (insertion_stack->item_function);
              insertion_stack->item_function = xstrdup ("@bullet");
              insertion_stack->item_function[0] = COMMAND_PREFIX;
            }
        }

      if (!*insertion_stack->item_function)
        {
          line_error (_("%s requires an argument: the formatter for %citem"),
                      insertion_type_pname (type), COMMAND_PREFIX);
        }
      break;

    case enumerate:
      close_single_paragraph ();
      no_indent = 0;
#if defined (INDENT_PARAGRAPHS_IN_TABLE)
      inhibit_paragraph_indentation = 0;
#else
      inhibit_paragraph_indentation = 1;
#endif /* !INDENT_PARAGRAPHS_IN_TABLE */

      current_indent += default_indentation_increment;
      filling_enabled = indented_fill = 1;

      if (isdigit (*enumeration_arg))
        start_enumerating (atoi (enumeration_arg), ENUM_DIGITS);
      else
        start_enumerating (*enumeration_arg, ENUM_ALPHA);
      break;

      /* Does nothing special in makeinfo. */
    case group:
      /* Only close the paragraph if we are not inside of an @example. */
      if (!insertion_stack->next ||
          insertion_stack->next->insertion != example)
        close_single_paragraph ();
      break;

      /* Insertions that are no-ops in info, but do something in TeX. */
    case ifinfo:
    case ifnothtml:
    case ifnottex:
    case ifset:
    case ifclear:
    case cartouche:
      if (in_menu)
        no_discard++;
      break;

    case deffn:
    case defun:
    case defmac:
    case defspec:
    case defvr:
    case defvar:
    case defopt:
    case deftypefn:
    case deftypefun:
    case deftypevr:
    case deftypevar:
    case defcv:
    case defivar:
    case defop:
    case defmethod:
    case deftypemethod:
    case deftp:
      inhibit_paragraph_indentation = 1;
      filling_enabled = indented_fill = 1;
      current_indent += default_indentation_increment;
      no_indent = 0;
      break;

    case flushleft:
      close_single_paragraph ();
      inhibit_paragraph_indentation = 1;
      filling_enabled = indented_fill = no_indent = 0;
      break;

    case flushright:
      close_single_paragraph ();
      filling_enabled = indented_fill = no_indent = 0;
      inhibit_paragraph_indentation = 1;
      force_flush_right++;
      break;
    }

  if (!no_discard)
    discard_until ("\n");
}

/* Try to end the insertion with the specified TYPE.  With a value of
   `bad_type', TYPE gets translated to match the value currently on top
   of the stack.  Otherwise, if TYPE doesn't match the top of the
   insertion stack, give error. */
void
end_insertion (type)
     enum insertion_type type;
{
  enum insertion_type temp_type;

  if (!insertion_level)
    return;

  temp_type = current_insertion_type ();

  if (type == bad_type)
    type = temp_type;

  if (type != temp_type)
    {
      line_error
        (_("`%cend' expected `%s', but saw `%s'"), COMMAND_PREFIX,
         insertion_type_pname (temp_type), insertion_type_pname (type));
      return;
    }

  pop_insertion ();

  switch (type)
    {
      /* Insertions which have no effect on paragraph formatting. */
    case ifnothtml:
    case ifnottex:
    case ifinfo:
    case ifset:
    case ifclear:
      break;

    case direntry:
      insert_string ("END-INFO-DIR-ENTRY\n\n");
      close_insertion_paragraph ();
      break;

    case detailmenu:
      in_detailmenu--;          /* No longer hacking menus. */
      if (!in_menu)
        {
          if (!no_headers)
            close_insertion_paragraph ();
        }
      break;

    case menu:
      in_menu--;                /* No longer hacking menus. */
      if (!no_headers)
        close_insertion_paragraph ();
      break;

    case multitable:
      end_multitable ();
      break;

    case enumerate:
      stop_enumerating ();
      close_insertion_paragraph ();
      current_indent -= default_indentation_increment;
      break;

    case flushleft:
    case group:
    case cartouche:
      close_insertion_paragraph ();
      break;

    case format:
    case display:
    case example:
    case smallexample:
    case lisp:
    case smalllisp:
    case quotation:
      /* @format is the only fixed_width insertion without a change
         in indentation. */
      if (type != format)
        current_indent -= default_indentation_increment;

      /* The ending of one of these insertions always marks the
         start of a new paragraph. */
      close_insertion_paragraph ();
      break;

    case table:
    case ftable:
    case vtable:
    case itemize:
      current_indent -= default_indentation_increment;
      break;

    case flushright:
      force_flush_right--;
      close_insertion_paragraph ();
      break;

      /* Handle the @defun style insertions with a default clause. */
    default:
      current_indent -= default_indentation_increment;
      close_insertion_paragraph ();
      break;
    }
}

/* Insertions cannot cross certain boundaries, such as node beginnings.  In
   code that creates such boundaries, you should call `discard_insertions'
   before doing anything else.  It prints the errors for you, and cleans up
   the insertion stack.  With nonzero SPECIALS_OK, allows unmatched
   ifinfo, ifset, ifclear, otherwise not.  */
void
discard_insertions (specials_ok)
    int specials_ok;
{
  int real_line_number = line_number;
  while (insertion_stack)
    {
      if (specials_ok && (insertion_stack->insertion == ifinfo
          || insertion_stack->insertion == ifset
          || insertion_stack->insertion == ifclear))
        break;
      else
        {
          char *offender = insertion_type_pname (insertion_stack->insertion);
          char *current_filename = input_filename;
          
          input_filename = insertion_stack->filename;
          line_number = insertion_stack->line_number;
          line_error (_("No matching `%cend %s'"), COMMAND_PREFIX, offender);
          input_filename = current_filename;
          pop_insertion ();
        }
    }
  line_number = real_line_number;
}

/* The Texinfo commands. */

/* Commands which insert their own names. */
void
insert_self (arg)
    int arg;
{
  if (arg == START)
    add_word (command);
}

void
insert_space (arg)
    int arg;
{
  if (arg == START)
    add_char (' ');
}

/* Force a line break in the output. */
void
cm_asterisk ()
{
  close_single_paragraph ();
  cm_noindent ();
}

/* Insert ellipsis. */
void
cm_dots (arg)
     int arg;
{
  if (arg == START)
    add_word ("...");
}

/* Insert ellipsis for sentence end. */
void
cm_enddots (arg)
     int arg;
{
  if (arg == START)
    add_word ("....");
}

void
cm_bullet (arg)
     int arg;
{
  if (arg == START)
    add_char ('*');
}

void
cm_minus (arg)
     int arg;
{
  if (arg == START)
    add_char ('-');
}

/* Insert "TeX". */
void
cm_TeX (arg)
     int arg;
{
  if (arg == START)
    add_word ("TeX");
}

/* Copyright symbol.  */
void
cm_copyright (arg)
    int arg;
{
  if (arg == START)
    add_word ("(C)");
}

/* Accent commands that take explicit arguments.  */
void
cm_accent (arg)
    int arg;
{
  if (arg == START)
    {
      if (strcmp (command, "dotaccent") == 0)  /* overdot */
        add_char ('.');
      else if (strcmp (command, "H") == 0)     /* Hungarian umlaut */
        add_word ("''");
      else if (strcmp (command, "ringaccent") == 0)
        add_char ('*');
      else if (strcmp (command, "tieaccent") == 0)
        add_char ('[');
      else if (strcmp (command, "u") == 0)     /* breve */
        add_char ('(');
      else if (strcmp (command, "v") == 0)     /* hacek/check */
        add_char ('<');
    }
  else if (arg == END)
    {
      if (strcmp (command, "ubaraccent") == 0) /* underbar */
        add_char ('_');
      else if (strcmp (command, "udotaccent") == 0) /* underdot */
        add_word ("-.");
      else if (strcmp (command, ",") == 0)     /* cedilla */
        add_word (",");
    }
} 

/* Non-English letters/characters that don't insert themselves.  */
void
cm_special_char (arg)
{
  if (arg == START)
    {
      if ((*command == 'L' || *command == 'l'
           || *command == 'O' || *command == 'o')
          && command[1] == 0)
        {
          /* Lslash lslash Oslash oslash */
          add_char (*command);
          add_char ('/');
        }
      else if (strcmp (command, "exclamdown") == 0)
        add_char ('!');
      else if (strcmp (command, "pounds") == 0)
        add_char ('#');
      else if (strcmp (command, "questiondown") == 0)
        add_char ('?');
      else
        fprintf (stderr, _("How did @%s end up in cm_special_char?\n"), command);
    }
}

/* Dotless i or j.  */
void
cm_dotless (arg, start, end)
    int arg, start, end;
{
  if (arg == END)
    {
      if (output_paragraph[start] != 'i' && output_paragraph[start] != 'j')
        /* This error message isn't perfect if the argument is multiple
           characters, but it doesn't seem worth getting right.  */
        line_error (_("%c%s expects `i' or `j' as argument, not `%c'"),
                    COMMAND_PREFIX, command, output_paragraph[start]);

      else if (end - start != 1)
        line_error (_("%c%s expects a single character `i' or `j' as argument"),
                    COMMAND_PREFIX, command);

      /* We've already inserted the `i' or `j', so nothing to do.  */
    }
}

void
cm_today (arg)
     int arg;
{
  static char *months [12] =
    { N_("January"), N_("February"), N_("March"), N_("April"), N_("May"),
      N_("June"), N_("July"), N_("August"), N_("September"), N_("October"),
      N_("November"), N_("December") };
  if (arg == START)
    {
      time_t timer = time (0);
      struct tm *ts = localtime (&timer);
      add_word_args ("%d %s %d", ts->tm_mday, _(months[ts->tm_mon]),
                     ts->tm_year + 1900);
    }
}

void
cm_code (arg)
     int arg;
{
  extern int printing_index;

  if (arg == START)
    {
      in_fixed_width_font++;

      if (!printing_index)
        add_char ('`');
    }
  else
    {
      if (!printing_index)
        add_char ('\'');
    }
}

void
cm_kbd (arg)
     int arg;
{
  /* People use @kbd in an example to get the "user input" font.
     We don't want quotes in that case.  */
  if (!in_fixed_width_font)
    cm_code (arg);
}

void
cm_key (arg)
     int arg;
{
  add_char (arg == START ? '<' : '>');
}

/* Convert the character at position into a true control character. */
void
cm_ctrl (arg, start, end)
     int arg, start, end;
{
  /* Should we allow multiple character arguments?  I think yes. */
  if (arg == END)
    {
      register int i, character;
#if defined (NO_MULTIPLE_CTRL)
      if ((end - start) != 1)
        line_error (_("%c%s expects a single character as an argument"),
                    COMMAND_PREFIX, command);
      else
#endif
        for (i = start; i < end; i++)
          {
            character = output_paragraph[i];

            if (isletter (character))
              output_paragraph[i] = CTL (coerce_to_upper (character));
          }
    }
}

/* Handle a command that switches to a non-fixed-width font.  */
void
not_fixed_width (arg)
     int arg;
{
  if (arg == START)
    in_fixed_width_font = 0;
}

/* Small caps and @var in makeinfo just uppercase the text. */
void
cm_var_sc (arg, start_pos, end_pos)
     int arg, start_pos, end_pos;
{
  not_fixed_width (arg);

  if (arg == END)
    {
      while (start_pos < end_pos)
        {
          output_paragraph[start_pos] =
            coerce_to_upper (output_paragraph[start_pos]);
          start_pos++;
        }
    }
}

void
cm_dfn (arg, position)
     int arg, position;
{
  add_char ('"');
}

void
cm_emph (arg)
     int arg;
{
  add_char ('*');
}

void
cm_strong (arg, position)
     int arg, position;
{
  cm_emph (arg);
}

void
cm_cite (arg, position)
     int arg, position;
{
  if (arg == START)
    add_word ("`");
  else
    add_word ("'");
}

/* No highlighting, but argument switches fonts.  */
void
cm_not_fixed_width (arg, start, end)
     int arg, start, end;
{
  not_fixed_width (arg);
}

/* Various commands are no-op's. */
void
cm_no_op ()
{
}

/* No-op that eats its argument on same line.  */
void
cm_no_op_line_arg ()
{
  char *temp;
  get_rest_of_line (&temp);
  free (temp);
}

/* Prevent the argument from being split across two lines. */
void
cm_w (arg, start, end)
     int arg, start, end;
{
  if (arg == START)
    non_splitting_words++;
  else
    non_splitting_words--;
}


/* Explain that this command is obsolete, thus the user shouldn't
   do anything with it. */
void
cm_obsolete (arg, start, end)
     int arg, start, end;
{
  if (arg == START)
    warning (_("%c%s is obsolete"), COMMAND_PREFIX, command);
}

/* Insert the text following input_text_offset up to the end of the line
   in a new, separate paragraph.  Directly underneath it, insert a
   line of WITH_CHAR, the same length of the inserted text. */
void
insert_and_underscore (with_char)
     int with_char;
{
  register int i, len;
  int old_no_indent, starting_pos, ending_pos;
  char *temp;

  close_paragraph ();
  filling_enabled =  indented_fill = 0;
  old_no_indent = no_indent;
  no_indent = 1;

#if defined (HAVE_MACROS)
  if (macro_expansion_output_stream && !executing_string)
    append_to_expansion_output (input_text_offset + 1);
#endif /* HAVE_MACROS */

  get_rest_of_line (&temp);

  starting_pos = output_position + output_paragraph_offset;
#if defined (HAVE_MACROS)
  if (macro_expansion_output_stream && !executing_string)
    {
      char *temp1 = (char *) xmalloc (2 + strlen (temp));
      sprintf (temp1, "%s\n", temp);
      remember_itext (input_text, input_text_offset);
      me_execute_string (temp1);
      free (temp1);
    }
  else
#endif /* HAVE_MACROS */
    execute_string ("%s\n", temp);

  ending_pos = output_position + output_paragraph_offset;
  free (temp);

  len = (ending_pos - starting_pos) - 1;
  for (i = 0; i < len; i++)
    add_char (with_char);
  insert ('\n');
  close_paragraph ();
  filling_enabled = 1;
  no_indent = old_no_indent;
}

/* Here is a structure which associates sectioning commands with
   an integer, hopefully to reflect the `depth' of the current
   section. */
struct {
  char *name;
  int level;
} section_alist[] = {
  { "unnumberedsubsubsec", 5 },
  { "unnumberedsubsec", 4 },
  { "unnumberedsec", 3 },
  { "unnumbered", 2 },
  { "appendixsubsubsec", 5 },
  { "appendixsubsec", 4 },
  { "appendixsec", 3 },
  { "appendixsection", 3 },
  { "appendix", 2 },
  { "subsubsec", 5 },
  { "subsubsection", 5 },
  { "subsection", 4 },
  { "section", 3 },
  { "chapter", 2 },
  { "top", 1 },

  { (char *)NULL, 0 }
};

/* Amount to offset the name of sectioning commands to levels by. */
int section_alist_offset = 0;

/* Shift the meaning of @section to @chapter. */
void
cm_raisesections ()
{
  discard_until ("\n");
  section_alist_offset--;
}

/* Shift the meaning of @chapter to @section. */
void
cm_lowersections ()
{
  discard_until ("\n");
  section_alist_offset++;
}

/* Return an integer which identifies the type section present in TEXT. */
int
what_section (text)
     char *text;
{
  register int i, j;
  char *t;

 find_section_command:
  for (j = 0; text[j] && cr_or_whitespace (text[j]); j++);
  if (text[j] != COMMAND_PREFIX)
    return (-1);

  text = text + j + 1;

  /* We skip @c, @comment, and @?index commands. */
  if ((strncmp (text, "comment", strlen ("comment")) == 0) ||
      (text[0] == 'c' && cr_or_whitespace (text[1])) ||
      (strcmp (text + 1, "index") == 0))
    {
      while (*text++ != '\n');
      goto find_section_command;
    }

  /* Handle italicized sectioning commands. */
  if (*text == 'i')
    text++;

  for (j = 0; text[j] && !cr_or_whitespace (text[j]); j++);

  for (i = 0; (t = section_alist[i].name); i++)
    {
      if (j == strlen (t) && strncmp (t, text, j) == 0)
        {
          int return_val;

          return_val = (section_alist[i].level + section_alist_offset);

          if (return_val < 0)
            return_val = 0;
          else if (return_val > 5)
            return_val = 5;
          return (return_val);
        }
    }
  return (-1);
}

/* Set the level of @top to LEVEL.  Return the old level of @top. */
int
set_top_section_level (level)
     int level;
{
  register int i, result = -1;

  for (i = 0; section_alist[i].name; i++)
    if (strcmp (section_alist[i].name, "top") == 0)
      {
        result = section_alist[i].level;
        section_alist[i].level = level;
        break;
      }
  return (result);
}

/* Treat this just like @unnumbered.  The only difference is
   in node defaulting. */
void
cm_top ()
{
  /* It is an error to have more than one @top. */
  if (top_node_seen)
    {
      TAG_ENTRY *tag = tag_table;

      line_error (_("Node with %ctop as a section already exists"),
                  COMMAND_PREFIX);

      while (tag != (TAG_ENTRY *)NULL)
        {
          if ((tag->flags & IS_TOP))
            {
              int old_line_number = line_number;
              char *old_input_filename = input_filename;

              line_number = tag->line_no;
              input_filename = tag->filename;
              line_error (_("Here is the %ctop node"), COMMAND_PREFIX);
              input_filename = old_input_filename;
              line_number = old_line_number;
              return;
            }
          tag = tag->next_ent;
        }
    }
  else
    {
      top_node_seen = 1;

      /* It is an error to use @top before you have used @node. */
      if (!tag_table)
        {
          char *top_name;

          get_rest_of_line (&top_name);
          free (top_name);
          line_error (_("%ctop used before %cnode, defaulting to %s"),
                      COMMAND_PREFIX, COMMAND_PREFIX, top_name);
          execute_string ("@node Top, , (dir), (dir)\n@top %s\n", top_name);
          return;
        }

      cm_unnumbered ();

      /* The most recently defined node is the top node. */
      tag_table->flags |= IS_TOP;

      /* Now set the logical hierarchical level of the Top node. */
      {
        int orig_offset = input_text_offset;

        input_text_offset = search_forward (node_search_string, orig_offset);

        if (input_text_offset > 0)
          {
            int this_section;

            /* We have encountered a non-top node, so mark that one exists. */
            non_top_node_seen = 1;

            /* Move to the end of this line, and find out what the
               sectioning command is here. */
            while (input_text[input_text_offset] != '\n')
              input_text_offset++;

            if (input_text_offset < size_of_input_text)
              input_text_offset++;

            this_section = what_section (input_text + input_text_offset);

            /* If we found a sectioning command, then give the top section
               a level of this section - 1. */
            if (this_section != -1)
              set_top_section_level (this_section - 1);
          }
        input_text_offset = orig_offset;
      }
    }
}

/* Organized by level commands.  That is, "*" == chapter, "=" == section. */
char *scoring_characters = "*=-.";

void
sectioning_underscore (command)
     char *command;
{
  char character;
  char *temp;
  int level;

  temp = (char *)xmalloc (2 + strlen (command));
  temp[0] = COMMAND_PREFIX;
  strcpy (&temp[1], command);
  level = what_section (temp);
  free (temp);
  level -= 2;

  if (level < 0)
    level = 0;

  character = scoring_characters[level];

  insert_and_underscore (character);
}

/* The command still works, but prints a warning message in addition. */
void
cm_ideprecated (arg, start, end)
     int arg, start, end;
{
  warning (_("%c%s is obsolete; use %c%s instead"),
           COMMAND_PREFIX, command, COMMAND_PREFIX, command + 1);
  sectioning_underscore (command + 1);
}

/* The remainder of the text on this line is a chapter heading. */
void
cm_chapter ()
{
  sectioning_underscore ("chapter");
}

/* The remainder of the text on this line is a section heading. */
void
cm_section ()
{
  sectioning_underscore ("section");
}

/* The remainder of the text on this line is a subsection heading. */
void
cm_subsection ()
{
  sectioning_underscore ("subsection");
}

/* The remainder of the text on this line is a subsubsection heading. */
void
cm_subsubsection ()
{
  sectioning_underscore ("subsubsection");
}

/* The remainder of the text on this line is an unnumbered heading. */
void
cm_unnumbered ()
{
  cm_chapter ();
}

/* The remainder of the text on this line is an unnumbered section heading. */
void
cm_unnumberedsec ()
{
  cm_section ();
}

/* The remainder of the text on this line is an unnumbered
   subsection heading. */
void
cm_unnumberedsubsec ()
{
  cm_subsection ();
}

/* The remainder of the text on this line is an unnumbered
   subsubsection heading. */
void
cm_unnumberedsubsubsec ()
{
  cm_subsubsection ();
}

/* The remainder of the text on this line is an appendix heading. */
void
cm_appendix ()
{
  cm_chapter ();
}

/* The remainder of the text on this line is an appendix section heading. */
void
cm_appendixsec ()
{
  cm_section ();
}

/* The remainder of the text on this line is an appendix subsection heading. */
void
cm_appendixsubsec ()
{
  cm_subsection ();
}

/* The remainder of the text on this line is an appendix
   subsubsection heading. */
void
cm_appendixsubsubsec ()
{
  cm_subsubsection ();
}

/* Compatibility functions substitute for chapter, section, etc. */
void
cm_majorheading ()
{
  cm_chapheading ();
}

void
cm_chapheading ()
{
  cm_chapter ();
}

void
cm_heading ()
{
  cm_section ();
}

void
cm_subheading ()
{
  cm_subsection ();
}

void
cm_subsubheading ()
{
  cm_subsubsection ();
}

/* **************************************************************** */
/*                                                                  */
/*                 Adding nodes, and making tags                    */
/*                                                                  */
/* **************************************************************** */

/* Start a new tag table. */
void
init_tag_table ()
{
  while (tag_table != (TAG_ENTRY *) NULL)
    {
      TAG_ENTRY *temp = tag_table;
      free (temp->node);
      free (temp->prev);
      free (temp->next);
      free (temp->up);
      tag_table = tag_table->next_ent;
      free (temp);
    }
}

void
write_tag_table ()
{
  write_tag_table_internal (0); /* Not indirect. */
}

void
write_tag_table_indirect ()
{
  write_tag_table_internal (1);
}

/* Write out the contents of the existing tag table.
   INDIRECT_P says how to format the output. */
void
write_tag_table_internal (indirect_p)
     int indirect_p;
{
  TAG_ENTRY *node = tag_table;
  int old_indent = no_indent;

  no_indent = 1;
  filling_enabled = 0;
  must_start_paragraph = 0;
  close_paragraph ();

  if (!indirect_p)
    {
      no_indent = 1;
      insert ('\n');
    }

  add_word_args ("\037\nTag Table:\n%s", indirect_p ? "(Indirect)\n" : "");

  while (node != (TAG_ENTRY *) NULL)
    {
      execute_string ("Node: %s", node->node);
      add_word_args ("\177%d\n", node->position);
      node = node->next_ent;
    }

  add_word ("\037\nEnd Tag Table\n");
  flush_output ();
  no_indent = old_indent;
}

char *
get_node_token (expand)
      int expand;
{
  char *string;

  get_until_in_line (expand, ",", &string);

  if (curchar () == ',')
    input_text_offset++;

  canon_white (string);

  /* Force all versions of "top" to be "Top". */
  normalize_node_name (string);

  return (string);
}

/* Convert "top" and friends into "Top". */
void
normalize_node_name (string)
     char *string;
{
  if (strcasecmp (string, "Top") == 0)
    strcpy (string, "Top");
}

/* Look up NAME in the tag table, and return the associated
   tag_entry.  If the node is not in the table return NULL. */
TAG_ENTRY *
find_node (name)
     char *name;
{
  TAG_ENTRY *tag = tag_table;

  while (tag != (TAG_ENTRY *) NULL)
    {
      if (strcmp (tag->node, name) == 0)
        return (tag);
      tag = tag->next_ent;
    }
  return ((TAG_ENTRY *) NULL);
}

/* Remember NODE and associates. */
void
remember_node (node, prev, next, up, position, line_no, no_warn)
     char *node, *prev, *next, *up;
     int position, line_no, no_warn;
{
  /* Check for existence of this tag already. */
  if (validating)
    {
      register TAG_ENTRY *tag = find_node (node);
      if (tag)
        {
          line_error (
                 _("Node `%s' multiply defined (line %d is first definition at)"),
                      node, tag->line_no);
          return;
        }
    }

  /* First, make this the current node. */
  current_node = node;

  /* Now add it to the list. */
  {
    TAG_ENTRY *new = (TAG_ENTRY *) xmalloc (sizeof (TAG_ENTRY));
    new->node = node;
    new->prev = prev;
    new->next = next;
    new->up = up;
    new->position = position;
    new->line_no = line_no;
    new->filename = node_filename;
    new->touched = 0;           /* not yet referenced. */
    new->flags = 0;
    if (no_warn)
      new->flags |= NO_WARN;
    new->next_ent = tag_table;
    tag_table = new;
  }
}

/* The order is: nodename, nextnode, prevnode, upnode.
   If all of the NEXT, PREV, and UP fields are empty, they are defaulted.
   You must follow a node command which has those fields defaulted
   with a sectioning command (e.g. @chapter) giving the "level" of that node.
   It is an error not to do so.
   The defaults come from the menu in this node's parent. */
void
cm_node ()
{
  char *node, *prev, *next, *up;
  int new_node_pos, defaulting, this_section, no_warn = 0;
  extern int already_outputting_pending_notes;

  if (strcmp (command, "nwnode") == 0)
    no_warn = 1;

  /* Get rid of unmatched brace arguments from previous commands. */
  discard_braces ();

  /* There also might be insertions left lying around that haven't been
     ended yet.  Do that also. */
  discard_insertions (1);

  if (!already_outputting_pending_notes)
    {
      close_paragraph ();
      output_pending_notes ();
      free_pending_notes ();
    }

  filling_enabled = indented_fill = 0;
  new_node_pos = output_position;
  current_footnote_number = 1;

#if defined (HAVE_MACROS)
  if (macro_expansion_output_stream && !executing_string)
    append_to_expansion_output (input_text_offset + 1);
#endif /* HAVE_MACROS */

  node = get_node_token (1);
  next = get_node_token (0);
  prev = get_node_token (0);
  up = get_node_token (0);

  if (verbose_mode)
    printf (_("Formatting node %s...\n"), node);
    
#if defined (HAVE_MACROS)
  if (macro_expansion_output_stream && !executing_string)
    remember_itext (input_text, input_text_offset);
#endif /* HAVE_MACROS */

  no_indent = 1;
  if (!no_headers)
    {
      add_word_args ("\037\nFile: %s,  Node: ", pretty_output_filename);

#if defined (HAVE_MACROS)
      if (macro_expansion_output_stream && !executing_string)
        me_execute_string (node);
      else
#endif /* HAVE_MACROS */
        execute_string ("%s", node);
      filling_enabled = indented_fill = 0;
    }

  /* Check for defaulting of this node's next, prev, and up fields. */
  defaulting = (*next == 0 && *prev == 0 && *up == 0);

  this_section = what_section (input_text + input_text_offset);

  /* If we are defaulting, then look at the immediately following
     sectioning command (error if none) to determine the node's
     level.  Find the node that contains the menu mentioning this node
     that is one level up (error if not found).  That node is the "Up"
     of this node.  Default the "Next" and "Prev" from the menu. */
  if (defaulting)
    {
      NODE_REF *last_ref = (NODE_REF *)NULL;
      NODE_REF *ref = node_references;

      if ((this_section < 0) && (strcmp (node, "Top") != 0))
        {
          char *polite_section_name = "top";
          int i;

          for (i = 0; section_alist[i].name; i++)
            if (section_alist[i].level == current_section + 1)
              {
                polite_section_name = section_alist[i].name;
                break;
              }

          line_error
            (_("Node `%s' requires a sectioning command (e.g. %c%s)"),
             node, COMMAND_PREFIX, polite_section_name);
        }
      else
        {
          if (strcmp (node, "Top") == 0)
            {
              /* Default the NEXT pointer to be the first menu item in
                 this node, if there is a menu in this node.  We have to
                 try very hard to find the menu, as it may be obscured
                 by execution_strings which are on the filestack.  For
                 every member of the filestack which has a FILENAME
                 member which is identical to the current INPUT_FILENAME,
                 search forward from that offset. */
              int saved_input_text_offset = input_text_offset;
              int saved_size_of_input_text = size_of_input_text;
              char *saved_input_text = input_text;
              FSTACK *next_file = filestack;

              int orig_offset, orig_size;
              char *glean_node_from_menu ();

              /* No matter what, make this file point back at `(dir)'. */
              free (up);   up = xstrdup ("(dir)");

              while (1)
                {
                  orig_offset = input_text_offset;
                  orig_size =
                    search_forward (node_search_string, orig_offset);

                  if (orig_size < 0)
                    orig_size = size_of_input_text;

                  input_text_offset =
                    search_forward (menu_search_string, orig_offset);

                  if (input_text_offset > -1)
                    {
                      char *nodename_from_menu = (char *)NULL;

                      input_text_offset =
                        search_forward ("\n* ", input_text_offset);

                      if (input_text_offset != -1)
                        nodename_from_menu = glean_node_from_menu (0);

                      if (nodename_from_menu)
                        {
                          free (next); next = nodename_from_menu;
                          break;
                        }
                    }

                  /* We got here, so it hasn't been found yet.  Try
                     the next file on the filestack if there is one. */
                  if (next_file &&
                      (strcmp (next_file->filename, input_filename) == 0))
                    {
                      input_text = next_file->text;
                      input_text_offset = next_file->offset;
                      size_of_input_text = next_file->size;
                      next_file = next_file->next;
                    }
                  else
                    {
                      /* No more input files to check. */
                      break;
                    }
                }

              input_text = saved_input_text;
              input_text_offset = saved_input_text_offset;
              size_of_input_text = saved_size_of_input_text;
            }
        }

      /* Fix the level of the menu references in the Top node, iff it
         was declared with @top, and no subsequent reference was found. */
      if (top_node_seen && !non_top_node_seen)
        {
          /* Then this is the first non-@top node seen. */
          int level;

          level = set_top_section_level (this_section - 1);
          non_top_node_seen = 1;

          while (ref)
            {
              if (ref->section == level)
                ref->section = this_section - 1;
              ref = ref->next;
            }

          ref = node_references;
        }

      while (ref)
        {
          if (ref->section == (this_section - 1) &&
              ref->type == menu_reference &&
              strcmp (ref->node, node) == 0)
            {
              char *containing_node = ref->containing_node;

              free (up);
              up = xstrdup (containing_node);

              if (last_ref &&
                  last_ref->type == menu_reference &&
                  (strcmp (last_ref->containing_node,
                           containing_node) == 0))
                {
                  free (next);
                  next = xstrdup (last_ref->node);
                }

              while ((ref->section == this_section - 1) &&
                     (ref->next) &&
                     (ref->next->type != menu_reference))
                ref = ref->next;

              if (ref->next && ref->type == menu_reference &&
                  (strcmp (ref->next->containing_node,
                           containing_node) == 0))
                {
                  free (prev);
                  prev = xstrdup (ref->next->node);
                }
              else if (!ref->next &&
                       strcasecmp (ref->containing_node, "Top") == 0)
                {
                  free (prev);
                  prev = xstrdup (ref->containing_node);
                }
              break;
            }
          last_ref = ref;
          ref = ref->next;
        }
    }

#if defined (HAVE_MACROS)
  /* Insert the correct args if we are expanding macros, and the node's
     pointers weren't defaulted. */
  if (macro_expansion_output_stream && !executing_string && !defaulting)
    {
      char *temp;
      int op_orig = output_paragraph_offset;

      temp = (char *)xmalloc (3 + strlen (next));
      sprintf (temp, ", %s", next);
      me_execute_string (temp);
      free (temp);

      temp = (char *)xmalloc (3 + strlen (prev));
      sprintf (temp, ", %s", prev);
      me_execute_string (temp);
      free (temp);

      temp = (char *)xmalloc (4 + strlen (up));
      sprintf (temp, ", %s", up);
      me_execute_string (temp);
      free (temp);

      output_paragraph_offset = op_orig;
    }
#endif /* HAVE_MACROS */

  if (!no_headers)
    {
#if defined (HAVE_MACROS)
      if (macro_expansion_output_stream)
        me_inhibit_expansion++;
#endif /* HAVE_MACROS */

      if (*next)
        {
          execute_string (",  Next: %s", next);
          filling_enabled = indented_fill = 0;
        }

      if (*prev)
        {
          execute_string (",  Prev: %s", prev);
          filling_enabled = indented_fill = 0;
        }

      if (*up)
        {
          execute_string (",  Up: %s", up);
          filling_enabled = indented_fill = 0;
        }
#if defined (HAVE_MACROS)
      if (macro_expansion_output_stream)
        me_inhibit_expansion--;
#endif /* HAVE_MACROS */
    }

  close_paragraph ();
  no_indent = 0;

  if (!*node)
    {
      line_error ("No node name specified for `%c%s' command",
                  COMMAND_PREFIX, command);
      free (node);
      free (next);
      free (prev);
      free (up);
    }
  else
    {
      if (!*next) { free (next); next = (char *)NULL; }
      if (!*prev) { free (prev); prev = (char *)NULL; }
      if (!*up) { free (up); up = (char *)NULL; }
      remember_node (node, prev, next, up, new_node_pos, line_number, no_warn);
    }

  /* Change the section only if there was a sectioning command. */
  if (this_section >= 0)
    current_section = this_section;

  filling_enabled = 1;
}

/* Validation of an info file.
   Scan through the list of tag entries touching the Prev, Next, and Up
   elements of each.  It is an error not to be able to touch one of them,
   except in the case of external node references, such as "(DIR)".

   If the Prev is different from the Up,
   then the Prev node must have a Next pointing at this node.

   Every node except Top must have an Up.
   The Up node must contain some sort of reference, other than a Next,
   to this node.

   If the Next is different from the Next of the Up,
   then the Next node must have a Prev pointing at this node. */
void
validate_file (tag_table)
     TAG_ENTRY *tag_table;
{
  char *old_input_filename = input_filename;
  TAG_ENTRY *tags = tag_table;

  while (tags != (TAG_ENTRY *) NULL)
    {
      register TAG_ENTRY *temp_tag;

      input_filename = tags->filename;
      line_number = tags->line_no;

      /* If this is a "no warn" node, don't validate it in any way. */
      if (tags->flags & NO_WARN)
        {
          tags = tags->next_ent;
          continue;
        }

      /* If this node has a Next, then make sure that the Next exists. */
      if (tags->next)
        {
          validate (tags->next, tags->line_no, "Next");

          /* If the Next node exists, and there is no Up, then make
             sure that the Prev of the Next points back. */
          temp_tag = find_node (tags->next);
          if (temp_tag)
            {
              char *prev;

              if (temp_tag->flags & NO_WARN)
                {
                  /* Do nothing if we aren't supposed to issue warnings
                     about this node. */
                }
              else
                {
                  prev = temp_tag->prev;
                  if (!prev || (strcmp (prev, tags->node) != 0))
                    {
                      line_error (_("Node `%s''s Next field not pointed back to"),
                                  tags->node);
                      line_number = temp_tag->line_no;
                      input_filename = temp_tag->filename;
                      line_error
                        (_("This node (`%s') is the one with the bad `Prev'"),
                         temp_tag->node);
                      input_filename = tags->filename;
                      line_number = tags->line_no;
                      temp_tag->flags |= PREV_ERROR;
                    }
                }
            }
        }

      /* Validate the Prev field if there is one, and we haven't already
         complained about it in some way.  You don't have to have a Prev
         field at this stage. */
      if (!(tags->flags & PREV_ERROR) && tags->prev)
        {
          int valid_p = validate (tags->prev, tags->line_no, "Prev");

          if (!valid_p)
            tags->flags |= PREV_ERROR;
          else
            {
              /* If the Prev field is not the same as the Up field,
                 then the node pointed to by the Prev field must have
                 a Next field which points to this node. */
              if (tags->up && (strcmp (tags->prev, tags->up) != 0))
                {
                  temp_tag = find_node (tags->prev);

                  /* If we aren't supposed to issue warnings about the
                     target node, do nothing. */
                  if (!temp_tag || (temp_tag->flags & NO_WARN))
                    {
                      /* Do nothing. */
                    }
                  else
                    {
                      if (!temp_tag->next ||
                          (strcmp (temp_tag->next, tags->node) != 0))
                        {
                          line_error
                            (_("Node `%s's Prev field not pointed back to"),
                             tags->node);
                          line_number = temp_tag->line_no;
                          input_filename = temp_tag->filename;
                          line_error (_("This node (`%s') has the bad Next"),
                                      temp_tag->node);
                          input_filename = tags->filename;
                          line_number = tags->line_no;
                          temp_tag->flags |= NEXT_ERROR;
                        }
                    }
                }
            }
        }

      if (!tags->up && (strcasecmp (tags->node, _("Top")) != 0))
        line_error (_("Node `%s' missing Up field"), tags->node);
      else if (tags->up)
        {
          int valid_p = validate (tags->up, tags->line_no, "Up");

          /* If node X has Up: Y, then warn if Y fails to have a menu item
             or note pointing at X, if Y isn't of the form "(Y)". */
          if (valid_p && *tags->up != '(')
            {
              NODE_REF *nref, *tref, *list;
              NODE_REF *find_node_reference ();

              tref = (NODE_REF *) NULL;
              list = node_references;

              for (;;)
                {
                  if (!(nref = find_node_reference (tags->node, list)))
                    break;

                  if (strcmp (nref->containing_node, tags->up) == 0)
                    {
                      if (nref->type != menu_reference)
                        {
                          tref = nref;
                          list = nref->next;
                        }
                      else
                        break;
                    }
                  list = nref->next;
                }

              if (!nref)
                {
                  temp_tag = find_node (tags->up);
                  line_number = temp_tag->line_no;
                  input_filename = temp_tag->filename;
                  if (!tref)
                    line_error (
_("`%s' has an Up field of `%s', but `%s' has no menu item for `%s'"),
                                tags->node, tags->up, tags->up, tags->node);
                  line_number = tags->line_no;
                  input_filename = tags->filename;
                }
            }
        }
      tags = tags->next_ent;
    }

  validate_other_references (node_references);
  /* We have told the user about the references which didn't exist.
     Now tell him about the nodes which aren't referenced. */

  tags = tag_table;
  while (tags != (TAG_ENTRY *) NULL)
    {
      /* If this node is a "no warn" node, do nothing. */
      if (tags->flags & NO_WARN)
        {
          tags = tags->next_ent;
          continue;
        }

      /* Special hack.  If the node in question appears to have
         been referenced more than REFERENCE_WARNING_LIMIT times,
         give a warning. */
      if (tags->touched > reference_warning_limit)
        {
          input_filename = tags->filename;
          line_number = tags->line_no;
          warning (_("node `%s' has been referenced %d times"),
                   tags->node, tags->touched);
        }

      if (tags->touched == 0)
        {
          input_filename = tags->filename;
          line_number = tags->line_no;

          /* Notice that the node "Top" is special, and doesn't have to
             be referenced. */
          if (strcasecmp (tags->node, _("Top")) != 0)
            warning (_("unreferenced node `%s'"), tags->node);
        }
      tags = tags->next_ent;
    }
  input_filename = old_input_filename;
}

/* Return 1 if tag correctly validated, or 0 if not. */
int
validate (tag, line, label)
     char *tag;
     int line;
     char *label;
{
  TAG_ENTRY *result;

  /* If there isn't a tag to verify, or if the tag is in another file,
     then it must be okay. */
  if (!tag || !*tag || *tag == '(')
    return (1);

  /* Otherwise, the tag must exist. */
  result = find_node (tag);

  if (!result)
    {
      line_number = line;
      line_error (_("%s reference to nonexistent node `%s'"), label, tag);
      return (0);
    }
  result->touched++;
  return (1);
}

/* Split large output files into a series of smaller files.  Each file
   is pointed to in the tag table, which then gets written out as the
   original file.  The new files have the same name as the original file
   with a "-num" attached.  SIZE is the largest number of bytes to allow
   in any single split file. */
void
split_file (filename, size)
     char *filename;
     int size;
{
  char *root_filename, *root_pathname;
  char *the_file, *filename_part ();
  struct stat fileinfo;
  long file_size;
  char *the_header;
  int header_size;

  /* Can only do this to files with tag tables. */
  if (!tag_table)
    return;

  if (size == 0)
    size = DEFAULT_SPLIT_SIZE;

  if ((stat (filename, &fileinfo) != 0) ||
      (((long) fileinfo.st_size) < SPLIT_SIZE_THRESHOLD))
    return;
  file_size = (long) fileinfo.st_size;

  the_file = find_and_load (filename);
  if (!the_file)
    return;

  root_filename = filename_part (filename);
  root_pathname = pathname_part (filename);

  if (!root_pathname)
    root_pathname = xstrdup ("");

  /* Start splitting the file.  Walk along the tag table
     outputting sections of the file.  When we have written
     all of the nodes in the tag table, make the top-level
     pointer file, which contains indirect pointers and
     tags for the nodes. */
  {
    int which_file = 1;
    TAG_ENTRY *tags = tag_table;
    char *indirect_info = (char *)NULL;

    /* Remember the `header' of this file.  The first tag in the file is
       the bottom of the header; the top of the file is the start. */
    the_header = (char *)xmalloc (1 + (header_size = tags->position));
    memcpy (the_header, the_file, header_size);

    while (tags)
      {
        int file_top, file_bot, limit;

        /* Have to include the Control-_. */
        file_top = file_bot = tags->position;
        limit = file_top + size;

        /* If the rest of this file is only one node, then
           that is the entire subfile. */
        if (!tags->next_ent)
          {
            int i = tags->position + 1;
            char last_char = the_file[i];

            while (i < file_size)
              {
                if ((the_file[i] == '\037') &&
                    ((last_char == '\n') ||
                     (last_char == '\014')))
                  break;
                else
                  last_char = the_file[i];
                i++;
              }
            file_bot = i;
            tags = tags->next_ent;
            goto write_region;
          }

        /* Otherwise, find the largest number of nodes that can fit in
           this subfile. */
        for (; tags; tags = tags->next_ent)
          {
            if (!tags->next_ent)
              {
                /* This entry is the last node.  Search forward for the end
                   of this node, and that is the end of this file. */
                int i = tags->position + 1;
                char last_char = the_file[i];

                while (i < file_size)
                  {
                    if ((the_file[i] == '\037') &&
                        ((last_char == '\n') ||
                         (last_char == '\014')))
                      break;
                    else
                      last_char = the_file[i];
                    i++;
                  }
                file_bot = i;

                if (file_bot < limit)
                  {
                    tags = tags->next_ent;
                    goto write_region;
                  }
                else
                  {
                    /* Here we want to write out everything before the last
                       node, and then write the last node out in a file
                       by itself. */
                    file_bot = tags->position;
                    goto write_region;
                  }
              }

            if (tags->next_ent->position > limit)
              {
                if (tags->position == file_top)
                  tags = tags->next_ent;

                file_bot = tags->position;

              write_region:
                {
                  int fd;
                  char *split_filename;

                  split_filename = (char *) xmalloc
                    (10 + strlen (root_pathname) + strlen (root_filename));
                  sprintf
                    (split_filename,
                     "%s%s-%d", root_pathname, root_filename, which_file);

                  fd = open
                    (split_filename, O_WRONLY | O_TRUNC | O_CREAT, 0666);

                  if ((fd < 0) ||
                      (write (fd, the_header, header_size) != header_size) ||
                      (write (fd, the_file + file_top, file_bot - file_top)
                       != (file_bot - file_top)) ||
                      ((close (fd)) < 0))
                    {
                      perror (split_filename);
                      if (fd != -1)
                        close (fd);
                      exit (FATAL);
                    }

                  if (!indirect_info)
                    {
                      indirect_info = the_file + file_top;
                      sprintf (indirect_info, "\037\nIndirect:\n");
                      indirect_info += strlen (indirect_info);
                    }

                  sprintf (indirect_info, "%s-%d: %d\n",
                           root_filename, which_file, file_top);

                  free (split_filename);
                  indirect_info += strlen (indirect_info);
                  which_file++;
                  break;
                }
              }
          }
      }

    /* We have sucessfully created the subfiles.  Now write out the
       original again.  We must use `output_stream', or
       write_tag_table_indirect () won't know where to place the output. */
    output_stream = fopen (filename, "w");
    if (!output_stream)
      {
        perror (filename);
        exit (FATAL);
      }

    {
      int distance = indirect_info - the_file;
      fwrite (the_file, 1, distance, output_stream);

      /* Inhibit newlines. */
      paragraph_is_open = 0;

      write_tag_table_indirect ();
      fclose (output_stream);
      free (the_header);
      free (the_file);
      return;
    }
  }
}

/* The strings here are followed in the message by `reference to...' in
   the `validate' routine.  */
char *
reftype_type_string (type)
     enum reftype type;
{
  switch (type)
    {
    case menu_reference:
      return ("Menu");
    case followed_reference:
      return ("Cross");
    default:
      return ("Internal-bad-reference-type");
    }
}

/* Remember this node name for later validation use.  This is used to
   remember menu references while reading the input file.  After the
   output file has been written, if validation is on, then we use the
   contents of `node_references' as a list of nodes to validate.  */
void
remember_node_reference (node, line, type)
     char *node;
     int line;
     enum reftype type;
{
  NODE_REF *temp = (NODE_REF *) xmalloc (sizeof (NODE_REF));

  temp->next = node_references;
  temp->node = xstrdup (node);
  temp->line_no = line;
  temp->section = current_section;
  temp->type = type;
  temp->containing_node = xstrdup (current_node ? current_node : "");
  temp->filename = node_filename;

  node_references = temp;
}

void
validate_other_references (ref_list)
     register NODE_REF *ref_list;
{
  char *old_input_filename = input_filename;

  while (ref_list != (NODE_REF *) NULL)
    {
      input_filename = ref_list->filename;
      validate (ref_list->node, ref_list->line_no,
                reftype_type_string (ref_list->type));
      ref_list = ref_list->next;
    }
  input_filename = old_input_filename;
}

/* Find NODE in REF_LIST. */
NODE_REF *
find_node_reference (node, ref_list)
     char *node;
     register NODE_REF *ref_list;
{
  while (ref_list)
    {
      if (strcmp (node, ref_list->node) == 0)
        break;
      ref_list = ref_list->next;
    }
  return (ref_list);
}

void
free_node_references ()
{
  register NODE_REF *list, *temp;

  list = node_references;

  while (list)
    {
      temp = list;
      free (list->node);
      free (list->containing_node);
      list = list->next;
      free (temp);
    }
  node_references = (NODE_REF *) NULL;
}

  /* This function gets called at the start of every line while inside of
     a menu.  It checks to see if the line starts with "* ", and if so,
     remembers the node reference that this menu refers to.
     input_text_offset is at the \n just before the line start. */
#define menu_starter "* "
char *
glean_node_from_menu (remember_reference)
     int remember_reference;
{
  int i, orig_offset = input_text_offset;
  char *nodename;

  if (strncmp (&input_text[input_text_offset + 1],
               menu_starter,
               strlen (menu_starter)) != 0)
    return ((char *)NULL);
  else
    input_text_offset += strlen (menu_starter) + 1;

  get_until_in_line (0, ":", &nodename);
  if (curchar () == ':')
    input_text_offset++;
  canon_white (nodename);

  if (curchar () == ':')
    goto save_node;

  free (nodename);
  get_rest_of_line (&nodename);

  /* Special hack: If the nodename follows the menu item name,
     then we have to read the rest of the line in order to find
     out what the nodename is.  But we still have to read the
     line later, in order to process any formatting commands that
     might be present.  So un-count the carriage return that has just
     been counted. */
  line_number--;

  isolate_nodename (nodename);

save_node:
  input_text_offset = orig_offset;
  normalize_node_name (nodename);
  i = strlen (nodename);
  if (i && nodename[i - 1] == ':')
    nodename[i - 1] = 0;

  if (remember_reference)
    {
      remember_node_reference (nodename, line_number, menu_reference);
      free (nodename);
      return ((char *)NULL);
    }
  else
    return (nodename);
}

static void
isolate_nodename (nodename)
     char *nodename;
{
  register int i, c;
  int paren_seen, paren;

  if (!nodename)
    return;

  canon_white (nodename);
  paren_seen = paren = i = 0;

  if (*nodename == '.' || !*nodename)
    {
      *nodename = 0;
      return;
    }

  if (*nodename == '(')
    {
      paren++;
      paren_seen++;
      i++;
    }

  for (; (c = nodename[i]); i++)
    {
      if (paren)
        {
          if (c == '(')
            paren++;
          else if (c == ')')
            paren--;

          continue;
        }

      /* If the character following the close paren is a space, then this
         node has no more characters associated with it. */
      if (c == '\t' ||
          c == '\n' ||
          c == ','  ||
          ((paren_seen && nodename[i - 1] == ')') &&
           (c == ' ' || c == '.')) ||
          (c == '.' &&
           ((!nodename[i + 1] ||
             (cr_or_whitespace (nodename[i + 1])) ||
             (nodename[i + 1] == ')')))))
        break;
    }
  nodename[i] = 0;
}

void
cm_menu ()
{
  if (current_node == (char *)NULL)
    {
      warning (_("%cmenu seen before first node"), COMMAND_PREFIX);
      warning (_("creating `Top' node"));
      execute_string ("@node Top");
    }
  begin_insertion (menu);
}

void
cm_detailmenu ()
{
  if (current_node == (char *)NULL)
    {
      warning (_("%cmenu seen before first node"), COMMAND_PREFIX);
      warning (_("creating `Top' node"));
      execute_string ("@node Top");
    }
  begin_insertion (detailmenu);
}

/* **************************************************************** */
/*                                                                  */
/*                      Cross Reference Hacking                     */
/*                                                                  */
/* **************************************************************** */

/* Return next comma-delimited argument, but do not cross a close-brace
   boundary.  Clean up whitespace, too.  */
char *
get_xref_token ()
{
  char *string;

  get_until_in_braces (",", &string);
  if (curchar () == ',')
    input_text_offset++;
  fix_whitespace (string);
  return (string);
}

int px_ref_flag = 0;            /* Controls initial output string. */

/* Make a cross reference. */
void
cm_xref (arg)
{
  if (arg == START)
    {
      char *arg1, *arg2, *arg3, *arg4, *arg5;

      arg1 = get_xref_token ();
      arg2 = get_xref_token ();
      arg3 = get_xref_token ();
      arg4 = get_xref_token ();
      arg5 = get_xref_token ();

      add_word_args ("%s", px_ref_flag ? "*note " : "*Note ");

      if (*arg5 || *arg4)
        {
          char *node_name;

          if (!*arg2)
            {
              if (*arg3)
                node_name = arg3;
              else
                node_name = arg1;
            }
          else
            node_name = arg2;

          execute_string ("%s: (%s)%s", node_name, arg4, arg1);
          /* Free all of the arguments found. */
          if (arg1) free (arg1);
          if (arg2) free (arg2);
          if (arg3) free (arg3);
          if (arg4) free (arg4);
          if (arg5) free (arg5);
          return;
        }
      else
        remember_node_reference (arg1, line_number, followed_reference);

      if (*arg3)
        {
          if (!*arg2)
            execute_string ("%s: %s", arg3, arg1);
          else
            execute_string ("%s: %s", arg2, arg1);
        }
      else
        {
          if (*arg2)
            execute_string ("%s: %s", arg2, arg1);
          else
            execute_string ("%s::", arg1);
        }

      /* Free all of the arguments found. */
      if (arg1) free (arg1);
      if (arg2) free (arg2);
      if (arg3) free (arg3);
      if (arg4) free (arg4);
      if (arg5) free (arg5);
    }
  else
    {
      /* Check to make sure that the next non-whitespace character is either
         a period or a comma. input_text_offset is pointing at the "}" which
         ended the xref or pxref command. */
      int temp = input_text_offset + 1;

      if (output_paragraph[output_paragraph_offset - 2] == ':' &&
          output_paragraph[output_paragraph_offset - 1] == ':')
        return;
      while (temp < size_of_input_text)
        {
          if (cr_or_whitespace (input_text[temp]))
            temp++;
          else
            {
              if (input_text[temp] != '.'
                  && input_text[temp] != ','
                  && input_text[temp] != '\t')
               {
                 line_error (
                          _("`.' or `,' must follow cross reference, not %c"), 
                             input_text[temp]);
                }
              break;
            }
        }
    }
}

void
cm_pxref (arg)
     int arg;
{
  if (arg == START)
    {
      px_ref_flag++;
      cm_xref (arg);
      px_ref_flag--;
    }
  else
    add_char ('.');
}

void
cm_inforef (arg)
     int arg;
{
  if (arg == START)
    {
      char *node = get_xref_token ();
      char *pname = get_xref_token ();
      char *file = get_xref_token ();

      if (*pname)
        execute_string ("*note %s: (%s)%s", pname, file, node);
      else
        execute_string ("*note (%s)%s::", file, node);

      free (node);
      free (pname);
      free (file);
    }
}

/* A URL reference.  */
void
cm_uref (arg, start_pos, end_pos)
     int arg, start_pos, end_pos;
{
  if (arg == END)
    {
      char *comma;
      char *arg = (char *) &output_paragraph[start_pos];

      output_paragraph[end_pos] = 0;
      output_column -= end_pos - start_pos;
      output_paragraph_offset = start_pos;

      arg = xstrdup (arg);
      comma = strchr (arg, ','); /* let's hope for no commas in the url  */
      if (comma)
        {
          *comma = 0;
          /* Ignore spaces at beginning of second arg.  */
          for (comma++; isspace (*comma); comma++)
            ;
          add_word (comma);
          add_char (' ');
          add_char ('(');
          add_word (arg);
          add_char (')');
        }
      else
        {
          extern int printing_index;
          
          if (!printing_index)
            add_char ('`');
            
          add_word (arg);
          
          if (!printing_index)
            add_char ('\'');
        }
      free (arg);
   }
}

/* An email reference.  */
void
cm_email (arg, start_pos, end_pos)
     int arg, start_pos, end_pos;
{
  if (arg == END)
    {
      char *comma;
      char *arg = (char *) &output_paragraph[start_pos];

      output_paragraph[end_pos] = 0;
      output_column -= end_pos - start_pos;
      output_paragraph_offset = start_pos;

      arg = xstrdup (arg);
      comma = strchr (arg, ',');
      if (comma)
        {
          *comma = 0;
          for (comma++; isspace (*comma); comma++)
            ;
          add_word (comma);
          add_char (' ');
        }
      add_char ('<');
      add_word (arg);
      add_char ('>');
      free (arg);
    }
}

/* An external image is a reference, kind of.  The parsing is (not
   coincidentally) similar, anyway.  */
void
cm_image (arg)
     int arg;
{
  if (arg == START)
    {
      char *name_arg = get_xref_token ();
      /* We don't yet care about any other args, but read them so they
         don't end up in the text.  */
      char *arg = get_xref_token ();
      if (arg) free (arg);
      arg = get_xref_token ();
      if (arg) free (arg);
      
      if (*name_arg)
        {
          /* Try to open foo.txt.  */
          FILE *image_file;
          char *name = xmalloc (strlen (name_arg) + 4);
          strcpy (name, name_arg);
          strcat (name, ".txt");
          image_file = fopen (name, "r");
          if (image_file)
            {
              int ch;
              int save_inhibit_indentation = inhibit_paragraph_indentation;
              int save_filling_enabled = filling_enabled;
              
              inhibit_paragraph_indentation = 1;
              filling_enabled = 0;
              last_char_was_newline = 0;
              
              /* Maybe we need to remove the final newline if the image
                 file is only one line to allow in-line images.  On the
                 other hand, they could just make the file without a
                 final newline.  */
              while ((ch = getc (image_file)) != EOF)
                add_char (ch);
              
              inhibit_paragraph_indentation = save_inhibit_indentation;
              filling_enabled = save_filling_enabled;
              
              if (fclose (image_file) != 0) {
                perror (name);
              }
            }
          else
            warning (_("@image file `%s' unreadable: %s"), name,
                     strerror (errno));
        }
      else
        line_error (_("@image missing filename argument"));

      if (name_arg) free (name_arg);
    }
}

/* **************************************************************** */
/*                                                                  */
/*                      Insertion Command Stubs                     */
/*                                                                  */
/* **************************************************************** */

void
cm_quotation ()
{
  begin_insertion (quotation);
}

void
cm_example ()
{
  begin_insertion (example);
}

void
cm_smallexample ()
{
  begin_insertion (smallexample);
}

void
cm_lisp ()
{
  begin_insertion (lisp);
}

void
cm_smalllisp ()
{
  begin_insertion (smalllisp);
}

/* @cartouche/@end cartouche draws box with rounded corners in
   TeX output.  Right now, just a no-op insertion. */
void
cm_cartouche ()
{
  begin_insertion (cartouche);
}

void
cm_format ()
{
  begin_insertion (format);
}

void
cm_display ()
{
  begin_insertion (display);
}

void
cm_direntry ()
{
  if (no_headers)
    command_name_condition ();
  else
    begin_insertion (direntry);
}

void
cm_itemize ()
{
  begin_insertion (itemize);
}

void
cm_enumerate ()
{
  do_enumeration (enumerate, "1");
}

/* Start an enumeration insertion of type TYPE.  If the user supplied
   no argument on the line, then use DEFAULT_STRING as the initial string. */
void
do_enumeration (type, default_string)
     int type;
     char *default_string;
{
  get_until_in_line (0, ".", &enumeration_arg);
  canon_white (enumeration_arg);

  if (!*enumeration_arg)
    {
      free (enumeration_arg);
      enumeration_arg = xstrdup (default_string);
    }

  if (!isdigit (*enumeration_arg) && !isletter (*enumeration_arg))
    {
      warning (_("%s requires letter or digit"), insertion_type_pname (type));

      switch (type)
        {
        case enumerate:
          default_string = "1";
          break;
        }
      enumeration_arg = xstrdup (default_string);
    }
  begin_insertion (type);
}

void
cm_table ()
{
  begin_insertion (table);
}

void
cm_multitable ()
{
  begin_insertion (multitable); /* @@ */
}

void
cm_ftable ()
{
  begin_insertion (ftable);
}

void
cm_vtable ()
{
  begin_insertion (vtable);
}

void
cm_group ()
{
  begin_insertion (group);
}

void
cm_ifinfo ()
{
  begin_insertion (ifinfo);
}

void
cm_ifnothtml ()
{
  begin_insertion (ifnothtml);
}

void
cm_ifnottex ()
{
  begin_insertion (ifnottex);
}

/* Begin an insertion where the lines are not filled or indented. */
void
cm_flushleft ()
{
  begin_insertion (flushleft);
}

/* Begin an insertion where the lines are not filled, and each line is
   forced to the right-hand side of the page. */
void
cm_flushright ()
{
  begin_insertion (flushright);
}

/* End existing insertion block. */
void
cm_end ()
{
  char *temp;
  enum insertion_type type;

  if (!insertion_level)
    {
      line_error (_("Unmatched `%c%s'"), COMMAND_PREFIX, command);
      return;
    }

  get_rest_of_line (&temp);

  if (temp[0] == 0)
    line_error (_("`%c%s' needs something after it"), COMMAND_PREFIX, command);

  type = find_type_from_name (temp);

  if (type == bad_type)
    {
      line_error (_("Bad argument to `%s', `%s', using `%s'"),
           command, temp, insertion_type_pname (current_insertion_type ()));
    }
  end_insertion (type);
  free (temp);
}

/* **************************************************************** */
/*                                                                  */
/*                        Conditional Handling                      */
/*                                                                  */
/* **************************************************************** */

/* A structure which contains `defined' variables. */
typedef struct defines {
  struct defines *next;
  char *name;
  char *value;
} DEFINE;

/* The linked list of `set' defines. */
DEFINE *defines = (DEFINE *)NULL;

/* Add NAME to the list of `set' defines. */
void
set (name, value)
     char *name;
     char *value;
{
  DEFINE *temp;

  for (temp = defines; temp; temp = temp->next)
    if (strcmp (name, temp->name) == 0)
      {
        free (temp->value);
        temp->value = xstrdup (value);
        return;
      }

  temp = (DEFINE *)xmalloc (sizeof (DEFINE));
  temp->next = defines;
  temp->name = xstrdup (name);
  temp->value = xstrdup (value);
  defines = temp;
}

/* Remove NAME from the list of `set' defines. */
void
clear (name)
     char *name;
{
  register DEFINE *temp, *last;

  last = (DEFINE *)NULL;
  temp = defines;

  while (temp)
    {
      if (strcmp (temp->name, name) == 0)
        {
          if (last)
            last->next = temp->next;
          else
            defines = temp->next;

          free (temp->name);
          free (temp->value);
          free (temp);
          break;
        }
      last = temp;
      temp = temp->next;
    }
}

/* Return the value of NAME.  The return value is NULL if NAME is unset. */
char *
set_p (name)
     char *name;
{
  register DEFINE *temp;

  for (temp = defines; temp; temp = temp->next)
    if (strcmp (temp->name, name) == 0)
      return (temp->value);

  return ((char *)NULL);
}

/* Conditionally parse based on the current command name. */
void
command_name_condition ()
{
  char *discarder;

  discarder = (char *)xmalloc (8 + strlen (command));

  sprintf (discarder, "\n%cend %s", COMMAND_PREFIX, command);
  discard_until (discarder);
  discard_until ("\n");

  free (discarder);
}

/* Create a variable whose name appears as the first word on this line. */
void
cm_set ()
{
  handle_variable (SET);
}

/* Remove a variable whose name appears as the first word on this line. */
void
cm_clear ()
{
  handle_variable (CLEAR);
}

void
cm_ifset ()
{
  handle_variable (IFSET);
}

void
cm_ifclear ()
{
  handle_variable (IFCLEAR);
}

/* This command takes braces, but we parse the contents specially, so we
   don't use the standard brace popping code.

   The syntax @ifeq{arg1, arg2, texinfo-commands} performs texinfo-commands
   if ARG1 and ARG2 caselessly string compare to the same string, otherwise,
   it produces no output. */
void
cm_ifeq ()
{
  char **arglist;

  arglist = get_brace_args (0);

  if (arglist)
    {
      if (array_len (arglist) > 1)
        {
          if ((strcasecmp (arglist[0], arglist[1]) == 0) &&
              (arglist[2] != (char *)NULL))
            execute_string ("%s\n", arglist[2]);
        }

      free_array (arglist);
    }
}

void
cm_value (arg, start_pos, end_pos)
     int arg, start_pos, end_pos;
{
  if (arg == END)
    {
      char *name = (char *) &output_paragraph[start_pos];
      char *value;
      output_paragraph[end_pos] = 0;
      name = xstrdup (name);
      value = set_p (name);
      output_column -= end_pos - start_pos;
      output_paragraph_offset = start_pos;

      if (value)
        execute_string ("%s", value);
      else
        add_word_args (_("{No Value For \"%s\"}"), name);

      free (name);
    }
}

/* Set, clear, or conditionalize based on ACTION. */
void
handle_variable (action)
     int action;
{
  char *name;

  get_rest_of_line (&name);
  backup_input_pointer ();
  handle_variable_internal (action, name);
  free (name);
}

void
handle_variable_internal (action, name)
     int action;
     char *name;
{
  char *temp;
  int delimiter, additional_text_present = 0;

  /* Only the first word of NAME is a valid tag. */
  temp = name;
  delimiter = 0;
  while (*temp && (delimiter || !whitespace (*temp)))
    {
/* #if defined (SET_WITH_EQUAL) */
      if (*temp == '"' || *temp == '\'')
        {
          if (*temp == delimiter)
            delimiter = 0;
          else
            delimiter = *temp;
        }
/* #endif SET_WITH_EQUAL */
      temp++;
    }

  if (*temp)
    additional_text_present++;

  *temp = 0;

  if (!*name)
    line_error (_("%c%s requires a name"), COMMAND_PREFIX, command);
  else
    {
      switch (action)
        {
        case SET:
          {
            char *value;

#if defined (SET_WITH_EQUAL)
            /* Allow a value to be saved along with a variable.  The value is
               the text following an `=' sign in NAME, if any is present. */

            for (value = name; *value && *value != '='; value++);

            if (*value)
              *value++ = 0;

            if (*value == '"' || *value == '\'')
              {
                value++;
                value[strlen (value) - 1] = 0;
              }

#else /* !SET_WITH_EQUAL */
            /* The VALUE of NAME is the remainder of the line sans
               whitespace. */
            if (additional_text_present)
              {
                value = temp + 1;
                canon_white (value);
              }
            else
              value = "";
#endif /* !SET_WITH_VALUE */

            set (name, value);
          }
          break;

        case CLEAR:
          clear (name);
          break;

        case IFSET:
        case IFCLEAR:
          /* If IFSET and NAME is not set, or if IFCLEAR and NAME is set,
             read lines from the file until we reach a matching
             "@end CONDITION".  This means that we only take note of
             "@ifset/clear" and "@end" commands. */
          {
            char condition[8];
            int condition_len;
            int orig_line_number = line_number;

            if (action == IFSET)
              strcpy (condition, "ifset");
            else
              strcpy (condition, "ifclear");

            condition_len = strlen (condition);

          if ((action == IFSET && !set_p (name))
              || (action == IFCLEAR && set_p (name)))
            {
              int level = 0, done = 0;

              while (!done && input_text_offset < size_of_input_text)
                {
                  char *freeable_line, *line;

                  get_rest_of_line (&freeable_line);

                  for (line = freeable_line; whitespace (*line); line++);

                  if (*line == COMMAND_PREFIX &&
                      (strncmp (line + 1, condition, condition_len) == 0))
                    level++;
                  else if (strncmp (line, "@end", 4) == 0)
                    {
                      char *cname = line + 4;
                      char *temp;

                      while (*cname && whitespace (*cname))
                        cname++;
                      temp = cname;

                      while (*temp && !whitespace (*temp))
                        temp++;
                      *temp = 0;

                      if (strcmp (cname, condition) == 0)
                        {
                          if (!level)
                            {
                              done = 1;
                            }
                          else
                            level--;
                        }
                    }
                  free (freeable_line);
                }
              
              if (!done)
                {
                  int save = line_number;
                  line_number = orig_line_number;
                  line_error (_("Reached eof before matching @end %s"),
                              condition);
                  line_number = save;
                }
                
              /* We found the end of a false @ifset/ifclear.  If we are
                 in a menu, back up over the newline that ends the ifset,
                 since that newline may also begin the next menu entry. */
              break;
            }
          else
            {
              if (action == IFSET)
                begin_insertion (ifset);
              else
                begin_insertion (ifclear);
            }
          }
          break;
        }
    }
}

/* Execution of random text not in file. */

typedef struct {
  char *string;                 /* The string buffer. */
  int size;                     /* The size of the buffer. */
  int in_use;                   /* Nonzero means string currently in use. */
} EXECUTION_STRING;

static EXECUTION_STRING **execution_strings = (EXECUTION_STRING **)NULL;
static int execution_strings_index = 0;
static int execution_strings_slots = 0;

EXECUTION_STRING *
get_execution_string (initial_size)
     int initial_size;
{
  register int i = 0;
  EXECUTION_STRING *es = (EXECUTION_STRING *)NULL;

  if (execution_strings)
    {
      for (i = 0; i < execution_strings_index; i++)
        if (execution_strings[i] && (execution_strings[i]->in_use == 0))
          {
            es = execution_strings[i];
            break;
          }
    }

  if (!es)
    {
      if (execution_strings_index + 1 >= execution_strings_slots)
        {
          execution_strings = (EXECUTION_STRING **)xrealloc
            (execution_strings,
             (execution_strings_slots += 3) * sizeof (EXECUTION_STRING *));
          for (; i < execution_strings_slots; i++)
            execution_strings[i] = (EXECUTION_STRING *)NULL;
        }

      execution_strings[execution_strings_index] =
        (EXECUTION_STRING *)xmalloc (sizeof (EXECUTION_STRING));
      es = execution_strings[execution_strings_index];
      execution_strings_index++;

      es->size = 0;
      es->string = (char *)NULL;
      es->in_use = 0;
    }

  if (initial_size > es->size)
    {
      es->string = (char *) xrealloc (es->string, initial_size);
      es->size = initial_size;
    }
  return (es);
}

/* Execute the string produced by formatting the ARGs with FORMAT.  This
   is like submitting a new file with @include. */
void
#if defined (VA_FPRINTF) && __STDC__
execute_string (char *format, ...)
#else
execute_string (format, va_alist)
    char *format;
    va_dcl
#endif
{
  EXECUTION_STRING *es;
  char *temp_string;
#ifdef VA_FPRINTF
  va_list ap;
#endif

  es = get_execution_string (4000);
  temp_string = es->string;
  es->in_use = 1;

  VA_START (ap, format);
#ifdef VA_SPRINTF
  VA_SPRINTF (temp_string, format, ap);
#else
  sprintf (temp_string, format, a1, a2, a3, a4, a5, a6, a7, a8);
#endif /* not VA_SPRINTF */
  va_end (ap);

  pushfile ();
  input_text_offset = 0;
  input_text = temp_string;
  input_filename = xstrdup (input_filename);
  size_of_input_text = strlen (temp_string);

  executing_string++;
  reader_loop ();
  free (input_filename);

  popfile ();
  executing_string--;
  es->in_use = 0;
}


/* Return what would be output for STR, i.e., expand Texinfo commands.
   If IMPLICIT_CODE is set, expand @code{STR}.  */

char *
expansion (str, implicit_code)
    char *str;
    int implicit_code;
{
  int length;
  char *result;

  /* Inhibit any real output.  */
  int start = output_paragraph_offset;
  int saved_paragraph_is_open = paragraph_is_open;

  inhibit_output_flushing ();
  paragraph_is_open = 1;
  execute_string (implicit_code ? "@code{%s}" : "%s", str);
  uninhibit_output_flushing ();

  /* Copy the expansion from the buffer.  */
  length = output_paragraph_offset - start;
  result = xmalloc (1 + length);
  memcpy (result, (char *) (output_paragraph + start), length);
  result[length] = 0;
  
  /* Pretend it never happened.  */
  output_paragraph_offset = start;
  paragraph_is_open = saved_paragraph_is_open;

  return result;
}

/* @itemx, @item. */

static int itemx_flag = 0;

void
cm_itemx ()
{
  itemx_flag++;
  cm_item ();
  itemx_flag--;
}

void
cm_item ()
{
  char *rest_of_line, *item_func;

  /* Can only hack "@item" while inside of an insertion. */
  if (insertion_level)
    {
      INSERTION_ELT *stack = insertion_stack;
      int original_input_text_offset;

      skip_whitespace ();
      original_input_text_offset = input_text_offset;

      get_rest_of_line (&rest_of_line);
      item_func = current_item_function ();

      /* Okay, do the right thing depending on which insertion function
         is active. */

    switch_top:
      switch (stack->insertion)
        {
        case multitable:
          multitable_item ();
          /* Ultra special hack.  It appears that some people incorrectly
             place text directly after the @item, instead of on a new line
             by itself.  This happens to work in TeX, so I make it work
             here. */
          if (*rest_of_line)
            {
              line_number--;
              input_text_offset = original_input_text_offset;
            }
          break;

        case ifinfo:
        case ifset:
        case ifclear:
        case cartouche:
          stack = stack->next;
          if (!stack)
            goto no_insertion;
          else
            goto switch_top;
          break;

        case menu:
        case quotation:
        case example:
        case smallexample:
        case lisp:
        case format:
        case display:
        case group:
          line_error (_("The `%c%s' command is meaningless within a `@%s' block"),
                      COMMAND_PREFIX, command,
                      insertion_type_pname (current_insertion_type ()));
          break;

        case itemize:
        case enumerate:
          if (itemx_flag)
            {
              line_error (_("%citemx is not meaningful inside of a `%s' block"),
                          COMMAND_PREFIX,
                          insertion_type_pname (current_insertion_type ()));
            }
          else
            {
              start_paragraph ();
              kill_self_indent (-1);
              filling_enabled = indented_fill = 1;

              if (current_insertion_type () == itemize)
                {
                  indent (output_column = current_indent - 2);

                  /* I need some way to determine whether this command
                     takes braces or not.  I believe the user can type
                     either "@bullet" or "@bullet{}".  Of course, they
                     can also type "o" or "#" or whatever else they want. */
                  if (item_func && *item_func)
                    {
                      if (*item_func == COMMAND_PREFIX)
                        if (item_func[strlen (item_func) - 1] != '}')
                          execute_string ("%s{}", item_func);
                        else
                          execute_string ("%s", item_func);
                      else
                        execute_string ("%s", item_func);
                    }
                  insert (' ');
                  output_column++;
                }
              else
                enumerate_item ();

              /* Special hack.  This makes `close_paragraph' a no-op until
                 `start_paragraph' has been called. */
              must_start_paragraph = 1;

              /* Handle text directly after the @item.  */
              if (*rest_of_line)
                {
                  line_number--;
                  input_text_offset = original_input_text_offset;
                }
            }
          break;

        case table:
        case ftable:
        case vtable:
          {
            /* We need this to determine if we have two @item's in a row
               (see test just below).  */
            static int last_item_output_position = 0;
            
            /* Get rid of extra characters. */
            kill_self_indent (-1);

            /* If we have one @item followed directly by another @item,
               we need to insert a blank line.  This is not true for
               @itemx, though.  */
            if (!itemx_flag && last_item_output_position == output_position)
              insert ('\n');
              
            /* `close_paragraph' almost does what we want.  The problem
               is when paragraph_is_open, and last_char_was_newline, and
               the last newline has been turned into a space, because
               filling_enabled. I handle it here. */
            if (last_char_was_newline && filling_enabled && paragraph_is_open)
              insert ('\n');
            close_paragraph ();

#if defined (INDENT_PARAGRAPHS_IN_TABLE)
            /* Indent on a new line, but back up one indentation level. */
            {
              int save = inhibit_paragraph_indentation;
              inhibit_paragraph_indentation = 1;
              /* At this point, inserting any non-whitespace character will
                 force the existing indentation to be output. */
              add_char ('i');
              inhibit_paragraph_indentation = save;
            }
#else /* !INDENT_PARAGRAPHS_IN_TABLE */
            add_char ('i');
#endif /* !INDENT_PARAGRAPHS_IN_TABLE */

            output_paragraph_offset--;
            kill_self_indent (default_indentation_increment + 1);

            /* Add item's argument to the line. */
            filling_enabled = 0;
            if (item_func && *item_func)
              execute_string ("%s{%s}", item_func, rest_of_line);
            else
              execute_string ("%s", rest_of_line);

            if (current_insertion_type () == ftable)
              execute_string ("%cfindex %s\n", COMMAND_PREFIX, rest_of_line);
            else if (current_insertion_type () == vtable)
              execute_string ("%cvindex %s\n", COMMAND_PREFIX, rest_of_line);

            /* Start a new line, and let start_paragraph ()
               do the indenting of it for you. */
            close_single_paragraph ();
            indented_fill = filling_enabled = 1;
            last_item_output_position = output_position;
          }
        }
      free (rest_of_line);
    }
  else
    {
    no_insertion:
      line_error (_("%c%s found outside of an insertion block"),
                  COMMAND_PREFIX, command);
    }
}

/* **************************************************************** */
/*                                                                  */
/*                      Defun and Friends                           */
/*                                                                  */
/* **************************************************************** */

#define DEFUN_SELF_DELIMITING(c)                                        \
  (((c) == '(')                                                         \
   || ((c) == ')')                                                      \
   || ((c) == '[')                                                      \
   || ((c) == ']'))

struct token_accumulator
{
  unsigned int length;
  unsigned int index;
  char **tokens;
};

void
initialize_token_accumulator (accumulator)
     struct token_accumulator *accumulator;
{
  (accumulator->length) = 0;
  (accumulator->index) = 0;
  (accumulator->tokens) = NULL;
}

void
accumulate_token (accumulator, token)
     struct token_accumulator *accumulator;
     char *token;
{
  if ((accumulator->index) >= (accumulator->length))
    {
      (accumulator->length) += 10;
      (accumulator->tokens) = (char **) xrealloc
        (accumulator->tokens, (accumulator->length * sizeof (char *)));
    }
  accumulator->tokens[accumulator->index] = token;
  accumulator->index += 1;
}

char *
copy_substring (start, end)
     char *start;
     char *end;
{
  char *result, *scan, *scan_result;

  result = (char *) xmalloc ((end - start) + 1);
  scan_result = result;
  scan = start;

  while (scan < end)
    *scan_result++ = *scan++;

  *scan_result = 0;
  return (result);
}

/* Given `string' pointing at an open brace, skip forward and return a
   pointer to just past the matching close brace. */
int
scan_group_in_string (string_pointer)
     char **string_pointer;
{
  register int c;
  register char *scan_string;
  register unsigned int level = 1;

  scan_string = (*string_pointer) + 1;

  while (1)
    {
      if (level == 0)
        {
          (*string_pointer) = scan_string;
          return (1);
        }
      c = (*scan_string++);
      if (c == 0)
        {
          /* Tweak line_number to compensate for fact that
             we gobbled the whole line before coming here. */
          line_number -= 1;
          line_error (_("Missing `}' in %cdef arg"), COMMAND_PREFIX);
          line_number += 1;
          (*string_pointer) = (scan_string - 1);
          return (0);
        }
      if (c == '{')
        level += 1;
      if (c == '}')
        level -= 1;
    }
}

/* Return a list of tokens from the contents of `string'.
   Commands and brace-delimited groups count as single tokens.
   Contiguous whitespace characters are converted to a token
   consisting of a single space. */
char **
args_from_string (string)
     char *string;
{
  struct token_accumulator accumulator;
  register char *scan_string = string;
  char *token_start, *token_end;

  initialize_token_accumulator (&accumulator);

  while ((*scan_string) != 0)
    {
      /* Replace arbitrary whitespace by a single space. */
      if (whitespace (*scan_string))
        {
          scan_string += 1;
          while (whitespace (*scan_string))
            scan_string += 1;
          accumulate_token ((&accumulator), (xstrdup (" ")));
          continue;
        }

      /* Commands count as single tokens. */
      if ((*scan_string) == COMMAND_PREFIX)
        {
          token_start = scan_string;
          scan_string += 1;
          if (self_delimiting (*scan_string))
            scan_string += 1;
          else
            {
              register int c;
              while (1)
                {
                  c = *scan_string++;

                  if ((c == 0) || (c == '{') || (whitespace (c)))
                    {
                      scan_string -= 1;
                      break;
                    }
                }

              if (*scan_string == '{')
                {
                  char *s = scan_string;
                  (void) scan_group_in_string (&s);
                  scan_string = s;
                }
            }
          token_end = scan_string;
        }

      /* Parentheses and brackets are self-delimiting. */
      else if (DEFUN_SELF_DELIMITING (*scan_string))
        {
          token_start = scan_string;
          scan_string += 1;
          token_end = scan_string;
        }

      /* Open brace introduces a group that is a single token. */
      else if (*scan_string == '{')
        {
          char *s = scan_string;
          int balanced = scan_group_in_string (&s);

          token_start = scan_string + 1;
          scan_string = s;
          token_end = balanced ? (scan_string - 1) : scan_string;
        }

      /* Otherwise a token is delimited by whitespace, parentheses,
         brackets, or braces.  A token is also ended by a command. */
      else
        {
          token_start = scan_string;

          while (1)
            {
              register int c;

              c = *scan_string++;

              /* Do not back up if we're looking at a }; since the only
                 valid }'s are those matched with {'s, we want to give
                 an error.  If we back up, we go into an infinite loop.  */
              if (!c || whitespace (c) || DEFUN_SELF_DELIMITING (c)
                  || c == '{')
                {
                  scan_string--;
                  break;
                }

              /* If we encounter a command embedded within a token,
                 then end the token. */
              if (c == COMMAND_PREFIX)
                {
                  scan_string--;
                  break;
                }
            }
          token_end = scan_string;
        }

      accumulate_token
        (&accumulator, copy_substring (token_start, token_end));
    }
  accumulate_token (&accumulator, NULL);
  return (accumulator.tokens);
}

void
process_defun_args (defun_args, auto_var_p)
     char **defun_args;
     int auto_var_p;
{
  int pending_space = 0;

  while (1)
    {
      char *defun_arg = *defun_args++;

      if (defun_arg == NULL)
        break;

      if (defun_arg[0] == ' ')
        {
          pending_space = 1;
          continue;
        }

      if (pending_space)
        {
          add_char (' ');
          pending_space = 0;
        }

      if (DEFUN_SELF_DELIMITING (defun_arg[0]))
        add_char (defun_arg[0]);
      else if (defun_arg[0] == '&')
        add_word (defun_arg);
      else if (defun_arg[0] == COMMAND_PREFIX)
        execute_string ("%s", defun_arg);
      else if (auto_var_p)
        execute_string ("%cvar{%s}", COMMAND_PREFIX, defun_arg);
      else
        add_word (defun_arg);
    }
}

char *
next_nonwhite_defun_arg (arg_pointer)
     char ***arg_pointer;
{
  char **scan = (*arg_pointer);
  char *arg = (*scan++);

  if ((arg != 0) && (*arg == ' '))
    arg = *scan++;

  if (arg == 0)
    scan -= 1;

  *arg_pointer = scan;

  return ((arg == 0) ? "" : arg);
}

/* Make the defun type insertion.
   TYPE says which insertion this is.
   X_P, if nonzero, says not to start a new insertion. */
void
defun_internal (type, x_p)
     enum insertion_type type;
     int x_p;
{
  enum insertion_type base_type;
  char **defun_args, **scan_args;
  char *category, *defined_name, *type_name, *type_name2;

  {
    char *line;
    get_rest_of_line (&line);
    defun_args = (args_from_string (line));
    free (line);
  }

  scan_args = defun_args;

  switch (type)
    {
    case defun:
      category = _("Function");
      base_type = deffn;
      break;
    case defmac:
      category = _("Macro");
      base_type = deffn;
      break;
    case defspec:
      category = _("Special Form");
      base_type = deffn;
      break;
    case defvar:
      category = _("Variable");
      base_type = defvr;
      break;
    case defopt:
      category = _("User Option");
      base_type = defvr;
      break;
    case deftypefun:
      category = _("Function");
      base_type = deftypefn;
      break;
    case deftypevar:
      category = _("Variable");
      base_type = deftypevr;
      break;
    case defivar:
      category = _("Instance Variable");
      base_type = defcv;
      break;
    case defmethod:
      category = _("Method");
      base_type = defop;
      break;
    case deftypemethod:
      category = _("Method");
      base_type = deftypemethod;
      break;
    default:
      category = next_nonwhite_defun_arg (&scan_args);
      base_type = type;
      break;
    }

  if ((base_type == deftypefn)
      || (base_type == deftypevr)
      || (base_type == defcv)
      || (base_type == defop)
      || (base_type == deftypemethod))
    type_name = next_nonwhite_defun_arg (&scan_args);

  if (base_type == deftypemethod)
    type_name2 = next_nonwhite_defun_arg (&scan_args);

  defined_name = next_nonwhite_defun_arg (&scan_args);

  /* This hack exists solely for the purposes of formatting the texinfo
     manual.  I couldn't think of a better way.  The token might be
     a simple @@ followed immediately by more text.  If this is the case,
     then the next defun arg is part of this one, and we should concatenate
     them. */
  if (*scan_args && **scan_args && !whitespace (**scan_args) &&
      (strcmp (defined_name, "@@") == 0))
    {
      char *tem = (char *)xmalloc (3 + strlen (scan_args[0]));

      sprintf (tem, "@@%s", scan_args[0]);

      free (scan_args[0]);
      scan_args[0] = tem;
      scan_args++;
      defined_name = tem;
    }

  if (!x_p)
    begin_insertion (type);

  /* Write the definition header line.
     This should start at the normal indentation.  */
  current_indent -= default_indentation_increment;
  start_paragraph ();

  switch (base_type)
    {
    case deffn:
    case defvr:
    case deftp:
      execute_string (" -- %s: %s", category, defined_name);
      break;
    case deftypefn:
    case deftypevr:
      execute_string (" -- %s: %s %s", category, type_name, defined_name);
      break;
    case defcv:
      execute_string (" -- %s of %s: %s", category, type_name, defined_name);
      break;
    case defop:
      execute_string (" -- %s on %s: %s", category, type_name, defined_name);
      break;
    case deftypemethod:
      execute_string (" -- %s on %s: %s %s", category, type_name, type_name2,
                      defined_name);
      break;
    }
  current_indent += default_indentation_increment;

  /* Now process the function arguments, if any.
     If these carry onto the next line, they should be indented by two
     increments to distinguish them from the body of the definition,
     which is indented by one increment.  */
  current_indent += default_indentation_increment;

  switch (base_type)
    {
    case deffn:
    case defop:
      process_defun_args (scan_args, 1);
      break;

      /* Through Makeinfo 1.67 we processed remaining args only for deftp,
         deftypefn, and deftypemethod.  But the libc manual, for example,
         needs to say:
            @deftypevar {char *} tzname[2]
         And simply allowing the extra text seems far simpler than trying
         to invent yet more defn commands.  In any case, we should either
         output it or give an error, not silently ignore it.  */
    default:
      process_defun_args (scan_args, 0);
      break;
    }
  current_indent -= default_indentation_increment;
  close_single_paragraph ();

  /* Make an entry in the appropriate index. */
  switch (base_type)
    {
    case deffn:
    case deftypefn:
      execute_string ("%cfindex %s\n", COMMAND_PREFIX, defined_name);
      break;
    case defvr:
    case deftypevr:
    case defcv:
      execute_string ("%cvindex %s\n", COMMAND_PREFIX, defined_name);
      break;
    case defop:
    case deftypemethod:
      execute_string ("%cfindex %s on %s\n",
		      COMMAND_PREFIX, defined_name, type_name);
      break;
    case deftp:
      execute_string ("%ctindex %s\n", COMMAND_PREFIX, defined_name);
      break;
    }

  /* Deallocate the token list. */
  scan_args = defun_args;
  while (1)
    {
      char * arg = (*scan_args++);
      if (arg == NULL)
        break;
      free (arg);
    }
  free (defun_args);
}

/* Add an entry for a function, macro, special form, variable, or option.
   If the name of the calling command ends in `x', then this is an extra
   entry included in the body of an insertion of the same type. */
void
cm_defun ()
{
  int x_p;
  enum insertion_type type;
  char *temp = xstrdup (command);

  x_p = (command[strlen (command) - 1] == 'x');

  if (x_p)
    temp[strlen (temp) - 1] = 0;

  type = find_type_from_name (temp);
  free (temp);

  /* If we are adding to an already existing insertion, then make sure
     that we are already in an insertion of type TYPE. */
  if (x_p &&
      (!insertion_level || insertion_stack->insertion != type))
    {
      line_error (_("Must be in a `%s' insertion in order to use `%s'x"),
                  command, command);
      discard_until ("\n");
      return;
    }

  defun_internal (type, x_p);
}

/* **************************************************************** */
/*                                                                  */
/*                      Other Random Commands                       */
/*                                                                  */
/* **************************************************************** */

/* This says to inhibit the indentation of the next paragraph, but
   not of following paragraphs.  */
void
cm_noindent ()
{
  if (!inhibit_paragraph_indentation)
    inhibit_paragraph_indentation = -1;
}

/* I don't know exactly what to do with this.  Should I allow
   someone to switch filenames in the middle of output?  Since the
   file could be partially written, this doesn't seem to make sense.
   Another option: ignore it, since they don't *really* want to
   switch files.  Finally, complain, or at least warn. */
void
cm_setfilename ()
{
  char *filename;
  get_rest_of_line (&filename);
  /* warning ("`@%s %s' encountered and ignored", command, filename); */
  free (filename);
}

void
cm_ignore_line ()
{
  discard_until ("\n");
}

/* @br can be immediately followed by `{}', so we have to read those here.
   It should simply close the paragraph. */
void
cm_br ()
{
  if (looking_at ("{}"))
    input_text_offset += 2;

  if (curchar () == '\n')
    {
      input_text_offset++;
      line_number++;
    }

  close_paragraph ();
}

 /* Insert the number of blank lines passed as argument. */
void
cm_sp ()
{
  int lines;
  char *line;

  get_rest_of_line (&line);

  if (sscanf (line, "%d", &lines) != 1)
    {
      line_error (_("%csp requires a positive numeric argument"), COMMAND_PREFIX);
    }
  else
    {
      if (lines < 0)
        lines = 0;

      while (lines--)
        add_char ('\n');
    }
  free (line);
}

/* @dircategory LINE  outputs  INFO-DIR-SECTION LINE,
   but not if --no-headers.  */

void
cm_dircategory ()
{
  char *line;

  get_rest_of_line (&line);;

  if (!no_headers)
    {
      insert_string ("INFO-DIR-SECTION ");
      insert_string (line);
      insert ('\n');
    }

  free (line);
}

/* Start a new line with just this text on it.
   Then center the line of text.
   This always ends the current paragraph. */
void
cm_center ()
{
  register int i, start, length;
  int fudge_factor = 1;
  unsigned char *line;

  close_paragraph ();
  filling_enabled = indented_fill = 0;
  cm_noindent ();
  start = output_paragraph_offset;
  inhibit_output_flushing ();
  get_rest_of_line ((char **)&line);
  execute_string ("%s", (char *)line);
  free (line);
  uninhibit_output_flushing ();

  i = output_paragraph_offset - 1;
  while (i > (start - 1) && output_paragraph[i] == '\n')
        i--;

  output_paragraph_offset = ++i;
  length = output_paragraph_offset - start;

  if (length < (fill_column - fudge_factor))
    {
      line = (unsigned char *)xmalloc (1 + length);
      memcpy (line, (char *)(output_paragraph + start), length);

      i = (fill_column - fudge_factor - length) / 2;
      output_paragraph_offset = start;

      while (i--)
        insert (' ');

      for (i = 0; i < length; i++)
        insert (line[i]);

      free (line);
    }

  insert ('\n');
  close_paragraph ();
  filling_enabled = 1;
}

/* Show what an expression returns. */
void
cm_result (arg)
     int arg;
{
  if (arg == END)
    add_word ("=>");
}

/* What an expression expands to. */
void
cm_expansion (arg)
     int arg;
{
  if (arg == END)
    add_word ("==>");
}

/* Indicates two expressions are equivalent. */
void
cm_equiv (arg)
     int arg;
{
  if (arg == END)
    add_word ("==");
}

/* What an expression may print. */
void
cm_print (arg)
     int arg;
{
  if (arg == END)
    add_word ("-|");
}

/* An error signaled. */
void
cm_error (arg)
     int arg;
{
  if (arg == END)
    add_word ("error-->");
}

/* The location of point in an example of a buffer. */
void
cm_point (arg)
     int arg;
{
  if (arg == END)
    add_word ("-!-");
}

/* Start a new line with just this text on it.
   The text is outdented one level if possible. */
void
cm_exdent ()
{
  char *line;
  int i = current_indent;

  if (current_indent)
    current_indent -= default_indentation_increment;

  get_rest_of_line (&line);
  close_single_paragraph ();
  execute_string ("%s", line);
  current_indent = i;
  free (line);
  close_single_paragraph ();
}


/* Remember this file, and move onto the next. */
void
cm_include ()
{
  char *filename;

#if defined (HAVE_MACROS)
  if (macro_expansion_output_stream && !executing_string)
    me_append_before_this_command ();
#endif /* HAVE_MACROS */

  close_paragraph ();
  get_rest_of_line (&filename);

#if defined (HAVE_MACROS)
  if (macro_expansion_output_stream && !executing_string)
    remember_itext (input_text, input_text_offset);
#endif /* HAVE_MACROS */

  pushfile ();

  /* In verbose mode we print info about including another file. */
  if (verbose_mode)
    {
      register int i = 0;
      register FSTACK *stack = filestack;

      for (i = 0, stack = filestack; stack; stack = stack->next, i++);

      i *= 2;

      printf ("%*s", i, "");
      printf ("%c%s %s\n", COMMAND_PREFIX, command, filename);
      fflush (stdout);
    }

  if (!find_and_load (filename))
    {
      extern int errno;

      popfile ();
      line_number--;

      /* Cannot "@include foo", in line 5 of "/wh/bar". */
      line_error ("%c%s %s: %s", COMMAND_PREFIX, command, filename,
                  strerror (errno));

      free (filename);
      return;
    }
  else
    {
#if defined (HAVE_MACROS)
      if (macro_expansion_output_stream && !executing_string)
        remember_itext (input_text, input_text_offset);
#endif /* HAVE_MACROS */
      reader_loop ();
    }
  free (filename);
  popfile ();
}

/* The other side of a malformed expression. */
void
misplaced_brace ()
{
  line_error (_("Misplaced %c"), '}');
}

/* Signals end of processing.  Easy to make this happen. */
void
cm_bye ()
{
  input_text_offset = size_of_input_text;
}

/* Set the paragraph indentation variable to the value specified in STRING.
   Values can be:
     `asis': Don't change existing indentation.
     `none': Remove existing indentation.
        NUM: Indent NUM spaces at the starts of paragraphs.
             If NUM is zero, we assume `none'.
   Returns 0 if successful, or nonzero if STRING isn't one of the above. */
int
set_paragraph_indent (string)
     char *string;
{
  if (strcmp (string, "asis") == 0 || strcmp (string, _("asis")) == 0)
    paragraph_start_indent = 0;
  else if (strcmp (string, "none") == 0 || strcmp (string, _("none")) == 0)
    paragraph_start_indent = -1;
  else
    {
      if (sscanf (string, "%d", &paragraph_start_indent) != 1)
        return (-1);
      else
        {
          if (paragraph_start_indent == 0)
            paragraph_start_indent = -1;
        }
    }
  return (0);
}

void
cm_paragraphindent ()
{
  char *arg;

  get_rest_of_line (&arg);
  if (set_paragraph_indent (arg) != 0)
    line_error (_("Bad argument to %c%s"), COMMAND_PREFIX, command);

  free (arg);
}

/* **************************************************************** */
/*                                                                  */
/*                      Indexing Stuff                              */
/*                                                                  */
/* **************************************************************** */


/* An index element... */
typedef struct index_elt
{
  struct index_elt *next;
  char *entry;                  /* The index entry itself. */
  char *node;                   /* The node from whence it came. */
  int code;                     /* Nonzero means add `@code{...}' when
                                   printing this element. */
  int defining_line;            /* Line number where this entry was written. */
  char *defining_file;          /* Source file for defining_line. */
} INDEX_ELT;

/* A list of short-names for each index.

   There are two indices into the the_indices array.

   * read_index is the index that points to the list of index
     entries that we will find if we ask for the list of entries for
     this name.

   * write_index is the index that points to the list of index entries
     that we will add new entries to.

   Initially, read_index and write index are the same, but the
   @syncodeindex and @synindex commands can change the list we add
   entries to.

   For example, after the commands

     @cindex foo
     @defindex ii
     @synindex cp ii
     @cindex bar

   the cp index will contain the entry `foo', and the new ii
   index will contain the entry `bar'.  This is consistent with the
   way texinfo.tex handles the same situation.

   In addition, for each index, it is remembered whether that index is
   a code index or not.  Code indices have @code{} inserted around the
   first word when they are printed with printindex. */
typedef struct
{
  char *name;
  int read_index;   /* index entries for `name' */
  int write_index;  /* store index entries here, @synindex can change it */
  int code;
} INDEX_ALIST;

INDEX_ALIST **name_index_alist = (INDEX_ALIST **) NULL;

/* An array of pointers.  Each one is for a different index.  The
   "synindex" command changes which array slot is pointed to by a
   given "index". */
INDEX_ELT **the_indices = (INDEX_ELT **) NULL;

/* The number of defined indices. */
int defined_indices = 0;

void
init_indices ()
{
  int i;

  /* Create the default data structures. */

  /* Initialize data space. */
  if (!the_indices)
    {
      the_indices = (INDEX_ELT **) xmalloc ((1 + defined_indices) *
                                            sizeof (INDEX_ELT *));
      the_indices[defined_indices] = (INDEX_ELT *) NULL;

      name_index_alist = (INDEX_ALIST **) xmalloc ((1 + defined_indices) *
                                                   sizeof (INDEX_ALIST *));
      name_index_alist[defined_indices] = (INDEX_ALIST *) NULL;
    }

  /* If there were existing indices, get rid of them now. */
  for (i = 0; i < defined_indices; i++)
    {
      undefindex (name_index_alist[i]->name);
      if (name_index_alist[i])
        { /* Suppose we're called with two input files, and the first
             does a @synindex pg cp.  Then, when we get here to start
             the second file, the "pg" element won't get freed by
             undefindex (because it's pointing to "cp").  So free it
             here; otherwise, when we try to define the pg index again
             just below, it will still point to cp.  */
          free (name_index_alist[i]->name);
          free (name_index_alist[i]);
          name_index_alist[i] = (INDEX_ALIST *) NULL;
        }
    }

  /* Add the default indices. */
  top_defindex ("cp", 0);           /* cp is the only non-code index.  */
  top_defindex ("fn", 1);
  top_defindex ("ky", 1);
  top_defindex ("pg", 1);
  top_defindex ("tp", 1);
  top_defindex ("vr", 1);
}

/* Find which element in the known list of indices has this name.
   Returns -1 if NAME isn't found. */
int
find_index_offset (name)
     char *name;
{
  register int i;
  for (i = 0; i < defined_indices; i++)
    if (name_index_alist[i] &&
        strcmp (name, name_index_alist[i]->name) == 0)
      return (i);
  return (-1);
}

/* Return a pointer to the entry of (name . index) for this name.
   Return NULL if the index doesn't exist. */
INDEX_ALIST *
find_index (name)
     char *name;
{
  int offset = find_index_offset (name);
  if (offset > -1)
    return (name_index_alist[offset]);
  else
    return ((INDEX_ALIST *) NULL);
}

/* Given an index name, return the offset in the_indices of this index,
   or -1 if there is no such index. */
int
translate_index (name)
     char *name;
{
  INDEX_ALIST *which = find_index (name);

  if (which)
    return (which->read_index);
  else
    return (-1);
}

/* Return the index list which belongs to NAME. */
INDEX_ELT *
index_list (name)
     char *name;
{
  int which = translate_index (name);
  if (which < 0)
    return ((INDEX_ELT *) -1);
  else
    return (the_indices[which]);
}

/* Please release me, let me go... */
void
free_index (index)
     INDEX_ELT *index;
{
  INDEX_ELT *temp;

  while ((temp = index) != (INDEX_ELT *) NULL)
    {
      free (temp->entry);
      /* Do not free the node, because we already freed the tag table,
         which freed all the node names.  */
      /* free (temp->node); */
      index = index->next;
      free (temp);
    }
}

/* Flush an index by name.  This will delete the list of entries that
   would be written by a @printindex command for this index. */
void
undefindex (name)
     char *name;
{
  int i;
  int which = find_index_offset (name);

  /* The index might have already been freed if this was the target of
     an @synindex.  */
  if (which < 0 || !name_index_alist[which])
    return;

  i = name_index_alist[which]->read_index;

  free_index (the_indices[i]);
  the_indices[i] = (INDEX_ELT *) NULL;

  free (name_index_alist[which]->name);
  free (name_index_alist[which]);
  name_index_alist[which] = (INDEX_ALIST *) NULL;
}

/* Define an index known as NAME.  We assign the slot number.
   CODE if Nonzero says to make this a code index. */
void
defindex (name, code)
     char *name;
     int code;
{
  register int i, slot;

  /* If it already exists, flush it. */
  undefindex (name);

  /* Try to find an empty slot. */
  slot = -1;
  for (i = 0; i < defined_indices; i++)
    if (!name_index_alist[i])
      {
        slot = i;
        break;
      }

  if (slot < 0)
    {
      /* No such luck.  Make space for another index. */
      slot = defined_indices;
      defined_indices++;

      name_index_alist = (INDEX_ALIST **)
        xrealloc ((char *)name_index_alist,
                  (1 + defined_indices) * sizeof (INDEX_ALIST *));
      the_indices = (INDEX_ELT **)
        xrealloc ((char *)the_indices,
                  (1 + defined_indices) * sizeof (INDEX_ELT *));
    }

  /* We have a slot.  Start assigning. */
  name_index_alist[slot] = (INDEX_ALIST *) xmalloc (sizeof (INDEX_ALIST));
  name_index_alist[slot]->name = xstrdup (name);
  name_index_alist[slot]->read_index = slot;
  name_index_alist[slot]->write_index = slot;
  name_index_alist[slot]->code = code;

  the_indices[slot] = (INDEX_ELT *) NULL;
}

/* Add the arguments to the current index command to the index NAME. */
void
index_add_arg (name)
     char *name;
{
  int which;
  char *index_entry;
  INDEX_ALIST *tem;

  tem = find_index (name);

  which = tem ? tem->write_index : -1;

#if defined (HAVE_MACROS)
  if (macro_expansion_output_stream && !executing_string)
    append_to_expansion_output (input_text_offset + 1);
#endif /* HAVE_MACROS */

  get_rest_of_line (&index_entry);
  ignore_blank_line ();

#if defined (HAVE_MACROS)
  if (macro_expansion_output_stream && !executing_string)
    {
      int op_orig;

      remember_itext (input_text, input_text_offset);
      op_orig = output_paragraph_offset;
      me_execute_string (index_entry);
      me_execute_string ("\n");
      output_paragraph_offset = op_orig;
    }
#endif /* HAVE_MACROS */

  if (which < 0)
    {
      line_error (_("Unknown index `%s'"), name);
      free (index_entry);
    }
  else
    {
      INDEX_ELT *new = (INDEX_ELT *) xmalloc (sizeof (INDEX_ELT));
      new->next = the_indices[which];
      new->entry = index_entry;
      new->node = current_node;
      new->code = tem->code;
      new->defining_line = line_number - 1;
      new->defining_file = input_filename;
      the_indices[which] = new;
    }
}

#define INDEX_COMMAND_SUFFIX "index"

/* The function which user defined index commands call. */
void
gen_index ()
{
  char *name = xstrdup (command);
  if (strlen (name) >= strlen ("index"))
    name[strlen (name) - strlen ("index")] = 0;
  index_add_arg (name);
  free (name);
}

void
top_defindex (name, code)
     char *name;
     int code;
{
  char *temp;

  temp = (char *) xmalloc (1 + strlen (name) + strlen ("index"));
  sprintf (temp, "%sindex", name);
  define_user_command (temp, gen_index, 0);
  defindex (name, code);
  free (temp);
}

/* Define a new index command.  Arg is name of index. */
void
cm_defindex ()
{
  gen_defindex (0);
}

void
cm_defcodeindex ()
{
  gen_defindex (1);
}

void
gen_defindex (code)
     int code;
{
  char *name;
  get_rest_of_line (&name);

  if (find_index (name))
    {
      line_error (_("Index `%s' already exists"), name);
      free (name);
      return;
    }
  else
    {
      char *temp = (char *) alloca (1 + strlen (name) + strlen ("index"));
      sprintf (temp, "%sindex", name);
      define_user_command (temp, gen_index, 0);
      defindex (name, code);
      free (name);
    }
}

/* Expects 2 args, on the same line.  Both are index abbreviations.
   Make the first one be a synonym for the second one, i.e. make the
   first one have the same index as the second one. */
void
cm_synindex ()
{
  int source, target;
  char *abbrev1, *abbrev2;

  skip_whitespace ();
  get_until_in_line (0, " ", &abbrev1);
  target = find_index_offset (abbrev1);
  skip_whitespace ();
  get_until_in_line (0, " ", &abbrev2);
  source = find_index_offset (abbrev2);
  if (source < 0 || target < 0)
    {
      line_error (_("Unknown index `%s' and/or `%s' in @synindex"),
                  abbrev1, abbrev2);
    }
  else
    {
      name_index_alist[target]->write_index
        = name_index_alist[source]->write_index;
    }
  
  free (abbrev1);
  free (abbrev2);
}

void
cm_pindex ()                    /* Pinhead index. */
{
  index_add_arg ("pg");
}

void
cm_vindex ()                    /* Variable index. */
{
  index_add_arg ("vr");
}

void
cm_kindex ()                    /* Key index. */
{
  index_add_arg ("ky");
}

void
cm_cindex ()                    /* Concept index. */
{
  index_add_arg ("cp");
}

void
cm_findex ()                    /* Function index. */
{
  index_add_arg ("fn");
}

void
cm_tindex ()                    /* Data Type index. */
{
  index_add_arg ("tp");
}

/* Sorting the index. */
int
index_element_compare (element1, element2)
     INDEX_ELT **element1, **element2;
{
  return (strcasecmp ((*element1)->entry, (*element2)->entry));
}

/* Force all index entries to be unique. */
void
make_index_entries_unique (array, count)
     INDEX_ELT **array;
     int count;
{
  register int i, j;
  INDEX_ELT **copy;
  int counter = 1;

  copy = (INDEX_ELT **)xmalloc ((1 + count) * sizeof (INDEX_ELT *));

  for (i = 0, j = 0; i < count; i++)
    {
      if ((i == (count - 1)) ||
          (array[i]->node != array[i + 1]->node) ||
          (strcmp (array[i]->entry, array[i + 1]->entry) != 0))
        copy[j++] = array[i];
      else
        {
          free (array[i]->entry);
          free (array[i]);
        }
    }
  copy[j] = (INDEX_ELT *)NULL;

  /* Now COPY contains only unique entries.  Duplicated entries in the
     original array have been freed.  Replace the current array with
     the copy, fixing the NEXT pointers. */
  for (i = 0; copy[i] != (INDEX_ELT *)NULL; i++)
    {

      copy[i]->next = copy[i + 1];

      /* Fix entry names which are the same.  They point to different nodes,
         so we make the entry name unique. */
      if ((copy[i + 1] != (INDEX_ELT *)NULL) &&
          (strcmp (copy[i]->entry, copy[i + 1]->entry) == 0))
        {
          char *new_entry_name;

          new_entry_name = (char *)xmalloc (10 + strlen (copy[i]->entry));
          sprintf (new_entry_name, "%s <%d>", copy[i]->entry, counter);
          free (copy[i]->entry);
          copy[i]->entry = new_entry_name;
          counter++;
        }
      else
        counter = 1;

      array[i] = copy[i];
    }
  array[i] = (INDEX_ELT *)NULL;

  /* Free the storage used only by COPY. */
  free (copy);
}

/* Sort the index passed in INDEX, returning an array of
   pointers to elements.  The array is terminated with a NULL
   pointer.  We call qsort because it's supposed to be fast.
   I think this looks bad. */
INDEX_ELT **
sort_index (index)
     INDEX_ELT *index;
{
  INDEX_ELT **array;
  INDEX_ELT *temp = index;
  int count = 0;
  int save_line_number = line_number;
  char *save_input_filename = input_filename;

  while (temp != (INDEX_ELT *) NULL)
    {
      count++;
      temp = temp->next;
    }

  /* We have the length.  Make an array. */

  array = (INDEX_ELT **) xmalloc ((count + 1) * sizeof (INDEX_ELT *));
  count = 0;
  temp = index;

  while (temp != (INDEX_ELT *) NULL)
    {
      array[count++] = temp;

      /* Set line number and input filename to the source line for this
         index entry, as this expansion finds any errors.  */
      line_number = array[count - 1]->defining_line;
      input_filename = array[count - 1]->defining_file;

      /* If this particular entry should be printed as a "code" index,
         then wrap the entry with "@code{...}". */
      array[count - 1]->entry = expansion (temp->entry, index->code);
      
      temp = temp->next;
    }
  array[count] = (INDEX_ELT *) NULL;    /* terminate the array. */
  line_number = save_line_number;
  input_filename = save_input_filename;

  /* Sort the array. */
  qsort (array, count, sizeof (INDEX_ELT *), index_element_compare);
  make_index_entries_unique (array, count);
  return (array);
}

/* Nonzero means that we are in the middle of printing an index. */
int printing_index = 0;

/* Takes one arg, a short name of an index to print.
   Outputs a menu of the sorted elements of the index. */
void
cm_printindex ()
{
  int item;
  INDEX_ELT *index;
  INDEX_ELT **array;
  char *index_name;
  unsigned line_length;
  char *line;
  int saved_inhibit_paragraph_indentation = inhibit_paragraph_indentation;
  int saved_filling_enabled = filling_enabled;

  close_paragraph ();
  get_rest_of_line (&index_name);

  index = index_list (index_name);
  if (index == (INDEX_ELT *)-1)
    {
      line_error (_("Unknown index `%s' in @printindex"), index_name);
      free (index_name);
      return;
    }
  else
    free (index_name);

  /* Do this before sorting, so execute_string in index_element_compare
     will give the same results as when we actually print.  */
  printing_index = 1;
  filling_enabled = 0;
  inhibit_paragraph_indentation = 1;
  array = sort_index (index);

  close_paragraph ();
  add_word (_("* Menu:\n\n"));

#if defined (HAVE_MACROS)
  me_inhibit_expansion++;
#endif /* HAVE_MACROS */

  /* This will probably be enough.  */
  line_length = 100;
  line = xmalloc (line_length);
  
  for (item = 0; (index = array[item]); item++)
    {
      /* A pathological document might have an index entry outside of any
         node.  Don't crash.  Perhaps should warn.  */
      char *index_node = index->node ? index->node : "(none)";
      unsigned new_length = strlen (index->entry);

      if (new_length < 37) /* minimum length used below */
        new_length = 37;
      new_length += strlen (index_node) + 7; /* * : .\n\0 */
      
      if (new_length > line_length)
        {
          line_length = new_length;
          line = xrealloc (line, line_length);
        }

      /* Print the entry, nicely formatted.  We've already expanded any
         commands, including any implicit @code.  Thus, can't call
         execute_string, since @@ has turned into @.  */
      sprintf (line, "* %-37s  %s.\n", index->entry, index_node);
      line[2 + strlen (index->entry)] = ':';
      insert_string (line);

      /* Previous `output_paragraph' from growing to the size of the
         whole index.  */
      flush_output ();
    }

  free (line);
  
#if defined (HAVE_MACROS)
  me_inhibit_expansion--;
#endif /* HAVE_MACROS */

  printing_index = 0;
  free (array);
  close_single_paragraph ();
  filling_enabled = saved_filling_enabled;
  inhibit_paragraph_indentation = saved_inhibit_paragraph_indentation;
}

/* User-defined commands, which happens only from user-defined indexes. */

void
define_user_command (name, proc, needs_braces_p)
     char *name;
     COMMAND_FUNCTION *proc;
     int needs_braces_p;
{
  int slot = user_command_array_len;
  user_command_array_len++;

  if (!user_command_array)
    user_command_array = (COMMAND **) xmalloc (1 * sizeof (COMMAND *));

  user_command_array = (COMMAND **) xrealloc (user_command_array,
                                              (1 + user_command_array_len) *
                                              sizeof (COMMAND *));

  user_command_array[slot] = (COMMAND *) xmalloc (sizeof (COMMAND));
  user_command_array[slot]->name = xstrdup (name);
  user_command_array[slot]->proc = proc;
  user_command_array[slot]->argument_in_braces = needs_braces_p;
}

/* Some support for footnotes. */

/* Footnotes are a new construct in Info.  We don't know the best method
   of implementing them for sure, so we present two possiblities.

   SeparateNode:
        Make them look like followed references, with the reference
        destinations in a makeinfo manufactured node or,

   EndNode:
        Make them appear at the bottom of the node that they originally
        appeared in. */
#define SeparateNode 0
#define EndNode 1

int footnote_style = EndNode;
int first_footnote_this_node = 1;
int footnote_count = 0;

/* Set the footnote style based on he style identifier in STRING. */
int
set_footnote_style (string)
     char *string;
{
  if ((strcasecmp (string, "separate") == 0) ||
      (strcasecmp (string, "MN") == 0))
    footnote_style = SeparateNode;
  else if ((strcasecmp (string, "end") == 0) ||
           (strcasecmp (string, "EN") == 0))
    footnote_style = EndNode;
  else
    return (-1);

 return (0);
}

void
cm_footnotestyle ()
{
  char *arg;

  get_rest_of_line (&arg);

  /* If set on command line, do not change the footnote style.  */
  if (!footnote_style_preset && set_footnote_style (arg) != 0)
    line_error ("Bad argument to %c%s", COMMAND_PREFIX, command);

  free (arg);
}

typedef struct fn
{
  struct fn *next;
  char *marker;
  char *note;
}  FN;

FN *pending_notes = (FN *) NULL;

/* A method for remembering footnotes.  Note that this list gets output
   at the end of the current node. */
void
remember_note (marker, note)
     char *marker, *note;
{
  FN *temp = (FN *) xmalloc (sizeof (FN));

  temp->marker = xstrdup (marker);
  temp->note = xstrdup (note);
  temp->next = pending_notes;
  pending_notes = temp;
  footnote_count++;
}

/* How to get rid of existing footnotes. */
void
free_pending_notes ()
{
  FN *temp;

  while ((temp = pending_notes) != (FN *) NULL)
    {
      free (temp->marker);
      free (temp->note);
      pending_notes = pending_notes->next;
      free (temp);
    }
  first_footnote_this_node = 1;
  footnote_count = 0;
}

/* What to do when you see a @footnote construct. */

 /* Handle a "footnote".
    footnote *{this is a footnote}
    where "*" is the (optional) marker character for this note. */
void
cm_footnote ()
{
  char *marker;
  char *note;

  get_until ("{", &marker);
  canon_white (marker);

  if (macro_expansion_output_stream && !executing_string)
    append_to_expansion_output (input_text_offset + 1); /* include the { */

  /* Read the argument in braces. */
  if (curchar () != '{')
    {
      line_error (_("`%c%s' needs an argument `{...}', not just `%s'"),
                  COMMAND_PREFIX, command, marker);
      free (marker);
      return;
    }
  else
    {
      int len;
      int braces = 1;
      int loc = ++input_text_offset;

      while (braces)
        {
          if (loc == size_of_input_text)
            {
              line_error (_("No closing brace for footnote `%s'"), marker);
              return;
            }

          if (input_text[loc] == '{')
            braces++;
          else if (input_text[loc] == '}')
            braces--;
          else if (input_text[loc] == '\n')
            line_number++;

          loc++;
        }

      len = (loc - input_text_offset) - 1;
      note = (char *)xmalloc (len + 1);
      strncpy (note, &input_text[input_text_offset], len);
      note[len] = 0;
      input_text_offset = loc;
    }

  /* Must write the macro-expanded argument to the macro expansion
     output stream.  This is like the case in index_add_arg.  */
  if (macro_expansion_output_stream && !executing_string)
    {
      int op_orig;

      remember_itext (input_text, input_text_offset);
      op_orig = output_paragraph_offset;
      me_execute_string (note);
      /* Calling me_execute_string on a lone } provokes an error, since
         as far as the reader knows there is no matching {.  We wrote
         the { above in the call to append_to_expansion_output. */
      write_region_to_macro_output ("}", 0, 1);
      output_paragraph_offset = op_orig;
    }

  if (!current_node || !*current_node)
    {
      line_error (_("Footnote defined without parent node"));
      free (marker);
      free (note);
      return;
    }

  if (!*marker)
    {
      free (marker);

      if (number_footnotes)
        {
          marker = (char *)xmalloc (10);
          sprintf (marker, "%d", current_footnote_number);
          current_footnote_number++;
        }
      else
        marker = xstrdup ("*");
    }

  remember_note (marker, note);

  /* Your method should at least insert MARKER. */
  switch (footnote_style)
    {
    case SeparateNode:
      add_word_args ("(%s)", marker);
      if (first_footnote_this_node)
        {
          char *temp_string;

          temp_string = (char *)
            xmalloc ((strlen (current_node)) + (strlen (_("-Footnotes"))) + 1);

          add_word_args (" (*note %s-Footnotes::)", current_node);
          strcpy (temp_string, current_node);
          strcat (temp_string, "-Footnotes");
          remember_node_reference (temp_string, line_number, followed_reference);
          free (temp_string);
          first_footnote_this_node = 0;
        }
      break;

    case EndNode:
      add_word_args ("(%s)", marker);
      break;

    default:
      break;
    }
  free (marker);
  free (note);
}

/* Nonzero means that we are currently in the process of outputting
   footnotes. */
int already_outputting_pending_notes = 0;

/* Output the footnotes.  We are at the end of the current node. */
void
output_pending_notes ()
{
  FN *footnote = pending_notes;

  if (!pending_notes)
    return;

  switch (footnote_style)
    {
    case SeparateNode:
      {
        char *old_current_node = current_node;
        char *old_command = xstrdup (command);

        already_outputting_pending_notes++;
        execute_string ("%cnode %s-Footnotes,,,%s\n",
                        COMMAND_PREFIX, current_node, current_node);
        already_outputting_pending_notes--;
        current_node = old_current_node;
        free (command);
        command = old_command;
      }
      break;

    case EndNode:
      close_paragraph ();
      in_fixed_width_font++;
      execute_string (_("---------- Footnotes ----------\n\n"));
      in_fixed_width_font--;
      break;
    }

  /* Handle the footnotes in reverse order. */
  {
    FN **array = (FN **) xmalloc ((footnote_count + 1) * sizeof (FN *));

    array[footnote_count] = (FN *) NULL;

    while (--footnote_count > -1)
      {
        array[footnote_count] = footnote;
        footnote = footnote->next;
      }

    filling_enabled = 1;
    indented_fill = 1;

    while ((footnote = array[++footnote_count]))
      {
        execute_string ("(%s) %s", footnote->marker, footnote->note);
        close_paragraph ();
      }
    close_paragraph ();
    free (array);
  }
}

/* **************************************************************** */
/*                                                                  */
/*              User definable Macros (text substitution)           */
/*                                                                  */
/* **************************************************************** */

#if defined (HAVE_MACROS)

/* Array of macros and definitions. */
MACRO_DEF **macro_list = (MACRO_DEF **)NULL;

int macro_list_len = 0;         /* Number of elements. */
int macro_list_size = 0;        /* Number of slots in total. */

/* Return the macro definition of NAME or NULL if NAME is not defined. */
MACRO_DEF *
find_macro (name)
     char *name;
{
  register int i;
  register MACRO_DEF *def;

  def = (MACRO_DEF *)NULL;
  for (i = 0; macro_list && (def = macro_list[i]); i++)
    {
      if ((!def->inhibited) && (strcmp (def->name, name) == 0))
        break;
    }
  return (def);
}

/* Add the macro NAME with ARGLIST and BODY to the list of defined macros.
   SOURCE_FILE is the name of the file where this definition can be found,
   and SOURCE_LINENO is the line number within that file.  If a macro already
   exists with NAME, then a warning is produced, and that previous
   definition is overwritten. */
void
add_macro (name, arglist, body, source_file, source_lineno, flags)
     char *name;
     char **arglist;
     char *body;
     char *source_file;
     int source_lineno, flags;
{
  register MACRO_DEF *def;

  def = find_macro (name);

  if (!def)
    {
      if (macro_list_len + 2 >= macro_list_size)
        macro_list = (MACRO_DEF **)xrealloc
          (macro_list, ((macro_list_size += 10) * sizeof (MACRO_DEF *)));

      macro_list[macro_list_len] = (MACRO_DEF *)xmalloc (sizeof (MACRO_DEF));
      macro_list[macro_list_len + 1] = (MACRO_DEF *)NULL;

      def = macro_list[macro_list_len];
      macro_list_len += 1;
      def->name = name;
    }
  else
    {
      char *temp_filename = input_filename;
      int temp_line = line_number;

      warning (_("macro `%s' previously defined"), name);

      input_filename = def->source_file;
      line_number = def->source_lineno;
      warning (_("here is the previous definition of `%s'"), name);

      input_filename = temp_filename;
      line_number = temp_line;

      if (def->arglist)
        {
          register int i;

          for (i = 0; def->arglist[i]; i++)
            free (def->arglist[i]);

          free (def->arglist);
        }
      free (def->source_file);
      free (def->body);
    }

  def->source_file = xstrdup (source_file);
  def->source_lineno = source_lineno;
  def->body = body;
  def->arglist = arglist;
  def->inhibited = 0;
  def->flags = flags;
}

/* Delete the macro with name NAME.  The macro is deleted from the list,
   but it is also returned.  If there was no macro defined, NULL is
   returned. */
MACRO_DEF *
delete_macro (name)
     char *name;
{
  register int i;
  register MACRO_DEF *def;

  def = (MACRO_DEF *)NULL;

  for (i = 0; macro_list && (def = macro_list[i]); i++)
    if (strcmp (def->name, name) == 0)
      {
        memmove (macro_list + i, macro_list + i + 1,
               ((macro_list_len + 1) - i) * sizeof (MACRO_DEF *));
        macro_list_len--;
        break;
      }
  return (def);
}

/* Return the arglist on the current line.  This can behave in two different
   ways, depending on the variable BRACES_REQUIRED_FOR_MACRO_ARGS. */
int braces_required_for_macro_args = 0;

char **
get_macro_args (def)
    MACRO_DEF *def;
{
  register int i;
  char *word;

  /* Quickly check to see if this macro has been invoked with any arguments.
     If not, then don't skip any of the following whitespace. */
  for (i = input_text_offset; i < size_of_input_text; i++)
    if (!cr_or_whitespace (input_text[i]))
      break;

  if (input_text[i] != '{')
    {
      if (braces_required_for_macro_args)
        {
          return ((char **)NULL);
        }
      else
        {
          /* Braces are not required to fill out the macro arguments.  If
             this macro takes one argument, it is considered to be the
             remainder of the line, sans whitespace. */
          if (def->arglist && def->arglist[0] && !def->arglist[1])
            {
              char **arglist;

              get_rest_of_line (&word);
              if (input_text[input_text_offset - 1] == '\n')
                {
                  input_text_offset--;
                  line_number--;
                }
              /* canon_white (word); */
              arglist = (char **)xmalloc (2 * sizeof (char *));
              arglist[0] = word;
              arglist[1] = (char *)NULL;
              return (arglist);
            }
          else
            {
              /* The macro either took no arguments, or took more than
                 one argument.  In that case, it must be invoked with
                 arguments surrounded by braces. */
              return ((char **)NULL);
            }
        }
    }
  return (get_brace_args (def->flags & ME_QUOTE_ARG));
}

/* Substitute actual parameters for named parameters in body.
   The named parameters which appear in BODY must by surrounded
   reverse slashes, as in \foo\. */
char *
apply (named, actuals, body)
     char **named, **actuals, *body;
{
  register int i;
  int new_body_index, new_body_size;
  char *new_body, *text;
  int length_of_actuals;

  length_of_actuals = array_len (actuals);
  new_body_size = strlen (body);
  new_body = (char *)xmalloc (1 + new_body_size);

  /* Copy chars from BODY into NEW_BODY. */
  i = 0; new_body_index = 0;

  while (1)
    {
      if (!body[i])
        break;

      if (body[i] != '\\')
        new_body[new_body_index++] = body[i++];
      else
        {
          /* Snarf parameter name, check against named parameters. */
          char *param;
          int param_start, which, len;

          param_start = ++i;
          while ((body[i]) && (body[i] != '\\'))
            i++;

          len = i - param_start;
          param = (char *)xmalloc (1 + len);
          memcpy (param, body + param_start, len);
          param[len] = 0;

          if (body[i]) /* move past \ */
            i++;

          /* Now check against named parameters. */
          for (which = 0; named && named[which]; which++)
            if (strcmp (named[which], param) == 0)
              break;

          if (named && named[which])
            {
              if (which < length_of_actuals)
                text = actuals[which];
              else
                text = (char *)NULL;

              if (!text)
                text = "";

              len = strlen (text);
            }
          else
            { /* not a parameter, restore \'s */
              i = body[i] ? (i - 1) : i;
              len++;
              text = xmalloc (1 + len);
              sprintf (text, "\\%s", param);
            }

          if ((2 + strlen (param)) < len)
            {
              new_body_size += len + 1;
              new_body = xrealloc (new_body, new_body_size);
            }

          free (param);

          strcpy (new_body + new_body_index, text);
          new_body_index += len;

          if (!named || !named[which])
            free (text);
        }
    }
  new_body[new_body_index] = 0;
  return (new_body);
}

/* Execute the macro passed in DEF, a pointer to a MACRO_DEF.  */
void
execute_macro (def)
     MACRO_DEF *def;
{
  char **arglist;
  int num_args;
  char *execution_string = (char *)NULL;

  if (macro_expansion_output_stream && !executing_string && !me_inhibit_expansion)
    me_append_before_this_command ();

  /* Find out how many arguments this macro definition takes. */
  num_args = array_len (def->arglist);

  /* Gather the arguments present on the line if there are any. */
  arglist = get_macro_args (def);

  if (num_args < array_len (arglist))
    {
      free_array (arglist);
      line_error (_("Macro `%s' called with too many args"), def->name);
      return;
    }

  if (def->body)
    execution_string = apply (def->arglist, arglist, def->body);

  free_array (arglist);

  if (def->body)
    {
      if (macro_expansion_output_stream && !executing_string && !me_inhibit_expansion)
        {
          remember_itext (input_text, input_text_offset);
          me_execute_string (execution_string);
        }
      else
        execute_string ("%s", execution_string);

      free (execution_string);
    }
}

/* Read and remember the definition of a macro. */
void
cm_macro ()
{
  register int i;
  char *name, **arglist, *body, *line;
  int body_size, body_index;
  int depth = 1;
  int defining_line = line_number;
  int flags = 0;

  arglist = (char **)NULL;
  body = (char *)NULL;
  body_size = 0;
  body_index = 0;

  if (macro_expansion_output_stream && !executing_string)
    me_append_before_this_command ();

  skip_whitespace ();

  /* Get the name of the macro.  This is the set of characters which are
     not whitespace and are not `{' immediately following the @macro. */
  {
    int start = input_text_offset;
    int len;

    for (i = start;
         (i < size_of_input_text) &&
         (input_text[i] != '{') &&
         (!cr_or_whitespace (input_text[i]));
         i++);

    len = i - start;
    name = (char *)xmalloc (1 + len);
    strncpy (name, input_text + start, len);
    name[len] = 0;
    input_text_offset = i;
  }

  skip_whitespace ();

  /* It is not required that the definition of a macro includes an arglist.
     If not, don't try to get the named parameters, just use a null list. */
  if (curchar () == '{')
    {
      int arglist_index = 0, arglist_size = 0;
      int gathering_words = 1;
      char *word = (char *)NULL;
      int character;

      /* Read the words inside of the braces which determine the arglist.
         These words will be replaced within the body of the macro at
         execution time. */

      input_text_offset++;
      skip_whitespace_and_newlines ();

      while (gathering_words)
        {
          int len;

          for (i = input_text_offset;
               (character = input_text[i]);
               i++)
            {
              switch (character)
                {
                case '\n':
                  line_number++;
                case ' ':
                case '\t':
                case ',':
                case '}':
                  /* Found the end of the current arglist word.  Save it. */
                  len = i - input_text_offset;
                  word = (char *)xmalloc (1 + len);
                  strncpy (word, input_text + input_text_offset, len);
                  word[len] = 0;
                  input_text_offset = i;

                  /* Advance to the comma or close-brace that signified
                     the end of the argument. */
                  while ((character = curchar ())
                         && character != ','
                         && character != '}')
                    {
                      input_text_offset++;
                      if (character == '\n')
                        line_number++;
                    }

                  /* Add the word to our list of words. */
                  if ((arglist_index + 2) >= arglist_size)
                    arglist = (char **)xrealloc
                      (arglist, (arglist_size += 10) * sizeof (char *));

                  arglist[arglist_index++] = word;
                  arglist[arglist_index] = (char *)NULL;
                  break;
                }

              if (character == '}')
                {
                  input_text_offset++;
                  gathering_words = 0;
                  break;
                }

              if (character == ',')
                {
                  input_text_offset++;
                  skip_whitespace_and_newlines ();
                  i = input_text_offset - 1;
                }
            }
        }
    }

  /* Read the text carefully until we find an "@end macro" which
     matches this one.  The text in between is the body of the macro. */
  skip_whitespace_and_newlines ();

  while (depth)
    {
      if ((input_text_offset + 9) > size_of_input_text)
        {
          int temp_line = line_number;
          line_number = defining_line;
          line_error (_("%cend macro not found"), COMMAND_PREFIX);
          line_number = temp_line;
          return;
        }

      get_rest_of_line (&line);

      /* Handle commands only meaningful within a macro. */
      if ((*line == COMMAND_PREFIX) && (depth == 1) &&
          (strncmp (line + 1, "allow-recursion", 15) == 0) &&
          (line[16] == 0 || whitespace (line[16])))
        {
          for (i = 16; whitespace (line[i]); i++);
          strcpy (line, line + i);
          flags |= ME_RECURSE;
          if (!*line)
            {
              free (line);
              continue;
            }
        }

      if ((*line == COMMAND_PREFIX) && (depth == 1) &&
          (strncmp (line + 1, "quote-arg", 9) == 0) &&
          (line[10] == 0 || whitespace (line[10])))
        {
          for (i = 10; whitespace (line[i]); i++);
          strcpy (line, line + i);

          if (arglist && arglist[0] && !arglist[1])
            {
              flags |= ME_QUOTE_ARG;
              if (!*line)
                {
                  free (line);
                  continue;
                }
            }
          else
            {
              line_error (_("%cquote-arg only useful when the macro takes a single argument"),
                          COMMAND_PREFIX);
            }
        }

      if ((*line == COMMAND_PREFIX) &&
          (strncmp (line + 1, "macro ", 6) == 0))
        depth++;

      if ((*line == COMMAND_PREFIX) &&
          (strncmp (line + 1, "end macro", 9) == 0))
        depth--;

      if (depth)
        {
          if ((body_index + strlen (line) + 3) >= body_size)
            body = (char *)xrealloc
              (body, body_size += 3 + strlen (line));
          strcpy (body + body_index, line);
          body_index += strlen (line);
          body[body_index++] = '\n';
          body[body_index] = 0;
        }
      free (line);
    }

  /* If it was an empty macro like
     @macro foo
     @end macro
     create an empty body.  (Otherwise, the macro is not expanded.)  */
  if (!body)
    {
      body = (char *)malloc(1);
      *body = 0;
    }

  /* We now have the name, the arglist, and the body.  However, BODY
     includes the final newline which preceded the `@end macro' text.
     Delete it. */
  if (body && strlen (body))
    body[strlen (body) - 1] = 0;

  add_macro (name, arglist, body, input_filename, defining_line, flags);

  if (macro_expansion_output_stream && !executing_string)
    remember_itext (input_text, input_text_offset);
}

void
cm_unmacro ()
{
  register int i;
  char *line, *name;
  MACRO_DEF *def;

  if (macro_expansion_output_stream && !executing_string)
    me_append_before_this_command ();

  get_rest_of_line (&line);

  for (i = 0; line[i] && !whitespace (line[i]); i++);
  name = (char *)xmalloc (i + 1);
  strncpy (name, line, i);
  name[i] = 0;

  def = delete_macro (name);

  if (def)
    {
      free (def->source_file);
      free (def->name);
      free (def->body);

      if (def->arglist)
        {
          register int i;

          for (i = 0; def->arglist[i]; i++)
            free (def->arglist[i]);

          free (def->arglist);
        }

      free (def);
    }

  free (line);
  free (name);

  if (macro_expansion_output_stream && !executing_string)
    remember_itext (input_text, input_text_offset);
}

/* How to output sections of the input file verbatim. */

/* Set the value of POINTER's offset to OFFSET. */
ITEXT *
remember_itext (pointer, offset)
     char *pointer;
     int offset;
{
  register int i;
  ITEXT *itext = (ITEXT *)NULL;

  /* If we have no info, initialize a blank list. */
  if (!itext_info)
    {
      itext_info = (ITEXT **)xmalloc ((itext_size = 10) * sizeof (ITEXT *));
      for (i = 0; i < itext_size; i++)
        itext_info[i] = (ITEXT *)NULL;
    }

  /* If the pointer is already present in the list, then set the offset. */
  for (i = 0; i < itext_size; i++)
    if ((itext_info[i] != (ITEXT *)NULL) &&
        (itext_info[i]->pointer == pointer))
      {
        itext = itext_info[i];
        itext_info[i]->offset = offset;
        break;
      }

  if (i == itext_size)
    {
      /* Find a blank slot (or create a new one), and remember the
         pointer and offset. */
      for (i = 0; i < itext_size; i++)
        if (itext_info[i] == (ITEXT *)NULL)
          break;

      /* If not found, then add some slots. */
      if (i == itext_size)
        {
          register int j;

          itext_info = (ITEXT **)xrealloc
            (itext_info, (itext_size += 10) * sizeof (ITEXT *));

          for (j = i; j < itext_size; j++)
            itext_info[j] = (ITEXT *)NULL;
        }

      /* Now add the pointer and the offset. */
      itext_info[i] = (ITEXT *)xmalloc (sizeof (ITEXT));
      itext_info[i]->pointer = pointer;
      itext_info[i]->offset = offset;
      itext = itext_info[i];
    }
  return (itext);
}

/* Forget the input text associated with POINTER. */
void
forget_itext (pointer)
     char *pointer;
{
  register int i;

  for (i = 0; i < itext_size; i++)
    if (itext_info[i] && (itext_info[i]->pointer == pointer))
      {
        free (itext_info[i]);
        itext_info[i] = (ITEXT *)NULL;
        break;
      }
}

/* Append the text which appeared in input_text from the last offset to
   the character just before the command that we are currently executing. */
void
me_append_before_this_command ()
{
  register int i;

  for (i = input_text_offset; i && (input_text[i] != COMMAND_PREFIX); i--);
  maybe_write_itext (input_text, i);
}

/* Similar to execute_string (), but only takes a single string argument,
   and remembers the input text location, etc. */
void
me_execute_string (execution_string)
     char *execution_string;
{
  pushfile ();
  input_text_offset = 0;
  input_text = execution_string;
  input_filename = xstrdup (input_filename);
  size_of_input_text = strlen (execution_string);

  remember_itext (execution_string, 0);

  me_executing_string++;
  reader_loop ();
  popfile ();
  me_executing_string--;
}

/* Append the text which appears in input_text from the last offset to
   the current OFFSET. */
void
append_to_expansion_output (offset)
     int offset;
{
  register int i;
  ITEXT *itext = (ITEXT *)NULL;

  for (i = 0; i < itext_size; i++)
    if (itext_info[i] && itext_info[i]->pointer == input_text)
      {
        itext = itext_info[i];
        break;
      }

  if (!itext)
    return;

  if (offset > itext->offset)
    {
      write_region_to_macro_output
        (input_text, itext->offset, offset);
      remember_itext (input_text, offset);
    }
}

/* Only write this input text iff it appears in our itext list. */
void
maybe_write_itext (pointer, offset)
     char *pointer;
     int offset;
{
  register int i;
  ITEXT *itext = (ITEXT *)NULL;

  for (i = 0; i < itext_size; i++)
    if (itext_info[i] && (itext_info[i]->pointer == pointer))
      {
        itext = itext_info[i];
        break;
      }

  if (itext && (itext->offset < offset))
    {
      write_region_to_macro_output (itext->pointer, itext->offset, offset);
      remember_itext (pointer, offset);
    }
}

void
write_region_to_macro_output (string, start, end)
     char *string;
     int start, end;
{
  if (macro_expansion_output_stream)
    fwrite (string + start, 1, end - start, macro_expansion_output_stream);
}

#endif /* HAVE_MACROS */

/* Return the length of the array in ARRAY. */
int
array_len (array)
     char **array;
{
  register int i = 0;

  if (array)
    for (i = 0; array[i] != (char *)NULL; i++);

  return (i);
}

void
free_array (array)
     char **array;
{
  if (array)
    {
      register int i;

      for (i = 0; array[i] != (char *)NULL; i++)
        free (array[i]);

      free (array);
    }
}

/* Function is used even when we don't have macros.  Although, I have
   to admit, it is unlikely that you would have a use for it if you
   aren't using macros. */
char **
get_brace_args (quote_single)
     int quote_single;
{
  char **arglist, *word;
  int arglist_index, arglist_size;
  int character, escape_seen, start;
  int depth = 1;

  /* There is an arglist in braces here, so gather the args inside of it. */
  skip_whitespace_and_newlines ();
  input_text_offset++;
  arglist = (char **)NULL;
  arglist_index = arglist_size = 0;

 get_arg:
  skip_whitespace_and_newlines ();
  start = input_text_offset;
  escape_seen = 0;

  while ((character = curchar ()))
    {
      if (character == '\\')
        {
          input_text_offset += 2;
          escape_seen = 1;
        }
      else if (character == '{')
        {
          depth++;
          input_text_offset++;
        }
      else if ((character == ',' && !quote_single) ||
               ((character == '}') && depth == 1))
        {
          int len = input_text_offset - start;

          if (len || (character != '}'))
            {
              word = (char *)xmalloc (1 + len);
              strncpy (word, input_text + start, len);
              word[len] = 0;

              /* Clean up escaped characters. */
              if (escape_seen)
                {
                  register int i;

                  for (i = 0; word[i]; i++)
                    if (word[i] == '\\')
                      memmove (word + i, word + i + 1,
                               1 + strlen (word + i + 1));
                }

              if (arglist_index + 2 >= arglist_size)
                arglist = (char **)xrealloc
                  (arglist, (arglist_size += 10) * sizeof (char *));

              arglist[arglist_index++] = word;
              arglist[arglist_index] = (char *)NULL;
            }

          input_text_offset++;
          if (character == '}')
            break;
          else
            goto get_arg;
        }
      else if (character == '}')
        {
          depth--;
          input_text_offset++;
        }
      else
        {
          input_text_offset++;
          if (character == '\n') line_number++;
        }
    }
  return (arglist);
}

/* **************************************************************** */
/*                                                                  */
/*                  Looking For Include Files                       */
/*                                                                  */
/* **************************************************************** */

/* Given a string containing units of information separated by colons,
   return the next one pointed to by INDEX, or NULL if there are no more.
   Advance INDEX to the character after the colon. */
char *
extract_colon_unit (string, index)
     char *string;
     int *index;
{
  int i, start;

  i = *index;

  if (!string || (i >= strlen (string)))
    return ((char *)NULL);

  /* Each call to this routine leaves the index pointing at a colon if
     there is more to the path.  If I is > 0, then increment past the
     `:'.  If I is 0, then the path has a leading colon.  Trailing colons
     are handled OK by the `else' part of the if statement; an empty
     string is returned in that case. */
  if (i && string[i] == ':')
    i++;

  start = i;

  while (string[i] && string[i] != ':') i++;

  *index = i;

  if (i == start)
    {
      if (string[i])
        (*index)++;

      /* Return "" in the case of a trailing `:'. */
      return (xstrdup (""));
    }
  else
    {
      char *value;

      value = (char *)xmalloc (1 + (i - start));
      strncpy (value, &string[start], (i - start));
      value [i - start] = 0;

      return (value);
    }
}

/* Return the full pathname for FILENAME by searching along PATH.
   When found, return the stat () info for FILENAME in FINFO.
   If PATH is NULL, only the current directory is searched.
   If the file could not be found, return a NULL pointer. */
char *
get_file_info_in_path (filename, path, finfo)
     char *filename, *path;
     struct stat *finfo;
{
  char *dir;
  int result, index = 0;

  if (path == (char *)NULL)
    path = ".";

  /* Handle absolute pathnames. "./foo", "/foo", "../foo". */
  if (*filename == '/' ||
      (*filename == '.' &&
       (filename[1] == '/' ||
        (filename[1] == '.' && filename[2] == '/')))
#ifdef WIN32
      /* Handle names that look like "d:/foo/bar" */
      || (isalpha (*filename) && filename [1] == ':' 
          && (filename [2] == '/' || filename [2] == '\\'))
#endif
     )
    {
      if (stat (filename, finfo) == 0)
        return (xstrdup (filename));
      else
        return ((char *)NULL);
    }

  while ((dir = extract_colon_unit (path, &index)))
    {
      char *fullpath;

      if (!*dir)
        {
          free (dir);
          dir = xstrdup (".");
        }

      fullpath = (char *)xmalloc (2 + strlen (dir) + strlen (filename));
      sprintf (fullpath, "%s/%s", dir, filename);
      free (dir);

      result = stat (fullpath, finfo);

      if (result == 0)
        return (fullpath);
      else
        free (fullpath);
    }
  return NULL;
}
