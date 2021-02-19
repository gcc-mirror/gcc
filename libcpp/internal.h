/* Part of CPP library.
   Copyright (C) 1997-2021 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This header defines all the internal data structures and functions
   that need to be visible across files.  It should not be used outside
   cpplib.  */

#ifndef LIBCPP_INTERNAL_H
#define LIBCPP_INTERNAL_H

#include "symtab.h"
#include "cpplib.h"

#if HAVE_ICONV
#include <iconv.h>
#else
#define HAVE_ICONV 0
typedef int iconv_t;  /* dummy */
#endif

#ifdef __cplusplus
extern "C" {
#endif

struct directive;		/* Deliberately incomplete.  */
struct pending_option;
struct op;
struct _cpp_strbuf;

typedef bool (*convert_f) (iconv_t, const unsigned char *, size_t,
			   struct _cpp_strbuf *);
struct cset_converter
{
  convert_f func;
  iconv_t cd;
  int width;
  const char* from;
  const char* to;
};

#define BITS_PER_CPPCHAR_T (CHAR_BIT * sizeof (cppchar_t))

/* Test if a sign is valid within a preprocessing number.  */
#define VALID_SIGN(c, prevc) \
  (((c) == '+' || (c) == '-') && \
   ((prevc) == 'e' || (prevc) == 'E' \
    || (((prevc) == 'p' || (prevc) == 'P') \
        && CPP_OPTION (pfile, extended_numbers))))

#define DIGIT_SEP(c) ((c) == '\'' && CPP_OPTION (pfile, digit_separators))

#define CPP_OPTION(PFILE, OPTION) ((PFILE)->opts.OPTION)
#define CPP_BUFFER(PFILE) ((PFILE)->buffer)
#define CPP_BUF_COLUMN(BUF, CUR) ((CUR) - (BUF)->line_base)
#define CPP_BUF_COL(BUF) CPP_BUF_COLUMN(BUF, (BUF)->cur)

#define CPP_INCREMENT_LINE(PFILE, COLS_HINT) do { \
    const class line_maps *line_table = PFILE->line_table; \
    const struct line_map_ordinary *map = \
      LINEMAPS_LAST_ORDINARY_MAP (line_table); \
    linenum_type line = SOURCE_LINE (map, line_table->highest_line); \
    linemap_line_start (PFILE->line_table, line + 1, COLS_HINT); \
  } while (0)

/* Host alignment handling.  */
struct dummy
{
  char c;
  union
  {
    double d;
    int *p;
  } u;
};

#define DEFAULT_ALIGNMENT offsetof (struct dummy, u)
#define CPP_ALIGN2(size, align) (((size) + ((align) - 1)) & ~((align) - 1))
#define CPP_ALIGN(size) CPP_ALIGN2 (size, DEFAULT_ALIGNMENT)

#define _cpp_mark_macro_used(NODE) 					\
  (cpp_user_macro_p (NODE) ? (NODE)->value.macro->used = 1 : 0)

/* A generic memory buffer, and operations on it.  */
typedef struct _cpp_buff _cpp_buff;
struct _cpp_buff
{
  struct _cpp_buff *next;
  unsigned char *base, *cur, *limit;
};

extern _cpp_buff *_cpp_get_buff (cpp_reader *, size_t);
extern void _cpp_release_buff (cpp_reader *, _cpp_buff *);
extern void _cpp_extend_buff (cpp_reader *, _cpp_buff **, size_t);
extern _cpp_buff *_cpp_append_extend_buff (cpp_reader *, _cpp_buff *, size_t);
extern void _cpp_free_buff (_cpp_buff *);
extern unsigned char *_cpp_aligned_alloc (cpp_reader *, size_t);
extern unsigned char *_cpp_unaligned_alloc (cpp_reader *, size_t);

#define BUFF_ROOM(BUFF) (size_t) ((BUFF)->limit - (BUFF)->cur)
#define BUFF_FRONT(BUFF) ((BUFF)->cur)
#define BUFF_LIMIT(BUFF) ((BUFF)->limit)

/* #include types.  */
enum include_type
  {
   /* Directive-based including mechanisms.  */
   IT_INCLUDE,  /* #include */
   IT_INCLUDE_NEXT,  /* #include_next */
   IT_IMPORT,   /* #import  */

   /* Non-directive including mechanisms.  */
   IT_CMDLINE,  /* -include */
   IT_DEFAULT,  /* forced header  */
   IT_MAIN,     /* main, start on line 1 */
   IT_PRE_MAIN,  /* main, but there will be a preamble before line
		    1 */

   IT_DIRECTIVE_HWM = IT_IMPORT + 1,  /* Directives below this.  */
   IT_HEADER_HWM = IT_DEFAULT + 1     /* Header files below this.  */
  };

union utoken
{
  const cpp_token *token;
  const cpp_token **ptoken;
};

/* A "run" of tokens; part of a chain of runs.  */
typedef struct tokenrun tokenrun;
struct tokenrun
{
  tokenrun *next, *prev;
  cpp_token *base, *limit;
};

/* Accessor macros for struct cpp_context.  */
#define FIRST(c) ((c)->u.iso.first)
#define LAST(c) ((c)->u.iso.last)
#define CUR(c) ((c)->u.trad.cur)
#define RLIMIT(c) ((c)->u.trad.rlimit)

/* This describes some additional data that is added to the macro
   token context of type cpp_context, when -ftrack-macro-expansion is
   on.  */
typedef struct
{
  /* The node of the macro we are referring to.  */
  cpp_hashnode *macro_node;
  /* This buffer contains an array of virtual locations.  The virtual
     location at index 0 is the virtual location of the token at index
     0 in the current instance of cpp_context; similarly for all the
     other virtual locations.  */
  location_t *virt_locs;
  /* This is a pointer to the current virtual location.  This is used
     to iterate over the virtual locations while we iterate over the
     tokens they belong to.  */
  location_t *cur_virt_loc;
} macro_context;

/* The kind of tokens carried by a cpp_context.  */
enum context_tokens_kind {
  /* This is the value of cpp_context::tokens_kind if u.iso.first
     contains an instance of cpp_token **.  */
  TOKENS_KIND_INDIRECT,
  /* This is the value of cpp_context::tokens_kind if u.iso.first
     contains an instance of cpp_token *.  */
  TOKENS_KIND_DIRECT,
  /* This is the value of cpp_context::tokens_kind when the token
     context contains tokens resulting from macro expansion.  In that
     case struct cpp_context::macro points to an instance of struct
     macro_context.  This is used only when the
     -ftrack-macro-expansion flag is on.  */
  TOKENS_KIND_EXTENDED
};

typedef struct cpp_context cpp_context;
struct cpp_context
{
  /* Doubly-linked list.  */
  cpp_context *next, *prev;

  union
  {
    /* For ISO macro expansion.  Contexts other than the base context
       are contiguous tokens.  e.g. macro expansions, expanded
       argument tokens.  */
    struct
    {
      union utoken first;
      union utoken last;
    } iso;

    /* For traditional macro expansion.  */
    struct
    {
      const unsigned char *cur;
      const unsigned char *rlimit;
    } trad;
  } u;

  /* If non-NULL, a buffer used for storage related to this context.
     When the context is popped, the buffer is released.  */
  _cpp_buff *buff;

  /* If tokens_kind is TOKEN_KIND_EXTENDED, then (as we thus are in a
     macro context) this is a pointer to an instance of macro_context.
     Otherwise if tokens_kind is *not* TOKEN_KIND_EXTENDED, then, if
     we are in a macro context, this is a pointer to an instance of
     cpp_hashnode, representing the name of the macro this context is
     for.  If we are not in a macro context, then this is just NULL.
     Note that when tokens_kind is TOKEN_KIND_EXTENDED, the memory
     used by the instance of macro_context pointed to by this member
     is de-allocated upon de-allocation of the instance of struct
     cpp_context.  */
  union
  {
    macro_context *mc;
    cpp_hashnode *macro;
  } c;

  /* This determines the type of tokens held by this context.  */
  enum context_tokens_kind tokens_kind;
};

struct lexer_state
{
  /* 1 if we're handling a directive.  2 if it's an include-like
     directive.  */
  unsigned char in_directive;

  /* Nonzero if in a directive that will handle padding tokens itself.
     #include needs this to avoid problems with computed include and
     spacing between tokens.  */
  unsigned char directive_wants_padding;

  /* True if we are skipping a failed conditional group.  */
  unsigned char skipping;

  /* Nonzero if in a directive that takes angle-bracketed headers.  */
  unsigned char angled_headers;

  /* Nonzero if in a #if or #elif directive.  */
  unsigned char in_expression;

  /* Nonzero to save comments.  Turned off if discard_comments, and in
     all directives apart from #define.  */
  unsigned char save_comments;

  /* Nonzero if lexing __VA_ARGS__ and __VA_OPT__ are valid.  */
  unsigned char va_args_ok;

  /* Nonzero if lexing poisoned identifiers is valid.  */
  unsigned char poisoned_ok;

  /* Nonzero to prevent macro expansion.  */
  unsigned char prevent_expansion;

  /* Nonzero when parsing arguments to a function-like macro.  */
  unsigned char parsing_args;

  /* Nonzero if prevent_expansion is true only because output is
     being discarded.  */
  unsigned char discarding_output;

  /* Nonzero to skip evaluating part of an expression.  */
  unsigned int skip_eval;

  /* Nonzero when tokenizing a deferred pragma.  */
  unsigned char in_deferred_pragma;

  /* Count to token that is a header-name.  */
  unsigned char directive_file_token;

  /* Nonzero if the deferred pragma being handled allows macro expansion.  */
  unsigned char pragma_allow_expansion;
};

/* Special nodes - identifiers with predefined significance.  */
struct spec_nodes
{
  cpp_hashnode *n_defined;		/* defined operator */
  cpp_hashnode *n_true;			/* C++ keyword true */
  cpp_hashnode *n_false;		/* C++ keyword false */
  cpp_hashnode *n__VA_ARGS__;		/* C99 vararg macros */
  cpp_hashnode *n__VA_OPT__;		/* C++ vararg macros */

  enum {M_EXPORT, M_MODULE, M_IMPORT, M__IMPORT, M_HWM};
  
  /* C++20 modules, only set when module_directives is in effect.
     incoming variants [0], outgoing ones [1] */
  cpp_hashnode *n_modules[M_HWM][2];
};

typedef struct _cpp_line_note _cpp_line_note;
struct _cpp_line_note
{
  /* Location in the clean line the note refers to.  */
  const unsigned char *pos;

  /* Type of note.  The 9 'from' trigraph characters represent those
     trigraphs, '\\' an escaped newline, ' ' an escaped newline with
     intervening space, 0 represents a note that has already been handled,
     and anything else is invalid.  */
  unsigned int type;
};

/* Represents the contents of a file cpplib has read in.  */
struct cpp_buffer
{
  const unsigned char *cur;        /* Current location.  */
  const unsigned char *line_base;  /* Start of current physical line.  */
  const unsigned char *next_line;  /* Start of to-be-cleaned logical line.  */

  const unsigned char *buf;        /* Entire character buffer.  */
  const unsigned char *rlimit;     /* Writable byte at end of file.  */
  const unsigned char *to_free;	   /* Pointer that should be freed when
				      popping the buffer.  */

  _cpp_line_note *notes;           /* Array of notes.  */
  unsigned int cur_note;           /* Next note to process.  */
  unsigned int notes_used;         /* Number of notes.  */
  unsigned int notes_cap;          /* Size of allocated array.  */

  struct cpp_buffer *prev;

  /* Pointer into the file table; non-NULL if this is a file buffer.
     Used for include_next and to record control macros.  */
  struct _cpp_file *file;

  /* Saved value of __TIMESTAMP__ macro - date and time of last modification
     of the assotiated file.  */
  const unsigned char *timestamp;

  /* Value of if_stack at start of this file.
     Used to prohibit unmatched #endif (etc) in an include file.  */
  struct if_stack *if_stack;

  /* True if we need to get the next clean line.  */
  bool need_line : 1;

  /* True if we have already warned about C++ comments in this file.
     The warning happens only for C89 extended mode with -pedantic on,
     or for -Wtraditional, and only once per file (otherwise it would
     be far too noisy).  */
  bool warned_cplusplus_comments : 1;

  /* True if we don't process trigraphs and escaped newlines.  True
     for preprocessed input, command line directives, and _Pragma
     buffers.  */
  bool from_stage3 : 1;

  /* At EOF, a buffer is automatically popped.  If RETURN_AT_EOF is
     true, a CPP_EOF token is then returned.  Otherwise, the next
     token from the enclosing buffer is returned.  */
  bool return_at_eof : 1;

  /* One for a system header, two for a C system header file that therefore
     needs to be extern "C" protected in C++, and zero otherwise.  */
  unsigned char sysp;

  /* The directory of the this buffer's file.  Its NAME member is not
     allocated, so we don't need to worry about freeing it.  */
  struct cpp_dir dir;

  /* Descriptor for converting from the input character set to the
     source character set.  */
  struct cset_converter input_cset_desc;
};

/* The list of saved macros by push_macro pragma.  */
struct def_pragma_macro {
  /* Chain element to previous saved macro.  */
  struct def_pragma_macro *next;
  /* Name of the macro.  */
  char *name;
  /* The stored macro content.  */
  unsigned char *definition;

  /* Definition line number.  */
  location_t line;
  /* If macro defined in system header.  */
  unsigned int syshdr   : 1;
  /* Nonzero if it has been expanded or had its existence tested.  */
  unsigned int used     : 1;

  /* Mark if we save an undefined macro.  */
  unsigned int is_undef : 1;
  /* Nonzero if it was a builtin macro.  */
  unsigned int is_builtin : 1;
};

/* A cpp_reader encapsulates the "state" of a pre-processor run.
   Applying cpp_get_token repeatedly yields a stream of pre-processor
   tokens.  Usually, there is only one cpp_reader object active.  */
struct cpp_reader
{
  /* Top of buffer stack.  */
  cpp_buffer *buffer;

  /* Overlaid buffer (can be different after processing #include).  */
  cpp_buffer *overlaid_buffer;

  /* Lexer state.  */
  struct lexer_state state;

  /* Source line tracking.  */
  class line_maps *line_table;

  /* The line of the '#' of the current directive.  */
  location_t directive_line;

  /* Memory buffers.  */
  _cpp_buff *a_buff;		/* Aligned permanent storage.  */
  _cpp_buff *u_buff;		/* Unaligned permanent storage.  */
  _cpp_buff *free_buffs;	/* Free buffer chain.  */

  /* Context stack.  */
  struct cpp_context base_context;
  struct cpp_context *context;

  /* If in_directive, the directive if known.  */
  const struct directive *directive;

  /* Token generated while handling a directive, if any. */
  cpp_token directive_result;

  /* When expanding a macro at top-level, this is the location of the
     macro invocation.  */
  location_t invocation_location;

  /* This is the node representing the macro being expanded at
     top-level.  The value of this data member is valid iff
     cpp_in_macro_expansion_p() returns TRUE.  */
  cpp_hashnode *top_most_macro_node;

  /* Nonzero if we are about to expand a macro.  Note that if we are
     really expanding a macro, the function macro_of_context returns
     the macro being expanded and this flag is set to false.  Client
     code should use the function cpp_in_macro_expansion_p to know if we
     are either about to expand a macro, or are actually expanding
     one.  */
  bool about_to_expand_macro_p;

  /* Search paths for include files.  */
  struct cpp_dir *quote_include;	/* "" */
  struct cpp_dir *bracket_include;	/* <> */
  struct cpp_dir no_search_path;	/* No path.  */

  /* Chain of all hashed _cpp_file instances.  */
  struct _cpp_file *all_files;

  struct _cpp_file *main_file;

  /* File and directory hash table.  */
  struct htab *file_hash;
  struct htab *dir_hash;
  struct file_hash_entry_pool *file_hash_entries;

  /* Negative path lookup hash table.  */
  struct htab *nonexistent_file_hash;
  struct obstack nonexistent_file_ob;

  /* Nonzero means don't look for #include "foo" the source-file
     directory.  */
  bool quote_ignores_source_dir;

  /* Nonzero if any file has contained #pragma once or #import has
     been used.  */
  bool seen_once_only;

  /* Multiple include optimization.  */
  const cpp_hashnode *mi_cmacro;
  const cpp_hashnode *mi_ind_cmacro;
  bool mi_valid;

  /* Lexing.  */
  cpp_token *cur_token;
  tokenrun base_run, *cur_run;
  unsigned int lookaheads;

  /* Nonzero prevents the lexer from re-using the token runs.  */
  unsigned int keep_tokens;

  /* Buffer to hold macro definition string.  */
  unsigned char *macro_buffer;
  unsigned int macro_buffer_len;

  /* Descriptor for converting from the source character set to the
     execution character set.  */
  struct cset_converter narrow_cset_desc;

  /* Descriptor for converting from the source character set to the
     UTF-8 execution character set.  */
  struct cset_converter utf8_cset_desc;

  /* Descriptor for converting from the source character set to the
     UTF-16 execution character set.  */
  struct cset_converter char16_cset_desc;

  /* Descriptor for converting from the source character set to the
     UTF-32 execution character set.  */
  struct cset_converter char32_cset_desc;

  /* Descriptor for converting from the source character set to the
     wide execution character set.  */
  struct cset_converter wide_cset_desc;

  /* Date and time text.  Calculated together if either is requested.  */
  const unsigned char *date;
  const unsigned char *time;

  /* Time stamp, set idempotently lazily.  */
  time_t time_stamp;
  int time_stamp_kind; /* Or errno.  */

  /* A token forcing paste avoidance, and one demarking macro arguments.  */
  cpp_token avoid_paste;
  cpp_token endarg;

  /* Opaque handle to the dependencies of mkdeps.c.  */
  class mkdeps *deps;

  /* Obstack holding all macro hash nodes.  This never shrinks.
     See identifiers.c */
  struct obstack hash_ob;

  /* Obstack holding buffer and conditional structures.  This is a
     real stack.  See directives.c.  */
  struct obstack buffer_ob;

  /* Pragma table - dynamic, because a library user can add to the
     list of recognized pragmas.  */
  struct pragma_entry *pragmas;

  /* Call backs to cpplib client.  */
  struct cpp_callbacks cb;

  /* Identifier hash table.  */
  struct ht *hash_table;

  /* Expression parser stack.  */
  struct op *op_stack, *op_limit;

  /* User visible options.  */
  struct cpp_options opts;

  /* Special nodes - identifiers with predefined significance to the
     preprocessor.  */
  struct spec_nodes spec_nodes;

  /* Whether cpplib owns the hashtable.  */
  bool our_hashtable;

  /* Traditional preprocessing output buffer (a logical line).  */
  struct
  {
    unsigned char *base;
    unsigned char *limit;
    unsigned char *cur;
    location_t first_line;
  } out;

  /* Used for buffer overlays by traditional.c.  */
  const unsigned char *saved_cur, *saved_rlimit, *saved_line_base;

  /* A saved list of the defined macros, for dependency checking
     of precompiled headers.  */
  struct cpp_savedstate *savedstate;

  /* Next value of __COUNTER__ macro. */
  unsigned int counter;

  /* Table of comments, when state.save_comments is true.  */
  cpp_comment_table comments;

  /* List of saved macros by push_macro.  */
  struct def_pragma_macro *pushed_macros;

  /* If non-zero, the lexer will use this location for the next token
     instead of getting a location from the linemap.  */
  location_t forced_token_location;

  /* Location identifying the main source file -- intended to be line
     zero of said file.  */
  location_t main_loc;
};

/* Character classes.  Based on the more primitive macros in safe-ctype.h.
   If the definition of `numchar' looks odd to you, please look up the
   definition of a pp-number in the C standard [section 6.4.8 of C99].

   In the unlikely event that characters other than \r and \n enter
   the set is_vspace, the macro handle_newline() in lex.c must be
   updated.  */
#define _dollar_ok(x)	((x) == '$' && CPP_OPTION (pfile, dollars_in_ident))

#define is_idchar(x)	(ISIDNUM(x) || _dollar_ok(x))
#define is_numchar(x)	ISIDNUM(x)
#define is_idstart(x)	(ISIDST(x) || _dollar_ok(x))
#define is_numstart(x)	ISDIGIT(x)
#define is_hspace(x)	ISBLANK(x)
#define is_vspace(x)	IS_VSPACE(x)
#define is_nvspace(x)	IS_NVSPACE(x)
#define is_space(x)	IS_SPACE_OR_NUL(x)

#define SEEN_EOL() (pfile->cur_token[-1].type == CPP_EOF)

/* This table is constant if it can be initialized at compile time,
   which is the case if cpp was compiled with GCC >=2.7, or another
   compiler that supports C99.  */
#if HAVE_DESIGNATED_INITIALIZERS
extern const unsigned char _cpp_trigraph_map[UCHAR_MAX + 1];
#else
extern unsigned char _cpp_trigraph_map[UCHAR_MAX + 1];
#endif

#if !defined (HAVE_UCHAR) && !defined (IN_GCC)
typedef unsigned char uchar;
#endif

#define UC (const uchar *)  /* Intended use: UC"string" */

/* Accessors.  */

inline int
_cpp_in_system_header (cpp_reader *pfile)
{
  return pfile->buffer ? pfile->buffer->sysp : 0;
}
#define CPP_PEDANTIC(PF) CPP_OPTION (PF, cpp_pedantic)
#define CPP_WTRADITIONAL(PF) CPP_OPTION (PF, cpp_warn_traditional)

/* Return true if we're in the main file (unless it's considered to be
   an include file in its own right.  */
inline int
_cpp_in_main_source_file (cpp_reader *pfile)
{
  return (!CPP_OPTION (pfile, main_search)
	  && pfile->buffer->file == pfile->main_file);
}

/* True if NODE is a macro for the purposes of ifdef, defined etc.  */
inline bool _cpp_defined_macro_p (cpp_hashnode *node)
{
  /* Do not treat conditional macros as being defined.  This is due to
     the powerpc port using conditional macros for 'vector', 'bool',
     and 'pixel' to act as conditional keywords.  This messes up tests
     like #ifndef bool.  */
  return cpp_macro_p (node) && !(node->flags & NODE_CONDITIONAL);
}

/* In macro.c */
extern bool _cpp_notify_macro_use (cpp_reader *pfile, cpp_hashnode *node,
				   location_t);
inline bool _cpp_maybe_notify_macro_use (cpp_reader *pfile, cpp_hashnode *node,
					 location_t loc)
{
  if (!(node->flags & NODE_USED))
    return _cpp_notify_macro_use (pfile, node, loc);
  return true;
}
extern cpp_macro *_cpp_new_macro (cpp_reader *, cpp_macro_kind, void *);
extern void _cpp_free_definition (cpp_hashnode *);
extern bool _cpp_create_definition (cpp_reader *, cpp_hashnode *);
extern void _cpp_pop_context (cpp_reader *);
extern void _cpp_push_text_context (cpp_reader *, cpp_hashnode *,
				    const unsigned char *, size_t);
extern bool _cpp_save_parameter (cpp_reader *, unsigned, cpp_hashnode *,
				 cpp_hashnode *);
extern void _cpp_unsave_parameters (cpp_reader *, unsigned);
extern bool _cpp_arguments_ok (cpp_reader *, cpp_macro *, const cpp_hashnode *,
			       unsigned int);
extern const unsigned char *_cpp_builtin_macro_text (cpp_reader *,
						     cpp_hashnode *,
						     location_t = 0);
extern int _cpp_warn_if_unused_macro (cpp_reader *, cpp_hashnode *, void *);
extern void _cpp_push_token_context (cpp_reader *, cpp_hashnode *,
				     const cpp_token *, unsigned int);
extern void _cpp_backup_tokens_direct (cpp_reader *, unsigned int);

/* In identifiers.c */
extern void _cpp_init_hashtable (cpp_reader *, cpp_hash_table *);
extern void _cpp_destroy_hashtable (cpp_reader *);

/* In files.c */
enum _cpp_find_file_kind
  { _cpp_FFK_NORMAL, _cpp_FFK_FAKE, _cpp_FFK_PRE_INCLUDE, _cpp_FFK_HAS_INCLUDE };
extern _cpp_file *_cpp_find_file (cpp_reader *, const char *, cpp_dir *,
				  int angle, _cpp_find_file_kind, location_t);
extern bool _cpp_find_failed (_cpp_file *);
extern void _cpp_mark_file_once_only (cpp_reader *, struct _cpp_file *);
extern const char *_cpp_find_header_unit (cpp_reader *, const char *file,
					  bool angle_p,  location_t);
extern void _cpp_fake_include (cpp_reader *, const char *);
extern bool _cpp_stack_file (cpp_reader *, _cpp_file*, include_type, location_t);
extern bool _cpp_stack_include (cpp_reader *, const char *, int,
				enum include_type, location_t);
extern int _cpp_compare_file_date (cpp_reader *, const char *, int);
extern void _cpp_report_missing_guards (cpp_reader *);
extern void _cpp_init_files (cpp_reader *);
extern void _cpp_cleanup_files (cpp_reader *);
extern void _cpp_pop_file_buffer (cpp_reader *, struct _cpp_file *,
				  const unsigned char *);
extern bool _cpp_save_file_entries (cpp_reader *pfile, FILE *f);
extern bool _cpp_read_file_entries (cpp_reader *, FILE *);
extern const char *_cpp_get_file_name (_cpp_file *);
extern struct stat *_cpp_get_file_stat (_cpp_file *);
extern bool _cpp_has_header (cpp_reader *, const char *, int,
			     enum include_type);

/* In expr.c */
extern bool _cpp_parse_expr (cpp_reader *, bool);
extern struct op *_cpp_expand_op_stack (cpp_reader *);

/* In lex.c */
extern void _cpp_process_line_notes (cpp_reader *, int);
extern void _cpp_clean_line (cpp_reader *);
extern bool _cpp_get_fresh_line (cpp_reader *);
extern bool _cpp_skip_block_comment (cpp_reader *);
extern cpp_token *_cpp_temp_token (cpp_reader *);
extern const cpp_token *_cpp_lex_token (cpp_reader *);
extern cpp_token *_cpp_lex_direct (cpp_reader *);
extern unsigned char *_cpp_spell_ident_ucns (unsigned char *, cpp_hashnode *);
extern int _cpp_equiv_tokens (const cpp_token *, const cpp_token *);
extern void _cpp_init_tokenrun (tokenrun *, unsigned int);
extern cpp_hashnode *_cpp_lex_identifier (cpp_reader *, const char *);
extern int _cpp_remaining_tokens_num_in_context (cpp_context *);
extern void _cpp_init_lexer (void);
static inline void *_cpp_reserve_room (cpp_reader *pfile, size_t have,
				       size_t extra)
{
  if (BUFF_ROOM (pfile->a_buff) < (have + extra))
    _cpp_extend_buff (pfile, &pfile->a_buff, extra);
  return BUFF_FRONT (pfile->a_buff);
}
extern void *_cpp_commit_buff (cpp_reader *pfile, size_t size);

/* In init.c.  */
extern void _cpp_maybe_push_include_file (cpp_reader *);
extern const char *cpp_named_operator2name (enum cpp_ttype type);
extern void _cpp_restore_special_builtin (cpp_reader *pfile,
					  struct def_pragma_macro *);

/* In directives.c */
extern int _cpp_test_assertion (cpp_reader *, unsigned int *);
extern int _cpp_handle_directive (cpp_reader *, bool);
extern void _cpp_define_builtin (cpp_reader *, const char *);
extern char ** _cpp_save_pragma_names (cpp_reader *);
extern void _cpp_restore_pragma_names (cpp_reader *, char **);
extern int _cpp_do__Pragma (cpp_reader *, location_t);
extern void _cpp_init_directives (cpp_reader *);
extern void _cpp_init_internal_pragmas (cpp_reader *);
extern void _cpp_do_file_change (cpp_reader *, enum lc_reason, const char *,
				 linenum_type, unsigned int);
extern void _cpp_pop_buffer (cpp_reader *);
extern char *_cpp_bracket_include (cpp_reader *);

/* In traditional.c.  */
extern bool _cpp_scan_out_logical_line (cpp_reader *, cpp_macro *, bool);
extern bool _cpp_read_logical_line_trad (cpp_reader *);
extern void _cpp_overlay_buffer (cpp_reader *pfile, const unsigned char *,
				 size_t);
extern void _cpp_remove_overlay (cpp_reader *);
extern cpp_macro *_cpp_create_trad_definition (cpp_reader *);
extern bool _cpp_expansions_different_trad (const cpp_macro *,
					    const cpp_macro *);
extern unsigned char *_cpp_copy_replacement_text (const cpp_macro *,
						  unsigned char *);
extern size_t _cpp_replacement_text_len (const cpp_macro *);

/* In charset.c.  */

/* The normalization state at this point in the sequence.
   It starts initialized to all zeros, and at the end
   'level' is the normalization level of the sequence.  */

struct normalize_state 
{
  /* The previous starter character.  */
  cppchar_t previous;
  /* The combining class of the previous character (whether or not a
     starter).  */
  unsigned char prev_class;
  /* The lowest normalization level so far.  */
  enum cpp_normalize_level level;
};
#define INITIAL_NORMALIZE_STATE { 0, 0, normalized_KC }
#define NORMALIZE_STATE_RESULT(st) ((st)->level)

/* We saw a character C that matches ISIDNUM(), update a
   normalize_state appropriately.  */
#define NORMALIZE_STATE_UPDATE_IDNUM(st, c)	\
  ((st)->previous = (c), (st)->prev_class = 0)

extern bool _cpp_valid_ucn (cpp_reader *, const unsigned char **,
			    const unsigned char *, int,
			    struct normalize_state *state,
			    cppchar_t *,
			    source_range *char_range,
			    cpp_string_location_reader *loc_reader);

extern bool _cpp_valid_utf8 (cpp_reader *pfile,
			     const uchar **pstr,
			     const uchar *limit,
			     int identifier_pos,
			     struct normalize_state *nst,
			     cppchar_t *cp);

extern void _cpp_destroy_iconv (cpp_reader *);
extern unsigned char *_cpp_convert_input (cpp_reader *, const char *,
					  unsigned char *, size_t, size_t,
					  const unsigned char **, off_t *);
extern const char *_cpp_default_encoding (void);
extern cpp_hashnode * _cpp_interpret_identifier (cpp_reader *pfile,
						 const unsigned char *id,
						 size_t len);

/* Utility routines and macros.  */
#define DSC(str) (const unsigned char *)str, sizeof str - 1

/* These are inline functions instead of macros so we can get type
   checking.  */
static inline int ustrcmp (const unsigned char *, const unsigned char *);
static inline int ustrncmp (const unsigned char *, const unsigned char *,
			    size_t);
static inline size_t ustrlen (const unsigned char *);
static inline const unsigned char *uxstrdup (const unsigned char *);
static inline const unsigned char *ustrchr (const unsigned char *, int);
static inline int ufputs (const unsigned char *, FILE *);

/* Use a const char for the second parameter since it is usually a literal.  */
static inline int ustrcspn (const unsigned char *, const char *);

static inline int
ustrcmp (const unsigned char *s1, const unsigned char *s2)
{
  return strcmp ((const char *)s1, (const char *)s2);
}

static inline int
ustrncmp (const unsigned char *s1, const unsigned char *s2, size_t n)
{
  return strncmp ((const char *)s1, (const char *)s2, n);
}

static inline int
ustrcspn (const unsigned char *s1, const char *s2)
{
  return strcspn ((const char *)s1, s2);
}

static inline size_t
ustrlen (const unsigned char *s1)
{
  return strlen ((const char *)s1);
}

static inline const unsigned char *
uxstrdup (const unsigned char *s1)
{
  return (const unsigned char *) xstrdup ((const char *)s1);
}

static inline const unsigned char *
ustrchr (const unsigned char *s1, int c)
{
  return (const unsigned char *) strchr ((const char *)s1, c);
}

static inline int
ufputs (const unsigned char *s, FILE *f)
{
  return fputs ((const char *)s, f);
}

/* In line-map.c.  */

/* Create and return a virtual location for a token that is part of a
   macro expansion-list at a macro expansion point.  See the comment
   inside struct line_map_macro to see what an expansion-list exactly
   is.

   A call to this function must come after a call to
   linemap_enter_macro.

   MAP is the map into which the source location is created.  TOKEN_NO
   is the index of the token in the macro replacement-list, starting
   at number 0.

   ORIG_LOC is the location of the token outside of this macro
   expansion.  If the token comes originally from the macro
   definition, it is the locus in the macro definition; otherwise it
   is a location in the context of the caller of this macro expansion
   (which is a virtual location or a source location if the caller is
   itself a macro expansion or not).

   MACRO_DEFINITION_LOC is the location in the macro definition,
   either of the token itself or of a macro parameter that it
   replaces.  */
location_t linemap_add_macro_token (const line_map_macro *,
				    unsigned int,
				    location_t,
				    location_t);

/* Return the source line number corresponding to source location
   LOCATION.  SET is the line map set LOCATION comes from.  If
   LOCATION is the location of token that is part of the
   expansion-list of a macro expansion return the line number of the
   macro expansion point.  */
int linemap_get_expansion_line (class line_maps *,
				location_t);

/* Return the path of the file corresponding to source code location
   LOCATION.

   If LOCATION is the location of a token that is part of the
   replacement-list of a macro expansion return the file path of the
   macro expansion point.

   SET is the line map set LOCATION comes from.  */
const char* linemap_get_expansion_filename (class line_maps *,
					    location_t);

#ifdef __cplusplus
}
#endif

#endif /* ! LIBCPP_INTERNAL_H */
