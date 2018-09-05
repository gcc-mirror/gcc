/* CPP Library. (Directive handling.)
   Copyright (C) 1986-2018 Free Software Foundation, Inc.
   Contributed by Per Bothner, 1994-95.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

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

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "internal.h"
#include "mkdeps.h"
#include "obstack.h"

/* Stack of conditionals currently in progress
   (including both successful and failing conditionals).  */
struct if_stack
{
  struct if_stack *next;
  source_location line;		/* Line where condition started.  */
  const cpp_hashnode *mi_cmacro;/* macro name for #ifndef around entire file */
  bool skip_elses;		/* Can future #else / #elif be skipped?  */
  bool was_skipping;		/* If were skipping on entry.  */
  int type;			/* Most recent conditional for diagnostics.  */
};

/* Contains a registered pragma or pragma namespace.  */
typedef void (*pragma_cb) (cpp_reader *);
struct pragma_entry
{
  struct pragma_entry *next;
  const cpp_hashnode *pragma;	/* Name and length.  */
  bool is_nspace;
  bool is_internal;
  bool is_deferred;
  bool allow_expansion;
  union {
    pragma_cb handler;
    struct pragma_entry *space;
    unsigned int ident;
  } u;
};

/* Values for the origin field of struct directive.  KANDR directives
   come from traditional (K&R) C.  STDC89 directives come from the
   1989 C standard.  EXTENSION directives are extensions.  */
#define KANDR		0
#define STDC89		1
#define EXTENSION	2

/* Values for the flags field of struct directive.  COND indicates a
   conditional; IF_COND an opening conditional.  INCL means to treat
   "..." and <...> as q-char and h-char sequences respectively.  IN_I
   means this directive should be handled even if -fpreprocessed is in
   effect (these are the directives with callback hooks).

   EXPAND is set on directives that are always macro-expanded.  */
#define COND		(1 << 0)
#define IF_COND		(1 << 1)
#define INCL		(1 << 2)
#define IN_I		(1 << 3)
#define EXPAND		(1 << 4)
#define DEPRECATED	(1 << 5)

/* Defines one #-directive, including how to handle it.  */
typedef void (*directive_handler) (cpp_reader *);
typedef struct directive directive;
struct directive
{
  directive_handler handler;	/* Function to handle directive.  */
  const uchar *name;		/* Name of directive.  */
  unsigned short length;	/* Length of name.  */
  unsigned char origin;		/* Origin of directive.  */
  unsigned char flags;	        /* Flags describing this directive.  */
};

/* Forward declarations.  */

static void skip_rest_of_line (cpp_reader *);
static void check_eol (cpp_reader *, bool);
static void start_directive (cpp_reader *);
static void prepare_directive_trad (cpp_reader *);
static void end_directive (cpp_reader *, int);
static void directive_diagnostics (cpp_reader *, const directive *, int);
static void run_directive (cpp_reader *, int, const char *, size_t);
static char *glue_header_name (cpp_reader *);
static const char *parse_include (cpp_reader *, int *, const cpp_token ***,
				  source_location *);
static void push_conditional (cpp_reader *, int, int, const cpp_hashnode *);
static unsigned int read_flag (cpp_reader *, unsigned int);
static bool strtolinenum (const uchar *, size_t, linenum_type *, bool *);
static void do_diagnostic (cpp_reader *, int, int, int);
static cpp_hashnode *lex_macro_node (cpp_reader *, bool);
static int undefine_macros (cpp_reader *, cpp_hashnode *, void *);
static void do_include_common (cpp_reader *, enum include_type);
static struct pragma_entry *lookup_pragma_entry (struct pragma_entry *,
                                                 const cpp_hashnode *);
static int count_registered_pragmas (struct pragma_entry *);
static char ** save_registered_pragmas (struct pragma_entry *, char **);
static char ** restore_registered_pragmas (cpp_reader *, struct pragma_entry *,
                                           char **);
static void do_pragma_once (cpp_reader *);
static void do_pragma_poison (cpp_reader *);
static void do_pragma_system_header (cpp_reader *);
static void do_pragma_dependency (cpp_reader *);
static void do_pragma_warning_or_error (cpp_reader *, bool error);
static void do_pragma_warning (cpp_reader *);
static void do_pragma_error (cpp_reader *);
static void do_linemarker (cpp_reader *);
static const cpp_token *get_token_no_padding (cpp_reader *);
static const cpp_token *get__Pragma_string (cpp_reader *);
static void destringize_and_run (cpp_reader *, const cpp_string *,
				 source_location);
static bool parse_answer (cpp_reader *, int, source_location, cpp_macro **);
static cpp_hashnode *parse_assertion (cpp_reader *, int, cpp_macro **);
static cpp_macro **find_answer (cpp_hashnode *, const cpp_macro *);
static void handle_assertion (cpp_reader *, const char *, int);
static void do_pragma_push_macro (cpp_reader *);
static void do_pragma_pop_macro (cpp_reader *);
static void cpp_pop_definition (cpp_reader *, struct def_pragma_macro *);

/* This is the table of directive handlers.  It is ordered by
   frequency of occurrence; the numbers at the end are directive
   counts from all the source code I have lying around (egcs and libc
   CVS as of 1999-05-18, plus grub-0.5.91, linux-2.2.9, and
   pcmcia-cs-3.0.9).  This is no longer important as directive lookup
   is now O(1).  All extensions other than #warning, #include_next,
   and #import are deprecated.  The name is where the extension
   appears to have come from.  */

#define DIRECTIVE_TABLE							\
D(define,	T_DEFINE = 0,	KANDR,     IN_I)	   /* 270554 */ \
D(include,	T_INCLUDE,	KANDR,     INCL | EXPAND)  /*  52262 */ \
D(endif,	T_ENDIF,	KANDR,     COND)	   /*  45855 */ \
D(ifdef,	T_IFDEF,	KANDR,     COND | IF_COND) /*  22000 */ \
D(if,		T_IF,		KANDR, COND | IF_COND | EXPAND) /*  18162 */ \
D(else,		T_ELSE,		KANDR,     COND)	   /*   9863 */ \
D(ifndef,	T_IFNDEF,	KANDR,     COND | IF_COND) /*   9675 */ \
D(undef,	T_UNDEF,	KANDR,     IN_I)	   /*   4837 */ \
D(line,		T_LINE,		KANDR,     EXPAND)	   /*   2465 */ \
D(elif,		T_ELIF,		STDC89,    COND | EXPAND)  /*    610 */ \
D(error,	T_ERROR,	STDC89,    0)		   /*    475 */ \
D(pragma,	T_PRAGMA,	STDC89,    IN_I)	   /*    195 */ \
D(warning,	T_WARNING,	EXTENSION, 0)		   /*     22 */ \
D(include_next,	T_INCLUDE_NEXT,	EXTENSION, INCL | EXPAND)  /*     19 */ \
D(ident,	T_IDENT,	EXTENSION, IN_I)           /*     11 */ \
D(import,	T_IMPORT,	EXTENSION, INCL | EXPAND)  /* 0 ObjC */	\
D(assert,	T_ASSERT,	EXTENSION, DEPRECATED)	   /* 0 SVR4 */	\
D(unassert,	T_UNASSERT,	EXTENSION, DEPRECATED)	   /* 0 SVR4 */	\
D(sccs,		T_SCCS,		EXTENSION, IN_I)           /* 0 SVR4? */

/* #sccs is synonymous with #ident.  */
#define do_sccs do_ident

/* Use the table to generate a series of prototypes, an enum for the
   directive names, and an array of directive handlers.  */

#define D(name, t, o, f) static void do_##name (cpp_reader *);
DIRECTIVE_TABLE
#undef D

#define D(n, tag, o, f) tag,
enum
{
  DIRECTIVE_TABLE
  N_DIRECTIVES
};
#undef D

#define D(name, t, origin, flags) \
{ do_##name, (const uchar *) #name, \
  sizeof #name - 1, origin, flags },
static const directive dtable[] =
{
DIRECTIVE_TABLE
};
#undef D

/* A NULL-terminated array of directive names for use
   when suggesting corrections for misspelled directives.  */
#define D(name, t, origin, flags) #name,
static const char * const directive_names[] = {
DIRECTIVE_TABLE
  NULL
};
#undef D

#undef DIRECTIVE_TABLE

/* Wrapper struct directive for linemarkers.
   The origin is more or less true - the original K+R cpp
   did use this notation in its preprocessed output.  */
static const directive linemarker_dir =
{
  do_linemarker, UC"#", 1, KANDR, IN_I
};

#define SEEN_EOL() (pfile->cur_token[-1].type == CPP_EOF)

/* Skip any remaining tokens in a directive.  */
static void
skip_rest_of_line (cpp_reader *pfile)
{
  /* Discard all stacked contexts.  */
  while (pfile->context->prev)
    _cpp_pop_context (pfile);

  /* Sweep up all tokens remaining on the line.  */
  if (! SEEN_EOL ())
    while (_cpp_lex_token (pfile)->type != CPP_EOF)
      ;
}

/* Helper function for check_oel.  */

static void
check_eol_1 (cpp_reader *pfile, bool expand, int reason)
{
  if (! SEEN_EOL () && (expand
			? cpp_get_token (pfile)
			: _cpp_lex_token (pfile))->type != CPP_EOF)
    cpp_pedwarning (pfile, reason, "extra tokens at end of #%s directive",
		    pfile->directive->name);
}

/* Variant of check_eol used for Wendif-labels warnings.  */

static void
check_eol_endif_labels (cpp_reader *pfile)
{
  check_eol_1 (pfile, false, CPP_W_ENDIF_LABELS);
}

/* Ensure there are no stray tokens at the end of a directive.  If
   EXPAND is true, tokens macro-expanding to nothing are allowed.  */

static void
check_eol (cpp_reader *pfile, bool expand)
{
  check_eol_1 (pfile, expand, CPP_W_NONE);
}

/* Ensure there are no stray tokens other than comments at the end of
   a directive, and gather the comments.  */
static const cpp_token **
check_eol_return_comments (cpp_reader *pfile)
{
  size_t c;
  size_t capacity = 8;
  const cpp_token **buf;

  buf = XNEWVEC (const cpp_token *, capacity);
  c = 0;
  if (! SEEN_EOL ())
    {
      while (1)
	{
	  const cpp_token *tok;

	  tok = _cpp_lex_token (pfile);
	  if (tok->type == CPP_EOF)
	    break;
	  if (tok->type != CPP_COMMENT)
	    cpp_error (pfile, CPP_DL_PEDWARN,
		       "extra tokens at end of #%s directive",
		       pfile->directive->name);
	  else
	    {
	      if (c + 1 >= capacity)
		{
		  capacity *= 2;
		  buf = XRESIZEVEC (const cpp_token *, buf, capacity);
		}
	      buf[c] = tok;
	      ++c;
	    }
	}
    }
  buf[c] = NULL;
  return buf;
}

/* Called when entering a directive, _Pragma or command-line directive.  */
static void
start_directive (cpp_reader *pfile)
{
  /* Setup in-directive state.  */
  pfile->state.in_directive = 1;
  pfile->state.save_comments = 0;
  pfile->directive_result.type = CPP_PADDING;

  /* Some handlers need the position of the # for diagnostics.  */
  pfile->directive_line = pfile->line_table->highest_line;
}

/* Called when leaving a directive, _Pragma or command-line directive.  */
static void
end_directive (cpp_reader *pfile, int skip_line)
{
  if (CPP_OPTION (pfile, traditional))
    {
      /* Revert change of prepare_directive_trad.  */
      if (!pfile->state.in_deferred_pragma)
	pfile->state.prevent_expansion--;

      if (pfile->directive != &dtable[T_DEFINE])
	_cpp_remove_overlay (pfile);
    }
  else if (pfile->state.in_deferred_pragma)
    ;
  /* We don't skip for an assembler #.  */
  else if (skip_line)
    {
      skip_rest_of_line (pfile);
      if (!pfile->keep_tokens)
	{
	  pfile->cur_run = &pfile->base_run;
	  pfile->cur_token = pfile->base_run.base;
	}
    }

  /* Restore state.  */
  pfile->state.save_comments = ! CPP_OPTION (pfile, discard_comments);
  pfile->state.in_directive = 0;
  pfile->state.in_expression = 0;
  pfile->state.angled_headers = 0;
  pfile->directive = 0;
}

/* Prepare to handle the directive in pfile->directive.  */
static void
prepare_directive_trad (cpp_reader *pfile)
{
  if (pfile->directive != &dtable[T_DEFINE])
    {
      bool no_expand = (pfile->directive
			&& ! (pfile->directive->flags & EXPAND));
      bool was_skipping = pfile->state.skipping;

      pfile->state.in_expression = (pfile->directive == &dtable[T_IF]
				    || pfile->directive == &dtable[T_ELIF]);
      if (pfile->state.in_expression)
	pfile->state.skipping = false;

      if (no_expand)
	pfile->state.prevent_expansion++;
      _cpp_scan_out_logical_line (pfile, NULL, false);
      if (no_expand)
	pfile->state.prevent_expansion--;

      pfile->state.skipping = was_skipping;
      _cpp_overlay_buffer (pfile, pfile->out.base,
			   pfile->out.cur - pfile->out.base);
    }

  /* Stop ISO C from expanding anything.  */
  pfile->state.prevent_expansion++;
}

/* Output diagnostics for a directive DIR.  INDENTED is nonzero if
   the '#' was indented.  */
static void
directive_diagnostics (cpp_reader *pfile, const directive *dir, int indented)
{
  /* Issue -pedantic or deprecated warnings for extensions.  We let
     -pedantic take precedence if both are applicable.  */
  if (! pfile->state.skipping)
    {
      if (dir->origin == EXTENSION
	  && !(dir == &dtable[T_IMPORT] && CPP_OPTION (pfile, objc))
	  && CPP_PEDANTIC (pfile))
	cpp_error (pfile, CPP_DL_PEDWARN, "#%s is a GCC extension", dir->name);
      else if (((dir->flags & DEPRECATED) != 0
		|| (dir == &dtable[T_IMPORT] && !CPP_OPTION (pfile, objc)))
	       && CPP_OPTION (pfile, cpp_warn_deprecated))
	cpp_warning (pfile, CPP_W_DEPRECATED,
                     "#%s is a deprecated GCC extension", dir->name);
    }

  /* Traditionally, a directive is ignored unless its # is in
     column 1.  Therefore in code intended to work with K+R
     compilers, directives added by C89 must have their #
     indented, and directives present in traditional C must not.
     This is true even of directives in skipped conditional
     blocks.  #elif cannot be used at all.  */
  if (CPP_WTRADITIONAL (pfile))
    {
      if (dir == &dtable[T_ELIF])
	cpp_warning (pfile, CPP_W_TRADITIONAL,
		     "suggest not using #elif in traditional C");
      else if (indented && dir->origin == KANDR)
	cpp_warning (pfile, CPP_W_TRADITIONAL,
		     "traditional C ignores #%s with the # indented",
		     dir->name);
      else if (!indented && dir->origin != KANDR)
	cpp_warning (pfile, CPP_W_TRADITIONAL,
		     "suggest hiding #%s from traditional C with an indented #",
		     dir->name);
    }
}

/* Check if we have a known directive.  INDENTED is nonzero if the
   '#' of the directive was indented.  This function is in this file
   to save unnecessarily exporting dtable etc. to lex.c.  Returns
   nonzero if the line of tokens has been handled, zero if we should
   continue processing the line.  */
int
_cpp_handle_directive (cpp_reader *pfile, int indented)
{
  const directive *dir = 0;
  const cpp_token *dname;
  bool was_parsing_args = pfile->state.parsing_args;
  bool was_discarding_output = pfile->state.discarding_output;
  int skip = 1;

  if (was_discarding_output)
    pfile->state.prevent_expansion = 0;

  if (was_parsing_args)
    {
      if (CPP_OPTION (pfile, cpp_pedantic))
	cpp_error (pfile, CPP_DL_PEDWARN,
	     "embedding a directive within macro arguments is not portable");
      pfile->state.parsing_args = 0;
      pfile->state.prevent_expansion = 0;
    }
  start_directive (pfile);
  dname = _cpp_lex_token (pfile);

  if (dname->type == CPP_NAME)
    {
      if (dname->val.node.node->is_directive)
	dir = &dtable[dname->val.node.node->directive_index];
    }
  /* We do not recognize the # followed by a number extension in
     assembler code.  */
  else if (dname->type == CPP_NUMBER && CPP_OPTION (pfile, lang) != CLK_ASM)
    {
      dir = &linemarker_dir;
      if (CPP_PEDANTIC (pfile) && ! CPP_OPTION (pfile, preprocessed)
	  && ! pfile->state.skipping)
	cpp_error (pfile, CPP_DL_PEDWARN,
		   "style of line directive is a GCC extension");
    }

  if (dir)
    {
      /* If we have a directive that is not an opening conditional,
	 invalidate any control macro.  */
      if (! (dir->flags & IF_COND))
	pfile->mi_valid = false;

      /* Kluge alert.  In order to be sure that code like this

	 #define HASH #
	 HASH define foo bar

	 does not cause '#define foo bar' to get executed when
	 compiled with -save-temps, we recognize directives in
	 -fpreprocessed mode only if the # is in column 1.  macro.c
	 puts a space in front of any '#' at the start of a macro.
	 
	 We exclude the -fdirectives-only case because macro expansion
	 has not been performed yet, and block comments can cause spaces
	 to precede the directive.  */
      if (CPP_OPTION (pfile, preprocessed)
	  && !CPP_OPTION (pfile, directives_only)
	  && (indented || !(dir->flags & IN_I)))
	{
	  skip = 0;
	  dir = 0;
	}
      else
	{
	  /* In failed conditional groups, all non-conditional
	     directives are ignored.  Before doing that, whether
	     skipping or not, we should lex angle-bracketed headers
	     correctly, and maybe output some diagnostics.  */
	  pfile->state.angled_headers = dir->flags & INCL;
	  pfile->state.directive_wants_padding = dir->flags & INCL;
	  if (! CPP_OPTION (pfile, preprocessed))
	    directive_diagnostics (pfile, dir, indented);
	  if (pfile->state.skipping && !(dir->flags & COND))
	    dir = 0;
	}
    }
  else if (dname->type == CPP_EOF)
    ;	/* CPP_EOF is the "null directive".  */
  else
    {
      /* An unknown directive.  Don't complain about it in assembly
	 source: we don't know where the comments are, and # may
	 introduce assembler pseudo-ops.  Don't complain about invalid
	 directives in skipped conditional groups (6.10 p4).  */
      if (CPP_OPTION (pfile, lang) == CLK_ASM)
	skip = 0;
      else if (!pfile->state.skipping)
	{
	  const char *unrecognized
	    = (const char *)cpp_token_as_text (pfile, dname);
	  const char *hint = NULL;

	  /* Call back into gcc to get a spelling suggestion.  Ideally
	     we'd just use best_match from gcc/spellcheck.h (and filter
	     out the uncommon directives), but that requires moving it
	     to a support library.  */
	  if (pfile->cb.get_suggestion)
	    hint = pfile->cb.get_suggestion (pfile, unrecognized,
					     directive_names);

	  if (hint)
	    {
	      rich_location richloc (pfile->line_table, dname->src_loc);
	      source_range misspelled_token_range
		= get_range_from_loc (pfile->line_table, dname->src_loc);
	      richloc.add_fixit_replace (misspelled_token_range, hint);
	      cpp_error_at (pfile, CPP_DL_ERROR, &richloc,
			    "invalid preprocessing directive #%s;"
			    " did you mean #%s?",
			    unrecognized, hint);
	    }
	  else
	    cpp_error (pfile, CPP_DL_ERROR,
		       "invalid preprocessing directive #%s",
		       unrecognized);
	}
    }

  pfile->directive = dir;
  if (CPP_OPTION (pfile, traditional))
    prepare_directive_trad (pfile);

  if (dir)
    pfile->directive->handler (pfile);
  else if (skip == 0)
    _cpp_backup_tokens (pfile, 1);

  end_directive (pfile, skip);
  if (was_parsing_args && !pfile->state.in_deferred_pragma)
    {
      /* Restore state when within macro args.  */
      pfile->state.parsing_args = 2;
      pfile->state.prevent_expansion = 1;
    }
  if (was_discarding_output)
    pfile->state.prevent_expansion = 1;
  return skip;
}

/* Directive handler wrapper used by the command line option
   processor.  BUF is \n terminated.  */
static void
run_directive (cpp_reader *pfile, int dir_no, const char *buf, size_t count)
{
  cpp_push_buffer (pfile, (const uchar *) buf, count,
		   /* from_stage3 */ true);
  start_directive (pfile);

  /* This is a short-term fix to prevent a leading '#' being
     interpreted as a directive.  */
  _cpp_clean_line (pfile);

  pfile->directive = &dtable[dir_no];
  if (CPP_OPTION (pfile, traditional))
    prepare_directive_trad (pfile);
  pfile->directive->handler (pfile);
  end_directive (pfile, 1);
  _cpp_pop_buffer (pfile);
}

/* Checks for validity the macro name in #define, #undef, #ifdef and
   #ifndef directives.  IS_DEF_OR_UNDEF is true if this call is
   processing a #define or #undefine directive, and false
   otherwise.  */
static cpp_hashnode *
lex_macro_node (cpp_reader *pfile, bool is_def_or_undef)
{
  const cpp_token *token = _cpp_lex_token (pfile);

  /* The token immediately after #define must be an identifier.  That
     identifier may not be "defined", per C99 6.10.8p4.
     In C++, it may not be any of the "named operators" either,
     per C++98 [lex.digraph], [lex.key].
     Finally, the identifier may not have been poisoned.  (In that case
     the lexer has issued the error message for us.)  */

  if (token->type == CPP_NAME)
    {
      cpp_hashnode *node = token->val.node.node;

      if (is_def_or_undef && node == pfile->spec_nodes.n_defined)
	cpp_error (pfile, CPP_DL_ERROR,
		   "\"defined\" cannot be used as a macro name");
      else if (is_def_or_undef
	    && (node == pfile->spec_nodes.n__has_include__
	     || node == pfile->spec_nodes.n__has_include_next__))
	cpp_error (pfile, CPP_DL_ERROR,
		   "\"__has_include__\" cannot be used as a macro name");
      else if (! (node->flags & NODE_POISONED))
	return node;
    }
  else if (token->flags & NAMED_OP)
    cpp_error (pfile, CPP_DL_ERROR,
       "\"%s\" cannot be used as a macro name as it is an operator in C++",
	       NODE_NAME (token->val.node.node));
  else if (token->type == CPP_EOF)
    cpp_error (pfile, CPP_DL_ERROR, "no macro name given in #%s directive",
	       pfile->directive->name);
  else
    cpp_error (pfile, CPP_DL_ERROR, "macro names must be identifiers");

  return NULL;
}

/* Process a #define directive.  Most work is done in macro.c.  */
static void
do_define (cpp_reader *pfile)
{
  cpp_hashnode *node = lex_macro_node (pfile, true);

  if (node)
    {
      /* If we have been requested to expand comments into macros,
	 then re-enable saving of comments.  */
      pfile->state.save_comments =
	! CPP_OPTION (pfile, discard_comments_in_macro_exp);

      if (pfile->cb.before_define)
	pfile->cb.before_define (pfile);

      if (_cpp_create_definition (pfile, node))
	if (pfile->cb.define)
	  pfile->cb.define (pfile, pfile->directive_line, node);

      node->flags &= ~NODE_USED;
    }
}

/* Handle #undef.  Mark the identifier NT_VOID in the hash table.  */
static void
do_undef (cpp_reader *pfile)
{
  cpp_hashnode *node = lex_macro_node (pfile, true);

  if (node)
    {
      if (pfile->cb.before_define)
	pfile->cb.before_define (pfile);

      if (pfile->cb.undef)
	pfile->cb.undef (pfile, pfile->directive_line, node);

      /* 6.10.3.5 paragraph 2: [#undef] is ignored if the specified
	 identifier is not currently defined as a macro name.  */
      if (cpp_macro_p (node))
	{
	  if (node->flags & NODE_WARN)
	    cpp_error (pfile, CPP_DL_WARNING,
		       "undefining \"%s\"", NODE_NAME (node));
	  else if (cpp_builtin_macro_p (node)
		   && CPP_OPTION (pfile, warn_builtin_macro_redefined))
	    cpp_warning_with_line (pfile, CPP_W_BUILTIN_MACRO_REDEFINED,
				   pfile->directive_line, 0,
				   "undefining \"%s\"", NODE_NAME (node));

	  if (CPP_OPTION (pfile, warn_unused_macros))
	    _cpp_warn_if_unused_macro (pfile, node, NULL);

	  _cpp_free_definition (node);
	}
    }

  check_eol (pfile, false);
}

/* Undefine a single macro/assertion/whatever.  */

static int
undefine_macros (cpp_reader *pfile ATTRIBUTE_UNUSED, cpp_hashnode *h,
		 void *data_p ATTRIBUTE_UNUSED)
{
  /* Body of _cpp_free_definition inlined here for speed.
     Macros and assertions no longer have anything to free.  */
  h->type = NT_VOID;
  h->value.answers = NULL;
  h->flags &= ~(NODE_POISONED|NODE_DISABLED|NODE_USED);
  return 1;
}

/* Undefine all macros and assertions.  */

void
cpp_undef_all (cpp_reader *pfile)
{
  cpp_forall_identifiers (pfile, undefine_macros, NULL);
}


/* Helper routine used by parse_include.  Reinterpret the current line
   as an h-char-sequence (< ... >); we are looking at the first token
   after the <.  Returns a malloced filename.  */
static char *
glue_header_name (cpp_reader *pfile)
{
  const cpp_token *token;
  char *buffer;
  size_t len, total_len = 0, capacity = 1024;

  /* To avoid lexed tokens overwriting our glued name, we can only
     allocate from the string pool once we've lexed everything.  */
  buffer = XNEWVEC (char, capacity);
  for (;;)
    {
      token = get_token_no_padding (pfile);

      if (token->type == CPP_GREATER)
	break;
      if (token->type == CPP_EOF)
	{
	  cpp_error (pfile, CPP_DL_ERROR, "missing terminating > character");
	  break;
	}

      len = cpp_token_len (token) + 2; /* Leading space, terminating \0.  */
      if (total_len + len > capacity)
	{
	  capacity = (capacity + len) * 2;
	  buffer = XRESIZEVEC (char, buffer, capacity);
	}

      if (token->flags & PREV_WHITE)
	buffer[total_len++] = ' ';

      total_len = (cpp_spell_token (pfile, token, (uchar *) &buffer[total_len],
				    true)
		   - (uchar *) buffer);
    }

  buffer[total_len] = '\0';
  return buffer;
}

/* Returns the file name of #include, #include_next, #import and
   #pragma dependency.  The string is malloced and the caller should
   free it.  Returns NULL on error.  LOCATION is the source location
   of the file name.  */

static const char *
parse_include (cpp_reader *pfile, int *pangle_brackets,
	       const cpp_token ***buf, source_location *location)
{
  char *fname;
  const cpp_token *header;

  /* Allow macro expansion.  */
  header = get_token_no_padding (pfile);
  *location = header->src_loc;
  if ((header->type == CPP_STRING && header->val.str.text[0] != 'R')
      || header->type == CPP_HEADER_NAME)
    {
      fname = XNEWVEC (char, header->val.str.len - 1);
      memcpy (fname, header->val.str.text + 1, header->val.str.len - 2);
      fname[header->val.str.len - 2] = '\0';
      *pangle_brackets = header->type == CPP_HEADER_NAME;
    }
  else if (header->type == CPP_LESS)
    {
      fname = glue_header_name (pfile);
      *pangle_brackets = 1;
    }
  else
    {
      const unsigned char *dir;

      if (pfile->directive == &dtable[T_PRAGMA])
	dir = UC"pragma dependency";
      else
	dir = pfile->directive->name;
      cpp_error (pfile, CPP_DL_ERROR, "#%s expects \"FILENAME\" or <FILENAME>",
		 dir);

      return NULL;
    }

  if (pfile->directive == &dtable[T_PRAGMA])
    {
      /* This pragma allows extra tokens after the file name.  */
    }
  else if (buf == NULL || CPP_OPTION (pfile, discard_comments))
    check_eol (pfile, true);
  else
    {
      /* If we are not discarding comments, then gather them while
	 doing the eol check.  */
      *buf = check_eol_return_comments (pfile);
    }

  return fname;
}

/* Handle #include, #include_next and #import.  */
static void
do_include_common (cpp_reader *pfile, enum include_type type)
{
  const char *fname;
  int angle_brackets;
  const cpp_token **buf = NULL;
  source_location location;

  /* Re-enable saving of comments if requested, so that the include
     callback can dump comments which follow #include.  */
  pfile->state.save_comments = ! CPP_OPTION (pfile, discard_comments);

  fname = parse_include (pfile, &angle_brackets, &buf, &location);
  if (!fname)
    {
      if (buf)
	XDELETEVEC (buf);
      return;
    }

  if (!*fname)
  {
    cpp_error_with_line (pfile, CPP_DL_ERROR, location, 0,
			 "empty filename in #%s",
			 pfile->directive->name);
    XDELETEVEC (fname);
    if (buf)
      XDELETEVEC (buf);
    return;
  }

  /* Prevent #include recursion.  */
  if (pfile->line_table->depth >= CPP_STACK_MAX)
    cpp_error (pfile, CPP_DL_ERROR, "#include nested too deeply");
  else
    {
      /* Get out of macro context, if we are.  */
      skip_rest_of_line (pfile);

      if (pfile->cb.include)
	pfile->cb.include (pfile, pfile->directive_line,
			   pfile->directive->name, fname, angle_brackets,
			   buf);

      _cpp_stack_include (pfile, fname, angle_brackets, type, location);
    }

  XDELETEVEC (fname);
  if (buf)
    XDELETEVEC (buf);
}

static void
do_include (cpp_reader *pfile)
{
  do_include_common (pfile, IT_INCLUDE);
}

static void
do_import (cpp_reader *pfile)
{
  do_include_common (pfile, IT_IMPORT);
}

static void
do_include_next (cpp_reader *pfile)
{
  enum include_type type = IT_INCLUDE_NEXT;

  /* If this is the primary source file, warn and use the normal
     search logic.  */
  if (cpp_in_primary_file (pfile))
    {
      cpp_error (pfile, CPP_DL_WARNING,
		 "#include_next in primary source file");
      type = IT_INCLUDE;
    }
  do_include_common (pfile, type);
}

/* Subroutine of do_linemarker.  Read possible flags after file name.
   LAST is the last flag seen; 0 if this is the first flag. Return the
   flag if it is valid, 0 at the end of the directive. Otherwise
   complain.  */
static unsigned int
read_flag (cpp_reader *pfile, unsigned int last)
{
  const cpp_token *token = _cpp_lex_token (pfile);

  if (token->type == CPP_NUMBER && token->val.str.len == 1)
    {
      unsigned int flag = token->val.str.text[0] - '0';

      if (flag > last && flag <= 4
	  && (flag != 4 || last == 3)
	  && (flag != 2 || last == 0))
	return flag;
    }

  if (token->type != CPP_EOF)
    cpp_error (pfile, CPP_DL_ERROR, "invalid flag \"%s\" in line directive",
	       cpp_token_as_text (pfile, token));
  return 0;
}

/* Subroutine of do_line and do_linemarker.  Convert a number in STR,
   of length LEN, to binary; store it in NUMP, and return false if the
   number was well-formed, true if not. WRAPPED is set to true if the
   number did not fit into 'unsigned long'.  */
static bool
strtolinenum (const uchar *str, size_t len, linenum_type *nump, bool *wrapped)
{
  linenum_type reg = 0;
  linenum_type reg_prev = 0;

  uchar c;
  *wrapped = false;
  while (len--)
    {
      c = *str++;
      if (!ISDIGIT (c))
	return true;
      reg *= 10;
      reg += c - '0';
      if (reg < reg_prev) 
	*wrapped = true;
      reg_prev = reg;
    }
  *nump = reg;
  return false;
}

/* Interpret #line command.
   Note that the filename string (if any) is a true string constant
   (escapes are interpreted), unlike in #line.  */
static void
do_line (cpp_reader *pfile)
{
  struct line_maps *line_table = pfile->line_table;
  const line_map_ordinary *map = LINEMAPS_LAST_ORDINARY_MAP (line_table);

  /* skip_rest_of_line() may cause line table to be realloc()ed so note down
     sysp right now.  */

  unsigned char map_sysp = ORDINARY_MAP_IN_SYSTEM_HEADER_P (map);
  const cpp_token *token;
  const char *new_file = ORDINARY_MAP_FILE_NAME (map);
  linenum_type new_lineno;

  /* C99 raised the minimum limit on #line numbers.  */
  linenum_type cap = CPP_OPTION (pfile, c99) ? 2147483647 : 32767;
  bool wrapped;

  /* #line commands expand macros.  */
  token = cpp_get_token (pfile);
  if (token->type != CPP_NUMBER
      || strtolinenum (token->val.str.text, token->val.str.len,
		       &new_lineno, &wrapped))
    {
      if (token->type == CPP_EOF)
	cpp_error (pfile, CPP_DL_ERROR, "unexpected end of file after #line");
      else
	cpp_error (pfile, CPP_DL_ERROR,
		   "\"%s\" after #line is not a positive integer",
		   cpp_token_as_text (pfile, token));
      return;
    }

  if (CPP_PEDANTIC (pfile) && (new_lineno == 0 || new_lineno > cap || wrapped))
    cpp_error (pfile, CPP_DL_PEDWARN, "line number out of range");
  else if (wrapped)
    cpp_error (pfile, CPP_DL_WARNING, "line number out of range");

  token = cpp_get_token (pfile);
  if (token->type == CPP_STRING)
    {
      cpp_string s = { 0, 0 };
      if (cpp_interpret_string_notranslate (pfile, &token->val.str, 1,
					    &s, CPP_STRING))
	new_file = (const char *)s.text;
      check_eol (pfile, true);
    }
  else if (token->type != CPP_EOF)
    {
      cpp_error (pfile, CPP_DL_ERROR, "\"%s\" is not a valid filename",
		 cpp_token_as_text (pfile, token));
      return;
    }

  skip_rest_of_line (pfile);
  _cpp_do_file_change (pfile, LC_RENAME_VERBATIM, new_file, new_lineno,
		       map_sysp);
  line_table->seen_line_directive = true;
}

/* Interpret the # 44 "file" [flags] notation, which has slightly
   different syntax and semantics from #line:  Flags are allowed,
   and we never complain about the line number being too big.  */
static void
do_linemarker (cpp_reader *pfile)
{
  struct line_maps *line_table = pfile->line_table;
  const line_map_ordinary *map = LINEMAPS_LAST_ORDINARY_MAP (line_table);
  const cpp_token *token;
  const char *new_file = ORDINARY_MAP_FILE_NAME (map);
  linenum_type new_lineno;
  unsigned int new_sysp = ORDINARY_MAP_IN_SYSTEM_HEADER_P (map);
  enum lc_reason reason = LC_RENAME_VERBATIM;
  int flag;
  bool wrapped;

  /* Back up so we can get the number again.  Putting this in
     _cpp_handle_directive risks two calls to _cpp_backup_tokens in
     some circumstances, which can segfault.  */
  _cpp_backup_tokens (pfile, 1);

  /* #line commands expand macros.  */
  token = cpp_get_token (pfile);
  if (token->type != CPP_NUMBER
      || strtolinenum (token->val.str.text, token->val.str.len,
		       &new_lineno, &wrapped))
    {
      /* Unlike #line, there does not seem to be a way to get an EOF
	 here.  So, it should be safe to always spell the token.  */
      cpp_error (pfile, CPP_DL_ERROR,
		 "\"%s\" after # is not a positive integer",
		 cpp_token_as_text (pfile, token));
      return;
    }

  token = cpp_get_token (pfile);
  if (token->type == CPP_STRING)
    {
      cpp_string s = { 0, 0 };
      if (cpp_interpret_string_notranslate (pfile, &token->val.str,
					    1, &s, CPP_STRING))
	new_file = (const char *)s.text;

      new_sysp = 0;
      flag = read_flag (pfile, 0);
      if (flag == 1)
	{
	  reason = LC_ENTER;
	  /* Fake an include for cpp_included ().  */
	  _cpp_fake_include (pfile, new_file);
	  flag = read_flag (pfile, flag);
	}
      else if (flag == 2)
	{
	  reason = LC_LEAVE;
	  flag = read_flag (pfile, flag);
	}
      if (flag == 3)
	{
	  new_sysp = 1;
	  flag = read_flag (pfile, flag);
	  if (flag == 4)
	    new_sysp = 2;
	}
      pfile->buffer->sysp = new_sysp;

      check_eol (pfile, false);
    }
  else if (token->type != CPP_EOF)
    {
      cpp_error (pfile, CPP_DL_ERROR, "\"%s\" is not a valid filename",
		 cpp_token_as_text (pfile, token));
      return;
    }

  skip_rest_of_line (pfile);

  if (reason == LC_LEAVE)
    {
      /* Reread map since cpp_get_token can invalidate it with a
	 reallocation.  */
      map = LINEMAPS_LAST_ORDINARY_MAP (line_table);
      const line_map_ordinary *from
	= linemap_included_from_linemap (line_table, map);
      if (MAIN_FILE_P (map)
	  || (from
	      && filename_cmp (ORDINARY_MAP_FILE_NAME (from), new_file) != 0))
	{
	  cpp_warning (pfile, CPP_W_NONE,
		       "file \"%s\" linemarker ignored due to "
		       "incorrect nesting", new_file);
	  return;
	}
    }
  /* Compensate for the increment in linemap_add that occurs in
     _cpp_do_file_change.  We're currently at the start of the line
     *following* the #line directive.  A separate source_location for this
     location makes no sense (until we do the LC_LEAVE), and
     complicates LAST_SOURCE_LINE_LOCATION.  */
  pfile->line_table->highest_location--;

  _cpp_do_file_change (pfile, reason, new_file, new_lineno, new_sysp);
  line_table->seen_line_directive = true;
}

/* Arrange the file_change callback.  pfile->line has changed to
   FILE_LINE of TO_FILE, for reason REASON.  SYSP is 1 for a system
   header, 2 for a system header that needs to be extern "C" protected,
   and zero otherwise.  */
void
_cpp_do_file_change (cpp_reader *pfile, enum lc_reason reason,
		     const char *to_file, linenum_type file_line,
		     unsigned int sysp)
{
  linemap_assert (reason != LC_ENTER_MACRO);
  const struct line_map *map = linemap_add (pfile->line_table, reason, sysp,
					    to_file, file_line);
  const line_map_ordinary *ord_map = NULL;
  if (map != NULL)
    {
      ord_map = linemap_check_ordinary (map);
      linemap_line_start (pfile->line_table,
			  ORDINARY_MAP_STARTING_LINE_NUMBER (ord_map),
			  127);
    }

  if (pfile->cb.file_change)
    pfile->cb.file_change (pfile, ord_map);
}

/* Report a warning or error detected by the program we are
   processing.  Use the directive's tokens in the error message.  */
static void
do_diagnostic (cpp_reader *pfile, int code, int reason, int print_dir)
{
  const unsigned char *dir_name;
  unsigned char *line;
  source_location src_loc = pfile->cur_token[-1].src_loc;

  if (print_dir)
    dir_name = pfile->directive->name;
  else
    dir_name = NULL;
  pfile->state.prevent_expansion++;
  line = cpp_output_line_to_string (pfile, dir_name);
  pfile->state.prevent_expansion--;

  if (code == CPP_DL_WARNING_SYSHDR && reason)
    cpp_warning_with_line_syshdr (pfile, reason, src_loc, 0, "%s", line);
  else if (code == CPP_DL_WARNING && reason)
    cpp_warning_with_line (pfile, reason, src_loc, 0, "%s", line);
  else
    cpp_error_with_line (pfile, code, src_loc, 0, "%s", line);
  free (line);
}

static void
do_error (cpp_reader *pfile)
{
  do_diagnostic (pfile, CPP_DL_ERROR, 0, 1);
}

static void
do_warning (cpp_reader *pfile)
{
  /* We want #warning diagnostics to be emitted in system headers too.  */
  do_diagnostic (pfile, CPP_DL_WARNING_SYSHDR, CPP_W_WARNING_DIRECTIVE, 1);
}

/* Report program identification.  */
static void
do_ident (cpp_reader *pfile)
{
  const cpp_token *str = cpp_get_token (pfile);

  if (str->type != CPP_STRING)
    cpp_error (pfile, CPP_DL_ERROR, "invalid #%s directive",
	       pfile->directive->name);
  else if (pfile->cb.ident)
    pfile->cb.ident (pfile, pfile->directive_line, &str->val.str);

  check_eol (pfile, false);
}

/* Lookup a PRAGMA name in a singly-linked CHAIN.  Returns the
   matching entry, or NULL if none is found.  The returned entry could
   be the start of a namespace chain, or a pragma.  */
static struct pragma_entry *
lookup_pragma_entry (struct pragma_entry *chain, const cpp_hashnode *pragma)
{
  while (chain && chain->pragma != pragma)
    chain = chain->next;

  return chain;
}

/* Create and insert a blank pragma entry at the beginning of a
   singly-linked CHAIN.  */
static struct pragma_entry *
new_pragma_entry (cpp_reader *pfile, struct pragma_entry **chain)
{
  struct pragma_entry *new_entry;

  new_entry = (struct pragma_entry *)
    _cpp_aligned_alloc (pfile, sizeof (struct pragma_entry));

  memset (new_entry, 0, sizeof (struct pragma_entry));
  new_entry->next = *chain;

  *chain = new_entry;
  return new_entry;
}

/* Register a pragma NAME in namespace SPACE.  If SPACE is null, it
   goes in the global namespace.  */
static struct pragma_entry *
register_pragma_1 (cpp_reader *pfile, const char *space, const char *name,
		   bool allow_name_expansion)
{
  struct pragma_entry **chain = &pfile->pragmas;
  struct pragma_entry *entry;
  const cpp_hashnode *node;

  if (space)
    {
      node = cpp_lookup (pfile, UC space, strlen (space));
      entry = lookup_pragma_entry (*chain, node);
      if (!entry)
	{
	  entry = new_pragma_entry (pfile, chain);
	  entry->pragma = node;
	  entry->is_nspace = true;
	  entry->allow_expansion = allow_name_expansion;
	}
      else if (!entry->is_nspace)
	goto clash;
      else if (entry->allow_expansion != allow_name_expansion)
	{
	  cpp_error (pfile, CPP_DL_ICE,
		     "registering pragmas in namespace \"%s\" with mismatched "
		     "name expansion", space);
	  return NULL;
	}
      chain = &entry->u.space;
    }
  else if (allow_name_expansion)
    {
      cpp_error (pfile, CPP_DL_ICE,
		 "registering pragma \"%s\" with name expansion "
		 "and no namespace", name);
      return NULL;
    }

  /* Check for duplicates.  */
  node = cpp_lookup (pfile, UC name, strlen (name));
  entry = lookup_pragma_entry (*chain, node);
  if (entry == NULL)
    {
      entry = new_pragma_entry (pfile, chain);
      entry->pragma = node;
      return entry;
    }

  if (entry->is_nspace)
    clash:
    cpp_error (pfile, CPP_DL_ICE,
	       "registering \"%s\" as both a pragma and a pragma namespace",
	       NODE_NAME (node));
  else if (space)
    cpp_error (pfile, CPP_DL_ICE, "#pragma %s %s is already registered",
	       space, name);
  else
    cpp_error (pfile, CPP_DL_ICE, "#pragma %s is already registered", name);

  return NULL;
}

/* Register a cpplib internal pragma SPACE NAME with HANDLER.  */
static void
register_pragma_internal (cpp_reader *pfile, const char *space,
			  const char *name, pragma_cb handler)
{
  struct pragma_entry *entry;

  entry = register_pragma_1 (pfile, space, name, false);
  entry->is_internal = true;
  entry->u.handler = handler;
}

/* Register a pragma NAME in namespace SPACE.  If SPACE is null, it
   goes in the global namespace.  HANDLER is the handler it will call,
   which must be non-NULL.  If ALLOW_EXPANSION is set, allow macro
   expansion while parsing pragma NAME.  This function is exported
   from libcpp. */
void
cpp_register_pragma (cpp_reader *pfile, const char *space, const char *name,
		     pragma_cb handler, bool allow_expansion)
{
  struct pragma_entry *entry;

  if (!handler)
    {
      cpp_error (pfile, CPP_DL_ICE, "registering pragma with NULL handler");
      return;
    }

  entry = register_pragma_1 (pfile, space, name, false);
  if (entry)
    {
      entry->allow_expansion = allow_expansion;
      entry->u.handler = handler;
    }
}

/* Similarly, but create mark the pragma for deferred processing.
   When found, a CPP_PRAGMA token will be insertted into the stream
   with IDENT in the token->u.pragma slot.  */
void
cpp_register_deferred_pragma (cpp_reader *pfile, const char *space,
			      const char *name, unsigned int ident,
			      bool allow_expansion, bool allow_name_expansion)
{
  struct pragma_entry *entry;

  entry = register_pragma_1 (pfile, space, name, allow_name_expansion);
  if (entry)
    {
      entry->is_deferred = true;
      entry->allow_expansion = allow_expansion;
      entry->u.ident = ident;
    }
}  

/* Register the pragmas the preprocessor itself handles.  */
void
_cpp_init_internal_pragmas (cpp_reader *pfile)
{
  /* Pragmas in the global namespace.  */
  register_pragma_internal (pfile, 0, "once", do_pragma_once);
  register_pragma_internal (pfile, 0, "push_macro", do_pragma_push_macro);
  register_pragma_internal (pfile, 0, "pop_macro", do_pragma_pop_macro);

  /* New GCC-specific pragmas should be put in the GCC namespace.  */
  register_pragma_internal (pfile, "GCC", "poison", do_pragma_poison);
  register_pragma_internal (pfile, "GCC", "system_header",
			    do_pragma_system_header);
  register_pragma_internal (pfile, "GCC", "dependency", do_pragma_dependency);
  register_pragma_internal (pfile, "GCC", "warning", do_pragma_warning);
  register_pragma_internal (pfile, "GCC", "error", do_pragma_error);
}

/* Return the number of registered pragmas in PE.  */

static int
count_registered_pragmas (struct pragma_entry *pe)
{
  int ct = 0;
  for (; pe != NULL; pe = pe->next)
    {
      if (pe->is_nspace)
	ct += count_registered_pragmas (pe->u.space);
      ct++;
    }
  return ct;
}

/* Save into SD the names of the registered pragmas referenced by PE,
   and return a pointer to the next free space in SD.  */

static char **
save_registered_pragmas (struct pragma_entry *pe, char **sd)
{
  for (; pe != NULL; pe = pe->next)
    {
      if (pe->is_nspace)
	sd = save_registered_pragmas (pe->u.space, sd);
      *sd++ = (char *) xmemdup (HT_STR (&pe->pragma->ident),
                                HT_LEN (&pe->pragma->ident),
                                HT_LEN (&pe->pragma->ident) + 1);
    }
  return sd;
}

/* Return a newly-allocated array which saves the names of the
   registered pragmas.  */

char **
_cpp_save_pragma_names (cpp_reader *pfile)
{
  int ct = count_registered_pragmas (pfile->pragmas);
  char **result = XNEWVEC (char *, ct);
  (void) save_registered_pragmas (pfile->pragmas, result);
  return result;
}

/* Restore from SD the names of the registered pragmas referenced by PE,
   and return a pointer to the next unused name in SD.  */

static char **
restore_registered_pragmas (cpp_reader *pfile, struct pragma_entry *pe,
			    char **sd)
{
  for (; pe != NULL; pe = pe->next)
    {
      if (pe->is_nspace)
	sd = restore_registered_pragmas (pfile, pe->u.space, sd);
      pe->pragma = cpp_lookup (pfile, UC *sd, strlen (*sd));
      free (*sd);
      sd++;
    }
  return sd;
}

/* Restore the names of the registered pragmas from SAVED.  */

void
_cpp_restore_pragma_names (cpp_reader *pfile, char **saved)
{
  (void) restore_registered_pragmas (pfile, pfile->pragmas, saved);
  free (saved);
}

/* Pragmata handling.  We handle some, and pass the rest on to the
   front end.  C99 defines three pragmas and says that no macro
   expansion is to be performed on them; whether or not macro
   expansion happens for other pragmas is implementation defined.
   This implementation allows for a mix of both, since GCC did not
   traditionally macro expand its (few) pragmas, whereas OpenMP
   specifies that macro expansion should happen.  */
static void
do_pragma (cpp_reader *pfile)
{
  const struct pragma_entry *p = NULL;
  const cpp_token *token, *pragma_token;
  source_location pragma_token_virt_loc = 0;
  cpp_token ns_token;
  unsigned int count = 1;

  pfile->state.prevent_expansion++;

  pragma_token = token = cpp_get_token_with_location (pfile,
						      &pragma_token_virt_loc);
  ns_token = *token;
  if (token->type == CPP_NAME)
    {
      p = lookup_pragma_entry (pfile->pragmas, token->val.node.node);
      if (p && p->is_nspace)
	{
	  bool allow_name_expansion = p->allow_expansion;
	  if (allow_name_expansion)
	    pfile->state.prevent_expansion--;

	  token = cpp_get_token (pfile);
	  if (token->type == CPP_NAME)
	    p = lookup_pragma_entry (p->u.space, token->val.node.node);
	  else
	    p = NULL;
	  if (allow_name_expansion)
	    pfile->state.prevent_expansion++;
	  count = 2;
	}
    }

  if (p)
    {
      if (p->is_deferred)
	{
	  pfile->directive_result.src_loc = pragma_token_virt_loc;
	  pfile->directive_result.type = CPP_PRAGMA;
	  pfile->directive_result.flags = pragma_token->flags;
	  pfile->directive_result.val.pragma = p->u.ident;
	  pfile->state.in_deferred_pragma = true;
	  pfile->state.pragma_allow_expansion = p->allow_expansion;
	  if (!p->allow_expansion)
	    pfile->state.prevent_expansion++;
	}
      else
	{
	  /* Since the handler below doesn't get the line number, that
	     it might need for diagnostics, make sure it has the right
	     numbers in place.  */
	  if (pfile->cb.line_change)
	    (*pfile->cb.line_change) (pfile, pragma_token, false);
	  if (p->allow_expansion)
	    pfile->state.prevent_expansion--;
	  (*p->u.handler) (pfile);
	  if (p->allow_expansion)
	    pfile->state.prevent_expansion++;
	}
    }
  else if (pfile->cb.def_pragma)
    {
      if (count == 1 || pfile->context->prev == NULL)
	_cpp_backup_tokens (pfile, count);
      else
	{
	  /* Invalid name comes from macro expansion, _cpp_backup_tokens
	     won't allow backing 2 tokens.  */
	  /* ??? The token buffer is leaked.  Perhaps if def_pragma hook
	     reads both tokens, we could perhaps free it, but if it doesn't,
	     we don't know the exact lifespan.  */
	  cpp_token *toks = XNEWVEC (cpp_token, 2);
	  toks[0] = ns_token;
	  toks[0].flags |= NO_EXPAND;
	  toks[1] = *token;
	  toks[1].flags |= NO_EXPAND;
	  _cpp_push_token_context (pfile, NULL, toks, 2);
	}
      pfile->cb.def_pragma (pfile, pfile->directive_line);
    }

  pfile->state.prevent_expansion--;
}

/* Handle #pragma once.  */
static void
do_pragma_once (cpp_reader *pfile)
{
  if (cpp_in_primary_file (pfile))
    cpp_error (pfile, CPP_DL_WARNING, "#pragma once in main file");

  check_eol (pfile, false);
  _cpp_mark_file_once_only (pfile, pfile->buffer->file);
}

/* Handle #pragma push_macro(STRING).  */
static void
do_pragma_push_macro (cpp_reader *pfile)
{
  cpp_hashnode *node;
  size_t defnlen;
  const uchar *defn = NULL;
  char *macroname, *dest;
  const char *limit, *src;
  const cpp_token *txt;
  struct def_pragma_macro *c;

  txt = get__Pragma_string (pfile);
  if (!txt)
    {
      source_location src_loc = pfile->cur_token[-1].src_loc;
      cpp_error_with_line (pfile, CPP_DL_ERROR, src_loc, 0,
		 "invalid #pragma push_macro directive");
      check_eol (pfile, false);
      skip_rest_of_line (pfile);
      return;
    }
  dest = macroname = (char *) alloca (txt->val.str.len + 2);
  src = (const char *) (txt->val.str.text + 1 + (txt->val.str.text[0] == 'L'));
  limit = (const char *) (txt->val.str.text + txt->val.str.len - 1);
  while (src < limit)
    {
      /* We know there is a character following the backslash.  */
      if (*src == '\\' && (src[1] == '\\' || src[1] == '"'))
	src++;
      *dest++ = *src++;
    }
  *dest = 0;
  check_eol (pfile, false);
  skip_rest_of_line (pfile);
  c = XNEW (struct def_pragma_macro);
  memset (c, 0, sizeof (struct def_pragma_macro));
  c->name = XNEWVAR (char, strlen (macroname) + 1);
  strcpy (c->name, macroname);
  c->next = pfile->pushed_macros;
  node = _cpp_lex_identifier (pfile, c->name);
  if (node->type == NT_VOID)
    c->is_undef = 1;
  else
    {
      defn = cpp_macro_definition (pfile, node);
      defnlen = ustrlen (defn);
      c->definition = XNEWVEC (uchar, defnlen + 2);
      c->definition[defnlen] = '\n';
      c->definition[defnlen + 1] = 0;
      c->line = node->value.macro->line;
      c->syshdr = node->value.macro->syshdr;
      c->used = node->value.macro->used;
      memcpy (c->definition, defn, defnlen);
    }

  pfile->pushed_macros = c;
}

/* Handle #pragma pop_macro(STRING).  */
static void
do_pragma_pop_macro (cpp_reader *pfile)
{
  char *macroname, *dest;
  const char *limit, *src;
  const cpp_token *txt;
  struct def_pragma_macro *l = NULL, *c = pfile->pushed_macros;
  txt = get__Pragma_string (pfile);
  if (!txt)
    {
      source_location src_loc = pfile->cur_token[-1].src_loc;
      cpp_error_with_line (pfile, CPP_DL_ERROR, src_loc, 0,
		 "invalid #pragma pop_macro directive");
      check_eol (pfile, false);
      skip_rest_of_line (pfile);
      return;
    }
  dest = macroname = (char *) alloca (txt->val.str.len + 2);
  src = (const char *) (txt->val.str.text + 1 + (txt->val.str.text[0] == 'L'));
  limit = (const char *) (txt->val.str.text + txt->val.str.len - 1);
  while (src < limit)
    {
      /* We know there is a character following the backslash.  */
      if (*src == '\\' && (src[1] == '\\' || src[1] == '"'))
	src++;
      *dest++ = *src++;
    }
  *dest = 0;
  check_eol (pfile, false);
  skip_rest_of_line (pfile);

  while (c != NULL)
    {
      if (!strcmp (c->name, macroname))
	{
	  if (!l)
	    pfile->pushed_macros = c->next;
	  else
	    l->next = c->next;
	  cpp_pop_definition (pfile, c);
	  free (c->definition);
	  free (c->name);
	  free (c);
	  break;
	}
      l = c;
      c = c->next;
    }
}

/* Handle #pragma GCC poison, to poison one or more identifiers so
   that the lexer produces a hard error for each subsequent usage.  */
static void
do_pragma_poison (cpp_reader *pfile)
{
  const cpp_token *tok;
  cpp_hashnode *hp;

  pfile->state.poisoned_ok = 1;
  for (;;)
    {
      tok = _cpp_lex_token (pfile);
      if (tok->type == CPP_EOF)
	break;
      if (tok->type != CPP_NAME)
	{
	  cpp_error (pfile, CPP_DL_ERROR,
		     "invalid #pragma GCC poison directive");
	  break;
	}

      hp = tok->val.node.node;
      if (hp->flags & NODE_POISONED)
	continue;

      if (cpp_macro_p (hp))
	cpp_error (pfile, CPP_DL_WARNING, "poisoning existing macro \"%s\"",
		   NODE_NAME (hp));
      _cpp_free_definition (hp);
      hp->flags |= NODE_POISONED | NODE_DIAGNOSTIC;
    }
  pfile->state.poisoned_ok = 0;
}

/* Mark the current header as a system header.  This will suppress
   some categories of warnings (notably those from -pedantic).  It is
   intended for use in system libraries that cannot be implemented in
   conforming C, but cannot be certain that their headers appear in a
   system include directory.  To prevent abuse, it is rejected in the
   primary source file.  */
static void
do_pragma_system_header (cpp_reader *pfile)
{
  if (cpp_in_primary_file (pfile))
    cpp_error (pfile, CPP_DL_WARNING,
	       "#pragma system_header ignored outside include file");
  else
    {
      check_eol (pfile, false);
      skip_rest_of_line (pfile);
      cpp_make_system_header (pfile, 1, 0);
    }
}

/* Check the modified date of the current include file against a specified
   file. Issue a diagnostic, if the specified file is newer. We use this to
   determine if a fixed header should be refixed.  */
static void
do_pragma_dependency (cpp_reader *pfile)
{
  const char *fname;
  int angle_brackets, ordering;
  source_location location;

  fname = parse_include (pfile, &angle_brackets, NULL, &location);
  if (!fname)
    return;

  ordering = _cpp_compare_file_date (pfile, fname, angle_brackets);
  if (ordering < 0)
    cpp_error (pfile, CPP_DL_WARNING, "cannot find source file %s", fname);
  else if (ordering > 0)
    {
      cpp_error (pfile, CPP_DL_WARNING,
		 "current file is older than %s", fname);
      if (cpp_get_token (pfile)->type != CPP_EOF)
	{
	  _cpp_backup_tokens (pfile, 1);
	  do_diagnostic (pfile, CPP_DL_WARNING, 0, 0);
	}
    }

  free ((void *) fname);
}

/* Issue a diagnostic with the message taken from the pragma.  If
   ERROR is true, the diagnostic is a warning, otherwise, it is an
   error.  */
static void
do_pragma_warning_or_error (cpp_reader *pfile, bool error)
{
  const cpp_token *tok = _cpp_lex_token (pfile);
  cpp_string str;
  if (tok->type != CPP_STRING
      || !cpp_interpret_string_notranslate (pfile, &tok->val.str, 1, &str,
					    CPP_STRING)
      || str.len == 0)
    {
      cpp_error (pfile, CPP_DL_ERROR, "invalid \"#pragma GCC %s\" directive",
		 error ? "error" : "warning");
      return;
    }
  cpp_error (pfile, error ? CPP_DL_ERROR : CPP_DL_WARNING,
	     "%s", str.text);
  free ((void *)str.text);
}

/* Issue a warning diagnostic.  */
static void
do_pragma_warning (cpp_reader *pfile)
{
  do_pragma_warning_or_error (pfile, false);
}

/* Issue an error diagnostic.  */
static void
do_pragma_error (cpp_reader *pfile)
{
  do_pragma_warning_or_error (pfile, true);
}

/* Get a token but skip padding.  */
static const cpp_token *
get_token_no_padding (cpp_reader *pfile)
{
  for (;;)
    {
      const cpp_token *result = cpp_get_token (pfile);
      if (result->type != CPP_PADDING)
	return result;
    }
}

/* Check syntax is "(string-literal)".  Returns the string on success,
   or NULL on failure.  */
static const cpp_token *
get__Pragma_string (cpp_reader *pfile)
{
  const cpp_token *string;
  const cpp_token *paren;

  paren = get_token_no_padding (pfile);
  if (paren->type == CPP_EOF)
    _cpp_backup_tokens (pfile, 1);
  if (paren->type != CPP_OPEN_PAREN)
    return NULL;

  string = get_token_no_padding (pfile);
  if (string->type == CPP_EOF)
    _cpp_backup_tokens (pfile, 1);
  if (string->type != CPP_STRING && string->type != CPP_WSTRING
      && string->type != CPP_STRING32 && string->type != CPP_STRING16
      && string->type != CPP_UTF8STRING)
    return NULL;

  paren = get_token_no_padding (pfile);
  if (paren->type == CPP_EOF)
    _cpp_backup_tokens (pfile, 1);
  if (paren->type != CPP_CLOSE_PAREN)
    return NULL;

  return string;
}

/* Destringize IN into a temporary buffer, by removing the first \ of
   \" and \\ sequences, and process the result as a #pragma directive.  */
static void
destringize_and_run (cpp_reader *pfile, const cpp_string *in,
		     source_location expansion_loc)
{
  const unsigned char *src, *limit;
  char *dest, *result;
  cpp_context *saved_context;
  cpp_token *saved_cur_token;
  tokenrun *saved_cur_run;
  cpp_token *toks;
  int count;
  const struct directive *save_directive;

  dest = result = (char *) alloca (in->len - 1);
  src = in->text + 1 + (in->text[0] == 'L');
  limit = in->text + in->len - 1;
  while (src < limit)
    {
      /* We know there is a character following the backslash.  */
      if (*src == '\\' && (src[1] == '\\' || src[1] == '"'))
	src++;
      *dest++ = *src++;
    }
  *dest = '\n';

  /* Ugh; an awful kludge.  We are really not set up to be lexing
     tokens when in the middle of a macro expansion.  Use a new
     context to force cpp_get_token to lex, and so skip_rest_of_line
     doesn't go beyond the end of the text.  Also, remember the
     current lexing position so we can return to it later.

     Something like line-at-a-time lexing should remove the need for
     this.  */
  saved_context = pfile->context;
  saved_cur_token = pfile->cur_token;
  saved_cur_run = pfile->cur_run;

  pfile->context = XCNEW (cpp_context);

  /* Inline run_directive, since we need to delay the _cpp_pop_buffer
     until we've read all of the tokens that we want.  */
  cpp_push_buffer (pfile, (const uchar *) result, dest - result,
		   /* from_stage3 */ true);
  /* ??? Antique Disgusting Hack.  What does this do?  */
  if (pfile->buffer->prev)
    pfile->buffer->file = pfile->buffer->prev->file;

  start_directive (pfile);
  _cpp_clean_line (pfile);
  save_directive = pfile->directive;
  pfile->directive = &dtable[T_PRAGMA];
  do_pragma (pfile);
  end_directive (pfile, 1);
  pfile->directive = save_directive;

  /* We always insert at least one token, the directive result.  It'll
     either be a CPP_PADDING or a CPP_PRAGMA.  In the later case, we 
     need to insert *all* of the tokens, including the CPP_PRAGMA_EOL.  */

  /* If we're not handling the pragma internally, read all of the tokens from
     the string buffer now, while the string buffer is still installed.  */
  /* ??? Note that the token buffer allocated here is leaked.  It's not clear
     to me what the true lifespan of the tokens are.  It would appear that
     the lifespan is the entire parse of the main input stream, in which case
     this may not be wrong.  */
  if (pfile->directive_result.type == CPP_PRAGMA)
    {
      int maxcount;

      count = 1;
      maxcount = 50;
      toks = XNEWVEC (cpp_token, maxcount);
      toks[0] = pfile->directive_result;

      do
	{
	  if (count == maxcount)
	    {
	      maxcount = maxcount * 3 / 2;
	      toks = XRESIZEVEC (cpp_token, toks, maxcount);
	    }
	  toks[count] = *cpp_get_token (pfile);
	  /* _Pragma is a builtin, so we're not within a macro-map, and so
	     the token locations are set to bogus ordinary locations
	     near to, but after that of the "_Pragma".
	     Paper over this by setting them equal to the location of the
	     _Pragma itself (PR preprocessor/69126).  */
	  toks[count].src_loc = expansion_loc;
	  /* Macros have been already expanded by cpp_get_token
	     if the pragma allowed expansion.  */
	  toks[count++].flags |= NO_EXPAND;
	}
      while (toks[count-1].type != CPP_PRAGMA_EOL);
    }
  else
    {
      count = 1;
      toks = XNEW (cpp_token);
      toks[0] = pfile->directive_result;

      /* If we handled the entire pragma internally, make sure we get the
	 line number correct for the next token.  */
      if (pfile->cb.line_change)
	pfile->cb.line_change (pfile, pfile->cur_token, false);
    }

  /* Finish inlining run_directive.  */
  pfile->buffer->file = NULL;
  _cpp_pop_buffer (pfile);

  /* Reset the old macro state before ...  */
  XDELETE (pfile->context);
  pfile->context = saved_context;
  pfile->cur_token = saved_cur_token;
  pfile->cur_run = saved_cur_run;

  /* ... inserting the new tokens we collected.  */
  _cpp_push_token_context (pfile, NULL, toks, count);
}

/* Handle the _Pragma operator.  Return 0 on error, 1 if ok.  */
int
_cpp_do__Pragma (cpp_reader *pfile, source_location expansion_loc)
{
  const cpp_token *string = get__Pragma_string (pfile);
  pfile->directive_result.type = CPP_PADDING;

  if (string)
    {
      destringize_and_run (pfile, &string->val.str, expansion_loc);
      return 1;
    }
  cpp_error (pfile, CPP_DL_ERROR,
	     "_Pragma takes a parenthesized string literal");
  return 0;
}

/* Handle #ifdef.  */
static void
do_ifdef (cpp_reader *pfile)
{
  int skip = 1;

  if (! pfile->state.skipping)
    {
      cpp_hashnode *node = lex_macro_node (pfile, false);

      if (node)
	{
	  /* Do not treat conditional macros as being defined.  This is due to
	     the powerpc and spu ports using conditional macros for 'vector',
	     'bool', and 'pixel' to act as conditional keywords.  This messes
	     up tests like #ifndef bool.  */
	  skip = !cpp_macro_p (node) || (node->flags & NODE_CONDITIONAL);
	  _cpp_mark_macro_used (node);
	  _cpp_maybe_notify_macro_use (pfile, node);
	  if (pfile->cb.used)
	    pfile->cb.used (pfile, pfile->directive_line, node);
	  check_eol (pfile, false);
	}
    }

  push_conditional (pfile, skip, T_IFDEF, 0);
}

/* Handle #ifndef.  */
static void
do_ifndef (cpp_reader *pfile)
{
  int skip = 1;
  cpp_hashnode *node = 0;

  if (! pfile->state.skipping)
    {
      node = lex_macro_node (pfile, false);

      if (node)
	{
	  /* Do not treat conditional macros as being defined.  This is due to
	     the powerpc and spu ports using conditional macros for 'vector',
	     'bool', and 'pixel' to act as conditional keywords.  This messes
	     up tests like #ifndef bool.  */
	  skip = (cpp_macro_p (node)
		  && !(node->flags & NODE_CONDITIONAL));
	  _cpp_mark_macro_used (node);
	  _cpp_maybe_notify_macro_use (pfile, node);
	  if (pfile->cb.used)
	    pfile->cb.used (pfile, pfile->directive_line, node);
	  check_eol (pfile, false);
	}
    }

  push_conditional (pfile, skip, T_IFNDEF, node);
}

/* _cpp_parse_expr puts a macro in a "#if !defined ()" expression in
   pfile->mi_ind_cmacro so we can handle multiple-include
   optimizations.  If macro expansion occurs in the expression, we
   cannot treat it as a controlling conditional, since the expansion
   could change in the future.  That is handled by cpp_get_token.  */
static void
do_if (cpp_reader *pfile)
{
  int skip = 1;

  if (! pfile->state.skipping)
    skip = _cpp_parse_expr (pfile, true) == false;

  push_conditional (pfile, skip, T_IF, pfile->mi_ind_cmacro);
}

/* Flip skipping state if appropriate and continue without changing
   if_stack; this is so that the error message for missing #endif's
   etc. will point to the original #if.  */
static void
do_else (cpp_reader *pfile)
{
  cpp_buffer *buffer = pfile->buffer;
  struct if_stack *ifs = buffer->if_stack;

  if (ifs == NULL)
    cpp_error (pfile, CPP_DL_ERROR, "#else without #if");
  else
    {
      if (ifs->type == T_ELSE)
	{
	  cpp_error (pfile, CPP_DL_ERROR, "#else after #else");
	  cpp_error_with_line (pfile, CPP_DL_ERROR, ifs->line, 0,
			       "the conditional began here");
	}
      ifs->type = T_ELSE;

      /* Skip any future (erroneous) #elses or #elifs.  */
      pfile->state.skipping = ifs->skip_elses;
      ifs->skip_elses = true;

      /* Invalidate any controlling macro.  */
      ifs->mi_cmacro = 0;

      /* Only check EOL if was not originally skipping.  */
      if (!ifs->was_skipping && CPP_OPTION (pfile, warn_endif_labels))
	check_eol_endif_labels (pfile);
    }
}

/* Handle a #elif directive by not changing if_stack either.  See the
   comment above do_else.  */
static void
do_elif (cpp_reader *pfile)
{
  cpp_buffer *buffer = pfile->buffer;
  struct if_stack *ifs = buffer->if_stack;

  if (ifs == NULL)
    cpp_error (pfile, CPP_DL_ERROR, "#elif without #if");
  else
    {
      if (ifs->type == T_ELSE)
	{
	  cpp_error (pfile, CPP_DL_ERROR, "#elif after #else");
	  cpp_error_with_line (pfile, CPP_DL_ERROR, ifs->line, 0,
			       "the conditional began here");
	}
      ifs->type = T_ELIF;

      /* See DR#412: "Only the first group whose control condition
	 evaluates to true (nonzero) is processed; any following groups
	 are skipped and their controlling directives are processed as
	 if they were in a group that is skipped."  */
      if (ifs->skip_elses)
	pfile->state.skipping = 1;
      else
	{
	  pfile->state.skipping = ! _cpp_parse_expr (pfile, false);
	  ifs->skip_elses = ! pfile->state.skipping;
	}

      /* Invalidate any controlling macro.  */
      ifs->mi_cmacro = 0;
    }
}

/* #endif pops the if stack and resets pfile->state.skipping.  */
static void
do_endif (cpp_reader *pfile)
{
  cpp_buffer *buffer = pfile->buffer;
  struct if_stack *ifs = buffer->if_stack;

  if (ifs == NULL)
    cpp_error (pfile, CPP_DL_ERROR, "#endif without #if");
  else
    {
      /* Only check EOL if was not originally skipping.  */
      if (!ifs->was_skipping && CPP_OPTION (pfile, warn_endif_labels))
	check_eol_endif_labels (pfile);

      /* If potential control macro, we go back outside again.  */
      if (ifs->next == 0 && ifs->mi_cmacro)
	{
	  pfile->mi_valid = true;
	  pfile->mi_cmacro = ifs->mi_cmacro;
	}

      buffer->if_stack = ifs->next;
      pfile->state.skipping = ifs->was_skipping;
      obstack_free (&pfile->buffer_ob, ifs);
    }
}

/* Push an if_stack entry for a preprocessor conditional, and set
   pfile->state.skipping to SKIP.  If TYPE indicates the conditional
   is #if or #ifndef, CMACRO is a potentially controlling macro, and
   we need to check here that we are at the top of the file.  */
static void
push_conditional (cpp_reader *pfile, int skip, int type,
		  const cpp_hashnode *cmacro)
{
  struct if_stack *ifs;
  cpp_buffer *buffer = pfile->buffer;

  ifs = XOBNEW (&pfile->buffer_ob, struct if_stack);
  ifs->line = pfile->directive_line;
  ifs->next = buffer->if_stack;
  ifs->skip_elses = pfile->state.skipping || !skip;
  ifs->was_skipping = pfile->state.skipping;
  ifs->type = type;
  /* This condition is effectively a test for top-of-file.  */
  if (pfile->mi_valid && pfile->mi_cmacro == 0)
    ifs->mi_cmacro = cmacro;
  else
    ifs->mi_cmacro = 0;

  pfile->state.skipping = skip;
  buffer->if_stack = ifs;
}

/* Read the tokens of the answer into the macro pool, in a directive
   of type TYPE.  Only commit the memory if we intend it as permanent
   storage, i.e. the #assert case.  Returns 0 on success, and sets
   ANSWERP to point to the answer.  PRED_LOC is the location of the
   predicate.  */
static bool
parse_answer (cpp_reader *pfile, int type, source_location pred_loc,
	      cpp_macro **answer_ptr)
{
  /* In a conditional, it is legal to not have an open paren.  We
     should save the following token in this case.  */
  const cpp_token *paren = cpp_get_token (pfile);

  /* If not a paren, see if we're OK.  */
  if (paren->type != CPP_OPEN_PAREN)
    {
      /* In a conditional no answer is a test for any answer.  It
         could be followed by any token.  */
      if (type == T_IF)
	{
	  _cpp_backup_tokens (pfile, 1);
	  return true;
	}

      /* #unassert with no answer is valid - it removes all answers.  */
      if (type == T_UNASSERT && paren->type == CPP_EOF)
	return true;

      cpp_error_with_line (pfile, CPP_DL_ERROR, pred_loc, 0,
			   "missing '(' after predicate");
      return false;
    }

  cpp_macro *answer = _cpp_new_macro (pfile, cmk_assert,
				      _cpp_reserve_room (pfile, 0,
							 sizeof (cpp_macro)));
  answer->parm.next = NULL;
  unsigned count = 0;
  for (;;)
    {
      const cpp_token *token = cpp_get_token (pfile);

      if (token->type == CPP_CLOSE_PAREN)
	break;

      if (token->type == CPP_EOF)
	{
	  cpp_error (pfile, CPP_DL_ERROR, "missing ')' to complete answer");
	  return false;
	}

      answer = (cpp_macro *)_cpp_reserve_room
	(pfile, sizeof (cpp_macro) + count * sizeof (cpp_token),
	 sizeof (cpp_token));
      answer->exp.tokens[count++] = *token;
    }

  if (!count)
    {
      cpp_error (pfile, CPP_DL_ERROR, "predicate's answer is empty");
      return false;
    }

  /* Drop whitespace at start, for answer equivalence purposes.  */
  answer->exp.tokens[0].flags &= ~PREV_WHITE;

  answer->count = count;
  *answer_ptr = answer;

  return true;
}

/* Parses an assertion directive of type TYPE, returning a pointer to
   the hash node of the predicate, or 0 on error.  The node is
   guaranteed to be disjoint from the macro namespace, so can only
   have type 'NT_VOID'.  If an answer was supplied, it is placed in
   *ANSWER_PTR, which is otherwise set to 0.  */
static cpp_hashnode *
parse_assertion (cpp_reader *pfile, int type, cpp_macro **answer_ptr)
{
  cpp_hashnode *result = 0;

  /* We don't expand predicates or answers.  */
  pfile->state.prevent_expansion++;

  *answer_ptr = NULL;

  const cpp_token *predicate = cpp_get_token (pfile);
  if (predicate->type == CPP_EOF)
    cpp_error (pfile, CPP_DL_ERROR, "assertion without predicate");
  else if (predicate->type != CPP_NAME)
    cpp_error_with_line (pfile, CPP_DL_ERROR, predicate->src_loc, 0,
			 "predicate must be an identifier");
  else if (parse_answer (pfile, type, predicate->src_loc, answer_ptr))
    {
      unsigned int len = NODE_LEN (predicate->val.node.node);
      unsigned char *sym = (unsigned char *) alloca (len + 1);

      /* Prefix '#' to get it out of macro namespace.  */
      sym[0] = '#';
      memcpy (sym + 1, NODE_NAME (predicate->val.node.node), len);
      result = cpp_lookup (pfile, sym, len + 1);
    }

  pfile->state.prevent_expansion--;

  return result;
}

/* Returns a pointer to the pointer to CANDIDATE in the answer chain,
   or a pointer to NULL if the answer is not in the chain.  */
static cpp_macro **
find_answer (cpp_hashnode *node, const cpp_macro *candidate)
{
  unsigned int i;
  cpp_macro **result = NULL;

  for (result = &node->value.answers; *result; result = &(*result)->parm.next)
    {
      cpp_macro *answer = *result;

      if (answer->count == candidate->count)
	{
	  for (i = 0; i < answer->count; i++)
	    if (!_cpp_equiv_tokens (&answer->exp.tokens[i],
				    &candidate->exp.tokens[i]))
	      break;

	  if (i == answer->count)
	    break;
	}
    }

  return result;
}

/* Test an assertion within a preprocessor conditional.  Returns
   nonzero on failure, zero on success.  On success, the result of
   the test is written into VALUE, otherwise the value 0.  */
int
_cpp_test_assertion (cpp_reader *pfile, unsigned int *value)
{
  cpp_macro *answer;
  cpp_hashnode *node = parse_assertion (pfile, T_IF, &answer);

  /* For recovery, an erroneous assertion expression is handled as a
     failing assertion.  */
  *value = 0;

  if (node)
    {
      if (node->value.answers)
	*value = !answer || *find_answer (node, answer);
    }
  else if (pfile->cur_token[-1].type == CPP_EOF)
    _cpp_backup_tokens (pfile, 1);

  /* We don't commit the memory for the answer - it's temporary only.  */
  return node == 0;
}

/* Handle #assert.  */
static void
do_assert (cpp_reader *pfile)
{
  cpp_macro *answer;
  cpp_hashnode *node = parse_assertion (pfile, T_ASSERT, &answer);

  if (node)
    {
      /* Place the new answer in the answer list.  First check there
         is not a duplicate.  */
      if (*find_answer (node, answer))
	{
	  cpp_error (pfile, CPP_DL_WARNING, "\"%s\" re-asserted",
		     NODE_NAME (node) + 1);
	  return;
	}

      /* Commit or allocate storage for the answer.  */
      answer = (cpp_macro *)_cpp_commit_buff
	(pfile, sizeof (cpp_macro) - sizeof (cpp_token)
	 + sizeof (cpp_token) * answer->count);

      /* Chain into the list.  */
      answer->parm.next = node->value.answers;
      node->value.answers = answer;

      check_eol (pfile, false);
    }
}

/* Handle #unassert.  */
static void
do_unassert (cpp_reader *pfile)
{
  cpp_macro *answer;
  cpp_hashnode *node = parse_assertion (pfile, T_UNASSERT, &answer);

  /* It isn't an error to #unassert something that isn't asserted.  */
  if (node)
    {
      if (answer)
	{
	  cpp_macro **p = find_answer (node, answer);

	  /* Remove the assert from the list.  */
	  if (cpp_macro *temp = *p)
	    *p = temp->parm.next;

	  check_eol (pfile, false);
	}
      else
	_cpp_free_definition (node);
    }

  /* We don't commit the memory for the answer - it's temporary only.  */
}

/* These are for -D, -U, -A.  */

/* Process the string STR as if it appeared as the body of a #define.
   If STR is just an identifier, define it with value 1.
   If STR has anything after the identifier, then it should
   be identifier=definition.  */
void
cpp_define (cpp_reader *pfile, const char *str)
{
  char *buf;
  const char *p;
  size_t count;

  /* Copy the entire option so we can modify it.
     Change the first "=" in the string to a space.  If there is none,
     tack " 1" on the end.  */

  count = strlen (str);
  buf = (char *) alloca (count + 3);
  memcpy (buf, str, count);

  p = strchr (str, '=');
  if (p)
    buf[p - str] = ' ';
  else
    {
      buf[count++] = ' ';
      buf[count++] = '1';
    }
  buf[count] = '\n';

  run_directive (pfile, T_DEFINE, buf, count);
}


/* Use to build macros to be run through cpp_define() as
   described above.
   Example: cpp_define_formatted (pfile, "MACRO=%d", value);  */

void
cpp_define_formatted (cpp_reader *pfile, const char *fmt, ...)
{
  char *ptr;

  va_list ap;
  va_start (ap, fmt);
  ptr = xvasprintf (fmt, ap);
  va_end (ap);

  cpp_define (pfile, ptr);
  free (ptr);
}


/* Slight variant of the above for use by initialize_builtins.  */
void
_cpp_define_builtin (cpp_reader *pfile, const char *str)
{
  size_t len = strlen (str);
  char *buf = (char *) alloca (len + 1);
  memcpy (buf, str, len);
  buf[len] = '\n';
  run_directive (pfile, T_DEFINE, buf, len);
}

/* Process MACRO as if it appeared as the body of an #undef.  */
void
cpp_undef (cpp_reader *pfile, const char *macro)
{
  size_t len = strlen (macro);
  char *buf = (char *) alloca (len + 1);
  memcpy (buf, macro, len);
  buf[len] = '\n';
  run_directive (pfile, T_UNDEF, buf, len);
}

/* Replace a previous definition DEF of the macro STR.  If DEF is NULL,
   or first element is zero, then the macro should be undefined.  */
static void
cpp_pop_definition (cpp_reader *pfile, struct def_pragma_macro *c)
{
  cpp_hashnode *node = _cpp_lex_identifier (pfile, c->name);
  if (node == NULL)
    return;

  if (pfile->cb.before_define)
    pfile->cb.before_define (pfile);

  if (cpp_macro_p (node))
    {
      if (pfile->cb.undef)
	pfile->cb.undef (pfile, pfile->directive_line, node);
      if (CPP_OPTION (pfile, warn_unused_macros))
	_cpp_warn_if_unused_macro (pfile, node, NULL);
      _cpp_free_definition (node);
    }

  if (c->is_undef)
    return;

  {
    size_t namelen;
    const uchar *dn;
    cpp_hashnode *h = NULL;
    cpp_buffer *nbuf;

    namelen = ustrcspn (c->definition, "( \n");
    h = cpp_lookup (pfile, c->definition, namelen);
    dn = c->definition + namelen;

    nbuf = cpp_push_buffer (pfile, dn, ustrchr (dn, '\n') - dn, true);
    if (nbuf != NULL)
      {
	_cpp_clean_line (pfile);
	nbuf->sysp = 1;
	if (!_cpp_create_definition (pfile, h))
	  abort ();
	_cpp_pop_buffer (pfile);
      }
    else
      abort ();
    h->value.macro->line = c->line;
    h->value.macro->syshdr = c->syshdr;
    h->value.macro->used = c->used;
  }
}

/* Process the string STR as if it appeared as the body of a #assert.  */
void
cpp_assert (cpp_reader *pfile, const char *str)
{
  handle_assertion (pfile, str, T_ASSERT);
}

/* Process STR as if it appeared as the body of an #unassert.  */
void
cpp_unassert (cpp_reader *pfile, const char *str)
{
  handle_assertion (pfile, str, T_UNASSERT);
}

/* Common code for cpp_assert (-A) and cpp_unassert (-A-).  */
static void
handle_assertion (cpp_reader *pfile, const char *str, int type)
{
  size_t count = strlen (str);
  const char *p = strchr (str, '=');

  /* Copy the entire option so we can modify it.  Change the first
     "=" in the string to a '(', and tack a ')' on the end.  */
  char *buf = (char *) alloca (count + 2);

  memcpy (buf, str, count);
  if (p)
    {
      buf[p - str] = '(';
      buf[count++] = ')';
    }
  buf[count] = '\n';
  str = buf;

  run_directive (pfile, type, str, count);
}

/* The options structure.  */
cpp_options *
cpp_get_options (cpp_reader *pfile)
{
  return &pfile->opts;
}

/* The callbacks structure.  */
cpp_callbacks *
cpp_get_callbacks (cpp_reader *pfile)
{
  return &pfile->cb;
}

/* Copy the given callbacks structure to our own.  */
void
cpp_set_callbacks (cpp_reader *pfile, cpp_callbacks *cb)
{
  pfile->cb = *cb;
}

/* The dependencies structure.  (Creates one if it hasn't already been.)  */
struct deps *
cpp_get_deps (cpp_reader *pfile)
{
  if (!pfile->deps)
    pfile->deps = deps_init ();
  return pfile->deps;
}

/* Push a new buffer on the buffer stack.  Returns the new buffer; it
   doesn't fail.  It does not generate a file change call back; that
   is the responsibility of the caller.  */
cpp_buffer *
cpp_push_buffer (cpp_reader *pfile, const uchar *buffer, size_t len,
		 int from_stage3)
{
  cpp_buffer *new_buffer = XOBNEW (&pfile->buffer_ob, cpp_buffer);

  /* Clears, amongst other things, if_stack and mi_cmacro.  */
  memset (new_buffer, 0, sizeof (cpp_buffer));

  new_buffer->next_line = new_buffer->buf = buffer;
  new_buffer->rlimit = buffer + len;
  new_buffer->from_stage3 = from_stage3;
  new_buffer->prev = pfile->buffer;
  new_buffer->need_line = true;

  pfile->buffer = new_buffer;

  return new_buffer;
}

/* Pops a single buffer, with a file change call-back if appropriate.
   Then pushes the next -include file, if any remain.  */
void
_cpp_pop_buffer (cpp_reader *pfile)
{
  cpp_buffer *buffer = pfile->buffer;
  struct _cpp_file *inc = buffer->file;
  struct if_stack *ifs;
  const unsigned char *to_free;

  /* Walk back up the conditional stack till we reach its level at
     entry to this file, issuing error messages.  */
  for (ifs = buffer->if_stack; ifs; ifs = ifs->next)
    cpp_error_with_line (pfile, CPP_DL_ERROR, ifs->line, 0,
			 "unterminated #%s", dtable[ifs->type].name);

  /* In case of a missing #endif.  */
  pfile->state.skipping = 0;

  /* _cpp_do_file_change expects pfile->buffer to be the new one.  */
  pfile->buffer = buffer->prev;

  to_free = buffer->to_free;
  free (buffer->notes);

  /* Free the buffer object now; we may want to push a new buffer
     in _cpp_push_next_include_file.  */
  obstack_free (&pfile->buffer_ob, buffer);

  if (inc)
    {
      _cpp_pop_file_buffer (pfile, inc, to_free);

      _cpp_do_file_change (pfile, LC_LEAVE, 0, 0, 0);
    }
}

/* Enter all recognized directives in the hash table.  */
void
_cpp_init_directives (cpp_reader *pfile)
{
  unsigned int i;
  cpp_hashnode *node;

  for (i = 0; i < (unsigned int) N_DIRECTIVES; i++)
    {
      node = cpp_lookup (pfile, dtable[i].name, dtable[i].length);
      node->is_directive = 1;
      node->directive_index = i;
    }
}

/* Extract header file from a bracket include. Parsing starts after '<'.
   The string is malloced and must be freed by the caller.  */
char *
_cpp_bracket_include(cpp_reader *pfile)
{
  return glue_header_name (pfile);
}

