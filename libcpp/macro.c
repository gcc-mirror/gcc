/* Part of CPP library.  (Macro and #define handling.)
   Copyright (C) 1986-2018 Free Software Foundation, Inc.
   Written by Per Bothner, 1994.
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
<http://www.gnu.org/licenses/>.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "internal.h"

typedef struct macro_arg macro_arg;
/* This structure represents the tokens of a macro argument.  These
   tokens can be macro themselves, in which case they can be either
   expanded or unexpanded.  When they are expanded, this data
   structure keeps both the expanded and unexpanded forms.  */
struct macro_arg
{
  const cpp_token **first;	/* First token in unexpanded argument.  */
  const cpp_token **expanded;	/* Macro-expanded argument.  */
  const cpp_token *stringified;	/* Stringified argument.  */
  unsigned int count;		/* # of tokens in argument.  */
  unsigned int expanded_count;	/* # of tokens in expanded argument.  */
  source_location *virt_locs;	/* Where virtual locations for
				   unexpanded tokens are stored.  */
  source_location *expanded_virt_locs; /* Where virtual locations for
					  expanded tokens are
					  stored.  */
};

/* The kind of macro tokens which the instance of
   macro_arg_token_iter is supposed to iterate over.  */
enum macro_arg_token_kind {
  MACRO_ARG_TOKEN_NORMAL,
  /* This is a macro argument token that got transformed into a string
     literal, e.g. #foo.  */
  MACRO_ARG_TOKEN_STRINGIFIED,
  /* This is a token resulting from the expansion of a macro
     argument that was itself a macro.  */
  MACRO_ARG_TOKEN_EXPANDED
};

/* An iterator over tokens coming from a function-like macro
   argument.  */
typedef struct macro_arg_token_iter macro_arg_token_iter;
struct macro_arg_token_iter
{
  /* Whether or not -ftrack-macro-expansion is used.  */
  bool track_macro_exp_p;
  /* The kind of token over which we are supposed to iterate.  */
  enum macro_arg_token_kind kind;
  /* A pointer to the current token pointed to by the iterator.  */
  const cpp_token **token_ptr;
  /* A pointer to the "full" location of the current token.  If
     -ftrack-macro-expansion is used this location tracks loci across
     macro expansion.  */
  const source_location *location_ptr;
#if CHECKING_P
  /* The number of times the iterator went forward. This useful only
     when checking is enabled.  */
  size_t num_forwards;
#endif
};

/* Saved data about an identifier being used as a macro argument
   name.  */
struct macro_arg_saved_data {
  /* The canonical (UTF-8) spelling of this identifier.  */
  cpp_hashnode *canonical_node;
  /* The previous value & type of this identifier.  */
  union _cpp_hashnode_value value;
  node_type type;
};

static const char *vaopt_paste_error =
  N_("'##' cannot appear at either end of __VA_OPT__");

/* A class for tracking __VA_OPT__ state while iterating over a
   sequence of tokens.  This is used during both macro definition and
   expansion.  */
class vaopt_state {

 public:

  /* Initialize the state tracker.  ANY_ARGS is true if variable
     arguments were provided to the macro invocation.  */
  vaopt_state (cpp_reader *pfile, bool is_variadic, bool any_args)
    : m_pfile (pfile),
    m_allowed (any_args),
    m_variadic (is_variadic),
    m_last_was_paste (false),
    m_state (0),
    m_paste_location (0),
    m_location (0)
  {
  }

  enum update_type
  {
    ERROR,
    DROP,
    INCLUDE,
    BEGIN,
    END
  };

  /* Given a token, update the state of this tracker and return a
     boolean indicating whether the token should be be included in the
     expansion.  */
  update_type update (const cpp_token *token)
  {
    /* If the macro isn't variadic, just don't bother.  */
    if (!m_variadic)
      return INCLUDE;

    if (token->type == CPP_NAME
	&& token->val.node.node == m_pfile->spec_nodes.n__VA_OPT__)
      {
	if (m_state > 0)
	  {
	    cpp_error_at (m_pfile, CPP_DL_ERROR, token->src_loc,
			  "__VA_OPT__ may not appear in a __VA_OPT__");
	    return ERROR;
	  }
	++m_state;
	m_location = token->src_loc;
	return BEGIN;
      }
    else if (m_state == 1)
      {
	if (token->type != CPP_OPEN_PAREN)
	  {
	    cpp_error_at (m_pfile, CPP_DL_ERROR, m_location,
			  "__VA_OPT__ must be followed by an "
			  "open parenthesis");
	    return ERROR;
	  }
	++m_state;
	return DROP;
      }
    else if (m_state >= 2)
      {
	if (m_state == 2 && token->type == CPP_PASTE)
	  {
	    cpp_error_at (m_pfile, CPP_DL_ERROR, token->src_loc,
			  vaopt_paste_error);
	    return ERROR;
	  }
	/* Advance states before further considering this token, in
	   case we see a close paren immediately after the open
	   paren.  */
	if (m_state == 2)
	  ++m_state;

	bool was_paste = m_last_was_paste;
	m_last_was_paste = false;
	if (token->type == CPP_PASTE)
	  {
	    m_last_was_paste = true;
	    m_paste_location = token->src_loc;
	  }
	else if (token->type == CPP_OPEN_PAREN)
	  ++m_state;
	else if (token->type == CPP_CLOSE_PAREN)
	  {
	    --m_state;
	    if (m_state == 2)
	      {
		/* Saw the final paren.  */
		m_state = 0;

		if (was_paste)
		  {
		    cpp_error_at (m_pfile, CPP_DL_ERROR, token->src_loc,
				  vaopt_paste_error);
		    return ERROR;
		  }

		return END;
	      }
	  }
	return m_allowed ? INCLUDE : DROP;
      }

    /* Nothing to do with __VA_OPT__.  */
    return INCLUDE;
  }

  /* Ensure that any __VA_OPT__ was completed.  If ok, return true.
     Otherwise, issue an error and return false.  */
  bool completed ()
  {
    if (m_variadic && m_state != 0)
      cpp_error_at (m_pfile, CPP_DL_ERROR, m_location,
		    "unterminated __VA_OPT__");
    return m_state == 0;
  }

 private:

  /* The cpp_reader.  */
  cpp_reader *m_pfile;

  /* True if there were varargs.  */
  bool m_allowed;
  /* True if the macro is variadic.  */
  bool m_variadic;
  /* If true, the previous token was ##.  This is used to detect when
     a paste occurs at the end of the sequence.  */
  bool m_last_was_paste;

  /* The state variable:
     0 means not parsing
     1 means __VA_OPT__ seen, looking for "("
     2 means "(" seen (so the next token can't be "##")
     >= 3 means looking for ")", the number encodes the paren depth.  */
  int m_state;

  /* The location of the paste token.  */
  source_location m_paste_location;

  /* Location of the __VA_OPT__ token.  */
  source_location m_location;
};

/* Macro expansion.  */

static int enter_macro_context (cpp_reader *, cpp_hashnode *,
				const cpp_token *, source_location);
static int builtin_macro (cpp_reader *, cpp_hashnode *,
			  source_location, source_location);
static void push_ptoken_context (cpp_reader *, cpp_hashnode *, _cpp_buff *,
				 const cpp_token **, unsigned int);
static void push_extended_tokens_context (cpp_reader *, cpp_hashnode *,
					  _cpp_buff *, source_location *,
					  const cpp_token **, unsigned int);
static _cpp_buff *collect_args (cpp_reader *, const cpp_hashnode *,
				_cpp_buff **, unsigned *);
static cpp_context *next_context (cpp_reader *);
static const cpp_token *padding_token (cpp_reader *, const cpp_token *);
static void expand_arg (cpp_reader *, macro_arg *);
static const cpp_token *new_string_token (cpp_reader *, uchar *, unsigned int);
static const cpp_token *stringify_arg (cpp_reader *, macro_arg *);
static void paste_all_tokens (cpp_reader *, const cpp_token *);
static bool paste_tokens (cpp_reader *, source_location,
			  const cpp_token **, const cpp_token *);
static void alloc_expanded_arg_mem (cpp_reader *, macro_arg *, size_t);
static void ensure_expanded_arg_room (cpp_reader *, macro_arg *, size_t, size_t *);
static void delete_macro_args (_cpp_buff*, unsigned num_args);
static void set_arg_token (macro_arg *, const cpp_token *,
			   source_location, size_t,
			   enum macro_arg_token_kind,
			   bool);
static const source_location *get_arg_token_location (const macro_arg *,
						      enum macro_arg_token_kind);
static const cpp_token **arg_token_ptr_at (const macro_arg *,
					   size_t,
					   enum macro_arg_token_kind,
					   source_location **virt_location);

static void macro_arg_token_iter_init (macro_arg_token_iter *, bool,
				       enum macro_arg_token_kind,
				       const macro_arg *,
				       const cpp_token **);
static const cpp_token *macro_arg_token_iter_get_token
(const macro_arg_token_iter *it);
static source_location macro_arg_token_iter_get_location
(const macro_arg_token_iter *);
static void macro_arg_token_iter_forward (macro_arg_token_iter *);
static _cpp_buff *tokens_buff_new (cpp_reader *, size_t,
				   source_location **);
static size_t tokens_buff_count (_cpp_buff *);
static const cpp_token **tokens_buff_last_token_ptr (_cpp_buff *);
static inline const cpp_token **tokens_buff_put_token_to (const cpp_token **,
                                                          source_location *,
                                                          const cpp_token *,
                                                          source_location,
                                                          source_location,
                                                          const line_map_macro *,
                                                          unsigned int);

static const cpp_token **tokens_buff_add_token (_cpp_buff *,
						source_location *,
						const cpp_token *,
						source_location,
						source_location,
						const line_map_macro *,
						unsigned int);
static inline void tokens_buff_remove_last_token (_cpp_buff *);
static void replace_args (cpp_reader *, cpp_hashnode *, cpp_macro *,
			  macro_arg *, source_location);
static _cpp_buff *funlike_invocation_p (cpp_reader *, cpp_hashnode *,
					_cpp_buff **, unsigned *);
static cpp_macro *create_iso_definition (cpp_reader *);

/* #define directive parsing and handling.  */

static cpp_macro *lex_expansion_token (cpp_reader *, cpp_macro *);
static bool warn_of_redefinition (cpp_reader *, cpp_hashnode *,
				  const cpp_macro *);
static bool parse_params (cpp_reader *, unsigned *, bool *);
static void check_trad_stringification (cpp_reader *, const cpp_macro *,
					const cpp_string *);
static bool reached_end_of_context (cpp_context *);
static void consume_next_token_from_context (cpp_reader *pfile,
					     const cpp_token **,
					     source_location *);
static const cpp_token* cpp_get_token_1 (cpp_reader *, source_location *);

static cpp_hashnode* macro_of_context (cpp_context *context);

static bool in_macro_expansion_p (cpp_reader *pfile);

/* Statistical counter tracking the number of macros that got
   expanded.  */
unsigned num_expanded_macros_counter = 0;
/* Statistical counter tracking the total number tokens resulting
   from macro expansion.  */
unsigned num_macro_tokens_counter = 0;

/* Emits a warning if NODE is a macro defined in the main file that
   has not been used.  */
int
_cpp_warn_if_unused_macro (cpp_reader *pfile, cpp_hashnode *node,
			   void *v ATTRIBUTE_UNUSED)
{
  if (cpp_user_macro_p (node))
    {
      cpp_macro *macro = node->value.macro;

      if (!macro->used
	  && MAIN_FILE_P (linemap_check_ordinary
			    (linemap_lookup (pfile->line_table,
					     macro->line))))
	cpp_warning_with_line (pfile, CPP_W_UNUSED_MACROS, macro->line, 0,
			       "macro \"%s\" is not used", NODE_NAME (node));
    }

  return 1;
}

/* Allocates and returns a CPP_STRING token, containing TEXT of length
   LEN, after null-terminating it.  TEXT must be in permanent storage.  */
static const cpp_token *
new_string_token (cpp_reader *pfile, unsigned char *text, unsigned int len)
{
  cpp_token *token = _cpp_temp_token (pfile);

  text[len] = '\0';
  token->type = CPP_STRING;
  token->val.str.len = len;
  token->val.str.text = text;
  token->flags = 0;
  return token;
}

static const char * const monthnames[] =
{
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

/* Helper function for builtin_macro.  Returns the text generated by
   a builtin macro. */
const uchar *
_cpp_builtin_macro_text (cpp_reader *pfile, cpp_hashnode *node,
			 source_location loc)
{
  const uchar *result = NULL;
  linenum_type number = 1;

  switch (node->value.builtin)
    {
    default:
      cpp_error (pfile, CPP_DL_ICE, "invalid built-in macro \"%s\"",
		 NODE_NAME (node));
      break;

    case BT_TIMESTAMP:
      {
	if (CPP_OPTION (pfile, warn_date_time))
	  cpp_warning (pfile, CPP_W_DATE_TIME, "macro \"%s\" might prevent "
		       "reproducible builds", NODE_NAME (node));

	cpp_buffer *pbuffer = cpp_get_buffer (pfile);
	if (pbuffer->timestamp == NULL)
	  {
	    /* Initialize timestamp value of the assotiated file. */
            struct _cpp_file *file = cpp_get_file (pbuffer);
	    if (file)
	      {
    		/* Generate __TIMESTAMP__ string, that represents 
		   the date and time of the last modification 
		   of the current source file. The string constant 
		   looks like "Sun Sep 16 01:03:52 1973".  */
		struct tm *tb = NULL;
		struct stat *st = _cpp_get_file_stat (file);
		if (st)
		  tb = localtime (&st->st_mtime);
		if (tb)
		  {
		    char *str = asctime (tb);
		    size_t len = strlen (str);
		    unsigned char *buf = _cpp_unaligned_alloc (pfile, len + 2);
		    buf[0] = '"';
		    strcpy ((char *) buf + 1, str);
		    buf[len] = '"';
		    pbuffer->timestamp = buf;
		  }
		else
		  {
		    cpp_errno (pfile, CPP_DL_WARNING,
			"could not determine file timestamp");
		    pbuffer->timestamp = UC"\"??? ??? ?? ??:??:?? ????\"";
		  }
	      }
	  }
	result = pbuffer->timestamp;
      }
      break;
    case BT_FILE:
    case BT_BASE_FILE:
      {
	unsigned int len;
	const char *name;
	uchar *buf;
	
	if (node->value.builtin == BT_FILE)
	  name = linemap_get_expansion_filename (pfile->line_table,
						 pfile->line_table->highest_line);
	else
	  {
	    name = _cpp_get_file_name (pfile->main_file);
	    if (!name)
	      abort ();
	  }
	if (pfile->cb.remap_filename)
	  name = pfile->cb.remap_filename (name);
	len = strlen (name);
	buf = _cpp_unaligned_alloc (pfile, len * 2 + 3);
	result = buf;
	*buf = '"';
	buf = cpp_quote_string (buf + 1, (const unsigned char *) name, len);
	*buf++ = '"';
	*buf = '\0';
      }
      break;

    case BT_INCLUDE_LEVEL:
      /* The line map depth counts the primary source as level 1, but
	 historically __INCLUDE_DEPTH__ has called the primary source
	 level 0.  */
      number = pfile->line_table->depth - 1;
      break;

    case BT_SPECLINE:
      /* If __LINE__ is embedded in a macro, it must expand to the
	 line of the macro's invocation, not its definition.
	 Otherwise things like assert() will not work properly.
	 See WG14 N1911, WG21 N4220 sec 6.5, and PR 61861.  */
      if (CPP_OPTION (pfile, traditional))
	loc = pfile->line_table->highest_line;
      else
	loc = linemap_resolve_location (pfile->line_table, loc,
					LRK_MACRO_EXPANSION_POINT, NULL);
      number = linemap_get_expansion_line (pfile->line_table, loc);
      break;

      /* __STDC__ has the value 1 under normal circumstances.
	 However, if (a) we are in a system header, (b) the option
	 stdc_0_in_system_headers is true (set by target config), and
	 (c) we are not in strictly conforming mode, then it has the
	 value 0.  (b) and (c) are already checked in cpp_init_builtins.  */
    case BT_STDC:
      if (cpp_in_system_header (pfile))
	number = 0;
      else
	number = 1;
      break;

    case BT_DATE:
    case BT_TIME:
      if (CPP_OPTION (pfile, warn_date_time))
	cpp_warning (pfile, CPP_W_DATE_TIME, "macro \"%s\" might prevent "
		     "reproducible builds", NODE_NAME (node));
      if (pfile->date == NULL)
	{
	  /* Allocate __DATE__ and __TIME__ strings from permanent
	     storage.  We only do this once, and don't generate them
	     at init time, because time() and localtime() are very
	     slow on some systems.  */
	  time_t tt;
	  struct tm *tb = NULL;

	  /* Set a reproducible timestamp for __DATE__ and __TIME__ macro
	     if SOURCE_DATE_EPOCH is defined.  */
	  if (pfile->source_date_epoch == (time_t) -2
	      && pfile->cb.get_source_date_epoch != NULL)
	    pfile->source_date_epoch = pfile->cb.get_source_date_epoch (pfile);

	  if (pfile->source_date_epoch >= (time_t) 0)
	    tb = gmtime (&pfile->source_date_epoch);
	  else
	    {
	      /* (time_t) -1 is a legitimate value for "number of seconds
		 since the Epoch", so we have to do a little dance to
		 distinguish that from a genuine error.  */
	      errno = 0;
	      tt = time (NULL);
	      if (tt != (time_t)-1 || errno == 0)
		tb = localtime (&tt);
	    }

	  if (tb)
	    {
	      pfile->date = _cpp_unaligned_alloc (pfile,
						  sizeof ("\"Oct 11 1347\""));
	      sprintf ((char *) pfile->date, "\"%s %2d %4d\"",
		       monthnames[tb->tm_mon], tb->tm_mday,
		       tb->tm_year + 1900);

	      pfile->time = _cpp_unaligned_alloc (pfile,
						  sizeof ("\"12:34:56\""));
	      sprintf ((char *) pfile->time, "\"%02d:%02d:%02d\"",
		       tb->tm_hour, tb->tm_min, tb->tm_sec);
	    }
	  else
	    {
	      cpp_errno (pfile, CPP_DL_WARNING,
			 "could not determine date and time");
		
	      pfile->date = UC"\"??? ?? ????\"";
	      pfile->time = UC"\"??:??:??\"";
	    }
	}

      if (node->value.builtin == BT_DATE)
	result = pfile->date;
      else
	result = pfile->time;
      break;

    case BT_COUNTER:
      if (CPP_OPTION (pfile, directives_only) && pfile->state.in_directive)
	cpp_error (pfile, CPP_DL_ERROR,
	    "__COUNTER__ expanded inside directive with -fdirectives-only");
      number = pfile->counter++;
      break;

    case BT_HAS_ATTRIBUTE:
      number = pfile->cb.has_attribute (pfile);
      break;
    }

  if (result == NULL)
    {
      /* 21 bytes holds all NUL-terminated unsigned 64-bit numbers.  */
      result = _cpp_unaligned_alloc (pfile, 21);
      sprintf ((char *) result, "%u", number);
    }

  return result;      
}

/* Convert builtin macros like __FILE__ to a token and push it on the
   context stack.  Also handles _Pragma, for which a new token may not
   be created.  Returns 1 if it generates a new token context, 0 to
   return the token to the caller.  LOC is the location of the expansion
   point of the macro.  */
static int
builtin_macro (cpp_reader *pfile, cpp_hashnode *node,
	       source_location loc, source_location expand_loc)
{
  const uchar *buf;
  size_t len;
  char *nbuf;

  if (node->value.builtin == BT_PRAGMA)
    {
      /* Don't interpret _Pragma within directives.  The standard is
         not clear on this, but to me this makes most sense.  */
      if (pfile->state.in_directive)
	return 0;

      return _cpp_do__Pragma (pfile, loc);
    }

  buf = _cpp_builtin_macro_text (pfile, node, expand_loc);
  len = ustrlen (buf);
  nbuf = (char *) alloca (len + 1);
  memcpy (nbuf, buf, len);
  nbuf[len]='\n';

  cpp_push_buffer (pfile, (uchar *) nbuf, len, /* from_stage3 */ true);
  _cpp_clean_line (pfile);

  /* Set pfile->cur_token as required by _cpp_lex_direct.  */
  pfile->cur_token = _cpp_temp_token (pfile);
  cpp_token *token = _cpp_lex_direct (pfile);
  /* We should point to the expansion point of the builtin macro.  */
  token->src_loc = loc;
  if (pfile->context->tokens_kind == TOKENS_KIND_EXTENDED)
    {
      /* We are tracking tokens resulting from macro expansion.
	 Create a macro line map and generate a virtual location for
	 the token resulting from the expansion of the built-in
	 macro.  */
      source_location *virt_locs = NULL;
      _cpp_buff *token_buf = tokens_buff_new (pfile, 1, &virt_locs);
      const line_map_macro * map =
	linemap_enter_macro (pfile->line_table, node, loc, 1);
      tokens_buff_add_token (token_buf, virt_locs, token,
			     pfile->line_table->builtin_location,
			     pfile->line_table->builtin_location,
			    map, /*macro_token_index=*/0);
      push_extended_tokens_context (pfile, node, token_buf, virt_locs,
				    (const cpp_token **)token_buf->base,
				    1);
    }
  else
    _cpp_push_token_context (pfile, NULL, token, 1);
  if (pfile->buffer->cur != pfile->buffer->rlimit)
    cpp_error (pfile, CPP_DL_ICE, "invalid built-in macro \"%s\"",
	       NODE_NAME (node));
  _cpp_pop_buffer (pfile);

  return 1;
}

/* Copies SRC, of length LEN, to DEST, adding backslashes before all
   backslashes and double quotes. DEST must be of sufficient size.
   Returns a pointer to the end of the string.  */
uchar *
cpp_quote_string (uchar *dest, const uchar *src, unsigned int len)
{
  while (len--)
    {
      uchar c = *src++;

      switch (c)
	{
	case '\n':
	  /* Naked LF can appear in raw string literals  */
	  c = 'n';
	  /* FALLTHROUGH */

	case '\\':
	case '"':
	  *dest++ = '\\';
	  /* FALLTHROUGH */

	default:
	  *dest++ = c;
	}
    }

  return dest;
}

/* Convert a token sequence ARG to a single string token according to
   the rules of the ISO C #-operator.  */
static const cpp_token *
stringify_arg (cpp_reader *pfile, macro_arg *arg)
{
  unsigned char *dest;
  unsigned int i, escape_it, backslash_count = 0;
  const cpp_token *source = NULL;
  size_t len;

  if (BUFF_ROOM (pfile->u_buff) < 3)
    _cpp_extend_buff (pfile, &pfile->u_buff, 3);
  dest = BUFF_FRONT (pfile->u_buff);
  *dest++ = '"';

  /* Loop, reading in the argument's tokens.  */
  for (i = 0; i < arg->count; i++)
    {
      const cpp_token *token = arg->first[i];

      if (token->type == CPP_PADDING)
	{
	  if (source == NULL
	      || (!(source->flags & PREV_WHITE)
		  && token->val.source == NULL))
	    source = token->val.source;
	  continue;
	}

      escape_it = (token->type == CPP_STRING || token->type == CPP_CHAR
		   || token->type == CPP_WSTRING || token->type == CPP_WCHAR
		   || token->type == CPP_STRING32 || token->type == CPP_CHAR32
		   || token->type == CPP_STRING16 || token->type == CPP_CHAR16
		   || token->type == CPP_UTF8STRING || token->type == CPP_UTF8CHAR
		   || cpp_userdef_string_p (token->type)
		   || cpp_userdef_char_p (token->type));

      /* Room for each char being written in octal, initial space and
	 final quote and NUL.  */
      len = cpp_token_len (token);
      if (escape_it)
	len *= 4;
      len += 3;

      if ((size_t) (BUFF_LIMIT (pfile->u_buff) - dest) < len)
	{
	  size_t len_so_far = dest - BUFF_FRONT (pfile->u_buff);
	  _cpp_extend_buff (pfile, &pfile->u_buff, len);
	  dest = BUFF_FRONT (pfile->u_buff) + len_so_far;
	}

      /* Leading white space?  */
      if (dest - 1 != BUFF_FRONT (pfile->u_buff))
	{
	  if (source == NULL)
	    source = token;
	  if (source->flags & PREV_WHITE)
	    *dest++ = ' ';
	}
      source = NULL;

      if (escape_it)
	{
	  _cpp_buff *buff = _cpp_get_buff (pfile, len);
	  unsigned char *buf = BUFF_FRONT (buff);
	  len = cpp_spell_token (pfile, token, buf, true) - buf;
	  dest = cpp_quote_string (dest, buf, len);
	  _cpp_release_buff (pfile, buff);
	}
      else
	dest = cpp_spell_token (pfile, token, dest, true);

      if (token->type == CPP_OTHER && token->val.str.text[0] == '\\')
	backslash_count++;
      else
	backslash_count = 0;
    }

  /* Ignore the final \ of invalid string literals.  */
  if (backslash_count & 1)
    {
      cpp_error (pfile, CPP_DL_WARNING,
		 "invalid string literal, ignoring final '\\'");
      dest--;
    }

  /* Commit the memory, including NUL, and return the token.  */
  *dest++ = '"';
  len = dest - BUFF_FRONT (pfile->u_buff);
  BUFF_FRONT (pfile->u_buff) = dest + 1;
  return new_string_token (pfile, dest - len, len);
}

/* Try to paste two tokens.  On success, return nonzero.  In any
   case, PLHS is updated to point to the pasted token, which is
   guaranteed to not have the PASTE_LEFT flag set.  LOCATION is
   the virtual location used for error reporting.  */
static bool
paste_tokens (cpp_reader *pfile, source_location location,
	      const cpp_token **plhs, const cpp_token *rhs)
{
  unsigned char *buf, *end, *lhsend;
  cpp_token *lhs;
  unsigned int len;

  len = cpp_token_len (*plhs) + cpp_token_len (rhs) + 1;
  buf = (unsigned char *) alloca (len);
  end = lhsend = cpp_spell_token (pfile, *plhs, buf, true);

  /* Avoid comment headers, since they are still processed in stage 3.
     It is simpler to insert a space here, rather than modifying the
     lexer to ignore comments in some circumstances.  Simply returning
     false doesn't work, since we want to clear the PASTE_LEFT flag.  */
  if ((*plhs)->type == CPP_DIV && rhs->type != CPP_EQ)
    *end++ = ' ';
  /* In one obscure case we might see padding here.  */
  if (rhs->type != CPP_PADDING)
    end = cpp_spell_token (pfile, rhs, end, true);
  *end = '\n';

  cpp_push_buffer (pfile, buf, end - buf, /* from_stage3 */ true);
  _cpp_clean_line (pfile);

  /* Set pfile->cur_token as required by _cpp_lex_direct.  */
  pfile->cur_token = _cpp_temp_token (pfile);
  lhs = _cpp_lex_direct (pfile);
  if (pfile->buffer->cur != pfile->buffer->rlimit)
    {
      source_location saved_loc = lhs->src_loc;

      _cpp_pop_buffer (pfile);
      _cpp_backup_tokens (pfile, 1);
      *lhsend = '\0';

      /* We have to remove the PASTE_LEFT flag from the old lhs, but
	 we want to keep the new location.  */
      *lhs = **plhs;
      *plhs = lhs;
      lhs->src_loc = saved_loc;
      lhs->flags &= ~PASTE_LEFT;

      /* Mandatory error for all apart from assembler.  */
      if (CPP_OPTION (pfile, lang) != CLK_ASM)
	cpp_error_with_line (pfile, CPP_DL_ERROR, location, 0,
	 "pasting \"%s\" and \"%s\" does not give a valid preprocessing token",
		   buf, cpp_token_as_text (pfile, rhs));
      return false;
    }

  *plhs = lhs;
  _cpp_pop_buffer (pfile);
  return true;
}

/* Handles an arbitrarily long sequence of ## operators, with initial
   operand LHS.  This implementation is left-associative,
   non-recursive, and finishes a paste before handling succeeding
   ones.  If a paste fails, we back up to the RHS of the failing ##
   operator before pushing the context containing the result of prior
   successful pastes, with the effect that the RHS appears in the
   output stream after the pasted LHS normally.  */
static void
paste_all_tokens (cpp_reader *pfile, const cpp_token *lhs)
{
  const cpp_token *rhs = NULL;
  cpp_context *context = pfile->context;
  source_location virt_loc = 0;

  /* We are expanding a macro and we must have been called on a token
     that appears at the left hand side of a ## operator.  */
  if (macro_of_context (pfile->context) == NULL
      || (!(lhs->flags & PASTE_LEFT)))
    abort ();

  if (context->tokens_kind == TOKENS_KIND_EXTENDED)
    /* The caller must have called consume_next_token_from_context
       right before calling us.  That has incremented the pointer to
       the current virtual location.  So it now points to the location
       of the token that comes right after *LHS.  We want the
       resulting pasted token to have the location of the current
       *LHS, though.  */
    virt_loc = context->c.mc->cur_virt_loc[-1];
  else
    /* We are not tracking macro expansion.  So the best virtual
       location we can get here is the expansion point of the macro we
       are currently expanding.  */
    virt_loc = pfile->invocation_location;

  do
    {
      /* Take the token directly from the current context.  We can do
	 this, because we are in the replacement list of either an
	 object-like macro, or a function-like macro with arguments
	 inserted.  In either case, the constraints to #define
	 guarantee we have at least one more token.  */
      if (context->tokens_kind == TOKENS_KIND_DIRECT)
	rhs = FIRST (context).token++;
      else if (context->tokens_kind == TOKENS_KIND_INDIRECT)
	rhs = *FIRST (context).ptoken++;
      else if (context->tokens_kind == TOKENS_KIND_EXTENDED)
	{
	  /* So we are in presence of an extended token context, which
	     means that each token in this context has a virtual
	     location attached to it.  So let's not forget to update
	     the pointer to the current virtual location of the
	     current token when we update the pointer to the current
	     token */

	  rhs = *FIRST (context).ptoken++;
	  /* context->c.mc must be non-null, as if we were not in a
	     macro context, context->tokens_kind could not be equal to
	     TOKENS_KIND_EXTENDED.  */
	  context->c.mc->cur_virt_loc++;
	}

      if (rhs->type == CPP_PADDING)
	{
	  if (rhs->flags & PASTE_LEFT)
	    abort ();
	}
      if (!paste_tokens (pfile, virt_loc, &lhs, rhs))
	break;
    }
  while (rhs->flags & PASTE_LEFT);

  /* Put the resulting token in its own context.  */
  if (context->tokens_kind == TOKENS_KIND_EXTENDED)
    {
      source_location *virt_locs = NULL;
      _cpp_buff *token_buf = tokens_buff_new (pfile, 1, &virt_locs);
      tokens_buff_add_token (token_buf, virt_locs, lhs,
			     virt_loc, 0, NULL, 0);
      push_extended_tokens_context (pfile, context->c.mc->macro_node,
				    token_buf, virt_locs,
				    (const cpp_token **)token_buf->base, 1);
    }
  else
    _cpp_push_token_context (pfile, NULL, lhs, 1);
}

/* Returns TRUE if the number of arguments ARGC supplied in an
   invocation of the MACRO referenced by NODE is valid.  An empty
   invocation to a macro with no parameters should pass ARGC as zero.

   Note that MACRO cannot necessarily be deduced from NODE, in case
   NODE was redefined whilst collecting arguments.  */
bool
_cpp_arguments_ok (cpp_reader *pfile, cpp_macro *macro, const cpp_hashnode *node, unsigned int argc)
{
  if (argc == macro->paramc)
    return true;

  if (argc < macro->paramc)
    {
      /* In C++2a (here the va_opt flag is used), and also as a GNU
	 extension, variadic arguments are allowed to not appear in
	 the invocation at all.
	 e.g. #define debug(format, args...) something
	 debug("string");

	 This is exactly the same as if an empty variadic list had been
	 supplied - debug("string", ).  */

      if (argc + 1 == macro->paramc && macro->variadic)
	{
	  if (CPP_PEDANTIC (pfile) && ! macro->syshdr
	      && ! CPP_OPTION (pfile, va_opt))
	    {
	      if (CPP_OPTION (pfile, cplusplus))
		cpp_error (pfile, CPP_DL_PEDWARN,
			   "ISO C++11 requires at least one argument "
			   "for the \"...\" in a variadic macro");
	      else
		cpp_error (pfile, CPP_DL_PEDWARN,
			   "ISO C99 requires at least one argument "
			   "for the \"...\" in a variadic macro");
	    }
	  return true;
	}

      cpp_error (pfile, CPP_DL_ERROR,
		 "macro \"%s\" requires %u arguments, but only %u given",
		 NODE_NAME (node), macro->paramc, argc);
    }
  else
    cpp_error (pfile, CPP_DL_ERROR,
	       "macro \"%s\" passed %u arguments, but takes just %u",
	       NODE_NAME (node), argc, macro->paramc);

  return false;
}

/* Reads and returns the arguments to a function-like macro
   invocation.  Assumes the opening parenthesis has been processed.
   If there is an error, emits an appropriate diagnostic and returns
   NULL.  Each argument is terminated by a CPP_EOF token, for the
   future benefit of expand_arg().  If there are any deferred
   #pragma directives among macro arguments, store pointers to the
   CPP_PRAGMA ... CPP_PRAGMA_EOL tokens into *PRAGMA_BUFF buffer.

   What is returned is the buffer that contains the memory allocated
   to hold the macro arguments.  NODE is the name of the macro this
   function is dealing with.  If NUM_ARGS is non-NULL, *NUM_ARGS is
   set to the actual number of macro arguments allocated in the
   returned buffer.  */
static _cpp_buff *
collect_args (cpp_reader *pfile, const cpp_hashnode *node,
	      _cpp_buff **pragma_buff, unsigned *num_args)
{
  _cpp_buff *buff, *base_buff;
  cpp_macro *macro;
  macro_arg *args, *arg;
  const cpp_token *token;
  unsigned int argc;
  source_location virt_loc;
  bool track_macro_expansion_p = CPP_OPTION (pfile, track_macro_expansion);
  unsigned num_args_alloced = 0;

  macro = node->value.macro;
  if (macro->paramc)
    argc = macro->paramc;
  else
    argc = 1;

#define DEFAULT_NUM_TOKENS_PER_MACRO_ARG 50
#define ARG_TOKENS_EXTENT 1000

  buff = _cpp_get_buff (pfile, argc * (DEFAULT_NUM_TOKENS_PER_MACRO_ARG
				       * sizeof (cpp_token *)
				       + sizeof (macro_arg)));
  base_buff = buff;
  args = (macro_arg *) buff->base;
  memset (args, 0, argc * sizeof (macro_arg));
  buff->cur = (unsigned char *) &args[argc];
  arg = args, argc = 0;

  /* Collect the tokens making up each argument.  We don't yet know
     how many arguments have been supplied, whether too many or too
     few.  Hence the slightly bizarre usage of "argc" and "arg".  */
  do
    {
      unsigned int paren_depth = 0;
      unsigned int ntokens = 0;
      unsigned virt_locs_capacity = DEFAULT_NUM_TOKENS_PER_MACRO_ARG;
      num_args_alloced++;

      argc++;
      arg->first = (const cpp_token **) buff->cur;
      if (track_macro_expansion_p)
	{
	  virt_locs_capacity = DEFAULT_NUM_TOKENS_PER_MACRO_ARG;
	  arg->virt_locs = XNEWVEC (source_location,
				    virt_locs_capacity);
	}

      for (;;)
	{
	  /* Require space for 2 new tokens (including a CPP_EOF).  */
	  if ((unsigned char *) &arg->first[ntokens + 2] > buff->limit)
	    {
	      buff = _cpp_append_extend_buff (pfile, buff,
					      ARG_TOKENS_EXTENT
					      * sizeof (cpp_token *));
	      arg->first = (const cpp_token **) buff->cur;
	    }
	  if (track_macro_expansion_p
	      && (ntokens + 2 > virt_locs_capacity))
	    {
	      virt_locs_capacity += ARG_TOKENS_EXTENT;
	      arg->virt_locs = XRESIZEVEC (source_location,
					   arg->virt_locs,
					   virt_locs_capacity);
	    }

	  token = cpp_get_token_1 (pfile, &virt_loc);

	  if (token->type == CPP_PADDING)
	    {
	      /* Drop leading padding.  */
	      if (ntokens == 0)
		continue;
	    }
	  else if (token->type == CPP_OPEN_PAREN)
	    paren_depth++;
	  else if (token->type == CPP_CLOSE_PAREN)
	    {
	      if (paren_depth-- == 0)
		break;
	    }
	  else if (token->type == CPP_COMMA)
	    {
	      /* A comma does not terminate an argument within
		 parentheses or as part of a variable argument.  */
	      if (paren_depth == 0
		  && ! (macro->variadic && argc == macro->paramc))
		break;
	    }
	  else if (token->type == CPP_EOF
		   || (token->type == CPP_HASH && token->flags & BOL))
	    break;
	  else if (token->type == CPP_PRAGMA)
	    {
	      cpp_token *newtok = _cpp_temp_token (pfile);

	      /* CPP_PRAGMA token lives in directive_result, which will
		 be overwritten on the next directive.  */
	      *newtok = *token;
	      token = newtok;
	      do
		{
		  if (*pragma_buff == NULL
		      || BUFF_ROOM (*pragma_buff) < sizeof (cpp_token *))
		    {
		      _cpp_buff *next;
		      if (*pragma_buff == NULL)
			*pragma_buff
			  = _cpp_get_buff (pfile, 32 * sizeof (cpp_token *));
		      else
			{
			  next = *pragma_buff;
			  *pragma_buff
			    = _cpp_get_buff (pfile,
					     (BUFF_FRONT (*pragma_buff)
					      - (*pragma_buff)->base) * 2);
			  (*pragma_buff)->next = next;
			}
		    }
		  *(const cpp_token **) BUFF_FRONT (*pragma_buff) = token;
		  BUFF_FRONT (*pragma_buff) += sizeof (cpp_token *);
		  if (token->type == CPP_PRAGMA_EOL)
		    break;
		  token = cpp_get_token_1 (pfile, &virt_loc);
		}
	      while (token->type != CPP_EOF);

	      /* In deferred pragmas parsing_args and prevent_expansion
		 had been changed, reset it.  */
	      pfile->state.parsing_args = 2;
	      pfile->state.prevent_expansion = 1;

	      if (token->type == CPP_EOF)
		break;
	      else
		continue;
	    }
	  set_arg_token (arg, token, virt_loc,
			 ntokens, MACRO_ARG_TOKEN_NORMAL,
			 CPP_OPTION (pfile, track_macro_expansion));
	  ntokens++;
	}

      /* Drop trailing padding.  */
      while (ntokens > 0 && arg->first[ntokens - 1]->type == CPP_PADDING)
	ntokens--;

      arg->count = ntokens;
      set_arg_token (arg, &pfile->eof, pfile->eof.src_loc,
		     ntokens, MACRO_ARG_TOKEN_NORMAL,
		     CPP_OPTION (pfile, track_macro_expansion));

      /* Terminate the argument.  Excess arguments loop back and
	 overwrite the final legitimate argument, before failing.  */
      if (argc <= macro->paramc)
	{
	  buff->cur = (unsigned char *) &arg->first[ntokens + 1];
	  if (argc != macro->paramc)
	    arg++;
	}
    }
  while (token->type != CPP_CLOSE_PAREN && token->type != CPP_EOF);

  if (token->type == CPP_EOF)
    {
      /* We still need the CPP_EOF to end directives, and to end
	 pre-expansion of a macro argument.  Step back is not
	 unconditional, since we don't want to return a CPP_EOF to our
	 callers at the end of an -include-d file.  */
      if (pfile->context->prev || pfile->state.in_directive)
	_cpp_backup_tokens (pfile, 1);
      cpp_error (pfile, CPP_DL_ERROR,
		 "unterminated argument list invoking macro \"%s\"",
		 NODE_NAME (node));
    }
  else
    {
      /* A single empty argument is counted as no argument.  */
      if (argc == 1 && macro->paramc == 0 && args[0].count == 0)
	argc = 0;
      if (_cpp_arguments_ok (pfile, macro, node, argc))
	{
	  /* GCC has special semantics for , ## b where b is a varargs
	     parameter: we remove the comma if b was omitted entirely.
	     If b was merely an empty argument, the comma is retained.
	     If the macro takes just one (varargs) parameter, then we
	     retain the comma only if we are standards conforming.

	     If FIRST is NULL replace_args () swallows the comma.  */
	  if (macro->variadic && (argc < macro->paramc
				  || (argc == 1 && args[0].count == 0
				      && !CPP_OPTION (pfile, std))))
	    args[macro->paramc - 1].first = NULL;
	  if (num_args)
	    *num_args = num_args_alloced;
	  return base_buff;
	}
    }

  /* An error occurred.  */
  _cpp_release_buff (pfile, base_buff);
  return NULL;
}

/* Search for an opening parenthesis to the macro of NODE, in such a
   way that, if none is found, we don't lose the information in any
   intervening padding tokens.  If we find the parenthesis, collect
   the arguments and return the buffer containing them.  PRAGMA_BUFF
   argument is the same as in collect_args.  If NUM_ARGS is non-NULL,
   *NUM_ARGS is set to the number of arguments contained in the
   returned buffer.  */
static _cpp_buff *
funlike_invocation_p (cpp_reader *pfile, cpp_hashnode *node,
		      _cpp_buff **pragma_buff, unsigned *num_args)
{
  const cpp_token *token, *padding = NULL;

  for (;;)
    {
      token = cpp_get_token (pfile);
      if (token->type != CPP_PADDING)
	break;
      if (padding == NULL
	  || (!(padding->flags & PREV_WHITE) && token->val.source == NULL))
	padding = token;
    }

  if (token->type == CPP_OPEN_PAREN)
    {
      pfile->state.parsing_args = 2;
      return collect_args (pfile, node, pragma_buff, num_args);
    }

  /* CPP_EOF can be the end of macro arguments, or the end of the
     file.  We mustn't back up over the latter.  Ugh.  */
  if (token->type != CPP_EOF || token == &pfile->eof)
    {
      /* Back up.  We may have skipped padding, in which case backing
	 up more than one token when expanding macros is in general
	 too difficult.  We re-insert it in its own context.  */
      _cpp_backup_tokens (pfile, 1);
      if (padding)
	_cpp_push_token_context (pfile, NULL, padding, 1);
    }

  return NULL;
}

/* Return the real number of tokens in the expansion of MACRO.  */
static inline unsigned int
macro_real_token_count (const cpp_macro *macro)
{
  if (__builtin_expect (!macro->extra_tokens, true))
    return macro->count;

  for (unsigned i = macro->count; i--;)
    if (macro->exp.tokens[i].type != CPP_PASTE)
      return i + 1;

  return 0;
}

/* Push the context of a macro with hash entry NODE onto the context
   stack.  If we can successfully expand the macro, we push a context
   containing its yet-to-be-rescanned replacement list and return one.
   If there were additionally any unexpanded deferred #pragma
   directives among macro arguments, push another context containing
   the pragma tokens before the yet-to-be-rescanned replacement list
   and return two.  Otherwise, we don't push a context and return
   zero. LOCATION is the location of the expansion point of the
   macro.  */
static int
enter_macro_context (cpp_reader *pfile, cpp_hashnode *node,
		     const cpp_token *result, source_location location)
{
  /* The presence of a macro invalidates a file's controlling macro.  */
  pfile->mi_valid = false;

  pfile->state.angled_headers = false;

  /* From here to when we push the context for the macro later down
     this function, we need to flag the fact that we are about to
     expand a macro.  This is useful when -ftrack-macro-expansion is
     turned off.  In that case, we need to record the location of the
     expansion point of the top-most macro we are about to to expand,
     into pfile->invocation_location.  But we must not record any such
     location once the process of expanding the macro starts; that is,
     we must not do that recording between now and later down this
     function where set this flag to FALSE.  */
  pfile->about_to_expand_macro_p = true;

  if (cpp_user_macro_p (node))
    {
      cpp_macro *macro = node->value.macro;
      _cpp_buff *pragma_buff = NULL;

      if (macro->fun_like)
	{
	  _cpp_buff *buff;
	  unsigned num_args = 0;

	  pfile->state.prevent_expansion++;
	  pfile->keep_tokens++;
	  pfile->state.parsing_args = 1;
	  buff = funlike_invocation_p (pfile, node, &pragma_buff,
				       &num_args);
	  pfile->state.parsing_args = 0;
	  pfile->keep_tokens--;
	  pfile->state.prevent_expansion--;

	  if (buff == NULL)
	    {
	      if (CPP_WTRADITIONAL (pfile) && ! node->value.macro->syshdr)
		cpp_warning (pfile, CPP_W_TRADITIONAL,
 "function-like macro \"%s\" must be used with arguments in traditional C",
			     NODE_NAME (node));

	      if (pragma_buff)
		_cpp_release_buff (pfile, pragma_buff);

	      pfile->about_to_expand_macro_p = false;
	      return 0;
	    }

	  if (macro->paramc > 0)
	    replace_args (pfile, node, macro,
			  (macro_arg *) buff->base,
			  location);
	  /* Free the memory used by the arguments of this
	     function-like macro.  This memory has been allocated by
	     funlike_invocation_p and by replace_args.  */
	  delete_macro_args (buff, num_args);
	}

      /* Disable the macro within its expansion.  */
      node->flags |= NODE_DISABLED;

      /* Laziness can only affect the expansion tokens of the macro,
	 not its fun-likeness or parameters.  */
      _cpp_maybe_notify_macro_use (pfile, node);
      if (pfile->cb.used)
	pfile->cb.used (pfile, location, node);

      macro->used = 1;

      if (macro->paramc == 0)
	{
	  unsigned tokens_count = macro_real_token_count (macro);
	  if (CPP_OPTION (pfile, track_macro_expansion))
	    {
	      unsigned int i;
	      const cpp_token *src = macro->exp.tokens;
	      const line_map_macro *map;
	      source_location *virt_locs = NULL;
	      _cpp_buff *macro_tokens
		= tokens_buff_new (pfile, tokens_count, &virt_locs);

	      /* Create a macro map to record the locations of the
		 tokens that are involved in the expansion. LOCATION
		 is the location of the macro expansion point.  */
	      map = linemap_enter_macro (pfile->line_table,
					 node, location, tokens_count);
	      for (i = 0; i < tokens_count; ++i)
		{
		  tokens_buff_add_token (macro_tokens, virt_locs,
					 src, src->src_loc,
					 src->src_loc, map, i);
		  ++src;
		}
	      push_extended_tokens_context (pfile, node,
					    macro_tokens,
					    virt_locs,
					    (const cpp_token **)
					    macro_tokens->base,
					    tokens_count);
	    }
	  else
	    _cpp_push_token_context (pfile, node, macro->exp.tokens,
				     tokens_count);
	  num_macro_tokens_counter += tokens_count;
	}

      if (pragma_buff)
	{
	  if (!pfile->state.in_directive)
	    _cpp_push_token_context (pfile, NULL,
				     padding_token (pfile, result), 1);
	  do
	    {
	      unsigned tokens_count;
	      _cpp_buff *tail = pragma_buff->next;
	      pragma_buff->next = NULL;
	      tokens_count = ((const cpp_token **) BUFF_FRONT (pragma_buff)
			      - (const cpp_token **) pragma_buff->base);
	      push_ptoken_context (pfile, NULL, pragma_buff,
				   (const cpp_token **) pragma_buff->base,
				   tokens_count);
	      pragma_buff = tail;
	      if (!CPP_OPTION (pfile, track_macro_expansion))
		num_macro_tokens_counter += tokens_count;

	    }
	  while (pragma_buff != NULL);
	  pfile->about_to_expand_macro_p = false;
	  return 2;
	}

      pfile->about_to_expand_macro_p = false;
      return 1;
    }

  pfile->about_to_expand_macro_p = false;
  /* Handle built-in macros and the _Pragma operator.  */
  {
    source_location expand_loc;

    if (/* The top-level macro invocation that triggered the expansion
	   we are looking at is with a function-like user macro ...  */
	cpp_fun_like_macro_p (pfile->top_most_macro_node)
	/* ... and we are tracking the macro expansion.  */
	&& CPP_OPTION (pfile, track_macro_expansion))
      /* Then the location of the end of the macro invocation is the
	 location of the expansion point of this macro.  */
      expand_loc = location;
    else
      /* Otherwise, the location of the end of the macro invocation is
	 the location of the expansion point of that top-level macro
	 invocation.  */
      expand_loc = pfile->invocation_location;

    return builtin_macro (pfile, node, location, expand_loc);
  }
}

/* De-allocate the memory used by BUFF which is an array of instances
   of macro_arg.  NUM_ARGS is the number of instances of macro_arg
   present in BUFF.  */
static void
delete_macro_args (_cpp_buff *buff, unsigned num_args)
{
  macro_arg *macro_args;
  unsigned i;

  if (buff == NULL)
    return;

  macro_args = (macro_arg *) buff->base;

  /* Walk instances of macro_arg to free their expanded tokens as well
     as their macro_arg::virt_locs members.  */
  for (i = 0; i < num_args; ++i)
    {
      if (macro_args[i].expanded)
	{
	  free (macro_args[i].expanded);
	  macro_args[i].expanded = NULL;
	}
      if (macro_args[i].virt_locs)
	{
	  free (macro_args[i].virt_locs);
	  macro_args[i].virt_locs = NULL;
	}
      if (macro_args[i].expanded_virt_locs)
	{
	  free (macro_args[i].expanded_virt_locs);
	  macro_args[i].expanded_virt_locs = NULL;
	}
    }
  _cpp_free_buff (buff);
}

/* Set the INDEXth token of the macro argument ARG. TOKEN is the token
   to set, LOCATION is its virtual location.  "Virtual" location means
   the location that encodes loci across macro expansion. Otherwise
   it has to be TOKEN->SRC_LOC.  KIND is the kind of tokens the
   argument ARG is supposed to contain.  Note that ARG must be
   tailored so that it has enough room to contain INDEX + 1 numbers of
   tokens, at least.  */
static void
set_arg_token (macro_arg *arg, const cpp_token *token,
	       source_location location, size_t index,
	       enum macro_arg_token_kind kind,
	       bool track_macro_exp_p)
{
  const cpp_token **token_ptr;
  source_location *loc = NULL;

  token_ptr =
    arg_token_ptr_at (arg, index, kind,
		      track_macro_exp_p ? &loc : NULL);
  *token_ptr = token;

  if (loc != NULL)
    {
      /* We can't set the location of a stringified argument
	 token and we can't set any location if we aren't tracking
	 macro expansion locations.   */
      gcc_checking_assert (kind != MACRO_ARG_TOKEN_STRINGIFIED
			   && track_macro_exp_p);
      *loc = location;
    }
}

/* Get the pointer to the location of the argument token of the
   function-like macro argument ARG.  This function must be called
   only when we -ftrack-macro-expansion is on.  */
static const source_location *
get_arg_token_location (const macro_arg *arg,
			enum macro_arg_token_kind kind)
{
  const source_location *loc = NULL;
  const cpp_token **token_ptr =
    arg_token_ptr_at (arg, 0, kind, (source_location **) &loc);

  if (token_ptr == NULL)
    return NULL;

  return loc;
}

/* Return the pointer to the INDEXth token of the macro argument ARG.
   KIND specifies the kind of token the macro argument ARG contains.
   If VIRT_LOCATION is non NULL, *VIRT_LOCATION is set to the address
   of the virtual location of the returned token if the
   -ftrack-macro-expansion flag is on; otherwise, it's set to the
   spelling location of the returned token.  */
static const cpp_token **
arg_token_ptr_at (const macro_arg *arg, size_t index,
		  enum macro_arg_token_kind kind,
		  source_location **virt_location)
{
  const cpp_token **tokens_ptr = NULL;

  switch (kind)
    {
    case MACRO_ARG_TOKEN_NORMAL:
      tokens_ptr = arg->first;
      break;
    case MACRO_ARG_TOKEN_STRINGIFIED:      
      tokens_ptr = (const cpp_token **) &arg->stringified;
      break;
    case MACRO_ARG_TOKEN_EXPANDED:
	tokens_ptr = arg->expanded;
      break;
    }

  if (tokens_ptr == NULL)
    /* This can happen for e.g, an empty token argument to a
       funtion-like macro.  */
    return tokens_ptr;

  if (virt_location)
    {
      if (kind == MACRO_ARG_TOKEN_NORMAL)
	*virt_location = &arg->virt_locs[index];
      else if (kind == MACRO_ARG_TOKEN_EXPANDED)
	*virt_location = &arg->expanded_virt_locs[index];
      else if (kind == MACRO_ARG_TOKEN_STRINGIFIED)
	*virt_location =
	  (source_location *) &tokens_ptr[index]->src_loc;
    }
  return &tokens_ptr[index];
}

/* Initialize an iterator so that it iterates over the tokens of a
   function-like macro argument.  KIND is the kind of tokens we want
   ITER to iterate over. TOKEN_PTR points the first token ITER will
   iterate over.  */
static void
macro_arg_token_iter_init (macro_arg_token_iter *iter,
			   bool track_macro_exp_p,
			   enum macro_arg_token_kind kind,
			   const macro_arg *arg,
			   const cpp_token **token_ptr)
{
  iter->track_macro_exp_p = track_macro_exp_p;
  iter->kind = kind;
  iter->token_ptr = token_ptr;
  /* Unconditionally initialize this so that the compiler doesn't warn
     about iter->location_ptr being possibly uninitialized later after
     this code has been inlined somewhere.  */
  iter->location_ptr = NULL;
  if (track_macro_exp_p)
    iter->location_ptr = get_arg_token_location (arg, kind);
#if CHECKING_P
  iter->num_forwards = 0;
  if (track_macro_exp_p
      && token_ptr != NULL
      && iter->location_ptr == NULL)
    abort ();
#endif
}

/* Move the iterator one token forward. Note that if IT was
   initialized on an argument that has a stringified token, moving it
   forward doesn't make sense as a stringified token is essentially one
   string.  */
static void
macro_arg_token_iter_forward (macro_arg_token_iter *it)
{
  switch (it->kind)
    {
    case MACRO_ARG_TOKEN_NORMAL:
    case MACRO_ARG_TOKEN_EXPANDED:
      it->token_ptr++;
      if (it->track_macro_exp_p)
	it->location_ptr++;
      break;
    case MACRO_ARG_TOKEN_STRINGIFIED:
#if CHECKING_P
      if (it->num_forwards > 0)
	abort ();
#endif
      break;
    }

#if CHECKING_P
  it->num_forwards++;
#endif
}

/* Return the token pointed to by the iterator.  */
static const cpp_token *
macro_arg_token_iter_get_token (const macro_arg_token_iter *it)
{
#if CHECKING_P
  if (it->kind == MACRO_ARG_TOKEN_STRINGIFIED
      && it->num_forwards > 0)
    abort ();
#endif
  if (it->token_ptr == NULL)
    return NULL;
  return *it->token_ptr;
}

/* Return the location of the token pointed to by the iterator.*/
static source_location
macro_arg_token_iter_get_location (const macro_arg_token_iter *it)
{
#if CHECKING_P
  if (it->kind == MACRO_ARG_TOKEN_STRINGIFIED
      && it->num_forwards > 0)
    abort ();
#endif
  if (it->track_macro_exp_p)
    return *it->location_ptr;
  else
    return (*it->token_ptr)->src_loc;
}

/* Return the index of a token [resulting from macro expansion] inside
   the total list of tokens resulting from a given macro
   expansion. The index can be different depending on whether if we
   want each tokens resulting from function-like macro arguments
   expansion to have a different location or not.

   E.g, consider this function-like macro: 

        #define M(x) x - 3

   Then consider us "calling" it (and thus expanding it) like:
   
       M(1+4)

   It will be expanded into:

       1+4-3

   Let's consider the case of the token '4'.

   Its index can be 2 (it's the third token of the set of tokens
   resulting from the expansion) or it can be 0 if we consider that
   all tokens resulting from the expansion of the argument "1+2" have
   the same index, which is 0. In this later case, the index of token
   '-' would then be 1 and the index of token '3' would be 2.

   The later case is useful to use less memory e.g, for the case of
   the user using the option -ftrack-macro-expansion=1.

   ABSOLUTE_TOKEN_INDEX is the index of the macro argument token we
   are interested in.  CUR_REPLACEMENT_TOKEN is the token of the macro
   parameter (inside the macro replacement list) that corresponds to
   the macro argument for which ABSOLUTE_TOKEN_INDEX is a token index
   of.

   If we refer to the example above, for the '4' argument token,
   ABSOLUTE_TOKEN_INDEX would be set to 2, and CUR_REPLACEMENT_TOKEN
   would be set to the token 'x', in the replacement list "x - 3" of
   macro M.

   This is a subroutine of replace_args.  */
inline static unsigned
expanded_token_index (cpp_reader *pfile, cpp_macro *macro,
		      const cpp_token *cur_replacement_token,
		      unsigned absolute_token_index)
{
  if (CPP_OPTION (pfile, track_macro_expansion) > 1)
    return absolute_token_index;
  return cur_replacement_token - macro->exp.tokens;
}

/* Copy whether PASTE_LEFT is set from SRC to *PASTE_FLAG.  */

static void
copy_paste_flag (cpp_reader *pfile, const cpp_token **paste_flag,
		 const cpp_token *src)
{
  cpp_token *token = _cpp_temp_token (pfile);
  token->type = (*paste_flag)->type;
  token->val = (*paste_flag)->val;
  if (src->flags & PASTE_LEFT)
    token->flags = (*paste_flag)->flags | PASTE_LEFT;
  else
    token->flags = (*paste_flag)->flags & ~PASTE_LEFT;
  *paste_flag = token;
}

/* True IFF the last token emitted into BUFF (if any) is PTR.  */

static bool
last_token_is (_cpp_buff *buff, const cpp_token **ptr)
{
  return (ptr && tokens_buff_last_token_ptr (buff) == ptr);
}

/* Replace the parameters in a function-like macro of NODE with the
   actual ARGS, and place the result in a newly pushed token context.
   Expand each argument before replacing, unless it is operated upon
   by the # or ## operators. EXPANSION_POINT_LOC is the location of
   the expansion point of the macro. E.g, the location of the
   function-like macro invocation.  */
static void
replace_args (cpp_reader *pfile, cpp_hashnode *node, cpp_macro *macro,
	      macro_arg *args, source_location expansion_point_loc)
{
  unsigned int i, total;
  const cpp_token *src, *limit;
  const cpp_token **first = NULL;
  macro_arg *arg;
  _cpp_buff *buff = NULL;
  source_location *virt_locs = NULL;
  unsigned int exp_count;
  const line_map_macro *map = NULL;
  int track_macro_exp;

  /* First, fully macro-expand arguments, calculating the number of
     tokens in the final expansion as we go.  The ordering of the if
     statements below is subtle; we must handle stringification before
     pasting.  */

  /* EXP_COUNT is the number of tokens in the macro replacement
     list.  TOTAL is the number of tokens /after/ macro parameters
     have been replaced by their arguments.   */
  exp_count = macro_real_token_count (macro);
  total = exp_count;
  limit = macro->exp.tokens + exp_count;

  for (src = macro->exp.tokens; src < limit; src++)
    if (src->type == CPP_MACRO_ARG)
      {
	/* Leading and trailing padding tokens.  */
	total += 2;
	/* Account for leading and padding tokens in exp_count too.
	   This is going to be important later down this function,
	   when we want to handle the case of (track_macro_exp <
	   2).  */
	exp_count += 2;

	/* We have an argument.  If it is not being stringified or
	   pasted it is macro-replaced before insertion.  */
	arg = &args[src->val.macro_arg.arg_no - 1];

	if (src->flags & STRINGIFY_ARG)
	  {
	    if (!arg->stringified)
	      arg->stringified = stringify_arg (pfile, arg);
	  }
	else if ((src->flags & PASTE_LEFT)
		 || (src != macro->exp.tokens && (src[-1].flags & PASTE_LEFT)))
	  total += arg->count - 1;
	else
	  {
	    if (!arg->expanded)
	      expand_arg (pfile, arg);
	    total += arg->expanded_count - 1;
	  }
      }

  /* When the compiler is called with the -ftrack-macro-expansion
     flag, we need to keep track of the location of each token that
     results from macro expansion.

     A token resulting from macro expansion is not a new token. It is
     simply the same token as the token coming from the macro
     definition.  The new things that are allocated are the buffer
     that holds the tokens resulting from macro expansion and a new
     location that records many things like the locus of the expansion
     point as well as the original locus inside the definition of the
     macro.  This location is called a virtual location.
     
     So the buffer BUFF holds a set of cpp_token*, and the buffer
     VIRT_LOCS holds the virtual locations of the tokens held by BUFF.

     Both of these two buffers are going to be hung off of the macro
     context, when the latter is pushed.  The memory allocated to
     store the tokens and their locations is going to be freed once
     the context of macro expansion is popped.
     
     As far as tokens are concerned, the memory overhead of
     -ftrack-macro-expansion is proportional to the number of
     macros that get expanded multiplied by sizeof (source_location).
     The good news is that extra memory gets freed when the macro
     context is freed, i.e shortly after the macro got expanded.  */

  /* Is the -ftrack-macro-expansion flag in effect?  */
  track_macro_exp = CPP_OPTION (pfile, track_macro_expansion);

  /* Now allocate memory space for tokens and locations resulting from
     the macro expansion, copy the tokens and replace the arguments.
     This memory must be freed when the context of the macro MACRO is
     popped.  */
  buff = tokens_buff_new (pfile, total, track_macro_exp ? &virt_locs : NULL);

  first = (const cpp_token **) buff->base;

  /* Create a macro map to record the locations of the tokens that are
     involved in the expansion.  Note that the expansion point is set
     to the location of the closing parenthesis.  Otherwise, the
     subsequent map created for the first token that comes after the
     macro map might have a wrong line number.  That would lead to
     tokens with wrong line numbers after the macro expansion.  This
     adds up to the memory overhead of the -ftrack-macro-expansion
     flag; for every macro that is expanded, a "macro map" is
     created.  */
  if (track_macro_exp)
    {
      int num_macro_tokens = total;
      if (track_macro_exp < 2)
	/* Then the number of macro tokens won't take in account the
	   fact that function-like macro arguments can expand to
	   multiple tokens. This is to save memory at the expense of
	   accuracy.

	   Suppose we have #define SQUARE(A) A * A

	   And then we do SQUARE(2+3)

	   Then the tokens 2, +, 3, will have the same location,
	   saying they come from the expansion of the argument A.  */
	num_macro_tokens = exp_count;
      map = linemap_enter_macro (pfile->line_table, node,
				 expansion_point_loc,
				 num_macro_tokens);
    }
  i = 0;
  vaopt_state vaopt_tracker (pfile, macro->variadic,
			     args[macro->paramc - 1].count > 0);
  const cpp_token **vaopt_start = NULL;
  for (src = macro->exp.tokens; src < limit; src++)
    {
      unsigned int arg_tokens_count;
      macro_arg_token_iter from;
      const cpp_token **paste_flag = NULL;
      const cpp_token **tmp_token_ptr;

      /* __VA_OPT__ handling.  */
      vaopt_state::update_type vostate = vaopt_tracker.update (src);
      if (vostate != vaopt_state::INCLUDE)
	{
	  if (vostate == vaopt_state::BEGIN)
	    {
	      /* Padding on the left of __VA_OPT__ (unless RHS of ##).  */
	      if (src != macro->exp.tokens && !(src[-1].flags & PASTE_LEFT))
		{
		  const cpp_token *t = padding_token (pfile, src);
		  unsigned index = expanded_token_index (pfile, macro, src, i);
		  /* Allocate a virtual location for the padding token and
		     append the token and its location to BUFF and
		     VIRT_LOCS.   */
		  tokens_buff_add_token (buff, virt_locs, t,
					 t->src_loc, t->src_loc,
					 map, index);
		}
	      vaopt_start = tokens_buff_last_token_ptr (buff);
	    }
	  else if (vostate == vaopt_state::END)
	    {
	      const cpp_token **start = vaopt_start;
	      vaopt_start = NULL;

	      /* Remove any tail padding from inside the __VA_OPT__.  */
	      paste_flag = tokens_buff_last_token_ptr (buff);
	      while (paste_flag && paste_flag != start
		     && (*paste_flag)->type == CPP_PADDING)
		{
		  tokens_buff_remove_last_token (buff);
		  paste_flag = tokens_buff_last_token_ptr (buff);
		}

	      if (src->flags & PASTE_LEFT)
		{
		  /* With a non-empty __VA_OPT__ on the LHS of ##, the last
		     token should be flagged PASTE_LEFT.  */
		  if (paste_flag && (*paste_flag)->type != CPP_PADDING)
		    copy_paste_flag (pfile, paste_flag, src);
		}
	      else
		{
		  /* Otherwise, avoid paste on RHS, __VA_OPT__(c)d or
		     __VA_OPT__(c)__VA_OPT__(d).  */
		  const cpp_token *t = &pfile->avoid_paste;
		  tokens_buff_add_token (buff, virt_locs,
					 t, t->src_loc, t->src_loc,
					 NULL, 0);
		}
	    }
	  continue;
	}

      if (src->type != CPP_MACRO_ARG)
	{
	  /* Allocate a virtual location for token SRC, and add that
	     token and its virtual location into the buffers BUFF and
	     VIRT_LOCS.  */
	  unsigned index = expanded_token_index (pfile, macro, src, i);
	  tokens_buff_add_token (buff, virt_locs, src,
				 src->src_loc, src->src_loc,
				 map, index);
	  i += 1;
	  continue;
	}

      paste_flag = 0;
      arg = &args[src->val.macro_arg.arg_no - 1];
      /* SRC is a macro parameter that we need to replace with its
	 corresponding argument.  So at some point we'll need to
	 iterate over the tokens of the macro argument and copy them
	 into the "place" now holding the correspondig macro
	 parameter.  We are going to use the iterator type
	 macro_argo_token_iter to handle that iterating.  The 'if'
	 below is to initialize the iterator depending on the type of
	 tokens the macro argument has.  It also does some adjustment
	 related to padding tokens and some pasting corner cases.  */
      if (src->flags & STRINGIFY_ARG)
	{
	  arg_tokens_count = 1;
	  macro_arg_token_iter_init (&from,
				     CPP_OPTION (pfile,
						 track_macro_expansion),
				     MACRO_ARG_TOKEN_STRINGIFIED,
				     arg, &arg->stringified);
	}
      else if (src->flags & PASTE_LEFT)
	{
	  arg_tokens_count = arg->count;
	  macro_arg_token_iter_init (&from,
				     CPP_OPTION (pfile,
						 track_macro_expansion),
				     MACRO_ARG_TOKEN_NORMAL,
				     arg, arg->first);
	}
      else if (src != macro->exp.tokens && (src[-1].flags & PASTE_LEFT))
	{
	  int num_toks;
	  arg_tokens_count = arg->count;
	  macro_arg_token_iter_init (&from,
				     CPP_OPTION (pfile,
						 track_macro_expansion),
				     MACRO_ARG_TOKEN_NORMAL,
				     arg, arg->first);

	  num_toks = tokens_buff_count (buff);

	  if (num_toks != 0)
	    {
	      /* So the current parameter token is pasted to the previous
		 token in the replacement list.  Let's look at what
		 we have as previous and current arguments.  */

	      /* This is the previous argument's token ...  */
	      tmp_token_ptr = tokens_buff_last_token_ptr (buff);

	      if ((*tmp_token_ptr)->type == CPP_COMMA
		  && macro->variadic
		  && src->val.macro_arg.arg_no == macro->paramc)
		{
		  /* ... which is a comma; and the current parameter
		     is the last parameter of a variadic function-like
		     macro.  If the argument to the current last
		     parameter is NULL, then swallow the comma,
		     otherwise drop the paste flag.  */
		  if (macro_arg_token_iter_get_token (&from) == NULL)
		    tokens_buff_remove_last_token (buff);
		  else
		    paste_flag = tmp_token_ptr;
		}
	      /* Remove the paste flag if the RHS is a placemarker, unless the
		 previous emitted token is at the beginning of __VA_OPT__;
		 placemarkers within __VA_OPT__ are ignored in that case.  */
	      else if (arg_tokens_count == 0
		       && tmp_token_ptr != vaopt_start)
		paste_flag = tmp_token_ptr;
	    }
	}
      else
	{
	  arg_tokens_count = arg->expanded_count;
	  macro_arg_token_iter_init (&from,
				     CPP_OPTION (pfile,
						 track_macro_expansion),
				     MACRO_ARG_TOKEN_EXPANDED,
				     arg, arg->expanded);

	  if (last_token_is (buff, vaopt_start))
	    {
	      /* We're expanding an arg at the beginning of __VA_OPT__.
		 Skip padding. */
	      while (arg_tokens_count)
		{
		  const cpp_token *t = macro_arg_token_iter_get_token (&from);
		  if (t->type != CPP_PADDING)
		    break;
		  macro_arg_token_iter_forward (&from);
		  --arg_tokens_count;
		}
	    }
	}

      /* Padding on the left of an argument (unless RHS of ##).  */
      if ((!pfile->state.in_directive || pfile->state.directive_wants_padding)
	  && src != macro->exp.tokens && !(src[-1].flags & PASTE_LEFT)
	  && !last_token_is (buff, vaopt_start))
	{
	  const cpp_token *t = padding_token (pfile, src);
	  unsigned index = expanded_token_index (pfile, macro, src, i);
	  /* Allocate a virtual location for the padding token and
	     append the token and its location to BUFF and
	     VIRT_LOCS.   */
	  tokens_buff_add_token (buff, virt_locs, t,
				 t->src_loc, t->src_loc,
				 map, index);
	}

      if (arg_tokens_count)
	{
	  /* So now we've got the number of tokens that make up the
	     argument that is going to replace the current parameter
	     in the macro's replacement list.  */
	  unsigned int j;
	  for (j = 0; j < arg_tokens_count; ++j)
	    {
	      /* So if track_macro_exp is < 2, the user wants to
		 save extra memory while tracking macro expansion
		 locations.  So in that case here is what we do:

		 Suppose we have #define SQUARE(A) A * A

		 And then we do SQUARE(2+3)

		 Then the tokens 2, +, 3, will have the same location,
		 saying they come from the expansion of the argument
		 A.

	      So that means we are going to ignore the COUNT tokens
	      resulting from the expansion of the current macro
	      argument. In other words all the ARG_TOKENS_COUNT tokens
	      resulting from the expansion of the macro argument will
	      have the index I.  Normally, each of those tokens should
	      have index I+J.  */
	      unsigned token_index = i;
	      unsigned index;
	      if (track_macro_exp > 1)
		token_index += j;

	      index = expanded_token_index (pfile, macro, src, token_index);
	      tokens_buff_add_token (buff, virt_locs,
				     macro_arg_token_iter_get_token (&from),
				     macro_arg_token_iter_get_location (&from),
				     src->src_loc, map, index);
	      macro_arg_token_iter_forward (&from);
	    }

	  /* With a non-empty argument on the LHS of ##, the last
	     token should be flagged PASTE_LEFT.  */
	  if (src->flags & PASTE_LEFT)
	    paste_flag
	      = (const cpp_token **) tokens_buff_last_token_ptr (buff);
	}
      else if (CPP_PEDANTIC (pfile) && ! CPP_OPTION (pfile, c99)
	       && ! macro->syshdr && ! cpp_in_system_header (pfile))
	{
	  if (CPP_OPTION (pfile, cplusplus))
	    cpp_pedwarning (pfile, CPP_W_PEDANTIC,
			    "invoking macro %s argument %d: "
			    "empty macro arguments are undefined"
			    " in ISO C++98",
			    NODE_NAME (node), src->val.macro_arg.arg_no);
	  else if (CPP_OPTION (pfile, cpp_warn_c90_c99_compat))
	    cpp_pedwarning (pfile, 
			    CPP_OPTION (pfile, cpp_warn_c90_c99_compat) > 0
			    ? CPP_W_C90_C99_COMPAT : CPP_W_PEDANTIC,
			    "invoking macro %s argument %d: "
			    "empty macro arguments are undefined"
			    " in ISO C90",
			    NODE_NAME (node), src->val.macro_arg.arg_no);
	}
      else if (CPP_OPTION (pfile, cpp_warn_c90_c99_compat) > 0
	       && ! CPP_OPTION (pfile, cplusplus)
	       && ! macro->syshdr && ! cpp_in_system_header (pfile))
	cpp_warning (pfile, CPP_W_C90_C99_COMPAT,
		     "invoking macro %s argument %d: "
		     "empty macro arguments are undefined"
		     " in ISO C90",
		     NODE_NAME (node), src->val.macro_arg.arg_no);

      /* Avoid paste on RHS (even case count == 0).  */
      if (!pfile->state.in_directive && !(src->flags & PASTE_LEFT)
	  && !last_token_is (buff, vaopt_start))
	{
	  const cpp_token *t = &pfile->avoid_paste;
	  tokens_buff_add_token (buff, virt_locs,
				 t, t->src_loc, t->src_loc,
				 NULL, 0);
	}

      /* Add a new paste flag, or remove an unwanted one.  */
      if (paste_flag)
	copy_paste_flag (pfile, paste_flag, src);

      i += arg_tokens_count;
    }

  if (track_macro_exp)
    push_extended_tokens_context (pfile, node, buff, virt_locs, first,
				  tokens_buff_count (buff));
  else
    push_ptoken_context (pfile, node, buff, first,
			 tokens_buff_count (buff));

  num_macro_tokens_counter += tokens_buff_count (buff);
}

/* Return a special padding token, with padding inherited from SOURCE.  */
static const cpp_token *
padding_token (cpp_reader *pfile, const cpp_token *source)
{
  cpp_token *result = _cpp_temp_token (pfile);

  result->type = CPP_PADDING;

  /* Data in GCed data structures cannot be made const so far, so we
     need a cast here.  */
  result->val.source = (cpp_token *) source;
  result->flags = 0;
  return result;
}

/* Get a new uninitialized context.  Create a new one if we cannot
   re-use an old one.  */
static cpp_context *
next_context (cpp_reader *pfile)
{
  cpp_context *result = pfile->context->next;

  if (result == 0)
    {
      result = XNEW (cpp_context);
      memset (result, 0, sizeof (cpp_context));
      result->prev = pfile->context;
      result->next = 0;
      pfile->context->next = result;
    }

  pfile->context = result;
  return result;
}

/* Push a list of pointers to tokens.  */
static void
push_ptoken_context (cpp_reader *pfile, cpp_hashnode *macro, _cpp_buff *buff,
		     const cpp_token **first, unsigned int count)
{
  cpp_context *context = next_context (pfile);

  context->tokens_kind = TOKENS_KIND_INDIRECT;
  context->c.macro = macro;
  context->buff = buff;
  FIRST (context).ptoken = first;
  LAST (context).ptoken = first + count;
}

/* Push a list of tokens.

   A NULL macro means that we should continue the current macro
   expansion, in essence.  That means that if we are currently in a
   macro expansion context, we'll make the new pfile->context refer to
   the current macro.  */
void
_cpp_push_token_context (cpp_reader *pfile, cpp_hashnode *macro,
			 const cpp_token *first, unsigned int count)
{
  cpp_context *context;

   if (macro == NULL)
     macro = macro_of_context (pfile->context);

   context = next_context (pfile);
   context->tokens_kind = TOKENS_KIND_DIRECT;
   context->c.macro = macro;
   context->buff = NULL;
   FIRST (context).token = first;
   LAST (context).token = first + count;
}

/* Build a context containing a list of tokens as well as their
   virtual locations and push it.  TOKENS_BUFF is the buffer that
   contains the tokens pointed to by FIRST.  If TOKENS_BUFF is
   non-NULL, it means that the context owns it, meaning that
   _cpp_pop_context will free it as well as VIRT_LOCS_BUFF that
   contains the virtual locations.

   A NULL macro means that we should continue the current macro
   expansion, in essence.  That means that if we are currently in a
   macro expansion context, we'll make the new pfile->context refer to
   the current macro.  */
static void
push_extended_tokens_context (cpp_reader *pfile,
			      cpp_hashnode *macro,
			      _cpp_buff *token_buff,
			      source_location *virt_locs,
			      const cpp_token **first,
			      unsigned int count)
{
  cpp_context *context;
  macro_context *m;

  if (macro == NULL)
    macro = macro_of_context (pfile->context);

  context = next_context (pfile);
  context->tokens_kind = TOKENS_KIND_EXTENDED;
  context->buff = token_buff;

  m = XNEW (macro_context);
  m->macro_node = macro;
  m->virt_locs = virt_locs;
  m->cur_virt_loc = virt_locs;
  context->c.mc = m;
  FIRST (context).ptoken = first;
  LAST (context).ptoken = first + count;
}

/* Push a traditional macro's replacement text.  */
void
_cpp_push_text_context (cpp_reader *pfile, cpp_hashnode *macro,
			const uchar *start, size_t len)
{
  cpp_context *context = next_context (pfile);

  context->tokens_kind = TOKENS_KIND_DIRECT;
  context->c.macro = macro;
  context->buff = NULL;
  CUR (context) = start;
  RLIMIT (context) = start + len;
  macro->flags |= NODE_DISABLED;
}

/* Creates a buffer that holds tokens a.k.a "token buffer", usually
   for the purpose of storing them on a cpp_context. If VIRT_LOCS is
   non-null (which means that -ftrack-macro-expansion is on),
   *VIRT_LOCS is set to a newly allocated buffer that is supposed to
   hold the virtual locations of the tokens resulting from macro
   expansion.  */
static _cpp_buff*
tokens_buff_new (cpp_reader *pfile, size_t len,
		 source_location **virt_locs)
{
  size_t tokens_size = len * sizeof (cpp_token *);
  size_t locs_size = len * sizeof (source_location);

  if (virt_locs != NULL)
    *virt_locs = XNEWVEC (source_location, locs_size);
  return _cpp_get_buff (pfile, tokens_size);
}

/* Returns the number of tokens contained in a token buffer.  The
   buffer holds a set of cpp_token*.  */
static size_t
tokens_buff_count (_cpp_buff *buff)
{
  return (BUFF_FRONT (buff) - buff->base) / sizeof (cpp_token *);
}

/* Return a pointer to the last token contained in the token buffer
   BUFF.  */
static const cpp_token **
tokens_buff_last_token_ptr (_cpp_buff *buff)
{
  if (BUFF_FRONT (buff) == buff->base)
    return NULL;
  return &((const cpp_token **) BUFF_FRONT (buff))[-1];
}

/* Remove the last token contained in the token buffer TOKENS_BUFF.
   If VIRT_LOCS_BUFF is non-NULL,  it should point at the buffer
   containing the virtual locations of the tokens in TOKENS_BUFF; in
   which case the function updates that buffer as well.   */
static inline void
tokens_buff_remove_last_token (_cpp_buff *tokens_buff)

{
  if (BUFF_FRONT (tokens_buff) > tokens_buff->base)
    BUFF_FRONT (tokens_buff) =
      (unsigned char *) &((cpp_token **) BUFF_FRONT (tokens_buff))[-1];
}

/* Insert a token into the token buffer at the position pointed to by
   DEST.  Note that the buffer is not enlarged so the previous token
   that was at *DEST is overwritten.  VIRT_LOC_DEST, if non-null,
   means -ftrack-macro-expansion is effect; it then points to where to
   insert the virtual location of TOKEN.  TOKEN is the token to
   insert.  VIRT_LOC is the virtual location of the token, i.e, the
   location possibly encoding its locus across macro expansion.  If
   TOKEN is an argument of a function-like macro (inside a macro
   replacement list), PARM_DEF_LOC is the spelling location of the
   macro parameter that TOKEN is replacing, in the replacement list of
   the macro.  If TOKEN is not an argument of a function-like macro or
   if it doesn't come from a macro expansion, then VIRT_LOC can just
   be set to the same value as PARM_DEF_LOC.  If MAP is non null, it
   means TOKEN comes from a macro expansion and MAP is the macro map
   associated to the macro.  MACRO_TOKEN_INDEX points to the index of
   the token in the macro map; it is not considered if MAP is NULL.

   Upon successful completion this function returns the a pointer to
   the position of the token coming right after the insertion
   point.  */
static inline const cpp_token **
tokens_buff_put_token_to (const cpp_token **dest,
			  source_location *virt_loc_dest,
			  const cpp_token *token,
			  source_location virt_loc,
			  source_location parm_def_loc,			  
			  const line_map_macro *map,
			  unsigned int macro_token_index)
{
  source_location macro_loc = virt_loc;
  const cpp_token **result;

  if (virt_loc_dest)
    {
      /* -ftrack-macro-expansion is on.  */
      if (map)
	macro_loc = linemap_add_macro_token (map, macro_token_index,
					     virt_loc, parm_def_loc);
      *virt_loc_dest = macro_loc;
    }
  *dest = token;
  result = &dest[1];

  return result;
}

/* Adds a token at the end of the tokens contained in BUFFER.  Note
   that this function doesn't enlarge BUFFER when the number of tokens
   reaches BUFFER's size; it aborts in that situation.

   TOKEN is the token to append. VIRT_LOC is the virtual location of
   the token, i.e, the location possibly encoding its locus across
   macro expansion. If TOKEN is an argument of a function-like macro
   (inside a macro replacement list), PARM_DEF_LOC is the location of
   the macro parameter that TOKEN is replacing.  If TOKEN doesn't come
   from a macro expansion, then VIRT_LOC can just be set to the same
   value as PARM_DEF_LOC.  If MAP is non null, it means TOKEN comes
   from a macro expansion and MAP is the macro map associated to the
   macro.  MACRO_TOKEN_INDEX points to the index of the token in the
   macro map; It is not considered if MAP is NULL.  If VIRT_LOCS is
   non-null, it means -ftrack-macro-expansion is on; in which case
   this function adds the virtual location DEF_LOC to the VIRT_LOCS
   array, at the same index as the one of TOKEN in BUFFER.  Upon
   successful completion this function returns the a pointer to the
   position of the token coming right after the insertion point.  */
static const cpp_token **
tokens_buff_add_token (_cpp_buff *buffer,
		       source_location *virt_locs,
		       const cpp_token *token,
		       source_location virt_loc,
		       source_location parm_def_loc,
		       const line_map_macro *map,
		       unsigned int macro_token_index)
{
  const cpp_token **result;
  source_location *virt_loc_dest = NULL;
  unsigned token_index = 
    (BUFF_FRONT (buffer) - buffer->base) / sizeof (cpp_token *);

  /* Abort if we pass the end the buffer.  */
  if (BUFF_FRONT (buffer) > BUFF_LIMIT (buffer))
    abort ();

  if (virt_locs != NULL)
    virt_loc_dest = &virt_locs[token_index];

  result =
    tokens_buff_put_token_to ((const cpp_token **) BUFF_FRONT (buffer),
			      virt_loc_dest, token, virt_loc, parm_def_loc,
			      map, macro_token_index);

  BUFF_FRONT (buffer) = (unsigned char *) result;
  return result;
}

/* Allocate space for the function-like macro argument ARG to store
   the tokens resulting from the macro-expansion of the tokens that
   make up ARG itself. That space is allocated in ARG->expanded and
   needs to be freed using free.  */
static void
alloc_expanded_arg_mem (cpp_reader *pfile, macro_arg *arg, size_t capacity)
{
  gcc_checking_assert (arg->expanded == NULL
		       && arg->expanded_virt_locs == NULL);

  arg->expanded = XNEWVEC (const cpp_token *, capacity);
  if (CPP_OPTION (pfile, track_macro_expansion))
    arg->expanded_virt_locs = XNEWVEC (source_location, capacity);

}

/* If necessary, enlarge ARG->expanded to so that it can contain SIZE
   tokens.  */
static void
ensure_expanded_arg_room (cpp_reader *pfile, macro_arg *arg,
			  size_t size, size_t *expanded_capacity)
{
  if (size <= *expanded_capacity)
    return;

  size *= 2;

  arg->expanded =
    XRESIZEVEC (const cpp_token *, arg->expanded, size);
  *expanded_capacity = size;

  if (CPP_OPTION (pfile, track_macro_expansion))
    {
      if (arg->expanded_virt_locs == NULL)
	arg->expanded_virt_locs = XNEWVEC (source_location, size);
      else
	arg->expanded_virt_locs = XRESIZEVEC (source_location,
					      arg->expanded_virt_locs,
					      size);
    }
}

/* Expand an argument ARG before replacing parameters in a
   function-like macro.  This works by pushing a context with the
   argument's tokens, and then expanding that into a temporary buffer
   as if it were a normal part of the token stream.  collect_args()
   has terminated the argument's tokens with a CPP_EOF so that we know
   when we have fully expanded the argument.  */
static void
expand_arg (cpp_reader *pfile, macro_arg *arg)
{
  size_t capacity;
  bool saved_warn_trad;
  bool track_macro_exp_p = CPP_OPTION (pfile, track_macro_expansion);

  if (arg->count == 0
      || arg->expanded != NULL)
    return;

  /* Don't warn about funlike macros when pre-expanding.  */
  saved_warn_trad = CPP_WTRADITIONAL (pfile);
  CPP_WTRADITIONAL (pfile) = 0;

  /* Loop, reading in the tokens of the argument.  */
  capacity = 256;
  alloc_expanded_arg_mem (pfile, arg, capacity);

  if (track_macro_exp_p)
    push_extended_tokens_context (pfile, NULL, NULL,
				  arg->virt_locs,
				  arg->first,
				  arg->count + 1);
  else
    push_ptoken_context (pfile, NULL, NULL,
			 arg->first, arg->count + 1);

  for (;;)
    {
      const cpp_token *token;
      source_location location;

      ensure_expanded_arg_room (pfile, arg, arg->expanded_count + 1,
				&capacity);

      token = cpp_get_token_1 (pfile, &location);

      if (token->type == CPP_EOF)
	break;

      set_arg_token (arg, token, location,
		     arg->expanded_count, MACRO_ARG_TOKEN_EXPANDED,
		     CPP_OPTION (pfile, track_macro_expansion));
      arg->expanded_count++;
    }

  _cpp_pop_context (pfile);

  CPP_WTRADITIONAL (pfile) = saved_warn_trad;
}

/* Returns the macro associated to the current context if we are in
   the context a macro expansion, NULL otherwise.  */
static cpp_hashnode*
macro_of_context (cpp_context *context)
{
  if (context == NULL)
    return NULL;

  return (context->tokens_kind == TOKENS_KIND_EXTENDED)
    ? context->c.mc->macro_node
    : context->c.macro;
}

/* Return TRUE iff we are expanding a macro or are about to start
   expanding one.  If we are effectively expanding a macro, the
   function macro_of_context returns a pointer to the macro being
   expanded.  */
static bool
in_macro_expansion_p (cpp_reader *pfile)
{
  if (pfile == NULL)
    return false;

  return (pfile->about_to_expand_macro_p 
	  || macro_of_context (pfile->context));
}

/* Pop the current context off the stack, re-enabling the macro if the
   context represented a macro's replacement list.  Initially the
   context structure was not freed so that we can re-use it later, but
   now we do free it to reduce peak memory consumption.  */
void
_cpp_pop_context (cpp_reader *pfile)
{
  cpp_context *context = pfile->context;

  /* We should not be popping the base context.  */
  if (context == &pfile->base_context)
    abort ();

  if (context->c.macro)
    {
      cpp_hashnode *macro;
      if (context->tokens_kind == TOKENS_KIND_EXTENDED)
	{
	  macro_context *mc = context->c.mc;
	  macro = mc->macro_node;
	  /* If context->buff is set, it means the life time of tokens
	     is bound to the life time of this context; so we must
	     free the tokens; that means we must free the virtual
	     locations of these tokens too.  */
	  if (context->buff && mc->virt_locs)
	    {
	      free (mc->virt_locs);
	      mc->virt_locs = NULL;
	    }
	  free (mc);
	  context->c.mc = NULL;
	}
      else
	macro = context->c.macro;

      /* Beware that MACRO can be NULL in cases like when we are
	 called from expand_arg.  In those cases, a dummy context with
	 tokens is pushed just for the purpose of walking them using
	 cpp_get_token_1.  In that case, no 'macro' field is set into
	 the dummy context.  */
      if (macro != NULL
	  /* Several contiguous macro expansion contexts can be
	     associated to the same macro; that means it's the same
	     macro expansion that spans across all these (sub)
	     contexts.  So we should re-enable an expansion-disabled
	     macro only when we are sure we are really out of that
	     macro expansion.  */
	  && macro_of_context (context->prev) != macro)
	macro->flags &= ~NODE_DISABLED;

      if (macro == pfile->top_most_macro_node && context->prev == NULL)
	/* We are popping the context of the top-most macro node.  */
	pfile->top_most_macro_node = NULL;
    }

  if (context->buff)
    {
      /* Decrease memory peak consumption by freeing the memory used
	 by the context.  */
      _cpp_free_buff (context->buff);
    }

  pfile->context = context->prev;
  /* decrease peak memory consumption by feeing the context.  */
  pfile->context->next = NULL;
  free (context);
}

/* Return TRUE if we reached the end of the set of tokens stored in
   CONTEXT, FALSE otherwise.  */
static inline bool
reached_end_of_context (cpp_context *context)
{
  if (context->tokens_kind == TOKENS_KIND_DIRECT)
      return FIRST (context).token == LAST (context).token;
  else if (context->tokens_kind == TOKENS_KIND_INDIRECT
	   || context->tokens_kind == TOKENS_KIND_EXTENDED)
    return FIRST (context).ptoken == LAST (context).ptoken;
  else
    abort ();
}

/* Consume the next token contained in the current context of PFILE,
   and return it in *TOKEN. It's "full location" is returned in
   *LOCATION. If -ftrack-macro-location is in effeect, fFull location"
   means the location encoding the locus of the token across macro
   expansion; otherwise it's just is the "normal" location of the
   token which (*TOKEN)->src_loc.  */
static inline void
consume_next_token_from_context (cpp_reader *pfile,
				 const cpp_token ** token,
				 source_location *location)
{
  cpp_context *c = pfile->context;

  if ((c)->tokens_kind == TOKENS_KIND_DIRECT)
    {
      *token = FIRST (c).token;
      *location = (*token)->src_loc;
      FIRST (c).token++;
    }
  else if ((c)->tokens_kind == TOKENS_KIND_INDIRECT)		
    {
      *token = *FIRST (c).ptoken;
      *location = (*token)->src_loc;
      FIRST (c).ptoken++;
    }
  else if ((c)->tokens_kind == TOKENS_KIND_EXTENDED)
    {
      macro_context *m = c->c.mc;
      *token = *FIRST (c).ptoken;
      if (m->virt_locs)
	{
	  *location = *m->cur_virt_loc;
	  m->cur_virt_loc++;
	}
      else
	*location = (*token)->src_loc;
      FIRST (c).ptoken++;
    }
  else
    abort ();
}

/* In the traditional mode of the preprocessor, if we are currently in
   a directive, the location of a token must be the location of the
   start of the directive line.  This function returns the proper
   location if we are in the traditional mode, and just returns
   LOCATION otherwise.  */

static inline source_location
maybe_adjust_loc_for_trad_cpp (cpp_reader *pfile, source_location location)
{
  if (CPP_OPTION (pfile, traditional))
    {
      if (pfile->state.in_directive)
	return pfile->directive_line;
    }
  return location;
}

/* Routine to get a token as well as its location.

   Macro expansions and directives are transparently handled,
   including entering included files.  Thus tokens are post-macro
   expansion, and after any intervening directives.  External callers
   see CPP_EOF only at EOF.  Internal callers also see it when meeting
   a directive inside a macro call, when at the end of a directive and
   state.in_directive is still 1, and at the end of argument
   pre-expansion.

   LOC is an out parameter; *LOC is set to the location "as expected
   by the user".  Please read the comment of
   cpp_get_token_with_location to learn more about the meaning of this
   location.  */
static const cpp_token*
cpp_get_token_1 (cpp_reader *pfile, source_location *location)
{
  const cpp_token *result;
  /* This token is a virtual token that either encodes a location
     related to macro expansion or a spelling location.  */
  source_location virt_loc = 0;
  /* pfile->about_to_expand_macro_p can be overriden by indirect calls
     to functions that push macro contexts.  So let's save it so that
     we can restore it when we are about to leave this routine.  */
  bool saved_about_to_expand_macro = pfile->about_to_expand_macro_p;

  for (;;)
    {
      cpp_hashnode *node;
      cpp_context *context = pfile->context;

      /* Context->prev == 0 <=> base context.  */
      if (!context->prev)
	{
	  result = _cpp_lex_token (pfile);
	  virt_loc = result->src_loc;
	}
      else if (!reached_end_of_context (context))
	{
	  consume_next_token_from_context (pfile, &result,
					   &virt_loc);
	  if (result->flags & PASTE_LEFT)
	    {
	      paste_all_tokens (pfile, result);
	      if (pfile->state.in_directive)
		continue;
	      result = padding_token (pfile, result);
	      goto out;
	    }
	}
      else
	{
	  if (pfile->context->c.macro)
	    ++num_expanded_macros_counter;
	  _cpp_pop_context (pfile);
	  if (pfile->state.in_directive)
	    continue;
	  result = &pfile->avoid_paste;
	  goto out;
	}

      if (pfile->state.in_directive && result->type == CPP_COMMENT)
	continue;

      if (result->type != CPP_NAME)
	break;

      node = result->val.node.node;

      if (node->type == NT_VOID || (result->flags & NO_EXPAND))
	break;

      if (!(node->flags & NODE_DISABLED))
	{
	  int ret = 0;
	  /* If not in a macro context, and we're going to start an
	     expansion, record the location and the top level macro
	     about to be expanded.  */
	  if (!in_macro_expansion_p (pfile))
	    {
	      pfile->invocation_location = result->src_loc;
	      pfile->top_most_macro_node = node;
	    }
	  if (pfile->state.prevent_expansion)
	    break;

	  /* Conditional macros require that a predicate be evaluated
	     first.  */
	  if ((node->flags & NODE_CONDITIONAL) != 0)
	    {
	      if (pfile->cb.macro_to_expand)
		{
		  bool whitespace_after;
		  const cpp_token *peek_tok = cpp_peek_token (pfile, 0);

		  whitespace_after = (peek_tok->type == CPP_PADDING
				      || (peek_tok->flags & PREV_WHITE));
		  node = pfile->cb.macro_to_expand (pfile, result);
		  if (node)
		    ret = enter_macro_context (pfile, node, result,
					       virt_loc);
		  else if (whitespace_after)
		    {
		      /* If macro_to_expand hook returned NULL and it
			 ate some tokens, see if we don't need to add
			 a padding token in between this and the
			 next token.  */
		      peek_tok = cpp_peek_token (pfile, 0);
		      if (peek_tok->type != CPP_PADDING
			  && (peek_tok->flags & PREV_WHITE) == 0)
			_cpp_push_token_context (pfile, NULL,
						 padding_token (pfile,
								peek_tok), 1);
		    }
		}
	    }
	  else
	    ret = enter_macro_context (pfile, node, result, 
				       virt_loc);
	  if (ret)
 	    {
	      if (pfile->state.in_directive || ret == 2)
		continue;
	      result = padding_token (pfile, result);
	      goto out;
	    }
	}
      else
	{
	  /* Flag this token as always unexpandable.  FIXME: move this
	     to collect_args()?.  */
	  cpp_token *t = _cpp_temp_token (pfile);
	  t->type = result->type;
	  t->flags = result->flags | NO_EXPAND;
	  t->val = result->val;
	  result = t;
	}

      break;
    }

 out:
  if (location != NULL)
    {
      if (virt_loc == 0)
	virt_loc = result->src_loc;
      *location = virt_loc;

      if (!CPP_OPTION (pfile, track_macro_expansion)
	  && macro_of_context (pfile->context) != NULL)
	/* We are in a macro expansion context, are not tracking
	   virtual location, but were asked to report the location
	   of the expansion point of the macro being expanded.  */
	*location = pfile->invocation_location;

      *location = maybe_adjust_loc_for_trad_cpp (pfile, *location);
    }

  pfile->about_to_expand_macro_p = saved_about_to_expand_macro;
  return result;
}

/* External routine to get a token.  Also used nearly everywhere
   internally, except for places where we know we can safely call
   _cpp_lex_token directly, such as lexing a directive name.

   Macro expansions and directives are transparently handled,
   including entering included files.  Thus tokens are post-macro
   expansion, and after any intervening directives.  External callers
   see CPP_EOF only at EOF.  Internal callers also see it when meeting
   a directive inside a macro call, when at the end of a directive and
   state.in_directive is still 1, and at the end of argument
   pre-expansion.  */
const cpp_token *
cpp_get_token (cpp_reader *pfile)
{
  return cpp_get_token_1 (pfile, NULL);
}

/* Like cpp_get_token, but also returns a virtual token location
   separate from the spelling location carried by the returned token.

   LOC is an out parameter; *LOC is set to the location "as expected
   by the user".  This matters when a token results from macro
   expansion; in that case the token's spelling location indicates the
   locus of the token in the definition of the macro but *LOC
   virtually encodes all the other meaningful locuses associated to
   the token.

   What? virtual location? Yes, virtual location.

   If the token results from macro expansion and if macro expansion
   location tracking is enabled its virtual location encodes (at the
   same time):

   - the spelling location of the token

   - the locus of the macro expansion point

   - the locus of the point where the token got instantiated as part
     of the macro expansion process.

   You have to use the linemap API to get the locus you are interested
   in from a given virtual location.

   Note however that virtual locations are not necessarily ordered for
   relations '<' and '>'.  One must use the function
   linemap_location_before_p instead of using the relational operator
   '<'.

   If macro expansion tracking is off and if the token results from
   macro expansion the virtual location is the expansion point of the
   macro that got expanded.

   When the token doesn't result from macro expansion, the virtual
   location is just the same thing as its spelling location.  */

const cpp_token *
cpp_get_token_with_location (cpp_reader *pfile, source_location *loc)
{
  return cpp_get_token_1 (pfile, loc);
}

/* Returns true if we're expanding an object-like macro that was
   defined in a system header.  Just checks the macro at the top of
   the stack.  Used for diagnostic suppression.  */
int
cpp_sys_macro_p (cpp_reader *pfile)
{
  cpp_hashnode *node = NULL;

  if (pfile->context->tokens_kind == TOKENS_KIND_EXTENDED)
    node = pfile->context->c.mc->macro_node;
  else
    node = pfile->context->c.macro;

  return node && node->value.macro && node->value.macro->syshdr;
}

/* Read each token in, until end of the current file.  Directives are
   transparently processed.  */
void
cpp_scan_nooutput (cpp_reader *pfile)
{
  /* Request a CPP_EOF token at the end of this file, rather than
     transparently continuing with the including file.  */
  pfile->buffer->return_at_eof = true;

  pfile->state.discarding_output++;
  pfile->state.prevent_expansion++;

  if (CPP_OPTION (pfile, traditional))
    while (_cpp_read_logical_line_trad (pfile))
      ;
  else
    while (cpp_get_token (pfile)->type != CPP_EOF)
      ;

  pfile->state.discarding_output--;
  pfile->state.prevent_expansion--;
}

/* Step back one or more tokens obtained from the lexer.  */
void
_cpp_backup_tokens_direct (cpp_reader *pfile, unsigned int count)
{
  pfile->lookaheads += count;
  while (count--)
    {
      pfile->cur_token--;
      if (pfile->cur_token == pfile->cur_run->base
          /* Possible with -fpreprocessed and no leading #line.  */
          && pfile->cur_run->prev != NULL)
        {
          pfile->cur_run = pfile->cur_run->prev;
          pfile->cur_token = pfile->cur_run->limit;
        }
    }
}

/* Step back one (or more) tokens.  Can only step back more than 1 if
   they are from the lexer, and not from macro expansion.  */
void
_cpp_backup_tokens (cpp_reader *pfile, unsigned int count)
{
  if (pfile->context->prev == NULL)
    _cpp_backup_tokens_direct (pfile, count);
  else
    {
      if (count != 1)
	abort ();
      if (pfile->context->tokens_kind == TOKENS_KIND_DIRECT)
	FIRST (pfile->context).token--;
      else if (pfile->context->tokens_kind == TOKENS_KIND_INDIRECT)
	FIRST (pfile->context).ptoken--;
      else if (pfile->context->tokens_kind == TOKENS_KIND_EXTENDED)
	{
	  FIRST (pfile->context).ptoken--;
	  if (pfile->context->c.macro)
	    {
	      macro_context *m = pfile->context->c.mc;
	      m->cur_virt_loc--;
	      gcc_checking_assert (m->cur_virt_loc >= m->virt_locs);
	    }
	  else
	    abort ();
	}
      else
	abort ();
    }
}

/* #define directive parsing and handling.  */

/* Returns nonzero if a macro redefinition warning is required.  */
static bool
warn_of_redefinition (cpp_reader *pfile, cpp_hashnode *node,
		      const cpp_macro *macro2)
{
  unsigned int i;

  /* Some redefinitions need to be warned about regardless.  */
  if (node->flags & NODE_WARN)
    return true;

  /* Suppress warnings for builtins that lack the NODE_WARN flag,
     unless Wbuiltin-macro-redefined.  */
  if (cpp_builtin_macro_p (node))
    return CPP_OPTION (pfile, warn_builtin_macro_redefined);

  /* Redefinitions of conditional (context-sensitive) macros, on
     the other hand, must be allowed silently.  */
  if (node->flags & NODE_CONDITIONAL)
    return false;

  cpp_macro *macro1 = node->value.macro;
  if (macro1->lazy)
    {
      /* We don't want to mark MACRO as used, but do need to finalize
	 its laziness.  */
      pfile->cb.user_lazy_macro (pfile, macro1, macro1->lazy - 1);
      macro1->lazy = 0;
    }

  /* Redefinition of a macro is allowed if and only if the old and new
     definitions are the same.  (6.10.3 paragraph 2).  */

  /* Don't check count here as it can be different in valid
     traditional redefinitions with just whitespace differences.  */
  if (macro1->paramc != macro2->paramc
      || macro1->fun_like != macro2->fun_like
      || macro1->variadic != macro2->variadic)
    return true;

  /* Check parameter spellings.  */
  for (i = 0; i < macro1->paramc; i++)
    if (macro1->parm.params[i] != macro2->parm.params[i])
      return true;

  /* Check the replacement text or tokens.  */
  if (CPP_OPTION (pfile, traditional))
    return _cpp_expansions_different_trad (macro1, macro2);

  if (macro1->count != macro2->count)
    return true;

  for (i = 0; i < macro1->count; i++)
    if (!_cpp_equiv_tokens (&macro1->exp.tokens[i], &macro2->exp.tokens[i]))
      return true;

  return false;
}

/* Free the definition of hashnode H.  */
void
_cpp_free_definition (cpp_hashnode *h)
{
  /* Macros and assertions no longer have anything to free.  */
  h->type = NT_VOID;
  h->value.answers = NULL;
  h->flags &= ~(NODE_DISABLED | NODE_USED);
}

/* Save parameter NODE (spelling SPELLING) to the parameter list of
   macro MACRO.  Returns true on success, false on failure.   */
bool
_cpp_save_parameter (cpp_reader *pfile, unsigned n, cpp_hashnode *node,
		     cpp_hashnode *spelling)
{
  /* Constraint 6.10.3.6 - duplicate parameter names.  */
  if (node->type == NT_MACRO_ARG)
    {
      cpp_error (pfile, CPP_DL_ERROR, "duplicate macro parameter \"%s\"",
		 NODE_NAME (node));
      return false;
    }

  unsigned len = (n + 1) * sizeof (struct macro_arg_saved_data);
  if (len > pfile->macro_buffer_len)
    {
      pfile->macro_buffer
	= XRESIZEVEC (unsigned char, pfile->macro_buffer, len);
      pfile->macro_buffer_len = len;
    }
  
  macro_arg_saved_data *saved = (macro_arg_saved_data *)pfile->macro_buffer;
  saved[n].canonical_node = node;
  saved[n].value = node->value;
  saved[n].type = node->type;

  void *base = _cpp_reserve_room (pfile, n * sizeof (cpp_hashnode *),
				  sizeof (cpp_hashnode *));
  ((cpp_hashnode **)base)[n] = spelling;

  /* Morph into a macro arg.  */
  node->type = NT_MACRO_ARG;
  /* Index is 1 based.  */
  node->value.arg_index = n + 1;

  return true;
}

/* Restore the parameters to their previous state.  */
void
_cpp_unsave_parameters (cpp_reader *pfile, unsigned n)
{
  /* Clear the fast argument lookup indices.  */
  while (n--)
    {
      struct macro_arg_saved_data *save =
	&((struct macro_arg_saved_data *) pfile->macro_buffer)[n];

      struct cpp_hashnode *node = save->canonical_node;
      node->type = save->type;
      node->value = save->value;
    }
}

/* Check the syntax of the parameters in a MACRO definition.  Return
   false on failure.  Set *N_PTR and *VARADIC_PTR as appropriate.
   '(' ')'
   '(' parm-list ',' last-parm ')'
   '(' last-parm ')'
   parm-list: name
            | parm-list, name
   last-parm: name
   	    | name '...'
            | '...'
*/

static bool
parse_params (cpp_reader *pfile, unsigned *n_ptr, bool *varadic_ptr)
{
  unsigned nparms = 0;
  bool ok = false;

  for (bool prev_ident = false;;)
    {
      const cpp_token *token = _cpp_lex_token (pfile);

      switch (token->type)
	{
	case CPP_COMMENT:
	  /* Allow/ignore comments in parameter lists if we are
	     preserving comments in macro expansions.  */
	  if (!CPP_OPTION (pfile, discard_comments_in_macro_exp))
	    break;

	  /* FALLTHRU  */
	default:
	bad:
	  {
	    const char *const msgs[5] =
	      {
	       N_("expected parameter name, found \"%s\""),
	       N_("expected ',' or ')', found \"%s\""),
	       N_("expected parameter name before end of line"),
	       N_("expected ')' before end of line"),
	       N_("expected ')' after \"...\"")
	      };
	    unsigned ix = prev_ident;
	    const unsigned char *as_text = NULL;
	    if (*varadic_ptr)
	      ix = 4;
	    else if (token->type == CPP_EOF)
	      ix += 2;
	    else
	      as_text = cpp_token_as_text (pfile, token);
	    cpp_error (pfile, CPP_DL_ERROR, msgs[ix], as_text);
	  }
	  goto out;

	case CPP_NAME:
	  if (prev_ident || *varadic_ptr)
	    goto bad;
	  prev_ident = true;

	  if (!_cpp_save_parameter (pfile, nparms, token->val.node.node,
				    token->val.node.spelling))
	    goto out;
	  nparms++;
	  break;

	case CPP_CLOSE_PAREN:
	  if (prev_ident || !nparms || *varadic_ptr)
	    {
	      ok = true;
	      goto out;
	    }

	  /* FALLTHRU */
	case CPP_COMMA:
	  if (!prev_ident || *varadic_ptr)
	    goto bad;
	  prev_ident = false;
	  break;

	case CPP_ELLIPSIS:
	  if (*varadic_ptr)
	    goto bad;
	  *varadic_ptr = true;
	  if (!prev_ident)
	    {
	      /* An ISO bare ellipsis.  */
	      _cpp_save_parameter (pfile, nparms,
				   pfile->spec_nodes.n__VA_ARGS__,
				   pfile->spec_nodes.n__VA_ARGS__);
	      nparms++;
	      pfile->state.va_args_ok = 1;
	      if (! CPP_OPTION (pfile, c99)
		  && CPP_OPTION (pfile, cpp_pedantic)
		  && CPP_OPTION (pfile, warn_variadic_macros))
		cpp_pedwarning
		  (pfile, CPP_W_VARIADIC_MACROS,
		   CPP_OPTION (pfile, cplusplus)
		   ? N_("anonymous variadic macros were introduced in C++11")
		   : N_("anonymous variadic macros were introduced in C99"));
	      else if (CPP_OPTION (pfile, cpp_warn_c90_c99_compat) > 0
		       && ! CPP_OPTION (pfile, cplusplus))
		cpp_error (pfile, CPP_DL_WARNING,
			   "anonymous variadic macros were introduced in C99");
	    }
	  else if (CPP_OPTION (pfile, cpp_pedantic)
		   && CPP_OPTION (pfile, warn_variadic_macros))
	    cpp_pedwarning (pfile, CPP_W_VARIADIC_MACROS,
			    CPP_OPTION (pfile, cplusplus)
			    ? N_("ISO C++ does not permit named variadic macros")
			    : N_("ISO C does not permit named variadic macros"));
	  break;
	}
    }

 out:
  *n_ptr = nparms;

  return ok;
}

/* Lex a token from the expansion of MACRO, but mark parameters as we
   find them and warn of traditional stringification.  */
static cpp_macro *
lex_expansion_token (cpp_reader *pfile, cpp_macro *macro)
{
  macro = (cpp_macro *)_cpp_reserve_room (pfile,
					  sizeof (cpp_macro) - sizeof (cpp_token)
					  + macro->count * sizeof (cpp_token),
					  sizeof (cpp_token));
  cpp_token *saved_cur_token = pfile->cur_token;
  pfile->cur_token = &macro->exp.tokens[macro->count];
  cpp_token *token = _cpp_lex_direct (pfile);
  pfile->cur_token = saved_cur_token;

  /* Is this a parameter?  */
  if (token->type == CPP_NAME && token->val.node.node->type == NT_MACRO_ARG)
    {
      /* Morph into a parameter reference.  */
      cpp_hashnode *spelling = token->val.node.spelling;
      token->type = CPP_MACRO_ARG;
      token->val.macro_arg.arg_no = token->val.node.node->value.arg_index;
      token->val.macro_arg.spelling = spelling;
    }
  else if (CPP_WTRADITIONAL (pfile) && macro->paramc > 0
	   && (token->type == CPP_STRING || token->type == CPP_CHAR))
    check_trad_stringification (pfile, macro, &token->val.str);

  return macro;
}

static cpp_macro *
create_iso_definition (cpp_reader *pfile)
{
  bool following_paste_op = false;
  const char *paste_op_error_msg =
    N_("'##' cannot appear at either end of a macro expansion");
  unsigned int num_extra_tokens = 0;
  unsigned nparms = 0;
  cpp_hashnode **params = NULL;
  bool varadic = false;
  bool ok = false;
  cpp_macro *macro = NULL;

  /* Look at the first token, to see if this is a function-like
     macro.   */
  cpp_token first;
  cpp_token *saved_cur_token = pfile->cur_token;
  pfile->cur_token = &first;
  cpp_token *token = _cpp_lex_direct (pfile);
  pfile->cur_token = saved_cur_token;

  if (token->flags & PREV_WHITE)
    /* Preceeded by space, must be part of expansion.  */;
  else if (token->type == CPP_OPEN_PAREN)
    {
      /* An open-paren, get a parameter list.  */
      if (!parse_params (pfile, &nparms, &varadic))
	goto out;

      params = (cpp_hashnode **)_cpp_commit_buff
	(pfile, sizeof (cpp_hashnode *) * nparms);
      token = NULL;
    }
  else if (token->type != CPP_EOF
	   && !(token->type == CPP_COMMENT
		&& ! CPP_OPTION (pfile, discard_comments_in_macro_exp)))
    {
      /* While ISO C99 requires whitespace before replacement text
	 in a macro definition, ISO C90 with TC1 allows characters
	 from the basic source character set there.  */
      if (CPP_OPTION (pfile, c99))
	cpp_error (pfile, CPP_DL_PEDWARN,
		   CPP_OPTION (pfile, cplusplus)
		   ? N_("ISO C++11 requires whitespace after the macro name")
		   : N_("ISO C99 requires whitespace after the macro name"));
      else
	{
	  int warntype = CPP_DL_WARNING;
	  switch (token->type)
	    {
	    case CPP_ATSIGN:
	    case CPP_AT_NAME:
	    case CPP_OBJC_STRING:
	      /* '@' is not in basic character set.  */
	      warntype = CPP_DL_PEDWARN;
	      break;
	    case CPP_OTHER:
	      /* Basic character set sans letters, digits and _.  */
	      if (strchr ("!\"#%&'()*+,-./:;<=>?[\\]^{|}~",
			  token->val.str.text[0]) == NULL)
		warntype = CPP_DL_PEDWARN;
	      break;
	    default:
	      /* All other tokens start with a character from basic
		 character set.  */
	      break;
	    }
	  cpp_error (pfile, warntype,
		     "missing whitespace after the macro name");
	}
    }

  macro = _cpp_new_macro (pfile, cmk_macro,
			  _cpp_reserve_room (pfile, 0, sizeof (cpp_macro)));

  if (!token)
    {
      macro->variadic = varadic;
      macro->paramc = nparms;
      macro->parm.params = params;
      macro->fun_like = true;
    }
  else
    {
      /* Preserve the token we peeked, there is already a single slot for it.  */
      macro->exp.tokens[0] = *token;
      token = &macro->exp.tokens[0];
      macro->count = 1;
    }

  for (vaopt_state vaopt_tracker (pfile, macro->variadic, true);; token = NULL)
    {
      if (!token)
	{
	  macro = lex_expansion_token (pfile, macro);
	  token = &macro->exp.tokens[macro->count++];
	}

      /* Check the stringifying # constraint 6.10.3.2.1 of
	 function-like macros when lexing the subsequent token.  */
      if (macro->count > 1 && token[-1].type == CPP_HASH && macro->fun_like)
	{
	  if (token->type == CPP_MACRO_ARG)
	    {
	      if (token->flags & PREV_WHITE)
		token->flags |= SP_PREV_WHITE;
	      if (token[-1].flags & DIGRAPH)
		token->flags |= SP_DIGRAPH;
	      token->flags &= ~PREV_WHITE;
	      token->flags |= STRINGIFY_ARG;
	      token->flags |= token[-1].flags & PREV_WHITE;
	      token[-1] = token[0];
	      macro->count--;
	    }
	  /* Let assembler get away with murder.  */
	  else if (CPP_OPTION (pfile, lang) != CLK_ASM)
	    {
	      cpp_error (pfile, CPP_DL_ERROR,
			 "'#' is not followed by a macro parameter");
	      goto out;
	    }
	}

      if (token->type == CPP_EOF)
	{
	  /* Paste operator constraint 6.10.3.3.1:
	     Token-paste ##, can appear in both object-like and
	     function-like macros, but not at the end.  */
	  if (following_paste_op)
	    {
	      cpp_error (pfile, CPP_DL_ERROR, paste_op_error_msg);
	      goto out;
	    }
	  if (!vaopt_tracker.completed ())
	    goto out;
	  break;
	}

      /* Paste operator constraint 6.10.3.3.1.  */
      if (token->type == CPP_PASTE)
	{
	  /* Token-paste ##, can appear in both object-like and
	     function-like macros, but not at the beginning.  */
	  if (macro->count == 1)
	    {
	      cpp_error (pfile, CPP_DL_ERROR, paste_op_error_msg);
	      goto out;
	    }

	  if (following_paste_op)
	    {
	      /* Consecutive paste operators.  This one will be moved
		 to the end.  */
	      num_extra_tokens++;
	      token->val.token_no = macro->count - 1;
	    }
	  else
	    {
	      /* Drop the paste operator.  */
	      --macro->count;
	      token[-1].flags |= PASTE_LEFT;
	      if (token->flags & DIGRAPH)
		token[-1].flags |= SP_DIGRAPH;
	      if (token->flags & PREV_WHITE)
		token[-1].flags |= SP_PREV_WHITE;
	    }
	  following_paste_op = true;
	}
      else
	following_paste_op = false;

      if (vaopt_tracker.update (token) == vaopt_state::ERROR)
	goto out;
    }

  /* We're committed to winning now.  */
  ok = true;

  /* Don't count the CPP_EOF.  */
  macro->count--;

  macro = (cpp_macro *)_cpp_commit_buff
    (pfile, sizeof (cpp_macro) - sizeof (cpp_token)
     + sizeof (cpp_token) * macro->count);

  /* Clear whitespace on first token for warn_of_redefinition().  */
  if (macro->count)
    macro->exp.tokens[0].flags &= ~PREV_WHITE;

  if (num_extra_tokens)
    {
      /* Place second and subsequent ## or %:%: tokens in sequences of
	 consecutive such tokens at the end of the list to preserve
	 information about where they appear, how they are spelt and
	 whether they are preceded by whitespace without otherwise
	 interfering with macro expansion.   Remember, this is
	 extremely rare, so efficiency is not a priority.  */
      cpp_token *temp = (cpp_token *)_cpp_reserve_room
	(pfile, 0, num_extra_tokens * sizeof (cpp_token));
      unsigned extra_ix = 0, norm_ix = 0;
      cpp_token *exp = macro->exp.tokens;
      for (unsigned ix = 0; ix != macro->count; ix++)
	if (exp[ix].type == CPP_PASTE)
	  temp[extra_ix++] = exp[ix];
	else
	  exp[norm_ix++] = exp[ix];
      memcpy (&exp[norm_ix], temp, num_extra_tokens * sizeof (cpp_token));

      /* Record there are extra tokens.  */
      macro->extra_tokens = 1;
    }

 out:
  pfile->state.va_args_ok = 0;
  _cpp_unsave_parameters (pfile, nparms);

  return ok ? macro : NULL;
}

cpp_macro *
_cpp_new_macro (cpp_reader *pfile, cpp_macro_kind kind, void *placement)
{
  cpp_macro *macro = (cpp_macro *) placement;

  macro->line = pfile->directive_line;
  macro->parm.params = 0;
  macro->lazy = 0;
  macro->paramc = 0;
  macro->variadic = 0;
  macro->used = !CPP_OPTION (pfile, warn_unused_macros);
  macro->count = 0;
  macro->fun_like = 0;
  macro->extra_tokens = 0;
  /* To suppress some diagnostics.  */
  macro->syshdr = pfile->buffer && pfile->buffer->sysp != 0;

  macro->kind = kind;

  return macro;
}

/* Parse a macro and save its expansion.  Returns nonzero on success.  */
bool
_cpp_create_definition (cpp_reader *pfile, cpp_hashnode *node)
{
  cpp_macro *macro;

  if (CPP_OPTION (pfile, traditional))
    macro = _cpp_create_trad_definition (pfile);
  else
    macro = create_iso_definition (pfile);

  if (!macro)
    return false;

  if (cpp_macro_p (node))
    {
      if (CPP_OPTION (pfile, warn_unused_macros))
	_cpp_warn_if_unused_macro (pfile, node, NULL);

      if (warn_of_redefinition (pfile, node, macro))
	{
          const int reason
	    = (cpp_builtin_macro_p (node) && !(node->flags & NODE_WARN))
	    ? CPP_W_BUILTIN_MACRO_REDEFINED : CPP_W_NONE;

	  bool warned = 
	    cpp_pedwarning_with_line (pfile, reason,
				      pfile->directive_line, 0,
				      "\"%s\" redefined", NODE_NAME (node));

	  if (warned && cpp_user_macro_p (node))
	    cpp_error_with_line (pfile, CPP_DL_NOTE,
				 node->value.macro->line, 0,
			 "this is the location of the previous definition");
	}
      _cpp_free_definition (node);
    }

  /* Enter definition in hash table.  */
  node->type = NT_USER_MACRO;
  node->value.macro = macro;
  if (! ustrncmp (NODE_NAME (node), DSC ("__STDC_"))
      && ustrcmp (NODE_NAME (node), (const uchar *) "__STDC_FORMAT_MACROS")
      /* __STDC_LIMIT_MACROS and __STDC_CONSTANT_MACROS are mentioned
	 in the C standard, as something that one must use in C++.
	 However DR#593 and C++11 indicate that they play no role in C++.
	 We special-case them anyway.  */
      && ustrcmp (NODE_NAME (node), (const uchar *) "__STDC_LIMIT_MACROS")
      && ustrcmp (NODE_NAME (node), (const uchar *) "__STDC_CONSTANT_MACROS"))
    node->flags |= NODE_WARN;

  /* If user defines one of the conditional macros, remove the
     conditional flag */
  node->flags &= ~NODE_CONDITIONAL;

  return true;
}

extern void
cpp_define_lazily (cpp_reader *pfile, cpp_hashnode *node, unsigned num)
{
  cpp_macro *macro = node->value.macro;

  gcc_checking_assert (pfile->cb.user_lazy_macro && macro && num < UCHAR_MAX);

  macro->lazy = num + 1;
}

/* Notify the use of NODE in a macro-aware context (i.e. expanding it,
   or testing its existance).  Also applies any lazy definition.  */

extern void
_cpp_notify_macro_use (cpp_reader *pfile, cpp_hashnode *node)
{
  node->flags |= NODE_USED;
  switch (node->type)
    {
    case NT_USER_MACRO:
      {
	cpp_macro *macro = node->value.macro;
	if (macro->lazy)
	  {
	    pfile->cb.user_lazy_macro (pfile, macro, macro->lazy - 1);
	    macro->lazy = 0;
	  }
      }
      /* FALLTHROUGH.  */

    case NT_BUILTIN_MACRO:
      if (pfile->cb.used_define)
	pfile->cb.used_define (pfile, pfile->directive_line, node);
      break;

    case NT_VOID:
      if (pfile->cb.used_undef)
	pfile->cb.used_undef (pfile, pfile->directive_line, node);
      break;

    default:
      abort ();
    }
}

/* Warn if a token in STRING matches one of a function-like MACRO's
   parameters.  */
static void
check_trad_stringification (cpp_reader *pfile, const cpp_macro *macro,
			    const cpp_string *string)
{
  unsigned int i, len;
  const uchar *p, *q, *limit;

  /* Loop over the string.  */
  limit = string->text + string->len - 1;
  for (p = string->text + 1; p < limit; p = q)
    {
      /* Find the start of an identifier.  */
      while (p < limit && !is_idstart (*p))
	p++;

      /* Find the end of the identifier.  */
      q = p;
      while (q < limit && is_idchar (*q))
	q++;

      len = q - p;

      /* Loop over the function macro arguments to see if the
	 identifier inside the string matches one of them.  */
      for (i = 0; i < macro->paramc; i++)
	{
	  const cpp_hashnode *node = macro->parm.params[i];

	  if (NODE_LEN (node) == len
	      && !memcmp (p, NODE_NAME (node), len))
	    {
	      cpp_warning (pfile, CPP_W_TRADITIONAL,
	   "macro argument \"%s\" would be stringified in traditional C",
			 NODE_NAME (node));
	      break;
	    }
	}
    }
}

/* Returns the name, arguments and expansion of a macro, in a format
   suitable to be read back in again, and therefore also for DWARF 2
   debugging info.  e.g. "PASTE(X, Y) X ## Y", or "MACNAME EXPANSION".
   Caller is expected to generate the "#define" bit if needed.  The
   returned text is temporary, and automatically freed later.  */
const unsigned char *
cpp_macro_definition (cpp_reader *pfile, cpp_hashnode *node)
{
  unsigned int i, len;
  unsigned char *buffer;

  gcc_checking_assert (cpp_user_macro_p (node));

  const cpp_macro *macro = node->value.macro;

  /* Calculate length.  */
  len = NODE_LEN (node) * 10 + 2;		/* ' ' and NUL.  */
  if (macro->fun_like)
    {
      len += 4;		/* "()" plus possible final ".." of named
			   varargs (we have + 1 below).  */
      for (i = 0; i < macro->paramc; i++)
	len += NODE_LEN (macro->parm.params[i]) + 1; /* "," */
    }

  /* This should match below where we fill in the buffer.  */
  if (CPP_OPTION (pfile, traditional))
    len += _cpp_replacement_text_len (macro);
  else
    {
      unsigned int count = macro_real_token_count (macro);
      for (i = 0; i < count; i++)
	{
	  const cpp_token *token = &macro->exp.tokens[i];

	  if (token->type == CPP_MACRO_ARG)
	    len += NODE_LEN (token->val.macro_arg.spelling);
	  else
	    len += cpp_token_len (token);

	  if (token->flags & STRINGIFY_ARG)
	    len++;			/* "#" */
	  if (token->flags & PASTE_LEFT)
	    len += 3;		/* " ##" */
	  if (token->flags & PREV_WHITE)
	    len++;              /* " " */
	}
    }

  if (len > pfile->macro_buffer_len)
    {
      pfile->macro_buffer = XRESIZEVEC (unsigned char,
                                        pfile->macro_buffer, len);
      pfile->macro_buffer_len = len;
    }

  /* Fill in the buffer.  Start with the macro name.  */
  buffer = pfile->macro_buffer;
  buffer = _cpp_spell_ident_ucns (buffer, node);

  /* Parameter names.  */
  if (macro->fun_like)
    {
      *buffer++ = '(';
      for (i = 0; i < macro->paramc; i++)
	{
	  cpp_hashnode *param = macro->parm.params[i];

	  if (param != pfile->spec_nodes.n__VA_ARGS__)
	    {
	      memcpy (buffer, NODE_NAME (param), NODE_LEN (param));
	      buffer += NODE_LEN (param);
	    }

	  if (i + 1 < macro->paramc)
	    /* Don't emit a space after the comma here; we're trying
	       to emit a Dwarf-friendly definition, and the Dwarf spec
	       forbids spaces in the argument list.  */
	    *buffer++ = ',';
	  else if (macro->variadic)
	    *buffer++ = '.', *buffer++ = '.', *buffer++ = '.';
	}
      *buffer++ = ')';
    }

  /* The Dwarf spec requires a space after the macro name, even if the
     definition is the empty string.  */
  *buffer++ = ' ';

  if (CPP_OPTION (pfile, traditional))
    buffer = _cpp_copy_replacement_text (macro, buffer);
  else if (macro->count)
  /* Expansion tokens.  */
    {
      unsigned int count = macro_real_token_count (macro);
      for (i = 0; i < count; i++)
	{
	  const cpp_token *token = &macro->exp.tokens[i];

	  if (token->flags & PREV_WHITE)
	    *buffer++ = ' ';
	  if (token->flags & STRINGIFY_ARG)
	    *buffer++ = '#';

	  if (token->type == CPP_MACRO_ARG)
	    {
	      memcpy (buffer,
		      NODE_NAME (token->val.macro_arg.spelling),
		      NODE_LEN (token->val.macro_arg.spelling));
	      buffer += NODE_LEN (token->val.macro_arg.spelling);
	    }
	  else
	    buffer = cpp_spell_token (pfile, token, buffer, true);

	  if (token->flags & PASTE_LEFT)
	    {
	      *buffer++ = ' ';
	      *buffer++ = '#';
	      *buffer++ = '#';
	      /* Next has PREV_WHITE; see _cpp_create_definition.  */
	    }
	}
    }

  *buffer = '\0';
  return pfile->macro_buffer;
}

/* Get the line at which the macro was defined.  */

source_location
cpp_macro_definition_location (cpp_hashnode *node)
{
  return node->value.macro->line;
}
