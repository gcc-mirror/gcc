/* CPP Library.
   Copyright (C) 1986-2024 Free Software Foundation, Inc.
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
#include "localedir.h"
#include "filenames.h"

#ifndef ENABLE_CANONICAL_SYSTEM_HEADERS
#ifdef HAVE_DOS_BASED_FILE_SYSTEM
#define ENABLE_CANONICAL_SYSTEM_HEADERS 1
#else
#define ENABLE_CANONICAL_SYSTEM_HEADERS 0
#endif
#endif

static void init_library (void);
static void mark_named_operators (cpp_reader *, int);
static bool read_original_filename (cpp_reader *);
static void read_original_directory (cpp_reader *);
static void post_options (cpp_reader *);

/* If we have designated initializers (GCC >2.7) these tables can be
   initialized, constant data.  Similarly for C++14 and later.
   Otherwise, they have to be filled in at runtime.  */
#if HAVE_DESIGNATED_INITIALIZERS

#define init_trigraph_map()  /* Nothing.  */
#define TRIGRAPH_MAP \
__extension__ const uchar _cpp_trigraph_map[UCHAR_MAX + 1] = {

#define END };
#define s(p, v) [p] = v,

#elif __cpp_constexpr >= 201304L

#define init_trigraph_map()  /* Nothing.  */
#define TRIGRAPH_MAP \
constexpr _cpp_trigraph_map_s::_cpp_trigraph_map_s () : map {} {
#define END } \
constexpr _cpp_trigraph_map_s _cpp_trigraph_map_d;
#define s(p, v) map[p] = v;

#else

#define TRIGRAPH_MAP uchar _cpp_trigraph_map[UCHAR_MAX + 1] = { 0 }; \
 static void init_trigraph_map (void) { \
 unsigned char *x = _cpp_trigraph_map;

#define END }
#define s(p, v) x[p] = v;

#endif

TRIGRAPH_MAP
  s('=', '#')	s(')', ']')	s('!', '|')
  s('(', '[')	s('\'', '^')	s('>', '}')
  s('/', '\\')	s('<', '{')	s('-', '~')
END

#undef s
#undef END
#undef TRIGRAPH_MAP

/* A set of booleans indicating what CPP features each source language
   requires.  */
struct lang_flags
{
  unsigned int c99 : 1;
  unsigned int cplusplus : 1;
  unsigned int extended_numbers : 1;
  unsigned int extended_identifiers : 1;
  unsigned int c11_identifiers : 1;
  unsigned int xid_identifiers : 1;
  unsigned int std : 1;
  unsigned int digraphs : 1;
  unsigned int uliterals : 1;
  unsigned int rliterals : 1;
  unsigned int user_literals : 1;
  unsigned int binary_constants : 1;
  unsigned int digit_separators : 1;
  unsigned int trigraphs : 1;
  unsigned int utf8_char_literals : 1;
  unsigned int va_opt : 1;
  unsigned int scope : 1;
  unsigned int dfp_constants : 1;
  unsigned int size_t_literals : 1;
  unsigned int elifdef : 1;
  unsigned int warning_directive : 1;
  unsigned int delimited_escape_seqs : 1;
  unsigned int true_false : 1;
  unsigned int embed : 1;
};

static const struct lang_flags lang_defaults[] = {
  /*					       u         e w
					 b d   8         l a   t
			     x         u i i   c v s   s i r d r e
		       x     i   d u r d n g t h a c   z f n e u m
		   c c n x c d s i l l l c s r l o o d l d d l f b
		   9 + u i 1 i t g i i i s e i i p p f i e i i a e
		   9 + m d 1 d d r t t t t p g t t e p t f r m l d  */
  /* GNUC89   */ { 0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0 },
  /* GNUC99   */ { 1,0,1,1,0,0,0,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0 },
  /* GNUC11   */ { 1,0,1,1,1,0,0,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0 },
  /* GNUC17   */ { 1,0,1,1,1,0,0,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0 },
  /* GNUC23   */ { 1,0,1,1,1,1,0,1,1,1,0,1,1,0,1,1,1,1,0,1,1,0,1,1 },
  /* GNUC2Y   */ { 1,0,1,1,1,1,0,1,1,1,0,1,1,0,1,1,1,1,0,1,1,0,1,1 },
  /* STDC89   */ { 0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0 },
  /* STDC94   */ { 0,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0 },
  /* STDC99   */ { 1,0,1,1,0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0 },
  /* STDC11   */ { 1,0,1,1,1,0,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0 },
  /* STDC17   */ { 1,0,1,1,1,0,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0 },
  /* STDC23   */ { 1,0,1,1,1,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,0,1,1 },
  /* STDC2Y   */ { 1,0,1,1,1,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,0,1,1 },
  /* GNUCXX   */ { 0,1,1,1,0,1,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,1,0 },
  /* CXX98    */ { 0,1,0,1,0,1,1,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0 },
  /* GNUCXX11 */ { 1,1,1,1,1,1,0,1,1,1,1,0,0,0,0,1,1,0,0,0,0,0,1,0 },
  /* CXX11    */ { 1,1,0,1,1,1,1,1,1,1,1,0,0,1,0,0,1,0,0,0,0,0,1,0 },
  /* GNUCXX14 */ { 1,1,1,1,1,1,0,1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,1,0 },
  /* CXX14    */ { 1,1,0,1,1,1,1,1,1,1,1,1,1,1,0,0,1,0,0,0,0,0,1,0 },
  /* GNUCXX17 */ { 1,1,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,0,0,0,0,0,1,0 },
  /* CXX17    */ { 1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,1,0,0,0,0,0,1,0 },
  /* GNUCXX20 */ { 1,1,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,0,0,0,0,0,1,0 },
  /* CXX20    */ { 1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,0,0,0,0,1,0 },
  /* GNUCXX23 */ { 1,1,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,0,1,1,1,1,1,0 },
  /* CXX23    */ { 1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,1,1,1,1,1,0 },
  /* GNUCXX26 */ { 1,1,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,0,1,1,1,1,1,0 },
  /* CXX26    */ { 1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,1,1,1,1,1,0 },
  /* ASM      */ { 0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 }
};

/* Sets internal flags correctly for a given language.  */
void
cpp_set_lang (cpp_reader *pfile, enum c_lang lang)
{
  const struct lang_flags *l = &lang_defaults[(int) lang];

  CPP_OPTION (pfile, lang) = lang;

  CPP_OPTION (pfile, c99)			 = l->c99;
  CPP_OPTION (pfile, cplusplus)			 = l->cplusplus;
  CPP_OPTION (pfile, extended_numbers)		 = l->extended_numbers;
  CPP_OPTION (pfile, extended_identifiers)	 = l->extended_identifiers;
  CPP_OPTION (pfile, c11_identifiers)		 = l->c11_identifiers;
  CPP_OPTION (pfile, xid_identifiers)		 = l->xid_identifiers;
  CPP_OPTION (pfile, std)			 = l->std;
  CPP_OPTION (pfile, digraphs)			 = l->digraphs;
  CPP_OPTION (pfile, uliterals)			 = l->uliterals;
  CPP_OPTION (pfile, rliterals)			 = l->rliterals;
  CPP_OPTION (pfile, user_literals)		 = l->user_literals;
  CPP_OPTION (pfile, binary_constants)		 = l->binary_constants;
  CPP_OPTION (pfile, digit_separators)		 = l->digit_separators;
  CPP_OPTION (pfile, trigraphs)			 = l->trigraphs;
  CPP_OPTION (pfile, utf8_char_literals)	 = l->utf8_char_literals;
  CPP_OPTION (pfile, va_opt)			 = l->va_opt;
  CPP_OPTION (pfile, scope)			 = l->scope;
  CPP_OPTION (pfile, dfp_constants)		 = l->dfp_constants;
  CPP_OPTION (pfile, size_t_literals)		 = l->size_t_literals;
  CPP_OPTION (pfile, elifdef)			 = l->elifdef;
  CPP_OPTION (pfile, warning_directive)		 = l->warning_directive;
  CPP_OPTION (pfile, delimited_escape_seqs)	 = l->delimited_escape_seqs;
  CPP_OPTION (pfile, true_false)		 = l->true_false;
  CPP_OPTION (pfile, embed)			 = l->embed;
}

/* Initialize library global state.  */
static void
init_library (void)
{
  static int initialized = 0;

  if (! initialized)
    {
      initialized = 1;

      _cpp_init_lexer ();

      /* Set up the trigraph map.  This doesn't need to do anything if
	 we were compiled with a compiler that supports C99 designated
	 initializers.  */
      init_trigraph_map ();

#ifdef ENABLE_NLS
       (void) bindtextdomain (PACKAGE, LOCALEDIR);
#endif
    }
}

/* Initialize a cpp_reader structure.  */
cpp_reader *
cpp_create_reader (enum c_lang lang, cpp_hash_table *table,
		   class line_maps *line_table, cpp_hash_table *extra_table)
{
  cpp_reader *pfile;

  /* Initialize this instance of the library if it hasn't been already.  */
  init_library ();

  pfile = XCNEW (cpp_reader);
  memset (&pfile->base_context, 0, sizeof (pfile->base_context));

  cpp_set_lang (pfile, lang);
  CPP_OPTION (pfile, warn_multichar) = 1;
  CPP_OPTION (pfile, discard_comments) = 1;
  CPP_OPTION (pfile, discard_comments_in_macro_exp) = 1;
  CPP_OPTION (pfile, max_include_depth) = 200;
  CPP_OPTION (pfile, operator_names) = 1;
  CPP_OPTION (pfile, warn_trigraphs) = 2;
  CPP_OPTION (pfile, warn_endif_labels) = 1;
  CPP_OPTION (pfile, cpp_warn_c90_c99_compat) = -1;
  CPP_OPTION (pfile, cpp_warn_c11_c23_compat) = -1;
  CPP_OPTION (pfile, cpp_warn_cxx11_compat) = 0;
  CPP_OPTION (pfile, cpp_warn_cxx20_compat) = 0;
  CPP_OPTION (pfile, cpp_warn_deprecated) = 1;
  CPP_OPTION (pfile, cpp_warn_long_long) = 0;
  CPP_OPTION (pfile, dollars_in_ident) = 1;
  CPP_OPTION (pfile, warn_dollars) = 1;
  CPP_OPTION (pfile, warn_variadic_macros) = 1;
  CPP_OPTION (pfile, warn_builtin_macro_redefined) = 1;
  CPP_OPTION (pfile, cpp_warn_implicit_fallthrough) = 0;
  CPP_OPTION (pfile, warn_header_guard) = 0;
  /* By default, track locations of tokens resulting from macro
     expansion.  The '2' means, track the locations with the highest
     accuracy.  Read the comments for struct
     cpp_options::track_macro_expansion to learn about the other
     values.  */
  CPP_OPTION (pfile, track_macro_expansion) = 2;
  CPP_OPTION (pfile, warn_normalize) = normalized_C;
  CPP_OPTION (pfile, warn_literal_suffix) = 1;
  CPP_OPTION (pfile, canonical_system_headers)
      = ENABLE_CANONICAL_SYSTEM_HEADERS;
  CPP_OPTION (pfile, ext_numeric_literals) = 1;
  CPP_OPTION (pfile, warn_date_time) = 0;
  CPP_OPTION (pfile, cpp_warn_bidirectional) = bidirectional_unpaired;
  CPP_OPTION (pfile, cpp_warn_invalid_utf8) = 0;
  CPP_OPTION (pfile, cpp_warn_unicode) = 1;
  CPP_OPTION (pfile, cpp_input_charset_explicit) = 0;

  /* Default CPP arithmetic to something sensible for the host for the
     benefit of dumb users like fix-header.  */
  CPP_OPTION (pfile, precision) = CHAR_BIT * sizeof (long);
  CPP_OPTION (pfile, char_precision) = CHAR_BIT;
  CPP_OPTION (pfile, wchar_precision) = CHAR_BIT * sizeof (int);
  CPP_OPTION (pfile, int_precision) = CHAR_BIT * sizeof (int);
  CPP_OPTION (pfile, unsigned_char) = 0;
  CPP_OPTION (pfile, unsigned_wchar) = 1;
  CPP_OPTION (pfile, unsigned_utf8char) = 1;
  CPP_OPTION (pfile, bytes_big_endian) = 1;  /* does not matter */

  /* Default to no charset conversion.  */
  CPP_OPTION (pfile, narrow_charset) = _cpp_default_encoding ();
  CPP_OPTION (pfile, wide_charset) = 0;

  /* Default the input character set to UTF-8.  */
  CPP_OPTION (pfile, input_charset) = _cpp_default_encoding ();

  /* A fake empty "directory" used as the starting point for files
     looked up without a search path.  Name cannot be '/' because we
     don't want to prepend anything at all to filenames using it.  All
     other entries are correct zero-initialized.  */
  pfile->no_search_path.name = (char *) "";

  /* Initialize the line map.  */
  pfile->line_table = line_table;

  /* Initialize lexer state.  */
  pfile->state.save_comments = ! CPP_OPTION (pfile, discard_comments);

  /* Set up static tokens.  */
  pfile->avoid_paste.type = CPP_PADDING;
  pfile->avoid_paste.val.source = NULL;
  pfile->avoid_paste.src_loc = 0;
  pfile->endarg.type = CPP_EOF;
  pfile->endarg.flags = 0;
  pfile->endarg.src_loc = 0;

  /* Create a token buffer for the lexer.  */
  _cpp_init_tokenrun (&pfile->base_run, 250);
  pfile->cur_run = &pfile->base_run;
  pfile->cur_token = pfile->base_run.base;

  /* Initialize the base context.  */
  pfile->context = &pfile->base_context;
  pfile->base_context.c.macro = 0;
  pfile->base_context.prev = pfile->base_context.next = 0;

  /* Aligned and unaligned storage.  */
  pfile->a_buff = _cpp_get_buff (pfile, 0);
  pfile->u_buff = _cpp_get_buff (pfile, 0);

  /* Initialize table for push_macro/pop_macro.  */
  pfile->pushed_macros = 0;

  /* Do not force token locations by default.  */
  pfile->forced_token_location = 0;

  /* Note the timestamp is unset.  */
  pfile->time_stamp = time_t (-1);
  pfile->time_stamp_kind = 0;

  /* The expression parser stack.  */
  _cpp_expand_op_stack (pfile);

  /* Initialize the buffer obstack.  */
  obstack_specify_allocation (&pfile->buffer_ob, 0, 0, xmalloc, free);

  _cpp_init_files (pfile);

  _cpp_init_hashtable (pfile, table, extra_table);

  return pfile;
}

/* Set the line_table entry in PFILE.  This is called after reading a
   PCH file, as the old line_table will be incorrect.  */
void
cpp_set_line_map (cpp_reader *pfile, class line_maps *line_table)
{
  pfile->line_table = line_table;
}

/* Free resources used by PFILE.  Accessing PFILE after this function
   returns leads to undefined behavior.  Returns the error count.  */
void
cpp_destroy (cpp_reader *pfile)
{
  cpp_context *context, *contextn;
  struct def_pragma_macro *pmacro;
  tokenrun *run, *runn;
  int i;

  free (pfile->op_stack);

  while (CPP_BUFFER (pfile) != NULL)
    _cpp_pop_buffer (pfile);

  free (pfile->out.base);

  if (pfile->macro_buffer)
    {
      free (pfile->macro_buffer);
      pfile->macro_buffer = NULL;
      pfile->macro_buffer_len = 0;
    }

  if (pfile->deps)
    deps_free (pfile->deps);
  obstack_free (&pfile->buffer_ob, 0);

  _cpp_destroy_hashtable (pfile);
  _cpp_cleanup_files (pfile);
  _cpp_destroy_iconv (pfile);

  _cpp_free_buff (pfile->a_buff);
  _cpp_free_buff (pfile->u_buff);
  _cpp_free_buff (pfile->free_buffs);

  for (run = &pfile->base_run; run; run = runn)
    {
      runn = run->next;
      free (run->base);
      if (run != &pfile->base_run)
	free (run);
    }

  for (context = pfile->base_context.next; context; context = contextn)
    {
      contextn = context->next;
      free (context);
    }

  if (pfile->comments.entries)
    {
      for (i = 0; i < pfile->comments.count; i++)
	free (pfile->comments.entries[i].comment);

      free (pfile->comments.entries);
    }
  if (pfile->pushed_macros)
    {
      do
	{
	  pmacro = pfile->pushed_macros;
	  pfile->pushed_macros = pmacro->next;
	  free (pmacro->name);
	  free (pmacro);
	}
      while (pfile->pushed_macros);
    }

  free (pfile);
}

/* This structure defines one built-in identifier.  A node will be
   entered in the hash table under the name NAME, with value VALUE.

   There are two tables of these.  builtin_array holds all the
   "builtin" macros: these are handled by builtin_macro() in
   macro.cc.  Builtin is somewhat of a misnomer -- the property of
   interest is that these macros require special code to compute their
   expansions.  The value is a "cpp_builtin_type" enumerator.

   operator_array holds the C++ named operators.  These are keywords
   which act as aliases for punctuators.  In C++, they cannot be
   altered through #define, and #if recognizes them as operators.  In
   C, these are not entered into the hash table at all (but see
   <iso646.h>).  The value is a token-type enumerator.  */
struct builtin_macro
{
  const uchar *const name;
  const unsigned short len;
  const unsigned short value;
  const bool always_warn_if_redefined;
};

#define B(n, t, f)    { DSC(n), t, f }
static const struct builtin_macro builtin_array[] =
{
  B("__TIMESTAMP__",	 BT_TIMESTAMP,     false),
  B("__TIME__",		 BT_TIME,          false),
  B("__DATE__",		 BT_DATE,          false),
  B("__FILE__",		 BT_FILE,          false),
  B("__FILE_NAME__",	 BT_FILE_NAME,     false),
  B("__BASE_FILE__",	 BT_BASE_FILE,     false),
  B("__LINE__",		 BT_SPECLINE,      true),
  B("__INCLUDE_LEVEL__", BT_INCLUDE_LEVEL, true),
  B("__COUNTER__",	 BT_COUNTER,       true),
  /* Make sure to update the list of built-in
     function-like macros in traditional.cc:
     fun_like_macro() when adding more following */
  B("__has_attribute",	 BT_HAS_ATTRIBUTE, true),
  B("__has_c_attribute", BT_HAS_STD_ATTRIBUTE, true),
  B("__has_cpp_attribute", BT_HAS_ATTRIBUTE, true),
  B("__has_builtin",	 BT_HAS_BUILTIN,   true),
  B("__has_include",	 BT_HAS_INCLUDE,   true),
  B("__has_include_next",BT_HAS_INCLUDE_NEXT,   true),
  B("__has_embed",	 BT_HAS_EMBED, true),
  B("__has_feature",	 BT_HAS_FEATURE, true),
  B("__has_extension",	 BT_HAS_EXTENSION, true),
  /* Keep builtins not used for -traditional-cpp at the end, and
     update init_builtins() if any more are added.  */
  B("_Pragma",		 BT_PRAGMA,        true),
  B("__STDC__",		 BT_STDC,          true),
};
#undef B

struct builtin_operator
{
  const uchar *const name;
  const unsigned short len;
  const unsigned short value;
};

#define B(n, t)    { DSC(n), t }
static const struct builtin_operator operator_array[] =
{
  B("and",	CPP_AND_AND),
  B("and_eq",	CPP_AND_EQ),
  B("bitand",	CPP_AND),
  B("bitor",	CPP_OR),
  B("compl",	CPP_COMPL),
  B("not",	CPP_NOT),
  B("not_eq",	CPP_NOT_EQ),
  B("or",	CPP_OR_OR),
  B("or_eq",	CPP_OR_EQ),
  B("xor",	CPP_XOR),
  B("xor_eq",	CPP_XOR_EQ)
};
#undef B

/* Mark the C++ named operators in the hash table.  */
static void
mark_named_operators (cpp_reader *pfile, int flags)
{
  const struct builtin_operator *b;

  for (b = operator_array;
       b < (operator_array + ARRAY_SIZE (operator_array));
       b++)
    {
      cpp_hashnode *hp = cpp_lookup (pfile, b->name, b->len);
      hp->flags |= flags;
      hp->is_directive = 0;
      hp->directive_index = b->value;
    }
}

/* Helper function of cpp_type2name. Return the string associated with
   named operator TYPE.  */
const char *
cpp_named_operator2name (enum cpp_ttype type)
{
  const struct builtin_operator *b;

  for (b = operator_array;
       b < (operator_array + ARRAY_SIZE (operator_array));
       b++)
    {
      if (type == b->value)
	return (const char *) b->name;
    }

  return NULL;
}

void
cpp_init_special_builtins (cpp_reader *pfile)
{
  const struct builtin_macro *b;
  size_t n = ARRAY_SIZE (builtin_array);

  if (CPP_OPTION (pfile, traditional))
    n -= 2;
  else if (! CPP_OPTION (pfile, stdc_0_in_system_headers)
	   || CPP_OPTION (pfile, std))
    n--;

  for (b = builtin_array; b < builtin_array + n; b++)
    {
      if ((b->value == BT_HAS_ATTRIBUTE
	   || b->value == BT_HAS_STD_ATTRIBUTE
	   || b->value == BT_HAS_BUILTIN)
	  && (CPP_OPTION (pfile, lang) == CLK_ASM
	      || pfile->cb.has_attribute == NULL))
	continue;
      cpp_hashnode *hp = cpp_lookup (pfile, b->name, b->len);
      hp->type = NT_BUILTIN_MACRO;
      if (b->always_warn_if_redefined)
	hp->flags |= NODE_WARN;
      hp->value.builtin = (enum cpp_builtin_type) b->value;
    }
}

/* Restore macro C to builtin macro definition.  */

void
_cpp_restore_special_builtin (cpp_reader *pfile, struct def_pragma_macro *c)
{
  size_t len = strlen (c->name);

  for (const struct builtin_macro *b = builtin_array;
       b < builtin_array + ARRAY_SIZE (builtin_array); b++)
    if (b->len == len && memcmp (c->name, b->name, len + 1) == 0)
      {
	cpp_hashnode *hp = cpp_lookup (pfile, b->name, b->len);
	hp->type = NT_BUILTIN_MACRO;
	if (b->always_warn_if_redefined)
	  hp->flags |= NODE_WARN;
	hp->value.builtin = (enum cpp_builtin_type) b->value;
      }
}

/* Read the builtins table above and enter them, and language-specific
   macros, into the hash table.  HOSTED is true if this is a hosted
   environment.  */
void
cpp_init_builtins (cpp_reader *pfile, int hosted)
{
  cpp_init_special_builtins (pfile);

  if (!CPP_OPTION (pfile, traditional)
      && (! CPP_OPTION (pfile, stdc_0_in_system_headers)
	  || CPP_OPTION (pfile, std)))
    _cpp_define_builtin (pfile, "__STDC__ 1");

  if (CPP_OPTION (pfile, cplusplus))
    {
      /* C++26 is not yet a standard.  For now, use an invalid
	 year/month, 202400L, which is larger than 202302L.  */
      if (CPP_OPTION (pfile, lang) == CLK_CXX26
	  || CPP_OPTION (pfile, lang) == CLK_GNUCXX26)
	_cpp_define_builtin (pfile, "__cplusplus 202400L");
      else if (CPP_OPTION (pfile, lang) == CLK_CXX23
	  || CPP_OPTION (pfile, lang) == CLK_GNUCXX23)
	_cpp_define_builtin (pfile, "__cplusplus 202302L");
      else if (CPP_OPTION (pfile, lang) == CLK_CXX20
	  || CPP_OPTION (pfile, lang) == CLK_GNUCXX20)
	_cpp_define_builtin (pfile, "__cplusplus 202002L");
      else if (CPP_OPTION (pfile, lang) == CLK_CXX17
	  || CPP_OPTION (pfile, lang) == CLK_GNUCXX17)
	_cpp_define_builtin (pfile, "__cplusplus 201703L");
      else if (CPP_OPTION (pfile, lang) == CLK_CXX14
	  || CPP_OPTION (pfile, lang) == CLK_GNUCXX14)
	_cpp_define_builtin (pfile, "__cplusplus 201402L");
      else if (CPP_OPTION (pfile, lang) == CLK_CXX11
	       || CPP_OPTION (pfile, lang) == CLK_GNUCXX11)
	_cpp_define_builtin (pfile, "__cplusplus 201103L");
      else
	_cpp_define_builtin (pfile, "__cplusplus 199711L");
    }
  else if (CPP_OPTION (pfile, lang) == CLK_ASM)
    _cpp_define_builtin (pfile, "__ASSEMBLER__ 1");
  else if (CPP_OPTION (pfile, lang) == CLK_STDC94)
    _cpp_define_builtin (pfile, "__STDC_VERSION__ 199409L");
  else if (CPP_OPTION (pfile, lang) == CLK_STDC23
	   || CPP_OPTION (pfile, lang) == CLK_GNUC23)
    _cpp_define_builtin (pfile, "__STDC_VERSION__ 202311L");
  else if (CPP_OPTION (pfile, lang) == CLK_STDC2Y
	   || CPP_OPTION (pfile, lang) == CLK_GNUC2Y)
    _cpp_define_builtin (pfile, "__STDC_VERSION__ 202500L");
  else if (CPP_OPTION (pfile, lang) == CLK_STDC17
	   || CPP_OPTION (pfile, lang) == CLK_GNUC17)
    _cpp_define_builtin (pfile, "__STDC_VERSION__ 201710L");
  else if (CPP_OPTION (pfile, lang) == CLK_STDC11
	   || CPP_OPTION (pfile, lang) == CLK_GNUC11)
    _cpp_define_builtin (pfile, "__STDC_VERSION__ 201112L");
  else if (CPP_OPTION (pfile, c99))
    _cpp_define_builtin (pfile, "__STDC_VERSION__ 199901L");

  if (CPP_OPTION (pfile, uliterals)
      && !(CPP_OPTION (pfile, cplusplus)
	   && (CPP_OPTION (pfile, lang) == CLK_GNUCXX
	    || CPP_OPTION (pfile, lang) == CLK_CXX98)))
    {
      _cpp_define_builtin (pfile, "__STDC_UTF_16__ 1");
      _cpp_define_builtin (pfile, "__STDC_UTF_32__ 1");
    }

  if (hosted)
    _cpp_define_builtin (pfile, "__STDC_HOSTED__ 1");
  else
    _cpp_define_builtin (pfile, "__STDC_HOSTED__ 0");

  _cpp_define_builtin (pfile, "__STDC_EMBED_NOT_FOUND__ 0");
  _cpp_define_builtin (pfile, "__STDC_EMBED_FOUND__ 1");
  _cpp_define_builtin (pfile, "__STDC_EMBED_EMPTY__ 2");

  if (CPP_OPTION (pfile, objc))
    _cpp_define_builtin (pfile, "__OBJC__ 1");
}

/* Sanity-checks are dependent on command-line options, so it is
   called as a subroutine of cpp_read_main_file.  */
#if CHECKING_P
static void sanity_checks (cpp_reader *);
static void sanity_checks (cpp_reader *pfile)
{
  cppchar_t test = 0;
  size_t max_precision = 2 * CHAR_BIT * sizeof (cpp_num_part);

  /* Sanity checks for assumptions about CPP arithmetic and target
     type precisions made by cpplib.  */
  test--;
  if (test < 1)
    cpp_error (pfile, CPP_DL_ICE, "cppchar_t must be an unsigned type");

  if (CPP_OPTION (pfile, precision) > max_precision)
    cpp_error (pfile, CPP_DL_ICE,
	       "preprocessor arithmetic has maximum precision of %lu bits;"
	       " target requires %lu bits",
	       (unsigned long) max_precision,
	       (unsigned long) CPP_OPTION (pfile, precision));

  if (CPP_OPTION (pfile, precision) < CPP_OPTION (pfile, int_precision))
    cpp_error (pfile, CPP_DL_ICE,
	       "CPP arithmetic must be at least as precise as a target int");

  if (CPP_OPTION (pfile, char_precision) < 8)
    cpp_error (pfile, CPP_DL_ICE, "target char is less than 8 bits wide");

  if (CPP_OPTION (pfile, wchar_precision) < CPP_OPTION (pfile, char_precision))
    cpp_error (pfile, CPP_DL_ICE,
	       "target wchar_t is narrower than target char");

  if (CPP_OPTION (pfile, int_precision) < CPP_OPTION (pfile, char_precision))
    cpp_error (pfile, CPP_DL_ICE,
	       "target int is narrower than target char");

  /* This is assumed in eval_token() and could be fixed if necessary.  */
  if (sizeof (cppchar_t) > sizeof (cpp_num_part))
    cpp_error (pfile, CPP_DL_ICE,
	       "CPP half-integer narrower than CPP character");

  if (CPP_OPTION (pfile, wchar_precision) > BITS_PER_CPPCHAR_T)
    cpp_error (pfile, CPP_DL_ICE,
	       "CPP on this host cannot handle wide character constants over"
	       " %lu bits, but the target requires %lu bits",
	       (unsigned long) BITS_PER_CPPCHAR_T,
	       (unsigned long) CPP_OPTION (pfile, wchar_precision));
}
#else
# define sanity_checks(PFILE)
#endif

/* This is called after options have been parsed, and partially
   processed.  */
void
cpp_post_options (cpp_reader *pfile)
{
  int flags;

  sanity_checks (pfile);

  post_options (pfile);

  /* Mark named operators before handling command line macros.  */
  flags = 0;
  if (CPP_OPTION (pfile, cplusplus) && CPP_OPTION (pfile, operator_names))
    flags |= NODE_OPERATOR;
  if (CPP_OPTION (pfile, warn_cxx_operator_names))
    flags |= NODE_DIAGNOSTIC | NODE_WARN_OPERATOR;
  if (flags != 0)
    mark_named_operators (pfile, flags);
}

/* Setup for processing input from the file named FNAME, or stdin if
   it is the empty string.  Return the original filename on success
   (e.g. foo.i->foo.c), or NULL on failure.  INJECTING is true if
   there may be injected headers before line 1 of the main file.  */
const char *
cpp_read_main_file (cpp_reader *pfile, const char *fname, bool injecting)
{
  if (mkdeps *deps = cpp_get_deps (pfile))
    /* Set the default target (if there is none already).  */
    deps_add_default_target (deps, fname);

  pfile->main_file
    = _cpp_find_file (pfile, fname,
		      CPP_OPTION (pfile, preprocessed) ? &pfile->no_search_path
		      : CPP_OPTION (pfile, main_search) == CMS_user
		      ? pfile->quote_include
		      : CPP_OPTION (pfile, main_search) == CMS_system
		      ? pfile->bracket_include : &pfile->no_search_path,
		      /*angle=*/0, _cpp_FFK_NORMAL, 0);

  if (_cpp_find_failed (pfile->main_file))
    return NULL;

  _cpp_stack_file (pfile, pfile->main_file,
		   injecting || CPP_OPTION (pfile, preprocessed)
		   ? IT_PRE_MAIN : IT_MAIN, 0);

  /* For foo.i, read the original filename foo.c now, for the benefit
     of the front ends.  */
  if (CPP_OPTION (pfile, preprocessed))
    if (!read_original_filename (pfile))
      {
	/* We're on line 1 after all.  */
	auto *last = linemap_check_ordinary
	  (LINEMAPS_LAST_MAP (pfile->line_table, false));
	last->to_line = 1;
	/* Inform of as-if a file change.  */
	_cpp_do_file_change (pfile, LC_RENAME_VERBATIM, LINEMAP_FILE (last),
			     LINEMAP_LINE (last), LINEMAP_SYSP (last));
      }

  auto *map = LINEMAPS_LAST_ORDINARY_MAP (pfile->line_table);
  pfile->main_loc = MAP_START_LOCATION (map);

  return ORDINARY_MAP_FILE_NAME (map);
}

location_t
cpp_main_loc (const cpp_reader *pfile)
{
  return pfile->main_loc;
}

/* For preprocessed files, if the very first characters are
   '#<SPACE>[01]<SPACE>', then handle a line directive so we know the
   original file name.  This will generate file_change callbacks,
   which the front ends must handle appropriately given their state of
   initialization.  We peek directly into the character buffer, so
   that we're not confused by otherwise-skipped white space &
   comments.  We can be very picky, because this should have been
   machine-generated text (by us, no less).  This way we do not
   interfere with the module directive state machine.  */

static bool
read_original_filename (cpp_reader *pfile)
{
  auto *buf = pfile->buffer->next_line;

  if (pfile->buffer->rlimit - buf > 4
      && buf[0] == '#'
      && buf[1] == ' '
      // Also permit '1', as that's what used to be here
      && (buf[2] == '0' || buf[2] == '1')
      && buf[3] == ' ')
    {
      const cpp_token *token = _cpp_lex_direct (pfile);
      gcc_checking_assert (token->type == CPP_HASH);
      if (_cpp_handle_directive (pfile, token->flags & PREV_WHITE))
	{
	  read_original_directory (pfile);

	  auto *penult = &linemap_check_ordinary
	    (LINEMAPS_LAST_MAP (pfile->line_table, false))[-1];
	  if (penult[1].reason == LC_RENAME_VERBATIM)
	    {
	      /* Expunge any evidence of the original linemap.  */
	      pfile->line_table->highest_location
		= pfile->line_table->highest_line
		= penult[0].start_location;

	      penult[1].start_location = penult[0].start_location;
	      penult[1].reason = penult[0].reason;
	      penult[0] = penult[1];
	      pfile->line_table->info_ordinary.used--;
	      pfile->line_table->info_ordinary.m_cache = 0;
	    }

	  return true;
	}
    }

  return false;
}

/* For preprocessed files, if the tokens following the first filename
   line is of the form # <line> "/path/name//", handle the
   directive so we know the original current directory.

   As with the first line peeking, we can do this without lexing by
   being picky.  */
static void
read_original_directory (cpp_reader *pfile)
{
  auto *buf = pfile->buffer->next_line;

  if (pfile->buffer->rlimit - buf > 4
      && buf[0] == '#'
      && buf[1] == ' '
      // Also permit '1', as that's what used to be here
      && (buf[2] == '0' || buf[2] == '1')
      && buf[3] == ' ')
    {
      const cpp_token *hash = _cpp_lex_direct (pfile);
      gcc_checking_assert (hash->type == CPP_HASH);
      pfile->state.in_directive = 1;
      const cpp_token *number = _cpp_lex_direct (pfile);
      gcc_checking_assert (number->type == CPP_NUMBER);
      const cpp_token *string = _cpp_lex_direct (pfile);
      pfile->state.in_directive = 0;

      const unsigned char *text = nullptr;
      size_t len = 0;
      if (string->type == CPP_STRING)
	{
	  /* The string value includes the quotes.  */
	  text = string->val.str.text;
	  len = string->val.str.len;
	}
      if (len < 5
	  || !IS_DIR_SEPARATOR (text[len - 2])
	  || !IS_DIR_SEPARATOR (text[len - 3]))
	{
	  /* That didn't work out, back out.   */
	  _cpp_backup_tokens (pfile, 3);
	  return;
	}

      if (pfile->cb.dir_change)
	{
	  /* Smash the string directly, it's dead at this point  */
	  char *smashy = (char *)text;
	  smashy[len - 3] = 0;
	  
	  pfile->cb.dir_change (pfile, smashy + 1);
	}

      /* We should be at EOL.  */
    }
}

/* This is called at the end of preprocessing.  It pops the last
   buffer and writes dependency output.

   Maybe it should also reset state, such that you could call
   cpp_start_read with a new filename to restart processing.  */
void
cpp_finish (struct cpp_reader *pfile, FILE *deps_stream, FILE *fdeps_stream)
{
  /* Warn about unused macros before popping the final buffer.  */
  if (CPP_OPTION (pfile, warn_unused_macros))
    cpp_forall_identifiers (pfile, _cpp_warn_if_unused_macro, NULL);

  /* lex.cc leaves the final buffer on the stack.  This it so that
     it returns an unending stream of CPP_EOFs to the client.  If we
     popped the buffer, we'd dereference a NULL buffer pointer and
     segfault.  It's nice to allow the client to do worry-free excess
     cpp_get_token calls.  */
  while (pfile->buffer)
    _cpp_pop_buffer (pfile);

  cpp_fdeps_format fdeps_format = CPP_OPTION (pfile, deps.fdeps_format);
  if (fdeps_format == FDEPS_FMT_P1689R5 && fdeps_stream)
    deps_write_p1689r5 (pfile->deps, fdeps_stream);

  if (CPP_OPTION (pfile, deps.style) != DEPS_NONE
      && deps_stream)
    {
      deps_write (pfile, deps_stream, 72);
    }

  /* Report on headers that could use multiple include guards.  */
  if (CPP_OPTION (pfile, print_include_names))
    _cpp_report_missing_guards (pfile);
}

static void
post_options (cpp_reader *pfile)
{
  /* -Wtraditional is not useful in C++ mode.  */
  if (CPP_OPTION (pfile, cplusplus))
    CPP_OPTION (pfile, cpp_warn_traditional) = 0;

  /* Permanently disable macro expansion if we are rescanning
     preprocessed text.  Read preprocesed source in ISO mode.  */
  if (CPP_OPTION (pfile, preprocessed))
    {
      if (!CPP_OPTION (pfile, directives_only))
	pfile->state.prevent_expansion = 1;
      CPP_OPTION (pfile, traditional) = 0;
    }

  if (CPP_OPTION (pfile, warn_trigraphs) == 2)
    CPP_OPTION (pfile, warn_trigraphs) = !CPP_OPTION (pfile, trigraphs);

  if (CPP_OPTION (pfile, traditional))
    {
      CPP_OPTION (pfile, trigraphs) = 0;
      CPP_OPTION (pfile, warn_trigraphs) = 0;
    }

  if (CPP_OPTION (pfile, module_directives))
    {
      /* These unspellable tokens have a leading space.  */
      const char *const inits[spec_nodes::M_HWM]
	= {"export ", "module ", "import ", "__import"};

      for (int ix = 0; ix != spec_nodes::M_HWM; ix++)
	{
	  cpp_hashnode *node = cpp_lookup (pfile, UC (inits[ix]),
					   strlen (inits[ix]));

	  /* Token we pass to the compiler.  */
	  pfile->spec_nodes.n_modules[ix][1] = node;

	  if (ix != spec_nodes::M__IMPORT)
	    /* Token we recognize when lexing, drop the trailing ' '.  */
	    node = cpp_lookup (pfile, NODE_NAME (node), NODE_LEN (node) - 1);

	  node->flags |= NODE_MODULE;
	  pfile->spec_nodes.n_modules[ix][0] = node;
	}
    }
}
