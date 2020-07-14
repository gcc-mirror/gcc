/* CPP Library.
   Copyright (C) 1986-2020 Free Software Foundation, Inc.
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
static void read_original_filename (cpp_reader *);
static void read_original_directory (cpp_reader *);
static void post_options (cpp_reader *);

/* If we have designated initializers (GCC >2.7) these tables can be
   initialized, constant data.  Otherwise, they have to be filled in at
   runtime.  */
#if HAVE_DESIGNATED_INITIALIZERS

#define init_trigraph_map()  /* Nothing.  */
#define TRIGRAPH_MAP \
__extension__ const uchar _cpp_trigraph_map[UCHAR_MAX + 1] = {

#define END };
#define s(p, v) [p] = v,

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
  char c99;
  char cplusplus;
  char extended_numbers;
  char extended_identifiers;
  char c11_identifiers;
  char std;
  char digraphs;
  char uliterals;
  char rliterals;
  char user_literals;
  char binary_constants;
  char digit_separators;
  char trigraphs;
  char utf8_char_literals;
  char va_opt;
  char scope;
  char dfp_constants;
};

static const struct lang_flags lang_defaults[] =
{ /*              c99 c++ xnum xid c11 std digr ulit rlit udlit bincst digsep trig u8chlit vaopt scope dfp */
  /* GNUC89   */  { 0,  0,  1,  0,  0,  0,  1,   0,   0,   0,    0,     0,     0,   0,      1,   1,     0 },
  /* GNUC99   */  { 1,  0,  1,  1,  0,  0,  1,   1,   1,   0,    0,     0,     0,   0,      1,   1,     0 },
  /* GNUC11   */  { 1,  0,  1,  1,  1,  0,  1,   1,   1,   0,    0,     0,     0,   0,      1,   1,     0 },
  /* GNUC17   */  { 1,  0,  1,  1,  1,  0,  1,   1,   1,   0,    0,     0,     0,   0,      1,   1,     0 },
  /* GNUC2X   */  { 1,  0,  1,  1,  1,  0,  1,   1,   1,   0,    0,     0,     0,   1,      1,   1,     1 },
  /* STDC89   */  { 0,  0,  0,  0,  0,  1,  0,   0,   0,   0,    0,     0,     1,   0,      0,   0,     0 },
  /* STDC94   */  { 0,  0,  0,  0,  0,  1,  1,   0,   0,   0,    0,     0,     1,   0,      0,   0,     0 },
  /* STDC99   */  { 1,  0,  1,  1,  0,  1,  1,   0,   0,   0,    0,     0,     1,   0,      0,   0,     0 },
  /* STDC11   */  { 1,  0,  1,  1,  1,  1,  1,   1,   0,   0,    0,     0,     1,   0,      0,   0,     0 },
  /* STDC17   */  { 1,  0,  1,  1,  1,  1,  1,   1,   0,   0,    0,     0,     1,   0,      0,   0,     0 },
  /* STDC2X   */  { 1,  0,  1,  1,  1,  1,  1,   1,   0,   0,    0,     0,     1,   1,      0,   1,     1 },
  /* GNUCXX   */  { 0,  1,  1,  1,  0,  0,  1,   0,   0,   0,    0,     0,     0,   0,      1,   1,     0 },
  /* CXX98    */  { 0,  1,  0,  1,  0,  1,  1,   0,   0,   0,    0,     0,     1,   0,      0,   1,     0 },
  /* GNUCXX11 */  { 1,  1,  1,  1,  1,  0,  1,   1,   1,   1,    0,     0,     0,   0,      1,   1,     0 },
  /* CXX11    */  { 1,  1,  0,  1,  1,  1,  1,   1,   1,   1,    0,     0,     1,   0,      0,   1,     0 },
  /* GNUCXX14 */  { 1,  1,  1,  1,  1,  0,  1,   1,   1,   1,    1,     1,     0,   0,      1,   1,     0 },
  /* CXX14    */  { 1,  1,  0,  1,  1,  1,  1,   1,   1,   1,    1,     1,     1,   0,      0,   1,     0 },
  /* GNUCXX17 */  { 1,  1,  1,  1,  1,  0,  1,   1,   1,   1,    1,     1,     0,   1,      1,   1,     0 },
  /* CXX17    */  { 1,  1,  1,  1,  1,  1,  1,   1,   1,   1,    1,     1,     0,   1,      0,   1,     0 },
  /* GNUCXX20 */  { 1,  1,  1,  1,  1,  0,  1,   1,   1,   1,    1,     1,     0,   1,      1,   1,     0 },
  /* CXX20    */  { 1,  1,  1,  1,  1,  1,  1,   1,   1,   1,    1,     1,     0,   1,      1,   1,     0 },
  /* ASM      */  { 0,  0,  1,  0,  0,  0,  0,   0,   0,   0,    0,     0,     0,   0,      0,   0,     0 }
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
		   class line_maps *line_table)
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
  CPP_OPTION (pfile, cpp_warn_c11_c2x_compat) = -1;
  CPP_OPTION (pfile, cpp_warn_cxx11_compat) = 0;
  CPP_OPTION (pfile, cpp_warn_deprecated) = 1;
  CPP_OPTION (pfile, cpp_warn_long_long) = 0;
  CPP_OPTION (pfile, dollars_in_ident) = 1;
  CPP_OPTION (pfile, warn_dollars) = 1;
  CPP_OPTION (pfile, warn_variadic_macros) = 1;
  CPP_OPTION (pfile, warn_builtin_macro_redefined) = 1;
  CPP_OPTION (pfile, cpp_warn_implicit_fallthrough) = 0;
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

  /* Default CPP arithmetic to something sensible for the host for the
     benefit of dumb users like fix-header.  */
  CPP_OPTION (pfile, precision) = CHAR_BIT * sizeof (long);
  CPP_OPTION (pfile, char_precision) = CHAR_BIT;
  CPP_OPTION (pfile, wchar_precision) = CHAR_BIT * sizeof (int);
  CPP_OPTION (pfile, int_precision) = CHAR_BIT * sizeof (int);
  CPP_OPTION (pfile, unsigned_char) = 0;
  CPP_OPTION (pfile, unsigned_wchar) = 1;
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
  pfile->eof.type = CPP_EOF;
  pfile->eof.flags = 0;

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

  /* Initialize source_date_epoch to -2 (not yet set).  */
  pfile->source_date_epoch = (time_t) -2;

  /* The expression parser stack.  */
  _cpp_expand_op_stack (pfile);

  /* Initialize the buffer obstack.  */
  obstack_specify_allocation (&pfile->buffer_ob, 0, 0, xmalloc, free);

  _cpp_init_files (pfile);

  _cpp_init_hashtable (pfile, table);

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
   macro.c.  Builtin is somewhat of a misnomer -- the property of
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
  B("__BASE_FILE__",	 BT_BASE_FILE,     false),
  B("__LINE__",		 BT_SPECLINE,      true),
  B("__INCLUDE_LEVEL__", BT_INCLUDE_LEVEL, true),
  B("__COUNTER__",	 BT_COUNTER,       true),
  B("__has_attribute",	 BT_HAS_ATTRIBUTE, true),
  B("__has_cpp_attribute", BT_HAS_ATTRIBUTE, true),
  B("__has_builtin",	 BT_HAS_BUILTIN,   true),
  B("__has_include",	 BT_HAS_INCLUDE,   true),
  B("__has_include_next",BT_HAS_INCLUDE_NEXT,   true),
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
      if (CPP_OPTION (pfile, lang) == CLK_CXX20
	  || CPP_OPTION (pfile, lang) == CLK_GNUCXX20)
	_cpp_define_builtin (pfile, "__cplusplus 201709L");
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
  else if (CPP_OPTION (pfile, lang) == CLK_STDC2X
	   || CPP_OPTION (pfile, lang) == CLK_GNUC2X)
    _cpp_define_builtin (pfile, "__STDC_VERSION__ 202000L");
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
  if (CPP_OPTION (pfile, deps.style) != DEPS_NONE)
    {
      if (!pfile->deps)
	pfile->deps = deps_init ();

      /* Set the default target (if there is none already).  */
      deps_add_default_target (pfile->deps, fname);
    }

  pfile->main_file
    = _cpp_find_file (pfile, fname, &pfile->no_search_path, /*angle=*/0,
		      _cpp_FFK_NORMAL, 0);
  if (_cpp_find_failed (pfile->main_file))
    return NULL;

  _cpp_stack_file (pfile, pfile->main_file,
		   injecting ? IT_MAIN_INJECT : IT_MAIN, 0);

  /* For foo.i, read the original filename foo.c now, for the benefit
     of the front ends.  */
  if (CPP_OPTION (pfile, preprocessed))
    {
      read_original_filename (pfile);
      fname = (ORDINARY_MAP_FILE_NAME
	       ((LINEMAPS_LAST_ORDINARY_MAP (pfile->line_table))));
    }
  return fname;
}

/* For preprocessed files, if the first tokens are of the form # NUM.
   handle the directive so we know the original file name.  This will
   generate file_change callbacks, which the front ends must handle
   appropriately given their state of initialization.  */
static void
read_original_filename (cpp_reader *pfile)
{
  const cpp_token *token, *token1;

  /* Lex ahead; if the first tokens are of the form # NUM, then
     process the directive, otherwise back up.  */
  token = _cpp_lex_direct (pfile);
  if (token->type == CPP_HASH)
    {
      pfile->state.in_directive = 1;
      token1 = _cpp_lex_direct (pfile);
      _cpp_backup_tokens (pfile, 1);
      pfile->state.in_directive = 0;

      /* If it's a #line directive, handle it.  */
      if (token1->type == CPP_NUMBER
	  && _cpp_handle_directive (pfile, token->flags & PREV_WHITE))
	{
	  read_original_directory (pfile);
	  return;
	}
    }

  /* Backup as if nothing happened.  */
  _cpp_backup_tokens (pfile, 1);
}

/* For preprocessed files, if the tokens following the first filename
   line is of the form # <line> "/path/name//", handle the
   directive so we know the original current directory.  */
static void
read_original_directory (cpp_reader *pfile)
{
  const cpp_token *hash, *token;

  /* Lex ahead; if the first tokens are of the form # NUM, then
     process the directive, otherwise back up.  */
  hash = _cpp_lex_direct (pfile);
  if (hash->type != CPP_HASH)
    {
      _cpp_backup_tokens (pfile, 1);
      return;
    }

  token = _cpp_lex_direct (pfile);

  if (token->type != CPP_NUMBER)
    {
      _cpp_backup_tokens (pfile, 2);
      return;
    }

  token = _cpp_lex_direct (pfile);

  if (token->type != CPP_STRING
      || ! (token->val.str.len >= 5
	    && IS_DIR_SEPARATOR (token->val.str.text[token->val.str.len-2])
	    && IS_DIR_SEPARATOR (token->val.str.text[token->val.str.len-3])))
    {
      _cpp_backup_tokens (pfile, 3);
      return;
    }

  if (pfile->cb.dir_change)
    {
      char *debugdir = (char *) alloca (token->val.str.len - 3);

      memcpy (debugdir, (const char *) token->val.str.text + 1,
	      token->val.str.len - 4);
      debugdir[token->val.str.len - 4] = '\0';

      pfile->cb.dir_change (pfile, debugdir);
    }      
}

/* This is called at the end of preprocessing.  It pops the last
   buffer and writes dependency output.

   Maybe it should also reset state, such that you could call
   cpp_start_read with a new filename to restart processing.  */
void
cpp_finish (cpp_reader *pfile, FILE *deps_stream)
{
  /* Warn about unused macros before popping the final buffer.  */
  if (CPP_OPTION (pfile, warn_unused_macros))
    cpp_forall_identifiers (pfile, _cpp_warn_if_unused_macro, NULL);

  /* lex.c leaves the final buffer on the stack.  This it so that
     it returns an unending stream of CPP_EOFs to the client.  If we
     popped the buffer, we'd dereference a NULL buffer pointer and
     segfault.  It's nice to allow the client to do worry-free excess
     cpp_get_token calls.  */
  while (pfile->buffer)
    _cpp_pop_buffer (pfile);

  if (CPP_OPTION (pfile, deps.style) != DEPS_NONE && deps_stream)
    deps_write (pfile->deps, deps_stream,
		CPP_OPTION (pfile, deps.phony_targets), 72);

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
}
