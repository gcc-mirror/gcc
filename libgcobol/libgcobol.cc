/*
 * Copyright (c) 2021-2025 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <set>
#include <stack>
#include <string>
#include <unordered_map>
#include <vector>
#include <cwctype>

#include <dirent.h>
#include <dlfcn.h>
#include <err.h>
#include <fcntl.h>
#include <fenv.h>
#include <math.h> // required for fpclassify(3), not in cmath
#include <setjmp.h>
#include <signal.h>
#include <syslog.h>
#include <unistd.h>
#include <stdarg.h>
#if __has_include(<errno.h>)
# include <errno.h> // for program_invocation_short_name
#endif
#include <langinfo.h>

#include "config.h"
#include "libgcobol-fp.h"

#include "ec.h"
#include "common-defs.h"
#include "io.h"
#include "gcobolio.h"
#include "libgcobol.h"
#include "gfileio.h"
#include "charmaps.h"
#include "valconv.h"
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <execinfo.h>
#include "exceptl.h"
#include "stringbin.h"

#define NO_RDIGITS (0)

// Forward reference:
extern "C"
int
__gg__move( cblc_field_t        *fdest,
            size_t               dest_offset,
            size_t               dest_size,
            cblc_field_t        *fsource,
            size_t               source_offset,
            size_t               source_size,
            int                  source_flags,
            cbl_round_t          rounded );


/* BSD extension.  */
#if !defined(LOG_PERROR)
#define LOG_PERROR 0
#endif

#if !defined (HAVE_STRFROMF32)
# if __FLT_MANT_DIG__ == 24 && __FLT_MAX_EXP__ == 128
static int
strfromf32 (char *s, size_t n, const char *f, float v)
{
  return snprintf (s, n, f, (double) v);
}
# else
#  error "It looks like float on this platform is not IEEE754"
# endif
#endif

#if !defined (HAVE_STRFROMF64)
# if __DBL_MANT_DIG__ == 53 && __DBL_MAX_EXP__ == 1024
static int
strfromf64 (char *s, size_t n, const char *f, double v)
{
  return snprintf (s, n, f, v);
}
# else
#  error "It looks like double on this platform is not IEEE754"
# endif
#endif

// Enable Declarative tracing via "match_declarative" environment variable.
#if defined(MATCH_DECLARATIVE) || true
# undef  MATCH_DECLARATIVE
# define MATCH_DECLARATIVE getenv("match_declarative")
#else
# define MATCH_DECLARATIVE (nullptr)
#endif

// This couldn't be defined in symbols.h because it conflicts with a LEVEL66
// in parse.h
#define LEVEL66 (66)
#define LEVEL88 (88)

// These global variables are returned when the functions
//    EXCEPTION-FILE
//    EXCEPTION-LOCATION
//    EXCEPTION-STATEMENT
//    EXCEPTION-STATUS
//  are called

// These global values are established as the COBOL program executes
int         __gg__exception_code              = 0    ;
int         __gg__exception_file_status       = 0    ;
const char *__gg__exception_file_name         = NULL ;
const char *__gg__exception_program_id        = NULL ;
const char *__gg__exception_section           = NULL ;
const char *__gg__exception_paragraph         = NULL ;
const char *__gg__exception_source_file       = NULL ;
int         __gg__exception_line_number       = 0    ;
const char *__gg__exception_statement         = NULL ;
int         __gg__default_compute_error       = 0    ;
int         __gg__rdigits                     = 0    ;
int         __gg__nop                         = 0    ;
int         __gg__main_called                 = 0    ;
void       *__gg__entry_label                 = NULL ;

// During SORT operations, we don't want the end-of-file condition, which
// happens as a matter of course, from setting the EOF exception condition.
// Setting this variable to 'true' suppresses the error condition.
static bool sv_suppress_eof_ec = false;

// What follows are arrays that are used by features like INSPECT, STRING,
// UNSTRING, and, particularly, arithmetic_operation.  These features are
// characterized by having unknown, and essentially unlimited, numbers of
// variables. Consider, for example, ADD A B C D ... TO L M N O ...

// Although originally implemented with malloc/free, that's terribly inefficient
// on its face; arithmetic is done frequently.  The next step was to malloc
// buffers just once, and have them grow as needed, but that resulted in a lot
// of code being laid down, because it meant checking each buffer size at
// run-time, and laying down the code to be executed if the size was inadequate.
//
// The current solution is to make the pointers to the arrays of values global,
// and initialize them with space for MIN_FIELD_BLOCK_SIZE values.  Thus, at
// compile time, we can ignore all tests for fewer than MIN_FIELD_BLOCK_SIZE
// (which is generally the case).  Only when N is greater than the MIN do we
// have to check the current run-time size and, if necessary, expand the buffer
// with realloc.
size_t       __gg__arithmetic_rounds_size      = 0     ;
int *        __gg__arithmetic_rounds           = NULL  ;

size_t       __gg__fourplet_flags_size         = 0     ;
int *        __gg__fourplet_flags              = NULL  ;

static size_t         treeplet_1_size          = 0     ;
cblc_field_t ** __gg__treeplet_1f              = NULL  ;
size_t       *  __gg__treeplet_1o              = NULL  ;
size_t       *  __gg__treeplet_1s              = NULL  ;

static size_t         treeplet_2_size          = 0     ;
cblc_field_t ** __gg__treeplet_2f              = NULL  ;
size_t       *  __gg__treeplet_2o              = NULL  ;
size_t       *  __gg__treeplet_2s              = NULL  ;

static size_t         treeplet_3_size          = 0     ;
cblc_field_t ** __gg__treeplet_3f              = NULL  ;
size_t       *  __gg__treeplet_3o              = NULL  ;
size_t       *  __gg__treeplet_3s              = NULL  ;

static size_t         treeplet_4_size          = 0     ;
cblc_field_t ** __gg__treeplet_4f              = NULL  ;
size_t       *  __gg__treeplet_4o              = NULL  ;
size_t       *  __gg__treeplet_4s              = NULL  ;


// This value is increased every time PROCEDURE DIVISION is processed.  It is
// used to keep track of local variables.
size_t      __gg__unique_prog_id              = 0    ;

// Whenever an exception status is set, a snapshot of the current statement
// location information are established in the "last_exception..." variables.
// This is in accordance with the ISO requirements of "14.6.13.1.1 General" that
// describe how a "last exception status" is maintained.
// other "location" information
static int         last_exception_code;
static const char *last_exception_program_id;
static const char *last_exception_section;
static const char *last_exception_paragraph;
static const char *last_exception_source_file;
static int         last_exception_line_number;
static const char *last_exception_statement;
// These variables are similar, and are established when an exception is
// raised for a file I-O operation.
static cblc_file_prior_op_t last_exception_file_operation;
static file_status_t        last_exception_file_status;
static const char          *last_exception_file_name;

static int sv_from_raise_statement = 0;

typedef void (*PFUNC)();
static std::unordered_map<int, char ***> accessible_programs;
static std::unordered_map<int, PFUNC **> accessible_pointers;

#define ARG_LIMIT 512
char   *__gg__call_parameter_signature = NULL;
int     __gg__call_parameter_count = A_ZILLION;
size_t  __gg__call_parameter_lengths[ARG_LIMIT];

// This is used for managing ENTRY statements in COBOL routines
void       *__gg__entry_location = NULL;

// This is the current value at the back of the PERFORM <PROC> stack of
// procedure signatures.  Said another way:  When the exit address at
// the end of a paragraph matches this value address, then it is time to pop
// the return address off of the stack.  It's in this fashion that we implements
// nested PERFORM PROC statements.
void       *__gg__exit_address = NULL;

// This is the encoding used for sorting tables and files
static cbl_encoding_t encoding_for_sort;

/*
 * ec_status_t represents the runtime exception condition status for
 * any statement.  There are 4 states:
 *   1.  initial, all zeros
 *   2.  updated, copy global EC state for by Declarative and/or default
 *   3.  matched, Declarative found, isection nonzero
 *   4.  handled, where handled == type
 *
 * If the statement includes some kind of ON ERROR
 * clause that covers it, the generated code does not raise an EC.
 *
 * The status is updated by __gg_match_exception if it runs, else
 * __gg__check_fatal_exception.
 *
 * If a Declarative is matched, its section number is passed to handled_by(),
 * which does two things:
 *  1. sets isection to record the declarative
 *  2. for a nonfatal EC, sets handled, indication no further action is needed
 *
 * A Declarative may use RESUME, which clears ec_status, which is a "handled" state.
 *
 * Default processing ensures return to initial state.
 */
class ec_status_t {
 public:
  struct file_status_t {
    size_t ifile;
    cblc_file_prior_op_t operation;
    cbl_file_mode_t mode;
    cblc_field_t *user_status;
    const char * filename;
    file_status_t()
                    : ifile(0)
                    ,  operation(file_op_none)
                    ,  mode(file_mode_none_e)
                    ,  user_status(nullptr)
                    ,  filename(nullptr)
                      {}
    explicit file_status_t( const cblc_file_t *file )
                    : ifile(file->symbol_table_index)
                    , operation(file->prior_op)
                    , mode(cbl_file_mode_t(file->mode_char))
                    , user_status(file->user_status)
                    , filename(file->filename)
                      {}
    const char * op_str() const {
      switch( operation ) {
      case file_op_none: return "none";
      case file_op_open: return "open";
      case file_op_close: return "close";
      case file_op_start: return "start";
      case file_op_read: return "read";
      case file_op_write: return "write";
      case file_op_rewrite: return "rewrite";
      case file_op_delete: return "delete";
      case file_op_remove: return "remove";
      }
      return "???";
    }
  };
 private:
  char msg[132];
  ec_type_t type, handled;
  size_t isection;
  cbl_enabled_exceptions_t enabled;
  cbl_declaratives_t declaratives;
  struct file_status_t file;
 public:
  int lineno;
  const char *source_file;
  cbl_name_t statement; // e.g., "ADD"

  ec_status_t()
    : type(ec_none_e)
    , handled(ec_none_e)
    , isection(0)
    , lineno(0)
    , source_file(NULL)
  {
    msg[0] = statement[0] = '\0';
  }

  bool is_fatal() const;
  ec_status_t& update();

  bool is_enabled() const { return enabled.match(type); }
  bool is_enabled( ec_type_t ec) const { return enabled.match(ec); }
  ec_status_t& handled_by( size_t declarative_section ) {
    isection = declarative_section;
    // A fatal exception remains unhandled unless RESUME clears it.
    if( ! is_fatal() ) {
      handled = type;
    }
    return *this;
  }
  ec_status_t& clear() {
    handled = type = ec_none_e;
    isection = 0;
    lineno = 0;
    msg[0] = statement[0] = '\0';
    return *this;
  }
  bool unset() const { return isection == 0 && lineno == 0; }

  void reset_environment() const;
  ec_status_t& copy_environment();

  // Return the EC's type if it is *not* handled.
  ec_type_t unhandled() const {
    bool was_handled = ec_cmp(type, handled);
    return was_handled? ec_none_e : type;
  }

  bool done() const { return unhandled() == ec_none_e; }

  const file_status_t& file_status() const { return file; }

  const char * exception_location() {
    snprintf(msg, sizeof(msg), "%s:%d: '%s'", source_file, lineno, statement);
    return msg;
  }
};

/*
 * Capture the global EC status at the beginning of Declarative matching. While
 * executing the Declarative, push the current status on a stack. When the
 * Declarative returns, restore EC status from the stack.
 *
 * If the Declarative includes a RESUME statement, it clears the on-stack
 * status, thus avoiding any default handling.
 */
static ec_status_t ec_status;
static std::stack<ec_status_t> ec_stack;

static cbl_enabled_exceptions_t enabled_ECs;
static cbl_declaratives_t declaratives;

static const ec_descr_t *
local_ec_type_descr( ec_type_t type ) {
  auto p = std::find( __gg__exception_table, __gg__exception_table_end, type );
  if( p == __gg__exception_table_end )
    {
      warnx("%s:%d: no such EC value %08x", __func__, __LINE__, type);
    __gg__abort("Fell off the end of the __gg__exception_table");
    }
  return p;
}

cblc_file_t * __gg__file_stashed();

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
// Keep this debugging function around for when it is needed
static const char *
local_ec_type_str( ec_type_t type ) {
  if( type == ec_none_e ) return "EC-NONE";
  auto p = local_ec_type_descr(type);
  return p->name;
}
#pragma GCC diagnostic pop

bool
ec_status_t::is_fatal() const {
  auto descr = local_ec_type_descr(type);
  return descr->disposition == ec_category_fatal_e;
}

ec_status_t&
ec_status_t::update() {
  handled =   ec_none_e;
  type =      ec_type_t(__gg__exception_code);
  source_file =         __gg__exception_source_file;
  lineno =              __gg__exception_line_number;
  if( __gg__exception_statement ) {
    snprintf(statement, sizeof(statement), "%s", __gg__exception_statement);
  }
  cblc_file_t *stashed = __gg__file_stashed();
  this->file = stashed? file_status_t(stashed) : file_status_t();

  if( type != ec_none_e && MATCH_DECLARATIVE ) {
    warnx( "ec_status_t::update:%d: EC %s by %s (handled %s) " , __LINE__,
           local_ec_type_str(type),
           __gg__exception_statement? statement : "<none>",
           local_ec_type_str(handled) );
  }

  this->enabled = ::enabled_ECs;
  this->declaratives = ::declaratives;

  return *this;
}

ec_status_t&
ec_status_t::copy_environment() {
  this->enabled = ::enabled_ECs;
  this->declaratives = ::declaratives;
  return *this;
}

void
ec_status_t::reset_environment() const {
  ::enabled_ECs = enabled;
  ::declaratives = declaratives;
}


// This is the default truncation mode
static cbl_truncation_mode truncation_mode = trunc_std_e;

extern "C"
void
__gg__set_truncation_mode(cbl_truncation_mode trunc_mode)
  {
  truncation_mode = trunc_mode;
  }

struct program_state
  {
  // These are the run-time values of these characters.

  // They are always in source_code space; they get converted to native
  // when they are used.
  int rt_decimal_point;
  int rt_decimal_separator;
  int rt_quote_character;
  int rt_low_value_character;
  int rt_high_value_character;
  std::vector<std::string> rt_currency_signs;
  const unsigned short *rt_collation;  // Points to a table of 256 values;
  cbl_encoding_t rt_display_encoding;
  cbl_encoding_t rt_national_encoding;
  char *rt_program_name;
  cbl_char_t rt_working_init;
  cbl_char_t rt_local_init;

  program_state() : rt_currency_signs(256)
    {
    // IBM defaults to the \" QUOTE compiler option.  quote_character must
    // be set to \' when the APOST compiler option is in effect

    // rt_currency_signs provides for replacing a PICTURE currency "symbol"
    // with a character string referred to in the language specification as
    // a "sign". The string can be an arbitrary length, allowing the
    // replacement of, as an example, the currency <symbol> "$" with the
    // <sign> "USD"

    rt_decimal_point        = ascii_period  ;
    rt_decimal_separator    = ascii_comma   ;
    rt_quote_character      = ascii_dquote  ;    // Change this with APOST
    rt_low_value_character  = DEGENERATE_LOW_VALUE ;
    rt_high_value_character = DEGENERATE_HIGH_VALUE ;
    rt_working_init         = NOT_A_CHARACTER       ;
    rt_local_init           = NOT_A_CHARACTER       ;

    // Set all the currency_sign pointers to NULL:

    rt_display_encoding  = __gg__display_encoding;
    rt_national_encoding = __gg__national_encoding;
    rt_collation = __gg__one_to_one_values;
    rt_program_name = NULL;
    }

    program_state(const program_state &ps)
      : rt_currency_signs(ps.rt_currency_signs)
    {
    rt_decimal_point        = ps.rt_decimal_point         ;
    rt_decimal_separator    = ps.rt_decimal_separator     ;
    rt_quote_character      = ps.rt_quote_character       ;
    rt_low_value_character  = ps.rt_low_value_character   ;
    // Note throughout the code that there is special processing for the
    // default high-value character.  In EBCDIC 0xFF doesn't map
    // to ASCII 0xFF, so we are forced to avoid converting EBCDIC 0xFF.
    rt_high_value_character = ps.rt_high_value_character  ;
    rt_display_encoding     = ps.rt_display_encoding      ;
    rt_national_encoding    = ps.rt_national_encoding     ;
    rt_collation            = ps.rt_collation             ;
    rt_program_name         = ps.rt_program_name          ;
    rt_working_init         = ps.rt_working_init          ;
    rt_local_init           = ps.rt_local_init            ;
    }
  };

static std::vector<program_state> program_states;
#define collated(a)          (program_states.back().rt_collation[(unsigned int)(a&0xFF)])
#define program_name         (program_states.back().rt_program_name)
#define currency_signs(a)    (__gg__currency_signs[(a)])

const unsigned short *
__gg__current_collation()
  {
  return program_states.back().rt_collation;
  }

#ifdef DEBUG_MALLOC
void *malloc(size_t a)
  {
  void *retval = malloc(a);
  fprintf(stderr, " --malloc(%p)-- ", retval);
  return retval;
  }
#endif

void
__gg__abort(const char *msg)
  {
  fprintf(stderr, "%s: %s\n", program_name, msg);
  abort();
  }

void
__gg__mabort()
  {
  __gg__abort("Memory allocation error\n");
  }

extern "C"
char
__gg__get_decimal_point()
  {
  return __gg__decimal_point;
  }

extern "C"
char
__gg__get_decimal_separator()
  {
  return __gg__decimal_separator;
  }

extern "C"
const char *
__gg__get_default_currency_string()
  {
  return currency_signs(__gg__default_currency_sign).c_str();
  }

extern "C"
void
__gg__resize_int_p( size_t *size,
                    int    **block,
                    size_t  new_size)
  {
  if( new_size > *size )
    {
    *size = new_size;
    *block = static_cast<int *>(realloc(*block, new_size * sizeof(int)));
    }
  }

extern "C"
void
__gg__resize_treeplet(int     ngroup,
                      size_t  new_size)
  {
  switch( ngroup )
    {
    case 1:
      if( new_size > treeplet_1_size )
        {
        treeplet_1_size = new_size;
        __gg__treeplet_1f = static_cast<cblc_field_t **>(realloc(__gg__treeplet_1f, new_size * sizeof(cblc_field_t *)));
        __gg__treeplet_1o = static_cast<size_t *>(realloc(__gg__treeplet_1o, new_size * sizeof(size_t)));
        __gg__treeplet_1s = static_cast<size_t *>(realloc(__gg__treeplet_1s, new_size * sizeof(size_t)));
        }
    break;
    case 2:
      if( new_size > treeplet_2_size )
        {
        treeplet_2_size = new_size;
        __gg__treeplet_2f = static_cast<cblc_field_t **>(realloc(__gg__treeplet_2f, new_size * sizeof(cblc_field_t *)));
        __gg__treeplet_2o = static_cast<size_t *>(realloc(__gg__treeplet_2o, new_size * sizeof(size_t)));
        __gg__treeplet_2s = static_cast<size_t *>(realloc(__gg__treeplet_2s, new_size * sizeof(size_t)));
        }
    break;
    case 3:
      if( new_size > treeplet_3_size )
        {
        treeplet_3_size = new_size;
        __gg__treeplet_3f = static_cast<cblc_field_t **>(realloc(__gg__treeplet_3f, new_size * sizeof(cblc_field_t *)));
        __gg__treeplet_3o = static_cast<size_t *>(realloc(__gg__treeplet_3o, new_size * sizeof(size_t)));
        __gg__treeplet_3s = static_cast<size_t *>(realloc(__gg__treeplet_3s, new_size * sizeof(size_t)));
        }
    break;
    case 4:
      if( new_size > treeplet_4_size )
        {
        treeplet_4_size = new_size;
        __gg__treeplet_4f = static_cast<cblc_field_t **>(realloc(__gg__treeplet_4f, new_size * sizeof(cblc_field_t *)));
        __gg__treeplet_4o = static_cast<size_t *>(realloc(__gg__treeplet_4o, new_size * sizeof(size_t)));
        __gg__treeplet_4s = static_cast<size_t *>(realloc(__gg__treeplet_4s, new_size * sizeof(size_t)));
        }
    break;
    }
  }

static void
initialize_program_state()
  {
  // This routine gets called exactly once for a COBOL executable
  program_state initial_value = {};
  program_states.push_back(initial_value);
  __gg__currency_signs = program_states.back().rt_currency_signs;

  // This is where we initialize the various tables that have
  // MIN_FIELD_BLOCK_SIZE elements:

  __gg__resize_int_p(&__gg__arithmetic_rounds_size,
                     &__gg__arithmetic_rounds,
                      MIN_FIELD_BLOCK_SIZE );
  __gg__resize_int_p(&__gg__fourplet_flags_size,
                     &__gg__fourplet_flags,
                      MIN_FIELD_BLOCK_SIZE );
  __gg__resize_treeplet(1, MIN_FIELD_BLOCK_SIZE);
  __gg__resize_treeplet(2, MIN_FIELD_BLOCK_SIZE);
  __gg__resize_treeplet(3, MIN_FIELD_BLOCK_SIZE);
  __gg__resize_treeplet(4, MIN_FIELD_BLOCK_SIZE);
  }

extern "C"
void
__gg__set_program_name(char *progname)
  {
  program_name = progname;
  }

extern "C"
void
__gg__push_program_state()
  {
  // Duplicate the state at the back of the stack
  program_states.push_back(program_states.back());
  __gg__currency_signs = program_states.back().rt_currency_signs;
  }

extern "C"
void
__gg__pop_program_state()
  {
  program_states.pop_back();
  __gg__decimal_point        = program_states.back().rt_decimal_point        ;
  __gg__decimal_separator    = program_states.back().rt_decimal_separator    ;
  __gg__quote_character      = program_states.back().rt_quote_character      ;
  __gg__low_value_character  = program_states.back().rt_low_value_character  ;
  __gg__high_value_character = program_states.back().rt_high_value_character ;
  __gg__display_encoding     = program_states.back().rt_display_encoding     ;
  __gg__national_encoding    = program_states.back().rt_national_encoding    ;
  __gg__currency_signs       = program_states.back().rt_currency_signs       ;
  __gg__working_init         = program_states.back().rt_working_init         ;
  __gg__local_init           = program_states.back().rt_local_init           ;

  }

extern "C"
void
__gg__decimal_point_is_comma()
  {
  program_states.back().rt_decimal_point      = ascii_comma ;
  program_states.back().rt_decimal_separator  = ascii_period ;
  __gg__decimal_point     = ascii_comma ;
  __gg__decimal_separator = ascii_period ;
  }

static __int128
edited_to_binary( const cblc_field_t *field,
                  char *ps_,
                  int length,
                  int *rdigits)
  {
  charmap_t *charmap = __gg__get_charmap(field->encoding);

  const unsigned char *ps = const_cast<const unsigned char *>(PTRCAST(unsigned char, ps_));
  // This routine is used for converting NumericEdited strings to
  // binary.

  // Numeric edited strings can have all kinds of crap in them: spaces,
  // slashes, dollar signs...you name it.  It might have a minus sign at
  // the beginning or end, or it might have CR or DB at the end.

  // We are going to look for a minus sign, D (or d) and use that to flag the
  // result as negative.  We are going to look for a decimal point and count up
  // the numerical digits to the right of it.  And we are going to pretend
  // that nothing else matters.

  int hyphen = 0;
  *rdigits = 0;

  // index into the ps string
  int index = 0;

  // Create a delta_r for counting digits to the right of
  // any decimal point.  If and when we encounter a decimal point,
  // we'll set this to one, otherwise it'll stay zero.
  int delta_r = 0;

  __int128 result = 0;

  // We need to check the last two characters.  If CR or DB, then the result
  // is negative:
  if( length >= 2)
    {
    if(         ((ps[length-2]&0xFF) == charmap->mapped_character(ascii_D)
              || (ps[length-2]&0xFF) == charmap->mapped_character(ascii_d))
            &&  ((ps[length-1]&0xFF) == charmap->mapped_character(ascii_B)
              || (ps[length-1]&0xFF) == charmap->mapped_character(ascii_b)) )
      {
      hyphen = 1;
      }
    else if(    ((ps[length-2]&0xFF) == charmap->mapped_character(ascii_C)
              || (ps[length-2]&0xFF) == charmap->mapped_character(ascii_c))
            &&  ((ps[length-1]&0xFF) == charmap->mapped_character(ascii_R)
              || (ps[length-1]&0xFF) == charmap->mapped_character(ascii_r)) )
      {
      hyphen = 1;
      }
    }

  while( index < length )
    {
    unsigned char ch = ps[index++] & 0xFF;
    if( ch == charmap->mapped_character(__gg__decimal_point) )
      {
      delta_r = 1;
      continue;
      }
    if( ch == charmap->mapped_character(ascii_minus)  )
      {
      hyphen = 1;
      continue;
      }

    if(  charmap->mapped_character(ascii_0) <= ch
      && ch <= charmap->mapped_character(ascii_9) )
      {
      result *= 10;
      // In both EBCDIC and ASCII, this works:
      result += ch & 0x0F ;
      *rdigits += delta_r ;
      continue;
      }
    }

  if( hyphen )
    {
    result = -result;
    }
  return result;
  }

static
__int128
big_endian_to_binary_signed(
  const unsigned char *psource,
  int   capacity
)
  {
  // This subroutine takes a big-endian value of "capacity" bytes and
  // converts it to a signed INT128.  The highest order bit of the big-endian
  // value determines whether or not the highest-order bits of the INT128
  // return value are off or on.

  __int128 retval;
  if( *psource >= 128 )
    {
    retval = -1;
    }
  else
    {
    retval = 0;
    }

  // move the bytes of psource into retval, flipping them end-to-end
  unsigned char *dest = PTRCAST(unsigned char, &retval);
  while(capacity > 0)
    {
    *dest++ = psource[--capacity];
    }
  return retval;
  }

static
__int128
little_endian_to_binary_signed(
  const unsigned char *psource,
  int capacity
)
  {
  // This subroutine takes a little-endian value of "capacity" bytes and
  // converts it to a signed INT128.  The highest order bit of the little-endian
  // value determines whether or not the highest-order bits of the INT128
  // return value are off or on.

  __int128 result;

  // Set all the bits of the result based on the sign of the source:
  if( psource[capacity-1] >= 128 )
    {
    result = -1;
    }
  else
    {
    result = 0;
    }

  // Copy the low-order bytes into place:
  memcpy(&result, psource, capacity);
  return result;
  }

static
__int128
little_endian_to_binary_unsigned(
  const unsigned char *psource,
  int capacity
)
  {
  __int128 result = 0;

  // Copy the low-order bytes into place:
  memcpy(&result, psource, capacity);
  return result;
  }

static
__int128
big_endian_to_binary_unsigned(
  const unsigned char *psource,
  int   capacity
)
  {
  // This subroutine takes an unsigned big-endian value of "capacity" bytes and
  // converts it to an INT128.

  __int128 retval = 0 ;

  // move the bytes of psource into retval, flipping them end-to-end
  unsigned char *dest = PTRCAST(unsigned char, &retval);
  while(capacity > 0)
    {
    *dest++ = psource[--capacity];
    }
  return retval;
  }

static
__int128
get_binary_value_local(  int                 *rdigits,
                         const cblc_field_t  *resolved_var,
                         unsigned char       *resolved_location,
                         size_t               resolved_length)
  {
  __int128 retval = 0;

  switch( resolved_var->type )
    {
    case FldLiteralA :
      fprintf(stderr, "%s(): is trying to handle a FldLiteralA\n", __func__);
      abort();
      break;

    case FldGroup :
    case FldAlphanumeric :
      // Read the data area as a dirty string:
      retval = __gg__dirty_to_binary(
                        PTRCAST(const char, resolved_location),
                        resolved_var->encoding,
                        resolved_length,
                        rdigits );
      break;

    case FldNumericDisplay:
      {
      *rdigits = resolved_var->rdigits;
      if( resolved_location[resolved_length-1] == DEGENERATE_HIGH_VALUE )
        {
        // This is a degenerate case, which violates the language
        // specification, but nonetheless seems to be a thing.  By
        // default, HIGH-VALUE is usually assumed to be 0xFF.  This is
        // not necessarily true; HIGH-VALUE can be changed by the
        // SPECIAL-NAMES ALPHABET clause.  Furthermore, by definition,
        // HIGH-VALUE applies *only* to text literals.  However, there
        // seems to be code out in the universe that wants to be able
        // to compare NumericDisplay values that have been set to
        // HIGH-VALUE.  Consider, for example, code that reads from
        // a disk file which sets the input field to HIGH-VALUE upon
        // an end-of-file condition.

        // This code detects that particular condition, and sets the
        // resulting binary number to the maximum possible positive
        // value.

        // Turn all the bits on
        memset( &retval, 0xFF, sizeof(retval) );

        // Make it positive by turning off the highest order bit:
        (PTRCAST(unsigned char, &retval))[sizeof(retval)-1] = 0x3F;
        }
      else
        {
        const charmap_t *charmap = __gg__get_charmap(resolved_var->encoding);
        int stride = charmap->stride();
        unsigned char *digits;
        unsigned char *sign_byte_location;
        int ndigits;
        if( resolved_var->attr & signable_e )
          {
          // Pick up the sign byte, and force our value to be positive
          if(   (resolved_var->attr  & separate_e )
             && (resolved_var->attr  & leading_e  ) )
            {
            // LEADING SEPARATE
            digits             = resolved_location+stride;
            sign_byte_location = resolved_location;
            ndigits = resolved_length - stride;
            }
          else if(    (resolved_var->attr & separate_e)
                  && !(resolved_var->attr & leading_e ) )
            {
            // TRAILING SEPARATE
            digits             = resolved_location;
            sign_byte_location = resolved_location + resolved_length - stride;
            ndigits = resolved_length - stride;
            }
          else if( (resolved_var->attr & leading_e) )
            {
            // LEADING
            digits             = resolved_location;
            sign_byte_location = resolved_location;
            ndigits = resolved_length;
            }
          else // if( !(resolved_var->attr & leading_e) )
            {
            // TRAILING
            digits             = resolved_location;
            sign_byte_location = resolved_location + resolved_length - stride;
            ndigits = resolved_length;
            }
          }
        else
          {
          digits             = resolved_location;
          sign_byte_location = resolved_location;
          ndigits = resolved_length;
          }
        ndigits /= stride;
        retval = __gg__numeric_display_to_binary(sign_byte_location,
                                                 digits,
                                                 ndigits,
                                                 resolved_var->encoding);
        }
      break;
      }

    case FldNumericEdited :
      retval = edited_to_binary(resolved_var,
                                PTRCAST(char, resolved_location),
                                resolved_length,
                                rdigits);
      break;

    case FldNumericBinary :
      if( resolved_var->attr & signable_e)
        {
        retval = big_endian_to_binary_signed(
                        PTRCAST(const unsigned char, resolved_location),
                        resolved_length);
        }
      else
        {
        retval = big_endian_to_binary_unsigned(
                        PTRCAST(const unsigned char, resolved_location),
                        resolved_length);
        }
      *rdigits = resolved_var->rdigits;
      break;

    case FldLiteralN:
      {
      if( resolved_var->attr & signable_e)
        {
        retval = little_endian_to_binary_signed(resolved_var->data,
                                                resolved_var->capacity);
        }
      else
        {
        retval = little_endian_to_binary_unsigned(resolved_var->data,
                                                  resolved_var->capacity);
        }
      *rdigits = resolved_var->rdigits;
      break;
      }

    case FldNumericBin5:
    case FldIndex:
    case FldPointer:
      if( resolved_var->attr & signable_e)
        {
        retval = little_endian_to_binary_signed(
                      PTRCAST(const unsigned char, resolved_location),
                      resolved_length);
        }
      else
        {
        retval = little_endian_to_binary_unsigned(
                      PTRCAST(const unsigned char, resolved_location),
                      resolved_length);
        }
      *rdigits = resolved_var->rdigits;
      break;

    case FldPacked:
      {
      *rdigits = resolved_var->rdigits;
      retval = __gg__packed_to_binary(resolved_location,
                                      resolved_length);
      break;
      }
    }

  if( resolved_var->attr & scaled_e )
    {
    // Here's where we handle a P-scaled number.

    if( resolved_var->rdigits >= 0)
      {
      // We might be dealing with a source with a PICTURE string of
      // PPPPPP999, which means retval is a three-digit number
      // and resolved_var->rdigits is +6.  That means we need to divide retval
      // by 10**9, and we need to make rdigits 9
      *rdigits = resolved_var->digits + resolved_var->rdigits;
      }
    else
      {
      // We have a source with a PIC string like 999PPPPPP, which is
      // a capacity of 3 and a resolved_var->rdigits of -6.  We need to multiply
      // retval by +6, and make rdigits zero:
      retval *= __gg__power_of_ten( -resolved_var->rdigits );
      *rdigits = 0;
      }
    }

  return retval;
  }

static uint32_t
get_init_value(cblc_field_t *field)
  {
  uint32_t retval = 0;
  cbl_figconst_t figconst = (cbl_figconst_t)(field->attr & FIGCONST_MASK);
  if( figconst )
    {
    switch(figconst)
      {
      case normal_value_e :
        // This is not possible, it says here in the fine print.
        abort();
        break;
      case low_value_e    :
        retval = __gg__low_value_character;
        break;
      case zero_value_e   :
        retval = ascii_zero;
        break;
      case space_value_e  :
        retval = ascii_space;
        break;
      case quote_value_e  :
        retval = __gg__quote_character;
        break;
      case high_value_e   :
        retval = __gg__high_value_character;
        break;
      case null_value_e:
        retval = 0x00;
        break;
      }
    }
  else
    {
    int rdigits;
    retval = get_binary_value_local(&rdigits,
                                    field,
                                    field->data,
                                    field->capacity
                                    );
    }
  return retval;
  }

extern "C"
void __gg__initialization_values( uint32_t wsclear,
                                  cblc_field_t *working_init,
                                  cblc_field_t *local_init)
  {
  __gg__wsclear = wsclear;
  __gg__working_init = NOT_A_CHARACTER;
  __gg__local_init   = NOT_A_CHARACTER;

  if( working_init )
    {
    __gg__working_init = get_init_value(working_init);
    }
  if( local_init )
    {
    __gg__local_init = get_init_value(local_init);
    }
  program_states.back().rt_working_init = __gg__working_init;
  program_states.back().rt_local_init   = __gg__local_init;
  }

extern "C"
void
__gg__init_program_state(cbl_encoding_t display_encoding,
                         cbl_encoding_t national_encoding)
  {
  // This routine gets called at DATA DIVISION time.

  __gg__display_encoding  = display_encoding;
  __gg__national_encoding = national_encoding;

  // We need to make sure that the program_states vector has at least one
  // entry in it.  This happens when we are the very first PROGRAM-ID called
  // in this module.
  if( program_states.empty() )
    {
    initialize_program_state();
    }
  }

static int
var_is_refmod( const cblc_field_t *var )
  {
  return (var->attr & refmod_e) != 0;
  }

extern "C"
__int128
__gg__power_of_ten(int n)
  {
  // 2** 64 = 1.8E19
  // 2**128 = 3.4E38
  __int128 retval = 1;
  static const int MAX_POWER = 19 ;
  static const __int128 pos[MAX_POWER+1] =
    {
    1ULL,                       // 00
    10ULL,                      // 01
    100ULL,                     // 02
    1000ULL,                    // 03
    10000ULL,                   // 04
    100000ULL,                  // 05
    1000000ULL,                 // 06
    10000000ULL,                // 07
    100000000ULL,               // 08
    1000000000ULL,              // 09
    10000000000ULL,             // 10
    100000000000ULL,            // 11
    1000000000000ULL,           // 12
    10000000000000ULL,          // 13
    100000000000000ULL,         // 14
    1000000000000000ULL,        // 15
    10000000000000000ULL,       // 16
    100000000000000000ULL,      // 17
    1000000000000000000ULL,     // 18
    10000000000000000000ULL,    // 19
    };
  if( n < 0 || n>MAX_POWER*2)     // The most we can handle is 10**38
    {
    fprintf(stderr,
            "Trying to raise 10 to %d as an int128, which we can't do.\n",
            n);
    fprintf(stderr, "The problem is in %s %s:%d.\n", __func__, __FILE__, __LINE__);
    abort();
    }
  if( n <= MAX_POWER )
    {
    // Up to 10**18 we do directly:
    retval = pos[n];
    }
  else
    {
    // 19 through 38:
    retval = pos[n/2];
    retval *= retval;
    if( n & 1 )
      {
      retval *= 10;
      }
    }
  return retval;
  }

extern "C"
__int128
__gg__scale_by_power_of_ten_1(__int128 value, int N)
  {
  // This routine is called when the result of the scaling is not allowed to
  // have non-zero rdigits.  __gg__rdigits is set to 1 when the result is
  // in the bad zone.  The ultimate caller needs to examine __gg__rdigits to
  // decide what to do about it.

  // This is a separate routine because of the performance hit caused by the
  // value % pot operation, which is needed only when certain EC checking is
  // turned on.
  if( N > 0 )
    {
    __gg__rdigits = 0;
    value *= __gg__power_of_ten(N);
    }
  else if( N < 0)
    {
    // We throwing away the N rightmost digits.  Use __gg__rdigits
    // to let the calling chain know they were non-zero:
    __int128 pot = __gg__power_of_ten(-N);
    if( value % pot)
      {
      __gg__rdigits = 1;
      }
    else
      {
      __gg__rdigits = 0;
      }

    value /= pot;
    }
  else
    {
    // N is zero
    __gg__rdigits = 0;
    }
  return value;
  }

extern "C"
__int128
__gg__scale_by_power_of_ten_2(__int128 value, int N)
  {
  if( N > 0 )
    {
    value *= __gg__power_of_ten(N);
    }
  else if( N < 0)
    {
    value /= __gg__power_of_ten(-N);
    }
  return value;
  }

static bool
value_is_too_big(const cblc_field_t *var,
                 __int128            value,
                 int                 source_rdigits)
  {
  // This routine is in support of arithmetic ON SIZE ERROR.  It returns
  // TRUE if var hasn't enough bytes to hold the decimal representation
  // of value:
  bool retval = false;

  if( !(var->attr & intermediate_e) )
    {
    if( value < 0 )
      {
      value = -value;
      }
    if( var->digits )
      {
      // I don't know how to describe this calculation.  I came up with the
      // equation by working a few examples.  For instance, if value is 12345 and
      // source_rdigits is two, then we are trying to cram 123.45 into 99v99999
      // and we have a size error.  So, digits is 7, rdigits is 5 and source_rdigits
      // 2.  That means we compare 12345 with 10^(7 - 5 + 2), which is 12345 versus
      // 10000, which is too big, which means we have a size error.
      retval =
          value >= __gg__power_of_ten( var->digits - var->rdigits + source_rdigits);
      }
    else
      {
      // var->digits is zero.  We are dealing with a binary-style number that
      // fills the whole of the value
      if( !(  var->type == FldNumericBin5
              || var->type == FldPointer
              || var->type == FldIndex) )
        {
        __gg__abort("value_is_too_big() was given a type it doesn't know about");
        }
      if( var->capacity < 16 )
        {
        __int128 max_possible = 1;
        max_possible = max_possible << (var->capacity * 8);
        retval = value >= max_possible;
        }
      }
    }

  return retval;
  }

static void
binary_to_big_endian(   unsigned char *dest,
                        int            bytes,
                        __int128       value
                    )
  {
  if( value < 0 )
    {
    memset(dest, 0xFF, bytes);
    }
  else
    {
    memset(dest, 0x00, bytes);
    }

  dest += bytes-1;
  while( bytes-- )
    {
    *dest-- = (unsigned char) value;
    value >>= 8;
    }
  }

static void
binary_to_little_endian(   unsigned char *dest,
                           int            bytes,
                           __int128       value
                       )
  {
  if( value < 0 )
    {
    memset(dest, 0xFF, bytes);
    }
  else
    {
    memset(dest, 0x00, bytes);
    }
  memcpy(dest, &value, bytes);
  }

static __int128
int128_to_int128_rounded( cbl_round_t rounded,
                          __int128    value,
                          __int128    factor,
                          __int128    remainder,
                          int        *compute_error)
  {
  // value is signed, and is scaled to the target
  GCOB_FP128 fpart = ((GCOB_FP128)remainder) / ((GCOB_FP128)factor);
  __int128 retval = value;

  if(rounded == nearest_even_e
     && fpart != GCOB_FP128_LITERAL (-0.5)
     && fpart != GCOB_FP128_LITERAL (0.5))
    {
    // "bankers rounding" has been requested.
    //
    // Since the fraction is not 0.5, this is an ordinary rounding
    // problem
    rounded =  nearest_away_from_zero_e;
    }

  switch(rounded)
    {
    case truncation_e:
      break;

    case nearest_away_from_zero_e:
      {
      // This is ordinary rounding, like you learned in grade school
      // 0.0 through 0.4 becomes 0
      // 0.5 through 0.9 becomes 1
      if( value < 0 )
        {
        if( fpart <= GCOB_FP128_LITERAL(-0.5) )
          {
          retval -= 1;
          }
        }
      else
        {
        if( fpart >= GCOB_FP128_LITERAL(0.5) )
          {
          retval += 1;
          }
        }
      break;
      }

    case away_from_zero_e:
      {
      // zero stays zero, otherwise head for the next number away from zero
      if( value < 0 )
        {
        if( fpart != 0 )
          {
          retval -= 1;
          }
        }
      else
        {
        if( fpart != 0 )
          {
          retval += 1;
          }
        }
      break;
      }

    case nearest_toward_zero_e:
      {
      // 0.0 through 0.5 becomes 0
      // 0.6 through 0.9 becomes 1
      if( value < 0 )
        {
        if( fpart < GCOB_FP128_LITERAL(-0.5) )
          {
          retval -= 1;
          }
        }
      else
        {
        if( fpart > GCOB_FP128_LITERAL(0.5) )
          {
          retval += 1;
          }
        }
      break;
      }

    case toward_greater_e:
      {
      if( value > 0 )
        {
        if( fpart != 0 )
          {
          retval += 1;
          }
        }
      break;
      }

    case toward_lesser_e:
      {
      if( value < 0 )
        {
        if(fpart != 0)
          {
          retval -= 1;
          }
        }
      break;
      }

    case nearest_even_e:
      {
      // This is "banker's rounding"
      // 3.4 -> 3.0
      // 3.5 -> 4.0
      // 3.6 -> 4.0

      // 4.4 -> 4.0
      // 4.5 -> 4.0
      // 4.6 -> 5.0

     // We know that the fractional part is 0.5 or -0.5, and we know that
     // we want 3 to become 4 and for 4 to stay 4.

    if( value < 0 )
      {
      if( retval & 1 )
        {
        retval -= 1;
        }
      }
    else
      {
      if( retval & 1 )
        {
        retval += 1;
        }
      }
      break;
      }

    case prohibited_e:
      {
      if( fpart != 0 )
        {
        *compute_error |= compute_error_truncate;
        }

      break;
      }

    default:
      abort();
      break;
    }
  return retval;
  }

static __int128
f128_to_i128_rounded( cbl_round_t rounded,
                    GCOB_FP128   value,
                    int        *compute_error)
  {
  // value is signed, and is scaled to the target
  GCOB_FP128 ipart;
  GCOB_FP128 fpart = FP128_FUNC(modf)(value, &ipart);
  __int128 retval = (__int128)ipart;

  if(rounded == nearest_even_e
     && fpart != GCOB_FP128_LITERAL (-0.5)
     && fpart != GCOB_FP128_LITERAL (0.5))
    {
    // "bankers rounding" has been requested.
    //
    // Since the fraction is not 0.5, this is an ordinary rounding
    // problem
    rounded =  nearest_away_from_zero_e;
    }

  switch(rounded)
    {
    case truncation_e:
      break;

    case nearest_away_from_zero_e:
      {
      // This is ordinary rounding, like you learned in grade school
      // 0.0 through 0.4 becomes 0
      // 0.5 through 0.9 becomes 1
      if( value < 0 )
        {
        if( fpart <= GCOB_FP128_LITERAL (-0.5) )
          {
          retval -= 1;
          }
        }
      else
        {
        if( fpart >= GCOB_FP128_LITERAL (0.5) )
          {
          retval += 1;
          }
        }
      break;
      }

    case away_from_zero_e:
      {
      // zero stays zero, otherwise head for the next number away from zero
      if( value < 0 )
        {
        if( fpart != 0 )
          {
          retval -= 1;
          }
        }
      else
        {
        if( fpart != 0 )
          {
          retval += 1;
          }
        }
      break;
      }

    case nearest_toward_zero_e:
      {
      // 0.0 through 0.5 becomes 0
      // 0.6 through 0.9 becomes 1
      if( value < 0 )
        {
        if( fpart < GCOB_FP128_LITERAL (-0.5) )
          {
          retval -= 1;
          }
        }
      else
        {
        if( fpart > GCOB_FP128_LITERAL (0.5) )
          {
          retval += 1;
          }
        }
      break;
      }

    case toward_greater_e:
      {
      if( value > 0 )
        {
        if( fpart != 0 )
          {
          retval += 1;
          }
        }
      break;
      }

    case toward_lesser_e:
      {
      if( value < 0 )
        {
        if(fpart != 0)
          {
          retval -= 1;
          }
        }
      break;
      }

    case nearest_even_e:
      {
      // This is "banker's rounding"
      // 3.4 -> 3.0
      // 3.5 -> 4.0
      // 3.6 -> 4.0

      // 4.4 -> 4.0
      // 4.5 -> 4.0
      // 4.6 -> 5.0

     // We know that the fractional part is 0.5 or -0.5, and we know that
     // we want 3 to become 4 and for 4 to stay 4.

    if( value < 0 )
      {
      if( retval & 1 )
        {
        retval -= 1;
        }
      }
    else
      {
      if( retval & 1 )
        {
        retval += 1;
        }
      }
      break;
      }

    case prohibited_e:
      {
      if( fpart != 0 )
        {
        *compute_error |= compute_error_truncate;
        }

      break;
      }

    default:
      abort();
      break;
    }
  return retval;
  }

static void
int128_to_field(cblc_field_t   *var,
                unsigned char  *location,
                size_t          length,
                __int128        value,
                int             source_rdigits,
                enum cbl_round_t  rounded,
                int            *compute_error)
  {
  // This routine takes a numerical value, and scales and converts it to the
  // target field type.

  // It operates in the source codeset space, and converts the final result
  // to the native codeset space

  switch( var->type )
    {
    case FldFloat:
      {
      switch( var->capacity )
        {
        case 4:
          {
          float tvalue = (float)value;
          tvalue /= (float)__gg__power_of_ten(source_rdigits);
          *PTRCAST(float, location) = tvalue;
          break;
          }

        case 8:
          {
          double tvalue = (double)value;
          tvalue /= (double)__gg__power_of_ten(source_rdigits);
          *PTRCAST(double, location) = tvalue;
          break;
          }

        case 16:
          {
          // It turns out we have a problem.  The IEEE 754 quadruple-precision
          // binary representation can handle up to 33 digits exactly, and can
          // handle at most 36 digits.  I decided to implement fixed-point
          // values to 38 places (which is what an __int128 can hold), and as a
          // result, at this point in the code we can be asking the compiler to
          // turn a 38-digit __int128 into a _Float128.

          // This caused a problem that I noticed in COMPUTE var = (2/3)*3.

          // The default is truncation, and so the PIC 9V9999 result should be
          // 1.9999.

          // At this point in the code, the 128-bit value was the
          // 38-digit 19999999999999999999999999999999999998

          // So, I then converted that to a _Float128, and the conversion
          // routine properly did the best it could and returned exactly
          // 2E37

          // The problem: This rounded the number up from 1.9999...., and so
          // the truncation resulted in 2.0000 when we wanted 1.9999

          // The solution:  Throw away digits on the right to make sure there
          // are no more than 33 significant digits.

          bool isneg = value < 0;
          if(isneg)
            {
            value = -value;
            }

          static __int128 ten33 = __gg__power_of_ten(33);

          while( value >= ten33 )
            {
            // Lop off the rightmost digits until the result has no more than
            // 33 digits.
            value /= 10;
            source_rdigits -= 1;
            }

          if(isneg)
            {
            value = -value;
            }
          GCOB_FP128 tvalue = (GCOB_FP128 )value;
          tvalue /= (GCOB_FP128 )__gg__power_of_ten(source_rdigits);
          // *(_Float128  *)location = tvalue;
          // memcpy because *(_Float128  *) requires a 16-byte boundary.
          memcpy(location, &tvalue, 16);
          break;
          }
        }
      break;
      }

    default:
      {
      int target_rdigits = var->rdigits;
      if( var->attr & intermediate_e && var->type == FldNumericBin5)
        {
        // The target is an intermediate, meaning that we want to
        // Make sure our intermediate target has just enough digits and rdigits
        // to hold the value we've been given:
        target_rdigits = source_rdigits;
        var->rdigits = target_rdigits;
        var->digits = MAX_FIXED_POINT_DIGITS;
        }
      else if( var->attr & scaled_e )
        {
        // Our target is scaled.  No matter which way we are going, the result
        // going into memory has no decimal places.
        target_rdigits = 0;

        // We have some additional scaling of value to do to make things line up.

        if( var->rdigits >= 0 )
          {
          // Our target is something like PPPPPP999, meaning that var->actual_length
          // is 3, and var->rdigits is 6.

          // By rights, our caller should have given us something like 123 with
          // source_rdigits of 9.  So, we multiply by 10**9 to put the 123 just
          // to the left of the decimal point, so that they line up with the
          // target_rdigits of zero we are targeting:
          source_rdigits -= var->digits + var->rdigits;
          if(source_rdigits < 0)
            {
            // We overshot
            value *= __gg__power_of_ten(-source_rdigits);
            source_rdigits = 0;
            }
          }
        else
          {
          // Our target is something like 999PPPPPP, so there is a ->digits
          // of 3 and var->rdigits of -6.

          // If our caller gave us 123000000, we need to effectively divide
          // it by 1000000 to line up the 123 with where we want it to go:

          source_rdigits += (-var->rdigits);
          }
        // Either way, we now have everything aligned for the remainder of the
        // processing to work:
        }

      // Convert the scale of value to match the scale of var
      if( source_rdigits < target_rdigits )
        {
        // The source (value), has fewer rdigits than the target (var)

        // Multiply value by ten until the source_rdigits matches the
        // target_rdigits.  No rounding will be necessary
        value *= __gg__power_of_ten(target_rdigits - source_rdigits);
        source_rdigits = target_rdigits;
        }

      if( source_rdigits > target_rdigits )
        {
        // The source(value) has more rdigits than the target (var)

        // Extract those extra digits; we'll need them for rounding:
        __int128 factor = __gg__power_of_ten(source_rdigits - target_rdigits);

        __int128 remainder = value % factor;
        value /= factor;
        source_rdigits = target_rdigits;

        value = int128_to_int128_rounded( rounded,
                                          value,
                                          factor,
                                          remainder,
                                          compute_error);
        }

      // The documentation for ROUNDED MODE PHOHIBITED says that if the value
      // doesn't fit into the target, "...the content of the resultant
      // identifier is unchanged"

      if( compute_error && *compute_error && rounded == prohibited_e )
        {
        // This is the case where we are not supposed to do anything
        }
      else
        {
        // Value is now scaled to the target's target_rdigits
        bool size_error = false;

        bool is_negative = value < 0 ;

        if( !(var->attr & signable_e) && is_negative )
          {
          if(false)
            {
            // I believe the COBOL spec allows for throwing INCOMPATIBLE-DATA
            // errors.  <sound effect: can being kicked down road>
            printf(  "runtime exception: assigning negative "
                     "value to unsigned variable %s\n",
                     var->name);
            }
          // Take the absolute value of value
          value = -value;
          is_negative = false;
          }

        // And now we put value where it belongs
        switch( var->type )
          {
          case FldGroup:
          case FldAlphanumeric:
            {
            // This is sort of a Hail Mary play.  We aren't supposed to do this
            // conversion if rdigits is non-zero.  But we shouldn't have gotten
            // here if rdigits is non-zero.  But we're here, so we'll do the
            // best we can in case somebody came up with a dialect that allows
            // the attempt.

            // Note that sending a signed value to an alphanumeric strips off
            // any plus or minus signs.
            memset(location, 0, length);
            const charmap_t *charmap = __gg__get_charmap(var->encoding);
            size_error = __gg__binary_to_string_encoded(
                                           PTRCAST(char, location),
                                           length > MAX_FIXED_POINT_DIGITS
                                                    ? MAX_FIXED_POINT_DIGITS
                                                    : length/charmap->stride(),
                                           value,
                                           var->encoding);
            break;
            }

          case FldNumericDisplay:
            if( var->attr & signable_e )
              {
              charmap_t *charmap = __gg__get_charmap(var->encoding);
              int stride = charmap->stride();

              // Things get exciting when a numeric-display value is signable

              if( var->attr & separate_e )
                {
                // Whether positive or negative, a sign there will be:
                cbl_char_t sign_ch = is_negative ?
                                charmap->mapped_character(ascii_minus)
                              : charmap->mapped_character(ascii_plus)  ;
                if( var->attr & leading_e )
                  {
                  // The sign character goes into the first location
                  size_error =
                     __gg__binary_to_string_encoded(
                                                PTRCAST(char, location+stride),
                                                var->digits,
                                                value,
                                                var->encoding);
                  charmap->putch(sign_ch, location, (size_t)0);
                  }
                else
                  {
                  // The sign character goes into the last location
                  size_error =
                    __gg__binary_to_string_encoded(PTRCAST(char, location),
                                                    var->digits,
                                                    value,
                                                    var->encoding);
                  charmap->putch(sign_ch, location, length-stride);
                  }
                }
              else
                {
                /*  The sign information is not separate.  The sign information
                    goes into the first byte for LEADING, or the last byte for
                    TRAILING.  For ASCII, the zone will be 0x30.  For EBCDIC,
                    the the zone is 0xC0.  Those get modified, respectively, to
                    0x70 and 0xD0 when the value is negative.  */

                // First, convert the binary value to the correct-length string
                size_error =
                  __gg__binary_to_string_encoded(PTRCAST(char, location),
                                                  var->digits,
                                                  value,
                                                  var->encoding);

                // Check for a size error on a negative value.  It conceivably
                // was truncated down to zero, in which case we need to
                // suppress this is_negative flag.
                if( size_error && is_negative )
                  {
                  // If all of the digits are zero, then the result is zero, and
                  // we have to kill the is_negative flag:
                  is_negative = false;
                  size_t index = 0;
                  while(index<length)
                    {
                    if( charmap->getch(location, &index)
                                        != charmap->mapped_character(ascii_0) )
                      {
                      is_negative = true;
                      break;
                      }
                    }
                  }

                unsigned char *sign_location =
                  var->attr & leading_e ? location
                                        : location + length - stride;
                cbl_char_t sign_digit = charmap->getch(sign_location,
                                                       (size_t)0);
                sign_digit = charmap->set_digit_negative(sign_digit,
                                                         is_negative);
                charmap->putch(sign_digit, sign_location, (size_t)0);
                }
              }
            else
              {
              // It's a simple positive number
              size_error = __gg__binary_to_string_encoded(
                                                    PTRCAST(char, location),
                                                    var->digits,
                                                    value,
                                                    var->encoding);
              }

            break;

          case FldNumericEdited:
            {
            charmap_t *charmap = __gg__get_charmap(var->encoding);
            if( value == 0 && (var->attr & blank_zero_e) )
              {
              charmap->memset(location, charmap->mapped_character(ascii_space), length);
              }
            else
              {
              char ach[512];

              // At this point, value is scaled to the target's rdigits

              size_error = __gg__binary_to_string_ascii(ach,
                                                        var->digits,
                                                        value);
              ach[var->digits] = NULLCH;

              // Convert that string according to the PICTURE clause
              size_error |= __gg__string_to_numeric_edited(
                                 PTRCAST(char, location),
                                ach,
                                target_rdigits,
                                is_negative,
                                var->picture);
              size_t outlength;
              const char *converted = __gg__iconverter(
                                     DEFAULT_SOURCE_ENCODING,
                                     var->encoding,
                                     PTRCAST(char, location),
                                     var->capacity/charmap->stride(),
                                     &outlength);
              memcpy(location, converted, outlength);
              }

            break;
            }

          case FldNumericBinary:
            binary_to_big_endian(   location,
                                    length,
                                    value);
            size_error = value_is_too_big(var, value, source_rdigits);
            break;

          case FldNumericBin5:
          case FldIndex:
          case FldLiteralN:
          case FldPointer:
            // Weirdly, this might be a figurative constant, hopefully usually
            // ZERO.  Everything but HIGH-VALUE will end up zero. HIGH-VALUE
            // will become one, but it is, apparently harmless.  The HIGH-VALUE
            // must get processed separately elsewhere.  As the author, it
            // would be nice if I knew -- but I don't.
            binary_to_little_endian(location,
                                    length,
                                    value);
            size_error = value_is_too_big(var, value, source_rdigits);
            break;

          case FldAlphaEdited:
            {
            char ach[128];
            size_error = __gg__binary_to_string_ascii(ach, length, value);
            ach[length] = NULLCH;

            // Convert that string according to the PICTURE clause
            __gg__string_to_alpha_edited(
                            PTRCAST(char, location),
                            var->encoding,
                            ach,
                            strlen(ach),
                            var->picture);
            break;
            }

          case FldPacked:
            {
            // Convert the binary value to packed decimal.
            int digits = var->digits;

            // Assume for the moment that the res
            unsigned char sign_nybble = 0;
            if( var->attr & packed_no_sign_e )
              {
              // This is COMP-6 packed decimal, with no sign nybble
              sign_nybble = 0;
              }
            else
              {
              // This is COMP-3 packed decimal, so we need to make room to the
              // right of the final decimal digit for the sign nybble:
              value *= 10;
              digits += 1;
              // Figure out what the sign nybble is going to be, and make the
              // the value positive:
              if(var->attr & signable_e)
                {
                // It is signable, so 0xD for negative, and 0xC for positive
                if(value < 0)
                  {
                  sign_nybble = 0x0D;
                  value = -value;
                  }
                else
                  {
                  sign_nybble = 0x0C;
                  }
                }
              else
                {
                // The value is not signable, so the sign nybble is 0xF
                sign_nybble = 0x0F;
                if(value < 0)
                  {
                  value = -value;
                  }
                }
              }

            /*  We need to check if the value is too big, in case our caller
                wants to check for the error condition.  In any event, we need
                to make sure the value actually fits, because otherwise the
                result might have a bad high-place digit for a value with an
                odd number of places. */

            __int128 mask = __gg__power_of_ten(digits);
            size_error = !!(value / mask);
            value %= mask;

            // We are now set up to do the conversion:
            __gg__binary_to_packed(location, digits, value);

            // We can put the sign nybble into place at this point.  Note that
            // for COMP-6 numbers the sign_nybble value is zero, so the next
            // operation is harmless.
            location[length -1] |= sign_nybble;

            // And we're done.
            break;
            }

          default:
            fprintf(stderr, "can't convert in %s() %s %d\n",
                    __func__,
                    var->name,
                    var->type);
            abort();
            break;
          }
        if( compute_error )
          {
          *compute_error |= size_error ? compute_error_truncate : 0;
          }
        }
      }
      break;
    }
  }

#pragma GCC diagnostic ignored "-Wformat-overflow"

static time_t
cobol_time()
  {
  struct cbl_timespec tp;
  __gg__clock_gettime(&tp);
  return tp.tv_sec;
  }

extern "C"
void
__gg__field_from_string(cblc_field_t *field,
                        size_t field_o,
                        size_t field_s,
                  const char *string,
                        size_t string_length)
  {
  // Warning:  field_from_string uses charmap_t, so you can't safely feed it
  // the results of __gg__iconverter without copying them.

  // The string has to be in the field->encoding.  It's legitimate for
  // string_length to be less than field_s; we will right fill with spaces. And
  // it can be greater than field_s, in which case __gg__move will truncate.

  cblc_field_t source = {};
  source.type = FldAlphanumeric;
  source.encoding = field->encoding;
  source.data = reinterpret_cast<unsigned char *>
                                                (const_cast<char *>(string)),
  source.capacity = string_length;
  __gg__move(  field, field_o, field_s,
              &source, source.offset, source.capacity,
              0, truncation_e );
  }

static void
field_from_ascii(cblc_field_t *field, char *psz)
  {
  cblc_field_t source = {};
  source.type = FldAlphanumeric;
  source.capacity = strlen(psz);
  source.data = reinterpret_cast<unsigned char *>(psz);
  source.encoding = __gg__console_encoding;
  __gg__move(  field, field->offset, field->capacity,
             &source, source.offset, source.capacity,
             0, truncation_e );
  }

extern "C"
void
__gg__get_date_yymmdd(cblc_field_t *field)
  {
  char ach[32];

  time_t t = cobol_time();
  const struct tm *local = localtime(&t);

  sprintf(ach,
          "%2.2d%2.2d%2.2d",
          local->tm_year  % 100,
          local->tm_mon+1 % 100,
          local->tm_mday  % 100 );
  field_from_ascii(field, ach);
  }

extern "C"
void
__gg__get_date_yyyymmdd(cblc_field_t *field)
  {
  char ach[32];
  time_t t = cobol_time();
  const struct tm *local = localtime(&t);
  sprintf(ach,
          "%4.4d%2.2d%2.2d",
          local->tm_year + 1900,
          local->tm_mon+1,
          local->tm_mday);
  field_from_ascii(field, ach);
  }

extern "C"
void
__gg__get_date_yyddd(cblc_field_t *field)
  {
  char ach[32];

  time_t t = cobol_time();
  const struct tm *local = localtime(&t);

  sprintf(ach,
          "%2.2d%3.3d",
          local->tm_year % 100,
          local->tm_yday+1);
  field_from_ascii(field, ach);
  }

extern "C"
void
__gg__get_yyyyddd(cblc_field_t *field)
  {
  char ach[32];

  time_t t = cobol_time();
  const struct tm *local = localtime(&t);

  sprintf(ach,
          "%4.4d%3.3d",
          local->tm_year + 1900,
          local->tm_yday+1);
  field_from_ascii(field, ach);
  }

extern "C"
void
__gg__get_date_dow(cblc_field_t *field)
  {
  char ach[32];

  time_t t = cobol_time();
  const struct tm *local = localtime(&t);

  sprintf(ach,
          "%1.1d",
          local->tm_wday == 0 ? 7 : local->tm_wday);
  field_from_ascii(field, ach);
  }

static int
int_from_digits(const char * &p, int ndigits)
  {
  int retval = 0;
  while( p && ndigits )
    {
    char ch = *p++;
    if( isdigit(ch) )
      {
      retval *= 10;
      retval += (ch & 0xF);
      ndigits -= 1;
      }
    }
  return retval;
  }

// For testing purposes, this undef causes the use of gettimeofday().
// #undef HAVE_CLOCK_GETTIME

static uint64_t
get_time_nanoseconds_local()
{
  // This code was unabashedly stolen from gcc/timevar.cc.
  // It returns the Unix epoch with nine decimal places.

  /*  Note:  I am perplexed.  I have been examining the gcc Makefiles and
      configure.ac files, and I am unable to locate where HAVE_GETTIMEOFDAY
      is established.  There have been issues compiling on MacOS, where
      apparently clock_gettime() is not available.  But I don't see exactly
      how gettimeofday() gets used, instead.  But without the ability to
      compile on a MacOS system, I am fumbling along as best I can.

      I decided to simply replace clock_gettime() with getttimeofday() when
      clock_gettime() isn't available, even though gcc/timevar.cc handles
      the situation differently.

           -- Bob Dubner, 2025-06-11*/

  uint64_t retval = 0;

#ifdef HAVE_CLOCK_GETTIME
  struct timespec ts;
  clock_gettime (CLOCK_REALTIME, &ts);
  retval = ts.tv_sec * 1000000000 + ts.tv_nsec;
  return retval;
//#endif
//#ifdef HAVE_GETTIMEOFDAY
#else
  struct timeval tv;
  gettimeofday (&tv, NULL);
  retval = tv.tv_sec * 1000000000 + tv.tv_usec * 1000;
  return retval;
#endif
  return retval;
}

extern "C"
void
__gg__clock_gettime(struct cbl_timespec *tp)
  {
  const char *p = getenv("GCOBOL_CURRENT_DATE");

  if( p )
    {
    time_t t;
    time (&t);
    struct tm tm;

    memset(&tm, 0, sizeof(tm));
    localtime_r(&t, &tm); // This sets tm_isdst for the local time

    tm.tm_year  = int_from_digits(p, 4) - 1900;
    tm.tm_mon   = int_from_digits(p, 2) - 1;
    tm.tm_mday  = int_from_digits(p, 2);
    tm.tm_hour  = int_from_digits(p, 2);
    tm.tm_min   = int_from_digits(p, 2);
    tm.tm_sec   = int_from_digits(p, 2);

    tm.tm_isdst = 0;
    tp->tv_sec  = mktime(&tm);
    tp->tv_nsec = 0;
    if( tm.tm_isdst )
      {
      tp->tv_sec  -= 3600;
      }
    }
  else
    {
    uint64_t ns = get_time_nanoseconds_local();
    tp->tv_sec  = ns/1000000000;
    tp->tv_nsec = ns%1000000000;
    }
  }

extern "C"
void
__gg__get_date_hhmmssff(cblc_field_t *field)
  {
  char ach[32];
  struct cbl_timespec tv;
  __gg__clock_gettime(&tv);

  struct tm tm;
  localtime_r(&tv.tv_sec, &tm);

  // // This routine returns local time:
  // int day_frac = (tv.tv_sec - timezone) % 86400;

  // int hour   = (day_frac / 3600);
  // int minute = (day_frac%3600) / 60;
  // int second = (day_frac % 60);
  int hundredths = tv.tv_nsec/10000000;

  sprintf(ach,
          "%2.2d%2.2d%2.2d%2.2d",
          tm.tm_hour,
          tm.tm_min,
          tm.tm_sec,
          hundredths);
  field_from_ascii(field, ach);
  }

static
uint32_t collation_position( cbl_char_t ch )
  {
  uint32_t retval;
  if( (ch & 0xFFFFFF00) == 0x00000000 )
    {
    // The character fits into the current DISPLAY collation
    retval = collated(ch);
    }
  else
    {
    // It doesn't fit, so use the character value itself
    retval = ch;
    }
  return retval;
  }

static cbl_char_t
uber_compare(cbl_char_t ch_left, cbl_char_t ch_right)
  {
  if( ((ch_left | ch_right) & 0xFFFFFF00) == 0x00000000 )
    {
    // This is where collation is going to have to be fixed for multi-byte
    // encodings.  For now, if both characters fit into 0xFF, then we will
    // use the current collation.  Otherwise, we just compare them

    // Both characters fit into the current DISPLAY codeset, so assume we
    // are using the DISPLAY collation:
    ch_left  = collated(ch_left);
    ch_right = collated(ch_right);
    }
  else
    {
    // Just compare the raw characters.
    }
  cbl_char_t retval = ch_left - ch_right;
  return retval;
  }


extern "C"
int
__gg__setop_compare(
  const cblc_field_t *candidate_field,
  char *domain)
  {
  // This routine is called to compare the characters of 'candidate'
  // against the list of character pairs in 'domain'

  int retval = 0;
  int l;
  int h;
  char *d;

  /* The domain was created by converting characters to their UTF32
     equivalents and then turning that information to hex.  Numerical values,
     which represent collation positions, are flagged as negative values.

     In order to compare the apples in candidate to the UTF32 values in the
     domain, we need to convert the candidate to UTF32 as well: */

  const charmap_t *charmap = __gg__get_charmap(DEFAULT_32_ENCODING);

  size_t nbytes_converted;
  const char *candidate = __gg__iconverter(candidate_field->encoding,
                                            DEFAULT_32_ENCODING,
                                            candidate_field->data,
                                            candidate_field->capacity,
                                            &nbytes_converted);
  const char *candidate_end = candidate + nbytes_converted;
  while(candidate < candidate_end)
    {
    cbl_char_t ch = charmap->getch(candidate, size_t(0));
    candidate += charmap->stride();
    int collation_pos = collation_position(ch);
    d = domain;
    while(*d)
      {
      retval = 0;
      // We are decoding hexadecimal numbers, either in pairs,
      // or singletons:  "20/30 " or "20 ".  The final one is
      // terminated with ' \0'

      // See the comments in genapi.cc::get_class_condition_string
      // to see how this string was encoded.

      l = (int)strtoll(d, reinterpret_cast<char **>(&d), 16);
      if( l < 0 )
        {
        // This is a collation position, as given in the COBOL program. Make
        // it positive, and subtract 1 from it to make it the same space
        // as the collation table:
        l = -l;
        l -= 1;
        }
      else
        {

        }
      h = l;
      if( *d == '/' )
        {
        d += 1;
        h = (int)strtoll(d, reinterpret_cast<char **>(&d), 16);
        if( h < 0 )
          {
          // This is a collation position; make it the same as
          h = -h;
          h -= 1;
          }
        }
      else if( *d == ' ' )
        {
        d += 1;
        }

      if( collation_pos >= l && collation_pos <= h )
        {
        // This character is acceptable
        retval = 1;
        break;
        }
      }

    // We checked the entire list of pairs for the candidate character
    if( retval == 0 )
      {
      // This candidate character failed, so we don't need to check
      // the rest of the candidates
      break;
      }
    }

  return retval;
  }

extern "C"
__int128
__gg__dirty_to_binary_source(const char *dirty,
                             int length,
                             int *rdigits)
  {
  // This routine is used for converting uncontrolled strings to a
  // a 128-bit signed binary number.

  // The string can start with a plus or minus
  // It can contain a single embedded dot
  // The rest of the characters have to be [0-9]
  // Any other character, including a second dot, ends processing.

  // So, a "1ABC" will yield 1; "ABC" will yield 0.

  // It takes pointers to "s_hyphen" and "s_rdigits" so that it can
  // report what it saw.

  // It returns the binary result. So, 1031.2 returns 10312 and s_rdigits=1

  // The binary number, if signed, is returned as a negative number.

  __int128 retval = 0;

  int hyphen = 0;
  *rdigits = 0;

  // Create a delta_r for counting digits to the right of
  // any decimal point.  If and when we encounter a decimal separator,
  // we'll set this to one, otherwise it'll stay zero.
  int delta_r = 0;

  // We now loop over the remaining input characters:
  while( length-- >0 )
    {
    char ch = *dirty++;

    if( ch == ascii_minus )
      {
      hyphen = 1;
      continue;
      }
    if( ch == ascii_plus )
      {
      continue;
      }
    if( ch == __gg__decimal_point && delta_r == 0 )
      {
      // This is the first decimal point we've seen, so we
      // can start counting rdigits:
      delta_r = 1;
      continue;
      }
    if( ch < ascii_0 || ch > ascii_9 )
      {
      // When we hit something that isn't a digit, then we are done
      break;
      }
    retval *= 10;
    retval += ch - ascii_0;
    *rdigits += delta_r;
    }
  if( !retval )
    {
    // Because the result is zero, there can't be a minus sign
    hyphen = 0;
    }
  if( hyphen )
    {
    // We saw a minus sign, so negate the result
    retval = -retval;
    }
  return retval;
  }

extern "C"
__int128
__gg__dirty_to_binary(const char *dirty,
                      cbl_encoding_t encoding,
                      int length,
                      int *rdigits)
  {
  // This routine is used for converting uncontrolled strings to a
  // a 128-bit signed binary number.

  // The string can start with a plus or minus
  // It can contain a single embedded dot
  // The rest of the characters have to be [0-9]
  // Any other character, including a second dot, ends processing.

  // So, a "1ABC" will yield 1; "ABC" will yield 0.

  // It also can handle 12345E-2 notation.

  // It returns the binary result. So, 1031.2 returns 10312 and rdigits=1

  // The binary number, if signed, is returned as a negative number.

  // We are limiting the number of digits in the number to
  // MAX_FIXED_POINT_DIGITS

  charmap_t *charmap    = __gg__get_charmap(encoding);
  int stride = charmap->stride();

  cbl_char_t mapped_minus          = charmap->mapped_character(ascii_minus);
  cbl_char_t mapped_plus           = charmap->mapped_character(ascii_plus);
  cbl_char_t mapped_decimal_point  = charmap->mapped_character(__gg__decimal_point);
  cbl_char_t mapped_0              = charmap->mapped_character(ascii_0);
  cbl_char_t mapped_9              = charmap->mapped_character(ascii_9);
  cbl_char_t mapped_E              = charmap->mapped_character(ascii_E);
  cbl_char_t mapped_e              = charmap->mapped_character(ascii_e);

  __int128 retval = 0;

  int digit_count = 0;
  int hyphen = 0;
  *rdigits = 0;

  // Create a delta_r for counting digits to the right of
  // any decimal point.  If and when we encounter a decimal separator,
  // we'll set this to one, otherwise it'll stay zero.
  int delta_r = 0;

  // We now loop over the remaining input characters:
  cbl_char_t ch = '\0';
  size_t chindex = 0;

  if(length > 0)
    {
    length -= stride;
    ch = charmap->getch(dirty, &chindex);
    if( ch == mapped_minus )
      {
      hyphen = 1;
      }
    else if( ch == mapped_plus )
      {
      // A plus sign is okay
      }
    else if( ch == mapped_decimal_point )
      {
      delta_r = 1;
      }
    else if( ch >= mapped_0
          && ch <= mapped_9  )
      {
      retval = ch - mapped_0 ;
      if( retval )
        {
        digit_count += 1;
        }
      }
    else
      {
      // Because didn't start with minus, plus, a decimal_place or a digit,
      // this isn't a number
      length = 0;
      ch = '\0';
      }
    }

  while( length > 0 )
    {
    length -= stride;
    ch = charmap->getch(dirty, &chindex);
    if( ch == mapped_decimal_point && delta_r == 0 )
      {
      // This is the first decimal point we've seen, so we
      // can start counting rdigits:
      delta_r = 1;
      continue;
      }
    if(    ch < mapped_0
        || ch > mapped_9 )
      {
      // When we hit something that isn't a digit, then we are done
      break;
      }
    if( digit_count < MAX_FIXED_POINT_DIGITS )
      {
      retval *= 10;
      retval += ch - mapped_0 ;
      *rdigits += delta_r;
      if( retval )
        {
        digit_count += 1;
        }
      }
    }

  // Let's check for an exponent:
  if(   ch == mapped_E
     || ch == mapped_e )
    {
    int exponent = 0;
    int exponent_sign = 1;
    if( length > 0  )
      {
      ch = charmap->getch(dirty, chindex);
      if( ch == mapped_plus)
        {
        length -= stride;
        dirty += stride;
        }
      else if( ch == mapped_minus )
        {
        exponent_sign = -1;
        length -= stride;
        dirty += stride;
        }
      }
    while(length > 0)
      {
      length -= stride;
      ch = charmap->getch(dirty, &chindex);
      if(    ch < mapped_0
          || ch > mapped_9 )
        {
        // When we hit something that isn't a digit, then we are done
        break;
        }
      exponent *= 10;
      exponent += ch - mapped_0 ;
      }
    exponent *= exponent_sign;
    // We need to adjust the retval and the rdigits based on the exponent.
    if( exponent < 0)
      {
      *rdigits += -exponent;
      }
    else if(exponent > 0)
      {
      if( exponent <= *rdigits )
        {
        *rdigits -= exponent;
        }
      else
        {
        // Exponent is > rdigits
        retval *= __gg__power_of_ten(exponent - *rdigits);
        *rdigits = 0;
        }
      }
    }

  if( !retval )
    {
    // Because the result is zero, there can't be a minus sign
    hyphen = 0;
    }
  if( hyphen )
    {
    // We saw a minus sign, so negate the result
    retval = -retval;
    }
  return retval;
  }

extern "C"
GCOB_FP128
__gg__dirty_to_float( const char *dirty,
                      int length,
                const cblc_field_t *field)
  {
  // This routine is used for converting uncontrolled strings to a
  // a _Float128

  // The string can start with a plus or minus
  // It can contain a single embedded dot
  // The rest of the characters have to be [0-9]
  // Any other character, including a second dot, ends processing.

  // So, a "1ABC" will yield 1; "ABC" will yield 0.

  // It also can handle 12345E-2 notation.

  GCOB_FP128 retval = 0;

  int rdigits = 0;
  int hyphen  = 0;

  // Create a delta_r for counting digits to the right of
  // any decimal point.  If and when we encounter a decimal separator,
  // we'll set this to one, otherwise it'll stay zero.
  int delta_r = 0;

  // We now loop over the remaining input characters:
  cbl_char_t ch = '\0';

  charmap_t *charmap = __gg__get_charmap(field->encoding);
  cbl_char_t mapped_minus  = charmap->mapped_character(ascii_minus);
  cbl_char_t mapped_plus   = charmap->mapped_character(ascii_plus);
  cbl_char_t mapped_decimal = charmap->mapped_character(__gg__decimal_point);
  cbl_char_t mapped_0       = charmap->mapped_character(ascii_0);
  cbl_char_t mapped_9       = charmap->mapped_character(ascii_9);
  cbl_char_t mapped_E       = charmap->mapped_character(ascii_E);
  cbl_char_t mapped_e       = charmap->mapped_character(ascii_e);

  size_t index = 0;
  if(length-- > 0)
    {
    ch = charmap->getch(dirty, &index);
    if( ch == mapped_minus )
      {
      hyphen = 1;
      }
    else if( ch == mapped_plus )
      {
      // A plus sign is okay
      }
    else if( ch == mapped_decimal )
      {
      delta_r = 1;
      }
    else if(   ch >= mapped_0
            && ch <= mapped_9 )
      {
      retval = ch & 0x0F ;
      }
    else
      {
      // Because didn't start with minus, plus, a decimal_place or a digit,
      // this isn't a number.  Set length to zero to prevent additional
      // processing
      length = 0;
      ch = '\0';
      }
    }

  while( length-- > 0 )
    {
    ch = charmap->getch(dirty, &index);
    if( ch == mapped_decimal && delta_r == 0 )
      {
      // This is the first decimal point we've seen, so we
      // can start counting rdigits:
      delta_r = 1;
      continue;
      }
    if(   ch < mapped_0
       || ch > mapped_9 )
      {
      // When we hit something that isn't a digit, then we are done
      break;
      }
    retval *= 10;
    retval += ch & 0x0F ;
    rdigits += delta_r;
    }

  // Let's check for an exponent:
  int exponent = 0;
  if(    ch == mapped_E
      || ch == mapped_e )
    {
    int exponent_sign = 1;
    if( length > 0  )
      {
      ch = charmap->getch(dirty, &index);
      if( ch == mapped_plus )
        {
        length -= 1;
        dirty += 1;
        }
      else if(ch == mapped_minus )
        {
        exponent_sign = -1;
        length -= 1;
        dirty += 1;
        }
      }
    while(length-- > 0)
      {
      ch = charmap->getch(dirty, &index);
      if(   ch < mapped_0
         || ch > mapped_9 )
        {
        // When we hit something that isn't a digit, then we are done
        break;
        }
      exponent *= 10;
      exponent += ch & 0x0F ;
      }
    exponent *= exponent_sign;
    }

  // We need to adjust the retval based on rdigits and exponent.
  // Notice that 123.45E2 properly comes out to be 12345
  if( exponent - rdigits >= 0 )
    {
    retval *= __gg__power_of_ten(exponent - rdigits);
    }
  else
    {
    retval /= __gg__power_of_ten(rdigits - exponent);
    }

  if( !retval )
    {
    // Because the result is zero, there can't be a minus sign
    hyphen = 0;
    }
  if( hyphen )
    {
    // We saw a minus sign, so negate the result
    retval = -retval;
    }
  return retval;
  }

extern "C"
__int128
__gg__get_integer_binary_value(cblc_field_t *var)
  {
  // This routine is called when a rounded integer is needed

  __int128 retval;

  int rdigits;

  retval = __gg__binary_value_from_field(&rdigits, var);

  while( rdigits-- > 1)
    {
    retval /= 10;
    }

  if( rdigits-- == 1)
    {
    if( retval < 0 )
      {
      retval -= 5;
      }
    else
      {
      retval += 5;
      }
    retval /= 10;
    }

  return retval;
  }

static int
get_scaled_rdigits(const cblc_field_t *field)
  {
  int retval;
  if( !(field->attr & scaled_e) )
    {
    // The value is not P-scaled, so we just use the unchanged rdigits value
    retval = field->rdigits;
    }
  else
    {
    if( field->rdigits < 0 )
      {
      // The PIC string was something like 999PPPP, which means an rdigits value
      // of -4.  We return zero; somebody else will have the job of multiplying
      // the three significant digits by 10^4 to get the magnitude correct.
      retval = 0;
      }
    else
      {
      // The PIC string was something like PPPP999, which means an rdigits value
      // of +4.  We return an rdigits value of 4 + 3 = 7, which will mean that
      // the three significant digits will be scaled to 0.0000999
      retval = field->digits + field->rdigits;
      }
    }
  return retval;
  }

static cbl_encoding_t
format_for_display_internal(char **dest,
                            size_t *dest_size,
                            cblc_field_t *var,
                            unsigned char *actual_location,
                            int   actual_length,
                            int   address_of)
  {
  // dest and dest_size represent a malloced buffer of dest_size.

  // The returned value is NUL-terminated.

  // This routine will put the formatted result into dest if it fits, and
  // realloc dest if it doesn't.  The new_size goes into the dest_size
  // reference.  Used properly, the caller's buffer just keeps getting bigger
  // as necessary, cutting down on the number of reallocations needed.

  // The routine returns the cbl_encoding_t of the result.

  cbl_encoding_t enc_dest = var->encoding;

  int source_rdigits = var->rdigits;

  if( var->attr & scaled_e )
    {
    source_rdigits = 0;
    }

  // Special case, when var->address_of is on

  if( address_of )
    {
    // Assume that DISPLAY OF ADDRESS OF should be what's expected:
    const charmap_t *charmap = __gg__get_charmap(enc_dest);
    __gg__realloc_if_necessary(dest,
                               dest_size,
                               2*sizeof(void *) + charmap->stride());

    sprintf(  *dest,
              "0x%*.*lx",
              (int)(2*sizeof(void *)),
              (int)(2*sizeof(void *)),
              (unsigned long)actual_location);
    enc_dest = __gg__console_encoding;
    goto done;
    }

  switch( var->type )
    {
    case FldLiteralA:
      {
      charmap_t *charmap = __gg__get_charmap(enc_dest);
      __gg__realloc_if_necessary(dest,
                                 dest_size,
                                 actual_length+charmap->stride());

      cbl_figconst_t figconst = (cbl_figconst_t)(var->attr & FIGCONST_MASK);
      if( figconst )
        {
        charmap = __gg__get_charmap(enc_dest);
        int figconst_char  = charmap->figconst_character(figconst);
        memset(*dest, figconst_char, actual_length);
        (*dest)[actual_length] = NULLCH;
        }
      else
        {
        if( actual_location )
          {
          memcpy(*dest, actual_location, actual_length);
          }
        else
          {
          fprintf(stderr, "attempting to display a NULL pointer in %s\n", var->name);
          abort();
          }
        (*dest)[actual_length] = NULLCH;
        }
      break;
      }

    case FldGroup:
    case FldAlphanumeric:
    case FldNumericEdited:
    case FldAlphaEdited:
      {
      charmap_t *charmap = __gg__get_charmap(enc_dest);
      __gg__realloc_if_necessary(dest,
                                 dest_size,
                                 actual_length+charmap->stride());

      cbl_figconst_t figconst = (cbl_figconst_t)(var->attr & FIGCONST_MASK);
      if( figconst )
        {
        charmap = __gg__get_charmap(enc_dest);
        int figconst_char  = charmap->figconst_character(figconst);
        memset(*dest, figconst_char, actual_length);
        (*dest)[actual_length] = NULLCH;
        }
      else
        {
        if( actual_location )
          {
          memcpy(*dest, actual_location, actual_length);
          }
        else
          {
          fprintf(stderr, "attempting to display a NULL pointer in %s\n", var->name);
          abort();
          }
        (*dest)[actual_length] = NULLCH;
        }
      break;
      }

    case FldNumericDisplay:
      {
      charmap_t *charmap = __gg__get_charmap(enc_dest);
      if( var_is_refmod(var) )
        {
        // Because we are dealing with a refmod, we just output those
        // characters.
        __gg__realloc_if_necessary(dest,
                                   dest_size,
                                   actual_length+charmap->stride());
        memcpy((*dest), actual_location, actual_length);
        (*dest)[actual_length] = NULLCH;
        break;
        }

      // We need the counts of digits to the left and right of the decimal point
      int rdigits = get_scaled_rdigits(var);
      int ldigits = var->digits - rdigits;

      // Calculate the minimum allocated size we need, keeping in mind that
      // ldigits can be negative when working with a PPP999 number
      int nsize   = std::max(ldigits,0) + rdigits+1;
      if( ldigits < 0 )
        {
        // We are dealing with a scaled_e number
        rdigits += ldigits;
        }

      if( rdigits )
        {
        // We need room for the inside decimal point
        nsize += 1;
        }
      if( var->attr & signable_e )
        {
        // We need room for a leading or trailing sign in the output
        nsize += 1;
        }

      // nsize is now the actual number of bytes we need in the destination
      __gg__realloc_if_necessary(dest, dest_size, nsize);

      if( actual_location )
        {
        // This buffer is larger than can validly be needed
        unsigned char converted[128];
        size_t outlength;
        enc_dest = DEFAULT_SOURCE_ENCODING;
        const char *mapped = __gg__iconverter(
                                  var->encoding,
                                  enc_dest,
                                  PTRCAST(char, actual_location),
                                  actual_length,
                                  &outlength);
        memcpy(converted, mapped, outlength);
        charmap = __gg__get_charmap(enc_dest);

        // converted[] is now an ASCII version of the value in memory.  We are
        // going to "validate" the characters, which might be garbage.

        int signtype =    (var->attr & signable_e ? 4 : 0)
                        + (var->attr & separate_e ? 2 : 0)
                        + (var->attr & leading_e  ? 1 : 0);
        unsigned char *signloc;
        unsigned char *digits;
        const unsigned char *digits_e;
        bool is_negative;
        int index = 0;  // This is the running index into our output destination

        switch(signtype)
          {
          case 0:
          case 1:
          case 2:
          case 3:
            // not signable
            signloc  = converted;
            digits   = converted;
            digits_e = converted + outlength;
            is_negative = false;
            break;
          case 4:
            {
            // internal trailing
            const charmap_t *charmap_from = __gg__get_charmap(var->encoding);
            cbl_char_t original_sign_digit =
                charmap_from->getch(actual_location,
                                    actual_length - charmap_from->stride());
            signloc  = converted + outlength-1;
            digits   = converted;
            digits_e = converted + outlength;
            /*  In ascii, negative is indicated by turning bit 0x40 on.
                In ebcdic, by turning bit 0x20 off.  In both cases, the result
                is outside of the range '0' through '9'.  Working this way is
                slightly dangerous, because we might miss some particular
                set of garbage that might have been READ or REDEFINED into the
                variable's memory.  I am not overly concerned.
            */
            is_negative = *signloc > ascii_9 || *signloc < ascii_0;
            *signloc = ascii_0 + (original_sign_digit & 0x0F);
            break;
            }
          case 5:
            {
            // internal leading
            const charmap_t *charmap_from = __gg__get_charmap(var->encoding);
            cbl_char_t original_sign_digit =
                charmap_from->getch(actual_location,
                                    (size_t)0);
            signloc  = converted;
            digits   = converted;
            digits_e = converted + outlength;
            is_negative = *signloc > ascii_9 || *signloc < ascii_0;
            *signloc = ascii_0 + (original_sign_digit & 0x0F);
            break;
            }
          case 6:
            // separate trailing
            signloc  = converted + outlength-1;
            digits   = converted;
            digits_e = converted + outlength-1;
            is_negative = *signloc == ascii_minus;
            break;
          case 7:
            // separate leading
            signloc  = converted;
            digits   = converted+1;
            digits_e = converted + outlength;
            is_negative = *signloc == ascii_minus;
            break;
          }
        // We have the sign sorted out; make sure that the digits are valid:
        unsigned char *running_location = digits;
        while(running_location < digits_e)
          {
          if( *running_location < ascii_0 || *running_location > ascii_9 )
            {
            // An invalid digit becomes '0', and the value is flagged positive
            *running_location = ascii_0;
            is_negative = false;
            }
          running_location += 1;
          }

        // converted[] is now full of valid digits, and is_negative has been
        // established.

        switch(signtype)
          {
          case 0:
          case 1:
          case 2:
          case 3:
            // not signable
            break;
          case 4:
          case 5:
            // internal trailing
            // internal leading
            (*dest)[index++] = is_negative ? ascii_minus : ascii_plus;
            break;
          case 6:
            // separate trailing
            // We'll stick on the trailing sign character later
            break;
          case 7:
            // separate leading
            (*dest)[index++] = is_negative ? ascii_minus : ascii_plus;
            break;
          }
        running_location = digits;

        // copy over the characters to the left of the decimal point:
        for(int i=0; i<ldigits; i++ )
          {
          unsigned char ch = *running_location++;
          (*dest)[index++] = ch;
          }

        if( rdigits )
          {
          // Lay down a decimal point
          (*dest)[index++] = charmap->decimal_point();

          if( ldigits < 0 )
            {
            // This is a scaled_e value, and we need that many zeroes:
            for( int i=0; i<-ldigits; i++ )
              {
              (*dest)[index++] = ascii_0;
              }
            }

          // And the digits to the right
          for(int i=0; i<rdigits; i++ )
            {
            unsigned char ch = *running_location++;
            (*dest)[index++] = ch;
            }
          }
        // At this point, for a 999PPP number, we need to tack on the zeroes
        if( var->rdigits < 0 )
          {
          for(int i=0; i < -(var->rdigits); i++)
            {
            (*dest)[index++] = ascii_0;
            }
          }

        if(      var->attr & signable_e
            &&   var->attr & separate_e
            && !(var->attr & leading_e) )
          {
          // We need a trailing plus or minus
          (*dest)[index++] = is_negative ? ascii_minus : ascii_plus;
          }

        (*dest)[index++] = NULLCH;
        }
      else
        {
        fprintf(stderr, "attempting to display a NULL pointer in %s\n", var->name);
        abort();
        }
      }
    break;

    case FldNumericBinary:
    case FldPacked:
    case FldNumericBin5:
      {
      int dummy;
      int digits;
      __int128 value = get_binary_value_local(&dummy,
                                              var,
                                              actual_location,
                                              actual_length);
      // Perhaps weirdly, for a p-scaled value with negative rdigits,
      // value has been scaled by 10**(-rdigits), because for most purposes,
      // get_binary_value_local should be the actual value.  But for display,
      // we need to bring it back down by that factor, because down below we
      // will be adding zeroes to the right.  This is a tug-of-war between
      // USAGE DISPLAY and other USAGES:

      if( (var->attr & scaled_e) && var->rdigits < 0 )
        {
        value = __gg__scale_by_power_of_ten_2(value, var->rdigits);
        }

      if( var->digits )
        {
        digits = var->digits;
        }
      else
        {
        // USAGE BINARY comes through without a digits value.  Force it based on
        // the capacity:
        switch( var->capacity )
          {
          case 1:
            digits = 3;
            break;
          case 2:
            digits = 5;
            break;
          case 4:
            digits = 10;
            break;
          case 8:
            digits = 19;
            break;
          case 16:
            digits = MAX_FIXED_POINT_DIGITS;
            break;
          default:
            warnx("%s(): %s has capacity %ld\n",
                  __func__,
                  var->name,
                  var->capacity);
            abort();
            break;
          }
        }

      char ach[128];
      enc_dest = DEFAULT_SOURCE_ENCODING;
      charmap_t *charmap = __gg__get_charmap(enc_dest);

      __gg__binary_to_string_ascii(ach, digits, value);

      // And copy the code from up above:
      int nsize = digits+1;
      int index = 0;
      if( source_rdigits )
        {
        // We need room for the inside decimal point
        nsize += 1;
        }
      if( var->attr & signable_e )
        {
        // We need room for the leading sign
        nsize += 1;
        }
      __gg__realloc_if_necessary(dest, dest_size, nsize);

      if( var->attr & signable_e )
        {
        if( value < 0 )
          {
          (*dest)[index++] = ascii_minus;
          }
        else
          {
          (*dest)[index++] = ascii_plus;
          }
        }
      // copy over the characters to the left of the decimal point:
      memcpy((*dest)+index, ach, digits - source_rdigits);
      index += digits - source_rdigits;
      if( source_rdigits )
        {
        (*dest)[index++] = charmap->decimal_point();
        memcpy((*dest)+index, ach+(digits-source_rdigits), source_rdigits);
        index += source_rdigits;
        }
      (*dest)[index++] = NULLCH ;
      }
    break;

    case FldIndex:
      {
      // The display of a FldIndex doesn't need to provide clues about its
      // length, so don't bother with leading zeroes.
      int dummy;
      __int128 value = get_binary_value_local(&dummy,
                                              var,
                                              actual_location,
                                              actual_length);
      char ach[64];
      sprintf(ach, "%lu", (unsigned long)value);
      __gg__realloc_if_necessary(dest, dest_size, strlen(ach)+1);
      strcpy(*dest, ach);
      enc_dest = __gg__console_encoding;
      }
    break;

    case FldClass:
      {
      if( var->level != LEVEL88 )
        {
        size_t retsize = MINIMUM_ALLOCATION_SIZE;
        memset(*dest, 0, retsize);
        strcpy(*dest, "<CLASS>");
        }
      else
        {
        // This is a LEVEL 88 variable
        size_t retsize = MINIMUM_ALLOCATION_SIZE;
        memset(*dest, 0, retsize);
        strcpy(*dest, "<LEVEL88>");
        }
      enc_dest = __gg__console_encoding;
      break;
      }

    case FldPointer:
      {
      int digits;
      __int128 value = get_binary_value_local( &digits,
                                               var,
                                               actual_location,
                                               actual_length);
      __gg__realloc_if_necessary(dest, dest_size, 2*sizeof(void *) + 3);

      sprintf(  *dest,
                "0x%*.*lx",
                (int)(2*sizeof(void *)),
                (int)(2*sizeof(void *)),
                (unsigned long)value);
      enc_dest = __gg__console_encoding;
      break;
      }

    case FldFloat:
      {
      switch(var->capacity)
        {
        case 4:
          {
          // We will convert based on the fact that for float32, any seven-digit
          // number converts to float32 and then back again unchanged.

          // We will also format numbers so that we produce 0.01 and 1E-3 on the low
          // side, and 9999999 and then 1E+7 on the high side
          // 10,000,000 = 1E7
          char ach[64];
          _Float32 floatval = *PTRCAST(_Float32, actual_location);
          strfromf32(ach, sizeof(ach), "%.9E", floatval);
          char *p = strchr(ach, 'E');
          if( !p )
            {
            // Probably INF -INF NAN or -NAN, so ach has our result
            }
          else
            {
            p += 1;
            int exp = atoi(p);
            if( exp >= 6 || exp <= -5 )
              {
              // We are going to stick with the E notation, so ach has our result
              }
            else
              {
              // We are going to produce our number in such a way that we specify
              // seven signicant digits, no matter where the decimal point lands.
              // Note that exp is in the range of 6 to -2

              int precision = 9 - exp;
              sprintf(ach, "%.*f", precision, (double)floatval );
              }
            __gg__remove_trailing_zeroes(ach);
            __gg__realloc_if_necessary(dest, dest_size, strlen(ach)+1);
            }
          strcpy(*dest, ach);
          break;
          }

       case 8:
          {
          // We will convert based on the fact that for float32, any 15-digit
          // number converts to float64 and then back again unchanged.

          // We will also format numbers so that we produce 0.01 and 1E-3 on the low
          // side, and 9999999 and then 1E+15 on the high side
          char ach[64];
          _Float64 floatval = *PTRCAST(_Float64, actual_location);
          strfromf64(ach, sizeof(ach), "%.17E", floatval);
          char *p = strchr(ach, 'E');
          if( !p )
            {
            // Probably INF -INF NAN or -NAN, so ach has our result
            }
          else
            {
            p += 1;
            int exp = atoi(p);
            if( exp >= 6 || exp <= -5 )
              {
              // We are going to stick with the E notation, so ach has our result
              }
            else
              {
              // We are going to produce our number in such a way that we specify
              // seven signicant digits, no matter where the decimal point lands.
              // Note that exp is in the range of 6 to -2

              int precision = 17 - exp;

              sprintf(ach, "%.*f", precision, (double)floatval );
              }
            __gg__remove_trailing_zeroes(ach);
            __gg__realloc_if_necessary(dest, dest_size, strlen(ach)+1);
            }
          strcpy(*dest, ach);
          break;
          }

       case 16:
          {
          // We will convert based on the fact that for float32, any 15-digit
          // number converts to float64 and then back again unchanged.

          // We will also format numbers so that we produce 0.01 and 1E-3 on the low
          // side, and 9999999 and then 1E+15 on the high side
          char ach[128];
          // We can't use *(_Float64 *)actual_location;
          // That uses the SSE registers, which won't work if the source isn't
          // on a 16-bit boundary.
          GCOB_FP128 floatval;
          memcpy(&floatval, actual_location, 16);
          strfromfp128(ach, sizeof(ach), "%.36" FP128_FMT "E", floatval);
          char *p = strchr(ach, 'E');
          if( !p )
            {
            // Probably INF -INF NAN or -NAN, so ach has our result
            }
          else
            {
            p += 1;
            int exp = atoi(p);
            if( exp >= 6 || exp <= -5 )
              {
              // We are going to stick with the E notation, so ach has our result
              }
            else
              {
              // We are going to produce our number in such a way that we specify
              // seven signicant digits, no matter where the decimal point lands.
              // Note that exp is in the range of 6 to -2

              int precision = 36 - exp;
              char achFormat[24];
              sprintf(achFormat, "%%.%d" FP128_FMT "f", precision);
              strfromfp128(ach, sizeof(ach), achFormat, floatval);
              }
            __gg__remove_trailing_zeroes(ach);
            __gg__realloc_if_necessary(dest, dest_size, strlen(ach)+1);
            }

          strcpy(*dest, ach);
          break;
          }
        }
      enc_dest = __gg__console_encoding;
      break;
      }

    default:
      fprintf(stderr,
              "Unknown conversion %d in format_for_display_internal\n",
              var->type );
      abort();
      break;
    }

  if( (var->attr & scaled_e) && var->type != FldNumericDisplay )
    {
    charmap_t *charmap = __gg__get_charmap(enc_dest);

    static size_t buffer_size = MINIMUM_ALLOCATION_SIZE;
    static char  *buffer = static_cast<char *>(malloc(buffer_size));
    massert(buffer);
    if( var->rdigits > 0)
      {
      // We have something like 123 or +123.  We need to insert a decimal
      // point and a rdigits zeroes to make it +.000000123

      size_t new_length = strlen(*dest) + var->rdigits + 1 + 1;
      __gg__realloc_if_necessary(&buffer, &buffer_size, new_length);


      memset(buffer, ascii_0, new_length);
      char *p = buffer;
      char *s = *dest;
      if(    ((*dest)[0]&0xFF) < ascii_0
          || ((*dest)[0]&0xFF) > ascii_9 )
        {
        *p++ = (*dest)[0];
        s += 1;
        }
      *p++ = charmap->decimal_point();
      p += var->rdigits;  // Skip over the zeroes
      strcpy(p, s);

      __gg__realloc_if_necessary(dest, dest_size, new_length);
      strcpy(*dest, buffer);
      }
    else // var->rdigits < 0
      {
      // We have something like 123 or +123.  All we need to do is
      // add zeroes to the end:
      size_t new_length = strlen(*dest) + -var->rdigits + 1;
      __gg__realloc_if_necessary(&buffer, &buffer_size, new_length);

      memset(buffer, charmap->mapped_character(ascii_0), new_length);
      buffer[new_length-1] = NULLCH;
      memcpy(buffer, *dest, strlen(*dest));

      __gg__realloc_if_necessary(dest, dest_size, new_length);
      strcpy(*dest, buffer);
      }
    }

  if( var->type == FldNumericBin5 && (var->attr & intermediate_e) )
    {
    // Because this is a intermediate Bin5, let's strip off leading zeroes.
    //
    // Because we don't know what we are dealing with, we created a 37-digit
    // number with a variable number of rdigits.  So, we usually have a boatload
    // of leading zeroes.  I find that display offensive, so let's fix it:
    unsigned char *p1 = (unsigned char *)(*dest);
    if( *p1 == ascii_plus || *p1 == ascii_minus )
      {
      p1 += 1;
      }
    unsigned char *p2 = p1;
    while( p2[0] == ascii_0 && p2[1] != '\0' )
      {
      p2 += 1;
      }
    strcpy(PTRCAST(char, p1), PTRCAST(char, p2));
    }

  done:
  if( enc_dest == custom_encoding_e )
    {
    fprintf(stderr, "Bum encoding in format_for_display_internal\n");
    abort();
    }
  return enc_dest;
  }

static int
compare_88( const char    *list,
            const char    *list_e,
            bool           fig_const,
      const cblc_field_t  *conditional_,
      const unsigned char *conditional_location_,
            int            conditional_length_)
  {
  int cmpval;

  // We know that list through list_e are characters in UTF32 encoding.
  size_t list_len = list_e-list;

  // We need to convert the conditional to be UTF32 as well:
  charmap_t *charmap = __gg__get_charmap(DEFAULT_32_ENCODING);
  size_t stride = charmap->stride();
  cbl_char_t mapped_space = charmap->mapped_character(ascii_space);

  // First, convert the conditional to UTF32
  size_t conditional_length=0;
  char * conditional_i = __gg__miconverter(
                                conditional_->encoding,
                                DEFAULT_32_ENCODING,
                                conditional_location_,
                                conditional_length_,
                                &conditional_length);
  const char *conditional = conditional_i;

  // Now we want to trim away trailing spaces from the conditional, leaving
  // just one so that we don't get down to an empty string.
  while( conditional_length > stride)
    {
    cbl_char_t ch = charmap->getch(conditional, conditional_length - stride);
    if( ch != mapped_space )
      {
      break;
      }
    conditional_length -= stride;
    }

  // We have conditional_length bytes at conditional.  Create a test area that
  // we will compare against conditional:

  int test_len;
  char *test;
  if( fig_const )
    {
    // The 'list' is a figurative constant, so we need to create a test
    // buffer that is all the character designated by the figurative constant.

    test = static_cast<char *>(malloc(conditional_length));
    massert(test);
    test_len = conditional_length;

    // This is where we handle the zero-length strings that
    // nonetheless can magically be expanded into figurative
    // constants:

    // We default to space, since we know that the figurative constant is
    // S, Z, H, Q, or L
    cbl_char_t ch = charmap->mapped_character(ascii_space);
    // Check for the strings starting with 0xFF whose second character
    // indicates a figurative constant:
    cbl_char_t char_0 = charmap->getch(list, (size_t)0);
    if( char_0 == charmap->mapped_character(ascii_Z) )
      {
      ch = charmap->mapped_character(ascii_0);
      }
    else if( char_0 == charmap->mapped_character(ascii_H) )
      {
      ch = charmap->high_value_character();
      }
    else if( char_0 == charmap->mapped_character(ascii_Q) )
      {
      ch = charmap->quote_character();
      }
    else if( char_0 == charmap->mapped_character(ascii_L) )
      {
      ch = charmap->low_value_character();
      }
    // The test location is full of the figurative constant
    charmap->memset( test, ch, conditional_length );
    }
  else if( list_len < conditional_length )
    {
    // 'list' element is too short; we have to right-fill with spaces:
    test = static_cast<char *>(malloc(conditional_length));
    massert(test);
    test_len = conditional_length;
    // Copy over the shorty string from 'list'
    memcpy(test, list, list_len);
    // Right fill with spaces:
    charmap->memset(test+list_len,
                    charmap->mapped_character(ascii_space),
                    conditional_length-list_len);
    }
  else
    {
    // list_len is >= conditional length.  Presumably the parser ensured that
    // the list element couldn't be bigger than the maximum condition length,
    // we we'll truncate at list_len:
    test = static_cast<char *>(malloc(list_len));
    massert(test);
    test_len = list_len;
    memcpy(test, list, list_len);
    }

  // At this point we have conditional and test, and they both have at least
  // test_len bytes.

  cmpval = memcmp(test, conditional, test_len);

  free(test);
  free(conditional_i);

  return cmpval;
  }

static GCOB_FP128
get_float128( const cblc_field_t *field,
              unsigned char *location )
  {
  GCOB_FP128 retval=0;
  if(field->type == FldFloat )
    {
    switch( field->capacity )
      {
      case 4:
        retval = *PTRCAST(_Float32 , location);
        break;
      case 8:
        retval = *PTRCAST(_Float64 , location);
        break;
      case 16:
        // retval = *(_Float128 *)location; doesn't work, because the SSE
        // registers need the source on a 16-byte boundary, and we can't
        // guarantee that.
        memcpy(&retval, location, 16);
        break;
      }
    }
  else if( field->type == FldLiteralN )
    {
    union
      {
      __int128 i128;
      uint64_t u64;
      uint32_t u32;
      uint16_t u16;
      uint8_t  u8 ;
      int64_t  i64;
      int32_t  i32;
      int16_t  i16;
      int8_t   i8 ;
      };
    i128 = 0;
    memcpy(&i128, field->data, field->capacity);

    if( field->attr & signable_e )
      {
      switch(field->capacity)
        {
        case 16:
          retval = i128;
          break;
        case 8:
          retval = i64;
          break;
        case 4:
          retval = i32;
          break;
        case 2:
          retval = i16;
          break;
        case 1:
          retval = i8;
          break;
        }
      }
    else
      {
      switch(field->capacity)
        {
        case 16:
          retval = i128;
          break;
        case 8:
          retval = u64;
          break;
        case 4:
          retval = u32;
          break;
        case 2:
          retval = u16;
          break;
        case 1:
          retval = u8;
          break;
        }
      }
    if( field->rdigits )
      {
      retval /= __gg__power_of_ten( field->rdigits );
      }
    }
  return retval;
  }

static
int
compare_field_class(const cblc_field_t  *conditional,
                    unsigned char       *conditional_location,
                    int                  conditional_length,
                    cblc_field_t        *list)
  {
  int retval = 1; // Zero means equal
  __int128 value;
  int rdigits;

  charmap_t *charmap32 = __gg__get_charmap(DEFAULT_32_ENCODING);
  int stride32 = charmap32->stride();
  cbl_char_t mapped_F = charmap32->mapped_character(ascii_F);
  cbl_char_t mapped_Z = charmap32->mapped_character(ascii_Z);

  // We are disassembling strings that have the form <length><flag><value>

  switch( conditional->type )
    {
    case FldNumericDisplay:
    case FldNumericEdited:
    case FldNumericBinary:
    case FldPacked:
    case FldNumericBin5:
    case FldIndex:
      {
      value = get_binary_value_local (&rdigits,
                                      conditional,
                                      conditional_location,
                                      conditional_length);
      char *walker = list->initial;
      while(*walker)
        {
        cbl_char_t left_flag;
        size_t left_len;
        char *left;

        cbl_char_t right_flag;
        size_t right_len;
        char *right;

        char *pend;

        left_len = charmap32->strtoull(walker, &pend, 10);
        left_flag = charmap32->getch(pend, (size_t)0);
        left = pend+stride32;

        right = left + left_len*stride32;
        right_len = charmap32->strtoull(right, &pend, 10);
        right_flag = charmap32->getch(pend, (size_t)0);
        right = pend+stride32;

        walker = right + right_len*stride32;

        int left_rdigits;
        int right_rdigits;

        __int128 left_value;
        cbl_char_t left_0 = charmap32->getch(left, size_t(0));
        if( left_flag == mapped_F && left_0 == mapped_Z )
          {
          left_value = 0;
          left_rdigits = 0;
          }
        else
          {
          left_value = __gg__dirty_to_binary(
                                  left,
                                  DEFAULT_32_ENCODING,
                                  left_len*stride32,
                                  &left_rdigits);
          }

        __int128 right_value;
        cbl_char_t right_0 = charmap32->getch(right, size_t(0));
        if( right_flag == ascii_F && right_0 == mapped_Z )
          {
          right_value = 0;
          right_rdigits = 0;
          }
        else
          {
          right_value = __gg__dirty_to_binary(
                                   right,
                                   DEFAULT_32_ENCODING,
                                   right_len*stride32,
                                   &right_rdigits);
          }

        // Normalize all three numbers to the same rdigits
        int max = std::max(rdigits, left_rdigits);
        max = std::max(max, right_rdigits);

        if( max > rdigits )
          {
          value *= __gg__power_of_ten(max - rdigits);
          }
        if( max > left_rdigits )
          {
          left_value *= __gg__power_of_ten(max - left_rdigits);
          }
        if( max > right_rdigits )
          {
          right_value *= __gg__power_of_ten(max - right_rdigits);
          }
        if( left_value <= value && value <= right_value )
          {
          retval = 0;
          break;
          }
        }
      break;
      }

    case FldGroup:
    case FldAlphanumeric:
    case FldLiteralA:
      {
      // This is an alphanumeric comparison.  The list is in UTF32, so we
      // are going to have to convert the conditional to UTF32.
      char *walker = list->initial;
      while(*walker)
        {
        bool fig1;
        bool fig2;
        char *first;
        char *last;
        char *first_e;
        char *last_e;
        size_t first_len;
        size_t last_len;

        char *pend;

        cbl_char_t ch;

        first = walker;
        first_len = charmap32->strtoull(first, &pend, 10);
        ch = charmap32->getch(pend, (size_t)0);
        fig1 = ch == mapped_F;
        first = pend+stride32;
        first_e = first + first_len*stride32;

        last = first_e;

        last_len = charmap32->strtoull(last, &pend, 10);
        ch = charmap32->getch(pend, (size_t)0);
        fig2 = ch == mapped_F;
        last = pend+stride32;
        last_e = last + last_len*stride32;

        walker = last_e;

        int compare_result;

        compare_result = compare_88(first,
                                    first_e,
                                    fig1,
                                    conditional,
                                    conditional_location,
                                    conditional_length);
        if( compare_result > 0 )
          {
          // First is > conditional, so this is no good
          continue;
          }
        compare_result = compare_88(last,
                                    last_e,
                                    fig2,
                                    conditional,
                                    conditional_location,
                                    conditional_length);
        if( compare_result < 0 )
          {
          // Last is < conditional, so this is no good
          continue;
          }

        // conditional is inclusively between first and last
        retval = 0;
        break;
        }
      break;
      }

    case FldFloat:
      {
      // We need a fake field to hold the encoding for the
      // __gg__dirty_to_float() routine.
      cblc_field_t fakir;
      fakir.encoding = DEFAULT_32_ENCODING;

      GCOB_FP128 fp128 = get_float128(conditional, conditional_location) ;
      char *walker = list->initial;
      while(*walker)
        {
        cbl_char_t   left_flag;
        size_t left_len;
        char * left;

        cbl_char_t   right_flag;
        size_t right_len;
        char * right;

        char *pend;
        left_len = charmap32->strtoull(walker, &pend, 10);
        left_flag = charmap32->getch(pend, (size_t)0);
        left = pend+stride32;

        right = left + left_len*stride32;
        right_len = charmap32->strtoull(right, &pend, 10);
        right_flag = charmap32->getch(pend, (size_t)0);
        right = pend+stride32;

        walker = right + right_len*stride32;

        GCOB_FP128 left_value;
        if( left_flag == mapped_F
            && charmap32->getch(left, (size_t)0) == mapped_Z )
          {
          left_value = 0;
          }
        else
          {
          left_value = __gg__dirty_to_float(left,
                                            left_len,
                                            &fakir);
          }

        GCOB_FP128 right_value;
        if( right_flag == mapped_F
            && charmap32->getch(right, (size_t)0) == mapped_Z )
          {
          right_value = 0;
          }
        else
          {
          right_value = __gg__dirty_to_float( right,
                                              right_len,
                                              &fakir);
          }

        if( left_value <= fp128 && fp128 <= right_value )
          {
          retval = 0;
          break;
          }
        }
      break;
      }

    default:
      printf( "%s(): doesn't know what to do with %s\n",
              __func__,
              conditional->name);
      abort();
    }

  return retval;
  }

static
bool
local_is_numeric(int type, bool address_of)
  {
  bool retval;
  if( address_of )
    {
    retval = true;
    }
  else
    {
    switch(type)
      {
      case FldNumericDisplay:
      case FldNumericBinary:
      case FldPacked:
      case FldNumericBin5:
      case FldIndex:
      case FldPointer:
      case FldLiteralN:
      case FldFloat:
        retval = true;
        break;
      default:
        retval = false;
        break;
      }
    }
  return retval;
  }

static
bool
local_is_alpha(int type, bool address_of)
  {
  bool retval;
  if( address_of )
    {
    retval = false;
    }
  else
    {
    switch(type)
      {
      case FldGroup:
      case FldAlphanumeric:
      case FldAlphaEdited:
      case FldNumericEdited:
      case FldLiteralA:
        retval = true;
        break;
      default:
        retval = false;
        break;
      }
    }
  return retval;
  }

static void
interconvert( char **allocated_left,
              char **allocated_right,
              char **left_string,
              char **right_string,
              size_t *left_length,
              size_t *right_length,
              cbl_encoding_t *encoding_left,
              cbl_encoding_t *encoding_right)
  {
  // This routine looks at two encodings and decides what do to about comparing
  // apples to apples.
  *allocated_left  = nullptr;
  *allocated_right = nullptr;

  bool convert_left_to_right = false;
  bool convert_right_to_left = false;

  size_t converted_length;
  const char *converted;
  if( *encoding_left == *encoding_right )
    {
    // This is both the most-seen situation, and, happily, the easiest to
    // handle.  We just do nothing.
    }
  else if(   *encoding_left  == __gg__national_encoding
          || *encoding_right == __gg__national_encoding )
    {
    // The encodings are different, but at least one is the national encoding.
    // Convert the other one to be national as well:
    if( *encoding_left != __gg__national_encoding )
      {
      convert_left_to_right = true;
      }
    else
      {
      convert_right_to_left = true;
      }
    }
  else
    {
    // We have two different encodings, and neither of them are national.  This
    // can happen when a file descriptor has a specific codeset that doesn't
    // match the national codeset.  We will convert the narrower to the wider;
    // if they are both the same width we will pick one arbitrarily.
    const charmap_t *charmap_left  = __gg__get_charmap(*encoding_left);
    const charmap_t *charmap_right = __gg__get_charmap(*encoding_right);
    if( charmap_right->stride() >= charmap_left->stride() )
      {
      convert_left_to_right = true;
      }
    else
      {
      convert_right_to_left = true;
      }
    }

  if( convert_left_to_right )
    {
    // Convert the left side to the right encoding
    converted = __gg__iconverter(*encoding_left,
                                 *encoding_right,
                                 *left_string,
                                 *left_length,
                                 &converted_length);
    *encoding_left = *encoding_right ;
    *allocated_left = static_cast<char *>(malloc(converted_length));
    massert(*allocated_left);
    *left_string = *allocated_left;
    *left_length = converted_length;
    memcpy(*left_string, converted, *left_length);
    }
  if( convert_right_to_left )
    {
    // Convert the right side to the left_encoding
    converted = __gg__iconverter(*encoding_right,
                                 *encoding_left,
                                 *right_string,
                                 *right_length,
                                 &converted_length);
    *encoding_right = *encoding_left ;
    *allocated_right = static_cast<char *>(malloc(converted_length));
    massert(*allocated_right);
    *right_string = *allocated_right;
    *right_length = converted_length;
    memcpy(right_string, converted, *right_length);
    }
  }

static
int
compare_strings(char   *left_string,
                size_t  left_length,
                bool    left_all,
                char   *right_string,
                size_t  right_length,
                bool    right_all,
                cbl_encoding_t encoding_left,
                cbl_encoding_t encoding_right)
  {
  // This routine compares two strings.  It sounds innocent enough, right? But
  // we have to deal with different encodings.  It's not clear what the rules
  // are, or should be, and collation is just a mess.  We are going to be
  // playing Whac-A-Mole with this routine, possibly until the end of time.

  char *allocated_left  = nullptr;
  char *allocated_right = nullptr;

  interconvert(&allocated_left,
               &allocated_right,
               &left_string,
               &right_string,
               &left_length,
               &right_length,
               &encoding_left,
               &encoding_right);

  charmap_t *charmap_left  = __gg__get_charmap(encoding_left);
  charmap_t *charmap_right = __gg__get_charmap(encoding_right);

  int retval = 0;
  size_t index_left  = 0;
  size_t index_right = 0;

  if( right_all && right_length > left_length )
    {
    // If the right side is ALL, and is longer than the left side, we just
    // compare the matching characters.
    right_length = left_length;
    }

  if( left_all && left_length > right_length )
    {
    // If the left side is ALL, and is longer than the right side, we just
    // compare the matching characters.
    left_length = right_length;
    }

  while( !retval && index_left<left_length && index_right<right_length )
    {
    cbl_char_t ch_left  = charmap_left->getch(left_string, &index_left);
    cbl_char_t ch_right = charmap_right->getch(right_string, &index_right);
    retval = uber_compare(ch_left, ch_right);
    }

  // We need to space-extend the shorter value.  That's because
  // "Bob" is equal to "Bob     "
  if( !right_all )
    {
    while( !retval && index_left<left_length )
      {
      cbl_char_t ch_left  = charmap_left->getch(left_string, &index_left);
      cbl_char_t ch_right = charmap_right->mapped_character(ascii_space);
      retval = uber_compare(ch_left, ch_right);
      }
    }
  else
    {
    // In an ALL situation where the ALL is shorter than the fixed side, we
    // wrap around the ALL characters
    while( !retval && index_left<left_length )
      {
      index_right %= right_length;
      cbl_char_t ch_left  = charmap_left->getch(left_string, &index_left);
      cbl_char_t ch_right = charmap_right->getch(right_string, &index_right);
      retval = uber_compare(ch_left, ch_right);
      }
    }

  if( !left_all )
    {
    while( !retval && index_right<right_length )
      {
      cbl_char_t ch_left  = charmap_left->mapped_character(ascii_space);
      cbl_char_t ch_right = charmap_right->getch(right_string, &index_right);
      retval = uber_compare(ch_left, ch_right);
      }
    }
  else
    {
    while( !retval && index_right<right_length )
      {
      index_left %= left_length;
      cbl_char_t ch_left  = charmap_left->mapped_character(ascii_space);
      cbl_char_t ch_right = charmap_right->getch(right_string, &index_right);
      retval = uber_compare(ch_left, ch_right);
      }
    }

  free(allocated_right);
  free(allocated_left);
  return retval;
  }

extern "C"
int
__gg__compare_2(cblc_field_t  *left_side,
                unsigned char *left_location,
                size_t         left_length,
                uint64_t       left_attr,
                int            left_flags,
                cblc_field_t  *right_side,
                unsigned char *right_location,
                size_t         right_length,
                uint64_t       right_attr,
                int            right_flags,
                int            second_time_through)
  {
  // First order of business:  If right_side is a FldClass, pass that off
  // to the speciality squad:

  if( right_side->type == FldClass )
    {
    return compare_field_class( left_side,
                                left_location,
                                left_length,
                                right_side);
    }

  // Serene in our conviction that the left_side isn't a FldClass, we
  // move on.

  // Extract the individual flags from the flag words:
  bool left_all         = !!(left_flags  & REFER_T_MOVE_ALL  );
  bool left_address_of  = !!(left_flags  & REFER_T_ADDRESS_OF);
  bool right_all        = !!(right_flags & REFER_T_MOVE_ALL  );
  bool right_address_of = !!(right_flags & REFER_T_ADDRESS_OF);
//bool left_refmod      = !!(left_flags  & REFER_T_REFMOD    );
  bool right_refmod     = !!(right_flags & REFER_T_REFMOD    );

  // There are a bunch of cases where we might be dealing with encoding:
  cbl_encoding_t encoding_left = left_side->encoding;
  cbl_encoding_t encoding_right = right_side->encoding;
  charmap_t *charmap_left  = __gg__get_charmap(encoding_left);
  charmap_t *charmap_right = __gg__get_charmap(encoding_right);
  int stride = charmap_left->stride();

  // Figure out if we have any figurative constants
  cbl_figconst_t left_figconst  = (cbl_figconst_t)(left_attr  & FIGCONST_MASK);
  cbl_figconst_t right_figconst = (cbl_figconst_t)(right_attr & FIGCONST_MASK);

  cbl_char_t fig_left  = 0;
  cbl_char_t fig_right = 0;

  if( left_figconst )
    {
    fig_left  = charmap_left->figconst_character(left_figconst);
    }
  if( right_figconst )
    {
    fig_right = charmap_right->figconst_character(right_figconst);
    }

  // We have four high-level conditions to consider depending on whether
  // left and/or right are figurative constants:

  int retval = 0;
  bool compare = false;

  if( left_figconst && right_figconst )
    {
    // We are comparing two figurative constants
    retval = uber_compare(fig_left, fig_right);
    compare = true;
    goto fixup_retval;
    }
  if( left_figconst && !right_figconst )
    {
    // Go directly to fixup_retval.  Because 'compare' is false, we'll
    // end up trying again with the variables swapped:
    goto fixup_retval;
    }
  if( !left_figconst && right_figconst )
    {
    // We are comparing the left side to a figurative constant:
    switch( right_figconst )
      {
      default:
        fprintf(stderr,
                "%s() %s:%d -- Unknown figurative constant %d\n",
                __func__, __FILE__, __LINE__,
                (int)right_figconst);
        abort();
        break;

      case null_value_e:
        break;

      case low_value_e:
      case high_value_e:
      case quote_value_e:
      case space_value_e:
        retval = 0;
        for(size_t i=0; i<left_length; i+=stride)
          {
          // The right side is a figurative constant.  Compare data from the
          // left side to the figurative constant from the right converted to
          // the left encoding:
          cbl_char_t fig_of_right =
                             charmap_left->figconst_character(right_figconst);
          cbl_char_t left_ch = charmap_left->getch(left_location, i);
          retval = uber_compare(left_ch, fig_of_right);
          if( retval )
            {
            break;
            }
          }

        compare = true;
        goto fixup_retval;
        break;

      case zero_value_e:
        {
        switch( left_side->type )
          {
          case FldNumericBinary:
          case FldPacked:
          case FldNumericBin5:
          case FldNumericDisplay:
          case FldLiteralN:
          case FldIndex:
          case FldPointer:
            // ZEROES is a chameleon.  When compared to a numeric, it is
            // the number zero:
            {
            int rdigits;
            __int128 value;

            value = get_binary_value_local( &rdigits,
                                            left_side,
                                            left_location,
                                            left_length);
            retval = 0;
            retval = value < 0 ? -1 : retval;
            retval = value > 0 ?  1 : retval;
            compare = true;
            break;
            }

          case FldFloat:
            {
            GCOB_FP128 value = __gg__float128_from_location(left_side,
                                                           left_location);
            retval = 0;
            retval = value < 0 ? -1 : retval;
            retval = value > 0 ?  1 : retval;
            compare = true;
            break;
            }

          default:
            // We are comparing a alphanumeric string to ZEROES
            retval = 0;
            for(size_t i=0; i<left_length; i+=stride)
              {
              unsigned int fig_of_right =
                             charmap_left->figconst_character(right_figconst);
              cbl_char_t ch_left = charmap_left->getch(left_location, i);
              retval = uber_compare(ch_left, fig_of_right);
              if( retval )
                {
                break;
                }
              }
            compare = true;
            break;
          }
        goto fixup_retval;
        }
      }
    }
  else
    {
    // Neither left_side nor right_side is a figurative constant.

    // Our strategy here is to compare two alphanumerics, two numerics,
    // or an alphanumeric to a numeric.  We'll handle a numeric to an
    // alphanumeric on a second-time-through.

    if(    local_is_alpha(left_side->type,  left_address_of)
        && local_is_alpha(right_side->type, right_address_of) )
      {
      if( (left_side->attr | right_side->attr) & hex_encoded_e )
        {
        encoding_left = encoding_right = iconv_CP1252_e;
        }
      retval = compare_strings(   reinterpret_cast<char *>(left_location),
                                  left_length,
                                  left_all,
                                  reinterpret_cast<char *>(right_location),
                                  right_length,
                                  right_all,
                                  encoding_left,
                                  encoding_right );

      compare = true;
      goto fixup_retval;
      }

    if(     local_is_numeric(left_side->type,  left_address_of)
        &&  local_is_numeric(right_side->type, right_address_of) )
      {
      if( left_side->type == FldFloat && right_side->type == FldFloat )
        {
        // One or the other of the numerics is a FldFloat
        GCOB_FP128 left_value  = __gg__float128_from_location(left_side,  left_location);
        GCOB_FP128 right_value = __gg__float128_from_location(right_side, right_location);
        retval = 0;
        retval = left_value < right_value ? -1 : retval;
        retval = left_value > right_value ?  1 : retval;
        compare = true;
        goto fixup_retval;
        }

      if( left_side->type == FldFloat )
        {
        // The left side is a FldFloat; the other is another type of numeric:
        int rdecimals;
        GCOB_FP128 left_value;
        GCOB_FP128 right_value;

        if( right_side->type == FldLiteralN)
          {
          right_value = get_float128(right_side, right_location);
          // In order to do the comparision, we need the value from the
          // literal to be the same flavor as the left side:
          switch(left_side->capacity)
            {
            case 4:
              {
              _Float32 left_value4  = *PTRCAST(_Float32, left_location);
              _Float32 right_value4 = (_Float32)right_value;
              retval = 0;
              retval = left_value4 < right_value4 ? -1 : retval;
              retval = left_value4 > right_value4 ?  1 : retval;
              break;
              }
            case 8:
              {
              _Float64 left_value8  = *PTRCAST(_Float64, left_location);
              _Float64 right_value8 = (_Float64)right_value;
              retval = 0;
              retval = left_value8 < right_value8 ? -1 : retval;
              retval = left_value8 > right_value8 ?  1 : retval;
              break;
              }
            case 16:
              {
              //_Float128 left_value  = *(_Float128 *)left_location;
              GCOB_FP128 left_value16;
              memcpy(&left_value16, left_location, 16);
              GCOB_FP128 right_value16 = right_value;
              retval = 0;
              retval = left_value16 < right_value16 ? -1 : retval;
              retval = left_value16 > right_value16 ?  1 : retval;
              break;
              }
            }
          }
        else
          {
          left_value  = __gg__float128_from_location(left_side,  left_location);

          right_value = get_binary_value_local( &rdecimals,
                                                right_side,
                                                right_location,
                                                right_length);
          right_value /= __gg__power_of_ten(rdecimals);
          retval = 0;
          retval = left_value < right_value ? -1 : retval;
          retval = left_value > right_value ?  1 : retval;
          }

        compare = true;
        goto fixup_retval;
        }

      if( left_side->type != FldFloat && right_side->type != FldFloat)
        {
        // We are comparing a numeric to a numeric, neither are floats
        int ldecimals;
        int rdecimals;
        __int128 left_value;
        __int128 right_value;

        if( left_address_of )
          {
          left_value = (__int128)left_location;
          ldecimals = 0;
          }
        else
          {
          left_value  = get_binary_value_local( &ldecimals,
                                                left_side,
                                                left_location,
                                                left_length);
          }
        if( right_address_of )
          {
          right_value = (__int128)right_location;
          rdecimals = 0;
          }
        else
          {
          right_value = get_binary_value_local( &rdecimals,
                                              right_side,
                                              right_location,
                                              right_length);
          }

        // We need to align the decimal points:
        if(rdecimals > ldecimals)
          {
          left_value *= __gg__power_of_ten(rdecimals-ldecimals);
          }
        else if( ldecimals > rdecimals )
          {
          right_value *= __gg__power_of_ten(ldecimals-rdecimals);
          }

        retval = 0;
        retval = left_value < right_value ? -1 : retval;
        retval = left_value > right_value ?  1 : retval;
        compare = true;
        goto fixup_retval;
        }
      }

    if(    local_is_alpha(left_side->type, left_address_of)
        && local_is_numeric(right_side->type, right_address_of) )
      {
      // We are comparing an alphanumeric to a numeric.

      // The right side is numeric.  Sometimes people write code where they
      // take the refmod of a numeric displays.  If somebody did that here,
      // just do a complete straight-up character by character comparison:

      if( right_refmod )
        {
        if( (left_side->attr | right_side->attr) & hex_encoded_e )
          {
          encoding_left = encoding_right = iconv_CP1252_e;
          }
        retval = compare_strings(   reinterpret_cast<char *>(left_location),
                                    left_length,
                                    left_all,
                                    reinterpret_cast<char *>(right_location),
                                    right_length,
                                    right_all,
                                    left_side->encoding,
                                    right_side->encoding );
        compare = true;
        goto fixup_retval;
        }


      // The trick here is to convert the numeric to its display form,
      // and compare that to the alphanumeric. For example, when comparing
      // a VAL5 PIC X(3) VALUE 5 to literals,
      //
      // VAL5 EQUAL    5  is TRUE
      // VAL5 EQUAL  005  is TRUE
      // VAL5 EQUAL   "5" is FALSE
      // VAL5 EQUAL "005" is TRUE
      if( left_side->type == FldLiteralA )
        {
        left_location = reinterpret_cast<unsigned char *>(left_side->data);
        left_length   = left_side->capacity;
        }

      static size_t right_string_size = MINIMUM_ALLOCATION_SIZE;
      static char *right_string
                              = static_cast<char *>(malloc(right_string_size));

      cbl_encoding_t encoding_formatted =
                            format_for_display_internal( &right_string,
                                                         &right_string_size,
                                                         right_side,
                                                         right_location,
                                                         right_length,
                                                         0);
      size_t right_string_length = strlen(right_string);
      if( encoding_formatted != encoding_left )
        {
        // The encodings are not the same.  We need to convert the right_string
        // to the same encoding as the left side:
        size_t outsize;
        const char *converted = __gg__iconverter(encoding_formatted,
                                                 encoding_left,
                                                 right_string,
                                                 right_string_length,
                                                 &outsize);
        memcpy(right_string, converted, outsize);
        right_string_length = outsize;
        }

      // There is a tricky aspect to comparing an alphanumeric to
      // a string.  In short, we have to strip off any leading plus sign

      // And, according to the NIST tests, the same is true for minus signs.
      // Apparently, when comparing a number to an alphanumeric, it is
      // considered a "pseudo-move", and the rule for moving a negative
      // number to an alphanumeric is that negative signs get stripped off

      cbl_char_t left_ch = charmap_left->getch(left_location, size_t(0));
      if(    left_ch == charmap_left->mapped_character(ascii_plus)
          || left_ch == charmap_left->mapped_character(ascii_minus)  )
        {
        left_location += charmap_left->stride();
        left_length -= charmap_left->stride();
        }

      char *right_fixed;
      cbl_char_t right_ch = charmap_right->getch(right_string, size_t(0));
      if(  right_ch == charmap_right->mapped_character(ascii_plus)
        || right_ch == charmap_right->mapped_character(ascii_minus)  )
        {
        right_fixed = right_string + charmap_right->stride();
        right_string_length -= charmap_right->stride();
        }
      else
        {
        right_fixed = right_string;
        }

      if( (left_side->attr | right_side->attr) & hex_encoded_e )
        {
        encoding_left = encoding_right = iconv_CP1252_e;
        }
      retval = compare_strings(   reinterpret_cast<char *>(left_location),
                                  left_length,
                                  left_all,
                                  right_fixed,
                                  right_string_length,
                                  right_all,
                                  encoding_left,
                                  encoding_left);
      compare = true;
      goto fixup_retval;
      }
    }

fixup_retval:

  if( !compare && !second_time_through)
    {
    // This is the first time through, and we couldn't do the comparison.
    // Maybe we have to reverse the inputs:
    retval = __gg__compare_2(   right_side,
                                right_location,
                                right_length,
                                right_attr,
                                right_flags,
                                left_side,
                                left_location,
                                left_length,
                                left_attr,
                                left_flags,
                                1);
    // And reverse the sense of the return value:
    compare = true;
    retval = -retval;
    }

  if( !compare && second_time_through )
    {
    // Nope.  We still couldn't do the comparison
    fprintf(stderr, "###### %10s in %s:%d\n", __func__, __FILE__, __LINE__ );
    fprintf(stderr, "###### We don't know how to compare types %d and %d\n",
            left_side->type,
            right_side->type);
    __gg__abort("__gg__compare_2() couldn't do the comparison");
    }

  // Normalize negative and positive to just -1 and +1
  if( retval < 0 )
    {
    retval = -1;
    }
  else if( retval > 0)
    {
    retval = 1;
    }
  return retval;
  }

extern "C"
int
__gg__compare(struct cblc_field_t *left,
              size_t               left_offset,
              size_t               left_length,
              size_t               left_flags,
              struct cblc_field_t *right,
              size_t               right_offset,
              size_t               right_length,
              size_t               right_flags,
              int second_time_through )
  {
  int retval;
  left_length  = left_length  ? left_length  : left->capacity;
  right_length = right_length ? right_length : right->capacity;
  retval = __gg__compare_2( left,
                            left->data + left_offset,
                            left_length,
                            left->attr,
                            left_flags,
                            right,
                            right->data + right_offset,
                            right_length,
                            right->attr,
                            right_flags,
                            second_time_through);
  return retval;
  }

extern "C"
void
__gg__double_to_target(cblc_field_t *tgt, double tgt_value,  cbl_round_t rounded)
  {
  int tgt_rdigits = 0;

  if( tgt->attr & intermediate_e )
    {
    // We are calculating an intermediate result.  We want to keep as
    // much of the fractional part as we can.  We are using a double,
    // which can hold almost 16 digits.  So, keep multiplying non-zero
    // value by 10 until it's that big.  Limit the number of
    // multiplies, in case we were given a ridiculously tiny number to
    // begin with:
    const double digits = 1E15;

    if( tgt_value )
      {
      while( tgt_value > -digits && tgt_value < digits && tgt_rdigits < 15 )
        {
        tgt_value *= 10.0;
        tgt_rdigits += 1;
        }
      }
    // Alter the run-time target's rdigits
    tgt->rdigits = tgt_rdigits;
    }
  else
    {
    // We want a specific number of rdigits.  We will multiply by
    // 10^(tgt->rdigits + 1), to allow for the possibility of rounding:
    tgt_rdigits = tgt->rdigits+1;
    for(int i=0; i<tgt_rdigits; i++)
      {
      tgt_value *= 10.0;
      }
    }

  __gg__int128_to_field(tgt,
                        tgt_value,
                        tgt_rdigits,
                        rounded,
                        NULL);
  }

struct for_sort_table
  {
  size_t               nkeys;
  cblc_field_t       **keys;
  size_t              *ascending;
  unsigned char       *contents;
  size_t              *offsets;
  size_t               base;
  };

static for_sort_table sorter;

static int
compare_two_records(unsigned char *range1,
                    unsigned char *range2)
  {
  int retval = 0;

  for(size_t i=0; i<sorter.nkeys; i++)
    {
    // Pick up the basic information about the current key:
    cblc_field_t field1;
    cblc_field_t field2;

    memcpy(&field1, sorter.keys[i], sizeof(cblc_field_t));
    memcpy(&field2, sorter.keys[i], sizeof(cblc_field_t));

    // Establish the locations inside the contents buffer
    field1.data = range1
                  + field1.offset
                  - sorter.base;
    field2.data = range2
                  + field2.offset
                  - sorter.base;
    field1.encoding = field2.encoding = encoding_for_sort;

    // We handle descending by swapping the data sources:
    if( !sorter.ascending[i] )
      {
      std::swap(field1.data, field2.data);
      }

    retval = __gg__compare( &field1,
                            0,
                            field1.capacity,
                            0,
                            &field2,
                            0,
                            field2.capacity,
                            0,
                            0 );
    if( retval != 0 )
      {
      break;
      }
    }

  return retval;
  }

static int
compare_for_sort_table(const size_t e1, const size_t e2)
  {
  int retval = 0;
  // Pick up the offsets to the two records:
  size_t offset1 = e1;
  size_t offset2 = e2;

  offset1 += sizeof(size_t);

  offset2 += sizeof(size_t);

  retval = compare_two_records(sorter.contents + offset1,
                               sorter.contents + offset2);

  if(retval == 0)
    {
    // Create a stable sort by using the original offset as a way of breaking
    // ties:
    retval = e1 - e2;
    }

  return retval < 0;
  }

static void
sort_contents(unsigned char       *contents,
              std::vector<size_t> &offsets,
              size_t               key_base,
              size_t               nkeys,
              cblc_field_t       **keys,
              size_t              *ascending,
              int                /*duplicates*/)
  {
  sorter.contents  = contents;
  sorter.offsets   = offsets.data();
  sorter.base      = key_base,
  sorter.nkeys     = nkeys;
  sorter.keys      = keys;
  sorter.ascending = ascending;

  std::sort(offsets.begin(), offsets.end(), compare_for_sort_table);
  }

extern "C"
void
__gg__sort_table( const cblc_field_t    *table,
                  size_t                 table_o,
                  size_t                 depending_on,
                  size_t                 nkeys,
                  cblc_field_t         **keys,
                  size_t                *ascending,
                  int                    duplicates )
  {
  size_t buffer_size = 128;
  unsigned char *contents = static_cast<unsigned char *>(malloc(buffer_size));
  size_t offset = 0;
  std::vector<size_t>offsets;
  size_t record_size = table->capacity;
  unsigned char *next_record = table->data + table_o;

  // Convert the table to our normalized form
  for(size_t i=0; i<depending_on; i++)
    {
    while( offset + sizeof(size_t) + record_size > buffer_size )
      {
      buffer_size *= 2;
      contents = static_cast<unsigned char *>(realloc(contents, buffer_size));
      }
    offsets.push_back(offset);
    memcpy(contents+offset, &record_size, sizeof(size_t));
    offset += sizeof(size_t);
    memcpy(contents+offset, next_record, record_size);
    offset      += record_size;
    next_record += record_size;
    }

  encoding_for_sort = table->encoding;

  // Sort it
  sort_contents(contents,
                offsets,
                table->offset,
                nkeys,
                keys,
                ascending,
                duplicates);

  // Put the sorted contents back
  next_record = table->data + table_o;
  for(size_t i=0; i<depending_on; i++)
    {
    offset = offsets[i];
    offset += sizeof(size_t);
    memcpy(next_record, contents+offset, record_size);
    next_record += record_size;
    }

  free(contents);
  }

static char *
as_initial(char *initial)
  {
  if( (size_t)initial & ~0xF )
    {
    return initial;
    }
  else
    {
    static char empty_string[] = "";
    return empty_string;
    }
  }

static const int DEFAULT_BYTE_MASK = 0x00000000FF;
static const int NSUBSCRIPT_MASK   = 0x0000000F00;
static const int NSUBSCRIPT_SHIFT  =            8;
static const int DEFAULTBYTE_BIT   = 0x0000001000;
static const int EXPLICIT_BIT      = 0x0000002000;
static const int REDEFINED_BIT     = 0x0000004000;
static const int JUST_ONCE_BIT     = 0x0000008000;

static
void
init_var_both(cblc_field_t  *var,
              unsigned char *qual_data,
              int            flag_bits)
  {
  if( flag_bits & JUST_ONCE_BIT && var->attr & initialized_e )
    {
    return;
    }

  bool is_redefined = !!(flag_bits & REDEFINED_BIT);
  bool explicitly   = !!(flag_bits & EXPLICIT_BIT);
  bool defaultbyte_in_play = !!(flag_bits & DEFAULTBYTE_BIT);
  char defaultbyte  = flag_bits & DEFAULT_BYTE_MASK;
  unsigned int nsubscripts = (flag_bits & NSUBSCRIPT_MASK) >> NSUBSCRIPT_SHIFT;

  if(    var->data == NULL
      && var->attr & (intermediate_e)
      && var->type != FldLiteralA
      && var->type != FldLiteralN )
    {
    var->data = static_cast<unsigned char *>(malloc(var->capacity));
    }

  // Set the "initialized" bit, which is tested in parser_symbol_add to make
  // sure this code gets executed only once.
  //fprintf(stderr, "__gg__initialize_variable %s setting initialize_e\n", var->name);
  var->attr |= initialized_e;

  // We need to make sure that the program_states vector has at least one
  // entry in it.  This happens when we are the very first PROGRAM-ID called
  // in this module.

  // When there is no DATA DIVISION, program_states will be empty the first time
  // we arrive here.
  if( program_states.empty() )
    {
    initialize_program_state();
    }

  const char *local_initial = as_initial(var->initial);

  // Next order of business: When the variable was allocated in
  // parser_symbol_add(), only LEVEL 01 variables had memory allocated.  All
  // child variables were given NULL data pointers.  It is at this point that
  // we apply offsets:

  cblc_field_t *parent = var->parent;
  if( !var->data )
    {

    // We don't have any data.  If our immediate parent does have data, then
    // we can calculate our data+offset from his data+offset:

    if( parent && parent->data )
      {
      var->data = parent->data - parent->offset + var->offset;
      }
    }

  if( !var->data )
    {
    // This can happen with BASED variables before they are allocated
    return;
    }

  if( !(var->attr & based_e) && (var->attr & external_e) )
    {
    // These types can't be initialized
    return;
    }

  // There are times, for example, when we are table with OCCURS, that we
  // look like a variable with no initial, and we might be tempted to set our
  // memory to the default.  But if a parent has been initialized, we must not
  // touch our memory:
  bool a_parent_initialized = false;
  if( !explicitly )
    {
    while(parent)
      {
      if( strlen(as_initial(parent->initial)) )
        {
        a_parent_initialized = true;
        }
      if(     parent->level == LEVEL01
          ||  parent->level == LEVEL77)
        {
        break;
        }
      parent = parent->parent;    // I can't help it.  This just tickles me.
      }
    }

  if( is_redefined || a_parent_initialized || var->level == LEVEL66 || var->level == LEVEL88)
    {
    // Don't initialize variables that have the REDEFINES clause.  Many things
    // in COBOL programs don't work if you do, in particular the initialization
    // of tables.

    // Likewise, don't initialize variables with an OCCURS clause.  To do so
    // means that we will likely clobber the values in the flat data item we
    // effectively redefine.
    return;
    }

  // This is a little brutish, but it is nonetheless simple, effective, and
  // not at all costly.  The numeric-edited variable type can have a
  // BLANK WHEN ZERO clause, which causes the storage to be set to spaces
  // when receiving a value of zero.  But according to the ISO/IEC 1989:2014
  // specification, section 13.18.63.3 sentence 8, initialization is not
  // affected by any BLANK WHEN ZERO clause.

  // So, I am going to rather ham-handedly turn that bit off here, and
  // restore it when initialization is done.

  // Save this for later
  int save_the_attribute = var->attr;

  // Turn off the bit in question
  var->attr &= ~blank_zero_e;

  size_t capacity = var->capacity ;

  size_t number_of_dimensions = 0;
  size_t limits[MAXIMUM_TABLE_DIMENSIONS];
  size_t capacities[MAXIMUM_TABLE_DIMENSIONS];
  size_t dimension[MAXIMUM_TABLE_DIMENSIONS+1];

  bool there_can_be_more = nsubscripts == 0;
  if( nsubscripts == 0 )
    {
    cblc_field_t *family_tree = var;
    while(family_tree && number_of_dimensions < MAXIMUM_TABLE_DIMENSIONS)
      {
      if( family_tree->occurs_upper )
        {
        limits[number_of_dimensions] = family_tree->occurs_upper;
        capacities[number_of_dimensions] = family_tree->capacity;
        dimension[number_of_dimensions] = 0;
        number_of_dimensions += 1;
        }

      family_tree = family_tree->parent;
      }

    switch( var->type )
      {
      case FldIndex:
      case FldGroup:
      case FldClass:
        there_can_be_more = false;
        break;
      default:
        break;
      }
    }

  // We need to save the location in case we start changing the location
  // to handle initializing table elements:
  unsigned char *save_the_location = var->data;
  bool there_is_more = false;
  unsigned char *outer_location;
  if( nsubscripts )
    {
    outer_location = qual_data;
    }
  else
    {
    outer_location = var->data;
    }
  do
    {
    var->data = outer_location;
    switch( var->type )
      {
      case FldGroup:
        {
        if( var->initial )
          {
          memcpy(outer_location, var->initial, var->capacity);
          }
        break;
        }

      case FldAlphanumeric:
      case FldAlphaEdited:
      case FldNumericEdited:
      case FldLiteralA:
        {
        if( var->initial )
          {
          memcpy(outer_location, var->initial, var->capacity);
          }
        else
          {
          charmap_t *charmap = __gg__get_charmap(var->encoding);
          if( !defaultbyte_in_play )
            {
            cbl_char_t initialization_character = ascii_space;
            if( var->attr & linkage_e && __gg__local_init != NOT_A_CHARACTER )
              {
              initialization_character = __gg__local_init;
              }
            if( !(var->attr & linkage_e) && __gg__working_init != NOT_A_CHARACTER )
              {
              initialization_character = __gg__working_init;
              }
            charmap->memset(outer_location,
                            charmap->mapped_character(initialization_character),
                            capacity );
            }
          else
            {
            charmap->memset(outer_location,
                            defaultbyte,
                            capacity );
            }
          }
        break;
        }

      case FldNumericDisplay:
        {
        // Any initialization values were converted to single-byte-coding in
        // the right codeset during parser_symbol_add()
        if( var->initial )
          {
          memcpy(outer_location, var->initial, var->capacity);
          }
        else
          {
          cbl_char_t init_zero = ascii_zero;
          cbl_char_t init_plus = ascii_plus;

          charmap_t *charmap = __gg__get_charmap(var->encoding);
          if( !defaultbyte_in_play )
            {
            charmap->memset( outer_location,
                             charmap->mapped_character(init_zero),
                             capacity );
            if( (var->attr & signable_e) && (var->attr & separate_e) )
              {
              if( var->attr & leading_e )
                {
                charmap->putch(charmap->mapped_character(init_plus),
                               outer_location,
                               size_t(0));
                }
              else
                {
                charmap->putch(charmap->mapped_character(init_plus),
                               outer_location,
                               var->capacity-charmap->stride());
                }
              }
            }
          else
            {
            charmap->memset(outer_location,
                            defaultbyte,
                            capacity );
            }
          }
        break;
        }

      case FldNumericBinary:
      case FldPacked:
      case FldNumericBin5:
      case FldIndex:
      case FldFloat:
        {
        // During  parser_symbol_add(), the original text initial value was turned
        // into the appropriate binary value.
        if( var->initial )
          {
          memcpy(outer_location, var->initial, var->capacity);
          }
        else
          {
          if( !defaultbyte_in_play )
            {
            memset(  outer_location,
                     0,
                     capacity );
            }
         else
            {
            memset(  outer_location,
                     defaultbyte,
                     capacity );
            }
          }
        break;
        }

      case FldLiteralN:
        break;

      case FldClass:
        // Do nothing for class
        break;

      case FldPointer:
        memset(var->data, 0, var->capacity);
        break;

      default:
        fprintf(stderr, "###### %10s in %s:%d\n", __func__, __FILE__, __LINE__ );
        fprintf(stderr, "###### You got yourself a new CVT_type %d for variable %s (%s)\n",
                var->type,
                var->name,
                local_initial);
        __gg__abort("Unknown variable type");
      }

    char *location = reinterpret_cast<char *>(save_the_location);

    there_is_more = false;
    size_t i=0;
    // Roll up through the dimensions like an odometer.
    for(i=0; i<number_of_dimensions; i++)
      {
      if( ++dimension[i] < limits[i] )
        {
        break;
        }
      dimension[i] = 0;
      }
    if( i < number_of_dimensions )
      {
      there_is_more = there_can_be_more;
      }
    if( there_is_more )
      {
      // Augment location by the size of each dimension:
      for(i=0; i<number_of_dimensions; i++)
        {
        location += dimension[i] * capacities[i];
        }
      }

    outer_location = reinterpret_cast<unsigned char *>(location);
    } while(there_is_more);

  var->data = save_the_location;

  // See the comment up above about suppressing and restoring
  // BLANK WHEN ZERO during initialization.
  var->attr |= (save_the_attribute&blank_zero_e);
  }

extern "C"
void
__gg__initialize_variable(cblc_field_t *var,
                          size_t        offset,
                          int           flag_bits)
  {
  init_var_both(  var,
                  var->data + offset,
                  flag_bits);
  }

extern "C"
void
__gg__initialize_variable_clean(cblc_field_t *var, int flag_bits)
  {
  init_var_both(  var,
                  var->data,
                  flag_bits);
  }

static void
alpha_to_alpha_move_from_location(cblc_field_t *field,
                                  size_t dest_offset,
                                  size_t dest_length,
                                  const char * source_location,
                                  size_t       source_length,
                                  bool         move_all)
  {
  // This is a helper function, called when it is known that both source
  // and dest are alphanumeric.

  // It is also required that at this point the source_location data be in the
  // same encoding as field->encoding.
  dest_length = dest_length ? dest_length : field->capacity;

  char *to         = reinterpret_cast<char *>(field->data + dest_offset);
  const char *from = source_location;
  size_t count     = std::min(dest_length, source_length);

  if( source_length >= dest_length )
    {
    // We have more source characters than places to put them
    if( field->attr & rjust_e )
      {
      // Destination is right-justified, so we
      // discard the leading source characters:
      memmove(to,
              from + (source_length - count),
              count);
      }
    else
      {
      // Destination is right-justified, so we
      // discard the trailing source characters:
      memmove(to,
              from,
              count);
      }
    }
  else
    {
    // We have too few source characters to fill the destination.
    if( field->attr & rjust_e )
      {
      // The destination is right-justified
      if( move_all )
        {
        // This does "BOBBO"
        size_t isource = 0;
        for(size_t i=0; i<dest_length; i++)
          {
          to[i] = from[isource++];
          if( isource >= source_length )
            {
            isource = 0;
            }
          }
        }
      else
        {
        // The destination is right-justified, and the source is an
        // ordinary string too short to fill it.  So, we space-fill
        // the leading characters.
        // We do the move first, in case this is an overlapping move
        // involving characters that will be space-filled
        charmap_t *charmap = __gg__get_charmap(field->encoding);
        memmove(to + (dest_length-count),
                from,
                count);
        charmap->memset(to,
                        charmap->mapped_character(ascii_space),
                        dest_length-count);
        }
      }
    else
      {
      // The source is smaller than the destination
      // The destination is left-justified
      if( move_all )
        {
        // and the source is move_all.  We will repeat the input until
        // it fills the output, starting from the left side.
        size_t isource = 0;
        for(size_t i=0; i<dest_length; i++)
          {
          to[i] = from[isource++];
          if( isource >= source_length )
            {
            isource = 0;
            }
          }
        }
      else
        {
        // The destination is right-justified, and the source is an
        // ordinary string too short to fill it.  So, we space-fill
        // the trailing characters.
        // We do the move first, in case this is an overlapping move
        // involving characters that will be space-filled
        charmap_t *charmap = __gg__get_charmap(field->encoding);
        memmove(to,
                from,
                count);
        charmap->memset(to + count,
                        charmap->mapped_character(ascii_space),
                        dest_length-count);
        }
      }
    }
  }

extern "C"
void *
__gg__memdup(const void *p, size_t size)
  {
  void *retval = nullptr;
  if(size)
    {
    retval = malloc(size);
    massert(retval);
    memcpy(retval, p, size);
    }
  return retval;
  }

static void
alpha_to_alpha_move(cblc_field_t *dest,
                    size_t dest_offset,
                    size_t dest_size,
              const cblc_field_t *source,
                    size_t source_offset,
                    size_t source_size,
                    bool source_move_all)
  {
  const char *source_location
                      = reinterpret_cast<char *>(source->data + source_offset);
  size_t outlength;
  if(dest->encoding == source->encoding)
    {
    // we don't need to bother calling __gg__iconverter
    outlength = source_size;
    }
  else
    {
    // Before calling the mover, we need to convert the source to the
    // destination encoding:
    source_location = __gg__iconverter( source->encoding,
                                        dest->encoding,
                                        source_location,
                                        source_size,
                                        &outlength);
    }
  char *duped = static_cast<char *>(__gg__memdup(source_location, outlength));
  alpha_to_alpha_move_from_location(dest,
                                    dest_offset,
                                    dest_size,
                                    duped,
                                    outlength,
                                    source_move_all);
  free(duped);
  }

extern "C"
void
__gg__psz_to_alpha_move(  cblc_field_t *field,
                          size_t offset,
                          size_t length,
                          const char *source,
                          size_t source_length )
  {
  alpha_to_alpha_move_from_location(  field,
                                      offset,
                                      length,
                                      source,
                                      source_length,
                                      false);
  }

extern "C"
int
__gg__move( cblc_field_t        *fdest,
            size_t               dest_offset,
            size_t               dest_size,
            cblc_field_t        *fsource,
            size_t               source_offset,
            size_t               source_size,
            int                  source_flags,
            cbl_round_t          rounded )
  {
  int size_error = 0; // This is the return value

  __int128 value;
  int rdigits;

  charmap_t *charmap = __gg__get_charmap(fdest->encoding);
  int stride = charmap->stride();
  cbl_figconst_t source_figconst =
                        (cbl_figconst_t)(fsource->attr & FIGCONST_MASK);
  int special_char = 0; // quiets cppcheck
  if( source_figconst == low_value_e )
    {
    special_char = charmap->low_value_character();
    }
  else if( source_figconst == high_value_e )
    {
    special_char = charmap->high_value_character();
    }
  else if( source_figconst == quote_value_e )
    {
    special_char = charmap->quote_character();
    }
  else if( source_figconst == space_value_e )
    {
    special_char = charmap->mapped_character(ascii_space);
    }
  else if( source_figconst == zero_value_e )
    {
    special_char = charmap->mapped_character(ascii_zero);
    }

  cbl_field_type_t dest_type   = (cbl_field_type_t)fdest->type;
  cbl_field_type_t source_type = (cbl_field_type_t)fsource->type;

  if( var_is_refmod(fdest) )
    {
    // one or both are refmods,
    dest_type   = FldAlphanumeric;
    if( source_figconst == normal_value_e )
      {
      source_type = FldAlphanumeric;
      }
    }

  if( (   source_figconst == low_value_e
      ||  source_figconst == space_value_e
      ||  source_figconst == quote_value_e
      ||  source_figconst == high_value_e )
      &&
       (    fdest->type == FldNumericBinary
        ||  fdest->type == FldPacked
        ||  fdest->type == FldNumericBin5
        ||  fdest->type == FldNumericDisplay
        ||  fdest->type == FldFloat )
        )
    {
    // Regardless of what you see below, as time went on it became clear that
    // high-value and low-value required special processing in order to cope
    // with code.  Or, at least, to cope with legacy tests.

    // The ISO 2014 specification has this to say about the moving of figurative
    // constants to numerics:

    // 14.9.24.3, paragraph 7)

    /*  NOTE: MOVE of the figurative constant QUOTE or QUOTES to a numeric data
     *  item is an obsolete feature and is to be removed from the next edition
     *  of standard COBOL. MOVE of figurative constants that are not numeric,
     *  other than QUOTE or QUOTES, to a numeric item is an archaic feature of
     *  standard COBOL and its use should be avoided
     */
    charmap->memset( fdest->data + dest_offset,
                          special_char,
                          dest_size);
    }
  else
    {
    size_t min_length;
    bool moved = true;
    switch( dest_type )
      {
      case FldGroup:
        switch( source_type )
          {
          // For all other types, we just do a straight byte-for-byte move
          case FldAlphanumeric:
          case FldNumericEdited:
          case FldAlphaEdited:
          case FldNumericDisplay:
          case FldNumericBinary:
          case FldPacked:
          case FldNumericBin5:
          case FldGroup:
          case FldLiteralA:
            {
            // This is a little bold, but non-alphabetics will never
            // have the rjust_e or MOVE_ALL bits on, so it's safe
            // enough.

            alpha_to_alpha_move(fdest,
                                dest_offset,
                                dest_size,
                                fsource,
                                source_offset,
                                source_size,
                                !!(source_flags & REFER_T_MOVE_ALL));
            break;
            }

          default:
            moved = false;
            break;
          }

        break;

      case FldAlphanumeric:
        {
        switch( source_type )
          {
          case FldGroup:
            alpha_to_alpha_move(fdest,
                                dest_offset,
                                dest_size,
                                fsource,
                                source_offset,
                                source_size,
                                !!(source_flags & REFER_T_MOVE_ALL));
            break;

          case FldAlphanumeric:
          case FldNumericEdited:
          case FldAlphaEdited:
          case FldLiteralA:
            // This is an ordinary alpha-to-alpha move:
            if( source_figconst )
              {
              charmap->memset( fdest->data + dest_offset,
                                    special_char,
                                    dest_size);
              }
            else
              {
              alpha_to_alpha_move(fdest,
                                  dest_offset,
                                  dest_size,
                                  fsource,
                                  source_offset,
                                  source_size,
                                  !!(source_flags & REFER_T_MOVE_ALL));
              }
            break;

          case FldNumericDisplay:
            // We are moving a FldNumericDisplay to an alphanumeric:
            if( fsource->rdigits > 0 )
              {
              fprintf(stderr, "%s() %s:%d -- It isn't legal to move a"
                      " non-integer NumericDisplay to an"
                      " alphanumeric\n",
                      __func__, __FILE__, __LINE__);
              fprintf(    stderr,
                          "%s to %s\n",
                          fsource->name,
                          fdest->name);
              abort();
              }
            else
              {
              // We are moving a integer NumericDisplay to an
              // alphanumeric.  We ignore any sign bit, and just
              // move the characters:

              size_t source_digits
                = fsource->digits + ( fsource->rdigits < 0
                                      ? -fsource->rdigits : 0) ;

              // Pick up the absolute value of the source
              value = __gg__binary_value_from_qualified_field(&rdigits,
                                                              fsource,
                                                              source_offset,
                                                              source_size);

              char ach[128];

              // Convert it to the full complement of digits available
              // from the source...but no more
              __gg__binary_to_string_encoded(ach,
                                             source_digits,
                                             value,
                                             fdest->encoding);

              if( !(fdest->attr & rjust_e) )
                {
                min_length = std::min(  source_digits*stride,
                                        dest_size);
                memmove(fdest->data + dest_offset, ach, min_length);
                if( min_length < dest_size )
                  {
                  // min_length is smaller than dest_length, so we
                  // have to space-fill the excess bytes in the
                  // destination:
                  charmap->memset(fdest->data + dest_offset + min_length ,
                                  charmap->mapped_character(ascii_space),
                                  dest_size - min_length );
                  }
                }
              else
                {
                // Destination is right-justified, so things are
                // slightly more complex
                if( source_digits*stride >= dest_size )
                  {
                  // We need to truncate the source data on the
                  // left:
                  memmove(
                    fdest->data + dest_offset,
                    ach + (source_digits*stride - dest_size),
                    dest_size );
                  }
                else
                  {
                  // We need to move the shorty source string to
                  // the right side of the destination, and space-fill
                  //  the prefix:
                  memmove(fdest->data
                            + dest_offset + (dest_size - source_digits*stride),
                          ach,
                          source_digits*stride );
                  charmap->memset( fdest->data + dest_offset,
                                  charmap->mapped_character(ascii_space),
                                  dest_size - source_digits*stride);
                  }
                }
              }
            break;

          case FldNumericBinary:
          case FldPacked:
          case FldNumericBin5:
          case FldLiteralN:
            // We are moving a binary number to an alphanumeric:
            if( fsource->rdigits > 0 )
              {
              fprintf(stderr, "%s() %s:%d -- It isn't legal to move a"
                      " non-integer binary number to an"
                      " alphanumeric\n",
                      __func__, __FILE__, __LINE__);
              fprintf(stderr, "%s to %s\n", fsource->name, fdest->name);
              abort();
              }
            else
              {
              char ach[128];

              // Turn the integer source into a value:
              value = __gg__binary_value_from_qualified_field(&rdigits,
                                                              fsource,
                                                              source_offset,
                                                              source_size);

              source_size   = fsource->digits;

              // Turn the integer value into a string:
              __gg__binary_to_string_encoded(ach,
                                             source_size,
                                             value,
                                             fdest->encoding);

              char *pach = ach;

              // When source is a temporary variable, it was set to
              // a large number of digits, which will give the wrong
              // result.  So, we will make like the US Marine Corp,
              // and improvise, adapt, and overcome.

              // Specifically, we'll move pach to point to the first
              // character that isn't zero.

              if( fsource->attr & intermediate_e )
                {
                while(source_size > 1)  // This ensures we leave one '0'
                  {
                  cbl_char_t ch = charmap->getch(pach, stride);
                  if( ch == '\0' )
                    {
                    break;
                    }
                  ch = charmap->getch(pach, size_t(0));
                  if( ch != charmap->mapped_character(ascii_0))
                    {
                    break;
                    }
                  pach += stride;
                  source_size -= 1;
                  }
                }

              if( !(fdest->attr & rjust_e) )
                {
                min_length = std::min(  source_size*stride,
                                        dest_size);
                memmove(fdest->data+dest_offset, pach, min_length);
                if( min_length < dest_size )
                  {
                  // min_length is smaller than dest_length, so we have to
                  // space-fill the excess bytes in the destination:
                  charmap->memset( fdest->data+dest_offset + min_length,
                          charmap->mapped_character(ascii_space),
                          dest_size - min_length );
                  }
                }
              else
                {
                // Destination is right-justified, so things are slightly more
                // complex
                if( source_size >= dest_size )
                  {
                  // We need to truncate the source data on the left:
                  memmove(fdest->data+dest_offset,
                          pach + (source_size - dest_size),
                          dest_size );
                  }
                else
                  {
                  // We need to move the shorty source string to the
                  // right side of the destination, and space-fill the prefix:
                  memmove(fdest->data+dest_offset +
                              (dest_size - source_size*stride),
                          pach,
                          source_size*stride );
                  charmap->memset(fdest->data+dest_offset,
                         charmap->mapped_character(ascii_space),
                         (dest_size - source_size*stride));
                  }
                }
              }
            break;

          case FldIndex:
            {
            char ach[128];

            // Turn the integer source into a value:
            value = __gg__binary_value_from_qualified_field(&rdigits,
                                                            fsource,
                                                            source_offset,
                                                            source_size);
            // Turn the integer value into a string:
            __gg__binary_to_string_encoded(ach,
                                           source_size,
                                           value,
                                           fdest->encoding);
            char *pach = ach;

            if( !(fdest->attr & rjust_e) )
              {
              min_length = std::min(  source_size,
                                      dest_size);
              memmove(fdest->data+dest_offset, pach, min_length);
              if( min_length < dest_size )
                {
                // min_length is smaller than dest_length, so we have to
                // space-fill the excess bytes in the destination:
                charmap->memset( fdest->data+dest_offset + min_length,
                        charmap->mapped_character(ascii_space),
                        dest_size - min_length );
                }
              }
            else
              {
              // Destination is right-justified, so things are slightly more complex
              if( source_size >= dest_size )
                {
                // We need to truncate the source data on the left:
                memmove(fdest->data+dest_offset,
                        pach + (source_size - dest_size),
                        dest_size );
                }
              else
                {
                // We need to move the shorty source string to the
                // right side of the destination, and space-fill the prefix:
                memmove(fdest->data+dest_offset + (dest_size - source_size),
                        pach,
                        source_size );
                memset(fdest->data+dest_offset,
                       charmap->mapped_character(ascii_space),
                       (dest_size - source_size));
                }
              }
            }
            break;

          default:
            moved = false;
            break;
          }
        break;
        }

      case FldNumericBinary:
        {
        switch( source_type )
          {
          case FldGroup:
            min_length = std::min(source_size, dest_size);
            memmove(fdest->data+dest_offset, fsource->data+source_offset, min_length);
            if( min_length < dest_size )
              {
              // min_length is smaller than dest_length, so we have to
              // space-fill the excess bytes in the destination:
              memset( fdest->data+dest_offset + min_length,
                      charmap->mapped_character(ascii_space),
                      dest_size - min_length );
              }
            fdest->attr &= ~FIGCONST_MASK;
            break;

          case FldAlphanumeric:
          case FldNumericDisplay:
          case FldNumericEdited:
          case FldNumericBinary:
          case FldPacked:
          case FldNumericBin5:
          case FldIndex:
          case FldLiteralN:
            {
            // We are moving a number to a number:
            value = __gg__binary_value_from_qualified_field(&rdigits,
                                                            fsource,
                                                            source_offset,
                                                            source_size);

            if( truncation_mode == trunc_std_e )
              {
              // We need to adjust the value to have the rdigits of the
              // the destination:

              int scaler = rdigits - fdest->rdigits;
              if( scaler > 0 )
                {
                value /= __gg__power_of_ten(scaler);
                rdigits -= scaler;
                }
              else if( scaler < 0 )
                {
                value *= __gg__power_of_ten(-scaler);
                rdigits -= scaler;
                }
              if( value < 0 )
                {
                value = -value;
                value %= __gg__power_of_ten(fdest->digits);
                value = -value;
                }
              else
                {
                value %= __gg__power_of_ten(fdest->digits);
                }
              }

            __gg__int128_to_qualified_field(
                                  fdest,
                                  dest_offset,
                                  dest_size,
                                  value,
                                  rdigits,
                                  rounded,
                                  &size_error );
            break;
            }

          case FldFloat:
            {
            rdigits = get_scaled_rdigits(fdest);
            bool negative = false;
            __int128 value128 = 0;
            switch(fsource->capacity)
              {
              case 4:
                {
                _Float32 val = *PTRCAST(_Float32, fsource->data+source_offset);
                if(val < 0)
                  {
                  negative = true;
                  val = -val;
                  }
                val *= static_cast<_Float32>(__gg__power_of_ten(rdigits));
                value128 = (__int128)val;
                break;
                }
              case 8:
                {
                _Float64 val = *PTRCAST(_Float64, fsource->data+source_offset);
                if(val < 0)
                  {
                  negative = true;
                  val = -val;
                  }
                val *= (_Float32)__gg__power_of_ten(rdigits);
                value128 = (__int128)val;
                break;
                }
              case 16:
                {
                //_Float128 val = *(_Float128 *)(fsource->data+source_offset);
                GCOB_FP128 val;
                memcpy(&val, fsource->data+source_offset, 16);
                if(val < 0)
                  {
                  negative = true;
                  val = -val;
                  }
                val *= (_Float32)__gg__power_of_ten(rdigits);
                value128 = (__int128)val;
                break;
                }
              }
            if( negative )
              {
              value128 = -value128;
              }
            __gg__int128_to_qualified_field(
                                  fdest,
                                  dest_offset,
                                  dest_size,
                                  value128,
                                  rdigits,
                                  rounded,
                                  &size_error );
            break;
            }

          default:
            {
            moved = false;
            break;
            }
          }
        break;
        }

      case FldNumericDisplay:
      case FldNumericEdited:
      case FldNumericBin5:
      case FldPacked:
      case FldIndex:
        {
        // Bin5 and Index are treated with no truncation, as if they were
        // trunc_bin_e.  The other types aren't subject to truncation.
        switch( source_type )
          {
          case FldGroup:
            min_length = std::min(source_size, dest_size);
            memmove(fdest->data+dest_offset, fsource->data+source_offset, min_length);
            if( min_length < dest_size )
              {
              // min_length is smaller than dest_length, so we have to
              // space-fill the excess bytes in the destination:
              memset( fdest->data+dest_offset + min_length,
                      charmap->mapped_character(ascii_space),
                      dest_size - min_length );
              }
            break;

          case FldAlphanumeric:
          case FldNumericDisplay:
          case FldNumericEdited:
          case FldNumericBinary:
          case FldPacked:
          case FldNumericBin5:
          case FldIndex:
          case FldLiteralN:
            {
            // We are moving a number to a number:
            value = __gg__binary_value_from_qualified_field(&rdigits,
                                                            fsource,
                                                            source_offset,
                                                            source_size);
            __gg__int128_to_qualified_field(
                                       fdest,
                                       dest_offset,
                                       dest_size,
                                       value,
                                       rdigits,
                                       rounded,
                                       &size_error );
            break;
            }

          case FldFloat:
            {
            // We are converted a floating-point value fixed-point

            rdigits = get_scaled_rdigits(fdest);
            GCOB_FP128 fp128=0;
            switch(fsource->capacity)
              {
              case 4:
                {
                fp128 = *reinterpret_cast<_Float32 *>(fsource->data+source_offset);
                break;
                }
              case 8:
                {
                fp128 = *reinterpret_cast<_Float64 *>(fsource->data+source_offset);
                break;
                }
              case 16:
                {
                // value = *(_Float128 *)(fsource->data+source_offset);
                memcpy(&fp128, fsource->data+source_offset, 16);
                break;
                }
              }
            __gg__float128_to_qualified_field(
                                    fdest,
                                    dest_offset,
                                    fp128,
                                    rounded,
                                    &size_error);
            break;
            }

          default:
            moved = false;
            break;
          }
        break;
        }

      case FldAlphaEdited:
        {
        switch( source_type )
          {
          case FldGroup:
            min_length = std::min(source_size, dest_size);
            memmove(fdest->data+dest_offset, fsource->data+source_offset, min_length);
            if( min_length < dest_size )
              {
              // min_length is smaller than dest_length, so we have to
              // space-fill the excess bytes in the destination:
              memset( fdest->data+dest_offset + min_length,
                      charmap->mapped_character(ascii_space),
                      dest_size - min_length );
              }
            break;

          case FldNumericDisplay:
            {
            int source_digits = fsource->digits
                              + (fsource->rdigits<0 ? -fsource->rdigits : 0) ;

            // Pick up the absolute value of the source
            value = __gg__binary_value_from_qualified_field(&rdigits,
                                                            fsource,
                                                            source_offset,
                                                            source_size);
            char ach[64];

            // Convert it to the full complement of digits available
            // from the source...but no more
            __gg__binary_to_string_encoded(ach,
                                           source_digits,
                                           value,
                                           fdest->encoding);
            // And move them into place:
            __gg__string_to_alpha_edited(
                          reinterpret_cast<char *>(fdest->data+dest_offset),
                          fdest->encoding,
                          ach,
                          source_digits,
                          fdest->picture);
            break;
            }

          default:
            {
            static size_t display_string_size = MINIMUM_ALLOCATION_SIZE;
            static char *display_string = static_cast<char *>(malloc(display_string_size));

            size_t display_string_length = dest_size;
            __gg__realloc_if_necessary( &display_string,
                                        &display_string_size,
                                        display_string_length);

            int fc_char = __gg__fc_char(fsource);
            if( fc_char != NOT_A_CHARACTER )
              {
              memset(display_string, fc_char, dest_size);
              __gg__convert_encoding_length(display_string,
                                            dest_size,
                                            fsource->encoding,
                                            fdest->encoding );
              }
            else
              {
              format_for_display_internal(
                              &display_string,
                              &display_string_size,
                              fsource,
                              reinterpret_cast<unsigned char *>
                                                 (fsource->data+source_offset),
                              source_size,
                              source_flags && REFER_T_ADDRESS_OF);
              display_string_length = strlen(display_string);
              }
            __gg__string_to_alpha_edited( reinterpret_cast<char *>
                                                     (fdest->data+dest_offset),
                                                      fdest->encoding,
                                                      display_string,
                                                      display_string_length,
                                                      fdest->picture);
            break;
            }
          }
        break;
        }

      case FldFloat:
        {
        switch( source_type )
          {
          case FldAlphanumeric:
          case FldGroup:
            {
            // Converting alphanumeric to float means first converting to
            // ascii:
            size_t charsout;
            const char *converted = __gg__iconverter(fsource->encoding,
                                                     DEFAULT_SOURCE_ENCODING,
                                    PTRCAST(char, fsource->data+source_offset),
                                                     source_size,
                                                     &charsout);
            char ach[256];
            size_t len = std::min(source_size, sizeof(ach)-1);
            memcpy(ach, converted, len);
            ach[len] = '\0';
            switch( fdest->capacity )
              {
              case 4:
                {
                *PTRCAST(float, fdest->data+dest_offset) = strtod(ach, NULL);
                break;
                }
              case 8:
                {
                *PTRCAST(double, fdest->data+dest_offset) = strtod(ach, NULL);
                break;
                }
              case 16:
                {
                GCOB_FP128 t = strtofp128(ach, NULL);
                memcpy(fdest->data+dest_offset, &t, 16);
                break;
                }
              }
            break;
            }
          default:
            moved = false;
            break;
          }
        break;
        }

      default:
        moved = false;
        break;
      }
    if( !moved )
      {
      fprintf(stderr, "%s() %s:%d -- We were unable to do a move from "
              "type %d to %d\n",
              __func__, __FILE__, __LINE__,
              fsource->type, fdest->type);
      abort();
      }
    }
  return size_error;
  }

extern "C"
int
__gg__move_literala(cblc_field_t *field,
                    size_t        field_offset,
                    size_t        field_size,
                    cbl_round_t   rounded_,
                    const char   *str,
                    size_t        strlen )
  {
  // It is required that the source 'str' be encoded the same as the
  // field-encoding.

  cbl_round_t rounded = static_cast<cbl_round_t>(rounded_ & ~REFER_ALL_BIT);
  bool move_all = !!(rounded_ & REFER_ALL_BIT);

  int size_error = 0; // This is the return value

  bool moved = true;

  __int128 value;
  int rdigits;

  cbl_field_type_t dest_type   = (cbl_field_type_t)field->type;
  if( var_is_refmod(field) )
    {
    dest_type   = FldAlphanumeric;
    }

  switch( dest_type )
    {
    case FldGroup:
    case FldAlphanumeric:
      {
      alpha_to_alpha_move_from_location(field,
                                        field_offset,
                                        field_size,
                                        str,
                                        strlen,
                                        move_all);
      break;
      }

    case FldNumericBinary:
      {
      value = __gg__dirty_to_binary(str,
                                    field->encoding,
                                    strlen,
                                    &rdigits );
      if( truncation_mode == trunc_std_e )
        {
        // We need to adjust the value to have the rdigits of the
        // the destination:

        int scaler = rdigits - field->rdigits;
        if( scaler > 0 )
          {
          value /= __gg__power_of_ten(scaler);
          rdigits -= scaler;
          }
        else if( scaler < 0 )
          {
          value *= __gg__power_of_ten(-scaler);
          rdigits -= scaler;
          }

        if( value < 0 )
          {
          value = -value;
          value %= __gg__power_of_ten(field->digits);
          value = -value;
          }
        else
          {
          value %= __gg__power_of_ten(field->digits);
          }
        }
      __gg__int128_to_qualified_field(
                            field,
                            field_offset,
                            field_size,
                            value,
                            rdigits,
                            rounded,
                            &size_error );
      break;
      }

    case FldNumericDisplay:
    case FldNumericEdited:
    case FldNumericBin5:
    case FldPacked:
    case FldIndex:
      // Bin5 and Index are treated with no truncation, as if they were
      // trunc_bin_e.  The other types aren't subject to truncation.
      // We are moving a number to a number:
      value = __gg__dirty_to_binary(str,
                                    field->encoding,
                                    strlen,
                                    &rdigits );
      __gg__int128_to_qualified_field(
                            field,
                            field_offset,
                            field_size,
                            value,
                            rdigits,
                            rounded,
                            &size_error );
      break;

    case FldAlphaEdited:
      {
      static size_t display_string_size = MINIMUM_ALLOCATION_SIZE;
      static char *display_string = static_cast<char *>(malloc(display_string_size));

      __gg__realloc_if_necessary( &display_string,
                                  &display_string_size,
                                  field_size);

      charmap_t *charmap = __gg__get_charmap(field->encoding);
      memset( display_string,
              charmap->mapped_character(ascii_space),
              display_string_size);
      size_t len = std::min(display_string_size, strlen);
      memcpy(display_string, str, len);
      __gg__string_to_alpha_edited(
                          reinterpret_cast<char *>(field->data+field_offset),
                          field->encoding,
                          display_string,
                          field_size,
                          field->picture);
      break;
      }

    case FldFloat:
      {
      char ach[256];
      size_t len = std::min(strlen, sizeof(ach)-1);
      memcpy(ach, str, len);
      ach[len] = '\0';
      switch( field->capacity )
        {
        case 4:
          {
          *PTRCAST(float, field->data+field_offset) = strtod(ach, NULL);
          break;
          }
        case 8:
          {
          *PTRCAST(double, field->data+field_offset) = strtod(ach, NULL);
          break;
          }
        case 16:
          {
          GCOB_FP128 t = strtofp128(ach, NULL);
          memcpy(field->data+field_offset, &t, 16);
          break;
          }
        }
      break;
      }

    default:
      moved = false;
      break;
    }

  if( !moved )
    {
    fprintf(stderr, "%s() %s:%d -- We were unable to do a move to "
            "type %d\n",
            __func__, __FILE__, __LINE__,
            field->type);
    abort();
    }

  return size_error;
  }

extern "C"
void
__gg__file_sort_ff_input(   cblc_file_t *workfile,
                            cblc_file_t *input)
  {
  // The name means "file-file input"

  // We are going to read records from input and write them to workfile.  These
  // files are already open.

  sv_suppress_eof_ec = true;
  for(;;)
    {
    // Read the data from the input file into its record_area
    __gg__file_read(input,
                    -1);
    if( input->io_status >= FhNotOkay )
      {
      break;
      }
    // We have the data we need.  Transmit it to workfile.
    int before_advancing = 0;
    if( workfile->org == file_line_sequential_e )
      {
      // we need a newline at the end of each line
      before_advancing = 1;
      }
    else if( workfile->org == file_sequential_e )
      {
      // We don't want any vertical movement
      before_advancing = -1;
      }

    size_t bytes_to_write = std::min( workfile->record_area_max,
                                      input->record_area_max );

    __gg__file_write(   workfile,
                        input->default_record->data,
                        bytes_to_write,
                        0,
                        before_advancing,
                        0); // non-random
    }
  sv_suppress_eof_ec = false;
  }

extern "C"
void
__gg__file_sort_ff_output(  cblc_file_t *output,
                            cblc_file_t *workfile)
  {
  // The name means "file-file output"

  // We read records from workfile and write them to the output file

  // Make sure workfile is positioned at the beginning
  __gg__file_reopen(workfile, 'r');

  sv_suppress_eof_ec = true;
  for(;;)
    {
    __gg__file_read(  workfile,
                      -1);
    if( workfile->io_status >= FhNotOkay )
      {
      break;
      }
    int advancing = -1; // Default to no vertical movement
    if( output->org == file_line_sequential_e )
      {
      advancing = 1;
      }

    __gg__file_write(   output,
                        workfile->default_record->data,
                        workfile->record_area_max,
                        0,
                        advancing,
                        0); // 1 would be is_random
    }
  sv_suppress_eof_ec = false;
  }

extern "C"
void
__gg__sort_workfile(cblc_file_t    *workfile,
                    size_t          nkeys,
                    cblc_field_t  **keys,
                    size_t         *ascending,
                    int             duplicates)
  {
  encoding_for_sort = workfile->encoding;

  // We are going to read the records of workfile into memory.  We keep offsets
  // into the memory buffer, and then we'll sort those offsets according to the
  // things they point to.

  // The workfile is open and positioned at zero when we arrive here.

  // Read the file into memory
  size_t buffer_size = 128;
  unsigned char *contents = static_cast<unsigned char *>(malloc(buffer_size));
  size_t offset = 0;
  std::vector<size_t>offsets;
  size_t bytes_read;
  size_t bytes_to_write;

  const charmap_t *charmap = __gg__get_charmap(workfile->encoding);

  sv_suppress_eof_ec = true;
  for(;;)
    {
    __gg__file_read(workfile,
                    -1);
    if( workfile->record_length )
      {
      int rdigits;
      // The record length is reported in character positions:
      bytes_read = charmap->stride() * (size_t) __gg__binary_value_from_field(
                                                  &rdigits,
                                                  workfile->record_length);
      }
    else
      {
      bytes_read = workfile->record_area_max;
      }
    if( workfile->io_status >= FhNotOkay )
      {
      break;
      }

    while( offset + sizeof(size_t) + bytes_read > buffer_size )
      {
      buffer_size *= 2;
      contents = static_cast<unsigned char *>(realloc(contents, buffer_size));
      }
    offsets.push_back(offset);

    // Copy over the record size:
    memcpy(contents+offset, &bytes_read, sizeof(size_t));
    offset += sizeof(size_t);

    // And the contents of the record
    memcpy(contents+offset, workfile->default_record->data, bytes_read);
    offset += bytes_read;
    }
  sv_suppress_eof_ec = false;

  sort_contents(contents,
                offsets,
                0,
                nkeys,
                keys,
                ascending,
                duplicates);

  // We now put the sorted data back out onto the disk:
  fclose(workfile->file_pointer);
  __gg__file_reopen(workfile, 'w');

  for(size_t i=0; i<offsets.size(); i++)
    {
    offset = offsets[i];
    memcpy(&bytes_to_write, contents+offset, sizeof(size_t));
    offset += sizeof(size_t);
    int advancing = -1;
    if( workfile->org == file_line_sequential_e )
      {
      advancing = 1;
      }
    if(    workfile->record_area_min != workfile->record_area_max
        && workfile->record_length )
      {
      // Set the number of bytes to write, remembering that record_length is
      // in characters, not bytes:
      __gg__int128_to_field(workfile->record_length,
                            bytes_to_write/charmap->stride(),
                            0,
                            truncation_e,
                            NULL);
      }
    __gg__file_write(   workfile,
                        contents+offset,
                        bytes_to_write,
                        0,
                        advancing,
                        0);  // 1 would be is_random
    }
  free(contents);
  }

extern "C"
void
__gg__merge_files( cblc_file_t   *workfile,
                   size_t         nkeys,
                   cblc_field_t **keys,
                   size_t        *ascending,
                   size_t         ninputs,
                   cblc_file_t  **inputs)
  {
  // Merge takes in N files that are already sorted.  It looks at the N records
  // at the top of the N files, and figures out who the winner is, and puts each
  // winner into workfile.  If it notices that any of the files are not in the
  // order specified by the keys it raises the EC-SORT-MERGE-SEQUENCE exception.

  // Is everybody ready?

  // Then we will begin.

  encoding_for_sort = workfile->encoding;

  sorter.nkeys     = nkeys;
  sorter.keys      = keys;
  sorter.ascending = ascending;

  // We need to prime the pump by reading one record from everybody
  size_t the_biggest = 0;
  for(size_t i=0; i<ninputs; i++)
    {
    the_biggest = std::max(the_biggest, inputs[i]->record_area_max);

    __gg__file_read(inputs[i],
                    -1);
    if( inputs[i]->io_status >= FhNotOkay )
      {
      inputs[i] = NULL;
      }
    }

  // For each input, either there is a good record in its record area, or else
  // inputs[i] for that file is NULL

  if( !the_biggest )
    {
    return;
    }

  unsigned char *prior_winner = static_cast<unsigned char *>(malloc(the_biggest));
  massert(prior_winner);
  *prior_winner = '\0';

  for(;;)
    {
    int winner = -1;
    for(int i=0; i<(int)ninputs; i++ )
      {
      if( !inputs[i] )
        {
        // This input has been exhausted
        continue;
        }
      if( winner == -1 )
        {
        // Establish the first file as the current winner
        winner = i;
        continue;
        }
      // We now compare inputs[i] to the current winner
      int ncompare = compare_two_records( inputs[i]->default_record->data,
                                          inputs[winner]->default_record->data);
      if( ncompare < 0 )
        {
        // We have a new winner
        winner = i;
        }
      }
    // We have scanned all the inputs, looking for the smallest of them.
    if( winner == -1 )
      {
      // We have exhausted all of the inputs, which means we are done.
      break;
      }
    if( *prior_winner )
      {
      // We need to compare the current winner to the prior winner
      int ncompare = compare_two_records( prior_winner,
                                          inputs[winner]->default_record->data);
      if( ncompare > 0 )
        {
        // The prior winner is bigger than the current winner, which means that
        // the input files were not in order.  This is a run-time error.
        exception_raise(ec_sort_merge_sequence_e);
        abort();
        }
      }
    // Establish winner as the prior_winner
    memcpy( prior_winner,
            inputs[winner]->default_record->data,
            inputs[winner]->record_area_max);
    // And send it to the workfile

    int before_advancing = -1;  // No vertical movement...
    if( workfile->org == file_line_sequential_e )
      {
      // we need a newline at the end of each line sequential line
      before_advancing = 1;
      }

    __gg__file_write(   workfile,
                        inputs[winner]->default_record->data,
                        inputs[winner]->record_area_max,
                        0,
                        before_advancing,
                        0);  // 1 means is_random


    // And now we need to replace the winner:

    __gg__file_read(inputs[winner],
                    -1);
    if( inputs[winner]->io_status >= FhNotOkay )
      {
      inputs[winner] = NULL;
      }
    }

  free(prior_winner);
  }

typedef std::vector<cbl_char_t>::const_iterator char_it_c ;
typedef std::vector<cbl_char_t>::iterator       char_it   ;

static const char *
funky_find( const char *piece,
            const char *piece_end,
            const char *whole,
            const char *whole_end )
  {
  const char *retval = NULL;

  size_t length_of_piece = piece_end - piece;
  if(length_of_piece == 0)
    {
    __gg__abort("funky_find() length_of_piece shouldn't be zero");
    }

  whole_end -= length_of_piece;

  while( whole <= whole_end )
    {
    if( memcmp( piece, whole, length_of_piece) == 0 )
      {
      retval = whole;
      break;
      }
    whole += 1;
    }
  return retval;
  }

static char_it_c
funky_find_wide( char_it_c needle,
                 char_it_c needle_end,    // Actually end+1
                 char_it_c haystack,
                 char_it_c haystack_end,  // Actually end+1
                 char_it_c notfound)
  {
  // We are looking for the needle in the haystack

  char_it_c retval = notfound;

  size_t length_of_piece = needle_end - needle;
  if(length_of_piece == 0)
    {
    __gg__abort("funky_find_wide() length_of_piece shouldn't be zero");
    }

  haystack_end -= length_of_piece;

  while( haystack <= haystack_end )
    {
    // Compare the memory at needle to the memory at haystack
    if( memcmp( &(*needle),
                &(*haystack),
                length_of_piece*sizeof(cbl_char_t)) == 0 )
      {
      // They are the same; return where needle was found
      retval = haystack;
      break;
      }
    // Not found; move to the next location in the haystach
    haystack += 1;
    }
  return retval;
  }

static const char *
funky_find_backward(const char *piece,
                    const char *piece_end,
                    const char *whole,
                    const char *whole_end )
  {
  const char *retval = NULL;

  size_t length_of_piece = piece_end - piece;
  if(length_of_piece == 0)
    {
    __gg__abort("funky_find_backward() length_of_piece shouldn't be zero");
    }

  whole_end -= length_of_piece;

  while( whole <= whole_end )
    {
    if( memcmp( piece, whole_end, length_of_piece) == 0 )
      {
      retval = whole_end;
      break;
      }
    whole_end -= 1;
    }
  return retval;
  }

static char_it_c
funky_find_wide_backward( char_it_c needle,
                 char_it_c needle_end,    // Actually end+1
                 char_it_c haystack,
                 char_it_c haystack_end,  // Actually end+1
                 char_it_c notfound)
  {
  // We are looking for the needle in the haystack

  char_it_c retval = notfound;

  size_t length_of_piece = needle_end - needle;
  if(length_of_piece == 0)
    {
    __gg__abort("funky_find_wide_backward() length_of_piece shouldn't be zero");
    }

  haystack_end -= length_of_piece;

  while( haystack <= haystack_end )
    {
    if( memcmp( &(*needle),
                &(*haystack_end),
                length_of_piece*sizeof(cbl_char_t)) == 0 )
      {
      // They are the same; return where needle was found
      retval = haystack_end;
      break;
      }
    // Not found; move to the next location in the haystack
    haystack_end -= 1;
    }
  return retval;
  }

typedef struct normalized_operand
  {
  // These are the characters of the string.  When the field is NumericDisplay
  // any leading or trailing +/- characters are removed, and any embedded
  // minus bits are removed.

  // In order for INSPECT to handle things like UTF-8, which often has
  // multi-byte codepoints, and UTF-16, which sometimes has multi-pair
  // codepoints we are going to convert everything to UTF-32 for internal
  // calculations and searches.
  std::string the_characters;
  std::vector<cbl_char_t>the_vectorxxxx;

  // offset and length are maintained in characters, not bytes
  size_t offset;  // Usually zero.  Increased by one for leading separate sign.
  size_t length;  // Usually the same as the original.  But it is one less
  //              // than the original when there is a trailing separate sign.
  } normalized_operand;

typedef struct comparand
  {
  size_t id_2_index;
  cbl_inspect_bound_t operation;
  normalized_operand identifier_3; // The thing to be found
  normalized_operand identifier_5; // The replacement, for FORMAT 2
  const char *alpha; // The start location within normalized_id_1
  const char *omega; // The end+1 location within normalized_id_1
  char_it_c     alpha_it;   // The start location within normalized_id_1
  char_it_c     omega_it;   // The end+1 location within normalized_id_1
  size_t leading_count;
  bool leading;
  bool first;
  } comparand;

typedef struct id_2_result
  {
  cblc_field_t *id2;
  size_t        id2_o;
  size_t        id2_s;
  size_t result;
  } id_2_result;

static normalized_operand
normalize_id( const cblc_field_t *field,
              size_t              field_o,
              size_t              field_s,
              cbl_encoding_t      encoding )
  {
  normalized_operand retval;

  if( field )
    {
    charmap_t *charmap = __gg__get_charmap(encoding);

    // This is the old-style byte-based assumption
    const unsigned char *data = field->data + field_o;
    cbl_figconst_t figconst
      = (cbl_figconst_t)(field->attr & FIGCONST_MASK);

    retval.offset = 0;
    retval.length = field_s;

    if( field->type == FldNumericDisplay )
      {
      // The value is NumericDisplay.
      if( field->attr & separate_e )
        {
        // Because the sign is a separate plus or minus, the length
        // gets reduced by one:
        retval.length = field_s - 1;
        if( field->attr & leading_e )
          {
          // Because the sign character is LEADING, we increase the
          // offset by one
          retval.offset = 1;
          }
        }
      for( size_t i=retval.offset; i<retval.length; i+=1 )
        {
        // Because we are dealing with a NumericDisplay that might have
        // the minus bit turned on, we will to mask it off as we copy the
        // input characters over to retval:
        retval.the_characters += charmap->set_digit_negative(data[i], false);
        }
      }
    else
      {
      // We are set up to create the_characters;
      if( figconst == normal_value_e )
        {
        for( size_t i=retval.offset; i<retval.length; i+=1 )
          {
          retval.the_characters += data[i];
          }
        }
      else
        {
        char ch =  charmap->figconst_character(figconst);
        for( size_t i=retval.offset; i<retval.length; i+=1 )
          {
          retval.the_characters += ch;
          }
        }
      }
    }
  else
    {
    // There is no field, so leave the_characters empty.
    retval.offset = 0;
    retval.length = 0;
    }

  if( field )
    {
    cbl_encoding_t source_encoding = field->encoding;
    const charmap_t *charmap_source = __gg__get_charmap(source_encoding);
    charmap_t *charmap = __gg__get_charmap(encoding);
    int stride = charmap->stride();

    const unsigned char *data = field->data + field_o;
    cbl_figconst_t figconst = (cbl_figconst_t)(field->attr & FIGCONST_MASK);
    if( figconst == normal_value_e )
      {
      retval.offset = 0;
      retval.length = field_s / stride;

      if( field->type == FldNumericDisplay )
        {
        // The value is NumericDisplay, so we might need to adjust the offset
        // and length:
        if( field->attr & separate_e )
          {
          // Because the sign is a separate plus or minus, the length
          // gets reduced by one:
          retval.length = field_s - 1;
          if( field->attr & leading_e )
            {
            // Because the sign character is LEADING, we increase the
            // offset by one
            retval.offset = 1;
            }
          }
        }
      // We are ready to convert from the input to UTF32
      size_t converted_characters;
      const char *converted = __gg__iconverter(source_encoding,
                                               DEFAULT_32_ENCODING,
                                               data+retval.offset * stride,
                                               retval.length * stride,
                                               &converted_characters);
      // We are ready to copy the characters over:
      for( size_t i=0; i<converted_characters; i+=width_of_utf32 )
        {
        // Because we are dealing with a NumericDisplay that might have
        // the minus bit turned on, we will to mask it off as we copy the
        // input characters over to retval:
        cbl_char_t ch = charmap->getch(converted, i);
        if( field->type == FldNumericDisplay )
          {
          if( charmap_source->is_like_ebcdic() )
            {
            // In EBCDIC, a flagged negative digit 0xF0 through 0xF9 becomes
            // 0xD0 through 0xD9.  Those represent the characters
            // "}JKLMNOPQR", which, now that we are in UTF32 space, don't have
            // the right bit pattern to be fixed with set_digit_negative().
            // So, we fix it separately with this table:  Note that location
            // 0x7D, which is ASCII '{', becomes 0x30 '0'.  See also that
            // locations 0x4A through 0x52 become 0x31 through 0x39.
            static const uint8_t fixit[256] =
              {
              0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x80, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
              0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x81, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
              0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x82, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
              0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x83, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
              0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x84, 0x49, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36,
              0x37, 0x38, 0x39, 0x53, 0x54, 0x55, 0x56, 0x57, 0x85, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
              0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x86, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
              0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x87, 0x79, 0x7a, 0x7b, 0x7c, 0x30, 0x7e, 0x7f,
              0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
              0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x89, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
              0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0x8a, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
              0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0x8b, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
              0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0x8c, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
              0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0x8d, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
              0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0x8e, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
              0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0x8f, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff,
              };
            ch = fixit[ch & 0xFF];
            }
          else
            {
            ch = charmap->set_digit_negative(ch, false);
            }
          }
        retval.the_vectorxxxx.push_back(ch);
        }
      }
    else
      {
      // We need to fill the field with a figurative constant:
      // We are set up to create the_characters;
      charmap_t *charmap32 = __gg__get_charmap(DEFAULT_32_ENCODING);
      char ch =  charmap32->figconst_character(figconst);
      for( size_t i=retval.offset; i<retval.length; i+=1 )
        {
        retval.the_characters += ch;
        retval.the_vectorxxxx.push_back(ch);
        }
      }
    }
  else
    {
    // There is no field, so leave the_characters empty.
    retval.offset = 0;
    retval.length = 0;
    }

  return retval;
  }

static void
match_lengths(      normalized_operand &id_target,
                    const normalized_operand &id_source)
  {
  // This routine gets called when id_source is a figurative constant and
  // we need the target to be the same length as the source

  char ch = id_target.the_characters[0];
  id_target.the_characters.clear();
  for(size_t i=0; i<id_source.length; i++)
    {
    id_target.the_characters += ch;
    }

  cbl_char_t wch = id_target.the_vectorxxxx[0];
  id_target.the_vectorxxxx.clear();
  for(size_t i=0; i<id_source.length; i++)
    {
    id_target.the_vectorxxxx.push_back(wch);
    }
  id_target.length = id_source.length;
  }

static void
the_alpha_and_omega(const normalized_operand &id_before,
                    const normalized_operand &id_after,
                    const char *          &alpha,
                    const char *          &omega,
                    char_it_c             &alpha_it,
                    char_it_c             &omega_it,
                    char_it_c              notfound)
  {
  /*  The 2023 ISO description of the AFTER and BEFORE phrases of the INSPECT
      statement is, in a word, garbled.

      IBM's COBOL for Linux 1.2 is a little better, but still a bit confusing
      because the description for AFTER neglects to specifically state that
      the scan starts one character to the right of the *first* occurrence of
      the AFTER value.

      Micro Focus 9.2.5 has the advantage of being ungarbled, succinct, and
      unambiguous.

      The BEFORE phrase modifies the character position to use as the rightmost
      position in source for the corresponding comparison operation. Comparisons
      in source occur only to the left of the first occurrence of delimiter. If
      delimiter is not present in source, then the comparison proceeds as if
      there were no BEFORE phrase.

      The AFTER phrase modifies the character position to use as the leftmost
      position in source for the corresponding comparison operation. Comparisons
      in source occur only to the right of the first occurrence of delimiter.
      This character position is the one immediately to the right of the
      rightmost character of the delimiter found. If delimiter is not found in
      source, the INSPECT statement has no effect (no tallying or replacement
      occurs).

      "xyzxyzAFTERxyzxyzxyzxyzBEFORExyzxyzAFTERxyzxyz"
                  ^           ^
                  |           |
                  |           |-- omega
                  ----------------alpha
  */

  if( id_before.length )
    {
    // This is the BEFORE delimiter.   We look for the first occurrence of that
    // delimiter starting at the left of id_1

    const char *start = id_before.the_characters.c_str();
    const char *end   = start + id_before.length;
    const char *found = funky_find(start, end, alpha, omega);
    if( found )
      {
      // We found id_before within alpha/omega, so reduce omega
      // to the found location.
      omega = found;
      // If not found, we just leave omega alone.
      }

    char_it_c omega_found = funky_find_wide(id_before.the_vectorxxxx.begin(),
                                            id_before.the_vectorxxxx.end(),
                                            alpha_it,
                                            omega_it,
                                            notfound );
    if( omega_found != notfound )
      {
      // We found id_before within alpha/omega, so reduce omega
      // to the found location.
      omega_it = omega_found;
      }
    }

  if( id_after.length )
    {
    // This is the AFTER delimiter.  We look for the first occurrence of that
    // delimiter in id_1

    const char *start = id_after.the_characters.c_str();
    const char *end   = start + id_after.length;
    const char *found = funky_find(start, end, alpha, omega);
    if( found )
      {
      // We found id_after in the alpha/omega segment.  We update alpha
      // be the character after the id_after substring.
      alpha = found + (end-start);
      }
    else
      {
      // We didn't find the id_after string, so we set the alpha to be
      // omega.  That means that no tally or replace operation will take
      // because no characters will qualify.
      alpha = omega;
      }

    char_it_c omega_found = funky_find_wide(id_after.the_vectorxxxx.begin(),
                                            id_after.the_vectorxxxx.end(),
                                            alpha_it,
                                            omega_it,
                                            notfound );
    if( omega_found != notfound)
      {
      // We found id_after in the alpha/omega segment.  We update alpha
      // be the character after the id_after substring.
      alpha_it = omega_found + (end-start);
      }
    else
      {
      // We didn't find the id_after string, so we set the alpha to be
      // omega.  That means that no tally or replace operation will take
      // because no characters will qualify.
      alpha_it = omega_it;
      }
    }

  }

static void
the_alpha_and_omega_backward( const normalized_operand &id_before,
                              const normalized_operand &id_after,
                              const char *          &alpha,
                              const char *          &omega,
                              char_it_c             &alpha_it,
                              char_it_c             &omega_it,
                              char_it_c              notfound)
  {
  /*  Like the_alpha_and_omega(), but for handling BACKWARD.

      "xyzxyzBEFORExyzxyzAFTERxyzxyzxyzxyzBEFORExyzxyzAFTERxyzxyz"
                                                ^     ^
                                                |     |
                                                |     -- omega
                                                |--------alpha
  */

  const char *id_1     = alpha;
  const char *id_1_end = omega;

  if( id_before.length )
    {
    // This is the BEFORE delimiter.  We look for the first occurrence of it
    // from the right end of id_1

    const char *start = id_before.the_characters.c_str();
    const char *end   = start + id_before.length;
    const char *found = funky_find_backward(start, end, id_1, id_1_end);
    if( found )
      {
      // We found id_before within id_1, so change alpha to the character just
      // to the right of BEFORE.  Otherwise, we will leave alpha alone, so that
      // it stays at the beginning of id_1. That's because if you can't find
      // id_before, it's as if there were no BEFORE phrase.
      alpha = found + id_before.length;
      }

    char_it_c omega_found = funky_find_wide_backward(id_before.the_vectorxxxx.begin(),
                                            id_before.the_vectorxxxx.end(),
                                            alpha_it,
                                            omega_it,
                                            notfound );
    if( omega_found != notfound )
      {
      // We found id_before within id_1, so change alpha to the character just
      // to the right of BEFORE.  Otherwise, we will leave alpha alone, so that
      // it stays at the beginning of id_1
      alpha_it = omega_found + id_before.length;
      }
    }

  if( id_after.length )
    {
    // This is the AFTER delimiter.  We look for the first occurrence in id_1

    const char *start = id_after.the_characters.c_str();
    const char *end   = start + id_after.length;
    const char *found = funky_find_backward(start, end, alpha, omega);
    if( found )
      {
      // We found id_after in id_1.  We update omega to be
      // at that location.
      omega = found;
      }
    else
      {
      // If the AFTER isn't found, we need to adjust things so that nothing
      // happens.
      omega = alpha;
      }

    char_it_c omega_found = funky_find_wide_backward(id_after.the_vectorxxxx.begin(),
                                            id_after.the_vectorxxxx.end(),
                                            alpha_it,
                                            omega_it,
                                            notfound );
    if( omega_found != notfound)
      {
      // We found id_after in id_1.  We update omega to be
      // at that location.
      omega_it = omega_found;
      }
    else
      {
      // If the AFTER isn't found, we need to adjust things so that nothing
      // happens.
      omega_it = alpha_it;
      }
    }
  }

static
void
inspect_backward_format_1(const size_t integers[])
  {
  size_t int_index = 0;
  size_t cblc_index = 0;

  // Reference the language specification for the meanings of identifier_X

  // Pick up the number of identifier_2 loops in this INSPECT statement
  size_t n_identifier_2 = integers[int_index++];

  std::vector<id_2_result> id_2_results(n_identifier_2);

  // Pick up identifier_1, which is the string being inspected
  const cblc_field_t *id1   = __gg__treeplet_1f[cblc_index];
  size_t              id1_o = __gg__treeplet_1o[cblc_index];
  size_t              id1_s = __gg__treeplet_1s[cblc_index];
  cblc_index += 1;
  // normalize it, according to the language specification.
  normalized_operand normalized_id_1 = normalize_id(id1, id1_o, id1_s, id1->encoding);

  std::vector<comparand> comparands;

  for(size_t i=0; i<n_identifier_2; i++)
    {
    // For each identifier_2, we pick up its value:

    id_2_results[i].id2   = __gg__treeplet_1f  [cblc_index];
    id_2_results[i].id2_o = __gg__treeplet_1o[cblc_index];
    id_2_results[i].id2_s = __gg__treeplet_1s[cblc_index];

    cblc_index += 1;
    id_2_results[i].result = 0;

    // For each identifier 2, there is a count of operations:
    size_t nbounds = integers[int_index++];

    for(size_t j=0; j<nbounds; j++ )
      {
      // each operation has a bound code:
      cbl_inspect_bound_t operation
        = (cbl_inspect_bound_t)integers[int_index++];
      switch( operation )
        {
        case bound_characters_e:
          {
          // We are counting characters.  There is no identifier-3,
          // but we we hard-code the length to one to represent a
          // single character.
          comparand next_comparand = {};
          next_comparand.id_2_index = i;
          next_comparand.operation = operation;
          next_comparand.identifier_3.length = 1;

          const cblc_field_t *id4_before   = __gg__treeplet_1f  [cblc_index];
          size_t              id4_before_o = __gg__treeplet_1o[cblc_index];
          size_t              id4_before_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          const cblc_field_t *id4_after   = __gg__treeplet_1f  [cblc_index];
          size_t              id4_after_o = __gg__treeplet_1o[cblc_index];
          size_t              id4_after_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          normalized_operand normalized_id_4_before
            = normalize_id(id4_before, id4_before_o, id4_before_s, id1->encoding);

          normalized_operand normalized_id_4_after
            = normalize_id(id4_after, id4_after_o, id4_after_s, id1->encoding);

          next_comparand.alpha
            = normalized_id_1.the_characters.c_str();

          next_comparand.omega
            = next_comparand.alpha + normalized_id_1.length;

          next_comparand.alpha_it = normalized_id_1.the_vectorxxxx.begin();
          next_comparand.omega_it = normalized_id_1.the_vectorxxxx.end();

          the_alpha_and_omega_backward(normalized_id_4_before,
                              normalized_id_4_after,
                              next_comparand.alpha,
                              next_comparand.omega,
                              next_comparand.alpha_it,
                              next_comparand.omega_it,
                              normalized_id_1.the_vectorxxxx.end());

          comparands.push_back(next_comparand);
          break;
          }
        default:
          {
          // We have some number of identifer-3 values,
          // each with possible PHRASE1 modifiers.
          size_t pair_count = integers[int_index++];

          // We need to build up pair_count comparand structures:

          for(size_t k=0; k<pair_count; k++)
            {
            comparand next_comparand = {};
            next_comparand.id_2_index = i;
            next_comparand.operation = operation;

            const cblc_field_t *id3   = __gg__treeplet_1f[cblc_index];
            size_t              id3_o = __gg__treeplet_1o[cblc_index];
            size_t              id3_s = __gg__treeplet_1s[cblc_index];
            cblc_index += 1;

            const cblc_field_t *id4_before   = __gg__treeplet_1f[cblc_index];
            size_t              id4_before_o = __gg__treeplet_1o[cblc_index];
            size_t              id4_before_s = __gg__treeplet_1s[cblc_index];
            cblc_index += 1;

            const cblc_field_t *id4_after   = __gg__treeplet_1f[cblc_index];
            size_t              id4_after_o = __gg__treeplet_1o[cblc_index];
            size_t              id4_after_s = __gg__treeplet_1s[cblc_index];
            cblc_index += 1;

            next_comparand.identifier_3
                                    = normalize_id(id3, id3_o, id3_s, id1->encoding);

            next_comparand.alpha
              = normalized_id_1.the_characters.c_str();
            next_comparand.omega
              = next_comparand.alpha + normalized_id_1.length;

            normalized_operand normalized_id_4_before
              = normalize_id(id4_before, id4_before_o, id4_before_s, id1->encoding);

            normalized_operand normalized_id_4_after
              = normalize_id(id4_after, id4_after_o, id4_after_s, id1->encoding);

            next_comparand.alpha_it = normalized_id_1.the_vectorxxxx.begin();
            next_comparand.omega_it = normalized_id_1.the_vectorxxxx.end();

            the_alpha_and_omega_backward(normalized_id_4_before,
                                normalized_id_4_after,
                                next_comparand.alpha,
                                next_comparand.omega,
                                next_comparand.alpha_it,
                                next_comparand.omega_it,
                                normalized_id_1.the_vectorxxxx.end());

            next_comparand.leading = true;
            next_comparand.leading_count = 0;
            comparands.push_back(next_comparand);
            }
          }
        }
      }
    }

  // We are now ready to walk through identifier-1, character by
  // character, checking each of the comparands for a match:

  // We are now set up to accomplish the data flow described
  // in the language specification.  We loop through the
  // the character positions in normalized_id_1:
  char_it_c leftmost  = normalized_id_1.the_vectorxxxx.begin();
  char_it_c rightmost = leftmost + normalized_id_1.length;
  char_it_c the_end_of_the_world = rightmost;

  while( leftmost < rightmost )
    {
    size_t rightmost_delta = 0;
    rightmost -= 1;
    // We look at the rightmost position.  If that position is within the
    // alpha-to-omega qualified range, we check all possible matches:

    for(size_t k=0; k<comparands.size(); k++)
      {
      if( rightmost < comparands[k].alpha_it )
        {
        // This can't be a match, because rightmost is
        // to the left of the comparand's alpha.
        continue;
        }
      if( rightmost + comparands[k].identifier_3.length >
                                                       comparands[k].omega_it )
        {
        // This can't be a match, because the rightmost
        // character of the comparand falls to the right
        // of the comparand's omega
        continue;
        }
      if( rightmost + comparands[k].identifier_3.length >
                                                        the_end_of_the_world )
        {
        // This can't be a match, because the rightmost character of the
        // comparand falls past the new edge of id_1 established by a prior
        // match.
        continue;
        }
      // A match is theoretically possible, because all
      // the characters of the comparand fall between
      // alpha and omega:
      bool possible_match = true;

      if( comparands[k].operation != bound_characters_e )
        {
        for(size_t m=0; m<comparands[k].identifier_3.length; m++)
          {
          if( comparands[k].identifier_3.the_vectorxxxx[m] != rightmost[m] )
            {
            possible_match = false;
            break;
            }
          }
        }
      if( possible_match )
        {
        // The characters of the comparand match the
        // characters at rightmost.
        bool match = false;
        switch( comparands[k].operation )
          {
          case bound_first_e:
            // This can't happen in a FORMAT_1
            warnx("The compiler goofed: "
                  "INSPECT FORMAT 1 "
                  "shouldn't have "
                  "bound_first_e");
            abort();
            break;

          case bound_characters_e:
            match = 1;
            break;

          case bound_all_e:
            {
            // We have a match.
            match = true;
            break;
            }

          case bound_leading_e:
            {
            // We have a match at rightmost.  But we need to figure out if this
            // particular match is valid for LEADING.

            if( comparands[k].leading )
              {
              if( rightmost + comparands[k].identifier_3.length
                                                     == comparands[k].omega_it)
                {
                // This means that the match here is just the latest of a
                // string of LEADING matches that started at .omega
                comparands[k].leading_count += 1;
                match = true;
                comparands[k].omega_it -= comparands[k].identifier_3.length;
                the_end_of_the_world = rightmost;
                rightmost_delta = comparands[k].identifier_3.length-1;
                }
              }
            break;
            }

          case bound_trailing_e:
            {
            // We have a match at rightmost.
            //
            // We want to know if this is a trailing match.  For that to be,
            // all of the possible matches from here leftward to the alpha have
            // to be true as well:

            if( (rightmost - comparands[k].alpha_it )
                    % comparands[k].identifier_3.length == 0 )
              {
              // The remaining number of characters is correct for a match.
              // Keep checking.

              // Assume a match until we learn otherwise:
              match = true;
              char_it_c local_left = rightmost;
              local_left -= comparands[k].identifier_3.length;
              while( local_left >= comparands[k].alpha_it )
                {
                for(size_t m=0; m<comparands[k].identifier_3.length; m++)
                  {
                  if( comparands[k].identifier_3.the_vectorxxxx[m]
                      != local_left[m] )
                    {
                    // We have a mismatched character, so no trailing match is
                    // possible
                    match = false;
                    break;
                    }
                  }
                local_left -= comparands[k].identifier_3.length;
                }
              }
            break;
            }
          }

        if( match )
          {
          // We have a match at rightmost:
          // Bump the result counter
          id_2_results[comparands[k].id_2_index].result += 1;

          // We have a match here at rightmost, so we need to set the end of
          // the world here
          the_end_of_the_world = rightmost;

          // Adjust rightmost by the additional characters in a BACKWARD
          // LEADING search:
          rightmost -= rightmost_delta;
          break;
          }
        }
      else
        {
        // We are within alpha/omega, but there was no
        // match, which permanently disqualifies the
        // possibility of LEADING
        comparands[k].leading = false;
        }
      }
    }

  // Add our results to the identifier_2 values:

  for(size_t i = 0; i<id_2_results.size(); i++)
    {
    int rdigits;
    __int128 id_2_value
      = __gg__binary_value_from_qualified_field(&rdigits,
                                                id_2_results[i].id2,
                                                id_2_results[i].id2_o,
                                                id_2_results[i].id2_s);
    while(rdigits--)
      {
      id_2_value /= 10.0;
      }

    // Accumulate what we've found into it
    id_2_value += id_2_results[i].result;

    // And put it back:
    __gg__int128_to_qualified_field(id_2_results[i].id2,
                                    id_2_results[i].id2_o,
                                    id_2_results[i].id2_s,
                                    id_2_value,
                                    0,
                                    truncation_e,
                                    NULL);
    }
  }

extern "C"
void
__gg__inspect_format_1(int backward, size_t integers[])
  {
  if( backward )
    {
    return inspect_backward_format_1(integers);
    }

  size_t int_index = 0;
  size_t cblc_index = 0;

  // Reference the language specification for the meanings of identifier_X

  // Pick up the number of identifier_2 loops in this INSPECT statement
  size_t n_identifier_2 = integers[int_index++];

  std::vector<id_2_result> id_2_results(n_identifier_2);

  // Pick up identifier_1, which is the string being inspected
  const cblc_field_t *id1   = __gg__treeplet_1f[cblc_index];
  size_t              id1_o = __gg__treeplet_1o[cblc_index];
  size_t              id1_s = __gg__treeplet_1s[cblc_index];
  cblc_index += 1;
  // normalize it, according to the language specification.
  normalized_operand normalized_id_1
                             = normalize_id(id1, id1_o, id1_s, id1->encoding);

  std::vector<comparand> comparands;

  for(size_t i=0; i<n_identifier_2; i++)
    {
    // For each identifier_2, we pick up its value:

    id_2_results[i].id2   = __gg__treeplet_1f  [cblc_index];
    id_2_results[i].id2_o = __gg__treeplet_1o[cblc_index];
    id_2_results[i].id2_s = __gg__treeplet_1s[cblc_index];

    cblc_index += 1;
    id_2_results[i].result = 0;

    // For each identifier 2, there is a count of operations:
    size_t nbounds = integers[int_index++];

    for(size_t j=0; j<nbounds; j++ )
      {
      // each operation has a bound code:
      cbl_inspect_bound_t operation
        = (cbl_inspect_bound_t)integers[int_index++];
      switch( operation )
        {
        case bound_characters_e:
          {
          // We are counting characters.  There is no identifier-3,
          // but we we hard-code the length to one to represent a
          // single character.
          comparand next_comparand = {};
          next_comparand.id_2_index = i;
          next_comparand.operation = operation;
          next_comparand.identifier_3.length = 1;

          const cblc_field_t *id4_before   = __gg__treeplet_1f  [cblc_index];
          size_t              id4_before_o = __gg__treeplet_1o[cblc_index];
          size_t              id4_before_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          const cblc_field_t *id4_after   = __gg__treeplet_1f  [cblc_index];
          size_t              id4_after_o = __gg__treeplet_1o[cblc_index];
          size_t              id4_after_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          normalized_operand normalized_id_4_before
            = normalize_id(id4_before, id4_before_o, id4_before_s, id1->encoding);

          normalized_operand normalized_id_4_after
            = normalize_id(id4_after, id4_after_o, id4_after_s, id1->encoding);

          next_comparand.alpha
            = normalized_id_1.the_characters.c_str();

          next_comparand.omega
            = next_comparand.alpha + normalized_id_1.length;

          next_comparand.alpha_it = normalized_id_1.the_vectorxxxx.begin();
          next_comparand.omega_it = normalized_id_1.the_vectorxxxx.end();

          the_alpha_and_omega(normalized_id_4_before,
                              normalized_id_4_after,
                              next_comparand.alpha,
                              next_comparand.omega,
                              next_comparand.alpha_it,
                              next_comparand.omega_it,
                              normalized_id_1.the_vectorxxxx.end());

          comparands.push_back(next_comparand);
          break;
          }
        default:
          {
          // We have some number of identifer-3 values,
          // each with possible PHRASE1 modifiers.
          size_t pair_count = integers[int_index++];

          // We need to build up pair_count comparand structures:

          for(size_t k=0; k<pair_count; k++)
            {
            comparand next_comparand = {};
            next_comparand.id_2_index = i;
            next_comparand.operation = operation;

            const cblc_field_t *id3   = __gg__treeplet_1f[cblc_index];
            size_t              id3_o = __gg__treeplet_1o[cblc_index];
            size_t              id3_s = __gg__treeplet_1s[cblc_index];
            cblc_index += 1;

            const cblc_field_t *id4_before   = __gg__treeplet_1f[cblc_index];
            size_t              id4_before_o = __gg__treeplet_1o[cblc_index];
            size_t              id4_before_s = __gg__treeplet_1s[cblc_index];
            cblc_index += 1;

            const cblc_field_t *id4_after   = __gg__treeplet_1f[cblc_index];
            size_t              id4_after_o = __gg__treeplet_1o[cblc_index];
            size_t              id4_after_s = __gg__treeplet_1s[cblc_index];
            cblc_index += 1;

            next_comparand.identifier_3
                                    = normalize_id(id3,
                                                           id3_o,
                                                           id3_s,
                                               id1->encoding);

            next_comparand.alpha
              = normalized_id_1.the_characters.c_str();
            next_comparand.omega
              = next_comparand.alpha + normalized_id_1.length;

            next_comparand.alpha_it = normalized_id_1.the_vectorxxxx.begin();
            next_comparand.omega_it = normalized_id_1.the_vectorxxxx.end();

            normalized_operand normalized_id_4_before
              = normalize_id(id4_before, id4_before_o, id4_before_s, id1->encoding);

            normalized_operand normalized_id_4_after
              = normalize_id(id4_after, id4_after_o, id4_after_s, id1->encoding);

            the_alpha_and_omega(normalized_id_4_before,
                                normalized_id_4_after,
                                next_comparand.alpha,
                                next_comparand.omega,
                                next_comparand.alpha_it,
                                next_comparand.omega_it,
                                normalized_id_1.the_vectorxxxx.end());

            next_comparand.leading = true;
            next_comparand.leading_count = 0;
            comparands.push_back(next_comparand);
            }
          }
        }
      }
    }

  // We are now ready to walk through identifier-1, character by
  // character, checking each of the comparands for a match:

  // We are now set up to accomplish the data flow described
  // in the language specification.  We loop through the
  // the character positions in normalized_id_1:
  char_it_c leftmost  = normalized_id_1.the_vectorxxxx.begin();
  char_it_c rightmost = leftmost + normalized_id_1.length;

  while( leftmost < rightmost )
    {
    // For each leftmost position, we check each of the
    // pairs:

    for(size_t k=0; k<comparands.size(); k++)
      {
      if( leftmost < comparands[k].alpha_it )
        {
        // This can't be a match, because leftmost is
        // to the left of the comparand's alpha.
        continue;
        }
      if( leftmost + comparands[k].identifier_3.length > comparands[k].omega_it )
        {
        // This can't be a match, because the rightmost
        // character of the comparand falls to the right
        // of the comparand's omega
        continue;
        }
      // A match is theoretically possible, because all
      // the characters of the comparand fall between
      // alpha and omega:
      bool possible_match = true;

      if( comparands[k].operation != bound_characters_e )
        {
        for(size_t m=0; m<comparands[k].identifier_3.length; m++)
          {
          if( comparands[k].identifier_3.the_vectorxxxx[m] != leftmost[m] )
            {
            possible_match = false;
            break;
            }
          }
        }
      if( possible_match )
        {
        // The characters of the comparand match the
        // characters at leftmost.
        bool match = false;
        switch( comparands[k].operation )
          {
          case bound_first_e:
            // This can't happen in a FORMAT_1
            warnx("The compiler goofed: "
                  "INSPECT FORMAT 1 "
                  "shouldn't have "
                  "bound_first_e");
            abort();
            break;

          case bound_characters_e:
            match = true;
            break;

          case bound_all_e:
            {
            // We have a match.
            match = true;
            break;
            }

          case bound_leading_e:
            {
            // We have a match at leftmost.  But we need to figure out if this
            // particular match is valid for LEADING.

            // Hang onto your hat.  This is delightfully clever.
            //
            // This position is LEADING if:
            //  1) .leading is still true
            //  2) leftmost / (length_of_comparand ) = current_count
            //
            // I get chills every time I look at that.

            if( comparands[k].leading )
              {
              // So far, so good.
              size_t count = ((leftmost - comparands[k].alpha_it))
                              / comparands[k].identifier_3.length;
              if( count == comparands[k].leading_count )
                {
                // This means that the match here is just the latest of a
                // string of LEADING matches that started at .alpha
                comparands[k].leading_count += 1;
                match = true;
                }
              }
            break;
            }

          case bound_trailing_e:
            {
            // We have a match at leftmost.
            //
            // We want to know if this is a trailing match.  For that to be,
            // all of the possible matches from here to the omega have to be
            // true as well:

            if( (comparands[k].omega_it-leftmost)
                    % comparands[k].identifier_3.length == 0 )
              {
              // The remaining number of characters is correct for a match.
              // Keep checking.

              // Assume a match until we learn otherwise:
              match = true;
              char_it_c local_left = leftmost;
              local_left += comparands[k].identifier_3.length;
              while( match && local_left < comparands[k].omega_it )
                {
                for(size_t m=0; m<comparands[k].identifier_3.length; m++)
                  {
                  if( comparands[k].identifier_3.the_vectorxxxx[m]
                      != local_left[m] )
                    {
                    // We have a mismatched character, so no trailing match is
                    // possible
                    match = false;
                    break;
                    }
                  }
                local_left += comparands[k].identifier_3.length;
                }
              }
            break;
            }
          }

        if( match )
          {
          // We have a match at leftmost:

          // Bump the result counter
          id_2_results[comparands[k].id_2_index].result += 1;

          // Adjust the leftmost pointer to point to
          // the rightmost character of the matched
          // string, keeping in mind that it will be
          // bumped again after we break out of the
          // k<pair_count loop:
          leftmost += comparands[k].identifier_3.length - 1;
          break;
          }
        }
      else
        {
        // We are within alpha/omega, but there was no
        // match, which permanently disqualifies the
        // possibility of LEADING
        comparands[k].leading = false;
        }
      }
    leftmost += 1;
    }

  // Add our results to the identifier_2 values:

  for(size_t i = 0; i<id_2_results.size(); i++)
    {
    int rdigits;
    __int128 id_2_value
      = __gg__binary_value_from_qualified_field(&rdigits,
                                                id_2_results[i].id2,
                                                id_2_results[i].id2_o,
                                                id_2_results[i].id2_s);
    while(rdigits--)
      {
      id_2_value /= 10.0;
      }

    // Accumulate what we've found into it
    id_2_value += id_2_results[i].result;

    // And put it back:
    __gg__int128_to_qualified_field(id_2_results[i].id2,
                                    id_2_results[i].id2_o,
                                    id_2_results[i].id2_s,
                                    id_2_value,
                                    0,
                                    truncation_e,
                                    NULL);
    }
  }

static
void
inspect_backward_format_2(const size_t integers[])
  {
  size_t int_index = 0;
  size_t cblc_index = 0;

  // Reference the language specification for the meanings of identifier_X

  // Pick up identifier_1, which is the string being inspected
  cblc_field_t *id1   = __gg__treeplet_1f[cblc_index];
  size_t        id1_o = __gg__treeplet_1o[cblc_index];
  size_t        id1_s = __gg__treeplet_1s[cblc_index];
  cblc_index += 1;

  // normalize it, according to the language specification.
  normalized_operand normalized_id_1
                                   = normalize_id(id1, id1_o, id1_s, id1->encoding);

  std::vector<comparand> comparands;

  // Pick up the count of operations:
  size_t nbounds = integers[int_index++];

  for(size_t j=0; j<nbounds; j++ )
    {
    // each operation has a bound code:
    cbl_inspect_bound_t operation = (cbl_inspect_bound_t)integers[int_index++];
    switch( operation )
      {
      case bound_characters_e:
        {
        comparand next_comparand = {};
        next_comparand.operation = operation;

        const cblc_field_t *id5   = __gg__treeplet_1f[cblc_index];
        size_t              id5_o = __gg__treeplet_1o[cblc_index];
        size_t              id5_s = __gg__treeplet_1s[cblc_index];
        cblc_index += 1;

        const cblc_field_t *id4_before   = __gg__treeplet_1f[cblc_index];
        size_t              id4_before_o = __gg__treeplet_1o[cblc_index];
        size_t              id4_before_s = __gg__treeplet_1s[cblc_index];
        cblc_index += 1;

        const cblc_field_t *id4_after   = __gg__treeplet_1f  [cblc_index];
        size_t              id4_after_o = __gg__treeplet_1o[cblc_index];
        size_t              id4_after_s = __gg__treeplet_1s[cblc_index];
        cblc_index += 1;

        next_comparand.identifier_5
          = normalize_id(id5, id5_o, id5_s, id1->encoding);
        normalized_operand normalized_id_4_before
          = normalize_id(id4_before, id4_before_o, id4_before_s, id1->encoding);
        normalized_operand normalized_id_4_after
          = normalize_id(id4_after, id4_after_o, id4_after_s, id1->encoding);

        // Because this is a CHARACTER operation, the lengths of
        // identifier-3 and identifier-5 should be one.  Let's avoid the
        // chaos that will otherwise ensue should the lengths *not* be
        // one.
        next_comparand.identifier_3.length = 1;
        next_comparand.identifier_5.length = 1;

        next_comparand.alpha = normalized_id_1.the_characters.c_str();
        next_comparand.omega
          = next_comparand.alpha + normalized_id_1.length;

        next_comparand.alpha_it = normalized_id_1.the_vectorxxxx.begin();
        next_comparand.omega_it = normalized_id_1.the_vectorxxxx.end();

        the_alpha_and_omega_backward(normalized_id_4_before,
                            normalized_id_4_after,
                            next_comparand.alpha,
                            next_comparand.omega,
                            next_comparand.alpha_it,
                            next_comparand.omega_it,
                            normalized_id_1.the_vectorxxxx.end());


        comparands.push_back(next_comparand);
        break;
        }
      default:
        {
        // We have some number of identifer-3/identifier-5 pairs,
        // each with possible PHRASE1 modifiers.
        size_t pair_count = integers[int_index++];

        for(size_t k=0; k<pair_count; k++)
          {
          comparand next_comparand = {};
          next_comparand.operation = operation;

          const cblc_field_t *id3   = __gg__treeplet_1f[cblc_index];
          size_t              id3_o = __gg__treeplet_1o[cblc_index];
          size_t              id3_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          const cblc_field_t *id5   = __gg__treeplet_1f[cblc_index];
          size_t              id5_o = __gg__treeplet_1o[cblc_index];
          size_t              id5_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          const cblc_field_t *id4_before   = __gg__treeplet_1f[cblc_index];
          size_t              id4_before_o = __gg__treeplet_1o[cblc_index];
          size_t              id4_before_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          const cblc_field_t *id4_after   = __gg__treeplet_1f[cblc_index];
          size_t              id4_after_o = __gg__treeplet_1o[cblc_index];
          size_t              id4_after_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          next_comparand.identifier_3 = normalize_id(id3, id3_o, id3_s, id1->encoding);
          next_comparand.identifier_5 = normalize_id(id5, id5_o, id5_s, id1->encoding);

          // Identifiers 3 and 5 have to be the same length.  But
          // but either, or both, can be figurative constants.  If
          // they are figurative constants, they start off with a
          // length of one.  We will expand figurative constants to
          // match the length of the other one:

          if( id3->attr & FIGCONST_MASK )
            {
            match_lengths(  next_comparand.identifier_3,
                            next_comparand.identifier_5);
            }
          else if( id5->attr & FIGCONST_MASK )
            {
            match_lengths(  next_comparand.identifier_5,
                            next_comparand.identifier_3);
            }

          next_comparand.alpha
            = normalized_id_1.the_characters.c_str();
          next_comparand.omega
            = next_comparand.alpha + normalized_id_1.length;

          normalized_operand normalized_id_4_before
            = normalize_id(id4_before, id4_before_o, id4_before_s, id1->encoding);
          normalized_operand normalized_id_4_after
            = normalize_id(id4_after, id4_after_o, id4_after_s, id1->encoding);

          next_comparand.alpha_it = normalized_id_1.the_vectorxxxx.begin();
          next_comparand.omega_it = normalized_id_1.the_vectorxxxx.end();

          the_alpha_and_omega_backward(normalized_id_4_before,
                              normalized_id_4_after,
                              next_comparand.alpha,
                              next_comparand.omega,
                              next_comparand.alpha_it,
                              next_comparand.omega_it,
                              normalized_id_1.the_vectorxxxx.end());

          next_comparand.leading = true;
          next_comparand.leading_count = 0;
          next_comparand.first   = true;
          comparands.push_back(next_comparand);
          }
        }
      }
    }

  // We can now look through normalized_id_1 and replace characters:

  char_it_c leftmost  = normalized_id_1.the_vectorxxxx.begin();
  char_it_c rightmost = leftmost + normalized_id_1.length;
  char_it_c the_end_of_the_world = rightmost;

  while( leftmost < rightmost )
    {
    size_t rightmost_delta = 0;

    rightmost -= 1;
    // We look at the rightmost position.  If that position is within the
    // alpha-to-omega qualified range, we check all possible matches:

    for(size_t k=0; k<comparands.size(); k++)
      {
      if( rightmost < comparands[k].alpha_it )
        {
        // This can't be a match, because rightmost is
        // to the left of the comparand's alpha.
        continue;
        }
      if( rightmost + comparands[k].identifier_3.length > comparands[k].omega_it )
        {
        // This can't be a match, because the rightmost
        // character of the comparand falls to the right
        // of the comparand's omega
        continue;
        }
      if( rightmost + comparands[k].identifier_3.length > the_end_of_the_world )
        {
        // This can't be a match, because the rightmost character of the
        // comparand falls past the new edge of id_1 established by a prior
        // match.
        continue;
        }
      // A match is theoretically possible, because all
      // the characters of the comparand fall between
      // alpha and omega:
      bool possible_match = true;

      if( comparands[k].operation != bound_characters_e )
        {
        for(size_t m=0; m<comparands[k].identifier_3.length; m++)
          {
          if( comparands[k].identifier_3.the_vectorxxxx[m] != rightmost[m] )
            {
            possible_match = false;
            break;
            }
          }
        }
      if( possible_match )
        {
        // The characters of the comparand match the
        // characters at rightmost.
        bool match = false;
        switch( comparands[k].operation )
          {
          case bound_first_e:
            // This can't happen in a FORMAT_2
            warnx("The compiler goofed: "
                  "INSPECT FORMAT 2 "
                  "shouldn't have "
                  "bound_first_e");
            abort();
            break;

          case bound_characters_e:
            match = 1;
            break;

          case bound_all_e:
            {
            // We have a match.
            match = true;
            break;
            }

          case bound_leading_e:
            {
            // We have a match at rightmost.  But we need to figure out if this
            // particular match is valid for LEADING.

            if( comparands[k].leading )
              {
              if(   rightmost
                  + comparands[k].identifier_3.length * (comparands[k].leading_count +1)
                    == comparands[k].omega_it)
                {
                // This means that the match here is just the latest of a
                // string of LEADING matches that started at .omega
                comparands[k].leading_count += 1;
                match = true;
                rightmost_delta = comparands[k].identifier_3.length-1;
                }
              }
            break;
            }

          case bound_trailing_e:
            {
            // We have a match at rightmost.
            //
            // We want to know if this is a trailing match.  For that to be,
            // all of the possible matches from here leftward to the alpha have
            // to be true as well:

            if( (rightmost - comparands[k].alpha_it )
                    % comparands[k].identifier_3.length == 0 )
              {
              // The remaining number of characters is correct for a match.
              // Keep checking.

              // Assume a match until we learn otherwise:
              match = true;
              char_it_c local_left = rightmost;
              local_left -= comparands[k].identifier_3.length;
              while( local_left >= comparands[k].alpha_it )
                {
                for(size_t m=0; m<comparands[k].identifier_3.length; m++)
                  {
                  if( comparands[k].identifier_3.the_vectorxxxx[m]
                      != local_left[m] )
                    {
                    // We have a mismatched character, so no trailing match is
                    // possible
                    match = false;
                    break;
                    }
                  }
                local_left -= comparands[k].identifier_3.length;
                }
              }
            break;
            }
          }

        if( match )
          {
          // We have a match at rightmost.  We need to
          // to replace the characters in normalized_id_1
          // with the characters from normalized_id_5
          //fprintf(stderr, "Rule: %ld %p %s\n", k+1, rightmost, rightmost);

          size_t index = rightmost - normalized_id_1.the_vectorxxxx.begin();
          for( size_t l = 0;
               l < comparands[k].identifier_5.length;
               l++ )
            {
            cbl_char_t ch = comparands[k].identifier_5.
                      the_vectorxxxx[l];
            normalized_id_1.the_vectorxxxx[index++] = ch;
            }

          the_end_of_the_world = rightmost;
          rightmost -= rightmost_delta;
          break;
          }
        }
      else
        {
        comparands[k].leading = false;
        }
      }
    }

  // Here is where we take the characters from normalized_id_1 and put them
  // back into identifier_1.

  charmap_t *charmap = __gg__get_charmap(id1->encoding);
  // Wastefully prefill id_1 with spaces in case the processing resulted in a
  // string shorter than the original.  (There is always the possiblity that
  // a UTF-8 or UTF-16 codeset pair got replaced with a single character.) Do
  // this before calling __gg__converter, because both mapped_character and
  // __gg__iconverter use the same static buffer.
  unsigned char *id1_data = id1->data + id1_o;
  charmap->memset(id1_data, charmap->mapped_character(ascii_space), id1_s);

  // We've been working in UTF32; we convert back to the original id1 encoding.
  size_t bytes_converted;
  const char *converted = __gg__iconverter( DEFAULT_32_ENCODING,
                                         id1->encoding,
                                         normalized_id_1.the_vectorxxxx.data(),
                                         normalized_id_1.length*width_of_utf32,
                                         &bytes_converted) ;
  // And move those characters into place in id_1:
  memcpy(id1_data,
         converted,
         std::min(bytes_converted, id1_s));

  return;
  }

extern "C"
void
__gg__inspect_format_2(int backward, size_t integers[])
  {
  if( backward )
    {
    return inspect_backward_format_2(integers);
    }
  size_t int_index = 0;
  size_t cblc_index = 0;

  // Reference the language specification for the meanings of identifier_X

  // Pick up identifier_1, which is the string being inspected
  cblc_field_t *id1   = __gg__treeplet_1f[cblc_index];
  size_t        id1_o = __gg__treeplet_1o[cblc_index];
  size_t        id1_s = __gg__treeplet_1s[cblc_index];
  cblc_index += 1;

  // normalize it, according to the language specification.
  normalized_operand normalized_id_1
                                   = normalize_id(id1, id1_o, id1_s, id1->encoding);

  std::vector<comparand> comparands;

  // Pick up the count of operations:
  size_t nbounds = integers[int_index++];

  for(size_t j=0; j<nbounds; j++ )
    {
    // each operation has a bound code:
    cbl_inspect_bound_t operation
      = (cbl_inspect_bound_t)integers[int_index++];
    switch( operation )
      {
      case bound_characters_e:
        {
        comparand next_comparand = {} ;
        next_comparand.operation = operation;

        const cblc_field_t *id5   = __gg__treeplet_1f[cblc_index];
        size_t              id5_o = __gg__treeplet_1o[cblc_index];
        size_t              id5_s = __gg__treeplet_1s[cblc_index];
        cblc_index += 1;

        const cblc_field_t *id4_before   = __gg__treeplet_1f[cblc_index];
        size_t              id4_before_o = __gg__treeplet_1o[cblc_index];
        size_t              id4_before_s = __gg__treeplet_1s[cblc_index];
        cblc_index += 1;

        const cblc_field_t *id4_after   = __gg__treeplet_1f  [cblc_index];
        size_t              id4_after_o = __gg__treeplet_1o[cblc_index];
        size_t              id4_after_s = __gg__treeplet_1s[cblc_index];
        cblc_index += 1;

        next_comparand.identifier_5
          = normalize_id(id5, id5_o, id5_s, id1->encoding);
        normalized_operand normalized_id_4_before
          = normalize_id(id4_before, id4_before_o, id4_before_s, id1->encoding);
        normalized_operand normalized_id_4_after
          = normalize_id(id4_after, id4_after_o, id4_after_s, id1->encoding);

        // Because this is a CHARACTER operation, the lengths of
        // identifier-3 and identifier-5 should be one.  Let's avoid the
        // chaos that will otherwise ensue should the lengths *not* be
        // one.
        next_comparand.identifier_3.length = 1;
        next_comparand.identifier_5.length = 1;

        next_comparand.alpha = normalized_id_1.the_characters.c_str();
        next_comparand.omega
          = next_comparand.alpha + normalized_id_1.length;

        next_comparand.alpha_it = normalized_id_1.the_vectorxxxx.begin();
        next_comparand.omega_it = normalized_id_1.the_vectorxxxx.end();

        the_alpha_and_omega(normalized_id_4_before,
                            normalized_id_4_after,
                            next_comparand.alpha,
                            next_comparand.omega,
                            next_comparand.alpha_it,
                            next_comparand.omega_it,
                            normalized_id_1.the_vectorxxxx.end());
        comparands.push_back(next_comparand);
        break;
        }
      default:
        {
        // We have some number of identifer-3/identifier-5 pairs,
        // each with possible PHRASE1 modifiers.
        size_t pair_count = integers[int_index++];

        for(size_t k=0; k<pair_count; k++)
          {
          comparand next_comparand = {};
          next_comparand.operation = operation;

          const cblc_field_t *id3   = __gg__treeplet_1f[cblc_index];
          size_t              id3_o = __gg__treeplet_1o[cblc_index];
          size_t              id3_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          const cblc_field_t *id5   = __gg__treeplet_1f[cblc_index];
          size_t              id5_o = __gg__treeplet_1o[cblc_index];
          size_t              id5_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          const cblc_field_t *id4_before   = __gg__treeplet_1f[cblc_index];
          size_t              id4_before_o = __gg__treeplet_1o[cblc_index];
          size_t              id4_before_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          const cblc_field_t *id4_after   = __gg__treeplet_1f[cblc_index];
          size_t              id4_after_o = __gg__treeplet_1o[cblc_index];
          size_t              id4_after_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          next_comparand.identifier_3 = normalize_id(id3,
                                                     id3_o,
                                                     id3_s,
                                                id1->encoding);
          next_comparand.identifier_5 = normalize_id(id5,
                                                     id5_o,
                                                     id5_s,
                                                id1->encoding);

          // Identifiers 3 and 5 have to be the same length.  But
          // but either, or both, can be figurative constants.  If
          // they are figurative constants, they start off with a
          // length of one.  We will expand figurative constants to
          // match the length of the other one:

          if( id3->attr & FIGCONST_MASK )
            {
            match_lengths(  next_comparand.identifier_3,
                            next_comparand.identifier_5);
            }
          else if( id5->attr & FIGCONST_MASK )
            {
            match_lengths(  next_comparand.identifier_5,
                            next_comparand.identifier_3);
            }

          next_comparand.alpha
            = normalized_id_1.the_characters.c_str();
          next_comparand.omega
            = next_comparand.alpha + normalized_id_1.length;

          normalized_operand normalized_id_4_before
            = normalize_id(id4_before, id4_before_o, id4_before_s, id1->encoding);
          normalized_operand normalized_id_4_after
            = normalize_id(id4_after, id4_after_o, id4_after_s, id1->encoding);

          next_comparand.alpha_it = normalized_id_1.the_vectorxxxx.begin();
          next_comparand.omega_it = normalized_id_1.the_vectorxxxx.end();

          the_alpha_and_omega(normalized_id_4_before,
                              normalized_id_4_after,
                              next_comparand.alpha,
                              next_comparand.omega,
                              next_comparand.alpha_it,
                              next_comparand.omega_it,
                              normalized_id_1.the_vectorxxxx.end());

          next_comparand.leading = true;
          next_comparand.leading_count = 0;
          next_comparand.first   = true;
          comparands.push_back(next_comparand);
          }
        }
      }
    }

  // We are now set up to accomplish the data flow described
  // in the language specification.  We loop through the
  // the character positions in normalized_id_1:
  char_it_c leftmost  = normalized_id_1.the_vectorxxxx.begin();
  char_it_c rightmost = leftmost + normalized_id_1.length;

  while( leftmost < rightmost )
    {
    // For each leftmost position, we check each of the
    // comparands

    for(size_t k=0; k<comparands.size(); k++)
      {
      if( leftmost < comparands[k].alpha_it )
        {
        // This can't be a match, because leftmost is
        // to the left of the comparand's alpha.
        continue;
        }
      if( leftmost + comparands[k].identifier_3.length
          > comparands[k].omega_it )
        {
        // This can't be a match, because the rightmost
        // character of the comparand falls to the right
        // of the comparand's omega
        continue;
        }
      // A match is theoretically possible, because all
      // the characters of the comparand fall between
      // alpha and omega:
      bool possible_match = true;
      if( comparands[k].operation != bound_characters_e)
        {
        for(size_t m=0; m<comparands[k].identifier_3.length; m++)
          {
          if( comparands[k].identifier_3.the_vectorxxxx[m]
              != leftmost[m] )
            {
            possible_match = false;
            break;
            }
          }
        }
      if( possible_match )
        {
        // The characters of the comparand match the
        // characters at leftmost.  See if further processing is
        // indicated:

        bool match = false;
        switch( comparands[k].operation )
          {
          case bound_characters_e:
            match = true;
            break;

          case bound_first_e:
            if( comparands[k].first )
              {
              match = true;
              comparands[k].first = false;
              }
            break;

          case bound_all_e:
            {
            // We have a match.
            match = true;
            break;
            }

          case bound_leading_e:
            {
            // We have a match at leftmost.  But we need to figure out if this
            // particular match is valid for LEADING.

            // Hang onto your hat.  This is delightfully clever.
            //
            // This position is LEADING if:
            //  1) .leading is still true
            //  2) leftmost / (length_of_comparand ) = current_count
            //
            // I get chills every time I look at that.
            if( comparands[k].leading )
              {
              // So far, so good.
              size_t count = (leftmost - comparands[k].alpha_it)
                              / comparands[k].identifier_3.length;
              if( count == comparands[k].leading_count )
                {
                // This means that the match here is just the latest of a
                // string of LEADING matches that started at .alpha
                comparands[k].leading_count += 1;
                match = true;
                }
              }
            break;
            }

          case bound_trailing_e:
            {
            // We have a match at leftmost.
            //
            // We want to know if this is a trailing match.  For that to be,
            // all of the possible matches from here to the omega have to be
            // true as well:

            if( (comparands[k].omega_it-leftmost)
                    % comparands[k].identifier_3.length == 0 )
              {
              // The remaining number of characters is correct for a match.
              // Keep checking.

              // Assume a match until we learn otherwise:
              match = true;
              char_it_c local_left = leftmost;
              local_left += comparands[k].identifier_3.length;
              while( local_left < comparands[k].omega_it )
                {
                for(size_t m=0; m<comparands[k].identifier_3.length; m++)
                  {
                  if( comparands[k].identifier_3.the_vectorxxxx[m]
                      != local_left[m] )
                    {
                    // We have a mismatched character, so no trailing match is
                    // possible
                    match = false;
                    break;
                    }
                  }
                local_left += comparands[k].identifier_3.length;
                }
              }
            break;
            }
          }
        if( match )
          {
          // We have a match at leftmost.  We need to
          // to replace the characters in normalized_id_1
          // with the characters from normalized_id_5

          size_t index = leftmost
                         - normalized_id_1.the_vectorxxxx.begin();
          for( size_t l = 0;
               l < comparands[k].identifier_5.length;
               l++ )
            {
            char ch = comparands[k].identifier_5.
                      the_vectorxxxx[l];
            normalized_id_1.the_vectorxxxx[index++] = ch;
            }
          // Adjust the leftmost pointer to point to
          // the rightmost character of the matched
          // string, keeping in mind that it will be
          // bumped again after we break out of the
          // k<pair_count loop:
          leftmost += comparands[k].identifier_3.length - 1;
          break;
          }
        }
      else
        {
        comparands[k].leading = false;
        }
      }
    leftmost += 1;
    }

  // Here is where we take the characters from normalized_id_1 and put them
  // back into identifier_1.

  charmap_t *charmap = __gg__get_charmap(id1->encoding);
  // Wastefully prefill id_1 with spaces in case the processing resulted in a
  // string shorter than the original.  (There is always the possiblity that
  // a UTF-8 or UTF-16 codeset pair got replaced with a single character.) Do
  // this before calling __gg__converter, because both mapped_character and
  // __gg__iconverter use the same static buffer.
  unsigned char *id1_data = id1->data + id1_o;
  charmap->memset(id1_data, charmap->mapped_character(ascii_space), id1_s);

  // We've been working in UTF32; we convert back to the original id1 encoding.
  size_t bytes_converted;
  const char *converted = __gg__iconverter( DEFAULT_32_ENCODING,
                                         id1->encoding,
                                         normalized_id_1.the_vectorxxxx.data(),
                                         normalized_id_1.length*width_of_utf32,
                                         &bytes_converted) ;
  // And move those characters into place in id_1:
  memcpy(id1_data,
         converted,
         std::min(bytes_converted, id1_s));
  return;
  }

static std::u32string
normalize_for_inspect_format_4(const cblc_field_t  *var,
                                size_t              var_offset,
                                size_t              var_size,
                                cbl_encoding_t      source_encoding)
  {
  std::u32string retval;
  if(var)
    {
    const charmap_t *charmap_var = __gg__get_charmap(source_encoding);
    charmap_t *charmap32 = __gg__get_charmap(DEFAULT_32_ENCODING);

    cbl_figconst_t figconst =
                      static_cast<cbl_figconst_t>(var->attr & FIGCONST_MASK);
    // We have a corner case to deal with:
    if( strcmp(var->name, "NULLS") == 0 )
      {
      figconst = null_value_e;
      }

    if( figconst )
      {
      // Build up an var_size array of figconst characters
      cbl_char_t figchar = '\0';
      switch( figconst )
        {
        case low_value_e   :
          figchar = charmap32->low_value_character();
          break;
        case zero_value_e  :
          figchar = charmap32->mapped_character(ascii_0);
          break;
        case space_value_e :
          figchar = charmap32->mapped_character(ascii_space);
          break;
        case quote_value_e :
          figchar = charmap32->quote_character();
          break;
        case high_value_e  :
          {
          if( __gg__high_value_character == DEFAULT_HIGH_VALUE_8 )
            {
            // See the comments where these constants are defined.
            if(charmap_var->stride() == 1)
              {
              if(charmap_var->is_like_ebcdic())
                {
                // This maps back to 0xFF in CP1140
                figchar = EBCDIC_HIGH_VALUE_32;
                }
              else
                {
                // This maps back to 0xFF in CP1252
                figchar = ASCII_HIGH_VALUE_32;
                }
              }
            else if(charmap_var->stride() == 2)
              {
              figchar = UTF16_HIGH_VALUE_32;
              }
            else
              {
              figchar = UTF32_HIGH_VALUE_32;
              }
            }
          else
            {
            figchar = charmap32->mapped_character(__gg__high_value_character);
            }
          break;
          }
        case null_value_e:
          break;
        default:
          figchar = '\0';
          abort();
          break;
        }
      retval.push_back(figchar);
      }
    else
      {
      // It's not a figurative constant, so convert var to UTF32.
      size_t converted_bytes;
      const char *converted = __gg__iconverter(
                              var->encoding,
                              DEFAULT_32_ENCODING,
                              var->data + var_offset,
                              var_size,
                              &converted_bytes);
      void *duped = __gg__memdup(converted, converted_bytes);
      for(size_t i=0; i<converted_bytes; i+=width_of_utf32)
        {
        cbl_char_t ch = charmap32->getch(duped, i);
        retval.push_back(ch);
        }
      free(duped);
      }
    }
  return retval;
  }

extern "C"
void
__gg__inspect_format_4( int backward,
                        cblc_field_t *input,              // identifier-1
                        size_t        input_offset,
                        size_t        input_size,
                  const cblc_field_t *original,           // id-6 / literal-4
                        size_t        original_offset,
                        size_t        original_size,
                  const cblc_field_t *replacement,        // id-7 / literal-5
                        size_t        replacement_offset,
                        size_t        replacement_size,
                  const cblc_field_t *after,              // id-4 / literal-2
                        size_t        after_offset,
                        size_t        after_size,
                  const cblc_field_t *before,             // id-4 / literal-2
                        size_t        before_offset,
                        size_t        before_size
                        )
  {
  // We need to cope with multiple encodings; the ISO specification says only
  // that identifier-1 and -3 through -n are display or national.

  // We will leave the input encoded as whatever it is, and we will convert the
  // others to match.

  // We also need to cope with anything except identifier-1 being a figurative
  // constant.

  cbl_figconst_t figconst_original =
                static_cast<cbl_figconst_t>(original->attr & FIGCONST_MASK);
  cbl_figconst_t figconst_replacement =
                static_cast<cbl_figconst_t>(replacement->attr & FIGCONST_MASK);
  int figswitch = (figconst_original ? 2 : 0) + (figconst_replacement ? 1 : 0);
  switch( figswitch )
    {
    case 0:
      // Neither are figconst; we leave the sizes alone
      break;
    case 1:
      // Only replacement is figconst, so we make its size -1
      // This will cause CONVERTING "ABC" TO ZERO to be the same as
      //                            CONVERTING "ABC" TO "000"
      replacement_size = (size_t)(-1LL);
      break;
    case 2:
      // Only original is figconst.  Set the size to one.  (This is necessary
      // because the size of NULL is eight, since NULL does double-duty as both
      // a character (this is a MicroFocus specification) and a pointer.
      original_size = 1;
      break;
    case 3:
      // Both are figconst
      replacement_size = original_size = 1;
      break;
    }

  // Because before and after can be figurative constant NULL, we have to make
  // sure that in such cases the size is 1:
  if(before && before_size && before->attr & FIGCONST_MASK)
    {
    before_size = 1;
    }
  if(after && after_size && after->attr & FIGCONST_MASK)
    {
    after_size = 1;
    }

  bool all = (replacement_size == (size_t)(-1LL));
  if( all )
    {
    // A replacement_size of -1 means that the statement is something like
    // INSPECT XYZ CONVERTING "abcxyz" to ALL "?"  That means replacement is
    // a single character.  We need to convert it to the target encoding.
    const charmap_t * charmap = __gg__get_charmap(input->encoding);
    replacement_size = charmap->stride();
    }

  std::u32string str_input       = normalize_for_inspect_format_4(input      , input_offset      , input_size      , input->encoding);
  std::u32string str_original    = normalize_for_inspect_format_4(original   , original_offset   , original_size   , input->encoding);
  std::u32string str_replacement = normalize_for_inspect_format_4(replacement, replacement_offset, replacement_size, input->encoding);
  std::u32string str_after       = normalize_for_inspect_format_4(after      , after_offset      , after_size      , input->encoding);
  std::u32string str_before      = normalize_for_inspect_format_4(before     , before_offset     , before_size     , input->encoding);

  if( all )
    {
    // We now expand the single-character replacement to be the same length as
    // original.
    cbl_char_t ch = str_replacement[0];
    str_replacement.clear();
    for(size_t i=0; i<str_original.size(); i++)
      {
      str_replacement.push_back(ch);
      }
    }

  // Use a  map to make this O(N), rather than an O(N-squared),
  // computational complexity
  std::unordered_map<cbl_char_t, cbl_char_t>map;
  typedef std::unordered_map<cbl_char_t, cbl_char_t>::const_iterator map_it_t ;

  // The rule is, if the same character appears more than once in the
  // original (which is identifier-6), then the first occurrence of the
  // matching character in replacement is used.  So, we create the map
  // backwards.  The one closest to zero will win.
  for(size_t i=str_original.size()-1; i<str_original.size(); i--)
    {
    map[str_original[i]] = str_replacement[i];
    }

  size_t leftmost_i;   // Leftmost index to replace at.
  size_t rightmost_i;  // Rightmost+1 index to replace at.

  if( !backward )
    {
    // This is a forward conversion.  We look for the first instance
    // of str_after from the left.  And then we look for the first instance
    // of str_before after that.  When there is no str_before, we move the
    // rightmost limit to the end of str_input, as if there were no BEFORE
    // phrase:

    if( str_after.empty() )
      {
      // There is no AFTER phrase, so we start from the left.
      leftmost_i = 0;
      }
    else
      {
      size_t nfound = str_input.find(str_after);
      if( nfound != std::u32string::npos )
        {
        // Move the left limit to one character past the found element
        leftmost_i = nfound + str_after.size();
        }
      else
        {
        // We didn't find the after phrase, so we move the left limit to the
        // end of input, which means nothing will be replaced
        leftmost_i = str_input.size();
        }
      }

    // At this point, leftmost_i has been set to something.  Look for the
    // BEFORE phrase somewhere to the right of it:

    if( str_before.empty() )
      {
      // There is no BEFORE phrase, so set rightmost to the end of the input
      rightmost_i = str_input.size();
      }
    else
      {
      // Look for BEFORE to the right of leftmost_i:
      size_t nfound = str_input.find(str_before, leftmost_i);
      if( nfound != std::u32string::npos )
        {
        // We found the BEFORE phrase.
        rightmost_i = nfound;
        }
      else
        {
        // We didn't find the BEFORE phrase; IOS says to treat this situation
        // as if there were no BEFORE phrase
        rightmost_i = str_input.size();
        }
      }
    }
  else
    {
    // We are doing a BACKWARD conversion.  So, we look for the AFTER phrase
    // and use that to establish the rightmost limit.  And we look for the
    // BEFORE to the left of AFTER phrase and use that to establish the
    // leftmost limit

    if( str_after.empty() )
      {
      // There is no AFTER phrase, so we set the rightmost limit to the end
      // of the input:
      rightmost_i = str_input.size();
      }
    else
      {
      // Start from the right and look for AFTER
      size_t nfound = str_input.rfind(str_after, str_input.size());
      if( nfound != std::u32string::npos )
        {
        // We found str_after, so its location becomes rightmost
        rightmost_i = nfound;
        }
      else
        {
        // We didn't find str_after, so we move rightmost all the way to the
        // left, so that nothing will ever be found.
        rightmost_i = 0;
        }
      }
    // rightmost_i has been established, so now look for BEFORE to the left
    // of it
    if( str_before.empty() )
      {
      // There is no str_before, so the left limit is all the way to the left
      leftmost_i = 0;
      }
    else
      {
      size_t nfound = str_input.rfind(str_before, rightmost_i);
      if( nfound != std::u32string::npos )
        {
        // We found BEFORE, so we put the left limit just to the right of
        // where we found it:
        leftmost_i = nfound + str_before.size();
        }
      else
        {
        // Not finding the BEFORE phrase is the same as the BEFORE phrase
        // not having been specified:
        leftmost_i = 0;
        }
      }
    }
  // leftmost_i and rightmost_i have been established.  Do the conversion of
  // characters inside those limits:
  for(size_t i=leftmost_i; i<rightmost_i; i++)
    {
    cbl_char_t ch = str_input[i];
    map_it_t cvt = map.find(ch);
    if( cvt != map.end() )
      {
      str_input[i] = cvt->second;
      }
    }

  // We now take the converted str_input, and put it back into id_1:

  size_t bytes_converted;
  const char *converted = __gg__iconverter(DEFAULT_32_ENCODING,
                                           input->encoding,
                                           str_input.data(),
                                           str_input.size()*width_of_utf32,
                                           &bytes_converted) ;

  // And move those characters into place in input:
  memcpy(input->data + input_offset,
         converted,
         std::min(bytes_converted, input_size));
  }

static void
move_string(cblc_field_t *field,
                size_t offset,
                size_t length,
                const char *from,
                cbl_encoding_t src_encoding,
                size_t strlen_from = (size_t)(-1) )
  {
  bool moved = true;

  if( strlen_from == (size_t)(-1) )
    {
    strlen_from = strlen(from);
    }

  switch(field->type)
    {
    case FldGroup:
    case FldAlphanumeric:
    case FldAlphaEdited:
      {
      char *to = reinterpret_cast<char *>(field->data + offset);
      size_t dest_length = length ? length : field->capacity;
      size_t source_length = strlen_from;

      // We need to convert the source string to the destination encoding:
      size_t charsout;
      const char *converted = __gg__iconverter(src_encoding,
                                               field->encoding,
                                               from,
                                               source_length,
                                               &charsout);

      size_t count = std::min(dest_length, source_length);
      if( source_length >= dest_length )
        {
        // We have more source characters than places to put them
        if( field->attr & rjust_e )
          {
          // Destination is right-justified, so we
          // discard the leading source characters:
          memmove(to,
                  converted + (source_length - count),
                  count);
          }
        else
          {
          // Destination is right-justified, so we
          // discard the trailing source characters:
          memmove(to,
                  converted,
                  count);
          }
        }
      else
        {
        // We have too few source characters to fill the destination.
        if( field->attr & rjust_e )
          {
          // The destination is right-justified, and the source is an
          // ordinary string too short to fill it.  So, we space-fill
          // the leading characters.
          memmove(to + (dest_length-count),
                  converted,
                  count);
          // Get the charmap after the move, because it can mess with the
          // static 'to' buffer.
          charmap_t *charmap = __gg__get_charmap(field->encoding);
          charmap->memset(to, charmap->mapped_character(ascii_space), dest_length-count);
          }
        else
          {
          // The destination is left-justified
          // We do the move first, in case this is an overlapping move
          // involving characters that will be space-filled
          memmove(to,
                  converted,
                  count);
          charmap_t *charmap = __gg__get_charmap(field->encoding);
          memset( to + count,
                  charmap->mapped_character(ascii_space),
                  dest_length-count);
          }
        }
      break;
      }

    case FldNumericBinary:
    case FldPacked:
    case FldNumericDisplay:
    case FldNumericEdited:
    case FldNumericBin5:
    case FldIndex:
      {
      // We are starting with a string, and setting it to a numerical
      // target.
      int rdigits;
      __int128 value = __gg__dirty_to_binary(from,
                                             src_encoding,
                                             strlen_from,
                                             &rdigits);
      __gg__int128_to_qualified_field(field,
                                      offset,
                                      length,
                                      value,
                                      rdigits,
                                      truncation_e,
                                      NULL);
      break;
      }

    default:
      moved = false;
      break;
    }
  if( !moved )
    {
    fprintf(stderr, "%s() %s:%d -- We were unable move a string to "
            "field type %d\n",
            __func__, __FILE__, __LINE__,
            field->type);
    abort();
    }
  }

static char *
brute_force_trim(char *str, cbl_encoding_t encoding)
  {
  charmap_t *charmap = __gg__get_charmap(encoding);
  int stride = charmap->stride();

  char *retval = str;

  while(   charmap->getch(retval, size_t(0))
        == charmap->mapped_character(ascii_space) )
    {
    retval += stride;
    }
  char *p = retval + strlen(retval)-stride;
  while(    p > retval
        && (   charmap->getch(p, size_t(0))
            == charmap->mapped_character(ascii_space)) )
    {
    charmap->putch(NULLCH, p, size_t(0));
    p -= stride;
    }
  return retval;
  }

extern "C"
int
__gg__string(const size_t integers[])
  {
  // The first integer is the count of identifier-2 values.  Call it N
  // The following N integers are the counts of each of the identifier-1 values,
  // one for each identifier-1.  Call them M.

  // The first refer is the target
  // The second refer is the pointer
  // The third refer is identifier-2 for N1
  // That's followed by M1 identifier-1 values
  // That's followed by identifier2 for N2
  // And so on

  cblc_field_t **ref   = __gg__treeplet_1f;
  const size_t  *ref_o = __gg__treeplet_1o;
  const size_t  *ref_s = __gg__treeplet_1s;

  static const int INDEX_OF_POINTER = 1;

  size_t index_cblc = 0 ;

  // Pick up the target
  const cblc_field_t *tgt = ref[index_cblc];

  // Pick up the target encoding, which according to the ISO specification
  // controls all the parameters.
  cbl_encoding_t tgt_encoding = tgt->encoding;
  charmap_t *charmap = __gg__get_charmap(tgt_encoding);
  int stride = charmap->stride();

  // Pick up the rest of the parameters
  size_t tgt_o              = ref_o[index_cblc];
  size_t tgt_s              = ref_s[index_cblc];
  index_cblc += 1;

  char  *dest         = reinterpret_cast<char *>(tgt->data + tgt_o);
  size_t dest_length = tgt_s/stride;

  // Skip over the index of POINTER:
  index_cblc += 1;

  // Pick up the pointer, if any
  size_t pointer = 0;
  int overflow = 0;
  if( ref[INDEX_OF_POINTER] )
    {
    int rdigits;
    int p  = (size_t)__gg__binary_value_from_qualified_field(
                                                    &rdigits,
                                                    ref  [INDEX_OF_POINTER],
                                                    ref_o[INDEX_OF_POINTER],
                                                    ref_s[INDEX_OF_POINTER]
                                                    );
    if( p<0 )
      {
      overflow = 1;
      }
    pointer = p - 1;
    }

  // Make sure that the destination pointer is within the destination
  if( pointer < dest_length )
    {
    // We are go for looping through identifier-2 values:

    size_t index_int  = 0;

    // Pick up the number of identifier-2 values
    size_t N = integers[index_int++];

    for( size_t i=0; i<N; i++ )
      {
      // Pick up the number of M identifier-1 values for this list of
      // identifier-2 values:
      size_t M = integers[index_int++];

      // Pick up the identifier_2 DELIMITED BY value
      std::u32string str_id2 = normalize_for_inspect_format_4(
                                                        ref[index_cblc],
                                                        ref_o[index_cblc],
                                                        ref_s[index_cblc],
                                                        tgt_encoding);
      index_cblc += 1;

      for(size_t j=0; j<M; j++)
        {
        // Pick up the next id-1 source string for the current id-2 delimiter
        std::u32string str_id1 = normalize_for_inspect_format_4(
                                                        ref[index_cblc],
                                                        ref_o[index_cblc],
                                                        ref_s[index_cblc],
                                                        tgt_encoding);
        index_cblc += 1;

        size_t nfound;
        if( str_id2.size() == 0 )
          {
          // No given delimiter means DELIMITED BY SIZE
          nfound = str_id1.size();
          }
        else
          {
          // We have an id2, so we look for it inside id1
          nfound = str_id1.find(str_id2);
          if( nfound == std::u32string::npos )
            {
            nfound = str_id1.size();
            }
          }


          {
          // We have found id2 inside id1 at location nfound.

          // Convert the UTF32 to the original encoding:
          size_t bytes_converted;
          char *converted = __gg__miconverter(DEFAULT_32_ENCODING,
                                              tgt_encoding,
                                              str_id1.data(),
                                              nfound*width_of_utf32,
                                              &bytes_converted );
          size_t k = 0;
          while(k < nfound)
            {
            if( pointer >= dest_length )
              {
              overflow = 1;
              break;
              }
            cbl_char_t ch = charmap->getch(converted, k*stride);
            charmap->putch(ch, dest, pointer*stride);
            k += 1;
            pointer += 1;
            }
          free(converted);
          }
        if( overflow )
          {
          break;
          }
        }
      }

    // Update the pointer, if there is one
    if( ref[INDEX_OF_POINTER] )
      {
      __gg__int128_to_qualified_field(ref  [INDEX_OF_POINTER],
                                      ref_o[INDEX_OF_POINTER],
                                      ref_s[INDEX_OF_POINTER],
                                      (__int128)(pointer+1),
                                      0,
                                      truncation_e,
                                      NULL );
      }
    }
  else
    {
    // The initial pointer is not inside the destination
    overflow = 1;
    }

  return overflow;
  }

static
void
display_both(cblc_field_t  *field,
             unsigned char *qual_data,
             size_t         qual_size,
             int            flags,
             int            file_descriptor,
             int            advance )
  {
  static size_t display_string_size = MINIMUM_ALLOCATION_SIZE;
  static char *display_string = static_cast<char *>(malloc(display_string_size));

  cbl_encoding_t encoding = format_for_display_internal(
                                            &display_string,
                                            &display_string_size,
                                            field,
                                            qual_data,
                                            qual_size,
                                            !!(flags & REFER_T_ADDRESS_OF) );

  cbl_encoding_t encout = __gg__console_encoding;

  // It can be the case in COBOL programs that a variable set to HIGH-VALUE is
  // displayed.  In CP1252, the result for 0xFF is a y-with diaresis.

  // In EBCDIC CP1140, however, the 0xFF character is non-printing.  It's my
  // opinion that's protentially confusing, especially when debugging.

  // So, I am going to go out on a limb.  When the character set is known to be
  // EBCDIC-ish, I am going to scan the output string and convert 0xFF to 0xDF.

  // In this way both ASCII and EBCDIC displays of HIGH-VALUE will be the same.

  // There are valid arguments against doing this.  But when I was doing some
  // debugging, I found the EBCDIC behavior of displaying nothing for
  // HIGH-VALUE to be more astonishing than printing a y-with-diaresis.  There
  // is, of course, the potential for confusing a real y-with-diaresis with a
  // a HIGH-VALUE character.  But it is my opinion that those will be resolved
  // by examining the context.

  const charmap_t *charmap = __gg__get_charmap(encoding);
  if( charmap->is_like_ebcdic() )
    {
    for(size_t i=0; i<qual_size; i++)
      {
      if( (unsigned char)display_string[i] == (unsigned char)(0xFF) )
        {
        display_string[i] = 0xDF;
        }
      }
    }

  size_t conversion_length = strlen(display_string);
  if( charmap->stride() != 1 )
    {
    conversion_length = qual_size;
    }

  size_t outlength;
  const char *converted = __gg__iconverter( encoding,
                                            encout,
                                            display_string,
                                            conversion_length,
                                            &outlength);
  // Trim off the trailing null, if present.
  outlength = strlen(converted);
  write(file_descriptor,
        converted,
        outlength);

  if( advance )
    {
    write( file_descriptor,
           "\n",
           1);
    }
  }

extern "C"
void
__gg__display(    cblc_field_t *field,
                  size_t offset,
                  size_t size,
                  int file_descriptor,
                  int advance )
  {
  display_both( field,
                field->data + offset,
                size,
                0,
                file_descriptor,
                advance);
  }

extern "C"
void
__gg__display_clean(cblc_field_t *field,
                    int file_descriptor,
                    int advance )
  {
  display_both( field,
                field->data,
                field->capacity,
                0,
                file_descriptor,
                advance);
  }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-result"

extern "C"
void
__gg__display_string( int            file_descriptor,
                      cbl_encoding_t encoding,
                const char          *str,
                      size_t         length,
                      int            advance )
  {
  cbl_encoding_t encout = __gg__console_encoding;

  size_t outlength;
  const char *converted = __gg__iconverter( encoding,
                                          encout,
                                          str,
                                          length,
                                          &outlength);
  // Trim off trailing NUL, if present.
  outlength = strlen(converted);
  write( file_descriptor,
         converted,
         outlength);
  if( advance )
    {
    write( file_descriptor,
           "\n",
           1);
    }
  }

static
char *
mangler_core(const char *s, const char *eos)
  {
  // The caller needs to be aware that the return value is a static character
  // array.  Program accordingly.

  // This is the run-time version of the code in cobol1.cc routine that mangles
  // cobol names into workable form.  The logic here has to match the logic
  // there so that calls work.

  static char cobol_name[1024];

  while( s < eos && *s == ascii_space )
    {
    s += 1;
    }
  while( s < eos && *(eos-1) == ascii_space )
    {
    eos -= 1;
    }

  char *d = cobol_name;
  if( s[0] >= ascii_0 && s[0] <= ascii_9 )
    {
    *d++ = '_';
    }

  while(s < eos)
    {
    int ch = *s++;
    if( ch == ascii_hyphen )
      {
      *d++ = ascii_dollar_sign;
      }
    else
      {
      *d++ = tolower((unsigned char)ch);
      }
    }
  *d++ = NULLCH;

  return cobol_name;
  }

static
char *
not_mangled_core(const char *s, const char *eos)
  {
  const char *s1 = s;
  const char *s2 = eos;
  bool has_dash = false;

  while( s < eos && *s == ascii_space )
    {
    s += 1;
    }
  while( s < eos && *(eos-1) == ascii_space )
    {
    eos -= 1;
    }

  if( s[0] >= ascii_0 && s[0] <= ascii_9 )
    {
    has_dash = true;
    }
  else
    {
    while(s < eos)
      {
      int ch = *s++;
      if( ch == ascii_hyphen )
        {
        has_dash = true;
        }
      }
    }

  if( has_dash )
    {
    return mangler_core(s1, s2);
    }

  return (char *)s1;
  }

extern "C"
void
__gg__accept(   enum special_name_t special_e,
                cblc_field_t *field,
                size_t offset,
                size_t length)
  {
  int file_descriptor = 0;    // Default to stdin

  size_t max_chars = length ? length : field->capacity;

  if( special_e == CONSOLE_e )
    {
    // Welcome to the land of possibly screwball assumptions.  If reading
    // from CONSOLE/stdin it's possible that the target variable is
    // a NumericBinary of length 4, which can hold a 10-digit number.  So,
    // we need room to accept the characters, which will later on be converted
    // to a binary value.

    // But SYSIN and SYSIPT seem to require that characters be read until the
    // size of the target variable is satisfied, which implies further that
    // the target must be alphanumeric.

    // What reality will ultimately offer is unknown to me.  But I'm doing
    // the best I can with what I've got, and, right now, this is what
    // I've got.
    if( max_chars < 64 )
      {
      // Set a floor for the length of the buffer.  This will let us cope
      // with, say, a four-byte binary value that can hold ten digits
      max_chars = 64;
      }
    }

  char *buffer = static_cast<char *>(malloc(max_chars+1));
  massert(buffer);
  memset(buffer, ascii_space, max_chars);
  buffer[max_chars] = NULLCH;
  size_t i = 0;

  for(;;)
    {
    char ch;
    ssize_t bytes_read = read(file_descriptor, &ch, 1);
    if( bytes_read <= 0 )
      {
      // Error or end-of-file, so give up
      break;
      }

    if( ch == '\n' )
      {
      // End-of-line
      if( special_e == CONSOLE_e )
        {
        // When reading from the console, a newline means that the
        // typist pressed ENTER/RETURN, and the input is done.  This is
        // also the case even when stdin was redirected from a file or
        // another process
        break;
        }
      else
        {
        // But if SYSIN_e or SYSIPT_e was specified, we are emulating
        // the universe of punched cards, so we just keep reading in
        // characters until we have read in max_chars.  We found it
        // necessary to implement ACCEPT in this fashion to get the
        // NIST test suite to work.

        // Note that in both cases, we keep reading until we hit
        // an actual newline or end-of-file

        if( i >= max_chars )
          {
          break;
          }
        continue;
        }
      }
    if(i < max_chars)
      {
      buffer[i++] = ch;
      }
    }

  switch(field->type)
    {
    case FldGroup :
    case FldAlphanumeric :
    case FldAlphaEdited :
      move_string(field,
                  offset,
                  length,
                  buffer,
                  __gg__console_encoding,
                  strlen(buffer));
      break;

    case FldNumericDisplay:
      {
      // In the NIST tests, feeding ten digits 0123456789 into a
      // PIC 9(9) results in a nine-digit 012345678 rather than our
      // default 123456789

      int digit_count = 0;
      char *p = buffer;
      while(*p && digit_count < field->digits)
        {
        if( *p == __gg__decimal_point )
          {
          p += 1;
          continue;
          }

        switch(*p)
          {
          case ascii_0:
          case ascii_1:
          case ascii_2:
          case ascii_3:
          case ascii_4:
          case ascii_5:
          case ascii_6:
          case ascii_7:
          case ascii_8:
          case ascii_9:
            p += 1;
            digit_count += 1;
            continue;
            break;
          case ascii_minus:
          case ascii_plus:
            p += 1;
            continue;
            break;
          default:
            goto we_are_done;
            break;
          }
        }
we_are_done:
      *p = NULLCH;

      int rdigits;
      __int128 value = __gg__dirty_to_binary(buffer,
                                             __gg__console_encoding,
                                             (int)i,
                                             &rdigits);
      __gg__int128_to_qualified_field(field,
                                      offset,
                                      length,
                                      value,
                                      rdigits,
                                      truncation_e,
                                      NULL);
      break;
      }

    default:
      {
      int rdigits;

      __int128 value = __gg__dirty_to_binary_source( buffer,
                       (int)i,
                       &rdigits);

      __gg__int128_to_qualified_field(field,
                                      offset,
                                      length,
                                      value,
                                      rdigits,
                                      truncation_e,
                                      NULL);
      break;
      }
    }
#if LOGGING_FOR_TESTING
  if( isatty(file_descriptor) )
    {
    auto p = strchr(buffer, '\0');
    *p = '\n';
    write(1, buffer, (p - buffer) + 1);
    }
#endif
  free(buffer);
  }

extern "C"
__int128
__gg__binary_value_from_field(   int *rdigits,
                                 cblc_field_t *var)
  {
  return  get_binary_value_local(  rdigits,
                                   var,
                                   var->data,
                                   var->capacity);
  }

extern "C"
__int128
__gg__binary_value_from_qualified_field(int          *rdigits,
                                        const cblc_field_t *var,
                                        size_t        offset,
                                        size_t        size)
  {
  return  get_binary_value_local(  rdigits,
                                   var,
                                   var->data + offset,
                                   size);
  }

extern "C"
GCOB_FP128
__gg__float128_from_field( cblc_field_t *field )
  {
  GCOB_FP128 retval=0;
  if( field->type == FldFloat || field->type == FldLiteralN )
    {
    retval = get_float128(field, field->data);
    }
  else
    {
    int rdigits;
    retval = (GCOB_FP128)__gg__binary_value_from_field(&rdigits, field);
    if( rdigits )
      {
      retval /= (GCOB_FP128)__gg__power_of_ten(rdigits);
      }
    }
  return retval;
  }

extern "C"
GCOB_FP128
__gg__float128_from_qualified_field(const cblc_field_t *field, size_t offset, size_t size)
  {
  GCOB_FP128 retval=0;
  if( field->type == FldFloat || field->type == FldLiteralN )
    {
    retval = get_float128(field, field->data+offset);
    }
  else
    {
    int rdigits;
    retval = (GCOB_FP128)__gg__binary_value_from_qualified_field(&rdigits, field, offset, size);
    if( rdigits )
      {
      retval /= (GCOB_FP128)__gg__power_of_ten(rdigits);
      }
    }
  return retval;
  }

extern "C"
__int128
__gg__integer_from_qualified_field( cblc_field_t *var,
                                    size_t var_offset,
                                    size_t var_size)
  {
  // This is useful when a temporary value with some number of rdigits
  // is passed when the value is known to be an integer

  int rdigits;
  __int128 retval = get_binary_value_local(  &rdigits,
                                             var,
                                             var->data + var_offset,
                                             var_size);
  if( rdigits )
    {
    retval /= __gg__power_of_ten(rdigits);
    }
  return retval;
  }

extern "C"
void
__gg__int128_to_field(cblc_field_t   *tgt,
                      __int128        value,
                      int             source_rdigits,
                      enum cbl_round_t  rounded,
                      int            *compute_error)
  {
  int128_to_field(tgt,
                  tgt->data,
                  tgt->capacity,
                  value,
                  source_rdigits,
                  rounded,
                  compute_error);
  }

extern "C"
void
__gg__int128_to_qualified_field(cblc_field_t   *tgt,
                                size_t          offset,
                                size_t          length,
                                __int128        value,
                                int             source_rdigits,
                                enum cbl_round_t  rounded,
                                int            *compute_error)
  {
  int128_to_field(tgt,
                  tgt->data + offset,
                  length ? length : tgt->capacity,
                  value,
                  source_rdigits,
                  rounded,
                  compute_error);
  }

static __int128
float128_to_int128( int                *rdigits,
                    const cblc_field_t *field,
                    GCOB_FP128          value,
                    cbl_round_t         rounded,
                    int                *compute_error)
  {
  __int128 retval = 0;
  if( value == INFINITY )
    {
    *compute_error = compute_error_overflow;
    }
  else if( value == NAN )
    {
    *compute_error = compute_error_underflow;
    }
  else
    {
    // Our mission is to take a 128-bit floating point value and convert it
    // to a 128-bit number.  If we can't, we flag it appropriately.

    if( field->attr & intermediate_e )
      {
      // Our target doesn't have a fixed number of rdigits.  We will look at the
      // value, and from that calculate the maximum number of rdigits we can
      // get away with.

      // Calculate the number of digits to the left of the decimal point:
      int digits = (int)(FP128_FUNC(floor)(FP128_FUNC(log)(FP128_FUNC(fabs)(value)))+1);

      // Make sure it is not a negative number
      digits = std::max(0, digits);

      // From that we can calculate the number of rdigits
      *rdigits = MAX_FIXED_POINT_DIGITS - digits;
      }
    else
      {
      // Our target has a fixed number of rdigits:
      *rdigits = field->rdigits;
      }

    // We now multiply our value by 10**rdigits, in order to make the
    // floating-point value have the same magnitude as our target __int128

    value *= FP128_FUNC(pow)(GCOB_FP128_LITERAL (10.0), (GCOB_FP128)(*rdigits));

    // We are ready to cast value to an __int128.  But this value could be
    // too large to fit, which is an error condition we want to flag:

    if( FP128_FUNC(fabs)(value) >= GCOB_FP128_LITERAL (1.0E38) )
      {
      *compute_error = compute_error_overflow;
      }
    else
      {
      retval = f128_to_i128_rounded(rounded, value, compute_error);
      }
    }

  return retval;
  }

static void
float128_to_location( cblc_field_t   *tgt,
                      unsigned char  *data,
                      size_t          size,
                      GCOB_FP128       value,
                      enum cbl_round_t  rounded,
                      int            *compute_error)
  {
  switch(tgt->type)
    {
    case FldFloat:
      {
      switch(tgt->capacity)
        {
        case 4:
          if(    FP128_FUNC(fabs)(value) == (GCOB_FP128)INFINITY
              || FP128_FUNC(fabs)(value) > GCOB_FP128_LITERAL (3.4028235E38) )
            {
            if( compute_error )
              {
              *compute_error |= compute_error_overflow;
              }
            if( value < 0 )
              {
              *PTRCAST(float, data) = -INFINITY;
              }
            else
              {
              *PTRCAST(float, data) = INFINITY;
              }
            }
          else
            {
            *PTRCAST(float, data) = static_cast<float>(value);
            }
          break;

        case 8:
          if(    FP128_FUNC(fabs)(value) == (GCOB_FP128)INFINITY
              || FP128_FUNC(fabs)(value) > GCOB_FP128_LITERAL (1.7976931348623157E308) )
            {
            if( compute_error )
              {
              *compute_error |= compute_error_overflow;
              }
            if( value < 0 )
              {
              *PTRCAST(double, data) = -INFINITY;
              }
            else
              {
              *PTRCAST(double, data) = INFINITY;
              }
            }
          else
            {
            *PTRCAST(double, data) = static_cast<double>(value);
            }
          break;

        case 16:
          if( FP128_FUNC(fabs)(value) == (GCOB_FP128)INFINITY )
            {
            if( compute_error )
              {
              *compute_error |= compute_error_overflow;
              }
            }

          //*(_Float128 *)(data) = value;
          memcpy(data, &value, 16);
          break;
        }
      break;
      }

    default:
      {
      if( compute_error )
        {
        int digits;
        if( tgt->attr & intermediate_e )
          {
          digits = MAX_FIXED_POINT_DIGITS;
          }
        else
          {
          digits = tgt->digits;
          }

        GCOB_FP128 maximum;

        if( digits )
          {
          maximum = __gg__power_of_ten(digits);
          }

        // When digits is zero, this is a binary value without a PICTURE string.
        // we don't truncate in that case
        if( digits && FP128_FUNC(fabs)(value) >= maximum )
          {
          *compute_error |= compute_error_truncate;
          }
        }

      int rdigits=0;  // Initialized to quiet a compiler warning.
      __int128 val128 = float128_to_int128( &rdigits,
                                            tgt,
                                            value,
                                            rounded,
                                            compute_error);
      int128_to_field(tgt,
                      data,
                      size,
                      val128,
                      rdigits,
                      rounded,
                      compute_error);
      break;
      }
    }
  }


extern "C"
void
__gg__float128_to_field(cblc_field_t   *tgt,
                        GCOB_FP128       value,
                        enum cbl_round_t  rounded,
                        int            *compute_error)
  {
  float128_to_location( tgt,
                        tgt->data,
                        tgt->capacity,
                        value,
                        rounded,
                        compute_error);
  }

extern "C"
void
__gg__float128_to_qualified_field(cblc_field_t   *tgt,
                                  size_t          tgt_offset,
                                  GCOB_FP128       value,
                                  enum cbl_round_t  rounded,
                                  int            *compute_error)
  {
  float128_to_location( tgt,
                        tgt->data + tgt_offset,
                        tgt->capacity,
                        value,
                        rounded,
                        compute_error);
  }

extern "C"
bool
__gg__bitop(cblc_field_t *a,
            bitop_t op,
            size_t bitmask)
  {
  bool retval = false;
  int rdigits;
  __int128 value = __gg__binary_value_from_field(&rdigits, a);
  switch(op)
    {
    case bit_set_op:      // set bits on
      value |= bitmask;
      __gg__int128_to_field(a,
                            value,
                            0,
                            truncation_e,
                            NULL);
      break;

    case bit_clear_op:    // set bits off
      value &= ~bitmask;
      __gg__int128_to_field(a,
                            value,
                            0,
                            truncation_e,
                            NULL);
      break;

    case bit_on_op:       // true if any bitmask bit is on
      retval = bitmask & value;
      break;

    case bit_off_op:      // true if any bitmask bit is off
      retval = bitmask & ~value;
      break;

    default:
      __gg__abort("__gg__bitop() unknown operation code");
      break;
    }

  return retval;
  }

extern "C"
void
__gg__bitwise_op( cblc_field_t *tgt,
                  cblc_field_t *a,
                  bitop_t op,
                  size_t bitmask)
  {
  int rdigits;
  __int128 value = __gg__binary_value_from_field(&rdigits, a);
  switch(op)
    {
    case bit_and_op:
      value &= bitmask;
      __gg__int128_to_field(tgt,
                            value,
                            0,
                            truncation_e,
                            NULL);
      break;

    case bit_or_op:
      value |= bitmask;
      __gg__int128_to_field(tgt,
                            value,
                            0,
                            truncation_e,
                            NULL);
      break;

    case bit_xor_op:
      value ^= bitmask;
      __gg__int128_to_field(tgt,
                            value,
                            0,
                            truncation_e,
                            NULL);
      break;

    default:
      __gg__abort("__gg__bitwise_op() unknown operation code");
      break;
    }
  }

extern "C"
void
__gg__onetime_initialization( )
  {
  // This routine gets called once per executable before anything else runs

  // We need to establish the initial value of the UPSI-1 switch register We
  // are using IBM's conventions:
  // https://www.ibm.com/docs/en/zvse/6.2?topic=SSB27H_6.2.0/fa2sf_communicate_appl_progs_via_job_control.html
  // UPSI 10000110 means that bits 0, 5, and 6 are on, which means that SW-0,
  // SW-5, and SW-6 are on.

  __int128 value = 0;
  __int128 bit = 1;
  char ach[129];
  memset(ach, 0, sizeof(ach));
  const char *p = getenv("UPSI");
  if( p )
    {
    snprintf(ach, sizeof(ach), "%s", p);
    p = ach;
    while(*p)
      {
      if( *p++ == ascii_1 )
        {
        value |= bit;
        }
      bit <<= 1 ;
      }
    }
  __gg__data_upsi_0[0] = (value>>0) & 0xFF;
  __gg__data_upsi_0[1] = (value>>8) & 0xFF;
  }

static int
is_numeric_edited_numeric(cblc_field_t *, size_t, size_t )
  {
  fprintf(stderr, "We don't know how to see if numeric-edited is numeric\n");
  abort();
  }

static int
is_numeric_display_numeric(cblc_field_t *field, size_t offset, size_t size)
  {
  charmap_t *charmap = __gg__get_charmap(field->encoding);
  int stride = charmap->stride();

  int retval = 1;
  bool signable = !!(field->attr & signable_e);
  bool leading  = !!(field->attr & leading_e);
  bool separate = !!(field->attr & separate_e);

  char *digits   = reinterpret_cast<char *>(field->data + offset);
  char *digits_e = digits + size;

  if( leading && separate && signable )
    {
    // First character must be +/-
    cbl_char_t ch = charmap->getch(digits, size_t(0));
    if(     digits < digits_e
        || (   ch != charmap->mapped_character(ascii_plus)
            && ch !=  charmap->mapped_character(ascii_minus)) )
      {
      retval = 0;
      }
    digits += stride;
    }

  if( !leading && separate && signable )
    {
    // Last character must be +/-
    digits_e -= stride;
    cbl_char_t ch = charmap->getch(digits_e, size_t(0));
    if(     digits < digits_e
        || (   ch != charmap->mapped_character(ascii_plus)
            && ch !=  charmap->mapped_character(ascii_minus)) )
      {
      retval = 0;
      }
    }

  if( leading && !separate && signable )
    {
    // The first character is allowed to have a sign bit. Let's make sure that
    // making that first digit unsigned leaves us with zero through nine:
    if( digits < digits_e )
      {
      cbl_char_t first_char = charmap->getch(digits, size_t(0));
      first_char = charmap->set_digit_negative(first_char, false);
      if(  first_char < charmap->mapped_character(ascii_0)
        || first_char > charmap->mapped_character(ascii_9))
        {
        retval = 0;
        }
      }
    digits += 1;
    }

  if( !leading && !separate && signable )
    {
    // The final character is allowed to have a sign bit.
    if( digits < digits_e )
      {
      digits_e -= 1;
      cbl_char_t final_char = charmap->getch(digits, size_t(0));
      final_char = charmap->set_digit_negative(final_char, false);
      if(   final_char<charmap->mapped_character(ascii_0)
         || final_char>charmap->mapped_character(ascii_9) )
        {
        retval = 0;
        }
      }
    }

  // all remaining characters are supposed to be zero through nine
  while( digits < digits_e )
    {
    if(     (unsigned char)(*digits)<charmap->mapped_character(ascii_0)
        ||  (unsigned char)(*digits)>charmap->mapped_character(ascii_9) )
      {
      retval = 0;
      break;
      }
    digits += 1;
    }
  return retval;
  }

static int
is_packed_numeric(const cblc_field_t *field, size_t offset, size_t size)
  {
  int retval = 1;
  bool is_comp6 = !!(field->attr&packed_no_sign_e);
  int digits = field->digits;
  bool signable = !!(field->attr & signable_e);
  const unsigned char *bytes   = field->data + offset;

  int nybble   = 0;
  int nybble_e = nybble + digits;
  unsigned char should_be_zero = 0;
  if( is_comp6 )
    {
    // This is packed decimal with no sign nybble at the end
    if( digits & 1 )
      {
      // There are an odd number of digits, so the string starts on the
      // the right side of the first byte
      nybble += 1;
      nybble_e += 1;
      should_be_zero = *bytes & 0xF0;
      }
    }
  else
    {
    // This is packed decimal, and ends with a sign nybble
    if( size )
      {
      unsigned char nyb = bytes[size-1] & 0x0F;
      if( !signable && nyb != 0x0F)
        {
        retval = 0;
        }
      if( signable && nyb != 0x0C && nyb != 0x0D )
        {
        retval = 0;
        }
      }
    if( !(digits & 1) )
      {
      // There are an even number of digits before the sign nybble.  So the
      // string starts on the right side of the first byte
      nybble += 1;
      nybble_e += 1;
      should_be_zero = *bytes & 0xF0;
      }
    }
  if( should_be_zero != 0 )
    {
    retval = 0;
    }
  // At this point, all nybbles between nybble and nybble_e should be between
  // 0x00 and 0x09.
  while(nybble < nybble_e)
    {
    unsigned char nyb = bytes[nybble/2];
    if( !(nybble & 1))
      {
      nyb >>= 4;
      }
    else
      {
      nyb &= 0xF;
      }
    if( nyb > 0x09 )
      {
      retval = 0;
      break;
      }
    nybble += 1;
    }
  return retval;
  }

static int
is_alpha_a_number(const cblc_field_t *field,
                  size_t offset,
                  size_t size)
  {
  charmap_t *charmap = __gg__get_charmap(field->encoding);
  cbl_char_t mapped_0 = charmap->mapped_character(ascii_0);
  cbl_char_t mapped_9 = charmap->mapped_character(ascii_9);
  int retval = 1;
  size_t i = offset;
  while(i < size)
    {
    cbl_char_t ch = charmap->getch(field->data, &i);
    if(    (ch < mapped_0)
        || (ch > mapped_9) )
      {
      retval = 0;
      break;
      }
    }
  return retval;
  }

static int
classify_numeric_type(cblc_field_t *field,
                      size_t offset,
                      size_t size)
  {
  int retval = 1;
  switch( field->type )
    {
    case FldNumericEdited:
      retval = is_numeric_edited_numeric(field, offset, size);
      break;
    case FldNumericDisplay:
      retval = is_numeric_display_numeric(field, offset, size);
      break;
    case FldPacked:
      retval = is_packed_numeric(field, offset, size);
      break;
    case FldGroup:
    case FldAlphanumeric:
    case FldAlphaEdited:
      retval = is_alpha_a_number(field, offset, size);
      break;

    case FldNumericBinary:
    case FldNumericBin5:
      // These need to checked for fitting into field->digits
      break;

    default:
      fprintf(stderr,
              "We need code for %s numeric type %d\n",
              field->name,
              field->type);
      abort();
      break;
    }
  return retval;
  }

static int
classify_alphabetic_type( const cblc_field_t *field,
                          size_t offset,
                          size_t size,
                          int (checker)( std::wint_t ch ))
  {
  int retval = 1;
  charmap_t *charmap = __gg__get_charmap(DEFAULT_32_ENCODING);
  cbl_char_t space = charmap->mapped_character(ascii_space);
  size_t nbytes_converted;
  const char *converted = __gg__iconverter(field->encoding,
                                           DEFAULT_32_ENCODING,
                                           field->data+offset,
                                           size,
                                           &nbytes_converted);
  size_t i=0;
  while( i < nbytes_converted )
    {
    cbl_char_t ch = charmap->getch(converted, &i);
    if( !checker(ch) && ch != space )
      {
      retval = 0;
      break;
      }
    }
  return retval;
  }

extern "C"
int
__gg__classify( classify_t type,
                cblc_field_t *field,
                size_t offset,
                size_t size)
  {
  // The default answer is TRUE
  int retval;

  if( size == 0 )
    {
    // If there is nothing there, then it can't be TRUE.
    retval = 0;
    }
  else
    {
    switch(type)
      {
      case ClassNumericType:
        retval = classify_numeric_type(field, offset, size);
        break;

      case ClassAlphabeticType:
        retval = classify_alphabetic_type(field, offset, size, std::iswalpha);
        break;

      case ClassLowerType:
        retval = classify_alphabetic_type(field, offset, size, std::iswlower);
        break;

      case ClassUpperType:
        retval = classify_alphabetic_type(field, offset, size, std::iswupper);
        break;

      case ClassInvalidType:
      case ClassDbcsType:
      case ClassKanjiType:
      default:
        warnx("%s(): Don't know how to handle %s",
              __func__,
              classify_str(type));
        abort();
        break;
      }
    }

  return retval;
  }

extern "C"
void
__gg__convert_encoding( char *psz,
                        cbl_encoding_t from,
                        cbl_encoding_t to )
  {
  // This does an in-place conversion of psz
  charmap_t *charmap_from = __gg__get_charmap(from);
  const charmap_t *charmap = __gg__get_charmap(to);
  if( from > custom_encoding_e )
    {
    size_t charsout;
    const char *converted  = __gg__iconverter(from,
                                              to,
                                              psz,
                                              charmap_from->strlen(psz),
                                              &charsout);
    // Copy over the converted string, including the final NUL
    memcpy(psz, converted, charsout + charmap->stride());
    }
  }

extern "C"
void
__gg__convert_encoding_length(char *pch,
                              size_t length,
                              cbl_encoding_t from,
                              cbl_encoding_t to )
  {
  // This does an in-place conversion of length characters at pch
  if( from > custom_encoding_e )
    {
    size_t charsout;
    const char *converted  = __gg__iconverter(from,
                                              to,
                                              pch,
                                              length,
                                              &charsout);
    memcpy(pch, converted, length);
    }
  }

static
int
accept_envar( cblc_field_t  *tgt,
              size_t         tgt_offset,
              size_t         tgt_length,
              const char    *psz_name,
              cbl_encoding_t encoding)
  {
  int retval = 1; // 1 means we couldn't find it

  if( psz_name )
    {
    charmap_t *charmap = __gg__get_charmap(encoding);
    size_t psz_name_length = charmap->strlen(psz_name);

    // convert psz_name to the console encoding:
    size_t converted_length;
    const char *converted = __gg__iconverter(encoding,
                                       __gg__console_encoding,
                                       psz_name,
                                       psz_name_length,
                                       &converted_length);
    // Copy converted, because brute_force_trim uses charmap_t:
    char *env = strdup(converted);
    // Get rid of leading and trailing space characters:
    const char *trimmed_env = brute_force_trim( env,
                                          __gg__console_encoding );

    // Pick up the environment variable
    const char *p = getenv(trimmed_env);
    free(env);
    if(p)
      {
      retval = 0; // We found the environment variable:
      // Convert it to the target encoding:
      converted = __gg__iconverter(__gg__console_encoding,
                                   tgt->encoding,
                                   p,
                                   strlen(p),
                                   &converted_length);
      __gg__field_from_string(tgt, tgt_offset, tgt_length,
                              converted, converted_length);
      }
    else
      {
      // Leave the target unchanged, as per spec.
      }
    }

  if( retval == 1 )
    {
    // Could't find that environment variable
    exception_raise(ec_argument_imp_environment_e);
    }

  return retval;
  }

extern "C"
int
__gg__accept_envar( cblc_field_t *tgt,
                    size_t        tgt_offset,
                    size_t        tgt_length,
              const cblc_field_t *name,
                    size_t        name_offset,
                    size_t        name_length)
  {
  // We need the name to be nul-terminated, so we will tack on four extra
  // nulls to handle characters up to 32 bits wide
  char *p = static_cast<char *>(malloc(name_length + width_of_utf32));
  massert(p);
  memcpy(p, name->data+name_offset, name_length);
  memset(p + name_length, 0, width_of_utf32);
  p[name_length] = '\0';
  int retval = accept_envar(tgt,
                            tgt_offset,
                            tgt_length,
                            p,
                            tgt->encoding);
  free(p);
  return retval;
  }

extern "C"
bool
__gg__set_envar(cblc_field_t    *name,
                size_t           name_offset,
                size_t           name_length,
                cblc_field_t    *value,
                size_t           value_offset,
                size_t           value_length)
  {
  bool retval = false;  // true means the variable existed:

  name_length  = name_length  ? name_length  : name->capacity;
  value_length = value_length ? value_length : value->capacity;

  static char   *env        = NULL;
  static size_t  env_length = 0;
  static char   *val        = NULL;
  static size_t  val_length = 0;
  if( env_length < name_length+1 )
    {
    env_length = name_length+1;
    env = static_cast<char *>(realloc(env, env_length));
    }
  if( val_length < value_length+1 )
    {
    val_length = value_length+1;
    val = static_cast<char *>(realloc(val, val_length));
    }

  massert(val);
  massert(env);

  const char *converted;
  size_t charsout;

  converted = __gg__iconverter(name->encoding,
                               __gg__console_encoding,
                               PTRCAST(char, name->data+name_offset),
                               name_length,
                               &charsout );
  memcpy(env, converted, name_length);
  env[name_length] = '\0';

  converted = __gg__iconverter(value->encoding,
                               __gg__console_encoding,
                               PTRCAST(char, value->data+value_offset),
                               value_length,
                               &charsout );
  memcpy(val, converted, value_length);
  val[value_length] = '\0';

  // Get rid of leading and trailing space characters
  char *trimmed_env = brute_force_trim(env, __gg__console_encoding);
  char *trimmed_val = brute_force_trim(val, __gg__console_encoding);

  if( getenv(trimmed_env) )
    {
    // It already existed:
    retval = true;
    }

  // And now, anticlimactically, set the variable:
  setenv(trimmed_env, trimmed_val, 1);

  return retval;
  }

static int stashed_argc = 0;
// The stashed arguments are in __gg__console_encoding.
static char **stashed_argv = NULL;

extern "C"
void
__gg__stash_argc_argv(int argc, char **argv)
  {
  stashed_argc = argc;
  stashed_argv = argv;

  // This routine is called once by main(), so it is a convenient place to make
  // the stack much bigger, because when people create COBOL programs with
  // ridiculous numbers of variables, the stack gets ridiculously large.

  struct rlimit stack_size = {33554432, 33554432};
  if( setrlimit(RLIMIT_STACK, &stack_size) )
    {
    fprintf(stderr, "warning: attempt to set stack size to 32M failed\n");
    }
  }

static void
command_line_plan_b()
  {
  // It's vaguely possible that somebody can try to access these command-line
  // functions without a main() function having been invoked. This code, for
  // example, could have been created as a stand-alone .so, or it could be
  // from a COBOL .o that was linked to main() from another language.  So, we
  // are going to believe that /proc/cmdline is available, and proceed from
  // there:
  if( !stashed_argc )
    {
    static char input[4096];
    sprintf(input, "/proc/%ld/cmdline", (long)getpid());
    FILE *f = fopen(input, "r");
    if( f )
      {
      size_t bytes_read = fread(input, 1, sizeof(input), f);
      fclose(f);
      if( bytes_read )
        {
        char *p = input;
        const char *p_end = p + bytes_read;
        char prior_char = '\0';
        while( p < p_end )
          {
          if( prior_char == '\0' )
            {
            stashed_argc += 1;
            stashed_argv = static_cast<char **>(realloc(stashed_argv,
                                            stashed_argc * sizeof(char *)));
            stashed_argv[stashed_argc-1] = p;
            }
          prior_char = *p++;
          }
        }
      }
    }
  }

extern "C"
void
__gg__get_argc(cblc_field_t *dest, size_t offset, size_t length)
  {
  command_line_plan_b();
  char ach[128];
  sprintf(ach, "%d", stashed_argc);
  size_t nbytes;
  char *converted = __gg__miconverter(__gg__console_encoding,
                                            dest->encoding,
                                            ach,
                                            strlen(ach),
                                            &nbytes );
  __gg__field_from_string(dest, offset, length, converted, nbytes);
  __gg__adjust_dest_size(dest, nbytes);
  free(converted);
  }

extern "C"
int
__gg__get_argv( cblc_field_t *dest,
                size_t dest_offset,
                size_t dest_length,
                cblc_field_t *index,
                size_t index_offset,
                size_t index_size)
  {
  int retcode;
  command_line_plan_b();
  int rdigits;
  __int128 N = get_binary_value_local(&rdigits,
                                      index,
                                      index->data + index_offset,
                                      index_size);

  // N is 1-based, per normal COBOL.  We have to decrement it here:
  N -= 1;

  dest_length = dest_length ? dest_length : dest->capacity;

  // If he gives us fractional digits, just truncate
  N /= __gg__power_of_ten(rdigits);

  if( N >= stashed_argc || N < 0 )
    {
    exception_raise(ec_argument_imp_command_e);
    retcode = 1;  // Error
    }
  else
    {
    size_t nbytes;
    char *converted = __gg__miconverter(__gg__console_encoding,
                                        dest->encoding,
                                        stashed_argv[N],
                                        strlen(stashed_argv[N]),
                                        &nbytes );
    __gg__field_from_string(dest, dest_offset, dest_length, converted, nbytes);
    __gg__adjust_dest_size(dest, nbytes);
    free(converted);
    retcode = 0;  // Okay
    }
  return retcode;
  }

extern "C"
int
__gg__get_command_line( cblc_field_t *field,
                        size_t offset,
                        size_t flength)
  {
  int retcode;
  command_line_plan_b();
  size_t length = 1;
  char *retval = static_cast<char *>(malloc(length));
  massert(retval);
  *retval = NULLCH;

  for( int i=1; i<stashed_argc; i++ )
    {
    while( strlen(retval) + strlen(stashed_argv[i]) + 2 > length )
      {
      length *= 2;
      retval = static_cast<char *>(realloc(retval, length));
      massert(retval);
      }
    if( *retval )
      {
      strcat(retval, " ");
      }
    strcat(retval, stashed_argv[i]);
    }

  if( *retval )
    {
    flength = flength ? flength : field->capacity;
    size_t nbytes;
    char *converted = __gg__miconverter(__gg__console_encoding,
                                        field->encoding,
                                        retval,
                                        strlen(retval),
                                        &nbytes );
    __gg__field_from_string(field, offset, flength, converted, nbytes);
    __gg__adjust_dest_size(field, nbytes);
    free(converted);
    retcode = 0; // Okay
    }
  else
    {
    exception_raise(ec_argument_imp_command_e);
    retcode = 1;// Error
    }

  free(retval);
  return retcode;
  }

extern "C"
void
__gg__set_pointer(cblc_field_t       *target,
                  size_t              target_o,
                  int                 target_flags,
                  const cblc_field_t *source,
                  size_t              source_o,
                  int                 source_flags)
  {
  void *source_address;
  if( source_flags & REFER_T_ADDRESS_OF )
    {
    // This is SET <something> TO ADDRESS OF SOURCE
    source_address = source->data + source_o;
    }
  else
    {
    // This is SET <something> TO POINTER
    if( source )
      {
      source_address = *reinterpret_cast<void **>(source->data + source_o);
      }
    else
      {
      // This is SET xxx TO NULL
      source_address = NULL;
      }
    }

  if( target_flags & REFER_T_ADDRESS_OF )
    {
    // This is SET ADDRESS OF target TO ....
    // We know it has to be an unqualified LINKAGE level 01 or level 77
    target->data  = reinterpret_cast<unsigned char *>(source_address);
    // The caller will propogate data + offset to their children.
    }
  else
    {
    // This is SET <pointer> TO ....
    if( source->type == FldLiteralN )
      {
      // This is [almost certainly] INITIALIZE <pointer> when -fdefaultbyte
      // was specified.
      memset( target->data+target_o,
              *reinterpret_cast<unsigned char *>(source_address),
              target->capacity);
      }
    else
      {
      *reinterpret_cast<void **>(target->data+target_o) = source_address;
      }
    }
  }

extern "C"
void
__gg__alphabet_use( cbl_encoding_t display_encoding,
                    cbl_encoding_t national_encoding,
                    cbl_encoding_t encoding,
                    size_t alphabet_index)
  {
  // We simply replace the values in the current program_state.  If the
  // state needs to be saved -- for example, if we are doing a SORT with an
  // ALPHABET override -- that's up to the caller

  __gg__display_encoding  = display_encoding;
  __gg__national_encoding = national_encoding;

  if( program_states.empty() )
    {
    // When there is no DATA DIVISION, program_states can be empty when
    // we arrive here.  So, we need to remedy that.
    initialize_program_state();
    }

  const charmap_t *charmap_alphabetic = __gg__get_charmap(display_encoding);

  switch( encoding )
    {
    case iconv_CP1252_e:
    case ASCII_e:
    case iso646_e:
      // This is one of the very common standard situations; where we are using
      // something like a CP1252 Western European ASCII-like character set.

      __gg__low_value_character  = DEGENERATE_LOW_VALUE;
      __gg__high_value_character = DEGENERATE_HIGH_VALUE;

      program_states.back().rt_low_value_character   = DEGENERATE_LOW_VALUE;
      program_states.back().rt_high_value_character  = DEGENERATE_HIGH_VALUE;

      if( !charmap_alphabetic->is_like_ebcdic() )
        {
        // The codeset is ascii-like, and the collation is ascii, so we use
        // one-to-one values:
        program_states.back().rt_collation = __gg__one_to_one_values;
        }
      else
        {
        // The codeset is ebcdic-like, but the collation is specified as
        // ascii-like.  So, we need that collation:
        program_states.back().rt_collation = __gg__ebcdic_to_cp1252_collation;
        }

      break;

    case EBCDIC_e:
      __gg__low_value_character  = DEGENERATE_LOW_VALUE;
      __gg__high_value_character = DEGENERATE_HIGH_VALUE;

      program_states.back().rt_low_value_character   = DEGENERATE_LOW_VALUE;
      program_states.back().rt_high_value_character  = DEGENERATE_HIGH_VALUE;
      if( charmap_alphabetic->is_like_ebcdic() )
        {
        // The alphanumeric codeset is ebcdic-like, and so is the specified
        // collation:
        program_states.back().rt_collation = __gg__one_to_one_values;
        }
      else
        {
        // The alphanumeric codeset is ebcdic-like, but the specified collation
        // is ascii-like:
        program_states.back().rt_collation = __gg__cp1252_to_ebcdic_collation;
        }
      break;

    case custom_encoding_e:
      {
      std::unordered_map<size_t, alphabet_state>::const_iterator it =
        __gg__alphabet_states.find(alphabet_index);

      if( it == __gg__alphabet_states.end() )
        {
        __gg__abort("__gg__alphabet_use() fell off the end of __gg__alphabet_states");
        }
      __gg__low_value_character  = it->second.low_char;
      __gg__high_value_character = it->second.high_char;
      program_states.back().rt_low_value_character  = it->second.low_char;
      program_states.back().rt_high_value_character = it->second.high_char;

      program_states.back().rt_collation = it->second.collation;
      break;
      }

    default:
      break;
    }
  return;
  }

extern "C"
void
__gg__parser_set_conditional(cblc_field_t *var, int figconst_)
  {
  charmap_t *charmap = __gg__get_charmap(var->encoding);

  cbl_figconst_t figconst = (cbl_figconst_t)figconst_;

  cbl_char_t special = charmap->mapped_character(ascii_space);
  switch(figconst)
    {
    case space_value_e:
      special = charmap->mapped_character(ascii_space);
      break;
    case low_value_e:
      special = charmap->low_value_character();
      break;
    case high_value_e:
      special = charmap->high_value_character();
      break;
    case zero_value_e:
      special = charmap->mapped_character(ascii_0);
      break;
    case quote_value_e:
      special = charmap->quote_character();
      break;
    default:
      break;
    }
  charmap->memset( var->data, special, var->capacity);
  }

extern "C"
int
__gg__routine_to_call(const char *name,
                      int         program_id)
  {
  // The list of names is sorted, so at the very least this should be replaced
  // with a binary search:

  std::unordered_map<int, char***>::const_iterator it =
         accessible_programs.find(program_id);

  if(it == accessible_programs.end())
    {
    __gg__abort("__gg__routine_to_call() couldn't find program_id");
    }

  char **names = *(it->second);

  int retval = -1;

  if( names )
    {
    int i=0;
    while(*names)
      {
      if( strstr(*names, name) )
        {
        // The first part of the names match
        if( (*names)[strlen(name)] == '.' )
          {
          // And the first character after the match is a '.'
          retval = i;
          break;
          }
        }
      i += 1;
      names += 1;
      }
    }
  return retval;
  }

extern "C"
__int128
__gg__fetch_call_by_value_value(const cblc_field_t *field,
                                size_t field_o,
                                size_t field_s)

  {
  int rdigits;
  unsigned char *data   = field->data + field_o;
  const size_t   length = field_s;

  __int128 retval = 0;
  switch(field->type)
    {
    case FldGroup:
    case FldAlphanumeric:
    case FldAlphaEdited:
    case FldLiteralA:
      retval = *reinterpret_cast<char *>(data);
      break;

    case FldFloat:
      {
      switch(length)
        {
        case 4:
          *PTRCAST(float, &retval) = *PTRCAST(float, data);
          break;

        case 8:
          *PTRCAST(double, &retval) = *PTRCAST(double, data);
          break;

        case 16:
          // *(_Float128 *)(&retval) = double(*(_Float128 *)data);
          GCOB_FP128 t;
          memcpy(&t, data, 16);
          memcpy(&retval, &t, 16);
          break;
        }
      break;
      }

    case FldNumericBinary:
    case FldPacked:
    case FldNumericBin5:
    case FldNumericDisplay:
    case FldNumericEdited:
    case FldLiteralN:
    case FldIndex:
    case FldPointer:
      retval = __gg__binary_value_from_qualified_field( &rdigits,
                                                        field,
                                                        field_o,
                                                        field_s);
      if( rdigits )
        {
        retval /= __gg__power_of_ten(rdigits);
        }
    default:
      break;
    }
  return retval;
  }

extern "C"
void
__gg__assign_value_from_stack(cblc_field_t *dest, __int128 parameter)
  {
  switch(dest->type)
    {
    case FldGroup:
    case FldAlphanumeric:
    case FldAlphaEdited:
    case FldNumericEdited:
      if( dest->capacity >= 1)
        {
        warnx("%s is not valid for BY VALUE", dest->name);
        abort();
        }
      break;

    case FldFloat:
      {
      switch(dest->capacity)
        {
        case 4:
          *PTRCAST(float, dest->data) = *PTRCAST(float, (&parameter));
          break;

        case 8:
          *PTRCAST(double, dest->data) = *PTRCAST(double, (&parameter));
          break;

        case 16:
          // *(_Float128 *)(dest->data) = *(_Float128 *)&parameter;
          GCOB_FP128 t;
          memcpy(&t, &parameter, 16);
          memcpy(dest->data, &t, 16);
          break;
        }
      break;
      }

    case FldNumericBinary:
    case FldPacked:
    case FldNumericBin5:
    case FldNumericDisplay:
    case FldLiteralN:
    case FldIndex:
    case FldPointer:
    __gg__int128_to_field(dest,
                          parameter,
                          0,
                          truncation_e,
                          NULL);
      break;

    default:
      break;
    }
  }

extern "C"
int
__gg__literaln_alpha_compare(      char         *left_side,
                             const cblc_field_t *right,
                             size_t              offset,
                             size_t              length,
                             int                 flags)
  {
  int retval;
  if( length == 0 )
    {
    length = right->capacity;
    }

  cbl_encoding_t right_encoding = right->encoding;
  if( right->attr & hex_encoded_e )
    {
    right_encoding = iconv_CP1252_e;
    }
  retval = compare_strings(   left_side,
                              strlen(left_side),
                              false,
                              reinterpret_cast<char *>((right->data + offset)),
                              length,
                              !!(flags & REFER_T_MOVE_ALL),
                              right_encoding,
                              right_encoding);
  return retval;
  }

extern "C"
int
__gg__unstring( const cblc_field_t *id1,        // The string being unstring
                size_t              id1_o,
                size_t              id1_s,
                size_t ndelimiteds,       // The number of DELIMITED entries
                const char *all_flags,    // The number of ALL flags, one per ndelimiteds
                size_t nreceivers,        // The number of DELIMITER receivers
                cblc_field_t *id7,        // The index of characters, both for starting updated at end
                size_t        id7_o,
                size_t        id7_s,
                cblc_field_t *id8,        // Count of the number of times identifier-4 was updated
                size_t        id8_o,
                size_t        id8_s)
  {
  // The names of the parameters are based on the ISO 1989:2014 specification.

  // There are complexities because of figurative constants, including the
  // LOW-VALUE figurative constant, which precludes the use of string
  // operations that would be confused by embedded NUL characters.  Dammit.

  // For each delimiter, there is an identifier-4 receiver that must be
  // resolved.  Each might have an identifier-5 delimiter, and each might have
  // an identifier-6 count.

  // The delimiting strings; one per ndelimiteds
  cblc_field_t **id2         = __gg__treeplet_1f;
  const size_t        *id2_o = __gg__treeplet_1o;
  const size_t        *id2_s = __gg__treeplet_1s;
  // The delimited string; one per nreceiver
  cblc_field_t **id4         = __gg__treeplet_2f;
  const size_t        *id4_o = __gg__treeplet_2o;
  const size_t        *id4_s = __gg__treeplet_2s;
  // The delimiting string; one per receiver
  cblc_field_t **id5         = __gg__treeplet_3f;
  const size_t        *id5_o = __gg__treeplet_3o;
  const size_t        *id5_s = __gg__treeplet_3s;
  // The count of characters examined;  one per receiver
  cblc_field_t **id6         = __gg__treeplet_4f;
  const size_t        *id6_o = __gg__treeplet_4o;
  const size_t        *id6_s = __gg__treeplet_4s;

  // Initialize the state variables
  int overflow = 0;
  int tally = 0;
  size_t pointer = 1;
  size_t nreceiver;
  size_t left;
  size_t right;

  std::u32string str_id1;
  std::vector<std::u32string> delimiters;

  const charmap_t *charmap_id1 = __gg__get_charmap(id1->encoding);
  int stride_id1 = charmap_id1->stride();

  if( id8  )
    {
    int rdigits;
    tally = (int)__gg__binary_value_from_qualified_field(&rdigits,
                                                         id8,
                                                         id8_o,
                                                         id8_s);
    }

  if( id7 )
    {
    int rdigits;
    int p = (int)__gg__binary_value_from_qualified_field(&rdigits,
                                                         id7,
                                                         id7_o,
                                                         id7_s);
    if( p < 1 )
      {
      overflow = 1;
      goto done;
      }
    pointer = p;
    }

  // As per the spec, if the string is zero-length; we are done.
  if( id1_s == 0 )
    {
    goto done;
    }

  // As per the spec, we have an overflow condition if pointer is out of
  // range:
  if( pointer > id1_s/stride_id1 )
    {
    overflow = 1;
    goto done;
    }
  // pointer is one-based throughout; don't forget that

  /* I thought long and hard about converting things to UTF32 for UNSTRING. It
     was not obviously necessary.  But, darn it all, sooner or later somebody
     is going to demand UTF-8 capability and I can't think of any obvious way
     of being able to handle multibyte codepoints as single characters without
     doing something like converting to UTF32.  */

  str_id1 = normalize_for_inspect_format_4( id1,
                                            id1_o,
                                            id1_s,
                                            id1->encoding);
  left = pointer-1;
  right = str_id1.size();
  if( ndelimiteds == 0 )
    {
    // There are no DELIMITED BY identifier-2 values, so we just peel off
    // characters from identifier-1 and put them into each identifier-4:
    for( size_t receiver=0; receiver<nreceivers; receiver++ )
      {
      if( left >= right )
        {
        // We have run out of input characters.
        break;
        }
      // We will peel off enough characters to fit the receiving id4:
      size_t id_4_size = id4_s[receiver]/stride_id1;
      if( id4[receiver]->attr & separate_e )
        {
        // The receiver is NumericDisplay with a separate sign, so, as per
        // the spec, we reduce the size by one character.
        id_4_size = id4_s[receiver] - 1;
        }

      // Make sure id_4_size doesn't take us past the end of the universe
      if( left + id_4_size > right )
        {
        id_4_size = right - left;
        }

      // Convert the specified str_id1 characters back to id1->encoding.
      size_t bytes_converted;
      const char *converted = __gg__iconverter(DEFAULT_32_ENCODING,
                                               id1->encoding,
                                               &str_id1[left],
                                               (right-left)*width_of_utf32,
                                               &bytes_converted );
      char *duped = static_cast<char *>(__gg__memdup(converted, bytes_converted));
      // Put the converted string into place:
      __gg__field_from_string(id4[receiver],
                        id4_o[receiver],
                        id4_s[receiver],
                        duped,
                        bytes_converted);
      free(duped);
      // Update the state variables:
      left += id_4_size;
      pointer += id_4_size;
      tally += 1;
      }
    goto done;
    }

  // Arriving here means there is some number of ndelimiteds

  // Convert them to the same encoding as str_id1:
  for( size_t i=0; i<ndelimiteds; i++ )
    {
    std::u32string delimiter
        = normalize_for_inspect_format_4(id2[i],
                                         id2_o[i],
                                         id2_s[i],
                                         id1->encoding);
    delimiters.push_back(delimiter);
    }

  nreceiver = 0;
  while( left < right )
    {
    // Starting at 'left', see if we can find any of the delimiters.  For each
    // 'left' position, we look through all of the delimiters,

    int    best_delimiter = -1;
    size_t best_leftmost = right; // This is the location of the start of ALL
    size_t best_location = right; // This is the location of the last of ALL
    for( size_t i=0; i<ndelimiteds; i++ )
      {
      std::u32string str_id2 = delimiters[i];
      size_t nfound = str_id1.find(str_id2, left);
      if( nfound != std::u32string::npos )
        {
        // We found a delimiter
        if( nfound > best_leftmost )
          {
          // This delimiter lives to the right of the best one we found so far.
          // Ignore it, and proceed to the next delimiter.
          continue;
          }
        // This delimiter is the leftmost we've seen so far:
        best_delimiter = i;
        best_leftmost  = nfound;
        best_location  = nfound;

        if( all_flags[i] == ascii_1 )
          {
          // This delimiter is flagged as ALL, so we need to see if we have
          // a flock of them:
          size_t next = nfound + str_id2.size() ;
          while( str_id1.find(str_id2, next ) == next )
            {
            // We found another consecutive one at next:
            best_location = next;
            next += str_id2.size();
            }
          }
        }
      }

    // If we've used up all receivers, we bail at this point
    if( nreceiver >= nreceivers )
      {
      break;
      }

    if( best_delimiter == -1 )
      {
      // We were unable to find a delimiter, so we eat up the remainder
      // of the sender:
      best_leftmost = right;
      best_location = right;
      }

    // Apply what we have learned to the next receiver:

    size_t examined = best_leftmost - left;

    // Convert the data from left to leftmost_delimiter back to encoding of
    // id1:
    size_t bytes_converted;
    const char *converted = __gg__iconverter(
                           DEFAULT_32_ENCODING,
                           id1->encoding,
                           &str_id1[left],
                           (best_leftmost-left)*width_of_utf32,
                           &bytes_converted );
    char *duped = static_cast<char *>(__gg__memdup(converted, bytes_converted));
    // Put the converted string into place:
    __gg__field_from_string(id4[nreceiver],
                      id4_o[nreceiver],
                      id4_s[nreceiver],
                      duped,
                      bytes_converted);
    free(duped);
    // Update the left edge
    left = best_location + (best_delimiter > -1
                            ? delimiters[best_delimiter].size()
                            : 0) ;
    if( id5[nreceiver] )
      {
      // The caller wants to know what the delimiter was:
      if( best_delimiter > -1 )
        {
        converted = __gg__iconverter(
                             DEFAULT_32_ENCODING,
                             id1->encoding,
                             delimiters[best_delimiter].data(),
                             delimiters[best_delimiter].size()*width_of_utf32,
                             &bytes_converted );
        duped = static_cast<char *>(__gg__memdup(converted, bytes_converted));
        __gg__field_from_string(id5[nreceiver],
                          id5_o[nreceiver],
                          id5_s[nreceiver],
                          duped,
                          bytes_converted);
        free(duped);
        }
      else
        {
        // We didn't find a delimiter
        __gg__field_from_string(id5[nreceiver],
                          id5_o[nreceiver],
                          id5_s[nreceiver],
                          "",
                          0);
        }
      }

    if( id6[nreceiver] )
      {
      __gg__int128_to_qualified_field(id6[nreceiver],
                                      id6_o[nreceiver],
                                      id6_s[nreceiver],
                                      (__int128)examined,
                                      0,
                                      truncation_e,
                                      NULL );
      }

    // Update the state variables:
    tally += 1;
    nreceiver += 1;
    if( best_delimiter > -1  )
      {
      pointer = left+1 ;
      }
    }

done:

  if( id8 )
    {
    __gg__int128_to_qualified_field(id8,
                                    id8_o,
                                    id8_s,
                                    (__int128)tally,
                                    0,
                                    truncation_e,
                                    NULL );
    }

  if( id7 )
    {
    __gg__int128_to_qualified_field(id7,
                                    id7_o,
                                    id7_s,
                                    (__int128)pointer,
                                    0,
                                    truncation_e,
                                    NULL );
    }

  if( left < right )
    {
    overflow = 1;
    }

  return overflow;
  }

static std::set<size_t> to_be_canceled;

extern "C"
void __gg__to_be_canceled(size_t function_pointer)
  {
  if( function_pointer )
    {
    to_be_canceled.insert(function_pointer);
    }
  }

extern "C"
int __gg__is_canceled(size_t function_pointer)
  {
  int retval = 0;
  std::set<size_t>::iterator it = to_be_canceled.find(function_pointer);
  if( it == to_be_canceled.end() )
    {
    retval = 0;
    }
  else
    {
    retval = 1;
    to_be_canceled.erase(it);
    }
  return retval;
  }

// Tue Feb 28 10:10:16 2023
// Associate specific I/O status an with exception condition.
// Cf. ISO Table 14 — Exception-names and exception conditions

static inline ec_type_t
local_ec_type_of( file_status_t status )
  {
  int status10 = (int)status / 10;
  assert( 0 <= status10 ); // was enum, can't be negative.
  if( 10 < status10 )
    {
    __gg__abort("local_ec_type_of(): status10 out of range");
    }

  static const std::vector<ec_type_t> ec_by_status {
    /* 0 */ ec_none_e, // ec_io_warning_e if low byte is nonzero
    /* 1 */ ec_io_at_end_e,
    /* 2 */ ec_io_invalid_key_e,
    /* 3 */ ec_io_permanent_error_e,
    /* 4 */ ec_io_logic_error_e,
    /* 5 */ ec_io_record_operation_e,
    /* 6 */ ec_io_file_sharing_e,
    /* 7 */ ec_io_record_content_e,
    /* 8 */ ec_none_e, // unused, not defined by ISO
    /* 9 */ ec_io_imp_e,
  };
  assert(ec_by_status.size() == 10);

  return ec_by_status[status10];
  }

/*
 * Store and report the enabled exceptions.
 * 7.3.20.3 General rules:
 *  1) The default TURN directive is '>>TURN EC-ALL CHECKING OFF'.
 */
struct exception_descr_t {
  bool location;
  //std::set<size_t> files;
};

struct cbl_exception_t {
  size_t file;
  ec_type_t type;
  cbl_file_mode_t mode;
};

/*
 * Compare the raised exception, cbl_exception_t, to the USE critera
 * of a declarative, cbl_declarative_t.
 */
static bool
match_declarative( bool enabled,
                   const cbl_exception_t& raised,
                   const cbl_declarative_t& dcl )
{
  if( MATCH_DECLARATIVE && raised.type) {
    warnx("match_declarative: checking:    ec %s vs. dcl %s (%s enabled and %s format_1)",
          local_ec_type_str(raised.type),
          local_ec_type_str(dcl.type),
          enabled? "is" : "not",
          dcl.is_format_1()? "is" : "not");
  }
  if( ! (enabled || dcl.is_format_1()) ) return false;

  bool matches = ec_cmp(raised.type, (dcl.type));

  if( matches && dcl.nfile > 0 ) {
    matches = dcl.match_file(raised.file);
  }

  // Having matched, the EC must either be enabled, or
  // the Declarative must be USE Format 1.
  if( matches ) {
    // I/O declaratives match by file or mode, not EC.
    if( dcl.is_format_1() ) { // declarative is for particular files or mode
      if( dcl.nfile == 0 ) {
        matches = raised.mode == dcl.mode;
      }
    } else {
      matches = enabled;
    }

    if( matches && MATCH_DECLARATIVE ) {
      warnx("                   matches exception      %s (file %u mode %s)",
            local_ec_type_str(raised.type),
            static_cast<unsigned int>(raised.file),
            cbl_file_mode_str(raised.mode));
    }
  }
  return matches;
}

static
void open_syslog(int option, int facility)
{
  static bool first_time = true;
  if( first_time ) {
#if HAVE_DECL_PROGRAM_INVOCATION_SHORT_NAME
  /* Declared in errno.h, when available.  */
    static const char * const ident = program_invocation_short_name;
#elif defined (HAVE_GETPROGNAME)
  /* Declared in stdlib.h.  */
    static const char * const ident = getprogname();
#else
  /* Avoid a NULL entry.  */
    static const char * const ident = "unnamed_COBOL_program";
#endif
    // TODO: Program to set option in library via command-line and/or environment.
    //       Library listens to program, not to the environment.
    openlog(ident, option, facility);
    first_time = false;
  }
}

/*
 * The default exception handler is called if:
 *   1.  The EC is enabled and was not handled by a Declarative, or
 *   2.  The EC is EC-I-O and was not handled by a Format-1 Declarative, or
 *   3.  The EC is EC-I-O, associated with a file, and is not OPEN or CLOSE.
 */
static void
default_exception_handler( ec_type_t ec )
{
  static const int priority = LOG_INFO, option = LOG_PERROR, facility = LOG_USER;
  open_syslog(option, facility);

  ec_disposition_t disposition = ec_category_fatal_e;


  if( ec != ec_none_e ) {
    auto pec = std::find_if( __gg__exception_table, __gg__exception_table_end,
                           [ec](const ec_descr_t& descr) {
                             return descr.type == ec;
                           } );
    if( pec != __gg__exception_table_end ) {
      disposition = pec->disposition;
    } else {
      warnx("logic error: unknown exception %x", ec );
    }
    /*
     * An enabled, unhandled fatal EC normally results in termination. But
     * EC-I-O is a special case:
     *   OPEN and CLOSE never result in termination.
     *   A SELECT statement with FILE STATUS indicates the user will handle the error.
     *   Only I/O statements are considered.
     * Declaratives are handled first.  We are in the default handler here,
     * which is reached only if no Declarative was matched.
     */
    auto file = ec_status.file_status();
    const char *filename = nullptr;

    if( file.ifile ) {
      filename = file.filename;
      switch( last_exception_file_operation ) {
      case file_op_none:   // not an I/O statement
        break;
      case file_op_open:
      case file_op_close:  // No OPEN/CLOSE results in a fatal error.
        disposition = ec_category_none_e;
        break;
      default:
        if( file.user_status ) {
          // Not fatal if FILE STATUS is part of the file's SELECT statement.
          disposition = ec_category_none_e;
        }
        break;
      }
    } else {
      assert( ec_status.is_enabled() );
      assert( ec_status.is_enabled(ec) );
    }

    switch( disposition ) {
    case ec_category_none_e:
    case uc_category_none_e:
      break;
    case ec_category_fatal_e:
    case uc_category_fatal_e:
      if( filename ) {
        syslog(priority, "fatal exception: %s:%d: %s %s: %s (%s)",
               program_name,
               ec_status.lineno,
               ec_status.statement,
               filename, // show affected file before EC name
               pec->name,
               pec->description);
      } else {
        syslog(priority, "fatal exception: %s:%d: %s: %s (%s)",
               program_name,
               ec_status.lineno,
               ec_status.statement,
               pec->name,
               pec->description);
      }
      abort();
      break;
    case ec_category_nonfatal_e:
    case uc_category_nonfatal_e:
      syslog(priority, "%s:%d: %s: %s (%s)",
             program_name,
             ec_status.lineno,
             ec_status.statement,
             pec->name,
             pec->description);
      break;
    case ec_category_implementor_e:
    case uc_category_implementor_e:
      break;
    }

    ec_status.clear();
  }
}

/*
 * To reach the default handler, an EC must have effect and not have been
 * handled by program logic.  To have effect, it must have been enabled
 * explictly, or be of type EC-I-O.  An EC may be handled by the statement or
 * by a Declarative.
 *
 * Any EC handled by statement's conditional clause (e.g. ON SIZE ERROR)
 * prevents an EC from being raised.  Because it is not raised, it is handled
 * neither by a Declarative, nor by the the default handler.
 *
 * A nonfatal EC matched to a Declarative is considered handled.  A fatal EC is
 * considered handled if the Declarative uses RESUME.  For any EC that is
 * handled (with RESUME for fatal), program control passes to the next
 * statement. Else control passes here first.
 *
 * Any EC explicitly enabled (with >>TURN) must be explicitly handled.  Only
 * explicitly enabled ECs appear in enabled_ECs.  when EC-I-O is raised as a
 * byproduct of error status on a file operation, we say it is "implicitly
 * enabled".  It need not be explicitly handled.
 *
 * Implicit EC-I-O not handled by the statement or a Declarative is considered
 * handled if the statement includes the FILE STATUS phrase.  OPEN and CLOSE
 * never cause program termination with EC-I-O; for those two statements the
 * fatal status is ignored.  These conditions are screened out by
 * __gg__check_fatal_exception(), so that the default handler is not called.
 *
 * An unhandled EC reaches the default handler for any of 3 reasons:
 *   1.  It is EC-I-O (enabled does not matter).
 *   2.  It is enabled.
 *   3.  It is fatal and was matched to a Declarative that did not use RESUME.
 * The default handler, default_exception_handler(), logs the EC.  For a fatal
 * EC, the process terminated with abort(3).
 *
 * Except for OPEN and CLOSE, I/O statements that raise an unhandled fatal EC
 * cause program termination, consistent with IBM documentation.  See
 * Enterprise COBOL for z/OS: Enterprise COBOL for z/OS 6.4 Programming Guide,
 * page 244, "Handling errors in input and output operations".
 */
extern "C"
void
__gg__check_fatal_exception()
{
  if( MATCH_DECLARATIVE )
    warnx("%s: ec_status is %s", __func__, ec_status.unset()? "unset" : "set");

  if( ec_status.copy_environment().unset() )
    {
    ec_status.update();  // __gg__match_exception was not called first
    // This is a good time to set the exception code back to zero
    __gg__exception_code = 0;
    }

  if( ec_status.done() ) { // false for part-handled fatal
    if( MATCH_DECLARATIVE )
      warnx("%s: clearing ec_status", __func__);
    ec_status.clear();
    return; // already handled
  }

  auto ec = ec_status.unhandled();

  if( MATCH_DECLARATIVE )
    warnx("%s: %s was not handled %s enabled", __func__,
          local_ec_type_str(ec), ec_status.is_enabled(ec)? "is" : "is not");

  // Look for ways I/O statement might have dealt with EC.
  auto file = ec_status.file_status();
  if( file.ifile && ec_cmp(ec, ec_io_e) ) {
    if( MATCH_DECLARATIVE )
      warnx("%s: %s with %sFILE STATUS", __func__,
            file.op_str(), file.user_status? "" : "no ");
    if( file.user_status ) {
      ec_status.clear();
      return; // has FILE STATUS, ok
    }
    switch( file.operation ) {
    case file_op_none:
      assert(false);
      abort();
    case file_op_open: // implicit, no Declarative, no FILE STATUS, but ok
    case file_op_close:
      ec_status.clear();
      return;
    case file_op_start:
    case file_op_read:
    case file_op_write:
    case file_op_rewrite:
    case file_op_delete:
    case file_op_remove:
      break;
    }
  } else {
    if( ! ec_status.is_enabled() ) {
      if( MATCH_DECLARATIVE )
        warnx("%s: %s is not enabled", __func__, local_ec_type_str(ec));
      ec_status.clear();
      return;
    }
    if( MATCH_DECLARATIVE )
      warnx("%s: %s is enabled", __func__, local_ec_type_str(ec));
  }

  if( MATCH_DECLARATIVE )
    warnx("%s: calling default_exception_handler(%s)", __func__,
          local_ec_type_str(ec));

  default_exception_handler(ec);
}

/*
 * Preserve the state of the raised EC during Declarative execution.
 */
extern "C"
void
__gg__exception_push()
{
  ec_stack.push(ec_status);
  if( MATCH_DECLARATIVE )
    warnx("%s: %s: %u ECs, %u declaratives", __func__,
          __gg__exception_statement,
	  static_cast<unsigned int>(enabled_ECs.size()),
	  static_cast<unsigned int>(declaratives.size()));
}

/*
 * Restore the state of the raised EC after Declarative execution.
 */
extern "C"
void
__gg__exception_pop()
{
  ec_status = ec_stack.top();
  ec_stack.pop();
  ec_status.reset_environment();
  if( MATCH_DECLARATIVE )
    warnx("%s: %s: %u ECs, %u declaratives", __func__,
          __gg__exception_statement,
	  static_cast<unsigned int>(enabled_ECs.size()),
	  static_cast<unsigned int>(declaratives.size()));
  __gg__check_fatal_exception();
}

// Called for RESUME in a Declarative to indicate a fatal EC was handled.
extern "C"
void
__gg__clear_exception()
{
  ec_stack.top().clear();
}

void
cbl_enabled_exception_t::dump( int i ) const {
  warnx("cbl_enabled_exception_t: %2d  {%s, %s, %u}",
        i,
        location? "location" : "    none",
        local_ec_type_str(ec),
        static_cast<unsigned int>(file) );
}

/*
 * Match the raised exception against a Declarative.
 *
 * A Declarative that handles I/O errors with USE Format 1 doesn't name a
 * specific EC.  It's matched based on the file's status, irrespective of
 * whether or not EC-I-O is enabled.  USE Format 1 Declaratives are honored
 * regardless of any >>TURN directive.
 *
 * An EC is enabled by the >>TURN directive.  The only ECs that can be disabled
 * are those that were explicitly enabled.  If EC-I-O is enabled, and mentioned
 * in a Declarative with USE Format 3, then it is matched just like any other.
 */
extern "C"
void
__gg__match_exception( cblc_field_t *index )
{
  size_t isection = 0;

  if( MATCH_DECLARATIVE ) enabled_ECs.dump("match_exception begin");

  auto ec = ec_status.update().unhandled();

  if( ec != ec_none_e ) {
    /*
     * An EC was raised and was not handled by the statement.
     * We know the EC and, for I/O, the current file and its mode.
     * Scan declaratives for a match:
     *   - EC is enabled or program has a Format 1 Declarative
     *   - EC matches the Declarative's USE statement
     * Format 1 declaratives apply only to EC-I-O, whether or not enabled.
     * Format 1 may be restricted to a particular mode (for all files).
     * Format 1 and 3 may be restricted to a set of files.
     */

    // This is a good time to set the actual exception code back to zero.
    __gg__exception_code = 0;

    auto f = ec_status.file_status();
    cbl_exception_t raised = { /*0,*/ f.ifile, ec, f.mode };
    bool enabled = enabled_ECs.match(ec);

    if( MATCH_DECLARATIVE ) enabled_ECs.dump("match_exception enabled");

    auto p = std::find_if( declaratives.begin(), declaratives.end(),
                           [enabled, raised]( const cbl_declarative_t& dcl ) {
                             return match_declarative(enabled, raised, dcl);
                           } );

    if( p == declaratives.end() ) {
      if( MATCH_DECLARATIVE ) {
        warnx("__gg__match_exception:%d: raised exception "
              "%s not matched (%u enabled)", __LINE__,
              local_ec_type_str(ec),
	      static_cast<unsigned int>(enabled_ECs.size()));
      }
    } else {
      isection = p->section;
      ec_status.handled_by(isection);

      if( MATCH_DECLARATIVE ) {
        warnx("__gg__match_exception:%d: matched "
              "%s against mask %s for section #%u",
              __LINE__,
              local_ec_type_str(ec),
              local_ec_type_str(p->type),
              static_cast<unsigned int>(p->section));
      }
    }
    assert(ec != ec_none_e);
  } // end EC match logic

  // If a declarative matches the raised exception, return its
  // symbol_table index.
  __gg__int128_to_field(index,
                        (__int128)isection,
                        0,
                        truncation_e,
                        NULL);
}

static std::vector<void *>proc_signatures;
static std::vector<void *>return_addresses;
static std::vector<size_t>bookmarks;

extern "C"
void
__gg__pseudo_return_push( void *proc_signature,
                          void *return_address)
  {
  proc_signatures.push_back(proc_signature);
  return_addresses.push_back(return_address);
  __gg__exit_address = proc_signature;
  }

extern "C"
void *
__gg__pseudo_return_pop()
  {
  void *retval = return_addresses.back();

  return_addresses.pop_back();
  proc_signatures.pop_back();

  if( proc_signatures.size() > bookmarks.back() )
    {
    __gg__exit_address = proc_signatures.back();
    }
  else
    {
    // We can't go below the floor established by the bookmark.
    __gg__exit_address = NULL;
    }

  return retval;
  }

extern "C"
void
__gg__pseudo_return_bookmark()
  {
  bookmarks.push_back(proc_signatures.size());
  }

extern "C"
void
__gg__pseudo_return_flush()
  {
  if( bookmarks.size() == 0 )
    {
    __gg__abort("__gg__pseudo_return_flush(): bookmarks.size() is zero");
    }
  proc_signatures.resize(bookmarks.back());
  return_addresses.resize(bookmarks.back());
  bookmarks.pop_back();

  if( proc_signatures.size() )
    {
    __gg__exit_address = proc_signatures.back();
    }
  }

extern "C"
GCOB_FP128
__gg__float128_from_location( const cblc_field_t *var,
                              const unsigned char *location)
  {
  GCOB_FP128 retval = 0;
  switch( var->capacity )
    {
    case 4:
      {
      retval = *reinterpret_cast<_Float32 *>(
                                    const_cast<unsigned char *>(location));
      break;
      }

    case 8:
      {
      retval = *reinterpret_cast<_Float64 *>(
                                    const_cast<unsigned char *>(location));
      break;
      }

    case 16:
      {
      //retval = *(_Float128 *)location;
      memcpy(&retval, location, 16);
      break;
      }
    }
  return retval;
  }

extern "C"
__int128
__gg__integer_from_float128(const cblc_field_t *field)
  {
  GCOB_FP128 fvalue = __gg__float128_from_location(field, field->data);
  // we round() to take care of the possible 2.99999999999... problem.
  fvalue = FP128_FUNC(round)(fvalue);
  return (__int128)fvalue;
  }

extern "C"
void
__gg__adjust_dest_size(cblc_field_t *dest, size_t ncount)
  {
  if( dest->attr & (intermediate_e) )
    {
    if( dest->allocated < ncount )
      {
      fprintf(stderr, "libgcobol.cc:__gg__adjust_dest_size(): "
                      "Adjusting %s size upward is not possible.\n",
                      dest->name);
      abort();
//      dest->allocated = ncount;
//      dest->data = (unsigned char *)realloc(dest->data, ncount);
      }
    dest->capacity = ncount;
    }
  }

extern "C"
void
__gg__adjust_encoding(cblc_field_t *field)
  {
  // Assume that field->data is in ASCII;  We need to convert it to the target
  size_t nbytes;
  const char *converted = __gg__iconverter(DEFAULT_SOURCE_ENCODING,
                                           field->encoding,
                                           PTRCAST(char, field->data),
                                           field->capacity,
                                           &nbytes);
  size_t tocopy = std::min(nbytes, field->allocated);
  field->capacity = tocopy;
  memcpy(field->data, converted, tocopy);
  }


extern "C"
void
__gg__func_exception_location(cblc_field_t *dest)
  {
  char ach[512] = " ";
  if( last_exception_code )
    {
    ach[0] = '\0';
    if( last_exception_program_id )
      {
      strcat(ach, last_exception_program_id);
      strcat(ach, "; ");
      }

    if( last_exception_paragraph )
      {
      strcat(ach, last_exception_paragraph );
      if( last_exception_section )
        {
        strcat(ach, " OF ");
        strcat(ach, last_exception_section);
        }
      }
    else
      {
      if( last_exception_section )
        {
        strcat(ach, last_exception_section);
        }
      }
    strcat(ach, "; ");

    if( last_exception_source_file )
      {
      char achSource[128] = "";
      snprintf( achSource,
                sizeof(achSource),
                "%s:%d ",
                last_exception_source_file,
                last_exception_line_number);
      strcat(ach, achSource);
      }
    else
      {
      strcpy(ach, " ");
      }
    }
  __gg__adjust_dest_size(dest, strlen(ach));
  memcpy(dest->data, ach, strlen(ach));
  __gg__adjust_encoding(dest);
  }

extern "C"
void
__gg__func_exception_statement(cblc_field_t *dest)
  {
  char ach[128] = " ";
  if(last_exception_statement)
    {
    snprintf(ach, sizeof(ach), "%s", last_exception_statement);
    ach[sizeof(ach)-1] = '\0';
    }
  __gg__adjust_dest_size(dest, strlen(ach));
  memcpy(dest->data, ach, strlen(ach));
  __gg__adjust_encoding(dest);
  }

extern "C"
void
__gg__func_exception_status(cblc_field_t *dest)
  {
  char ach[128] = "<not in table?>";
  if(last_exception_code)
    {
    ec_descr_t *p = __gg__exception_table;
    while(p < __gg__exception_table_end )
      {
      if( p->type == (ec_type_t)last_exception_code )
        {
        snprintf(ach, sizeof(ach), "%s", p->name);
        break;
        }
      p += 1;
      }
    }
  else
    {
    strcpy(ach, " ");
    }
  __gg__adjust_dest_size(dest, strlen(ach));
  memcpy(dest->data, ach, strlen(ach));
  __gg__adjust_encoding(dest);
  }

extern "C"
void
__gg__set_exception_file(const cblc_file_t *file)
  {
  ec_type_t ec = local_ec_type_of( file->io_status );
  if( ec )
    {
    // During SORT operations, which routinely read files until they end, we
    // need to suppress them.
    if( ec != ec_io_at_end_e || !sv_suppress_eof_ec )
      {
      last_exception_file_operation = file->prior_op;
      last_exception_file_status    = file->io_status;
      last_exception_file_name      = file->name;
      exception_raise(ec);
      }
    }
  }

extern "C"
void
__gg__func_exception_file(cblc_field_t      *dest,
                          const cblc_file_t *file)
  {
  char ach[128];
  if( !file )
    {
    // This is where we process FUNCTION EXCEPTION-FILE <no parameter>
    if( !(last_exception_code & ec_io_e) )
      {
      // There is no EC-I-O exception code, so we return two alphanumeric zeros.
      strcpy(ach, "00");
      }
    else
      {
      // The last exception code is an EC-I-O
      if( sv_from_raise_statement )
        {
        strcpy(ach, "  ");
        }
      else
        {
        snprintf( ach,
                  sizeof(ach), "%2.2d%s",
                  last_exception_file_status,
                  last_exception_file_name);
        }
      }
    }
  else
    {
    // This is where we process FUNCTION EXCEPTION-FILE file->name
    if( file->prior_op == file_op_none )
      {
      // this file hasn't been accessed
      strcpy(ach, "  ");
      }
    else
      {
      snprintf(ach, sizeof(ach), "%2.2d%s", file->io_status, file->name);
      }
    }

  __gg__adjust_dest_size(dest, strlen(ach));
  memcpy(dest->data, ach, strlen(ach));
  __gg__adjust_encoding(dest);
  }

extern "C"
void
__gg__set_exception_code(ec_type_t ec, int from_raise_statement)
  {
  if( MATCH_DECLARATIVE )
    {
    warnx("%s: %s:%u: %s: %s",
          __func__,
          __gg__exception_source_file,
          __gg__exception_line_number,
          __gg__exception_statement,
          local_ec_type_str(ec));
    }
  sv_from_raise_statement = from_raise_statement;

  __gg__exception_code = ec;
  if( ec == ec_none_e)
    {
    last_exception_code           = 0            ;
    last_exception_program_id     = NULL         ;
    last_exception_section        = NULL         ;
    last_exception_paragraph      = NULL         ;
    last_exception_source_file    = NULL         ;
    last_exception_line_number    = 0            ;
    last_exception_statement      = NULL         ;
    last_exception_file_operation = file_op_none ;
    last_exception_file_status    = FsSuccess    ;
    last_exception_file_name      = NULL         ;
    }
  else
    {
    last_exception_code           = __gg__exception_code         ;
    last_exception_program_id     = __gg__exception_program_id   ;
    last_exception_section        = __gg__exception_section      ;
    last_exception_paragraph      = __gg__exception_paragraph    ;
    last_exception_source_file    = __gg__exception_source_file  ;
    last_exception_line_number    = __gg__exception_line_number  ;
    last_exception_statement      = __gg__exception_statement    ;

    // These are set in __gg__set_exception_file just before this routine is
    // called.  In cases where the ec is not a file-i-o operation, we clear
    // them here:
    if( !(ec & ec_io_e) )
      {
      last_exception_file_operation = file_op_none ;
      last_exception_file_status    = FsSuccess  ;
      last_exception_file_name      = NULL    ;
      }
    }
  }

extern "C"
void
__gg__float32_from_int128(cblc_field_t *destination,
                          size_t        destination_offset,
                          cblc_field_t *source,
                          size_t        source_offset,
                          cbl_round_t rounded,
                          int *size_error)
  {
  int rdigits;
  GCOB_FP128 value = get_binary_value_local( &rdigits,
                                            source,
                                            source->data + source_offset,
                                            source->capacity);
  value /= __gg__power_of_ten(rdigits);

  if( FP128_FUNC(fabs)(value) > GCOB_FP128_LITERAL (3.4028235E38) )
    {
    if(size_error)
      {
      *size_error = 1;
      }
    }
  else
    {
    if(size_error)
      {
      *size_error = 0;
      }
    __gg__float128_to_qualified_field(destination,
                                      destination_offset,
                                      value,
                                      rounded,
                                      NULL);
    }
  }

extern "C"
void
__gg__float64_from_int128(cblc_field_t *destination,
                          size_t        destination_offset,
                          cblc_field_t *source,
                          size_t        source_offset,
                          cbl_round_t rounded,
                          int *size_error)
  {
  if(size_error)
    {
    *size_error = 0;
    }
  int rdigits;
  GCOB_FP128 value = get_binary_value_local( &rdigits,
                                            source,
                                            source->data + source_offset,
                                            source->capacity);
  value /= __gg__power_of_ten(rdigits);
  __gg__float128_to_qualified_field(destination,
                                    destination_offset,
                                    value,
                                    rounded,
                                    NULL);
  }

extern "C"
void
__gg__float128_from_int128(cblc_field_t *destination,
                          size_t        destination_offset,
                          cblc_field_t *source,
                          size_t        source_offset,
                          cbl_round_t rounded,
                          int *size_error)
  {
  if(size_error) *size_error = 0;
  int rdigits;
  GCOB_FP128 value = get_binary_value_local( &rdigits,
                                            source,
                                            source->data + source_offset,
                                            source->capacity);
  value /= __gg__power_of_ten(rdigits);
  __gg__float128_to_qualified_field(destination,
                                    destination_offset,
                                    value,
                                    rounded,
                                    NULL);
  }

extern "C"
int
__gg__is_float_infinite(const cblc_field_t *source, size_t offset)
  {
  int retval = 0;
  switch(source->capacity)
    {
    case 4:
      retval = fpclassify(  *reinterpret_cast<_Float32*>(source->data+offset)) == FP_INFINITE;
      break;
    case 8:
      retval = fpclassify(  *reinterpret_cast<_Float64*>(source->data+offset)) == FP_INFINITE;
      break;
    case 16:
      // retval = *(_Float128*)(source->data+offset) == INFINITY;
      GCOB_FP128 t;
      memcpy(&t, source->data+offset, 16);
      retval = t == INFINITY;
      break;
    }
  return retval;
  }

extern "C"
int
__gg__float32_from_128( const cblc_field_t *dest,
                        size_t              dest_offset,
                        const cblc_field_t *source,
                        size_t              source_offset)
  {
  int retval = 0;
  //_Float128 value = *(_Float128*)(source->data+source_offset);
  GCOB_FP128 value;
  memcpy(&value, source->data+source_offset, 16);
  if( FP128_FUNC(fabs)(value) > GCOB_FP128_LITERAL (3.4028235E38) )
    {
    retval = 1;
    }
  else
    {
    *reinterpret_cast<_Float32 *>(dest->data+dest_offset) = (_Float32)value;
    }
  return retval;
  }

extern "C"
int
__gg__float32_from_64(  const cblc_field_t *dest,
                        size_t              dest_offset,
                        const cblc_field_t *source,
                        size_t              source_offset)
  {
  int retval = 0;
  _Float64 value = *reinterpret_cast<_Float64*>(source->data+source_offset);
  if( FP128_FUNC(fabs)(value) > GCOB_FP128_LITERAL (3.4028235E38) )
    {
    retval = 1;
    }
  else
    {
    *reinterpret_cast<_Float32 *>(dest->data+dest_offset) = (_Float32)value;
    }
  return retval;
  }

extern "C"
int
__gg__float64_from_128( const cblc_field_t *dest,
                        size_t              dest_offset,
                        const cblc_field_t *source,
                        size_t              source_offset)
  {
  int retval = 0;
  // _Float128 value = *(_Float128*)(source->data+source_offset);
  GCOB_FP128 value;
  memcpy(&value, source->data+source_offset, 16);
  if( FP128_FUNC(fabs)(value) > GCOB_FP128_LITERAL(1.7976931348623157E308) )
    {
    retval = 1;
    }
  else
    {
    *reinterpret_cast<_Float64 *>(dest->data+dest_offset) = (_Float64)value;
    }
  return retval;
  }

extern "C"
char *
__gg__display_int128(__int128 value)
  {
  static char ach[64];

  if( value == 0 )
    {
    strcpy(ach, "0");
    return ach;
    }

  bool is_negative = false;
  if( value < 0 )
    {
    is_negative = true;
    value = -value;
    }
  char *p = ach+64;
  *--p = '\0';

  while(value)
    {
    *--p = value%10 + '0';
    value /= 10;
    }
  if( is_negative )
    {
    *--p = '-';
    }
  return p;
  }

typedef struct LV_DATA
  {
  size_t         unique_id;
  cblc_field_t  *field;
  unsigned char *data;
  }LV_DATA;

static std::vector<LV_DATA> lv_variables;

extern "C"
void
__gg__push_local_variable(cblc_field_t *field)
  {
  LV_DATA lv_data;
  lv_data.unique_id     = __gg__unique_prog_id;
  lv_data.field         = field;
  lv_data.data          = field->data;
  lv_variables.push_back(lv_data);
  }

extern "C"
void
__gg__pop_local_variables()
  {
  while(     lv_variables.size()
          && lv_variables.back().unique_id == __gg__unique_prog_id)
    {
    lv_variables.back().field->data = lv_variables.back().data;
    lv_variables.pop_back();
    }
  }

extern "C"
void
__gg__copy_as_big_endian( unsigned char *dest,
                          const unsigned char *source)
  {
  // copy eight bytes of source to dest, flipping the endianness
  for(size_t i=0; i<8; i++)
    {
    dest[i] = source[7-i];
    }
  }

extern "C"
void
__gg__codeset_figurative_constants()
  {
  // This routine gets called after the codeset has been changed

  // __gg__data_space and __gg__data_zeros don't change because they are
  // permanently encoded as iconv_CP1252_e.  These other three can be changed
  // as either compiler options or ALPHABET clauses.
  *__gg__data_low_values  = __gg__low_value_character;
  *__gg__data_high_values = __gg__high_value_character;
  *__gg__data_quotes      = __gg__quote_character;
  }

extern "C"
unsigned char *
__gg__get_figconst_data(const cblc_field_t *field)
  {
  unsigned char *retval = NULL;
  cbl_figconst_t figconst = (cbl_figconst_t)(size_t)(field->initial);
  switch(figconst)
    {
    case low_value_e    :
      retval = __gg__data_low_values;
      break;
    case zero_value_e   :
      retval = __gg__data_zeros;
      break;
    case space_value_e  :
      retval = __gg__data_space;
      break;
    case quote_value_e  :
      retval = __gg__data_quotes;
      break;
    case high_value_e   :
      retval = __gg__data_high_values;
      break;
    case normal_value_e :
      fprintf(stderr, "__gg__get_figconst_data(): Weird figconst\n");
      abort();
      break;
    case null_value_e:
      break;
    }
  return retval;
  }

extern "C"
void
__gg__set_program_list( int program_id,
                        char ***prog_list,
                        PFUNC **prog_pointers)
  {
  accessible_programs[program_id]  = prog_list;
  accessible_pointers[program_id]  = prog_pointers;
  }

static std::unordered_map<std::string, void *> already_found;

static
void *
find_in_dirs(const char *dirs, char *unmangled_name, char *mangled_name)
  {

  std::unordered_map<std::string, void *>::const_iterator it =
                    already_found.find(unmangled_name);

  if( it != already_found.end() )
    {
    return it->second;
    }
  it = already_found.find(mangled_name);
  if( it != already_found.end() )
    {
    return it->second;
    }

  void *retval = NULL;
  if( dirs )
    {
    char directory[1024];
    char file[1024];
    const char *p = dirs;
    while( !retval && *p )
      {
      size_t index = 0;
      while( index < sizeof(directory)-1 && *p && *p != ':' )
        {
        directory[index++] = *p++;
        }
      directory[index++] = '\0';
      if( *p == ':' )
        {
        p += 1;
        }
      // directory is the next one for us to check:
      DIR *dir = opendir(directory);
      if( dir )
        {
        while( !retval )
          {
          const dirent *entry = readdir(dir);
          if( !entry )
            {
            break;
            }
          size_t len = strlen(entry->d_name);
          if(    len > 3
              && entry->d_name[len-3] == '.'
              && entry->d_name[len-2] == 's'
              && entry->d_name[len-1] == 'o'
              )
            {
            strcpy(file, directory);
            strcat(file, "/");
            strcat(file, entry->d_name);
            void *handle = dlopen(file, RTLD_LAZY|RTLD_NODELETE );
            if( handle )
              {
              retval = dlsym(handle, unmangled_name);
              if( retval )
                {
                already_found[unmangled_name] = retval;
                break;
                }
              retval = dlsym(handle, mangled_name);
              if( retval )
                {
                already_found[mangled_name] = retval;
                break;
                }
              dlclose(handle);
              }
            }
          }
        closedir(dir);
        }
      }
    }
  return retval;
  }

extern "C"
void *
__gg__function_handle_from_cobpath( char *unmangled_name, char *mangled_name)
  {
  void *retval;

  // We search for a function.  We check first for the unmangled name, and then
  // the mangled name.  We do this first for the executable, then for .so
  // files in COBPATH, and then for files in LD_LIBRARY_PATH

  static void *handle_executable = NULL;
  if( !handle_executable )
    {
    handle_executable = dlopen(NULL, RTLD_NOW);
    }
  retval = dlsym(handle_executable, unmangled_name);
  if( !retval )
    {
    retval = dlsym(handle_executable, mangled_name);
    }
  if( !retval )
    {
    const char *COBPATH = getenv("GCOBOL_LIBRARY_PATH");
    retval = find_in_dirs(COBPATH, unmangled_name, mangled_name);
    }
  if( !retval )
    {
    const char *LD_LIBRARY_PATH = getenv("LD_LIBRARY_PATH");
    retval = find_in_dirs(LD_LIBRARY_PATH, unmangled_name, mangled_name);
    }

  return retval;
  }

extern "C"
void
__gg__just_mangle_name( const cblc_field_t  *field,
                        char               **mangled_name
                        )
  {
  static char ach_name[1024];
  static char ach_unmangled[1024];
  static char ach_mangled[1024];

  assert(field);
  assert(field->data);

  size_t         length;
  length = field->capacity;

  // We need ach_name to be in ASCII:
  size_t charsout;
  const char *converted = __gg__iconverter(field->encoding,
                                           __gg__console_encoding,
                                           PTRCAST(char, field->data),
                                           length,
                                           &charsout);
  memcpy(ach_name, converted, charsout);
  ach_name[charsout] = '\0';

  bool is_pointer = false;

  if( field->type == FldPointer )
    {
    is_pointer = true;
    }

  if( is_pointer )
    {
    strcpy(ach_name, "<pointer>");
    }

  // At this point we have a null-terminated ascii function name.

  // Convert it to both unmangled and mangled forms:
  strcpy(ach_unmangled, not_mangled_core(ach_name, ach_name+length));
  strcpy(ach_mangled,   mangler_core(    ach_name, ach_name+length));

  if( mangled_name )
    {
    *mangled_name = ach_mangled;
    }
  }

extern "C"
void *
__gg__function_handle_from_literal(int         program_id,
                                   const char *literal)
  {
  void *retval = NULL;
  static char ach_unmangled[1024];
  static char ach_mangled[1024];

  size_t         length;

  length = strlen(literal);

  // At this point we have a null-terminated ascii function name.

  // Convert it to both unmangled and mangled forms:
  strcpy(ach_unmangled, not_mangled_core(literal, literal+length));
  strcpy(ach_mangled,   mangler_core(    literal, literal+length));

  int function_index = __gg__routine_to_call(ach_mangled, program_id);
  if( function_index > -1 )
    {
    std::unordered_map<int, PFUNC**>::const_iterator it =
           accessible_pointers.find(program_id);
    if( it == accessible_pointers.end() )
      {
      __gg__abort("__gg__function_handle_from_literal():"
                  " fell off the end of accessible_pointers");
      }
    PFUNC **pointers_p = it->second;
    PFUNC  *pointers   = *pointers_p;
    retval = reinterpret_cast<void *>(pointers[function_index]);
    }
  else
    {
    retval = __gg__function_handle_from_cobpath(ach_unmangled, ach_mangled);
    }

  return retval;
  }

extern "C"
void *
__gg__function_handle_from_name(int                 program_id,
                                const cblc_field_t *field,
                                size_t              offset,
                                size_t              length )
  {
  void *retval = NULL;
  static char ach_name[1024];
  static char ach_unmangled[1024];
  static char ach_mangled[1024];

  if( length == 0 )
    {
    length = field->capacity;
    }

  size_t charsout;
  const char *converted = __gg__iconverter(field->encoding,
                                           DEFAULT_SOURCE_ENCODING,
                                           PTRCAST(char, field->data + offset),
                                           length,
                                           &charsout);
  memcpy(ach_name, converted, length);
  char *p = strchr(ach_name, ascii_space);
  if(p)
    {
    *p = '\0';
    }
  length = strlen(ach_name);

  // At this point we have a null-terminated ascii function name.

  // Convert it to both unmangled and mangled forms:
  strcpy(ach_unmangled, not_mangled_core(ach_name, ach_name+length));
  strcpy(ach_mangled,   mangler_core(    ach_name, ach_name+length));

  int function_index = __gg__routine_to_call(ach_mangled, program_id);
  if( function_index > -1 )
    {
    std::unordered_map<int, PFUNC**>::const_iterator it =
           accessible_pointers.find(program_id);
    if(it == accessible_pointers.end())
      {
      __gg__abort("__gg__function_handle_from_name():"
                  " fell off the end of accessible_pointers");
      }
    PFUNC **pointers_p = it->second;
    PFUNC  *pointers   = *pointers_p;
    retval = reinterpret_cast<void *>(pointers[function_index]);
    }
  else
    {
    retval = __gg__function_handle_from_cobpath(ach_unmangled, ach_mangled);
    }

  return retval;
  }

extern "C"
void
__gg__variables_to_init(cblc_field_t *array[], const char *clear)
  {
  int i=0;
  for(;;)
    {
    cblc_field_t *field = array[i++];
    if( !field )
      {
      break;
      }
    int flag_bits  = 0;
    flag_bits     |=  clear
                      ? DEFAULTBYTE_BIT + (*clear & DEFAULT_BYTE_MASK)
                      : 0;
    __gg__initialize_variable_clean(field, flag_bits);
    }
  }

extern "C"
void
__gg__mirror_range( size_t         nrows,
                    cblc_field_t  *src,     // The row
                    size_t         src_o,
                    size_t         nspans,  // The number of spans
                    const size_t  *spans,
                    size_t         table,
                    size_t         ntbl,
                    const size_t  *tbls)
  {
  static std::unordered_map<size_t, size_t> rows_in_table;
  static std::unordered_map<size_t, size_t> widths_of_table;
  static std::unordered_map<size_t, std::vector<size_t>> spans_in_table;

  // Let's do the memorization bookkeeping for the target table
  // We have to do this every time, because if we have memory, everything gets
  // higgledepiggledy when INITIALZE ALL FILLER is followed by
  // INITIALIZE ALL VALUE
  std::vector<size_t> spans_this_time(2*nspans);
  for(size_t i=0; i<2*nspans; i++)
    {
    spans_this_time[i] = spans[i];
    }
  rows_in_table[table]  = nrows;
  spans_in_table[table] = spans_this_time;

  // We need to know the width of one row of this table, which is different
  // depending on type of src:

  const cblc_field_t *parent = src;
  while( parent )
    {
    if( parent->occurs_upper )
      {
      break;
      }
    }
  if( parent == nullptr )
    {
    __gg__abort("__gg__mirror_range() parent is NULL");
    }
  size_t width;
  if( parent->type == FldGroup )
    {
    width = parent->capacity;
    }
  else
    {
    width = parent->capacity * parent->occurs_upper;
    }
  widths_of_table[table] = width;

  if( nspans == 0 && ntbl == 0 )
    {
    // There are no FILLERS to be skipped, so we can just do our cute line
    // duplicating trick.  We understand that there isn't actually much in the
    // the way of computational savings -- every byte has to be written, after
    // all -- but it is satisfying.
    size_t stride = src->capacity;
    unsigned char *source = src->data + src_o;
    unsigned char *dest   = source + stride;
    nrows -= 1; // This reflects that row[0] equals row[0]

    size_t nrows_this_time = 1;
    while(nrows)
      {
      memcpy(dest, source, nrows_this_time * stride);
      dest += nrows_this_time * stride;
      nrows -= nrows_this_time;
      nrows_this_time *= 2;
      nrows_this_time = std::min(nrows_this_time, nrows);
      }
    }
  else
    {
    size_t stride = src->capacity;
    unsigned char *source = src->data + src_o;
    unsigned char *dest   = source;

    while( nrows-- )
      {
      // These spans are for non-table elements
      if( nspans == 0 )
        {
        // Special case: We have no spans, but in the case of, for example,
        //    01 four-by-four2.
        //      05 label5 pic x(12) value "four-by-four".
        //      05 four-outer2 occurs 4 times.
        //        10 four-inner2 occurs 4 times.
        //          15 label15 pic x(12) value "four-inner".
        //          15 FNAME  PIC X(7) VALUE "James".
        //          15 FILLER PIC X(7) VALUE "Keen ".
        //          15 LNAME  PIC X(7) VALUE "Lowden".
        //        10 label10 pic x(12) value "four-outer".
        //
        // the label10 field *should* be a span.  The parser doesn't do that,
        // though, so we have to:
        size_t left  = 0;
        size_t right = stride;
        for(size_t subtable=0; subtable<ntbl; subtable++)
          {
          size_t subtable_offset = tbls[2*subtable  ];
          size_t subtable_index  = tbls[2*subtable+1];

          if( widths_of_table.find(subtable_index) == widths_of_table.end() )
            {
            __gg__abort("__gg__mirror_range() fell off widths_of_table");
            }
          size_t subtable_width = widths_of_table[subtable_index];
          size_t subtable_rows   = rows_in_table  [subtable_index];

          right = subtable_offset;

          memcpy(dest   + left,
                 source + left ,
                 right - left);
          left = right + subtable_rows * subtable_width;
          // Set right to stride, in case this is the final 'subtable' iteration
          right = stride;
          }

        if( left < right )
          {
          memcpy(dest   + left,
                 source + left ,
                 right - left);
          }
        }
      for(size_t span=0; span<nspans; span++)
        {
        size_t offset = spans[2*span];
        size_t length = spans[2*span + 1] - offset;
        memcpy( dest+offset, source+offset, length);
        }

      // Our table's row might have sub-tables:
      for(size_t subtable=0; subtable<ntbl; subtable++)
        {
        size_t subtable_offset = tbls[2*subtable  ];
        size_t subtable_index  = tbls[2*subtable+1];

        if( widths_of_table.find(subtable_index) == widths_of_table.end() )
          {
          __gg__abort("__gg__mirror_range(): fell off widths of table");
          }

        size_t subtable_stride = widths_of_table[subtable_index];
        size_t subtable_rows   = rows_in_table  [subtable_index];
        std::vector<size_t> subtable_spans
                                = spans_in_table [subtable_index];

        const unsigned char *subtable_source = source + subtable_offset;

        if( subtable_spans.size() == 0 )
          {
          unsigned char *subtable_dest   = dest   + subtable_offset;
          size_t span_start   = 0;
          size_t span_end     = subtable_stride;
          size_t subtable_row = subtable_rows;
          while( subtable_row-- )
            {
            memcpy(subtable_dest   + span_start,
                   subtable_source + span_start,
                   span_end - span_start);
            subtable_dest += subtable_stride;
            }
          }

        for(size_t span=0; span<subtable_spans.size(); span += 2 )
          {
          unsigned char *subtable_dest   = dest   + subtable_offset;
          size_t span_start   = subtable_spans[span  ];
          size_t span_end     = subtable_spans[span+1];
          size_t subtable_row = subtable_rows;
          while( subtable_row-- )
            {
            memcpy(subtable_dest   + span_start,
                   subtable_source + span_start,
                   span_end - span_start);
            subtable_dest += subtable_stride;
            }
          }
        }
      dest += stride;
      }
    }
  }

extern "C"
void
__gg__sleep(cblc_field_t *field, size_t offset, size_t size)
  {
  int rdigits;
  __int128 value = get_binary_value_local( &rdigits,
                                           field,
                                           field->data + offset,
                                           size);
  double delay = (double)value / __gg__power_of_ten(rdigits);
  if( delay < 0 )
    {
    exception_raise(ec_continue_less_than_zero);
    delay = 0;
    }

  // Convert the time to nanoseconds.
  delay = delay * 1000000000;

  // Convert the result to seconds/nanoseconds for nanosleep()
  size_t tdelay = (size_t)delay;
  timespec duration;

  duration.tv_sec  = tdelay / 1000000000;
  duration.tv_nsec = tdelay % 1000000000;

  nanosleep(&duration, NULL);
  }

extern "C"
void
__gg__deallocate( cblc_field_t *target,
                  size_t        offset,
                  int           addr_of)
  {
  if( addr_of || (target->attr & based_e))
    {
    // Free the target's data pointer
    if( target->data )
      {
      free(target->data);
      target->data = NULL;
      }
    }
  else if (target->type == FldPointer)
    {
    // Target is a pointer.  Free the data location
    int rdigits;
    size_t addrv =  get_binary_value_local(&rdigits,
                                               target,
                                               target->data + offset,
                                               sizeof(void *));
    void *ptr = reinterpret_cast<void *>(addrv);
    if( ptr )
      {
      free(ptr);
      // And set the data location to zero
      *static_cast<char **>(static_cast<void *>(target->data + offset))
                                                                      = NULL;
      }
    }
  }

static int
get_the_byte(cblc_field_t *field)
  {
  // This is a helper routine for ALLOCATE
  int retval = -1;
  if( field )
    {
    // Get the encoded character associated with the figconst
    retval = __gg__fc_char(field);
    if(retval == NOT_A_CHARACTER)
      {
      retval = (int)(unsigned char)__gg__get_integer_binary_value(field);
      }
    else
      {
      // This is a bit of a hack.  It turns out the figurative constant is
      // encoded in ASCII.  We need it to be in the current DISPLAY encoding.
      charmap_t *charmap = __gg__get_charmap(__gg__display_encoding);
      retval = charmap->mapped_character(retval);
      }
    }
  return retval;
  }

extern "C"
void
__gg__allocate( cblc_field_t       *first,
                size_t              first_offset,
                int                 initialized,
                int                 default_byte,
                cblc_field_t       *f_working_byte,
                cblc_field_t       *f_local_byte,
                const cblc_field_t *returning,
                size_t              returning_offset)
  {
  int working_byte = get_the_byte(f_working_byte);
  int local_byte   = get_the_byte(f_local_byte);
  int fill_char;

  unsigned char *retval = NULL;
  if( first->attr & based_e )
    {
    // first is the BASED variable we are allocating memory for
    if( first->capacity )
      {
      retval = static_cast<unsigned char *>(malloc(first->capacity));

      fill_char = 0;
      if( initialized )
        {
        // This is ISO 2023 ALLOCATE rule 7 (ALL TO VALUE)
        if( default_byte >= 0 )
          {
          fill_char = default_byte;
          memset(retval, fill_char, first->capacity);
          }
        }
      else
        {
        // This is ISO 2023 ALLOCATE rule 9 (pointers NULL, otherwise OPT_INIT)
        if( default_byte >= 0 )
          {
          fill_char = default_byte;
          }
        if( first->attr & (linkage_e | local_e) )
          {
          if( local_byte >= 0 )
            {
            fill_char = local_byte;
            }
          }
        else
          {
          if( working_byte >= 0 )
            {
            fill_char = working_byte;
            }
          }
        memset(retval, fill_char, first->capacity);
        }
      }
    first->data = retval;
    }
  else
    {
    // This is an ALLOCATE CHARACTERS
    // first contains the number of bytes to allocate
    int rdigits;
    ssize_t tsize = (ssize_t)get_binary_value_local(&rdigits,
                                                    first,
                                                    first->data + first_offset,
                                                    sizeof(void *));
    tsize = std::max(0L, tsize);
    size_t pof10 = __gg__power_of_ten(rdigits);

    // If there are any non-zero digits to the right of the decimal point,
    // increment the units place:
    tsize += (pof10-1);

    // Adjust the result to be an integer.
    tsize /= pof10;
    if( tsize )
      {
      retval = static_cast<unsigned char *>(malloc(tsize));
      if(!retval)
        {
        abort();
        }

      fill_char = 0;
      if( initialized )
        {
        // This is ISO 2023 rule 6 (defaultbyte if specified, else zero)
        if( default_byte >= 0 )
          {
          fill_char = default_byte;
          }
        }
      else
        {
        // This is ISO 2023 rule 8 (OPT_INIT if specified, otherwise defaultbyte, otherwise zero)")
        if( default_byte >= 0 )
          {
          fill_char = default_byte;
          }
        if( first->attr & (linkage_e | local_e) )
          {
          if( local_byte >= 0 )
            {
            fill_char = local_byte;
            }
          }
        else
          {
          if( working_byte >= 0 )
            {
            fill_char = working_byte;
            }
          }
        }

      memset(retval, fill_char, tsize);
      }
    }

  if( returning )
    {
    // 'returning' has to be a FldPointer variable; assign the retval to it.
    *reinterpret_cast<unsigned char **>(returning->data + returning_offset) = retval;
    }
  }

static std::vector<std::string>module_name_stack;

extern "C"
void
__gg__module_name_push(const char *module_name)
  {
  module_name_stack.push_back(module_name);
  }

extern "C"
void
__gg__module_name_pop()
  {
  if( module_name_stack.size() == 0 )
    {
    __gg__abort("__gg__module_name_pop(): module_name_stack is empty");
    }
  module_name_stack.pop_back();
  }

extern "C"
void
__gg__module_name(cblc_field_t *dest, module_type_t type)
  {
  static size_t result_size = 64;
  static char *result = static_cast<char *>(malloc(result_size));
  massert(result);

  strcpy(result, "");

  size_t ssize = module_name_stack.size();

  switch( type )
    {
    case module_activating_e:
    /* 5) If the ACTIVATING keyword is specified and the function is in a COBOL
    main program, then the returned value shall be a single space. The
    implementor shall document how a main program is identified. If the function
    is not specified in a main program, then the returned value is the name of
    the runtime element that activated the currently running runtime element.
    This may be by a CALL statement, an INVOKE statement, a function reference,
    or an inline invocation.  */

    /* 6) If the ACTIVATING keyword is specified and the activating statement
    is within a nested program, then it is implementor defined what value is
    returned. This may be the name of the nested program or the name of the
    outermost program containing the nested program.  */

    switch( module_name_stack.back()[0] )
      {
      case 'T':
        // We are in a top-level program, not nested:
        if(    module_name_stack.size() == 1
            || (module_name_stack.size() == 2
               && module_name_stack.front()[0] == 'M' ) )
          {
          // This is a "main program", so we return a single space.
          strcpy(result, " ");
          }
        else
          {
          // This is a called program, so we return the name of the "runtime
          // element" that called us:
          strcpy(result, module_name_stack[ssize-2].substr(1).c_str());
          }
        break;

      case 'N':
        // We are in a nested function.
        {
        // This is a called program, so we return the name of the program that
        // called us
        strcpy(result, module_name_stack[ssize-2].substr(1).c_str());
        }
      }
      break;

    case module_current_e:
      // 7) If the CURRENT keyword is specified then the returned value is the
      // name of the runtime element of the outermost program of the compilation
      // unit’s code that is currently running.

      // Look upward for our parent T.
      // Termination is weird because size_t is unsigned
      for(size_t i=ssize-1; i<ssize; i--)
        {
        if( module_name_stack[i][0] == 'T' )
          {
          strcpy(result, module_name_stack[i].substr(1).c_str());
          break;
          }
        }
      break;

    case module_nested_e:
      // 8) If the NESTED keyword is specified, then the returned value is the
      // name, as specified in the PROGRAM-ID, of the currently running, most
      // recently nested program.

      // This specification seems weird to me.  What if the currently running
      // program isn't nested?

      // So, we'll just return us
      strcpy(result, module_name_stack[ssize-1].substr(1).c_str());
      break;

    case module_stack_e:
      for(size_t i=ssize-1; i<ssize; i--)
        {
        if(    module_name_stack[i][0] == 'T'
            || module_name_stack[i][0] == 'N' )
          {
          if( strlen(result) + module_name_stack[i].substr(1).length() + 4 > result_size)
            {
            result_size *= 2;
            result = static_cast<char *>(realloc(result, result_size));
            }
          strcat(result, module_name_stack[i].substr(1).c_str());
          strcat(result, ";");
          }
        }
      strcat(result, " ");

      break;

    case module_toplevel_e:
      {
      size_t i = 0;
      if( module_name_stack[i] == "Mmain" )
        {
        i += 1;
        }
      strcpy(result, module_name_stack[i].substr(1).c_str());
      }
      break;
    }

  __gg__adjust_dest_size(dest, strlen(result));
  memcpy(dest->data, result, strlen(result)+1);
  __gg__adjust_encoding(dest);
  }

/*
 * Runtime functions defined for cbl_enabled_exceptions_t
 */
cbl_enabled_exceptions_t&
cbl_enabled_exceptions_t::decode( const std::vector<uint64_t>& encoded ) {
  auto p = encoded.begin();
  while( p != encoded.end() ) {
    auto location = static_cast<bool>(*p++);
    auto ec = static_cast<ec_type_t>(*p++);
    auto file = *p++;
    cbl_enabled_exception_t enabled(location, ec, file);
    insert(enabled);
  }
  return *this;
}
const cbl_enabled_exception_t *
cbl_enabled_exceptions_t::match( ec_type_t type, size_t file ) const {
  auto output = enabled_exception_match( begin(), end(), type, file );

  if( output != end() ) {
    if( MATCH_DECLARATIVE )
      warnx("          enabled_exception_match found %x in input\n", type);
    return &*output;
  }
  return nullptr;
}

void
cbl_enabled_exceptions_t::dump( const char tag[] ) const {
  if( empty() ) {
    warnx("%s:  no enabled exceptions", tag );
    return;
  }
  int i = 1;
  for( auto& elem : *this ) {
    warnx("%s: %2d  {%s, %04x %s, %u}", tag,
	  i++,
	  elem.location? "with location" : "  no location",
	  elem.ec,
	  local_ec_type_str(elem.ec),
	  static_cast<unsigned int>(elem.file) );
  }
}


static std::vector<cbl_declarative_t>&
decode( std::vector<cbl_declarative_t>& dcls,
        const std::vector<uint64_t>& encoded ) {
  auto p = encoded.begin();
  while( p != encoded.end() ) {
    auto section = static_cast<size_t>(*p++);
    auto global = static_cast<bool>(*p++);
    auto type = static_cast<ec_type_t>(*p++);
    auto nfile = static_cast<uint32_t>(*p++);
    std::list<size_t> files;
    assert(nfile <= cbl_declarative_t::files_max);
    auto pend = p + nfile;
    std::copy(p, pend, std::back_inserter(files));
    p += cbl_declarative_t::files_max;
    auto mode = cbl_file_mode_t(*p++);
    cbl_declarative_t dcl( section, type, files, mode, global );
    dcls.push_back(dcl);
  }
  return dcls;
}

static std::vector<cbl_declarative_t>&
operator<<( std::vector<cbl_declarative_t>& dcls,
            const std::vector<uint64_t>& encoded ) {
  return decode( dcls, encoded );
}

// The first element of each array is the number of elements that follow
// The first element of each array is the number of elements that follow
extern "C"
void
__gg__set_exception_environment( uint64_t *ecs, uint64_t *dcls )
  {
  static struct prior_t {
    uint64_t *ecs = nullptr, *dcls = nullptr;
  } prior;

  if( MATCH_DECLARATIVE )
    if( prior.ecs != ecs || prior.dcls != dcls )
      warnx("set_exception_environment: %s: %p, %p",
            __gg__exception_statement, ecs, dcls);

  if( ecs ) {
    if( prior.ecs != ecs ) {
      uint64_t *ecs_begin = ecs + 1, *ecs_end = ecs_begin + ecs[0];
      if( MATCH_DECLARATIVE ) {
        warnx("%u elements implies %u ECs",
	      static_cast<unsigned int>(ecs[0]),
	      static_cast<unsigned int>(ecs[0] / 3));
      }
      cbl_enabled_exceptions_t enabled;
      enabled_ECs = enabled.decode( std::vector<uint64_t>(ecs_begin, ecs_end) );
      if( MATCH_DECLARATIVE ) enabled_ECs.dump("set_exception_environment");
    }
  } else {
    enabled_ECs.clear();
  }

  if( dcls ) {
    if( prior.dcls != dcls ) {
      uint64_t *dcls_begin = dcls + 1, *dcls_end = dcls_begin + dcls[0];
      if( MATCH_DECLARATIVE ) {
        warnx("%u elements implies %u declaratives",
	      static_cast<unsigned int>(dcls[0]),
	      static_cast<unsigned int>(dcls[0] / 21));
      }
      declaratives.clear();
      declaratives << std::vector<uint64_t>( dcls_begin, dcls_end );
    }
  } else {
    declaratives.clear();
  }

  __gg__exception_code = ec_none_e;

  prior.ecs = ecs;
  prior.dcls = dcls;
  }

static char *sv_envname = NULL;

extern "C"
void
__gg__set_env_name( const cblc_field_t *var,
                    size_t              offset,
                    size_t              length )
  {
  // implements DISPLAY UPON ENVIRONMENT-NAME
  free(sv_envname);
  sv_envname = static_cast<char *>(malloc(length+1));
  massert(sv_envname);

  // We need to convert the name to the console encoding:
  size_t charsout;
  const char *converted = __gg__iconverter(var->encoding,
                                           __gg__console_encoding,
                                           PTRCAST(char, var->data+offset),
                                           length,
                                           &charsout);
  memcpy(sv_envname, converted, length);
  sv_envname[length] = '\0';
  brute_force_trim(sv_envname, __gg__console_encoding);
  }


extern "C"
void
__gg__get_env_name( cblc_field_t *dest,
                    size_t dest_offset,
                    size_t dest_length)
  {
  // Implements ACCEPT FROM ENVIRONMENT-NAME
  // It returns the value previously established by __gg__set_env_name.
  if( !sv_envname )
    {
    sv_envname = strdup("");
    }
  move_string(dest,
              dest_offset,
              dest_length,
              sv_envname,
              __gg__console_encoding);
  }

extern "C"
int
__gg__get_env_value(cblc_field_t *dest,
                    size_t dest_offset,
                    size_t dest_length)
  {
  return accept_envar(dest,
                      dest_offset,
                      dest_length,
                      sv_envname,
                      __gg__console_encoding);
  }

extern "C"
void
__gg__set_env_value(const cblc_field_t *value,
                    size_t              offset,
                    size_t              length )
  {
  // implements DISPLAY UPON ENVIRONMENT-VALUE
  size_t value_length = length;

  static size_t  val_length = 0;
  static char   *val        = nullptr;
  if( val_length < length+1 )
    {
    val_length = length+1;
    val = static_cast<char *>(realloc(val, val_length));
    }
  massert(val);

  memcpy(val, value->data+offset, value_length);
  val[value_length] = '\0';

  __gg__convert_encoding( val,
                          value->encoding,
                          __gg__console_encoding);


  // Get rid of leading and trailing space characters
  char *trimmed_val = brute_force_trim(val, __gg__console_encoding);

  // And now, anticlimactically, set the variable:
  if( sv_envname )
    {
    setenv(sv_envname, trimmed_val, 1);
    }
  }

extern "C"
void
__gg__fprintf_stderr(const char *format_string, ...)
  __attribute__ ((__format__ (__printf__, 1, 2)));

extern "C"
void
__gg__fprintf_stderr(const char *format_string, ...)
  {
  /*  This routine allows the compiler to send stuff to stderr in a way
      that is straightforward to use..  */
  va_list ap;
  va_start(ap, format_string);
  vfprintf(stderr, format_string, ap);
  va_end(ap);
  }

static int sv_argument_number = 0;

extern "C"
void
__gg__set_arg_num( const cblc_field_t *index,
                   size_t              index_offset,
                   size_t              index_size )
  {
  // Implements DISPLAY UPON ARGUMENT-NUMBER.
  int rdigits;
  __int128 N = get_binary_value_local(&rdigits,
                                      index,
                                      index->data + index_offset,
                                      index_size);
  // If he gives us fractional digits, just truncate
  N /= __gg__power_of_ten(rdigits);

  // N is 1-based, per normal COBOL.  We have to decrement it here:
  N -= 1;
  sv_argument_number = static_cast<int>(N);
  }

extern "C"
int
__gg__accept_arg_value( cblc_field_t *dest,
                        size_t dest_offset,
                        size_t dest_length)
  {
  // Implements ACCEPT FROM ARGUMENT-VALUE
  int retcode;
  command_line_plan_b();
  if( sv_argument_number >= stashed_argc || sv_argument_number < 0 )
    {
    exception_raise(ec_argument_imp_command_e);
    retcode = 1;  // Error
    }
  else
    {
    move_string(dest,
                dest_offset,
                dest_length,
                stashed_argv[sv_argument_number],
                DEFAULT_SOURCE_ENCODING);
    retcode = 0;  // Okay

    // The Fujitsu spec says bump this value by one.
    sv_argument_number += 1;
    }
  return retcode;
  }

extern "C"
int
__gg__get_file_descriptor(const char *device)
  {
  int retval = open(device, O_WRONLY);

  if( retval == -1 )
    {
    char *msg;
    int ec = asprintf(&msg,
                      "Trying to open %s.  Got error %s",
                      device,
                      strerror(errno));
    if( ec != -1 )
      {
      static const int priority = LOG_INFO,
                         option = LOG_PERROR,
                       facility = LOG_USER;
      open_syslog(option, facility);
      syslog(priority, "%s", msg);
      }

    // Open a new handle to /dev/stdout, since our caller will be closing it
    retval = open("/dev/stdout", O_WRONLY);
    }
  return retval;
  }

int
__gg__fc_char(const cblc_field_t *field)
  {
  // This returns the figconst character for a field, if the field->attr
  // indicates that the field is a figconst.  Otherwise, it comes back -1
  int retval = NOT_A_CHARACTER;
  charmap_t *charmap = __gg__get_charmap(field->encoding);
  cbl_figconst_t figconst = (cbl_figconst_t)(field->attr & FIGCONST_MASK);
  if( figconst )
    {
    retval = charmap->figconst_character(figconst);
    }
  return retval;
  }

extern "C"
void
__gg__refer_from_string(cblc_field_t *field,
                         size_t field_offset,
                         size_t field_size,
                   const char *string)
  {
  // 'string' has to be in the 'field' encoding.  Use this when the input
  // might, or might not, be nul-terminated, and you don't want a
  // nul-terminator in the data of the target field.
  charmap_t *charmap = __gg__get_charmap(field->encoding);
  size_t nbytes = charmap->strlen(string, field_size);
  __gg__field_from_string(field, field_offset, field_size, string, nbytes);
  }

extern "C"
void
__gg__refer_from_psz(cblc_field_t *field,
                     size_t field_offset,
                     size_t field_size,
               const char *string)
  {
  // 'string' has to be in the 'field' encoding.  Use this when the input
  // might, or might not, be nul-terminated, and you *do* want a
  // nul-terminator in the data of the target field if there was one in the
  // input.

  // One typical use is processing returned values from external C-style
  // functions, which often return nul-terminated strings.
  charmap_t *charmap = __gg__get_charmap(field->encoding);
  size_t nbytes = charmap->strlen(string, field_size);
  __gg__field_from_string(field,
                    field_offset,
                    field_size,
                    string,
                    nbytes);
  }


extern "C"
void
__gg__find_string(        cblc_field_t *dest,
                    const cblc_field_t *haystack,
                          size_t        haystack_o,
                          size_t        haystack_s,
                    const cblc_field_t *needle,
                          size_t        needle_o,
                          size_t        needle_s,
                    const cblc_field_t *after,
                          size_t        after_o,
                          size_t        after_s,
                          bool          last,
                          bool          anycase)
  {
  int retval = 0;
  cbl_encoding_t encoding = dest->encoding;
  std::u32string str_id1 = normalize_for_inspect_format_4(
                                                  haystack,
                                                  haystack_o,
                                                  haystack_s,
                                                  encoding);
  std::u32string str_id2 = normalize_for_inspect_format_4(
                                                  needle,
                                                  needle_o,
                                                  needle_s,
                                                  encoding);
  if( !str_id1.empty() && !str_id2.empty() )
    {
    if( anycase )
      {
      std::transform( str_id1.begin(),
                      str_id1.end(),
                      str_id1.begin(),
                      std::towlower);
      std::transform( str_id2.begin(),
                      str_id2.end(),
                      str_id2.begin(),
                      std::towlower);
      }

    // This is the count of how many to skip before returning an answer:
    int after_count = 1;
    size_t search_position;
    if( last )
      {
      // We will search from right to left:
      search_position = str_id1.size();
      }
    else
      {
      // We will search from left to right:
      search_position = 0;
      }

    if( after )
      {
      int rdigits;
      after_count = static_cast<int>(get_binary_value_local(&rdigits,
                                                           after,
                                                           after->data+after_o,
                                                           after_s));
      after_count += 1;
      }
    while( after_count-- >= 1 )
      {
      if( !last )
        {
        // We are searching from left to right
        search_position = str_id1.find(str_id2, search_position);
        if( search_position == std::u32string::npos )
          {
          // Alas, our search was fruitless
          retval = 0;
          break;
          }
        search_position += 1;
        if( after_count == 0 )
          {
          // This was the find we were looking for!
          // COBOL positions are 1-based positions, not zero-based offsets:
          retval = search_position;
          break;
          }
        }
      else
        {
        // We are searching from right_to)left
        search_position = str_id1.rfind(str_id2, search_position);
        if( search_position == std::u32string::npos )
          {
          // Alas, our search was fruitless
          break;
          }
        if( after_count == 0 )
          {
          // This was the find we were looking for!
          // COBOL positions are 1-based positions, not zero-based offsets:
          retval = search_position + 1;
          break;
          }
        if( search_position == 0)
          {
          // There's no point in continuing the search leftwardsspast the
          // left edge, and if we subtract 1 from the size_t search_position,
          // we are not going to be happy with the result.
          break;
          }
        search_position -= 1;
        }
      }
    }
  // Set the return value:
  __gg__int128_to_field(dest,
                        retval,
                        NO_RDIGITS,
                        truncation_e,
                        NULL);
  }

static
char *
convert_for_convert(      cbl_encoding_t dest_enc,
                    const cblc_field_t *input,
                          size_t        input_o,
                          size_t        input_s,
                          size_t       *nbytes)
  {
  // iconverter takes care of untranslateable characters.
  char *retval = __gg__miconverter(input->encoding,
                                   dest_enc,
                                   input->data + input_o,
                                   input_s,
                                   nbytes);
  return retval;
  }

extern "C"
void
__gg__convert(cblc_field_t *dest,
        const cblc_field_t *input,
              size_t        input_o,
              size_t        input_s,
              int           /*source_format*/,
              int           dest_format)
  {
  /* convert formulations: 
   *  1. ANY to ALNUM HEX, or NAT HEX
   *  2. HEX to BYTE
   *  3. ALNUM to NAT, ALNUM HEX, or NAT HEX
   *  4. NAT to ALNUM, ALNUM HEX, or NAT HEX
   */

  /* enum convert_type_t
   *convert_alpha_e      = 0x01,
   *convert_nat_e        = 0x02,
   *convert_any_e        = 0x03, // i.e., both
   *convert_byte_e       = 0x04,
   *convert_hex_e        = 0x08, // may be combined with alpha or national
   *convert_just_bit_e   = 0x10, 
   *convert_just_e       = 0x18, // combined with HEX
   *convert_rjust_bit_e  = 0x20, 
   *convert_rjust_e      = 0x38, // combined with JUSTIFY
   */
  cbl_encoding_t tgt_enc = (dest_format & convert_nat_e) 
                         ? __gg__national_encoding
                         : __gg__display_encoding;
  const charmap_t *charmap_tgt = __gg__get_charmap(tgt_enc);

  charmap_t *charmap_dest = __gg__get_charmap(dest->encoding);
  
  if( dest_format & convert_hex_e )
    {
    size_t nbytes;
    char *converted = convert_for_convert(tgt_enc,
                                          input,
                                          input_o,
                                          input_s,
                                          &nbytes);
    // We output 'converted' as a stream of hexadecimal characters in the
    // destination encoding:
    size_t i = 0;
    size_t d = 0;
    while(i < nbytes && d < dest->capacity )
      {
      cbl_char_t byte = charmap_tgt->getch(converted, &i);
      unsigned char hi = byte>>4;
      hi += hi < 10 ? ascii_0 : ascii_A-10;
      char lo = byte & 0x0F;
      lo += lo < 10 ? ascii_0 : ascii_A-10;
      charmap_dest->putch(charmap_dest->mapped_character(hi), dest->data, &d);
      charmap_dest->putch(charmap_dest->mapped_character(lo), dest->data, &d);
      }
    free(converted);
    __gg__adjust_dest_size(dest, d);
    }
  else if( dest_format == convert_byte_e )
    {
    // The input is a series of hexadecimal characters
    size_t nbytes;
    char *converted = __gg__miconverter( input->encoding,
                                         iconv_CP1252_e,
                                         input->data+input_o,
                                         input_s,
                                         &nbytes);
    size_t i = 0;
    size_t d = 0;
    while(i < nbytes && d < dest->capacity )
      {
      // Each character is part of a string of hexadecimal digits.  So, the
      // idea is that A1 should be turned into 1010.0001.  There is no
      // guarantee that these characters actually are hexadecimal.
      cbl_char_t nybble = charmap_tgt->getch(converted, &i);
      if( nybble >= ascii_0 && nybble <= ascii_9 )
        {
        nybble -= ascii_0;
        }
      else if( nybble >= ascii_a && nybble <= ascii_f )
        {
        nybble -= ascii_a - 10;
        }
      else if( nybble >= ascii_A && nybble <= ascii_F )
        {
        nybble -= ascii_A - 10;
        }
      else
        {
        nybble = 0;
        }

      for(int j=0; j<4; j++)
        {
        if( nybble & 0x08 )
          {
          charmap_dest->putch(ascii_1, dest->data, &d);
          }
        else
          {
          charmap_dest->putch(ascii_0, dest->data, &d);
          }
        nybble <<= 1;
        }
      }
    free(converted);
    __gg__adjust_dest_size(dest, d);
    }
  else
    {
    size_t nbytes;
    char *converted = convert_for_convert(dest->encoding,
                                          input,
                                          input_o,
                                          input_s,
                                          &nbytes);
    size_t len = std::min(nbytes, dest->capacity);
    memcpy(dest->data, converted, len);
    free(converted);
    __gg__adjust_dest_size(dest, len);
    }
  }
