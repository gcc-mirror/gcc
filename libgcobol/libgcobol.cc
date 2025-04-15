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
#include <ctype.h>
#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <fenv.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <vector>
#include <algorithm>
#include <unordered_map>
#include <set>
#include <string>
#include <setjmp.h>
#include <signal.h>
#include <dlfcn.h>
#include <dirent.h>
#include <sys/resource.h>

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
#include <sys/stat.h>
#include <sys/types.h>

#include <execinfo.h>

#include "exceptl.h"

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
int         __gg__exception_handled           = 0    ;
int         __gg__exception_file_number       = 0    ;
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
int         __gg__odo_violation               = 0    ;
int         __gg__nop                         = 0    ;
int         __gg__main_called                 = 0    ;

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

// These values are the persistent stashed versions of the global values
static int         stashed_exception_code;
static int         stashed_exception_handled;
static int         stashed_exception_file_number;
static int         stashed_exception_file_status;
static const char *stashed_exception_file_name;
static const char *stashed_exception_program_id;
static const char *stashed_exception_section;
static const char *stashed_exception_paragraph;
static const char *stashed_exception_source_file;
static int         stashed_exception_line_number;
static const char *stashed_exception_statement;

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

static ec_status_t ec_status;

static const ec_descr_t *
local_ec_type_descr( ec_type_t type ) {
  auto p = std::find( __gg__exception_table, __gg__exception_table_end, type );
  if( p == __gg__exception_table_end )
    {
    __gg__abort("Fell off the end of the __gg__exception_table");
    }
  return p;
}

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

ec_status_t& ec_status_t::update() {
  handled =   ec_type_t(__gg__exception_handled);
  type    =   ec_type_t(__gg__exception_code);
  __gg__exception_code = ec_none_e;
  source_file = __gg__exception_source_file;
  lineno = __gg__exception_line_number;
  if( __gg__exception_statement ) {
    snprintf(statement, sizeof(statement), "%s", __gg__exception_statement);
  }

  return *this;
}

static cbl_truncation_mode truncation_mode = trunc_std_e;

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
  char *rt_currency_signs[256];
  const unsigned short *rt_collation;  // Points to a table of 256 values;
  char *rt_program_name;

  program_state()
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

    // Set all the currency_sign pointers to NULL:

    memset(rt_currency_signs, 0, sizeof(rt_currency_signs));

    // The default collating sequence:
    if( internal_is_ebcdic )
      {
      rt_collation = __gg__cp1140_to_cp1252_values;
      }
    else
      {
      rt_collation = __gg__one_to_one_values;
      }
    rt_program_name = NULL;
    }

  program_state(const program_state &ps)
    {
    rt_decimal_point        = ps.rt_decimal_point         ;
    rt_decimal_separator    = ps.rt_decimal_separator     ;
    rt_quote_character      = ps.rt_quote_character       ;
    rt_low_value_character  = ps.rt_low_value_character   ;
    // Note throughout the code that there is special processing for the
    // high-value character.  In EBCDIC 0xFF doesn't map to ASCII 0xFF, so
    // we are forced to avoid converting EBCDIC 0xFF.
    rt_high_value_character = ps.rt_high_value_character  ;
    rt_collation            = ps.rt_collation  ;

    for( int i=0; i<256; i++ )
      {
      if( ps.rt_currency_signs[i] )
        {
        rt_currency_signs[i] = strdup(ps.rt_currency_signs[i]);
        }
      else
        {
        rt_currency_signs[i] = NULL;
        }
      }

    rt_program_name                  = ps.rt_program_name                  ;
    }

  ~program_state()
    {
    for(int symbol=0; symbol<256; symbol++)
      {
      if( rt_currency_signs[symbol] )
        {
        free(rt_currency_signs[symbol]);
        rt_currency_signs[symbol] = NULL;
        }
      }
    }
  };

static std::vector<program_state> program_states;
#define collated(a)          (program_states.back().rt_collation[(unsigned int)(a&0xFF)])
#define program_name         (program_states.back().rt_program_name)
// #define decimal_point        (program_states.back().rt_decimal_point)
// #define decimal_separator    (program_states.back().rt_decimal_separator)
// #define quote_character      (program_states.back().rt_quote_character)
// #define low_value_character  (program_states.back().rt_low_value_character)
// #define high_value_character (program_states.back().rt_high_value_character)
// #define currency_signs(a)    (program_states.back().rt_currency_signs[(a)])
#define currency_signs(a)    (__gg__currency_signs[(a)])

#ifdef DEBUG_MALLOC
void *malloc(size_t a)
  {
  void *retval = malloc(a);
  fprintf(stderr, " --malloc(%p)-- ", retval);
  return retval;
  return retval;
  }
#endif

void
__gg__abort(const char *msg)
  {
  fprintf(stderr, "%s: %s\n", program_name, msg);
  abort();
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
char *
__gg__get_default_currency_string()
  {
  return currency_signs(__gg__default_currency_sign);
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
    *block = (int *)realloc(*block, new_size * sizeof(int));
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
        __gg__treeplet_1f = (cblc_field_t **)realloc(__gg__treeplet_1f, new_size * sizeof(cblc_field_t *));
        __gg__treeplet_1o = (size_t *)realloc(__gg__treeplet_1o, new_size * sizeof(size_t));
        __gg__treeplet_1s = (size_t *)realloc(__gg__treeplet_1s, new_size * sizeof(size_t));
        }
    break;
    case 2:
      if( new_size > treeplet_2_size )
        {
        treeplet_2_size = new_size;
        __gg__treeplet_2f = (cblc_field_t **)realloc(__gg__treeplet_2f, new_size * sizeof(cblc_field_t *));
        __gg__treeplet_2o = (size_t *)realloc(__gg__treeplet_2o, new_size * sizeof(size_t));
        __gg__treeplet_2s = (size_t *)realloc(__gg__treeplet_2s, new_size * sizeof(size_t));
        }
    break;
    case 3:
      if( new_size > treeplet_3_size )
        {
        treeplet_3_size = new_size;
        __gg__treeplet_3f = (cblc_field_t **)realloc(__gg__treeplet_3f, new_size * sizeof(cblc_field_t *));
        __gg__treeplet_3o = (size_t *)realloc(__gg__treeplet_3o, new_size * sizeof(size_t));
        __gg__treeplet_3s = (size_t *)realloc(__gg__treeplet_3s, new_size * sizeof(size_t));
        }
    break;
    case 4:
      if( new_size > treeplet_4_size )
        {
        treeplet_4_size = new_size;
        __gg__treeplet_4f = (cblc_field_t **)realloc(__gg__treeplet_4f, new_size * sizeof(cblc_field_t *));
        __gg__treeplet_4o = (size_t *)realloc(__gg__treeplet_4o, new_size * sizeof(size_t));
        __gg__treeplet_4s = (size_t *)realloc(__gg__treeplet_4s, new_size * sizeof(size_t));
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

// #define decimal_point        (program_states.back().rt_decimal_point)
// #define decimal_separator    (program_states.back().rt_decimal_separator)
// #define quote_character      (program_states.back().rt_quote_character)
// #define low_value_character  (program_states.back().rt_low_value_character)
// #define high_value_character (program_states.back().rt_high_value_character)

  __gg__decimal_point        = program_states.back().rt_decimal_point        ;
  __gg__decimal_separator    = program_states.back().rt_decimal_separator    ;
  __gg__quote_character      = program_states.back().rt_quote_character      ;
  __gg__low_value_character  = program_states.back().rt_low_value_character  ;
  __gg__high_value_character = program_states.back().rt_high_value_character ;
  __gg__currency_signs       = program_states.back().rt_currency_signs       ;

}

static
int
cstrncmp(   char const * const left_,
            char const * const right_,
            size_t count)
  {
  const char *left  = left_;
  const char *right = right_;
  // This is the version of strncmp() that uses the current collation

  // It also is designed to handle strings with embedded NUL characters, so
  // it treats NULs like any other characters.
  int retval = 0;
  while( count-- )
    {
    unsigned char chl = *left++;
    unsigned char chr = *right++;
    retval = chl - chr;
    if( retval )
      {
      break;
      }
    }
  return retval;
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

extern "C"
void
__gg__init_program_state()
  {
  // This routine gets called at DATA DIVISION time.

  // We need to make sure that the program_states vector has at least one
  // entry in it.  This happens when we are the very first PROGRAM-ID called
  // in this module.
  if( program_states.empty() )
    {
    initialize_program_state();
    }
  }

static int
var_is_refmod( cblc_field_t *var )
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
    fprintf(stderr, "The problem is in %s.\n", __func__);
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

extern "C"
bool
__gg__binary_to_string(char *result, int digits, __int128 value)
  {
  // The result is not terminated, because this routine is used
  // to put information directly into cblc_field_t::data
  // Our caller has to keep track of whether value was negative.

  // Note that this routine operates in the source code-set space; that is
  // the result comes back with zero as an ASCII 0x30, not an EBCDIC 0xF0

  if( value < 0 )
    {
    value = -value;
    }
  result += digits-1 ;
  while( digits-- )
    {
    *result-- = value%10 + ascii_zero;
    value /= 10;
    }
  // Should value be non-zero, it means we potentially have a size error
  return value != 0;
  }

extern "C"
bool
__gg__binary_to_string_internal(char *result, int digits, __int128 value)
  {
  // The result is not terminated, because this routine is used
  // to put information directly into cblc_field_t::data
  // Our caller has to keep track of whether value was negative.

  // Note that this routine operates in the source code-set space; that is
  // the result comes back with zero as an ASCII 0x30, not an EBCDIC 0xF0

  if( value < 0 )
    {
    value = -value;
    }
  result += digits-1 ;
  while( digits-- )
    {
    *result-- = (value%10) + internal_zero;
    value /= 10;
    }
  // Should value be non-zero, it means we potentially have a size error
  return value != 0;
  }

static bool
value_is_too_big(   cblc_field_t *var,
                    __int128 value,
                    int source_rdigits)
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

static void
turn_sign_bit_on(unsigned char *location)
  {
  if( internal_is_ebcdic )
    {
    *location &= ~NUMERIC_DISPLAY_SIGN_BIT;
    }
  else
    {
    *location |=  NUMERIC_DISPLAY_SIGN_BIT;
    }
  }

static void
turn_sign_bit_off(unsigned char *location)
  {
  if( internal_is_ebcdic )
    {
    *location |=  NUMERIC_DISPLAY_SIGN_BIT;
    }
  else
    {
    *location &= ~NUMERIC_DISPLAY_SIGN_BIT;
    }
  }

static bool
is_sign_bit_on(char ch)
  {
  bool retval;
  if( (unsigned char)ch == 0xFF || ch == 0x00 )
    {
    // Don't let HIGH-VALUE or LOW_VALUE confuse sign detection
    retval = false;
    }
  else
    {
    if( internal_is_ebcdic )
      {
      retval = (ch & NUMERIC_DISPLAY_SIGN_BIT) == 0;
      }
    else
      {
      retval = (ch & NUMERIC_DISPLAY_SIGN_BIT) != 0;
      }
    }
  return retval;
  }

extern "C"
void
__gg__string_to_alpha_edited_ascii( char *dest,
                                    char *source,
                                    int slength,
                                    char *picture)
  {
  char *dupe = (char *)malloc(slength);
  memcpy(dupe, source, slength);
  ascii_to_internal_str(dupe, slength);
  __gg__string_to_alpha_edited(dest, dupe, slength, picture);
  free(dupe);
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
          *(float *)location = tvalue;
          break;
          }

        case 8:
          {
          double tvalue = (double)value;
          tvalue /= (double)__gg__power_of_ten(source_rdigits);
          *(double *)location = tvalue;
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
      bool size_error = false;

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

        int is_negative = value < 0 ;

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
            // This is sort of a Hail Mary play.  We aren't supposed to do this
            // conversion if rdigits is non-zero.  But we shouldn't have gotten
            // here if rdigits is non-zero.  So, we'll just go with the flow.

            // Note that sending a signed value to an alphanumeric strips off
            // any plus or minus signs.
            size_error = __gg__binary_to_string_internal( (char *)location,
                         length, value);
            break;

          case FldNumericDisplay:
            if( var->attr & signable_e )
              {
              // Things get exciting when a numeric-display value is signable

              if( var->attr & separate_e )
                {
                // Whether positive or negative, a sign there will be:
                char sign_ch = is_negative ? internal_minus : internal_plus ;
                if( var->attr & leading_e )
                  {
                  // The sign character goes into the first location
                  size_error =
                    __gg__binary_to_string_internal((char *)(location+1),
                                                    length-1, value);
                  location[0] = sign_ch;
                  }
                else
                  {
                  // The sign character goes into the last location
                  size_error =
                    __gg__binary_to_string_internal(  (char *)location,
                                                      length-1, value);
                  location[length-1] = sign_ch;
                  }
                }
              else
                {
                // The sign information is not separate, so we put it into
                // the number
                size_error =
                  __gg__binary_to_string_internal(( char *)location,
                                                  length, value);

                if( size_error && is_negative )
                  {
                  // If all of the digits are zero, then the result is zero, and
                  // we have to kill the is_negative flag:
                  is_negative = false;
                  for(size_t i=0; i<length; i++)
                    {
                    if( location[i] != internal_zero )
                      {
                      is_negative = true;
                      break;
                      }
                    }
                  }

                if( is_negative )
                  {
                  if( var->attr & leading_e )
                    {
                    // The sign bit goes into the first digit:
                    turn_sign_bit_on(&location[0]);
                    }
                  else
                    {
                    // The sign bit goes into the last digit:
                    turn_sign_bit_on(&location[length-1]);
                    }
                  }
                }
              }
            else
              {
              // It's a simple positive number
              size_error = __gg__binary_to_string_internal( (char *)location,
                           length, value);
              }

            break;

          case FldNumericEdited:
            {
            if( value == 0 && (var->attr & blank_zero_e) )
              {
              memset(location, internal_space, length);
              }
            else
              {
              char ach[512];

              // At this point, value is scaled to the target's rdigits

              size_error = __gg__binary_to_string(ach, var->digits, value);
              ach[var->digits] = NULLCH;

              // Convert that string according to the PICTURE clause
              size_error |= __gg__string_to_numeric_edited(
                                (char *)location,
                                ach,
                                target_rdigits,
                                is_negative,
                                var->picture);
              ascii_to_internal_str((char *)location, var->capacity);
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
            // must get processed separately elsewhere.  As the author, it would
            // be nice if I knew -- but I don't.
            binary_to_little_endian(location,
                                    length,
                                    value);
            size_error = value_is_too_big(var, value, source_rdigits);
            break;

          case FldAlphaEdited:
            {
            char ach[128];
            size_error = __gg__binary_to_string(ach, length, value);
            ach[length] = NULLCH;

            // Convert that string according to the PICTURE clause
            __gg__string_to_alpha_edited(
              (char *)location,
              ach,
              strlen(ach),
              var->picture);
            break;
            }

          case FldPacked:
            {
            static const unsigned char bin2pd[100] =
              {
              0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
              0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
              0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29,
              0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
              0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
              0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
              0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
              0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79,
              0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
              0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99,
              } ;

            // Convert the binary value to packed decimal.

            // Set the destination bytes to zero
            memset(location, 0, length);
            unsigned char sign_nybble = 0;
            if( !(var->attr & packed_no_sign_e) )
              {
              // This is COMP-3 packed decimal, so we need to make room to the
              // right of the final decimal digit for the sign nybble:
              value *= 10;
              // Figure out what the sign nybble is going to be, and make the
              // the value positive:
              if(var->attr & signable_e)
                {
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
                sign_nybble = 0x0F;
                if(value < 0)
                  {
                  value = -value;
                  }
                }
              }
            // ploc points to the current rightmost byte of the location:
            unsigned char *ploc = location + length -1 ;

            // Build the target from right to left, so that the result is
            // big-endian:
            while( value && ploc >= location )
              {
              *ploc-- = bin2pd[value%100];
              value /= 100;
              }

            // We can put the sign nybble into place at this point.  Note that
            // for COMP-6 numbers the sign_nybble value is zero, so the next
            // operation is harmless.
            location[length -1] |= sign_nybble;

            // If we still have value left, we have a size error
            if( value )
              {
              size_error = true;
              }
            else
              {
              if(    (  sign_nybble && !(var->digits&1) )
                  || ( !sign_nybble &&  (var->digits&1) ) )
                {
                // This is either
                // comp-3 with an even number of digits, or
                // comp-6 with an odd  number of digits.
                // Either way, the first byte of the target has to have a high
                // nybble of zero.  If it's non-zero, then we have a size error:
                if( location[0] & 0xF0 )
                  {
                  size_error = true;
                  }
                }
              }
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

static __int128
edited_to_binary( const char *ps_,
                  int length,
                  int *rdigits)
  {
  const unsigned char *ps = (const unsigned char *)ps_;
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

  unsigned char ch;

  // We need to check the last two characters.  If CR or DB, then the result
  // is negative:
  if( length >= 2)
    {
    if(((ps[length-2]&0xFF) == internal_D || (ps[length-2]&0xFF) == internal_d )
     &&((ps[length-1]&0xFF) == internal_B || (ps[length-1]&0xFF) == internal_b))
      {
      hyphen = 1;
      }
    else if(    ((ps[length-2]&0xFF) == internal_C
              || (ps[length-2]&0xFF) == internal_c)
            &&  ((ps[length-1]&0xFF) == internal_R
              || (ps[length-1]&0xFF) == internal_r) )
      {
      hyphen = 1;
      }
    }

  while( index < length )
    {
    ch = ps[index++] & 0xFF;
    if( ch == ascii_to_internal(__gg__decimal_point) )
      {
      delta_r = 1;
      continue;
      }
    if( ch == internal_minus )
      {
      hyphen = 1;
      continue;
      }

    if( internal_0 <= ch && ch <= internal_9 )
      {
      result *= 10;
      // In both EBCDIC and ASCII, this works:
      result += ch & 0x0F ;
      *rdigits += delta_r ;
      continue;
      }
    }

  if( result == 0 )
    {
    hyphen = 0;
    }
  else if( hyphen )
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
  unsigned char *dest = (unsigned char *)&retval;
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
  unsigned char *dest = (unsigned char *)&retval;
  while(capacity > 0)
    {
    *dest++ = psource[--capacity];
    }
  return retval;
  }

static
__int128
get_binary_value_local(  int           *rdigits,
                         cblc_field_t  *resolved_var,
                         unsigned char *resolved_location,
                         size_t         resolved_length)
  {
  __int128 retval = 0;

  unsigned char ch;
  switch( resolved_var->type )
    {
#if 1
    case FldLiteralA :
      fprintf(stderr, "%s(): is trying to handle a FldLiteralA\n", __func__);
      abort();
      // // Read the data area as a dirty string:
      // retval = __gg__dirty_to_binary_internal( (const char *)resolved_location,
               // resolved_length,
               // rdigits );
      break;
#endif

    case FldGroup :
    case FldAlphanumeric :
      // Read the data area as a dirty string:
      retval = __gg__dirty_to_binary_internal( (const char *)resolved_location,
               resolved_length,
               rdigits );
      break;

    case FldNumericDisplay :
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

        // Make it positive
        ((unsigned char *)&retval)[sizeof(retval)-1] = 0x3F;
        *rdigits = resolved_var->rdigits;
        }
      else
        {
        // Pick up the sign byte, and force our value to be positive
        unsigned char *sign_byte_location;
        if(   (resolved_var->attr  & separate_e )
           && (resolved_var->attr  & leading_e  ) )
          {
          sign_byte_location = resolved_location;
          ch = *sign_byte_location;
          *sign_byte_location = internal_plus;
          }
        else if(    (resolved_var->attr & separate_e)
                && !(resolved_var->attr & leading_e ) )
          {
          sign_byte_location = resolved_location + resolved_length - 1;
          ch = *sign_byte_location;
          *sign_byte_location = internal_plus;
          }
        else if( (resolved_var->attr & leading_e) )
          {
          sign_byte_location = resolved_location;
          ch = *sign_byte_location;
          turn_sign_bit_off(sign_byte_location);
          }
        else // if( !(resolved_var->attr & leading_e) )
          {
          sign_byte_location = resolved_location + resolved_length - 1;
          ch = *sign_byte_location;
          turn_sign_bit_off(sign_byte_location);
          }

        // We know where the decimal point is because of rdigits.  Because
        // we know that it a clean string of ASCII digits, we can use the
        // dirty converter:
        retval = __gg__dirty_to_binary_internal((const char *)resolved_location,
                 resolved_length,
                 rdigits );
        *rdigits = resolved_var->rdigits;

        // Restore the sign byte
        *sign_byte_location = ch;

        if( ch == internal_minus || is_sign_bit_on(ch) )
          {
          retval = -retval;
          }
        }
      break;

    case FldNumericEdited :
      retval = edited_to_binary(  (const char *)resolved_location,
                                  resolved_length,
                                  rdigits);
      break;

    case FldNumericBinary :
      if( resolved_var->attr & signable_e)
        {
        retval = big_endian_to_binary_signed(
                        (const unsigned char *)resolved_location,
                        resolved_length);
        }
      else
        {
        retval = big_endian_to_binary_unsigned(
                        (const unsigned char *)resolved_location,
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
                      (const unsigned char *)resolved_location,
                      resolved_length);
        }
      else
        {
        retval = little_endian_to_binary_unsigned(
                      (const unsigned char *)resolved_location,
                      resolved_length);
        }
      *rdigits = resolved_var->rdigits;
      break;

    case FldPacked:
      {
      static const unsigned char dp2bin[160] =
        {
        // This may not be the weirdest table I've ever created, but it is
        // certainly a contender.  Given the packed decimal byte 0x23, it
        // returns the equivalent decimal value of 23.
        00, 01, 02, 03, 04, 05, 06, 07,  8,  9, 0, 0, 0, 0, 0, 0, // 0x00
        10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 0, 0, 0, 0, 0, 0, // 0x10
        20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 0, 0, 0, 0, 0, 0, // 0x20
        30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 0, 0, 0, 0, 0, 0, // 0x30
        40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 0, 0, 0, 0, 0, 0, // 0x40
        50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 0, 0, 0, 0, 0, 0, // 0x50
        60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 0, 0, 0, 0, 0, 0, // 0x60
        70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 0, 0, 0, 0, 0, 0, // 0x70
        80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 0, 0, 0, 0, 0, 0, // 0x80
        90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 0, 0, 0, 0, 0, 0, // 0x90
        };

      if( resolved_var->attr & packed_no_sign_e )
        {
        // This is packed decimal without a sign nybble
        retval = 0;
        for(size_t i=0; i<resolved_var->capacity; i++)
          {
          retval *= 100;
          retval += dp2bin[resolved_location[i]];
          }
        }
      else
        {
        // This is packed decimal with a final sign nybble
        retval = 0;
        size_t imputed_length = (resolved_var->digits + 2)/2;
        for(size_t i=0; i<imputed_length-1; i++)
          {
          retval *= 100;
          retval += dp2bin[resolved_location[i]];
          }
        retval *= 10;
        retval += resolved_location[imputed_length-1]>>4;
        if(    (resolved_location[imputed_length-1]&0x0F) == 0x0D
            || (resolved_location[imputed_length-1]&0x0F) == 0x0B )
          {
          retval = -retval;
          }
        }
     *rdigits = resolved_var->rdigits;
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

#pragma GCC diagnostic ignored "-Wformat-overflow"

static time_t
cobol_time()
  {
  struct timespec tp;
  __gg__clock_gettime(CLOCK_REALTIME, &tp);
  return tp.tv_sec;
  }

extern "C"
char *
__gg__get_date_yymmdd()
  {
  char ach[32];

  time_t t = cobol_time();
  struct tm *local = localtime(&t);

  sprintf(ach,
          "%2.2d%2.2d%2.2d",
          local->tm_year  % 100,
          local->tm_mon+1 % 100,
          local->tm_mday  % 100 );

  ascii_to_internal_str(ach, strlen(ach));
  return strdup(ach);
  }

extern "C"
char *
__gg__get_date_yyyymmdd()
  {
  char ach[32];

  time_t t = cobol_time();
  struct tm *local = localtime(&t);

  sprintf(ach,
          "%4.4d%2.2d%2.2d",
          local->tm_year + 1900,
          local->tm_mon+1,
          local->tm_mday);

  ascii_to_internal_str(ach, strlen(ach));
  return strdup(ach);
  }

extern "C"
char *
__gg__get_date_yyddd()
  {
  char ach[32];

  time_t t = cobol_time();
  struct tm *local = localtime(&t);

  sprintf(ach,
          "%2.2d%3.3d",
          local->tm_year % 100,
          local->tm_yday+1);

  ascii_to_internal_str(ach, strlen(ach));
  return strdup(ach);
  }

extern "C"
char *
__gg__get_yyyyddd()
  {
  char ach[32];

  time_t t = cobol_time();
  struct tm *local = localtime(&t);

  sprintf(ach,
          "%4.4d%3.3d",
          local->tm_year + 1900,
          local->tm_yday+1);

  ascii_to_internal_str(ach, strlen(ach));
  return strdup(ach);
  }

extern "C"
char *
__gg__get_date_dow()
  {
  char ach[32];

  time_t t = cobol_time();
  struct tm *local = localtime(&t);

  sprintf(ach,
          "%1.1d",
          local->tm_wday == 0 ? 7 : local->tm_wday);

  ascii_to_internal_str(ach, strlen(ach));
  return strdup(ach);
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


extern "C"
void
__gg__clock_gettime(clockid_t clk_id, struct timespec *tp)
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
    clock_gettime(clk_id, tp);
    }
  }

extern "C"
char *
__gg__get_date_hhmmssff()
  {
  char ach[32];

  struct timespec tv;
  __gg__clock_gettime(CLOCK_REALTIME, &tv);

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

  ascii_to_internal_str(ach, strlen(ach));
  return strdup(ach);
  }

extern "C"
int
__gg__setop_compare(
  const char *candidate,
  int capacity,
  const char *domain)
  {
  // This routine is called to compare the characters of 'candidate'
  // against the list of character pairs in 'domain'

  int retval = 0;
  int ch;
  int l;
  int h;
  const char *d;

  for(int i=0; i<capacity; i++)
    {
    ch = (*candidate++ & 0xFF);
    d = domain;
    while(*d)
      {
      retval = 0;
      // We are decoding hexadecimal numbers, either in pairs,
      // or singletons:  "20/30 " or "20 ".  The final one is
      // terminated with '\0'

      // See the comments in genapi.cc::get_class_condition_string
      // to see how this string was encoded.

      l = (int)strtoll(d, (char **)&d, 16);
      if( l < 0 )
        {
        l = -l;
        }
      h = l;
      if( *d == '/' )
        {
        d += 1;
        h = (int)strtoll(d, (char **)&d, 16);
        if( h < 0 )
          {
          h = -h;
          }
        }
      else if( *d == ' ' )
        {
        d += 1;
        }
      if( ch >= l && ch <= h )
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
__gg__dirty_to_binary_internal( const char *dirty,
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

  // We are limiting the number of digits in the number to MAX_FIXED_POINT_DIGITS

  __int128 retval = 0;

  int digit_count = 0;
  int hyphen = 0;
  *rdigits = 0;

  // Create a delta_r for counting digits to the right of
  // any decimal point.  If and when we encounter a decimal separator,
  // we'll set this to one, otherwise it'll stay zero.
  int delta_r = 0;

  // We now loop over the remaining input characters:
  unsigned char ch = '\0';

  if(length-- > 0)
    {
    ch = *dirty++;
    if( ch == internal_minus )
      {
      hyphen = 1;
      }
    else if( ch == internal_plus )
      {
      // A plus sign is okay
      }
    else if( ch == ascii_to_internal(__gg__decimal_point) )
      {
      delta_r = 1;
      }
    else if( ch >= internal_0 && ch <= internal_9 )
      {
      retval = ch - internal_0 ;
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

  while( length-- > 0 )
    {
    ch = *dirty++;
    if( ch == ascii_to_internal(__gg__decimal_point) && delta_r == 0 )
      {
      // This is the first decimal point we've seen, so we
      // can start counting rdigits:
      delta_r = 1;
      continue;
      }
    if( ch < internal_0 || ch > internal_9 )
      {
      // When we hit something that isn't a digit, then we are done
      break;
      }
    if( digit_count < MAX_FIXED_POINT_DIGITS )
      {
      retval *= 10;
      retval += ch - internal_0 ;
      *rdigits += delta_r;
      if( retval )
        {
        digit_count += 1;
        }
      }
    }

  // Let's check for an exponent:
  if( ch == internal_E || ch == internal_e )
    {
    int exponent = 0;
    int exponent_sign = 1;
    if( length > 0  )
      {
      ch = *dirty;
      if( ch == internal_plus)
        {
        length -= 1;
        dirty += 1;
        }
      else if (ch == internal_minus)
        {
        exponent_sign = -1;
        length -= 1;
        dirty += 1;
        }
      }
    while(length-- > 0)
      {
      ch = *dirty++;
      if( ch < internal_0 || ch > internal_9 )
        {
        // When we hit something that isn't a digit, then we are done
        break;
        }
      exponent *= 10;
      exponent += ch - internal_0 ;
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
                      int length)
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
  char ch = '\0';

  if(length-- > 0)
    {
    ch = *dirty++;
    if( ch == internal_minus )
      {
      hyphen = 1;
      }
    else if( ch == internal_plus )
      {
      // A plus sign is okay
      }
    else if( ch == ascii_to_internal(__gg__decimal_point) )
      {
      delta_r = 1;
      }
    else if( ch >= internal_0 && ch <= internal_9 )
      {
      retval = ch - internal_0 ;
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
    ch = *dirty++;
    if( ch == ascii_to_internal(__gg__decimal_point) && delta_r == 0 )
      {
      // This is the first decimal point we've seen, so we
      // can start counting rdigits:
      delta_r = 1;
      continue;
      }
    if( ch < internal_0 || ch > internal_9 )
      {
      // When we hit something that isn't a digit, then we are done
      break;
      }
    retval *= 10;
    retval += ch - internal_0 ;
    rdigits += delta_r;
    }

  // Let's check for an exponent:
  int exponent = 0;
  if( ch == internal_E || ch == internal_e )
    {
    int exponent_sign = 1;
    if( length > 0  )
      {
      ch = *dirty;
      if( ch == internal_plus)
        {
        length -= 1;
        dirty += 1;
        }
      else if (ch == internal_minus)
        {
        exponent_sign = -1;
        length -= 1;
        dirty += 1;
        }
      }
    while(length-- > 0)
      {
      ch = *dirty++;
      if( ch < internal_0 || ch > internal_9 )
        {
        // When we hit something that isn't a digit, then we are done
        break;
        }
      exponent *= 10;
      exponent += ch - internal_0 ;
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

static
void psz_to_internal(char *psz)
  {
  char *p = psz;
  while( *p )
    {
    *p = ascii_to_internal(*p);
    p += 1;
    }
  }

static int
get_scaled_rdigits(cblc_field_t *field)
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

static char *
format_for_display_internal(char **dest,
                            size_t *dest_size,
                            cblc_field_t *var,
                            unsigned char *actual_location,
                            int   actual_length,
                            int   address_of)
  {
  // dest and dest_size represent a malloced buffer of dest_size.

  // This routine will put the formatted result into dest if it fits, and
  // realloc dest if it doesn't.  The new_size goes into the dest_size
  // reference.  Used properly, the caller's buffer just keeps getting bigger
  // as necessary, cutting down on the number of reallocations needed.

  int source_rdigits = var->rdigits;

  if( var->attr & scaled_e )
    {
    source_rdigits = 0;
    }

  // Special case, when var->address_of is on

  if( address_of )
    {
    // Assume that DISPLAY OF ADDRESS OF should be what's expected:

    __gg__realloc_if_necessary(dest, dest_size, 2*sizeof(void *) + 1);

    sprintf(  *dest,
              "0x%*.*lx",
              (int)(2*sizeof(void *)),
              (int)(2*sizeof(void *)),
              (unsigned long)actual_location);
    ascii_to_internal_str(*dest, strlen(*dest));

    goto done;
    }

  switch( var->type )
    {
    case FldLiteralA:
    case FldGroup:
    case FldAlphanumeric:
    case FldNumericEdited:
    case FldAlphaEdited:
      __gg__realloc_if_necessary(dest, dest_size, actual_length+1);
      if( actual_location )
        {
        memcpy(*dest, actual_location, actual_length);
        }
      else
        {
        fprintf(stderr, "attempting to display a NULL pointer in %s\n", var->name);
        abort();
        //memset(*dest, internal_query, actual_length);
        //memcpy(*dest, actual_location, actual_length);
        }
      (*dest)[actual_length] = NULLCH;
      break;

    case FldNumericDisplay:
      {
      // We are going to make use of fact that a NumericDisplay's data is
      // almost already in the format we need.  We have to add a decimal point,
      // if necessary, in the right place, and we need to tack on leading or
      // trailing zeroes for PPP999 and 999PPP scaled-e variables.

      if( var_is_refmod(var) )
        {
        __gg__realloc_if_necessary(dest, dest_size, actual_length+1);
        memcpy((*dest), actual_location, actual_length);
        (*dest)[actual_length] = NULLCH;
        break;
        }

      unsigned char *running_location = actual_location;

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

      int index = 0;  // This is the running index into our output destination
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
        if( var->attr & signable_e )
          {
          if( var->attr & separate_e )
            {
            // We are dealing with a sign character maintained separately in
            // the data.
            if( var->attr & leading_e )
              {
              // The first character is the sign character
              (*dest)[index++] = *running_location++;
              }
            }
          else
            {
            // The sign character is not separate.  It's in either the first
            // or last byte of the data:
            size_t sign_location = var->attr & leading_e ? 0 : actual_length-1 ;
            if( is_sign_bit_on( actual_location[sign_location]) )
              {
              (*dest)[index++] = internal_minus;
              }
            else
              {
              (*dest)[index++] = internal_plus;
              }
            }
          }

          {//xxx
          // copy over the characters to the left of the decimal point:
          for(int i=0; i<ldigits; i++ )
            {
            char ch = *running_location++;

            // The default HIGH-VALUE of 0xFF runs afoul of the
            // NumericDisplay sign bit 0f 0x40 when running in
            // ASCII mode.  The following test handles that problem
            // when HIGH-VALUE is still 0xFF.  That HIGH-VALUE can
            // be changed by the SPECIAL-NAMES ALPHABET clause. But

            // I have decided that the onus of that problem is on
            // the user.
            if( (*dest)[index-1] != (char)DEGENERATE_HIGH_VALUE )
              {
              turn_sign_bit_off((unsigned char *)&ch);
              }
            (*dest)[index++] = ch;
            }
          if( rdigits )
            {
            // Lay down a decimal point
            (*dest)[index++] = ascii_to_internal(__gg__decimal_point);

            if( ldigits < 0 )
              {
              // This is a scaled_e value, and we need that many zeroes:
              for( int i=0; i<-ldigits; i++ )
                {
                (*dest)[index++] = internal_zero;
                }
              }

            // And the digits to the right
            for(int i=0; i<rdigits; i++ )
              {
              char ch = *running_location++;
              if( (*dest)[index-1] != (char)DEGENERATE_HIGH_VALUE )
                {
                turn_sign_bit_off((unsigned char *)&ch);
                }
              (*dest)[index++] = ch;
              }
            }
          }
        // At this point, for a 999PPP number, we need to tack on the zeroes
        if( var->rdigits < 0 )
          {
          for(int i=0; i < -(var->rdigits); i++)
            {
            (*dest)[index++] = internal_zero;
            }
          }

        if(      var->attr & signable_e
            &&   var->attr & separate_e
            && !(var->attr & leading_e) )
          {
          (*dest)[index++] = actual_location[actual_length-1];
          }

        (*dest)[index++] = NULLCH;
        }
      else
        {
        fprintf(stderr, "attempting to display a NULL pointer in %s\n", var->name);
        abort();
        // memset(*dest, internal_query, nsize-1);
        // (*dest)[nsize] = NULLCH;
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
            digits = 38;
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
      __gg__binary_to_string_internal(ach, digits, value);

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

      bool is_signed = value < 0;

      if( var->attr & signable_e )
        {
        if( is_signed )
          {
          (*dest)[index++] = internal_minus;
          }
        else
          {
          (*dest)[index++] = internal_plus;
          }
        }
      // copy over the characters to the left of the decimal point:
      memcpy((*dest)+index, ach, digits - source_rdigits);
      index += digits - source_rdigits;
      if( source_rdigits )
        {
        (*dest)[index++] = ascii_to_internal(__gg__decimal_point);
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
      sprintf(ach, "%lu", (size_t)value);
      __gg__realloc_if_necessary(dest, dest_size, strlen(ach)+1);
      strcpy(*dest, ach);
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
      ascii_to_internal_str(*dest, strlen(*dest));
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
          _Float32 floatval = *(_Float32 *)actual_location;
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
          psz_to_internal(ach);
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
          _Float64 floatval = *(_Float64 *)actual_location;
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
          psz_to_internal(ach);
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

          psz_to_internal(ach);
          strcpy(*dest, ach);
          break;
          }
        }
      break;
      }

    default:
      fprintf(stderr,
              "Unknown conversion %d in format_for_display_internal\n",
              var->type );
      abort();
      break;
    }

  if( var->attr & scaled_e && var->type != FldNumericDisplay )
    {
    static size_t buffer_size = MINIMUM_ALLOCATION_SIZE;
    static char * buffer = (char *)malloc(buffer_size);
    if( var->rdigits > 0)
      {
      // We have something like 123 or +123.  We need to insert a decimal
      // point and a rdigits zeroes to make it +.000000123

      size_t new_length = strlen(*dest) + var->rdigits + 1 + 1;
      __gg__realloc_if_necessary(&buffer, &buffer_size, new_length);


      memset(buffer, internal_0, new_length);
      char *p = buffer;
      char *s = *dest;
      if(    ((*dest)[0]&0xFF) < internal_0
          || ((*dest)[0]&0xFF) > internal_9 )
        {
        *p++ = (*dest)[0];
        s += 1;
        }
      *p++ = ascii_to_internal(__gg__decimal_point);
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

      memset(buffer, internal_0, new_length);
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
    // Because we don't know what we are dealing with, we created a 38-digit
    // number with a variable number of rdigits.  So, we usually have a boatload
    // of leading zeroes.  I find that display offensive, so let's fix it:
    unsigned char *p1 = (unsigned char *)(*dest);
    if( *p1 == internal_plus || *p1 == internal_minus )
      {
      p1 += 1;
      }
    unsigned char *p2 = p1;
    while( p2[0] == internal_zero && p2[1] != '\0' )
      {
      p2 += 1;
      }
    strcpy((char *)p1, (char *)p2);
    }

  done:
  return *dest;
  }

static char *
format_for_display_local( char **dest,
                          size_t *dest_size,
                          cblc_field_t *var,
                          size_t var_offset,
                          size_t var_size,
                          int    var_flags)
  {
  if(var)
    {
    // At this point, format the entire length.  It's up to our caller to
    // trim it further, because this routine is used by both receivers and
    // senders
    format_for_display_internal(dest,
                                dest_size,
                                var,
                                var->data + var_offset,
                                var_size,
                                var_flags & REFER_T_ADDRESS_OF);
    }
  else
    {
    **dest = '\0';
    }
  return *dest;
  }

static int
compare_88( const char    *list,
            const char    *list_e,
            bool           fig_const,
            cblc_field_t  * /*conditional*/,
            unsigned char *conditional_location,
            int            conditional_length)
  {
  int list_len = (int)(list_e-list);
  int test_len;
  char *test;
  if( fig_const )
    {
    // We are working with a figurative constant

    test = (char *)malloc(conditional_length);
    test_len = conditional_length;
    // This is where we handle the zero-length strings that
    // nonetheless can magically be expanded into figurative
    // constants:

    int ch = internal_space;
    // Check for the strings starting with 0xFF whose second character
    // indicates a figurative constant:
    if( list[0] == ascii_Z )
      {
      ch = internal_zero;
      }
    else if( list[0] == ascii_H )
      {
      if( __gg__high_value_character == DEGENERATE_HIGH_VALUE )
        {
        ch = __gg__high_value_character;
        }
      else
        {
        ch = ascii_to_internal(__gg__high_value_character);
        }
      }
    else if( list[0] == ascii_Q )
      {
      ch = ascii_to_internal(__gg__quote_character);
      }
    else if( list[0] == ascii_L )
      {
      ch = ascii_to_internal(__gg__low_value_character);
      }
    memset( test, ch, conditional_length );
    }
  else if( list_len < conditional_length )
    {
    // 'list' is too short; we have to right-fill with spaces:
    test = (char *)malloc(conditional_length);
    test_len = conditional_length;
    memset(test, internal_space, conditional_length);
    memcpy(test, list, list_len);
    }
  else
    {
    test = (char *)malloc(list_len);
    test_len = list_len;
    memcpy(test, list, list_len);
    }

  int cmpval;

  if( test[0] == NULLCH && conditional_location[0] == 0)
    {
    cmpval = 0;
    }
  else
    {
    cmpval = cstrncmp(test, (char *)conditional_location, conditional_length);
    if( cmpval == 0 && (int)strlen(test) != conditional_length )
      {
      // When strncmp returns 0, the actual smaller string is the
      // the shorter of the two:
      cmpval = test_len - conditional_length;
      }
    }

  free(test);

  if( cmpval < 0 )
    {
    cmpval = -1;
    }
  else if(cmpval > 0)
    {
    cmpval = +1;
    }
  return cmpval;
  }

static GCOB_FP128
get_float128( cblc_field_t *field,
              unsigned char *location )
  {
  GCOB_FP128 retval=0;
  if(field->type == FldFloat )
    {
    switch( field->capacity )
      {
      case 4:
        retval = *(_Float32 *)location;
        break;
      case 8:
        retval = *(_Float64 *)location;
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
    if( __gg__decimal_point == '.' )
      {
      retval = strtofp128(field->initial, NULL);
      }
    else
      {
      // We need to replace any commas with periods
      static size_t size = 128;
      static char *buffer = (char *)malloc(size);
      while( strlen(field->initial)+1 > size )
        {
        size *= 2;
        buffer = (char *)malloc(size);
        }
      strcpy(buffer, field->initial);
      char *p = strchr(buffer, ',');
      if(p)
        {
        *p = '.';
        }
      retval = strtofp128(buffer, NULL);
      }
    }
  else
    {
    fprintf(stderr, "What's all this then?\n");
    abort();
    }
  return retval;
  }

static
int
compare_field_class(cblc_field_t  *conditional,
                    unsigned char *conditional_location,
                    int            conditional_length,
                    cblc_field_t  *list)
  {
  int retval = 1; // Zero means equal
  __int128 value;
  int rdigits;

  // list->initial points to a superstring: a double-null terminated
  // string containing pairs of strings.  We are looking for equality.

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
        char   left_flag;
        size_t left_len;
        char * left;

        char   right_flag;
        size_t right_len;
        char * right;

        char *pend;
        left_len = strtoull(walker, &pend, 10);
        left_flag = *pend;
        left = pend+1;

        right = left + left_len;
        right_len = strtoull(right, &pend, 10);
        right_flag = *pend;
        right = pend+1;

        walker = right + right_len;

        int left_rdigits;
        int right_rdigits;

        __int128 left_value;
        if( left_flag == 'F' && left[0] == 'Z' )
          {
          left_value = 0;
          left_rdigits = 0;
          }
        else
          {
          left_value = __gg__dirty_to_binary_internal(
                                  left,
                                  left_len,
                                  &left_rdigits);
          }

        __int128 right_value;
        if( right_flag == 'F' && right[0] == 'Z' )
          {
          right_value = 0;
          right_rdigits = 0;
          }
        else
          {
          right_value = __gg__dirty_to_binary_internal(
                                   right,
                                   right_len,
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

        first = walker;
        first_len = strtoull(first, &pend, 10);
        fig1 = *pend == 'F';
        first = pend+1;
        first_e = first + first_len;

        last = first_e;

        last_len = strtoull(last, &pend, 10);
        fig2 = *pend == 'F';
        last = pend+1;
        last_e = last + last_len;

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
      GCOB_FP128 value = get_float128(conditional, conditional_location) ;
      char *walker = list->initial;
      while(*walker)
        {
        char   left_flag;
        size_t left_len;
        char * left;

        char   right_flag;
        size_t right_len;
        char * right;

        char *pend;
        left_len = strtoull(walker, &pend, 10);
        left_flag = *pend;
        left = pend+1;

        right = left + left_len;
        right_len = strtoull(right, &pend, 10);
        right_flag = *pend;
        right = pend+1;

        walker = right + right_len;

        GCOB_FP128 left_value;
        if( left_flag == 'F' && left[0] == 'Z' )
          {
          left_value = 0;
          }
        else
          {
          left_value = __gg__dirty_to_float(left,
                                            left_len);
          }

        GCOB_FP128 right_value;
        if( right_flag == 'F' && right[0] == 'Z' )
          {
          right_value = 0;
          }
        else
          {
          right_value = __gg__dirty_to_float( right,
                                              right_len);
          }

        if( left_value <= value && value <= right_value )
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

static
int
compare_strings(char   *left_string,
                size_t  left_length,
                bool    left_all,
                char   *right_string,
                size_t  right_length,
                bool    right_all)
  {
  int retval = 0;
  size_t i = 0;

  if( right_all && right_length > left_length )
    {
    // In the rubber-bandy ALL situation, and the ALL is longer than the
    // fixed side, we just compare the characters of the fixed side:
    right_length = left_length;
    }

  if( left_all && left_length > right_length )
    {
    left_length = right_length;
    }

  while( !retval && i<left_length && i<right_length )
    {
    retval = collated((unsigned char)left_string[i])
             - collated((unsigned char)right_string[i]);
    i += 1;
    }

  // We need to space-extend the shorter value.  That's because
  // "Bob" is equal to "Bob     "
  if( !right_all )
    {
    while( !retval && i<left_length )
      {
      retval = collated((unsigned char)left_string[i])
               - collated(internal_space);
      i += 1;
      }
    }
  else
    {
    // In an ALL situation where the ALL is shorter than the fixed side, we
    // wrap around the ALL characters
    while( !retval && i<left_length )
      {
      retval = collated((unsigned char)left_string[i])
               - collated((unsigned char)right_string[i%right_length]);
      i += 1;
      }
    }

  if( !left_all )
    {
    while( !retval && i<right_length )
      {
      retval = collated(internal_space)
               - collated((unsigned char)right_string[i]);
      i += 1;
      }
    }
  else
    {
    if( left_length > right_length )
      {
      left_length = right_length;
      }
    while( !retval && i<right_length )
      {
      retval = collated((unsigned char)left_string[i%left_length])
               - collated((unsigned char)right_string[i]);
      i += 1;
      }
    }
  return retval;
  }

extern "C"
int
__gg__compare_2(cblc_field_t *left_side,
                unsigned char   *left_location,
                size_t  left_length,
                int     left_attr,
                int     left_flags,
                cblc_field_t *right_side,
                unsigned char   *right_location,
                size_t  right_length,
                int     right_attr,
                int     right_flags,
                int     second_time_through)
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

  // Figure out if we have any figurative constants
  cbl_figconst_t left_figconst  = (cbl_figconst_t)(left_attr  & FIGCONST_MASK);
  cbl_figconst_t right_figconst = (cbl_figconst_t)(right_attr & FIGCONST_MASK);

  unsigned int fig_left  = 0;
  unsigned int fig_right = 0;

  switch(left_figconst)
    {
    case normal_value_e :
      fig_left = 0;
      break;
    case low_value_e    :
      fig_left = ascii_to_internal(__gg__low_value_character);
      break;
    case zero_value_e   :
      fig_left = internal_zero;
      break;
    case space_value_e  :
      fig_left = internal_space;
      break;
    case quote_value_e  :
      fig_left = ascii_to_internal(__gg__quote_character);
      break;
    case high_value_e   :
      if( __gg__high_value_character == DEGENERATE_HIGH_VALUE )
        {
        fig_left = __gg__high_value_character;
        }
      else
        {
        fig_left = ascii_to_internal(__gg__high_value_character);
        }
      break;
    case null_value_e:
      break;
    }
  switch(right_figconst)
    {
    case normal_value_e :
      fig_right = 0;
      break;
    case low_value_e    :
      fig_right = ascii_to_internal(__gg__low_value_character);
      break;
    case zero_value_e   :
      fig_right = internal_zero;
      break;
    case space_value_e  :
      fig_right = internal_space;
      break;
    case quote_value_e  :
      fig_right = ascii_to_internal(__gg__quote_character);
      break;
    case high_value_e   :
      if( __gg__high_value_character == DEGENERATE_HIGH_VALUE )
        {
        fig_right = __gg__high_value_character;
        }
      else
        {
        fig_right = ascii_to_internal(__gg__high_value_character);
        }
      break;
    case null_value_e:
      break;
    }

  // We have four high-level conditions to consider:
  int retval = 0;
  bool compare = false;

  if( left_figconst && right_figconst )
    {
    // We are comparing two figurative constants
    retval = collated(fig_left) - collated(fig_right);
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
        for(size_t i=0; i<left_length; i++)
          {
          retval = collated((unsigned int)left_location[i])
                   - collated(fig_right);
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

            if( left_side)

            value = get_binary_value_local( &rdigits,
                                            left_side,
                                            left_location,
                                            left_length);
            retval = 0;
            retval = value < 0 ? -1 : retval;
            retval = value > 0 ?  1 : retval;
            break;
            }

          case FldFloat:
            {
            GCOB_FP128 value = __gg__float128_from_location(left_side,
                                                           left_location);
            retval = 0;
            retval = value < 0 ? -1 : retval;
            retval = value > 0 ?  1 : retval;
            break;
            }

          default:
            // We are comparing a alphanumeric string to ZEROES
            retval = 0;
            for(size_t i=0; i<left_length; i++)
              {
              retval = collated((unsigned int)left_location[i])
                       - collated(fig_right);
              if( retval )
                {
                break;
                }
              }
            compare = true;
            break;
          }
        compare = true;
        goto fixup_retval;
        break;
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
      retval = compare_strings(   (char *)left_location,
                                  left_length,
                                  left_all,
                                  (char *)right_location,
                                  right_length,
                                  right_all );

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
          // In order to do the comparision, we need the value from the
          // literal to be the same flavor as the left side:
          // We need to replace any commas with periods
          static size_t size = 128;
          static char *buffer = (char *)malloc(size);
          while( strlen(right_side->initial)+1 > size )
            {
            size *= 2;
            buffer = (char *)malloc(size);
            }
          strcpy(buffer, right_side->initial);
          if( __gg__decimal_point == ',' )
            {
            // We need to replace any commas with periods
            char *p = strchr(buffer, ',');
            if(p)
              {
              *p = '.';
              }
            }

          // buffer[] now contains the string we want to convert

          switch(left_side->capacity)
            {
            case 4:
              {
              _Float32 left_value  = *(_Float32 *)left_location;
              _Float32 right_value = strtof(buffer, NULL);
              retval = 0;
              retval = left_value < right_value ? -1 : retval;
              retval = left_value > right_value ?  1 : retval;
              break;
              }
            case 8:
              {
              _Float64 left_value  = *(_Float64 *)left_location;
              _Float64 right_value = strtod(buffer, NULL);
              retval = 0;
              retval = left_value < right_value ? -1 : retval;
              retval = left_value > right_value ?  1 : retval;
              break;
              }
            case 16:
              {
              //_Float128 left_value  = *(_Float128 *)left_location;
              GCOB_FP128 left_value;
              memcpy(&left_value, left_location, 16);
              GCOB_FP128 right_value = strtofp128(buffer, NULL);
              retval = 0;
              retval = left_value < right_value ? -1 : retval;
              retval = left_value > right_value ?  1 : retval;
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
        retval = compare_strings(   (char *)left_location,
                                    left_length,
                                    left_all,
                                    (char *)right_location,
                                    right_length,
                                    right_all);
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
        left_location = (unsigned char *)left_side->data;
        left_length   = left_side->capacity;
        }

      static size_t right_string_size = MINIMUM_ALLOCATION_SIZE;
      static char *right_string = (char *)malloc(right_string_size);

      right_string = format_for_display_internal(
                             &right_string,
                             &right_string_size,
                             right_side,
                             right_location,
                             right_length,
                             0);

      // There is a tricky aspect to comparing an alphanumeric to
      // a string.  In short, we have to strip off any leading plus sign

      // And, according to the NIST tests, the same is true for minus signs.
      // Apparently, when comparing a number to an alphanumeric, it is
      // considered a "pseudo-move", and the rule for moving a negative
      // number to an alphanumeric is that negative signs get stripped off

      if( *left_location == internal_plus || *left_location == internal_minus )
        {
        left_location += 1;
        left_length -= 1;
        }

      char *right_fixed;
      if( *right_string == internal_plus || *right_string == internal_minus )
        {
        right_fixed = right_string + 1;
        }
      else
        {
        right_fixed = right_string;
        }

      retval = compare_strings(   (char *)left_location,
                                  left_length,
                                  left_all,
                                  right_fixed,
                                  strlen(right_fixed),
                                  right_all);
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
compare_two_records(unsigned char *range1, unsigned char *range2)
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
__gg__sort_table( cblc_field_t    *table,
                  size_t           table_o,
                  size_t          depending_on,
                  size_t          nkeys,
                  cblc_field_t  **keys,
                  size_t         *ascending,
                  int             duplicates )
  {
  size_t buffer_size = 128;
  unsigned char *contents = (unsigned char *)malloc(buffer_size);
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
      contents = (unsigned char *)realloc(contents, buffer_size);
      }
    offsets.push_back(offset);
    memcpy(contents+offset, &record_size, sizeof(size_t));
    offset += sizeof(size_t);
    memcpy(contents+offset, next_record, record_size);
    offset      += record_size;
    next_record += record_size;
    }

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
  //fprintf(stderr, "CALLED WITH %s 0x%x\n", var->name, flag_bits);

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
    //fprintf(stderr, "ABORTING on %2.2d %s %d\n", var->level, var->name, var->type);
    //abort();
    var->data = (unsigned char *)malloc(var->capacity);
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

  char *local_initial = as_initial(var->initial);

  if( var->level == LEVEL88 )
    {
    // We need to convert the options to the internal native codeset

    size_t buffer_size = 4;
    char *buffer = (char *)malloc(buffer_size);

    size_t index = 0;

    cblc_field_t *parent = var->parent;
    switch(parent->type)
      {
      case FldGroup:
      case FldAlphanumeric:
        {
        char *walker = local_initial;
        while(*walker)
          {
          static size_t first_size = MINIMUM_ALLOCATION_SIZE;
          static char *first = (char *)malloc(first_size);
          static size_t last_size = MINIMUM_ALLOCATION_SIZE;
          static char *last = (char *)malloc(last_size);
          if( (*walker & 0xFF) == 0xFF )
            {
            strcpy(first, walker);
            }
          else
            {
            raw_to_internal(&first, &first_size, walker, strlen(walker));
            }
          walker += strlen(first) + 1;

          if( (*walker & 0xFF) == 0xFF )
            {
            strcpy(last, walker);
            }
          else
            {
            raw_to_internal(&last, &last_size, walker, strlen(walker));
            }
          walker += strlen(last) + 1;
          while(index + strlen(first) + strlen(last) + 3 > buffer_size)
            {
            buffer_size *= 2;
            buffer = (char *)realloc(buffer, buffer_size);
            }
          strcpy(buffer+index, first);
          index += strlen(first) + 1;
          strcpy(buffer+index, last);
          index += strlen(last) + 1;
          }
        buffer[index++] = 0;
        break;
        }
      }
    if( index > 0 )
      {
      buffer = (char *)realloc(buffer, index);
      local_initial = buffer;
      }
    }

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
  if( var->data && !explicitly )
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
      case FldAlphanumeric:
      case FldAlphaEdited:
      case FldNumericEdited:
      case FldLiteralA:
        {
        // Any initialization values were converted to single-byte-coding in the
        // right codeset during parser_symbol_add()
        if( var->initial )
          {
          memcpy(outer_location, var->initial, var->capacity);
          }
        else
          {
          if( !defaultbyte_in_play )
            {
            memset(  outer_location,
                     internal_space,
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

      case FldNumericDisplay:
        {
        // Any initialization values were converted to single-byte-coding in the
        // right codeset during parser_symbol_add()
        if( var->initial )
          {
          memcpy(outer_location, var->initial, var->capacity);
          }
        else
          {
          if( !defaultbyte_in_play )
            {
            memset(  outer_location,
                     internal_zero,
                     capacity );
            if( (var->attr & signable_e) && (var->attr & separate_e) )
              {
              if( var->attr & leading_e )
                {
                outer_location[0] = internal_plus;
                }
              else
                {
                outer_location[var->capacity-1] = internal_plus;
                }
              }
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

    char *location = (char *)save_the_location;

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

    outer_location = (unsigned char *)location;
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
//  if( var->type == FldLiteralA )
//    {
//    fprintf(stderr, "BAZINGA!\n");
//    }

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
  // and dest are alphanumeric
  dest_length = dest_length ? dest_length : field->capacity;

  char *to         = (char *)field->data + dest_offset;
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
        memmove(to + (dest_length-count),
                from,
                count);
        memset(to, internal_space, dest_length-count);
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
        memmove(to,
                from,
                count);
        memset( to + count,
                internal_space,
                dest_length-count);
        }
      }
    }
  }

static void
alpha_to_alpha_move(cblc_field_t *dest,
                    size_t dest_offset,
                    size_t dest_size,
                    cblc_field_t *source,
                    size_t source_offset,
                    size_t source_size,
                    bool source_move_all)
  {
  alpha_to_alpha_move_from_location(  dest,
                                      dest_offset,
                                      dest_size,
                                      (char *)(source->data + source_offset),
                                      source_size,
                                      source_move_all);
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

  bool moved = true;

  __int128 value;
  int rdigits;

  size_t min_length;

  cbl_figconst_t source_figconst =
                        (cbl_figconst_t)(fsource->attr & FIGCONST_MASK);
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

    int special_char;
    if( source_figconst == low_value_e )
      {
      special_char = ascii_to_internal(__gg__low_value_character);
      }
    else if( source_figconst == high_value_e )
      {
      special_char = ascii_to_internal(__gg__high_value_character);
      }
    else if( source_figconst == quote_value_e )
      {
      special_char = ascii_to_internal(__gg__quote_character);
      }
    else if( source_figconst == space_value_e )
      {
      special_char = ascii_to_internal(ascii_space);
      }
    memset( fdest->data + dest_offset,
            special_char,
            dest_size);
    }
  else
    {
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
            // This is an ordinary alpha-to-alpha move:
            alpha_to_alpha_move(fdest,
                                dest_offset,
                                dest_size,
                                fsource,
                                source_offset,
                                source_size,
                                !!(source_flags & REFER_T_MOVE_ALL));
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

              int rdigits;
              __int128 value;

              size_t source_digits
                = fsource->digits
                  + ( fsource->rdigits < 0
                      ? -fsource->rdigits : 0) ;

              // Pick up the absolute value of the source
              value = __gg__binary_value_from_qualified_field(&rdigits,
                                                              fsource,
                                                              source_offset,
                                                              source_size);

              char ach[128];

              // Convert it to the full complement of digits available
              // from the source...but no more
              __gg__binary_to_string_internal(ach, source_digits, value);

              if( !(fdest->attr & rjust_e) )
                {
                min_length = std::min(  source_digits,
                                        dest_size);
                memmove(fdest->data + dest_offset, ach, min_length);
                if( min_length < dest_size )
                  {
                  // min_length is smaller than dest_length, so we
                  // have to space-fill the excess bytes in the
                  // destination:
                  memset( fdest->data + dest_offset + min_length,
                          internal_space,
                          dest_size - min_length );
                  }
                }
              else
                {
                // Destination is right-justified, so things are
                // slightly more complex
                if( source_digits >= dest_size )
                  {
                  // We need to truncate the source data on the
                  // left:
                  memmove(
                    fdest->data + dest_offset,
                    ach + (source_digits - dest_size),
                    dest_size );
                  }
                else
                  {
                  // We need to move the shorty source string to
                  // the right side of the destination, and space-fill
                  //  the prefix:
                  memmove(fdest->data + dest_offset + (dest_size - source_digits),
                          ach,
                          source_digits );
                  memset( fdest->data + dest_offset,
                          internal_space,
                          dest_size - source_digits);
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
              __gg__binary_to_string_internal(ach,
                                              source_size,
                                              value);

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
                  if( *(pach+1) == '\0' )
                    {
                    break;
                    }
                  if( ((*pach)&0xFF) != internal_zero )
                    {
                    break;
                    }
                  pach += 1;
                  source_size -= 1;
                  }
                }

              if( !(fdest->attr & rjust_e) )
                {
                min_length = std::min(  source_size,
                                        dest_size);
                memmove(fdest->data+dest_offset, pach, min_length);
                if( min_length < dest_size )
                  {
                  // min_length is smaller than dest_length, so we have to
                  // space-fill the excess bytes in the destination:
                  memset( fdest->data+dest_offset + min_length,
                          internal_space,
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
                  memset(fdest->data+dest_offset, internal_space, (dest_size - source_size));
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
            sprintf(ach, "%lu", (size_t)value);

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
                memset( fdest->data+dest_offset + min_length,
                        internal_space,
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
                memset(fdest->data+dest_offset, internal_space, (dest_size - source_size));
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
                      internal_space,
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
            __int128 value=0;
            switch(fsource->capacity)
              {
              case 4:
                {
                _Float32 val = *(_Float32 *)(fsource->data+source_offset);
                if(val < 0)
                  {
                  negative = true;
                  val = -val;
                  }
                val *= (_Float32)__gg__power_of_ten(rdigits);
                value = (__int128)val;
                break;
                }
              case 8:
                {
                _Float64 val = *(_Float64 *)(fsource->data+source_offset);
                if(val < 0)
                  {
                  negative = true;
                  val = -val;
                  }
                val *= (_Float32)__gg__power_of_ten(rdigits);
                value = (__int128)val;
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
                value = (__int128)val;
                break;
                }
              }
            if( negative )
              {
              value = -value;
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
                      internal_space,
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
            GCOB_FP128 value=0;
            switch(fsource->capacity)
              {
              case 4:
                {
                value = *(_Float32 *)(fsource->data+source_offset);
                break;
                }
              case 8:
                {
                value = *(_Float64 *)(fsource->data+source_offset);
                break;
                }
              case 16:
                {
                // value = *(_Float128 *)(fsource->data+source_offset);
                memcpy(&value, fsource->data+source_offset, 16);
                break;
                }
              }
            __gg__float128_to_qualified_field(
                                    fdest,
                                    dest_offset,
                                    value,
                                    rounded,
                                    &size_error);
            break;
            }

          default:
            moved = false;
            break;
          }
        break;

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
                      internal_space,
                      dest_size - min_length );
              }
            break;

          case FldNumericDisplay:
            {
            int rdigits;
            __int128 value;

            int source_digits = fsource->digits + (fsource->rdigits<0 ? -fsource->rdigits : 0) ;

            // Pick up the absolute value of the source
            value = __gg__binary_value_from_qualified_field(&rdigits,
                                                            fsource,
                                                            source_offset,
                                                            source_size);
            char ach[64];

            // Convert it to the full complement of digits available
            // from the source...but no more
            __gg__binary_to_string(ach, source_digits, value);

            // Binary to string returns ASCII characters:
            for(int i=0; i<source_digits; i++)
              {
              ach[i] = ascii_to_internal(ach[i]);
              }

            // And move them into place:
            __gg__string_to_alpha_edited( (char *)(fdest->data+dest_offset),
                                          ach,
                                          source_digits,
                                          fdest->picture);
            break;
            }

          default:
            {
            static size_t display_string_size = MINIMUM_ALLOCATION_SIZE;
            static char *display_string = (char *)malloc(display_string_size);

            size_t display_string_length = dest_size;
            __gg__realloc_if_necessary( &display_string,
                                        &display_string_size,
                                        display_string_length);

            if( source_figconst == low_value_e )
              {
              memset(display_string, ascii_to_internal(__gg__low_value_character), dest_size);
              }
            else if( source_figconst == zero_value_e )
              {
              memset(display_string, internal_zero, dest_size);
              }
            else if( source_figconst == space_value_e )
              {
              memset(display_string, internal_space, dest_size);
              }
            else if( source_figconst == quote_value_e )
              {
              memset(display_string, ascii_to_internal(__gg__quote_character), dest_size);
              }
            else if( source_figconst == high_value_e )
              {
              memset(display_string, ascii_to_internal(__gg__high_value_character), dest_size);
              }
            else
              {
              display_string = format_for_display_internal(
                                &display_string,
                                &display_string_size,
                                fsource,
                                (unsigned char *)(fsource->data+source_offset),
                                source_size,
                                source_flags && REFER_T_ADDRESS_OF);
              display_string_length = strlen(display_string);
              }
            __gg__string_to_alpha_edited( (char *)(fdest->data+dest_offset),
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
            {
            char ach[256];
            size_t len = std::min(source_size, sizeof(ach)-1);
            memcpy(ach, fsource->data+source_offset, len);
            ach[len] = '\0';
            __gg__internal_to_console_in_place(ach, len);
            switch( fdest->capacity )
              {
              case 4:
                {
                *(float *)(fdest->data+dest_offset) = strtof(ach, NULL);
                break;
                }
              case 8:
                {
                *(double *)(fdest->data+dest_offset) = strtod(ach, NULL);
                break;
                }
              case 16:
                {
                //*(_Float128 *)(fdest->data+dest_offset) = strtofp128(ach, NULL);
                GCOB_FP128 t = strtofp128(ach, NULL);
                memcpy(fdest->data+dest_offset, &t, 16);
                break;
                }
              break;
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
                    cbl_round_t rounded_,
                    const char *str,
                    size_t strlen )
  {
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
      alpha_to_alpha_move_from_location(field, field_offset, field_size, str, strlen, move_all);
      break;
      }

    case FldNumericBinary:
      {
      value = __gg__dirty_to_binary_internal( str,
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
      value = __gg__dirty_to_binary_internal( str,
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
      static char *display_string = (char *)malloc(display_string_size);

      __gg__realloc_if_necessary( &display_string,
                                  &display_string_size,
                                  field_size);

      memset(display_string, internal_space, display_string_size);
      size_t len = std::min(display_string_size, strlen);
      memcpy(display_string, str, len);
      __gg__string_to_alpha_edited( (char *)(field->data+field_offset),
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
          *(float *)(field->data+field_offset) = strtof(ach, NULL);
          break;
          }
        case 8:
          {
          *(double *)(field->data+field_offset) = strtod(ach, NULL);
          break;
          }
        case 16:
          {
          GCOB_FP128 t = strtofp128(ach, NULL);
          memcpy(field->data+field_offset, &t, 16);
          break;
          }
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
  }

extern "C"
void
__gg__sort_workfile(cblc_file_t    *workfile,
                    size_t          nkeys,
                    cblc_field_t  **keys,
                    size_t         *ascending,
                    int             duplicates)
  {
  // We are going to read the records of workfile into memory.  We keep offsets
  // into the memory buffer, and then we'll sort those offsets according to the
  // things they point to.

  // The workfile is open and positioned at zero when we arrive here.

  // Read the file into memory
  size_t buffer_size = 128;
  unsigned char *contents = (unsigned char *)malloc(buffer_size);
  size_t offset = 0;
  std::vector<size_t>offsets;
  size_t bytes_read;
  size_t bytes_to_write;

  for(;;)
    {
    __gg__file_read(workfile,
                    -1);
    if( workfile->record_length )
      {
      int rdigits;
      bytes_read = (size_t) __gg__binary_value_from_field(
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
      contents = (unsigned char *)realloc(contents, buffer_size);
      }
    offsets.push_back(offset);

    // Copy over the record size:
    memcpy(contents+offset, &bytes_read, sizeof(size_t));
    offset += sizeof(size_t);

    // And the contents of the record
    memcpy(contents+offset, workfile->default_record->data, bytes_read);
    offset += bytes_read;
    }

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
      __gg__int128_to_field(workfile->record_length,
                            bytes_to_write,
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

  unsigned char *prior_winner = (unsigned char *)malloc(the_biggest);
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

typedef struct normalized_operand
  {
  // These are the characters of the string.  When the field is NumericDisplay
  // any leading or trailing +/- characters are removed, and any embedded
  // NUMERIC_DISPLAY_SIGN_BIT bits are removed.
  std::string the_characters;
  size_t offset;  // Usually zero.  One when there is a leading sign.
  size_t length;  // Usually the same as the original.  But it is one less
  //              // than the original when there is a trailing sign.
  } normalized_operand;

typedef struct comparand
  {
  size_t id_2_index;
  cbl_inspect_bound_t operation;
  normalized_operand identifier_3; // The thing to be found
  normalized_operand identifier_5; // The replacement, for FORMAT 2
  const char *alpha; // The start location within normalized_id_1
  const char *omega; // The end+1 location within normalized_id_1
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
normalize_id( const cblc_field_t *refer,
              size_t              refer_o,
              size_t              refer_s
              )
  {
  normalized_operand retval;

  if( refer )
    {
    unsigned char *data = refer->data + refer_o;
    cbl_figconst_t figconst
      = (cbl_figconst_t)(refer->attr & FIGCONST_MASK);

    retval.offset = 0;
    retval.length = refer_s;

    if( refer->type == FldNumericDisplay )
      {
      // The value is NumericDisplay.
      if( refer->attr & separate_e )
        {
        // Because the sign is a separate plus or minus, the length
        // gets reduced by one:
        retval.length = refer_s - 1;
        if( refer->attr & leading_e )
          {
          // Because the sign character is LEADING, we increase the
          // offset by one
          retval.offset = 1;
          }
        }
      for( size_t i=retval.offset; i<retval.length; i++ )
        {
        // Because we are dealing with a NumericDisplay that might have
        // the NUMERIC_DISPLAY_SIGN_BIT turned on, we need to mask it off
        unsigned char ch = data[i];
        turn_sign_bit_off(&ch);
        retval.the_characters += ch;
        }
      }
    else
      {
      // We are set up to create the_characters;
      if( figconst == normal_value_e )
        {
        for( size_t i=retval.offset; i<retval.length; i++ )
          {
          retval.the_characters += data[i];
          }
        }
      else
        {
        char ch=0;
        switch( figconst )
          {
          case low_value_e    :
            ch = ascii_to_internal(__gg__low_value_character);
            break;
          case zero_value_e   :
            ch = internal_zero;
            break;
          case space_value_e  :
            ch = internal_space;
            break;
          case quote_value_e  :
            ch = ascii_to_internal(__gg__quote_character);
            break;
          case high_value_e   :
            if( __gg__high_value_character == DEGENERATE_HIGH_VALUE )
              {
              ch = __gg__high_value_character;
              }
            else
              {
              ch = ascii_to_internal(__gg__high_value_character);
              }
            break;
          case normal_value_e:
            // We can't get here
            break;
          case null_value_e:
            break;
          }
        for( size_t i=retval.offset; i<retval.length; i++ )
          {
          retval.the_characters += ch;
          }
        }
      }
    }
  else
    {
    // THere is no field, so leave the_characters empty.
    retval.offset = 0;
    retval.length = 0;
    }
  return retval;
  }

static void
match_lengths(      normalized_operand &id_target,
                    const normalized_operand &id_source)
  {
  char ch = id_target.the_characters[0];
  id_target.the_characters.clear();
  for(size_t i=0; i<id_source.length; i++)
    {
    id_target.the_characters += ch;
    }
  id_target.length = id_source.length;
  }

static void
the_alpha_and_omega(const normalized_operand &id_before,
                    const normalized_operand &id_after,
                    const char *          &alpha,
                    const char *          &omega)
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
    }
  }

static void
the_alpha_and_omega_backward( const normalized_operand &id_before,
                              const normalized_operand &id_after,
                              const char *          &alpha,
                              const char *          &omega)
  {
  /*  Not unlike the_alpha_and_omega(), but for handling BACKWARD.

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
      // it stays at the beginning of id_1
      alpha = found + id_before.length;
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
      omega = id_1;
      }
    }
  }

static
void
inspect_backward_format_1(size_t integers[])
  {
  size_t int_index = 0;
  size_t cblc_index = 0;

  // Reference the language specification for the meanings of identifier_X

  // Pick up the number of identifier_2 loops in this INSPECT statement
  size_t n_identifier_2 = integers[int_index++];

  std::vector<id_2_result> id_2_results(n_identifier_2);

  // Pick up identifier_1, which is the string being inspected
  cblc_field_t *id1   = __gg__treeplet_1f[cblc_index];
  size_t        id1_o = __gg__treeplet_1o[cblc_index];
  size_t        id1_s = __gg__treeplet_1s[cblc_index];
  cblc_index += 1;
  // normalize it, according to the language specification.
  normalized_operand normalized_id_1 = normalize_id(id1, id1_o, id1_s);

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
          comparand next_comparand;
          next_comparand.id_2_index = i;
          next_comparand.operation = operation;
          next_comparand.identifier_3.length = 1;

          cblc_field_t *id4_before   = __gg__treeplet_1f  [cblc_index];
          size_t        id4_before_o = __gg__treeplet_1o[cblc_index];
          size_t        id4_before_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          cblc_field_t *id4_after   = __gg__treeplet_1f  [cblc_index];
          size_t        id4_after_o = __gg__treeplet_1o[cblc_index];
          size_t        id4_after_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          normalized_operand normalized_id_4_before
            = normalize_id(id4_before, id4_before_o, id4_before_s);

          normalized_operand normalized_id_4_after
            = normalize_id(id4_after, id4_after_o, id4_after_s);

          next_comparand.alpha
            = normalized_id_1.the_characters.c_str();

          next_comparand.omega
            = next_comparand.alpha + normalized_id_1.length;

          the_alpha_and_omega_backward( normalized_id_4_before,
                                        normalized_id_4_after,
                                        next_comparand.alpha,
                                        next_comparand.omega);
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
            comparand next_comparand;
            next_comparand.id_2_index = i;
            next_comparand.operation = operation;

            cblc_field_t *id3   = __gg__treeplet_1f[cblc_index];
            size_t        id3_o = __gg__treeplet_1o[cblc_index];
            size_t        id3_s = __gg__treeplet_1s[cblc_index];
            cblc_index += 1;

            cblc_field_t *id4_before   = __gg__treeplet_1f[cblc_index];
            size_t        id4_before_o = __gg__treeplet_1o[cblc_index];
            size_t        id4_before_s = __gg__treeplet_1s[cblc_index];
            cblc_index += 1;

            cblc_field_t *id4_after   = __gg__treeplet_1f[cblc_index];
            size_t        id4_after_o = __gg__treeplet_1o[cblc_index];
            size_t        id4_after_s = __gg__treeplet_1s[cblc_index];
            cblc_index += 1;

            next_comparand.identifier_3
                                    = normalize_id(id3, id3_o, id3_s);

            next_comparand.alpha
              = normalized_id_1.the_characters.c_str();
            next_comparand.omega
              = next_comparand.alpha + normalized_id_1.length;

            normalized_operand normalized_id_4_before
              = normalize_id(id4_before, id4_before_o, id4_before_s);

            normalized_operand normalized_id_4_after
              = normalize_id(id4_after, id4_after_o, id4_after_s);

            the_alpha_and_omega_backward( normalized_id_4_before,
                                          normalized_id_4_after,
                                          next_comparand.alpha,
                                          next_comparand.omega);
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
  const char *leftmost  = normalized_id_1.the_characters.c_str();
  const char *rightmost = leftmost + normalized_id_1.length;
  const char *the_end_of_the_world = rightmost;

  while( leftmost < rightmost )
    {
    rightmost -= 1;
    // We look at the rightmost position.  If that position is within the
    // alpha-to-omega qualified range, we check all possible matches:

    for(size_t k=0; k<comparands.size(); k++)
      {
      if( rightmost < comparands[k].alpha )
        {
        // This can't be a match, because rightmost is
        // to the left of the comparand's alpha.
        continue;
        }
      if( rightmost + comparands[k].identifier_3.length > comparands[k].omega )
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
          if( comparands[k].identifier_3.the_characters[m] != rightmost[m] )
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
                                                        == comparands[k].omega)
                {
                // This means that the match here is just the latest of a
                // string of LEADING matches that started at .omega
                comparands[k].leading_count += 1;
                match = true;
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

            if( (rightmost - comparands[k].alpha )
                    % comparands[k].identifier_3.length == 0 )
              {
              // The remaining number of characters is correct for a match.
              // Keep checking.

              // Assume a match until we learn otherwise:
              match = true;
              const char *local_left = rightmost;
              local_left -= comparands[k].identifier_3.length;
              while( local_left >= comparands[k].alpha )
                {
                for(size_t m=0; m<comparands[k].identifier_3.length; m++)
                  {
                  if( comparands[k].identifier_3.the_characters[m]
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

          // Because we are scanning from right to left, we have to drag
          // the goalpost along with us to ensure that following
          // comparisions don't spill over into the characters we just matched.
          the_end_of_the_world = rightmost;

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
  cblc_field_t *id1   = __gg__treeplet_1f[cblc_index];
  size_t        id1_o = __gg__treeplet_1o[cblc_index];
  size_t        id1_s = __gg__treeplet_1s[cblc_index];
  cblc_index += 1;
  // normalize it, according to the language specification.
  normalized_operand normalized_id_1
                                    = normalize_id(id1, id1_o, id1_s);

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
          comparand next_comparand;
          next_comparand.id_2_index = i;
          next_comparand.operation = operation;
          next_comparand.identifier_3.length = 1;

          cblc_field_t *id4_before   = __gg__treeplet_1f  [cblc_index];
          size_t        id4_before_o = __gg__treeplet_1o[cblc_index];
          size_t        id4_before_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          cblc_field_t *id4_after   = __gg__treeplet_1f  [cblc_index];
          size_t        id4_after_o = __gg__treeplet_1o[cblc_index];
          size_t        id4_after_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          normalized_operand normalized_id_4_before
            = normalize_id(id4_before, id4_before_o, id4_before_s);

          normalized_operand normalized_id_4_after
            = normalize_id(id4_after, id4_after_o, id4_after_s);

          next_comparand.alpha
            = normalized_id_1.the_characters.c_str();

          next_comparand.omega
            = next_comparand.alpha + normalized_id_1.length;

          the_alpha_and_omega(normalized_id_4_before,
                              normalized_id_4_after,
                              next_comparand.alpha,
                              next_comparand.omega);
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
            comparand next_comparand;
            next_comparand.id_2_index = i;
            next_comparand.operation = operation;

            cblc_field_t *id3   = __gg__treeplet_1f[cblc_index];
            size_t        id3_o = __gg__treeplet_1o[cblc_index];
            size_t        id3_s = __gg__treeplet_1s[cblc_index];
            cblc_index += 1;

            cblc_field_t *id4_before   = __gg__treeplet_1f[cblc_index];
            size_t        id4_before_o = __gg__treeplet_1o[cblc_index];
            size_t        id4_before_s = __gg__treeplet_1s[cblc_index];
            cblc_index += 1;

            cblc_field_t *id4_after   = __gg__treeplet_1f[cblc_index];
            size_t        id4_after_o = __gg__treeplet_1o[cblc_index];
            size_t        id4_after_s = __gg__treeplet_1s[cblc_index];
            cblc_index += 1;

            next_comparand.identifier_3
                                    = normalize_id(id3, id3_o, id3_s);

            next_comparand.alpha
              = normalized_id_1.the_characters.c_str();
            next_comparand.omega
              = next_comparand.alpha + normalized_id_1.length;

            normalized_operand normalized_id_4_before
              = normalize_id(id4_before, id4_before_o, id4_before_s);

            normalized_operand normalized_id_4_after
              = normalize_id(id4_after, id4_after_o, id4_after_s);

            the_alpha_and_omega(normalized_id_4_before,
                                normalized_id_4_after,
                                next_comparand.alpha,
                                next_comparand.omega);
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
  const char *leftmost
    = normalized_id_1.the_characters.c_str();
  const char *rightmost
    = leftmost + normalized_id_1.length;

  while( leftmost < rightmost )
    {
    // For each leftmost position, we check each of the
    // pairs:

    for(size_t k=0; k<comparands.size(); k++)
      {
      if( leftmost < comparands[k].alpha )
        {
        // This can't be a match, because leftmost is
        // to the left of the comparand's alpha.
        continue;
        }
      if( leftmost + comparands[k].identifier_3.length > comparands[k].omega )
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
          if( comparands[k].identifier_3.the_characters[m] != leftmost[m] )
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
              size_t count = (leftmost - comparands[k].alpha)
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

            if( (comparands[k].omega-leftmost)
                    % comparands[k].identifier_3.length == 0 )
              {
              // The remaining number of characters is correct for a match.
              // Keep checking.

              // Assume a match until we learn otherwise:
              match = true;
              const char *local_left = leftmost;
              local_left += comparands[k].identifier_3.length;
              while( local_left < comparands[k].omega )
                {
                for(size_t m=0; m<comparands[k].identifier_3.length; m++)
                  {
                  if( comparands[k].identifier_3.the_characters[m]
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
inspect_backward_format_2(size_t integers[])
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
                                   = normalize_id(id1, id1_o, id1_s);

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
        comparand next_comparand;
        next_comparand.operation = operation;

        cblc_field_t *id5   = __gg__treeplet_1f[cblc_index];
        size_t        id5_o = __gg__treeplet_1o[cblc_index];
        size_t        id5_s = __gg__treeplet_1s[cblc_index];
        cblc_index += 1;

        cblc_field_t *id4_before   = __gg__treeplet_1f[cblc_index];
        size_t        id4_before_o = __gg__treeplet_1o[cblc_index];
        size_t        id4_before_s = __gg__treeplet_1s[cblc_index];
        cblc_index += 1;

        cblc_field_t *id4_after   = __gg__treeplet_1f  [cblc_index];
        size_t        id4_after_o = __gg__treeplet_1o[cblc_index];
        size_t        id4_after_s = __gg__treeplet_1s[cblc_index];
        cblc_index += 1;

        next_comparand.identifier_5
          = normalize_id(id5, id5_o, id5_s);
        normalized_operand normalized_id_4_before
          = normalize_id(id4_before, id4_before_o, id4_before_s);
        normalized_operand normalized_id_4_after
          = normalize_id(id4_after, id4_after_o, id4_after_s);

        // Because this is a CHARACTER operation, the lengths of
        // identifier-3 and identifier-5 should be one.  Let's avoid the
        // chaos that will otherwise ensue should the lengths *not* be
        // one.
        next_comparand.identifier_3.length = 1;
        next_comparand.identifier_5.length = 1;

        next_comparand.alpha = normalized_id_1.the_characters.c_str();
        next_comparand.omega
          = next_comparand.alpha + normalized_id_1.length;

        the_alpha_and_omega_backward( normalized_id_4_before,
                                      normalized_id_4_after,
                                      next_comparand.alpha,
                                      next_comparand.omega);
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
          comparand next_comparand;
          next_comparand.operation = operation;

          cblc_field_t *id3   = __gg__treeplet_1f[cblc_index];
          size_t        id3_o = __gg__treeplet_1o[cblc_index];
          size_t        id3_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          cblc_field_t *id5   = __gg__treeplet_1f[cblc_index];
          size_t        id5_o = __gg__treeplet_1o[cblc_index];
          size_t        id5_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          cblc_field_t *id4_before   = __gg__treeplet_1f[cblc_index];
          size_t        id4_before_o = __gg__treeplet_1o[cblc_index];
          size_t        id4_before_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          cblc_field_t *id4_after   = __gg__treeplet_1f[cblc_index];
          size_t        id4_after_o = __gg__treeplet_1o[cblc_index];
          size_t        id4_after_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          next_comparand.identifier_3 = normalize_id(id3, id3_o, id3_s);
          next_comparand.identifier_5 = normalize_id(id5, id5_o, id5_s);

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
            = normalize_id(id4_before, id4_before_o, id4_before_s);
          normalized_operand normalized_id_4_after
            = normalize_id(id4_after, id4_after_o, id4_after_s);

          the_alpha_and_omega_backward( normalized_id_4_before,
                                        normalized_id_4_after,
                                        next_comparand.alpha,
                                        next_comparand.omega);
          next_comparand.leading = true;
          next_comparand.leading_count = 0;
          next_comparand.first   = true;
          comparands.push_back(next_comparand);
          }
        }
      }
    }

  const char *leftmost  = normalized_id_1.the_characters.c_str();
  const char *rightmost = leftmost + normalized_id_1.length;
  const char *the_end_of_the_world = rightmost;

  while( leftmost < rightmost )
    {
    rightmost -= 1;
    // We look at the rightmost position.  If that position is within the
    // alpha-to-omega qualified range, we check all possible matches:

    for(size_t k=0; k<comparands.size(); k++)
      {
      if( rightmost < comparands[k].alpha )
        {
        // This can't be a match, because rightmost is
        // to the left of the comparand's alpha.
        continue;
        }
      if( rightmost + comparands[k].identifier_3.length > comparands[k].omega )
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
          if( comparands[k].identifier_3.the_characters[m] != rightmost[m] )
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
                  + comparands[k].identifier_3.length
                  + comparands[k].leading_count
                    == comparands[k].omega)
                {
                // This means that the match here is just the latest of a
                // string of LEADING matches that started at .omega
                comparands[k].leading_count += 1;
                match = true;
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

            if( (rightmost - comparands[k].alpha )
                    % comparands[k].identifier_3.length == 0 )
              {
              // The remaining number of characters is correct for a match.
              // Keep checking.

              // Assume a match until we learn otherwise:
              match = true;
              const char *local_left = rightmost;
              local_left -= comparands[k].identifier_3.length;
              while( local_left >= comparands[k].alpha )
                {
                for(size_t m=0; m<comparands[k].identifier_3.length; m++)
                  {
                  if( comparands[k].identifier_3.the_characters[m]
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

          size_t index = rightmost - normalized_id_1.the_characters.c_str();
          for( size_t l = 0;
               l < comparands[k].identifier_5.length;
               l++ )
            {
            char ch = comparands[k].identifier_5.
                      the_characters[l];
            normalized_id_1.the_characters[index++] = ch;
            }

          the_end_of_the_world = rightmost;

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
  // back into identifier_1.  There is some special processing to make sure
  // an embedded sign in a NumericDisplay survives the processing.
  unsigned char *id1_data = id1->data + id1_o;
  int index_dest = normalized_id_1.offset;
  if( id1->type == FldNumericDisplay )
    {
    for(size_t i=0; i<normalized_id_1.length; i++)
      {
      id1_data[index_dest] = normalized_id_1.the_characters[i];
      if( is_sign_bit_on (normalized_id_1.the_characters[i]) )
        {
        turn_sign_bit_on(&id1_data[index_dest]);
        }
      else
        {
        turn_sign_bit_off(&id1_data[index_dest]);
        }
      index_dest += 1;
      }
    }
  else
    {
    for(size_t i=0; i<normalized_id_1.length; i++)
      {
      id1_data[index_dest++] = normalized_id_1.the_characters[i];
      }
    }
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
                                   = normalize_id(id1, id1_o, id1_s);

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
        comparand next_comparand;
        next_comparand.operation = operation;

        cblc_field_t *id5   = __gg__treeplet_1f[cblc_index];
        size_t        id5_o = __gg__treeplet_1o[cblc_index];
        size_t        id5_s = __gg__treeplet_1s[cblc_index];
        cblc_index += 1;

        cblc_field_t *id4_before   = __gg__treeplet_1f[cblc_index];
        size_t        id4_before_o = __gg__treeplet_1o[cblc_index];
        size_t        id4_before_s = __gg__treeplet_1s[cblc_index];
        cblc_index += 1;

        cblc_field_t *id4_after   = __gg__treeplet_1f  [cblc_index];
        size_t        id4_after_o = __gg__treeplet_1o[cblc_index];
        size_t        id4_after_s = __gg__treeplet_1s[cblc_index];
        cblc_index += 1;

        next_comparand.identifier_5
          = normalize_id(id5, id5_o, id5_s);
        normalized_operand normalized_id_4_before
          = normalize_id(id4_before, id4_before_o, id4_before_s);
        normalized_operand normalized_id_4_after
          = normalize_id(id4_after, id4_after_o, id4_after_s);

        // Because this is a CHARACTER operation, the lengths of
        // identifier-3 and identifier-5 should be one.  Let's avoid the
        // chaos that will otherwise ensue should the lengths *not* be
        // one.
        next_comparand.identifier_3.length = 1;
        next_comparand.identifier_5.length = 1;

        next_comparand.alpha = normalized_id_1.the_characters.c_str();
        next_comparand.omega
          = next_comparand.alpha + normalized_id_1.length;

        the_alpha_and_omega(normalized_id_4_before,
                            normalized_id_4_after,
                            next_comparand.alpha,
                            next_comparand.omega);
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
          comparand next_comparand;
          next_comparand.operation = operation;

          cblc_field_t *id3   = __gg__treeplet_1f[cblc_index];
          size_t        id3_o = __gg__treeplet_1o[cblc_index];
          size_t        id3_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          cblc_field_t *id5   = __gg__treeplet_1f[cblc_index];
          size_t        id5_o = __gg__treeplet_1o[cblc_index];
          size_t        id5_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          cblc_field_t *id4_before   = __gg__treeplet_1f[cblc_index];
          size_t        id4_before_o = __gg__treeplet_1o[cblc_index];
          size_t        id4_before_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          cblc_field_t *id4_after   = __gg__treeplet_1f[cblc_index];
          size_t        id4_after_o = __gg__treeplet_1o[cblc_index];
          size_t        id4_after_s = __gg__treeplet_1s[cblc_index];
          cblc_index += 1;

          next_comparand.identifier_3 = normalize_id(id3, id3_o, id3_s);
          next_comparand.identifier_5 = normalize_id(id5, id5_o, id5_s);

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
            = normalize_id(id4_before, id4_before_o, id4_before_s);
          normalized_operand normalized_id_4_after
            = normalize_id(id4_after, id4_after_o, id4_after_s);

          the_alpha_and_omega(normalized_id_4_before,
                              normalized_id_4_after,
                              next_comparand.alpha,
                              next_comparand.omega);
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
  const char *leftmost
    = normalized_id_1.the_characters.c_str();
  const char *rightmost
    = leftmost + normalized_id_1.length;

  while( leftmost < rightmost )
    {
    // For each leftmost position, we check each of the
    // comparands

    for(size_t k=0; k<comparands.size(); k++)
      {
      if( leftmost < comparands[k].alpha )
        {
        // This can't be a match, because leftmost is
        // to the left of the comparand's alpha.
        continue;
        }
      if( leftmost + comparands[k].identifier_3.length
          > comparands[k].omega )
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
          if( comparands[k].identifier_3.the_characters[m]
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
              size_t count = (leftmost - comparands[k].alpha)
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

            if( (comparands[k].omega-leftmost)
                    % comparands[k].identifier_3.length == 0 )
              {
              // The remaining number of characters is correct for a match.
              // Keep checking.

              // Assume a match until we learn otherwise:
              match = true;
              const char *local_left = leftmost;
              local_left += comparands[k].identifier_3.length;
              while( local_left < comparands[k].omega )
                {
                for(size_t m=0; m<comparands[k].identifier_3.length; m++)
                  {
                  if( comparands[k].identifier_3.the_characters[m]
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
                         - normalized_id_1.the_characters.c_str();
          for( size_t l = 0;
               l < comparands[k].identifier_5.length;
               l++ )
            {
            char ch = comparands[k].identifier_5.
                      the_characters[l];
            normalized_id_1.the_characters[index++] = ch;
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
  // back into identifier_1.  There is some special processing to make sure
  // an embedded sign in a NumericDisplay survives the processing.
  unsigned char *id1_data = id1->data + id1_o;
  int index_dest = normalized_id_1.offset;
  if( id1->type == FldNumericDisplay )
    {
    for(size_t i=0; i<normalized_id_1.length; i++)
      {
      id1_data[index_dest] = normalized_id_1.the_characters[i];
      if( is_sign_bit_on (normalized_id_1.the_characters[i]) )
        {
        turn_sign_bit_on(&id1_data[index_dest]);
        }
      else
        {
        turn_sign_bit_off(&id1_data[index_dest]);
        }
      index_dest += 1;
      }
    }
  else
    {
    for(size_t i=0; i<normalized_id_1.length; i++)
      {
      id1_data[index_dest++] = normalized_id_1.the_characters[i];
      }
    }
  return;
  }

extern "C"
void
__gg__inspect_format_4( int backward,
                        cblc_field_t *input,
                        size_t        input_offset,
                        size_t        input_size,
                        cblc_field_t *original,
                        size_t        original_offset,
                        size_t        original_size,
                        cblc_field_t *replacement,
                        size_t        replacement_offset,
                        size_t        replacement_size,
                        cblc_field_t *after,
                        size_t        after_offset,
                        size_t        after_size,
                        cblc_field_t *before,
                        size_t        before_offset,
                        size_t        before_size
                        )
  {
  static size_t psz_input_size       = MINIMUM_ALLOCATION_SIZE;
  static size_t psz_original_size    = MINIMUM_ALLOCATION_SIZE;
  static size_t psz_replacement_size = MINIMUM_ALLOCATION_SIZE;
  static size_t psz_after_size       = MINIMUM_ALLOCATION_SIZE;
  static size_t psz_before_size      = MINIMUM_ALLOCATION_SIZE;
  static size_t psz_figstring_size   = MINIMUM_ALLOCATION_SIZE;

  static char *psz_input       = (char *)malloc(psz_input_size      );
  static char *psz_original    = (char *)malloc(psz_original_size   );
  static char *psz_replacement = (char *)malloc(psz_replacement_size);
  static char *psz_after       = (char *)malloc(psz_after_size      );
  static char *psz_before      = (char *)malloc(psz_before_size     );
  static char *psz_figstring   = (char *)malloc(psz_figstring_size  );

  bool all = replacement_size == (size_t)(-1LL);
  if( all )
    {
    replacement_size = psz_original_size;
    }

  psz_input       = format_for_display_local(&psz_input      , &psz_input_size      , input      , input_offset      , input_size      , 0);
  psz_original    = format_for_display_local(&psz_original   , &psz_original_size   , original   , original_offset   , original_size   , 0);
  psz_replacement = format_for_display_local(&psz_replacement, &psz_replacement_size, replacement, replacement_offset, replacement_size, 0);
  psz_after       = format_for_display_local(&psz_after      , &psz_after_size      , after      , after_offset      , after_size      , 0);
  psz_before      = format_for_display_local(&psz_before     , &psz_before_size     , before     , before_offset     , before_size     , 0);

  if( all )
    {
    memset(psz_replacement, *(replacement->data+replacement_offset), replacement_size);
    }

  cbl_figconst_t figconst =
                (cbl_figconst_t)(replacement->attr & FIGCONST_MASK);
  if( figconst )
    {
    size_t figchars = strlen(psz_input)+1;
    __gg__realloc_if_necessary(&psz_figstring, &psz_figstring_size, figchars);
    char figchar = '\0';
    switch( figconst )
      {
      case normal_value_e:
        abort();
        break;
      case low_value_e   :
        figchar = __gg__low_value_character;
        break;
      case zero_value_e  :
        figchar = internal_0;
        break;
      case space_value_e :
        figchar = internal_space;
        break;
      case quote_value_e :
        figchar = ascii_to_internal(__gg__quote_character);
        break;
      case high_value_e  :
        figchar = __gg__high_value_character;
        break;
      case null_value_e:
        break;
      }
    memset(psz_figstring, figchar, figchars-1);
    psz_figstring[figchars] = '\0';
    psz_replacement = psz_figstring;
    }

  // Use a simple map to make this O(N), rather than an O(N-squared),
  // computational complexity
  static const unsigned char map_init[256] =
    {
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
    0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
    0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
    0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
    0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
    0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
    0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
    0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
    0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff
    };
  unsigned char map[256];
  unsigned char replaced[256];

  memcpy(map, map_init, 256);
  memset(replaced, 0, 256);

  for(size_t i=0; i<strlen(psz_original); i++)
    {
    if( !replaced[(unsigned char )psz_original[i]] )
      {
      // The rule is, if the same character appears more than once in the
      // original (which is identifier-6), then the first occurrence of the
      // character is used for replacement
      map[ (unsigned char )psz_original[i] ] = (unsigned char )psz_replacement[i];
      replaced[(unsigned char )psz_original[i]] = 1;
      }
    }

  char *pstart = NULL;
  char *pend = NULL;
  if( backward )
    {
    if( strlen(psz_before) )
      {
      size_t nfound = std::string(psz_input).rfind(psz_before);
      if( nfound == std::string::npos )
        {
        // The BEFORE string isn't in the input, so we will scan from
        // the leftmost character
        pstart = psz_input;
        }
      else
        {
        pstart = psz_input + nfound;
        if( !pstart )
          {
          pstart = psz_input;
          }
        pstart += strlen(psz_before);
        }
      }
    else
      {
      pstart = psz_input;
      }

    if( strlen(psz_after) )
      {
      size_t nfound = std::string(psz_input).rfind(psz_after);
      if( nfound == std::string::npos )
        {
        nfound = strlen(psz_input);
        }
      pend = psz_input + nfound;
      }
    if( !pend )
      {
      pend = psz_input+strlen(psz_input);
      }
    }
  else
    {
    if( strlen(psz_after) )
      {
      pstart = strstr(psz_input, psz_after);
      }
    if( !pstart )
      {
      pstart = psz_input;
      }
    pstart += strlen(psz_after);

    if( strlen(psz_before) )
      {
      pend = strstr(psz_input, psz_before);
      }
    if( !pend )
      {
      pend = psz_input + strlen(psz_input);
      }
    }


  while(pstart && pstart < pend)
    {
    *pstart = map[(unsigned char)*pstart];
    pstart += 1;
    }

  memcpy(input->data+input_offset, psz_input, input_size);
  }

static void
move_string(cblc_field_t *field,
                size_t offset,
                size_t length,
                const char *from,
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
      char *to = (char *)(field->data + offset);
      size_t dest_length = length ? length : field->capacity;
      size_t source_length = strlen_from;
      size_t count = std::min(dest_length, source_length);

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
          // The destination is right-justified, and the source is an
          // ordinary string too short to fill it.  So, we space-fill
          // the leading characters.
          memmove(to + (dest_length-count),
                  from,
                  count);
          memset(to, internal_space, dest_length-count);
          }
        else
          {
          // The destination is left-justified
          // We do the move first, in case this is an overlapping move
          // involving characters that will be space-filled
          memmove(to,
                  from,
                  count);
          memset( to + count,
                  internal_space,
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
      __int128 value = __gg__dirty_to_binary_internal( from,
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
brute_force_trim(char *str)
  {
  char *retval = str;
  while( *retval == internal_space )
    {
    retval += 1;
    }
  char *p = retval + strlen(retval)-1;
  while( p > retval && *p == internal_space )
    {
    *p-- = NULLCH;
    }
  return retval;
  }

extern "C"
int
__gg__string(size_t integers[])
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
  size_t        *ref_o = __gg__treeplet_1o;
  size_t        *ref_s = __gg__treeplet_1s;

  static const int INDEX_OF_POINTER = 1;

  size_t index_int  = 0;
  size_t index_cblc = 0 ;

  char figlow[2]   = {ascii_to_internal(__gg__low_value_character), 0x00};
  char fighigh[2]  = {ascii_to_internal(__gg__high_value_character), 0x00};
  char figzero[2]  = {(char)internal_zero, 0x00};
  char figquote[2] = {ascii_to_internal(__gg__quote_character), 0x00};
  char figspace[2] = {(char)internal_space, 0x00};

  if( __gg__high_value_character == DEGENERATE_HIGH_VALUE )
    {
    fighigh[0] = __gg__high_value_character;
    }
  else
    {
    fighigh[0] = ascii_to_internal(__gg__high_value_character);
    }

  // Pick up the number of identifier-2 values
  size_t N = integers[index_int++];

  // Pick up the target
  cblc_field_t *tgt   = ref[index_cblc];
  size_t tgt_o        = ref_o[index_cblc];
  size_t tgt_s        = ref_s[index_cblc];
  index_cblc += 1;
  char  *dest         = (char *)(tgt->data + tgt_o);
  ssize_t dest_length = tgt_s;

  // Skip over the index of POINTER:
  index_cblc += 1;

  // Pick up the pointer, if any
  ssize_t pointer = 0;
  if( ref[INDEX_OF_POINTER] )
    {
    int rdigits;
    pointer = (size_t)__gg__binary_value_from_qualified_field(
                                                    &rdigits,
                                                    ref  [INDEX_OF_POINTER],
                                                    ref_o[INDEX_OF_POINTER],
                                                    ref_s[INDEX_OF_POINTER]
                                                    );
    pointer -= 1;
    }

  int overflow = 0;

  // Make sure that the destination pointer is within the destination
  if( pointer >= 0 || pointer < dest_length )
    {
    // We are go for looping through identifier-2 values:

    for( size_t i=0; i<N; i++ )
      {
      size_t M = integers[index_int++];

      // Pick up the identifier_2 DELIMITED BY value
      cblc_field_t *id2   = ref[index_cblc];
      size_t        id2_o = ref_o[index_cblc];
      size_t        id2_s = ref_s[index_cblc];
      index_cblc += 1;

      char *piece;
      char *piece_end;
      cbl_figconst_t figconst = (cbl_figconst_t) ( id2
                                  ? (id2->attr & FIGCONST_MASK)
                                  : 0 );
      switch(figconst)
        {
        case low_value_e:
          piece = figlow;
          piece_end = piece + 1;
          break;
        case zero_value_e:
          piece = figzero;
          piece_end = piece + 1;
          break;
        case space_value_e:
          piece = figspace;
          piece_end = piece + 1;
          break;
        case quote_value_e:
          piece = figquote;
          piece_end = piece + 1;
          break;
        case high_value_e:
          piece = fighigh;
          piece_end = piece + 1;
          break;
        default:
          piece = id2 ? (char *)(id2->data + id2_o) : NULL;
          piece_end = id2 ? piece + id2_s : NULL;
          break;
        }

      for(size_t i=0; i<M; i++)
        {
        // Pick up the next identifier-1 source string:
        cblc_field_t *id1 = ref[index_cblc];
        size_t id1_o = ref_o[index_cblc];
        size_t id1_s = ref_s[index_cblc];
        index_cblc += 1;

        const char *whole = id1 ? (const char *)(id1->data + id1_o): NULL ;
        const char *whole_end = id1 ? whole + id1_s : NULL;

        // As usual, we need to cope with figurative constants:
        cbl_figconst_t figconst = (cbl_figconst_t) ( id1 ? (id1->attr & FIGCONST_MASK) : 0 );
        switch( figconst )
          {
          case low_value_e:
            whole = figlow;
            whole_end = whole + 1;
            break;
          case zero_value_e:
            whole = figzero;
            whole_end = whole + 1;
            break;
          case space_value_e:
            whole = figspace;
            whole_end = whole + 1;
            break;
          case quote_value_e:
            whole = figquote;
            whole_end = whole + 1;
            break;
          case high_value_e:
            whole = fighigh;
            whole_end = whole + 1;
            break;
          default:
            break;
          }

        if(piece)
          {
          const char *found = funky_find(   piece, piece_end,
                                            whole, whole_end);
          if(found)
            {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
            char *wfound = (char *)found;
#pragma GCC diagnostic pop
            whole_end = wfound;
            }
          }
        while(whole < whole_end)
          {
          if(pointer >= dest_length)
            {
            overflow = 1;
            break;
            }
          dest[pointer++] = *whole++;
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
  static char *display_string = (char *)malloc(display_string_size);

  format_for_display_internal(&display_string,
                              &display_string_size,
                              field,
                              qual_data,
                              qual_size,
                              !!(flags & REFER_T_ADDRESS_OF) );

  // Let's honor the locale of the system, as best we can:
  static size_t converted_size = MINIMUM_ALLOCATION_SIZE;
  static char *converted = (char *)malloc(converted_size);

  internal_to_console(&converted, &converted_size, display_string, strlen(display_string));

  ssize_t ss = write( file_descriptor,
                      converted,
                      strlen(converted));
  if(ss == -1)
    {
    fprintf(stderr, "__gg__display() %s %p\n", field->name, qual_data);
    fprintf(stderr, "__gg__display() %zd\n", converted_size);
    fprintf(stderr, "__gg__display() ");
    for(size_t i=0; i<converted_size; i++)
      {
      fprintf(stderr, "%c(%2.2x) ", converted[i]<32 ? '?' : converted[i], converted[i]);
      }
    __gg__abort("display_both() some kind of write() error");
    fprintf(stderr, "\n");
    }

  if( advance )
    {
    ss = write( file_descriptor,
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
                size ? size : field->capacity,
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
__gg__display_string( int     file_descriptor,
                      char   *str,
                      size_t  length,
                      int     advance )
  {
  // Let's honor the locale of the system, as best we can:
  static size_t converted_size = MINIMUM_ALLOCATION_SIZE;
  static char *converted = (char *)malloc(converted_size);

  size_t max_possible = 2 * length;
  if( max_possible > converted_size )
    {
    converted_size = max_possible;
    converted = (char *)realloc(converted, converted_size);
    }

  __gg__ascii_to_console(&converted, &converted_size, str, length);

  write( file_descriptor,
         converted,
         strlen(converted));
  if( advance )
    {
    write( file_descriptor,
           "\n",
           1);
    }
  }

#pragma GCC diagnostic push

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

  while( s < eos && *s == internal_space )
    {
    s += 1;
    }
  while( s < eos && *(eos-1) == internal_space )
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

  char *buffer = (char *)malloc(max_chars+1);
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
      console_to_internal(buffer, i);
      move_string(field,
                  offset,
                  length,
                  buffer,
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
                                        cblc_field_t *var,
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
__gg__float128_from_qualified_field( cblc_field_t *field, size_t offset, size_t size)
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
float128_to_int128( int          *rdigits,
                    cblc_field_t *field,
                    GCOB_FP128     value,
                    cbl_round_t   rounded,
                    int          *compute_error)
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
              *(float *)(data) = -INFINITY;
              }
            else
              {
              *(float *)(data) = INFINITY;
              }
            }
          else
            {
            *(float *)(data) = (float)value;
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
              *(double *)(data) = -INFINITY;
              }
            else
              {
              *(double *)(data) = INFINITY;
              }
            }
          else
            {
            *(double *)(data) = (double)value;
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
__gg__set_initial_switch_value( )
  {
  // We need to establish the initial value of the UPSI-1 switch register
  // We are using IBM's conventions:
  // https://www.ibm.com/docs/en/zvse/6.2?topic=SSB27H_6.2.0/fa2sf_communicate_appl_progs_via_job_control.html
  // UPSI 10000110 means that bits 0, 5, and 6 are on, which means that SW-0, SW-5, and SW-6 are on.

  __int128 value = 0;
  __int128 bit = 1;
  char ach[129];
  memset(ach, 0, sizeof(ach));
  char *p = getenv("UPSI");
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
  int retval = 1;
  bool signable = !!(field->attr & signable_e);
  bool leading  = !!(field->attr & leading_e);
  bool separate = !!(field->attr & separate_e);

  char *digits   = (char *)(field->data + offset);
  char *digits_e = digits + size;

  if( leading && separate && signable )
    {
    // First character must be +/-
    if(     digits < digits_e
        || (   *digits != internal_plus
            && *digits != internal_minus) )
      {
      retval = 0;
      }
    digits += 1;
    }

  if( !leading && separate && signable )
    {
    // Last character must be +/-
    digits_e -= 1;
    if(     digits < digits_e
        || (   *digits_e != internal_plus
            && *digits_e != internal_minus) )
      {
      retval = 0;
      }
    }

  if( leading && !separate && signable )
    {
    // The first character is allowed to have a sign bit.
    if( digits < digits_e )
      {
      unsigned char first_char = (unsigned char)*digits;
      turn_sign_bit_off(&first_char);
      if(first_char<internal_0 || first_char>internal_9)
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
      unsigned char final_char = (unsigned char)*digits_e;
      turn_sign_bit_off(&final_char);
      if(final_char<internal_0 || final_char>internal_9)
        {
        retval = 0;
        }
      }
    }

  // all remaining characters are supposed to be zero through nine
  while( digits < digits_e )
    {
    if(     (unsigned char)(*digits)<internal_0
        ||  (unsigned char)(*digits)>internal_9 )
      {
      retval = 0;
      break;
      }
    digits += 1;
    }
  return retval;
  }

static int
is_packed_numeric(cblc_field_t *field, size_t offset, size_t size)
  {
  int retval = 1;
  bool is_comp6 = !!(field->attr&packed_no_sign_e);
  int digits = field->digits;
  bool signable = !!(field->attr & signable_e);
  unsigned char *bytes   = field->data + offset;

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
is_alpha_a_number(cblc_field_t *field, size_t offset, size_t size)
  {
  int retval = 1;
  unsigned char *bytes = (field->data + offset);
  for( size_t i=0; i<size; i++ )
    {
    unsigned char ch = bytes[i];
    if(    (ch < internal_0)
        || (ch > internal_9) )
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
  int retval = 1;

  const unsigned char *alpha = (unsigned char *)(field->data+offset);

  size_t str_length = size;

  const unsigned char *omega = alpha + str_length;

  if(alpha >= omega)
    {
    // If there is nothing there, then it can't be TRUE.  Can it?
    retval = 0;
    }

  unsigned char ch;
  switch(type)
    {
    case ClassNumericType:
      {
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

      break;
      }

    case ClassAlphabeticType:
      while(alpha < omega)
        {
        ch = (*alpha++)&0xFF;
        if( ch == internal_space )
          {
          continue;
          }
        // If necessary, this could be sped up with the creation of
        // appropriate mapping tables.

        // The oddball construction of this if() statement is a consequence of
        // EBCDIC.  Because of peculiarities going all the back to the encoding
        // of characters on IBM cards, where it wasn't a good idea to have too
        // many consecutive punches in a column because it would weaken the card
        // to the point where its structural integrity might be threatened, the
        // coding for the letter of the alphabet are not contiguous.
        if(!(   (    ch >= internal_A && ch <= internal_I)
                ||  (ch >= internal_J && ch <= internal_R)
                ||  (ch >= internal_S && ch <= internal_Z)
                ||  (ch >= internal_a && ch <= internal_i)
                ||  (ch >= internal_j && ch <= internal_r)
                ||  (ch >= internal_s && ch <= internal_z) ) )
          {
          // The character is not alphabetic
          retval = 0;
          break;
          }
        }
      break;

    case ClassLowerType:
      while(alpha < omega)
        {
        ch = *alpha++;
        if( ch == internal_space )
          {
          continue;
          }
        if(!(   (    ch >= internal_a && ch <= internal_i)
                ||  (ch >= internal_j && ch <= internal_r)
                ||  (ch >= internal_s && ch <= internal_z) ) )
          {
          retval = 0;
          break;
          }
        }
      break;
    case ClassUpperType:
      while(alpha < omega)
        {
        ch = *alpha++;
        if( ch == internal_space )
          {
          continue;
          }
        if(!(   (    ch >= internal_A && ch <= internal_I)
                ||  (ch >= internal_J && ch <= internal_R)
                ||  (ch >= internal_S && ch <= internal_Z) ) )
          {
          retval = 0;
          break;
          }
        }
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

  return retval;
  }

extern "C"
int
__gg__accept_envar( cblc_field_t *tgt,
                    size_t        tgt_offset,
                    size_t        tgt_length,
                    cblc_field_t *name,
                    size_t        name_offset,
                    size_t        name_length)
  {
  int retval;
  tgt_length  = tgt_length  ? tgt_length  : tgt->capacity;
  name_length = name_length ? name_length : name->capacity;

  // Pick up the environment variable name, which is in teh internal codeset
  static char   *env        = NULL;
  static size_t  env_length = 0;
  if( env_length < name_length+1 )
    {
    env_length = name_length+1;
    env = (char *)realloc(env, env_length);
    }
  memcpy(env, name->data + name_offset, name_length);
  env[name_length] = '\0';

  // Get rid of leading and trailing internal_space characters:
  char *trimmed_env = brute_force_trim(env);

  // Convert the name to the console codeset:
  __gg__internal_to_console_in_place(trimmed_env, strlen(trimmed_env));

  // Pick up the environment variable, and convert it to the internal codeset
  char *p = getenv(trimmed_env);
  if(p)
    {
    char *pp = strdup(p);
    console_to_internal(pp, strlen(pp));
    retval = 0; // Okay
    move_string(tgt, tgt_offset, tgt_length, pp);
    free(pp);
    }
  else
    {
    retval = 1; // Could't find it
    exception_raise(ec_argument_imp_environment_e);
    }

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
    env = (char *)realloc(env, env_length);
    }
  if( val_length < value_length+1 )
    {
    val_length = value_length+1;
    val = (char *)realloc(val, val_length);
    }

  // The name and the value arrive in the internal codeset:
  memcpy(env, name->data+name_offset  , name_length);
  env[name_length] = '\0';
  memcpy(val, value->data+value_offset, value_length);
  val[value_length] = '\0';

  // Get rid of leading and trailing internal_space characters
  char *trimmed_env = brute_force_trim(env);
  char *trimmed_val = brute_force_trim(val);

  // Conver them to the console codeset
  __gg__internal_to_console_in_place(trimmed_env, strlen(trimmed_env));
  __gg__internal_to_console_in_place(trimmed_val, strlen(trimmed_val));

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
        char *p_end = p + bytes_read;
        char prior_char = '\0';
        while( p < p_end )
          {
          if( prior_char == '\0' )
            {
            stashed_argc += 1;
            stashed_argv = (char **)realloc(stashed_argv,
                                            stashed_argc * sizeof(char *));
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
  ascii_to_internal_str(ach, strlen(ach));
  move_string(dest, offset, length, ach);
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
    char *retval = strdup(stashed_argv[N]);
    console_to_internal(retval, strlen(retval));
    move_string(dest, dest_offset, dest_length, retval);
    free(retval);
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
  char *retval = (char *)malloc(length);
  *retval = NULLCH;

  for( int i=1; i<stashed_argc; i++ )
    {
    while( strlen(retval) + strlen(stashed_argv[i]) + 2 > length )
      {
      length *= 2;
      retval = (char *)realloc(retval, length);
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
    console_to_internal(retval, strlen(retval));
    move_string(field, offset, flength, retval);
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
__gg__set_pointer(cblc_field_t *target,
                  size_t        target_o,
                  int           target_flags,
                  cblc_field_t *source,
                  size_t        source_o,
                  int           source_flags)
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
      source_address = *(void **)(source->data + source_o);
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
    target->data  = (unsigned char *)source_address;
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
              *(unsigned char *)source_address,
              target->capacity);
      }
    else
      {
      *(void **)(target->data+target_o) = source_address;
      }
    }
  }

extern "C"
void
__gg__alphabet_use( cbl_encoding_t encoding,
                    size_t alphabet_index)
  {
  // We simply replace the values in the current program_state.  If the
  // state needs to be saved -- for example, if we are doing a SORT with an
  // ALPHABET override -- that's up to the caller

  // When there is no DATA DIVISION, program_states can be empty when
  // we arrive here.
  if( program_states.empty() )
    {
    initialize_program_state();
    }

  switch( encoding )
    {
    case ASCII_e:
    case iso646_e:
      __gg__low_value_character  = DEGENERATE_LOW_VALUE;
      __gg__high_value_character = DEGENERATE_HIGH_VALUE;

      program_states.back().rt_low_value_character   = DEGENERATE_LOW_VALUE;
      program_states.back().rt_high_value_character  = DEGENERATE_HIGH_VALUE;

      if( !internal_is_ebcdic )
        {
        program_states.back().rt_collation = __gg__one_to_one_values;
        }
      else
        {
        program_states.back().rt_collation = __gg__ebcdic_to_cp1252_collation;
        }

      break;

    case EBCDIC_e:
      __gg__low_value_character  = DEGENERATE_LOW_VALUE;
      __gg__high_value_character = DEGENERATE_HIGH_VALUE;

      program_states.back().rt_low_value_character   = DEGENERATE_LOW_VALUE;
      program_states.back().rt_high_value_character  = DEGENERATE_HIGH_VALUE;
      if( internal_is_ebcdic )
        {
        program_states.back().rt_collation = __gg__one_to_one_values;
        }
      else
        {
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
    }
  return;
  }

extern "C"
void
__gg__ascii_to_internal_field(cblc_field_t *var)
  {
  ascii_to_internal_str((char *)var->data, var->capacity);
  }

extern "C"
void
__gg__ascii_to_internal(char *location, size_t length)
  {
  ascii_to_internal_str(location, length);
  }

extern "C"
void
__gg__console_to_internal(char *location, size_t length)
  {
  console_to_internal(location, length);
  }

extern "C"
void
__gg__parser_set_conditional(cblc_field_t *var, int figconst_)
  {
  cbl_figconst_t figconst = (cbl_figconst_t)figconst_;

  unsigned char special = internal_space;
  switch(figconst)
    {
    case space_value_e:
      special = *__gg__data_space;
      break;
    case low_value_e:
      special = *__gg__data_low_values;
      break;
    case high_value_e:
      special = *__gg__data_high_values;
      break;
    case zero_value_e:
      special = *__gg__data_zeros;
      break;
    case quote_value_e:
      special = *__gg__data_quotes;
      break;
    default:
      break;
    }
  memset( var->data, special, var->capacity);
  }

extern "C"
void
__gg__internal_to_console_in_place(char *loc, size_t length)
  {
  static size_t dest_size = MINIMUM_ALLOCATION_SIZE;
  static char *dest = (char *)malloc(dest_size);

  internal_to_console(&dest, &dest_size, loc, length);
  memcpy(loc, dest, length);
  }

extern "C"
int
__gg__routine_to_call(char *name,
                      int  program_id)
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
  int i=0;

  if( names )
    {
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
__gg__fetch_call_by_value_value(cblc_field_t *field,
                                size_t field_o,
                                size_t field_s)

  {
  int rdigits;
  unsigned char *data = field->data + field_o;
  size_t         length = field_s;

  __int128 retval = 0;
  switch(field->type)
    {
    case FldGroup:
    case FldAlphanumeric:
    case FldAlphaEdited:
    case FldLiteralA:
      retval = *(char *)data;
      break;

    case FldFloat:
      {
      switch(length)
        {
        case 4:
          *(float *)(&retval) = *(float *)data;
          break;

        case 8:
          *(double *)(&retval) = *(double *)data;
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
          *(float *)(dest->data) = *(float *)&parameter;
          break;

        case 8:
          *(double *)(dest->data) = *(double *)&parameter;
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
__gg__literaln_alpha_compare(char *left_side,
                             cblc_field_t *right,
                             size_t        offset,
                             size_t        length,
                             int  flags)
  {
  int retval;
  if( length == 0 )
    {
    length = right->capacity;
    }
  retval = compare_strings(   (char *)left_side,
                              strlen(left_side),
                              false,
                              (char *)right->data + offset,
                              length,
                              !!(flags & REFER_T_MOVE_ALL) );
  return retval;
  }

static char *
string_in(char *str, char *str_e, char *frag, char *frag_e)
  {
  // This simple routine could be improved.  Instead of using memcmp, we could
  // use established, albeit complex, techniques of string searching:

  // Looking for "abcde" in "abcdabcde", for example.  One could notice that
  // starting at the first 'a' results in a mismatch at the second 'a'.  There
  // is thus no need to start the second search at the first 'b' in the searched
  // string; one could jump ahead to the second 'a' and continue from there.

  // Feel free.  It won't matter in the real world; a program whose innermost
  // loop is an UNSTRING is difficult to imagine.  But feel free.

  char *retval = NULL;
  size_t nchars = frag_e - frag;
  char *p = str;
  while( p + nchars <= str_e )
    {
    if( memcmp(p, frag, nchars) == 0 )
      {
      retval = p;
      break;
      }
    p += 1;
    }
  return retval;
  }

extern "C"
int
__gg__unstring( cblc_field_t *id1,        // The string being unstring
                size_t        id1_o,
                size_t        id1_s,
                size_t ndelimiteds,       // The number of DELIMITED entries
                char *all_flags,          // The number of ALL flags, one per ndelimiteds
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

  cblc_field_t **id2   = __gg__treeplet_1f;      // The delimiting strings; one per ndelimiteds
  size_t        *id2_o = __gg__treeplet_1o;
  size_t        *id2_s = __gg__treeplet_1s;
  cblc_field_t **id4   = __gg__treeplet_2f;      // The delimited string; one per nreceiver
  size_t        *id4_o = __gg__treeplet_2o;
  size_t        *id4_s = __gg__treeplet_2s;
  cblc_field_t **id5   = __gg__treeplet_3f;      // The delimiting string; one per receiver
  size_t        *id5_o = __gg__treeplet_3o;
  size_t        *id5_s = __gg__treeplet_3s;
  cblc_field_t **id6   = __gg__treeplet_4f;      // The count of characters examined;  one per receiver
  size_t        *id6_o = __gg__treeplet_4o;
  size_t        *id6_s = __gg__treeplet_4s;

  // Initialize the state variables
  int overflow = 0;
  int tally = 0;
  int pointer = 1;
  size_t nreceiver;
  char *left  = NULL;
  char *right = NULL;
  int previous_delimiter;

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
    pointer = (int)__gg__binary_value_from_qualified_field(&rdigits,
                                                         id7,
                                                         id7_o,
                                                         id7_s);
    }

  // As per the spec, if the string is zero-length; we are done.
  if( id1_s == 0 )
    {
    goto done;
    }

  // As per the spec, we have an overflow condition if pointer is out of
  // range:
  if( pointer < 1 || pointer > (int)id1_s )
    {
    overflow = 1;
    goto done;
    }

  left  = (char *)(id1->data+id1_o) + pointer-1;
  right = (char *)(id1->data+id1_o) + id1_s;

  if( ndelimiteds == 0 )
    {
    // There are no DELIMITED BY identifier-2 values, so we just peel off
    // characters from identifier-1 and put them into each identifier-4:
    for( size_t i=0; i<nreceivers; i++ )
      {
      if( left >= right )
        {
        break;
        }
      size_t id_4_size = id4_s[i];
      if( id4[i]->attr & separate_e )
        {
        // The receiver is NumericDisplay with a separate signe
        id_4_size = id4_s[i] - 1;
        }

      // Make sure id_4_size doesn't move past the end of the universe
      if( left + id_4_size > right )
        {
        id_4_size = right - left;
        }

      // Move the data into place:
      move_string(id4[i], id4_o[i], id4_s[i], left, id_4_size);

      // Update the state variables:
      left += id_4_size;
      pointer += id_4_size;
      tally += 1;
      }
    goto done;
    }

  // Arriving here means there is some number of ndelimiteds

  nreceiver = 0;
  previous_delimiter = -1;
  while( left < right )
    {
    // Starting at 'left', see if we can find any of the delimiters
    char *leftmost_delimiter = NULL;
    int ifound = -1;
    cbl_figconst_t figconst;
    char achfigconst[1];
    for( size_t i=0; i<ndelimiteds; i++ )
      {
      char *pfound;
      figconst = (cbl_figconst_t)(id2[i]->attr  & FIGCONST_MASK);

      switch(figconst)
        {
        case low_value_e    :
          achfigconst[0] = ascii_to_internal(__gg__low_value_character);
          pfound = string_in( left,
                              right,
                              achfigconst,
                              achfigconst+1);
          break;

        case zero_value_e   :
          achfigconst[0] = internal_zero;
          pfound = string_in( left,
                              right,
                              achfigconst,
                              achfigconst+1);
          break;

        case space_value_e  :
          achfigconst[0] = internal_space;
          pfound = string_in( left,
                              right,
                              achfigconst,
                              achfigconst+1);
          break;

        case quote_value_e  :
          achfigconst[0] = ascii_to_internal(__gg__quote_character);
          pfound = string_in( left,
                              right,
                              achfigconst,
                              achfigconst+1);
          break;

        case high_value_e   :
          achfigconst[0] = ascii_to_internal(__gg__high_value_character);
          pfound = string_in( left,
                              right,
                              achfigconst,
                              achfigconst+1);
          break;

        case normal_value_e :
        default:
          pfound = string_in( left,
                              right,
                              (char *)(id2[i]->data+id2_o[i]),
                              (char *)(id2[i]->data+id2_o[i]) + id2_s[i]);
          break;
        }

      if( pfound )
        {
        // We found a delimiter
        if( !leftmost_delimiter || pfound < leftmost_delimiter )
          {
          ifound = i;
          leftmost_delimiter = pfound;
          }
        }
      }

    if(    ifound >= 0
        && leftmost_delimiter == left
        && ifound == previous_delimiter )
      {
      // We found another instance of an ALL delimiter.
      // So, we just skip it.
      left    += id2_s[previous_delimiter];
      pointer += id2_s[previous_delimiter];
      continue;
      }

    // We did not re-find an ALL DELIMITER
    previous_delimiter = -1;

    // If we've used up all receivers, we bail at this point
    if( nreceiver >= nreceivers )
      {
      break;
      }

    if( ifound >= 0 && all_flags[ifound] == ascii_1 )
      {
      // Arriving here means we found a new delimiter.
      // If the ALL flag was on, set up to notice repeats
      previous_delimiter = ifound;
      }

    if( !leftmost_delimiter )
      {
      // We were unable to find a delimiter, so we eat up the remainder
      // of the sender:
      leftmost_delimiter = right;
      }

    // Apply what we have learned to the next receiver:

    size_t examined = leftmost_delimiter - left;

    // Move the data into place:
    move_string(id4[nreceiver], id4_o[nreceiver], id4_s[nreceiver], left, examined);

    // Update the left pointer
    left = leftmost_delimiter;
    if( ifound >= 0 )
      {
      // And skip over the delimiter
      left += id2_s[ifound];
      }

    if( id5[nreceiver] )
      {
      if( ifound >= 0 )
        {
        if( figconst )
          {
          move_string(id5[nreceiver], id5_o[nreceiver], id5_s[nreceiver],
                      achfigconst,
                      1);
          }
        else
          {
          move_string(id5[nreceiver], id5_o[nreceiver], id5_s[nreceiver],
                      (char *)(id2[ifound]->data+id2_o[ifound]),
                      id2_s[ifound]);
          }
        }
      else
        {
        move_string(id5[nreceiver], id5_o[nreceiver], id5_s[nreceiver], "");
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
    pointer += examined + id2_s[ifound];
    tally += 1;
    nreceiver += 1;
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
  ec_type_t retval;
  int status10 = (int)status / 10;
  if( !(status10 < 10 && status10 >= 0) )
    {
    __gg__abort("local_ec_type_of(): status10 out of range");
    }
  switch(status10)
    {
    case 0:
      // This actually should be ec_io_warning_e, but that's new for ISO 1989:2013
      retval = ec_none_e;
      break;
    case 1:
      retval = ec_io_at_end_e;
      break;
    case 2:
      retval = ec_io_invalid_key_e;
      break;
    case 3:
      retval = ec_io_permanent_error_e;
      break;
    case 4:
      retval = ec_io_logic_error_e;
      break;
    case 5:
      retval = ec_io_record_operation_e;
      break;
    case 6:
      retval = ec_io_file_sharing_e;
      break;
    case 7:
      retval = ec_io_record_content_e;
      break;
    case 9:
      retval = ec_io_imp_e;
      break;

    default:
      retval = ec_none_e;
      break;
    }
  return retval;
  }

bool
cbl_enabled_exceptions_array_t::match( ec_type_t ec, size_t file ) const {
  auto output = enabled_exception_match( ecs, ecs + nec, ec, file );
  return output < ecs + nec? output->enabled : false;
}

static cbl_enabled_exceptions_array_t enabled_ECs;

/*
 * Store and report the enabled exceptions.
 * 7.3.20.3 General rules:
 *  1) The default TURN directive is '>>TURN EC-ALL CHECKING OFF'.
 */
struct exception_descr_t {
  bool location;
  std::set<size_t> files;
};

/*
 * Compare the raised exception, cbl_exception_t, to the USE critera
 * of a declarative, cbl_declarative_t.  Return FALSE if the exception
 * raised was already handled by the statement that provoked the
 * exception, as indicated by the "handled" file status.
 *
 * This copes with I/O exceptions: ec_io_e and friends.
 */

class match_file_declarative {
  const cbl_exception_t& oops;
  const ec_type_t handled_type;
 protected:
  bool handled() const {
    return oops.type == handled_type || oops.type == ec_none_e;
  }
 public:
  match_file_declarative( const cbl_exception_t& oops, file_status_t handled )
    : oops(oops), handled_type( local_ec_type_of(handled) )
  {}

  bool operator()( const cbl_declarative_t& dcl ) {

    // Declarative is for the raised exception and not handled by the statement.
    if( handled() ) return false;
    bool matches = enabled_ECs.match(dcl.type);

    // I/O declaratives match by file or mode, not EC.
    if( dcl.is_format_1() ) { // declarative is for particular files or mode
      if( dcl.nfile > 0 ) {
        matches = dcl.match_file(oops.file);
      } else {
        matches = oops.mode == dcl.mode;
      }
    }

    return matches;
  }
};

cblc_file_t * __gg__file_stashed();
static ec_type_t ec_raised_and_handled;

static void
default_exception_handler( ec_type_t ec)
{
  if( ec != ec_none_e ) {
    auto p = std::find_if( __gg__exception_table, __gg__exception_table_end,
                           [ec](const ec_descr_t& descr) {
                             return descr.type == ec;
                           } );
    if( p == __gg__exception_table_end ) {
      err(EXIT_FAILURE,
          "logic error: %s:%zu: %s unknown exception %x",
           ec_status.source_file,
           ec_status.lineno,
           ec_status.statement,
           ec );
    }

    const char *disposition = NULL;

    switch( p->disposition ) {
    case ec_category_fatal_e:
      warnx("fatal exception at %s:%zu:%s %s (%s)",
            ec_status.source_file,
            ec_status.lineno,
            ec_status.statement,
            p->name,
            p->description );
      abort();
      break;
    case ec_category_none_e:
      disposition = "category none?";
      break;
    case ec_category_nonfatal_e:
      disposition = "nonfatal";
      break;
    case ec_category_implementor_e:
      disposition = "implementor";
      break;
    case uc_category_none_e:
      disposition = "uc_category_none_e";
      break;
    case uc_category_fatal_e:
      disposition = "uc_category_fatal_e";
      break;
    case uc_category_nonfatal_e:
      disposition = "uc_category_nonfatal_e";
      break;
    case uc_category_implementor_e:
      disposition = "uc_category_implementor_e";
      break;
    }

    // If the EC was handled by a declarative, keep mum.
    if( ec == ec_raised_and_handled ) {
      ec_raised_and_handled = ec_none_e;
      return;
    }

    warnx("%s exception at %s:%zu:%s %s (%s)",
          disposition,
          ec_status.source_file,
          ec_status.lineno,
          ec_status.statement,
          p->name,
          p->description );
  }
}

extern "C"
void
__gg__check_fatal_exception()
{
  if( ec_raised_and_handled == ec_none_e ) return;
  /*
   * "... if checking for EC-I-O exception conditions is not enabled,
   * there is no link between EC-I-O exception conditions and I-O
   * status values."
   */
  if( ec_cmp(ec_raised_and_handled, ec_io_e) ) return;

  default_exception_handler(ec_raised_and_handled);
  ec_raised_and_handled = ec_none_e;
}

extern "C"
void
__gg__clear_exception()
{
  ec_raised_and_handled = ec_none_e;
}


cbl_enabled_exceptions_array_t&
cbl_enabled_exceptions_array_t::operator=( const cbl_enabled_exceptions_array_t& input )
{
  if( nec == input.nec ) {
    if( nec == 0 || 0 == memcmp(ecs, input.ecs, nbytes()) ) return *this;
  }

  if( nec < input.nec ) {
    if( nec > 0 ) delete[] ecs;
    ecs = new cbl_enabled_exception_t[1 + input.nec];
  }
  if( input.nec > 0 ) {
    auto pend = std::copy( input.ecs, input.ecs + input.nec, ecs );
    std::fill(pend, ecs + input.nec, cbl_enabled_exception_t());
  }
  nec = input.nec;
  return *this;
}

// Update the list of compiler-maintained enabled exceptions.
extern "C"
void
__gg__stash_exceptions( size_t nec, cbl_enabled_exception_t *ecs )
{
  enabled_ECs = cbl_enabled_exceptions_array_t(nec, ecs);

  if( false && getenv("match_declarative") )
    warnx("%s: %zu exceptions enabled", __func__, nec);
}


/*
 * Match the raised exception against a declarative handler
 *
 * ECs unrelated to I/O are not matched to a Declarative unless
 * enabled.  Declaratives for I/O errors, on the other hand, match
 * regardless of whether or not any EC is enabled.
 *
 * Declaratives handle I-O errors with USE Format 1. They don't name a
 * specific EC.  They're matched based on the file's status,
 * irrespective of whether or not EC-I-O is enabled.  If EC-I-O is
 * enabled, and mentioned in a Declarative USE statement, then it is
 * matched just like any other Format 3 USE statement.
 */
extern "C"
void
__gg__match_exception( cblc_field_t *index,
                       const cbl_declarative_t *dcls )
{
  static const cbl_declarative_t no_declaratives[1] = {};

  size_t ifile = __gg__exception_file_number;
  // The exception file number is assumed to always be zero, unless it's
  // been set to a non-zero value.  Having picked up that value it is our job
  // to immediately set it back to zero:
  __gg__exception_file_number = 0;

  int  handled = __gg__exception_handled;
  cblc_file_t *stashed = __gg__file_stashed();

  if( dcls == NULL ) dcls = no_declaratives;
  size_t ndcl = dcls[0].section;
  auto eodcls  = dcls + 1 + ndcl, p = eodcls;

  auto ec = ec_status.update().unhandled();

  // We need to set exception handled back to 0.  We do it here because
  // ec_status.update() looks at it
  __gg__exception_handled = 0;

  if(__gg__exception_code != ec_none_e) // cleared by ec_status_t::update
    {
    __gg__abort("__gg__match_exception(): __gg__exception_code should be ec_none_e");
    }
  if( ec == ec_none_e ) {
    if( ifile == 0) goto set_exception_section;

    if( stashed == nullptr )
      {
      __gg__abort("__gg__match_exception(): stashed is null");
      }
    ec = local_ec_type_of( stashed->io_status );
  }

  if( ifile > 0 ) { // an I/O exception is raised
    if( stashed == nullptr )
      {
      __gg__abort("__gg__match_exception(): stashed is null (2)");
      }
    auto mode = cbl_file_mode_t(stashed->mode_char);
    cbl_exception_t oops = {0, ifile, ec, mode };
    p = std::find_if( dcls + 1, eodcls,
                      match_file_declarative(oops, file_status_t(handled)) );

  } else {  // non-I/O exception
    auto enabled = enabled_ECs.match(ec);
    if( enabled ) {
      p = std::find_if( dcls + 1, eodcls, [ec] (const cbl_declarative_t& dcl) {
                          if( ! enabled_ECs.match(dcl.type) ) return false;
                          if( ! ec_cmp(ec, dcl.type) ) return false;
                          return true;
                        } );
      if( p == eodcls ) {
        default_exception_handler(ec);
      }
    } else { // not enabled
    }
  }

 set_exception_section:
  size_t retval = p == eodcls? 0 : p->section;
  ec_raised_and_handled = retval? ec : ec_none_e;

  // If a declarative matches the raised exception, return its
  // symbol_table index.
  __gg__int128_to_field(index,
                        (__int128)retval,
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
__gg__float128_from_location(cblc_field_t *var, unsigned char *location)
  {
  GCOB_FP128 retval = 0;
  switch( var->capacity )
    {
    case 4:
      {
      retval = *(_Float32 *)location;
      break;
      }

    case 8:
      {
      retval = *(_Float64 *)location;
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
__gg__integer_from_float128(cblc_field_t *field)
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
      fprintf(stderr, "libgcobol.cc:__gg__adjust_dest_size(): Adjusting size upward is not possible.\n");
      abort();
//      dest->allocated = ncount;
//      dest->data = (unsigned char *)realloc(dest->data, ncount);
      }
    dest->capacity = ncount;
    }
  }

extern "C"
void
__gg__func_exception_location(cblc_field_t *dest)
  {
  char ach[512] = " ";
  if( stashed_exception_code )
    {
    ach[0] = '\0';
    if( stashed_exception_program_id )
      {
      strcat(ach, stashed_exception_program_id);
      strcat(ach, "; ");
      }

    if( stashed_exception_paragraph )
      {
      strcat(ach, stashed_exception_paragraph );
      if( stashed_exception_section )
        {
        strcat(ach, " OF ");
        strcat(ach, stashed_exception_section);
        }
      }
    else
      {
      if( stashed_exception_section )
        {
        strcat(ach, stashed_exception_section);
        }
      }
    strcat(ach, "; ");

    if( stashed_exception_source_file )
      {
      char achSource[128] = "";
      snprintf( achSource,
                sizeof(achSource),
                "%s:%d ",
                stashed_exception_source_file,
                stashed_exception_line_number);
      strcat(ach, achSource);
      }
    else
      {
      strcpy(ach, " ");
      }
    }
  __gg__adjust_dest_size(dest, strlen(ach));
  memcpy(dest->data, ach, strlen(ach));
  }

extern "C"
void
__gg__func_exception_statement(cblc_field_t *dest)
  {
  char ach[128] = " ";
  if(stashed_exception_statement)
    {
    snprintf(ach, sizeof(ach), "%s", stashed_exception_statement);
    ach[sizeof(ach)-1] = '\0';
    }
  __gg__adjust_dest_size(dest, strlen(ach));
  memcpy(dest->data, ach, strlen(ach));
  }

extern "C"
void
__gg__func_exception_status(cblc_field_t *dest)
  {
  char ach[128] = "<not in table?>";
  if(stashed_exception_code)
    {
    ec_descr_t *p = __gg__exception_table;
    while(p < __gg__exception_table_end )
      {
      if( p->type == (ec_type_t)stashed_exception_code )
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
  }

static cblc_file_t *recent_file = NULL;

extern "C"
void
__gg__set_exception_file(cblc_file_t *file)
  {
  recent_file = file;
  ec_type_t ec = local_ec_type_of( file->io_status );
  if( ec )
    {
    exception_raise(ec);
    }
  }


extern "C"
void
__gg__func_exception_file(cblc_field_t *dest, cblc_file_t *file)
  {
  char ach[128];
  if( !file )
    {
    // This is where we process FUNCTION EXCEPTION-FILE <no parameter>
    if( !(stashed_exception_code & ec_io_e) || !recent_file)
      {
      // There is no EC-I-O exception code, so we return two spaces
      strcpy(ach, "00");
      }
    else
      {
      if( sv_from_raise_statement )
        {
        strcpy(ach, "  ");
        }
      else
        {
        snprintf(ach, sizeof(ach), "%2.2d%s", recent_file->io_status, recent_file->name);
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
  }

extern "C"
void
__gg__set_exception_code(ec_type_t ec, int from_raise_statement)
  {
  sv_from_raise_statement = from_raise_statement;

  __gg__exception_code = ec;
  if( ec == ec_none_e)
    {
    stashed_exception_code          = 0    ;
    stashed_exception_handled       = 0    ;
    stashed_exception_file_number   = 0    ;
    stashed_exception_file_status   = 0    ;
    stashed_exception_file_name     = NULL ;
    stashed_exception_program_id    = NULL ;
    stashed_exception_section       = NULL ;
    stashed_exception_paragraph     = NULL ;
    stashed_exception_source_file   = NULL ;
    stashed_exception_line_number   = 0    ;
    stashed_exception_statement     = NULL ;
    }
  else
    {
    stashed_exception_code          = __gg__exception_code         ;
    stashed_exception_handled       = __gg__exception_handled      ;
    stashed_exception_file_number   = __gg__exception_file_number  ;
    stashed_exception_file_status   = __gg__exception_file_status  ;
    stashed_exception_file_name     = __gg__exception_file_name    ;
    stashed_exception_program_id    = __gg__exception_program_id   ;
    stashed_exception_section       = __gg__exception_section      ;
    stashed_exception_paragraph     = __gg__exception_paragraph    ;
    stashed_exception_source_file   = __gg__exception_source_file  ;
    stashed_exception_line_number   = __gg__exception_line_number  ;
    stashed_exception_statement     = __gg__exception_statement    ;
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
__gg__is_float_infinite(cblc_field_t *source, size_t offset)
  {
  int retval = 0;
  switch(source->capacity)
    {
    case 4:
      retval = fpclassify(  *(_Float32*)(source->data+offset)) == FP_INFINITE;
      break;
    case 8:
      retval = fpclassify(  *(_Float64*)(source->data+offset)) == FP_INFINITE;
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
__gg__float32_from_128( cblc_field_t *dest,
                        size_t        dest_offset,
                        cblc_field_t *source,
                        size_t        source_offset)
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
    *(_Float32 *)(dest->data+dest_offset) = (_Float32)value;
    }
  return retval;
  }

extern "C"
int
__gg__float32_from_64(  cblc_field_t *dest,
                        size_t        dest_offset,
                        cblc_field_t *source,
                        size_t        source_offset)
  {
  int retval = 0;
  _Float64 value = *(_Float64*)(source->data+source_offset);
  if( FP128_FUNC(fabs)(value) > GCOB_FP128_LITERAL (3.4028235E38) )
    {
    retval = 1;
    }
  else
    {
    *(_Float32 *)(dest->data+dest_offset) = (_Float32)value;
    }
  return retval;
  }

extern "C"
int
__gg__float64_from_128( cblc_field_t *dest,
                        size_t        dest_offset,
                        cblc_field_t *source,
                        size_t        source_offset)
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
    *(_Float64 *)(dest->data+dest_offset) = (_Float64)value;
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
__gg__copy_as_big_endian(unsigned char *dest, unsigned char *source)
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
  *__gg__data_space       = internal_space;
  *__gg__data_low_values  = ascii_to_internal(__gg__low_value_character);
  *__gg__data_zeros       = internal_0;
  *__gg__data_high_values = ascii_to_internal(__gg__high_value_character);
  *__gg__data_quotes      = ascii_to_internal(__gg__quote_character);;
  }

extern "C"
unsigned char *
__gg__get_figconst_data(cblc_field_t *field)
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
          dirent *entry = readdir(dir);
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
  void *retval = NULL;

  // We search for a function.  We check first for the unmangled name, and then
  // the mangled name.  We do this first for the executable, then for .so
  // files in COBPATH, and then for files in LD_LIBRARY_PATH

  static void *handle_executable = NULL;
  if( !handle_executable )
    {
    handle_executable = dlopen(NULL, RTLD_LAZY);
    }
  if( !retval )
    {
    retval = dlsym(handle_executable, unmangled_name);
    }
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
__gg__just_mangle_name( cblc_field_t *field,
                        char        **mangled_name
                        )
  {
  static char ach_name[1024];
  static char ach_unmangled[1024];
  static char ach_mangled[1024];

  size_t         length;
  length = field->capacity;
  memcpy(ach_name, field->data, length);
  ach_name[length] = '\0';

  if( internal_is_ebcdic)
    {
    // The name is in EBCDIC
    __gg__ebcdic_to_ascii(ach_name, length);
    }

  bool is_pointer = false;

  if( (field && field->type == FldPointer) )
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
__gg__function_handle_from_literal(int   program_id,
                                   char *literal)
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
    retval = (void *)pointers[function_index];
    }
  else
    {
    retval = __gg__function_handle_from_cobpath(ach_unmangled, ach_mangled);
    }

  return retval;
  }

extern "C"
void *
__gg__function_handle_from_name(int           program_id,
                                cblc_field_t *field,
                                size_t        offset,
                                size_t        length )
  {
  void *retval = NULL;
  static char ach_name[1024];
  static char ach_unmangled[1024];
  static char ach_mangled[1024];

  if( length == 0 )
    {
    length = field->capacity;
    }

  memcpy(ach_name, field->data + offset, length);

  if( internal_is_ebcdic)
    {
    // The name is in EBCDIC
    __gg__ebcdic_to_ascii(ach_name, length);
    }

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
    retval = (void *)pointers[function_index];
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
                    size_t        *spans,
                    size_t         table,
                    size_t         ntbl,
                    size_t        *tbls)
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

  cblc_field_t *parent = src;
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

        unsigned char *subtable_source = source + subtable_offset;

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
    void *ptr = (void *)get_binary_value_local(&rdigits,
                                               target,
                                               target->data + offset,
                                               sizeof(void *));
    if( ptr )
      {
      free(ptr);
      // And set the data location to zero
      *(char **)(target->data + offset) = NULL;
      }
    }
  }

static int
get_the_byte(cblc_field_t *field)
  {
  int retval = -1;
  if( field )
    {
    cbl_figconst_t figconst  = (cbl_figconst_t)(field->attr  & FIGCONST_MASK);
    switch(figconst)
      {
      case null_value_e:
        retval = 0;
        break;
      case low_value_e:
        retval = ascii_to_internal(__gg__low_value_character);
        break;
      case zero_value_e:
        retval = internal_zero;
        break;
      case space_value_e:
        retval = internal_space;
        break;
      case quote_value_e:
        retval = ascii_to_internal(__gg__quote_character);
        break;
      case high_value_e:
        retval = ascii_to_internal(__gg__high_value_character) & 0xFF;
        break;
      case normal_value_e:
        retval = (int)__gg__get_integer_binary_value(field);
        break;
      }
    }
  return retval;
  }

extern "C"
void
__gg__allocate( cblc_field_t *first,
                size_t        first_offset,
                int           initialized,
                int           default_byte,
                cblc_field_t *f_working_byte,
                cblc_field_t *f_local_byte,
                cblc_field_t *returning,
                size_t        returning_offset)
  {
  int working_byte = get_the_byte(f_working_byte);
  int local_byte   = get_the_byte(f_local_byte);

  unsigned char *retval = NULL;
  if( first->attr & based_e )
    {
    // first is the BASED variable we are allocating memory for
    if( first->capacity )
      {
      retval = (unsigned char *)malloc(first->capacity);

      if( initialized )
        {
        // This is ISO 2023 ALLOCATE rule 7 (ALL TO VALUE)
        int fill_char = 0;
        if( default_byte >= 0 )
          {
          fill_char = default_byte;
          memset(retval, fill_char, first->capacity);
          }
        }
      else
        {
        // This is ISO 2023 ALLOCATE rule 9 (pointers NULL, otherwise OPT_INIT)
        int fill_char = 0;
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
      retval = (unsigned char *)malloc(tsize);

      int fill_char = 0;
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
    *(unsigned char **)(returning->data + returning_offset) = retval;
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
  static char *result = (char *)malloc(result_size);

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
            result = (char *)realloc(result, result_size);
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
  }

