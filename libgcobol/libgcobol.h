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
#ifndef LIBGCOBOL_H_
#define LIBGCOBOL_H_

#include <stdio.h>

#include <map>
#include <vector>

#define MIN_FIELD_BLOCK_SIZE (16)

// RUNTIME structures *must* match the ones created in structs.c and initialized
// and used in genapi.c.  It's actually not all that important to emphasize that
// fact, since the compiled executable will crash and burn quickly if they don't
// match precisely.

// Note that it must match the same structure in the GDB-COBOL debugger

#define A_ZILLION (1000000) // Absurdly large number for __gg__call_parameter_count

// These bits are used for the "call flags" of arithmetic operations
#define ON_SIZE_ERROR 0x01
#define REMAINDER_PRESENT 0x02

/*  'offset' is overloaded for FldAlphanumeric/temporary/intermediate variables
 *  For such variables, offset is a copy of the initial capacity.  This is in
 *  support of the FUNCTION TRIM function, which both needs to be able to
 *  reduce the capacity of the target variable, and then to reset it back to
 *  the original value
 */

enum substitute_flags_t
  {
  substitute_anycase_e  = 1,
  substitute_first_e    = 2,  // first and last are mutually exclusive
  substitute_last_e     = 4,
  };

enum cblc_file_flags_t
    {
    file_flag_optional_e      = 0x00001,
    file_flag_existed_e       = 0x00002,
    file_name_quoted_e        = 0x00004,
    file_flag_initialized_e   = 0x00008,
    };

// For indexed files, there can be one or more indexes, one per key.
// Each index is one or more fields.

struct file_hole_t
  {
  long location;
  size_t size;
  };

struct file_index_t
    {
    std::multimap<std::vector<unsigned char>, long> key_to_position;
    std::multimap<std::vector<unsigned char>, long>::iterator current_iterator;
    std::multimap<std::vector<unsigned char>, long>::iterator ending_iterator;
    };

class supplemental_t
  {
  public:
    std::vector<file_hole_t>  holes;
    std::vector<file_index_t> indexes;
    std::vector<int>          uniques;
  };

struct cblc_subscript_t
    {
    cblc_field_t *field;    // That's what it usually is:
    unsigned int type;      // When type is FldLiteralN, field is a pointer to __int128
    };

#define REFER_T_ALL_FLAGS_MASK 0x0FF  // We allow for seven subscripts
#define REFER_T_MOVE_ALL       0x100  // This is the move_all flag
#define REFER_T_ADDRESS_OF     0x200  // This is the address_of flag

struct cblc_declarative_t
    {
    int format;
    int culprit;  //declarative_culprit_t
    int nfiles;
    };

/*  According to the standard, the first digit of the file operation status
    register is interpreted like this:

    EC-I-O-AT-END               '1'
    EC-I-O-INVALID-KEY          '2'
    EC-I-O-PERMANENT-ERROR      '3'
    EC-I-O-LOGIC-ERROR          '4'
    EC-I-O-RECORD-OPERATION     '5'
    EC-I-O-FILE-SHARING         '6'
    EC-I-O-IMP                  '9'

When the tens digit is '0', there are a number of conditions for
successful completion.  See section 9.1.12.1

    00      unqualified success
    02      duplicate key detected
    04      the data read were either too short or too long
    05      the operator couldn't find the tape
    07      somebody tried to rewind the card reader.

For now, I am going to treat the io_status as an integer 00 through 99.  I
anticipate mostly returning
    00 for ordinary success,
    04 for a mismatched record size
    10 for an end-of-file

*/

// This global variable is constantly being updated with the yylineno.  This is
// useful for creating error messages, and for handling EXCEPTION_CONDITIONS
extern int         __gg__exception_code;
extern int         __gg__exception_line_number;
extern int         __gg__exception_file_status;
extern const char *__gg__exception_file_name;
extern const char *__gg__exception_statement;
extern const char *__gg__exception_source_file;
extern const char *__gg__exception_program_id;
extern const char *__gg__exception_section;
extern const char *__gg__exception_paragraph;

extern "C" void __gg__set_exception_code( ec_type_t ec,
                                          int from_raise_statement=0);

extern int *        __gg__fourplet_flags;

extern cblc_field_t ** __gg__treeplet_1f;
extern size_t       *  __gg__treeplet_1o;
extern size_t       *  __gg__treeplet_1s;
extern cblc_field_t ** __gg__treeplet_2f;
extern size_t       *  __gg__treeplet_2o;
extern size_t       *  __gg__treeplet_2s;
extern cblc_field_t ** __gg__treeplet_3f;
extern size_t       *  __gg__treeplet_3o;
extern size_t       *  __gg__treeplet_3s;
extern cblc_field_t ** __gg__treeplet_4f;
extern size_t       *  __gg__treeplet_4o;
extern size_t       *  __gg__treeplet_4s;

#if 1
  static inline
  void exception_raise(ec_type_t ec_code) { __gg__set_exception_code(ec_code); }
#else
# define exception_raise(ec_code)do{__gg__set_exception_code(ec_code);}while(0);
#endif

extern "C" __int128 __gg__power_of_ten(int n);

extern "C" __int128 __gg__dirty_to_binary_source( const char *dirty,
                                                  int length,
                                                  int *rdigits);
extern "C" __int128 __gg__dirty_to_binary_internal( const char *dirty,
                                                    int length,
                                                    int *rdigits);
extern "C" __int128 __gg__binary_value_from_field(  int *rdigits,
                                                    cblc_field_t *var);
extern "C" int __gg__compare_2( cblc_field_t *left_side,
                                unsigned char   *left_location,
                                size_t  left_length,
                                int     left_attr,
                                bool    left_all,
                                bool    left_address_of,
                                cblc_field_t *right_side,
                                unsigned char   *right_location,
                                size_t  right_length,
                                int     right_attr,
                                bool    right_all,
                                bool    right_address_of,
                                int     second_time_through);
extern "C" void __gg__int128_to_field(cblc_field_t   *tgt,
                                      __int128        value,
                                      int             source_rdigits,
                                      enum cbl_round_t  rounded,
                                      int            *compute_error);
extern "C" void __gg__float128_to_field(cblc_field_t   *tgt,
                                        _Float128       value,
                                        enum cbl_round_t  rounded,
                                      int            *compute_error);
extern "C" void __gg__int128_to_qualified_field(cblc_field_t   *tgt,
                                size_t          offset,
                                size_t          length,
                                __int128        value,
                                int             source_rdigits,
                                enum cbl_round_t  rounded,
                                int            *compute_error);
extern "C" void __gg__float128_to_qualified_field(cblc_field_t   *tgt,
                                  size_t          tgt_offset,
                                  _Float128       value,
                                  enum cbl_round_t  rounded,
                                  int            *compute_error);

extern "C" void __gg__double_to_target( cblc_field_t *tgt,
                                        double tgt_value,
                                        cbl_round_t rounded);
extern "C" char __gg__get_decimal_separator();
extern "C" char __gg__get_decimal_point();
extern "C" char * __gg__get_default_currency_string();

extern "C" void __gg__clock_gettime(clockid_t clk_id, struct timespec *tp);
extern "C" _Float128 __gg__float128_from_location(cblc_field_t *var,
                                                  unsigned char *location);
extern "C" void __gg__adjust_dest_size(cblc_field_t *dest, size_t ncount);
#define MINIMUM_ALLOCATION_SIZE 16
extern "C" void __gg__realloc_if_necessary( char **dest,
                                            size_t *dest_size,
                                            size_t new_size);
extern "C" void __gg__set_exception_file(cblc_file_t *file);
extern "C" void __gg__internal_to_console_in_place(char *loc, size_t length);
extern "C" __int128 __gg__binary_value_from_qualified_field(int        *rdigits,
                                                            cblc_field_t *var,
                                                            size_t     offset,
                                                            size_t     size);
extern "C"  _Float128 __gg__float128_from_qualified_field(cblc_field_t *field,
                                                          size_t offset,
                                                          size_t size);
extern "C"  __int128 __gg__integer_from_qualified_field(cblc_field_t *var,
                                                        size_t var_offset,
                                                        size_t var_size);
void __gg__abort(const char *msg);


#endif
