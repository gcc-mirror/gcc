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
#ifndef COMMON_DEFS_H_
#define COMMON_DEFS_H_

#include <stdint.h>
#include <list>

#define COUNT_OF(X) (sizeof(X) / sizeof(X[0]))

// This constant establishes the maximum number of digits in a fixed point
// number.  We are using 37 digits as a maximum because a full-size 37-digit
// number (10**37) takes 123 bits, and a full-size 38-digit number (10**38)
// takes 127 bits.  By using a maximum of 37, that gives us an additional digit
// of headroom in order to accomplish rounding.

// You should keep in mind that the 128-bit binary floating point numbers that
// we use can reliably reproduce numbers of 33 decimal digits when going to
// binary and back.

#define MAX_FIXED_POINT_DIGITS (37)

// COBOL tables can have up to seven subscripts
#define MAXIMUM_TABLE_DIMENSIONS 7

// This bit gets turned on in the first or last byte (depending on the leading_e attribute
// phrase) of a NumericDisplay to indicate that the value is negative.

// When running the EBCDIC character set, the meaning of this bit is flipped,
// because an EBCDIC zero is 0xF0, while ASCII is 0x30
#define NUMERIC_DISPLAY_SIGN_BIT 0x40

#define LEVEL01 (1)
#define LEVEL49 (49)
#define LEVEL77 (77)

// In the __gg__move_literala() call, we piggyback this bit onto the
// cbl_round_t parameter, just to cut down on the number of parameters passed

#define REFER_ALL_BIT 0x80

// Other bits for handling MOVE ALL and so on.
#define REFER_T_ALL_FLAGS_MASK 0x0FF  // We allow for seven subscripts
#define REFER_T_MOVE_ALL       0x100  // This is the move_all flag
#define REFER_T_ADDRESS_OF     0x200  // This is the address_of flag
#define REFER_T_REFMOD         0x400  // Indicates to library the refer was a refmod

#define MIN_FIELD_BLOCK_SIZE (16)

#define A_ZILLION (1000000) // Absurdly large number for __gg__call_parameter_count

// These bits are used for the "call flags" of arithmetic operations
#define ON_SIZE_ERROR 0x01
#define REMAINDER_PRESENT 0x02

#define MINIMUM_ALLOCATION_SIZE 16

/*
 * User-defined names in IBM COBOL can have at most 30 characters.
 * For DBCS, the maximum is 14.
 *
 * Per ISO/IEC 1989:2023(E), 8.3.2 COBOL words,
 * "A COBOL word is a character-string of not more than 63 characters"
 */
typedef char cbl_name_t[64];

// Note that the field_type enum is duplicated in the source code for the
// COBOL-aware GDB, and so any changes here (or there) have to be reflected
// there (or here)

// Note further that if this list changes, then the valid_move() matrix has to
// change as will.  Currently that matrix is in util.cc.

enum cbl_field_type_t {
  FldInvalid,           // uninitialized
  FldGroup,
  FldAlphanumeric,      // For X(n).
  FldNumericBinary,     // For 999v9 comp       big-endian, 1 to 16 bytes
  FldFloat,             // 4-, 8-, and 16-byte floating point.  See ieeedec_e and big_endian_e flags
  FldPacked,            // For 999v9 comp-3     internal decimal, packed decimal representation;
  FldNumericBin5,       // For 999v9 comp-5     little-endian, 1 to 16 bytes. (Native binary)
  FldNumericDisplay,    // For 999v9            one decimal character per byte
  FldNumericEdited,     // For 999.9            PIC BPVZ90/,.+-CRDB*cs; must have one of  B/Z0,.*+-CRDBcs
  FldAlphaEdited,       //                      PIC AX9B0/; must have at least one A or X, and at least one B0/
  FldLiteralA,          // Alphanumeric literal
  FldLiteralN,          // Numeric literal
  FldClass,
  FldConditional,       // Target for parser_relop()
  FldForward,
  FldIndex,
  FldSwitch,
  FldDisplay,
  FldPointer,
  FldBlob,
};


/*  BINARY, COMP, COMPUTATIONAL, COMP-4, COMPUTATIONAL-4 are the same:
 *      Storage, by default, is big-endian.
 *      PIC 9(1 to 4)   is  2 bytes
 *      PIC 9(5 to 9)   is  4 bytes
 *      PIC 9(10 to 18) is  8 bytes
 *      PIC 9(19-37)    is 16 bytes
 *  COMP-1, COMPUTATIONAL-1
 *      4-byte floating point (single)
 *  COMP-2, COMPUTATIONAL-2
 *      8-byte floating point (double)
 *  PACKED-DECIMAL, COMP-3, COMPUTATIONAL-3
 *      Packed decimal. Final nybble is 0xF for unsigned numbers.  For signable
 *                      values, it is 0xD for negative, and 0xC for non-negative
 *  COMP-5, COMPUTATIONAL-5
 *      Native binary.  The maximum number of digits is implied by
 *      the 2, 4, or 8 bytes of data storage.  By "native", little-endian
 *      is implied on Intel processors.
 */

/*
 * Enumerated bit mask of variable attributes.
 * A field as either left- or right-justified.
 * A field is padded (in the unjustified direction) either with 0 or SPC.
 *   (But maybe the fill character should just be an explicit character.)
 */
enum cbl_field_attr_t : size_t {
  none_e            = 0x0000000000,
  figconst_1_e      = 0x0000000001, // This needs to be 1 - don't change the position
  figconst_2_e      = 0x0000000002, // This needs to be 2
  figconst_4_e      = 0x0000000004, // This needs to be 4
  rjust_e           = 0x0000000008, // justify right
  ljust_e           = 0x0000000010, // justify left
  zeros_e           = 0x0000000020, // zero fill
  signable_e        = 0x0000000040,
  constant_e        = 0x0000000080, // pre-assigned constant
  function_e        = 0x0000000100,
  quoted_e          = 0x0000000200,
  filler_e          = 0x0000000400,
  _spare_e          = 0x0000000800, //
  intermediate_e    = 0x0000001000, // Compiler-defined temporary variable
  embiggened_e      = 0x0000002000, // redefined numeric made 64-bit by USAGE POINTER
  all_alpha_e       = 0x0000004000, // FldAlphanumeric, but all A's
  all_x_e           = 0x0000008000, // picture is all X's
  all_ax_e          = 0x000000a000, // picture is all A's or all X's
  prog_ptr_e        = 0x0000010000, // FUNCTION-POINTER or PROGRAM-POINTER
  scaled_e          = 0x0000020000,
  refmod_e          = 0x0000040000, // Runtime; indicates a refmod is active
  based_e           = 0x0000080000, // pointer capacity, for ADDRESS OF or ALLOCATE
  any_length_e      = 0x0000100000, // inferred length of linkage in nested program
  global_e          = 0x0000200000, // field has global scope
  external_e        = 0x0000400000, // field has external scope
  blank_zero_e      = 0x0000800000, // BLANK WHEN ZERO
  // data division uses 2 low bits of high byte
  linkage_e         = 0x0001000000, // field is in linkage section
  local_e           = 0x0002000000, // field is in local section
  leading_e         = 0x0004000000, // leading sign (signable_e alone means trailing)
  separate_e        = 0x0008000000, // separate sign
  envar_e           = 0x0010000000, // names an environment variable
   dnu_1_e          = 0x0020000000, // unused: this attribute bit is available
  bool_encoded_e    = 0x0040000000, // data.initial is a boolean string
  hex_encoded_e     = 0x0080000000, // data.initial is a hex-encoded string
  depends_on_e      = 0x0100000000, // A group hierachy contains a DEPENDING_ON
  initialized_e     = 0x0200000000, // Don't call parser_initialize from parser_symbol_add
  has_value_e       = 0x0400000000, // Flag to hierarchical descendents to ignore .initial
  ieeedec_e         = 0x0800000000, // Indicates a FldFloat is IEEE 754 decimal, rather than binary
  big_endian_e      = 0x1000000000, // Indicates a value is big-endian
  same_as_e         = 0x2000000000, // Field produced by SAME AS (cannot take new members)
  record_key_e      = 0x4000000000,
  typedef_e         = 0x8000000000, // IS TYPEDEF
  strongdef_e       = typedef_e + intermediate_e, // STRONG TYPEDEF (not temporary)
};
// The separate_e value does double-duty for FldPacked/COMP-6, which is not
// the same as FldPacked COMP-3.  A COMP-3 can have signable_e, meaning that the
// final nybble is 0x0D for negative, and 0x0C for non-negative.  When a COMP-3
// has no sign, then the final nybble is 0x0F. The packed_no_sign_e bit means
// that there is no sign nybble.
#define packed_no_sign_e separate_e

enum cbl_figconst_t
    {
    normal_value_e = 0, // This one must be zero
    low_value_e    = 1, // The order is important, because
    null_value_e   = 2,
    zero_value_e   = 3, // at times we compare, for example, low_value_e to
    space_value_e  = 4,
    quote_value_e  = 5, //
    high_value_e   = 6, // high_value_e to determine that low is less than high
    };
#define FIGCONST_MASK (figconst_1_e|figconst_2_e|figconst_4_e)
#define DATASECT_MASK (linkage_e | local_e)


enum cbl_file_org_t {
  file_disorganized_e,
  file_sequential_e,
  file_line_sequential_e,
  file_indexed_e,
  file_relative_e,
};

enum cbl_file_access_t {
  file_inaccessible_e,
  file_access_seq_e,
  file_access_rnd_e,
  file_access_dyn_e,
};

enum cbl_file_mode_t {
  file_mode_none_e,
  file_mode_input_e  = 'r',
  file_mode_output_e = 'w',
  file_mode_extend_e = 'a',
  file_mode_io_e     = '+',
};

enum cbl_round_t {
  away_from_zero_e,
  nearest_toward_zero_e,
  toward_greater_e,
  toward_lesser_e,
  nearest_away_from_zero_e,
  nearest_even_e,
  prohibited_e,
  truncation_e,
};

#define RELOP_START 0
enum relop_t {
  lt_op = RELOP_START,
  le_op,
  eq_op,
  ne_op,
  ge_op,
  gt_op,
};

#define LOGOP_START 100
enum logop_t {
  not_op = LOGOP_START,
  and_op,
  or_op,
  xor_op,
  xnor_op,
  true_op,
  false_op,
};

#define SETOP_START 200
enum setop_t {
  is_op = SETOP_START,
};

enum bitop_t {
  bit_set_op,      // set bit on
  bit_clear_op,    // set bit off
  bit_on_op,       // true if bit is on
  bit_off_op,      // true if bit is off
  bit_and_op,
  bit_or_op,
  bit_xor_op,
};

enum file_close_how_t {
  file_close_no_how_e     = 0x00,
  file_close_removal_e    = 0x01,
  file_close_no_rewind_e  = 0x02,
  file_close_with_lock_e  = 0x04,
  file_close_reel_unit_e  = 0x08,
};

enum cbl_compute_error_code_t {
    compute_error_none              = 0x0000,
    compute_error_truncate          = 0x0001,
    compute_error_divide_by_zero    = 0x0002,
    compute_error_exp_zero_by_zero  = 0x0004,
    compute_error_exp_zero_by_minus = 0x0008,
    compute_error_exp_minus_by_frac = 0x0010,
    compute_error_overflow          = 0x0020,
    compute_error_underflow         = 0x0040,
};

enum cbl_arith_format_t {
    not_expected_e,
    no_giving_e, giving_e,
    corresponding_e };

enum cbl_encoding_t {
  ASCII_e,   // STANDARD-1 (in caps to avoid conflict with ascii_e in libgcobol.cc)
  iso646_e,  // STANDARD-2
  EBCDIC_e,  // NATIVE or EBCDIC
  custom_encoding_e,
};

enum cbl_truncation_mode {
    trunc_std_e,
    trunc_opt_e,
    trunc_bin_e,
};

enum cbl_inspect_bound_t {
                bound_characters_e,
                bound_all_e,
                bound_first_e,
                bound_leading_e,
                bound_trailing_e,
};

// a SPECIAL-NAME
enum special_name_t {
  SYSIN_e, SYSIPT_e, SYSOUT_e,
  SYSLIST_e, SYSLST_e,
  SYSPUNCH_e, SYSPCH_e,
  CONSOLE_e,
  C01_e, C02_e, C03_e, C04_e, C05_e, C06_e,
  C07_e, C08_e, C09_e, C10_e, C11_e, C12_e,
  CSP_e,
  S01_e, S02_e, S03_e, S04_e, S05_e,
  AFP_5A_e,
  STDIN_e, STDOUT_e, STDERR_e, SYSERR_e,
  ARG_NUM_e, ARG_VALUE_e, ENV_NAME_e, ENV_VALUE_e,
};

enum classify_t {
  ClassInvalidType,
  ClassNumericType,
  ClassAlphabeticType,
  ClassLowerType,
  ClassUpperType,
  ClassDbcsType,
  ClassKanjiType,
};

static inline const char *
classify_str( enum classify_t classify ) {
  switch(classify) {
  case ClassInvalidType:    return "ClassInvalidType";
  case ClassNumericType:    return "ClassNumericType";
  case ClassAlphabeticType: return "ClassAlphabeticType";
  case ClassLowerType:      return "ClassLowerType";
  case ClassUpperType:      return "ClassUpperType";
  case ClassDbcsType:       return "ClassDbcsType";
  case ClassKanjiType:      return "ClassKanjiType";
  };
  return "(unknown classification)";
}

static inline const char *
cbl_file_mode_str( cbl_file_mode_t mode ) {
  switch(mode) {
  case file_mode_none_e:   return "file_mode_none_e";
  case file_mode_input_e:  return "file_mode_input_e: 'r'";
  case file_mode_output_e: return "file_mode_output_e: 'w'";
  case file_mode_io_e:     return "file_mode_io_e: '+'";
  case file_mode_extend_e: return "file_mode_extend_e: 'a'";
  }
  return "???";
};

enum module_type_t {
  module_activating_e,
  module_current_e,
  module_nested_e,
  module_stack_e,
  module_toplevel_e,
};


static inline bool
ec_cmp( ec_type_t raised, ec_type_t mask )
{
  if( raised == mask ) return true;

  // Do not match on only the low byte.
  if( 0 < (~EC_ALL_E & static_cast<uint32_t>(mask)) ) return false;

  return  0 != ( static_cast<uint32_t>(raised)
                 &
                 static_cast<uint32_t>(mask) );
}

struct cbl_enabled_exception_t {
  bool enabled, location;
  ec_type_t ec;
  size_t file;

  cbl_enabled_exception_t()
    : enabled(false)
    , location(false)
    , ec(ec_none_e)
    , file(0)
  {}

  cbl_enabled_exception_t( bool enabled, bool location,
                           ec_type_t ec, size_t file = 0 )
    : enabled(enabled)
    , location(location)
    , ec(ec)
    , file(file)
  {}

  // sort by  ec and file, not enablement
  bool operator<( const cbl_enabled_exception_t& that ) const {
    if( ec == that.ec ) return file < that.file;
    return ec < that.ec;
  }
  // match on ec and file, not enablement
  bool operator==( const cbl_enabled_exception_t& that ) const {
    return ec == that.ec && file == that.file;
  }
};


class cbl_enabled_exceptions_array_t;

class cbl_enabled_exceptions_t : protected std::set<cbl_enabled_exception_t>
{
  friend cbl_enabled_exceptions_array_t;
  void apply( const cbl_enabled_exception_t& elem ) {
    auto inserted = insert( elem );
    if( ! inserted.second ) {
      erase(inserted.first);
      insert(elem);
    }
  }

 public:
  bool turn_on_off( bool enabled, bool location, ec_type_t type,
                    std::set<size_t> files );

  const cbl_enabled_exception_t * match( ec_type_t type, size_t file = 0 );

  void dump() const;

  void clear() { std::set<cbl_enabled_exception_t>::clear(); }

  bool   empty() const { return std::set<cbl_enabled_exception_t>::empty(); }
  size_t  size() const { return std::set<cbl_enabled_exception_t>::size(); }

  cbl_enabled_exceptions_t& operator=( const cbl_enabled_exceptions_t& ) = default;
};

extern cbl_enabled_exceptions_t enabled_exceptions;

/*
 * This class is passed to the runtime function evaluating the raised exception.
 * It is constructed in genapi.cc from the compile-time table.
 */
struct cbl_enabled_exceptions_array_t {
  size_t nec;
  cbl_enabled_exception_t *ecs;

  cbl_enabled_exceptions_array_t( size_t nec, cbl_enabled_exception_t *ecs )
    : nec(nec), ecs(ecs) {}

  cbl_enabled_exceptions_array_t( const cbl_enabled_exceptions_t& input =
                                  cbl_enabled_exceptions_t() )
    : nec(input.size())
    , ecs(NULL)
  {
    if( ! input.empty() ) {
      ecs = new cbl_enabled_exception_t[nec];
      std::copy(input.begin(), input.end(), ecs);
    }
  }

  cbl_enabled_exceptions_array_t&
  operator=( const cbl_enabled_exceptions_array_t& input);


  bool match( ec_type_t ec, size_t file = 0 ) const;

  size_t nbytes() const { return nec * sizeof(ecs[0]); }
};

template <typename T>
T enabled_exception_match( T beg, T end, ec_type_t type, size_t file ) {
  cbl_enabled_exception_t input( true, true, // don't matter
                                 type, file );
  auto output = std::find(beg, end, input);
  if( output == end ) {
    output = std::find_if( beg, end, // match any file
                           [ec = type]( const cbl_enabled_exception_t& elem ) {
                             return
                               elem.file == 0 &&
                               ec_cmp(ec, elem.ec); } );
  }
  return output;
}

enum substitute_flags_t
  {
  substitute_anycase_e  = 1,
  substitute_first_e    = 2,  // first and last are mutually exclusive
  substitute_last_e     = 4,
  };

#endif
