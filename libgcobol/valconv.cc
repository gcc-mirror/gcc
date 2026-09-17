// This file is included in both the libgcobol and gcc/cobol compilations
/*
 * Copyright (c) 2021-2026 Symas Corporation
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

#include <cctype>
#include <cstdio>
#include <cstring>

#include <algorithm>
#include <unordered_map>
#include <vector>

#if defined(IN_GCC_FRONTEND)
#include "cobol-system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "diagnostic-core.h"
#include "target.h"
#include "tm.h"
#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "../../libgcobol/valconv.h"
#include "../../libgcobol/cobol-endian.h"
#include "../../libgcobol/charmaps.h"
#include "../../libgcobol/exceptl.h"
#else
#include "ec.h"
#include "common-defs.h"
#include "valconv.h"
#include "cobol-endian.h"
#include "charmaps.h"
#include "exceptl.h"
////static char
////TOUPPER(char ch)
////  {
////  if(ch >= 'a' && ch <= 'z' )
////    {
////    return 'A' + ch - 'a';
////    }
////  return ch;
////  }
#endif

std::unordered_map<size_t, alphabet_state> __gg__alphabet_states;

extern "C"
void
__gg__realloc_if_necessary(char **dest, size_t *dest_size, size_t new_size)
  {
  if( new_size > *dest_size )
    {
    // Find the next power of two bigger than us:
    new_size |= new_size>>1;
    new_size |= new_size>>2;
    new_size |= new_size>>4;
    new_size |= new_size>>8;
    new_size |= new_size>>16;
    new_size |= (new_size>>16)>>16;
    *dest_size = new_size + 1;
#if defined(IN_GCC_FRONTEND)
    *dest = static_cast<char *>(xrealloc(*dest, *dest_size));
#else
    *dest = static_cast<char *>(realloc(*dest, *dest_size));
#endif
    }
  }

extern "C"
void
__gg__alphabet_create(  cbl_encoding_t encoding,
                        size_t alphabet_index,
                        const unsigned char *alphabet,
                        int low_char,
                        int high_char )
  {
  assert( encoding == custom_encoding_e );

  std::unordered_map<size_t, alphabet_state>::const_iterator it =
    __gg__alphabet_states.find(alphabet_index);

  if( it == __gg__alphabet_states.end() )
    {
    // This is an alphabet we don't know about.  So just assume the collation
    // is the same as the character ordering:

    alphabet_state new_state;
    new_state.low_char  = low_char;
    new_state.high_char = high_char;

    for(int i=0; i<256; i++)
      {
      if( alphabet[i] != 0xFF )
        {
        new_state.collation[i] = alphabet[i] ;
        }
      else
        {
        // Use a value bigger than HIGH, but which will sort according to the
        // original character-based order.
        new_state.collation[i] = 256 + i;
        }
      }
    __gg__alphabet_states[alphabet_index] = new_state;
    }

  return;
  }

extern "C"
void
__gg__string_to_numeric_edited( char * const dest,
                                const char *source,     // In source characters
                                int /*rdigits*/,
                                int is_negative,
                                const char *picture)
  {
  // This routine operates in ASCII space.  Life is hard enough without trying
  // to do this in EBCDIC, too.  So, 'source' and 'picture' are assumed to be
  // CP1252

  // It is required that the source string's implied decimal point line up with
  // the decimal point specified in the picture.

  // We assume that the caller left enough room in dest to take the expanded
  // picture string.

  // Note that we do not put on a nul terminator, so if you need one, it's
  // the caller's job to put it there.

  // Make a copy of the PICTURE string in the destination space.  The picture
  // is supposed to be the same length as the char_capacity of the variable.
  const int length_s = strlen(source);
  const int length_d = strlen(picture);
  memcpy(dest, picture, length_d);
  /* We have two kinds of floating insertion:  currency and sign.

     We have two kinds of zero suppression replacement: the 'Z' character is
     replaced with a space, and a '*' character is replaced with a '*'.

     Within the leading-zero range of floating insertion, the characters
     B Z 0 / comma are replaced with space.

     Within the leading-zero range of zero suppression, those characters take
     on the replacement character.

     We accomplish through two passes of the data.  The first pass scans the
     'dest' and replaces any possible digit position with the actual data from
     the 'source'.  While doing that, it finds the left/right limits for
     possible floating insertion and possible zero suppression.

     During a second pass, the limits of floating insertion and zero
     suppression are applied.  */

  // We need to know if we have a currency picture symbol in this string:
  // This is the currency character in the PICTURE
  unsigned char currency_char = NULLCH;
  const char *currency_text = NULL;   // This is the text we output when
  //                                  // encountering the currency_picture
  //                                  // character
  // Note that the currency_picture can be upper- or lower-case, and mean
  // separate things in IBM.  In ISO COBOL, the comparison is case-insensitive.

  // This is the character for a floating sign
  char sign_char;

  // In the following limits, all of the rightmosts are past-the-end indexes

  int leftmost_currency = -1;
  int rightmost_currency = -1;

  int leftmost_sign = -1;
  int rightmost_sign = -1;

  int leftmost_asterisk = -1;
  int rightmost_asterisk = -1;
  int zeroed_asterisk = 0;

  int leftmost_z = -1;
  int rightmost_z = -1;
  int zeroed_z = 0;

  int leftmost_nonzero = -1;

  int decimal_position = -1;

  // This is the first pass.  Because the currency
  int index_s = 0;
  for(int i=0; i<length_d; i++)
    {
    int ch = (unsigned int)dest[i] & 0xFF;
    if( ! __gg__currency_signs[ch].empty() )
      {
      currency_char = ch;
      currency_text = __gg__currency_signs[ch].c_str();
      if( leftmost_currency == -1 )
        {
        leftmost_currency = i;
        }
      else
        {
        // This is not the leftmost currency character, so it is where a digit
        // goes.
        dest[i] = source[index_s++];
        if( leftmost_nonzero == -1 && dest[i] != ascii_zero )
          {
          leftmost_nonzero = i;
          }
        }
      rightmost_currency = i+1;
      }
    if( ch == currency_char )
      {
      // We have already handled the currency char.
      continue;
      }
    switch(ch)
      {
      case ascii_minus:
      case ascii_plus:
        {
        if( is_negative )
          {
          sign_char = ascii_minus;
          }
        else
          {
          if( ch == ascii_plus )
            {
            sign_char = ascii_plus;
            }
          else
            {
            sign_char = ascii_space;
            }
          }
        dest[i] = sign_char;

        if( leftmost_sign == -1 )
          {
          leftmost_sign = i;
          }
        else
          {
          // This is not the leftmost sign character, so it is where a digit
          // goes.
          dest[i] = source[index_s++];
          if( leftmost_nonzero == -1 && dest[i] != ascii_zero )
            {
            leftmost_nonzero = i;
            }
          }
        rightmost_sign = i+1;
        break;
        }

      case ascii_comma:
      case ascii_period:
        if( ch == __gg__decimal_point )
          {
          decimal_position = i;
          }
      break;

      case ascii_slash:
      case ascii_d:
      case ascii_D:
      case ascii_b:
      case ascii_c:
      case ascii_C:
      case ascii_r:
      case ascii_R:
      case ascii_0:
        // These are left where they are
        break;

      case ascii_B:
        // This needs some special attention, because a DB at the end is not
        // the same as a B on its own at the end.
        if( i < length_d-1 )
          {
          dest[i] = ascii_space;
          }
        else if( length_d >= 1 )
          {
          if( dest[i-1] != ascii_D && dest[i-1] != ascii_d )
            {
            // This is an isolated B at the very end of the string
            dest[i] = ascii_space;
            }
          // Otherwise, that final B is part of a DB, and is left alone.
          }
        break;

      case ascii_9:
        // These are positions that hold digits
        dest[i] = source[index_s++];
        if( leftmost_nonzero == -1 && dest[i] != ascii_zero )
          {
          leftmost_nonzero = i;
          }
        break;

      case ascii_asterisk:
        // These are positions that hold digits
        if( leftmost_asterisk == -1 )
          {
          leftmost_asterisk = i;
          }
        rightmost_asterisk = i+1;

        dest[i] = source[index_s++];
        if( dest[i] == ascii_zero )
          {
          zeroed_asterisk += 1;
          }
        else
          {
          if( leftmost_nonzero == -1 )
            {
            leftmost_nonzero = i;
            }
          }
        break;

      case ascii_Z:
        // These are positions that hold digits
        if( leftmost_z == -1 )
          {
          leftmost_z = i;
          }
        rightmost_z = i+1;

        dest[i] = source[index_s++];
        if( dest[i] == ascii_zero )
          {
          zeroed_z += 1;
          }
        else
          {
          if( leftmost_nonzero == -1 )
            {
            leftmost_nonzero = i;
            }
          }
        break;

      default:
        abort();
      }
    }

  // Do currency replacement
  if( leftmost_currency >= 0 )
    {
    if( leftmost_currency == rightmost_currency-1 )
      {
      // This is a solo currency symbol
      memcpy(dest + leftmost_currency - (strlen(currency_text)-1),
             currency_text,
             strlen(currency_text));
      }
    else
      {
      // This is a floating currency symbol.  We need to start at
      // leftmost_currency, and walk to the lesser of rightmost_currency and
      // leftmost_nonzero.  We blank every character we encounter.
      int left = leftmost_currency;
      int right;
      if( leftmost_nonzero >= 0 )
        {
        right = std::min(leftmost_nonzero, rightmost_currency);
        if( decimal_position >= 0 )
          {
          right = std::min(right, decimal_position);
          }
        }
      else
        {
        right = rightmost_currency;
        }
      // blank out that range
      memset(dest+left, ascii_space, right-left);

      // To handle situations like PIC $$$,999, where the currency sign has to
      // overwrite the comma, we walk from here until we hit the column just to
      // the left of a digit
      while( right < length_d )
        {
      if(    (dest[right] >= ascii_zero && dest[right] <= ascii_nine)
          || right == decimal_position ) 
          {
          break;
          }
        dest[right] = ascii_space;
        right += 1;
        }
      if( right < length_d )
        {
        right -= 1;
        memcpy(dest + right - (strlen(currency_text)-1),
               currency_text,
               strlen(currency_text));
        }
      }
    }

  // Do sign replacement
  if( leftmost_sign >= 0 )
    {
    if( leftmost_sign == rightmost_sign-1 )
      {
      // This is a solo sign symbol
      dest[leftmost_sign] = sign_char;
      }
    else
      {
      // This is a floating sign symbol.  We need to start at
      // leftmost_sign, and walk to the lesser of rightmost_sign and
      // leftmost_nonzero.  We blank every character we encounter.
      int left = leftmost_sign;
      int right;
      if( leftmost_nonzero >= 0 )
        {
        right = std::min(leftmost_nonzero, rightmost_sign);
        if( decimal_position >= 0 )
          {
          right = std::min(right, decimal_position);
          }
        }
      else
        {
        right = rightmost_sign;
        }
      // blank out that range
      memset(dest+left, ascii_space, right-left);

      // To handle situations like PIC $$$,999, where the sign char has to
      // overwrite the comma, we walk from here until we hit the column just to
      // the left of a digit
      while( right < length_d )
        {
        if(    (dest[right] >= ascii_zero && dest[right] <= ascii_nine)
            || right == decimal_position ) 
          {
          break;
          }
        dest[right] = ascii_space;
        right += 1;
        }
      if( right < length_d )
        {
        right -= 1;
        dest[right] = sign_char;
        }
      }
    }

  // Do '*' zero suppression
  if( leftmost_asterisk >= 0 )
    {
    // This is a floating zero suppression.  

    int left = leftmost_asterisk;
    int right;
    if( leftmost_nonzero >= 0 )
      {
      // The value being formatted is non-zero, so make sure 'right' is no more
      // than the decimal position.
      right = std::min(leftmost_nonzero, rightmost_asterisk);
      if( decimal_position >= 0 && decimal_position > leftmost_asterisk )
        {
        right = std::min(right, decimal_position);
        }
      }
    else
      {
      // The value being formatted is zero
      if( zeroed_asterisk == length_s )
        {
        // Every numeric position was a zero on asterisk, so the whole thing
        // is starred.
        left = 0;
        right = length_d;
        }
      else
        {
        right = rightmost_asterisk;
        }
      }
    // blank out that range
    memset(dest+left, ascii_asterisk, right-left);

    // To handle situations like PIC ZZZ,999, where the suppression char has to
    // overwrite the comma, we walk from here until we hit the column just to
    // the left of a digit
    while( right < length_d )
      {
      if(    (dest[right] >= ascii_zero && dest[right] <= ascii_nine)
          || right == decimal_position ) 
        {
        break;
        }
      dest[right] = ascii_asterisk;
      right += 1;
      }
    // One final fillip:  The nature of '*' zero suppression is that the
    // decimal point is always visible.  Sending zero to ***,***.** results in
    // "*******.***"
    if( decimal_position >= 0 )
      {
      dest[decimal_position] = __gg__decimal_point;
      }
    }

  // Do 'Z' zero suppression
  if( leftmost_z >= 0 )
    {
    // This is a floating zero suppression.  

    int left = leftmost_z;
    int right;
    if( leftmost_nonzero >= 0 )
      {
      // The value being formatted is non-zero, so make sure 'right' is no more
      // than the decimal position.
      right = std::min(leftmost_nonzero, rightmost_z);
      if( decimal_position >= 0 && decimal_position > leftmost_asterisk )
        {
        right = std::min(right, decimal_position);
        }
      }
    else
      {
      if( zeroed_z == length_s )
        {
        // Every numeric position was a zero on asterisk, so the whole thing
        // is blanked.
        left = 0;
        right = length_d;
        }
      else
        {
        right = rightmost_z;
        }
      }
    // blank out that range
    memset(dest+left, ascii_space, right-left);

    // To handle situations like PIC ZZZ,999, where the suppression char has to
    // overwrite the comma, we walk from here until we hit the column just to
    // the left of a digit
    while( right < length_d )
      {
      if(    (dest[right] >= ascii_zero && dest[right] <= ascii_nine)
          || right == decimal_position ) 
        {
        break;
        }
      dest[right] = ascii_space;
      right += 1;
      }
    }

  // Any final DB/CR needs to be addressed:
  if(    !is_negative
      && length_d >= 2 
      && (   dest[length_d-2] == ascii_D 
          || dest[length_d-2] == ascii_d
          || dest[length_d-2] == ascii_C
          || dest[length_d-2] == ascii_c ) )
    {
    dest[length_d-2] = dest[length_d-1] = ascii_space;
    }

  return;
  }

extern "C"
void
__gg__string_to_alpha_edited(   char *dest,
                                cbl_encoding_t dest_encoding,
                                const char *source,
                                int slength,
                                const char *picture)
  {
  // 'source' is in 'dest' encoding

  // Put the PICTURE into the data area.  If the caller didn't leave enough
  // room, well, poo on them.  Said another way; if they specify disaster,
  // disaster is what they will get.

  // This routine expands picture into dest using ascii characters, but
  // replaces them with encoded characters

  charmap_t *charmap_dest = __gg__get_charmap(dest_encoding);

  int destlength = strlen(picture);
  memcpy(dest, picture, destlength);

  int dindex = 0;
  int sindex = 0;

  while( dindex < destlength )
    {
    char dch = dest[dindex];
    char sch;
    switch(dch)
      {
      case ascii_b:   // Replaced with space
      case ascii_B:
        dest[dindex] = charmap_dest->mapped_character(ascii_space);
        break;

      case ascii_zero:   // These are left alone:
        dest[dindex] = charmap_dest->mapped_character(ascii_0);
        break;

      case ascii_slash:
        dest[dindex] = charmap_dest->mapped_character(ascii_slash);
        break;

      default:
        // We assume that the parser isn't giving us a bad PICTURE
        // string, which means this character should be X, A, or 9
        // We don't check; we just replace it:
        if(sindex < slength)
          {
          sch = source[sindex++];
          }
        else
          {
          sch = charmap_dest->mapped_character(ascii_space);;
          }
        dest[dindex] = sch;
      }
    dindex += 1;
    }
  }

extern "C"
void
__gg__currency_sign_init() // This duplicates the constructor.
  {
  for( auto str : __gg__currency_signs ) {
    str.clear();
  }
  }

extern "C"
void
__gg__currency_sign(int symbol, const char *sign)
  {
  __gg__currency_signs[symbol] = sign;
  __gg__default_currency_sign = *sign;
  }

extern "C"
void
__gg__remove_trailing_zeroes(char *p)
  {
  // *p is a floating point number created by strfromN.  There will be no
  // leading spaces nor unnecessary leading zeroes, but there could be trailing
  // zeroes, e.g.  123.456000 or 1.23456000E2
  // Remove the trailing zeroes:
  if( *p == '-' )
    {
    p += 1;
    }
  char *left = p;
  char *right;
  char *pE = strchr(p, 'E');
  if( pE )
    {
    right = pE - 1;
    }
  else
    {
    right = p + strlen(p)-1;
    }

  if( strchr(left, '.') )
    {
    while( *right == '0' )
      {
      right -= 1;
      }
    if( *right == '.' )
      {
      right -= 1;
      }
    }

  right += 1;
  memmove(p, left, right-left);
  if( pE )
    {
    memmove(p + (right-left), pE, strlen(pE)+1);
    }
  else
    {
    p[right-left] = '\0';
    }
  }

// This is a convenient place to put this table, since it is used in both the
// run-time and compile-time code, and this source code module is one of the
// ones that are copied from ./libgcobol to gcc/cobol as part of the build.

ec_descr_t __gg__exception_table[] = {
  { ec_all_e,                    ec_category_none_e,
   "EC-ALL", "Any exception" },

  { ec_argument_e,               ec_category_none_e,
   "EC-ARGUMENT", "Argument error" },
  { ec_argument_function_e,      ec_category_fatal_e,
   "EC-ARGUMENT-FUNCTION", "Function argument error" },
  { ec_argument_imp_e,           uc_category_implementor_e,
   "EC-ARGUMENT-IMP", "Implementor-defined argument error" },

  { ec_argument_imp_command_e,   uc_category_implementor_e,
   "EC-ARGUMENT-IMP-COMMAND", "COMMAND-LINE Subscript out of bounds" },
  { ec_argument_imp_environment_e, uc_category_implementor_e,
   "EC-ARGUMENT-IMP-ENVIRONMENT", "Environment Variable is not defined" },

  { ec_bound_e,                  ec_category_none_e,
   "EC-BOUND", "Boundary violation" },
  { ec_bound_func_ret_value_e,   uc_category_nonfatal_e,
  "EC-BOUND-FUNC-RET-VALUE",
  "Intrinsic function output does not fit in returned value item" },
  { ec_bound_imp_e,              uc_category_implementor_e,
   "EC-BOUND-IMP", "Implementor-defined boundary violation" },
  { ec_bound_odo_e,              ec_category_fatal_e,
   "EC-BOUND-ODO", "OCCURS ... DEPENDING ON data item out of bounds" },
  { ec_bound_overflow_e,         uc_category_nonfatal_e,
   "EC-BOUND-OVERFLOW",
    "Current capacity of dynamic-capacity table greater than expected value" },
  { ec_bound_ptr_e,              uc_category_fatal_e,
   "EC-BOUND-PTR", "Data-pointer contains an address that is out of bounds" },
  { ec_bound_ref_mod_e,          ec_category_fatal_e,
   "EC-BOUND-REF-MOD", "Reference modifier out of bounds" },
  { ec_bound_set_e,              uc_category_nonfatal_e,
   "EC-BOUND-SET", "Invalid use of SET to set capacity of "
                      "dynamic-capacity table above specified maximum" },
  { ec_bound_subscript_e,        ec_category_fatal_e,
   "EC-BOUND-SUBSCRIPT", "Subscript out of bounds" },
  { ec_bound_table_limit_e,      uc_category_fatal_e,
   "EC-BOUND-TABLE-LIMIT",
    "Capacity of dynamic-capacity table would exceed implementor's maximum" },

  { ec_data_e,                   ec_category_none_e,
   "EC-DATA", "Data exception" },
  { ec_data_conversion_e,        uc_category_nonfatal_e,
   "EC-DATA-CONVERSION",
    "Conversion failed because of incomplete character correspondence" },
  { ec_data_imp_e,               uc_category_implementor_e,
   "EC-DATA-IMP", "Implementor-defined data exception" },
  { ec_data_incompatible_e,      uc_category_fatal_e,
   "EC-DATA-INCOMPATIBLE", "Incompatible data exception" },
  { ec_data_not_finite_e,        uc_category_fatal_e,
   "EC-DATA-NOT-FINITE",
    "Attempt to use a data item described with a standard floating-point usage "
    "when its contents are either a NaN or a representation of infinity" },
  { ec_data_overflow_e,          uc_category_fatal_e,
   "EC-DATA-OVERFLOW",
    "Exponent overflow during MOVE to a receiving data item described with a "
    "standard floating-point usage" },
  { ec_data_ptr_null_e,          uc_category_fatal_e,
   "EC-DATA-PTR-NULL",
    "Based item data-pointer is set to NULL when referenced" },

  { ec_external_data_mismatch_e,   uc_category_fatal_e,
   "EC-EXTERNAL-DATA-MISMATCH",
   "File referencing control item conflict because the linage, "
   "file status or relative key references are not to the same item " },
   { ec_external_file_mismatch_e,  uc_category_fatal_e,
     "EC-EXTERNAL-FILE-MISMATCH",
     "File control SELECT statements are not compatible" },
   { ec_external_format_conflict_e,  uc_category_fatal_e,
     "EC-EXTERNAL-FORMAT-CONFLICT",
     "Data definitions definitions do not conform" },

  { ec_flow_e,                   ec_category_none_e,
   "EC-FLOW", "Execution control flow violation" },
  { ec_flow_global_exit_e,       uc_category_fatal_e,
   "EC-FLOW-GLOBAL-EXIT", "EXIT PROGRAM in a global Declarative" },
  { ec_flow_global_goback_e,     uc_category_fatal_e,
   "EC-FLOW-GLOBAL-GOBACK", "GOBACK in a global declarative" },
  { ec_flow_imp_e,               uc_category_implementor_e,
   "EC-FLOW-IMP", "Implementor-defined control flow violation" },
  { ec_flow_release_e,           uc_category_fatal_e,
   "EC-FLOW-RELEASE", "RELEASE not in range of SORT" },
  { ec_flow_report_e,            uc_category_fatal_e,
   "EC-FLOW-REPORT",
    "GENERATE, INITIATE, or TERMINATE during USE BEFORE REPORTING declarative" },
  { ec_flow_return_e,            uc_category_fatal_e,
   "EC-FLOW-RETURN", "RETURN not in range of MERGE or SORT" },
  { ec_flow_search_e,            uc_category_fatal_e,
   "EC-FLOW-SEARCH",
    "Invalid use of SET to change capacity of dynamic- capacity table during "
    "SEARCH of same table" },
  { ec_flow_use_e,               uc_category_fatal_e,
   "EC-FLOW-USE", "A USE statement caused another to be executed" },

  { ec_function_e,               ec_category_none_e,
   "EC-FUNCTION", "Function exception" },
  { ec_function_not_found_e,     uc_category_fatal_e,
   "EC-FUNCTION-NOT-FOUND",
    "Function not found or function pointer does not point to a function" },
  { ec_function_ptr_invalid_e,   uc_category_fatal_e,
   "EC-FUNCTION-PTR-INVALID", "Signature mismatch" },
  { ec_function_ptr_null_e,      uc_category_fatal_e,
   "EC-FUNCTION-PTR-NULL",
    "Function pointer used in calling a function is NULL" },

  { ec_imp_e,                     ec_category_none_e,
   "EC-IMP", "GCC-defined exception" },
  { ec_imp_iconv_open_e,          uc_category_fatal_e,
   "EC-IMP-ICONV-OPEN", "Encoding conversion unavailable for requested pair" },

  { ec_io_e,                     ec_category_none_e,
   "EC-IO", "Input-output exception" },
  { ec_io_at_end_e,              uc_category_nonfatal_e,
   "EC-I-O-AT-END", "I-O status 1x" },
  { ec_io_eop_e,                 uc_category_nonfatal_e,
   "EC-I-O-EOP", "An end of page condition occurred" },
  { ec_io_eop_overflow_e,        uc_category_nonfatal_e,
   "EC-I-O-EOP-OVERFLOW", "A page overflow condition occurred" },
  { ec_io_file_sharing_e,        uc_category_nonfatal_e,
   "EC-I-O-FILE-SHARING", "I-O status 6x" },
  { ec_io_imp_e,                 uc_category_implementor_e,
   "EC-I-O-IMP", "I-O status 9x" },
  { ec_io_invalid_key_e,         uc_category_nonfatal_e,
   "EC-I-O-INVALID-KEY", "I-O status 2x" },
  { ec_io_linage_e,              uc_category_fatal_e,
   "EC-I-O-LINAGE",
    "The value of a data item referenced in the LINAGE clause is not within "
    "the required range" },
  { ec_io_logic_error_e,         uc_category_fatal_e,
   "EC-I-O-LOGIC-ERROR", "I-O status 4x" },
  { ec_io_permanent_error_e,     uc_category_fatal_e,
   "EC-I-O-PERMANENT-ERROR", "I-O status 3x" },
  { ec_io_record_operation_e,    uc_category_nonfatal_e,
   "EC-I-O-RECORD-OPERATION", "I-O status 5x" },

  { ec_imp_e,                    ec_category_none_e,
   "EC-IMP", "Implementor-defined exception condition" },

  { ec_imp_suffix_e,             ec_category_none_e,
   "EC-IMP-SUFFIX", "Imp" },

  { ec_locale_e,                 ec_category_none_e,
   "EC-LOCALE", "Any locale related exception" },
  { ec_locale_imp_e,             uc_category_implementor_e,
   "EC-LOCALE-IMP", "Implementor-defined locale related exception" },
  { ec_locale_incompatible_e,    uc_category_fatal_e,
   "EC-LOCALE-INCOMPATIBLE",
    "The referenced locale does not specify the expected characters in "
    "LC_COLLATE" },
  { ec_locale_invalid_e,         uc_category_fatal_e,
   "EC-LOCALE-INVALID", "Locale content is invalid or incomplete" },
  { ec_locale_invalid_ptr_e,     uc_category_fatal_e,
   "EC-LOCALE-INVALID-PTR", "Pointer does not reference a saved locale" },
  { ec_locale_missing_e,         uc_category_fatal_e,
   "EC-LOCALE-MISSING", "The specified locale is not available" },
  { ec_locale_size_e,            uc_category_fatal_e,
   "EC-LOCALE-SIZE", "Digits were truncated in locale editing" },

  { ec_oo_e,                     ec_category_none_e,
   "EC-OO", "Any predefined OO related exception" },
  { ec_oo_arg_omitted_e,         uc_category_fatal_e,
   "EC-OO-ARG-OMITTED", "Reference to an omitted argument" },
  { ec_oo_conformance_e,         uc_category_fatal_e,
   "EC-OO-CONFORMANCE", "Failure for an object-view" },
  { ec_oo_exception_e,           uc_category_fatal_e,
   "EC-OO-EXCEPTION", "An exception object was not handled" },
  { ec_oo_imp_e,                 uc_category_implementor_e,
   "EC-OO-IMP", "Implementor-defined OO exception" },
  { ec_oo_method_e,              uc_category_fatal_e,
   "EC-OO-METHOD", "Requested method is not available" },
  { ec_oo_null_e,                uc_category_fatal_e,
   "EC-OO-NULL",
    "Method invocation was attempted with a null object reference" },
  { ec_oo_resource_e,            uc_category_fatal_e,
   "EC-OO-RESOURCE", "Insufficient system resources to create the object" },
  { ec_oo_universal_e,           uc_category_fatal_e,
   "EC-OO-UNIVERSAL", "A runtime type check failed" },

  { ec_order_e,                  ec_category_none_e,
   "EC-ORDER", "Ordering exception" },
  { ec_order_imp_e,              uc_category_implementor_e,
   "EC-ORDER-IMP", "Implementor-defined ordering exception" },
  { ec_order_not_supported_e,    uc_category_fatal_e,
   "EC-ORDER-NOT-SUPPORTED",
    "Cultural ordering table or ordering level specified for "
    "STANDARD-COMPARE function not supported" },

  { ec_overflow_e,               ec_category_none_e,
   "EC-OVERFLOW", "Overflow condition" },
  { ec_overflow_imp_e,           uc_category_implementor_e,
   "EC-OVERFLOW-IMP", "Implementor-defined overflow condition" },
  { ec_overflow_string_e,        uc_category_nonfatal_e,
   "EC-OVERFLOW-STRING", "STRING overflow condition" },
  { ec_overflow_unstring_e,      uc_category_nonfatal_e,
   "EC-OVERFLOW-UNSTRING", "UNSTRING overflow condition" },

  { ec_program_e,                ec_category_none_e,
   "EC-PROGRAM", "Inter-program communication exception" },
  { ec_program_arg_mismatch_e,   uc_category_fatal_e,
   "EC-PROGRAM-ARG-MISMATCH", "Parameter mismatch" },
  { ec_program_arg_omitted_e,    uc_category_fatal_e,
   "EC-PROGRAM-ARG-OMITTED", "A reference to an omitted argument" },
  { ec_program_cancel_active_e,  uc_category_fatal_e,
   "EC-PROGRAM-CANCEL-ACTIVE", "Canceled program active" },
  { ec_program_imp_e,            uc_category_implementor_e,
   "EC-PROGRAM-IMP",
    "Implementor-defined inter-program communication exception" },
  { ec_program_not_found_e,      uc_category_fatal_e,
   "EC-PROGRAM-NOT-FOUND", "Called program not found" },
  { ec_program_ptr_null_e,       uc_category_fatal_e,
   "EC-PROGRAM-PTR-NULL", "Program-pointer used in CALL is set to NULL" },
  { ec_program_recursive_call_e, uc_category_fatal_e,
   "EC-PROGRAM-RECURSIVE-CALL", "Called program active" },
  { ec_program_resources_e,      uc_category_fatal_e,
   "EC-PROGRAM-RESOURCES", "Resources not available for called program" },

  { ec_raising_e,                ec_category_none_e,
   "EC-RAISING", "EXIT ... RAISING or GOBACK RAISING exception" },
  { ec_raising_imp_e,            uc_category_implementor_e,
   "EC-RAISING-IMP",
    "Implementor-defined EXIT ... RAISING or GOBACK RAISING exception" },
  { ec_raising_not_specified_e,  uc_category_fatal_e,
   "EC-RAISING-NOT-SPECIFIED",
    "EXIT ... RAISING or GOBACK RAISING an EC-USER exception condition not "
    "specified in RAISING phrase of procedure division header" },

  { ec_range_e,                  ec_category_none_e,
   "EC-RANGE", "Range exception" },
  { ec_range_imp_e,              uc_category_implementor_e,
   "EC-RANGE-IMP", "Implementor-defined range exception" },
  { ec_range_index_e,            uc_category_fatal_e,
   "EC-RANGE-INDEX",
    "Index set outside the range of values allowed by the implementor" },
  { ec_range_inspect_size_e,     uc_category_fatal_e,
   "EC-RANGE-INSPECT-SIZE", "Size of replace items in INSPECT differs" },
  { ec_range_invalid_e,          uc_category_nonfatal_e,
   "EC-RANGE-INVALID",
    "Starting value of THROUGH range greater than ending value" },
  { ec_range_perform_varying_e,  uc_category_fatal_e,
   "EC-RANGE-PERFORM-VARYING",
    "Setting of varied item in PERFORM is negative" },
  { ec_range_ptr_e,              uc_category_fatal_e,
   "EC-RANGE-PTR", "Pointer SET UP or DOWN is outside range" },
  { ec_range_search_index_e,     uc_category_nonfatal_e,
   "EC-RANGE-SEARCH-INDEX",
    "No table element found in SEARCH because initial index out of range" },
  { ec_range_search_no_match_e,  uc_category_nonfatal_e,
   "EC-RANGE-SEARCH-NO-MATCH",
    "No table element found in SEARCH because no element matched criteria" },

  { ec_report_e,                 ec_category_none_e,
   "EC-REPORT", "Report writer exception" },
  { ec_report_active_e,          uc_category_fatal_e,
   "EC-REPORT-ACTIVE", "INITIATE on an active report" },
  { ec_report_column_overlap_e,  uc_category_nonfatal_e,
   "EC-REPORT-COLUMN-OVERLAP", "Overlapping report items" },
  { ec_report_file_mode_e,       uc_category_fatal_e,
   "EC-REPORT-FILE-MODE",
    "An INITIATE statement was executed for a file connector that was not "
    "open in the extend or output mode" },
  { ec_report_imp_e,             uc_category_implementor_e,
   "EC-REPORT-IMP", "Implementor-defined report writer exception" },
  { ec_report_inactive_e,        uc_category_fatal_e,
   "EC-REPORT-INACTIVE", "GENERATE or TERMINATE on an inactive report" },
  { ec_report_line_overlap_e,    uc_category_nonfatal_e,
   "EC-REPORT-LINE-OVERLAP", "Overlapping report lines" },
  { ec_report_not_terminated_e,  uc_category_nonfatal_e,
   "EC-REPORT-NOT-TERMINATED", "Report file closed with active report" },
  { ec_report_page_limit_e,      uc_category_nonfatal_e,
   "EC-REPORT-PAGE-LIMIT", "Vertical page limit exceeded" },
  { ec_report_page_width_e,      uc_category_nonfatal_e,
   "EC-REPORT-PAGE-WIDTH", "Page width exceeded" },
  { ec_report_sum_size_e,        uc_category_fatal_e,
   "EC-REPORT-SUM-SIZE", "Overflow of sum counter" },
  { ec_report_varying_e,         uc_category_fatal_e,
   "EC-REPORT-VARYING", "VARYING clause expression noninteger" },

  { ec_screen_e,                 ec_category_none_e,
   "EC-SCREEN", "Screen handling exception" },
  { ec_screen_field_overlap_e,   uc_category_nonfatal_e,
   "EC-SCREEN-FIELD-OVERLAP", "Screen fields overlap" },
  { ec_screen_imp_e,             uc_category_implementor_e,
   "EC-SCREEN-IMP", "Implementor-defined screen handling exception" },
  { ec_screen_item_truncated_e,  uc_category_nonfatal_e,
   "EC-SCREEN-ITEM-TRUNCATED", "Screen field too long for line" },
  { ec_screen_line_number_e,     uc_category_nonfatal_e,
   "EC-SCREEN-LINE-NUMBER",
    "Screen item line number exceeds terminal size" },
  { ec_screen_starting_column_e, uc_category_nonfatal_e,
   "EC-SCREEN-STARTING-COLUMN",
    "Screen item starting column exceeds line size" },

  { ec_size_e,                   ec_category_none_e,
   "EC-SIZE", "Size error exception" },
  { ec_size_address_e,           uc_category_fatal_e,
   "EC-SIZE-ADDRESS", "Invalid pointer arithmetic" },
  { ec_size_exponentiation_e,    ec_category_fatal_e,
   "EC-SIZE-EXPONENTIATION", "Exponentiation rules violated" },
  { ec_size_imp_e,               uc_category_implementor_e,
   "EC-SIZE-IMP", "Implementor-defined size error exception" },
  { ec_size_overflow_e,          ec_category_fatal_e,
   "EC-SIZE-OVERFLOW", "Arithmetic overflow in calculation" },
  { ec_size_truncation_e,        ec_category_fatal_e,
   "EC-SIZE-TRUNCATION", "Significant digits truncated in store" },
  { ec_size_underflow_e,         ec_category_fatal_e,
   "EC-SIZE-UNDERFLOW", "Floating-point underflow" },
  { ec_size_zero_divide_e,       ec_category_fatal_e,
   "EC-SIZE-ZERO-DIVIDE", "Division by zero" },

  { ec_sort_merge_e,             ec_category_none_e,
   "EC-SORT-MERGE", "SORT or MERGE exception" },
  { ec_sort_merge_active_e,      uc_category_fatal_e,
   "EC-SORT-MERGE-ACTIVE",
    "File SORT or MERGE executed when one is already active" },
  { ec_sort_merge_file_open_e,   ec_category_fatal_e,
   "EC-SORT-MERGE-FILE-OPEN",
    "A USING or GIVING file is open upon execution of a SORT or MERGE" },
  { ec_sort_merge_imp_e,         uc_category_implementor_e,
   "EC-SORT-MERGE-IMP",
    "Implementor-defined SORT or MERGE exception" },
  { ec_sort_merge_release_e,     uc_category_fatal_e,
   "EC-SORT-MERGE-RELEASE", "RELEASE record too long or too short" },
  { ec_sort_merge_return_e,      uc_category_fatal_e,
   "EC-SORT-MERGE-RETURN", "RETURN executed when at end condition exists" },
  { ec_sort_merge_sequence_e,    uc_category_fatal_e,
   "EC-SORT-MERGE-SEQUENCE", "Sequence error on MERGE USING file" },

  { ec_storage_e,                ec_category_none_e,
   "EC-STORAGE", "Storage allocation exception" },
  { ec_storage_imp_e,            uc_category_implementor_e,
   "EC-STORAGE-IMP", "Implementor-defined storage allocation exception" },
  { ec_storage_not_alloc_e,      uc_category_nonfatal_e,
   "EC-STORAGE-NOT-ALLOC",
    "The data-pointer specified in a FREE statement does not identify "
    "currently allocated storage" },
  { ec_storage_not_avail_e,      uc_category_nonfatal_e,
   "EC-STORAGE-NOT-AVAIL",
    "The amount of storage requested by an ALLOCATE statement "
    "is not available"},
  { ec_user_e,                   ec_category_none_e,
   "EC-USER", "User-defined exception condition" },
  { ec_user_suffix_e,            uc_category_nonfatal_e,
   "EC-USER-SUFFIX", "Level-3 user-defined exception condition" },

  { ec_validate_e,               ec_category_none_e,
   "EC-VALIDATE", "VALIDATE exception" },
  { ec_validate_content_e,       uc_category_nonfatal_e,
   "EC-VALIDATE-CONTENT", "VALIDATE content error" },
  { ec_validate_format_e,        uc_category_nonfatal_e,
   "EC-VALIDATE-FORMAT", "VALIDATE format error" },
  { ec_validate_imp_e,           uc_category_implementor_e,
   "EC-VALIDATE-IMP", "Implementor-defined VALIDATE exception" },
  { ec_validate_relation_e,      uc_category_nonfatal_e,
   "EC-VALIDATE-RELATION", "VALIDATE relation error" },
  { ec_validate_varying_e,       uc_category_fatal_e,
   "EC-VALIDATE-VARYING", "VARYING clause expression noninteger" },
} ;

ec_descr_t *__gg__exception_table_end = __gg__exception_table + COUNT_OF(__gg__exception_table);

