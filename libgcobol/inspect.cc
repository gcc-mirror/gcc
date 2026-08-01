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
#include "cobol-endian.h"
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

static inline char *
as_chars(unsigned char *p)
  {
  return reinterpret_cast<char *>(p);
  }

static inline const char *
as_chars(const unsigned char *p)
  {
  return reinterpret_cast<const char *>(p);
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
  size_t length_of_whole = whole_end - whole;

  if( length_of_piece == 0 )
    {
    __gg__abort("funky_find() length_of_piece shouldn't be zero");
    }

  if( length_of_piece > length_of_whole )
    {
    return NULL;
    }

  const char *last = whole_end - length_of_piece;

  while( whole <= last )
    {
    if( memcmp(piece, whole, length_of_piece) == 0 )
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
  size_t length_of_haystack = haystack_end - haystack;

  if( length_of_piece == 0 )
    {
    __gg__abort("funky_find_wide() length_of_piece shouldn't be zero");
    }

  if( length_of_piece > length_of_haystack )
    {
    return notfound;
    }

  char_it_c last = haystack_end - length_of_piece;

  while( haystack <= last )
    {
    // Compare the memory at needle to the memory at haystack
    if( memcmp(&(*needle),
               &(*haystack),
               length_of_piece * sizeof(cbl_char_t)) == 0 )
      {
      // They are the same; return where needle was found
      retval = haystack;
      break;
      }
    // Not found; move to the next location in the haystack
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
  size_t length_of_whole = whole_end - whole;

  if( length_of_piece == 0 )
    {
    __gg__abort("funky_find_backward() length_of_piece shouldn't be zero");
    }

  if( length_of_piece > length_of_whole )
    {
    return NULL;
    }

  const char *last = whole_end - length_of_piece;

  while( whole <= last )
    {
    if( memcmp(piece, last, length_of_piece) == 0 )
      {
      retval = last;
      break;
      }
    last -= 1;
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
  size_t length_of_haystack = haystack_end - haystack;

  if( length_of_piece == 0 )
    {
    __gg__abort(
      "funky_find_wide_backward() length_of_piece shouldn't be zero");
    }

  if( length_of_piece > length_of_haystack )
    {
    return notfound;
    }

  char_it_c last = haystack_end - length_of_piece;

  while( haystack <= last )
    {
    if( memcmp(&(*needle),
               &(*last),
               length_of_piece * sizeof(cbl_char_t)) == 0 )
      {
      // They are the same; return where needle was found
      retval = last;
      break;
      }
    // Not found; move to the next location in the haystack
    last -= 1;
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

typedef struct comparand_sbc
  {
  size_t id_2_index;
  cbl_inspect_bound_t operation;
  std::string identifier_3; // The thing to be found
//q  std::string identifier_5; // The replacement, for FORMAT 2
  size_t      alpha; // The start location within normalized_id_1
  size_t      omega; // The end+1 location within normalized_id_1
  size_t      leading_count;
  bool        leading;
  bool        first;
  } comparand_sbc;

typedef struct id_2_result
  {
  cblc_field_t *id2;
  size_t        id2_o;
  size_t        id2_s;
  size_t result;
  } id_2_result;

static void
append_normalized_character(normalized_operand &operand, cbl_char_t ch)
  {
  operand.the_vectorxxxx.push_back(ch);
  operand.the_characters += static_cast<char>(ch & 0xFF);
  }

static normalized_operand
normalize_id( const cblc_field_t *field,
              size_t              field_o,
              size_t              field_s,
              cbl_encoding_t      encoding )
  {
  normalized_operand retval;

  retval.offset = 0;
  retval.length = 0;

  if( !field )
    {
    return retval;
    }

  cbl_encoding_t source_encoding = field->encoding;
  const charmap_t *charmap_source = __gg__get_charmap(source_encoding);
  charmap_t *charmap_host32 = __gg__get_charmap(HOST_32_ENCODING);
  (void)encoding;
  int source_stride = charmap_source->stride();

  const unsigned char *data = field->data + field_o;
  cbl_figconst_t figconst
    = static_cast<cbl_figconst_t>(field->attr & FIGCONST_MASK);

  size_t character_count = field_s / source_stride;
  size_t offset = 0;
  size_t length = character_count;

  if( field->type == FldNumericDisplay && (field->attr & separate_e) )
    {
    if( length > 0 )
      {
      length -= 1;
      }
    if( field->attr & leading_e )
      {
      offset = 1;
      }
    }

  retval.offset = offset;
  retval.length = length;

  if( figconst == normal_value_e )
    {
    size_t converted_bytes = 0;
    const char *converted = __gg__iconverter(source_encoding,
                                             HOST_32_ENCODING,
                                             data + offset * source_stride,
                                             length * source_stride,
                                             &converted_bytes);

    for( size_t i = 0; i < converted_bytes; i += width_of_utf32 )
      {
      cbl_char_t ch = charmap_host32->getch(converted, i);

      if( field->type == FldNumericDisplay )
        {
        if( charmap_source->is_like_ebcdic() )
          {
          // In EBCDIC, a flagged negative digit 0xF0 through 0xF9 becomes
          // 0xD0 through 0xD9.  Those represent the characters
          // "}JKLMNOPQR", which, now that we are in UTF32 space, don't
          // have the right bit pattern to be fixed with set_digit_negative().
          // So, we fix it separately with this table.  Note that location
          // 0x7D, which is ASCII '{', becomes 0x30 '0'.  See also that
          // locations 0x4A through 0x52 become 0x31 through 0x39.
          static const uint8_t fixit[256] =
            {
      0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
      0x80, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
      0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
      0x81, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
      0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
      0x82, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
      0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
      0x83, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
      0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
      0x84, 0x49, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36,
      0x37, 0x38, 0x39, 0x53, 0x54, 0x55, 0x56, 0x57,
      0x85, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
      0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
      0x86, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
      0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
      0x87, 0x79, 0x7a, 0x7b, 0x7c, 0x30, 0x7e, 0x7f,
      0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
      0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
      0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
      0x89, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
      0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
      0x8a, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
      0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
      0x8b, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
      0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
      0x8c, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
      0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
      0x8d, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
      0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
      0x8e, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
      0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
      0x8f, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff,
            };
          ch = fixit[ch & 0xFF];
          }
        else
          {
          ch = charmap_host32->set_digit_negative(ch, false);
          }
        }
      append_normalized_character(retval, ch);
      }
    }
  else
    {
    cbl_char_t ch = charmap_host32->figconst_character(figconst);
    for( size_t i = 0; i < length; i += 1 )
      {
      append_normalized_character(retval, ch);
      }
    }

  retval.length = retval.the_vectorxxxx.size();
  return retval;
  }

static std::string
normalize_id_sbc( const cblc_field_t *field,
                  size_t              field_o,
                  size_t              field_s,
                  cbl_encoding_t      encoding )
  {
  // We know that the field is ASCII or EBCDIC
  std::string retval;

  if( field && field_s )
    {
    charmap_t *charmap = __gg__get_charmap(encoding);

    const unsigned char *data = field->data + field_o;
    cbl_figconst_t figconst
      = (cbl_figconst_t)(field->attr & FIGCONST_MASK);

    if( field->type == FldNumericDisplay )
      {
      // The value is NumericDisplay.
      if( field->attr & separate_e )
        {
        // Because the sign is a separate plus or minus, the length
        // gets reduced by one:
        field_s -= 1;
        if( field->attr & leading_e )
          {
          // Because the sign character is LEADING, we increase the
          // offset by one
          data += 1;
          }
        }
      // At this point, the bytes start at data, and there are field_s of them.
      retval.assign(as_chars(data), field_s);
      if( field->attr & signable_e )
        {
        if( field->attr & leading_e )
          {
          // The sign might be in the first byte; get rid of it
          retval[0] = charmap->set_digit_negative(data[0], false);
          }
        else
          {
          // The sign might be in the last byte; get rid of it
          retval[0] = charmap->set_digit_negative(data[field_s-1], false);
          }
        }
      }
    else
      {
      // We aren't dealing with numeric-display, so
      if( figconst == normal_value_e )
        {
        retval.assign(as_chars(data), field_s);
        }
      else
        {
        // This field is flagged as figconst
        uint8_t ch =  charmap->figconst_character(figconst);
        retval.assign(field_s, ch);
        }
      }
    }
  else
    {
    // There is no field, so leave retval empty
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
      position in source for the corresponding comparison
      operation. Comparisons
      in source occur only to the left of the first occurrence of delimiter. If
      delimiter is not present in source, then the comparison proceeds as if
      there were no BEFORE phrase.

      The AFTER phrase modifies the character position to use as the leftmost
      position in source for the corresponding comparison
      operation. Comparisons
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
the_alpha_and_omega_sbc(const std::string     &id_before,
                        const std::string     &id_after,
                        const std::string     &haystack,
                        size_t                &alpha,
                        size_t                &omega)
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
      position in source for the corresponding comparison
      operation. Comparisons
      in source occur only to the left of the first occurrence of delimiter. If
      delimiter is not present in source, then the comparison proceeds as if
      there were no BEFORE phrase.

      The AFTER phrase modifies the character position to use as the leftmost
      position in source for the corresponding comparison
      operation. Comparisons
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

  if( id_before.length() )
    {
    // Look for BEFORE in the haystack.
    omega = haystack.find(id_before);
    if( omega == std::string::npos )
      {
      // If BEFORE isn't found, we use the whole haystack.
      omega = haystack.length();
      }
    }
  else
    {
    omega = haystack.length();
    }

  if( id_after.length() )
    {
    // This is the AFTER delimiter.  We look for the first occurrence of that
    // delimiter in id_1 that occurs to the left of BEFORE/omega

    alpha = haystack.substr(0, omega).find(id_after);
    if( alpha == std::string::npos )
      {
      // If there is no AFTER to the left of omega, then we can't find anything
      // in this haystack.
      alpha = haystack.length();
      }
    else
      {
      alpha += id_after.length();
      }
    }
  else
    {
    alpha = 0;
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

    char_it_c omega_found
      = funky_find_wide_backward(id_before.the_vectorxxxx.begin(),
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

    char_it_c omega_found
      = funky_find_wide_backward(id_after.the_vectorxxxx.begin(),
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
inspect_backward_format_1(const size_t integers[],
                          const cblc_referlet_t *params)
  {
  size_t int_index = 0;
  size_t cblc_index = 0;

  // Reference the language specification for the meanings of identifier_X

  // Pick up the number of identifier_2 loops in this INSPECT statement
  size_t n_identifier_2 = integers[int_index++];

  std::vector<id_2_result> id_2_results(n_identifier_2);

  // Pick up identifier_1, which is the string being inspected
  const cblc_field_t *id1   = params[cblc_index].field ;
  size_t              id1_o = params[cblc_index].offset;
  size_t              id1_s = params[cblc_index].size  ;
  cblc_index += 1;
  // normalize it, according to the language specification.
  normalized_operand normalized_id_1
    = normalize_id(id1, id1_o, id1_s, id1->encoding);

  std::vector<comparand> comparands;

  for(size_t i=0; i<n_identifier_2; i++)
    {
    // For each identifier_2, we pick up its value:

    id_2_results[i].id2   = params[cblc_index].field ;
    id_2_results[i].id2_o = params[cblc_index].offset;
    id_2_results[i].id2_s = params[cblc_index].size  ;

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

          const cblc_field_t *id4_before   = params[cblc_index].field ;
          size_t              id4_before_o = params[cblc_index].offset;
          size_t              id4_before_s = params[cblc_index].size  ;
          cblc_index += 1;

          const cblc_field_t *id4_after   = params[cblc_index].field ;
          size_t              id4_after_o = params[cblc_index].offset;
          size_t              id4_after_s = params[cblc_index].size  ;
          cblc_index += 1;

          normalized_operand normalized_id_4_before
            = normalize_id(id4_before,
                           id4_before_o,
                           id4_before_s,
                           id1->encoding);

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
          // We have some number of identifier-3 values,
          // each with possible PHRASE1 modifiers.
          size_t pair_count = integers[int_index++];

          // We need to build up pair_count comparand structures:

          for(size_t k=0; k<pair_count; k++)
            {
            comparand next_comparand = {};
            next_comparand.id_2_index = i;
            next_comparand.operation = operation;

            const cblc_field_t *id3   = params[cblc_index].field ;
            size_t              id3_o = params[cblc_index].offset;
            size_t              id3_s = params[cblc_index].size  ;
            cblc_index += 1;

            const cblc_field_t *id4_before   = params[cblc_index].field ;
            size_t              id4_before_o = params[cblc_index].offset;
            size_t              id4_before_s = params[cblc_index].size  ;
            cblc_index += 1;

            const cblc_field_t *id4_after   = params[cblc_index].field ;
            size_t              id4_after_o = params[cblc_index].offset;
            size_t              id4_after_s = params[cblc_index].size  ;
            cblc_index += 1;

            next_comparand.identifier_3
              = normalize_id(id3, id3_o, id3_s, id1->encoding);

            next_comparand.alpha
              = normalized_id_1.the_characters.c_str();
            next_comparand.omega
              = next_comparand.alpha + normalized_id_1.length;

            normalized_operand normalized_id_4_before
              = normalize_id(id4_before,
                           id4_before_o,
                           id4_before_s,
                           id1->encoding);

            normalized_operand normalized_id_4_after
              = normalize_id(id4_after,
                             id4_after_o,
                             id4_after_s,
                             id1->encoding);

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
    int128 id_2_value;
    __gg__int128_from_qualified_field(id_2_value,
                                      id_2_results[i].id2,
                                      id_2_results[i].id2_o,
                                      id_2_results[i].id2_s);
    while(id_2_value.rdigits--)
      {
      id_2_value.i128 /= 10.0;
      }

    // Accumulate what we've found into it
    id_2_value.i128 += id_2_results[i].result;

    // And put it back:
    __gg__int128_to_qualified_field(id_2_results[i].id2,
                                    id_2_results[i].id2_o,
                                    id_2_results[i].id2_s,
                                    id_2_value.i128,
                                    0,
                                    truncation_e,
                                    NULL);
    }
  }

extern "C"
void
__gg__inspect_format_1( int backward,
                        size_t integers[],
                        const cblc_referlet_t *params)
  {
  if( backward )
    {
    return inspect_backward_format_1(integers, params);
    }

  size_t int_index = 0;
  size_t cblc_index = 0;

  // Reference the language specification for the meanings of identifier_X

  // Pick up the number of identifier_2 loops in this INSPECT statement
  size_t n_identifier_2 = integers[int_index++];

  std::vector<id_2_result> id_2_results(n_identifier_2);

  // Pick up identifier_1, which is the string being inspected
  const cblc_field_t *id1   = params[cblc_index].field ;
  size_t              id1_o = params[cblc_index].offset;
  size_t              id1_s = params[cblc_index].size  ;
  cblc_index += 1;
  // normalize it, according to the language specification.
  normalized_operand normalized_id_1
                             = normalize_id(id1, id1_o, id1_s, id1->encoding);

  std::vector<comparand> comparands;

  for(size_t i=0; i<n_identifier_2; i++)
    {
    // For each identifier_2, we pick up its value:

    id_2_results[i].id2   = params[cblc_index].field ;
    id_2_results[i].id2_o = params[cblc_index].offset;
    id_2_results[i].id2_s = params[cblc_index].size  ;

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

          const cblc_field_t *id4_before   = params[cblc_index].field ;
          size_t              id4_before_o = params[cblc_index].offset;
          size_t              id4_before_s = params[cblc_index].size  ;
          cblc_index += 1;

          const cblc_field_t *id4_after   = params[cblc_index].field ;
          size_t              id4_after_o = params[cblc_index].offset;
          size_t              id4_after_s = params[cblc_index].size  ;
          cblc_index += 1;

          normalized_operand normalized_id_4_before
            = normalize_id(id4_before,
                           id4_before_o,
                           id4_before_s,
                           id1->encoding);

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
          // We have some number of identifier-3 values,
          // each with possible PHRASE1 modifiers.
          size_t pair_count = integers[int_index++];

          // We need to build up pair_count comparand structures:

          for(size_t k=0; k<pair_count; k++)
            {
            comparand next_comparand = {};
            next_comparand.id_2_index = i;
            next_comparand.operation = operation;

            const cblc_field_t *id3   = params[cblc_index].field ;
            size_t              id3_o = params[cblc_index].offset;
            size_t              id3_s = params[cblc_index].size  ;
            cblc_index += 1;

            const cblc_field_t *id4_before   = params[cblc_index].field ;
            size_t              id4_before_o = params[cblc_index].offset;
            size_t              id4_before_s = params[cblc_index].size  ;
            cblc_index += 1;

            const cblc_field_t *id4_after   = params[cblc_index].field ;
            size_t              id4_after_o = params[cblc_index].offset;
            size_t              id4_after_s = params[cblc_index].size  ;
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
              = normalize_id(id4_before,
                           id4_before_o,
                           id4_before_s,
                           id1->encoding);

            normalized_operand normalized_id_4_after
              = normalize_id(id4_after,
                             id4_after_o,
                             id4_after_s,
                             id1->encoding);

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
    int128 id_2_value;
    __gg__int128_from_qualified_field(id_2_value,
                                      id_2_results[i].id2,
                                      id_2_results[i].id2_o,
                                      id_2_results[i].id2_s);
    while(id_2_value.rdigits--)
      {
      id_2_value.i128 /= 10.0;
      }

    // Accumulate what we've found into it
    id_2_value.i128 += id_2_results[i].result;

    // And put it back:
    __gg__int128_to_qualified_field(id_2_results[i].id2,
                                    id_2_results[i].id2_o,
                                    id_2_results[i].id2_s,
                                    id_2_value.i128,
                                    0,
                                    truncation_e,
                                    NULL);
    }
  }

static
void
inspect_backward_format_2(const size_t integers[],
                          const cblc_referlet_t *params)
  {
  size_t int_index = 0;
  size_t cblc_index = 0;

  // Reference the language specification for the meanings of identifier_X

  // Pick up identifier_1, which is the string being inspected
  cblc_field_t *id1   = params[cblc_index].field ;
  size_t        id1_o = params[cblc_index].offset;
  size_t        id1_s = params[cblc_index].size  ;
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

        const cblc_field_t *id5   = params[cblc_index].field ;
        size_t              id5_o = params[cblc_index].offset;
        size_t              id5_s = params[cblc_index].size  ;
        cblc_index += 1;

        const cblc_field_t *id4_before   = params[cblc_index].field ;
        size_t              id4_before_o = params[cblc_index].offset;
        size_t              id4_before_s = params[cblc_index].size  ;
        cblc_index += 1;

        const cblc_field_t *id4_after   = params[cblc_index].field ;
        size_t              id4_after_o = params[cblc_index].offset;
        size_t              id4_after_s = params[cblc_index].size  ;
        cblc_index += 1;

        next_comparand.identifier_5
          = normalize_id(id5, id5_o, id5_s, id1->encoding);
        normalized_operand normalized_id_4_before
          = normalize_id(id4_before,
                         id4_before_o,
                         id4_before_s,
                         id1->encoding);
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
        // We have some number of identifier-3/identifier-5 pairs,
        // each with possible PHRASE1 modifiers.
        size_t pair_count = integers[int_index++];

        for(size_t k=0; k<pair_count; k++)
          {
          comparand next_comparand = {};
          next_comparand.operation = operation;

          const cblc_field_t *id3   = params[cblc_index].field ;
          size_t              id3_o = params[cblc_index].offset;
          size_t              id3_s = params[cblc_index].size  ;
          cblc_index += 1;

          const cblc_field_t *id5   = params[cblc_index].field ;
          size_t              id5_o = params[cblc_index].offset;
          size_t              id5_s = params[cblc_index].size  ;
          cblc_index += 1;

          const cblc_field_t *id4_before   = params[cblc_index].field ;
          size_t              id4_before_o = params[cblc_index].offset;
          size_t              id4_before_s = params[cblc_index].size  ;
          cblc_index += 1;

          const cblc_field_t *id4_after   = params[cblc_index].field ;
          size_t              id4_after_o = params[cblc_index].offset;
          size_t              id4_after_s = params[cblc_index].size  ;
          cblc_index += 1;

          next_comparand.identifier_3
            = normalize_id(id3, id3_o, id3_s, id1->encoding);
          next_comparand.identifier_5
            = normalize_id(id5, id5_o, id5_s, id1->encoding);

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
            = normalize_id(id4_before,
                           id4_before_o,
                           id4_before_s,
                           id1->encoding);
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
      if( rightmost + comparands[k].identifier_3.length
          > comparands[k].omega_it )
        {
        // This can't be a match, because the rightmost
        // character of the comparand falls to the right
        // of the comparand's omega
        continue;
        }
      if( rightmost + comparands[k].identifier_3.length
          > the_end_of_the_world )
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
                  + comparands[k].identifier_3.length
                    * (comparands[k].leading_count + 1)
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
  // string shorter than the original.  (There is always the possibility that
  // a UTF-8 or UTF-16 codeset pair got replaced with a single character.) Do
  // this before calling __gg__converter, because both mapped_character and
  // __gg__iconverter use the same static buffer.
  unsigned char *id1_data = id1->data + id1_o;
  charmap->memset(id1_data, charmap->mapped_character(ascii_space), id1_s);

  // We've been working in UTF32; we convert back to the original id1 encoding.
  size_t bytes_converted;
  const char *converted = __gg__iconverter( HOST_32_ENCODING,
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
__gg__inspect_format_2( int backward,
                        size_t integers[],
                        const cblc_referlet_t *params)
  {
  if( backward )
    {
    return inspect_backward_format_2(integers, params);
    }
  size_t int_index = 0;
  size_t cblc_index = 0;

  // Reference the language specification for the meanings of identifier_X

  // id1 is the string being inspected
  cblc_field_t *id1   = params[cblc_index].field;
  size_t        id1_o = params[cblc_index].offset;
  size_t        id1_s = params[cblc_index].size;
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

        const cblc_field_t *id5   = params[cblc_index].field;
        size_t              id5_o = params[cblc_index].offset;
        size_t              id5_s = params[cblc_index].size;
        cblc_index += 1;

        const cblc_field_t *id4_before   = params[cblc_index].field;
        size_t              id4_before_o = params[cblc_index].offset;
        size_t              id4_before_s = params[cblc_index].size;
        cblc_index += 1;

        const cblc_field_t *id4_after   = params[cblc_index].field;
        size_t              id4_after_o = params[cblc_index].offset;
        size_t              id4_after_s = params[cblc_index].size;
        cblc_index += 1;

        next_comparand.identifier_5
          = normalize_id(id5, id5_o, id5_s, id1->encoding);
        normalized_operand normalized_id_4_before
          = normalize_id(id4_before,
                         id4_before_o,
                         id4_before_s,
                         id1->encoding);
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
        // We have some number of identifier-3/identifier-5 pairs,
        // each with possible PHRASE1 modifiers.
        size_t pair_count = integers[int_index++];

        for(size_t k=0; k<pair_count; k++)
          {
          comparand next_comparand = {};
          next_comparand.operation = operation;

          const cblc_field_t *id3   = params[cblc_index].field;
          size_t              id3_o = params[cblc_index].offset;
          size_t              id3_s = params[cblc_index].size;
          cblc_index += 1;

          const cblc_field_t *id5   = params[cblc_index].field;
          size_t              id5_o = params[cblc_index].offset;
          size_t              id5_s = params[cblc_index].size;
          cblc_index += 1;

          const cblc_field_t *id4_before   = params[cblc_index].field;
          size_t              id4_before_o = params[cblc_index].offset;
          size_t              id4_before_s = params[cblc_index].size;
          cblc_index += 1;

          const cblc_field_t *id4_after   = params[cblc_index].field;
          size_t              id4_after_o = params[cblc_index].offset;
          size_t              id4_after_s = params[cblc_index].size;
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
            = normalize_id(id4_before,
                           id4_before_o,
                           id4_before_s,
                           id1->encoding);
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
  // string shorter than the original.  (There is always the possibility that
  // a UTF-8 or UTF-16 codeset pair got replaced with a single character.) Do
  // this before calling __gg__converter, because both mapped_character and
  // __gg__iconverter use the same static buffer.
  unsigned char *id1_data = id1->data + id1_o;
  charmap->memset(id1_data, charmap->mapped_character(ascii_space), id1_s);

  // We've been working in UTF32; we convert back to the original id1 encoding.
  size_t bytes_converted;
  const char *converted = __gg__iconverter( HOST_32_ENCODING,
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
    charmap_t *charmap32 = __gg__get_charmap(HOST_32_ENCODING);

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
                              HOST_32_ENCODING,
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

  std::u32string str_input
    = normalize_for_inspect_format_4(input,
                                     input_offset,
                                     input_size,
                                     input->encoding);
  std::u32string str_original
    = normalize_for_inspect_format_4(original,
                                     original_offset,
                                     original_size,
                                     input->encoding);
  std::u32string str_replacement
    = normalize_for_inspect_format_4(replacement,
                                     replacement_offset,
                                     replacement_size,
                                     input->encoding);
  std::u32string str_after
    = normalize_for_inspect_format_4(after,
                                     after_offset,
                                     after_size,
                                     input->encoding);
  std::u32string str_before
    = normalize_for_inspect_format_4(before,
                                     before_offset,
                                     before_size,
                                     input->encoding);

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
  const char *converted = __gg__iconverter(HOST_32_ENCODING,
                                           input->encoding,
                                           str_input.data(),
                                           str_input.size()*width_of_utf32,
                                           &bytes_converted) ;

  // And move those characters into place in input:
  memcpy(input->data + input_offset,
         converted,
         std::min(bytes_converted, input_size));
  }



extern "C"
void
__gg__inspect_format_1_sbc( int backward,
                            size_t integers[],
                            const cblc_referlet_t *params)
  {
  // When this routine is called, we know we are working in a single-byte-coded
  // codeset like ASCII or EBCDIC.
  if( backward )
    {
    return inspect_backward_format_1(integers, params);
    }

  size_t int_index = 0;
  size_t cblc_index = 0;

  // Reference the language specification for the meanings of identifier_X

  // Pick up the number of identifier_2 loops in this INSPECT statement
  size_t n_identifier_2 = integers[int_index++];

  std::vector<id_2_result> id_2_results(n_identifier_2);

  // Pick up identifier_1, which is the string being inspected
  const cblc_field_t *id1   = params[cblc_index].field ;
  size_t              id1_o = params[cblc_index].offset;
  size_t              id1_s = params[cblc_index].size  ;
  cblc_index += 1;
  // normalize it, according to the language specification.
  std::string normalized_id_1
                         = normalize_id_sbc(id1, id1_o, id1_s, id1->encoding);

  std::vector<comparand_sbc> comparands;

  for(size_t i=0; i<n_identifier_2; i++)
    {
    // For each identifier_2, we pick up its value:

    id_2_results[i].id2   = params[cblc_index].field ;
    id_2_results[i].id2_o = params[cblc_index].offset;
    id_2_results[i].id2_s = params[cblc_index].size  ;

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
          // but we we hard-code it to " " to set the length to 1.
          comparand_sbc next_comparand = {};
          next_comparand.id_2_index = i;
          next_comparand.operation = operation;
          next_comparand.identifier_3 = " ";

          const cblc_field_t *id4_before   = params[cblc_index].field ;
          size_t              id4_before_o = params[cblc_index].offset;
          size_t              id4_before_s = params[cblc_index].size  ;
          cblc_index += 1;

          const cblc_field_t *id4_after   = params[cblc_index].field ;
          size_t              id4_after_o = params[cblc_index].offset;
          size_t              id4_after_s = params[cblc_index].size  ;
          cblc_index += 1;

          std::string normalized_id_4_before
                                      = normalize_id_sbc( id4_before,
                                                          id4_before_o,
                                                          id4_before_s,
                                                          id1->encoding);
          std::string normalized_id_4_after
                                      = normalize_id_sbc( id4_after,
                                                          id4_after_o,
                                                          id4_after_s,
                                                          id1->encoding);
          the_alpha_and_omega_sbc(normalized_id_4_before,
                                  normalized_id_4_after,
                                  normalized_id_1,
                                  next_comparand.alpha,
                                  next_comparand.omega);

          comparands.push_back(next_comparand);
          break;
          }

        default:
          {
          // We have some number of identifier-3 values,
          // each with possible PHRASE1 modifiers.
          size_t pair_count = integers[int_index++];

          // We need to build up pair_count comparand structures:

          for(size_t k=0; k<pair_count; k++)
            {
            comparand_sbc next_comparand = {};
            next_comparand.id_2_index = i;
            next_comparand.operation = operation;

            const cblc_field_t *id3   = params[cblc_index].field ;
            size_t              id3_o = params[cblc_index].offset;
            size_t              id3_s = params[cblc_index].size  ;
            cblc_index += 1;

            const cblc_field_t *id4_before   = params[cblc_index].field ;
            size_t              id4_before_o = params[cblc_index].offset;
            size_t              id4_before_s = params[cblc_index].size  ;
            cblc_index += 1;

            const cblc_field_t *id4_after   = params[cblc_index].field ;
            size_t              id4_after_o = params[cblc_index].offset;
            size_t              id4_after_s = params[cblc_index].size  ;
            cblc_index += 1;

            next_comparand.identifier_3 = normalize_id_sbc(id3,
                                                           id3_o,
                                                           id3_s,
                                                           id1->encoding);
            std::string normalized_id_4_before
                                          = normalize_id_sbc( id4_before,
                                                              id4_before_o,
                                                              id4_before_s,
                                                              id1->encoding);
            std::string normalized_id_4_after
                                          = normalize_id_sbc( id4_after,
                                                              id4_after_o,
                                                              id4_after_s,
                                                              id1->encoding);
            the_alpha_and_omega_sbc(normalized_id_4_before,
                                    normalized_id_4_after,
                                    normalized_id_1,
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
  size_t leftmost = 0;
  size_t rightmost = leftmost + normalized_id_1.length();

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
      if( leftmost + comparands[k].identifier_3.length() > comparands[k].omega)
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
        for(size_t m=0; m<comparands[k].identifier_3.length(); m++)
          {
          if( comparands[k].identifier_3[m] != normalized_id_1[leftmost+m] )
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
              size_t count = ((leftmost - comparands[k].alpha))
                              / comparands[k].identifier_3.length();
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
                    % comparands[k].identifier_3.length() == 0 )
              {
              // The remaining number of characters is correct for a match.
              // Keep checking.

              // Assume a match until we learn otherwise:
              match = true;
              size_t local_left = leftmost;
              local_left += comparands[k].identifier_3.length();
              while( match && local_left < comparands[k].omega )
                {
                for(size_t m=0; m<comparands[k].identifier_3.length(); m++)
                  {
                  if( comparands[k].identifier_3[m] 
                                            != normalized_id_1[local_left+m] )
                    {
                    // We have a mismatched character, so no trailing match is
                    // possible
                    match = false;
                    break;
                    }
                  }
                local_left += comparands[k].identifier_3.length();
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
          leftmost += comparands[k].identifier_3.length() - 1;
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
    int128 id_2_value;
    __gg__int128_from_qualified_field(id_2_value,
                                      id_2_results[i].id2,
                                      id_2_results[i].id2_o,
                                      id_2_results[i].id2_s);
    while(id_2_value.rdigits--)
      {
      id_2_value.i128 /= 10.0;
      }

    // Accumulate what we've found into it
    id_2_value.i128 += id_2_results[i].result;

    // And put it back:
    __gg__int128_to_qualified_field(id_2_results[i].id2,
                                    id_2_results[i].id2_o,
                                    id_2_results[i].id2_s,
                                    id_2_value.i128,
                                    0,
                                    truncation_e,
                                    NULL);
    }
  }
