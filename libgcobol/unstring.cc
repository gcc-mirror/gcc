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
#include <limits>

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

static inline char *
as_chars(unsigned char *p)
  {
  return reinterpret_cast<char *>(p);
  }

template <typename T>
static T
load_unaligned(const void *p)
  {
  T value;
  memcpy(&value, p, sizeof(value));
  return value;
  }

// The compiler ensures that every operand uses the target encoding, so an
// ordinary operand can be used directly.
static inline const char *
string_operand(const cblc_referlet_t &refer,
               charmap_t             *charmap,
               size_t                 stride,
               char                   figurative[4],
               size_t                *size)
  {
  const cblc_field_t *field = refer.field;
  if( !field )
    {
    *size = 0;
    return figurative;
    }

  cbl_figconst_t figconst =
                  static_cast<cbl_figconst_t>(field->attr & FIGCONST_MASK);
  // We have a corner case to deal with:
  if(    !figconst
      && field->name[0] == 'N'
      && strcmp(field->name, "NULLS") == 0 )
    {
    figconst = null_value_e;
    }

  if( figconst )
    {
    cbl_char_t figchar = '\0';
    switch( figconst )
      {
      case low_value_e   :
        figchar = charmap->low_value_character();
        break;
      case zero_value_e  :
        figchar = charmap->mapped_character(ascii_0);
        break;
      case space_value_e :
        figchar = charmap->mapped_character(ascii_space);
        break;
      case quote_value_e :
        figchar = charmap->quote_character();
        break;
      case high_value_e  :
        figchar = charmap->high_value_character();
        break;
      case null_value_e:
        break;
      default:
        abort();
        break;
      }
    charmap->putch(figchar, figurative, size_t(0));
    *size = stride;
    return figurative;
    }

  *size = refer.size;
  return as_chars(field->data + refer.offset);
  }

extern "C"
int
__gg__string_1(const size_t integers[], const cblc_referlet_t *ref)
  {
  // The first integer is the count of identifier-2 values.  Call it N
  // The following N integers are the counts of each of the identifier-1
  // values,
  // one for each identifier-1.  Call them M.

  // The first refer is the target
  // The second refer is the pointer
  // The third refer is identifier-2 for N1
  // That's followed by M1 identifier-1 values
  // That's followed by identifier2 for N2
  // And so on

  static const int INDEX_OF_POINTER = 1;

  size_t index_cblc = 0 ;

  // Pick up the target
  const cblc_field_t *tgt = ref[index_cblc].field;

  // Pick up the target encoding, which according to the ISO specification
  // controls all the parameters.
  cbl_encoding_t tgt_encoding = tgt->encoding;
  charmap_t *charmap = __gg__get_charmap(tgt_encoding);

  // Pick up the rest of the parameters
  size_t tgt_o = ref[index_cblc].offset;
  size_t tgt_s = ref[index_cblc].size;
  index_cblc += 1;

  char  *dest         = as_chars(tgt->data + tgt_o);
  size_t dest_length = tgt_s;

  // Skip over the index of POINTER:
  index_cblc += 1;

  // Pick up the pointer, if any
  size_t pointer = 0;
  int overflow = 0;
  if( ref[INDEX_OF_POINTER].field )
    {
    __int128 p = __gg__int128_from_qualified_field(
                                              ref[INDEX_OF_POINTER].field,
                                              ref[INDEX_OF_POINTER].offset,
                                              ref[INDEX_OF_POINTER].size);
    if( p < 1 || static_cast<unsigned __int128>(p) > dest_length )
      {
      overflow = 1;
      }
    else
      {
      pointer = static_cast<size_t>(p - 1);
      }
    }

  // Make sure that the destination pointer is within the destination
  if( !overflow && pointer < dest_length )
    {
    // We are go for looping through identifier-2 values:

    size_t index_int  = 0;

    // Pick up the number of identifier-2 values
    size_t N = integers[index_int++];

    std::string delimiter;
    for( size_t i=0; i<N; i++ )
      {
      // Pick up the number of M identifier-1 values for this list of
      // identifier-2 values:
      size_t M = integers[index_int++];

      // Pick up the identifier_2 DELIMITED BY value
      char fig_id2[4];
      size_t str_id2_size;
      const char *str_id2 = string_operand(ref[index_cblc],
                                            charmap,
                                            1,
                                            fig_id2,
                                            &str_id2_size);
      index_cblc += 1;

      // Preserve the delimiter in case it overlaps the target.
      delimiter.assign(str_id2, str_id2_size);

      for(size_t j=0; j<M; j++)
        {
        // Pick up the next id-1 source string for the current id-2 delimiter
        char fig_id1[4];
        size_t str_id1_size;
        const char *str_id1 = string_operand(ref[index_cblc],
                                              charmap,
                                              1,
                                              fig_id1,
                                              &str_id1_size);
        index_cblc += 1;

        size_t nfound;
        if( delimiter.empty() )
          {
          // No given delimiter means DELIMITED BY SIZE
          nfound = str_id1_size;
          }
        else if( delimiter.size() == 1 )
          {
          // A one-character delimiter is overwhelmingly the common case.
          const void *found = memchr(str_id1,
                                     static_cast<unsigned char>(delimiter[0]),
                                     str_id1_size);
          nfound = found
                 ? static_cast<size_t>(
                              static_cast<const char *>(found) - str_id1)
                 : str_id1_size;
          }
        else
          {
          // We have an id2, so we look for it inside id1
          const char *found = std::search(str_id1,
                                          str_id1 + str_id1_size,
                                          delimiter.begin(),
                                          delimiter.end());
          nfound = static_cast<size_t>(found - str_id1);
          }

        // We have found id2 inside id1 at location nfound.
        size_t available = dest_length - pointer;
        size_t count = std::min(nfound, available);
        memmove(dest + pointer, str_id1, count);
        pointer += count;
        if( count != nfound )
          {
          overflow = 1;
          }
        if( overflow )
          {
          break;
          }
        }
      if( overflow )
        {
        break;
        }
      }

    // Update the pointer, if there is one
    if( ref[INDEX_OF_POINTER].field )
      {
      __gg__int128_to_qualified_field(ref[INDEX_OF_POINTER].field,
                                      ref[INDEX_OF_POINTER].offset,
                                      ref[INDEX_OF_POINTER].size,
                                      (__int128)(pointer+1),
                                      0,
                                      truncation_e);
      }
    }
  else
    {
    // The initial pointer is not inside the destination
    overflow = 1;
    }

  return overflow;
  }

static size_t
find_string_24(const char *str_id1,
               size_t      str_id1_size,
               const char *str_id2,
               size_t      str_id2_size,
               size_t      stride)
  {
  size_t id1_characters = str_id1_size / stride;
  size_t id2_characters = str_id2_size / stride;

  if( id2_characters == 0 || id2_characters > id1_characters )
    {
    return id1_characters;
    }

  size_t last = id1_characters - id2_characters;
  if( stride == 2 )
    {
    uint16_t first = load_unaligned<uint16_t>(str_id2);
    for(size_t i=0; i<=last; i++)
      {
      const char *candidate = str_id1 + i * stride;
      if(    load_unaligned<uint16_t>(candidate) == first
          && (   id2_characters == 1
              || memcmp(candidate, str_id2, str_id2_size) == 0) )
        {
        return i;
        }
      }
    }
  else
    {
    uint32_t first = load_unaligned<uint32_t>(str_id2);
    for(size_t i=0; i<=last; i++)
      {
      const char *candidate = str_id1 + i * stride;
      if(    load_unaligned<uint32_t>(candidate) == first
          && (   id2_characters == 1
              || memcmp(candidate, str_id2, str_id2_size) == 0) )
        {
        return i;
        }
      }
    }
  return id1_characters;
  }

extern "C"
int
__gg__string_24(const size_t integers[], const cblc_referlet_t *ref)
  {
  // The first integer is the count of identifier-2 values.  Call it N
  // The following N integers are the counts of each of the identifier-1
  // values,
  // one for each identifier-1.  Call them M.

  // The first refer is the target
  // The second refer is the pointer
  // The third refer is identifier-2 for N1
  // That's followed by M1 identifier-1 values
  // That's followed by identifier2 for N2
  // And so on

  static const int INDEX_OF_POINTER = 1;

  size_t index_cblc = 0 ;

  // Pick up the target
  const cblc_field_t *tgt = ref[index_cblc].field;

  // Pick up the target encoding, which according to the ISO specification
  // controls all the parameters.
  cbl_encoding_t tgt_encoding = tgt->encoding;
  charmap_t *charmap = __gg__get_charmap(tgt_encoding);
  size_t stride = static_cast<size_t>(charmap->stride());

  // Pick up the rest of the parameters
  size_t tgt_o = ref[index_cblc].offset;
  size_t tgt_s = ref[index_cblc].size;
  index_cblc += 1;

  char  *dest        = as_chars(tgt->data + tgt_o);
  size_t dest_length = tgt_s / stride;

  // Skip over the index of POINTER:
  index_cblc += 1;

  // Pick up the pointer, if any
  size_t pointer = 0;
  int overflow = 0;
  if( ref[INDEX_OF_POINTER].field )
    {
    __int128 p = __gg__int128_from_qualified_field(
                                              ref[INDEX_OF_POINTER].field,
                                              ref[INDEX_OF_POINTER].offset,
                                              ref[INDEX_OF_POINTER].size);
    if( p < 1 || static_cast<unsigned __int128>(p) > dest_length )
      {
      overflow = 1;
      }
    else
      {
      pointer = static_cast<size_t>(p - 1);
      }
    }

  // Make sure that the destination pointer is within the destination
  if( !overflow && pointer < dest_length )
    {
    // We are go for looping through identifier-2 values:

    size_t index_int = 0;

    // Pick up the number of identifier-2 values
    size_t N = integers[index_int++];

    std::string delimiter;
    for(size_t i=0; i<N; i++)
      {
      // Pick up the number of M identifier-1 values for this list of
      // identifier-2 values:
      size_t M = integers[index_int++];

      // Pick up the identifier_2 DELIMITED BY value
      char fig_id2[4];
      size_t str_id2_size;
      const char *str_id2 = string_operand(ref[index_cblc],
                                            charmap,
                                            stride,
                                            fig_id2,
                                            &str_id2_size);
      index_cblc += 1;

      // Preserve the delimiter in case it overlaps the target.
      delimiter.assign(str_id2, str_id2_size);

      for(size_t j=0; j<M; j++)
        {
        // Pick up the next id-1 source string for the current id-2 delimiter
        char fig_id1[4];
        size_t str_id1_size;
        const char *str_id1 = string_operand(ref[index_cblc],
                                              charmap,
                                              stride,
                                              fig_id1,
                                              &str_id1_size);
        index_cblc += 1;

        size_t nfound;
        if( delimiter.empty() )
          {
          // No given delimiter means DELIMITED BY SIZE
          nfound = str_id1_size / stride;
          }
        else
          {
          // We have an id2, so we look for it inside id1
          nfound = find_string_24(str_id1,
                                  str_id1_size,
                                  delimiter.data(),
                                  delimiter.size(),
                                  stride);
          }

        // We have found id2 inside id1 at location nfound.
        size_t available = dest_length - pointer;
        size_t count = std::min(nfound, available);
        memmove(dest + pointer * stride,
                str_id1,
                count * stride);
        pointer += count;
        if( count != nfound )
          {
          overflow = 1;
          }
        if( overflow )
          {
          break;
          }
        }
      if( overflow )
        {
        break;
        }
      }

    // Update the pointer, if there is one
    if( ref[INDEX_OF_POINTER].field )
      {
      __gg__int128_to_qualified_field(ref[INDEX_OF_POINTER].field,
                                      ref[INDEX_OF_POINTER].offset,
                                      ref[INDEX_OF_POINTER].size,
                                      (__int128)(pointer+1),
                                      0,
                                      truncation_e);
      }
    }
  else
    {
    // The initial pointer is not inside the destination
    overflow = 1;
    }

  return overflow;
  }

extern "C"
int
__gg__string(const size_t integers[], const cblc_referlet_t *ref)
  {
  const charmap_t *charmap = __gg__get_charmap(ref[0].field->encoding);
  return charmap->stride() == 1
       ? __gg__string_1(integers, ref)
       : __gg__string_24(integers, ref);
  }

struct unstring_finder_1
  {
  static size_t
  find(const char *str_id1,
       size_t      str_id1_size,
       const char *str_id2,
       size_t      str_id2_size,
       size_t)
    {
    if( str_id2_size == 1 )
      {
      const void *found = memchr(str_id1,
                                 static_cast<unsigned char>(str_id2[0]),
                                 str_id1_size);
      return found
           ? static_cast<size_t>(static_cast<const char *>(found) - str_id1)
           : str_id1_size;
      }

    const char *found = std::search(str_id1,
                                    str_id1 + str_id1_size,
                                    str_id2,
                                    str_id2 + str_id2_size);
    return static_cast<size_t>(found - str_id1);
    }
  };

struct unstring_finder_24
  {
  static size_t
  find(const char *str_id1,
       size_t      str_id1_size,
       const char *str_id2,
       size_t      str_id2_size,
       size_t      stride)
    {
    return find_string_24(str_id1,
                          str_id1_size,
                          str_id2,
                          str_id2_size,
                          stride);
    }
  };

template <typename finder_t>
static int
unstring_fixed( const cblc_referlet_t *id2,
                const cblc_referlet_t *id4,
                const cblc_referlet_t *id5,
                const cblc_referlet_t *id6,
                const cblc_field_t *id1,        // The string being unstring
                size_t              id1_o,
                size_t              id1_s,
                size_t ndelimiteds,       // The number of DELIMITED entries
                const char *all_flags,    // ALL flags, one per ndelimiteds
                size_t nreceivers,        // The number of DELIMITER receivers
                cblc_field_t *id7,        // Character index, start and end
                size_t        id7_o,
                size_t        id7_s,
                cblc_field_t *id8,        // Count of identifier-4 updates
                size_t        id8_o,
                size_t        id8_s,
                size_t        stride_id1)
  {
  // The names of the parameters are based on the ISO 1989:2014 specification.

  // There are complexities because of figurative constants, including the
  // LOW-VALUE figurative constant, which precludes the use of string
  // operations that would be confused by embedded NUL characters.

  // For each delimiter, there is an identifier-4 receiver that must be
  // resolved.  Each might have an identifier-5 delimiter, and each might have
  // an identifier-6 count.

  // Initialize the state variables
  int overflow = 0;
  int tally = 0;
  size_t pointer = 1;
  size_t nreceiver;
  size_t left=0;
  size_t right=0;

  std::string str_id1;
  std::vector<std::string> delimiters;

  charmap_t *charmap_id1 = __gg__get_charmap(id1->encoding);

  if( id8  )
    {
    tally = (int)__gg__int128_from_qualified_field(id8,
                                                   id8_o,
                                                   id8_s);
    }

  if( id7 )
    {
    __int128 p = __gg__int128_from_qualified_field(id7,
                                                    id7_o,
                                                    id7_s);
    if(    p < 1
        || static_cast<unsigned __int128>(p)
                                      > std::numeric_limits<size_t>::max() )
      {
      // No characters were examined; leave all operands unchanged.
      return 1;
      }
    pointer = static_cast<size_t>(p);
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

  /* The earlier implementation converted everything to UTF32.  The
     fixed-width entry points handle their encodings directly. */

  // Preserve identifier-1 because a receiving item can overlap it.
  str_id1.assign(as_chars(id1->data + id1_o), id1_s);
  left = pointer-1;
  right = str_id1.size() / stride_id1;
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
      size_t id_4_size = id4[receiver].size/stride_id1;
      if( id4[receiver].field->attr & separate_e )
        {
        // The receiver is NumericDisplay with a separate sign, so, as per
        // the spec, we reduce the size by one character.
        if( id_4_size )
          {
          id_4_size -= 1;
          }
        }

      // Make sure id_4_size doesn't take us past the end of the universe
      if( left + id_4_size > right )
        {
        id_4_size = right - left;
        }

      // Put the specified string into place:
      __gg__move_literala(id4[receiver].field,
                          id4[receiver].offset,
                          id4[receiver].size,
                          truncation_e,
                          str_id1.data() + left * stride_id1,
                          id_4_size * stride_id1);
      // Update the state variables:
      left += id_4_size;
      pointer += id_4_size;
      tally += 1;
      }
    goto done;
    }

  // Arriving here means there is some number of ndelimiteds

  // Preserve the delimiters because receiving items can overlap them.
  delimiters.reserve(ndelimiteds);
  for( size_t i=0; i<ndelimiteds; i++ )
    {
    char figurative[4];
    size_t delimiter_size;
    const char *delimiter = string_operand(id2[i],
                                            charmap_id1,
                                            stride_id1,
                                            figurative,
                                            &delimiter_size);
    delimiters.emplace_back(delimiter, delimiter_size);
    }

  nreceiver = 0;
  while( left < right )
    {
    // If we've used up all receivers, we bail at this point
    if( nreceiver >= nreceivers )
      {
      break;
      }

    // Starting at 'left', see if we can find any of the delimiters.  For each
    // 'left' position, we look through all of the delimiters,

    size_t best_delimiter = ndelimiteds;
    size_t best_leftmost = right; // This is the location of the start of ALL
    size_t best_location = right; // This is the location of the last of ALL
    for( size_t i=0; i<ndelimiteds; i++ )
      {
      const std::string &str_id2 = delimiters[i];
      if( str_id2.empty() )
        {
        continue;
        }
      size_t nfound = left + finder_t::find(
                                     str_id1.data() + left * stride_id1,
                                     (right - left) * stride_id1,
                                     str_id2.data(),
                                     str_id2.size(),
                                     stride_id1);
      if( nfound != right )
        {
        // We found a delimiter
        if( nfound >= best_leftmost )
          {
          // Keep the first listed delimiter when matches start at the same
          // position, and ignore matches to its right.
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
          size_t delimiter_characters = str_id2.size() / stride_id1;
          size_t next = nfound + delimiter_characters;
          while(    delimiter_characters <= right - next
                 && memcmp(str_id1.data() + next * stride_id1,
                           str_id2.data(),
                           str_id2.size()) == 0 )
            {
            // We found another consecutive one at next:
            best_location = next;
            next += delimiter_characters;
            }
          }
        }
      }

    if( best_delimiter == ndelimiteds )
      {
      // We were unable to find a delimiter, so we eat up the remainder
      // of the sender:
      best_leftmost = right;
      best_location = right;
      }

    // Apply what we have learned to the next receiver:

    size_t examined = best_leftmost - left;

    // Put the specified string into place:
    __gg__move_literala(id4[nreceiver].field,
                        id4[nreceiver].offset,
                        id4[nreceiver].size,
                        truncation_e,
                        str_id1.data() + left * stride_id1,
                        examined * stride_id1);
    // Update the left edge
    left = best_location + (best_delimiter != ndelimiteds
                            ? delimiters[best_delimiter].size() / stride_id1
                            : 0) ;
    if( id5[nreceiver].field )
      {
      // The caller wants to know what the delimiter was:
      if( best_delimiter != ndelimiteds )
        {
        __gg__move_literala(id5[nreceiver].field,
                            id5[nreceiver].offset,
                            id5[nreceiver].size,
                            truncation_e,
                            delimiters[best_delimiter].data(),
                            delimiters[best_delimiter].size());
        }
      else
        {
        // We didn't find a delimiter
        __gg__move_literala(id5[nreceiver].field,
                            id5[nreceiver].offset,
                            id5[nreceiver].size,
                            truncation_e,
                            "",
                            0);
        }
      }

    if( id6[nreceiver].field )
      {
      __gg__int128_to_qualified_field(id6[nreceiver].field,
                                      id6[nreceiver].offset,
                                      id6[nreceiver].size,
                                      (__int128)examined,
                                      0,
                                      truncation_e);
      }

    // Update the state variables:
    tally += 1;
    nreceiver += 1;
    pointer = left+1 ;
    }

done:

  if( id8 )
    {
    __gg__int128_to_qualified_field(id8,
                                    id8_o,
                                    id8_s,
                                    (__int128)tally,
                                    0,
                                    truncation_e);
    }

  if( id7 )
    {
    __gg__int128_to_qualified_field(id7,
                                    id7_o,
                                    id7_s,
                                    (__int128)pointer,
                                    0,
                                    truncation_e);
    }

  if( left < right )
    {
    overflow = 1;
    }

  return overflow;
  }

extern "C"
int
__gg__unstring_1( const cblc_referlet_t *id2,
                  const cblc_referlet_t *id4,
                  const cblc_referlet_t *id5,
                  const cblc_referlet_t *id6,
                  const cblc_field_t *id1,
                  size_t              id1_o,
                  size_t              id1_s,
                  size_t              ndelimiteds,
                  const char         *all_flags,
                  size_t              nreceivers,
                  cblc_field_t       *id7,
                  size_t              id7_o,
                  size_t              id7_s,
                  cblc_field_t       *id8,
                  size_t              id8_o,
                  size_t              id8_s)
  {
  return unstring_fixed<unstring_finder_1>(id2,
                                           id4,
                                           id5,
                                           id6,
                                           id1,
                                           id1_o,
                                           id1_s,
                                           ndelimiteds,
                                           all_flags,
                                           nreceivers,
                                           id7,
                                           id7_o,
                                           id7_s,
                                           id8,
                                           id8_o,
                                           id8_s,
                                           1);
  }

extern "C"
int
__gg__unstring_24( const cblc_referlet_t *id2,
                   const cblc_referlet_t *id4,
                   const cblc_referlet_t *id5,
                   const cblc_referlet_t *id6,
                   const cblc_field_t *id1,
                   size_t              id1_o,
                   size_t              id1_s,
                   size_t              ndelimiteds,
                   const char         *all_flags,
                   size_t              nreceivers,
                   cblc_field_t       *id7,
                   size_t              id7_o,
                   size_t              id7_s,
                   cblc_field_t       *id8,
                   size_t              id8_o,
                   size_t              id8_s)
  {
  size_t stride = static_cast<size_t>(
                         __gg__get_charmap(id1->encoding)->stride());
  return unstring_fixed<unstring_finder_24>(id2,
                                            id4,
                                            id5,
                                            id6,
                                            id1,
                                            id1_o,
                                            id1_s,
                                            ndelimiteds,
                                            all_flags,
                                            nreceivers,
                                            id7,
                                            id7_o,
                                            id7_s,
                                            id8,
                                            id8_o,
                                            id8_s,
                                            stride);
  }

extern "C"
int
__gg__unstring( const cblc_referlet_t *id2,
                const cblc_referlet_t *id4,
                const cblc_referlet_t *id5,
                const cblc_referlet_t *id6,
                const cblc_field_t *id1,
                size_t              id1_o,
                size_t              id1_s,
                size_t              ndelimiteds,
                const char         *all_flags,
                size_t              nreceivers,
                cblc_field_t       *id7,
                size_t              id7_o,
                size_t              id7_s,
                cblc_field_t       *id8,
                size_t              id8_o,
                size_t              id8_s)
  {
  const charmap_t *charmap = __gg__get_charmap(id1->encoding);
  if( charmap->stride() == 1 )
    {
    return __gg__unstring_1(id2,
                            id4,
                            id5,
                            id6,
                            id1,
                            id1_o,
                            id1_s,
                            ndelimiteds,
                            all_flags,
                            nreceivers,
                            id7,
                            id7_o,
                            id7_s,
                            id8,
                            id8_o,
                            id8_s);
    }
  return __gg__unstring_24(id2,
                           id4,
                           id5,
                           id6,
                           id1,
                           id1_o,
                           id1_s,
                           ndelimiteds,
                           all_flags,
                           nreceivers,
                           id7,
                           id7_o,
                           id7_s,
                           id8,
                           id8_o,
                           id8_s);
  }
