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

/*  This routine evolved from a primitive binary-to-string routine that simply
    peeled digits off the bottom of an __int128 by using

        value % 10 + '0';
        value /= 10;

    That turns out to be unnecessarily slow.

    The routine implemented here uses a divide-and-conquer approach to
    minimimizing the number of operations, and when you get down to two
    digits it does a divide-by-100 and uses the remainder in a table lookup
    to get the digits. */

/*  These static tables are born of a pathologic desire to avoid calculations.
    Whether that paranoia is justified (perhaps "digit%10 + '0';" ) would
    actually be faster) is currently untested.  But I figured this would be
    pretty darn fast.

    Use them when you know the index is between zero and one hundred.  */

static const char digit_low[100] =
  {
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
  };

static const char digit_high[100] =
  {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  };

static char combined_string[128];
static char zero_char;

typedef struct
  {
  int   start;
  int   run;
  union
    {
    unsigned __int128 val128;
    uint64_t          val64;
    uint32_t          val32;
    uint16_t          val16;
    uint8_t           val8;
    };
  } COMBINED;

static
void
string_from_combined(const COMBINED &combined)
  {
  COMBINED left;
  COMBINED right;

  uint16_t v16;

  switch(combined.run)
    {
    case 1:
      // We know that val8 is a single digit
      combined_string[combined.start] = combined.val8 + zero_char;
      break;

    case 2:
      // We know that val8 has two digits
      combined_string[combined.start]   = digit_high[combined.val8] + zero_char;
      combined_string[combined.start+1] = digit_low [combined.val8] + zero_char;
      break;

    case 3:
      // We know that val16 has three digits.
      v16 = combined.val16;
      combined_string[combined.start] = v16 / 100 + zero_char;
      v16 %= 100;
      combined_string[combined.start+1] = v16 / 10 + zero_char;
      combined_string[combined.start+2] = v16 % 10 + zero_char;
      break;

    case 4:
      // We know that val16 has four digits:
      v16 = combined.val16;
      combined_string[combined.start] = v16 / 1000 + zero_char;
      v16 %= 1000;
      combined_string[combined.start+1] = v16 / 100 + zero_char;
      v16 %= 100;
      combined_string[combined.start+2] = v16 / 10 + zero_char;
      combined_string[combined.start+3] = v16 % 10 + zero_char;
      break;

    case 5:
    case 6:
    case 7:
    case 8:
      // We know that val32 can be treated as two 4-digit pieces
      left.start  = combined.start;
      left.run    = combined.run - 4;
      left.val16  = combined.val32 / 10000;

      right.start = combined.start+left.run;
      right.run   =                4;
      right.val16 = combined.val32 % 10000;

      string_from_combined(left);
      string_from_combined(right);
      break;

    case 9:
      // We break val32 into a 1-digit piece, and an 8-digit piece:
      left.start  = combined.start;
      left.run    = combined.run - 8;
      left.val32  = combined.val32 / 100000000;

      right.start = combined.start+left.run;
      right.run   =                8;
      right.val32 = combined.val32 % 100000000;

      string_from_combined(left);
      string_from_combined(right);
      break;

    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 15:
    case 16:
    case 17:
    case 18:
      // We know we can treat val64 as two 9-digit pieces:
      left.start  = combined.start;
      left.run    = combined.run - 9;
      left.val32  = combined.val64 / 1000000000;

      right.start = combined.start+left.run;
      right.run   =                9;
      right.val32 = combined.val64 % 1000000000;

      string_from_combined(left);
      string_from_combined(right);
      break;

    case 19:
      // We split off the bottom nine digits
      left.start  = combined.start;
      left.run    = combined.run - 9;
      left.val64 = combined.val64 / 1000000000;

      right.start = combined.start+left.run;
      right.run   =                9;
      right.val32 = combined.val64 % 1000000000;

      string_from_combined(left);
      string_from_combined(right);
      break;

    default:
      // For twenty or more digits we peel eighteen digits at a time off the
      // right side:
      left.start  = combined.start;
      left.run    = combined.run - 18;
      left.val128 = combined.val128 / 1000000000000000000ULL;

      right.start = combined.start+left.run;
      right.run   =                18;
      right.val64 = combined.val128 % 1000000000000000000ULL;

      string_from_combined(left);
      string_from_combined(right);
      break;
    }
  }

bool
__gg__binary_to_string_ascii(char *result, int digits, __int128 value)
  {
  zero_char = ascii_zero;

  // Note that this routine does not terminate the generated string with a
  // NUL.  This routine is sometimes used to generate a NumericDisplay string
  // of digits in place, with no terminator.
  __int128 mask = __gg__power_of_ten(digits);

  COMBINED combined;
  if( value < 0 )
    {
    value = -value;
    }

  // A non-zero retval means the number was too big to fit into the desired
  // number of digits:
  bool retval = !!(value / mask);

  // mask off the bottom digits to avoid garbage when value is too large
  value %= mask;

  combined.start = 0;
  combined.run = digits;
  combined.val128 = value;
  string_from_combined(combined);
  memcpy(result, combined_string, digits);
  return retval;
  }

bool
__gg__binary_to_string_encoded( char *result,
                                size_t digits,
                                __int128 value,
                                cbl_encoding_t encoding)
  {
  // A non-zero retval means the number was too big to fit into the desired
  // number of digits.

  zero_char = ascii_0;

  // Note that this routine does not terminate the generated string with a
  // NUL.  This routine is sometimes used to generate a NumericDisplay string
  // of digits in place, with no terminator.
  __int128 mask = __gg__power_of_ten(digits);

  COMBINED combined;
  if( value < 0 )
    {
    value = -value;
    }

  bool retval = !!(value / mask);

  // mask off the bottom digits to avoid garbage when value is too large
  value %= mask;

  combined.start = 0;
  combined.run = digits;
  combined.val128 = value;
  string_from_combined(combined);
  size_t converted_bytes;
  const char *converted = __gg__iconverter(DEFAULT_SOURCE_ENCODING,
                                           encoding,
                                           combined_string,
                                           digits,
                                           &converted_bytes);
  memcpy(result, converted, converted_bytes);
  return retval;
  }

static
void
packed_from_combined(const COMBINED &combined)
  {
  /*  The combined.value must be positive at this point.

      The combined.run value has to be the number of places needed to hold
      combined.value.  The proper calculation is (digits+1)/2.

      For a signable value, the caller had to multiple the original value by
      ten to create room on the right for the sign nybble. */

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

  COMBINED left;
  COMBINED right;

  switch(combined.run)
    {
    case 1:
      // We know that val8 has two digits.
      combined_string[combined.start] = bin2pd[combined.val8];
      break;

    case 2:
      // We know that val16 has four digits.
      combined_string[combined.start  ] = bin2pd[combined.val16/100];
      combined_string[combined.start+1] = bin2pd[combined.val16%100];
      break;

    case 3:
    case 4:
      // We know that val32 can hold up to eight digits. Break it in half.
      left.start  = combined.start;
      left.run    = combined.run - 2;
      left.val16  = combined.val32 / 10000;

      right.start = combined.start+left.run;
      right.run   =                2;
      right.val16 = combined.val32 % 10000;

      packed_from_combined(left);
      packed_from_combined(right);
      break;

    case 5:
    case 6:
    case 7:
    case 8:
      // We know that val64 is holding up to 18 digits.  Break it into two
      // eight-digit places that can each go into a val23
      left.start  = combined.start;
      left.run    = combined.run - 4;
      left.val32  = combined.val64 / 100000000;

      right.start = combined.start+left.run;
      right.run   =                4;
      right.val32 = combined.val64 % 100000000;

      packed_from_combined(left);
      packed_from_combined(right);
      break;

    case 9:
      // We know that val64 is holding 17 or 18 digits.  Break off the
      // bottom eight.
      left.start  = combined.start;
      left.run    = combined.run - 4;
      left.val64  = combined.val64 / 100000000;

      right.start = combined.start+left.run;
      right.run   =                4;
      right.val32 = combined.val64 % 100000000;

      packed_from_combined(left);
      packed_from_combined(right);
      break;

    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 15:
    case 16:
    case 17:
    case 18:
      // We know that val64 is holding between 18 and 36 digits.  Break it
      // two val64:

      left.start  = combined.start;
      left.run    = combined.run - 9;
      left.val64  = combined.val128 / 1000000000000000000ULL;

      right.start = combined.start+left.run;
      right.run   =                9;
      right.val64 = combined.val128 % 1000000000000000000ULL;

      packed_from_combined(left);
      packed_from_combined(right);
      break;

    default:
      // For twenty or more digits we peel eighteen digits at a time off the
      // right side:
      left.start  = combined.start;
      left.run    = combined.run - 9;
      left.val128 = combined.val128 / 1000000000000000000ULL;

      right.start = combined.start+left.run;
      right.run   =                9;
      right.val64 = combined.val128 % 1000000000000000000ULL;

      packed_from_combined(left);
      packed_from_combined(right);
      break;
    }
  }

extern "C"
void
__gg__binary_to_packed( unsigned char *result,
                             int digits,
                             __int128 value)
  {
  size_t length = (digits+1)/2;

  COMBINED combined;
  combined.start = 0;
  combined.run = length;
  combined.val128 = value;
  packed_from_combined(combined);
  memcpy(result, combined_string, length);
  }

extern "C"
__int128
__gg__numeric_display_to_binary(unsigned char *signp,
                          const unsigned char *pdigits,
                                int            ndigits,
                                cbl_encoding_t encoding)
  {
  /*  This is specific to numeric display values.

      Such values can be unsigned, or they can have leading or trailing
      internal sign information, or they can have leading or trailing external
      sign information.

      In ASCII, digits are 030; internal sign is has the zone 0x70.

      In EBDIC, normal digits are 0xF0.  The sign byte in for a positive
      signable number has the zone 0xC0; a negative value has the zone 0xD0.

      A further complication is that it is legal for NumericDisplay values to
      have non-digit characters.  This is because of REDEFINES, and whatnot.
      Some COBOL implementations just look at the bottom four bits of
      characters regardless of their legality.  I am choosing to have non-legal
      characters come back as zero.  I do this with tables, so the cost is low.
      */

  /*  We are assuming that 64-bit arithmetic is faster than 128-bit arithmetic,
      and so we build up a 128-bit result in three 64-bit pieces, and assemble
      them at the end.  */
  size_t digit_index = 0;
  cbl_char_t ch;

  charmap_t *charmap = __gg__get_charmap(encoding);
  cbl_char_t minus = charmap->mapped_character(ascii_minus);

  bool is_ebcdic = charmap->is_like_ebcdic();

  static const uint8_t lookup[] =
    {
     0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0,0,0,0,0,0,
    10,11,12,13,14,15,16,17,18,19, 0,0,0,0,0,0,
    20,21,22,23,24,25,26,27,28,29, 0,0,0,0,0,0,
    30,31,32,33,34,35,36,37,38,39, 0,0,0,0,0,0,
    40,41,42,43,44,45,46,47,48,49, 0,0,0,0,0,0,
    50,51,52,53,54,55,56,57,58,59, 0,0,0,0,0,0,
    60,61,62,63,64,65,66,67,68,69, 0,0,0,0,0,0,
    70,71,72,73,74,75,76,77,78,79, 0,0,0,0,0,0,
    80,81,82,83,84,85,86,87,88,89, 0,0,0,0,0,0,
    90,91,92,93,94,95,96,97,98,99, 0,0,0,0,0,0,
    };

  static const uint8_t from_ebcdic[256] =
    {
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x00
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x10
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x20
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x30
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x40
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x50
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x60
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x70
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x80
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x90
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xa0
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xb0
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xc0
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xd0
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xe0
    0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0, // 0xf0
    };

  static const uint8_t from_ascii[256] =
    {
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x00
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x10
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x20
    0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0, // 0x30
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x40
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x50
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x60
    0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0, // 0x70
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x80
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x90
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xa0
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xb0
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xc0
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xd0
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xe0
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xf0
    };

  __int128 retval;

  uint64_t top = 0;
  uint64_t middle = 0;
  uint64_t bottom = 0;

  int count_bottom;
  int count_middle;
  int count_top;

  bool is_negative = false;

  // Pick up the original sign byte:
  cbl_char_t sign_byte = charmap->getch(signp, (size_t)0);

  const unsigned char *mapper;
  if( is_ebcdic )
    {
    mapper = from_ebcdic;
    if( sign_byte == minus )
      {
      is_negative = true;
      }
    else if( (sign_byte & 0xF0) == 0xD0 )
      {
      is_negative = true;
      }
    // No matter what the digit, force it to be a valid positive digit by
    // forcing the zone to 0xF0.  Note that this is harmless if redundant, and
    // harmless as well if the data SIGN IS SEPARATE.  Whatever we do to this
    // byte will be undone at the end of the routine.
    charmap->putch(sign_byte|0xF0, signp, (size_t)0);
    }
  else
    {
    mapper = from_ascii;
    if( sign_byte == minus )
      {
      is_negative = true;
      }
    else if( (sign_byte & 0xF0) == 0x70 )
      {
      is_negative = true;

      // Make it a valid positive digit by turning the zone to 0x30
      charmap->putch(sign_byte&0x3F, signp, (size_t)0);
      }
    }

  // Digits 1 through 18 come from the bottom:
  if( ndigits <= 18 )
    {
    count_bottom = ndigits;
    count_middle = 0;
    count_top = 0;
    }
  else if( ndigits<= 36 )
    {
    count_bottom = 18;
    count_middle = ndigits - 18;
    count_top = 0;
    }
  else
    {
    count_bottom = 18;
    count_middle = 18;
    count_top = ndigits - 36;
    }

  if( ndigits & 1 )
    {
    // We are dealing with an odd number of digits
    if( count_top )
      {
      ch = charmap->getch(pdigits, &digit_index);
      top = mapper[ch];
      count_top -= 1;
      }
    else if( count_middle )
      {
      ch = charmap->getch(pdigits, &digit_index);
      middle = mapper[ch];
      count_middle -= 1;
      }
    else
      {
      ch = charmap->getch(pdigits, &digit_index);
      bottom = mapper[ch];
      count_bottom -= 1;
      }
    }

  uint8_t add_me;

  while( count_top )
    {
    ch = charmap->getch(pdigits, &digit_index);
    add_me  = mapper[ch] << 4;
    ch = charmap->getch(pdigits, &digit_index);
    add_me += mapper[ch];
    top *= 100 ;
    top += lookup[add_me];
    count_top -= 2;
    }

  while( count_middle )
    {
    ch = charmap->getch(pdigits, &digit_index);
    add_me  = mapper[ch] << 4;
    ch = charmap->getch(pdigits, &digit_index);
    add_me += mapper[ch];
    middle *= 100 ;
    middle += lookup[add_me];
    count_middle -= 2;
    }

  while( count_bottom )
    {
    ch = charmap->getch(pdigits, &digit_index);
    add_me  = mapper[ch] << 4;
    ch = charmap->getch(pdigits, &digit_index);
    add_me += mapper[ch];
    bottom *= 100 ;
    bottom += lookup[add_me];
    count_bottom -= 2;
    }

  retval = top;
  retval *= 1000000000000000000ULL; // 10E18

  retval += middle;
  retval *= 1000000000000000000ULL;

  retval += bottom;

  if( is_negative )
    {
    retval = -retval;
    }

  // Replace the original sign byte:
  charmap->putch(sign_byte, signp, (size_t)0);
  return retval;
  }

extern "C"
__int128
__gg__packed_to_binary(const unsigned char *psz,
                             int            nplaces )
  {
  // See the comments in __gg__numeric_display_to_binary() above.

  __int128 retval = 0;

  static const unsigned char dp2bin[160] =
    {
    // This may not be the weirdest table I've ever created, but it is
    // certainly a contender.  Given the packed decimal byte 0x23, it
    // returns the equivalent decimal value of 23.  Note that the final
    // entries in each line are intended to handle the final place of
    // signed values.  0x2D, for example, gets picked up as 20.
    00, 01, 02, 03, 04, 05, 06, 07,  8,  9,  0,  0,  0,  0,  0,  0, // 0x00
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 10, 10, 10, 10, 10, 10, // 0x10
    20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 20, 20, 20, 20, 20, 20, // 0x20
    30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 30, 30, 30, 30, 30, 30, // 0x30
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 40, 40, 40, 40, 40, 40, // 0x40
    50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 50, 50, 50, 50, 50, 50, // 0x50
    60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 60, 60, 60, 60, 60, 60, // 0x60
    70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 70, 70, 70, 70, 70, 70, // 0x70
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 80, 80, 80, 80, 80, 80, // 0x80
    90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 90, 90, 90, 90, 90, 90, // 0x90
    };

  uint64_t top = 0;
  uint64_t middle = 0;
  uint64_t bottom = 0;

  int count_bottom;
  int count_middle;
  int count_top;

  // Turn places into n digits
  int n = nplaces * 2;

  // Digits 1 through 18 come from the bottom:
  if( n <= 18 )
    {
    count_bottom = n;
    count_middle = 0;
    count_top = 0;
    }
  else if( n<= 36 )
    {
    count_bottom = 18;
    count_middle = n - 18;
    count_top = 0;
    }
  else
    {
    count_bottom = 18;
    count_middle = 18;
    count_top = n - 36;
    }

  while( count_top )
    {
    top *= 100 ;
    top += dp2bin[*psz++];
    count_top -= 2;
    }

  while( count_middle )
    {
    middle *= 100 ;
    middle += dp2bin[*psz++];
    count_middle -= 2;
    }

  while( count_bottom )
    {
    bottom *= 100 ;
    bottom += dp2bin[*psz++];
    count_bottom -= 2;
    }

  retval = top;
  retval *= 1000000000000000000ULL; // 10E18

  retval += middle;
  retval *= 1000000000000000000ULL;

  retval += bottom;

  // retval is now the binary value of the packed decimal number.

  // back up one byte to fetch the sign nybble.
  uint8_t sign_nybble = *(psz-1) & 0x0F;
  enum{ PACKED_NYBBLE_MINUS= 0x0D};

  if( sign_nybble > 9 )
    {
    // There is a sign nybble.  We have to divide the result by ten to offset
    // left shift due place taken up by the sign nybble.
    retval /= 10;

    if( sign_nybble == PACKED_NYBBLE_MINUS )
      {
      retval = -retval ;
      }
    }

  return retval;
  }





