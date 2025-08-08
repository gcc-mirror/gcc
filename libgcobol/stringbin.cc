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
      combined_string[combined.start] = combined.val8 + zero_char;;
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
__gg__binary_to_string_internal(char *result, int digits, __int128 value)
  {
  zero_char = internal_zero;

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


static
void
packed_from_combined(COMBINED &combined)
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
