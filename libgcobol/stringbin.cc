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
  int   run;
  union
    {
    unsigned __int128 val128;
    };
  } COMBINED;

#if defined(__cplusplus) && __cplusplus >= 201703L
#  define FALLTHROUGH [[fallthrough]]
#elif defined(__GNUC__) && __GNUC__ >= 7
#  define FALLTHROUGH __attribute__((fallthrough))
#else
#  define FALLTHROUGH ((void)0)
#endif

static const unsigned char digits2[100][2] =
  {
  {0,0},{0,1},{0,2},{0,3},{0,4},{0,5},{0,6},{0,7},{0,8},{0,9},
  {1,0},{1,1},{1,2},{1,3},{1,4},{1,5},{1,6},{1,7},{1,8},{1,9},
  {2,0},{2,1},{2,2},{2,3},{2,4},{2,5},{2,6},{2,7},{2,8},{2,9},
  {3,0},{3,1},{3,2},{3,3},{3,4},{3,5},{3,6},{3,7},{3,8},{3,9},
  {4,0},{4,1},{4,2},{4,3},{4,4},{4,5},{4,6},{4,7},{4,8},{4,9},
  {5,0},{5,1},{5,2},{5,3},{5,4},{5,5},{5,6},{5,7},{5,8},{5,9},
  {6,0},{6,1},{6,2},{6,3},{6,4},{6,5},{6,6},{6,7},{6,8},{6,9},
  {7,0},{7,1},{7,2},{7,3},{7,4},{7,5},{7,6},{7,7},{7,8},{7,9},
  {8,0},{8,1},{8,2},{8,3},{8,4},{8,5},{8,6},{8,7},{8,8},{8,9},
  {9,0},{9,1},{9,2},{9,3},{9,4},{9,5},{9,6},{9,7},{9,8},{9,9}
  };

static void
uint_to_8_digits(unsigned int a, unsigned char *ach, int n)
  {
  unsigned int x;

  switch(n)
    {
    case 8:
      x = a % 100;
      ach[6] = digits2[x][0];
      ach[7] = digits2[x][1];
      a /= 100;
      FALLTHROUGH;

    case 7:
    case 6:
      x = a % 100;
      ach[4] = digits2[x][0];
      ach[5] = digits2[x][1];
      a /= 100;
      FALLTHROUGH;

    case 5:
    case 4:
      x = a % 100;
      ach[2] = digits2[x][0];
      ach[3] = digits2[x][1];
      a /= 100;
      FALLTHROUGH;

    case 3:
    case 2:
      x = a % 100;
      ach[0] = digits2[x][0];
      ach[1] = digits2[x][1];
      FALLTHROUGH;
    default:
      break;
    }
  }

static
void
string_from_combined(const COMBINED &combined)
  {
  int ndigits = combined.run;
  unsigned __int128 value = combined.val128;

  if( ndigits & 0x01 )
    {
    combined_string[ndigits-1] = value%10;
    value /= 10;
    ndigits -= 1;
    }
  while(ndigits >= 8)
    {
    unsigned int val = value % 100000000;
    uint_to_8_digits(val,
           reinterpret_cast<unsigned char *>(combined_string + ndigits-8), 8);
    value /= 100000000;
    ndigits -= 8;
    }
  if( ndigits )
    {
    const unsigned int pots[8] =
      {
      1,
      10,
      100,
      1000,
      10000,
      100000,
      1000000,
      10000000,
      };

    unsigned int val = value % pots[ndigits];
    uint_to_8_digits(val,
                  reinterpret_cast<unsigned char *>(combined_string), ndigits);
    value /= 100000000;
    }
  char *p = combined_string;
  const char *pend = p + combined.run;
  while(p < pend)
    {
    *p++ += zero_char;
    }
  }

static bool
binary_to_string(char *result, int digits, __int128 value)
  {
  bool retval; // True means the value was too big to fit into digits
  if( digits < 39 )
    {
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
    retval = !!(value / mask);

    // mask off the bottom digits to avoid garbage when value is too large
    value %= mask;

    combined.run = digits;
    combined.val128 = value;
    string_from_combined(combined);
    memcpy(result, combined_string, digits);
    return retval;
    }
  else
    {
    // We assume that this is a PIC X(16) COMP-X, so the value is always
    // positive.
    COMBINED combined;
    // A non-zero retval means the number was too big to fit into the desired
    // number of digits:
    retval = false;

    combined.run = digits;
    combined.val128 = value;
    string_from_combined(combined);
    memcpy(result, combined_string, digits);
    }
  return retval;
  }

extern "C"
bool
__gg__binary_to_string_ascii(char *result, int digits, __int128 value)
  {
  zero_char = ascii_zero;
  return binary_to_string(result, digits, value);
  }

extern "C"
bool
__gg__binary_to_string_ebcdic(char *result, int digits, __int128 value)
  {
  zero_char = ebcdic_zero;
  return binary_to_string(result, digits, value);
  }

bool
__gg__binary_to_string_encoded( char *result,
                                size_t digits,
                                __int128 value,
                                cbl_encoding_t encoding)
  {
  // A non-zero retval means the number was too big to fit into the desired
  // number of digits.

  const charmap_t *charmap = __gg__get_charmap(encoding);
  int stride = charmap->stride();

  zero_char = charmap->is_like_ebcdic() ? ebcdic_zero : ascii_0;

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

  combined.run = digits;
  combined.val128 = value;
  string_from_combined(combined);
  if( stride == 1 )
    {
    memcpy(result, combined_string, digits);
    }
  else
    {
    char *p = combined_string;
    const char *pend = p + digits;
    char *d = result;

    /* We take advantage of knowing that every character in combined_string
       becomes just the low byte of a little-endian int16 or 32, and the high
       byte of a big-endian.  */
    if( charmap->is_big_endian() )
      {
      while(p < pend)
        {
        memset(d, 0, stride-1);
        d += stride-1;
        *d++ = *p++;
        }
      }
    else
      {
      while(p < pend)
        {
        *d++ = *p++;
        memset(d, 0, stride-1);
        d += stride-1;
        }
      }
    }
  return retval;
  }

static void
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

  char *d = combined_string + combined.run;

  if( combined.run > 9)
    {
    // Stage 1: pull from __int128 until the top half is zero.
    __int128 value128 = combined.val128;
#if COBOL_LITTLE_ENDIAN
    while(value128>>64)
      {
      *(--d) = bin2pd[value128%100];
      value128 /= 100;
      }
    // Stage 2: Keep going with the 64-bit bottom half.
    uint64_t value64 = value128;
    while(d > combined_string)
      {
      *(--d) = bin2pd[value64%100];
      value64 /= 100;
      }
#else
    // The cute trick for little-endian is trickier in big-endian.  Right now
    // it's late, and I don't feel like it.  It would be easier if there were
    // __int128 constants, because the test up above could be
    //    while(value128/2^64)
    // but that's not available as of this writing.
    while(d > combined_string)
      {
      *(--d) = bin2pd[value128%100];
      value128 /= 100;
      }
#endif
    }
  else
    {
    uint64_t value = combined.val128;
    while(d > combined_string)
      {
      *(--d) = bin2pd[value%100];
      value /= 100;
      }
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
  combined.run = length;
  combined.val128 = value;
  packed_from_combined(combined);
  memcpy(result, combined_string, length);
  }

const unsigned char __gg__dp2bin[256] =
  {
  // This table is used both by the compile-time and the run-time.  Given the
  // packed decimal byte 0x23, it provides s the equivalent decimal value of
  // 23.  This table is not used on the final byte of COMP-3 values; that
  // digit has to be extracted specifically.

// 0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
//--------------------------------------------------------------
  00, 01, 02, 03, 04, 05, 06, 07,  8,  9,  0,  0,  0,  0,  0,  0, // 0x00
  10, 11, 12, 13, 14, 15, 16, 17, 18, 19,  0,  0,  0,  0,  0,  0, // 0x10
  20, 21, 22, 23, 24, 25, 26, 27, 28, 29,  0,  0,  0,  0,  0,  0, // 0x20
  30, 31, 32, 33, 34, 35, 36, 37, 38, 39,  0,  0,  0,  0,  0,  0, // 0x30
  40, 41, 42, 43, 44, 45, 46, 47, 48, 49,  0,  0,  0,  0,  0,  0, // 0x40
  50, 51, 52, 53, 54, 55, 56, 57, 58, 59,  0,  0,  0,  0,  0,  0, // 0x50
  60, 61, 62, 63, 64, 65, 66, 67, 68, 69,  0,  0,  0,  0,  0,  0, // 0x60
  70, 71, 72, 73, 74, 75, 76, 77, 78, 79,  0,  0,  0,  0,  0,  0, // 0x70
  80, 81, 82, 83, 84, 85, 86, 87, 88, 89,  0,  0,  0,  0,  0,  0, // 0x80
  90, 91, 92, 93, 94, 95, 96, 97, 98, 99,  0,  0,  0,  0,  0,  0, // 0x90
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 0xA0
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 0xB0
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 0xC0
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 0xD0
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 0xE0
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 0xF0
  };

static
__int128
pd_dive_rt(const unsigned char *psz, int nplaces)
  {
  __int128 retval;
  switch(nplaces)
    {
    case 0:
      retval =   0;
      break;
    case 1:
      retval =   __gg__dp2bin[psz[0]];
      break;
    case 2:
      retval =   __gg__dp2bin[psz[0]] * 100
               + __gg__dp2bin[psz[1]];
      break;
    case 3:
      retval =   __gg__dp2bin[psz[0]] * 10000
               + __gg__dp2bin[psz[1]] * 100
               + __gg__dp2bin[psz[2]];
      break;
    case 4:
      retval =   __gg__dp2bin[psz[0]] * 1000000
               + __gg__dp2bin[psz[1]] * 10000
               + __gg__dp2bin[psz[2]] * 100
               + __gg__dp2bin[psz[3]];
      break;
    default:
      {
      int nright = nplaces/2;
      int nleft  = nplaces - nright;
      __int128 pot = __gg__power_of_ten(nright*2);
      retval =   pd_dive_rt(psz,       nleft) * pot
               + pd_dive_rt(psz+nleft, nright);
      break;
      }
    }

  return retval;
  }

extern "C"
__int128
__gg__packed_to_binary(const unsigned char *psz,
                             int            nplaces) // Number of bytes
  {
  __int128 retval;
  // Check to see if the final nybble is a sign bit:
  bool signable = (psz[nplaces-1] & 0x0F) >= 0x0C;

  if( signable )
    {
    retval = pd_dive_rt(psz, nplaces-1) * 10 + (psz[nplaces-1] >> 4);
    }
  else
    {
    retval = pd_dive_rt(psz, nplaces);
    }
  if(     signable
      && (psz[nplaces-1] & 0x0F) == 0x0D )
    {
    retval = -retval;
    }
  return retval;
  }

