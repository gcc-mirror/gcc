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

    The routine implemented here splits wide values into large decimal chunks
    to minimize the number of divisions.  Within each chunk, it emits digits
    two at a time through a table lookup. */

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

template<int stride>
static inline void
uint_to_8_digits_direct( unsigned int   value,
                         unsigned char *result,
                         int            digits,
                         unsigned char  zero )
  {
  unsigned int pair;

  switch( digits )
    {
    case 8:
      pair = value % 100;
      result[6*stride] = digits2[pair][0] + zero;
      result[7*stride] = digits2[pair][1] + zero;
      value /= 100;
      FALLTHROUGH;

    case 6:
      pair = value % 100;
      result[4*stride] = digits2[pair][0] + zero;
      result[5*stride] = digits2[pair][1] + zero;
      value /= 100;
      FALLTHROUGH;

    case 4:
      pair = value % 100;
      result[2*stride] = digits2[pair][0] + zero;
      result[3*stride] = digits2[pair][1] + zero;
      value /= 100;
      FALLTHROUGH;

    case 2:
      /* The caller guarantees that value fits in digits, so the final pair
         is already in the range zero through 99. */
      result[0] = digits2[value][0] + zero;
      result[stride] = digits2[value][1] + zero;
      break;

    default:
      __builtin_unreachable();
    }
  }

template<int stride>
static inline void
string_from_uint64( unsigned char *result,
                    int            digits,
                    uint64_t       value,
                    unsigned char  zero )
  {
  if( digits & 0x01 )
    {
    result[(digits-1)*stride]
      = static_cast<unsigned char>(value % 10 + zero);

    if( digits == 1 )
      {
      return;
      }

    value /= 10;
    digits -= 1;
    }

  /* Leave the final one-to-eight digits in value.  Their value is already
     known to be less than 10^digits, so neither a remainder nor a final
     division is needed for that last group. */
  while( digits > 8 )
    {
    unsigned int chunk
      = static_cast<unsigned int>(value % 100000000);

    uint_to_8_digits_direct<stride>(
      chunk,
      result + (digits-8)*stride,
      8,
      zero);

    value /= 100000000;
    digits -= 8;
    }

  if( digits )
    {
    uint_to_8_digits_direct<stride>(
      static_cast<unsigned int>(value),
      result,
      digits,
      zero);
    }
  }

template<int stride>
static bool
binary_to_string( char          *result,
                  int            digits,
                  __int128       signed_value,
                  unsigned char  zero )
  {
  unsigned __int128 value
    = static_cast<unsigned __int128>(signed_value);

  if( signed_value < 0 )
    {
    /* Unsigned negation also handles the minimum signed __int128 value. */
    value = -value;
    }

  bool overflow = false;

  if( digits < 39 )
    {
    unsigned __int128 mask
      = static_cast<unsigned __int128>(__gg__power_of_ten(digits));

    overflow = value >= mask;

    /* Overflow should be uncommon.  Avoid 128-bit division entirely when
       the value already fits the requested number of digits. */
    if( overflow )
      {
      value %= mask;
      }
    }

  /* 10^19 is the largest power of ten that fits in uint64_t.  For values
     wider than 64 bits, one 128-bit division produces two pieces that can be
     formatted using only 64-bit and 32-bit arithmetic. */
  static const uint64_t ten_to_19 = 10000000000000000000ULL;
  unsigned char *output = reinterpret_cast<unsigned char *>(result);

  if( (value >> 64) == 0 )
    {
    string_from_uint64<stride>(
      output,
      digits,
      static_cast<uint64_t>(value),
      zero);
    }
  else
    {
    uint64_t low
      = static_cast<uint64_t>(value % ten_to_19);
    uint64_t high
      = static_cast<uint64_t>(value / ten_to_19);

    string_from_uint64<stride>(output, digits-19, high, zero);
    string_from_uint64<stride>(
      output+(digits-19)*stride,
      19,
      low,
      zero);
    }

  return overflow;
  }

extern "C"
bool
__gg__binary_to_string_ascii( char     *result,
                              int       digits,
                              __int128  value )
  {
  return binary_to_string<1>(result, digits, value, ascii_zero);
  }

extern "C"
bool
__gg__binary_to_string_ebcdic( char     *result,
                               int       digits,
                               __int128  value )
  {
  return binary_to_string<1>(result, digits, value, ebcdic_zero);
  }

bool
__gg__binary_to_string_encoded( char           *result,
                                size_t          digits,
                                __int128        value,
                                cbl_encoding_t  encoding )
  {
  const charmap_t *charmap = __gg__get_charmap(encoding);
  int stride = charmap->stride();
  unsigned char zero
    = charmap->is_like_ebcdic() ? ebcdic_zero : ascii_0;

  if( stride == 1 )
    {
    return binary_to_string<1>(
      result,
      static_cast<int>(digits),
      value,
      zero);
    }

  /* Clear the complete destination once, then write each digit directly into
     the byte selected by the encoding's byte order. */
  size_t output_size = digits * static_cast<size_t>(stride);
  memset(result, 0, output_size);

  size_t digit_offset
    = charmap->is_big_endian() ? static_cast<size_t>(stride-1) : 0;
  char *digit_result = result + digit_offset;

  if( stride == 2 )
    {
    return binary_to_string<2>(
      digit_result,
      static_cast<int>(digits),
      value,
      zero);
    }

  return binary_to_string<4>(
    digit_result,
    static_cast<int>(digits),
    value,
    zero);
  }

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
  };

static inline void
uint32_to_packed( unsigned char *result,
                  int            bytes,
                  uint32_t       value )
  {
  unsigned int pair;

  switch( bytes )
    {
    case 4:
      pair = value % 100;
      result[3] = bin2pd[pair];
      value /= 100;
      FALLTHROUGH;

    case 3:
      pair = value % 100;
      result[2] = bin2pd[pair];
      value /= 100;
      FALLTHROUGH;

    case 2:
      pair = value % 100;
      result[1] = bin2pd[pair];
      value /= 100;
      FALLTHROUGH;

    case 1:
      /* Retain the low-order pair when the source has more decimal digits than
         the destination. */
      result[0] = bin2pd[value % 100];
      break;

    default:
      __builtin_unreachable();
    }
  }

static inline void
uint64_to_packed( unsigned char *result,
                  int            bytes,
                  uint64_t       value )
  {
  /* Four packed bytes hold eight decimal digits.  Extracting four bytes at a
     time limits the divisions of value to one per four output bytes; the
     remaining divisions operate on 32-bit chunks. */
  while( bytes > 4 )
    {
    uint32_t chunk
      = static_cast<uint32_t>(value % 100000000);

    uint32_to_packed(result+bytes-4, 4, chunk);
    value /= 100000000;
    bytes -= 4;
    }

  if( bytes )
    {
    uint32_t final_chunk;

    if( value >> 32 )
      {
      /* Truncate in base ten before narrowing to uint32_t. */
      final_chunk = static_cast<uint32_t>(value % 100000000);
      }
    else
      {
      final_chunk = static_cast<uint32_t>(value);
      }

    uint32_to_packed(
      result,
      bytes,
      final_chunk);
    }
  }

static inline void
moderate_uint128_to_packed( unsigned char     *result,
                            int                bytes,
                            unsigned __int128  value )
  {
  unsigned char *d = result + bytes;

  /* GCC expands division of an unsigned __int128 by the constant 100 inline.
     For moderately wide values, peeling a few pairs this way is faster than
     invoking the general 128-bit division helper for a large divisor. */
  while( d > result && value >> 64 )
    {
    *(--d) = bin2pd[static_cast<unsigned int>(value % 100)];
    value /= 100;
    }

  uint64_to_packed(
    result,
    static_cast<int>(d-result),
    static_cast<uint64_t>(value));
  }

static inline void
large_uint128_to_packed( unsigned char     *result,
                         int                bytes,
                         unsigned __int128  value )
  {
  static const uint64_t ten_to_18 = 1000000000000000000ULL;
  static const uint64_t ten_to_19 = 10000000000000000000ULL;

  /* One division at 10^19 divides every supported packed value into two
     uint64_t values.  Since 19 is odd, one packed byte crosses the boundary
     between the two values. */
  uint64_t low
    = static_cast<uint64_t>(value % ten_to_19);
  uint64_t high
    = static_cast<uint64_t>(value / ten_to_19);

  /* low contains the low-order 19 decimal digits.  It therefore contains all
     the digits needed by a destination of nine bytes or fewer. */
  if( bytes <= 9 )
    {
    uint64_to_packed(result, bytes, low);
    return;
    }

  unsigned int low_leading_digit
    = static_cast<unsigned int>(low / ten_to_18);
  uint64_t low_trailing_digits = low % ten_to_18;

  unsigned int high_trailing_digit
    = static_cast<unsigned int>(high % 10);
  high /= 10;

  uint64_to_packed(result+bytes-9, 9, low_trailing_digits);
  result[bytes-10]
    = bin2pd[high_trailing_digit*10 + low_leading_digit];
  uint64_to_packed(result, bytes-10, high);
  }

extern "C"
void
__gg__binary_to_packed( unsigned char *result,
                        int            digits,
                        __int128       value )
  {
  /* The caller supplies a positive value.  For a signable item, it has
     already multiplied the magnitude by ten to reserve the low nybble for
     the sign. */
  unsigned __int128 magnitude
    = static_cast<unsigned __int128>(value);
  int bytes = (digits+1)/2;

  if( (magnitude >> 64) == 0 )
    {
    uint64_to_packed(result, bytes, static_cast<uint64_t>(magnitude));
    }
  else
    {
    /* This threshold is a performance choice, not a numeric boundary.  Below
       10^27, peeling a small number of pairs is faster on current GCC targets.
       At and above it, the single 10^19 split is faster. */
    static const unsigned __int128 ten_to_27
      =   static_cast<unsigned __int128>(10000000000000ULL)
        * 100000000000000ULL;

    if( magnitude < ten_to_27 )
      {
      moderate_uint128_to_packed(result, bytes, magnitude);
      }
    else
      {
      large_uint128_to_packed(result, bytes, magnitude);
      }
    }
  }

const unsigned char __gg__dp2bin[256] =
  {
  // This table is used both by the compile-time and the run-time.  Given the
  // packed decimal byte 0x23, it provides the equivalent decimal value of
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

static inline uint32_t
four_packed_bytes_to_uint32(const unsigned char *p)
  {
  return   static_cast<uint32_t>(__gg__dp2bin[p[0]]) * 1000000
         + static_cast<uint32_t>(__gg__dp2bin[p[1]]) * 10000
         + static_cast<uint32_t>(__gg__dp2bin[p[2]]) * 100
         + static_cast<uint32_t>(__gg__dp2bin[p[3]]);
  }

static inline uint32_t
initial_packed_bytes_to_uint32( const unsigned char *p,
                                int                  nplaces )
  {
  switch( nplaces )
    {
    case 1:
      return __gg__dp2bin[p[0]];

    case 2:
      return   __gg__dp2bin[p[0]] * 100
             + __gg__dp2bin[p[1]];

    case 3:
      return   __gg__dp2bin[p[0]] * 10000
             + __gg__dp2bin[p[1]] * 100
             + __gg__dp2bin[p[2]];

    case 4:
      return four_packed_bytes_to_uint32(p);

    default:
      __builtin_unreachable();
    }
  }

static inline uint64_t
packed_bytes_to_uint64( const unsigned char *p,
                        int                  nplaces )
  {
  int first = nplaces & 3;
  if( first == 0 )
    {
    first = 4;
    }

  uint64_t value = initial_packed_bytes_to_uint32(p, first);
  p += first;
  nplaces -= first;

  while( nplaces )
    {
    value = value * 100000000 + four_packed_bytes_to_uint32(p);
    p += 4;
    nplaces -= 4;
    }

  return value;
  }

static inline unsigned __int128
packed_bytes_to_uint128( const unsigned char *p,
                         int                  nplaces )
  {
  switch( nplaces )
    {
    case 0:
      return 0;

    case 1:
    case 2:
    case 3:
    case 4:
      return initial_packed_bytes_to_uint32(p, nplaces);

    default:
      break;
    }

  if( nplaces <= 9 )
    {
    return packed_bytes_to_uint64(p, nplaces);
    }

  /* The low nine packed bytes contain 18 decimal digits and fit in uint64_t.
     Decode both halves with 64-bit arithmetic, then combine them with one
     128-bit multiplication.  Only a 19-byte COMP-6 value has a ten-byte high
     portion; split that portion into one byte and nine bytes. */
  static const uint64_t ten_to_18 = 1000000000000000000ULL;
  int high_places = nplaces - 9;
  unsigned __int128 high;

  if( high_places <= 9 )
    {
    high = packed_bytes_to_uint64(p, high_places);
    }
  else
    {
    high =   static_cast<unsigned __int128>(__gg__dp2bin[p[0]]) * ten_to_18
           + packed_bytes_to_uint64(p+1, 9);
    }

  uint64_t low = packed_bytes_to_uint64(p+high_places, 9);
  return high * ten_to_18 + low;
  }

extern "C"
__int128
__gg__packed_to_binary( const unsigned char *psz,
                        int                  nplaces ) // Number of bytes
  {
  // Check to see if the final nybble is a sign bit:
  unsigned int sign = psz[nplaces-1] & 0x0F;
  bool signable = sign >= 0x0A;
  unsigned __int128 magnitude;

  if( signable )
    {
    magnitude =   packed_bytes_to_uint128(psz, nplaces-1) * 10
                + (psz[nplaces-1] >> 4);
    }
  else
    {
    magnitude = packed_bytes_to_uint128(psz, nplaces);
    }

  if( sign == 0x0B || sign == 0x0D )
    {
    return -static_cast<__int128>(magnitude);
    }
  return static_cast<__int128>(magnitude);
  }
