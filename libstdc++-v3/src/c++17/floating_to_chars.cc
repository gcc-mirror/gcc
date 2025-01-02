// std::to_chars implementation for floating-point types -*- C++ -*-

// Copyright (C) 2020-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <charconv>

#include <bit>
#include <cfenv>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
#if __has_include(<langinfo.h>)
# include <langinfo.h> // for nl_langinfo
#endif
#include <optional>
#include <string_view>
#include <type_traits>

#ifdef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
#ifndef __LONG_DOUBLE_IBM128__
#error "floating_to_chars.cc must be compiled with -mabi=ibmlongdouble"
#endif
// sprintf for __ieee128
extern "C" int __sprintfieee128(char*, const char*, ...);
#elif __FLT128_MANT_DIG__ == 113 && __LDBL_MANT_DIG__ != 113 \
      && defined(__GLIBC_PREREQ)
extern "C" int __strfromf128(char*, size_t, const char*, _Float128)
  __asm ("strfromf128")
#ifndef _GLIBCXX_HAVE_FLOAT128_MATH
  __attribute__((__weak__))
#endif
  ;
#endif

// This implementation crucially assumes float/double have the
// IEEE binary32/binary64 formats.
#if _GLIBCXX_FLOAT_IS_IEEE_BINARY32 && _GLIBCXX_DOUBLE_IS_IEEE_BINARY64 \
    /* And it also assumes that uint64_t POW10_SPLIT_2[3133][3] is valid.  */\
    && __SIZE_WIDTH__ >= 32

// Determine the binary format of 'long double'.

// We support the binary64, float80 (i.e. x86 80-bit extended precision),
// binary128, and ibm128 formats.
#define LDK_UNSUPPORTED 0
#define LDK_BINARY64    1
#define LDK_FLOAT80     2
#define LDK_BINARY128   3
#define LDK_IBM128      4

#if __LDBL_MANT_DIG__ == __DBL_MANT_DIG__
# define LONG_DOUBLE_KIND LDK_BINARY64
#elif __LDBL_MANT_DIG__ == 64
#  define LONG_DOUBLE_KIND LDK_FLOAT80
#elif __LDBL_MANT_DIG__ == 113
# define LONG_DOUBLE_KIND LDK_BINARY128
#elif __LDBL_MANT_DIG__ == 106
# define LONG_DOUBLE_KIND LDK_IBM128
#else
# define LONG_DOUBLE_KIND LDK_UNSUPPORTED
#endif

// For now we only support __float128 when it's the powerpc64 __ieee128 type.
#if defined _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT && __FLT128_MANT_DIG__ == 113
// Define overloads of std::to_chars for __float128.
# define FLOAT128_TO_CHARS 1
using F128_type = __float128;
#elif __FLT128_MANT_DIG__ == 113 && __LDBL_MANT_DIG__ != 113 \
      && defined(__GLIBC_PREREQ)
# define FLOAT128_TO_CHARS 1
using F128_type = _Float128;
#else
using F128_type = void;
#endif

#include <stdint.h>

namespace
{
#if defined __SIZEOF_INT128__
  using uint128_t = unsigned __int128;
#else
# include "uint128_t.h"
#endif

  namespace ryu
  {
#include "ryu/common.h"
#include "ryu/digit_table.h"
#include "ryu/d2s_intrinsics.h"
#include "ryu/d2s_full_table.h"
#include "ryu/d2fixed_full_table.h"
#include "ryu/f2s_intrinsics.h"
#include "ryu/d2s.c"
#include "ryu/d2fixed.c"
#include "ryu/f2s.c"

    namespace generic128
    {
      // Put the generic Ryu bits in their own namespace to avoid name conflicts.
# include "ryu/generic_128.h"
# include "ryu/ryu_generic_128.h"
# include "ryu/generic_128.c"
    } // namespace generic128

    using generic128::floating_decimal_128;
    using generic128::generic_binary_to_decimal;

    int
    to_chars(const floating_decimal_128 v, char* const result)
    { return generic128::generic_to_chars(v, result); }
  } // namespace ryu

  // A traits class that contains pertinent information about the binary
  // format of each of the floating-point types we support.
  template<typename T>
    struct floating_type_traits
    { };

  template<>
    struct floating_type_traits<float>
    {
      static constexpr int mantissa_bits = 23;
      static constexpr int exponent_bits = 8;
      static constexpr bool has_implicit_leading_bit = true;
      using mantissa_t = uint32_t;
      using shortest_scientific_t = ryu::floating_decimal_32;

      static constexpr uint64_t pow10_adjustment_tab[]
	= { 0b0000000000011101011100110101100101101110000000000000000000000000 };
    };

  template<>
    struct floating_type_traits<double>
    {
      static constexpr int mantissa_bits = 52;
      static constexpr int exponent_bits = 11;
      static constexpr bool has_implicit_leading_bit = true;
      using mantissa_t = uint64_t;
      using shortest_scientific_t = ryu::floating_decimal_64;

      static constexpr uint64_t pow10_adjustment_tab[]
	= { 0b0000000000000000000000011000110101110111000001100101110000111100,
	    0b0111100011110101011000011110000000110110010101011000001110011111,
	    0b0101101100000000011100100100111100110110110100010001010101110000,
	    0b0011110010111000101111110101100011101100010001010000000101100111,
	    0b0001010000011001011100100001010000010101101000001101000000000000 };
    };

#if LONG_DOUBLE_KIND == LDK_BINARY128 || defined FLOAT128_TO_CHARS
  // Traits for the IEEE binary128 format.
  struct floating_type_traits_binary128
  {
    static constexpr int mantissa_bits = 112;
    static constexpr int exponent_bits = 15;
    static constexpr bool has_implicit_leading_bit = true;
    using mantissa_t = uint128_t;
    using shortest_scientific_t = ryu::floating_decimal_128;

    static constexpr uint64_t pow10_adjustment_tab[]
      = { 0b0000000000000000000000000000000000000000000000000100000010000000,
	  0b1011001111110100000100010101101110011100100110000110010110011000,
	  0b1010100010001101111111000000001101010010100010010000111011110111,
	  0b1011111001110001111000011111000010110111000111110100101010100101,
	  0b0110100110011110011011000011000010011001110001001001010011100011,
	  0b0000011111110010101111101011101010000110011111100111001110100111,
	  0b0100010101010110000010111011110100000010011001001010001110111101,
	  0b1101110111000010001101100000110100000111001001101011000101011011,
	  0b0100111011101101010000001101011000101100101110010010110000101011,
	  0b0100000110111000000110101000010011101000110100010110000011101101,
	  0b1011001101001000100001010001100100001111011101010101110001010110,
	  0b1000000001000000101001110010110010001111101101010101001100000110,
	  0b0101110110100110000110000001001010111110001110010000111111010011,
	  0b1010001111100111000100011100100100111100100101000001011001000111,
	  0b1010011000011100110101100111001011100101111111100001110100000100,
	  0b1100011100100010100000110001001010000000100000001001010111011101,
	  0b0101110000100011001111101101000000100110000010010111010001111010,
	  0b0100111100011010110111101000100110000111001001101100000001111100,
	  0b1100100100111110101011000100000101011010110111000111110100110101,
	  0b0110010000010111010100110011000000111010000010111011010110000100,
	  0b0101001001010010110111010111000101011100000111100111000001110010,
	  0b1101111111001011101010110001000111011010111101001011010110100100,
	  0b0001000100110000011111101011001101110010110110010000000011100100,
	  0b0001000000000101001001001000000000011000100011001110101001001110,
	  0b0010010010001000111010011011100001000110011011011110110100111000,
	  0b0000100110101100000111100010100100011100110111011100001111001100,
	  0b1011111010001110001100000011110111111111100000001011111111101100,
	  0b0000011100001111010101110000100110111100101101110111101001000001,
	  0b1100010001110110111100001001001101101000011100000010110101001011,
	  0b0100101001101011111001011110101101100011011111011100101010101111,
	  0b0001101001111001110000101101101100001011010001011110011101000010,
	  0b1111000000101001101111011010110011101110100001011011001011100010,
	  0b0101001010111101101100001111100010010110001101001000001101100100,
	  0b0101100101011110001100101011111000111001111001001001101101100001,
	  0b1111001101010010100100011011000110110010001111000111010001001101,
	  0b0001110010011000000001000110110111011000011100001000011001110111,
	  0b0100001011011011011011110011101100100101111111101100101000001110,
	  0b0101011110111101010111100111101111000101111111111110100011011010,
	  0b1110101010001001110100000010110111010111111010111110100110010110,
	  0b1010001111100001001100101000110100001100011100110010000011010111,
	  0b1111111101101111000100111100000101011000001110011011101010111001,
	  0b1111101100001110100101111101011001000100000101110000110010100011,
	  0b1001010110110101101101000101010001010000101011011111010011010000,
	  0b0111001110110011101001100111000001000100001010110000010000001101,
	  0b0101111100111110100111011001111001111011011110010111010011101010,
	  0b1110111000000001100100111001100100110001011011001110101111110111,
	  0b0001010001001101010111101010011111000011110001101101011001111111,
	  0b0101000011100011010010001101100001011101011010100110101100100010,
	  0b0001000101011000100101111100110110000101101101111000110001001011,
	  0b0101100101001011011000010101000000010100011100101101000010011111,
	  0b1000010010001011101001011010100010111011110100110011011000100111,
	  0b1000011011100001010111010111010011101100100010010010100100101001,
	  0b1001001001010111110101000010111010000000101111010100001010010010,
	  0b0011011110110010010101111011000001000000000011011111000011111011,
	  0b1011000110100011001110000001000100000001011100010111010010011110,
	  0b0111101110110101110111110000011000000100011100011000101101101110,
	  0b1001100101111011011100011110101011001111100111101010101010110111,
	  0b1100110010010001100011001111010000000100011101001111011101001111,
	  0b1000111001111010100101000010000100000001001100101010001011001101,
	  0b0011101011110000110010100101010100110010100001000010101011111101,
	  0b1100000000000110000010101011000000011101000110011111100010111111,
	  0b0010100110000011011100010110111100010110101100110011101110001101,
	  0b0010111101010011111000111001111100110111111100100011110001101110,
	  0b1001110111001001101001001001011000010100110001000000100011010110,
	  0b0011110101100111011011111100001000011001010100111100100101111010,
	  0b0010001101000011000010100101110000010101101000100110000100001010,
	  0b0010000010100110010101100101110011101111000111111111001001100001,
	  0b0100111111011011011011100111111011000010011101101111011111110110,
	  0b1111111111010110101011101000100101110100001110001001101011100111,
	  0b1011111101000101110000111100100010111010100001010000010010110010,
	  0b1111010101001011101011101010000100110110001110111100100110111111,
	  0b1011001101000001001101000010101010010110010001100001011100011010,
	  0b0101001011011101010001110100010000010001111100100100100001001101,
	  0b0010100000111001100011000101100101000001111100111001101000000010,
	  0b1011001111010101011001000100100110100100110111110100000110111000,
	  0b0101011111010011100011010010111101110010100001111111100010001001,
	  0b0010111011101100100000000000001111111010011101100111100001001101,
	  0b1101000000000000000000000000000000000000000000000000000000000000 };
  };

# ifdef FLOAT128_TO_CHARS
  template<>
    struct floating_type_traits<F128_type> : floating_type_traits_binary128
    { };
# endif
#endif

#if LONG_DOUBLE_KIND == LDK_BINARY64
  // When long double is equivalent to double, we just forward the long double
  // overloads to the double overloads, so we don't need to define a
  // floating_type_traits<long double> specialization in this case.
#elif LONG_DOUBLE_KIND == LDK_FLOAT80
  template<>
    struct floating_type_traits<long double>
    {
      static constexpr int mantissa_bits = 64;
      static constexpr int exponent_bits = 15;
      static constexpr bool has_implicit_leading_bit = false;
      using mantissa_t = uint64_t;
      using shortest_scientific_t = ryu::floating_decimal_128;

      static constexpr uint64_t pow10_adjustment_tab[]
	= { 0b0000000000000000000000000000110101011111110100010100110000011101,
	    0b1001100101001111010011011111101000101111110001011001011101110000,
	    0b0000101111111011110010001000001010111101011110111111010100011001,
	    0b0011100000011111001101101011111001111100100010000101001111101001,
	    0b0100100100000000100111010010101110011000110001101101110011001010,
	    0b0111100111100010100000010011000010010110101111110101000011110100,
	    0b1010100111100010011110000011011101101100010110000110101010101010,
	    0b0000001111001111000000101100111011011000101000110011101100110010,
	    0b0111000011100100101101010100001101111110101111001000010011111111,
	    0b0010111000100110100100100010101100111010110001101010010111001000,
	    0b0000100000010110000011001001000111000001111010100101101000001111,
	    0b0010101011101000111100001011000010011101000101010010010000101111,
	    0b1011111011101101110010101011010001111000101000101101011001100011,
	    0b1010111011011011110111110011001010000010011001110100101101000101,
	    0b0011000001110110011010010000011100100011001011001100001101010110,
	    0b0100011111011000111111101000011110000010111110101001000000001001,
	    0b1110000001110001001101101110011000100000001010000111100010111010,
	    0b1110001001010011101000111000001000010100110000010110100011110000,
	    0b0000011010110000110001111000011111000011001101001101001001000110,
	    0b1010010111001000101001100101010110100100100010010010000101000010,
	    0b1011001110000111100010100110000011100011111001110111001100000101,
	    0b0110101001001000010110001000010001010101110101100001111100011001,
	    0b1111100011110101011110011010101001010010100011000010110001101001,
	    0b0100000100001000111101011100010011011111011001000000001100011000,
	    0b1110111111000111100101110111110000000011001110011100011011011001,
	    0b1100001100100000010001100011011000111011110000110011010101000011,
	    0b1111111011100111011101001111111000010000001111010111110010000100,
	    0b1110111001111110101111000101000000001010001110011010001000111010,
	    0b1000010001011000101111111010110011111101110101101001111000111010,
	    0b0100000111101001000111011001101000001010111011101001101111000100,
	    0b0000011100110001000111011100111100110001101111111010110111100000,
	    0b0000011101011100100110010011110101010100010011110010010111010000,
	    0b0011011001100111110101111100001001101110101101001110110011110110,
	    0b1011000101000001110100111001100100111100110011110000000001101000,
	    0b1011100011110100001001110101010110111001000000001011101001011110,
	    0b1111001010010010100000010110101010101011101000101000000000001100,
	    0b1000001111100100111001110101100001010011111111000001000011110000,
	    0b0001011101001000010000101101111000001110101100110011001100110111,
	    0b1110011100000010101011011111001010111101111110100000011100000011,
	    0b1001110110011100101010011110100010110001001110110000101011100110,
	    0b1001101000100011100111010000011011100001000000110101100100001001,
	    0b1010111000101000101101010111000010001100001010100011111100000100,
	    0b0111101000100011000101101011111011100010001101110111001111001011,
	    0b1110100111010110001110110110000000010110100011110000010001111100,
	    0b1100010100011010001011001000111001010101011110100101011001000000,
	    0b0000110001111001100110010110111010101101001101000000000010010101,
	    0b0001110111101000001111101010110010010000111110111100000111110100,
	    0b0111110111001001111000110001101101001010101110110101111110000100,
	    0b0000111110111010101111100010111010011100010110011011011001000001,
	    0b1010010100100100101110111111111000101100000010111111101101000110,
	    0b1000100111111101100011001101000110001000000100010101010100001101,
	    0b1100101010101000111100101100001000110001110010100000000010110101,
	    0b1010000100111101100100101010010110100010000000110101101110000100,
	    0b1011111011110001110000100100000000001010111010001101100000100100,
	    0b0111101101100011001110011100000001000101101101111000100111011111,
	    0b0100111010010011011001010011110100001100111010010101111111100011,
	    0b0010001001011000111000001100110111110111110010100011000110110110,
	    0b0101010110000000010000100000110100111011111101000100000111010010,
	    0b0110000011011101000001010100110101101110011100110101000000001001,
	    0b1101100110100000011000001111000100100100110001100110101010101100,
	    0b0010100101010110010010001010101000011111111111001011001010001111,
	    0b0111001010001111001100111001010101001000110101000011110000001000,
	    0b0110010011001001001111110001010010001011010010001101110110110011,
	    0b0110010100111011000100111000001001101011111001110010111110111111,
	    0b0101110111001001101100110100101001110010101110011001101110001000,
	    0b0100110101010111011010001100010111100011010011111001010100111000,
	    0b0111000110110111011110100100010111000110000110110110110001111110,
	    0b1000101101010100100100111110100011110110110010011001110011110101,
	    0b1001101110101001010100111101101011000101000010110101101111110000,
	    0b0100100101001011011001001011000010001101001010010001010110101000,
	    0b0010100001001011100110101000010110000111000111000011100101011011,
	    0b0110111000011001111101101011111010001000000010101000101010011110,
	    0b1000110110100001111011000001111100001001000000010110010100100100,
	    0b1001110100011111100111101011010000010101011100101000010010100110,
	    0b0001010110101110100010101010001110110110100011101010001001111100,
	    0b1010100101101100000010110011100110100010010000100100001110000100,
	    0b0001000000010000001010000010100110000001110100111001110111101101,
	    0b1100000000000000000000000000000000000000000000000000000000000000 };
    };
#elif LONG_DOUBLE_KIND == LDK_BINARY128
  template<>
    struct floating_type_traits<long double> : floating_type_traits_binary128
    { };
#elif LONG_DOUBLE_KIND == LDK_IBM128
  template<>
    struct floating_type_traits<long double>
    {
      static constexpr int mantissa_bits = 105;
      static constexpr int exponent_bits = 11;
      static constexpr bool has_implicit_leading_bit = true;
      using mantissa_t = uint128_t;
      using shortest_scientific_t = ryu::floating_decimal_128;

      static constexpr uint64_t pow10_adjustment_tab[]
	= { 0b0000000000000000000000000000000000000000000000001000000100000000,
	    0b0000000000000000000100000000000000000000001000000000000000000010,
	    0b0000100000000000000000001001000000000000000001100100000000000000,
	    0b0011000000000000000000000000000001110000010000000000000000000000,
	    0b0000100000000000001000000000000000000000000000100000000000000000 };
    };
#endif

  // Wrappers around float for std::{,b}float16_t promoted to float.
  struct floating_type_float16_t
  {
    float x;
    operator float() const { return x; }
  };
  struct floating_type_bfloat16_t
  {
    float x;
    operator float() const { return x; }
  };

  template<>
    struct floating_type_traits<floating_type_float16_t>
    {
      static constexpr int mantissa_bits = 10;
      static constexpr int exponent_bits = 5;
      static constexpr bool has_implicit_leading_bit = true;
      using mantissa_t = uint32_t;
      using shortest_scientific_t = ryu::floating_decimal_128;

      static constexpr uint64_t pow10_adjustment_tab[]
	= { 0 };
    };

  template<>
    struct floating_type_traits<floating_type_bfloat16_t>
    {
      static constexpr int mantissa_bits = 7;
      static constexpr int exponent_bits = 8;
      static constexpr bool has_implicit_leading_bit = true;
      using mantissa_t = uint32_t;
      using shortest_scientific_t = ryu::floating_decimal_128;

      static constexpr uint64_t pow10_adjustment_tab[]
	= { 0b0000111001110001101010010110100101010010000000000000000000000000 };
    };

  // An IEEE-style decomposition of a floating-point value of type T.
  template<typename T>
    struct ieee_t
    {
      typename floating_type_traits<T>::mantissa_t mantissa;
      uint32_t biased_exponent;
      bool sign;
    };

  // Decompose the floating-point value into its IEEE components.
  template<typename T>
    ieee_t<T>
    get_ieee_repr(const T value)
    {
      using mantissa_t = typename floating_type_traits<T>::mantissa_t;
      constexpr int mantissa_bits = floating_type_traits<T>::mantissa_bits;
      constexpr int exponent_bits = floating_type_traits<T>::exponent_bits;
      constexpr int total_bits = mantissa_bits + exponent_bits + 1;

      constexpr auto get_uint_t = [] {
	if constexpr (total_bits <= 32)
	  return uint32_t{};
	else if constexpr (total_bits <= 64)
	  return uint64_t{};
	else if constexpr (total_bits <= 128)
	  return uint128_t{};
      };
      using uint_t = decltype(get_uint_t());
      uint_t value_bits = 0;
      memcpy(&value_bits, &value, sizeof(value));

      ieee_t<T> ieee_repr;
      ieee_repr.mantissa
	= static_cast<mantissa_t>(value_bits & ((uint_t{1} << mantissa_bits) - 1u));
      value_bits >>= mantissa_bits;
      ieee_repr.biased_exponent
	= static_cast<uint32_t>(value_bits & ((uint_t{1} << exponent_bits) - 1u));
      value_bits >>= exponent_bits;
      ieee_repr.sign = (value_bits & 1) != 0;
      return ieee_repr;
    }

#if LONG_DOUBLE_KIND == LDK_IBM128
  template<>
    ieee_t<long double>
    get_ieee_repr(const long double value)
    {
      // The layout of __ibm128 isn't compatible with the standard IEEE format.
      // So we transform it into an IEEE-compatible format, suitable for
      // consumption by the generic Ryu API, with an 11-bit exponent and 105-bit
      // mantissa (plus an implicit leading bit).  We use the exponent and sign
      // of the high part, and we merge the mantissa of the high part with the
      // mantissa (and the implicit leading bit) of the low part.
      uint64_t value_bits[2] = {};
      memcpy(value_bits, &value, sizeof(value_bits));

      const uint64_t value_hi = value_bits[0];
      const uint64_t value_lo = value_bits[1];

      uint64_t mantissa_hi = value_hi & ((1ull << 52) - 1);
      unsigned exponent_hi = (value_hi >> 52) & ((1ull << 11) - 1);
      const int sign_hi = (value_hi >> 63) & 1;

      uint64_t mantissa_lo = value_lo & ((1ull << 52) - 1);
      const unsigned exponent_lo = (value_lo >> 52) & ((1ull << 11) - 1);
      const int sign_lo = (value_lo >> 63) & 1;

	{
	  // The following code for adjusting the low-part mantissa to combine
	  // it with the high-part mantissa is taken from the glibc source file
	  // sysdeps/ieee754/ldbl-128ibm/printf_fphex.c.
	  mantissa_lo <<= 7;
	  if (exponent_lo != 0)
	    mantissa_lo |= (1ull << (52 + 7));
	  else
	    mantissa_lo <<= 1;

	  const int ediff = exponent_hi - exponent_lo - 53;
	  if (ediff > 63)
	    mantissa_lo = 0;
	  else if (ediff > 0)
	    mantissa_lo >>= ediff;
	  else if (ediff < 0)
	    mantissa_lo <<= -ediff;

	  if (sign_lo != sign_hi && mantissa_lo != 0)
	    {
	      mantissa_lo = (1ull << 60) - mantissa_lo;
	      if (mantissa_hi == 0)
		{
		  mantissa_hi = 0xffffffffffffeLL | (mantissa_lo >> 59);
		  mantissa_lo = 0xfffffffffffffffLL & (mantissa_lo << 1);
		  exponent_hi--;
		}
	      else
		mantissa_hi--;
	    }
	}

      ieee_t<long double> ieee_repr;
      ieee_repr.mantissa = ((uint128_t{mantissa_hi} << 64)
			    | (uint128_t{mantissa_lo} << 4)) >> 11;
      ieee_repr.biased_exponent = exponent_hi;
      ieee_repr.sign = sign_hi;
      return ieee_repr;
    }
#endif

  template<>
    ieee_t<floating_type_float16_t>
    get_ieee_repr(const floating_type_float16_t value)
    {
      using mantissa_t = typename floating_type_traits<float>::mantissa_t;
      constexpr int mantissa_bits = floating_type_traits<float>::mantissa_bits;
      constexpr int exponent_bits = floating_type_traits<float>::exponent_bits;

      uint32_t value_bits = 0;
      memcpy(&value_bits, &value.x, sizeof(value));

      ieee_t<floating_type_float16_t> ieee_repr;
      ieee_repr.mantissa
	= static_cast<mantissa_t>(value_bits & ((uint32_t{1} << mantissa_bits) - 1u));
      value_bits >>= mantissa_bits;
      ieee_repr.biased_exponent
	= static_cast<uint32_t>(value_bits & ((uint32_t{1} << exponent_bits) - 1u));
      value_bits >>= exponent_bits;
      ieee_repr.sign = (value_bits & 1) != 0;
      // We have mantissa and biased_exponent from the float (originally
      // float16_t converted to float).
      // Transform that to float16_t mantissa and biased_exponent.
      // If biased_exponent is 0, then value is +-0.0.
      // If biased_exponent is 0x67..0x70, then it is a float16_t denormal.
      if (ieee_repr.biased_exponent >= 0x67
	  && ieee_repr.biased_exponent <= 0x70)
	{
	  int n = ieee_repr.biased_exponent - 0x67;
	  ieee_repr.mantissa = ((uint32_t{1} << n)
				| (ieee_repr.mantissa >> (mantissa_bits - n)));
	  ieee_repr.biased_exponent = 0;
	}
      // If biased_exponent is 0xff, then it is a float16_t inf or NaN.
      else if (ieee_repr.biased_exponent == 0xff)
	{
	  ieee_repr.mantissa >>= 13;
	  ieee_repr.biased_exponent = 0x1f;
	}
      // If biased_exponent is 0x71..0x8e, then it is a float16_t normal number.
      else if (ieee_repr.biased_exponent > 0x70)
	{
	  ieee_repr.mantissa >>= 13;
	  ieee_repr.biased_exponent -= 0x70;
	}
      return ieee_repr;
    }

  template<>
    ieee_t<floating_type_bfloat16_t>
    get_ieee_repr(const floating_type_bfloat16_t value)
    {
      using mantissa_t = typename floating_type_traits<float>::mantissa_t;
      constexpr int mantissa_bits = floating_type_traits<float>::mantissa_bits;
      constexpr int exponent_bits = floating_type_traits<float>::exponent_bits;

      uint32_t value_bits = 0;
      memcpy(&value_bits, &value.x, sizeof(value));

      ieee_t<floating_type_bfloat16_t> ieee_repr;
      ieee_repr.mantissa
	= static_cast<mantissa_t>(value_bits & ((uint32_t{1} << mantissa_bits) - 1u));
      value_bits >>= mantissa_bits;
      ieee_repr.biased_exponent
	= static_cast<uint32_t>(value_bits & ((uint32_t{1} << exponent_bits) - 1u));
      value_bits >>= exponent_bits;
      ieee_repr.sign = (value_bits & 1) != 0;
      // We have mantissa and biased_exponent from the float (originally
      // bfloat16_t converted to float).
      // Transform that to bfloat16_t mantissa and biased_exponent.
      ieee_repr.mantissa >>= 16;
      return ieee_repr;
    }

  // Invoke Ryu to obtain the shortest scientific form for the given
  // floating-point number.
  template<typename T>
    typename floating_type_traits<T>::shortest_scientific_t
    floating_to_shortest_scientific(const T value)
    {
      if constexpr (std::is_same_v<T, float>)
	return ryu::floating_to_fd32(value);
      else if constexpr (std::is_same_v<T, double>)
	return ryu::floating_to_fd64(value);
      else if constexpr (std::is_same_v<T, long double>
			 || std::is_same_v<T, F128_type>
			 || std::is_same_v<T, floating_type_float16_t>
			 || std::is_same_v<T, floating_type_bfloat16_t>)
	{
	  constexpr int mantissa_bits
	    = floating_type_traits<T>::mantissa_bits;
	  constexpr int exponent_bits
	    = floating_type_traits<T>::exponent_bits;
	  constexpr bool has_implicit_leading_bit
	    = floating_type_traits<T>::has_implicit_leading_bit;

	  const auto [mantissa, exponent, sign] = get_ieee_repr(value);
	  return ryu::generic_binary_to_decimal(mantissa, exponent, sign,
						mantissa_bits, exponent_bits,
						!has_implicit_leading_bit);
	}
    }

  // This subroutine returns true if the shortest scientific form fd is a
  // positive power of 10, and the floating-point number that has this shortest
  // scientific form is smaller than this power of 10.
  //
  // For instance, the exactly-representable 64-bit number
  // 99999999999999991611392.0 has the shortest scientific form 1e23, so its
  // exact value is smaller than its shortest scientific form.
  //
  // For these powers of 10 the length of the fixed form is one digit less
  // than what the scientific exponent suggests.
  //
  // This subroutine inspects a lookup table to detect when fd is such a
  // "rounded up" power of 10.
  template<typename T>
    bool
    is_rounded_up_pow10_p(const typename
			  floating_type_traits<T>::shortest_scientific_t fd)
    {
      if (fd.exponent < 0 || fd.mantissa != 1) [[likely]]
	return false;

      constexpr auto& pow10_adjustment_tab
	= floating_type_traits<T>::pow10_adjustment_tab;
      __glibcxx_assert(fd.exponent/64 < (int)std::size(pow10_adjustment_tab));
      return (pow10_adjustment_tab[fd.exponent/64]
	      & (1ull << (63 - fd.exponent%64)));
    }

  int
  get_mantissa_length(const ryu::floating_decimal_32 fd)
  { return ryu::decimalLength9(fd.mantissa); }

  int
  get_mantissa_length(const ryu::floating_decimal_64 fd)
  { return ryu::decimalLength17(fd.mantissa); }

  int
  get_mantissa_length(const ryu::floating_decimal_128 fd)
  { return ryu::generic128::decimalLength(fd.mantissa); }

#if !defined __SIZEOF_INT128__
  // An implementation of base-10 std::to_chars for the uint128_t class type,
  // used by targets that lack __int128.
  std::to_chars_result
  to_chars(char* first, char* const last, uint128_t x)
  {
    const int len = ryu::generic128::decimalLength(x);
    if (last - first < len)
      return {last, std::errc::value_too_large};
    if (x == 0)
      {
	*first++ = '0';
	return {first, std::errc{}};
      }
    for (int i = 0; i < len; ++i)
      {
	first[len - 1 - i] = '0' + static_cast<char>(x % 10);
	x /= 10;
      }
    __glibcxx_assert(x == 0);
    return {first + len, std::errc{}};
  }
#endif
} // anon namespace

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

// This subroutine of __floating_to_chars_* handles writing nan, inf and 0 in
// all formatting modes.
template<typename T>
  static optional<to_chars_result>
  __handle_special_value(char* first, char* const last, const T value,
			 const chars_format fmt, const int precision)
  {
    __glibcxx_assert(precision >= 0);

    string_view str;
    switch (__builtin_fpclassify(FP_NAN, FP_INFINITE, FP_NORMAL, FP_SUBNORMAL,
				 FP_ZERO, value))
      {
      case FP_INFINITE:
	str = "-inf";
	break;

      case FP_NAN:
	str = "-nan";
	break;

      case FP_ZERO:
	break;

      default:
      case FP_SUBNORMAL:
      case FP_NORMAL: [[likely]]
	return nullopt;
      }

    if (!str.empty())
      {
	// We're formatting +-inf or +-nan.
	if (!__builtin_signbit(value))
	  str.remove_prefix(strlen("-"));

	if (last - first < (int)str.length())
	  return {{last, errc::value_too_large}};

	memcpy(first, &str[0], str.length());
	first += str.length();
	return {{first, errc{}}};
      }

    // We're formatting 0.
    __glibcxx_assert(value == 0);
    const auto orig_first = first;
    const bool sign = __builtin_signbit(value);
    int expected_output_length;
    switch (fmt)
      {
      case chars_format::fixed:
      case chars_format::scientific:
      case chars_format::hex:
	expected_output_length = sign + 1;
	if (precision)
	  expected_output_length += strlen(".") + precision;
	if (fmt == chars_format::scientific)
	  expected_output_length += strlen("e+00");
	else if (fmt == chars_format::hex)
	  expected_output_length += strlen("p+0");
	if (last - first < expected_output_length)
	  return {{last, errc::value_too_large}};

	if (sign)
	  *first++ = '-';
	*first++ = '0';
	if (precision)
	  {
	    *first++ = '.';
	    memset(first, '0', precision);
	    first += precision;
	  }
	if (fmt == chars_format::scientific)
	  {
	    memcpy(first, "e+00", 4);
	    first += 4;
	  }
	else if (fmt == chars_format::hex)
	  {
	    memcpy(first, "p+0", 3);
	    first += 3;
	  }
	break;

      case chars_format::general:
      default: // case chars_format{}:
	expected_output_length = sign + 1;
	if (last - first < expected_output_length)
	  return {{last, errc::value_too_large}};

	if (sign)
	  *first++ = '-';
	*first++ = '0';
	break;
      }
    __glibcxx_assert(first - orig_first == expected_output_length);
    return {{first, errc{}}};
  }

template<>
  optional<to_chars_result>
  __handle_special_value<floating_type_float16_t>(char* first,
						  char* const last,
						  const floating_type_float16_t value,
						  const chars_format fmt,
						  const int precision)
  {
    return __handle_special_value(first, last, value.x, fmt, precision);
  }

template<>
  optional<to_chars_result>
  __handle_special_value<floating_type_bfloat16_t>(char* first,
						   char* const last,
						   const floating_type_bfloat16_t value,
						   const chars_format fmt,
						   const int precision)
  {
    return __handle_special_value(first, last, value.x, fmt, precision);
  }

// This subroutine of the floating-point to_chars overloads performs
// hexadecimal formatting.
template<typename T>
  static to_chars_result
  __floating_to_chars_hex(char* first, char* const last, const T value,
			  const optional<int> precision)
  {
    if (precision.has_value() && precision.value() < 0) [[unlikely]]
      // A negative precision argument is treated as if it were omitted.
      return __floating_to_chars_hex(first, last, value, nullopt);

    __glibcxx_requires_valid_range(first, last);

    constexpr int mantissa_bits = floating_type_traits<T>::mantissa_bits;
    constexpr bool has_implicit_leading_bit
      = floating_type_traits<T>::has_implicit_leading_bit;
    constexpr int exponent_bits = floating_type_traits<T>::exponent_bits;
    constexpr int exponent_bias = (1u << (exponent_bits - 1)) - 1;
    using mantissa_t = typename floating_type_traits<T>::mantissa_t;
    constexpr int mantissa_t_width = sizeof(mantissa_t) * __CHAR_BIT__;

    if (auto result = __handle_special_value(first, last, value,
					     chars_format::hex,
					     precision.value_or(0)))
      return *result;

    // Extract the sign, mantissa and exponent from the value.
    const auto [ieee_mantissa, biased_exponent, sign] = get_ieee_repr(value);
    const bool is_normal_number = (biased_exponent != 0);

    // Calculate the unbiased exponent.
    int32_t unbiased_exponent = (is_normal_number
				 ? biased_exponent - exponent_bias
				 : 1 - exponent_bias);

    // Shift the mantissa so that its bitwidth is a multiple of 4.
    constexpr unsigned rounded_mantissa_bits = (mantissa_bits + 3) / 4 * 4;
    static_assert(mantissa_t_width >= rounded_mantissa_bits);
    mantissa_t effective_mantissa
      = ieee_mantissa << (rounded_mantissa_bits - mantissa_bits);
    if (is_normal_number)
      {
	if constexpr (has_implicit_leading_bit)
	  // Restore the mantissa's implicit leading bit.
	  effective_mantissa |= mantissa_t{1} << rounded_mantissa_bits;
	else
	  // The explicit mantissa bit should already be set.
	  __glibcxx_assert(effective_mantissa & (mantissa_t{1} << (mantissa_bits
								   - 1u)));
      }
    else if (!precision.has_value() && effective_mantissa)
      {
	// 1.8p-23 is shorter than 0.00cp-14, so if precision is
	// omitted, try to canonicalize denormals such that they
	// have the leading bit set.
	int width = __bit_width(effective_mantissa);
	int shift = rounded_mantissa_bits - width + has_implicit_leading_bit;
	unbiased_exponent -= shift;
	effective_mantissa <<= shift;
      }

    // Compute the shortest precision needed to print this value exactly,
    // disregarding trailing zeros.
    constexpr int full_hex_precision = (has_implicit_leading_bit
					? (mantissa_bits + 3) / 4
					// With an explicit leading bit, we
					// use the four leading nibbles as the
					// hexit before the decimal point.
					: (mantissa_bits - 4 + 3) / 4);
    const int trailing_zeros = __countr_zero(effective_mantissa) / 4;
    const int shortest_full_precision = full_hex_precision - trailing_zeros;
    __glibcxx_assert(shortest_full_precision >= 0);

    int written_exponent = unbiased_exponent;
    int effective_precision = precision.value_or(shortest_full_precision);
    int excess_precision = 0;
    if (effective_precision < shortest_full_precision)
      {
	// When limiting the precision, we need to determine how to round the
	// least significant printed hexit.  The following branchless
	// bit-level-parallel technique computes whether to round up the
	// mantissa bit at index N (according to round-to-nearest rules) when
	// dropping N bits of precision, for each index N in the bit vector.
	// This technique is borrowed from the MSVC implementation.
	using bitvec = mantissa_t;
	const bitvec round_bit = effective_mantissa << 1;
	const bitvec has_tail_bits = round_bit - 1;
	const bitvec lsb_bit = effective_mantissa;
	const bitvec should_round = round_bit & (has_tail_bits | lsb_bit);

	const int dropped_bits = 4*(full_hex_precision - effective_precision);
	// Mask out the dropped nibbles.
	effective_mantissa >>= dropped_bits;
	effective_mantissa <<= dropped_bits;
	if (should_round & (mantissa_t{1} << dropped_bits))
	  {
	    // Round up the least significant nibble.
	    effective_mantissa += mantissa_t{1} << dropped_bits;
	    // Check and adjust for overflow of the leading nibble.  When the
	    // type has an implicit leading bit, then the leading nibble
	    // before rounding is either 0 or 1, so it can't overflow.
	    if constexpr (!has_implicit_leading_bit)
	      {
		// The only supported floating-point type with explicit
		// leading mantissa bit is LDK_FLOAT80, i.e. x86 80-bit
		// extended precision, and so we hardcode the below overflow
		// check+adjustment for this type.
		static_assert(mantissa_t_width == 64
			      && rounded_mantissa_bits == 64);
		if (effective_mantissa == 0)
		  {
		    // We rounded up the least significant nibble and the
		    // mantissa overflowed, e.g f.fcp+10 with precision=1
		    // became 10.0p+10.  Absorb this extra hexit into the
		    // exponent to obtain 1.0p+14.
		    effective_mantissa
		      = mantissa_t{1} << (rounded_mantissa_bits - 4);
		    written_exponent += 4;
		  }
	      }
	  }
      }
    else
      {
	excess_precision = effective_precision - shortest_full_precision;
	effective_precision = shortest_full_precision;
      }

    // Compute the leading hexit and mask it out from the mantissa.
    char leading_hexit;
    if constexpr (has_implicit_leading_bit)
      {
	const auto nibble = unsigned(effective_mantissa >> rounded_mantissa_bits);
	__glibcxx_assert(nibble <= 2);
	leading_hexit = '0' + nibble;
	effective_mantissa &= ~(mantissa_t{0b11} << rounded_mantissa_bits);
      }
    else
      {
	const auto nibble = unsigned(effective_mantissa >> (rounded_mantissa_bits-4));
	__glibcxx_assert(nibble < 16);
	leading_hexit = "0123456789abcdef"[nibble];
	effective_mantissa &= ~(mantissa_t{0b1111} << (rounded_mantissa_bits-4));
	written_exponent -= 3;
      }

    // Now before we start writing the string, determine the total length of
    // the output string and perform a single bounds check.
    int expected_output_length = sign + 1;
    if (effective_precision + excess_precision > 0)
      expected_output_length += strlen(".");
    expected_output_length += effective_precision;
    const int abs_written_exponent = abs(written_exponent);
    expected_output_length += (abs_written_exponent >= 10000 ? strlen("p+ddddd")
			       : abs_written_exponent >= 1000 ? strlen("p+dddd")
			       : abs_written_exponent >= 100 ? strlen("p+ddd")
			       : abs_written_exponent >= 10 ? strlen("p+dd")
			       : strlen("p+d"));
    if (last - first < expected_output_length
	|| last - first - expected_output_length < excess_precision)
      return {last, errc::value_too_large};
    char* const expected_output_end = first + expected_output_length + excess_precision;

    // Write the negative sign and the leading hexit.
    if (sign)
      *first++ = '-';
    *first++ = leading_hexit;

    if (effective_precision + excess_precision > 0)
      *first++ = '.';

    if (effective_precision > 0)
      {
	int written_hexits = 0;
	// Extract and mask out the leading nibble after the decimal point,
	// write its corresponding hexit, and repeat until the mantissa is
	// empty.
	int nibble_offset = rounded_mantissa_bits;
	if constexpr (!has_implicit_leading_bit)
	  // We already printed the entire leading hexit.
	  nibble_offset -= 4;
	while (effective_mantissa != 0)
	  {
	    nibble_offset -= 4;
	    const auto nibble = unsigned(effective_mantissa >> nibble_offset);
	    __glibcxx_assert(nibble < 16);
	    *first++ = "0123456789abcdef"[nibble];
	    ++written_hexits;
	     effective_mantissa &= ~(mantissa_t{0b1111} << nibble_offset);
	  }
	__glibcxx_assert(nibble_offset >= 0);
	__glibcxx_assert(written_hexits <= effective_precision);
	// Since the mantissa is now empty, every hexit hereafter must be '0'.
	if (int remaining_hexits = effective_precision - written_hexits)
	  {
	    memset(first, '0', remaining_hexits);
	    first += remaining_hexits;
	  }
      }

    if (excess_precision > 0)
      {
	memset(first, '0', excess_precision);
	first += excess_precision;
      }

    // Finally, write the exponent.
    *first++ = 'p';
    if (written_exponent >= 0)
      *first++ = '+';
    const to_chars_result result = to_chars(first, last, written_exponent);
    __glibcxx_assert(result.ec == errc{} && result.ptr == expected_output_end);
    return result;
  }

namespace
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wabi"
  template<typename T, typename... Extra>
  inline int
  sprintf_ld(char* buffer, size_t length __attribute__((unused)),
	     const char* format_string, T value, Extra... args)
  {
    int len;

#if _GLIBCXX_USE_C99_FENV_TR1 && defined(FE_TONEAREST)
    const int saved_rounding_mode = fegetround();
    if (saved_rounding_mode != FE_TONEAREST)
      fesetround(FE_TONEAREST); // We want round-to-nearest behavior.
#endif

#ifdef FLOAT128_TO_CHARS
#ifdef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
    if constexpr (is_same_v<T, __ieee128>)
      len = __sprintfieee128(buffer, format_string, args..., value);
    else
#else
    if constexpr (is_same_v<T, _Float128>)
      {
#ifndef _GLIBCXX_HAVE_FLOAT128_MATH
	if (&__strfromf128 == nullptr)
	  len = sprintf(buffer, format_string, args..., (long double)value);
	else
#endif
	if constexpr (sizeof...(args) == 0)
	  len = __strfromf128(buffer, length, "%.0f", value);
	else
	  {
	    // strfromf128 unfortunately doesn't allow .*
	    char fmt[3 * sizeof(int) + 6];
	    sprintf(fmt, "%%.%d%c", args..., int(format_string[4]));
	    len = __strfromf128(buffer, length, fmt, value);
	  }
      }
    else
#endif
#endif
    len = sprintf(buffer, format_string, args..., value);

#if _GLIBCXX_USE_C99_FENV_TR1 && defined(FE_TONEAREST)
    if (saved_rounding_mode != FE_TONEAREST)
      fesetround(saved_rounding_mode);
#endif

    return len;
  }
#pragma GCC diagnostic pop
}

template<typename T>
  static to_chars_result
  __floating_to_chars_shortest(char* first, char* const last, const T value,
			       chars_format fmt)
  {
    if (fmt == chars_format::hex)
      {
	// std::bfloat16_t has the same exponent range as std::float32_t
	// and so we can avoid instantiation of __floating_to_chars_hex
	// for bfloat16_t.  Shortest hex will be the same as for float.
	// When we print shortest form even for denormals, we can do it
	// for std::float16_t as well.
	if constexpr (is_same_v<T, floating_type_float16_t>
		      || is_same_v<T, floating_type_bfloat16_t>)
	  return __floating_to_chars_hex(first, last, value.x, nullopt);
	else
	  return __floating_to_chars_hex(first, last, value, nullopt);
      }

    __glibcxx_assert(fmt == chars_format::fixed
		     || fmt == chars_format::scientific
		     || fmt == chars_format::general
		     || fmt == chars_format{});
    __glibcxx_requires_valid_range(first, last);

    if (auto result = __handle_special_value(first, last, value, fmt, 0))
      return *result;

    const auto fd = floating_to_shortest_scientific(value);
    const int mantissa_length = get_mantissa_length(fd);
    const int scientific_exponent = fd.exponent + mantissa_length - 1;

    if (fmt == chars_format::general)
      {
	// Resolve the 'general' formatting mode as per the specification of
	// the 'g' printf output specifier.  Since there is no precision
	// argument, the default precision of the 'g' specifier, 6, applies.
	if (scientific_exponent >= -4 && scientific_exponent < 6)
	  fmt = chars_format::fixed;
	else
	  fmt = chars_format::scientific;
      }
    else if (fmt == chars_format{})
      {
	// The 'plain' formatting mode resolves to 'scientific' if it yields
	// the shorter string, and resolves to 'fixed' otherwise.  The
	// following lower and upper bounds on the exponent characterize when
	// to prefer 'fixed' over 'scientific'.
	int lower_bound = -(mantissa_length + 3);
	int upper_bound = 5;
	if (mantissa_length == 1)
	  // The decimal point in scientific notation will be omitted in this
	  // case; tighten the bounds appropriately.
	  ++lower_bound, --upper_bound;

	if (fd.exponent >= lower_bound && fd.exponent <= upper_bound)
	  fmt = chars_format::fixed;
	else
	  fmt = chars_format::scientific;
      }

    if (fmt == chars_format::scientific)
      {
	// Calculate the total length of the output string, perform a bounds
	// check, and then defer to Ryu's to_chars subroutine.
	int expected_output_length = fd.sign + mantissa_length;
	if (mantissa_length > 1)
	  expected_output_length += strlen(".");
	const int abs_exponent = abs(scientific_exponent);
	expected_output_length += (abs_exponent >= 1000 ? strlen("e+dddd")
				   : abs_exponent >= 100 ? strlen("e+ddd")
				   : strlen("e+dd"));
	if (last - first < expected_output_length)
	  return {last, errc::value_too_large};

	const int output_length = ryu::to_chars(fd, first);
	__glibcxx_assert(output_length == expected_output_length);
	return {first + output_length, errc{}};
      }
    else if (fmt == chars_format::fixed && fd.exponent >= 0)
      {
	// The Ryu exponent is positive, and so this number's shortest
	// representation is a whole number, to be formatted in fixed instead
	// of scientific notation "as if by std::printf".  This means we may
	// need to print more digits of the IEEE mantissa than what the
	// shortest scientific form given by Ryu provides.
	//
	// For instance, the exactly representable number
	// 12300000000000001048576.0 has as its shortest scientific
	// representation 123e+22, so in this case fd.mantissa is 123 and
	// fd.exponent is 22, which doesn't have enough information to format
	// the number exactly.  So we defer to Ryu's d2fixed_buffered_n with
	// precision=0 to format the number in the general case here.

	// To that end, first compute the output length and perform a bounds
	// check.
	int expected_output_length = fd.sign + mantissa_length + fd.exponent;
	if (is_rounded_up_pow10_p<T>(fd))
	  --expected_output_length;
	if (last - first < expected_output_length)
	  return {last, errc::value_too_large};

	// Optimization: if the shortest representation fits inside the IEEE
	// mantissa, then the number is certainly exactly-representable and
	// its shortest scientific form must be equal to its exact form.  So
	// we can write the value in fixed form exactly via fd.mantissa and
	// fd.exponent.
	//
	// Taking log2 of both sides of the desired condition
	//   fd.mantissa * 10^fd.exponent < 2^mantissa_bits
	// we get
	//   log2 fd.mantissa + fd.exponent * log2 10 < mantissa_bits
	// where log2 10 is slightly smaller than 10/3=3.333...
	//
	// After adding some wiggle room due to rounding we get the condition
	// value_fits_inside_mantissa_p below.
	const int log2_mantissa = __bit_width(fd.mantissa) - 1;
	const bool value_fits_inside_mantissa_p
	  = (log2_mantissa + (fd.exponent*10 + 2) / 3
	     < floating_type_traits<T>::mantissa_bits - 2);
	if (value_fits_inside_mantissa_p)
	  {
	    // Print the small exactly-representable number in fixed form by
	    // writing out fd.mantissa followed by fd.exponent many 0s.
	    if (fd.sign)
	      *first++ = '-';
	    to_chars_result result = to_chars(first, last, fd.mantissa);
	    __glibcxx_assert(result.ec == errc{});
	    memset(result.ptr, '0', fd.exponent);
	    result.ptr += fd.exponent;
	    const int output_length = fd.sign + (result.ptr - first);
	    __glibcxx_assert(output_length == expected_output_length);
	    return result;
	  }
	else if constexpr (is_same_v<T, long double>
			   || is_same_v<T, F128_type>)
	  {
	    // We can't use d2fixed_buffered_n for types larger than double,
	    // so we instead format larger types through sprintf.
	    // TODO: We currently go through an intermediate buffer in order
	    // to accommodate the mandatory null terminator of sprintf, but we
	    // can avoid this if we use sprintf to write all but the last
	    // digit, and carefully compute and write the last digit
	    // ourselves.
	    char buffer[expected_output_length + 1];
	    const int output_length = sprintf_ld(buffer,
						 expected_output_length + 1,
						 "%.0Lf", value);
	    __glibcxx_assert(output_length == expected_output_length);
	    memcpy(first, buffer, output_length);
	    return {first + output_length, errc{}};
	  }
	else
	  {
	    // Otherwise, the number is too big, so defer to d2fixed_buffered_n.
	    const int output_length = ryu::d2fixed_buffered_n(value, 0, first);
	    __glibcxx_assert(output_length == expected_output_length);
	    return {first + output_length, errc{}};
	  }
      }
    else if (fmt == chars_format::fixed && fd.exponent < 0)
      {
	// The Ryu exponent is negative, so fd.mantissa definitely contains
	// all of the whole part of the number, and therefore fd.mantissa and
	// fd.exponent contain all of the information needed to format the
	// number in fixed notation "as if by std::printf" (with precision
	// equal to -fd.exponent).
	const int whole_digits = max<int>(mantissa_length + fd.exponent, 1);
	const int expected_output_length
	  = fd.sign + whole_digits + strlen(".") + -fd.exponent;
	if (last - first < expected_output_length)
	  return {last, errc::value_too_large};
	if (mantissa_length <= -fd.exponent)
	  {
	    // The magnitude of the number is less than one.  Format the
	    // number appropriately.
	    const auto orig_first = first;
	    if (fd.sign)
	      *first++ = '-';
	    *first++ = '0';
	    *first++ = '.';
	    const int leading_zeros = -fd.exponent - mantissa_length;
	    memset(first, '0', leading_zeros);
	    first += leading_zeros;
	    const to_chars_result result = to_chars(first, last, fd.mantissa);
	    const int output_length = result.ptr - orig_first;
	    __glibcxx_assert(output_length == expected_output_length
			     && result.ec == errc{});
	    return result;
	  }
	else
	  {
	    // The magnitude of the number is at least one.
	    const auto orig_first = first;
	    if (fd.sign)
	      *first++ = '-';
	    to_chars_result result = to_chars(first, last, fd.mantissa);
	    __glibcxx_assert(result.ec == errc{});
	    // Make space for and write the decimal point in the correct spot.
	    memmove(&result.ptr[fd.exponent+1], &result.ptr[fd.exponent],
		    -fd.exponent);
	    result.ptr[fd.exponent] = '.';
	    const int output_length = result.ptr + 1 - orig_first;
	    __glibcxx_assert(output_length == expected_output_length);
	    ++result.ptr;
	    return result;
	  }
      }

    __glibcxx_assert(false);
    __builtin_unreachable();
  }

template<typename T>
  static to_chars_result
  __floating_to_chars_precision(char* first, char* const last, const T value,
				chars_format fmt, const int precision)
  {
    if (fmt == chars_format::hex)
      return __floating_to_chars_hex(first, last, value, precision);

    if (precision < 0) [[unlikely]]
      // A negative precision argument is treated as if it were omitted, in
      // which case the default precision of 6 applies, as per the printf
      // specification.
      return __floating_to_chars_precision(first, last, value, fmt, 6);

    __glibcxx_assert(fmt == chars_format::fixed
		     || fmt == chars_format::scientific
		     || fmt == chars_format::general);
    __glibcxx_requires_valid_range(first, last);

    if (auto result = __handle_special_value(first, last, value,
					     fmt, precision))
      return *result;

    constexpr int mantissa_bits = floating_type_traits<T>::mantissa_bits;
    constexpr int exponent_bits = floating_type_traits<T>::exponent_bits;
    constexpr int exponent_bias = (1u << (exponent_bits - 1)) - 1;

    // Extract the sign and exponent from the value.
    const auto [mantissa, biased_exponent, sign] = get_ieee_repr(value);
    const bool is_normal_number = (biased_exponent != 0);

    // Calculate the unbiased exponent.
    const int32_t unbiased_exponent = (is_normal_number
				       ? biased_exponent - exponent_bias
				       : 1 - exponent_bias);

    // Obtain trunc(log2(abs(value))), which is just the unbiased exponent.
    const int floor_log2_value = unbiased_exponent;
    // This is within +-1 of log10(abs(value)).  Note that log10 2 is 0.3010..
    const int approx_log10_value = (floor_log2_value >= 0
				    ? (floor_log2_value*301 + 999)/1000
				    : (floor_log2_value*301 - 999)/1000);

    // Compute (an upper bound of) the number's effective precision when it is
    // formatted in scientific and fixed notation.  Beyond this precision all
    // digits are definitely zero, and this fact allows us to bound the sizes
    // of any local output buffers that we may need to use.  TODO: Consider
    // the number of trailing zero bits in the mantissa to obtain finer upper
    // bounds.
    // ???: Using "mantissa_bits + 1" instead of just "mantissa_bits" in the
    // bounds below is necessary only for __ibm128, it seems.  Even though the
    // type has 105 bits of precision, printf may output 106 fractional digits
    // on some inputs, e.g. 0x1.bcd19f5d720d12a3513e3301028p+0.
    const int max_eff_scientific_precision
      = (floor_log2_value >= 0
	 ? max(mantissa_bits + 1, approx_log10_value + 1)
	 : -(7*floor_log2_value + 9)/10 + 2 + mantissa_bits + 1);
    __glibcxx_assert(max_eff_scientific_precision > 0);

    const int max_eff_fixed_precision
      = (floor_log2_value >= 0
	 ? mantissa_bits + 1
	 : -floor_log2_value + mantissa_bits + 1);
    __glibcxx_assert(max_eff_fixed_precision > 0);

    // Ryu doesn't support formatting floating-point types larger than double
    // with an explicit precision, so instead we just go through printf.
    if constexpr (is_same_v<T, long double> || is_same_v<T, F128_type>)
      {
	int effective_precision;
	const char* output_specifier;
	if (fmt == chars_format::scientific)
	  {
	    effective_precision = min(precision, max_eff_scientific_precision);
	    output_specifier = "%.*Le";
	  }
	else if (fmt == chars_format::fixed)
	  {
	    effective_precision = min(precision, max_eff_fixed_precision);
	    output_specifier = "%.*Lf";
	  }
	else if (fmt == chars_format::general)
	  {
	    effective_precision = min(precision, max_eff_scientific_precision);
	    output_specifier = "%.*Lg";
	  }
	else
	  __builtin_unreachable();
	const int excess_precision = (fmt != chars_format::general
				      ? precision - effective_precision : 0);

	// Since the output of printf is locale-sensitive, we need to be able
	// to handle a radix point that's different from '.'.
	char radix[6] = {'.', '\0', '\0', '\0', '\0', '\0'};
#ifdef RADIXCHAR
	if (effective_precision > 0)
	  // ???: Can nl_langinfo() ever return null?
	  if (const char* const radix_ptr = nl_langinfo(RADIXCHAR))
	    {
	      strncpy(radix, radix_ptr, sizeof(radix)-1);
	      // We accept only radix points which are at most 4 bytes (one
	      // UTF-8 character) wide.
	      __glibcxx_assert(radix[4] == '\0');
	    }
#endif

	// Compute straightforward upper bounds on the output length.
	int output_length_upper_bound;
	if (fmt == chars_format::scientific || fmt == chars_format::general)
	  output_length_upper_bound = (strlen("-d") + sizeof(radix)
				       + effective_precision
				       + strlen("e+dddd"));
	else if (fmt == chars_format::fixed)
	  {
	    if (approx_log10_value >= 0)
	      output_length_upper_bound = sign + approx_log10_value + 1;
	    else
	      output_length_upper_bound = sign + strlen("0");
	    output_length_upper_bound += sizeof(radix) + effective_precision;
	  }
	else
	  __builtin_unreachable();

	// Do the sprintf into the local buffer.
	char buffer[output_length_upper_bound + 1];
	int output_length
	  = sprintf_ld(buffer, output_length_upper_bound + 1, output_specifier,
		       value, effective_precision);
	__glibcxx_assert(output_length <= output_length_upper_bound);

	if (effective_precision > 0)
	  // We need to replace a radix that is different from '.' with '.'.
	  if (const string_view radix_sv = {radix}; radix_sv != ".")
	    {
	      const string_view buffer_sv = {buffer, (size_t)output_length};
	      const size_t radix_index = buffer_sv.find(radix_sv);
	      if (radix_index != string_view::npos)
		{
		  buffer[radix_index] = '.';
		  if (radix_sv.length() > 1)
		    {
		      memmove(&buffer[radix_index + 1],
			      &buffer[radix_index + radix_sv.length()],
			      output_length - radix_index - radix_sv.length());
		      output_length -= radix_sv.length() - 1;
		    }
		}
	    }

	// Copy the string from the buffer over to the output range.
	if (last - first < output_length
	    || last - first - output_length < excess_precision)
	  return {last, errc::value_too_large};
	memcpy(first, buffer, output_length);
	first += output_length;

	// Add the excess 0s to the result.
	if (excess_precision > 0)
	  {
	    if (fmt == chars_format::scientific)
	      {
		char* const significand_end
		  = (output_length >= 6 && first[-6] == 'e' ? &first[-6]
		     : first[-5] == 'e' ? &first[-5]
		     : &first[-4]);
		__glibcxx_assert(*significand_end == 'e');
		  memmove(significand_end + excess_precision, significand_end,
			  first - significand_end);
		  memset(significand_end, '0', excess_precision);
		  first += excess_precision;
	      }
	    else if (fmt == chars_format::fixed)
	      {
		memset(first, '0', excess_precision);
		first += excess_precision;
	      }
	  }
	return {first, errc{}};
      }
    else if (fmt == chars_format::scientific)
      {
	const int effective_precision
	  = min(precision, max_eff_scientific_precision);
	const int excess_precision = precision - effective_precision;

	// We can easily compute the output length exactly whenever the
	// scientific exponent is far enough away from +-100.  But if it's
	// near +-100, then our log2 approximation is too coarse (and doesn't
	// consider precision-dependent rounding) in order to accurately
	// distinguish between a scientific exponent of +-100 and +-99.
	const bool scientific_exponent_near_100_p
	  = abs(abs(floor_log2_value) - 332) <= 4;

	// Compute an upper bound on the output length.  TODO: Maybe also
	// consider a lower bound on the output length.
	int output_length_upper_bound = sign + strlen("d");
	if (effective_precision > 0)
	  output_length_upper_bound += strlen(".") + effective_precision;
	if (scientific_exponent_near_100_p
	    || (floor_log2_value >= 332 || floor_log2_value <= -333))
	  output_length_upper_bound += strlen("e+ddd");
	else
	  output_length_upper_bound += strlen("e+dd");

	int output_length;
	if (last - first >= output_length_upper_bound
	    && last - first - output_length_upper_bound >= excess_precision)
	  {
	    // The result will definitely fit into the output range, so we can
	    // write directly into it.
	    output_length = ryu::d2exp_buffered_n(value, effective_precision,
						  first, nullptr);
	    __glibcxx_assert(output_length == output_length_upper_bound
			     || (scientific_exponent_near_100_p
				 && (output_length
				     == output_length_upper_bound - 1)));
	  }
	else if (scientific_exponent_near_100_p)
	  {
	    // Write the result of d2exp_buffered_n into an intermediate
	    // buffer, do a bounds check, and copy the result into the output
	    // range.
	    char buffer[output_length_upper_bound];
	    output_length = ryu::d2exp_buffered_n(value, effective_precision,
						  buffer, nullptr);
	    __glibcxx_assert(output_length == output_length_upper_bound - 1
			     || output_length == output_length_upper_bound);
	    if (last - first < output_length
		|| last - first - output_length < excess_precision)
	      return {last, errc::value_too_large};
	    memcpy(first, buffer, output_length);
	  }
	else
	  // If the scientific exponent is not near 100, then the upper bound
	  // is actually the exact length, and so the result will definitely
	  // not fit into the output range.
	  return {last, errc::value_too_large};
	first += output_length;
	if (excess_precision > 0)
	  {
	    // Splice the excess zeros into the result.
	    char* const significand_end = (first[-5] == 'e'
					   ? &first[-5] : &first[-4]);
	    __glibcxx_assert(*significand_end == 'e');
	    memmove(significand_end + excess_precision, significand_end,
		    first - significand_end);
	    memset(significand_end, '0', excess_precision);
	    first += excess_precision;
	  }
	return {first, errc{}};
      }
    else if (fmt == chars_format::fixed)
      {
	const int effective_precision
	  = min(precision, max_eff_fixed_precision);
	const int excess_precision = precision - effective_precision;

	// Compute an upper bound on the output length.  TODO: Maybe also
	// consider a lower bound on the output length.
	int output_length_upper_bound;
	if (approx_log10_value >= 0)
	  output_length_upper_bound = sign + approx_log10_value + 1;
	else
	  output_length_upper_bound = sign + strlen("0");
	if (effective_precision > 0)
	  output_length_upper_bound += strlen(".") + effective_precision;

	int output_length;
	if (last - first >= output_length_upper_bound
	    && last - first - output_length_upper_bound >= excess_precision)
	  {
	    // The result will definitely fit into the output range, so we can
	    // write directly into it.
	    output_length = ryu::d2fixed_buffered_n(value, effective_precision,
						    first);
	    __glibcxx_assert(output_length <= output_length_upper_bound);
	  }
	else
	  {
	    // Write the result of d2fixed_buffered_n into an intermediate
	    // buffer, do a bounds check, and copy the result into the output
	    // range.
	    char buffer[output_length_upper_bound];
	    output_length = ryu::d2fixed_buffered_n(value, effective_precision,
						    buffer);
	    __glibcxx_assert(output_length <= output_length_upper_bound);
	    if (last - first < output_length
		|| last - first - output_length < excess_precision)
	      return {last, errc::value_too_large};
	    memcpy(first, buffer, output_length);
	  }
	first += output_length;
	if (excess_precision > 0)
	  {
	    // Append the excess zeros into the result.
	    memset(first, '0', excess_precision);
	    first += excess_precision;
	  }
	return {first, errc{}};
      }
    else if (fmt == chars_format::general)
      {
	// Handle the 'general' formatting mode as per C11 printf's %g output
	// specifier.  Since Ryu doesn't do zero-trimming, we always write to
	// an intermediate buffer and manually perform zero-trimming there
	// before copying the result over to the output range.
	int effective_precision
	  = min(precision, max_eff_scientific_precision + 1);
	const int output_length_upper_bound
	  = strlen("-d.") + effective_precision + strlen("e+ddd");
	// The four bytes of headroom is to avoid needing to do a memmove when
	// rewriting a scientific form such as 1.00e-2 into the equivalent
	// fixed form 0.001.
	char buffer[4 + output_length_upper_bound];

	// 7.21.6.1/8: "Let P equal ... 1 if the precision is zero."
	if (effective_precision == 0)
	  effective_precision = 1;

	// Perform a trial formatting in scientific form, and obtain the
	// scientific exponent.
	int scientific_exponent;
	char* buffer_start = buffer + 4;
	int output_length
	  = ryu::d2exp_buffered_n(value, effective_precision - 1,
				  buffer_start, &scientific_exponent);
	__glibcxx_assert(output_length <= output_length_upper_bound);

	// 7.21.6.1/8: "Then, if a conversion with style E would have an
	// exponent of X:
	//   if P > X >= -4, the conversion is with style f and
	//     precision P - (X + 1).
	//   otherwise, the conversion is with style e and precision P - 1."
	const bool resolve_to_fixed_form
	  = (scientific_exponent >= -4
	     && scientific_exponent < effective_precision);
	if (resolve_to_fixed_form)
	  {
	    // Rather than invoking d2fixed_buffered_n to reformat the number
	    // for us from scratch, we can just rewrite the scientific form
	    // into fixed form in-place.  This is safe to do because whenever
	    // %g resolves to %f, the fixed form will be no larger than the
	    // corresponding scientific form, and it will also contain the
	    // same significant digits as the scientific form.
	    fmt = chars_format::fixed;
	    if (scientific_exponent < 0)
	      {
		// e.g. buffer_start == "-1.234e-04"
		char* leading_digit = &buffer_start[sign];
		leading_digit[1] = leading_digit[0];
		// buffer_start == "-11234e-04"
		buffer_start -= -scientific_exponent;
		__glibcxx_assert(buffer_start >= buffer);
		// buffer_start == "????-11234e-04"
		char* head = buffer_start;
		if (sign)
		  *head++ = '-';
		*head++ = '0';
		*head++ = '.';
		memset(head, '0', -scientific_exponent - 1);
		// buffer_start == "-0.00011234e-04"

		// Now drop the exponent suffix, and add the leading zeros to
		// the output length.
		output_length -= strlen("e-0d");
		output_length += -scientific_exponent;
		if (effective_precision - 1 == 0)
		  // The scientific form had no decimal point, but the fixed
		  // form now does.
		  output_length += strlen(".");
	      }
	    else if (effective_precision == 1)
	      {
		// The scientific exponent must be 0, so the fixed form
		// coincides with the scientific form (minus the exponent
		// suffix).
		__glibcxx_assert(scientific_exponent == 0);
		output_length -= strlen("e+dd");
	      }
	    else
	      {
		// We are dealing with a scientific form which has a
		// non-empty fractional part and a nonnegative exponent,
		// e.g. buffer_start == "1.234e+02".
		__glibcxx_assert(effective_precision >= 1);
		char* const decimal_point = &buffer_start[sign + 1];
		__glibcxx_assert(*decimal_point == '.');
		memmove(decimal_point, decimal_point+1,
			scientific_exponent);
		// buffer_start == "123.4e+02"
		decimal_point[scientific_exponent] = '.';
		if (scientific_exponent >= 100)
		  output_length -= strlen("e+ddd");
		else
		  output_length -= strlen("e+dd");
		if (effective_precision - 1 == scientific_exponent)
		  output_length -= strlen(".");
	      }
	    effective_precision -= 1 + scientific_exponent;

	    __glibcxx_assert(output_length <= output_length_upper_bound);
	  }
	else
	  {
	    // We're sticking to the scientific form, so keep the output as-is.
	    fmt = chars_format::scientific;
	    effective_precision = effective_precision - 1;
	  }

	// 7.21.6.1/8: "Finally ... any any trailing zeros are removed from
	// the fractional portion of the result and the decimal-point
	// character is removed if there is no fractional portion remaining."
	if (effective_precision > 0)
	  {
	    char* decimal_point = nullptr;
	    if (fmt == chars_format::scientific)
	      decimal_point = &buffer_start[sign + 1];
	    else if (fmt == chars_format::fixed)
	      decimal_point
		= &buffer_start[output_length] - effective_precision - 1;
	    __glibcxx_assert(*decimal_point == '.');

	    char* const fractional_part_start = decimal_point + 1;
	    char* fractional_part_end = nullptr;
	    if (fmt == chars_format::scientific)
	      {
		fractional_part_end = (buffer_start[output_length-5] == 'e'
				       ? &buffer_start[output_length-5]
				       : &buffer_start[output_length-4]);
		__glibcxx_assert(*fractional_part_end == 'e');
	      }
	    else if (fmt == chars_format::fixed)
	      fractional_part_end = &buffer_start[output_length];

	    const string_view fractional_part
	      = {fractional_part_start, (size_t)(fractional_part_end
						 - fractional_part_start) };
	    const size_t last_nonzero_digit_pos
	      = fractional_part.find_last_not_of('0');

	    char* trim_start;
	    if (last_nonzero_digit_pos == string_view::npos)
	      trim_start = decimal_point;
	    else
	      trim_start = &fractional_part_start[last_nonzero_digit_pos] + 1;
	    if (fmt == chars_format::scientific)
	      memmove(trim_start, fractional_part_end,
		      &buffer_start[output_length] - fractional_part_end);
	    output_length -= fractional_part_end - trim_start;
	  }

	if (last - first < output_length)
	  return {last, errc::value_too_large};

	memcpy(first, buffer_start, output_length);
	return {first + output_length, errc{}};
      }

    __glibcxx_assert(false);
    __builtin_unreachable();
  }

// Define the overloads for float.
to_chars_result
to_chars(char* first, char* last, float value) noexcept
{ return __floating_to_chars_shortest(first, last, value, chars_format{}); }

to_chars_result
to_chars(char* first, char* last, float value, chars_format fmt) noexcept
{ return __floating_to_chars_shortest(first, last, value, fmt); }

to_chars_result
to_chars(char* first, char* last, float value, chars_format fmt,
	 int precision) noexcept
{ return __floating_to_chars_precision(first, last, value, fmt, precision); }

// Define the overloads for double.
to_chars_result
to_chars(char* first, char* last, double value) noexcept
{ return __floating_to_chars_shortest(first, last, value, chars_format{}); }

to_chars_result
to_chars(char* first, char* last, double value, chars_format fmt) noexcept
{ return __floating_to_chars_shortest(first, last, value, fmt); }

to_chars_result
to_chars(char* first, char* last, double value, chars_format fmt,
	 int precision) noexcept
{ return __floating_to_chars_precision(first, last, value, fmt, precision); }

// Define the overloads for long double.
to_chars_result
to_chars(char* first, char* last, long double value) noexcept
{
  if constexpr (LONG_DOUBLE_KIND == LDK_BINARY64
		|| LONG_DOUBLE_KIND == LDK_UNSUPPORTED)
    return __floating_to_chars_shortest(first, last, static_cast<double>(value),
					chars_format{});
  else
    return __floating_to_chars_shortest(first, last, value, chars_format{});
}

to_chars_result
to_chars(char* first, char* last, long double value, chars_format fmt) noexcept
{
  if constexpr (LONG_DOUBLE_KIND == LDK_BINARY64
		|| LONG_DOUBLE_KIND == LDK_UNSUPPORTED)
    return __floating_to_chars_shortest(first, last, static_cast<double>(value),
					fmt);
  else
    return __floating_to_chars_shortest(first, last, value, fmt);
}

to_chars_result
to_chars(char* first, char* last, long double value, chars_format fmt,
	 int precision) noexcept
{
  if constexpr (LONG_DOUBLE_KIND == LDK_BINARY64
		|| LONG_DOUBLE_KIND == LDK_UNSUPPORTED)
    return __floating_to_chars_precision(first, last, static_cast<double>(value),
					 fmt,
					 precision);
  else
    return __floating_to_chars_precision(first, last, value, fmt, precision);
}

#ifdef FLOAT128_TO_CHARS
#ifdef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
to_chars_result
to_chars(char* first, char* last, __float128 value) noexcept
{
  return __floating_to_chars_shortest(first, last, value, chars_format{});
}

to_chars_result
to_chars(char* first, char* last, __float128 value, chars_format fmt) noexcept
{
  return __floating_to_chars_shortest(first, last, value, fmt);
}

to_chars_result
to_chars(char* first, char* last, __float128 value, chars_format fmt,
	 int precision) noexcept
{
  return __floating_to_chars_precision(first, last, value, fmt, precision);
}

extern "C" to_chars_result
_ZSt8to_charsPcS_DF128_(char* first, char* last, __float128 value) noexcept
  __attribute__((alias ("_ZSt8to_charsPcS_u9__ieee128")));

extern "C" to_chars_result
_ZSt8to_charsPcS_DF128_St12chars_format(char* first, char* last,
					__float128 value,
					chars_format fmt) noexcept
  __attribute__((alias ("_ZSt8to_charsPcS_u9__ieee128St12chars_format")));

extern "C" to_chars_result
_ZSt8to_charsPcS_DF128_St12chars_formati(char* first, char* last,
					 __float128 value,
					 chars_format fmt,
					 int precision) noexcept
  __attribute__((alias ("_ZSt8to_charsPcS_u9__ieee128St12chars_formati")));
#else
to_chars_result
to_chars(char* first, char* last, _Float128 value) noexcept
{
  return __floating_to_chars_shortest(first, last, value, chars_format{});
}

to_chars_result
to_chars(char* first, char* last, _Float128 value, chars_format fmt) noexcept
{
  return __floating_to_chars_shortest(first, last, value, fmt);
}

to_chars_result
to_chars(char* first, char* last, _Float128 value, chars_format fmt,
	 int precision) noexcept
{
  return __floating_to_chars_precision(first, last, value, fmt, precision);
}
#endif
#endif

// Entrypoints for 16-bit floats.
[[gnu::cold]] to_chars_result
__to_chars_float16_t(char* first, char* last, float value,
		     chars_format fmt) noexcept
{
  return __floating_to_chars_shortest(first, last,
				      floating_type_float16_t{ value }, fmt);
}

[[gnu::cold]] to_chars_result
__to_chars_bfloat16_t(char* first, char* last, float value,
		      chars_format fmt) noexcept
{
  return __floating_to_chars_shortest(first, last,
				      floating_type_bfloat16_t{ value }, fmt);
}

#ifdef _GLIBCXX_LONG_DOUBLE_COMPAT
// Map the -mlong-double-64 long double overloads to the double overloads.
extern "C" to_chars_result
_ZSt8to_charsPcS_e(char* first, char* last, double value) noexcept
  __attribute__((alias ("_ZSt8to_charsPcS_d")));

extern "C" to_chars_result
_ZSt8to_charsPcS_eSt12chars_format(char* first, char* last, double value,
				   chars_format fmt) noexcept
  __attribute__((alias ("_ZSt8to_charsPcS_dSt12chars_format")));

extern "C" to_chars_result
_ZSt8to_charsPcS_eSt12chars_formati(char* first, char* last, double value,
				    chars_format fmt, int precision) noexcept
  __attribute__((alias ("_ZSt8to_charsPcS_dSt12chars_formati")));
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // _GLIBCXX_FLOAT_IS_IEEE_BINARY32 && _GLIBCXX_DOUBLE_IS_IEEE_BINARY64
