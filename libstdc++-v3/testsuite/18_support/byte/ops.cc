// Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-do compile { target c++17 } }

#include <cstddef>

constexpr bool is_byte(std::byte) { return true; }
template<typename T> constexpr bool is_byte(T) { return false; }

template<typename T>
constexpr bool test_lshift_assign(unsigned char c, T t)
{
  std::byte b{c};
  b <<= t;
  return b == std::byte(c << t);
}

static_assert( test_lshift_assign(0, 1) );
static_assert( test_lshift_assign(0, 1u) );
static_assert( test_lshift_assign(0, 1ll) );
static_assert( test_lshift_assign(4, 1) );
static_assert( test_lshift_assign(9, 2u) );
static_assert( test_lshift_assign(16, 3ll) );
static_assert( test_lshift_assign(127, 1) );
static_assert( test_lshift_assign(255, 2u) );
static_assert( test_lshift_assign(63, 3ll) );

template<typename T>
constexpr bool test_lshift(unsigned char c, T t)
{
  const std::byte b{c};
  const std::byte b2 = b << t;
  return b2 == std::byte(c << t);
}

static_assert( test_lshift(0, 1) );
static_assert( test_lshift(0, 1u) );
static_assert( test_lshift(0, 1ll) );
static_assert( test_lshift(4, 1) );
static_assert( test_lshift(9, 2u) );
static_assert( test_lshift(16, 3ll) );
static_assert( test_lshift(127, 1) );
static_assert( test_lshift(255, 2u) );
static_assert( test_lshift(63, 3ll) );

template<typename T>
constexpr bool test_rshift_assign(unsigned char c, T t)
{
  std::byte b{c};
  b >>= t;
  return b == std::byte(c >> t);
}

static_assert( test_rshift_assign(0, 1) );
static_assert( test_rshift_assign(0, 1u) );
static_assert( test_rshift_assign(0, 1ll) );
static_assert( test_rshift_assign(4, 1) );
static_assert( test_rshift_assign(9, 2u) );
static_assert( test_rshift_assign(16, 3ll) );
static_assert( test_rshift_assign(127, 1) );
static_assert( test_rshift_assign(255, 2u) );
static_assert( test_rshift_assign(63, 3ll) );

template<typename T>
constexpr bool test_rshift(unsigned char c, T t)
{
  const std::byte b{c};
  const std::byte b2 = b >> t;
  return b2 == std::byte(c >> t);
}

static_assert( test_rshift(0, 1) );
static_assert( test_rshift(0, 1u) );
static_assert( test_rshift(0, 1ll) );
static_assert( test_rshift(4, 1) );
static_assert( test_rshift(9, 2u) );
static_assert( test_rshift(16, 3ll) );
static_assert( test_rshift(127, 1) );
static_assert( test_rshift(255, 2u) );
static_assert( test_rshift(63, 3ll) );

constexpr bool test_or_assign(unsigned char l, unsigned char r)
{
  std::byte b{l};
  b |= std::byte{r};
  return b == std::byte(l | r);
}

static_assert( test_or_assign(0, 1) );
static_assert( test_or_assign(4, 1) );
static_assert( test_or_assign(9, 2) );
static_assert( test_or_assign(16, 3) );
static_assert( test_or_assign(63, 3) );
static_assert( test_or_assign(127, 1) );
static_assert( test_or_assign(255, 2) );

constexpr bool test_or(unsigned char l, unsigned char r)
{
  const std::byte b1{l};
  const std::byte b2{r};
  return (b1 | b2) == std::byte(l | r);
}

static_assert( test_or(0, 1) );
static_assert( test_or(0, 1u) );
static_assert( test_or(0, 1ll) );
static_assert( test_or(4, 1) );
static_assert( test_or(9, 2u) );
static_assert( test_or(16, 3ll) );
static_assert( test_or(127, 1) );
static_assert( test_or(255, 2u) );
static_assert( test_or(63, 3ll) );

constexpr bool test_and_assign(unsigned char l, unsigned char r)
{
  std::byte b{l};
  b &= std::byte{r};
  return b == std::byte(l & r);
}

static_assert( test_and_assign(0, 1) );
static_assert( test_and_assign(0, 1u) );
static_assert( test_and_assign(0, 1ll) );
static_assert( test_and_assign(4, 1) );
static_assert( test_and_assign(9, 2u) );
static_assert( test_and_assign(16, 3ll) );
static_assert( test_and_assign(127, 1) );
static_assert( test_and_assign(255, 2u) );
static_assert( test_and_assign(63, 3ll) );

constexpr bool test_and(unsigned char l, unsigned char r)
{
  const std::byte b1{l};
  const std::byte b2{r};
  return (b1 & b2) == std::byte(l & r);
}

static_assert( test_and(0, 1) );
static_assert( test_and(0, 1u) );
static_assert( test_and(0, 1ll) );
static_assert( test_and(4, 1) );
static_assert( test_and(9, 2u) );
static_assert( test_and(16, 3ll) );
static_assert( test_and(127, 1) );
static_assert( test_and(255, 2u) );
static_assert( test_and(63, 3ll) );

constexpr bool test_xor_assign(unsigned char l, unsigned char r)
{
  std::byte b{l};
  b ^= std::byte{r};
  return b == std::byte(l ^ r);
}

static_assert( test_xor_assign(0, 1) );
static_assert( test_xor_assign(0, 1u) );
static_assert( test_xor_assign(0, 1ll) );
static_assert( test_xor_assign(4, 1) );
static_assert( test_xor_assign(9, 2u) );
static_assert( test_xor_assign(16, 3ll) );
static_assert( test_xor_assign(127, 1) );
static_assert( test_xor_assign(255, 2u) );
static_assert( test_xor_assign(63, 3ll) );

constexpr bool test_xor(unsigned char l, unsigned char r)
{
  const std::byte b1{l};
  const std::byte b2{r};
  return (b1 ^ b2) == std::byte(l ^ r);
}

static_assert( test_xor(0, 1) );
static_assert( test_xor(0, 1u) );
static_assert( test_xor(0, 1ll) );
static_assert( test_xor(4, 1) );
static_assert( test_xor(9, 2u) );
static_assert( test_xor(16, 3ll) );
static_assert( test_xor(127, 1) );
static_assert( test_xor(255, 2u) );
static_assert( test_xor(63, 3ll) );

constexpr bool test_complement(unsigned char c)
{
  const std::byte b{c};
  return ~b == std::byte(~c);
}

static_assert( test_complement(0) );
static_assert( test_complement(4) );
static_assert( test_complement(9) );
static_assert( test_complement(16) );
static_assert( test_complement(63) );
static_assert( test_complement(127) );
static_assert( test_complement(255) );

template<typename T>
constexpr bool test_to_integer(unsigned char c)
{
  std::byte b{c};
  return std::to_integer<T>(b) == T(c);
}

static_assert( test_to_integer<int>(0) );
static_assert( test_to_integer<int>(255) );
static_assert( test_to_integer<signed char>(0) );
static_assert( test_to_integer<signed char>(255) );
static_assert( test_to_integer<unsigned>(0) );
static_assert( test_to_integer<unsigned>(255) );
#ifdef _GLIBCXX_USE_CHAR8_T
static_assert( test_to_integer<char8_t>(0) );
static_assert( test_to_integer<char8_t>(255) );
#endif
static_assert( test_to_integer<char16_t>(0) );
static_assert( test_to_integer<char16_t>(255) );
static_assert( test_to_integer<char32_t>(0) );
static_assert( test_to_integer<char32_t>(255) );
