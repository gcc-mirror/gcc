// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <bit>
#include <limits>

template<typename UInt>
constexpr bool
test_negative_shifts()
{
  constexpr unsigned digits = std::numeric_limits<UInt>::digits;

  UInt xarr[] = { (UInt)-1, 0, 1, 3, 6, 7, 0x10, 0x11, 0x22, 0x44, 0x80 };
  int sarr[] = { 1, 4, 5, digits - 1, digits };
  for (UInt x : xarr)
    for (int s : sarr)
      if (std::rotr(x, -s) != std::rotl(x, s))
	return false;
  return true;
}

template<typename UInt>
constexpr auto
test(UInt x)
-> decltype(std::rotr(x, 0))
{
  static_assert( noexcept(std::rotr(x, 0)) );

  constexpr unsigned digits = std::numeric_limits<UInt>::digits;

  static_assert( std::rotr((UInt)0, 0) == 0 );
  static_assert( std::rotr((UInt)0, 1) == 0 );
  static_assert( std::rotr((UInt)0, 4) == 0 );
  static_assert( std::rotr((UInt)0, 8) == 0 );
  static_assert( std::rotr((UInt)-1, 0) == (UInt)-1 );
  static_assert( std::rotr((UInt)-1, 1) == (UInt)-1 );
  static_assert( std::rotr((UInt)-1, 4) == (UInt)-1 );
  static_assert( std::rotr((UInt)-1, 8) == (UInt)-1 );

  static_assert( std::rotr((UInt)128, 0) == (UInt)128 >> 0 );
  static_assert( std::rotr((UInt)128, 1) == (UInt)128 >> 1 );
  static_assert( std::rotr((UInt)128, 4) == (UInt)128 >> 4 );
  static_assert( std::rotr((UInt)1, digits) == (UInt)1 );
  static_assert( std::rotr((UInt)7, digits) == (UInt)7 );
  static_assert( std::rotr((UInt)6, digits - 1) == (UInt)12 );
  static_assert( std::rotr((UInt)36, digits - 2) == (UInt)144 );

  static_assert( std::rotr((UInt)0b0110'1100, 1) == 0b0011'0110 );
  static_assert( std::rotr((UInt)0b0110'1100, digits - 1) == 0b1101'1000 );

  static_assert( std::rotr((UInt)0x01, 0 ) == 0x01 );
  static_assert( std::rotr((UInt)0x10, 0 ) == 0x10 );
  static_assert( std::rotr((UInt)0x10, 1 ) == 0x08 );
  static_assert( std::rotr((UInt)0x10, 2 ) == 0x04 );
  static_assert( std::rotr((UInt)0x10, 3 ) == 0x02 );
  static_assert( std::rotr((UInt)0x11, digits - 1 ) == 0x22 );
  static_assert( std::rotr((UInt)0x11, digits - 2 ) == 0x44 );

  if constexpr (std::numeric_limits<UInt>::digits > 8)
  {
    static_assert( std::rotr((UInt)0b0011'0111, 3)
			      == (0b0110 | ((UInt)0b0111 << digits - 3)) );
    static_assert( std::rotr((UInt)0b1010'0101, 4)
			      == (0b1010 | ((UInt)0b0101 << digits - 4)) );
  }

  static_assert( test_negative_shifts<UInt>() );

  return true;
}

static_assert( test( (unsigned char)0 ) );
static_assert( test( (unsigned short)0 ) );
static_assert( test( (unsigned int)0 ) );
static_assert( test( (unsigned long)0 ) );
static_assert( test( (unsigned long long)0 ) );

// std::rotr(T) shall not participate in overload resolution
// unless T is an unsigned integer type.
struct X { constexpr bool did_not_match() { return true; } };
constexpr X test(...) { return X{}; }
static_assert( test( (bool)0 ).did_not_match() );
static_assert( test( (char)0 ).did_not_match() );
static_assert( test( (int)0 ).did_not_match() );
static_assert( test( (char16_t)0 ).did_not_match() );
static_assert( test( (float)0 ).did_not_match() );
static_assert( test( (void*)0 ).did_not_match() );
static_assert( test( X{} ).did_not_match() );
enum E : unsigned { e };
static_assert( test( e ).did_not_match() );

#if !defined(__STRICT_ANSI__) && defined __SIZEOF_INT128__
static_assert( test( (unsigned __int128)0 ) );
static_assert( test( (__int128)0 ).did_not_match() );
#endif
#if defined(__GLIBCXX_TYPE_INT_N_0)
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_0)0 ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_0)0 ).did_not_match() );
#endif
#if defined(__GLIBCXX_TYPE_INT_N_1)
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_1)0 ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_1)0 ).did_not_match() );
#endif
#if defined(__GLIBCXX_TYPE_INT_N_2)
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_2)0 ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_2)0 ).did_not_match() );
#endif
#if defined(__GLIBCXX_TYPE_INT_N_3)
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_3)0 ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_3)0 ).did_not_match() );
#endif

#include <cstddef>
static_assert( test( (std::byte)0 ).did_not_match() );
