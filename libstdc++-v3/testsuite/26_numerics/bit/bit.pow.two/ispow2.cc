// Copyright (C) 2018 Free Software Foundation, Inc.
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

template<typename UInt>
constexpr auto
test(UInt x)
-> decltype(std::ispow2(x))
{
  static_assert( noexcept(std::ispow2(x)) );

  static_assert( ! std::ispow2( (UInt)0 ) );
  static_assert( ! std::ispow2( (UInt)-1 ) );
  static_assert( ! std::ispow2( (UInt)3 ) );
  static_assert( ! std::ispow2( (UInt)0x0f ) );
  static_assert( ! std::ispow2( (UInt)0xff ) );
  static_assert( ! std::ispow2( (UInt)0x0a ) );
  static_assert( ! std::ispow2( (UInt)0xa0 ) );

  constexpr UInt one = 1;
  static_assert( std::ispow2( (UInt)(one << 0) ) );

  static_assert( std::ispow2( (UInt)(one << 1) ) );
  static_assert( std::ispow2( (UInt)(one << 2) ) );
  static_assert( std::ispow2( (UInt)(one << 3) ) );
  static_assert( std::ispow2( (UInt)(one << 4) ) );
  static_assert( std::ispow2( (UInt)(one << 5) ) );
  static_assert( std::ispow2( (UInt)(one << 6) ) );
  static_assert( std::ispow2( (UInt)(one << 7) ) );

  if constexpr (std::numeric_limits<UInt>::digits > 8)
  {
    static_assert( std::ispow2( (UInt)(one << 8) ) );
    static_assert( std::ispow2( (UInt)(one << 9) ) );
    static_assert( std::ispow2( (UInt)(one << 10) ) );
    static_assert( std::ispow2( (UInt)(one << 11) ) );
    static_assert( std::ispow2( (UInt)(one << 12) ) );
    static_assert( std::ispow2( (UInt)(one << 13) ) );
    static_assert( std::ispow2( (UInt)(one << 14) ) );
    static_assert( std::ispow2( (UInt)(one << 15) ) );

    static_assert( ! std::ispow2( (UInt)0xf000 ) );
    static_assert( ! std::ispow2( (UInt)0xff00 ) );
    static_assert( ! std::ispow2( (UInt)0xf0f0 ) );
    static_assert( ! std::ispow2( (UInt)0xf00f ) );
    static_assert( ! std::ispow2( (UInt)0x0f0f ) );
    static_assert( ! std::ispow2( (UInt)0x00ff ) );
  }

  if constexpr (std::numeric_limits<UInt>::digits > 16)
  {
    static_assert( std::ispow2( (UInt)(one << 16) ) );
    static_assert( std::ispow2( (UInt)(one << 17) ) );
    static_assert( ! std::ispow2( (UInt)((one << 16) + 1) ) );
    static_assert( ! std::ispow2( (UInt)((one << 16) + 0x10) ) );
  }

  // msp340 target has 20-bit __GLIBCXX_TYPE_INT_N_0 type
  if constexpr (std::numeric_limits<UInt>::digits > 20)
  {
    static_assert( std::ispow2( (UInt)(one << 20) ) );
    static_assert( std::ispow2( (UInt)(one << 21) ) );
    static_assert( std::ispow2( (UInt)(one << 24) ) );
    static_assert( std::ispow2( (UInt)(one << 28) ) );
    static_assert( std::ispow2( (UInt)(one << 31) ) );
  }

  if constexpr (std::numeric_limits<UInt>::digits > 32)
  {
    static_assert( std::ispow2( (UInt)(one << 32) ) );
    static_assert( std::ispow2( (UInt)(one << 33) ) );
    static_assert( std::ispow2( (UInt)(one << 41) ) );

    static_assert( ! std::ispow2( (UInt)((one << 32) + 1) ) );
    static_assert( ! std::ispow2( (UInt)((one << 32) + (one << 31)) ) );
    static_assert( ! std::ispow2( (UInt)((one << 33) + 1) ) );
    static_assert( ! std::ispow2( (UInt)((one << 33) + (one << 32)) ) );
  }

  if constexpr (std::numeric_limits<UInt>::digits == 64)
  {
    static_assert( std::ispow2( (UInt)(one << 63) ) );

    static_assert( ! std::ispow2( (UInt)((one << 63) + 1) ) );
    static_assert( ! std::ispow2( (UInt)((one << 63) + (one << 8)) ) );
    static_assert( ! std::ispow2( (UInt)((one << 63) + (one << 32)) ) );
  }
  return true;
}

static_assert( test( (unsigned char)0 ) );
static_assert( test( (unsigned short)0 ) );
static_assert( test( (unsigned int)0 ) );
static_assert( test( (unsigned long)0 ) );
static_assert( test( (unsigned long long)0 ) );

// std::ispow2(T) shall not participate in overload resolution
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

#ifndef __STRICT_ANSI__
#include <cstddef>
static_assert( std::ispow2(std::byte{0}) == false );
static_assert( std::ispow2(std::byte{1}) == true );
static_assert( std::ispow2(std::byte{2}) == true );
static_assert( std::ispow2(std::byte{3}) == false );
static_assert( std::ispow2(std::byte{100}) == false );
static_assert( std::ispow2(std::byte{128}) == true );
static_assert( std::ispow2(std::byte{255}) == false );
#else
static_assert( test( (std::byte)0 ).did_not_match() );
#endif

#if !defined(__STRICT_ANSI__) && defined _GLIBCXX_USE_INT128
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
