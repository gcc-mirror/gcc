// Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

template<typename T>
  constexpr T max = std::numeric_limits<T>::max();
// Largest representable power of two (i.e. has most significant bit set)
template<typename T>
  constexpr T maxpow2 = T(1) << (std::numeric_limits<T>::digits - 1);

// Detect whether std::ceil2(N) is a constant expression.
template<auto N, typename = void>
  struct ceil2_valid
  : std::false_type { };

template<auto N>
  struct ceil2_valid<N, std::void_t<char[(std::ceil2(N), 1)]>>
  : std::true_type { };

template<typename UInt>
constexpr auto
test(UInt x)
-> decltype(std::ceil2(x))
{
  static_assert( noexcept(std::ceil2(x)) );

  static_assert( std::ceil2(UInt(0)) == 1 );
  static_assert( std::ceil2(UInt(1)) == 1 );
  static_assert( std::ceil2(UInt(2)) == 2 );
  static_assert( std::ceil2(UInt(3)) == 4 );
  static_assert( std::ceil2(UInt(4)) == 4 );
  static_assert( std::ceil2(UInt(0x11)) == 0x20 );
  static_assert( std::ceil2(UInt(0x20)) == 0x20 );

  if constexpr (std::numeric_limits<UInt>::digits > 8)
  {
    static_assert( std::ceil2(UInt(0x201)) == 0x400 );
    static_assert( std::ceil2(UInt(0x8ff)) == 0x1000 );
    static_assert( std::ceil2(UInt(0x1000)) == 0x1000 );
  }

  if constexpr (std::numeric_limits<UInt>::digits > 32)
  {
    static_assert( std::ceil2(UInt(0xabcdef)) == 0x1000000 );
    static_assert( std::ceil2(UInt(0x1000000)) == 0x1000000 );
    static_assert( std::ceil2(UInt(0x1000001)) == 0x2000000 );
  }

  if constexpr (std::numeric_limits<UInt>::digits > 64)
  {
    static_assert( std::ceil2(UInt(1) << 64) == (UInt(1) << 64) );
    static_assert( std::ceil2(UInt(3) << 64) == (UInt(4) << 64) );
  }

  constexpr UInt msb = maxpow2<UInt>;
  static_assert( ceil2_valid<msb>() );
  static_assert( std::ceil2( msb ) == msb );
  static_assert( std::ceil2( UInt(msb - 1) ) == msb );
  static_assert( std::ceil2( UInt(msb - 2) ) == msb );
  static_assert( std::ceil2( UInt(msb - 3) ) == msb );

  // P1355R2: not a constant expression if the result is not representable
  static_assert( !ceil2_valid<UInt(msb + 1)>() );
  static_assert( !ceil2_valid<max<UInt>>() );
  static_assert( !ceil2_valid<UInt(max<UInt> - 1)>() );
  static_assert( !ceil2_valid<UInt(max<UInt> - 2)>() );

  return true;
}

static_assert( test( (unsigned char)0 ) );
static_assert( test( (unsigned short)0 ) );
static_assert( test( (unsigned int)0 ) );
static_assert( test( (unsigned long)0 ) );
static_assert( test( (unsigned long long)0 ) );

// std::ceil2(T) shall not participate in overload resolution
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
#if defined(__GLIBCXX_TYPE_INT_N_3)
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_3)0 ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_3)0 ).did_not_match() );
#endif

#include <cstddef>
static_assert( test( (std::byte)0 ).did_not_match() );
