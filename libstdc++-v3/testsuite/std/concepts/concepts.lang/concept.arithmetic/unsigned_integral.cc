// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <concepts>

// unsigned integer types
static_assert( std::unsigned_integral<unsigned char> );
static_assert( std::unsigned_integral<unsigned short> );
static_assert( std::unsigned_integral<unsigned int> );
static_assert( std::unsigned_integral<unsigned long> );
static_assert( std::unsigned_integral<unsigned long long> );

// signed integer types
static_assert( !std::unsigned_integral<signed char> );
static_assert( !std::unsigned_integral<signed short> );
static_assert( !std::unsigned_integral<signed int> );
static_assert( !std::unsigned_integral<signed long> );
static_assert( !std::unsigned_integral<signed long long> );

// other integral types
static_assert( std::unsigned_integral<bool> );
static_assert( std::is_signed_v<char> ? true : std::unsigned_integral<char> );
static_assert( std::unsigned_integral<char16_t> );
static_assert( std::unsigned_integral<char32_t> );
static_assert( std::is_signed_v<wchar_t> ? true : std::unsigned_integral<wchar_t> );
#ifdef _GLIBCXX_USE_CHAR8_T
static_assert( std::unsigned_integral<char8_t> );
#endif

#ifdef __GLIBCXX_TYPE_INT_N_0
static_assert( !std::unsigned_integral<signed __GLIBCXX_TYPE_INT_N_0> );
static_assert( std::unsigned_integral<unsigned __GLIBCXX_TYPE_INT_N_0> );
#endif

static_assert( !std::unsigned_integral<void> );
static_assert( !std::unsigned_integral<float> );
static_assert( !std::unsigned_integral<unsigned&> );
static_assert( !std::unsigned_integral<unsigned&> );
static_assert( !std::unsigned_integral<unsigned&&> );
static_assert( !std::unsigned_integral<const unsigned&> );
static_assert( !std::unsigned_integral<unsigned[]> );
static_assert( !std::unsigned_integral<unsigned[2]> );
static_assert( !std::unsigned_integral<unsigned()> );
static_assert( !std::unsigned_integral<unsigned(*)()> );
static_assert( !std::unsigned_integral<unsigned(&)()> );

enum E { };
static_assert( !std::unsigned_integral<E> );
enum class CE { };
static_assert( !std::unsigned_integral<CE> );
struct A { };
static_assert( !std::unsigned_integral<A> );
union B { };
static_assert( !std::unsigned_integral<B> );
