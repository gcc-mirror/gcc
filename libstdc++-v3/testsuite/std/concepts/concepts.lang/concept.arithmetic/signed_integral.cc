// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

#include <concepts>

// signed integer types
static_assert( std::signed_integral<signed char> );
static_assert( std::signed_integral<signed short> );
static_assert( std::signed_integral<signed int> );
static_assert( std::signed_integral<signed long> );
static_assert( std::signed_integral<signed long long> );

// unsigned integer types
static_assert( !std::signed_integral<unsigned char> );
static_assert( !std::signed_integral<unsigned short> );
static_assert( !std::signed_integral<unsigned int> );
static_assert( !std::signed_integral<unsigned long> );
static_assert( !std::signed_integral<unsigned long long> );

// other integral types
static_assert( !std::signed_integral<bool> );
static_assert( std::is_signed_v<char> ? std::signed_integral<char> : true );
static_assert( !std::signed_integral<char16_t> );
static_assert( !std::signed_integral<char32_t> );
static_assert( std::is_signed_v<wchar_t> ? std::signed_integral<wchar_t> : true );
#ifdef _GLIBCXX_USE_CHAR8_T
static_assert( !std::signed_integral<char8_t> );
#endif

#ifdef __GLIBCXX_TYPE_INT_N_0
static_assert( std::signed_integral<signed __GLIBCXX_TYPE_INT_N_0> );
static_assert( !std::signed_integral<unsigned __GLIBCXX_TYPE_INT_N_0> );
#endif

static_assert( !std::signed_integral<void> );
static_assert( !std::signed_integral<float> );
static_assert( !std::signed_integral<int&> );
static_assert( !std::signed_integral<int&> );
static_assert( !std::signed_integral<int&&> );
static_assert( !std::signed_integral<const int&> );
static_assert( !std::signed_integral<int[]> );
static_assert( !std::signed_integral<int[2]> );
static_assert( !std::signed_integral<int()> );
static_assert( !std::signed_integral<int(*)()> );
static_assert( !std::signed_integral<int(&)()> );

enum E { };
static_assert( !std::signed_integral<E> );
enum class CE { };
static_assert( !std::signed_integral<CE> );
struct A { };
static_assert( !std::signed_integral<A> );
union B { };
static_assert( !std::signed_integral<B> );
