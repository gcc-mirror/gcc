// { dg-do compile { target c++11 } }

// Copyright (C) 2015-2018 Free Software Foundation, Inc.
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

// 22.3.3.2.2  String conversions

#include <locale>
#include <string>
#include <type_traits>
#include <testsuite_allocator.h>

template<typename T>
  using alloc = __gnu_test::uneq_allocator<T>;

template<typename C>
  using Str = std::basic_string<C, std::char_traits<C>, alloc<C>>;

struct cvt : std::codecvt<wchar_t, char, std::mbstate_t> { };

using wconv = std::wstring_convert<cvt, wchar_t, alloc<wchar_t>, alloc<char>>;

static_assert( std::is_same<wconv::byte_string, Str<char>>::value,
	       "byte string is std::string" );
static_assert( std::is_same<wconv::wide_string, Str<wchar_t>>::value,
	       "wide string is std::wstring" );
