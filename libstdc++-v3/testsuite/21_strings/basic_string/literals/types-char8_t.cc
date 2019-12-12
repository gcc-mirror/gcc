// { dg-do compile { target c++14 } }
// { dg-options "-fchar8_t" }

// Copyright (C) 2013-2017 Free Software Foundation, Inc.
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

#include <string>
#include <type_traits>

void
test01()
{
  using namespace std::literals::string_literals;

  static_assert(std::is_same<decltype("Hello"s), std::string>::value,
		"\"Hello\"s is std::string");

#ifdef _GLIBCXX_USE_CHAR8_T
  static_assert(std::is_same<decltype(u8"Hello"s), std::u8string>::value,
		"u8\"Hello\"s is std::u8string");
#else
  static_assert(std::is_same<decltype(u8"Hello"s), std::string>::value,
		"u8\"Hello\"s is std::string");
#endif


#ifdef _GLIBCXX_USE_WCHAR_T
  static_assert(std::is_same<decltype(L"Hello"s), std::wstring>::value,
		"L\"Hello\"s is std::wstring");
#endif

  static_assert(std::is_same<decltype(u"Hello"s), std::u16string>::value,
		"u\"Hello\"s is std::u16string");

  static_assert(std::is_same<decltype(U"Hello"s), std::u32string>::value,
		"U\"Hello\"s is std::u32string");
}
