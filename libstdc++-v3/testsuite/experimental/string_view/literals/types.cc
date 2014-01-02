// { dg-options "-std=gnu++1y" }
// { dg-do compile }

// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

#include <experimental/string_view>
#include <type_traits>

void
test01()
{
  using namespace std::experimental::literals::string_view_literals;

  static_assert(std::is_same<decltype("Hello"sv), std::experimental::string_view>::value,
		"\"Hello\"s is std::string_view");

  static_assert(std::is_same<decltype(u8"Hello"sv), std::experimental::string_view>::value,
		"u8\"Hello\"s is std::string_view");

  static_assert(std::is_same<decltype(L"Hello"sv), std::experimental::wstring_view>::value,
		"L\"Hello\"s is std::wstring_view");

  static_assert(std::is_same<decltype(u"Hello"sv), std::experimental::u16string_view>::value,
		"u\"Hello\"s is std::u16string_view");

  static_assert(std::is_same<decltype(U"Hello"sv), std::experimental::u32string_view>::value,
		"U\"Hello\"s is std::u32string_view");
}
