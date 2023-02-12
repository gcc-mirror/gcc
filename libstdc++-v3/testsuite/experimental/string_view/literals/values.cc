// { dg-do run { target c++14 } }

// Copyright (C) 2013-2023 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

#ifdef _GLIBCXX_USE_CHAR8_T
using std::experimental::u8string_view;
#else
using u8string_view = std::experimental::string_view;
#endif

void
test01()
{
  using namespace std::experimental::literals::string_view_literals;

  std::experimental::string_view planet = "Mercury"sv;
  std::experimental::wstring_view wplanet = L"Venus"sv;
  u8string_view u8planet = u8"Mars"sv;
  std::experimental::u16string_view u16planet = u"Jupiter"sv;
  std::experimental::u32string_view u32planet = U"Saturn"sv;

  VERIFY( planet == std::experimental::string_view("Mercury") );
  VERIFY( wplanet == std::experimental::wstring_view(L"Venus") );
  VERIFY( u8planet == u8string_view(u8"Mars") );
  VERIFY( u16planet == std::experimental::u16string_view(u"Jupiter") );
  VERIFY( u32planet == std::experimental::u32string_view(U"Saturn") );
}

void
test02()
{
  using namespace std::experimental::literals::string_view_literals;

  std::experimental::string_view planet_cratered = "Mercury\0cratered"sv;
  std::experimental::wstring_view wplanet_cratered = L"Venus\0cratered"sv;
  u8string_view u8planet_cratered = u8"Mars\0cratered"sv;
  std::experimental::u16string_view u16planet_cratered = u"Jupiter\0cratered"sv;
  std::experimental::u32string_view u32planet_cratered = U"Saturn\0cratered"sv;

  VERIFY( planet_cratered ==
	  std::experimental::string_view("Mercury\0cratered", 16) );
  VERIFY( wplanet_cratered ==
	  std::experimental::wstring_view(L"Venus\0cratered", 14) );
  VERIFY( u8planet_cratered == u8string_view(u8"Mars\0cratered", 13) );
  VERIFY( u16planet_cratered ==
	  std::experimental::u16string_view(u"Jupiter\0cratered", 16) );
  VERIFY( u32planet_cratered ==
	  std::experimental::u32string_view(U"Saturn\0cratered", 15) );
}

int
main()
{
  test01();
  test02();
}
