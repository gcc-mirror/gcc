// { dg-options "-fchar8_t" }
// { dg-do run { target c++17 } }

// Copyright (C) 2013-2024 Free Software Foundation, Inc.
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

#include <string_view>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std::literals::string_view_literals;

  std::string_view planet = "Mercury"sv;
  std::wstring_view wplanet = L"Venus"sv;
#ifdef _GLIBCXX_USE_CHAR8_T
  std::u8string_view u8planet = u8"Mars"sv;
#else
  std::string_view u8planet = u8"Mars"sv;
#endif
  std::u16string_view u16planet = u"Jupiter"sv;
  std::u32string_view u32planet = U"Saturn"sv;

  VERIFY( planet == std::string_view("Mercury") );
  VERIFY( wplanet == std::wstring_view(L"Venus") );
#ifdef _GLIBCXX_USE_CHAR8_T
  VERIFY( u8planet == std::u8string_view(u8"Mars") );
#else
  VERIFY( u8planet == std::string_view(u8"Mars") );
#endif
  VERIFY( u16planet == std::u16string_view(u"Jupiter") );
  VERIFY( u32planet == std::u32string_view(U"Saturn") );
}

void
test02()
{
  using namespace std::literals::string_view_literals;

  std::string_view planet_cratered = "Mercury\0cratered"sv;
  std::wstring_view wplanet_cratered = L"Venus\0cratered"sv;
#ifdef _GLIBCXX_USE_CHAR8_T
  std::u8string_view u8planet_cratered = u8"Mars\0cratered"sv;
#else
  std::string_view u8planet_cratered = u8"Mars\0cratered"sv;
#endif
  std::u16string_view u16planet_cratered = u"Jupiter\0cratered"sv;
  std::u32string_view u32planet_cratered = U"Saturn\0cratered"sv;

  VERIFY( planet_cratered == std::string_view("Mercury\0cratered", 16) );
  VERIFY( wplanet_cratered == std::wstring_view(L"Venus\0cratered", 14) );
#ifdef _GLIBCXX_USE_CHAR8_T
  VERIFY( u8planet_cratered == std::u8string_view(u8"Mars\0cratered", 13) );
#else
  VERIFY( u8planet_cratered == std::string_view(u8"Mars\0cratered", 13) );
#endif
  VERIFY( u16planet_cratered == std::u16string_view(u"Jupiter\0cratered", 16) );
  VERIFY( u32planet_cratered == std::u32string_view(U"Saturn\0cratered", 15) );
}

int
main()
{
  test01();
  test02();
}
