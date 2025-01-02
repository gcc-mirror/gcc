// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

#include <experimental/filesystem>
#include <string>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std::experimental::filesystem;
  using string_type = std::basic_string<path::value_type>;
  const string_type s{ 'a', 'b', 'c' };
  path p(s);

  VERIFY( p.native() == s );
  VERIFY( p.c_str() == s );
  VERIFY( static_cast<string_type>(p) == s );

  string_type s2 = p; // implicit conversion
  VERIFY( s2 == p.native() );
}

void
test02()
{
  using namespace std::experimental::filesystem;
  const char* s = "abc";
  path p(s);

  auto str = p.string<char>();
  VERIFY( str == u"abc" );
  VERIFY( str == p.string() );

#ifdef _GLIBCXX_USE_WCHAR_T
  auto strw = p.string<wchar_t>();
  VERIFY( strw == L"abc" );
  VERIFY( strw == p.wstring() );
#endif

  auto str16 = p.string<char16_t>();
  VERIFY( str16 == u"abc" );
  VERIFY( str16 == p.u16string() );

  auto str32 = p.string<char32_t>();
  VERIFY( str32 == U"abc" );
  VERIFY( str32 == p.u32string() );
}

void
test03()
{
  std::experimental::filesystem::path p;
  auto str8 = p.u8string();
  VERIFY( str8 == u8"" );
  auto str16 = p.u16string();
  VERIFY( str16 == u"" );
  auto str32 = p.u32string();
  VERIFY( str32 == U"" );
}

void
test04()
{
  // PR libstdc++/90281
  auto p = std::experimental::filesystem::u8path("\xf0\x9d\x84\x9e");
  auto str8 = p.u8string();
  VERIFY( str8 == u8"\U0001D11E" );
  auto str16 = p.u16string();
  VERIFY( str16 == u"\U0001D11E" );
  auto str32 = p.u32string();
  VERIFY( str32 == U"\U0001D11E" );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
