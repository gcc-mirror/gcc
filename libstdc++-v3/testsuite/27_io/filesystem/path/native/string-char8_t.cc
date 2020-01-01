// Copyright (C) 2016-2020 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17 -fchar8_t" }
// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <string>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std::filesystem;
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
  using namespace std::filesystem;
  const char* s = "abc";
  path p(s);

  auto str = p.string<char>();
  VERIFY( str == "abc" );
  VERIFY( str == p.string() );

  auto strw = p.string<wchar_t>();
  VERIFY( strw == L"abc" );
  VERIFY( strw == p.wstring() );

#ifdef _GLIBCXX_USE_CHAR8_T
  auto str8 = p.string<char8_t>();
  VERIFY( str8 == u8"abc" );
  VERIFY( str8 == p.u8string() );
#endif

  auto str16 = p.string<char16_t>();
  VERIFY( str16 == u"abc" );
  VERIFY( str16 == p.u16string() );

  auto str32 = p.string<char32_t>();
  VERIFY( str32 == U"abc" );
  VERIFY( str32 == p.u32string() );
}

int
main()
{
  test01();
  test02();
}
