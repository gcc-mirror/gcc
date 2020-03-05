// Copyright (C) 2016-2019 Free Software Foundation, Inc.
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

// { dg-options "-lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

#include <experimental/filesystem>
#include <string>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

template<typename C>
  using alloc = __gnu_test::uneq_allocator<C>;

void
test01()
{
  using namespace std::experimental::filesystem;
  path p;

  auto str = p.string<char>(alloc<char>(1));
  VERIFY( str == "" );
  VERIFY( str.get_allocator() == alloc<char>(1) );

#ifdef _GLIBCXX_USE_CHAR8_T
  auto str8 = p.string<char8_t>(alloc<char8_t>(1));
  VERIFY( str8 == u8"" );
  VERIFY( str8.get_allocator() == alloc<char8_t>(1) );
#endif

  auto strw = p.string<wchar_t>(alloc<wchar_t>(2));
  VERIFY( strw == L"" );
  VERIFY( strw.get_allocator() == alloc<wchar_t>(2) );

  auto str16 = p.string<char16_t>(alloc<char16_t>(3));
  VERIFY( str16 == u"" );
  VERIFY( str16.get_allocator() == alloc<char16_t>(3) );

  auto str32 = p.string<char32_t>(alloc<char32_t>(4));
  VERIFY( str32 == U"" );
  VERIFY( str32.get_allocator() == alloc<char32_t>(4) );
}

void
test02()
{
  using namespace std::experimental::filesystem;
  path p = "abcdefghijklmnopqrstuvwxyz";

  auto str = p.string<char>(alloc<char>(1));
  VERIFY( str == "abcdefghijklmnopqrstuvwxyz" );
  VERIFY( str.get_allocator() == alloc<char>(1) );

#ifdef _GLIBCXX_USE_CHAR8_T
  auto str8 = p.string<char8_t>(alloc<char8_t>(1));
  VERIFY( str8 == u8"abcdefghijklmnopqrstuvwxyz" );
  VERIFY( str8.get_allocator() == alloc<char8_t>(1) );
#endif

  auto strw = p.string<wchar_t>(alloc<wchar_t>(2));
  VERIFY( strw == L"abcdefghijklmnopqrstuvwxyz" );
  VERIFY( strw.get_allocator() == alloc<wchar_t>(2) );

  auto str16 = p.string<char16_t>(alloc<char16_t>(3));
  VERIFY( str16 == u"abcdefghijklmnopqrstuvwxyz" );
  VERIFY( str16.get_allocator() == alloc<char16_t>(3) );

  auto str32 = p.string<char32_t>(alloc<char32_t>(4));
  VERIFY( str32 == U"abcdefghijklmnopqrstuvwxyz" );
  VERIFY( str32.get_allocator() == alloc<char32_t>(4) );
}

int
main()
{
  test01();
  test02();
}
