// { dg-do run { target c++14 } }

// Copyright (C) 2013-2016 Free Software Foundation, Inc.
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

void
test01()
{
  using namespace std::experimental::literals::string_view_literals;

  std::experimental::string_view planet = "Mercury"sv;
#ifdef _GLIBCXX_USE_WCHAR_T
  std::experimental::wstring_view wplanet = L"Venus"sv;
#endif
  std::experimental::string_view u8planet = u8"Mars"sv;
  std::experimental::u16string_view u16planet = u"Jupiter"sv;
  std::experimental::u32string_view u32planet = U"Saturn"sv;

  VERIFY( planet == std::experimental::string_view("Mercury") );
#ifdef _GLIBCXX_USE_WCHAR_T
  VERIFY( wplanet == std::experimental::wstring_view(L"Venus") );
#endif
  VERIFY( u8planet == std::experimental::string_view(u8"Mars") );
  VERIFY( u16planet == std::experimental::u16string_view(u"Jupiter") );
  VERIFY( u32planet == std::experimental::u32string_view(U"Saturn") );
}

int
main()
{
  test01();
}
