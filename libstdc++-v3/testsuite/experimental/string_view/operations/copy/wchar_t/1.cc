// { dg-do run { target c++14 } }

// Copyright (C) 2013-2019 Free Software Foundation, Inc.
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

// basic_string_view::copy

#include <experimental/string_view>
#include <testsuite_hooks.h>

void
test01()
{
  typedef std::experimental::wstring_view::size_type csize_type;
  csize_type csz01;

  const wchar_t str_lit01[] = L"123456789A";
  const std::experimental::wstring_view str01(str_lit01);
  wchar_t buffer[4] = { 0 };

  csize_type len = str01.copy(buffer, sizeof(buffer), 8);
  VERIFY( 2 == len );
  VERIFY( L'9' == buffer[0] );
}

int
main()
{ 
  test01();

  return 0;
}
