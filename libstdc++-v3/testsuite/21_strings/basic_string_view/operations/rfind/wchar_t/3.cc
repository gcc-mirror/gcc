// { dg-do run { target c++17 } }

// Copyright (C) 2013-2025 Free Software Foundation, Inc.
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

// basic_string_view::find_last_not_of

void
test03()
{
  typedef std::wstring_view::size_type csize_type;
  std::wstring_view::size_type pos;
  csize_type npos = std::wstring_view::npos;

  std::wstring_view x;
  pos = x.find_last_not_of(L'X');
  VERIFY( pos == npos );
  pos = x.find_last_not_of(L"XYZ");
  VERIFY( pos == npos );

  std::wstring_view y(L"a");
  pos = y.find_last_not_of(L'X');
  VERIFY( pos == 0 );
  pos = y.find_last_not_of(L'a');
  VERIFY( pos == npos );
  pos = y.find_last_not_of(L"XYZ");
  VERIFY( pos == 0 );
  pos = y.find_last_not_of(L"a");
  VERIFY( pos == npos );

  std::wstring_view z(L"ab");
  pos = z.find_last_not_of(L'X');
  VERIFY( pos == 1 );
  pos = z.find_last_not_of(L"XYZ");
  VERIFY( pos == 1 );
  pos = z.find_last_not_of(L'b');
  VERIFY( pos == 0 );
  pos = z.find_last_not_of(L"Xb");
  VERIFY( pos == 0 );
  pos = z.find_last_not_of(L"Xa");
  VERIFY( pos == 1 );
}
int
main()
{
  test03();

  return 0;
}
