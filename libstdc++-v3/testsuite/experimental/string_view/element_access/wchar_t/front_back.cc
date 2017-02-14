// { dg-do run { target c++14 } }
// { dg-require-string-conversions "" }

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

#include <experimental/string_view>
#include <testsuite_hooks.h>

void
test01()
{
  std::experimental::wstring_view str(L"ramifications");
  const std::experimental::wstring_view cstr(L"melodien");

  VERIFY( str.front() == L'r' );
  VERIFY( str.back() == L's' );
  VERIFY( cstr.front() == L'm' );
  VERIFY( cstr.back() == L'n' );
}

int
main()
{
  test01();

  return 0;
}
