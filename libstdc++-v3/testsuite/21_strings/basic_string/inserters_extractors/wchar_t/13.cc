// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do run }

#include <string>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  const std::wstring str = L"1234";
  std::wistringstream in(str);
  std::wstring buf;
  in.width(4);
  in >> buf;
  VERIFY( !in.eof() ); // should stop after reading 4 chars
  VERIFY( buf == str );
}

struct WT : std::char_traits<wchar_t> { };

void
test02()
{
  const std::basic_string<wchar_t, WT> str = L"1234";
  std::basic_istringstream<wchar_t, WT> in(str);
  std::basic_string<wchar_t, WT> buf;
  in.width(4);
  in >> buf;
  VERIFY( !in.eof() ); // should stop after reading 4 chars
  VERIFY( buf == str );
}

int
main()
{
  test01();
  test02();
}
