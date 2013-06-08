// { dg-do run }
// { dg-options "-std=gnu++1y" }

// Copyright (C) 2013 Free Software Foundation, Inc.
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

// 27.7.6 - Quoted manipulators		[quoted.manip]

#include <string>
#include <sstream>
#include <iomanip>
#include <testsuite_hooks.h>

void
test01()
{
  //  Basic test from paper.
  bool test [[gnu::unused]] = true;
  std::wstringstream ss;
  std::wstring original = L"foolish me";
  std::wstring round_trip;
  ss << std::quoted(original);
  ss >> std::quoted(round_trip);
  VERIFY( original == round_trip );
}

void
test02()
{
  //  Test skipws correctness.
  bool test [[gnu::unused]] = true;
  std::wstringstream ss;
  ss << std::quoted(L"Hello Goodbye") << L' ' << 1 << L' ' << 2;
  std::wstring song;
  int thing1, thing2;
  ss >> std::quoted(song) >> thing1 >> thing2;
  VERIFY( song == L"Hello Goodbye" );
  VERIFY( thing1 == 1 );
  VERIFY( thing2 == 2 );
}

void
test03()
{
  //  Test read of unquoted string.
  bool test [[gnu::unused]] = true;
  std::wstringstream ss;
  ss << L"Alpha Omega";
  std::wstring testit;
  ss >> std::quoted(testit);
  VERIFY( testit == L"Alpha" );
}

auto
test04(const std::wstring& message)
{
  //  Test 'const basic_string&'
  bool test [[gnu::unused]] = true;
  std::wstringstream ss;
  ss << L"**  Error: " << std::quoted(message) << L"  **";
  return ss.str();
}

int
main()
{
  test01();
  test02();
  test03();
  auto ss = test04(L"My biscuits are burnin'!");
  VERIFY( ss == L"**  Error: \"My biscuits are burnin'!\"  **" );

  return 0;
}
