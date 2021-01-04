// { dg-do run { target c++14 } }

// Copyright (C) 2013-2021 Free Software Foundation, Inc.
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

// C++14 27.7.6 - Quoted manipulators		[quoted.manip]

#include <string>
#include <sstream>
#include <iomanip>
#include <testsuite_hooks.h>

void
test01()
{
  //  Basic test from paper.
  std::stringstream ss;
  std::string original = "foolish me";
  std::string round_trip;
  ss << std::quoted(original);
  ss >> std::quoted(round_trip);
  VERIFY( original == round_trip );
}

void
test02()
{
  //  Test skipws correctness.
  std::stringstream ss;
  ss << std::quoted("Hello Goodbye") << ' ' << 1 << ' ' << 2;
  std::string song;
  int thing1, thing2;
  ss >> std::quoted(song) >> thing1 >> thing2;
  VERIFY( song == "Hello Goodbye" );
  VERIFY( thing1 == 1 );
  VERIFY( thing2 == 2 );
}

void
test03()
{
  //  Test read of unquoted string.
  std::stringstream ss;
  ss << "Alpha Omega";
  std::string testit;
  ss >> std::quoted(testit);
  VERIFY( testit == "Alpha" );
}

auto
test04(const std::string& message)
{
  //  Test 'const basic_string&'
  std::stringstream ss;
  ss << "**  Error: " << std::quoted(message) << "  **";
  return ss.str();
}

int
main()
{
  test01();
  test02();
  test03();
  auto ss = test04("My biscuits are burnin'!");
  VERIFY( ss == "**  Error: \"My biscuits are burnin'!\"  **" );

  return 0;
}
