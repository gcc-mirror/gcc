// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

// Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

// C++17 30.7.8 - Quoted manipulators		[quoted.manip]

#include <string_view>
#include <sstream>
#include <iomanip>
#include <testsuite_hooks.h>

void
test01()
{
  std::stringstream ss;
  const std::string_view original = R"(This "string" will be \"quoted\")";
  std::string raw, round_trip;
  ss << std::quoted(original);
  raw = ss.str();
  VERIFY( raw == R"("This \"string\" will be \\\"quoted\\\"")" );
  ss >> std::quoted(round_trip);
  VERIFY( original == round_trip );
}

void
test02()
{
  std::stringstream ss;
  const std::string_view original = R"(This "string" will be \"quoted\")";
  std::string raw, round_trip;
  ss << std::quoted(original, '\'', '!');
  raw = ss.str();
  VERIFY( raw == R"('This "string" will be \"quoted\"')" );
  ss >> std::quoted(round_trip, '\'', '!');
  VERIFY( original == round_trip );
}

void
test03()
{
  std::stringstream ss;
  const std::string_view original = R"(This 'string' will be !'quoted!')";
  std::string raw, round_trip;
  ss << std::quoted(original, '\'', '!');
  raw = ss.str();
  VERIFY( raw == R"('This !'string!' will be !!!'quoted!!!'')" );
  ss >> std::quoted(round_trip, '\'', '!');
  VERIFY( original == round_trip );
}

int
main()
{
  test01();
  test02();
  test03();
}
