// { dg-options "-std=c++0x" }
// { dg-do run { xfail *-*-* } }

// 2012-08-20  Benjamin Kosnik <bkoz@redhat.com>
//
// Copyright (C) 2012-2013 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// basic_regex constructors + raw string literals

#include <regex>
#include <testsuite_regex.h>

void
test01()
{
  using namespace __gnu_test;

  // raw string literals

  //string_type sre0(R"(\d{3}-\d{3}-\d{4})"); // expected fail

  string_type sre1(R"( this\n  and new : forms\n  )");

  string_type sre2(R"([:num:]{3}-[:num:]{3}-[:num:]{4})");

  // 1
  regex_type re(R"( this\n  and new : forms\n  )", std::regex::basic);

  // 2
  regex_sanity_check(sre1);
}

int main()
{
  test01();
  return 0;
}
