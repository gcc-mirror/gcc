// Copyright (C) 2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <string_view>
#include <vector>
#include <testsuite_hooks.h>

constexpr char str[] = "abcdefg";
constexpr std::basic_string_view s(std::begin(str), std::cend(str) - 1);
static_assert( s == str );
static_assert( s.data() == str );

void
test01()
{
  std::vector<char> v{'a', 'b', 'c'};
  std::basic_string_view s(v.begin(), v.end());
  VERIFY( s.data() == v.data() );
}

int
main()
{
  test01();
}
