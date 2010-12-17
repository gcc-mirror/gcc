// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_common_types.h>

int main()
{
  __gnu_test::constexpr_default_constructible test1;
  test1.operator()<std::tuple<int, int>>();

  __gnu_test::constexpr_single_value_constructible test2;
  test2.operator()<std::tuple<int, int>, std::tuple<int, int>>();
  //  test2.operator()<std::tuple<int, int>, std::pair<short, short>>();
  //  test2.operator()<std::tuple<int>, std::tuple<short>>();
  //  test2.operator()<std::tuple<int, int>, std::tuple<short, short>>();

  // test 3
  const int i1(129);
  const int i2(6);
  constexpr std::tuple<int, int> p3(i1, i2);

  // test 4
  const int i3(415);
  const int i4(550);
  const int i5(6414);
  constexpr std::tuple<int, int, int, int, int> p4(i1, i2, i3, i4, i5);

  return 0;
}
