// { dg-do compile { target c++11 } }

// Copyright (C) 2010-2020 Free Software Foundation, Inc.
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

#include <iostream>

// 2 element tuple
int main()
{
  typedef std::tuple<int, int> tuple_type;

  // 01: default ctor
  __gnu_test::constexpr_default_constructible test1;
  test1.operator()<tuple_type>();

  // 02: default copy ctor
  __gnu_test::constexpr_single_value_constructible test2;
  test2.operator()<tuple_type, tuple_type>();

  // 03: element move ctor, single element
  const int i1(415);
  constexpr tuple_type t2 { 44, std::move(i1) };

  // 04: element move ctor, two element
  const int i2(510);
  const int i3(408);
  constexpr tuple_type t4 { std::move(i2), std::move(i3) };

  // 05: value-type conversion constructor
  const int i4(650);
  const int i5(310);
  constexpr tuple_type t8(i4, i5);

  // 06: pair conversion ctor
  test2.operator()<tuple_type, std::pair<int, int>>();
  test2.operator()<std::tuple<short, short>, std::pair<int, int>>();
  test2.operator()<tuple_type, std::pair<short, short>>();

  // 07: different-tuple-type conversion constructor
  test2.operator()<tuple_type, std::tuple<short, short>>();
  test2.operator()<std::tuple<short, short>, tuple_type>();

  return 0;
}
