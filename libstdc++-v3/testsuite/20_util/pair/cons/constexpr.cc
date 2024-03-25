// { dg-do compile { target c++11 } }

// Copyright (C) 2010-2024 Free Software Foundation, Inc.
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
  typedef std::pair<int, int> pair_type;

  __gnu_test::constexpr_default_constructible test1;
  test1.operator()<pair_type>();

  __gnu_test::constexpr_single_value_constructible test2;
  test2.operator()<pair_type, pair_type>();
  test2.operator()<pair_type, std::pair<short, short>>();

  // test 3
  const int i1(129);
  const int i2(6);
  constexpr pair_type p0(i1, i2);

  // test 4
  constexpr int i(999);
  constexpr pair_type p1 { 44, 90 };
  constexpr pair_type p2 { std::move(p1.first),  i };
  constexpr pair_type p3 { i, std::move(p1.second) };

  constexpr pair_type p5 { 444, 904 };
  constexpr pair_type p6 { std::move(p5.first), std::move(p5.second) };

  constexpr std::pair<char, char> p8 { 'a', 'z' };
  constexpr pair_type p9(std::move(p8));

  constexpr pair_type p10(std::move(p0));

  return 0;
}
