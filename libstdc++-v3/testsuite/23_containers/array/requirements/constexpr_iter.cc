// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }
//
// Copyright (C) 2019 Free Software Foundation, Inc.
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

#include <array>

constexpr int
test()
{
  constexpr std::array<int, 3> a1{{1, 2, 3}};
  static_assert(1 == *a1.begin());
  auto n = a1[0] * a1[1]* a1[2];
  static_assert(1 == *a1.cbegin());

  std::array<int, 3> a2{{0, 0, 0}};
  std::copy(a1.begin(), a1.end(), a2.begin());

  return n;
}

void
run_test()
{
  constexpr int n = test();
}
