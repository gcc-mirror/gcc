// { dg-do compile { target c++17 } }
// { dg-add-options no_pch }

// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

#ifndef __cpp_lib_array_constexpr
# error "Feature test macro for array constexpr is missing in <array>"
#elif __cpp_lib_array_constexpr < 201803L
# error "Feature test macro for array constexpr has wrong value in <array>"
#endif

// This test is compiled as C++17 because array::iterator is just a pointer,
// so always meets the C++20 constexpr iterator requirements, even in C++17.

constexpr int
test()
{
  constexpr std::array<int, 3> a1{{1, 2, 3}};
  static_assert(1 == *a1.begin());
  auto n = a1[0] * a1[1]* a1[2];
  static_assert(1 == *a1.cbegin());

  std::array<int, 3> a2{{0, 0, 0}};
  auto a1i = a1.begin();
  auto a1e = a1.end();
  auto a2i = a2.begin();
  while (a1i != a1e)
    *a2i++ = *a1i++;

  return n;
}

void
run_test()
{
  constexpr int n = test();
}
