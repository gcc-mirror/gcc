// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

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

#include <array>

#ifndef __cpp_lib_to_array
# error "Feature test macro for to_array is missing"
#elif __cpp_lib_to_array < 201907L
# error "Feature test macro for to_array has wrong value"
#endif

void test01()
{
  const char x[6]{};
  std::array<char, 6> y = std::to_array(x);

  constexpr char x2[] = "foo";
  constexpr std::array<char, 4> y2 = std::to_array(x2);
  static_assert( std::equal(y2.begin(), y2.end(), x2) );
}

void
test02()
{
  struct MoveOnly
  {
    constexpr MoveOnly(int i = 0) : i(i) { }
    constexpr MoveOnly(MoveOnly&& m) : i(m.i + 100) { }
    int i;
  };

  struct X {
    MoveOnly m[3];
  };
  X x;
  std::array<MoveOnly, 3> y = std::to_array(std::move(x).m);

  constexpr std::array<MoveOnly, 3> y2 = std::to_array(X{{1, 2, 3}}.m);
  static_assert( y2[0].i == 101 && y2[1].i == 102 && y2[2].i == 103 );
}
