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
// { dg-do compile { target c++2a } }

#include <algorithm>
#include <array>

constexpr bool
test()
{
  auto ok = true;

  std::array<int, 12> ar0{{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}};
  std::array<int, 12> ar1{{11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0}};

  const auto out01 = std::swap_ranges(ar0.begin(), ar0.begin() + 5,
				      ar1.begin() + 2);

  return ok = ar0[0] == 9 && ar1[2] == 0;
}

static_assert(test());
