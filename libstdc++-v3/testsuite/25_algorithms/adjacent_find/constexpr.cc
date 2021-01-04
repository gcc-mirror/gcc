// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

constexpr std::array<int, 12> car{{0, 1, 2, 3, 4, 5, 6, 6, 8, 9, 9, 11}};

constexpr auto out0x = std::adjacent_find(car.begin(), car.end());

constexpr auto out1x = std::adjacent_find(car.begin(), car.end(),
					  std::equal_to<int>());

constexpr bool
test()
{
  return out0x == car.begin() + 6 && out1x == car.begin() + 6;
}

static_assert(test());
