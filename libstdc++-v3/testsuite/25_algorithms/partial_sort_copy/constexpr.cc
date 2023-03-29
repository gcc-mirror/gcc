// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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
#include <functional>

constexpr bool
test()
{
  auto ok = true;

  const std::array<int, 12> ar0{{0, 7, 8, 9, 1, 2, 5, 6, 10, 3, 4, 11}};
  std::array<int, 12> or0{{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

  std::partial_sort_copy(ar0.begin() + 5, ar0.begin() + 10,
			 or0.begin(), or0.begin() + 5);

  std::partial_sort_copy(ar0.begin() + 5, ar0.begin() + 10,
			 or0.begin(), or0.begin() + 5, std::greater<>());

  return ok;
}

static_assert(test());
