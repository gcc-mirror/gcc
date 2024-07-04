// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <algorithm>
#include <array>
#include <functional>

constexpr bool
test()
{
  constexpr std::array<int, 12> aus{{0, 1, 2, 3, 4, 5, 66, 7, 8, 9, 10, 11}};

  const auto outx = std::is_sorted_until(aus.begin(), aus.end());

  const auto outy = std::is_sorted_until(aus.begin(), aus.end(),
					 std::less<int>());

  const auto outz = std::is_sorted_until(outx - 1, aus.end(),
					 std::greater<int>());

  return outx == aus.begin() + 7 && outy == outx && outz == (outx + 1);
}

static_assert(test());
