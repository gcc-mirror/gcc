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
  constexpr std::array<int, 12> ca0{{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}};

  const auto out4 = std::binary_search(ca0.begin(), ca0.end(), 5);

  const auto out5 = std::binary_search(ca0.begin(), ca0.end(), 5,
				       std::less<int>());

  const auto out6 = std::binary_search(ca0.begin(), ca0.end(), 4.5,
				       std::less<>());

  return out4 && out5 && !out6;
}

static_assert(test());
