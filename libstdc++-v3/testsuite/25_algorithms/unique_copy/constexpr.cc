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
  std::array<int, 12> ar3{{0, 1, 2, 3, 4, 5, 6, 6, 8, 9, 9, 11}};
  std::array<int, 24> out0{{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}};

  const auto out55 = std::unique_copy(ar3.begin(), ar3.end(), out0.begin());

  const auto out66 = std::unique_copy(ar3.begin(), ar3.end(), out55,
				      std::equal_to<int>());

  return out55 == (out0.begin() + 10) && out0[7] == 8
    && out66 == (out55 + 10) ; // && out0[19] == 11;
}

static_assert(test());
