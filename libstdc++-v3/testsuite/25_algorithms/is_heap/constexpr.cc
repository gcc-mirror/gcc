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
#include <functional>
#include <array>

// heap
constexpr std::array<int, 23>
ah{{22,
    21, 20,
    17, 16, 19, 18,
    11, 10, 9, 8, 15, 14, 13, 12, 3, 2, 1, 0, 7, 6, 5, 4}};

constexpr auto outo = std::is_heap(ah.begin(), ah.end());

constexpr auto outp = std::is_heap(ah.begin(), ah.end(), std::less<int>());

constexpr bool
test()
{
  return outo && outp;
}

static_assert(test());
