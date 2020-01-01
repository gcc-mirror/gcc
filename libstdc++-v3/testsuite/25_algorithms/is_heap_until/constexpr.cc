// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

// heap
constexpr std::array<int, 23>
ahu{{22,
     21, 20,
     17, 16, 19, 18,//v
     11, 10,  9,  8, 23, 14, 13, 12, 3, 2, 1, 0, 7, 6, 5, 4}};

constexpr auto outq = std::is_heap_until(ahu.begin(), ahu.end());

constexpr auto outr = std::is_heap_until(ahu.begin(), ahu.end(),
					 std::less<int>());

constexpr bool
test()
{
  return outq == ahu.begin() + 11 && outr == ahu.begin() + 11;
}

static_assert(test());
