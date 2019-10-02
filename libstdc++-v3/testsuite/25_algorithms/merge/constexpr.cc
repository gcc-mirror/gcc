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
  constexpr std::array<int, 12> ca0{{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}};
  constexpr std::array<int, 12> cas{{3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14}};
  constexpr std::array<int, 3> camm{{-6, -5, -4}};
  std::array<int, 24> out0{{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}};

  const auto outdd = std::merge(ca0.begin(), ca0.end(),
				cas.begin(), cas.end(), out0.begin());

  const auto outee = std::merge(ca0.begin(), ca0.end(),
				camm.begin(), camm.end(), out0.begin(),
				[](int i, int j){ return i < j; });

  return true;
}

static_assert(test());
