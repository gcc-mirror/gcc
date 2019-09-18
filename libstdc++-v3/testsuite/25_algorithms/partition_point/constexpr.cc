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

constexpr std::array<int, 12> caeo{{0, 2, 4, 6, 8, 10, 1, 3, 5, 7, 9, 11}};

constexpr bool
test()
{
  const auto outjj = std::partition_point(caeo.begin(), caeo.end(),
					  [](int i){ return i % 2 == 0; });

  return outjj == caeo.begin() + 6;
}

static_assert(test());
