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

constexpr bool
test()
{
  auto ok = true;

  std::array<int, 12> ar0{{10, 0, 5, 1, 2, 6, 7, 8, 3, 4, 9, 11}};

  auto iter1 = std::partition(ar0.begin(), ar0.end(),
			      [](int i){ return i < 7; });
  ok = ok && *iter1 == 8;

  return ok;
}

static_assert(test());
