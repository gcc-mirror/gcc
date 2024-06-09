// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

#include <functional>
#include <numeric>

constexpr bool
test01()
{
  int x[5] = {1,2,3,4,5};
  int y[5] = {2,4,6,8,10};
  auto ret = std::inner_product(x, x+5, y, 0);
  return ret == 110;
}

static_assert(test01());

constexpr bool
test02()
{
  int x[5] = {1,2,3,4,5};
  int y[5] = {2,4,6,8,10};
  auto ret = std::inner_product(x, x+5, y, 1,
				std::multiplies{}, std::plus{});
  return ret == 3*6*9*12*15;
}

static_assert(test02());
