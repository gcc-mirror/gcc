// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include <functional>
#include <numeric>

constexpr bool
test01()
{
  int x[3] = {1,2,3};
  int y[3];
  std::partial_sum(x, x+3, y);
  return y[0] == 1 && y[1] == 3 && y[2] == 6;
}

static_assert(test01());

constexpr bool
test02()
{
  int x[3] = {1,2,3};
  int y[3];
  std::partial_sum(x, x+3, y, std::multiplies{});
  return y[0] == 1 && y[1] == 2 && y[2] == 6;
}

static_assert(test02());
