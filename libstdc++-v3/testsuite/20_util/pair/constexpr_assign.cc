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

#include <utility>
#include <tuple>

constexpr int
test01(int i, int j)
{
  using T = std::tuple<int>;
  std::pair<int, int> p0, p1, p2, p3;
  std::pair<int, int> pij(std::piecewise_construct, T(i), T(j));
  p0 = pij;
  p1 = std::move(pij);
  std::pair<long, long> pijl(i, j);
  p2 = pijl;
  p3 = std::move(pijl);
  return p0.first + p0.second + p1.first + p1.second
    + p2.first + p2.second + p3.first + p3.second;
}

static_assert( test01(3, 100) == 412 );
