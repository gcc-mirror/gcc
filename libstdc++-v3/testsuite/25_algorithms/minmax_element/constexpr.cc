// { dg-do compile { target c++14 } }

// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

#include <algorithm>
#include <functional>
#include <utility>

constexpr std::initializer_list<int> test{2, 1};
constexpr auto x = std::minmax_element(test.begin(), test.end());
constexpr auto y = std::minmax_element(test.begin(), test.end(),
				       std::greater<int>());
static_assert(x.first == test.begin()+1, "");
static_assert(x.second == test.begin(), "");
static_assert(y.first == test.begin(), "");
static_assert(y.second == test.begin()+1, "");

