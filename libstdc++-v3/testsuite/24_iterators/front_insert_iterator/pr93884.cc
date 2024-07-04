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

// { dg-do run { target c++20 } }

#include <iterator>
#include <algorithm>
#include <deque>
#include <ranges>
#include <testsuite_hooks.h>

namespace ranges = std::ranges;
namespace views = std::views;

void
test01()
{
  auto v = std::deque<int>{};
  auto i = views::iota(0, 10);
  auto o = std::front_inserter(v);
  static_assert(std::output_iterator<decltype(o), int>);
  ranges::copy(i, o);
  VERIFY( ranges::equal(v | views::reverse, i) );
}

int
main()
{
  test01();
}
