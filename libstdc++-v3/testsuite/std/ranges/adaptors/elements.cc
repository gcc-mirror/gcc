// Copyright (C) 2020 Free Software Foundation, Inc.
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
// { dg-do run { target c++2a } }

#include <algorithm>
#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <tuple>

namespace ranges = std::ranges;
namespace views = ranges::views;

void
test01()
{
  std::tuple<int, int> x[] = {{1,2},{3,4},{5,6}};
  auto v0 = x | views::elements<0>;
  VERIFY( ranges::equal(v0, (int[]){1,3,5}) );
  VERIFY( ranges::equal(v0, x | views::keys) );
  VERIFY( ranges::size(v0) == 3 );

  using R0 = decltype(v0);
  static_assert(ranges::random_access_range<R0>);
  static_assert(ranges::sized_range<R0>);

  auto v1 = x | views::reverse | views::elements<1> | views::reverse;
  VERIFY( ranges::equal(v1, (int[]){2,4,6}) );
  VERIFY( ranges::equal(v1, x | views::values) );
}

int
main()
{
  test01();
}
