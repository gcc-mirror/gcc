// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#include <algorithm>
#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_range;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;
namespace views = ranges::views;

void
test01()
{
  int x[] = {0,1,2,3,4,5,0,1,2,3,4,5};
  auto v = views::counted(x, 5);
  VERIFY( ranges::equal(v, (int[]){0,1,2,3,4}) );
  using R = decltype(v);
  static_assert(ranges::view<R>);
  static_assert(ranges::sized_range<R>);
  static_assert(ranges::common_range<R>);
  static_assert(ranges::random_access_range<R>);
}

void
test02()
{
  int x[] = {0,1,2,3,4,5,0,1,2,3,4,5};
  test_range<int, forward_iterator_wrapper> rx(x);
  auto v = views::counted(rx.begin(), 5);
  VERIFY( ranges::equal(v, (int[]){0,1,2,3,4}) );
  using R = decltype(v);
  static_assert(ranges::view<R>);
  static_assert(ranges::sized_range<R>);
  static_assert(!ranges::common_range<R>);
  static_assert(!ranges::bidirectional_range<R>);
}

int
main()
{
  test01();
  test02();
}
