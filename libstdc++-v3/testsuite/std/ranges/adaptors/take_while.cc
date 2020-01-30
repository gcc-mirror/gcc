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

using __gnu_test::test_range;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;
namespace views = std::ranges::views;

void
test01()
{
  auto p = [] (int i) { return i != 16; };
  auto v = views::iota(10) | views::take_while(p);
  VERIFY( ranges::equal(v, (int[]){10,11,12,13,14,15}) );
  using R = decltype(v);
  static_assert(ranges::view<R>);
  static_assert(!ranges::common_range<R>);
  static_assert(ranges::random_access_range<R>);
}

void
test02()
{
  int x[] = {1,2,3,4,5};
  test_range<int, forward_iterator_wrapper> rx(x);
  auto v = rx | views::take_while([] (int i) { return i<4; });
  VERIFY( ranges::equal(v, (int[]){1,2,3}) );
  using R = decltype(v);
  static_assert(ranges::view<R>);
  static_assert(!ranges::common_range<R>);
  static_assert(ranges::forward_range<R>);
}

int
main()
{
  test01();
  test02();
}
