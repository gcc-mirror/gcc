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
using __gnu_test::bidirectional_iterator_wrapper;

namespace ranges = std::ranges;
namespace views = ranges::views;

void
test01()
{
  int x[] = {1,2,3,4,5};
  auto v = x | views::reverse;
  VERIFY( ranges::equal(v, (int[]){5,4,3,2,1}) );
  VERIFY( ranges::equal(v | views::reverse, x) );
  static_assert(ranges::view<decltype(v)>);
  static_assert(ranges::sized_range<decltype(v)>);
  static_assert(ranges::common_range<decltype(v)>);
  static_assert(ranges::random_access_range<decltype(v)>);
}

void
test02()
{
  int x[] = {1,2,3,4,5};
  test_range<int, bidirectional_iterator_wrapper> rx(x);
  auto v = views::reverse(rx);
  VERIFY( ranges::equal(v, (int[]){5,4,3,2,1}) );
  VERIFY( ranges::equal(v | views::reverse, rx) );
  static_assert(ranges::view<decltype(v)>);
  static_assert(!ranges::sized_range<decltype(v)>);
  static_assert(ranges::common_range<decltype(v)>);
  static_assert(!ranges::random_access_range<decltype(v)>);
  static_assert(ranges::bidirectional_range<decltype(v)>);
}

void
test03()
{
  int x[] = {1,7,3,6,5,2,4,8};
  auto is_even = [] (int i) { return i%2==0; };
  int sum = 0;
  for (auto i : x | views::reverse | views::filter(is_even))
    sum += i;
  VERIFY( sum == 20 );
}

void
test04()
{
  int x[] = {1,2,3,4,5};
  VERIFY( ranges::equal(x | views::reverse | (views::reverse | views::reverse),
			(int[]){5,4,3,2,1}) );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
