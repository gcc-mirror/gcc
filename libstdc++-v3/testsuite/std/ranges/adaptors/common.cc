// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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
namespace views = ranges::views;

void
test01()
{
  int x[] = {1,2,1,3};
  auto v = x | views::common;
  VERIFY( std::count(v.begin(), v.end(), 1) == 2);
  static_assert(ranges::common_range<decltype(v)>);
  static_assert(ranges::view<decltype(v)>);
  static_assert(ranges::random_access_range<decltype(v)>);
  static_assert(std::same_as<decltype(v), decltype(views::common(v))>);

  auto v2 = v | (views::common | views::common);
  VERIFY( std::count(v2.begin(), v2.end(), 1) == 2);
}

void
test02()
{
  int x[] = {1,2,1,3};
  test_range<int, forward_iterator_wrapper> rx(x);
  auto v = ranges::common_view(rx);
  VERIFY( std::count(v.begin(), v.end(), 1) == 2);
  static_assert(ranges::common_range<decltype(v)>);
  static_assert(ranges::view<decltype(v)>);
  static_assert(ranges::forward_range<decltype(v)>);
  static_assert(std::same_as<decltype(v), decltype(views::common(v))>);

  auto v2 = v | (views::common | views::common);
  VERIFY( std::count(v2.begin(), v2.end(), 1) == 2);
}

template<auto common = views::common>
void
test03()
{
  // Verify SFINAE behavior.
  static_assert(!requires { common(); });
  static_assert(!requires { common(0, 0); });
  static_assert(!requires { common(0); });
  static_assert(!requires { 0 | common; });
}

int
main()
{
  test01();
  test02();
  test03();
}
