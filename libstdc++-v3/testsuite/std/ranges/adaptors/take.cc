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
// { dg-do run { target c++2a } }

#include <algorithm>
#include <forward_list>
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
  auto v = views::iota(0) | views::take(5);
  static_assert(ranges::view<decltype(v)>);
  static_assert(!ranges::sized_range<decltype(v)>);
  static_assert(!ranges::common_range<decltype(v)>);
  static_assert(ranges::random_access_range<decltype(v)>);
  static_assert(!ranges::contiguous_range<decltype(v)>);
  static_assert(ranges::range<const decltype(v)>);
  VERIFY( ranges::equal(v, (int[]){0,1,2,3,4}) );
}

void
test02()
{
  auto v = views::take(views::iota(0, 20), 5);
  static_assert(ranges::view<decltype(v)>);
  static_assert(ranges::sized_range<decltype(v)>);
  static_assert(ranges::common_range<decltype(v)>);
  static_assert(ranges::random_access_range<decltype(v)>);
  static_assert(!ranges::contiguous_range<decltype(v)>);
  static_assert(ranges::range<const decltype(v)>);
  VERIFY( ranges::equal(v, (int[]){0,1,2,3,4}) );
}

void
test03()
{
  int x[] = {0,1,2,3,4,5};
  auto is_odd = [] (int i) { return i%2 == 1; };
  auto v = x | views::filter(is_odd) | views::take(3);
  ranges::begin(v);
  using R = decltype(v);
  static_assert(ranges::view<R>);
  static_assert(!ranges::sized_range<R>);
  static_assert(!ranges::common_range<R>);
  static_assert(ranges::forward_range<R>);
  static_assert(!ranges::random_access_range<R>);
  static_assert(!ranges::range<const R>);
  VERIFY( ranges::equal(v, (int[]){1,3,5}) );
}

void
test04()
{
  int x[] = {1,2,3,4,5};
  test_range<int, bidirectional_iterator_wrapper> rx(x);
  auto v = ranges::take_view{rx, 3};
  using R = decltype(v);
  static_assert(ranges::view<R>);
  static_assert(!ranges::sized_range<R>);
  static_assert(ranges::bidirectional_range<R>);
  VERIFY( ranges::equal(v | views::take(5), (int[]){1,2,3}) );
}

void
test05()
{
  std::forward_list<int> x = {1,2,3,4,5};
  auto v = x | views::transform(std::negate{}) | views::take(4);

  // Verify that _Sentinel<false> is implicitly convertible to _Sentinel<true>.
  static_assert(!ranges::common_range<decltype(v)>);
  static_assert(!std::same_as<decltype(ranges::end(v)),
			      decltype(ranges::cend(v))>);
  auto b = ranges::cend(v);
  b = ranges::end(v);
}

template<auto take = views::take>
void
test06()
{
  // Verify SFINAE behavior.
  extern int x[5];
  int* n = 0;
  static_assert(!requires { take(); });
  static_assert(!requires { take(x, n, n); });
  static_assert(!requires { take(x, n); });
  static_assert(!requires { take(n)(x); });
  static_assert(!requires { x | (take(n) | views::all); });
  static_assert(!requires { (take(n) | views::all)(x); });
  static_assert(!requires { take | views::all; });
  static_assert(!requires { views::all | take; });
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
}
