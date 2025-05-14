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
#include <forward_list>
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

void
test03()
{
  std::forward_list<int> x = {1,2,3,4,5};
  auto v
    = x | views::transform(std::negate{}) | views::take_while(std::identity{});

  // Verify that _Sentinel<false> is implicitly convertible to _Sentinel<true>.
  static_assert(!ranges::common_range<decltype(v)>);
  static_assert(!std::same_as<decltype(ranges::end(v)),
			      decltype(ranges::cend(v))>);
  auto b = ranges::cend(v);
  b = ranges::end(v);
}

void
test04()
{
  // LWG 3450
  auto v = views::single(1) | views::take_while([](int& x) { return true;});
  static_assert(ranges::range<decltype(v)>);
  static_assert(!ranges::range<decltype(v) const>);
}

template<auto take_while = views::take_while>
void
test05()
{
  // Verify SFINAE behavior.
  extern int x[5];
  auto p = [] (int*) { return true; };
  static_assert(!requires { take_while(); });
  static_assert(!requires { take_while(x, p, p); });
  static_assert(!requires { take_while(x, p); });
  static_assert(!requires { take_while(p)(x); });
  static_assert(!requires { x | (take_while(p) | views::all); });
  static_assert(!requires { (take_while(p) | views::all)(x); });
  static_assert(!requires { take_while | views::all; });
  static_assert(!requires { views::all | take_while; });
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
}
