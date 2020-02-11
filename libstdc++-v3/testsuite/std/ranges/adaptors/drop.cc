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
  auto v = x | views::drop(3);
  using R = decltype(v);
  static_assert(ranges::view<R>);
  static_assert(ranges::sized_range<R>);
  static_assert(ranges::random_access_range<R>);
  VERIFY( ranges::equal(v, (int[]){4,5}) );
}

void
test02()
{
  int x[] = {1,2,3,4,5};
  auto t = views::drop(3) | views::reverse;
  auto v = x | t;
  using R = decltype(v);
  static_assert(ranges::view<R>);
  static_assert(ranges::sized_range<R>);
  static_assert(ranges::random_access_range<R>);
  VERIFY( ranges::equal(v, (int[]){5,4}) );
}

void
test03()
{
  int x[] = {1,2,3,4,5};
  test_range<int, bidirectional_iterator_wrapper> rx(x);
  auto v = rx | views::drop(3);
  using R = decltype(v);
  static_assert(ranges::view<R>);
  static_assert(!ranges::sized_range<R>);
  static_assert(ranges::bidirectional_range<R>);
  VERIFY( ranges::equal(v, (int[]){4,5}) );
}


void
test04()
{
  auto v = views::iota(0) | views::drop(10);
  using R = decltype(v);
  static_assert(ranges::view<R>);
  static_assert(!ranges::sized_range<R>);
  VERIFY( ranges::equal(v | views::take(3), (int[]){10,11,12}) );
}

void
test05()
{
  int x[] = {1,2,3};
  auto r = ranges::subrange(x, x+1);
  auto v = views::drop(r, 2);
  VERIFY( ranges::begin(v) == x+1 );
  VERIFY( ranges::size(v) == 0 );
}

void
test06()
{
  int x[] = {1,2,3};
  VERIFY( ranges::empty(x | views::drop(10)) );
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
