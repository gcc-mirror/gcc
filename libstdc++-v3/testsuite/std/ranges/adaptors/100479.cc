// Copyright (C) 2021-2023 Free Software Foundation, Inc.
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

// PR libstdc++/100479

#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_forward_range;
using __gnu_test::test_random_access_range;

namespace ranges = std::ranges;
namespace views = std::views;

void
test01()
{
  // Verify we don't propagate cached iterators when copying/moving an adapted
  // forward range that memoizes its begin().
  static int pred_counter;
  int x[] = {1,2,3,4,5};
  test_forward_range rx(x);
  auto is_odd = [](auto n) { ++pred_counter; return n%2 != 0; };

  auto v = rx | views::filter(is_odd);
  v.begin(); v.begin();
  VERIFY( pred_counter == 1 ); // filter_view caches its begin() iterator

  auto w = v;
  v.begin(); v.begin();
  VERIFY( pred_counter == 1 ); // cached iterator not invalidated on copy
  w.begin(); w.begin();
  VERIFY( pred_counter == 2 ); // cached iterator not propagated on copy

  auto z = std::move(w);
  w.begin(); w.begin();
  VERIFY( pred_counter == 3 ); // cached iterator invalidated on move
  z.begin(); z.begin();
  VERIFY( pred_counter == 4 ); // cached iterator not propagated on move
}

void
test02()
{
  // Verify we invalidate the cached offset when moving an adapted
  // random access range that memoizes its begin().
  static int pred_counter;
  int x[] = {1,2,3,4,5};
  test_random_access_range rx(x);
  auto is_odd = [](auto n) { ++pred_counter; return n%2 != 0; };

  auto v = rx | views::filter(is_odd);
  v.begin(); v.begin();
  VERIFY( pred_counter == 1 ); // filter_view caches its begin() iterator

  auto w = v;
  v.begin(); v.begin();
  VERIFY( pred_counter == 1 ); // cached offset not invalidated on copy
  w.begin(); w.begin();
  VERIFY( pred_counter == 1 ); // cached offset propagated on copy

  auto z = std::move(w);
  w.begin(); w.begin();
  VERIFY( pred_counter == 2 ); // cached offset invalidated on move
  z.begin(); z.begin();
  VERIFY( pred_counter == 2 ); // cached offset propagated on move
}

constexpr bool
test03()
{
  // Propagating cached iterators during copy/move would cause these asserts
  // to fail here.
  auto v = views::single(1)
    | views::lazy_split(1)
    | views::drop(0)
    | views::drop_while([](auto) { return false; })
    | views::filter([](auto) { return true; });
  static_assert(ranges::forward_range<decltype(v)>);
  VERIFY( ranges::distance(v) == 2 );
  auto w = v;
  VERIFY( ranges::distance(v) == 2 );
  auto z = std::move(w);
  VERIFY( ranges::distance(v) == 2 );
  return true;
}

int
main()
{
  test01();
  test02();
  test03();
  static_assert(test03());
}
