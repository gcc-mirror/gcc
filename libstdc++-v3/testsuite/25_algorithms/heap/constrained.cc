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
// { dg-require-cstdint "" }

#include <algorithm>
#include <random>
#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::random_access_iterator_wrapper;

namespace ranges = std::ranges;

template<template<typename, template<typename> typename> typename container>
void
test01()
{
  int x[50];

  auto pred = std::greater{};
  auto proj = [] (int a) { return -a; };
  for (int i = 0; i < 50; i++)
    {
      std::iota(x, x+50, 1);
      container<int, random_access_iterator_wrapper> rx(x);

      std::ranlux48_base g(i);
      ranges::shuffle(rx, g);

      auto iter = ranges::make_heap(rx, pred, proj);
      VERIFY( iter == rx.end() );
      VERIFY( ranges::is_heap(rx, pred, proj) );
      VERIFY( ranges::is_heap_until(rx, pred, proj) == rx.end() );

      iter = ranges::pop_heap(rx, pred, proj);
      VERIFY( iter == rx.end() );
      VERIFY( *ranges::prev(iter) == 50 );
      VERIFY( ranges::is_heap_until(rx, pred, proj) == ranges::prev(iter) );

      iter = ranges::pop_heap(rx.begin(), ranges::prev(iter), pred, proj);
      VERIFY( ranges::next(iter) == rx.end() );
      VERIFY( *ranges::prev(iter) == 49 );
      VERIFY( ranges::is_heap_until(rx, pred, proj) == ranges::prev(iter) );

      *ranges::prev(iter) = i;
      iter = ranges::push_heap(rx.begin(), iter, pred, proj);
      VERIFY( ranges::next(iter) == rx.end() );
      VERIFY( ranges::is_heap_until(rx, pred, proj) == iter );

      *iter = 2*i;
      iter = ranges::push_heap(rx.begin(), rx.end(), pred, proj);
      VERIFY( iter == rx.end() );
      VERIFY( ranges::is_heap_until(rx, pred, proj) == iter );

      *ranges::next(rx.begin()) *= -1;
      VERIFY( !ranges::is_heap(rx, pred, proj) );
      *ranges::next(rx.begin()) *= -1;
      VERIFY( ranges::is_heap(rx, pred, proj) );

      iter = ranges::sort_heap(rx, pred, proj);
      VERIFY( iter == rx.end() );
      VERIFY( ranges::is_sorted(rx, pred, proj) );
    }
}

constexpr bool
test02()
{
  bool ok = true;
  int x[] = {1,2,3,4,5};
  ranges::make_heap(x);
  ranges::pop_heap(x);
  x[4] = 7;
  ranges::push_heap(x);
  ok &= ranges::is_heap(x);
  ok &= ranges::is_heap_until(x) == x+5;
  ranges::sort_heap(x);
  ok &= ranges::equal(x, (int[]){1,2,3,4,7});
  return ok;
}

constexpr bool
test03()
{
  // PR libstdc++/100795 - ranges::heap algos should not use std::heap directly
#if __SIZEOF_INT128__
  auto v = std::views::iota(__int128(0), __int128(20));
#else
  auto v = std::views::iota(0ll, 20ll);
#endif

  int storage[20] = {2,5,4,3,1,6,7,9,10,8,11,14,12,13,15,16,18,0,19,17};
  auto w = v | std::views::transform([&](auto i) -> int& { return storage[i]; });
  using type = decltype(w);
  using cat = std::iterator_traits<std::ranges::iterator_t<type>>::iterator_category;
  static_assert( std::same_as<cat, std::output_iterator_tag> );
  static_assert( std::ranges::random_access_range<type> );

  for (int i = 1; i < 20; i++)
    ranges::push_heap(w.begin(), w.begin() + i);
  ranges::sort_heap(w);
  VERIFY( ranges::equal(w, v) );
  ranges::make_heap(w);
  auto it = ranges::pop_heap(w);
  VERIFY( it[-1] == 19 );

  for (int i = 1; i < 20; i++)
    ranges::push_heap(w.begin(), w.begin() + i, std::ranges::greater{});
  ranges::sort_heap(w, std::ranges::greater{});
  VERIFY( ranges::equal(w, v | std::views::reverse) );
  ranges::make_heap(w, std::ranges::greater{});
  it = ranges::pop_heap(w, std::ranges::greater{});
  VERIFY( it[-1] == 0 );

  for (int i = 1; i < 20; i++)
    ranges::push_heap(w.begin(), w.begin() + i, std::ranges::greater{}, std::negate{});
  ranges::sort_heap(w, std::ranges::greater{}, std::negate{});
  VERIFY( ranges::equal(w, v) );
  ranges::make_heap(w, std::ranges::greater{}, std::negate{});
  it = ranges::pop_heap(w, std::ranges::greater{}, std::negate{});
  VERIFY( it[-1] == 19 );

  return true;
}

int
main()
{
  test01<test_range>();
  test01<test_container>();
  static_assert(test02());
  static_assert(test03());
}
