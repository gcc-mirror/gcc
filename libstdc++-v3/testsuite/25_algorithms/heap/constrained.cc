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
      VERIFY( *(iter-1) == 50 );
      VERIFY( ranges::is_heap_until(rx, pred, proj) == iter-1 );

      iter = ranges::pop_heap(rx.begin(), iter-1, pred, proj);
      VERIFY( iter+1 == rx.end() );
      VERIFY( *(iter-1) == 49 );
      VERIFY( ranges::is_heap_until(rx, pred, proj) == iter-1 );

      *(iter-1) = i;
      iter = ranges::push_heap(rx.begin(), iter, pred, proj);
      VERIFY( iter+1 == rx.end() );
      VERIFY( ranges::is_heap_until(rx, pred, proj) == iter );

      *iter = 2*i;
      iter = ranges::push_heap(rx.begin(), rx.end(), pred, proj);
      VERIFY( iter == rx.end() );
      VERIFY( ranges::is_heap_until(rx, pred, proj) == iter );

      *(rx.begin()+1) *= -1;
      VERIFY( !ranges::is_heap(rx, pred, proj) );
      *(rx.begin()+1) *= -1;
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

int
main()
{
  test01<test_range>();
  test01<test_container>();
  static_assert(test02());
}
