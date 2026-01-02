// Copyright (C) 2020-2026 Free Software Foundation, Inc.
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

using __gnu_test::test_range;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;

namespace ranges = std::ranges;

std::mt19937 rng;

template<template<typename> typename in_wrapper,
	 template<typename> typename out_wrapper>
void
test01()
{
  const int x[] = {1,2,3,4,5,6,7,8,9,10};
  test_range<const int, in_wrapper> rx(x);
  int y[10];
  test_range<int, out_wrapper> ry(y);
  auto out = ranges::sample(rx.begin(), rx.end(), ry.begin(), 20, rng);
  VERIFY( out.ptr == y+10 );
  VERIFY( ranges::equal(x, y) );

  for (int i = 0; i < 100; i++)
    {
      int z[5] = {0};
      test_range<int, out_wrapper> rz(z);
      rx.bounds.first = x;
      auto out = ranges::sample(rx, rz.begin(), 5, rng);
      VERIFY( out.ptr == z+5 );
      ranges::sort(z);
      VERIFY( ranges::adjacent_find(z) == out.ptr );
      VERIFY( ranges::includes(x, z) );
    }
}

void
test02()
{
  // PR libstdc++/100795 - ranges::sample should not use std::sample
#if 0 // FIXME: ranges::sample rejects integer-class difference types.
#if __SIZEOF_INT128__
  auto v = std::views::iota(__int128(0), __int128(20));
#else
  auto v = std::views::iota(0ll, 20ll);
#endif
#else
  auto v = std::views::iota(0, 20);
#endif

  int storage[20] = {2,5,4,3,1,6,7,9,10,8,11,14,12,13,15,16,18,0,19,17};
  auto w = v | std::views::transform([&](auto i) -> int& { return storage[i]; });
  using type = decltype(w);
  static_assert( std::ranges::random_access_range<type> );

  ranges::sample(v, w.begin(), 20, rng);
  ranges::sort(w);
  VERIFY( ranges::equal(w, v) );
}

int
main()
{
  test01<forward_iterator_wrapper, output_iterator_wrapper>();
  test01<input_iterator_wrapper, random_access_iterator_wrapper>();
  test02();
}
