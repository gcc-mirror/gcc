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
// { dg-require-cstdint "" }

#include <algorithm>
#include <random>
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

int
main()
{
  test01<forward_iterator_wrapper, output_iterator_wrapper>();
  test01<input_iterator_wrapper, random_access_iterator_wrapper>();
}
