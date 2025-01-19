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
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::bidirectional_iterator_wrapper;

namespace ranges = std::ranges;

template<template<typename> typename wrapper>
void
test01()
{
    {
      int x[] = {1,2,3,4,5,6,7,8,9,10};
      test_container<int, wrapper> cx(x);
      auto range = ranges::partition(cx, [] (int a) { return a%2==0; });
      VERIFY( range.begin().ptr == x+5 );
      VERIFY( range.end().ptr == x+10 );
      VERIFY( ranges::is_partitioned(cx, [] (int a) { return a%2==0; }) );
    }

    {
      int x[] = {1,2,3,4,5,6,7,8};
      test_range<int, wrapper> rx(x);
      auto range = ranges::partition(rx,
				     [] (int a) { return a%2==0; },
				     [] (int a) { return a+1; });
      VERIFY( range.begin().ptr == x+4 );
      VERIFY( range.end().ptr == x+8 );
      VERIFY( ranges::is_partitioned(rx, [] (int a) { return a%2==1; }) );
    }
}

constexpr bool
test02()
{
  int x[] = {1,2,3,4,5,6,7,8,9,10};
  auto range = ranges::partition(x, x+9, [] (int a) { return a < 100; });
  return (range.begin() == x+9 && range.end() == x+9);
}

int
main()
{
  test01<forward_iterator_wrapper>();
  test01<bidirectional_iterator_wrapper>();
  static_assert(test02());
}
