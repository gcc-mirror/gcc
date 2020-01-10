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
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  for (int k = 1; k <= 7; k++)
    {
      int x[] = {1,2,3,4,5,6,7};
      test_container<int, forward_iterator_wrapper> cx(x);
      auto pred = [&] (int a) { return a <= k; };
      auto middle = ranges::partition_point(cx, pred);
      VERIFY( middle.ptr == x+k );
    }

  for (int k = 1; k <= 8; k++)
    {
      int x[] = {1,2,3,4,5,6,7,8};
      test_range<int, forward_iterator_wrapper> rx(x);
      auto pred = [&] (int a) { return a > -k; };
      auto proj = [] (int a) { return -a; };
      auto middle = ranges::partition_point(rx, pred, proj);
      VERIFY( middle.ptr == x+k-1 );
    }
}

constexpr bool
test02()
{
  int x[] = {1,2,3,4,5};
  return (ranges::partition_point(x, x+5, [] (int a) { return a < 6; }) == x+5
	  && ranges::partition_point(x, x+5, [] (int a) { return a < 0; }) == x);
}

int
main()
{
  test01();
  static_assert(test02());
}
