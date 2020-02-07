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
  float x[] = {1, 2, 3, 4, 5, 5, 6, 7};
  test_container<float, forward_iterator_wrapper> cx(x);
  for (int i = 0; i < 7; i++)
    {
      VERIFY( ranges::binary_search(cx, i, {}, [] (int a) { return a-1; }) );
      VERIFY( !ranges::binary_search(cx.begin(), cx.end(), i+0.5) );
    }
  VERIFY( !ranges::binary_search(cx, 0) );

  ranges::reverse(x);
  test_range<float, forward_iterator_wrapper> rx(x);
  VERIFY( ranges::binary_search(rx, 5, ranges::greater{}) );
}

constexpr bool
test02()
{
  int x[] = {1, 2, 3};
  return (ranges::binary_search(x, 3)
	  && !ranges::binary_search(x, x, 3));
}

int
main()
{
  test01();
  static_assert(test02());
}
