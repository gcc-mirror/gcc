// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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
  int x[] = {1, 2, 3, 4, 5, 5, 6, 7};
  for (unsigned i = 0; i < 5; i++)
    for (unsigned j = 6; j < 8; j++)
      {
	test_container<int, forward_iterator_wrapper> cx(x);
	auto range = ranges::equal_range(std::next(cx.begin(), i),
					 std::next(cx.begin(), j),
					 4, {}, [] (int a) { return a-1; });
	VERIFY( range.begin().ptr == x+4 && range.end().ptr == x+6 );
      }

  ranges::reverse(x);
  test_range<int, forward_iterator_wrapper> rx(x);
  auto range = ranges::equal_range(rx, 5, ranges::greater{},
				   [] (int a) { return a+1; });
  VERIFY( range.begin().ptr == x+4 && range.end().ptr == x+5 );
}

constexpr bool
test02()
{
  int x[] = {1, 2, 3, 4, 5};
  auto range1 = ranges::equal_range(x, 6);
  auto range2 = ranges::equal_range(x, x, 6);
  auto range3 = ranges::equal_range(x, 1);
  return (range1.begin() == x+5 && range1.end() == x+5
	  && range2.begin() == x && range2.end() == x
	  && range3.begin() == x && range3.end() == x+1);
}

int
main()
{
  test01();
  static_assert(test02());
}
