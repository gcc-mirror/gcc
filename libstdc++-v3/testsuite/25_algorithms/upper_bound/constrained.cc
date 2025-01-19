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

namespace ranges = std::ranges;

void
test01()
{
  int x[] = {1, 2, 3, 4, 5, 5, 6, 7};
  for (unsigned i = 0; i < 5; i++)
    for (unsigned j = 6; j < 8; j++)
      {
	test_container<int, forward_iterator_wrapper> cx(x);
	auto result = ranges::upper_bound(std::next(cx.begin(), i),
					  std::next(cx.begin(), j),
					  4, {}, [] (int a) { return a-1; });
	VERIFY( result.ptr == x+6 );
      }

  ranges::reverse(x);
  test_range<int, forward_iterator_wrapper> rx(x);
  auto result = ranges::upper_bound(rx, 5, ranges::greater{},
				    [] (int a) { return a+1; });
  VERIFY( result.ptr == x+5 );
}

constexpr bool
test02()
{
  int x[] = {1, 2, 3, 4, 5};
  return (ranges::upper_bound(x, 6) == x+5
	  && ranges::upper_bound(x, x, 6) == x
	  && ranges::upper_bound(x, 1) == x+1);
}

int
main()
{
  test01();
  static_assert(test02());
}
