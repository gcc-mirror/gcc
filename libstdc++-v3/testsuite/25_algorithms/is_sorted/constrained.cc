// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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
  int x[] = {3,4,5,1};
  test_container<int, forward_iterator_wrapper> cx(x);
  VERIFY( ranges::is_sorted(cx.begin(), ranges::next(cx.begin(), 3)) );
  VERIFY( !ranges::is_sorted(cx) );
  VERIFY( !ranges::is_sorted(cx, ranges::greater{}) );
  VERIFY( ranges::is_sorted(cx, {}, [] (int a) { return 0; }) );
}

void
test02()
{
  int x[] = {1,2,3,4,5};
  test_range<int, forward_iterator_wrapper> rx(x);
  VERIFY( ranges::is_sorted(rx) );
  VERIFY( !ranges::is_sorted(ranges::begin(rx),
			     ranges::next(ranges::begin(rx), 2),
			     ranges::greater{}) );
}

constexpr bool
test03()
{
  int x[] = { 1,2 };
  return (ranges::is_sorted(x)
	  && ranges::is_sorted(x, x) );
}

int
main()
{
  test01();
  test02();
  static_assert(test03());
}
