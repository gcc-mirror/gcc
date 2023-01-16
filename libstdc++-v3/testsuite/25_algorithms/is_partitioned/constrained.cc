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

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  int x[] = {2,4,6,1,3,5};
  test_container<int, forward_iterator_wrapper> cx(x);
  VERIFY( ranges::is_partitioned(cx, [] (int a) { return a%2==0; }) );

  test_range<int, input_iterator_wrapper> rx(x);
  VERIFY( ranges::is_partitioned(rx,
				 [] (int a) { return a%2==1; },
				 [] (int a) { return a+1; }) );
}

constexpr bool
test02()
{
  int x[] = {1,2,3,4,5,6,1};
  return (ranges::is_partitioned(x, x+6, [] (int a) { return a<=2; })
	  && !ranges::is_partitioned(x, x+7, [] (int a) { return a<=2; }));
}

int
main()
{
  test01();
  static_assert(test02());
}
