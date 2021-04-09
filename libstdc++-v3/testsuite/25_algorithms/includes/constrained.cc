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
using __gnu_test::input_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  int x[] = {1,2,3,4,5,6,7};
  int y[] = {2,4,6};
  test_range<int, input_iterator_wrapper> rx(x), ry(y);

  VERIFY( ranges::includes(rx, ry) );

  rx.bounds.first = x;
  ry.bounds.first = y;
  VERIFY( ranges::includes(rx, ry,
			   ranges::greater{},
			   std::negate<>{},
			   std::negate<>{}) );

  test_container<int, forward_iterator_wrapper> cx(x), cy(y);
  VERIFY( ranges::includes(cx.begin(), cx.end(),
			   cy.begin(), cy.end(),
			   {},
			   [] (int a) { return a+1; },
			   [] (int a) { return a+2; }) );

  VERIFY( ranges::includes(x, x+1, y, y) );
}

constexpr bool
test03()
{
  bool ok = true;
  ok &= ranges::includes((int[]){1,2,3},
			 (int[]){1});
  ok &= !ranges::includes((int[]){1,2,3},
			  (int[]){1,2,3,4});
  return true;
}

int
main()
{
  test01();
  static_assert(test03());
}
