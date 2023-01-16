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
using __gnu_test::bidirectional_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  int x[] = {1, 2, 3, 4, 5};
  int y[] = {1, 2, 3, 4, 5};

  for (int i = 0; i <= 5; i++)
    {
      test_container<int, bidirectional_iterator_wrapper> cx(x, x+i);
      test_container<int, bidirectional_iterator_wrapper> cy(y, y+i);
      for (int j = 0; ; j++)
	{
	  auto found1 = std::next_permutation(cx.begin(), cx.end());
	  auto [last,found2] = ranges::next_permutation(cy.begin(), cy.end());
	  VERIFY( found1 == found2 );
	  VERIFY( ranges::equal(cx, cy) );
	  if (!found2)
	    break;
	}
    }
}

void
test02()
{
  int x[] = {5, 4, 3, 2, 1};
  test_range<int, bidirectional_iterator_wrapper> rx(x);
  auto [last,found] = ranges::next_permutation(rx, ranges::greater{});
  VERIFY( found && last == rx.end() );
  VERIFY( last == rx.end() );
  VERIFY( ranges::equal(rx, (int[]){5,4,3,1,2}) );
  ranges::next_permutation(rx, {}, [] (int a) { return -a; });
  VERIFY( ranges::equal(rx, (int[]){5,4,2,3,1}) );

  VERIFY( !ranges::next_permutation(x, x).found );
  VERIFY( !ranges::next_permutation(x, x+1).found );
}

constexpr bool
test03()
{
  int x[] = {1,2,3};
  ranges::next_permutation(x);
  return ranges::equal(x, (int[]){1,3,2});
}

int
main()
{
  test01();
  test02();
  static_assert(test03());
}
