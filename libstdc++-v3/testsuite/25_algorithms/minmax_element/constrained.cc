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
#include <functional>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

struct X
{
  int i, j;
};

void
test01()
{
  int x[] = {1,2,3,4};
  do
    {
      test_range<int, forward_iterator_wrapper> cx(x);
      VERIFY( *ranges::minmax_element(cx).min == 1 );
      VERIFY( *ranges::minmax_element(cx).max == 4 );
      VERIFY( *ranges::minmax_element(cx, ranges::greater{}).min == 4 );
      VERIFY( *ranges::minmax_element(cx, ranges::greater{}).max == 1 );
      VERIFY( *ranges::minmax_element(cx, {}, std::negate<>{}).min == 4);
      VERIFY( *ranges::minmax_element(cx, {}, std::negate<>{}).max == 1);
      VERIFY( *ranges::minmax_element(cx, ranges::greater{}, std::negate<>{}).min
	      == 1 );
      VERIFY( *ranges::minmax_element(cx, ranges::greater{}, std::negate<>{}).max
	      == 4 );
    } while (ranges::next_permutation(x).found);

  test_container<int, forward_iterator_wrapper> cx(x);
  VERIFY( ranges::minmax_element(cx.begin(), cx.begin()).min == cx.begin() );
  VERIFY( ranges::minmax_element(cx.begin(), cx.begin()).max == cx.begin() );

  constexpr X y[] = {{1,5},{1,2},{1,3}};
  static_assert(ranges::minmax_element(y, y+3, {}, &X::i).min->j == 5);
  static_assert(ranges::minmax_element(y, y+3, {}, &X::i).max->j == 3);
}

void
test02()
{
  // Verify we perform at most 3*N/2 applications of the comparison predicate.
  static int counter;
  struct counted_less
  { bool operator()(int a, int b) { ++counter; return a < b; } };

  int x[] = {1,2,3,4,5,6,7,8,9,10};
  ranges::minmax_element(x, x+2, counted_less{});
  VERIFY( counter == 1 );

  counter = 0;
  ranges::minmax_element(x, x+3, counted_less{});
  VERIFY( counter == 3 );

  counter = 0;
  ranges::minmax_element(x, counted_less{});
  VERIFY( counter <= 15 );

  ranges::reverse(x);
  counter = 0;
  ranges::minmax_element(x, counted_less{});
  VERIFY( counter <= 15 );
}

int
main()
{
  test01();
  test02();
}
