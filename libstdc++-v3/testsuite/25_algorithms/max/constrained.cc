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

namespace ranges = std::ranges;

struct X
{
  int i, j;
};

void
test01()
{
  VERIFY( ranges::max(1, 2) == 2);
  VERIFY( ranges::max(2, 1) == 2);
  VERIFY( ranges::max(1, 2, ranges::greater{}) == 1);
  VERIFY( ranges::max(1, 2, ranges::greater{}, std::negate<>{}) == 2);
  VERIFY( ranges::max(1, 2, {}, std::negate<>{}) == 1);
  VERIFY( ranges::max(X{1,2}, X{1,3}, {}, &X::i).j == 2 );
}

void
test02()
{
  int x[] = {1,2,3,4};
  do
    {
      test_range<int, input_iterator_wrapper> cx(x);
      VERIFY( ranges::max(cx) == 4 );
      cx.bounds.first = x;
      VERIFY( ranges::max(cx, ranges::greater{}) == 1 );
      cx.bounds.first = x;
      VERIFY( ranges::max(cx, {}, std::negate<>{}) == 1);
      cx.bounds.first = x;
      VERIFY( ranges::max(cx, ranges::greater{}, std::negate<>{}) == 4 );
    } while (ranges::next_permutation(x).found);

  constexpr X y[] = {{0,5},{1,2},{1,3}};
  static_assert(ranges::max(y, {}, &X::i).j == 2);
}

void
test03()
{
  VERIFY( ranges::max({2,3,1,4}) == 4 );
  VERIFY( ranges::max({2,3,1,4}, ranges::greater{}) == 1 );
  VERIFY( ranges::max({2,3,1,4}, {}, std::negate<>{}) == 1 );
  VERIFY( ranges::max({2,3,1,4}, ranges::greater{}, std::negate<>{}) == 4 );
}

int
main()
{
  test01();
  test02();
  test03();
}
