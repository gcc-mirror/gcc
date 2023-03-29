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
using __gnu_test::input_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::bidirectional_iterator_wrapper;

namespace ranges = std::ranges;

struct X { int i; };

void
test01()
{
  int x[] = { {2}, {2}, {6}, {8}, {10}, {11} };
  auto res = ranges::search_n(x+0, x+6, 2, 2);
  VERIFY( res.begin() == x+0 && res.end() == x+2 );

  int z[] = { {1}, {2}, {2}, {4}, {5}, {6} };
  res = ranges::search_n(z, 3, 3, std::greater<int>());
  VERIFY( res.begin() == z+3 && res.end() == z+6 );

  test_container<int, forward_iterator_wrapper> cx(x);
  auto res2 = ranges::search_n(cx, 2, 2);
  VERIFY( res2.begin() == cx.begin() && *res2.end() == 6 );

  int y[] = { {2}, {2}, {8}, {2}, {2}, {2}, {5} };
  test_range<int, forward_iterator_wrapper> ry(y);
  auto res3 = ranges::search_n(ry, 3, 2);
  VERIFY( *res3.begin() == 2 && *res3.end() == 5 );

  auto res4 = ranges::search_n(ry, 1, 8);
  VERIFY( res4.begin().ptr == y+2 && res4.end().ptr == y+3 );
}

void
test02()
{
  static constexpr X x[] = { {2}, {2}, {6}, {8}, {10}, {2} };
  static constexpr X y[] = { {2}, {6}, {8}, {8}, {8}, {2} };
  static constexpr int z[] = { {2}, {6}, {8}, {10}, {2}, {2} };

  static_assert(ranges::search_n(z, 0, 5).end() == z+0);
  static_assert(ranges::search_n(z, 1, 5).begin() == z+6);
  static_assert(ranges::search_n(x, 2, 3, {}, &X::i).begin() == x+6);
  static_assert(ranges::search_n(x, 2, 2, {}, &X::i).end() == x+2);
  static_assert(ranges::search_n(y, 3, 8, {}, &X::i).begin() == y+2);
  static_assert(ranges::search_n(y, 3, 8, {}, &X::i).end() == y+5);
}

int
main()
{
  test01();
  test02();
}

