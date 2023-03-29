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

struct X { int i; };

void
test01()
{
  X x[] = { {2}, {2}, {6}, {8}, {10}, {11} };
  auto res = ranges::find(x, x+6, 8, &X::i);
  VERIFY( res == x+3 );
  res = ranges::find(x, x+6, 2, &X::i);
  VERIFY( res == x+0 );
  res = ranges::find(x, x+6, 9, &X::i);
  VERIFY( res == x+6 );

  test_container<X, forward_iterator_wrapper> c(x);
  auto res2 = ranges::find(c, 8, &X::i);
  VERIFY( res2 != ranges::end(c) && res2->i == 8 );
  res2 = ranges::find(c, 9, &X::i);
  VERIFY( res2 == ranges::end(c) );

  test_range<X, input_iterator_wrapper> r(x);
  auto res3 = ranges::find(r, 8, &X::i);
  VERIFY( res3 != ranges::end(r) && res3->i == 8 );

  r.bounds.first = x;
  res3 = ranges::find(r, 9, &X::i);
  VERIFY( res3 == ranges::end(r) );
}

struct Y { int i; int j; };

void
test02()
{
  static constexpr Y y[] = { {1,2}, {2,4}, {3,6} };
  static_assert(ranges::find(y, 4, &Y::j) == y+1);
  static_assert(ranges::find(y, 5, &Y::j) == y+3);
}

int
main()
{
  test01();
  test02();
}
