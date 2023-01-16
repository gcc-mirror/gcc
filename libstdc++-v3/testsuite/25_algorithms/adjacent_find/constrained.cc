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
  int y[] = { 2, 7, 8, 8, 9 };

  VERIFY( ranges::adjacent_find(x, x+6, {}, &X::i) == x+0 );
  VERIFY( ranges::adjacent_find(x+1, x+6, {}, &X::i) == x+6 );
  VERIFY( ranges::adjacent_find(y) == y+2 );
  VERIFY( ranges::adjacent_find(y, y+4) == y+2 );

  test_container<X, forward_iterator_wrapper> c(x);
  VERIFY( ranges::adjacent_find(c, {}, &X::i) == ranges::begin(c) );

  test_range<int, forward_iterator_wrapper> r(y);
  auto res = ranges::adjacent_find(r);
  VERIFY( *res == 8 && *++res == 8 );
}

void
test02()
{
  static constexpr X x[] = { {2}, {2}, {6}, {8}, {10}, {11} };
  static constexpr X y[] = { {2}, {6}, {8}, {10}, {11} };
  static_assert(ranges::adjacent_find(x, {}, &X::i) == x+0);
  static_assert(ranges::adjacent_find(y, {}, &X::i) == y+5);
}

int
main()
{
  test01();
  test02();
}

