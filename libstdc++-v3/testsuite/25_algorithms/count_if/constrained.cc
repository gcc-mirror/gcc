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
using __gnu_test::input_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

struct X { int i; };

void
test01()
{
  X x[] = { {2}, {2}, {6}, {8}, {10}, {11}, {2} };
  auto res = ranges::count_if(x, x+7, [] (int i) { return i % 2 == 0; }, &X::i);
  VERIFY( res == 6 );
  res = ranges::count_if(x, x+7, [] (int i) { return i % 2 == 1; }, &X::i);
  VERIFY( res == 1 );
  res = ranges::count_if(x, x+7, [] (int i) { return i < 0; }, &X::i);
  VERIFY( res == 0 );

  test_container<X, forward_iterator_wrapper> c(x);
  res = ranges::count_if(c, [] (int i) { return i == 2; }, &X::i);
  VERIFY( res == 3 );
  res = ranges::count_if(c, [] (int i) { return i < 0; }, &X::i);
  VERIFY( res == 0 );

  test_range<X, input_iterator_wrapper> r(x);
  res = ranges::count_if(c, [] (int i) { return i == 2; }, &X::i);
  VERIFY( res == 3 );
  res = ranges::count_if(c, [] (int i) { return i < 0; }, &X::i);
  VERIFY( res == 0 );
}

struct Y { int i; int j; };

void
test02()
{
  static constexpr Y y[] = { {1,2}, {2,4}, {3,6}, {1,6} };
  static_assert(ranges::count_if(y, [] (int i) { return i < 5; }, &Y::j) == 2);
  static_assert(ranges::count_if(y, [] (int i) { return i != 4; }, &Y::j) == 3);
}

int
main()
{
  test01();
  test02();
}
