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
using __gnu_test::output_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

struct X
{
  int i;
};

void
test01()
{
  int x[5] = { 1, 2, 3, 4, 5 };
  const int y[4] = { 1, 2, 4, 5 };
  auto res = ranges::remove(x, 3);
  VERIFY( res.begin() == x+4 && res.end() == x+5 );
  VERIFY( ranges::equal(x, x+4, y, y+4) );
}

void
test02()
{
  int x[1];
  test_container<int, forward_iterator_wrapper> c(x, x);
  auto res = ranges::remove(c, 1);
  VERIFY( res.begin().ptr == x && res.end().ptr == x );
}

void
test03()
{
  int x[1] = {1};
  test_container<int, forward_iterator_wrapper> c(x);
  auto res = ranges::remove(c, 0);
  VERIFY( res.begin().ptr == x+1 && res.end().ptr == x+1 );
  res = ranges::remove(c, 1);
  VERIFY( res.begin().ptr == x && res.end().ptr == x+1 );
}

void
test04()
{
  X x[8] = { {0}, {1}, {0}, {1}, {0}, {0}, {1}, {1} };
  const int y[4] = { 0, 0, 0, 0 };
  test_container<X, forward_iterator_wrapper> c(x);
  auto res = ranges::remove(c, 1, &X::i);
  VERIFY( res.begin().ptr == x+4 && res.end().ptr == x+8 );
  VERIFY( ranges::equal(x, x+4, y, y+4, {}, &X::i) );
}

constexpr bool
test05()
{
  int x[6] = { 3, 2, 3, 3, 5, 3 };
  const int y[2] = { 2, 5 };
  auto res = ranges::remove(x, 3);
  return ranges::equal(x, res.begin(), y, y+2);
}


int
main()
{
  test01();
  test02();
  test03();
  test04();
  static_assert(test05());
}
