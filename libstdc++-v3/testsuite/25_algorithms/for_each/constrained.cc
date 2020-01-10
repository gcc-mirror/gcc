// Copyright (C) 2020 Free Software Foundation, Inc.
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

static int a;

void
f(int& i)
{
  a += i;
}

void
test01()
{
  X x[] = { {2}, {4}, {6}, {8}, {10}, {11} };

  auto res = ranges::for_each(x, x+6, f, &X::i);
  VERIFY( res.in == x+6 );
  VERIFY( res.fun == &f );
  VERIFY( a == 41 );

  test_container<X, forward_iterator_wrapper> c(x);
  int p = 0;
  ranges::for_each(c, [&p](int i) { ++p; }, &X::i);
  VERIFY( p == 6 );

  test_range<X, input_iterator_wrapper> r(x);
  int q = 0;
  ranges::for_each(r, [&q](X&) { ++q; });
  VERIFY( q == 6 );
}

struct Y { int i; int j; };

void
test02()
{
  auto f = []
  {
    Y y[] = { {1,2}, {2,4}, {3,6} };
    int a = 0;
    ranges::for_each(y, [&a](int i) { a += i; }, &Y::i);
    return a;
  };
  static_assert(f() == 6);
}

int
main()
{
  test01();
  test02();
}
