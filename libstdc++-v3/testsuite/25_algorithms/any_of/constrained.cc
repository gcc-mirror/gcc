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

struct X { int i; };

struct XLess
{
  int val;
  bool operator()(X& x) const { return x.i < val; }
};

struct ILess
{
  int val;
  bool operator()(int& i) const { return i < val; }
};

template<typename T>
struct NotZero
{
  bool operator()(T& t) const { return t != 0; }
};

void
test01()
{
  X x[] = { {2}, {4}, {6}, {8}, {10}, {11} };

  VERIFY( ranges::any_of(x, x+6, XLess{3}) );
  VERIFY( ranges::any_of(x, x+6, ILess{3}, &X::i) );
  VERIFY( !ranges::any_of(x+1, x+6, XLess{3}) );
  VERIFY( !ranges::any_of(x+1, x+6, ILess{3}, &X::i) );
  VERIFY( ranges::any_of(x, XLess{5}) );
  VERIFY( ranges::any_of(x, ILess{5}, &X::i) );

  test_container<X, forward_iterator_wrapper> c(x);
  VERIFY( ranges::any_of(c, NotZero<int>{}, &X::i) );

  test_range<X, input_iterator_wrapper> r(x);
  VERIFY( ranges::any_of(r, NotZero<int>{}, &X::i) );
  VERIFY( ranges::any_of(r, NotZero<X* const>{}, [](X& x) { return &x; }) );
}

struct Y { int i; int j; };

void
test02()
{
  static constexpr Y y[] = { {1,2}, {2,4}, {3,6} };
  static_assert(ranges::any_of(y, [](int i) { return i%2 == 0; }, &Y::i));
  static_assert(ranges::any_of(y, [](const Y& y) { return y.i + y.j == 3; }));
  static_assert(!ranges::any_of(y, [](const Y& y) { return y.i == y.j; }));
}

int
main()
{
  test01();
  test02();
}
