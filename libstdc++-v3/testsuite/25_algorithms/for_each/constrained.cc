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
using __gnu_test::random_access_iterator_wrapper;

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

template<template<typename> typename wrapper>
void
test03()
{
  int x[] = {1,2,3,4,5};
  test_range<int, wrapper> rx(x);
  int s = 0;
  auto func = [&s](int i){ s += i; };
  auto [i,f] = ranges::for_each_n(rx.begin(), 3, func);
  VERIFY( i.ptr = x+3 );
  VERIFY( s == 1+2+3 );
  f(1);
  VERIFY( s == 1+2+3+1 );

  s = 0;
  rx.bounds.first = x;
  auto [j,g] = ranges::for_each_n(rx.begin(), -1, func);
  VERIFY( j.ptr == x );
  VERIFY( s == 0 );
  g(1);
  VERIFY( s == 1 );

  s = 0;
  rx.bounds.first = x;
  auto [k,h] = ranges::for_each_n(rx.begin(), 5, func, std::negate<>{});
  VERIFY( k.ptr == x+5 );
  VERIFY( s == -(1+2+3+4+5) );
  h(-6);
  VERIFY( s == -(1+2+3+4+5+6) );
}

constexpr bool
test04()
{
  int x[] = {1,2,3,4,5};
  int p = 1;
  ranges::for_each_n(x+1, 4, [&p](int i){ p*=i; }, [](int i){ return i+1; });
  return p == 3*4*5*6;
}

int
main()
{
  test01();
  test02();
  test03<input_iterator_wrapper>();
  test03<random_access_iterator_wrapper>();
  static_assert(test04());
}
