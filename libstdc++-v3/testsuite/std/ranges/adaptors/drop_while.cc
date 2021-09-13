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
#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_range;
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;

namespace ranges = std::ranges;
namespace views = std::ranges::views;

void
test01()
{
  auto p = [] (int i) { return i != 16; };
  auto v = views::iota(10) | views::drop_while(p);
  VERIFY( ranges::equal(v | views::take(5), (int[]){16,17,18,19,20}) );
  using R = decltype(v);
  static_assert(ranges::view<R>);
  static_assert(!ranges::common_range<R>);
  static_assert(ranges::random_access_range<R>);
}

void
test02()
{
  int x[] = {1,2,3,4,5};
  test_range<int, bidirectional_iterator_wrapper> rx(x);
  auto v = rx | views::drop_while([] (int i) { return i<4; });
  VERIFY( ranges::equal(v, (int[]){4,5}) );
  using R = decltype(v);
  static_assert(ranges::view<R>);
  static_assert(!ranges::common_range<R>);
  static_assert(ranges::bidirectional_range<R>);
}

// The following tests that drop_while_view::begin caches its result.

template<template<typename> typename wrapper>
struct test_view : ranges::view_base
{
  bool begin_already_called = false;
  static inline int x[] = {1,2,3,4,5};
  test_range<int, wrapper> rx{x};

  auto
  begin()
  {
    if (begin_already_called)
      x[0] = 10;
    begin_already_called = true;
    return rx.begin();
  }

  auto
  end()
  { return rx.end(); }
};

template<template<typename> typename wrapper>
void
test03()
{
  auto v
    = test_view<wrapper>{} | views::drop_while([] (int i) { return i<3; });
  VERIFY( ranges::equal(v, (int[]){3,4,5}) );
  VERIFY( ranges::equal(v, (int[]){3,4,5}) );
}

template<auto drop_while = views::drop_while>
void
test04()
{
  // Verify SFINAE behavior.
  extern int x[5];
  auto p = [] (int*) { return true; };
  static_assert(!requires { drop_while(); });
  static_assert(!requires { drop_while(x, p, p); });
  static_assert(!requires { drop_while(x, p); });
  static_assert(!requires { drop_while(p)(x); });
  static_assert(!requires { x | (drop_while(p) | views::all); });
  static_assert(!requires { (drop_while(p) | views::all)(x); });
  static_assert(!requires { drop_while | views::all; });
  static_assert(!requires { views::all | drop_while; });
}

int
main()
{
  test01();
  test02();
  test03<forward_iterator_wrapper>();
  test03<random_access_iterator_wrapper>();
  test04();
}
