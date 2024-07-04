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

using __gnu_test::test_range;
using __gnu_test::test_container;
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;

namespace ranges = std::ranges;

template<template<typename> typename wrapper>
void
test01()
{
  int x[] = { 1, 2, 3, 4 };
  test_container<int, wrapper> cx(x);
  const int y[] = { 4, 3, 2, 1 };

  auto res = ranges::reverse(cx);
  VERIFY( res == ranges::end(cx) );
  VERIFY( ranges::equal(cx, y) );
}

template<template<typename> typename wrapper>
void
test02()
{
  int x[] = { 1, 2, 3, 4, 5 };
  test_range<int, wrapper> rx(x);
  const int y[] = { 5, 4, 3, 2, 1 };

  auto res = ranges::reverse(rx);
  VERIFY( res == ranges::end(rx) );
  VERIFY( ranges::equal(rx, y) );
}

constexpr bool
test03()
{
  int x[] = { 1, 2, 3 };
  const int y[] = { 2, 1, 3 };
  ranges::reverse(x, x+2);
  return ranges::equal(x, y);
}

int
main()
{
  test01<bidirectional_iterator_wrapper>();
  test02<bidirectional_iterator_wrapper>();

  test01<random_access_iterator_wrapper>();
  test02<random_access_iterator_wrapper>();

  static_assert(test03());
}
