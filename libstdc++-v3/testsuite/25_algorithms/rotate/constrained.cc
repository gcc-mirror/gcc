// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;

namespace ranges = std::ranges;

struct X
{
  int i;
  X (int a) : i(a) { }

  friend bool
  operator==(const X& lhs, const X& rhs)
  {
    return lhs.i == rhs.i;
  }
};

static_assert(!std::is_trivially_default_constructible_v<X>);

template<template<typename, template<typename> typename> typename container,
	 template<typename> typename wrapper,
	 typename T = int>
void
test01()
{
  for (int a = 0; a <= 7; a++)
    {
      T x[] = {1, 2, 3, 4, 5, 6, 7};
      container<T, wrapper> rx(x);
      auto i = ranges::begin(rx);
      std::advance(i, a);
      auto res = ranges::rotate(rx, i);
      if (a == 0)
	VERIFY( ranges::begin(res) == ranges::end(rx) );
      else
	VERIFY( ranges::begin(res)
		 == std::next(ranges::begin(rx),
			      ranges::distance(i, ranges::end(rx))) );
      VERIFY( ranges::end(res) == ranges::end(rx) );
      for (int k = 0; k < 7; k++)
	VERIFY( x[k] == (k+a)%7 + 1 );
    }
}

constexpr bool
test02()
{
  int x[] = {1, 2, 3, 4};
  const int y[] = { 2, 3, 1, 4 };
  ranges::rotate(x, x+1, x+3);
  return ranges::equal(x, y);
}

int
main()
{
  test01<test_container, forward_iterator_wrapper>();
  test01<test_range, forward_iterator_wrapper>();

  test01<test_container, bidirectional_iterator_wrapper>();
  test01<test_range, bidirectional_iterator_wrapper>();

  test01<test_container, random_access_iterator_wrapper>();
  test01<test_range, random_access_iterator_wrapper>();

  test01<test_container, random_access_iterator_wrapper, X>();
  test01<test_range, random_access_iterator_wrapper, X>();

  static_assert(test02());
}
