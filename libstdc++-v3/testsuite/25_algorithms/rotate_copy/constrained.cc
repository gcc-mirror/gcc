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
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;

namespace ranges = std::ranges;

struct X
{
  int i;
  X () : i(0) { }
  X (int a) : i(a) { }

  friend bool
  operator==(const X& lhs, const X& rhs)
  {
    return lhs.i == rhs.i;
  }
};

static_assert(!std::is_trivial_v<X>);

template<template<typename, template<typename> typename> typename container,
	 template<typename> typename wrapper,
	 typename T = int>
void
test01()
{
  for (int a = 0; a <= 7; a++)
    {
      T x[] = {1, 2, 3, 4, 5, 6, 7};
      T w[7];
      container<T, wrapper> rx(x), rw(w);
      auto i = ranges::begin(rx);
      std::advance(i, a);
      auto [in,out] = ranges::rotate_copy(rx, i, ranges::begin(rw));
      VERIFY( in == ranges::end(rx) );
      VERIFY( out == ranges::end(rw) );
      for (int k = 0; k < 7; k++)
	VERIFY( w[k] == (k+a)%7 + 1 );
    }
}

constexpr bool
test02()
{
  const int x[] = {1, 2, 3, 4};
  int w[3];
  const int y[] = { 2, 3, 1};
  auto [in,out] = ranges::rotate_copy(x, x+1, x+3, w);
  return (in == x+3
	  && out == w+3
	  && ranges::equal(w, y));
}

int
main()
{
  test01<test_container, forward_iterator_wrapper>();
  test01<test_range, forward_iterator_wrapper>();

  test01<test_container, random_access_iterator_wrapper>();
  test01<test_range, random_access_iterator_wrapper>();

  test01<test_container, random_access_iterator_wrapper, X>();
  test01<test_range, random_access_iterator_wrapper, X>();

  static_assert(test02());
}
