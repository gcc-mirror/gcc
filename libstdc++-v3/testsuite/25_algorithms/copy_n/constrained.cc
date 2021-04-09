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
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;

namespace ranges = std::ranges;

template<template<typename> typename in_wrapper,
	 template<typename> typename out_wrapper>
void
test01()
{
  int x[] = {1,2,3,4,5,6,7};
  for (int i = -1; i <= 7; i++)
    {
      test_range<int, in_wrapper> rx(x);
      int w[7];
      test_range<int, out_wrapper> rw(w);
      ranges::copy_n(rx.begin(), i, rw.begin());
      if (i >= 0)
	VERIFY( ranges::equal(x, x+i, w, w+i) );
    }
}

constexpr bool
test02()
{
  int x[] = {1,2,3};
  int y[2];
  auto [in,out] = ranges::copy_n(x, 2, y);
  return (in == x+2
	  && out == y+2
	  && ranges::equal(x, x+2, y, y+2));
}

int
main()
{
  test01<input_iterator_wrapper,
	 output_iterator_wrapper>();
  test01<random_access_iterator_wrapper,
	 output_iterator_wrapper>();
  test01<random_access_iterator_wrapper,
	 random_access_iterator_wrapper>();
  static_assert(test02());
}
