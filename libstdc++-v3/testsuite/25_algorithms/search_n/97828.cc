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

// PR libstdc++/97828

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_sized_range;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;

template<template<typename> typename Wrapper>
void
test01()
{
  int x[] = {0,42,42,0,42,42,42};
  test_sized_range<int, Wrapper> rx(x);
  auto res = std::ranges::search_n(rx, 3, 42);
  VERIFY( res.begin().ptr == x+4 && res.end().ptr == x+7 );
}

template<template<typename> typename Wrapper>
void
test02()
{
  int x[] = {0,42,42,0,42};
  test_sized_range<int, Wrapper> rx(x);
  auto res = std::ranges::search_n(rx, 3, 42);
  VERIFY( res.begin().ptr == x+5 && res.end().ptr == x+5 );
}

int
main()
{
  test01<forward_iterator_wrapper>();
  test01<random_access_iterator_wrapper>();
  test02<forward_iterator_wrapper>();
  test02<random_access_iterator_wrapper>();
}
