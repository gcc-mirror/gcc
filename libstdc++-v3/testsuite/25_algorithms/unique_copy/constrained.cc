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
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

template<template<typename> typename source_wrapper,
	 template<typename> typename dest_wrapper>
void
test01()
{
  int x[6] = {0, 0, 0, 1, 1, 1};
  int y[2];
  const int z[2] = {0, 1};

  test_range<int, source_wrapper> rx(x);
  test_range<int, dest_wrapper> ry(y);
  auto [in,out] = ranges::unique_copy(rx, ry.begin());
  VERIFY( in == ranges::end(rx) && out == ranges::end(ry) );
  VERIFY( ranges::equal(y, z) );
}

template<template<typename> typename source_wrapper,
	 template<typename> typename dest_wrapper>
void
test02()
{
  int x[6] = {0, 0, 0, 1, 1, 1};
  int y[2] = {0, 0};
  const int z[2] = {0, 0};

  test_range<int, source_wrapper> rx(x, x);
  test_range<int, dest_wrapper> ry(y, y);
  auto [in, out] = ranges::unique_copy(rx.begin(), rx.end(), ry.begin());
  VERIFY( in.ptr == x && out.ptr == y );
  VERIFY( ranges::equal(y, z) );
}

template<template<typename> typename source_wrapper,
	 template<typename> typename dest_wrapper>
void
test03()
{
  struct X { int i; };
  X x[6] = { {1}, {2}, {2}, {4}, {4}, {6} };
  X y[4] = { {1}, {2}, {4}, {6} };
  const X z[4] = { {1}, {2}, {4}, {6} };

  test_range<X, source_wrapper> rx(x);
  test_range<X, dest_wrapper> ry(y);
  auto [in, out]
    = ranges::unique_copy(rx, ry.begin(), ranges::equal_to{}, &X::i);
  VERIFY( in == ranges::end(rx) && out == ranges::end(ry) );
  VERIFY( ranges::equal(y, z, {}, &X::i, &X::i) );
}

constexpr bool
test04()
{
  struct X { int i; };
  X x[7] = { {1}, {2}, {2}, {2}, {4}, {4}, {6} };
  X y[4] = { {1}, {2}, {4}, {6} };
  const X z[4] = { {1}, {2}, {4}, {6} };

  auto [in, out]
    = ranges::unique_copy(x, x+7, y, ranges::equal_to{}, &X::i);
  return (in == ranges::end(x)
	  && out == ranges::end(y)
	  && ranges::equal(y, z, {}, &X::i, &X::i));
}

int
main()
{
  test01<input_iterator_wrapper, output_iterator_wrapper>();
  test01<input_iterator_wrapper, forward_iterator_wrapper>();
  test01<forward_iterator_wrapper, output_iterator_wrapper>();

  test02<input_iterator_wrapper, output_iterator_wrapper>();
  test02<input_iterator_wrapper, forward_iterator_wrapper>();
  test02<forward_iterator_wrapper, output_iterator_wrapper>();

  test03<input_iterator_wrapper, output_iterator_wrapper>();
  test03<input_iterator_wrapper, forward_iterator_wrapper>();
  test03<forward_iterator_wrapper, output_iterator_wrapper>();

  static_assert(test04());
}
