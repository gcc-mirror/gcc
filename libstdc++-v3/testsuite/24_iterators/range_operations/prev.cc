// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#include <iterator>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_range;
using __gnu_test::random_access_iterator_wrapper;
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;

void
test01()
{
  int a[10] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  test_range<int, random_access_iterator_wrapper> r(a);
  auto begin = r.begin();
  auto end = r.end();
  auto endi = std::ranges::next(begin, end);
  VERIFY( *std::ranges::prev(endi) == 9 );
  VERIFY(  std::ranges::prev(begin, 0) == begin );
  VERIFY( *std::ranges::prev(endi, 1) == 9 );
  VERIFY( *std::ranges::prev(endi, 3) == 7 );
  VERIFY( *std::ranges::prev(begin, -4) == 4 );
  VERIFY(  std::ranges::prev(begin, 0, begin) == begin );
  VERIFY(  std::ranges::prev(begin, 5, begin) == begin );
  VERIFY(  std::ranges::prev(begin, -5, begin) == begin );
  VERIFY(  std::ranges::prev(begin, 0, endi) == begin );
  VERIFY( *std::ranges::prev(endi, 5, begin) == 5 );
  VERIFY(  std::ranges::prev(endi, 55, begin) == begin );
  VERIFY(  std::ranges::prev(endi, 0, endi) == end );
  VERIFY(  std::ranges::prev(endi, -5, endi) == end );
  VERIFY(  std::ranges::prev(endi, -55, endi) == end );
  VERIFY(  std::ranges::prev(endi, 0, begin) == end );
  VERIFY( *std::ranges::prev(begin, -5, endi) == 5 );
  VERIFY(  std::ranges::prev(begin, -55, endi) == end );
}

void
test02()
{
  int a[10] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  test_range<int, bidirectional_iterator_wrapper> r(a);
  auto begin = r.begin();
  auto end = r.end();
  auto endi = std::ranges::next(begin, end);
  VERIFY( *std::ranges::prev(endi) == 9 );
  VERIFY(  std::ranges::prev(begin, 0) == begin );
  VERIFY( *std::ranges::prev(endi, 1) == 9 );
  VERIFY( *std::ranges::prev(endi, 3) == 7 );
  VERIFY( *std::ranges::prev(begin, -4) == 4 );
  VERIFY(  std::ranges::prev(begin, 0, begin) == begin );
  VERIFY(  std::ranges::prev(begin, 5, begin) == begin );
  VERIFY(  std::ranges::prev(begin, -5, begin) == begin );
  VERIFY(  std::ranges::prev(begin, 0, endi) == begin );
  VERIFY( *std::ranges::prev(endi, 5, begin) == 5 );
  VERIFY(  std::ranges::prev(endi, 55, begin) == begin );
  VERIFY(  std::ranges::prev(endi, 0, endi) == end );
  VERIFY(  std::ranges::prev(endi, -5, endi) == end );
  VERIFY(  std::ranges::prev(endi, -55, endi) == end );
  VERIFY(  std::ranges::prev(endi, 0, begin) == end );
  VERIFY( *std::ranges::prev(begin, -5, endi) == 5 );
  VERIFY(  std::ranges::prev(begin, -55, endi) == end );
}

template<typename T>
  concept can_prev = requires(T& t) { std::ranges::prev(t); }
    || requires(T& t) { std::ranges::prev(t, 1); }
    || requires(T& t) { std::ranges::prev(t, 1, t); };

static_assert( !can_prev<forward_iterator_wrapper<int>> );
static_assert( !can_prev<input_iterator_wrapper<int>> );
static_assert( !can_prev<output_iterator_wrapper<int>> );

int
main()
{
  test01();
  test02();
}
