// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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
  VERIFY( *std::ranges::next(begin) == 1 );
  VERIFY(  std::ranges::next(begin, 0) == begin );
  VERIFY( *std::ranges::next(begin, 1) == 1 );
  VERIFY( *std::ranges::next(begin, 3) == 3 );
  VERIFY( *std::ranges::next(endi, -4) == 6 );
  VERIFY(  std::ranges::next(begin, begin) == begin );
  VERIFY(  std::ranges::next(begin, end) == end );
  VERIFY(  std::ranges::next(endi, end) == end );
  VERIFY(  std::ranges::next(endi, begin) == begin );
  VERIFY(  std::ranges::next(begin, 0, begin) == begin );
  VERIFY(  std::ranges::next(begin, 5, begin) == begin );
  VERIFY(  std::ranges::next(begin, -5, begin) == begin );
  VERIFY(  std::ranges::next(begin, 0, end) == begin );
  VERIFY( *std::ranges::next(begin, 5, end) == 5 );
  VERIFY(  std::ranges::next(begin, 55, end) == end );
  VERIFY(  std::ranges::next(endi, 0, end) == end );
  VERIFY(  std::ranges::next(endi, -5, end) == end );
  VERIFY(  std::ranges::next(endi, -55, end) == end );
  VERIFY(  std::ranges::next(endi, 0, begin) == end );
  VERIFY( *std::ranges::next(endi, -5, begin) == 5 );
  VERIFY(  std::ranges::next(endi, -55, begin) == begin );
}

void
test02()
{
  int a[10] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  test_range<int, bidirectional_iterator_wrapper> r(a);
  auto begin = r.begin();
  auto end = r.end();
  auto endi = std::ranges::next(begin, end);
  VERIFY( *std::ranges::next(begin) == 1 );
  VERIFY(  std::ranges::next(begin, 0) == begin );
  VERIFY( *std::ranges::next(begin, 1) == 1 );
  VERIFY( *std::ranges::next(begin, 3) == 3 );
  VERIFY( *std::ranges::next(endi, -4) == 6 );
  VERIFY(  std::ranges::next(begin, begin) == begin );
  VERIFY(  std::ranges::next(begin, end) == end );
  VERIFY(  std::ranges::next(endi, end) == end );
  VERIFY(  std::ranges::next(endi, begin) == begin );
  VERIFY(  std::ranges::next(begin, 0, begin) == begin );
  VERIFY(  std::ranges::next(begin, 5, begin) == begin );
  VERIFY(  std::ranges::next(begin, -5, begin) == begin );
  VERIFY(  std::ranges::next(begin, 0, end) == begin );
  VERIFY( *std::ranges::next(begin, 5, end) == 5 );
  VERIFY(  std::ranges::next(begin, 55, end) == end );
  VERIFY(  std::ranges::next(endi, 0, end) == end );
  VERIFY(  std::ranges::next(endi, -5, end) == end );
  VERIFY(  std::ranges::next(endi, -55, end) == end );
  VERIFY(  std::ranges::next(endi, 0, begin) == end );
  VERIFY( *std::ranges::next(endi, -5, begin) == 5 );
  VERIFY(  std::ranges::next(endi, -55, begin) == begin );
}

void
test03()
{
  int a[10] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  test_range<int, forward_iterator_wrapper> r(a);
  auto begin = r.begin();
  auto end = r.end();
  auto endi = std::ranges::next(begin, end);
  VERIFY( *std::ranges::next(begin) == 1 );
  VERIFY(  std::ranges::next(begin, 0) == begin );
  VERIFY( *std::ranges::next(begin, 1) == 1 );
  VERIFY( *std::ranges::next(begin, 3) == 3 );
  VERIFY(  std::ranges::next(begin, begin) == begin );
  VERIFY(  std::ranges::next(begin, end) == end );
  VERIFY(  std::ranges::next(endi, end) == end );
  VERIFY(  std::ranges::next(begin, 0, begin) == begin );
  VERIFY(  std::ranges::next(begin, 5, begin) == begin );
  VERIFY(  std::ranges::next(begin, -5, begin) == begin );
  VERIFY(  std::ranges::next(begin, 0, end) == begin );
  VERIFY( *std::ranges::next(begin, 5, end) == 5 );
  VERIFY(  std::ranges::next(begin, 55, end) == end );
  VERIFY(  std::ranges::next(endi, 0, end) == end );
  VERIFY(  std::ranges::next(endi, 5, end) == end );
  VERIFY(  std::ranges::next(endi, 55, end) == end );
  VERIFY(  std::ranges::next(endi, 0, begin) == end );
}

void
test04()
{
  int a[10] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  test_range<int, input_iterator_wrapper> r(a);
  auto begin = r.begin();
  auto end = r.end();
  auto iter = std::ranges::next(begin);
  VERIFY( *iter == 1 );
  iter = std::ranges::next(iter, 0);
  VERIFY( *iter == 1 );
  iter = std::ranges::next(iter, 1);
  VERIFY( *iter == 2 );
  iter = std::ranges::next(iter, 4);
  VERIFY( *iter == 6 );

  iter = std::ranges::next(iter, iter);
  VERIFY( *iter == 6 );
  iter = std::ranges::next(iter, end);
  VERIFY( iter == end );
  iter = std::ranges::next(iter, end);
  VERIFY( iter == end );

  test_range<int, input_iterator_wrapper> r2(a);
  begin = r2.begin();
  end = r2.end();
  auto endi = std::ranges::next(begin, end);
  // reset single-pass input range
  r2.bounds.first = a;
  iter = std::ranges::next(begin, 0, begin);
  VERIFY( *iter == 0 );
  iter = std::ranges::next(begin, 5, begin);
  VERIFY( *iter == 0 );
  iter = std::ranges::next(begin, -5, begin);
  VERIFY( *iter == 0 );
  iter = std::ranges::next(begin, 0, end);
  VERIFY( *iter == 0 );
  iter = std::ranges::next(endi, 0, begin);
  VERIFY( iter == end );
  iter = std::ranges::next(begin, 5, end); // invalidates begin
  VERIFY( *iter == 5 );
  iter = std::ranges::next(iter, 55, end);
  VERIFY( iter == end );
  iter = std::ranges::next(endi, 0, end);
  VERIFY( iter == end );
  iter = std::ranges::next(endi, 5, end);
  VERIFY( iter == end );
}

void
test05()
{
  int a[10] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  test_range<int, output_iterator_wrapper> r(a);
  auto iter = r.begin();
  auto end = r.end(); // sentinel, !same_as<decltype(iter), decltype(end)>

  iter = std::ranges::next(iter);
  *iter = 10;
  VERIFY( a[1] == 10 );
  iter = std::ranges::next(iter, 0);
  iter = std::ranges::next(iter, 1);
  *iter = 20;
  VERIFY( a[2] == 20 );
  iter = std::ranges::next(iter, 4);
  iter = std::ranges::next(iter, 0);
  *iter = 60;
  VERIFY( a[6] == 60 );

  iter = std::ranges::next(iter, end);
  VERIFY( iter == end );
  iter = std::ranges::next(iter, end);
  VERIFY( iter == end );

  test_range<int, output_iterator_wrapper> r2(a);
  iter = std::ranges::next(r2.begin(), 5);
  end = r2.end();

  iter = std::ranges::next(iter, 0, end);
  *iter = 50;
  VERIFY( a[5] == 50 );
  iter = std::ranges::next(iter, 2, end);
  *iter = 70;
  VERIFY( a[7] == 70 );
  iter = std::ranges::next(iter, 5, end);
  VERIFY( iter == end );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
}
