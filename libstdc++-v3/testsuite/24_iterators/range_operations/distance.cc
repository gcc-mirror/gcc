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
using __gnu_test::test_sized_range;
using __gnu_test::random_access_iterator_wrapper;
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;

void
test01()
{
  int a[10] = { };
  VERIFY( std::ranges::distance(a) == 10 );

  test_range<int, random_access_iterator_wrapper> c(a);
  VERIFY( std::ranges::distance(c) == 10 );

  auto b = c.begin();
  auto e = c.end();
  auto ei = std::ranges::next(b, e);
  VERIFY( std::ranges::distance(b, e) == 10 );
  VERIFY( std::ranges::distance(ei, b) == -10 );

  const auto cb = b;
  const auto ce = e;
  const auto cei = ei;
  VERIFY( std::ranges::distance(cb, ce) == 10 );
  VERIFY( std::ranges::distance(cei, cb) == -10 );

  test_sized_range<int, random_access_iterator_wrapper> c2(a);
  VERIFY( std::ranges::distance(c2) == 10 );
}

void
test02()
{
  int a[2] = { };
  VERIFY( std::ranges::distance(a) == 2 );

  test_range<int, bidirectional_iterator_wrapper> c(a);
  VERIFY( std::ranges::distance(c) == 2 );

  auto b = c.begin();
  auto e = c.end();
  VERIFY( std::ranges::distance(b, e) == 2 );

  const auto cb = b;
  const auto ce = e;
  VERIFY( std::ranges::distance(cb, ce) == 2 );

  test_sized_range<int, bidirectional_iterator_wrapper> c2(a);
  VERIFY( std::ranges::distance(c2) == 2 );
}

void
test03()
{
  int a[3] = { };
  test_range<int, forward_iterator_wrapper> c(a);
  VERIFY( std::ranges::distance(c) == 3 );

  auto b = c.begin();
  auto e = c.end();
  VERIFY( std::ranges::distance(b, e) == 3 );

  const auto cb = b;
  const auto ce = e;
  VERIFY( std::ranges::distance(cb, ce) == 3 );

  test_sized_range<int, forward_iterator_wrapper> c2(a);
  VERIFY( std::ranges::distance(c2) == 3 );
}

void
test04()
{
  int a[4] = { };
  test_range<int, input_iterator_wrapper> c(a);
  static_assert( std::ranges::range<decltype(c)> );

  VERIFY( std::ranges::distance(c) == 4 );
  // first call to distance has traversed the range:
  VERIFY( std::ranges::distance(c) == 0 );

  c = test_range<int, input_iterator_wrapper>(a);
  auto b = c.begin();
  auto e = c.end();
  VERIFY( std::ranges::distance(b, e) == 4 );

  test_range<int, input_iterator_wrapper> c2(a);
  const auto cb = c2.begin();
  const auto ce = c2.end();
  VERIFY( std::ranges::distance(cb, ce) == 4 );

  test_sized_range<int, input_iterator_wrapper> c3(a);
  VERIFY( std::ranges::distance(c3) == 4 );
  // first call to distance just called size() without affecting the range:
  VERIFY( std::ranges::distance(c3) == 4 );
}

void
test05()
{
  int a[5] = { };
  test_range<int, output_iterator_wrapper> c(a);
  VERIFY( std::ranges::distance(c) == 5 );

  test_range<int, output_iterator_wrapper> c2(a);
  auto b = c2.begin();
  auto e = c2.end();
  VERIFY( std::ranges::distance(b, e) == 5 );

  test_range<int, output_iterator_wrapper> c3(a);
  const auto cb = c3.begin();
  const auto ce = c3.end();
  VERIFY( std::ranges::distance(cb, ce) == 5 );

  test_sized_range<int, output_iterator_wrapper> c4(a);
  VERIFY( std::ranges::distance(c4) == 5 );
  // first call to distance just called size() without affecting the range:
  VERIFY( std::ranges::distance(c4) == 5 );
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
