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
  int a[2] = { };
  test_range<int, random_access_iterator_wrapper> r(a);
  auto iter = r.begin();
  std::ranges::advance(iter, 1);
  VERIFY( iter != r.begin() );
  VERIFY( iter != r.end() );
  std::ranges::advance(iter, 1);
  VERIFY( iter == r.end() );
  std::ranges::advance(iter, -2);
  VERIFY( iter == r.begin() );

  std::ranges::advance(iter, r.begin() + 1);
  VERIFY( iter != r.begin() );
  VERIFY( iter != r.end() );
  std::ranges::advance(iter, r.begin());
  VERIFY( iter == r.begin() );

  auto diff = std::ranges::advance(iter, 99, r.end());
  VERIFY( iter == r.end() );
  VERIFY( diff == 97 );
  diff = std::ranges::advance(iter, -222, r.begin());
  VERIFY( iter == r.begin() );
  VERIFY( diff == -220 );
}

void
test02()
{
  int a[2] = { };
  test_range<int, bidirectional_iterator_wrapper> r(a);
  auto iter = r.begin();
  std::ranges::advance(iter, 1);
  VERIFY( iter != r.begin() );
  VERIFY( iter != r.end() );
  std::ranges::advance(iter, 1);
  VERIFY( iter == r.end() );
  std::ranges::advance(iter, -2);
  VERIFY( iter == r.begin() );

  auto iter1 = r.begin();
  ++iter1;
  std::ranges::advance(iter, iter1);
  VERIFY( iter != r.begin() );
  VERIFY( iter != r.end() );
  std::ranges::advance(iter, r.begin());
  VERIFY( iter == r.begin() );

  auto diff = std::ranges::advance(iter, 99, r.end());
  VERIFY( iter == r.end() );
  VERIFY( diff == 97 );
  diff = std::ranges::advance(iter, -222, r.begin());
  VERIFY( iter == r.begin() );
  VERIFY( diff == -220 );
}

void
test03()
{
  int a[2] = { };
  test_range<int, forward_iterator_wrapper> r(a);
  auto iter = r.begin();
  std::ranges::advance(iter, 1);
  VERIFY( iter != r.begin() );
  VERIFY( iter != r.end() );
  std::ranges::advance(iter, 1);
  std::ranges::advance(iter, 0);
  VERIFY( iter == r.end() );
  std::ranges::advance(iter, 0);
  VERIFY( iter == r.end() );

  auto iter1 = r.begin();
  ++iter1;
  std::ranges::advance(iter, iter1);
  VERIFY( iter != r.begin() );
  VERIFY( iter != r.end() );
  std::ranges::advance(iter, r.end());
  VERIFY( iter == r.end() );
  std::ranges::advance(iter, r.end());
  VERIFY( iter == r.end() );

  auto diff = std::ranges::advance(iter, 99, r.end());
  VERIFY( iter == r.end() );
  VERIFY( diff == 99 ); // PR libstdc++/100833
  diff = std::ranges::advance(iter, 99, r.end());
  VERIFY( iter == r.end() );
  VERIFY( diff == 99 );
  iter = r.begin();
  diff = std::ranges::advance(iter, 99, r.end());
  VERIFY( iter == r.end() );
  VERIFY( diff == 97 );
  diff = std::ranges::advance(iter, 99, r.end());
  VERIFY( iter == r.end() );
  VERIFY( diff == 99 );
}

void
test04()
{
  int a[2] = { };
  test_range<int, input_iterator_wrapper> r(a);
  auto iter = r.begin();
  std::ranges::advance(iter, 1);
  VERIFY( iter != r.end() );
  std::ranges::advance(iter, 1);
  std::ranges::advance(iter, 0);
  VERIFY( iter == r.end() );
  std::ranges::advance(iter, 0);
  VERIFY( iter == r.end() );

  test_range<int, input_iterator_wrapper> r2(a);
  iter = r2.begin();
  ++iter;
  const auto iter1 = iter;
  std::ranges::advance(iter, iter1);
  VERIFY( iter == iter1 );
  VERIFY( iter != r2.end() );
  std::ranges::advance(iter, r2.end());
  VERIFY( iter == r2.end() );
  std::ranges::advance(iter, r2.end());
  VERIFY( iter == r2.end() );

  auto diff = std::ranges::advance(iter, 99, r2.end());
  VERIFY( iter == r2.end() );
  VERIFY( diff == 99 );
  diff = std::ranges::advance(iter, 99, r2.end());
  VERIFY( iter == r2.end() );
  VERIFY( diff == 99 );

  test_range<int, input_iterator_wrapper> r3(a);
  iter = r3.begin();
  diff = std::ranges::advance(iter, 99, r3.end());
  VERIFY( iter == r3.end() );
  VERIFY( diff == 97 );
  diff = std::ranges::advance(iter, 99, r3.end());
  VERIFY( iter == r3.end() );
  VERIFY( diff == 99 );
}

void
test05()
{
  int a[2] = { };
  test_range<int, output_iterator_wrapper> r(a);
  auto iter = r.begin();
  std::ranges::advance(iter, 1);
  VERIFY( iter != r.end() );
  std::ranges::advance(iter, 1);
  std::ranges::advance(iter, 0);
  VERIFY( iter == r.end() );
  std::ranges::advance(iter, 0);
  VERIFY( iter == r.end() );

  test_range<int, output_iterator_wrapper> r2(a);
  iter = r2.begin();
  ++iter;
  std::ranges::advance(iter, r2.end());
  VERIFY( iter == r2.end() );
  std::ranges::advance(iter, r2.end());
  VERIFY( iter == r2.end() );

  auto diff = std::ranges::advance(iter, 99, r2.end());
  VERIFY( iter == r2.end() );
  VERIFY( diff == 99 );
  diff = std::ranges::advance(iter, 99, r2.end());
  VERIFY( iter == r2.end() );
  VERIFY( diff == 99 );

  test_range<int, output_iterator_wrapper> r3(a);
  iter = r3.begin();
  diff = std::ranges::advance(iter, 99, r3.end());
  VERIFY( iter == r3.end() );
  VERIFY( diff == 97 );
  diff = std::ranges::advance(iter, 99, r3.end());
  VERIFY( iter == r3.end() );
  VERIFY( diff == 99 );
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
