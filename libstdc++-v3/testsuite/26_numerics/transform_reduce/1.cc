// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

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

// C++17 29.8.5 [transform.reduce]

#include <numeric>
#include <iterator>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

int a[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
double b[] = {0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5};

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;

/*
template<class InputIterator1, class InputIterator2, class T>
  T transform_reduce(InputIterator1, InputIterator1, InputIterator2, T);
*/
void
test01()
{
  auto res = std::transform_reduce(std::begin(a), std::end(a), std::begin(b),
				   1.0f);
  static_assert(std::is_same_v<decltype(res), float>);
  VERIFY( res == (float)(1 + 0.5 + 1 + 1.5 + 2 + 2.5 + 3 + 3.5 + 4 + 4.5 + 5) );

  test_container<int, input_iterator_wrapper> ca(a);
  test_container<double, input_iterator_wrapper> cb(b);

  auto res2 = std::transform_reduce(ca.begin(), ca.end(), cb.begin(),
				   1.0f);
  static_assert(std::is_same_v<decltype(res2), float>);
  VERIFY( res2 == res );
}

/*
template<class InputIterator1, class InputIterator2, class T,
	 class BinaryOperation1, class BinaryOperation2>
  T transform_reduce(InputIterator1, InputIterator1, InputIterator2, T,
		     BinaryOperation1, BinaryOperation2);
*/
void
test02()
{
  auto res = std::transform_reduce(std::begin(a), std::end(a), std::begin(b),
				    1L, std::multiplies<>(), std::plus<int>());
  static_assert(std::is_same_v<decltype(res), long>);
  VERIFY( res == (1L * 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10) );

  test_container<int, input_iterator_wrapper> ca(a);
  test_container<double, input_iterator_wrapper> cb(b);

  auto res2 = std::transform_reduce(ca.begin(), ca.end(), cb.begin(),
				    1L, std::multiplies<>(), std::plus<int>());
  static_assert(std::is_same_v<decltype(res2), long>);
  VERIFY( res2 == res );
}

/*
template<class InputIterator, class T, class BinaryOperation,
	 class UnaryOperation>
  T transform_reduce(InputIterator, InputIterator, T,
		     BinaryOperation, UnaryOperation);
*/
void
test03()
{
  auto res = std::transform_reduce(std::begin(a), std::end(a), 10.0,
				   std::plus<>(),
				   [](int i) { return i * i; });
  static_assert(std::is_same_v<decltype(res), double>);
  VERIFY( res == (10.0 + 1 + 4 + 9 + 16 + 25 + 36 + 49 + 64 + 81 + 100) );

  test_container<int, input_iterator_wrapper> ca(a);
  test_container<double, input_iterator_wrapper> cb(b);

  auto res2 = std::transform_reduce(ca.begin(), ca.end(), 10.0,
				   std::plus<>(),
				   [](int i) { return i * i; });
  static_assert(std::is_same_v<decltype(res2), double>);
  VERIFY( res2 == (10.0 + 1 + 4 + 9 + 16 + 25 + 36 + 49 + 64 + 81 + 100) );
}

int
main()
{
  test01();
  test02();
  test03();
}
