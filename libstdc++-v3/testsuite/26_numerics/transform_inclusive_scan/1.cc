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

// C++17 29.8.10 [transform.inclusive.scan]

#include <numeric>
#include <iterator>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

int a[] = {1, 2, 3, 4, 5, 6, 7};

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;

/*
template<class InputIterator, class OutputIterator, class BinaryOperation,
	 class UnaryOperation>
  OutputIterator
  transform_inclusive_scan(InputIterator, InputIterator, OutputIterator,
			   BinaryOperation, UnaryOperation);
*/
void
test01()
{
  int out[7];
  test_container<int, output_iterator_wrapper> co(out);
  test_container<int, input_iterator_wrapper> ca(a);
  auto end = std::transform_inclusive_scan(ca.begin(), ca.end(), co.begin(),
					   std::multiplies<>(),
					   [](int i) { return i+1; });
  static_assert(std::is_same_v<decltype(end), decltype(co.begin())>);
  VERIFY( end.ptr == out+7 );
  VERIFY( out[0] == 2 );
  VERIFY( out[1] == (2*3) );
  VERIFY( out[2] == (2*3*4) );
  VERIFY( out[3] == (2*3*4*5) );
  VERIFY( out[4] == (2*3*4*5*6) );
  VERIFY( out[5] == (2*3*4*5*6*7) );
  VERIFY( out[6] == (2*3*4*5*6*7*8) );
}

/*
template<class InputIterator, class OutputIterator, class BinaryOperation,
	 class UnaryOperation, class T>
  OutputIterator
  transform_inclusive_scan(InputIterator, InputIterator, OutputIterator,
			   BinaryOperation, UnaryOperation, T);
*/
void
test02()
{
  int out[7];
  test_container<int, output_iterator_wrapper> co(out);
  test_container<int, input_iterator_wrapper> ca(a);
  auto end = std::transform_inclusive_scan(ca.begin(), ca.end(), co.begin(),
					   std::multiplies<>(),
					   [](int i) { return i+1; },
					   3);
  static_assert(std::is_same_v<decltype(end), decltype(co.begin())>);
  VERIFY( end.ptr == out+7 );
  VERIFY( out[0] == 3*2 );
  VERIFY( out[1] == (3*2*3) );
  VERIFY( out[2] == (3*2*3*4) );
  VERIFY( out[3] == (3*2*3*4*5) );
  VERIFY( out[4] == (3*2*3*4*5*6) );
  VERIFY( out[5] == (3*2*3*4*5*6*7) );
  VERIFY( out[6] == (3*2*3*4*5*6*7*8) );
}

int
main()
{
  test01();
  test02();
}
