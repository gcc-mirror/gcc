// { dg-do run { target c++17 } }

// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

// C++17 29.8.9 [transform.exclusive.scan]

#include <numeric>
#include <iterator>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

int a[] = {1, 2, 3, 4, 5, 6, 7};

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;

/*
template<class InputIterator, class OutputIterator, class T,
	 class BinaryOperation, class UnaryOperation>
  OutputIterator
  transform_exclusive_scan(InputIterator, InputIterator, OutputIterator, T,
			   BinaryOperation, UnaryOperation);
*/
void
test01()
{
  int out[7];
  test_container<int, output_iterator_wrapper> co(out);
  test_container<int, input_iterator_wrapper> ca(a);
  auto end = std::transform_exclusive_scan(ca.begin(), ca.end(), co.begin(), 5,
					   std::multiplies<>(),
					   std::negate<>());
  static_assert(std::is_same_v<decltype(end), decltype(co.begin())>);
  VERIFY( end.ptr == out+7 );
  VERIFY( out[0] == 5 );
  VERIFY( out[1] == -5 );
  VERIFY( out[2] == 10 );
  VERIFY( out[3] == -30 );
  VERIFY( out[4] == 120 );
  VERIFY( out[5] == -600 );
  VERIFY( out[6] == 3600 );
}

int
main()
{
  test01();
}
