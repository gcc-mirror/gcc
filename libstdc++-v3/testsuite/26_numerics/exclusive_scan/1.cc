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

// C++17 29.8.7 [exclusive.scan]

#include <numeric>
#include <iterator>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

int a[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;

/*
template<class InputIterator, class OutputIterator, class T>
  OutputIterator
  exclusive_scan(InputIterator, InputIterator, OutputIterator, T);
*/
void
test01()
{
  int out[10];
  test_container<int, output_iterator_wrapper> co(out);
  test_container<int, input_iterator_wrapper> ca(a);
  auto end = std::exclusive_scan(ca.begin(), ca.end(), co.begin(), 5);
  static_assert(std::is_same_v<decltype(end), decltype(co.begin())>);
  VERIFY( end.ptr == out+10 );
  VERIFY( out[0] == 5 );
  VERIFY( out[1] == 6 );
  VERIFY( out[2] == 8 );
  VERIFY( out[3] == 11 );
  VERIFY( out[4] == 15 );
  VERIFY( out[5] == 20 );
  VERIFY( out[6] == 26 );
  VERIFY( out[7] == 33 );
  VERIFY( out[8] == 41 );
  VERIFY( out[9] == 50 );
}

/*
template<class InputIterator, class OutputIterator, class T,
	 class BinaryOperation>
  OutputIterator
  exclusive_scan(InputIterator, InputIterator, OutputIterator, T,
		 BinaryOperation);
*/
void
test02()
{
  int out[10];
  test_container<int, output_iterator_wrapper> co(out);
  test_container<int, input_iterator_wrapper> ca(a);
  auto end = std::exclusive_scan(ca.begin(), ca.end(), co.begin(), 2,
				 [](int i, int j) { return 2*i + 2*j; });
  static_assert(std::is_same_v<decltype(end), decltype(co.begin())>);
  VERIFY( end.ptr == out+10 );
  VERIFY( out[0] == 2 );
  VERIFY( out[1] == 6 );
  VERIFY( out[2] == 16 );
  VERIFY( out[3] == 38 );
  VERIFY( out[4] == 84 );
  VERIFY( out[5] == 178 );
  VERIFY( out[6] == 368 );
  VERIFY( out[7] == 750 );
  VERIFY( out[8] == 1516 );
  VERIFY( out[9] == 3050 );
}

int
main()
{
  test01();
  test02();
}
