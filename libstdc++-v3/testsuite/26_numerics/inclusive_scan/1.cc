// { dg-do run { target c++17 } }

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

// C++17 29.8.8 [inclusive.scan]

#include <numeric>
#include <iterator>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

int a[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;

/*
template<class InputIterator, class OutputIterator>
  OutputIterator
  inclusive_scan(InputIterator, InputIterator, OutputIterator);
*/
void
test01()
{
  int out[10];
  test_container<int, output_iterator_wrapper> co(out);
  test_container<int, input_iterator_wrapper> ca(a);
  auto end = std::inclusive_scan(ca.begin(), ca.end(), co.begin());
  static_assert(std::is_same_v<decltype(end), decltype(co.begin())>);
  VERIFY( end.ptr == out+10 );
  VERIFY( out[0] == 1 );
  VERIFY( out[1] == (1+2) );
  VERIFY( out[2] == (1+2+3) );
  VERIFY( out[3] == (1+2+3+4) );
  VERIFY( out[4] == (1+2+3+4+5) );
  VERIFY( out[5] == (1+2+3+4+5+6) );
  VERIFY( out[6] == (1+2+3+4+5+6+7) );
  VERIFY( out[7] == (1+2+3+4+5+6+7+8) );
  VERIFY( out[8] == (1+2+3+4+5+6+7+8+9) );
  VERIFY( out[9] == (1+2+3+4+5+6+7+8+9+10) );
}

/*
template<class InputIterator, class OutputIterator, class BinaryOperation>
  OutputIterator
  inclusive_scan(InputIterator, InputIterator, OutputIterator,
		 BinaryOperation);
*/
void
test02()
{
  int out[10];
  test_container<int, output_iterator_wrapper> co(out);
  test_container<int, input_iterator_wrapper> ca(a);
  auto end = std::inclusive_scan(ca.begin(), ca.end(), co.begin(),
				 [](int i, int j) { return 2*i + 2*j; });
  static_assert(std::is_same_v<decltype(end), decltype(co.begin())>);
  VERIFY( end.ptr == out+10 );
  VERIFY( out[0] == 1 );
  VERIFY( out[1] == (2*1+2*2) );
  VERIFY( out[2] == (2*6+2*3) );
  VERIFY( out[3] == (2*18+2*4) );
  VERIFY( out[4] == (2*44+2*5) );
  VERIFY( out[5] == (2*98+2*6));
  VERIFY( out[6] == (2*208+2*7) );
  VERIFY( out[7] == (2*430+2*8) );
  VERIFY( out[8] == (2*876+2*9) );
  VERIFY( out[9] == (2*1770+2*10) );
}

/*
template<class InputIterator, class OutputIterator, class BinaryOperation, T>
  OutputIterator
  inclusive_scan(InputIterator, InputIterator, OutputIterator,
		 BinaryOperation, T);
*/
void
test03()
{
  int out[10];
  test_container<int, output_iterator_wrapper> co(out);
  test_container<int, input_iterator_wrapper> ca(a);
  auto end = std::inclusive_scan(ca.begin(), ca.end(), co.begin(),
				 [](int i, int j) { return 2*i + 2*j; },
				 1);
  static_assert(std::is_same_v<decltype(end), decltype(co.begin())>);
  VERIFY( end.ptr == out+10 );
  VERIFY( out[0] == 4 );
  VERIFY( out[1] == (2*4+2*2) );
  VERIFY( out[2] == (2*12+2*3) );
  VERIFY( out[3] == (2*30+2*4) );
  VERIFY( out[4] == (2*68+2*5) );
  VERIFY( out[5] == (2*146+2*6) );
  VERIFY( out[6] == (2*304+2*7));
  VERIFY( out[7] == (2*622+2*8) );
  VERIFY( out[8] == (2*1260+2*9) );
  VERIFY( out[9] == (2*2538+2*10) );
}

int
main()
{
  test01();
  test02();
  test03();
}
