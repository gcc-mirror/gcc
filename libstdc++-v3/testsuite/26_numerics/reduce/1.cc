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

// C++17 29.8.3 [reduce]

#include <numeric>
#include <iterator>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

/*
template<class InputIterator>
  iterator_traits<InputIterator>::value_type
  reduce(InputIterator, InputIterator);
*/
void
test01()
{
  using __gnu_test::test_container;
  using __gnu_test::input_iterator_wrapper;
  int array[5] = { 1, 2, 3, 4, 5 };
  test_container<int, input_iterator_wrapper> con(array);
  int res = std::reduce(con.begin(), con.end());
  VERIFY( res == 15 );
}

/*
template<class InputIterator, class T>
  T reduce(InputIterator, InputIterator, T);
*/
void
test02()
{
  bool b[] = {true, false, true, true, false, true, false, true, true, false};
  int res = std::reduce(std::begin(b), std::end(b), 100);
  VERIFY( res == 106 );
}

/*
template<class InputIterator, class T>
  T reduce(InputIterator, InputIterator, T);
template<class InputIterator, class T, class BinaryOperation>
  T reduce(InputIterator, InputIterator, T, BinaryOperation);
*/
void
test03()
{
  int a[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

  auto res = std::reduce(std::begin(a), std::end(a), (short)11);
  static_assert(std::is_same_v<decltype(res), short>);
  VERIFY( res == 66 );

  auto res2 = std::reduce(std::begin(a), std::end(a), -1l, std::multiplies<>());
  static_assert(std::is_same_v<decltype(res2), long>);
  VERIFY( res2 == -3628800 );
}

int
main()
{
  test01();
  test02();
  test03();
}
