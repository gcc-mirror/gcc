// { dg-do run { target c++11 } }

// 2011-01-13  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2011-2024 Free Software Foundation, Inc.
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

// 25.2.12 [alg.is_permutation] Is permutation

#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>

struct my_equal_to
{
  bool
  operator()(int __x, int __y) const
  { return __x % 10 == __y % 10; }
};

const int arr0[] = { 11, 22, 33, 44, 55 };

void
do_test(int arr1[5], bool np = true)
{
  do
    VERIFY( std::is_permutation(arr1, arr1 + 5, arr0) == np );
  while (std::next_permutation(arr1, arr1 + 5));
}

template<typename Predicate>
  void
  do_test(int arr1[5], Predicate pred, bool np = true)
  {
    do
      VERIFY( std::is_permutation(arr1, arr1 + 5, arr0, pred) == np );
    while (std::next_permutation(arr1, arr1 + 5));
  }

void test01()
{
  int arr1[] = { 11, 22, 33, 44, 55 };
  do_test(arr1);

  int arr2[] = { 11, 33, 33, 44, 55 };
  do_test(arr2, false);

  int arr3[] = { 33, 33, 33, 44, 44 };
  do_test(arr3, false);

  int arr4[] = { 11, 22, 33, 44, 55 };
  do_test(arr4, std::equal_to<int>());

  int arr5[] = { 11, 33, 33, 44, 55 };
  do_test(arr5, std::equal_to<int>(), false);

  int arr6[] = { 33, 33, 33, 44, 44 };
  do_test(arr6, std::equal_to<int>(), false);

  int arr7[] = { 1, 2, 3, 4, 5 };
  do_test(arr7, my_equal_to());

  int arr8[] = { 1, 3, 3, 4, 5 };
  do_test(arr8, my_equal_to(), false);

  int arr9[] = { 3, 3, 3, 4, 4 };
  do_test(arr9, my_equal_to(), false);

  int arr10[] = { 111, 222, 333, 444, 555 };
  do_test(arr10, my_equal_to());

  int arr11[] = { 1, 222, 33, 4, 55 };
  do_test(arr11, my_equal_to());

  int arr12[] = { 111, 333, 333, 444, 555 };
  do_test(arr12, my_equal_to(), false);

  int arr13[] = { 333, 333, 333, 444, 444 };
  do_test(arr13, my_equal_to(), false);
}

int main()
{
  test01();
  return 0;
}
