// Copyright (C) 2013-2025 Free Software Foundation, Inc.
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

// 25.2.10 [mismatch]

// { dg-do run { target c++14 } }

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;

typedef test_container<int, input_iterator_wrapper> Container;

int array1[] = {0, 1};
int array2[] = {1, 0};
int array3[] = {1, 0, 1};

struct equal_to
{
  static int count;

  bool operator()(int l, int r)
  {
    ++count;
    return l == r;
  }
} eq;

int equal_to::count = 0;

bool __attribute__((unused)) test = false;

void test1()
{
  // empty ranges
  Container con1(array1, array1);
  Container con2(array2, array2);
  auto res = std::mismatch(con1.begin(), con1.end(), con2.begin(), con2.end());
  VERIFY( res.first.ptr == array1 );
  VERIFY( res.second.ptr == array2 );
  res = std::mismatch(con1.begin(), con1.end(), con2.begin(), con2.end(), eq);
  VERIFY( res.first.ptr == array1 );
  VERIFY( res.second.ptr == array2 );
  VERIFY( equal_to::count == 0 );
}

void test2()
{
  // first range empty, second non-empty
  Container con1(array1, array1);
  Container con2(array2, array2 + 2);
  auto res = std::mismatch(con1.begin(), con1.end(), con2.begin(), con2.end());
  VERIFY( res.first.ptr == array1 );
  VERIFY( res.second.ptr == array2 );

  res = std::mismatch(con1.begin(), con1.end(), con2.begin(), con2.end(), eq);
  VERIFY( res.first.ptr == array1 );
  VERIFY( res.second.ptr == array2 );
  VERIFY( equal_to::count == 0 );
}

void test3()
{
  // first range non-empty, second empty
  Container con1(array1, array1 + 2);
  Container con2(array2, array2);
  auto res = std::mismatch(con1.begin(), con1.end(), con2.begin(), con2.end());
  VERIFY( res.first.ptr == array1 );
  VERIFY( res.second.ptr == array2 );

  res = std::mismatch(con1.begin(), con1.end(), con2.begin(), con2.end(), eq);
  VERIFY( res.first.ptr == array1 );
  VERIFY( res.second.ptr == array2 );
  VERIFY( equal_to::count == 0 );
}

void test4()
{
  // non-empty, mismatching ranges
  Container con1(array1, array1 + 2);
  Container con2(array2, array2 + 2);
  auto res = std::mismatch(con1.begin(), con1.end(), con2.begin(), con2.end());
  VERIFY( res.first.ptr == array1 );
  VERIFY( res.second.ptr == array2 );

  con1.bounds.first = array1;
  con2.bounds.first = array2;
  res = std::mismatch(con1.begin(), con1.end(), con2.begin(), con2.end(), eq);
  VERIFY( res.first.ptr == array1 );
  VERIFY( res.second.ptr == array2 );
  VERIFY( equal_to::count == 1 );
  equal_to::count = 0;
}

void test5()
{
  // non-empty, matching ranges
  Container con3(array3, array3 + 2);
  Container con2(array2, array2 + 2);
  auto res = std::mismatch(con3.begin(), con3.end(), con2.begin(), con2.end());
  VERIFY( res.first.ptr == array3 + 2 );
  VERIFY( res.second.ptr == array2 + 2 );

  con3.bounds.first = array3;
  con2.bounds.first = array2;
  res = std::mismatch(con3.begin(), con3.end(), con2.begin(), con2.end(), eq);
  VERIFY( res.first.ptr == array3 + 2 );
  VERIFY( res.second.ptr == array2 + 2 );
  VERIFY( equal_to::count == 2 );
  equal_to::count = 0;
}

void test6()
{
  // non-empty, matching sub-ranges, first range longer
  Container con3(array3, array3 + 3);
  Container con2(array2, array2 + 2);
  auto res = std::mismatch(con3.begin(), con3.end(), con2.begin(), con2.end());
  VERIFY( res.first.ptr == array3 + 2 );
  VERIFY( res.second.ptr == array2 + 2 );

  con3.bounds.first = array3;
  con2.bounds.first = array2;
  res = std::mismatch(con3.begin(), con3.end(), con2.begin(), con2.end(), eq);
  VERIFY( res.first.ptr == array3 + 2 );
  VERIFY( res.second.ptr == array2 + 2 );
  VERIFY( equal_to::count == 2 );
  equal_to::count = 0;
}

void test7()
{
  // non-empty, matching sub-ranges, second range longer
  Container con3(array3, array3 + 3);
  Container con2(array2, array2 + 2);
  auto res = std::mismatch(con2.begin(), con2.end(), con3.begin(), con3.end());
  VERIFY( res.first.ptr == array2 + 2 );
  VERIFY( res.second.ptr == array3 + 2 );

  con3.bounds.first = array3;
  con2.bounds.first = array2;
  res = std::mismatch(con2.begin(), con2.end(), con3.begin(), con3.end(), eq);
  VERIFY( res.first.ptr == array2 + 2 );
  VERIFY( res.second.ptr == array3 + 2 );
  VERIFY( equal_to::count == 2 );
  equal_to::count = 0;
}

int main()
{
  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
}
