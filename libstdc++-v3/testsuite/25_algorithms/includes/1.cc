// Copyright (C) 2005-2017 Free Software Foundation, Inc.
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

// 25.3.5.1 [lib.includes]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;
using std::includes;

typedef test_container<int, input_iterator_wrapper> Container;

void
test1()
{
  int array[] = {0};
  Container con1(array, array);
  Container con2(array, array);
  VERIFY(includes(con1.begin(), con1.end(), con2.begin(), con2.end()));
}

void
test2()
{
  int array[] = {0, 1};
  Container con1(array, array);
  Container con2(array, array + 2);
  VERIFY(!includes(con1.begin(), con1.end(), con2.begin(), con2.end()));
}

void
test3()
{
  int array[] = {0, 1};
  Container con1(array, array + 2);
  Container con2(array, array);
  VERIFY(includes(con1.begin(), con1.end(), con2.begin(), con2.end()));
}

void
test4()
{
  int array1[] = {1, 2, 3, 4, 6, 8, 9};
  int array2[] = {2, 4, 6, 8};
  Container con1(array1, array1 + 7);
  Container con2(array2, array2 + 4);
  VERIFY(includes(con1.begin(), con1.end(), con2.begin(), con2.end()));
}

void
test5()
{
  int array1[] = {1, 2, 3, 5};
  int array2[] = {2, 4, 6, 8};
  Container con1(array1, array1 + 4);
  Container con2(array2, array2 + 4);
  VERIFY(!includes(con1.begin(), con1.end(), con2.begin(), con2.end()));
}

int main()
{
  test1();
  test2();
  test3();
  test4();
  test5();
}
