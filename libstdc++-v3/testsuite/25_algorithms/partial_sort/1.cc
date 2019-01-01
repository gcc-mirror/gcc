// Copyright (C) 2005-2019 Free Software Foundation, Inc.
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

// 25.3.1.3 [lib.partial.sort]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;
using std::partial_sort;

typedef test_container<int, random_access_iterator_wrapper> Container;

void 
test1()
{
  int array[]={2,1,0};
  Container con1(array, array + 2);
  Container con2(array, array);
  partial_sort(con2.begin(), con2.begin(), con2.end());
  partial_sort(con1.begin(), con1.begin(), con1.end());
  partial_sort(con1.begin(), con1.end(), con1.end());
}

void 
test2()
{
  int array[] = {6, 5, 4, 3, 2, 1, 0};
  Container con(array, array + 7);
  partial_sort(con.begin(), con.it(3), con.end());
  VERIFY(array[0] == 0 && array[1] == 1 && array[2] == 2);
}

void 
test3()
{
  int array[] = {0, 6, 1, 5, 2, 4, 3};
  Container con(array,array + 7);
  partial_sort(con.begin(), con.it(3), con.end());
  VERIFY(array[0] == 0 && array[1] == 1 && array[2] == 2);
}

int 
main()
{
  test1();
  test2();
  test3();
}
