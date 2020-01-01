// Copyright (C) 2005-2020 Free Software Foundation, Inc.
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

// 25.1.4 [lib.alg.find.first.of]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;

typedef test_container<int, forward_iterator_wrapper> Container;

using std::find_first_of;

void 
test1()
{
  int array[] = {0};
  Container con1(array, array);
  Container con2(array, array + 1);
  VERIFY(find_first_of(con1.begin(), con1.end(), con1.begin(), con1.end()).ptr == array);
  VERIFY(find_first_of(con1.begin(), con1.end(), con2.begin(), con2.end()).ptr == array);
  VERIFY(find_first_of(con2.begin(), con2.end(), con1.begin(), con1.end()).ptr == array + 1);
}

void 
test2()
{
  int array1[] = {1 ,2, 3, 4, 5, 6};
  int array2[] = {3, 4, 9};
  Container con1(array1, array1 + 6);
  Container con2(array2, array2 + 3);
  VERIFY(find_first_of(con1.begin(), con1.end(), con2.begin(), con2.end()).ptr == array1 + 2);
}

int 
main()
{
  test1();
  test2();
}
