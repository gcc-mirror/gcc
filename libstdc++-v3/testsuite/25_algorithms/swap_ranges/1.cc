// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

// 25.2.2 swap_ranges

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;

typedef test_container<int, forward_iterator_wrapper> Container;


void
test1()
{
  int array1[]={1, 2};
  int array2[]={3, 4};
  Container con1(array1, array1 + 2);
  Container con2(array2, array2 + 2);
  VERIFY(std::swap_ranges(con1.begin(), con1.end(), con2.begin()).ptr ==
	 array2 + 2);
  VERIFY(array1[0] == 3 && array1[1] == 4 && array2[0] == 1 && array2[1] == 2);
}

void
test2()
{
  int array1[] = {1};
  int array2[] = {1};
  Container con1(array1, array1);
  Container con2(array2, array2);
  VERIFY(std::swap_ranges(con1.begin(), con1.end(), con2.begin()).ptr == array2);
}

int
main()
{
  test1();
  test2();
}
