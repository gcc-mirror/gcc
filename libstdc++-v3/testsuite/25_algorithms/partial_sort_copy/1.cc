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

// 25.3.1.4 [lib.partial.sort.copy]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;
using __gnu_test::input_iterator_wrapper;
using std::partial_sort_copy;

typedef test_container<int, random_access_iterator_wrapper> Rcontainer;
typedef test_container<int, input_iterator_wrapper> Icontainer;

void 
test1()
{
  int array[]={2,1,0};
  Rcontainer rcon1(array, array);
  Rcontainer rcon2(array, array + 2);
  Icontainer icon1(array, array);
  Icontainer icon2(array, array + 2);
  partial_sort_copy(icon1.begin(), icon1.end(), rcon1.begin(), rcon1.end());
  partial_sort_copy(icon1.begin(), icon1.end(), rcon2.begin(), rcon2.end());
  partial_sort_copy(icon2.begin(), icon2.end(), rcon1.begin(), rcon1.end());
  partial_sort_copy(icon2.begin(), icon2.end(), rcon2.begin(), rcon2.end()); 
}

void 
test2()
{
  int array1[] = {4, 3, 2, 1, 0};
  int array2[5];
  Icontainer icon(array1, array1 + 5);
  Rcontainer rcon(array2, array2 + 5);
  partial_sort_copy(icon.begin(), icon.end(), rcon.begin(), rcon.end());
  VERIFY(array2[0] == 0 && array2[1] == 1 && array2[2] == 2 &&
	 array2[3] == 3 && array2[4] == 4);
}

void 
test3()
{
  int array1[] = {4, 0, 1, 3, 2};
  int array2[5];
  Icontainer icon(array1, array1 + 5);
  Rcontainer rcon(array2, array2 + 2);
  partial_sort_copy(icon.begin(), icon.end(), rcon.begin(), rcon.end());
  VERIFY(array2[0] == 0 && array2[1] == 1);
}

void 
test4()
{
  int array1[] = {4, 1, 3, 2, 0};
  int array2[20];
  Icontainer icon(array1, array1 + 5);
  Rcontainer rcon(array2, array2 + 20);
  partial_sort_copy(icon.begin(), icon.end(), rcon.begin(), rcon.end());
  VERIFY(array2[0] == 0 && array2[1] == 1 && array2[2] == 2 &&
	 array2[3] == 3 && array2[4] == 4);
}

int 
main()
{
  test1();
  test2();
  test3();
  test4();
}
