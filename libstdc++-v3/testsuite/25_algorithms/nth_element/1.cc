// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 25.3.2 [lib.alg.nth.element]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;
using std::nth_element;

typedef test_container<int, random_access_iterator_wrapper> Container;

void 
test1()
{
  int array[]={0};
  Container con(array, array);
  partial_sort(con.begin(), con.begin(), con.end());
}

void 
test2()
{
  int array[]={2,1,0};
  Container con(array, array + 2);
  partial_sort(con.begin(), con.begin(), con.end());
  partial_sort(con.begin(), con.end(), con.end());
}

void 
test3()
{
  int array[] = {6, 5, 4, 3, 2, 1, 0};
  Container con(array, array + 7);
  nth_element(con.begin(), con.it(3), con.end());
  for(int i = 0; i < 3; ++i)
    VERIFY(array[i] < array[3]);
  for(int i = 4; i < 7; ++i)
    VERIFY(array[3] < array[i]);
}

void 
test4()
{
  int array[] = {0, 6, 1, 5, 2, 4, 3};
  Container con(array,array + 7);
  nth_element(con.begin(), con.it(3), con.end());
  for(int i = 0; i < 3; ++i)
    VERIFY(array[i] < array[3]);
  for(int i = 4; i < 7; ++i)
    VERIFY(array[3] < array[i]);
}

int 
main()
{
  test1();
  test2();
  test3();
  test4();
}
